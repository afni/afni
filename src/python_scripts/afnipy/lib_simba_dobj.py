#!/usr/bin/env python

# A library of functions for SIMBA processing
#
# auth : Yuan Zhong (University of Michigan, USA)
#        PA Taylor (SSCC, NIMH, NIH, USA)
#        Yuan Zhong (University of Michigan, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : start of main interface 
# ============================================================================

import sys, copy
import torch

import numpy    as np

from   afnipy     import afni_base              as ab
from   afnipy     import afni_util              as au
from   communifti import lib_nibabel_read_nifti as lnrn

# ============================================================================

class SimbaDataObj:
    """The group-level data object for running SIMBA.

There are multiple ways to provide data.  The in_pickle object is the
current starting point, from initial SIMBA code development.

Parameters
----------
fname_pickle : str
    file name of a pickled object (**have to provide rule/formatting
    used for pickling process)  ***someday won't be main input
infiles : list of str
    list of filenames to read in, each of which is a dataset to be
    translated
fname_mask : str

    """

    def __init__(self, fname_pickle=None, infiles=[],
                 fname_mask='', fname_ulay='', verb=1):

        # ----- set up attributes

        # general variables
        self.verb            = verb

        # input variables
        self.fname_pickle    = fname_pickle # fname of pickle dset
        self.infiles         = infiles      # list of infile names
        self.fname_mask      = fname_mask   # fname of mask dset (for calc)
        self.fname_ulay      = fname_ulay   # fname of ulay dset (for images)

        # main data variables
        self.data            = None         # torch arr, dims=(nsubj, nvox)
        self.grid            = None         # torch arr, dims=(nvox, 3); coords
        self.ii              = None         # i-th idx of unflattened vol arr
        self.jj              = None         # j-th idx of unflattened vol arr
        self.kk              = None         # k-th idx of unflattened vol arr
        self.voxdim          = None         # np arr, dims=(3); voxel dim (mm)
        self.matdim          = None         # np arr, dims=(3); matrix dim

        # additional data variables/dsets
        self.mask_data       = None         # np.ndarray, mask data
        self.mask_hdr        = None         # NIFTI nibabel header
        self.ulay_data       = None         # np.ndarray, underlay data
        self.ulay_hdr        = None         # NIFTI nibabel header

        # ----- take action(s)

        tmp = self.basic_setup()
        if tmp : return

        # first dset to load, so we can get relevant shape/data info
        tmp = self.unpack_mask()
        if tmp : return

        tmp = self.set_dset_info_all()
        if tmp : return

        if len(self.infiles) :
            tmp = self.unpack_infiles()
            if tmp: return
        elif self.fname_pickle is not None :
            tmp = self.unpack_pickle()
            if tmp: return

        if self.fname_ulay : 
            tmp = self.load_ulay()
            if tmp: return


        # *****

    # ----- methods

    def basic_setup(self):
        """Verify that datasets and other input choices exist"""

        ab.IP("Prepare simba")

        BAD_RETURN = -1

        # (req) input dsets: either infiles or pickle
        if len(self.infiles) and self.fname_pickle :
            ab.EP1("Cannot have _both_ pickle and infiles")
            return BAD_RETURN
        elif len(self.infiles) :
            nfail = au.check_all_dsets_exist(self.infiles, label='infiles', 
                                             verb=self.verb)
            if nfail :
                ab.EP1("Failed to load infiles")
                return BAD_RETURN
        elif self.fname_pickle :
            if not(os.path.isfile(self.fname_pickle)) :
                msg = "Failed to find pickle: {}".format(self.fname_pickle)
                ab.EP1(msg)
                return BAD_RETURN
        else:
            ab.EP1("Need to provide infiles or pickle inset")
            return BAD_RETURN

        # (req) must have mask at present
        if not(self.fname_mask) :
            ab.EP1("Need to provide mask")
            return BAD_RETURN
        else:
            nfail = au.check_all_dsets_exist([self.fname_mask], label='mask', 
                                             verb=self.verb)
            if nfail :
                ab.EP1("Failed to load mask")
                return BAD_RETURN

        # (opt) check existence of ulay, if provided
        if self.fname_ulay :
            nfail = au.check_all_dsets_exist([self.fname_ulay], label='ulay', 
                                             verb=self.verb)
            if nfail :
                ab.EP1("Failed to load ulay")
                return BAD_RETURN

        return 0

    def unpack_pickle(self):
        """Unpack the pickle object"""

        ppp = load_pickle(self.fname_pickle)

        self.data = ppp['data']
        self.grid = ppp['S']
        self.ii, self.jj, self.kk = ppp['coord']

        return 0

    def unpack_infiles(self):
        """Unpack the list of infiles; they have already been checked for
        existence and grid consistency
        """

        BAD_RETURN = -1

        if self.verb > 1 : 
            print("++ Load {} dsets".format(self.ninfiles), flush=True)

        # init torch tensor to hold all infile data, dims=(ndsets, nvox)
        self.data = torch.tensor( np.zeros((self.ninfiles, self.nvox), 
                                           dtype=np.float64) )

        # loop over all infiles and load flat data
        for nn in range(self.ninfiles) :
            dset = self.infiles[nn]
            if self.verb > 1 : print(" ++ Load dset {}: {}".format(nn+1, dset))

            # read NIFTI to data array and header obj
            is_fail, arr, hdr = \
                lnrn.read_nifti_to_nibabel(dset, verb=self.verb)
            if is_fail :
                ab.EP1("Could not read in NIFTI: {}".format(dset))
                return BAD_RETURN 

            # keep all arrays in list
            self.data[nn, :] = torch.from_numpy(arr[self.ii, self.jj, self.kk])


        return 0

    def unpack_mask(self):
        """Unpack the mask dset, from its name. NB: the self.mask_data and
        self.mask_hdr is where we get the dset_info_all from, like
        grid and nonzero vox.
        """

        BAD_RETURN = -4

        if self.verb > 1 :  print(" ++ Load mask")

        # read NIFTI to tmp data array (needs proc) and header obj
        is_fail, self.mask_data, self.mask_hdr = \
            lnrn.read_nifti_to_nibabel(self.fname_mask, verb=self.verb)
        if is_fail :
            ab.EP1("Could not read in NIFTI: {}".format(self.fname_mask))
            return BAD_RETURN 

        return 0

    def set_dset_info_all(self):
        """Save the basic info from the dset that will be needed within
        SIMBA, like IJK coords, voxel dimensions and grid points."""

        BAD_RETURN = -3

        if self.mask_data is None or self.mask_hdr is None :
            ab.EPI1("Cannot get dset_info_all without mask input")
            return BAD_RETURN

        tmp = self.set_ijk_coords()
        if tmp : return BAD_RETURN

        tmp = self.set_matdim()
        if tmp : return BAD_RETURN

        tmp = self.set_voxdim()
        if tmp : return BAD_RETURN

        tmp = self.set_grid()
        if tmp : return BAD_RETURN

        return 0

    def set_ijk_coords(self):
        """Define the IJK indices where data is going to be analyzed
        from. Here, this is done by using indices where the mask is
        nonzero."""

        # get IJK indices where mask is nonzero, stored in 3 sep arrays
        self.ii, self.jj, self.kk = np.nonzero(self.mask_data)

        return 0

    def set_matdim(self):
        """Define the matrix dimension of the dataset, for writing out
        dset arrays. Here, this is done by using the mask's NIFTI header 
        (pixdim)."""

        BAD_RETURN = -6

        try:
            self.matdim = np.array(self.mask_hdr['dim'][1:4], dtype=int)
        except:
            ab.EP1("Could not get matrix dim from mask")
            return BAD_RETURN

        return 0

    def set_voxdim(self):
        """Define the voxel dimension of the dataset, for providing physical
        grid. Here, this is done by using the mask's NIFTI header
        (pixdim)."""

        BAD_RETURN = -7

        try:
            self.voxdim = np.array(self.mask_hdr['pixdim'][1:4], dtype=float)
        except:
            ab.EP1("Could not get voxel dim from mask")
            return BAD_RETURN

        return 0

    def set_grid(self):
        """Define the floating point version of the grid. Here, this is done
        by using indices where the mask is nonzero (as defined by
        self.ii, self.jj and self.kk) as well as the voxel dims.  NB:
        this is a made-up grid, where only relative spacing matters,
        so (x,y,z) = (ii*dx, jj*dy, kk*dz), rather than getting
        dataset's official coords, which have its own origin and could
        be oblique, etc."""

        if 0 :
            # only for historical reasons, earlier testing
            coord     = np.column_stack((self.ii, self.jj, self.kk))
            self.grid = torch.from_numpy( scale_old(coord, tmin=-1, tmax=1) )
        else:
            self.grid = torch.from_numpy( scale_phys_mm(self.ii, 
                                                        self.jj, 
                                                        self.kk, 
                                                        self.voxdim) )

        return 0

    def load_ulay(self):
        """Load the ulay dataset into separate data array and header pieces,
        and extract just the [0]th volume of ulay, in case fname_ulay
        has multiple volumes (more specifically, is 4D or 5D), since nilearn
        only wants a 3D dataset to plot."""

        BAD_RETURN = -5

        if self.verb > 1 :  print("++ Load ulay")

        # read NIFTI to tmp data array (needs proc) and header obj
        is_fail, arr, self.ulay_hdr = \
            lnrn.read_nifti_to_nibabel(self.fname_ulay, verb=self.verb)
        if is_fail :
            ab.EP1("Could not read in NIFTI: {}".format(self.fname_ulay))
            return BAD_RETURN 

        S  = np.shape(arr)
        LS = len(S)
        if LS == 5 :
            self.ulay_data = arr[:,:,:,0,0]
        elif LS == 4 :
            self.ulay_data = arr[:,:,:,0]
        elif LS == 4 :
            self.ulay_data = copy.deepcopy(arr)
        else:
            ab.EP1("Ulay shape is not 3D, 4D or 5D, but: {}".format(S))
            return BAD_RETURN 

        return 0

    # ----- decorators

    @property
    def ninfiles(self):
        """number of infiles (return 0 if None)"""
        if self.infiles is not None :
            return len(self.infiles)
        else:
            return 0

    @property
    def shape(self):
        """shape of data array"""
        if self.data is not None :
            return self.data.shape
        else:
            return (0, 0)

    @property
    def ndset(self):
        """number of dsets in group object (from shape of data array)"""
        if self.infiles is not None :
            return len(self.infiles)
        elif self.data is not None :
            return self.data.shape[0]
        else:
            return 0

    @property
    def nvox(self):
        """number of voxels in group object (from index list of nonzero vox)"""
        if self.ii is not None :
            return len(self.ii)
        else:
            return 0

    @property
    def nifti_affine(self):
        """Nifti affine array from mask dset; else, return an empty 2D array"""
        if self.mask_hdr is not None :
            return self.mask_hdr.get_best_affine()
        else:
            ab.EP1("Missing mask_hdr for dset_info: affine")
            return np.zeros((0,0))

    @property
    def nifti_qform_code(self):
        """Nifti qform_code array from mask dset; else, return -1"""
        if self.mask_hdr is not None :
            return int(self.mask_hdr['qform_code'])
        else:
            ab.EP1("Missing mask_hdr for dset_info: qform_code")
            return -1

    @property
    def nifti_sform_code(self):
        """Nifti sform_code array from mask dset; else, return -1"""
        if self.mask_hdr is not None :
            return int(self.mask_hdr['sform_code'])
        else:
            ab.EP1("Missing mask_hdr for dset_info: sform_code")
            return -1

def scale_old(vec, tmin=-1, tmax=1):
    '''scale function tp [-1, 1]; early method for making grid, no longer
    used here'''

    vmin = np.min(vec, axis=0)
    vmax = np.max(vec, axis=0)
    scaled = (vec - vmin) / (vmax - vmin) * (tmax - tmin) + tmin
    return scaled

def scale_phys_mm(vii, vjj, vkk, voxdim):
    '''Each of vii, vjj and vkk are length=N arrays, describing the
    (ii,jj,kk) indices where the data is nonzero; voxdim is an array
    with the 3 voxel dimensions.  Return an Nx3 array of floating
    point grid values.'''

    lv = len(voxdim)
    if lv != 3 :
        sys.exit("** ERROR: voxdim has incorrect length: {}".format(voxdim))

    gridx = vii * voxdim[0]
    gridy = vjj * voxdim[1]
    gridz = vkk * voxdim[2]

    grid3 = np.column_stack((gridx, gridy, gridz))

    return grid3

# ============================================================================

if __name__ == "__main__" :

    print("No examples yet.")
    sys.exit(0)
