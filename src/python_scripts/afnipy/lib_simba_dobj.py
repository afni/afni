#!/usr/bin/env python

# A library of functions for SIMBA processing
#
# auth : Yuan Zhong (University of Michigan, USA)
#        PA Taylor (SSCC, NIMH, NIH, USA)
#        Yuan Zhong (University of Michigan, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : start of main interface 
# ============================================================================

import sys

import numpy   as np
import torch

import nibabel as nib

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

    def __init__(self, fname_pickle=None, infiles=None,
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

        # additional data variables/dsets
        self.mask_img        = None         # Nifti1Image, mask data
        self.ulay_img        = None         # Nifti1Image, underlay data
        self.ulay_data       = None         # Nifti1Image, underlay data

        # ----- take action(s)

        if self.fname_mask : 
            # import mask, and define IJK indices (ii, jj, kk) & grid from it
            tmpm   = self.unpack_mask()
            tmpijk = self.set_ijk_coords()
            tmpv   = self.set_voxdim()
            tmpg   = self.set_grid()
        else:
            sys.exit("** ERROR: no mask")

        if self.infiles is not None :
            tmpi = self.unpack_infiles()
        elif self.fname_pickle is not None :
            tmpp = self.unpack_pickle()

        if self.fname_ulay : 
            tmpu = self.unpack_ulay()

        # *****

    # ----- methods

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

        if self.verb > 1 : 
            print(" ++ Load {} dsets".format(self.ninfiles))

        # init torch tensor to hold all infile data, dims=(ndsets, nvox)
        self.data = torch.tensor( np.zeros((self.ninfiles, self.nvox), 
                                           dtype=np.float64) )

        # loop over all infiles and load flat data
        for nn in range(self.ninfiles) :
            dset = self.infiles[nn]
            if self.verb > 1 : print(" ++ Load dset {}: {}".format(nn+1, dset))
            img  = nib.load(dset)
            arr  = img.get_fdata()
            self.data[nn, :] = torch.from_numpy(arr[self.ii, self.jj, self.kk])

        return 0

    def unpack_mask(self):
        """Unpack the mask dset, from its name."""

        if self.verb > 1 :  print(" ++ Load mask")

        # load in the data
        self.mask_img = nib.Nifti1Image.from_filename(self.fname_mask)

        return 0

    def set_ijk_coords(self):
        """Define the IJK indices where data is going to be analyzed
        from. Here, this is done by using indices where the mask is
        nonzero."""

        # get IJK indices where mask is nonzero, stored in 3 sep arrays
        img  = nib.load(self.fname_mask)
        mask = img.get_fdata()
        self.ii, self.jj, self.kk = np.nonzero(mask)

        return 0

    def set_voxdim(self):
        """Define the voxel dimension of the dataset, for providing physical
        grid. Here, this is done by using the mask's NIFTI header (pixdim)."""

        try:
            self.voxdim = np.array(self.mask_img.header['pixdim'][1:4], 
                                   dtype=float)
        except:
            sys.exit("** ERROR: Could not get voxel dim from mask")

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

    def unpack_ulay(self):

        """Unpack the ulay dset, from its name
        *** REVISIT THIS """

        #self.ulay_img = nib.Nifti1Image.from_filename(self.fname_ulay)

        if self.verb > 1 :  print(" ++ Load ulay")

        self.ulay_img  = nib.load(self.fname_ulay)
        # Extract the 0-th volume
        self.ulay_data = self.ulay_img.get_fdata()[:, :, :, 0, 0]  

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
        """Nifti affine array from one of the input dsets; if we don't have
        that info from a mask or ulay dset, return an empty 2D array"""
        if self.mask_img :
            return self.mask_img.affine
        elif self.ulay_img :
            return self.mask_img.ulay
        else:
            return np.zeros((0,0))

    @property
    def nifti_qform_code(self):
        """Nifti qform_code int from one of the input dsets; if we don't have
        that info from a mask or ulay dset, return 0"""
        if self.mask_img :
            return int(self.mask_img.header['qform_code'])
        elif self.ulay_img :
            return int(self.mask_img.header['qform_code'])
        else:
            return 0

    @property
    def nifti_sform_code(self):
        """Nifti sform_code int from one of the input dsets; if we don't have
        that info from a mask or ulay dset, return 0"""
        if self.mask_img :
            return int(self.mask_img.header['sform_code'])
        elif self.ulay_img :
            return int(self.mask_img.header['sform_code'])
        else:
            return 0

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
