#!/usr/bin/env python

import os
import numpy   as     np
import nibabel as     nib

from   afnipy  import lib_info_dict as LID
from   afnipy  import afni_util     as au
from   afnipy  import afni_base     as ab

# assorted I/O and other tools for going back and forth from
# volumetric datasets, via nibabel

# ===============================================================================

# read in: dset vol -> python arr
class BabelObj:
    """Object for reading in volumetric dataset (either NIFTI or
BRIK/HEAD) into the Python realm, using nibabel plus enhancements with
3dinfo and other conveniences.

Parameters
----------
inset : str
    data set file name (can include path)
do_log : bool
    output a text log file of all the shell commands run during this
    object's operation/creation; def name: log_nibabel_utils.txt
do_3dinfo : bool
    should we run 3dinfo during this processing?  (def: of course!)
verb: int
    level of verbosity to use while processing (probably not very widely
    used here)

Returns
-------
bo : BabelObj
    an object that includes both nibabel-loaded volume and header info,
    as well as a 3dinfo-created dictionary and some other attributes/methods

    """

    def __init__(self, inset=None, 
                 do_log=False, do_3dinfo=True, verb=1):

        # general variables
        self.verb            = verb
        self.do_log          = do_log
        self.do_3dinfo       = do_3dinfo

        # main data variables
        self.inset           = inset

        # attributes
        self.info            = []           # dict of 3dinfo attributes
        self.nibobj          = None         # loaded nibabel object

        # ----- take action(s)

        # prelim stuff
        if inset :
            tmp1 = self.basic_setup()

            if do_3dinfo :
                tmp2 = self.make_3dinfo_dict()

            ###tmp3 = self.set_datum_type()
            tmp4 = self.load_inset_volume()

        if self.do_log :
            olog = 'log_nibabel_utils.txt'
            au.write_afni_com_log(olog)

    # ----- methods

    def load_inset_volume(self):
        """Use nibabel to load a NIFTI or BRIK/HEAD dset. At this point, the
        self.inset str contains the full header_name, so nibabel can
        load either a NIFTI or BRIK/HEAD dset, which has also been
        shown earlier to exist and be loadable by 3dinfo."""
        
        self.nibobj = nib.load(self.inset)
        
        return 0

    def make_3dinfo_dict(self):
        """Run 3dinfo on the dset for a lot of simple-but-useful bits of info
        stored in this dictionary attribute."""

        self.info = LID.get_all_3dinfo_dset_neatly(self.inset,
                                                   numberize_values=True, 
                                                   remove_dash_in_keys=True)
        
        return 0

    def basic_setup(self):
        """Run through basic checks of what has been input, and fill in any
        further information that is needed (like wdir, etc.)"""

        # check basic requirements

        if not(self.inset) :
            ab.EP("Need to provide an inset")

        # check about existence of dset, its loadability, AND make
        # sure we get a full header_name for it (not an abbreviated
        # version, like NAME+orig, because nibabel can't load that);
        # the header_name will replace the inset string, to ensure
        # loadability
        is_ok, is_nifti, prefix_noext, header_name = \
            au.info_dset_exists_with_names(self.inset)

        if not(is_ok) :
            ab.EP("Failed to load inset:", self.inset)

        # ensure full filename reference, for non-AFNI progs
        self.inset = header_name

        return 0

    # ----- decorators

    @property
    def nb_data_dtype(self):
        """shortcut to nibabel header-parsing method get_data_dtype(); this is
        not a function, though, just a decorator, so call it without
        any parentheses at the end."""
        return self.nibobj.header.get_data_dtype()

    @property
    def nb_header(self):
        """shortcut to getting a copy of the nibabel header (no parentheses)"""
        return self.nibobj.header.copy()

    @property
    def nb_data_arr(self):
        """shortcut to getting a copy of the nibabel data array (no parentheses)"""
        return np.asanyarray(self.nibobj.dataobj).astype(self.nb_data_dtype)

# ------------------------------------------------------------------------------

# write out: python arr -> dset vol

# *** this function is a work in progress, still needing more thought
# *** about getting all header info safe and correct.
def write_arr_to_nifti_simple(A, prefix, head=None, overwrite=True,
                              verb=1):
    """Take an array A and write it out to a NIFTI dataset, hopefully (if
not necessarily) guided by having a header from an associated (e.g.,
input or upstream) dataset.

A key part of this program when providing a header is to ensure that
fields that *might* have changed during processing (e.g., number of
volumes, data_type, cal_min, cal_max, etc.). This function works for
the 'simple' class of cases where voxel and/or matrix dimensions will
*not* need to be adjusted.

We have hardwired in *some* data consistency checks, as much as
possible (like, do spatial dims of A match with those of head?).  We
also hardwire in replacing elements of the final dataset header that
might require being updated, based on the array A (like data_type).

If the prefix (=output dset name) does not end with '.nii' or
'.nii.gz', such a prefix will be automatically added.

Parameters
----------
A : numpy array (likely 3D or 4D)
    data to be written out as a NIFTI dataset
prefix : str
    output dset filename
head : nibabel NIFTI header
    header obtained earlier or from some other related dset using nibabel
overwrite : bool
    flag to overwrite data with pre-existing name
verb : int
    verbosity level

Returns
-------
ecode : int
    exit code, 0=success and 1=failure

    """

    # header info is required
    if head is None :
        msg = "At present, this function *requires* user to provide "
        msg+= "header info via the head kwarg"
        ab.EP(msg)

    # output dset must be nifti
    if not(prefix.endswith('.nii')) and not(prefix.endswith('.nii.gz')) :
        prefix+= '.nii.gz'

    # check shape
    SA = np.shape(A)
    if len(SA) not in [3, 4] :
        msg = "Array does not have 3 or 4 dims, "
        msg+= "instead {}: {}".format(len(SA), ', '.join(SA))
        ab.EP(msg)

    # ... and shape consistency between first 3 dims of A and head info
    SH = head.get_data_shape()
    for ii in range(3) :
        if SA[ii] != SH[ii] :
            msg = "First 3 dims are not consistent for array and header:\n"
            msg+= "array dims : {}\n".format(', '.join(SA[:3]))
            msg+= "head dims  : {}\n".format(', '.join(SH[:3]))
            ab.EP(msg)

    # overwriting or not
    if os.path.isfile(prefix) :
        if not(overwrite) :
            msg = "Cannot write this dataset, since"
            msg+= "it exists already and overwrite was not used:\n"
            msg+= prefix
            ab.EP(msg)
        else:
            msg = "Writing this dataset, which overwrites an existing file:\n"
            msg+= prefix
            ab.WP(msg)

    # get affine from header: different method whether AFNI or NIFTI header
    # Q: how to get all header info right, bc AFNIheader and NIFTIheader have
    #    some different info; here is one example of navigating differences
    #    need to think about sform_code and qform_code carefully, bc nibabel
    #    just defaults to qform_code=0 and sform_code=1; 
    # NB0 : I wonder if 3dinfo should output qform_code and sform_code values
    #       directly, so that we can get those from a single source---the C code
    #       rather than re-use rules here for them.
    try: 
        # AFNI header method
        Haff = head.get_affine()
    except:
        # NIFTI header method
        Haff = head.get_best_affine()

    # this should work whether head is AFNIheader or Nifti?Header type;
    # get element datatype info from array itself
    # NB1: using header=head helps qform_code and sform_code be correct for
    #      NIFTI is input, but not when AFNI is input
    # NB2: using header=head helps xyzt_units be correct; without it, it is 0
    # NB3: using header=head helps pixdim[5:] be more like input values, which
    #      were uniformly 0.0; without it, they appeared to each be 1.0
    # NB4: when using header=head, the regular field value was 'r'; without it,
    #      the field value was empty
    # NB5: if we use header=head, we should remember to purge the extensions
    # NB6: still need to check consequences of using header=head if we have 
    #      changed the number of volumes in A vs what the dset of head had
    nifti_img = nib.Nifti1Image(A, Haff, header=head,
                                dtype=A.dtype)
    # *** might still need to set_qform() and set_sform()

    nifti_img.to_filename(prefix)
    ###nib.save(ovol, prefix)

    return 0

# ============================================================================

if __name__ == "__main__" :

    # example use cases
    print("++ Some examples using Bootcamp data")

    # short
    dset1 = '~/AFNI_data6/afni/anat+orig'
    bo1   = BabelObj(dset1)

    # byte
    dset2 = '~/AFNI_data6/afni/mask.auto.nii.gz'
    bo2   = BabelObj(dset2)

    A2  = bo2.nb_data_arr
    H2  = bo2.nb_header
    ok2 = write_arr_to_nifti_simple(A2, 'ex2.nii', head=H2, overwrite=True)

    # float
    dset3 ='~/AFNI_data6/afni/func_slim+orig.HEAD'
    bo3   = BabelObj(dset3)

    A3  = bo3.nb_data_arr
    H3  = bo3.nb_header
    ok3 = write_arr_to_nifti_simple(A3, 'ex3.nii.gz', head=H3, overwrite=True)
