#!/usr/bin/env python

import numpy   as     np
import nibabel as     nib

from   afnipy  import lib_info_dict as LID
from   afnipy  import afni_util     as au
from   afnipy  import afni_base     as ab

# assorted I/O and other tools for going back and forth from
# volumetric datasets, via nibabel


# NOTE
# *** probably want to control types differently!  
# *** not sure nibabel always picks a good type


# ===============================================================================

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
        # *** probably want to control types differently!  
        # *** not sure nibabel always picks a good type
        return np.asanyarray(self.nibobj.dataobj).astype(self.nb_data_dtype)

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

    # float
    dset3 ='~/AFNI_data6/afni/mask.auto.nii.gz'
    bo3   = BabelObj(dset3)

