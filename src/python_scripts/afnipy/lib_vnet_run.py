#!/usr/bin/env python

# A library of functions for dealing with 3dBrainTeaser
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : start of vnet processing for skullstripping
# ============================================================================

import sys, os, copy, glob

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au
from   afnipy import lib_info_dict      as lid
from   afnipy import lib_info_items     as lii
from   afnipy import lib_vnet_defs      as DEF

from   vnet_afni import lib_test_vnet   as VALTV

# ----------------------------------------------------------------------------


# ============================================================================

class MainObj:
    """Object for ***

Parameters
----------
inobj : InOpts object 
    object constructed from running TEMPLATE_tool.py on the command
    line. At present, the only way to really provide inputs here.

    """

    def __init__(self, user_inobj=None):

        # ----- set up attributes

        # main input variables
        self.status           = 0                       # not used
        self.user_opts        = DEF.DOPTS['user_opts']  # command the user ran
        self.user_inobj       = user_inobj

        # general variables
        self.verb             = DEF.DOPTS['verb']
        self.overwrite        = DEF.DOPTS['overwrite']
        self.do_clean         = DEF.DOPTS['do_clean']
        self.do_log           = DEF.DOPTS['do_log']

        # main data variables
        self.inset            = DEF.DOPTS['inset']
        self.prefix           = DEF.DOPTS['prefix']

        self.mask             = DEF.DOPTS['mask']
        self.checkpoint       = DEF.DOPTS['checkpoint']
        self.device           = DEF.DOPTS['device']

        # "preproc_forward" items: each only gets populated if used,
        # so we know to undo that step later (most have supplemental
        # info to help in undoing the step later)
        self.dset_pp_last       = None           # name of dset to actually proc 
        self.dset_pp_copy       = None           # 
        self.dset_pp_obl        = None           # dset that was deobliqued
        self.dset_pp_obl_base   = None           # base of suppl files, to use later
        self.dset_pp_orient     = None           # dset that was re-oriented
        self.dset_pp_orient_ori = None           # orient of input, to match later
        self.dset_pp_matrix     = None           # dset that was zeropadded
        self.dset_pp_matrix_n3  = None           # mat of input, to match to later

        # control variables
        self.outdir          = DEF.DOPTS['outdir']         # None or str
        self.workdir         = DEF.DOPTS['workdir']

        # ----- take action(s)

        # prelim stuff
        if user_inobj :
            tmp = self.load_from_inopts()
            if tmp : return

            tmp = self.basic_setup()
            if tmp : return

            tmp = self.make_workdir()
            if tmp : return

            tmp = self.preproc_forward_all()
            if tmp : return

            tmp = self.run_vnet()
            if tmp : return

            if self.do_clean :
                tmp = self.remove_workdir()
                if tmp : return

    # ----- methods

    def run_vnet(self):
        """ """

        VTO = VALTV.VnetTestObj(self.dset_pp_last, prefix=self.prefix, 
                                mask=self.mask, checkpoint=self.checkpoint,
                                device=self.device, 
                                do_overwrite=False, verb=self.verb)

        return 0


    def preproc_forward_all(self):
        """Go through all steps to prepare the inset for processing.  This
        includes dealing with any obliquity, dset re-orienting, and/or
        zeropadding to appropriate matrix dims.

        A later function will deal with undoing these to prepare the
        output mask to match the input grid.
        """

        # can determine which preproc_forward steps need to be done,
        # via single header check at the beginning
        is_fail, do_obl, do_ori, do_mat = self.preproc_forward_header_check()
        if is_fail :
            ab.EP1("failed in preproc step: header check")

        is_fail = self.preproc_forward_copy()
        if is_fail :
            ab.EP1("failed in preproc step: copy")

        if do_obl : 
            is_fail = self.preproc_forward_obl()
            if is_fail :
                ab.EP1("failed in preproc step: obliquity")

        if do_ori :
            is_fail = self.preproc_forward_orient()
            if is_fail :
                ab.EP1("failed in preproc step: orient")

        if do_mat :
            is_fail = self.preproc_forward_matrix()
            if is_fail :
                ab.EP1("failed in preproc step: matrix")

        # NB: at this point, self.dset_pp_last should be all set for
        # the 'real' processing with the VNET. After the mask has been
        # estimated, we traverse the trail of created "dset_pp" dsets
        # created in the preproc_forward stage, inverting what was
        # done along the way.

        return 0

    def preproc_forward_header_check(self):
        """Check header properties of input dset, to figure out which
        preprocessing steps will need to be done.

        Returns: is_fail, do_obl, do_ori, do_mat"""

        do_obl = 0
        do_ori = 0
        do_mat = 0        

        BAD_RETURN = (-3, do_obl, do_ori, do_mat)

        # get dictionary of all header items
        is_fail, D = lid.get_all_3dinfo_dset_neatly(self.inset,
                                                    numberize_values=True)
        if is_fail :
            ab.EP1("failed in preproc step: header check")
            return BAD_RETURN

        # use header values to set do_* values (and store some pieces
        # of information now, for undoing later; some have to be set
        # later, though, like the matrix)

        do_obl = D['is_oblique']

        do_ori = int(D['orient'] != DEF.model_orient)
        if do_ori :
            self.dset_pp_orient_ori = D['orient']

        # are all matrix dimensions multiples of magic number?
        for val in D['n3'] :
            if val % DEF.model_matrix :
                do_mat = 1

        return 0, do_obl, do_ori, do_mat

    def preproc_forward_copy(self):
        """Copy inset to wdir"""
        
        if self.verb : ab.IP("preproc forward: copy")

        BAD_RETURN = -1

        self.dset_pp_copy = self.workdir + '/' + 'dset_00_copy.nii.gz'

        cmd  = '''3dcalc -overwrite '''
        cmd += '''-a "{}" '''.format(self.inset)
        cmd += '''-expr "a" '''
        cmd += '''-prefix "{}" '''.format(self.dset_pp_copy)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Failed in preproc forward step: copy")
            return BAD_RETURN

        # keeping updating what the current/latest preproc dset is
        self.dset_pp_last = self.dset_pp_copy

        return 0

    def preproc_forward_obl(self):
        """Remove and preserve any obliquity in current preproc step dset"""
        
        if self.verb : ab.IP("preproc forward: obliquity")

        BAD_RETURN = -1

        # in this case, useful to have a separate base, bc we have
        # supplementary dsets to use later
        self.dset_pp_obl_base = self.workdir + '/' + 'dset_01_obl'
        self.dset_pp_obl      = self.dset_pp_obl_base + '.nii.gz'

        cmd  = '''obliquity_remover.py -overwrite '''
        cmd += '''-inset  "{}" '''.format(self.dset_pp_last)
        cmd += '''-prefix "{}" '''.format(self.dset_pp_obl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Failed in preproc forward step: obliquity")
            return BAD_RETURN

        # keeping updating what the current/latest preproc dset is
        self.dset_pp_last = self.dset_pp_obl

        return 0

    def preproc_forward_orient(self):
        """Reorient data to match training data value, in current preproc step
        dset"""
        
        if self.verb : ab.IP("preproc forward: orient")

        BAD_RETURN = -1

        self.dset_pp_orient = self.workdir + '/' + 'dset_02_orient.nii.gz'

        cmd  = '''3dresample -overwrite '''
        cmd += '''-orient "{}" '''.format(DEF.model_orient)
        cmd += '''-inset  "{}" '''.format(self.dset_pp_last)
        cmd += '''-prefix "{}" '''.format(self.dset_pp_orient)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Failed in preproc forward step: orient")
            return BAD_RETURN

        # keeping updating what the current/latest preproc dset is
        self.dset_pp_last = self.dset_pp_orient

        return 0

    def preproc_forward_matrix(self):
        """Make sure dataset matrix is a multiple of 16 (for kernel
        progression)"""

        if self.verb : ab.IP("preproc forward: matrix")
        
        BAD_RETURN = -1

        self.dset_pp_matrix = self.workdir + '/' + 'dset_03_matrix.nii.gz'

        cmd  = '''3dZeropad -overwrite '''
        cmd += '''-pad2mult "{}" '''.format(DEF.model_matrix)
        cmd += '''-prefix "{}" '''.format(self.dset_pp_matrix)
        cmd += ''' "{}" '''.format(self.dset_pp_last)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Failed in preproc forward step: matrix")
            return BAD_RETURN

        # save the matrix of the input dset
        is_fail, n3 = lii.get_n3(self.dset_pp_last)
        if is_fail :
            return BAD_RETURN
        self.dset_pp_matrix_n3 = n3

        # keeping updating what the current/latest preproc dset is
        self.dset_pp_last = self.dset_pp_matrix

        return 0

    def load_from_inopts(self):
        """Populate the input values using the command line interface
        input. The user information is provided as the self.user_inobj
        object, which gets parsed and redistributed here.
        """
        
        if not(self.user_inobj) :
            ab.WP("No user_inobj? Nothing to do.")
            return 0

        # shorter name to use, and less confusing with 'self' usage
        io = self.user_inobj

        if io.user_opts is not None :
            self.user_opts = io.user_opts

        # general variables
        if io.verb is not None :
            self.verb = io.verb
        if io.overwrite is not None :
            self.overwrite = io.overwrite
        if io.do_clean is not None :
            self.do_clean = io.do_clean
        if io.do_log is not None :
            self.do_log = io.do_log

        # main data variables
        if io.inset is not None :
            self.inset = io.inset
        if io.prefix is not None :
            self.prefix = io.prefix

        if io.mask is not None :
            self.mask = io.mask
        if io.checkpoint is not None :
            self.checkpoint = io.checkpoint

        if io.device is not None :
            self.device = io.device

        # control variables
        if io.workdir is not None :
            self.workdir = io.workdir
        if io.outdir is not None :          # or this could be derived
            self.outdir = io.outdir

        return 0


    def basic_setup(self):
        """Run through basic checks of what has been input, and fill in any
        further information that is needed (like wdir, etc.)"""

        # check basic requirements

        if not(self.inset) :
            ab.EP("Need to provide an inset")
        else:
            nfail = au.check_all_dsets_exist([self.inset], label='inset', 
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load inset")

        # (opt) mask
        if self.mask :
            nfail = au.check_all_dsets_exist([self.mask], label='mask',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load mask")

            # mask grid must match inset
            is_fail = au.check_all_dsets_same_grid([self.inset, self.mask],
                                                   label='inset and mask')

        # (opt) checkpoint
        if self.checkpoint :
            is_ok = os.path.isfile(self.checkpoint)
            if not(is_ok) :
                ab.EP("Failed to load checkpoint")

        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")

        if self.device not in DEF.LIST_all_device :
            msg = "Unrecognized device '{}'. ".format(self.device)
            msg+= "Must use one from list: {}".format(DEF.STR_all_device)
            ab.EP(msg)

        # generate basic items

        # generate wdir with random component, if none provided
        if not(self.workdir) :
            cmd  = '3dnewid -fun11'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            rstr = com.so[0].strip()
            self.workdir = '__wdir_brainteaser_' + rstr

        # convert bool-ish opts to bools
        self.do_clean       = au.convert_to_bool_yn10(self.do_clean)

        return 0

    def make_workdir(self):
        """Make the workdir"""

        BAD_RETURN = -10

        cmd  = '\\mkdir -p "{}" '.format(self.workdir)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Could not make workdir")
            return BAD_RETURN

        return 0

    def remove_workdir(self):
        """Remove the workdir"""

        BAD_RETURN = -10

        # see if we have a workdir to remove (if not, just return)
        if not(os.path.isdir(self.workdir)) :
            return 0

        cmd  = '\\rm -rf "{}" '.format(self.workdir)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Could not remove workdir: {}".format(self.workdir))
            return BAD_RETURN

        return 0

    # ----- decorators

    @property
    def ninset(self):
        """number of insets"""
        return len(self.inset)

    @property
    def wdir_pp_copy(self):
        """name of anatomical dset copied into workdir"""
        return self.workdir + '/' + 'dset_00_copy.nii.gz'

    @property
    def wdir_pp_obliquity_base(self):
        """base of anatomical dset in forward preproc: deobliqued (if done);
        useful to have the base separately for this step, because
        there are supplementary dsets involved."""
        return self.workdir + '/' + 'dset_01_no_obl'

    @property
    def wdir_pp_obliquity(self):
        """name of anatomical dset in forward preproc: deobliqued (if done)"""
        return self.wdir_pp_obliquity_base + '.nii.gz'

    @property
    def wdir_pp_orient(self):
        """name of anatomical dset in forward preproc: orient changed (if done)"""
        return self.workdir + '/' + 'dset_02_orient.nii.gz'

    @property
    def wdir_pp_matrix(self):
        """name of anatomical dset in forward preproc: matrix changed (if done)"""
        return self.workdir + '/' + 'dset_03_matrix.nii.gz'


# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

