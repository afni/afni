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

from   vnet_afni  import lib_vnet_test   as VALVT

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

        self.comp_mask        = DEF.DOPTS['comp_mask']
        self.comp_opts        = DEF.DOPTS['comp_opts']
        self.comp_mask_odir   = None         # output dir for comparison items

        self.checkpoint       = DEF.DOPTS['checkpoint']
        self.device           = DEF.DOPTS['device']

        # "preproc_forward" items: each only gets populated if used,
        # so we know to undo that step later (most have supplemental
        # info to help in undoing the step later)
        self.dset_pp_copy       = None       # copy of inset
        self.dset_pp_copy_info  = None       # many 3dinfo items of copy

        self.dset_pp_last       = None       # name of dset to (finally) proc 
        self.dset_pp_obl        = None       # dset that was deobliqued
        self.dset_pp_obl_base   = None       # base of suppl files...
        self.dset_pp_obl_ijkrl  = None       # pre-deobl IJK_TO_DICOM_REAL
        self.dset_pp_obl_ijk    = None       # pre-deobl IJK_TO_DICOM
        self.dset_pp_obl_origin = None       # pre-deobl ORIGIN
        self.dset_pp_orient     = None       # dset that was re-oriented
        self.dset_pp_orient_ori = None       # orient of input
        self.dset_pp_dims       = None       # dset that was zeropadded
        self.dset_pp_dims_mast  = None       # master dset for dims

        # "proc_mask" : calc from vnet and check/refine
        self.dset_proc_last        = None    # primary calc obj
        self.dset_proc_vnet        = None
        self.dset_proc_clust_base  = None
        self.dset_proc_clust       = None
        self.dset_proc_clust_table = None
        self.dset_proc_main        = None

        # "afterproc_reverse" items: go back through any
        # preproc_forward steps and reverse them, so we get back to
        # same grid
        self.dset_ap_last       = None       # dset getting un-altered grid
        self.dset_ap_obl        = None       # dset that was un-deobliqued
        self.dset_ap_orient     = None       # dset that was un-re-oriented
        self.dset_ap_dims       = None       # dset that was un-zeropadded

        # control variables
        self.outdir          = DEF.DOPTS['outdir']         # None or str
        self.workdir         = DEF.DOPTS['workdir']

        # ----- take action(s)

        if user_inobj :
            tmp = self.load_from_inopts()
            if tmp : return

            tmp = self.basic_setup()
            if tmp : return

            tmp = self.make_workdir()
            if tmp : return

            tmp = self.preproc_forward_all()
            if tmp : return

            tmp = self.proc_mask_all()
            if tmp : return

            tmp = self.afterproc_reverse_all()
            if tmp : return

            tmp = self.write_out_prefix()
            if tmp : return

            tmp = self.compare_mask_overlap()
            if tmp : return

            if self.do_clean :
                tmp = self.remove_workdir()
                if tmp : return

    # ----- methods

    def compare_mask_overlap(self):
        """If a comp_mask was provided, run the comparison(s). Otherwise, do
        nothing"""

        if not(self.comp_mask) :
            return 0

        if self.verb : ab.IP("compare with comp_mask")

        BAD_RETURN = -8

        cmd  = 'compare_mask_overlap.tcsh '
        cmd += '{} '.format(self.overwrite)
        cmd += '-inputA {} '.format(self.prefix)
        cmd += '-inputB {} '.format(self.comp_mask)
        cmd += '-ulay   {} '.format(self.inset)
        cmd += '-outdir {} '.format(self.comp_mask_odir)
        cmd += '{} '.format(' '.join(self.comp_opts))
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat : 
            ab.EP1("Failed in final dset: compare with comp_mask")
            return BAD_RETURN

        return 0
        
    def write_out_prefix(self):
        """Copy final dset out. Should be last step. **Add in history**
        """

        if self.verb : ab.IP("final copy: {}".format(self.prefix))

        BAD_RETURN = -7
                
        # keeping updating what the current/latest afterproc dset is
        
        # make new dset to reformat
        cmd  = '3dcopy -overwrite '
        cmd += '{} '.format(self.dset_ap_last)
        cmd += '{}'.format(self.prefix)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat : 
            ab.EP1("Failed in final dset: copying")
            return BAD_RETURN

        # ----- add history to original note of inset (which might not
        # ----- have even had a history
        
        # prepare the history to write
        argv = self.user_inobj.argv
        if '/' in argv[0] :
            # terse-ify full path to the cmd being run
            argv[0] = argv[0].split('/')[-1]
        hist_str = ' NB: several preceding steps via :: '
        hist_str+= ' '.join(argv)

        cmd = '''3dNotes -h "{}" '''.format(hist_str)
        cmd+= '''"{}" '''.format(self.prefix)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat : 
            ab.EP1("Failed in final dset: adding history")
            return BAD_RETURN

        return 0

    def afterproc_reverse_all(self):
        """Undo any of the preproc_forward_all() steps that were done in
        preparation for running the vnet. The goal is to put the mask
        back on the same grid on which it was input, so we check those
        steps in reverse, by seeing what (if any) self.dset_pp_*
        attribute dsets are not None.

        The vnet-estimated mask dset undergoes this set up steps.

        Again, we note that no voxel interpolating will be needed in
        this procedure.
        """

        BAD_RETURN = -7

        # keeping updating what the current/latest afterproc dset is
        self.dset_ap_last = self.dset_proc_last

        if self.dset_pp_dims :
            is_fail = self.afterproc_reverse_dims()
            if is_fail :
                ab.EP1("failed in afterproc step: dims")
                return BAD_RETURN

        if self.dset_pp_orient :
            is_fail = self.afterproc_reverse_orient()
            if is_fail :
                ab.EP1("failed in afterproc step: orient")
                return BAD_RETURN

        if self.dset_pp_obl is not None :
            is_fail = self.afterproc_reverse_obl()
            if is_fail :
                ab.EP1("failed in afterproc step: obliquity")
                return BAD_RETURN

        return 0

    def afterproc_reverse_dims(self):
        """Reverse changes of any zeropadding"""

        if self.verb : ab.IP("afterproc reverse: dims")
        
        BAD_RETURN = -1

        self.dset_ap_dims = self.workdir + '/' + 'dset_21_mask_dims.nii.gz'

        cmd  = '''3dZeropad -overwrite '''
        cmd += '''-master "{}" '''.format(self.dset_pp_dims_mast)
        cmd += '''-prefix "{}" '''.format(self.dset_ap_dims)
        cmd += ''' "{}" '''.format(self.dset_ap_last)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Failed in afterproc reverse step: dims")
            return BAD_RETURN

        # keeping updating what the current/latest preproc dset is
        self.dset_ap_last = self.dset_ap_dims

        return 0

    def afterproc_reverse_orient(self):
        """Reorient dset to match input dset orient, in current afterproc step
        dset"""
        
        if self.verb : ab.IP("afterproc reverse: orient")

        BAD_RETURN = -1

        self.dset_ap_orient = self.workdir + '/' + 'dset_22_mask_orient.nii.gz'

        cmd  = '''3dresample -overwrite '''
        cmd += '''-orient "{}" '''.format(self.dset_pp_orient_ori)
        cmd += '''-inset  "{}" '''.format(self.dset_ap_last)
        cmd += '''-prefix "{}" '''.format(self.dset_ap_orient)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Failed in afterproc reverse step: orient")
            return BAD_RETURN

        # keeping updating what the current/latest preproc dset is
        self.dset_ap_last = self.dset_ap_orient

        return 0

    def afterproc_reverse_obl(self):
        """Return any obliquity in current afterproc step dset, which means
        updating IJK_TO_DICOM, IJK_TO_DICOM_REAL, and ORIGIN. 

        NB: in this step, we make a BRIK/HEAD format dset.

        Odd quirk to note: 3drefit will not produce a status message
        if a command fails because it can't open a particular dset,
        because it will try to keep processing multiple ones.  So, the
        stat checks here are not fully helpful."""
        
        if self.verb : ab.IP("afterproc reverse: obliquity")

        BAD_RETURN = -1

        # use BRIK/HEAD format dset, to modify attributes more cleanly
        av_space = self.dset_pp_copy_info['av_space']
        self.dset_ap_obl = self.workdir + '/' 
        self.dset_ap_obl+= 'dset_23_mask_obl{}.HEAD'.format(av_space)

        # make new dset to reformat
        cmd  = '3dcopy -overwrite '
        cmd += '{} '.format(self.dset_ap_last)
        cmd += '{}'.format(self.dset_ap_obl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat : 
            ab.EP1("Failed in afterproc_reverse: 3dcopy")
            return BAD_RETURN

        # attach new aform to dset
        cmd  = '3drefit -atrfloat IJK_TO_DICOM_REAL '
        cmd += '{} '.format(self.dset_pp_obl_ijkrl)
        cmd += '{}'.format(self.dset_ap_obl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat : 
            ab.EP1("Failed in afterproc_reverse: 3drefit IJK_TO_DICOM_REAL")
            return BAD_RETURN

        # attach new IJK_TO_DICOM to dset
        cmd  = '3drefit -atrfloat IJK_TO_DICOM '
        cmd += '{} '.format(self.dset_pp_obl_ijk)
        cmd += '{}'.format(self.dset_ap_obl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat : 
            ab.EP1("Failed in afterproc_reverse: 3drefit IJK_TO_DICOM")
            return BAD_RETURN

        # attach new ORIGIN to dset
        cmd  = '3drefit -atrfloat ORIGIN '
        cmd += '{} '.format(self.dset_pp_obl_origin)
        cmd += '{}'.format(self.dset_ap_obl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat : 
            ab.EP1("Failed in afterproc_reverse: 3drefit ORIGIN")
            return BAD_RETURN

        # keeping updating what the current/latest preproc dset is
        self.dset_ap_last = self.dset_ap_obl

        return 0

    def proc_mask_all(self):
        """Do all stages of mask generation, and possible refining"""

        BAD_RETURN = -4

        is_fail = self.proc_mask_vnet()
        if is_fail :
            ab.EP1("failed in proc mask step: vnet")
            return BAD_RETURN

        is_fail = self.proc_mask_clust()
        if is_fail :
            ab.EP1("failed in proc mask step: clust")
            return BAD_RETURN

        return 0

    def proc_mask_vnet(self):
        """Run the vnet model/checkpoint to get the mask estimate """

        self.dset_proc_vnet = self.workdir + '/' + 'dset_10_mask_vnet.nii.gz'

        VTO = VALVT.VnetTestObj(self.dset_pp_last, 
                                prefix=self.dset_proc_vnet,
                                checkpoint=self.checkpoint,
                                device=self.device, 
                                do_overwrite=True, verb=self.verb)

        self.dset_proc_last = self.dset_proc_vnet

        return 0

    def proc_mask_clust(self):
        """Run 3dClusterize on the vnet output to avoid any little tiny extra
        bits possibly floating around"""

        BAD_RETURN = -10

        # clusterize 

        self.dset_proc_clust_base  = self.workdir + '/' + 'dset_11_mask_clust'
        self.dset_proc_clust       = self.dset_proc_clust_base + '.nii.gz'
        self.dset_proc_clust_table = self.dset_proc_clust_base + '_table.1D'

        cmd  = '3dClusterize -overwrite '
        cmd += '-idat 0 -ithr 0 -bisided -0.5 0.5 -NN 2 '
        cmd += '-pref_map "{}" '.format(self.dset_proc_clust)
        cmd += '-inset "{}" '.format(self.dset_proc_last)
        cmd += '> "{}" '.format(self.dset_proc_clust_table)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat : 
            ab.EP1("Failed in proc mask: clust check")
            return BAD_RETURN

        self.dset_proc_last = self.dset_proc_clust

        # ... and get largest cluster for result
        self.dset_proc_main = self.workdir + '/' + 'dset_12_mask_main.nii.gz'
        
        cmd  = '3dcalc '
        cmd += '-a "{}<1>" '.format(self.dset_proc_last)
        cmd += '-expr "step(a)" '
        cmd += '-prefix "{}" '.format(self.dset_proc_main)
        cmd += '-datum short -nscale'
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat : 
            ab.EP1("Failed in proc mask: clust select")
            return BAD_RETURN

        self.dset_proc_last = self.dset_proc_main

        return 0

    def preproc_forward_all(self):
        """Go through all steps to prepare the inset for processing.  This
        includes dealing with any obliquity, dset re-orienting, and/or
        zeropadding to appropriate spatial dims. NB: none of these involve
        regridding, in the sense of interpolating voxel values.

        The input anatomical undergoes this set of steps.

        A later function afterproc_reverse_all() will deal with
        undoing these to prepare the output mask to match the input
        grid.
        """

        BAD_RETURN = -3

        # can determine which preproc_forward steps need to be done,
        # via single header check at the beginning
        is_fail, do_obl, do_ori, do_mat = self.preproc_forward_header_check()
        if is_fail :
            ab.EP1("failed in preproc step: header check")

        is_fail = self.preproc_forward_copy()
        if is_fail :
            ab.EP1("failed in preproc step: copy")
            return BAD_RETURN

        if do_obl :
            is_fail = self.preproc_forward_obl()
            if is_fail :
                ab.EP1("failed in preproc step: obliquity")
                return BAD_RETURN

        if do_ori :
            is_fail = self.preproc_forward_orient()
            if is_fail :
                ab.EP1("failed in preproc step: orient")
                return BAD_RETURN

        if do_mat :
            is_fail = self.preproc_forward_dims()
            if is_fail :
                ab.EP1("failed in preproc step: dims")
                return BAD_RETURN

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

        # save a copy into an object attribute here (but keep
        # referring to D below, for simplicity)
        self.dset_pp_copy_info = copy.deepcopy(D)

        # use header values to set do_* values (and store some pieces
        # of information now, for undoing later; some have to be set
        # later, though, like the dims)

        do_obl = D['is_oblique']

        do_ori = int(D['orient'] != DEF.model_orient)
        if do_ori :
            self.dset_pp_orient_ori = D['orient']

        # are all spatial dimensions multiples of magic number?
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
        """Remove and preserve any obliquity in current preproc step dset.
        There are a few supplementary text files saved in this case."""
        
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

        # ... and save supplementary info from current inset, used later
        self.dset_pp_obl_ijk    = self.dset_pp_obl_base + '_ijk2dcm.1D'
        self.dset_pp_obl_ijkrl  = self.dset_pp_obl_base + '_ijk2dcmrl.1D'
        self.dset_pp_obl_origin = self.dset_pp_obl_base + '_origin.1D'

        cmd  = '''cat_matvec -ONELINE '''
        cmd += '''{}::IJK_TO_DICOM '''.format(self.dset_pp_last)
        cmd += '''> "{}" '''.format(self.dset_pp_obl_ijk)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat :
            ab.EP1("Failed in preproc forward step: obliquity, ijk2dcm")
            return BAD_RETURN

        cmd  = '''cat_matvec -ONELINE '''
        cmd += '''{}::IJK_TO_DICOM_REAL '''.format(self.dset_pp_last)
        cmd += '''> "{}" '''.format(self.dset_pp_obl_ijkrl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat :
            ab.EP1("Failed in preproc forward step: obliquity, ijk2dcmrl")
            return BAD_RETURN

        cmd  = '''3dinfo -o3 "{}" '''.format(self.dset_pp_last)
        cmd += '''> "{}" '''.format(self.dset_pp_obl_origin)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat :
            ab.EP1("Failed in preproc forward step: obliquity, ijk2dcmrl")
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

    def preproc_forward_dims(self):
        """Make sure dataset spatial dims are a multiple of 16 (for kernel
        progression)"""

        if self.verb : ab.IP("preproc forward: dims")
        
        BAD_RETURN = -1

        self.dset_pp_dims = self.workdir + '/' + 'dset_03_dims.nii.gz'

        cmd  = '''3dZeropad -overwrite '''
        cmd += '''-pad2mult "{}" '''.format(DEF.model_matrix)
        cmd += '''-prefix "{}" '''.format(self.dset_pp_dims)
        cmd += ''' "{}" '''.format(self.dset_pp_last)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Failed in preproc forward step: dims")
            return BAD_RETURN

        # save the input dset here to be the master dset on afterproc_reverse
        self.dset_pp_dims_mast = self.dset_pp_last

        # keeping updating what the current/latest preproc dset is
        self.dset_pp_last = self.dset_pp_dims

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

        if io.comp_mask is not None :
            self.comp_mask = io.comp_mask
        if io.comp_opts is not None :
            self.comp_opts = io.comp_opts

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

        # (req) prefix is needed; if it doesn't have nifti ext, give it one
        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")
        else:
            if not(self.prefix.endswith('.nii')) and \
               not(self.prefix.endswith('.nii.gz')) :
                self.prefix += '.nii.gz'
                                    
        # (opt) mask
        if self.comp_mask :
            nfail = au.check_all_dsets_exist([self.comp_mask], 
                                             label='comp_mask',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load comp_mask")

            # comp_mask grid must match inset
            is_fail = au.check_all_dsets_same_grid([self.inset, self.comp_mask],
                                                   label='inset and comp_mask')

            # make the output dir for comp mask stuff, based on
            # prefix, which we know will be a nifti dset
            if self.prefix.endswith('.nii.gz') :
                self.comp_mask_odir = self.prefix[:-7] + '_comp'
            elif self.prefix.endswith('.nii') :
                self.comp_mask_odir = self.prefix[:-4] + '_comp'
            else:
                msg = "Failed to make comp_mask_odir, from "
                msg+= "prefix that is somehow not nifti (???): "
                msg+= "{}".format(self.prefix)
                ab.EP(msg)

            # make sure prohibited opts are not used here
            if len(self.comp_opts) :
                for opt in DEF.LIST_all_comp_opts_nono :
                    if opt in self.comp_opts :
                        ab.EP("Cannot manage opt '{}' here".format(opt))
        else:
            # warn user they are likely missing comp_mask, or forgot
            # to delete the opts for it
            if len(self.comp_opts) :
                msg = "Using -comp_opts without -comp_mask will have "
                msg+= "no effect. Consider using either both or neither."
                ab.WP(msg)

        # (opt) checkpoint
        if self.checkpoint :
            is_ok = os.path.isfile(self.checkpoint)
            if not(is_ok) :
                ab.EP("Failed to load checkpoint")


        if os.path.isfile(self.prefix) and not(self.overwrite) :
            msg = "The prefix '{}' dset exists already, ".format(self.prefix)
            msg+= "and you must either (re)move it or use '-overwrite'"
            ab.EP(msg)

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
        """name of anatomical dset in forward preproc: orient changed (if
        done)"""
        return self.workdir + '/' + 'dset_02_orient.nii.gz'

    @property
    def wdir_pp_dims(self):
        """name of anatomical dset in forward preproc: dims are changed (if
        done)"""
        return self.workdir + '/' + 'dset_03_dims.nii.gz'


# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

