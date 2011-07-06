#!/usr/bin/env python

# note: in the script, runs are 1-based (probably expected)

import string, sys, os
from time import asctime

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *
from db_mod import *
import ask_me

# ----------------------------------------------------------------------
# globals

g_history = """
    afni_proc.py history:

    1.0  Dec 20, 2006 : initial release
    1.1  Dec 20, 2006 : added -regress_use_stim_files
    1.2  Dec 21, 2006 : help, start -ask_me, updated when to use -iresp/ideal
    1.3  Dec 22, 2006 : change help to assumme ED's stim_times files exist
    1.4  Dec 25, 2006 : initial -ask_me
    1.5  Dec 27, 2006 : ask_me help
    1.6  Dec 28, 2006 : -gylsym examples, min(a,b) in scale block
    1.7  Jan 03, 2007 : help updates, no blank '\\' line from -gltsym
    1.8  Jan 08, 2007 :
         - changed default script name to proc.SUBJ_ID, and removed -script
           from most examples
         - added options -bash, -copy_files, -volreg_zpad, -tlrc_anat,
           -tlrc_base, -tlrc_no_ss, -tlrc_rmode, -tlrc_suffix
    1.9  Jan 09, 2007 : added aligned line wrapping (see afni_util.py)
    1.10 Jan 12, 2007 : set subj = $argv[1], added index to -glt_label in -help
    1.11 Jan 12, 2007 :
         - added options -move_preproc_files, -regress_no_motion
         - use $output_dir var in script, and echo version at run-time
         - append .$subj to more output files 
    1.12 Jan 16, 2007 : allow no +view when using -tlrc_anat
    1.13 Jan 17, 2007 : if -tlrc_anat, apply default option '-tlrc_suffix NONE'
    1.14 Jan 26, 2007 :
         - if only 1 run, warn user, do not use 3dMean
         - changed all True/False uses to 1/0 (for older python versions)
    1.15 Feb 02, 2007 :
         - output float for -blur_size
         - put execution command at top of script
    1.16 Feb 21, 2007 :
         - added optional 'despike' block
         - added options -do_block and -despike_opts_3dDes
    1.17 Feb 27, 2007 :
         - volreg_align_to defaults to 'third' (was 'first')
         - added +orig to despike input
         - added 'empty' block type, for a placeholder
    1.18 Mar 15, 2007 : minor changes on the ides of March (oooooooh...)
         - x1D output file uses x1D suffix
         - removed now unneeded -full_first option in 3dDeconvolve
    1.19 Mar 19, 2007 : allow for dataset TR stored in depreciated ms
    1.20 Mar 25, 2007 : added -help for long-existing -regress_use_stim_files
    1.21 Apr 19, 2007 : apply +orig in 1-run mean using 3dcopy
    1.22 May 08, 2007 :
         - change read_options() to be compatible with python version 2.2
         - '-basis_normall 1' is no longer used by default
         - rename -regress_no_stim_times to -regress_use_stim_files
    1.23 Jun 01, 2007 :
         - changed name of Xmat to X.xmat.1D
         - by default, apply -xjpeg in 3dDeconvolve
    1.24 Jun 04 2007 : added -scale_no_max
    1.25 Jun 27 2007 : on error, display failed command
    1.26 Oct 03 2007 : set default polort based on run length (like 3dDecon)
    1.27 Nov 26 2007 : added -volreg_interp, default is -cubic (was Fourier)
    1.28 Jan 22 2008 : estimating smoothness
         - added -regress_est_blur_errts, -regress_est_blur_epits options
           for estimating the blur in the EPI and errts data
         - added -regress_no_mask, -regress_errts_prefix and -show_valid_opts
    1.29 Jun 12 2008 : move code to afni_util.get_dset_reps_tr
    1.30 Jun 30 2008 : added -gen_epi_review and -no_epi_review options
    1.31 Sep 23 2008 : added -remove_preproc_files
    1.32 Oct 27 2008 : added -regress_motion_file
    1.33 Dec 10 2008 :
        - allow NIfTI datasets as input (but process as AFNI)
        - added -regress_extra_stim_files and -regress_extra_stim_labels
        - added -regress_RONI and -volreg_base_dset (for Jill Weisberg)
        - moved unloved g_help_string to db_mod_py
    1.34 Feb 17 2009 : added -regress_reml_exec and -regress_3dD_stop
    1.35 Mar 12 2009 :
        - if despiking and no regression mask, apply -nomask
        - added 'MASKING NOTE', to suggest no regresion mask until group space
    1.36 Mar 24 2009 :
        * -regress_no_mask is now the default *
        - added -regress_apply_mask
    1.37 Mar 25 2009 : allow +tlrc processing (+view comes from data)
    1.38 Mar 26 2009 : added helpstr to options
    1.39 Apr 01 2009 :
        - by default, the script will now terminate on any error
        - added -exit_on_error, -check_setup_errors
        - whine about block order problems
    1.40 Apr 01 2009 :
        - added 'ricor' processing block, for RETROICOR regressor removal
          (per-run method only, still needs '-help')
          options: -ricor_regs, -ricor_regs_nfirst, -ricor_polort,
                   -ricor_regress_solver, -ricor_regress_method
        - small format changes
    1.41 Apr 09 2009 : ricor changes
        - added 'across-runs' ricor_regress_method
        - added ricor information and usage to help (see "RETROICOR NOTE")
        - maintain unscaled shorts if they are input
        - added -ricor_datum
    1.42 Apr 11 2009 :
        - added -volreg_regress_per_run
        - fixed use of -regress_errts_prefix with blur est
    1.43 Apr 23 2009 : updates to help and tcsh options
        - added -f as a recommended tcsh option
        - added help section 'SCRIPT EXECUTION NOTE'
        - reordered help: intro, BLOCKS, DEFAULTS, EXAMPLES, NOTEs, OPTIONS
        - shifted execution command to separate line
    1.44 May 08 2009 : added tlrc (anat) as a processing block
    1.45 May 12 2009 : fixed 'cat' of 'across-runs' ricor regressors
    1.46 May 14 2009 : no 'rm rm.*' if such files were not created
    1.47 May 15 2009 : added -volreg_tlrc_warp, to warp at volreg step
    1.48 May 21 2009 :
        - added 'align' processing block (anat to EPI)
        - added -volreg_align_e2a (do EPI to anat alignment in volreg step)
    1.49 May 27 2009 :
        - added -volreg_warp_dxyz
        - if align a2e, add -no_ss to @auto_tlrc
        - for varying run lengths, fixed application of '-volreg_align_to last'
          and the -regress_est_blur_* options
          (blur estimation loops were modified for this)
        - warping to new grid truncates to 2 significant bits (if < 2 mm)
    1.50 May 28 2009 : example updates for AFNI_data4 and new options
    1.51 May 29 2009 :
        - added -execute option (to execute processing script)
        - fail on block options that have no such block applied
    1.52 Jun 08 2009 :
        - added -despike_mask option
        - fixed missing block warning
    1.53 Jun 11 2009 :
        - in mask block, try to create anat and group masks
        - added -mask_apply option, for choosing mask to apply to regression
        - added -align_opts_aea, for extra opts to align_epi_anat.py
    2.0  Jun 26 2009 : process method update: suggest processing in group space
        - mask warped EPI by its extents (at volreg step)
        - added -volreg_no_extent_mask, to block this masking
        - added 'extents' to list of mask in -mask_apply
        - change block dividers to more visual '===' with block names
    2.01 Jul 07 2009 : added warning to edit script on use of dmBLOCK
    2.02 Jul 27 2009 : used -slibase_sm instead of -slibase in 3dREMLfit
    2.03 Jul 29 2009 : fixed creation of extents mask when only 1 run
    2.04 Aug 06 2009 : fixed problems found by I Mukai and K Bahadur:
        - fixed -volreg_align_to base as applied in align_epi_anat.py
        - fixed blur 'averages' computation when only one run
    2.05 Aug 10 2009 :
        - Changed default min grid truncation from 2 significant bits to 3
          when applying -volreg_tlrc_warp/-volreg_align_e2s.
    2.06 Aug 13 2009 : added -volreg_tlrc_adwarp, to apply manual tlrc xform
    2.07 Aug 14 2009 : added -align_epi_ext_dset, to align anat to external EPI
    2.08 Aug 21 2009 : added -regress_censor_motion and -regress_censor_prev
                       motivated by L Thomas and B Bones
    2.09 Aug 25 2009 :
        - change the censor_motion prefix from '$subj' to 'motion_$subj'
        - if volreg block, always create 'motion_${subj}_enorm.1D'
    2.10 Aug 26 2009 : explicitly nuke negatives in scale block
    2.11 Aug 27 2009 : added -regress_local_times/-regress_global_times
    2.12 Aug 27 2009 : fixed motion_ in '3dD -censor', found by B Bones
    2.13 Oct 19 2009 :
        - added -blur_in_mask, to apply 3dBlurInMask (a.o.t. 3dmerge)
        - added -blur_in_automask and -blur_opts_BIM
        - added -sep_char (which probably needs more work) for Jill Weisberg
        - added -subj_curly (applied when -sep_char is '_')
    2.14 Nov 16 2009 : allow motion censoring with varying run lengths
    2.15 Jan 15 2010 : added -regress_fout yes/no option
    2.16 Jan 21 2010 :
        - added -tlrc_opts_at for adding options to @auto_tlrc
        - changed max(0,...) to *step(a)*step(b) in scaling block
    2.17 Mar 03 2010 : when censoring, create uncensored ideals and sum
    2.18 Mar 08 2010 : minor: changed option order in some examples
    2.19 Mar 18 2010 : minor: help updates to alignment options
    2.20 Mar 18 2010 : deal with args having '\\n' (from quoted newlines)
    2.21 Mar 23 2010 : added -regress_compute_fitts, to save memory in 3dD
    2.22 Mar 28 2010 : applied fitts computation to REML case
    2.23 Apr 26 2010 : added -regress_opts_reml
    2.24 May 12 2010 : added -regress_censor_first_trs
    2.25 May 29 2010 :
        - fixed use of -volreg_regress_per_run and -regress_censor_motion pair
          (problem noted by D Drake)
    2.26 Jun 04 2010 :
        - if only one regressor, use 1dcat for "sum" ideal
        - added -outlier_count, default to "yes"
        - outlier counting is now at end of tcat block
    2.27 Jun 09 2010 :
        - added -regress_censor_outliers and -regress_skip_first_outliers
        - specified 'auto block:' in block headers for those not chosen by user
    2.28 Jun 10 2010 : fixed copying EPI and anat as NIfTI
          (problem noted by S Tambalo)
    2.29 Jun 17 2010 : apply default polort in 3dToutcount
    2.30 Jun 17 2010 :
        - 3dToutcount detrending now defaults to Legendre polynomials and
          can so exceed polort 3 (limit found by I Mukai and K Bahadur)
        - added options -outlier_legendre and -outlier_polort
    2.31 Jul 14 2010 : added -mask_test_overlap and -regress_cormat_warnigns
    2.32 Jul 19 2010 : added -check_afni_version and -requires_afni_version
    2.33 Jul 22 2010 : added -regress_run_clustsim and -regress_opts_CS
    2.34 Aug 02 2010 :
        - check that stim_file/_time files match datasets
        - check for existence of input datasets
        - added -test_stim_files and -test_for_dsets options
        - now depends on lib_afni1D
    2.35 Aug 04 2010 :
        - added -regress_CS_NN, default to 123
        - changed 3dClustSim to use -both instead of just -niml
        - changed prefix to ClustSim (so resulting .1D files are not removed)
        - if request for ClustSim, require blur estimation
    2.36 Aug 04 2010 :
        - allow married timing files (needed for -test_stim_files)
        - added -keep_script_on_err (NEW default: delete script on error)
    2.37 Oct 20 2010 : added -tcat_remove_last_trs, -ricor_regs_rm_nlast
    2.38 Nov 04 2010 :
        - create sum_ideal.1D is now the default
        - allow varying basis functions (affects ideals/iresp output)
        - added -regress_no_ideal_sum, -regress_basis_multi
    2.39 Nov 04 2010 : no X.full_length.xmat.1D, use X.uncensored.xmat.1D
    2.40 Nov 10 2010 : added new NOTE sections for ANAT/EPI ALIGNMENT to -help
    2.41 Nov 18 2010 :
        - fixed stim_files to stim_times conversion after multi_basis change
        - problem noted by M Weber
    2.42 Nov 22 2010 : improved line wrapping
    2.43 Dec 14 2010 :
        - fixed problem with timing file tests on 'empty' files with '*'
        - problem noted by C Deveney and R Momenan
    2.44 Dec 16 2010 : small changes to file type warnings
    2.45 Jan 13 2011 : small changes to warnings for missing stimulus files
    2.46 Mar 07 2011 : make proc script executable
    2.47 Mar 14 2011 : if no mask but have extents, apply in scale
    2.48 Mar 15 2011 : use X.nocensor.1D (just to save 2 spaces)
    2.49 Apr 22 2011 :
        - if manual tlrc and -volreg_tlrc_adwarp, also transform extents mask
          (noted by J Britton)
        - if -regress_reml_exec, insert 3dClustSim table in stats_REML
          (noted by R Momenan)
    2.50 Apr 29 2011 :
        - added -align_epi_strip_method for align_epi_anat.py skull strip
        - added help for -volreg_no_extent_mask
        - no EPI Automask is not a comment, not a warning
        - check that process blocks are unique (except for 'empty')
    2.51 May 31 2011 :
        - re-worked motion as prep for more motion options
        - replaced -volreg_regress_per_run with -regress_motion_per_run
        - made uniq_list_as_dsets() a warning, not an error (for J Britton)
    2.52 Jun 02 2011 :
        - by default, demean and deriv motion parameters are simply created
        - by default, demean motion parameters are applied in the regression
          (replacing the original 'basic' parameters, which should have no
          change in betas of interest, just the constant polort betas)
        - added -regress_apply_mot_types to specify motion types for regression
        - added -regress_no_motion_demean and -regress_no_motion_deriv
    2.53 Jun 02 2011 :
        - added -regress_make_cbucket
        - include view in 3dcopy for single run extents mask
          (so there are no missing view warnigns, done for J Jarcho)
        - make regress TSNR dataset by default (added option -compute_tsnr)
    2.54 Jun 03 2011: volreg tsnr is not default
        - added -volreg_compute_tsnr (def no), -regress_compute_tsnr (def yes)
    2.55 Jun 30 2011: rename aligned anat output (from align_epi_anat.py)
        - OLD_al_keep, if output anat is useful (want anat -> EPI  alignment)
        - OLD_al_junk, if output anat is not    (want EPI  -> anat alignment)
    2.56 Jul 06, 2011:
        - create anat_final dset, to be clear it is aligned with the stats
        - suggest uber_subject.py in -ask_me dialog
"""

g_version = "version 2.56, July 6, 2011"

# version of AFNI required for script execution
g_requires_afni = "4 Nov 2010"

# ----------------------------------------------------------------------
# dictionary of block types and modification functions

BlockLabels  = ['tcat', 'despike', 'ricor', 'tshift', 'align', 'volreg',
                'blur', 'mask', 'scale', 'regress', 'tlrc', 'empty']
BlockModFunc  = {'tcat'   : db_mod_tcat,     'despike': db_mod_despike,
                 'ricor'  : db_mod_ricor,    'tshift' : db_mod_tshift,
                 'align'  : db_mod_align,
                 'volreg' : db_mod_volreg,   'blur'   : db_mod_blur,
                 'mask'   : db_mod_mask,     'scale'  : db_mod_scale,
                 'regress': db_mod_regress,  'tlrc'   : db_mod_tlrc,
                 'empty'  : db_mod_empty}
BlockCmdFunc  = {'tcat'   : db_cmd_tcat,     'despike': db_cmd_despike,
                 'ricor'  : db_cmd_ricor,    'tshift' : db_cmd_tshift,
                 'align'  : db_cmd_align,
                 'volreg' : db_cmd_volreg,   'blur'   : db_cmd_blur,
                 'mask'   : db_cmd_mask,     'scale'  : db_cmd_scale,
                 'regress': db_cmd_regress,  'tlrc'   : db_cmd_tlrc,
                 'empty'  : db_cmd_empty}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# default block labels, and other labels (along with the label they follow)
DefLabels   = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
OtherDefLabels = {'despike':'tcat', 'align':'tcat', 'ricor':'despike'}
OtherLabels    = ['empty']

# --------------------------------------------------------------------------
# data processing stream class
class SubjProcSream:
    def __init__(self, label):
        self.label      = label         # name for stream
                                        # copy argv, but nuke any newlines
        self.argv = [UTIL.replace_n_squeeze(arg, '\n', ' ') for arg in sys.argv]
        self.valid_opts = None          # list of possible user options
        self.user_opts  = None          # list of given user options
        self.sep_char   = '.'           # filename separator character

        self.blocks     = []            # list of ProcessBlock elements
        self.dsets      = []            # list of afni_name elements
        self.stims_orig = []            # orig list of stim files to apply
        self.stims      = []            # list of stim files to apply
        self.extra_stims_orig = []      # orig list of extra_stims
        self.extra_stims      = []      # extra -stim_file list
        self.extra_labs       = []      # labels for extra -stim_file list

        self.vr_ext_base= None          # name of external volreg base 
        self.vr_ext_pre = 'external_volreg_base' # copied volreg base prefix

        self.mot_labs   = []            # labels for motion params
        # motion parameter file (across all runs)
        self.mot_file   = 'dfile.rall.1D' # either mot_default or mot_extern
        # for regression, maybe just mot_file, maybe per run, external
        # or might include demean and/or derivatives
        self.mot_regs   = []            # motion files to use in regression
        self.mot_per_run= 0             # motion regression per run
        self.mot_default= ''            # probably 'dfile.rall.1D', if set
        self.mot_extern = ''            # from -regress_motion_file
        self.mot_demean = ''            # from demeaned motion file
        self.mot_deriv  = ''            # motion derivatives

        self.opt_src    = 'cmd'         # option source
        self.subj_id    = 'SUBJ'        # hopefully user will replace this
        self.subj_label = '$subj'       # replace this for execution
        self.out_dir    = ''            # output directory for use by script
        self.od_var     = ''            # output dir variable: $output_dir
        self.script     = None          # script name, default proc.SUBJ
        self.overwrite  = 0             # overwrite script file?
        self.fp         = None          # file object
        self.anat       = None          # anatomoy to copy (afni_name class)
        self.tlrcanat   = None          # expected name of tlrc dataset
        self.tlrc_base  = None          # afni_name dataset used in -tlrc_base
        self.tlrc_ss    = 1             # whether to assume skull strip in tlrc
        self.warp_epi   = 0             # xform bitmap: tlrc, adwarp, a2e, e2a
        self.a2e_mat    = None          # anat2epi transform matrix file
        self.align_ebase= None          # external EPI for align_epi_anat.py
        self.align_epre = 'ext_align_epi' # copied align epi base prefix
        self.rm_rm      = 1             # remove rm.* files (user option)
        self.have_rm    = 0             # have rm.* files (such files exist)
        self.gen_review = '@epi_review.$subj' # filename for gen_epi_review.py
        self.test_stims = 1             # test stim_files for appropriateness
        self.test_dsets = 1             # test datasets for existence

        self.ricor_reg    = None        # ricor reg to apply in regress block
        self.ricor_nreg   = 0           # number of regs in ricor_reg
        self.ricor_regs   = []          # RETROICOR regressor files
        self.ricor_nfirst = 0           # number of TRs to remove
        self.ricor_nlast  = 0           # number of final TRs to remove

        self.check_setup_errors = 0     # count init setup errors
        self.exit_on_error      = 1     # exit on any encountered error
        self.verb               = 1     # verbosity level

        self.tr         = 0.0           # TR, in seconds
        self.reps       = 0             # TRs per run
        self.runs       = 0             # number of runs
        self.reps_all   = []            # number of TRs in each run
        self.reps_vary  = 0             # do the repetitions vary
        self.datatype   = -1            # 1=short, 3=float, ..., -1=uninit
        self.scaled     = -1            # if shorts, are they scaled?
        self.mask       = None          # mask dataset: one of the following
        self.mask_epi   = None          # mask dataset (from EPI)
        self.mask_anat  = None          # mask dataset (from subject anat)
        self.mask_group = None          # mask dataset (from tlrc base)
        self.mask_extents = None        # mask dataset (of EPI extents)
        self.censor_file  = ''          # for use as '-censor FILE' in 3dD
        self.censor_count = 0           # count times censoring
        self.exec_cmd   = ''            # script execution command string
        self.bash_cmd   = ''            # bash formatted exec_cmd
        self.tcsh_cmd   = ''            # tcsh formatted exec_cmd
        self.regmask    = 0             # apply any full_mask in regression
        self.origview   = '+orig'       # view could also be '+tlrc'
        self.view       = '+orig'       # (starting and 'current' views)
        self.xmat       = 'X.xmat.1D'   # X-matrix file (might go uncensored)

        self.bindex     = 0             # current block index
        self.pblabel    = ''            # previous block label

        return
        
    def show(self, mesg):
        print '%sSubjProcSream: %s' % (mesg, self.label)
        if self.verb > 3:
            for block in self.blocks:
                block.show('    Block %d: ' % self.blocks.index(block))
        print '    Dsets : ',
        if len(self.dsets) < 1: print 'not yet set'
        else:
            if self.verb > 2:
                print
                for dset in self.dsets: dset.show()
            else:
                for dset in self.dsets: print dset.rel_input(),
                print
        if self.verb > 3: self.valid_opts.show('valid_opts: ')
        if self.verb > 1: self.user_opts.show('user_opts: ')

    def init_opts(self):
        self.valid_opts = OptionList('afni_proc.py options')

        # input style options  rcr - update
        # self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

        # terminal options
        self.valid_opts.add_opt('-help', 0, [],
                        helpstr="show this help")
        self.valid_opts.add_opt('-hist', 0, [],
                        helpstr="show revision history")
        self.valid_opts.add_opt('-requires_afni_version', 0, [],
                        helpstr='show which date is required of AFNI')
        self.valid_opts.add_opt('-show_valid_opts', 0, [],
                        helpstr="show all valid options")
        self.valid_opts.add_opt('-ver', 0, [],
                        helpstr="show module version")

        # general execution options
        self.valid_opts.add_opt('-blocks', -1, [],
                        helpstr='specify ordered list of blocks to apply')
        self.valid_opts.add_opt('-do_block', -1, [],
                        helpstr='add extra blocks to the default list')
        self.valid_opts.add_opt('-dsets', -1, [],
                        helpstr='EPI datasets to process, ordered by run')

        self.valid_opts.add_opt('-out_dir', 1, [],
                        helpstr='result directory, where script is run')
        self.valid_opts.add_opt('-scr_overwrite', 0, [],
                        helpstr='overwrite existing processing script')
        self.valid_opts.add_opt('-script', 1, [],
                        helpstr='specify processing script to generate')
        self.valid_opts.add_opt('-sep_char', 1, [],
                        helpstr="output filename separator char, def='.'")
        self.valid_opts.add_opt('-subj_curly', 0, [],
                        helpstr="always use {} around $subj")
        self.valid_opts.add_opt('-subj_id', -1, [],
                        helpstr='subject ID, used in most filenames')

        self.valid_opts.add_opt('-ask_me', 0, [],       # QnA session
                        helpstr='have afni_proc.py as the user for options')
        self.valid_opts.add_opt('-bash', 0, [],
                        helpstr='show execution help in bash syntax')
        self.valid_opts.add_opt('-check_afni_version', 1, [],
                        acplist=['yes','no'],
                        helpstr='check that AFNI is current enough')
        self.valid_opts.add_opt('-check_setup_errors', 1, [],
                        acplist=['yes','no'],
                        helpstr='terminate on setup errors')
        self.valid_opts.add_opt('-copy_anat', 1, [],
                        helpstr='anatomy to copy to results directory')
        self.valid_opts.add_opt('-copy_files', -1, [],
                        helpstr='list of files to copy to results directory')
        self.valid_opts.add_opt('-execute', 0, [],
                        helpstr='execute script as suggested to user')
        self.valid_opts.add_opt('-exit_on_error', 1, [],
                        acplist=['yes','no'],
                        helpstr='exit script on any command error')
        self.valid_opts.add_opt('-gen_epi_review', 1, [],
                        helpstr='generate a script to review orig EPI data')
        self.valid_opts.add_opt('-no_epi_review', 0, [],
                        helpstr='do not generate an EPI review script')
        self.valid_opts.add_opt('-keep_rm_files', 0, [],
                        helpstr='do not delete temporary rm.* files')
        self.valid_opts.add_opt('-keep_script_on_err', 1, [],
                        acplist=['yes','no'],
                        helpstr='do not delete script on failure')
        self.valid_opts.add_opt('-move_preproc_files', 0, [],
                        helpstr='move preprocessing files to preproc.data dir')
        self.valid_opts.add_opt('-outlier_count', 1, [],
                        acplist=['yes','no'],
                        helpstr='run 3dToutcount?  (default=yes)')
        self.valid_opts.add_opt('-outlier_legendre', 1, [],
                        acplist=['yes','no'],
                        helpstr='use -legendre in 3dToutcount?  (def=yes)')
        self.valid_opts.add_opt('-outlier_polort', 1, [],
                        helpstr='3dToutcount polort (default is as with 3dD)')
        self.valid_opts.add_opt('-remove_preproc_files', 0, [],
                        helpstr='remove pb0* preprocessing files')
        self.valid_opts.add_opt('-test_for_dsets', 1, [],
                        acplist=['yes','no'],
                        helpstr="test input datasets for existence (def=yes)")
        self.valid_opts.add_opt('-test_stim_files', 1, [],
                        acplist=['yes','no'],
                        helpstr="test stim_files for validity (default=yes)")
        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr="set the verbose level")

        # block options
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [],
                        helpstr='num TRs to remove from start of each run')
        self.valid_opts.add_opt('-tcat_remove_last_trs', 1, [],
                        helpstr='num TRs to remove from end of each run')

        self.valid_opts.add_opt('-despike_mask', 0, [],
                        helpstr="allow 3dDespike to automask (-dilate 4)")
        self.valid_opts.add_opt('-despike_opts_3dDes', -1, [],
                        helpstr='additional options directly for 3dDespike')

        self.valid_opts.add_opt('-ricor_datum', 1, [],
                        acplist=['short', 'float'],
                        helpstr='output datatype from ricor processing block')
        self.valid_opts.add_opt('-ricor_polort', 1, [],
                        helpstr='polort to apply for RETROICOR regression')
        self.valid_opts.add_opt('-ricor_regress_method', 1, [],
                        acplist=['per-run', 'across-runs'],
                        helpstr="use stimuli 'per-run' or 'across-runs'")
        self.valid_opts.add_opt('-ricor_regress_solver', 1, [],
                        helpstr="regression via 'OLSQ' or 'REML'")
        self.valid_opts.add_opt('-ricor_regs', -1, [],
                        helpstr='slice-based regressors for RETROICOR')
        self.valid_opts.add_opt('-ricor_regs_nfirst', 1, [],
                        helpstr='num first TRs to remove from ricor_regs')
        self.valid_opts.add_opt('-ricor_regs_rm_nlast', 1, [],
                        helpstr='num last TRs to remove from ricor_regs')

        self.valid_opts.add_opt('-tshift_align_to', -1, [],
                        helpstr='time alignment option given to 3dTshift')
        self.valid_opts.add_opt('-tshift_interp', 1, [],
                        helpstr='interpolation method used in 3dTshift')
        self.valid_opts.add_opt('-tshift_opts_ts', -1, [],
                        helpstr='additional options directly for 3dTshift')

        self.valid_opts.add_opt('-align_epi_ext_dset', 1, [],
                        helpstr='external EPI volume for align_epi_anat.py')
        self.valid_opts.add_opt('-align_opts_aea', -1, [],
                        helpstr='additional options for align_epi_anat.py')
        self.valid_opts.add_opt('-align_epi_strip_method', 1, [],
                        acplist=['3dSkullStrip','3dAutomask','None'],
                        helpstr="specify method for 'skull stripping' the EPI")

        self.valid_opts.add_opt('-tlrc_anat', 0, [],
                        helpstr='run @auto_tlrc on anat from -copy_anat')
        self.valid_opts.add_opt('-tlrc_base', 1, [],
                        helpstr='alternate @auto_tlrc base (not TT_N27, say)')
        self.valid_opts.add_opt('-tlrc_opts_at', -1, [],
                        helpstr='additional options supplied to @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_no_ss', 0, [],
                        helpstr='do not skull-strip during @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_rmode', 1, [],
                        helpstr='resample mode applied in @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_suffix', 1, [],
                        helpstr='suffix applied in @auto_tlrc (default: NONE)')

        self.valid_opts.add_opt('-volreg_align_e2a', 0, [],
                        helpstr="align EPI to anatomy (via align block)")
        self.valid_opts.add_opt('-volreg_align_to', 1, [],
                        acplist=['first','third', 'last'],
                        helpstr="align to 'first', 'third' or 'last' TR")
        self.valid_opts.add_opt('-volreg_base_dset', 1, [],
                        helpstr='external dataset to use as volreg base')
        self.valid_opts.add_opt('-volreg_base_ind', 2, [],
                        helpstr='run/sub-brick indices for volreg')
        self.valid_opts.add_opt('-volreg_compute_tsnr', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute TSNR datasets (yes/no) of volreg run1')
        self.valid_opts.add_opt('-volreg_interp', 1, [],
                        helpstr='interpolation method used in volreg')
        self.valid_opts.add_opt('-volreg_no_extent_mask', 0, [],
                        helpstr='do not restrict warped EPI to extents')
        self.valid_opts.add_opt('-volreg_opts_vr', -1, [],
                        helpstr='additional options directly for 3dvolreg')
        self.valid_opts.add_opt('-volreg_regress_per_run', 0, [],
                        helpstr='apply separate motion regressors per run')
        self.valid_opts.add_opt('-volreg_tlrc_adwarp', 0, [],
                        helpstr='apply adwarp tlrc transformation after volreg')
        self.valid_opts.add_opt('-volreg_tlrc_warp', 0, [],
                        helpstr='warp volreg data to standard space')
        self.valid_opts.add_opt('-volreg_warp_dxyz', 1, [],
                        helpstr='output grid size for _tlrc_warp, _align_e2a')
        self.valid_opts.add_opt('-volreg_zpad', 1, [],
                        helpstr='number of slices to pad by in volreg')

        self.valid_opts.add_opt('-blur_filter', 1, [],
                        helpstr='blurring filter option (def: -1blur_fwhm)')
        self.valid_opts.add_opt('-blur_in_mask', 1, [],
                        acplist=['yes','no'],
                        helpstr='restrict blur to mask: yes/no')
        self.valid_opts.add_opt('-blur_in_automask', 0, [],
                        helpstr='applies -automask to 3dBlurInMask')
        self.valid_opts.add_opt('-blur_size', 1, [],
                        helpstr='size of blur kernel (def: 4)')
        self.valid_opts.add_opt('-blur_opts_BIM', -1, [],
                        helpstr='additional options directly for 3dBlurInMask')
        self.valid_opts.add_opt('-blur_opts_merge', -1, [],
                        helpstr='additional options directly for 3dmerge')

        self.valid_opts.add_opt('-mask_apply', 1, [],
                        acplist=['epi', 'anat', 'group', 'extents'],
                        helpstr="select mask to apply in regression")
        self.valid_opts.add_opt('-mask_dilate', 1, [],
                        helpstr="dilation to be applied in automask")
        self.valid_opts.add_opt('-mask_test_overlap', 1, [],
                        acplist=['yes','no'],
                        helpstr='test anat/EPI mask overlap (yes/no)')
        self.valid_opts.add_opt('-mask_type', 1, [],
                        acplist=['union','intersection'],
                        helpstr="specify a 'union' or 'intersection' mask type")

        self.valid_opts.add_opt('-scale_max_val', 1, [],
                        helpstr="maximum value for scaled data (def: 200)")
        self.valid_opts.add_opt('-scale_no_max', 0, [],
                        helpstr="do not limit scaled data")

        self.valid_opts.add_opt('-regress_3dD_stop', 0, [],
                        helpstr="stop 3dDeconvolve after matrix generation")
        self.valid_opts.add_opt('-regress_apply_mask', 0, [],
                        helpstr="apply the mask in regression")
        self.valid_opts.add_opt('-regress_basis', 1, [],
                        helpstr="basis function to use in regression")
        self.valid_opts.add_opt('-regress_basis_multi', -1, [],
                        helpstr="one basis function per stimulus class")
        self.valid_opts.add_opt('-regress_basis_normall', 1, [],
                        helpstr="specify magnitude of basis functions")
        self.valid_opts.add_opt('-regress_compute_tsnr', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute TSNR datasets (yes/no) after regress')
        self.valid_opts.add_opt('-regress_make_cbucket', 1, [],
                        acplist=['yes','no'],
                        helpstr="request cbucket dataset of all betas (yes/no)")

        self.valid_opts.add_opt('-regress_censor_motion', 1, [],
                        helpstr="censor TR if motion derivative exceeds limit")
        self.valid_opts.add_opt('-regress_censor_prev', 1, [],
                        acplist=['yes','no'],
                        helpstr="set whether to censor previous motion TR")
        self.valid_opts.add_opt('-regress_censor_first_trs', 1, [],
                        helpstr="censor first TRs per run (if censor motion)")
        self.valid_opts.add_opt('-regress_censor_outliers', 1, [],
                        helpstr="censor TR if outlier fraction exceeds limit")
        self.valid_opts.add_opt('-regress_skip_first_outliers', 1, [],
                        helpstr="ignore outliers in first few TRs of each run")

        self.valid_opts.add_opt('-regress_cormat_warnigns', 1, [],
                        acplist=['yes','no'],
                        helpstr="show cormat warnings from X-matrix (def: yes)")
        self.valid_opts.add_opt('-regress_fout', 1, [],
                        acplist=['yes','no'],
                        helpstr="output individual F-stats? (def: yes)")
        self.valid_opts.add_opt('-regress_polort', 1, [],
                        helpstr="baseline polynomial degree per run")
        self.valid_opts.add_opt('-regress_stim_files', -1, [],
                        helpstr="0/1 or pre-convolved stimulus files")
        self.valid_opts.add_opt('-regress_stim_labels', -1, [],
                        helpstr="labels for specified regressors")
        self.valid_opts.add_opt('-regress_stim_times', -1, [],
                        helpstr="stimulus timing files")
        self.valid_opts.add_opt('-regress_no_stim_times', 0, [],
                        helpstr="do not convert stim_files to timing")
        self.valid_opts.add_opt('-regress_stim_times_offset', 1, [],
                        helpstr="add offset when converting to timing")
        self.valid_opts.add_opt('-regress_use_stim_files', 0, [],
                        helpstr="do not convert stim_files to timing")

        self.valid_opts.add_opt('-regress_apply_mot_types', -1, [],
                        acplist=['basic','demean','deriv'],
                        helpstr="specify which motion parameters to apply")
        self.valid_opts.add_opt('-regress_motion_file', 1, [],
                        helpstr="external file to apply as motion regressors")
        self.valid_opts.add_opt('-regress_motion_per_run', 0, [],
                        helpstr="apply all motion parameters per run")
        self.valid_opts.add_opt('-regress_no_motion_demean', 0, [],
                        helpstr="do not compute demeaned motion params")
        self.valid_opts.add_opt('-regress_no_motion_deriv', 0, [],
                        helpstr="do not compute motion param derivatives")

        self.valid_opts.add_opt('-regress_extra_stim_files', -1, [],
                        helpstr="extra -stim_files to apply")
        self.valid_opts.add_opt('-regress_extra_stim_labels', -1, [],
                        helpstr="labels for extra -stim_files")

        self.valid_opts.add_opt('-regress_compute_fitts', 0, [],
                        helpstr="compute fitts only after 3dDeconvolve")
        self.valid_opts.add_opt('-regress_est_blur_epits', 0, [],
                        helpstr="estimate blur from scaled EPI time series")
        self.valid_opts.add_opt('-regress_est_blur_errts', 0, [],
                        helpstr="estimate blur from scaled error time series")
        self.valid_opts.add_opt('-regress_errts_prefix', 1, [],
                        helpstr="prefix to use for errts dataset")
        self.valid_opts.add_opt('-regress_fitts_prefix', 1, [],
                        helpstr="prefix to use for fitts dataset")
        self.valid_opts.add_opt('-regress_iresp_prefix', 1, [],
                        helpstr="prefix to use for iresp datasets")
        self.valid_opts.add_opt('-regress_global_times', 0, [],
                        helpstr="apply -global_times option to 3dDeconvolve")
        self.valid_opts.add_opt('-regress_local_times', 0, [],
                        helpstr="apply -local_times option to 3dDeconvolve")
        self.valid_opts.add_opt('-regress_make_ideal_sum', 1, [],
                        helpstr="filename for sum of ideal regressors")
        self.valid_opts.add_opt('-regress_no_ideal_sum', 0, [],
                        helpstr="do not compute the sum of regressors")
        self.valid_opts.add_opt('-regress_no_fitts', 0, [],
                        helpstr="do not output a fit timeseries dataset")
        self.valid_opts.add_opt('-regress_no_ideals', 0, [],
                        helpstr="do not generate ideal regressors")
        self.valid_opts.add_opt('-regress_no_iresp', 0, [],
                        helpstr="do not output impulse reponse datasets")
        self.valid_opts.add_opt('-regress_no_mask', 0, [],
                        helpstr="do not apply any mask during regression")
        self.valid_opts.add_opt('-regress_no_motion', 0, [],
                        helpstr="do not apply motion parameters in regression")
        self.valid_opts.add_opt('-regress_opts_3dD', -1, [],
                        helpstr='additional options directly to 3dDeconvolve')
        self.valid_opts.add_opt('-regress_opts_reml', -1, [],
                        helpstr='additional options directly to 3dREMLfit')
        self.valid_opts.add_opt('-regress_reml_exec', 0, [],
                        helpstr="execute 3dREMLfit command script")
        self.valid_opts.add_opt('-regress_RONI', -1, [],
                        helpstr="1-based list of regressors of no interest")

        # 3dClustSim options
        self.valid_opts.add_opt('-regress_CS_NN', 1, [],
                        acplist=['1','2','3','12','13','23','123'],
                        helpstr='specify subset of {1,2,3} (default=123)')
        self.valid_opts.add_opt('-regress_opts_CS', -1, [],
                        helpstr='additional options directly to 3dClustSim')
        self.valid_opts.add_opt('-regress_run_clustsim', 1, [],
                        acplist=['yes','no'],
                        helpstr="add 3dClustSim attrs to regression bucket")

        self.valid_opts.trailers = 0   # do not allow unknown options
        
    def get_user_opts(self):
        self.valid_opts.check_special_opts(self.argv)
        self.user_opts = read_options(self.argv, self.valid_opts)
        if self.user_opts == None: return 1     # error condition
        if len(self.user_opts.olist) == 0:      # no options: apply -help
            print g_help_string
            return 0
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt: print "** seem to have trailers, but cannot find them!"
            else: print "** have invalid trailing args: %s", opt.show()
            return 1  # failure

        # maybe the users justs wants a complete option list
        if self.user_opts.find_opt('-show_valid_opts'):
            self.valid_opts.show('', 1)
            return 0  # gentle termination
        
        # apply the user options
        rv = self.apply_initial_opts(self.user_opts)
        if rv != None: return rv

        # update out_dir now (may combine option results)
        if self.out_dir == '': self.out_dir = '%s.results' % self.subj_label
        self.od_var = '$output_dir'

        if self.verb > 1: show_args_as_command(self.argv, "executing command:")
        if self.verb > 4: show_args_as_command(sys.argv, "system command:")
        if self.verb > 1: self.show('end get_user_opts ')

    # apply the general options - many terminate program
    def apply_initial_opts(self, opt_list):
        opt = opt_list.find_opt('-verb')   # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        if opt_list.find_opt('-help'):     # just print help
            print g_help_string
            return 0  # gentle termination
        
        if opt_list.find_opt('-hist'):     # print the history
            print g_history
            return 0  # gentle termination
        
        if opt_list.find_opt('-requires_afni_version'): # print required version
            print g_requires_afni
            return 0  # gentle termination
        
        if opt_list.find_opt('-ver'):      # show the version string
            print g_version
            return 0  # gentle termination
        
        opt = opt_list.find_opt('-check_setup_errors')
        if opt and opt.parlist[0] == 'yes': self.check_setup_errors = 1
        else:                               self.check_setup_errors = 0

        opt = opt_list.find_opt('-exit_on_error')
        if opt and opt.parlist[0] == 'no': self.exit_on_error = 0
        else:                              self.exit_on_error = 1

        opt = opt_list.find_opt('-copy_anat')
        if opt != None:
            self.anat = afni_name(opt.parlist[0])
            self.tlrcanat = self.anat.new(new_view='+tlrc')

        opt = opt_list.find_opt('-gen_epi_review')  # name epi review script
        if opt != None: self.gen_review = opt.parlist[0]

        opt = opt_list.find_opt('-no_epi_review') # no epi review script
        if opt != None: self.gen_review = None

        opt = opt_list.find_opt('-keep_rm_files')
        if opt != None: self.rm_rm = 0

        opt = opt_list.find_opt('-out_dir')
        if opt != None: self.out_dir = opt.parlist[0]

        opt = opt_list.find_opt('-sep_char') # -- in default output names
        if opt != None:
            self.sep_char = opt.parlist[0]
            if self.sep_char == '_': self.subj_label = '${subj}'

        # if sep_char changes, might need this
        if opt_list.find_opt('-subj_curly'): self.subj_label = '${subj}'

        opt = opt_list.find_opt('-subj_id') # -- needs to be before script
        if opt != None: self.subj_id = opt.parlist[0]

        opt = opt_list.find_opt('-script')
        if opt != None: self.script = opt.parlist[0]
        else:           self.script = 'proc.%s' % self.subj_id

        opt = opt_list.find_opt('-scr_overwrite')
        if opt != None: self.overwrite = 1

        # do we check input datasets for existence?  default to yes
        opt = opt_list.find_opt('-test_for_dsets')
        if not opt or opt_is_yes(opt): self.test_dsets = 1
        else:                          self.test_dsets = 0

        # do we test stim files for validity?  default to yes
        opt = opt_list.find_opt('-test_stim_files')
        if not opt or opt_is_yes(opt): self.test_stims = 1
        else:                          self.test_stims = 0


    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        # first, note datasets
        opt = self.user_opts.find_opt('-dsets')
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))

            # possibly verify that all of the input datasets exist
            if self.test_dsets:
                missing = 0
                for dset in self.dsets:
                    if not dset.exist():
                        print '** missing dataset: %s' % dset.rpv()
                        missing = 1
                if missing: return 1

            if self.dsets[0].view and self.dsets[0].view != self.view:
                self.view = self.dsets[0].view
                self.origview = self.view
                if self.verb > 0: print '-- applying view as %s' % self.view

        # init block either from DefLabels or -blocks
        opt = self.user_opts.find_opt('-blocks')
        if opt:  # then create blocklist from user opts (but prepend tcat)
            if opt.parlist[0] != 'tcat':
                blocks = ['tcat'] + opt.parlist
            else: blocks = opt.parlist
        else: blocks = DefLabels  # init to defaults

        # check for -do_block options
        opt = self.user_opts.find_opt('-do_block')
        if opt and opt.parlist and len(opt.parlist) > 0:
            #if self.user_opts.find_opt('-blocks'):
            #    print '** error: -do_block invalid when using -blocks'
            #    return 1

            # ****
            # if no -blocks option, but a long list of -do_block, maybe start
            # from normal blocks subset of -do_block list

            # check additional blocks one by one
            errs = 0
            for bname in opt.parlist:
                err, blocks = self.add_block_to_list(blocks, bname)
                errs += err
            if errs > 0 : return 1

        # allow for -tlrc_anat option
        opt = self.user_opts.find_opt('-tlrc_anat')
        if opt and not self.find_block('tlrc'):
            if self.user_opts.find_opt('-volreg_tlrc_warp') or  \
               self.user_opts.find_opt('-volreg_tlrc_adwarp') :
                err, blocks = self.add_block_before_label(blocks,
                                        'tlrc', 'volreg')
            else: err, blocks = self.add_block_to_list(blocks, 'tlrc')
            if err: return 1

        # if user has supplied options for blocks that are not used, fail
        if self.opts_include_unused_blocks(blocks, 1): return 1

        # check for unique blocks (except for 'empty')
        if not self.blocks_are_unique(blocks):
            print '** blocks must be unique\n'  \
                  '   (is there overlap between -blocks and -do_block?)\n'
            return 1

        # call db_mod_functions
        for label in blocks:
            rv = self.add_block(label)
            if rv != None: return rv

        # maybe the user wants to be quizzed for options
        uopt = self.user_opts.find_opt('-ask_me')
        if uopt != None:
            if ask_me.ask_me_subj_proc(self):
                return 1
            for label in blocks:
                block = self.find_block(label)
                if not block:
                    print "** error: missing block '%s' in ask_me update"%label
                    return 1
                BlockModFunc[label](block, self, self.user_opts) # modify block

        # rcr - if there is another place to update options from, do it
        uopt = self.user_opts.find_opt('-opt_source')
        if uopt != None:
            print '** not ready to update options from %s' % str(uopt.parlist)
            return 1

        # do some final error checking
        if len(self.dsets) == 0:
            print 'error: dsets have not been specified (consider -dsets)'
            return 1

        # no errors, just warn the user (for J Britton)   25 May 2011
        uniq_list_as_dsets(self.dsets, 1)
        self.check_block_order()

    def add_block_to_list(self, blocks, bname, adj=None, dir=0):
        """given current block list, add a block for bname after that
           of prevlab or from OtherDefLabels if None

                blocks : current list of block labels
                bname  : label of block to insert
                adj    : name of adjacent block (if None, try to decide)
                dir    : if adj, dir is direction of bname to adj
                         (-1 : bname is before, 1: bname is after)
           
           return error code and new list"""

        # if we are not given an adjacent block, try to find one
        if not adj:
            dir, adj = self.find_best_block_posn(blocks, bname)
            if not dir: return 1, blocks

        # good cases
        if dir < 0: return self.add_block_before_label(blocks, bname, adj)
        if dir > 0: return self.add_block_after_label(blocks, bname, adj)

        # failure
        print "** ABTL: have adj=%s but no dir" % adj
        return 1, blocks


    def find_best_block_posn(self, blocks, bname):
        """decide where it is best to insert the block bname
           return dir, nextto
                dir    = -1,0,1 means before, error, after
                nextto = name of relevant adjacent block"""

        if bname == 'ricor':
            try: ind = blocks.index('despike')
            except: ind = -1
            if ind >= 0: return 1, 'despike'    # after despike

            return 1, 'tcat'                    # else, just after tcat

        if bname == 'align':
            try: ind = blocks.index('tlrc')
            except: ind = -1
            if ind >= 0: return -1, 'tlrc'      # before tlrc
            try: ind = blocks.index('volreg')
            except: ind = -1
            if ind >= 0: return -1, 'volreg'    # before volreg
            try: ind = blocks.index('tshift')
            except: ind = -1
            if ind >= 0: return 1, 'tshift'     # after tshift

            return  1, 'tcat'                   # else, stick at beginning

        if bname == 'tlrc':
            try: ind = blocks.index('volreg')
            except: ind = -1
            if ind >= 0: return -1, 'volreg'    # before volreg
            try: ind = blocks.index('align')
            except: ind = -1
            if ind >= 0: return 1, 'align'      # after align
            return 1, blocks[-1]                # stick it at the end

        # if those didn't apply, go with the OtherDefLabels array

        try: prevlab = OtherDefLabels[bname]
        except:
            print "** failed to find position for block '%s' in %s" \
                  % (bname, blocks)
            prevlab = ''
        
        if prevlab == '': return 0, prevlab     # failure
        else:             return 1, prevlab     # success

    def add_block_after_label(self, blocks, bname, prelab):
        """add a block for bname after that of prelab
           
           return error code and new list"""
        err = 0
        if not bname in BlockLabels:
            print "** ABAL error: block '%s' is invalid" % bname
            return 1, blocks
        if not prelab in BlockLabels:
            print "** ABAL error: prelab '%s' is invalid" % prelab
            return 1, blocks
        try: preindex = blocks.index(prelab)
        except:     
            print "** cannot find block '%s' to insert block '%s' after" \
                  % (prelab, bname)
            preindex = 0
            err = 1
        if preindex < 0:
            print "** error: blocks.index failure for '%s'" % prelab
            err = 1
        
        if err: return 1, blocks

        # else add the block to blocklist
        preindex += 1
        blocks[preindex:preindex] = [bname]

        return 0, blocks

    def add_block_before_label(self, blocks, bname, postlab):
        """add a block for bname before that of postlab
           
           return error code and new list"""
        err = 0
        if not bname in BlockLabels:
            print "** ABBL error: block '%s' is invalid" % bname
            return 1, blocks
        if not postlab in BlockLabels:
            print "** ABBL error: postlab '%s' is invalid" % postlab
            return 1, blocks
        try: postindex = blocks.index(postlab)
        except:     
            print "** cannot find block '%s' to insert block '%s' before" \
                  % (postlab, bname)
            postindex = 0
            err = 1
        if postindex < 0:
            print "** error: blocks.index failure for '%s'" % postlab
            err = 1
        
        if err: return 1, blocks

        # else add the block to blocklist
        blocks[postindex:postindex] = [bname]

        return 0, blocks

    # create script from blocks and options
    def create_script(self):
        rv = self.get_run_info()
        if rv != None: return rv

        rv = self.init_script()         # create the script file and header
        if rv != None: return rv

        # small cleanup: after this point, anat needs a view
        if self.anat and self.anat.view == '':
            if self.verb > 3: print '++ applying %s view to anat'%self.view
            self.anat.view = self.view

        errs = 0
        for block in self.blocks:
            if not block.apply: continue        # skip such blocks
            cmd_str = BlockCmdFunc[block.label](self, block)
            if cmd_str == None:
                print "** script creation failure for block '%s'" % block.label
                errs += 1
            else:
                self.fp.write(add_line_wrappers(cmd_str))
                if self.verb>3: block.show('+d post command creation: ')
                if self.verb>4: print '+d %s cmd: \n%s'%(block.label, cmd_str)

        if self.gen_review:
            cmd_str = db_cmd_gen_review(self)
            if cmd_str:
                self.fp.write(add_line_wrappers(cmd_str))
                if self.verb > 1:
                    print "+d generated EPI review script %s" % self.gen_review
            else:
                errs += 1
                if self.verb > 1:
                    print '** failed to generate EPI review script'

        rv = self.finalize_script()     # finish the script
        if rv: errs += 1

        if self.fp: self.fp.close()
        os.chmod(self.script, 0755)     # make executable

        if errs > 0:
            # default to removing any created script
            opt = self.user_opts.find_opt('-keep_script_on_err')
            if not opt or opt_is_no(opt):
                os.remove(self.script)
            return 1    # so we print all errors before leaving

        if self.verb > 0:
            # last warning, if user is masking EPI data...
            if self.mask != None:
                if self.regmask:
                  if self.mask != self.mask_group:
                    print "** masking single subject EPI is not recommended"
                    print "   (see 'MASKING NOTE' from the -help for details)"
                else:
                    print "-- using default: will not apply EPI Automask"
                    print "   (see 'MASKING NOTE' from the -help for details)"

            if self.runs == 1:
                print "\n-------------------------------------\n" \
                        "** warning have only 1 run to analyze\n" \
                        "-------------------------------------"
            print "\n--> script is file: %s" % self.script
            print   '    consider the script execution command: \n\n' \
                    '      %s\n' % self.exec_cmd

        return

    def get_run_info(self):
        self.runs = len(self.dsets)
        if self.runs < 1:
            print "** have no dsets to analyze"
            return 1

        # updated by 'tcat' opteration (and -remove_trs option)
        # (use rpve to include NIfTI, etc.)
        dset = self.dsets[0].rpve()

        err, self.reps, self.tr = get_dset_reps_tr(dset, self.verb)
        if err: return 1   # check for failure

        # set reps in each run
        self.reps_all = []
        self.reps_vary = 0
        for dr in self.dsets:
            err, reps, tr = get_dset_reps_tr(dr.rpve(), self.verb)
            if err: return 1
            self.reps_all.append(reps)
            if reps != self.reps: self.reps_vary = 1
            if tr != self.tr:
                print '** TR of %g != run #1 TR %g' % (tr, self.tr)
                return 1

        # note data type and whether data is scaled
        err, vlist = get_typed_dset_attr_list(dset, "BRICK_TYPES", int)
        if not err and len(vlist) >= 1:
            self.datatype = vlist[0]

        err, vlist = get_typed_dset_attr_list(dset, "BRICK_FLOAT_FACS", int)
        if not err and len(vlist) >= 1:
            if vals_are_constant(vlist, 0) or vals_are_constant(vlist, 1):
                self.scaled = 0
            else:
                self.scaled = 1

        if self.verb > 1:
            print '-- reps = %g, tr = %g, datatype = %g, scaled = %d' \
                  % (self.reps, self.tr, self.datatype, self.scaled)

    def get_vr_base_indices(self, verb=1):
        """return 0-based run and TR indices for volreg base
           (return runs==-1 if they cannot be set)
           if verb > 0 and values cannot be set, print why not"""
        
        block = self.find_block('volreg')
        if not block:
            if verb > 0: print "** cannot get vr_base_indices: no volreg block"
            return -1, -1
        opt = block.opts.find_opt('-volreg_base_ind')

        if not opt:
            if self.verb > 2: print '-- no -volreg_base_ind opt for vr_base'
            return proc.runs-1, proc.reps-1  # defaults

        # if parlist values are -1, set to last TR
        if opt.parlist[0] < 0 or opt.parlist[1] < 0:
            return self.runs-1, self.reps-1

        return opt.parlist[0], opt.parlist[1]

    # create a new block for the given label, and append it to 'blocks'
    def add_block(self, label):
        block = ProcessBlock(label, self)
        if not block.valid:
            print '** invalid block : %s' % block.label
            return 1
        if self.verb > 3: block.show('+d post init block: ')
        self.blocks.append(block)

    def find_block(self, label):
        for block in self.blocks:
            if block.label == label: return block
        return None

    def find_block_opt(self, label, opt_name):
        """return any found comompt instance in block.opts"""
        for block in self.blocks:
            if block.label == label: return block.opts.find_opt(opt_name)
        return None

    def find_block_index(self, label):
        block = self.find_block(label)
        if block: return self.blocks.index(block)
        return None

    def blocks_are_unique(self, blocks, exclude_list=['empty'], whine=1):
        """return whether the blocs are unique
                - ignore blocks in exclude_list
                - if whine, complain on repeats
           Process one by one, so we know what to complain about.
        """

        unique = 1
        checked = []
        for block in blocks:
            if block in exclude_list: continue
            if block in checked:
                if whine: print "** warning: block '%s' used multiple times" \
                                % block
                unique = 0
            else: checked.append(block)

        return unique

    def opts_include_unused_blocks(self, blocks, whine=1):
        """return whether options refer to blocks that are not being used"""

        # start with all BlockLabels and remove those in passed list
        badlist = BlockLabels[:]
        for label in blocks:
            if label in badlist: badlist.remove(label)

        if self.verb > 3: print '++ unused blocks: %s' % badlist

        # for speed, make an explicit list of option prefixes to search for
        badlist = ['-%s_' % label for label in badlist]

        errs = 0
        for opt in self.user_opts.olist:
            ind = opt.name.find('_')
            if ind < 0: continue
            if opt.name[0:ind+1] in badlist:
                if whine: print "** missing '%s' block for option '%s'" \
                                % (opt.name[1:ind], opt.name)
                errs += 1

        return errs

    def check_block_order(self):
        """whine about any disliked block orderings
                - despike < ricor < tshift/volreg
                - blur < scale
                - tshift/volreg/blur/scale < regress
           return the number of errors
        """
        errs = 0
        if self.find_block('align'):
            if not self.blocks_ordered('align', 'volreg'):
                errs += 1
                print "** warning: 'align' should preceed 'volreg'"
            if not self.blocks_ordered('align', 'tlrc'):
                errs += 1
                print "** warning: 'align' should preceed 'tlrc'"
        if self.find_block('ricor'):
            if not self.blocks_ordered('despike', 'ricor', must_exist=1):
                errs += 1
                print "** warning: 'despike' should preceed 'ricor'"
            if not self.blocks_ordered('ricor', 'tshift'):
                errs += 1
                print "** warning: 'tshift' should preceed 'ricor'"
            if not self.blocks_ordered('ricor', 'volreg'):
                errs += 1
                print "** warning: 'volreg' should preceed 'ricor'"
        if self.find_block('blur'):
            if not self.blocks_ordered('blur', 'scale'):
                errs += 1
                print "** warning: 'blur' should preceed 'scale'"
        if self.find_block('regress'):
            if not self.blocks_ordered('tshift', 'regress'):
                errs += 1
                print "** warning: 'tshift' should preceed 'regress'"
            if not self.blocks_ordered('volreg', 'regress'):
                errs += 1
                print "** warning: 'volreg' should preceed 'regress'"
            if not self.blocks_ordered('blur', 'regress'):
                errs += 1
                print "** warning: 'blur' should preceed 'regress'"
            if not self.blocks_ordered('scale', 'regress'):
                errs += 1
                print "** warning: 'scale' should preceed 'regress'"

        return errs

    def blocks_ordered(self, name0, name1, must_exist=0):
        """check that the name0 block comes before the name1 block

              - something missing means order is okay
              - must_exist implies both must exist, else print message
                (often one has been check already)

           return 0 only if both exist and are out of order"""

        bind0 = self.find_block_index(name0)
        bind1 = self.find_block_index(name1)
        if bind0 < 0 or bind1 < 0: # something is missing
            if must_exist:
                print '** warning: missing block',
                if bind0 < 0: print "'%s' (to preceed '%s')" % (name0,name1)
                if bind1 < 0: print "'%s' (to follow '%s')" % (name1,name0)
            return 1
        elif bind0 < bind1: return 1
        else:               return 0

    # set subj shell variable, check output dir, create and cd
    def init_script(self):
        if not self.overwrite and os.path.isfile(self.script):
            print "error: script file '%s' already exists, exiting..." % \
                  self.script
            return 0
        try: self.fp = open(self.script,'w')
        except:
            print "cannot open script file '%s' for writing" % self.script
            return 1

        if self.exit_on_error: topt = ' -xef'
        else:                  topt = ' -x'
        self.fp.write('#!/bin/tcsh%s\n\n' % topt)
        self.fp.write('echo "auto-generated by afni_proc.py, %s"\n' % asctime())
        self.fp.write('echo "(%s)"\n\n'% g_version)

        # include execution method in script
        if self.exit_on_error: opts = '-xef'
        else:                  opts = '-x'

        # store both tcsh and bash versions
        self.bash_cmd = 'tcsh %s %s 2>&1 | tee output.%s' % \
                        (opts, self.script, self.script)
        self.tcsh_cmd = 'tcsh %s %s |& tee output.%s'     % \
                        (opts, self.script, self.script)

        if self.user_opts.find_opt('-bash'): self.exec_cmd = self.bash_cmd
        else:                                self.exec_cmd = self.tcsh_cmd

        self.fp.write('# execute via : \n'      \
                      '#   %s\n\n' % self.exec_cmd)

        # maybe the user want to check the status of the init operations
        if not self.check_setup_errors: stat_inc = ''
        else: stat_inc = '@ nerrors += $status      # accumulate error count\n'

        self.fp.write('# %s\n'
                      '# script setup\n\n' % block_header('auto block: setup'))

        if len(stat_inc) > 0:
            self.fp.write("# prepare to count setup errors\n"
                          "set nerrors = 0\n\n")

        # possibly check the AFNI version (via afni_history)
        opt = self.user_opts.find_opt('-check_afni_version')
        if not opt or opt_is_yes(opt):
          self.fp.write(                                                      \
          '# check that the current AFNI version is recent enough\n'          \
          'afni_history -check_date %s\n'                                     \
          'if ( $status ) then\n'                                             \
          '    echo "** this script requires newer AFNI binaries (than %s)"\n'\
          '    echo "   (consider: @update.afni.binaries -defaults)"\n'       \
          '    exit\n'                                                        \
          'endif\n\n' % (g_requires_afni, g_requires_afni) )

        self.fp.write('# the user may specify a single subject to run with\n'\
                      'if ( $#argv > 0 ) then\n'                             \
                      '    set subj = $argv[1]\n'                            \
                      'else\n'                                               \
                      '    set subj = %s\n'                                  \
                      'endif\n\n' % self.subj_id )
        self.fp.write('# assign output directory name\n'
                      'set output_dir = %s\n\n' % self.out_dir)
        self.fp.write( \
                '# verify that the results directory does not yet exist\n'\
                'if ( -d %s ) then\n'                                     \
                '    echo output dir "$subj.results" already exists\n'    \
                '    exit\n'                                              \
                'endif\n\n' % self.od_var)
        self.fp.write('# set list of runs\n')
        self.fp.write('set runs = (`count -digits 2 1 %d`)\n\n' % self.runs)

        self.fp.write('# create results and stimuli directories\n')
        self.fp.write('mkdir %s\nmkdir %s/stimuli\n%s\n' \
                      % (self.od_var, self.od_var, stat_inc))

        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
            str = '# copy stim files into stimulus directory\ncp'
            for ind in range(len(self.stims)):
                str += ' %s' % self.stims_orig[ind]
            str += ' %s/stimuli\n' % self.od_var
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        if len(self.extra_stims) > 0: # copy extra stim files into stim dir
            str = '# copy extra stim files\n'   \
                  'cp %s %s/stimuli\n' %        \
                  (' '.join(self.extra_stims_orig), self.od_var)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        if self.anat:
            str = '# copy anatomy to results dir\n'     \
                  '3dcopy %s %s/%s\n' %                 \
                      (self.anat.rel_input(), self.od_var, self.anat.prefix)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        # possibly copy over any volreg base
        if self.vr_ext_base != None:
            str = "# copy over the external volreg base\n"  \
                  "3dbucket -prefix %s/%s '%s'\n" %         \
                  (self.od_var, self.vr_ext_pre, self.vr_ext_base)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        # possibly copy over any align EPI base
        if self.align_ebase != None:
            str = "# copy over the external align_epi_anat.py EPI volume\n" \
                  "3dbucket -prefix %s/%s '%s'\n" %         \
                  (self.od_var, self.align_epre, self.align_ebase)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        opt = self.user_opts.find_opt('-regress_motion_file')
        if opt and len(opt.parlist) > 0:
            str = '# copy external motion file into results dir\n' \
                  'cp %s %s\n' %                                   \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        opt = self.user_opts.find_opt('-copy_files')
        if opt and len(opt.parlist) > 0:
            str = '# copy extra files into results dir\n' \
                  'cp -rv %s %s\n' %                      \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        # copy ricor_regs last to possibly match 3dTcat TR removal
        if len(self.ricor_regs) > 0:
            str = copy_ricor_regs_str(self)
            self.fp.write(add_line_wrappers(str))
            self.fp.write("%s\n" % stat_inc)

        if len(stat_inc) > 0:
            self.fp.write("# check for any setup failures\n"
                          "if ( $nerrors > 0 ) then\n"
                          "    echo '** setup failure ($nerrors errors)'\n"
                          "    exit\n"
                          "endif\n\n")

        self.fp.flush()

    # and last steps
    def finalize_script(self):
        str = '# %s\n\n' % block_header('auto block: cleanup')
        self.fp.write(str)

        if self.rm_rm and self.have_rm:
            self.fp.write('# remove temporary rm.* files\n'
                          '\\rm -f rm.*\n\n')

        if self.user_opts.find_opt('-move_preproc_files'):
            cmd_str = \
              "# move preprocessing files to 'preproc.data' directory\n"   \
              "mkdir preproc.data\n"                                       \
              "mv dfile.r??.1D outcount* pb??.$subj.r??.* rm.* preproc.data\n\n"
            self.fp.write(add_line_wrappers(cmd_str))
        elif self.user_opts.find_opt('-remove_preproc_files'):
            cmd_str = \
              "# remove preprocessing files to save disk space\n"   \
              "\\rm dfile.r??.1D pb??.$subj.r??.* rm.*\n\n"
            self.fp.write(add_line_wrappers(cmd_str))

        self.fp.write('# return to parent directory\n'
                      'cd ..\n\n')

        opt = self.user_opts.find_opt('-no_proc_command')
        if not opt:
            str = '\n\n\n'                              \
              '# %s\n'                                  \
              '# script generated by the command:\n'    \
              '#\n'                                     \
              '# %s %s\n' %                             \
                (block_header(''), os.path.basename(self.argv[0]),
                                 ' '.join(quotize_list(self.argv[1:],'')))
            self.fp.write(add_line_wrappers(str))

        if self.user_opts.find_opt('-ask_me'):
            str = '#\n# all applied options: '
            for opt in self.user_opts.olist:
                if opt.name == '-ask_me': continue
                str += opt.name+' '
                str += ' '.join(quotize_list(opt.parlist,''))
            str += '\n'
            self.fp.write(add_line_wrappers(str))

    # given a block, run, return a prefix of the form: pNN.SUBJ.rMM.BLABEL
    #    NN = block index, SUBJ = subj label, MM = run, BLABEL = block label
    def prefix_form(self, block, run, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        if self.sep_char == '.': # default
           return 'pb%02d.%s.r%02d.%s%s' %    \
                (self.bindex, self.subj_label, run, block.label, vstr)
        else:
           s = self.sep_char
           return 'pb%02d%s%s%sr%02d%s%s%s' %    \
                (self.bindex, s, self.subj_label, s, run, s, block.label, vstr)

    # same, but leave run as a variable
    def prefix_form_run(self, block, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        if self.sep_char == '.': # default
           return 'pb%02d.%s.r$run.%s%s' %    \
               (self.bindex, self.subj_label, block.label, vstr)
        else:
           s = self.sep_char
           return 'pb%02d%s%s%sr${run}%s%s%s' %    \
               (self.bindex, s, self.subj_label, s, s, block.label, vstr)

    # same as prefix_form, but use previous block values (index and label)
    # (so we don't need the block)
    def prev_prefix_form(self, run, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        if self.sep_char == '.': # default
           return 'pb%02d.%s.r%02d.%s%s' %    \
                (self.bindex-1, self.subj_label, run, self.pblabel, vstr)
        else:
           s = self.sep_char
           return 'pb%02d%s%s%sr%02d%s%s%s' %    \
                (self.bindex-1, s, self.subj_label,s,run,s, self.pblabel, vstr)

    # same, but leave run as a variable
    def prev_prefix_form_run(self, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        if self.sep_char == '.': # default
           return 'pb%02d.%s.r$run.%s%s' %    \
                (self.bindex-1, self.subj_label, self.pblabel, vstr)
        else:
           s = self.sep_char
           return 'pb%02d%s%s%sr${run}%s%s%s' %    \
                (self.bindex-1, s, self.subj_label, s, s, self.pblabel, vstr)

    # same, but leave run wild
    def prev_dset_form_wild(self, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        if self.sep_char == '.': # default
           return 'pb%02d.%s.r??.%s%s.HEAD' %    \
                (self.bindex-1, self.subj_label, self.pblabel, self.view)
        else:
           s = self.sep_char
           return 'pb%02d%s%s%sr??%s%s%s.HEAD' %    \
                (self.bindex-1, s, self.subj_label,s,s,self.pblabel, self.view)

    # like prefix, but list the whole dset form, in wildcard format
    def dset_form_wild(self, blabel, view=None):
        bind = self.find_block_index(blabel)
        if bind == None:
            print "** DFW: failed to find block for label '%s'" % blabel
            return ''
        if not view: view = self.view
        if self.sep_char == '.': # default
           return 'pb%02d.%s.r??.%s%s.HEAD' %      \
               (bind, self.subj_label, blabel, view)
        else:
           s = self.sep_char
           return 'pb%02d%s%s%sr??%s%s%s.HEAD' %      \
               (bind, s, self.subj_label, s, s, blabel, view)

class ProcessBlock:
    def __init__(self, label, proc):
        self.label = label      # label is block type
        self.valid = 0          # block is not yet valid
        self.apply = 1          # apply block to output script
        self.verb  = proc.verb  # verbose level
        if not label in BlockModFunc: return

        self.opts = OptionList(label)                   # init option list
        BlockModFunc[label](self, proc, proc.user_opts) # add block

        if self.verb > 2:
            if self.valid: self.show('+d successful add ')
            else:          print '+d invalid ProcessBlock: %s' % label

    def show(self, mesg):
        print '------- %sProcessBlock: %s -------' % (mesg, self.label)
        self.opts.show('new options: ')

def run_proc():

    ps = SubjProcSream('subject regression')
    ps.init_opts()

    rv = ps.get_user_opts()
    if rv != None:  # 0 is a valid return
        if rv != 0:
            show_args_as_command(ps.argv, "** failed command (get_user_opts):")
        return rv

    # run db_mod functions, and possibly allow other mods
    if ps.create_blocks():
        show_args_as_command(ps.argv, "** failed command (create_blocks):")
        return rv

    # run db_cm functions, to create the script
    rv = ps.create_script()
    if rv != None:  # terminal, but do not display command on 0
        if rv != 0:
            show_args_as_command(ps.argv, "** failed command (create_script):")
        return rv

    # finally, execute if requested
    if ps.user_opts.find_opt('-execute'): rv = os.system(ps.bash_cmd)

# main
if __name__ == '__main__':

    run_proc()

