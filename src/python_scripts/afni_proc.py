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
"""

g_version = "version 1.38, Mar 26, 2009"

# ----------------------------------------------------------------------
# dictionary of block types and modification functions

BlockLabels  = ['tcat', 'despike', 'tshift', 'volreg',
                'blur', 'mask', 'scale', 'regress', 'empty']
BlockModFunc  = {'tcat'   : db_mod_tcat,     'despike': db_mod_despike,
                 'tshift' : db_mod_tshift,
                 'volreg' : db_mod_volreg,   'blur'   : db_mod_blur,
                 'mask'   : db_mod_mask,     'scale'  : db_mod_scale,
                 'regress': db_mod_regress,  'empty'  : db_mod_empty}
BlockCmdFunc  = {'tcat'   : db_cmd_tcat,     'despike': db_cmd_despike,
                 'tshift' : db_cmd_tshift,
                 'volreg' : db_cmd_volreg,   'blur'   : db_cmd_blur,
                 'mask'   : db_cmd_mask,     'scale'  : db_cmd_scale,
                 'regress': db_cmd_regress,  'empty'  : db_cmd_empty}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# default block labels, and other labels (along with the label they follow)
DefLabels   = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
OtherDefLabels = {'despike':'tcat'}
OtherLabels    = ['empty']

# ----------------------------------------------------------------------
# data processing stream class
class SubjProcSream:
    def __init__(self, label):
        self.label      = label         # name for stream
        self.valid_opts = None          # list of possible user options
        self.user_opts  = None          # list of given user options

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
        self.mot_file   = 'dfile.rall.1D' # motion parameter file
        self.opt_src    = 'cmd'         # option source
        self.subj_id    = 'SUBJ'        # hopefully user will replace this
        self.subj_label = '$subj'       # replace this for execution
        self.out_dir    = ''            # output directory for use by script
        self.od_var     = ''            # output dir variable: $output_dir
        self.script     = None          # script name, default proc.SUBJ
        self.overwrite  = 0             # overwrite script file?
        self.fp         = None          # file object
        self.anat       = None          # anatomoy to copy (afni_name class)
        self.rm_rm      = 1             # remove rm.* files
        self.gen_review = '@epi_review.$subj' # filename for gen_epi_review.py

        self.verb       = 1             # verbosity level

        self.tr         = 0.0           # TR, in seconds
        self.reps       = 0             # TRs per run
        self.runs       = 0             # number of runs
        self.mask       = None          # mask dataset
        self.regmask    = 0             # apply any full_mask in regression
        self.view       = '+orig'       # view could also be '+tlrc'

        self.bindex     = 0             # current block index
        self.pblabel    = ''            # previous block label

        return
        
    def show(self, mesg):
        print '%sSubjProcSream: %s' % (mesg, self.label)
        for block in self.blocks:
            block.show('    Block %d: ' % self.blocks.index(block))
        print '    Dsets : ',
        if self.verb > 2:
            print
            for dset in self.dsets: dset.show()
        else:
            for dset in self.dsets: print dset.pv(),
            print
        if self.verb > 2: self.valid_opts.show('valid_opts: ')
        if self.verb > 1: self.user_opts.show('user_opts: ')

    def init_opts(self):
        self.valid_opts = OptionList('afni_proc.py options')

        # input style options  rcr - update
        # self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

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
        self.valid_opts.add_opt('-subj_id', -1, [],
                        helpstr='subject ID, used in most filenames')

        self.valid_opts.add_opt('-ask_me', 0, [],       # QnA session
                        helpstr='have afni_proc.py as the user for options')
        self.valid_opts.add_opt('-bash', 0, [],
                        helpstr='show execution help in bash syntax')
        self.valid_opts.add_opt('-copy_anat', 1, [],
                        helpstr='anatomy to copy to results directory')
        self.valid_opts.add_opt('-copy_files', -1, [],
                        helpstr='list of files to copy to results directory')
        self.valid_opts.add_opt('-gen_epi_review', 1, [],
                        helpstr='generate a script to review orig EPI data')
        self.valid_opts.add_opt('-no_epi_review', 0, [],
                        helpstr='do not generate an EPI review script')
        self.valid_opts.add_opt('-keep_rm_files', 0, [],
                        helpstr='do not delete temporary rm.* files')
        self.valid_opts.add_opt('-move_preproc_files', 0, [],
                        helpstr='move preprocessing files to preproc.data dir')
        self.valid_opts.add_opt('-remove_preproc_files', 0, [],
                        helpstr='remove pb0* preprocessing files')
        self.valid_opts.add_opt('-tlrc_anat', 0, [],
                        helpstr='run @auto_tlrc on anat from -copy_anat')
        self.valid_opts.add_opt('-tlrc_base', 1, [],
                        helpstr='alternate @auto_tlrc base (not TT_N27, say)')
        self.valid_opts.add_opt('-tlrc_no_ss', 0, [],
                        helpstr='do not skull-strip during @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_rmode', 1, [],
                        helpstr='resample mode applied in @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_suffix', 1, [],
                        helpstr='suffix applied in @auto_tlrc (default: NONE)')

        # block options
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [],
                        helpstr='num TRs to remove from start of each run')

        self.valid_opts.add_opt('-despike_opts_3dDes', -1, [],
                        helpstr='additional options directly for 3dDespike')

        self.valid_opts.add_opt('-tshift_align_to', -1, [],
                        helpstr='time alignment option given to 3dTshift')
        self.valid_opts.add_opt('-tshift_interp', 1, [],
                        helpstr='interpolation method used in 3dTshift')
        self.valid_opts.add_opt('-tshift_opts_ts', -1, [],
                        helpstr='additional options directly for 3dTshift')

        self.valid_opts.add_opt('-volreg_align_to', 1, [],
                                ['first','third', 'last'],
                        helpstr="align to 'first', 'third' or 'last' TR")
        self.valid_opts.add_opt('-volreg_base_dset', 1, [],
                        helpstr='external dataset to use as volreg base')
        self.valid_opts.add_opt('-volreg_base_ind', 2, [],
                        helpstr='run/sub-brick indices for volreg')
        self.valid_opts.add_opt('-volreg_interp', 1, [],
                        helpstr='interpolation method used in volreg')
        self.valid_opts.add_opt('-volreg_opts_vr', -1, [],
                        helpstr='additional options directly for 3dvolreg')
        self.valid_opts.add_opt('-volreg_zpad', 1, [],
                        helpstr='number of slices to pad by in volreg')

        self.valid_opts.add_opt('-blur_filter', 1, [],
                        helpstr='blurring filter option (def: -1blur_fwhm)')
        self.valid_opts.add_opt('-blur_size', 1, [],
                        helpstr='size of blur kernel (def: 4)')
        self.valid_opts.add_opt('-blur_opts_merge', -1, [],
                        helpstr='additional options directly for 3dmerge')

        self.valid_opts.add_opt('-mask_type', 1, [], ['union','intersection'],
                        helpstr="specify a 'union' or 'intersection' mask type")
        self.valid_opts.add_opt('-mask_dilate', 1, [],
                        helpstr="dilation to be applied in automask")

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
        self.valid_opts.add_opt('-regress_basis_normall', 1, [],
                        helpstr="specify magnitude of basis functions")
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
        self.valid_opts.add_opt('-regress_motion_file', 1, [],
                        helpstr="external motion regressors to apply")
        self.valid_opts.add_opt('-regress_extra_stim_files', -1, [],
                        helpstr="extra -stim_files to apply")
        self.valid_opts.add_opt('-regress_extra_stim_labels', -1, [],
                        helpstr="labels for extra -stim_files")

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
        self.valid_opts.add_opt('-regress_make_ideal_sum', 1, [],
                        helpstr="filename for sum of ideal regressors")
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
                        helpstr='additional options directly for 3dDeconvolve')
        self.valid_opts.add_opt('-regress_reml_exec', 0, [],
                        helpstr="execute 3dREMLfit command script")
        self.valid_opts.add_opt('-regress_RONI', -1, [],
                        helpstr="1-based list of regressors of no interest")

        # other options
        self.valid_opts.add_opt('-help', 0, [],
                        helpstr="show this help")
        self.valid_opts.add_opt('-hist', 0, [],
                        helpstr="show revision history")
        self.valid_opts.add_opt('-show_valid_opts', 0, [],
                        helpstr="show all valid options")
        self.valid_opts.add_opt('-ver', 0, [],
                        helpstr="show module version")
        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr="set the verbose level")

        self.valid_opts.trailers = 0   # do not allow unknown options
        
    def get_user_opts(self):
        self.user_opts = read_options(sys.argv, self.valid_opts)
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

        if self.verb > 1: show_args_as_command(sys.argv, "executing command:")
        if self.verb > 3: self.show('end get_user_opts ')

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
        
        if opt_list.find_opt('-ver'):      # show the version string
            print g_version
            return 0  # gentle termination
        
        opt = opt_list.find_opt('-copy_anat')
        if opt != None: self.anat = afni_name(opt.parlist[0])

        opt = opt_list.find_opt('-gen_epi_review')  # name epi review script
        if opt != None: self.gen_review = opt.parlist[0]

        opt = opt_list.find_opt('-no_epi_review') # no epi review script
        if opt != None: self.gen_review = None

        opt = opt_list.find_opt('-keep_rm_files')
        if opt != None: self.rm_rm = 0

        opt = opt_list.find_opt('-out_dir')
        if opt != None: self.out_dir = opt.parlist[0]

        opt = opt_list.find_opt('-subj_id') # -- needs to be before script
        if opt != None: self.subj_id = opt.parlist[0]

        opt = opt_list.find_opt('-script')
        if opt != None: self.script = opt.parlist[0]
        else:           self.script = 'proc.%s' % self.subj_id

        opt = opt_list.find_opt('-scr_overwrite')
        if opt != None: self.overwrite = 1

    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        # first, note datasets
        opt = self.user_opts.find_opt('-dsets')
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))
            if self.dsets[0].view != self.view:
                self.view = self.dsets[0].view
                if self.verb > 0: print '-- applying view as %s' % self.view

        blocklist = DefLabels  # init to defaults

        # check for -do_block options
        opt = self.user_opts.find_opt('-do_block')
        if opt and opt.parlist and len(opt.parlist) > 0:
            if self.user_opts.find_opt('-blocks'):
                print '** error: -do_block invalid when using -blocks'
                return 1

            # check additional blocks one by one
            errs = 0
            for bname in opt.parlist:
                if bname in OtherDefLabels:
                    preindex = blocklist.index(OtherDefLabels[bname])
                    if preindex < 0:
                        print "** error: -do_block failure for '%s'" % bname
                        errs += 1
                    # so add the block to blocklist
                    preindex += 1
                    blocklist[preindex:preindex] = [bname]
                else:
                    print "** error: '%s' is invalid in '-do_block'" % bname
                    errs += 1
            if errs > 0 : return 1

        opt = self.user_opts.find_opt('-blocks')
        if opt:  # then create blocklist from user opts (but prepend tcat)
            if opt.parlist[0] != 'tcat':
                blocklist = ['tcat'] + opt.parlist
            else: blocklist = opt.parlist

        for label in blocklist:
            rv = self.add_block(label)
            if rv != None: return rv

        # maybe the user wants to be quizzed for options
        uopt = self.user_opts.find_opt('-ask_me')
        if uopt != None:
            if ask_me.ask_me_subj_proc(self):
                return 1
            for label in blocklist:
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

        if not uniq_list_as_dsets(self.dsets, 1): return 1
        if not db_tlrc_opts_okay(self.user_opts): return 1

    # create script from blocks and options
    def create_script(self):
        rv = self.get_run_info()
        if rv != None: return rv

        rv = self.init_script()         # create the script file and header
        if rv != None: return rv

        errs = 0
        for block in self.blocks:
            if not block.apply: continue        # skip such blocks
            cmd_str = BlockCmdFunc[block.label](self, block)
            if cmd_str == None:
                print "** script creation failure for block '%s'" % block.label
                errs += 1
            else:
                self.fp.write(add_line_wrappers(cmd_str))
                if self.verb>2: block.show('+d post command creation: ')
                if self.verb>1: print '+d %s cmd: \n%s'%(block.label, cmd_str)

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

        if errs > 0: return 1    # so we print all errors before leaving

        if self.verb > 0:
            # last warning, if user is masking EPI data...
            if self.mask != None and self.regmask:
                print "** masking EPI data is not recommended"

            if self.runs == 1:
                print "\n-------------------------------------\n" \
                        "** warning have only 1 run to analyze\n" \
                        "-------------------------------------"
            print "\n--> script is file: %s" % self.script
            if self.user_opts.find_opt('-bash'): # give bash form
                print '    (consider: "tcsh -x %s 2>&1 | tee output.%s") ' \
                                % (self.script, self.script)
            else:                                # give tcsh form
              print '    (consider the command "tcsh -x %s |& tee output.%s")' \
                                % (self.script, self.script)
            print

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

    # create a new block for the given label, and append it to 'blocks'
    def add_block(self, label):
        block = ProcessBlock(label, self)
        if not block.valid:
            print '** invalid block : %s' % block.label
            return 1
        if self.verb > 2: block.show('+d post init block: ')
        self.blocks.append(block)

    def find_block(self, label):
        for block in self.blocks:
            if block.label == label: return block
        return None

    def find_block_index(self, label):
        block = self.find_block(label)
        if block: return self.blocks.index(block)
        return None

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

        self.fp.write('#!/usr/bin/env tcsh\n\n')
        self.fp.write('echo "auto-generated by afni_proc.py, %s"\n' % asctime())
        self.fp.write('echo "(%s)"\n\n'% g_version)

        # include execution method in script
        if self.user_opts.find_opt('-bash'): # give bash form
            self.fp.write('# execute via : tcsh -x %s 2>&1 | tee output.%s\n\n'\
                            % (self.script, self.script))
        else:                                # give tcsh form
            self.fp.write('# execute via : tcsh -x %s |& tee output.%s\n\n' \
                            % (self.script, self.script))

        self.fp.write('# --------------------------------------------------\n'
                      '# script setup\n\n'
                      '# the user may specify a single subject to run with\n')
        self.fp.write('if ( $#argv > 0 ) then  \n'
                      '    set subj = $argv[1] \n'
                      'else                    \n'
                      '    set subj = %s       \n'
                      'endif\n\n' % self.subj_id )
        self.fp.write('# assign output directory name\n'
                      'set output_dir = %s\n\n' % self.out_dir)
        self.fp.write('# verify that the results directory does not yet exist\n'
                      'if ( -d %s ) then \n'
                      '    echo output dir "$subj.results" already exists\n'
                      '    exit                     \n'
                      'endif\n\n' % self.od_var)
        self.fp.write('# set list of runs\n')
        self.fp.write('set runs = (`count -digits 2 1 %d`)\n\n' % self.runs)

        self.fp.write('# create results directory\n')
        self.fp.write('mkdir %s\n\n' % self.od_var)

        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
            str = '# create stimuli directory, and copy stim files\n' \
                  'mkdir %s/stimuli\ncp ' % self.od_var
            for ind in range(len(self.stims)):
                str += '%s ' % self.stims_orig[ind]
            str += '%s/stimuli\n\n' % self.od_var
            self.fp.write(add_line_wrappers(str))

        if len(self.extra_stims) > 0: # copy extra stim files into stim dir
            str = '# copy extra stim files\n'   \
                  'cp %s %s/stimuli\n\n' %      \
                  (' '.join(self.extra_stims_orig), self.od_var)
            self.fp.write(add_line_wrappers(str))

        if self.anat:
            str = '# copy anatomy to results dir\n'     \
                  '3dcopy %s %s/%s\n\n' %               \
                      (self.anat.rpv(), self.od_var, self.anat.prefix)
            self.fp.write(add_line_wrappers(str))

        # possibly copy over any volreg base
        if self.vr_ext_base != None:
            str = "# copy over the external volreg base\n"  \
                  "3dbucket -prefix %s/%s '%s'\n\n" %       \
                  (self.od_var, self.vr_ext_pre, self.vr_ext_base)
            self.fp.write(add_line_wrappers(str))

        opt = self.user_opts.find_opt('-copy_files')
        if opt and len(opt.parlist) > 0:
            str = '# copy extra files into results dir\n' \
                  'cp -rv %s %s\n\n' %                    \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.fp.write(add_line_wrappers(str))

        self.fp.flush()

    # and last steps
    def finalize_script(self):
        str = '# -------------------------------------------------------\n\n'
        self.fp.write(str)

        if self.rm_rm:
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

        if self.user_opts.find_opt('-tlrc_anat'):
            cmd_str = db_cmd_tlrc(self.anat.pv(), self.user_opts)
            if cmd_str == None:
                print "** script creation failure for block 'tlrc'"
                return 1
            self.fp.write(add_line_wrappers(cmd_str))

        self.fp.write('# return to parent directory\n'
                      'cd ..\n\n')

        opt = self.user_opts.find_opt('-no_proc_command')
        str = '\n\n\n'                                                       \
              '# -------------------------------------------------------\n'  \
              '# script generated by the command:\n'                         \
              '#\n'                                                          \
              '# %s %s\n' % (os.path.basename(sys.argv[0]),
                                 ' '.join(quotize_list(sys.argv[1:],'')))
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
        return 'pb%02d.%s.r%02d.%s%s' %    \
                (self.bindex, self.subj_label, run, block.label, vstr)

    # same, but leave run as a variable
    def prefix_form_run(self, block, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        return 'pb%02d.%s.r$run.%s%s' %    \
               (self.bindex, self.subj_label, block.label, vstr)

    # same as prefix_form, but use previous block values (index and label)
    # (so we don't need the block)
    def prev_prefix_form(self, run, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        return 'pb%02d.%s.r%02d.%s%s' %    \
                (self.bindex-1, self.subj_label, run, self.pblabel, vstr)

    # same, but leave run as a variable
    def prev_prefix_form_run(self, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        return 'pb%02d.%s.r$run.%s%s' %    \
                (self.bindex-1, self.subj_label, self.pblabel, vstr)

    # same, but leave run wild
    def prev_dset_form_wild(self, view=0):
        if view: vstr = self.view
        else:    vstr = ''
        return 'pb%02d.%s.r??.%s%s.HEAD' %    \
                (self.bindex-1, self.subj_label, self.pblabel, self.view)

    # like prefix, but list the whole dset form, in wildcard format
    def dset_form_wild(self, blabel):
        bind = self.find_block_index(blabel)
        if bind == None:
            print "** DFW: failed to find block for label '%s'" % blabel
            return ''
        return 'pb%02d.%s.r??.%s%s.HEAD' %      \
               (bind, self.subj_label, blabel, self.view)

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
            show_args_as_command(sys.argv, "** failed command (get_user_opts):")
        return rv

    if ps.create_blocks():
        show_args_as_command(sys.argv, "** failed command (create_blocks):")
        return rv

    rv = ps.create_script()
    if rv != None:  # terminal, but do not display command on 0
        if rv != 0:
            show_args_as_command(sys.argv, "** failed command (create_script):")
        return rv

# main
if __name__ == '__main__':

    run_proc()

