#!/usr/bin/env python

# system libraries
import sys, os, glob

if 1 :  # for testing, might add the current dir and ~/abin to the PATH
   try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
   except: pass

# AFNI libraries
import afni_base as BASE
import option_list as OL
import afni_util as UTIL        # not actually used, but probably will be
import lib_afni1D as LAD
import lib_subjects as SUBJ

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
gen_ss_review_scripts.py - generate single subject analysis review scripts

        o  figure out basic details (sid, trs, xmat, censor stats files, etc.)
        o  generate an @ss_review_basic script to output simple details about
           this subject and results
        o  generate a @ss_review_driver script to actually inspect the results
           (running some commands by the user's control)
        o  generate @ss_review_driver_commands
           (same as @ss_review_driver, but a pure command file)

------------------------------------------

   examples:

      1. Run this program without any options, assuming everything is there.

                gen_ss_review_scripts.py

         Additional run the basic review script and then the drive script.

                ./@ss_review_basic
                ./@ss_review_driver

      2. Esoteric.  Set all the output file names (for now via control vars).

                gen_ss_review_scripts.py              \\
                        -cvar scr_basic ~/tmp/s.basic \\
                        -cvar scr_drive ~/tmp/s.drive \\
                        -cvar cmds_drive ~/tmp/s.cmds \\
                        -cvar xstim ~/tmp/x.stim.1D

       2b. Similar to 2, but put all scripts and intermediate files under
           a new ~/tmp/gen_dir.  So as an example for testing:

                mkdir ~/tmp/gen_dir
                gen_ss_review_scripts.py -cvar out_prefix ~/tmp/gen_dir/

           Note that if out_prefix is a directory, it will need a trailing
           '/', since it is a file name prefix.

       2c. Simplified.  Use -prefix instead of -cvar out_prefix.

                gen_ss_review_scripts.py -prefix test.

------------------------------------------

   required files/datasets (these must exist in the current directory):

      variable name        example file name
      -------------        -----------------
      tcat_dset            pb00.FT.r01.tcat+orig.HEAD
      outlier_dset         outcount_rall.1D
      enorm_dset           motion_FT_enorm.1D
      censor_dset          motion_FT_censor.1D
      motion_dset          dfile_rall.1D
      volreg_dset          pb02.FT.r01.volreg+tlrc.HEAD
      xmat_regress         X.xmat.1D
      stats_dset           stats.FT+tlrc.HEAD
      final_anat           FT_anat+tlrc.HEAD

   optional files/datasets (censor files are required if censoring was done):

      mask_dset            full_mask.FT+tlrc.HEAD
      censor_dset          motion_FT_censor.1D
      sum_ideal            sum_ideal.1D
      xmat_uncensored      X.nocensor.xmat.1D
      tsnr_dset            TSNR.ft+tlrc.HEAD

------------------------------------------

   terminal options:

      -help                     : show this help
      -hist                     : show module history
      -show_uvar_dict           : show all user variables
      -show_uvar_eg             : show example of user variables
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options

      -exit0                    : regardless of errors, exit with status 0
      -prefix OUT_PREFIX        : set prefix for script names
      -verb LEVEL               : set the verbosity level

   options for setting main variables

      -subj SID                 : subject ID
      -rm_trs N                 : number of TRs removed per run
      -num_stim N               : number of main stimulus classes
      -motion_dset DSET         : motion parameters
      -outlier_dset DSET        : outlier fraction time series
      -enorm_dset DSET          : euclidean norm of motion params
      -mot_limit LIMIT          : (optional) motion limit - maybe for censoring
      -out_limit LIMIT          : (optional) outlier fraction limit
      -xmat_regress XMAT        : X-matrix file used in regression (X.xmat.1D)
      -xmat_uncensored XMAT     : if censoring, un-censored X-matrix file
      -stats_dset DSET          : output from 3dDeconvolve
      -final_anat DSET          : final anatomical dataset
      -final_view VIEW          : final view of data (e.g. 'orig' or 'tlrc')

      -uvar VAR PARAMS ...      : generic option form for control variables
      -uvar VAR PARAMS ...      : generic option form for user variables


-----------------------------------------------------------------------------

Here are some potential artifacts to ponder (just so they are saved
somewhere), as noted by many of us, including D Glen and J Gonzalez.
We can try to add to this list, and maybe even do something to take
them off <gasp!>.

    1. Striping - across slices - EPI, anatomical
    2. Artifacts - checkerboard, ringing - EPI, anatomical
    3. Spiking (regional or global)
        - global would be caught in the outlier fractions
    4. Shifts in baseline (regional or global)
        - maybe @ANATICOR can help to deal with it, but how to notice?
    5. "PURE" on or off / acquisition protocol changes
    6. Poor contrast between CSF and WM/GM in EPI
    7. Low resolution anatomical data
    8. Noisy anatomical data
    9. Left-right flipping between anatomical and EPI
        - run align_epi_anat.py between flipped versions
          (as was done by _____ on the fcon_1000 data)
   10. Poor alignment between anatomical and EPI
        - currently users can view as part of @ss_review_driver
        - can use some large limit test on value from out.mask_overlap.txt
   11. Excessive motion
        - currently report average motion and censor details
   12. "Reshimming-like" shears between EPI volumes
   13. Non-uniformity because of surface coils
   14. Incorrect DICOM data
   15. Inconsistent data types within a study
   16. TR not properly set
   17. Missing data
   18. Inconsistent number of TRs within multiple EPI datasets
   19. Missing pre-steady state in EPI data

-----------------------------------------------------------------------------

Thanks to J Jarcho and C Deveney for suggestions, feedback and testing.

R Reynolds    July 2011
=============================================================================
"""

# cannot have empty line
g_basic_header_str = """#!/bin/tcsh

# ----------------------------------------------------------------------
# output some basic details about this subjects input and results:
#   - subject ID, TRs removed, num stim files, motion limit, outlier limit
#   - num runs, TRs per run, total TRs
#   - TRs censored, fraction, num regs of interest
#   - per reg: num TRs applied, num TRs censored, fraction censored
#   - num stim files found, TSNR average, max F-stat, blur estimates
#
# This script should be run from a '.results' directory, produced by an
# afni_proc.py processing script.
# 
# NOTE: this script is not particularly meant to be easy to read and follow,
#       but is more to just be executed.  Yanking numbers from datasets is
#       not such a common need.
#
# NOTE: this script creates one new file: X.stim.xmat.1D
#
#       - an X-matrix containing only uncensored regressors of interest
#
#       This new file is used to compute fractions of "response TRs" censored,
#       per stimulus class of interest, where a "response TR" means the given
#       regressor was non-zero at that TR.
#

"""

g_overview_str = """
# ------------------------------------------------------------
# some overview details
echo ""
echo "subject ID                : $subj"

if ( $?rm_trs ) then
   set value = $rm_trs
else
   set value = UNKNOWN
endif
echo "TRs removed (per run)     : $value"

echo "num stim classes provided : $num_stim"
"""

g_mot_n_trs_str = """
# ------------------------------------------------------------
# report motion limit, average motion and number of TRs exceeding limit

echo "motion limit              : $mot_limit"

set mcount = `1deval -a $enorm_dset -expr "step(a-$mot_limit)"      \\
                        | awk '$1 != 0 {print}' | wc -l`
echo "num TRs above mot limit   : $mcount"

set mmean = `3dTstat -prefix - -mean $enorm_dset\\' | & tail -n 1`
echo "average motion (per TR)   : $mmean"

if ( $?motion_dset ) then
    if ( $was_censored ) then
        # compute average censored motion
        1deval -a $enorm_dset -b $censor_dset -expr 'a*b' > rm.ec.1D
        set mmean = `3dTstat -prefix - -nzmean rm.ec.1D\\' | & tail -n 1`
        echo "average censored motion   : $mmean"
        \\rm -f rm.ec.1D
    endif

    # compute the maximum motion displacement over all TR pairs
    set disp = `1d_tool.py -infile $motion_dset -show_max_displace -verb 0`
    echo "max motion displacement   : $disp"

    if ( $was_censored ) then
        # compute the maximum motion displacement over all TR pairs
        set disp = `1d_tool.py -infile $motion_dset -show_max_displace \\
                               -censor_infile $censor_dset -verb 0`
        echo "max censored displacement : $disp"
    endif
endif

# ------------------------------------------------------------
# report outlier limit, average and number of TRs exceeding limit

echo "outlier limit             : $out_limit"

set mmean = `3dTstat -prefix - -mean $outlier_dset\\' | & tail -n 1`
echo "average outlier frac (TR) : $mmean"

set mcount = `1deval -a $outlier_dset -expr "step(a-$out_limit)"      \\
                        | awk '$1 != 0 {print}' | wc -l`
echo "num TRs above out limit   : $mcount"


echo ""

# ------------------------------------------------------------
# get tcat files and count TRs
set tcat_files = ( `find . -maxdepth 1 -name "pb00*$subj*.HEAD" -print` )
if ( $#tcat_files == 0 ) then
    echo "** missing tcat files, skipping num runs and TRs per run..."
    echo ""
else
    set trs = ( )
    foreach file ( $tcat_files )
        set trs = ( $trs `3dnvals $file` )
    end
    echo "num runs found            : $#tcat_files"
    echo "num TRs per run           : $trs"
endif

# count total (uncensored) TRs from X-matrix (as a check against sum of TRs)
if ( $?xmat_uncensored ) then
   set xmat = $xmat_uncensored
else
   set xmat = $xmat_regress
endif
set rows_cols = ( `1d_tool.py -infile $xmat -show_rows_cols -verb 0` )
set num_trs = $rows_cols[1]
echo "TRs total (uncensored)    : $num_trs"
echo ""

"""

g_censor_results_str = """
# ----------------------------------------------------------------------
# report censoring results (if applicable)
if ( $was_censored ) then
    set ntr_censor = `cat $censor_dset | grep 0 | wc -l`
    echo "TRs censored              : $ntr_censor"
    echo "censor fraction           : `ccalc $ntr_censor/$num_trs`"

    # note number of regressors of interest
    set rc = ( `1d_tool.py -infile $xstim -show_rows_cols -verb 0` )
    set nint = $rc[2]
    echo "num regs of interest      : $nint"

    # compute fractions of stimulus TRs censored in each
    set stim_trs = ()
    set stim_trs_censor = ()
    set stim_frac_censor = ()
    @ nm1 = $nint - 1
    foreach index ( `count -digits 1 0 $nm1` )
        # count response TRs, with and without censoring
        # (their difference is the number of TRs lost to censoring)
        set st = `1deval -a $xstim"[$index]" -expr 'bool(a)' | grep 1 | wc -l`
        set sc = `1deval -a $xstim"[$index]" -b $censor_dset -expr 'bool(a*b)'\
                         | grep 1 | wc -l`
        set sc = `ccalc -i "$st-$sc"`           # change to num censored
        set ff = `ccalc -form '%.3f' "$sc/$st"` # and the fraction censored

        # keep lists of reponse TRs, # censored and fractions censored
        # (across all stimulus regressors)
        set stim_trs = ( $stim_trs $st )
        set stim_trs_censor = ( $stim_trs_censor $sc )
        set stim_frac_censor = ( $stim_frac_censor $ff )
    end

    echo "num TRs per stim          : $stim_trs"
    echo "num TRs censored per stim : $stim_trs_censor"
    echo "fraction TRs censored     : $stim_frac_censor"
    echo ""
endif  # $was_censored

"""

# rcr - do not include this...
g_basic_count_sfiles = """
# ------------------------------------------------------------
# count stim files
set nstim_found = `\ls stimuli | wc -w`
echo "num stim files found      : $nstim_found"
"""

g_basic_tsnr_str = """
# ------------------------------------------------------------
# get TSNR average
if ( -f $tsnr_dset && -f $mask_dset ) then
    eval 'set tsnr_ave = `3dmaskave -quiet -mask $mask_dset $tsnr_dset`' \
         >& /dev/null
    echo "TSNR average              : $tsnr_ave"
endif
"""

g_basic_finish_str = """
# ------------------------------------------------------------
# note maximum F-stat
if ( -f $stats_dset ) then
    set fmax = `3dBrickStat -slow -max $stats_dset"[Full_Fstat]"`
    echo "maximum F-stat            : $fmax"
endif

# ------------------------------------------------------------'
# note blur estimates
set blur_file = blur_est.$subj.1D
if ( -f $blur_file ) then
    set best = `awk '/errts/ {print $1, $2, $3}' $blur_file`
    if ( $#best != 3 ) then
        set best = `awk '/epits/ {print $1, $2, $3}' $blur_file`
    endif
    if ( $#best == 3 ) then
        echo "blur estimates            : $best"
    endif
endif

echo ""

# ------------------------------------------------------------'
# if there happen to be pre-steady state warnings, show them
set pre_ss_warn = 'out.pre_ss_warn.txt'
if ( -f $pre_ss_warn ) then
    cat $pre_ss_warn
    echo ""
endif

"""

g_drive_init_str = """#!/bin/tcsh

# This script is meant to help review single subject results.
#
# It should be run from a '.results' directory, produced by an
# afni_proc.py processing script.
# 
# It will not use variables, so that commands are very clear to users
# who read the script.  Hopefully the only "ugly" things will be the
# prompt_user commands, which need to pass text to the prompt program.
#
# Some AFNI commands have extra options supplied to make them more
# clear when executing.  Users need not bother with such worries when
# running similar commands on their own.
#
# at each step:
#    - a "review" command is executed
#    - a prompt explains to the user what to do or look at
#    - when ready to proceed, the user clicks OK in the prompt window

"""

g_no_oblique_warn_str = \
"""# ------------------------------------------------------------
# try to avoid any oblique warnings throughout script
setenv AFNI_NO_OBLIQUE_WARNING YES

"""

g_eg_uvar = SUBJ.VarsObject('sample user vars')
g_eg_uvar.subj            = 'FT'
g_eg_uvar.rm_trs          = 2
g_eg_uvar.num_stim        = 2
g_eg_uvar.tcat_dset       = 'pb00.FT.r01.tcat+orig.HEAD'
g_eg_uvar.enorm_dset      = 'motion_FT_enorm.1D'
g_eg_uvar.censor_dset     = 'motion_FT_censor.1D'
g_eg_uvar.motion_dset     = 'dfile_rall.1D'
g_eg_uvar.outlier_dset    = 'outcount_rall.1D'
g_eg_uvar.mot_limit       = 0.3
g_eg_uvar.out_limit       = 0.1
g_eg_uvar.xmat_regress    = 'X.xmat.1D'
g_eg_uvar.xmat_uncensored = 'X.nocensor.xmat.1D'
g_eg_uvar.stats_dset      = 'stats.FT+tlrc.HEAD'
g_eg_uvar.sum_ideal       = 'sum_ideal.1D'
g_eg_uvar.align_anat      = 'FT_anat_al_junk+orig.HEAD'
g_eg_uvar.final_anat      = 'anat_final.FT+tlrc.HEAD'
g_eg_uvar.final_view      = 'tlrc'
g_eg_uvar.mask_dset       = 'full_mask.FT+tlrc.HEAD'
g_eg_uvar.tsnr_dset       = 'TSNR.FT+tlrc.HEAD'

# dictionary of variable names with help string
g_uvar_dict = { 
 'subj'             :'set subject ID',
 'rm_trs'           :'set number of TRs removed per run',
 'num_stim'         :'set number of main stimulus classes',
 'tcat_dset'        :'set first tcat dataset',
 'censor_dset'      :'set motion_censor file',
 'enorm_dset'       :'set motion_enorm file',
 'motion_dset'      :'set motion parameter file',
 'outlier_dset'     :'set outcount_rall file',
 'mot_limit'        :'set motion limit (maybe for censoring)',
 'out_limit'        :'set outlier limit (maybe for censoring)',
 'xmat_regress'     :'set X-matrix file used in regression',
 'xmat_uncensored'  :'if censoring, set un-censored X-matrix',
 'stats_dset'       :'set main output from 3dDeconvolve',
 'sum_ideal'        :'set 1D file for ideal sum',
 'align_anat'       :'anat aligned with original EPI',
 'final_anat'       :'anat aligned with stats dataset',
 'final_view'       :'set final view of data (orig/tlrc)',
 'mask_dset'        :'set EPI mask',
 'tsnr_dset'        :'set temporal signal to noise dataset',
 # todo
 'template_space'   :'set final view of data (orig/tlrc)'
}

g_cvars_defs = SUBJ.VarsObject('default control vars')
g_cvars_defs.verb       = 1
g_cvars_defs.scr_basic  = '@ss_review_basic'
g_cvars_defs.scr_drive  = '@ss_review_driver'
g_cvars_defs.cmds_drive = '@ss_review_driver_commands'
g_cvars_defs.xstim      = 'X.stim.xmat.1D'
g_cvars_defs.out_prefix = ''    # if set, use as prefix to cvar files
g_cvars_defs.exit0      = 0     # if set, return 0 even on errors

# this is a corresponding list of control vars that define output files
g_outfile_cvars = ['scr_basic', 'scr_drive', 'cmds_drive', 'xstim' ]

g_history = """
   gen_ss_review_scripts.py history:

   0.0  Jul 02, 2011    - initial version
   0.1  Jul 11, 2011    - almost a "release version"
        - some updates to the help
        - added more basic tests
        - updated driver script to useful level
   0.2  Jul 11, 2011
        - added -exit0, so errors would not terminate scripts 
        - babbled in -help about our old list of artifacts to ponder
   0.3  Jul 15, 2011
        - added 'max motion displacement' to basic script
        - check if X.stim.xmat.1D already exists
   0.4  Jul 21, 2011: changed TR counts to come via awk instead of grep
   0.5  Aug 02, 2011: added control var out_prefix, a prefix for output files
   0.6  Aug 12, 2011: gave volreg 3dAllineate command priority for final anat
   0.7  Aug 17, 2011: fixed some final anat dset assignments
   0.8  Sep 22, 2011: 
        - added check_for_file
        - updated find_x_mat, guess_enorm_dset, drive_view_stats
   0.9  Oct  4, 2011:
        - added 'max censored displacement', 'final anat dset',
                'final voxel resolution' to basic script
        - removed 'num stim files found'
   0.10 Oct 25, 2011:
        - look for files with '_' separators
        - added for J Weisberg
   0.11 Nov 02, 2011: added out.TENT_warn.txt to warning file review
   0.12 Nov 21, 2011: fixed -ynames in plot of motion/outliers
   0.13 Jan 28, 2012: look for TSNR* in case of surf analysis
   0.14 Jan 31, 2012: look for aligned anat: _al_junk/keep (but not yet used)
   0.15 Feb 01, 2012: check for pre-steady state outliers
   0.16 Feb 10, 2012:
        - make tcat files optional (for J Britton)
        - apply prefix to driver commands
   0.17 Mar 21, 2012:
        - look harder for motion files
        - use '3dinfo -ad3' for voxel resolution (so no negatives)
        - use 3 decimal places for TR censor fractions
   0.18 Apr 12, 2012: replace enumerate(), for backport to python 2.2
   0.19 May 01, 2012: added -prefix option; added censoring to 1dplot commands
   0.20 May 09, 2012: accomodate more than 99 runs
   0.21 May 11, 2012: also output average censored motion (per TR)
   0.22 Jun 14, 2012: use afni -com instead of plugout_drive
                      (avoids issue on systems with multiple users)
                      Thanks to V Razdan and N Adleman for noting the issue.
   0.23 Jun 25, 2012: ick, fixed uninitilaized cpad1,2 (if no censoring)
   0.24 Jul 17, 2012:
        - add checks for volreg and uncensored X-mat
        - probably init view from volreg
   0.25 Aug 23, 2012: allow passing of -censor_dset
   0.26 Sep 06, 2012: print missing xmat message w/out debug as it is fatal
   0.27 Apr 29, 2013: set AFNI_NO_OBLIQUE_WARNING in scripts
"""

g_version = "gen_ss_review_scripts.py version 0.27, April 29, 2013"

g_todo_str = """
   - figure out template_space
   - add @epi_review execution as a run-time choice (in the 'drive' script)
   - generate and evaluate overlap mask(s) (atlas regions)
   - execute basic?  save output?
"""

# todo notes:
# - if volreg+tlrc, could get final_anat from '3dAllineate -base'
#   or 'adwarp -apar' from HISTORY
# - can have censor file but no limits (report as such in basic)
#   --> do not allow in case of inspect?

class MyInterface:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None
      self.uvars           = SUBJ.VarsObject('user variables')
      self.cvars           = SUBJ.VarsObject('control variables')
      self.cvars.merge(g_cvars_defs)

      self.dsets           = SUBJ.VarsObject('dset instances')

      # script text
      self.text_basic      = ''
      self.text_drive      = ''

      # initialize valid_opts
      self.valid_opts = self.get_valid_opts()

   def get_valid_opts(self):
      vopts = OL.OptionList('valid opts')

      # short, terminal arguments
      vopts.add_opt('-help', 0, [], helpstr='display program help')
      vopts.add_opt('-help_todo', 0, [], helpstr='display current "todo" list')
      vopts.add_opt('-hist', 0, [], helpstr='display the modification history')
      vopts.add_opt('-show_uvar_dict', 0, [],
                    helpstr='show all user variables in dictionary')
      vopts.add_opt('-show_uvar_eg', 0, [],
                    helpstr='show user var example (AFNI_data6)')
      vopts.add_opt('-show_cvar_defs', 0, [],
                    helpstr='show control var defaults')
      vopts.add_opt('-show_valid_opts', 0, [],\
                    helpstr='display all valid options')
      vopts.add_opt('-ver', 0, [], helpstr='display the current version number')

      # user variable options - add from dictionary
      ukeys = g_uvar_dict.keys()
      ukeys.sort()
      for opt in ukeys:
         vopts.add_opt('-'+opt, -1, [], helpstr=g_uvar_dict[opt])

      # general options
      vopts.add_opt('-cvar', -2, [],
                    helpstr='set given control variable to given value(s)')
      vopts.add_opt('-exit0', 0, [], helpstr='force return of 0 on exit')
      vopts.add_opt('-prefix', 1, [],
                    helpstr='short for -cvar out_prefix')
      vopts.add_opt('-script_basic', 1, [],
                    helpstr='specify basic overview script name')
      vopts.add_opt('-script_driver', 1, [],
                    helpstr='specify data inspection script name')
      vopts.add_opt('-uvar', -2, [],
                    helpstr='set given user variable to given value(s)')
      vopts.add_opt('-verb', 1, [], helpstr='set the verbose level (def=1)')

      vopts.sort()

      return vopts

   def process_options(self):
      """return  1 on valid and exit
         return  0 on valid and continue
         return -1 on invalid
      """

      argv = sys.argv

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, do default processing
      if '-help' in argv:
         print g_help_string
         return 1

      if '-help_todo' in argv:
         print g_todo_str
         return 1

      if '-hist' in argv:
         print g_history
         return 1

      if '-show_uvar_dict' in argv:
         keys = g_uvar_dict.keys()
         keys.sort()
         for key in keys:
            print '   %-20s : %s' % (key, g_uvar_dict[key])
         return 1

      if '-show_cvar_defs' in argv:
         g_cvars_defs.show('control vars defaults', name=0)
         return 1

      if '-show_uvar_eg' in argv:
         g_eg_uvar.show('user vars example', name=0)
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         print g_version
         return 1

      # check this before any non-terminal return
      if '-exit0' in argv: self.cvars.exit0 = 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process non-chronological options, verb comes first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.cvars.verb = val

      # make list to process user vars by name
      ukeys = g_uvar_dict.keys()

      # convenience
      C = self.cvars

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script
      errs = 0
      for oind in range(len(uopts.olist)):
         opt = uopts.olist[oind]
         uvar = opt.name[1:]

         # check for anything to skip
         if opt.name == '-verb':        continue
         if opt.name == '-exit0':       continue

         # check uvar opts by name (process as strings)
         elif uvar in ukeys:
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return -1
            if self.uvars.set_var_with_defs(uvar, val, g_eg_uvar,
                        as_type=1, oname='uvars', verb=C.verb) < 0:
               errs += 1
               continue

         # uvar requires at least 2 parameters, name and value
         elif opt.name == '-uvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return -1
            if self.uvars.set_var_with_defs(val[0], val[1:], g_eg_uvar,
                        as_type=1, oname='uvars', verb=C.verb) < 0:
               errs += 1
               continue

         # uvar requires at least 2 parameters, name and value
         elif opt.name == '-cvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err: return -1
            if C.set_var_with_defs(val[0], val[1:], g_cvars_defs,
                        as_type=1, oname='cvars', verb=C.verb) < 0:
               errs += 1
               continue

         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err:
               errs +=1
               continue
            C.out_prefix = val

         elif opt.name == '-script_basic':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err:
               errs +=1
               continue
            C.scr_basic = val
         elif opt.name == '-script_driver':
            val, err = uopts.get_string_opt('', opt=opt)
            if val == None or err:
               errs +=1
               continue
            C.scr_drive = val
         else:
            print '** unknown option %d: %s' % (oind+1, opt.name)
            errs += 1

      if errs: return -1

      # ------------------------------------------------------------
      # apply any trailing logic

      # if out prefix is set, apply to any file names in g_outfile_cvars
      # (unless they have a full path)
      if C.is_not_empty('out_prefix'):
         for filevar in g_outfile_cvars:
            if C.is_not_empty(filevar):
               fname = C.val(filevar)
               # if not an absolute path, apply prefix
               if fname[0]!='/': C.set_var(filevar,'%s%s'%(C.out_prefix,fname))

      return 0

   def init_basics(self):
      """fill the self.dsets object with dataset instances
           - tcat_dset       : afni_name
           - xmat_regress    : afni_name
           - xmat_uncensored : afni_name
           - xmat_ad         : Afni1D (for xmat_regress)
           - xmat_ad_nocen   : Afni1D (for xmat_uncensored)
           - stats_dset      : afni_name
           - censor_dset     : afni_name (used iff censor limits passed)

         fill some of the basic vars:
           - subj, rm_trs, enorm_dset, motion_dset, outlier_dset,
             num_stim, xmat_regress, xmat_uncensored,
             stats_dset, final_view, final_anat, align_anat

         find any censor file (require some limit):
           - mot_limit, out_limit
      """

      if self.find_tcat_dset():    return 1
      if self.find_x_mat():        return 1

      if self.guess_subject_id():  return 1
      if self.guess_xmat_nocen():  return 1
      if self.guess_num_stim():    return 1
      if self.guess_rm_trs():      return 1
      if self.guess_stats_dset():  return 1
      if self.guess_sum_ideal():   return 1
      if self.guess_final_view():  return 1
      if self.guess_volreg_dset(): return 1
      if self.guess_enorm_dset():  return 1
      if self.guess_motion_dset(): return 1
      if self.guess_outlier_dset():return 1
      if self.guess_final_anat():  return 1
      if self.guess_align_anat():  return 1
      if self.guess_mask_dset():   return 1
      if self.guess_tsnr_dset():   return 1

      if self.find_censor_file():  return 1

      if self.cvars.verb > 2:
         print '-' * 75
         self.uvars.show('post-init uvars', name=0)
         self.cvars.show('post-init cvars', name=0)
         for datr in self.dsets.attributes(getall=1):
            obj = self.dsets.val(datr)
            if isinstance(obj, BASE.afni_name):
               print '-- dset   %-16s : %s' % (datr, obj.pv())
            elif isinstance(obj, LAD.Afni1D):
               print '-- Afni1D %-16s : %s' % (datr, obj.fname)
         print '-' * 75

      return 0

   def find_x_mat(self, verb=-1):
      """try to find the X-matrix files
         - set uvars.xmat_regress and dsets.xmat_regress, ad

         - return 0 on success

         - note any X*.1D file
         - look for contained CommandLine and -x1D option
           if found, see if appropriate file is in list
      """
      verb = self.cvars.verb

      # check if already set
      if self.uvars.is_not_empty('xmat_regress'):
         if self.cvars.verb > 3:
            print '-- already set: xmat_regress = %s' % self.uvars.xmat_regress
         # have cvar, check dsets
         if self.dsets.is_empty('xmat_ad'):
            return self.set_xmat_dset_from_name(self.uvars.xmat_regress)
         return 0

      # get a list of file candidates
      xfiles = glob.glob('X*.xmat.1D')
      if len(xfiles) == 0: xfiles = glob.glob('X*.1D')
      if len(xfiles) == 0: xfiles = glob.glob('*.xmat.1D')
      if len(xfiles) == 0:
         # error is fatal so print message
         print '** failed to match any x-matrix files'
         return 1

      # we have some list now, start looking through them
      use_xmat = ''
      for xfile in xfiles:
         ax = LAD.Afni1D(xfile, verb=0)    # try to load, no whining
         if verb > 2: print '-- testing xmat file %s' % xfile
         if ax == None: continue
         if not ax.ready: continue
         if not ax.command: continue
         if verb > 2: print '-- have command in xmat file'

         # now we have a command
         opt = '-x1D'
         copts = self.find_opt_and_params(ax.command, opt, 1)
         if len(copts) < 2:
            if verb > 2: print '-- no %s option in command, skipping file' % opt
            continue
         xf = copts[1]

         if xf not in xfiles:
            if verb > 1:
               print '-- have "-x1D %s", but no such file' % xf
               continue

         # we have a file to use
         use_xmat = xf
         break

      # failed to get one from '3dDeconvolve -x1D' option, so search for one
      if not use_xmat and len(xfiles) == 1:      use_xmat = xfiles[0]
      if not use_xmat and 'X.xmat.1D' in xfiles: use_xmat = 'X.xmat.1D'

      # last gasp, just use the first one in the list
      if not use_xmat: use_xmat = xfiles[0]

      self.uvars.xmat_regress = use_xmat
      return self.set_xmat_dset_from_name(use_xmat)

   def set_xmat_dset_from_name(self, xname, regress=1):
      """set dsets.xmat_regress, xmat_ad, xmat_uncensored (maybe)
         - if regress, this applies to xmat_regress, else xmat_uncensored
      """

      xb = BASE.afni_name(xname)
      if not xb.exist():
         print '** X-matrix dataset does not exist: %s' % xname
         return 1

      ax = LAD.Afni1D(xname, verb=0)
      if ax == None:
         print '** failed to read X-matrix file %s' % xname
         return 1
      if not ax.ready:
         print '** failed to load X-matrix file %s' % xname
         return 1

      # set corresponding dset items and then go after uncensored
      if regress:
         self.dsets.xmat_regress = xb
         self.dsets.xmat_ad = ax
         if self.cvars.verb > 1: print '++ guessing xmat_baisc = %s' % xname
      else:
         self.dsets.xmat_uncensored = xb
         self.dsets.xmat_ad_nocen = ax
         if self.cvars.verb > 1: print '++ guessing xmat_uncensored = %s'%xname

      return 0

   def guess_xmat_nocen(self):
      """set uvars,dsets.xmat_uncensored (if possible)"""

      # check if already set
      if self.uvars.is_not_empty('xmat_uncensored'):
         if self.cvars.verb > 3: print '-- already set: xmat_uncensored = %s' \
                                       % self.uvars.xmat_uncensored
         return 0

      # now try to set uncensored dset (cvar and dset)
      ax = self.dsets.val('xmat_ad')
      if ax == None:
         print '** no xmat_ad to set xmat_uncensored from'
         return 1

      copts = self.find_opt_and_params(ax.command, '-x1D_uncensored', 1, last=1)
      if len(copts) == 2:
         self.uvars.xmat_uncensored = copts[1]
         self.dsets.xmat_uncensored = BASE.afni_name(copts[1])
         if self.set_xmat_dset_from_name(self.uvars.xmat_uncensored, regress=0):
            return 0  

         if self.cvars.verb > 2: print '-- xmat_uncensored exists = %d' \
                                       % self.dsets.xmat_uncensored.exist()
      elif self.cvars.verb > 2: print '-- no uncensored X-matrix'

      return 0  # success

   def find_tcat_dset(self):
      """set self.dsets.tcat_dset

         look for pb00.$subj.r01.tcat+orig.HEAD

         return 0 on success, 1 on error
      """

      # check if already set
      if self.dsets.is_not_empty('tcat_dset'): return 0

      # get a list of file candidates

      pref = 'pb00.'
      suf  = '.r01.tcat+orig.HEAD'
      glist = UTIL.glob_list_minus_pref_suf(pref, suf)

      if len(glist) < 1:  # try again
         pref = 'pb00_'
         suf  = '_r01_tcat+orig.HEAD'
         glist = UTIL.glob_list_minus_pref_suf(pref, suf)

      if len(glist) < 1:
         print '** guess tcat: failed to determine subject ID from pb00 files'
         return 0  # non-fatal error
      if len(glist) > 1:
         print '** warning: guess tcat, non-unique tcat list: %s' % glist

      self.uvars.tcat_dset = pref + glist[0] + suf
      tcat = BASE.afni_name(self.uvars.tcat_dset)
      self.dsets.tcat_dset = tcat

      if self.cvars.verb > 1:
         print '-- tcat dset (exists=%d) = %s' % (tcat.exist(), tcat.pv())

      return 0

   def guess_subject_id(self):
      """just set uvars.sub_id

         guess from pb00.$subj.r01.tcat+orig.HEAD in dsets.tcat_dset
         else guess from enorm

         return 0 on success
      """

      # check if already set
      if self.uvars.is_not_empty('subj'):
         if self.cvars.verb > 3:
            print '-- already set: subj = %s' % self.uvars.subj
         return 0

      sid = ''

      if self.dsets.valid('tcat_dset'):
         if self.cvars.verb > 3: print '-- guessing subj from tcat dset...'
         prefix = self.dsets.tcat_dset.prefix
         if prefix[0:4] != 'pb00':
            print '** SID: odd tcat dset prefix, %s' % prefix
            return 1
         posn = prefix.find('r01')
         if posn < 7: # so at least 1 char for $subj
            print '** SID: odd tcat dset name, %s' % prefix
            return 1

         sid = prefix[5:posn-1]

      else:
         if self.cvars.verb > 3: print '-- guessing subj from enorm...'
         gstr = 'motion_*_enorm.1D'
         glist = glob.glob(gstr)
         glen = len(glist)
         if glen >= 1:
            mfile = glist[0]
            posn = mfile.find('_enorm.1D')
            sid = mfile[7:posn]

      if sid == '':
         print '** failed to guess subject ID'
         return 1

      if self.cvars.verb > 1: print '++ guessing subject id = %s' % sid

      self.uvars.subj = sid

      return 0

   def guess_num_stim(self):
      """set uvars.num_stim (from dsets.xmat_ad)
         backup plan: panic?  count stimuli/*?
         return 0 on success
      """

      # check if already set
      if self.uvars.is_not_empty('num_stim'):
         if self.cvars.verb > 3:
            print '-- already set: num_stim = %s' % self.uvars.num_stim
         return 0

      if self.dsets.is_empty('xmat_ad'):
         print '** no xmat_ad to set num_stim from'
         return 1

      self.uvars.num_stim = self.dsets.xmat_ad.nroi

      if self.cvars.verb > 1:
         print '++ guessing num_stim (from X) = %d' % self.uvars.num_stim

      return 0

   def guess_rm_trs(self):
      """set uvars.rm_trs (from dsets.tcat_dset)
         try using 3dinfo or 3dnotes on pb00 'tcat' dset
         return 0 on success
      """

      # check if already set
      if self.uvars.is_not_empty('rm_trs'):
         if self.cvars.verb > 3:
            print '-- already set: rm_trs = %s' % self.uvars.rm_trs
         return 0

      if self.dsets.is_empty('tcat_dset'):
         print '** guess rm_trs: no pb00 dset to detect removed TRs'
         return 0  # non-fatal?

      cmd = UTIL.get_last_history_command(self.dsets.tcat_dset.pv(), '3dTcat')
      intstr = UTIL.extract_subbrick_selection(cmd)
      intlist = UTIL.decode_1D_ints(intstr)

      if len(intlist) == 0:
         print '** guess rm_trs: failed to find sub-brick selector in 3dTcat'
         return 0  # non-fatal?

      self.uvars.rm_trs = intlist[0]

      if self.cvars.verb > 1:
         print '++ guessing rm_trs (from 3dTcat command) = %d' \
               % self.uvars.rm_trs

      return 0

   def guess_final_view(self):
      """set uvars.final_view (check against dsets.stats_dset)
         return 0 on success
      """

      # check if already set
      if self.uvars.is_not_empty('final_view'):
         if self.cvars.verb > 3:
            print '-- already set: final_view = %s' % self.uvars.final_view
         return 0

      if not self.dsets.is_empty('volreg_dset'):
         view = self.dsets.volreg_dset.view
      elif not self.dsets.is_empty('stats_dset'):
         view = self.dsets.stats_dset.view
      else:
         print '** no stats_dset to get final_view from'
         view = ''

      if len(view) != 5: # maybe surface, go after volreg explicitly for now
         vv = "+orig"
         glist = glob.glob('pb0*%s?r0*volreg%s.HEAD'%(self.uvars.subj, vv))
         if len(glist) == 0:
            vv = "+tlrc"
            glist = glob.glob('pb0*%s?r0*volreg%s.HEAD'%(self.uvars.subj, vv))
         if len(glist) > 0: view = vv

      if len(view) != 5:
         print "** could not find view in stats or volreg dsets"
         return 1

      self.uvars.final_view = view[1:]

      if self.cvars.verb > 1:
         print '++ guessing final_view (from stats_dset) = %s' \
               % self.uvars.final_view

      # do a basic test of the subject ID and view
      gform = 'pb*%s?r0*volreg+%s.HEAD' \
              % (self.uvars.subj, self.uvars.final_view)
      glist = glob.glob(gform)
      if len(glist) == 0:
         # try a more general form
         gform = 'pb*r*1*volreg+%s.HEAD' % self.uvars.final_view
         glist = glob.glob(gform)

      if len(glist) == 0:
         if self.cvars.verb > 0:
            print '** warning: failed to test sid/view with dset check on %s' \
                  % gform
      elif self.cvars.verb > 2:
         print '-- found volreg dset in view: %s' % glist[0]

      return 0

   def guess_final_anat(self):
      """set uvars.final_anat
         return 0 on sucess

         - look for anat_final*
         - look for *_al_keep+VIEW.HEAD
         - look for *_al_junk+VIEW.HEAD
         - look for *_al+VIEW.HEAD
         - if volreg+tlrc, could get final_anat from '3dAllineate -base'
           or 'adwarp -apar' from HISTORY
         - could go after mask_anat
      """

      # check if already set
      if self.uvars.is_not_empty('final_anat'):
         if self.cvars.verb > 3:
            print '-- already set: final_anat = %s' % self.uvars.final_anat
         return 0

      # go after known file
      gstr = 'anat_final.%s+%s.HEAD' % (self.uvars.subj, self.uvars.final_view)
      if os.path.isfile(gstr):
         self.uvars.final_anat = gstr
         self.dsets.final_anat = BASE.afni_name(gstr)
         return 0

      # else, anything close
      gstr = 'anat_final*+%s.HEAD' % self.uvars.final_view
      glist = glob.glob(gstr)
      glen = len(glist)
      if glen >= 1:     # shouldn't be more than 1, but to be safe
         self.uvars.final_anat = glist[0]
         self.dsets.final_anat = BASE.afni_name(self.uvars.final_anat)
         return 0

      # else, maybe there is a 3dAllineate command in volreg
      anat = self.guess_anat_from_volreg()
      if anat: 
         self.uvars.final_anat = anat
         self.dsets.final_anat = BASE.afni_name(self.uvars.final_anat)
         return 0

      # else, _al_keep
      gstr = '*_al_keep+%s.HEAD' % self.uvars.final_view
      glist = glob.glob(gstr)
      glen = len(glist)
      if glen >= 1:     # shouldn't be more than 1, but to be safe
         self.uvars.final_anat = glist[0]
         self.dsets.final_anat = BASE.afni_name(self.uvars.final_anat)
         return 0

      # else, _al_junk, use to get the rest of the file name
      gstr = '*_al_junk+*.HEAD'
      glist = glob.glob(gstr)
      glen = len(glist)
      if glen >= 1:
         name = glist[0]
         posn = name.find('_al_junk')
         fname = name[0:posn] + '+%s.HEAD' % self.uvars.final_view
         if self.cvars.verb > 2:
            print '-- final anat: found %s, trying %s...' % (name, fname)
         if os.path.isfile(fname):
            self.uvars.final_anat = fname
            self.dsets.final_anat = BASE.afni_name(self.uvars.final_anat)
            return 0

      # else, _al, use to get the rest of the file name
      gstr = '*_al+*.HEAD'
      glist = glob.glob(gstr)
      glen = len(glist)
      if glen == 1:
         name = glist[0]
         posn = name.find('_al')
         fname = '%s*.HEAD' % name[0:posn]
         if self.cvars.verb > 2:
            print '++ found potential anat %s, looking for %s' % (name, fname)
         glist = glob.glob(fname)
         glen = len(glist)
         if glen == 1:
            # now check volreg dset for '3dAllineate -base ANAT'
            # if found, use that anat, else use this one (glist[0])
            anat = self.guess_anat_from_volreg()
            if not anat: anat = glist[0]
            self.uvars.final_anat = anat
            self.dsets.final_anat = BASE.afni_name(self.uvars.final_anat)
            return 0

      if glen > 1:
         for gname in glist:
            if gname.find(self.uvars.final_view) > 0:
               self.uvars.final_anat = gname
               self.dsets.final_anat = BASE.afni_name(self.uvars.final_anat)
               return 0

      # else, go after mask_anat
      # find -input for (command line "-prefix rm.resam.anat")
      # rcr - todo

      print '** failed to guess final_anat (continuing)'

      return 0

   def guess_align_anat(self):
      """set uvars.align_anat
         return 0 on sucess

         This variable is non-vital, so return 0 on anything but fatal error.

         - look for *_al_keep+VIEW.HEAD
         - look for *_al_junk+VIEW.HEAD
         - look for *_al+VIEW.HEAD
      """

      # check if already set
      if self.uvars.is_not_empty('align_anat'):
         if self.cvars.verb > 3:
            print '-- already set: align_anat = %s' % self.uvars.align_anat
         return 0

      # go after known files (view should be +orig for anat<-->EPI alignment)
      for suff in ['junk', 'keep']:
            gstr = '%s_al_%s+orig.HEAD' % (self.uvars.subj, suff)
            if os.path.isfile(gstr):
               self.uvars.align_anat = gstr
               return 0

      # else try using wildcards
      glist = glob.glob('*_al_*+orig.HEAD')
      if self.cvars.verb > 2:
         print '-- have %d align files via wildcard: %s' % (len(glist), glist)
      for file in glist:
         if os.path.isfile(file):
            self.uvars.align_anat = gstr
            return 0

      print '** failed to guess align_anat (continuing)'

      return 0

   def guess_anat_from_volreg(self):
      """look for 3dAllineate -base DSET, returning DSET"""

      if self.uvars.is_empty('volreg_dset'): return ''

      vset = self.uvars.volreg_dset

      # go after 3dAllineate -base ANAT
      cmd = UTIL.get_last_history_command(self.dsets.volreg_dset.pv(),
                                          '3dAllineate')
      if cmd:   
         copts = self.find_opt_and_params(cmd, '-base', 1)
         if len(copts) == 2:
            anat = copts[1]
            aset = BASE.afni_name(anat)
            if aset.exist(): anat = aset.pv() + '.HEAD'
            if self.cvars.verb > 3: print '== GAFVR %s, %s' % (anat, copts[1])
            return anat

      # go after adwarp -apar ANAT ...

      return ''

   def guess_enorm_dset(self):
      """set uvars.enorm_dset"""

      # check if already set
      if self.uvars.is_not_empty('enorm_dset'):
         if self.cvars.verb > 3:
            print '-- already set: enorm_dset = %s' % self.uvars.enorm_dset

      gstr = 'motion_%s_enorm.1D' % self.uvars.subj
      if os.path.isfile(gstr):
         self.uvars.enorm_dset = gstr
         return 0

      # else go after anything of the sort
      glist = glob.glob(gstr)
      if len(glist) == 0: glist = glob.glob('mot*_enorm.1D')
      if len(glist) == 0: glist = glob.glob('*_enorm.1D')
      if len(glist) > 0: # grab first, for lack of anything better
         self.uvars.enorm_dset = glist[0]
         return 0

      print '** failed to find motion enorm dset'

      return 1

   def guess_motion_dset(self):
      """set uvars.motion_dset"""

      # check if already set
      if self.uvars.is_not_empty('motion_dset'):
         if self.cvars.verb > 3:
            print '-- already set: motion_dset = %s' % self.uvars.motion_dset

      gstr = 'dfile_rall.1D'
      if not os.path.isfile(gstr): gstr = 'dfile.rall.1D'

      if os.path.isfile(gstr):
         self.uvars.motion_dset = gstr
         return 0

      # else go 1D or text files with motion in the name
      glist = glob.glob('*motion*.1D')
      if len(glist) == 0: glist = glob.glob('*motion*.txt')
      if len(glist) > 1:
         self.uvars.motion_dset = glist[0]
         if self.cvars.verb > 1:
            print '-- guessing motion_dset = %s' % self.uvars.motion_dset
         return 0

      print '** failed to find motion parameter dset, continuing...'

      return 0  # not fatal

   def guess_outlier_dset(self):
      """set uvars.outlier_dset"""

      # check if already set
      if self.uvars.is_not_empty('outlier_dset'):
         if self.cvars.verb > 3:
            print '-- already set: outlier_dset = %s' % self.uvars.outlier_dset

      gstr = 'outcount_rall.1D'
      if not os.path.isfile(gstr): gstr = 'outcount.rall.1D'

      if os.path.isfile(gstr):
         self.uvars.outlier_dset = gstr
         return 0

      # else go after anything of the sort
      glist = glob.glob('outcount*rall*')
      if len(glist) == 1:
         self.uvars.outlier_dset = glist[0]
         return 0

      print '** failed to find outlier fraction dset'

      return 1

   def guess_mask_dset(self):
      """set uvars.mask_dset"""

      # check if already set
      if self.uvars.is_not_empty('mask_dset'):
         if self.cvars.verb > 3:
            print '-- already set: mask_dset = %s' % self.uvars.mask_dset

      gstr = 'full_mask?%s+%s.HEAD' % (self.uvars.subj, self.uvars.final_view)
      glist = glob.glob(gstr)
      if len(glist) == 0:
         print '** failed to find mask dset, continuing...'
         return 0 # failure is not terminal

      self.uvars.mask_dset = glist[0]
      self.dsets.mask_dset = BASE.afni_name(self.uvars.mask_dset)

      return 0 # not failure

   def guess_tsnr_dset(self):
      """set uvars.tsnr_dset"""

      # check if already set
      if self.uvars.is_not_empty('tsnr_dset'):
         if self.cvars.verb > 3:
            print '-- already set: tsnr_dset = %s' % self.uvars.tsnr_dset

      gstr = 'TSNR?%s+%s.HEAD' % (self.uvars.subj, self.uvars.final_view)
      glist = glob.glob(gstr)
      if len(glist) == 0:
         gstr = '*[tT][sS][nN][rR]*'
         glist = glob.glob(gstr)
      if len(glist) == 0:
         print '** failed to find tsnr dset, continuing...'
         return 0 # failure is not terminal

      self.uvars.tsnr_dset = glist[0]
      self.dsets.tsnr_dset = BASE.afni_name(self.uvars.tsnr_dset)

      return 0

   def guess_volreg_dset(self):
      """set uvars.volreg_dset"""

      # check if already set
      if self.uvars.is_not_empty('volreg_dset'):
         if self.cvars.verb > 3:
            print '-- already set: volreg_dset = %s' % self.uvars.volreg_dset

      glist = self.glob_slist_per_view(                                 \
                ['pb??.*.r01.volreg+%s.HEAD', 'pb*r001*volreg+%s.HEAD', \
                 'pb*r01*volreg+%s.HEAD'],                              \
                ['tlrc', 'orig'])

      if len(glist) >= 1:
         glist.sort()   # just to be sure
         self.uvars.volreg_dset = glist[0]
         self.dsets.volreg_dset = BASE.afni_name(glist[0])
         return 0

      print '** failed to find volreg dset, continuing...'

      return 0 # not failure

   def glob_slist_per_view(self, slist, vlist):
      """for each view, search for any files in slist"""
      for v in vlist:
         for s in slist:
            glist = glob.glob(s % v)
            if len(glist) > 0: return glist
      return []

   def guess_sum_ideal(self):
      """set uvars.sum_ideal

         return 0 on success
      """

      # check if already set
      if self.uvars.is_not_empty('sum_ideal'):
         if self.cvars.verb > 3:
            print '-- already set: sum_ideal = %s' % self.uvars.sum_ideal
         return 0

      gstr = 'sum_ideal.1D'
      if os.path.isfile(gstr):
         self.uvars.sum_ideal = gstr
         return 0

      gstr = '*[sS][uU][mM]*.1D'
      glist = glob.glob(gstr)
      if len(glist) >= 1:
         if self.cvars.verb > 3:
            print '-- trying to set sum_ideal from list : %s' % glist
         self.uvars.sum_ideal = glist[0]
         return 0

      print '** failed to guess sum_ideal file, continuing...'

      return 0  # non-fatal

   def guess_stats_dset(self):
      """set uvars.stats_dset (check against dsets.xmat_ad)

         ** try to use a _REML dset if a 3dD one is not found

         return 0 on success
      """

      # check if already set
      if self.uvars.is_not_empty('stats_dset'):
         if self.cvars.verb > 3:
            print '-- already set: stats_dset = %s' % self.uvars.stats_dset
         # have cvar, check dset
         if self.dsets.is_empty('stats_dset'):
            return self.set_stats_dset_from_name(self.uvars.stats_dset)
         return 0

      if self.dsets.is_empty('xmat_ad'):
         print '** no xmat_ad to set stats_dset from'
         return 1

      ax = self.dsets.xmat_ad

      # form a glob form, either from ax.command or just 'stats.*.HEAD'
      opt = '-bucket'
      copts = self.find_opt_and_params(ax.command, opt, 1)
      if len(copts) == 2:
         # check for AFNI, NIML or NIfTI datasets
         posn = copts[1].find('.HEAD')
         if posn < 0: posn = copts[1].find('.niml.dset')
         if posn < 0: posn = copts[1].find('.nii')

         # if not found, assume AFNI
         if posn > 0: gform = copts[1]
         else:        gform = '%s*HEAD' % copts[1]
      else:
         if self.cvars.verb > 2: print '-- no %s option in command ...' % opt
         gform = 'stats*HEAD'

      # now find all datasets, but remove expected REMLvar+VIEW ones
      dlist = glob.glob(gform)
      dlist = [d for d in dlist if d.find('_REMLvar+') < 0]

      if len(dlist) < 1:
         print '** failed to guess at any stats dset, globstr = "%s"' % gform
         print '   (X-matrix file "%s" may not apply)' % ax.fname
         return 1
      if len(dlist) == 1:
         sset = dlist[0]
      else:     # must pare down the list
         if self.cvars.verb > 3:
            print '-- found %d potential stats dsets: %s' % (len(dlist), dlist)
         sset = ''
         # take the first one without any '_REML', else take the first one
         for dfile in dlist:
            if not dfile.find('_REML'):
               sset = dfile
         if not sset: sset = dlist[0]

      self.uvars.stats_dset = sset

      if self.cvars.verb > 1:
         print '++ guessing stats_dset (from X) = %s' % self.uvars.stats_dset

      return self.set_stats_dset_from_name(self.uvars.stats_dset)

   def set_stats_dset_from_name(self, sname):
      """set dsets.stats_dset"""

      sb = BASE.afni_name(sname)
      if not sb.exist():
         print '** stats dataset not found: %s' % xname
         return 1

      # set corresponding dset items and then go after uncensored
      self.dsets.stats_dset = sb
      if self.cvars.verb > 1:
         print '-- setting stats_dset = %s' % (sb.pv() + '.HEAD')

      return 0

   def find_censor_file(self):
      """try to set dsets.censor_dset

         compare mot,out_limit against any -censor option in 3dD

         if censor
            - set dsets.censor_dset
            - warn if no mot/cen limit (and set to defaults)
      """

      # if empty, init the limits
      if self.uvars.is_empty('mot_limit'): self.uvars.mot_limit = 0.3
      if self.uvars.is_empty('out_limit'): self.uvars.out_limit = 0.1

      # check if censor dset already set
      if self.dsets.is_not_empty('censor_dset'):
         if self.cvars.verb > 3:
            print '-- already set: censor_dset = %s' \
                  % self.dsets.censor_dset.prefix
         return 0

      # if we don't have one, try to figure it out
      if self.uvars.is_empty('censor_dset'):
         # check for -censor option in 3dD command
         ax = self.dsets.val('xmat_ad')
         if ax == None:
            if self.cvars.verb: print '** no xmat_ad to check censoring with'
            return 1

         opt = '-censor'
         ax = self.dsets.xmat_ad
         copts = self.find_opt_and_params(ax.command, opt, 1)

         if len(copts) != 2: # no censoring
            if self.cvars.verb > 1: print '-- no censoring...'
            return 0

         cset = copts[1]
      else: # we have a dset
         cset = self.uvars.censor_dset

      # now apply
      if not os.path.isfile(cset):
         print '** warning: have censoring in X-matrix, but no censor file'
         return 0
      self.uvars.censor_dset = cset
      self.dsets.censor_dset = BASE.afni_name(cset)
      if self.cvars.verb > 1:
         print '-- setting censor_dset = %s' % cset
      
      # we should have some limit option
      if self.uvars.is_empty('mot_limit') and \
         self.uvars.is_empty('out_limit'):
         print '** have censor file %s, but no passed -mot/out_limit' \
               % copts[1]

      return 0

   def write_scripts(self):
      scr_basic = self.cvars.val('scr_basic')
      if scr_basic:
         if self.make_basic_script(): return 1
         # write to executable text file
         UTIL.write_text_to_file(scr_basic, self.text_basic, exe=1)
         print '++ writing ss review basic:          %s' % scr_basic

      scr_drive = self.cvars.val('scr_drive')
      if scr_drive:
         if self.make_drive_script(): return 1
         # write to executable text file
         UTIL.write_text_to_file(scr_drive, self.text_drive, exe=1)
         print '++ writing ss review driver:         %s' % scr_drive

      cmds_drive = self.cvars.val('cmds_drive')
      if cmds_drive:
         UTIL.write_text_to_file(cmds_drive, self.commands_drive, exe=1)
         print '++ writing ss review drive commands: %s' % cmds_drive

      return 0

   def make_basic_script(self):
      self.text_basic = ''
      if self.basic_init(): return 1
      if self.basic_overview(): return 1

      # most of script is just raw text
      self.text_basic += g_censor_results_str
      if self.uvars.is_not_empty('mask_dset') and \
         self.uvars.is_not_empty('tsnr_dset'):
         self.text_basic += g_basic_tsnr_str
      self.text_basic += g_basic_finish_str

      return 0

   def basic_overview(self):
      anat = self.uvars.val('final_anat')
      if anat != None:
         astr = 'echo "final anatomy dset        : $final_anat"\n'
      else: astr = ''

      sset = self.uvars.val('stats_dset')
      if sset != None:
         sstr = 'echo "final stats dset          : $stats_dset"\n' \
                'echo "final voxel resolution    : '               \
                                        '`3dinfo -ad3 $stats_dset`"\n'
      else: sstr = ''

      txt = g_overview_str + astr + sstr + 'echo ""\n' + g_mot_n_trs_str

      txt += \
        '# ------------------------------------------------------------\n'   \
        '# make non-basline X-matrix, if one is not already here\n'          \
        'set xstim = %s\n'                                                   \
        'if ( ! -f $xstim ) then\n'                                          \
        '   set reg_cols = `1d_tool.py -infile $xmat -show_indices_interest`\n'\
        '   1d_tool.py -infile $xmat"[$reg_cols]" -overwrite -write $xstim\n'  \
        'endif\n' % self.cvars.xstim

      self.text_basic += txt

   def basic_init(self):
      """write header and set applied variables"""
      self.text_basic += g_basic_header_str + g_no_oblique_warn_str

      # now append variable assignments
      txt  = '# ' + '-'*70 + '\n' + \
             '# main variables regarding this single subject analysis\n'

      format = 'set %-16s = %s\n'
      uvars  = self.uvars
      errs   = 0
      # some cases with matching var names
      # rcr - add final_anat
      for var in ['subj', 'rm_trs', 'num_stim', 'mot_limit', 'out_limit',
                  'final_view']:
         if uvars.valid(var):
            txt += format % (var,self.uvars.val(var))
         elif var in ['rm_trs']: # non-fatal
            print '** warning: basic script, missing variable %s' % var
         else:
            print '** basic script: missing variable %s' % var
            errs += 1

      if self.dsets.is_not_empty('censor_dset'):
         txt += format % ('was_censored', '1')
      else:
         txt += format % ('was_censored', '0')

      txt += '\n'

      var = 'tcat_dset'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))
      else: # non-fatal
         print '** warning: basic script: missing variable %s' % var

      var = 'enorm_dset'
      if uvars.is_not_empty(var):
         txt += format % ('enorm_dset', uvars.val(var))
      else:
         print '** basic script: missing variable %s' % var
         errs += 1

      var = 'motion_dset'
      if uvars.is_not_empty(var):
         txt += format % ('motion_dset', uvars.val(var))

      var = 'outlier_dset'
      if uvars.is_not_empty(var):
         txt += format % ('outlier_dset', uvars.val(var))
      else:
         print '** basic script: missing variable %s' % var
         errs += 1

      var = 'xmat_regress'
      if uvars.is_not_empty(var):
         txt += format % ('xmat_regress', uvars.val(var))
      else:
         print '** basic script: missing variable %s' % var
         errs += 1

      var = 'stats_dset'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))
      else:
         print '** basic script: missing variable %s' % var
         errs += 1

      var = 'censor_dset'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))

      var = 'xmat_uncensored'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))
      # else: okay - not required

      var = 'final_anat'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))
      # else: okay - not required

      var = 'mask_dset'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))
      # not required

      var = 'tsnr_dset'
      if self.uvars.is_not_empty(var):
         txt += format % (var, self.uvars.val(var))
      # not required

      if errs > 0: return 1

      self.text_basic += txt + '\n'

      return 0

   def make_drive_script(self):
      # init script and commands-only text
      self.text_drive = ''
      self.commands_drive = 'tcsh -ef %s\n' % self.cvars.val('scr_basic')

      if self.drive_init(): return 1
      if self.drive_motion(): return 1
      # if self.drive_tsnr(): return 1          # not worth including?
      if self.drive_align_check(): return 1
      if self.drive_regress_warnings(): return 1
      if self.drive_regress_xmatrix(): return 1
      if self.drive_view_stats(): return 1

      return 0

   def drive_init(self):
      txt = g_drive_init_str + g_no_oblique_warn_str

      scr = self.cvars.val('scr_basic')

      txt += \
       '# ------------------------------------------------------------\n'   \
       '# if the expected "basic" script is here, run it\n'                 \
       '\n'                                                                 \
       'if ( -f %s ) then\n'                                                \
       '   echo ------------------- %s --------------------\n'              \
       '   tcsh -ef %s\n'                                                   \
       '   echo ---------------------------------------------------------\n'\
       '\n'                                                                 \
       '   prompt_user -pause "                      \\\n'                  \
       '      review output from %s    \\\n'                                \
       '      (in terminal window) for anything that \\\n'                  \
       '      looks unreasonable                     \\\n'                  \
       '                                             \\\n'                  \
       '      --- click OK when finished ---         \\\n'                  \
       '      "\n'                                                          \
       '   echo ""\n'                                                       \
       'else\n'                                                             \
       '   echo ""\n'                                                       \
       '   echo "*** missing %s script ***"\n'                              \
       '   echo ""\n'                                                       \
       'endif\n'                                                            \
       '\n'                                                                 \
       '# ------------------------------------------------------------\n'   \
       '# possibly consider running the @epi_review script here\n\n\n'      \
       % (scr, scr, scr, scr, scr)

      self.text_drive += txt

   def drive_view_stats(self):

      # check that dsets are okay before using them
      errs = 0
      emesg = 'cannot drive view_stats, skipping...'
      if self.check_for_dset('stats_dset', emesg): errs += 1
      if self.check_for_dset('mask_dset', emesg): errs += 1
      if errs: return 0

      sset = self.dsets.val('stats_dset')
      mset = self.dsets.val('mask_dset')

      txt = 'echo ' + UTIL.section_divider('view stats results',
                                           maxlen=60, hchar='-') + '\n\n'

      s1   = 'set pp = ( `3dBrickStat -slow -percentile 90 1 90 \\\n' \
             '            -mask %s %s"[0]"` )\n' % (mset.pv(), sset.pv())

      s2   = 'set thresh = $pp[2]\n'                                    \
             'echo -- thresholding F-stat at $thresh\n'

      aset = self.dsets.val('final_anat')
      if not self.check_for_dset('final_anat', ''):
         s3 = '     -com "SWITCH_UNDERLAY %s" \\\n'%aset.prefix
      else: s3 = ''

      s4  = \
          '# locate peak coords of biggest cluster and jump there\n'    \
          'set maxcoords = ( `3dclust -1thresh $thresh -dxyz=1 1 2 \\\n'\
          '    %s"[0]" | & awk \'/^ / {print $14, $15, $16}\' | head -n 1` )\n'\
          'echo -- jumping to max coords: $maxcoords\n'                 \
          % sset.pv()

      txt += '# get 90 percentile for thresholding in afni GUI\n'       \
             '%s'                                                       \
             '%s'                                                       \
             '\n'                                                       \
             '%s\n' % (s1, s2, s4)

      ac   = 'afni -com "OPEN_WINDOW A.axialimage"     \\\n'            \
             '     -com "OPEN_WINDOW A.sagittalimage"  \\\n'            \
             '%s'                                                       \
             '     -com "SWITCH_OVERLAY %s"   \\\n'                     \
             '     -com "SET_SUBBRICKS A 0 0 0"        \\\n'            \
             '     -com "SET_THRESHNEW A $thresh"      \\\n'            \
             '     -com "SET_DICOM_XYZ A $maxcoords"\n'                 \
             '\n' % (s3, sset.prefix)
      
      txt += '# start afni with stats thresholding at peak location\n'  \
             + ac

      txt += '\n'                                                      \
             'prompt_user -pause "                                 \\\n' \
             '   review: peruse statistical retsults               \\\n' \
             '      - thresholding Full-F at masked 90 percentile  \\\n' \
             '        (thresh = $thresh)                           \\\n' \
             '                                                     \\\n' \
             '   --- close afni and click OK when finished ---     \\\n' \
             '   "\n'                                                  \

      self.commands_drive += s1 + s2 + s4 + ac

      self.text_drive += txt + '\n\n'

      return 0

   def drive_regress_xmatrix(self):

      # can we do this?
      xset = self.uvars.val('xmat_regress')
      xstim = self.cvars.val('xstim')

      if not os.path.isfile(xset):
         print '** missing X-matrix, cannot drive regress_xmatrix'
         return 1

      txt = 'echo ' + UTIL.section_divider('X-matrix',
                                           maxlen=60, hchar='-') + '\n\n'

      txt += '# plot X-matrix, but without baseline and motion\n'       \
             'if ( -f %s ) then\n'                                      \
             '   1dplot -sepscl %s &\n'                                 \
             'else\n'                                                   \
             '   echo --- no file %s ---   \n'                          \
             'endif\n'                                                  \
             '\n' % (xstim, xstim, xstim)

      ifile = self.uvars.val('sum_ideal')
      if ifile:
         txt += '# also plot the sum of those same regressors\n'        \
                'if ( -f %s ) then\n'                                   \
                '   1dplot %s &\n'                                      \
                'else\n'                                                \
                '   echo --- no file %s ---   \n'                       \
                'endif\n'                                               \
                '\n' % (ifile, ifile, ifile)

      txt += '\n'                                                        \
             'prompt_user -pause "                                 \\\n' \
             '   review: non-baseline regressors in X-matrix       \\\n' \
             '      - %s shows non-baseline regressors \\\n'             \
             '      - sum_ideal.1D shows their sum                 \\\n' \
             '                                                     \\\n' \
             '   --- close plots and click OK when finished ---    \\\n' \
             '   "\n' % xstim

      self.commands_drive += '1dplot -sepscl %s &\n' % xstim
      if ifile: self.commands_drive += '1dplot %s &\n' % ifile

      self.text_drive += txt + '\n\n'

      return 0

   def drive_regress_warnings(self):

      # can we do this?
      xset = self.uvars.val('xmat_regress')

      if not os.path.isfile(xset):
         print '** missing X-matrix, cannot drive regress_warnings'
         return 1

      txt = 'echo ' + UTIL.section_divider('regession warnings',
                                           maxlen=60, hchar='-') + '\n\n'

      txt += '# if 3dDeconvolve made an error/warnings file, show it\n' \
             'if ( -f 3dDeconvolve.err ) then\n'                        \
             '   echo ------------- 3dDeconvolve.err -------------\n'   \
             '   cat 3dDeconvolve.err\n'                                \
             '   echo --------------------------------------------\n'   \
             'else\n'                                                   \
             '   echo --- no 3dDeconvolve.err warnings file ---   \n'   \
             'endif\n'                                                  \
             '\n'                                                       \
             'echo ""\n'                                                \
             '\n'                                                       \
             '# show any timing_tool.py warnings about TENTs        \n' \
             'if ( -f out.TENT_warn.txt ) then\n'                       \
             '   echo ------------ out.TENT_warn.txt -------------\n'   \
             '   cat out.TENT_warn.txt\n'                               \
             '   echo --------------------------------------------\n'   \
             'else\n'                                                   \
             '   echo --- no out.TENT_warn.txt warnings file ---  \n'   \
             'endif\n'                                                  \
             '\n'                                                       \
             'echo ""\n'                                                \
             '\n'                                                       \
             '# show any pairwise correlation warnings from X-matrix\n' \
             'echo ----------- correlation warnings -----------\n'      \
             '1d_tool.py -show_cormat_warnings -infile %s\n'            \
             'echo --------------------------------------------\n'      \
             '\n' % xset

      wfile = 'out.pre_ss_warn.txt'
      txt += '# if there are any pre-steady state warnings, show them\n'\
             'if ( -f %s && ! -z %s ) then\n'                           \
             '   echo --------- pre-steady state warnings --------\n'   \
             '   cat %s\n'                                              \
             '   echo --------------------------------------------\n'   \
             'endif\n'                                                  \
             '\n' % (wfile, wfile, wfile)

      txt += '\n'                                                        \
             'prompt_user -pause "                                 \\\n' \
             '   review: check for regression warnings             \\\n' \
             '      - review any warnings from 3dDeconvolve        \\\n' \
             '      - review any TENT warnings from timing_tool.py \\\n' \
             '      - review any X-matrix warnings from 1d_tool.py \\\n' \
             '                                                     \\\n' \
             '   --- click OK when finished ---                    \\\n' \
             '   "\n'                                                    \

      self.commands_drive += \
             'cat 3dDeconvolve.err\n'                                    \
             '1d_tool.py -show_cormat_warnings -infile %s\n' % xset

      self.text_drive += txt + '\n\n'

      return 0

   def check_for_file(self, varname, mesg):
      """check for existence of file
         if not, print mesg
         if no mesg, print nothing

         e.g. if check_for_file('X.xmat.1D', 'failed basic init'): errs += 1
      """
      fname = self.uvars.val(varname)
      if not fname:
         if mesg: print '** no %s, %s' % (varname, mesg)
         return 1
      if not os.path.isfile(fname):
         if mesg: print '** missing %s %s, %s' % (varname, fname, mesg)
         return 1
      return 0

   def check_for_dset(self, dname, mesg):
      """akin to check_for_file, but use dset.exists instead of os.path.isfile
         and get the filename from self.dsets
      """
      dset = self.dsets.val(dname)
      if not dset:
         if mesg: print '** no %s dset, %s' % (dname, mesg)
         return 1
      if not dset.exist():
         if mesg: print '** missing %s %s, %s' % (dname, dset, mesg)
         return 1
      return 0

   def drive_align_check(self):

      errs = 0
      emesg = 'skipping align check...'
      if self.check_for_file('final_anat', emesg): errs += 1
      if self.check_for_file('volreg_dset', emesg): errs += 1
      if errs: return 0  # continue

      # so we are good to go with these...
      aset = self.uvars.val('final_anat')
      vset = self.uvars.val('volreg_dset')

      apre = self.dsets.final_anat.prefix
      aset = self.dsets.final_anat.pve()
      vpre = self.dsets.volreg_dset.prefix
      vset = self.dsets.volreg_dset.pve()

      txt = 'echo ' + UTIL.section_divider('anat/EPI alignment check',
                                           maxlen=60, hchar='-') + '\n\n'

      txt += '# start afni with anat and volreg datasets only\n'     \
             'afni %s %s &\n\n' % (aset, vset)

      txt += '\n'                                                      \
             'prompt_user -pause "                                     \\\n' \
             '   review: check alignment between anat and EPI          \\\n' \
             '      - set underlay to anat                             \\\n' \
             '      - set overlay to EPI                               \\\n' \
             '      - look at gross alignment, maybe with thresholding \\\n' \
             "      - turn off 'See Overlay'                           \\\n" \
             "      - use 'u' key in image windows to toggle underlay  \\\n" \
             "        dataset between anat and EPI                     \\\n" \
             '          - CSF should be dark in anat and bright in EPI \\\n' \
             '      - follow ventricles and gyral patterns             \\\n' \
             '                                                         \\\n' \
             '   --- close afni and click OK when finished ---         \\\n' \
             '   "\n'                                                        \

      self.commands_drive += 'afni %s %s &\n' % (aset, vset)

      self.text_drive += txt + '\n\n'

   def drive_tsnr(self):

      # do the dsets exist?
      if self.uvars.is_empty('tsnr_dset') or self.uvars.is_empty('mask_dset'):
         print '** skipping drive_tsnr'
         return 0

      tset = self.dsets.tsnr_dset
      mset = self.dsets.mask_dset

      errs = 0
      if not tset.exist():
         print '** missing TSNR dataset %s' % tset
         errs += 1
      if not mset.exist():
         print '** missing mask dataset %s' % mset
         errs += 1
      if errs:
         print '** skipping TSNR review block'
         return 0

      txt = 'echo ' + UTIL.section_divider('temporal signal to noise',
                                           maxlen=60, hchar='-') + '\n\n'

      txt += '3dhistog -mask %s -prefix tsnr.histog %s\n'%(mset.pv(),tset.pv())
      txt += '1dplot -sepscl -x tsnr.histog.1D"[0]" tsnr.histog.1D"[1]" &\n'

      txt += '\n'                                                    \
             'prompt_user -pause "                             \\\n' \
             '   review: histogram of masked temporal          \\\n' \
             '           signal to noise volume                \\\n' \
             '                                                 \\\n' \
             '   --- close plot and click OK when finished --- \\\n' \
             '   "\n'                                                \

      self.commands_drive += \
         '3dhistog -mask %s -prefix tsnr.histog %s\n'                    \
         '1dplot -sepscl -x tsnr.histog.1D"[0]" tsnr.histog.1D"[1]" &\n' \
         %(mset.pv(),tset.pv())

      self.text_drive += txt + '\n\n'

      return 0

   def drive_motion(self):

      # note the enorm and outcount files
      efile = self.uvars.val('enorm_dset')
      ofile = self.uvars.val('outlier_dset')

      errs = 0
      if not os.path.isfile(efile):
         print '** missing motion file %s' % efile
         errs += 1
      if not os.path.isfile(ofile):
         print '** missing outlier file %s' % ofile
         errs += 1
      if errs: return 1

      # maybe include -censor option
      cstr = ''
      cpad1 = ''
      cpad2 = ''
      if self.uvars.is_not_empty('censor_dset'):
         cfile = self.uvars.val('censor_dset')
         if os.path.isfile(cfile):
            cstr  = '-censor_RGB green -censor %s ' % cfile
            cpad1 = '       %s \\\n' % cstr
            cpad2 = '%s \\\n       ' % cstr


      txt = 'echo ' + UTIL.section_divider('outliers and motion',
                                           maxlen=60, hchar='-') + '\n\n'

      txt += '1dplot -wintitle "motion, outliers" -ynames Mot OFrac \\\n' \
             '%s'                                                         \
             '       -sepscl %s %s &\n' % (cpad1, efile, ofile)

      # get total TRs from any uncensored X-matrix
      if self.dsets.is_not_empty('xmat_ad_nocen'):
         xmat = self.dsets.xmat_ad_nocen
      else: xmat = self.dsets.xmat_ad
      nt = xmat.nt
      mlimit = self.uvars.mot_limit
      olimit = self.uvars.out_limit

      colorstr = '(colored TRs are censored):'
      if self.dsets.is_empty('censor_dset'):
         colorstr = ':' + ' ' * len(colorstr)

      txt += '1dplot -one %s%s "1D: %d@%g" &\n' % (cpad2, ofile, nt, olimit)
      txt += '1dplot -one %s%s "1D: %d@%g" &\n' % (cpad2, efile, nt, mlimit)

      txt += '\n'                                                       \
             'prompt_user -pause "                              \\\n'   \
             '   review plots %s       \\\n'                            \
             '     - outliers and motion (plotted together)     \\\n'   \
             '     - outliers with limit %g                    \\\n' \
             '     - motion with limit %g                      \\\n' \
             '                                                  \\\n'   \
             '   --- close plots and click OK when finished --- \\\n'   \
             '   "\n'                                                   \
             'echo ""\n\n\n' % (colorstr, olimit, mlimit)

      self.commands_drive += \
         '1dplot -wintitle "motion, outliers" -ynames Mot OFrac \\\n' \
         '       -sepscl %s%s %s &\n' % (cstr, efile, ofile)
      self.commands_drive += '1dplot -one %s%s "1D: %d@%g" &\n' \
                             % (cstr, ofile, nt, olimit)
      self.commands_drive += '1dplot -one %s%s "1D: %d@%g" &\n' \
                             % (cstr, efile, nt, mlimit)

      self.text_drive += txt

   def find_opt_and_params(self, text, opt, nopt=0, last=0):
      """given some text, return the option with that text, as well as
         the following 'nopt' parameters (truncated list if not found)
       
         if last: return right-most position
      """
      tlist = text.split()

      if not opt in tlist: return []

      if last: tlist.reverse()

      tind = tlist.index(opt)

      if last:
         tlist.reverse()
         tind = len(tlist) - 1 - tind

      return tlist[tind:tind+1+nopt]

def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # valid and exit
   if rv < 0: # error and exit
      print '** failed to process options...'
      if me.cvars.exit0: return 0
      return 1

   if me.init_basics():
      print '** failed to init basics...'
      if me.cvars.exit0: return 0
      return 1

   if me.write_scripts():
      if me.cvars.exit0: return 0
      return 1

   return 0

if __name__ == '__main__':
   sys.exit(main())


