#!/usr/bin/env tcsh

@global_parse `basename $0` "$*" ; if ($status) exit 0

# ===========================================================================
# Run a simple afni_proc.py resting state analysis, possibly to get QC output.
# This version is for processing multi-echo data, combining echoes using the
# optimal combination.
#
# To keep it simple, this script puts results in the current directory.  It is
# up to the user/calling script to control.
#
# required inputs:
#    -anat       : anat dset (okay, this is not really required)
#    -epi_me_run : EPI time series (one option set per run)
#      .
#      .
#      .
#    -epi_me_run : EPI time series (one option set per run)
#
# See -help output for details.
#
# ===========================================================================

# todo:
#     - automatically compute blur size

# ----------------------------------------------------------------------
# required user parameters
set anat       = () # optional anat dset
set echo_times = () # we found this spoon, sir!
set epi_list   = () # all echoes, all runs
set reg_echo   = 2  # registration echo

# ----------------------------------------------------------------------
# other user-controllable parameters
set nt_rm     = 2       # number of time points to remove
                        # rcr - make def to compute from TR?
set subjid    = 'SUBJ'  # rcr - make def to guess from BIDS?
                        # rcr - rewrite this in python?  (or subsume in AP)
set run_ap    = 0       # do we run afni_proc.py?
set run_proc  = 0       # do we run the resulting proc script?

set blur_size = 0
set template  = MNI152_2009_template_SSW.nii.gz
set verb      = 1

# run_clustsim is not relevant for rest

# ----------------------------------------------------------------------
# computed variables
set nruns      = 0  # number of runs (num -epi_me_run options)
set necho      = 0  # num echoes (per run)

# ===========================================================================
# display the -hist output
goto POST_HIST
SHOW_HIST:

cat << EOF

-----------------------------------------------------------------
$prog modification history:

   0.0  : Apr  8, 2024: same as ap_run_simple_rest.tcsh
                      - but for multi-echo data

EOF

exit
POST_HIST:

# ----------------------------------------------------------------------
# parameters not controlled by user
set script_version = 0.0   # see SHOW_HIST, just above

set prog = `basename $0`


# ===========================================================================
# process options
#
# The main if-else option processing in the while loop has each condition
# left in the state of pointing to the final argument processed for the
# given condition.  So $ac is incremented afterwards, and we prefer not to
# use "continue".

if ( $#argv < 1 ) then
   goto SHOW_HELP
endif

set ac = 1
set narg = $#argv
while ( $ac <= $narg )
   # -------------------- main terminal opts --------------------
   if ( "$argv[$ac]" == '-help' ) then
      goto SHOW_HELP

   else if ( "$argv[$ac]" == '-hist' ) then
      goto SHOW_HIST

   else if ( "$argv[$ac]" == '-ver' ) then
      echo "version $script_version"
      exit 0

   # -------------------- main user opts --------------------
   else if ( "$argv[$ac]" == '-anat' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -anat requires 1 parameter"
         exit 1
      endif
      set anat = $argv[$ac]

   else if ( "$argv[$ac]" == '-echo_times' ) then
      @ ac ++
      # first collect current list
      set echo_times = ()
      while ( $ac <= $narg )
         if ( "`echo $argv[$ac] | cut -b 1`" == "-" ) break
         set echo_times = ( $echo_times $argv[$ac] )
         @ ac ++
      end

      # we are no longer looking at an argument that applies to this option
      @ ac -= 1

   else if ( "$argv[$ac]" == '-epi_me_run' ) then
      @ ac ++
      # first collect current list
      set epi_tmp = ()
      while ( $ac <= $narg )
         if ( "`echo $argv[$ac] | cut -b 1`" == "-" ) break
         set epi_tmp = ( $epi_tmp $argv[$ac] )
         @ ac ++
      end

      # we are no longer looking at an argument that applies to this option
      @ ac -= 1

      # if this is the first time, initialize
      if ( $nruns == 0 ) then
         set necho = $#epi_tmp
      else
         # verify that necho is consistent
         if ( $necho != $#epi_tmp ) then
            echo "** inconsistent number of echoes per run with -epi_me_run"
            echo "   initial number of echoes was $necho, current is $#epi_tmp"
            exit 1
         endif
      endif

      # we have a new run
      set epi_list = ( $epi_list $epi_tmp )
      @ nruns ++

   # -------------------- other opts --------------------
   else if ( "$argv[$ac]" == '-nt_rm' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -subjid requires 1 parameter"
         exit 1
      endif
      set nt_rm = $argv[$ac]

   else if ( "$argv[$ac]" == '-reg_echo' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -reg_echo requires 1 parameter"
         exit 1
      endif
      set reg_echo = $argv[$ac]

   else if ( "$argv[$ac]" == '-run_ap' ) then
      set run_ap = 1
   else if ( "$argv[$ac]" == '-run_proc' ) then
      set run_ap = 1
      set run_proc = 1
   else if ( "$argv[$ac]" == '-subjid' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -subjid requires 1 parameter"
         exit 1
      endif
      set subjid = $argv[$ac]

   else if ( "$argv[$ac]" == '-template' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -template requires 1 parameter"
         exit 1
      endif
      set template = $argv[$ac]

   else if ( "$argv[$ac]" == '-blur_size' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -blur_size requires 1 parameter"
         exit 1
      endif
      set blur_size = $argv[$ac]

   else if ( "$argv[$ac]" == '-compressor' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -compressor requires 1 parameter"
         exit 1
      endif

      if ( ! ( "$argv[$ac]" == "COMPRESS" || \
               "$argv[$ac]" == "GZIP"     || \
               "$argv[$ac]" == "BZIP2"    || \
               "$argv[$ac]" == "PIGZ" ) ) then
         echo "** Must use an allowed keyword for AFNI_COMPRESSOR env var:"
         echo "   GZIP  COMPRESS  BZIP2  PIGZ"
         exit 1
      endif

      setenv AFNI_COMPRESSOR $argv[$ac]

   # -echo is special case of -verb
   else if ( "$argv[$ac]" == '-echo' ) then
      set verb = 3
      set echo
   else if ( "$argv[$ac]" == '-verb' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -verb requires 1 parameter"
         exit 1
      endif
      set verb = $argv[$ac]

      # test for integral
      set vtmp = `ccalc -i $verb`
      if ( $vtmp != $verb ) then
         echo "** -verb requires an integral parameter"
         exit 1
      endif

      # on very verbose, turn on echo
      if ( $verb > 2 ) then
         set echo
      endif

   # bad things, man
   else
      echo "** unknown option '$argv[$ac]'"
      exit 1
   endif

    @ ac ++
end

# ---------------------------------------------------------------------------
if ( $verb > 1 ) then
   # display parameters?
cat << EOF
++ parameters for program $prog :
   anat        = $anat
   echo_times  = $echo_times
   epi_list    = $epi_list
   nt_rm       = $nt_rm
   reg_echo    = $reg_echo
   blur_size   = $blur_size
   run_ap      = $run_ap
   run_proc    = $run_proc
   subjid      = $subjid
   template    = $template
   verb        = $verb
EOF
endif


# ===========================================================================
# check for missing inputs and set main variables

# are expect processing files already here?
set script_ap   = run_ap_$subjid
set script_proc = proc.$subjid
set dir_results = $subjid.results

foreach file ( $script_ap $script_proc $dir_results )
   if ( -e $file ) then
      echo "** refusing to overwrite existing '$file'"
      exit 1
   endif
end

# allow missing anat (skip align and tlrc blocks)

# do we have EPI datasets?
if ( $#epi_list == 0 ) then
   echo "** missing -epi_me_run dataset inputs"
   exit 1
endif

# do we have echo times?
if ( $#echo_times == 0 ) then
   echo "** missing -echo_times"
   exit 1
else if ( $#echo_times != $necho ) then
   echo "** have $#echo_times echo times but $necho echoes per run"
   exit 1
endif

# ===========================================================================
# apply any defaults (after validating input) if the user has not specified

if ( $blur_size == 0 ) then
   set blur_size = `afni_python_wrapper.py -print \
         "get_def_blur_from_dims('$epi_list[1]',scale=1.1)" |& tail -n 1`
   if ( $blur_size == "0" ) then
      echo "** failed to get blur size from dims of $epi_list[1]"
      exit 1
   endif
   echo "++ setting blur from voxel sizes: $blur_size"
endif

# ===========================================================================
# generate afni_proc.py script
# (variable newlines are kept tight)

if ( $verb > 0 ) echo "++ writing afni_proc.py command script, $script_ap"

# generate -dsets_me_run options
set opt_me_run = ()
set ebase = 1
foreach run ( `count_afni 1 $nruns` )
   @ elast = $ebase + $necho - 1
   set opt_me_run = ( $opt_me_run "-dsets_me_run" $epi_list[$ebase-$elast] )
   @ ebase += $necho
end

# ---------------------------------------------------------------------------
# write AP command
# ----------------

# This does not need to start out pretty, as we will adjust it.

# ----- case: with anat
#       (no indentation, sorry)
if ( $#anat > 0 ) then

cat << EOF > $script_ap

# ----------------------------------------------------------------------
# This is a simple afni_proc.py command used for QC of multi-echo data.
# The task is treated as rest.  Template alignment is merely affine.
# 
# generated by $prog, version $script_version
# ----------------------------------------------------------------------

afni_proc.py                                                        \
    -subj_id                   $subjid \
    -blocks                    tshift align tlrc volreg mask        \
                               combine blur scale regress           \
    -radial_correlate_blocks   tcat volreg regress                  \
    -copy_anat                 $anat \
    $opt_me_run \
    -echo_times                $echo_times \
    -combine_method            OC \
    -reg_echo                  $reg_echo \
    -tcat_remove_first_trs     $nt_rm \
    -tshift_interp             -wsinc9                              \
    -align_unifize_epi         local                                \
    -align_opts_aea            -cost lpc+ZZ -giant_move -check_flip \
    -tlrc_base                 $template \
    -volreg_align_to           MIN_OUTLIER                          \
    -volreg_align_e2a                                               \
    -volreg_tlrc_warp                                               \
    -volreg_warp_final_interp  wsinc5                               \
    -volreg_compute_tsnr       yes                                  \
    -mask_epi_anat             yes                                  \
    -blur_size                 $blur_size                           \
    -regress_censor_motion     0.25                                 \
    -regress_censor_outliers   0.05                                 \
    -regress_motion_per_run                                         \
    -regress_apply_mot_types   demean deriv                         \
    -regress_est_blur_epits                                         \
    -regress_est_blur_errts                                         \
    -regress_make_ideal_sum    sum_ideal.1D                         \
    -html_review_style         pythonic

EOF

# save status
set rv = $status

# ----- case: only EPI, no anat
else

# no anat, so omit:
#
#     -blocks align tlrc
#     -copy_anat
#     -align_unifize_epi
#     -align_opts_aea
#     -tlrc_base
#     -volreg_align_e2a
#     -volreg_tlrc_warp
#     -mask_epi_anat
#

cat << EOF > $script_ap

# ----------------------------------------------------------------------
# This is a simple afni_proc.py command used for QC of multi-echo data.
# The task is treated as rest.  There is no anat or corresponding options.
# 
# generated by $prog, version $script_version
# ----------------------------------------------------------------------

afni_proc.py                                                        \
    -subj_id                   $subjid \
    -blocks                    tshift volreg mask                   \
                               combine blur scale regress           \
    -radial_correlate_blocks   tcat volreg regress                  \
    $opt_me_run \
    -echo_times                $echo_times \
    -combine_method            OC                                   \
    -reg_echo                  $reg_echo \
    -tcat_remove_first_trs     $nt_rm \
    -tshift_interp             -wsinc9                              \
    -volreg_align_to           MIN_OUTLIER                          \
    -volreg_compute_tsnr       yes                                  \
    -blur_size                 $blur_size                           \
    -regress_censor_motion     0.25                                 \
    -regress_censor_outliers   0.05                                 \
    -regress_motion_per_run                                         \
    -regress_apply_mot_types   demean deriv                         \
    -regress_est_blur_epits                                         \
    -regress_est_blur_errts                                         \
    -regress_make_ideal_sum    sum_ideal.1D                         \
    -html_review_style         pythonic

EOF

# save status
set rv = $status

# ----- end case: for w/wout anat
endif

# ---------------------------------------------------------------------------
# end: write AP command
# ---------------------

# did write succeed?
if ( $rv ) then
   echo "** failed to create $script_ap, can you write here?"
   exit 1
endif

# ---------------------------------------------------------------------------
# possibly fix line wrappers
set wapw = `which afni_python_wrapper.py`
if ( ! $status ) then
    if ( $verb > 1 ) echo "++ adjusting line wrappers"
    cat $script_ap | afni_python_wrapper.py -exec "wrap_file_text()" \
                   > .tmp.$script_ap
    if ( $status ) then
       echo "** afni_python_wrapper.py failure"
       \mv .tmp.$script_ap failed.$script_ap
    else
       \mv .tmp.$script_ap $script_ap
    endif
endif

# ===========================================================================
# if we are not actually running the proc script, we are done
if ( ! $run_ap ) exit 0

echo ""

# ===========================================================================
# check on existence of datasets, only when processing

# do we have valid anat (if given) and EPI
if ( $#anat > 0 ) then
   set vtmp = `3dinfo -nt $anat`
   if ( $status || $vtmp == "NO-DSET" ) then
      echo "** do not seem to have valid anat, '$anat'"
      exit 1
   endif
endif

set vtmp = `3dinfo -nt $epi_list | grep NO-DSET | wc -l`
if ( $status || $vtmp > 0 ) then
   echo "** invalid EPI: found $vtmp bad dsets out of $#epi_list"
   exit 1
endif

# ===========================================================================
# actually run afni_proc.py, and possibly the proc script

tcsh -x $script_ap |& tee output.$script_ap

if ( $status || ! $run_proc ) exit 0

echo ""
time tcsh -xef $script_proc |& tee output.$script_proc


# ===========================================================================
# terminate script, to separate help
exit


# ===========================================================================
# display the -help output

SHOW_HELP:
cat << EOF

------------------------------------------------------------------------------
ap_run_simple_rest_me.tcsh 

Run a quick afni_proc.py analysis for QC on multi-echo data.

   usage:  ap_run_simple_rest_me.tcsh            \
               [options]                         \
               -anat        anat.nii             \
               -epi_me_run  epi_run1_echo_*.nii ...

This program is meant to run a moderately quick single subject analysis,
treating the EPI as resting state data.

Overview:

   0. This program will change over time.  Do not rely on a fixed version.
      See "$prog -ver" for the current version number.
   1. Output files are placed in the current directory, so it is suggested
      to run from a "clean" location, generally away from the raw inputs.
   2. Template registration is merely affine, to be fast.
   3. By default, the first 2 time points are removed as pre-steady state.
      It is a good idea to set -nt_rm appropriately.

   inputs   : anat (optional), EPI echos (one set of echoes per run),
              echo times

   controls : recommended opts: -subjid, -nt_rm

   outputs  : run_ap_SUBJID   - afni_proc.py command script
            : proc.SUBJID     -_proc script (if AP is run)
            : SUBJID.results  - proc results dir (if run)
            : out.*           - text output files from AP and proc scripts

This program may be devoured by afni_proc.py itself, at some point.

------------------------------------------------------------------------------
example 0: just create an afni_proc.py script, run_ap_SUBJ, no data required

      ap_run_simple_rest_me.tcsh -anat anat.nii \
          -epi_me_run epi_echo_*.nii -echo_times 20 30 40


example 1: quickly process EPI (no anat, so no align/tlrc blocks)

      $prog -epi_me_run epi_echo_*.nii -echo_times 20 30 40


example 2: run an analysis from a clean directory

   We should really not run from a data source directory, but it is done to
   keep paths short.  The test.ap directory can be removed once run.

      cd APMULTI_Demo1_rest/data_00_basic/sub-005/ses-01
      mkdir test.ap
      cd test.ap

      $prog                                       \\
          -subjid sub-005                                              \\
          -anat       ../anat/sub-*_mprage_run-1_T1w.nii.gz            \\
          -epi_me_run ../func/sub-*_task-rest_run-1_echo-*_bold.nii.gz \\
          -echo_times 12.5 27.6 42.7                                   \\
          -nt_rm 4                                                     \\
          -run_proc


example 3: similar to 2, but assuming there are 4 runs, 3 echoes in each

      $prog                                       \\
          -subjid sub-005                                              \\
          -epi_me_run ../func/sub-*_task-rest_run-1_echo-*_bold.nii.gz \\
          -epi_me_run ../func/sub-*_task-rest_run-2_echo-*_bold.nii.gz \\
          -epi_me_run ../func/sub-*_task-rest_run-3_echo-*_bold.nii.gz \\
          -epi_me_run ../func/sub-*_task-rest_run-4_echo-*_bold.nii.gz \\
          -echo_times 12.5 27.6 42.7                                   \\
          -nt_rm 4                                                     \\
          -run_proc


------------------------------------------------------------------------------
terminal options:

   -help                   : show this help
   -hist                   : show the program history
   -ver                    : show the version number

required parameters:

   -epi_me_run EPI_echo_1 EPI_echo_2 ...  : specify one run of EPI echo dsets

         example: -epi_me_run  epi_run-1_echo-*.nii.gz

         example: -epi_me_run  epi_run-1_echo-*.nii.gz
                  -epi_me_run  epi_run-2_echo-*.nii.gz
                  -epi_me_run  epi_run-3_echo-*.nii.gz
                  -epi_me_run  epi_run-4_echo-*.nii.gz

      This option specifies the EPI data, but each such option specifies one
      run of all echoes.  If there are 5 runs, then 5 such option sets should
      be used.

   -echo_times e1_time e2_time e3_time ... : specify echo times, in ms

         example: -echo_times 12.5 27.6 42.7

optional parameters:

   -anat ANAT              : specify single anatomical dataset

      This is used for anat/EPI alignment, as well as anat/template alignment.

   -nt_rm NT               : num time points to remove from starts of runs
                             def: $nt_rm

   -run_ap                 : actually run the afni_proc.py command
                             def: do not, just generate AP command script

   -run_proc               : do the processing (run the proc script from AP)
                             def: do not run AP or proc script

   -subjid SUBJ_ID         : specify subject ID for file names
                             def: $subjid

   -template TEMPLATE      : specify template for standard space
                             def: $template

   -compressor COMP        : control automatic compression of *.BRIK files.
                             'COMP' must be one of the allowed keywords for
                             the AFNI_COMPRESSOR environment variable:
                                GZIP  COMPRESS  BZIP2  PIGZ
                             and you must have the associated program for
                             compression installed (e.g., 'gzip')
                             def: not set here

   -verb VERB              : specify verbosity level (3 == -echo)
                             def: $verb

   -echo                   : set 'echo' in the shell, as if run via 'tcsh -x'
                             (same as '-verb 3')

------------------------------------------------------------------------------
R Reynolds March, 2024
version $script_version
------------------------------------------------------------------------------

EOF

# terminate after -help
exit


