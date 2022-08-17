#!/usr/bin/env tcsh

# ===========================================================================
# Run a simple afni_proc.py resting state analysis, possibly to get QC output.
#
# To keep it simple, this script puts results in the current directory.  It is
# up to the user/calling script to control.
#
# required inputs:
#    -anat   : anat dset
#    -epi    : EPI time series
#
# See -help output for details.
#
# ===========================================================================


# ----------------------------------------------------------------------
# required user parameters
set anat     = ()
set epi_list = ()

# ----------------------------------------------------------------------
# other user-controllable parameters
set nt_rm    = 2        # number of time points to remove
                        # rcr - make def to compute from TR?
set subjid   = 'SUBJ'   # rcr - make def to guess from BIDS?
                        # rcr - rewrite this in python?  (or subsume in AP)
set run_ap   = 0        # do we run afni_proc.py?
set run_proc = 0        # do we run the resulting proc script?

set template = MNI152_2009_template_SSW.nii.gz
set verb     = 1

# rcr-todo
set run_clustsim = 1    # time-saving option, but takes time

# ----------------------------------------------------------------------
# parameters not controlled by user
set prog = `basename $0`
set script_version = 0.2


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

   else if ( "$argv[$ac]" == '-epi' ) then
      @ ac ++
      # keep adding to list until another option or end of inputs
      while ( $ac <= $narg )
         if ( "`echo $argv[$ac] | cut -b 1`" == "-" ) break
         set epi_list = ( $epi_list $argv[$ac] )
         @ ac ++
      end

      # we are no longer looking at an argument that applies to this option
      @ ac -= 1

   # -------------------- other opts --------------------
   else if ( "$argv[$ac]" == '-nt_rm' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -subjid requires 1 parameter"
         exit 1
      endif
      set nt_rm = $argv[$ac]

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
   anat     = $anat
   epi_list = $epi_list
   nt_rm    = $nt_rm
   run_ap   = $run_ap
   run_proc = $run_proc
   subjid   = $subjid
   template = $template
   verb     = $verb
EOF
endif


# ===========================================================================
# prepare afni_proc.py command script

# are expect processing files already here?
set script_ap   = run_ap_$subjid
set script_proc = proc.$subjid
set dir_results = $subjid.results

set out_ap      = out.$script_ap
set out_proc    = out.$script_proc

foreach file ( $script_ap $script_proc $dir_results )
   if ( -e $file ) then
      echo "** refusing to overwrite existing '$file'"
      exit 1
   endif
end

# allow missing anat (skip align and tlrc blocks)

# do we have EPI datasets?
if ( $#epi_list == 0 ) then
   echo "** missing -epi dataset inputs"
   exit 1
endif

# ===========================================================================
# generate afni_proc.py script
# (variable newlines are kept tight)

if ( $verb > 0 ) echo "++ writing afni_proc.py command script, $script_ap"

# ---------------------------------------------------------------------------
# write AP command
# ----------------

# ----- case: both EPI and anat
#       (no indentation, sorry)
if ( $#anat > 0 ) then

cat << EOF > $script_ap

# ----------------------------------------------------------------------
# This is a simple afni_proc.py command used for QC.
# The EPI is treated as rest.  Template alignment is merely affine.
# 
# generated by $prog, version $script_version
# ----------------------------------------------------------------------

afni_proc.py                                                        \
    -subj_id                   $subjid \
    -script                    $script_proc \
    -out_dir                   $dir_results \
    -blocks                    tshift align tlrc volreg mask        \
                               blur scale regress                   \
    -radial_correlate_blocks   tcat volreg                          \
    -copy_anat                 $anat \
    -dsets                     $epi_list \
    -tcat_remove_first_trs     $nt_rm \
    -align_opts_aea            -cost lpc+ZZ -giant_move -check_flip \
    -tlrc_base                 $template \
    -volreg_align_to           MIN_OUTLIER                          \
    -volreg_align_e2a                                               \
    -volreg_tlrc_warp                                               \
    -volreg_compute_tsnr       yes                                  \
    -mask_epi_anat             yes                                  \
    -blur_size                 6                                    \
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
#     -align_opts_aea
#     -tlrc_base
#     -volreg_align_e2a
#     -volreg_tlrc_warp
#     -mask_epi_anat
#

cat << EOF > $script_ap

# ----------------------------------------------------------------------
# This is a simple afni_proc.py command used for QC.
# The EPI is treated as rest.  Template alignment is merely affine.
# 
# generated by $prog, version $script_version
# ----------------------------------------------------------------------

afni_proc.py                                                        \
    -subj_id                   $subjid \
    -script                    $script_proc \
    -out_dir                   $dir_results \
    -blocks                    tshift volreg mask                   \
                               blur scale regress                   \
    -radial_correlate_blocks   tcat volreg                          \
    -dsets                     $epi_list \
    -tcat_remove_first_trs     $nt_rm \
    -volreg_align_to           MIN_OUTLIER                          \
    -volreg_compute_tsnr       yes                                  \
    -blur_size                 6                                    \
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
       \rm .tmp.$script_ap
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
$prog  - run a quick afni_proc.py analysis for QC

   usage: $prog [options] -anat ANAT -epi EPI1 EPI2 EPI3 ...

This program is meant to run a moderately quick single subject analysis,
treating the EPI as resting state data.

Overview:

   0. This program will change over time.  Do not rely on a fixed version.
      See "$prog -ver" for the current version number.
   1. Output files are placed in the current directory, so it is suggested
      to run from a "clean" location, generally away from the raw inputs.
   2. Template registration is merely affine, to be fast.
   3. Cluster simulations are run at the end, which take time.
      That might become optional, if there is interest.
   4. By default, the first 2 time points are removed as pre-steady state.
      It is a good idea to set -nt_rm appropriately.

   inputs   : anat (optional), EPI

   controls : recommended opts: -subjid, -nt_rm

   outputs  : run_ap_SUBJID   - afni_proc.py command script
            : proc.SUBJID     -_proc script (if AP is run)
            : SUBJID.results  - proc results dir (if run)
            : out.*           - text output files from AP and proc scripts

This program may be devoured by afni_proc.py itself, at some point.

------------------------------------------------------------------------------
example 0: just create an afni_proc.py script, run_ap_SUBJ, no data required

      $prog -anat anat.nii -epi epi.nii


example 1: quickly process EPI (no anat, so no align/tlrc blocks)

      $prog -epi epi.nii -run_proc


example 2: preferred - run an analysis from a clean directory

      cd AFNI_data6/FT_analysis
      mkdir test.ap
      cd test.ap
      $prog -subjid ft.qc \\
          -run_proc -nt_rm 2                \\
          -anat ../FT/FT_anat+orig          \\
          -epi ../FT/FT_epi_r*.HEAD

------------------------------------------------------------------------------
terminal options:

   -help                   : show this help

required perameters:

   -epi EPI_r1 EPI_r2 ...  : specify a list of EPI datasets

optional perameters:

   -anat ANAT              : specify single anatomical dataset

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

   -echo                   : same as -verb 3

------------------------------------------------------------------------------
R Reynolds Apr, 2021
version $script_version
------------------------------------------------------------------------------

EOF

# terminate after -help
exit


# ===========================================================================
# display the -hist output

SHOW_HIST:

cat << EOF

-----------------------------------------------------------------
$prog modification history:

   0.1  : Apr  8, 2021: initial version
   0.2  : Aug 17, 2022: -anat is now optional (only -epi is needed)

   current version: $script_version
EOF

# ******** sync with $script_version ********

# terminate after -hist
exit

