#!/usr/bin/env tcsh

# ----------------------------------------------------------------------
# Script: s.nimh_subject_level_02_ap.tcsh
# Run on: openfmri/ds001_R2.0.4
# Date  : April, 2018
#
# Time series analysis for one subject, modified from BMN afni_proc.py
# command to adhere to current AFNI usage recommendations. The
# accompanying "s.nimh_subject_level_01_qwarp.tcsh" script must have
# been run previously to get the warped-to-MNI files for the current
# subject via @SSwarper.
#
# Used for "NIMH-AFNI" processing in:
#
#   Some comments and corrections on FMRI processing with AFNI in
#   "Exploring the Impact of Analysis Software on Task fMRI Results"
# 
#   Paul A. Taylor, Gang Chen, Daniel R. Glen, Justin K. Rajendra,
#   Richard C. Reynolds, Robert W. Cox.
#   Scientific and Statistical Computing Core, NIMH/NIH/DHHS,
#   Bethesda, MD, USA.
#
# NOTE: This afni_proc.py command includes several features that would
# have been chosen in preference to those of BMN, as described in the
# above text.  *However*, it is not a complete set.  Further features
# should also likely be adjusted, such as in the GLT specification
# (see points A-5 and A-6 in the accompanying text) and upsampling to
# 2x2x2 mm^3 would also note be chosen.  These features were not
# changed *here*, for the purposes of matching the BMN specifications
# more closely.  In general, please see the descriptions in the above
# bioRxiv text for details on settings/options that were chosen.
#
# ----------------------------------------------------------------------
#
# To run for a single subject, for example:
#
#   tcsh -ef s.nimh_subject_level_02_ap.tcsh "01"
#
# After this, one can run the following for QC and checking processing
# steps:
#
#   @ss_review_basic
#   @ss_review_driver
#
# ... and after all subjects in the group have been processed, the
# following can be run to combine the single subject processing
# numbers into one table:
#
#   gen_ss_review_table.py                       \
#      -tablefile review_table_nimh.xls          \
#      -infiles data_proc_nimh/sub*/out.ss_review*
#
#########################################################################

# ===================== Set inputs and paths =========================

# The only input argument is the subject number (zero-paddeed, as
# "01", "02", etc., if that is how the file naming conventions are)
if( $#argv < 1 )then
    echo "** ERROR: no command line argument?" 
    exit 1
endif

# Set subject ID from input
set ss         = $argv[1]
set subj       = sub-${ss}  
set grp        = "nimh"

# Set reference template for standard space (location found below)
set itemp      = MNI152_2009_template.nii.gz

# Make path/file structure variables for readability.

# Top level directory
set p_0        = /data/NIMH_SSCC
set path_main  = ${p_0}/openfmri/ds001_R2.0.4
# Inputs
set prep_onset = ${path_main}/data_basic/ONSETS
set path_func  = ${path_main}/data_basic/sub-${ss}/func
set path_anat  = ${path_main}/data_basic/sub-${ss}/anat
# Outputs
set path_ogrp  = ${path_main}/data_proc_${grp}
set path_out   = ${path_ogrp}/sub-${ss}

mkdir -p ${path_ogrp}

# =================== Environment variables ===========================

# NB: need this because of initial data's qform code
setenv AFNI_NIFTI_VIEW orig

# ================== Optional: Biowulf cluster =========================

# This section for possibly running on Biowulf cluster.

# Set thread count
if( $?SLURM_CPUS_PER_TASK )then
  setenv OMP_NUM_THREADS $SLURM_CPUS_PER_TASK
endif

# Set temporary output directory; then requires using something like
# this on the swarm command line: --sbatch '--gres=lscratch:100'.
# These variables used again *after* afni_proc.py command, if Biowulfing.
if( $?SLURM_JOBID )then
  set tempdir = /lscratch/$SLURM_JOBID/${subj}
  set usetemp = 1
else
  set tempdir = $path_out
  set usetemp = 0
endif

# ================== Find standard space template ======================

# Selecting/finding reference template (for defining final standard
# space); the chosen template file is specified above

set tpath    = `@FindAfniDsetPath $itemp`
set basedset = $tpath/$itemp

if( "$tpath" == "" )then
    echo "** ERROR: can't find my template $itemp"
    exit 1
endif

if( ! -f $basedset )then
    echo "** ERROR: template $basedset is not present" 
    exit 1
endif

# ========================= Specify processing =========================

# FINALLY: the afni_proc.py command itself, which makes a single
# subject processing script
afni_proc.py                                                                \
    -scr_overwrite                                                          \
    -subj_id  sub${ss}                                                      \
    -script   proc_${grp}.sub${ss}                                          \
    -out_dir  ${tempdir}                                                    \
    -blocks tshift align tlrc volreg blur mask scale regress                \
    -copy_anat ${path_anat}/anatSS.${subj}.nii                              \
          -anat_has_skull no                                                \
    -dsets                                                                  \
       ${path_func}/sub-${ss}_task-balloonanalogrisktask_run-01_bold.nii.gz \
       ${path_func}/sub-${ss}_task-balloonanalogrisktask_run-02_bold.nii.gz \
       ${path_func}/sub-${ss}_task-balloonanalogrisktask_run-03_bold.nii.gz \
    -tcat_remove_first_trs 2                                                \
    -align_opts_aea -cost lpc+ZZ -ginormous_move -check_flip                \
    -volreg_warp_dxyz 2                                                     \
    -volreg_align_to MIN_OUTLIER                                            \
    -volreg_align_e2a                                                       \
    -volreg_tlrc_warp                                                       \
    -tlrc_base $basedset                                                    \
    -tlrc_NL_warp                                                           \
    -tlrc_NL_warped_dsets                                                   \
         $path_anat/anatQQ.${subj}.nii                                      \
         $path_anat/anatQQ.${subj}.aff12.1D                                 \
         $path_anat/anatQQ.${subj}_WARP.nii                                 \
    -blur_size 5.0                                                          \
    -regress_stim_times                                                     \
       ${prep_onset}/sub-${ss}_combined_cash_demean_afni.1d                 \
       ${prep_onset}/sub-${ss}_combined_cash_RT_afni.1d                     \
       ${prep_onset}/sub-${ss}_combined_control_pumps_demean_afni.1d        \
       ${prep_onset}/sub-${ss}_combined_control_pumps_RT_afni.1d            \
       ${prep_onset}/sub-${ss}_combined_explode_demean_afni.1d              \
       ${prep_onset}/sub-${ss}_combined_pumps_demean_afni.1d                \
       ${prep_onset}/sub-${ss}_combined_pumps_RT_afni.1d                    \
    -regress_stim_labels                                                    \
       cash_demean cash_RT control_pumps_demean                             \
       control_pumps_RT explode_demean pumps_demean                         \
       pumps_RT                                                             \
    -regress_basis_multi                                                    \
       'BLOCK(0.772,1)' 'dmBLOCK' 'BLOCK(0.772,1)' 'dmBLOCK'                \
       'BLOCK(0.772,1)' 'BLOCK(0.772,1)' 'dmBLOCK'                          \
    -regress_stim_types                                                     \
       AM2 AM1 AM2 AM1 AM2 AM2 AM1                                          \
    -regress_censor_motion 0.3                                              \
    -regress_motion_per_run                                                 \
    -regress_opts_3dD                                                       \
    -gltsym 'SYM: pumps_demean -control_pumps_demean'                       \
    -glt_label 1 pumps_demean_vs_ctrl_demean                                \
    -regress_make_ideal_sum sum_ideal.1D                                    \
    -regress_3dD_stop                                                       \
    -regress_reml_exec                                                      \
    -regress_est_blur_epits                                                 \
    -regress_est_blur_errts                                                 \
    -execute

# =============================================================================

# Again, Biowulf-running considerations: if processing went fine and
# we were using a temporary directory, copy data back.
if( $usetemp && -d $tempdir )then
    echo "Copying data from $tempdir to $path_out"
    mkdir -p $path_out
    \cp -pr $tempdir/* $path_out/
endif

# If it worked, run some volreg snapshots and compress outputs:
# compare chosen EPI reference volume (from alignment to anatomical)
# to the final anatomical.
if( -d $path_out )then
    cd $path_out
    @snapshot_volreg                       \
        anat_final.${subj}+tlrc.HEAD       \
        final_epi_vr_base*+tlrc.HEAD       \
        ${subj}
    cd ..
endif

# =============================================================================

echo "++ DONE with afni_proc for subject: $subj"

time ; exit 0
