#!/usr/bin/env tcsh

# ----------------------------------------------------------------------
# Script: s.bmn_subject_level_02_ap.tcsh
# Run on: openfmri/ds001_R2.0.4
# Date  : April, 2018
#
# Time series analysis for one subject, basically unmodified from the
# BMN afni_proc.py command.  (Does not require pre-running of any
# script.)
#
# Used for "BMN-AFNI" processing in:
#
#   Some comments and corrections on FMRI processing with AFNI in
#   "Exploring the Impact of Analysis Software on Task fMRI Results"
# 
#   Paul A. Taylor, Gang Chen, Daniel R. Glen, Justin K. Rajendra,
#   Richard C. Reynolds, Robert W. Cox.
#   Scientific and Statistical Computing Core, NIMH/NIH/DHHS,
#   Bethesda, MD, USA.
#
# NOTE: This afni_proc.py command follows the Bowringer et al. (2018)
# command, described on bioRxiv.  Variable names have been adjusted
# for convenience of running on local computers, but not selected
# options.  See their bioRxiv draft and github page for reference.
#
# ----------------------------------------------------------------------
#
# To run for a single subject, for example:
#
#   tcsh -ef s.bmn_subject_level_02_ap.tcsh "01"
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
#      -tablefile review_table_bmn.xls           \
#      -infiles data_proc_bmn/sub*/out.ss_review*
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
set grp        = "bmn"

# Set reference template for standard space (location found below)
set itemp      = MNI_avg152T1+tlrc.HEAD

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
    -subj_id sub${ss}                                                       \
    -script  proc_${grp}.sub${ss}                                           \
    -out_dir ${path_out}                                                    \
    -scr_overwrite                                                          \
    -blocks tshift align tlrc volreg blur mask scale regress                \
    -copy_anat ${path_anat}/sub-${ss}_T1w.nii                               \
    -dsets                                                                  \
       ${path_func}/sub-${ss}_task-balloonanalogrisktask_run-01_bold.nii.gz \
       ${path_func}/sub-${ss}_task-balloonanalogrisktask_run-02_bold.nii.gz \
       ${path_func}/sub-${ss}_task-balloonanalogrisktask_run-03_bold.nii.gz \
    -tcat_remove_first_trs 2                                                \
    -align_opts_aea -cost mi -ginormous_move -check_flip                    \
    -anat_uniform_method unifize                                            \
    -tlrc_base ${basedset}                                                  \
    -volreg_warp_dxyz 2                                                     \
    -volreg_align_to third                                                  \
    -volreg_align_e2a                                                       \
    -volreg_tlrc_warp                                                       \
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
    -regress_opts_3dD                                                       \
    -gltsym 'SYM: pumps_demean -control_pumps_demean'                       \
    -glt_label 1 pumps_demean_vs_ctrl_demean                                \
    -regress_make_ideal_sum sum_ideal.1D                                    \
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
