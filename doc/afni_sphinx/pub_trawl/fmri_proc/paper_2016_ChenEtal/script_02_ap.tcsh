#!/bin/tcsh

# --------------------------------------------------------------------
# Script: script_02_ap.tcsh
#
# From:
# Chen GC, Taylor PA, Shin Y-W, Reynolds RC, Cox RW (2016). Untangling
# the Relatedness among Correlations, Part II: Inter-Subject
# Correlation Group Analysis through Linear Mixed-Effects
# Modeling. Neuroimage (in press).
#
# Originally run using: AFNI_16.1.16
# --------------------------------------------------------------------


# FMRI processing script, ISC movie data.
# Assumes previously run FS and SUMA commands, respectively: 
# $ recon-all -all -subject $subj -i $anat
# $ @SUMA_Make_Spec_FS -sid $subj -NIFTI

# Set top level directory structure
set subj    = $1
set topdir  = TOP_LEVEL_FILE_LOCATION
set task    = movie
set fsroot  = $topdir/freesurfer/subjects
set outroot = $topdir/subject_results/$task.6

# Input directory: unprocessed FMRI data
set indir   = $topdir/orig.data
# Input directory: FreeSurfer + @SUMA_MakeSpec_FS results
set fsindir = $fsroot/$subj/SUMA

# Output directory: name for output
set outdir  = $outroot/$subj

# Input data: list of partitioned EPIs (movie clips)
set epi_dpattern = "movie*.HEAD"

# Input data: FreeSurfer results (anatomy, ventricle and WM maps)
set fsanat = ${subj}_SurfVol.nii
set fsvent = FSmask_vent.nii
set fswm   = FSmask_WM.nii

afni_proc.py -subj_id $subj.$task                                       \
    -blocks despike tshift align tlrc volreg blur mask regress          \
    -copy_anat $fsindir/$fsanat                                         \
    -anat_follower_ROI aaseg  anat $fsindir/aparc.a2009s+aseg_rank.nii  \
    -anat_follower_ROI aeseg  epi  $fsindir/aparc.a2009s+aseg_rank.nii  \
    -anat_follower_ROI FSvent epi  $fsindir/$fsvent                     \
    -anat_follower_ROI FSWMe  epi  $fsindir/$fswm                       \
    -anat_follower_erode FSvent FSWMe                                   \
    -dsets $epi_dpattern                                                \
    -tcat_remove_first_trs 0                                            \
    -tlrc_base TT_N27+tlrc                                              \
    -tlrc_NL_warp                                                       \
    -volreg_align_to MIN_OUTLIER                                        \
    -volreg_align_e2a                                                   \
    -volreg_tlrc_warp                                                   \
    -regress_ROI_PC FSvent 3                                            \
    -regress_make_corr_vols aeseg FSvent                                \
    -regress_anaticor_fast                                              \
    -regress_anaticor_label FSWMe                                       \
    -regress_censor_motion 0.2                                          \
    -regress_censor_outliers 0.1                                        \
    -regress_apply_mot_types demean deriv                               \
    -regress_est_blur_epits                                             \
    -regress_est_blur_errts                                             \
    -regress_run_clustsim no
