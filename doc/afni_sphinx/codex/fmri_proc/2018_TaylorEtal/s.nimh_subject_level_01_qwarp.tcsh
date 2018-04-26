#!/usr/bin/env tcsh

# ----------------------------------------------------------------------
# Script: s.nimh_subject_level_01_qwarp.tcsh
# Run on: openfmri/ds001_R2.0.4
# Date  : April, 2018
#
# This script runs @SSwarper for one subject, whose cognomen (ID) is
# the sole command line argument. The results are used in the analysis
# script.
#
# From @SSwarper's help: 
#   Script to skull-strip and warp to the MNI 2009 template.
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
# ----------------------------------------------------------------------
#
# To run for a single subject, for example:
#
#   tcsh -ef s.nimh_subject_level_01_qwarp.tcsh "01"
#
#########################################################################

# The only input argument is the subject number (zero-paddeed, as
# "01", "02", etc., if that is how the file naming conventions are)
if( $#argv < 1 )then
    echo "** ERROR: no command line argument?" 
    exit 1
endif

# Set subject ID 
set ss = $argv[1]

# Make path/file structure variables for readability.

# Top level directory
set p_0        = /data/NIMH_SSCC
set path_main  = ${p_0}/openfmri/ds001_R2.0.4
# Inputs
set prep_onset = ${path_main}/data_basic/ONSETS
set path_func  = ${path_main}/data_basic/sub-${ss}/func
set path_anat  = ${path_main}/data_basic/sub-${ss}/anat

# ========================================================================

# This section for possibly running on Biowulf cluster.

# Set thread count
if( $?SLURM_CPUS_PER_TASK )then
  setenv OMP_NUM_THREADS $SLURM_CPUS_PER_TASK
endif

# ========================================================================

# switch to the subject’s anat directory
if( ! -e $path_anat/sub-${ss}_T1w.nii.gz ) then
    echo "** ERROR: can’t find file $path_anat/sub-${ss}_T1w.nii" 
    exit 1
endif

cd $path_anat

# Do the work
@SSwarper sub-${ss}_T1w.nii.gz sub-${ss}

# =============================================================================

echo "++ DONE with warping for subject: $subj"

time ; exit 0

