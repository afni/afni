#!/bin/tcsh

# --------------------------------------------------------------------
# Script: script_01_init.tcsh
#
# From:
# Chen GC, Taylor PA, Shin Y-W, Reynolds RC, Cox RW (2016). Untangling
# the Relatedness among Correlations, Part II: Inter-Subject
# Correlation Group Analysis through Linear Mixed-Effects
# Modeling. Neuroimage (in press).
#
# Run using: AFNI_16.1.16
# --------------------------------------------------------------------


# Commands run prior to afni_proc.py, each in appropriate 
# directories for the data sets for each subject

# Run FreeSurfer on the anatomical, and then use
# SUMA to convert the FS output to NIFTI for AFNI to use.
recon-all -all -subject $subj -i $anat
@SUMA_Make_Spec_FS -sid $subj -NIFTI

# Select the ventricle maps from the FS output.
3dcalc -a aparc+aseg.nii -datum byte -prefix FT_vent.nii \
     -expr 'amongst(a,4,43)'

# Select the WM maps from the FS output.
3dcalc -a aparc+aseg.nii -datum byte -prefix FT_WM.nii \
     -expr 'amongst(a,2,7,16,41,46,251,252,253,254,255)'
