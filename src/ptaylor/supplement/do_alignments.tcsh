#!/bin/tcsh

# Warp data from MNI space to subject DWI

# set ver  = 1.0 ; set date = 'June 12, 2019'
#
# set ver  = 1.1 ; set date = 'June 13, 2019'
# + [PT] fixed some quotations and variable names 
#
#######################################################################


# --------- set input dsets/names ---------------------

set subj      = SUB-001
set dat_topd  = /data/FATCAT_DEMO2/data_proc/SUBJ_001
set odir      = "${dat_topd}"

set vol_t1w   = ${dat_topd}/anat_01/t1w.nii.gz
set vol_b0    = ${dat_topd}/dwi_05/dwi_dwi.nii.gz'[0]'
set template  = /data/REF_TEMPLATES_AFNI/MNI152_2009_template_SSW.nii.gz
set templ_roi = /data/REF_TEMPLATES_AFNI/MNI_caez_ml_18+tlrc.HEAD

setenv OMP_NUM_THREADS  8

# -----------------------------------------------------------------------

# Align T1w to MNI (produces an affine transform and a warp data set),
# and make a copy of T1w that is skull stripped
@SSwarper \
    -base  "$template"    \
    -input "$vol_t1w"     \
    -subid "$subj"        \
    -odir  "$odir"

# skullstripped t1w vol output by @SSwarper
set t1w_ss = $odir/anatSS.${subj}.nii

# will be t1w vol in b0 space (for checking alignment); is created by
# 3dAllineate
set vol_t1w2b0 = $odir/t1w_in_b0.nii

# Align T1w to b0
3dAllineate   -echo_edu               \
    -1Dmatrix_save  ${odir}/mat_t1w_to_b0.aff12.1D   \
    -prefix         "$vol_t1w2b0"        \
    -base           "$vol_b0"         \
    -twopass                          \
    -cost lpa                         \
    -cmass                            \
    -source_automask                  \
    -autoweight                       \
    -source         "$t1w_ss"         \
    -final wsinc5                     \
    -overwrite

# now, concatenate warps from above
3dNwarpCat                                             \
    -prefix $odir/warp_from_MNI_to_b0.nii              \
    -warp1 "${odir}/mat_t1w_to_b0.aff12.1D"            \
    -warp2 "INV(${odir}/anatQQ.${subj}.aff12.1D)"      \
    -warp3 "INV(${odir}/anatQQ.${subj}_WARP.nii)"      

# finally, apply the full warp from MNI to b0:
# ... the MNI volume, to check
3dNwarpApply -prefix $odir/mni_in_b0.nii.gz               \
             -source $template                      \
             -master "$vol_b0"                      \
             -nwarp "$odir/warp_from_MNI_to_b0.nii" \
             -ainterp wsinc5

# ... the ROI volume, from MNI space
3dNwarpApply -prefix $odir/mni_in_b0_ROI.nii.gz    \
             -source $templ_roi     \
             -master "$vol_b0"         \
             -nwarp "$odir/warp_from_MNI_to_b0.nii" \
             -ainterp NN
