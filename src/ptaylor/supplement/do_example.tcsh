#!/bin/tcsh

# Warp data from MNI space to subject DWI

# set ver  = 1.0 ; set date = 'June 12, 2019'
#
# set ver  = 1.1 ; set date = 'June 13, 2019'
# + [PT] fixed some quotations and variable names 
#
#######################################################################

#:TITLE: Combining multiple (linear and affine) alignments

#:SECTION: Define variables

# --------- set input dsets/names ---------------------

set subj      = SUB-001
set dat_topd  = /data/FATCAT_DEMO2/data_proc/SUBJ_001
set odir      = "${dat_topd}"

set vol_t1w   = ${dat_topd}/anat_01/t1w.nii.gz
set vol_b0    = ${dat_topd}/dwi_05/dwi_dwi.nii.gz  # [0]th vol selected below
set template  = /data/REF_TEMPLATES_AFNI/MNI152_2009_template_SSW.nii.gz
set templ_roi = /data/REF_TEMPLATES_AFNI/MNI_caez_ml_18+tlrc.HEAD

#:SECTION: Nonlinear warp T1w -> MNI (and T1w skullstrip)

cat <<TEXTBLOCK

Use @SSwarper to make warps from T1w coord sys to standard/MNI one.

TEXTBLOCK

# Align T1w to MNI (produces an affine transform and a warp data set),
# and make a copy of T1w that is skull stripped
#@SSwarper \
#    -base  "$template"    \
#    -input "$vol_t1w"     \
#    -subid "$subj"        \
#    -odir  "$odir"

ls *tcsh

@chauffeur_afni                                               \
    -ulay       "$template"                                   \
    -prefix     $odir/IMAGE                                   \
    -montx 8 -monty 1                                         \
    -set_subbricks   0 0 0                                    \
    -set_dicom_xyz   5 18 18                                  \
    -delta_slices   10 20 10                                  \
    -set_xhairs     OFF                                       \
    -label_mode 1 -label_size 3                               \
    -do_clean

cat <<TEXTBLOCK

We will display these images, with ones in same line put into same
line, and separate lines into separate lines.

#:IMAGE: Ooooptional title || || indeed
    AAAA.sag.png AAAA.cor.png
    NULL  AAAA.axi.png
#:IMCAPTION: caption for any image(s)

TEXTBLOCK

# skullstripped t1w vol output by @SSwarper
set t1w_ss = $odir/anatSS.${subj}.nii

#:SECTION: Affine align T1w to b0

# will be t1w vol in b0 space (for checking alignment); is created by
# 3dAllineate
set vol_t1w2b0 = $odir/t1w_in_b0.nii

goto JUMP2
# Align T1w to b0
3dAllineate   -echo_edu               \
    -1Dmatrix_save  ${odir}/mat_t1w_to_b0.aff12.1D   \
    -prefix         "$vol_t1w2b0"     \
    -base           "${vol_b0}[0]"    \
    -twopass                          \
    -cost lpa                         \
    -cmass                            \
    -source_automask                  \
    -autoweight                       \
    -source         "$t1w_ss"         \
    -final wsinc5                     \
    -overwrite
JUMP2:

#:HIDE_ON:

# put an @chauffeur command here ....
@chauffeur_afni \
    -ulay "$vol_b0" \
    -set_subbricks 0 0 0 \
    -olay "$vol_t1w2b0"  \
    -prefix AAAA         \
    -label_mode 1 -label_size 3             \
    -do_clean 

#:HIDE_OFF:

#:SECTION: Concatenate warps in appropriate order/directions

goto JUMP4

# now, concatenate warps from above
3dNwarpCat                                             \
    -overwrite                                         \
    -prefix $odir/warp_from_MNI_to_b0.nii              \
    -warp1 "${odir}/mat_t1w_to_b0.aff12.1D"            \
    -warp2 "INV(${odir}/anatQQ.${subj}.aff12.1D)"      \
    -warp3 "INV(${odir}/anatQQ.${subj}_WARP.nii)"      

JUMP4:

#:SECTION: Apply warps, create new dsets

cat <<TEXTBLOCK

Here is a text description of what we are doing

TEXTBLOCK

goto JUMP3
# finally, apply the full warp from MNI to b0:
# ... the MNI volume, to check
3dNwarpApply 
    -prefix mni_in_b0.nii.gz               \
    -source $template                      \
    -master "$vol_b0"                      \
    -nwarp "$odir/warp_from_MNI_to_b0.nii" \
    -ainterp wsinc5

# ... the ROI volume, from MNI space
3dNwarpApply -prefix mni_in_b0_ROI.nii.gz    \
             -source $templ_roi     \
             -master "$vol_b0"         \
             -nwarp "$odir/warp_from_MNI_to_b0.nii" \
             -ainterp NN
JUMP3:

echo "++ DONE"
