#!/bin/tcsh

# This is an example of making the [4] brick for the @SSwarper target
# volume, which is an inflated GM mask.
#
# Here, we take a GM mask from FS parcellation and bring it to the
# present grid; inflate it by one within the present grid.
#
# ver: 1.0
# dat: 2018-06-06
# written by PA Taylor
#
# ====================================================================

set rset  = TT_N27+tlrc.
set rmask = `ls BRICK3*nii.gz`

# Both of the dsets in the "suma_TT_N27" directory are FS output of
# standard brains, available for download from the AFNI website.
set iset  = ~/.afni/data/suma_TT_N27/brain.nii
# this dset is integer, but with many values >1; later we will
# binarize it
set igm   = ~/.afni/data/suma_TT_N27/aparc+aseg_REN_gm.nii.gz

set tpref = _tmp_brick4
set opref = BRICK4
set blab  = "GCmask"

# ---------------------------------------------------------

# make and apply warp; *should* be quasi-trivial alignment-- mostly
# this is regridding
3dAllineate                          \
    -lpa                             \
    -autoweight                      \
    -source_automask                 \
    -1Dmatrix_save ${tpref}.aff12.1D \
    -source  $iset                   \
    -base    $rset                   \
    -final   wsinc5                  \
    -prefix  ${tpref}_alnd.nii.gz
3dAllineate \
    -1Dmatrix_apply ${tpref}.aff12.1D   \
    -source         $igm                \
    -master         $rset               \
    -prefix         ${tpref}_GM.nii.gz  \
    -final NN

# inflate by 1 voxel
3dmask_tool                         \
    -inputs ${tpref}_GM.nii.gz      \
    -dilate_inputs 1                \
    -prefix ${tpref}_GMI.nii.gz     \

set fout = "${opref}_${blab}.nii.gz"
3dcalc                              \
    -a ${tpref}_GMI.nii.gz          \
    -b $rmask                       \
    -expr 'step(a)*step(b)'         \
    -prefix $fout

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# clean up
\rm ${tpref}*

echo "\n\n++ Done, brick [4]: $fout\n\n"

exit 0
