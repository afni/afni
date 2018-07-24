#!/bin/tcsh

# This is an example of making the [4] brick for the @SSwarper target
# volume, which is an inflated GM mask.
#
# Here, we take a GM mask from FS parcellation that is used as the
# atlas with this template (so it is fortunately in the same space);
# we remove the "white matter" parts that we can find, and inflate it
# one layer.
#
# ver: 1.0
# dat: 2018-06-06
# written by PA Taylor
#
# ====================================================================

set rset  = HaskinsPeds_NL_template1.0+tlrc.
set rmask = `ls BRICK3*nii.gz`

# this dset is integer, but with many values >1; later we will
# binarize it
set igm   = HaskinsPeds_NL_atlas1.0+tlrc.

set tpref = _tmp_brick4
set opref = BRICK4
set blab  = "GCmask"

# ---------------------------------------------------------

# make a gray matter-only map: AFAIK, only ROIs [3] and [21] are
# non-GM.
set fout = ${tpref}_GM.nii.gz
3dcalc                                             \
    -a $igm                                        \
    -expr 'a*(not(equals(a,3))+not(equals(a,21)))' \
    -prefix $fout

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
