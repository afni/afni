#!/bin/tcsh

# This is an example of making the [3] brick for the @SSwarper target
# volume, which is simply a whole brain mask of the skullstripped
# dset.  Might not even really need a script to do...
#
# ver: 1.0
# dat: 2018-06-06
# written by PA Taylor
#
# ver: 1.1
# dat: 2018-07-04
# + [PT] filled in holes in interior of mask
#
# ====================================================================

set rset  = HaskinsPeds_NL_template1.0+tlrc.
set opref = BRICK3        
set blab  = "Bmask"
set tpref = _tmp_brick3   # prefix for tmp files-- clean at end

# ----------------------------------------------------------------

# mask dset
set fout = ${tpref}_bin.nii.gz
3dcalc                              \
    -a "$rset"                      \
    -expr 'step(a)'                 \
    -prefix ${fout}

set fin  = $fout
set fout = "${opref}_${blab}.nii.gz"
3dmask_tool                         \
    -fill_holes                     \
    -inputs $fin                    \
    -prefix $fout

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# -----------------------------------------------

\rm ${tpref}*

# -----------------------------------------------

echo "\n\n++ Done: ${fout}\n\n"

exit 0
