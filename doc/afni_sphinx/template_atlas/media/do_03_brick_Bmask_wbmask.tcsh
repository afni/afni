#!/bin/tcsh

# This is an example of making the [3] brick for the @SSwarper target
# volume, which is simply a whole brain mask of the skullstripped
# dset.  Might not even really need a script to do...
#
# ver: 1.0
# dat: 2018-06-06
# written by PA Taylor
#
# ====================================================================

set rset  = TT_N27+tlrc.
set opref = BRICK3        
set blab  = "Bmask"

# ----------------------------------------------------------------

# mask dset
set fout = "${opref}_${blab}.nii.gz"
3dcalc                              \
    -a "$rset"                      \
    -expr 'step(a)'                 \
    -prefix ${fout}

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# -----------------------------------------------

echo "\n\n++ Done: ${fout}\n\n"

exit 0
