#!/bin/tcsh

# This is an example of making the [0] brick for the @SSwarper target
# volume, which is simply a copy of the already-skullstripped
# HaskinsPeds_NL_template1.0+tlrc.  Basically, we just copy the dset
# and place a new label on it.  Might not even really need a script to
# do...
#
# ver: 1.0
# dat: 2018-07-05
# written by PA Taylor
#
# ====================================================================

set rset  = HaskinsPeds_NL_template1.0+tlrc.
set opref = BRICK0        
set blab  = "SKoff"

# ----------------------------------------------------------------

# mask dset
set fout = "${opref}_${blab}.nii.gz"
3dcalc                              \
    -a "$rset"                      \
    -expr 'a'                       \
    -prefix ${fout}                 \
    -nscale -short

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# -----------------------------------------------

echo "\n\n++ Done: ${fout}\n\n"

exit 0
