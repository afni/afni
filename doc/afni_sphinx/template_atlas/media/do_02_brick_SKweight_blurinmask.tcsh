#!/bin/tcsh

# This is an example of making the [2] brick for the @SSwarper target
# volume, which is a whole head (including skull), *with* additional
# considerations: the skull-stripped brain has been blurred within its
# own mask, and the surrounding tissue (skull, neck, face, etc.) has
# been blurred within *its* own mask AND decreased in magnitude.
# Thus, this is a whole head template, but it is smoothed and the
# brightness of the non-brain tissue is attenuated.
#
# This script takes in two data sets as arguments (skull-stripped
# whole brain and full head volume with skull), and make the "blurred
# in mask" version for @SSwarper.
#
# ver: 1.0
# dat: 2018-06-06
# written by PA Taylor, RW Cox
#
# ver: 1.1
# dat: 2018-07-04
# + [PT] filled in holes in interior of mask before blurring
#
# ====================================================================

set rset  = TT_N27+tlrc.    # whole brain dset-- no skull
set iset  = `ls BRICK1_*nii.gz`  # head dset-- has skull

# ----------------------------------------------------------------

set nblur = 5             # int to mult edge length, for blur rad
set opref = BRICK2        # prefix for tmp files-- clean at end
set tpref = _tmp_brick2   # prefix for tmp files-- clean at end
set blab  = "SKweight"

# ----------------------------------------------------------------

# get min edge len of voxels for blur est

set md = 100000                  # just a reaaally big value
set ddd = `3dinfo -ad3 "$rset"`  # all vals to sort through
foreach d ( $ddd )
    set ll = `echo " $d < $md" | bc`
    if ( $ll == 1 ) then
        set md = $d
    endif
end
echo "++ Min dim = $md"

set rblur = `echo "$nblur * $md" | bc`
echo "++ Blur size (in mm): $rblur"

# ================================================================

# mask brain...
set fout = ${tpref}_00w.nii.gz
3dcalc                              \
    -a "$rset"                      \
    -expr 'step(a)'                 \
    -prefix ${tpref}_00w.nii.gz

# ... and fill in holes in this particular one
set fin  = $fout
set fout = ${tpref}_01w.nii.gz
3dmask_tool                         \
    -fill_holes                     \
    -inputs $fin                    \
    -prefix $fout

# mask of head *outside* brain
3dcalc                              \
    -a "$iset"                      \
    -b "$rset"                      \
    -expr 'step(a)*not(b)'          \
    -prefix ${tpref}_01h.nii.gz

# blur each
3dBlurInMask \
     -input  "$rset"                \
     -mask   ${tpref}_01w.nii.gz    \
     -prefix ${tpref}_02w.nii.gz    \
     -FWHM   $rblur

3dBlurInMask \
     -input  "$iset"                \
     -mask   ${tpref}_01h.nii.gz    \
     -prefix ${tpref}_02h.nii.gz    \
     -FWHM   $rblur

# combine
set fout = "${opref}_${blab}.nii.gz"
3dcalc                              \
    -a ${tpref}_02w.nii.gz          \
    -b ${tpref}_02h.nii.gz          \
    -expr 'a+0.1*b'                 \
    -prefix ${fout}

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# -----------------------------------------------

\rm ${tpref}*

# -----------------------------------------------

echo "\n\n++ Done: ${fout}\n\n"

exit 0
