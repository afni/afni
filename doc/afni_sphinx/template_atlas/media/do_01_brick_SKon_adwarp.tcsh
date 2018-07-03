#!/bin/tcsh

# This is an example of making the [1] brick for the @SSwarper target
# volume, which is a whole head (including skull).  
#
# In the present case, this involves taking the original "colin" brain
# and applying the 12 "Talairach" warps that are stored in the
# TT_N27's header to the original.  After this step, the dsets align,
# and it is mostly a matter of scaling the *veeery* large values
# (>10**6 !) of the original "colin" brain down to something
# reasonably matching the TT_N27.  This is likely a pretty special
# case for this process of making an @SSwarper template target -- in
# most cases of making an @SSwarper template, this will *not* be
# necessary.
#
# ver: 1.0
# dat: 2018-06-06
# written by PA Taylor, DR Glen
#
# --------------------------------------------------------------------

# INPUTS: Datasets that are assumed to be in the present directory;
# user could adjust the locations+assignments, if desired.
set rset  = TT_N27+tlrc.
set iset  = colin27_t1_tal_lin.nii
set imask = colin27_t1_tal_lin_headmask.nii

set tpref = _tmp_brick1
set blab  = "SKon"

# -------------------------------------------------------------------

# make just a head-only mask to start
set fin = ${tpref}_mskd.nii.gz
3dcalc                                  \
    -a $iset                            \
    -b $imask                           \
    -expr 'a*step(b)'                   \
    -prefix $fin

# oddly, adwarp doesn't output NIFTIs... cobble together the output
# name
set ipref = `3dinfo -prefix_noext $iset`
set ravsp = `3dinfo -av_space $rset`
set apref = ${tpref}_adwrpd
set aset  = ${apref}${ravsp}

adwarp                          \
    -apar   $rset               \
    -prefix $apref              \
    -dpar   $fin 

# scale the dset because the original colin sets have values > 10**6 !
set ratset = ${tpref}_diff.nii.gz
3dcalc                                 \
    -a $aset                           \
    -b $rset                           \
    -expr 'a/b'                        \
    -prefix $ratset
set vvv = `3dBrickStat                 \
                -percentile 50 1 50    \
                -non-zero              \
                -mask $ratset          \
                $ratset`
set scal = $vvv[2]

# $fout will be brick [1]
set fout = ${tpref}_scld.nii.gz
3dcalc                                 \
    -a      $aset                      \
    -expr   "a/$scal"                  \
    -prefix $fout

# Frankly, I don't know why this shift is necessary, but it appears to
# be so.
set fin    = $fout
set fout   = "BRICK1_${blab}.nii.gz"
set fout1d = shift_BRICK1.aff12.1D
3dAllineate                            \
    -lpa                               \
    -source_automask                   \
    -autoweight                        \
    -warp shift_only                   \
    -1Dmatrix_save  $fout1d            \
    -prefix         $fout              \
    -final          wsinc5             \
    -base           $rset              \
    -source         $fin

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# clean up
\rm ${tpref}*

echo "\n\n++ Done.  For brick [1]:  $fout\n\n"

exit 0
