#!/bin/tcsh

# This is an example of making the [1] brick for the @SSwarper target
# volume, which is a whole head (including skull).  
#
# In the present case, this involves aligning the desired, skull-less
# brain to a template that has a skull; then backprojecting that skull
# around the skull-less brain.  The @SSwarper script *does* need a
# brain with a skull for some initial alignment, but it shouldn't have
# to be veeery exact.  NB: the present approach can be considered ad
# hoc, and if you have a better one, by all means...
# 
# This might be a medium-special case for this process of making an
# @SSwarper template target -- if a copy of the dset with a skull *on*
# were alreayd present, as in several templates, then this step would
# not be necessary in the current form.
#
# ver: 1.0
# dat: 2018-07-05
# written by PA Taylor
#
# --------------------------------------------------------------------

# INPUTS: Datasets that are assumed to be in the present directory;
# user could adjust the locations+assignments, if desired.
set rset  = HaskinsPeds_NL_template1.0+tlrc.
# set with skull, same contrast as $rset
set skset = mni_icbm152_nlin_sym_09a/mni_icbm152_t1_tal_nlin_sym_09a.nii


set tpref = _tmp_brick1
set blab  = "SKon"

# -------------------------------------------------------------------

#  could make skullstripped version for registration purposes, but
#  this looks fine here as is.
set pout = ${tpref}_sk_to_ref.aff12.1D
set fout = ${tpref}_sk_in_ref.nii.gz
3dAllineate                                  \
    -twopass                                 \
    -cmass                                   \
    -lpa                                     \
    -autoweight                              \
    -source_automask                         \
    -base          $rset                     \
    -source        $skset                    \
    -1Dmatrix_save $pout                     \
    -prefix        $fout

# estimate a scale for the skull-bearing volume to be ~appropriate for
# the refset

set vvv = `3dBrickStat                 \
                -non-zero              \
                -percentile 10 1 10    \
                $rset`

set ratvol = ${tpref}_ratios.nii.gz
3dcalc                                 \
    -a      $rset                      \
    -b      $fout                      \
    -expr   "step(${vvv[2]}-a)*(b/a)"  \
    -prefix ${ratvol}

set uuu = `3dBrickStat        \
                -non-zero     \
                -median       \
                $ratvol`

echo "++ ratio is: ${uuu[2]}"

set fin  = $fout
set fout = "BRICK1_${blab}.nii.gz"
3dcalc                              \
    -a $rset                        \
    -b $fin                         \
    -expr "(b/${uuu[2]})*not(a)+a"  \
    -prefix $fout                   \
    -short

# use same brick labels as original MNI152_2009_template
3drefit -sublabel 0 "$blab" $fout

# clean up
\rm ${tpref}*

echo "\n\n++ Done.  For brick [1]:  $fout\n\n"

exit 0
