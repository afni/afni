#!/bin/tcsh

# This is an example of combining the 5 bricks for the @SSwarper
# template
#
# Here, we take a GM mask from FS parcellation and bring it to the
# present grid; inflate it by one within the present grid.
#
# ====================================================================

set rset  = TT_N27+tlrc.
set ipref = BRICK
set oset  = TT_N27_SSW.nii.gz

set tpref = _tmp_bricks

# --------------------------------------------------------------

# concatenate volumes
set fin = ${tpref}_cat.nii.gz
3dTcat -prefix ${fin} ${ipref}*

# ----------- check dset max values ------------------------------

# Here, we are making a short dset. This requires that the range of
# values be [0, 32767]; if not, something would need to be done to
# scale a volume(s) down.  A check like this could also be performed
# in earlier scripts...
set maxind = `3dinfo -nvi ${fin}`

echo "++ Checking max values of each brick:"
set allval = ()
set maxval = 0
foreach ii ( `seq 0 1 $maxind` )
    set mm    = `3dinfo -dmax "${fin}[$ii]"`
    set allval = ( $allval $mm )
    set isbig = `echo "$maxval < $mm" | bc`
    if ( $isbig == 1 ) then
        set maxval = "$mm"
    endif
    printf "   [%d]  %10.5f \n" ${ii}  $mm
end    

set badind = ()
foreach ii ( `seq 0 1 $maxind` )
    set mm    = `3dinfo -dmax "${fin}[$ii]"`
    set istoobig = `echo "$mm > 32767" | bc`
    if ( $istoobig == 1 ) then
        set badind = ( $badind $ii )
    endif
end
if ( $#badind > 0 ) then
    echo "** ERROR! These volumes have too large of values to be shorts:"
    foreach ii ( `seq 1 1 ${#badind}` ) 
        printf "   [%d]  %10.5f \n" ${badind[${ii}]}  ${allval[${ii}]}
    end
    echo "** Those would need to be scaled down below 32768."
    exit 1
else
    echo "++ Good:  no values appear to be too big for shorts."
endif

# ----------------- make final dset -------------------------

# If we've made it this far, make final output as unscaled shorts.
3dcalc                              \
    -a ${fin}                       \
    -expr 'a'                       \
    -prefix ${oset}                 \
    -datum short                    \
    -nscale

# --------------------------------------------------------------

# clean up
\rm ${tpref}*

echo "\n\n++ Done, final target volume is: $oset\n\n"

exit 0
