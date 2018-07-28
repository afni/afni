#!/bin/tcsh

# Applies to epi <-> anat, anat <-> temp, etc.


set version   = "0.1";  set rev_dat   = "July 27, 2018"
#   + birth
#
# ----------------------------------------------------------------

set this_prog = "do_align_check.tcsh"
set here      = $PWD

# ----------------------- set defaults --------------------------

set ulay    = "$1"
set olay    = "$2"
set opref   = "$3"

if ( $#argv < 3 ) then
    echo "** ERROR: need 3 args!"
    goto BAD_EXIT
endif


set tpref   = "__tmp"

set olayE   = ${tpref}_olayE.nii.gz

set odir    = ${here}/QC

set DO_CLEAN  = 1                       # default: remove temp files

# ====================================================================

\mkdir -p $odir

set idx  = 0

set fin  = "$olay"
set fout = "${tpref}_${idx}_res.nii"
3dAllineate \
    -input  "$fin"            \
    -master "$ulay"           \
    -prefix "$fout"           \
    -1Dparam_apply IDENTITY   \
    -final cubic
@ idx += 1

set mout = ${tpref}_${idx}_mask.nii
3dAutomask                             \
    -prefix ${mout}                    \
    "$fout"
@ idx += 1

set fin  = "$fout"
set fout = "${tpref}_${idx}_mfilt.nii"
3dMedianFilter                       \
    -irad 1.01                       \
    -iter 1                          \
    -prefix "$fout"                  \
    "$fin"
@ idx += 1

set fin  = "$fout"
set fout = "${tpref}_${idx}_edgy.nii"
3dedge3 -overwrite -prefix "${fout}" -input "${fin}"
@ idx += 1

set fin  = "$fout"
set fout = "${tpref}_${idx}_edge_mskd.nii"
3dcalc -a $mout -b $fin -expr 'a*b' -prefix "${fout}" 
@ idx += 1

set vvv  = `3dBrickStat -non-zero -percentile 1 1 1 ${fout}`
set ethr = $vvv[2]

# Like snapshot volreg
@chauffeur_afni                             \
    -ulay  "${ulay}"                        \
    -olay  "${fout}"                        \
    -ulay_range 0% 150%                     \
    -func_range_perc_nz 33                  \
    -cbar "Reds_and_Blues_Inv"              \
    -set_subbricks 0 0 0                    \
    -alpha_par Linear                       \
    -thr_olay $ethr                         \
    -opacity 5                              \
    -prefix   "$odir/${opref}"              \
    -montx 6 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean


# Another option
@chauffeur_afni                             \
    -ulay  "${ulay}"                        \
    -olay  "${fout}"                        \
    -ulay_range 0% 150%                     \
    -func_range_perc_nz 33                  \
    -cbar "Reds_and_Blues_Inv"              \
    -set_subbricks 0 0 0                    \
    -alpha_par Off                          \
    -thr_olay $ethr                         \
    -opacity 5                              \
    -prefix   "$odir/${opref}_B"            \
    -montx 6 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

# ---------------------------------------------------------------------

if ( $DO_CLEAN == 1 ) then
    echo "\n+* Removing temporary files '${tpref}*'\n"
    \rm ${tpref}*
else
    echo "\n+* NOT removing temporary files '${tpref}*'\n"
endif

goto GOOD_EXIT

# =======================================================================

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0
