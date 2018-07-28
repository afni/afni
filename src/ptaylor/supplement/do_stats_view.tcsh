#!/bin/tcsh

# view stats over anat_final or template
# (could easily do more to specify brick num, etc.)

set version   = "0.1";  set rev_dat   = "July 27, 2018"
#   + birth
#
# ----------------------------------------------------------------

set this_prog = "do_stats_view.tcsh"
set here      = $PWD

if ( $#argv < 3 ) then
    echo "** ERROR: need 3 args!"
    goto BAD_EXIT
endif

# ----------------------- set defaults --------------------------

set ulay    = "$1"
set olay    = "$2"
set imask   = "$3"
set opref   = "$4"

set ithr    = 0
set ibrik   = "[${ithr}]"

set tpref   = "__tmp"

set odir    = ${here}/QC

set DO_CLEAN  = 1                       # default: remove temp files

# ====================================================================

\mkdir -p $odir

set idx  = 0

# get 90 percentile for thresholding in afni GUI
set pp = ( `3dBrickStat -slow -percentile 90 1 90 \
            -mask "$imask" "${olay}${ibrik}"` )
set sthr = $pp[2]
echo -- thresholding F-stat at $sthr

# locate peak coords of biggest masked cluster and jump there
set fout = ${tpref}_${idx}_Fstat.nii
3dcalc                      \
    -a "${olay}${ibrik}"    \
    -b "$imask"             \
    -expr "a*b"             \
    -prefix $fout           \
    -overwrite
@ idx += 1

set maxcoords = ( \
    `3dClusterize                   \
        -orient RAI                 \
        -1sided RIGHT_TAIL $sthr    \
        -ithr $ithr                 \
        -idat 0                     \
        -NN 1                       \
        -inset $fout                \
        -quiet                      \
        | & awk '/^ / {print $14, $15, $16}' | head -n 1` )

echo -- jumping to max coords: $maxcoords

# If always using Fstat to show, then onesided colorbar makes sense...
set my_cbar = "Spectrum:yellow_to_red" #"Spectrum:yellow_to_red FLIP" # "Viridis"
set operc   = "99.5"

@chauffeur_afni                             \
    -ulay  ${ulay}                          \
    -olay  ${olay}                          \
    -cbar "$my_cbar"                        \
    -pbar_posonly                           \
    -ulay_range 0% 150%                     \
    -func_range_perc_nz "$operc"            \
    -thr_olay ${sthr}                       \
    -set_subbricks -1 $ithr $ithr           \
    -opacity 5                              \
    -prefix   "$odir/${opref}"              \
    -montx 6 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

@chauffeur_afni                             \
    -ulay  ${ulay}                          \
    -olay  ${olay}                          \
    -cbar "$my_cbar"                        \
    -pbar_posonly                           \
    -ulay_range 0% 150%                     \
    -func_range_perc_nz "$operc"            \
    -thr_olay ${sthr}                       \
    -set_subbricks -1 $ithr $ithr           \
    -alpha_par Linear                       \
    -opacity 5                              \
    -prefix   "$odir/${opref}_B"            \
    -montx 6 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

# jumping to max coor prob means we want to set delta slices, which
# could involve more intricate calcs
@chauffeur_afni                             \
    -ulay  ${ulay}                          \
    -olay  ${olay}                          \
    -cbar "$my_cbar"                        \
    -pbar_posonly                           \
    -ulay_range 0% 150%                     \
    -func_range_perc_nz "$operc"            \
    -set_dicom_xyz $maxcoords               \
    -delta_slices 5 5 5                     \
    -thr_olay ${sthr}                       \
    -set_subbricks -1 $ithr $ithr           \
    -opacity 5                              \
    -prefix   "$odir/${opref}_C"            \
    -montx 6 -monty 1                       \
    -set_xhairs SINGLE                      \
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
