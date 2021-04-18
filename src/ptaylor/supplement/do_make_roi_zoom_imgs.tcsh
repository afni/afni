#!/bin/tcsh

#set version   = "0.1";  set rev_dat   = "April 16, 2021"
# [PT] start of program
#    + used to take zoomed-in snapshots of TSNR in regions
#
set version   = "0.2";  set rev_dat   = "April 17, 2021"
# [PT] start of program
#    + generalize input a bit, and realizing more than TSNR can be
#      snapshotted... "tsnr" -> "olay", in most cases, excepting prog
#      names and example input file names
#
# ---------------------------------------------------------------------

set prog = "do_make_roi_zoom_imgs.tcsh" # who am i?

set full_dset_roi = ""       # dset<roi>: together in one, or...
set dset_roi  = ""           # dset
set roi       = ""           # roi
set dset_olay = ""            
set npad      = 2

set hot_bot    = 100     # the lower bound of hot; def for final TSNR
set hot_top    = 240     # upper bound of hot    ; def for final TSNR

set prefix    = ""

set pppp      = "`3dnewid -fun11`"
set wname     = __wdir_roi_zoom_imgs_${pppp}
set DO_CLEAN  = 1

# ---------------------------------------------------------------------
# names of dsets in wdir

set dset_abox    = dset_00_abox.nii.gz
set dset_olay_ZP = dset_01_olay_ZP.nii.gz
set dset_all_one = dset_02_all_one.nii.gz
set dset_thr     = dset_03_thr.nii.gz
set dset_tcat    = dset_04_tcat.nii.gz

# ====================================================================

if ( $#argv == 0 ) goto SHOW_HELP

set ac = 1
while ( $ac <= $#argv )
    # terminal options
    if ( ("$argv[$ac]" == "-h" ) || ("$argv[$ac]" == "-help" )) then
        goto SHOW_HELP
    endif
    if ( "$argv[$ac]" == "-ver" ) then
        goto SHOW_VERSION
    endif

    #  ---------- inputs: required ---------------

    if ( "$argv[$ac]" == "-dset_roi" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set dset_roi = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-name_roi" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set roi = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-full_dset_name_roi" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set full_dset_roi = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-dset_olay" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set dset_olay = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-prefix" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set prefix = "$argv[$ac]"

    # --------------- opt

    else if ( "$argv[$ac]" == "-npad" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set npad = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-cbar_hot_range" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set hot_bot = "$argv[$ac]"
        @ ac += 1
        set hot_top = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-no_clean" ) then
        set DO_CLEAN = 0

    else if ( "$argv[$ac]" == "-echo" ) then
        set echo

    # ------- end

    else
        echo "\n\n** ERROR: unexpected option #$ac = '$argv[$ac]'\n\n"
        goto BAD_EXIT
        
    endif
    @ ac += 1
end

# =======================================================================

if ( "${dset_roi}" != "" && "${roi}" != "" ) then
    set full_dset_roi = "${dset_roi}<${roi}>"
    echo "++ combining dset and name: ${full_dset_roi}"
else if ( "${full_dset_roi}" != "" ) then
    echo "++ using already-combined dset and name: ${full_dset_roi}"
else
    echo "** ERROR: use '-dset_roi ..' and '-name_roi ..'"
    echo "   OR"
    echo "   '-full_dset_name_roi ..'"
    goto BAD_EXIT
endif

if ( "${dset_olay}" == "" ) then
    echo "** ERROR: use '-dset_olay ..'"
    goto BAD_EXIT
endif

if ( "$prefix" == "" ) then
    echo "** ERROR: need '-prefix ...' option provided."
    echo "   See the helpfile for more information."
    goto BAD_EXIT
else
    set odir  = `dirname  $prefix`
    set opref = `basename $prefix`
    echo ""
    echo "++ Based on prefix, the output directory will be:"
    echo "     $odir"
    echo "++ Based on prefix, the output prefix will be:"
    echo "     $opref"
    echo ""
endif

# name and make working dir
set wdir = ${odir}/${wname} 
\mkdir -p ${wdir}

# these will be output from wdir, so just place "up"
set oimg         = ../${opref}
set opbarrt      = ../${opref}.cbar

# =======================================================================

3dAutobox                                 \
    -overwrite                            \
    -noclust                              \
    -prefix  "${wdir}/${dset_abox}"       \
    -npad    "${npad}"                    \
    -input   "${full_dset_roi}"

3dZeropad                                   \
    -overwrite                              \
    -master "${wdir}/${dset_abox}"          \
    -prefix "${wdir}/${dset_olay_ZP}"       \
    "${dset_olay}"

# now can work in wdir
cd ${wdir}

3dcalc                                         \
    -overwrite                                 \
    -a      "${dset_abox}"                     \
    -expr   '1'                                \
    -prefix "${dset_all_one}"

3dcalc                                         \
    -overwrite                                 \
    -a      "${dset_abox}"                     \
    -expr   'step(a) + not(a)*0.5'             \
    -prefix "${dset_thr}"

# TR just to silence warning
3dTcat                                      \
    -overwrite                              \
    -tr 1                                   \
    -prefix "${dset_tcat}"                  \
    "${dset_olay_ZP}" "${dset_thr}"

set center_coor = `3dCM "${dset_all_one}"`

# --------------------------------------------------------------------

adjunct_apqc_tsnr_general                \
    -ulay         "${dset_abox}"         \
    -olay         "${dset_tcat}"         \
    -focus        "${dset_all_one}"      \
    -prefix       "${oimg}"              \
    -prefix_cbar  "${opbarrt}"           \
    -olay_disc_hot_range  "${hot_bot}" "${hot_top}"        \
    -set_dicom_xyz ${center_coor}        \
    -label_mode    0                     \
    -ulay_range    0 1                   \
    -set_subbricks 0 0 1                 \
    -olay_alpha    "Yes"                 \
    -olay_boxed    "No"                  \
    -thr_olay      0.85                  \
    -montgap       1                     \
    -save_ftype    PNG                   \
    -blowup        4                     \
    -montcolor     'black'               \
    -delta_slices  1 1 1                 \
    -opacity       9                     \
    -montx 3 -monty 1                    \

if ( $status ) then
    echo "** ERROR in adjunct_apqc_tsnr_general command"
    goto BAD_EXIT
endif

# ---------------------------------

if ( $DO_CLEAN ) then
    echo "++ Clean working dir:  ${wname}"
    cd ..
    \rm -rf ${wname}
endif

goto GOOD_EXIT

# ========================================================================
# ========================================================================

SHOW_HELP:
cat << EOF
-------------------------------------------------------------------------

Make zoomed-in images of region.

# Examples

# 1) quotes necessary around name here (at least around "<....>")

tcsh ${prog}  \
    -full_dset_name_roi "follow_ROI_FSall00+tlrc.HEAD<Left-Accumbens-area>" \
    -cbar_hot_range     100 240                                             \
    -dset_olay          TSNR.sub-22228_ses-02+tlrc.HEAD                     \
    -prefix             img_tsnr_Left-Accumbens-area_sub-22228_ses-02

# 2) exact alternative to above, roi info just provided separately

tcsh ${prog}  \
    -dset_roi       follow_ROI_FSall00+tlrc.HEAD                         \
    -name_roi       Left-Accumbens-area                                  \
    -cbar_hot_range 100 240                                              \
    -dset_olay      TSNR.sub-22228_ses-02+tlrc.HEAD                      \
    -prefix         img_tsnr_Left-Accumbens-area_sub-22228_ses-02

# 3) same as above just the other side o' the brain 

tcsh ${prog}  \
    -dset_roi       follow_ROI_FSall00+tlrc.HEAD                         \
    -name_roi       Right-Accumbens-area                                 \
    -cbar_hot_range 100 240                                              \
    -dset_olay      TSNR.sub-22228_ses-02+tlrc.HEAD                      \
    -prefix         img_tsnr_Right-Accumbens-area_sub-22228_ses-02

EOF

# ----------------------------------------------------------------------

    goto GOOD_EXIT

SHOW_VERSION:
   echo "version  $version (${rev_dat})"
   goto GOOD_EXIT

FAIL_MISSING_ARG:
    echo "** ERROR! Missing an argument after option flag: '$argv[$ac]'"
    goto BAD_EXIT

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0


