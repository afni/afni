#!/bin/tcsh

# check olay of mask extent: 
# + for full_mask* of epi
# + for skull-stripping of anatomical


set version   = "0.1";  set rev_dat   = "July 27, 2018"
#   + birth
#
# ----------------------------------------------------------------

set this_prog = "do_mask_check.tcsh"
set here      = $PWD

# ----------------------- set defaults --------------------------

if ( $#argv < 3 ) then
    echo "** ERROR: need 3 args!"
    goto BAD_EXIT
endif

set ulay    = "$1"
set olay    = "$2"
set opref   = "$3"

set tpref   = "__tmp"

set odir    = ${here}/QC

set DO_CLEAN  = 1                       # default: remove temp files

# ====================================================================

\mkdir -p $odir

@chauffeur_afni                             \
    -ulay  "${ulay}"                        \
    -olay  "${olay}"                        \
    -cbar "green_monochrome"                \
    -ulay_range 0% 150%                     \
    -opacity 2                              \
    -prefix   "$odir/${opref}"              \
    -montx 6 -monty 1                       \
    -set_xhairs OFF                         \
    -label_mode 1 -label_size 3             \
    -do_clean

# ---------------------------------------------------------------------

#if ( $DO_CLEAN == 1 ) then
#    echo "\n+* Removing temporary files '${tpref}*'\n"
#    \rm ${tpref}*
#else
#    echo "\n+* NOT removing temporary files '${tpref}*'\n"
#endif

goto GOOD_EXIT

# =======================================================================

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0
