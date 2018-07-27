#!/bin/tcsh

set version   = "0.0";  set rev_dat   = "- -, 2018"
#   + 
#
# ----------------------------------------------------------------

set this_prog = "****"
#set tpname    = "${this_prog:gas///}"
set here      = $PWD

# ----------------- find AFNI and set viewer ---------------------

# find AFNI binaries directory and viewer location
set adir      = ""
set my_viewer = ""
which afni >& /dev/null
if ( $status ) then
    echo "** Cannot find 'afni' (?!)."
    goto BAD_EXIT
else
    set aa   = `which afni`
    set adir = $aa:h
endif

# ----------------------- set defaults --------------------------

set ulay    = $1
set olay    = $2

set odir    = $here
set opref   = anatepi_al



set wdir    = 


set DO_CLEAN  = 1                       # default: keep working dir


# ------------------- process options, a la rr ----------------------

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

    # required
    if ( "$argv[$ac]" == "-input" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set ifile = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-prefix" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set opref = `basename "$argv[$ac]"`
        set odir  = `dirname  "$argv[$ac]"`


    else if ( "$argv[$ac]" == "-no_clean" ) then
        set DO_CLEAN = 0
        
    else
        echo "\n\n** ERROR: unexpected option #$ac = '$argv[$ac]'\n\n"
        goto BAD_EXIT
        
    endif
    @ ac += 1
end

# =======================================================================
# ============================ ** SETUP ** ==============================
# =======================================================================





# ===================== output dir + wdir =======================
 
# check output directory, use input one if nothing given

if ( ! -e $odir ) then
    echo "++ Making new output directory: $odir"
    \mkdir -p $odir
endif

# make the working directory
if ( ! -e $odir/$wdir ) then
    echo "++ Making working directory: $odir/$wdir"
    \mkdir -p $odir/$wdir
else
    echo "+* WARNING: Somehow found a premade working directory (?):"
    echo "      $odir/$wdir"
endif

# ====================================================================





# ---------------------------------------------------------------------

# move out of wdir to the odir
cd ..
set whereout = $PWD

if ( $DO_CLEAN == 1 ) then
    echo "\n+* Removing temporary axialization working dir: '$wdir'\n"

    # ***** clean

else
    echo "\n++ NOT removing temporary axialization working dir: '$wdir'\n"
endif

echo ""
echo "++ DONE! View the finished, axialized product:"
echo "     $whereout/$fout"
echo ""




goto GOOD_EXIT

# ========================================================================
# ========================================================================

SHOW_HELP:
cat << EOF
-------------------------------------------------------------------------



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
