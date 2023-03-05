#!/bin/tcsh

set version   = "0.0";  set rev_dat   = "Feb 1, 2023"
# + better template for scripting
#
# ----------------------------------------------------------------

set this_prog = "THIS_PROG"
#set tpname    = "${this_prog:gas///}"
set here      = $PWD

# ----------------- find AFNI and set viewer ---------------------

# find AFNI binaries directory and viewer location
set adir      = ""
which afni >& /dev/null
if ( $status ) then
    echo "** Cannot find 'afni' (?)."
    goto BAD_EXIT
else
    set aa   = `which afni`
    set adir = $aa:h
endif

# ----------------------- set defaults --------------------------

set input   = ""
set prefix  = ""

set odir    = $here
set opref   = ""

set wdir    = ""


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

    # --------- required

    if ( "$argv[$ac]" == "-input" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set ifile = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-prefix" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set prefix = "$argv[$ac]"
        set opref  = `basename "$argv[$ac]"`
        set odir   = `dirname  "$argv[$ac]"`


    # --------- opt

    if ( "$argv[$ac]" == "-workdir" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set wdir = "$argv[$ac]"

        set tf = `python -c "print('/' in '${wdir}')"`
        if ( "${tf}" == "True" ) then
            echo "** ERROR: '-workdir ..' is a name only, no '/' allowed\n"
            goto BAD_EXIT
        endif

    else if ( "$argv[$ac]" == "-no_clean" ) then
        set DO_CLEAN = 0
        
    else
        echo "\n\n** ERROR: unexpected option #$ac = '$argv[$ac]'\n\n"
        goto BAD_EXIT
        
    endif
    @ ac += 1
end

# =======================================================================
# ======================== ** Verify + setup ** =========================
# =======================================================================

if ( "${prefix}" = "" ) then
    echo "** ERROR: need to provide output name with '-prefix ..'"
    goto BAD_EXIT
endif

if ( "${input}" = "" ) then
    echo "** ERROR: need to provide input dataset with '-input ..'"
    goto BAD_EXIT
endif

# make workdir name, if nec
if ( "${wdir}" = "" ) then
    set tmp_code = `3dnewid -fun11`  # should be essentially unique hash
    set wdir     = __workdir_${this_prog}_${tmp_code}
endif

# check output directory, use input one if nothing given
if ( ! -e "${odir}" ) then
    echo "++ Making new output directory: $odir"
    \mkdir -p "${odir}"
endif

# make the working directory
if ( ! -e "${odir}/${wdir}" ) then
    echo "++ Making working directory: ${odir}/${wdir}"
    \mkdir -p "${odir}/${wdir}"
else
    echo "+* WARNING:  Somehow found a premade working directory (?):"
    echo "      ${odir}/${wdir}"
endif

# =======================================================================
# =========================== ** Main work ** ===========================
# =======================================================================





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
echo "++ DONE.  View the finished, axialized product:"
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
    echo "** ERROR: Missing an argument after option flag: '$argv[$ac]'"
    goto BAD_EXIT

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0
