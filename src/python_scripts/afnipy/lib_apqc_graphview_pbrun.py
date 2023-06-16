#!/usr/bin/env python

# This library contains functions for creating a Graph View driver
# script for the AP results directory.
#
auth = 'PA Taylor'
ver = 1.4 # date: June 9, 2023
# + create this script to be a general Graph View script for all pb*HEAD 
#   files.  It requires cmd line args to run, but it can be used by the
#   APQC HTML relatively conveniently
#
#########################################################################


import os, copy
import sys
import glob
import json
import subprocess

# ----------------------------------------------------------------------

DEF_scriptname = 'run_graphview_pbrun.tcsh'        # output file, tcsh script

# ===========================================================================
# ===========================================================================
# main text defaults: most of the script is type-cast

text_gv_top     = """#!/bin/tcsh

# This script was created by the afni_proc.py quality control (APQC)
# generator.  
#
# Its purpose is to facilitate investigating the properties of time
# series data, using the AFNI GUI's Graph Viewer functionality.  
#
# As described in the popup help, users should just need to hold down
# the Ctrl+Shift keys and then left-click and move the mouse around
# (dragging or re-clicking).  Watch the correlation patterns to that
# seed location change, and this often provides an excellent way to
# understand the data.
#
# In this script, one *must* provide 2 command line args: 
# + a pb label (pb00, pb01, etc.), 
# + a run number (r01, r02, r03, etc.).
#
# Additionally, one *can* also add three numbers on the command line
# to represent the starting location (RAI coordinate notation) of the 
# initial seed.

# ver = {ver}
# -------------------------------------------------------------------------

""".format(ver=ver)

# ... and functions to define a few quantities are given below:
#     make_apqc_gv_*( ... ) ...

text_gv_bot = """

set pb  = "$1"
set run = "$2"

if ( "${run}" == "" ) then
    echo "** Exiting: this program requires 2 cmd line args to run:"
    echo "   + a pb label (pb00, pb01, etc.)"
    echo "   + a run number (r01, r02, r03, etc.)."
    echo "   Additionally, you can then put 3 numbers as an initial"
    echo "   seed location coordinate"
    exit 1
else
    set ic_label = "${pb} ${run}"
endif

# ----- find main dset 
set dset_ulay = `find . -maxdepth 1 -name "${pb}.*.${run}.*.HEAD" | cut -b3-`

if ( ${#dset_ulay} == 0 ) then
    echo "** Exiting: could not find dset: ${pb}.*.${run}.*.HEAD"
    exit 1
else if ( ${#dset_ulay} == 1 ) then
    echo "++ Found ulay dset: ${dset_ulay}"
else
    echo "** Exiting: too many (${#dset_ulay}) dsets: ${pb}.*.${run}.*.HEAD"
    exit 1
endif

# ----- find associated vline file, if exists
set dset_vline = ""
set dir_vline  = `find . -maxdepth 1 -type d        \\
                      -name "vlines.${pb}.*"        \\
                      | cut -b3-`
if ( ${#dir_vline} == 1 ) then
    set dset_vline  = `find ./${dir_vline} -maxdepth 1 -type f        \\
                           -name "var.1.*${run}*"                     \\
                           | cut -b3-`
endif

# ----- find associated radcor file, if exists
set dset_radcor = ""
set dir_radcor  = `find . -maxdepth 1 -type d       \\
                     -name "radcor.${pb}.*"         \\
                     | cut -b3-`
if ( ${#dir_radcor} == 1 ) then
    set dset_radcor  = `find ./${dir_radcor} -maxdepth 1 -type f     \\
                            -name "radcor.*.${run}*HEAD"             \\
                            | cut -b3-`
endif

# ----- make ordered list of dsets to load
set all_load  = ( "${dset_ulay}"                    \\
                   *.${run}.*HEAD                   \\
                   ${dset_vline} ${dset_radcor}     \\
                   *.HEAD *.nii* )

# ----- finalize remaining parameters

# possible starting seed coordinate (in RAI notation)
set xcoor = "$3"
set ycoor = "$4"
set zcoor = "$5"

if ( "${zcoor}" != "" ) then
    set coord = ( "${xcoor}" "${ycoor}" "${zcoor}" )
else
    set coord = `3dinfo -dc3 "${dset_ulay}"`
endif

# ===========================================================================
# general GUI parameters

setenv AFNI_ENVIRON_WARNINGS   NO
setenv AFNI_THRESH_INIT_EXPON  0
setenv AFNI_NOSPLASH           YES
setenv AFNI_SPLASH_MELT        NO
setenv AFNI_STARTUP_WARNINGS   NO
setenv AFNI_NIFTI_TYPE_WARN    NO
setenv AFNI_NO_OBLIQUE_WARNING YES
setenv AFNI_COMPRESSOR         NONE
setenv AFNI_NEVER_SAY_GOODBYE  YES
setenv AFNI_MOTD_CHECK         NO
setenv AFNI_VERSION_CHECK      NO
setenv AFNI_IMAGE_DATASETS     NO

# graph specific parameters (faster to do here than with driving)

setenv AFNI_graph_width     800  # initial width of graph window
setenv AFNI_graph_height    500  # initial height of graph window
setenv AFNI_graph_matrix    5    # initial number of sub-graphs

# GUI visualization parameters

set crossh      = MULTI
set xh_gap      = -1
set OW          = "OPEN_WINDOW"
set graxis      = "axial"

# port communication
set portnum = `afni -available_npb_quiet`

# ===========================================================================

afni -q  -no_detach                                                     \\
    -npb ${portnum}                                                     \\
     -com "SWITCH_UNDERLAY    ${dset_ulay}"                             \\
     -com "SET_XHAIRS         ${crossh}"                                \\
     -com "SET_XHAIR_GAP      ${xh_gap}"                                \\
     -com "$OW sagittalimage"                                           \\
     -com "$OW ${graxis}graph"                                          \\
     ${all_load:q}  &

sleep 1

set l = `prompt_popup -message \\
"   View graphs+images of AP results data:  ${ic_label}\\n\\n\\
\\n\\
Initial ulay dataset : ${dset_ulay}\\n\\
Initial graph shown  : ${graxis}\\n\\
\\n\\
Some useful graph keyboard shortcuts:\\n\\
+ g/G = decrease/increase vertical grid spacing\\n\\
+ m/M = reduce/increase matrix size of sub-graphs by 1  \\n\\
+ v/V = 'video' scroll forward/backward in time\\n\\
+ spacebar = pause scrolling\\n\\
+ a/A = autoscale graphs once/always\\n\\
+ z/Z = change slice number down/up by 1\\n\\
+ -/+ = scale graphs down/up vertically\\n\\
+ S   = save view of graph to image file\\n\\
+ w   = write highlighted time series as *.1D text file  \\n\\
\\n" \\
-b '          Done - Close AFNI GUI          '`


if ("$l" != "1") then
    echo "+* Warn: AFNI Graph Viewer guidance message failed to open"
endif

@Quiet_Talkers -npb_val ${portnum}

cat << EOF
===========================================
++ Goodbye, and thank you for graph viewing.

EOF
exit 0

"""

# ===========================================================================
# ===========================================================================

def make_apqc_graphview_script( ):
    """Make the full text (string) of the Graph Viewing script

    Parameters
    ----------
    (none)

    Return
    ------
    otxt  : str
            the tcsh-syntax string (=full script) for the Graph Viewing
            run script

    """

    # start of script text, default text (above)
    otxt = text_gv_top

    # and finish, with default text (above)
    otxt+= text_gv_bot

    return otxt

def write_apqc_graphview_script(pname = '' ):
    """Write out the text file of the Graph View script.

Parameters
----------
pname : str
    optional path name to prepend to the default filename

Return
------
okay : int
    success or not of writing file: 0 = success, else = failure

    """

    # get text of script
    otext = make_apqc_graphview_script()

    # write the text file in the results directory
    ofile = ''
    if pname :
        ofile = pname + '/'
    ofile+= DEF_scriptname

    fff = open(ofile, 'w')
    fff.write(otext)
    fff.close()
    
    # make executable, a la rcr
    try: code = eval('0o755')
    except: code = eval('0755')
    try:
        os.chmod(ofile, code)
    except:
        omsg = "failed: chmod {} {}".format(code, ofile)
        print(omsg)
        return 1

    msg = '''++ Done making (executable) InstaCorr script: 
    {}
    '''.format(ofile)
    print( msg )

    return 0

    
