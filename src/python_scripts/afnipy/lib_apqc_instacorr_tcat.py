#!/usr/bin/env python

# This library contains functions for creating an InstaCorr driver
# script for the AP results directory.
#
auth = 'PA Taylor'
# ver : 0.1 || date: Oct 5, 2022
# + some level of written-ness
#
#ver = 1.1 # date: Oct 16, 2022
# + allow 1D files (e.g., ideal stim files) and plugins in the GUI
#
#ver = 1.2 # date: Nov 15, 2022
# + tweak pop-up msg text and minor function fix
#
ver = 1.3 # date: June 5, 2023
# + add in ability to take 3 cmd line args to represent initial seed loc
#
#########################################################################


import os, copy
import sys
import glob
import json
import subprocess

from afnipy import lib_apqc_tcsh       as lat

# ----------------------------------------------------------------------

scriptname = 'run_instacorr_tcat.tcsh'         # output file, tcsh script

# ===========================================================================
# ===========================================================================
# main text defaults: most of the script is type-cast

text_ic_top     = """#!/bin/tcsh

# This script was created by the afni_proc.py quality control (APQC)
# generator.  
#
# It's purpose is to facilitate investigating the properties of the
# raw/unprocessed input data, using the AFNI GUI's InstaCorr functionality.  
#
# As described in the popup help, users should just need to hold down
# the Ctrl+Shift keys and then left-click and move the mouse around
# (dragging or re-clicking).  Watch the correlation patterns to that
# seed location change, and this often provides an excellent way to
# understand the data.
#
# Now, one can also provide three numbers on the command line to represent
# the starting location (RAI coordinate notation) of the initial seed.

# ver = {ver}
# -------------------------------------------------------------------------

""".format(ver=ver)

# ... and functions to define a few quantities are given below:
#     make_apqc_ic_*( ... ) ...

text_ic_bot = """

# possible starting seed coordinate (in RAI notation)
set xcoor = "$1"
set ycoor = "$2"
set zcoor = "$3"

if ( "${zcoor}" != "" ) then
    set coord = ( "${xcoor}" "${ycoor}" "${zcoor}" )
else
    set coord = `3dinfo -dc3 "${ic_dset}"`
endif

set voxvol      = `3dinfo -voxvol "${ic_dset}"`
set ic_seedrad  = `echo "${voxvol}"                                      \\
                        | awk '{printf "%0.2f",(2*($1)^0.3334);}'`
echo "++ seedcorr radius: ${ic_seedrad}"
set ic_blur     = `echo "${voxvol}"                                      \\
                        | awk '{printf "%0.2f",(1.5*($1)^0.3334);}'`
echo "++ blurring radius: ${ic_blur}"

# ===========================================================================
# parameters set by default

setenv AFNI_THRESH_INIT_EXPON  0
setenv AFNI_NOSPLASH           YES
setenv AFNI_SPLASH_MELT        NO
setenv AFNI_STARTUP_WARNINGS   NO
setenv AFNI_NIFTI_TYPE_WARN    NO
setenv AFNI_NO_OBLIQUE_WARNING YES

# InstaCorr parameters

set ic_ignore   = 0
set ic_blur     = ${ic_blur}           # bc the data be unprocessed
set ic_automask = no
set ic_despike  = no
set ic_bandpass = 0,99999
set ic_polort   = 3                    # bc the data be unprocessed
set ic_method   = P

# GUI visualization parameters

set pbar_sign   = "-"
set ncolors     = 99
set topval      = 0.6
set cbar        = "Reds_and_Blues_Inv"
set olay_alpha  = "Quadratic"
set olay_boxed  = "Yes"
set thresh      = 0.3
set frange      = ${topval}
set crossh      = MULTI
set xh_gap      = -1
set opacity     = 7
set OW          = "OPEN_WINDOW"

# port communication
set portnum = `afni -available_npb_quiet`

# ===========================================================================

afni -q  -no_detach                                                     \\
    -npb ${portnum}                                                     \\
     -com "SWITCH_UNDERLAY    ${dset_ulay}"                             \\
     -com "INSTACORR INIT                                               \\
                     DSET=${ic_dset}                                    \\
                   IGNORE=${ic_ignore}                                  \\
                     BLUR=${ic_blur}                                    \\
                 AUTOMASK=${ic_automask}                                \\
                  DESPIKE=${ic_despike}                                 \\
                 BANDPASS=${ic_bandpass}                                \\
                   POLORT=${ic_polort}                                  \\
                  SEEDRAD=${ic_seedrad}                                 \\
                   METHOD=${ic_method}"                                 \\
     -com "INSTACORR SET      ${coord} J"                               \\
     -com "SET_THRESHNEW      ${thresh}"                                \\
     -com "SET_PBAR_ALL       ${pbar_sign}${ncolors} ${topval} ${cbar}" \\
     -com "SET_FUNC_RANGE     ${frange}"                                \\
     -com "SET_XHAIRS         ${crossh}"                                \\
     -com "SET_XHAIR_GAP      ${xh_gap}"                                \\
     -com "SET_FUNC_ALPHA     ${olay_alpha}"                            \\
     -com "SET_FUNC_BOXED     ${olay_boxed}"                            \\
     -com "$OW sagittalimage  opacity=${opacity}"                       \\
     ${all_load:q}  &

sleep 1

set l = `prompt_user -pause \\
"      Run InstaCorr on the initial (tcat) dataset\\n\\n\\
\\n\\
InstaCorr calc using : ${ic_dset}\\n\\
Initial ulay dataset : ${dset_ulay}\\n\\
\\n\\
Wait briefly for the initial correlation patterns to appear.\\n\\
\\n\\
To use InstaCorr:\\n\\
Hold down Ctrl+Shift, and Left-click anywhere in the dataset.\\n\\
You can hold down Left-click and drag the cursor around, too.\\n\\
\\n\\
You will see the (unmasked) wholebrain correlation patterns\\n\\
from each clicked 'seed' location, updating instantly.\\n\\
Transparent+boxed thresholding is ON (via 'A' and 'B' buttons).\\n\\
\\n\\
To jump to particular coordinates:\\n\\
+ Right-click -> 'Jump to (xyz)' \\n\\
+ Enter 3 space-separated coords\\n\\
+ Then, Right-click -> 'InstaCorr set',\\n\\
  or use standard Ctrl+Shift and Left-click.\\n\\
\\n\\
When done, hit 'OK' to exit.\\n"`


if ("$l" != "1") then
    echo "+* Warn: InstaCorr guidance message failed to open"
endif

@Quiet_Talkers -npb_val ${portnum}

cat << EOF
===========================================
++ Goodbye, and thank you for InstaCorring.

EOF
exit 0

"""

# ===========================================================================
# ===========================================================================
# 

def make_apqc_ic_ulay_and_ic_dset( ap_ssdict ):
    """Decide which 3D or 4D dataset should be the underlay in the APQC
    run_instacorr* script.  This just involves going through a list of
    possible items, in order.  

    This dataset will be both the ulay and olay.

    Parameters
    ----------
    ap_ssdict : dict
                the dictionary of 'uvar' elements for a given subject

    Return
    ------
    otxt  : str
                the tcsh-syntax string that sets the variable with the
                chosen dset's value.
    """

    # default return, if error exiting
    BAD_RETURN = ""

    # list of uvars, in decreasing order of preference
    ldep = ['tcat_dset']
    if lat.check_dep(ap_ssdict, ldep) :
        otxt  = """set dset_ulay = "{}"\n""".format(ap_ssdict[ldep[0]])
    else:
        all_pb00 = glob.glob('pb00*HEAD')
        all_pb00.sort()
        if not(len(all_pb00)) :
            print("** ERROR: Could not find ulay for instacorr tcat?")
            return BAD_RETURN
        otxt  = """set dset_ulay = "{}"\n""".format(all_pb00[0])

    otxt+= """set ic_dset   = "${dset_ulay}"\n"""

    return otxt

def make_apqc_ic_all_load( ap_ssdict, do_check_vlines=True ):
    """All results directory dsets get loaded in, to be available to be
used.  We build the list order so that the most useful to explore with
InstaCorr are first, and then everything follows in alphabetical
order.  This just involves going through a list of possible items, in
order.

    Parameters
    ----------
    ap_ssdict : dict
                the dictionary of 'uvar' elements for a given subject
    do_check_vline : bool
                check if the vlines_tcat_dir exists, and then load its
                contents as well

    Return
    ------
    otxt  : str
                the tcsh-syntax string that sets the variable with the
                list.

    """

    # default return, if error exiting
    BAD_RETURN = ""

    # initialize---even though the ulay and ic_dset here are likely to
    # be same file, leave script like this in case they are not (in
    # the future)
    otxt = """\n"""
    otxt+= """set all_load  = ( "${dset_ulay}" "${ic_dset}"        \\\n"""

    # indentation index for below
    nindent = otxt.index('(') + 2
    nfinal  = otxt.index('\\') - 1

    list_ldep = ['vlines_tcat_dir']
    if do_check_vlines and lat.check_dep(ap_ssdict, list_ldep) :
        vline_dsets = ap_ssdict[list_ldep[0]] + '/' + '*.nii*'
        # don't put quotes if using wildcard
        line = '''{}{} '''.format(' '*nindent, vline_dsets)
        npad = nfinal - len(line)
        if npad > 0 :
            line+= ' '*npad
        line+= '''\\\n'''
        otxt+= line
    
    if len(glob.glob('pb00*HEAD')) > 1 :
        # don't put quotes if using wildcard
        line = '''{}{} '''.format(' '*nindent, 'pb00*HEAD' )
        npad = nfinal - len(line)
        if npad > 0 :
            line+= ' '*npad
        line+= '''\\\n'''
        otxt+= line

    otxt+= """{}*.HEAD *.nii* )""".format(' '*nindent)
    otxt+= """\n"""

    return otxt

def make_apqc_ic_script( ap_ssdict ):
    """Make the full text (string) of the InstaCorr script

    Parameters
    ----------
    ap_ssdict : dict
                the dictionary of 'uvar' elements for a given subject

    Return
    ------
    otxt  : str
                the tcsh-syntax string (=full script) for the InstaCorr
                run script

    """

    # start of script text, default text (above)
    otxt = text_ic_top

    # get the text for the underlay and "input" dset for InstaCorr
    text_ulay_and_ic = make_apqc_ic_ulay_and_ic_dset( ap_ssdict )

    # ... and make list of all dsets to load
    text_all_load = make_apqc_ic_all_load( ap_ssdict )

    otxt+= text_ulay_and_ic
    otxt+= text_all_load

    # and finish, with default text (above)
    otxt+= text_ic_bot

    return otxt
