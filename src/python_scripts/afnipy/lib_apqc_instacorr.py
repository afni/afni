#!/usr/bin/env python

# This library contains functions for creating an InstaCorr driver
# script for the AP results directory.
#
auth = 'PA Taylor'
# ver : 0.1 || date: Oct 5, 2022
# + some level of written-ness
#
ver = 1.1 # date: Oct 16, 2022
# + allow 1D files (e.g., ideal stim files) and plugins in the GUI
#
#########################################################################


import os, copy
import sys
import glob
import json
import subprocess

from afnipy import lib_apqc_tcsh       as lat

# ----------------------------------------------------------------------

scriptname = 'run_instacorr_errts.tcsh'         # output file, tcsh script

# ===========================================================================
# ===========================================================================
# main text defaults: most of the script is type-cast

text_ic_top     = """#!/bin/tcsh

# This script was created by the afni_proc.py quality control (APQC)
# generator.  
#
# It's purpose is to facilitate investigating the properties of the
# processing's residual dataset (errts*HEAD) file, by using the AFNI
# GUI's InstaCorr functionality.  As described in the popup help,
# users should just need to hold down the Ctrl+Shift keys and then
# left-click and move the mouse around (dragging or re-clicking).
# Watch the correlation patterns to that seed location change, and
# this often provides an excellent way to understand the data.
#
# ver = {ver}
# -------------------------------------------------------------------------

""".format(ver=ver)

# ... and functions to define a few quantities are given below:
#     make_apqc_ic_*( ... ) ...

text_ic_bot = """

set coord       = `3dinfo -dc3 "${ic_dset}"`

set voxvol      = `3dinfo -voxvol "${ic_dset}"`
set ic_seedrad  = `echo "${voxvol}"                                      \\
                        | awk '{printf "%0.2f",(2*($1)^0.3334);}'`
echo "++ seedcorr radius: ${ic_seedrad}"

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
set ic_blur     = 0
set ic_automask = no
set ic_despike  = no
set ic_bandpass = 0,99999
set ic_polort   = -1
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
"      Run InstaCorr on the residuals (errts) dataset\\n\\n\\
\\n\\
InstaCorr calc using : ${ic_dset}\\n\\
Initial ulay dataset : ${dset_ulay}\\n\\
\\n\\
Wait briefly for the initial correlation patterns to appear.\\n\\
\\n\\
Hold down Ctrl+Shift, and Left-click anywhere in the dataset,\\n\\
and/or hold down the Left-click and drag the cursor around.\\n\\
\\n\\
You will see the (unmasked) wholebrain correlation patterns\\n\\
from each clicked "seed" location, updating instantly.\\n\\
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

def make_apqc_ic_ulay( ap_ssdict ):
    """Decide which 3D or 4D dataset should be the underlay in the APQC
    run_instacorr* script.  This just involves going through a list of
    possible items, in order.

    Parameters
    ----------
    ap_ssdict : dict
                the dictionary of 'uvar' elements for a given subject

    Return
    ------
    ouvar : str
                the uvar of the chosen dset
    otxt  : str
                the tcsh-syntax string that sets the variable with the
                chosen uvar's value.
    """

    # default return, if error exiting
    BAD_RETURN = "", ""

    # list of uvars, in decreasing order of preference
    list_ldep = ['final_anat', 'vr_base_dset', 'errts_dset']

    # initialize
    ouvar = ''
    otxt  = """set dset_ulay = """

    for ldep in list_ldep:
        if lat.check_dep(ap_ssdict, [ldep]) :
            ouvar = ldep
            otxt += '''"{}"'''.format(ap_ssdict[ldep])
            break
    otxt+= """\n"""

    return ouvar, otxt

def make_apqc_ic_dset( ap_ssdict ):
    """Decide which 4D dataset should be used to generate the correlations
    in the APQC run_instacorr* script.  This just involves going
    through a list of possible items, in order.

    Parameters
    ----------
    ap_ssdict : dict
                the dictionary of 'uvar' elements for a given subject

    Return
    ------
    ouvar : str
                the uvar of the chosen dset
    otxt  : str
                the tcsh-syntax string that sets the variable with the
                chosen uvar's value.
    """

    # default return, if error exiting
    BAD_RETURN = "", ""

    # list of uvars, in decreasing order of preference
    list_ldep = ['errts_dset']

    # initialize
    ouvar = ''
    otxt  = """set ic_dset   = """

    for ldep in list_ldep:
        if lat.check_dep(ap_ssdict, [ldep]) :
            ouvar = ldep
            otxt += '''"{}"'''.format(ap_ssdict[ldep])
            break
    otxt+= """\n"""

    return ouvar, otxt

def make_apqc_ic_all_load( ap_ssdict, skip_uvars=None ):
    """All results directory dsets get loaded in, to be available to be
used.  We build the list order so that the most useful to explore with
InstaCorr are first, and then everything follows in alphabetical
order.  This just involves going through a list of possible items, in
order.

    Parameters
    ----------
    ap_ssdict : dict
                the dictionary of 'uvar' elements for a given subject
    skip_uvars: list
                list of uvars to skip (because they might already be 
                set as $dset_ulay and/or $ic_dset).

    Return
    ------
    otxt  : str
                the tcsh-syntax string that sets the variable with the
                chosen uvar's value.

    """

    # default return, if error exiting
    BAD_RETURN = "", ""

    # list of uvars, in decreasing order of preference
    list_ldep = ['anat_final', 'vr_base_dset', 
                 'errts_dset', 'mask_dset']

    # initialize
    otxt = """\n"""
    otxt+= """set all_load  = ( "${dset_ulay}" "${ic_dset}"        \\\n"""
    # indentation index for below
    nindent = otxt.index('(') + 2
    nfinal  = otxt.index('\\') - 1

    for ldep in list_ldep:
        if lat.check_dep(ap_ssdict, [ldep]) :
            if skip_uvars and not(ldep in skip_uvars) :
                line = '''{}"{}" '''.format(' '*nindent, ap_ssdict[ldep])
                npad = nfinal - len(line)
                if npad > 0 :
                    line+= ' '*npad
                line+= '''\\\n'''
                otxt+= line
    
    otxt+= """{}all_runs.*.HEAD *.HEAD *.nii* )""".format(' '*nindent)
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
    uvar_ulay, text_ulay = make_apqc_ic_ulay( ap_ssdict )
    uvar_dset, text_dset = make_apqc_ic_dset( ap_ssdict )

    # ... and make list of already-loaded dsets, which don't need to
    # be listed again in the all_load var
    list_skip     = [uvar_ulay, uvar_dset]
    text_all_load = make_apqc_ic_all_load( ap_ssdict,
                                           skip_uvars = list_skip )

    otxt+= text_ulay
    otxt+= text_dset
    otxt+= text_all_load

    # and finish, with default text (above)
    otxt+= text_ic_bot

    return otxt
