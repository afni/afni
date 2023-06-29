#!/usr/bin/env python

# This library contains functions for creating either an InstaCorr
# (IC) or Graph View (GV) driver script for the AP results directory.
#
auth = 'PA Taylor'
ver = 2.0 
# [June 25, 2023] merged from earlier versions of script writing
#
#########################################################################


import os, copy
import sys
import glob
import json
import subprocess

from afnipy import lib_apqc_tcsh       as lat

# ========================================================================
# some simple mappings of names and abbrevs

DICT_proc_type = {
    'IC' : 'InstaCorr',
    'GV' : 'GraphView',
}

LIST_input_type = ['pbrun', 'errts']

##########################################################################

# some utility bits

# check valid proc_type: IC, GV, etc.
def is_valid_proc_type( proc_type, do_warn=1, do_exit=0 ):
    if proc_type in DICT_proc_type.keys() : 
        return True
    else: 
        if do_warn :
            print('+* WARN: unrecognized proc_type:', proc_type)
        if do_exit :
            sys.exit(3)
        return False

# check valid input_type: errts, pbrun, etc.
def is_valid_input_type( input_type, do_warn=1, do_exit=0 ):
    if input_type in LIST_input_type : 
        return True
    else: 
        if do_warn :
            print('+* WARN: unrecognized input_type:', input_type)
        if do_exit :
            sys.exit(3)
        return False

# single check for validity
def is_valid_proc_and_input(proc_type, input_type, do_warn=1 ):
    val = is_valid_input_type(input_type, do_warn=do_warn) 
    val = val and is_valid_proc_type(proc_type, do_warn=do_warn) 
    return val 

# check if ap_ssdict entered (needed sometimes here)
def is_valid_ap_ssdict( ap_ssdict, do_warn=1, do_exit=0 ):
    if ap_ssdict == None :
        if do_warn :
            print('+* WARN: no ap_ssdict entered as arg')
        if do_exit :
            sys.exit(3)
        return False
    return True

# ------------------------------------------------------------------------

def make_scriptname( proc_type, input_type):
    """Give a name to the script, depending on whether the proc_type is IC
or GV, and whether the input_type is pbrun or errts

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'

input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
scriptname : str
    name of script to create

"""
    
    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    scriptname = 'run_'
    scriptname+= DICT_proc_type[proc_type].lower() + '_'
    scriptname+= input_type
    scriptname+= '.tcsh'

    return scriptname 

def make_script_text_intro(proc_type, input_type):
    """Make the top descriptive comment in the script, depending on
whether the proc_type is IC or GV.

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
otxt : str
    text descriptive str

"""
    
    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = '''#!/bin/tcsh

# This script was created by the afni_proc.py quality control (APQC)
# generator.  
#
# Its purpose is to facilitate investigating the properties of time 
# series data, using the AFNI GUI's {proc_type} functionality.'''

    if input_type == 'pbrun' :
        otxt+= '''
#
# In this script, one *must* provide 2 command line args: 
# + a pb label (pb00, pb01, etc.), 
# + a run number (r01, r02, r03, etc.).'''

    otxt+= '''
#
# Additionally, one *can* add three numbers on the command line
# to represent the starting location (RAI coordinate notation) of the 
# initial seed.'''

    if proc_type == 'IC' :
        otxt+= '''
#
# Using InstaCorr:
# As described in the popup help, once the GUI is open and InstaCorr
# has been set up, users should just need to hold down the Ctrl+Shift
# keys and then left-click and move the mouse around (dragging or
# re-clicking).  Watch the correlation patterns to that seed location
# change, and this often provides an excellent way to understand the
# data.'''

    otxt+= '''
#
# ver = {ver}
# -------------------------------------------------------------------------

'''.format(ver=ver, proc_type=proc_type)

    return otxt

def make_script_text_input_label(proc_type, input_type):
    """Make the part of the script getting inputs and creating the label.

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
otxt : str
    text descriptive str

"""

    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = '''
# ----- Get inputs and create label
'''

    if input_type == 'pbrun' :
        otxt+= '''
set pb  = "$1"
set run = "$2"

if ( "${run}" == "" ) then
    echo "** Exiting: this program requires 2 cmd line args to run:"
    echo "   + a pb label (pb00, pb01, etc.)"
    echo "   + a run number (r01, r02, r03, etc.)."
    echo "   Additionally, you can then put 3 numbers as an initial"
    echo "   seed location coordinate"
    exit 1
endif

set label = "${pb} ${run}"

# possible starting seed coordinate (in RAI notation)
set xcoor = "$3"
set ycoor = "$4"
set zcoor = "$5"
'''
    elif input_type == 'errts' :
        otxt+= '''
# possible starting seed coordinate (in RAI notation)
set xcoor = "$1"
set ycoor = "$2"
set zcoor = "$3"

set label = 'errts (residuals)'
'''

    return otxt

# -----------------------------------------------------------------------

def make_script_text_main_dset(proc_type, input_type, ap_ssdict=None):
    """Make the part of the script getting main input ulay dset (and
ic_dset for IC case).

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'
ap_ssdict : dict
    some functionality chains require having the ap_ssdict from APQC

Returns
-------
otxt : str
    text

    """

    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = '''
# ----- find main dset(s)
'''

    if input_type == 'pbrun' :
        otxt+= '''
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
'''
        if proc_type == 'IC' :
            otxt+= '''
set ic_dset = "${dset_ulay}"
'''

    elif input_type == 'errts' :
        # here, specific files have to be searched for
        if proc_type == 'IC' :
            qqq = is_valid_ap_ssdict( ap_ssdict, do_exit=1 )
            uvar, ttt = make_apqc_ic_ulay( ap_ssdict )
            otxt+= ttt
            if proc_type == 'IC' :
                uvar, uuu = make_apqc_ic_dset( ap_ssdict )
                otxt+= uuu
        elif proc_type == 'GV' :
            otxt+= '''
set dset_ulay = {}
'''.format(ap_ssdict['errts_dset'])


    return otxt

        
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

# -----------------------------------------------------------------------

def make_script_text_load_dset(proc_type, input_type, ap_ssdict=None):
    """Make the part of the script loading dsets

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'
ap_ssdict : dict
    some functionality chains require having the ap_ssdict from APQC

Returns
-------
otxt : str
    text

    """

    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = ''

    if input_type == 'pbrun' :
        # search for vlines and radcor first
        otxt+= '''
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
'''

        if proc_type == 'IC' :
            # load across pb, and don't forget the IC dset itself
            otxt+= '''
# ----- make ordered list of dsets to load
set all_load  = ( "${dset_ulay}" "${ic_dset}"       \\
                   *.${run}.*HEAD                   \\
                   ${dset_vline} ${dset_radcor}     \\
                   *.HEAD *.nii* )
'''
        elif proc_type == 'GV' :
            # load across run
            otxt+= '''
# ----- make ordered list of dsets to load
set all_load  = ( "${dset_ulay}"                    \\
                   *.${pb}.*HEAD                    \\
                   ${dset_vline} ${dset_radcor}     \\
                   *.HEAD *.nii* )
'''

    elif input_type == 'errts' :
        # no radcor or vlines
        qqq = is_valid_ap_ssdict( ap_ssdict, do_exit=1 )
        otxt+= '''
# ----- make list of dsets to load
'''
        otxt+= make_apqc_errts_all_load( proc_type, ap_ssdict )


    return otxt


def make_apqc_errts_all_load( proc_type, ap_ssdict, skip_uvars=None ):
    """All results directory dsets get loaded in, to be available to be
used.  We build the list order so that the most useful to explore with
InstaCorr are first, and then everything follows in alphabetical
order.  This just involves going through a list of possible items, in
order.

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
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
    BAD_RETURN = ""

    # list of uvars, in decreasing order of preference
    list_ldep = ['anat_final', 'vr_base_dset', 
                 'errts_dset', 'mask_dset']

    # initialize
    otxt = """\n"""
    if proc_type == 'IC' :
        otxt+= """set all_load  = ( "${dset_ulay}" "${ic_dset}" """
        otxt+= ' '*20 
    elif proc_type == 'GV' :
        otxt+= """set all_load  = ( "${dset_ulay}" """
        otxt+= ' '*28 
    otxt+= """\\\n"""
    # indentation index for below
    nindent = otxt.index('(') + 2
    nfinal  = otxt.index('\\') - 1

    # add corr_brain, and vert align COL char
    line = '''{}corr_brain*HEAD'''.format(' '*nindent)
    npad = nfinal - len(line)
    if npad > 0 :
        line+= ' '*npad
    line+= '''\\\n'''
    otxt+= line

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

# ------------------------------------------------------------------------

def make_script_text_coors_params(proc_type, input_type):
    """Make the part of the script, about coordinates and other possible
params like seed radius (for IC).

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
otxt : str
    text

    """

    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = '''
# ----- finalize remaining parameters
'''

    otxt+= '''
if ( "${zcoor}" != "" ) then
    set coord = ( "${xcoor}" "${ycoor}" "${zcoor}" )
else
    set coord = `3dinfo -dc3 "${dset_ulay}"`
endif
'''
    if proc_type == 'IC' :
        otxt+= '''
set voxvol      = `3dinfo -voxvol "${ic_dset}"`
set ic_seedrad  = `echo "${voxvol}"                                      \\
                        | awk '{printf "%0.2f",(2*($1)^0.3334);}'`
echo "++ seedcorr radius: ${ic_seedrad}"

set ic_blur = 0
'''

        if input_type == 'pbrun' :
            otxt+= '''
# apply blur in IC if pb is not 'blur' or 'scale' block
set bind = `echo "${ic_dset}" | awk '{print index($0, "blur")}'`
set sind = `echo "${ic_dset}" | awk '{print index($0, "scale")}'`
if ( ${bind} || ${sind} ) then
    echo "++ Apply no blur for 'blur' or 'scale' blocks"
    set ic_blur = 0
else
    set ic_blur = `echo "${voxvol}"                                      \\
                        | awk '{printf "%0.2f",(1.5*($1)^0.3334);}'`
endif
'''

        otxt+= '''echo "++ blurring radius: ${ic_blur}"
'''

    return otxt

# ------------------------------------------------------------------------

def make_script_text_env_vars(proc_type, input_type):
    """Make the part of the script, env vars.

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
otxt : str
    text

    """

    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = '''
# ===========================================================================
'''

    otxt+= '''
# General GUI parameters set by default
setenv AFNI_ENVIRON_WARNINGS    NO
setenv AFNI_THRESH_INIT_EXPON   0
setenv AFNI_NOSPLASH            YES
setenv AFNI_SPLASH_MELT         NO
setenv AFNI_STARTUP_WARNINGS    NO
setenv AFNI_NIFTI_TYPE_WARN     NO
setenv AFNI_NO_OBLIQUE_WARNING  YES
setenv AFNI_COMPRESSOR          NONE
setenv AFNI_NEVER_SAY_GOODBYE   YES
setenv AFNI_MOTD_CHECK          NO
setenv AFNI_VERSION_CHECK       NO
setenv AFNI_IMAGE_DATASETS      NO
setenv AFNI_NO_ADOPTION_WARNING YES
setenv AFNI_FLASH_VIEWSWITCH    NO
'''

    if proc_type == 'IC' :
        otxt+= '''
# GUI params, set here for speed, perhaps 
setenv AFNI_DEFAULT_OPACITY    7
setenv AFNI_THRESH_AUTO        NO
'''
        if input_type == 'errts' :
            otxt+='''setenv AFNI_FUNC_BOXED         YES
'''
        elif input_type == 'pbruns' :
            otxt+='''setenv AFNI_FUNC_BOXED         NO
'''

    if proc_type == 'GV' :
        otxt+= '''
# graph specific parameters (faster to do here than with driving)
setenv AFNI_graph_width     800  # initial width of graph window
setenv AFNI_graph_height    500  # initial height of graph window
setenv AFNI_graph_matrix    5    # initial number of sub-graphs
'''

    return otxt

# ---------------------------------------------------------------------

def make_script_text_ic_params_cmd(input_type):
    """Make the part of the script, specifically for InstaCorr (so no
proc_type choice).

Parameters
----------
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
otxt : str
    text

"""

    if not(is_valid_input_type(input_type)) :  return ''

    if input_type == 'pbrun' :
        dpar = {
            'ic_polort' : '3' + ' '*20 + '# bc the data be unprocessed',
        }
    elif input_type == 'errts' :
        dpar = {
            'ic_polort' : '-1' + ' '*20 + '# bc the data be residuals',
        }

    otxt = '''
# ----- InstaCorr setup and cmd

# IC parameters
set ic_ignore   = 0
set ic_automask = no
set ic_despike  = no
set ic_bandpass = 0,99999
set ic_polort   = {ic_polort}
set ic_method   = P

# GUI visualization parameters
set pbar_sign   = "-"
set ncolors     = 99
set topval      = 0.6
set cbar        = "Reds_and_Blues_Inv"
set olay_alpha  = "Quadratic"
set thresh      = 0.3
set frange      = ${{topval}}
set crossh      = MULTI
set xh_gap      = -1
set OW          = "OPEN_WINDOW"

# port communication
set portnum = `afni -available_npb_quiet`

'''.format( **dpar )

    otxt+= '#' + '=' * 76

    otxt+= '''

afni -q  -no_detach                                                     \\
    -npb ${portnum}                                                     \\
    -com "SWITCH_UNDERLAY    ${dset_ulay}"                              \\
    -com "INSTACORR INIT                                                \\
                    DSET=${ic_dset}                                     \\
                  IGNORE=${ic_ignore}                                   \\
                    BLUR=${ic_blur}                                     \\
                AUTOMASK=${ic_automask}                                 \\
                 DESPIKE=${ic_despike}                                  \\
                BANDPASS=${ic_bandpass}                                 \\
                  POLORT=${ic_polort}                                   \\
                 SEEDRAD=${ic_seedrad}                                  \\
                  METHOD=${ic_method}"                                  \\
    -com "INSTACORR SET      ${coord} J"                                \\
    -com "SET_THRESHNEW      ${thresh}"                                 \\
    -com "SET_PBAR_ALL       ${pbar_sign}${ncolors} ${topval} ${cbar}"  \\
    -com "SET_FUNC_RANGE     ${frange}"                                 \\
    -com "SET_XHAIRS         ${crossh}"                                 \\
    -com "SET_XHAIR_GAP      ${xh_gap}"                                 \\
    -com "SET_FUNC_ALPHA     ${olay_alpha}"                             \\
    -com "$OW sagittalimage"                                            \\
    ${all_load:q}  &

sleep 1

set l = `prompt_popup -message \\
"   Run InstaCorr on AP results data:  ${label}\\n\\n\\
\\n\\
InstaCorr calc using : ${ic_dset}\\n\\
Initial ulay dataset : ${dset_ulay}\\n\\
         IC seed rad : ${ic_seedrad} mm\\n\\
         IC blur rad : ${ic_blur} mm\\n\\
         IC polort N : ${ic_polort}\\n\\
\\n\\
Wait briefly for initial correlation patterns to appear.  \\n\\
\\n\\
To use InstaCorr:\\n\\
+ Hold down Ctrl+Shift\\n\\
+ Left-click in the dataset\\n\\
+ Drag the cursor around.\\n\\
... and correlation patterns instantly update\\n\\
from each new seed location.\\n\\
\\n\\
To jump to particular coordinates:\\n\\
+ Right-click -> 'Jump to (xyz)' \\n\\
+ Enter 3 space-separated coords\\n\\
+ Right-click -> 'InstaCorr set'\\n\\
\\n\\'''

    if input_type == 'errts' :
        otxt+= '''
Alpha (transparent) thresholding is ON, and boxes have\\n\\
been placed around suprathreshold regions.\\n\\
\\n" \\
-b '          Done - Close AFNI GUI          '`
'''
    elif input_type == 'pbrun' :
        otxt+= '''
Alpha (transparent) thresholding is ON. To put boxes\\n\\
around suprathreshold regions, click 'B' above the   \\n\\
colorbar in the GUI.\\n\\
\\n" \\
-b '          Done - Close AFNI GUI          '`

'''

    otxt+= '''
if ("$l" != "1") then
    echo "+* Warn: InstaCorr guidance message failed to open"
endif

echo "++ Quiet talkers"
@Quiet_Talkers -quiet -npb_val ${portnum}

cat << EOF
===========================================
++ Goodbye, and thank you for InstaCorring.

EOF
exit 0
'''

    return otxt

def make_script_text_gv_params_cmd(input_type):
    """Make the part of the script, specifically for GraphView (so no
proc_type choice).

Parameters
----------
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'

Returns
-------
otxt : str
    text

"""

    if not(is_valid_input_type(input_type)) :  return ''

    otxt = '''
# ----- GraphView setup and cmd

# GUI visualization parameters
set crossh      = MULTI
set xh_gap      = -1
set OW          = "OPEN_WINDOW"
set graxis      = "axial"

# port communication
set portnum = `afni -available_npb_quiet`

'''

    otxt+= '#' + '=' * 76

    otxt+= '''

afni -q  -no_detach                                                     \\
    -npb ${portnum}                                                     \\
    -com "SWITCH_UNDERLAY    ${dset_ulay}"                              \\
    -com "SET_DICOM_XYZ      ${coord}"                                  \\
    -com "SET_XHAIRS         ${crossh}"                                 \\
    -com "SET_XHAIR_GAP      ${xh_gap}"                                 \\
    -com "$OW sagittalimage"                                            \\
    -com "$OW ${graxis}graph"                                           \\
    ${all_load:q}  &

sleep 1

set l = `prompt_popup -message \\
"   View graphs+images of AP results data:  ${label}\\n\\n\\
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
'''

    return otxt

# ========================================================================

def make_script_text_full(proc_type, input_type, ap_ssdict=None):
    """Make the full script for InstaCorr, GraphView, etc.  Basically go
through all functions to build pieces in order correctly.

Parameters
----------
proc_type : str
    one of an accepted list of processing types, which are currently:
    'IC', 'GV'
input_type : str
    one of an accepted list of input types, which are currently:
    'pbrun', 'errts'
ap_ssdict : dict
    some functionality chains (proc_type==IC) require having the
    ap_ssdict from APQC

Returns
-------
otxt : str
    text descriptive str

    """

    if not(is_valid_proc_and_input(proc_type, input_type)) :  return ''

    otxt = make_script_text_intro(proc_type, input_type)
    otxt+= make_script_text_input_label(proc_type, input_type)
    otxt+= make_script_text_main_dset(proc_type, input_type,
                                      ap_ssdict=ap_ssdict)
    otxt+= make_script_text_load_dset(proc_type, input_type,
                                      ap_ssdict=ap_ssdict)
    otxt+= make_script_text_coors_params(proc_type, input_type)
    otxt+= make_script_text_env_vars(proc_type, input_type)
    if proc_type == 'IC' :
        otxt+= make_script_text_ic_params_cmd(input_type)
    elif proc_type == 'GV' :
        otxt+= make_script_text_gv_params_cmd(input_type)

    return otxt


def write_apqc_icgv_script(proc_type, input_type, ap_ssdict, pname = '' ):
    """Write out the text file of the InstaCorr script.

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
    otext = make_script_text_full(proc_type, input_type, ap_ssdict)

    # write the text file in the results directory
    oname = make_scriptname( proc_type, input_type)
    if oname :
        if pname :    ofile = pname + '/' 
        else:         ofile = ''
        ofile+= oname
    else:
        print("** ERROR: could not write scriptname for:", proc_type, 
              input_type)
        sys.exit(7)

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

    msg = '''++ Done making (executable) {proc_type} {input_type} script: 
      {ofile}'''.format(proc_type=proc_type, input_type=input_type,
                        ofile=ofile)
    print( msg )

    return 0
