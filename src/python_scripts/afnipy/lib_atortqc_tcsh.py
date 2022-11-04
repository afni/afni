#!/usr/bin/env python

# This library contains functions for creating part of the
# AFNI-TORTOISE QC HTML.  Specifically, these functions build the
# script 'run_atort_review_html' that is run for single-subject HTML
# review.
#
#
auth = 'PA Taylor'
ver = 1.00 ; date = 'June 28, 2022'
# + start this program
#
#########################################################################

import os, copy
import sys
import glob
import json
import subprocess
import collections   as coll

from afnipy import afni_base           as ab
from afnipy import lib_atortqc_review  as latr
from afnipy import lib_atortqc_html    as lath

from afnipy import lib_apqc_tcsh       as lat

# ----------------------------------------------------------------------

ohtml      = 'index.html'            # output file, HTML page

scriptname = 'run_atort_review_html.tcsh'
qcbase     = 'ATORTQC'               # full odir has subj ID concatenated
dir_info   = 'extra_info'            # for gen_ss- and AP-JSONs, and more

page_title_json = '__page_title'


# ----------------------------------------------------------------------

# ======================== html page title ============================

def atortqc_Top_pagetop( opref, qcb, qci, task_name = '' ):

    comm  = '''subject ID for html page title'''

    pre = '''
    set opref = {}
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}.json
    '''.format( opref )

    jsontxt = '''
    cat << EOF >! ${{tjson}}
    itemtype    :: TITLE
    itemid      :: {}
    blockid     :: {}
    blockid_hov :: {}
    title       :: {}
    subj        :: "${{subj}}"
    EOF
    '''.format(qci, qcb, lath.qc_title[qcb][0], lath.qc_title[qcb][1] )

    ## !!!!! to be added at some point, from new uvar:
    ##    taskname  ::  task

    jsontxt_cmd = '''
    abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
    '''

    comm  = lat.commentize( comm )
    pre   = lat.commandize( pre, cmdindent=0, 
                       ALIGNASSIGN=True, ALLEOL=False )
    jsontxt = lat.commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = lat.commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre, jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# ==========================================================================
# ==========================================================================
# ==========================================================================

def apqc_vorig_all_4D( obase, qcb, qci, olay_posonly=True, ulay_name='' ):
    # Plot 1 slice of data across time; scale each slice by local
    # brightness. Here, qci has additional roles of a string label,
    # because we use this same function to plot all ORIG vols (EPI or
    # anat, at the moment)

    opref = '_'.join([obase, qcb, qci]) # full name

    perc_olay_top = 98                    # %ile for top of pbar for olay

    # what will minval of pbar be? 0, or -max?
    if olay_posonly :
        olay_minval_str = "-pbar_posonly"
        pbar_min        = "0"

    comm  = '''Check the quality of acquired {} in orig space (ulay)
            {} %ile topval for pbar'''.format( qci, perc_olay_top )

    # minor tweaks/formatting/expanding
    epi_comm = ''
    #if qci == "Up_DWI":
    #    epi_comm =  ' (Up DWI)'
    #if qci == "Down_DWI":
    #    epi_comm =  ' (Down DWI)'
    qci_comm = qci
    if qci == "anat":
        qci_comm = 'Anatomical'

    STR_json_text = '''"{} in original space{}"'''.format( qci_comm, epi_comm )
    STR_json_text+= ''' ,, '''   # separator for 2-line text in JSON
    STR_json_text+= '''"dset: ${{ulay_name}} ({})"'''.format( qci )

    # note the json file name has extra bit
    pre = '''
    set opref = {0}
    set ulay = "${{{1}}}"
    set ulay_name = `3dinfo -prefix ${{{1}}}`
    set ulay_ob = `3dinfo -obliquity ${{{1}}}`
    set tjson  = _tmp.txt
    set ojson  = ${{odir_img}}/${{opref}}_qc_sepscl.sag.json
    '''.format( opref, ulay_name )

    cmd4 = '''
    @djunct_4d_imager
    -inset    ${ulay}
    -save_ftype JPEG
    -onescl_off
    -prefix "${odir_img}/${opref}"
    '''

    jsontxt = '''
    cat << EOF >! ${{tjson}}
    itemtype    :: VOL
    itemid      :: {}
    blockid     :: {}
    blockid_hov :: {}
    title       :: {}
    text        :: {}
    EOF
    '''.format( qci, qcb, lath.qc_blocks[qcb][0], lath.qc_blocks[qcb][1],
                STR_json_text )

    jsontxt_cmd = '''
    abids_json_tool.py   
    -overwrite       
    -txt2json              
    -delimiter_major '::'    
    -delimiter_minor ',,'     
    -input  ${tjson}
    -prefix ${ojson}
    '''

    comm = lat.commentize( comm )
    pre  = lat.commandize( pre, cmdindent=0, 
                           ALIGNASSIGN=True, ALLEOL=False )
    cmd4  = lat.commandize( cmd4 )

    # NB: for commandizing the *jsontxt commands, one NEEDS
    # 'cmdindent=0', bc 'EOF' cannot be indented to be detected
    jsontxt = lat.commandize( jsontxt, cmdindent=0, ALLEOL=False )
    jsontxt_cmd  = lat.commandize( jsontxt_cmd, padpost=2 )

    lout = [comm, pre,  cmd4, 
            jsontxt, jsontxt_cmd]
    return '\n\n'.join(lout)

# =========================================================================
# =========================================================================

if __name__ == "__main__":
    # Whee.
    print('Done.')
