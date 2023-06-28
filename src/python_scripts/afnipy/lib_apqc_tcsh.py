#!/usr/bin/env python

# This library contains functions for creating part of the
# afni_proc.py QC HTML.  Specifically, these functions build the
# image, data and text of the single-subject HTML review.
#
#
auth = 'PA Taylor'
# ver : 1.32 || date: Oct 15, 2018
# + some level of written-ness
#
# ver : 1.33 || date: Oct 16, 2018
# + new uvars
# + new checks
# + new QC dir and subdir defs
#
# ver : 1.34 || date: Oct 17, 2018
# + new uvars
# + new text, new output strings
# + WARN type introduced
#
#ver = '1.4' ; date = 'Oct 23, 2018'
# + do stats differently: separate olay and thr
# + also start a json for the pbar info
#
#ver = '1.5' ; date = 'Nov 1, 2018'
# + update regression warning 
#
#ver = '1.51' ; date = 'Nov 19, 2018'
# + update 1dplot string labels (add in "enorm" and "outliers")
#
#ver = '1.6' ; date = 'Dec 2, 2018'
# + check_dep() now has more conditions, because of some specific
#   vars+values that may or may not be there
#   - first one: for stats_dsets, check about "NO_STATS" value
#
#ver = '1.7' ; date = 'Dec 23, 2018'
# + [PT] put the censoring vertical color bars into the reg* 1D plots,
#        for both basic and pythonic styles
# + [PT] rename "run_mode" -> "run_style" (more specific/correct)
#
#ver = '1.8' ; date = 'Jan 2, 2019'
# + [PT] add media_info
# + [PT] change order of enorm and outlier
# + [PT] add in EPI in orig space viewing
# + [PT] copy gen_ss JSON file into QC_*/media_info/
#
#ver = '2.1' ; date = 'Feb 26, 2019' 
# + [PT] new plot in regr: grayplot
#
#ver = '2.2' ; date = 'May 14, 2019' 
# + [PT] new images: radcor
#
#ver = '2.21' ; date = 'May 16, 2019' 
# + [PT] correctifying radcor behavior
#
#ver = '2.21' ; date = 'May 17, 2019' 
# + [PT] simplifying radcor behavior
#
#ver = '2.4' ; date = 'May 20, 2019' 
# + [PT] more details of aea_checkflip
# + [PT] radcor to own QC block
#
#ver = '2.5' ; date = 'May 23, 2019' 
# + [PT] switched to using afni_base functions for executing on
#        commandline
#
#ver = '2.7' ; date = 'June 26, 2019' 
# + [PT] elif for vstat: no anat or templ, use volreg as ulay; West
#        Coast usage for S Torrisi.
#
#ver = '2.8' ; date = 'June 28, 2019' 
# + [PT] better grayplotting, more informative stuff happening
# + [PT] PBAR size is now in terms of char width-- preserve verticality. Duh.
#
#ver = '2.9' ; date = 'July 3, 2019' 
# + [PT] vorig block now starting to be used
# + [PT] add in more stats to be viewed
# + [PT] add in QC block ID to QC block titles
#
#ver = '2.91' ; date = 'July 3, 2019' 
# + [PT] bannerize now has 'padsymb=' kwarg
#
#ver = '3.0' ; date = 'July 18, 2019' 
# + [PT] include obliquity in vorig QC block
# + [PT] simplify radcor text; decrease repetition
# + [PT] -> merge in changed opts for radcor
#
#ver = '3.1' ; date = 'Sep 6, 2019' 
# [PT] put a montgap (1 line, black) into QC montages: sep imgs a bit
#    + put in censoring to the 1dplot.py command when showing VR6 -
#      also known as the 'Molfese approach'
#
#ver = '3.11' ; date = 'Sep 9, 2019' 
# [PT] spacing fix in VR6 with censoring
#
#ver = '3.12' ; date = 'Dec 26, 2019' 
# [PT] for regr QC block, indiv stim plotting: don't need 'xmat_uncensored'
#      as a dependency, so remove it from the list
#
#ver = '3.2' ; date = 'Jan 9, 2020' 
# [PT] new warning block: censor fraction
#    + seed point plotting introduced (vstat section for resting state)
#
#ver = '3.21' ; date = 'Jan 9, 2020' 
# [PT] raise the thresholds in the vstat_seedcorr regr_corr plots
#    + so much above threshold otherwise, including noise
#
#ver = '3.22' ; date = 'Jan , 2020' 
# [PT] fix type conversion error of scalars -> lists
#
#ver = '3.3' ; date = 'Feb 15, 2020' 
# [PT] new funcs for 'widely used' params
#    + for censor and sundry info.  
#
#ver = '3.31' ; date = 'Feb 17, 2020' 
# [PT] further cleaned up (simplified?) a lot of the censoring info
#
#ver = '3.32' ; date = 'Feb 21, 2020' 
# [PT] fix minor bug in case of: 'basic' html with no outlier-based censoring
#
#ver = '3.33' ; date = 'Feb 26, 2020' 
# [PT] fix minor bug in case of: 'pythonic' html with no censoring at all. Sigh.
#
#ver = '3.4' ; date = 'March 11, 2020' 
# [PT] change way template/final_anat dsets are proc'ed/used.
#    + new top level section to get template/anat_final properties
#    + va2t: now underlay anat, and use template for edges
#    + vstat: now underlay template (if there), instead of anat_final
#    + regr: use template as ulay (if there), instead of anat_final
#
#ver = '3.41' ; date = 'March 12, 2020' 
# [PT] no vstat if 'surf' block was used in AP (-> stats dset is
#      *.niml.dset)
#
#ver = '3.5' ; date = 'March 27, 2020' 
# [PT] remove dependency on lib_apqc_html_helps.py
#
#ver = '3.6' ; date = 'May 26, 2020' 
# [PT] ve2a and LR-flipcheck now show EPI under anat edges
#
#ver = '3.61' ; date = 'May 28, 2020' 
# [PT] in vstat maps, report DF value(s)
#
#ver = '3.62' ; date = 'May 31, 2020' 
# [PT] EPI ulay ranges in ve2a and LR-flipcheck now: NZ 2-98%
#
#ver = '3.63' ; date = 'May 31, 2020' 
# [PT] vstat seedbased corr seed thr from 0.3 -> 0.2
#
#ver = '3.64' ; date = 'June 14, 2020' 
# [PT] return vstat seedbased corr seed thr to 0.3 (from 0.2)
#    + if ~normal smoothing is done, this is needed
#    + if no smoothing is done
#
#ver = '3.65' ; date = 'July 30, 2020' 
# [PT] 
#    + search for template first with full given path, then just by
#      basename.  this makes it easier to have QC work if directory
#      structure changes (e.g., for demos, that are downloaded+run on
#      different computers)
#    + also include a wildcard to help clean intermed file, in case
#      auto GZIP is on
#
#ver = '3.7' ; date = 'Feb 22, 2021'
# [PT] add in TSNR images
#
#ver = '3.71' ; date = 'Feb 22, 2021'
# [PT] updates with TSNR images/fnames 
#    + following on from AP updates from RCR---thanks!
#
#ver = '3.72' ; date = 'Feb 24, 2021'
# [PT] TSNR image no longer *requires* mask
#    + add a type-check to dep_check.  
#
#ver = '3.73' ; date = 'Mar 5, 2021'
# [PT] cp review basic text to QC_*/ dir
#
#ver = '3.74' ; date = 'Apr 6, 2021'
# [PT] update TSNR-vreg checks
#    + give sep names for TSNR images: tsnr_vreg and tsnr_fin
#
#ver = '3.75' ; date = 'Apr 6, 2021'
# [PT] now use adjunct*tsnr*general prog (just added, only need 1 prog)
#
#ver = '3.76' ; date = 'Sep 21, 2021'
# [PT] use '-no_cor' to not make coronal plane images
#    + save nearly 33% of space in QC_${subj} dir
#
#ver = '3.77' ; date = 'Sep 21, 2021'
# [PT] adjunct*tsnr: '-no_cor' to not make coronal plane images
#    + keep applying new opt
#
#ver = '3.78' ; date = 'Sep 27, 2021'
# [PT] Due to recent changes (from ~Aug 23) in label_size defaults
#      in imseq.c, adjust the default labelsize from 3 -> 4.
#    + this should restore labels to their longrunning size (since Aug
#      23 they have been one size smaller by default); but the new font
#      will be bolder than previously, due to those imseq.c changes.
#
#ver = '3.80' ; date = 'Jan 20, 2022'
# [PT] move the parts of code for task-based FMRI vstat selection to a
# new library: lib_apqc_stats_dset.py
# + as part of this, adding in fuller stats representation by default.
#
#ver = '3.93' ; date = 'Jan 26, 2022'
# [PT] epi-anat overlap in vorig QC block 
#
#ver = '4.0' ; date = 'June 5, 2022'
# [PT] ve2a: better %ile range for ulay: should have have better contrast
# + ve2a: also introduce scaling/values for local-unifized EPI as ulay
#
#ver = '4.01' ; date = 'June 6, 2022'
# [PT] ve2a: new scaling for ulay, extra control of grayscale with
#   ulay_min_fac
#
#ver = '4.02' ; date = 'July 27, 2022'
# [PT] mecho: cp -> rsync, because of annoying Mac difference in cp
#
#ver = '4.03' ; date = 'Aug 18, 2022'
# [PT] add warns: 3dDeconvolve *.err text file
#
#ver = '4.04' ; date = 'Aug 18, 2022'
# [PT] add mask_dset images: overlays final dset, whether in 
#      va2t, ve2a or vorig QC block
#
#ver = '4.05' ; date = 'Aug 18, 2022'
# [PT] put already-calc'ed Dice info below ve2a and va2t olay imgs
#      ---> but just as quickly have removed it; might distract from the
#           important sulcal/gyral overlap
#
#ver = '4.06' ; date = 'Aug 31, 2022'
# [PT] make a JSON version of ss_rev_basic TXT file in QC*/extra_info
#      -> will use this for 'saving' mode of APQC HTML interaction
#
#ver = '4.1' ; date = 'Nov 15, 2022'
# [PT] many new parts for var_lines (AKA vlines) and tcat QC
#
ver = '5.0' ; date = 'Mar 05, 2023'
# [PT] move toward Python-only implementation, rather than generating
#      a script intermediately, to simplify flexibility, additions and
#      apqc2/NiiVue functionality
#
#########################################################################

import os, copy
import sys
import glob
import json
import subprocess
import collections    as coll
from   datetime   import datetime

from afnipy import afni_base           as ab
from afnipy import lib_apqc_html       as lah
from afnipy import lib_apqc_stats_dset as lasd
from afnipy import lib_ss_review       as lssr
from afnipy import lib_apqc_io         as laio
from afnipy import lib_apqc_niivue     as lanv

# ----------------------------------------------------------------------

ohtml      = 'index.html'            # output file, HTML page

scriptname = '@ss_review_html'
qcbase     = 'QC'                    # full odir has subj ID concatenated
dir_info   = 'extra_info'            # for gen_ss- and AP-JSONs, and more

page_title_json = '__page_title'

# used in one of the warnings checks
fname_vlines_img = 'QC_var_lines.jpg'
fname_vlines_txt = 'QC_var_lines.txt'

# logo files
all_logo = [ 'apqc_logo_main.svg',
             'apqc_logo_help.svg',
]

# ----------------------------------------------------------------------

coord_opp = { 'R' : 'L',
              'L' : 'R',
              'A' : 'P',
              'P' : 'A',
              'I' : 'S',
              'S' : 'I',
              }

def coord_to_gen_sys(x, order='RAI'):
    '''
    Input
    -----
    x     : list of three coords, either float or str to be turned into float
    order : (opt) coord order (def = 'RAI')

    Output
    ------
    out   : list of 3 strings with generalized coord vals, e.g.:
               [-4R, 3A, 27S]
    '''

    N = len(x)

    if len(x) != 3 :
        print("** ERROR: need x to be list of 3 coord values")
        sys.exit(4)

    out = []
    for i in range(N):
        val = float(x[i])
        if val > 0 :
            out.append(str(abs(val))+coord_opp[order[i]])
        else:
            out.append(str(abs(val))+order[i])
    return out

# --------------------------------------------------------------------

def read_in_txt_to_dict(fname, tmp_name='__tmp_txt2json.json', DO_CLEAN=True) :
    '''Take a colon-separate file 'fname', convert it to a JSON file
'tmp_name', and then return the dictionary created thereby.

    Cleaning text file is optional (def: clean it)

    '''

    if not(os.path.isfile(fname)):
        print("** ERROR: could not find text file: {}".format(fname))
        return {}

    odict = {}

    cmd = '''abids_json_tool.py \
    -overwrite                  \
    -txt2json                   \
    -prefix {outp}              \
    -input  {inp}
    '''.format( inp  = fname,
                outp = tmp_name )
    com = ab.shell_com(cmd, capture=1)
    com.run()

    # get dictionary form of json
    with open(tmp_name, 'r') as fff:
        odict = json.load(fff)    
        
    if DO_CLEAN :
        # rm tmp json
        cmd = '''\\rm {outp}'''.format( outp = tmp_name )
        com = ab.shell_com(cmd, capture=1, save_hist=0)
        com.run()

    return odict

# --------------------------------------------------------------------

def get_path_abin():
    
    do_cap = True

    cmd  = '''dirname `which apqc_make_tcsh.py`'''
    com  = ab.shell_com(cmd, capture=do_cap)
    stat = com.run()
    abin_path = com.so[0]

    return abin_path

# --------------------------------------------------------------------

def get_space_from_dset( dset ):
    
    cmd = '''@FindAfniDsetPath -full_path -append_file -space {}'''.format(dset)
    com = ab.shell_com(cmd, capture=1, save_hist=0)
    com.run()
    dset_fullpath = com.so[0]

    cmd = '''3dinfo -space {}'''.format(dset_fullpath)
    com = ab.shell_com(cmd, capture=1, save_hist=0)
    com.run()
    space = com.so[0]

    return space

# --------------------------------------------------------------------

def get_warn_level_3( val, cutoff_list=[] ):
    '''Go through simple conditional cases for assigning level of
    warning; cutoff_list must be 3 items here.
    
    List should be in order of descending severity.

    '''

    if not(cutoff_list) :
        print("** ERROR: no cutoff list provided ")
        sys.exit(3)

    try:
        N = len(cutoff_list)
        if N != 3 :
            print("** ERROR: len of cutoff_list is {}, not 3".format(N))
            sys.exit(3)
    except :
        print("** ERROR: cutoff_list entry not a list, but of type {}?".format(type(cutoff_list)))
        sys.exit(3)

    # parse text file for warning severity
    if   val >= cutoff_list[0] :
        out = "severe"
    elif val >= cutoff_list[1] :
        out = "medium"
    elif val >= cutoff_list[2] :
        out = "mild"
    else :
        out = "none"

    return out


# --------------------------------------------------------------------

def check_dep(D, lcheck):
    '''Does dictionary 'D' contain each of the elements of 'lcheck'?'''

    if type(lcheck) != list :
        sys.exit("** ERROR: input to check_dep() needs to be a list, not:\n"
                 "          {}".format(type(lcheck)))


    HAS_ALL = 1
    for x in lcheck:

        # general check: based on existence
        if not(D.__contains__(x)) :
            HAS_ALL = 0
            break

        # ---- specific check(s), based on known values:

        # in this case, the $stats_dset var can contain essentially a
        # NULL value, NO_STATS, and that is reflected here
        elif x == "stats_dset" :
            if D[x] == "NO_STATS" :
                HAS_ALL = 0
                break

            # also need to consider case that stats is on surface; at
            # the moment, won't have any QC imaging for this scenario
            elif D[x][-10:] == ".niml.dset" :
                HAS_ALL = 0
                break

        elif x == "have_radcor_dirs" :
            if D[x] == "no" : 
                HAS_ALL = 0
                break

    return HAS_ALL

# ----------------------------------------------------------------------

def bannerize( x, fullwid=76, indent=0, padpre=1, padpost=2, padsymb='='):
    '''Make the banner for each QC image block.  

Inputs
------
    x : a string that is the message to put, and it gets wrapped with a
        comment symbol and '=' (or what user specifies).

    fullwid : can be any number of width; default is 76 because there
        are 2 other characters put in: ' ' before/after the string.

    indent : number of spaces to prepend to the line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

Returns
-------
    string : x padded with comment/lines/spaces.

    '''

    out = indent*' ' + "# "

    x = x.strip()
    freespace = fullwid - len(out) - len(x)

    if freespace < 0:
        freespace = 0

    lban = freespace // 2
    rban = freespace - lban
        
    out+= lban * padsymb
    out+= ' '+x+' '
    out+= rban * padsymb

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    return out

# ----------------------------------------------------------------------

def padassign(x, L):
    '''Move an assignment operator '=' rightward to a new index L, if it
exists to the left; if it exists to the right of that spot, or if it
doesn't appear in that string, just return the original string.  If
multiple occurrences of '=' occur in a string, just return original.

    '''
    
    # check if string contains it
    if not('=' in x):
        return x

    # check if multiple occurrences
    if x.count('=') > 1:
        return x

    # check if we are already there/rightward
    K = x.index('=')
    if K >= L:
        return x

    out = x.replace('=', (L-K)*' '+'=')

    return out

# ----------------------------------------------------------------------

def commandize( x, fullwid=76, indent=0, padpre=0, padpost=0,
                ALIGNASSIGN=False, ALLEOL=True, cmdindent=4,
                REP_TIL=True):
    '''Make the readable/spaced/EOL-char'ed version for each QC image
cmd.

Inputs
------

    x : a string that is the cmd; should arranged into multiple lines
        (for long commands). Don't need/shouldn't have EOL chars.

    fullwid : can be any number of width; default is 76, since ' \'
        gets added on to most lines.

    indent : number of spaces to prepend to each line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

    ALIGNASSIGN : flag to line up assignment operators, using longest in
        list of commands

    ALLEOL : flag to use the EOL char at the end of all (but the last)
        lines;  if entering separate cmds, turn off.

    cmdindent : number of spaces to prepend to lines after the [0]th,
        i.e., just indenting within a single command.  Wouldn't be
        used if there are several sep lines, for example.

    REP_TIL : replace any tilde '~' with a space at the end (may be
        useful because the lines are stripped of surrounding white
        space to start)

Returns
-------

    string : will have the same number of lines as x, but all lines
    after [0]th will have indentation, and each line except [-1]th
    will have EOL char at end

    '''

    y = x.split('\n')
    Nlines = len(y)
    count = 0
    new = []
    maxia = 0    # will be loc of assign ops if using ALIGNASSIGN

    # go through, strip whitespace from ends, and find out where the
    # '=' are in each line
    z = []
    for line in y:
        ll = line.strip()
        if ALIGNASSIGN :
            # [PT: May 19, 2019] Make this align " = " instead of "=",
            # because of possible "==" usage that we *don't* want to align
            if ' = ' in ll:
                iassign = ll.index(' = ')
                if iassign > maxia :
                    maxia = iassign
        z.append(ll)

    # line up assign ops if asked; also, need nonzero maxia
    if ALIGNASSIGN and maxia:
        for i in range(len(z)):
            # above, we found locus of ' = '; this func places '='
            z[i] = padassign(z[i], maxia+1)

    # go through last time and space everything appropriately
    for line in z:
        ll = line.strip()
        if len(ll) :
            ll = indent*' ' +ll
            if count:
                ll = cmdindent*' ' + ll
            if ALLEOL :
                lenll = len(ll)
                pad = fullwid - lenll
                if pad > 0:
                    ll+= pad*' '
            if REP_TIL:
                ll = ll.replace('~', ' ')
            new.append( ll )
            count+=1

    # put newlines at ends of each, or not
    if ALLEOL :
        out = ' \\\n'.join(new)
    else:
        out = '\n'.join(new)

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    return out

# ---------------------------------------------------------------------

def commentize( x, fullwid=76, indent=0, padpre=0, padpost=0,
                REP_TIL=True):
    '''Take a string and make it into a comment; indent uniformly, if
necessary.  Having '||' in a string (surrounded by whitespace) will
translate to starting a new line.

Inputs
------

    x : a string that is the cmd; should arranged into multiple lines
        (for long commands). Don't need/shouldn't have EOL chars.

    fullwid : can be any number of width; default is 76, since ' \'
        gets added on to most lines.

    indent : number of spaces to prepend to each line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

    REP_TIL : replace any tilde '~' with a space at the end (may be
        useful because the lines are stripped of surrounding white
        space to start)

Returns
-------

    string : output >=1 line string version of the original, but
        spaced to not overrun the fullwid line width.  Can be
        uniformly indented.  If the input x is only whitespace or
        empty, then a null string is returned.

    '''

    y = x.split()

    new = []
    line = indent*' ' + '#'
    lenstart = len(line)
    for word in y:
        if word == "||":
            new.append(line)
            line = indent*' ' + '#'
        elif len(line) + len(word) < fullwid:
            line+= ' ' + word
        else:
            new.append(line)
            line = indent*' ' + '#'
            line+= ' ' + word
    # and get the last line, if there is any text still there beyond
    # just the starting stuff
    if len(line) > lenstart :
        new.append(line)

    out = '\n'.join(new)

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    if REP_TIL :
        out = out.replace('~', ' ')

    return out

# ---------------------------------------------------------------------

def echoize( x, efile='', indent=0, padpre=0, padpost=0,
             quote='''"''', REP_TIL=True):
    '''Take a string and make it into a series of echo statements that
will be sent into a text file; indent uniformly, if necessary.  Each
line is stripped of white space to start, but the '~' character will
be replaced by a space at the end, by default.  The 

Inputs
------

    x : a string that is the cmd; should arranged into multiple lines
        (for long commands). Don't need/shouldn't have EOL chars.

    efile : redirect echo to a file; otherwise, just sent to screen

    indent : number of spaces to prepend to each line, if desired

    padpre : number of empty lines to affix beforehand

    padpost : number of empty lines to affix after text

    quote : select kind of quote to use for echo; default is ".

    REP_TIL : replace any tilde '~' with a space at the end (may be
        useful because the lines are stripped of surrounding white
        space to start)

Returns
-------

    string : output >=1 line string version of the original, but
        spaced to not overrun the fullwid line width.  Can be
        uniformly indented.  If the input x is only whitespace or
        empty, then a null string is returned.

    '''

    multix = x.split('\n')
    N   = len(multix)
    new = []

    # set variable to contain output text file name
    if efile :
        new.append( '# text for output section in html'  )
        new.append( 'set otxt = {}'.format(efile) )

    for i in range(N):
        line = multix[i]
        ll = line.strip()
        y = indent * ' '
        y+= '''echo {}'''.format(quote) + ll + '''{}'''.format(quote)
        if REP_TIL:
            y = y.replace('~', ' ')
        if efile :
            # choose how to redirect each line
            red = '> ${otxt}' if ( i == 0 ) else '>> ${otxt}'
            y+= '''   {}'''.format(red)
        new.append(y)
        

    out = '\n'.join(new)

    if padpre:
        out = padpre*'\n'+out
    if padpost:
        out+= padpost*'\n'

    return out

# ======================================================================
# ======================================================================
# ===================== specifics for the dsets ========================
# ======================================================================
# ======================================================================

def set_apqc_dirs(ap_ssdict):
    """Set the names for the QC/ and subdirs for images, etc. in the
dict ap_ssdict.  These are fixed names, basically. 

Also include the path location of the abin dir.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
ap_ssdict : dict
    updated dictionary of subject uvars
"""

    ap_ssdict['odir_qc']   = qcbase + '_' + ap_ssdict['subj']
    ap_ssdict['odir_img']  = ap_ssdict['odir_qc'] + '/' + lah.dir_img
    ap_ssdict['odir_info'] = ap_ssdict['odir_qc'] + '/' + dir_info

    ap_ssdict['abin_dir']  = get_path_abin()

    return ap_ssdict


def make_apqc_dirs(ap_ssdict, ow_mode='backup', bup_dir=None):
    """Create the output QC directory, along with its main
subdirectories.  These names and paths exist in the dictionary
ap_ssdict. 

The mode determines whether this program will overwrite an existing
entity of that name or not:
  shy         -> make new QC/ only if one does not already exist
  overwrite   -> purge an existing QC dir and make new QC/
  backup      -> move an existing QC dir to QC_<time> and make new QC dir
                 + if bup_dir is given, then use that for backup name

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
ow_mode : str
    label for the overwrite mode behavior of replacing or backing up
    an existing QC dir (or a file with the same name as the dir)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    do_cap = True
    com    = ab.shell_com('# make output QC dir', capture=do_cap)
    stat   = com.run()

    if not( ow_mode in laio.list_apqc_ow_modes ) :
        print("** ERROR: illegal ow_mode '{}', not in the list:\n"
              "   {}".format(ow_mode, laio.str_apqc_ow_modes))
        sys.exit(11)

    # check if the main QC dir exists already
    qcdir_exists = os.path.exists(ap_ssdict['odir_qc'])

    if qcdir_exists :
        if ow_mode=='shy' or ow_mode==None :
            print("** ERROR: output QC dir exists already: {}\n"
                  "   Exiting.".format(ap_ssdict['odir_qc']))
            print("   Check out '-ow_mode ..' for overwrite/backup opts.")
            sys.exit(10)

        elif ow_mode=='backup' :
            if bup_dir==None :
                now     = datetime.now() # current date and time
                bname   = ap_ssdict['odir_qc']
                bup_dir = now.strftime( bname + "_%Y-%m-%d-%H-%M-%S")

            print("+* WARN: output QC dir exists already: {}\n"
                  "   -> backing it up to: {}".format(ap_ssdict['odir_qc'],
                                                     bup_dir))
            cmd     = '''\\mv {} {}'''.format(ap_ssdict['odir_qc'], bup_dir)
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()

        elif ow_mode=='overwrite' :
            print("+* WARN: output QC dir exists already: {}\n"
                  "   -> overwriting it".format(ap_ssdict['odir_qc']))
            cmd    = '''\\rm -rf {}'''.format(ap_ssdict['odir_qc'])
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()

    # Now make the new output QC dir (via its subdirs)
    cmd    = '''\\mkdir -p {}'''.format(ap_ssdict['odir_img'])
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    cmd    = '''\\mkdir -p {}'''.format(ap_ssdict['odir_info'])
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    return 0


def copy_apqc_logos(ap_ssdict):
    """Copy APQC logos to the correct directory in the APQC HTML.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    do_cap = True
    com    = ab.shell_com('# copy logos to QC info dir', capture=do_cap)
    stat   = com.run()

    for logo in all_logo:
        logo_file = ap_ssdict['abin_dir'] + '/' + logo
        if os.path.isfile( logo_file ) :
            cmd     = '''\\cp {} {}'''.format(logo_file, 
                                              ap_ssdict['odir_info'])
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()
        else:
            print("+* WARNING: cannot find logo file:\n   {}"
                  "".format(logo_file))

    # TEMPORARY !!!!!!
    cmd     = '''\\cp {} {}'''.format(ap_ssdict['abin_dir'] + '/' + 'niivue_afni.umd.js',
                                      ap_ssdict['odir_qc'])
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    return 0

def set_apqc_sundry(ap_ssdict, ssrev_dict):
    """Get number of TRs per run from the ss_review dictionary ssrev_dict,
and populate this info into ap_ssdict.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
ssrev_dict : dict
    dictionary of ss review info

Returns
----------
ap_ssdict : dict
    updated dictionary of subject uvars
"""

    key   = "num_TRs_per_run"
    val   = ssrev_dict[key]
    vtype = type(val)

    if vtype == str :
        ap_ssdict['pats'] = val
    elif vtype == int :
        ap_ssdict['pats'] = str(val)
    elif vtype == list :
        ap_ssdict['pats'] = ' '.join([str(v) for v in val])
    else:
        print("** ERROR: the value of key '{}' (= '{}') should be str, int or "
              "list, not {}".format(key, val, vtype))

    return ap_ssdict


def set_apqc_censor_info_INIT(ap_ssdict):
    """Make the initial pieces for censoring info that are used
repeatedly, and populate this info into ap_ssdict.  Executing this
function is necessary, regardless of run style.

This mostly sets up strings for reporting about censor counts and
fractions, as well as graph y-axis limits.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
ap_ssdict : dict
    updated dictionary of subject uvars

    """

    do_cap = True
    com    = ab.shell_com('# setup init/gen censoring info', capture=do_cap)
    stat   = com.run()

    # Initialize some values: these values typically only get used
    # when censoring has *not* been applied.
    ap_ssdict['rep_cen_str'] = "(no censoring applied)"
    ap_ssdict['addtxt_cen_str'] = ""
    ap_ssdict['cen_used']     = 0
    ap_ssdict['cen_have_mot'] = 0
    ap_ssdict['cen_have_out'] = 0
    ap_ssdict['ytop_mot']     = ''
    ap_ssdict['ytop_out']     = ''

    if check_dep(ap_ssdict, ['censor_dset']) :
        # when censoring *has* been applied---likely the norm
        ap_ssdict['cen_used'] = 1


        # build a censor-reporting string
        istr = '''-infile {}'''.format( ap_ssdict['censor_dset'] )

        cmd  = '''1d_tool.py -show_trs_censored encoded {}'''.format(istr)
        com  = ab.shell_com(cmd, capture=do_cap)
        stat = com.run()
        cstr = com.so[0]   # censor string, like 1..4,6,18..23

        cmd  = '''1d_tool.py -verb 0 -show_censor_count {}'''.format(istr)
        com  = ab.shell_com(cmd, capture=do_cap)
        stat = com.run()
        Ncen = int(com.so[0]) # total num of censored time points

        Pcen = 100. * Ncen / float(ap_ssdict['nt_orig'])  # perc censored
        pstr = "{}".format(int(Pcen))
        if Ncen > 0 and Pcen < 1.0 :
            pstr = "<1"
        rep_cen_str = "censored vols ({}%): {}".format(pstr, cstr)
        ap_ssdict['rep_cen_str'] = rep_cen_str
        ap_ssdict['addtxt_cen_str'] = " and combined censoring"

        # [PT] A note about the multiplicative factor in ${ytop_*}, below:
        # visually, a factor of 3 seems to be a good balance for this
        # scaling.  The plot is clear, and sub-threshold motion seems
        # "small" quickly: with a smaller factor like 2, even some
        # subthreshold motion appears "large"; larger factors waste
        # space.

        if check_dep(ap_ssdict, ['mot_limit']) :
            ap_ssdict['cen_have_mot'] = 1
            ap_ssdict['ytop_mot'] = 3. * float(ap_ssdict['mot_limit'])

        if check_dep(ap_ssdict, ['out_limit']) :
            ap_ssdict['cen_have_out'] = 1
            ap_ssdict['ytop_out'] = 3. * float(ap_ssdict['out_limit'])

    return ap_ssdict

def set_apqc_censor_info_BASIC(ap_ssdict):
    """Make the initial pieces for censoring info that are used
repeatedly, and populate this info into ap_ssdict.  This is
specifically for the 'basic' run_style.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
ap_ssdict : dict
    updated dictionary of subject uvars

    """

    do_cap = True
    com    = ab.shell_com('# setup *basic* censoring info', capture=do_cap)
    stat   = com.run()

    # initial censor variable settings
    ap_ssdict = set_apqc_censor_info_INIT( ap_ssdict )

    if check_dep(ap_ssdict, ['censor_dset']) :
        ap_ssdict['cen_color'] = 'green'
        ap_ssdict['cen_hline_color'] = 'red'
        ttt = "-censor_RGB {} -censor {}".format(ap_ssdict['cen_color'],
                                                 ap_ssdict['censor_dset'])
        ap_ssdict['cen_cmd'] = ttt

        # also include yaxis scaling by censor levels, if given
        if check_dep(ap_ssdict, ['mot_limit']) :
            ttt = "1D: {}@{}".format(ap_ssdict['nt_orig'],
                                     ap_ssdict['mot_limit'])
            ap_ssdict['cen_lim_mot'] = ttt

            uuu = "-yaxis 0:{:.8f}:6:2".format(float(ap_ssdict['ytop_mot']))
            ap_ssdict['cen_lim_mot_yax'] = uuu
        else:
            ap_ssdict['cen_lim_mot']     = ''
            ap_ssdict['cen_lim_mot_yax'] = ''

        if check_dep(ap_ssdict, ['out_limit']) :
            ttt = "1D: {}@{}".format(ap_ssdict['nt_orig'],
                                     ap_ssdict['out_limit'])
            ap_ssdict['cen_lim_out'] = ttt

            uuu = "-yaxis 0:{:.8f}:6:2".format(float(ap_ssdict['ytop_out']))
            ap_ssdict['cen_lim_out_yax'] = uuu
        else:
            # this is a cheap way out of needing to have quotes
            # around ${cen_lim_out} in the case that 'out_limit'
            # is present, and not wanting it if it is *not*
            # present-- just use -echo_edu here
            ap_ssdict['cen_lim_out']     = '-echo_edu'
            ap_ssdict['cen_lim_out_yax'] = ''

        # this is not used in 'basic' run_style, only in 'pythonic'
        # Q: might not even need these here (since '')?
        ap_ssdict['cen_lim_all'] = ''
        ap_ssdict['cen_lim_all_yax'] = ''

    return ap_ssdict


def set_apqc_censor_info_PYTHONIC(ap_ssdict):
    """Make the initial pieces for censoring info that are used
repeatedly, and populate this info into ap_ssdict. This is
specifically for the 'pythonic' run_style.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
ap_ssdict : dict
    updated dictionary of subject uvars

    """

    do_cap = True
    com    = ab.shell_com('# setup *pythonic* censoring info', capture=do_cap)
    stat   = com.run()

    # initial censor variable settings
    ap_ssdict = set_apqc_censor_info_INIT( ap_ssdict )

    # default/null values, changed just below for either/both that exist
    # Q: does this need to be set of 'basic' output?
    ap_ssdict['mot_hline'] = 'NONE'
    ap_ssdict['out_hline'] = 'NONE'

    if check_dep(ap_ssdict, ['censor_dset']) :

        ap_ssdict['cen_color'] = 'red'
        ap_ssdict['cen_hline_color'] = 'cyan'

        ttt = "-censor_files {}".format(ap_ssdict['censor_dset'])
        ap_ssdict['cen_cmd'] = ttt

        if check_dep(ap_ssdict, ['mot_limit']) :
            ap_ssdict['mot_hline'] = ap_ssdict['mot_limit']

            ttt = "-censor_hline {}".format(ap_ssdict['mot_limit'])
            ap_ssdict['cen_lim_mot'] = ttt

            uuu = "-yaxis 0:{}".format(ap_ssdict['ytop_mot'])
            ap_ssdict['cen_lim_mot_yax'] = uuu                

        if check_dep(ap_ssdict, ['out_limit']) :
            ap_ssdict['out_hline'] = ap_ssdict['out_limit']

            ttt = "-censor_hline {}".format(ap_ssdict['out_limit'])
            ap_ssdict['cen_lim_out'] = ttt

            uuu = "-yaxis 0:{}".format(ap_ssdict['ytop_out'])
            ap_ssdict['cen_lim_out_yax'] = uuu                

    # order matters here: mot, out
    ttt = "-censor_hline {} {}".format( ap_ssdict['mot_hline'], 
                                        ap_ssdict['out_hline'] )
    ap_ssdict['cen_lim_all'] = ttt

    uuu = "-yaxis 0:{} 0:{}".format( ap_ssdict['ytop_mot'], 
                                     ap_ssdict['ytop_out'] )
    ap_ssdict['cen_lim_all_yax'] = uuu

    return ap_ssdict

# ---------

def set_apqc_main_dset(ap_ssdict):
    """Find the main dset, mostly for ulay functionality, in descending
order of preference: template, anat_final, vr_base.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
ap_ssdict : dict
    updated dictionary of subject uvars
"""

    do_cap = True
    com    = ab.shell_com('# find main dset', capture=do_cap)
    stat   = com.run()

    HAVE_MAIN = 0    # NB: should prob fail if this remains 0?

    # order to check in!
    ldep      = ['template']
    ldep_alt1 = ['final_anat']
    ldep_alt2 = ['vr_base_dset']

    if check_dep(ap_ssdict, ldep) :         # --------- check for template
        # pre-check, output might be used in a couple ways
        cmd  = '''basename {}'''.format( ap_ssdict['template'] )
        com  = ab.shell_com(cmd, capture=do_cap)
        stat = com.run()
        btemp = com.so[0] # always produces output, even if no file exists

        # search using full template name (might include path)
        cmd  = '''@FindAfniDsetPath {}'''.format( ap_ssdict['template'] )
        com  = ab.shell_com(cmd, capture=do_cap)
        stat = com.run()

        # only include path to template if it is *not* '.'
        if com.so[0] != '.' :    path2dset = com.so[0] + '/'
        else:                    path2dset = ''

        if not(stat) and len(com.so) :
            HAVE_MAIN = 1
            ap_ssdict['main_dset'] = path2dset + btemp
        else:
            # search using basename of template only (no path)
            cmd  = '''@FindAfniDsetPath {}'''.format( btemp )
            com  = ab.shell_com(cmd, capture=do_cap)
            stat = com.run()

            if not(stat) and len(com.so) :
                HAVE_MAIN = 1
                ap_ssdict['main_dset'] = path2dset + btemp
            else:
                print("** ERROR: Cannot find specified template: {}"
                      "".format(ap_ssdict['template'] ))
                sys.exit(1)

    elif check_dep(ap_ssdict, ldep_alt1) :     # --------- check for anat
        HAVE_MAIN = 1
        ap_ssdict['main_dset'] = ap_ssdict['final_anat']

    elif check_dep(ap_ssdict, ldep_alt2) :     # --------- check for vr_base
        HAVE_MAIN = 1
        ap_ssdict['main_dset'] = ap_ssdict['vr_base_dset']

    else:
        print("+* WARN: no main dset (not template, anat_final nor vr_base)")

    # also add main_dset space info
    if HAVE_MAIN :
        cmd  = '''3dinfo -space {}'''.format( ap_ssdict['main_dset'] )
        com  = ab.shell_com(cmd, capture=do_cap)
        stat = com.run()
        if not(stat) and len(com.so) :
            ap_ssdict['main_dset_sp'] = com.so[0]

    return ap_ssdict


# ========================== 1D files/plots ==============================

# ['motion_dset', 'nt_orig']
# Now also check about: ['censor_dset']
def apqc_mot_VR6( ap_ssdict, obase, qcb, qci, run_style, jpgsize ):
    """Make image for individual motion estimates (6 dof), with possible
censoring.  Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'
jpgsize : int or float
    size of JPG for 'basic' 1dplot command (opt of same name)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# volreg motion pars, and censoring'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Setup text info for plot and toptxt
    ptitle  = 'Estimated motion parameters (volreg)'
    otoptxt = '6 volume registration motion parameters '
    otoptxt+= '(in {})'.format(ap_ssdict['motion_dset'])
    ttt2    = ''                   # stuff to (maybe) go on second line
    if ap_ssdict['cen_used'] :
        ptitle+= ', with combined censoring '
        ptitle+= '({} bars)'.format(ap_ssdict['cen_color'])
        ttt2  += 'with combined censoring'
    if ttt2 :
        otoptxt = [otoptxt, ttt2] 

    # Make info above and below images
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : ap_ssdict['rep_cen_str'],
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # Make image
    if run_style == 'basic' :
        cmd = '''
        1dplot                                                               \
            -sepscl                                                          \
            -volreg                                                          \
            {cen_cmd}                                                        \
            -xlabel           "vol"                                          \
            -title            "{ptitle}"                                     \
            -jpgs             {jpgsize} "{opref}"                            \
            "{motion_dset}"
        '''.format(**ap_ssdict, jpgsize=jpgsize, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    elif run_style == 'pythonic' :
        cmd = '''
        1dplot.py                                                            \
            -sepscl                                                          \
            -boxplot_on                                                      \
            -patches        {pats}                                           \
            -reverse_order                                                   \
            -infiles        "{motion_dset}"                                  \
            -ylabels        VOLREG                                           \
            {cen_cmd}                                                        \
            -xlabel         "vol index"                                      \
            -title          "{ptitle}"                                       \
            -prefix         "{opref}.jpg"
        '''.format(**ap_ssdict, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0

# ----------------------------------------------------------------------

# **NB**: it is likely that any changes in this function should mirror
# those of apqc_mot_enorm() and apqc_mot_enormoutlr

# ['outlier_dset', 'nt_orig'], also use 'censor_dset' and 'out_limit'
def apqc_mot_outlr( ap_ssdict, obase, qcb, qci, run_style, jpgsize ):
    """Make image for (only) outlier fractions and possible censoring.
Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'
jpgsize : int or float
    size of JPG for 'basic' 1dplot command (opt of same name)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# outlier frac and censoring'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Setup text info for plot and toptxt
    ptitle  = 'Outlier frac (black)'
    otoptxt = 'Volumetric fraction of outliers'
    ttt2    = ''                   # stuff to (maybe) go on second line
    if ap_ssdict['cen_used'] :
        if ap_ssdict['cen_have_out'] :
            ptitle+= ', with limit ({})'.format(ap_ssdict['cen_hline_color'])
            ttt2  += 'with limit'
        ptitle+= ' and combined censoring ({})'.format(ap_ssdict['cen_color'])
        ttt2  += ' and combined censoring'
    if ttt2 :
        otoptxt = [otoptxt, ttt2] 

    # Make info above and below images
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : ap_ssdict['rep_cen_str'],
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # Make output plots
    if run_style == 'basic' :
        cmd = '''
        1dplot                                                               \
            -one                                                             \
            {cen_cmd}                                                        \
            {cen_lim_out_yax}                                                \
            -jpgs               {jpgsize} "{opref}"                          \
            -aspect             2                                            \
            -xlabel             "vol"                                        \
            -title              "{ptitle}"                                   \
            {outlier_dset}                                                   \
            "{cen_lim_out}"
        '''.format(**ap_ssdict, jpgsize=jpgsize, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    elif run_style == 'pythonic' :
        cmd = '''
        1dplot.py                                                            \
            -boxplot_on                                                      \
            -patches            {pats}                                       \
            -colors             black                                        \
            -infiles            "{outlier_dset}"                             \
            -ylabels            "frac"                                       \
            {cen_cmd}                                                        \
            {cen_lim_out}                                                    \
            {cen_lim_out_yax}                                                \
            -xlabel             "vol index"                                  \
            -title              "{ptitle}"                                   \
            -prefix             "{opref}.jpg"
        '''.format( **ap_ssdict, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0

# ----------------------------------------------------------------------

# **NB**: it is likely that any changes in this function should mirror
# those of apqc_mot_outlr and apqc_mot_enormoutlr

# ['censor_dset', 'enorm_dset', 'mot_limit', 'nt_orig']
def apqc_mot_enorm( ap_ssdict, obase, qcb, qci, run_style, jpgsize ):
    """Make image for (only) motion and possible censoring.  Also create
text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'
jpgsize : int or float
    size of JPG for 'basic' 1dplot command (opt of same name)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# mot enorm and censoring'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Setup text info for plot and toptxt
    ptitle  = 'Mot enorm (black)'
    otoptxt = 'Motion Euclidean norm (enorm)'
    ttt2    = ''                   # stuff to (maybe) go on second line
    if ap_ssdict['cen_used'] :
        if ap_ssdict['cen_have_mot'] :
            ptitle+= ', with limit ({})'.format(ap_ssdict['cen_hline_color'])
            ttt2  += 'with limit'
        ptitle+= ' and combined censoring ({})'.format(ap_ssdict['cen_color'])
        ttt2  += ' and combined censoring'
    if ttt2 :
        otoptxt = [otoptxt, ttt2] 

    # Make info above and below images
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : ap_ssdict['rep_cen_str'],
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    if run_style == 'basic' :
        cmd = '''
        1dplot                                                               \
            -one                                                             \
            {cen_cmd}                                                        \
            {cen_lim_mot_yax}                                                \
            -jpgs               {jpgsize} "{opref}"                          \
            -aspect             2                                            \
            -xlabel             "vol"                                        \
            -title              "{ptitle}"                                   \
            {enorm_dset}                                                     \
            "{cen_lim_mot}"
        '''.format(**ap_ssdict, jpgsize=jpgsize, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    elif run_style == 'pythonic' :
        cmd = '''
        1dplot.py                                                            \
            -boxplot_on                                                      \
            -patches            {pats}                                       \
            -colors             black                                        \
            -infiles            "{enorm_dset}"                               \
            -ylabels            "enorm (~mm)"                                \
            {cen_cmd}                                                        \
            {cen_lim_mot}                                                    \
            {cen_lim_mot_yax}                                                \
            -xlabel             "vol index"                                  \
            -title              "{ptitle}"                                   \
            -prefix             "{opref}.jpg"
        '''.format( **ap_ssdict, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0

# ---------------------------------------------------------------------
# [PT: Jan 13, 2019] combo img when both motion and outliers are calc'ed

# ['outlier_dset', 'out_limit', 'enorm_dset', 'mot_limit',
# 'censor_dset', 'nt_orig']
def apqc_mot_enormoutlr( ap_ssdict, obase, qcb, qci, run_style ):
    """Make image for both motion and outliers, with possible censoring.
Also create text for above/below images.

For now, this function *only* runs in 'pythonic' style.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# mot enorm plus outlier frac, and censoring'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Setup text info for plot and toptxt
    ptitle  = 'Mot enorm and outlier frac (black)'
    otoptxt = 'Motion Euclidean norm (enorm) and outlier fraction'
    ttt2    = ''                   # stuff to (maybe) go on second line
    if ap_ssdict['cen_used'] :
        if ap_ssdict['cen_have_mot'] or ap_ssdict['cen_have_out'] :
            ptitle+= ', with limit ({})'.format(ap_ssdict['cen_hline_color'])
            ttt2  += 'with limit'
        ptitle+= ' and combined censoring ({})'.format(ap_ssdict['cen_color'])
        ttt2  += ' and combined censoring'
    if ttt2 :
        otoptxt = [otoptxt, ttt2] 

    # Make info above and below images
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : ap_ssdict['rep_cen_str'],
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # A truism for this plot, at the moment
    if run_style == 'pythonic' :
        cmd = '''
        1dplot.py                                                            \
            -boxplot_on                                                      \
            -patches            {pats}                                       \
            -colors             black                                        \
            -infiles            "{enorm_dset}" "{outlier_dset}"              \
            -ylabels            "enorm (~mm)"  "outlier frac"                \
            {cen_cmd}                                                        \
            {cen_lim_all}                                                    \
            {cen_lim_all_yax}                                                \
            -xlabel             "vol index"                                  \
            -title              "{ptitle}"                                   \
            -prefix             "{opref}.jpg"
        '''.format( **ap_ssdict, ptitle=ptitle, opref=opref )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0

# ---------------------------------------------------------------------

# ['combine_method'] -> 'm_tedana'
def apqc_mecho_mtedana( ap_ssdict, obase, qcb, qci, comb_meth ):
    """When MEICA group's tedana (mtedana) is used, copy their QC HTML
into the APQC dirs and make buttons that link to it.  Also create text
for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
comb_meth : str
    name of the ME combination method used

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat_mted = opref + '.dat'
    odir_mtedana = ap_ssdict['odir_qc'] + '/' + 'dir_mtedana'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# multi-echo (mecho) processing via MEICA group TEDANA'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Copy tedana QC figure dirs.  NB: using 'rsync -R ...' here,
    # instead of 'cp --parents ...', because '--parents' doesn't exist
    # on Mac version of cp.
    cmd    = '\\mkdir -p {}'.format(odir_mtedana)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    cmd    = '\\rsync -avR tedana_r*/figures {}/'.format(odir_mtedana)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    cmd    = '\\rsync -avR tedana_r*/tedana*.html {}/'.format(odir_mtedana)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Make a text file with necessary tedana info: button text and
    # link(s)
    all_ted = glob.glob('tedana_r*')
    dattxt  = ''
    for ted in all_ted:
        dattxt+= 'TEXT: Open: {} report\n'.format(ted)
        dattxt+= 'LINK: dir_mtedana/{}/tedana_report.html\n'.format(ted)
        dattxt+= '\n'
    fff = open(odat_mted, 'w')
    fff.write(dattxt)
    fff.close()
    
    # text above buttons
    otoptxt = []
    otoptxt.append("ME combine_method: {}".format( comb_meth ))
    otoptxt.append("Links to TEDANA QC html pages")

    # Make info above images
    otopdict = {
        'itemtype'    : 'BUTTON',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0


# ---------------------------------------------------------------------
# [PT: Dec 23, 2018] add in viewing censor dset, if present
# [PT: Apr 23, 2021] add "-ylabels_maxlen", to wrap long labels


# ['xmat_stim']
def apqc_regr_stims( ap_ssdict, obase, qcb, qci, run_style, jpgsize ):
    """Make image for individual ideal response regressors of interest,
with possible censoring and perhaps even boxplots (for pythonic mode).
Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'
jpgsize : int or float
    size of JPG for 'basic' 1dplot command (opt of same name)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# individual regressors of interest in X-matrix'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # plot info: labels from xmat file
    cmd    = '1d_tool.py -verb 0 -infile '
    cmd   += '{} -show_labels'.format(ap_ssdict['xmat_stim'])
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    labels = com.so[0]

    # Setup text info for plot and toptxt
    ptitle  = 'Regressors of interest in the X-matrix'
    otoptxt = 'Regressors of interest '
    otoptxt+= '(per stim, in {})'.format(ap_ssdict['xmat_stim'])
    if ap_ssdict['cen_used'] :
        ptitle += ' and combined censoring '
        ptitle += '({} bars)'.format(ap_ssdict['cen_color'])
        otoptxt+= ' and combined censoring'

    # Make info above and below images
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : ap_ssdict['rep_cen_str'],
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # Make images: 1D plots (and maybe boxplots)
    if run_style == 'basic' :
        cmd = '''
        1dplot                                                               \
            -sepscl                                                          \
            {cen_cmd}                                                        \
            -jpgs         $jpgsize "{opref}"                                 \
            -aspect       2                                                  \
            -xlabel       "vol"                                              \
            -title        "{ptitle}"                                         \
            {xmat_stim}
        '''.format( **ap_ssdict, opref=opref, ptitle=ptitle )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    elif run_style == 'pythonic' :
        cmd = '''
        1dplot.py                                                            \
            -reverse_order                                                   \
            -boxplot_on                                                      \
            -patches         {pats}                                          \
            -sepscl                                                          \
            -infiles         {xmat_stim}                                     \
            -xlabel          "vol"                                           \
            -ylabels         {labels}                                        \
            -ylabels_maxlen  7                                               \
            {cen_cmd}                                                        \
            -title           "{ptitle}"                                      \
            -prefix          "{opref}.jpg"
        '''.format( **ap_ssdict, opref=opref, ptitle=ptitle, labels=labels )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0

# ------------------------------------------------------------------

# ['sum_ideal']
def apqc_regr_ideal( ap_ssdict, obase, qcb, qci, run_style, jpgsize ):
    """Make image for sum of ideal response regressors of interest, with
possible censoring and perhaps even boxplots (for pythonic mode).  Also
create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'
jpgsize : int or float
    size of JPG for 'basic' 1dplot command (opt of same name)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# sum of regressors of interest in X-matrix'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # plot info
    labels = "regressor sum"

    # Setup text info for plot and toptxt
    ptitle  = 'Sum of regressors of interest in the X-matrix'
    otoptxt = 'Sum of regressors of interest '
    otoptxt+= '(in {})'.format(ap_ssdict['sum_ideal'])
    if ap_ssdict['cen_used'] :
        ptitle += ' and combined censoring '
        ptitle += '({} bars)'.format(ap_ssdict['cen_color'])
        otoptxt+= ' and combined censoring'

    # Make info above and below images
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : ap_ssdict['rep_cen_str'],
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # Make images: 1D plots (and maybe boxplots)
    if run_style == 'basic' :
        cmd = '''
        1dplot                                                               \
            -sepscl                                                          \
            {cen_cmd}                                                        \
            -jpgs         $jpgsize "{opref}"                                 \
            -aspect       2                                                  \
            -xlabel       "vol"                                              \
            -title        "{ptitle}"                                         \
            {sum_ideal}
        '''.format( **ap_ssdict, opref=opref, ptitle=ptitle )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    elif run_style == 'pythonic' :
        cmd = '''
        1dplot.py                                                            \
            -boxplot_on                                                      \
            -patches     {pats}                                              \
            -colors      black                                               \
            -sepscl                                                          \
            -infiles     {sum_ideal}                                         \
            -xlabel      "vol"                                               \
            -ylabels     "{labels}"                                          \
            {cen_cmd}                                                        \
            -title       "{ptitle}"                                          \
            -prefix      "{opref}.jpg"
        '''.format( **ap_ssdict, opref=opref, ptitle=ptitle, labels=labels )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0


# ========================== images ================================

# ----------------------------------------------------------------------

# ['vr_base_dset'] OR ['anat_orig']
def apqc_vorig_all( ap_ssdict, obase, qcb, qci, ulay='' ):
    """Make images for some datasets (EPI, anatomical) in their original
spaces.  Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
ulay : str
    filename of (sole) input dset

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # Here, qci has additional roles of a string label, because we use
    # this same function to plot all ORIG vols (EPI or anat, at the
    # moment)

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    # colorbar/range stuff
    perc_olay_top   = 98                    # %ile for top of pbar for olay
    olay_minval_str = "-pbar_posonly"
    olay_botval     = "0"
    cbar            = "gray_scale"

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# Check {} (ulay) in orig space ({} %ile topval for pbar)
             '''.format( qci, perc_olay_top )
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get ulay prefix and obliquity
    cmd    = '3dinfo -prefix -obliquity ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    ulay_pref = lll[0]
    ulay_obl  = float(lll[1])

    # get ulay min/max (for text info) and perc olay (for cbar)
    cmd    = '3dBrickStat -slow -perclist 3 0 100 {} {}'.format(perc_olay_top,
                                                                ulay)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    ulay_min    = float(lll[1])
    ulay_max    = float(lll[3])
    olay_topval = float(lll[5])

    # Make QC images
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              {ulay}                                            \
        -olay              {ulay}                                            \
        -ulay_range_nz     0 {olay_topval}                                   \
        -func_range        {olay_topval}                                     \
        -box_focus_slices  AMASK_FOCUS_ULAY                                  \
        -cbar              {cbar}                                            \
        {olay_minval_str}                                                    \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -pbar_comm_range   "{perc_olay_top}{pstr}"                           \
        -pbar_for          "dset"                                            \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            2                                                 \
        -opacity           9                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        {odoafni}                                         \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -do_clean
    '''.format( ulay=ulay, olay_topval=olay_topval, cbar=cbar,
                olay_minval_str=olay_minval_str, opbarrt=opbarrt,
                perc_olay_top=perc_olay_top, pstr='%ile in vol',
                opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # minor tweaks/formatting/expanding
    if qci == "EPI":   ulay_comm = ' (volreg base)'
    else:              ulay_comm = ''
    if qci == "anat":  qci_comm = 'Anatomical'
    else:              qci_comm = qci

    # text above images
    otoptxt = []
    otoptxt.append('{} in original space{}'.format( qci_comm, ulay_comm ))
    otoptxt.append('dset: {} ({})'.format( ulay_pref, qci ))

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # text below images
    osubtxt = []
    osubtxt.append('{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname))
    ttt = 'range: [{:.3f}, {:.3f}]; '.format(ulay_min, ulay_max)
    ttt+= 'obliquity: {:.3f}'.format(ulay_obl)
    osubtxt.append(ttt)

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
        'subtext'     : osubtxt,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text; **special case: olay_name=ulay!
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=ulay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0

# ----------------------------------------------------------------------

# ['vr_base_dset', 'copy_anat']
def apqc_vorig_olap( ap_ssdict, obase, qcb, qci ):
    """Make images for initial EPI-anatomical overlap in their original
spaces, both with and without applying obliquity.  Also create text
for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    oimg      = opref + '.sag'                       # bc of quirk of prog
    otopjson  = opref + '.sag.json'
    otopjson2 = opref + '.sag_DEOB.json'
    otopdeobtxt2     = opref + '.sag_DEOB.txt'
    otopdeobtxt2_bup = opref + '.sag_DEOB.txt_info'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# init overlap between EPI (ulay) and anatomical (olay)'
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get ulay prefix
    ulay   = ap_ssdict['copy_anat']
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix
    olay   = ap_ssdict['vr_base_dset']
    cmd    = '3dinfo -prefix ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref = com.so[0].strip()

    # Make QC images
    cmd = '''
    @djunct_overlap_check                                                    \
        -ulay              {ulay}                                            \
        -olay              {olay}                                            \
        -box_focus_slices  AMASK_FOCUS_ULAY                                  \
        -opacity           4                                                 \
        -no_cor                                                              \
        -no_axi                                                              \
        -montx_cat         1                                                 \
        -monty_cat         1                                                 \
        -montx             7                                                 \
        -prefix            {oimg}
    '''.format(ulay=ulay, olay=olay, oimg=oimg)
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # Rename this file: won't be in QC, but can be viewed, if desired
    if os.path.isfile( otopdeobtxt2 ) :
        cmd     = '''\\mv {} {}'''.format(otopdeobtxt2, otopdeobtxt2_bup)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    # text above image1
    otoptxt = "Initial overlap, no obliquity: anat (ulay) and EPI (olay)"

    # Make info above image1
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # text above image2
    otoptxt2 = "Initial overlap, applying obliquity: anat (ulay) and EPI (olay)"

    # Make info above image2
    otopdict2 = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt2,
    }
    with open(otopjson2, 'w', encoding='utf-8') as fff:
        json.dump( otopdict2, fff, ensure_ascii=False, indent=4 )

    return 0

#-------------------------------------------------------------------------

# ['mask_dset', 'template']
def apqc_gen_mask2final( ap_ssdict, obase, qcb, qci, ulay, focusbox ):
    """Make images for the mask on the final dset.  Also create text
for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
ulay : str
    filename of underlay dataset
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    com    = ab.shell_com('# check EPI mask over main_dset', capture=do_cap)
    stat   = com.run()

    # get ulay prefix (fname from arg)
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix
    olay   = ap_ssdict['mask_dset']
    cmd    = '3dinfo -prefix ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref = com.so[0].strip()

    # Make QC images
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              {ulay}                                            \
        -box_focus_slices  {focusbox}                                        \
        -olay              {olay}                                            \
        -cbar              {cbar}                                            \
        -ulay_range        0% 120%                                           \
        -func_range        1                                                 \
        -olay_alpha        No                                                \
        -olay_boxed        No                                                \
        -set_subbricks     0 0 0                                             \
        -opacity           4                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            1                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        {odoafni}                                         \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay,
                opbarrt=opbarrt, 
                cbar='Reds_and_Blues_Inv', opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # minor formatting
    olay_desc = 'final EPI mask coverage'
    if   qcb == 'va2t' :   ulay_desc = 'template dset'
    elif qcb == 've2a' :   ulay_desc = 'final anatomical dset'
    elif qcb == 'vorig' :  ulay_desc = 'volreg base dset'
    else:                  ulay_desc = '-'

    # text above images
    otoptxt = []
    otoptxt.append("ulay: {} ({})".format(ulay_pref, ulay_desc))
    otoptxt.append('olay: {} ({})'.format(olay_pref, olay_desc))

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images (leads to sag mont being shown)
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0

# ----------------------------------------------------------------------

# ['final_anat', 'final_epi_dset'],
# ['final_anat', 'final_epi_unif_dset']
def apqc_ve2a_epi2anat( ap_ssdict, obase, qcb, qci, focusbox, dice_file ):
    """Make images for EPI-anatomical alignment.  Also create text for
above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)
dice_file : str
    *not currently used*, but some day will be name of dice file to
    report overlap quantity

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# alignment of EPI (ulay) and anatomical (olay) in final sp'
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # [pt: June 5, 2022] because we want both better contrast for
    # 'normal' ulay=EPI, and also possibility of having normalized
    # brightness EPI
    ulay_ran = [1, 95]
    umin_fac = 0.2

    # get ulay prefix
    ulay   = ap_ssdict['final_epi_dset']
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix
    olay   = ap_ssdict['final_anat']
    cmd    = '3dinfo -prefix ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref = com.so[0].strip()

    # Make QC images
    cmd = '''
    @djunct_edgy_align_check                                                 \
        -olay              {olay}                                            \
        -box_focus_slices  {focusbox}                                        \
        -ulay              {ulay}                                            \
        -use_olay_grid     wsinc5                                            \
        -ulay_range_am     "{umin}%" "{umax}%"                               \
        -ulay_min_fac      {umin_fac}                                        \
        -blowup            2                                                 \
        -no_cor                                                              \
        -prefix            {opref}
    '''.format(olay=olay, focusbox=focusbox, ulay=ulay,
               umin=ulay_ran[0], umax=ulay_ran[1], umin_fac=umin_fac,
               opref=opref )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # Make QC images: *dry run only*, for AV/NV
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              "{ulay}"                                          \
        -box_focus_slices  "{focusbox}"                                      \
        -olay              "{olay}"                                          \
        -cbar              "{cbar}"                                          \
        -ulay_range        0% 120%                                           \
        -func_range_perc   98                                                \
        -pbar_posonly                                                        \
        -olay_alpha        No                                                \
        -olay_boxed        No                                                \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            1                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        "{odoafni}"                                       \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -c2s_text2     "++ Hover over image, hit 'o' to toggle olay on/off"  \
        -dry_run                                                             \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay,
                opbarrt=opbarrt, 
                cbar='gray_scale flip', opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # text above images
    otoptxt = []
    otoptxt.append('ulay: {} ({})'.format( ulay_pref, 'EPI' ))
    otoptxt.append('olay: {} ({})'.format( olay_pref, 'anat edges' ))

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images (leads to sag mont being shown)
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    ### [PT: Aug 18, 2022] ignore this for now---the patterns are more
    ### important
    # Dice coef info
    #if dice_file :
    #    dice = lah.read_dat(dice_file)
    #else:
    #    dice = 'unknown'
    #osubtext2 = "Dice coefficient (EPI-anatomical masks): {}".format(dice)

    return 0

# ----------------------------------------------------------------------

# ['final_anat', 'template']
def apqc_va2t_anat2temp( ap_ssdict, obase, qcb, qci, focusbox, dice_file ):
    """Make images for anatomical-template alignment.  Also create text
for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)
dice_file : str
    *not currently used*, but some day will be name of dice file to
    report overlap quantity

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# alignment of anatomical (ulay) and template (olay) in final sp'
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get ulay prefix
    ulay   = ap_ssdict['final_anat']
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix
    olay   = ap_ssdict['main_dset']
    cmd    = '3dinfo -prefix ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref = com.so[0].strip()

    # Make QC images
    cmd = '''
    @djunct_edgy_align_check                                                 \
        -olay              {olay}                                            \
        -box_focus_slices  {focusbox}                                        \
        -ulay              {ulay}                                            \
        -blowup            2                                                 \
        -no_cor                                                              \
        -prefix            {opref}
    '''.format(olay=olay, focusbox=focusbox, ulay=ulay,
               opref=opref)
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()


    # Make QC images: *dry run only*, for AV/NV
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              "{ulay}"                                          \
        -box_focus_slices  "{focusbox}"                                      \
        -olay              "{olay}"                                          \
        -cbar              "{cbar}"                                          \
        -ulay_range        0% 120%                                           \
        -func_range_perc   98                                                \
        -pbar_posonly                                                        \
        -olay_alpha        No                                                \
        -olay_boxed        No                                                \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            1                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        "{odoafni}"                                       \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -c2s_text2     "++ Hover over image, hit 'o' to toggle olay on/off"  \
        -dry_run                                                             \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay,
                opbarrt=opbarrt, 
                cbar='gray_scale', opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()


    # minor formatting
    olay_desc = 'template edges, {} space'.format(ap_ssdict['main_dset_sp'])

    # text above images
    otoptxt = []
    otoptxt.append('ulay: {} ({})'.format( ulay_pref, 'anat' ))
    otoptxt.append('olay: {} ({})'.format( olay_pref, olay_desc ))

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images (leads to sag mont being shown)
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0


# complicated/tiered dependencies...
def apqc_regr_corr_errts( ap_ssdict, obase, qcb, qci, 
                          ulay, focusbox, corr_brain ):
    """Make images of the correlation map of the average residual signal
(corr_errts), which complements GCOR information. Also create text for
above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
ulay : str
    filename of underlay dataset
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)
corr_brain : str
    filename of the correlation map of the average errts (error 
    time series, AKA residual)

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# check ave errts corr through brain'
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # parameters for images
    cbar    = 'Reds_and_Blues_Inv'
    pbar_cr = 'Pearson r'
    pbar_tr = 'alpha+boxed on' 

    # get ulay prefix (name from arg)
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix
    olay   = corr_brain
    cmd    = '3dinfo -prefix ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref = com.so[0].strip()

    # Make images
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              {ulay}                                            \
        -box_focus_slices  {focusbox}                                        \
        -olay              {olay}                                            \
        -cbar              {cbar}                                            \
        -ulay_range        0% 120%                                           \
        -func_range        0.6                                               \
        -thr_olay          0.3                                               \
        -olay_alpha        Yes                                               \
        -olay_boxed        Yes                                               \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -pbar_comm_range   "{pbar_cr}"                                       \
        -pbar_comm_thr     "{pbar_tr}"                                       \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            2                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        {odoafni}                                         \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay, cbar=cbar,
                opbarrt=opbarrt, pbar_cr=pbar_cr, pbar_tr=pbar_tr, 
                opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # text above images
    otoptxt = "olay: corr of WB-average errts "
    otoptxt+= "with each voxel ({})".format(olay_pref)

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
        'ic_file'     : 'run_instacorr_errts.tcsh',
        'gv_file'     : 'run_graphview_errts.tcsh',
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # text below images
    osubtxt = '{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
        'subtext'     : osubtxt,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0

#-------------------------------------------------------------------------

# complicated/tiered dependencies...
def apqc_vstat_seedcorr( ap_ssdict, obase, qcb, qci, 
                         ulay, focusbox, seed ):
    """Make images of the volumetric statistics (and effect estimate, when
possible!) information.  These images are made in **highlight** mode,
using transparent thresholding. Also create text for above/below
images.

This function creates new datasets and 1D time series (from the seed
location) in the ap_ssdict['vstat_dir'] directory, which exists in the
AP results dir.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
ulay : str
    filename of underlay dataset
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)
seed : afni_seeds
    an instance of an object that holds useful information about the
    present seed, such as coords and str labels

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# view errts seed-based correlation: {}'.format(seed.roi_label)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # do we have a mask here? changes how some stat thresholds are gotten
    HAVE_MASK = check_dep(ap_ssdict, ['mask_dset'])

    # image settings
    seed_loc_gen = coord_to_gen_sys(seed.xyz)   # general coords: -4R, etc.
    cbar         = 'Reds_and_Blues_Inv'
    pbar_cr      = 'Pearson r'
    pbar_tr      = 'alpha+boxed on'

    # Make seedcorr data (ave time series and corr map volume) in new AP dir
    vdir         = ap_ssdict['vstat_dir']
    bname        = 'seed_{}_in_{}'.format(seed.roi_label, seed.netw)
    t1dfile      = vdir + '/' + bname + '_ave_ts.txt'
    tcorrvol     = vdir + '/' + bname + '_corr_map.nii.gz'

    # get ulay prefix (name from arg)
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix and voxel volume
    olay   = ap_ssdict['errts_dset']
    cmd    = '3dinfo -prefix -voxvol ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    olay_pref = lll[0]
    voxvol    = float(lll[1])
    seed_rad  = 2 * (voxvol**0.3334)

    # Make ave time series
    cmd    = '''
    3dmaskave                                                                \
        -quiet                                                               \
        -dball  {sx} {sy} {sz} {seed_rad}                                    \
        {errts_dset}                                                         \
        > {t1dfile}
    '''.format( sx=seed.xyz[0], sy=seed.xyz[1], sz=seed.xyz[2], 
                seed_rad=seed_rad, errts_dset=ap_ssdict['errts_dset'],
                t1dfile=t1dfile )
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Make corr map with ave time series
    cmd    = '''
    3dTcorr1D                                                                \
        -overwrite                                                           \
        -prefix       {tcorrvol}                                             \
        {errts_dset}                                                         \
        {t1dfile}
    '''.format( tcorrvol=tcorrvol, errts_dset=ap_ssdict['errts_dset'],
                t1dfile=t1dfile )
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              {ulay}                                            \
        -box_focus_slices  {focusbox}                                        \
        -olay              {tcorrvol}                                        \
        -cbar              {cbar}                                            \
        -ulay_range        0% 120%                                           \
        -func_range        0.6                                               \
        -thr_olay          0.3                                               \
        -olay_alpha        Yes                                               \
        -olay_boxed        Yes                                               \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -pbar_comm_range   "{pbar_cr}"                                       \
        -pbar_comm_thr     "{pbar_tr}"                                       \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            2                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        {odoafni}                                         \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, tcorrvol=tcorrvol, cbar=cbar,
                opbarrt=opbarrt, pbar_cr=pbar_cr, pbar_tr=pbar_tr,
                opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # minor formatting
    if seed.netw :    netw_str = """'{}' in {}""".format(seed.roi_label, 
                                                         seed.netw)
    else:             netw_str = """'{}'""".format(seed.roi_label)

    loc_str = """rad = {:.2f} mm ({}, {}, {})""".format(seed_rad, 
                                                        seed_loc_gen[0],
                                                        seed_loc_gen[1],
                                                        seed_loc_gen[2])
    coords  = ' '.join([str(cc) for cc in seed.xyz])


    # text above images
    otoptxt = []
    otoptxt.append('olay: {} (in {})'.format('seed-based corr map', olay_pref))
    otoptxt.append('seed: {}, {}'.format(netw_str, loc_str))

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
        'ic_file'     : 'run_instacorr_errts.tcsh',
        'ic_args'     : coords,
        'gv_file'     : 'run_graphview_errts.tcsh',
        'gv_args'     : coords,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # text below images
    osubtxt = '{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'subtext'     : osubtxt,
        'nv_html'     : onvhtml_name,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text; **special case for seedcorr map
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=tcorrvol, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0
    
#-------------------------------------------------------------------------

# ['stats_dset', 'mask_dset', 'final_anat']
# ['template'] # secondary consideration
def apqc_vstat_stvol( ap_ssdict, obase, qcb, qci, 
                      ulay, focusbox, vso ):
    """Make images of the volumetric statistics (and effect estimate, when
possible!) information.  These images are made in **highlight** mode,
using transparent thresholding. Also create text for above/below
images.

We now also read back in the pbar JSON dictionary, to help with
driving AFNI-view and NiiVue instances.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
ulay : str
    filename of underlay dataset
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)
vso : vstat_obj
    an instance of an object that holds a lot of effect estimate,
    statistic, and colorbar information for plotting

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# peruse statistical results: '
    cmd   += 'thr [{}], olay [{}]'.format(vso.thr_index, vso.olay_index )
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # do we have a mask here? changes how some stat thresholds are gotten
    HAVE_MASK = check_dep(ap_ssdict, ['mask_dset'])

    # values used in some scenarios without a mask
    pvalue_thr = 0.001
    tcoef      = '__tmp_coef_vol.nii.gz'

    # what will minval of pbar be? 0, or -max?
    if vso.olay_posonly :
        olay_minval_str = "-pbar_posonly"
        pbar_min        = "0"
    else:
        olay_minval_str = "-pass"
        pbar_min        = "-${olay_topval}" # $olay_topval is defined below

    # get ulay prefix (name from arg)
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix and AFNI view space
    olay   = ap_ssdict['stats_dset']
    cmd    = '3dinfo -prefix -av_space ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    olay_pref = lll[0]
    avsp      = lll[1]

    olaybrick = vso.olay_index
    cmd    = '3dinfo -label {}"[{}]"'.format(olay, olaybrick)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olaylabel = com.so[0].strip()

    thrbrick = vso.thr_index
    cmd    = '3dinfo -label {}"[{}]"'.format(olay, thrbrick)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    thrlabel = com.so[0].strip()

    cmd    = '3dAttribute BRICK_STATAUX {}"[{}]"'.format(olay, thrbrick)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    thr_dof = ' '.join(lll[3:]) # one or more ints, joined as strings

    if vso.thr_mode == 'percentile' :
        if HAVE_MASK :

            # set %iles 
            perc_olay_top = 99                 
            perc_thr      = 90                 
            perc_olay_fov = " in mask"

            # get threshold for stat dset from %ile in mask
            cmd    = '''
            3dBrickStat -slow -perclist 1 {} -mask {} {}"[{}]"
            '''.format(perc_thr, ap_ssdict['mask_dset'], 
                       olay, thrbrick)
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()
            lll    = com.so[0].split()
            thr_thresh = float(lll[1])

            # get top val for cbar of olay from %ile in mask
            cmd    = '''
            3dBrickStat -slow -perclist 1 {} -mask {} {}"[{}]"
            '''.format(perc_olay_top, ap_ssdict['mask_dset'], 
                       olay, olaybrick)
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()
            lll    = com.so[0].split()
            olay_topval = float(lll[1])

        else:    # no mask case

            # set %iles
            perc_olay_top = 99                 
            perc_thr      = 95            
            perc_olay_fov = " in full nonzero volume"

            # get threshold for stat dset from thrbrick %ile in mask
            cmd    = '''
            3dBrickStat -slow -non-zero -perclist 1 {} {}"[{}]"
            '''.format(perc_thr, olay, thrbrick)
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()
            lll    = com.so[0].split()
            thr_thresh = float(lll[1])

            # get top val for cbar of olay from olaybrick %ile in mask
            cmd    = '''
            3dBrickStat -slow -non-zero -perclist 1 {} {}"[{}]"
            '''.format(perc_olay_top, olay, olaybrick)
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()
            lll    = com.so[0].split()
            olay_topval = float(lll[1])

        pbar_comm_range = str(perc_olay_top)+'%ile' + perc_olay_fov
        pbar_comm_thr   = str(perc_thr)+'%ile' 
        pbar_comm_thr  += perc_olay_fov + ', alpha+boxed on'

    else:  # using pvalue, not %ile

        # get threshold for stat dset from pvalue
        cmd    = '''
        p2dsetstat                                                           \
            -quiet                                                           \
            -{}                                                              \
            -pval   {}                                                       \
            -inset  {}"[{}]"
        '''.format(vso.thr_sided, pvalue_thr, olay, thrbrick)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
        thr_thresh = float(com.so[0].strip())

        if HAVE_MASK :
            pvalue_olay_perctop = 99
            pvalue_olay_fov     = " in suprathresh mask voxels"

            # Make intermediate dataset
            cmd = '''
            3dcalc                                                           \
                -a       {olay}"[{olaybrick}]"                               \
                -b       {olay}"[{thrbrick}]"                                \
                -c       {mask_dset}                                         \
                -expr    "abs(a)*step(b-{thr_thresh})*step(c)"               \
                -prefix  {tcoef}
            '''.format( olay=olay, olaybrick=olaybrick, thrbrick=thrbrick,
                        mask_dset=ap_ssdict['mask_dset'], 
                        thr_thresh=thr_thresh, tcoef=tcoef )
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()

        else:
            pvalue_olay_perctop = 90
            pvalue_olay_fov     = " in suprathresh volume voxels"

            # Make intermediate dataset
            cmd = '''
            3dcalc                                                           \
                -a       {olay}"[{olaybrick}]"                               \
                -b       {olay}"[{thrbrick}]"                                \
                -expr    "abs(a)*step(b-{thr_thresh})"                       \
                -prefix  {tcoef}
            '''.format( olay=olay, olaybrick=olaybrick, thrbrick=thrbrick,
                        thr_thresh=thr_thresh, tcoef=tcoef )
            com    = ab.shell_com(cmd, capture=do_cap)
            stat   = com.run()

        # get top val for cbar of olay from olaybrick %ile in mask
        cmd    = '''
        3dBrickStat -slow -non-zero -perclist 1 {} {}
        '''.format(pvalue_olay_perctop, tcoef)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
        lll    = com.so[0].split()
        olay_topval = float(lll[1])
        # olay_botval = pbar_min

        # remove temp/intermediate dset
        cmd     = '''\\rm {}'''.format(tcoef)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

        pbar_comm_range = str(pvalue_olay_perctop)+"%ile" 
        pbar_comm_range+= pvalue_olay_fov
        pbar_comm_thr   = "{} p={}, {}".format( vso.thr_sided_txt, 
                                                pvalue_thr, 
                                                "alpha+boxed on" )

    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              {ulay}                                            \
        -box_focus_slices  {focusbox}                                        \
        -olay              {olay}                                            \
        -cbar              {cbar}                                            \
        {olay_minval_str}                                                    \
        -ulay_range        0% 120%                                           \
        -func_range        {olay_topval}                                     \
        -thr_olay          {thr_thresh}                                      \
        -olay_alpha        Yes                                               \
        -olay_boxed        Yes                                               \
        -set_subbricks     0 {olaybrick} {thrbrick}                          \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -pbar_comm_range   "{pbar_comm_range}"                               \
        -pbar_comm_thr     "{pbar_comm_thr}"                                 \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            2                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        {odoafni}                                         \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay,
                cbar=vso.olay_pbar, olay_minval_str=olay_minval_str, 
                olay_topval=olay_topval, thr_thresh=thr_thresh,
                olaybrick=olaybrick, thrbrick=thrbrick,
                opbarrt=opbarrt, pbar_comm_range=pbar_comm_range, 
                pbar_comm_thr=pbar_comm_thr, opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # minor formatting
    olay_desc = 'template edges, {} space'.format(ap_ssdict['main_dset_sp'])

    # text above images
    otoptxt = []
    otoptxt.append("olay: [{}] '{}' (in {})".format( olaybrick, olaylabel, 
                                                     olay_pref ))
    otoptxt.append(" thr: [{}] '{}' (df = {})".format( thrbrick, thrlabel,
                                                       thr_dof ))

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'av_file'     : odoafni,
        'ic_file'     : 'run_instacorr_errts.tcsh',
        'gv_file'     : 'run_graphview_errts.tcsh',
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # text below images
    osubtxt = '{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
        'subtext'     : osubtxt,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()
    
    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0

# -----------------------------------------------------------------

# [PT: Feb 22, 2021] add in TSNR plots for final dset

# ['tsnr_dset']
def apqc_regr_tsnr( ap_ssdict, obase, qcb, qci, 
                    ulay, focusbox, olay,
                    descrip="" ):
    """Make images of TSNR information, which can be done at a couple
different times during processing (descrip contains that
information). Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
ulay : str
    filename of underlay dataset
focusbox : str
    filename of dataset to use to focus in on part of the dataset
    (i.e., to ignore empty slices)
olay : str
    filename of overlay dataset (here, always TSNR volume)
descrip : str
    a string to be included in the text above images, which
    describes what the 4D time series used for the TSNR calc was

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.axi.json'
    osubjson = opref + '.sag.json'
    opbarrt  = opref + '.pbar'
    onvhtml  = opref + '.niivue.html'                # output niivue canvas
    odoafni  = 'run_' + oname + '.tcsh'              # AV script name

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '# calc TSNR ' + descrip
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # do we have a mask here?
    HAVE_MASK = check_dep(ap_ssdict, ['mask_dset'])

    # get ulay prefix (name from arg)
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay prefix, AFNI view space and label (name from arg)
    cmd    = '3dinfo -prefix -av_space -label ' + olay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    olay_pref = lll[0]
    avsp      = lll[1]
    olay_lab  = lll[2]

    otoptxt = "olay: {} {}".format(olay_pref, descrip)
    ttt2    = ''                   # stuff to (maybe) go on second line
    cbar    = 'CET_L17'
    olay_minval_str = '-pbar_posonly'

    if HAVE_MASK :
        loc   = 'in mask'
        lperc = [5, 95]

        # get mask prefix
        cmd    = '3dinfo -prefix ' + ap_ssdict['mask_dset']
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
        mask_pref = com.so[0].strip()
        ttt2   = "mask: {} (for percentile range)".format(mask_pref)

        # get percentile range values
        cmd = '3dBrickStat -slow -non-zero -perc_quiet '
        cmd+= '-mask {mask_dset} '.format(mask_dset=ap_ssdict['mask_dset'])
        cmd+= '-perclist {} {} {} '.format(len(lperc), lperc[0], lperc[1])
        cmd+= '{olay}'.format(olay=olay)

    else:
        loc   = 'in dset vol'
        lperc = [90, 98]

        # get percentile range values
        cmd = '3dBrickStat -slow -non-zero -perc_quiet '
        cmd+= '-perclist {} {} {} '.format(len(lperc), lperc[0], lperc[1])
        cmd+= '{olay}'.format(olay=olay)

    # run whichever 3dBrickStat cmd is appropriate
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    lll    = com.so[0].split()
    percvalA = int(float(lll[0]))    # int for simpler reporting
    percvalB = int(float(lll[1]))
    olay_topval = percvalB
    pbar_comm_range = "{}%ile {}".format(lperc[1], loc)
    pbar_comm_gen   = " info: {}-{}%ile ".format(lperc[0], lperc[1])
    pbar_comm_gen  += "TSNR {}: {} - {}".format(loc, percvalA, percvalB)

    cmd = '''
    @chauffeur_afni                                                      \
        -ulay              {ulay}                                        \
        -box_focus_slices  {focusbox}                                    \
        -olay              {olay}                                        \
        -cbar              {cbar}                                        \
        {olay_minval_str}                                                \
        -ulay_range        0% 120%                                       \
        -func_range        {olay_topval}                                 \
        -olay_alpha        No                                            \
        -olay_boxed        No                                            \
        -set_subbricks     0 0 0                                         \
        -opacity           5                                             \
        -pbar_saveim       "{opbarrt}.jpg"                               \
        -pbar_comm_range   "{pbar_comm_range}"                           \
        -pbar_comm_gen     "{pbar_comm_gen}"                             \
        -prefix            "{opref}"                                     \
        -save_ftype        JPEG                                          \
        -blowup            2                                             \
        -montx             7                                             \
        -monty             1                                             \
        -montgap           1                                             \
        -montcolor         black                                         \
        -set_xhairs        OFF                                           \
        -label_mode        1                                             \
        -label_size        4                                             \
        -no_cor                                                          \
        -cmd2script        {odoafni}                                     \
        -c2s_text          'APQC, {qcb}: {qci}'                          \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay,
                cbar=cbar, olay_minval_str=olay_minval_str, 
                olay_topval=olay_topval, 
                opbarrt=opbarrt, pbar_comm_range=pbar_comm_range, 
                pbar_comm_gen=pbar_comm_gen, opref=opref,
                odoafni=odoafni, qcb=qcb, qci=qci )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    if ttt2 :
        otoptxt = [otoptxt, ttt2] 

    # some IC+GV driving scripts
    if oname.endswith('fin') :
        ic_file = 'run_instacorr_errts.tcsh'
        ic_args = ''
        gv_file = 'run_graphview_errts.tcsh'
        gv_args = ''
    elif oname.endswith('vreg') :
        # have to figure out pb number for vreg (and AP uses r01 here)
        all_volreg = glob.glob("pb*volreg*HEAD")
        if len(all_volreg) :
            pb = all_volreg[0].split('.')[0]
            print('++ pb for volreg is:', pb)
            ic_file = 'run_instacorr_pbrun.tcsh'
            ic_args = '{} {}'.format(pb, 'r01')
            gv_file = 'run_graphview_pbrun.tcsh'
            gv_args = '{} {}'.format(pb, 'r01')

    # Make info above images
    otopdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'ic_file'     : ic_file,
        'ic_args'     : ic_args,
        'gv_file'     : gv_file,
        'gv_args'     : gv_args,
        'av_file'     : odoafni,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # text below images
    osubtxt = '{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)

    # store name of NiiVue html
    onvhtml_name = onvhtml.split('/')[-1]

    # Make info below images
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'nv_html'     : onvhtml_name,
        'subtext'     : osubtxt,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay, itemid=qci,
                                     verb=0 )
    fff = open(onvhtml, 'w')
    fff.write(nv_txt)
    fff.close()
    onvhtml_name = onvhtml.split('/')[-1]

    return 0

# -----------------------------------------------------------------

# [PT: June 27, 2019] expanding to include enorm, if available and
# in Pythonic mode
# [PT: Feb 23, 2021] moved here to 'mot' from 'regr'

# ['errts_dset', 'mask_dset']
# ['enorm_dset', 'nt_orig']    # [PT: June 27, 2019]
def apqc_mot_grayplot( ap_ssdict, obase, qcb, qci,
                       run_style ):
    """Make grayplot image, which can include the enorm/outlier/censoring
line plot above it (when using pythonic run_style).  Also create text
for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
run_style : str
    mode of running, such as 'pythonic' or 'basic'

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    opbarrt  = opref + '.pbar'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# grayplot of residuals'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # parameters and temp files
    grange      = 3.29                            # grayplot range value
    pbcomm      = "for normal distr, bounds of 0.001 prob tail"
    tmpvol_pref = '__tmp_ZXCV_img'
    tmp_gplot   = '__tmp_ZXCV_gplot.jpg'
    tmp_img     = '__tmp_img_enorm.jpg'

    # get errts prefix
    errts  = ap_ssdict['errts_dset']
    cmd    = '3dinfo -prefix ' + errts
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    errts_pref = com.so[0].strip()

    # get errts prefix
    mask  = ap_ssdict['mask_dset']
    cmd    = '3dinfo -prefix ' + mask
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    mask_pref = com.so[0].strip()

    # [PT: June 28, 2019] No longer getting the range from time series--
    # the values are basically normalized/Zscores, so just use
    # relations for Norm(0,1); z=3.29 corresponds to two-sided p=0.001

    # The grayplot itself: will be overwritten if using enorm
    cmd = '''
    3dGrayplot                                                               \
        -polort   -1                                                         \
        -pvorder                                                             \
        -dimen    1800 500                                                   \
        -range    {grange}                                                   \
        -input    {errts_dset}                                               \
        -mask     {mask_dset}                                                \
        -prefix   {tmp_gplot}
    '''.format(**ap_ssdict, grange=grange, tmp_gplot=tmp_gplot)
    com = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # Make image -> but just a cheating way to make a colorbar to use later
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              {mask_dset}                                       \
        -olay              {mask_dset}                                       \
        -box_focus_slices  AMASK_FOCUS_OLAY                                  \
        -cbar              gray_scale                                        \
        -func_range        {grange}                                          \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -pbar_comm_range   "{pbcomm}"                                        \
        -prefix            "{tmpvol_pref}"                                   \
        -save_ftype        JPEG                                              \
        -blowup            1                                                 \
        -montx             1                                                 \
        -monty             1                                                 \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -do_clean
    '''.format( mask_dset=ap_ssdict['mask_dset'], opbarrt=opbarrt,
                grange=grange, pbcomm=pbcomm, tmpvol_pref=tmpvol_pref )
    com = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # Make pbar json file (used by apqc_make_html.py)
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt )
    com = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # text above and below images
    otoptxt = "Grayplot ('-pvorder') of residuals dset: " + errts_pref
    ttt2    = ''
    osubtxt = '{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)
    uuu2    = ''

    # depending on censoring, add to otoptxt (via ttt2), and fill in
    # dictionary of plot settings
    pset = {} 
    if (ap_ssdict['cen_have_mot'] or ap_ssdict['cen_have_out']) and \
       (run_style == 'pythonic') :
        ttt2 = 'top:'
        pset['scale']        = ' -scale SCALE_TO_HLINE '
        pset['yaxis']        = ' -yaxis 0:3 '
        pset['infiles']      = ' -infiles '
        pset['colors']       = ' -colors '
        pset['censor_files'] = ''

        if ap_ssdict['cen_have_mot'] :
            lcol = 'blue'
            ttt2+= ' motion enorm ({})'.format(lcol)
            pset['colors'] += ' {} '.format(lcol)
            pset['infiles']+= ' {} '.format(ap_ssdict['enorm_dset'])

        if ap_ssdict['cen_have_out'] :
            lcol = 'green'
            if ap_ssdict['cen_have_mot'] :
                ttt2+= ' and'
            ttt2+= ' outlier frac ({})'.format(lcol)
            pset['colors'] += ' {} '.format(lcol)
            pset['infiles']+= ' {} '.format(ap_ssdict['outlier_dset'])

        if ap_ssdict['cen_used'] :
            ttt2+= ', with censoring ({})'.format(ap_ssdict['cen_color'])
            ppp = ' -censor_files "{}" '.format(ap_ssdict['censor_dset'])
            pset['censor_files']+= ppp

        uuu2+= " rows: ordered by similarity to top two principal comps "
        uuu2+= "in mask ({})".format(mask_pref)

        # Make grayplot image. NB: labels aren't used here: number of
        # pixels in x-dim matches grayplot!
        cmd = '''
        1dplot.py                                                            \
            -margin_off                                                      \
            -one_graph                                                       \
            -figsize        12 0.5                                           \
            -dpi            150                                              \
            -patches        {pats}                                           \
            {infiles}                                                        \
            {scale}                                                          \
            {yaxis}                                                          \
            {cen_cmd}                                                        \
            {cen_lim_all}                                                    \
            {colors}                                                         \
            -prefix         {tmp_img}
        '''.format( **pset, **ap_ssdict, tmp_img=tmp_img )
        com = ab.shell_com(cmd, capture=do_cap)
        com.run()

        # Glue images together
        cmd = '''
        @djunct_glue_imgs_vert                                               \
            -imbot   {tmp_gplot}                                             \
            -imtop   {tmp_img}                                               \
            -prefix  {opref}.jpg
        '''.format( tmp_gplot=tmp_gplot, tmp_img=tmp_img, opref=opref )
        com = ab.shell_com(cmd, capture=do_cap)
        com.run()

        # clean up a bit \rm (note use of double \\ in the python string)
        cmd     = '''\\rm {}'''.format(tmp_img)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    else:
        cmd     = '''\\cp {} {}.jpg'''.format(tmp_gplot, opref)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    # clean up a bit at end (note use of double \\ in the python string)
    # remove temp/intermediate dset
    cmd     = '''\\rm {} {}.*.jpg'''.format(tmp_gplot, tmpvol_pref)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # finalize output text info
    if ttt2 :
        otoptxt = [otoptxt, ttt2] 
    if uuu2 :
        osubtxt = [osubtxt, uuu2] 

    # Make info below images 
    otopdict = {
        'itemtype'    : '1D',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'subtext'     : osubtxt,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ========================== dat/txt ================================


# summary quantities from 1d_tool.py degree-o-freedom check
# ['xmat_regress']
def apqc_regr_df( ap_ssdict, obase, qcb, qci ):
    """Make a simple data table of text reporting on degrees of freedom
(DFs).  Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# degree of freedom (df) check for processing'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # create a text data file, summarizing DFs
    cmd    = '1d_tool.py -show_df_info -infile '
    cmd   += '{} > {}'.format(ap_ssdict['xmat_regress'], odat)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # text above data
    otoptxt = "Summary of degrees of freedom (DF) usage from processing"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'DAT',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ---------------------------------------------------------------------

# summary quantities from @ss_review_basic dumped to text file
# 1
def apqc_qsumm_ssrev( ap_ssdict, obase, qcb, qci ):
    """Make a simple display of the basic info output by @ss_review_basic.
Also create text for above this display.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# summary quantities (ss_review_basic) from processing'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Make (= copy) text file of ss_rev_basic info
    cmd    = '''\\cp {} {}'''.format(ap_ssdict["ss_review_dset"], odat)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # text above data
    otoptxt = "Basic summary quantities from processing"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'DAT',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0


# ========================== warn/txt ================================


# parse df info dset
# ['stats_dset', 'censor_dset', 'ss_review_dset', 'xmat_stim']
def apqc_warns_cen_stim( ap_ssdict, obase, qcb, qci ):
    """Make the text info which will be displayed for this warning, which
is about the fraction of censoring (during ideal response interval)
per stim.  Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check for warnings due to censoring per stim'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # reference guide of fractions for warning levels (respectively):
    # sev, med, mild
    cutoff_list = [0.6, 0.4, 0.2] 

    # calc numbers and warn levels to report (after getting the
    # ss_review_basic info as a dict)
    rev_dict = read_in_txt_to_dict( ap_ssdict['ss_review_dset'],
                                    tmp_name='__tmp_ss_rev.json' )
    cmd = '''1d_tool.py -verb 0 -infile {xmat_stim} -show_labels
    '''.format( **ap_ssdict )
    com = ab.shell_com(cmd, capture=1, save_hist=0)
    com.run()
    label_list = com.so[0].split() # list of all labels

    ntr_init_per_stim = rev_dict['num_TRs_per_stim_orig']
    ntr_cen_per_stim  = rev_dict['num_TRs_censored_per_stim']
    frac_tr_cen       = rev_dict['fraction_TRs_censored']

    # if >1 time series, above vars are each list; else, float
    try:
        nruns = len(ntr_init_per_stim)
    except:
        # convert to lists, for simpler scripting, with indexing
        nruns = 1
        ntr_init_per_stim = [ntr_init_per_stim]
        ntr_cen_per_stim  = [ntr_cen_per_stim]
        frac_tr_cen       = [frac_tr_cen]

    if len(label_list) != nruns :
        print("** ERROR: generating censor fraction\n"
              "   Found {} labels (doesn't match {} runs):\n"
              "     {}\n" 
              "   Skipping this QC block\n"
              "".format(len(label_list), nruns, ', '.join(label_list)))
        return 1

    # use the max frac to determine warning level
    max_frac_cen   = max(frac_tr_cen)
    max_perc_cen   = 100*max_frac_cen
    warn_level     = get_warn_level_3( max_frac_cen, cutoff_list)

    # to vertically align text
    labmax = max([len(lab) for lab in label_list])

    # Make output text file
    dattxt = 'Max indiv stim censoring fraction : {:5.1f}%'.format(max_perc_cen)
    dattxt+= '\n' + '-'*44 + '\n'
    for ii in range(nruns):
        A = int(ntr_cen_per_stim[ii])
        B = int(ntr_init_per_stim[ii])
        C = label_list[ii]
        D = 100*float(frac_tr_cen[ii])
        dattxt+= "Censored {:4d} of {:4d} ".format(A, B)
        dattxt+= "TRs of '{:<{}s}' stim : {:5.1f}%\n".format(C, labmax, D)
    dattxt+= "\n"
    fff = open(odat, 'w')
    fff.write(dattxt)
    fff.close()

    # text above data
    otoptxt = "Censor fraction warnings (per stim)"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ---------------------------------------------------------------------

# parse df info dset
# ['df_info_dset', 'censor_dset']
def apqc_warns_cen_total( ap_ssdict, obase, qcb, qci ):
    """Make the text info which will be displayed for this warning, which
is about the total number of degrees of freedom (DFs) that have been
used up in the modeling due to censoring.  Also create text for
above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check for warnings due to censoring (total)'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # reference guide of fractions for warning levels (respectively):
    # sev, med, mild
    cutoff_list = [0.5, 0.2, 0.1]

    # calc numbers and warn levels to report (from DF text info, now dict)
    df_dict    = read_in_txt_to_dict( ap_ssdict['df_info_dset'],
                                      tmp_name='__tmp_df_info.json' )
    ninit      = df_dict["initial_DF"]
    ncen       = df_dict["DF_used_for_censoring"]
    frac_cen   = float(ncen) / float(ninit)
    warn_level = get_warn_level_3( frac_cen, cutoff_list)

    # Make output text file
    dattxt  = 'Censored {:4d} of {:4d} '.format(int(ncen), int(ninit))
    dattxt += 'total time points : {:5.1f}%\n\n'.format(100*frac_cen)
    fff = open(odat, 'w')
    fff.write(dattxt)
    fff.close()

    # text above data
    otoptxt = "General censor fraction warnings"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level,
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ---------------------------------------------------------------------

# Text warning, goes to dir_img output
# ['xmat_regress']
def apqc_warns_xmat( ap_ssdict, obase, qcb, qci,
                     fname = '' ):
    """Make the text info which will be displayed for this warning, which
is about correlations within the X-matrix.  Also create text for
above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
fname : str
    the name of the text file that contains the relevant warning/info, 
    which will be displayed in the HTML.

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check for correlation matrix warnings'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # parse text file for warning severity
    warn_level = "undecided"
    if fname : 
        txt = lah.read_dat(fname)
        if txt.__contains__("IDENTICAL:") or txt.__contains__("high:") :
            warn_level = "severe"
        elif txt.__contains__("medium:") :
            warn_level = "mild"
        elif txt.__contains__("no warnings") :
            warn_level = "none"
            
    # create a text data file, summarizing DFs
    cmd    = '1d_tool.py -show_cormat_warnings -infile '
    cmd   += '{} > {}'.format(ap_ssdict['xmat_regress'], odat)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # text above data
    otoptxt = "Regression matrix correlation warnings"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ----------------------------------------------------------------------

# Text warning, 3dDeconvolve issues
# ['decon_err_dset']
def apqc_warns_decon( ap_ssdict, obase, qcb, qci ):
    """Make the text info which will be displayed for this warning, which
is about 3dDeconvolve execution.  Also create text for above/below
images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check for 3dDeconvolve warnings'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get name of text file that might have warnings
    fname  = ap_ssdict['decon_err_dset']

    # parse text file for warning severity
    warn_level = "undecided"
    if fname :  
        txt = lah.read_dat(fname)
        if txt.strip() == '' :
            warn_level = "none"
        ### could add more conditions here, as the need arises
        #elif txt.__contains__("WARNING:") :
        #    warn_level = "medium"

    # Make dat file
    if os.path.isfile(fname) and os.path.getsize(fname) :
        cmd     = '''\\cp {} {}'''.format(fname, odat)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
    else:
        fff = open(odat, 'w')
        fff.write("")
        fff.close()
            
    # text above data
    otoptxt = "3dDeconvolve warnings"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ----------------------------------------------------------------------

# Text warning, goes to dir_img output
# ['pre_ss_warn_dset']
def apqc_warns_press( ap_ssdict, obase, qcb, qci ):
    """Make the text info which will be displayed for this warning, which
is about possible pre-steady state time points appearing in the data.
Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check for pre-steady state warnings'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get name of text file that might have warnings
    fname  = ap_ssdict['pre_ss_warn_dset']

    # parse text file for warning severity
    warn_level = "undecided"
    if fname : 
        txt = lah.read_dat(fname)
        if "possible pre-steady state TRs in run" in txt :
            warn_level = "severe"
        else:
            warn_level = "none"

    # Make dat file
    if os.path.isfile(fname) and os.path.getsize(fname) :
        cmd     = '''\\cp {} {}'''.format(fname, odat)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
    else:
        fff = open(odat, 'w')
        fff.write("")
        fff.close()

    # text above data
    otoptxt = "Pre-steady state warnings"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# ----------------------------------------------------------------------

# Text warning, goes to dir_img output
# ['tent_warn_dset']
def apqc_warns_TENT( ap_ssdict, obase, qcb, qci ):
    """Make the text info which will be displayed for this warning, which
is about possible TENT warnings from 1d_tool.py.  Also create text for
above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check for any TENT warnings from timing_tool.py'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get name of text file that might have warnings
    fname  = ap_ssdict['tent_warn_dset']

    # parse text file for warning severity
    warn_level = "undecided"
    if fname : 
        txt = lah.read_dat(fname)
        if not(txt.strip()) :
            # empty file means no warning
            warn_level = "none"
        else:
            warn_level = "medium"

    # Make dat file
    if os.path.isfile(fname) and os.path.getsize(fname) :
        cmd     = '''\\cp {} {}'''.format(fname, odat)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
    else:
        fff = open(odat, 'w')
        fff.write(" none ")
        fff.close()

    # text above data
    otoptxt = "TENT warnings from timing_tool.py"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    return 0

# -------------------------------------------------------------------
# -------------------------------------------------------------------

def aea_checkflip_json_warn_level( cf_dict ):

    EPS_scale = 10.**-4
    EPS_delta = 0.1        # percent diff change over scale of values

    # Do some simple calcs with cost values, refining guess, possible.
    # NB: these 'rules' might be highly in flux over time.
    fc_orig = float(cf_dict['flip_cost_orig'])
    fc_flpd = float(cf_dict['flip_cost_flipped'])

    numer = abs(fc_orig - fc_flpd)
    denom = 0.5 * (abs(fc_orig) + abs(fc_flpd))

    if cf_dict['flip_guess'] == "DO_FLIP" :
        warn_level = 'severe'
    else:
        warn_level = 'none'

    if denom < EPS_scale :
        # scale of both cost functions is waaay tiny
        warn_level = 'undecided'
    else:
        if numer/denom < EPS_delta :
            # difference is tiny: hard to tell
            warn_level = 'medium'

    return warn_level    

# -------------------------------------------------------------------

def apqc_warns_flip( ap_ssdict, obase, qcb, qci ):
    """Make the text info which will be displayed for this warning, which
is about LR-flip inconsistency between EPI and anatomical.  Also
create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'
    odat     = opref + '.dat'

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check LR-flip warnings'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get name of text file that might have warnings
    fname  = ap_ssdict['flip_check_dset']
    # ... and read it into a dict to use
    fdict  = read_in_txt_to_dict(fname)

    # parse text file for warning severity
    warn_level = "undecided"
    if fname :
        cf_dict    = read_in_txt_to_dict( fname )
        warn_level = aea_checkflip_json_warn_level( cf_dict )
    
    # [PT: May 26, 2020] emphasize the BETTER one
    if fdict['flip_guess'] == "NO_FLIP" :
        state_o = "BETTER match"
        state_f = "worse match"
    else:
        state_o = "worse match"
        state_f = "BETTER match"

    # Make data string for text output
    dattxt = " {:10s}  {:9s}  {:12s}  {:11s} \n".format("flip guess",
                                                        "cost func",
                                                        "original val",
                                                        "flipped val")
    dattxt+= " {:10s}  {:9s}  {:12s}  {:11s} \n".format("-"*10,
                                                        "-"*9,
                                                        "-"*12,
                                                        "-"*11)
    dattxt+= " {:>10s}  {:>9s} ".format(fdict['flip_guess'],
                                        fdict['flip_cost_func'])
    dattxt+= " {:12.5f}  {:11.5f} \n".format(fdict['flip_cost_orig'],
                                             fdict['flip_cost_flipped'])
    dattxt+= "\n"

    # Make descriptive warning text file 
    fff = open(odat, 'w')
    fff.write(dattxt)
    fff.close()

    # NB: focus box has to be the orig dset, not the AMASK* keyword,
    # because the automasking of the edgified dset might get rid of
    # *everything*
    focusbox = fdict['flip_base']

    # get ulay name 'orig'
    ulay   = fdict['flip_base']
    cmd    = '3dinfo -prefix ' + ulay
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    ulay_pref = com.so[0].strip()

    # get olay name 'orig'
    olay_o = fdict['flip_dset_orig']
    cmd    = '3dinfo -prefix ' + olay_o
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref_o  = com.so[0].strip()
    oname_o      = oname + '_0'
    opref_o      = opref + '_0'
    otopjson_o   = opref_o + '.axi.json'
    opbarrt_o    = opref_o + '.pbar'
    onvhtml_o    = opref_o + '.niivue.html'           # output niivue canvas
    odoafni_o    = 'run_' + oname_o + '.tcsh'         # AV script name
    qci_o        = qci + '_o'

    # get olay name 'flipped'
    olay_f = fdict['flip_dset_flipped']
    cmd    = '3dinfo -prefix ' + olay_f
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    olay_pref_f  = com.so[0].strip()
    oname_f      = oname + '_1'
    opref_f      = opref + '_1'
    otopjson_f   = opref_f + '.axi.json'
    opbarrt_f    = opref_f + '.pbar'
    onvhtml_f    = opref_f + '.niivue.html'           # output niivue canvas
    odoafni_f    = 'run_' + oname_f + '.tcsh'         # AV script name
    qci_f        = qci + '_f'

    # text above data
    otoptxt = "Left-right flip check warnings"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # Q: is it alright to leave out the sagittal image?  Should be,
    # and am doing so now!

    # always 'orig' dset first
    cmd = '''
    @djunct_edgy_align_check                                                 \
        -olay              {olay_o}                                          \
        -box_focus_slices  {focusbox}                                        \
        -ulay              {ulay}                                            \
        -use_olay_grid     wsinc5                                            \
        -ulay_range_am     "1%" "95%"                                        \
        -ulay_min_fac      0.2                                               \
        -blowup            1                                                 \
        -no_cor -no_sag                                                      \
        -prefix            {opref_o}
    '''.format(olay_o=olay_o, focusbox=focusbox, ulay=ulay,
               opref_o=opref_o)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Make QC images: *dry run only*, for AV/NV
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              "{ulay}"                                          \
        -box_focus_slices  "{focusbox}"                                      \
        -olay              "{olay}"                                          \
        -cbar              "{cbar}"                                          \
        -ulay_range        0% 120%                                           \
        -func_range_perc   98                                                \
        -pbar_posonly                                                        \
        -olay_alpha        No                                                \
        -olay_boxed        No                                                \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            1                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        "{odoafni}"                                       \
        -c2s_text          'APQC, {qcb}: {qci}  '                            \
        -c2s_text2     "++ Hover over image, hit 'o' to toggle olay on/off"  \
        -dry_run                                                             \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay_o,
                opbarrt=opbarrt_o, 
                cbar='gray_scale flip', opref=opref_o,
                odoafni=odoafni_o, qcb=qcb, qci=qci_o )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # ... followed by "flipped" one
    cmd = '''
    @djunct_edgy_align_check                                                 \
        -olay              {olay_f}                                          \
        -box_focus_slices  {focusbox}                                        \
        -ulay              {ulay}                                            \
        -use_olay_grid     wsinc5                                            \
        -ulay_range_am     "1%" "95%"                                        \
        -ulay_min_fac      0.2                                               \
        -blowup            1                                                 \
        -no_cor -no_sag                                                      \
        -prefix            {opref_f}
    '''.format(olay_f=olay_f, focusbox=focusbox, ulay=ulay,
               opref_f=opref_f)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Make QC images: *dry run only*, for AV/NV
    cmd = '''
    @chauffeur_afni                                                          \
        -ulay              "{ulay}"                                          \
        -box_focus_slices  "{focusbox}"                                      \
        -olay              "{olay}"                                          \
        -cbar              "{cbar}"                                          \
        -ulay_range        0% 120%                                           \
        -func_range_perc   98                                                \
        -pbar_posonly                                                        \
        -olay_alpha        No                                                \
        -olay_boxed        No                                                \
        -set_subbricks     0 0 0                                             \
        -opacity           9                                                 \
        -pbar_saveim       "{opbarrt}.jpg"                                   \
        -prefix            "{opref}"                                         \
        -save_ftype        JPEG                                              \
        -blowup            1                                                 \
        -montx             7                                                 \
        -monty             1                                                 \
        -montgap           1                                                 \
        -montcolor         black                                             \
        -set_xhairs        OFF                                               \
        -label_mode        1                                                 \
        -label_size        4                                                 \
        -no_cor                                                              \
        -cmd2script        "{odoafni}"                                       \
        -c2s_text          'APQC, {qcb}: {qci}'                              \
        -c2s_text2     "++ Hover over image, hit 'o' to toggle olay on/off"  \
        -dry_run                                                             \
        -do_clean
    '''.format( ulay=ulay, focusbox=focusbox, olay=olay_f,
                opbarrt=opbarrt_f, 
                cbar='gray_scale flip', opref=opref_f,
                odoafni=odoafni_f, qcb=qcb, qci=qci_f )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()


    # text above+below 'orig' image
    otoptxt_o = "... original anat edges ({})".format(state_o)
    osubtxt_o = "olay: {} (over EPI, {})".format(olay_pref_o, ulay_pref)

    # store name of NiiVue html
    onvhtml_name_o = onvhtml_o.split('/')[-1]

    # Make info above+below 'orig' image
    otopdict_o = {
        'itemtype'    : 'VOL',
        'itemid'      : qci_o,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt_o,
        'subtext'     : osubtxt_o,
        'av_file'     : odoafni_o,
        'nv_html'     : onvhtml_name_o,
   }
    with open(otopjson_o, 'w', encoding='utf-8') as fff:
        json.dump( otopdict_o, fff, ensure_ascii=False, indent=4 )


    # text above+below 'flipped' image
    otoptxt_f = "... flipped anat edges ({})".format(state_f)
    osubtxt_f = "olay: {} (over EPI, {})".format(olay_pref_f, ulay_pref)

    # store name of NiiVue html
    onvhtml_name_f = onvhtml_f.split('/')[-1]

    # Make info above+below 'flipped' image
    otopdict_f = {
        'itemtype'    : 'VOL',
        'itemid'      : qci_f,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt_f,
        'subtext'     : osubtxt_f,
        'av_file'     : odoafni_f,
        'nv_html'     : onvhtml_name_f,
    }
    with open(otopjson_f, 'w', encoding='utf-8') as fff:
        json.dump( otopdict_f, fff, ensure_ascii=False, indent=4 )


    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt_o )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt_o)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay_o, itemid=qci_o,
                                     verb=0 )
    fff = open(onvhtml_o, 'w')
    fff.write(nv_txt)
    fff.close()


    # Make pbar text
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -delimiter_major  '::'                                               \
        -delimiter_minor  ',,'                                               \
        -input            "{opbarrt}.txt"                                    \
        -prefix           "{opbarrt}.json"
    '''.format( opbarrt=opbarrt_f )
    com    = ab.shell_com(cmd, capture=do_cap)
    com.run()

    # For AV/NV: get pbar/cmap info as dict (so must be done after
    # pbar text is made)
    pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt_f)
    with open(pbar_json, 'r') as fff:
        pbar_dict = json.load(fff)

    # Make NiiVue canvas text
    nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                     ulay, pbar_dict, 
                                     olay_name=olay_f, itemid=qci_f,
                                     verb=0 )
    fff = open(onvhtml_f, 'w')
    fff.write(nv_txt)
    fff.close()

    return 0

# -------------------------------------------------------------------

def vlines_read_in_coords(fname):
    '''Read in a file 'fname' of coords, and return a list of strings (one
    per row).  Also return the number of columns.  The fname file
    might be empty.

    Parameters
    ----------
    fname  : str
             filename to read in

    Return
    ------
    list_rows : list (of str)
             list form of fname, each row being 1 string that has no 
             whitespace at the right.
    '''

    fff = open(fname, 'r')
    X = fff.readlines()
    fff.close()

    list_rows = []
    for x in X:
        y = x.rstrip()
        if y :
            list_rows.append(y)

    return list_rows

def vlines_combine_coord_lists(list_coordlist, list_title, maxnblock = 3):
    '''Take a list of lists (of coords) and create a block of text to
visualize in APQC by stacking the lists adjacently.
    
    Parameters
    ----------
    list_coordlist : list (of lists (of str))
            1 or more lists of coordinates;  each sublist is a list
            of strings (one string is a row of coords)
    list_title : list (of str)
            list of titles to put at the top of each column
    maxnblock : int
            max number text blocks to output, by default.

    Return
    ------
    otext : str
            text block to be used in APQC report. Consecutive lists
            of text

    '''

    nlist = len(list_coordlist)

    if nlist == 0 :                  return ''
    if nlist != len(list_title) :    return ''

    # only use as many as we can and/or are allowed
    ntot  = nlist
    extra = ''
    if maxnblock < ntot :
        ntot  = maxnblock
        extra = '  ...'      # hint that more files than are shown exist

    # store nrows and max row width for each list
    l_nrow   = []
    l_maxwid = []
    for n in range(ntot):
        coordlist = list_coordlist[n]
        l_nrow.append(len(coordlist))
        maxwid = -1
        for y in coordlist :
            if len(y) > maxwid :
                maxwid = len(y)
        l_maxwid.append(maxwid)
    max_nrow = max(l_nrow)

    # commence the text string
    otext = ''

    # the title line
    for n in range(ntot) :
        otext+= '{title:^{wid}s}'.format(title=list_title[n], 
                                         wid=l_maxwid[n])
        if n < ntot-1 :
            otext+= ' '*2
        else:
            otext+= extra + '\n'

    # the title underlining
    for n in range(ntot) :
        otext+= '-'*l_maxwid[n]
        if n < ntot-1 :
            otext+= ' '*2
        else:
            otext+= '\n'
            
    # each row of info
    for i in range(max_nrow):
        for n in range(ntot) :
            # print a row *if* it exists
            if i < l_nrow[n]: 
                otext+= '{row:<{wid}s}'.format(row=list_coordlist[n][i], 
                                               wid=l_maxwid[n])
            else:
                otext+= ' '*l_maxwid[n]
            if n < ntot-1 :
                otext+= ' '*2
            else:
                otext+= '\n'

    return otext

def vlines_parse_QC_txt(fname):
    '''The text file created by find_variance_lines.tcsh has useful
    information for QC pieces. This function opens, reads and parses
    the file, returning both the full text and the list/name of the
    [0]th line found (i.e., what is the [0]th image shown); returns
    null strings if there were no image found

    Parameters
    ----------
    fname : str
            name of text file

    Return
    ------
    str_full : str
            the full contents of the file, as a single string (it is only
            one line, at present)
    str_name : str
            name of the [0]th entry (e.g., 'inter' or 'r01', 'r02', ...)

    '''

    # default return, if error exiting
    BAD_RETURN = ""

    if not(os.path.isfile( fname )) :    BAD_RETURN
    
    fff = open(fname, 'r')
    X = fff.readlines()
    fff.close()
    
    # parse first entry
    str_full = X[0].strip()
    if not(str_full) :    BAD_RETURN

    str_name = str_full.split(":")[0]

    return str_full, str_name

# -----------------

# check for lines in EPI: ['vlines_tcat_dir']
def apqc_warns_vlines( ap_ssdict, obase, qcb, qci ):
    """Make the text info and images which will be displayed for this
warning, which is about (possibly) having artifactual variance lines
in the EPI.  Also create text for above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block

Returns
----------
num : int
    return 0 up on success, or a different int if failure

    """

    # output names/prefixes/etc.
    oname    = '_'.join([obase, qcb, qci])           # output name
    opref    = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    otopjson = opref + '.json'             
    odat     = opref + '.dat'
    oimg     = opref + '_0' + '.sag.jpg'   
    osubjson = opref + '_0' + '.sag.json'  

    if 1 :
        print("++ APQC create:", oname, flush=True)

    do_cap = True
    cmd    = '''# check variance lines (var_line) warnings'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get the name of the directory that might have warnings, as well
    # as prefixes of files that might exist, containing bad data.
    dirname  = ap_ssdict['vlines_tcat_dir'] 
    file_img = dirname + '/' + fname_vlines_img
    file_txt = dirname + '/' + fname_vlines_txt

    # default: no variance lines, which makes life simple :)
    warn_level = "none"
    list_bad   = []
    otext      = 'No lines found\n'

    # use the text file info, if it exists
    text_loc = ''
    str_full, str_name = vlines_parse_QC_txt( file_txt )
    if str_full :
        text_loc = 'olay: line markers for ' + str_full

    # alternate: some variance lines, and now we have to do work :(
    if os.path.isfile( file_img ) : 
        warn_level = "medium"   ## or should be severe?

        otext = ''              # start afresh

        # get the list of files, and count
        list_bad = glob.glob(dirname + '/' + 'bad_coords.r*.txt')
        list_bad.sort()
        nbad = len(list_bad)
        # ... and check for an intersection file (should only be 1)
        list_inter = glob.glob(dirname + '/' + 'bad_coords.inter.txt')

        # get coordinate list, and store title+list of all non-empty
        # files.  Remember: [0]th list is intersection, and [1:] lists
        # are per-run
        list_coordlist = []
        list_title     = []
        list_nlines    = []
        sum_nrow       = 0

        # info from intersection file
        if list_inter and nbad>1 :
            title = 'Intersecting all'

            coordlist = vlines_read_in_coords(list_inter[0])
            nrow      = len(coordlist)
            if nrow :
                list_coordlist.append(coordlist)
                list_title.append(title)
                sum_nrow+= nrow
            text_inter = 'Intersecting  : {}\n'.format(nrow)

        # info from bad_coords.r* files
        for i in range(nbad):
            title = list_bad[i].split('/')[-1]
            title = title.split('.')[1]

            coordlist = vlines_read_in_coords(list_bad[i])
            nrow      = len(coordlist)
            list_nlines.append(nrow)  # keep track of ALL of them
            if nrow :
                list_coordlist.append(coordlist)
                list_title.append(title)
                sum_nrow+= nrow
        all_num = [str(x) for x in list_nlines]
        otext+= 'Lines per run : {}\n'.format(' '.join(all_num))

        if list_inter and nbad>1 :
            otext+= text_inter 
        otext+= '\n'

        ttt_extra = 'of each'
        if sum_nrow > 7 :
            ttt_extra = 'of the first 7'

        # main table text
        ttt   = 'Coordinates (see images {}, below, '.format(ttt_extra)
        ttt  += 'check locations with InstaCorr)\n'
        otext+= ttt + '-'*(len(ttt)-1) + '\n'
        otext+= vlines_combine_coord_lists(list_coordlist, list_title)
        otext+= '\n'

        # Make (=copy) output image
        cmd    = '''\\cp {} {}'''.format(file_img, oimg)
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()


    # Make descriptive warning text file 
    fff = open(odat, 'w')
    fff.write(otext)
    fff.close()
    
    # text above data
    otoptxt = "EPI variance lines warnings"

    # Make info below images 
    otopdict = {
        'itemtype'    : 'WARN',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'text'        : otoptxt,
        'warn_level'  : warn_level
    }
    with open(otopjson, 'w', encoding='utf-8') as fff:
        json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

    # Make text below images, if badness is found
    descrip = "scaled variance per run"
    osubtxt = "ulay: {}/var*scale*.nii.gz ({})".format(dirname, descrip)
    if text_loc :
        osubtxt = [osubtxt, text_loc]

    # Make info below images
    osubdict = {
        'itemtype'    : 'VOL',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_blocks[qcb][0],
        'title'       : lah.qc_blocks[qcb][1],
        'subtext'     : osubtxt,
    }
    with open(osubjson, 'w', encoding='utf-8') as fff:
        json.dump( osubdict, fff, ensure_ascii=False, indent=4 )

    return 0

# -------------------------------------------------------------------

# look for radcor*/ dirs, and the goodies therein
def apqc_radcor_rcvol( ap_ssdict, obase, qcb, qci,
                       rcdir, ith_run=0 ):  
    """Make images of radcor (radial correlation), which can be done
during a couple different blocks during processing (info contained in
rcdir and counted by ith_run) and contain one image per run for each
block.  We only use axial images here. Also create text for
above/below images.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
obase : str
    start of output filenames, likely qc_<zeropadded idx>
qcb : str
    QC block ID
qci : str
    item ID of this information within the QC block
rcdir : str
    name of the radcor dir for this particular block of processing,
    which can contain multiple runs inside it (looped over within
    the code)
ith_run : int
    a number counting/indexing the AP blocks that have radcor calcs

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    if 1 :
        print("++ APQC create:", '_'.join([obase, qcb, qci]), flush=True)

    do_cap = True
    cmd    = '# @radial_correlate images for: ' + rcdir
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get ulay and olay names
    if rcdir.endswith('regress') :
        # should only be a single errts file
        all_ulay = [ap_ssdict['main_dset']]     # bc errts makes a poor ulay
        all_olay = sorted(glob.glob(rcdir + "/" + "radcor.*.errts.corr*.HEAD"))
    else:
        all_ulay = sorted(glob.glob(rcdir + "/" + "epi.ulay*HEAD"))
        all_olay = sorted(glob.glob(rcdir + "/" + "radcor.*.corr*HEAD"))

    Nulay = len( all_ulay )
    Nolay = len( all_olay )
    Ncbar = Nolay - 1                # the idx of volume to get cbar under

    pb       = rcdir.split(".")[1]
    rc_block = rcdir.split(".")[2]

    if Nulay != Nolay :
        sys.exit("** ERROR: radcor number of ulays ({}) "
                 "doesn't match number of olays ({})".format(Nulay, Nolay))

    # start a dictionary of main parameters for making images
    chauff_params             = {}
    chauff_params['thr_val']  = 0.4
    chauff_params['olay_top'] = 0.7
    chauff_params['cbar']     = "Reds_and_Blues_Inv"
    if rcdir.endswith('regress') :
        chauff_params['pbar_cr']  = "ulay is " + ap_ssdict['main_dset']
    else:
        chauff_params['pbar_cr']  = "ulay is 0th vol of EPI"
    chauff_params['pbar_tr']  = "alpha on"

    for ii in range(Nolay):
        chauff_params['ulay'] = all_ulay[ii]
        chauff_params['olay'] = all_olay[ii]
        aaa  = chauff_params['olay'].split("/")
        rnum = aaa[1].split(".")[2]   # get run number

        # output name. NB: all other prefixes done below in loop here.
        # NB: *This* radcor oname formulation differs from other
        # functions, bc of the looping
        qci_num  = qci + "_" + rnum                    # special qci, use here
        oname    = '_'.join([obase, qcb, qci_num])     # output name for [ii]th
        opref    = ap_ssdict['odir_img'] + '/' + oname # prefix = path + name
        otopjson = opref + '.axi.json'
        #osubjson = opref + '.sag.json'
        opbarrt  = opref + '.pbar'
        onvhtml  = opref + '.niivue.html'                # output niivue canvas
        odoafni  = 'run_' + oname + '.tcsh'              # AV script name

        if 1 :
            print(' '*16 + 'run: {}/{}'.format(rnum, Nolay))

        # get ulay prefix
        ulay   = chauff_params['ulay']
        cmd    = '3dinfo -prefix ' + ulay
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
        ulay_pref = com.so[0].strip()

        # get olay prefix
        olay   = chauff_params['olay']
        cmd    = '3dinfo -prefix ' + olay
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()
        olay_pref = com.so[0].strip()

        cmd = '''
        @chauffeur_afni                                                      \
            -ulay              {ulay}                                        \
            -ulay_range        0% 110%                                       \
            -olay              {olay}                                        \
            -box_focus_slices  AMASK_FOCUS_OLAY                              \
            -cbar              {cbar}                                        \
            -func_range        {olay_top}                                    \
            -thr_olay          {thr_val}                                     \
            -olay_alpha        Yes                                           \
            -olay_boxed        No                                            \
            -set_subbricks     0 0 0                                         \
            -opacity           9                                             \
            -pbar_saveim       "{opbarrt}.jpg"                               \
            -pbar_comm_range   "{pbar_cr}"                                   \
            -pbar_comm_thr     "{pbar_tr}"                                   \
            -prefix            "{opref}"                                     \
            -save_ftype        JPEG                                          \
            -blowup            2                                             \
            -montx             7                                             \
            -monty             1                                             \
            -montgap           1                                             \
            -montcolor         black                                         \
            -set_xhairs        OFF                                           \
            -label_mode        1                                             \
            -label_size        4                                             \
            -no_cor -no_sag                                                  \
            -cmd2script        {odoafni}                                     \
            -c2s_text          'APQC, {qcb}: {qci}'                          \
            -do_clean
        '''.format( **chauff_params, opbarrt=opbarrt, opref=opref,
                    odoafni=odoafni, qcb=qcb, qci=qci_num )
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

        # Make text for above and below images (latter differs between
        # first and subsequent images)
        otoptxt = ''
        ttt2    = ''
        osubtxt = "olay: " + olay_pref
        if not(ii) :
            otoptxt = '@radial_correlate check for data dir: ' + rcdir
        if not(ith_run) and not(ii) :
            ttt2 = '   {}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)
        if ttt2 :
            otoptxt = [otoptxt, ttt2]

        # store name of NiiVue html
        onvhtml_name = onvhtml.split('/')[-1]

        # conditions for top text
        olay_title = 'olay: ' + olay_pref
        if not(ii) :
            otoptxt = []
            ttt  = '@radial_correlate check for data dir: ' + rcdir
            lent = len(ttt)
            otoptxt.append(ttt)
            otoptxt.append("{:^{}s}".format(olay_title, lent)) # center info
        else:
            otoptxt = olay_title

        # some IC driving scripts
        if rcdir.endswith('regress') :
            ic_file = 'run_instacorr_errts.tcsh'
            ic_args = ''
            gv_file = 'run_graphview_errts.tcsh'
            gv_args = ''
        else:
            ic_file = 'run_instacorr_pbrun.tcsh'
            ic_args = '{} {}'.format(pb, rnum)
            gv_file = 'run_graphview_pbrun.tcsh'
            gv_args = '{} {}'.format(pb, rnum)

        # conditions for placing text and cbar
        if ii == Ncbar :
            # Make a cbar and text below image, *only* for last one in set

            # text below images
            osubtxt = '{}:{}.pbar.json'.format(lah.PBAR_FLAG, oname)

            # Make info above+below images
            otopdict = {
                'itemtype'    : 'VOL',
                'itemid'      : qci_num,
                'blockid'     : qcb,
                'blockid_hov' : lah.qc_blocks[qcb][0],
                'title'       : lah.qc_blocks[qcb][1],
                'text'        : otoptxt,
                'nv_html'     : onvhtml_name,
                'av_file'     : odoafni,
                'ic_file'     : ic_file,
                'ic_args'     : ic_args,
                'gv_file'     : gv_file,
                'gv_args'     : gv_args,
                'subtext'     : osubtxt,
            }
            with open(otopjson, 'w', encoding='utf-8') as fff:
                json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

        else:
            # Make text above image only

            # Make info below images
            otopdict = {
                'itemtype'    : 'VOL',
                'itemid'      : qci_num,
                'blockid'     : qcb,
                'blockid_hov' : lah.qc_blocks[qcb][0],
                'title'       : lah.qc_blocks[qcb][1],
                'nv_html'     : onvhtml_name,
                'av_file'     : odoafni,
                'ic_file'     : ic_file,
                'ic_args'     : ic_args,
                'gv_file'     : gv_file,
                'gv_args'     : gv_args,
                'text'        : otoptxt,
            }
            with open(otopjson, 'w', encoding='utf-8') as fff:
                json.dump( otopdict, fff, ensure_ascii=False, indent=4 )

        # Make pbar text (always done now, for AV/NV buttons)
        cmd = '''
        abids_json_tool.py                                               \
            -overwrite                                                   \
            -txt2json                                                    \
            -delimiter_major  '::'                                       \
            -delimiter_minor  ',,'                                       \
            -input            "{opbarrt}.txt"                            \
            -prefix           "{opbarrt}.json"
        '''.format( opbarrt=opbarrt )
        com    = ab.shell_com(cmd, capture=do_cap)
        com.run()

        # For AV/NV: get pbar/cmap info as dict (so must be done after
        # pbar text is made)
        pbar_json = '{opbarrt}.json'.format(opbarrt=opbarrt)
        with open(pbar_json, 'r') as fff:
            pbar_dict = json.load(fff)

        # Make NiiVue canvas text
        nv_txt = lanv.make_niivue_2dset( ap_ssdict['odir_qc'],
                                         ulay, pbar_dict, 
                                         olay_name=olay, itemid=qci_num,
                                         verb=0 )
        fff = open(onvhtml, 'w')
        fff.write(nv_txt)
        fff.close()
        onvhtml_name = onvhtml.split('/')[-1]
        

    return 0

# ========================== cp jsons ==============================

# cp json(s) to QC_*/ dir
# 1
def apqc_DO_cp_subj_jsons( ap_ssdict, all_json ):
    """Copy over JSON files to an appropriate spot in the QC directory
(the dir_info subdir).

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
all_json : list
    list of names of JSON files to copy to the APQC's dir_info

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    if 1 :
        print("++ APQC create: copy jsons to info dir", flush=True)

    do_cap = True
    cmd    = '''# copy subj json(s)'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    for json in all_json :
        # Make (=copy) output image
        cmd    = '''\\cp {} {}/.'''.format(json, ap_ssdict['odir_info'])
        com    = ab.shell_com(cmd, capture=do_cap)
        stat   = com.run()

    return 0

# ========================== cp rev basic txt ===========================

# cp rev basic text to QC_*/ dir
# ['ss_review_dset']
def apqc_DO_cp_subj_rev_basic( ap_ssdict ):
    """Copy the ss_review_basic information into the appropriate spot in
the QC_*/ directory.  Also create an editable JSON version.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    if 1 :
        print("++ APQC create: copy ss_review_basic file", flush=True)

    do_cap = True
    cmd    = '''# copy review basic text file to QC dir'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # get base name of ss_rev file (remove ext)
    obase = '.'.join(ap_ssdict['ss_review_dset'].split('.')[:-1])
    ojson = ap_ssdict['odir_info'] + '/' + obase + '.json'

    # Make (= copy) ss_rev in QC 
    cmd    = '''\\cp {} {}/.'''.format(ap_ssdict['ss_review_dset'],
                                       ap_ssdict['odir_info'])
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    # Make the JSON version in same dir
    cmd = '''
    abids_json_tool.py                                                       \
        -overwrite                                                           \
        -txt2json                                                            \
        -literal_keys                                                        \
        -values_stay_str                                                     \
        -input            {ss_review_dset}                                   \
        -prefix           {ojson}
    '''.format(**ap_ssdict, ojson=ojson)
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    return 0

# ========================== term echo ==============================

# @ss_review_basic dumped to terminal
# 1
def apqc_DO_term_ss_review_basic( ap_ssdict ):
    """Display the ss_review_basic information in the terminal.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    if 1 :
        print("++ APQC create: display ss_review_basic info", flush=True)

    do_cap = False
    cmd    = '''# disp basic information from processing'''
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()

    ptext = "# ++++++++++++++++ Check output of @ss_review_basic ++++++++++++++++ #"
    print("\n" + ptext)
    print('-'*len(ptext))
    cmd    = '''\\cat {} '''.format(ap_ssdict['ss_review_dset'])
    com    = ab.shell_com(cmd, capture=do_cap)
    stat   = com.run()
    print('-'*len(ptext))

    return 0


# ======================== html page title ============================

def make_apqc_Top_pagetop( ap_ssdict, oname, qcb, qci, task_name = '' ):
    """Write out info (JSON file) for the HTML page top---mainly the subj
ID info.

Parameters
----------
ap_ssdict : dict
    dictionary of subject uvars
oname: str
    name (without path) of the output JSON file
qcb: str
    QC block ID
qci: str
    item ID of this information within the QC block
task_name : str
    (some day to be added) string label for the page, like a group or task
    name

Returns
----------
num : int
    return 0 up on success, or a different int if failure

"""

    do_cap = True
    com    = ab.shell_com('# write HTML pagetop: subj ID', capture=do_cap)
    stat   = com.run()

    opref  = ap_ssdict['odir_img'] + '/' + oname   # prefix = path + name
    ojson  = opref + '.json'

    # NB: most blockid_hov are fully formed; this one needs subj ID added
    odict = {
        'itemtype'    : 'TITLE',
        'itemid'      : qci,
        'blockid'     : qcb,
        'blockid_hov' : lah.qc_title[qcb][0] + ap_ssdict['subj'],
        'title'       : lah.qc_title[qcb][1],
        'subj'        : ap_ssdict['subj'],
    }

    # write JSON
    with open(ojson, 'w', encoding='utf-8') as fff:
        json.dump( odict, fff, ensure_ascii=False, indent=4 )

    return 0

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# =========================================================================
# =========================================================================

if __name__ == "__main__":
    # Whee.
    print('Done.')
