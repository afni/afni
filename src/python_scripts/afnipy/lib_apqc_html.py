#!/usr/bin/env python

# ver : 1.2 || date: Oct 17, 2018 || auth: PA Taylor
# + separate title and text strings
# + new warn type
#
# ver : 1.3 || date: Nov 1, 2018 
# + [PT] wrap_imag now includes href, so clicking on image opens it in
#   new link
#
# ver : 1.4 || date: Nov 1, 2018 
# + [PT] Making py3 compatible: 
#        from 2to3, updating DICT.has_key(x) -> x in DICT
#
# ver : 1.5 || date: Feb 21, 2019
# + [PT] adding comment to empty button sets it to "?", not "X"
#
#ver = '2.21' ; date = 'May 17, 2019' 
# + [PT] simplifying radcor behavior
#
#ver = '2.3' ; date = 'July 3, 2019' 
# + [PT] Colorbars standard widths
# + [PT] QC block ID now in QC block titles
# + [PT] added more help descriptions
#
#ver = '2.4' ; date = 'March 27, 2020' 
# [PT] remove dependency on lib_apqc_html_helps.py
#    + absorb those lahh.* functions and variables here
#
#ver = '2.5' ; date = 'Feb 23, 2021' 
# [PT] update helps, reorder
#
#ver = '2.6' ; date = 'Jan 18, 2022' 
# [PT] several changes
# + add mecho QC block help
# + tweak some text
# + add embedded URLs, where appropriate
#
#ver = '2.61' ; date = 'Mar 10, 2022' 
# [PT] bug fix in m_tedana button creation
# + fix oversight in wrap_button(), where different buttons pointed to
#   only one tedana directory.  Thanks, Dan H for noting this!
#
#ver = '3.0' ; date = 'June 18, 2022' 
# [Taylor Hanayik] add in updates for QC button functionality
# + specifically, to accommodate local flask server (open_apqc.py)
#
#ver = '3.01' ; date = 'June 28, 2022' 
# [PT] have the A+, A-, A? and clr buttons *also* update apqc_*.json
#
#ver = '3.02' ; date = 'Aug 2, 2022' 
# [PT] 'SAVE' button to 'SAVING', now reflects whether server is active
# + also updated the help file a lot
#
#ver = '3.03' ; date = 'Aug 2, 2022' 
# [PT] forgot some pieces in last update; adding now
# + also rename 'srvr' to 'saving'
#
#ver = '3.04' ; date = 'Aug 2, 2022' 
# [PT] the QC rating information now gets saved whenever the text dialogue
#      box gets closed.
#
#ver = '3.05' ; date = 'Aug 4, 2022' 
# [PT] add link to AFNI MB in APQC help file
#
#ver = '3.06' ; date = 'Sep 2, 2022' 
# [TH, PT] update what is posted, so the saving happens better across
# multiple open APQC pages
#
#ver = '3.07' ; date = 'Sep 2, 2022' 
# [PT] update allYourBaseAreBelongToUs() to save with single click when
#      serving
#
#ver = '3.08' ; date = 'Sep 4, 2022' 
# [PT] create+use subj ID info now
#
ver = '3.09' ; date = 'Sep 6, 2022' 
# [PT] make URL more flexible by reading in origin---don't assume it is 
# just 5000
#
#########################################################################


# mostly ways to read & wrap text & images.
import sys
import os
import json
import collections as coll

from afnipy import lib_apqc_html_css   as lahc

NULL_BTN1     = '' # empty space, used to keep button populated
NULL_BTN0     = '|' # empty space, used to keep button populated

json_cols  = [ 'qcele', 'rating', 'comment' ]
PBAR_FLAG  = 'SHOW_PBAR'
dir_img    = 'media'

## ==========================================================================
## ==========================================================================
## ==========================================================================
## ==========================================================================
## ==========================================================================

# --------- Section on what used to be library of "apqc html helps" ---------

# For single subj QC, we now identify major groupings/categories to
# look at: QC blocks.  Those will determine general organization
# (sections, section titles, etc.) and guide usage.
# 
# Format of each entry:
# qc_block[ ABBREV ] = [ "SECTION HOVER TEXT", "SECTION TITLE" ]
#
# For organizational purposes, these quantity abbrevs are/should be
# used in the names of functions used to generate each section. (Makes
# it easier to see what is connected to what, even if they change.)

qc_title           = coll.OrderedDict()
qc_title["HOME"]   = [ "Top of page for subj:&#10  ", # and append subj here!
                       "afni_proc.py single subject report" ]

qc_blocks          = coll.OrderedDict()
qc_blocks["vorig"]  = [ "vols in orig space", 
                        "Check vols in original space" ]

qc_blocks["ve2a" ]  = [ "vol alignment (EPI-anat)", 
                        "Check vol alignment (EPI to anat)" ]

qc_blocks["va2t" ]  = [ "vol alignment (anat-template)", 
                        "Check vol alignment (anat to template)" ]

qc_blocks["vstat"]  = [ "statistics vols", 
                        "Check statistics vols (and effect estimates)" ]

qc_blocks["mot"  ]  = [ "motion and outliers", 
                        "Check motion and outliers" ] 

qc_blocks["mecho" ]  = [ "multi-echo", 
                        "Check multi-echo data and processing" ]

qc_blocks["regr" ]  = [ "regressors", 
                        "Check regressors, DFs and residuals" ]

qc_blocks["radcor"] = [ "@radial_correlate vols", 
                        "Check extent of local correlation" ]

qc_blocks["warns"]  = [ "all warnings from processing", 
                        "Check all warnings from processing" ]

qc_blocks["qsumm"]  = [ "summary quantities from @ss_review_basic", 
                        "Check summary quantities from @ss_review_basic" ]

qc_link_final       = [ "FINAL", 
                       "overall subject rating" ] 

# ------------------------------

# a brief help string for online help
qcb_helps           = coll.OrderedDict()
qcb_helps["vorig"]  = '''
Volumetric mages of data (EPI and anat) in original/native space.
'''

qcb_helps["ve2a" ]  = '''
Volumetric images of the alignment of the subject's anat
(underlay/grayscale) and EPI (overlay/hot color edges) volumes. Likely
these will be shown in the template space, if using the tlrc block.
'''

qcb_helps["va2t" ]  = '''
Volumetric images of the alignment of the standard space template
(underlay/grayscale) and subject's anat (overlay/hot color edges)
volumes.
'''

qcb_helps["vstat"]  = '''
Volumetric images of statistics results (and, where available, effect
estimates).  

For task-based datasets (where stimulus timing was used in AP), the
(full) F-stat of an overall regression model is shown.  Additionally,
one can specify labels of stimuli or GLTs used in the afni_proc.py
command, and statistical results will be shown.  For stimuli with
effect estimates, the 'Coef' vales will be displayed as the olay
colors (preferably with the 'scale' block having been used in
afni_proc.py, producing meaningful units of BOLD % signal change in
the 'Coef' volumes).

For resting-state and naturalistic scans, seedbased correlation maps are
displayed (when the final space is recognized).

Colorbar ranges and thresholds are chosen from either percentile
values within the data set (preferably from within a WB mask,
available when the 'mask' block was used in afni_proc.py) or from
pre-set statistical levels (like p=0.001).  Each is case is described.

'''

qcb_helps["mot"  ]  = '''
Summary of motion and outlier information, which may each/both be
used as censoring criteria.

The 6 rigid body motion parameters (3 rotation + 3 translation) are
combined into a single quantity: the Euclidean norm (enorm), which has
approx. units of 'mm'.  Large changes in the enorm time series show
moments of subject motion.

Separate runs are shown with the background alternating between white
and light gray.

Boxplots summarize parameter values, both before censoring (BC) and
after censoring (AC).

And a grayplot of residuals (with motion/outliers/censoring) is
provided.  The '-pvorder' is used for output, placing the time series
in decreasing order of similarity to the top two principal components
of the (masked) time series data.  The colorbar max is set to 3.29,
the value at which a standard normal distribution N(0,1) has a
two-sided tail probability of 0.001.  The grayplot's top row contains
a plot of the motion enorm and outlier frac across time, for reference
with the grayplot series.
'''

qcb_helps["mecho"]  = '''
There are many ways to process multi-echo (ME) EPI data.  Fortunately,
afni_proc.py provides the ability to include most of them in your FMRI
processing.  Please see the afni_proc.py help for the full argument
list of '-combine_method ..'.

The OC/OC_A ('optimally combined') methods were proposed by Posse et
al. (1999).

When any of the 'tedana*' or 'OC_tedort' methods is chosen, then
processing uses outputs from the Kundu et al. (2011) work.

When any of the 'm_tedana*' methods is chosen, then processing uses
outputs from the MEICA group's tedana tool.  For more details, see the
<urlin><a href="https://tedana.readthedocs.io/en/stable/" target="_blank">TEDANA project webpage</a></urlin>.
'''

qcb_helps["regr" ]  = '''
When processing with stimulus time series, both individual and
combined stimulus plots are generated (with any censoring also shown).

The degrees of freedom (DF) summary is also provided, so one can check
if too many get used up during processing (careful with bandpassing!).

The "corr_brain" plot shows correlation of each voxel with the errts
average within the whole brain mask (what could be called the 'global
signal').

Two TSNR dsets can be shown.  In each case, voxelwise TSNR is shown
throughout the full FOV, and any brain mask dset is just used for
defining a region within which percentiles are calculated. The generic 
formula for TSNR is:
            TSNR = average(signal) / stdev(noise)
+ First, the TSNR of r01 after volreg is shown if the user used the
  '-volreg_compute_tsnr yes' opt in AP. Here, the "signal" is the time
  series and the "noise" is the detrended time series.
+ Second, the TSNR of the combined runs after regression modeling is
  shown. Here, the "signal" is the all_runs dset and the "noise" is
  the errts time series.

When a mask is present, the olay's hot colors (yellow-orange-red) are
defined by the 5-95%ile range of TSNR in the mask.  The 1-5%ile values
within the mask are shown in light blue, and the lower values are
shown in dark blue.  In the absence of a mask, then the colorbar goes
from 0 to the 98%ile value within the whole dset.
'''

qcb_helps["radcor"] = '''
@radial_correlate plots (per run, per block). These can show
scanner coil artifacts, as well as large subject motion; both factors
can lead to large areas of very high correlation, which would be
highlighted here.  
'''

qcb_helps["warns"]  = '''
Several AFNI programs carry out consistency checks while
processing (e.g., pre-steady state check, regression matrix corr
warnings, left-right flip checks).  Warnings are conglomerated here.

Each warning has one of the following levels:
    {}

The warning level is written, with color coding, at the top of each
warning's text box.  The QC block label 'warns' at the top of the page
is also colored according to the maximum warning level present.  
'''.format( lahc.wlevel_str )

qcb_helps["qsumm"]  = '''
This is the output of @ss_review_basic, which contains a loooot of
useful information about your single subject processing.
'''

qcbh = ''
for x in qcb_helps.keys():
    y    = qcb_helps[x]
    yspl = y.split('\n')
    yind = '\n    '.join(yspl)
    qcbh+= '\n' + x 
    qcbh+= yind


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

apqc_help = [ 
['HELP FILE FOR APQC', 
 '''*** APQC HTML: afni_proc.py's QC report for single subject analysis ***

Some useful links:
+ The APQC HTML's <urlin><a href="https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/tutorials/apqc_html/main_toc.html" target="_blank">online tutorial</a></urlin>
+ The afni_proc.py (AP) program's <urlin><a href="https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/afni_proc.py_sphx.html" target="_blank">online help file</a></urlin>
+ The AFNI Message Board <urlin><a href="https://discuss.afni.nimh.nih.gov" target="_blank">homepage</a></urlin>
'''], 
['OVERVIEW', 
'''QC organization
    The quality control (QC) is organized into thematic blocks to
    check, such as original data acquisition, different alignments,
    motion, regression modeling, and more. At the top of the QC page,
    there is a navigation bar with a label for each QC block (vorig,
    ve2a, etc.) that functions as a button to the top of that section.

Rating
    Beneath each section label is a(n initially empty) QC button,
    which users can click to set a rating for that QC block and for
    the overall subject rating ("FINAL"). Clicking on the QC button
    toggles its state through good (+), bad (X) or other (?, which may
    include 'ugly', or just a hint to revisit later).  Users can also
    use convenient 'filler buttons' at the right (A+, Ax, etc.), when
    the ratings are constant/uniform-- one hopes for 'all good'
    processing, but who knows...

Commenting
    Additionally, users can ctrl+click on the QC button to enter a
    comment for that block.  For example, they can write why a rating
    was good or bad, or what question they have led them to rate it as
    'other'.

Saving info
    If you have a local server running, then as you click and type
    the rating/QC buttons the information will be saved automatically
    in the QC directory.  The 'SAVE:' button in the upper-left corner
    shows whether the server is running:  
    + if the letters are green and visible, then the server is running
      (and your QC info is being saved).
    + if the letters are gray with a red line through them, then the
      server is not running (and your QC info is not being saved).
 
    It is useful to start the server and save QC/rating info for
    sharing and/or later using inclusion/exclusion criteria for the
    subject in group analysis.

    The ratings and comments are both saved in 'apqc_*.json'.
    The ratings are also saved in 'extra_info/out.ss_review.*.json', 
    which is a file that can be used as an input file to 
    gen_ss_review_table.py, combining the qualitative evaluations of
    the APQC HTML with the quantitative ones gathered by afni_proc.py,
    for systematic evaluation and QC of subject data.

'''
],
['''DEFINITIONS''', 
'''QC block 
    One thematic section of the QC form (original data, alignment
    step, etc.).  Each block has a label in the navigation bar (vorig,
    ve2a, etc.)-- click on the label to jump to that block. Click on
    the button below the label to provide a rating for that block.

QC buttons
    Below each QC block label in the navigation bar is a button
    (initially empty after running afni_proc.py). The user can click
    on it to toggle its state to one of three ratings (good, +; bad,
    X; other, ?), as well as to enter a comment for that QC block (via
    ctrl+click).

'FINAL' 
    Label for the QC button to hold the user's overall/final
    evaluation of the subject's data and processing. (Clicking on this
    label does nothing.)

filler buttons
    |A+|, |Ax|, |A?|: these are located in the upper right corner of the
    navigation menu.  These can be used to provide uniform ratings
    across the QC buttons (click to fill empty buttons, double-click
    to fill *all* buttons, regardless of state).  
    |clr|-- double-clicking on this will empty all ratings+comments
    from the QC buttons.

'SAVING'
    Denoting whether the local server is running or not.  Python's Flask
    module is used to start a local server, so QC ratings and comments 
    can be saved as they are made.  
    This mode is ON when the SAVING button text is green and unobscured, 
    and it is OFF when the text is gray and covered by a red line.

'HELP'
    Button : well, how did you get here??

'BC', 'AC'
    When using the 'pythonic' HTML style, 1D-plotting (like motion
    enorm plots) also produces boxplots to summarize values.  When
    censoring has been applied, by default there will be two boxplots
    made: one of values 'before censoring' (BC), and one of values
    'after censoring' (AC).  Those labels are affixed in order to the
    titles of the boxplots.

'''],
['''SET BUTTON RATING''', 
'''
To record an evaluation, click the button below any section label, and
toggle through:
    + : good,
    X : bad,
    ? : other (or 'revisit').

For speed, you can click 'filler button' |A+| once to fill all *empty*
buttons with +, or doubly to fill *all* buttons with +.  |Ax| behaves
the same for X, and |A?| for ?.

Double-click |clr| to clear all rating and comment values.

Pro-tip: if data are mostly all in a single state like good or bad,
just use filler buttons to save yourself click time, and then just
click any individual buttons that are different.  '''],
['''COMMENT''',
'''
Use ctrl+click (or cmd+click, on Macs) on a QC button to toggle a comment
window open/closed.

Save a comment with the green (left) button, or hit Enter at any point.
Remove a comment with the pink (right) button, or hit Esc at any point.

Any QC button with a comment gets a pair of quotes added, like ''+''.
Comments are independent of rating, but adding a comment to an empty
button changes its rating to ''?'' (which can be altered further from
there).
'''],
['''SAVE INFO''',
'''Have the local server running (check the 'SAVING' button in the 
upper-right corner).  

When the local server is running, the QC and rating information is saved
every time a button is updated.'''
],
['''KEYBOARD NAVIGATION''',
'''
Use Tab to navigate the QC menu mirroring all above functionality.

Hit Tab to move through the menu.  Hit Enter on a section label to
scroll the page there.

On QC buttons hit Enter to toggle through the rating list.  Use
ctrl+Enter to open comments; as above, use Enter or Esc to keep or
erase, respectively.

On the filler buttons |A+|, |Ax| and |A?|, use Enter to fill empty
QC buttons and ctrl+Enter to fill *all* buttons. 
On |clr|, ctrl+Enter clears all rating and comment values.  
'''],
['''QC BLOCKS''',
qcbh ]
]

# ---------------------------------------------------------------------

bhelp_hover = '''Click (or hit Enter) to open new help page.

             -- Quick help on QC buttons --

NAVIGATE
Scroll, or click a label ('vorig', 've2a', etc.) to jump to a section.

RATE + COMMENT
Click the QC button below each label to rate it, toggling through:
    +  :  good.
    X  :  bad,
    ?  :  other/revisit,
Use ctrl+click (or cmd+click, on Macs) on a QC button to toggle a comment
window open/closed. Comments also have save/clear buttons.

SPEEDIFY
There are 'filler buttons' for each rating: |A+|, |Ax|, |A?|.
Click once to fill all *empty* buttons with that rating, or
double click to fill *all* buttons (will overwrite) with that rating.
'''

# ---------------------------------------------------------------------

bgood_hover = '''Speed rating with:  +.
Click once (or Enter) to fill all *empty* QC buttons;
Double-click (or ctrl+Enter) to fill *all* QC buttons.  
'''

bbad_hover = '''Speed rating with:  X.
Click once (or Enter) to fill all *empty* QC buttons;
Double-click (or ctrl+Enter) to fill *all* QC buttons.  
'''

bother_hover = '''Speed rating with:  ?. 
Click once (or Enter) to fill all *empty* QC buttons;
Double-click (or ctrl+Enter) to fill *all* QC buttons.  
'''

bclear_hover = '''Clear all (!) QC buttons.
Double-click (or ctrl+Enter) to clear *all* QC buttons.
'''

# ---------------------------------------------------------------------

bsaving_hover = '''Display if QC/rating info is being saved.

Automatic saving is:
+ ON, if the text is green
+ OFF, if the text is gray with red strikethrough
'''

bsaving  = 'RATE:'
bhelp  = 'HELP'

#bhelp_hover = '''Open help page.
#Click once (or Enter) to open new help page.
#'''



def write_help_html_file( ofile, ocss ):

    # -------------- start ---------------------

    ht  = '''<html>\n'''

    # -------------- head ---------------------

    # use same style sheet as main pages
    ht+= '''
<head>
    <title>APQC help</title>
    <link rel="stylesheet" type="text/css" href="{ocss}" />
    <link rel="icon" type="icon.svg" href="extra_info/apqc_logo_help.svg"> 
</head>\n'''.format( ocss=ocss )

    ht+= '''
<!-- set background color of page -->
<style>
  body {
    background-color: #333333; /* dark grey */
  }
</style>
\n\n'''

    # -------------- body ---------------------

    ht+= '''<body>
    '''

    Nsec = len(apqc_help)

    for ii in range(Nsec):
        x = apqc_help[ii]

        ht+= wrap_block_title( x[0],
                               vpad=1,
                               addclass=" class='padtop' ",
                               blockid='',
                               do_close_prev_div=bool(ii))

        ht+= wrap_block_text( x[1],
                              addclass="class='container' " )



    # -------------- finish ---------------------

    ht+= '''</body>'''
    ht+= '''</html>'''

    # -------------- output ---------------------

    fff = open( ofile, "w" )
    fff.write( ht )
    fff.close()

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# -------------- functions for main HTML file generation ---------------

def parse_apqc_text_fields(tt) :
    '''Input: tt can be either a string or list of strings.

    Output: a single string, concatenation of the list (if input as
    such).  Special case parsing happens if one string contains a flag
    to include a PBAR in the string.
    '''

    out = ""

    # not all versions of python seem to have 'unicode' type
    try:
        if type(tt) == unicode :
            tt = [str(tt)]
    except:
        if type(tt) == str :
            tt = [tt]

    Ntt = len(tt)

    for ii in range(Ntt) :
        ss = tt[ii]
        #print(ss)
        if ss.__contains__(PBAR_FLAG) :
            sspbar = make_inline_pbar_str(ss)
            out+= sspbar
        else:
            out+= ss 
        if ii < (Ntt - 1) :
            out+= "\n"

    return out

# -------------------------------------------------------------------

def make_inline_pbar_str(ss) : 
    '''Input: ss is single string of form:  {PBAR_FLAG}:{pbar.json}

    Output: a single string, formatted to put pbar inline in text, and
    possibly have a second line of thr info.

    '''

    add_indent = ss.find(PBAR_FLAG)
    if add_indent >= 0 :
        str_indent = add_indent * ' '
    else:
        sys.error("** ERROR: missing pbar flag in {}".format(ss))

    # file name of JSON should be only thing after ":"
    fname_json = dir_img + "/"  + ss.split(":")[1]

    # pbar dict keys given in @chauffeur_afni
    with open(fname_json, 'r') as fff:
        pbar_dict = json.load(fff)    

    if not(pbar_dict.__contains__('pbar_for')) :
        pbar_dict['pbar_for'] = 'olay'
    
    out = str_indent + ''' {pbar_for}: {pbar_bot} <img  class='pbar'  style="display: inline;" src="media/{pbar_fname}" > {pbar_top}'''.format(**pbar_dict)

    if pbar_dict['pbar_comm'] :
        out+= ''' ({pbar_comm})'''.format(**pbar_dict)

    if pbar_dict['vthr'] :
        out+= '''\n'''
        out+= str_indent + ''' thr : {vthr}'''.format(**pbar_dict)

        if pbar_dict['vthr_comm'] :
            out+= ''' ({vthr_comm})'''.format(**pbar_dict)

    if pbar_dict['gen_comm'] :
        out+= '''\n'''
        out+= str_indent + '''{gen_comm}'''.format(**pbar_dict)

    return out


# -------------------------------------------------------------------

# these are the properties/fields that the incoming text might have.
# These define what fields the jsons created by @ss_review_html (and
# therefore, ultimately in lib_apqc_tcsh.py) can have.
class apqc_item_info:

    title       = ""
    text        = ""
    subtext     = ""
    nv_html     = ""
    av_file     = ""
    ic_file     = ""
    ic_args     = ""
    gv_file     = ""
    gv_args     = ""
    itemtype    = ""
    itemid      = ""
    blockid     = ""
    blockid_hov = ""
    warn_level  = ""   # [PT: May 21, 2019]

    def set_title(self, DICT):
        if 'title' in DICT :
            self.title = DICT['title']

    def set_itemtype(self, DICT):
        if 'itemtype' in DICT :
            self.itemtype = DICT['itemtype']

    def set_itemid(self, DICT):
        if 'itemid' in DICT :
            self.itemid = DICT['itemid']

    def set_blockid(self, DICT):
        if 'blockid' in DICT :
            self.blockid = DICT['blockid']

    def set_blockid_hov(self, DICT):
        if 'blockid_hov' in DICT :
            self.blockid_hov = DICT['blockid_hov']
            
    def set_warn_level(self, DICT):
        if 'warn_level' in DICT :
            self.warn_level = DICT['warn_level']

    def set_nv_html(self, DICT):
        if 'nv_html' in DICT :
            self.nv_html = DICT['nv_html']

    def set_av_file(self, DICT):
        if 'av_file' in DICT :
            self.av_file = DICT['av_file']

    def set_ic_file(self, DICT):
        if 'ic_file' in DICT :
            self.ic_file = DICT['ic_file']

    def set_ic_args(self, DICT):
        if 'ic_args' in DICT :
            self.ic_args = DICT['ic_args']

    def set_gv_file(self, DICT):
        if 'gv_file' in DICT :
            self.gv_file = DICT['gv_file']

    def set_gv_args(self, DICT):
        if 'gv_args' in DICT :
            self.gv_args = DICT['gv_args']

    # [PT: May 16, 2019] updated to deal with parsing of PBAR stuff here
    def add_text(self, DICT):
        if 'text' in DICT :
            xx = parse_apqc_text_fields(DICT['text'])
            self.text+= xx
#            if type(DICT['text']) == list :
#                xx = '\n'.join(DICT['text'])
#                self.text+= xx
#            else:
#                self.text+= DICT['text']

    # [PT: May 16, 2019] updated to deal with parsing of PBAR stuff here
    def add_subtext(self, DICT):
        if 'subtext' in DICT :
            xx = parse_apqc_text_fields(DICT['subtext'])
            self.subtext+= xx
#            if type(DICT['subtext']) == list :
#                xx = '\n'.join(DICT['subtext'])
#                self.subtext+= xx
#            else:
#                self.subtext+= DICT['subtext']

    # this just runs through all possible things above and fills in
    # what it can
    def set_all_from_dict(self, DICT):
        self.set_title(DICT)
        self.set_itemtype(DICT)
        self.set_itemid(DICT)
        self.set_blockid(DICT)
        self.set_blockid_hov(DICT)
        self.add_text(DICT)
        self.set_nv_html(DICT)
        self.set_av_file(DICT)
        self.set_ic_file(DICT)
        self.set_ic_args(DICT)
        self.set_gv_file(DICT)
        self.set_gv_args(DICT)
        self.add_subtext(DICT)
        self.set_warn_level(DICT)

# -------------------------------------------------------------------

# these are the properties/fields that the incoming *page-title* info
# might have.  These define what fields the jsons created by
# @ss_review_html (and therefore, ultimately in lib_apqc_tcsh.py) can
# have.
class apqc_title_info:

    title       = ""
    subj        = ""
    taskname    = ""
    itemtype    = ""
    itemid      = ""
    blockid     = ""
    blockid_hov = ""

    def set_title(self, DICT):
        if 'title' in DICT :
            self.title = DICT['title']

    def set_itemtype(self, DICT):
        if 'itemtype' in DICT :
            self.itemtype = DICT['itemtype']

    def set_itemid(self, DICT):
        if 'itemid' in DICT :
            self.itemid = DICT['itemid']

    def set_blockid(self, DICT):
        if 'blockid' in DICT :
            self.blockid = DICT['blockid']

    def set_blockid_hov(self, DICT):
        if 'blockid_hov' in DICT :
            self.blockid_hov = DICT['blockid_hov']

    def set_taskname(self, DICT):
        if 'taskname' in DICT :
            self.taskname = DICT['taskname']

    def set_subj(self, DICT):
        if 'subj' in DICT :
            self.subj = DICT['subj']

    # this just runs through all possible things above and fills in
    # what it can
    def set_all_from_dict(self, DICT):
        self.set_title(DICT)
        self.set_itemtype(DICT)
        self.set_itemid(DICT)
        self.set_blockid(DICT)
        self.set_blockid_hov(DICT)
        self.set_taskname(DICT)
        self.set_subj(DICT)

# -------------------------------------------------------------------

def write_json_file( ll, fname ):

    olist = []
    olist.append( json_cols )

    # skip the first element here because it came from the title, and
    # that doesn't have a QC button with it (it's just the Top/Home of
    # the page).
    for i in range(1,len(ll)):
        x = ll[i]
        olist.append( [x[0], "null", ""] )
    
    # output with indentation
    ojson = json.dumps( olist, indent=4 )
    fff = open( fname, "w" )
    fff.write( ojson )
    fff.close()

# --------------------------------------------------------------------

def write_list_ids_file(oids, list_ids):
    otxt = ''
    N = len(list_ids)
    for ii in range(N):
        otxt+= ' ' * (list_ids[ii][0] == 'itemid') * 4
        otxt+= list_ids[ii][1] + '\n'
    
    fff = open( oids, "w" )
    fff.write( otxt )
    fff.close()


# --------------------------------------------------------------------

# !!! UNUSED
#def wrap_block_lab(x, vpad=0):
#    y = """<h3><center>block: """+x+"""</center></h3>"""
#    if vpad:
#        y= """\n"""+y
#        y+="""\n"""
#    return y

# -------------------------------------------------------------------

def make_nav_table(llinks, subj='', max_wlevel=''):
    # table form, not ul 
    N = len(llinks)
    idx = 0

    # =======================================================================
    # dummy, background nav

    y = '''
<!-- make dummy navbar, which sits in the background -->
<div class="navbar">
  <table style="width: 100%">

    <tr>
      <td style="width: 100%">
        <a style="text-align: left"> {0} </a>
      </td>
    </tr>

    <tr>
      <td style="width: 100%">
        <button class="button-generic button-LHS btn0" 
                onclick="">
        {0} </button>
      </td>
    </tr>

  </table>
</div> <!-- end of dummy navbar -->
'''.format( NULL_BTN0 ) 

    # =======================================================================
    # real, foreground nav
    # + use z-index to keep it always on top

    y+= '''
<!-- start of real/foreground navbar (uses z-index to keep on top) -->
<div class="navbar" style="z-index: 10;">
'''

    # -----------------------------------------------------
    # L-floating part: section anchors and rating buttons
    # NB: these are fixed width

    for i in range(0, N):
        ll, hov = llinks[i][0], llinks[i][1]
        #print(ll)

        color_change = ''
        
        # add colors to warning button (and nothing else)
        if ll == 'warns' and max_wlevel :
            wcol = lahc.wlevel_colors[max_wlevel]
            if lahc.wlevel_ranks[max_wlevel] > lahc.wlevel_ranks['mild'] :
                finaltab = '''style="color: {}; '''.format("#000") 
                finaltab+= '''background-color: {};" '''.format(wcol)
            else:
                finaltab = '''style="color: {};" '''.format(wcol) 
        else:
            finaltab = ''

        # new table
        y+= '''
  <!-- start QC button table for block={ll} -->
  <table style="float: left">
'''.format( ll=ll )

        # TOP ROW (blockid)
        if i :
            y+= '''
    <!-- top button for block={ll} -->
    <tr>
      <td class="td1" id=td1_{ll}>
        <button class="button-generic button-LHS btn5" id="btn5_{ll}" 
        onmousedown="moveToDiv(hr_{ll})" 
        title="{hov}" 
        {finaltab} 
        onkeypress="if ( event.keyCode == 13 ) {{ moveToDiv(hr_{ll}); }}">
        {ll}</button>
      </td>
    </tr>
'''.format( ll=ll, hov=hov, finaltab=finaltab ) 
        else:
            # this is specifically for the HOME/jump button
            # &#8679TOP&#8679
            # ACME&#8679
            y+= '''
    <!-- top button for block={ll} -->
    <tr>
      <td class="td1" id=td1_{ll}>
        <button class="button-generic button-LHS btn0" id="btn5_{ll}" 
        onmousedown="moveToDiv(hr_{ll})" 
        title="{hov}" 
        {finaltab} 
        onkeypress="if ( event.keyCode == 13 ) {{ moveToDiv(hr_{ll}); }}">
        TOP&#8679</button>
      </td>
    </tr>
'''.format( ll=ll, hov=hov, finaltab=finaltab ) 

        # BOT ROW (QC button)
        if i :
            # NB: with button clicks, if using onkeypress with
            # onclick, the former *also* drives the latter a second
            # time, so get annoying behavior; hence, distinguish those
            y+= '''
    <!-- bot button for block={ll} -->
    <tr>
      <td>
        <button class="button-generic button-LHS btn1" 
                id="btn1_{ll}" data-txtcomm="" 
                onmousedown="btn1Clicked(event, this)" 
        onkeypress="if ( event.keyCode == 13 ) {{ btn1Clicked(event, this); }}" 
        {txt}</button>
      </td>
    </tr>
'''.format( ll=ll, txt=NULL_BTN1 )
        else:
            bhelp  = 'HELP'
            y+= '''
     <!-- bot button for block={lab} -->
    <tr>
      <td>
        <button class="button-generic button-RHS btn3saving" id=td3_{lab}
                title="{hov}"   
              onclick="colorizeSavingButton(is_served)">
        {txt}</button>
      </td>
    </tr>
'''.format( lab=bsaving, hov=bsaving_hover, txt=bsaving )



        if i :
            # ~dropdown form button
            # NB: the onkeydown stuff makes it that hitting "Enter"
            # (event.keyCode == 10 || event.keyCode == 13) inside the
            # text field is like submitting the text (and the
            # .preventDefault() means that it does NOT input a
            # newline):
            # https://stackoverflow.com/questions/155188/trigger-a-button-click-with-javascript-on-the-enter-key-in-a-text-box
            # https://stackoverflow.com/questions/26975349/textarea-wont-stop-making-new-line-when-enter-is-pressed
            # ... and hitting "Esc" (event.keyCode == 27) is like
            # canceling.
            y+= '''
</table>
<!-- top of QC button comment form for block={ll} -->
<div class="form-popup" id="cform_{ll}" > 
    <form class="form-container" onsubmit="return false;"> 
    <textarea type="text" placeholder="Enter comment" 
    rows="4" cols="40" id="comm_{ll}" 
    onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ 
       event.preventDefault(); 
       keepFromCommentForm(comm_{ll}.id, cform_{ll}.id);}} 
       else if (event.keyCode == 27) {{ 
           clearCommentForm(comm_{ll}.id, cform_{ll}.id); }}">
    </textarea>  
    <button type="button" class="btn" 
    onclick="keepFromCommentForm(comm_{ll}.id, cform_{ll}.id)">
    keep+close</button> 
    <button type="button" class="btn cancel" 
    onclick="clearCommentForm(comm_{ll}.id, cform_{ll}.id)">
    clear+close</button> 
    </form> 
</div> <!-- bot of QC button comment form -->
'''.format( ll=ll )

    y+= '''
<!-- END of QC block buttons -->
'''

    # add in subj ID
    y+= '''
<!-- show subj ID in navbar -->
<table style="float: left">
  <tr>
    <td style="width: fit-content;">
    <p class="subj_text">{subj}</p>
    </td>
  </tr>
</table>
'''.format(subj=subj)



    # ------------------------------------------------------ 
    # R-floating part: subj ID and SAVE button 
    # NB: this is flexible width
    bgood  = 'A+'  ; bgood_ind  =  1 
    bbad   = 'Ax'  ; bbad_ind   =  2 
    bother = 'A?'  ; bother_ind =  0 
    bclear = 'clr' ; bclear_ind = -1

    # Start right-side table
    y+= '''
<!-- START of (right-floating) quick buttons -->
<table style="float: right; margin-right: 2px">'''

    # ROW: "all fill" buttons-- click (or Enter) fills empty QC buttons,
    # dblclick (or ctrl+Enter) fills ALL QC buttons
    ### NTS: could add more <td>s here, but doesn't appear necessary
    y+= '''
  <tr>
    <td style="width: 140px; white-space:nowrap;">
'''

    y+= '''
      <button class="button-generic button-RHS button-RHS-little btn2{lab}" 
      title="{hov}" 
      onmousedown="allYourBaseAreBelongToUs({ind})" 
      onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ 
        if (event.ctrlKey) {{
          reallyAllYourBaseAreBelongToUs({ind}); 
        }} else {{ 
          allYourBaseAreBelongToUs({ind}); 
        }}
      }} " 
      ondblclick="reallyAllYourBaseAreBelongToUs({ind})"> 
      {txt}</button>
'''.format( lab='good', hov=bgood_hover, ind=bgood_ind, txt=bgood )

    y+= '''
      <button class="button-generic button-RHS button-RHS-little btn2{lab}" 
      title="{hov}" 
      onmousedown="allYourBaseAreBelongToUs({ind})" 
      onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ 
        if (event.ctrlKey) {{ 
          reallyAllYourBaseAreBelongToUs({ind});
        }} else {{
          allYourBaseAreBelongToUs({ind});
        }}
      }} " 
      ondblclick="reallyAllYourBaseAreBelongToUs({ind})"> 
      {txt}</button>
'''.format( lab='bad', hov=bbad_hover, ind=bbad_ind, txt=bbad )

    y+= '''
      <button class="button-generic button-RHS button-RHS-little btn2{lab}" 
      title="{hov}" 
      onmousedown="allYourBaseAreBelongToUs({ind})" 
      onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ 
        if (event.ctrlKey) {{ 
          reallyAllYourBaseAreBelongToUs({ind});
        }} else {{ 
          allYourBaseAreBelongToUs({ind}); 
        }}
      }} " 
      ondblclick="reallyAllYourBaseAreBelongToUs({ind})"> 
      {txt}</button>
'''.format( lab='other', hov=bother_hover, ind=bother_ind, txt=bother )

    y+= '''
    </td>
  </tr>
'''

    # ROW:  hyperlinks (anchors) within the page; could add more <td>
    y+= '''
  <!-- bot row: clear and help buttons -->
  <tr>
    <td style="width: 140px; white-space:nowrap;" id=td3_TOBEDETERMINED>
'''

    y+= '''
      <button class="button-generic button-RHS button-RHS-little btn2{lab}" 
      title="{hov}" 
      onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ 
        if (event.ctrlKey) {{ 
          reallyAllYourBaseAreBelongToUs({ind}); 
        }} 
      }} " 
      ondblclick="reallyAllYourBaseAreBelongToUs({ind})"> 
      {txt}</button>
'''.format( lab='clear', hov=bclear_hover, ind=bclear_ind, txt=bclear )

    y+= '''
    <button class="button-generic button-RHS btn3{lab}"
            title="{hov}" 
            onclick="doShowHelp()">
    {txt}</button>
'''.format( lab='help', hov=bhelp_hover, txt=bhelp )

    y+= '''
    </td>
  </tr>
</table> <!-- end of right-side table of buttons -->
</div> <!-- end of real/foreground navbar -->
'''

    return y

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------

# JAVASCRIPT script functions and variables

def make_javascript_btn_func(subj ):

    y = ''

    y+= '''<script type="text/javascript">
'''

    y+= '''
// global vars
var allBtn1, allTd1, allhr_sec;  // selection/location IDs
var topi, previ;                 // keeps our current/last location
//var subj_id  = "{subj}";            // subject's ID
var jsonfile = "apqc_{subj}.json";  // json file: apqc_SUBJ.json
var json_ssrev = "extra_info/out.ss_review.{subj}.json"; // ss_review JSON
var qcjson = {{}};                 // I/O json of QC button responses
var nb_offset = 66-1;            // navbar height: needed for scroll/jump

// properties of QC buttons that get toggled through
const bkgds   = [ "#fff" , "#67a9cf", "#d7191c"  ];
const valeurs = [ "?"    , "+"      , "X"        ];
const tcols   = [ "#777" , "#FFF"   , "#000"     ];

// check where server is running (to alert users before filling in buttons)
let url       = window.location.href
let origin    = window.location.origin
let is_served = url.startsWith('file:') ? false : true

console.log('URL', url)
console.log('is_served', is_served)
'''.format( subj=subj )

    y+= '''
/* For using is_served to set saving button color:
    First, get the root element, which has color
    defined
*/
var r = document.querySelector(':root');

/* ... then, this function will help set colors
*/
function colorizeSavingButton(val) {
  if (val) {
    r.style.setProperty('--SavingTextCol', '#009933');
    r.style.setProperty('--SavingBkgdCol', '#000'); //'#fff');
    r.style.setProperty('--SavingTextDec', 'none');

    r.style.setProperty('--SavingTextColB6', '#000');
    r.style.setProperty('--SavingBkgdColB6', '#029a64');
  } else {
    r.style.setProperty('--SavingTextCol', '#9f9f9f');
    r.style.setProperty('--SavingBkgdCol', '#000'); //'#fff');
    r.style.setProperty('--SavingTextDec', 'line-through');

    r.style.setProperty('--SavingTextColB6', '#016843');
    r.style.setProperty('--SavingBkgdColB6', '#016843');
  }
}

/* ... finally, use and colorize */
colorizeSavingButton(is_served);
'''

    y+= '''

/*
   Do both checking of server status and resetting of the is_served
   var if it is no longer running.  Also, ensure button colors are 
   up-to-date.
*/
function checkServerStatus() {
  return fetch(url, { method: 'GET' })
    .then(response => {
      if (is_served && response.ok) { 
        // Server is alive
        return true;
      } else {
        // Server is not alive
        return false;
      }
    })
    .catch(error => {
      // Server is not alive or there was an error
      console.log('+* Warning: the server is not serving: ' + error);
      return false;
    });
}

async function updateServerStatus() {
  let isAlive = await checkServerStatus()
      if (isAlive) {
        console.log("Server is up");
      } else {
        is_served = false;
        colorizeSavingButton(is_served);
      }
   console.log("Report on update. Server status is: "+is_served);
}

/* Different than checking server status; this returns OK if: 1) the
   page was started with a server running and the server is still
   running now; 2) the page was started without a server at all. 
   
   This is useful for the help button to work, for example.
*/
function checkPageStatus() {
  return fetch(url, { method: 'GET' })
    .then(response => {
      if (response.ok) { 
        // started as non-server, or started with server AND still okay
        return true;
      } else {
        // started with server AND server not okay
        return false;
      }
    })
    .catch(error => {
      // Server is not alive or there was an error
      console.log('+* Warning: the page has some issue: ' + error);
      return false;
    });
}

/* for the NV button, toggle NiiVue instance on/off by id */
async function toggle_niivue(is_served, id) {
  await updateServerStatus();
  if ( is_served ) {
    let element = document.getElementById(id);
    let current_disp = element.style.display; // current disp value
    // toggle between 'block' and 'none'
    element.style.display = current_disp === '' ? 'block' : '';
    if (element.style.display == ''){
      // remove all children from the element
      while (element.firstChild) {
          element.removeChild(element.firstChild);
      }
    } else {
      // add an iframe to the element. The iframe will load its 
      // own niivue js and images
      let html_name = './' + id + '.html';
      let nv_iframe = document.createElement('iframe');
      nv_iframe.src = html_name;
      nv_iframe.frameBorder = '0';
      nv_iframe.style.display = 'block';
      nv_iframe.style.width = '100%';
      nv_iframe.style.height = 'auto';
      nv_iframe.style.aspectRatio = '4.5/1'; // accommodate hide olay btn
      element.appendChild(nv_iframe);
    }
  }
}


/* show/hide olay in NV (for align checks)
    obj : NV object ID
    bid : button ID in that object's NV canvas
*/
function niivue_ShowHideOlay(obj, bid) {
  let element = document.getElementById(bid);
  if (obj.volumes[1].opacity) {
    obj.setOpacity(1, 0);
    element.innerText = "View Olay";
  } else {
    obj.setOpacity(1, 1);
    element.innerText = "Hide Olay";
  }
}

'''


    # --------------- load/reload initialize -----------------

    # gets run at loadtime
    y+= '''
//window.onload = function() {
async function RunAtStart() {
  // initialize arrays and location
  initializeParams();

  // read in JSON, if we have server running
  if ( is_served ) {
    qcjson = await loadJSON(jsonfile)
    console.log(qcjson)
    CheckJsonfileMatchesQcbuttons();
    ApplyJsonfileToQcbuttons();
  }
};

'''

    y+= '''
// used for string formatting numbers (C Rorden), below
function flt2str0(flt, ndec = 0) {
    //retain trailing zero
    return flt.toFixed(ndec); //return string
}

/* more string formatting numbers, below ... WITH directionality here,
   assuming the coords are what AFNI calls LPI (and what some other
   software call RAS); basically, L/P/I are neg coords, and R/A/S are
   positive.  
*/
function flt2str0_dir(flt, ndec = 0, dir = '' ) {
    //retain trailing zero
    if ( dir == '' ) {
      return flt.toFixed(ndec); //return string
    } else if ( dir == 'RL' ) {
      let aflt = Math.abs(flt);
      let lab  = (flt < 0 ) ? 'L' : 'R';
      return aflt.toFixed(ndec) + lab;
    } else if ( dir == 'AP' ) {
      let aflt = Math.abs(flt);
      let lab  = (flt < 0 ) ? 'P' : 'A';
      return aflt.toFixed(ndec) + lab;
    } else if ( dir == 'IS' ) {
      let aflt = Math.abs(flt);
      let lab  = (flt < 0 ) ? 'I' : 'S';
      return aflt.toFixed(ndec) + lab;
    } else {
      return ''; 
    }
}

'''

    y+= '''
/*
  OFF AT THE MOMENT, but a guard for reloading page
*/
window.onbeforeunload = function(event)
{
  doQuit()
  let seriouslyQuit = confirm()
  if (seriouslyQuit) {
    doQuit()
  }
  return seriouslyQuit;
};
'''

    y+= '''
/* This function gets run when page loads ("onload").
   The classes matter for identifying properties that certain buttons
   or other objects have:
     btn1   : QC rating buttons
     td1    : QC comment buttons
     hr_sec : locations for where to jump for each QC block ID
*/
function initializeParams() {
    allBtn1   = document.getElementsByClassName("btn1");   // btn1_vepi, btn1_*
    allTd1    = document.getElementsByClassName("td1");    // td1_vepi,  td1_*
    allhr_sec = document.getElementsByClassName("hr_sec"); // hr_vepi,   hr_*

    topi      = findTopSectionIdx();      // idx of current loc
    previ     = topi;                     // init "previous" idx
    setTd1Border(topi, "#ffea00");        // show location
}
'''

    # read in JSON file, from:
    # https://codepen.io/KryptoniteDove/post/load-json-file-locally-using-pure-javascript
    # https://stackoverflow.com/questions/7346563/loading-local-json-file
    # importantly, the xobjs.open(...) func NEEDS to have the 'false'
    # set to read synchronously, which is necessary for the JSON to
    # load fully before use-- slower, but this is a small file
    y+= '''
/*
  Read in JSON file.  Importantly, the xobjs.open(...) func NEEDS to
  have the 'false' set to read synchronously, which is necessary for
  the JSON to load fully before use---slower, but this is a small file.
*/
async function loadJSON(ifile) {
    let json = await fetch(ifile)
    .then(response => response.json())
    return json
}

async function postJSON(data = {}, quit=false) {
    let route = ''
    if (quit){
      route = 'quit'
    } else {
      route = 'save'
    }
    let url = origin + '/' + route

    // Default options are marked with *
    const response = await fetch(url, {
        
    /* from: *GET, POST, PUT, DELETE, etc. */
    method: quit ? 'GET' : 'POST',

    /* from: *default, no-cache, reload, force-cache,
    only-if-cached */
    cache: 'no-cache', 

    headers: {
      'Content-Type': 'application/json'
    },

    /* from: no-referrer, *no-referrer-when-downgrade, origin,
    origin-when-cross-origin, same-origin, strict-origin,
    strict-origin-when-cross-origin, unsafe-url */
    referrerPolicy: 'no-referrer', 

    /* body data type must match "Content-Type" header */
    body: quit ? null : JSON.stringify(data) 
  });

  // parse JSON response into native JavaScript objects
  qcjson = await response.json()
  console.log(qcjson)
  return qcjson; 
}

/* used to be able to run pre-built scripts in the AP results 
   directory.  These are fired up when the AV button by an image
   is clicked, via doRunAV(...).
*/
async function postJSON_AV(data = {}) {
    let url = origin + '/' + 'run_av'

    // Default options are marked with *
    const response = await fetch(url, {
        
    /* from: *GET, POST, PUT, DELETE, etc. */
    method: 'POST',

    /* from: *default, no-cache, reload, force-cache,
    only-if-cached */
    cache: 'no-cache', 

    headers: {
      'Content-Type': 'application/json'
    },

    /* from: no-referrer, *no-referrer-when-downgrade, origin,
    origin-when-cross-origin, same-origin, strict-origin,
    strict-origin-when-cross-origin, unsafe-url */
    referrerPolicy: 'no-referrer', 

    /* body data type must match "Content-Type" header */
    body: JSON.stringify(data) 
  });
}
'''

    y+= '''
/*
  Both the QC element names AND their order need to match
*/
function CheckJsonfileMatchesQcbuttons() {
    var Nele = qcjson.length;

    for( var i=1; i<Nele; i++ ) {
        var ele = qcjson[i][0];

        var jj = i - 1;  // offset because of col heading in JSON
        var bname = new String(allBtn1[jj].id); 
        var bpost = bname.slice(5);  // skip the 'btn1_' part of button ID

        if ( bpost != ele ) {
             window.alert("**Error: postfix on button ID " + bname + " does not match with JSON entry " + ele); 
             throw "DONE";
        }
    }
}
'''

    y+= '''
/* This function gets run when page loads ("onload").

   Because order matches (offset by 1), we can just apply directly
   with the counting index, based on the allBtn1 list.
*/
function ApplyJsonfileToQcbuttons() {
    var Nele = qcjson.length;

    for( var i=1; i<Nele; i++ ) {
        var jj = i - 1;  // offset because of col heading in JSON
        var bid = new String(allBtn1[jj].id);

        // Set the comments first, because the button presentation 
        // of rating depends on whether that has been set
        sendCommentToButtonAndForm(qcjson[i][2], bid);
        sendRatingToButton(qcjson[i][1], bid);
    }
}
'''

    y+= '''
/*    
   This function gets run when page loads ("onload").  'ss' is the
   JSON rating, and 'bid' is the button ID in AllBtn1.
*/
function sendRatingToButton(ss, bid) {
    if ( ss == "good" ) {
       setThisButtonRating(bid, 1);
    } else if ( ss == "bad" ) {
       setThisButtonRating(bid, 2);
    } else if ( ss == "other" ) {
       setThisButtonRating(bid, 0);
    } else if ( ss == "null" || ss == "" ) {
       setThisButtonRating(bid, -1);
    } else {
      window.alert("**Error: unallowed rating in JSON:" + ss);
      throw "DONE";
    }
}
'''

    y+= '''
/*
   When the JSON is read in, get comments and give any text to both
   the btn1 and associated comment form textarea
*/
function sendCommentToButtonAndForm(comm, bid) {
    thisButtonGetsAComment(bid, comm);

    var bname = new String(bid); // basename
    // skip the 'btn1_' part of button ID
    var cid   = 'comm_' + bname.slice(5);

    // because of how "null" is read in; this just matters in form
    if ( comm == "null" ) {
        var comm = "";
    }
    thisFormTextAreaGetsAComment(cid, comm);
}
'''

    # --------------- scroll location in page stuff -----------------

    y+= '''
/*
  Checks/rechecks whenever change in page location occurs.
*/
window.addEventListener("scroll", function(event) {
    var newi = findTopSectionIdx();

    if ( newi != topi ) {
        setTd1Border(newi, "#ffea00"); 
        setTd1Border(topi, "inherit");
        previ = topi;
        topi  = newi;
    }
}, false);
'''

    y+= '''
/*
  Go through (short) list from top, and first one that has pos
  coor, is the one at top
*/
function findTopSectionIdx() {
    for( var i=0; i<allhr_sec.length; i++ ) {
        var bid = allhr_sec[i].id; 
        var bbox = document.getElementById(bid).getBoundingClientRect();
        if ( bbox.top - nb_offset > 1 ) {
            break;
        }
    }
    return i-1;
}
'''

    y+= '''
function setTd1Border(ii, bkgdcol) {
    var newtid = allTd1[ii].id;
    document.getElementById(newtid).style.background = bkgdcol;
}
'''

    # --------------- QC button: toggle indiv or fill group -------------
    
    y+= '''
/*
  A click on the QC buttons will scroll through the color values;
  ctrl+click on the QC buttons will toggle between the comment
  form being open or closed (saving what is in form when closing).
  [PT: July 2, 2023] also recognize meta+click (= command+click on Mac)
  like a ctrl+click, because of Mac webpage behavior---thanks, DRG.
*/
async function btn1Clicked(event, button) {
  await updateServerStatus();
  if ( is_served ) {  
    if (event.metaKey) {
       btn1ClickedWithCtrl(event, button);
    }  else if (event.ctrlKey) {
       btn1ClickedWithCtrl(event, button);
    }  else {
       changeColor(button); //alert("The CTRL key was NOT pressed!");
    }
  }
}
'''

    y+= '''
function btn1ClickedWithCtrl(event, button) {
    // get the form ID from button ID
    var bname = new String(button.id);
    var bpost = bname.slice(5); // skip the 'btn1_' part of button ID
    var cFormID = 'cform_' + bpost;
    // if closed, this opens it; otherwise, it closes it
    if ( document.getElementById(cFormID).style.display == false ||
         document.getElementById(cFormID).style.display == "none" ) {
         openCommentForm(cFormID, button.id); 
    } else {
         keepFromCommentFormViaBtn1(button.id, cFormID);
    }
}
'''


    # Toggle individual;
    ## ... and VERY useful comment about the "!important" keyword for
    ## hovering after changing DOM properties.
    ## https://stackoverflow.com/questions/46553405/css-hover-not-working-after-javascript-dom
    y+= '''
/*
  Toggle individual button colors/etc.
*/
function changeColor(button) {
  newidx = Number(button.dataset.idx || 0);     // idx=0 on first click
  newidx = (newidx + 1) % bkgds.length;         // calc new idx, mod Ncol
  button.dataset.idx       = newidx;            // store new idx in ele
  button.style.color       = tcols[newidx];     // set color
  button.style.background  = bkgds[newidx];     // set bkgd
  button.style.borderColor = bkgds[newidx];     // set bkgd
  button.textContent       = valeurs[newidx];
  checkIfButtonCommented( button );             // set text
  doSaveAllInfo();
}
'''

    y+= '''
function checkIfButtonCommented( button ) {
    var value = button.textContent;
    var bcomm = button.dataset.txtcomm;
    var VAL_HAS_QUOTE = value.includes(`"`);

    // if no comment, make sure there is no
    if ( ( bcomm == "" || bcomm == "null" ) ) {
       if ( VAL_HAS_QUOTE ) {
         var newval = value.replace(/\"/g, "");
         button.textContent = newval;
       }
    } else {
       if ( !VAL_HAS_QUOTE ) {
          button.textContent = `"` + value + `"`;
       }
    }
}
'''

    # two arguments: the button ID 'bid' from an element of
    # AllBt1n, and the 'idx' which picks out valeurs[idx]
    # etc. properties.
    y+= '''
/*
  two arguments: 
  + the button ID 'bid' from an element of AllBt1n, 
  + the 'idx' which picks out valeurs[idx] etc. properties.
*/
function setThisButtonRating(bid, idx) {{
    // normal values
    if ( idx >= 0 ) {{
      document.getElementById(bid).textContent       = valeurs[idx];
      document.getElementById(bid).style.background  = bkgds[idx];
      document.getElementById(bid).style.borderColor = bkgds[idx];
      document.getElementById(bid).style.color       = tcols[idx];
      document.getElementById(bid).dataset.idx       = idx;
      checkIfButtonCommented( document.getElementById(bid) );
    
    }} else {{
      // the reset, for "null" JSON
      document.getElementById(bid).textContent       = "{}";
      document.getElementById(bid).style.background  = '';  // reset to CSS
      document.getElementById(bid).style.borderColor = '';  // reset to CSS
      document.getElementById(bid).style.color       = '';  // reset to CSS
      document.getElementById(bid).dataset.idx       = 0;   // null
    }}
}}
'''.format ( NULL_BTN1 )

    y+= '''
function isBtn1InNullState( bid ) {{
    var tc = document.getElementById(bid).textContent;
    if ( tc == "{}" ) {{
        return true;
    }} else {{
        return false;
    }}
}}
'''.format ( NULL_BTN1 )

    y+= '''
/*
  two arguments: 
  + the button ID 'bid' from an element of AllBt1n,
  + the 'comment' that gets added/overwritten (in the newly created
  element, txtcomm).  
  Basically used to put the form comments into the button fields, and
  then later into jsons.
*/
function thisButtonGetsAComment(bid, comm) {
    document.getElementById(bid).dataset.txtcomm = comm;

    // and don't allow a null state anymore if it has a comment:
    // update it to "other"/"?"
    if ( comm == "" || comm == "null" ) {
    } else {
       if ( isBtn1InNullState(bid) ) {
           setThisButtonRating(bid, 0);
       }
    }

    // and reset quotes, if necessary.
    //window.alert(bid);
    checkIfButtonCommented( document.getElementById(bid) );
}
'''

    y+= '''
/*
  "ALL OTHER" fill button, here to set every btn1-button value to
  "+" or "x", depending on input arg 'ii' (index in list)
*/
async function allYourBaseAreBelongToUs(ii) {{ 
  await updateServerStatus();
  if ( is_served ) {{
    for( var i=0; i<allBtn1.length; i++ ) {{ 
      var bid = allBtn1[i].id; 
      var ival = document.getElementById(bid).textContent; 
      if ( ival == "{}" ) {{ 
        setThisButtonRating(bid, ii); 
      }}
    }} 
    doSaveAllInfo();
  }}
}}
'''.format( NULL_BTN1 )

    y+= '''
/*
  "ALL-ALL" fill button: regardless of initial state set every
  btn1-button value to "+" or "x", depending on input arg 'ii'
  (index in list); that is, this overruns earlier button values
*/
async function reallyAllYourBaseAreBelongToUs(ii) { 
  await updateServerStatus();
  if ( is_served ) {
    for( var i=0; i<allBtn1.length; i++ ) { 
      var bid = allBtn1[i].id; 
      var ival = document.getElementById(bid).textContent; 
      setThisButtonRating(bid, ii);
      if ( ii < 0 ) {
         sendCommentToButtonAndForm("", bid);
      }
    }
    doSaveAllInfo();
  }
} 
'''

    # ------------------- commentize form ------------------------
    
    y+= '''
/*
  Get position coordinates of an object, knowing its ID
*/
function getBoundingRect(iid) {
    var bbox = document.getElementById(iid).getBoundingClientRect();
    return bbox;
}
'''

    y+= '''
/*
  Use this to place the thing: the height comes from the height of the
  menu bar, and the L-R positioning comes from the QC button itself.
*/
function openCommentForm(cfID, bid) {
    document.getElementById(cfID).style.display = "block";
    var bbox = getBoundingRect(bid);
    document.getElementById(cfID).style.left  = bbox.left; 
}
'''

    y+= '''
/*
  Just close the form button when done (mainly for ctrl+click)
*/
function closeCommentForm(cfID) {
    document.getElementById(cfID).style.display = "none";
}
'''

    y+= '''
/*
  Close *and* remove value (esc key, or clear+close button)
*/
function clearCommentForm(cid, cfID) {
    document.getElementById(cid).value = "";

    // get the btn1 ID from comm ID
    var bname = new String(cid); // basename
    // skip the 'comm_' part of button ID
    var bid   = "btn1_" + bname.slice(5);

    thisButtonGetsAComment(bid, null);

    closeCommentForm(cfID);
    doSaveAllInfo();
}
'''

    y+= '''
/*
   needed for when JSON file is read in, to give values from that to
   the text area field (as well as bt1n)
*/
function thisFormTextAreaGetsAComment(cid, comm) {
    document.getElementById(cid).value = comm;
}
'''

    y+= '''
/*
  "Saving" here means taking the comment (cid) and associating it with
  a button (bid), while also closing the comment form (cfID).  (enter
  key, or keep+close button)
*/
function keepFromCommentForm(cid, cfID) {

    // user's text
    var commtext = document.getElementById(cid).value;

    // get the btn1 ID from comm ID
    var bname = new String(cid); // basename
    // skip the 'comm_' part of button ID
    var bid   = "btn1_" + bname.slice(5);

    thisButtonGetsAComment(bid, commtext);
    closeCommentForm(cfID);
    doSaveAllInfo();
}
'''

    y+= '''
/*
  Same as keepFromCommentForm(...), but used when user is
  ctrl+clicking on btn1 to close comment.
*/
function keepFromCommentFormViaBtn1(bid, cfID) {

    // get the btn1 ID from comm ID
    var bname = new String(bid); // basename
    // skip the 'comm_' part of button ID
    var cid   = 'comm_' + bname.slice(5);

    // user's text
    var commtext = document.getElementById(cid).value;

    thisButtonGetsAComment(bid, commtext);
    closeCommentForm(cfID);
    doSaveAllInfo();
}
'''

    # ------------------- page scrolling ------------------------------

    y+= '''
/*
  THIS is now how we move on the page, so that there is no need to
  jump into the page, and hence tabbing through buttons is allowed.
*/
function moveToDiv( hr_sec ) {
    var sid = new String(hr_sec.id)
    var rect = getBoundingRect(sid);

    var scrtop =  this.scrollY;
    var newloc = rect.top + scrtop - nb_offset;
    window.scrollTo(0, newloc);

    //window.alert("earlier: " + rect.top + ", and now: " + this.scrollY);
}
'''


    # ------------------- saving into JSON obj ------------------------

    y+= '''
/*
  Submit values by element and col names
*/
function saveJsonValuesByNames(elename, colname, VAL) {
    cc = findCol(colname);
    rr = findQceleRow(elename);

    qcjson[rr][cc] = VAL;
}
'''

    y+= '''
/* 
  Submit values by row and col nums
*/
function saveJsonValuesByNums(rr, cc, VAL) {
    Ncol = qcjson[0].length;
    if ( cc >= Ncol ) {
      window.alert("**Error: Column [" + cc + "] not in JSON table!");
      throw "DONE";
    }

    var Nrow = qcjson.length;
    if ( i >= Nrow ) {
      window.alert("**Error: QC element [" + rr + "] not in JSON table!"); 
      throw "DONE!";
    }

    qcjson[rr][cc] = VAL;
}
'''

    y+= '''
// find row index of QC element in JSON table
function findQceleRow(elename) {
    var Nrow = qcjson.length;
    for( var i=1 ; i<Nrow ; i++ ) {
       if ( elename == qcjson[i][0] ) {
         break;
       }
    }

    if ( i >= Nrow ) {
      window.alert("**Error: QC element " + elename + " not in JSON table!"); 
      throw "DONE";
    }

    return i;
}
'''

    y+= '''
// find col index of item in JSON table
function findCol(colname) {
    Ncol = qcjson[0].length;
    for( var cc=1 ; cc<Ncol ; cc++ ) {
       if ( colname == qcjson[0][cc] ) {
         break;
       }
    }

    if ( cc >= Ncol ) {
       window.alert("**Error: Column " + colname + " not in JSON table!"); 
       throw "DONE";
    }

    return cc;
}
'''



    # At present, THIS is the form of the input json: just a list of
    # lists.  This will be convenient in order to remain ordered
    # (dictionaries are *not* ordered).  Column headings are still
    # included, for the moment.  At the moment, the column heading
    # names are hardcoded into the data I/O.

    '''
[
    ["qcele", "rating", "comment"],
    ["vepi", "good", "null"],
    ["ve2a", "bad",  "hello"],
    ["va2t", "null", "null"],
    ["vstat", "null", "null"],
    ["mot6", "null", "hello"],
    ["motE", "null", "null"],
    ["out", "null", "hello"],
    ["regps", "null", "null"],
    ["regcs", "null", "hello"],
    ["warns", "null", "null"],
    ["summ", "null", "hello"]
]
'''

    # ----------- SAVE FORM: update JSON (qcjson) and save to file
    # ----------- (hopefully)!

    # The Saver
    y+= '''
// the function that saves QC button ratings and comments to the *.json
function doSaveAllInfo() {
    updateLocalJson();

    // prepare to output all info needed for server
    pathParts = window.location.pathname.split('/')
    qcPath = pathParts.slice(1,-1)
    // rem is 'remainder', because this is the (full) remainder
    // of the path to the APQC JSON file from the end of the 
    // common abs path used to start the server
    remJsonFilename = qcPath.join('/') + '/' + jsonfile
    remJson_ssrev   = qcPath.join('/') + '/' + json_ssrev

    dataToPost = {
        'remJson_ssrev': remJson_ssrev,
        'remJsonFilename': remJsonFilename,
        'JsonFileContents': qcjson,
    }

    postJSON(dataToPost);
} 

/* Function that is called by AV button to run AFNI script
   the script should be the name of the script. It basically 
   provides the script name and the 'remainder' (or 'tail') part
   of a filepath that is used in open_apqc.py to run the script.
*/
async function doRunAV(script) {
  await updateServerStatus();
  if ( is_served ) {  
    // prepare to output all info needed for server
    pathParts = window.location.pathname.split('/')
    qcPath = pathParts.slice(1,-1)

    // rem is 'remainder', because this is the (full) remainder
    // of the path to the APQC JSON file from the end of the 
    // common abs path used to start the server
    remJsonFilename = qcPath.join('/') + '/' + jsonfile

    dataToPost = {
        'remJsonFilename': remJsonFilename,
        'script': script,
    }

    postJSON_AV(dataToPost);
  }
} 

'''

    y+= '''
// The Quitter
function doQuit() {
    updateLocalJson();
}

'''

    y+= '''
// The Helper
async function doShowHelp() {
  let is_ok = await checkPageStatus();
  if ( is_ok ) {
    window.open('help.html', '_blank');
  } else {
    await updateServerStatus();
  }
} 

// Link to mtedana QC page, if present
async function doShowMtedana(link) {
  let is_ok = await checkPageStatus();
  if ( is_ok ) {
    window.open(link, '_blank');
  } else {
    await updateServerStatus();
  }
} 


'''

    # Step 1 of saving the dataset: push button vals to JSON
    y+= '''
function updateLocalJson() {
    var Nele = qcjson.length;
    for( var i=1; i<Nele; i++ ) {

        var qcele = qcjson[i][0];

        var jj    = i - 1;  // offset because of col heading in JSON
        var bid   = new String(allBtn1[jj].id); 

        var rattext = translateBtn1TextToJsonRating(document.getElementById(bid).textContent);
        saveJsonValuesByNames(qcele, "rating", rattext);

        // save the comment part
        var commtext = document.getElementById(bid).dataset.txtcomm;
        saveJsonValuesByNames(qcele, "comment", commtext); 
    }
}
'''

    ### This is NO LONGER used, so not including at the moment
    # Step 2 of saving the dataset: write to file
    '''
function saveDownloadJsonfile(text, filename){
    var a = document.createElement('a');
    a.setAttribute('href', 'data:text/plain;charset=utf-u,'+encodeURIComponent(text));
    a.setAttribute('download', filename);
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
}
'''

    y+= '''
function translateBtn1TextToJsonRating( tt ) {
    if ( tt.includes("+") ) {
       return "good";
    } else if ( tt.includes("X") ) {
       return "bad";
    } else if ( tt.includes("?") ) {
       return "other";
    } else if ( tt == "null" || tt == "" ) {
       return "null";
    } else {
      window.alert("**Error: unallowed text/rating in button:" + tt);
      throw "DONE";
    }
}
'''

    y+= '''
</script>
'''

    return y

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------

def wrap_page_title( xtitle, xsubj, xstudy='',
                     vpad=0, addclass="", blockid='', padmarg=0 ):


    txt_study = ''
    if xstudy :
        txt_study+= '<pre><h3>task: {study}</h3></pre>'.format( study=xstudy )

    # start the first div on the page
    y = '''<!-- start of title block div -->
<div class="div_pad_class">'''

    # the boundary line: location+ID necessary for highlighting page
    # location
    y+= '''
  <hr class="hr_sec" id="hr_{blockid}"/>
  <div id="{blockid}" '''.format(blockid=blockid)

    # this line offsets the anchor location for the navigation bar to
    # head to: the values here should be equal to the height of the
    # navigation bar (plus the line beneath it).
    y+= ''' 
       style="padding-top: {0}px; margin-top: -{0}px;">'''.format(padmarg)

    y+= '''
    <h1><center> {title} <center></h1>
  </div>

  <!-- top of subj/title info -->
  <div style="text-align: center;">
    <div style="display: inline-block; text-align: left;">
      <pre><h2>subj: {subj}</h2></pre>
      {txt_study}
    </div>
  </div> <!-- bot of subj/title info -->
</div> <!-- end of title block div -->
'''.format( title=xtitle, subj=xsubj, txt_study=txt_study )

    if vpad:
        y = """\n"""+y
        y+="""\n"""

    return y


# -------------------------------------------------------------------

def wrap_block_title(x, vpad=0, addclass="", blockid='', padmarg=0,
                     do_close_prev_div=True):

    y = ''
    if do_close_prev_div :
        # close the previous section div (at least the title will have one)
        y+= '''</div> <!-- end of block div -->
'''

    # start the new section div
    y+= '''
<!-- start of div for QC block: '{blockid}' -->
<div class="div_pad_class">
'''.format(blockid=blockid)

    # the boundary line: location+ID necessary for highlighting page
    # location
    y+= '''
<hr class="hr_sec" ''' 
    if blockid :
        y+= ''' id="hr_{}" '''.format(blockid)
    y+= '''/>\n''' 

    # the title.  NB: the spacing in this section *matters*, so don't
    # worry about trying to make it readable within 80 chars.
    y+= '''<div '''
    if blockid :
        y+= ''' id="{}" '''.format(blockid)
    # this line offsets the anchor location for the navigation bar to
    # head to: the values here should be equal to the height of the
    # navigation bar (plus the line beneath it).
    y+= ''' style="padding-top: {0}px; margin-top: -{0}px;">
'''.format(padmarg)
    y+= '''  <pre {}>'''.format(addclass)
    y+= '''<center>[''' + blockid + ''']<b>'''
    y+= ''' <u>'''+x+'''</u>'''
    y+= ' '*(len(blockid)+3)       # balance blockid text
    y+= '''</b></center></pre>
</div>'''
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_block_text( x, vpad=0, addclass="", dobold=True, itemid='',
                     padmarg=0 ):
    addid = ''
    if itemid :
        addid = '''id="{}"'''.format( itemid )

    y = '''<!-- top of text {addid} -->
<div {addid} 
     style="padding-top: {padmarg}px; margin-top: -{padmarg}px;"
     {addclass}>
'''.format( addid=addid, padmarg=padmarg, addclass=addclass )
    if dobold :
#        y+= '''  <pre><b>'''+x+'''</b></pre>
#</div> <!-- bot of text -->'''
        y+= '''  <pre>'''+x+'''</pre>
</div> <!-- bot of text -->'''
    else:
        y+= '''  <pre>'''+x+'''</pre>
</div> <!-- bot of text -->'''
    if vpad:
        y= '''\n'''+y
        y+='''\n'''
    return y

# -------------------------------------------------------------------

def wrap_img(x, wid=500, itemid='', vpad=0, addclass="", 
             add_nvbtn=False,
             av_file='', ic_file='', ic_args='', gv_file='', gv_args='' ):
    # [PT: Nov 20, 2018] needed this next line to center the text, and
    # needed "display: inline-block" in the img {} def to not have
    # whole line be a link.
    # [PT: Mar 15, 2023] add in items for NiiVue button
    # [PT: June 5, 2023] add in items for InstaCorr (IC) button, which only
    #                    exists in add_nvbtn is True; ic_args are optional
    #                    seed location coords (3 numbers)
    # [PT: June 24, 2023] add in items for GraphView (GV) buttons, which are
    #                    used whenever IC ones are

    y = ''
    y+= vpad*'\n'

    y+= '''<!-- top of image: {itemid} ({img}) -->
<div style="text-align: center; position: relative;">
    <a href="{img}">
    <img src="{img}" 
         alt="{img}" {addclass} 
         style="display: inline-block; text-align: center;">
    </a>'''.format( img=x, addclass=addclass, itemid=itemid )
    
    if add_nvbtn :
        y+= '''
    <!-- top AFNI-view and NiiVue buttons -->
    <div class="container_avnv">
      <td style="white-space:nowrap;" id=td6_NV_{itemid}>
      <button class="button-generic button-RHS btn6"
              title="Run NiiVue" 
            onclick="toggle_niivue(is_served,'nvcan_{itemid}_container')">NV</button>
      </td>
      <td style="white-space:nowrap;" id=td6_AV_{itemid}>
      <button class="button-generic button-RHS btn6"
              title="Run AFNI-view" 
              onclick="doRunAV('{av_file}')">AV</button>
      </td>
    </div> <!-- bot AV/NV buttons -->'''.format( itemid=itemid, 
                                                 av_file=av_file )

    if ic_file or gv_file :
        y+= '''
    <!-- top InstaCorr and Graph-View buttons (IC/GV) -->
    <div class="container_icgv">'''

        if ic_file :
            y+= '''
      <td style="white-space:nowrap;" id=td6_IC_{itemid}>
      <button class="button-generic button-RHS btn6b"
              title="Run InstaCorr" 
              onclick="doRunAV('{ic_file} {ic_args}')">IC</button>
      </td>'''.format( itemid=itemid, ic_file=ic_file, ic_args=ic_args )

        if gv_file :
            y+= '''
      <td style="white-space:nowrap;" id=td6_GV_{itemid}>
      <button class="button-generic button-RHS btn6b"
              title="Run AFNI graph-view" 
              onclick="doRunAV('{gv_file} {gv_args}')">GV</button>
      </td>'''.format( itemid=itemid, gv_file=gv_file, gv_args=gv_args )

        y+= '''
    </div> <!-- bot IC/GV buttons -->'''

    y+= '''
</div> <!-- bottom of image -->
'''
    y+= vpad*'\n'

    return y

# -------------------------------------------------------------------

def wrap_nv_html(fname):
    """Basically just echo the contents of the HTML file in 

"""
    
    y = '''
'''

    if not(os.path.isfile(fname)) :
        print("+* WARN: cannot open file", fname)
    else:
        fff = open(fname, 'r')
        X = fff.readlines()
        fff.close()

        y+= ''.join(X)

        y+= '''
'''

    return y

# -------------------------------------------------------------------

# string literal
def wrap_dat(x, wid=500, vpad=0, addclass="", warn_level = "",
             remove_top_empty=False):

    # some formatting: get rid of unnecessary top empty lines
    if remove_top_empty:
        newx = x.split("\n")
        if not(newx[0].strip()):
            newx = '\n'.join(newx[1:])
        else:
            newx = '\n'.join(newx)
    else:
        newx = str(x)

    top_line = ''
    if warn_level:
        top_line+= '''<p class="{}">{}</p>'''.format(
            'wcol_'+warn_level, warn_level )
    
    y = ''
    y+= vpad*'\n'
#    y+= '''<div> 
#    <pre {} ><left><b>{}{}</b></left></pre>
#</div>'''.format(addclass, top_line, newx)
    y+= '''<div> 
    <pre {} ><left>{}{}</left></pre>
</div>'''.format(addclass, top_line, newx)
    y+= vpad*'\n'

    return y

# -------------------------------------------------------------------

# item is a button (with a link)
def wrap_button(x, vpad=0, button_type=""):

    # current format of text is:
    #     TEXT: ...
    #     LINK: ...
    #
    #     TEXT: ...
    #     LINK: ...
    # etc.

    text_list = x.split("\n")
    nlines = len(text_list)

    # got through text and get text+link pairs
    list_buttons = []
    i = 0
    while i < nlines:
        ttt = text_list[i].strip()
        if ttt.startswith('TEXT:') :
            text = ttt[5:].strip()
            lll = text_list[i+1].strip()
            if lll.startswith('LINK:') :
                link = lll[5:].strip()
            else:
                print("** ERROR in looking for link in button text (i={})"
                      "".format(i))

            list_buttons.append([text, link])
        elif ttt == '':
            i = nlines+1
        else:
            print("** ERROR in looking for text in button text (i={})"
                  "".format(i))
        i+= 3

    nbutton = len(list_buttons)

    y = ''

    for n in range(nbutton):
        text = list_buttons[n][0]
        link = list_buttons[n][1]

        #print("DEBUG: {}, {} -- {}...{}".format(n, text, link, button_type))
        if button_type == 'mtedana' :
            button_name = 'btn_' + button_type
            onclick = '''onclick="doShowMtedana('{link}')"'''.format(link=link)
            title   = 'title="Click to open TEDANA HTML."'
        # ... and can add in more button types, as they arise
        y+= vpad*'\n'
        y+= '''
        <td style="width: 1800px; white-space:nowrap;" id=asdf>
            <center><button class="button-generic {button_name}" 
                    {title}
                    {onclick}>{text}</button></center>
        </td>'''.format( text=text, onclick=onclick, title=title,
                         button_name=button_name )
        y+= vpad*'\n'

    return y

# -------------------------------------------------------------------

def read_descrip_json(x):
    '''Take the input json file 'x' and return an instance of the
apqc_item_info() class.
'''

    ddd   = read_json_to_dict(x) # get json as dictionary
    ainfo = apqc_item_info()     # initialize obj to hold info
    ainfo.set_all_from_dict(ddd) # set everything in this obj that we can

    return ainfo

# -------------------------------------------------------------------

def read_title_json(x):
    '''Take the input json file 'x' and return an instance of the
apqc_title_info() class.
'''
    
    ddd   = read_json_to_dict(x) # get json as dictionary
    tinfo = apqc_title_info()    # initialize obj to hold title info
    tinfo.set_all_from_dict(ddd) # set everything in this obj that we can

    return tinfo

# ----------------------------------------------------------------------

def read_dat(x, do_join=True):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    if do_join :
        out = ''.join(txt)
        return out
    else:
        return txt

# ----------------------------------------------------------------------

# check if json exists- return full or null dict
def read_json_to_dict(x):

    if os.path.isfile(x):
        with open(x, 'r') as fff:
            xdict = json.load(fff)
    else:
        xdict = {}

    return xdict

# ----------------------------------------------------------------------

def make_pbar_line(d, imgpbar, vpad=0, addclassdiv="", addclassimg="",
                   dobold=True):

    y = '''<div {} ><pre>'''.format(addclassdiv)
    if dobold :
        y+= """<b>"""

    # [PT: Jan 2, 2019] Typically, the pbar/cbar is for an olay, hence
    # the default; in some cases, we might want flexibility here,
    # though.
    voltype = "olay" 
    if 'pbar_vol' in d :
        voltype = d['pbar_vol']

    y+= """{}: {} """.format(voltype, d['pbar_bot'])

    y+= '''<img {} '''.format(addclassimg)
    y+= '''style="display: inline; margin: -5 -5px;" '''
    y+= '''src="{}" > '''.format(imgpbar)
    y+= '''{} ({})'''.format(d['pbar_top'], d['pbar_comm'])

    if 'vthr' in d :
        y+= '''\nthr : {}'''.format(d['vthr'])
        if 'vthr_reason' in d :
            y+= ''' ({})'''.format(d['vthr_comm'])

    # [PT: Jan 2, 2019] can add in comments, too
    if 'pbar_comm' in d :
        if type(d['pbar_comm']) == list :
            for x in d['pbar_comm']:
                y+= '''\n{}'''.format(x)
        else : 
            # assume it is unicode (esp. in py2) or str (likely in
            # py3)
            y+= '''\n{}'''.format(d['pbar_comm'])

    if dobold :
        y+= '''</b>'''
    y+= '''</pre></div>\n'''

    if vpad:
        y= """\n"""+y
        y+="""\n"""

    return y

# ----------------------------------------------------------------------

def read_pbar_range(x, dtype="NA"):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    Nlines = len(txt)
    if not(Nlines):
        sys.exit("** ERROR: no lines of text in {}?".format(x))
    elif Nlines > 1:
        sys.exit("** ERROR: too many lines (={}) in {}?".format(Nlines, x))

    l0 = txt[0]
    y  = l0.split()

    Nnums = len(y) 
    if Nnums != 3:
        sys.exit("** ERROR: wrong number of nums (={}) in {}?".format(Nnums, x))

    out = []
    if dtype == int :
        for nn in y:
            z = int(nn)
            out.append(z)
    elif dtype == float :
        for nn in y:
            z = float(nn)
            out.append(z)
    else: # the NA or other cases...
        for nn in y:
            if nn.__contains__('.'):
                z = float(nn)
            else:
                z = int(nn)
            out.append(z)

    return out[0], out[1], out[2]

# ----------------------------------------------------------------------
