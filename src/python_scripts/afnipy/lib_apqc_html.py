#
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
ver = '2.61' ; date = 'Mar 10, 2022' 
# [PT] bug fix in m_tedana button creation
# + fix oversight in wrap_button(), where different buttons pointed to
#   only one tedana directory.  Thanks, Dan H for noting this!
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
qc_title["Top"]    = [ "Top of page for:&#10  ${subj}", 
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

(EPI should now be shown;  anat vol will be along shortly.)
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
estimates).  These images are only created for task data sets, i.e.,
where GLTs or stimuli are specified (so not for resting state data).

By default, the (full) F-stat of an overall regression model is shown.
Additionally, one can specify labels of stimuli or GLTs used in the
afni_proc.py command, and statistical results will be shown.  For
stimuli with effect estimates, the 'Coef' vales will be displayed as
the olay colors (preferably with the 'scale' block having been used in
afni_proc.py, producing meaningful units of BOLD % signal change in
the 'Coef' volumes).

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
 '''      *** afni_proc.py's single subject QC report form ***

For questions about afni_proc.py (AP), please see the program or
<urlin><a href="https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/afni_proc.py_sphx.html" target="_blank">webpage help</a></urlin>.
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

Saving
    Clicking SAVE will let the user save the QC ratings+comments on
    their computer for later use, such as inclusion/exclusion criteria
    for the subject in group analysis.  (NB: This action is treated
    the same as downloading a file from online, and is subject to
    standard limitations on simplicity due to browser security
    settings.)

See also
    There is an online <urlin><a href="https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/tutorials/apqc_html/main_toc.html" target="_blank">web tutorial</a></urlin>. It's more verbose and pictorial, 
    if that's useful.
'''
],
['''DEFINITIONS''', 
'''QC block 
    One thematic section of the QC form (original data, alignment
    step, etc.).  Each block has a label in the navigation bar (vorig,
    ve2a, etc.)-- click on the label to jump to that block. Click on
    the button below the label to provide a rating for that block.

QC button
    Below each QC block label in the navigation bar is a button
    (initially empty after running afni_proc.py). The user can click
    on it to toggle its state to one of three ratings (good, +; bad,
    X; other, ?), as well as to enter a comment for that QC block (via
    ctrl+click).

'FINAL' 
    Label for the QC button to hold the user's overall/final
    evaluation of the subject's data and processing. (Clicking on this
    label does nothing.)

filler button 
    |A+|, |Ax|, |A?|-- located in the upper right corner of the
    navigation menu.  These can be used to provide uniform ratings
    across the QC buttons (click to fill empty buttons, double-click
    to fill *all* buttons, regardless of state).  
    |clr|-- double-clicking on this will empty all ratings+comments
    from the QC buttons.

'SAVE'
    Write the ratings+comments to disk.  This action is done through
    the browser, and is subject to the browser settings; probably
    users should not have the browser set to automatically download
    all files to the same location, for example.

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
Use ctrl+click on a QC button to open (or close) a comment window.  

Save a comment with the green (left) button, or hit Enter at any point.
Remove a comment with the pink (right) button, or hit Esc at any point.

Any QC button with a comment gets a pair of quotes added, like ''+''.
Comments are independent of rating, but adding a comment to an empty
button changes its rating to ''?'' (which can be altered further from
there).
'''],
['''SAVE FORM''',
'''Click on the 'SAVE' button.  

Unfortunately, the file will not be directly saved by this, due to
security settings on most web-browsers, and the user will be prompted
to save the file as if downloading from the Web.'''
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

brate_hover = '''QC BUTTON FORM

NAVIGATE
Click a label ('Top', etc.) to jump to a section.

RATE + COMMENT
Click the QC button below it to record your rating, toggling through:
    X  :  bad,
    ?  :  other/revisit,
    +  :  good.
Use ctrl+click on a QC button to provide a comment.  Close the comment 
panel with ctrl+click or its buttons.

SPEEDIFY
There are 'filler buttons' for each rating: |A+|, |Ax|, |A?|.
Click once to fill all *empty* buttons with that rating, or
double click to fill *all* buttons with that rating.

--- click 'HELP' at the right for more details and features ---'''

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

bsave_hover = '''Save ratings+comments.
Click once (or Enter) to write QC form to disk.
'''

bhelp_hover = '''Open help page.
Click once (or Enter) to open new help page.
'''



def write_help_html_file( ofile, ocss ):

    # -------------- start ---------------------

    ht  = '''
    <html>
    '''

    # -------------- head ---------------------

    # use same style sheet as main pages
    ht+= '''
    <head>
    <link rel="stylesheet" type="text/css" href="{}" />
    '''.format( ocss )

    ht+= '''
    </head>
    '''

    # -------------- body ---------------------

    ht+= '''
    <body>
    '''

    Nsec = len(apqc_help)

    for ii in range(Nsec):
        x = apqc_help[ii]

        ht+= wrap_block_title( x[0],
                               vpad=1,
                               addclass=" class='padtop' ",
                               blockid='' )

        ht+= wrap_block_text( x[1],
                              addclass=" class='container' " )



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
    # that doesn't have a QC button with it (it's just the 'Top' of
    # the page).
    for i in range(1,len(ll)):
        x = ll[i]
        olist.append( [x[0], "", ""] )
    
    # output with indentation
    ojson = json.dumps( olist, indent=4 )
    fff = open( fname, "w" )
    fff.write( ojson )
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

def make_nav_table(llinks, max_wlevel=''):
    # table form, not ul 
    N = len(llinks)
    idx = 0

    # =======================================================================
    # dummy, background nav

    y = '''
    <div class="navbar">
      <table style="width: 100%">

        <tr>
          <td style="width: 100%">
            <a style="text-align: left"> {0} </a>
          </td>
        </tr>

        <tr>
          <td style="width: 100%">
            <button class="button-generic button-LHS btn0" onclick="">
            {0} </button>
          </td>
        </tr>

      </table>
    </div>
    '''.format( NULL_BTN0 ) 

    # =======================================================================
    # real, foreground nav

    y+= '''\n<div class="navbar">\n'''

    # -----------------------------------------------------
    # L-floating part: section anchors and rating buttons
    # NB: these are fixed width

    ## note about keycodes on internet explorer, might have to do
    ## something like this for each one:
    # https://stackoverflow.com/questions/1750223/javascript-keycode-values-are-undefined-in-internet-explorer-8

    for i in range(0, N):
        ll, hov = llinks[i][0], llinks[i][1]
        #print(ll)

        color_change = ''
        
        # Put lines around "FINAL" element
        if i<N-1 : 
            finaltab = ''
            if ll == 'warns' and max_wlevel :
                wcol = lahc.wlevel_colors[max_wlevel]
                if lahc.wlevel_ranks[max_wlevel] > lahc.wlevel_ranks['mild'] :
                    finaltab = '''style="color: {}; '''.format("#000") 
                    finaltab+= '''background-color: {};" '''.format(wcol)
                else:
                    finaltab = '''style="color: {};" '''.format(wcol) 

        else:
            finaltab = '''style="background-color: #ccc; color: #000;" '''

        # new table
        y+= '''<table style="float: left">\n'''

        # TOP ROW (blockid)
        y+= '''
        <tr>
          <td class="td1" id=td1_{0}>
            <button class="button-generic button-LHS btn5" id="btn5_{0}" 
            onmousedown="moveToDiv(hr_{0})" title="{1}" {2} 
            onkeypress="if ( event.keyCode == 13 ) {{ moveToDiv(hr_{0}); }}">
            {0}</button>
          </td>
        </tr>
        '''.format( ll, hov, finaltab ) 

        # BOT ROW (QC button)
        y+= '''<td >''' # set boundary between QC buttons here
        if i :
            # NB: with button clicks, if using onkeypress with
            # onclick, the former *also* drives the latter a second
            # time, so get annoying behavior; hence, distinguish those
            y+= '''
              <button class="button-generic button-LHS btn1" id="btn1_{0}" data-txtcomm="" 
              onmousedown="btn1Clicked(event, this)" 
              onkeypress="if ( event.keyCode == 13 ) {{ btn1Clicked(event, this); }}" 
              {1}</button>
            </td>
            '''.format( ll, NULL_BTN1 )
        else:
            y+= '''
              <button class="button-generic button-LHS btn0" id="btn0_{0}" 
              onclick="" 
              title="{1}">
              {2}</button></td>
            '''.format( ll, brate_hover, "FORM:" ) 
        y+= '''</tr>\n'''
        y+= '''</table>'''

        if i :
            # ~dropdown form button
            ## NB: the onkeydown stuff makes it that hitting "Enter"
            ## (event.keyCode == 10 || event.keyCode == 13) inside the
            ## text field is like submitting the text (and the
            ## .preventDefault() means that it does NOT input a
            ## newline):
            ## https://stackoverflow.com/questions/155188/trigger-a-button-click-with-javascript-on-the-enter-key-in-a-text-box
            ## https://stackoverflow.com/questions/26975349/textarea-wont-stop-making-new-line-when-enter-is-pressed
            ## ... and hitting "Esc" (event.keyCode == 27) is like
            ## canceling.
            y+= '''
            <div class="form-popup" id="cform_{0}" > 
                <form class="form-container" onsubmit="return false;"> 
                <textarea type="text" placeholder="Enter comment" 
                rows="4" cols="40" id="comm_{0}" 
                onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ 
                   event.preventDefault(); keepFromCommentForm(comm_{0}.id, cform_{0}.id);}} 
                   else if (event.keyCode == 27) {{ 
                       clearCommentForm(comm_{0}.id, cform_{0}.id); }}">
                </textarea>  
                <button type="button" class="btn" 
                onclick="keepFromCommentForm(comm_{0}.id, cform_{0}.id)">keep+close</button> 
                <button type="button" class="btn cancel" 
                onclick="clearCommentForm(comm_{0}.id, cform_{0}.id)">clear+close</button> 
                </form> 
            </div> 
            '''.format( ll )

    # ------------------------------------------------------ 
    # R-floating part: subj ID and SAVE button 
    # NB: this is flexible width
    bsave  = 'SAVE'
    bhelp  = 'HELP'
    bgood  = 'A+'  ; bgood_ind  =  1 
    bbad   = 'Ax'  ; bbad_ind   =  2 
    bother = 'A?'  ; bother_ind =  0 
    bclear = 'clr' ; bclear_ind = -1

    # Start right-side table
    y+= '''<table style="float: right; margin-right: 2px">\n'''

    # ROW: "all fill" buttons-- click (or Enter) fills empty QC buttons,
    # dblclick (or ctrl+Enter) fills ALL QC buttons
    y+= '''<tr>\n'''
    y+= '''<td style="width: 180px; white-space:nowrap;">\n'''

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onmousedown="allYourBaseAreBelongToUs({2})" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{
reallyAllYourBaseAreBelongToUs({2}); }} else {{ allYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'good', bgood_hover, bgood_ind, bgood )

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onmousedown="allYourBaseAreBelongToUs({2})" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{ 
reallyAllYourBaseAreBelongToUs({2}); }} else {{ allYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'bad', bbad_hover, bbad_ind, bbad )

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onmousedown="allYourBaseAreBelongToUs({2})" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{ 
reallyAllYourBaseAreBelongToUs({2}); }} else {{ allYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'other', bother_hover, bother_ind, bother )

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{ 
reallyAllYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'clear', bclear_hover, bclear_ind, bclear )

    y+= '''</td>\n'''
    y+= '''</tr>\n'''

    # ROW:  hyperlinks (anchors) within the page
    y+= '''<tr>\n'''
    y+= '''<td style="width: 180px; white-space:nowrap;" id=td3_{}>'''.format( bsave )

    y+= '''<button class="button-generic button-RHS btn3save" title="{}" '''.format( bsave_hover ) 
    y+= '''onclick="doSaveAllInfo()">'''
    y+= '''{}</button>\n'''.format( bsave )

    y+= '''<button class="button-generic button-RHS btn3help" title="{}" '''.format( bhelp_hover ) 
    y+= '''onclick="doShowHelp()">'''
#    y+= '''href="help.html" target="_blank">'''
#    y+= '''onclick="location.href='help.html';">'''
    y+= '''{}</button>\n'''.format( bhelp )

    y+= '''</td>\n'''
    y+= '''</tr>\n'''

    # End right-side table
    y+= '''</table>'''
    y+= '''</div>'''

    return y

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------

# JAVASCRIPT script functions and variables

def make_javascript_btn_func(subj ):

    y = ''

    y+= '''<script type="text/javascript">\n'''

    y+= '''

// global vars
var allBtn1, allTd1, allhr_sec;  // selection/location IDs
var topi, previ;                 // keeps our current/last location
var jsonfile = "apqc_{0}.json";  // json file: apqc_SUBJ.json
var qcjson;                      // I/O json of QC button responses
var nb_offset = 66-1;            // navbar height: needed for scroll/jump

// properties of QC buttons that get toggled through
const bkgds   = [ "#fff" , "#67a9cf", "#d7191c"  ];
const valeurs = [ "?"    , "+"      , "X"        ];
const tcols   = [ "#777" , "#FFF"   , "#000"     ];

'''.format( subj )

    # --------------- load/reload initialize -----------------

    # gets run at loadtime
    y+= '''
//window.onload = function() {
function RunAtStart() {

    // initialize arrays and location
    initializeParams();

    // read in JSON
    loadJSON(jsonfile, function(unparsed_json) {
      // give the global variable values
      window.qcjson = JSON.parse(unparsed_json);
    });

    CheckJsonfileMatchesQcbuttons();

    ApplyJsonfileToQcbuttons();
};

'''

    # OFF AT HTE MOMENT, but a guard for reloading page
    y+= '''
    window.onbeforeunload = function(event)
    {
        return confirm();
    };
'''

    y+= '''
// This function gets run when page loads ("onload").
function initializeParams() {
    allBtn1   = document.getElementsByClassName("btn1");   // btn1_vepi, btn1_*
    allTd1    = document.getElementsByClassName("td1");    // td1_vepi,  td1_*
    allhr_sec = document.getElementsByClassName("hr_sec"); // hr_vepi,   hr_*

    topi      = findTopSectionIdx()       // idx of current loc
    previ     = topi;                     // init "previous" idx
    setTd1Border(topi, "#ffea00"); //"yellow"); // show location
}
'''

    # read in JSON file, from:
    # https://codepen.io/KryptoniteDove/post/load-json-file-locally-using-pure-javascript
    # https://stackoverflow.com/questions/7346563/loading-local-json-file
    # importantly, the xobjs.open(...) func NEEDS to have the 'false'
    # set to read synchronously, which is necessary for the JSON to
    # load fully before use-- slower, but this is a small file
    y+= '''
function loadJSON(ifile, callback) {   
  var xobj = new XMLHttpRequest();
  xobj.overrideMimeType("application/json");
  xobj.open('GET', ifile, false);  // need 'false' for synchrony!
  xobj.onreadystatechange = function () {
    if (xobj.readyState == 4 && xobj.status == "200") { !!!!
      callback(xobj.responseText);
    }
  };
  xobj.send(null);  
}
'''

    # Both the QC element names AND their order need to match
    y+= '''
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

    # Because order matches (offset by 1), we can just apply directly
    # with the counting index, based on the allBtn1 list.
    y+= '''
// This function gets run when page loads ("onload").
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

    # This function gets run when page loads ("onload").  'ss' is the
    # JSON rating, and 'bid' is the button ID in AllBtn1.
    y+= '''
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

    # When the JSON is read in, get comments and give any text to both
    # the btn1 and associated comment form textarea
    y+= '''
function sendCommentToButtonAndForm(comm, bid) {
    thisButtonGetsAComment(bid, comm);

    var bname = new String(bid); // basename
    var cid   = 'comm_' + bname.slice(5);  // skip the 'btn1_' part of button ID

    // because of how "null" is read in; this just matters in form
    if ( comm == "null" ) {
        var comm = "";
    }
    thisFormTextAreaGetsAComment(cid, comm);
}
'''

    # --------------- scroll location in page stuff -----------------

    # Checks/rechecks whenever change in page location occurs.
    y+= '''
window.addEventListener("scroll", function(event) {

    var newi = findTopSectionIdx();

    if ( newi != topi ) {
        setTd1Border(newi, "#ffea00"); /* "yellow ");*/
        setTd1Border(topi,  "inherit");    /*  ; //"#FFF", "#444"); */
        previ = topi;
        topi  = newi;
    }
}, false);
'''

   # Just go through (short) list from top, and first one that has pos
   # coor, is the one at top
    y+= '''
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
    
    # click on the QC buttons will scroll through the color values
    ## ctrl+click on the QC buttons will toggle between the comment
    ## form being open or closed (saving what is in form when closing).
    y+= '''
function btn1Clicked(event, button) {
    if (event.ctrlKey) {
       btn1ClickedWithCtrl(event, button);
    }  else {
       changeColor(button); //alert("The CTRL key was NOT pressed!");
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
function changeColor(button) {
  newidx = Number(button.dataset.idx || 0);    // idx=0 on first click
  newidx = (newidx + 1) % bkgds.length;        // calc new idx, mod Ncol
  button.dataset.idx       = newidx;            // store new idx in ele
  button.style.color       = tcols[newidx];     // set color
  button.style.background  = bkgds[newidx];     // set bkgd
  button.style.borderColor = bkgds[newidx];     // set bkgd
  button.textContent       = valeurs[newidx];
  checkIfButtonCommented( button );            // set text
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
function setThisButtonRating(bid, idx) {{
    // normal values
    if ( idx >= 0 ) {{
      document.getElementById(bid).textContent      = valeurs[idx];
      document.getElementById(bid).style.background = bkgds[idx];
      document.getElementById(bid).style.borderColor = bkgds[idx];
      document.getElementById(bid).style.color      = tcols[idx];
      document.getElementById(bid).dataset.idx      = idx;
      checkIfButtonCommented( document.getElementById(bid) );
    
    }} else {{
    // the reset, for "null" JSON
      document.getElementById(bid).textContent      = "{}";
      document.getElementById(bid).style.background = ''; // reset to CSS
      document.getElementById(bid).style.borderColor = ''; // reset to CSS
      document.getElementById(bid).style.color      = ''; // reset to CSS
      document.getElementById(bid).dataset.idx      = 0; //null;
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

    # two arguments: the button ID 'bid' from an element of AllBt1n,
    # and the 'comment' that gets added/overwritten (in the newly
    # created element, txtcomm).  Basically used to put the form
    # comments into the button fields, and then later into jsons.
    y+= '''
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

    # "ALL OTHER" fill button, here to set every btn1-button value to
    # "+" or "x", depending on input arg 'ii' (index in list)
    y+= '''
function allYourBaseAreBelongToUs(ii) {{ 
   for( var i=0; i<allBtn1.length; i++ ) {{ 
     var bid = allBtn1[i].id; 
     var ival = document.getElementById(bid).textContent; 
     if ( ival == "{}" ) {{ 
       setThisButtonRating(bid, ii); 
     }}
   }} 
}}
'''.format( NULL_BTN1 )

    # "ALL-ALL" fill button: regardless of initial state set every
    # btn1-button value to "+" or "x", depending on input arg 'ii'
    # (index in list); that is, this overruns earlier button values
    y+= '''
function reallyAllYourBaseAreBelongToUs(ii) { 
   for( var i=0; i<allBtn1.length; i++ ) { 
     var bid = allBtn1[i].id; 
     var ival = document.getElementById(bid).textContent; 
     setThisButtonRating(bid, ii);
     if ( ii < 0 ) {
        sendCommentToButtonAndForm("", bid);
     }
   } 
} 
'''

    # ------------------- commentize form ------------------------
    
    # Get position coordinates of an object, knowing its ID
    y+= '''
function getBoundingRect(iid) {
    var bbox = document.getElementById(iid).getBoundingClientRect();
    return bbox;
}
'''

    # Use this to place the thing: the height comes from the height of
    # the menu bar, and the L-R positioning comes from the QC button
    # itself.
    y+= '''
function openCommentForm(cfID, bid) {
    document.getElementById(cfID).style.display = "block";
    var bbox = getBoundingRect(bid);
    document.getElementById(cfID).style.left  = bbox.left; 
}
'''

    # just close the form button when done (mainly for ctrl+click)
    y+= '''
function closeCommentForm(cfID) {
    document.getElementById(cfID).style.display = "none";
}
'''

    # close *and* remove value (esc key, or clear+close button)
    y+= '''
function clearCommentForm(cid, cfID) {
    document.getElementById(cid).value = "";

    // get the btn1 ID from comm ID
    var bname = new String(cid); // basename
    var bid   = "btn1_" + bname.slice(5);  // skip the 'comm_' part of button ID

    thisButtonGetsAComment(bid, null);

    closeCommentForm(cfID);
}
'''

    # needed for when JSON file is read in, to give values from that
    # to the text area field (as well as bt1n)
    y+= '''
function thisFormTextAreaGetsAComment(cid, comm) {
    document.getElementById(cid).value = comm;
}
'''

    # "Saving" here means taking the comment (cid) and associating it
    # with a button (bid), while also closing the comment form (cfID).
    # (enter key, or keep+close button)
    y+= '''
function keepFromCommentForm(cid, cfID) {

    // user's text
    var commtext = document.getElementById(cid).value;

    // get the btn1 ID from comm ID
    var bname = new String(cid); // basename
    var bid   = "btn1_" + bname.slice(5);  // skip the 'comm_' part of button ID

    thisButtonGetsAComment(bid, commtext);
    closeCommentForm(cfID);
}
'''

    # Same as keepFromCommentForm(...), but used when user is
    # ctrl+clicking on btn1 to close comment
    y+= '''
function keepFromCommentFormViaBtn1(bid, cfID) {

    // get the btn1 ID from comm ID
    var bname = new String(bid); // basename
    var cid   = 'comm_' + bname.slice(5);  // skip the 'comm_' part of button ID

    // user's text
    var commtext = document.getElementById(cid).value;

    thisButtonGetsAComment(bid, commtext);
    closeCommentForm(cfID);
}
'''

    # ------------------- page scrolling ------------------------------

    # THIS is now how we move on the page, so that there is no need to
    # jump into the page, and hence tabbing through buttons is allowed.
    y+= '''
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

    # submit values by element and col names
    y+= '''
function saveJsonValuesByNames(elename, colname, VAL) {

    cc = findCol(colname);
    rr = findQceleRow(elename);

    qcjson[rr][cc] = VAL;
}
'''

    # submit values by row and col nums
    y+= '''
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

    # find row index of QC element in JSON table
    y+= '''
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

    # find col index of item in JSON table
    y+= '''
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
function doSaveAllInfo() {
    updateLocalJson();

    var text     = JSON.stringify(qcjson);
    //var filename = "apqc.json";
    saveDownloadJsonfile(text, jsonfile);

} 
'''
    # The Helper
    y+= '''
function doShowHelp() {
    window.open('help.html', '_blank');

} 

function doShowMtedana(link) {
    window.open(link, '_blank');
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
    // window.alert("SAVING JSON: " + qcjson);
}
'''

    # Step 2 of saving the dataset: write to file
    y+= '''
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

def wrap_page_title( xtitle, xstudy, xsubj, 
                     vpad=0, addclass="", blockid='', padmarg=0 ):

    # start the first div on the page
    y = '''<div class="div_pad_class">'''

    # the boundary line: location+ID necessary for highlighting page
    # location
    y+= '''\n\n<hr class="hr_sec" id="hr_{}"/>'''.format(blockid)

    # the title
    y+= '''<div id="{}" '''.format(blockid)

    # this line offsets the anchor location for the navigation bar to
    # head to: the values here should be equal to the height of the
    # navigation bar (plus the line beneath it).
    y+= ''' style="padding-top: {0}px; margin-top: -{0}px;">'''.format(padmarg)

    y+= '''
    <h1><center> {} <center></h1></div>

<div style="text-align: center;">
    <div style="display: inline-block; text-align: left;">
    <pre><h2>subj: {}</h2></pre>
    <pre><h3>task: {}</h3></pre>

    </div>
</div>
'''.format( xtitle, xsubj, xstudy )



    if vpad:
        y = """\n"""+y
        y+="""\n"""

    return y


# -------------------------------------------------------------------

def wrap_block_title(x, vpad=0, addclass="", blockid='', padmarg=0):

    # close the previous section div (at least the title will have one)
    y = '''</div>\n\n'''

    # start the new section div
    y+= '''<div class="div_pad_class">'''

    # the boundary line: location+ID necessary for highlighting page
    # location
    y+= '''\n\n<hr class="hr_sec" ''' 
    if blockid :
        y+= ''' id="hr_{}" '''.format(blockid)
    y+= '''/>\n''' 

    # the title
    y+= '''<div '''
    if blockid :
        y+= ''' id="{}" '''.format(blockid)
    # this line offsets the anchor location for the navigation bar to
    # head to: the values here should be equal to the height of the
    # navigation bar (plus the line beneath it).
    y+= ''' style="padding-top: {0}px; margin-top: -{0}px;"'''.format(padmarg)
    y+= """><pre """
    y+= ''' {} '''.format(addclass)
    y+= """><center>["""+blockid+"""]<b> """
    y+= """<u>"""+x+"""</u>"""
    y+= ' '*(len(blockid)+3)       # balance blockid text
    y+= """</b></center></pre></div>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_block_text( x, vpad=0, addclass="", dobold=True, itemid='',
                     padmarg=0 ):
    addid = ''
    if itemid :
        addid = ''' id="{}" '''.format( itemid )

    y = """<div {0}""".format( addid )
    y+= ''' style="padding-top: {0}px; margin-top: -{0}px;"'''.format(padmarg)
    y+= ''' {} >'''.format(addclass)
    if dobold :
        y+= """<pre><b>"""+x+"""</b></pre></div>"""
    else:
        y+= """<pre>"""+x+"""</pre></div>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_img(x, wid=500, vpad=0, addclass=""):
    # [PT: Nov 20, 2018] needed this next line to center the text, and
    # needed "display: inline-block" in the img {} def to not have
    # whole line be a link.

    y = ''
    y+= vpad*'\n'

    y+= '''<div style="text-align: center">
    <a href="{0}"><img src="{0}" alt="{0}" {1} 
    style="display: inline-block; text-align: center;"></a> 
    </div>'''.format( x, addclass)
    y+= vpad*'\n'

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
    y+= '''<div> 
    <pre {} ><left><b>{}{}</b></left></pre>
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
