#
# ver : 1.0 || date: Jan 14, 2019 || auth: PA Taylor
# + For organizational purposes, put many help messages from APQC help
#   pages here
# + This includes things like hover text of functional buttons, the help
#   description of the page overall, etc.
#
#########################################################################

import sys
import os
import json
import collections         as coll
import lib_apqc_html       as lah

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

# ----------------------------------------------------------------------

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
#

qc_title           = coll.OrderedDict()
qc_title["Top"]    = [ "Top of page for:&#10${subj}", 
                       "afni_proc.py single subject report" ]

qc_blocks          = coll.OrderedDict()
qc_blocks["vorig"] = [ "vols in orig space", 
                       "Check: vols in orig space" ]
qc_blocks["ve2a" ] = [ "vol alignment (EPI-anat)", 
                       "Check: vol alignment (EPI-anat)" ]
qc_blocks["va2t" ] = [ "vol alignment (anat-template)", 
                       "Check: vol alignment (anat-template)" ]
qc_blocks["vstat"] = [ "statistics vols", 
                       "Check: statistics vols" ]
qc_blocks["mot"  ] = [ "motion and outliers", 
                       "Check: motion and outliers" ] 
qc_blocks["regr" ] = [ "regressors", 
                       "Check: regressors (combined and individual)" ]
qc_blocks["warns"] = [ "all warnings from processing", 
                       "Check: all warnings from processing" ]
qc_blocks["qsumm"] = [ "summary quantities from @ss_review_basic", 
                       "Check: summary quantities from @ss_review_basic" ]

qc_link_final      = [ "FINAL", 
                       "overall subject rating" ] 


# -----------------------------------------------------------------------------

# --------------------------------------------------------------------

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

        ht+= lah.wrap_block_title( x[0],
                                   vpad=1,
                                   addclass=" class='padtop' ",
                                   blockid='' )

        ht+= lah.wrap_block_text( x[1],
                                  addclass=" class='container' " )



    # -------------- finish ---------------------

    ht+= '''</body>'''
    ht+= '''</html>'''

    # -------------- output ---------------------

    fff = open( ofile, "w" )
    fff.write( ht )
    fff.close()

# -----------------------------------------------------------------------

apqc_help = [ 
['HELP FILE FOR AP-QC', 
 '''afni_proc.py's single subject QC report form'''], 
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
    settings.)'''
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
Use ctrl+click on a QC button to open a comment window.  

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
''']]

# ---------------------------------------------------------------------

brate_hover = '''QC BUTTON FORM

NAVIGATE
Click a label ('Top', etc.) to jump to a section.

RATE + COMMENT
Click the QC button below it to record your rating, toggling through:
    X  :  bad,
    ?  :  other/revisit,
    +  :  good.
Double click the QC button to provide a comment.

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
