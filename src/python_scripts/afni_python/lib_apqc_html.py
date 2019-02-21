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
#########################################################################


# mostly ways to read & wrap text & images.
import sys
import os
import json
import lib_apqc_html_helps as lahh
import lib_apqc_html_css   as lahc

NULL_BTN1     = '' # empty space, used to keep button populated
NULL_BTN0     = '|' # empty space, used to keep button populated

json_cols = [ 'qcele', 'rating', 'comment' ]

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

    def add_text(self, DICT):
        if 'text' in DICT :
            if type(DICT['text']) == list :
                xx = '\n'.join(DICT['text'])
                self.text+= xx
            else:
                self.text+= DICT['text']

    def add_subtext(self, DICT):
        if 'subtext' in DICT :
            if type(DICT['subtext']) == list :
                xx = '\n'.join(DICT['subtext'])
                self.subtext+= xx
            else:
                self.subtext+= DICT['subtext']

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

def make_nav_table(llinks):
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
        # Put lines around "FINAL" element
        if i<N-1 : 
            finaltab = ''
        else:
            finaltab = '''style="background-color: #ccc; color: #000;" '''
            #finaltab = '''; border-left:2px solid cyan;" ''' # border-right:2px solid cyan;" '''

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
            '''.format( ll, lahh.brate_hover, "FORM:" ) 
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
    '''.format( 'good', lahh.bgood_hover, bgood_ind, bgood )

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onmousedown="allYourBaseAreBelongToUs({2})" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{ 
reallyAllYourBaseAreBelongToUs({2}); }} else {{ allYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'bad', lahh.bbad_hover, bbad_ind, bbad )

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onmousedown="allYourBaseAreBelongToUs({2})" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{ 
reallyAllYourBaseAreBelongToUs({2}); }} else {{ allYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'other', lahh.bother_hover, bother_ind, bother )

    y+= '''
<button class="button-generic button-RHS button-RHS-little btn2{0}" title="{1}" 
onkeydown="if (event.keyCode == 10 || event.keyCode == 13) {{ if (event.ctrlKey) {{ 
reallyAllYourBaseAreBelongToUs({2}); }} }} " 
ondblclick="reallyAllYourBaseAreBelongToUs({2})"> 
{3}</button>
    '''.format( 'clear', lahh.bclear_hover, bclear_ind, bclear )

    y+= '''</td>\n'''
    y+= '''</tr>\n'''

    # ROW:  hyperlinks (anchors) within the page
    y+= '''<tr>\n'''
    y+= '''<td style="width: 180px; white-space:nowrap;" id=td3_{}>'''.format( bsave )

    y+= '''<button class="button-generic button-RHS btn3save" title="{}" '''.format( lahh.bsave_hover ) 
    y+= '''onclick="doSaveAllInfo()">'''
    y+= '''{}</button>\n'''.format( bsave )

    y+= '''<button class="button-generic button-RHS btn3help" title="{}" '''.format( lahh.bhelp_hover ) 
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
        setTd1Border(newi, "#ffea00"); //"yellow");
        setTd1Border(topi,  "inherit");      ; //"#FFF", "#444");
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
    y+= """><center><b>"""+x+"""</b></center></pre></div>""" # width="80"
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
def wrap_dat(x, wid=500, vpad=0, addclass=""):
    y = ''
    y+= vpad*'\n'
    y+= '''<div>  
    <pre {} ><left><b>{}</b></left></pre>
</div>'''.format(addclass, x)
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

def read_dat(x):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    out = ''.join(txt)
    return out

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
    y+= '''{} ({})'''.format(d['pbar_top'], d['pbar_reason'])

    if 'vthr' in d :
        y+= '''\nthr : {}'''.format(d['vthr'])
        if 'vthr_reason' in d :
            y+= ''' ({})'''.format(d['vthr_reason'])

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



