
# Define and write the CSS
# --------------------------------------------------------------------
# --------------------------------------------------------------------
#
#
# 
ver = '2.21' ; date = 'May 22, 2019' 
# + [PT] fix pbar width
# + [PT] sectionize the CSS for putting in vars easier to parts
# + [PT] warning level color specification
#
# --------------------------------------------------------------------




# --------------------------------------------------------------------
# Notes on CSS choices:
#
# + need to use "display: inline-block" in the img { } def in
#   order to not have the whole line be a clickable link
#
# + .container and .container2 are the same except for the font
#   colors in each's "pre"; at this present time, that is to have
#   "subtxt" be gray, and header stuff be yellow.
# 
# + In the .container pre { } stuff, one could add the following
#   to have wrapping on long lines (though it won't help too much
#   with long censor lists):
#   white-space: pre;
# 
# + Move someday to light gray background?  background-color:
#   #DCDCDC;
# 
# + Re. the navigation bar, at the moment: 
#   !!!!!!!!!!!!!! need to update-- is 64 px here!
#   - Line 1: the text height is 20px, and the vertical margins on
#     it are 8px, so the total height is *36 px*;
#   - Line 2: the text height is 20px, and the vertical margins on
#     it are 2px, so the total height is *24 px*;
#   ---> for a TOTAL of: 62 px. ---> but use **74 px**
#
#   That total
#   navbar height determines the amount of padding needed above
#   the title text per section for jumping around, which is
#   actually done for most parts in lah.wrap_block_title().
#
# --------------------------------------------------------------------

# [PT: May 22, 2019] new param to keep track of max warn level and
# display something based on it.
# rank of warning levels
wlevel_ranks = {
    ''          : 0,
    'none'      : 1,
    'undecided' : 2,
    'mild'      : 3,
    'medium'    : 4,
    'severe'    : 5
    }
wlevel_colors = {
    ''          : "#CCCCCC",
    'none'      : "#CCFFCC",
    'undecided' : "#EAEA99",
    'mild'      : "#FFC3C4",
    'medium'    : "#FF7F7F",
    'severe'    : "#FF0000"
    }

wlevel_str = ' '.join(list(wlevel_ranks.keys())) 

# --------------------------------------------------------------------

# The CSS!
css_text = '''

h1 {
    padding-top: 80px;
    padding-bottom: 0px;
    margin: 0px;
    border: 0px;
    color: #FFC310; //#ccc;
    font-weight: bold;
    text-decoration: underline;
    font-size: 26px;
    font-family: "courier new", courier, monospace;
}

h2 {
    padding: 0px;
    margin: 0px;
    border: 0px;
    color: #fff; //#ccc; //#FFC310;
    font-weight: bold;
    font-size: 26px;
    font-family: "courier new", courier, monospace;
}

h3 {
    padding: 0px;
    margin: 0px;
    border: 0px;
    color: #ccc; //#FFC310; //#ccc; //#FFC310;
    font-weight: bold;
    font-size: 26px;
    font-family: "courier new", courier, monospace;
}
'''

css_text+= '''

/* have padding at top and bottom of each main section*/ 
.div_pad_class{
    padding: 25px 0px 15px 0px;
    margin: 0px;
}

img {
    padding: 2px 1px;
    display: inline-block;
    margin-left: auto;
    margin-right: auto;
    width:90%; 
}

pre {
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    color: #FFC310;
}

body {
    background-color: #014E33;
    font-family: "Lucida Console", Monaco, monospace;
    margin: 0px;
}

.hr_sec {
    display: block;
    height: 3px;
    //border: 0;
    //border-top: 2px solid #ccc;
    background-color: #ccc;
    margin: 0px 0px 10px 0px;
    padding: 0;
}

.padtop {
    padding-top: 10px;
    padding-bottom: 0px;
    margin: 0px;
    text-decoration: underline;
}
'''

css_text+= '''

.bordered {{
    padding: 0px;
    margin:  0px;
    border:  2px solid black;
}}

.warnbord {{
    padding: 0px;
    border:  2px solid black;
    color: #000000;
    background-color: #FFFFFF; /*#FFC3C4; */
    padding-left: 20px;
    padding-right: 20px;
    margin-top: 0px;
    margin-bottom: 10px;
    margin-left: auto;
    margin-right: auto;
    width: 90%; 
}}

.wcol_none {{
    text-align: center;
    background-color: {none};
}}

.wcol_undecided {{
    text-align: center;
    background-color: {undecided};
}}

.wcol_mild {{
    text-align: center;
    background-color: {mild};
}}

.wcol_medium {{
    text-align: center;
    background-color: {medium};
}}

.wcol_severe {{
    text-align: center;
    background-color: {severe};
}}

.datbord {{
    padding: 0px;
    border:  2px solid black;
    color: #000000;
    background-color: #ffffff;
    padding-left: 20px;
    margin-top: 0px;
    margin-left: auto;
    margin-right: auto;
    width:90%; 
}}
'''.format(**wlevel_colors)

css_text+= '''
.container {
    text-align: center;
    padding-top: 0px;
    margin: 0px;
    padding: px
}

.container pre {
    margin: 3px;
    padding-top: 20px;
    white-space: pre;
    display: inline-block;
    text-align: left;
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    color: #FFC310;
}

.container2 {
    text-align: center;
    padding-top: 0px;
    margin: 3px;
}

.container2 pre {
    margin: 3px;
    padding-top: 0px;
    white-space: pre;
    display: inline-block;
    text-align: left;
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    color: #ccc;
}
'''

css_text+= '''
img.pbar {
    width: 16ch;  /* 200px; 20rem; */
/*    border:  2px solid black; */
    height: 0.7em; /* 14px; */
    text-align: middle;
    margin: 0px;
    border: 0px;
    padding: 0px 0ch; /* 4px; */
}

.navbar {
    position: fixed; 
    width: 100%;
    padding-top: 2px;
    padding-bottom: 0px;
    height: 70px;
    width:100%;
    background-color: #000; // #444;
    border-bottom: 3px solid #222; //#ddd; //#fff ; //; //#fff;
    //margin: 5px solid #000;
}

table, tr {
    table-style-type: none;
    margin: 0px;
    padding: 0px;
    overflow: hidden;
    background-color: #000; // #444;
    border-collapse: collapse;
    border-bottom:  0px solid #ccc;
    top: 0;
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    //color: #ccc;
}

td {
    float: left;
    width: 80px;
    height: 30px;
    background-color: #000; // #444;
    border: 0px solid #bbb;
    margin: 0px;
    padding: 1px;
}

td a {
    display: block;
    color: white;
    text-align: center;
    text-vertical: center;
    padding: 0px;
    margin: 0px;
    text-decoration: none;
}

td a:hover:not(.active) {
    background-color: #111;
    color: #ffea00; //yellow;
}

td a:active {
    background-color: #4CAF50;
    color: green;
}
'''

css_text+= '''

/* Applies to all buttons */
.button-generic {
    overflow: hidden;
    text-align: center;
    vertical-align: middle; // text-vertical: center;
    font-size: 22px;
    cursor: pointer;
}

/* Applies to LHS buttons: .btn1, .btn5, .btn0 */
.button-LHS {
    background-color: #000; // #444;
    height: 28px;
    margin: 1px 3px;
    padding: 0px; 
    width: 74px;
}

/* "QC:" or "SET:" button, and background button in navbar */
.btn0 {
    color: #FFF;
    border: none;
    border-radius: 0px;
    font-family: "courier new", courier, monospace;
    font-size: 22px;
    font-weight: bold;
}


/* QC buttons */
.btn1 { 
    color: #000; // #444;
    border: solid 2px #bbb; //transparent; //none;
    border-radius: 12px;
    font-family:  "courier new", courier, monospace;
    font-size: 23px;
    font-weight: bold;
    margin-top: 2px;
}

/* section labels */
.btn5 {  
    color: #FFF; // #ffea00; //#FFF;
    border: solid 1px transparent;
    border-radius: 0px;
    font-family: Arial, "courier new", courier, monospace;
    font-size: 21px;
    font-weight: normal; //bold;
}
'''

css_text+= '''

// see: https://fvsch.com/styling-buttons/
/* Firefox: removes the inner border shown on focus */
.btn5::-moz-focus-inner {
  border: none;
}

/* Applies to .btn2* (A+, Ax) and .btn3save (Save) */
.button-RHS {
    border: solid 1px transparent;
    border-radius: 4px;
    font-family: "courier new", courier, monospace;
    font-size: 22px;
    font-weight: bold;
}

.button-RHS-little {
    float: left;
    margin: 4px 2px 0px 3px;
    height: 26px;
    width: 40px;
    padding: 0px 2px;
}

.btn2good {
    background-color: #67a9cf;
    color: #FFF;
}

.btn2bad {
    background-color: #d7191c;
    color: #000;
}

.btn2other {
    background-color: #fff;
    color: #777;
}

.btn2clear {
    background-color: #777;
    color: #fff;
    padding: 0px 0px;
}

.btn3save, .btn3help {
    background-color: #F0F0F0;
    float: left;
    color: #000;
    margin: 2;
    width: 86px;
    height: 28px;
    padding: 2px 2px;
}

button:hover:not(.active) {
    background-color: #333;
    color: #ffea00; //yellow;
    border: solid 2px #ffea00!important;
    border-color: #ffea00!important; //yellow;
}

button:focus:not(.active) {
    //background-color: #333;
    //color: #ffea00; //yellow;
    border: dashed 1px #ffea00!important;
    //border-color: #ffea00!important; //yellow;
}

button:active {
  transform: translateY(1px);
}

/* For the commenting buttons! */

/* The popup form format - hidden by default */
.form-popup {
  display: none;
  position: absolute;
  width: 400px;
  height: 75px;
  top: 75px;
  right: 15px;
  //border: 3px solid #f1f1f1;
  //z-index: 9;    
}
'''

css_text+= '''

/* Add styles to the form container */
.form-container {
  max-width: 400px;
  padding: 5px 5px 35px 5px;
  background-color: #ccc;
}

/* Full-width input fields */
.form-container textarea[type=text] {
  width: 100%;
  height: 100%;
  padding: 5px;
  margin: 0px 0px 0px 0px;
  border: none;
  background: #eee;
  font-family: helvetica, "courier new", courier, monospace;
  font-size: 18px;
  color: black;
}


/* When the inputs get focus, do something */
.form-container textarea[type=text]:focus {
  background-color: #FFF;
  outline: none;
}

/* Set a style for the submit/login button */
.form-container .btn {
  background-color: #7ece8c;
  color: white;
  padding: 0px 0px;
  border: 3px solid #5f926a;
  float: left;
  cursor: pointer;
  margin:0;
  width: 50%;
  height: 30px;
  opacity: 1;
  font-weight: bold;
  color: #FFF;
}

/* Add a red background color to the cancel button */
.form-container .cancel {
  background-color: #f36b9b;
  border: 3px solid #c20000;
  color: #000;
}

'''

# --------------------------------------------------------------------

# write the CSS file out
def write_css_file( ofile ):

    # output
    fff = open( ofile, "w" )
    fff.write( css_text )
    fff.close()
