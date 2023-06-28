#!/usr/bin/env python

#
# A main program for opening several APQC HTML files using a single
# Flask-provided server.  In this way, QC buttons get saved
# automatically as any of them is edited/updated.
#
# Example running, from above the results directory:
#    run open_apqc.py -i FT.NL_NEW12.results/QC_FT.NL_NEW12/index.html 
#
# written by T Hanayik (Oxford Uni, UK) and PA Taylor (NIMH, NIH, USA)
# 
# ==========================================================================

version = '1.0'
version = '1.1'  # adds in Timer functionality, so multiple pages can
                 # load when opened.
                 # Also add '-hview' functionality
version = '1.11' # add more help text and examples
version = '2.0'  # add in AV button functionality 

# ==========================================================================

# primary imports (flask* mods imported below, after help parsing)
from threading    import Timer
import json
import pprint     as     pp
import sys
import os
import argparse   as     argp
import webbrowser
import textwrap

from afnipy       import lib_apqc_open as lao
from afnipy       import afni_base     as BASE

dent = '\n' + 5*' '

help_dict = {
    'ddashline' : '='*76,
    'ver'       : version,
}

# ========================================================================== 
# setup help and options

help_str_top = '''
Overview ~1~

This program is used to open one or more of afni_proc.py's quality
control (APQC) HTML files.

It is designed to allow saving QC ratings and notes as the files are
browsed, by using a local server.  **This functionality requires
Python's Flask and Flask-CORS modules to both be installed.**

{ddashline}

Options ~1~

'''.format(**help_dict)

help_str_epi = '''
{ddashline}

Notes on usage ~1~

While running/viewing the HTMLs:
  When the server is running, the terminal must be left open so the
  server can keep running (a lot like using a Jupyter-Notebook).  

When finished:
  When you are doing viewing the APQC HTMLs, you can close all of
  them, and type 'Ctrl+c' in the terminal (to cancel/exit the server).

{ddashline}

Examples ~1~

  1) Open many APQC HTML pages for several subjects, with the server
     on so QC ratings/comments will be saved:

     open_apqc.py  -infiles  data_21_ap/sub*/*results/QC_*/index.html 

  2) The same as #1, but have each page jump to the 'vstat' block of
     the HTML:

     open_apqc.py                                                 \\
         -infiles  data_21_ap/sub*/*results/QC_*/index.html       \\
         -jump_to  vstat

  3) The same as #2, but open all pages in new tabs of the existing
     browser window (instead of starting new window):

     open_apqc.py                                                 \\
         -infiles  data_21_ap/sub*/*results/QC_*/index.html       \\
         -jump_to  vstat                                          \\
         -new_tabs_only

{ddashline}
written by: T Hanayik (Oxford Uni, UK)
            PA Taylor (SSCC, NIMH, NIH, USA)
{ddashline}
'''.format(**help_dict)

# ========================================================================== 
# ============================== input stuff ===============================

# get args
parser = argp.ArgumentParser( prog=str(sys.argv[0]).split('/')[-1],
                              add_help=False,
                              formatter_class=argp.RawDescriptionHelpFormatter,
                              description=textwrap.dedent(help_str_top),
                              epilog=textwrap.dedent(help_str_epi) )

parser.add_argument('-infiles', nargs='+',          
                    default=[lao.DEF['infiles']],
                    help='path to one or more APQC index.html files')

parser.add_argument('-jump_to', nargs=1,
                    default=[lao.DEF['jump_to']],
                    help='when opening the APQC HTML, jump to the provided '
                    'QC block or sub-block name (e.g., "ve2a", "qsumm", etc.)')

parser.add_argument('-disp_jump_ids', action="store_true", 
                    default=lao.DEF['disp_jump_ids'],
                    help='display list of IDs within first index.html file '
                    'that can be jumped to with the "-jump_to .." option '
                    '(must be used with "-infiles ..")')

parser.add_argument('-new_tabs_only', action="store_true", 
                    default=lao.DEF['new_tabs_only'],
                    help='open each page in new tab '
                    '(def: open first page in a new window, then any more '
                    'in new tabs)')

parser.add_argument('-new_windows_only', action="store_true", 
                    default=lao.DEF['new_wins_only'],
                    help='open each page in a new window '
                    '(def: open first page in a new window, then any more '
                    'in new tabs)')

parser.add_argument('-pause_time', nargs=1,
                    default=[lao.DEF['pause_time']],
                    help='total time (s) to pause to let pages load '
                    '(def: {})'.format(lao.DEF['pause_time']))

parser.add_argument('-open_pages_off', action="store_false", 
                    default=lao.DEF['open_pages'],
                    help='(not typically needed) '
                    'turn off default behavior to open pages in a '
                    'browswer (def: open in new window[+tabs])')

parser.add_argument('-portnum', nargs=1,
                    default=[lao.DEF['portnum']], 
                    help='(not typically needed) '
                    'specify port number to first try to open '
                    '(def: {})'.format(lao.DEF['portnum']))

parser.add_argument('-port_nsearch', nargs=1,
                    default=[lao.DEF['nsearch']],
                    help='(not typically needed) '
                    'specify how many port numbers to search through '
                    '(def: {})'.format(lao.DEF['nsearch']))

parser.add_argument('-host', nargs=1,
                    default=[lao.DEF['host']],
                    help='(not typically needed) '
                    'specify hostname '
                    'def: {})'.format(lao.DEF['host']))

parser.add_argument('-verb', nargs=1,
                    default=[lao.DEF['verb']],
                    help='verbosity level '
                    '(def: {})'.format(lao.DEF['verb']))

parser.add_argument('-ver', action="store_true", 
                    default=False,
                    help='display version') 

parser.add_argument('-help', action="store_true", 
                    default=False,
                    help='display help in terminal') 

parser.add_argument('-hview', action="store_true", 
                    default=False,
                    help='display help in a text editor') 

args             = parser.parse_args()
all_inpath       = args.infiles
portnum          = int(args.portnum[0])
port_nsearch     = int(args.port_nsearch[0])
host             = args.host[0]
jump_to          = args.jump_to[0]
do_open_pages    = args.open_pages_off
do_ver           = args.ver
do_help          = args.help
do_hview         = args.hview
do_disp_jump_ids = args.disp_jump_ids
do_new_tabs_only = args.new_tabs_only
do_new_wins_only = args.new_windows_only
pause_time       = float(args.pause_time[0])
verb             = int(args.verb[0])

# ---------------------------- help stuff -------------------------------

# hview functionality
if do_hview :
    prog = str(sys.argv[0]).split('/')[-1]
    cmd  = 'apsearch -view_prog_help {}'.format( prog )
    BASE.simple_shell_exec(cmd)
    sys.exit(0)
    
# display program version
if len(sys.argv) == 1 or do_help :
    parser.print_help()
    sys.exit(0)

# if nothing, show help
if do_ver :
    print(version)
    sys.exit(0)

# show jump_to IDs
if do_disp_jump_ids :
    lao.disp_jump_ids_file(all_inpath)
    sys.exit(0)

# ----------------------- import flask -----------------------------------
# put import here, so users can get help info without it
try:
    from flask        import Flask, send_from_directory, request, jsonify
    from flask_cors   import CORS   # to circumvent 'no access issues from UI'
except (ImportError,NotImplementedError):
    emsg = """** ERROR. To be able to run this program, please install the 
   following Python modules:
      flask (v>= ***)
      flask_cors (v>= ***)
   For example, this could be done with a package manager or conda."""
    print(emsg)
    sys.exit(3)

# !!! (perhaps) TO DO: add in a non-flask-driven open option, perhaps,
# !!! though this is annoying/tricky/unstable with browser+OS variation

# ------------------------------------------------------------------------
# continue on: process opts slightly

if verb > 2 :
    print("++ do_new_tabs_only:", do_new_tabs_only)
    print("++ do_new_wins_only:", do_new_wins_only)

# how to open first page in browser
if do_new_tabs_only:   first_page_code = 2         # in new tab
else:                  first_page_code = 1         # in new window

if do_new_wins_only:   other_page_code = 1         # in new tab
else:                  other_page_code = 2         # in new window

if verb > 2 :
    print("++ first_page_code:", first_page_code)
    print("++ other_page_code:", other_page_code)

# ======================== determine path pieces ===========================

# check that the user input a set of valid paths to index.html
is_valid = lao.verify_all_paths_to_html(all_inpath)
if not(is_valid) :
    print("** ERROR: invalid input paths, cannot proceed")
    sys.exit(1)

# parse the input list of N paths.  Get an appropriate common path and
# a list of N 'remainder' paths for each index.html
common_abs_path, rem_html_list = \
    lao.find_common_and_remainder_paths(all_inpath, min_rem_len=2)

npath = len(rem_html_list)

# ... and then get accompanying 'remainder' list of apqc_*.json files
rem_apqc_json_list, rem_ssrev_json_list = \
    lao.find_other_json_for_each_index_html(common_abs_path, rem_html_list)

# debugging display at present
if verb :
    print("++ Number of paths:", npath)
    print("++ common_abs_path:" + dent + common_abs_path)
    print("++ rem index.html:" + dent + dent.join(rem_html_list))
    print("++ rem apqc_json:" + dent + dent.join(rem_apqc_json_list))
    print("++ rem ssrev_json:" + dent + dent.join(rem_ssrev_json_list))

# ===========================================================================
# ========================== flask/decorator stuff ==========================

app = Flask(__name__)           # initialize flask app
CORS(app)                       # let CORS package upgrade app

@app.route("/<path:path>", methods=["GET"])
def index(path):
    '''comment this...

    'path' is the relative path to a particular index.html from the
    common_abs_path base

    '''
    print("++ path is:", path)
    return send_from_directory(common_abs_path, path)

@app.route("/save", methods=["POST"])
def save_json():
    """
    Function to save the QC button/rating info to the apqc*.json file.
    """

    # [TH] javascript needs to send the url parts, so that we can
    # parse them here and know which subject json to update
    posted_dict = request.get_json()
    pjson_fname = posted_dict['remJsonFilename']
    pjson_ssrev = posted_dict['remJson_ssrev']
    pjson_data  = posted_dict['JsonFileContents']

    # save the form information to apqc_{subj}.json
    try:
        # should be a match from within the initial input list
        index = rem_apqc_json_list.index(pjson_fname)
        # open using full path; means this prog can be run from anywhere
        json_to_open = common_abs_path + '/' + rem_apqc_json_list[index]
        with open(json_to_open, 'w', encoding='utf-8') as fff:
            json.dump( pjson_data, fff, 
                       ensure_ascii=False, indent=4 )
    except:
        print(f'** ERROR: could not find index for path {pjson_fname}')

    # add+save the form information to extra_info/out.ss_review.{subj}.json
    try:
        # should be a match from within the initial input list
        index = rem_ssrev_json_list.index(pjson_ssrev)

        # open using full path; means this prog can be run from anywhere
        json_to_open = common_abs_path + '/' + rem_ssrev_json_list[index]
        with open(json_to_open, 'r', encoding='utf-8') as fff:
            ssrev_dict = json.load(fff)

        # turn the list into a dict
        pjson_dict = lao.apqc_list_to_dict(pjson_data)

        # add the rating dictionary to the ssrev dict
        mdict = lao.merge_two_dicts(ssrev_dict, pjson_dict)

        with open(json_to_open, 'w', encoding='utf-8') as fff:
            json.dump( mdict, fff, 
                       ensure_ascii=False, indent=4 )
    except:
        print(f'** ERROR: could not find index for path {pjson_ssrev}')

    return jsonify(pjson_data)


#@app.route('/quit', methods=["POST", "GET"])
#def quit():
#    os.kill(os.getpid(), signal.SIGTERM)

# for AV button
@app.route("/run_av", methods=["POST"])
def run_av():
    """Using the name of a script passed from the HTML, execute a shell
command to run that script in the expected, appropriate directory.
This assumes that the HTML is still in the results directory.

The way that the HTML's location is known is modeled on how the QC
buttons work (from the button JSON's files name, and using it to find
the appropriate index.html).

The return is purely pro forma.

    """
    posted_dict = request.get_json()              # provided by HTML
    script_name = posted_dict['script']           # name of script to run
    pjson_fname = posted_dict['remJsonFilename']  # used to figure out dir loc

    curr_dir = os.getcwd()                        # use to jump back

    try:
        # should be a match from within the initial input list
        index = rem_apqc_json_list.index(pjson_fname)
        # open using full path; means this prog can be run from anywhere
        json_to_open = common_abs_path + '/' + rem_apqc_json_list[index]
        location = os.path.dirname(os.path.dirname(json_to_open))

        # jump to the appropriate AP results directory location, run
        # the script, and then jump back.
        os.chdir(location)
        cmd = 'tcsh ' + script_name
        com = BASE.shell_com(cmd, capture=1)
        com.run()
        os.chdir(curr_dir)

    except Exception as e: 
        print("** ERROR with script '{}':".format(script_name))
        print(e)

    return 'something'

### [TH] for finding NiiVue from a more general spot
# create a new URL to access
@app.route("/assets/<path:path>", methods=["GET"])
def assets(path):
    '''This is used to help find NiiVue from outside the QC directory,
namely from the AFNI binaries directory (e.g.,
somewhere/abin/niivue_afni.umd.js).

Parameters
----------
path : str 
    the relative path to a particular asset from the (global)
    common_abs_path base

Returns
-------
something very important, re. the HTML

    '''

    # get location of AFNI binaries dir, which is where niivue_afni.umd.js
    # will live
    cmd = 'which afni'
    com = BASE.shell_com(cmd, capture=1)
    com.run()
    afni_fullpath = com.so[0]                 # full path + prog
    abin_dir = os.path.dirname(afni_fullpath) # full path

    return send_from_directory(abin_dir, path)


@app.route("/load", methods=["GET"])
def load_json():
    print('loading json')

def open_all_browser_pages( portnum ):
    """This function loops over the list of HTML pages (rem_html_list) and
    opens them at the 'jump_to' location, using the given host and
    portnum.

    The following could all be parameters, in addition to the current
    portnum:
    rem_html_list, host, jump_to, page_code, other_page_code.

    This function exists so that the Time functionality of threading
    can be used, so delay page rendering slightly until the Flask
    server is up and running.

    """
    page_code = first_page_code
    for rem_html in rem_html_list:
        url = lao.construct_url(host, portnum, rem_html, jump_to=jump_to)
        print('''++ URL for browser: '{}' '''.format( url ))
        if do_open_pages :
            webbrowser.open(url, new = page_code)
            page_code = other_page_code

# ================================ main =====================================

if __name__ == "__main__":

    # find/verify an open port for the given host
    portnum = lao.find_open_port( portnum=portnum,
                                  nsearch=port_nsearch,
                                  host=host,
                                  verb=verb )

    if 1 :
        print("++ NB: Ignore the 'This is a development server ...' warning")

    # construct the web address for each page to be opened, and if
    # asked for open the browser (first page in new window and others
    # in new tab)
    Timer(pause_time, open_all_browser_pages, [portnum]).start() 
    
    # start the flask application---have to refresh above pages?
    app.run(host=host, port=portnum) #, debug=True)

    print("++ DONE.  Goodbye.")

    sys.exit(0)
