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

# ==========================================================================

from flask      import Flask, send_from_directory, request, jsonify
from flask_cors import CORS # to circumvent 'no access issues from UI'
import json
import pprint   as     pp
import sys
import os
import argparse as     argp
import webbrowser
import textwrap
#import signal
#from threading import Timer

from afnipy     import lib_apqc_open as lao

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
Python's Flask module to be installed.**

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

  open_apqc.py  -infiles  data_21_ap/sub*/*results/QC_*/index.html 

{ddashline}
written by: T Hanayik (Oxford Uni, UK)
            PA Taylor (SSCC, NIMH, NIH, USA)
{ddashline}
'''.format(**help_dict)

# ========================================================================== 
# ============================== input stuff ===============================

# get args
parser = argp.ArgumentParser( prog=str(sys.argv[0]), 
                              add_help=False,
                              formatter_class=argp.RawDescriptionHelpFormatter,
                              description=textwrap.dedent(help_str_top),
                              epilog=textwrap.dedent(help_str_epi) )

parser.add_argument('-infiles', nargs='+',          
                    default=[lao.DEF['infiles']],
                    help='path to one or more APQC index.html files')

parser.add_argument('-portnum', nargs=1,
                    default=[lao.DEF['portnum']], 
                    help='specify port number to first try to open '
                    '(def: {})'.format(lao.DEF['portnum']))

parser.add_argument('-port_nsearch', nargs=1,
                    default=[lao.DEF['nsearch']],
                    help='specify how many port numbers to search through '
                    '(def: {})'.format(lao.DEF['nsearch']))

parser.add_argument('-host', nargs=1,
                    default=[lao.DEF['host']],
                    help='specify hostname (def: {})'.format(lao.DEF['host']))

parser.add_argument('-jump_to', nargs=1,
                    default=[lao.DEF['jump_to']],
                    help='when opening the APQC HTML, jump to the provided '
                    'QC block or sub-block name (e.g., "ve2a", "qsumm", etc.)')

parser.add_argument('-open_pages_off', action="store_false", 
                    default=[lao.DEF['do_open']],
                    help='turn off default behavior to open pages in a '
                    'browswer (def: open in new window[+tabs])')

parser.add_argument('-ver', action="store_true", 
                    default=False,
                    help='display version') 

parser.add_argument('-help', '-h', action="store_true", 
                    default=False,
                    help='display help') 

args         = parser.parse_args()
all_inpath   = args.infiles
portnum      = int(args.portnum[0])
port_nsearch = int(args.port_nsearch[0])
host         = args.host[0]
jump_to      = args.jump_to[0]
do_open      = bool(args.open_pages_off)
do_ver       = args.ver
do_help      = args.help

# display program version
if len(sys.argv) == 1 or do_help :
    parser.print_help()
    sys.exit(0)

# if nothing, show help
if do_ver :
    print(version)
    sys.exit(0)

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
rem_json_list = \
    lao.find_apqc_json_for_each_index_html(common_abs_path, rem_html_list)

# debugging display at present
print("++ Number of paths:", npath)
print("++ common_abs_path:" + dent + common_abs_path)
print("++ rem index.html:" + dent + dent.join(rem_html_list))
print("++ rem apqc_json:" + dent + dent.join(rem_json_list))

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

    #print('++++ RUNNING on port: {}'.format(request.host))

    # [TH] javascript needs to send the url parts, so that we can
    # parse them here and know which subject json to update
    posted_dict = request.get_json()
    pjson_fname = posted_dict['remJsonFilename']
    pjson_data  = posted_dict['JsonFileContents']

    #print('++ json fname is: ')
    #pp.pprint(pjson_fname)
    #print('++ json data are: ')
    #pp.pprint(pjson_data)

    try:
        # should be a match from within the initial input list
        index = rem_json_list.index(pjson_fname)
        # open using full path; means this prog can be run from anywhere
        json_to_open = common_abs_path + '/' + rem_json_list[index]
        with open(json_to_open, 'w', encoding='utf-8') as fff:
            json.dump( pjson_data, fff, 
                       ensure_ascii=False, indent=4 )
    except:
        print(f'** ERROR: could not find index for path {pjson_fname}')

    ''' TO ADD:  for FINAL info
    < get name of extra_info/out.ss_review.*.txt from apqc_json text str>

    if <final not ""> :
        < call function with subprocess calls to set abids >
    '''

    return jsonify(pjson_data)


#@app.route('/quit', methods=["POST", "GET"])
#def quit():
#    os.kill(os.getpid(), signal.SIGTERM)

@app.route("/load", methods=["GET"])
def load_json():
    print('loading json')

# ================================ main =====================================

if __name__ == "__main__":

    # find/verify an open port for the given host
    portnum = lao.find_open_port( portnum=portnum,
                                  nsearch=port_nsearch,
                                  host=host,
                                  verb=0 )

    # construct the web address for each page to be opened, and if
    # asked for open the browser (first page in new window and others
    # in new tab)
    be_new = 1
    for rem_html in rem_html_list:
        url = lao.construct_url(host, portnum, rem_html, jump_to=jump_to)
        print('''++ URL for browser: '{}' '''.format( url ))
        if do_open :
            webbrowser.open(url, new = be_new)
            be_new = 2

    # start the flask application---have to refresh above pages?
    app.run(host=host, port=portnum) #, debug=True)

    print("++ DONE.  Goodbye.")

    sys.exit(0)
