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

from flask      import Flask, send_from_directory, request, jsonify
from flask_cors import CORS # to circumvent 'no access issues from UI'
import json
import pprint as pp
import sys
import os
import argparse
#import signal

from afnipy import lib_apqc_open as lao

dent = '\n' + 5*' '

# ========================================================================== 
# ============================== input stuff ===============================

# get args
parser = argparse.ArgumentParser()
parser.add_argument('-i', nargs='+') # relative path to index.html
args       = parser.parse_args()
all_inpath = args.i


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
    app.run()

