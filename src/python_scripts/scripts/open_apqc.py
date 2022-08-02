from flask      import Flask, send_from_directory, request, jsonify
from flask_cors import CORS # to circumvent 'no access issues from UI'
import json
import sys
import os
import glob
import argparse
import signal

# 
#
# Example running, from above the results directory:
#    run open_apqc.py -i FT.NL_NEW12.results/QC_FT.NL_NEW12/index.html 
#
# ========================================================================== 


### PT:  have to do something to capture ~ and interpret it
def parse_qc_html_path(pstr):
    """Take in a string pstr that should be the path to an APQC index.html
file, and return path information to a QC HTML

    Parameters
    ----------
    pstr   : str
             path to subject's index.html in QC_${str}/

    Return
    ------
    qc_dir : str
             relative path to (and including) QC_${subj} dir, which contains
             the index.html file

    apqc_json : str
             relative path to (and including) apqc_${subj}.json
    """

    plist = pstr.split('/')
    ndir  = len(plist)
    if not(ndir) or plist[-1] != 'index.html':
        print("** ERROR: input should be path to, and including, index.html")
        sys.exit(1)

    # make sure the dir holding index.html exists as a separate link
    # the path name (even though this should never occur in practice)
    if ndir == 1 :
        plist.insert(0, '.')
        ndir+= 1

    qc_dir = '/'.join(plist[:-1])

    apqc_json  = glob.glob(qc_dir+"/apqc_*.json")
    napqc_json = len(apqc_json)

    if not(napqc_json) :
        print("** ERROR: could not find apqc_*.json")
        sys.exit(1)
    elif napqc_json > 1 :
        print("** ERROR: found too many ({}) apqc_*.json".format(napqc_json))
        sys.exit(1)

    return qc_dir, apqc_json[0]

# ========================== input stuff ==========================

start_dir = os.getcwd()

parser = argparse.ArgumentParser()
#parser.add_argument('-d', nargs='+') # input name of dir holding *.results
#args = parser.parse_args()
#dirs = args.d
#exemplar = dirs[0]
#dirname = os.path.dirname(exemplar) #"FT_NEW.results/QC_FT/" # os.path.dirname(exemplar)

parser.add_argument('-i', nargs='+') # relative path to index.html
args  = parser.parse_args()
ipath = args.i[0]           # later deal with LIST ASPECT


# these variables are used below
qc_dir, apqc_json = parse_qc_html_path(ipath)

print("++ ipath     : {}".format(ipath))
print("++ apqc_json : {}".format(apqc_json))
print("++ qc_dir    : {}".format(qc_dir))

# ========================== flask/decorator stuff ==========================

app = Flask(__name__)           # initialize flask app
CORS(app)                       # let CORS package upgrade app

@app.route("/<path:path>", methods=["GET"])
def index(path):
    '''comment this...

    'path' is the name of the apqc_*.json file, which sits in the
    directory 'qc_dir'.

    '''
    print("++ path is:", path)
    return send_from_directory(qc_dir, path)

@app.route("/save", methods=["POST"])
def save_json():
    """
    Function to save the QC button/rating info to the apqc*.json file.
    """

    jdata = request.get_json()
    with open(apqc_json, 'w', encoding='utf-8') as f:
        json.dump(jdata, f, ensure_ascii=False, indent=4)
    # TODO: save "FINAL" to extra text file 
    return jsonify(jdata)

#@app.route('/quit', methods=["POST", "GET"])
#def quit():
#    os.kill(os.getpid(), signal.SIGTERM)

@app.route("/load", methods=["GET"])
def load_json():
    print('loading json')

# ================================ main =====================================

if __name__ == "__main__":
    app.run()

