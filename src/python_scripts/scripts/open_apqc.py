from flask      import Flask, send_from_directory, request, jsonify
from flask_cors import CORS # to circumvent 'no access issues from UI'
import json
import sys
import os
import glob
import argparse
import signal

# [PT: Aug 8, 2022] add in functionality to have multiple paths to
# several QC directory index.html files input.
# + following on from discussion with TH about finding common_abs_path
#   and 'remainder' paths.
#
# Example running, from above the results directory:
#    run open_apqc.py -i FT.NL_NEW12.results/QC_FT.NL_NEW12/index.html 
#
# ========================================================================== 


### [PT: Aug 8, 2022] this function is no longer used
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

def calc_nstep_path(pstr):
    """Calculate the number of steps in a path pstr.  For example, /a/b/c
    has 3 steps, as does /d/e/f/, and g/h has 2 steps, as does i//j.
    And the single char path / has 1 step.

    This function does NOT verify that pstr is a valid path on the OS.

    Parameters
    ----------
    pstr   : str
             a string representing a path

    Return
    ------
    nstep  : int
             number of 'steps' in the path
    """

    RETURN_IF_BAD = -1

    if type(pstr) != str :
        print("**ERROR: need to a string of a path name")
        return RETURN_IF_BAD

    N = len(pstr) 

    # 'simple' cases
    if not(N) :                return 0
    if not( '/' in pstr ) :    return 1

    # get rid of instances of '//'
    if '//' in pstr:
        count = 0
        while '//' in pstr and count<N :
            pstr = pstr.replace('//', '/')
            count+=1

    # another special case
    if pstr == '/' :    return 1

    # get rid of any hanging slash
    if pstr[-1] == '/' :    pstr = pstr[:-1]
    
    # mainstream calcs
    if pstr[0] == '/' :    nstep = pstr.count('/')
    else:                  nstep = pstr.count('/') + 1

    return nstep

def find_common_and_remainder_paths( inp_path_list, min_rem_len = None,
                                     verb = 0 ):
    """Input a list of N paths (relative or absolute).  This program
    converts each to an absolute path and outputs two things:
      + the 'greatest common' absolute path among that list (which might 
        be further constrained by specifying a minimum length of remainder
        paths)
      + an N-length list of the 'remainder' paths for each index.html.

    One also can control the minimal length of remainder paths (for
    specific APQC utility).  For example, open_apqc.py calls might use
    min_rem_len=2.

    Parameters
    ----------
    inp_path_list : list (of str)
             list of rel or abs paths 
    min_rem_len : int
             minimum path length of the remainder files.  This places a
             constraint on the common path
    verb : int
             verbosity level

    Return
    -------
    common_abs_path : str
             the common (absolute) path of the input list of paths 
    rem_path_list : list (of str)
             list of rel paths to each index.html

    """
    
    RETURN_IF_BAD = "", [], []

    if type(inp_path_list) != list :
        print("**ERROR: need to input a list of paths")
        return RETURN_IF_BAD

    N = len(inp_path_list)

    if not(N) :
        print("+* WARNING: input list appears to be empty")
        return RETURN_IF_BAD

    # get abs path version of each path (expanduser deals with '~')
    abs_path_list = []
    for path in inp_path_list:
        abs_path = os.path.abspath(os.path.expanduser(path))
        abs_path_list.append(abs_path)

    # calc the common path, and check it quickly.  NB: it is possible
    # that the path list could include a path on an OS and one on a
    # USB, which would lead to the common path being just '/'
    common_abs_path = os.path.commonpath(abs_path_list)
    common_nstep    = calc_nstep_path(common_abs_path)
    if common_nstep < 1 :
        print("** ERROR: common *abs* path '{}' is nonexistent?"
              "".format(common_abs_path))
        print("   How is that possible?  Too implausible to proceed")
        return RETURN_IF_BAD

    if verb :
        print("++ Initial common path:", common_abs_path)

    rem_path_list = []
    rem_lens_list = []
    for path in abs_path_list:
        rem_path  = os.path.relpath(path, start = common_abs_path)
        # special case (e.g., if N==1)
        if path == common_abs_path and rem_path == '.' :
            rem_path = ''
        rem_nstep = calc_nstep_path(rem_path)
        if rem_nstep < 0 :
            print("** ERROR: '{}' appears to not exist?".format(path))
            return RETURN_IF_BAD
        rem_lens_list.append(rem_nstep)
        rem_path_list.append(rem_path)

    if verb :
        rem_str = '   '.join(rem_path_list)
        print("++ Initial remainder paths:\n   ", rem_path_list)

    # did user enter a minimal length condition for remainder paths?
    if min_rem_len != None :
        # at this point, min_rem_nstep must be >0, because of the
        # check done when calc'ing remainder paths
        rem_nstep_min = min(rem_lens_list)

        # criterion to shift between common and rem paths
        nshift = min_rem_len - rem_nstep_min
        if nshift > 0 :                # do adjustment
            print("++ Adjusting common/rem paths by {} steps".format(nshift))
            if nshift > common_nstep :
                print("** ERROR: user's min_rem_len '{}' arg is too large for\n"
                      "   the current min remainder path len '{}' and the\n"
                      "   common_abs_path '{}'"
                      "".format(min_rem_len, rem_nstep_min, common_abs_path))
                return RETURN_IF_BAD

            # adjust both parts
            common_split    = common_abs_path.split('/')
            common_abs_path = '/'.join(common_split[:-nshift])
            rem_adjust      = '/'.join(common_split[-nshift:])

            if verb :
                print("++ common_split:", common_split)
                print("++ (raw) common_abs_path:", common_abs_path)
                print("++ (raw) rem_adjust:", rem_adjust)

            for ii in range(N):
                if rem_path_list[ii] :
                    rem_path_list[ii] = rem_adjust + '/' + rem_path_list[ii] 
                else:  # e.g., in case of N==1
                    rem_path_list[ii] = rem_adjust

            # for case where ndiff == common_nstep
            if not(common_abs_path) :
                common_abs_path = '/'

            # final sanity checks: *should* never get errors here
            common_nstep_final = calc_nstep_path(common_abs_path)
            if common_nstep < 1 :
                print("** ERROR: final common *abs* path '{}' is nonexistent?"
                      "".format(common_abs_path))
                print("   How is that possible?  Too implausible to proceed")
                return RETURN_IF_BAD

            for path in rem_path_list:
                nrem = calc_nstep_path(path)
                if nrem < min_rem_len :
                    print("** ERROR: final remainder path '{}' is nonexistent?"
                          "".format(path))
                    return RETURN_IF_BAD

    return common_abs_path, rem_path_list

def find_apqc_json_for_each_index_html(common_abs_path, rem_html_list):
    """

    Parameters
    ----------
    common_abs_path : str
             the common (absolute) path of the input list of paths 
    rem_html_list : list (of str)
             list of rel paths to each index.html

    Return
    -------
    rem_json_list : list (of str)
             list of rel paths to each apqc_*.json, accompanying each 
             index.html

    """

    RETURN_IF_BAD = []

    if type(rem_html_list) != list :
        print("**ERROR: need to input a list of paths 'rem_html_list'")
        return RETURN_IF_BAD
    if type(common_abs_path ) != str :
        print("**ERROR: need to input a str pathname 'common_ab_path'")
        return RETURN_IF_BAD

    N = len(rem_html_list)

    if not(N) :
        print("+* WARNING: input list appears to be empty")
        return RETURN_IF_BAD

    # create matched list to apqc*json files
    rem_json_list = []
    for path in rem_html_list :
        gpath = common_abs_path
        if path :
            gpath += '/' + path
        # full path to json
        json_list  = glob.glob(gpath.replace("index.html", "apqc_*.json"))
        njson_list = len(json_list)
        if njson_list != 1 :
            print("** ERROR: found wrong number of possible JSON files for '{}'"
                  "".format(path))
            print("   Should have exactly 1, but here had {}."
                  "".format(njson_list))
            return RETURN_IF_BAD
        rem_json = os.path.relpath(json_list[0], start = common_abs_path)
        rem_json_list.append(rem_json)

    return rem_json_list


def verify_all_paths_to_html( inp_path_list ):
    """Input a list of N paths (relative or absolute).  This program
    returns 1 if all paths exist and end with 'index.html', and
    returns 0 in all other cases.  

    Inputting an empty list leads to 0 being returned.

    Parameters
    ----------
    inp_path_list : list (of str)
             list of rel or abs paths 

    Return
    -------
    is_ok : int
             1 if all paths exist and end with 'index.html'; else, 0
    """
    
    
    RETURN_IF_BAD = 0

    if type(inp_path_list) != list :
        print("**ERROR: need to input a list of paths")
        return RETURN_IF_BAD

    N = len(inp_path_list)

    if not(N) :    return RETURN_IF_BAD

    # run the gauntlet
    for path in inp_path_list:
        if not(path.endswith('index.html')) :
            print("**ERROR: path+file name '{}' does not end with index.html"
            "".format(path))
            return RETURN_IF_BAD
        if not(os.path.isfile(os.path.expanduser(path))) :
            print("**ERROR: path+file name '{}' does not exist"
            "".format(path))
            return RETURN_IF_BAD

    return 1


# ========================== input stuff ==========================

start_dir = os.getcwd()

# get args
parser = argparse.ArgumentParser()
parser.add_argument('-i', nargs='+') # relative path to index.html
args       = parser.parse_args()
all_inpath = args.i

# check that the user input a set of valid paths to index.html
is_valid = verify_all_paths_to_html(all_inpath)
if not(is_valid) :
    print("** ERROR: invalid input paths, cannot proceed")
    sys.exit(1)

# ***question: should we verify that each path in all_inpath is
# ***unique? probably, since we are opening+writing

# parse the input list of N paths.  Get an appropriate common path and
# a list of N 'remainder' paths for each index.html
common_abs_path, rem_html_list = \
    find_common_and_remainder_paths(all_inpath, min_rem_len=2)

# ... and then get accompanying 'remainder' list of apqc_*.json files
rem_json_list = \
    find_apqc_json_for_each_index_html(common_abs_path, rem_html_list)


# ------- right now, using the first element in the input list of
# ------- args (hence the [0] selectors), but later will loop over all

apqc_json = rem_json_list[0]

print("++ common_abs_path : {}".format(common_abs_path))
print("++ rem index.html  : {}".format(rem_html_list[0]))
print("++ rem apqc_json   : {}".format(rem_json_list[0]))

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

    jdata = request.get_json()
    with open(apqc_json, 'w', encoding='utf-8') as f:
        json.dump(jdata, f, ensure_ascii=False, indent=4)
    
    ''' TO ADD:  for FINAL info
    < get name of extra_info/out.ss_review.*.txt from apqc_json text str>

    if <final not ""> :
        < call function with subprocess calls to set abids >
    '''

    return jsonify(jdata)


# abids_json_tool.py -input extra_info/out.ss_review.*.txt -txt2json -prefix TMP.json
# abids_json_tool.py -input TMP.json -add_json "FINAL" TEST -force_add -prefix TMP2.json
# abids_json_tool.py -input TMP2.json -prefix extra_info/out.ss_review.*.txt -json2txt -overwrite


#@app.route('/quit', methods=["POST", "GET"])
#def quit():
#    os.kill(os.getpid(), signal.SIGTERM)

@app.route("/load", methods=["GET"])
def load_json():
    print('loading json')

# ================================ main =====================================

if __name__ == "__main__":
    app.run()

