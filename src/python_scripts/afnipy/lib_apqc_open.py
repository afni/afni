#!/usr/bin/env python

# A library of supplementary functions for helping to open APQC HTMLs
# with a Flask server.
# 
# Primarily used with open_apqc.py
#
# written by PA Taylor (NIMH, NIH, USA) and T Hanayik (Oxford Uni, UK)
# 
# ==========================================================================

import sys, os
import glob
import copy
import socket


# ==========================================================================

# default parameter settings
DEF = {
    'infiles' : [],             # list of */index.html files to open
    'portnum' : 5000,           # port number to start trying
    'nsearch' : 500,            # number of ports to search for an open one
    'host'    : '127.0.0.1',    # hostname
    'jump_to' : None,           # hash to jump to in APQC page
    'open_pages'    : True,     # T/F: open pages in browser?
    'disp_jump_ids' : False,    # T/F: display jump IDs in index.html
    'new_tabs_only' : False,    # T/F: do not open [0]th page in new win
    'new_wins_only' : False,    # T/F: open each page in new win (not tabs)
    'pause_time'    : 2.0,      # time (s) to pause to let pages load
    'verb'          : 1,        # verbosity level
}

# ==========================================================================

def apqc_list_to_dict(A):
    """APQC files are lists of lists of: QC block, rating and comment.
This functions takes this length-N list of length-3 lists and creates
a dictionary of the ratings.

Parameters
----------
A : list (of lists)
    list from apqc_*.json file, each element being a list of 3 items:
    QC_block, rating, comment

Returns
-------
D : dict
    dict of key-value pairs: '<QC_block> rating': <rating>

    """
    D = {}
    
    # [0]th element of A is essentially a header row, so skip
    for x in A[1:]: 
        key = x[0] + ' rating'
        val = x[1]
        D[key] = val

    return D

def merge_two_dicts(A, B):
    """Merge 2 dictionaries, such that B updates A, and return the merger
as a new dict.

Parameters
----------
A : dict
    the 'base' dictionary
B : dict
    the 'updating' dictionary

Return
------
C : dict
    the merger of A and B, where B's values get precedence

    """

    C = copy.deepcopy(A)
    C.update(B)
    return C

# ==========================================================================
# port-related functions

def find_open_port( portnum = DEF['portnum'], 
                    host    = DEF['host'],
                    nsearch = DEF['nsearch'],
                    verb    = 0 ):
    '''Find an open port for a given host.  Can check a provided number
    (and move upward through a specified interval on integers, if
    necessary) or just start from a default.

    Parameters
    ----------
    portnum : int
              first port number to check for availability
    host    : str
              hostname 
    nsearch : int
              number of ports to search through
    verb    : int
              verbosity level (0, 1, 2)

    Return
    ------
    portnum : int
              the first port number that appears to be open

    '''

    RETURN_BAD = -1, host

    if type(portnum) != int or portnum<=0 :
        print("** ERROR: need portnum to be an int that is >0")
        sys.exit(1)
    if type(nsearch) != int or nsearch<1 :
        print("** ERROR: need nsearch to be an int that is >1")
        sys.exit(1)

    max_port = portnum+nsearch

    if verb :
        print("++ Search for open port on host: '{}'".format(host))
        print("   Use interval: [{}, {})".format(portnum, max_port))

    while portnum < max_port :
        if verb > 1 :
            print("++ check port:", portnum)
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        if sock.connect_ex((host, portnum)):
            sock.close()
            break
        else:
            portnum += 1

    if portnum >= max_port :
        print("** ERROR: could not find open port")
        sys.exit(1)
    
    if verb :
        print("++ Found open port on host '{}': {}"
              "".format(host, portnum))

    return portnum

def construct_url( host, portnum, rem_path, 
                   jump_to=None ):
    '''Take the input hostname/number, port number and 'remainder' path
    from where the server started running, and construct a string for
    the local web address to open in the browser.

    The output URL will be formed as:
    
      http://host:portnum/rem_path
      http://host:portnum/rem_path#jump_to

    Parameters
    ----------
    host     : str
               hostname 
    portnum  : int
               first port number to check for availability
    rem_path : str
               the final part of the pathname (after the port number)
    jump_to  : str
               name of a location/hash to jump to in APQC HTML page

    Return
    ------
    url      : str
               a string describing the local path to open up for the
               given inputs

    '''

    dpieces = {
        'host'     : host,
        'portnum'  : portnum,
        'rem_path' : rem_path,
    }

    url = f'''http://{host}:{portnum}/{rem_path}'''.format( **dpieces )

    # add a hash/location?
    if jump_to :
        url+= '''#{}'''.format(jump_to)

    return url

# ==========================================================================

def disp_jump_ids_file(all_inpath):
    """From list of input index.html files, display list of possible IDs
for jumping, which is stored in extra_info.

"""
    
    # bc of how argparse works, if no infiles are input, then:
    # all_inpath == [[]].  So, it is never an empty list, and check
    # len of zeroth element to see if none were input
    if not(len(all_inpath[0])) :
        print("** ERROR: no index.html files were input, so I cannot list "
              "any IDs.\n"
              "   Please use '-infiles ..' to input at least one index.html "
              "file.")
        sys.exit(1)

    is_valid = verify_all_paths_to_html(all_inpath)
    if not(is_valid) :
        print("** ERROR: invalid input paths, cannot proceed")
        sys.exit(1)

    common_abs_path, rem_html_list = \
        find_common_and_remainder_paths(all_inpath, min_rem_len=2)

    # get full path to index.html
    ppp = common_abs_path + '/' + rem_html_list[0]
    # get full path *except* index.html
    qqq = '/'.join(ppp.split('/')[:-1])
    # get file of interest
    rrr = qqq + '/' + 'extra_info/list_ids.txt'
    # check that it exists
    if not(os.path.isfile(os.path.expanduser(rrr))) :
        print("+* WARN: cannot find file listing IDs:", rrr)
        return 1
    
    fff = open(rrr, 'r')
    X   = fff.read()
    fff.close()
    print('\n' + X)

    return 0

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
        print("** ERROR: need to a string of a path name")
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
                                     remove_dup = True, verb = 0 ):
    """Input a list of N paths (relative or absolute).  This program
    converts each to an absolute path and outputs two things:
      + the 'greatest common' absolute path among that list (which might 
        be further constrained by specifying a minimum length of remainder
        paths)
      + an N-length list of the 'remainder' paths for each index.html.

    One also can control the minimal length of remainder paths (for
    specific APQC utility).  For example, open_apqc.py calls *will be
    assumed* to use min_rem_len=2.

    By default, duplicate paths are removed.

    Parameters
    ----------
    inp_path_list : list (of str)
             list of rel or abs paths 
    min_rem_len : int
             minimum path length of the remainder files.  This places a
             constraint on the common path
    remove_dup : bool
             remove instances of duplicated remainder paths.  Will remove
             later occurrences in the list
    verb : int
             verbosity level

    Return
    -------
    common_abs_path : str
             the common (absolute) path of the input list of paths 
    rem_path_list : list (of str)
             list of rel paths to each index.html

    """
    
    RETURN_IF_BAD = "", []

    if type(inp_path_list) != list :
        print("** ERROR: need to input a list of paths")
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
            if verb > 1 :
                print("++ Adjusting common/rem paths by {} steps"
                      "".format(nshift))
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

    if remove_dup :
        rem_path_set = set(rem_path_list)
        nset  = len(rem_path_set)
        nlist = len(rem_path_list)
        if nset != nlist :
            diff = nlist - nset
            # this should create new list of unique items, preserving
            # order (higher index duplicates are removed)
            rem_path_list = list(dict.fromkeys(rem_path_list).keys())
            nnew = len(rem_path_list)
            print("+* WARNING: removing duplicates in list".format(diff))
            print("++ length pre-dup removal : {}".format(nlist))
            print("++ number of duplicates   : {}".format(diff))
            print("++ length post-dup removal: {}".format(nnew))

    return common_abs_path, rem_path_list

def find_other_json_for_each_index_html(common_abs_path, rem_html_list):
    """Each index.html should be accompanied by specific JSONs.  For the
list of index.html files the user has input, create the lists of those
accompanying JSONs.

Parameters
----------
common_abs_path : str
    the common (absolute) path of the input list of paths 
rem_html_list : list (of str)
    list of rel paths to each index.html

Return
-------
rem_apqc_json_list : list (of str)
    list of rel paths to each apqc_*.json, accompanying each 
    index.html
rem_ssrev_json_list : list (of str)
    list of rel paths to each exta_info/out.ss_review.*.json,
    ccompanying each index.html

    """

    RETURN_IF_BAD = [], []

    if type(rem_html_list) != list :
        print("** ERROR: need to input a list of paths 'rem_html_list'")
        return RETURN_IF_BAD
    if type(common_abs_path ) != str :
        print("** ERROR: need to input a str pathname 'common_ab_path'")
        return RETURN_IF_BAD

    N = len(rem_html_list)

    if not(N) :
        print("+* WARNING: input list appears to be empty")
        return RETURN_IF_BAD

    # create matched list to apqc*json files
    rem_apqc_json_list  = []
    rem_ssrev_json_list = []
    for path in rem_html_list :
        gpath = common_abs_path
        if path :
            gpath += '/' + path

        # glob with full path to apqc json
        jstr = "apqc_*.json"
        json_list  = glob.glob(gpath.replace("index.html", jstr))
        njson_list = len(json_list)
        if njson_list != 1 :
            print("** ERROR: found wrong number of possible {} files for '{}'"
                  "".format(jstr, path))
            print("   Should have exactly 1, but here had {}."
                  "".format(njson_list))
            return RETURN_IF_BAD
        rem_apqc_json = os.path.relpath(json_list[0], start=common_abs_path)
        rem_apqc_json_list.append(rem_apqc_json)

        # glob with full path to ssrev json
        jstr = "extra_info/out.ss_review.*.json"
        json_list  = glob.glob(gpath.replace("index.html", jstr))
        njson_list = len(json_list)
        if njson_list != 1 :
            print("** ERROR: found wrong number of possible {} files for '{}'"
                  "".format(jstr, path))
            print("   Should have exactly 1, but here had {}."
                  "".format(njson_list))
            return RETURN_IF_BAD
        rem_ssrev_json = os.path.relpath(json_list[0], start=common_abs_path)
        rem_ssrev_json_list.append(rem_ssrev_json)

    return rem_apqc_json_list, rem_ssrev_json_list


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
        print("** ERROR: need to input a list of paths")
        return RETURN_IF_BAD

    N = len(inp_path_list)

    if not(N) :    return RETURN_IF_BAD

    # run the gauntlet
    for path in inp_path_list:
        if not(path.endswith('index.html')) :
            print("** ERROR: path+file name '{}' does not end with index.html"
            "".format(path))
            return RETURN_IF_BAD
        if not(os.path.isfile(os.path.expanduser(path))) :
            print("** ERROR: path+file name '{}' does not exist"
            "".format(path))
            return RETURN_IF_BAD

    return 1


# =========================================================================
# =========================================================================

if __name__ == "__main__":

    print("Ceci n'est pas un programme principal.")
