#!/usr/bin/env python

# Read in and parse options for the new retroicorLauren.py program.
# 
# ==========================================================================

version = '1.0'

# ==========================================================================

import sys
import os
import copy
import json
import textwrap
import subprocess as     SP
import argparse   as     argp
from   datetime   import datetime
from   platform   import python_version_tuple
import borrow_afni_util  as BAU

# threshold values for some floating point comparisons
EPS_TH = 1.e-3

# ==========================================================================
# PART_01: default parameter settings

# default outdir name
now      = datetime.now() # current date and time
odir_def = now.strftime("retro_%Y-%m-%d-%H-%M-%S")

# Each key in DEF should have an opt listing below in argparse, and
# vice versa. 

DEF = {
    'resp_file'         : None,      # (str) fname for resp data
    'card_file'         : None,      # (str) fname for card data
    'phys_file'         : None,      # (str) fname of physio input data
    'phys_json'         : None,      # (str) fname of json file
    'freq'              : None,      # (float) freq, in Hz
    'num_slices'        : None,      # (int) number of MRI vol slices
    'volume_tr'         : None,      # (float) TR of MRI
    'num_time_pts'      : None,      # (int) Ntpts (e.g., len MRI time series)
    'start_time'        : None,      # (float) 
    'slice_times'       : None,      # (list) slice times
    'slice_pattern'     : None,      # (str) code or file for slice timing
    'out_dir'           : odir_def,  # (str) output dir name
    'prefix'            : 'physio',  # (str) output filename prefix
    'fir_order'         : 40,        # (int?) FIR order 
    'no_rvt_out'        : False,     # (bool) do not output RVT info
    'no_card_out'       : False,     # (bool) do not output card info
    'no_resp_out'       : False,     # (bool) do not output resp info
    'font_size'         : 10,        # (float) font size for plots 
    'phase_offset'      : 0.0,       # (float) offset added to initial phase
    'ab_disp'           : False,     # (bool) 
    'ab_out'            : False,     # (bool)
    'niml'              : False,     # (bool)
    'show_graph_level'  : 0,         # (int) amount of graphs to show
    'save_graph_level'  : 1,         # (int) amount of graphs to save
    'verbose'           : 0,         # (int) verbosity level
    'demo'              : False,     # (bool) show demo?
    'dev'               : False,     # (bool) work in dev mode?
    'debug'             : False,     # (bool) debug mode
    'disp_all_slice_patterns' : False, # (bool) display known sli patterns
    'disp_all_opts'     : False,     # (bool) display opts for this prog
    'ver'               : False,     # (bool) do show ver num?
    'help'              : False,     # (bool) do show help in term?
    'hview'             : False,     # (bool) do show help in text ed?
}

# list of lists of corresponding phys_json and args_dict entries,
# respectively; for each, there is also an EPS value for how to
# compare them if needing to reconcile command line opt values with a
# read-in value; more can be added over time
ALL_JA_MATCH = [
    ['SamplingFrequency', 'freq', EPS_TH],
    ['StartTime', 'start_time', EPS_TH],
]

AJM_str = "{:20s}   {:15s}   {:9s}\n".format('JSON KEY', 'ARG OPT', 'EPS VAL')
for ii in range(len(ALL_JA_MATCH)):
    sss = "{:20s}   {:15s}   {:.3e}\n".format(ALL_JA_MATCH[ii][0],
                                              ALL_JA_MATCH[ii][1],
                                              ALL_JA_MATCH[ii][2])
    AJM_str+= sss
# --------------------------------------------------------------------------
# sundry other items

verb = 0

dent = '\n' + 5*' '

help_dict = {
    'ddashline' : '='*76,
    'ver'       : version,
}

# ========================================================================== 
# PART_02: helper functions

def parser_to_dict(parser, verb=0):
    """Convert an argparse parser object to a dictionary of key (=opt) and
value pairs.  
    
Parameters
----------
parser : argparse.ArgumentParser
    object from parsing program options
verb : int
    verbosity level whilst working

Returns
-------
args_dict : dict
    dictionary whose keys are option names and values are the
    user-entered values (which might still need separate interpreting
    later)

    """

    # get args obj, and make a dict out of it
    args      = parser.parse_args()
    args_dict = vars(args)

    # each value in the args_dict is a list of something---extract that
    # something.  NB: this affects the type we read things in as, above.
    # We can parse below on a per-opt basis (e.g., turn a string into a
    # list of float)
    for key in args_dict.keys():
        # this condition is needed bc when a boolean flag is used, its
        # value will NOT be a list, magically, but everything else
        # appears to be
        if type(args_dict[key]) == list :
            args_dict[key] = args_dict[key][0]
        else:
            if verb:
                print("++ non-list option key -> value: ", 
                      key, '->', args_dict[key])

    return args_dict

def compare_keys_in_two_dicts(A, B, nameA=None, nameB=None):
    """Compare sets of keys between two dictionaries A and B (which could
have names nameA and nameB, when referring to them in output text).

Parameters
----------
A : dict
    a dictionary
B : dict
    a dictionary
nameA : str
    optional name for referring to dict A when reporting
nameB : str
    optional name for referring to dict B when reporting

Returns
-------
DIFF_KEYS : int
    integer encoding whether there is a difference in the set of keys
    in A and B:
      0 -> no difference
      1 -> difference

    """

    DIFF_KEYS = 0

    if not(nameA) :    nameA = 'A'
    if not(nameB) :    nameA = 'B'

    # simple count
    na = len(A)
    nb = len(B)

    if na != nb :
        DIFF_KEYS = 1
        print("** ERROR: number of keys in {} '{}' and in {} '{}' "
              "do not match.\n"
              "          This is a programming/dev issue."
              "".format(nameA, na, nameB, nb))

    # detailed check per opt class
    setA = set(A.keys())
    setB  = set(B.keys())

    missA = list(setB.difference(setA))
    missB = list(setA.difference(setB))

    if len(missA) :
        DIFF_KEYS = 1
        missA.sort()
        str_missA = ', '.join(missA)
        print("** ERROR: keys in {} that are missing in {}:\n"
              "          {}".format(nameB, nameA, str_missA))

    if len(missB) :
        DIFF_KEYS = 1
        missB.sort()
        str_missB = ', '.join(missB)
        print("** ERROR: keys in {} that are missing in {}:\n"
              "          {}".format(nameA, nameB, str_missB))

    return DIFF_KEYS

def check_simple_opts_to_exit(args_dict, parser):
    """Check for simple options, after which to exit, such as help/hview,
ver, disp all slice patterns, etc.

Parameters
----------
args_dict : dict
    a dictionary of input options (=keys) and their values

Returns
-------
int : int
    return 1 on the first instance of a simple opt being
    found, else return 0.

"""

    # if nothing or help opt, show help
    if args_dict['help'] :
        parser.print_help()
        return 1

    # hview functionality
    if args_dict['hview'] :
        print("hview on")

        prog = parser.prog 
        acmd = 'apsearch -view_prog_help {}'.format( prog )
        check_info = SP.Popen(acmd, shell=True, 
                              stdout=SP.PIPE, stderr=SP.PIPE,
                              close_fds=True)
        so, se = check_info.communicate() 

        if se :
            print("-- no hview--")
            # act like simple help disp
            parser.print_help()

        return 1

    # display program version
    if args_dict['ver'] :
        print(version)
        return 1

    # slice patterns, from list somewhere
    if args_dict['disp_all_slice_patterns'] :
        lll = BAU.g_valid_slice_patterns
        lll.sort()
        print("{}".format('\n'.join(lll)))
        return 1

    # all opts for this program, via DEF list
    if args_dict['disp_all_opts'] :
        lll = list(args_dict.keys())
        lll.sort()
        print("-{}".format('\n-'.join(lll)))
        return 1



    return 0

def read_slice_pattern_file(fname, verb=0):
    """Read in a text file fname that contains a slice timing pattern.
That pattern must be either a single row or column of (floating point)
numbers.

Parameters
----------
fname : str
    filename of slice timing info to be read in

Returns
-------
slice_times : list (of floats)
    a list of floats, the slice times

    """

    BAD_RETURN = []

    if not(os.path.isfile(fname)) :
        print("** ERROR: {} is not a file (to read for slice timing)"
              "".format(fname))
        return BAD_RETURN

    try:
        fff = open(fname, 'r')
        X   = fff.readlines()
        fff.close()
    except:
        print("** ERROR opening {} (to read for slice timing)"
              "".format(fname))
        return BAD_RETURN

    # get list of floats, and length of each row when reading
    N = 0
    all_num = []
    all_len = []
    for ii in range(len(X)):
        row = X[ii]
        rlist = row.split()
        if rlist :
            N+= 1
            try:
                # use extend so all_num stays 1D
                all_num.extend([float(rr) for rr in rlist])
                all_len.append(len(rlist))
            except:
                print("** ERROR: badness in float conversion within "
                      "slice timing file {}".format(fname))
                print("   Bad line {} is: '{}'".format(ii+1, row))
                return BAD_RETURN
    
    if not(N) :
        print("** ERROR: no data in slice timing file {}?".format(fname))
        return BAD_RETURN

    M = max(all_len)  # (max) number of cols

    if verb :
        print("++ Slice timing file {} has {} rows and {} columns"
              "".format(fname, N, M))

    if not(N==1 or M==1) :
        print("** ERROR: slice_pattern file {} is not Nx1 or 1xN.\n"
              "   Its dims of data are: nrow={},  max_ncol={}"
              .format(fname, N, M))
        return BAD_RETURN

    # finally, after more work than we thought...
    return all_num

def read_json_to_dict(fname):
    """Read in a text file fname that is supposed to be a JSON file and
output a dictionary.

Parameters
----------
fname : str
    JSON filename

Returns
-------
jdict : dict
    dictionary form of the JSON

    """
    
    BAD_RETURN = {}

    if not(os.path.isfile(fname)) :
        print("** ERROR: cannot read file: {}".format(fname))
        return BAD_RETURN

    with open(fname, 'rt') as fff:
        jdict = json.load(fff)

    return jdict

def reconcile_phys_json_with_args(jdict, args_dict, verb=None):
    """Go through the jdict created from the command-line given phys_json,
and pull out any pieces of information like sampling freq, etc. that
should be added to the args_dict (dict of all command line opts
used). These pieces of info can get added to the args_dict, but they
also have to be checked against possible conflict from command line opts.

Matched partners include:
{AJM_str}

The jdict itself gets added to the output args_dict2 as a value for
a new key 'phys_json_dict', for possible later use.

Parameters
----------
jdict : dict
    a dictionary from the phys_json input from the user
args_dict : dict
    the args_dict of input opts.

Returns
-------
BAD_RECON : int
    integer signifying bad reconiliation of files (> 1) or a
    non-problematic one (= 0)
args_dict2 : dict
    copy of input args_dict, that may be augmented with other info.

    """ 

    BAD_RETURN = 1, {}

    if not(jdict) :    return BAD_RETURN

    args_dict2 = copy.deepcopy(args_dict)

    # add known items that might be present
    for ja_match in ALL_JA_MATCH:
        jname   = ja_match[0]
        aname   = ja_match[1]
        eps_val = ja_match[2]
        if jname in jdict :
            val_json = jdict[jname]
            val_args = args_dict2[aname]
            if val_args != None :
                if abs(val_json - val_args) > eps_val :
                    print("** ERROR: inconsistent JSON '{}' = {} and "
                          " input arg '{}' = {}"
                          "".format(jname, val_json, aname, val_args))
                    return BAD_RETURN
                else:
                    print("+* WARN: start time provided in two ways, but "
                          " it may be OK because they are consistent (at "
                          "eps={}):\n"
                          "   JSON '{}' = {} and input arg '{}' = {}"
                          "".format(eps_val, jname, val_json, aname, val_args))
            else:
                args_dict2[aname] = val_json

    # and add in this JSON to the args dict, so it is only ever read
    # in once
    args_dict2['phys_json_dict'] = copy.deepcopy(jdict)

    return 0, args_dict2

# ... and needed with the above to insert a variable into the docstring
reconcile_phys_json_with_args.__doc__ = \
    reconcile_phys_json_with_args.__doc__.format(AJM_str=AJM_str)

# ========================================================================== 
# PART_03: setup argparse help and arguments/options

help_str_top = '''
Overview ~1~

This program creates slice-based regressors for regressing out
components of cardiac and respiratory rates, as well as the
respiration volume per time (RVT).

Much of the calculations are based on the following paper (GLR00):

  Glover GH, Li TQ, Ress D (2000). Image-based method for
  retrospective correction of physiological motion effects in fMRI:
  RETROICOR. Magn Reson Med 44(1):162-7. doi:
  10.1002/1522-2594(200007)44:1<162::aid-mrm23>3.0.co;2-e. PMID:
  10893535.


{ddashline}

Options ~1~

'''.format(**help_dict)

help_str_epi = '''
{ddashline}

Notes on usage ~1~

***

{ddashline}

Examples ~1~

  1) 

  2) 

  3) 

{ddashline}
written by: Peter Lauren (SSCC, NIMH, NIH, USA)

{ddashline}
'''.format(**help_dict)

# ========================================================================== 
# ============================== input stuff ===============================

# keep track of all opts over time, make sure it matches default list 
odict = {}

# unused right now, but could be used to control spacing
formatter = lambda prog: argp.HelpFormatter(prog, 
                                            indent_increment=2,
                                            max_help_position=12,
                                            width=80)

# get args
parser = argp.ArgumentParser( prog=str(sys.argv[0]).split('/')[-1],
                              add_help=False,
                              #formatter_class=argp.RawDescriptionHelpFormatter,
                              formatter_class=argp.RawTextHelpFormatter,
                              #formatter_class=formatter,
                              description=textwrap.dedent(help_str_top),
                              epilog=textwrap.dedent(help_str_epi) )

opt = '''resp_file'''
hlp = '''Path to one respiration data file'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''card_file'''
hlp = '''Path to one cardiac data file'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''phys_file'''
hlp = '''BIDS-formatted physio file in tab-separated format. May be
gzipped'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''phys_json'''
hlp = '''BIDS-formatted physio metadata JSON file. If not specified, the
JSON corresponding to the '-phys_file ..'  will be loaded'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''freq'''
hlp = '''Physiological signal sampling frequency (in Hz)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''num_slices'''
hlp = '''Integer number of slices in MRI volume'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''volume_tr'''
hlp = '''MRI volume's repetition time (TR), which defines the time interval
between consecutive volumes (in s) '''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''start_time'''
hlp = '''*** add *** (in s)
'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''num_time_pts'''
hlp = '''Integer number of time points to have in the output (should likely
match MRI time series length)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''out_dir'''
hlp = '''Output directory name (can include path)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''prefix'''
hlp = '''Prefix of output filenames, without path (def: {dopt})
'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''slice_times'''
hlp = '''Slice time values (space separated list of numbers)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs='+', type=str) # parse later

opt = '''slice_pattern'''
hlp = '''Slice timing pattern code (def: {dopt}). The following codes are
allowed *** list? ***'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs='+', type=str)

opt = '''fir_order'''
hlp = '''*** add *** (def: {dopt})
'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''phase_offset'''
hlp = '''Time added to initial offset, in sec (def: {dopt})
'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''no_card_out'''
hlp = '''Turn off output of cardiac regressors'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''no_resp_out'''
hlp = '''Turn off output of respiratory regressors'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''no_rvt_out'''
hlp = '''Turn off output of RVT regressors
'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''font_size'''
hlp = '''Font size used for graphics (def: {dopt})
'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''save_graph_level'''
hlp = '''Integer value for one of the following behaviors:
0 - Do not save graphs
1 - Save end results (card and resp peaks, final RVT)
2 - Save end results and intermediate (band-pass filter)
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''show_graph_level'''
hlp = '''Integer value for one of the following behaviors:
0 - Do not show graphs
1 - Show end results (card and resp peaks, final RVT)
2 - Show end results and intermediate (band-pass filter)
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''ab_disp'''
hlp = '''Output the a and b coefficients from GLR00 to terminal'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''ab_out'''
hlp = '''Output the time series based on the a and b coefficients from
GLR00'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''niml'''
hlp = '''Output ***what?*** in NIML format, instead of CSV format'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''verbose'''
hlp = '''Integer values to control verbosity level 
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''demo'''
hlp = '''Enter 'demonstration mode': run example'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''dev'''
hlp = '''Enter 'development mode': *** describe'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''debug'''
hlp = '''Enter 'debug mode': drop into pdb upon an exception'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

# ---- help-y stuff

opt = '''disp_all_slice_patterns'''
hlp = '''Display all allowed slice pattern names'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''disp_all_opts'''
hlp = '''Display all options for this program'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''ver'''
hlp = '''Display program version number'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''help'''
hlp = '''Display help text in terminal'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''hview'''
hlp = '''Display help text in a text editor (AFNI functionality)
'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

# --------------------------------------------------------------------------
# programming check: are all opts and defaults accounted for?

have_diff_keys = compare_keys_in_two_dicts( odict, DEF,
                                            nameA = 'odict',
                                            nameB = 'DEF' )

if have_diff_keys :
    print("** ERROR: exiting because of opt name setup failure")
    sys.exit(1)

# =========================================================================
# PART_04: process opts slightly, checking if all required ones are
# present (~things with None for def), and also note that some pieces
# of info can come from the phys_json, so open and interpret that (and
# check against conflicts!)

def check_required_args(args_dict):
    """A primary check of the user provided arguments (as well as the
potential update of some).  

This first checks whether everything that *should* have been provided
actually *was* provided, namely for each opt that has a default None
value (as well as some strings that could have been set to '').  In
many cases, a failure here will lead to a direct exit.

This also might read in a JSON of info and use that to populate
args_dict items, updating them.  It will also then save that JSON to a
new field, phys_json_dict.

Parameters
----------
args_dict : dict
    The dictionary of arguments and option values.

Returns
-------
args_dict2 : dict
    A potentially updated args_dict (if no updates happen, then just
    a copy of the original).

    """

    verb = args_dict['verbose']

    args_dict2 = copy.deepcopy(args_dict)

    if not(args_dict2['card_file'] or args_dict2['resp_file']) and \
       not(args_dict2['phys_file'] and args_dict2['phys_json']) :
        print("** ERROR: no physio inputs provided. Allowed physio inputs:\n"
              "   A) '-card_file ..', '-resp_file ..' or both."
              "   B) '-phys_file ..' and '-phys_json ..'.")
        sys.exit(4)

    # for any filename that was provided, check if it actually exists
    # (slice_pattern possible filename checked below)
    all_fopt = [ 'card_file', 'resp_file', 'phys_file', 'phys_json' ]
    for fopt in all_fopt:
        if args_dict2[fopt] != None :
            if not(os.path.isfile(args_dict2[fopt])) :
                print("** ERROR: no {} '{}'".format(fopt, args_dict2[fopt]))
                sys.exit(5)

    # deal with json for a couple facets: getting args_dict2 info, and
    # making sure there are no inconsistencies (in case both JSON and opt
    # provide the same info)
    if args_dict2['phys_json'] :
        jdict = read_json_to_dict(args_dict2['phys_json'])
        if not(jdict) :
            print("** ERROR: JSON unreadable or empty")
            sys.exit(5)

        # jdict info can get added to args_dict; also want to make sure it
        # does not conflict, if items were entered with other opts
        check_fail, args_dict2 = reconcile_phys_json_with_args(jdict, 
                                                               args_dict2)
        if check_fail :
            print("** ERROR: issue using the JSON")
            sys.exit(5)

    if not(args_dict2['num_slices']) :
        print("** ERROR: must provide '-num_slices ..' information")
        sys.exit(4)

    if not(args_dict2['num_time_pts']) :
        print("** ERROR: must provide '-num_time_pts ..' information")
        sys.exit(4)

    if not(args_dict2['volume_tr']) :
        print("** ERROR: must provide '-volume_tr ..' information")
        sys.exit(4)

    if not(args_dict2['freq']) :
        print("** ERROR: must provide '-freq ..' information")
        sys.exit(4)

    if not(args_dict2['prefix']) :
        print("** ERROR: must provide '-prefix ..' information")
        sys.exit(4)

    if not(args_dict2['out_dir']) :
        print("** ERROR: must provide '-out_dir ..' information")
        sys.exit(4)

    if not(args_dict2['slice_times']) and not(args_dict2['slice_pattern']) :
        print("** ERROR: must exactly one of either slice_times or "
              "slice_pattern")
        sys.exit(4)

    if args_dict2['slice_times'] and args_dict2['slice_pattern'] :
        print("** ERROR: must use only one of either slice_times or "
              "slice_pattern")
        sys.exit(4)

    return args_dict2


def interpret_args(args_dict):
    """Interpret the user provided arguments (and potentially update
some).  This also checks that entered values are valid, and in some
cases a failure here will lead to a direct exit.

Parameters
----------
args_dict : dict
    The dictionary of arguments and option values.

Returns
-------
args_dict2 : dict
    A potentially updated args_dict (if no updates happen, then just
    a copy of the original).

    """

    args_dict2 = copy.deepcopy(args_dict)

    if args_dict2['start_time'] == None :
        print("++ No start time provided; will assume it is 0.0.")
        args_dict2['start_time'] = 0.0

    if args_dict2['slice_times'] :
        # interpret string to be list of floats
        IS_BAD = 0

        L = args_dict2['slice_times'].split()
        try:
            slice_times = [float(ll) for ll in L]
            args_dict2['slice_times'] = copy.deepcopy(slice_times)
        except:
            print("** ERROR interpreting slice times")
            IS_BAD = 1

        if IS_BAD :
            sys.exit(1)

    if args_dict2['slice_pattern'] :
        # if pattern, check if it is allowed; elif if is a file, check if
        # it exists *and* use it to fill in args_dict2['slice_times'];
        # else, whine

        pat = args_dict2['slice_pattern']
        if pat in BAU.g_valid_slice_patterns :
            print("++ Found slice_pattern '{}' in allowed list".format(pat))
        elif os.path.isfile(pat) :
            print("++ Found slice_pattern '{}' exists as a file".format(pat))
            slice_times = read_slice_pattern_file(pat, verb=verb)
            if not(slice_times) :
                sys.exit(7)
            args_dict2['slice_times'] = slice_times
        else:
            print("** ERROR: could not match slice_pattern '{}' as "
                  "either a recognized pattern or file".format(pat))
            sys.exit(3)

    if '/' in args_dict2['prefix'] :
        print("** ERROR: Cannot have path information in '-prefix ..'\n"
              "   Use '-out_dir ..' for path info instead")
        sys.exit(4)

    # successful navigation
    return args_dict2

# *** temporary way of calling, like a main prog, here
# --------------------------------------------------------------------------
# null case of no opts: just show help and quit

# We do this check separately, because in this case, each item in
# args_dict is *not* a list containing what we want, but just is that
# thing itself.  That confuses the means for checking it, so treat
# that differently.

if len(sys.argv) == 1 :
    parser.print_help()
    sys.exit(0)

# -------------------------------------------------------------------------
# case of >=1 opt being used: parse!

# get all opts and values as a dict
args_dict = parser_to_dict( parser )

# check for simple-simple cases with a quick exit: ver, help, etc.
have_simple_opt = check_simple_opts_to_exit(args_dict, parser)
if have_simple_opt :
    sys.exit(0)

args_dict = check_required_args(args_dict)
args_dict = interpret_args(args_dict)



# ================================ main =====================================

if __name__ == "__main__":

    print("++ DONE.  Goodbye.")

    sys.exit(0)
