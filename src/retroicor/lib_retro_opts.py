#!/usr/bin/env python

# Read in and parse options for the new retroicorLauren.py program.
# 
# ==========================================================================

#version = '1.0'
version = '1.1'   # add remove_val_list, for some vals to get replaced

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

sys.path.append("/home/ptaylor/afni_build/src/linux_ubuntu_16_64_glw_local_shared")

from   afnipy     import afni_base as BASE   # or add to BAU?

# threshold values for some floating point comparisons
EPS_TH = 1.e-3

# min beats or breaths per minute
DEF_min_bpm_card = 25.0
DEF_min_bpm_resp = 6.0   

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
    'slice_times'       : None,      # (list) slice times
    'slice_pattern'     : None,      # (str) code or file for slice timing
    'freq'              : None,      # (float) freq, in Hz
    'num_slices'        : None,      # (int) number of MRI vol slices
    'volume_tr'         : None,      # (float) TR of MRI
    'num_time_pts'      : None,      # (int) Ntpts (e.g., len MRI time series)
    'start_time'        : None,      # (float) leave none, bc can be set in json
    'dset_epi'          : None,      # (str) name of MRI dset, for vol pars
    'out_dir'           : odir_def,  # (str) output dir name
    'prefix'            : 'physio',  # (str) output filename prefix
    'do_fix_nan'        : False,     # (str) fix/interp NaN in physio
    'do_fix_null'       : False,     # (str) fix/interp null/missing in physio
    'do_fix_outliers'   : False,     # (list) fix/interp outliers
    'extra_fix_list'    : [],        # (list) extra values to fix
    'remove_val_list'   : [],        # (list) purge some values from ts
    'no_rvt_out'        : False,     # (bool) do not output RVT info
    'no_card_out'       : False,     # (bool) do not output card info
    'no_resp_out'       : False,     # (bool) do not output resp info
    'min_bpm_resp'      : DEF_min_bpm_resp, # (float) min breaths per min
    'min_bpm_card'      : DEF_min_bpm_card, # (float) min beats per min
    'font_size'         : 10,        # (float) font size for plots 
    'do_calc_ab'        : False,     # (bool) calc a,b coeffs and use
    'do_save_ab'        : False,     # (bool) save a,b coeffs to file
    'niml'              : False,     # (bool)
    'show_graph_level'  : 0,         # (int) amount of graphs to show
    'save_graph_level'  : 1,         # (int) amount of graphs to save
    'verb'              : 0,         # (int) verbosity level
    'demo'              : False,     # (bool) show demo?
    'debug'             : False,     # (bool) debug mode
    'disp_all_slice_patterns' : False, # (bool) display known sli patterns
    'disp_all_opts'     : False,     # (bool) display opts for this prog
    'ver'               : False,     # (bool) do show ver num?
    'help'              : False,     # (bool) do show help in term?
    'hview'             : False,     # (bool) do show help in text ed?
    'RVT_lags'          : [],        # (dict) start, end and number of RVTs 
}

# list of keys for volume-related items, that will get parsed
# separately so we peel them off when parsing initial opts
vol_key_list = [
    'dset_epi',
    'volume_tr',
    'num_slices',
    'num_time_points',
    'slice_times',
    'slice_pattern',
]

# ---- sublists to check for properties ----

# list of lists of corresponding args_dict and phys_json entries,
# respectively; for each, there is also an EPS value for how to
# compare them if needing to reconcile command line opt values with a
# read-in value; more can be added over time
ALL_AJ_MATCH = [
    ['freq', 'SamplingFrequency', EPS_TH],
    ['start_time', 'StartTime', EPS_TH],
]

AJM_str = "    {:15s}   {:20s}   {:9s}\n".format('ARG/OPT', 'JSON KEY', 
                                                 'EPS VAL')
for ii in range(len(ALL_AJ_MATCH)):
    sss = "    {:15s}   {:20s}   {:.3e}\n".format(ALL_AJ_MATCH[ii][0],
                                                  ALL_AJ_MATCH[ii][1],
                                                  ALL_AJ_MATCH[ii][2])
    AJM_str+= sss

# for dset_epi matching; following style of aj_match, but key names
# don't differ and some things are integer.
ALL_EPIM_MATCH = [
    ['volume_tr',  EPS_TH],
    ['num_time_points', EPS_TH],
    ['num_slices', EPS_TH],
]
# extension of the above if the tested object is a list; the items in
# the list will be looped over and compared at the given eps
ALL_EPIM_MATCH_LISTS = [
    ['slice_times', EPS_TH ],
]

EPIM_str = "    {:15s}   {:9s}\n".format('ITEM', 'EPS VAL')
for ii in range(len(ALL_EPIM_MATCH)):
    sss  = "    {:15s}   {:.3e}\n".format(ALL_EPIM_MATCH[ii][0],
                                          ALL_EPIM_MATCH[ii][1])
    EPIM_str+= sss
for ii in range(len(ALL_EPIM_MATCH_LISTS)):
    sss  = "    {:15s}   {:.3e}\n".format(ALL_EPIM_MATCH_LISTS[ii][0],
                                          ALL_EPIM_MATCH_LISTS[ii][1])
    EPIM_str+= sss

# quantities that must be >= 0
all_quant_ge_zero = [
    'font_size',
    'freq',
    'min_bpm_card',
    'min_bpm_resp',
    'num_slices',
    'num_time_pts',
    'volume_tr',
]


# --------------------------------------------------------------------------
# sundry other items

verb = 0

dent = '\n' + 5*' '

help_dict = {
    'ddashline' : '='*76,
    'ver'       : version,
    'AJM_str'   : AJM_str,
}

# ========================================================================== 
# PART_02: helper functions

def parser_to_dict(parser, argv, verb=0):
    """Convert an argparse parser object to a dictionary of key (=opt) and
value pairs.  Also store the argv as a value of returned dict (key =
'argv').  There is an intermediate dictionary of volumetric-related
items that is output first, because parsing it is complicated.  That
will later get merged with the main args_dict
    
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
vol_dict : dict
    secondary dictionary whose keys are option names and values are
    the user-entered values (which might still need separate
    interpreting later) specifically related to the volume, which will
    get parsed separately and merged into the main dir later

    """

    # get args obj, and make a dict out of it
    args      = parser.parse_args(argv[1:])
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
            if len(args_dict[key]) == 1 :
                args_dict[key] = args_dict[key][0]
            else:
                # list-> str with space sep, to be split later
                args_dict[key] = ' '.join(args_dict[key])
        else:
            if verb:
                print("++ non-list option key -> value: ", 
                      key, '->', args_dict[key])

    args_dict['argv'] = copy.deepcopy(argv)

    # pop out the volumetric-related items, to parse separately, and
    # return later
    vol_dict = {}
    for key in vol_key_list:
        if key in args_dict :
            val = args_dict.pop(key)
            vol_dict[key] = copy.deepcopy(val)

    return args_dict, vol_dict

def compare_keys_in_two_dicts(A, B, nameA=None, nameB=None):
    """Compare sets of keys between two dictionaries A and B (which could
have names nameA and nameB, when referring to them in output text).

Parameters
----------
A : dict
    some dictionary
B : dict
    some dictionary
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
ver, disp all slice patterns, etc.  The parser is an included arg
because it has the help info to display, if called for.

Parameters
----------
args_dict : dict
    a dictionary of input options (=keys) and their values
parser : argparse.ArgumentParser
    object from parsing program options

Returns
-------
int : int
    return 1 on the first instance of a simple opt being found, else
    return 0

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
        tmp = disp_keys_sorted(DEF, pref='-')
        if not(tmp) :
            return 1

    # getting here means a mistake happened
    return 0

def disp_keys_sorted(D, pref=''):
    """Display a list of sorted keys from dict D, one per line.  Can
include a prefix (left-concatenated string) for each.

Parameters
----------
D : dict
    some dictionary
pref : str
    some string to be left-concatenated to each key

Returns
-------
SF : int
    return 0 if successful
    """

    if type(D) != dict :
        print("** ERROR: input D must be dict")
        sys.exit(13)
    
    all_key = [pref+str(x) for x in get_keys_sorted(D)]
    print('{}'.format('\n'.join(all_key)))

    return 0

def get_keys_sorted(D):
    """Return a list of sorted keys from dict D.

Parameters
----------
D : dict
    some dictionary

Returns
-------
L : list
    a sorted list (of keys from D)

    """

    if type(D) != dict :
        print("** ERROR: input D must be dict")
        sys.exit(13)

    L = list(D.keys())
    L.sort()
    return L


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

def read_dset_epi_to_dict(fname, verb=None):
    """Extra properties from what should be a valid EPI dset and output a
dictionary.

Parameters
----------
fname : str
    EPI dset filename

Returns
-------
epi_dict : dict
    dictionary of necessary EPI info

    """
    
    BAD_RETURN = {}

    if not(os.path.isfile(fname)) :
        print("** ERROR: cannot read file: {}".format(fname))
        return BAD_RETURN

    # initialize, and store dset name
    epi_dict = {}
    epi_dict['dset_epi'] = fname

    # get simple dset info, which should/must exist
    cmd = '''3dinfo -n4 -tr {}'''.format(fname)
    com = BASE.shell_com(cmd, capture=1, save_hist=0)
    stat = com.run()
    lll = com.so[0].split()
    try:
        nk = int(lll[2])
        nv = int(lll[3])
        tr = float(lll[4])
        
        epi_dict['num_slices']   = int(lll[2])
        epi_dict['num_time_pts'] = int(lll[3])
        epi_dict['volume_tr']    = float(lll[4])
    except:
        print("+* WARN: problem extracting info from dset_epi")
        return BAD_RETURN 

    # try getting timing info from this dset, which might not exist
    cmd = '''3dinfo -slice_timing {}'''.format(fname)
    com = BASE.shell_com(cmd, capture=1, save_hist=0)
    stat = com.run()
    lll = [float(x) for x in com.so[0].strip().split('|')]
    
    nslice = len(lll)
    if nk != nslice :
        print("** ERROR: number of slice_times in header ({}) "
              "does not match slice count in k-direction ({})"
              "".format(nslice, nk))
        sys.exit(10)

    epi_dict['slice_times'] = copy.deepcopy(lll)

    return epi_dict

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
    for aj_match in ALL_AJ_MATCH:
        aname   = aj_match[0]
        jname   = aj_match[1]
        eps_val = aj_match[2]
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
                    print("++ Reconciled: input info provided in two ways, "
                          "which is OK because they are consistent (at "
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
  RETROICOR. Magn Reson Med 44(1):162-7. PMID: 10893535. 
  doi: 10.1002/1522-2594(200007)44:1<162::aid-mrm23>3.0.co;2-e. 


{ddashline}

Options ~1~

'''.format(**help_dict)

help_str_epi = '''
{ddashline}

Notes on usage and inputs ~1~

* At least one of the following input option sets must be used:
  - '-card_file ..'
  - '-resp_file ..'
  - '-card_file ..' and '-resp_file ..'
  - '-phys_file ..' and '-phys_json'

* It is preferred to use:
  - '-dset_epi ..' 
  to provide EPI dset for which regressors will be made, to provide
  the volumetric information that would otherwise be provided with:
  - '-volume_tr ..'   
  - '-num_slices ..'  
  - '-num_time_pts ..'
  ... and the slice timing information

* If '-dset_epi ..' is not used to provide the slice timing (and other
  useful) volumetric information, then exactly one of the following
  input option must be used:
  - '-slice_times ..'
  - '-slice_patterns ..'

* Each of the following input options must be provided through some
  combination of phys_json file, dset_epi file, or the command line opts
  themselves:
  - '-freq ..'        
  - '-volume_tr ..'   
  - '-num_slices ..'  
  - '-num_time_pts ..'

* The following table shows which keys from 'phys_json' can be used to
  set (= replace) certain command line argument/option usage:
{AJM_str}
  The 'EPS VAL' shows the maximum difference tolerated between a
  JSON-provided key and an option-provided one, in case both exist in
  a command line call.  It would be better to avoid such dual-calling.

{ddashline}

Notes on input peculiarities ~1~

With Siemens physiological monitoring, values of 5000, 5003 and 6000 can be 
used as trigger events to mark the beginning or end of something, like the 
beginning of a TR.  The meanings, from the Siemens Matlab code are:
     5000 = cardiac pulse on
     5003 = cardiac pulse off
     6000 = cardiac pulse off
     6002 = phys recording on
     6003 = phys recording off

It appears that the number is inserted into the series, in which case,
5000 values could simply be removed rather than replaced by an
interpolation of the two adjacent values, using the option
'remove_val_list ..'.

{ddashline}

Examples 

  Example ~1~ 
    
    python ~/retroicor/retroicorTaylor.py                                    \\
        -card_file                      physiopy/test000c                    \\
        -num_slices                     33                                   \\
        -volume_tr                      2.2                                  \\
        -freq                           400                                  \\
        -do_fix_nan                                                          \\
        -num_time_pts                   17                                   \\
        -slice_pattern                  alt+z                                \\
        -extra_fix_list                 5000                                 \\
        -out_dir                        $outDir                              \\
        -prefix                         $prefix

  Example ~2~  
    
    python                                                                   \\
        retroicorTaylor.py                                                   \\
        -phys_file          physiopy/test003c.tsv.gz                         \\
        -phys_json          physiopy/test003c.json                           \\
        -num_slices         34                                               \\
        -volume_tr          2.2                                              \\
        -do_fix_nan                                                          \\
        -num_time_pts       34                                               \\
        -slice_pattern      alt+z                                            \\
        -extra_fix_list     5000                                             \\
        -out_dir            $outDir                                          \\
        -prefix             $prefix

  Example ~3~  

    python ~/retroicor/retroicorTaylor.py                                    \\
        -card_file       sub-005_ses-01_task-rest_run-1_physio-ECG.txt       \\
        -resp_file       sub-005_ses-01_task-rest_run-1_physio-Resp.txt      \\
        -num_slices      33                                                  \\
        -volume_tr       2.2                                                 \\
        -freq            50                                                  \\
        -do_fix_nan                                                          \\
        -num_time_pts    219                                                 \\
        -slice_pattern   alt+z                                               \\
        -extra_fix_list  5000                                                \\
        -out_dir         $outDir                                             \\
        -prefix          $prefix
    
{ddashline}
written by: Peter Lauren (SSCC, NIMH, NIH, USA)

{ddashline}
'''.format(**help_dict)

# ========================================================================== 
# ============================ the args/opts ===============================

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

opt = '''dset_epi'''
hlp = '''Accompanying EPI/FMRI dset to which the physio regressors will be
applied, for obtaining the volumetric parameters (namely, volume_tr,
num_slices, num_time_pts)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

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
hlp = '''Slice timing pattern code (def: {dopt}). Use
'-disp_all_slice_patterns' to see all allowed patterns.'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs='+', type=str)

opt = '''do_fix_nan'''
hlp = '''Fix (= replace with interpolation) any NaN values in the physio
time series (def: exit if any appears)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''do_fix_null'''
hlp = '''Fix (= replace with interpolation) any null or missing values in
the physio time series (def: exit if any appears)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''do_fix_outliers'''
hlp = '''Fix (= replace with interpolation) any outliers in the physio time
series (def: don't change them and continue)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''extra_fix_list'''
hlp = '''List of one or more values that will also be considered 'bad' if
they appear in the physio time series, and replaced with interpolated
values'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs='+', type=str) # parse later

opt = '''remove_val_list'''
hlp = '''List of one or more values that will removed (not interpolated: the
time series will be shorter, if any are found) if they appear in the
physio time series; this is necessary with some manufacturers'
outputs, see "Notes of input peculiarities," below.'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs='+', type=str) # parse later

opt = '''RVT_lags'''
hlp = '''This optional argument is followed the three values: the start
time, end time and number of RVTs.  The (unshifted single regressor) RVT sample 
times are currently 0, TR, 2*TR, 3*TR, etc.  By default we output 5 shifted 
versions of this single regressor, at offsets 0, -5, -10, -15, -20.  So the 
start time is the earliest offset, the end time is the latest offset, and the 
number of RVT's (or regressors) is how many total offsets to include.  So the 
default parameterization would be as:  -rvt_lags -20 0 5 .  It is supposed to
be as: -rvt_lags 0 20 5 (which includes a starting time of 0 s relative to the
sampled RVT regressors.)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=3, type=str) # parse later

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

opt = '''min_bpm_resp'''
hlp = '''Set the minimum breaths per minute for respiratory proc (def: {})
'''.format(DEF_min_bpm_resp)
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''min_bpm_card'''
hlp = '''Set the minimum beats per minute for cardiac proc (def: {})
'''.format(DEF_min_bpm_card)
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

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
2 - Save end results and intermediate (bandpass filter)
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''show_graph_level'''
hlp = '''Integer value for one of the following behaviors:
0 - Do not show graphs
1 - Show end results (card and resp peaks, final RVT)
2 - Show end results and intermediate (bandpass filter)
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''do_calc_ab'''
hlp = '''Calculate the a,b coefficients from GLR00, and use these in the
output time series (generally not needed)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''do_save_ab'''
hlp = '''Save the a,b coefficients from GLR00 in a file (generally not
needed'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''niml'''
hlp = '''Output ***what?*** in NIML format, instead of CSV format'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''verb'''
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

This function might likely grow over time as more options get added or
usage changes.

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

    verb = args_dict['verb']

    args_dict2 = copy.deepcopy(args_dict)

    if not(args_dict2['card_file'] or args_dict2['resp_file']) and \
       not(args_dict2['phys_file'] and args_dict2['phys_json']) :
        print("** ERROR: no physio inputs provided. Allowed physio inputs:\n"
              "   A) '-card_file ..', '-resp_file ..' or both."
              "   B) '-phys_file ..' and '-phys_json ..'.")
        sys.exit(4)

    # for any filename that was provided, check if it actually exists
    # (slice_pattern possible filename checked below)
    all_fopt = [ 'card_file', 'resp_file', 'phys_file', 'phys_json',
                 'dset_epi' ]
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

     # different ways to provide volumetric EPI info, and ONE must be used
    if not( args_dict2['volume_tr'] ) :
        print("** ERROR: must provide '-volume_tr ..' information")
        sys.exit(4)

    if not(args_dict2['num_slices']) :
        print("** ERROR: must provide '-num_slices ..' information")
        sys.exit(4)

    if not(args_dict2['num_time_pts']) :
        print("** ERROR: must provide '-num_time_pts ..' information")
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

    if not(args_dict2['slice_times']) :
        print("** ERROR: must provide slice_time info in some way")
        sys.exit(4)

    #if args_dict2['slice_times'] and args_dict2['slice_pattern'] :
    #    print("** ERROR: must use only one of either slice_times or "
    #          "slice_pattern")
    #    sys.exit(4)

    return args_dict2

def interpret_vol_info(vol_dict, verb=1):
    """This function takes a dictionary of all command line-entered items
that are/might be related to MRI acquistion, and will: 1)
expand/calculate any info (like slice times); 2) check info for
conflicts; 3) reduce info down (= reconcile items) where it is OK to do so.

The output of this function can be merged into the main arg_dicts.

Parameters
----------
vol_dict : dict
    The dictionary of volume-related arguments and option values,
    before any parsing

Returns
-------
vol_dict2 : dict
    A potentially updated vol_dict (if no updates happen, then just
    a copy of the original).  But likely this will have been parsed in 
    important ways

    """

    BAD_RETURN = {}

    # initialize what will be output dictionary
    vol_dict2 = {}

    # first see if a dset_epi has been entered, which might have a lot
    # of important information (and add to output dict); any/all EPI
    # info *might* be in here (at least at the time of writing)
    if 'dset_epi' in vol_dict and vol_dict['dset_epi'] :
        vol_dict2 = read_dset_epi_to_dict(vol_dict['dset_epi'], verb=verb)
        if not(vol_dict2) :
            print("** ERROR: dset_epi unreadable or problematic")
            sys.exit(5)
    else:
        vol_dict2['dset_epi'] = None

    # then check scalar values about volume properties from simple
    # command line opts; try to reconcile or add each (and add to
    # output dict)
    ndiff, nmerge = compare_dict_nums_with_merge(vol_dict2, vol_dict, 
                                                 L=ALL_EPIM_MATCH, 
                                                 do_merge=True, verb=1)
    if ndiff :
        print("** ERROR: inconsistent dset_epi and command line info")
        sys.exit(5)

    # next/finally, check about slice timing specifically, which might
    # use existing scalar values (from dset or cmd line, which would
    # be in vol_dict2 now) 
    if vol_dict['slice_times'] and vol_dict['slice_pattern'] :
        print("** ERROR: must use only one of either slice_times or "
              "slice_pattern")
        sys.exit(4)

    if vol_dict['slice_times'] :
        # the input cmd line string has not been split yet; interpret
        # this single string to be a list of floats, and replace it in
        # the dict

        L = vol_dict['slice_times'].split()
        try:
            # replace single string of slice times with list of
            # numerical values
            slice_times = [float(ll) for ll in L]
            vol_dict['slice_times'] = copy.deepcopy(slice_times)
        except:
            print("** ERROR interpreting slice times from cmd line")
            sys.exit(1)
    elif vol_dict['slice_pattern'] :
        # if pattern, check if it is allowed; elif it is a file, check
        # if it exists *and* use it to fill in
        # vol_dict['slice_times']; else, whine.  Use any supplementary
        # info from the output dict, but edit vol_dict slice times in
        # place

        pat = vol_dict['slice_pattern']
        if pat in BAU.g_valid_slice_patterns :
            print("++ Slice pattern from cmd line: '{}'".format(pat))
            # check with vol info in vol_dict2 (not in vol_dict) bc
            # vol_dict2 should be the merged superset of info
            slice_times = BAU.slice_pattern_to_timing(pat, 
                                                      vol_dict2['num_slices'],
                                                      vol_dict2['volume_tr'])
            if not(slice_times) :
                print("** ERROR: could not convert slice pattern to timing")
                sys.exit(8)
            vol_dict['slice_times'] = copy.deepcopy(slice_times)
        elif os.path.isfile(pat) :
            print("++ Found slice_pattern '{}' exists as a file".format(pat))
            slice_times = read_slice_pattern_file(pat, verb=verb)
            if not(slice_times) :
                print("** ERROR: translate slice pattern file to timing")
                sys.exit(7)
            vol_dict['slice_times'] = copy.deepcopy(slice_times)
        else:
            print("** ERROR: could not match provided slice_pattern '{}' as "
                  "either a recognized pattern or file".format(pat))
            sys.exit(3)

    # ... and now that we might have explicit slice times in vol_dict,
    # reconcile any vol['slice_times'] with vol_dict2['slice_times']
    if 'slice_times' in vol_dict and vol_dict['slice_times'] != None :
        if 'slice_times' in vol_dict2 and vol_dict2['slice_times'] != None :
            # try to reconcile
            ndiff = compare_list_items( vol_dict['slice_times'],
                                        vol_dict2['slice_times'],
                                        eps=EPS_TH )
            if ndiff :
                print("** ERROR: inconsistent slice times entered")
                sys.exit(5)
        else:
            # nothing to reconcile, just copy over
            vol_dict2['slice_times'] = copy.deepcopy(vol_dict['slice_times'])
    else:
        # I believe these cases hold
        if 'slice_times' in vol_dict2 and vol_dict2['slice_times'] != None :
            pass
        else:
            # this is a boring one, which will probably lead to an
            # error exit in a downstream check
            vol_dict2['slice_times'] = None

    # copy this over just for informational purposes
    if 'slice_pattern' in vol_dict :
        vol_dict2['slice_pattern'] = copy.deepcopy(vol_dict['slice_pattern'])

    return vol_dict2


def compare_dict_nums_with_merge(A, B, L=[], do_merge=True, verb=1):
    """Let A and B be dictionaries, each of arbitrary size.  Let L be a
list of lists, where each sublist contains the name of a possible key
(which would have a numerical value) and a tolerance for differencing
its value between A and B.  This function goes through L and 1) sees
if that element exists in B; 2) if yes, sees if it also exists in A;
3) if yes, sees if they are the same to within allowed tolerance and
(if do_merge==True) else if no, adds that value to B.

Parameters
----------
A : dict
    dict of arbitrary size, which main contain numerical elements
    listed in L
B : dict
    dict of arbitrary size, which main contain numerical elements
    listed in L
L: list
    list of 2-element sublists, each of which contains the str name of
    a parameter to search for in A and B, and a numerical value eps
    representing the tolerance for checking the element's difference
do_merge : bool
    if True, add any found key-value pair in B whose key is in L to A,
    if the tolerance allows or if it isn't already in A; otherwise, don't
    try merging

Returns
-------
ndiff : int
    number of different elements
nmerge : int
    number of merged elements (may not be useful, but heck, just return it)
    """

    ndiff  = 0
    nmerge = 0

    for row in L:
        ele = row[0]
        eps = row[1]
        if ele in B and B[ele] != None :
            if ele in A and A[ele] != None :
                valA = A[ele]
                valB = B[ele]
                if abs(A[ele] - B[ele]) > eps :
                    if verb :
                        print("+* Difference in dictionary elements:")
                        print('   eps = {}'.format(eps))
                        print('   A[{}] = {}'.format(ele, A[ele]))
                        print('   B[{}] = {}'.format(ele, B[ele]))
                    ndiff+= 1
                    # ... and cannot merge
                else:
                    if verb :
                        print("++ Reconciled dictionary elements:")
                        print('   eps = {}'.format(eps))
                        print('   A[{}] = {}'.format(ele, A[ele]))
                        print('   B[{}] = {}'.format(ele, B[ele]))
                    # ... and no need to merge
            else:
                if do_merge :
                    A[ele] = B[ele]
                    nmerge+= 1

    return ndiff, nmerge


def compare_list_items(A, B, eps=0.0, verb=1):
    """Let A and B be 1D lists of numerical values, each of length N.
This function goes through and compares elements of the same index,
checking whether values are equal within a tolerance of eps.  Output
the number of different elements (so, returning 0 means they are the
same).

Parameters
----------
A : list
    1D list of numerical-valued elements
B : list
    1D list of numerical-valued elements
eps: float
    tolerance of elementwise differences

Returns
-------
ndiff : int
    number of different elements, to within tolerance eps

    """

    N = len(A)
    if len(B) != N :
        print("** ERROR: unequal length lists:")
        print(    "len(A) =", N)
        print(    "len(B) =", len(B))
        sys.exit(3)

    ndiff = 0
    for ii in range(N):
        if abs(A[ii] - B[ii]) > eps :
            ndiff+= 1
            if verb :
                print("+* Difference in list elements:")
                print(    "A[{}] = {}".format(ii, A[ii]))
                print(    "B[{}] = {}".format(ii, B[ii]))

    return ndiff


def interpret_args(args_dict):
    """Interpret the user provided arguments (and potentially update
some).  This also checks that entered values are valid, and in some
cases a failure here will lead to a direct exit.

This function might likely grow over time as more options get added or
usage changes.

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

    verb = args_dict['verb']

    args_dict2 = copy.deepcopy(args_dict)

    if args_dict2['start_time'] == None :
        print("++ No start time provided; will assume it is 0.0.")
        args_dict2['start_time'] = 0.0

    '''
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
            print("++ Slice pattern: '{}'".format(pat))
            slice_times = BAU.slice_pattern_to_timing(pat, 
                                                      args_dict2['num_slices'],
                                                      args_dict2['volume_tr'])
            if not(slice_times) :
                print("** ERROR: could not convert slice pattern to timing")
                sys.exit(8)
            args_dict2['slice_times'] = copy.deepcopy(slice_times)
        elif os.path.isfile(pat) :
            print("++ Found slice_pattern '{}' exists as a file".format(pat))
            slice_times = read_slice_pattern_file(pat, verb=verb)
            if not(slice_times) :
                print("** ERROR: translate slice pattern file to timing")
                sys.exit(7)
            args_dict2['slice_times'] = copy.deepcopy(slice_times)
        else:
            print("** ERROR: could not match slice_pattern '{}' as "
                  "either a recognized pattern or file".format(pat))
            sys.exit(3)
    '''

    if args_dict2['extra_fix_list'] :
        # Interpret string to be list of ints or floats. NB: written
        # as floats, but these are OK for equality checks in this case
        IS_BAD = 0

        L = args_dict2['extra_fix_list'].split()
        try:
            efl = [float(ll) for ll in L]
            args_dict2['extra_fix_list'] = copy.deepcopy(efl)
        except:
            print("** ERROR interpreting extra_fix_list")
            IS_BAD = 1

        if IS_BAD :
            sys.exit(1)

    if args_dict2['remove_val_list'] :
        # Interpret string to be list of ints or floats. NB: written
        # as floats, but these are OK for equality checks in this case
        IS_BAD = 0

        L = args_dict2['remove_val_list'].split()
        try:
            lll = [float(ll) for ll in L]
            args_dict2['remove_val_list'] = copy.deepcopy(lll)
        except:
            print("** ERROR interpreting remove_val_list")
            IS_BAD = 1

        if IS_BAD :
            sys.exit(1)

    if args_dict2['RVT_lags'] :
        # Interpret string to be list of ints or floats. NB: written
        # as floats, but these are OK for equality checks in this case
        IS_BAD = 0

        L = args_dict2['RVT_lags'].split()
        try:
            lll = [float(ll) for ll in L]
            args_dict2['RVT_lags'] = copy.deepcopy(lll)
        except:
            print("** ERROR interpreting RVT_lags")
            IS_BAD = 1

        if IS_BAD :
            sys.exit(1)

    if '/' in args_dict2['prefix'] :
        print("** ERROR: Cannot have path information in '-prefix ..'\n"
              "   Use '-out_dir ..' for path info instead")
        sys.exit(4)

    # check many numerical inputs for being >0; probably leave this
    # one as last in this function
    for quant in all_quant_ge_zero:
        if args_dict2[quant] <= 0 :
            print("** ERROR: Provided '{}' value ({}) not allowed to be <=0.\n"
                  "".format(quant, args_dict2[quant]))
            sys.exit(4)

    # successful navigation
    return args_dict2

def add_info_to_dict(A, B):
    """Simply loop over keys in B and add them to A. Assume nothing
overlaps (might add checks for that later...).

Parameters
----------
A : dict
    The base dictionary to which new items get entered
B : dict
    The source dictionary from which new items are obtained

Returns
-------
C : dict
    The new dict of values (copy of A with B added)
"""

    C = copy.deepcopy(A)
    for key in B.keys() :
        C[key] = B[key]

    return C


def main_option_processing(argv):

    """This is the main function for running the physio processing
program.  It executes the first round of option processing: 
1) Make sure that all necessary inputs are present and accounted for.
2) Check all entered files exist.
3) Check many of the option values for validity (e.g., being >0, if
   appropriate).

This does not do the main physio processing itself, just is the first
step/gateway for doing so.

In some 'simple option' use cases, running this program simply
displays terminal text (or opens a text editor for displaying the
help) and then quits.  Otherwise, it returns a dictionary of checked
argument values.

Typically call this from another main program like:
    args_dict = lib_retro_opts.main_option_processing(sys.argv)

Parameters
----------
argv : list
    The list of strings that defines the command line input.

Returns
-------
args_dict : dict
    Dictionary of arg values from the command line. *Or* nothing might
    be returned, and some text is simply displayed (depending on the
    options used).

    """

    # ---------------------------------------------------------------------
    # case of 0 opt used: just show help and quit

    # We do this check separately, because in this case, each item in
    # args_dict is *not* a list containing what we want, but just is that
    # thing itself.  That confuses the means for checking it, so treat
    # that differently.

    if len(argv) == 1 :
        parser.print_help()
        sys.exit(0)

    # ---------------------------------------------------------------------
    # case of >=1 opt being used: parse!

    # get all opts and values as a main dict and secondary/temporary
    # dict of volume-related items, separately
    args_dict, vol_dict = parser_to_dict( parser, argv )

    # check for simple-simple cases with a quick exit: ver, help, etc.
    have_simple_opt = check_simple_opts_to_exit(args_dict, parser)
    if have_simple_opt :
        sys.exit(0)

    # parse/merge the volumetric dict items, which can have some
    # tangled relations.
    vol_dict  = interpret_vol_info(vol_dict, verb=args_dict['verb'])
    args_dict = add_info_to_dict(args_dict, vol_dict)

    # real work to be done now: check that all required inputs were
    # provided, and do a bit of verification of some of their
    # attributes (e.g., that files exist, values that should be >=0
    # are, etc.)
    args_dict = check_required_args(args_dict)
    args_dict = interpret_args(args_dict)

    return args_dict


# ================================ main =====================================

if __name__ == "__main__":

    args_dict = main_option_processing(sys.argv)
    print("++ DONE.  Goodbye.")

    sys.exit(0)
