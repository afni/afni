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
import numpy      as     np

from   afnipy     import afni_base as BASE
from   afnipy     import afni_util as UTIL

# ==========================================================================

# threshold values for some floating point comparisons
EPS_TH = 1.e-3

# min beats or breaths per minute
DEF_min_bpm_card = 25.0
DEF_min_bpm_resp = 6.0   

# max beats or breaths per minute
DEF_max_bpm_card = 250.0
DEF_max_bpm_resp = 60.0   

# RVT shifts: either no RVT, direct list, or linspace set of pars
# (units: sec)
all_rvt_opt = ['rvt_off', 'rvt_shift_list', 'rvt_shift_linspace']
DEF_rvt_off            = False
DEF_rvt_shift_list     = '0 1 2 3 4'  # split+listified, below, if used
DEF_rvt_shift_linspace = None         # can be pars for np.linspace(A,B,C)

# some QC image plotting options that the user can change
DEF_img_figsize   = []
DEF_img_fontsize  = 10
DEF_img_line_time = 60               # units = seconds
DEF_img_dot_freq  = 50               # points per sec
DEF_img_bp_max_f  = 5.0              # Hz, for bandpass plot

# some init proc options for phys time series
DEF_prefilt_max_freq  = -1           # Hz, for init filter to reduce ts
all_prefilt_mode = ['none', 'median'] # list of possible downsamp types
DEF_prefilt_mode = 'none'            # str, keyword for filtering in downsamp
DEF_prefilt_win_card  = 0.10         # flt, window size (s) for median filter
DEF_prefilt_win_resp  = 0.25         # flt, window size (s) for median filter

# ===========================================================================

TEXT_interact_key_mouse = '''Key+mouse bindings being used:

            4  : delete the vertex (peak or trough) nearest to mouse point
            3  : add a peak vertex
            2  : add a trough vertex
            1  : toggle vertex visibility+editability on and off
   Left-click  : select closest vertex, which can then be dragged along
                the reference line.

   Some additional Matplotlib keyboard shortcuts:
            f  : toggle fullscreen view of panel
            o  : toggle zoom-to-rectangle mode
            p  : toggle pan/zoom mode
            r  : reset panel view (not point edits, but zoom/scroll/etc.)
            q  : quit/close viewer (also Ctrl+w), when done editing
'''

# ===========================================================================
# PART_01: default parameter settings

# default outdir name
now      = datetime.now() # current date and time
now_str  = now.strftime("%Y-%m-%d-%H-%M-%S")
odir_def = 'retro_' + now_str

# Each key in DEF should have an opt listing below in argparse, and
# vice versa. 

DEF = {
    'resp_file'         : None,      # (str) fname for resp data
    'card_file'         : None,      # (str) fname for card data
    'phys_file'         : None,      # (str) fname of physio input data
    'phys_json'         : None,      # (str) fname of json file
    'prefilt_max_freq'  : DEF_prefilt_max_freq,  # (num) init phys ts downsample
    'prefilt_mode'      : DEF_prefilt_mode,      # (str) kind of downsamp
    'prefilt_win_card'  : DEF_prefilt_win_card,  # (num) window size for dnsmpl
    'prefilt_win_resp'  : DEF_prefilt_win_resp,  # (num) window size for dnsmpl
    'do_interact'       : False,     # (bool) turn on interactive mode
    'dset_epi'          : None,      # (str) name of MRI dset, for vol pars
    'dset_tr'           : None,      # (float) TR of MRI
    'dset_nslice'       : None,      # (int) number of MRI vol slices
    'dset_nt'           : None,      # (int) Ntpts (e.g., len MRI time series)
    'dset_slice_times'  : None,      # (list) FMRI dset slice times
    'dset_slice_pattern' : None,     # (str) code or file for slice timing
    'freq'              : None,      # (float) freq, in Hz
    'start_time'        : None,      # (float) leave none, bc can be set in json
    'out_dir'           : odir_def,  # (str) output dir name
    'prefix'            : 'physio',  # (str) output filename prefix
    'do_fix_nan'        : False,     # (str) fix/interp NaN in physio
    'do_fix_null'       : False,     # (str) fix/interp null/missing in physio
    'do_fix_outliers'   : False,     # (list) fix/interp outliers
    'extra_fix_list'    : [],        # (list) extra values to fix
    'remove_val_list'   : [],        # (list) purge some values from ts
    'no_card_out'       : False,     # (bool) do not output card info
    'no_resp_out'       : False,     # (bool) do not output resp info
    'min_bpm_resp'      : DEF_min_bpm_resp, # (float) min breaths per min
    'min_bpm_card'      : DEF_min_bpm_card, # (float) min beats per min
    'max_bpm_resp'      : DEF_max_bpm_resp, # (float) max breaths per min
    'max_bpm_card'      : DEF_max_bpm_card, # (float) max beats per min
    'verb'              : 0,         # (int) verbosity level
    'disp_all_slice_patterns' : False, # (bool) display known sli patterns
    'disp_all_opts'     : False,     # (bool) display opts for this prog
    'ver'               : False,     # (bool) do show ver num?
    'help'              : False,     # (bool) do show help in term?
    'hview'             : False,     # (bool) do show help in text ed?
    'rvt_off'           : DEF_rvt_off, # (bool) turn off RVT output
    'rvt_shift_list'    : None,      # (str) space sep list of nums
    'rvt_shift_linspace': DEF_rvt_shift_linspace, # (str) pars for RVT shift 
    'img_verb'          : 1,         # (int) amount of graphs to save
    'img_figsize'       : DEF_img_figsize,   # (tuple) figsize dims for QC imgs
    'img_fontsize'      : DEF_img_fontsize,  # (float) font size for QC imgs 
    'img_line_time'     : DEF_img_line_time, # (float) time per QC imgs
    'img_dot_freq'      : DEF_img_dot_freq,  # (float) max dots per sec in img
    'img_bp_max_f'      : DEF_img_bp_max_f,  # (float) xaxis max for bp plot
    'save_proc_peaks'   : False,     # (bool) dump peaks to text file
    'save_proc_troughs' : False,     # (bool) dump troughs to text file
}

# list of keys for volume-related items, that will get parsed
# separately so we peel them off when parsing initial opts
vol_key_list = [
    'dset_epi',
    'dset_tr',
    'dset_nslice',
    'dset_nt',
    'dset_slice_times',
    'dset_slice_pattern',
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
    ['dset_tr',  EPS_TH],
    ['dset_nt', EPS_TH],
    ['dset_nslice', EPS_TH],
]
# extension of the above if the tested object is a list; the items in
# the list will be looped over and compared at the given eps
ALL_EPIM_MATCH_LISTS = [
    ['dset_slice_times', EPS_TH ],
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

# quantities that must be strictly > 0
all_quant_gt_zero = [
    'freq',
    'dset_nslice',
    'dset_nt',
    'dset_tr',
    'img_line_time',
    'img_fontsize',
    'img_dot_freq',
    'img_bp_max_f',
    'prefilt_win_card',
    'prefilt_win_resp',
]

# quantities that must be >= 0
all_quant_ge_zero = [
    'min_bpm_card',
    'min_bpm_resp',
    'max_bpm_card',
    'max_bpm_resp',
]

# --------------------------------------------------------------------------
# sundry other items

verb = 0

dent = '\n' + 5*' '

help_dict = {
    'ddashline' : '='*76,
    'ver'       : version,
    'AJM_str'   : AJM_str,
    'tikd'      : TEXT_interact_key_mouse,
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
        lll = UTIL.g_valid_slice_patterns
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
all_sli : list (of floats)
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
    all_sli = []
    all_len = []
    for ii in range(len(X)):
        row = X[ii]
        rlist = row.split()
        if rlist :
            N+= 1
            try:
                # use extend so all_sli stays 1D
                all_sli.extend([float(rr) for rr in rlist])
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
        print("** ERROR: dset_slice_pattern file {} is not Nx1 or 1xN.\n"
              "   Its dims of data are: nrow={},  max_ncol={}"
              .format(fname, N, M))
        return BAD_RETURN

    # finally, after more work than we thought...
    return all_sli

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
        
        epi_dict['dset_nslice']  = int(lll[2])
        epi_dict['dset_nt']      = int(lll[3])
        epi_dict['dset_tr']      = float(lll[4])
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
        print("** ERROR: number of dset_slice_times in header ({}) "
              "does not match slice count in k-direction ({})"
              "".format(nslice, nk))
        sys.exit(10)

    epi_dict['dset_slice_times'] = copy.deepcopy(lll)

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

def interpret_rvt_shift_linspace_opts(A, B, C):
    """Three numbers are used to determine the shifts for RVT when
processing.  These get interpreted as np.linspace(A, B, C).  Verify
that any entered set (which might come from the user) works fine.

Parameters
----------
A : float
    start of range
B : float
    end of range (inclusive)
C : int
    number of steps in range

Returns
-------
is_fail : bool
    False if everything is OK;  True otherwise
shift_list : list
    1D list of (floating point) shift values

    """

    try :
        shift_list = list(np.linspace(A, B, C))
    except:
        return True, np.zeros(0, dtype=float)
    
    return False, shift_list


# ========================================================================== 
# PART_03: setup argparse help and arguments/options

help_str_top = '''
Overview ~1~

This program creates slice-based regressors for regressing out
components of cardiac and respiratory rates, as well as the
respiration volume per time (RVT).

Much of the calculations are based on the following papers:

  Glover GH, Li TQ, Ress D (2000). Image-based method for
  retrospective correction of physiological motion effects in fMRI:
  RETROICOR. Magn Reson Med 44(1):162-7.

  Birn RM, Diamond JB, Smith MA, Bandettini PA (2006). Separating
  respiratory-variation-related fluctuations from
  neuronal-activity-related fluctuations in fMRI. Neuroimage
  31(4):1536-48.

This code has been informed by earlier programs that estimated
RETROICOR and RVT regressors, namely the RetroTS.py code by J Zosky,
which itself follows on from the original RetroTS.m code by ZS Saad.
That being said, the current code's implementation was written
separately, to understand the underlying processes and algorithms
afresh, to modularize several pieces, to refactorize others, and to
produce more QC outputs and logs of information.  Several steps in the
eventual regressor estimation depend on processes like peak- (and
trough-) finding and outlier rejection, which can be reasonably
implemented in many ways.  We do not expect exact matching of outcomes
between this and the previous versions.

Below, "resp" refers to respiratory input and results, and "card"
refers to the same for cardiac data.

{ddashline}

Options ~1~

'''.format(**help_dict)

help_str_epi = '''
{ddashline}

Notes on usage and inputs ~1~

* Physio data input: 
  At least one of the following input option sets must be used:
    -card_file 
    -resp_file 
    -card_file  and  -resp_file 
    -phys_file  and  -phys_json

* FMRI information input:
  It is preferable to use:
    -dset_epi
  to provide EPI dset for which regressors will be made, to provide
  the volumetric information that would otherwise be provided with:
    -dset_tr
    -dset_nslice
    -dset_nt
  ... and the slice timing information

* Slice timing input:
  If '-dset_epi ..' is not used to provide the slice timing (and other
  useful) volumetric information, then exactly one of the following
  input option must be used:
    -dset_slice_times
    -dset_slice_pattern

* Physio information input: 
  Each of the following input options must be provided through some
  combination of phys_json file, dset_epi file, or the command line
  opts themselves:
    -freq
    -dset_tr
    -dset_nslice
    -dset_nt

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

Notes on prefiltering physio time series ~1~

Many physio time series contain noisy spikes or occasional blips.  The
effects of these can be reduced during processing with some
"prefiltering".  At present, this includes using a moving median
filter along the time series, to try to remove spiky things that are
likely nonphysiological.  This can be implemented by using this opt+arg:
    -prefilt_mode median

An additional decision to make then becomes what width of filter to
apply.  That is, over how many points should the median be calculated?
One wants to balance making it large enough to be stable/useful with
small enough to not remove real features (like real peaks, troughs or
other time series changes).  This is done by choosing a time interval,
and this interval is specified separately for each of the card and
resp time series, because each has a different expected time scale of
variability (and experimental design can affect this choice, as well).  
So, the user can use:
    -prefilt_win_card  TIME_C
    -prefilt_win_resp  TIME_R
... and replace TIME_* with real time values, in using of seconds.  There
are default time values in place, when '-prefilt_mode ..' is used; see
above.

Finally, physio time series are acquired with a variety of sampling
frequencies.  These can easily range from 50 Hz to 2000 Hz (or more).
That means 50 (or 2000) point estimates per second---which is a lot
for most applications.  Consider that typical FMRI sampling rates are
TR = 1-2 sec or so, meaning that they have 0.5 or 1 point estimates
per sec.  Additionally, many (human) cardiac cycles are roughly of
order 1 per sec or so, and (human) respiration is at a much slower
rate.  All this is to say, having a highly sampled physio time series
can be unnecessary for most practical applications and analyses.  We
can reduce computational cost and processing time by downsampling it
near the beginning of processing. This would be done by specifying a
max sampling frequency MAX_F for the input data, to downsample to (or 
near to), via: 
    -prefilt_max_freq  MAX_F

All of the above prefiltering is applied after initial 'badness'
checks for outliers or missing values, so those processes can be a bit
slow for densely acquired data.

*Recommendation*
In general, at least for human applications, it seems hard to see why
one would need more than 50 physio measures per second.  It also seems
like median filtering over even relatively small windows typically be
useful.  So, perhaps consider adding these options to most processing 
(but adjust as appropriate!):
    -prefilt_max_freq   50 
    -prefilt_mode       median

If reasonable, the '-prefilt_win_card ..' and '-prefilt_win_resp ..'
values could also be adjusted.


{ddashline}

User interaction for peak/trough editing ~1~

This program includes functionality whereby the user can directly edit
the peaks and troughs that have estimated.  This includes adding,
deleting or moving the points around, with the built-in constraint of
keeping the points on the displayed physio time series line.  It's
kind of fun.

To enter interactive mode during the runtime of the program, use the
'-do_interact' option.  Then, at some stage during the processing, a
Matplotlib panel will pop up, showing estimated troughs and/or peaks,
which the user can edit if desired.  Upon closing the pop-up panel,
the final locations of peaks/troughs are kept and used for the
remainder of the code's run.

{tikd}

For more on the Matplotlib panel navigation keypresses and tips, see:
https://matplotlib.org/3.2.2/users/navigation_toolbar.html

At present, there is no "undo" functionality. If you accidentally
delete a point, you can add one back, or vice versa.

{ddashline}

Output files ~1~

The following files will/can be created in the output dir, with the
chosen prefix PREFIX.  Some are primary output files (like the file of
physio and RVT regressors), and some are helpful QC images.  The
*resp* files are only output if respiratory signal information were
input, and similarly for *card* files with cardiac input.  At present,
RVT is only calculated from resp input.

  PREFIX_slibase.1D         : slice-based regressor file, which can include
                              card, resp and RVT regressors, and provided 
                              to afni_proc.py for inclusion in FMRI processing

  PREFIX_regressors_phys.svg: QC image of all physio regressors (including
                              card and/or resp), corresponding to slice=0
                              physio regressors in *slibase.1D
  PREFIX_regressors_rvt_resp.svg: 
                              QC image of all RVT regressors from resp data,
                              corresponding to all shifted RVT regressors in
                              in *slibase.1D

  PREFIX_resp_review.txt    : summary statistics and information for resp proc
  PREFIX_card_review.txt    : summary statistics and information for card proc

  PREFIX_pc_cmd.tcsh        : log/copy of the command used
  PREFIX_info.json          : reference dictionary of all command inputs after
                              interpreting user options and integrating
                              default values

  PREFIX_card_*_final_peaks*.svg
                            : QC image of final peak estimation for card data.
                              Can be several files, depending on length of
                              input data. Colorbands highlight longer (red)  
                              and shorter (blue) intervals, compared to median 
                              (white)
  PREFIX_resp_10_final_peaks_troughs*.svg
                            : same as above image but for resp data (so also
                              includes troughs)

The following text files are only output when using the
'-save_proc_peaks' and/or '-save_proc_troughs' option flag(s):

  PREFIX_card_peaks_00.1D   : 1D column file of peak indices for card data,
                              corresponding to card*final_peaks*svg image.
  PREFIX_resp_peaks_00.1D   : 1D column file of peak indices for resp data,
                              corresponding to resp*final_peaks*svg image.
  PREFIX_resp_troughs_00.1D : 1D column file of trough indices for resp data,
                              corresponding to resp*final_peaks*svg image.

The following intermediate QC images are only output when the value of
'-img_verb' is 2 or more.  In each time series plotting case, there
may be multiple images, depending on time series length:

  PREFIX_card_*_peaks*.svg  : QC images showing intermediate stages of peak
                              calculation for card data
  PREFIX_resp_*_peaks*.svg  : same as above image but for resp data peaks
  PREFIX_resp_*_troughs*.svg: same as above image but for resp data troughs

  PREFIX_card_bandpass_spectrum.svg,
  PREFIX_resp_bandpass_spectrum.svg
                            : QC images showing intermediate stage of peak
                              and/or trough estimation, namely the Fourier
                              Transform frequency spectrum (magnitude only),
                              both full and bandpassed.

  PREFIX_card_bandpass_ts_peaks*.svg,
  PREFIX_resp_bandpass_ts_peaks*.svg,
  PREFIX_resp_bandpass_ts_troughs*.svg
                            : QC images showing intermediate stage of peak
                              and/or trough estimation, namely the initial
                              peak/trough estimation on the bandpassed
                              physio time series

  PREFIX_card_20_est_phase*.svg, 
  PREFIX_resp_20_est_phase*.svg
                            : QC images showing intermediate stages of phase
                              calculation for card and/or resp data

  PREFIX_resp_21_rvt_env*.svg
                            : QC images showing intermediate stages of RVT
                              calculation, namely envelope estimation

  PREFIX_resp_22_rvt_measure*.svg
                            : QC images showing intermediate stages of RVT
                              calculation, RVT per input time series point

{ddashline}

Examples ~1~

  Example 1
    
    physio_calc.py                                                           \\
        -card_file           physiopy/test000c                               \\
        -freq                400                                             \\
        -dset_epi            DSET_MRI                                        \\
        -dset_slice_pattern  alt+z                                           \\
        -extra_fix_list      5000                                            \\
        -do_fix_nan                                                          \\
        -out_dir             OUT_DIR                                         \\
        -prefix              PREFIX

  Example 2
    
    physio_calc.py                                                           \\
        -phys_file           physiopy/test003c.tsv.gz                        \\
        -phys_json           physiopy/test003c.json                          \\
        -dset_tr             2.2                                             \\
        -dset_nt             34                                              \\
        -dset_nslice         34                                              \\
        -dset_slice_pattern  alt+z                                           \\
        -do_fix_nan                                                          \\
        -extra_fix_list      5000                                            \\
        -out_dir             OUT_DIR                                         \\
        -prefix              PREFIX

  Example 3 

    physio_calc.py                                                           \\
        -card_file           sub-005_ses-01_task-rest_run-1_physio-ECG.txt   \\
        -resp_file           sub-005_ses-01_task-rest_run-1_physio-Resp.txt  \\
        -freq                50                                              \\
        -dset_tr             2.2                                             \\
        -dset_nt             219                                             \\
        -dset_nslice         33                                              \\
        -dset_slice_pattern  alt+z                                           \\
        -do_fix_nan                                                          \\
        -out_dir             OUT_DIR                                         \\
        -prefix              PREFIX
    
{ddashline}
written by: Peter Lauren, Paul Taylor, Richard Reynolds and 
            Daniel Glen (SSCC, NIMH, NIH, USA)

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
                              usage=argp.SUPPRESS, # don't show ugly usage
                              add_help=False,
                              allow_abbrev=False,
                              formatter_class=argp.RawDescriptionHelpFormatter,
                              #formatter_class=argp.RawTextHelpFormatter,
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

opt = '''start_time'''
hlp = '''The start time for the physio time series, relative to the initial
MRI volume (in s) (def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''prefilt_max_freq'''
hlp = '''Allow for downsampling of the input physio time series, by
providing a maximum sampling frequency (in Hz). This is applied just
after badness checks.  Values <=0 mean that no downsampling will occur
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''prefilt_mode'''
hlp = '''Filter input physio time series (after badness checks), likely
aiming at reducing noise; can be combined usefully with
prefilt_max_freq. Allowed modes: {all_mode} '''.format(all_mode = 
', '.join(all_prefilt_mode))
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''prefilt_win_card'''
hlp = '''Window size (in s) for card time series, if prefiltering input
physio time series with '-prefilt_mode ..'; value must be >0 (def:
{dopt}, only used if prefiltering is on)'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''prefilt_win_resp'''
hlp = '''Window size (in s) for resp time series, if prefiltering input
physio time series with '-prefilt_mode ..'; value must be >0 (def:
{dopt}, only used if prefiltering is on)'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''do_interact'''
hlp = '''Enter into interactive mode as the last stage of peak/trough
estimation for the physio time series (def: only automatic peak/trough
estimation)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

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

opt = '''dset_epi'''
hlp = '''Accompanying EPI/FMRI dset to which the physio regressors will be
applied, for obtaining the volumetric parameters (namely, dset_tr,
dset_nslice, dset_nt)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

opt = '''dset_tr'''
hlp = '''FMRI dataset's repetition time (TR), which defines the time
interval between consecutive volumes (in s)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''dset_nt'''
hlp = '''Integer number of time points to have in the output (should likely
match FMRI dataset's number of volumes)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''dset_nslice'''
hlp = '''Integer number of slices in FMRI dataset'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''dset_slice_times'''
hlp = '''Slice time values (space separated list of numbers)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    metavar=('SLI_T1', 'STI_T2'),
                    nargs='+', type=str) # parse later

opt = '''dset_slice_pattern'''
hlp = '''Slice timing pattern code (def: {dopt}). Use
'-disp_all_slice_patterns' to see all allowed patterns. Alternatively,
one can enter the filename of a file containing a single column of
slice times.'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=str)

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
                    metavar=('FVAL1', 'FVAL2'),
                    nargs='+', type=str) # parse later

opt = '''remove_val_list'''
hlp = '''List of one or more values that will removed (not interpolated: the
time series will be shorter, if any are found) if they appear in the
physio time series; this is necessary with some manufacturers'
outputs, see "Notes of input peculiarities," below.'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    metavar=('RVAL1', 'RVAL2'),
                    nargs='+', type=str) # parse later

opt = '''rvt_shift_list'''
hlp = '''Provide one or more values to specify how many and what kinds of
shifted copies of RVT are output as regressors. Units are seconds, and
including 0 may be useful. Shifts could also be entered via
'-rvt_shift_linspace ..' (def: {}) '''.format(DEF_rvt_shift_list)
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    metavar=('SHIFT1', 'SHIFT2'),
                    nargs='+', type=str) # parse later

opt = '''rvt_shift_linspace'''
hlp = '''Alternative to '-rvt_shift_list ..'. Provide three space-separated
values (start stop N) used to determine how many and what kinds of
shifted copies of RVT are output as regressors, according to the
Python-Numpy function np.linspace(start, stop, N). Both start and stop
(units of seconds) can be negative, zero or positive.  Including 0 may
be useful.  Example params: 0 4 5, which lead to shifts of 0, 1, 2, 3
and 4 sec (def: None, use '-rvt_shift_list')'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    metavar=('START', 'STOP', 'N'),
                    nargs=3, type=str) # parse later

opt = '''rvt_off'''
hlp = '''Turn off output of RVT regressors
'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

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

opt = '''max_bpm_resp'''
hlp = '''Set the maximum breaths per minute for respiratory proc (def: {})
'''.format(DEF_max_bpm_resp)
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''min_bpm_card'''
hlp = '''Set the minimum beats per minute for cardiac proc (def: {})
'''.format(DEF_min_bpm_card)
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''max_bpm_card'''
hlp = '''Set the maximum beats per minute for cardiac proc (def: {})
'''.format(DEF_max_bpm_card)
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''img_verb'''
hlp = '''Verbosity level for saving QC images during processing, by choosing
one integer:
0 - Do not save graphs
1 - Save end results (card and resp peaks, final RVT)
2 - Save end results and intermediate steps (bandpassing, peak refinement, etc.)
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

opt = '''img_figsize'''
hlp = '''Figure dimensions used for QC images (def: depends on length of
physio time series)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    metavar=('LEN', 'WID'),
                    nargs=2, type=str) # parse later

opt = '''img_fontsize'''
hlp = '''Font size used for QC images (def: {dopt})
'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''img_line_time'''
hlp = '''Maximum time duration per line in the QC images, in units of sec
(def: {dopt}) '''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''img_dot_freq'''
hlp = '''Maximum number of dots per line in the QC images (to save filesize
and plot time), in units of dots per sec (def: {dopt}) '''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''img_bp_max_f'''
hlp = '''Maximum frequency in the bandpass QC images (i.e., upper value of
x-axis), in units of Hz (def: {dopt}) '''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''save_proc_peaks'''
hlp = '''Write out the final set of peaks indices to a text file called
PREFIX_LABEL_proc_peaks_00.1D ('LABEL' is 'card', 'resp', etc.), which is
a single column of the integer values (def: don't write them out)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''save_proc_troughs'''
hlp = '''Write out the final set of trough indices to a text file called
PREFIX_LABEL_proc_troughs_00.1D ('LABEL' is 'card', 'resp', etc.), which
is a single column of the integer values (def: don't write them
out). The file is only output for LABEL types where troughs were
estimated (e.g., resp).'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''verb'''
hlp = '''Integer values to control verbosity level 
(def: {dopt})'''.format(dopt=DEF[opt])
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=int)

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
    # (dset_slice_pattern possible filename checked below)
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
    if not( args_dict2['dset_tr'] ) :
        print("** ERROR: must provide '-dset_tr ..' information")
        sys.exit(4)

    if not(args_dict2['dset_nslice']) :
        print("** ERROR: must provide '-dset_nslice ..' information")
        sys.exit(4)

    if not(args_dict2['dset_nt']) :
        print("** ERROR: must provide '-dset_nt ..' information")
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

    if not(args_dict2['dset_slice_times']) :
        print("** ERROR: must provide slice timing info in some way")
        sys.exit(4)

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
    if vol_dict['dset_slice_times'] and vol_dict['dset_slice_pattern'] :
        print("** ERROR: must use only one of either dset_slice_times or "
              "dset_slice_pattern")
        sys.exit(4)

    if vol_dict['dset_slice_times'] :
        # the input cmd line string has not been split yet; interpret
        # this single string to be a list of floats, and replace it in
        # the dict

        L = vol_dict['dset_slice_times'].split()
        try:
            # replace single string of slice times with list of
            # numerical values
            dset_slice_times = [float(ll) for ll in L]
            vol_dict['dset_slice_times'] = copy.deepcopy(dset_slice_times)
        except:
            print("** ERROR interpreting dset_slice_times from cmd line")
            sys.exit(1)
    elif vol_dict['dset_slice_pattern'] :
        # if pattern, check if it is allowed; elif it is a file, check
        # if it exists *and* use it to fill in
        # vol_dict['dset_slice_times']; else, whine.  Use any supplementary
        # info from the output dict, but edit vol_dict slice times in
        # place

        pat = vol_dict['dset_slice_pattern']
        if pat in UTIL.g_valid_slice_patterns :
            print("++ Slice pattern from cmd line: '{}'".format(pat))
            # check with vol info in vol_dict2 (not in vol_dict) bc
            # vol_dict2 should be the merged superset of info
            dset_slice_times = UTIL.slice_pattern_to_timing(pat, 
                                                       vol_dict2['dset_nslice'],
                                                       vol_dict2['dset_tr'])
            if not(dset_slice_times) :
                print("** ERROR: could not convert slice pattern to timing")
                sys.exit(8)
            vol_dict['dset_slice_times'] = copy.deepcopy(dset_slice_times)
        elif os.path.isfile(pat) :
            print("++ Found dset_slice_pattern '{}' exists as a file"
                  "".format(pat))
            dset_slice_times = read_slice_pattern_file(pat, verb=verb)
            if not(dset_slice_times) :
                print("** ERROR: translate slice pattern file to timing")
                sys.exit(7)
            vol_dict['dset_slice_times'] = copy.deepcopy(dset_slice_times)
        else:
            print("** ERROR: could not match provided dset_slice_pattern "
                  "'{}' as either a recognized pattern or file".format(pat))
            sys.exit(3)

    # ... and now that we might have explicit slice times in vol_dict,
    # reconcile any vol['dset_slice_times'] with vol_dict2['dset_slice_times']
    if 'dset_slice_times' in vol_dict and \
       vol_dict['dset_slice_times'] != None :
        if 'dset_slice_times' in vol_dict2 and \
           vol_dict2['dset_slice_times'] != None :
            # try to reconcile
            ndiff = compare_list_items( vol_dict['dset_slice_times'],
                                        vol_dict2['dset_slice_times'],
                                        eps=EPS_TH )
            if ndiff :
                print("** ERROR: inconsistent slice times entered")
                sys.exit(5)
        else:
            # nothing to reconcile, just copy over
            vol_dict2['dset_slice_times'] = \
                copy.deepcopy(vol_dict['dset_slice_times'])
    else:
        # I believe these cases hold
        if 'dset_slice_times' in vol_dict2 and \
           vol_dict2['dset_slice_times'] != None :
            pass
        else:
            # this is a boring one, which will probably lead to an
            # error exit in a downstream check
            vol_dict2['dset_slice_times'] = None

    # copy this over just for informational purposes
    if 'dset_slice_pattern' in vol_dict :
        vol_dict2['dset_slice_pattern'] = \
            copy.deepcopy(vol_dict['dset_slice_pattern'])

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

def check_multiple_rvt_shift_opts(args_dict):
    """Can only use at most one '-rvt_shift*' opt.  Simplest to check for
that at one time.

Parameters
----------
args_dict : dict
    The dictionary of arguments and option values.

Returns
-------
is_bad : int
    Value is 1 if badness from arg usage, 0 otherwise.

"""

    is_bad = 0
    count  = 0
    lopt   = []

    # go through and check, building a list
    for opt in all_rvt_opt :
        if args_dict[opt] :
            lopt.append('-' + opt)
            count+= 1

    # bad if more than one opt was used
    if count > 1 :
        print("** ERROR: more than one '-rvt_shift_*' opt was used:\n"
              "   {}\n"
              "   ... but at most only one can be.".format(' '.join(lopt)))
        is_bad = 1

    return is_bad

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


    if args_dict2['out_dir'] :
        # remove any rightward '/'.  Maybe also check for pre-existing
        # out_dir?
        args_dict2['out_dir'] = args_dict2['out_dir'].rstrip('/')

    if args_dict2['start_time'] == None :
        print("++ No start time provided; will assume it is 0.0.")
        args_dict2['start_time'] = 0.0

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

        if IS_BAD :  sys.exit(1)

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

        if IS_BAD :  sys.exit(1)

    # RVT considerations: several branches here; first check if >1 opt
    # was used, which is bad; then check for any other opts.  When
    # this full conditional is complete, we should have our shift
    # list, one way or another
    if check_multiple_rvt_shift_opts(args_dict2) :
            sys.exit(1)
    elif args_dict2['rvt_shift_list'] != None :
        # RVT branch A: direct list of shifts from user to make into array

        IS_BAD = 0

        L = args_dict2['rvt_shift_list'].split()

        try:
            # make list of floats
            shift_list = [float(ll) for ll in L]
            # and copy list of shifts
            args_dict2['rvt_shift_list'] = copy.deepcopy(shift_list) 
        except:
            print("** ERROR interpreting '-rvt_shift_list ..' args: '{}'"
                  "".format(args_dict2['rvt_shift_list']))
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)
    elif args_dict2['rvt_shift_linspace'] :
        # RVT branch B: linspace pars from user, list of ints or floats

        IS_BAD = 0

        # make sure -rvt_shift_list had 3 entries
        L = args_dict2['rvt_shift_linspace'].split()
        if len(L) != 3 :
            print("** ERROR, '-rvt_shift_linspace ..' takes exactly 3 values.")
            IS_BAD = 1

        try:
            # first 2 numbers can be int or float, but last must be int
            lll     = [float(ll) for ll in L]
            lll[-1] = int(lll[-1])
            # These 3 values get interpreted as (start, stop, N);
            # verify that this is a legit expression
            IS_BAD, all_shift = \
                interpret_rvt_shift_linspace_opts(lll[0], lll[1], lll[2])

            # copy original params in place
            args_dict2['rvt_shift_linspace'] = copy.deepcopy(lll) 
            # and copy arr of shifts
            args_dict2['rvt_shift_list'] = copy.deepcopy(all_shift) 
        except:
            print("** ERROR interpreting '-rvt_shift_linspace ..' args: '{}'"
                  "".format(args_dict2['rvt_shift_linspace']))
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)
    elif  args_dict2['rvt_off'] :
        # RVT branch C: no shifts (simple), as per user
        args_dict2['rvt_shift_list'] = []
    else:
        # RVT branch D: use default shifts
        L   = DEF_rvt_shift_list.split()
        args_dict2['rvt_shift_list'] = [float(ll) for ll in L]

    if args_dict2['img_figsize'] :
        # Interpret string to be list of floats.
        IS_BAD = 0

        L = args_dict2['img_figsize'].split()
        try:
            aaa = [float(ll) for ll in L]
            args_dict2['img_figsize'] = copy.deepcopy(aaa)
        except:
            print("** ERROR interpreting img_figsize")
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)

    if '/' in args_dict2['prefix'] :
        print("** ERROR: Cannot have path information in '-prefix ..'\n"
              "   Use '-out_dir ..' for path info instead")
        sys.exit(4)

    if args_dict2['prefilt_mode'] :
        # there are only certain allowed values
        IS_BAD = 0
        if args_dict2['prefilt_mode'] not in all_prefilt_mode :
           IS_BAD = 1
        if IS_BAD :  sys.exit(1)

    # check many numerical inputs for being >=0 or >0; probably leave this
    # one as last in this function
    IS_BAD = 0
    for quant in all_quant_ge_zero:
        if args_dict2[quant] == None :
            print("** ERROR: Must provide a value for '{}' via options.\n"
                  "".format(quant, args_dict2[quant]))
            IS_BAD+= 1
        elif args_dict2[quant] < 0 :
            print("** ERROR: Provided '{}' value ({}) not allowed to be <0.\n"
                  "".format(quant, args_dict2[quant]))
            IS_BAD+= 1
    for quant in all_quant_gt_zero:
        if args_dict2[quant] == None :
            print("** ERROR: Must provide a value for '{}' via options.\n"
                  "".format(quant, args_dict2[quant]))
            IS_BAD+= 1
        elif args_dict2[quant] <= 0 :
            print("** ERROR: Provided '{}' value ({}) not allowed to be <=0.\n"
                  "".format(quant, args_dict2[quant]))
            IS_BAD+= 1
    if IS_BAD :
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
