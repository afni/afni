#!/usr/bin/env python

# Read in and parse options for the new retroicorLauren.py program.
# 
# ==========================================================================

version = '1.0'

# ==========================================================================

import sys
import os
import copy
import textwrap
import subprocess as     SP
import argparse   as     argp
from   datetime   import datetime
from   platform   import python_version_tuple
import borrow_afni_util  as BAU

# ==========================================================================
# default parameter settings

# default outdir name
now      = datetime.now() # current date and time
odir_def = now.strftime("retro_%Y-%m-%d-%H-%M-%S")

# Each key in DEF should have an opt listing below in argparse, and
# vice versa.

### [PT] Q: do we want a default slice_pattern, or always have user
###         enter it?
##  [PT] Q: how do quiet and verbose opts interact? just use one?
### [PT] Q: initialize more with null of types?
DEF = {
    'resp_file'         : None,      # (str) fname for resp data
    'card_file'         : None,      # (str) fname for card data
    'phys_file'         : None,      # (str) fname of physio input data
    'phys_json'         : None,      # (str) fname of json file
    'phys_file'         : None,      # (str) fname of physio input data
    'phys_json'         : None,      # (str) fname of json file
    'freq'              : None,      # (float) freq, in Hz
    'num_slices'        : None,      # (int) number of MRI vol slices
    'volume_tr'         : None,      # (float) TR of MRI
    'start_time'        : 0,         # (float) 
    'num_time_pts'      : None,      # (int) Ntps (e.g., len MRI time series)
    'out_dir'           : odir_def,  # (str) output dir name
    'prefix'            : None,      # (str) output filename prefix
    'slice_times'       : 0,         # (list) slice times
    'fir_order'         : 40,        # (int?) FIR order 
    'no_rvt_out'        : False,     # (bool) do not output RVT info
    'no_card_out'       : False,     # (bool) do not output card info
    'no_resp_out'       : False,     # (bool) do not output resp info
    'slice_pattern'     : 'alt+z',   # (str) code for slice timing
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
    'disp_slice_patterns' : False,   # (bool) display known sli patterns
    'ver'               : False,     # (bool) do show ver num?
    'help'              : False,     # (bool) do show help in term?
    'hview'             : False,     # (bool) do show help in text ed?
}

# ==========================================================================
# sundry other items

dent = '\n' + 5*' '

help_dict = {
    'ddashline' : '='*76,
    'ver'       : version,
}

## determine if captured subprocess encoding is needed; requiring after 3.6.0
REQ_ENCODE = python_version_tuple() >= ('3', '6', '0')

# ========================================================================== 
# helper functions

def parser_to_dict(parser, verb=0):
    """Convert an argparse parser object to a dictionary of key (=opt) and
value pairs.  
    
    Parameters
    ----------
    parser    : argparse.ArgumentParser
                object from parsing program options

    verb      : int
                verbosity level whilst working

    Return
    ------
    args_dict : dict
                dictionary whose keys are option names and values are the 
                user-entered values (which might still need separate 
                interpreting later)
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
    A         : dict
                a dictionary
    B         : dict
                a dictionary
    nameA     : str
                optional name for referring to dict A when reporting
    nameB     : str
                optional name for referring to dict B when reporting

    Return
    ------
    HAVE_DIFF_KEYS : int
                integer encoding whether there is a difference in the 
                set of keys in A and B:
                  0 -> no difference
                  1 -> difference

    """

    HAVE_DIFF_KEYS = 0

    if not(nameA) :    nameA = 'A'
    if not(nameB) :    nameA = 'B'

    # simple count
    na = len(A)
    nb = len(B)

    if na != nb :
        HAVE_DIFF_KEYS = 1
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
        HAVE_DIFF_KEYS = 1
        missA.sort()
        str_missA = ', '.join(missA)
        print("** ERROR: keys in {} that are missing in {}:\n"
              "          {}".format(nameB, nameA, str_missA))

    if len(missB) :
        HAVE_DIFF_KEYS = 1
        missB.sort()
        str_missB = ', '.join(missB)
        print("** ERROR: keys in {} that are missing in {}:\n"
              "          {}".format(nameA, nameB, str_missB))

    return HAVE_DIFF_KEYS

def check_simple_opts_to_exit(args_dict):
    """Check for simple options, after which to exit, such as help/hview,
ver etc.

    Parameters
    ----------
    args_dict : dict
                a dictionary of input options (=keys) and their values

    Return
    ------
    int       : int
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

        prog = str(sys.argv[0]).split('/')[-1]
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

    # if nothing or help opt, show help
    if args_dict['disp_slice_patterns'] :
        lll = BAU.g_valid_slice_patterns
        lll.sort()
        print("{}".format('\n'.join(lll)))
        return 1

    return 0


# ========================================================================== 
# setup help and options

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

# get args
parser = argp.ArgumentParser( prog=str(sys.argv[0]).split('/')[-1],
                              add_help=False,
                              formatter_class=argp.RawDescriptionHelpFormatter,
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
hlp = '''BIDS-formatted physio file in tab-separated format. May be gzipped'''
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
hlp = '''MRI volume's repetition time (TR), which defines 
the time interval between consecutive volumes (in s)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

opt = '''start_time'''
hlp = '''*** add *** (in s)'''
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
hlp = '''Prefix of output filenames (no path)'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    nargs=1, type=float)

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
hlp = '''*** add *** (def: {dopt})'''.format(dopt=DEF[opt])
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
hlp = '''Turn off output of RVT regressors'''
odict[opt] = hlp
parser.add_argument('-'+opt, default=[DEF[opt]], help=hlp,
                    action="store_true")

opt = '''font_size'''
hlp = '''Font size used for graphics (def: {dopt})'''.format(dopt=DEF[opt])
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
hlp = '''Output the time series based on the a and b coefficients from GLR00'''
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

opt = '''disp_slice_patterns'''
hlp = '''Display allowed slice pattern names'''
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

# =========================================================================
# programming check: are all opts and defaults accounted for?

have_diff_keys = compare_keys_in_two_dicts( odict, DEF,
                                            nameA = 'odict',
                                            nameB = 'DEF' )

if have_diff_keys :
    print("** ERROR: exiting because of opt name setup failure")
    sys.exit(1)

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
have_simple_opt = check_simple_opts_to_exit(args_dict)
if have_simple_opt :
    sys.exit(0)

# ---------------------------------------------------------------------------
# process opts slightly: case by case basis

if args_dict['slice_times'] :
    # interpret string to be list of floats
    IS_BAD = 0

    L = args_dict['slice_times'].split()
    try:
        slice_times = [float(ll) for ll in L]
        args_dict['slice_times'] = copy.deepcopy(slice_times)
    except:
        print("** ERROR interpreting slice times")
        IS_BAD = 1

    if IS_BAD :
        sys.exit(1)
        

if args_dict['slice_pattern'] :
    # if pattern, check if it is allowed; elif if is a file, check if
    # it exists; else, whine

    pat = args_dict['slice_pattern']
    if pat in BAU.g_valid_slice_patterns :
        print("++ Found slice_pattern '{}' in allowed list".format(pat))
    elif os.path.isfile("pat") :
        print("++ Found slice_pattern '{}' exists as a file".format(pat))
    else:
        print("** ERROR: could not match slice_pattern '{}' as "
              "either a recognized pattern or file".format(pat))
        sys.exit(3)







# ================================ main =====================================

if __name__ == "__main__":

    print("++ DONE.  Goodbye.")

    sys.exit(0)
