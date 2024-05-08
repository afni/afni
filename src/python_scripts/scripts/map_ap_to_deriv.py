#!/usr/bin/env python

# This is a program to map AP results directory contents
# (specifically, some in the out.ss_review_uvars.json) to a separate
# derivatives directory.
#
#
#
# ===========================================================================

version = '1.0' # start
version = '1.1' # add options and real interface

# ==========================================================================

import sys, json
from   afnipy import lib_ap_derivs      as lader
from   afnipy import lib_ap_derivs_opts as ladopts
from   afnipy import afni_base          as BASE
from   afnipy import afni_util          as UTIL
from   afnipy import lib_apqc_tcsh      as lat
import argparse                         as argp
import textwrap

# ========================================================================== 
# setup help and options

help_dict = {
    'ddashline' : '='*76,
    'ver'       : version,
}

help_str_top = '''
Overview ~1~

This program is used to map afni_proc.py (AP) results directory
outputs to names and file structure of FMRI BIDS derivatives.

The number of outputs may increase over time.

{ddashline}

Options ~1~

'''.format(**help_dict)

help_str_epi = '''
{ddashline}

Examples ~1~

  1) Run the program from within an AP results dir, to make the output
     derivatives directory in the current spot:

     map_ap_to_deriv.py -subj_dir .

  2) Run the program from within an AP results dir, to make the output
     derivatives directory in a different group directory output:

     map_ap_to_deriv.py                               \\
         -subj_dir   .                                \\
         -deriv_dir  ../../group_derivs/deriv_sub-001




{ddashline}
written by: PA Taylor (SSCC, NIMH, NIH, USA)
{ddashline}
'''.format(**help_dict)



# ===========================================================================

# get args
parser = argp.ArgumentParser( prog=str(sys.argv[0]).split('/')[-1],
                              add_help=False,
                              formatter_class=argp.RawDescriptionHelpFormatter,
                              description=textwrap.dedent(help_str_top),
                              epilog=textwrap.dedent(help_str_epi) )

parser.add_argument('-subj_dir', nargs=1,
                    default=[ladopts.DEF['subj_dir']],
                    help='(req) location of the input AP results directory '
                    '(often ".", as this program is often run from within '
                    'the AP results directory).')

parser.add_argument('-deriv_dir', nargs=1,
                    default=[ladopts.DEF['deriv_dir']],
                    help='location of the output derivatives directory, '
                    'which can include path as well as output directory name. '
                    '(def: "{}_SUBJ", in "-subj_dir ..").'
                    ''.format(ladopts.DEF_deriv_dir_base))

parser.add_argument('-ow_mode', nargs=1,
                    default=[ladopts.DEF['ow_mode']],
                    help='set overwrite mode; choices are:\n'
                    ''.format(UTIL.hstr_ow_modes))

parser.add_argument('-verb', nargs=1,
                    default=[ladopts.DEF['verb']],
                    help='verbosity level '
                    '(def: {})'.format(ladopts.DEF['verb']))

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
ap_res_dir       = args.subj_dir[0].rstrip('/')
deriv_dir        = args.deriv_dir[0].rstrip('/')
ow_mode          = args.ow_mode[0]
verb             = int(args.verb[0])
do_ver           = args.ver
do_help          = args.help
do_hview         = args.hview

# =========================================================================

if __name__ == "__main__":

    # ---------------------------- help stuff -----------------------------

    # hview functionality
    if do_hview :
        prog = str(sys.argv[0]).split('/')[-1]
        cmd  = 'apsearch -view_prog_help {}'.format( prog )
        BASE.simple_shell_exec(cmd)
        sys.exit(0)

    # if nothing input or help opt, show help
    if len(sys.argv) == 1 or do_help :
        parser.print_help()
        sys.exit(0)

    # display prog version
    if do_ver :
        print(version)
        sys.exit(0)

    # ---------------------------------------------------------------------
    # check/ verify inputs

    # ap_res_dir?
    is_not_valid = ladopts.is_valid_ap_res_dir( ap_res_dir )
    if is_not_valid :    sys.exit(1)

    # get dictionary form of uvar json (and esp. subj ID)
    uvar_json = ap_res_dir + '/' + 'out.ss_review_uvars.json'
    with open(uvar_json, 'r') as fff:
        ap_ssdict = json.load(fff)
    subj = ap_ssdict['subj']

    # ow_mode?
    is_valid = UTIL.is_valid_ow_mode( ow_mode )
    if not(is_valid) :    sys.exit(2)

    # make deriv dir (might also have to generate its name)
    deriv_dir = ladopts.determine_deriv_dir_name(deriv_dir, ap_res_dir, 
                                                 subj=subj)
    bad_mno = UTIL.make_new_odir(deriv_dir, ow_mode=ow_mode, bup_dir=None)
    if bad_mno :    sys.exit(3)

    # ---------------------------------------------------------------------
    # do main work

    apder_obj = lader.ap_deriv_obj( ap_res_dir,
                                    deriv_dir=deriv_dir,
                                    ow_mode=ow_mode,
                                    verb=verb,
    )

    apder_obj.map_all()

