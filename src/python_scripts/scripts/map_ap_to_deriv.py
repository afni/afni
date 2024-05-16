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
         -deriv_dir  /path/to/derivatives/task_NAME




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
                    '(def: "{}", in "-subj_dir ..").'
                    ''.format(ladopts.DEF_deriv_dir_base))

parser.add_argument('-ow_mode_top', nargs=1,
                    default=[ladopts.DEF['ow_mode_top']],
                    help='set overwrite mode for top-level dir; '
                    'choices are:\n{}\n'
                    '(def: {})'.format(UTIL.hstr_ow_modes, 
                                       ladopts.DEF['ow_mode_top']))

parser.add_argument('-ow_mode_subj', nargs=1,
                    default=[ladopts.DEF['ow_mode_subj']],
                    help='set overwrite mode for subject-level dir; '
                    'choices are:\n{}\n'
                    '(def: {})'.format(UTIL.hstr_ow_modes, 
                                       ladopts.DEF['ow_mode_subj']))

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
ow_mode_top      = args.ow_mode_top[0]
ow_mode_subj     = args.ow_mode_subj[0]
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

    # ow_modes?
    is_valid = UTIL.is_valid_ow_mode( ow_mode_top )
    if not(is_valid) :    sys.exit(2)
    is_valid = UTIL.is_valid_ow_mode( ow_mode_subj )
    if not(is_valid) :    sys.exit(2)

    # check or make deriv_dir name
    deriv_dir = ladopts.determine_deriv_dir_name(deriv_dir, ap_res_dir)

    # make deriv dir itself (will *always* be made; ow_mode applies to
    # subdir)
    ##bad_mno   = UTIL.make_new_odir(deriv_dir, ow_mode=ow_mode_top)
    ##if bad_mno :    sys.exit(3)     # should not happen, bc of ow_mode

    # ---------------------------------------------------------------------
    # do main work

    apder_obj = lader.ap_deriv_obj( ap_res_dir,
                                    deriv_dir=deriv_dir,
                                    ow_mode_top=ow_mode_top,
                                    ow_mode_subj=ow_mode_subj,
                                    verb=verb,
    )

    apder_obj.map_all()

