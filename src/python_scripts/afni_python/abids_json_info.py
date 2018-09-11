#!/usr/bin/env python

########################################################################
## 05/2018 Justin Rajendra
## do stuff with info from bids json file or any json file

## system libraries
import sys, os, glob, subprocess, csv, re, argparse, signal, textwrap, json

## locations of stuff
import afni_base, abids_lib
from collections import OrderedDict

########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),add_help=False,
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
------------------------------------------
Overview ~1~

    This program extracts info from BIDS formatted json files created
    with dcm2niix_afni or dcm2niix. This is mostly for internal use as a
    python library. It will also extract fields from any json formatted file.

Caveats ~1~

    This assumes that the json file was converted from dicoms using
    dcm2niix_afni or dcm2niix with the -b (BIDS) option. So a json file and
    matching dataset should be present.

Example ~1~

    abids_json_info.py -TR -json my_bids_fmri.json

------------------------------------------

Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 05/2018
Keep on keeping on!
------------------------------------------
                                 '''))

## setup the groups
bids = parser.add_argument_group('BIDS specific arguments')
required = parser.add_argument_group('Required arguments')
parser._optionals.title = 'Optional arguments'
parser._action_groups.reverse()

## required
required.add_argument('-json',type=str,metavar='JSON',default="",nargs='+',
                    help='Specify .json file(s).')
## bids specific
bids.add_argument('-TR',action="store_true",default=False,
                     help=('Print the TR from the json file in seconds,'+
                           ' from the "RepetitionTime" field.'))
bids.add_argument('-TE',action="store_true",default=False,
                     help=('Print out the "EchoTime" field in milliseconds '+
                           '(the json file stores it in seconds)'))
bids.add_argument('-TE_sec',action="store_true",default=False,
                     help=('Print the "EchoTime" field in seconds'))
bids.add_argument('-match_nii',action="store_true",default=False,
                     help=('Is there a .nii or .nii.gz file that matches '+
                           'the .json file? (1 if the dataset is loadable)'))
## optional
parser.add_argument('-field',type=str,metavar='STR',nargs='+',
                     help=('Print any field or list of fields from the '+
                           ' json file.'))
parser.add_argument('-list_fields',action="store_true",default=False,
                     help=('Print a list of the available fields from'+
                           ' the .json file. (This must be the only argument'+
                           ' specified)'))
parser.add_argument('-help',action='help',help='Show this help and exit.')

## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

########################################################################
## collect the arguments
args = parser.parse_args()
json_files = args.json
TR = args.TR
EchoTime = args.TE
EchoTime_sec = args.TE_sec
json_field = args.field
field_list = args.list_fields
match_nii = args.match_nii

## get order of arguments that they request
arg_list = [i for i in sys.argv if i.startswith('-')]
arg_list.remove('-json')

## make sure that field_list is not in there
if '-list_fields' in arg_list and len(arg_list) > 1:
    print("\nError: -list_fields must be specified ALONE!!\n")
    sys.exit(1)

########################################################################
## go for all
for i in json_files:

    ## create the json_info object and a list to print
    j = abids_lib.json_info_dset(i)
    out_list = []

    ########################################################################
    ## print out all available fields from the json file
    if field_list:
        j.dict = OrderedDict(j.dict)
        for key, value in j.dict.items():
            print(key)
        continue

    ########################################################################
    ## loop through the order of arguments
    for a in arg_list:

        ## tr
        if a in ['-TR'] and TR:
            out_list.append(j.tr)

        ## te ms and sec
        if a in ['-TE'] and EchoTime:
            out_list.append(j.te)
        if a in ['-TE_sec'] and EchoTime_sec:
            out_list.append(j.te_sec)

        ## is there a nifti?
        if a in ['-match_nii'] and match_nii:
            afni_cmd = ("3dinfo -exists "+j.nii.rppv())
            check_info = subprocess.check_output(afni_cmd,shell=True).split()
            out_list.append(check_info[0])

        ## is there a nifti?
        if a in ['-field'] and json_field is not None:
            for f in json_field:
                out_list.append(j.field(f))

    ## print out the combined string with spaces
    print(' '.join(map(str,out_list)))

## end dset loop
sys.exit(0)


