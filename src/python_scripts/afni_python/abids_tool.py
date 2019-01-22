#!/usr/bin/env python

########################################################################
## 05/2018 Justin Rajendra
## some tools to deal with bids data

## system libraries
import sys, os, glob, subprocess, csv, re, argparse, signal, textwrap, json
import afni_base, abids_lib


########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),add_help=False,
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
------------------------------------------
Overview ~1~

    This program does various things with BIDS formatted datasets created
    with dcm2niix_afni or dcm2niix.  The main point as of now is to pull
    information from the matching json file and 3drefit the input dataset.
    If you just want info from the matching json file, use abids_json_info.py.

Caveats ~1~

    This assumes that the nifti dataset was converted from dicoms using
    dcm2niix_afni or dcm2niix with the -b (BIDS) option. So a json file and
    matching dataset in NIFTI format should be present. (NO AFNI FORMAT...)

    The json file should end in .json (lower case) as outputted from dcm2niix.
    The program will try to find a json file that matches the prefix of the
    input dataset.
    Like this:
         my_bids_fmri.nii.gz <-> my_bids_fmri.json

    For most options, 3drefit will be run on the -input dataset(s).
    So the dataset(s) will be overwritten!!
    Make sure you want to do this!!
    All of the caveates for 3drefit apply here...!!
    (https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/3drefit_sphx.html)

    For example, when using -add_TR, if the dataset has slice time offsets,
    these will be scaled by the factor newTR/oldTR. So you may want to
    use -add_TR BEFORE -add_slice_times. However, afni_dcm2niix usually
    adds the correct TR to the dataset header automatically. So you
    should not need -add_TR...

    Also, this has only been tested with 3d+time fMRI data acquired in the
    axial (z or k) direction. If you have problems with data acquired in the
    sagittal or coronal direction, post to the message board.

Example ~1~

    abids_tool.py -add_slice_times -input my_bids_fmri.nii.gz

------------------------------------------

Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 05/2018
For Wolverine, hiya Bub...
Keep on keeping on!
------------------------------------------
                                 '''))

## setup the groups
OneRequired = parser.add_argument_group('Only one of these')
OnlyOne = OneRequired.add_mutually_exclusive_group(required=True)
required = parser.add_argument_group('Required arguments')
parser._optionals.title = 'Optional arguments'
parser._action_groups.reverse()

## required
required.add_argument('-input',type=str,help='At least one 3d+time dataset.',
                      required=True,metavar='DSET',nargs='+')
## optional
parser.add_argument('-help',action='help',help='Show this help and exit.')

## mutally exclusive but one required
OnlyOne.add_argument('-TR_match',action="store_true",default=False,
                     help=('Check if the TR in the json file matches the'+
                           ' TR from input dataset header. (1 if match)'))
OnlyOne.add_argument('-add_TR',action="store_true",default=False,
                     help=('Add the TR from the BIDS json file'+
                           ' to the input dataset using 3drefit.'))
OnlyOne.add_argument('-add_slice_times',action="store_true",default=False,
                     help=('Add the slice times from the BIDS json file'+
                           ' to the input dataset using 3drefit.'))
OnlyOne.add_argument('-copy',type=str,nargs='+',metavar='PREFIX',
                     help=('Copy both the NIFTI dataset(s) AND matching '+
                           '.json file(s) to PREFIX. Must have the same '+
                           'number of prefixes as datasets!'))
## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

########################################################################
## collect the arguments
args = parser.parse_args()
dset_list = args.input
add_slice_times = args.add_slice_times
TR_match = args.TR_match
add_TR = args.add_TR
new_prefix = args.copy

## verify rename lengths
if new_prefix is not None:
    if len(new_prefix) != len(dset_list):
        print("\nError: "+str(len(new_prefix))+" prefix(es) do(es) not match "+
              str(len(dset_list))+" dataset(s)!!\n")
        sys.exit(1)

########################################################################
## go for all
for i in range(0,len(dset_list)):

    ########################################################################
    ## check input dataset

    ## name the dset
    dset = afni_base.afni_name(dset_list[i])

    ## get some info
    afni_cmd = ("3dinfo -exists -ni -nj -nk -TR "+dset.rppv())
    check_info = subprocess.check_output(afni_cmd,shell=True).split()

    ## is the input dataset there?
    if check_info[0] == "0":
        print("\nError: "+dset.pv()+" does not exist or is not loadable!!\n")
        sys.exit(1)

    ########################################################################
    ## check for the json file and read it in

    ## automatically find matching .json ad check it
    json_file = dset.realp()+dset.prefix+".json"
    if not os.path.isfile(json_file):
        print("\nError: "+os.path.basename(json_file)+
              " not found!!\n")
        sys.exit(1)

    ## read in the json file
    json_data = abids_lib.json_info(json_file)

    ########################################################################
    ## add slice timing
    if add_slice_times:
        ## get the slice times
        slice_list = json_data.field("SliceTiming")

        ## make sure it is there
        if slice_list is None:
            print("\nError: "+os.path.basename(json_file)+
                  " has no SliceTiming field!!\n")
            sys.exit(1)

        ## get the number of slices from the json file
        json_slices = len(slice_list)

        ## get the number of slices in the k direction from the nii dataset
        k_slices = int(check_info[3])

        ## only one direction (hopefully k) should match the number of timings
        ## find all matches (may take this out later)
        match = [i for i, x in enumerate(check_info[1:4]) if x == str(json_slices)]

        ## no match boo
        if len(match) == 0:
            print("\nERROR: No voxel dimension ("+str(check_info[1:4])+
                  ") matches the number of slices in the json file ("+
                  str(json_slices)+")!!!\n")
            sys.exit(1)

        ## more than one match boo
        if len(match) > 1:
            print("\nERROR: Multiple voxel dimensions ("+
                  str([check_info[1:4][i] for i in match])+
                  ") match the number of slices"+" in the json file ("+
                  str(json_slices)+")!!!\n")
            sys.exit(1)

        ## hard check for k direction assumption
        if k_slices != json_slices:
            print("\nERROR: There are "+str(json_slices)+
                  " slice times in the json file:\n       "+
                  os.path.basename(json_file)+"\n       "+
                  "and there are "+str(k_slices)+
                  " slices in the k direction of the dataset:\n       "+
                  dset.pv()+"!!!\n")
            sys.exit(1)

        ########################################################################
        ## all should be good now... add the slice timings to the header

        ## save the integers as one big string
        slice_string = ' '.join(map(str,slice_list))

        ## add the slices to the nii header
        afni_cmd = ("3drefit -Tslices "+slice_string+" "+dset.rppv())
        abids_lib.exec_or_error(afni_cmd,"ERROR: Failed to add slice times!!")
        print("Added slice times:\n"+slice_string+"\nto "+dset.pv()+".\n")

    ########################################################################
    ## see if TR from the json matches the TR from the dataset
    if TR_match:
        ## get TR from json file and make sure it is there
        json_TR = json_data.tr
        if json_TR is None:
            print("\nError: "+os.path.basename(json_file)+
                  " has no RepetitionTime field!!\n")
            sys.exit(1)

        ## get TR from dataset
        dset_TR = float(check_info[4])

        ## return values
        val = "1" if json_TR == dset_TR else "0"
        print(val)

    ########################################################################
    ## add TR from json file to the dataset header
    if add_TR:
        ## get TR from json file and make sure it is there
        json_TR = json_data.tr
        if json_TR is None:
            print("\nError: "+os.path.basename(json_file)+
                  " has no RepetitionTime field!!\n")
            sys.exit(1)

        ## add the slices to the nii header
        afni_cmd = ("3drefit -TR "+str(json_TR)+" "+dset.rppv())
        abids_lib.exec_or_error(afni_cmd,"ERROR: Failed to add TR!!")
        print("Added TR of "+str(json_TR)+" seconds to "+dset.pv()+".\n")

    ########################################################################
    ## rename both the dataset and json file
    if new_prefix is not None:
        if os.path.isfile(new_prefix[i]+".json"):
            print("\nError: "+new_prefix[i]+".json already exists. "+
                 "Not overwritting!!\n")
            sys.exit(1)
        else:
            os.system("cp "+json_file+" "+new_prefix[i]+".json")

        afni_cmd = ("3dcopy "+dset.rppv()+" "+new_prefix[i]+".nii.gz")
        abids_lib.exec_or_error(afni_cmd,"ERROR: Failed 3dcopy!!")

## end dset loop
sys.exit(0)

