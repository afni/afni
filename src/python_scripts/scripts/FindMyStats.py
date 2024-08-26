#!/usr/bin/env python

########################################################################
## 08/2024 Justin Rajendra
## Find afni stats program outputs. Return table
## This is to be used as an input for a shiny app for "meta analysis"

## system libraries
import sys
import os
import glob
import subprocess
import csv
import re
import shutil
import argparse
import signal
import textwrap
import random
import time
import copy
import lib_discoFunc

## AFNI libraries
## import afni_util as UTIL
## import option_list as OL

########################################################################
## these may become command line arguments
delim = '\t'

########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(
    prog=str(sys.argv[0]),
    formatter_class=argparse.RawDescriptionHelpFormatter,
    description=textwrap.dedent('''\
------------------------------------------------------------------------------
## Overview ~1~

The purpose of the program is to find information about and the location of 
data sets that were created using any of AFNI's statistics programs.
First, it does a recursive file search from the starting path and finds all
files that have extensions that match neuroimaging data sets. 
(nii, nii.gz, HEAD, niml.dset)
Then the program gathers info on the data sets including the location on 
disk, which AFNI statistics program generated it, and dimensional data.
The purpose of this information is to feed another (as yet unnamed) program.

------------------------------------------------------------------------------
## Outputs ~1~

This program generates a tab separated text file containing the information 
described above and saves it to disk.

------------------------------------------------------------------------------
## Caveats ~1~

Warning for cloud service users (iCloud, One Drive, Dropbox, etc.):

This program may behave strangely if you are running it on a directory that 
has cloud storage enabled. If a file has an entry or link on the file system, 
but the file is actually in 'the cloud', your cloud storage program may try 
to download the file to your local file system. This will be very slow and 
may fill up your storage space. 

For example, running this on a macOS machine with iCloud storage enabled may 
cause the file system to download the files from the cloud if they are not 
already on your local file system. 

Make sure you want this or download the files first!

------------------------------------------------------------------------------
## Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 08/2024
------------------------------------------
                                 '''))

parser._action_groups.pop()
required = parser.add_argument_group('required')
optional = parser.add_argument_group('optional')

## required
required.add_argument('-path',type=str,required=True,
                      help=('Starting path for searching. '+
                            '(For the current directory use ".")') )

## optional
optional.add_argument('-prefix',type=str,default="MyStats",
                      help="Name for output (no extension). [MyStats]")
optional.add_argument('-verb',action="store_true",default=False,
                      help="Print out all found stats data sets to the "+
                      "screen without the full path.")
optional.add_argument('-no_recurse',action="store_true", default=False,
                      help="Search ONLY in the -path directory. NOT RECURSIVE!")
optional.add_argument('-overwrite',action="store_true", default=False,
                      help="Overwrite previous output file with same PREFIX")
parser.add_argument('-help',action='help',help='Show this help.')

## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

## do the parsing
args = parser.parse_args()
search_path = args.path
prefix = args.prefix
no_recurse = args.no_recurse
overwrite = args.overwrite
verb = args.verb

########################################################################
## definitions and fixed variables

dset_ext = ['HEAD', 'nii', 'nii.gz', 'niml.dset']

def find_stats(hist_in):
    '''See if the history has any of the specified program names in it.
    The history input should be string. The output is the name of one of the 
    statistics programs defined in stats_progs.'''
    stats_progs = ['3dttest++', '3dMVM', '3dLME', '3dLMEr', '3dMEMA', '3dANOVA',
                   '3dANOVA2', '3dANOVA3', '3dICC', '3dISC', '3dRegAna']
    if any((x := sub) for sub in stats_progs if sub in hist_in.decode().split()):
        return (x)

def get_stats(dset_in):
    '''pull the history of a dset as a string, find stat and return stat 
    prog name'''
    hist_cmd = "3dinfo -history "+dset_in
    hist = subprocess.check_output(hist_cmd, shell=True)
    stat_found = find_stats(hist)
    if stat_found:
        return (stat_found)

#################################################
## check for overwrite
if lib_discoFunc.check_overwrite(prefix+'.xls',overwrite):
    sys.exit(1)

#################################################
## find all of the suspected dsets with the usual file extensions
## maybe add option here for a specific number of levels rather than 
## all recursive or no recurse

## get a rough count and display 
print("\nReading file system recursively from "+search_path)
file_cmd = "find "+search_path+" -type f -name '*'  | wc -l"
num_files = subprocess.check_output(file_cmd, shell=True).decode().split()[0]
print('Approximately '+str(num_files)+' items found.')

if no_recurse:
    all_files = [os.path.join(search_path, file_names)
                 for file_names in os.listdir(search_path)
                 if os.path.isfile(os.path.join(search_path, file_names))]
    dset_files = [os.path.join(search_path, file_name)
                  for file_name in all_files
                  if any(file_name.endswith(ext) for ext in dset_ext)]
else:
    dset_files = [os.path.join(root,file_name)
                  for root, dir_names, file_names in os.walk(search_path)
                  for file_name in file_names
                  if any(file_name.endswith(ext) for ext in dset_ext)]

## make sure we have at least one
if len(dset_files) < 1:
    lib_discoFunc.print_afni_error('No datasets found')
    sys.exit(1)

print('\nIn the path '+search_path+',')
print(str(len(dset_files))+' datasets were found.\n')

#################################################
## find the ones that have stats programs in the history
## store the file with path and which stat program was found

## column headers
stats_dsets = ['File']
stat_used = ['StatProg']

## parameters for the progress bar
message = "Finding stats dsets: "
pb_len = lib_discoFunc.prog_bar_len(message,len(dset_files))

for i in lib_discoFunc.progressbar(range(len(dset_files)),message,pb_len):
    su = get_stats(dset_files[i])
    if su:
        stats_dsets.append(dset_files[i])
        stat_used.append(su)

## make sure we have at least one (1 for the header)
if len(stats_dsets) < 2:
    lib_discoFunc.print_afni_error('No datasets from AFNI stats programs found')
    sys.exit(1)

## print to screen if requested
if verb:
    first_col_max = len(max(stat_used, key=len)) + 2
    for i in range(len(stats_dsets)):
        print(
            f"{stat_used[i]:<{first_col_max}} {os.path.basename(stats_dsets[i])}")
    print()

print(str(len(stats_dsets)-1)+' stats dsets were found.\n')

#################################################
## get info on stats files

## so far I think we need these to compare the files later
# 3dinfo -header_line -n3 -ad3 -dc3 -obliquity -orient -space
# -space
# -same_dim = -n3
# -same_delta = -ad3
# -same_orient = -orient
# -same_center = -dc3
# -same_obl = -obliquity

## what do we want to know
info_req = '-n3 -ad3 -dc3 -obliquity -orient -space'

## change these to match the info requested (lazy...)
stats_info = [['Ni', 'Nj', 'Nk', 'ADi', 'ADj', 'ADk', 'DCx', 'DCy', 'DCz',
               'oblq', 'orient', 'space']]

## parameters for the progress bar
message = "Getting Info: "
pb_len = lib_discoFunc.prog_bar_len(message,len(stats_dsets)-1)

## run 3dinfo to get the requested info
## start at 1 because of the header
for i in lib_discoFunc.progressbar(range(1,len(stats_dsets)),message,pb_len):
    info_cmd = "3dinfo "+info_req+' '+stats_dsets[i]
    thd_info = subprocess.check_output(info_cmd,shell=True)
    stats_info.append(thd_info.decode().split())

#################################################
## output a tsv

## combine outputs
tab_out = []
for i, v in enumerate(stats_info):
    tab_out.append(copy.deepcopy(v))
    tab_out[-1].append(stat_used[i])
    tab_out[-1].append(stats_dsets[i])

## write file
with open(prefix+'.xls', 'w', newline='') as file:
    writer = csv.writer(file, delimiter=delim)
    writer.writerows(tab_out)

print('Output file saved as: '+prefix+'.xls\n')
sys.exit(0)
