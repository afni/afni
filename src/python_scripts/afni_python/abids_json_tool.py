#!/usr/bin/env python

########################################################################
## 08/2018 Justin Rajendra
## convert : sep txt to json or the other way around
## add stuff to json files

## system libraries
import sys, os, glob, subprocess, csv, re, argparse, signal, textwrap, json
import abids_lib
from collections import OrderedDict

########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),add_help=False,
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
------------------------------------------
Overview ~1~

    This script helps to manipulate json files in various ways.

Caveats ~1~

    None yet.

Example ~1~

    abids_json_tool.py -input out.ss_review.FT.txt \
                       -prefix out.ss_review.FT.json \
                       -txt2json

------------------------------------------

Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 08/2018
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
required.add_argument('-input',type=str,metavar='FILE',required=True,
                      help=('One file to convert. '+
                            '(either ":" separated or json formatted.) '+
                            'Enter NULL with -add_json to create new json file.'))
required.add_argument('-prefix',type=str,metavar='PREFIX',required=True,
                      help='Output file name.')

## only one of these at a time
OnlyOne.add_argument('-txt2json',action="store_true",default=False,
                     help=('Convert from ":" separated text file to '+
                           'json formatted file.'))
OnlyOne.add_argument('-json2txt',action="store_true",default=False,
                     help=('Convert from json formatted file to '+
                           '":" separated text file.'))
OnlyOne.add_argument('-add_json',type=str,nargs='+',metavar=('KEY','VALUE'),
                     action="append",
                     help=('Add an attribute to the end of the specified '+
                            'json file. Needs exactly two arguments. '+
                            '(e.g. Fruit Apple) '+
                            'The KEY must not have spaces and must be only '+
                            'one word. If the VALUE is more than one item, it '+
                            'needs to be surrounded by single or double quotes '+
                            'and be comma separated (e.g. Fruit "Apple,Orange")'))
OnlyOne.add_argument('-del_json',type=str,nargs=1,metavar='KEY',
                     help=('Remove attribute (KEY) from the -input json file.'))
## optional
parser.add_argument('-force_add','-f',action="store_true",default=False,
                    help=('Use with -add_json to overwrite an existing '+
                          'attribute in the specified json file.'))
parser.add_argument('-overwrite',action="store_true",default=False,
                    help=('Use caution as this will overwrite the -prefix '+
                          'file if it exists!!'))
parser.add_argument('-help',action='help',help='Show this help and exit.')

## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

########################################################################
## collect the arguments
args = parser.parse_args()
input = args.input
txt2json = args.txt2json
json2txt = args.json2txt
new_entry = args.add_json
del_entry = args.del_json
prefix = args.prefix
overwrite = args.overwrite
force = args.force_add

########################################################################
## check stuff

## check input file
if input == "NULL":
    input = prefix
else:
    if not os.path.isfile(input):
        print("\nERROR: "+input+" not found!!\n")
        sys.exit(1)

## namey things
infile = os.path.abspath(input)
full_path = os.path.dirname(infile)
infile_base = os.path.basename(infile)
infile_ext = os.path.splitext(infile_base)[1]

## check prefix
if os.path.isfile(prefix) and not overwrite:
    print("\nERROR: "+prefix+" exists!!\n")
    sys.exit(1)

########################################################################
## txt2json
if txt2json:
    json_dict = OrderedDict()   ## preserve order
    with open(infile) as f:
        for line in f:

            ## split on : and skip if blank line
            field = re.split(': | =',line)
            # field = line.split(":")
            if len(field) == 1: continue

            ## make dictionary key and clean up
            key = field[0].rstrip().replace(" ","_")    ## get rid of spaces
            key = re.sub("[()]","",key)                 ## get rid of ()

            ## make value list or entry and convert to float if number
            value_list = field[1].rstrip().lstrip().split()
            value = []
            for v in value_list:
                try:
                    value.append(float(v))
                except ValueError:
                    value.append(str(v))

            ## if only one, make not a list and add to dictionary
            if len(value) == 1: value = value[0]
            json_dict.update({key:value})

    ######################
    ## write out json file
    json_out = json.dumps(json_dict,indent=4)
    f = open(prefix,"w")
    f.write(json_out)
    f.close()
## end txt2json

########################################################################
## json2txt
if json2txt:
    ## read in json from handy abids_lib function
    json_data = abids_lib.json_import(infile)

    ## get the max characters for lining everything up
    max_char = max([len(i) for i in json_data.keys()])

    ## write out
    with open(prefix,'wb') as csv_file:
        writer = csv.writer(csv_file,delimiter=":")
        for key, value in json_data.items():
            trailing = max_char - len(key) + 2  ## spacing

            ## check if list and print space separated
            if isinstance(value, (list,)):
                writer.writerow([key+' '*trailing,'  '+' '.join(map(str,value))])
            else:
                writer.writerow([key+' ' * trailing,'  '+str(value)])
## end json2txt

########################################################################
## add entry
if new_entry is not None:

    if input is not prefix:
        ## read in json from handy abids_lib function
        json_data = abids_lib.json_import(infile)
    else:
        json_data = OrderedDict()   ## make empty one

    ## get the new stuff
    for new in new_entry:

        key = new[0]
        value_list = new[1].split(',')

        ## check to see if the attribute is already there
        if key in json_data.keys() and not force:
            print("\nERROR: The '"+key+"' attribute is already exists in "+
                  infile_base+"!!\n"+
                  "       Use the -force (-f) option to overwrite attribute.\n")
            sys.exit(1)

        ## make value list or entry and convert to float if number
        value = []
        for v in value_list:
            try:
                value.append(float(v))
            except ValueError:
                value.append(str(v))

        ## not a list if only 1 and add new entry
        if len(value) == 1: value = value[0]
        json_data.update({key:value})

    ######################
    ## write out json file
    json_out = json.dumps(json_data,indent=4)
    f = open(prefix,"w")
    f.write(json_out)
    f.close()
## end add entry

########################################################################
## delete entry
if del_entry is not None:
    ## read in json from handy abids_lib function
    json_data = abids_lib.json_import(infile)

    ## make sure it is there and remove it
    if del_entry[0] in json_data:

        print("\nRemoving '"+del_entry[0]+"' from "+infile_base+"\n")
        del json_data[del_entry[0]]

        ## write out json file
        json_out = json.dumps(json_data,indent=4)
        f = open(prefix,"w")
        f.write(json_out)
        f.close()
    else:
        print("\nERROR: The '"+del_entry[0]+"' attribute does not exist in "+
              infile_base+"!!\n"+
              "       View the available attributes with:\n"+
              "       abids_json_info.py -json "+infile+" -list_fields\n")
        sys.exit(1)
## end delete entry

sys.exit(0)

