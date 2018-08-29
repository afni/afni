#!/usr/bin/env python

########################################################################
## 01/2018 Justin Rajendra
## parse help outputs to make sphinx pages

## system libraries
import sys, os, glob, subprocess, csv, re, shutil, argparse, signal, textwrap

# requires PYTHONPATH to directory of AFNI binaries
import afni_util as au

## [PT: Mar 22, 2018] Make the reference for each help in the "All
## Help" section be ".. _ahelp_PROGNAME"

## possible codes as characters
hdr_codes = ['1','2','3','4']

########################################################################
## functions

## check for a code at the end of a line
## takes in a line from help, returns code as character
def get_code(line_in):

    ## set the header code
    hdr_code = "boo"

    ## get the current line without leading or trailing whitespace
    cur_line = line_in.rstrip().lstrip()

    ## get the last digits and see if the code is there
    line_end = cur_line[-3:]
    if len(line_end) == 3:
        if line_end[0] == "~" and line_end[2] == "~":
            hdr_code = line_end[1]

    return hdr_code

## check the next lines to see if there is a code after a code
def next_line(help_in,line_num):

    ## loop through the next lines
    for l in range(line_num+1,len(help_in)-1):

        ## strip the line and search for a non blank or separator line
        cur_line = help_in[l].rstrip().lstrip()
        if len(set(cur_line)) != 1 and cur_line != "":

            ## get code duh and
            if get_code(help_in[l]) in hdr_codes:
                return(1)
            else:
                return(0)  ## has text but no header code

########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
Overview ~1~

Parse the help output of all AFNI programs (or just one) to create a
sphinxy version.

This program will look for codes in the help output and use those to
make sphinx headers and tables of contents.

The codes are 3 characters at the end of a line of the help output:
~1~ = Main section
~2~ = Sub section
~3~ = Sub sub section
~4~ = Horizontal line separator above the line with the code

Caveats ~1~

If there are no codes, this program will create a code-block of the
help output.  It will also create a table of contents with all
programs listed in a 3 column table.

If you use the -prog option, you will break the main_toc as it will be
overwritten with just one entry. Use this option for testing only!

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra 01/2018
Keep on keeping on!
------------------------------------------
                                 '''))
########################################################################
## collect the arguments

parser._action_groups.pop()
required = parser.add_argument_group('required')
optional = parser.add_argument_group('optional')

required.add_argument('-OutFolder',
                      type=str,
                      help='Where do you want the .rst files to go?',
                      required=True)
optional.add_argument('-prog',
                      type=str,
                      default="nothing",
                      help="Single AFNI program to sphinxify. (For testing only. Will break main_toc.)")
parser.add_argument('-help',action='help',help='Show this help.')


args = parser.parse_args()
prog_list = args.prog

## where to dump the .rst files? (with trailing "/") (create if not there)
OutFolder = args.OutFolder
if OutFolder[-1] != "/":
    OutFolder = OutFolder+"/"
if not os.path.exists(OutFolder):
    os.makedirs(OutFolder)


########################################################################
## get list of afni files or use the one provided
if prog_list == "nothing":
    stat,prog_list = au.exec_tcsh_command("apsearch -list_all_afni_progs",
                                       lines=1,noblank=0)
else:
    prog_list = [prog_list]

########################################################################
## create the main_toc.rst

## open file for writing
toc_file = OutFolder+"main_toc.rst"
main_toc = open(toc_file,"w")

## write out the header
main_toc.write(":tocdepth: 2\n\n")
main_toc.write(".. _programs_main:\n\n")
main_toc.write("##################\n")
main_toc.write("All program helps\n")
main_toc.write("##################\n\n")
main_toc.write('This is a list of all AFNI programs.  Click on any name to see the help for that program.\n\nFor additional reference, please also see the "classified" list of helps :ref:`HERE<edu_class_prog>`\, where programs are loosely grouped by topic and functionality, with a brief description of each provided.\n\n')
main_toc.write(".. csv-table::\n\n")

########################################################################
## main loop through prog_list

## for the main_toc 3 column csv
csv_line = ""
csv_index = 1

for afni_prog in prog_list:

    ## skip the progs that hang on -help
    if afni_prog in ["qdelaunay","qhull","scan_niml_vals.csh"] :
        continue

    ## read in the help file
    stat,help_in = au.exec_tcsh_command(afni_prog+' -help',
                                     lines=1,
                                     noblank=0)

    ## check to see if there was a help
    if len(help_in) < 1:
        # print(help_in)
        print("ERROR: "+afni_prog+" not found or -help failed!")
        # sys.exit(1)
    else:
        print(afni_prog)

    ## add to main_toc as a 3 column csv
    if csv_index == 1:
        csv_line = "   :ref:`"+afni_prog+" <ahelp_"+afni_prog+">`,"
        csv_index = 2
    elif csv_index == 2:
        csv_line = csv_line+":ref:`"+afni_prog+" <ahelp_"+afni_prog+">`,"
        csv_index = 3
    elif csv_index == 3:
        csv_line = csv_line+":ref:`"+afni_prog+" <ahelp_"+afni_prog+">`"
        main_toc.write("   "+csv_line+"\n")
        csv_index = 1

    ## open file for writing
    out_file = OutFolder+afni_prog+"_sphx.rst"
    sphinx_out = open(out_file,"w")

    ## main header as the prog name
    sphinx_out.write(".. _ahelp_"+afni_prog+":\n\n")
    sphinx_out.write((str("*") * len(afni_prog))+"\n")
    sphinx_out.write(afni_prog+"\n")
    sphinx_out.write((str("*") * len(afni_prog))+"\n\n")

    ## table of contents and a blank to remove the indentation for the
    ## next line
    sphinx_out.write(".. contents:: :local:\n") # [PT: Aug 29, 2018]
    #sphinx_out.write("    :depth: 4 \n\n")
    sphinx_out.write("| \n\n")

    ## flag for the existence of codes
    has_codes = 0

    ## loop through the lines of the help
    for l in range(0,len(help_in)-1):

        ## get code duh
        code = get_code(help_in[l])

        ## check if it is all the same character (some separator)
        if len(set(help_in[l].rstrip().lstrip())) == 1:
            sphinx_out.write("")

        ######################################
        ## we can has code?
        elif code in hdr_codes:

            ## set the no_codes to something else
            has_codes = 1

            ## strip the current line
            cur_line = help_in[l].rstrip().lstrip()[0:-4]

            ## give the appropriate header punctuation and the header
            ## line
            if code == '1':
                sphinx_out.write("\n"+cur_line+"\n")
                sphinx_out.write((str("=") * len(cur_line))+"\n\n")

                if next_line(help_in,l) == 0:
                    sphinx_out.write(".. code-block:: none\n\n")


            elif code == '2':
                sphinx_out.write("\n"+cur_line+"\n")
                sphinx_out.write((str("+") * len(cur_line))+"\n\n")
                sphinx_out.write(".. code-block:: none\n\n")
            elif code == '3':
                sphinx_out.write("\n"+cur_line+"\n")
                sphinx_out.write((str("~") * len(cur_line))+"\n\n")
                sphinx_out.write(".. code-block:: none\n\n")
            elif code == '4':
                ## break line then line as code
                sphinx_out.write((str("-") * len(cur_line))+"\n\n")
                sphinx_out.write(".. code-block:: none\n\n")
                sphinx_out.write("    "+help_in[l][0:-4]+"\n")

        else:
            ## write out the line indented to match the code block
            sphinx_out.write("    "+help_in[l]+"\n")

    ## close the sphinx out
    sphinx_out.close()

    ###############################################################
    ## if no has codes
    if has_codes == 0:

        ## open file for writing
        sphinx_out = open(out_file,"w")

        ## table of contents and a blank to remove the indentation for
        ## the next line
        sphinx_out.write(".. _ahelp_"+afni_prog+":\n\n")
        sphinx_out.write((str("*") * len(afni_prog))+"\n")
        sphinx_out.write(afni_prog+"\n")
        sphinx_out.write((str("*") * len(afni_prog))+"\n\n")

        ## table of contents
        sphinx_out.write(".. contents:: :local:\n") # [PT: Aug 29, 2018]
        #sphinx_out.write("    :depth: 4 \n\n")
        sphinx_out.write("| \n\n")

        ## set for code block for all
        sphinx_out.write(".. code-block:: none\n\n")

        ## loop through the lines of the help
        for l in range(0,len(help_in)-1):
            sphinx_out.write("    "+help_in[l]+"\n")

    ## close the sphinx out
    sphinx_out.close()

    ## reset code flag
    has_codes = 0

## close the main_toc
main_toc.close()
