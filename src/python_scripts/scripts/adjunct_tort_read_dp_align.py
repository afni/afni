#!/usr/bin/env python

# python3 status: compatible

# ver = 1.1 ; date = April 2, 2019
# + [PA Taylor] Fix enorm values, to be *derivatives* of mot pars.
#   Yikes.
#
# ======================================================================

import sys

RADtoDEG = 180./3.141592653589793 

helpy_list = ['-h', '-hview', '-help']

HELP_STRING = '''

Overview ~1~

This program is just meant to be used via: adjunct_tort_plot_mot.
Please see that program for help usage.  It extracts the 3 translation
(in mm) and 3 rotation (in deg) parameters estimated by TORTOISE's
DIFF_PREP tool during DWI processing.

auth:  PA Taylor

Usage ~1~

If you really, really need to use this program separately (why?  you
will miss out on the *pictures*!), then we will note that you can run
this program with precisely two arguments, as:

adjunct_tort_read_mot_transforms.py  \\
    IN_FILE                          \\
    OUT_FILE

... where:

   IN_FILE = *_transformations.txt file output by TORTOISE's DIFF_PREP.

  OUT_FILE = a '1D' file, in AFNI-ese.  Basically, a text file with 6
             columns and with the same number of columns as input
             DWIs.  The columns represent:

                  Column #0 : del x (for axial data, RL)
                  Column #1 : del y (for axial data, AP)
                  Column #2 : del z (for axial data, IS)
                  Column #3 : Rx
                  Column #4 : Ry
                  Column #5 : Rz

Note ~1~

This program (and its partner-in-crime, adjunct_tort_plot_mot) have
been checked with TORTOISE versions 3.1* - 3.2.  Please contact the
TORTOISE group if you have any doubts/questions about the input file
format (you can cc us AFNI folks, too).

'''

def read_tortoise_transformations(my_file):
    '''
    Read in a TORTOISE (DIFFPREP-)produced *_transformations.txt file
    and output a simple text file of 6 columns in TORT order, but with 
    rot in deg:  translations (mm) and 3 rotations (deg).

    output order of columns:

        Column #0 > x (for axial data = RL)
        Column #1 > y (for axial data = AP)
        Column #2 > z (for axial data = IS)
        Column #3 > Rx
        Column #4 > Ry
        Column #5 > Rz

    Tested/used on TORTOISE v3.1; may work on transformations.txt
    files from different versions of the same software.

    '''

    fff = open( my_file ,'r')
    raw_data = fff.readlines()
    fff.close()

    data_list1 = []
    for line in raw_data:
        aa = []
        #w = line.translate(None,"[]")
        w = line.replace("[", "")
        w = w.replace("]", "")
        x = w.split(',')
        Nx = len(x)
        if Nx < 6:
            sys.exit("** Problem in this line:\n %s\n\n" % line)
        for i in range (6):
            aa.append( float(x[i]) )
        for i in range(3, 6):
            aa[i]*= RADtoDEG
        data_list1.append(aa)

    return data_list1


def write_tortoise_transformations(ofile, ilist):

    Nrow = len(ilist)

    fff = open(ofile, mode='w')

    for i in range(Nrow):
        rr = ilist[i]
        for cc in rr:
            fff.write("{:15.8f}".format(cc))
        if i<Nrow-1:
            fff.write("\n")

    fff.close()

    return 0

def check_for_help(arg_list):
    
    if not(len(arg_list)) :
        print(HELP_STRING)
        return 1
    elif (len(arg_list) == 1) or (len(arg_list) > 2):
        print(HELP_STRING)
        return 1

    if helpy_list.__contains__(arg_list[0]) :
        print(HELP_STRING)
        return 1




    return 0

# =========================================================================

if __name__ == "__main__":

    arg_list = sys.argv[1:]

    is_help = check_for_help(arg_list)

    if is_help :
        sys.exit(0)

    tort_mot = read_tortoise_transformations(arg_list[0])
    
    out      = write_tortoise_transformations(arg_list[1], tort_mot)

    print("++ Wrote out file: {}".format(arg_list[1]))

    sys.exit(0)
