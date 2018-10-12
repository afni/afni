#!/usr/bin/env python
#
# Version 1.0, Sept, 2014.
# written:  PA Taylor (UCT, AIMS).
# Updated, ver 1.2: Sept 2014
#
# Select a single row out of a connectivity matrix file (*.grid
# or *.netcc) for viewing and/or further analysis.
# 
# 
# 
#
#  # for help on commandline running and usage:
#  $  fat_roi_row.py -h
#
#
###########################################################################
###########################################################################


import lib_fat_funcs as GR
from numpy import set_printoptions
import getopt, sys 
from glob import glob

def main(argv):
    '''Basic reading in of commandline options.'''

    help_line = ''' \
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     ++ Sept, 2014.

     ++ Select a single ROI's row out of a connectivity matrix file (*.grid
           or *.netcc) for viewing and/or further analysis.
     ++ written by PA Taylor.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     TO USE (from a terminal commandline):

      $ fat_roi_row.py  -r ROI  { -m MATR_FILES | -l LIST }
     where:
        -r, --roi=ROI            :specify which ROI's row of connectivity you
                                  want to select out.
                                  If you have used labeltables in your tracking
                                  and output, then you may select the ROI by
                                  using either the string label (checked first;
                                  probably should be put in single quotation
                                  marks) or by the ROI mask number.  
        -m, --matr_in=MATR_FILES :one way of providing the set of matrix
                                  (*.grid or *.netcc) file(s)- by searchable 
                                  path. This can be a globbable entry in quotes
                                  containing wildcard characters, such as
                                  'DIR1/*/*000.grid'.
        -l, --list_match=LIST    :another way of inputting the matrix
                                  (*.grid or *.netcc) files-- by explicit
                                  path in a text file.
                                  The LIST text file must contain at least
                                  one column:
                                  col 1: path to subject matrix file.
                                  with an optional second column:
                                  col 2: output file names.
                                  (NB: columns must be the same length.)
                                  The first line can be '#'-commented,
                                  which is not read for filenames).
                                  If no second column is given, then the
                                  default naming convention is applied:
                                  NAME.grid   ->  NAME_grid_ROI.row
                                  NAME.netcc  ->  NAME_netcc_ROI.row
                                  where 'ROI' would be the 3-zero-padded 
                                  ROI label.
      -E, --ExternLabsNo         :switch to turn off the writing/usage of 
                                  user-defined labels in the *.grid/*.netcc 
                                  files.  Can't see why this would be desired,
                                  to be honest.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     Example:
       $ fat_roi_row.py  --roi=3  --matr_in='./GROUP/*/*_000.grid' 
     or, equivalently:
       $ fat_roi_row.py  -r 3  -m './GROUP/*/*_000.grid' 

-----------------------------------------------------------------------------
\n'''

    file_matr_glob = ''
    file_listmatch = ''
    ele = 0
    SWITCH_ExternLabsOK = 1

    # allow status 0 on -help   24 Sep 2018 [rickr]
    if "-help" in argv:
        print help_line
        sys.exit()

    try:
        opts, args = getopt.getopt(argv,"hEm:l:r:",[ "help",
                                                     "ExternLabsNo",
                                                     "matr_in=",
                                                     "list_match=",
                                                     "roi="])
    except getopt.GetoptError:
        print help_line
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print help_line
            sys.exit()
        elif opt in ("-m", "--matr_in"):
            file_matr_glob = arg
        elif opt in ("-l", "--list_match"):
            file_listmatch = arg
        elif opt in ("-r", "--roi"):
            ele = arg
        elif opt in ("-E", "--ExternLabsNo"):
            SWITCH_ExternLabsOK = 0

    if ( file_matr_glob == '' ) and ( file_listmatch == '' ):
        print "** ERROR: missing a necessary matrix file input."
        print "\t Need to use either '-m' or '-l'."
        sys.exit()
    if not( file_matr_glob == '' ) and not( file_listmatch == '' ):
        print "*+ Warning: both a path for globbing *and* a listfile have",
        print " been input for the matrix file."
        print "\tThe glob one after '-m' will be ignored."

    return file_matr_glob, file_listmatch, ele, SWITCH_ExternLabsOK

########################################################################



if __name__=="__main__":
    set_printoptions(linewidth=200)
    print "\n"
    file_matr_glob, file_listmatch, ele,  ExternLabsOK \
     = main(sys.argv[1:])

    # get file list from either of two ways.
    if file_listmatch:
        list_all = GR.ReadSection_and_Column(file_listmatch, 0)
    elif file_matr_glob:
        list_all = glob(file_matr_glob)
    else:
        print "** Error! Cannot read in matrix files."
        sys.exit(4)

    if not(list_all):
        print "** Error! Could not find/read in any matrix files."
        sys.exit(4)

    # this one gets the matched pair name.
    if GR.IsFirstUncommentedSection_Multicol(file_listmatch):
        list_all_out = GR.ReadSection_and_Column(file_listmatch, 1)
    else:
        # we still allow selection by ROI number, not label, so we
        # check and see if 'ele' is purely a digit (-> number, to
        # zero-pad) or not (-> label, to use 'as is')
        if ele.isdigit():
            # it's a digit, so name like a digit -> zero-pad name
            list_all_out = GR.DefaultNamingOutRowfile( list_all, 
                                                       ele,
                                                       0)
        else:
            # treat it 'as is', unless user turns off ExternLabs
            list_all_out = GR.DefaultNamingOutRowfile( list_all, 
                                                       ele,
                                                       ExternLabsOK)


    N = len(list_all)
    if not( N ==len(list_all_out)) :
        print '** Error: unmatched input and output names'
        sys.exit(6)

    for i in range(N):
        p = GR.MakeRowFile_From_FATmat(list_all[i], 
                                       list_all_out[i], 
                                       ele,
                                       ExternLabsOK)

        print "++ Wrote:  %s" % list_all_out[i]

    if 1:
        print "++ DONE.\n\n"




