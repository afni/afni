#!/usr/bin/env python
#
# Version 1.1, Jan, 2015.
# written:  PA Taylor (UCT, AIMS).
#
# Modernize the format of *.grid files for fat_mvm_prep.py.  For a
# long time, the matrices weren't explicitly labelled using the
# commented '#' lines.  Now they are, and life is better for all.
# This program 'modernizes' the format of bare *.grid files.
# 
# As of Jan 2015, it also modernizes bare *.netcc files (though it would
# likely take a very old version of 3dNetCorr to have produced such).
#
# for help on commandline running and usage:
# $  fat_mvm_convert.py -h
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
     ++ Jan, 2015.
     ++ Preprocess some 'old school' (=poorly formatted) *.grid files
           so that they can be fat_mvm_prep'ed for statistical modeling
           using 3dMVM.
     ++ written by PA Taylor.
     
     This program is designed to convert old 3dTrackID output *.grid files
     (which have no labels in '#'-started comments) into modern format.
     This program reads in individual or a set of old *.grid files, and
     outputs new ones in the same folder with '_MOD.grid' postfix (or
     an explicit output prefix can be entered using '--list_match').
     
     This program now also applies to updating *.netcc files that have 
     Pearson correlation ('CC') and Fisher Z-transform matrices ('FZ') but
     no labels in '#'-started comments in the modern format.  The same 
     output naming conventions/options as above apply.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     TO USE (from a terminal commandline):\n
      $ fat_mvm_gridconv.py { -m MATR_FILES | -l LIST }
     where:
        -m, --matr_in=MATR_FILES :one way of providing the set of matrix
                                  (*.grid) files- by searchable path.
                                  This can be a globbable entry in quotes
                                  containing wildcard characters, such as
                                  'DIR1/*/*000.grid'.
        -l, --list_match=LIST    :another way of inputting the matrix
                                  (*.grid) files-- by explicit
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
                                  default '_MOD.grid' postfix is applied.
     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     Example:
       $ fat_mvm_gridconv.py --matr_in='./GROUP/*/*_000.grid' 
     or, equivalently:
       $ fat_mvm_gridconv.py -m './GROUP/*/*_000.grid' 

-----------------------------------------------------------------------------
\n'''

    file_csv = ''
    file_matr_glob = ''
    file_prefix = ''
    file_listmatch = ''

    # allow -help with exit status 0   21 Sep 2018 [rickr]
    for opt in ("-h", "--help", "-help"):
        if opt in argv:
            print help_line
            sys.exit()

    try:
        opts, args = getopt.getopt(argv,"hm:l:",["matr_in=","list_match="])
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

    if ( file_matr_glob == '' ) and ( file_listmatch == '' ):
        print "** ERROR: missing a necessary matrix file input."
        print "\t Need to use either '-m' or '-l'."
        sys.exit()
    if not( file_matr_glob == '' ) and not( file_listmatch == '' ):
        print "*+ Warning: both a path for globbing *and* a listfile have",
        print " been input for the matrix file."
        print "\tThe glob one after '-m' will be ignored."

    return file_matr_glob, file_listmatch


########################################################################

if __name__=="__main__":
    set_printoptions(linewidth=200)
    print "\n"
    file_matr_glob, file_listmatch = main(sys.argv[1:])

    # get file list from either of two ways.
    if file_listmatch:
        list_all = GR.ReadSection_and_Column(file_listmatch, 0)
    elif file_matr_glob:
        list_all = glob(file_matr_glob)
        list_all.sort()
    else:
        print "** Error! Cannot read in matrix files."
        sys.exit(4)

    # this one gets the matched pair name.
    if GR.IsFirstUncommentedSection_Multicol(file_listmatch):
        list_all_out = GR.ReadSection_and_Column(file_listmatch, 1)
    else:
        list_all_out = GR.DefaultNamingOutGrid(list_all)

    N = len(list_all)
    if not( N ==len(list_all_out)) :
        print '** Error: unmatched input and output names'
        sys.exit(6)

    for i in range(N):
        p = GR.OldFash_Grid_modernize(list_all[i], list_all_out[i])
        print "Converted %15s -> %15s" % (list_all[i], list_all_out[i])

    if 1:
        print "++ DONE.\n\n"
    else:
        print "\n** Some error in file writing of table file."
        print "\tSo close, yet so far."
