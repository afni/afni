#!/usr/bin/env python
#
# Version 1.0, July, 2014.
# written:  PA Taylor (UCT, AIMS).
#
#
# Combine CSV data and matrix file output (currently either *.grid
# from 3dTrackID or *.netcc from 3dNetCorr) into a format readable by
# 3dMVM (written by G. Chen).  One can then perform statistical
# modeling to combine the structural/functional network properties
# with (clinical, neurophysiological, subject-specific, etc.)
# measures.
#
#  # for help on commandline running and usage:
#  $ fat_mvm_prep.py -h
#
#
###########################################################################
###########################################################################


import FAT_MVM_GRID_READERS as GR
from numpy import set_printoptions
import getopt, sys 


def main(argv):
    '''Basic reading in of commandline options.'''

    help_line = '''\
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     ++ July, 2014.  Written by PA Taylor.
     ++ Combine FATCAT output with CSV data for statistical modeling.
     
     This program is designed to prep and combine network-based data from
     an MRI study with other subject data (clinical, neurophysiological,
     genetic, etc.) for repeated measure, statistical analysis with
     3dMVM (written by G. Chen).

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     This program reads in a group-worth of information from a CSV file
     (which could be dumped from a study spreadsheet) as well as the
     group\'s set of matrix files output by either 3dTrackID (*.grid)
     or by 3dNetCorr (*.netcc).  Then, it outputs a tabular text
     (*_MVMtbl.txt) file which can be called straightforwardly in 3dMVM.
     
     It also produces a record (*_MVMprep.log) of: how it matched CSV
     subject IDs with matrix file paths (for user verification); a list
     of the ROIs found across all subjects (which are the only information
     that is stored in the *_MVMtbl.txt file-- no analysis with missing
     data is performed currently); and a list of the matrix file
     parameters that were found for all subjects and entered into the
     *_MVMtbl.txt file.
     
     The *_MVMtbl.txt file contains subject information, one subject per
     row.  The titles of some columns are keywords:
     - the first col must have header 'Subj' and contain the subject
       identifiers;
     - the penultimate col must have header 'matrPar' and contain
       parameter identifiers ('FA', 'CC', etc.);
     - the last col must have header 'Ausgang_val' and contain the
       numerical parameter values themselves, e.g. output by 3dTrackID or
       3dNetCorr.
     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     TO USE (from a terminal commandline):\n
      $ fat_mvm_prep.py  -p PREFIX  -c CSV_FILE \\
                       { -m MATR_FILES | -l LIST}
     
     where:
        -p, --prefix=PREFIX      :prefix for output files.
        -c, --csv_in=CSV_FILE    :name of comma-separated variable (CSV)
                                  file for input. Format notes: each row
                                  contains a single subject\'s data, and
                                  the first row contains column/variable
                                  labels (with no spaces in them); first
                                  column is subject IDs (no spaces); and
                                  factor/categorical variables (gender,
                                  etc.) should be recorded with at least
                                  one character (i.e., M/F and not 0/1). 
                                  I will replace spaces in the first row 
                                  and column.
        -m, --matr_in=MATR_FILES :one way of providing the set of matrix
                                  (*.grid or *.netcc) files- by searchable
                                  path.  This can be a globbable entry in
                                  quotes containing wildcard characters,
                                  such as 'DIR1/*/*000.grid'.
                                  If this option is used instead of '-l',
                                  below, then this program tries to match
                                  each CSV subj ID to a matrix file by 
                                  finding which matrix file path in the
                                  MATR_FILES contains a given ID string;
                                  this method may not always find unique
                                  matches, in which case, use '-l'
                                  approach.
        -l, --list_match=LIST    :another way of inputting the matrix
                                  (*.grid or *.netcc) files-- by explicit
                                  path, matched per file with a CSV
                                  subject ID.
                                  The LIST text file contains two columns:
                                  col 1: path to subject matrix file.
                                  col 2: CSV IDs,
                                  (first line can be a '#'-commented one.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     Example:
        $ fat_mvm_prep.py  --prefix='study'                    \\
                            --csv_in='allsubj.csv'             \\
                            --matr_in='./GROUP/*/*_000.grid' 
     or, equivalently:
        $ fat_mvm_prep.py -p study -c allsubj.csv -m './GROUP/*/*_000.grid' 

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   This program is part of AFNI-FATCAT:
    Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And
    Tractographic Connectivity Analysis Toolbox. Brain Connectivity.

   For citing the statistical approach, please use the following:
    Chen, G., Adleman, N.E., Saad, Z.S., Leibenluft, E., Cox, R.W. (2014).
    Applications of Multivariate Modeling to Neuroimaging Group Analysis:
    A Comprehensive Alternative to Univariate General Linear Model. 
    NeuroImage 99:571-588.
    http://afni.nimh.nih.gov/pub/dist/HBM2014/Chen_in_press.pdf

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'''


    file_csv = ''
    file_matr_glob = ''
    file_prefix = ''
    file_listmatch = ''

    try:
        opts, args = getopt.getopt(argv,"hc:m:p:l:",["csv_in=",
                                                     "matr_in=",
                                                     "prefix=",
                                                     "list_match="])
    except getopt.GetoptError:
        print help_line
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print help_line
            sys.exit()
        elif opt in ("-c", "--csv_in"):
            file_csv = arg
        elif opt in ("-m", "--matr_in"):
            file_matr_glob = arg
        elif opt in ("-p", "--prefix"):
            file_prefix = arg
        elif opt in ("-l", "--list_match"):
            file_listmatch = arg

    if ( file_csv == '' ) or ( file_prefix == '' ) :
	print "** ERROR: missing a necessary input."
    	sys.exit()
    if ( file_matr_glob == '' ) and ( file_listmatch == '' ):
	print "** ERROR: missing a necessary matrix file input."
        print "\t Need to use either '-m' or '-l'."
        sys.exit()
    if not( file_matr_glob == '' ) and not( file_listmatch == '' ):
        print "*+ Warning: both a path for globbing *and* a listfile have",
        print " been input for the matrix file."
        print "\tThe glob one after '-m' will be ignored."

    return file_csv, file_matr_glob, file_prefix, file_listmatch


########################################################################

if __name__=="__main__":
    set_printoptions(linewidth=200)
    print "\n"
    file_csv, file_matr_glob, file_prefix, file_listmatch     \
     = main(sys.argv[1:])

    arg_list = sys.argv
    str_sep = ' '
    arg_str = str_sep.join(arg_list)

    ### get list of CSV file data
    csv_raw, csv_colvars = GR.LoadInCSV(file_csv)
    csv_data, csv_coltypes = GR.ConvertCSVfromStr(csv_raw, csv_colvars)
    csv_subj = [x[0] for x in csv_data]
    
    ### NET/GRID-- want col [1] of listfile, if it's there
    grid_data, grid_subj = GR.GroupListOfFullMatrs(file_matr_glob, \
                                                       file_listmatch, \
                                                       0)

    if len(grid_subj)==0:
        print "** ERROR: Ended up with no subjects when reading '%s'." \
         %  file_matr_glob
        sys.exit(1)

    ## check about grid or not...
    MTYPE = GR.Check_Matr_type(grid_subj)
    
    if not(MTYPE=='GRID' or MTYPE=='NETCC'):
        print "** ERROR: Badness in matrix file reading."
        sys.exit(2)

    if grid_subj:
        grid_ROIlist, grid_VARlist = GR.FindGroupwiseTargets(grid_data, MTYPE)

    # Match the CSV_subjectIDs and the GRID_subject list. This is
    # based on A) just the filenames and CSV labels; or B) explicit
    # input file with two columns.
    DICT_CSV_grid = GR.MakeDict_CSV_to_grid(grid_subj,           \
                                                csv_subj,        \
                                                file_listmatch)

    # check dictionary, and make new csv_* if necessary;
    # but after this, these *are* all the csv_* with matching matrices
    csv_data, csv_subj = \
     GR.CheckDict_CSV_to_grid(DICT_CSV_grid,          \
                                  grid_subj,          \
                                  csv_subj,           \
                                  csv_data)

    temp = GR.Write_CSVandMatrix_log(file_prefix,        \
                                        DICT_CSV_grid,   \
                                        grid_ROIlist,    \
                                        grid_VARlist,    \
                                        arg_str)

    grid_tabled = GR.MakeGridTableau(DICT_CSV_grid,     \
                                         csv_data,      \
                                         grid_data,     \
                                         grid_ROIlist,  \
                                         grid_VARlist,  \
                                         grid_subj)

    temp2 = GR.Write_MVM_File(file_prefix,           \
                                  csv_data,          \
                                  csv_colvars,       \
                                  grid_ROIlist,      \
                                  grid_VARlist,      \
                                  grid_tabled)

    if temp2:
        print "\n++ Success!"
        print "++ The logfile of CSV and matrix matches is:  %s%s." % \
         (file_prefix, GR.MVM_matchlog_postfix)
        print "++ The data table for reading into 3dMVM is:  %s%s." % \
         (file_prefix, GR.MVM_file_postfix)
        print "++ DONE.\n\n"
    else:
        print "\n** Some error in file writing of table file."
        print "\tSo close, yet so far."
