#!/usr/bin/env python
#
# Version 1.0, Oct, 2014.
# written:  PA Taylor (UCT, AIMS).
#
#
#
#  # for help on commandline running and usage:
#  $ fat_lat_csv.py -h
#
#
###########################################################################
###########################################################################


import lib_fat_funcs as GR
import lib_fat_Rfactor as FR
from numpy import set_printoptions
import getopt, sys 
import numpy as np


def main(argv):
    '''Basic reading in of commandline options.'''

    help_line = '''\
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    ++ Oct, 2014.  Written by PA Taylor (UCT/AIMS).  
    ++ Perform factor analysis on quantitative CSV data to find 
       latent variables.
     
    This program is designed to help prep subject data (clinical,
    neurophysiological, test scores, etc.) for statistical analysis,
    for example using 3dMVM (written by G. Chen).

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This program reads in a group-worth of information from a CSV file
    (which could be dumped from a study spreadsheet), and calculates
    latent variables using factor analysis. Default is to perform
    factor analysis for all quantitative variables in the CSV file, as
    well as for all subjects.  If you want a subset of variables to be
    used only, you can input these via commandline list or as a file.
    If you want a subset of subjects used, this can be selected by
    globbing a list of *netcc or *.grid files from 3dNetCorr or
    3dTrackID creation, respectively (or, you can always edit your CSV
    file to chop out subjects).  The output CSV file is a copy of the
    input, with the set of variables input into factor analysis
    *replaced* by the estimated latent factors.

    NB: *Currently*, columns of data with missing data (->'NA') will
    be ignored during factor analysis processing but passed along to
    the output.  This might change over time, moving to a point of
    analysis phase space where data with NA values can somehow be
    treated in the formation of latent variables.

    Here, Factor analysis is performed using the existing R function
    'factanal()', as well as the function 'paran()' if you choose to
    use parallel analysis to determine the number of factors from the
    data.  For more on those methods, please read their individual R
    documentations.  Each function has maaany available options; for
    the most part, we use default settings, but also allow some
    further manipulation.  If you really need some other parameter
    settings to be directly input from the Python commandline, let the
    author know.  Two important parameters currently used in the
    factanal() function are: scores='regression', and
    rotation="varimax".  If you have strong opinions about this, then
    great.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    OUTPUT: 

    + new *.csv file, with a set of quantitative variables replaced by
    latent factors. 

    + (option) an image file of Horn's Test, if you use parallel
    analysis to determine the number of latent factors.

    + a file recording loading factors of input variables for each
    output latent variable, so you can try to interpret the 'meaning'
    of the latent variables. Buon fortuna...
     
     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     TO USE (from a terminal commandline):\n
      $ fat_lat_csv.py  -p PREFIX  -c CSV_FILE                    \\
                       { -m MATR_FILES | -l LIST }                \\
                       { -v, --vars='X Y Z ...' | -f VAR_FILE }   \\
                       -N                                         \\
                       -L FACLAB  -C NLOAD   -n NF                \\
                       -P PERC   -I N_ITER                        \\
                       -g   -i ITYPE 
        
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

        -m, --matr_in=MATR_FILES :[optional here-- can be used to select
                                  a subset of subjects in the CSV file;
                                  otherwise all CSV subjects are used in 
                                  computations]
                                  one way of providing the set of matrix
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
        -l, --list_match=LIST    :[optional here (see previous option)]
                                  another way of inputting the matrix
                                  (*.grid or *.netcc) files-- by explicit
                                  path, matched per file with a CSV
                                  subject ID.
                                  The LIST text file contains two columns:
                                  col 1: path to subject matrix file.
                                  col 2: CSV IDs,
                                  (first line can be a '#'-commented one).

        -v, --vars='X Y Z ...'   :one method for supplying a list of
                                  quantitative variables from which one wants
                                  to calculate latent variables. Names must
                                  be separated with whitespace. If no list of
                                  variables is input, then *all* quantitative
                                  variables in the CSV_FILE are used.
        -f, --file_vars=VAR_FILE :the second method for supplying a list of
                                  quantitative variables.  VAR_FILE is a text
                                  file with a single column of variable
                                  names. If no list of variables is input,
                                  then *all* quantitative variables in the
                                  CSV_FILE are used.

        -N, --NA_warn_off        :switch to turn off the automatic
                                  warnings as the data table is created. 
                                  (Default is to warn.)

    -L, --Label_factors=FACLAB   :Prefix string for latent variable column 
                                  in the output CSV, so that the names are:
                                  'FACLAB_01', 'FACLAB_02', etc.
                                  (Default: 'FACTOR'.)
    -C, --Count_loads=NLOAD      :When reporting loading factors for each 
                                  latent factor (that is, essentially what is
                                  the correlation of the most-related input
                                  variables to this particular factor), report
                                  NLOAD many values. If NLOAD<0, then ALL get 
                                  reported. (Default: 5.)

        -n, --num_facs=NF        :specify the number of latent factors 'NF'
                                  in which to decompose your variable data
                                  using factor analysis (default is to not use
                                  this, but instead to perform parallel 
                                  analysis, which has options below.)

     If not using an explicit number of factors, parallel analysis is
        performed to estimate the number of latent factors from the data, with
        the following options for Horn's test in the R function 'paran()':

    -P, --Percentile=PERC        :percentile value used in estimating bias;
                                  'percentile' in paran().
                                  (Default: conservative value of 99.)
    -I, --Iter_Horn=N_ITER       :number of Monte Carlo iterations in Horn's 
                                  test; 'iterations' in paran().  
                                  (Default: 5000.)
    -g, --graph_on               :switch to turn on saving the paran()-produced
                                  graph of Horn's test results. File prefix
                                  will be PREFIX string input above; file
                                  type can be set with the next option.
    -i, --image-type=ITYPE       :if 'graph_on' is used, then you can choose
                                  the output image file type from: 
                                      'jpeg', 'pdf', 'png', 'tiff'.
                                  (Default: 'jpeg'.)

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     Example:
            fat_lat_csv.py -c Subject_data.csv     \\
                           -p Subject_data_LAT     \\
                           -N  -g   -C -1
         or, equivalently,
            fat_lat_csv.py --csv_in='Subject_data.csv'     \\
                           --prefix='Subject_data_LAT'     \\
                           --NA_warn_off --graph_on        \\
                           --Count_loads=-1

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   This program is part of AFNI-FATCAT:
    Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And
    Tractographic Connectivity Analysis Toolbox. Brain Connectivity.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'''

    comm_str = 'fat_lat_csv.py '

    file_csv = ''
    file_matr_glob = ''
    file_prefix = ''
    file_listmatch = ''
    SWITCH_NAwarn = 1
    SWITCH_ExternLabsOK = 1  # user doesn't choose this here
    SWITCH_DoGraph = 0
    IMG_type = FR.PARN_OUT_types[0]
    N_FACS = 0
    file_listmodel = ''
    list_model = ''
    N_LOADS = 0
    fact_lab_pref = ''
    Percentile = 99
    N_iter = 5000

    # allow status 0 on -help   24 Sep 2018 [rickr]
    if "-help" in argv:
        print help_line
        sys.exit()

    try:
        opts, args = getopt.getopt(argv,"hNgc:m:l:i:n:C:L:P:I:p:v:f:",
                                   ["help",
                                    "NA_warn_off",
                                    "graph_on",
                                    "csv_in=",
                                    "matr_in=",
                                    "list_match=",
                                    "image_type=",
                                    "num_facs=",
                                    "Count_loads=",
                                    "Label_factors=",
                                    "Percentile=",
                                    "Iter_Horn=",
                                    "prefix=",
                                    "vars=", 
                                    "file_vars="])

    except getopt.GetoptError:
        print "** Error reading options. Try looking at the helpfile:"
        print "\t $  fat_mvm_prep.py -h\n"
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-h", "--help", "-help"):
            print help_line
            sys.exit()
        elif opt in ("-N", "--NA_warn_off"):
            SWITCH_NAwarn = 0
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-g", "--graph_on"):
            SWITCH_DoGraph = 1
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-c", "--csv_in"):
            file_csv = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-m", "--matr_in"):
            file_matr_glob = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-p", "--prefix"):
            file_prefix = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-l", "--list_match"):
            file_listmatch = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-v", "--vars"):
            list_model = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-f", "--file_vars"):
            file_listmodel = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-i", "--image_type"):
            IMG_type = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-n", "--num_facs"):
            N_FACS = int(arg)
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-C", "--Count_loads"):
            N_LOADS = int(arg)
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-L", "--Label_factors"):
            fact_lab_pref = arg
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-P", "--Percentile"):
            Percentile = int(arg)
            comm_str = GR.RecapAttach(comm_str, opt, arg)
        elif opt in ("-I", "--Iter_Horn"):
            N_iter = int(arg)
            comm_str = GR.RecapAttach(comm_str, opt, arg)

            
    if N_FACS <0 :
        print "** ERROR: bad number of factors asked for: ",N_FACS
        sys.exit()
    if (Percentile <=0) or (Percentile >=100):
        print "** ERROR: bad percentile asked for; should be (0, 100)."
        sys.exit()
    if not( FR.PARN_OUT_types.__contains__(IMG_type) ):
        print "** Error! ",
        print "Output file type '-i %s' is not valid. Select from:" % (IMG_type)
        print "\t",
        for x in FR.PARN_OUT_types:
            print " '"+x+"' ",
        print "\n"
        sys.exit(32)
    if ( file_csv == '' ) or ( file_prefix == '' ) :
        print "** ERROR: missing a necessary input."
        sys.exit()
    if ( file_matr_glob == '' ) and ( file_listmatch == '' ):
        print "*+ No matrices input -> going to use all subjects in calcs."
    if not( file_matr_glob == '' ) and not( file_listmatch == '' ):
        print "*+ Warning: both a path for globbing *and* a listfile have",
        print " been input for the matrix file."
        print "\tThe glob one after '-m' will be ignored."
    if not( list_model == '' ) and not( file_listmodel == '' ):
        print "*+ Warning: both a variable list *and* a variable file have",
        print " been input (file will be given precedence)."

    return comm_str, file_csv, file_matr_glob, file_prefix, file_listmatch, \
     SWITCH_NAwarn, SWITCH_ExternLabsOK, SWITCH_DoGraph, \
     IMG_type, N_FACS, N_LOADS, fact_lab_pref, Percentile, N_iter, \
     list_model, file_listmodel



########################################################################

if __name__=="__main__":
    set_printoptions(linewidth=200)
    print "\n"
    comm_str, file_csv, file_matr_glob, file_prefix, file_listmatch, \
        NA_WARN, ExternLabsOK, SWITCH_DoGraph, IMG_type, N_FACS, N_LOADS, \
        fact_lab_pref, Percentile, N_iter, \
        list_model, file_listmodel = main(sys.argv[1:])

    if not(NA_WARN):
        print "++ Won't warn about NAs in the data."

    arg_list = sys.argv
    str_sep = ' '
    arg_str = str_sep.join(arg_list)

    ### get list of CSV file data
    csv_raw, csv_colvars = GR.LoadInCSV(file_csv)
    csv_data, csv_coltypes = GR.ConvertCSVfromStr(csv_raw, csv_colvars,NA_WARN)
    csv_subj = [x[0] for x in csv_data]
    

    ## For VAR lists
    # get file list from either of two ways.
    var_sublist = []
    if file_listmodel:
        var_sublist = GR.ReadSection_and_Column(file_listmodel, 0)
    elif list_model:
        var_sublist = list_model.split()


    ### NET/GRID-- want col [1] of listfile, if it's there
    grid_subj = [] 
    if file_matr_glob or file_listmatch:
        grid_data, grid_subj = GR.GroupListOfFullMatrs(file_matr_glob, \
                                                           file_listmatch, \
                                                           0)
        if len(grid_subj)==0:
            print "** ERROR: Ended up with no subjects when reading '%s'." \
             %  file_matr_glob
            sys.exit(1)

        # check about grid or not...
        MTYPE = GR.Check_Matr_type(grid_subj)
    
        if not(MTYPE=='GRID' or MTYPE=='NETCC'):
            print "** ERROR: Badness in matrix file reading."
            sys.exit(2)

    if grid_subj:
        grid_ROIlist, grid_PARlist = GR.FindGroupwiseTargets(grid_data, 
                                                             MTYPE,
                                                             ExternLabsOK)

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

        if not(csv_subj):
            print "\n** Error! Must not have found",
            print "any matches between CSV data and matrices!\n"
            sys.exit(26) 
    
    FACTS_of_LIFE, FACTOR_TITLES, \
        USED_NAMES = \
                     FR.R_Factor_Analysis( comm_str,
                                           csv_data,
                                           csv_colvars, 
                                           csv_coltypes,
                                           file_prefix,
                                           var_sublist, 
                                           N_FACS,
                                           N_LOADS,
                                           fact_lab_pref,
                                           SWITCH_DoGraph,
                                           Percentile,
                                           N_iter,
                                           IMG_type)

    csv_data, csv_colvars, \
        csv_coltypes = FR.Out_With_the_Old( csv_data,
                                            csv_colvars,
                                            csv_coltypes,
                                            FACTS_of_LIFE, 
                                            FACTOR_TITLES,
                                            USED_NAMES )
    
    okay = FR.WriteOutCSV( file_prefix,
                         csv_data,
                         csv_colvars,
                         csv_coltypes )
    
    if okay:
        print "\n++ Success!"
        print "++ The CSV file is:  %s.csv" % \
            (file_prefix)
        print "++ The log file is:  %s.log" % \
            (file_prefix)
        if SWITCH_DoGraph:
            print "++ The Horn's test figure file is:  %s.%s" % \
                (file_prefix, IMG_type)
        print "++ DONE.\n\n"
    else:
        print "\n** Some error in file writing of CSV file."
        print "\tSo close, yet so far."
