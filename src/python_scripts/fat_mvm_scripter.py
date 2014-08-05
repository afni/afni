#!/usr/bin/env python
#
# Version 1.0, July, 2014.
# written:  PA Taylor (UCT, AIMS).
#
# Take the output of fat_mvm_prep.py and run statistical models using
# 3dMVM.  This will allow user to build a simple 'repeated measures'
# model by choosing a set of covariates; run the network-level model
# using 3dMVM; and run follow-up post hoc tests for each ROI.
# 
#
#  # for help on commandline running and usage:
#  $  fat_mvm_scripter.py -h
#
#
###########################################################################
###########################################################################


import FAT_MVM_GRID_READERS as GR
from numpy import set_printoptions
import getopt, sys 
from glob import glob


def main(argv):
    '''Basic reading in of commandline options.'''

    help_line = '''\
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     ++ July, 2014.  Written by PA Taylor.
     ++ Read in a data table file (likely formatted using the program
        fat_mvm_prep.py) and build an executable command for 3dMVM 
        (written by G Chen) with a user-specified variable model. This
        should allow for useful repeated measures multivariate modeling
        of networks of data (such as from 3dNetCorr or 3dTrackID), as
        well as follow-up analysis of subconnections within the network.
     

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     + INPUTS: 
        1) Group data table text file (formatted as the *_MVMtbl.txt file
           output by fat_mvm_prep.py); contains subject network info (ROI
           parameter values) and individual variables.
        2) Log file (formatted as the *_MVMprep.log file output by 
           fat_mvm_prep.py) containing, among other things, a list of
           network ROIs and a list of parameters whose values are stored
           in the group data table.
        3) A list of variables, whose values are also stored in the group
           data table, which are to be statistically modeled.  The list
           may be provided either directly on the commandline or in a 
           separate text file.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     + OUTPUTS
       1a) A text file (named PREFIX_scri.tcsh) containing a script for
           running 3dMVM, using the prescribed variables along with each 
           individual parameter.  If N parameters are contained in the 
           group data table and M variables selected for the model, then
           N network-wise ANOVAs for set of M+1 (includes the intercept)
           effects will be run.
           Additionally, if there are P ROIs comprising the network,
           then the generated script file is automatically set to perform
           PxM "post hoc" tests for the interactions of each ROI and
           each variable (if the variable is categorical, then there are
           actually more tests-- using one for each subcategory).
           This basic script can be run simply from the commandline:
           $  tcsh PREFIX_scri.tcsh
           after which ...
       1b) ... a text file of the test results is saved in a file
           called  "PREFIX_MVM.txt".
           Results in the default *MVM.txt file are grouped by variable,
           first producing a block of ANOVA output with three columns
           per variable:
           Chi-square value, degrees of freedom, and p-value.
           This is followed by a block of post hoc testing output with
           four columns:
           test value, t-statistic, degrees of freedom and p-value.
           See 3dMVM for more information.
    
           NB: The '1a' script is a *very basic starter/suggestion*
           for performing statistical tests.  Feel free to modify it as
           you wish for your particular study.  See '3dMVM -help' for more
           information.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     + USAGE:
       $  fat_mvm_scripter.py  --prefix=PREFIX                     \\
            --table=TABLE_FILE  --log=LOG_FILE                     \\
            { --vars='VAR1 VAR2 VAR3 ...' | --file_vars=VAR_FILE } \\
            { --no_posthoc } { --rois='ROI1 ROI2 ROI3 ...' } 
     
      -p, --prefix=PREFIX      :output prefix for script file, which will
                                then be called PREFIX_scri.tcsh, for
                                ultimately creating a PREFIX_MVM.txt file
                                of statistical results from 3dMVM.
      -t, --table=TABLE_FILE   :text file containing columns of subject
                                data, one subject per row, formatted as
                                a *_MVMtbl.txt output by fat_mvm_prep.py (see
                                that program's help for more description.
      -l, --log=LOG_FILE       :file formatted according to fat_mvm_prep.py
                                containing commented headings and also
                                lists of cross-group ROIs and parameters.
                                for which there were network matrices
                                (potentially among other useful bits of
                                information).  See output of fat_mvm_prep.py
                                for more info;  NB: commented headings
                                generally contain selection keywords, so
                                pay attention to those if generating your
                                own.
      -v, --vars='X Y Z ...'   :one method for supplying a list of
                                variables for the 3dMVM model. Names must
                                be separated with whitespace.  Categorical
                                variables will be detected automatically
            *or*                by the presence of nonnumeric characters
                                in their columns; quantitative variables
                                will be automatically put into a list for
                                post hoc tests.
      -f, --file_vars=VAR_FILE :the second method for supplying a list of
                                variables for 3dMVM.  VAR_FILE is a text
                                file with a single column of variable
                                names.
      -r, --rois='A B C ...'   :optional command to be able to select
                                a subset of available network ROIs,
                                if that's useful for some reason (NB:
                                fat_mvm_prep.py should have already found
                                a set of ROIs with data across all the
                                the subjects in the group, listed in the
                                *MVMprep.log file.
      -n, --no_posthoc         :switch to turn off the automatic
                                generation of per-ROI post hoc tests
                                (default is to do them all).

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

     Example:
       $ fat_mvm_scripter.py --file_vars=VARLIST.txt          \\
                             --log_file=study_MVMprep.log     \\
                             --table=study_MVMtbl.txt         \\
                             --prefix=study 
     or, equivalently:
       $ fat_mvm_scripter.py -f VARLIST.txt -l study_MVMprep.log -t study_MVMtbl.txt -p study
 
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
 
    file_prefix = ''
    list_model = ''
    userlist_roi = ''
    userlist_par = ''  #!! no user subset of par at the moment
    file_listmodel = ''
    file_table = ''
    file_log = ''
    SWITCH_posthoc = 1

    try:
        opts, args = getopt.getopt(argv,"hnv:f:p:t:l:r:",["no_posthoc",
                                                            "vars=",
                                                            "file_vars=",
                                                            "prefix=",
                                                            "table=",
                                                            "log_file=",
                                                            "rois="
 #                                                           "Params"
                                                            ])
    except getopt.GetoptError:
        print help_line
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print help_line
            sys.exit()
        elif opt in ("-v", "--vars"):
            list_model = arg
        elif opt in ("-r", "--rois"):
            userlist_roi = arg
#        elif opt in ("-P", "--Params"):
#            userlist_par = arg
        elif opt in ("-f", "--file_vars"):
            file_listmodel = arg
        elif opt in ("-p", "--prefix"):
            file_prefix = arg
        elif opt in ("-t", "--table"):
            file_table = arg
        elif opt in ("-l", "--log_file"):
            file_log = arg
        elif opt in ("-n", "--no_posthoc"):
            SWITCH_posthoc = 0


    if not(file_prefix):
        print "** ERROR: missing an output prefix."
        print "\t Need to use either '-p' or '--prefix'."
        sys.exit()
    if not(file_table):
        print "** ERROR: missing an input table."
        print "\t Need to use either '-t' or '--table'."
        sys.exit()
    if not(file_log):
        print "** ERROR: missing an input log file."
        print "\t Need to use either '-l' or '--log'."
        sys.exit()
    if ( list_model == '' ) and ( file_listmodel == '' ):
	print "** ERROR: missing a necessary model description input."
        print "\t Need to use either '-m' or '-f'."
        sys.exit()
    if not( list_model == '' ) and not( file_listmodel == '' ):
        print "*+ Warning: both a model list *and* a model file have",
        print " been input."
        print "\tThe one after '-m' will be ignored."

    return list_model, file_listmodel, file_prefix, file_table, \
     file_log, userlist_roi, userlist_par, SWITCH_posthoc


########################################################################

if __name__=="__main__":
    set_printoptions(linewidth=200)
    print "\n"

    list_model, file_listmodel, file_prefix, file_table, file_log, \
     userlist_roi, userlist_par, SWITCH_posthoc = main(sys.argv[1:])

    arg_list = sys.argv
    str_sep = ' '
    arg_str = str_sep.join(arg_list)

    # get file list from either of two ways.
    if file_listmodel:
        var_list = GR.ReadSection_and_Column(file_listmodel, 0)
    elif list_model:
        var_list = list_model.split()
    else:
        print "** Error! Cannot read in model values."
        sys.exit(4)

    Nvar = len( var_list )

    if userlist_roi:
        roi_list = userlist_roi.split()
    else:
        roi_list = GR.GetFromLogFile(file_log, GR.LOG_LABEL_roilist)
    Nroi = len(roi_list)

    if userlist_par:
        par_list = userlist_par.split()
    else:
        par_list = GR.GetFromLogFile(file_log, GR.LOG_LABEL_parlist)
    Npar = len(par_list)

    tab_raw, tab_colvars = GR.LoadInTable(file_table)
    tab_data, tab_coltypes = GR.ConvertCSVfromStr(tab_raw, tab_colvars)
    var_iscateg = GR.CheckVar_and_FindCategVar(tab_data,         \
                                                tab_colvars,  \
                                                tab_coltypes, \
                                                var_list)

    #--------------------------------------------------

    str_sep = '  '
    roi_str = str_sep.join(roi_list)
    par_str = str_sep.join(par_list)

    Nvartout = Nvar
    var_list_quant = []
    varQ_str = ''
    for i in range(Nvar):
        x = var_list[i]
        if var_list.__contains__(x):
            if not( var_iscateg[i] ):
                var_list_quant.append(x)
                varQ_str+= "  %s" % x
            else:
                Nvartout+= len(var_iscateg[i])-1

    varC_str = ''
    for i in range(Nvar):
        if var_iscateg[i]:
            varC_str+= "  %s (" % var_list[i]
            for y in var_iscateg[i]:
                varC_str+= " %s" % y
            varC_str+= " )"

    print "   List of ROIs is:\n   \t  %s." % roi_str
    print "   List of matrix parameters is:\n   \t  %s." % par_str
    print "   List of quantitative variables is:\n   \t%s." % varQ_str
    print "   List of categorical variables is:\n   \t%s." % varC_str



    #--------------------------------------------------

    bvar_sep = '+'
    bvar_entry = bvar_sep.join(var_list)
    qvar_sep = ','
    qvar_entry = qvar_sep.join(var_list_quant)

    Nglt = Nroi*Nvartout


    print '++ Starting to make and write MVM command...'
    tc = []
    # ~header
    Lhead = 8
    tc.append('# %s :\n' % (GR.LOG_LABEL_command) )
    tc.append('# %s\n' % (arg_str) )
    tc.append('#\n')
    tc.append('# %s : %s\n' % (GR.LOG_LABEL_roilist, roi_str))
    tc.append('# %s : %s\n' % (GR.LOG_LABEL_parlist, par_str))
    tc.append('# %s : %s\n' % (GR.VAR_LABEL_roilistQ, varQ_str))
    tc.append('# %s : %s\n' % (GR.VAR_LABEL_roilistC, varC_str))
    tc.append('\n')
    tc.append('3dMVM -prefix %s_MVM \\\n' % file_prefix )
    tc.append(' -bsVars "%s" \\\n' % bvar_entry )
    tc.append(' -wsVars "%s" \\\n' % 'ROI' )
    tc.append(' -qVars "%s" \\\n' % qvar_entry )
    if SWITCH_posthoc:
        tc.append(' -num_glt %d \\\n' % Nglt )
        idx = 1
        for x in roi_list:
            for j in range(Nvar):
                y = var_list[j]
                if not( var_iscateg[j] ):
                    tc.append(' -gltLabel %d %s-%s -gltCode %d "ROI : 1*%s %s : " \\\n'\
                              % (idx, x, y, idx,x,y) )
                    idx+=1
                else:
                    for z in var_iscateg[j]:
                        tc.append(' -gltLabel %d %s-%s^%s -gltCode %d "ROI : 1*%s %s : 1*%s " \\\n'\
                              % (idx, x, y,z, idx,x,y,z) )
                        idx+=1
    tc.append(' -dataTable @%s \n' % file_table )

    Command_sep = ' '
    Run_comm = Command_sep.join( tc )

    fout = file_prefix+'_scri.tcsh'
    f = open(fout,'w')
    Nlines = len(tc)
    for i in range(Nlines):
        f.write("%s" % (tc[i]))
#    for i in range(Nlines-1):
#        if i < Lhead:
#            f.write("%s  \n" % (tc[i]))
#        else:
#            f.write("%s  \\\n" % (tc[i]))
#    f.write("%s  \n" % (tc[Nlines-1]))
    f.close()
    
    print "++ ... done writing the new MVM script file, which can be run with:"
    print "\n   tcsh %s  \n\n" % fout
