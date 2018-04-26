#!/bin/python

# Processing of CSV files with some R functionality.
# Ver 1.0. 
# Oct. 2014, PA Taylor (UCT, AIMS).


import getopt, sys 
import numpy as np
from rpy2.robjects import r
import rpy2.robjects.numpy2ri
from rpy2.robjects.packages import importr

# allows input of np arrays into R functions.
rpy2.robjects.numpy2ri.activate()  
grDevices = importr('grDevices') # can't make plot work right now...

# Available output format types for R_Factor Analysis plots, if
# desired
PARN_OUT_types = ['jpeg', 'pdf', 'png', 'tiff']
PARN_OUT_devs = [ grDevices.jpeg , grDevices.pdf , 
                  grDevices.png , grDevices.tiff ]


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

def CorMatCheck(Y, cvars):
    
    MM = np.corrcoef(np.transpose(Y))
    for i in range(len(MM)):
        for j in range(i+1,len(MM)):
            if MM[i,j]>0.9:
                print "*+ WARNING! Very high correlation (%.4f)" % (MM[i,j]),
                print "among variables:  '%s' and '%s'." % (cvars[i], cvars[j])
                print "\t-> Consider removing one if eigenvalues are tiny?\n"


    M = np.cov(np.transpose(Y))
    E = np.linalg.eigvalsh(M)

    for e in E:
        if e < 10**-12:
            print "*+ WARNING! Small eigenvalues (%g)!" % (e)
            print "\t-> probably meaning numerical instability",
            print "in factor analysis.\n"
            
    return 1

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

def CheckForDuplicates( test_arr ):
    testout = list(test_arr)

    to_rem=[]
    N_rem = []
    print "\nChecking for duplicates..."
    for x in test_arr:
        N = test_arr.count(x)
        if (N > 1) and not( to_rem.__contains__(x) ):
            print "*+ Warning-- ",
            print str(N)+" duplicate column(s) '"+x+"' in CSV file."
            print "\t-> We will ignore all but one here."
            to_rem.append(x)
            N_rem.append(N)
    
    for j in range(len(to_rem)):
        for i in range(N_rem[j]-1):
            testout.remove(to_rem[j])

    return testout



def Cut_ColVars_with_NAs(csv_data, csv_colvars, test_arr):
    '''Inputs: a full list of colvars and csv_data, as well as the list of
    'test' colvars.
    Return: a modified list of colvars (subset of 'test' colvars) that
    doesn't include columns that have NA in them.'''

    Lx, Ly = np.shape(csv_data)
    testout = list(test_arr)

    to_rem = []

    for x in testout:
        if csv_colvars.__contains__(x):
            ii = csv_colvars.index(x)
            for j in range(Lx):
                if csv_data[j][ii] == 'NA':
                    to_rem.append(x)
                    break

    for x in to_rem:
        testout.remove(x)

    return testout
    
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

def WriteOutCSV(prefix, csv_data, csv_colvars, csv_coltypes):

    Lx, Ly = np.shape(csv_data)
    fff = open(prefix+'.csv','w')
     
    for i in range( Ly-1 ):
        fff.write("%s," % csv_colvars[i])
    fff.write("%s\n" % csv_colvars[-1])

    for i in range(Lx):
        for j in range(Ly):
#            strung_out = WhatToWrite(csv_data[i][j], csv_coltypes[j])
            strung_out = WhatToWrite(csv_data[i][j])
            if j == (Ly-1) :
                fff.write(strung_out+'\n')
            else:
                fff.write(strung_out+',')
    fff.close()
    return 1
            

# probably unnecessary,but I don't want too many decimals in output
def WhatToWrite(var):
    vtype=type(var)
    if [float, np.float64].__contains__(vtype):
        x = "%.5f" % (var)
    elif vtype == int :
        x = "%7d" % (var)
    elif vtype == str :
        x = "%7s" % (var)
    else:
        print "** ERROR: type output problem."
        print "\t-> it's not in {int | float | str}"
        print "\t-> somehow it's: ", vtype
        sys.exit(77)
    return x

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

def Out_With_the_Old( OLD_data,
                      OLD_colvars,
                      OLD_coltypes,
                      NEW_data,
                      NEW_titles,
                      list_out_colvars ):

    Nsubj, Nvar_old = np.shape(OLD_data)
    Nsubj2, Nvar_new = np.shape(NEW_data)

    if Nsubj != Nsubj2:
        print "** ERROR in placing new factors into the CSV data."
        print "\t Somehow wrong number of subjects??"
        sys.exit(34)

    AllSubj = []
    AllTitles = []
    AllTypes = []
    for i in range( Nsubj ):
        thissubj = []
        for j in range( Nvar_old ):
            x = OLD_colvars[j]
            if not( list_out_colvars.__contains__(x) ):
                thissubj.append(OLD_data[i][j])
                if i==0:
                    AllTitles.append(x)
                    AllTypes.append(OLD_coltypes[j])
        for j in range( Nvar_new ):
            x = NEW_titles[j]
            if not( list_out_colvars.__contains__(x) ):
                thissubj.append(NEW_data[i,j])
                if i==0:
                    AllTitles.append(x)
                    AllTypes.append(float) # we know they're float
        if thissubj:
            AllSubj.append(thissubj)
        else:
            print "** ERROR in placing new factors into the CSV data."
            print "\t Somehow empty subject data lists??"
            sys.exit(43)


    return AllSubj, AllTitles, AllTypes

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

def R_Factor_Analysis( comm_str,
                       csv_data, csv_colvars, csv_coltypes, fpref,
                       test_arr, # -> can be NULL for interior calc
                       Nfac,     # -> can be 0    for interior calc
                       Ntopload, # -> can be 0    for interior calc
                       flab,
                       DO_GRAPH,
                       N_cent = 99,   # 'centile'
                       N_iter = 5000,  # 'iterations'
                       ftype = 'jpeg'):
    '''Perform factor analysis using R function factanal().  User can
    specify the number of latent factors using the paran() function,
    which implements Horn's test.
    Returns:  Factor scores and loadings'''
    
    # R libraries used here.
    paran = importr('paran')

    # some stuff about format types
    if PARN_OUT_types.__contains__(ftype):
        ii = PARN_OUT_types.index(ftype)
        OUT_dev = PARN_OUT_devs[ii]
        OUT_paran = fpref+'.'+ftype
    else:
        print "** Error! ",
        print "Output file type '%s' is not valid. Select from:" % (ftype)
        print "\t",
        for x in PARN_OUT_types:
            print " '"+x+"' ",
        print "\n"
        sys.exit(32)

    
    fff = open(fpref+'.log','w')
    if comm_str:
        fff.write('# '+comm_str+"\n")

    # SETUP THE VARIABLE VALUES
    Lx,Ly = np.shape(csv_data)

    # if user hasn't entered a selection, then use 'em all.
    if not(test_arr):
        test_arr = list(csv_colvars)

    # Get rid of variable columns with 'NA'
    test_arr = Cut_ColVars_with_NAs(csv_data, csv_colvars, test_arr)

    # check for duplicate columns, which lead to bad singularities
    test_arr = CheckForDuplicates( test_arr )

    # if user hasn't entered a label, then use:
    if not(flab):
        flab = 'FACTOR'

    # only select variables that are represented in the csv_data headings,
    # as well as being either int or float
    VARS_inds = []
    VARS_names = []
    for x in test_arr:
        if csv_colvars.__contains__(x):
            ii = csv_colvars.index(x)
            if [int, float].__contains__(csv_coltypes[ii]):
                VARS_inds.append(ii)
                VARS_names.append(x)

    Nvars = len(VARS_names)
    Y = np.zeros((Lx,Nvars), dtype=float)

    print "++ Factor analysis contains %s variables:" % (Nvars)
    fff.write("\n++ Factor analysis contains %s variables:\n" % (Nvars))
    for j in range(Nvars):
        jj = VARS_inds[j]
        print "\t %s" % (VARS_names[j])
        fff.write("\t %s\n" % (VARS_names[j]))
        for i in range(Lx):
            Y[i,j] = csv_data[i][jj]


    i = CorMatCheck(Y, VARS_names)

    # SETUP THE NUMBER OF FACTORS
    # use eval info to pick number of vars, if user hasn't
    if not(Nfac):
        print "++ Graphing of parallel analysis (PA) Horn's test is:",
        if DO_GRAPH:
            print "ON."
        else:
            print "OFF."
        print "++ PA percentile in Horn's test is: ", N_cent 
        print "++ Number of PA Monte Carlo iterations: ", N_iter

        # mostly default values, some user control
        PARN = r.paran( Y, iterations=N_iter, centile=N_cent,
                        quietly=False, status=True, all=True,
                        cfa=True, graph=DO_GRAPH, color=True,
                        col=r.c("black","red","blue"), lty=r.c(1,2,3),
                        lwd=1, legend=True, file=OUT_paran, width=640,
                        height=640, grdevice=OUT_dev, seed=0)

        if DO_GRAPH:
            grDevices.dev_off()
            print "++ Don't worry about the briefly passing image."
            print "\tIt has been saved as: %s\n\n" % ( OUT_paran )

        N_PARN_arr = np.array(PARN.rx2('Retained'))
        Nfac = int(N_PARN_arr[0])

    else:
        if Nfac > Nvars:
            print "*+ Warning! The user has selected a number of factors larger"
            print "\tthan the number of variables (%d > %d)!" % (Nfac, Nvars)
            print "\t-> Therefore, we're setting it to be %d," % (Nvars)
            print "\t  but you might still want to check if anything went awry?"
        else:
            print "++ The user has selected the number of factors"
            print "\tto be %d out of %d." % (Nfac, Nvars)


    # RUN THE FACTOR ANALYSIS IN R
    FA_out = r.factanal(Y, 
                        factors=Nfac, 
                        scores='regression', 
                        rotation="varimax")

    FA_scores =np.array(FA_out.rx2('scores'))
    FA_loadings =np.array(FA_out.rx2('loadings'))
    

    # match up highest loadings with the variable names, so we have an
    # idea of what's going into the sausage

    # how many loadings to output.  
    # Can be: ALL, 5, or user-entered other
    if not(Ntopload):
        Ntopload = min(Nvars, 5)
    elif Ntopload<0 :
        Ntopload = Nvars
    else:
        Ntopload = min(Nvars, Ntopload)
    if Ntopload==Nvars:
        strNtopload = "ALL "+str(Nvars)
    else:
        strNtopload = 'top '+str(Ntopload)+'/'+str(Nvars)

    # ordering process
    FA_titles = []
    print "\n++ Factor loading contributions (%s):" % (strNtopload)
    fff.write("\n++ Factor loading contributions (%s):\n" % (strNtopload))
    for i in range(Nfac):
        P = list(FA_loadings[:,i])
        Q = list(VARS_names)
        PQ = sorted(zip(P,Q),reverse=1)
        str_title = "%s_%02d" % (flab, i+1)
        FA_titles.append(str_title)
        print "\n\t"+str_title
        fff.write("\n\t"+str_title+"\n")
        for j in range(Ntopload):
            print "\t%20s  %12.5f" % (PQ[j][1],PQ[j][0])
            fff.write("\t%20s  %12.5f\n" % (PQ[j][1],PQ[j][0]))
    fff.close()

    return FA_scores, FA_titles, VARS_names
