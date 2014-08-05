#!/usr/bin/env python
#
# July, 2014
#
# File of helper functions for fat_mvm_*.py.
#
#
#
#############################################################################


import numpy as np 
import csv
import sys

from glob import glob

np.set_printoptions(linewidth=200)

# DEFINITION
NLinesGridHead = 3
HeaderRowTrue = 1
FirstColLabels = 1    # first read in col is subj name-labels
GridCheckVar = 'NT'

# for writing file for MVM
ColOne_SUB = 'Subj'
ColEnding = ['ROI', 'matrPar', 'Ausgang_val']
MVM_file_postfix = '_MVMtbl.txt'

MVM_matchlog_postfix = '_MVMprep.log'
LOG_LABEL_colmatr = 'Matrixfile_ID'
LOG_LABEL_colsubj = 'CSV_Subj'
LOG_LABEL_roilist = 'ROI_list'
LOG_LABEL_parlist = 'Parameter_list'
VAR_LABEL_roilistQ = 'Qvar_list'
VAR_LABEL_roilistC = 'Cvar_list'
LOG_LABEL_command = 'Made_by_command'

def LoadInTable(fname):
    ''' open up a file, and return a 2-tuple: (raw) data set and header.'''

    fff = open(fname, 'r')
    x = fff.readlines()
    fff.close()

    N = len(x)

    print '++ Opening and reading table file...'

    data = []
    header = []

    idx = 0
    for row in x[:]:
        if not(idx) and HeaderRowTrue:
            tt = row.split()
            if tt[-1] == '\\':
                header = tt[:-1]
            else:
                header = tt
            idx+= 1
        else:
            #print row.split()
            tt = row.split()
            if tt[-1] == '\\':
                data.append(tt[:-1])
            else:
                data.append(tt)

    header = Check_Header_Spaces(header)

    print '++ ... done opening and reading table file.'
    
    return data, header





def CheckVar_and_FindCategVar(tab_data, tab_colvars, tab_coltypes, par_list):

    Npar = len(par_list)
    Nsub, Ncol = np.shape(tab_data)
    iscateg = []
    for i in range(Npar):
        if tab_colvars.__contains__(par_list[i]):
            j = tab_colvars.index(par_list[i])
            if tab_coltypes[j]==str :
                terms = set()
                for k in range(Nsub):

                    terms.add(tab_data[k][j])
                listterms = list(terms)
                listterms.sort()
                iscateg.append(listterms)
            else:
                iscateg.append([])
        else:
            print "** Error! Variable %s is not in the data table" \
             % par_list[i]
            sys.exit(11)

    return iscateg


#----------------------------------------------------------------

def GetFromLogFile(fname, ltype):

    if not(fname):
        print "** Error: can't open log file: '%s'." % fname
        sys.exit(9)

    fff = open(fname,'r')
    x = fff.readlines()
    fff.close()

    N = len(x)

    for start in range(N):
        if (x[start].split()[0] == '#') and (x[start].split()[1] == ltype):
            #print "FOUND", x[start].split()
            break

    outlist = []
    for i in range(start+1,N):
        if x[i].split()[0] == '#':
            break
        else:
            outlist.append(x[i].split()[0])

    if not(outlist):
        print "** Error: empty list from log file: '%s'." % fname
        sys.exit(10)

    return outlist


###------------------------------------------------------------------
###------------------------------------------------------------------
###--------------- START: Final prep output operations --------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def Write_MVM_File(PREFIX,            \
                   csv_data,          \
                   csv_colvars,       \
                   grid_ROIlist,      \
                   grid_VARlist,      \
                   grid_tabled):

    fff = open(PREFIX+MVM_file_postfix,'w')

    ### 'csv_colvars':  rename 0th col
    ### and we add three more col to output: ROI, VarName, VarVal
    csv_colvars[0] = ColOne_SUB
    Nsub, Ncsvar = np.shape(csv_data)
    Nroi = len(grid_ROIlist)
    Npar = len(grid_VARlist)
    Ncol = Ncsvar+len(ColEnding)


    for i in csv_colvars:
        fff.write("%s " % i)
    for i in ColEnding:
        fff.write("%s " % i)
    fff.write(" \\\n")

    for i in range(Nsub):
        for x in range(Nroi):
            for y in range(Npar):
                for j in range(Ncsvar):
                    fff.write("%s " % str(csv_data[i][j]))
                fff.write("%s " % grid_ROIlist[x])
                fff.write("%s " % grid_VARlist[y])
                fff.write("%s " % str(grid_tabled[i,x,y]))
                if not( i==(Nsub-1) and x==(Nroi-1) and y==(Npar-1) ):
                    fff.write("  \\\n")
                else:
                    fff.write("  \n")

    fff.close()

    return 1

#--------------------------------------------------------------------

def Check_EleIn_ROIlist(ROIlist, ele):
    try:
        return ROIlist.index(ele)
    except ValueError:
        return -1

#--------------------------------------------------------------------

def MakeGridTableau(DICT_CSV_grid, \
                    csv_data,      \
                    grid_data,     \
                    grid_ROIlist,  \
                    grid_VARlist,  \
                    grid_subj):
    '''Take a lot of matrix file info in, as well as the CSV file and
    conversion dictionary, and return a tableau of necessary
    variable&parameter values from all the subjects' matrix file data.
    Output is a list ordered by CSV subject, for ease in final
    writing.'''

    print '++ Starting to select and convert matrix data to usable',
    print 'form for output...'

    Nsub = len(csv_data)
    Nroi = len(grid_ROIlist)
    Nvar = len(grid_VARlist)
    MM = np.zeros((Nsub, Nroi, Nvar))

    # put in order of CSV stuff
    for i in range(Nsub):
        csv_subb = csv_data[i][0]
        # which sub in grid_data
        grid_subb = DICT_CSV_grid[csv_subb]
        ii = Check_EleIn_ROIlist(grid_subj, grid_subb)
        #print "HI!", i, ii
        if ii>=0: 
            for j in range(Nroi):
                Ntar = len(grid_data[ii][3])
                for x in range(Ntar):
                    for y in range(x+1,Ntar):
                        # inefficient search...
                        ele = ElementNameMaker(grid_data[ii][3][x], \
                                                      grid_data[ii][3][y])
                        if grid_ROIlist[j] == ele:
                            #print ii,j, ele
                            for k in range(Nvar):
                                var = grid_VARlist[k]
                                idx = grid_data[ii][2][var]
                                # which matrix in grid[i][0]
                                MM[i,j,k] = grid_data[ii][0][idx][x][y]

    print '++ ... done converting matrix stuff for outputting in table.'
    return MM

###------------------------------------------------------------------
###------------------------------------------------------------------
###---------------- STOP: Final prep output operations --------------
###------------------------------------------------------------------
###------------------------------------------------------------------

###------------------------------------------------------------------
###------------------------------------------------------------------
###-------- START: Make/Check/Print CSV and matrix matching ---------
###------------------------------------------------------------------
###------------------------------------------------------------------

def MakeDict_CSV_to_grid(grid_subj, csv_subj, file_match):
    
    if IsFirstUncommentedSection_Multicol(file_match):
        CtG = MakeDict_CSV_to_grid_FILE(grid_subj, csv_subj,file_match)
    else:
        CtG = MakeDict_CSV_to_grid_NOFILE(grid_subj, csv_subj)
    return CtG


#---------------------------------------------------------------------

def IsFirstUncommentedSection_Multicol(fname):
    tt = ReturnFirstUncommentedSection_ofReadlines(fname)
    if tt:
        if len(tt[0]) > 1:
            return 1
    return 0

#---------------------------------------------------------------------

def ReturnFirstUncommentedSection_ofReadlines(fname):

    if not(fname):
        return []

    fff = open(fname,'r')
    x = fff.readlines()
    fff.close()

    N = len(x)

    # ignore preliminary comments
    start = 0
    while x[start].split()[0] == '#':
        start+=1

    temp = []
    for i in range(start,N):
        y = x[i].split()
        if y[0] == '#': # stop if you hit a comment
            break 
        else:
            temp.append(y)
    return temp

def MakeDict_CSV_to_grid_FILE(grid_subj, csv_subj,file_match):
    
    print "++ Start matching of CSV and matrix subject data (with file)..."

    temp = ReturnFirstUncommentedSection_ofReadlines(file_match)
    print "LIST IS:"
    Nsub = len(temp)
    print "++ Looks like there are %d subjects in matching file: '%s'."  \
     % (Nsub, file_match )

    CtG = {}
    for i in range(Nsub):
        CtG[ temp[i][1] ] = temp[i][0]
        print temp[i][1], temp[i][0]

    print "++ ... done matching CSV and matrix subject data."

    return CtG

#------------------------------------------------------------------------

def MakeDict_CSV_to_grid_NOFILE(grid_subj, csv_subj):

    print "++ Start matching of CSV and matrix subject data (without file)..."

    CtG = {}
    for a in csv_subj:
        for b in grid_subj:
            if b.find(a) != -1:
                if CtG.has_key(a):
                    print 'Error: OVERWRITING MATCHES! Need uniquity.'
                    #return {}
                else:
                    CtG[a]=b
    
    print "++ ... done matching CSV and matrix subject data."

    return CtG

###--------------------------------------------------------------------

def CheckDict_CSV_to_grid(CtG, grid_subj, csv_subj, csv_data):

    print "++ Start checking dictionary of CSV and matrix subject data..."
    Nsub0 = len(csv_subj)

    csv_subj_new = []
    csv_data_new = []

    BADNESS = 0
    # check if all csv sub are used
    for i in range( Nsub0 ):
        a = csv_subj[i]
        if CtG.has_key(a):
            csv_subj_new.append(a)
            csv_data_new.append(csv_data[i])
        else:
            print '*+ Warning: CSV subj %s does not have a matrix match!' \
             % a
            BADNESS+=1
    # check if all grid matr subs are used:
    y = CtG.values()
    for b in grid_subj:
        if not( y.__contains__(b) ):
            print '*+ Warning: matrix subj %s does not have a CSV subj match!' \
             % b
            #BADNESS+=1

#    if BADNESS :
#        print "++ Possibly bad CSV<->matrix matching: ",
#        print "either incomplete or nonunique."
#        print "\tBut perhaps you are purposefully selecting subset(s)?"
#    else:
#        print "++ Successful matching: each CSV entry had single matrix match."

    print "++ ... done checking dictionary."

    return csv_data_new, csv_subj_new

###----------------------------------------------------------------------

def Write_CSVandMatrix_log(PREFIX,             \
                              CtG,             \
                              ROIlist,         \
                              PARlist,         \
                              ARGstr):
    '''Printout file of what I think the CSV and matrix file matches
    are, so user can doublecheck.  Output file is called
    'PREFIX_MVM_var.log'.'''

    fname = PREFIX+MVM_matchlog_postfix
    fff = open(fname,'w')
    x = CtG.keys()
    x.sort()
    
    fff.write("# %s :\n# %s\n" % (LOG_LABEL_command, ARGstr) )

    # CSV and matrix matching
    fff.write("# %-50s\t%s\n" % (LOG_LABEL_colmatr, LOG_LABEL_colsubj))
    for i in x:
        fff.write("%-52s\t%s\n" % (CtG[i], i) )

    # list of ROIs
    fff.write("# %s\n" % (LOG_LABEL_roilist))
    for i in ROIlist:
        fff.write("%s\n" % (i) )

    # list of ROIs
    fff.write("# %s\n" % (LOG_LABEL_parlist))
    for i in PARlist:
        fff.write("%s\n" % (i) )

    fff.close()

#    print "\tYou can check file '%s' for CSV<->matrix matching log." % fname
    return 1

###------------------------------------------------------------------
###------------------------------------------------------------------
###--------- STOP: Make/Check/Print CSV and matrix matching ---------
###------------------------------------------------------------------
###------------------------------------------------------------------


###------------------------------------------------------------------
###------------------------------------------------------------------
###------------ START: Matrix element and set operations ------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def ReadSection_and_Column(file_list, colnum):

    temp = ReturnFirstUncommentedSection_ofReadlines(file_list)
    N = len(temp)
    list_of_subj = []
    for i in range(N):
        if len(temp[i]) > colnum:
            list_of_subj.append(temp[i][colnum])

    if not( list_of_subj):
        print "*+ No output column number %d in file %s." % \
         (colnum, file_list)

    return list_of_subj

def GroupListOfFullMatrs(file_IDer, file_listmatch, colnum):
    '''Take file ID argument (probably from user commandline input),
    happily read all the grids/matrices, and return:
    1) a list of 4-tuples of the data matrices;
    2) list of the subjects (mainly for checking)'''

    print '++ Starting to find and open matrix files...'
    if file_listmatch:
        list_of_subj = ReadSection_and_Column(file_listmatch, colnum)

#        fff = open(file_listmatch,'r')
#        x = fff.readlines()
#        fff.close()
#        temp = ReturnFirstUncommentedSection_ofReadlines(x)
#        N = len(temp)
#        list_of_subj = []
#        for i in range(N):
#            list_of_subj.append(temp[i][1])
    elif file_IDer:
        list_of_subj = glob(file_IDer)
    else:
        print "** Error! Cannot read in matrix files."
        sys.exit(4)

    All_sub_grid = []
    for ff in list_of_subj:
        x = LoadInGridOrNetcc(ff)
        All_sub_grid.append(x)

    if len(All_sub_grid) == 0:
        print '** ERROR! No subjects found when looking for group!'
        sys.exit(5) #return [], []
    else:
        print '\tFound %d subjects in the group.' % len(All_sub_grid)
    

    print '++ ... done finding and opening matrix files.'

    return All_sub_grid, list_of_subj

###------------------------------------------------------------------

def Check_Matr_type(LM):

    N = len(LM)
    counts = np.zeros(3) # GRID: 0;  NETCC: 1;  OTHER:2

    for i in range(N):
        mpost = LM[i][-5:]
        if mpost =='.grid':
            counts[0]+=1
        elif mpost =='netcc':
            counts[1]+=1
        else:
            counts[2]+=1

    if counts[0] == N:
        print '++ Looks like all matrices are GRID. Fine.'
        return 'GRID'
    elif counts[1] == N:
        print '++ Looks like all matrices are NETCC. Fine.'
        return 'NETCC'
    else:
        print '*+ Warning: nonuniform or foreign type of matrix files!'
        return 'OTHER'

    return

def FindGroupwiseTargets(All_sub_grid, ftype):
    '''Take a list of 4-tuples representing subject data (and a string
    of what filetype it is), go through all, and find set of
    intersecting matrix elements based on labels.  When done, return
    that list of matrix elements, AND a list of the variables.'''

    print '++ Going through sets of matrix element ROIs...'
    Nsub = len(All_sub_grid)

    if Nsub == 0:
        print '** ERROR! No subjects found when looking for matrix set!'
        return []
    else:
        print '\tFound %d subjects for finding ROI elements.' % Nsub
    
    ## For finding the set of ROI elements
    a = GetSet(All_sub_grid[0], ftype)
    print '\tThe number of elements in the ROI matrix set is:\n\t  ',
    for i in range(1,Nsub):
        print '%d,' % len(a),
        a.intersection_update(GetSet(All_sub_grid[i], ftype))
    print '%d.' % len(a)

    temp = list(a)
    temp.sort()
    print '\tThe set of ROIs is:'
    for t in temp:
        print '\t   %s' % t

    ## For finding the set of variables
    b = set(All_sub_grid[0][1])
    print '\tThe (final) number of parameters in the ROI matrix set is:',
    for i in range(1,Nsub):
        b.intersection_update(set(All_sub_grid[i][1]))
    print '%d.' % len(b)

    temp2 = list(b)
    temp2.sort()
    print '\tThe set of parameters is (alphabetically):'
    print '\t ',
    for t in temp2:
        print ' %s ' % t,
    print ''
    print '++ ... done getting set of ROIs.'
    return temp, temp2  #a, b

###------------------------------------------------------------------

def GetSet(x, ftype):
    '''Input one of the 4-tuples returned by LoadInGridOrNetcc.
    Output is a 'set' of nonempty target-connecting elements.
    FOR NOW: this is specifically for a GRID file, where there's
    a nice 'NT' set of ints.'''
    
    if not(ftype=='GRID' or ftype=='NETCC'):
        print "** ERROR: unrecognized matrix type. Don't know what to do."
        return set()

    #if ftype=='GRID': # later, grid vs netcc
    Ntar = len(x[3])
    roi_set = set()  # empty set
    # should be UHT, nondiagonal selector
    for i in range(Ntar):
        for j in range(i+1,Ntar):
            # NETCC: all UHT elements
            # GRID: UHT elements, with nonzero 'NT' values
            if ((ftype=='GRID') and x[0][x[2][GridCheckVar]][i,j]) \
                 or (ftype=='NETCC'):
                roi_set.add(ElementNameMaker(x[3][i], x[3][j]))

    return roi_set

###------------------------------------------------------------------

def ElementNameMaker(A, B):
    '''Current format for turning integer ROI labels into 
    ROI element name.'''
    # paranoia
    if (type(A) != int) or (type(A) != int):
        print "ERROR in element maker: why not 'int' element labels?"
    if A > 999:
        print "POSSIBLE error in element maker: big label %d > 999!" % A
    if B > 999:
        print "POSSIBLE error in element maker: big label %d > 999!" % B

    x = str("%03d" % (A))
    y = str("%03d" % (B))
    return x+'_'+y

###------------------------------------------------------------------
###------------------------------------------------------------------
###------------- STOP: Matrix element and set operations ------------
###------------------------------------------------------------------
###------------------------------------------------------------------


###------------------------------------------------------------------
###------------------------------------------------------------------
###----------------- START: Reading in CSV files --------------------
###------------------------------------------------------------------
###------------------------------------------------------------------


def LoadInCSV(fname):
    ''' open up a file, and return a 2-tuple: (raw) data set and header.'''

    filein = open(fname, 'rb')
    csvread = csv.reader(filein)

    print '++ Opening and reading CSV file...'

    data = []
    CSVheader = []

    idx = 0
    for row in csvread:
        if not(idx) and HeaderRowTrue:
            CSVheader = row
            idx+= 1
        else:
            data.append(row) #print row
    filein.close()

    CSVheader = Check_Header_Spaces(CSVheader)

    print '++ ... done opening and reading CSV file.'
    
    return data, CSVheader

###------------------------------------------------------------------

def Check_Header_Spaces(X):

    N = len(X)

    for s in range(N):
        nn = X[s].rfind(' ')
        if nn >= 0:
            bleep = str(X[s])  # cheap copy
            X[s] = X[s].replace(' ','_')
            print "\tWarning: space in header name; replacing: '%s' -> '%s'." \
             % (bleep, X[s])

    return X

###------------------------------------------------------------------

def Choose_IntFloatStr(s):
    '''Check and see if the input string 's' represents a number:
          -> if it does, then return int (default) or float version;
          -> else, return the same string back.
       Returns error if the input is not of type 'str'.
    '''

    if not(type(s) == str):
        print "BAD USAGE OF THIS FUNCTION:",
        print "\tinput argument should be a string, not %s." % type(s) 
        return s

    try:
        float(s)
        try:
            return int(s)
        except ValueError:
            return float(s)
    except:
        return s

###------------------------------------------------------------------

def ConvertCSVfromStr(dat1, head1):
    '''Take the all-string input and header lists in,
       and return a copy with ints, floats and strings.'''

    print '++ Converting (hopefully appropriate) strings to numbers...'

    # careful copying...  need to recursively copy each list
    # unattachedly
    dat2 = [list(x) for x in dat1]
    head2 = list(head1)
    
    Lx,Ly = np.shape(dat2)
    for i in range(Lx):
        # usually, have first column be name labels, even if numeric
        for j in range(FirstColLabels, Ly):
            dat2[i][j] = Choose_IntFloatStr(dat2[i][j])

    # initialize
    for j in range(Ly):
        head2[j] = type(dat2[0][j])
        
    for j in range(Ly):
        ty0 = head2[j]
        for i in range(1,Lx):
            ty = type(dat2[i][j])
            if not(ty0 == ty):
                if ty0 == str :
                    print "Odd mixing in types of data in Col %d (=%s) " % \
                     ( j, head1[j] ),
                    print "\t-> seeing both %s and %s." % (ty0, ty)
                elif ty0 == int:
                    if ty == float:
                        print "\tCSV col %s" % head1[j],
                        print "-> making all floats." , dat2[i][j]
                        head2[i] = float
                        for k in range(Lx):
                            if type(dat2[k][j]) == int:
                                dat2[k][j] = float(dat2[k][j])
                            else:
                                "Odd combo..."
                    elif data[i][j] == 'NA':
                        print "Loc (%d, %d) has value 'NA'." % (i,j)
                    else:
                        print "Odd mixing in types of data in Col %d (=%s) " \
                         % ( j, head1[j] ),
                        print "\t-> seeing both %s and %s." % (ty0, ty)
                elif ty0 == float:
                    if ty == int:
                        #print "\tin CSV col %d ->" % j,
                        #print "making %d float." % dat2[i][j]
                        dat2[i][j] = float(dat2[i][j])
                    elif data[i][j] == 'NA':
                        print "Loc (%d, %d) has value 'NA'." % (i,j)
                    else:
                        print "Odd mixing in types of data in Col %d (=%s) " \
                         % ( j, head1[j] ),
                        print "\t-> seeing both %s and %s." % (ty0, ty)
                else:
                    print "Extra odd: non str, int or float type? %s" % ty0

    print '++ ... done with CSV str->numeric conversions.'

    return dat2, head2


###------------------------------------------------------------------
###------------------------------------------------------------------
###----------------- STOP: Reading in CSV files ---------------------
###------------------------------------------------------------------
###------------------------------------------------------------------


###------------------------------------------------------------------
###------------------------------------------------------------------
###--------------START: Reading in GRID/NETCC files -----------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def LoadInGridOrNetcc(fname):
    ''' Input the grid file name, and return:
    1)  a list of data [NMAT, NROI, NROI];
    2)  a list of labels [NMAT];
    3)  a supplementary dictionary for each calling use;
    4)  and a list of (int) ROI labels (which may not be consecutive).'''

    x = ReadInGridOrNetcc(fname)
    y1,y2,y3,y4 = SepHeaderFile(x, fname)
    return y1,y2,y3,y4

###------------------------------------------------------------------

def ReadInGridOrNetcc(fname):
    ''' open, read file, return raw/unsplit output.'''
    fff = open(fname,'r')
    x = fff.readlines()
    fff.close()
    return x

###------------------------------------------------------------------

def SepHeaderFile(RawX, fname):
    ''' Take in the data and return:
    1)  a list of data [NMAT, NROI, NROI];
    2)  a list of labels [NMAT];
    3)  a supplementary dictionary for each calling use;
    4)  and a list of (int) ROI labels (which may not be consecutive).'''

    Nroi,Nmat,ListLabels = HeaderInfo(RawX[:NLinesGridHead]) # header
    y = RawX[NLinesGridHead:]  # info grids

    y = RemoveEmptyWhitespaceFromReadlines(y, fname)

    Ly = len(y)

    if not( Ly == ((Nroi+1) * Nmat) ):
        print "ERROR reading grid file!"
        print "Number of non-header lines don't match Nmatrices (%d)" % (Nmat)
        return [0,0]

    OUT_data = []
    OUT_labs = []
    OUT_dlab = {}
    ctr = 0
    for i in range(Nmat):
        OUT_labs.append(y[ctr].split()[1])
        OUT_dlab[OUT_labs[i]]=i
        ctr+=1
        if (OUT_labs[i] == 'NT'):
            temp = np.zeros((Nroi,Nroi),dtype=int)
            for j in range(Nroi):
                temp[j,:] = [int(xx) for xx in y[ctr].split()]
                ctr+=1
        else:
            temp = np.zeros((Nroi,Nroi))
            for j in range(Nroi):
                temp[j,:] = [float(xx) for xx in y[ctr].split()]
                ctr+=1
  
        OUT_data.append(np.array(temp))
    return OUT_data, OUT_labs, OUT_dlab,ListLabels

###------------------------------------------------------------------

def RemoveEmptyWhitespaceFromReadlines(x, fname):
    
    N = len(x)
    whitespace = 0
    for i in range(1,N+1,1):
        if x[-i].split() == []:
            whitespace = i
        else:
            break

    if whitespace:
        print "\tNB: ignored %d lines of whitespace from the end of '%s'." \
     % ( whitespace, fname )
        return x[:-whitespace]
    else:
        return x

###------------------------------------------------------------------

def HeaderInfo(RawX):
    ''' Read in necessary numbers from first few lines. '''
    if not(RawX[0].split()[0] == '#'):
        print "** ERROR: the file format doesn't look modern!"
        print "**\t-> run Grid_Modernizer?"
        return 0,0, []

    Nroi = int(RawX[0].split()[1])
    if Nroi <=0:
        print "ERROR reading header! Number of ROIs is: %d" % Nroi
        return [0,0,0]

    Nmat = int(RawX[1].split()[1])
    if Nmat <=0:
        print "ERROR reading header! Number of Matrices is: %d" % Nmat
        return [0,0,0]

    ListLabels=RawX[2].split()
    ll = len(ListLabels)
    if not( ll == Nroi ):
        print "ERROR reading header!"
        print "  Number of ROIs (%d) doesn't match Labels (%d)" % (Nroi,ll)
        return [0,0,0]

    for i in range(ll):
        ListLabels[i] = int(ListLabels[i])
        if ListLabels[i] <=0:
            print "ERROR reading header! ListLabel[%d]=%d." % \
            (i,ListLabels[i])
            return [0,0,0]

    return Nroi, Nmat, ListLabels

###------------------------------------------------------------------
###------------------------------------------------------------------
###-------------- STOP: Reading in GRID/NETCC files -----------------
###------------------------------------------------------------------
###------------------------------------------------------------------



###------------------------------------------------------------------
###------------------------------------------------------------------
###---------- START: Modernizing/rewriting info into better ---------
###------------------------------------------------------------------
###------------------------------------------------------------------

OldFash_parnames = ['NT', 'fNT', 'FA', 'sFA', 'MD',         \
                      'sMD', 'RD', 'sRD', 'L1', 'sL1',    \
                      'NV']

def OldFash_Grid_modernize(fname, outname):

    x = ReadInGridOrNetcc(fname)
    y = mod_func_break(x, fname, outname)

    return y

###------------------------------------------------------------------

def mod_func_break(X, fname, outname):

    lnum = 4 # starting line number for reading as of Sept 2013

    # 1)
    Nroi = int(X[0].split()[0])

    # 2) 
    ROI_LABS = X[2].split()

    # 3)
    Npar = len(OldFash_parnames)
    Y = np.zeros((Npar,Nroi,Nroi))

    for i in range( Npar ):
        k = 0
        for j in range(lnum,lnum+Nroi):
            Y[i,k,:] = np.array(X[j].split(),dtype=np.float32)
            k+=1
        lnum = j+2

    z = mod_func_write(Nroi, ROI_LABS, Y, fname, outname)

    return Y, z

###------------------------------------------------------------------

def DefaultNamingOutGrid(list_all):
    
    out = []

    for x in list_all:
        if not(x[-5:] == '.grid'):
            print "ERROR-- doesn't look like you're modernizing a *.grid file!"

        out.append(x[:-5]+'_MOD'+x[-5:])

    return out


def mod_func_write(Nroi, ROI_LABS, Y, fname, outname):
    
    Ngrid=np.shape(Y)[0]
    if not(Ngrid == len(OldFash_parnames)):
        print "ERROR-- doesn't look like you have the right number of"
        print "\tmatrices in the *.grid file!"

    f = open(outname, 'w')
    f.write('# %d # Number of network ROIs\n' % Nroi)
    f.write('# %d # Number of grid matrices (=NT+fNT+2*N_par+NV)\n' % Ngrid)
    for roi in ROI_LABS:
        f.write("%8s    \t" % roi)
    f.write('\n')

    for i in range(Ngrid):
        f.write('# %s\n' % OldFash_parnames[i])
        for j in range(Nroi):
            for k in range(Nroi):
                if OldFash_parnames[i]=='NT':
                    f.write('%12d\t' % Y[i][j][k])
                else:
                    f.write('%12e\t' % Y[i][j][k])
            f.write('\n')

    f.close()

    return (1)

###------------------------------------------------------------------
###------------------------------------------------------------------
###------------ STOP: Modernizing/rewriting info into better ---------
###------------------------------------------------------------------
###------------------------------------------------------------------





### -----------------------------------------------------------------
### -----------------------------------------------------------------

