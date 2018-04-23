#!/usr/bin/env python
#
# ver 1.0:  July, 2014
# Update, ver 1.1: Sept 2014
# Updated, ver 1.2: Sept 2014
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

# Have to check 5 lines in
NLinesGridHead = 5

HEADER_Nroi = 'Number_of_network_ROIs'
HEADER_Ngrid = 'Number_of_grid_matrices'
HEADER_EleID = 'Special_element_ID'
HEADER_Labels = 'WITH_ROI_LABELS'   

HEADER_ALL = [HEADER_Nroi, HEADER_Ngrid, HEADER_EleID, HEADER_Labels]

# This will be ROI name connector, in line with ZSS usage in SUMA,
# etc.
ConnNames = '__'

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
VAR_LABEL_roilistI = 'Ivar_list'
LOG_LABEL_command = 'Made_by_command'

OldFash_parnames = ['NT', 'fNT', 'FA', 'sFA', 'MD',         \
                        'sMD', 'RD', 'sRD', 'L1', 'sL1',    \
                        'NV']


#class MVM_rev:
#    def __init__(self, var, par):
#        self.var = var
#        self.par = par
#        self.isquant = 1
#        self.mean_tstat = 0
#        self.minp_ROI = ''


###------------------------------------------------------------------
###------------------------------------------------------------------
###---------------- START: review functions and output --------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def BreakLineList(x):

    y = []
    for line in x:
        y.append(line.split())


    return y

###------------------------------------------------------------------

def Find_ANOVAE(x):
    ''' input is a line list broken into lines of words.'''

    Nline = len(x)

    RESULTS = []
    list_par = []
    list_var = []

    for i in range(Nline):
        if len(x[i]) > 0 :
            if (x[i][1] == 'RESULTS:') and (x[i][2] == 'ANOVA') :
                par = x[i][-1]      # name of DTI/FMRI par
                nval = int(x[i+1][0])    # where the num of vars is stored
                values = []
                names = []
                for j in range( nval ):
                    ii = i + 3 + j  # skip to vars
                    if x[ii][-1] != '(Intercept)' :
                        values.append(x[ii][-3])
                        names.append(x[ii][-1])
                RESULTS.append(values)
                list_par.append(par)
                list_var.append(names)


    npar = len(RESULTS)
    if npar <1:
        print "ERROR! No ANOVA results?!?"
        sys.exit(33)

    nvar = len( RESULTS[0] )
    if nvar < 1 :
        print "ERROR! No ANOVA variables?!?"
        sys.exit(34)

    z = np.array(RESULTS, dtype = float)

    # return the array, and it's two lists of labels
    return z, list_par, list_var[0]


###------------------------------------------------------------------





def Find_POSTHOC(x):
    ''' input is a line list broken into lines of words.'''

    Nline = len(x)

    RESULTS = []
    list_par = []
#    list_var = []

    for i in range(Nline):
        if len(x[i]) > 0 :
            if (x[i][1] == 'RESULTS:') and (x[i][2] == 'Post') :
                par = x[i][-1]           # name of DTI/FMRI par
                nglt = int(x[i+1][0])    # where the num of glts is stored
                values = []
                names = []
                ii = i + 3               # jump ahead to data
                phrois, phvars = Find_PosthocVars_and_NROI( x[ii:ii+nglt] )
                Nphrois = len(phrois)
                Nphvars = len(phvars)
                tmpv = np.zeros( Nphvars )
                for j in range( Nphvars ):
                    for k in range( Nphrois ):
                        ll = ii + j + k*Nphvars
                        tmpv[j]+= np.float(x[ll][-5]) # t-stat
                    tmpv[j]/= Nphrois
                    values.append(tmpv[j])
                RESULTS.append(values)
                list_par.append(par)
#                list_var.append(names)

    npar = len(RESULTS)
    if npar <1:
        print "No post hoc results-- guess you didn't want any?"
        return [],[],[]
    else:
        nvar = len( RESULTS[0] )
        if nvar < 1 :
            print "ERROR! No post hoc variables?!?"
            sys.exit(35)

        z = np.array(RESULTS, dtype = float)

        # return the array, and it's two lists of labels
        return z, list_par, phvars

###------------------------------------------------------------------

def Find_PosthocVars_and_NROI( x ):

    nvals = len(x)
    
    phrois = []
    phvars = []

    for y in x:
        twopiece = y[-1].split('--') # Sep,2015: new parser
        if not(phrois.__contains__(twopiece[0])):
            phrois.append(twopiece[0])

        if not(phvars.__contains__(twopiece[1])):
            phvars.append(twopiece[1])

    if len(phvars)*len(phrois) != nvals:
        print "Problem! number of posthoc vars, ROIs and tests not matching!"
        sys.exit(189)
    
    return phrois, phvars

###------------------------------------------------------------------

# ref mat allows us to use group pvalue mat to censor
def ScreenAndFileOutput(PO, TO, FI_mat, FI_par, FI_var, FI_indi, TS,
                        REF_mat):

    STARTER = 0
    if not(TS):
        STARTER = 1

    nvar = len(FI_var)
    npar = len(FI_par)

    calc_indent = 14*nvar+1

    if  (npar, nvar) != np.shape(FI_mat) :
        print "Weird error in numbers not matching internally!"
        sys.exit(35)

    # first lining ...
    if STARTER: TS.append('# %12s' % (FI_var[0]))
    for j in range(nvar):
        print "%14s" % (FI_var[j]),
        if (j>0):
            if STARTER: TS.append("%14s" % FI_var[j])
    print ""
    if STARTER: TS.append("\n")

    TS.append('#%s#   %s\n' % ('-'*calc_indent, FI_indi))
    



    # ... the rest.
    # use of 'REF_mat' to allow censoring by ANOVA values
    for i in range(npar):
        for j in range(nvar):
            #if FI_mat[i,j] < TO :
            if REF_mat[i,j] < TO :
                out = "%.4e" % (FI_mat[i,j])
                fout = out
            else:
                out = '-'
                fout = '0'
            print "%14s" % (out),
            TS.append("%14s" % (fout))
        print "  %s" % FI_par[i]
        TS.append("  # %s\n" % FI_par[i])


    if not(TS):
        print "**ERROR! Empty file?"
        sys.exit(12)

    return TS

''' OLD version

def ScreenAndFileOutput(PO, TO, FI_mat, FI_par, FI_var):

    nvar = len(FI_var)
    npar = len(FI_par)

    if  (npar, nvar) != np.shape(FI_mat) :
        print "Weird error in numbers not matching internally!"
        sys.exit(35)

    if PO: 
        outname = PO + '_REV.txt'
        f = open(outname, 'w')

    # first lining ...
    if PO: f.write('# %12s' % (FI_var[0]))
    for j in range(nvar):
        print "%14s" % (FI_var[j]),
        if (j>0) and PO:
            f.write("%14s" % FI_var[j])
    print ""
    if PO: f.write("\n")

    # ... the rest.
    for i in range(npar):
        for j in range(nvar):
            if FI_mat[i,j] < TO:
                out = "%.4e" % (FI_mat[i,j])
                fout = out
            else:
                out = '-'
                fout = '0'
            print "%14s" % (out),
            if PO: f.write("%14s" % (fout))
        print "  %s" % FI_par[i]
        if PO: f.write("  # %s\n" % FI_par[i])
    if PO: f.close()

    return 1
'''



###------------------------------------------------------------------
###------------------------------------------------------------------
###----------------- sTOP: review functions and output --------------
###------------------------------------------------------------------
###------------------------------------------------------------------

###------------------------------------------------------------------
###------------------------------------------------------------------
###---------------- START: Subsets of categorical vars --------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def SplitVar_Sublist(L, idx):
    '''Take in a list, and a 'which iteration is this' value called
    idx. Return either the first or second list in a lone-comma
    separated line.'''

    N = L.count(',')

    # only one list, can only go with first
    if N==0:
        if idx == 0 :
            return set(L)
        else:
            return set([])
    elif N==1:
        mm = L.index(',')
        if idx == 0 :
            return set(L[:mm])
        else:
            return set(L[mm+1:])
    else:
        print "Error reading variable list: too many commas!"
        print "\t There should only be a single comma, separating at most",
        print "two lists."
        sys.exit(18)





def Pars_CatVars_in_Listfile( file_listvars, 
                              par_list, 
                              var_iscateg, 
                              var_isinterac, 
                              Ninter ):
    '''User can specify subset of cat var for modelling. Might be
    useful if there are a lot of subcategories, and we want some
    pairwise tests.  Performed after we know where and what are the
    categorical variables (both lone and/or interaction ones.
    Returns: updated lists of categorical and interacting vars.'''

    coldat = ReturnFirstUncommentedSection_ofReadlines(file_listvars)
    for i in range(len(coldat)):
        if len(coldat[i]) > 1:
            print "Found a variable for subsetting"
            
            if var_iscateg[i]:
                newsub_set = set(coldat[i][1:]) # all other vars in that row
                oldvar_set = set(var_iscateg[i])
                nomatch = list(oldvar_set.__ixor__(newsub_set))
                print "Going to remove:", nomatch
                for x in nomatch:
                    var_iscateg[i].remove(x)

            elif var_isinterac[i]:
                ### with variable interaction terms, use a "lone"
                ### comma to separate the sub-lists; if you only want
                ### a subset of the second variable, then still use a
                ### lone comma before it
                for j in range(2):
                    if var_isinterac[i][2][j]:
                        newsub_set = SplitVar_Sublist(coldat[i][1:], j)
                        oldvar_set = set(var_isinterac[i][2][j])
                        nomatch = list(oldvar_set.__ixor__(newsub_set))
                        print "Going to remove:", nomatch
                        for x in nomatch:
                            var_isinterac[i][2][j].remove(x)


    return var_iscateg, var_isinterac



###------------------------------------------------------------------
###------------------------------------------------------------------
###---------------- STOP: Subsets of categorical vars ---------------
###------------------------------------------------------------------
###------------------------------------------------------------------


###------------------------------------------------------------------
###------------------------------------------------------------------
###--------------- START: Interaction modeling features -------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def VarLists_to_Strings_for_MVM(var_list, 
                                var_isinterac, 
                                var_iscateg, 
                                CAT_PAIR_COMP ):
    '''Get counts and things from varlists, interaction and category
    info.'''

    Nvar = len(var_list)

    # calc number of GLTs/ROI (= Nvartout) and number of Quant var
    Nvartout = Nvar
    var_list_quant = []
    varQ_str = ''
    var_list_inter = []
    varI_str = ''
    extra_qVar = '' # Jan,2015 -- added to pick up qvar in interac term
    varC_str = ''
    for i in range(Nvar):
        x = var_list[i]
        
        if not(var_isinterac[i][0][0]):
            if var_list.__contains__(x):
                if not( var_iscateg[i] ):
                    var_list_quant.append(x)
                    varQ_str+= "  %s" % x
                else:
                    nc = len(var_iscateg[i])
                    Nvartout+= nc-1              # individual ttests
                    if CAT_PAIR_COMP :
                        Nvartout+= (nc*(nc-1))/2 # pairwise combinatorial
        else:
            varI_str = "  "
            ncombo = 1
            for j in range( 2 ):
                varI_str+= var_isinterac[i][1][j]
                if var_isinterac[i][2][j] :
                    # binomial-type comparisons for each
                    nc = len(var_isinterac[i][2][j])
                    ncombo*= (nc*(nc-1))/2
                    varI_str+= "("
                    for ii in var_isinterac[i][2][j]:
                        varI_str+= " %s" % ii
                    varI_str+= " )"
                else: # Jan,2015
                    extra_qVar = "%s" % var_isinterac[i][1][j]
                if j == 0 :
                    varI_str+= " %s " % var_isinterac[i][0][0]
            Nvartout+= ncombo-1
                    
    for i in range(Nvar):
        if var_iscateg[i]:
            varC_str+= "  %s(" % var_list[i]
            for y in var_iscateg[i]:
                varC_str+= " %s" % y
            varC_str+= " )"




    return Nvartout, var_list_quant, varQ_str, var_list_inter, \
        varI_str, varC_str, extra_qVar








###------------------------------------------------------------------

def CheckFor_Cats_and_Inters( tab_data,
                              tab_colvars,
                              tab_coltypes,
                              var_list ):
    '''Return 1 if there is an interaction, and  '''

    simple_cat = []

    interac_terms = []
    count_interac = 0
    
    for x in var_list:
        # return either an empty list (no interac) or a list of
        # the interacting ones

        cats = []          # default if not cat var or interac
        inter_names = []    # default if no interacs
        Ncats = 0

        #print "TEST1 for var:",x
        check, inter_type = IsEntry_interaction(x)

        #print "\t->has check:", check,", and intertype:", inter_type

        #interac_terms = [inter_type]

        # if there is a non-empty list returned, check that both terms
        # in it are ok to use
        if check: 
            # inter_names will be a list of: lists (of categorical vars)
            # or empty lists (quant var)
            inter_names = CheckVar_and_FindCategVar( tab_data,       \
                                                       tab_colvars,  \
                                                       tab_coltypes, \
                                                       check )
            count_interac+=1

            # want to ensure categorical variable is first in list
            if not( inter_names[0] ):
                inter_names.reverse()
                check.reverse()

            for i in range(len(inter_names)):
                if inter_names[i]:
                    Ncats+=1


            # when there is interaction, have:
            # [0] type of interaction (or empty for none)
            # [1] var names (cat first or both)
            # [2] cat var names (or empty for quant)
            #interac_terms.append(check)
            #interac_terms.append(inter_names)
        else:
            # redefine cats if coming through here
            cats = CheckVar_and_FindCategVar( tab_data,       \
                                                  tab_colvars,  \
                                                  tab_coltypes, \
                                                  [x] )
            cats = list(cats[0])
        #print "\t\> has cat:", cats
        simple_cat.append(cats)
        interac_terms.append( [ [inter_type, Ncats], check, inter_names] )
    return simple_cat, interac_terms, count_interac



def IsEntry_interaction(str_var):
    '''Return an empty list if there is no interaction, or return a
    list of the interacting terms if there be/were one.'''

    type_col = ':' # this was a mistake to include!  Ooops.
    type_ast = '*'

    num_colon = str_var.count(type_col)
    num_aster = str_var.count(type_ast)

    if (num_colon + num_aster) > 1:
        print "Error! At most 1 interaction per var is permitted currently!"
        sys.exit(14)

    if num_colon :
        print "**ERROR! The ':' does not exist as a usable interaction call!"
        sys.exit(156)
        #return str_var.split(type_col), type_col
    elif num_aster :
        return str_var.split(type_ast), type_ast
    else:
        return [], ''


###------------------------------------------------------------------
###------------------------------------------------------------------
###---------------- STOP: Interaction modeling features -------------
###------------------------------------------------------------------
###------------------------------------------------------------------


###------------------------------------------------------------------
###------------------------------------------------------------------
###-------------------- START: Row selection features ---------------
###------------------------------------------------------------------
###------------------------------------------------------------------

#''' Input the grid file name, and return:
#    1)  a list of data [NMAT, NROI, NROI];
#    2)  a list of labels [NMAT];
#    3)  a supplementary dictionary for each calling use;
#    4)  and a list of (int) ROI labels (which may not be consecutive).'''

def Select_Row_From_FATmat(X4, outname, ele, ele_ind, UseL, ExternLabsOK):

    Ngrid = len( X4[1] )
    ROI_LABS = list(X4[UseL])
    Nroi = len( ROI_LABS )

    # have tested previously to make sure that ROI_LABS contains ele
    #ele_ind = ROI_LABS.index(ele)

    f = open(outname, 'w')
    f.write('# %d # %s\n' % (Nroi, HEADER_Nroi))
    f.write('# %d # %s\n' % (Ngrid, HEADER_Ngrid))

    if ExternLabsOK:
        # everything should have labels now
        f.write('# %s # %s\n' % (ele, HEADER_EleID))
        for roi in X4[4]:
            f.write("%12s\t" % roi)
        f.write('\n')
    
    for roi in X4[3]:
        f.write("%12s\t" % roi)
    f.write('\n')



    for i in range(Ngrid):
        f.write('# %s\n' % X4[1][i])
        for k in range(Nroi):
            if X4[1][i]=='NT':
                f.write('%12d\t' % X4[0][i][ele_ind][k])
            else:
                f.write('%12e\t' % X4[0][i][ele_ind][k])
        f.write('\n')

    f.close()






###------------------------------------------------------------------

def MakeRowFile_From_FATmat(fname, outname, ele, ExternLabsOK):

    X4 = LoadInGridOrNetcc(fname)

    UseL = 3
    if (len(X4) >= 4) and ExternLabsOK:
        if X4[4]:
            UseL = 4
    
    # make labels for WITH_LABELS...
    if not(X4[4]):
        for x in X4[3]:
            # take half of element id
            name = ElementNameMaker(x, x, 0)
            lele = (len(name)-len(ConnNames))/2
            X4[4].extend( name[:lele] )

    if X4[UseL].__contains__(ele):
        ele_ind = X4[UseL].index(ele)
        y = Select_Row_From_FATmat(X4, 
                                   outname, 
                                   ele, 
                                   ele_ind, 
                                   UseL, 
                                   ExternLabsOK)
    # extra check: even if LABELS are used, might still refer to ID
    # number ok
    elif (UseL == 4) and X4[3].__contains__(ele):
        print "*+ Even though you have labels in the file, it looks like",
        print "your 'roi' selection is just a digit."
        print "\t-> Not a problem-- row selection will be done based on ROI",
        print "number (and not on labeltable-info)."
        ele_ind = X4[3].index(ele)
        y = Select_Row_From_FATmat(X4, 
                                   outname, 
                                   ele, 
                                   ele_ind,
                                   3, 
                                   ExternLabsOK)
    else:
        print "** Error: chosen element ",
        print "'%s' doesn't appear in file's ROI list!" % ele
        print "For reference, the list of ROIs is:"
        for x in X4[UseL]:
            print "\t",x
        sys.exit(55)

    return y

###------------------------------------------------------------------

def DefaultNamingOutRowfile(list_all, ele, ExternLabsOK):
    
    out = []

    # take half of element id
    name = ElementNameMaker(ele, ele, ExternLabsOK)
    lele = (len(name)-len(ConnNames))/2
    roi = name[:lele] 

    for x in list_all:
        if not(x[-5:] == '.grid') and not(x[-6:] == '.netcc'):
            print "ERROR-- doesn't look like this is a *.grid or *netcc file!"
        elif x[-5:] == '.grid' :
            out.append(x[:-5]+'_grid_'+roi+'.row')
        else:
            out.append(x[:-6]+'_netcc_'+roi+'.row')

    return out

###------------------------------------------------------------------
###------------------------------------------------------------------
###--------------------- STOP: Row selection features ---------------
###------------------------------------------------------------------
###------------------------------------------------------------------


###------------------------------------------------------------------
###------------------------------------------------------------------
###----------------------- START: Assorted --------------------------
###------------------------------------------------------------------
###------------------------------------------------------------------

def RecapAttach(comm_str, opt, arg):
    '''Silly parsing of commandline options+arguments in order to be able
    to return the command with delineating apostrophes around user-input
    lists.'''

    comm_str+=' '+opt
    if arg:
        if opt[:2]=='--':
            comm_str+="="
        else:
            comm_str+=" "
        comm_str+="'"+arg+"' "
    else:
        comm_str+=" "

    return comm_str

###------------------------------------------------------------------

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

#----------------------------------------------------------------

# Sep,2015: new function to make table from subnetwork of ROIs
def MakeSubTable(file_table, pref_subnet, roi_list):

    tab_raw, tab_colvars = LoadInTable(file_table)
    Ncol = len(tab_colvars)
    Nrois = len(roi_list)
    
    # check for 'ROI' in col heading
    indroi = Check_EleIn_ROIlist(tab_colvars, ColEnding[0])
    if indroi < 0:
        print "** ERROR: can't find 'ROI' in column headings of"
        print "    the table file:  %s." % file_table
        sys.exit(543)

    list_count = np.zeros(Nrois, dtype=int)
    tab_subnet = []
    # go through row by row and keep those that have the 'right' ROI
    for x in tab_raw:
        if roi_list.__contains__(x[indroi]) :
            tab_subnet.append(x)
            list_count[roi_list.index(x[indroi])] = 1
            
    # check that each chosen ROI was found
    if not(list_count.all()) :
        for i in range(Nrois):
            if not( list_count[i]):
                # fixed indexing mistake in roi_list[].  [PT, March 27, 2017]
                print "** ERROR: can't find selected ROI %s in the original table list" % roi_list[i]
        print "** Please select again!"
        sys.exit(544)
    else:
        print "++ Found all %d ROIs in the desired subnet list:" % Nrois
        for x in roi_list:
            print "\t  %s" % x
        
    Lx, Ly = np.shape(tab_subnet)

    # write output
    fff = open(pref_subnet, 'w')
    for x in tab_colvars:
        fff.write("%s " % x)
    fff.write("  \\\n")
    for i in range(Lx-1):
        for x in tab_subnet[i]:
            fff.write("%s " % x)
        fff.write("  \\\n")
    for x in tab_subnet[Lx-1]:
        fff.write("%s " % x)
    fff.write("\n")

    fff.close()

    print "++ New subnetwork table has been made: %s" % pref_subnet
    
    return 1
            
            
#----------------------------------------------------------------

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
###------------------------ STOP: Assorted --------------------------
###------------------------------------------------------------------
###------------------------------------------------------------------



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
                    grid_subj,
                    ExternLabsOK):
    '''Take a lot of matrix file info in, as well as the CSV file and
    conversion dictionary, and return a tableau of necessary
    variable&parameter values from all the subjects' matrix file data.
    Output is a list ordered by CSV subject, for ease in final
    writing.'''

    # here, gridVARlist is actually a list of PARAMETERS.

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
            UseL = 3
            if (len(grid_data[ii]) >= 4) and ExternLabsOK:
                if grid_data[ii][4]:
                    UseL = 4
            Ntar = len(grid_data[ii][UseL])
            for j in range(Nroi):
                for x in range(Ntar):
                    for y in range(x+1,Ntar):
                        # inefficient search...
                        ele = ElementNameMaker(grid_data[ii][UseL][x], 
                                               grid_data[ii][UseL][y],
                                               UseL-3)
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
    '''Check all lines of input to see if there's more than one
    column present.'''
    tt = ReturnFirstUncommentedSection_ofReadlines(fname)
    if tt:
        for i in range(len(tt)):
            if len(tt[i]) > 1:
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

def FindGroupwiseTargets(All_sub_grid, ftype, ExternLabsOK, UNION=0):
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

    # START EDITING HERE WITH USING LABELS
    a = GetSet(All_sub_grid[0], ftype, ExternLabsOK)
    print '\tThe number of elements in the ROI matrix set is:\n\t  ',
    for i in range(1,Nsub):
        print '%d,' % len(a),
        if UNION: 
            # updating with the *union* of regions
            a.update(GetSet(All_sub_grid[i], ftype, ExternLabsOK))
        else:
            a.intersection_update(GetSet(All_sub_grid[i], ftype, ExternLabsOK))
    print '%d.' % len(a)

    temp = list(a)
    temp.sort()
    print '\tThe set of ROIs is:'
    for t in temp:
        print '\t   %s' % t

    ## For finding the set of parameters
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

def GetSet(x, ftype, ExternLabsOK):
    '''Input one of the 4-tuples returned by LoadInGridOrNetcc.
    Output is a 'set' of nonempty target-connecting elements.
    FOR NOW: this is specifically for a GRID file, where there's
    a nice 'NT' set of ints.'''
    
    if not(ftype=='GRID' or ftype=='NETCC'):
        print "** ERROR: unrecognized matrix type. Don't know what to do."
        sys.exit(34)

    # default: use the boring old numbers themselves as ROI labels,
    # just zero-padding them.
    # switch: if there's an externally input list of labels, such as
    # from a labeltable, then use those.
    UseL = 3
    if (len(x) >= 4) and ExternLabsOK:
        if x[4]:
            UseL = 4


    #if ftype=='GRID': # later, grid vs netcc
    Ntar = len(x[UseL])
    roi_set = set()  # empty set
    # should be UHT, nondiagonal selector
    for i in range(Ntar):
        for j in range(i+1,Ntar):
            # NETCC: all UHT elements
            # GRID: UHT elements, with nonzero 'NT' values
            if ((ftype=='GRID') and x[0][x[2][GridCheckVar]][i,j]) \
                 or (ftype=='NETCC'):
                roi_set.add(ElementNameMaker(x[UseL][i], x[UseL][j],UseL-3))

    return roi_set

###------------------------------------------------------------------

def ElementNameMaker(A, B, AS_IS):
    '''Current format for turning integer ROI labels A and B into ROI
    element name.  If AS_IS=0, then zero pad.  Else: use names as they
    is.'''
    # above, if UseL=3, then AS_IS=0 --> zero pad.

    # paranoia
    if (type(A) != str) or (type(B) != str):
        print "** ERROR: problem in element name-maker. Help!"
        sys.exit(43)
        
    # first case is original style
    # second case is if data labels are used
    if not(AS_IS) and A.isdigit() and B.isdigit():
        aint = int(A)
        bint = int(B)

        if aint > 999:
            print "*+ POSSIBLE error in element maker: big label %d > 999!" % A
            print "\tUgliness may ensue!"
        if bint > 999:
            print "*+ POSSIBLE error in element maker: big label %d > 999!" % B
            print "\tUgliness may ensue!"

        x = str("%03d" % (aint))
        y = str("%03d" % (bint))

    else:
        x = str(A)
        y = str(B)

    return x+ConnNames+y

#### Old version: used to treat ROI labels of elements as ints; 
#### new possibility that they will be strings now-- so treat all as str
#def ElementNameMaker(A, B):
#    '''Current format for turning integer ROI labels into 
#    ROI element name.'''
#    # paranoia
#    if (type(A) != int) or (type(A) != int):
#        print "ERROR in element maker: why not 'int' element labels?"
#    if A > 999:
#        print "POSSIBLE error in element maker: big label %d > 999!" % A
#    if B > 999:
#        print "POSSIBLE error in element maker: big label %d > 999!" % B
#
#    x = str("%03d" % (A))
#    y = str("%03d" % (B))
#    return x+'_'+y



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
        X[s] = X[s].strip()    # first strip away leading/trailing WS; aug,2015
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

def ConvertCSVfromStr(dat1, head1, NA_WARN):
    '''Take the all-string input and header lists in,
       and return a copy with ints, floats and strings.'''

    print '++ Converting (hopefully appropriate) strings to numbers...'

    # careful copying...  need to recursively copy each list
    # unattachedly

    # Jan,2015 !!!!
    #    dat2 = [list(x) for x in dat1] # OLD method
    # ---> new method: skip empty lines
    FOUND_WS = 0
    FOUND_EL = 0
    dat2 = []
    for x in dat1:
        if x:                      # check if empty line
            if x[0].split() :      # check if just whitespace
                dat2.append(x)
            else:
                FOUND_WS = 1
        else:
            FOUND_EL = 1

    if FOUND_WS:
        print "\t FYI: removed at least one whitespace line from CSV file."
    if FOUND_EL:
        print "\t FYI: removed at least one empty line from CSV file."

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
                        head2[j] = float    # Oct, 2014 fix
                        for k in range(Lx):
                            if type(dat2[k][j]) == int:
                                dat2[k][j] = float(dat2[k][j])
                            else:
                                "Odd combo..."
                    elif (dat2[i][j] == 'NA'):
                        if NA_WARN:
                        #print "Loc (%d, %d) has value 'NA'." % (i,j)
                            print "*+ Warning: 'NA' value in Col %d (='%s') !" \
                             % ( j, head1[j] )
                    else:
                        print "Odd mixing in types of data in Col %d (=%s) " \
                         % ( j, head1[j] ),
                        print "\t-> seeing both %s and %s." % (ty0, ty)
                elif ty0 == float:
                    if ty == int:
                        #print "\tin CSV col %d ->" % j,
                        #print "making %d float." % dat2[i][j]
                        dat2[i][j] = float(dat2[i][j])
                    elif dat2[i][j] == 'NA':
                        if NA_WARN:
                        #print "Loc (%d, %d) has value 'NA'." % (i,j)
                            print "*+ Warning: 'NA' value in Col %d (='%s') !" \
                             % ( j, head1[j] )
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
    y1,y2,y3,y4,y5 = SepHeaderFile(x, fname)
    return y1,y2,y3,y4,y5

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

    Nroi,Nmat,ListLabels,ReadNext,ROI_str_labs = \
     HeaderInfo(RawX[:NLinesGridHead]) # header

    y = RawX[ReadNext:]  # info grids

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
    return OUT_data, OUT_labs, OUT_dlab,ListLabels,ROI_str_labs

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
        sys.exit(52)

    Nroi = int(RawX[0].split()[1])
    if Nroi <=0:
        print "ERROR reading header! Number of ROIs is: %d" % Nroi
        sys.exit(53)

    Nmat = int(RawX[1].split()[1])
    if Nmat <=0:
        print "ERROR reading header! Number of Matrices is: %d" % Nmat
        sys.exit(54)

    # Sept 2014: might have labels here.
    # defaults
    #HAVE_LABS = 0
    listreadline = 2
    ReadNext = 3
    ROI_str_labs = []
    # check about changing if LABELS are input
    temp = RawX[2].split()
    if len(temp) > 1:
        if temp[1] == HEADER_Labels:
            #HAVE_LABS = 1
            listreadline = 4
            ReadNext = 5
            ROI_str_labs = RawX[3].split()
            ll = len(ROI_str_labs)
            if not( ll == Nroi ):
                print "ERROR reading header!"
                print "  Number of ROIs (%d) doesn't match stringnames (%d)" % \
                 (Nroi,ll)
                sys.exit(55)

    #if HAVE_LABS :
    #    print "++ Have ROI LABELS: reading."

    ListLabels=RawX[listreadline].split()
    ll = len(ListLabels)
    if not( ll == Nroi ):
        print "ERROR reading header!"
        print "  Number of ROIs (%d) doesn't match Labels (%d)" % (Nroi,ll)
        sys.exit(55)

    ### Don't do this--> may have non-numeric labels
#    for i in range(ll):
#        ListLabels[i] = int(ListLabels[i])
#        if ListLabels[i] <=0:
#            print "ERROR reading header! ListLabel[%d]=%d." % \
#            (i,ListLabels[i])
#            sys.exit(56)

    return Nroi, Nmat, ListLabels, ReadNext, ROI_str_labs

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
    f.write('# %d # %s\n' % (Nroi, HEADER_Nroi))
    f.write('# %d # %s\n' % (Ngrid, HEADER_Ngrid))

    #f.write('# %s \n' % HEADER_Labels)  
    #for roi in ROI_LABS:
    #    tt = "Funcky%dFresh" % int(roi)
    #    f.write("%12s\t" % tt)
    #f.write('\n')

    for roi in ROI_LABS:
        f.write("%12s\t" % roi)
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

