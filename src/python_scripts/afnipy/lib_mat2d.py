#!/usr/bin/env python

# python3 status: compatible

# Objects and functionality for dealing with matrix-related things,
# for plotting, stats and other calcs.
#
# In particular, these funcs are for interacting with 3dNetCorr and
# 3dTrackID outputs.

# --------------------------------------------------------------------------
#
# auth: 'PA Taylor'
#
#ver = '0.0' ; date = 'June 1, 2020'
# [PT] matrix-related things, for plotting, stats and other calcs
#
#ver = '0.1' ; date = 'June 1, 2020'
# [PT] debugged, testing on netcc and grid files
#
ver = '0.2' ; date = 'June 1, 2020'
# [PT] tweaks with eletype
#
# --------------------------------------------------------------------------

import sys, os, copy
from afnipy import afni_base as ab
from afnipy import afni_util as UTIL

# ---------------------------------------------------------------------------

class mat2d:
    """A class for a 2D matrix;  need not be square.  

    At the moment, not intended to allow raggedness: will be
    zeropadded (or fail).

    Because it can be generally rectangular, labels and things are all
    treated separately for rows and cols; wrappers can simplify
    creating these things.

    """

    ok_eletypes = ['bool', 'int', 'float']

    def __init__( self, M, label=None, 
                  eletype=None, allow_ragged=False, 
                  col_strlabs=None, row_strlabs=None,
                  col_intvals=None, row_intvals=None ):

        self.mat         = []   # data vals
        self.nrow        = 0
        self.ncol        = 0
        self.nelements   = 0
        self.label       = ''   # what is matrix
        self.col_strlabs = []   # e.g., can be labeltable/atlaspoint labels
        self.row_strlabs = [] 
        self.col_intvals = []   # e.g., can be ROI int vals
        self.row_intvals = []
        self.eletype     = None # int, float, bool...
        self.file_base   = ''   # can record file basename
        self.file_dir    = ''   # can record file dirname
        

        # ---- set vals ------

        # set mat, nrow, ncol, is_square
        self.set_mat(M, 
                     mform = None,
                     allow_ragged=allow_ragged,
                     eletype=eletype) 

        if label :
            self.label = label

        if col_strlabs :
            self.set_col_strlabs(col_strlabs)
        if row_strlabs :
            self.set_row_strlabs(row_strlabs)

        if col_intvals :
            self.set_col_intvals(col_intvals)
        if row_intvals :
            self.set_row_intvals(row_intvals)

    # -------------- methods -------------------        

    def is_square(self):
        if self.nrow and self.ncol and self.nrow == self.ncol :
            return True
        else:
            return False

    def set_col_strlabs(self, L):
        '''input a list of strings; must match number of mat cols'''

        N = len(L) 
        if N != self.ncol :
            ab.EP("Number of labels in list ({}) does not match \n"
                  "number of cols ({})".format(N, self.ncol))
            
        self.col_strlabs = []
        for x in L:
            self.col_strlabs.append(x)
        #ab.IP("Set col labels")

    def set_row_strlabs(self, L):
        '''input a list of strings; must match number of mat rows'''

        N = len(L) 
        if N != self.nrow :
            ab.EP("Number of labels in list ({}) does not match \n"
                  "number of rows ({})".format(N, self.nrow))
            
        self.row_strlabs = []
        for x in L:
            self.row_strlabs.append(x)
        #ab.IP("Set row labels")

    def set_col_intvals(self, L):
        '''input a list of strings; must match number of mat cols'''

        N = len(L) 
        if N != self.ncol :
            ab.EP("Number of int vals in list ({}) does not match \n"
                  "number of cols ({})".format(N, self.ncol))
            
        self.col_intvals = []
        for x in L:
            self.col_intvals.append(int(x))
        #ab.IP("Set col int vals")

    def set_row_intvals(self, L):
        '''input a list of strings; must match number of mat rows'''

        N = len(L) 
        if N != self.nrow :
            ab.EP("Number of int vals in list ({}) does not match \n"
                  "number of rows ({})".format(N, self.nrow))
            
        self.row_intvals = []
        for x in L:
            self.row_intvals.append(int(x))
        #ab.IP("Set row int vals")

    def set_mat(self, M, mform=None, eletype=None, allow_ragged=False):
        '''Set mat, nrow, ncol

        Ragged matrices not allowed by default (at present)'''

        self.mat = [] # reset

        if type(M) == list or mform == 'LIST' :
            # input M is a list (of lists) of numbers
            for rrr in M :
                self.mat.append(rrr)
        else:
            ab.EP("Unrecognized input mat format\n")

        self.set_mat_dims( allow_ragged=allow_ragged )

        if eletype :
            self.set_eletype(eletype)
            #ab.IP("Matrix elements set to be type: {}"
            #      "".format(eletype))

    def set_eletype( self, ET ):
        """Set type of each element in the matrix

        Allowed types are in the 'ok_eletypes' list.
        """

        self.eletype = ET

        if ET == 'float' or ET == float :
            for i in range(self.ncol):
                for j in range(self.nrow):
                    self.mat[i][j] = float(self.mat[i][j])
        elif ET == 'int' or ET == int :
            for i in range(self.ncol):
                for j in range(self.nrow):
                    self.mat[i][j] = int(self.mat[i][j])
        elif ET == 'bool' or ET == bool :
            for i in range(self.ncol):
                for j in range(self.nrow):
                    self.mat[i][j] = bool(self.mat[i][j])
        else:
            ab.EP("Don't have element type '{}' in recognized list\n"
                  "for mat elements:\n\t {}"
                  "".format(ET, ", ".join(self.ok_eletypes)))


    def set_mat_dims(self, allow_ragged=False):
        '''Set ncol and nrol of mat in this object.  

        If a ragged matrix is detected, fail by default (or zeropad,
        if user wants)

        '''

        if not(self.mat) :
            ab.EP("Can't check dimensions of empty mat")

        self.ncol = len(self.mat)

        lrows = [len(r) for r in self.mat]
        lmin  = min(lrows)
        lmax  = max(lrows)

        if lmin != lmax :
            if allow_ragged :
                ab.WP("ragged matrix: min = {}, max = {}\n"
                      "-> am zeropadding".format(lmin, lmax))
                for ii in range(self.ncol):
                    diff = lmax - lrows[ii]
                    if diff :
                        for jj in range(diff):
                            self.mat[ii].append(0)
            else:
                ab.EP("ragged matrix: min = {}, max = {}\n"
                      "no flag to allow ragged mats".format(lmin, lmax))

        self.nrow = lmax
        
        self.nelements = self.nrow * self.ncol

        ab.IP("mat dims (ncol, nrow): {}, {}".format(self.ncol, self.nrow))


# ------------------------------------------------------------------------

class file_grid_netcc:
    """
    Read in output from 3dNetCorr or from 3dTrackID.  
    """

    def __init__( self, fname ):

        self.data        = []       # full file, read in 
        self.data_len    = 0        # nlines file file
        self.header      = []       # just the header part of data
        self.header_len  = 0        # number of lines in header

        self.nmat        = 0        
        self.nroi        = 0

        self.allmat      = {}       # key=mat_lab; value=mat2d obj
        self.allmat_labs = []       # order list of mat_labs

        self.has_strlabs = False
        self.roi_strlabs = []       # file might not have these
        self.roi_intvals = [] 

        self.ext         = ''       # grid|netcc
        self.file_inp    = ''       # can record file basename
        #self.file_base   = ''       # can record file basename
        #self.file_dir    = ''       # can record file dirname


        # --------- 

        self.file_inp = fname
        self.open_and_check_GoN( )

    # -----------------------------------------------------------

    def read_in_GoN(self):
        '''
        set self.data and self.data_len 
        '''

        self.data = []
        fff = open(self.file_inp, 'r')
        for lll in fff.readlines():
            self.data.append(lll)
        fff.close()

        self.data_len = len(self.data)

    def set_ext(self) :
        '''
        set self.ext
        
        Don't know where the "OTHER" would come from??
        '''

        fname_split = self.file_inp.split('.')
        ext         = fname_split[-1]

        if ext == 'grid' :
            self.ext = 'grid'
        elif ext == 'netcc' :
            self.ext = 'netcc'
        else:
            ab.WP("Unrecognized file extension on what should be "
                  "grid/netcc file: {}".format(ext))
            self.ext = 'OTHER'
            
    def read_header_GoN(self):
        '''
        Read the header/top part of file, to get:
        self.nroi, self.nmat 
        as well as string labels and integer ROI values
        '''
        
        if self.data_len < 3 :
            ab.EP("Can't possibly be grid/netcc file: only {} lines"
                  "".format(self.data_len))

        # Now read through each line ('idx'=line number)

        # Number of ROIs per mat (req)
        idx  = 0
        line = [ x.strip() for x in self.data[idx].split("#")]
        if line[-1] == 'Number_of_network_ROIs' or \
           line[-1] == 'Number of network ROIs' :
            self.nroi = int(line[1])
        else:
            ab.EP("Can't recognize line {} format in {}:\n"
                  "{}".format(idx, self.file_inp, line))

        # Number of mats in this file (req)
        idx  = 1
        line = [ x.strip() for x in self.data[idx].split("#")]
        if line[-1] == 'Number_of_grid_matrices'  or \
           line[-1] == 'Number of grid matrices'  or \
           line[-1] == 'Number of netcc matrices' :
            self.nmat = int(line[1])
        else:
            ab.EP("Can't recognize line {} format in {}:\n"
                  "{}".format(idx, self.file_inp, line))

        # Check for str labs (opt)
        idx  = 2
        line = [ x.strip() for x in self.data[idx].split("#")]
        if line[-1] == 'WITH_ROI_LABELS':
            self.has_strlabs = True
            idx+= 1
            self.roi_strlabs = [ x.strip() for x in self.data[idx].split()]

        if self.has_strlabs : idx = 4
        else:                 idx = 2

        # ROI integer values (req)
        self.roi_intvals = [ int(x.strip()) for x in self.data[idx].split()]

        # check lengths
        nintvals = len(self.roi_intvals)
        if nintvals != self.nroi :
            ab.EP("Mismatch in number of int vals labels {} and\n"
                  "purported number of ROIs per mat {}"
                  "".format(nintvals, self.nroi))
        if self.has_strlabs:
            nstrlabs = len(self.roi_strlabs)
            if nstrlabs != nintvals :
                ab.EP("Mismatch in number of string labels {} and\n"
                      "ROI int vals {}"
                      "".format(nstrlabs, nintvals))
        
        self.header_len = idx+1
        self.header     = self.data[:self.header_len]

        # consistency check for file
        nleft_theory  = self.nmat * (self.nroi + 1)
        nleft_reality = self.data_len - self.header_len
        if nleft_reality < nleft_theory :
            ab.EP("Too few lines remaining ({}) after reading header,\n"
                  "since we have {} matrices each with {} ROIs\n"
                  "(and a row per mat for the label, needing {} rows total)"
                  "".format(nleft_reality, self.nmat, self.nroi,
                            nleft_theory))

    def read_table_mats_GoN(self):
        '''Read all the matrix info in the main part of the table
        each matrix has a "matrix label" part
        self.nroi, self.nmat 
        as well as string labels and integer ROI values

        We guess whether matrix element type is int or float based on
        whether a matrix has any '.' in it or not (respectively).

        '''
        
        # starting point from which to read, verily
        idx = self.header_len

        self.allmat_labs = []
        self.allmat      = {}

        while idx < self.data_len :
            # first, the matrix label line
            line    = [ x.strip() for x in self.data[idx].split("#")]
            mat_lab = line[-1]
            ab.IP("Getting matrix: {}".format(mat_lab))
            self.allmat_labs.append(mat_lab)
            idx+=1 

            # then, the matrix lines
            count_dots = 0 # use this as a way to tell int vs float
            M          = []
            for ii in range(self.nroi):
                count_dots+= self.data[idx+ii].count('.')
                line = [ x.strip() for x in self.data[idx+ii].split()]
                M.append(line)
            if count_dots :
                self.allmat[mat_lab] = mat2d(M, eletype=float,
                                             col_strlabs=self.roi_strlabs,
                                             row_strlabs=self.roi_strlabs,
                                             col_intvals=self.roi_intvals,
                                             row_intvals=self.roi_intvals)
            else:
                self.allmat[mat_lab] = mat2d(M, eletype=int,
                                             col_strlabs=self.roi_strlabs,
                                             row_strlabs=self.roi_strlabs,
                                             col_intvals=self.roi_intvals,
                                             row_intvals=self.roi_intvals)
            idx+= ii+1 

        if len(self.allmat_labs) != self.nmat :
            ab.EP("Mismatch in number of matrix labels {} and\n"
                  "purported number of matrices in the file {}"
                  "".format(self.allmat_labs, self.nmat))


    def open_and_check_GoN(self):

        if not(os.path.isfile(self.file_inp)) :
            ab.EP("Cannot find grid/netcc file: {}".format(file_inp))

        self.set_ext()
        self.read_in_GoN()

        self.read_header_GoN()
        self.read_table_mats_GoN()

