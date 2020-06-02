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
#ver = '0.2' ; date = 'June 1, 2020'
# [PT] tweaks with eletype
#
#ver = '0.3' ; date = 'June 2, 2020'
# [PT] add in writing/printing of the file
#
#ver = '0.31' ; date = 'June 2, 2020'
# [PT] ... fix printing bugs, add in other funcs to print subsets of things
#
ver = '0.32' ; date = 'June 2, 2020'
# [PT] attach label to mat2d obj when reading in file
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
                  col_intvals=None, row_intvals=None,
                  file_inp=None ):

        self.mat         = []   # data vals
        self.nrow        = 0
        self.ncol        = 0
        self.nelements   = 0
        self.label       = ''   # what is matrix

        self.min_ele     = None
        self.max_ele     = None
        self.is_square   = False
        self.is_ragged   = False

        self.col_strlabs = []   # e.g., can be labeltable/atlaspoint labels
        self.row_strlabs = [] 
        self.col_intvals = []   # e.g., can be ROI int vals
        self.row_intvals = []

        self.eletype     = None # int, float, bool...
        self.file_inp    = None
        #self.file_base   = ''   # can record file basename
        #self.file_dir    = ''   # can record file dirname
        
        # ---- set vals ------

        # set mat, nrow, ncol, is_square
        self.set_mat(M, 
                     mform = None,
                     allow_ragged=allow_ragged,
                     eletype=eletype) 

        if label :
            self.label = label
        if file_inp :
            self.file_inp = file_inp            

        if col_strlabs :
            self.set_col_strlabs(col_strlabs)
        if row_strlabs :
            self.set_row_strlabs(row_strlabs)

        if col_intvals :
            self.set_col_intvals(col_intvals)
        if row_intvals :
            self.set_row_intvals(row_intvals)

    # -------------- methods -------------------        

    def get_mat_min_max(self):
        '''calculate min and max values of entire array'''
        amin = []
        amax = []
        for rrr in self.mat:
            amin.append(min(rrr))
            amax.append(max(rrr))
        self.min_ele = min(amin)
        self.max_ele = max(amax)

    def mat_col_minrow_maxrow_ragged_square(self):
        """ check 5 properties and return 5 values:
        ncol      (int)
        min nrow  (int)
        max nrow  (int)
        is_ragged (bool)
        is_square (bool)
        """

        is_square = False         # just default; can change below

        ncol      = len(self.mat) # get from mat, cd use attribute

        all_rlen  = [len(r) for r in self.mat]
        minrow    = min(all_rlen)
        maxrow    = max(all_rlen)

        if minrow == maxrow :
            is_ragged = False
            if minrow == ncol :
                is_square = True
        else:
            is_ragged = True

        return ncol, minrow, maxrow, is_ragged, is_square

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
            # input M is a list (of lists) of numbers (or strings,
            # actually, bc eletype can format them)
            for rrr in M :
                self.mat.append(rrr)
            # at this point, elements might be strings still; wait
            # till after 'if eletype :' below to use values
        else:
            ab.EP("Unrecognized input mat format\n")

        self.set_mat_dims( allow_ragged=allow_ragged )

        if eletype :
            self.set_eletype(eletype)
            #ab.IP("Matrix elements set to be type: {}"
            #      "".format(eletype))

        nc, nrmin, nrmax, is_rag, is_sq \
            = self.mat_col_minrow_maxrow_ragged_square()                 
        self.is_ragged = is_rag
        self.is_square = is_sq

        # store min and max of ele
        self.get_mat_min_max()

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

        nc, nrmin, nrmax, is_rag, is_sq \
            = self.mat_col_minrow_maxrow_ragged_square()                 

        self.ncol = nc

        if is_rag :
            if allow_ragged :
                ab.WP("ragged matrix: min = {}, max = {}\n"
                      "-> am zeropadding".format(nrmin, nrmax))
                for ii in range(self.ncol):
                    diff = nrmax - len(self.mat[ii])
                    if diff :
                        for jj in range(diff):
                            self.mat[ii].append(0)
            else:
                ab.EP("ragged matrix: min = {}, max = {}\n"
                      "no flag to allow ragged mats".format(nrmin, nrmax))

        self.nrow = nrmax
        
        self.nelements = self.nrow * self.ncol

        ab.IP("mat dims (ncol, nrow): {}, {}".format(self.ncol, self.nrow))


# ------------------------------------------------------------------------

class file_grid_netcc:
    """
    Read in output from 3dNetCorr or from 3dTrackID.  

    Default mode is to "read" (mode='r') an input filename.

    """

    def __init__( self, fname='', mode='r', fout='' ):

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

        self.ext         = ''       # grid|netcc|amat
        self.file_inp    = ''       # can record file basename
        self.file_out    = ''       # can write to a diff file
        #self.file_base   = ''       # can record file basename
        #self.file_dir    = ''       # can record file dirname


        # --------- 
        if mode == 'r' :
            self.file_inp = fname
            self.open_and_check_GoN( )

    # -----------------------------------------------------------
    # -----------------------------------------------------------

    def make_header_GoN(self):
        """Form the header part for output.

        Can be netcc, grid or 'amat' (generalized form, 'AFNI matrix')
        """
        
        # haven't fully decided about "other" cases yet
        if    self.ext == 'netcc' : mstr = "Number of netcc matrices"
        elif  self.ext == 'grid'  : mstr = "Number of grid matrices"
        elif  self.ext == 'amat'  : mstr = "Number of amat matrices"
        else:                       mstr = "Number of amat matrices"

        h = []

        h.append( "# {}  # Number of network ROI".format(self.nroi) )
        h.append( "# {}  # {}".format(self.nmat, mstr) )
        if self.has_strlabs :
            h.append( "# WITH_ROI_LABELS" )
            h.append( ''.join([" {:>10s} \t".format(x) \
                               for x in self.roi_strlabs]) )
        h.append( ''.join([" {:10d} \t".format(x) \
                           for x in self.roi_intvals]) )
        return h

    def make_table_mat_GoN(self):
        """Form the table of matrix(ces) part for output.

        Can be netcc, grid or 'amat' (generalized form, 'AFNI matrix')
        """
        
        N = self.nmat
        t = []

        for ii in range(N):
            lab = self.allmat_labs[ii]
            mat = self.allmat[lab].mat
            ele = self.allmat[lab].eletype
            t.append("# {}".format( lab ))
            for jj in range(self.nroi):
                if ele == int or ele == bool :
                    t.append( ''.join(["{:12d}\t".format(x) for 
                                       x in mat[jj]]) )
                elif ele == float :
                    t.append( ''.join(["{:12e}\t".format(x) for 
                                       x in mat[jj]]) )
                else:
                    t.append( ''.join(["{:12}\t".format(x) for 
                                       x in mat[jj]]) )
        return t

    def make_full_out_str_GoN(self):
        """Create full string version of grid/netcc/amat file to print or
        write.

        """

        hdr_list       = self.make_header_GoN()
        table_mat_list = self.make_table_mat_GoN()

        full_out = '\n'.join(hdr_list)
        full_out+= '\n'
        full_out+= '\n'.join(table_mat_list)

        return full_out


    def disp_tab_mat(self):
        """Print current table of matrix (non-header) information to terminal

        """
        
        table_mat_list = self.make_table_mat_GoN()
        full_out       = '\n'.join(table_mat_list)
        print(full_out)

    def disp_hdr(self):
        """Print current header information to terminal

        """
        
        hdr_list = self.make_header_GoN()
        full_out = '\n'.join(hdr_list)
        print(full_out)


    def disp_full(self):
        """Print current information (all) to terminal

        """

        full_out = self.make_full_out_str_GoN()
        print(full_out)
       

    def write_to_file_GoN(self, fname=''):
        """Write current information to a file on disk.

        fname can be provided here (gets precedence), or have been
        provided and stored in the obj previously.

        """
        full_out = self.make_full_out_str_GoN()

        if fname :
            ooo = fname
        elif self.file_out :
            ooo = self.file_out
        else:
            ab.EP("Don't have an output name for this file-- can't write")

        fff = open(ooo, 'w')
        fff.write(full_out)
        fff.close()
        ab.IP("Wrote {} file: {}".format(self.ext, ooo))


    # ------------------------------------------------------------

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
        elif ext == 'amat' :
            self.ext = 'amat'
        else:
            ab.WP("Unrecognized file extension on what should be "
                  "grid/netcc/amat file: {}".format(ext))
            self.ext = 'amat'
            
    def read_header_GoN(self):
        '''
        Read the header/top part of file, to get:
        self.nroi, self.nmat 
        as well as string labels and integer ROI values
        '''
        
        if self.data_len < 3 :
            ab.EP("Can't possibly be grid/netcc/amat file: only {} lines"
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
           line[-1] == 'Number of netcc matrices' or \
           line[-1] == 'Number of amat matrices' :
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
                self.allmat[mat_lab] = mat2d(M,
                                             label=mat_lab,
                                             eletype=float,
                                             col_strlabs=self.roi_strlabs,
                                             row_strlabs=self.roi_strlabs,
                                             col_intvals=self.roi_intvals,
                                             row_intvals=self.roi_intvals)
            else:
                self.allmat[mat_lab] = mat2d(M,
                                             label=mat_lab,
                                             eletype=int,
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
            ab.EP("Cannot find grid/netcc/amat file: {}".format(file_inp))

        self.set_ext()
        self.read_in_GoN()

        self.read_header_GoN()
        self.read_table_mats_GoN()

