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
#ver = '0.32' ; date = 'June 2, 2020'
# [PT] attach label to mat2d obj when reading in file
#
#ver = '0.4' ; date = 'June 2, 2020'
# [PT] new multifile object
#    + checks that all files loading in to object match in terms of 
#      nmat, nroi, labels, etc.
#
ver = '0.41' ; date = 'June 8, 2020'
# [PT] moved matrix dims/ragged/square calc to afni_utils.py
#
# --------------------------------------------------------------------------

import sys, os, copy
from afnipy import afni_base as ab
from afnipy import afni_util as UTIL

# ---------------------------------------------------------------------------

# string of grid/netcc/amat file labels in line [0] (all the same...)
head0_str = "Number of network ROI"

# dictionary of grid/netcc/amat labels in line [1]
head1_dict = {
    'netcc' : "Number of netcc matrices",
    'grid'  : "Number of grid matrices",
    'amat'  : "Number of amat matrices",
}

head2_str = "WITH_ROI_LABELS"

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
        self.ext         = None  # file type
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
            self.set_ext_from_fname(file_inp)

        if col_strlabs :
            self.set_col_strlabs(col_strlabs)
        if row_strlabs :
            self.set_row_strlabs(row_strlabs)

        if col_intvals :
            self.set_col_intvals(col_intvals)
        if row_intvals :
            self.set_row_intvals(row_intvals)

    # -------------- methods -------------------        

    def set_ext_from_fname(self, FF ):
        """set file extension, trying to parse filename FF

        return name/ext str

        NB: 'amat' == 'AFNI matrix', which means the form is OK, but
        the type of netcc or grid specifically can't be identified.

        """
        
        ffsplit = FF.split('.')
        if len(ffsplit) < 2 :          return 'amat'

        if ffsplit[-1] == 'netcc'  :   return 'netcc'
        elif ffsplit[-1] == 'grid' :   return 'grid'
        else:                          return 'amat'

    def get_mat_min_max(self):
        '''calculate min and max values of entire array'''
        amin = []
        amax = []
        for rrr in self.mat:
            amin.append(min(rrr))
            amax.append(max(rrr))
        self.min_ele = min(amin)
        self.max_ele = max(amax)

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

        nrow, ncolmin, ncolmax, is_rag, is_sq \
            = UTIL.mat_row_mincol_maxcol_ragged_square(self.mat)

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
            for i in range(self.nrow):
                for j in range(self.ncol):
                    self.mat[i][j] = float(self.mat[i][j])
        elif ET == 'int' or ET == int :
            for i in range(self.nrow):
                for j in range(self.ncol):
                    self.mat[i][j] = int(self.mat[i][j])
        elif ET == 'bool' or ET == bool :
            for i in range(self.nrow):
                for j in range(self.ncol):
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

        #nc, nrmin, nrmax, is_rag, is_sq \
        #    = UTIL.mat_col_minrow_maxrow_ragged_square(self.mat)
        nr, ncmin, ncmax, is_rag, is_sq \
            = UTIL.mat_row_mincol_maxcol_ragged_square(self.mat)

        self.nrow = nr

        if is_rag :
            if allow_ragged :
                ab.WP("ragged matrix: min = {}, max = {}\n"
                      "-> am zeropadding".format(ncmin, ncmax))
                for ii in range(self.nrow):
                    diff = ncmax - len(self.mat[ii])
                    if diff :
                        for jj in range(diff):
                            self.mat[ii].append(0)
            else:
                ab.EP("ragged matrix: min = {}, max = {}\n"
                      "no flag to allow ragged mats".format(ncmin, ncmax))

        self.ncol = ncmax
        
        self.nelements = self.nrow * self.ncol

        ab.IP("mat dims (nrow, ncol): {}, {}".format(self.nrow, self.ncol))


# ------------------------------------------------------------------------

class file_grid_netcc:
    """
    Read in output from 3dNetCorr or from 3dTrackID.  

    Default mode is to "read" (mode='r') an input filename.

    """

    def __init__( self, fname='', mode='r', fout='' ):

        self.data        = []       # full file, read in 
        self.data_len    = 0        # nlines file file

        # these can be regenerated as data is updated/added/removed
        self.header      = []       # just the header part of data
        self.header_len  = 0        # number of lines in header
        self.table_mat     = []       # just the table of matrices part of data
        self.table_mat_len = 0        # number of lines in table_mat

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
        if fname and mode == 'r' :
            self.file_inp = fname
            self.open_and_check_GoN( )

    # -----------------------------------------------------------
    # -----------------------------------------------------------

    def set_file_inp(self, FF):
        self.file_inp = FF

    def add_mat2d(self, Mobj, idx_pos=None, ext=None ):
        """Mobj : is a mat2d object, which can have str labels.
        
        idx_pos : can be an index position (zerobase counting, of
                  course!) for inserting the current obj in the list
                  of things.  NB: again, it is the allmat_labs that
                  controls the order of mat2d obj.  Behaves as
                  list.insert(idx_pos, SOMETHING) would.

        This function adds to and updates the list of mat2d
        information in the current file_grid_netcc obj, as well as
        updating header and table (table_mat) info.

        """
        
        # the output here contains info about whether a file_* obj
        # exists already with mat2ds in it
        OK_TO_GO = self.check_new_mat2d_with_current( Mobj )
        
        # this will insert it at the end, if no position has been
        # specified
        if idx_pos == None :
            idx_pos = self.nmat

        self.allmat_labs.insert(idx_pos, Mobj.label)
        self.allmat[ Mobj.label ] =  Mobj
        self.nmat+= 1

        ab.IP("Inserted mat '{}' into index position [{}]"
              "".format(Mobj.label, idx_pos))

        if OK_TO_GO == 1 :

            self.nroi        = Mobj.ncol
            self.roi_intvals = copy.deepcopy(Mobj.col_intvals)

            if Mobj.col_strlabs :
                self.has_strlabs = True
                self.roi_strlabs = copy.deepcopy(Mobj.col_strlabs)

            if ext == None : 
                if Mobj.ext :
                    self.ext = Mobj.ext 
                else:
                    self.ext = 'amat'
            else:
                self.ext = ext

        # update table+header
        self.make_header_GoN()
        self.make_table_mat_GoN()

    def check_new_mat2d_with_current(self, Mobj):
        """Check about info that needs to match if we already have mats in
        this obj (e.g., nroi, roi_strlabs, roi_intvals, etc.)

        Output
        ------
        1      : if nothing exists
        2      : if things exist, and this new Mobj is OK to add
        [exit] : if things are NOT ok

        """

        # trivial case: nothing to check
        if self.nmat == 0:   return 1

        # check nroi|size; also, must be square
        if self.nroi != Mobj.nrow or self.nroi != Mobj.ncol :
            ab.EP("mismatch in size of current file obj (nroi= {}) "
                  "and new obj (row, col = {}, {})"
                  "".format(self.nroi, Mobj.nrow, Mobj.ncol))

        # intvals
        for ii in range(self.nroi) :
            if self.roi_intvals[ii] != Mobj.col_intvals[ii] :
                ab.EP("mismatch in intvals of current file obj:\n{}\n"
                      "and those of new obj:\n{}"
                      "".format(self.roi_intvals, Mobj.col_intvals))

        # check str labels
        if self.has_strlabs != Mobj.col_strlabs :
            ab.EP("mismatch in string labelling in current file obj ({}) "
                  "and new obj ({})"
                  "".format( self.has_strlabs, Mobj.col_strlabs ))

        # strlabs, if any
        if self.has_strlabs :
            for ii in range(self.nroi) :
                if self.roi_strlabs[ii] != Mobj.col_strlabs[ii] :
                    ab.EP("mismatch in strlabs of current file obj:\n{}\n"
                          "and those of new obj:\n{}"
                          "".format(self.roi_strlabs, Mobj.col_strlabs))

        # check if a mat2d with this label already exists --> confusion!
        if self.allmat_labs.__contains__(Mobj.label) :
            ab.EP("current file obj already contains that obj label: {}"
                  "".format(Mobj.label))

        # have we survived the gauntlet?
        return 2


    def make_header_GoN(self):
        """Form the header part for output.

        Can be netcc, grid or 'amat' (generalized form, 'AFNI matrix')

        Store internally
        """
        
        h = []

        # line-by-line labels to use; some are trivial copies
        h0_label = head0_str
        try:
            h1_label = head1_dict[self.ext]
        except:
            ab.WP("Can't recognize extension?")
            h1_label = head1_dict['amat']
        h2_label = head2_str

        h.append( "# {}  # {}".format(self.nroi, h0_label) )
        h.append( "# {}  # {}".format(self.nmat, h1_label) )
        if self.has_strlabs :
            h.append( "# {}".format( h2_label ) )
            h.append( ''.join([" {:>10s} \t".format(x) \
                               for x in self.roi_strlabs]) )
        h.append( ''.join([" {:10d} \t".format(x) \
                           for x in self.roi_intvals]) )
        #return h
        self.header     = h
        self.header_len = len(h)

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

        self.table_mat     = t
        self.table_mat_len = len(t)
        #return t
    

    def make_full_out_str_GoN(self):
        """Create full string version of grid/netcc/amat file to print or
        write.

        """

        # make sure these are uptodate
        self.make_header_GoN()
        self.make_table_mat_GoN()

        full_out = '\n'.join(self.header)
        full_out+= '\n'
        full_out+= '\n'.join(self.table_mat)

        return full_out


    def disp_table_mat(self):
        """Print current table of matrix (non-header) information to terminal

        """
        
        self.make_table_mat_GoN()
        out = '\n'.join(self.table_mat)
        print(out)

    def disp_hdr(self):
        """Print current header information to terminal

        """
        
        self.make_header_GoN()
        out = '\n'.join(self.header)
        print(out)


    def disp_full(self):
        """Print current information (all) to terminal

        """

        out = self.make_full_out_str_GoN()
        print(out)
       

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
        if line[-1] == head2_str :
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

    def read_table_mat_GoN(self):
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
        self.read_table_mat_GoN()

# --------------------------------------------------------------------------

class multi_file_GoN:
    """An obj to store a collection of grid/netcc/amat files.

    We will make a consistency check to ensure that they all have the same:
    + number of matrices
    + number of ROIs per matrix
    + same int vals per ROI
    + same str vals per ROI

    General usage: start empty and append items.

    Most info stored in dicts, with a master list.  Since we don't
    know if fname paths are annoyingly long, use integers as keys for
    dset and fname dictionaries, and a list with access to them.

    """

    def __init__( self ):

        self.all_file    = {}       # keys are just numbers, at the moment
        self.all_fname   = {}
        
        self.all_idx     = []       # this controls order and selection
        self.nfile       = 0


    def add_file(self, FF, fname='', idx_pos=None ):
        """
        Add a 'file_grid_netcc' obj.

        FF is an instance of the  'file_grid_netcc' class.

        Basically, won't use idx_pos property, most likely
        """

        if not(fname) :    fname       = FF.file_inp
        else:              FF.file_inp = fname

        if fname :         ab.IP("Adding {}".format(fname))
        else:              ab.IP("Adding unnamed file")

        # 1 if nothing prior, 2 if things exist and we are OK, exit if bad
        OK_TO_GO = self.check_new_file_with_current(FF)

        if idx_pos == None :
            idx_pos = self.nfile

        self.all_file[idx_pos]  = FF
        self.all_fname[idx_pos] = fname
        self.all_idx.append(idx_pos)

        self.nfile+= 1
        
    def check_new_file_with_current(self, FF):
        """Check about info that needs to match if we already have files in
        this obj

        Output
        ------
        1      : if nothing exists
        2      : if things exist, and this new file FF is OK to add
        [exit] : if things are NOT ok

        """

        # trivial case: nothing to check
        if self.nfile == 0:   return 1

        # since at this point we know there is at least one prior
        # file, use the [0]th one to compare for properties

        CC = self.all_file[0]  # get from dictionary

        # check nmat
        if CC.nmat != FF.nmat :
            ab.EP("mismatch in number of matrices in current file ({}) "
                  "and new file ({})"
                  "".format( CC.nroi, FF.nroi ))

        # check nroi/mat
        if CC.nroi != FF.nroi :
            ab.EP("mismatch in number of ROIs per mat in current file ({}) "
                  "and new file ({})"
                  "".format( CC.nroi, FF.nroi ))

        # check str labels
        if CC.has_strlabs != FF.has_strlabs :
            ab.EP("mismatch in string labelling in current file ({}) "
                  "and new file ({})"
                  "".format( CC.has_strlabs, FF.has_strlabs ))

        # check roi intvals
        for ii in range(CC.nroi):
            if CC.roi_intvals[ii] != FF.roi_intvals[ii] :
                ab.EP("mismatch in intvals of current existing file:\n{}\n"
                      "and those of new file:\n{}"
                      "".format(CC.roi_intvals, FF.roi_intvals))

        # check ROI strlabs, if any
        if CC.has_strlabs :
            for ii in range(CC.nroi) :
                if CC.roi_strlabs[ii] != FF.roi_strlabs[ii] :
                    ab.EP("mismatch in strlabs of existing file:\n{}\n"
                          "and those of new file:\n{}"
                          "".format(CC.roi_strlabs, FF.roi_strlabs))

        # check matrix names/types
        for ii in range(CC.nmat):
            if CC.allmat_labs[ii] != FF.allmat_labs[ii] :
                ab.EP("mismatch in matrix labels of existing file:\n{}\n"
                      "and those of new file:\n{}"
                      "".format(CC.allmat_labs, FF.allmat_labs))

        # check ext
        if CC.ext != FF.ext :
            ab.EP("mismatch in ext in current file ({}) "
                  "and new file ({})"
                  "".format( CC.ext, FF.ext ))

        # have we survived the gauntlet?
        return 2


    
