#!/usr/bin/env python

# python3 status: compatible

# Objects and functionality for dealing with csv-related things.
#
# In particular, these funcs are for combining 3dNetCorr and 3dTrackID
# outputs with other subject data, form building tables for group
# analysis statistics files.

# --------------------------------------------------------------------------
#
# auth: 'PA Taylor'
#
#ver = '0.0' ; date = 'June 4, 2020'
# [PT] birth
#
ver = '0.1' ; date = 'June 5, 2020'
# [PT] can read in and populate CSV file
#    + guess about which cols are cat|quant vars
#
# --------------------------------------------------------------------------

import sys, os, copy
from afnipy import afni_base as ab
from afnipy import afni_util as UTIL

# ---------------------------------------------------------------------------

ddefs = {
    'DEF_delim'               : ',',
    'DEF_has_header'          : True, 
}

class csv_data:

    def __init__(self, fname = '', delimiter=None, has_header=None):

        self.fname         = ''                  # input filename
        self.delimiter     = ddefs['DEF_delim']
        self.fulltext_list = []                  # what gets read in

        self.has_header    = ddefs['DEF_has_header']  # basically, labels
        self.header        = []                  # basically, labels

        self.table         = []                  # data
        self.ncol_table    = 0                   # number of cols in table
        self.nrow_table    = 0                   # number of rows in table

        self.var_type      = []                  # guess if var is quant|cat

        # ------------------------------------

        if delimiter != None :
            self.delimiter = delimiter
        if has_header != None :
            self.has_header = bool(has_header)

        if fname :
            self.read_csv(fname)

    def guess_variable_type(self):
        """Based on the values in the table, try to categorize each column
        (header entry) as representing either a quantitative or
        categorical variable.

        Do this by testing each variable value in the table for a
        column: if all values can be turned into a float (-1.0, 1, 25,
        0.00001, etc.), then that variable is guessed to be 'quant';
        otherwise, it will be 'cat'.

        We note that some 'quantitative' measures are actually
        categories, just encoded with numbers.  Hopefully, the user
        has encoded things well...

        """
        
        N       = self.ncol_table
        Nrow    = self.nrow_table
        guesses = []

        for ii in range(N): # for each col
            numcount = 0
            for jj in range(Nrow):
                if ab.isFloat(self.table[jj][ii]) :
                    numcount+=1

            if numcount == Nrow :
                guesses.append('quant')
            else:
                guesses.append('cat')
        
        self.var_type = copy.deepcopy(guesses)


    def parse_fulltext_list(self):
        """
        Split up the fulltext that is now a list of strings
        """
        
        N = len(self.fulltext_list)
        table_start = int(bool(self.has_header))

        if self.has_header:
            hh = [x.strip() for \
                  x in self.fulltext_list[0].split(self.delimiter)]

        tt  = []
        ltt = []
        for ii in range(table_start, N):
            line = [x.strip() for \
                    x in self.fulltext_list[ii].split(self.delimiter)]
            tt.append(line)
            ltt.append(len(line))

        # check lengths for consistency:  no raggedness
        if min(ltt) != max(ltt) :
            ab.EP("Looks like a ragged CSV, with min|max length = {}|{}"
                  "".format(min(ltt), max(ltt)))
        if self.has_header:
            if min(ltt) != max(ltt) :
                ab.EP("Looks like a mismatched CSV, with header length = {}\n"
                      "and table length  = {}"
                      "".format(max(ltt), len(hh)))
                
        # OK to set these values in the obj now
        self.header = copy.deepcopy(hh)
        self.table  = copy.deepcopy(tt)        

        self.nrow_table = len(self.table)
        if self.nrow_table :
            self.ncol_table = len(self.table[0])

        # try to categorize each var
        self.guess_variable_type()

    def read_csv(self, fname=None, delimiter=None, has_header=None):
        """Open a csv file to read in, and populate items.

        """

        if fname :               self.fname=fname
        if delimiter != None :   self.delimiter=delimiter
        if has_header != None :  self.has_header = bool(has_header)

        self.fulltext_list = []
        fff = open(self.fname, 'r')
        for lll in fff.readlines():
            self.fulltext_list.append(lll)
        fff.close()

        self.parse_fulltext_list()

    def get_table_col_by_idx(self, idx) :
        """Get a whole column of data from the table by providing its index.

        """

        out = []
        for ii in range(self.nrow_table):
            out.append(self.table[ii][idx])

        return out
    
    def get_table_col_by_header(self, hname) :
        """Get a whole column of data from the table by providing its header
        column name.
        
        """

        idx = self.header.index(hname)
        out = []
        for ii in range(self.nrow_table):
            out.append(self.table[ii][idx])

        return out
