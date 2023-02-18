#!/usr/bin/env python

import sys
import os
import json
import gzip
import copy
import numpy          as np
import lib_retro_opts as lro

# ==========================================================================

class retro_obj:
    """An object for starting the retroicor process for making physio
regressors for MRI data.

    """

    def __init__(self, verb=0):
        """Create object holding all retro info

        """
        
        # physio data
        self.resp_data  = None         # obj for resp data
        self.card_data  = None         # obj for card data
        # Q: add in RVT obj?

        # maybe not keep these in this obj?
        self.phys_jdict = None         # dict from JSON file, maybe col labels
        self.phys_file  = None         # phys file, might contain cardio/resp

        self.exit_on_rag = True        # exit if raggedness in data files?
        self.exit_on_nan = True        # exit if NaN values in data files?
        self.exit_on_null = True       # exit if null values in data files?
        self.exit_on_zero = False      # exit if zero values in data files?

        # physio info
        self.phys_freq   = None        # float, physio samp freq (in Hz)
        self.phys_samp   = None        # float, = 1/phys_freq (in s)
        self.start_time  = None        # float, time offset from start of MRI

        # MRI EPI info
        self.slice_times  = []         # list of floats for slice timing
        self.volume_tr    = None       # float, TR of MRI EPI
        self.num_time_pts = None       # int, Nvol MRI EPI

        # I/O info
        self.verb         = verb       # int, verbosity level
        self.out_dir      = None       # str, name of output dir
        self.prefix       = None       # str, prefix of output filenames
        self.show_graph_lev = 0        # int, amount of graphs to show
        self.save_graph_lev = 1        # int, amount of graphs to save
        self.font_size    = 10         # float, FS for output images
        self.niml         = False      # bool, use niml in output ***
        self.demo         = False      # bool, show demo?
        self.dev          = False      # bool, run in development mode?
        self.debug        = False      # bool, do debugging?
        self.no_rvt_out   = False      # bool, flag
        self.no_card_out  = False      # bool, flag
        self.no_resp_out  = False      # bool, flag

        # TBD
        self.fir_order    = None       # int, FIR order
        # phase offset, aby and abt

# ==========================================================================

def float_or_nan(x):
    """Take a number x and return float(x) if possible, or a numpy.nan if
it ain't.  Leading and trailing whitespace within the string should
not have any effect.

Parameters
----------
x : str
    possibly floatable str

Returns
-------
xfl : float
    a floatized version of x, either float(x) or NAN

    """

    try:
        return float(x)
    except:
        return np.nan

def float_or_nan_or_whitespace(x):
    """Take a number x and return '' if it is empty or only whitespace,
then float(x) if possible, or a numpy.nan if it ain't either.  Leading and
trailing whitespace within the string should not have any effect.


Parameters
----------
x : str
    possibly floatable str

Returns
-------
xfl : float or null str ('')
    a floatized version of x, either float(x) or NAN, or ''

    """

    y = x.strip()

    if not(y) :
        return ''

    try:
        return float(y)
    except:
        return np.nan

def check_raggedness_of_lines(L, verb=0):
    """For a given list of line lengths L (= simple list of integers),
determine and report on the raggedness of the lines it summarizes.

Parameters
----------
L : list 
    list of integer values, here corresponding to the line lengths of
    an input array
verb : int
    verbosity level whilst processing

Returns
-------
dict_of_len : dict
    dictionary of the list of lengths

    """

    # flag any raggedness, ignoring bad_nullist items
    maxlen = max(L)
    dict_of_len = {}
    for ii in range(1, maxlen+1):
        # keep track of how many row length varieties exist
        nnn = L.count(ii)
        if nnn :
            dict_of_len[ii] = nnn
    ndlen = len(dict_of_len)

    if verb and ndlen==1 :
        print("++ No apparent raggedness in the data columns")

    if ndlen > 1 :
        print("+* WARN: apparent raggedness in the file")

        # make parallel arrays of keys (each line length) and values
        # (number of lines for a given length), sorted in descending
        # order of values
        akeys = np.zeros(ndlen, dtype=int)
        avals = np.zeros(ndlen, dtype=int)
        idx   = 0 
        for k, v in dict_of_len.items():
            akeys[idx] = k
            avals[idx] = v
            idx+=1
        sortmap = np.argsort(avals) # map of how to sort akeys and avals

        # printout in reversed order
        for nn in range(ndlen):
            ii = sortmap[ndlen-1-nn]
            k = akeys[ii]
            v = avals[ii]

            if not(nn):
                ext = " (the majority)"
            else:
                all_ind = list(np.where(L == k)[0] + 1)
                ind_str = ", ".join([str(ind) for ind in all_ind])
                ext     = ", in lines: {}".format(ind_str)
            print("   {:6d} rows have {} items{}".format(v, k, ext))

    return dict_of_len

def sep_for_file(fname):
    """Rules to decide what char/str to use as a within-line separator for
a particular data file (mainly from its extension), such as CSV,
*.csv.gz, TSV, etc.

Parameters
----------
fname : str
    full name of file (path not needed, but OK); file need not exist

Returns
-------
sep : str
    string that will be used to separate items within each row.

    """

    # decide what to use as a within-line separator
    fname_low = fname.lower()
    if fname_low.endswith('.tsv') or fname_low.endswith('.tsv.gz') :
        sep = '\t'
    elif fname_low.endswith('.csv') or fname_low.endswith('.csv.gz') :
        sep = ','
    else:
        sep = None    # whitespace

    return sep


def read_column_text_to_float_list(fname, verb=1):
    """Read in a text file fname that is supposed to be one or more
columns of numbers.  The file fname will often be TSV or CSV (and can
be zipped), and split appropriately; other extensions will be split at
whitespace.

Identifying and dealing the potential NaN values, such as lines with
alphabetic text or which are empty, are the greatest challenge here.

If a row is empty or space-only, or if an element within a row is
empty or space-only (e.g., if a CSV has only whitespace between two
commas),
-> put a null str '' there, and add to bad_nulllist

If an element within a row cannot be converted with float(x),
-> put a np.nan there, and add to bad_nanlist

Additionally, raggedness is checked for in the file
-> recorded in dict_of_len

** NB: if any of the following is true for the outputs:
     len(bad_nulllist) > 0
     len(bad_nanlist)  > 0
     len(dict_of_len)  > 1
   ... then there is some kind of badness in the input file

If empty and/or whitespace-only rows occur at the end of fname, that
is not considered a warning/error.

Parameters
----------
fname : str
    text file, assumed to be columnar
verb : int
    verbosity level whilst processing

Returns
-------
tlist : list
    list form of text file, all numbers converted to floats (including
    NaNs) or empty strings (''s).  Each element in tlist is a list of
    floats and ''s.  The idea is ''s will be flagged to be dealt with
    later, not just left as they are!
bad_nanlist : list (of lists)
    list of NaNs occuring; each element of nanlist is a sublist, with
    elements:
      [0] zerobase index of row with the NaN
      [1] the full string of the NaN-causing row
bad_nulllist : list (of lists)
    list of ''s occuring; each element of nulllist is a sublist, with
    elements:
      [0] zerobase index of row which led to ''
      [1] the full string of the ''-causing row
dict_of_len : dict
    a dictionary summarizing the lengths of each line in fname.  Each
    key is a length of line occuring in the file, and each value is
    how many times that line length occurred.

    """

    BAD_RETURN = {}

    if verb:
        print("++ Start reporting this file:\n   {}".format(fname))

    if not(os.path.isfile(fname)) :
        print("** ERROR: cannot read file:\n   {}".format(fname))
        return BAD_RETURN

    # fname can be either zipped or unzipped
    if fname.endswith('.gz'):    opener = gzip.open 
    else:                        opener = open

    # decide what to use as a within-line separator
    sep = sep_for_file(fname)

    last_nonempty_row = -1          # used to trim empty rows at end
    idx   = 0
    tlist = []                      # total list, of lists or ''
    bad_nanlist  = []               # bad list: record nans
    bad_nulllist = []               # bad list: null/empty/missing items
    all_lenlist  = []               # count items per row
    with opener(fname, 'rt') as fff:
        for row in fff:
            # split row based on type of file
            rowsp     = row.split(sep)
            len_rowsp = len(rowsp)

            if len_rowsp == 0 :  
                # when sep=None and a row is empty/whitespace
                bad_nulllist.append([idx, row])
                all_lenlist.append( 0 )
                tlist.append('')
            elif len_rowsp == 1 :
                # single item per line
                xfl = float_or_nan_or_whitespace(rowsp[0])
                if '' == xfl :
                    bad_nulllist.append([idx, row])
                    all_lenlist.append( 0 )
                elif np.isnan(xfl) :
                    bad_nanlist.append([idx, row])
                    all_lenlist.append( 1 )
                    last_nonempty_row = idx
                else:
                    all_lenlist.append( 1 )
                    last_nonempty_row = idx 
                tlist.append([xfl])
            else:
                # multicolumn
                xfllist = [float_or_nan_or_whitespace(x) for x in rowsp]
                # one row can be in multiple bad lists
                if '' in xfllist :
                    bad_nulllist.append([idx, row])
                if np.sum([np.isnan(x) for x in xfllist \
                           if isinstance(x, float)]) :
                    bad_nanlist.append([idx, row])
                all_lenlist.append( len_rowsp )
                tlist.append(xfllist)
                last_nonempty_row = idx
            idx+=1 

    # clean up stuff

    # count number of empty/whitespace-only rows at end
    Nempty_end = idx - last_nonempty_row - 1
    if verb and Nempty_end:
        print("+* NB: there were {} empty/whitespace-only rows at the "
              "end of the file".format(Nempty_end))

    # if empty rows are simply at end of file, don't treat as bad and
    # remove from list (doing so from back of list)
    Nnull = len(bad_nulllist)
    for ii in range(Nnull-1, -1, -1):
        if bad_nulllist[ii][0] > last_nonempty_row :
            tmp = bad_nulllist.pop(ii)
    Nnull = len(bad_nulllist)

    # remove empty end rows from these lists, too
    tlist       = copy.deepcopy(tlist[:last_nonempty_row+1])
    all_lenlist = copy.deepcopy(all_lenlist[:last_nonempty_row+1])

    # flag any raggedness, ignoring bad_nullist items
    dict_of_len = check_raggedness_of_lines(all_lenlist, verb=verb)

    # COMMENT:
    # at this point there are 2 kinds of badness marked in the tlist: 
    #   ''     : where a row or element value was '' or whitespace
    #            (and see bad_nulllist)
    #   np.nan : otherwise, where float(x) would cause an error
    #            (and see bad_nanlist)
    # additionally, if len(dict_of_len)>1, then there is badness in
    # the form of raggedness in the input file.

    if verb:    print("++ End reporting.")

    return tlist, bad_nanlist, bad_nulllist, dict_of_len
