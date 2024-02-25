#!/usr/bin/env python

# This library contains functions for managing the outputs of
# compute_ROI_stats.tcsh.

import sys, os, copy
from   afnipy import afni_base as ab
from   afnipy import afni_util as au
from   afnipy import lib_apqc_html_css as lahc

# ============================================================================

# how to map warn levels to Q ratings here
warn2q = {
    'none'      : ' ',
    'undecided' : '?',
    'mild'      : '?',
    'medium'    : 'x',
    'severe'    : 'x',
}

# ============================================================================

class all_comp_roi_dset_table:
    """An object to accompany compute_ROI_stats.tcsh text file output.
This contains a set of tables."""

    def __init__(self, ftext, prefix='OUTFILE', fname='input_file', 
                 verb=0):
        """Take a full file text (list of strings) and loop over tables
        within."""

        self.verb              = verb            # verbosity level
        self.ftext             = ftext           # list of str, full file text
        self.prefix            = prefix          # str, output filename radix
        self.fname             = fname           # str, input filename

        # attributes defined by parsing self.ftext
        self.all_table         = []              # list of comp_roi_dset_* obj

        # ----- start doing work
        _tmp = is_comp_roi_str_list_ok(self.ftext)
        self.find_all_tables(self)


    # ---------------------------------------------------

    def find_all_tables(self):
        """Go through ftext and parcel out into separate tables."""
        
        # **************************
        #for ii in range(self.n_ftext):
        pass
    

    @property
    def n_ftext(self):
        """Number of lines in ftext list"""
        return len(self.ftext)

# ---------------------------------------------------------------------------

class comp_roi_dset_table:
    """An object to accompany compute_ROI_stats.tcsh text file output.
This contains a single dset's table.

The major input here is:
    table : list of strings from reading a text file created by 
            compute_ROI_stats.tcsh; just a single table, if that program
            put multiple ones into its text file.

Probably the major products of interest from creating this object are:
    self.table_values_q     : a copy of the raw input table that includes
                              Q-column ratings ('?', 'x', etc.)
    self.table_values_qhtml : a copy of the raw input table that includes
                              Q-column ratings ('?', 'x', etc.) *and*
                              HTML-style warning level coloration
"""

    def __init__(self, table, verb=0):
        """Take a dset table (list of strings) and go to work
        calculating/evaluating things.
        """

        self.verb              = verb            # verbosity level
        self.table             = table           # list of str, raw table text

        # attributes defined by parsing self.table
        self.table_top         = []              # list of top 3 header rows
        self.table_cols        = []              # list of column names
        self.table_coldash     = []              # list of col name dashes 
        self.table_values      = []              # list of Nroi str

        # attributes defined by evaluating table_values for warnings
        self.table_values_q     = []             # table_values plus Q warns
        self.table_values_qhtml = []             # table_values_q plus HTML col
        self.table_maxwarn      = []             # list of max warn per ROI
        # ... and there is a decorator for table_allq

        # ----- start doing work
        _tmp = is_comp_roi_str_list_ok(self.table)
        self.parse_input_table()
        self.evaluate_all_warns()

    # ---------------------------------------------------

    def evaluate_all_warns(self):
        """For each ROI (i.e., each element of table_values), go through all
        possible warn functions, and update: table_values_qhtml,
        table_values_q and table_maxwarn."""

        # each of these updates table_values_qhtml and table_maxwarn
        for idx in range(self.n_table_values):
            self.check_nz_frac(idx)
            self.check_vmax(idx)
            self.check_tsnr_slope_2575(idx)

        # put in Q values in table_values_q and table_values_qhtml
        self.update_q_in_tables()

    def update_q_in_tables(self):
        """For relevant table_values_* lists, update the Q column values."""

        allq = copy.deepcopy(self.table_allq)

        for idx in range(self.n_table_values):
            self.table_values_q[idx][0]     = allq[idx]
            self.table_values_qhtml[idx][0] = allq[idx]

    def check_tsnr_slope_2575(self, idx):
        """Run the check on TSNR slope in ROI."""

        # find columns with correct info
        all_col = [self.table_cols.index('Tmin'),
                   self.table_cols.index('T25%'),
                   self.table_cols.index('Tmed'),
                   self.table_cols.index('T75%'),
                   self.table_cols.index('Tmax')
        ]

        # get numbers from relevant row
        tlist = [int(self.table_values[idx][jj]) for jj in all_col]

        # evaluate warning level, and save
        wlev = warn_roi_stats_tsnr_slope_2575(tlist, verb=self.verb)
        self.table_maxwarn[idx] = maxwarn(wlev, self.table_maxwarn[idx])

        # ... and colorize in *qhtml* table, as needed (here, middle 3 of 5)
        for jj in all_col[1:4]:
            self.table_values_qhtml[idx][jj] = \
                wrap_val_with_html_span(self.table_values[idx][jj], wlev=wlev)

    def check_vmax(self, idx):
        """Run the check on max voxel depth in the ROI."""

        # find columns with correct info
        col_Vmax = self.table_cols.index('Vmax')

        # get numbers from relevant row
        Vmax = float(self.table_values[idx][col_Vmax])

        # evaluate warning level, and save
        wlev = warn_roi_stats_vmax(Vmax, verb=self.verb)
        self.table_maxwarn[idx] = maxwarn(wlev, self.table_maxwarn[idx])

        # ... and colorize in *qhtml* table, as needed
        self.table_values_qhtml[idx][col_Vmax] = \
            wrap_val_with_html_span(self.table_values[idx][col_Vmax], wlev=wlev)

    def check_nz_frac(self, idx):
        """Run the check on fraction of nonzero voxels in the ROI."""

        # find columns with correct info
        col_Nvox = self.table_cols.index('Nvox')
        col_Nz   = self.table_cols.index('Nz')

        # get numbers from relevant row
        Nvox = int(self.table_values[idx][col_Nvox])
        Nz   = int(self.table_values[idx][col_Nz])

        # evaluate warning level, and save
        wlev = warn_roi_stats_nz_frac(Nvox, Nz, verb=self.verb)
        self.table_maxwarn[idx] = maxwarn(wlev, self.table_maxwarn[idx])

        # ... and colorize in *qhtml* table, as needed
        self.table_values_qhtml[idx][col_Nz] = \
            wrap_val_with_html_span(self.table_values[idx][col_Nz], wlev=wlev)

    def parse_input_table(self):
        """Parse the input table, separating it into useful attributes"""
        
        # store the header part in pieces
        self.table_top      = copy.deepcopy(self.table[:3])
        self.table_cols     = break_line_at_whitespace(self.table[3])
        self.table_coldash  = copy.deepcopy(self.table[4])

        # store the table itself
        for i in range(5, self.n_table):
            x = self.table[i]
            y = break_line_at_whitespace(x)
            if len(y) == 0:
                ab.EP("Row {} in full table is empty?".format(i))
            # ensure row consistency with headers (bc Q val may or may
            # not be present)
            z = self.manage_q_col(y)
            # check that we have achieved consistency
            if len(z) != self.len_table_cols :
                ab.WP("[{}]th ROI has {} pieces, but col header has {}?"
                      "".format(iroi, len(z), self.len_table_cols))
                if self.verb :
                    print('row:', x)
            self.table_values.append(z)

        # init some evaluation things
        self.table_values_q     = copy.deepcopy(self.table_values)
        self.table_values_qhtml = copy.deepcopy(self.table_values)
        self.table_maxwarn      = ['none'] * self.n_table_values
        self.all_q              = [' '] * self.n_table_values

    def manage_q_col(self, y):
        """Make the table_values have a Q column entry in an appropriate
        way. If they have one already, do nothing. If they don't have
        one, then create a placeholder one, which is a ' ' carved out
        of the initial white space there. Here, y is a list of str,
        representing the full ROI row that has been broken up into
        words and whitespace chunks."""

        if y[0][0] != ' ' :
            # nothing to do in this case: the entry should be a
            # rating already.
            return copy.deepcopy(y)
        else:
            # no rating present; split [0]th block of len=N whitespace
            # into 2 blocks: one of len=1, and one of len=N-1
            N = len(y[0])
            z = copy.deepcopy(y)
            z[0] = ' '*(N-1)          # make y[0] shorter by 1 char
            z.insert(0, ' ')          # insert that space as new [0]th char
            return z

    @property
    def n_table(self):
        """Number of lines in raw input table"""
        return len(self.table)

    @property
    def n_table_top(self):
        """Number of lines in 'table top' part of header"""
        return len(self.table_top)

    @property
    def n_table_values(self):
        """Number of ROI rows in table"""
        return len(self.table_values)

    @property
    def len_table_cols(self):
        """Number of table columns (in broken format, that counts whitespace
        chunks)"""
        return len(self.table_cols)

    @property
    def table_allq(self):
        """For each ROI in table, what goes into Q column, based on max level
        of warning?"""
        if self.n_table_values == 0 :
            return []
        return [warn2q[x] for x in self.table_maxwarn]

# ----------------------------------------------------------------------------

def maxwarn(A, B):
    """For two warn level strings, A and B, return the max level. If A or
B is not an allowed warn level, produce an error.

Parameters
----------
A : str
    a warning level str from: 
      'none', 'undecided', 'mild', 'medium', 'severe'
B : str
    a warning level str (same list as above)

Returns
-------
C : str
    max warning level string between A and B

    """

    all_level = lahc.wlevel_ranks.keys()
    if not(A in all_level) :
        ab.EP("Input A ({}) is not in allowed list:\n"
              "{}".format(A, ', '.join(all_level)))
    if not(B in all_level) :
        ab.EP("Input A ({}) is not in allowed list:\n"
              "{}".format(B, ', '.join(all_level)))

    wA = lahc.wlevel_ranks[A]
    wB = lahc.wlevel_ranks[B]

    # return largest (in case of tie, can return either
    if wA > wB :  return A
    else:         return B


def warn_roi_stats_tsnr_slope_2575(tlist, verb=0):
    """For a given set of TSNR percentile values in tlist (= Tmin, T25%,
Tmed, T75%, Tmax), provide warning levels based on the ratio:
    rat = (T75% - T25%)/Tmed

Parameters
----------
tlist : list or order collection
    set of 5 TSNR values, as defined above
verb : int
    verbosity level

Returns
-------
wlevel : str
    string of warning level, from: 
      'none', 'undecided', 'mild', 'medium', 'severe'

    """

    ttt = ', '.join([str(x) for x in tlist])
    if verb :  ab.IP("TSNR values : ".format(ttt))

    N = len(tlist)
    if N != 5 :
        ab.EP("Should have exactly 5 TSNR values, not: {})".format(N))

    tmin = tlist[0]
    t25p = tlist[1]
    tmed = tlist[2]
    t75p = tlist[3]
    tmax = tlist[4]

    if tmax < 0 :
        ab.EP("Can't have negative max TSNR ({})".format(tmax))

    if tmax == 0 :      return 'undecided'

    rat = float(t75p - t25p)/tmed
    
    # not sure what these warn level ranges should be!
    if   rat > 2.0 : return 'severe'
    elif rat > 1.5 : return 'medium'
    elif rat > 1.0 : return 'mild'
    elif rat > 0.5 : return 'undecided'
    else:            return 'none'

def warn_roi_stats_nz_frac(nvox, nz, verb=0):
    """For a given number of voxels (nvox) and number of zero-valued
voxels (nz), determine warning level.

Parameters
----------
nvox : int
    number of voxels in ROI
nz : int
    number of zero-valued voxels in the ROI
verb : int
    verbosity level

Returns
-------
wlevel : str
    string of warning level, from: 
      'none', 'undecided', 'mild', 'medium', 'severe'
"""

    if verb :  ab.IP("nvox: {}, nz: {}".format(nvox, nz))

    if nvox < 0 or nz < 0 :
        ab.EP("Can't have negative nvox ({}) or nz ({})".format(nvox, nz))

    if nvox == 0 :      return 'undecided'
    elif nz == 0 :      return 'none'

    frac = float(nz) / nvox
    
    if   frac < 0.05 :  return 'undecided'
    elif frac < 0.1  :  return 'mild'
    elif frac < 0.3  :  return 'medium'
    else:               return 'severe'
    
def warn_roi_stats_vmax(vmax, verb=0):
    """Assign an appropriate warning level for the max volumetric depth
(vmax), which is the maximum depth in an ROI, counting in units of
(isotropic) voxel dimension.

Parameters
----------
vmax : float
    max voxel depth of ROI (units: number of voxels, which are likely
    isotropic)
verb : int
    verbosity level

Returns
-------
wlevel : str
    string of warning level, from: 
      'none', 'undecided', 'mild', 'medium', 'severe'

    """

    if verb :  ab.IP("vmax: {}".format(vmax))

    if   vmax < 0 :  
        ab.EP("Can't have negative vmax ({})".format(vmax))
    elif vmax and vmax < 1 :  
        ab.WP("Shouldn't have sub-unity vmax ({})".format(vmax))

    if   vmax == 0   :  return 'undecided'   # no vox in ROI
    elif vmax >= 2   :  return 'none'
    elif vmax >= 1.5 :  return 'mild'        # though, 1.4 is a common val
    else:               return 'medium'


# ============================================================================

def wrap_val_with_html_span(x, wlev='none'):
    """Take a column entry x, and either wrap it HTML-style in a
background color (if the warning level wlev is high enough), or return
it plainly as a string.  We use the same colormapping rules as main
APQC HTML (lahc.wlevel_colors); note that 'none' will be ignored,
though (no green, just leave plain).

Parameters
----------
x : str
    an entry in the table_values table to (possibly) be wrapped in HTML 
    coloration
wlev : str
    the warning level associated with x, which determines if and what 
    color will be wrapped

Returns
-------
sss : str
    the table entry either just returned back plainly (no change) or 
    wrapped in color

    """

    # use same colormapping rules as main APQC HTML
    # (lahc.wlevel_colors); note that 'none' will be ignored, though
    # (no green, just leave plain)

    # warning level not high enough to add color
    if wlev == 'none' :
        return """{}""".format(x)
    
    # warning level *is* high enough to add color
    wcol = lahc.wlevel_colors[wlev]
    sss = """<span style="background-color: {}">""".format(wcol)
    sss+= """{}</span>""".format(x)

    return sss

# ----------------------------------------------------------------------------

def break_line_at_whitespace(x, verb=0):

    """Take a string x and break it into a list of strings that are broken
up at whitespace boundaries. This is different than the str.split()
method, because it preserves all characters (even whitespace), but
chunks them in sequence into substrings.

Parameters
----------
x : str
    input string to be broken up
verb : int
    verbosity level

Returns
-------
y : list
    list of strings from x, broken up at whitespace boundaries. At the
    end, should have: ''.join(y) == x
"""

    # ----- simple checks

    xtype = type(x).__name__
    if xtype != 'str' :
        ab.EP("Input x must be of type str, not: {}".format(xtype))

    N = len(x)
    if N == 0 :
        return ['']
    if verb :
        ab.IP("Input string length: {} char".format(N))

    # ----- main work 

    all_word = x.split()           # list of non-whitespace words
    y        = []                  # init empty list for all str
    bot      = 0                   # init starting index of each search
    for word in all_word :
        # find next word, starting from end of last one
        top = x.find(word, bot)
        if bot != top :
            y.append(x[bot:top])
        y.append(word)
        # update starting index for next search
        bot = top + len(word)
    # get any whitespace that is after final word
    if bot != N :
        y.append(x[bot:])

    Ny = len(y)
    if verb :
        ab.IP("Output list has length: {} elements".format(Ny))

    # ----- check if we lost anything

    ystr = ''.join(y)
    if ystr != x :
        ab.WP("Reconstituted broken string y differs from x", y, x)
        if verb :
            print("   x: '{}'".format(x))
            print("   y: '{}'".format(ystr))

    return y

def is_comp_roi_str_list_ok(L):
    """Preliminary evaluation of the input table (which is a list of str),
    for fundamental/necessary properties."""
    
    # check type of input
    ttype = type(L).__name__
    if ttype != 'list' :
        ab.EP("Table must be a list, not: {}".format(ttype))

    # check minimum len of input
    N = len(L)
    if not(N) :
        ab.EP("Table is empty")
    elif N < 6 :
        ab.EP("Table must have at least 6 lines (to have >=1 ROI). "
              "This one has only: {}".format(N))

    # check input contents (list of str)
    for x in L:
        xtype = type(x).__name__
        if xtype != 'str' :
            ab.EP("Table must contain only str, not: {}".format(xtype))

    # [0]th row must start with 'dset:'
    lstart = L[0][:5]
    if lstart != 'dset:' :
        ab.EP("Table must start with 'dset:', not {}".format(lstart))

    # [3]rd row must start with 'Q'
    lstart = L[3][0]
    if lstart != 'Q' :
        ab.EP("Line [3] must start with 'Q', not {}".format(lstart))

    return 0


# ============================================================================

if __name__ == '__main__' :

    fname = '/home/ptaylor/AFNI_data6/FT_analysis/'
    fname+= 'sub-456.results_FT.rest.15/t.tsnr_stats_regress8/'
    fname+= 'stats_MNI-Glasser.txt'

    x = au.read_text_file(fname, strip=False)


    OBJ = comp_roi_dset_table(x)
