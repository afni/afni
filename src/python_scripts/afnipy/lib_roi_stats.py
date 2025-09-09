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

DEF_had_blur = True
DEF_verb = 1

# ============================================================================

class all_comp_roi_dset_table:
    """An object to accompany compute_ROI_stats.tcsh text file output.
This contains a *set* of one or more tables."""

    def __init__(self, ftext, prefix='', fname='input_file', 
                 write_out=True, disp_max_warn=False,
                 had_blur=DEF_had_blur, verb=0):
        """Take a full file text (list of strings) and loop over tables
        within."""

        self.verb              = verb            # verbosity level
        self.ftext             = ftext           # list of str, full file text
        self.prefix            = prefix          # str, output filename radix
        self.fname             = fname           # str, input filename
        self.write_out         = write_out       # bool, make output file?
        self.disp_max_warn     = disp_max_warn   # bool, show max warn str?
        self.had_blur          = had_blur        # bool, levels for unblurred

        # attributes defined by parsing self.ftext
        self.all_tables_raw    = []              # list of comp_roi_dset* text
        self.all_tables_eval   = []              # list of comp_roi_dset_* obj

        # ----- start doing work
        ### [PT] note to self: wd have to adjust this to be more
        ### flexible about whitespace/empty lines
        # _tmp = is_comp_roi_str_list_ok(self.ftext)
        self.find_all_tables()
        self.evaluate_all_tables()

        if self.write_out :
            self.write_out_table_file(self.prefix)

    # ---------------------------------------------------

    def write_out_table_file(self, prefix):
        """Save to disk the processed text files: table_values_html."""

        # default postfixes for particular kinds of tables, if prefix 
        # is empty or None
        dict_tables = {
            'values_html' : '_eval_html.txt',
        }

        for table in dict_tables.keys() :
            if prefix :
                opref = prefix
            elif '.' in self.fname :
                ppp   = '.'.join(self.fname.split('.')[:-1])
                opref = ppp + dict_tables[table]
            elif len(self.fname) :
                opref = self.fname + dict_tables[table]
            else:
                opref = 'prefix' + dict_tables[table]
            otext = '\n' # empty line at top

            # for each given table type, loop over all tables in the file
            for ii in range(self.n_tables):
                # insert empty line after any existing table
                if ii :    otext+= '\n'

                iitab  = self.all_tables_eval[ii]
                otext+= iitab.assemble_table_values(table)
            otext+= '\n'

            fff = open( opref, "w" )
            fff.write( otext )
            fff.close()

    def evaluate_all_tables(self):
        """ Run the quality evaluations for each table """

        for table_raw in self.all_tables_raw:
            OBJ = comp_roi_dset_table(table_raw,
                                      disp_max_warn = self.disp_max_warn,
                                      had_blur=self.had_blur,
                                      verb=self.verb)
            self.all_tables_eval.append(OBJ)

    def find_all_tables(self):
        """Go through ftext and parcel out into separate tables."""
        
        # bc is_comp_roi_str_list_ok() has been run, we know we can go
        # 6 lines in and then look for the first empty line break (or
        # end of file) to determine where the table ends
        start = 0
        while start < self.n_ftext and len(self.ftext[start].split()) == 0 :
            start+=1
        ii = start + 6
        while ii < self.n_ftext :
            if len(self.ftext[ii].split()) == 0 :
                # found a break between/at end of tables
                self.all_tables_raw.append(copy.deepcopy(self.ftext[start:ii]))
                # look for additional whitespace-only lines
                while ii < self.n_ftext and len(self.ftext[ii].split()) == 0 :
                    ii+=1
                start = ii
                # jump across possible table header height
                ii+= 6
            else:
                ii+= 1
        if start < self.n_ftext and ii != start :
            # must have found another table
            self.all_tables_raw.append(copy.deepcopy(self.ftext[start:ii]))

        ab.IP("Found {} tables".format(self.n_tables))

        # verify tables
        for table in self.all_tables_raw:
            _tmp = is_comp_roi_str_list_ok(table)

    @property
    def n_ftext(self):
        """Number of lines in ftext list"""
        return len(self.ftext)

    @property
    def n_tables(self):
        """Number of tables in all_tables_raw"""
        return len(self.all_tables_raw)

# ---------------------------------------------------------------------------

class comp_roi_dset_table:
    """An object to accompany compute_ROI_stats.tcsh text file output.
This contains a single dset's table.

The major input here is:
    table : list of strings from reading a text file created by 
            compute_ROI_stats.tcsh; just a single table, if that program
            put multiple ones into its text file.

Probably the major products of interest from creating this object are:
    self.table_values_html : a copy of the raw input table that includes
                             HTML-style warning level coloration
"""

    def __init__(self, table, disp_max_warn=False, had_blur=DEF_had_blur,
                 verb=0):
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
        self.table_values_html  = []             # table_values + HTML colors
        self.table_maxwarn      = []             # list of max warn per item
        self.disp_max_warn      = disp_max_warn  # do display max warning lev?
        self.had_blur            = had_blur        # data had no blurring?

        # ----- start doing work
        _tmp = is_comp_roi_str_list_ok(self.table)
        self.parse_input_table()
        self.init_eval_tables()
        self.evaluate_all_warns()
        self.apply_warns_to_values_html()
        if self.disp_max_warn :
            self.do_display_max_warn()
        
    # ---------------------------------------------------

    def assemble_table_values(self, table=None):
        """Combine the header+text for table_values*, such as for writing to
        a file; allowed table values are:
           'values', 'values_html'."""
        
        # allowed keyword list for 'table'
        tlist = ['values', 'values_html']

        if table == None :
            ab.WP("No table specified for assembling? Choose from one of:"
                  "{}".format(', '.join(tlist)))
            return ''

        otext = ''
        otext+= ''.join(self.table_top)
        otext+= ''.join(self.table_cols)
        otext+= ''.join(self.table_coldash)
        if table == 'values' :
            otext+= ''.join([''.join(x) for x in self.table_values])
        elif table == 'values_html' :
            otext+= ''.join([''.join(x) for x in self.table_values_html])
        else:
            ab.WP("No table specified for assembling? Choose from one of:"
                  "{}".format(', '.join(tlist)))
            return ''

        return otext

    def do_display_max_warn(self):
        """Get the maximum warning level across the table, and output it as
        text"""

        max_warn     = 0
        max_warn_str = ''
        for idx in range(self.n_table_values):
            for col in range(self.len_table_cols):
                wlev = lahc.wlevel_ranks[ self.table_maxwarn[idx][col] ]
                if wlev > max_warn :
                    max_warn     = wlev
                    max_warn_str = self.table_maxwarn[idx][col]
        ab.IP("max warn level : {}".format(max_warn_str))

    def apply_warns_to_values_html(self):
        """Go through all warns, and add HTML wrappers in appropriate
        places for table_values_html."""

        for idx in range(self.n_table_values):
            for col in range(self.len_table_cols):
                wlev = self.table_maxwarn[idx][col]
                self.table_values_html[idx][col] = \
                    wrap_val_with_html_span(self.table_values[idx][col], wlev=wlev)

    def evaluate_all_warns(self):
        """For each ROI (i.e., each element of table_values), go through all
        possible warn functions, and update: table_values_html and
        table_maxwarn."""

        if self.verb :
            print("++ option note, had_blur = {}".format(self.had_blur))


        # each of these updates table_values_html and table_maxwarn
        for idx in range(self.n_table_values):
            self.check_nvox(idx)
            self.check_nzer_frac(idx)
            self.check_dvox(idx)
            self.check_tsnr_slope_2575(idx)
            self.check_tsnr_value_75(idx)      # do after TSNR-slope check

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

        # ... and update warning levels in cols (here, middle 3 of 5)
        for jj in all_col[1:4]:
            self.update_table_maxwarn(wlev, idx, jj)
            #wrap_val_with_html_span(self.table_values[idx][jj], wlev=wlev)

    def check_tsnr_value_75(self, idx):
        """Run the check on TSNR 75th %ile (instead of checking the max),
        for its absolute magnitude."""

        # find columns with correct info
        col_T75p = self.table_cols.index('T75%')

        # get numbers from relevant row
        T75p = float(self.table_values[idx][col_T75p])

        # evaluate warning level, and save
        wlev = warn_roi_stats_tsnr_value_75(T75p, 
                                            had_blur=self.had_blur,
                                            verb=self.verb)

        # ... and update warning levels in cols
        self.update_table_maxwarn(wlev, idx, col_T75p)

    def check_dvox(self, idx):
        """Run the check on max voxel depth in the ROI."""

        # find columns with correct info
        col_Dvox = self.table_cols.index('Dvox')

        # get numbers from relevant row
        Dvox = float(self.table_values[idx][col_Dvox])

        # evaluate warning level, and save
        wlev = warn_roi_stats_dvox(Dvox, verb=self.verb)

        # ... and update warning levels in cols
        self.update_table_maxwarn(wlev, idx, col_Dvox)

    def check_nzer_frac(self, idx):
        """Run the check on fraction of nonzero voxels in the ROI."""

        # find columns with correct info
        col_Nvox = self.table_cols.index('Nvox')
        col_Nzer = self.table_cols.index('Nzer')

        # get numbers from relevant row
        Nvox = int(self.table_values[idx][col_Nvox])
        Nzer = int(self.table_values[idx][col_Nzer])

        # evaluate warning level, and save
        wlev = warn_roi_stats_nzer_frac(Nvox, Nzer, verb=self.verb)

        # ... and update warning levels in cols
        self.update_table_maxwarn(wlev, idx, col_Nzer)

    def check_nvox(self, idx):
        """Run the check on total number of voxels in the ROI."""

        # find columns with correct info
        col_Nvox = self.table_cols.index('Nvox')

        # get numbers from relevant row
        Nvox = int(self.table_values[idx][col_Nvox])

        # evaluate warning level, and save
        wlev = warn_roi_stats_nvox(Nvox, verb=self.verb)

        # ... and update warning levels in cols
        self.update_table_maxwarn(wlev, idx, col_Nvox)

    def update_table_maxwarn(self, wlev, idx, col):
        """For a given idx (row in table) and col (column number), update
        the warning level with wlev; that is, record the max warning level
        between what is there already and wlev."""

        # get old warn level value
        wlev_old = self.table_maxwarn[idx][col]
        # ... and update it with wlev, if the latter is larger
        self.table_maxwarn[idx][col] = maxwarn(wlev, wlev_old)

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
            ###z = self.manage_q_col(y)   # not used currently

            # check that we have achieved consistency
            if len(y) != self.len_table_cols :
                ab.WP("[{}]th ROI has {} pieces, but col header has {}?"
                      "".format(i, len(y), self.len_table_cols))
                if self.verb :
                    print('row:', x)
            self.table_values.append(y)

    def init_eval_tables(self):
        """Initialize some evaluation things"""

        self.table_values_html  = copy.deepcopy(self.table_values)
        
        # store max warning for each column in table---even white
        # space ones, sillily enough (well, for bookkeeping)
        row = ['none'] * self.len_table_cols
        self.table_maxwarn = [copy.deepcopy(row) for n in range(self.n_table_values)]

    def manage_q_col(self, y):
        """Make the table_values have a Q column entry in an appropriate
        way. If they have one already, do nothing. If they don't have
        one, then create a placeholder one, which is a ' ' carved out
        of the initial white space there. Here, y is a list of str,
        representing the full ROI row that has been broken up into
        words and whitespace chunks. **NOT USED CURRENTLY**"""

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

As the ratio gets larger, the warning level increases.

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
    if verb > 1 :  ab.IP("TSNR values : {}".format(ttt))

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

def warn_roi_stats_tsnr_value_75(t75p, had_blur=DEF_had_blur, verb=0):
    """For a given value of the 75%ile of TSNR in the ROI, determine
warning level. This increasingly warns as this TSNR value decreases.

The value of had_blur determines the levels of warning: if had_blur,
then the levels are lowered to about 2/3rds or 70 percent of what they
would be if blurring had been applied during processing.

Parameters
----------
t75p : int/float
    75th %ile value of TSNR in the ROI
had_blur : bool
    was blurring used in processing?
verb : int
    verbosity level

Returns
-------
wlevel : str
    string of warning level, from: 
      'none', 'undecided', 'mild', 'medium', 'severe'

    """

    if verb > 1 :  ab.IP("t75p: {}".format(t75p))

    if t75p < 0:
        ab.EP("Can't have negative t75p ({})".format(t75p))

    if t75p == 0 :     return 'none'   # no vox; gets flagged elsewhere

    if had_blur :
        if   t75p >=  70 :  return 'none'
        elif t75p >=  55 :  return 'mild'
        elif t75p >=  35 :  return 'medium'
        else:               return 'severe'
    else:
        if   t75p >= 100 :  return 'none'
        elif t75p >=  80 :  return 'mild'
        elif t75p >=  50 :  return 'medium'
        else:               return 'severe'

def warn_roi_stats_nvox(nvox, verb=0):
    """For a given number of voxels (nvox), determine warning level. This
increasingly warns as the absolute number of voxels an ROI drops.

Parameters
----------
nvox : int
    number of voxels in ROI
verb : int
    verbosity level

Returns
-------
wlevel : str
    string of warning level, from: 
      'none', 'undecided', 'mild', 'medium', 'severe'

    """

    if verb > 1 :  ab.IP("nvox: {}".format(nvox))

    if nvox < 0:
        ab.EP("Can't have negative nvox ({})".format(nvox))

    if nvox == 0 :      return 'none'   # no vox; gets flagged elsewhere
    
    if   nvox > 15  :  return 'none'
    elif nvox > 9   :  return 'mild'
    elif nvox > 4   :  return 'medium'
    else:              return 'severe'
    
def warn_roi_stats_nzer_frac(nvox, nzer, verb=0):
    """For a given number of voxels (nvox) and number of zero-valued
voxels (nzer), determine warning level for: frac = nzer/nvox. This warns
increasingly as the ROI gets filled with more empty voxels.

Parameters
----------
nvox : int
    number of voxels in ROI
nzer : int
    number of zero-valued voxels in the ROI
verb : int
    verbosity level

Returns
-------
wlevel : str
    string of warning level, from: 
      'none', 'undecided', 'mild', 'medium', 'severe'

    """

    if verb > 1 :  ab.IP("nvox: {}, nzer: {}".format(nvox, nzer))

    if nvox < 0 or nzer < 0 :
        ab.EP("Can't have negative nvox ({}) or nzer ({})".format(nvox, nzer))

    if nvox == 0 :      return 'none'   # no vox; gets flagged elsewhere
    elif nzer == 0 :    return 'none'

    frac = float(nzer) / nvox
    
    if   frac < 0.05 :  return 'none'
    elif frac < 0.1  :  return 'undecided'
    elif frac < 0.2  :  return 'mild'
    elif frac < 0.35 :  return 'medium'
    else:               return 'severe'
    
def warn_roi_stats_dvox(dvox, verb=0):
    """Assign an appropriate warning level for the max volumetric depth
(dvox) of the ROI, counting in units of (isotropic) voxel dimension.
As the dvox decreases, the warning level increases. Lower dvox
suggests greater influence of partial voluming, for example.

Parameters
----------
dvox : float
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

    if verb > 1 :  ab.IP("dvox: {}".format(dvox))

    if   dvox < 0 :  
        ab.EP("Can't have negative dvox ({})".format(dvox))
    elif dvox and dvox < 1 :  
        ab.WP("Shouldn't have sub-unity dvox ({})".format(dvox))

    if   dvox == 0   :  return 'none'        # no vox; gets flagged elsewhere
    elif dvox >= 1.7 :  return 'none'
    elif dvox >= 1.4 :  return 'mild'        # though, 1.4 is a common val
    else:               return 'medium'


# ============================================================================

def wrap_val_with_html_span(x, wlev='none', style='background'):
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
style : str
    a keyword for the kind of coloration to add; valid args are:
      'background', 'border', 'underline'

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
    if style == 'background' :
        sss = """<span style="background-color: {}">""".format(wcol)
        sss+= """{}</span>""".format(x)
    elif style == 'border' :
        # this does not work well, shifts things
        sss = """<span style="border-width:3px; border-style:solid; border-color: {}">""".format(wcol)
        sss+= """{}</span>""".format(x)
    elif style == 'underline' :
        sss = """<u style="text-decoration-offset: 1px; text-decoration-thickness: 3px; text-decoration-color: {}">""".format(wcol)
        sss+= """{}</u>""".format(x)

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
    for fundamental/necessary properties. Basically, fail if something
    looks wrong and just return zero if we successfully run the gamut
    of properties.

    """
    
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

    # [3]rd row must have 'R' (for 'ROI') as first non-whitespace char
    lstart = L[3].split()[0][0]
    if lstart != 'R' :
        ab.EP("Line [3]'s first non-whitespace char must be 'R', "
              "not {}".format(lstart))

    return 0

# ============================================================================

if __name__ == '__main__' :

    #fname = '/home/ptaylor/AFNI_data6/FT_analysis/'
    #fname+= 'sub-456.results_FT.rest.15/t.tsnr_stats_regress8/'
    #fname+= 'stats_MNI-Glasser.txt'
    #
    #x = au.read_text_file(fname, strip=False)
    #
    #OBJ = comp_roi_dset_table(x)

    '''
    fname2 = '/home/ptaylor/AFNI_data6/FT_analysis/'
    fname2+= 'sub-456.results_FT.rest.16/tsnr_stats_regress/'
    fname2+= 'stats_COMBO9.txt'
    prefout = '.'.join(fname2.split('.')[:-1]) # use input file for output pref

    x2 = au.read_text_file(fname2, strip=False)

    OBJ2 = all_comp_roi_dset_table(x2, prefix = prefout)
'''

    fname2 = '/home/ptaylor/AFNI_data6/FT_analysis/FT.results/tsnr_stats_regress/'
    fname2+= 'stats_APQC_atlas.txt'
    prefout = '.'.join(fname2.split('.')[:-1]) # use input file for output pref

    x2 = au.read_text_file(fname2, strip=False)

    OBJ2 = all_comp_roi_dset_table(x2, prefix = prefout)
