#!/usr/bin/env python

# A library of functions for dealing with gen_cluster_table.py
# Specifically, this file contains the supplementary object for
# managing the mini-table of overlaps per cluster
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : library for gen_cluster.py
# ============================================================================

import sys, os, copy, glob
import math
from   operator import itemgetter

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au

# ============================================================================

# parameters for combining threshold conditions
LIST_valid_olap = ['or', 'and']
STR_valid_olap  = ', '.join(LIST_valid_olap)

# ----------------------------------------------------------------------------

# what columns will be reported in table by default
LIST_clust_report_hdr = [
    "Clust",                     # int, cluster value
    "Nvox",                      # int, cluster size as num of voxels
    "Clust_perc",                # float, percent filled of cluster by ROI
    "Atlas_perc",                # float, percent filled of ROI by cluster
    "Atlas_region",              # str, atlas region name/label
    ]

# ... and what are extras that can be added to clust_report_hdr (crh),
# including the index it would be inserted as

# float (or str), mean of data (or label)
crh_extra_dat = [ "Clust_dat" , 2 ]

# ============================================================================

class ClustRegionObj:
    """Object for gen_cluster_table.py

Parameters
----------
clust : int
    value of cluster
table : list 
    list of lists representation of int+float value part of table created
    by adjunct_aw_tableize_roi_info.py (the ROI labels are input separately
    as the this arg: labels)
labels : list
    list of atlas/ROI names (must have same len as table)
min_fill_clust : float
    threshold for filling cluster; value in range [0.0, 1.0] to apply
    to column [5] ("RelVol_A2B") in table
min_fill_atlas : float
    threshold for filling atlas region; value in range [0.0, 1.0] to apply
    to column [7] ("Frac_B") in table

    """

    def __init__(self, clust, table, labels, 
                 min_fill_clust=0.0, min_fill_atlas=0.0, 
                 strict_fill_clust=True, olap_logic='or',
                 clust_dat=None, dat_col_as_sign=False,
                 verb=1 ):

        # ----- set up attributes

        # main input variables
        self.status            = 0                    # not used
        self.clust             = clust                # which cluster is it?
        self.table             = table                # number part of table
        self.labels            = labels               # list of labels

        # main input control params
        self.min_fill_clust    = min_fill_clust       # clust olap thr frac
        self.min_fill_atlas    = min_fill_atlas       # atlas olap thr frac
        self.strict_fill_clust = strict_fill_clust    # apply clust thr
        self.olap_logic        = olap_logic           # how to combine fracs?

        self.clust_dat         = clust_dat            # (extra) mean val of dat
        self.dat_col_as_sign   = dat_col_as_sign      # how to apply dat mean?

        self.verb              = verb

        # derived items
        self.clust_report_hdr  = []
        self.clust_report      = []

        # ----- take action(s)

        # prelim stuff
        tmp1 = self.check_inputs()
        if tmp1 : return

        tmp2 = self.setup_report()
        if tmp2 : return

        tmp3 = self.process_table()
        if tmp3 : return

    # ----- methods

    def process_table(self):
        """Process the input table of values.

        In a single loop, we find all suprathreshold overlaps.  If
        strict_fill_clust is false, we also keep track of the greatest
        subthreshold cluster fraction filler, in case no regions are
        above threshold, and then we will include that.

        """

        BAD_RETURN = -3

        # initialize some things to help with filling 

        prelim_list  = []             # ones that pass automatically
        
        # in case no olaps are above thr, and strict_fill_clust=False
        max_fill_sub     = []         # max subthr filler, just in case
        max_fill_sub_val = -1         # what is val of max Clust_fill

        for ii in range(self.len_table):
            row = self.table[ii]

            # Nvox_A col: if this is 0, nothing to do, don't even add
            if row[1] :

                # check+combine clust fill cond

                cond_clust = row[6] >= self.min_fill_clust   # Frac_A
                cond_atlas = row[5] >= self.min_fill_atlas   # RelVol_A2B

                if self.olap_logic == 'and' :
                    cond_total = cond_clust * cond_atlas
                elif self.olap_logic == 'or' :
                    cond_total = cond_clust + cond_atlas
                else:
                    msg = "Unknown olap_logic. Should never reach here!"
                    ab.EP1(msg)
                    return BAD_RETURN

                # make row in all cases (whether above thr or not); it
                # might get either added directly (if suprathr) or
                # potentially (if subthr, and needs to be included)

                L = [
                    self.clust,      # "Clust",        
                    row[1],          # "Nvox",         
                    100.0*row[6],    # "Clust_perc",   -> Frac_A
                    100.0*row[5],    # "Atlas_perc",   -> RelVol_A2B
                    self.labels[ii], # "Atlas_region", 
                ]
                
                # if user asked, include appropriate clust_dat info
                if self.clust_dat is not None :
                    # if using data, record as expon notation
                    rep_dat = '{:.3e}'.format(self.clust_dat)

                    # if using string label, get appropriate one
                    if self.dat_col_as_sign :
                        if self.clust_dat > 0 :
                            rep_dat = 'pos'
                        elif self.clust_dat < 0 :
                            rep_dat = 'neg'
                        else:
                            rep_dat = 'zero'

                    L.insert(rep_dat, crh_extra_dat[1])

                # figure out how/where to save L

                if cond_total :
                    # is suprathr, so store
                    prelim_list.append(L)
                elif not(self.strict_fill_clust) :
                    # is subthr, but is new clust_fill frac champion,
                    # so hold onto it
                    if row[5] > max_fill_sub_val :
                        max_fill_sub_val = row[5]
                        max_fill_sub = copy.deepcopy(L)

        # done processing all rows; we now figure out what to keep,
        # and return a list-of-lists sorted by the clust_fill
        # "column"; final results saved to clust_report

        if not(len(prelim_list)) :
            if not(self.strict_fill_clust) :
                if len(max_fill_sub) :
                    self.clust_report = [max_fill_sub]
                else:
                    self.clust_report = []
        else:
            # get index to sort by
            idx = self.clust_report_hdr.index('Clust_perc')
            prelim_list.sort(key=itemgetter(idx))
            self.clust_report = prelim_list

        is_fail = disp_cluster_table(self.clust_report, self.clust_report_hdr)


        return 0

    def setup_report(self):
        """Establish what header fields will be filled. Most are
        pre-determined, and this is mainly about checking if extra
        clust_dat has been input, and if so, whether to report the
        mean or just the sign."""

        BAD_RETURN = -2

        self.clust_report_hdr = copy.deepcopy(LIST_clust_report_hdr)
        
        if self.clust_dat is not None :
            try:
                self.clust_report_hdr.insert(crh_extra_dat[0],
                                             crh_extra_dat[1])
            except:
                return BAD_RETURN

        return 0

    def check_inputs(self):
        """Do some sanity checks on inputs"""

        BAD_RETURN = -1

        # verify lengths of table and labels: must match
        if self.len_table != self.nlabels :
            msg = "mismatch between table length ({}) ".format(self.len_table)
            msg+= "and number of region labels  ({})".format(self.nlabels)
            ab.EP1(msg)
            return BAD_RETURN

        # valid cluster fill frac
        if self.min_fill_clust < 0.0 or self.min_fill_clust > 1.0 :
            msg = "Min fill cluster must be in interval [0.0, 1.0] "
            msg+= "and given '{}' is outside that".format(self.min_fill_clust)
            ab.EP1(msg)
            return BAD_RETURN
        
        # valid atlas fill frac
        if self.min_fill_atlas < 0.0 or self.min_fill_atlas > 1.0 :
            msg = "Min fill atlaser must be in interval [0.0, 1.0] "
            msg+= "and given '{}' is outside that".format(self.min_fill_atlas)
            ab.EP1(msg)
            return BAD_RETURN

        # allowed keyword for combining threhsold conditions
        if not(self.olap_logic in LIST_valid_olap) :
            msg = "Invalid olap logic operator '{}'. ".format(self.olap_logic)
            mst+= "Must be one of: {}".format(STR_valid_olap)
            ab.EP1(msg)
            return BAD_RETURN

        return 0

    # ----- decorators

    @property
    def len_table(self):
        """number of rows in table"""
        return len(self.table)

    @property
    def nlabels(self):
        """number of labels"""
        return len(self.labels)

def disp_cluster_table(X, hdr, disp_hdr=True):
    """Display a table of cluster values X, with header/column titles hdr.

Parameters
----------
X : list
    list of lists, where each row is one overlap region to report
hdr : list
    list of column title strings (must be same len as X[0], etc.)
disp_hdr : bool
    should the hdr be displayed?

Returns
-------
is_fail : bool
    0 for success, nonzero for error
"""

    BAD_RETURN = -12

    ncol = len(hdr)
    nrow = len(X)

    if not(nrow) :
        return 0

    if len(X[0]) != ncol :
        msg = "Length of row is '{}', ".format(len(X[0]))
        msg+= "which does not match len of hdr ('{}')".format(nrow)
        ab.EP1(msg)
        return BAD_RETURN

    # make a list of lines of txt
    otxt = []
                           
    # make title row
    
    if disp_hdr :
        title = """{:>6s}  """.format(hdr[0])
        title+= """{:>6s}  """.format(hdr[1])
        if ncol == len(LIST_clust_report_hdr) :
            idx = 2
        else:
            title+= """{:<9s}  """.format(hdr[2])
            idx = 3
        title+= """{:10s}  """.format(hdr[idx])
        title+= """{:10s}  """.format(hdr[idx+1])
        title+= """{:<s}""".format(hdr[idx+2])
        otxt.append(title)
    
    # add text for each row

    for ii in range(nrow):
        row = X[ii]
        ttt = """{:>6d}  """.format(row[0])
        ttt+= """{:>6d}  """.format(row[1])
        if ncol == len(LIST_clust_report_hdr) :
            idx = 2
        else:
            ttt+= """{:<9s}  """.format(row[2])
            idx = 3
        ttt+= """{:10.1f}  """.format(row[idx])
        ttt+= """{:10.1f}  """.format(row[idx+1])
        ttt+= """{:<s}""".format(row[idx+2])
        otxt.append(ttt)

    for jj in range(len(otxt)):
        print(otxt[jj])

    return 0

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")
