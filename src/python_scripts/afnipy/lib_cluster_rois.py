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

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au

# ============================================================================

# ----------------------------------------------------------------------------


# ============================================================================

class ClustRegionObj:
    """Object for gen_cluster_table.py

Parameters
----------
inobj : InOpts object 
    object constructed from running gen_cluster_table.py on the command
    line. At present, the only way to really provide inputs here.

    """

    def __init__(self, clust, table, labels, 
                 clust_dat=None, dat_column_as_sign=False,
                 min_fill_clust=0.0, min_fill_atlas=0.0, 
                 strict_fill_clust=True, olap_logic='and', 
                 verb=1 ):

        # ----- set up attributes

        # main input variables
        self.status            = 0                    # not used
        self.clust             = clust                # which cluster is it?
        self.table             = table                # number part of table
        self.labels            = labels               # list of labels
        self.min_fill_clust    = min_fill_clust       # clust olap thr frac
        self.min_fill_atlas    = min_fill_atlas       # atlas olap thr frac
        self.strict_fill_clust = strict_fill_clust    # apply clust thr
        self.olap_logic        = olap_logic           # how to combine fracs?

        # general variables
        self.verb              = verb

        self.dat_col_as_sign   = None                 # **** have to apply this

        # ----- take action(s)

        # prelim stuff
        print("HEY, nothing DOING YET")

    # ----- methods

    # **************

    # ----- decorators

    @property
    def ninput_clust(self):
        """number of input_clusts; should always be 1"""
        return len(self.input_clust)



# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

