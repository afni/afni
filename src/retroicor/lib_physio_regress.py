#!/usr/bin/env python

import sys
import os
import json
import gzip
import copy
import numpy          as np
from   afnipy import  lib_physio_funcs as lpf

# ==========================================================================

def write_regressor_file(retobj):
    """


"""

    # copy names for some convenience (do NOT alter!)
    verb   = retobj.verb
    prefix = retobj.prefix
    odir   = retobj.out_dir
    nvol   = retobj.vol_nv                    # ni_dimen
    nslice = retobj.n_slice_times             # how many slices
    nreg   = 0                                # num of regressors per slice

    # build up count of number of regressors
    for label in lpf.PO_all_label :
        if retobj.have_label(label) :
            nreg+= retobj.data[label].n_regress_phys
            nreg+= retobj.data[label].n_regress_rvt
            
    ntype = nreg * nslice                     # ni_type quantity

    # make the filename
    fname = 'slibase.1D'
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    fff = open(fname, 'w')
    fff.write('# RetroTSout\n')
    fff.write('# ni_type = "{}*double"\n'.format(ntype))
    fff.write('# ni_dimen = "{}"\n'.format(nvol))
    fff.write('# ColumnLabels = " ')
    # !!!! ADD CONTENT
    fff.close()

    print("++ Wrote regressor file: {}".format(fname))

    return 0
