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
    nvol   = retobj.vol_nv                    # ni_dimen, nrow data
    nslice = retobj.n_slice_times             # how many slices
    nreg   = 0                                # num of regressors per slice

    # build up count of number of regressors
    for label in lpf.PO_all_label :
        if retobj.have_label(label) :
            phobj = retobj.data[label]        # simplify coding below
            nreg+= phobj.n_regress_rvt
            nreg+= phobj.n_regress_phys
            
    ntype = nreg * nslice                     # ni_type quantity, ncol data

    # to avoid possibly making label/regressors inconsistent later, we
    # will add all numbers to one big array to output, and one big
    # header list. That should still be fine, memory-wise, for foreseeable
    # applications at present
    data_shape = (nvol, ntype)
    data_arr   = np.zeros(data_shape, dtype=float)
    data_lab   = ['LABEL'] * ntype

    # make the filename
    fname = 'slibase.1D'
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # the order of columns will be:
    # + for each slice in MRI volume
    #   + for each label list in PO_all_label (in order)
    #     - check for all RVT
    #     - check for all phys

    # build set of regressors, slice by slice
    for ss in range(nslice):
        slab = 's{:03d}'.format(ss)   # zeropadded

        # count number of regressors per slice, as added
        rcount = 0 
 
        # RVT regressors: add label+data
        for label in lpf.PO_all_label :
            if retobj.have_label(label) :
                phobj = retobj.data[label]        # simplify coding below
                # process any/all RVT regressors
                for ii in range(phobj.n_regress_rvt):
                    key = phobj.regress_rvt_keys[ii]
                    title = slab + '.' + key      # column header title

                    # go to column, and add info (RVT = const across slice)
                    cc = ss*nreg + rcount
                    data_lab[cc] = title
                    data_arr[:,cc] = phobj.regress_dict_rvt[key]
                    rcount+= 1

        # physio regressors: add label+data (these *are* slicewise)
        for label in lpf.PO_all_label :
            if retobj.have_label(label) :
                phobj = retobj.data[label]        # simplify coding below
                # process any/all phys regressors
                for ii in range(phobj.n_regress_phys):
                    keyA = phobj.regress_rvt_phys[ii]
                    keyB = phobj.regress_dict_phys[keyA][ss][0]
                    
                    title = keyB + '.' + keyA     # column header title

                    # go to column, and add info (RVT = const across slice)
                    cc = ss*nreg + rcount
                    data_lab[cc] = title
                    data_arr[:,cc] = phobj.regress_dict_phys[keyA][ss][1]
                    rcount+= 1

    # --------------------- write -------------------------------

    # open the file and write the header/start
    fff = open(fname, 'w')
    fff.write('# RetroTSout\n')
    fff.write('# ni_type = "{}*double"\n'.format(ntype))
    fff.write('# ni_dimen = "{}"\n'.format(nvol))
    fff.write('# ColumnLabels = " ')

    # write labels
    fff.write(' ; '.join(data_lab)+' "\n')
    fff.write('# >\n')

    # write data
    for ii in range(data_shape[0]):
        for jj in range(data_shape[1]):
            fff.write(" {:6.4f} ".format(data_arr[ii,jj]))
        fff.write('\n')

    # close out the NIML
    fff.write('# </RetroTSout>\n')

    # le fin: close and finish
    fff.close()

    print("++ Wrote regressor file: {}".format(fname))

    return 0
