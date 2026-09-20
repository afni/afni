#!/usr/bin/env python

import sys
import os
import json
import gzip
import copy
import numpy          as np
from   afnipy import  lib_physio_funcs as lpf

# ==========================================================================

def write_regressor_file_OLD(pcobj):
    """This is the older version of writing regressors out.  As in the
style of RetroTS.py, here all regressors are made slicewise (even RVT)
and output to a single slicebase file.

This is deprecated in favor of the separate functions with a similar
name with suffixes of *_sli() and *_vol(), for respectively writing
out the slice- and volume-based ones sepraately.

These now include any of the following regressors that could/should be
volume-based: rvt, rvtrrf, hrcrf.

    """

    # copy names for some convenience (do NOT alter!)
    verb   = pcobj.verb
    prefix = pcobj.prefix
    odir   = pcobj.out_dir
    nvol   = pcobj.vol_nv                    # ni_dimen, nrow data
    nslice = pcobj.n_slice_times             # how many slices

    # make the filename
    suffix = 'physio_regress_old_slibase'
    fname  = suffix + '.1D'
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # ----- build up list of regressors and labels

    # to avoid possibly making label/regressors inconsistent later, we
    # will add all numbers to one big array to output, and one big
    # header list. That should still be fine, memory-wise, for foreseeable
    # applications at present
    nreg      = 0
    data_lab  = []  # hold all labels
    data_list = []  # hold all regressors (each a 1-D array)

    # build set of regressors, slice by slice
    for ss in range(nslice):
        slab = 's{:03d}'.format(ss)   # zeropadded

        # ----- add any resp regressors

        label = "resp"
        if pcobj.have_label(label) :
            tsobj = pcobj.data[label]        # simplify coding below
            if pcobj.do_out_rvt :
                for ii in range(tsobj.n_regress_rvt):
                    key = tsobj.regress_rvt_keys[ii]
                    dat = tsobj.regress_dict_rvt[key]
                    title = slab + '.' + key      # column header title
                    data_lab.append( title )
                    data_list.append( np.array(dat, dtype=float) )
                    nreg+= 1
            if pcobj.do_out_rvtrrf :
                for ii in range(tsobj.n_regress_rvtrrf):
                    key = tsobj.regress_rvtrrf_keys[ii]
                    dat = tsobj.regress_dict_rvtrrf[key]
                    title = slab + '.' + key      # column header title
                    data_lab.append( title )
                    data_list.append( np.array(dat, dtype=float) )
                    nreg+= 1
        # *** add more types here, as we are able to include them


        # ----- add any card regressors

        label = "card"
        if pcobj.have_label(label) :
            tsobj = pcobj.data[label]        # simplify coding below
            if pcobj.do_out_hrcrf :
                for ii in range(tsobj.n_regress_hrcrf):
                    key = tsobj.regress_hrcrf_keys[ii]
                    dat = tsobj.regress_dict_hrcrf[key]
                    title = slab + '.' + key      # column header title
                    data_lab.append( title )
                    data_list.append( np.array(dat, dtype=float) )
                    nreg+= 1
        # *** add more types here, as we are able to include them


        # ----- add any retroicor phys regressors (these *are* slicewise)
        
        # these can be either card or resp
        for label in lpf.PO_all_label :
            if pcobj.have_label(label) and pcobj.do_out_phys[label] : 
                tsobj = pcobj.data[label]        # simplify coding below
                for ii in range(tsobj.n_regress_retro):
                    keyA = tsobj.regress_retro_keys[ii]
                    keyB = tsobj.regress_dict_retro[keyA][ss][0]
                    dat  = tsobj.regress_dict_retro[keyA][ss][1]
                    title = keyB + '.' + keyA      # column header title
                    data_lab.append( title )
                    data_list.append( np.array(dat, dtype=float) )
                    nreg+= 1
        # *** add more types here, as we are able to include them

    # check if we have a regressor to output
    if nreg == 0 :
        print("+* NO (old) slicewise regressor to output: ", fname)
        return 0

    print("++ {} (old form) slicewise regressors, each with {} timepoints"
          "".format(nreg, nvol))

    # --------------------- write -------------------------------

    # open the file and write the header/start
    fff = open(fname, 'w')
    fff.write('# physio_calc_out_{}\n'.format(suffix))
    fff.write('# ni_type = "{}*double"\n'.format(nreg)) # no sep ntype here
    fff.write('# ni_dimen = "{}"\n'.format(nvol))
    fff.write('# ColumnLabels = " ')

    # write labels
    fff.write(' ; '.join(data_lab)+' "\n')
    fff.write('# >\n')

    # write data
    for ii in range(nvol):
        for jj in range(nreg):
            fff.write(" {:>7.4f} ".format(data_list[jj][ii]))
        fff.write('\n')

    # close out the NIML
    fff.write('# </physio_calc_out_{}\n'.format(suffix))

    # le fin: close and finish
    fff.close()

    print("++ Wrote (old form) slicewise regressor file: {}".format(fname))

    return 0

# ----------------------------------------------------------------------------

def write_regressor_file_sli(pcobj):
    """Write out all the slicewise regressors to a single text file. Right
now, this would be just the RETROICOR regressors, and *not* RVT,
RVTRRF, or others.

"""

    # copy names for some convenience (do NOT alter!)
    verb   = pcobj.verb
    prefix = pcobj.prefix
    odir   = pcobj.out_dir
    nvol   = pcobj.vol_nv                    # ni_dimen, nrow data
    nslice = pcobj.n_slice_times             # how many slices
    nreg   = 0                                # num of regressors per slice

    # make the filename
    suffix = 'physio_regress_slice'
    fname  = suffix + '.1D'
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # build up count of number of regressors
    for label in lpf.PO_all_label :
        if pcobj.have_label(label) and pcobj.do_out_phys[label] :
            tsobj = pcobj.data[label]        # simplify coding below
            nreg+= tsobj.n_regress_retro
            
    # check if we have a regressor to output
    if nreg == 0 :
        print("+* NO slicewise regressor to output: ", fname)
        return 0

    ntype = nreg * nslice                     # ni_type quantity, ncol data

    # to avoid possibly making label/regressors inconsistent later, we
    # will add all numbers to one big array to output, and one big
    # header list. That should still be fine, memory-wise, for foreseeable
    # applications at present
    data_shape = (nvol, ntype)
    data_arr   = np.zeros(data_shape, dtype=float)
    data_lab   = ['LABEL'] * ntype

    # the order of columns will be:
    # + for each slice in MRI volume
    #   + for each label list in PO_all_label (in order)
    #     - check for all phys

    # build set of regressors, slice by slice
    for ss in range(nslice):
        slab = 's{:03d}'.format(ss)   # zeropadded

        # count number of regressors per slice, as added
        rcount = 0 
 
        # physio regressors: add label+data (these *are* slicewise)
        for label in lpf.PO_all_label :
            if pcobj.have_label(label) and pcobj.do_out_phys[label] :
                tsobj = pcobj.data[label]        # simplify coding below
                # process any/all phys regressors
                for ii in range(tsobj.n_regress_retro):
                    keyA = tsobj.regress_retro_keys[ii]
                    keyB = tsobj.regress_dict_retro[keyA][ss][0]
                    
                    title = keyB + '.' + keyA     # column header title

                    # go to column, and add info
                    cc = ss*nreg + rcount
                    data_lab[cc] = title
                    data_arr[:,cc] = tsobj.regress_dict_retro[keyA][ss][1]
                    rcount+= 1

    # --------------------- write -------------------------------

    # open the file and write the header/start
    fff = open(fname, 'w')
    fff.write('# physio_calc_{}\n'.format(suffix))
    fff.write('# ni_type = "{}*double"\n'.format(ntype))
    fff.write('# ni_dimen = "{}"\n'.format(nvol))
    fff.write('# ColumnLabels = " ')

    # write labels
    fff.write(' ; '.join(data_lab)+' "\n')
    fff.write('# >\n')

    # write data
    for ii in range(data_shape[0]):
        for jj in range(data_shape[1]):
            fff.write(" {:>7.4f} ".format(data_arr[ii,jj]))
        fff.write('\n')

    # close out the NIML
    fff.write('# </physio_calc_{}\n'.format(suffix))

    # le fin: close and finish
    fff.close()

    print("++ Wrote slicewise regressor file  : {}".format(fname))

    return 0

# ----------------------------------------------------------------------------

def write_regressor_file_vol(pcobj):
    """Write out all the volumetric regressors to a single text
file. Right now, this would *not* include the RETROICOR regressors,
but all things like RVT, RVTRRF, etc.

The writing style of this relates to its origins as a creator of
slicewise regressors; basically, we just have nslice=1.

    """

    # copy names for some convenience (do NOT alter!)
    verb   = pcobj.verb
    prefix = pcobj.prefix
    odir   = pcobj.out_dir
    nvol   = pcobj.vol_nv                    # ni_dimen, nrow data

    # make the filename
    suffix = 'physio_regress_volume'
    fname  = suffix + '.1D'
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # ----- build up list of regressors and labels

    # to avoid possibly making label/regressors inconsistent later, we
    # will add all numbers to one big array to output, and one big
    # header list. That should still be fine, memory-wise, for foreseeable
    # applications at present
    nreg      = 0
    data_lab  = []  # hold all labels
    data_list = []  # hold all regressors (each a 1-D array)

    # check all resp...
    label = "resp"
    if pcobj.have_label(label) :
        tsobj = pcobj.data[label]        # simplify coding below
        if pcobj.do_out_rvt :
            for ii in range(tsobj.n_regress_rvt):
                key = tsobj.regress_rvt_keys[ii]
                dat = tsobj.regress_dict_rvt[key]
                title = label + '.' + key      # column header title
                data_lab.append( title )
                data_list.append( np.array(dat, dtype=float) )
                nreg+= 1
        if pcobj.do_out_rvtrrf :
            for ii in range(tsobj.n_regress_rvtrrf):
                key = tsobj.regress_rvtrrf_keys[ii]
                dat = tsobj.regress_dict_rvtrrf[key]
                title = label + '.' + key      # column header title
                data_lab.append( title )
                data_list.append( np.array(dat, dtype=float) )
                nreg+= 1
        # *** add more types here, as we are able to include them

    # ... and check all card
    label = "card"
    if pcobj.have_label(label) :
        tsobj = pcobj.data[label]        # simplify coding below
        if pcobj.do_out_hrcrf :
            for ii in range(tsobj.n_regress_hrcrf):
                key = tsobj.regress_hrcrf_keys[ii]
                dat = tsobj.regress_dict_hrcrf[key]
                title = label + '.' + key      # column header title
                data_lab.append( title )
                data_list.append( np.array(dat, dtype=float) )
                nreg+= 1
        # *** add more types here, as we are able to include them

    # check if we have a regressor to output
    if nreg == 0 :
        print("+* NO volumetric regressor to output: ", fname)
        return 0

    print("++ {} volbase regressors, each with {} timepoints"
          "".format(nreg, nvol))

    # --------------------- write -------------------------------

    # open the file and write the header/start
    fff = open(fname, 'w')
    fff.write('# physio_calc_{}\n'.format(suffix))
    fff.write('# ni_type = "{}*double"\n'.format(nreg)) # no sep ntype here
    fff.write('# ni_dimen = "{}"\n'.format(nvol))
    fff.write('# ColumnLabels = " ')

    # write labels
    fff.write(' ; '.join(data_lab)+' "\n')
    fff.write('# >\n')

    # write data
    for ii in range(nvol):
        for jj in range(nreg):
            fff.write(" {:>7.4f} ".format(data_list[jj][ii]))
        fff.write('\n')

    # close out the NIML
    fff.write('# </physio_calc_{}\n'.format(suffix))

    # le fin: close and finish
    fff.close()

    print("++ Wrote volumetric regressor file : {}".format(fname))

    return 0
