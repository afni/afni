import sys, os
import copy
import numpy               as np
from   afnipy import afni_base as BASE


# ===========================================================================

def interp_extrema_LIN(phobj, all_ext, verb=0):
    """Linearly interpolate a new layer for the fine time grid data in
phobj.ts_orig, using extrema indices listed in all_ext.  Essentially,
we are making an envelope around the time series phobj.ts_orig, at
that resolution.  Points in phobj.ts_orig that are not between any
pair of extrema retain their original value (that is, they are
self-enveloping).

Parameters
----------
phobj : phys_ts_obj
    object with physio time series information
all_ext : np.ndarray
    1D array of indices of the extrema to interpolate; that is, likely
    either a collection of peaks or troughs

Returns
-------
new_layer : np.ndarray
    the interpolated points, likely either an upper or lower envelope
    for the phobj.ts_orig data (and has same length as that time
    series, too)

    """

    Nts  = phobj.n_ts_orig
    Next = len(all_ext)

    # initialize output time series
    new_layer = np.zeros(Nts, dtype=float)

    # fill in between extrema
    for ii in range(Next-1):
        start = all_ext[ii]
        end   = all_ext[ii+1]
        diff  = end - start
        vstart = phobj.ts_orig[start]
        slope  = (phobj.ts_orig[end] - vstart)/diff
        # loop over this inter-extrema interval
        for jj in range(start, end):
            new_layer[jj] = vstart + (jj-start)*slope

    # outside of extrema, use constant padding with closest extremum
    for ii in range(all_ext[0]):
        new_layer[ii] = phobj.ts_orig[all_ext[0]]
    for ii in range(all_ext[-1], Nts):
        new_layer[ii] = phobj.ts_orig[all_ext[-1]-1]

    return new_layer

def interp_intervals_LIN(phobj, all_ext, verb=0):
    """Linearly interpolate the intervals of the indices listed in
all_ext.  Essentially, we are estimating a "rolling period" for the
time series phobj.ts_orig, at that resolution.  

Output units are physical period duration (in sec).

Time points that are not between any pair of extrema retain a constant
value, from either the first or last time point.

Parameters
----------
phobj : phys_ts_obj
    object with physio time series information
all_ext : np.ndarray
    1D array of indices of the extrema to interpolate; that is, likely
    either a collection of peaks or troughs

Returns
-------
y : np.ndarray
    the interpolated points, the rolling period for the phobj.ts_orig
    data (and has same length as that time series, too)

    """

    Nts  = phobj.n_ts_orig
    Next = len(all_ext)

    # init of output
    y  = np.zeros(Nts, dtype=float)
    Ny = len(y)

    # get intervals (in terms of indices)
    intervals = [j-i for i, j in zip(all_ext[:-1], all_ext[1:])]

    # store mid-extrema locations
    all_midext = [(j+i)//2 for i, j in zip(all_ext[:-1], all_ext[1:])]

    # same length of intervals and midpeaks
    Nival = len(intervals)   
    
    # go through all time points between midpeaks start and end
    for ii in range(Nival-1):
        start = all_midext[ii]
        end   = all_midext[ii+1]
        diff  = end - start
        # values to interp come from intervals
        vstart = intervals[ii]
        slope  = (intervals[ii+1] - vstart)/diff
        for jj in range(start, end):
            y[jj] = vstart + (jj-start)*slope

    # and for early/late points, just use initial/last values
    for jj in range(all_midext[0]):
        y[jj] = y[all_midext[0]]
    for jj in range(all_midext[-1], Ny):
        y[jj] = y[all_midext[-1]-1]

    # finally, apply units, so output period 
    y*= phobj.samp_rate

    return y

# ---------------------------------------------------------------------------
# dump a temp text file and plot RVT, if being used

def plot_rvt(retobj, label, ext='svg'):
    """


"""


    # the specific card/resp/etc. obj we use here (NB: not copying
    # obj, just dual-labelling for simplifying function calls while
    # still updating peaks info, at end)
    phobj  = retobj.data[label]
    odir   = retobj.out_dir
    prefix = retobj.prefix
    nvol   = retobj.vol_nv
    verb   = retobj.verb
    nrvt   = phobj.n_regress_rvt
    
    # make the filename (final image)
    fname = label + '_rvt_regressors.{}'.format(ext)
    if prefix  :  fname = prefix + '_' + fname
    if odir :     fname = odir + '/' + fname

    # make the data file (temporary file)
    ftmp = '__tmp' + label + '_rvt_regressors.dat'
    if prefix  :  ftmp = prefix + '_' + ftmp
    if odir :     ftmp = odir + '/' + ftmp

    title = 'Process {} data: RVT regressors'.format(label)

    # put data+labels into simple forms for writing; initialize objs
    data_shape = (nvol, nrvt)
    data_arr   = np.zeros(data_shape, dtype=float)
    data_lab   = ['LABEL'] * nrvt

    # process any/all RVT regressors
    for ii in range(nrvt):
        key  = phobj.regress_rvt_keys[ii]
        ylab = key + '\\n' + '$\Delta={}$'.format(retobj.rvt_shift_list[ii])

        data_lab[ii] = ylab
        data_arr[:,ii] = phobj.regress_dict_rvt[key]

    # --------------------- write tmp data file ---------------------

    # open the file and write the header/start
    fff = open(ftmp, 'w')
    # write data
    for ii in range(data_shape[0]):
        for jj in range(data_shape[1]):
            fff.write(" {:6.4f} ".format(data_arr[ii,jj]))
        fff.write('\n')
    # le fin: close and finish
    fff.close()

    # --------------------- make image of rvt data -----------------------

    par_dict = {
        'ftmp'    : ftmp,
        'fname'   : fname,
        'title'   : title,
        'all_lab' : ' '.join(['\''+lab+'\'' for lab in data_lab])
    }

    cmd = '''
    1dplot.py                                                            \
        -reverse_order                                                   \
        -infiles        {ftmp}                                           \
        -ylabels        {all_lab}                                        \
        -xlabel         "vol index"                                      \
        -title          "{title}"                                        \
        -prefix         "{fname}"
    '''.format(**par_dict)
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()

    # --------------- clean up tmp file
    cmd    = '''\\rm {ftmp}'''.format(**par_dict)
    com    = BASE.shell_com(cmd, capture=1)
    stat   = com.run()

    print("++ Made plot of {}-based RVT regressors: {}".format(label, fname))


    return 0
