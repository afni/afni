import sys, os
import copy
import numpy               as np


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
