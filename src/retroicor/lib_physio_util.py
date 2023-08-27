#!/usr/bin/env python

import os, sys
import copy
import numpy as np


# ===========================================================================

def calc_interval_stats_perc(A, samp_rate=None, 
                             all_perc = (10, 25, 40, 50, 60, 75, 90),
                             verb=0 ):
    """Calculate percentile-based statistics of the intervals of 1D array
A (which is assumed to be sorted already).  A sampling rate can be
input to provide output units; if none is entered, then units are
those of A (which are typically unitless when A represents peaks or
troughs).

Parameters
----------
A : list
    1D list (likely of int values if representing peaks or 
    troughs); assumed to be sorted already
samp_rate : float
    physical value of sampling rate associated with A
all_perc : set/np.ndarray
    1D Python array of percentile values in range [0, 100], for which 
    values of the distribution of intervals within A will be calculated

Returns
-------
stats_arr : np.ndarray
    an array of stats about the intervals of A, calculated from
    percentile values in all_perc

    """

    # make sure A and all_perc have values
    N     = len(A)
    Nperc = len(all_perc)
    if not(N) or not(Nperc) :
        return ()

    # make interval set
    intervals = [j-i for i, j in zip(A[:-1], A[1:])]

    # calculate percentiles
    stats_arr = np.percentile(intervals, q=all_perc)

    # scale values, if applicable
    if samp_rate :
        stats_arr*= samp_rate

    return stats_arr

def calc_interval_stats_mmms(A, samp_rate=None, 
                             verb=0 ):
    """Calculate statistics (min, max, mean and stdev) of the intervals of
1D array A (which is assumed to be sorted already).  A sampling rate
can be input to provide output units; if none is entered, then units
are those of A (which are typically unitless when A represents peaks
or troughs).

Parameters
----------
A : list
    1D list (likely of int values if representing peaks or 
    troughs); assumed to be sorted already
samp_rate : float
    physical value of sampling rate associated with A

Returns
-------
minval : float
    minimum value in A
maxval : float
    maximum value in A
meanval : float
    mean (average) value of collection A
stdval : float
    standard deviation value of collection A

    """

    # make sure A and all_perc have values
    N     = len(A)
    if not(N) :
        return ()

    # make interval set
    intervals = [j-i for i, j in zip(A[:-1], A[1:])]

    # calculate values
    minval  = np.min(intervals)
    maxval  = np.max(intervals)
    meanval = np.mean(intervals)
    stdval  = np.std(intervals)

    # scale values, if applicable
    if samp_rate :
        minval *= samp_rate
        maxval *= samp_rate
        meanval*= samp_rate
        stdval *= samp_rate

    return minval, maxval, meanval, stdval

# ----------------------------------------------------------------------------

def make_str_bandpass(label, lab_title, lab_short,
                      prefix='', odir='', ext='svg'):
    """Construct an output filename for the plot of the bandpassing, which
is the first part of peak finding, as well as a title for the plot.

Parameters
----------
label : str
    label for type of time series: 'card', 'resp', etc.
lab_title : str
    longer descriptor of the idx-th step, for the title
lab_short : str
    short descriptor of the idx-th step, for the filename
prefix : str
    optional basename for full output
odir : str
    optional path for output directory
ext : str
    file extension type (prob stick with vector graphics)
all_subscript : bool
    bc one title has a subscript, make sure *all* do, so vertical
    space taken by title is constant (and figs don't jump); this adds
    a whitespace subscript at the end of the title, if no '$' is 
    already present in the title.

Returns
-------
fname : str
    name of file to be written
title : str
    string for the title at the top of the figure

    """

    # make the filename
    fname = '{}_{}.{}'.format(label, lab_short, ext)
    if prefix :  fname = prefix + '_' + fname
    if odir :    fname = odir.rstrip('/') + '/' + fname

    # make the title string
    title = 'Intermediate step for {} data: {}'.format(label, lab_title)

    return fname, title
