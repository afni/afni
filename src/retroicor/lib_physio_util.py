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

# ---------------------------------------------------------------------------

def interleave_count(A, B, verb=0):
    """A and B are sorted lists of numbers. For each [i]th element of A,
count how many elements in B fall between pairs A[i] and A[i+1].  This
number is stored in an array C, which len(A)-1 elements.  NB: A can
have no repeating elements, and for the major use case here (where A
and B each store indices of peaks and troughs, respectively), neither
will.

This essentially provides information of how interleaved A and B are,
which is useful if (for example) they respectively represent indices
of peaks and troughs, for which there should theoretically be perfect
interleaving. For perfectly interleaved lists, each C[i] = 1.  If a
trough were missing between a pair of peaks, then C[i] = 0. Having
"too many" troughs between a pair of peaks would be reflected by
having C[i] > 1.  Endpoint dangling, for <A[0] and > A[-1], are also
returned, but in a separate tuple.

One could measure interleaving with A for peaks and B for troughs, or
vice versa.

NB: we assume A and B share no elements

Parameters
----------
A : list
    a 1D list of indices
B : list
    a 1D list of indices

Returns
-------
C : list
    a 1D list of length len(A)-1, recording how many of the values
    stored in B fall between A[i] and A[i+1].
D  : list
    a 1D list of length 2, recording how many of the values
    stored in B fall below A[0] and above A[-1], respectively.

    """

    # ensure no overlap of A and B, and nonrepetition of each
    olap = set(A).intersection(set(B))
    if len(olap):
        print("** ERROR: A and B cannot share any elements, but they do!")
        print("   " + str(list(olap)))
        sys.exit(3)
    if len(A) != len(set(A)) :
        print("** ERROR: List A cannot have repeated elements, but it does!")
        sys.exit(3)
    if len(B) != len(set(B)) :
        print("** ERROR: List B cannot have repeated elements, but it does!")
        sys.exit(3)
        
    NB = len(B)
    NA = len(A)
    C = np.zeros(NA-1, dtype=int)          # between-pair counts
    D = np.zeros(2, dtype=int)             # endpoint-dangling counts
    
    # precursor: fill in the first dangling endpoint.
    # NB: we have the start variable like this, to keep using below
    start = 0
    j = start
    while j < NB :
        if B[j] > A[0] :
            # nothing to count, so stop and move along to next A[i]
            start = j
            break
        elif B[j] < A[0] :
            # in the dangling range: count it
            D[0]+= 1
        # implied 'else': too small to be counted, just go to next ele
        j+= 1

    # start the main loop, filling in C (use value of start from above
    for i in range(NA-1):
        j = start
        while j < NB :
            if B[j] > A[i+1] :
                # nothing to count, so stop and move along to next A[i]
                start = j
                break
            elif A[i] < B[j] :
                # in the between-pair range: count it
                C[i]+= 1
            # implied 'else': too small to be counted, just go to next ele
            j+= 1

    # postcursor: fill in the last dangling endpoint.
    # NB: all remaining elements of B should be dangling, but count 
    # pedantically, anyways
    j = start
    while j < NB :
        if B[j] > A[-1] :
            # in the dangling range: count it
            D[1]+= 1
        # implied 'else': too small to be counted, just go to next ele
        j+= 1

    return C, D
