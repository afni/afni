import os, sys
import copy
import numpy as np
from   scipy import signal as sps

def bandPassFilterRawDataAroundDominantFrequency(x, samp_freq,
                                                 min_bps, max_bps=None,
                                                 label='', verb=0):
    """Band pass filter raw data based on overall typical period of the
time series, and also return the peak (mode) of freq between [1,
Nyquist] in units of indices.

Parameters
----------
x : np.ndarray
    1D Python array (real/float values), the input time series
samp_freq : float
    sampling frequency (units of Hz) of the time series
min_bps : int or float
    Minimum number of beats (or breaths) per second
max_bps : int or float
    Maximum number of beats (or breaths) per second; if not entered, just
    constrain with Nyquist estimate
label : str
    label for the time series, like 'card' or 'resp'

Returns
-------
x : np.ndarray
    1D Python array (real/float values), the time series that has been 
    filtered
idx_freq_peak : int
    index of the peak/mode frequency in the filter here, which might be
    used later as the index of the "typical" frequency.

    """

    if (np.sum(np.isnan(x))) :
        print('** ERROR in bandPassFilterRawDataAroundDominantFrequency: ' 
              'nan values in data')
        return []

    N = len(x)

    # Determine harmonic frequency unit
    F0 = samp_freq/N

    # Determine Nyquist index: ceil(N/2)
    idx_ny = N // 2 + N % 2

    # Get bottom cutoff index (as int)
    idx_min = round(min_bps/F0)
    # Get top cutoff index (as int): constrain by both user max and
    # Nyquist
    if max_bps == None :
        idx_max = idx_ny
    else:
        idx_max = min(round(max_bps/F0), idx_ny)

    # Get Fourier transform and magnitude series
    X    = np.fft.fft(x)
    Xabs = np.abs(X)

    # Index and phys freq of peak freq
    idx_freq_peak  = np.argmax(Xabs[idx_min:idx_max]) + idx_min
    freq_peak = idx_freq_peak * F0

    print('++ For ' + label + ' data, bandpass filter frequency peak: '
          '{:.6f} Hz'.format(freq_peak))

    # Find bounds based on -3 dB limits (half peak)
    val_peak    = Xabs[idx_freq_peak]
    val_hpeak   = val_peak/2.0

    # [PT] use idx=1 as minimum here in the range() and for default
    # lowerMin, not 0, because that would be mean of time series,
    # which is not what we want
    leftIndices = [i for i in range(1, idx_freq_peak) if Xabs[i] < val_hpeak]
    if not(len(leftIndices)):    lowerMin = 1 # 0
    else:                        lowerMin = max(leftIndices)

    rightIndices = [i for i in range(idx_freq_peak, idx_ny) \
                    if Xabs[i] < val_hpeak]
    if not(len(rightIndices)) :  lowerMax = idx_ny-1
    else:                        lowerMax = min(rightIndices)

    # Avoid filter being too narrow
    lowerMin = min(round(idx_freq_peak/2), lowerMin)
    lowerMax = max(round(1.5*idx_freq_peak), lowerMax)

    # Determine band limits
    upperMin = N - lowerMax
    upperMax = N - lowerMin

    if verb :
        print("++ Report on Fourier peak filtering")
        print("   Total number of time points:", N)
        print("   Lower bound range: [{}, {}]".format(lowerMin, lowerMax))
        print("   Upper bound range: [{}, {}]".format(upperMin, upperMax))
        print("   Filtering being:")

    # Zero part of FT outside limits
    filterArray = np.zeros(N, dtype=float)
    filterArray[lowerMin:lowerMax] = 1.0
    filterArray[upperMin:upperMax] = 1.0
    Xfilt = X * filterArray

    # IFT (of which real part should be everything, as long as filter
    # was symmetric)
    xfilt = np.real(np.fft.ifft(Xfilt))

    return xfilt, idx_freq_peak

def get_peaks_from_bandpass(x, samp_freq, min_bps, max_bps=None, 
                            width_fac=4, label='', verb=0):
    """***ADD***

Parameters
----------
x : np.ndarray
    1D Python array (real/float values), the input time series
samp_freq : float
    sampling frequency (units of Hz) of the time series
min_bps : int or float
    Minimum number of beats (or breaths) per second
max_bps : int or float
    Maximum number of beats (or breaths) per second; if not entered, just
    constrain with Nyquist estimate
width_fac : int/float
    parameter/number by which to scale the samp_fac; the int() of the
    resulting ratio defines the 'width' param in sps.find_peaks();
    default was simply used in original program formulation
label : str
    label for the time series, like 'card' or 'resp'

Returns
-------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
idx_freq_mode : int
    index of the peak/mode frequency in the filter here, which might be
    used later as the index of the "typical" frequency.
xfilt : np.ndarray
    1D Python array (float values) of the bandpass filtered input time
    series

    """

    # Band pass filter raw data, and also get idx of peak freq mode
    # within range filtered
    xfilt, idx_freq_mode \
        = bandPassFilterRawDataAroundDominantFrequency(x, samp_freq,
                                                       min_bps, 
                                                       max_bps=max_bps,
                                                       label=label, 
                                                       verb=0)
    if len(xfilt) == 0:
       print("** ERROR: Failed to band-pass filter '{}' data".format(label))
       return []

    # Get initial peaks of bandpassed time series
    width    = int(samp_freq / width_fac)
    peaks, _ = sps.find_peaks(xfilt, 
                              width=width)        

    # listify
    peaks    = list(peaks)

    return peaks, idx_freq_mode, xfilt

# ---------------------------------------------------------------------------

### !!! NB: original code had a kwarg 'period', but that was commented
### !!! out and not actually currently being used
# Adjust peaks from uniform spacing
#    peaks = lpf.refinePeakLocations(peaks, rawData, 
#            dataType = "Respiratory",  
#            phys_fs = parameters["phys_fs"], 

def refinePeakLocations(peaks, x, is_troughs = False, label='', verb=0):
    """Adjust peaks to correspond to local maxima.  This is usually
necessary if the peaks were determined from a band-pass filtered
version of the input raw data where the peaks are uniformly spaced
based on the overall typical period of raw data.

This basically takes each input peak, makes a tiny plus/minus interval
around it, and finds the local max in x within that. Output number of
peaks matches input number.

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
is_troughs: bool
    are we processing peaks or troughs here?
label : str
    label for the time series, like 'card' or 'resp'

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    # check for min number of peaks
    if len(peaks) < 1 :
        print("** No peaks to start with for refinement!")
        return []

    N      = len(x)
    Npeaks = len(peaks)
    opeaks = []                # init output

    # Determine half window width from distribution of intervals
    # !!! check more about this---why '4' here?
    # !!! probably need a minimum number of peaks, up above?
    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    halfWindowWidth = round(np.median(intervals)/4)

    # adjust each peak by location of local max in original ts
    for ii in range(Npeaks):
        # determine mini-window, and local extremum within the win
        idx     = peaks[ii]
        start   = max(0, idx - halfWindowWidth)
        finish  = min(idx + halfWindowWidth, N-1)
        if is_troughs:
            opeaks.append(start + np.argmin(x[start:finish]))
        else:
            opeaks.append(start + np.argmax(x[start:finish]))
    
    return opeaks

def percentileFilter_global(peaks, x, perc_filt = 10.0, 
                            is_troughs = False, label='', verb=0):
    """Filter peaks based on global percentile of time series data x.

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
perc_file : int/float
    Percentile value in range [0, 100] for calculating threshold
is_troughs: bool
    whether the threshold is the maximum acceptable value.  Default is
    False meaning it is the minimum acceptable value
label : str
    label for the time series, like 'card' or 'resp'

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    # !!! is (earlier) upperThreshold basically set based on whether
    # !!! the dset is peaks or troughs?

    # !!! what about not removing the peak *if* it creates a gap
    # !!! greater than 1.5 times the median peak distribution? that
    # !!! way, we don't remove little bumps that might still be
    # !!! believable based on earlier stuff

    N = len(x)
    
    # make list of time series values at peak locations
    peak_vals = [x[idx] for idx in peaks]
    
    # Remove peaks that are less than the the required percentile of
    # the input signal

    # global threshold
    thr = np.percentile(x, perc_filt)

    # interestingly elementwise boolean comparisons work when
    # peak_vals is a list, but not when an array
    # !!!! doublecheck this when peaks itself is a list!!!
    tmp = np.array(peaks)
    if is_troughs : 
        opeaks = list(tmp[peak_vals <= thr])
    else: 
        opeaks = list(tmp[peak_vals >= thr])

    return opeaks

def percentileFilter_local(peaks, x, perc_filt = 10, 
                           is_troughs = False, 
                           period_idx = None, nbhd_idx = 4,
                           label='', verb=0):
    """Filter peaks based on local percentile of time series x.

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
perc_file : int/float
    Percentile value in range [0, 100] for calculating threshold
is_troughs : bool
    whether the threshold is the maximum acceptable value.  Default is
    False meaning it is the minimum acceptable value
period_idx : int
    typical period of the time series, in units of index counts.
    Default is None, meaning the period is determined from the data
nbhd_idx : int
    size of a neighborhood for local percentile estimation, in units
    of index counts. Default chosen from early testing/practice
label : str
    label for the time series, like 'card' or 'resp'

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    # !!! is (earlier) upperThreshold basically set based on whether
    # !!! the dset is peaks or troughs?

    N = len(x)

    # Estimate period from time series if not supplied
    if period_idx == None :
        period_idx = getTimeSeriesPeriod_as_indices(x)

    # make list of time series values at peak locations
    peak_vals = [x[idx] for idx in peaks]

    # Determine local percentile-based threshold around each peak
    halfWindowWidth = round(period_idx * nbhd_idx / 2.0)

    # store per-index (local) thresholds
    all_thr = []
    halfWindowWidth = round(period_idx * nbhd_idx / 2.0)
    for idx in peaks:
        min_idx = max(0, idx - halfWindowWidth)
        max_idx = min(N-1, idx + halfWindowWidth)
        all_thr.append(np.percentile(x[min_idx:max_idx], perc_filt))

    # interestingly elementwise boolean comparisons work between
    # lists, but not if these were arrays
    peak_vals  = np.array(peak_vals)
    all_thr    = np.array(all_thr)
    tmp        = np.array(peaks)
    if is_troughs :
        opeaks = list(tmp[peak_vals <= all_thr])
    else: 
        opeaks = list(tmp[peak_vals >= all_thr])

    return opeaks


def getTimeSeriesPeriod_as_indices(x, min_nidx=1):
    """Get overall typical period(s) of time series x in terms of number
of indices.

!!! NB: this could also be separately returned by the bandpassFilter*()
function, which might be a preferable way to go. !!!

Parameters
----------
x : np.ndarray
    1D Python array (real/float values), the input time series
min_nidx : int
    value of minimum number of indices that can be in the period.

Returns
-------
nidx : int
    integer value for the time series period (counting indices)

    """
    
    # !!! have to check if we should move away from very low freq
    # !!! here?  Probably?

    N = len(x)

    max_nidx = round(N/2)              # Frequency limit is Nyquist frequency

    # FT, unshifted
    X = abs(np.fft.fft(x))
    if min_nidx == max_nidx :
        idx_freq_mode = min_nidx
    else:
        # subset between mean and max freq
        Xsub = X[min_nidx:max_nidx]
        # find and return the index at the mode value
        idx_freq_mode = min_nidx + np.argmax(Xsub)

    return  N/idx_freq_mode
    
# ----------------------------------------------------------------------------

def removeClosePeaks(peaks, x, period_idx,
                     is_troughs = False, width_fac=4.0, label='', verb=0):
    """Remove peaks (or troughs) that are closer than: period/width_fac.

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
period_idx : int
    typical period of the time series, in units of index counts.
is_troughs: bool
    are we processing peaks or troughs here?
width_fac : int/float
    parameter/number by which to scale the samp_fac; the int() of the
    resulting ratio defines the 'width' param in sps.find_peaks();
    default was simply used in original program formulation
label : str
    label for the time series, like 'card' or 'resp'

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    # Make and filter inter-peak intervals
    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    threshold = int(period_idx / width_fac)
    last = len(intervals) - 1
    for i in range(last, 0, -1):
        if (intervals[i] < threshold):
            intervals[i-1] = intervals[i-1] + intervals[i]
            del intervals[i]

    # Make peaks from intervals, recursively
    Nival  = len(intervals)
    opeaks = np.zeros(Nival+1, dtype=int)
    opeaks[0] = peaks[0]
    for ii in range(Nival):
        opeaks[ii+1] = opeaks[ii] + intervals[ii]

    ### !!! Q: why loop here?
    #### !!! NB: 'period' or 'period_idx' kwarg not actually used in
    #### !!! refinePeakLocations()
    # Adjust peaks/troughs from uniform spacing
    #for i in range(0,2):
    opeaks = refinePeakLocations(opeaks, x, is_troughs=is_troughs)
    # period_idx = period_idx)

    # bit of cleaning of peaks: remove degeneracies and sort
    opeaks  = list(set(opeaks))
    opeaks.sort()

    return opeaks


def addMissingPeaks(peaks, x, is_troughs=False, verb=0):
    """Use the information about the statistics of the intervals of peaks
(or troughs) to estimate where missing peaks (or troughs) should be
inserted into the input 'peaks' collection (which can also represent
troughs!).

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
is_troughs: bool
    are we processing peaks or troughs here?

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

"""

    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    Nival     = len(intervals)

    # min_ival: min interval scale, based on median of all intervals
    FRAC_FAC   = 0.9
    min_ival   = np.median(intervals)*FRAC_FAC
    # ... used to make an array for inserting points, only values >=0
    peaksToAdd = [max(round(intervals[i]/min_ival)-1,0) for i in range(Nival)]

    # report on number being added
    Ntoadd = np.sum(np.array(peaksToAdd))
    if verb :
        if is_troughs : nnn = 'troughs'
        else:           nnn = 'peaks'
        print("++ Adding this many {}: {}".format(nnn, Ntoadd))

    # work with a copy of the peaks
    opeaks = copy.deepcopy(peaks)

    # go backwords through the opeaks and insert likely missing peaks
    for i in range(Nival-1,-1,-1):
        if (peaksToAdd[i]):
            nadd_p1   = peaksToAdd[i] + 1
            start     = opeaks[i]
            end       = opeaks[i+1]
            increment = int((end-start)/nadd_p1)    # must be integer
            all_new   = [start+increment*j for j in range(1,nadd_p1)]
            # go through list backwards to get final, inserted order right.
            for jj in range(len(all_new)-1, -1, -1):
                opeaks.insert(i+1, all_new[jj])
            #opeaks    = np.insert(opeaks, i+1, all_new)

    # adjust peaks for uniform spacing
    opeaks = refinePeakLocations(opeaks, x, is_troughs=is_troughs)

    return opeaks


def calc_interval_stats(A, samp_rate=None, 
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
minval : float
    minimum value in A
maxval : float
    maximum value in A
stats_arr : np.ndarray
    an array of stats about the intervals of A, calculated from
    percentile values in all_perc

    """

    # make sure A has values
    N = len(A)
    if not(N):
        return None, None, np.zeros(0, dtype=float)

    # make interval set
    intervals = [j-i for i, j in zip(A[:-1], A[1:])]

    # min/max
    minval = np.min(A)
    maxval = np.max(A)

    # calculate percentiles
    Nperc  = len(all_perc)
    if Nperc :
        stats_arr = np.percentile(intervals, q=all_perc)
    else: 
        stats_arr = np.zeros(0, dtype=float)

    # scale values, if applicable
    if samp_rate :
        minval*= samp_rate
        maxval*= samp_rate
        if Nperc :
            stats_arr*= samp_rate

    return minval, maxval, stats_arr

# -------------------------------------------------------------------------

def classify_endpts(peaks, troughs, x, label=None, verb=0):
    """Figure out whether the endpoints of the time series x are peaks or
troughs, and add those to the approrpiate collection.

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
troughs : np.ndarray
    1D Python array (int values) of the indices of trough locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
label : str
    label for the time series, like 'card' or 'resp'


Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    # init output
    opeaks   = copy.deepcopy(peaks)
    otroughs = copy.deepcopy(troughs)

    Nx = len(x)                       # num of actual time points in ts

    # check first endpoint; assign simply by (anti)proximity, unless
    # it is already claimed
    if opeaks[0] == 0 or otroughs[0] == 0 :
        # nothing to do, endpt is already classified
        pass
    elif opeaks[0] > otroughs[0] :
        opeaks.insert(0,0)
    else :
        otroughs.insert(0,0)

    # similarly, check last endpoint
    if opeaks[-1] == Nx or otroughs[-1] == Nx :
        # nothing to do, endpt is already classified
        pass
    elif opeaks[-1] < otroughs[-1] :
        opeaks.insert(-1, Nx-1)
    else :
        otroughs.insert(-1, Nx-1)


    return opeaks, otroughs
