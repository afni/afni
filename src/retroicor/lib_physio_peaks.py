#!/usr/bin/env python

import os, sys
import copy
import numpy  as np
from   scipy  import signal as sps
from   afnipy import lib_physio_plot as lpplt
from   afnipy import lib_physio_util as lpu

def bandPassFilterRawDataAroundDominantFrequency(x, samp_freq,
                                                 min_bps, max_bps=None,
                                                 label='', retobj=None,
                                                 verb=0):
    """Band pass filter raw data based on overall typical period of the
time series, and also return the peak (mode) of freq between [1,
Nyquist] in units of indices.

If retobj is included, then a plot of the FT frequencies can be
created.

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
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs

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

    # Determine frequency step unit along freq axis (in Hz)
    delta_f = samp_freq/N

    # Determine Nyquist index: ceil(N/2)
    idx_ny = N // 2 + N % 2

    # Get bottom cutoff index (as int)
    idx_min = round(min_bps/delta_f)
    # Get top cutoff index (as int): constrain by both user max and
    # Nyquist
    if max_bps == None :
        idx_max = idx_ny
    else:
        idx_max = min(round(max_bps/delta_f), idx_ny)

    # Get Fourier transform and magnitude series
    X    = np.fft.fft(x)
    Xabs = np.abs(X)

    # Index and phys freq of peak freq
    idx_freq_peak  = np.argmax(Xabs[idx_min:idx_max]) + idx_min
    freq_peak = idx_freq_peak * delta_f

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

    # ---- done with work, but can save also FT freq magn
    if retobj != None and retobj.save_graph_level > 1 :
        odir      = retobj.out_dir
        prefix    = retobj.prefix
        lab_title = 'Frequency magnitude spectrum, with bandpassing'
        lab_short = 'bandpass_spectrum'
        
        fname, title = lpu.make_str_bandpass(label,
                                             lab_title, lab_short, 
                                             prefix=prefix, odir=odir)
        lpplt.makefig_ft_bandpass_magn(X, Xfilt,
                                       delta_f, idx_ny,
                                       title=title, fname=fname,
                                       label=label,
                                       retobj=retobj,
                                       verb=verb)

    return xfilt, idx_freq_peak

def get_peaks_from_bandpass(x, samp_freq, min_bps, max_bps=None, 
                            width_fac=4, label='', retobj=None, verb=0):
    """Use bandpassing to smooth out the time series, and then search for
peaks in what remains as a first pass.  The art of this is picking a
good band to apply.  Here, we look for a major peak above the
(extended) baseline peak, and roll with that.  The min_bps and max_bps
provide useful guard rails for this process.  Additionally, the
width_fac feature is used within the Scipy peak finding function
function

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

    # Bandpass filter raw data, and also get idx of peak freq mode
    # within range filtered
    xfilt, idx_freq_mode \
        = bandPassFilterRawDataAroundDominantFrequency(x, samp_freq,
                                                       min_bps, 
                                                       max_bps=max_bps,
                                                       label=label, 
                                                       retobj=retobj,
                                                       verb=0)
    if len(xfilt) == 0:
       print("** ERROR: Failed to band-pass filter '{}' data".format(label))
       return []

    # --- Get initial peaks of bandpassed time series
    # !!! PT: revisit this, starting to understand, but reset to original now

    # Use the mode of the bandpassed ts (idx_freq_mode) to put a
    # minimum-distance requirement on peaks
    delta_f = retobj.data[label].ft_delta_f         # FT freq step, in Hz
    phys_freq_mode = idx_freq_mode * delta_f        # FT peak freq, in Hz
    min_dist_idx = int(0.5 * phys_freq_mode / delta_f) # min interval bt pks

    width    = int(samp_freq / width_fac)    ### earlier approach
    peaks, _ = sps.find_peaks(xfilt, distance=min_dist_idx,
                              width=width)

    # listify
    peaks    = list(peaks)

    return peaks, idx_freq_mode, xfilt

# ---------------------------------------------------------------------------

def refinePeakLocations(peaks, x, is_troughs = False, 
                        window_scale = 0.25, window_size = None,
                        verb=0):
    """Adjust peaks to correspond to local maxima.  This is usually
necessary if the peaks were determined from a band-pass filtered
version of the input raw data where the peaks are uniformly spaced
based on the overall typical period of raw data.

This basically takes each input peak, makes a plus/minus window around
it, and finds the local max (or min, for troughs) in x there; the peak
shifts to that new location. Output number of peaks matches input
number.

If window_size is None (default), use the median of peaks as the base
window size (scaled then by window_scale).  For local refinement, one
might want to first calculate the median interpeak interval across
*all* peaks, but input just a mini-list of locations while providing
that full-set interval as window_size (which will still be scaled by
window_scale).

Parameters
----------
peaks : list
    1D list (int values) of the indices of peak locations that
    were estimated within x
x : np.ndarray
    1D Python array (real/float values), the input time series
window_scale : float
    factor to scale the peak-searching window; the window is the 
    median(inter-peak interval)*window_scale
window_size : float
    size of window (which will be scaled by window_scale) within 
    which to refine.  Typically None, unless one enters a subset of 
    peaks to refine (whose median might not be trustworthy)
is_troughs: bool
    are we processing peaks or troughs here?

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
    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    if window_size :
        halfWindowWidth = round(window_size * window_scale)
    else:
        halfWindowWidth = round(np.median(intervals)*window_scale)

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
                            is_troughs = False, 
                            save_by_interval = True, verb=0):
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
save_by_interval: bool
    if on, choose to not filter out peaks whose removal would leave
    large gaps

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """


    # !!! what about not removing the peak *if* it creates a gap
    # !!! greater than 1.5 times the median peak distribution? that
    # !!! way, we don't remove little bumps that might still be
    # !!! believable based on earlier stuff

    N = len(x)
    
    # make list of time series values at peak locations
    peak_vals = np.array([x[idx] for idx in peaks])
    
    # Remove peaks that are less than the required percentile of the
    # input signal

    # global threshold
    thr = np.percentile(x, perc_filt)

    # estimate keep_arr with threshold: True means 'preserve'
    if is_troughs :  keep_arr = peak_vals <= thr
    else:            keep_arr = peak_vals >= thr

    if save_by_interval :
        keep_arr = make_save_by_interval(keep_arr, peaks, verb=verb)

    # apply 'keep' filter
    tmp    = np.array(peaks)
    opeaks = list(tmp[keep_arr])

    return opeaks


def make_save_by_interval(keep_arr, peaks, verb=0):
    """Go through 'keep' filter, and don't allow peaks to be removed if
doing so greatly increases within-peak distance.

The threshold value for deciding to not remove a point-to-be-filtered is:
+ if doing so leads to an interpeak distance of >1.5 * med(interpeak dist),
+ and if that points minimal interval is >0.5 * med(interpeak dist)

Parameters
----------
keep_arr : array
    1D boolean array, where True means 'keep that peak value' and False 
    means 'remove it'
peaks : array
    1D array of peak indices, same length as filt_arr

Returns
-------
okeep_arr : array
    1D boolean array, after possibly saving some peaks, where True/False
    have their original meanings

    """

    N = len(keep_arr)
    okeep_arr = copy.deepcopy(keep_arr)

    if np.sum(okeep_arr) == N :
        # all points being kept, nothing to do
        return okeep_arr

    # loop through candidate points, because changing filter can
    # affect other points.  Loop through until first time nothing is
    # updated, or a fixed max number of iterations.
    MAX_NLOOP = 50
    nloop = 0
    found = True
    while found and nloop < MAX_NLOOP :
        nloop+= 1
        found = False

        # indices of 'bad' peaks to (likely) filter, len N
        bad_indices = np.arange(N)[okeep_arr == False]
        nbad = len(bad_indices)
        # all_intervals, len N-1
        intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
        # the 'double interval' span, len N-2 (no first/last peak)
        doub_span = [j+i for i, j in zip(intervals[:-1], intervals[1:])]

        # define the threshold for the double-interval span, as well
        # as for single interval span (*can refine this?*)
        med_ival = np.median(intervals)
        span_thr = 1.5*med_ival
        ival_thr = 0.5*med_ival

        for ii in range(nbad):
            bad_idx = bad_indices[ii]
            if bad_idx > 0 and bad_idx < N-1 :
                bad_doub_span = doub_span[bad_idx-1]
                bad_ival_min  = min(intervals[bad_idx], intervals[bad_idx-1])
                if bad_doub_span > span_thr and \
                   bad_ival_min > ival_thr :
                    if verb > 1 :
                        print("   -> retain an extremum: "
                              'idx =', peaks[bad_idx], 
                              bad_doub_span, '>', span_thr)
                    # move this index to KEEP
                    okeep_arr[bad_idx] = True
                    found = True

    return okeep_arr

# -------------------------------------------------------------------------

def percentileFilter_local(peaks, x, perc_filt = 10, 
                           is_troughs = False, 
                           period_idx = None, nbhd_idx = 4,
                           save_by_interval = True, verb=0):
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
save_by_interval: bool
    if on, choose to not filter out peaks whose removal would leave
    large gaps

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    N = len(x)

    # Estimate period from time series if not supplied
    if period_idx == None :
        period_idx = getTimeSeriesPeriod_as_indices(x)

    # make list of time series values at peak locations
    peak_vals = np.array([x[idx] for idx in peaks])

    # Determine local percentile-based threshold around each peak
    halfWindowWidth = round(period_idx * nbhd_idx / 2.0)

    # store per-index (local) thresholds
    all_thr = []
    halfWindowWidth = round(period_idx * nbhd_idx / 2.0)
    for idx in peaks:
        min_idx = max(0, idx - halfWindowWidth)
        max_idx = min(N-1, idx + halfWindowWidth)
        all_thr.append(np.percentile(x[min_idx:max_idx], perc_filt))
    all_thr = np.array(all_thr)


    # estimate keep_arr with threshold: True means 'preserve'
    if is_troughs :  keep_arr = peak_vals <= all_thr
    else:            keep_arr = peak_vals >= all_thr

    if save_by_interval :
        keep_arr = make_save_by_interval(keep_arr, peaks, verb=verb)

    # apply 'keep' filter
    tmp    = np.array(peaks)
    opeaks = list(tmp[keep_arr])

    return opeaks


def getTimeSeriesPeriod_as_indices(x, min_nidx=1):
    """Get overall typical period(s) of time series x in terms of number
of indices.

NB: This function doesn't do try to stay away from baseline
frequencies.  Therefore, we try to use essentially the peak freq in
xfilt in the main code, rather than this.

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
                     is_troughs = False, width_fac=4.0, verb=0):
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

    # Adjust peaks/troughs with local refinement.  Looping allows a
    # bit of 'extra' movement for points on a slope while also
    # maintaining a smaller refinement window, so that peaks don't
    # jump wildly.
    for i in range(0, 2):
        opeaks = refinePeakLocations(opeaks, x, is_troughs=is_troughs)

    # bit of cleaning of peaks: remove degeneracies and sort
    opeaks  = list(set(opeaks))
    opeaks.sort()

    return opeaks


def addMissingPeaks(peaks, x, is_troughs=False, window_scale=1.75 , verb=0):
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
window_scale : float
    scale factor help determine whether two peaks are 'far enough'
    apart to add in a new one; will do so if interpeak interval is
    greater than: (1+window_scale)*median(interpeak interval)

Returns
-------
opeaks : list
    1D list (int values) of the indices of new peak locations that
    were estimated within x, starting from input set of peaks

    """

    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    Nival     = len(intervals)
    med_ival  = np.median(intervals)

    # ref_ival: reference value, min interval scale based on median of
    # all intervals
    ref_ival   = np.median(intervals)*window_scale
    # add a peak if: the interval is greater than ref_ival; the number
    # of peaks is the integer part of the interval width divided by
    # the median, minus 1
    peaksToAdd = [max(int(np.floor(intervals[i]/med_ival))-1,0)*(intervals[i]>ref_ival) for i in range(Nival)]

    # report on number being added
    Ntoadd = np.sum(np.array(peaksToAdd))
    if verb and Ntoadd :
        if is_troughs : nnn = 'troughs'
        else:           nnn = 'peaks'
        print("   --> adding this many {}: {}".format(nnn, Ntoadd))

    # work with a copy of the peaks
    opeaks = copy.deepcopy(peaks)

    # go backwards through the opeaks and insert likely missing peaks
    for i in range(Nival-1,-1,-1):
        if (peaksToAdd[i]):
            nadd_p1   = peaksToAdd[i] + 1  # '+1' makes this 'num of intervals'
            start     = opeaks[i]
            end       = opeaks[i+1]
            increment = int((end-start)/nadd_p1)    # must be integer
            all_new   = [start+increment*j for j in range(1,nadd_p1)]
            # refine this mini-set immediately, using the median of
            # the full peaks set as window size
            all_new = refinePeakLocations(all_new, x, is_troughs=is_troughs,
                                          window_size=med_ival)
            # go through list backwards to get final, inserted order right.
            for jj in range(len(all_new)-1, -1, -1):
                opeaks.insert(i+1, all_new[jj])
#### !!! done above now, locally
####    # adjust peaks for uniform spacing
####    opeaks = refinePeakLocations(opeaks, x, is_troughs=is_troughs)

    return opeaks

# -------------------------------------------------------------------------

def classify_endpts(peaks, troughs, x, verb=0):
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
