import sys, os
import copy
import numpy as np

# ===========================================================================
# params

PI2 = np.pi * 2


# ===========================================================================

def calc_phases_M1(phobj, verb=0):
    """Calculate phases of a time series, here using "Method 1", which
essentially just looks at peak info.  This is largely just a simple,
linear mapping of the interval [-Pi, Pi) to each interpeak interval.
This applies to card time series at present.

When done, add phases array to the phobj object.

Parameters
----------
phobj : phys_ts_obj class
    object with all necessary input time series info; will also store
    the newly populated phases array when done here

Returns
-------
phases : np.ndarray (1D)
    Phases estimated here, in range [-Pi, Pi)

    """

    # some simpler definitions here (NB: not copying obj, just
    # dual-labelling for simplifying function calls while still
    # updating peaks info, at end)
    peaks  = phobj.peaks
    Npeaks = phobj.n_peaks
    Nts    = phobj.n_ts_orig

    # init phases: matches len of input time series
    phases = np.zeros(Nts, dtype=float)

    # Fill in phases between all peaks, in range [0, 1) here;
    # shifted+scaled later
    for ii in range(Npeaks-1):
        start  = peaks[ii]
        end    = peaks[ii+1]
        period = end - start
        # start value is always zero
        for jj in range(start+1, end):
            phases[jj] = (jj-start)/period

    # Fill in phases before first peak, just copying first phase
    # interval (walking backwards and filling in as much as needed)
    len_ph = peaks[1] - peaks[0]           # len of first interval phase
    for jj in range(1, peaks[0]+1):
        # use modulo so we can keep repeating copy of first phase
        # interval, if necessary
        kk = jj % len_ph
        phases[peaks[0]-jj] = phases[peaks[1]-kk]

    # Fill in phases after last peak, just copying last phase
    len_ph = peaks[-1] - peaks[-2]           # len of last interval phase
    nfill  = Nts - peaks[-1]                 # num of pts to fill in 
    for jj in range(1, nfill):
        # use modulo so we can keep repeating copy of first phase
        # interval, if necessary
        kk = jj % len_ph
        phases[peaks[-1]+jj] = phases[peaks[-2]+kk]

    # final (vectorized) shifting+scaling; final interval is [-Pi, Pi)
    phases-= 0.5
    phases*= PI2

    return phases

# ===========================================================================

def calc_phases_M2(phobj, nbin=100, verb=0):
    """Calculate phases of a time series, here using "Method 2", which is
some reasonably complicated function of interpeak interval
distribution stats and the time series. This comes from Glover, Li and
Riess (2000), starting just before their Eq. 3. I think the logic of
this formulation is that one uses the deepest breath (largest measured
respiration time series value) to define what will be "full phase";
breaths with amplitude less than this max out at smaller phase
magnitudes.  This is PL's original implementation of this algorithm.


NB: this almost seems like it could be done much more
efficiently mapping the time series to the phase range, with
min(trough value) -> -Pi and max(peak value) -> Pi, and interpolate
everything else. (*note to self*: Try this latter idea in a parallel
func and compare!!!)

The phases are estimated within the interval [-Pi, Pi).  This applies
to resp time series at present.

When done, add phases array to the phobj object.

Parameters
----------
phobj : phys_ts_obj class
    object with all necessary input time series info; will also store
    the newly populated phases array when done here
nbin : int
    parameter for the distribution histogram

Returns
-------
phases : np.ndarray (1D)
    Phases estimated here, in range [-Pi, Pi)

    """

    # some simpler definitions here (NB: not copying obj, just
    # dual-labelling for simplifying function calls while still
    # updating peaks info, at end)
    peaks  = phobj.peaks
    Npeaks = phobj.n_peaks
    troughs  = phobj.troughs
    Ntroughs = phobj.n_troughs

    # vertically shift the time series used here
    ts_orig = phobj.ts_orig - min(phobj.ts_orig)
    Nts     = len(ts_orig)

    # init phases: matches len of input time series
    phases = np.zeros(Nts, dtype=float)

    # Determine whether currently inspiration or expiration
    if peaks[0] < troughs[0]:    polarity = 1
    else:                        polarity = -1
        
    # Number of segments where each segment is either inspiration 
    # or expiration
    numFullSegments = Npeaks + Ntroughs - 1

    # Assign values to time series before first full segment
    peakIndex = 0
    troughIndex = np.argmin(troughs)
    start = 0
    finish = min(peaks[peakIndex], troughs[troughIndex])
    if finish == 0:
        finish = max(peaks[peakIndex], 
                     troughs[troughIndex])
    denom = finish  # Total length of segment
    
    # Histogram values in segment
    sample = [x - ts_orig[troughs[troughIndex]] 
              for x in ts_orig[start:finish]] 

    counts, bins = np.histogram(sample, bins=nbin) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity > 0: Rmax = max(sample)
    else: Rmax = ts_orig[peaks[0]] # Maximum value in segment
    for i in range(start,finish): # Move through segment
        end = round(sample[i]*nbin/Rmax) # Summation limit
        
        # Count values, in segment that are not greater than the 
        # summation limit
        count = 0
        end = min(end,len(counts)-1)
        for j in range(0,end):
            count = count + counts[j]
            
        # Use result to estimate phase at given time point
        phases[i] = (np.pi*count*polarity)/denom
    
    # Switch polarity and increment peak indxe if new polarity 
    #   inspiration.  Otherwise increment trough index instead
    polarity = -polarity
    if polarity > 0: peakIndex = peakIndex + 1
    else: troughIndex = troughIndex + 1
    
    # Process each segment in turn
    peakIndex = 0
    troughIndex = 0    
    for segment in range(0,numFullSegments):    
    
        # Determine segment from the peak and trough indices
        start = min(peaks[peakIndex], 
                    troughs[troughIndex])
        finish = max(peaks[peakIndex], 
                     troughs[troughIndex])
        denom = finish - start  # Total length of segment
        
        # Histogram values in segment
        sample = [x - ts_orig[troughs[troughIndex]] \
                  for x in ts_orig[start:finish]] 
        sample = sample - min(sample)
        counts, bins = np.histogram([x 
                    for x in sample if np.isnan(x) == False], 
                                    bins=nbin) 
        
        # Determine phase based on equation 3 is Glover paper
        Rmax = max(sample) # Maximum value in segment
        for i in range(start,finish): # Move through segment
            # Summation limit
            end = round(sample[i-start]*nbin/Rmax) 
            
            # Count values, in segment that are <= the 
            # summation limit
            count = 0
            end = min(end, len(counts))
            if end > 0:
                for j in range(0,end):
                    count = count + counts[j]
                
            # Use result to estimate phase at given time point
            phases[i] = (np.pi*count*polarity)/denom
            
        # Switch polarity and increment peak indxe if new polarity 
        # inspiration.  Otherwise increment trough index instead
        polarity = -polarity
        if polarity > 0: peakIndex = peakIndex + 1
        else: troughIndex = troughIndex + 1
        if peakIndex>=len(peaks) or \
            troughIndex>=len(troughs): break
    
    # Assign values to time series after last full segment
    start = finish
    finish = len(ts_orig)
    denom = finish - start  # Total length of segment
    
    # Histogram values in segment
    sample = [x - ts_orig[troughs[-1]]  \
              for x in ts_orig[start:finish]]  
    counts, bins = np.histogram(sample, bins=nbin) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity < 0:  Rmax = max(sample)
    else:             Rmax = ts_orig[peaks[-1]]     # max value in segment
    for i in range(start,finish):  # Move through segment
        # Summation limit
        end = round(sample[i-start]*nbin/Rmax) 
        
        if end >= len(counts):
            end = len(counts) - 1
        
        # Count values, in segment that are not greater than 
        #   the summation limit
        count = 0
        for j in range(0,end):
            count = count + counts[j]
            
        # Use result to estimate phase at given time point
        phases[i] = (np.pi*count*polarity)/denom

    return phases

def calc_phases_M3(phobj, nbin=100, verb=0):
    """Calculate phases of a time series, here using "Method 3", which
should be a faster way to approximate the recipe/algorithm from
Glover, Li and Riess (2000) for respiratory data; see calc_phases_M2()
for the more official way of doing that.

Essentially, Glover et al. want to estimate phase in range -Pi to Pi,
and map the min(trough) to -Pi and max(peak) to Pi, and smoothly
interpolate the time series within this.  This strikes as slightly
susceptible to noisy/peak trough values, and we might have to
percentile-ize cap them.  But I think we can do this directly, as
here, without making a running sum over a histogram. 

The phases are estimated within the interval [-Pi, Pi).  This applies
to resp time series at present.

When done, add phases array to the phobj object.

Parameters
----------
phobj : phys_ts_obj class
    object with all necessary input time series info; will also store
    the newly populated phases array when done here

Returns
-------
phases : np.ndarray (1D)
    Phases estimated here, in range [-Pi, Pi)

    """

    # some simpler definitions here (NB: not copying obj, just
    # dual-labelling for simplifying function calls while still
    # updating peaks info, at end)
    peaks    = phobj.peaks
    Npeaks   = phobj.n_peaks
    troughs  = phobj.troughs
    Ntroughs = phobj.n_troughs
    ts_orig  = phobj.ts_orig
    Nts      = phobj.n_ts_orig


    # init phases: matches len of input time series
    phases = np.zeros(Nts, dtype=float)

    # make recentered time series, for counting
    new_ts = ts_orig - min(ts_orig)
    max_ts = max(new_ts)

    # make histogram
    counts, bin_edges = np.histogram(new_ts, bins=nbin, range=(0, max_ts)) 

    # make signum of each phase value
    arr_sign = np.ones(Nts, dtype=int)
    min_tidx = 0                           # starting trough idx
    for ii in range(Npeaks-1):
        start = peaks[ii]
        end   = peaks[ii+1]
        tidx  = min_tidx
        while tidx < Ntroughs :
            if start < troughs[tidx] and troughs[tidx] < end :
                arr_sign[start:troughs[tidx]]*= -1
                min_tidx = tidx
                break
            tidx+=1
    # carry on for special end case, if last extremum is a trough
    if troughs[-1] > peaks[-1] :
        start = peaks[-1]
        end   = Nts - 1
        tidx  = min_tidx
        while tidx < Ntroughs :
            if start < troughs[tidx] and troughs[tidx] < end :
                arr_sign[start:troughs[tidx]]*= -1
                min_tidx = tidx
                break
            tidx+=1




    # get cumulative sum and denominator
    denom      = len(new_ts)
    cumul_frac = np.array([1.0*np.sum(counts[:ii+1]) for ii in range(nbin)])
    cumul_frac/= denom
    delta_bin  = bin_edges[1] - bin_edges[0]

    # map each point in new_ts to a bin in the cumul_frac, and use
    # that frac to define phase (bc gets multiplied by pi)
    for ii in range(Nts):
        # where are we in the cumulative distribution?
        idx = min(max(int(new_ts[ii] // delta_bin),0),nbin-1)
        # use that index to get the fractional value, applying sign
        phases[ii] = cumul_frac[idx] * arr_sign[ii]
    phases*= np.pi

    return phases

