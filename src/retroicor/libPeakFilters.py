#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 25 14:46:15 2022

@author: peterlauren
"""

import numpy as np
import math
import matplotlib as mpl
from matplotlib import figure as mplf
import matplotlib.pyplot as plt
import bisect

def percentileFilter(peaks, rawData, percentile, upperThreshold=False, graph = False, 
            phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        percentileFilter
            Filter peaks based on percentile of raw data
            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        percentileFilter(peaks, rawData, percentile, upperThreshold=False, graph = False, 
                    phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        percentile: (dType = float) Minimum (or maximum) percentile of raw data for a peak 
                            value to imply a valid peak (or trough)
                            
        upperThreshold: (dType = bool) Whether the threshold is the maximum acceptable value.  
                        Default is False meaning it is the minimum acceptable value
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
  
    # Get peak values
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
    
    # Remove peaks that are less than the the required percentile of the input signal
    # Note that any nan values are first filtered out of the input signal
    threshold = np.percentile([x for x in rawData if math.isnan(x) == False], percentile)
    if upperThreshold: peaks = peaks[peakVals <= threshold]    
    else: peaks = peaks[peakVals >= threshold]
            
    # Graph (and save) results as required
    if graph and phys_fs:
        if upperThreshold:
           graphPeaksAgainstRawInput(rawData, [], phys_fs, dataType, troughs = peaks,
                OutDir = OutDir, prefix = dataType + 'AdjustTroughsAfterPctlFilt', 
                caption = 'Filter troughs based on local percentile of raw data.')
        else:
           graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
                OutDir = OutDir, prefix = dataType + 'AdjustPeaksAfterPctlFilt', 
                caption = 'Filter peaks based on percentile of raw data.')

    return peaks

def localPercentileFilter(peaks, rawData, percentile, period=None, numPeriods=4, upperThreshold=False, 
            graph = False, phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        localPercentileFilter
            Filter peaks based on local percentile of raw data
            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        localPercentileFilter(peaks, rawData, percentile, period=None, numPeriods=4, upperThreshold=False, 
                    graph = False, phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None)
        ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        percentile: (dType = float) Minimum (or maximum) percentile of raw data for a peak 
                            value to imply a valid peak (or trough)
                            
        period: (dType = NoneType) Overall typical period of raw data in time series index units.
                Default is none, meaning the period is determined from the raw data.
        
        numPeriods: (dType = int) Number of periods defining the local neighborhood over which
                            the percentile is determined.  Default is 4.
                            
        upperThreshold: (dType = bool) Whether the threshold is the maximum acceptable value.  
                        Default is False meaning it is the minimum acceptable value
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
        return peaks
    
    # Estimate period from raw data if not supplied
    if not period:
        period = getTimeSeriesPeriod(rawData)
  
    # Get peak values
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
    
    # Determine local percentil-based threshold around each peak
    halfWindowWidth = round(period * numPeriods/2)
    upperLimit = len(rawData) - 1
    thresholds = []
    for peak in peaks:
        Min = max(0,peak - halfWindowWidth)
        Max = min(upperLimit,peak + halfWindowWidth)
        thresholds.append(np.percentile([x for x in rawData[Min:Max] if math.isnan(x) == False], percentile))

    # Apply local percentile filter
    if upperThreshold: peaks = peaks[np.array(peakVals) <= np.array(thresholds)]
    else: peaks = peaks[np.array(peakVals) >= np.array(thresholds)]
            
    # Graph (and save) results as required
    if graph and phys_fs:
       graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
            OutDir = OutDir, prefix = dataType + 'AdjustPeaksAfterLocalPctlFilt', 
            caption = 'Filter peaks based on local percentile of raw data.')
       
    return peaks


def getTimeSeriesPeriod(rawData, minFrequency=1):
     """
     NAME
         getTimeSeriesPeriod
             Get overall typical period (s) of raw data in time series index units          
      TYPE
          <class 'numpy.float64'>
     SYNOPSIS
         getTimeSeriesPeriod(rawData)
     ARGUMENTS
         rawData: (array dType = float64) Raw cardiac data
         
         minFrequency: (dType = int) Minimum frequency (Hz) to be considered.  Default = 1
     AUTHOR
         Peter Lauren
     """
    
     # Note that nan values are removed from the input raw values
     limit = round(len(rawData)/2) # Frequency limit is Nyquist frequency
     validRawData = [x for x in rawData if math.isnan(x) == False] # Raw data with nan's removed
     FourierSpectrum = abs(np.fft.fft(validRawData))
     selectedFourerSpectrum = FourierSpectrum[minFrequency:limit]
     F0 = len(rawData)  # Length of Fourier spectrum
     frequencyModeIndex = minFrequency+np.argmax(selectedFourerSpectrum)
     return  F0/frequencyModeIndex

 
def removePeaksCloseToHigherPointInRawData(peaks, rawData, direction='right', 
        portion=0.25, period=None, graph = False, 
        phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        removePeaksCloseToHigherPointInRawData
            Remove peaks with values less than the maximum raw input data in a 
            specified portion of the overall period of the input data, from the 
            peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removePeaksCloseToHigherPointInRawData(peaks, rawData, direction='right', portion=0.25, period=None)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        direction: (dType = str) Direction to look for higher point in raw data.  Options are
            'right' or 'left'.  Default = 'right'.  That is, it aims to remove 
            minor peaks on the rising side of a larger peak
                            
        portion: (dType = float) Portion of period that defines the offset from the peak.  Default
            is 0.25.  That is a quarter of the period
            
        period: (dType = numpy.float64) Overall period, in seconds,  of the raw data if known.  
            The default is that this is not supplied and is estimated by the function
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if len(peaks) == 0:
        print('*** Error in removePeaksCloseToHigherPointInRawData: Peaks array empty')
        return peaks
    
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    threshold = -(max(rawData) - min(rawData)) * 0.1
        
    searchLength = round(period * portion)
    searchLength = min(searchLength, peaks[0] - 1)
    searchLength = min(searchLength, len(rawData) - peaks[-1] - 1)
    diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) for x in peaks]
    if len(diff) > 0: peaks = peaks[diff >= np.float64(threshold)]
            
    # Graph (and save) results as required
    if graph and phys_fs:
        if direction=='right':
            prefix = dataType + 'RemoveUpsideFalsePeaks'
            caption = 'Remove "peaks" that are less than the raw input a quarter of a period on right side.'
        else:
            prefix = dataType + 'RemoveDownsideFalsePeaks'
            caption = 'Remove "peaks" that are less than the raw input a quarter of a period on left side.'
        graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
            OutDir = OutDir, prefix = prefix, 
            caption = caption)
    
    return peaks

def removeTroughsCloseToLowerPointInRawData(troughs, rawData, direction='right', 
        portion=0.25, period=None, graph = False, 
        phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        removeTroughsCloseToLowerPointInRawData
            Remove troughs with values greater than the minimum raw input data in a 
            specified portion of the overall period of the input data, from the 
            peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removeTroughsCloseToLowerPointInRawData(troughs, rawData, direction='right', portion=0.25, period=None)
    ARGUMENTS
        troughs:   (array dType = numpy.int64) Array of trough locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        direction: (dType = str) Direction to look for lower point in raw data.  Options are
            'right' or 'left'.  Default = 'right'.  That is, it aims to remove 
            minor troughs on the falling side of a deeper trough
                            
        portion: (dType = float) Portion of period that defines the offset from the peak.  Default
            is 0.25.  That is a quarter of the period
            
        period: (dType = numpy.float64) Overall period of the raw data if known.  The default is that this
            is not supplied and is estimated by the function
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
    
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    searchLength = round(period/4)
    searchLength = min(searchLength, troughs[0] - 1)
    searchLength = min(searchLength, len(rawData) - troughs[-1] - 1)
    diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) for x in troughs]
    if len(diff) > 0: troughs = troughs[diff <= np.float64(0)]
            
    # Graph (and save) results as required
    if graph and phys_fs:
        if direction=='right':
            caption = 'Remove downstroke false troughs.'
            prefix = dataType + 'RemoveDpwnstrokeFalseTroughsFilt'
        else: 
            caption = 'Remove upstroke false troughs.'
            prefix = dataType + 'RemoveUpstrokeFalseTroughsFilt'
        graphPeaksAgainstRawInput(rawData, [], phys_fs, dataType, troughs = troughs,
             OutDir = OutDir, prefix = prefix, caption = caption)
    
    return troughs

def removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData, denominator=4.0, graph = False, 
            phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        removePeaksCloserToLocalMinsThanToAdjacentPeaks
            Remove peaks with values less than the maximum raw input data in a 
            specified portion of the overall period of the input data, from the 
            peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData, denominator=4.0)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        denominator: (dType = float) Number by which to divide the current amplitude to determine 
        the lower threshold of the accepitable peak value
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
        return peaks

    # Get peak values
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
           
    # Remove peaks that are less than a quarter as far from the local minimum to the adjacent peaks
    valleys = [((j-i)+(j-k))/2 for i, j, k in zip(peakVals[:-1], peakVals[1:], peakVals[2:])]
    fromLocalMin = [j-min(rawData[i:k]) for i, j, k in zip(peaks[:-1], peakVals[1:], peaks[2:])]
    ratios = [i/j for i,j in zip(valleys,fromLocalMin)]
    ratios.insert(0,0)
    ratios.append(0)
    threshold = np.float64(-denominator)
    
    peaks = peaks[ratios>threshold]
            
    # Graph (and save) results as required
    if graph and phys_fs:
       graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
            OutDir = OutDir, prefix = dataType + 'RemovePeaksCloseToMinimum', 
            caption = 'Remove peaks that are less than a quarter as far from the local minimum to the adjacent peaks.')
    
    return peaks

def removeTroughsCloserToLocalMaxsThanToAdjacentTroughs(troughs, rawData, denominator=4.0, 
        graph = False, phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        removeTroughsCloserToLocalMaxsThanToAdjacentTroughs
            Remove troughs with values greater than the minimum raw input data in a 
            specified portion of the overall period of the input data, from the 
            trough, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removeTroughsCloserToLocalMaxsThanToAdjacentTroughs(troughs, rawData, denominator=4.0)
    ARGUMENTS
        troughs:   (array dType = numpy.int64) Array of trough locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        denominator: (dType = float) Number by which to divide the current amplitude to determine 
        the upper threshold of the accepitable trough value
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')

    # Get trough values
    troughVals = []
    for i in troughs: troughVals.append(rawData[i])
           
    # Remove troughs that are less than a quarter as far from the local maximum to the adjacent troughs
    watersheds = [((j-i)+(j-k))/2 for i, j, k in zip(troughVals[:-1], troughVals[1:], troughVals[2:])]
    fromLocalMax = [j-max(rawData[i:k]) for i, j, k in zip(troughs[:-1], troughVals[1:], troughs[2:])]
    ratios = [i/j for i,j in zip(watersheds,fromLocalMax)]
    ratios.insert(0,0)
    ratios.append(0)
    threshold = np.float64(4.0)
    troughs = troughs[ratios<threshold]
            
    # Graph (and save) results as required
    if graph and phys_fs:
           graphPeaksAgainstRawInput(rawData, [], phys_fs, dataType, troughs = troughs,
                OutDir = OutDir, prefix = dataType + 'removeTroughsCloserToLocalMaxThanAdjacentTroughs', 
                caption = 'Remove troughs closer to local max than to adjacent troughs.')
    
    return troughs

def estimateSamplingFrequencyFromRawData(rawData, expectedCyclesPerMinute=70):
    """
    NAME
        estimateSamplingFrequencyFromRawData
            Estimate sampling frequency (Hz) from raw data based on expected cycles per minute            
     TYPE
         <class 'numpy.float64'>
    SYNOPSIS
        estimateSamplingFrequencyFromRawData(rawData, expectedCyclesPerMinute)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        expectedCyclesPerMinute: (dType = int) Expected number of cycles (heart beats, breaths)
            per minute based on input data type
    AUTHOR
        Peter Lauren
    """
    
    return (getTimeSeriesPeriod(rawData)*60)/expectedCyclesPerMinute

def removeOverlappingPeaksAndTroughs(peaks, troughs, rawData, graph = False, 
            phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        removeOverlappingPeaksAndTroughs
            Identify peaks that are also troughs and decide what each one should be
            (one or the other).
     TYPE
         <class 'numpy.float64'>, <class 'numpy.float64'>
    SYNOPSIS
        removeOverlappingPeaksAndTroughs(peaks, troughs, rawData)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        troughs:   (array dType = numpy.int64) Array of trough locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    RETURNS
        filtered peaks, filtered troughs
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
        
    numPeaks = len(peaks)
    lastPeak = numPeaks - 1
    numTroughs = len(troughs)
    lastTrough = numTroughs - 1
    
    peaksToDelete = []
    troughsToDelete = []
    for peakIndex in range(0,numPeaks):
        if peaks[peakIndex] in troughs:
            troughIndex = np.argwhere(troughs == peaks[peakIndex])[0]
            if (peakIndex > 0 and troughIndex > 0) or peakIndex == lastPeak or\
                    troughIndex == lastTrough: 
                if peaks[peakIndex - 1] > troughs[troughIndex - 1]:
                    peaksToDelete.append(peakIndex)
                else: troughsToDelete.append(troughIndex)
            else: 
                if peaks[peakIndex + 1] < troughs[troughIndex + 1]:
                    peaksToDelete.append(peakIndex)
                else: troughsToDelete.append(troughIndex)
                
    peaks = np.delete(peaks,peaksToDelete)
    troughs = np.delete(troughs,troughsToDelete)
    
    if graph and phys_fs:
        graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, troughs = troughs,
             OutDir = OutDir, prefix = dataType + 'removeOverlappingPeaksAndTroughs', 
             caption = 'Remove overlapping peaks and troughs.')
                
    return peaks, troughs

def removeExtraInterveningPeaksAndTroughs(peaks, troughs, rawData):
    """
    NAME
        removeExtraInterveningPeaksAndTroughs
            Ensure there is only one peak between each pair of troughs and only 
            one trough between each pair of peaks
     TYPE
         <class 'numpy.float64'>, <class 'numpy.float64'>
    SYNOPSIS
        removeExtraInterveningPeaksAndTroughs(peaks, troughs, rawData)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        troughs:   (array dType = numpy.int64) Array of trough locations in raw data indices.
        
        rawData: (array, dType = float) Raw input data
    RETURNS
        filtered peaks, filtered troughs
    AUTHOR
        Peter Lauren
    """
    
    # Get interpeak intervals
    peakRanges = [range(x,y) for x,y in zip(peaks[0:], peaks[1:])]
    
    # Count troughs in interpeak intervals
    interpeakTroughs = []
    for Range in peakRanges:
        interpeakTroughs.append(len([i for i in range(len(troughs)) if troughs[i] in Range]))
        
    # Identify interpeak intervals with more than one trough
    crowdedIntervals = list(np.where(np.array(interpeakTroughs)>1)[0])
    crowdedIntervals.insert(0,-1)   # Prepend dummy value so indices can run backwards an include 0
    
    # Keep only the deepest trough in interpeak intervals
    for interval in crowdedIntervals[-1:0:-1]:
        troughGroup = [i for i in range(0,len(troughs)) if troughs[i] in peakRanges[interval]]
        keep = np.argmin([rawData[i] for i in troughGroup])
        troughGroup.remove(troughGroup[keep])
        troughs = np.delete(troughs,troughGroup)
    
    # Get intertrough intervals
    troughRanges = [range(x,y) for x,y in zip(troughs[0:], troughs[1:])]
    
    # Count peaks in intertrough intervals
    intertroughPeaks = []
    for Range in troughRanges:
        intertroughPeaks.append(len([i for i in range(len(peaks)) if peaks[i] in Range]))
        
    # Identify intertrough intervals with more than one peak
    crowdedIntervals = list(np.where(np.array(intertroughPeaks)>1)[0])
    crowdedIntervals.insert(0,-1)   # Prepend dummy value so indices can run backwards an include 0
    
    # Keep only the hioghest peak in interpeak intervals
    for interval in crowdedIntervals[-1:0:-1]:
        peakGroup = [i for i in range(0,len(peaks)) if peaks[i] in troughRanges[interval]]
        keep = np.argmax([rawData[i] for i in peakGroup])
        peakGroup.remove(peakGroup[keep])
        peaks = np.delete(peaks,peakGroup)
        
    return peaks, troughs

def removeClosePeaks(peaks, period, rawData, Troughs = False, denominator=4, graph = False, 
            phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        removeClosePeaks
            Remove peaks (or troughs) that are closer than period/demonominator
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removeClosePeaks(peaks, period, denominator=4)
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data indices.
        
        period:   (dType = numpy.float64) Overall typical period of raw data in time series index units.
        
        Troughs:  (dType = bool) Whether processing troughs instead of peaks
        
        denominator: (dType = int) Number by which to divide the period in order to determine
        the minimum acceptable separation
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    RETURNS
        filtered peaks (or troughs)
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
        return peaks
    
    # Make and filter inter-peak itervals
    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    threshold = period/4
    last = len(intervals) - 1
    for i in range(last,0,-1):
        if (intervals[i] < threshold):
            intervals[i-1] = intervals[i-1] + intervals[i]
            del intervals[i]
            
    # Make peaks from intervals
    offset = 0 
    peaks = []
    threshold = round(threshold)
    if Troughs: # Processing troughs instead of peaks
        for interval in intervals:
            peaks.append(offset + np.argmin(rawData[offset:offset+interval]))
            offset = offset + interval
        peaks.append(offset + np.argmin(rawData[offset:]))
   
        # Adjust troughs from uniform spacing
        peaks = np.array(peaks)
        for i in range(0,2):
            peaks = refinePeakLocations(peaks, rawData, period = period, Troughs = True)
    else:   # Processing peaks
        for interval in intervals:
            # peaks.append(offset + np.argmax(rawData[offset:offset+interval]))
            peaks.append(offset+interval)
            offset = offset + interval
   
        # Adjust peaks from uniform spacing
        peaks = np.array(peaks)
        for i in range(0,2):
            peaks = refinePeakLocations(peaks, rawData, period = period)
            
    # Convert peaks back to numpy array and remove duplicates that may result from refining the locations
    peaks = np.unique(np.array(peaks))
    
    # Convert peaks to numpy array
    peaks = np.array(peaks)
            
    # Graph (and save) results as required
    if graph and phys_fs:
        if Troughs:
           graphPeaksAgainstRawInput(rawData, [], phys_fs, dataType, troughs = peaks,
                OutDir = OutDir, prefix = dataType + 'MergeCloseTroughs', 
                caption = 'Merge troughs that are closer than one quarter of the overall typical period.')
        else:
           graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
                OutDir = OutDir, prefix = dataType + 'MergeClosePeaks', 
                caption = 'Merge peaks that are closer than one quarter of the overall typical period.')
    
    return peaks

def bandPassFilterRawDataAroundDominantFrequency(rawData, minBeatsPerSecond, 
        phys_fs, graph = False, dataType = "Cardiac", saveGraph = False, OutDir = None) :
    """
    NAME
        bandPassFilterRawDataAroundDominantFrequency
            Band pass filter raw data based on overall typical period of raw data 
            in time series index units.            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        bandPassFilterRawDataAroundDominantFrequency(rawData, graph = True, 
                prefix = 'BPFilteredCardiacInput', OutDir = '.') 
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        minBeatsPerSecond: (dType = float) Minimum expected beats per second
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Required if graph True
        
        graph:   (dType = bool) Whether to graph the results
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """

    # Remove NaNs from raw data
    rawData = [x for x in rawData if math.isnan(x) == False]
    
    rawDataLength = len(rawData)
    
    # Determine harmonic frequency unit
    F0 = phys_fs/rawDataLength
    
    # Get lower cutoff index
    lowerCutoffIndex = round(minBeatsPerSecond/F0)
    
    # Get Fourier transform
    FourierTransform = np.fft.fft(rawData)
    
    # Determine frequency peak
    frequencyPeak = np.argmax(abs(np.fft.fft(rawData))[lowerCutoffIndex:round(rawDataLength/2)])+lowerCutoffIndex
    print('Frequency peak: ' + str(F0 * frequencyPeak) + ' Hz')
        
    # Determine band limits
    lowerMin = round(frequencyPeak/2)
    lowerMax = round(1.5*frequencyPeak)
    upperMin = rawDataLength - lowerMax
    upperMax = rawDataLength - lowerMin
    
    # Zero part of FT outside limits
    filterArray = np.zeros(rawDataLength)
    filterArray[lowerMin:lowerMax] = 1
    filterArray[upperMin:upperMax] = 1
    filteredFT = FourierTransform * filterArray
    
    # Get IFT
    filteredRawData = np.real(np.fft.ifft(filteredFT))
    
    if graph:
        # Show selected part of Fourier transform
        x = []  
        rawData1 = abs(FourierTransform)
        filteredrawData1 = filterArray
        end = len(rawData1)
        for i in range(0,end): x.append(i*F0)
        fig, ax_left = mpl.pyplot.subplots()
        mpl.pyplot.xlabel("Frequency (Hz)")
        mpl.pyplot.ylabel('Fourier Spectral Value',color='g')
        ax_right = ax_left.twinx()
        ax_right.plot(x[3:end//20], filteredrawData1[3:end//20], color='red')
        ax_left.plot(x[3:end//20],rawData1[3:end//20], color='green')
        mpl.pyplot.ylabel('Filter',color='r')
        mpl.pyplot.title("Selected part of the Fourier Sprctrum")
        
        # Save plot to file
        if saveGraph:
            prefix = dataType + 'SelectedFourierTransformPart'
            mpl.pyplot.savefig('%s/%s.pdf' % (OutDir, prefix)) 
            mpl.pyplot.show()
    
        # Plot filtered signal agains raw data
        x = []    
        end = len(filteredRawData)
        for i in range(0,end): x.append(i/phys_fs)
        fig, ax_left = mpl.pyplot.subplots()
        mpl.pyplot.xlabel("Time (s)")
        mpl.pyplot.ylabel('Raw input data value',color='g')
        ax_right = ax_left.twinx()
        ax_right.plot(x[2000:4000], filteredRawData[2000:4000], color='red')
        ax_left.plot(x[2000:4000],rawData[2000:4000], color='green')
        # ax_right.plot(x, filteredRawData, color='red')
        # ax_left.plot(x,rawData, color='green')
        mpl.pyplot.ylabel('Filtered Data Value',color='r')
        mpl.pyplot.title("BP Filtered [" + str(round(F0*lowerMin)) + ":" +\
            str(round(F0*lowerMax)) + "] (red) and raw input data (green)")
            
        # Save plot to file
        if saveGraph:
            prefix = dataType + 'BPF_VRawInput'
            mpl.pyplot.savefig('%s/%s.pdf' % (OutDir, prefix)) 
            
        mpl.pyplot.show()
        
    return filteredRawData

def refinePeakLocations(peaks, rawData, period = None, Troughs = False, graph = False, 
            phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        refinePeakLocations
            Adjust peaks to correspond to local maxima.  This is usually necessary if the 
            peaks were.Determined from a band-pass filtered version of the inut raw data where
            the peaks are uniformly spaced based on the overall typical period of raw data
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        refinePeakLocations(peaks, rawData, period = None) 
    ARGUMENTS
        peaks: (array dType = int64) Array of peaks to be refined
        
        rawData: (array, dType = float) Raw input data
        
        period: (dType = NoneType) Overall typical period of raw data in time series index units.
                Default is none, meaning the period is determined from the raw data.
                
        Troughs: (dType = bool) Whether troughs are processed instead of peaks
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
        return peaks
    
    # Find period if not supplied
    if not period:
        period = getTimeSeriesPeriod(rawData)

    # Determine half window width
    halfWindowWidth = round(period/4)
    
    # Determine offsets
    arrayLength = len(rawData)
    offsets = []
    if Troughs:
        for peak in peaks:
            start = max(0, peak-halfWindowWidth)
            finish = min(peak+halfWindowWidth, arrayLength-1)
            localOffset = peak - start
            offsets.append(np.argmin(rawData[start:finish])-localOffset)
    else:
        for peak in peaks:
            start = max(0, peak-halfWindowWidth)
            finish = min(peak+halfWindowWidth, arrayLength-1)
            localOffset = peak - start
            offsets.append(np.argmax(rawData[start:finish])-localOffset)
            
    # Adjust peak locations
    peaks = peaks + offsets
            
    # Graph (and save) results as required
    if graph and phys_fs:
       graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
            OutDir = OutDir, prefix = dataType + 'AdjustPeaksFromUniformSpacing', 
            caption = 'Adjust peaks from uniform spacing.')
           
    # Apply offsets
    return peaks

def addMissingPeaks(peaks, rawData, period=None, graph = False, phys_fs = None, 
                    dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        addMissingPeaks
            Find and fill gaps in period series of peaks
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        addMissingPeaks(peaks, rawData, period=None, graph = False, phys_fs = None, 
                            dataType = "Cardiac", saveGraph = False, OutDir = None) 
    ARGUMENTS
        peaks: (array dType = int64) Array of peaks to be refined
        
        rawData: (array, dType = float) Raw input data
        
        period: (dType = NoneType) Overall typical period of raw data in time series index units.
                Default is none, meaning the period is determined from the raw data.
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
        return peaks
    
    # Find period if not supplied
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    # Find interpeak intervals
    intervals = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
    factor = np.median(intervals)*0.9
    peaksToAdd = [round(intervals[i]/factor)-1 for i in range(0,len(intervals))]
    for i in range(len(intervals)-1,-1,-1):
        if (peaksToAdd[i]>0):
            additionPlus1 = peaksToAdd[i] + 1
            start = peaks[i]
            end = peaks[i+1]
            increment = (end-start)/additionPlus1
            peaks = np.insert(peaks, i+1, [start+increment*j for j in range(1,additionPlus1)])
   
    # Adjust peaks from uniform spacing
    peaks = refinePeakLocations(peaks, rawData, period = period)
            
    # Graph (and save) results as required
    if graph and phys_fs:
       graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, 
            OutDir = OutDir, prefix = dataType + 'AdjustPeaksAfterLocalPctlFilt', 
            caption = 'Filter peaks based on local percentile of raw data.')
    
    return peaks


def addMissingPeaksAndTroughs(peaks, troughs, rawData, period=None, graph = False, 
            phys_fs = None, dataType = "Cardiac", saveGraph = False, OutDir = None):
    """
    NAME
        addMissingPeaksAndTroughs
            Find and fill gaps in period series of peaks and troughs
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        addMissingPeaksAndTroughs(peaks, troughs, rawData, period=None) 
    ARGUMENTS
        peaks: (array dType = int64) Array of peaks to be refined
        
        troughs: (array dType = int64) Array of troughs to be refined
        
        rawData: (array, dType = float) Raw input data
        
        period: (dType = NoneType) Overall typical period of raw data in time series index units.
                Default is none, meaning the period is determined from the raw data.
        
        graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if graph and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied if graphing required')
    
    # Find period if not supplied
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    # Find interpeak intervals
    intervals = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
    factor = np.median(intervals)*0.9
    peaksToAdd = [round(intervals[i]/factor)-1 for i in range(0,len(intervals))]
    for i in range(len(intervals)-1,-1,-1):
        if (peaksToAdd[i]>0):
            additionPlus1 = peaksToAdd[i] + 1
            start = peaks[i]
            end = peaks[i+1]
            increment = (end-start)/additionPlus1
            peaks = np.insert(peaks, i+1, [start+increment*j for j in range(1,additionPlus1)])
        
    # Find intertrough intervals
    intervals = [x - troughs[i - 1] for i, x in enumerate(troughs)][1:]
    factor = np.median(intervals)*0.9
    troughsToAdd = [round(intervals[i]/factor)-1 for i in range(0,len(intervals))]
    for i in range(len(intervals)-1,-1,-1):
        if (troughsToAdd[i]>0):
            additionPlus1 = troughsToAdd[i] + 1
            start = troughs[i]
            end = troughs[i+1]
            increment = (end-start)/additionPlus1
            troughs = np.insert(troughs, i+1, [start+increment*j for j in range(1,additionPlus1)])
   
    # Adjust peaks from uniform spacing
    peaks = refinePeakLocations(peaks, rawData, period = period/2)
    troughs = refinePeakLocations(troughs, rawData, period = period/2, Troughs = True)
    
    # Remove extra peaks bewteen troughs and troughs between peaks
    peaks, troughs = removeExtraInterveningPeaksAndTroughs(peaks, troughs, rawData)
    
    if graph and phys_fs:
        graphPeaksAgainstRawInput(rawData, peaks, phys_fs, dataType, troughs = troughs,
             OutDir = OutDir, prefix = dataType + 'addMissingPeaksAndTroughs', 
             caption = 'Add missing peaks and troughs.')
    
    return peaks, troughs

def graphPeaksAgainstRawInput(rawData, peaks, phys_fs, peakType, troughs = [], 
        OutDir = None, prefix = 'cardiacPeaks', caption = []):
    '''
    NAME
        graphPeaksAgainstRawInput
        Graph graph raw input data along with peaks and optionally troughs
     TYPE
         <void>
    SYNOPSIS
        graphPeaksAgainstRawInput(rawData, peaks, parameters, peakType, troughs = [], 
                                      OutDir = None, display = True) 
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        peaks: (array dType = int64) Array of peaks to be refined
        
        phys_fs: (dType = float) Sampling frequency in Hz
                
        peakType: (dType = str) String that defines the type of data.  May be "Cardiac" or "Respiratory"
        
        troughs: (array dType = int64) Array of troughs to be refined. Not used if plotting troughs not required
        
        OutDir: (dType = str) String defining the directory to which the  graph is written.  
            Not used if it is not required to save the graph to disk.
    AUTHOR
        Peter Lauren
    '''
    
    # Graph respiratory peaks and troughs against respiratory time series
    x = []    
    end = len(rawData)
    for i in range(0,end): x.append(i/phys_fs)
    mpl.pyplot.subplot(211)
    mpl.pyplot.plot(x, rawData, "g") #Lines connecting peaks and troughs
    if len(peaks) > 0:
        peakVals = []
        for i in peaks: peakVals.append(rawData[i])
        mpl.pyplot.plot(peaks/phys_fs, peakVals, "ro") # Peaks
    if len(troughs) > 0:
        troughVals = []
        for i in troughs: troughVals.append(rawData[i])
        mpl.pyplot.plot(troughs/phys_fs, troughVals, "bo") # troughs
        if len(peaks) > 0:
            title = peakType + " peaks (red), troughs (blue) and raw input data (green)"
        else:
            title = peakType + " troughs (blue) and raw input data (green)"
    else:
        title = peakType + " peaks (red) and raw input data (green)"
    title += '\n' + caption
    mpl.pyplot.xlabel("Time (s)")
    mpl.pyplot.ylabel("Input data value")
    mpl.pyplot.title(title, fontdict={'fontsize': 10})
    mpl.text.Text(.5, .05, caption, ha='center')
         
    # Save plot to file
    if OutDir:
        mpl.pyplot.savefig('%s/%s.pdf' % (OutDir, prefix)) 
        mpl.pyplot.show()  # If this is left out, output file is blank

def checkForNans(rawData, dataType):
    
    if (np.isnan(np.sum(rawData))): # If nan's in raw data
        print('** WARNING. NaN entries at the following indices of the ' + dataType + ' data')
        nanIndices = np.argwhere(np.isnan(rawData))
        print(nanIndices)
        print('NaN values')
        print(rawData[nanIndices])
        
        # Try to replace each nan with adjacent valid value(s) 
        if (nanIndices[0] == 0):    # If first element a nan
            rawData[0] = rawData[bisect.bisect_left(nanIndices, -1)]
        if (nanIndices[-1] == len(rawData) - 1):    # If last element a nan
            rawData[-1] = rawData[bisect.bisect_right(nanIndices, nanIndices[-1]-1)]
        for i in nanIndices: # Process all Nans, replacing each one with previous value
            rawData[i] = rawData[i-1]
        
    return rawData
        

        
