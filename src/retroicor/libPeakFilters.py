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

def percentileFilter(peaks, rawData, percentile, upperThreshold=False):
    """
    NAME
        percentileFilter
            Filter peaks based on percentile of raw data
            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        getCardiacPeaks(peaks, rawData, percentile, upperThreshold=False)
    ARGUMENTS
        peaks:   Array of peak locations in raw data indices.
        
        rawData: Raw input data
        
        percentile: Minimum (or maximum) percentile of raw data for a peak 
                            value to imply a valid peak (or trough)
                            
        upperThreshold: Whether the threshold is the maximum acceptable value.  
                        Default is False meaning it is the minimum acceptable value
    AUTHOR
        Peter Lauren
    """
  
    # Get peak values
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
    
    # Remove peaks that are less than the the required percentile of the input signal
    # Note that any nan values are first filtered out of the input signal
    threshold = np.percentile([x for x in rawData if math.isnan(x) == False], percentile)
    if upperThreshold: return peaks[peakVals <= threshold]
    return peaks[peakVals >= threshold]

def localPercentileFilter(peaks, rawData, percentile, period=None, numPeriods=4, upperThreshold=False):
    """
    NAME
        localPercentileFilter
            Filter peaks based on local percentile of raw data
            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        getCardiacPeaks(peaks, rawData, percentile, period=None, numPeriods=4, upperThreshold=False)
    ARGUMENTS
        peaks:   Array of peak locations in raw data indices.
        
        rawData: Raw input data
        
        percentile: Minimum (or maximum) percentile of raw data for a peak 
                            value to imply a valid peak (or trough)
                            
        period: Overall typical period of raw data in time series index units.
                Default is none, meaning the period is determined from the raw data.
        
        numPeriods: Number of periods defining the local neighborhood over which
                            the percentile is determined.  Default is 4.
                            
        upperThreshold: Whether the threshold is the maximum acceptable value.  
                        Default is False meaning it is the minimum acceptable value
    AUTHOR
        Peter Lauren
    """
    
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
    if upperThreshold: return peaks[np.array(peakVals) <= np.array(thresholds)]
    return peaks[np.array(peakVals) >= np.array(thresholds)]

def getTimeSeriesPeriod(rawData):
     """
     NAME
         getTimeSeriesPeriod
             Get overall typical period of raw data in time series index units 
         
      TYPE
          <class 'numpy.float64'>
     SYNOPSIS
         getTimeSeriesPeriod(rawData)
     ARGUMENTS
         rawData: Raw cardiac data
     AUTHOR
         Peter Lauren
     """
    
     # Note that nan values are removed from the input raw values
     limit = round(len(rawData)/2)
     return len(rawData)/(1+np.argmax((abs(np.fft.fft([x for x in rawData if math.isnan(x) == False]))[1:limit])))
 
def removePeaksCloseToHigherPointInRawData(peaks, rawData, direction='right', 
                                           portion=0.25, period=None):
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
        peaks:   Array of peak locations in raw data indices.
        
        rawData: Raw input data
        
        direction: Direction to look for higher point in raw data.  Options are
            'right' or 'left'.  Default = 'right'.  That is, it aims to remove 
            minor peaks on the rising side of a larger peak
                            
        portion: Portion of period that defines the offset from the peak.  Default
            is 0.25.  That is a quarter of the period
            
        period: Overall period of the raw data if known.  The default is that this
            is not supplied and is estimated by the function
    AUTHOR
        Peter Lauren
    """
    
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    searchLength = round(period * portion)
    searchLength = min(searchLength, peaks[0] - 1)
    searchLength = min(searchLength, len(rawData) - peaks[-1] - 1)
    diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) for x in peaks]
    if len(diff) > 0: peaks = peaks[diff >= np.float64(0)]
    
    return peaks

def removeTroughsCloseToLowerPointInRawData(troughs, rawData, direction='right', 
                                           portion=0.25, period=None):
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
        troughs:   Array of trough locations in raw data indices.
        
        rawData: Raw input data
        
        direction: Direction to look for lower point in raw data.  Options are
            'right' or 'left'.  Default = 'right'.  That is, it aims to remove 
            minor troughs on the falling side of a deeper trough
                            
        portion: Portion of period that defines the offset from the peak.  Default
            is 0.25.  That is a quarter of the period
            
        period: Overall period of the raw data if known.  The default is that this
            is not supplied and is estimated by the function
    AUTHOR
        Peter Lauren
    """
    
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    searchLength = round(period/4)
    searchLength = min(searchLength, troughs[0] - 1)
    searchLength = min(searchLength, len(rawData) - troughs[-1] - 1)
    diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) for x in troughs]
    if len(diff) > 0: troughs = troughs[diff <= np.float64(0)]
    
    return troughs

def removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData, denominator=4.0):
    """
    NAME
        removePeaksCloserToLocalMinsThanToAdjacentPeaks
            Remove peaks with values less than the maximum raw input data in a 
            specified portion of the overall period of the input data, from the 
            peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removePeaksCloseToHigherPointInRawData(peaks, rawData, direction='right', 
        portion=0.25)
    ARGUMENTS
        peaks:   Array of peak locations in raw data indices.
        
        rawData: Raw input data
        
        denominator: Number by which to divide the current amplitude to determine 
        the lower threshold of the accepitable peak value
    AUTHOR
        Peter Lauren
    """

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
    return peaks[ratios>threshold]

def removeTroughsCloserToLocalMaxsThanToAdjacentTroughs(troughs, rawData, denominator=4.0):
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
        peaks:   Array of trough locations in raw data indices.
        
        rawData: Raw input data
        
        denominator: Number by which to divide the current amplitude to determine 
        the upper threshold of the accepitable trough value
    AUTHOR
        Peter Lauren
    """

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
    return troughs[ratios<threshold]

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
        peaks:   Array of peak locations in raw data indices.
        
        expectedCyclesPerMinute: Expected number of cycles (heart beats, breaths)
            per minute based on input data type
    AUTHOR
        Peter Lauren
    """
    
    return (getTimeSeriesPeriod(rawData)*60)/expectedCyclesPerMinute

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
        peaks:   Array of peak locations in raw data indices.
        
        troughs:   Array of trough locations in raw data indices.
        
        rawData: Raw input data
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

def removeClosePeaks(peaks, period, denominator=4):
    """
    NAME
        removeClosePeaks
            Remove peaks (or troughs) that are closer than period/demonominator
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        removeClosePeaks(peaks, period, denominator=4)
    ARGUMENTS
        peaks:   Array of peak locations in raw data indices.
        
        period:   Overall typical period of raw data in time series index units.
        
        denominator: Number by which to divide the period in order to determine
        the minimum acceptable separation
    RETURNS
        filtered peaks (or troughs)
    AUTHOR
        Peter Lauren
    """
    
    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    intervals.insert(0,round(period))
    threshold = period/4
    return peaks[intervals>=threshold]

def bandPassFilterRawDataAroundDominantFrequency(rawData, graph = True, 
        prefix = 'BPFilteredCardiacInput', OutDir = '.') :
    
    rawDataLength = len(rawData)
    
    # Get Fourier transform
    FourierTRansform = np.fft.fft(rawData)
    
    # Determine frequency peak
    frequencyPeak = np.argmax(abs(np.fft.fft(rawData))[0:round(rawDataLength/2)])
    
    # Determine band limits
    lowerMin = round(frequencyPeak/2)
    lowerMax = round(1.5*frequencyPeak)
    upperMin = rawDataLength - lowerMax
    upperMax = rawDataLength - lowerMin
    
    # Zero part of FT outside limits
    filterArray = np.zeros(len(rawData))
    filterArray[lowerMin:lowerMax] = 1
    filterArray[upperMin:upperMax] = 1
    filteredFT = FourierTRansform * filterArray
    
    # Get IFT
    filteredRawData = np.fft.ifft(filteredFT)
    
    # Plot result agains raw data
    if graph:
        fig, ax_left = mpl.pyplot.subplots()
        mpl.pyplot.xlabel("Time Points")
        mpl.pyplot.ylabel('Input data input value',color='g')
        ax_right = ax_left.twinx()
        ax_right.plot(filteredRawData, color='red')
        ax_left.plot(rawData, color='green')
        mpl.pyplot.ylabel('Filtered Data Value',color='r')
        mpl.pyplot.title("BP Filtered (red) and raw input data (green)")
            
        # Save plot to file
        mpl.pyplot.savefig('%s/%s.pdf' % (OutDir, prefix)) 
        mpl.pyplot.show()
        
    return filteredRawData
        

    

    
