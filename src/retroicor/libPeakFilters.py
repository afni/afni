#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 25 14:46:15 2022

@author: peterlauren

Peak detection is as follows.
1. Non-numeric (NaN) data in the input is identified and the gap replaced by a
   inear interpolation from the adjacent valid entries.
2. The remaining data is bandpass filtered as follows.
    a. The data is Fourier transformed to FT.
    b. The harmonic frequency unit, F0, is determined as the sampling frequency 
       (Hz) divided by the raw data length.
    c. The lower cutoff index, fmin, is the rounded quotient of the postulated 
       minimum number of beats, or breaths, per second divided by F0.
    d. Find the peak, indexed fp, between fmin and the Nyquist frequency.
    e. Find the bounds, around the peak, based on -3 dB limits.  The lower bound
       is constrained to be at ≤ fp/2 and the upper bound constrained to be ≥ 
       1.5fp.
    f. FT⟶0 outside these bounds but unchanged within.
    g. An inverse FT obtains the BP filtered signal.
3. Initial peaks found using the Python function scipy.signal.findpeaks with a 
       (smoothing window) width of 0.025 seconds.
4. Adjust peaks to account for non-uniform spacing.  Step 3 gives estimates of 
      the peak locations.  A gradient ascent peak search is done around each of 
      these estimates.
5. Remove peaks < the required percentile (typically 70th) of the local input 
      signal.
6. Merge peaks closer than a quarter of the overall typical period.
7. Remove peaks that are less than the raw input a quarter of a period on either
      side.  
8. Remove peaks < a quarter as far from the local minimum to the adjacent peaks.
9. Add missing peaks based on breaks in the periodicity.
10. Repeat step 7.
The following steps are subsequently done for the respiratory data.
11. Troughs initially found as peaks in the inverted bandpass filtered signal.
12. Repeat steps 4-10 with lower thresholds, for peaks, replaced by upper 
       thresholds for troughs.
13. Remove peaks/troughs that are also troughs/peaks.
14. Add missing peaks and troughs based on outliers in the interpeak, and 
       intertrough, intervals and refining estimated peak locations using 
       gradient ascent/descent.

"""

import numpy             as np
import matplotlib.pyplot as plt
import bisect

def percentileFilter(peaks, rawData, percentile, upperThreshold=False, 
        phys_fs = None, dataType = "Cardiac", show_graph = False, 
        save_graph = True, OutDir = None, font_size = 10):
    """
    NAME
        percentileFilter
            Filter peaks based on percentile of raw data
            
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data 
                 indices.
        
        rawData: (array, dType = float) Raw input data
        
        percentile: (dType = float) Minimum (or maximum) percentile of raw data 
                 for a peak value to imply a valid peak (or trough)
                            
        upperThreshold: (dType = bool) Whether the threshold is the maximum 
                        acceptable value.  Default is False meaning it is the 
                        minimum acceptable value
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to 
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (np.isnan(np.sum(rawData))):
        print('*** ERROR in percentileFilter: nan values in data: ')
        return []
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied if'+
              ' graphing required')
  
    # Get peak values
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
    
    # Remove peaks that are less than the the required percentile of the input 
    # signal
    threshold = np.percentile(rawData, percentile)
    if upperThreshold: peaks = peaks[peakVals <= threshold]    
    else: peaks = peaks[peakVals >= threshold]
            
    # Graph (and save) results as required
    if show_graph or save_graph:
        if upperThreshold:
           graphPeaksAgainstRawInput(show_graph, save_graph, rawData, [], 
                phys_fs, dataType, troughs = peaks,
                OutDir = OutDir, prefix = dataType + 
                    'PctlFilt', 
                caption = 'Filter troughs based on percentile of raw data.',
                font_size = font_size)
        else:
           graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
                phys_fs, dataType, 
                OutDir = OutDir, prefix = dataType + 'PctlFilt', 
                caption = 'Filter peaks based on percentile of raw data.',
                font_size = font_size)

    return peaks

def localPercentileFilter(peaks, rawData, percentile, period=None, numPeriods=4, 
            upperThreshold=False, show_graph = False, save_graph = True, 
            phys_fs = None, dataType = "Cardiac", OutDir = None, 
            font_size = 10):
    """
    NAME
        localPercentileFilter
            Filter peaks based on local percentile of raw data
            
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data 
                                       indices.
        
        rawData: (array, dType = float) Raw input data
        
        percentile: (dType = float) Minimum (or maximum) percentile of raw data 
                    for a peak value to imply a valid peak (or trough)
                            
        period: (dType = NoneType) Overall typical period of raw data in time 
                series index units. Default is none, meaning the period is 
                determined from the raw data.
        
        numPeriods: (dType = int) Number of periods defining the local 
                                  neighborhood over which the percentile is 
                                  determined.  Default is 4.
                            
        upperThreshold: (dType = bool) Whether the threshold is the maximum 
                        acceptable value.  Default is False meaning it is the 
                        minimum acceptable value
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to 
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (np.isnan(np.sum(rawData))):
        print('*** ERROR in localPercentileFilter: nan values in data: ')
        return []
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              ' if graphing required')
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
        thresholds.append(np.percentile(rawData[Min:Max], percentile))

    # Apply local percentile filter
    if upperThreshold: peaks = peaks[np.array(peakVals) <= np.array(thresholds)]
    else: peaks = peaks[np.array(peakVals) >= np.array(thresholds)]
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
       graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
            phys_fs, dataType, OutDir = OutDir, 
            prefix = dataType + 'LocalPctlFilt', 
            caption = 'Filter peaks based on local percentile of raw data.',
            font_size = font_size)
       
    return peaks


def getTimeSeriesPeriod(rawData, minFrequency=1):
     """
     NAME
         getTimeSeriesPeriod
             Get overall typical period (s) of raw data in time series index 
             units          
     TYPE
          <class 'numpy.float64'>
     ARGUMENTS
         rawData: (array dType = float64) Raw cardiac data
         
         minFrequency: (dType = int) Minimum frequency (Hz) to be considered.  
                                     Default = 1
     AUTHOR
         Peter Lauren
     """
    
     if (np.isnan(np.sum(rawData))):
        print('*** ERROR in getTimeSeriesPeriod: nan values in data: ')
        return -1
    
     # Note that nan values are removed from the input raw values
     limit = round(len(rawData)/2) # Frequency limit is Nyquist frequency
     validRawData = rawData
     FourierSpectrum = abs(np.fft.fft(validRawData))
     selectedFourerSpectrum = FourierSpectrum[minFrequency:limit]
     F0 = len(rawData)  # Length of Fourier spectrum
     frequencyModeIndex = minFrequency+np.argmax(selectedFourerSpectrum)
     return  F0/frequencyModeIndex

 
def removePeaksCloseToHigherPointInRawData(peaks, rawData, direction='right', 
        portion=0.25, period=None, show_graph = False, save_graph = True, 
        phys_fs = None, dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        removePeaksCloseToHigherPointInRawData
            Remove peaks with values less than the maximum raw input data in a 
            specified portion of the overall period of the input data, from the 
            peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
     ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data 
                                       indices.
        
        rawData: (array, dType = float) Raw input data
        
        direction: (dType = str) Direction to look for higher point in raw data.  
            Options are 'right' or 'left'.  Default = 'right'.  That is, it aims
            to remove minor peaks on the rising side of a larger peak
                            
        portion: (dType = float) Portion of period that defines the offset from
                 the peak.  Default is 0.25.  That is a quarter of the period
            
        period: (dType = numpy.float64) Overall period, in seconds,  of the raw 
            data if known.  The default is that this is not supplied and is 
            estimated by the function
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if len(peaks) == 0:
        print('*** Error in removePeaksCloseToHigherPointInRawData:' + 
              ' Peaks array empty')
        return peaks
    
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    threshold = -(max(rawData) - min(rawData)) * 0.1
        
    searchLength = round(period * portion)
    start = 0       # start and end are to handle peaks right at the beginning 
                    #  or end of the input data
    end = len(peaks)
    if peaks[0] > 1:
        searchLength = min(searchLength, peaks[0] - 1)
    else:
        searchLength = min(searchLength, peaks[1] - 1)
        start = 1
    if len(rawData) - peaks[-1] > 1:
        searchLength = min(searchLength, len(rawData) - peaks[-1] - 1)
    else:
        searchLength = min(searchLength, len(rawData) - peaks[-2] - 1)
        end = -1
    diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) 
            for x in peaks[start:end]]
    if start > 0: diff.insert(0,0)
    if end == -1: diff.append(0)
    if len(diff) > 0: peaks = peaks[diff >= np.float64(threshold)]
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
        if direction=='right':
            prefix = dataType + 'RemoveUpsideFalsePeaks'
            caption = 'Remove "peaks" that are less than the raw input a' + \
                ' quarter of a period on right side.'
        else:
            prefix = dataType + 'RemoveDownsideFalsePeaks'
            caption = 'Remove "peaks" that are less than the raw input a' +\
                'quarter of a period on left side.'
        graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
            phys_fs, dataType, OutDir = OutDir, prefix = prefix, 
            caption = caption, font_size = font_size)
    
    return peaks

def removeTroughsCloseToLowerPointInRawData(troughs, rawData, direction='right', 
        portion=0.25, period=None, show_graph = False, save_graph = True, 
        phys_fs = None, dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        removeTroughsCloseToLowerPointInRawData
            Remove troughs with values greater than the minimum raw input data 
            in a specified portion of the overall period of the input data, from
            the peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
     ARGUMENTS
        troughs:   (array dType = numpy.int64) Array of trough locations in raw
                                               data indices.
        
        rawData: (array, dType = float) Raw input data
        
        direction: (dType = str) Direction to look for lower point in raw data.
                   Options are 'right' or 'left'.  Default = 'right'.  That is,
                   it aims to remove minor troughs on the falling side of a 
                   deeper trough
                            
        portion: (dType = float) Portion of period that defines the offset from
                 the peak.  Default is 0.25.  That is a quarter of the period
            
        period: (dType = numpy.float64) Overall period of the raw data if known.
                The default is that this is not supplied and is estimated by the
                function
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              ' if graphing required')
    
    if not period:
        period = getTimeSeriesPeriod(rawData)
        
    searchLength = round(period/4)
    searchLength = min(searchLength, troughs[0] - 1)
    searchLength = min(searchLength, len(rawData) - troughs[-1] - 1)
    diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) 
            for x in troughs]
    if len(diff) > 0: troughs = troughs[diff <= np.float64(0)]
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
        if direction=='right':
            caption = 'Remove downstroke false troughs.'
            prefix = dataType + 'RemoveDpwnstrokeFalseTroughsFilt'
        else: 
            caption = 'Remove upstroke false troughs.'
            prefix = dataType + 'RemoveUpstrokeFalseTroughsFilt'
        graphPeaksAgainstRawInput(show_graph, save_graph, rawData, [], phys_fs, 
            dataType, troughs = troughs, OutDir = OutDir, prefix = prefix, 
            caption = caption, font_size = font_size)
    
    return troughs

def removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData, 
        denominator=4.0, show_graph = False, save_graph = True, 
        phys_fs = None, dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        removePeaksCloserToLocalMinsThanToAdjacentPeaks
            Remove peaks with values less than the maximum raw input data in a 
            specified portion of the overall period of the input data, from the 
            peak, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
     ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data 
                                       indices.
        
        rawData: (array, dType = float) Raw input data
        
        denominator: (dType = float) Number by which to divide the current 
                     amplitude to determine the lower threshold of the 
                     accepitable peak value
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              ' if graphing required')
        return peaks

    # Get peak values
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
           
    # Remove peaks that are less than a quarter as far from the local minimum
    # to the adjacent peaks
    valleys = [((j-i)+(j-k))/2 
               for i, j, k in zip(peakVals[:-1], peakVals[1:], peakVals[2:])]
    fromLocalMin = [j-min(rawData[i:k]) 
                for i, j, k in zip(peaks[:-1], peakVals[1:], peaks[2:])]
    ratios = [i/j for i,j in zip(valleys,fromLocalMin)]
    ratios.insert(0,0)
    ratios.append(0)
    threshold = np.float64(-denominator)
    
    peaks = peaks[ratios>threshold]
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
       graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
            phys_fs, dataType, OutDir = OutDir, 
            prefix = dataType + 'RemovePeaksCloseToMinimum', 
            caption = 'Remove peaks that are less than a quarter as far' + 
                ' from the local minimum to the adjacent peaks.',
                font_size = font_size)
    
    return peaks

def removeTroughsCloserToLocalMaxsThanToAdjacentTroughs(troughs, rawData, 
        denominator=4.0, show_graph = False, save_graph = True, phys_fs = None,
        dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        removeTroughsCloserToLocalMaxsThanToAdjacentTroughs
            Remove troughs with values greater than the minimum raw input data
            in a specified portion of the overall period of the input data, 
            from the trough, in the specified direction            
     TYPE
         <class 'numpy.ndarray'>
     ARGUMENTS
        troughs:   (array dType = numpy.int64) Array of trough locations in raw
                                               data indices.
        
        rawData: (array, dType = float) Raw input data
        
        denominator: (dType = float) Number by which to divide the current 
                                     amplitude to determine the upper threshold
                                     of the accepitable trough value
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('** WARNING: Sampling frequency (phys_fs) must be supplied' + 
              'if graphing required')

    # Get trough values
    troughVals = []
    for i in troughs: troughVals.append(rawData[i])
           
    # Remove troughs that are less than a quarter as far from the local 
    # maximum to the adjacent troughs
    watersheds = [((j-i)+(j-k))/2 for i, j, k in zip(troughVals[:-1], 
                                            troughVals[1:], troughVals[2:])]
    fromLocalMax = [j-max(rawData[i:k]) for i, j, k in zip(troughs[:-1], 
                                            troughVals[1:], troughs[2:])]
    ratios = [i/j for i,j in zip(watersheds,fromLocalMax)]
    ratios.insert(0,0)
    ratios.append(0)
    threshold = np.float64(4.0)
    troughs = troughs[ratios<threshold]
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
           graphPeaksAgainstRawInput(show_graph, save_graph, rawData, [], 
                phys_fs, dataType, troughs = troughs,
                OutDir = OutDir, 
                prefix = dataType + 
                    'removeTroughsCloserToLocalMaxThanAdjacentTroughs', 
                caption = 'Remove troughs closer to local max than to' + 
                    ' adjacent troughs.',
                font_size = font_size)
    
    return troughs

def estimateSamplingFrequencyFromRawData(rawData, expectedCyclesPerMinute=70):
    """
    NAME
        estimateSamplingFrequencyFromRawData
            Estimate sampling frequency (Hz) from raw data based on expected 
            cycles per minute            
    TYPE
       <class 'numpy.float64'>
    ARGUMENTS
       peaks: (array dType = int64) Array of peak locations in raw data indices.
        
        expectedCyclesPerMinute: (dType = int) Expected number of cycles (heart
            beats, breaths) per minute based on input data type
    AUTHOR
        Peter Lauren
    """
    
    return (getTimeSeriesPeriod(rawData)*60)/expectedCyclesPerMinute

def removeOverlappingPeaksAndTroughs(peaks, troughs, rawData, 
        show_graph = False, save_graph = True, phys_fs = None, 
        dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        removeOverlappingPeaksAndTroughs
            Identify peaks that are also troughs and decide what each one should
            be (one or the other).
     TYPE
         <class 'numpy.float64'>, <class 'numpy.float64'>
     ARGUMENTS
        peaks: (array dType = int64) Array of peak locations in raw data indices
        
        troughs:   (array dType = numpy.int64) Array of trough locations in raw 
                                               data indices.
        
        rawData: (array, dType = float) Raw input data
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    RETURNS
        filtered peaks, filtered troughs
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              'if graphing required')
        
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
    
    if (show_graph or save_graph) and phys_fs:
        graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
             phys_fs, dataType, troughs = troughs, OutDir = OutDir, 
             prefix = dataType + 'removeOverlappingPeaksAndTroughs', 
             caption = 'Remove overlapping peaks and troughs.',
             font_size = font_size)
                
    return peaks, troughs

def removeExtraInterveningPeaksAndTroughs(peaks, troughs, rawData):
    """
    NAME
        removeExtraInterveningPeaksAndTroughs
            Ensure there is only one peak between each pair of troughs and only 
            one trough between each pair of peaks
    TYPE
         <class 'numpy.float64'>, <class 'numpy.float64'>
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data 
                                       indices.
        
        troughs:   (array dType = numpy.int64) Array of trough locations in raw
                                               data indices.
        
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
        interpeakTroughs.append(len([i for i in range(len(troughs)) 
                                     if troughs[i] in Range]))
        
    # Identify interpeak intervals with more than one trough
    crowdedIntervals = list(np.where(np.array(interpeakTroughs)>1)[0])
    crowdedIntervals.insert(0,-1)   # Prepend dummy value so indices can run 
                                    # backwards an include 0
    
    # Keep only the deepest trough in interpeak intervals
    for interval in crowdedIntervals[-1:0:-1]:
        troughGroup = [i for i in range(0,len(troughs)) 
                       if troughs[i] in peakRanges[interval]]
        keep = np.argmin([rawData[i] for i in troughGroup])
        troughGroup.remove(troughGroup[keep])
        troughs = np.delete(troughs,troughGroup)
    
    # Get intertrough intervals
    troughRanges = [range(x,y) for x,y in zip(troughs[0:], troughs[1:])]
    
    # Count peaks in intertrough intervals
    intertroughPeaks = []
    for Range in troughRanges:
        intertroughPeaks.append(len([i for i in range(len(peaks)) 
                                     if peaks[i] in Range]))
        
    # Identify intertrough intervals with more than one peak
    crowdedIntervals = list(np.where(np.array(intertroughPeaks)>1)[0])
    crowdedIntervals.insert(0,-1)   # Prepend dummy value so indices can run 
                                    # backwards an include 0
    
    # Keep only the hioghest peak in interpeak intervals
    for interval in crowdedIntervals[-1:0:-1]:
        peakGroup = [i for i in range(0,len(peaks)) 
                     if peaks[i] in troughRanges[interval]]
        keep = peakGroup[0] + np.argmax([rawData[peaks[i]] for i in peakGroup])
        peakGroup.remove(keep)
        peaks = np.delete(peaks,peakGroup)
        
    return peaks, troughs

def removeClosePeaks(peaks, period, rawData, Troughs = False, denominator=4, 
                     show_graph = 0, save_graph = 1, phys_fs = None, 
                     dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        removeClosePeaks
            Remove peaks (or troughs) that are closer than period/demonominator
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        peaks:   (array dType = int64) Array of peak locations in raw data 
                                       indices.
        
        period:   (dType = numpy.float64) Overall typical period of raw data in 
                                          time series index units.
        
        Troughs:  (dType = bool) Whether processing troughs instead of peaks
        
        denominator: (dType = int) Number by which to divide the period in order
                                  to determine the minimum acceptable separation
        
        show_graph:   (dType = bool) Whether to graph the results
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    RETURNS
        filtered peaks (or troughs)
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              ' if graphing required')
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
    offset = peaks[0] 
    peaks = [offset]
    for interval in intervals:
        peaks.append(offset+interval)
        offset = offset + interval
   
    # Adjust peaks/troughs from uniform spacing
    if Troughs: # Processing troughs instead of peaks
        peaks = np.array(peaks)
        for i in range(0,2):
            peaks = refinePeakLocations(peaks, rawData, period = period, 
                Troughs = True, show_graph = max(show_graph-1,0), 
                save_graph = max(save_graph-1,0), phys_fs = phys_fs, 
                OutDir = OutDir, font_size = font_size)
    else:   # Processing peaks
        peaks = np.array(peaks)
        for i in range(0,2):
            peaks = refinePeakLocations(peaks, rawData, period = period, 
                    show_graph = max(show_graph-1,0), 
                    save_graph = max(save_graph-1,0), phys_fs = phys_fs, 
                    OutDir = OutDir, font_size = font_size)
            
    # Convert peaks back to numpy array and remove duplicates that may result
    # from refining the locations
    peaks = np.unique(np.array(peaks))
    
    # Convert peaks to numpy array
    peaks = np.array(peaks)
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
        if Troughs:
           graphPeaksAgainstRawInput(show_graph, save_graph, rawData, [], 
                phys_fs, dataType, troughs = peaks, OutDir = OutDir, 
                prefix = dataType + 'MergeCloseTroughs', 
                caption = 'Merge troughs that are closer than one quarter' + 
                                ' of the overall typical period.',
                font_size = font_size)
        else:
           graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
                phys_fs, dataType, OutDir = OutDir, 
                prefix = dataType + 'MergeClosePeaks', 
                caption = 'Merge peaks that are closer than one quarter' + 
                                             ' of the overall typical period.',
                font_size = font_size)
    
    return peaks

def bandPassFilterRawDataAroundDominantFrequency(rawData, minBeatsPerSecond, 
        phys_fs, dataType = "Cardiac", show_graph = False, save_graph = True, 
        OutDir = None, graphIndex = None, font_size = 10) :
    """
    NAME
        bandPassFilterRawDataAroundDominantFrequency
            Band pass filter raw data based on overall typical period of raw 
            data in time series index units.            
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        minBeatsPerSecond: (dType = float) Minimum expected beats per second
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Required if graph 
                                 True
        
        dataType: (dType = str) Type of data being processed
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (np.isnan(np.sum(rawData))):
        print('*** ERROR in bandPassFilterRawDataAroundDominantFrequency:' + 
              ' nan values in data: ')
        return []
    
    rawDataLength = len(rawData)
    
    # Determine harmonic frequency unit
    F0 = phys_fs/rawDataLength
    
    # Get lower cutoff index
    lowerCutoffIndex = round(minBeatsPerSecond/F0)
    
    # Get Fourier transform
    FourierTransform = np.fft.fft(rawData)
    FourierSpectrum = abs(FourierTransform)
    
    # Determine frequency peak
    NyquistLength = round(rawDataLength/2)
    frequencyPeak = np.argmax(FourierSpectrum[lowerCutoffIndex:NyquistLength])\
        +lowerCutoffIndex
    print('++ {} bandpass filter frequency peak: {:.6f} Hz'
          ''.format(dataType, F0 * frequencyPeak))
    
    # Find bounds based on -3 dB limits
    peakVal = FourierSpectrum[frequencyPeak]
    targetValue = peakVal/2
    leftIndices = [i for i in range(0,frequencyPeak) 
                   if FourierSpectrum[i] < targetValue]
    if len(leftIndices) == 0:
       lowerMin = 0
    else: lowerMin = max(leftIndices)
    rightIndices = [i for i in range(frequencyPeak,NyquistLength) 
                    if FourierSpectrum[i] < targetValue]
    if len(rightIndices) == 0:
       lowerMax = NyquistLength-1
    else: lowerMax = min(rightIndices)
    
    # Avoid filter being too narrow
    lowerMin = min(round(frequencyPeak/2), lowerMin)
    lowerMax = max(round(1.5*frequencyPeak), lowerMax)
                                
    # Determine band limits
    upperMin = rawDataLength - lowerMax
    upperMax = rawDataLength - lowerMin
    
    # Zero part of FT outside limits
    filterArray = np.zeros(rawDataLength)
    filterArray[lowerMin:lowerMax] = 1
    filterArray[upperMin:upperMax] = 1
    filteredFT = FourierTransform * filterArray
    
    # Get IFT
    filteredRawData = np.real(np.fft.ifft(filteredFT))
    
    if show_graph or save_graph :
        # Show selected part of Fourier transform
        x = []  
        rawData1 = abs(FourierTransform)
        filteredrawData1 = filterArray
        end = len(rawData1)
        for i in range(0,end): x.append(i*F0)
        fig, ax_left = plt.subplots()
        plt.xlabel("Frequency (Hz)", fontdict={'fontsize': font_size})
        plt.ylabel('Fourier Spectral Value',color='g', 
                   fontdict={'fontsize': font_size})
        ax_right = ax_left.twinx()
        ax_right.plot(x[3:end//20], filteredrawData1[3:end//20], color='red')
        ax_left.plot(x[3:end//20],rawData1[3:end//20], color='green')
        plt.ylabel('Filter',color='r')
        plt.title("Selected part of the Fourier Spectrum", 
                         fontdict={'fontsize': font_size})
        
        # Save plot to file
        if save_graph:
            prefix = dataType + 'SelectedFourierTransformPart'
            plt.savefig('%s/%s.pdf' % (OutDir, prefix)) 
            if show_graph: plt.show(block=True)
            if not show_graph: plt.close()  # Close graph after saving
    
        # Plot filtered signal agains raw data
        x = []    
        end = len(filteredRawData)
        for i in range(0,end): x.append(i/phys_fs)
        fig, ax_left = plt.subplots()
        plt.xlabel("Time (s)", fontdict={'fontsize': font_size})
        plt.ylabel('Raw input data value',color='g', 
                   fontdict={'fontsize': font_size})
        ax_right = ax_left.twinx()
        upperLimit = round(len(filteredRawData)/4)
        ax_right.plot(x[0:upperLimit], filteredRawData[0:upperLimit], 
                      color='red')
        ax_left.plot(x[0:upperLimit],rawData[0:upperLimit], color='green')
        # ax_right.plot(x, filteredRawData, color='red')
        # ax_left.plot(x,rawData, color='green')
        plt.ylabel('Filtered Data Value',color='r', 
                   fontdict={'fontsize': font_size})
        plt.title("BP Filtered [" + str(round(F0*lowerMin)) + ":" +\
            str(round(F0*lowerMax)) + "] (red) and raw input data (green)", 
            fontdict={'fontsize': font_size})
            
        # Save plot to file
        if save_graph:
            prefix = dataType + 'BPF_VRawInput'
            plt.savefig('%s/%s.pdf' % (OutDir, prefix)) 
            
        if show_graph: plt.show(block=True)
        if not show_graph: plt.close()  # Close graph after saving
        
    return filteredRawData

def refinePeakLocations(peaks, rawData, period = None, Troughs = False, 
        show_graph = False, save_graph = True, phys_fs = None, 
        dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        refinePeakLocations
            Adjust peaks to correspond to local maxima.  This is usually 
            necessary if the peaks were.Determined from a band-pass filtered 
            version of the input raw data where the peaks are uniformly spaced 
            based on the overall typical period of raw data
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        peaks: (array dType = int64) Array of peaks to be refined
        
        rawData: (array, dType = float) Raw input data
        
        period: (dType = NoneType) Overall typical period of raw data in time 
                series index units. Default is none, meaning the period is 
                determined from the raw data.
                
        Troughs: (dType = bool) Whether troughs are processed instead of peaks
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                                   be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              ' if graphing required')
        return peaks
    
    # Find period if not supplied
    # if not period:
    #     period = getTimeSeriesPeriod(rawData)

    # # Determine half window width
    # halfWindowWidth = round(period/4)
    
    intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
    halfWindowWidth = round(np.median(intervals)/4)
    
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
    if Troughs:
        Caption = 'Adjust troughs from uniform spacing.'
        Troughs = peaks
        Peaks = []
    else:
        Caption = 'Adjust peaks from uniform spacing.'
        Troughs = []
        Peaks = peaks
    if (show_graph or save_graph) and phys_fs:
       graphPeaksAgainstRawInput(show_graph, save_graph, rawData, Peaks, 
            phys_fs, dataType, OutDir = OutDir, 
            prefix = dataType + 'AdjustPeaksFromUniformSpacing', 
            caption = Caption, troughs = Troughs,
            font_size = font_size)
           
    # Apply offsets
    return peaks

def addMissingPeaks(peaks, rawData, period=None, show_graph = False, 
        save_graph = True, phys_fs = None, dataType = "Cardiac", OutDir = None, 
        font_size = 10):
    """
    NAME
        addMissingPeaks
            Find and fill gaps in period series of peaks
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        peaks: (array dType = int64) Array of peaks to be refined
        
        rawData: (array, dType = float) Raw input data
        
        period: (dType = NoneType) Overall typical period of raw data in time 
                series index units. Default is none, meaning the period is 
                determined from the raw data.
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is to
                                be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied' + 
              ' if graphing required')
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
            peaks = np.insert(peaks, i+1, [start+increment*j 
                                           for j in range(1,additionPlus1)])
   
    # Adjust peaks from uniform spacing
    peaks = refinePeakLocations(peaks, rawData, period = period, 
                show_graph = show_graph, 
                save_graph = save_graph, phys_fs = phys_fs, OutDir = OutDir,
                font_size = font_size)
            
    # Graph (and save) results as required
    if (show_graph or save_graph) and phys_fs:
       graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
            phys_fs, dataType, OutDir = OutDir, 
            prefix = dataType + 'AddMissingPeaksPerPeriodicity', 
            caption = 'Add missing peaks.', font_size = font_size)
    
    return peaks


def addMissingPeaksAndTroughs(peaks, troughs, rawData, period=None, 
        show_graph = False, save_graph = True, phys_fs = None, 
        dataType = "Cardiac", OutDir = None, font_size = 10):
    """
    NAME
        addMissingPeaksAndTroughs
            Find and fill gaps in period series of peaks and troughs
    TYPE
         <class 'numpy.ndarray'>
    ARGUMENTS
        peaks: (array dType = int64) Array of peaks to be refined
        
        troughs: (array dType = int64) Array of troughs to be refined
        
        rawData: (array, dType = float) Raw input data
        
        period: (dType = NoneType) Overall typical period of raw data in time 
            series index units.  Default is none, meaning the period is 
            determined from the raw data.
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        phys_fs: (dType = float) Sampling frequency in Hz.  Only relevant if 
                                 results are to be graphed
        
        dataType: (dType = str) Type of data being processed
        
        OutDir:   (dType = str) Output directory.  Only relevant if graph is 
                                to be saved to disk.
    AUTHOR
        Peter Lauren
    """
    
    if (show_graph or save_graph) and not phys_fs:
        print('+* WARNING: Sampling frequency (phys_fs) must be supplied if' +
              ' graphing required')
    
    # Find period if not supplied
    if not period:
        period = getTimeSeriesPeriod(rawData)

    # Threshold for peaks and troughs        
    threshold = (max(rawData) - min(rawData)) * 0.1
        
    # Find interpeak intervals
    intervals = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
    factor = np.median(intervals)*0.9
    searchLength = int(factor/2)
    peaksToAdd = [round(intervals[i]/factor)-1 for i in range(0,len(intervals))]
    for i in range(len(intervals)-1,-1,-1):
        if (peaksToAdd[i]>0):
            additionPlus1 = peaksToAdd[i] + 1
            start = peaks[i]
            end = peaks[i+1]
            increment = (end-start)/additionPlus1
            newPeaks = [start+increment*j for j in range(1,additionPlus1)]
            
            # Filter out false peaks
            diff = [rawData[round(x)] - min(rawData[round(x-searchLength):\
                                    round(x+searchLength)]) for x in newPeaks]
            newPeaks = np.array(newPeaks)[diff >= -np.float64(threshold)]
            if len(newPeaks) > 0:
                troughs = np.insert(troughs, i+1, newPeaks)
        
    # Find intertrough intervals
    intervals = [x - troughs[i - 1] for i, x in enumerate(troughs)][1:]
    factor = np.median(intervals)*0.9
    troughsToAdd = [round(intervals[i]/factor)-1 
                    for i in range(0,len(intervals))]
    for i in range(len(intervals)-1,-1,-1):
        if (troughsToAdd[i]>0):
            additionPlus1 = troughsToAdd[i] + 1
            start = troughs[i]
            end = troughs[i+1]
            increment = (end-start)/additionPlus1
            newTroughs = [start+increment*j for j in range(1,additionPlus1)]
            
            # Filter out false troughs
            diff = [rawData[round(x)] - min(rawData[round(x-searchLength):\
                                round(x+searchLength)]) for x in newTroughs]
            newTroughs = np.array(newTroughs)[diff <= np.float64(threshold)]
            if len(newTroughs) > 0:
                troughs = np.insert(troughs, i+1, newTroughs)
   
    # Adjust peaks from uniform spacing
    peaks = refinePeakLocations(peaks, rawData, period = np.median(intervals)/2, 
                show_graph = max(show_graph - 1, 0), 
                save_graph = max(save_graph - 1, 0),
                phys_fs = phys_fs, OutDir = OutDir)
    troughs = refinePeakLocations(troughs, rawData, 
       period = np.median(intervals)/2, Troughs = True, 
       show_graph = max(show_graph - 1, 0), save_graph = max(save_graph - 1, 0),
       phys_fs = phys_fs, OutDir = OutDir)
    
    # Remove extra peaks between troughs and troughs between peaks
    peaks, troughs = removeExtraInterveningPeaksAndTroughs(peaks, troughs, 
                                                           rawData)
    
    if (show_graph or save_graph) and phys_fs:
        graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, 
            phys_fs, dataType, troughs = troughs, OutDir = OutDir, 
            prefix = dataType + 'addMissingPeaksAndTroughs', 
            caption = 'Add missing peaks and troughs.',
            font_size = font_size)
    
    return peaks, troughs

def graphPeaksAgainstRawInput(show_graph, save_graph, rawData, peaks, phys_fs, 
        peakType, troughs = [], OutDir = None, prefix = 'cardiacPeaks', 
        caption = [], font_size = 10):
    '''
    NAME
        graphPeaksAgainstRawInput
        Graph graph raw input data along with peaks and optionally troughs
    TYPE
         <void>
    ARGUMENTS
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        rawData: (array, dType = float) Raw input data
        
        peaks: (array dType = int64) Array of peaks to be refined
        
        phys_fs: (dType = float) Sampling frequency in Hz
                
        peakType: (dType = str) String that defines the type of data.  May be 
                                "Cardiac" or "Respiratory"
        
        troughs: (array dType = int64) Array of troughs to be refined. Not used
                                       if plotting troughs not required
        
        OutDir: (dType = str) String defining the directory to which the graph
                              is written. Not used if it is not required to save
                              the graph to disk.
    AUTHOR
        Peter Lauren
    '''
    
    # Graph respiratory peaks and troughs against respiratory time series
    x = []    
    end = len(rawData)
    for i in range(0,end): x.append(i/phys_fs)
    plt.subplot(211)
    plt.plot(x, rawData, "g") #Lines connecting peaks and troughs
    if len(peaks) > 0:
        peakVals = []
        for i in peaks: peakVals.append(rawData[i])
        plt.plot(peaks/phys_fs, peakVals, "ro") # Peaks
    if len(troughs) > 0:
        troughVals = []
        for i in troughs: troughVals.append(rawData[i])
        plt.plot(troughs/phys_fs, troughVals, "bo") # troughs
        if len(peaks) > 0:
            title = peakType +\
                " peaks (red), troughs (blue) and raw input data (green)"
        else:
            title = peakType + " troughs (blue) and raw input data (green)"
    else:
        title = peakType + " peaks (red) and raw input data (green)"
    title += '\n' + caption
    plt.xlabel("Time (s)", fontdict={'fontsize': font_size})
    plt.ylabel("Input data value", fontdict={'fontsize': font_size})
    plt.title(title, fontdict={'fontsize': font_size})
    # plt.text(.5, .05, caption, ha='center') # PSL: This puts an unneeded  
                                              # caption in an unwated place.
    ### [PT] the above should replace this code, so we can avoid the
    ### nonstandard mpl import
    ### PDL: It seems this was not doing anything
    # mpl.text.Text(.5, .05, caption, ha='center')

    # Save plot to file
    if show_graph: 
        plt.ion()
        plt.show(block=True)
    else:
        plt.ioff()
        
    if save_graph:
        if not OutDir:
            print('+* WARNING (graphPeaksAgainstRawInput): Cannot save graph.' +
                  ' No output directory specified')
            if not show_graph: plt.close()  # Close graph after saving
            return 1
        else:
            plt.savefig('%s/%s.pdf' % (OutDir, prefix)) 
        
        
    plt.close()  # Close graph after saving
    
    return 0

def checkForNans(rawData, dataType, failureThreshold = 100):
    '''
    NAME
        checkForNans
        Check for NaN entries, in numeric data, and replace them
    TYPE
         <void>
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        dataType: (dType = str) Type of data; "Cardiac" or "Respiratory"
        
        failureThreshold: (dType = int) Maximum number of consecutive NaN values
                                        before failure
    AUTHOR
        Peter Lauren
    '''    
    
    if (np.isnan(np.sum(rawData))): # If nan's in raw data
        print('+* WARNING. NaN entries at the following indices of the ' +
              dataType + ' data')
        nanIndices = np.argwhere(np.isnan(rawData))
        print(nanIndices)
        print('NaN values')
        print(rawData[nanIndices])
        
        # Try to replace each nan with adjacent valid value(s) 
        if (nanIndices[0] == 0):    # If first element a nan
            rawData[0] = rawData[bisect.bisect_left(nanIndices, -1)]
            nanIndices = np.delete(nanIndices, 0)
        if (nanIndices[-1] == len(rawData) - 1):    # If last element a nan
            rawData[-1] = rawData[bisect.bisect_right(nanIndices, 
                                                      nanIndices[-1]-1)]
            nanIndices = np.delete(nanIndices, -1)
        for i in nanIndices: # Process all Nans, 
            nanIndex = i
            left = nanIndex - 1
            right = left+2
            nanLength = 1
            while np.isnan(rawData[right]): 
                right = right + 1
                nanLength = nanLength + 1
                if nanLength > failureThreshold:
                    print('*** ERROR: Too many consecutive NaNs')
                    return []
            rawData[nanIndex] = rawData[left]+(rawData[right]-rawData[left])*\
                                (float(nanIndex-left)/(right-left))
        
    return rawData
