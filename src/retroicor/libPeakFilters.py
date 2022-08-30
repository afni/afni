#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 25 14:46:15 2022

@author: peterlauren
"""

import numpy as np
import math

def percentileFilter(peaks, rawData, percentile, upperThreshold=False):
    """
    NAME
        percentileFilter
            Filter peaks based on 
            
     TYPE
         <class 'numpy.ndarray'>
    SYNOPSIS
        getCardiacPeaks(parameters)
    ARGUMENTS
        peaks:   Array of peak locations in raw data indices.
        
        rawData: Raw cardiac data
        
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

def getTimeSeriesPeriod(rawData):
     """
     NAME
         getTimeSeriesPeriod
             Get overall typical period of time series in time series index units 
         
      TYPE
          <class 'numpy.float64'>
     SYNOPSIS
         getTimeSeriesPeriod(rawData)
     ARGUMENTS
         rawData: Raw cardiac data
     AUTHOR
         Peter Lauren
     """
    
     limit = round(len(rawData)/2)
     return len(rawData)/(1+np.argmax((abs(fft(rawData))[1:limit])))
    
