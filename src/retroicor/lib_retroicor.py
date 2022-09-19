__authors__ = "Joshua Zosky and Peter Lauren"

"""
    Copyright 2022 Peter Lauren
    peterdlauren@gmail.com

    This file contains all the library functions for "RetroTS2".
    "RetroTS2" is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    "RetroTS2" is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with "RetroTS".  If not, see <http://www.gnu.org/licenses/>.
    
        TODO:
        - Align names of variables
"""

import numpy as np
import matplotlib as mpl
from matplotlib import figure as mplf
import matplotlib.pyplot as plt
import math
import scipy
from scipy import signal as sps
import pandas as pd
import gzip
import json
import statistics

# Local libraries
import libPeakFilters as lpf


# Global constants
GLOBAL_M = 3
numSections = 1
NUM_RVT = 5

# Global variables
OutDir = "."

# for checking whether quotients are sufficiently close to integral
g_epsilon = 0.00001

    
def setOutputDirectory(directory):
    """
    NAME
        setOutputDirectory
            Set directory to which all files are written
        
    TYPE
        void
        
    SYNOPSIS
        setOutputDirectory(directory)
        
    ARGUMENTS
        directory:   string variable specifying the output directory
    AUTHOR
       Peter Lauren
    """
    global OutDir
    OutDir = directory

def readArray(parameters, key):
    """
    NAME
        readArray
            Read an array from an input file specified by the key in the parameters field
            
    TYPE
        <class 'list'>
        
    SYNOPSIS
        readArray(parameters, key)
        
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   key to file of interest
   AUTHOR
       Peter Lauren
    """

    # Read single column of numbers into list of lists
    with open(parameters[key]) as f:
        array = []
        for line in f: # read rest of lines
            array.append([float(x) for x in line.split()])
            
    # Transform list of lists into single list
    array = [n for one_dim in array for n in one_dim]
    return array

def getCardiacPeaks(parameters, rawData, filterPercentile=70.0):
   """
   NAME
       getCardiacPeaks
           Get peaks from cardiac time series supplied as an ASCII 
           file with one time series entry per line
           
    TYPE
        <class 'numpy.ndarray'>, int
   SYNOPSIS
       getCardiacPeaks(parameters)
   ARGUMENTS
       parameters:   dictionary of input parameters which includes the following fields.
       
       rawData: Raw cardiac data
       
       filterPercentile: Minimum percentile of raw data for a peak value to imply a valid peak
   AUTHOR
       Peter Lauren
   """
   
   oldArray = rawData
   # # Debug
   # array = oldArray[0:200000]
   
   # Remove NaNs from raw data
   rawData = [x for x in rawData if math.isnan(x) == False]
    
   global OutDir

   # Find peaks in time domain
   timeDomainPeaks, _ = sps.find_peaks(np.array(rawData), width=int(parameters["phys_fs"]/10))
   
   # Estimate frequency of time domain points
   intervals = [j-i for i, j in zip(timeDomainPeaks[:-1], timeDomainPeaks[1:])]
   medianTimeDomainFrequency = len(rawData)/statistics.median(intervals)

   # Band pass filter raw data
   filterData = lpf.bandPassFilterRawDataAroundDominantFrequency(rawData,\
        graph = True,\
        phys_fs = parameters["phys_fs"], OutDir=OutDir)
   if len(filterData) == 0:
       print('Failed to band-pass filter cardiac data')   
       return []
   
   # Get initial peaks using window that is an tenth of a second  (HR <+ 680 BPM)
   peaks, _ = sps.find_peaks(np.array(filterData), width=int(parameters["phys_fs"]/20))
   
   # Adjust peaks from uniform spacing
   peaks = lpf.refinePeakLocations(peaks, rawData, period = None)
    
   # Remove peaks that are less than the required percentile of the local input signal
   peaks = lpf.localPercentileFilter(peaks, rawData, filterPercentile, numPeriods=3)

   # Estimate the overall typical period       
   # limit = round(len(rawData)/2)
   # period = len(rawData)/(1+np.argmax((abs(np.fft.fft(rawData))[1:limit])))

   # # Get peak values
   # peakVals = []
   # for i in peaks: peakVals.append(rawData[i])
   
   # # Remove "peaks" that are less than the raw input a quarter of a period on right side
   # # This is tomove false peaks on the upstroke
   # # searchLength = round(parameters["-phys_fs"]/16)
   # searchLength = round(period/4)
   # searchLength = min(searchLength, peaks[0] - 1)
   # searchLength = min(searchLength, len(rawData) - peaks[-1] - 1)
   # diff = [rawData[x] - max(rawData[x-searchLength:x+searchLength]) for x in peaks]
   # if len(diff) > 0: peaks = peaks[diff >= np.float64(0)]
   
   # # Remove false peaks on the downstroke
   # shiftArray = rawData[searchLength:]
   # shiftArray = np.insert(rawData,np.zeros(searchLength).astype(int),0)
   # diff = [rawData[x] - shiftArray[x] for x in peaks]
   # peaks = peaks[diff >= np.float64(0)]
   
   # # Get peak values
   # peakVals = []
   # for i in peaks: peakVals.append(rawData[i])
           
   # # Remove peaks that are less than a quarter as far from the local minimum to the adjacent peaks
   # valleys = [((j-i)+(j-k))/2 for i, j, k in zip(peakVals[:-1], peakVals[1:], peakVals[2:])]
   # fromLocalMin = [j-min(rawData[i:k]) for i, j, k in zip(peaks[:-1], peakVals[1:], peaks[2:])]
   # ratios = [i/j for i,j in zip(valleys,fromLocalMin)]
   # ratios.insert(0,0)
   # ratios.append(0)
   # threshold = np.float64(-4.0)
   # peaks = peaks[ratios>threshold]

   # # Merge peaks that are closer than one quarter of the overall typical period
   # intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
   # intervals.insert(0,round(period))
   # threshold = period/4
   # peaks = peaks[intervals>=threshold]
   
   
           
     # Check for, and fill in, missing peaks - MAKE OWN FUNCTION
   # interpeak = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
   # minSep = min(interpeak)
   # Threshold = minSep * 2
   # numPeaks = len(peaks)
   # for p in range(numPeaks-2,-1,-1):
   #      if interpeak[p] > Threshold:
   #          numberToAdd = int(round(interpeak[p]/minSep))
   #          sep = round(interpeak[p]/numberToAdd)
   #          if sep < minSep:
   #              numberToAdd = numberToAdd - 1
   #              sep = round(interpeak[p]/numberToAdd)               
   #          for i in range(1,numberToAdd):
   #              peaks = np.insert(peaks, p+i, peaks[p]+i*sep)
               
   # Graph cardiac peaks against cardiac time series
   peakVals = []
   for i in peaks: peakVals.append(rawData[i])
   mplf.Figure(figsize =(7,7))
   mpl.pyplot.subplot(211)
   mpl.pyplot.plot(rawData, "g") #Lines connecting peaks and troughs
   mpl.pyplot.plot(peaks, peakVals, "ro") # Peaks
   mpl.pyplot.xlabel("Input data index")
   mpl.pyplot.ylabel("Input data input value")
   mpl.pyplot.title("Cardiac peaks (red) and raw input data (green)",\
               fontdict={'fontsize': 12})
        
   # Save plot to file
   mpl.pyplot.savefig('%s/cardiacPeaks.pdf' % (OutDir)) 
   mpl.pyplot.show()  # If this is left out, output file is blank
    
   return peaks, len(rawData)


def getRespiratoryPeaks(parameters, rawData):
    """
    NAME
    getRespiratoryPeaks
    Get peaks from repiratory time series supplied as an ASCII 
    file with one time series entry per line
    TYPE
    <class 'numpy.ndarray'>, int
    SYNOPSIS
    respFile(parameters)
    ARGUMENTS
    parameters:   dictionary of input parameters which includes the following fields.
    
    -respFile: repiratory input file
    AUTHOR
    Peter Lauren
    """
   
    oldArray = rawData
    
    # Get initial peaks using window that is an eighth of a second  (BR <+ 480 BPM)
    peaks, _ = sps.find_peaks(np.array(rawData), width=int(parameters["phys_fs"]/8))
    
    # Remove peaks that are less than the 10th percentile of the input signal
    peaks = lpf.percentileFilter(peaks, rawData, percentile=10.0)
    
    # Estimate the overall typical period 
    period = lpf.getTimeSeriesPeriod(rawData)      
    
    # Remove "peaks" that are less than the raw input a quarter of a period on right side
    # This is tomove false peaks on the upstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, period=period)
    
    # Remove false peaks on the downstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, direction='left', period=period)
    
    # Remove peaks that are less than a quarter as far from the local minimum to the adjacent peaks
    peaks = lpf.removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData)
    
    # Merge peaks that are closer than one quarter of the overall typical period
    peaks = lpf.removeClosePeaks(peaks, period)
    
    troughs, _ = sps.find_peaks(-np.array(rawData), width=int(parameters["phys_fs"]/8))
    
    # Remove troughs that are more than the 90th percentile of the input signal
    troughs = lpf.percentileFilter(troughs, rawData, percentile=90.0, upperThreshold=True)
    
    # Remove "troughs" that are greater than the raw input a quarter of a period on right side
    # This is to remove false troughs on the upstroke
    troughs = lpf.removeTroughsCloseToLowerPointInRawData(troughs, rawData, period=period)
    
    # Remove false troughs on the downstroke
    troughs = lpf.removeTroughsCloseToLowerPointInRawData(troughs, rawData,\
            period=period, direction = 'left')
    
    # Remove troughs that are less than a quarter as far from the local maximum to the adjacent troughs
    troughs = lpf.removeTroughsCloserToLocalMaxsThanToAdjacentTroughs(troughs, rawData)
    
    # Merge troughs that are closer than one quarter of the overall typical period
    troughs = lpf.removeClosePeaks(troughs, period)
    
    # Remove extra peaks bewteen troughs and troughs between peaks
    peaks, troughs = lpf.removeExtraInterveningPeaksAndTroughs(peaks, troughs, rawData)
    
    # Filter peaks and troughs.  Reject peak/trough pairs where
    #    difference is less than one tenth of the total range
    # nPeaks = len(peaks)
    # nTroughs = len(troughs)
    # minNum = min(nPeaks,nTroughs)
    # ptPairs = [rawData[peaks[x]]-rawData[troughs[x]] for x in range(0,minNum)]
    # threshold = (max(rawData)-min(rawData))/10
    # indices2remove = list(filter(lambda x: ptPairs[x] <threshold, range(len(ptPairs))))
    # peaks = np.delete(peaks,indices2remove)
    # troughs = np.delete(troughs,indices2remove)
    
    # Graph respiratory peaks and troughs against respiratory time series
    peakVals = []
    for i in peaks: peakVals.append(rawData[i])
    troughVals = []
    for i in troughs: troughVals.append(rawData[i])
    mplf.Figure(figsize =(7,7))
    mpl.pyplot.subplot(211)
    mpl.pyplot.plot(rawData, "g") #Lines connecting peaks and troughs
    mpl.pyplot.plot(peaks, peakVals, "ro") # Peaks
    mpl.pyplot.plot(troughs, troughVals, "bo") # Peaks
    mpl.pyplot.xlabel("Input data index")
    mpl.pyplot.ylabel("Input data input value")
    mpl.pyplot.title("Respiratory peaks (red), troughs (blue) and raw input data (green)",\
    fontdict={'fontsize': 10})
         
    # Save plot to file
    global OutDir
    mpl.pyplot.savefig('%s/RespiratoryPeaks.pdf' % (OutDir)) 
    mpl.pyplot.show()  # If this is left out, output file is blank
     
    return peaks, troughs, len(rawData)

def determineCardiacPhases(peaks, fullLength, phys_fs, rawData):
    """
    NAME
       determineCardiacPhases
           Determine phases, in the cardiac cycle based on the Glover (2000) paper
    TYPE
        <class 'list'>
    SYNOPSIS
       determineCardiacPhases(peaks, fullLength)
    ARGUMENTS
        peaks:   Peaks in input cardiac time series.  Type = <class 'numpy.ndarray'>.
        
        fullLength:   Lenagth of cardiac time series
        
        phys_fs:   Physiological signal sampling frequency in Hz. 
    AUTHOR
       Peter Lauren
    """

    phases = []
    inc = 0
    k = math.pi * 2
    numIntervals = len(peaks) - 1
    
    # Assign -1 to phases of all time tpoints before the first peak
    for i in range(0,peaks[0]): phases.append(-1.0)
    
    # Get phases between peaks
    for i in range(0,numIntervals):
        start = peaks[i]
        end = peaks[i+1]
        period = end - start
        for j in range(start,end):
            phases.append((k*(j-start))/period)
            inc = inc+1
            
    # Estimate phases before first peak as the last 
    #   phases of the first full cycle
    iIndex =  peaks[1] -  peaks[0]
    for oIndex in range(0, peaks[0]):
      phases[oIndex] =  phases[iIndex] 
      iIndex = iIndex + 1
            
    # Estimate phases before last peak as the first 
    #   phases of the last full cycle
    iIndex =  peaks[-2] + 1
    tailLength = fullLength - peaks[-1]
    for i in range(0,tailLength):
      phases.append(phases[iIndex]) 
      iIndex = iIndex + 1
      
    # Move phase range from [0,2Pi] to [-Pi,Pi]
    phases = [x - math.pi for x in phases]
      
    # PLot phases
    x = []    
    end = min(len(phases),round(len(phases)*50.0/len(peaks)))
    for i in range(0,end): x.append(i/phys_fs)
    fig, ax_left = mpl.pyplot.subplots()
    mpl.pyplot.xlabel("Time (s)")
    mpl.pyplot.ylabel('Input data input value',color='g')
    ax_right = ax_left.twinx()
    ax_right.plot(x, phases[0:end], color='red')
    ax_left.plot(x, rawData[0:end], color='green')
    mpl.pyplot.ylabel('Phase (Radians)',color='r')
    mpl.pyplot.title("Cardiac phase (red) and raw input data (green)")
        
    # Save plot to file
    mpl.pyplot.savefig('%s/CardiacPhaseVRawInput.pdf' % (OutDir)) 
    mpl.pyplot.show()
            
    return phases

def getACoeffs(parameters, key, phases):
    """
    NAME
       getACoeffs
           Determine a coefficients from equation 4 of Glover (2000) paper (equation 4)
    TYPE
        <class 'list'>
    SYNOPSIS
       getACoeffs(parameters, key, phases)
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   key to file of interest
        
        phases    :   <class 'list'> containing phases determined as described in Glover (2000) paper
    AUTHOR
       Peter Lauren
    """

    data = readArray(parameters, key)
    mean = np.mean(data)
    N = len(data)
    global GLOBAL_M
    a = []
    for m in range(1,GLOBAL_M):
        num = 0
        denom = 0
        for n in range(0,N):
            num = num + (data[n]-mean)*math.cos(m*phases[n])
            temp = math.cos(m*phases[n])
            denom = denom + (temp*temp)
        a.append(num/denom)
        
    return a

def getBCoeffs(parameters, key, phases):
    """
    NAME
       getBCoeffs
           Determine b coefficients from equation 4 of Glover (2000) paper (equation 4)
    TYPE
        <class 'list'>
    SYNOPSIS
       getBCoeffs(parameters, key, phases)
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   key to file of interest
        
        phases    :   <class 'list'> containing phases determined as described in Glover (2000) paper
    AUTHOR
       Peter Lauren
    """

    data = readArray(parameters, key)
    mean = np.mean(data)
    N = len(data)
    global M
    b = []
    for m in range(1,M):
        num = 0
        denom = 0
        for n in range(0,N):
            num = num + (data[n]-mean)*math.sin(m*phases[n])
            temp = math.sin(m*phases[n])
            denom = denom + (temp*temp)
        b.append(num/denom)
        
    return b
            

def determineRespiratoryPhases(parameters, respiratory_peaks, respiratory_troughs, phys_fs, rawData):
    """
    NAME
        determineRespiratoryPhases
            Determine respiratory phases as descibed in Glover (2000) paper (equation 3)
    TYPE
        <class 'list'>
    SYNOPSIS
       determineRespiratoryPhases(parameters, respiratory_peaks, respiratory_troughs)
    ARGUMENTS
        parameters:   dictionary of input parameters which includes the following fields.
            -respFile;   Name of ASCII file with respiratory time series
            phys_fs:     Physiological signal sampling frequency in Hz.
        
        respiratory_peaks      :   peaks in respiratory time series.  Type = <class 'numpy.ndarray'>
        
        respiratory_troughs    :   <class 'numpy.ndarray'> containing troughs in the respiratory time series
        
        phys_fs:     Physiological signal sampling frequency in Hz 
        
        rawData:     Raw respiratory data
        
    AUTHOR
       Peter Lauren
    """
    
    NUM_BINS = 100
    
    # Determine whether currently inspiration or expiration
    if respiratory_peaks[0] < respiratory_troughs[0]:
        polarity = 1
    else:
        polarity = -1
        
    # Number of segments where each segment is either inspiration or expiration
    numFullSegments = len(respiratory_peaks) + len(respiratory_troughs) - 1
    
    # Initialize array of output phases
    phases = np.zeros(len(rawData))
    
    # Assign values to time series before first full segment
    peakIndex = 0
    troughIndex = 0
    start = 0
    finish = min(respiratory_peaks[peakIndex], respiratory_troughs[troughIndex])
    denom = finish  # Total length of segment
    
    # Histogram values in segment
    sample = [x - rawData[respiratory_troughs[troughIndex]] for x in rawData[start:finish]]  
    counts, bins = np.histogram(sample, bins=NUM_BINS) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity > 0: Rmax = max(sample)
    else: Rmax = rawData[respiratory_peaks[0]] # Maximum value in segment
    for i in range(start,finish): # Move through segment
        end = round(sample[i]*NUM_BINS/Rmax) # Summation limit
        
        # Count values, in segment that are not greater than the summation limit
        count = 0
        end = min(end,len(counts)-1)
        for j in range(0,end):
            count = count + counts[j]
            
        # Use result to estimate phase at given time point
        phases[i] = (math.pi*count*polarity)/denom
    
    # Switch polarity and increment peak indxe if new polarity inspiration
    #   Otherwise increment trough index instead
    polarity = -polarity
    if polarity > 0: peakIndex = peakIndex + 1
    else: troughIndex = troughIndex + 1
    
    peakIndex = 0
    troughIndex = 0
    for segment in range(0,numFullSegments):    # Process each segment in turn
    
        # Determine segment from the peak and trough indices
        start = min(respiratory_peaks[peakIndex], respiratory_troughs[troughIndex])
        finish = max(respiratory_peaks[peakIndex], respiratory_troughs[troughIndex])
        denom = finish - start  # Total length of segment
        
        # Histogram values in segment
        sample = [x - rawData[respiratory_troughs[troughIndex]] for x in rawData[start:finish]]  
        counts, bins = np.histogram([x for x in sample if math.isnan(x) == False], bins=NUM_BINS) 
        
        # Determine phase based on equation 3 is Glover paper
        Rmax = max(sample) # Maximum value in segment
        for i in range(start,finish): # Move through segment
            end = round(sample[i-start]*NUM_BINS/Rmax) # Summation limit
            
            # Count values, in segment that are not greater than the summation limit
            count = 0
            end = min(end, len(counts))
            if end > 0:
                for j in range(0,end):
                    count = count + counts[j]
                
            # Use result to estimate phase at given time point
            phases[i] = (math.pi*count*polarity)/denom
        
        # Switch polarity and increment peak indxe if new polarity inspiration
        #   Otherwise increment trough index instead
        polarity = -polarity
        if polarity > 0: peakIndex = peakIndex + 1
        else: troughIndex = troughIndex + 1
        if peakIndex>=len(respiratory_peaks) or troughIndex>=len(respiratory_troughs): break
    
    # Assign values to time series after last full segment
    start = finish
    finish = len(rawData)
    denom = finish - start  # Total length of segment
    
    # Histogram values in segment
    sample = [x - rawData[respiratory_troughs[-1]] for x in rawData[start:finish]]  
    counts, bins = np.histogram(sample, bins=NUM_BINS) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity < 0: Rmax = max(sample)
    else: Rmax = rawData[respiratory_peaks[-1]] # Maximum value in segment
    for i in range(start,finish): # Move through segment
        end = round(sample[i-start]*NUM_BINS/Rmax) # Summation limit
        
        # Count values, in segment that are not greater than the summation limit
        count = 0
        for j in range(0,end):
            count = count + counts[j]
            
        # Use result to estimate phase at given time point
        phases[i] = (math.pi*count*polarity)/denom
    
      
    # PLot phases and raw data against time in seconds.
    x = []    
    end = min(len(phases),round(len(phases)*50.0/len(respiratory_peaks)))
    for i in range(0,end): x.append(i/phys_fs)
    fig, ax_left = mpl.pyplot.subplots()
    mpl.pyplot.xlabel("Time (s)")
    mpl.pyplot.ylabel('Input data input value',color='g')
    ax_right = ax_left.twinx()
    ax_right.plot(x, phases[0:end], color='red')
    ax_left.plot(x, rawData[0:end], color='green')
    mpl.pyplot.ylabel('Phase (Radians)',color='r')
    mpl.pyplot.title("Respiratory phase (red) and raw input data (green)")
        
    # Save plot to file
    mpl.pyplot.savefig('%s/RespiratoryPhaseVRawInput.pdf' % (OutDir)) 
    mpl.pyplot.show()
    
        
    return phases

def getPhysiologicalNoiseComponents(parameters):
    """
    NAME
        getPhysiologicalNoiseComponents 
            Return physiological (respiratory and cardiac) contamination components of BOLD signal
    TYPE
        <class 'pandas.core.frame.DataFrame'>
    SYNOPSIS
       getPhysiologicalNoiseComponents(parameters)
    ARGUMENTS
        parameters:   Dictionary with the following fields.
        
            -respFile:   file containing respiratory time series
            
            -cardFile:   file containing cardiac time series
            
            -aby     :   whether  a and b coefficients as per Glover et al, Magnetic 
                                        Resonance in Medicine 44:162–167 (2000)
                                        
            -niml    :   whether output should be in niml format instead of CSV
            
    AUTHOR
       Peter Lauren
    """
    
    # Initializations
    respiratory_phases = np.array([]) #None
    cardiac_phases     = []           #
    
    # Parameters to read in raw data
    rawDataParams = dict()
    if 'StartTime' in parameters: rawDataParams['StartTime'] = parameters['StartTime']
    rawDataParams["phys_fs"] = parameters["phys_fs"]
    
    # Process cardiac data if any
    if parameters["-cardFile"] or len(parameters["phys_cardiac_dat"]) > 0:           
        # rawData = readArray(parameters, '-cardFile')
        rawData = readRawInputData(rawDataParams, parameters["-cardFile"], parameters["phys_cardiac_dat"])
        
        if not parameters['phys_fs']: # Sampling frequency not supplied
            parameters['phys_fs'] = lpf.estimateSamplingFrequencyFromRawData(rawData, 70)
                
        cardiac_peaks, fullLength = getCardiacPeaks(parameters, rawData) 
        
        if len(cardiac_peaks) > 0:
            cardiac_phases = determineCardiacPhases(cardiac_peaks, fullLength,\
                                                parameters['phys_fs'], rawData)
        
    # Process respiratory data if any
    if parameters["-respFile"] or len(parameters["phys_resp_dat"]) > 0:

        # rawData = readArray(parameters, '-respFile')
        rawData = readRawInputData(rawDataParams, parameters["-respFile"], parameters["phys_resp_dat"])
        
        respiratory_peaks, respiratory_troughs, fullLength = \
            getRespiratoryPeaks(parameters, rawData) 
        
        respiratory_phases = determineRespiratoryPhases(parameters, \
                respiratory_peaks, respiratory_troughs, parameters['phys_fs'], \
                    [x for x in rawData if math.isnan(x) == False])
        
    if (parameters['-aby']):    # Determine a and b coefficients as per Glover et al, Magnetic 
                                # Resonance in Medicine 44:162–167 (2000)
        # Get a coefficients
        cardiacACoeffs = getACoeffs(parameters, '-cardFile', cardiac_phases)
        respiratoryACoeffs = getACoeffs(parameters, '-respFile', respiratory_phases)
        
        # Get b coefficients
        cardiacBCoeffs = getBCoeffs(parameters, '-cardFile', cardiac_phases)
        respiratoryBCoeffs = getBCoeffs(parameters, '-respFile', respiratory_phases)
    else:   # a and b coefficients set to 1.0
        cardiacACoeffs = [1.0]
        respiratoryACoeffs = [1.0]
        cardiacBCoeffs = [1.0]
        respiratoryBCoeffs = [1.0]
        cardiacACoeffs.append(1.0)
        respiratoryACoeffs.append(1.0)
        cardiacBCoeffs.append(1.0)
        respiratoryBCoeffs.append(1.0)
    
    global GLOBAL_M
    
    # Initialize output table
    df = pd.DataFrame()
    
    # Make output table columns names
    columnNames = []
    numSections = parameters["-s"]
    len_resp = len(respiratory_phases)
    len_card = len(cardiac_phases)
    if len_resp:
        for s in range(0,numSections):
            for r in range(0,4):
                string = 's' + str(s) + '.Resp' + str(r)
                columnNames.append(string)
    if len_card:
        for s in range(0,numSections):
            for r in range(0,4):
                string = 's' + str(s) + '.Card' + str(r)
                columnNames.append(string)
        
    # Make output table data matrix
    data = []
    len_resp = len(respiratory_phases)
    len_card = len(cardiac_phases)

    if len_resp and len_card :
        T = min(len_resp, len_card)
    elif len_resp :
        T = len_resp
    elif len_card :
        T = len_card
    print('T = ', T)

    nreg = 0              # number of regressors we make
    for t in range(0,T):
        # sum = 0
        addend = []
        if len_resp :
            for m in range(1,GLOBAL_M):
                m0 = m - 1
                addend.append(respiratoryACoeffs[m0]*math.cos(m*respiratory_phases[t]))
                addend.append(respiratoryBCoeffs[m0]*math.sin(m*respiratory_phases[t]))
                if not(t):
                    nreg+= 2
        if len_card:
            for m in range(1,GLOBAL_M):
                m0 = m - 1
                addend.append(cardiacACoeffs[m0]*math.cos(m*cardiac_phases[t]))
                addend.append(cardiacBCoeffs[m0]*math.sin(m*cardiac_phases[t]))
                if not(t):
                    nreg+= 2
        data.append(addend)

    # Reshape matrix according to number of slices
    nrow = np.shape(data)[0]
    print("DEBUG: nrow =", nrow)
    print("DEBUG: shape BEFORE =", np.shape(data))
    print("DEBUG: nsections =", numSections)
    data = np.reshape(data[0:nrow-(nrow%numSections)][:], (nreg*numSections, -1)).T
    print("DEBUG: shape AFTER =", np.shape(data))
    
    df = pd.DataFrame(data,columns=columnNames)
        
    return df   

def readRawInputData(respcard_info, filename=None, phys_dat=None):
    """
    NAME
        readRawInputData 
            Read in raw input data according to the user=supplied program arguments.
            Outputs raw data.
    TYPE
        <class 'numpy.ndarray'> 
    SYNOPSIS
       readRawInputData(respcard_info, filename=None, phys_dat=None)
    ARGUMENTS
        respcard_info:   Dictionary with the following fields.
        
            StartTime:   (Optional)  Time at which the signal of interest starts
            
            phys_fs:   Physiological signal sampling frequency in Hz.
                   
        filename:   String giving the local name of the input file
        
        phys_dat:   BIDS style physio file
        
    AUTHOR
       Joshua Zosky & Peter Lauren (Documentation by Peter Lauren)
    """

    if phys_dat is None or len(phys_dat) == 0: # No BIDS style physio file
        # Read repiration/cardiac file into phys_dat
        phys_dat = []
        with open(filename, "rb") as h:
            for entry in h:
                phys_dat.append(float(entry))
            for line in h:
                phys_dat.append(float(line.strip()))
                
    v_np = np.asarray(phys_dat)
    
    # Trim leading datapoints if they precede start time
    if ('StartTime' in respcard_info and respcard_info['StartTime']<0):
        start_index = round(-respcard_info['StartTime'] * respcard_info["phys_fs"])
        print('start_index = ', start_index)
        v_np = v_np[start_index:-1]
    
    return v_np

def runAnalysis(parameters):
    """
    NAME
        runAnalysis 
            Run retroicor analysis as described by Glover (2000) and implemented by Peter Lauren
    TYPE
        void
    SYNOPSIS
       runAnalysis(parameters)
    ARGUMENTS
    
        parameters:   Dictionary with the following fields.
        
            -respFile:   file containing respiratory time series
            
            -cardFile:   file containing cardiac time series
            
            -aby: whether  a and b coefficients as per Glover et al, Magnetic  Resonance in Medicine 44:162–167 (2000)
                                        
            -niml: whether output should be in niml format instead of CSV
            
            -outputFileName:   Output filename
            
    AUTHOR
       Peter Lauren
    """
    
    physiologicalNoiseComponents = getPhysiologicalNoiseComponents(parameters)
    if parameters['-niml']:
        return 0
    
    physiologicalNoiseComponents.to_csv(parameters['-outputFileName'])
    
    # PLot first 200 rows of dataframe
    colors = ['blue','cyan','blueviolet','cadetblue', 'olive','yellowgreen','red','magenta']
    physiologicalNoiseComponents.head(200).plot(color=colors)
    
    # Send output to terminal
    if (parameters['-abt']): print(repr(physiologicalNoiseComponents))

def getInputFileParameters(respiration_info, cardiac_info, phys_file,\
                        phys_json_arg, respiration_out, cardiac_out, rvt_out):
    """
    NAME
        getInputFileParameters 
            Returns the local respiriation file name  (if JSON file absent, None otherwise), 
            respiration file data (if JSON file present, None otherwise), the 
            local cardiac file name  (if JSON file absent, None otherwise), 
            cardiac file data (if JSON file present, None otherwise).
    TYPE
        <class 'str'>, <class 'numpy.ndarray'>, <class 'str'>, <class 'numpy.ndarray'>
    SYNOPSIS
       getInputFileParameters(respiration_info, cardiac_info, phys_file,
       phys_json_arg, respiration_out, cardiac_out, rvt_out)
    ARGUMENTS
        respiration_info:   Dictionary with the following fields.
        
            respiration_file:  Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            zero_phase_offset:Phase offset added to the location of each peak.
            Default is 0.0
            
        cardiac_info:   Dictionary with the following fields.
            
            phys_fs:   Physiological signal sampling frequency in Hz.
        
            cardiac_file:  Name of ASCII file with cardiac time series
            
        phys_file: BIDS formatted physio file in tab separated format. May
        be gzipped.
                
        phys_json_arg: File metadata in JSON format
        
        respiration_out:  Whether to have respiratory output
        
        cardiac_out:  Whether to have cardiac output
        
        rvt_out:  Whether to have RVT output
            
    AUTHOR
       Josh Zosky and Peter Lauren
    """
    
    # Initialize outputs
    respiration_file = None
    phys_resp_dat = []
    cardiac_file = None
    phys_cardiac_dat = []
            
    # Ensure there are no conflicting input file types
    # BIDS = Brain Imaging Data Structure
    if (((phys_file is not None) and (respiration_info["respiration_file"] is not None))
        or ((phys_file is not None) and (cardiac_info["cardiac_file"] is not None))):
        raise ValueError('You should not pass a BIDS style phsyio file'
                         ' and respiration or cardiac files.')
        
    # Get the peaks for respiration_info and cardiac_info
    # init dicts, may need -cardiac_out 0, for example   [16 Nov 2021 rickr]
    if phys_file:
        # Use json reader to read file data into phys_meta
        with open(phys_json_arg, 'rt') as h:
            phys_meta = json.load(h)
        # phys_ending is last element following a period
        phys_ending = phys_file.split(".")[-1]
        
        # Choose file opening function on the basis of whether file is gzippped
        if phys_ending == 'gz':
            opener = gzip.open 
        else:
            opener = open
            
        # Read Columns field of JSON file
        phys_dat = {k:[] for k in phys_meta['Columns']}
        
        # Append tab delimited phys_file to phys_dat
        with opener(phys_file, 'rt') as h:
            # for pl in h.readlines():
            for pl in h:
                pls = pl.split("\t")
                for k,v in zip(phys_meta['Columns'], pls):
                    phys_dat[k].append(float(v))
                    
        # Process StartTime is in JSON file
        if ('StartTime' in phys_meta and "StartTime" not in respiration_info):
            startTime = float(phys_meta["StartTime"])
            if (startTime > 0):
                print('***** WARNING: JSON file gives positive start time which is not currently handled')
                print('    Start time must be <= 0')
            else:
                respiration_info["StartTime"] = startTime            
                cardiac_info["StartTime"] = startTime            
                    
        print('phys_meta = ', phys_meta)
        # Read columns field from JSON data
        print('Read columns field from JSON data')
        for k in phys_meta['Columns']:
            phys_dat[k] = np.array(phys_dat[k])
            
            # Read respiratory component
            if k.lower() == 'respiratory' or k.lower() == 'respiration':
                # create peaks only if asked for    25 May 2021 [rickr]
                if respiration_out or rvt_out:
                   if not respiration_info["phys_fs"]:
                       respiration_info['phys_fs'] = phys_meta['SamplingFrequency']
                   respiration_file = None
                   phys_resp_dat = phys_dat[k]
            
            # Read cardiac component
            elif k.lower() == 'cardiac':
                # create peaks only if asked for    25 May 2021 [rickr]
                if cardiac_out != 0:
                   if not cardiac_info["phys_fs"]:
                       cardiac_info['phys_fs'] = phys_meta['SamplingFrequency']
                   cardiac_file = None
                   phys_cardiac_dat = phys_dat[k]
            else:
                print("** warning phys data contains column '%s', but\n" \
                      "   RetroTS only handles cardiac or respiratory data" % k)
    else:   # Not a JSON file
        if respiration_info["respiration_file"]:
            respiration_file = respiration_info["respiration_file"]
            phys_resp_dat = []
        if cardiac_info["cardiac_file"]:
            cardiac_file = cardiac_info["cardiac_file"]
            phys_cardiac_dat = []
            
    return respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat

