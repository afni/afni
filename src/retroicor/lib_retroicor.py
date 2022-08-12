__authors__ = "Joshua Zosky and Peter Lauren"

"""
    Copyright 2015 Joshua Zosky
    Copyright 2022 Peter Lauren
    joshua.e.zosky@gmail.com
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
        - Modify formatting of docstrings
        - Align names of variables
"""

import sys
import numpy as np
from numpy.fft import fft, ifft
from matplotlib import pyplot as plt 
from pylab import plot, subplot, show, figure, text
import math
from scipy.signal import find_peaks
import pandas as pd
from numpy import size, shape, column_stack, savetxt,real,spacing,pi
from array import array
from scipy.signal import firwin, lfilter 
from scipy.interpolate import interp1d
import gzip
import json


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

def getCardiacPeaks(parameters, array):
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
       
           -cardFile: Cardiac input file
   AUTHOR
       Peter Lauren
   """
   
   oldArray = array
   # # Debug
   array = oldArray[0:200000]
   
   # Get initial peaks using window that is an eighth of a second  (HR <+ 480 BPM)
   peaks, _ = find_peaks(np.array(array), width=int(parameters["-phys_fs"]/8))
   
   # Remove peaks that are less than the 10th percentile of the input signal
   threshold = np.percentile(array, 10)
   numPeaks = len(peaks)
   for p in range(numPeaks-1,-1,-1):
        if array[peaks[p]] < threshold:
            peaks = np.delete(peaks,p) 

   # Estimate the overall typical period            
   period = len(array)/(1+np.argmax((abs(fft(array))[1:-1])))

   # Get peak values
   peakVals = []
   for i in peaks: peakVals.append(array[i])

   # Merge peaks that are closer than one quater of the overall typical period
   intervals = [j-i for i, j in zip(peaks[:-1], peaks[1:])]
   intervals.insert(0,round(period))
   oldPeaks = peaks
   peaks = peaks[intervals>=threshold]
   
   # Make sure local maxima have not been displaced
   # peakArray = np.zeros(len(array))
   # for i in oldPeaks: peakArray[i] = array[i]
   # corrections = [np.argmax(peakArray[i+1:j-1]) for i, j in zip(peaks[:-1], peaks[2:])]
   # peaks = [i+j for i,j, in zip (peaks[1:-1],corrections[:])]
           
   
   # Remove peaks that are less than 10% of the way from the local minimum to the adjacent peaks
   valleys = [((j-i)+(j-k))/2 for i, j, k in zip(peakVals[:-1], peakVals[1:], peakVals[2:])]
   fromLocalMin = [j-min(array[i:k]) for i, j, k in zip(peaks[:-1], peakVals[1:], peaks[2:])]
   ratios = [i/j for i,j in zip(valleys,fromLocalMin)]
   ratios.insert(0,0)
   ratios.append(0)
   threshold = np.float64(-1.0)
   peaks = peaks[ratios>threshold]
   # markForDeletion = []
   # for p in range(1,numPeaks-3):
   #      if fromLocalMin[p]/(-0.01-valleys[p]) < threshold:
   #          markForDeletion.append(p)
            # peaks = np.delete(peaks,p) 
   
           
   #  # Check for, and fill in, missing peaks - MAKE OWN FUNCTION
   # interpeak = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
   # minSep = min(interpeak)
   # Threshold = minSep * 2
   # numPeaks = len(peaks)
   # for p in range(numPeaks-2,-1,-1):
   #     if interpeak[p] > Threshold:
   #         numberToAdd = int(round(interpeak[p]/minSep))
   #         sep = round(interpeak[p]/numberToAdd)
   #         if sep < minSep:
   #             numberToAdd = numberToAdd - 1
   #             sep = round(interpeak[p]/numberToAdd)               
   #         for i in range(1,numberToAdd):
   #             peaks = np.insert(peaks, p+i, peaks[p]+i*sep)
               
   # Graph cardiac peaks against cardiac time series
   peakVals = []
   for i in peaks: peakVals.append(array[i])
   figure(1)
   subplot(211)
   plot(array, "g") #Lines connecting peaks and troughs
   plot(peaks, peakVals, "ro") # Peaks
   plt.xlabel("Input data index")
   plt.ylabel("Input data input value")
   plt.title("Cardiac peaks (red) and raw input data (green)",\
               fontdict={'fontsize': 12})
        
   # Save plot to file
   global OutDir
   plt.savefig('%s/cardiacPeaks.pdf' % (OutDir)) 
   plt.show()  # If this is left out, output file is blank
    
   return peaks, len(array)


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

   array = readArray(parameters, '-respFile')
    
    # Debug: Make even number of peaks followed by troughs
   # array = [ -x for x in array ]

   peaks, _ = find_peaks(np.array(array))
    
   troughs, _ = find_peaks(-np.array(array))
   
   # Filter peaks and troughs.  Reject peak/trough pairs where
   #    difference is less than one tenth of the total range
   nPeaks = len(peaks)
   nTroughs = len(troughs)
   minNum = min(nPeaks,nTroughs)
   ptPairs = [array[peaks[x]]-array[troughs[x]] for x in range(0,minNum)]
   threshold = (max(array)-min(array))/10
   indices2remove = list(filter(lambda x: ptPairs[x] <threshold, range(len(ptPairs))))
   peaks = np.delete(peaks,indices2remove)
   troughs = np.delete(troughs,indices2remove)
               
   # Graph respiratory peaks and troughs against respiratory time series
   peakVals = []
   for i in peaks: peakVals.append(array[i])
   troughVals = []
   for i in troughs: troughVals.append(array[i])
   figure(1)
   subplot(211)
   plot(array, "g") #Lines connecting peaks and troughs
   plot(peaks, peakVals, "ro") # Peaks
   plot(troughs, troughVals, "bo") # Peaks
   plt.xlabel("Input data index")
   plt.ylabel("Input data input value")
   plt.title("Respiratory peaks (red), troughs (blue) and raw input data (green)",\
               fontdict={'fontsize': 10})
        
   # Save plot to file
   global OutDir
   plt.savefig('%s/RespiratoryPeaks.pdf' % (OutDir)) 
   plt.show()  # If this is left out, output file is blank
    
   return peaks, troughs, len(array)

def getTroughs(parameters, key):
   """
   NAME
       getTroughs
           Get troughs from repiratory time series supplied as an ASCII file 
           with one time series entry per line
    TYPE
        <class 'numpy.ndarray'>
   SYNOPSIS
       getTroughs(parameters, key)
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   key to file of interest
   AUTHOR
       Peter Lauren
   """

   array = readArray(parameters, key)

   troughs, _ = find_peaks(-np.array(array))
    
   return troughs

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
    fig, ax_left = plt.subplots()
    plt.xlabel("Time (s)")
    plt.ylabel('Input data input value',color='g')
    ax_right = ax_left.twinx()
    ax_right.plot(x, phases[0:end], color='red')
    ax_left.plot(x, rawData[0:end], color='green')
    plt.ylabel('Phase (Radians)',color='r')
    plt.title("Cardiac phase (red) and raw input data (green)")
        
    # Save plot to file
    plt.savefig('%s/CardiacPhaseVRawInput.pdf' % (OutDir)) 
    plt.show()
            
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
        counts, bins = np.histogram(sample, bins=NUM_BINS) 
        
        # Determine phase based on equation 3 is Glover paper
        Rmax = max(sample) # Maximum value in segment
        for i in range(start,finish): # Move through segment
            end = round(sample[i-start]*NUM_BINS/Rmax) # Summation limit
            
            # Count values, in segment that are not greater than the summation limit
            count = 0
            for j in range(0,end):
                count = count + counts[j]
                
            # Use result to estimate phase at given time point
            phases[i] = (math.pi*count*polarity)/denom
        
        # Switch polarity and increment peak indxe if new polarity inspiration
        #   Otherwise increment trough index instead
        polarity = -polarity
        if polarity > 0: peakIndex = peakIndex + 1
        else: troughIndex = troughIndex + 1
    
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
    fig, ax_left = plt.subplots()
    plt.xlabel("Time (s)")
    plt.ylabel('Input data input value',color='g')
    ax_right = ax_left.twinx()
    ax_right.plot(x, phases[0:end], color='red')
    ax_left.plot(x, rawData[0:end], color='green')
    plt.ylabel('Phase (Radians)',color='r')
    plt.title("Respiratory phase (red) and raw input data (green)")
        
    # Save plot to file
    plt.savefig('%s/RespiratoryPhaseVRawInput.pdf' % (OutDir)) 
    plt.show()
    
        
    return phases

def determineRespiratoryPhasesOld(parameters, respiratory_peaks, respiratory_troughs, phys_fs, rawData):
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
        
    AUTHOR
       Peter Lauren
    """

    data = readArray(parameters, '-respFile')
    
    denom = len(data)
    offset = 0 - min(data)
    for i in range(0,denom):
        data[i] = data[i] + offset

    # Determine sign array
    # respiratory_peaks, _ = find_peaks(np.array(data))
    # respiratory_troughs, _ = find_peaks(-np.array(data))
    signs = []
    nPeaks = len(respiratory_peaks)
    nTroughs = len(respiratory_troughs)
    
    signs = []
    sign = 1
    for i in range(0,respiratory_troughs[0]):
        signs.append(sign)
    end = nTroughs - 1
    for t in range(0,end):
        sign = -sign
        start = respiratory_troughs[t]
        finish = respiratory_troughs[t+1]
        for i in range(start,finish):
            signs.append(sign)
    sign = -sign
    for i in range(respiratory_troughs[-1],denom):
        signs.append(sign)
        
    # Rmax = respiratory_peaks[0] - respiratory_troughs[0]
    phases = []
    
    if respiratory_peaks[0] < respiratory_troughs[0]:
        # Assign -1 to phases of all time tpoints before the first peak
        for i in range(0,respiratory_peaks[0]): phases.append(-1.0)
    
        for p in range(0,nTroughs):
            Min = data[respiratory_troughs[p]]
            Rmax = data[respiratory_peaks[p]]
            for i in range(respiratory_peaks[p],respiratory_troughs[p]):
                phase = math.pi*(data[i]-Min)*signs[i]/Rmax
                phases.append(phase)
            if p<nPeaks-1:
                for i in range(respiratory_troughs[p],respiratory_peaks[p+1]):
                    phase = math.pi*(data[i]-Min)*signs[i]/Rmax
                    phases.append(phase)
            
        # Prepend phases before first peak
        input = round(2*respiratory_peaks[0])
        output = 0
        while output < respiratory_peaks[0]:
           phases[output] = phases[input] 
           input = input - 1
           output = output + 1
    else:       # Begins with a trough
        # Assign -1 to phases of all time tpoints before the first trough
        for i in range(0,respiratory_troughs[0]): phases.append(-1.0)
    
        for p in range(0,nPeaks):
            Min = data[respiratory_troughs[p]]
            Rmax = data[respiratory_peaks[p]]
            for i in range(respiratory_troughs[p],respiratory_peaks[p]):
                phase = math.pi*(data[i]-Min)*signs[i]/Rmax
                phases.append(phase)
            if p<nTroughs-1:
                for i in range(respiratory_peaks[p],respiratory_troughs[p+1]):
                    phase = math.pi*(data[i]-Min)*signs[i]/Rmax
                    phases.append(phase)
            
        # Prepend phases before first trough
        input = round(2*respiratory_troughs[0])
        output = 0
        while output < respiratory_troughs[0]:
           phases[output] = -phases[input] 
           input = input - 1
           output = output + 1
            
    # Append phases after last turning point.
    if respiratory_troughs[-1] > respiratory_peaks[-1]:
        # Append phases after last tough
        input = respiratory_troughs[-1] - 1
        length = denom - respiratory_troughs[-1]
        for i in range(0,length):
            phases.append(-phases[input])
            input = input - 1
    else:       
        # Append phases after last peak
        input = respiratory_peaks[-1] - 1
        length = denom - respiratory_peaks[-1]
        for i in range(0,length):
            phases.append(phases[input])
            input = input - 1
      
    # PLot phases
    x = []    
    end = min(len(phases),round(len(phases)*50.0/len(respiratory_peaks)))
    for i in range(0,end): x.append(i/phys_fs)
    fig, ax_left = plt.subplots()
    plt.xlabel("Time (s)")
    plt.ylabel('Input data input value',color='g')
    ax_right = ax_left.twinx()
    ax_right.plot(x, phases[0:end], color='red')
    ax_left.plot(x, rawData, color='green')
    plt.ylabel('Phase (Radians)',color='r')
    plt.title("Respiratory phase (red) and raw input data (green)")
        
    # Save plot to file
    plt.savefig('%s/RespiratoryPhaseVRawInput.pdf' % (OutDir)) 
    plt.show()
        
    return phases

def compareLaurenAndZoskyPhases(cardiac_info, respiration_info, oldCardiacPhases,\
                oldRespiratoryPhases, newCardiacPhases, newRespiratoryPhases, sliceNumber):
    
    # Get old and new cardiac phases
    old = []
    new = []
    Inc = sliceNumber*shape(oldCardiacPhases)[0]*shape(oldCardiacPhases)[1]
    for i in range(0,shape(oldCardiacPhases)[1]):
        for j in range(0,shape(oldCardiacPhases)[0]): 
            old.append(oldCardiacPhases[j,i,sliceNumber])
            new.append(newCardiacPhases[Inc])
            Inc = Inc + 1
    
    # Plot cardiac phases together
    x = []    
    end = len(new)
    for i in range(0,end): x.append(i/respiration_info["phys_fs"])
    plt.plot(x, new, "b")          
    plt.plot(x, old, "r")          
    plt.xlabel("Time (s)")
    plt.ylabel("Respiratory Phase")
    plt.title("Respiratory phase using new phase algorithm with old determination of peaks")
        
    # Save plot to file
    plt.savefig('%s/cardiacNewVOld.pdf' % (OutDir)) 
    plt.show()
    
    
    # Get old and new respiratory phases
    old = []
    new = []
    Inc = sliceNumber*shape(oldRespiratoryPhases)[0]*shape(oldRespiratoryPhases)[1]
    for i in range(0,shape(oldRespiratoryPhases)[1]):
        for j in range(0,shape(oldRespiratoryPhases)[0]): 
            old.append(oldRespiratoryPhases[j,i,sliceNumber])
            new.append(newRespiratoryPhases[Inc])
            Inc = Inc + 1
    
    # Plot cardiac phases together
    x = []    
    end = len(new)
    for i in range(0,end): x.append(i/cardiac_info["phys_fs"])
    plt.plot(x, new, "b")          
    plt.plot(x, old, "r")          
    plt.xlabel("Time (s)")
    plt.ylabel("Respiratory Phase")
    plt.title("Old (red) and new(blue) respiratory Phase")
        
    # Save plot to file
    plt.savefig('%s/respiratoryNewVOld.pdf' % (OutDir)) 
    plt.show()
    
    return 0

def getNiml(data,columnNames,parameters,respiratory_phases,cardiac_phases):
    """
    NAME
        getNiml 
            Apply Peter Lauren's implementation of the algorithm, descibed in Glover (2000) paper, and output the results in NIML format
    TYPE
        Integer (0 for success and 1 for failure)
    SYNOPSIS
       getNiml(data,columnNames,parameters,respiratory_phases,cardiac_phases)
    ARGUMENTS
        data:   <class 'list'> containing coefficients determined by equation 4 of Glover (2000)
        
        parameters:   Dictionary with the following fields.
        
            -respFile:   file containing respiratory time series
            
            -cardFile:   file containing cardiac time series
            
        respiratory_phases:   <class 'list'> containing the respiratory phases
        
        cardiac_phases    :   <class 'list'> containing the cardiac phases
        
    AUTHOR
       Peter Lauren
    """
    
    global NUM_RVT
    
    respiration_file = parameters['-respFile']
    # Estimate RVT
    respiration_info = dict()
    respiration_info["amp_phase"] = 1
    respiration_info["frequency_cutoff"] = 3
    respiration_peak, error = peak_finder(respiration_info, respiration_file)
    respiration_info.update(respiration_peak)
    respiration_phased = phase_estimator(respiration_info["amp_phase"], respiration_info)
    respiration_phased["legacy_transform"] = 0
    respiration_phased["rvt_shifts"] = range(0,NUM_RVT)
    respiration_phased["rvtrs_slc"] = 0
    respiration_phased["time_series_time"] = np.abs(respiratory_phases)
    respiration_phased["interpolation_style"] = "linear"
    rvt = rvt_from_peakfinder(respiration_phased)
    rvt['number_of_slices'] = int(parameters['-cardFile'])
    numSlices = rvt['number_of_slices']
    respiration_info.update(rvt)
    respiration_info.update(respiration_peak)
    respiration_info.update(respiration_phased)
    
    head = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = '
        % ((shape(respiration_info["rvtrs_slc"])[0]+8)*numSlices, int(parameters['-Nt']))
    )
    
    label = head
    reml_out = []
    dataArray = np.array(data)
    offset = 0
    for i in range(0, numSlices):
        # RVT
        for j in range(0, shape(respiration_info["rvtrs_slc"])[0]):
            reml_out.append(
                respiration_info["rvtrs_slc"][j][0:8]
            )  # same regressor for each slice
            label = "%s s%d.RVT%d ;" % (label, i, j)
        # Resp
        for j in range(0, 4):
            reml_out.append(
                dataArray[j+offset,:]
                )
            label = "%s s%d.Resp%d ;" % (label, i, j)
        # Card
        offset += 4
        for j in range(0,4):
            reml_out.append(
                dataArray[j+offset,:]
            )  # same regressor for each slice
            label = "%s s%d.Card%d ;" % (label, i, j)
        offset += 4
                
    tail = '"\n>'
    tailclose = "</RetroTSout>"
    
    fid = open(("%s.slibase.1D" % '/home/peterlauren/retroicor/out'), "w")
    savetxt(
        "%s.slibase.1D" % '/home/peterlauren/retroicor/out',
        column_stack(reml_out),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (label, tail)),
        footer=("%s" % tailclose),
    )
    
    print('Theoretical number of time points: ', len(cardiac_phases)/(float(parameters['-Sr'])*float(parameters['-TR'])))
    print('Actual number of time points: ', parameters['-Nt'])

    return 0

def getFourierSeries(parameters):
    """
    NAME
        getFourierSeries 
            Return Fourier series of time series in cardiac and respiratory files
    TYPE
        <class 'list'>
    SYNOPSIS
       getFourierSeries(parameters)
    ARGUMENTS
        parameters:   Dictionary with the following fields.
        
            -respFile:   file containing respiratory time series
            
            -cardFile:   file containing cardiac time series
    AUTHOR
       Peter Lauren
    """
    
    cardiac_peaks, fullLength = getCardiacPeaks(parameters) 
    
    cardiac_phases = determineCardiacPhases(cardiac_peaks, fullLength)
    
    respiratory_peaks, respiratory_troughs, fullLength = \
        getRespiratoryPeaks(parameters) 
    
    respiratory_phases = determineRespiratoryPhases(parameters, \
            respiratory_peaks, respiratory_troughs)
        
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
    
    GLOBAL_M
    global numSections
        
    # Make data matrix
    data = []
    T = len(respiratory_phases)
    print('T = ', T)
    for t in range(0,T):
        sum = 0
        addend = []
        for m in range(1,GLOBAL_M):
            m0 = m - 1
            addend.append(respiratoryACoeffs[m0]*math.cos(m*respiratory_phases[t]))
            addend.append(respiratoryBCoeffs[m0]*math.sin(m*respiratory_phases[t]))
        for m in range(1,GLOBAL_M):
            m0 = m - 1
            addend.append(cardiacACoeffs[m0]*math.cos(m*cardiac_phases[t]))
            addend.append(cardiacBCoeffs[m0]*math.sin(m*cardiac_phases[t]))
        data.append(addend)
        
    return data

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

    # rawData = readArray(parameters, '-cardFile')
    rawData = readRawInputData(parameters, parameters["-cardFile"], parameters["phys_resp_dat"])
    
    cardiac_peaks, fullLength = getCardiacPeaks(parameters, rawData) 
    
    cardiac_phases = determineCardiacPhases(cardiac_peaks, fullLength,\
                                            parameters['-phys_fs'], rawData)

    # rawData = readArray(parameters, '-respFile')
    rawData = readRawInputData(parameters, parameters["-respFile"], parameters["phys_resp_dat"])
    
    respiratory_peaks, respiratory_troughs, fullLength = \
        getRespiratoryPeaks(parameters, rawData) 
    
    respiratory_phases = determineRespiratoryPhases(parameters, \
            respiratory_peaks, respiratory_troughs, parameters['-phys_fs'], rawData)
        
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
    for s in range(0,numSections):
        for r in range(0,4):
            string = 's' + str(s) + '.Resp' + str(r)
            columnNames.append(string)
    for s in range(0,numSections):
        for r in range(0,4):
            string = 's' + str(s) + '.Card' + str(r)
            columnNames.append(string)
        
    # Make output table data matrix
    data = []
    T = len(respiratory_phases)
    print('T = ', T)
    for t in range(0,T):
        sum = 0
        addend = []
        for m in range(1,GLOBAL_M):
            m0 = m - 1
            addend.append(respiratoryACoeffs[m0]*math.cos(m*respiratory_phases[t]))
            addend.append(respiratoryBCoeffs[m0]*math.sin(m*respiratory_phases[t]))
        for m in range(1,GLOBAL_M):
            m0 = m - 1
            addend.append(cardiacACoeffs[m0]*math.cos(m*cardiac_phases[t]))
            addend.append(cardiacBCoeffs[m0]*math.sin(m*cardiac_phases[t]))
        data.append(addend)
        
    # Reshape matrix according to number of slices
    nrow = shape(data)[0]
    data = np.reshape(data[0:nrow-(nrow%numSections)][:], (8*numSections, -1)).T
    
    if (parameters['-niml']):
        niml = getNiml(data,columnNames,parameters, respiratory_phases, cardiac_phases)
        return niml
    
    df = pd.DataFrame(data,columns=columnNames)
        
    return df   

def phase_estimator(amp_phase, phase_info):
    """
    NAME
        phase_estimator 
            Return phases of respiratory and cardiac time series and RVT or respiratory time series
    TYPE
        <class 'dict'>
    SYNOPSIS
       phase_estimator(amp_phase, phase_info)
    ARGUMENTS
        amp_phase:   Integer. Currently always 1 meaning amplitude-based phase for respiration 
        
        phase_info:   Dictionary with the following fields.
        
            respiration_file:  Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            legacy_transform:   Important-this will specify whether you use the 
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py  (default is 0)
            
            respcard:   Whether the respirator or cardiac tmie series is being processed
            
            t:   Time (Seconds)
            
            
            x:   Analytic signal
            
            iz:   Peak and trough locations
            
            p_trace:   Peak values
            
            tp_trace:   Peak locations
            
            n_trace:   Trough values
            
            tn_trace:   Trough locations
            
            prd:   Array of intervals between peaks
            
            t_mid_prd -Array of mean values betweem successive peaks
            
            p_trace_mid_prd:   Array of midpoints betweem successive peaks
            
            phase:   Array of phase values
            
            rv:   Peak to trough differences over interpeak intervals
            
            rvt:   Peak to trough differences over interpeak intervals
            
            v:   Scale, per Glover 2000's paper 
            
            tR:   Progressive array of step intervals (Straight line)
            
            p_trace_r:   Interpoalted peaks
            
            n_trace_r:   Interpolated troughs
            
            prdR:   Interval between peaks interpolated to temporal range of 
            
            phase_info["v_np"] but with time warping specified by phase_info["tn_trace"]
            
            amp_phase:   Currently always 1
            
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """
        
    # Phase determination parameters
    phasee = dict(
        quiet=0,
        frequency_cutoff=10,
        fir_order=80,
        demo=0,
        phasee_list=0,
        show_graphs=0,
    )
    phasee.update(phase_info)
    if isinstance(phasee["phasee_list"], type([])): # No phase list
        return_phase = []
        return_phase_list = []
        for phasee_column in phasee["phasee_list"]:
            return_phase.append(phase_base(amp_phase, phasee_column))
        return return_phase_list
    else:
        return_phase, rvt = phase_base(amp_phase, phasee)   # Actually determine the phase
        return return_phase, rvt

def my_hist(x, bin_centers):
    """
    NAME
        my_hist 
            Conversion from bin-edges to bin-centers is from Stack Overflow user Bas Swinckels
            http://stackoverflow.com/questions/18065951/why-does-numpy-histogram-python-leave-off-one-element-as-compared-to-hist-in-m
    TYPE
        Array of <class 'int'>
    SYNOPSIS
       my_hist(x, bin_centers)
    ARGUMENTS
        x:   dataset
        
        bin_centers:   bin values in a list to be moved from edges to centers
        
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """

    bin_edges = np.r_[-np.Inf, 0.5 * (bin_centers[:-1] + bin_centers[1:]), np.Inf]
    counts, edges = np.histogram(x, bin_edges)
    return counts

def z_scale(x, lower_bound, upper_bound, perc=[]):
    """
    NAME
        z_scale 
            Scales  X into Y such that its maximum value is UB and minimum value 
            is LB. If perc is specified, then clipping is done at the percentile 
            range specified (e.g. [2, 98]) before scaling.  If X is all 
            constants, it gets scaled to UB;
    TYPE
        <class 'numpy.ndarray'>
    SYNOPSIS
       z_scale(x, lower_bound, upper_bound, perc=[])
    ARGUMENTS
       x:   Dataset
       
       lower_bound:   Minimum peak value
       
       upper_bound:   Maximum peak value
       
       perc:   Lower and upper clipping bounds for input dataset.
       
    AUTHOR
       Ziad (Documentation by Peter Lauren)
    """

    print('x = ', x)
    if type(x) != type(array):
        if type(x) != type(np.ndarray):
            x = np.array(x).reshape(-1)
        else:
            x = array(x)
    print('x = ', x)
    if upper_bound < lower_bound:
        print("Error z_scale: Upper bound < Lower bound")
        return
    if perc:
        lower_clip = math.percentile(x, perc[0])
        upper_clip = math.percentile(x, perc[1])
        x = x.clip(lower_clip, upper_clip)

    xmin = min(x)
    xmax = max(x)

    if xmin == xmax:
        # If x is all constants, then scale up to upper_bound value
        y = array(size(x))
        y.fill(upper_bound)
    else:
        # If x is not all constants, then scale to bounds
        y = (((x - xmin) / (xmax - xmin)) * (upper_bound - lower_bound)) + lower_bound
    return y

def phase_base(amp_type, phasee):
    """
    NAME
        phase_base         
            Called by phase_estimator to determine phases of respiratory and cardiac 
            time series and RVT or respiratory time series
    TYPE
        <class 'dict'>, <class 'numpy.ndarray'> 
    SYNOPSIS
       phase_base(amp_type, phasee)
    ARGUMENTS    
        amp_type:   if 0, it is a time-based phase estimation if 1, it is an 
        amplitude-based phase estimation
            
        phasee:   Dictionary with the following fields.
        
            quiet:   0 if show graphs. 1 if do not show graphs
            
            frequency_cutoff:   Cutoff frequency for smoothing RVT
            
            fir_order:   Order of Finite Impulse Response (FIR) filter
            
            demo:   Whether running in demo mode.  (Show graphs and pause between graphs.)
            
            phasee_list:   Whether there is a phase list
            
            show_graphs:   Whether to show graphs
            
            respiration_file:   Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            legacy_transform:   Important-this will specify whether you use the            
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py
            (default is 0)
                   
            respcard:   Whether the respirator or cardiac tmie series is being processed
            
            t:   Time (Seconds)
            
            x:   Analytic signal
            
            iz:   Peak and trough locations
            
            p_trace:   Peak values
            
            tp_trace:   Peak locations
            
            n_trace:   Trough values
            
            tn_trace:   Trough locations
            
            prd:   Array of intervals between peaks
            
            t_mid_prd -Array of mean values betweem successive peaks
            
            p_trace_mid_prd:   Array of midpoints betweem successive peaks
            
            phase:   Array of phase values
            
            rv:   Peak to trough differences over interpeak intervals
            
            rvt:   Peak to trough differences over interpeak intervals
            
            v -Scale, per Glover 2000's paper 
            
            tR:   Progressive array of step intervals (Straight line)
            
            p_trace_r:   Interpoalted peaks
            
            n_trace_r:   Interpolated troughs
            
            prdR:   Interval between peaks interpolated to temporal range of             
            phase_info["v_np"] but with time warping specified by phase_info["tn_trace"]
                
            amp_phase:   Currently always 1
            
            
            The function adds the following fields.
            
            phase_pol:   Positive if inspiration.  Negative if expiration
            
            time_series_time:   Time series time vector
            
            phase_slice:   Phase sampled at slice acquisition time
            
            phase_slice_reg:   Regressors for each slice:   
                
            rvr:   Differences betwen interpolated peaks
            
            rvtr:   Elementw-se quotient of phasee["rvr"] over phasee["prdR"]
            
            rvtrs:   Demeaned and Hamming filtered version of phasee["rvtr"] with 
            mean subsequently added back
                
            rvtrs_slc:   RVT from peak finder
            
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """
    
    peakLocations_RawData = np.round(phasee["tp_trace"]*phasee["phys_fs"]).astype(int)
    troughLocations_RawData = np.round(phasee["tn_trace"]*phasee["phys_fs"]).astype(int)
    

    if amp_type == 0:   # Phase not based on amplitude.  This is a buggy option 
                        #   and was hardcoded out by JZ/ZSS
        # Calculate the phase of the trace, with the peak to be the start of the phase
        nptrc = len(peakLocations_RawData) # Number of peaks
        phasee["phase"] = -2 * np.ones(size(phasee["t"]))   # Array of -2s, the length of the input data
        i = 0   # i increments over the peak indices
        j = 0   # j increments over the time point indices
        while i <= (nptrc - 2): # For each peak
            while phasee["t"][j] < peakLocations_RawData[i + 1]: # Examine signal between this and next peak
                if phasee["t"][j] >= peakLocations_RawData[i]:   # Must be at least the index of the current peak
                    # Note: Using a constant 244 period for each interval
                    # causes slope discontinuity within a period.
                    # One should resample period[i] so that it is
                    # estimated at each time in phasee['t'][j],
                    # dunno if that makes much of a difference in the end however.
                    if j == 10975:  # Don't do the following block for this temporal index
                        pass
                    phasee["phase"][j] = (
                        phasee["t"][j] - peakLocations_RawData[i]
                    ) / phasee["prd"][i] + phasee["zero_phase_offset"]
                    if phasee["phase"][j] < 0:
                        phasee["phase"][j] = -phasee["phase"][j]
                    if phasee["phase"][j] > 1:
                        phasee["phase"][j] -= 1
                j += 1
            if i == 124:
                pass
            i += 1

        # Remove the points flagged as unset
        temp = np.nonzero(phasee["phase"] < -1)
        phasee["phase"][temp] = 0.0
        # Change phase to radians
        phasee["phase"] = phasee["phase"] * 2 * math.pi
    else:  # phase based on amplitude (default)
    
        # Step 1: Calculate the histogram of the input values
        # at first scale to the max
        mxamp = max(phasee["p_trace"])  # Maximum peak value
        phasee["phase_pol"] = []        # Only used for amplitude-based phase
        gR = z_scale(phasee["v"], 0, mxamp)  # Scale, per Glover 2000's paper
        bins = np.arange(0.01, 1.01, 0.01) * mxamp # Hundred bins in 1% increments up to maximum peak
        hb_value = my_hist(gR, bins)    # Histogram input using bins
        if phasee["quiet"] == 0:  # Display histogram
            center = (bins[:-1] + bins[1:]) / 2
            plt.bar(center, hb_value[: len(hb_value) - 1])
            plt.xlabel("Input value")
            plt.ylabel("Count")
            plt.title("Histogram of input %s values" % (phasee['respcard']))
        
            # Save plot to file
            global OutDir
            plt.savefig('%s/%s_histogram.pdf' % (OutDir, phasee['respcard'])) 
            # plt.ion()
            plt.show()  # If this is left out, output file is blank
            
        # Step 2. Assign polarities to every input timepoint.  Positive polarity 
        #   is inspiration while negative polarity is expiration
            
        # find the polarity of each time point in v
        i = 0
        itp = 0
        inp = 0
        tp = peakLocations_RawData[0]  # Location of first peak
        tn = troughLocations_RawData[0]  # Location of first trough
        
        # Find location of first peak or trough
        while (
            (i <= len(phasee["v"])) and (phasee["t"][i] < tp) and (phasee["t"][i] < tn)
        ):
            phasee["phase_pol"].append(0)
            i += 1
                    
        if tp > tn: # If first peak precedes first trought
            # Expiring phase (peak behind us)
            cpol = -1   # Polarity of -1 means expiration
            # inp = 1
        else:       # First trough precedes first peak
            # Inspiring phase (bottom behind us)
            cpol = 1    # Polarity of 1 means inpiration
            # itp = 1
            
        phasee["phase_pol"] = np.zeros(
            size(phasee["v"])
        )  # Not sure why you would replace the
        # list that you created 10 lines prior to this
        
        # Add a fake point to tptrace and tntrace to avoid ugly if statements
        peakLocations_RawData = np.append(peakLocations_RawData, phasee["t"][-1])
        troughLocations_RawData = np.append(troughLocations_RawData, phasee["t"][-1])
        
        # Assign polarities to every input timepoint
        i = 0
        while i < len(phasee["v"]): # Process every input time point
            phasee["phase_pol"][i] = cpol
            # if phasee["t"][i] == peakLocations_RawData[itp]:
            if i == peakLocations_RawData[itp]:
                cpol = -1   # Expiration polarity
                itp = min((itp + 1), (len(peakLocations_RawData) - 1)) # Expiration minimum
            # elif phasee["t"][i] == troughLocations_RawData[inp]:
            elif i == troughLocations_RawData[inp]:
                cpol = 1   # Inspiration polarity
                inp = min((inp + 1), (len(troughLocations_RawData) - 1)) # Inspiration minimum
            i += 1  # Next point
            
        peakLocations_RawData = np.delete(peakLocations_RawData, -1) # Remove last peak
        troughLocations_RawData = np.delete(troughLocations_RawData, -1) # Remove last trough
        
        # Plot graphs if required
        if phasee["quiet"] == 0:
            # clf
            x= phasee['t']
            plt.plot(x, gR, "b")          
            plt.xlabel("time (s) aka phasee['t']")
            plt.ylabel("Input scaled to [0,max(input)](gR)")
            plt.title("%s input scaled per Grover (2000) versus time (s)"  % (phasee['respcard']))
            
            # Inspiration
            ipositive = np.positive(phasee["phase_pol"])
            # ipositive = ipositive[0]
            ipositive_x = []
            for i in range(0,len(ipositive)):
                ipositive_x.append(phasee["t"][i])
            ipositive_y = np.zeros(size(ipositive_x))
            ipositive_y.fill(0.55 * mxamp)
            plt.plot(ipositive_x, ipositive_y, "r.")
            
            # Exspiration
            inegative = np.negative(phasee["phase_pol"])
            # inegative = inegative[0]
            inegative_x = []
            for i in range(0,len(inegative)):
                inegative_x.append(phasee["t"][i])
            inegative_y = np.zeros(size(inegative_x))
            inegative_y.fill(0.45 * mxamp)
            plt.plot(inegative_x, inegative_y, "g.")
            plt.xlabel("time (s) aka phasee['t']")
            plt.ylabel("Input value (unscaled")
            plt.title("%s inspiration (red) and expiration (green) versus time (s)"\
                      % (phasee['respcard']))
        
            # Save plot to file
            plt.savefig('%s/%s_phaseInspExp.pdf' % (OutDir, phasee['respcard'])) 
            # plt.ion()
            plt.show()
            
            # Plot time series with inspiration in red and expiration in green
            Y = gR
            x_filt = (np.where(phasee["phase_pol"]>0))[0]
            subset = np.where(x_filt<2000)[0]
            x_filt = x_filt[subset]
            y_filt = Y[x_filt]
            plt.plot(x_filt, y_filt, "r.")          
            x2_filt = (np.where(phasee["phase_pol"]<0))[0]
            subset = np.where(x2_filt<2000)[0]
            x2_filt = x2_filt[subset]
            y2_filt = Y[x2_filt]
            plt.plot(x2_filt, y2_filt, "g.")          
            plt.xlabel("Raw input data index")
            plt.ylabel("Input scaled to [0,max(input)](gR)")
            plt.title("Inspiration (red) and expiration (green) versus pt.index for the first 2000 timepoints of the input data")
        
            # Save plot to file
            plt.savefig('%s/%s_TimePtInspExp.pdf' % (OutDir, phasee['respcard'])) 
            plt.show()
            
        # Now that we have the polarity, without computing sign(dR/dt)
        #   as in Glover et al 2000, calculate the phase per eq. 3 of that paper
        # First the sum in the numerator
        for i, val in enumerate(gR):
            gR[i] = round(val / mxamp * 100) + 1
        gR = np.clip(gR, 0, 99)
        shb = sum(hb_value)
        hbsum = []
        hbsum.append(float(hb_value[0]) / shb)
        
        # Next the sum in the denominator
        for i in range(1, 100):
            hbsum.append(hbsum[i - 1] + (float(hb_value[i]) / shb))
            
        # Next multiply by polarity
        for i in range(len(phasee["t"])):
            phasee["phase"].append(math.pi * hbsum[int(gR[i]) - 1] * phasee["phase_pol"][i])
        phasee["phase"] = np.array(phasee["phase"])

    # Time series time vector
    phasee["time_series_time"] = np.arange(
        0, (max(phasee["t"]) - 0.5 * phasee["volume_tr"]), phasee["volume_tr"]
    )
    # Python uses half open ranges, so we need to catch the case when the stop
    # is evenly divisible by the step and add one more to the time series in
    # order to match Matlab, which uses closed ranges  1 Jun 2017 [D Nielson]
    if (max(phasee["t"]) - 0.5 * phasee["volume_tr"]) % phasee["volume_tr"] == 0:
        phasee["time_series_time"].append(
            [phasee["time_series_time"][-1] + phasee["volume_tr"]],
        )
    phasee["phase_slice"] = np.zeros(
        (len(phasee["time_series_time"]), phasee["number_of_slices"])
    )
    phasee["phase_slice_reg"] = np.zeros(
        (len(phasee["time_series_time"]), 4, phasee["number_of_slices"])
    )
    for i_slice in range(phasee["number_of_slices"]):
        tslc = phasee["time_series_time"] + phasee["slice_offset"][i_slice]
        for i in range(len(phasee["time_series_time"])):
            imin = np.argmin(abs(tslc[i] - phasee["t"]))
            phasee["phase_slice"][i, i_slice] = phasee["phase"][imin]
        # Make regressors for each slice
        phasee["phase_slice_reg"][:, 0, i_slice] = np.sin(
            phasee["phase_slice"][:, i_slice]
        )
        phasee["phase_slice_reg"][:, 1, i_slice] = np.cos(
            phasee["phase_slice"][:, i_slice]
        )
        phasee["phase_slice_reg"][:, 2, i_slice] = np.sin(
            2 * phasee["phase_slice"][:, i_slice]
        )
        phasee["phase_slice_reg"][:, 3, i_slice] = np.cos(
            2 * phasee["phase_slice"][:, i_slice]
        )

    # Determine RVT
    rvt = rvt_from_peakfinder(phasee)
    
    # Return regressors, for each slice, and RVT
    return phasee, rvt

def rvt_from_peakfinder(r):
    """
    NAME
        rvt_from_peakfinder 
            Calculates RVT, and RVT regressors.  Optionally graphs results
    TYPE
        <class 'numpy.ndarray'>
    SYNOPSIS
       rvt_from_peakfinder(r)
    ARGUMENTS
        r:   Dictionary with the following fields.
        
            quiet:   0 if show graphs. 1 if do not show graphs
            
            frequency_cutoff:   Cutoff frequency for smoothing RVT
            
            fir_order:   Order of Finite Impulse Response (FIR) filter
            
            demo:   Whether running in demo mode.  (Show graphs and pause between graphs.)
            
            phasee_list:   Whether there is a phase list
            
            show_graphs:   Whether to show graphs
            
            respiration_file:   Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            legacy_transform:   Important-this will specify whether you use the
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py
            (default is 0)
                   
            respcard:   Whether the respirator or cardiac tmie series is being processed
            
            t:   Time (Seconds)
            
            x:   Analytic signal
            
            iz:   Peak and trough locations
            
            p_trace:   Peak values
            
            tp_trace:   eak locations
            
            n_trace:   Trough values
            
            tn_trace:   Trough locations
            
            prd:   Array of intervals between peaks
            
            t_mid_prd -Array of mean values betweem successive peaks
            
            p_trace_mid_prd:   Array of midpoints betweem successive peaks
            
            phase:   Array of phase values
            
            rv:   Peak to trough differences over interpeak intervals
            
            rvt:   Peak to trough differences over interpeak intervals
            
            v -Scale, per Glover 2000's paper 
            
            tR:   Progressive array of step intervals (Straight line)
            
            p_trace_r:   Interpoalted peaks
            
            n_trace_r:   Interpolated troughs
            
            prdR:   Interval between peaks interpolated to temporal range of 
            phase_info["v_np"] but with time warping specified by phase_info["tn_trace"]
            
            amp_phase:   Currently always 1
            
            phase_pol:   Positive if inspiration.  Negative if expiration
            
            time_series_time:   Time series time vector
            
            phase_slice:   Phase sampled at slice acquisition time
            
            phase_slice_reg:   Regressors for each slice 
            
            
            The function adds the following fields.
            
            rvr:   Differences betwen interpolated peaks
            
            rvtr:   Elementw-se quotient of phasee["rvr"] over phasee["prdR"]
            
            rvtrs:   Demeaned and Hamming filtered version of phasee["rvtr"] with 
            mean subsequently added back
            
            rvtrs_slc:   RVT from peak finder
            
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """

    if r["demo"]:
        quiet = 0
        print("quiet = %s" % quiet)
        
    # Calculate RVT
    if len(r["p_trace"]) != len(r["n_trace"]):
        dd = abs(len(r["p_trace"]) - len(r["n_trace"]))
        if dd > 1:  # have not seen this yet, trap for it.
            print(
                "Error RVT_from_PeakFinder:\n"
                "  Peak trace lengths differ by %d\n"
                "  This is unusual, please upload data\n"
                "  sample to afni.nimh.nih.gov" % dd
            )
            # keyboard
            return
        else:  # just a difference of 1, happens sometimes, seems ok to discard one sample
            print(
                "Notice RVT_from_PeakFinder:\n"
                "   Peak trace lengths differ by %d\n"
                "   Clipping longer trace." % dd
            )
            dm = min(len(r["p_trace"]), len(r["n_trace"])) # Minimum of # peaks and # troughs
            if len(r["p_trace"]) != dm: # More peaks than troughs
                r["p_trace"] = r["p_trace"][0:dm] # Reduce # peaks to # troughs
                r["tp_trace"] = r["tp_trace"][0:dm]
            else:
                r["n_trace"] = r["n_trace"][0:dm] # Reduce # troughs to # peaks

    r["rv"] = np.subtract(r["p_trace"], r["n_trace"]) # Differences between peaks and troughs
    
    # NEED TO consider which starts first and
    # whether to initialize first two values by means
    # and also, what to do when we are left with one
    # incomplete pair at the end
    nptrc = len(r["tp_trace"])  # Number of peaks
    r["rvt"] = r["rv"][0 : nptrc - 1] / r["prd"] # Peak to trough differences over interpeak intervals
    if r["p_trace_r"].any: # If interpoalted peaks
        r["rvr"] = np.subtract(r["p_trace_r"], r["n_trace_r"]) # Differences betwen interpolated peaks
                                                               #  and interpolated troughs
        r["rvtr"] = np.ndarray(np.shape(r["rvr"]))  # Make array, the same length as r["rvr"])
        np.divide(r["rvr"], r["prdR"], r["rvtr"])   # Elementwise divide r["rvr"] by r["prdR"]
                                                    # and put results in r["rvtr"]
        # Smooth RVT so that we can resample it at volume_tr later
        fnyq = r["phys_fs"] / 2  # nyquist of physio signal
        w = float(r["frequency_cutoff"]) / float(fnyq)  # cut off frequency normalized
        
        # Produce Hamming window of width r["fir_order"]
        b = firwin(numtaps=(r["fir_order"] + 1), cutoff=w, window="hamming")
        
        v = r["rvtr"]       # Quotient array determined above
        np.around(v, 6, v)  # Evebnly round to 6 decimals
        mv = np.mean(v)     # Determine mean
        v = v - mv          # remove the mean
        
        # filter both ways to cancel phase shift
        v = lfilter(b, 1, v)    # Apply Hamming filter
        if r["legacy_transform"] == 0:  # r["legacy_transform"] currently hardcoded to 0
            v = np.flipud(  # Flip array
                v
            )  # Turns out these don't do anything in the MATLAB version(Might be a major problem)
        v = lfilter(b, 1, v)    # Reapply Hamming filter
        if r["legacy_transform"] == 0:
            v = np.flipud(  # Flip back
                v
            )  # Turns out these don't do anything in the MATLAB version(Might be a major problem)
        r["rvtrs"] = v + mv             # Add mean back

    # create RVT regressors
    r["rvtrs_slc"] = np.zeros((len(r["rvt_shifts"]), len(r["time_series_time"]))) # 2D array of zeros
    for i in range(0, len(r["rvt_shifts"])):    # Process each row
        shf = r["rvt_shifts"][i]                # i-th RVT 
        nsamp = int(round(shf * r["phys_fs"]))  # i-th RVT times sample frequency
        sind = np.add(list(range(0, len(r["t"]))), nsamp) # array of integers from nsamp
                                                          # to nsamp times the number of samples
        sind[np.nonzero(sind < 0)] = 0          # Rectify result
        sind[np.nonzero(sind > (len(r["t"]) - 1))] = len(r["t"]) - 1 # Limit result to length of input
        rvt_shf = interp1d(   # Build function that maps r["t"] to r["rvtrs"][sind]
            r["t"], r["rvtrs"][sind], r["interpolation_style"], bounds_error=True
        )
        rvt_shf_y = rvt_shf(r["time_series_time"]) # Apply function to r["time_series_time"]
        if r["quiet"] == 0 and r["show_graphs"] == 1:
           # pacify matplotlib by passing a label (to get new instance)
           subplot(111)
           plot(r["time_series_time"], rvt_shf_y)
           plt.xlabel('Time (s)')
           plt.ylabel("y-shifted RVT")
           plt.title('plot #%d'%i)
        r["rvtrs_slc"][:][i] = rvt_shf_y

    if r["quiet"] == 0 and r["show_graphs"] == 1 and len(r["rvt"]) > 0:
        print("--> Calculated RVT \n--> Created RVT regressors")
        subplot(211)
        plot(r["t_mid_prd"], z_scale(r["rvt"], min(r["p_trace"]), max(r["p_trace"])), "b")
        plt.xlabel('Time (s)')
        plt.ylabel("RVT")
        plt.title('Created RVT regressors')
        if any(r["p_trace_r"]):
            plot(
                r["tR"], z_scale(r["rvtrs"], min(r["p_trace"]), max(r["p_trace"])), "m"
            )
            plt.xlabel('Time (s)')
            plt.ylabel("RVT")
            plt.title('Created RVT regressors using interpolated peaks')
        show()
        if r["demo"]:
            # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
            pass

    return r['rvtrs_slc']

def fftsegs(ww, po, nv):
    """
    NAME
        fftsegs 
            Returns the segements that are to be used for fft calculations.
            Example: (bli, ble, num) = fftsegs (100, 70, 1000)
    TYPE
        <class 'list'>, <class 'list'>, <class 'numpy.ndarray'>
    SYNOPSIS
      fftsegs(ww, po, nv)
    ARGUMENTS
    
        ww:   Segment width (in number of samples)
        
        po:   Percent segment overlap
        
        nv:   Total number of samples in original symbol
        
    EXAMPLES
        return: (bli, ble, num)
            bli, ble: Two Nblck x 1 vectors defining the segments' starting and ending indices;
            num: An nv x 1 vector containing the number of segments each sample belongs to
        return_tuple = (ww, po, nv)
        return return_tuple
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
"""

    if ww == 0:
        po = 0
        ww = nv
    elif nv < ww < 32:
        print("Error fftsegs: Bad value for window width of %d" % ww)
        return
    out = 0
    while out == 0:
        # clear bli ble
        bli = []
        ble = []
        # % How many blocks?
        jmp = np.floor((100 - po) * ww / 100)  # jump from block to block
        nblck = nv / jmp
        ib = 0
        cnt = 0
        while ble == [] or ble[-1] < (nv - 1):
            bli.append(ib)
            ble.append(min(ib + ww - 1, nv))
            cnt += 1
            ib += jmp
        # If the last block is too small, spread the love
        if ble[-1] - bli[-1] < (0.1 * ww):  # Too small a last block, merge
            ble[-2] = ble[-1]  # into previous
            cnt -= 1
            del ble[-1]
            del bli[-1]
            out = 1
        elif ble[-1] - bli[-1] < (0.75 * ww):  # Too large to merge, spread it
            ww = ww + np.floor((ble[-1] - bli[-1]) / nblck)
            out = 0
        else:  # Last block big enough, proceed
            out = 1

    # now figure out the number of estimates each point of the time series gets
    num = np.zeros((nv, 1))
    cnt = 1
    while cnt <= len(ble):
        num[bli[-1] : ble[-1] + 1] = num[bli[-1] : ble[-1] + 1] + np.ones(
            (ble[-1] - bli[-1] + 1, 1)
        )
        cnt += 1
    return bli, ble, num

def analytic_signal(vi, windwidth, percover, win):
    """
    NAME
        analytic_signal 
            Returns the analytic signal
    TYPE
          <class 'numpy.ndarray'>
    SYNOPSIS
          analytic_signal(vi, windwidth, percover, win)
    ARGUMENTS
        vi:   Demeaned input which has typically been Hamming filtered in both directions
        
        windwidth:   Window for adjusting peak location in seconds
        
        percover:   Percent segment overlap
        
        win:   Whether to apply Hamming window before doing FT? (1=Yes, 0 - No)
        
    EXAMPLES
        r["x"] = analytic_signal(
        v_np
        var_vector["as_window_width"] * var_vector["phys_fs"],
        var_vector["as_percover"],
        var_vector["as_fftwin"],
        )
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
"""
    nvi = len(vi)   # Length of input array

    # Get segments used for FFT calculations
    # bli, ble: Two (Nblck = nvi / jmp) x 1 vectors defining the segments' starting and ending indices
    # jmp is the between blocks jump
    # num: An nv x 1 vector containing the number of segments each sample belongs to
    bli, ble, num = fftsegs(windwidth, percover, nvi)
          
    for ii in range(len(bli)): # Iterate over the fft segments
        v = vi[bli[ii] : ble[ii] + 1]   # Segment ii
        nv = len(v)                     # Length of segment ii
        if win == 1:                    # Apply Hamming window before doing FT?
            fv = fft(v * np.hamming(nv))
        else:
            fv = fft(v)
        wind = np.zeros(v.size)         # Initialize output window to all zeros
        # zero negative frequencies, double positive frequencies
        if nv % 2 == 0: # Length of segemnet is even
            wind[0] = 1  # keep DC component
            wind[(nv // 2)] = 1
            wind[1 : (nv // 2)] = 2  # double pos. freq

        else:           # Length of segment odd
            wind[0] = 1  # keep DC component
            wind[list(range(1, (nv + 1) // 2))] = 2
        h = ifft(fv * wind) # Get inverse FT
    for i in range(len(h)):
        h[i] /= np.complex(num[i])  # Return array are complex numbers with imagimary components set to zero
    return h

def remove_pn_duplicates(tp, vp, tn, vn, quiet):
    """
    NAME
        remove_pn_duplicates 
            Remove adjacent array points that are the same or very close
    TYPE
        <class 'numpy.ndarray'>, <class 'numpy.ndarray'>, <class 'numpy.ndarray'>, <class 'numpy.ndarray'>
    SYNOPSIS
          remove_pn_duplicates(tp, vp, tn, vn, quiet)
    ARGUMENTS
        tp, vp, tn, vn:   Four arrays to be filtered of adjacent duplicates.  Typically
        
        tp:   Peak indices
        
        vp:   Peak values
        
        tn:   Trough locations
        
        vn:   Trough values
        
        quiet:   0 if graphs are to be displayed. 1 if they are not
        
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
"""
    ok = np.zeros((1, len(tp)), dtype=np.int32)
    ok = ok[0]
    j = 0
    for i in range(1, min(len(tp), len(tn))):
        #  0.3 is the minimum time before the next beat
        if (tp[i] != tp[i - 1]) and (tn[i] != tn[i - 1]) and (tp[i] - tp[i - 1] > 0.3):
            j += 1
            if j == 127:
                print("stop")
            ok[j] = i
        else:
            if not quiet:
                print("Dropped peak at %s sec" % tp[i])
    ok = ok[: j + 1]
    tp = tp[ok]
    vp = vp[ok]
    tn = tn[ok]
    vn = vn[ok]
    return tp, vp, tn, vn

def remove_duplicates(t, v, quiet):
    """
    NAME
        remove_duplicates 
            Remove adjacent array points that are the same or very close
    TYPE
        <class 'numpy.ndarray'>, <class 'numpy.ndarray'>, <class 'numpy.ndarray'>, <class 'numpy.ndarray'>
    SYNOPSIS
          remove_duplicates(t, v, quiet)
    ARGUMENTS
        t, v:   Two arrays to be filtered of adjacent duplicates.  Typically:
            
        t:   Time (Seconds)
        
        v:   Scale, per Glover 2000's paper
        
        quiet:   0 if graphs are to be displayed. 1 if they are not
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
       """
    j = 0
    for i in range(1, len(t) + 1):
        #  0.3 is the minimum time before the next beat
        if t[i] != t[i - 1] and (t[i] - t[i - 1]) > 0.3:
            j += 1
            t[j] = t[i]
            v[j] = v[i]
        elif quiet == 0:
            print("Dropped peak at %s sec" % t[i])
    t = t[: j + 1]
    v = v[: j + 1]
    return t, v

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
        
            respiration_file:   Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            legacy_transform:   Important-this will specify whether you use the
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py
            (default is 0)
                   
        filename:   String giving the local name of the input file
        
        phys_dat:   BIDS style physio file
        
    AUTHOR
       Joshua Zosky & Peter Lauren (Documentation by Peter Lauren)
    """
    # #clear all but var_vector (useful if I run this function as as script)
    # keep('var_vector', 'opt')
    var_vector = dict(
        phys_fs=(1 / 0.025),
        zero_phase_offset=0.5,
        quiet=0,
        resample_fs=(1 / 0.025),
        frequency_cutoff=10,
        fir_order=80,
        interpolation_style="linear",
        demo=0,
        as_window_width=0,
        as_percover=0,
        as_fftwin=0,
        sep_dups=0,
    )
    var_vector.update(respcard_info)
    default_div = 1 / 0.025
    if (var_vector["phys_fs"] != default_div) and (
        var_vector["resample_fs"] == default_div
    ):
        var_vector["resample_fs"] = var_vector["phys_fs"]
    if var_vector["demo"]:
        var_vector["quiet"] = 0

    r = {}
    # Some filtering
    nyquist_filter = var_vector["phys_fs"] / 2.0
    w = var_vector["frequency_cutoff"] / nyquist_filter
    b = firwin(
        numtaps=(var_vector["fir_order"] + 1), cutoff=w, window="hamming"
    )  # FIR filter of order 40
    b = np.array(b)

    nl = 1  # temporary, delete this line when above lines get fixed with glob
    # del(r) # "Must clear it. Or next line fails" -- Probably unnecessary in Python
    r_list = []
    for i in range(nl):
        r = {
            "v_name": filename,
            "t": [],
            "x": [],
            "iz": [],  # zero crossing (peak) locations
            "p_trace": [],
            "tp_trace": [],
            "n_trace": [],
            "tn_trace": [],
            "prd": [],
            "t_mid_prd": [],
            "p_trace_mid_prd": [],
            "phase": [],
            "rv": [],
            "rvt": [],
        }
        r_list.append(r)
    """
    for i_column in range(nl):
        if L and not os.path.isdir(L):
            r_list[i_column]['v_name'] = '%s%s' % (sys.path, L[i_column]['name'])"""
    
    if phys_dat is None: # No BIDS style physio file
        # Read repiration/cardiac file into phys_dat
        phys_dat = []
        with open(r["v_name"], "rb") as h:
            for line in h:
                phys_dat.append(float(line.strip()))
                
    print('r["v_name"] = ', r["v_name"])
    print('phys_dat = ', phys_dat)
    
    v_np = np.asarray(phys_dat)
    
    # Trim leading datapoints if they precede start time
    if ('StartTime' in respcard_info and respcard_info['StartTime']<0):
        start_index = round(-respcard_info['StartTime'] * respcard_info["phys_fs"])
        print('start_index = ', start_index)
        v_np = v_np[start_index:-1]
    
    return v_np


def peak_finder(respcard_info, v_np):
    """
    NAME
        peak_finder 
            Find peaks in respiritory or cardiac raw data. Retirns updated dictionary 
            and whether there is an error
    TYPE
        <class 'dict'>, <class 'bool'> 
    OUTPUT
        The output dictionary contains the following fields
        
            t:   Time (Seconds)
            
            x:   Analytic signal
            
            iz:   Peak and trough locations
            
            p_trace:   Peak values
            
            tp_trace:   eak locations
            
            n_trace:   Trough values
            
            tn_trace:   Trough locations
            
            prd:   Array of intervals between peaks
            
            t_mid_prd: Array of mean values betweem successive peaks
            
            p_trace_mid_prd:   Array of midpoints betweem successive peaks
            
            phase:   Array of phase values
            
            rv:   Peak to trough differences over interpeak intervals
            
            rvt:   Peak to trough differences over interpeak intervals
            
            v -Scale, per Glover 2000's paper 
            
            tR:   Progressive array of step intervals (Straight line)
            
            p_trace_r:   Interpoalted peaks
            
            n_trace_r:   Interpolated troughs
            
            prdR:   Interval between peaks interpolated to temporal range of 
            phase_info["v_np"] but with time warping specified by phase_info["tn_trace"]
            
    SYNOPSIS
        peak_finder(respcard_info, v_np)
    ARGUMENTS
        respcard_info:   Dictionary with the following fields.
        
            respiration_file:   Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            legacy_transform:   Important-this will specify whether you use the
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py
            (default is 0)
            
            respcard:   Whether the respirator or cardiac tmie series is being processed
            
        v_np:   The raw input data
        
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """
    
    e = False  # default value for e
    nl = 1  # temporary, delete this line when above lines get fixed with glob
    
    var_vector = dict(
        quiet=0,
        resample_fs=(1 / 0.025),
        frequency_cutoff=10,
        fir_order=80,
        demo=0,
        as_window_width=0,
        as_percover=0,
        as_fftwin=0,
        sep_dups=0,
    )
    var_vector.update(respcard_info) # Add info. from respiratory or cradiac info.
    default_div = 1 / 0.025          # Default frequency is 40 Hz
    
    # Update frequency if given as argument
    if (var_vector["phys_fs"] != default_div) and ( 
        var_vector["resample_fs"] == default_div
    ):
        var_vector["resample_fs"] = var_vector["phys_fs"]
    if var_vector["demo"]:
        var_vector["quiet"] = 0 # Make verbose if demo mode
    
    nyquist_filter = var_vector["phys_fs"] / 2.0 # The Nyquist frequency is half the sampling rate
    
    # Frequency cutoff over Nyquist frequency to avoid aliasing
    w = var_vector["frequency_cutoff"] / nyquist_filter 
    
    # A finite impulse response (FIR) filter is a filter whose impulse response (or 
    #   response to any finite length input) is of finite duration, because it 
    #   settles to zero in finite time.
    b = firwin(
        numtaps=(var_vector["fir_order"] + 1), cutoff=w, window="hamming"
    )  # FIR filter of order 40
    for i in range(nl): # nl is currently (temporarily) hard coded at 1
        r = {
            "t": [],
            "x": [],
            "iz": [],  # zero crossing (peak) locations
            "p_trace": [],
            "tp_trace": [],
            "n_trace": [],
            "tn_trace": [],
            "prd": [],
            "t_mid_prd": [],
            "p_trace_mid_prd": [],
            "phase": [],
            "rv": [],
            "rvt": [],
        }
    no_dups = 1  # Remove duplicates that might come up when improving peak location
  
    window_width = 0.2  # Window for adjusting peak location in seconds
    
    # Remove the mean
    v_np_mean = np.mean(v_np)
    v_np = v_np - v_np_mean
    r["v"] = v_np  # Store result for debugging (found it is used in phase estimator)
    
    # Filter both ways to cancel phase shift
    v_np = lfilter(b, 1, v_np, axis=0)  # Apply Hamming window FIR filter to v_np array
    v_np = np.flipud(v_np)              # Flip v_np array
    v_np = lfilter(b, 1, v_np)          # Reapply Hamming window FIR filter
    v_np = np.flipud(v_np)              # Flip v_np array back
    
    # Get the analytic signal.  (Local function)
    r["x"] = analytic_signal(
        v_np,
        var_vector["as_window_width"] * var_vector["phys_fs"],
        var_vector["as_percover"],
        var_vector["as_fftwin"],
    )

    # Using local version to illustrate, can use hilbert
    # Doing ffts over smaller windows can improve peak detection in the few instances that go undetected but
    # what value to use is not clear and there seems to be at times more errors introduced in the lower envelope.
    nt = len(r["x"])

    # force 't' to have the same length as 'x', rather than trusting
    # divisions (when it should come out evenly)  5 Jun, 2017 [rickr]
    fsi = 1.0 / var_vector["phys_fs"]
    r["t"] = np.array([i * fsi for i in range(len(r["x"]))])

    # Rectify imaginary points on r["x"] where next imaginary point is not positive
    iz = np.nonzero((r["x"][0 : nt - 1].imag * r["x"][1:nt].imag) <= 0)
    # Get first array of result.  This approximates a straight line with a positive slope.
    # Maybe reflects the phase (although it is not the phase)
    # More likely integral of oxygenation over time.
    iz = iz[0]
    
    # Sign of difference between current imaginary point and previous imaginary point
    polall = -np.sign(r["x"][0 : nt - 1].imag - r["x"][1:nt].imag)
    
    pk = (r["x"][iz]).real           # Peaks and troughs with original values
    pol = polall[iz]                 # Peaks and troughs with peaks set to 1 and troughs to -1
    
    # Time warped time points
    tiz = []  
    for i in iz:
        tiz.append(r["t"][i])        
        
    # Get peak values and locations
    ppp = np.nonzero(pol > 0)
    ppp = ppp[0]    # Indices of peaks
    p_trace = []
    tp_trace = []    
    for i in ppp:
        p_trace.append(pk[i])   # Value of peaks
        tp_trace.append(tiz[i]) # Location of peaks
    
    # Get trough values and locations
    ppp = np.nonzero(pol < 0)
    ppp = ppp[0]    # Indices of troughs 
    n_trace = []
    tn_trace = []
    for i in ppp:
        n_trace.append(pk[i])        # Value of troughs
        tn_trace.append(tiz[i])      # Location of troughs

    # Plot peaks and troughs
    if var_vector["quiet"] == 0:
        print(
            "--> Load signal\n--> Smooth signal\n--> Calculate analytic signal Z\n--> Find zero crossing of imag(Z)\n"
        )
        figure(1)
        subplot(211)
        plot(r["t"], np.real(r["x"]), "g") #Lines connecting peaks and troughs
        plot(tp_trace, p_trace, "ro")   # Peak locations and values
        plot(tn_trace, n_trace, "bo")   # Trough locations and values
        plt.xlabel("time (s)")
        plt.ylabel("Re(Input Signal)")
        plt.title("%s peaks (red), troughs (blue), and lines (green) connecting them"\
                  % (respcard_info['respcard']), fontdict={'fontsize': 10})
        
        subplot(413)
        vn = np.real(r["x"]) / (abs(r["x"]) + np.spacing(1))
        plot(r["t"], vn, "g")
        ppp = np.nonzero(pol > 0)
        ppp = ppp[0]
        for i in ppp:
            plot(tiz[i], vn[iz[i]], "ro")   # Peak locations with values set to 1
        ppp = np.nonzero(pol < 0)
        ppp = ppp[0]
        for i in ppp:
            plot(tiz[i], vn[iz[i]], "bo")   # Trough locations with values set to -1
        plt.xlabel("time (s)")
        
        # Save plot to file
        global OutDir
        # PDF currently chosen over SVG because the files are much smaller with 
        #   undetectable loss of quality
        plt.savefig('%s/%s_peaks.pdf' % (OutDir, respcard_info['respcard'])) 
        # plt.savefig('%s/peaks.svg' % (OutDir)) 
        # plt.ion()
        plt.show()  # If this is left out, output file is blank
        
        # Pause for demo
        if var_vector["demo"]:
            # need to add a pause here - JZ
            # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
            pass

    # Some polishing
    if 1 == 1:
        nww = np.ceil((window_width / 2) * var_vector["phys_fs"]) # window_width currently hard-coded to 0.2
        pkp = pk             # Current peaks and troughs with original values
        r["iz"] = iz         # Peak and trough locations
        for i in range(len(iz)):
            ###################### left off here, turns out there's a
            # difference in floating point precision in the calculation
            # of r['x'], maybe look into the reason why they'd be different

            # force these to ints    17 May 2017 [DNielson]
            n0 = int(max(2, iz[i] - nww))
            n1 = int(min(nt, iz[i] + nww))
            temp = (r["x"][n0 : n1 + 1]).real # Real components of r["x"] where r["x"] is the analytic signal
            if pol[i] > 0:  # Peak
                xx, ixx = np.max(temp, 0), np.argmax(temp, 0)  # Value and index of peak
            else:           # Trough
                xx, ixx = np.min(temp, 0), np.argmin(temp, 0)  # Value and index of trough
            r["iz"][i] = n0 + ixx - 1   # Refined index
            pkp[i] = xx                 # Refined value
            if i == 100:
                print("pause")
        tizp = r["t"][r["iz"]]          # Refined indices of peaks and troughs

        # Get refined peaks
        ppp = np.nonzero(pol > 0)
        ppp = ppp[0]
        r["p_trace"] = pkp[ppp]         # Peak value
        r["tp_trace"] = tizp[ppp]       # Peak location
        
        # Get refined troughs
        ppp = np.nonzero(pol < 0)
        ppp = ppp[0]
        r["n_trace"] = pkp[ppp]         # Trough value
        r["tn_trace"] = tizp[ppp]       # Trough location

        if no_dups:
            # remove duplicates
            if var_vector["sep_dups"]:
                print("YOU SHOULD NOT BE USING THIS.\n")
                print(" left here for the record\n")
                [r["tp_trace"], r["p_trace"]] = remove_duplicates(
                    r["tp_trace"], r["p_trace"], var_vector["quiet"]
                )
                [r["tn_trace"], r["n_trace"]] = remove_duplicates(
                    r["tn_trace"], r["n_trace"], var_vector["quiet"]
                )
            else:
                r["tp_trace"], r["p_trace"], r["tn_trace"], r[
                    "n_trace"
                ] = remove_pn_duplicates(
                    r["tp_trace"],
                    r["p_trace"],
                    r["tn_trace"],
                    r["n_trace"],
                    var_vector["quiet"],
                )
            if len(r["p_trace"]) != len(r["n_trace"]):  # Number of troughs different from number of peaks
                print("Number of troughs different from number of peaks.\n")
                e = True
                return r, e

        if var_vector["quiet"] == 0:
            print("--> Improved peak location\n--> Removed duplicates")
            subplot(211)
            # Peaks with actual values
            plot(r["tp_trace"], r["p_trace"], "r+", r["tp_trace"], r["p_trace"], "r")
            # Troughs with actual values
            plot(r["tn_trace"], r["n_trace"], "b+", r["tn_trace"], r["n_trace"], "b")
            plt.xlabel("time (s)")
            plt.ylabel("Re(Input signal)")
            plt.title("%s peak envelope (red) and trough envelope (blue)"\
                      % (respcard_info['respcard']), fontdict={'fontsize': 11})
        
            # Save plot to file
            # PDF currently chosen over SVG because the files are much smaller with 
            #   undetectable loss of quality
            plt.savefig('%s/%s_peakEnvelopes.pdf' % (OutDir, respcard_info['respcard'])) 
            # plt.ion()
            plt.show()  # If this is left out, output file is blank
            
            if var_vector["demo"]:
                # need to add a pause here - JZ
                # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
                pass
    else:       # Not currently used
        tizp = tiz
        r["iz"] = iz
        pkp = pk
        r["p_trace"] = p_trace
        r[
            "n_trace"
        ] = (
            n_trace
        )  # This seems like a mistake, the .m file's version is highly suspect

    # Calculate the period
    nptrc = len(r["tp_trace"])  # Peak indices
    print(r["tp_trace"])
    r["prd"] = r["tp_trace"][1:nptrc] - r["tp_trace"][0 : nptrc - 1]    # Array of intervals between peaks
    r["p_trace_mid_prd"] = (r["p_trace"][1:nptrc] + r["p_trace"][0 : nptrc - 1]) / 2.0 # Array of midpoints betweem successive peaks
    r["t_mid_prd"] = (r["tp_trace"][1:nptrc] + r["tp_trace"][0 : nptrc - 1]) / 2.0 # Array of mean values betweem successive peaks
    if var_vector["quiet"] == 0:
        print("--> Calculated the period (from beat to beat)\n")
        subplot(211)
        plot(r["t_mid_prd"], r["p_trace_mid_prd"], "kx")
        fontSize = round(235.0/len(r["prd"]))
        fontSize = min(max(fontSize,12), 5)
        for i in range(0, len(r["prd"])):
            text(r["t_mid_prd"][i], r["p_trace_mid_prd"][i], ("%.2f" % r["prd"][i]),\
                 fontsize = fontSize, fontweight="light")
        plt.xlabel("time (s)")
        plt.ylabel("Midpoints between successive peaks")
        plt.title("%s intervals between peaks" % (respcard_info['respcard']))
        
        # Save plot to file
        plt.savefig('%s/%s_peakIntervals.pdf' % (OutDir, respcard_info['respcard'])) 
        # plt.ion()
        plt.show()  # If this is left out, output file is blank
        
        if var_vector["demo"]:
            # need to add a pause here - JZ
            # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
            pass
        show()

    # Interpolate
    if var_vector["interpolation_style"] != "":
        # Interpolate to slice sampling time grid:
        step_interval = 1.0 / var_vector["resample_fs"] # Multiplicative inverse of -freq

        # allow for a ratio that is barely below an integer
        #                               5 Jun, 2017 [rickr]
        step_size = int(max(r["t"]) / step_interval + g_epsilon) + 1  # g_epsilon hardcoded to 0.00001
        r["tR"] = []

        # Make progressive array of step intervals (Straight line)
        for i in range(0, step_size):
            r["tR"].append(i * step_interval)

        r["p_trace_r"] = interp1d(
            r["tp_trace"],
            r["p_trace"],
            var_vector["interpolation_style"],
            bounds_error=False,
        )
        r["p_trace_r"] = r["p_trace_r"](r["tR"])

        # Interpolate r["n_trace"] to temporal range of v_np but with time warping specified by r["tn_trace"]
        r["n_trace_r"] = interp1d(
            r["tn_trace"],
            r["n_trace"],
            var_vector["interpolation_style"],
            bounds_error=False,
        )(r["tR"])

        # Interpolate r["prd"] to temporal range of v_np but with time warping specified by r["tn_trace"]
        r["prdR"] = interp1d(
            r["t_mid_prd"],
            r["prd"],
            var_vector["interpolation_style"],
            bounds_error=False,
        )(r["tR"])
        # You get NaN when tR exceeds original signal time, so set those
        # to the last interpolated value
        r["p_trace_r"] = clean_resamp(r["p_trace_r"])
        r["n_trace_r"] = clean_resamp(r["n_trace_r"])
        r["prdR"] = clean_resamp(r["prdR"])

    return r, e

def clean_resamp(v):
    """
    NAME
        clean_resamp 
            Set NaN to the last interpolated value
    TYPE
        <class 'numpy.ndarray'>
    SYNOPSIS
      clean_resamp(v)
    ARGUMENTS
        v:   Array to be filtered
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
       """
       
    i_nan = np.nonzero(np.isnan(v))  # the bad
    i_nan = i_nan[0]
    i_good = np.nonzero(np.isfinite(v))  # the good
    i_good = i_good[0]
    for i in range(0, len(i_nan)):
        if i_nan[i] < i_good[0]:
            v[i_nan[i]] = v[i_good[0]]
        elif i_nan[i] > i_good[-1]:
            v[i_nan[i]] = v[i_good[-1]]
        else:
            print("Error: Unexpected NaN case")
            v[i_nan[i]] = 0
    return v


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

def show_rvt_peak(r, fg):
    """
    NAME
        show_rvt_peak 
            Generate plots of respiratory RVT and of phase sampled at slice acquisition time
    TYPE
        void
    SYNOPSIS
       show_rvt_peak(r, fg)
    ARGUMENTS
        r:   Dictionary with the following fields.
        
            respiration_file:   Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            legacy_transform:   Important-this will specify whether you use the
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py
            (default is 0)
                   
            respcard:   Whether the respirator or cardiac tmie series is being processed
            
            t:   Time (Seconds)
            
            x:   Analytic signal
            
            iz:   Peak and trough locations
            
            p_trace:   Peak values
            
            tp_trace:   eak locations
            
            n_trace:   Trough values
            
            tn_trace:   Trough locations
            
            prd:   Array of intervals between peaks
            
            t_mid_prd -Array of mean values betweem successive peaks
            
            p_trace_mid_prd:   Array of midpoints betweem successive peaks
            
            phase:   Array of phase values
            
            rv:   Peak to trough differences over interpeak intervals
            
            rvt:   Peak to trough differences over interpeak intervals
            
            v -Scale, per Glover 2000's paper 
            
            tR:   Progressive array of step intervals (Straight line)
            
            p_trace_r:   Interpoalted peaks
            
            n_trace_r:   Interpolated troughs
            
            prdR:   Interval between peaks interpolated to temporal range of 
            phase_info["v_np"] but with time warping specified by phase_info["tn_trace"]
                
            amp_phase:   Currently always 1
            
            The function adds the following fields.
            
            phase_pol:   Positive if inspiration.  Negative if expiration
            
            time_series_time:   Time series time vector
            
            phase_slice:   Phase sampled at slice acquisition time
            
        fg:   unique identifier for the figure
        
    AUTHOR
       Joshua Zosky. Debugged by Peter Lauren (Documentation by Peter Lauren)
    """

    rvt_peak_plot = figure(fg)
    rvt_peak_plot.clf()
    # set(fg, 'KeyPressFcn', @afni_fig_interface)  # Appears to be unnecessary to retain from MATLAB code
    subplot(211)
    plot(r["t"], real(r["x"]), "g")

    if r["rvt"]:
        subplot(211)
        plot(
            r["t_mid_prd"], z_scale(r["rvt"], min(r["p_trace"]), max(r["p_trace"])), "k"
        )
    plot(r["tp_trace"], r["p_trace"], "ro", r["tp_trace"], r["p_trace"], "r")
    plot(r["tn_trace"], r["n_trace"], "bo", r["tn_trace"], r["n_trace"], "b")
    plot(r["t_mid_prd"], r["p_trace_mid_prd"], "kx")
    fontSize = round(235.0/len(r["prd"]))
    fontSize = min(max(fontSize,12), 5)
    for i in range(len(r["prd"])):
        text(r["t_mid_prd"][i], r["p_trace_mid_prd"][i], "%.2f" % r["prd"][i],\
             fontsize = fontSize, fontweight="light")
    if len(r["tR"])>0:
        if len(r["p_trace_r"])>0:
            plot(r["tR"], r["p_trace_r"], "m")
            plot(r["tR"], r["n_trace_r"], "y")
        if 'rvtrs' in r.keys() and len(r["rvtrs"])>0:
            plot(
                r["tR"], z_scale(r["rvtrs"], min(r["p_trace"]), max(r["p_trace"])), "k."
            )
    plt.xlabel("time (sec)")
    plt.title(r["v_name"])  # , 'Interpreter', 'None')
    subplot(414)
    vn = real(r["x"]) / (abs(r["x"]) + spacing(1))
    plot(r["t"], vn, "g")
    plot(r["t"], list(element / 2 / pi for element in r["phase"]), "m")
    if "phase_r" in r:
        plot(r["tR"], list(element / 2 / pi for element in r["phase_r"]), "m-.")
    plt.xlabel("time (sec)")
    plt.title("Scaled by magnitude of analytical signal")  # , 'Interpreter', 'None')
    plt.legend(["Scaled signal", "phase"])
    if "phase_slice" in r.keys():
        subplot(414)
        spotSize=3
        plot(
            r["time_series_time"], r["phase_slice"][:, 0], "ro", markersize=spotSize
        )  # This one isn't any different than the next plot line, check out why values are funky
        plot(
            r["time_series_time"], r["phase_slice"][:, 1], "bo", markersize=spotSize
        )
        plot(r["time_series_time"], r["phase_slice"][:, 1], "b-")
    plot(r["t"], r["phase"])
    plt.grid("on")
    plt.xlabel("time (sec)")
    plt.title("Phase sampled at slice acquisition time")
    plt.legend(["slice 0", "slice 1", "slice 2", "original phase"], prop={'size': 6}, loc='center right')
    # plotsign2(fg)  # Another AFNI function, not crucial at the moment to making one figure
       
    # Save plot to file
    global OutDir
    plt.savefig('%s/RVT.pdf' % (OutDir)) 
    show()


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
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            frequency_cutoff:   Cutoff frequency for smoothing RVT
            
            zero_phase_offset:Phase offset added to the location of each peak.
            Default is 0.0
            
            legacy_transform:   Important-this will specify whether you use the 
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py  (default is 0)
            
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
       JPeter Lauren
    """
            
    # Handle file inputs
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
                   if not respiration_info["phys_fs"]:
                       cardiac_info['phys_fs'] = phys_meta['SamplingFrequency']
                   cardiac_file = None
                   phys_cardiac_dat = phys_dat[k]
            else:
                print("** warning phys data contains column '%s', but\n" \
                      "   RetroTS only handles cardiac or respiratory data" % k)
    else:   # Not a JSON file
        if respiration_info["respiration_file"]:
            respiration_file = respiration_info["respiration_file"]
            phys_resp_dat = None
        if cardiac_info["cardiac_file"]:
            cardiac_file = cardiac_info["cardiac_file"]
            phys_cardiac_dat = None
            
    return respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat

