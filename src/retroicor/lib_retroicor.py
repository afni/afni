__authors__ = "Peter Lauren and Josh Zosky"

"""
    2022 Peter Lauren
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
import matplotlib.pyplot as plt
import math                        # possible to remove this, use all numpy
import scipy
from   scipy import signal as sps
import gzip
import json
from   matplotlib.ticker import FormatStrFormatter

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
            Read an array from an input file specified by the key in the 
            parameters field
    TYPE
        <class 'list'>
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   (dType = str) key to file of interest
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
   ARGUMENTS
       parameters:   dictionary of input parameters which includes the following 
                     fields.
       
           phys_fs: (dType = float) Sampling frequency in Hz
           
           verbose: (dtype = bool) Whether running in verbose mode.  (Save graphs, 
                    of intermediate steps, to disk.)
           
       rawData: (dType = float, array) Raw cardiac data
       
       filterPercentile: (dType = float) Minimum percentile of raw data for a 
                           peak value to imply a valid peak
   AUTHOR
       Peter Lauren
   """
   
   global OutDir
   
   # Check for nan's
   failureThreshold = parameters['phys_fs'] / 4 # Consecutive NaNs cannot cover 
                                                # more than about 0.25 s
   rawData = lpf.checkForNans(rawData, "cardiac", 
                              failureThreshold = failureThreshold)
   if len(rawData) == 0:
       print('*** ERROR: Could not handle all of the NaN values')
   
   MIN_HEART_RATE = 25  # Minimum HR in BPM
    
   # Determine lower bound based based on minimum HR
   minBeatsPerSecond = MIN_HEART_RATE/60
   
   # Remove NaNs from raw data
   rawData = [x for x in rawData if math.isnan(x) == False]
   
   # Initialise graph index
   graphIndex = 0
    
   # Band pass filter raw data
   filterData = lpf.bandPassFilterRawDataAroundDominantFrequency(rawData,\
         minBeatsPerSecond,parameters["phys_fs"],\
         show_graph = parameters['show_graphs']>1,\
         save_graph = parameters["save_graphs"]>1, OutDir=OutDir,\
         graphIndex = graphIndex,  font_size = parameters['font_size'])
   if len(filterData) == 0:
       print('Failed to band-pass filter cardiac data')   
       return []
   
   # Get initial peaks using window that is a fortieth of a second (HR <= 680 BPM)
   peaks, _ = sps.find_peaks(np.array(filterData),\
                             width=int(parameters["phys_fs"]/40))
   
   # Graph initial peaks and save graph to disk
   lpf.graphPeaksAgainstRawInput(parameters['show_graphs']>1, 
        parameters["save_graphs"]>1, rawData, peaks, parameters["phys_fs"],  
        "Cardiac", OutDir = OutDir, prefix = 'cardiacPeaksFromBPFInput', 
        caption = 'Cardiac peaks from band-pass filtered input.',
        font_size = parameters['font_size'])
   
   # Adjust peaks from uniform spacing
   peaks = lpf.refinePeakLocations(peaks, rawData, 
             dataType = "Cardiac",  phys_fs = parameters["phys_fs"], 
            show_graph = parameters['show_graphs']>1, 
            save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
            font_size = parameters['font_size'])
    
   # Remove peaks less than the required percentile of the local input signal
   peaks = lpf.localPercentileFilter(peaks, rawData, filterPercentile, 
            numPeriods=3, show_graph = parameters['show_graphs']>1, 
            save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
            phys_fs = parameters["phys_fs"], OutDir = OutDir,
            font_size = parameters['font_size'])
   if len(peaks) == 0:
        print('*** ERROR: Failure to local percentile filter cardiac peaks')
        return [], 0

   # Estimate the overall typical period using filtered cardiac time series
   period = lpf.getTimeSeriesPeriod(filterData) 
   if period < 0:     
        print('*** ERROR: Failure to get typical period using filtered cardiac time series')
        return [], 0
    
   # Merge peaks that are closer than one quarter of the overall typical period
   peaks = lpf.removeClosePeaks(peaks, period, rawData, 
        show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
   
   # Remove "peaks" that are less than the raw input a quarter of a period on 
   #    right side
   # This is tomove false peaks on the upstroke
   # searchLength = round(parameters["phys_fs"]/16)
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData,  
        period=period, show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
   if len(peaks) == 0:
       print('ERROR in getCardiacPeaks: Peaks array empty')
       return peaks, len(rawData)
    
   # Remove false peaks on the downstroke
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
        direction='left', period=period, 
        show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
    
   # Remove peaks that are less than a quarter as far from the local minimum to 
   #  the adjacent peaks
   peaks = lpf.removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData, 
        show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])

   # Add missing peaks
   peaks = lpf.addMissingPeaks(peaks, rawData, period=period, 
                show_graph = max(parameters['show_graphs']-1,0), 
                save_graph = max(parameters["save_graphs"]-1,0), 
                phys_fs = parameters["phys_fs"], OutDir = OutDir,
                font_size = parameters['font_size'])   
   
   # Remove "peaks" that are less than the raw input a quarter of a period on 
   #  right side
   # This is tomove false peaks on the upstroke
   # searchLength = round(parameters["phys_fs"]/16)
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
        period=period, show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
   if len(peaks) == 0:
       print('ERROR in getCardiacPeaks: Peaks array empty')
       return peaks, len(rawData)
    
   # Remove false peaks on the downstroke
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
        direction='left', period=period, 
        show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Cardiac",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
      
   # Graph cardiac peaks against respiratory time series
   lpf.graphPeaksAgainstRawInput(parameters['show_graphs']>0, 
         parameters["save_graphs"]>0, rawData, peaks, parameters["phys_fs"], 
         "Cardiac", OutDir = OutDir, prefix = 'cardiacPeaksFinal', 
         caption = 'Cardiac peaks after all filtering.',
         font_size = parameters['font_size'])
    
   return peaks, len(rawData)


def getRespiratoryPeaks(parameters, rawData):
    """
    NAME
        getRespiratoryPeaks
        Get peaks from repiratory time series supplied as an ASCII 
        file with one time series entry per line
    TYPE
        <class 'numpy.ndarray'>, int
    ARGUMENTS
        parameters:   dictionary of input parameters which includes the 
                      following fields.
    
        rawData: (dType = float, array) Raw cardiac data
    AUTHOR
    Peter Lauren
    """

    # DEBUG
    oldArray = rawData
    
    global OutDir
   
    # Check for nan's
    rawData = lpf.checkForNans(rawData, "Respiratory")
   
    # Set maximum breathing period of 10 seconds
    MAX_BREATHING_PERIOD_IN_SECONDS = 10
    
    # Determine lower bound based based on minimum HR
    minBreathsPerSecond = 1.0/MAX_BREATHING_PERIOD_IN_SECONDS
   
    # Band pass filter raw data
    filterData = lpf.bandPassFilterRawDataAroundDominantFrequency(rawData, 
        minBreathsPerSecond,
        parameters["phys_fs"], show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, OutDir=OutDir, 
        dataType = "Respiratory", font_size = parameters['font_size'])
    if len(filterData) == 0:
       print('Failed to band-pass filter cardiac data')   
       return []
   
    # Get initial peaks using window that is eighth of a second  (BR <+ 480 BPM)
    peaks, _ = sps.find_peaks(np.array(filterData), 
                              width=int(parameters["phys_fs"]/4))
   
    # Graph initial peaks and save graph to disk
    lpf.graphPeaksAgainstRawInput(parameters['show_graphs']>1, 
        parameters["save_graphs"]>1, rawData, peaks, parameters["phys_fs"], 
        "Respiratory", 
         OutDir = OutDir, prefix = 'respiratoryPeaksFromBPFInput', 
         caption = 'Respiratory peaks from band-pass filtered input.',
         font_size = parameters['font_size'])
   
    # Adjust peaks from uniform spacing
    peaks = lpf.refinePeakLocations(peaks, rawData, 
             dataType = "Respiratory",  phys_fs = parameters["phys_fs"], 
            show_graph = parameters['show_graphs']>1, 
            save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
            font_size = parameters['font_size'])
    
    # Get period from filtered input data 
    period = lpf.getTimeSeriesPeriod(filterData)
    if period < 0:     
        print('*** ERROR: Failure to get typical period using filtered'+
              ' respiratory time series')
        return [], [], 0
    
    # Remove peaks that are less than the 10th percentile of the input signal
    peaks = lpf.percentileFilter(peaks, rawData, percentile=10.0, 
             dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])
    if len(peaks) == 0:
        print('*** ERROR: Failure to percentile filter respiratory peaks')
        return [], [], 0
    
    # Remove "peaks" that are less than the raw input a quarter of a period on 
    # right side.  This is tomove false peaks on the upstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
             period=period, dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])
    
    # Remove false peaks on the downstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
             direction='left', period=period, dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])
    
    # Remove peaks that are less than a quarter as far from the local minimum to 
    #  the adjacent peaks
    peaks = lpf.removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, rawData, 
             dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])
    
    # Merge peaks that are closer than one quarter of the overall typical period
    peaks = lpf.removeClosePeaks(peaks, period, rawData, 
             dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])

    # Add missing peaks
    peaks = lpf.addMissingPeaks(peaks, rawData, period=period, 
             dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])   
    
    # Remove "peaks" that are less than the raw input a quarter of a period on 
    # right side.  This is tomove false peaks on the upstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
             period=period, dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])
    
    # Remove false peaks on the downstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, rawData, 
             direction='left', period=period, dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], 
             show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, OutDir = OutDir,
             font_size = parameters['font_size'])
    
    troughs, _ = sps.find_peaks(-np.array(filterData), 
                                width=int(parameters["phys_fs"]/8))
   
    # Graph initial peaks and save graph to disk
    lpf.graphPeaksAgainstRawInput(parameters['show_graphs']>1, 
        parameters["save_graphs"]>1, rawData, peaks, parameters["phys_fs"], 
        "Respiratory", troughs = troughs, OutDir = OutDir, 
         prefix = 'respiratoryPeaksFromBPFInput', 
         caption = 'Respiratory troughs from band-pass filtered input.',
         font_size = parameters['font_size'])
    
    # Remove troughs that are more than the 90th percentile of the input signal
    troughs = lpf.percentileFilter(troughs, rawData, percentile=90.0, 
             upperThreshold=True, show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], OutDir = OutDir,
             font_size = parameters['font_size'])
    if len(troughs) == 0:
        print('*** ERROR: Failure to percentile filter respiratory troughs')
        return [], [], 0
    
    # Remove "troughs" that are greater than the raw input a quarter of a period 
    # on right side.  This is to remove false troughs on the downstroke
    troughs = lpf.removeTroughsCloseToLowerPointInRawData(troughs, rawData, 
             period=period, show_graph = parameters['show_graphs']>1, 
             save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
             phys_fs = parameters["phys_fs"], OutDir = OutDir,
             font_size = parameters['font_size'])
    
    # Remove false troughs on the uptroke
    troughs = lpf.removeTroughsCloseToLowerPointInRawData(troughs, rawData,\
            period=period, direction = 'left', 
            show_graph = parameters['show_graphs']>1, 
            save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
            phys_fs = parameters["phys_fs"], OutDir = OutDir,
            font_size = parameters['font_size'])
    
    # Remove troughs that are less than a quarter as far from the local maximum 
    # to the adjacent troughs
    troughs = lpf.removeTroughsCloserToLocalMaxsThanToAdjacentTroughs(troughs, 
        rawData, show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
    
    # Merge troughs that are closer than a quarter of the overall typical period
    troughs = lpf.removeClosePeaks(troughs, period, rawData, Troughs = True, 
        show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
    
    # Remove peaks/troughs that are also troughs/peaks
    peaks, troughs = lpf.removeOverlappingPeaksAndTroughs(peaks, troughs, rawData, 
        show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
    
    # Add missing peaks and troughs
    peaks, troughs = lpf.addMissingPeaksAndTroughs(peaks, troughs, 
        rawData, period=None, show_graph = parameters['show_graphs']>1, 
        save_graph = parameters["save_graphs"]>1, dataType = "Respiratory",  
        phys_fs = parameters["phys_fs"], OutDir = OutDir,
        font_size = parameters['font_size'])
   
    # Adjust troughs from uniform spacing
    troughs = lpf.refinePeakLocations(troughs, rawData, 
            show_graph = parameters['show_graphs']>1, 
            save_graph = parameters["save_graphs"]>1, 
             dataType = "Respiratory",  phys_fs = parameters["phys_fs"], 
             Troughs = True, OutDir = OutDir,
             font_size = parameters['font_size'])
    
    # Graph respiratory peaks and troughs against respiratory time series
    lpf.graphPeaksAgainstRawInput(parameters['show_graphs']>0, 
        parameters["save_graphs"]>0, rawData, peaks, parameters["phys_fs"], 
        "Respiratory", troughs = troughs, 
        caption = 'Respiratory peaks after all filtering.', OutDir = OutDir,
        prefix = 'respiratoryPeaksAndTroughsFinal',
        font_size = parameters['font_size'])
     
    return peaks, troughs, len(rawData)

def determineCardiacPhases(peaks, fullLength, phys_fs, rawData, 
                           show_graph = False, save_graph = True,
                           font_size = 10):
    """
    NAME
       determineCardiacPhases
         Determine phases, in the cardiac cycle based on the Glover (2000) paper
    TYPE
        <class 'list'>
    ARGUMENTS
        peaks:   (dType int64 array) Peaks in input cardiac time series.
        
        fullLength:   (dType = int) Lenagth of cardiac time series
        
        phys_fs:   (dType = float) Physiological signal sampling frequency in Hz. 
        
        rawData: (dType = float, array) Raw cardiac data
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
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
     
    if show_graph or save_graph:
        # PLot phases
        x = []    
        end = min(len(phases),round(len(phases)*50.0/len(peaks)))
        for i in range(0,end): x.append(i/phys_fs)
        fig, ax_left = plt.subplots()
        plt.xlabel("Time (s)", fontdict={'fontsize': font_size})
        plt.ylabel('Input data input value',color='g', 
                   fontdict={'fontsize': font_size})
        ax_right = ax_left.twinx()
        ax_right.plot(x, phases[0:end], color='red')
        ax_left.plot(x, rawData[0:end], color='green')
        plt.ylabel('Phase (Radians)',color='r')
        plt.title("Cardiac phase (red) and raw input data (green)",
                         fontdict={'fontsize': font_size})
            
        # Save plot to file
        if save_graph:
            plt.savefig('%s/CardiacPhaseVRawInput.pdf' % (OutDir)) 
            plt.show(block=False)
            if not show_graph: plt.close()  # Close graph after saving
            
    return phases

def getACoeffs(parameters, key, phases):
    """
    NAME
       getACoeffs
           Determine a coefficients from equation 4 of Glover (2000) paper 
           (equation 4)
    TYPE
        <class 'list'>
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   key to file of interest
        
        phases    :   <class 'list'> containing phases determined as described 
                      in Glover (2000) paper
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
           Determine b coefficients from equation 4 of Glover (2000) paper 
           (equation 4)
    TYPE
        <class 'list'>
    ARGUMENTS
        parameters:   dictionary of input parameters which includes a 'key' field.
        
        key       :   key to file of interest
        
        phases    :   <class 'list'> containing phases determined as described 
                      in Glover (2000) paper
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
            

def determineRespiratoryPhases(parameters, resp_peaks, resp_troughs, 
                    phys_fs, rawData, show_graph = False, save_graph = True,
                    font_size = 10):
    """
    NAME
        determineRespiratoryPhases
            Determine respiratory phases as descibed in Glover (2000) paper 
            (equation 3)
    TYPE
        <class 'list'>
    ARGUMENTS
        parameters:   dictionary of input parameters which includes the 
          following fields.
            respFile;   Name of ASCII file with respiratory time series
             phys_fs:     Physiological signal sampling frequency in Hz.
        
        resp_peaks      :   peaks in respiratory time series.  
                            Type = <class 'numpy.ndarray'>
        
        resp_troughs    :   <class 'numpy.ndarray'> containing troughs in the 
                            respiratory time series
        
        phys_fs:     Physiological signal sampling frequency in Hz 
        
        rawData:     Raw respiratory data
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
    AUTHOR
       Peter Lauren
    """
    
    rawData = rawData - min(rawData)
    
    NUM_BINS = 100
    
    # Determine whether currently inspiration or expiration
    if resp_peaks[0] < resp_troughs[0]:
        polarity = 1
    else:
        polarity = -1
        
    # Number of segments where each segment is either inspiration or expiration
    numFullSegments = len(resp_peaks) + len(resp_troughs) - 1
    
    # Initialize array of output phases
    phases = np.zeros(len(rawData))
    
    # Assign values to time series before first full segment
    peakIndex = 0
    troughIndex = np.argmin(resp_troughs)
    start = 0
    finish = min(resp_peaks[peakIndex], resp_troughs[troughIndex])
    if finish == 0:
        finish = max(resp_peaks[peakIndex], resp_troughs[troughIndex])
    denom = finish  # Total length of segment
    
    # Histogram values in segment
    sample = [x - rawData[resp_troughs[troughIndex]] 
              for x in rawData[start:finish]] 

    counts, bins = np.histogram(sample, bins=NUM_BINS) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity > 0: Rmax = max(sample)
    else: Rmax = rawData[resp_peaks[0]] # Maximum value in segment
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
        start = min(resp_peaks[peakIndex], resp_troughs[troughIndex])
        finish = max(resp_peaks[peakIndex], resp_troughs[troughIndex])
        denom = finish - start  # Total length of segment
        
        # Histogram values in segment
        sample = [x - rawData[resp_troughs[troughIndex]] 
                  for x in rawData[start:finish]] 
        sample = sample - min(sample)
        counts, bins = np.histogram([x 
                    for x in sample if math.isnan(x) == False], bins=NUM_BINS) 
        
        # Determine phase based on equation 3 is Glover paper
        Rmax = max(sample) # Maximum value in segment
        for i in range(start,finish): # Move through segment
            end = round(sample[i-start]*NUM_BINS/Rmax) # Summation limit
            
            # Count values, in segment that are <= the summation limit
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
        if peakIndex>=len(resp_peaks) or troughIndex>=len(resp_troughs): break
    
    # Assign values to time series after last full segment
    start = finish
    finish = len(rawData)
    denom = finish - start  # Total length of segment
    
    # Histogram values in segment
    sample = [x - rawData[resp_troughs[-1]] for x in rawData[start:finish]]  
    counts, bins = np.histogram(sample, bins=NUM_BINS) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity < 0: Rmax = max(sample)
    else: Rmax = rawData[resp_peaks[-1]] # Maximum value in segment
    for i in range(start,finish): # Move through segment
        end = round(sample[i-start]*NUM_BINS/Rmax) # Summation limit
        
        if end >= len(counts):
            end = len(counts) - 1
        
        # Count values, in segment that are not greater than the summation limit
        count = 0
        for j in range(0,end):
            count = count + counts[j]
            
        # Use result to estimate phase at given time point
        phases[i] = (math.pi*count*polarity)/denom
    
    if show_graph or save_graph:  
        # PLot phases and raw data against time in seconds.
        peakVals = []
        for i in resp_peaks: peakVals.append(rawData[i])
        troughVals = []
        for i in resp_troughs: troughVals.append(rawData[i])
        x = []    
        end = min(len(phases),round(len(phases)*50.0/len(resp_peaks)))
        for i in range(0,end): x.append(i/phys_fs)
        fig, ax_left = plt.subplots()
        plt.xlabel("Time (s)", fontdict={'fontsize': font_size})
        plt.ylabel('Input data input value',color='g', 
                   fontdict={'fontsize': font_size})
        ax_right = ax_left.twinx()
        ax_right.plot(x, phases[0:end], color='red')
        ax_left.plot(x, rawData[0:end], color='green')
        plt.ylabel('Phase (Radians)',color='r')
        plt.title("Respiratory phase (red) and raw input data (green)",
                         fontdict={'fontsize': font_size})
            
        # Save plot to file
        plt.savefig('%s/RespiratoryPhaseVRawInput.pdf' % (OutDir)) 
        plt.show(block=False)
        if not show_graph: plt.close()  # Close graph after saving
    
        
    return phases

def getPhysiologicalNoiseComponents(parameters):
    """
    NAME
        getPhysiologicalNoiseComponents 
            Return physiological (respiratory and cardiac) contamination 
            components of BOLD signal
    TYPE
        Dictionary with the following fields
    ARGUMENTS
        parameters:   Dictionary with the following fields.
        
            respFile:   file containing respiratory time series
            
            cardFile:   file containing cardiac time series
            
            aby     : whether  a and b coefficients as per Glover et al, 
                       Magnetic Resonance in Medicine 44:162–167 (2000)
                                        
            niml    : whether output should be in niml format instead of CSV
            
            TR      : (dtype = class 'float') (volume_tr) Volume repetition 
                       time (TR) which defines the length of time            
                        
            num_time_pts:  (dType = int) Number of time points in the output
    AUTHOR
       Peter Lauren
    """
    
    # Initializations
    resp_phases = np.array([]) #None
    card_phases     = []           #
    
    # Parameters to read in raw data
    rawDataParams = dict()
    if 'StartTime' in parameters: 
        rawDataParams['StartTime'] = parameters['StartTime']
        if 'TR' in parameters: # Start-time/TR test
            print('TR shift in regressors = ', 
                  rawDataParams['StartTime'] - parameters['TR'])
    rawDataParams["phys_fs"] = parameters["phys_fs"]
    
    # Process cardiac data if any
    if parameters["cardFile"] or len(parameters["phys_card_dat"]) > 0:
        # rawData = readArray(parameters, 'cardFile')
        rawData = readRawInputData(rawDataParams, parameters["cardFile"], 
                                   parameters["phys_card_dat"])
        if len(rawData) == 0:
            print('Error reading input cardiac data')
            return []
                
        card_peaks, fullLength = getCardiacPeaks(parameters, rawData) 
        if len(card_peaks) == 0:
            print('ERROR in getPhysiologicalNoiseComponents: No cardiac peaks')
            return []
        
        if len(card_peaks) > 0:
            card_phases = determineCardiacPhases(card_peaks, fullLength,\
                    parameters['phys_fs'], rawData,  
                    show_graph = parameters['show_graphs']>0, 
                    save_graph = parameters['save_graphs']>0,
                    font_size = parameters['font_size'])
        
        # Ensure number of output time points not too high
        parameters['num_time_pts'] = limitNumOutputTimepoints(rawData, 
                                                              parameters)
        
    # Process respiratory data if any
    if parameters["respFile"] or len(parameters["phys_resp_dat"]) > 0:

        # rawData = readArray(parameters, 'respFile')
        rawData = readRawInputData(rawDataParams, parameters["respFile"], 
                                   parameters["phys_resp_dat"])
        if len(rawData) == 0:
            print('Error reading input respiratory data')
            return []
        
        resp_peaks, resp_troughs, fullLength = \
            getRespiratoryPeaks(parameters, rawData) 
        if len(resp_peaks) == 0:
            print('*** EOORO: Error getting respiratory peaks or troughs')
            return []            
        
        resp_phases = determineRespiratoryPhases(parameters, \
                resp_peaks, resp_troughs, parameters['phys_fs'], \
                    [x for x in rawData if math.isnan(x) == False], 
                    show_graph = parameters['show_graphs']>0, 
                    save_graph = parameters['save_graphs']>0,
                    font_size = parameters['font_size'])
        
        # Ensure number of output time points not too high
        parameters['num_time_pts'] = limitNumOutputTimepoints(rawData, 
                                                              parameters)
            
        if parameters['rvt_out']:
            rvt_coeffs = getRVT(rawData, resp_peaks, resp_troughs, 
                                parameters['phys_fs'],
                         parameters['num_time_pts'], parameters['TR'], 
                         show_graph = parameters['show_graphs']>0, 
                         save_graph = parameters['save_graphs']>0, 
                         interpolationOrder = 'linear', 
                         font_size = parameters['font_size'])
    else:
        if parameters['rvt_out']: 
            print('WARNING: Cannot determine RVT.  No respiratory data')
        parameters['rvt_out'] = False
        
    if (parameters['aby']):    # Determine a and b coefficients as per Glover et 
                        #  al, Magnetic Resonance in Medicine 44:162–167 (2000)
        # Get a coefficients
        cardiacACoeffs = getACoeffs(parameters, 'cardFile', card_phases)
        respiratoryACoeffs = getACoeffs(parameters, 'respFile', resp_phases)
        
        # Get b coefficients
        cardiacBCoeffs = getBCoeffs(parameters, 'cardFile', card_phases)
        respiratoryBCoeffs = getBCoeffs(parameters, 'respFile', resp_phases)
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
           
    # Make output table data matrix
    data = []
    len_resp = len(resp_phases)
    len_card = len(card_phases)

    if len_resp and len_card :
        T = min(len_resp, len_card)
    elif len_resp :
        T = len_resp
    elif len_card :
        T = len_card
    print('Maximum length of phase array = ', T)

    nreg = 0              # number of regressors we make
    for t in range(0,T):
        # sum = 0
        addend = []
        if len_resp :
            for m in range(1,GLOBAL_M):
                m0 = m - 1
                addend.append(respiratoryACoeffs[m0]*math.cos(m*resp_phases[t]))
                addend.append(respiratoryBCoeffs[m0]*math.sin(m*resp_phases[t]))
                if not(t):
                    nreg+= 2
        if len_card:
            for m in range(1,GLOBAL_M):
                m0 = m - 1
                addend.append(cardiacACoeffs[m0]*math.cos(m*card_phases[t]))
                addend.append(cardiacBCoeffs[m0]*math.sin(m*card_phases[t]))
                if not(t):
                    nreg+= 2
        data.append(addend)

    # Reshape matrix according to number of slices
    nrow = np.shape(data)[0]
    data = np.reshape(data[0:nrow-(nrow%numSections)][:], 
                      (nreg*numSections, -1)).T
        
    # return df   
    physiologicalNoiseComponents = dict()
    physiologicalNoiseComponents['resp_phases'] = resp_phases
    physiologicalNoiseComponents['card_phases'] = card_phases
    if parameters['rvt_out']: 
        physiologicalNoiseComponents['rvt_coeffs'] = rvt_coeffs
    return physiologicalNoiseComponents

def limitNumOutputTimepoints(rawData, parameters):
    """
    NAME
        limitNumOutputTimepoints
            Ensure number of output time points not too high
     TYPE
        <class 'int'>
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        parameters:   Dictionary with the following fields.
            phys_fs: (dType = float) Physiological signal sampling frequency (Hz)
            
            TR:       (dtype = class 'float') (volume_tr) Volume repetition  
                        time (TR) which defines the length of time 
                        
            num_time_pts:  (dType = int) Number of time points in the output

    AUTHOR
        Peter Lauren
    """
    # DEBUG
    # Get maximum number of output time points
    # Num TR intervals covered by physio data (float)
    duration = len(rawData)/parameters['phys_fs']
    max_numTime_float = duration/parameters['TR'] 
    eps_nt = 0.1    # Tolerance for rounding up number of TRs (fraction of TR)
    max_numTime_pts = int(max_numTime_float + eps_nt)
    
    print("++ duration of physio signal:", duration)
    print("++ TR (MRI data)            :", parameters['TR'])
    print("++ number of TRs from physio:", max_numTime_float)
    print("++ number of TRs (as int)   :", max_numTime_pts)
    
    # If the user has supplied the number of output times points, it must not 
    #   be greater than the determined maximum
    if parameters['num_time_pts']: 
        if parameters['num_time_pts'] > max_numTime_pts:
            print('WARNING: num_time_pts argument too large for input data')
            print('  Adjusted to maximum allowable value, ', max_numTime_pts)
            parameters['num_time_pts'] = max_numTime_pts
    else: 
        parameters['num_time_pts'] = max_numTime_pts
        
    return parameters['num_time_pts']

def readRawInputData(respcard_info, filename=None, phys_dat=None):
    """
    NAME
        readRawInputData 
            Read in raw input data according to the user=supplied program 
            arguments. Outputs raw data.
    TYPE
        <class 'numpy.ndarray'> 
    ARGUMENTS
        respcard_info:   Dictionary with the following fields.
        
            StartTime:   (Optional)  Time at which the signal of interest starts
            
            phys_fs: (dType = float) Physiological signal sampling frequency (Hz)
                   
        filename:   String giving the local name of the input file
        
        phys_dat:   BIDS style physio file
        
    AUTHOR
       Peter Lauren
    """

    if phys_dat is None or len(phys_dat) == 0: # No BIDS style physio file
        # Read repiration/cardiac file into phys_dat
        phys_dat = []
        with open(filename, "rb") as h:
            lineNumber = 0
            for entry in h:
                lineNumber = lineNumber + 1
                try:
                    float(entry)
                    if float(entry) != 5000.0: # Some files have artifactual 
                                               # entries of 5000.0
                        phys_dat.append(float(entry))
                except:
                    print('*** ERROR: invalid, non-numeric data entry: ', entry, 
                          ' at line ', lineNumber, ' of ', filename)
                    return []
            for line in h:
                phys_dat.append(float(line.strip()))
                
    v_np = np.asarray(phys_dat)
    
    # Trim leading datapoints if they precede start time
    if ('StartTime' in respcard_info and respcard_info['StartTime']<0):
        start_index = round(-respcard_info['StartTime'] * 
                            respcard_info["phys_fs"])
        print('start_index = ', start_index)
        v_np = v_np[start_index:-1]
    
    return v_np

def runAnalysis(parameters):
    """
    NAME
        runAnalysis 
            Run retroicor analysis as described by Glover (2000) and implemented 
            by Peter Lauren
    TYPE
        void
    ARGUMENTS
    
        parameters:   Dictionary with the following fields.
        
            respFile:   file containing respiratory time series
            
            cardFile:   file containing cardiac time series
            
            aby: whether  a and b coefficients as per Glover et al, Magnetic  
                  Resonance in Medicine 44:162–167 (2000)
                                        
            niml: whether output should be in niml format instead of CSV
            
            outputFileName:   Output filename
            
    AUTHOR
       Peter Lauren
    """
    
    physiologicalNoiseComponents = getPhysiologicalNoiseComponents(parameters)
    if len(physiologicalNoiseComponents) == 0:
        print('Error in runAnalysis. Failure to get physionlogical noise'+
              ' components')
        return 1
    if parameters['niml']:
        return 0
    
    physiologicalNoiseComponents.to_csv(parameters['outputFileName'])
    
    # PLot first 200 rows of dataframe
    colors = ['blue','cyan','blueviolet','cadetblue', 'olive','yellowgreen',
              'red','magenta']
    physiologicalNoiseComponents.head(200).plot(color=colors)
    
    # Send output to terminal
    if (parameters['abt']): print(repr(physiologicalNoiseComponents))
    
def getPhysJsonPair(phys_json_arg, phys_file, resp_info, card_info, 
                    resp_out, card_out, rvt_out):
    """
    NAME
        getPhysJsonPair 
            Returns the respiration file data, and cardiac file data.  Requires
            phys file and JSON file.
    TYPE
        <class 'str'>, <class 'numpy.ndarray'>, <class 'str'>, 
        <class 'numpy.ndarray'>
    ARGUMENTS
        resp_info:   Dictionary with the following fields.
        
            phys_fs: (dType = float) Physiological signal sampling 
                                     frequency (Hz)
            
        card_info:   Dictionary with the following fields.
            
            phys_fs:   (dType = float) Physiological signal sampling 
                                       frequency (Hz)
        
        phys_file: (dType = NoneType) BIDS formatted physio file in tab 
                    separated format. May be gzipped.
                
        phys_json_arg: (dType = NoneType) File metadata in JSON format
        
        resp_out: (dType = int) Whether to have respiratory output
        
        card_out:  (dType = int) Whether to have cardiac output
        
        rvt_out:  (dType = int) Whether to have RVT output
            
    AUTHOR
       Peter Lauren
    """
    
    phys_resp_dat = []
    phys_card_dat = []

    if not phys_file or not phys_json_arg:
        print("*** ERROR.  getPhysJsonPair called without phys file or JSON"\
              " file!")
        return phys_resp_dat, phys_card_dat

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
    if ('StartTime' in phys_meta and "StartTime" not in resp_info):
        startTime = float(phys_meta["StartTime"])
        if (startTime > 0):
            print('***** WARNING: JSON file gives positive start time'+
                  ' which is not currently handled')
            print('    Start time must be <= 0')
        else:
            resp_info["StartTime"] = startTime            
            card_info["StartTime"] = startTime            
                
    print('phys_meta = ', phys_meta)
    # Read columns field from JSON data
    print('Read columns field from JSON data')
    for k in phys_meta['Columns']:
        phys_dat[k] = np.array(phys_dat[k])
        
        # Read respiratory component
        if k.lower() == 'respiratory' or k.lower() == 'respiration':
            # create peaks only if asked for    25 May 2021 [rickr]
            if resp_out or rvt_out:
               if not resp_info["phys_fs"]:
                   resp_info['phys_fs'] = phys_meta['SamplingFrequency']
               phys_resp_dat = phys_dat[k]
        
        # Read cardiac component
        elif k.lower() == 'cardiac':
            # create peaks only if asked for    25 May 2021 [rickr]
            if card_out != 0:
               if not card_info["phys_fs"]:
                   card_info['phys_fs'] = phys_meta['SamplingFrequency']
               phys_card_dat = phys_dat[k]
        else:
            print("** warning phys data contains column '%s', but\n" \
                  "   retroicor only handles cardiac or respiratory data" % k)
    
    return phys_resp_dat, phys_card_dat
    

def getInputFileParameters(resp_info, card_info, phys_file,\
                        phys_json_arg, resp_out, card_out, rvt_out):
    """
    NAME
        getInputFileParameters 
            Returns the local respiriation file name  (if JSON file absent, None 
            otherwise), respiration file data (if JSON file present, None 
            otherwise), the local cardiac file name  (if JSON file absent, None 
            otherwise), cardiac file data (if JSON file present, None otherwise)
    TYPE
        <class 'str'>, <class 'numpy.ndarray'>, <class 'str'>, 
        <class 'numpy.ndarray'>
    ARGUMENTS
        resp_info:   Dictionary with the following fields.
        
            resp_file:  (dType = str) Name of ASCII file with respiratory time 
                                      series
            
            phys_fs: (dType = float) Physiological signal sampling 
                                     frequency (Hz)
            
        card_info:   Dictionary with the following fields.
            
            phys_fs:   (dType = float) Physiological signal sampling 
                                       frequency (Hz)
        
            card_file:  (dType = str) Name of ASCII file with cardiac time 
                                      series
            
        phys_file: (dType = NoneType) BIDS formatted physio file in tab 
                    separated format. May be gzipped.
                
        phys_json_arg: (dType = NoneType) File metadata in JSON format
        
        resp_out: (dType = int) Whether to have respiratory output
        
        card_out:  (dType = int) Whether to have cardiac output
        
        rvt_out:  (dType = int) Whether to have RVT output
            
    AUTHOR
       Peter Lauren
    """
    
    # Initialize outputs
    resp_file = None
    phys_resp_dat = []
    card_file = None
    phys_card_dat = []
            
    # Ensure there are no conflicting input file types
    # BIDS = Brain Imaging Data Structure
    if (((phys_file is not None) and (resp_info["resp_file"] is not None))
        or ((phys_file is not None) and (card_info["card_file"] is not None))):
        raise ValueError('You should not pass a BIDS style phsyio file'
                         ' and respiration or cardiac files.')
        
    # Get the peaks for resp_info and card_info
    # init dicts, may need -card_out 0, for example   [16 Nov 2021 rickr]
    if phys_file:   # JSON file
        phys_resp_dat, phys_card_dat = getPhysJsonPair(phys_json_arg, phys_file,
                            resp_info, card_info, resp_out, card_out, rvt_out)
    else:   # Not a JSON file
        if resp_info["resp_file"]:
            resp_file = resp_info["resp_file"]
        if card_info["card_file"]:
            card_file = card_info["card_file"]
            
    return resp_file, phys_resp_dat, card_file, phys_card_dat

from numpy import zeros, size

def ouputInNimlFormat(physiologicalNoiseComponents, parameters):
    """
    NAME
        ouputInNimlFormat 
            Output physiological noise components to NeuroImaging Markup 
            Language (NIML) format
    TYPE
        <class int>
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases in time 
                                                points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in time points
                                                (not seconds)
            
        parameters:   Dictionary with the following fields.
        
            num_slices:        (dtype = class 'int') Number of slices
            
            TR:       (dtype = class 'float') (volume_tr) Volume repetition 
                       time (TR) which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal sampling frequency 
                                        in Hz.
        
            rvt_out:   (dType = int) Whether to have RVT output
            
            slice_offset: Vector of slice acquisition time offsets in seconds.
                          (default is equivalent of alt+z)
                          
            prefix: (dType = str) Prefix for output filename.
                       
    AUTHOR
       Peter Lauren and Josh Zosky 
    """
    
    main_info = dict()
    main_info["rvt_out"] = parameters["rvt_out"]
    main_info["number_of_slices"] = parameters['num_slices']
    main_info["prefix"] = parameters["prefix"]
    main_info["resp_out"] = len(physiologicalNoiseComponents['resp_phases']) > 0
    main_info["card_out"] = len(physiologicalNoiseComponents['card_phases']) > 0
    
    if len(physiologicalNoiseComponents['resp_phases']) > 0:
        resp_info = makeRegressorsForEachSlice(physiologicalNoiseComponents, 
                        'r', parameters)
        if resp_info == None:
            print('ERROR getting respiratory regressors')
            return 1
        resp_info["rvt_shifts"] = list(range(0, 21, 5))
        resp_info["rvtrs_slc"] = np.zeros((len(resp_info["rvt_shifts"]), 
                        len(resp_info["time_series_time"])))
    if len(physiologicalNoiseComponents['card_phases']) > 0:
        card_info = makeRegressorsForEachSlice(physiologicalNoiseComponents, 
                        'c', parameters)
        if card_info == None:
            print('ERROR getting cardiac regressors')
            return 1
        card_info["rvt_shifts"] = list(range(0, 21, 5))
        card_info["rvtrs_slc"] = np.zeros((len(card_info["rvt_shifts"]), 
                        len(card_info["time_series_time"])))

    n_n   = 0
    n_r_v = 0
    n_r_p = 0
    n_e   = 0

    if len(physiologicalNoiseComponents['resp_phases']) > 0:
        if "time_series_time" in resp_info:
            n_n = len(resp_info["time_series_time"])
            n_r_p = size(resp_info["phase_slice_reg"], 1)
            n_r_v = size(resp_info["rvtrs_slc"], 0)
    if len(physiologicalNoiseComponents['card_phases']) > 0:
        if "time_series_time" in card_info:
            n_n = len(card_info["time_series_time"])
            n_r_p = size(card_info["phase_slice_reg"], 1)
            n_r_v = size(card_info["rvtrs_slc"], 0)

    if 'card_info' in locals() and "time_series_time" in card_info:  
              # must have card_info
        n_n = len(
            card_info["time_series_time"]
        )  # ok to overwrite len(resp_info.tst), should be same.
        n_e = size(card_info["phase_slice_reg"], 1)
    elif 'resp_info' in locals() and "time_series_time" in resp_info:  
                                                # must have card_info
        n_n = len(
            resp_info["time_series_time"]
        )  # ok to overwrite len(resp_info.tst), should be same.
        n_e = size(resp_info["phase_slice_reg"], 1)
    
    cnt = 0
    temp_y_axis = main_info["number_of_slices"] * (
        (main_info["rvt_out"]) * int(n_r_v)
        + (main_info["resp_out"]) * int(n_r_p)
        + (main_info["card_out"]) * int(n_e)
    )
    main_info["reml_out"] = zeros((n_n, temp_y_axis))

    # Check number of time points
    if size(main_info["reml_out"], 0) != parameters['num_time_pts']:
        print('***ERROR: Mismatch between ni_dimen' +
              ' (',size(main_info["reml_out"], 0), ') and user supplied ' +
              'num_time_pts (', parameters['num_time_pts'], ')')
    head = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = "'
        % (size(main_info["reml_out"], 1), size(main_info["reml_out"], 0))
    )
    tail = '"\n>'
    tailclose = "</RetroTSout>"

    label = head

    main_info["slice_major"] = 1
    main_info["reml_out"]    = []
    if main_info["slice_major"] == 0:  # old approach, not handy for 3dREMLfit
        # RVT
        if main_info["rvt_out"] != 0:
            for j in range(0, size(resp_info["rvtrs_slc"], 2)):
                for i in range(0, main_info["number_of_slices"]):
                    cnt += 1
                    main_info["reml_out"][:, cnt] = resp_info["rvtrs_slc"][
                        :, j
                    ]  # same for each slice
                    label = "%s s%d.RVT%d ;" % (label, i, j)
        # Resp
        if main_info["resp_out"] != 0:
            for j in range(0, size(resp_info["phase_slice_reg"], 2)):
                for i in range(0, main_info["number_of_slices"]):
                    cnt += 1
                    main_info["reml_out"][:, cnt] = resp_info["phase_slice_reg"][
                        :, j, i
                    ]
                    label = "%s s%d.Resp%d ;" % (label, i, j)
        # Card
        if main_info["Card_out"] != 0:
            for j in range(0, size(card_info["phase_slice_reg"], 2)):
                for i in range(0, main_info["number_of_slices"]):
                    cnt += 1
                    main_info["reml_out"][:, cnt] = card_info["phase_slice_reg"][
                        :, j, i
                    ]
                    label = "%s s%d.Card%d ;" % (label, i, j)
    else:
        # main_info["rvt_out"] = 0
        if main_info['rvt_out']: resp_info["rvtrs_slc"] =\
                                     physiologicalNoiseComponents['rvt_coeffs']
        for i in range(0, main_info["number_of_slices"]):
            if main_info["rvt_out"] != 0:
                # RVT
                for j in range(0, np.shape(resp_info["rvtrs_slc"])[0]):
                    cnt += 1
                    main_info["reml_out"].append(
                        resp_info["rvtrs_slc"][j]
                    )  # same regressor for each slice
                    label = "%s s%d.RVT%d ;" % (label, i, j)
            if main_info["resp_out"] != 0:
                # Resp
                for j in range(0, np.shape(resp_info["phase_slice_reg"])[1]):
                    cnt += 1
                    main_info["reml_out"].append(
                        resp_info["phase_slice_reg"][:, j, i]
                    )
                    label = "%s s%d.Resp%d ;" % (label, i, j)
            if main_info["card_out"] != 0:
                # Card
                for j in range(0, np.shape(card_info["phase_slice_reg"])[1]):
                    cnt += 1
                    main_info["reml_out"].append(
                        card_info["phase_slice_reg"][:, j, i]
                    )
                    label = "%s s%d.Card%d ;" % (label, i, j)
        # fid = open(("%s.slibase.1D" % main_info["prefix"]), "w")

    # remove very last ';'
    label = label[1:-2]

    np.savetxt(
        "./%s/%s.slibase.1D" % (parameters['OutDir'], main_info["prefix"]),
        np.column_stack(main_info["reml_out"]),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (label, tail)),
        footer=("%s" % tailclose),
    )
    
    return 0
    
def selectPhaseListAndNumTimeSteps(dataType, physiologicalNoiseComponents):
    """
    NAME
        selectPhaseListAndNumTimeSteps 
            Select the phase list, of the required type, from the physiological
            noise components determined per the Glover (2000) paper and 
            detected peaks and troughs.  Also returns the number of time steps
            which is the length of the phase list
    TYPE
        <class 'list'>, <class 'int'>
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases in time 
                                                points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in time points 
                                                (not seconds)
            
        dataType:     (dtype = class 'str') Type of data to be processed.  'c' 
                                            for cardiac.'r' for respiratory
                           (default is equivalent of alt+z)                      
    AUTHOR
       Peter Lauren 
    """
        
    if dataType == 'c':
        numTimeSteps = len(physiologicalNoiseComponents['card_phases'])
        if numTimeSteps == 0:
            print('*** Error in makeRegressorsForEachSlice')
            print('*** Cardiac phases required but none available')
            return None, None
        phaseList = physiologicalNoiseComponents['card_phases']
    else:
        numTimeSteps = len(physiologicalNoiseComponents['resp_phases'])
        if numTimeSteps == 0:
            print('*** Error in makeRegressorsForEachSlice')
            print('*** Respiratory phases required but none available')
            return None, None
        phaseList = physiologicalNoiseComponents['resp_phases']
     
    return phaseList, numTimeSteps

def setUpTimeSeries(phasee, parameters, numTimeSteps):
    """
    NAME
        setUpTimeSeries 
            Set up time series which would be the rows of the output SliBase file
    TYPE
        <class 'dict'>
    ARGUMENTS
        phasee:   Dictionary that has already been initialized and may already contain fields.
            
        parameters:   Dictionary with the following fields.
        
            num_slices:        (dtype = class 'int') Number of slices
            
            TR:       (dtype = class 'float') (volume_tr) Volume repetition 
                        time (TR) which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal sampling frequency 
                                       in Hz.
        
            slice_times: Vector of slice acquisition time offsets in seconds.
                          (default is equivalent of alt+z)
                       
    AUTHOR
       Peter Lauren 
    """

    #initialize output from input parameters
    phasee['slice_times'] = parameters['slice_times']
    timeStepIncrement = 1.0/parameters['phys_fs']
    
    phasee["t"] = np.zeros(numTimeSteps)
    for i in range(1,numTimeSteps): phasee["t"][i] = timeStepIncrement * i

    phasee["volume_tr"] = parameters['TR']
    phasee["time_series_time"] = np.arange(
        0, (max(phasee["t"]) - 0.5 * phasee["volume_tr"]), phasee["volume_tr"]
    )
    
    # Reduce number of output time points to user-specified value if required.
    if parameters['num_time_pts']:
        phasee["time_series_time"] = \
        phasee["time_series_time"][0:parameters['num_time_pts']]  
    
    if (max(phasee["t"]) - 0.5 * phasee["volume_tr"]) % phasee["volume_tr"] == 0:
        phasee["time_series_time"] = np.append(
            phasee["time_series_time"],
            [phasee["time_series_time"][-1] + phasee["volume_tr"]],
        )
        
    return phasee

def initializePhaseSlices(phasee, parameters):
    """
    NAME
        initializePhaseSlices 
            Initialize phase coefficient output matrix where the number or rows
            are the number pf time points and the number of columns four times
            the number of slices
    TYPE
        <class 'dict'>
    ARGUMENTS
        phasee:   Dictionary with the following fields.
        
            time_series_time: (dType = class 'list') List of float which are 
                              integral multiples of TR, starting at 0
                        
        parameters:   Dictionary with the following fields.
        
            num_slices:        (dtype = class 'int') Number of slices
    AUTHOR
       Peter Lauren 
    """

    phasee["number_of_slices"] = parameters['num_slices']
    phasee["phase_slice_reg"] = zeros(
        (len(phasee["time_series_time"]), 4, phasee["number_of_slices"])
    )

    phasee["phase_slice"] = zeros(
        (len(phasee["time_series_time"]), phasee["number_of_slices"])
    )
    
    return phasee

def fillSliceRegressorArray(phasee):
    """
    NAME
        fillSliceRegressorArray 
            Fill phase regression matrix with regressors for each lice as per 
            "Image-Based Method for 
            Retrospective Correction of Physiological Motion Effects in 
            fMRI: RETROICOR" by Gary H. Glover, Tie-Qiang Li, and David Ress 
            (2000).  
    TYPE
        <class 'dict'>
    ARGUMENTS
        phasee:   Partly filled ictionary with the following fields.
        
            number_of_slices:        (dtype = class 'int') Number of slices
            
            time_series_time: (dType = class 'list') List of float which are 
                              integral multiples of TR, starting at 0
                              
            slice_times: Vector of slice acquisition time offsets in seconds.
                          (default is equivalent of alt+z)
                          
            t: (dtype = <class 'numpy.ndarray'>) Progressive multiples of time step increment
            
            phase: (dtype = <class 'list'>)  List of phases for a given type (cardiac or
                    respiratory).  derived using Glover's algorithm after peaks and troughs have
                    been identified'
            
            phase_slice: (dtype = <class 'numpy.ndarray'>)  2D array with # columns = # output
                        time points and # columns  = # slices
                        
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>)  3D array with first dimension =
                                # output time points, the second dimension 4 and the third
                                dimension the number of slices
            
                       
    AUTHOR
       Peter Lauren 
    """
    # phasee["time_series_time"] are TR * index.  I.e. integral multiples of TR, starting at zero
    for i_slice in range(phasee["number_of_slices"]):
        # To the TR multiples, add the determined slice pattern time for the current index
        tslc = phasee["time_series_time"] + phasee["slice_times"][i_slice]
        
        
        for i in range(len(phasee["time_series_time"])): # For each multiple of TR (time point)
            imin = np.argmin(abs(tslc[i] - phasee["t"]))
            phasee["phase_slice"][i, i_slice] = phasee["phase"][imin]
            
        # Make four regressors for each slice.  First dimension is the time and the last is the
        # slice.  Regressors as defined in Glover paper.
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
    
    return phasee
    
    
def makeRegressorsForEachSlice(physiologicalNoiseComponents, 
                               dataType, parameters):
    """
    NAME
        makeRegressorsForEachSlice 
            Make regressors for each lice as per "Image-Based Method for 
            Retrospective Correction of Physiological Motion Effects in 
            fMRI: RETROICOR" by Gary H. Glover, Tie-Qiang Li, and David Ress 
            (2000).  Also make time vector
    TYPE
        <class 'dict'>
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases in time 
                                                points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in time points 
                                                (not seconds)
            
        dataType:     (dtype = class 'str') Type of data to be processed.  'c' 
                                            for cardiac.'r' for respiratory
            
        parameters:   Dictionary with the following fields.
        
            num_slices:        (dtype = class 'int') Number of slices
            
            TR:       (dtype = class 'float') (volume_tr) Volume repetition 
                        time (TR) which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal sampling frequency 
                                       in Hz.
        
            slice_times: Vector of slice acquisition time offsets in seconds.
                          (default is equivalent of alt+z)
                       
    AUTHOR
       Peter Lauren 
    """

    phasee = dict() # Initialize output
    
    # Select phase list and get number of time steps
    phasee["phase"], numTimeSteps = selectPhaseListAndNumTimeSteps(dataType, 
                                        physiologicalNoiseComponents)
    if numTimeSteps == None: return None
    
    # Set up time series
    phasee = setUpTimeSeries(phasee, parameters, numTimeSteps)
    
    phasee = initializePhaseSlices(phasee, parameters)
    
    phasee = fillSliceRegressorArray(phasee)
    
    return  phasee

def getRVT(rawData, resp_peaks, resp_troughs, freq, num_time_pts, 
           TR, show_graph = False, save_graph = True, 
           interpolationOrder = 'linear', font_size = 10):
    """
    NAME
        getRVT 
            Get Regression Volume Per Time (RVT) as described in ``Separating 
            respiratory-variation-related fluctuations from neuronal-activity-
            related fluctuations in fMRI'' by Rasmus M. Birn, Jason B. Diamond, 
            Monica A. Smith, and Peter A. Bandettini
    TYPE
        <class 'list'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        resp_peaks:    <class 'numpy.ndarray'> Peaks in input respiratory time 
                                               series.
        
        resp_troughs:    <class 'numpy.ndarray'> Troughs in input respiratory 
                                                 time series.
        
        freq:       <class 'float'> Time point sampling frequency in Hz
        
        num_time_pts: (dType = int) Number of time points in the output
        
        interpolationOrder:    <class 'str'> Method of interpolation among 
                                critical points (peaks, troughs, etc.)  Valid 
                                values are 'linear' (the default), 'quadratic' 
                                and 'cubic'.  The following are also valid but 
                                NOT recommended; ‘nearest’, ‘nearest-up’, 
                                ‘zero’, ‘slinear’, ‘previous’, or ‘next’. 
                                ‘zero’, ‘slinear’.
                       
    AUTHOR
       Peter Lauren 
    """
       
    # Get raw RVT values
    rawRVT = getRawRVT(rawData, resp_peaks, resp_troughs, freq,
       show_graph = False, save_graph = True, 
       interpolationOrder = interpolationOrder, font_size = font_size)
    
    # Get RVT regressors
    NUM_RVT = 5
    rvtRegressors = getRvtRegressors(rawRVT, NUM_RVT, freq, num_time_pts, TR,\
                                     interpolationOrder)
    
    return rvtRegressors

def getRvtRegressors(rawRVT, NUM_RVT, freq, num_time_pts, TR, 
                     interpolationOrder = 'linear'):
    """
    NAME
        getRvtRegressors 
            Get RVT regressors that can be used in the fMRI time series analysis
            and is an estimate of the respiration volume per time.  See 
            ``Separating respiratory-variation-related fluctuations from 
            neuronal-activity-related fluctuations in fMRI'' by Rasmus M. Birn, 
            Jason B. Diamond, Monica A. Smith, and Peter A. Bandettini, 
            NeuroImage 31 (2006) p. 1537
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        rawRVT:     <class 'numpy.ndarray'> RVT at each input time point
        
        NUM_RVT:    <class 'int'> Number of time points in the output.
        
        freq:       <class 'float'> Time point sampling frequency in Hz
        
        num_time_pts: (dType = int) Number of time points in the output
        
        TR:    <class 'float'> Volume TR.
        
        interpolationOrder:    <class 'str'> Method of interpolation among 
                                critical points (peaks, troughs, etc.)  Valid 
                                values are 'linear' (the default), 'quadratic' 
                                and 'cubic'.  The following are also valid but 
                                NOT recommended; ‘nearest’, ‘nearest-up’, 
                                ‘zero’, ‘slinear’, ‘previous’, or ‘next’. 
                                ‘zero’, ‘slinear’.
                       
    AUTHOR
       Peter Lauren  and Joshua Zosky
    """
       
    time = []    
    end = len(rawRVT)
    # for i in range(0,end): time.append(i/freq)
    for i in range(0,end): time.append(i/freq)
    
    NT_InterpPts = np.zeros(num_time_pts)
    # for i in range(0,num_time_pts): NT_InterpPts[i] = i*TR
    for i in range(0,num_time_pts): NT_InterpPts[i] = i*TR
    
    rvt_shifts = []
    for i in range(0,NUM_RVT): rvt_shifts.append(i * NUM_RVT)

    output = np.zeros((len(rvt_shifts), num_time_pts)) # 2D array of zeros
    for i in range(0, NUM_RVT):    # Process each row
        shf = rvt_shifts[i]                # i-th RVT 
        nsamp = int(round(shf * freq))  # i-th RVT times sample frequency
        sind = np.add(list(range(0, len(time))), nsamp) # array of integers from
                                     # nsampto nsamp times the number of samples
        sind[np.nonzero(sind < 0)] = 0          # Rectify result
        sind[np.nonzero(sind > (len(time) - 1))] = len(time) - 1 # Limit result 
                                                           # to length of input
        rvt_shf = scipy.interpolate._interpolate.interp1d(   # Build function 
                                                # that maps time to rawRVT[sind]
            time, rawRVT[sind], kind = interpolationOrder, bounds_error=True
        )
        rvt_shf_y = rvt_shf(NT_InterpPts) # Apply function to time
        
        # output[:][i] = rvt_shf_y
        output[:][i] = rvt_shf_y
        
    return output 
      
def getRawRVT(rawData, resp_peaks, resp_troughs, freq, show_graph = 0, 
              save_graph = 1, interpolationOrder = 'linear', font_size = 10):
    """
    NAME
        getRawRVT 
            Get raw Regression Volume Per Time (RVT) as described in 
            ``Separating respiratory-variation-related fluctuations from 
            neuronal-activity-related fluctuations in fMRI'' by Rasmus M. Birn, 
            Jason B. Diamond, Monica A. Smith, and Peter A. Bandettini.  That 
            is, get RVT for each input time point.
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        resp_peaks:  <class 'numpy.ndarray'> Peaks in input respiratory time 
                                                                        series.
        
        resp_troughs:  <class 'numpy.ndarray'> Troughs in input respiratory time 
                                                                         series.
        
        freq:  <class 'float'> Time point sampling frequency in Hz
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        interpolationOrder:    <class 'str'> Method of interpolation among 
                                critical points (peaks, troughs, etc.)  Valid 
                                values are 'linear' (the default), 'quadratic' 
                                and 'cubic'.  The following are also valid but 
                                NOT recommended; ‘nearest’, ‘nearest-up’, 
                                ‘zero’, ‘slinear’, ‘previous’, or ‘next’. 
                                ‘zero’, ‘slinear’.
                       
    AUTHOR
       Peter Lauren 
    """
       
    # Get peak layer
    peakLayer = getLayer(rawData, resp_peaks.tolist(), 
                         interpolationOrder = interpolationOrder)
    
    # Get trough layer
    troughLayer = getLayer(rawData, resp_troughs.tolist(), 
                           interpolationOrder = interpolationOrder)
    
    # Get period layer
    periodLayer = getPeriodLayer(resp_peaks, len(rawData), freq, 
                                 interpolationOrder = interpolationOrder)
    
    # Get raw RVT
    rawRVT = (peakLayer-troughLayer)/periodLayer
    
    # Display raw RVT as required
    if show_graph or save_graph:
         x = []    
         end = len(rawData)
         for i in range(0,end): x.append(i/freq)
         fig, ax_left = plt.subplots()
         plt.xlabel("Time (s)", fontdict={'fontsize': font_size})
         plt.ylabel('Input data and RVT value',color='black', 
                    fontdict={'fontsize': font_size})
         ax_left.plot(x, rawData, color='green')
         ax_left.plot(x, peakLayer, color='red')
         ax_left.plot(x, troughLayer, color='blue')
         ax_right = ax_left.twinx()
         ax_right.yaxis.set_major_formatter(FormatStrFormatter('%.2f'))
         ax_right.plot(x, periodLayer, color='magenta')
         plt.ylabel('Period (s)',color='magenta', fontweight='bold', 
                    fontdict={'fontsize': font_size})
         ax_left.plot(x, rawRVT, color='darkgoldenrod')
         TitleStr = "Raw RVT (dark goldenrod) and raw input data (green).\n"
         TitleStr = TitleStr + 'Red = peak layer. Blue = trough layer. '+\
                                                       'Magenta = period layer'
         plt.title(TitleStr, fontdict={'fontsize': font_size})
             
         # Save plot to file
         if save_graph:
             plt.savefig('%s/RawRVTVRawInput.pdf' % (OutDir)) 
             plt.show(block=False)
             if not show_graph: plt.close()  # Close graph after saving

    
    return rawRVT
    
def getPeriodLayer(resp_peaks, fullLength, freq, interpolationOrder = 'linear'):
    """
    NAME
        getPeriodLayer 
            Get a 1D layer estimating the period at each time point based on 
            interpolation of the measured period in the middle of each 
            respiratory or cardiac interval.
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        resp_peaks:  <class 'numpy.ndarray'> Peaks in input respiratory time 
                                                                        series.
        
        fullLength:         <class 'int'> Full length of array of point on which
                            respiratory peaks are based
        
        freq:  <class 'float'> Time point sampling frequency in Hz
        
        interpolationOrder:    <class 'str'> Method of interpolation among 
                                critical points (peaks, troughs, etc.)  Valid 
                                values are 'linear' (the default), 'quadratic' 
                                and 'cubic'.  The following are also valid but 
                                NOT recommended; ‘nearest’, ‘nearest-up’, 
                                ‘zero’, ‘slinear’, ‘previous’, or ‘next’. 
                                ‘zero’, ‘slinear’.
                       
    AUTHOR
       Peter Lauren 
    """
       
    # Get critical point locations
    criticalPoints = [round((i+j)/2) for i, j in zip(resp_peaks[:-1], 
                                                     resp_peaks[1:])]
    
    # Get critical point periods
    criticalPointPeriods = [(j-i)/freq for i, j in zip(resp_peaks[:-1], 
                                                       resp_peaks[1:])]
    
    # Output layer is found by interpoalting the periods among the critical points
    f = scipy.interpolate._interpolate.interp1d(criticalPoints, 
                                criticalPointPeriods, kind = interpolationOrder)    
    layer = f([x for x in range(criticalPoints[0],criticalPoints[-1])])
    
    # Apply first period to beginning
    insertion = [layer[0]] * resp_peaks[0]
    layer = np.insert(layer, 0, insertion)
    # criticalPoints.insert(0,0)
    # criticalPointPeriods.insert(0,criticalPointPeriods[0])

    # Apply last period to end
    appendage = (fullLength - len(layer)) * [layer[-1]]
    layer = np.append(layer, appendage)
    # criticalPoints.append(fullLength)
    # criticalPointPeriods.append(criticalPointPeriods[-1])
    
    return layer

def getLayer(rawData, criticalPoints, interpolationOrder = 'linear'):
    """
    NAME
        getLayer 
            Get a 1D layer made by interpolating among a set of ``critical 
            points'' (which may be peaks or troughs).
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        criticalPoints:  <class 'numpy.ndarray'> Peaks or troughs in input 
                                                 respiratory time series.
        
        interpolationOrder:    <class 'str'> Method of interpolation among 
                                critical points (peaks, troughs, etc.)  Valid 
                                values are 'linear' (the default), 'quadratic' 
                                and 'cubic'.  The following are also valid but 
                                NOT recommended; ‘nearest’, ‘nearest-up’, 
                                ‘zero’, ‘slinear’, ‘previous’, or ‘next’. 
                                ‘zero’, ‘slinear’.
                       
    AUTHOR
       Peter Lauren 
    """
       
    # Get critical point values
    criticalPointValues = [rawData[i] for i in criticalPoints]
    
    # Apply first period to beginning
    criticalPoints.insert(0,0)
    criticalPointValues.insert(0,criticalPointValues[0])

    # Apply last period to end
    fullLength = len(rawData)
    criticalPoints.append(fullLength)
    criticalPointValues.append(criticalPointValues[-1])
    
    # Output layer is found by interpoalting the periods among the critical pts
    f = scipy.interpolate._interpolate.interp1d(criticalPoints, 
                                criticalPointValues, kind = interpolationOrder)    
    layer = f([x for x in range(0,fullLength)])
    
    return layer
    
    
def getRawRVTBasedOnTroughs(rawData, resp_peaks, resp_troughs, freq, 
                            show_graph = 0, save_graph = 1):
    """
    NAME
        getRawRVTBasedOnTroughs 
            Get Regression Volume Per Time (RVT) as described in ``Separating 
            respiratory-variation-related fluctuations from neuronal-
            activity-related fluctuations in fMRI'' by Rasmus M. Birn, Jason B. 
            Diamond, Monica A. Smith, and Peter A. Bandettini.  This approach is 
            based on interpolating among RVT values, for each trough, based on 
            the difference between the mean value of the adjacent peaks and the 
            trough divided by the period around the trough.  (Not currently 
                                                              used.)
    TYPE
        <class 'list'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        resp_peaks:    <class 'numpy.ndarray'> Peaks in input respiratory time 
                                               series.
        
        resp_troughs:    <class 'numpy.ndarray'> Troughs in input respiratory 
                                                 time series.
        
        freq:       <class 'float'> Time point sampling frequency in Hz
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
                       
    AUTHOR
       Peter Lauren 
    """

    # Get trough RVTs
    troughRVTs = getTroughRVTs(rawData, resp_peaks, resp_troughs, freq)

    # Get raw RVTs by connecting the troughs
    rawRVTs = connectTroughRVTs(resp_troughs, troughRVTs, len(rawData))
    
    # Display raw RVTs with peaks, troughs and raw data
    if show_graph or save_graph:
         x = []    
         end = min(len(rawRVTs),round(len(rawRVTs)*50.0/len(resp_peaks)))
         for i in range(0,end): x.append(i/freq)
         fig, ax_left = plt.subplots()
         plt.xlabel("Time (s)")
         plt.ylabel('Input data input value',color='g')
         ax_right = ax_left.twinx()
         ax_right.plot(x, rawRVTs[0:end], color='darkorange', linewidth=3)
         plt.plot(resp_troughs/freq, troughRVTs, "o", color = 'darkgoldenrod') 
                                                                  # Trough RVTs
         ax_left.plot(x, rawData[0:end], color='green')
         peakVals = []
         for i in resp_peaks: peakVals.append(rawData[i])
         ax_left.plot(resp_peaks/freq, peakVals, "ro") # Peaks
         troughVals = []
         for i in resp_troughs: troughVals.append(rawData[i])
         ax_left.plot(resp_troughs/freq, troughVals, "bo") # Peaks
         plt.ylabel('Raw RVT',color='darkorange', fontweight='bold')
         plt.title("Raw RVT (orange) and raw input data (green)")
             
         # Save plot to file
         if save_graph:
             plt.savefig('%s/RawRVTVRawInput.pdf' % (OutDir)) 
             plt.show()
             if not show_graph: plt.close()  # Close graph after saving
   
    return rawRVTs
    
def connectTroughRVTs(resp_troughs, troughRVTs, fullLength):
    """
    NAME
        connectTroughRVTs 
            Connect Regression Volume Per Time (RVT) as described in 
            ``Separating respiratory-variation-related fluctuations from 
            neuronal-activity-related fluctuations in fMRI'' by Rasmus M. Birn, 
            Jason B. Diamond, Monica A. Smith,       and Peter A. Bandettini, 
            based on troughs.  This approach is based on interpolating among
            RVT values, for each trough, based on the difference between the 
            mean value of the adjacent peaks and the trough divided by the 
            period around the trough. (Not currently used.)
    TYPE
        <class 'list'>
    ARGUMENTS
        resp_troughs:    <class 'numpy.ndarray'> Troughs in input respiratory 
                                                 time series.
        
        troughRVTs:   <class 'list'> Array of RVTs estimated at each trough
        
        fullLength:         <class 'int'> Full length of array of point on which
                            respiratory peaks are based
                       
    AUTHOR
       Peter Lauren 
    """
    
    # Initialise output array
    connectedRVTs = []
    
    # Assign value of first trought to preceding elements
    for i in range(0,resp_troughs[0]):
        connectedRVTs.append(troughRVTs[0])
        
    #Assign values to regions between troughs
    numTroughs = len(resp_troughs)
    for j in range(1,numTroughs):
        i = j - 1
        increment = (troughRVTs[j] - troughRVTs[i])/(resp_troughs[j] -\
                                                     resp_troughs[i])
        start = resp_troughs[i]
        end = resp_troughs[j]
        for k in range(start, end): connectedRVTs.append(connectedRVTs[k-1] +\
                                                         increment)
    
    # Assign value of last trought to postceding elements
    for i in range(resp_troughs[-1],fullLength):
        connectedRVTs.append(troughRVTs[-1])
    
    return connectedRVTs
    
    
def getTroughRVTs(rawData, resp_peaks, resp_troughs, freq):
    """
    NAME
        getRawRVTBasedOnTroughs 
            Get Regression Volume Per Time (RVT) as described in ``Separating 
            respiratory-variation-related fluctuations from neuronal-activity-
            related fluctuations in fMRI'' by Rasmus M. Birn, Jason B. Diamond, 
            Monica A. Smith, and Peter A. Bandettini, for each trough.  This 
            approach is based on the difference between the mean value of the 
            adjacent peaks and the trough divided by the period around the 
            trough.  (Not currently used.)
    TYPE
       <class 'list'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        resp_peaks:    <class 'numpy.ndarray'> Peaks in input respiratory time 
                                               series.
        
        resp_troughs:    <class 'numpy.ndarray'> Troughs in input respiratory 
                                                 time series.
                
        freq:       <class 'float'> Time point sampling frequency in Hz
                       
    AUTHOR
       Peter Lauren 
    """
   
    # Assign RVT to first trough
    if resp_peaks[0] < resp_troughs[0]:
        meanPeak = (rawData[resp_peaks[0]] + rawData[resp_peaks[1]])/2
        troughRVTs = [(meanPeak - rawData[resp_troughs[0]])*freq/
                    (resp_peaks[1] - resp_peaks[0])]
        peakIndexLeft = 1
    else:
        # Assume first breath symmetric about trough
        troughRVTs = [((rawData[resp_peaks[0]] -\
                    rawData[resp_troughs[0]])*freq)/
                    (2*(resp_peaks[0] - resp_troughs[0]))]
        peakIndexLeft = 0

    peakIndexRight = peakIndexLeft + 1
    
    # Determine range of troughs with peak on either side
    if  resp_peaks[-1] > resp_troughs[-1]:
        end = len(resp_troughs)
    else: end = len(resp_peaks) - 1
        
    # Assign RVT values to middle troughs
    for i in range(1,end):
        meanPeak = (rawData[resp_peaks[peakIndexLeft]] +\
                rawData[resp_peaks[peakIndexRight]])/2
        troughRVTs.append(((meanPeak - rawData[resp_troughs[i]])*freq)/
                (resp_peaks[peakIndexRight] - resp_peaks[peakIndexLeft]))
        peakIndexLeft += 1
        peakIndexRight += 1
        
    # Process last trough if it comes after the last peak.  Assume last breath 
    # symmetric
    if  resp_peaks[-1] < resp_troughs[-1]:
        troughRVTs.append(((rawData[resp_peaks[-1]] -\
                    rawData[resp_troughs[-1]])*freq)/
                    (2*(resp_troughs[-1] - resp_peaks[-1])))
        
    return troughRVTs

def show_rvt_peak(physiologicalNoiseComponents, parameters):
    """
    NAME
        show_rvt_peak 
            Show RVT peaks and phase sampled at different acquisition times.
    TYPE
       <class 'int'>
    ARGUMENTS
        physiologicalNoiseComponents: <class 'dict'> Dictionary with the
                                                            following field:
        
            resp_phases: (dType = class 'list') Respiratory phases in time 
                                                points (not seconds)
                                                 time series.
                
        parameters:       <class 'float'> Time point sampling frequency in Hz
        
            num_slices:   (dtype = class 'int') Number of slices
            
            TR:       (dtype = class 'float') (volume_tr) Volume repetition 
                        time (TR) which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal sampling frequency 
                                       in Hz.
        
            slice_offset: Vector of slice acquisition time offsets in seconds.
                          (default is equivalent of alt+z)
                          
            save_graphs: (dtype = <class 'bool'>) Whether to save graph to file
                
            show_graphs: (dtype = <class 'bool'>) Whether to display graph 
                            while program is running
                       
    AUTHOR
       Peter Lauren 
    """
   
    
    resp_info = makeRegressorsForEachSlice(physiologicalNoiseComponents, 'r', 
                        parameters)
    if not resp_info:
        print('*** Error in show_rvt_peak')
        print('Failed to make regressors for each slice')
        return 1

    numTimeSteps = len(resp_info["phase_slice"])
    timeStepIncrement = parameters['TR']
    time1 = np.zeros(numTimeSteps)
    for i in range(1,numTimeSteps): time1[i] = timeStepIncrement * i
    numTimeSteps = len(physiologicalNoiseComponents['resp_phases'])
    timeStepIncrement = 1.0/parameters['phys_fs']
    time2 = np.zeros(numTimeSteps)
    for i in range(1,numTimeSteps): time2[i] = timeStepIncrement * i
       
    plt.plot(time2, physiologicalNoiseComponents["resp_phases"], "y")
    plt.plot(
        time1, resp_info["phase_slice"][:, 0], "ro"
    )  
    plt.plot(time1, resp_info["phase_slice"][:, 1], "bo")
    plt.plot(time1, resp_info["phase_slice"][:, 1], "b-")
    plt.grid("on", color = "gray")
    plt.xlabel("time (sec)", fontdict={'fontsize': parameters['font_size']})
    
    TitleStr = "Phase sampled at slice acquisition time\n"
    TitleStr = TitleStr +\
        'Original Phase (yellow), slice 0 (red) and slice 1 (blue)'
    plt.title(TitleStr, fontdict={'fontsize': parameters['font_size']})
            
    # Save plot to file
    if parameters['save_graphs']:
        plt.savefig('%s/RvtPeak.pdf' % (OutDir)) 
        plt.show(block=False)
        if parameters['show_graphs']: plt.show()
        else: plt.close()  # Close graph after saving
        
    return 0
    
