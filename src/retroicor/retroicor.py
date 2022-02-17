import sys
import numpy as np
import matplotlib as mp
from matplotlib import pyplot as plt 
import math
import scipy
from scipy.signal import find_peaks

# Glocal constatns
M = 3

def getParameters():
    # Get parameters from inline arguments
    parameters=dict()
    num_parameters = len(sys.argv)
    i=1
    while i<num_parameters:
        parameters[sys.argv[i]] = sys.argv[i+1]
        i += 2
        
    return parameters

def readArray(parameters, key):
    # Read single column of numbers into list of lists
    with open(parameters[key]) as f:
        array = []
        for line in f: # read rest of lines
            array.append([int(x) for x in line.split()])
            
    # Transform list of lists into single list
    array = [n for one_dim in array for n in one_dim]
    return array

def getCardiacPeaks(parameters):

   array = readArray(parameters, '-c')

   peaks, _ = find_peaks(np.array(array))
   
   # Peaks must be at least the threshold value
   # Threshold is currently half the maximum
   Max = max(array)
   Threshold = Max/2
   numPeaks = len(peaks)
   for p in range(numPeaks-1,-1,-1):
       if array[peaks[p]] < Threshold:
           peaks = np.delete(peaks,p) 
           
    # Cheack for, and fill in, missing peaks
   interpeak = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
   minSep = min(interpeak)
   Threshold = minSep * 2
   numPeaks = len(peaks)
   for p in range(numPeaks-2,-1,-1):
       if interpeak[p] > Threshold:
           numberToAdd = round(interpeak[p]/minSep)
           sep = round(interpeak[p]/numberToAdd)
           if sep < minSep:
               numberToAdd = numberToAdd - 1
               sep = round(interpeak[p]/numberToAdd)               
           for i in range(1,numberToAdd):
               peaks = np.insert(peaks, p+i, peaks[p]+i*sep)
    
   return peaks, len(array)


def getPeaks(parameters, key):

   array = readArray(parameters, key)

   peaks, _ = find_peaks(np.array(array))
    
   return peaks, len(array)

def getTroughs(parameters, key):

   array = readArray(parameters, key)

   troughs, _ = find_peaks(-np.array(array))
    
   return troughs

def determineCardiacPhases(peaks, fullLength):
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
            
    return phases

def getACoeffs(parameters, key, phases):
    data = readArray(parameters, key)
    mean = np.mean(data)
    N = len(data)
    global M
    a = []
    for m in range(1,M):
        num = 0
        denom = 0
        for n in range(0,N):
            num = num + (data[n]-mean)*math.cos(m*phases[n])
            temp = math.cos(m*phases[n])
            denom = denom + (temp*temp)
        a.append(num/denom)
        
    return a

def getBCoeffs(parameters, key, phases):
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
            

def determineRespiratoryPhases(parameters):
    data = readArray(parameters, '-r')
    denom = len(data)
    offset = 0 - min(data)
    for i in range(0,denom):
        data[i] = data[i] + offset

    # Determine sign array
    respiratory_peaks, _ = find_peaks(np.array(data))
    respiratory_troughs, _ = find_peaks(-np.array(data))
    signs = []
    nPeaks = len(respiratory_peaks)
    nTroughs = len(respiratory_troughs)
    if respiratory_peaks[0]<respiratory_troughs[0]:
        for i in range(0,respiratory_peaks[0]): signs[0].append(1)
        for peak in range(0,min(nPeaks,nTroughs)):
            for i in range(respiratory_peaks[peak],respiratory_troughs[peak]):
                signs.append(-1)
            end = respiratory_peaks[peak+1]
            for i in range(respiratory_troughs[peak], end):
                signs.append(1)
        if nPeaks<nTroughs:
            for i in range(end,respiratory_troughs[-1]):
                signs.append(-1)
    else: # First critical point is a trough
        for i in range(0,respiratory_troughs[0]): signs.append(-1)
        last = min(nPeaks,nTroughs)
        for peak in range(0,last):
            for i in range(respiratory_troughs[peak],respiratory_peaks[peak]):
                signs.append(1)
            if peak+1 >= nTroughs: break
            end = respiratory_troughs[peak+1]
            for i in range(respiratory_peaks[peak], end):
                signs.append(-1)
        if nPeaks==nTroughs:
            for i in range(end,denom):
                signs.append(-1)
        else: # nTroughs > nPeaks
            for i in range(end,denom):
                signs.append(1)
        
                

    Rmax = max(data)
    Histogram = np.linspace(math.ceil(min(data)),
        math.floor(max(data)), 100)
    phases = []
    inc = 0
    for phase in range(0,denom):
        num = 0
        limit = round(100.0*data[phase]/Rmax)
        # print('limit = ', limit)
        for b in range(0,limit):
            num += Histogram[b]
        phases.append(math.pi*num*signs[inc]/denom)
        inc = inc + 1
                
    plt.plot(data)
        
    return phases

def getPhysiologicalNoiseComponents(parameters):
    cardiac_peaks, fullLength = getCardiacPeaks(parameters) 
    
    cardiac_phases = determineCardiacPhases(cardiac_peaks, fullLength)
    
    respiratory_peaks, fullLength = getPeaks(parameters, '-r') 
    
    respiratory_phases = determineRespiratoryPhases(parameters)

    # Get a coefficients
    cardiacACoeffs = getACoeffs(parameters, '-c', cardiac_phases)
    respiratoryACoeffs = getACoeffs(parameters, '-r', respiratory_phases)
    
    # Get b coefficients
    cardiacBCoeffs = getBCoeffs(parameters, '-c', cardiac_phases)
    respiratoryBCoeffs = getBCoeffs(parameters, '-r', respiratory_phases)
    
    global M
    
    output = []
    T = len(respiratory_phases)
    for t in range(0,T):
        sum = 0
        for m in range(1,M):
            m0 = m - 1
            sum = sum + cardiacACoeffs[m0]*math.cos(m*cardiac_phases[t]) + \
                cardiacBCoeffs[m0]*math.sin(m*cardiac_phases[t]) + \
                   respiratoryACoeffs[m0]*math.cos(m*respiratory_phases[t]) + \
                       respiratoryBCoeffs[m0]*math.sin(m*respiratory_phases[t])
        output.append(sum)
        
    return output    

parameters = getParameters()

physiologicalNoiseComponents = getPhysiologicalNoiseComponents(parameters)

# cardiac_peaks = getPeaks(parameters, '-c') 

# cardiac_phases = determineCardiacPhases(cardiac_peaks)

# respiratory_peaks = getPeaks(parameters, '-r') 

# respiratory_phases = determineRespiratoryPhases(parameters)
                
# plt.plot(respiratory_phases[0:3000])

# # Get a coefficients
# cardiacACoeffs = getACoeffs(parameters, '-c', cardiac_phases)
# respiratoryACoeffs = getACoeffs(parameters, '-r', respiratory_phases)

# # Get b coefficients
# cardiacBCoeffs = getBCoeffs(parameters, '-c', cardiac_phases)
# respiratoryBCoeffs = getBCoeffs(parameters, '-r', respiratory_phases)


