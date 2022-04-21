import sys
import numpy as np
import matplotlib as mp
from matplotlib import pyplot as plt 
import math
import scipy
from scipy.signal import find_peaks
import pandas as pd
from lib_RetroTS.RVT_from_PeakFinder import rvt_from_peakfinder
from lib_RetroTS.PhaseEstimator import phase_estimator
from lib_RetroTS.PeakFinder import peak_finder
from numpy import size, shape, column_stack, savetxt



# Glocal constants
M = 3
numSections = 1

class retroicorClass:

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
           
    # Check for, and fill in, missing peaks - MAKE OWN FUNCTION
   interpeak = [x - peaks[i - 1] for i, x in enumerate(peaks)][1:]
   minSep = min(interpeak)
   Threshold = minSep * 2
   numPeaks = len(peaks)
   for p in range(numPeaks-2,-1,-1):
       if interpeak[p] > Threshold:
           numberToAdd = int(round(interpeak[p]/minSep))
           sep = round(interpeak[p]/numberToAdd)
           if sep < minSep:
               numberToAdd = numberToAdd - 1
               sep = round(interpeak[p]/numberToAdd)               
           for i in range(1,numberToAdd):
               peaks = np.insert(peaks, p+i, peaks[p]+i*sep)
    
   return peaks, len(array)


def getRespiratoryPeaks(parameters):

   array = readArray(parameters, '-r')
    
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
    
   return peaks, troughs, len(array)

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
            

def determineRespiratoryPhases(parameters, respiratory_peaks, respiratory_troughs):
    data = readArray(parameters, '-r')
    
    # Debug: Make even number of peaks followed by troughs
    # data = [ -x for x in data ]
    
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
            
    # Append phases after last tunring point.
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

    # plt.plot(phases)
        
    return phases

def getNiml(data,columnNames,parameters,respiratory_phases,cardiac_phases):
    
    respiration_file = parameters['-r']
    # Estimate RVT
    respiration_info = dict()
    respiration_info["amp_phase"] = 1
    respiration_info["frequency_cutoff"] = 3
    respiration_peak, error = peak_finder(respiration_info, respiration_file)
    respiration_info.update(respiration_peak)
    respiration_phased = phase_estimator(respiration_info["amp_phase"], respiration_info)
    respiration_phased["legacy_transform"] = 0
    respiration_phased["rvt_shifts"] = range(0,len(columnNames))
    respiration_phased["rvtrs_slc"] = 0
    respiration_phased["time_series_time"] = np.abs(respiratory_phases)
    respiration_phased["interpolation_style"] = "linear"
    rvt = rvt_from_peakfinder(respiration_phased)
    rvt['number_of_slices'] = int(parameters['-s'])
    numSlices = rvt['number_of_slices']
    respiration_info.update(rvt)
    respiration_info.update(respiration_peak)
    respiration_info.update(respiration_phased)
    
    nx = 8
    ny = int((len(respiratory_phases)/(4*numSlices)))
    nz = numSlices
    
    respiration_info["phase_slice_reg"] = np.empty((nx,ny,nz))
    count = 0
    for k in range(nz):
        for j in range(ny):
            for i in range(nx):
                # respiration_info["phase_slice_reg"][i,j,k] = respiratory_phases[count]
                respiration_info["phase_slice_reg"][i,j,k] = 0
                count += 1

    head = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = '
        % ((shape(respiration_info["rvtrs_slc"])[0]+8)*numSlices, int(parameters['-Nt']))
    )
    
    label = head
    dCnt = 0
    respInc = 0
    cardInc = 0
    reml_out = []
    m = np.array(data)
    n = np.array(respiration_info["rvtrs_slc"])
    print('shape(respiration_info["rvtrs_slc"]) = ', n.shape)
    for i in range(0, numSlices):
        # RVT
        for j in range(0, shape(respiration_info["rvtrs_slc"])[0]):
            reml_out.append(
                respiration_info["rvtrs_slc"][j]
            )  # same regressor for each slice
            label = "%s s%d.RVT%d ;" % (label, i, j)
        # Resp
        for j in range(0, shape(respiration_info["rvtrs_slc"])[0]):
            for i in range(0,4):
                reml_out.append(
                    m[:,i]
                )  # same regressor for each slice
            label = "%s s%d.Resp%d ;" % (label, i, j)
        # Card
        for j in range(0, shape(respiration_info["rvtrs_slc"])[0]):
            for i in range(0,8):
                reml_out.append(
                    m[:,i]
                )  # same regressor for each slice
            label = "%s s%d.Card%d ;" % (label, i, j)
                
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

    return 0

def getPhysiologicalNoiseComponents(parameters):
    cardiac_peaks, fullLength = getCardiacPeaks(parameters) 
    
    cardiac_phases = determineCardiacPhases(cardiac_peaks, fullLength)
    
    respiratory_peaks, respiratory_troughs, fullLength = \
        getRespiratoryPeaks(parameters) 
    
    respiratory_phases = determineRespiratoryPhases(parameters, \
            respiratory_peaks, respiratory_troughs)
        
    if (parameters['-aby']):    # Determine a and b coefficients as per Glover et al, Magnetic 
                                # Resonance in Medicine 44:162–167 (2000)
        # Get a coefficients
        cardiacACoeffs = getACoeffs(parameters, '-c', cardiac_phases)
        respiratoryACoeffs = getACoeffs(parameters, '-r', respiratory_phases)
        
        # Get b coefficients
        cardiacBCoeffs = getBCoeffs(parameters, '-c', cardiac_phases)
        respiratoryBCoeffs = getBCoeffs(parameters, '-r', respiratory_phases)
    else:   # a and b coefficients set to 1.0
        cardiacACoeffs = [1.0]
        respiratoryACoeffs = [1.0]
        cardiacBCoeffs = [1.0]
        respiratoryBCoeffs = [1.0]
        cardiacACoeffs.append(1.0)
        respiratoryACoeffs.append(1.0)
        cardiacBCoeffs.append(1.0)
        respiratoryBCoeffs.append(1.0)
    
    global M
    global numSections
    
    # Initialize output table
    df = pd.DataFrame()
    
    # Make output table columns names
    columnNames = []
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
    for t in range(0,T):
        sum = 0
        addend = []
        for m in range(1,M):
            m0 = m - 1
            addend.append(respiratoryACoeffs[m0]*math.cos(m*respiratory_phases[t]))
            addend.append(respiratoryBCoeffs[m0]*math.sin(m*respiratory_phases[t]))
        for m in range(1,M):
            m0 = m - 1
            addend.append(cardiacACoeffs[m0]*math.cos(m*cardiac_phases[t]))
            addend.append(cardiacBCoeffs[m0]*math.sin(m*cardiac_phases[t]))
        data.append(addend)
    
    if (parameters['-niml']):
        niml = getNiml(data,columnNames,parameters, respiratory_phases, cardiac_phases)
        return data
    
    df = pd.DataFrame(data,columns=columnNames)
        
    return df    

def runAnalysis(parameters):
    # parameters = retroicorClass.getParameters()
    
    # parameters=dict()
    # parameters['-c'] = cardiacFile
    # parameters['-r'] = respiratoryFile
    # parameters['-s'] = nSlices
    # parameters['-abt'] = abt
    # parameters['-aby'] = aby
    # parameters['-niml'] = niml
    
    physiologicalNoiseComponents = getPhysiologicalNoiseComponents(parameters)
    if parameters['-niml']:
        return 0
    
    physiologicalNoiseComponents.to_csv(outputFile)
    
    # PLot first 200 rows of dataframe
    colors = ['blue','cyan','blueviolet','cadetblue', 'olive','yellowgreen','red','magenta']
    physiologicalNoiseComponents.head(200).plot(color=colors)
    
    # Send output to terminal
    if (abt): print(repr(physiologicalNoiseComponents))


