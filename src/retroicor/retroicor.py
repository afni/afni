import sys
import numpy as np
import matplotlib as mp
from matplotlib import pyplot as plt 
from matplotlib import pylab
import math
import scipy
from scipy.signal import find_peaks
import pandas as pd
from lib_RetroTS.RVT_from_PeakFinder import rvt_from_peakfinder
from lib_RetroTS.PhaseEstimator import phase_estimator
from lib_RetroTS.PeakFinder import peak_finder
from numpy import size, shape, column_stack, savetxt
from array import array

# Glocal constants
M = 3
numSections = 1
NUM_RVT = 5

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
    
    global NUM_RVT
    
    respiration_file = parameters['-r']
    # Estimate RVT
    respiration_info = dict()
    respiration_info["amp_phase"] = 1
    respiration_info["frequency_cutoff"] = 3
    respiration_peak, error = peak_finder(respiration_info, respiration_file)
    respiration_info.update(respiration_peak)
    respiration_phased = phase_estimator(respiration_info["amp_phase"], respiration_info)
    respiration_phased["legacy_transform"] = 0
    respiration_phased["rvt_shifts"] = range(0,NUM_RVT)
    print('respiration_phased["rvt_shifts"] = ', respiration_phased["rvt_shifts"])
    respiration_phased["rvtrs_slc"] = 0
    respiration_phased["time_series_time"] = np.abs(respiratory_phases)
    respiration_phased["interpolation_style"] = "linear"
    rvt = rvt_from_peakfinder(respiration_phased)
    rvt['number_of_slices'] = int(parameters['-s'])
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
    
    print('shape(reml_out) = ', shape(reml_out))

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
        
    # Make data matrix
    data = []
    T = len(respiratory_phases)
    print('T = ', T)
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
        
    return data

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
    print('T = ', T)
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

def phase_estimator(amp_phase, phase_info):
    print('phase_estimator in retroicor')
    """
    v_name='',
    amp_phase=0,
    t=[],
    x=[],
    iz=[],   # zero crossing (peak) locations
    p_trace=[],
    tp_trace=[],
    n_trace=[],
    tn_trace=[],
    prd=[],
    t_mid_prd=[],
    p_trace_mid_prd=[],
    phase=[],
    rv=[],
    rvt=[],
    var_vector=[],
    phys_fs=(1 / 0.025),
    zero_phase_offset=0.5,
    quiet=0,
    resample_fs=(1 / 0.025),
    f_cutoff=10,
    fir_order=80,
    resample_kernel='linear',
    demo=0,
    as_window_width=0,
    as_percover=0,
    as_fftwin=0,
    sep_dups=0,
    phasee_list=0,
    show_graphs=0
    """
    """
    Example: PhaseEstimator.phase_estimator(amp_phase, info_dictionary)
    or PhaseEstimator.phase_estimator(v) where v is a list
    if v is a matrix, each column is processed separately.
    :param var_vector: column vector--list of list(s)
    :param phys_fs: Sampling frequency
    :param zero_phase_offset: Fraction of the period that corresponds to a phase of 0
                                0.5 means the middle of the period, 0 means the 1st peak
    :param quiet:
    :param resample_fs:
    :param frequency_cutoff:
    :param fir_order: BC ???
    :param resample_kernel:
    :param demo:
    :param as_window_width:
    :param as_percover:
    :param fftwin:
    :param sep_dups:
    :return: *_phased: phase estimation of input signal
    """
    phasee = dict(
        v_name="",
        t=[],
        x=[],
        iz=[],  # zero crossing (peak) locations
        volume_tr=2,
        p_trace=[],
        tp_trace=[],
        n_trace=[],
        tn_trace=[],
        prd=[],
        t_mid_prd=[],
        p_trace_mid_prd=[],
        phase=[],
        rv=[],
        rvt=[],
        var_vector=[],
        phys_fs=(1 / 0.025),
        zero_phase_offset=0.5,
        quiet=0,
        resample_fs=(1 / 0.025),
        frequency_cutoff=10,
        fir_order=80,
        resample_kernel="linear",
        demo=0,
        as_window_width=0,
        as_percover=0,
        as_fftwin=0,
        sep_dups=0,
        phasee_list=0,
        show_graphs=0,
        number_of_slices=0,
    )
    phasee.update(phase_info)
    if isinstance(phasee["phasee_list"], type([])):
        return_phase = []
        return_phase_list = []
        for phasee_column in phasee["phasee_list"]:
            return_phase.append(phase_base(amp_phase, phasee_column))
        print('shape(return_phase_list) = ', shape(return_phase_list))
        return return_phase_list
    else:
        print('amp_phase = ', amp_phase)
        return_phase, rvt = phase_base(amp_phase, phasee)
        print('1: rvt = ', rvt)
        return return_phase, rvt

def my_hist(x, bin_centers):
    """
    This frivolous yet convenient conversion from bin-edges to bin-centers is from Stack Overflow user Bas Swinckels
    http://stackoverflow.com/questions/18065951/why-does-numpy-histogram-python-leave-off-one-element-as-compared-to-hist-in-m
    :param x:dataset
    :param bin_centers:bin values in a list to be moved from edges to centers
    :return: counts = the data in bin centers ready for pyplot.bar
    """
    bin_edges = np.r_[-np.Inf, 0.5 * (bin_centers[:-1] + bin_centers[1:]), np.Inf]
    counts, edges = np.histogram(x, bin_edges)
    return counts

def z_scale(x, lower_bound, upper_bound, perc=[]):
    # ZSCALE (X,UB,LB)
    #
    # This function scales  X into Y such that
    #   its maximum value is UB
    #   and minimum value is LB
    # If perc is specified, then clipping is done
    # at the percentile range specified (e.g. [2, 98])
    # before scaling.
    # If X is all constants, it gets scaled to UB;
    #
    #           Ziad, Oct 30 96 / modified March 18 97

    print('type(x) = ', type(x))
    if type(x) != type(array):
        if type(x) != type(np.ndarray):
            x = x.reshape(-1)
        else:
            x = array(x)
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


    :type phasee: object
    :param amp_type:    if 0, it is a time-based phase estimation
                        if 1, it is an amplitude-based phase estimation
    :param phasee: phasee information
    :return:
    """
    if amp_type == 0:
        # Calculate the phase of the trace, with the peak to be the start of the phase
        nptrc = len(phasee["tp_trace"])
        phasee["phase"] = -2 * np.ones(size(phasee["t"]))
        i = 0
        j = 0
        while i <= (nptrc - 2):
            while phasee["t"][j] < phasee["tp_trace"][i + 1]:
                if phasee["t"][j] >= phasee["tp_trace"][i]:
                    # Note: Using a constant 244 period for each interval
                    # causes slope discontinuity within a period.
                    # One should resample period[i] so that it is
                    # estimated at each time in phasee['t'][j],
                    # dunno if that makes much of a difference in the end however.
                    if j == 10975:
                        pass
                    phasee["phase"][j] = (
                        phasee["t"][j] - phasee["tp_trace"][i]
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
    else:  # phase based on amplitude
        # at first scale to the max
        mxamp = max(phasee["p_trace"])
        phasee["phase_pol"] = []
        gR = z_scale(phasee["v"], 0, mxamp)  # Scale, per Glover 2000's paper
        bins = np.arange(0.01, 1.01, 0.01) * mxamp
        hb_value = my_hist(gR, bins)
        # hb_value = histogram(gR, bins)
        if phasee["show_graphs"] == 1:
            center = (bins[:-1] + bins[1:]) / 2
            plt.bar(center, hb_value[: len(hb_value) - 1])  # , align='center')
            plt.show()
        # find the polarity of each time point in v
        i = 0
        itp = 0
        inp = 0
        tp = phasee["tp_trace"][0]
        tn = phasee["tn_trace"][0]
        while (
            (i <= len(phasee["v"])) and (phasee["t"][i] < tp) and (phasee["t"][i] < tn)
        ):
            phasee["phase_pol"].append(0)
            i += 1
        if tp < tn:
            # Expiring phase (peak behind us)
            cpol = -1
            itp = 1
        else:
            # Inspiring phase (bottom behind us)
            cpol = 1
            inp = 1
        phasee["phase_pol"] = np.zeros(
            size(phasee["v"])
        )  # Not sure why you would replace the
        # list that you created 10 lines prior to this
        # Add a fake point to tptrace and tntrace to avoid ugly if statements
        # phasee["tp_trace"].append(phasee["t"][-1])
        # phasee["tn_trace"].append(phasee["t"][-1])
        phasee["tp_trace"] = np.append(phasee["tp_trace"], phasee["t"][-1])
        phasee["tn_trace"] = np.append(phasee["tn_trace"], phasee["t"][-1])
        while i < len(phasee["v"]):
            phasee["phase_pol"][i] = cpol
            if phasee["t"][i] == phasee["tp_trace"][itp]:
                cpol = -1
                itp = min((itp + 1), (len(phasee["tp_trace"]) - 1))
            elif phasee["t"][i] == phasee["tn_trace"][inp]:
                cpol = 1
                inp = min((inp + 1), (len(phasee["tn_trace"]) - 1))
            # cpol, inp, itp, i, R
            i += 1
        phasee["tp_trace"] = np.delete(phasee["tp_trace"], -1)
        phasee["tn_trace"] = np.delete(phasee["tn_trace"], -1)
        if phasee["show_graphs"] == 1:
            # clf
            plt.plot(phasee["t"], gR, "b")
            ipositive = np.nonzero(phasee["phase_pol"] > 0)
            ipositive = ipositive[0]
            ipositive_x = []
            for i in ipositive:
                ipositive_x.append(phasee["t"][i])
            ipositive_y = np.zeros(size(ipositive_x))
            ipositive_y.fill(0.55 * mxamp)
            plt.plot(ipositive_x, ipositive_y, "r.")
            inegative = np.nonzero(phasee["phase_pol"] < 0)
            inegative = inegative[0]
            inegative_x = []
            for i in inegative:
                inegative_x.append(phasee["t"][i])
            inegative_y = np.zeros(size(inegative_x))
            inegative_y.fill(0.45 * mxamp)
            plt.plot(inegative_x, inegative_y, "g.")
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
        for i in range(1, 100):
            hbsum.append(hbsum[i - 1] + (float(hb_value[i]) / shb))
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
        # phasee["time_series_time"] = append(
        #     phasee["time_series_time"],
        #     [phasee["time_series_time"][-1] + phasee["volume_tr"]],
        # )
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
            # mi = abs(tslc[i] - phasee['t']) # probably not needed
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

    rvt = rvt_from_peakfinder(phasee)
    print('rvt = ', rvt)
    return phasee["phase_slice_reg"], rvt

def rvt_from_peakfinder(r):
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
            dm = min(len(r["p_trace"]), len(r["n_trace"]))
            if len(r["p_trace"]) != dm:
                r["p_trace"] = r["p_trace"][0:dm]
                r["tp_trace"] = r["tp_trace"][0:dm]
            else:
                r["n_trace"] = r["n_trace"][0:dm]
                r["tn_trace"] = r["tn_trace"][0:dm]

    r["rv"] = np.subtract(r["p_trace"], r["n_trace"])
    # NEED TO consider which starts first and
    # whether to initialize first two values by means
    # and also, what to do when we are left with one
    # incomplete pair at the end

    nptrc = len(r["tp_trace"])
    r["rvt"] = r["rv"][0 : nptrc - 1] / r["prd"]
    if r["p_trace_r"].any:
        r["rvr"] = np.subtract(r["p_trace_r"], r["n_trace_r"])
        # Debugging lines below
        # with open('rvr.csv', 'w') as f:
        #     for i in r['rvr']:
        #         f.write("%s\n" % i)
        # with open('prdR.csv', 'w') as f:
        #     for i in r['prdR']:
        #         f.write("%s\n" % i)
        r["rvtr"] = np.ndarray(np.shape(r["rvr"]))
        np.divide(r["rvr"], r["prdR"], r["rvtr"])
        # Smooth RVT so that we can resample it at volume_tr later
        fnyq = r["phys_fs"] / 2  # nyquist of physio signal
        fcut = 2 / r["volume_tr"]  # cut below nyquist for volume_tr
        w = float(r["frequency_cutoff"]) / float(fnyq)  # cut off frequency normalized
        b = scipy.signal.firwin(numtaps=(r["fir_order"] + 1), cutoff=w, window="hamming")
        v = r["rvtr"]
        np.around(v, 6, v)
        # Debugging lines below
        # with open('a.csv', 'w') as f:
        #     for i in v:
        #         f.write("%s\n" % i)
        mv = np.mean(v)
        # remove the mean
        v = v - mv
        # filter both ways to cancel phase shift
        v = scipy.signal.lfilter(b, 1, v)
        if r["legacy_transform"] == 0:
            v = np.flipud(
                v
            )  # Turns out these don't do anything in the MATLAB version(Might be a major problem)
        v = scipy.signal.lfilter(b, 1, v)
        if r["legacy_transform"] == 0:
            v = np.flipud(
                v
            )  # Turns out these don't do anything in the MATLAB version(Might be a major problem)
        r["rvtrs"] = v + mv

    # create RVT regressors
    r["rvtrs_slc"] = np.zeros((len(r["rvt_shifts"]), len(r["time_series_time"])))
    for i in range(0, len(r["rvt_shifts"])):
        shf = r["rvt_shifts"][i]
        nsamp = int(round(shf * r["phys_fs"]))
        sind = np.add(list(range(0, len(r["t"]))), nsamp)
        print(sind)
        sind[np.nonzero(sind < 0)] = 0
        sind[np.nonzero(sind > (len(r["t"]) - 1))] = len(r["t"]) - 1
        rvt_shf = scipy.interpolate.interp1d(
            r["t"], r["rvtrs"][sind], r["interpolation_style"], bounds_error=True
        )
        rvt_shf_y = rvt_shf(r["time_series_time"])
        if r["quiet"] == 0 and r["show_graphs"] == 1:
           # pacify matplotlib by passing a label (to get new instance)
           pylab.subplot(111, label='plot #%d'%i)
           pylab.plot(r["time_series_time"], rvt_shf_y)
        r["rvtrs_slc"][:][i] = rvt_shf_y

    if r["quiet"] == 0 and r["show_graphs"] == 1:
        print("--> Calculated RVT \n--> Created RVT regressors")
        pylab.subplot(211)
        pylab.plot(
            r["t_mid_prd"], z_scale(r["rvt"], min(r["p_trace"]), max(r["p_trace"])), "k"
        )
        if any(r["p_trace_r"]):
            pylab.plot(
                r["tR"], z_scale(r["rvtrs"], min(r["p_trace"]), max(r["p_trace"])), "m"
            )
        pylab.show()
        if r["demo"]:
            # uiwait(msgbox('Press button to resume', 'Pausing', 'modal'))
            pass

    print('r = ', r)
    # print('r[3] = ', r[3])
    print('r["rvtrs_slc"] = ', r["rvtrs"])
    return r['rvtrs_slc']


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
    
    physiologicalNoiseComponents.to_csv(parameters['-o'])
    
    # PLot first 200 rows of dataframe
    colors = ['blue','cyan','blueviolet','cadetblue', 'olive','yellowgreen','red','magenta']
    physiologicalNoiseComponents.head(200).plot(color=colors)
    
    # Send output to terminal
    if (parameters['-abt']): print(repr(physiologicalNoiseComponents))


