#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  7 09:11:12 2023

@author: peterlauren
"""

import numpy as np
    
def selectPhaseListAndNumTimeSteps(dataType, 
                                   physiologicalNoiseComponents):
    """
    NAME
        selectPhaseListAndNumTimeSteps 
            Select the phase list, of the required type, from the 
            physiological noise components determined per the 
            Glover (2000) paper and detected peaks and troughs.  
            Also returns the number of time steps which is the 
            length of the phase list
    TYPE
        <class 'list'>, <class 'int'>
    ARGUMENTS
        dataType:     (dtype = class 'str') Type of data to be 
                      processed.  'c' for cardiac.'r' for 
                      respiratory (default is equivalent of alt+z)                      

        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                         in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                         time points (not seconds)
            
    AUTHOR
       Peter Lauren 
    """
        
    if dataType == 'c':
        numTimeSteps = \
            len(physiologicalNoiseComponents['card_phases'])
        if numTimeSteps == 0:
            print('*** Error in selectPhaseListAndNumTimeSteps')
            print('*** Cardiac phases required but none available')
            return None, None
        phaseList = physiologicalNoiseComponents['card_phases']
    else:
        numTimeSteps = \
            len(physiologicalNoiseComponents['resp_phases'])
        if numTimeSteps == 0:
            print('*** Error in selectPhaseListAndNumTimeSteps')
            print('*** Respiratory phases required but ' + \
                  'none available')
            return None, None
        phaseList = physiologicalNoiseComponents['resp_phases']
     
    return phaseList, numTimeSteps

def setUpTimeSeries(phasee, physiologicalNoiseComponents, 
                    test_retro_obj, numTimeSteps):
    """
    NAME
        setUpTimeSeries 
            Set up time series which would be the rows of the 
            output SliBase file
    TYPE
        <class 'dict'> >with the  following fields.
            slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
             volume_tr: (dtype = class 'float') (volume_tr) Volume 
                         repetition time (TR) which defines the 
                         length of time 
                         
             time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                               from 0 to the rounded down maximum 't' value,
                               in increments of volume_tr.
                             
    ARGUMENTS
        phasee:   Dictionary that has already been initialized and 
                  may already contain fields.
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                  
        physiologicalNoiseComponents: Dictionary with the following fields.
                   
            resp_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        respiratory data is sampled, in Hertz
                        
            card_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        cardiac data is sampled, in Hertz
                        
            num_time_pts: (dType = int) Number of time points in the output
            
        test_retro_obj:   Object with the following fields.
        
            num_slices: (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
        numTimeSteps; (dtype = <class 'int'>) number of entries in the raw 
                      input data.
                       
    AUTHOR
       Peter Lauren 
    """

    #initialize output from input parameters
    phasee['slice_times'] = test_retro_obj.vol_slice_times
    if physiologicalNoiseComponents['resp_sample_frequency']:
        timeStepIncrement = 1.0/physiologicalNoiseComponents['resp_sample_frequency']
    else:
        timeStepIncrement = 1.0/physiologicalNoiseComponents['card_sample_frequency']
    
    phasee["t"] = np.zeros(numTimeSteps)
    for i in range(1,numTimeSteps): phasee["t"][i] = \
        timeStepIncrement * i

    phasee["volume_tr"] = test_retro_obj.vol_tr
    phasee["time_series_time"] = np.arange(
        0, (max(phasee["t"]) - 0.5 * phasee["volume_tr"]), 
            phasee["volume_tr"]
    )
    
    # Reduce number of output time points to user-specified value 
    # if required.
    if physiologicalNoiseComponents['num_time_pts']:
        phasee["time_series_time"] = \
        phasee["time_series_time"][0:physiologicalNoiseComponents['num_time_pts']]  
    
    if (max(phasee["t"]) - 0.5 * phasee["volume_tr"]) % \
        phasee["volume_tr"] == 0:
        phasee["time_series_time"] = np.append(
            phasee["time_series_time"],
            [phasee["time_series_time"][-1] + \
             phasee["volume_tr"]],
        )
        
    return phasee

def initializePhaseSlices(phasee, physiologicalNoiseComponents, test_retro_obj):
    """
    NAME
        initializePhaseSlices 
            Initialize phase coefficient output matrix where the 
            number or rows are the number pf time points and the 
            number of columns four times the number of slices
    TYPE
        <class 'dict'> with the  following fields.
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                   
            slice_times:  (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
            volume_tr: (dtype = class 'float') (volume_tr) Volume 
                         repetition time (TR) which defines the 
                         length of time 
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of volume_tr.
                              
            number_of_slices:(dtype = class 'int') Number of slices
            
            phase_slice_reg:  (dtype = <class 'numpy.ndarray'>) Registered 
                              phase slices
                              
            phase_slice:  (dtype = <class 'numpy.ndarray'>)  2D array with # 
                          columns = # output time points and 
                          # columns  = # slices         
                            
    ARGUMENTS
        phasee: <class 'dict'> with the  following fields.
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                   
            slice_times:  (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
            volume_tr: (dtype = class 'float') (volume_tr) Volume 
                         repetition time (TR) which defines the 
                         length of time 
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of volume_tr.
                        
        test_retro_obj:   Object with the following fields.
        
            n_slice_times:   (dtype = class 'int') Number of slices
    AUTHOR
       Peter Lauren 
    """

    phasee["number_of_slices"] = test_retro_obj.n_slice_times
    phasee["phase_slice_reg"] = np.zeros(
        (len(phasee["time_series_time"]), 4, 
             phasee["number_of_slices"])
    )

    phasee["phase_slice"] = np.zeros(
        (len(phasee["time_series_time"]), 
             phasee["number_of_slices"])
    )
    
    return phasee

def fillSliceRegressorArray(phasee):
    """
    NAME
        fillSliceRegressorArray 
            Fill phase regression matrix with regressors for each 
            slice as per "Image-Based Method for Retrospective 
            Correction of Physiological Motion Effects in fMRI: 
            RETROICOR" by Gary H. Glover, Tie-Qiang Li, and David 
            Ress (2000).  
    TYPE
        <class 'dict'> with the  following fields.
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                   
            slice_times:  (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
            volume_tr: (dtype = class 'float') (volume_tr) Volume 
                         repetition time (TR) which defines the 
                         length of time 
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of volume_tr.
                              
            number_of_slices:(dtype = class 'int') Number of slices
            
            phase_slice_reg:  (dtype = <class 'numpy.ndarray'>) Registered 
                              phase slices
                              
            phase_slice:  (dtype = <class 'numpy.ndarray'>)  2D array with # 
                          columns = # output time points and 
                          # columns  = # slices         
                            
    ARGUMENTS
        phasee:   Partly filled ictionary with the following fields.
        
            number_of_slices: (dtype = class 'int') Number 
            of slices
            
            time_series_time: (dType = class 'list') List of float 
                which are integral multiples of TR, starting at 0
                              
            slice_times: Vector of slice acquisition time offsets 
                in seconds. (default is equivalent of alt+z)
                          
            t: (dtype = <class 'numpy.ndarray'>) Progressive 
                multiples of time step increment
                
            volume_tr: (dtype = class 'float') (volume_tr) Volume repetition 
                       time (TR) which defines the length of time
            
            phase: (dtype = <class 'list'>)  List of phases for a 
                given type (cardiac or respiratory).  derived 
                using Glover's algorithm after peaks and troughs 
                have been identified'
            
            phase_slice: (dtype = <class 'numpy.ndarray'>)  2D 
                array with # columns = # output time points and # 
                columns  = # slices
                        
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>)  3D 
                array with first dimension = output time points, 
                the second dimension 4 and the third dimension the 
                number of slices           
                       
    AUTHOR
       Peter Lauren 
    """
    # phasee["time_series_time"] are TR * index.  I.e. integral 
    # multiples of TR, starting at zero
    for i_slice in range(phasee["number_of_slices"]):
        # To the TR multiples, add the determined slice pattern 
        # time for the current index
        tslc = phasee["time_series_time"] + \
            phasee["slice_times"][i_slice]
        
        # For each multiple of TR (time point)
        for i in range(len(phasee["time_series_time"])): 
            imin = np.argmin(abs(tslc[i] - phasee["t"]))
            phasee["phase_slice"][i, i_slice] = \
                phasee["phase"][imin]
            
        # Make four regressors for each slice.  First dimension is 
        # the time and the last is the
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
                               dataType, test_retro_obj):
    """
    NAME
        makeRegressorsForEachSlice 
            Make regressors for each lice as per "Image-Based 
            Method for Retrospective Correction of Physiological 
            Motion Effects in fMRI: RETROICOR" by Gary H. Glover, 
            Tie-Qiang Li, and David Ress (2000).  Also make time 
            vector.
    TYPE
        <class 'dict'> with the  following fields.
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                   
            slice_times:  (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
            volume_tr: (dtype = class 'float') (volume_tr) Volume 
                         repetition time (TR) which defines the 
                         length of time 
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of volume_tr.
                              
            number_of_slices:(dtype = class 'int') Number of slices
            
            phase_slice_reg:  (dtype = <class 'numpy.ndarray'>) Registered 
                              phase slices
                              
            phase_slice:  (dtype = <class 'numpy.ndarray'>)  2D array with # 
                          columns = # output time points and 
                          # columns  = # slices         
                            
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                         in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                         time points (not seconds)
                   
            resp_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        respiratory data is sampled, in Hertz
                        
            card_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        cardiac data is sampled, in Hertz
                        
            num_time_pts: (dType = int) Number of time points in the output
            
        dataType:     (dtype = class 'str') Type of data to be 
                      processed.  'c' for cardiac.'r' for 
                      respiratory
            
        test_retro_obj:   Object with the following fields.
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                       
    AUTHOR
       Peter Lauren 
    """

    phasee = dict() # Initialize output
    
    # Select phase list and get number of time steps
    phasee["phase"], numTimeSteps = \
        selectPhaseListAndNumTimeSteps(dataType, 
                   physiologicalNoiseComponents)
    if numTimeSteps == None: return None
    
    # Set up time series
    phasee = setUpTimeSeries(phasee, physiologicalNoiseComponents, 
                             test_retro_obj, numTimeSteps)
    
    phasee = initializePhaseSlices(phasee, physiologicalNoiseComponents, 
                                   test_retro_obj)
    
    phasee = fillSliceRegressorArray(phasee)
    
    return  phasee

def getPhysiologicalInfo(physiologicalNoiseComponents, test_retro_obj):
    """
    NAME
        getPhysiologicalInfo 
            Get cardiac and respirator slice, and time series, 
            info. to output to NIML file
    TYPE
        <class 'dict'>, <class 'dict'> 
        Two dictionaries, one for each data type, cardiac and respiratory, where
        ecah dictionary has the following fields.
        
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                   
            slice_times:  (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
            volume_tr: (dtype = class 'float') (volume_tr) Volume 
                         repetition time (TR) which defines the 
                         length of time 
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of volume_tr.
                              
            number_of_slices:(dtype = class 'int') Number of slices
            
            phase_slice_reg:  (dtype = <class 'numpy.ndarray'>) Registered 
                              phase slices
                              
            phase_slice:  (dtype = <class 'numpy.ndarray'>)  2D array with # 
                          columns = # output time points and 
                          # columns  = # slices 
                          
            rvt_shifts: (dtype = <class 'list'>) List of integers, from 0 to 20,
                        in increments of 5
                        
            rvtrs_slc:  (dtype = <class 'numpy.ndarray'>) RVT slices
                            
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                         in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                         time points (not seconds)
                   
            resp_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        respiratory data is sampled, in Hertz
                        
            card_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        cardiac data is sampled, in Hertz
                        
            num_time_pts: (dType = int) Number of time points in the output
            
        test_retro_obj:   Object with the following fields.
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                                              
    AUTHOR
       Peter Lauren  
    """
    
    # Initialize respiratory and cardiac dictionaries
    resp_info = dict()
    card_info = dict()
    
    if len(physiologicalNoiseComponents['resp_phases']) > 0:
        resp_info = \
          makeRegressorsForEachSlice(physiologicalNoiseComponents, 
          'r', test_retro_obj)
        if resp_info == None:
            print('ERROR getting respiratory regressors')
            return 1
        resp_info["rvt_shifts"] = list(range(0, 21, 5))
        resp_info["rvtrs_slc"] = \
            np.zeros((len(resp_info["rvt_shifts"]), 
                        len(resp_info["time_series_time"])))
    if len(physiologicalNoiseComponents['card_phases']) > 0:
        card_info = \
            makeRegressorsForEachSlice(\
                physiologicalNoiseComponents, 'c', test_retro_obj)
        if card_info == None:
            print('ERROR getting cardiac regressors')
            return 1
        card_info["rvt_shifts"] = list(range(0, 21, 5))
        card_info["rvtrs_slc"] = \
            np.zeros((len(card_info["rvt_shifts"]), 
            len(card_info["time_series_time"])))
    
    return resp_info, card_info

def getNimlDimensions(physiologicalNoiseComponents, resp_info, 
                      card_info):
    """
    NAME
        getNimlDimensions 
            Get number of RVT slices (nRvtSlices), phase slices (nPhaseSlices 
            and nPhaseSlicesFavoringCard), and time steps (nTimeSteps). 
    TYPE
        <class 'int'>, <class 'int'>, <class 'int'>, <class 'int'>
            nTimeSteps: number of time steps
                
            nRvtSlices: number of RVT slices
                
            nPhaseSlices: number of phase slices
                
            nPhaseSlicesFavoringCard: number of phase slices favoring cardio
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                                                in time points 
                                                (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                                                time points
                                                (not seconds)
       
        resp_info: (dtype = <class 'dict'>) Dictonary with the 
                   following fields for respiratory data
                    
            time_series_time: <class 'numpy.ndarray'>) List of 
                              float which are integral multiples 
                              of TR, starting at 0 
                    
            rvtrs_slc:  (dtype = <class 'numpy.ndarray'>) RVT slices
            
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>) 
                             Registered phase slices
        
        card_info: (dtype = <class 'dict'>)
                    
            time_series_time: (dType = <class 'numpy.ndarray'>) 
                              List of float which are integral 
                              multiples of TR, starting at 0 
                    
            rvtrs_slc:  (dtype = <class 'numpy.ndarray'>) RVT 
                        slices
            
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>)
                             Registered phase slices
            
    AUTHOR
       Peter Lauren
    """

    nTimeSteps   = 0
    nRvtSlices = 0
    nPhaseSlices = 0
    nPhaseSlicesFavoringCard   = 0

    if len(physiologicalNoiseComponents['resp_phases']) > 0:
        if "time_series_time" in resp_info:
            nTimeSteps = len(resp_info["time_series_time"])
            nPhaseSlices = np.size(resp_info["phase_slice_reg"], 1)
            nRvtSlices = np.size(resp_info["rvtrs_slc"], 0)
    if len(physiologicalNoiseComponents['card_phases']) > 0:
        if "time_series_time" in card_info:
            nTimeSteps = len(card_info["time_series_time"])
            nPhaseSlices = np.size(card_info["phase_slice_reg"], 1)
            nRvtSlices = np.size(card_info["rvtrs_slc"], 0)

    if 'card_info' in locals() and "time_series_time" in card_info:  
              # must have card_info
        nTimeSteps = len(
            card_info["time_series_time"]
        )  # ok to overwrite len(resp_info.tst), should be same.
        nPhaseSlicesFavoringCard = np.size(card_info["phase_slice_reg"], 1)
    elif 'resp_info' in locals() and \
        "time_series_time" in resp_info:  # must have resp_info
                                                
        nTimeSteps = len(
            resp_info["time_series_time"]
        )  # ok to overwrite len(resp_info.tst), should be same.
        nPhaseSlicesFavoringCard = np.size(resp_info["phase_slice_reg"], 1)
        
    return nTimeSteps, nRvtSlices, nPhaseSlices, nPhaseSlicesFavoringCard

def initializeMainInfoAndLabel(physiologicalNoiseComponents, test_retro_obj, 
                               nRvtSlices, nPhaseSlices, 
                               nPhaseSlicesFavoringCard, nTimeSteps):
    """
    NAME
        initializeMainInfoAndLabel 
            Initialize main info. and labels to output 
            physiological noise components in NeuroImaging Markup 
            Language (NIML) format
    TYPE
        <class 'dict'>, <class 'str'>
        The string is the header of the NIML file, not including the column
        labels.
        The dictionary has the following fields.

            number_of_slices: (dtype = class 'int') Number of slices
            
            prefix: (dtype = <class 'str'>)  Prefix for output file
            
            rvt_out: (dtype = <class 'int'>) Whether to calculate
                and output RVT. (0 = No, 1 = Yes, Defautl = 0)
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Defautl = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Defautl = True)
                
            slice_major: <class 'int'> Whether output is slice major
                         instead of registered phase slice major
                
    ARGUMENTS
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Defautl = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
            prefix: (dtype = <class 'str'>)  Prefix for output file
                                                                     
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                        time points (not seconds)
            
        nRvtSlices: (dtype = <class 'int'>) Number of RVT slices
        
        nPhaseSlices: (dtype = <class 'int'>) Number of phase slices
                                
        nPhaseSlicesFavoringCard: (dtype = <class 'int'>) Number of phase 
                                  slices favoring cardiac
        
        nTimeSteps: (dtype = <class 'int'>) Number of time steps
            
    AUTHOR
       Peter Lauren
    """

    main_info = dict()
    main_info["rvt_out"] = test_retro_obj.do_out_rvt
    main_info["number_of_slices"] = test_retro_obj.n_slice_times
    main_info["prefix"] = test_retro_obj.prefix
    main_info["resp_out"] = \
        len(physiologicalNoiseComponents['resp_phases']) > 0
    main_info["card_out"] = \
        len(physiologicalNoiseComponents['card_phases']) > 0
    
    # cnt = 0
    temp_y_axis = main_info["number_of_slices"] * (
        (main_info["rvt_out"]) * int(nRvtSlices)
        + (main_info["resp_out"]) * int(nPhaseSlices)
        + (main_info["card_out"]) * int(nPhaseSlicesFavoringCard)
    )
    main_info["reml_out"] = np.zeros((nTimeSteps, temp_y_axis))

    # Check number of time points
    if np.size(main_info["reml_out"], 
            0) != physiologicalNoiseComponents['num_time_pts']:
        print('***ERROR: Mismatch between ni_dimen' +
              ' (',np.size(main_info["reml_out"], 0), 
              ') and user supplied ' +
              'num_time_pts (', physiologicalNoiseComponents['num_time_pts'], 
                              ')')
        return None, None

    head = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = "'
        % (np.size(main_info["reml_out"], 1), 
           np.size(main_info["reml_out"], 0))
    )

    label = head

    main_info["slice_major"] = 1
    main_info["reml_out"]    = []
    
    return main_info, label

def getSliceMinorMainInfoAndLabel(main_info, label, resp_info, 
                                  card_info):
    """
    NAME
        getMainInfoAndLabel 
            Update initialized main info. and labels for
            slice-based physiological noise components in slice
            minor format.
    TYPE
        <class 'dict'>, <class 'str'>
        The string is the header of the NIML file, not including the column
        labels.
        The dictionary has the following fields.

            number_of_slices: (dtype = class 'int') Number of slices
            
            prefix: (dtype = <class 'str'>)  Prefix for output file
            
            rvt_out: (dtype = <class 'int'>) Whether to calculate
                and output RVT. (0 = No, 1 = Yes, Defautl = 0)
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Defautl = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Defautl = True)
                
            slice_major: <class 'int'> Whether output is slice major
                         instead of registered phase slice major
                
    ARGUMENTS
    
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Defautl = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
            prefix: (dtype = <class 'str'>)  Prefix for output file
                                                                     
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                        time points (not seconds)
            
        nRvtSlices: (dtype = <class 'int'>) Number of RVT slices
        
        nPhaseSlices: (dtype = <class 'int'>) Number of phase slices
                                
        nPhaseSlicesFavoringCard: (dtype = <class 'int'>) Number of phase 
                                  slices favoring cardiac
        
        nTimeSteps: (dtype = <class 'int'>) Number of time steps
            
    AUTHOR
       Peter Lauren
    """

    cnt = 0
    
    # RVT
    if main_info["rvt_out"] != 0:
        for j in range(0, np.size(resp_info["rvtrs_slc"], 2)):
            for i in range(0, main_info["number_of_slices"]):
                cnt += 1
                main_info["reml_out"][:, cnt] = \
                resp_info["rvtrs_slc"][
                    :, j
                ]  # same for each slice
                label = "%s s%d.RVT%d ;" % (label, i, j)
    # Resp
    if main_info["resp_out"] != 0:
        for j in range(0, np.size(resp_info["phase_slice_reg"], 2)):
            for i in range(0, main_info["number_of_slices"]):
                cnt += 1
                main_info["reml_out"][:, cnt] = \
                    resp_info["phase_slice_reg"][:, j, i
                ]
                label = "%s s%d.Resp%d ;" % (label, i, j)
    # Card
    if main_info["Card_out"] != 0:
        for j in range(0, np.size(card_info["phase_slice_reg"], 2)):
            for i in range(0, main_info["number_of_slices"]):
                cnt += 1
                main_info["reml_out"][:, cnt] = \
                card_info["phase_slice_reg"][:, j, i]
                label = "%s s%d.Card%d ;" % (label, i, j)
    
    return main_info, label

def getSliceMajorMainInfoAndLabel(main_info, label, resp_info, 
        card_info, physiologicalNoiseComponents):
    """
    NAME
        getSliceMajorMainInfoAndLabel 
            Update initialized main info. and labels for
            slice-based physiological noise components in slice
            major format.
    TYPE
        <class 'dict'>, <class 'str'>
        The string is the header of the NIML file including the column labels.
        The dictionary has the following fields.

            number_of_slices: (dtype = class 'int') Number of slices
            
            prefix: (dtype = <class 'str'>)  Prefix for output file
            
            rvt_out: (dtype = <class 'int'>) Whether to calculate
                and output RVT. (0 = No, 1 = Yes, Defautl = 0)
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Defautl = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Defautl = True)
                
            slice_major: (dtype = <class 'int'>) Whether output is slice major
                         instead of registered phase slice major
                         
            reml_out: (dtype = <class 'list'>)  2D list where the former 
                      dimension is the number of columns in the output and the
                      latter dimension is the number of rows.  The contents are
                      the output RVT, cardiac and/or respiratory calculations.
    ARGUMENTS
    
        main_info: (dtype = <class 'dict'>) Dictonary with the 
            following fields
            
            rvt_out: (dtype = <class 'int'>) Whether to calculate
                and output RVT. (0 = No, 1 = Yes, Defautl = 0)
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Defautl = True)
                
            Card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Defautl = True)
                
            number_of_slices:   (dtype = class 'int') Number of slices
                         
            reml_out: (dtype = <class 'list'>)  2D list where the former 
                      dimension is the number of columns in the output and the
                      latter dimension is the number of rows.  The contents are
                      the output RVT, cardiac and/or respiratory calculations.
        
        label: (dtype = <class 'str'>) Header, for the output
            NIML file, giving the names of the output slice
            components
        
        resp_info: (dtype = <class 'dict'>) Dictonary with the 
            following fields for respiratory data
                    
            rvtrs_slc: (dtype = <class 'numpy.ndarray'>) RVT slices
            
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>) 
                                        Registered phase slices
        
        card_info: (dtype = <class 'dict'>)
                    
            rvtrs_slc: (dtype = <class 'numpy.ndarray'>) RVT slices
            
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>) 
                Registered phase slices
                
        physiologicalNoiseComponents: (dtype = <class 'dict'>) with the 
                        following fields.
                        
            rvt_coeffs: (dtype = <class 'numpy.ndarray'>) RVT coefficients
            
    AUTHOR
       Peter Lauren
    """

    cnt = 0
    
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
            for j in range(0, 
                np.shape(resp_info["phase_slice_reg"])[1]):
                cnt += 1
                main_info["reml_out"].append(
                    resp_info["phase_slice_reg"][:, j, i]
                )
                label = "%s s%d.Resp%d ;" % (label, i, j)
        if main_info["card_out"] != 0:
            # Card
            for j in range(0, 
                np.shape(card_info["phase_slice_reg"])[1]):
                cnt += 1
                main_info["reml_out"].append(
                    card_info["phase_slice_reg"][:, j, i]
                )
                label = "%s s%d.Card%d ;" % (label, i, j)
                
    return main_info, label

def getMainInfoAndLabel(test_retro_obj, physiologicalNoiseComponents, 
                    nRvtSlices, nPhaseSlices, nPhaseSlicesFavoringCard, 
                    nTimeSteps, resp_info, card_info):
    """
    NAME
        getMainInfoAndLabel 
            Get main info. and labels to output physiological 
            noise components in NeuroImaging Markup Language 
            (NIML) format
    TYPE
        <class 'dict'>, <class 'str'>
        String is the full NIML header including the column labels.
        The dictionary has the following fields.

            number_of_slices: (dtype = class 'int') Number of slices
            
            prefix: (dtype = <class 'str'>)  Prefix for output file
            
            rvt_out: (dtype = <class 'int'>) Whether to calculate
                and output RVT. (0 = No, 1 = Yes, Defautl = 0)
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Defautl = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Defautl = True)
                
            slice_major: (dtype = <class 'int'>) Whether output is slice major
                         instead of registered phase slice major
                         
            reml_out: (dtype = <class 'list'>)  2D list where the former 
                      dimension is the number of columns in the output and the
                      latter dimension is the number of rows.  The contents are
                      the output RVT, cardiac and/or respiratory coefficients.
                      
    ARGUMENTS
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Defautl = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
            prefix: (dtype = <class 'str'>)  Prefix for output file

        physiologicalNoiseComponents:   Dictionary with the 
            following fields.
                        
            rvt_coeffs: (dtype = <class 'numpy.ndarray'>) RVT coefficients
        
            resp_phases: (dType = class 'list') Respiratory phases 
                in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                time points (not seconds)
            
        nRvtSlices: (dtype = <class 'int'>) Number of RVT slices
        
        nPhaseSlices: (dtype = <class 'int'>) Number of phase slices
                                
        nPhaseSlicesFavoringCard: (dtype = <class 'int'>) Number of phase 
                                  slices favoring cardiac.
        
        nTimeSteps: (dtype = <class 'int'>) Number of time steps
        
        resp_info: (dtype = <class 'dict'>) Dictonary with the 
            following fields for respiratory data
                    
            rvtrs_slc: (dtype = <class 'numpy.ndarray'>) RVT 
                       slices
            
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>) 
                Registered phase slices
        
        card_info: (dtype = <class 'dict'>)
                    
            rvtrs_slc: (dtype = <class 'numpy.ndarray'>) RVT 
                       slices
            
            phase_slice_reg: (dtype = <class 'numpy.ndarray'>) 
                Registered phase slices
            
    AUTHOR
       Peter Lauren
    """
    
    main_info, label = initializeMainInfoAndLabel( 
            physiologicalNoiseComponents, test_retro_obj, 
            nRvtSlices, nPhaseSlices, nPhaseSlicesFavoringCard, nTimeSteps)
    if not main_info:
        print('** ERROR: Failure to get main info. for output')
        return None, None

    if main_info["slice_major"] == 0:  # old approach, 
                                       # not handy for 3dREMLfit
        main_info, label = getSliceMinorMainInfoAndLabel(main_info, 
                label, resp_info, card_info)
    else:
        main_info, label = getSliceMajorMainInfoAndLabel(main_info, 
                            label, resp_info, card_info, 
                            physiologicalNoiseComponents)
    
    # remove very last ';'
    label = label[1:-2]
    
    return main_info, label

def ouputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
    """
    NAME
        ouputInNimlFormat 
            Output physiological noise components to NeuroImaging 
            Markup Language (NIML) format
    TYPE
        <class int> 0 on success.  1 on failure
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                         in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                         time points (not seconds)
                   
            resp_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        respiratory data is sampled, in Hertz
                        
            card_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        cardiac data is sampled, in Hertz
                        
            num_time_pts: (dType = int) Number of time points in the output
        
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Defautl = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
            prefix: (dtype = <class 'str'>)  Prefix for output file
            
            out_dir: (dtype = <class 'str'>) Durectory search path relative to
                     the working directory.
                       
    AUTHOR
       Peter Lauren  
    """
    
    resp_info, card_info = \
        getPhysiologicalInfo(physiologicalNoiseComponents,
                                                test_retro_obj)
    
    # Get number of slices and time points
    nTimeSteps, nRvtSlices, nPhaseSlices, nPhaseSlicesFavoringCard = \
        getNimlDimensions(physiologicalNoiseComponents,
                                  resp_info, card_info)
    
    # Get main info and label
    main_info, label = getMainInfoAndLabel(test_retro_obj, 
        physiologicalNoiseComponents, nRvtSlices, nPhaseSlices, 
        nPhaseSlicesFavoringCard, nTimeSteps, 
        resp_info, card_info)
    if not main_info:
        print('** ERROR getting main info and label for NIML File')
        return 1

    # Output file
    tail = '"\n>'
    tailclose = "</RetroTSout>"
    np.savetxt(
        "./%s/%s.slibase.1D" % (test_retro_obj.out_dir, 
        main_info["prefix"]),
        np.column_stack(main_info["reml_out"]),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (label, tail)),
        footer=("%s" % tailclose),
    )
    
    return 0
