#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  7 09:11:12 2023

@author: peterlauren
"""

import numpy as np
    
def selectPhaseListAndNumTimeSteps(dataType, physiologicalNoiseComponents):
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
    # TODO: Call once for each data type
    """
    NAME
        setUpTimeSeries 
            Set up time series which would be the rows of the 
            output SliBase file
    TYPE
        <class 'dict'> >with the  following fields.

            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
                         
             time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                               from 0 to the rounded down maximum 't' value,
                               in increments of vol_tr.
                             
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
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
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
    # TODO: Determine separately for each data type
    if physiologicalNoiseComponents['resp_sample_frequency']:
        timeStepIncrement = 1.0/physiologicalNoiseComponents['resp_sample_frequency']
    else:
        timeStepIncrement = 1.0/physiologicalNoiseComponents['card_sample_frequency']
    
    phasee["t"] = np.zeros(numTimeSteps)
    for i in range(1,numTimeSteps): phasee["t"][i] = \
        timeStepIncrement * i

    test_retro_obj.vol_tr = test_retro_obj.vol_tr
    phasee["time_series_time"] = np.arange(
        0, (max(phasee["t"]) - 0.5 * test_retro_obj.vol_tr), 
            test_retro_obj.vol_tr
    )
    
    # Reduce number of output time points to user-specified value 
    # if required.
    if physiologicalNoiseComponents['num_time_pts']:
        phasee["time_series_time"] = \
        phasee["time_series_time"][0:physiologicalNoiseComponents['num_time_pts']]  
    
    if (max(phasee["t"]) - 0.5 * test_retro_obj.vol_tr) % \
        test_retro_obj.vol_tr == 0:
        phasee["time_series_time"] = np.append(
            phasee["time_series_time"],
            [phasee["time_series_time"][-1] + \
             test_retro_obj.vol_tr],
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
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of vol_tr.
            
            phase_slice_reg:  (dtype = <class 'numpy.ndarray'>) Registered 
                              phase slices
                              
            phase_slice:  (dtype = <class 'numpy.ndarray'>)  2D array with # 
                          columns = # output time points and 
                          # columns  = # slices         
                            
    ARGUMENTS
        phasee: <class 'dict'> with the  following fields.
            phase: (dtype = <class 'numpy.ndarray'>) Phase array for the data
                   type (cardiac of respiratory) being considered.
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of vol_tr.
                        
        test_retro_obj:   Object with the following fields.
        
            n_slice_times:   (dtype = class 'int') Number of slices
    AUTHOR
       Peter Lauren 
    """

    # Initialize registered phase slices which will be fed into output file data
    phasee["phase_slice_reg"] = np.zeros(
        (len(phasee["time_series_time"]), 4, 
             test_retro_obj.n_slice_times)
    )

    phasee["phase_slice"] = np.zeros(
        (len(phasee["time_series_time"]), 
             test_retro_obj.n_slice_times)
    )
    
    return phasee

def fillSliceRegressorArray(phasee, test_retro_obj):
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
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
            
            phase_slice_reg:  (dtype = <class 'numpy.ndarray'>) Registered 
                              phase slices
                              
            phase_slice:  (dtype = <class 'numpy.ndarray'>)  2D array with # 
                          columns = # output time points and 
                          # columns  = # slices         
                            
    ARGUMENTS
        phasee:   Partly filled ictionary with the following fields.
            
            time_series_time: (dType = class 'list') List of float 
                which are integral multiples of TR, starting at 0
                          
            t: (dtype = <class 'numpy.ndarray'>) Progressive 
                multiples of time step increment
            
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
    for i_slice in range(test_retro_obj.n_slice_times):
        # To the TR multiples, add the determined slice pattern 
        # time for the current index
        tslc = phasee["time_series_time"] + test_retro_obj.vol_slice_times[i_slice]
        
        # For each multiple of TR (time point)
        for i in range(len(phasee["time_series_time"])): 
            # Get index of minimu absolute difference between the input time
            # series times (s) and the composite time determined above
            imin = np.argmin(abs(tslc[i] - phasee["t"]))
            # Phase, for this time and slice, is the pahse, for the data type, 
            # at that index
            phasee["phase_slice"][i, i_slice] = phasee["phase"][imin]
            
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
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
               
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of vol_tr.
            
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
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
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
    
    phasee = fillSliceRegressorArray(phasee, test_retro_obj)
    
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
                         
            t: (dtype = <class 'numpy.ndarray'>) Array of progressively 
               increasing time step increments where the increment size is the
               multiplicative inverse of the sampling frequency for the given
               data type (cardiac or respiratory)
                         
            time_series_time: (dtype = <class 'numpy.ndarray'>) float values,
                              from 0 to the rounded down maximum 't' value,
                              in increments of vol_tr.
            
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
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
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
            Get number of RVT slices (nRvtSlices), respiratory phase slices 
            (nRespiratoryPhaseSlices and nCardiacPhaseSlices), and time steps 
            (nTimeSteps). 
    TYPE
        <class 'int'>, <class 'int'>, <class 'int'>, <class 'int'>
            nTimeSteps: number of time steps
                
            nRvtSlices: number of RVT slices
                
            nRespiratoryPhaseSlices: number of respiratory phase slices
                
            nCardiacPhaseSlices: number of cardiac phase slices
    ARGUMENTS
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                                                in time points 
                                                (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                                                time points
                                                (not seconds)
       
        resp_info: (dtype = <class 'dict'>) Dictionary with the 
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
    nRespiratoryPhaseSlices = 0
    nCardiacPhaseSlices   = 0

    if len(physiologicalNoiseComponents['resp_phases']) > 0:
        if "time_series_time" in resp_info:
            nTimeSteps = len(resp_info["time_series_time"])
            nRespiratoryPhaseSlices = np.size(resp_info["phase_slice_reg"], 1)
            nRvtSlices = np.size(resp_info["rvtrs_slc"], 0)
    if len(physiologicalNoiseComponents['card_phases']) > 0:
        if "time_series_time" in card_info:
            nTimeSteps = len(card_info["time_series_time"])
            nRespiratoryPhaseSlices = np.size(card_info["phase_slice_reg"], 1)
            nRvtSlices = np.size(card_info["rvtrs_slc"], 0)

    if 'card_info' in locals() and "time_series_time" in card_info:  
              # must have card_info
        nTimeSteps = len(
            card_info["time_series_time"]
        )  # ok to overwrite len(resp_info.tst), should be same.
        nCardiacPhaseSlices = np.size(card_info["phase_slice_reg"], 1)
    elif 'resp_info' in locals() and \
        "time_series_time" in resp_info:  # must have resp_info
                                                
        nTimeSteps = len(
            resp_info["time_series_time"]
        )  # ok to overwrite len(resp_info.tst), should be same.
        nCardiacPhaseSlices = np.size(resp_info["phase_slice_reg"], 1)
        
    return nTimeSteps, nRvtSlices, nRespiratoryPhaseSlices, nCardiacPhaseSlices

def initializeMainInfoAndLabel(physiologicalNoiseComponents, test_retro_obj, 
                               nRvtSlices, nRespiratoryPhaseSlices, 
                               nCardiacPhaseSlices, nTimeSteps):
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
            
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Default = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Default = True)
                
            slice_major: <class 'int'> Whether output is slice major
                         instead of registered phase slice major
                
    ARGUMENTS
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Default = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                        time points (not seconds)
            
        nRvtSlices: (dtype = <class 'int'>) Number of RVT slices
        
        nRespiratoryPhaseSlices: (dtype = <class 'int'>) Number of 
                                 respiratory phase slices
                                
        nCardiacPhaseSlices: (dtype = <class 'int'>) Number of cardiac phase 
                                  slices
        
        nTimeSteps: (dtype = <class 'int'>) Number of time steps
            
    AUTHOR
       Peter Lauren
    """

    main_info = dict()
    main_info["resp_out"] = \
        len(physiologicalNoiseComponents['resp_phases']) > 0
    main_info["card_out"] = \
        len(physiologicalNoiseComponents['card_phases']) > 0
    
    # y-axis is the number of columns in the output file
    temp_y_axis = test_retro_obj.n_slice_times * ( # Num. output slices
        (test_retro_obj.do_out_rvt) * int(nRvtSlices) # Num. RVT entries per slices
        + (main_info["resp_out"]) * int(nRespiratoryPhaseSlices)
                                                # Num. resp entries per slices
        + (main_info["card_out"]) * int(nCardiacPhaseSlices)
                                                # Num. card entries per slices
    )
    
    # Initialize 2D output data array.  Former dimension is the number of rows
    # while the latter dimension is the number of columns
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

    # Make output file header without the column labels
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
                                  card_info, test_retro_obj):
    """
    NAME
        getSliceMinorMainInfoAndLabel 
            Update initialized main info. and labels for
            slice-based physiological noise components in slice
            minor format.
    TYPE
        <class 'dict'>, <class 'str'>
        The string is the header of the NIML file, not including the column
        labels.
        The dictionary has the following fields.
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Default = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Default = True)
                
            slice_major: <class 'int'> Whether output is slice major
                         instead of registered phase slice major
                
    ARGUMENTS
    
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Default = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
        physiologicalNoiseComponents:   Dictionary with the 
                                        following fields.
        
            resp_phases: (dType = class 'list') Respiratory phases 
                in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                        time points (not seconds)
            
        nRvtSlices: (dtype = <class 'int'>) Number of RVT slices
        
        nRespiratoryPhaseSlices: (dtype = <class 'int'>) Number of respiratory 
                                 phase slices
                                
        nCardiacPhaseSlices: (dtype = <class 'int'>) Number of cardiac phase 
                                  slices 
        
        nTimeSteps: (dtype = <class 'int'>) Number of time steps
            
    AUTHOR
       Peter Lauren
    """

    cnt = 0
    
    # RVT
    if test_retro_obj.do_out_rvt != 0:
        for j in range(0, np.size(resp_info["rvtrs_slc"], 2)):
            for i in range(0, test_retro_obj.n_slice_times):
                cnt += 1
                main_info["reml_out"][:, cnt] = \
                resp_info["rvtrs_slc"][
                    :, j
                ]  # same for each slice
                label = "%s s%d.RVT%d ;" % (label, i, j)
    # Resp
    if main_info["resp_out"] != 0:
        for j in range(0, np.size(resp_info["phase_slice_reg"], 2)):
            for i in range(0, test_retro_obj.n_slice_times):
                cnt += 1
                main_info["reml_out"][:, cnt] = \
                    resp_info["phase_slice_reg"][:, j, i
                ]
                label = "%s s%d.Resp%d ;" % (label, i, j)
    # Card
    if main_info["Card_out"] != 0:
        for j in range(0, np.size(card_info["phase_slice_reg"], 2)):
            for i in range(0, test_retro_obj.n_slice_times):
                cnt += 1
                main_info["reml_out"][:, cnt] = \
                card_info["phase_slice_reg"][:, j, i]
                label = "%s s%d.Card%d ;" % (label, i, j)
    
    return main_info, label

def getSliceMajorMainInfoAndLabel(main_info, label, resp_info, 
        card_info, physiologicalNoiseComponents, test_retro_obj):
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
            
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Default = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Default = True)
                
            slice_major: (dtype = <class 'int'>) Whether output is slice major
                         instead of registered phase slice major
                         
            reml_out: (dtype = <class 'list'>)  2D list where the former 
                      dimension is the number of columns in the output and the
                      latter dimension is the number of rows.  The contents are
                      the output RVT, cardiac and/or respiratory calculations.
    ARGUMENTS
    
        main_info: (dtype = <class 'dict'>) Dictionary with the 
            following fields
                
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Default = True)
                
            Card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Default = True)
                         
            reml_out: (dtype = <class 'list'>)  2D list where the former 
                      dimension is the number of columns in the output and the
                      latter dimension is the number of rows.  The contents are
                      the output RVT, cardiac and/or respiratory calculations.
        
        label: (dtype = <class 'str'>) Header, for the output
            NIML file, giving the names of the output slice
            components
        
        resp_info: (dtype = <class 'dict'>) Dictionary with the 
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
    
    # Get RVT coefficients if required.
    if test_retro_obj.do_out_rvt: resp_info["rvtrs_slc"] =\
                    physiologicalNoiseComponents['rvt_coeffs']

    # Fill output array (test_retro_obj.do_out_rvt).  There are about four
    # coefficients within each slice
    cnt = 0
    # Process each column of output
    for i in range(0, test_retro_obj.n_slice_times):
        # Coefficients within each slice
        if test_retro_obj.do_out_rvt != 0:
            # RVT: 2D array. Former dimension is number of coefficients per 
            # slice. Same coefficients for every slice
            for j in range(0, np.shape(resp_info["rvtrs_slc"])[0]):
                cnt += 1
                main_info["reml_out"].append(
                    resp_info["rvtrs_slc"][j] # Single column of output
                )  # same regressor for each slice
                label = "%s s%d.RVT%d ;" % (label, i, j) # Append column label
        if main_info["resp_out"] != 0:
            # Resp. 3D array.  First index is the number or output rows.  2nd
            # is the number of coeffcieints per slice.  3rd is the number of 
            # slices
            for j in range(0, # Each coefficient for given slice
                np.shape(resp_info["phase_slice_reg"])[1]):
                cnt += 1
                main_info["reml_out"].append(
                    resp_info["phase_slice_reg"][:, j, i]
                ) # Append output column
                label = "%s s%d.Resp%d ;" % (label, i, j) # Append column label
        if main_info["card_out"] != 0:
            # Card. 3D array.  First index is the number or output rows.  2nd
            # is the number of coeffcieints per slice.  3rd is the number of 
            # slices
            for j in range(0, # Each coefficient for given slice 
                np.shape(card_info["phase_slice_reg"])[1]):
                cnt += 1
                main_info["reml_out"].append(
                    card_info["phase_slice_reg"][:, j, i]
                ) # Append output column
                label = "%s s%d.Card%d ;" % (label, i, j) # Append column label
                
    return main_info, label

def getMainInfoAndLabel(test_retro_obj, physiologicalNoiseComponents, 
                    nRvtSlices, nRespiratoryPhaseSlices, nCardiacPhaseSlices, 
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
            
            resp_out: (dtype = <class 'bool'>) Whether to calculate
                and output respiratory noise. (Default = True)
                
            card_out: (dtype = <class 'bool'>) Whether to calculate
                and output cardiac noise. (Default = True)
                
            slice_major: (dtype = <class 'int'>) Whether output is slice major
                         instead of registered phase slice major
                         
            reml_out: (dtype = <class 'list'>)  2D list where the former 
                      dimension is the number of columns in the output and the
                      latter dimension is the number of rows.  The contents are
                      the output RVT, cardiac and/or respiratory coefficients.
                      
    ARGUMENTS
        test_retro_obj:   Object with the following fields.
        
            do_out_rvt:  (dtype = <class 'int'>) Whether to calculate
                         and output RVT. (0 = No, 1 = Yes, Default = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
        physiologicalNoiseComponents:   Dictionary with the 
            following fields.
                        
            rvt_coeffs: (dtype = <class 'numpy.ndarray'>) RVT coefficients
        
            resp_phases: (dType = class 'list') Respiratory phases 
                in time points (not seconds)
            
            card_phases: (dType = class 'list') Cardiac phases in 
                time points (not seconds)
            
        nRvtSlices: (dtype = <class 'int'>) Number of RVT slices
        
        nRespiratoryPhaseSlices: (dtype = <class 'int'>) Number of 
                                 respiratory phase slices
                                
        nCardiacPhaseSlices: (dtype = <class 'int'>) Number of cardiac phase 
                                  slices.
        
        nTimeSteps: (dtype = <class 'int'>) Number of time steps
        
        resp_info: (dtype = <class 'dict'>) Dictionary with the 
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
            nRvtSlices, nRespiratoryPhaseSlices, 
            nCardiacPhaseSlices, nTimeSteps)
    if not main_info:
        print('** ERROR: Failure to get main info. for output')
        return None, None

    if main_info["slice_major"] == 0:  # old approach, 
                                       # not handy for 3dREMLfit
        main_info, label = getSliceMinorMainInfoAndLabel(main_info, 
                label, resp_info, card_info, test_retro_obj)
    else:
        main_info, label = getSliceMajorMainInfoAndLabel(main_info, 
                            label, resp_info, card_info, 
                            physiologicalNoiseComponents, test_retro_obj)
    
    # remove very last ';'
    label = label[1:-2]
    
    return main_info, label

def outputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
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
                         and output RVT. (0 = No, 1 = Yes, Default = 0)
        
            num_slices: (dtype = class 'int') Number of slices
        
            n_slice_times:   (dtype = class 'int') Number of slices
            
            vol_tr:     (dtype = class 'float') Volume repetition time (TR) 
                        which defines the length of time 
            
            phys_fs:   (dType = float) Physiological signal 
                       sampling frequency in Hz.
        
            slice_times: (dtype = <class 'list'>)  Vector of slice acquisition 
                         time offsets in seconds. (default is equivalent of 
                                                   alt+z)
                         
            vol_slice_times: (dtype = <class 'list'>) list of floats for slice 
                             timing
                             
            prefix: (dtype = <class 'str'>)  Prefix for output file
            
            out_dir: (dtype = <class 'str'>) Directory search path relative to
                     the working directory.
                       
    AUTHOR
       Peter Lauren  
    """
    
    resp_info, card_info = \
        getPhysiologicalInfo(physiologicalNoiseComponents, test_retro_obj)
    
    # Get number of slices and time points
    nTimeSteps, nRvtSlices, nRespiratoryPhaseSlices, nCardiacPhaseSlices = \
        getNimlDimensions(physiologicalNoiseComponents, resp_info, card_info)
    
    # Get main info and label
    main_info, label = getMainInfoAndLabel(test_retro_obj, 
        physiologicalNoiseComponents, nRvtSlices, nRespiratoryPhaseSlices, 
        nCardiacPhaseSlices, nTimeSteps, 
        resp_info, card_info)
    if not main_info:
        print('** ERROR getting main info and label for NIML File')
        return 1

    # Output file
    tail = '"\n>'
    tailclose = "</RetroTSout>"
    np.savetxt(
        "./%s/%s.slibase.1D" % (test_retro_obj.out_dir, 
        test_retro_obj.prefix),
        np.column_stack(main_info["reml_out"]),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (label, tail)),
        footer=("%s" % tailclose),
    )
    
    return 0
