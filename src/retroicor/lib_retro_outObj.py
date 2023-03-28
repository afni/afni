#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 23 09:51:52 2023

@author: peterlauren
"""

import numpy as np

class PhysionlogicalNoise:
  def __init__(self, phases, nTimePoints, sampleFrequency, rvt_coeffs = None):
    self.phases = phases
    self.nTimePoints = nTimePoints
    self.sampleFrequency = sampleFrequency
    self. rvt_coeffs = rvt_coeffs
    
  def selectPhaseListAndNumTimeSteps(self):
        """
        NAME
            selectPhaseListAndNumTimeSteps 
                Recover determined phase for each input time step, along with 
                the number of time steps.
        TYPE
            <Void>
        ARGUMENTS
            N/A
                       
        AUTHOR
           Peter Lauren  
        """  
        
        self.numTimeSteps = len(self.phases)
        if self.numTimeSteps == 0:
            print('*** Error in selectPhaseListAndNumTimeSteps')
            print('*** Respiratory phases required but ' + \
                  'none available')
            return 1
        
        self.phaseList = self.phases
        
  def setUpTimeSeries(self, test_retro_obj):
      """
         NAME
            setUpTimeSeries 
                Set up number of output volumetric time points.
        TYPE
            <Void>
        ARGUMENTS
            test_retro_obj
                vol_tr:  (dtype = class 'float') Volume repetition time (TR) 
                            which defines the length of time 
                       
        AUTHOR
           Peter Lauren  
      """  
        
      #initialize output from input parameters
      self.timeStepIncrement = 1.0/self.sampleFrequency
      
      self.sampleTimes = np.zeros(self.numTimeSteps) # 't' in dictionary
      for i in range(1,self.numTimeSteps): self.sampleTimes[i] = \
          self.timeStepIncrement * i
          
      self.time_series_time = np.arange(
          0, (max(self.sampleTimes) - 0.5 * test_retro_obj.vol_tr), 
              test_retro_obj.vol_tr
              )
        
      # Reduce number of output time points to user-specified value 
      # if required.
      if self.nTimePoints:
        self.time_series_time = self.time_series_time[0:self.nTimePoints]  
    
      if (max(self.sampleTimes) - 0.5 * test_retro_obj.vol_tr) % \
        test_retro_obj.vol_tr == 0:
        self.time_series_time = np.append(
            self.time_series_time,
            [self.time_series_time[-1] + \
             test_retro_obj.vol_tr],
        )
            
  def initializePhaseSlices(self, test_retro_obj):
    """
         NAME
            initializePhaseSlices 
                Initialize arrays for phase slices and their times.
        TYPE
            <Void>
        ARGUMENTS
            test_retro_obj
                n_slice_times:  (dtype = <class 'int'>) Number of slices
                       
        AUTHOR
           Peter Lauren  
    """  
        
    # Initialize registered phase slices which will be fed into output file data
    self.phase_slice_reg = np.zeros(
        (len(self.time_series_time), 4, 
             test_retro_obj.n_slice_times)
    )

    self.phase_slice = np.zeros(
        (len(self.time_series_time), 
             test_retro_obj.n_slice_times)
    )
    
  def fillSliceRegressorArray(self, test_retro_obj):
    """
         NAME
            fillSliceRegressorArray 
                Determine sinusoids for each slice for each volume.
        TYPE
            <Void>
        ARGUMENTS
            test_retro_obj
                n_slice_times:  (dtype = <class 'int'>) Number of slices
                
                vol_slice_times: (dtype = <class 'list'>) list of floats for 
                                 slice timing
                       
        AUTHOR
           Peter Lauren  
    """  
        
    # phasee["time_series_time"] are TR * index.  I.e. integral 
    # multiples of TR, starting at zero
    for i_slice in range(test_retro_obj.n_slice_times):
        # To the TR multiples, add the determined slice pattern 
        # time for the current index
        tslc = self.time_series_time + test_retro_obj.vol_slice_times[i_slice]
        
        # For each multiple of TR (time point)
        for i in range(len(self.time_series_time)): 
            # Get index of minimu absolute difference between the input time
            # series times (s) and the composite time determined above
            imin = np.argmin(abs(tslc[i] - self.sampleTimes))
            # Phase, for this time and slice, is the pahse, for the data type, 
            # at that index
            self.phase_slice[i, i_slice] = self.phaseList[imin]
            
        # Make four regressors for each slice.  First dimension is 
        # the time and the last is the
        # slice.  Regressors as defined in Glover paper.
        self.phase_slice_reg[:, 0, i_slice] = np.sin(
            self.phase_slice[:, i_slice]
        )
        self.phase_slice_reg[:, 1, i_slice] = np.cos(
            self.phase_slice[:, i_slice]
        )
        self.phase_slice_reg[:, 2, i_slice] = np.sin(
            2 * self.phase_slice[:, i_slice]
        )
        self.phase_slice_reg[:, 3, i_slice] = np.cos(
            2 * self.phase_slice[:, i_slice]
        )
      

  def makeRegressorsForEachSlice(self, test_retro_obj):
    """
         NAME
            makeRegressorsForEachSlice 
                Determine sinusoids for each slice for each volume.
        TYPE
            <Void>
        ARGUMENTS
            test_retro_obj
                n_slice_times:  (dtype = <class 'int'>) Number of slices
                
                vol_slice_times:
                       
        AUTHOR
           Peter Lauren  
    """  
        
    # Select phase list and get number of time steps
    self.selectPhaseListAndNumTimeSteps()
    if self.numTimeSteps == None: return 1
    
    # Set up time series
    self.setUpTimeSeries(test_retro_obj)
    
    self.initializePhaseSlices(test_retro_obj)
    
    self.fillSliceRegressorArray(test_retro_obj)
    
  def getPhysiologicalInfo(self, test_retro_obj):
    
    # Initialize respiratory and cardiac dictionaries
    if len(self.phases) > 0:
        self.makeRegressorsForEachSlice(test_retro_obj)
    
    if len(self.phases) > 0:
        self.makeRegressorsForEachSlice(test_retro_obj)
        
        if self.rvt_coeffs is not None:
            self.rvt_shifts = list(range(0, 21, 5))
            self.rvtrs_slc = \
                np.zeros((len(self.rvt_shifts), 
                            len(self.time_series_time)))
                
  def getNimlDimensions(self):
        self.nTimeSteps   = 0
        self.nRvtSlices = 0
        self.nRespiratoryPhaseSlices = 0
        self.nCardiacPhaseSlices   = 0
    
        if len(self.phases) > 0:
            if "time_series_time" in dir(self):
                self.nTimeSteps = len(self.time_series_time)
                self.nPhaseSlices = np.size(self.phase_slice_reg, 1)
                if self.rvt_coeffs is not None:
                    self.nRvtSlices = np.size(self.rvtrs_slc, 0)
                    
def makeOutputHeaderAndOutputInfo(test_retro_obj, cardiacNoise, 
                                  respiratoryNoise):
    """
         NAME
            makeOutputHeaderAndOutputInfo 
                Make output NIML header, matrix and metadata.
        TYPE
            <Void>
        ARGUMENTS
            test_retro_obj
                n_slice_times:  (dtype = <class 'int'>) Number of slices
                
                vol_slice_times: (dtype = <class 'list'>) list of floats for 
                                 slice timing
                                 
                do_out_rvt:   (dtype = <class 'int'>) Whether to calculate
                             and output RVT. (0 = No, 1 = Yes, Defautl = 0)
                             
                vol_nv: (dtype = <class 'int'>) Nvol (num_time_pts) MRI EPI 
                
            cardiacNoise:
            respiratoryNoise: Objects, of type PhysionlogicalNoise, with cardiac
                              and respiratory information to output
                       
        AUTHOR
           Peter Lauren  
    """  
        
    outputInfo = dict()
    outputInfo["resp_out"] = \
        len(respiratoryNoise.phases) > 0
    outputInfo["card_out"] = \
        len(cardiacNoise.phases) > 0
    
    # # y-axis is the number of columns in the output file
    temp_y_axis = test_retro_obj.n_slice_times * ( # Num. output slices
        # Num. RVT entries per slices
        (test_retro_obj.do_out_rvt) * int(respiratoryNoise.nRvtSlices) 
        + (outputInfo["resp_out"]) * int(respiratoryNoise.nPhaseSlices)
                                                # Num. resp entries per slices
        + (outputInfo["card_out"]) * int(cardiacNoise.nPhaseSlices)
                                                # Num. card entries per slices
    )
    
    # # Initialize 2D output data array.  Former dimension is the number of rows
    # # while the latter dimension is the number of columns
    nTimeSteps = min(cardiacNoise.nTimeSteps, respiratoryNoise.nTimeSteps)
    outputInfo["reml_out"] = np.zeros((nTimeSteps, temp_y_axis))

    # # Check number of time points
    if np.size(outputInfo["reml_out"], 
            0) != test_retro_obj.vol_nv:
        print('***ERROR: Mismatch between ni_dimen' +
              ' (',np.size(outputInfo["reml_out"], 0), 
              ') and user supplied ' +
              'num_time_pts (', test_retro_obj.vol_nv, 
                              ')')
        return None, None

    # # Make output file header without the column labels
    header = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = "'
        % (np.size(outputInfo["reml_out"], 1), 
            np.size(outputInfo["reml_out"], 0))
    )

    outputInfo["slice_major"] = 1
    outputInfo["reml_out"]    = []
    
    # Get RVT coefficients if required.
    if test_retro_obj.do_out_rvt: respiratoryNoise.rvtrs_slc =\
                    respiratoryNoise.rvt_coeffs

    # Fill output array (test_retro_obj.do_out_rvt).  There are about four
    # coefficients within each slice
    cnt = 0
    # Process each column of output
    for i in range(0, test_retro_obj.n_slice_times):
        # Coefficients within each slice
        if test_retro_obj.do_out_rvt != 0:
            # RVT: 2D array. Former dimension is number of coefficients per 
            # slice. Same coefficients for every slice
            for j in range(0, np.shape(respiratoryNoise.rvtrs_slc )[0]):
                cnt += 1
                outputInfo["reml_out"].append(
                    respiratoryNoise.rvtrs_slc [j] # Single column of output
                )  # same regressor for each slice
                header = "%s s%d.RVT%d ;" % (header, i, j) # Append column label
        if outputInfo["resp_out"] != 0:
            # Resp. 3D array.  First index is the number or output rows.  2nd
            # is the number of coeffcieints per slice.  3rd is the number of 
            # slices
            for j in range(0, # Each coefficient for given slice
                np.shape(respiratoryNoise.phase_slice_reg)[1]):
                cnt += 1
                outputInfo["reml_out"].append(
                    respiratoryNoise.phase_slice_reg[:, j, i]
                ) # Append output column
                header = "%s s%d.Resp%d ;" % (header, i, j) # Append column label
        if outputInfo["card_out"] != 0:
            # Card. 3D array.  First index is the number or output rows.  2nd
            # is the number of coeffcieints per slice.  3rd is the number of 
            # slices
            for j in range(0, # Each coefficient for given slice 
                np.shape(cardiacNoise.phase_slice_reg)[1]):
                cnt += 1
                outputInfo["reml_out"].append(
                    cardiacNoise.phase_slice_reg[:, j, i]
                ) # Append output column
                header = "%s s%d.Card%d ;" % (header, i, j) # Append column label
    
    # remove very last ';' from header
    header = header[1:-2]
                
    return header, outputInfo
    
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
                         and output RVT. (0 = No, 1 = Yes, Defautl = 0)
        
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
            
            out_dir: (dtype = <class 'str'>) Durectory search path relative to
                     the working directory.
                       
    AUTHOR
       Peter Lauren  
    """
    
    # Initialise physiological noise objects
    cardiacNoise = PhysionlogicalNoise(
        physiologicalNoiseComponents['card_phases'], 
        physiologicalNoiseComponents['num_time_pts'], 
        physiologicalNoiseComponents['card_sample_frequency'])
    respiratoryNoise = PhysionlogicalNoise(
        physiologicalNoiseComponents['resp_phases'], 
        physiologicalNoiseComponents['num_time_pts'], 
        physiologicalNoiseComponents['resp_sample_frequency'],
        rvt_coeffs = physiologicalNoiseComponents['rvt_coeffs'])
    
    cardiacNoise.getPhysiologicalInfo(test_retro_obj)
    respiratoryNoise.getPhysiologicalInfo(test_retro_obj)
    
    cardiacNoise.getNimlDimensions()
    respiratoryNoise.getNimlDimensions()
    
    # Make output header and output matrix
    outputHeader, outputInfo = makeOutputHeaderAndOutputInfo(test_retro_obj, 
                                                cardiacNoise, respiratoryNoise)
    if not outputInfo:
        print('** ERROR getting main info and label for NIML File')
        return 1
    
    # # Output file
    tail = '"\n>'
    tailclose = "</RetroTSout>"
    np.savetxt(
        "./%s/%s.slibase.1D" % (test_retro_obj.out_dir, 
        test_retro_obj.prefix),
        np.column_stack(outputInfo["reml_out"]),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (outputHeader, tail)),
        footer=("%s" % tailclose),
    )
    
    return 0

