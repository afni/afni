#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  2 14:45:07 2023

@author: peterlauren
"""
import numpy as np
import matplotlib.pyplot as plt
import scipy
from   scipy import signal as sps
import math
from   matplotlib.ticker import FormatStrFormatter
import os
import shutil

# Local libraries
import libPeakFilters as lpf
import lib_retro_plot as lrp
import lib_retro_graph as lrg

# Global constants
GLOBAL_M = 3
numSections = 1
NUM_RVT = 5

global OutDir


def getCardiacPeaks(test_retro_obj, filterPercentile=70.0):
   """
   NAME
       getCardiacPeaks
           Get peaks from cardiac time series supplied as an ASCII 
           file with one time series entry per line
           
    TYPE
        <class 'numpy.ndarray'>, int
   ARGUMENTS
       test_retro_obj:   object which includes the following fields.
               
            show_graph_level: (dType = int) Level at which to show graphs on
                screen during run time.  Levels may be one of the following.
                0: Do not show graphs at all.
                1: Show coompleted steps such as final peaks and troughs,
                   phases and RVT
                2: Also graph intermediate stages such as the intermediate
                   stages of peak and trough detection
               
            save_graph_level: (dType = int) Level at which to save graphs to
                files (without necessarily showing on screen).  Levels may be 
                one of the following.  
                0: Do not show graphs at all.
                1: Show coompleted steps such as final peaks and troughs,
                   phases and RVT
                2: Also graph intermediate stages such as the intermediate
                   stages of peak and trough detection
                   
            font_size: (dtype = <class 'int'>) Size of the font used in graphs.
       
            card_data: (dType = <class 'lib_retro_reading.phys_ts_obj'>) Object
               containing the following fields.
               
               ts_orig: (dtype = <class 'numpy.ndarray'>)  Raw input cardiac
                        time series.
                        
                min_bps: (dtype = <class 'float'>) minimum number of heartbeats 
                         per second (Between 0 and 1)
               
               samp_freq: (dtype = <class 'float'>) Number of time series
                          sample points per second.
       
       filterPercentile: (dType = float) Minimum percentile of raw 
                       data for a peak value to imply a valid peak
   AUTHOR
       Peter Lauren
   """
   
   global OutDir
   
   rawData = test_retro_obj.card_data.ts_orig
   
   # Lower bound, of heartbeats per second based based on minimum HR
   minBeatsPerSecond = test_retro_obj.card_data.min_bps
   
   # Initialise graph index
   graphIndex = 0
    
   # Band pass filter raw data
   filterData = \
        lpf.bandPassFilterRawDataAroundDominantFrequency(rawData,\
        minBeatsPerSecond,
         test_retro_obj.card_data.samp_freq,\
         show_graph = test_retro_obj.show_graph_level>1,\
         save_graph = test_retro_obj.save_graph_level>1, OutDir=OutDir,\
         graphIndex = graphIndex,  
         font_size = test_retro_obj.font_size)
   if len(filterData) == 0:
       print('Failed to band-pass filter cardiac data')   
       return []
   
   # Get initial peaks using window that is a fortieth of a second 
   # (HR <= 680 BPM)
   peaks, _ = sps.find_peaks(np.array(filterData),\
                             width=int(test_retro_obj.card_data.samp_freq/40))
   
   # Graph initial peaks and save graph to disk
   lpf.graphPeaksAgainstRawInput(test_retro_obj.show_graph_level>1, 
        test_retro_obj.save_graph_level>1, rawData, peaks, 
        test_retro_obj.card_data.samp_freq,  "Cardiac", OutDir = OutDir, 
        prefix = 'cardiacPeaksFromBPFInput', 
        caption = 'Cardiac peaks from band-pass filtered input.',
        font_size = test_retro_obj.font_size)
   
   # Graph initial cardiac peaks found by scipy
   processName = 'Initial'
   Title = 'Initial Cardiac Peaks Found By scipy'
   lrg.plotPeaks(rawData, peaks, OutDir, processName, Title, 'Cardiac', 
                 test_retro_obj, lrp, 
                 saveGraph = test_retro_obj.save_graph_level>0, 
                 showGraph = test_retro_obj.show_graph_level>0)
   
   # Adjust peaks from uniform spacing
   peaks = lpf.refinePeakLocations(peaks, rawData, test_retro_obj, lrp,
            dataType = "Cardiac",  
            phys_fs = test_retro_obj.card_data.samp_freq, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            OutDir = OutDir,
            font_size = test_retro_obj.font_size)
    
   # Remove peaks less than the required percentile of the local 
   # input signal
   peaks = lpf.localPercentileFilter(peaks, rawData, filterPercentile, 
            test_retro_obj, lrp, numPeriods=3, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            dataType = "Cardiac",  
            phys_fs = test_retro_obj.card_data.samp_freq, OutDir = OutDir,
            font_size = test_retro_obj.font_size)
   if len(peaks) == 0:
        print('*** ERROR: Failure to local percentile filter ' + \
              'cardiac peaks')
        return [], 0

   # Estimate the overall typical period using filtered cardiac 
   # time series
   period = lpf.getTimeSeriesPeriod(filterData) 
   if period < 0:     
        print('*** ERROR: Failure to get typical period using' + \
              ' filtered cardiac time series')
        return [], 0
    
   # Merge peaks that are closer than one quarter of the overall 
   # typical period
   peaks = lpf.removeClosePeaks(peaks, period, rawData, test_retro_obj, lrp, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Cardiac",  
        phys_fs = test_retro_obj.card_data.samp_freq, OutDir = OutDir,
        font_size = test_retro_obj.font_size)
   
   # Remove "peaks" that are less than the raw input a quarter of 
   # a period on right side
   # This is tomove false peaks on the upstroke
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
        rawData, test_retro_obj, lrp,  period=period, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Cardiac",  
        phys_fs = test_retro_obj.card_data.samp_freq, OutDir = OutDir,
        font_size = test_retro_obj.font_size)
   if len(peaks) == 0:
       print('ERROR in getCardiacPeaks: Peaks array empty')
       return peaks, len(rawData)
   
   # Remove false peaks on the downstroke
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
        rawData, test_retro_obj, lrp, direction='left', period=period, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Cardiac",  
        phys_fs = test_retro_obj.card_data.samp_freq, OutDir = OutDir,
        font_size = test_retro_obj.font_size)
    
   # Remove peaks that are less than a quarter as far from the 
   # local minimum to the adjacent peaks
   peaks = lpf.removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, 
        rawData, test_retro_obj, lrp, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Cardiac",  phys_fs = test_retro_obj.card_data.samp_freq, 
        OutDir = OutDir, font_size = test_retro_obj.font_size)   
   
   # Add missing peaks
   peaks = lpf.addMissingPeaks(peaks, rawData, test_retro_obj, lrp, period=period, 
                show_graph = max(test_retro_obj.show_graph_level-1,0), 
                save_graph = max(test_retro_obj.save_graph_level-1,0), 
                phys_fs = test_retro_obj.card_data.samp_freq, OutDir = OutDir,
                font_size = test_retro_obj.font_size, dataType = "Cardiac")   
   
   # Remove "peaks" that are less than the raw input a quarter of 
   # a period on right side
   # This is tomove false peaks on the upstroke
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
        rawData, test_retro_obj, lrp, period=period, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Cardiac",  phys_fs = test_retro_obj.card_data.samp_freq, 
        OutDir = OutDir, font_size = test_retro_obj.font_size)
   if len(peaks) == 0:
       print('ERROR in getCardiacPeaks: Peaks array empty')
       return peaks, len(rawData)
    
   # Remove false peaks on the downstroke
   peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
        rawData, test_retro_obj, lrp, direction='left', period=period, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Cardiac", phys_fs = test_retro_obj.card_data.samp_freq, 
        OutDir = OutDir, font_size = test_retro_obj.font_size)
      
   # Graph cardiac peaks against cardiac time series
   lpf.graphPeaksAgainstRawInput(test_retro_obj.show_graph_level>0, 
         test_retro_obj.save_graph_level>0, rawData, peaks, 
         test_retro_obj.card_data.samp_freq, 
         "Cardiac", OutDir = OutDir, prefix = 'cardiacPeaksFinal', 
         caption = 'Cardiac peaks after all filtering.',
         font_size = test_retro_obj.font_size)
   
   # Graph final cardiac peaks 
   processName = 'Final'
   Title = 'Final Cardiac Peaks'
   lrg.plotPeaks(rawData, peaks, OutDir, processName, Title, 'Cardiac', 
                 test_retro_obj, lrp, 
                 saveGraph = test_retro_obj.save_graph_level>0, 
                 showGraph = test_retro_obj.show_graph_level>0)
    
   # ==== test plot ====
   # tmp_x_rD = np.arange(len(rawData)) * test_retro_obj.card_data.samp_rate
   # tmp_x_p  = np.arange(len(rawData))[peaks] * test_retro_obj.card_data.samp_rate

   # tmp_y_p  = rawData[peaks]

   # ret_plobj1 = lrp.RetroPlobj(tmp_x_rD, rawData, 
   #                             label='raw input data', 
   #                             alpha=1.0,
   #                             color='tab:orange')
   # ret_plobj2 = lrp.RetroPlobj(tmp_x_p, tmp_y_p, 
   #                             label='cardiac peaks',
   #                             ls='None', marker=7,
   #                             ms=4, mec='white', mew=0.02,
   #                             color='tab:blue')

   # oname = 'cardiacPeaksFinal_v2.pdf'
   # if OutDir :
   #      oname = OutDir + '/' + oname
   # fff = lrp.RetroFig(figname=oname,
   #                     max_n_per_sub=5000, 
   #                     title='Cardiac peaks')
   # fff.add_plobj(ret_plobj1)
   # fff.add_plobj(ret_plobj2)
   # fff.make_plot( do_show = test_retro_obj.show_graph_level,
   #                 do_save = test_retro_obj.save_graph_level )
   
   # plt.close() # Close empty figure window
   # ==== end test plot ====

   return peaks, len(rawData)


def getRespiratoryPeaks(test_retro_obj):
    """
    NAME
        getRespiratoryPeaks
        Get peaks and troughs from respiratory time series supplied as an ASCII 
        file with one time series entry per line
    TYPE
        <class 'numpy.ndarray'>, <class 'numpy.ndarray'>, int
   ARGUMENTS
       test_retro_obj:   object which includes the following fields.
               
            show_graph_level: (dType = int) Level at which to show graphs on
                screen during run time.  Levels may be one of the following.
                0: Do not show graphs at all.
                1: Show coompleted steps such as final peaks and troughs,
                   phases and RVT
                2: Also graph intermediate stages such as the intermediate
                   stages of peak and trough detection
               
            save_graph_level: (dType = int) Level at which to save graphs to
                files (without necessarily showing on screen).  Levels may be 
                one of the following.  
                0: Do not show graphs at all.
                1: Show coompleted steps such as final peaks and troughs,
                   phases and RVT
                2: Also graph intermediate stages such as the intermediate
                   stages of peak and trough detection
                   
            font_size: (dtype = <class 'int'>) Size of the font used in graphs.
       
            resp_data: (dType = <class 'lib_retro_reading.phys_ts_obj'>) Object
               containing the following fields.
               
               ts_orig: (dtype = <class 'numpy.ndarray'>)  Raw input respiratory
                        time series.
                        
                min_bps: (dtype = <class 'float'>) minimum number of breaths 
                         per second (Between 0 and 1)
               
               samp_freq: (dtype = <class 'float'>) Sampling frequency in Hz
               
               samp_freq: (dtype = <class 'float'>) Number of time series
                          sample points per second.
    AUTHOR
    Peter Lauren
    """
    
    global OutDir
    
    rawData = test_retro_obj.resp_data.ts_orig
   
    # Set maximum breathing period of 10 seconds
    # MAX_BREATHING_PERIOD_IN_SECONDS = 10
    
    # # Determine lower bound based based on minimum HR
    # minBreathsPerSecond = 1.0/MAX_BREATHING_PERIOD_IN_SECONDS
      
    # Lower bound, of breaths per second based based on maximum period for 
    # breath
    minBreathsPerSecond = test_retro_obj.resp_data.min_bps
   
    # Band pass filter raw data
    filterData = lpf.bandPassFilterRawDataAroundDominantFrequency(rawData, 
        minBreathsPerSecond,
        test_retro_obj.resp_data.samp_freq, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, OutDir=OutDir, 
        dataType = "Respiratory", 
        font_size = test_retro_obj.font_size)
    if len(filterData) == 0:
       print('Failed to band-pass filter respiratory data')   
       return []
   
    # Get initial peaks using window that is eighth of a second  
    #  (BR <+ 480 BPM)
    peaks, _ = sps.find_peaks(np.array(filterData), 
                              width=int(test_retro_obj.resp_data.samp_freq/4))
   
    # Graph initial peaks and save graph to disk
    lpf.graphPeaksAgainstRawInput(test_retro_obj.show_graph_level>1, 
        test_retro_obj.save_graph_level>1, rawData, peaks, 
        test_retro_obj.resp_data.samp_freq, "Respiratory", OutDir = OutDir, 
        prefix = 'respiratoryPeaksFromBPFInput', 
        caption = 'Respiratory peaks from band-pass filtered ' + \
            'input.',
        font_size = test_retro_obj.font_size)
   
    # Graph initial respiratory peaks found by scipy
    processName = 'Initial'
    Title = 'Initial Respiratory Peaks Found By scipy'
    lrg.plotPeaks(rawData, peaks, OutDir, processName, Title, 'Respiratory', 
                  test_retro_obj, lrp, 
                  saveGraph = test_retro_obj.save_graph_level>0, 
                  showGraph = test_retro_obj.show_graph_level>0)
   
    # Adjust peaks from uniform spacing
    peaks = lpf.refinePeakLocations(peaks, rawData, test_retro_obj, lrp, 
            dataType = "Respiratory",  
            phys_fs = test_retro_obj.resp_data.samp_freq, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    # Get period from filtered input data 
    period = lpf.getTimeSeriesPeriod(filterData)
    if period < 0:     
        print('*** ERROR: Failure to get typical period ' + \
              'using filtered respiratory time series')
        return [], [], 0
    
    # Remove peaks that are less than the 10th percentile of the 
    # input signal
    peaks = lpf.percentileFilter(peaks, rawData, test_retro_obj, lrp, 
             percentile=10.0, dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)
    if len(peaks) == 0:
        print('*** ERROR: Failure to percentile filter ' + \
              'respiratory peaks')
        return [], [], 0
    
    # Remove "peaks" that are less than the raw input a quarter 
    # of a period on right side.  This is tomove false peaks on 
    # the upstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
             rawData, test_retro_obj, lrp, period=period, dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    # Remove false peaks on the downstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
             rawData, test_retro_obj, lrp, direction='left', period=period, 
             dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    # Remove peaks that are less than a quarter as far from the 
    # local minimum to the adjacent peaks
    peaks = lpf.removePeaksCloserToLocalMinsThanToAdjacentPeaks(peaks, 
            rawData, test_retro_obj, lrp, dataType = "Respiratory",  
            phys_fs = test_retro_obj.resp_data.samp_freq, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    # Merge peaks that are closer than one quarter of the overall 
    # typical period
    peaks = lpf.removeClosePeaks(peaks, period, rawData, test_retro_obj, lrp, 
             dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)

    # Add missing peaks
    peaks = lpf.addMissingPeaks(peaks, rawData, test_retro_obj, lrp, 
             period=period, dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)   
    
    # Remove "peaks" that are less than the raw input a quarter of 
    # a period on right side.  This is tomove false peaks on the 
    # upstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
             rawData, test_retro_obj, lrp, period=period, dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    # Remove false peaks on the downstroke
    peaks = lpf.removePeaksCloseToHigherPointInRawData(peaks, 
             rawData, test_retro_obj, lrp, direction='left', 
             period=period, dataType = "Respiratory",  
             phys_fs = test_retro_obj.resp_data.samp_freq, 
             show_graph = test_retro_obj.show_graph_level>1, 
             save_graph = test_retro_obj.save_graph_level>1, 
             OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    troughs, _ = sps.find_peaks(-np.array(filterData), 
                              width=int(test_retro_obj.resp_data.samp_freq/8))
   
    # Graph initial peaks and troughs and save graph to disk
    lpf.graphPeaksAgainstRawInput(test_retro_obj.show_graph_level>1, 
        test_retro_obj.save_graph_level>1, rawData, peaks, 
        test_retro_obj.resp_data.samp_freq, "Respiratory", troughs = troughs, 
        OutDir = OutDir, prefix = 'respiratoryPeaksFromBPFInput', 
        caption = 'Respiratory troughs from band-pass filtered input.',
          font_size = test_retro_obj.font_size)
   
    # Graph initial respiratory peaks and troughs found by scipy
    processName = 'Initial'
    Title = 'Initial Respiratory Peaks and Troughs Found By scipy'
    lrg.plotPeaksAndTroughs(rawData, peaks, troughs, OutDir, processName, Title, 
                            'Respiratory', test_retro_obj, lrp, 
                            saveGraph = test_retro_obj.save_graph_level>1, 
                            showGraph = test_retro_obj.show_graph_level>1)
    
    # Remove troughs that are more than the 90th percentile of the 
    # input signal
    troughs = lpf.percentileFilter(troughs, rawData, test_retro_obj, lrp, 
            percentile=90.0, upperThreshold=True, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            dataType = "Respiratory",  
            phys_fs = test_retro_obj.resp_data.samp_freq, OutDir = OutDir,
            font_size = test_retro_obj.font_size)
    if len(troughs) == 0:
        print('*** ERROR: Failure to percentile filter ' + \
              'respiratory troughs')
        return [], [], 0
    
    # Remove "troughs" that are greater than the raw input a 
    # quarter of a period on right side.  This is to remove false 
    # troughs on the downstroke
    troughs = lpf.removeTroughsCloseToLowerPointInRawData(troughs, 
            rawData, test_retro_obj, lrp, period=period, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            dataType = "Respiratory",  
            phys_fs = test_retro_obj.resp_data.samp_freq, OutDir = OutDir,
            font_size = test_retro_obj.font_size)
    
    # Remove false troughs on the uptroke
    troughs = lpf.removeTroughsCloseToLowerPointInRawData(troughs, 
            rawData, test_retro_obj, lrp, period=period, direction = 'left', 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
            dataType = "Respiratory",  
            phys_fs = test_retro_obj.resp_data.samp_freq, OutDir = OutDir,
            font_size = test_retro_obj.font_size)
    
    # Remove troughs that are less than a quarter as far from the 
    # local maximum to the adjacent troughs
    troughs = \
        lpf.removeTroughsCloserToLocalMaxsThanToAdjacentTroughs( \
        troughs, rawData, test_retro_obj, lrp, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Respiratory", phys_fs = test_retro_obj.resp_data.samp_freq, 
        OutDir = OutDir, font_size = test_retro_obj.font_size)
    
    # Merge troughs that are closer than a quarter of the overall 
    # typical period
    troughs = lpf.removeClosePeaks(troughs, period, rawData, test_retro_obj, lrp, 
        Troughs = True, show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Respiratory",  
        phys_fs = test_retro_obj.resp_data.samp_freq, OutDir = OutDir,
        font_size = test_retro_obj.font_size)
    
    # Remove peaks/troughs that are also troughs/peaks
    peaks, troughs = lpf.removeOverlappingPeaksAndTroughs(peaks, 
        troughs, rawData, test_retro_obj, lrp, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Respiratory",  
        phys_fs = test_retro_obj.resp_data.samp_freq, OutDir = OutDir,
        font_size = test_retro_obj.font_size)
    
    # Add missing peaks and troughs
    peaks, troughs = lpf.addMissingPeaksAndTroughs(peaks, troughs, 
        rawData, test_retro_obj, lrp, period=None, 
        show_graph = test_retro_obj.show_graph_level>1, 
        save_graph = test_retro_obj.save_graph_level>1, 
        dataType = "Respiratory",  
        phys_fs = test_retro_obj.resp_data.samp_freq, OutDir = OutDir,
        font_size = test_retro_obj.font_size)
   
    # Adjust troughs from uniform spacing
    troughs = lpf.refinePeakLocations(troughs, rawData, test_retro_obj, lrp, 
            show_graph = test_retro_obj.show_graph_level>1, 
            save_graph = test_retro_obj.save_graph_level>1, 
              dataType = "Respiratory",  
              phys_fs = test_retro_obj.resp_data.samp_freq, 
              Troughs = True, OutDir = OutDir,
              font_size = test_retro_obj.font_size)
    
    # Graph respiratory peaks and troughs against respiratory 
    # time series
    if test_retro_obj.show_graph_level>0 or test_retro_obj.save_graph_level>0:
        lpf.graphPeaksAgainstRawInput(test_retro_obj.show_graph_level>0, 
            test_retro_obj.save_graph_level>0, rawData, peaks, 
            test_retro_obj.resp_data.samp_freq, "Respiratory", troughs = troughs, 
            caption = 'Respiratory peaks after all filtering.', 
            OutDir = OutDir, 
            prefix = 'respiratoryPeaksAndTroughsFinal',
            font_size = test_retro_obj.font_size)
   
    # Graph initial respiratory peaks and troughs found by scipy
    processName = 'Final'
    Title = 'Final Respiratory Peaks and Troughs'
    lrg.plotPeaksAndTroughs(rawData, peaks, troughs, OutDir, processName, Title, 
                            'Respiratory', test_retro_obj, lrp, 
                            saveGraph = test_retro_obj.save_graph_level>0, 
                            showGraph = test_retro_obj.show_graph_level>0)

    
    # # ==== test plot ====
    # tmp_x_rD = np.arange(len(rawData)) * test_retro_obj.resp_data.samp_rate
    # tmp_x_p  = np.arange(len(rawData))[peaks] * test_retro_obj.resp_data.samp_rate
    # tmp_x_t  = np.arange(len(rawData))[troughs] * test_retro_obj.resp_data.samp_rate

    # tmp_y_p  = rawData[peaks]
    # tmp_y_t  = rawData[troughs]

    # ret_plobj1 = lrp.RetroPlobj(tmp_x_rD, rawData, 
    #                             label='raw input data', 
    #                             alpha=1.0,
    #                             color='tab:orange')
    # ret_plobj2 = lrp.RetroPlobj(tmp_x_p, tmp_y_p, 
    #                             label='resp peaks',
    #                             ls='None', marker=7, 
    #                             ms=4, mec='white', mew=0.02, 
    #                             color='tab:blue')
    # ret_plobj3 = lrp.RetroPlobj(tmp_x_t, tmp_y_t,
    #                             label='resp troughs',
    #                             ls='None', marker=6, 
    #                             ms=4, mec='white', mew=0.02,
    #                             color='tab:green')

    # oname = 'respiratoryPeaksAndTroughsFinal_v2.pdf'
    # if OutDir :
    #     oname = OutDir + '/' + oname
    # fff = lrp.RetroFig(figname=oname,
    #                    max_n_per_sub=5000, 
    #                    fontsize = test_retro_obj.font_size,
    #                    title='Respiratory peaks and troughs')
    # fff.add_plobj(ret_plobj1)
    # fff.add_plobj(ret_plobj2)
    # fff.add_plobj(ret_plobj3)
    # fff.make_plot( do_show = test_retro_obj.show_graph_level,
    #                do_save = test_retro_obj.save_graph_level )
   
    # plt.close() # Close empty figure window
    # # ==== end test plot ====
    

    return peaks, troughs, len(rawData)

def determineCardiacPhases(peaks, fullLength, phys_fs, rawData, 
                           show_graph = False, save_graph = True,
                           font_size = 10):
    """
    NAME
       determineCardiacPhases
         Determine phases, in the cardiac cycle based on the 
         Glover (2000) paper
    TYPE
        <class 'list'>
    ARGUMENTS
        peaks:   (dType int64 array) Peaks in input cardiac time 
                 series.
        
        fullLength:   (dType = int) Lenagth of cardiac time series
        
        phys_fs:   (dType = float) Physiological signal sampling 
                   frequency in Hz. 
        
        rawData: (dType = float, array) Raw cardiac data
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
                   
        font_size: (dtype = <class 'int'>) Size of the font used in graphs.

    AUTHOR
       Peter Lauren
    """

    phases = []
    inc = 0
    k = math.pi * 2
    numIntervals = len(peaks) - 1
    
    # Assign -1 to phases of all time tpoints before the 
    # first peak
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
        ax_left = plt.subplot()
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
            if show_graph: 
                plt.ion() 
                plt.show(block=True)
            else:
                plt.ioff()    
                
    # Close graph after saving
    plt.close('all')  
            
    return phases

def getACoeffs(data, phases):
    """
    NAME
       getACoeffs
           Determine a coefficients from equation 4 of Glover 
           (2000) paper (equation 4)
    TYPE
        <class 'list'>
    ARGUMENTS
        data: (dtype = <class 'numpy.ndarray'>)  Raw input cardiac
                 time series.  
        
        phases:   <class 'list'> containing phases determined 
                        as described in Glover (2000) paper
    AUTHOR
       Peter Lauren
    """

    # data = readArray(parameters, key)
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

def getBCoeffs(data, phases):
    """
    NAME
       getBCoeffs
           Determine b coefficients from equation 4 of Glover 
           (2000) paper (equation 4)
    TYPE
        <class 'list'>
    ARGUMENTS
        data: (dtype = <class 'numpy.ndarray'>)  Raw input cardiac
                 time series.  
        
        phases:   <class 'list'> containing phases determined 
                        as described in Glover (2000) paper
    AUTHOR
       Peter Lauren
    """

    # data = readArray(parameters, key)
    mean = np.mean(data)
    N = len(data)
    global GLOBAL_M
    b = []
    for m in range(1,GLOBAL_M):
        num = 0
        denom = 0
        for n in range(0,N):
            num = num + (data[n]-mean)*math.sin(m*phases[n])
            temp = math.sin(m*phases[n])
            denom = denom + (temp*temp)
        b.append(num/denom)
        
    return b
            

def determineRespiratoryPhases(resp_peaks, 
                    resp_troughs, phys_fs, rawData, 
                    show_graph = False, save_graph = True,
                    font_size = 10):
    """
    NAME
        determineRespiratoryPhases
            Determine respiratory phases as described in Glover 
            (2000) paper (equation 3)
    TYPE
        <class 'list'>
    ARGUMENTS
        resp_peaks      :   peaks in respiratory time series.  
                            Type = <class 'numpy.ndarray'>
        
        resp_troughs    :   <class 'numpy.ndarray'> containing 
                            troughs in the respiratory time series
        
        phys_fs:     Physiological signal sampling frequency in Hz 
        
        rawData:     Raw respiratory data
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
                   
        font_size: (dtype = <class 'int'>) Size of the font used in graphs.
        
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
        
    # Number of segments where each segment is either inspiration 
    # or expiration
    numFullSegments = len(resp_peaks) + len(resp_troughs) - 1
    
    # Initialize array of output phases
    phases = np.zeros(len(rawData))
    
    # Assign values to time series before first full segment
    peakIndex = 0
    troughIndex = np.argmin(resp_troughs)
    start = 0
    finish = min(resp_peaks[peakIndex], resp_troughs[troughIndex])
    if finish == 0:
        finish = max(resp_peaks[peakIndex], 
                     resp_troughs[troughIndex])
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
        
        # Count values, in segment that are not greater than the 
        # summation limit
        count = 0
        end = min(end,len(counts)-1)
        for j in range(0,end):
            count = count + counts[j]
            
        # Use result to estimate phase at given time point
        phases[i] = (math.pi*count*polarity)/denom
    
    # Switch polarity and increment peak indxe if new polarity 
    #   inspiration.  Otherwise increment trough index instead
    polarity = -polarity
    if polarity > 0: peakIndex = peakIndex + 1
    else: troughIndex = troughIndex + 1
    
    # Process each segment in turn
    peakIndex = 0
    troughIndex = 0    
    for segment in range(0,numFullSegments):    
    
        # Determine segment from the peak and trough indices
        start = min(resp_peaks[peakIndex], 
                    resp_troughs[troughIndex])
        finish = max(resp_peaks[peakIndex], 
                     resp_troughs[troughIndex])
        denom = finish - start  # Total length of segment
        
        # Histogram values in segment
        sample = [x - rawData[resp_troughs[troughIndex]] 
                  for x in rawData[start:finish]] 
        sample = sample - min(sample)
        counts, bins = np.histogram([x 
                    for x in sample if math.isnan(x) == False], 
                                    bins=NUM_BINS) 
        
        # Determine phase based on equation 3 is Glover paper
        Rmax = max(sample) # Maximum value in segment
        for i in range(start,finish): # Move through segment
            # Summation limit
            end = round(sample[i-start]*NUM_BINS/Rmax) 
            
            # Count values, in segment that are <= the 
            # summation limit
            count = 0
            end = min(end, len(counts))
            if end > 0:
                for j in range(0,end):
                    count = count + counts[j]
                
            # Use result to estimate phase at given time point
            phases[i] = (math.pi*count*polarity)/denom
            
        # Switch polarity and increment peak indxe if new polarity 
        # inspiration.  Otherwise increment trough index instead
        polarity = -polarity
        if polarity > 0: peakIndex = peakIndex + 1
        else: troughIndex = troughIndex + 1
        if peakIndex>=len(resp_peaks) or \
            troughIndex>=len(resp_troughs): break
    
    # Assign values to time series after last full segment
    start = finish
    finish = len(rawData)
    denom = finish - start  # Total length of segment
    
    # Histogram values in segment
    sample = [x - rawData[resp_troughs[-1]] \
              for x in rawData[start:finish]]  
    counts, bins = np.histogram(sample, bins=NUM_BINS) 
    
    # Determine phase based on equation 3 is Glover paper
    if polarity < 0: Rmax = max(sample)
    else: Rmax = rawData[resp_peaks[-1]] # Maximum value in segment
    for i in range(start,finish): # Move through segment
        # Summation limit
        end = round(sample[i-start]*NUM_BINS/Rmax) 
        
        if end >= len(counts):
            end = len(counts) - 1
        
        # Count values, in segment that are not greater than 
        #   the summation limit
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
        end = min(len(phases),
                  round(len(phases)*50.0/len(resp_peaks)))
        for i in range(0,end): x.append(i/phys_fs)
        fig, ax_left = plt.subplots()
        plt.xlabel("Time (s)", fontdict={'fontsize': font_size})
        plt.ylabel('Input data input value',color='g', 
                   fontdict={'fontsize': font_size})
        ax_right = ax_left.twinx()
        ax_right.plot(x, phases[0:end], color='red')
        ax_left.plot(x, rawData[0:end], color='green')
        plt.ylabel('Phase (Radians)',color='r')
        plt.title("Respiratory phase (red) and raw input " \
                  "data (green)",
                  fontdict={'fontsize': font_size})
            
        # Save plot to file
        plt.savefig('%s/RespiratoryPhaseVRawInput.pdf' % (OutDir)) 
        if save_graph:
            plt.savefig('%s/CardiacPhaseVRawInput.pdf' % (OutDir)) 
            if show_graph: 
                plt.ion() 
                plt.show(block=True)
            else:
                plt.ioff() 
                
        # Close graph after saving
        plt.close()  
        
    return phases

def limitNumOutputTimepoints(phaseData, test_retro_obj, samp_freq):
    """
    NAME
        limitNumOutputTimepoints
            Ensure number of output time points not too high
     TYPE
        <class 'int'>
    ARGUMENTS
        phaseData: (array, dType = float) Phase data determined from peaks 
                                          (and troughs)
        
        test_retro_obj: Object with the following fields.
            
            vol_tr:  (dtype = class 'float') (volume_tr) Volume 
                     repetition time (TR)
                     
            vol_nv:  (dtype = <class 'int'>) Number of output time points
                        
            n_ts_orig:  (dType = int) Number of time points in 
                           the output
               
        samp_freq: (dtype = <class 'float'>) Sampling frequency in Hz

    AUTHOR
        Peter Lauren
    """

    # Get maximum number of output time points
    # Num TR intervals covered by physio data (float)
    duration = len(phaseData)/samp_freq
    max_numTime_float = duration/test_retro_obj.vol_tr 
    eps_nt = 0.1    # Tolerance for rounding up number of TRs 
                    # (fraction of TR)
    max_numTime_pts = int(max_numTime_float + eps_nt)
    num_time_pts = test_retro_obj.vol_nv
    
    print("++ duration of physio signal:", duration)
    print("++ TR (MRI data)            :", test_retro_obj.vol_tr)
    print("++ number of TRs from physio:", max_numTime_float)
    print("++ number of TRs (as int)   :", max_numTime_pts)
    
    # # If the user has supplied the number of output times points, 
    # # it must not be greater than the determined maximum
    if num_time_pts > max_numTime_pts:    
        print('+* WARNING: num_time_pts argument too large ' + \
              'for input data')
        print('  Adjusted to maximum allowable value, ', 
              max_numTime_pts)
        num_time_pts = max_numTime_pts
        
    return num_time_pts

def getRVT(rawData, resp_peaks, resp_troughs, freq, num_time_pts, 
           TR, show_graph = False, save_graph = True, 
           interpolationOrder = 'linear', font_size = 10, RVT_lags = []):
    """
    NAME
        getRVT 
            Get Regression Volume Per Time (RVT) as described in 
            ``Separating respiratory-variation-related 
            fluctuations from neuronal-activity-related 
            fluctuations in fMRI'' by Rasmus M. Birn, Jason B. 
            Diamond, Monica A. Smith, and Peter A. Bandettini
    TYPE
        <class 'list'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        resp_peaks:  <class 'numpy.ndarray'> Peaks in input 
                     respiratory time series.
        
        resp_troughs:    <class 'numpy.ndarray'> Troughs in input 
                         respiratory time series.
        
        freq:       <class 'float'> Time point sampling frequency 
                    in Hz
        
        num_time_pts: (dType = int) Number of time points in the 
                      output
                      
        TR:  (dtype = class 'float') (volume_tr) Volume repetition time
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        interpolationOrder:    <class 'str'> Method of 
                               interpolation among critical points 
                               (peaks, troughs, etc.)  Valid 
                               values are 'linear' (the default), 
                               'quadratic' and 'cubic'.  The 
                               following are also valid but NOT 
                               recommended; ‘nearest’, 
                               ‘nearest-up’, ‘zero’, ‘slinear’, 
                               ‘previous’, or ‘next’. ‘zero’, 
                               ‘slinear’.
                   
        font_size: (dtype = <class 'int'>) Size of the font used in graphs.
                       
    AUTHOR
       Peter Lauren 
    """
       
    # Get raw RVT values
    rawRVT = getRawRVT(rawData, resp_peaks, resp_troughs, freq,
       show_graph = False, save_graph = True, 
       interpolationOrder = interpolationOrder, 
       font_size = font_size)
    
    # Get RVT regressors
    NUM_RVT = 5
    rvtRegressors = getRvtRegressors(rawRVT, NUM_RVT, freq, 
                    num_time_pts, TR, interpolationOrder,
                    RVT_lags = RVT_lags)
    
    return rvtRegressors

def getRvtRegressors(rawRVT, NUM_RVT, freq, num_time_pts, TR, 
                     interpolationOrder = 'linear', RVT_lags = []):
    """
    NAME
        getRvtRegressors 
            Get RVT regressors that can be used in the fMRI time 
            series analysis and is an estimate of the respiration 
            volume per time.  See ``Separating respiratory-
            variation-related fluctuations from neuronal-activity-
            related fluctuations in fMRI'' by Rasmus M. Birn, 
            Jason B. Diamond, Monica A. Smith, and Peter A. 
            Bandettini, NeuroImage 31 (2006) p. 1537
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        rawRVT:     <class 'numpy.ndarray'> RVT at each input 
                    time point
        
        NUM_RVT:    <class 'int'> Number of time points in the 
                    output.
        
        freq:       <class 'float'> Time point sampling frequency 
                    in Hz
        
        num_time_pts: (dType = int) Number of time points in the 
                      output
        
        TR:         <class 'float'> Volume TR.
        
        interpolationOrder:    <class 'str'> Method of 
                               interpolation among critical points 
                               (peaks, troughs, etc.)  Valid 
                               values are 'linear' (the default), 
                               'quadratic' and 'cubic'.  The 
                               following are also valid but NOT 
                               recommended; ‘nearest’, 
                               ‘nearest-up’, ‘zero’, ‘slinear’, 
                               ‘previous’, or ‘next’. ‘zero’, 
                               ‘slinear’.
                               
        RVT_lags: (dtype = <class 'list'>) List with three floating point 
                  elements in this order:
                      0: Start time (s)
                      1: End time (s)
                      2: Number of RVTs
                       
    AUTHOR
       Peter Lauren  and Joshua Zosky
    """
    
    if len(RVT_lags) == 0:
        start_time = 0
        end_time = 20
        num_rvt = NUM_RVT
    else:
        start_time = int(RVT_lags[0])
        end_time = int(RVT_lags[1])
        num_rvt = int(RVT_lags[2])
       
    time = []    
    end = len(rawRVT)
    # for i in range(0,end): time.append(i/freq)
    for i in range(0,end): time.append(i/freq)
    
    NT_InterpPts = np.zeros(num_time_pts)
    # for i in range(0,num_time_pts): NT_InterpPts[i] = i*TR
    for i in range(0,num_time_pts): NT_InterpPts[i] = i*TR
    
    # rvt_shifts = []
    # increment = -(round((end_time-start_time)/num_rvt) + 1)
    # for i in range(end_time, start_time - 1, increment): rvt_shifts.append(i)
    rvt_shifts = [start_time+i*(end_time-start_time)/(num_rvt-1) 
                  for i in range(num_rvt)]

    # 2D array of zeros
    output = np.zeros((len(rvt_shifts), num_time_pts)) 
    
    for i in range(0, num_rvt):    # Process each row
        shf = rvt_shifts[i]                # i-th RVT 
        
        # i-th RVT times sample frequency
        nsamp = -int(round(shf * freq))  
        
        # array of integers from nsamp to nsamp times the number of 
        # samples
        sind = np.add(list(range(0, len(time))), nsamp) 
                                     
        sind[np.nonzero(sind < 0)] = 0          # Rectify result
        
        # Limit result to length of input
        sind[np.nonzero(sind > (len(time) - 1))] = len(time) - 1 
        
        # Build function that maps time to rawRVT[sind]                                                  
        rvt_shf = scipy.interpolate.interp1d(    
                                                
            time, rawRVT[sind], kind = interpolationOrder, 
                bounds_error=True
        )
        rvt_shf_y = rvt_shf(NT_InterpPts) # Apply function to time
        
        # output[:][i] = rvt_shf_y
        output[:][i] = rvt_shf_y
        
    return output 
      
def getRawRVT(rawData, resp_peaks, resp_troughs, freq, 
              show_graph = 0, save_graph = 1, 
              interpolationOrder = 'linear', font_size = 10):
    """
    NAME
        getRawRVT 
            Get raw Regression Volume Per Time (RVT) as described 
            in ``Separating respiratory-variation-related 
            fluctuations from neuronal-activity-related 
            fluctuations in fMRI'' by Rasmus M. Birn, Jason B. 
            Diamond, Monica A. Smith, and Peter A. Bandettini.  
            That is, get RVT for each input time point.
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        resp_peaks:  <class 'numpy.ndarray'> Peaks in input 
                     respiratory time series.
        
        resp_troughs:  <class 'numpy.ndarray'> Troughs in input 
                       respiratory time series.
        
        freq:  <class 'float'> Time point sampling frequency in Hz
        
        show_graph:   (dType = bool) Whether to graph the results
        
        save_graph: (dType = bool) Whether to save graoh to disk
        
        interpolationOrder:    <class 'str'> Method of 
                               interpolation among critical points 
                               (peaks, troughs, etc.)  Valid 
                               values are 'linear' (the default), 
                               'quadratic' and 'cubic'.  The 
                               following are also valid but NOT 
                               recommended; ‘nearest’, 
                               ‘nearest-up’, ‘zero’, ‘slinear’, 
                               ‘previous’, or ‘next’. ‘zero’, 
                               ‘slinear’.
                       
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
         ax_right.yaxis.\
             set_major_formatter(FormatStrFormatter('%.2f'))
         ax_right.plot(x, periodLayer, color='magenta')
         plt.ylabel('Period (s)',color='magenta', 
            fontweight='bold', fontdict={'fontsize': font_size})
         ax_left.plot(x, rawRVT, color='darkgoldenrod')
         TitleStr = "Raw RVT (dark goldenrod) and raw input" + \
             " data (green).\n"
         TitleStr = TitleStr + 'Red = peak layer. Blue = ' + \
             'trough layer. Magenta = period layer'
         plt.title(TitleStr, fontdict={'fontsize': font_size})
             
         # Save plot to file
         if save_graph:
             plt.savefig('%s/RawRVTVRawInput.pdf' % (OutDir)) 
         if save_graph:
            plt.savefig('%s/CardiacPhaseVRawInput.pdf' % (OutDir)) 
            if show_graph: 
                plt.ion() 
                plt.show(block=True)
         else:
                plt.ioff()                 
                # Close graph after saving
                plt.close()  

    
    return rawRVT
    
def getPeriodLayer(resp_peaks, fullLength, freq, 
                   interpolationOrder = 'linear'):
    """
    NAME
        getPeriodLayer 
            Get a 1D layer estimating the period at each time 
            point based on interpolation of the measured period in 
            the middle of each respiratory or cardiac interval.
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        resp_peaks:  <class 'numpy.ndarray'> Peaks in input 
                     respiratory time series.
        
        fullLength:         <class 'int'> Full length of array of 
                            point on which respiratory peaks are 
                            based
        
        freq:  <class 'float'> Time point sampling frequency in Hz
        
        interpolationOrder:    <class 'str'> Method of 
                                interpolation among critical 
                                points (peaks, troughs, etc.)  
                                Valid values are 'linear' (the 
                                default), 'quadratic' and 'cubic'.  
                                The following are also valid but 
                                NOT recommended; ‘nearest’, 
                                ‘nearest-up’, ‘zero’, ‘slinear’, 
                                ‘previous’, or ‘next’. ‘zero’, 
                                ‘slinear’.
                       
    AUTHOR
       Peter Lauren 
    """
       
    # Get critical point locations
    criticalPoints = [round((i+j)/2) 
        for i, j in zip(resp_peaks[:-1], resp_peaks[1:])]
    
    # Get critical point periods
    criticalPointPeriods = [(j-i)/freq 
        for i, j in zip(resp_peaks[:-1], resp_peaks[1:])]
    
    # Output layer is found by interpoalting the periods among 
    # the critical points
    f = scipy.interpolate.interp1d(criticalPoints, 
                criticalPointPeriods, kind = interpolationOrder)    
    layer = f([x for x in \
               range(criticalPoints[0],criticalPoints[-1])])
    
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

def getLayer(rawData, criticalPoints, 
             interpolationOrder = 'linear'):
    """
    NAME
        getLayer 
            Get a 1D layer made by interpolating among a set of 
            ``critical points'' (which may be peaks or troughs).
    TYPE
        <class 'numpy.ndarray'>
    ARGUMENTS
        rawData:     <class 'numpy.ndarray'> Raw respiratory data
        
        criticalPoints:  <class 'numpy.ndarray'> Peaks or troughs 
                         in input respiratory time series.
        
        interpolationOrder:    <class 'str'> Method of 
                                interpolation among critical 
                                points (peaks, troughs, etc.)  
                                Valid values are 'linear' (the 
                                default), 'quadratic' and 'cubic'.  
                                The following are also valid but 
                                NOT recommended; ‘nearest’, 
                                ‘nearest-up’, ‘zero’, ‘slinear’, 
                                ‘previous’, or ‘next’. ‘zero’, 
                                ‘slinear’.
                       
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
    
    # Output layer is found by interpoalting the periods among the 
    # critical pts
    f = scipy.interpolate.interp1d(criticalPoints, 
                   criticalPointValues, kind = interpolationOrder)    
    layer = f([x for x in range(0,fullLength)])
    
    return layer
   
def getPhysiologicalNoiseComponents(test_retro_obj):
    """
    NAME
        getPhysiologicalNoiseComponents 
            Return physiological (respiratory and cardiac) 
            contamination components of BOLD signal
    TYPE
        test_retro_obj: (dtype = <class 'lib_retro_reading.retro_obj'>)
            Dictionary with the following fields
            
            resp_phases: (dtype = <class 'numpy.ndarray'>) Respiratory phases
            
            card_phases: (dtype = <class 'numpy.ndarray'>) Cardiac phases
            
            rvt_coeffs: (dtype = <class 'numpy.ndarray'>) RVT coefficients.  
                        I.e. coefficients of Regression Volume Per Time (RVT) 
                        as described in ``Separating respiratory-variation-
                        related fluctuations from neuronal-activity-related 
                        fluctuations in fMRI'' by Rasmus M. Birn, Jason B. 
                        Diamond, Monica A. Smith, and Peter A. Bandettini
                        
            card_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        cardiac data is sample, in Hertz
            
            resp_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        respiratory data is sampled, in Hertz
            
    ARGUMENTS
        test_retro_obj:   Object with the following fields.
        
            out_dir: (dtype = <class 'str'>) Output directory relative to the
                     current working directory.
               
            show_graph_level: (dType = int) Level at which to show graphs on
                screen during run time.  Levels may be one of the following.
                0: Do not show graphs at all.
                1: Show coompleted steps such as final peaks and troughs,
                   phases and RVT
                2: Also graph intermediate stages such as the intermediate
                   stages of peak and trough detection
               
            save_graph_level: (dType = int) Level at which to save graphs to
                files (without necessarily showing on screen).  Levels may be 
                one of the following.  
                0: Do not show graphs at all.
                1: Show coompleted steps such as final peaks and troughs,
                   phases and RVT
                2: Also graph intermediate stages such as the intermediate
                   stages of peak and trough detection
                   
            font_size: (dtype = <class 'int'>) Size of the font used in graphs.
                
            card_data:   (dtype = <class 'lib_retro_reading.phys_ts_obj'>)
                         Object containing the following fields.
                            
                        ts_orig: (dtype = <class 'numpy.ndarray'>)  Raw input 
                                 cardiac time series.
                                 
                         min_bps: (dtype = <class 'float'>) minimum number of 
                                  heartbeats per second (Between 0 and 1)
                        
                        samp_freq: (dtype = <class 'float'>) Sampling frequency 
                                   in Hz
                        
                        samp_freq: (dtype = <class 'float'>) Number of time 
                                   series sample points per second.
                                   
                        start_time: <class 'float'> Time (seconds) at which 
                                    cardiac data collection starts relative to
                                    the beginning of the MRI data collection 
                                    start.
                                    
                        end_time: <class 'float'> Time (seconds) at which 
                                    cardiac data collection ends relative to
                                    the beginning of the MRI data collection 
                                    start.
                                  
            
            resp_data:     (dtype = <class 'lib_retro_reading.phys_ts_obj'>)
                         Object containing the following fields.
                            
                        ts_orig: (dtype = <class 'numpy.ndarray'>)  Raw input 
                                 respiratory time series.
                                 
                        min_bps: (dtype = <class 'float'>) minimum number of 
                                  breaths per second (Between 0 and 1)
                        
                        samp_freq: (dtype = <class 'float'>) Sampling frequency 
                                   in Hz
                        
                        samp_freq: (dtype = <class 'float'>) Number of time 
                                   series sample points per second.
                                   
                        start_time: <class 'float'> Time (seconds) at which 
                                    respiratory data collection starts relative 
                                    to the beginning of the MRI data collection 
                                    start.
                                    
                        end_time: <class 'float'> Time (seconds) at which 
                                    respiratory data collection ends relative to
                                    the beginning of the MRI data collection 
                                    start.
            
            niml    : whether output should be in niml format 
                        instead of CSV
            
            vol_tr      : (dtype = class 'float') (volume_tr) Volume 
                        repetition time (TR) which defines the 
                        length of time            
                     
            vol_nv:  (dtype = <class 'int'>) Number of output time points
                        
            n_ts_orig:  (dType = int) Number of time points in 
                            the output
            
            resp_phases: (dtype = <class 'numpy.ndarray'>) Respiratory phases
            
            card_phases: (dtype = <class 'numpy.ndarray'>) Cardiac phases
            
            rvt_coeffs: (dtype = <class 'numpy.ndarray'>) RVT coefficients.  
                        I.e. coefficients of Regression Volume Per Time (RVT) 
                        as described in ``Separating respiratory-variation-
                        related fluctuations from neuronal-activity-related 
                        fluctuations in fMRI'' by Rasmus M. Birn, Jason B. 
                        Diamond, Monica A. Smith, and Peter A. Bandettini
                        
            card_sample_frequency: (dtype = <class 'float'>) Frequency at which
                        cardiac data is sample, in Hertz
                        
            duration_vol: (dtype = <class 'float'>) Time (in seconds) of MRI data
                          collection.
                          
            do_out_rvt: (dtype = <class 'bool'>) Whether to determine Regression 
            Volume Per Time (RVT) as described in ``Separating respiratory-
            variation-related fluctuations from neuronal-activity-related 
            fluctuations in fMRI'' by Rasmus M. Birn, Jason B. 
            Diamond, Monica A. Smith, and Peter A. Bandettini
            
            do_calc_ab: (dtype = <class 'bool'>) Whether to determine the a and
                        b coefficients as per Glover et al, Magnetic Resonance 
                        in Medicine 44:162–167 (2000).  The alternative is to
                        set the a and b coefficients to 1.0.
    AUTHOR
       Peter Lauren
    """
    
    global OutDir
    
    # Initializations
    resp_phases = np.array([]) #None
    card_phases     = []           
    
    # Get full path for output directory
    OutDir = os.path.join(os.getcwd(), test_retro_obj.out_dir)
    if os.path.exists(OutDir) and os.path.isdir(OutDir):
        shutil.rmtree(OutDir)
    os.mkdir(OutDir)
    OutDir = test_retro_obj.out_dir
    
    # Initialise sample frequencies
    cardiac_sample_frequency = None
    respiratory_sample_frequency = None   
    
    # Process cardiac data if any
    if test_retro_obj.card_data:
        card_peaks, fullLength = getCardiacPeaks(test_retro_obj) 
        
        if len(card_peaks) == 0:
            print('ERROR in getPhysiologicalNoiseComponents: ' \
                  'No cardiac peaks')
            return []
        
        rawData = test_retro_obj.card_data.ts_orig
        cardiac_sample_frequency = test_retro_obj.card_data.samp_freq
        if len(card_peaks) > 0:
            card_phases = determineCardiacPhases(card_peaks, 
                    fullLength,  cardiac_sample_frequency, 
                    rawData,  
                    show_graph = test_retro_obj.show_graph_level>0, 
                    save_graph = test_retro_obj.save_graph_level>0,
                    font_size = test_retro_obj.font_size)
    
            # Trim phase data before start time
            if test_retro_obj.card_data.start_phys_idx > 0:
                card_phases = \
                    card_phases[test_retro_obj.card_data.start_phys_idx:]
                    
            plt.close('all') 
        
        # Ensure number of output time points not too high
        num_time_pts = limitNumOutputTimepoints(card_phases, test_retro_obj,
                                      cardiac_sample_frequency)
        
    # Process respiratory data if any
    if test_retro_obj.resp_data:

        resp_peaks, resp_troughs, fullLength = \
            getRespiratoryPeaks(test_retro_obj) 
        if len(resp_peaks) == 0:
            print('*** EOORO: Error getting respiratory ' + \
                  'peaks or troughs')
            return []            
        
        rawData = test_retro_obj.resp_data.ts_orig
        respiratory_sample_frequency = test_retro_obj.resp_data.samp_freq
        resp_phases = determineRespiratoryPhases(resp_peaks,
                   resp_troughs, respiratory_sample_frequency,
                   rawData, 
                   show_graph = test_retro_obj.show_graph_level>0, 
                   save_graph = test_retro_obj.save_graph_level>0,
                   font_size = test_retro_obj.font_size)

        # Trim phase data before start time
        if test_retro_obj.resp_data.start_time < 0:
            leading_length = round(-test_retro_obj.resp_data.start_time *\
                test_retro_obj.resp_data.samp_freq)
            resp_phases = resp_phases[leading_length:]
            
        # Trim phase at end
        lastTime = min(test_retro_obj.resp_data.end_time,
                           test_retro_obj.duration_vol)
        lastIndex = round(lastTime * test_retro_obj.resp_data.samp_freq)
        resp_phases = resp_phases[:lastIndex]
        
        # Ensure number of output time points not too high
        num_time_pts = limitNumOutputTimepoints(rawData, test_retro_obj, 
                                     respiratory_sample_frequency)
            
        if test_retro_obj.do_out_rvt:
            rvt_coeffs = getRVT(rawData, resp_peaks, resp_troughs, 
                         respiratory_sample_frequency,
                         num_time_pts, 
                         test_retro_obj.vol_tr, 
                         show_graph = test_retro_obj.show_graph_level>0, 
                         save_graph = test_retro_obj.save_graph_level>0, 
                         interpolationOrder = 'linear', 
                         font_size = test_retro_obj.font_size,
                         RVT_lags = test_retro_obj.RVT_lags)
        
        plt.close('all') 
    else:
        if test_retro_obj.do_out_rvt: 
            print('+* WARNING: Cannot determine RVT.  No ' + \
                  'respiratory data')
        test_retro_obj.do_out_rvt = False
                                      
    # Default: a and b coefficients set to 1.0
    cardiacACoeffs = [1.0]
    respiratoryACoeffs = [1.0]
    cardiacBCoeffs = [1.0]
    respiratoryBCoeffs = [1.0]
    cardiacACoeffs.append(1.0)
    respiratoryACoeffs.append(1.0)
    cardiacBCoeffs.append(1.0)
    respiratoryBCoeffs.append(1.0)

    if test_retro_obj.do_calc_ab:    # Determine a and b coefficients as 
                               # per Glover et al, Magnetic 
                               # Resonance in Medicine 44:162–167 
                               # (2000)
        # Get a and b coefficients
        if test_retro_obj.card_data:
            # Get a coefficients
            cardiacACoeffs = getACoeffs(test_retro_obj.card_data.ts_orig, 
                                    card_phases)
            # Get b coefficients
            cardiacBCoeffs = getBCoeffs(test_retro_obj.card_data.ts_orig, 
                                    card_phases)
            
        if test_retro_obj.resp_data:
            # Get a coefficients
            respiratoryACoeffs = getACoeffs(test_retro_obj.resp_data.ts_orig, 
                                        resp_phases)        
            # Get b coefficients
            respiratoryBCoeffs = getBCoeffs(test_retro_obj.resp_data.ts_orig, 
                                        resp_phases)
    
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
    print('++ Maximum length of phase array =', T)

    nreg = 0              # number of regressors we make
    for t in range(0,T):
        # sum = 0
        addend = []
        if len_resp :
            for m in range(1,GLOBAL_M):
                m0 = m - 1
                addend.append(respiratoryACoeffs[m0] * \
                              math.cos(m*resp_phases[t]))
                addend.append(respiratoryBCoeffs[m0] * \
                              math.sin(m*resp_phases[t]))
                if not(t):
                    nreg+= 2
        if len_card:
            for m in range(1,GLOBAL_M):
                m0 = m - 1
                addend.append(cardiacACoeffs[m0] * \
                              math.cos(m*card_phases[t]))
                addend.append(cardiacBCoeffs[m0] * \
                              math.sin(m*card_phases[t]))
                if not(t):
                    nreg+= 2
        data.append(addend)

    # Reshape matrix according to number of slices
    nrow = np.shape(data)[0]
    data = np.reshape(data[0:nrow-(nrow%numSections)][:], 
                      (nreg*numSections, -1)).T
        
    # return physiological noise parameters  
    physiologicalNoiseComponents = dict()
    physiologicalNoiseComponents['resp_phases'] = resp_phases
    physiologicalNoiseComponents['card_phases'] = card_phases
    if test_retro_obj.do_out_rvt: 
        physiologicalNoiseComponents['rvt_coeffs'] = rvt_coeffs
    else: physiologicalNoiseComponents['rvt_coeffs'] = []
    physiologicalNoiseComponents['num_time_pts'] = num_time_pts
    physiologicalNoiseComponents['card_sample_frequency'] = \
        cardiac_sample_frequency
    physiologicalNoiseComponents['resp_sample_frequency'] = \
        respiratory_sample_frequency
        
    return physiologicalNoiseComponents
