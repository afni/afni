#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 16 13:05:57 2023

@author: peterlauren

"""

import numpy as np
import matplotlib.pyplot as plt

def plotPeaks(rawData, peaks, OutDir, processName, Title, dataType, 
              test_retro_obj, lrp, saveGraph = True, showGraph = False, 
              Troughs = False):
   '''
    NAME
        plotPeaks
        Graph raw input data along with peaks or troughs
    TYPE
         <void>
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        peaks: (array dType = int64) Array of peaks to be displayed
        
        OutDir: (dType = str) String defining the directory to which the graph
                              is written. Not used if it is not required to save
                              the graph to disk.
        
        processName: (dType = str) String defining the name of the process, the
                     effect of which the graph displays.
                              
        Title: (dType = str) String defining the title shown on the graph
        
        dataType: (dType = str) String defining the physiological data type.
                                May be "Cardiac" or "Respiratory"
                                
        test_retro_obj: (dType = <class 'lib_retro_reading.retro_obj'>) Object 
                        for starting the retroicor process for making physio
                        regressors for MRI data.  It contains the following 
                        fields.
            card_data: (dType = <class 'lib_retro_reading.phys_ts_obj'>) Object
                       for cardiac data.  It contains the following fields.
                  samp_rate: (dType = <class 'float'>) Physical sampling rate  
                       (in sec)
            font_size: (dType = <class 'int'>) Font size for output images.
            
        lrp: (dType = <class 'module'>) Object for holding one time series or 
             set of points for plotting.
        
        saveGraph:   (dType = bool) Whether to graph the results
        
        saveGraph: (dType = bool) Whether to save graoh to disk
        
        Troughs: (dType = bool) Whether to treat the "peaks" argument as troughs

    AUTHOR
        Peter Lauren
   '''
   
   # Downsample display if too many points
   if dataType == "Respiratory":
       maxDisplayRawDataLen = test_retro_obj.resp_data.maxDisplayRawDataLen
       maxDisplaySampleFreq = test_retro_obj.resp_data.maxDisplaySampleFreq
       samp_freq = test_retro_obj.resp_data.samp_freq
   else:
       maxDisplayRawDataLen = test_retro_obj.card_data.maxDisplayRawDataLen
       maxDisplaySampleFreq = test_retro_obj.card_data.maxDisplaySampleFreq
       samp_freq = test_retro_obj.card_data.samp_freq
   if len(rawData) > maxDisplayRawDataLen and samp_freq >  maxDisplaySampleFreq:
            stepSize = round(samp_freq/200)
            rawData = rawData[::stepSize] 
            peaks = np.round(peaks/stepSize).astype(int)
           
   # Determine file index
   img_idx = popFileIndex(test_retro_obj, dataType)
   
   # New graph format
   if Troughs:
       filePrefix = "{phystype}_{ii:02d}_Troughs{process}".\
           format(phystype=dataType, ii=img_idx, process=processName)
   else:
       filePrefix = "{phystype}_{ii:02d}_Peaks{process}".\
           format(phystype=dataType, ii=img_idx, process=processName)
    
   tmp_x_rD = np.arange(len(rawData)) * test_retro_obj.card_data.samp_rate
   tmp_x_p  = np.arange(len(rawData))[peaks] * test_retro_obj.card_data.samp_rate

   tmp_y_p  = rawData[peaks]

   ret_plobj1 = lrp.RetroPlobj(tmp_x_rD, rawData, 
                               label='raw input data', 
                               alpha=1.0,
                               color='tab:orange')
   if Troughs:
       Label = dataType + ' troughs'
   else:
       Label = dataType + ' peaks'
   ret_plobj2 = lrp.RetroPlobj(tmp_x_p, tmp_y_p, 
                               label=Label,
                               ls='None', marker=7,
                               ms=4, mec='white', mew=0.02,
                               color='tab:blue')
   
   # plt.ion() This causes the fig to flash, when show fig false and is not 
   #    necessary to show fig

   oname = filePrefix + '.pdf'
   if OutDir :
        oname = OutDir + '/' + oname
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=5000, 
                       title=Title)
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=5000, 
                       fontsize = test_retro_obj.font_size,
                       title=Title)
   fff.add_plobj(ret_plobj1)
   fff.add_plobj(ret_plobj2)
   fff.make_plot( do_show = showGraph, do_save = saveGraph)

   # if showGraph:
   #     fff.make_plot( do_show = True, do_save = False)
   # if saveGraph:
   #     fff.make_plot( do_show = False, do_save = True)
    
   plt.close() # Close empty figure window

def plotPeaksAndTroughs(rawData, peaks, troughs, OutDir, processName, Title, 
                        dataType, test_retro_obj, lrp, saveGraph = True, 
                        showGraph = False):
   '''
    NAME
        plotPeaksAndTroughs
        Graph raw input data along with peaks and troughs
    TYPE
         <void>
    ARGUMENTS
        rawData: (array, dType = float) Raw input data
        
        peaks: (array dType = int64) Array of peaks to be displayed
        
        troughs: (array dType = int64) Array of troughs to be displayed
        
        OutDir: (dType = str) String defining the directory to which the graph
                              is written. Not used if it is not required to save
                              the graph to disk.
        
        processName: (dType = str) String defining the name of the process, the
                     effect of which the graph displays.
                              
        Title: (dType = str) String defining the title shown on the graph
        
        dataType: (dType = str) String defining the physiological data type.
                                May be "Cardiac" or "Respiratory"
                                
        test_retro_obj: (dType = <class 'lib_retro_reading.retro_obj'>) Object 
                        for starting the retroicor process for making physio
                        regressors for MRI data.  It contains the following 
                        fields.
            card_data: (dType = <class 'lib_retro_reading.phys_ts_obj'>) Object
                       for cardiac data.  It contains the following fields.
                  samp_rate: (dType = <class 'float'>) Physical sampling rate  
                       (in sec)
            font_size: (dType = <class 'int'>) Font size for output images.
            
        lrp: (dType = <class 'module'>) Object for holding one time series or 
             set of points for plotting.
        
        saveGraph:   (dType = bool) Whether to graph the results
        
        saveGraph: (dType = bool) Whether to save graoh to disk

    AUTHOR
        Peter Lauren
   '''
   
   # Downsample display if too many points
   if dataType == "Respiratory":
       maxDisplayRawDataLen = test_retro_obj.resp_data.maxDisplayRawDataLen
       maxDisplaySampleFreq = test_retro_obj.resp_data.maxDisplaySampleFreq
       samp_freq = test_retro_obj.resp_data.samp_freq
   else:
       maxDisplayRawDataLen = test_retro_obj.card_data.maxDisplayRawDataLen
       maxDisplaySampleFreq = test_retro_obj.card_data.maxDisplaySampleFreq
       samp_freq = test_retro_obj.card_data.samp_freq
   if len(rawData) > maxDisplayRawDataLen and samp_freq >  maxDisplaySampleFreq:
            stepSize = round(samp_freq/200)
            rawData = rawData[::stepSize] 
            peaks = np.round(peaks/stepSize).astype(int)
            troughs = np.round(troughs/stepSize).astype(int)
           
   # Determine file index
   img_idx = popFileIndex(test_retro_obj, dataType)
   
   # New graph format
   filePrefix = "{phystype}_{ii:02d}_Peaks&Troughs{process}".\
        format(phystype=dataType, ii=img_idx, process=processName)
    
   tmp_x_rD = np.arange(len(rawData)) * test_retro_obj.card_data.samp_rate
   tmp_x_p  = np.arange(len(rawData))[peaks] * test_retro_obj.card_data.samp_rate
   tmp_x_t  = np.arange(len(rawData))[troughs] * test_retro_obj.card_data.samp_rate

   tmp_y_p  = rawData[peaks]
   tmp_y_t  = rawData[troughs]

   ret_plobj1 = lrp.RetroPlobj(tmp_x_rD, rawData, 
                               label='raw input data', 
                               alpha=1.0,
                               color='tab:orange')
   ret_plobj2 = lrp.RetroPlobj(tmp_x_p, tmp_y_p, 
                               label=dataType + ' peaks',
                               ls='None', marker=7,
                               ms=4, mec='white', mew=0.02,
                               color='tab:blue')
   ret_plobj3 = lrp.RetroPlobj(tmp_x_t, tmp_y_t, 
                               label=dataType + ' troughs',
                               ls='None', marker=7,
                               ms=4, mec='white', mew=0.02,
                               color='tab:green')
   
   # plt.ion() This causes the fig to flash, when show fig false and is not 
   #    necessary to show fig

   oname = filePrefix + '.pdf'
   if OutDir :
        oname = OutDir + '/' + oname
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=5000, 
                       title=Title)
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=5000, 
                       fontsize = test_retro_obj.font_size,
                       title=Title)
   fff.add_plobj(ret_plobj1)
   fff.add_plobj(ret_plobj2)
   fff.add_plobj(ret_plobj3)
   fff.make_plot( do_show = showGraph, do_save = saveGraph)
    
   plt.close() # Close empty figure window
   
def popFileIndex(test_retro_obj, dataType):
    if dataType=='Respiratory':
         img_idx = test_retro_obj.resp_data.img_idx
         test_retro_obj.resp_data.img_idx += 1
    else:
         img_idx = test_retro_obj.card_data.img_idx
         test_retro_obj.card_data.img_idx += 1  

    return img_idx          

            