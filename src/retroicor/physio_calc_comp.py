#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 18 16:37:31 2023

@author: peterlauren
"""

import sys
import lib_physio_calc_cmp_opts    as lpo
from   datetime   import datetime
import os
import numpy as np
import lib_retro_plot as lrp
import shutil

# Allowed options
DEF = {
    'resp_file'         : None,      # (str) fname for resp data
    'card_file'         : None,      # (str) fname for card data
    'phys_file'         : None,      # (str) fname of physio input data
    'phys_json'         : None,      # (str) fname of json file
    'card_peak_files'   : None,      # (str) fnames of cardiac peak file
    'resp_peak_files'   : None,      # (str) fnames of respiratory peak file
    'resp_trough_files' : None,      # (str) fnames of respiratory trough files
    'slice_times'       : None,      # (list) slice times
    'slice_pattern'     : None,      # (str) code or file for slice timing
    'freq'              : None,      # (float) freq, in Hz
    'num_slices'        : None,      # (int) number of MRI vol slices
    'volume_tr'         : None,      # (float) TR of MRI
    'num_time_pts'      : None,      # (int) Ntpts (e.g., len MRI time series)
    'start_time'        : None,      # (float) leave none, bc can be set in json
    'dset_epi'          : None,      # (str) name of MRI dset, for vol pars
    'out_dir'           : None,      # (str) output dir name
    'prefix'            : 'physio',  # (str) output filename prefix
    'do_fix_nan'        : False,     # (str) fix/interp NaN in physio
    'do_fix_null'       : False,     # (str) fix/interp null/missing in physio
    'do_fix_outliers'   : False,     # (list) fix/interp outliers
    'extra_fix_list'    : [],        # (list) extra values to fix
    'remove_val_list'   : [],        # (list) purge some values from ts
    'font_size'         : 10,        # (float) font size for plots 
    'show_graph_level'  : 0,         # (int) amount of graphs to show
    'save_graph_level'  : 1,         # (int) amount of graphs to save
    'ver'               : False,     # (bool) do show ver num?
    'help'              : False,     # (bool) do show help in term?
    'hview'             : False,     # (bool) do show help in text ed?
    'RVT_lags'          : [],        # (dict) start, end and number of RVTs 
    'maxDisplayRawRespDataLen' : 10000, # Maximum respiratory raw data length 
                                     # used for display
    'maxDisplayRespSampleFreq' : 200, # Maximum respiratory sampling frequency 
                                     # used for display
    'maxDisplayRawCardDataLen' : 10000, # Maximum cardiac raw data length 
                                     # used for display
    'maxDisplayCardSampleFreq' : 200, # Maximum cardiac sampling frequency 
                                     # used for display
}

def readInputData(args_dict):
    output = {
        'cardiacRawData' : None,
        'respiratoryRawData' : None,
        'cardiacPeaksRef' : None,
        'respiratoryPeaksRef' : None,
        'respiratoryTroughsRef' : None,
        'cardiacPeaksTarget' : None,
        'respiratoryPeaksTarget' : None,
        'respiratoryTroughsTarget' : None,
        }
    
    # Get cardiac peaks and raw cardiac data
    if args_dict['card_file'] and args_dict['card_peak_files']:
        output['cardiacRawData'] = np.loadtxt(args_dict['card_file'])
        card_peak_files = args_dict['card_peak_files'].split(' ')
        output['cardiacPeaksRef'] = np.loadtxt(card_peak_files[0]).astype(int)
        output['cardiacPeaksTarget'] = np.loadtxt(card_peak_files[1]).astype(int)
    
    # Get respiratory peaks and raw respiratory data
    if args_dict['resp_file'] and args_dict['resp_peak_files']:
        output['respiratoryRawData'] = np.loadtxt(args_dict['resp_file'])
        resp_peak_files = args_dict['resp_peak_files'].split(' ')
        output['respiratoryPeaksRef'] = np.loadtxt(resp_peak_files[0]).astype(int)
        output['respiratoryPeaksTarget'] = np.loadtxt(resp_peak_files[1]).astype(int)
    
    # Get respiratory troughs and raw respiratory data
    if args_dict['resp_file'] and args_dict['resp_trough_files']:
        output['respiratoryRawData'] = np.loadtxt(args_dict['resp_file'])
        resp_trough_files = args_dict['resp_trough_files'].split(' ')
        output['respiratoryTroughsRef'] = np.loadtxt(resp_trough_files[0]).astype(int)
        output['respiratoryTroughsTarget'] = np.loadtxt(resp_trough_files[1]).astype(int)
        
    return output

def medianDownsampleRawDataPeaksTroughs(rawData, refPeaksTroughs, 
   targetPeaksTroughs, downsample_factor):
   rawData = medianDownsampleArray(rawData, downsample_factor)
   refPeaksTroughs = np.rint(refPeaksTroughs/downsample_factor).astype(int)
   targetPeaksTroughs = np.rint(targetPeaksTroughs/downsample_factor).astype(int)
   
   return rawData, refPeaksTroughs, targetPeaksTroughs
    
def medianDownsampleArray(array, downsample_factor):
   end =  downsample_factor * int(len(array)/downsample_factor)
   newArray = np.median(array[:end].reshape(-1, downsample_factor), 1)
   if (len(array[end:])>0): 
       newArray = np.append(newArray, np.median(array[end:]))
   
   return newArray

def plotPeakTroughComparisons(dataType, rawData, refPeaksTroughs, targetPeaksTroughs, samp_rate, font_size, showGraph = True, saveGraph = False):
   
   # New graph format
   filePrefix = "Compare{phystype}_Performance".\
        format(phystype=dataType)
   
   # DEBUG : Downsample input data
   downsample_factor = 10
   rawData, refPeaksTroughs, targetPeaksTroughs = \
       medianDownsampleRawDataPeaksTroughs(rawData, refPeaksTroughs, 
            targetPeaksTroughs, downsample_factor)
   samp_rate = samp_rate/downsample_factor
        
   # Ensure target peak/trough indices do not exceed the length of the raw data
   # (which is based on the reference data)
   targetPeaksTroughs = targetPeaksTroughs[targetPeaksTroughs<len(rawData)]
    
   tmp_x_rD = np.arange(len(rawData)) * samp_rate
   tmp_x_r  = np.arange(len(rawData))[refPeaksTroughs] * samp_rate
   tmp_x_t  = np.arange(len(rawData))[targetPeaksTroughs] * samp_rate

   tmp_y_r  = rawData[refPeaksTroughs]
   tmp_y_t  = rawData[targetPeaksTroughs]

   ret_plobj1 = lrp.RetroPlobj(tmp_x_rD, rawData, 
                               label='raw input data', 
                               alpha=1.0,
                               color='tab:orange')
   ret_plobj2 = lrp.RetroPlobj(tmp_x_r, tmp_y_r, 
                               label=dataType + ' reference',
                               ls='None', marker=7,
                               ms=5, mec='white', mew=0.02,
                               color='tab:red')
   ret_plobj3 = lrp.RetroPlobj(tmp_x_t, tmp_y_t, 
                               label=dataType + ' target',
                               ls='None', marker=7,
                               ms=3, mec='white', mew=0.02,
                               color='tab:blue')
   
   # plt.ion() This causes the fig to flash, when show fig false and is not 
   #    necessary to show fig

   oname = filePrefix + '.pdf'
   if OutDir :
        oname = OutDir + '/' + oname
   Title = 'Comparing ' + dataType
   # fff = lrp.RetroFig(figname=oname,
   #                     max_n_per_sub=min(1000, len(rawData)),
   #                     title=Title)
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=min(1000, len(rawData)),
                       fontsize = font_size,
                       title=Title)
   fff.add_plobj(ret_plobj1) # Number of subplots determined here
   fff.add_plobj(ret_plobj2)
   fff.add_plobj(ret_plobj3)
   fff.make_plot( do_show = showGraph, do_save = saveGraph)
    
# ================================ main =====================================

if __name__ == "__main__":


    # Read user-supplied arguments
    args_dict = lpo.main_option_processing( sys.argv )
    
    # Make output directory
    if args_dict['out_dir']:
        OutDir = args_dict['out_dir']
    else:
        now      = datetime.now() # current date and time
        OutDir = now.strftime("retro_%Y-%m-%d-%H-%M-%S")
    if os.path.exists(OutDir) and os.path.isdir(OutDir):
        shutil.rmtree(OutDir)
    os.mkdir(OutDir)        
    
    # Read input data
    inputData = readInputData(args_dict)
    
    # Display and save cardiac peaks against raw data
    dataType = 'CardiacPeaks'
    plotPeakTroughComparisons(dataType, inputData['cardiacRawData'], 
            inputData['cardiacPeaksRef'], inputData['cardiacPeaksTarget'], 
            args_dict['freq'], args_dict['font_size'], 
            saveGraph = args_dict['save_graph_level'])
    
    # Display and save respiratory peaks against raw data
    dataType = 'respiratoryPeaks'
    plotPeakTroughComparisons(dataType, inputData['respiratoryRawData'], 
            inputData['respiratoryPeaksRef'], inputData['respiratoryPeaksTarget'], 
            args_dict['freq'], args_dict['font_size'], 
            saveGraph = args_dict['save_graph_level'])
    
    # Display and save respiratory troughs against raw data
    dataType = 'respiratoryTroughs'
    plotPeakTroughComparisons(dataType, inputData['respiratoryRawData'], 
            inputData['respiratoryTroughsRef'], inputData['respiratoryTroughsTarget'], 
            args_dict['freq'], args_dict['font_size'], 
            saveGraph = args_dict['save_graph_level'])
    
    # End
    print('End')
    