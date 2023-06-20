#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 16 13:05:57 2023

@author: peterlauren

"""

import numpy as np
import matplotlib.pyplot as plt

def plotPeaks(rawData, peaks, OutDir, filePrefix, Title, test_retro_obj, lrp):
   tmp_x_rD = np.arange(len(rawData)) * test_retro_obj.card_data.samp_rate
   tmp_x_p  = np.arange(len(rawData))[peaks] * test_retro_obj.card_data.samp_rate

   tmp_y_p  = rawData[peaks]

   ret_plobj1 = lrp.RetroPlobj(tmp_x_rD, rawData, 
                               label='raw input data', 
                               alpha=1.0,
                               color='tab:orange')
   ret_plobj2 = lrp.RetroPlobj(tmp_x_p, tmp_y_p, 
                               label='cardiac peaks',
                               ls='None', marker=7,
                               ms=4, mec='white', mew=0.02,
                               color='tab:blue')
   
   # plt.ion() This causes the fig to flash, when show fig false and is not 
   #    necessary to show fig

   oname = 'AAAAAAAA' + filePrefix + '.pdf'
   if OutDir :
        oname = OutDir + '/' + oname
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=5000, 
                       title=Title)
   fff = lrp.RetroFig(figname=oname,
                       max_n_per_sub=5000, 
                       fontsize = test_retro_obj.font_size,
                       title='Respiratory peaks and troughs')
   fff.add_plobj(ret_plobj1)
   fff.add_plobj(ret_plobj2)
   fff.make_plot( do_show = test_retro_obj.show_graph_level > 0,
                   do_save = test_retro_obj.save_graph_level > 0)
    
   plt.close() # Close empty figure window
            