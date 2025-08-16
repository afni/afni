#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 14 19:33:08 2023

@author: peterlauren
"""

import numpy as np
import matplotlib.pyplot as plt
import math

class CRF:

  def __init__(self):
      self.RRFvector = []

def makeCRF():
    CRFvector = [0.6*(t**2.7) * np.exp(-t/1.6) - 16*((1/math.sqrt(18*math.pi))*np.exp(-((t-12)**2)/18))  for t in range(0,30)]

    plt.plot(CRFvector, color='red')
    plt.xlabel("Time (s)")
    plt.ylabel("CRF")
    # plt.show()
    
    plt.savefig('/home/peterlauren/retroicor/RVTRRF/FigCRF.pdf') 
    plt.show(block=True)
    
    return CRFvector

# CRF = [0.6*(t**2.7) * np.exp(-t/1.6) - 16*((1/math.sqrt(18*math.pi))*np.exp(-((t-12)**2)/18))  for t in range(0,30)]
# RRF = [0.6*t**2.1*np.exp((-t/1.6)) - 0.0023*t**3.54*np.exp((-t/4.25)) for t in range(0,50)]

