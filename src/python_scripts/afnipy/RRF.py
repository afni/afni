#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 14 19:33:08 2023

@author: peterlauren
"""

import numpy as np
import matplotlib.pyplot as plt

class RRF:

  def __init__(self):
      self.RRFvector = []

def makeRRF():
    RRFvector = [0.6*t**2.1*np.exp((-t/1.6)) - 0.0023*t**3.54*np.exp((-t/4.25)) for t in range(0,50)]
    
    plt.plot(RRFvector, color='red')
    plt.xlabel("Time (s)")
    plt.ylabel("RRF")
    # plt.show()
    
    plt.savefig('/home/peterlauren/retroicor/RVTRRF/RRF.pdf') 
    plt.show(block=True)
    
    return RRFvector

