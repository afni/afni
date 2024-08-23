#!/usr/bin/env python

########################################################################
## 08/2024 Justin Rajendra
## library of functions for disco

import sys
import os
import glob
import subprocess
import csv
import re
import shutil
import argparse
import signal
import textwrap
import random
import time
import copy

## stolen from the interwebs
def progressbar(it, prefix="", size=60, out=sys.stdout): # Python3.6+
    '''Create a progress bar with a label based on the number of loops.
    https://stackoverflow.com/questions/3160699/python-progress-bar'''
    count = len(it)
    start = time.time() # time estimate start
    def show(j):
        x = int(size*j/count)
        # time estimate calculation and string
        remaining = ((time.time() - start) / j) * (count - j)
        mins, sec = divmod(remaining, 60) # limited to minutes
        time_str = f"{int(mins):02}:{sec:03.1f}"
        print(
            f"{prefix}[{u'█'*x}{('.'*(size-x))}] {j}/{count} Est wait {time_str}",
            end='\r', file=out, flush=True)
    show(0.1) # avoid div/0 
    for i, item in enumerate(it):
        yield item
        show(i+1)
    print("\n", flush=True, file=out)

## calculate bar length for the progressbar function
def prog_bar_len(message,num_loops):
    '''Figure out the progress bar length given the message length, the 
    number of loops and fixed values from the progressbar function.'''
    window_width = 80
    pb_len = window_width - len(message) - (len(str(num_loops))*2) - 4 - 17
    if pb_len < 10:
        pb_len = 10
    return(pb_len)

## AFNI formatted error message
def print_afni_error(message,message2=False):
    '''Print out an error message in the AFNI format. Can have an optional
    second line'''
    print('\n** ERROR: '+message+' !!!')
    if message2:
        print('          '+message2+'\n')
    else:
        print()

## check if a file exists on disk
def check_overwrite(file_name, overwrite=False):
    '''Inputs are a string for the file name with path and a logical for
     the status of -overwrite. Returns True if error and prints out a 
     message.'''
    if os.path.exists(file_name) and not overwrite:
        print_afni_error(file_name+' exists. NOT OVERWRITING',
                         'Try -overwrite if available.')
        return(1)
    else:
        return(0)
    

