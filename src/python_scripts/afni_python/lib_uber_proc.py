#!/usr/bin/env python

# python3 status: compatible

# general functions for use by uber_tool*.py

import sys, os

g_history = """
  uber_proc.py history

    0.0  14 Feb, 2011: initial revision (just a skeleton)
    0.1  10 Dec, 2019: separate to allow -help without PyQt4

         ***  This is not really even started.  ***
"""

g_version = '0.1 (December 10, 2017)'

# ----------------------------------------------------------------------
# global definitions

g_help_string = """
help for uber_proc.py           - still a lot to do

1. Run from the directory where you would like the results to be placed.
   By default, a new 'uber_results' directory will be placed there, under
   which might be 'subjects', 'group_results', etc.

2. Panic into error.
"""


# ----------------------------------------------------------------------
# common functions

def directory_exists(dirname):
   """return whether dirname exists as a directory"""
   return 0

def in_home_dir():
   """return whether we are in the user's home dir, according to $HOME"""
   homedir = os.getenv('HOME')
   if homedir == None: return 0
   if os.getcwd() == homedir: return 1
   return 0

