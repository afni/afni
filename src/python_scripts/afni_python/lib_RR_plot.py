#!/usr/bin/env python

# a basic realtime_receiver plotting library

import sys, os

# verify system libraries
import module_test_lib
g_testlibs = ['gc', 'numpy', 'wx', 'matplotlib']
if module_test_lib.num_import_failures(g_testlibs,details=0):
   print """
     -- for details, consider xmat_tool -test_libs
     -- also, many computations do not require the GUI
        (e.g. 'xmat_tool -load_xmat X.xmat.1D -show_cormat_warnings')
   """
   sys.exit(1)

import numpy as N
import gc

# must use matplotlib with wx, not pylab
import wx
import matplotlib
matplotlib.use('WX')

# set some resource font values
matplotlib.rc('axes',titlesize=11)
matplotlib.rc('axes',labelsize=9)
matplotlib.rc('xtick',labelsize=8)
matplotlib.rc('ytick',labelsize=7)

from matplotlib.backends.backend_wx import FigureCanvasWx as FigureCanvas
from matplotlib.backends.backend_wx import NavigationToolbar2Wx
from matplotlib.figure import Figure

from matplotlib.ticker import FormatStrFormatter

import afni_util as UTIL

# ======================================================================
# general plotting routine:
# 

def plot(data, title='', verb=1):
   """plot data"""

   frame = CanvasFrame(title=title, verb=verb)

   if title == '': title = 'basic plot'
   frame.SetTitle(title)

   frame.Show(True)
   frame.plot_data(data)

   return 0, frame


# ======================================================================
# main plotting canvas class

class CanvasFrame(wx.Frame):
   """create a main plotting canvas
        title   : optional window title
        verb    : verbose level (default 1)
   """

   counter = 0
   def __init__(self, title='', verb=1):
      wx.Frame.__init__(self, None, -1, title, size=(400,300))
      self.verb   = verb
      self.figure = Figure()
      self.canvas = FigureCanvas(self, -1, self.figure)
      self.sizer = wx.BoxSizer(wx.VERTICAL)
      self.sizer.Add(self.canvas, 1, wx.LEFT | wx.TOP | wx.GROW)
      self.SetSizer(self.sizer)
      self.Fit()

      # toolbar?
      # self.toolbar = NavigationToolbar2Wx(self.canvas)
      # self.toolbar.Realize()
      # self.sizer.Add(self.toolbar, 0, wx.BOTTOM | wx.EXPAND)
      # self.toolbar.update()

      # axis plotting info
      self.ax     = None
      self.xmin   = 1.0
      self.xmax   = 0.0
      self.ymin   = 1.0
      self.ymax   = 0.0
      self.xlabel = ''
      self.ylabel = ''
      self.style  = 'graph'

      self.images = []

      self.toolbar = NavigationToolbar2Wx(self.canvas)
      self.toolbar.Realize()
      self.sizer.Add(self.toolbar, 0, wx.LEFT | wx.EXPAND)
      self.toolbar.update()

   def cb_keypress(self, event):
      if event.key == 'q':
         self.Close()

   def set_limits(self, xmin=1.0, xmax=0.0, ymin=1.0, ymax=0.0):
      """if xmin < xmax: apply, and similarly for y"""
      if xmin < xmax:
         self.xmin = xmin
         self.xmax = xmax
         if self.verb > 2: print '-- resetting xlimits to:', xmin, xmax
      if ymin < ymax:
         self.ymin = ymin
         self.ymax = ymax
         if self.verb > 2: print '-- resetting ylimits to:', ymin, ymax

   def plot_data(self, data, title=''):
      """plot data
         style can be 'graph' or 'bar'"""

      if self.ax == None:
         self.ax = self.figure.add_subplot(1,1,1,title=title)

      self.ax.clear()

      if self.style == 'graph':
         self.ax.plot(data)
         self.ax.grid(True)
      else:     # bars of width 1
         offsets = N.arange(len(data))
         bars = self.ax.bar(offsets, data, 1)

      self.ax.set_xlabel(self.xlabel)
      self.ax.set_ylabel(self.ylabel)

      # after data is plotted, set limits
      if self.xmin < self.xmax: self.ax.set_xlim((self.xmin, self.xmax))
      if self.ymin < self.ymax: self.ax.set_ylim((self.ymin, self.ymax))

      self.Fit()        # maybe not applied without a running app loop
      self.canvas.draw()

   def exit(self):
      self.Destroy()

