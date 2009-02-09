#!/usr/bin/env python

import sys, os

# verify system libraries
import module_test_lib
g_testlibs = ['os', 'gc', 'numpy', 'wx', 'matplotlib']
if module_test_lib.num_import_failures(g_testlibs,details=0):
   print """
     -- for details, consider xmat_tool -test_libs
     -- also, many computations do not require the GUI
        (e.g. 'xmat_tool -load_xmat X.xmat.1D -show_cormat_warnings')
   """
   sys.exit(1)

import numpy as N
import afni_xmat as AM
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
   """Convert data to an AfniMatrix and plot.

            data  : data to plot, one of:     o  AfniMatrix
                                              o  Numpy Matrix
                                              o  array (of arrays ?)
                                              o  1D filename
            title : optional window title
            verb  : verbose level

      return err, result
                err    : 0 = success, else failure
                result : CanvasFrame (if err == 0)
                         error message (otherwise)
   """

   if isinstance(data, AM.AfniXmat):
      amat = data
   elif isinstance(data, N.ndarray) or type(data) == type([]):
      amat = AM.AfniXmat('', from_mat=1, matrix=data, verb=verb)
   elif type(data) == type(''):
      amat = AM.AfniXmat(filename=data)
   else:
      mesg = "** plot: cannot convert %s to AfniXmat" % type(data)
      if verb > 1: print mesg
      return 1, mesg

   if not isinstance(amat, AM.AfniXmat):
      mesg = "** plot: failed to convert %s to AfniXmat" % type(data)
      if verb > 1: print mesg
      return 2, mesg

   # we have a valid AfniXmat, plot it

   frame = CanvasFrame(title=title, as_one=1, verb=verb)

   if title == '': title = 'AfniXmat'
   new_title = '%s   (%d)' % (title, frame.counter)
   frame.SetTitle(new_title)

   frame.Show(True)
   frame.plot_mat_by_cols(amat)

   return 0, frame


# ======================================================================
# main plotting canvas class

class CanvasFrame(wx.Frame):
   """create a main plotting canvas
        title   : optional window title
        as_one  : plot as one overlapping graph
                  (else a list of scaled graphs)
        verb    : verbose level (default 1)
   """

   counter = 0
   def __init__(self, title='', as_one=0, verb=1):
      wx.Frame.__init__(self, None, -1, title, size=(400,300))
      self.counter += 1
      self.verb   = verb
      self.figure = Figure()
      self.canvas = FigureCanvas(self, -1, self.figure)
      self.sizer = wx.BoxSizer(wx.VERTICAL)
      self.sizer.Add(self.canvas, 1, wx.LEFT | wx.TOP | wx.GROW)
      self.SetSizer(self.sizer)
      self.Fit()

      self.canvas.mpl_connect('key_press_event', self.cb_keypress)
      self.as_one  = as_one

      self.toolbar = NavigationToolbar2Wx(self.canvas)
      self.toolbar.Realize()
      self.sizer.Add(self.toolbar, 0, wx.LEFT | wx.EXPAND)
      self.toolbar.update()

   def cb_keypress(self, event):
      if event.key == 'q':
         self.Close()

   def plot_scaled_mat(self, amat, verb=1):
      """plot data scaled and shifted to fit on one graph

              data      - AfniMatrix, list of float lists, or 1D file
              title     - optional window title
              verb      - optional verbose level
      """

      # allow matlist, list of float lists, 
      # if type(matlist) == type([]):
      #     e0 = matlist[0]
      #     if type

      if not matlist: return
      nmats = len(matlist)
      if nmats < 1: return

      yformat = FormatStrFormatter('%5.1f')

   def plot_matlist(self, matlist, title='', ylabels=[], ftcolors=0, verb=1):
      """plot AfniMatrix list, one graph per AfniMatrix
              matlist   - list of AfniMatrix elements
              title     - optional window title
              ylabels   - optional list of ylabel per mat
              ftcolors  - flag: use fit/timeseries colors (black blue)
              verb      - optional verbose level
      """

      if not matlist: return
      nmats = len(matlist)
      if nmats < 1: return

      yformat = FormatStrFormatter('%5.1f')
      matplotlib.rcParams['lines.linewidth'] = 2

      # make a label list, and get max length (among any label[0])
      if ylabels: labels = ylabels
      else:       labels = []
      rlen = 0
      nruns = 0
      maxlen = 0
      for ind in range(nmats):
         if ylabels:                lab = ylabels[ind]
         else:
            if matlist[ind].labels: lab = matlist[ind].labels[0]
            else:                   lab = ''
            labels.append(lab)
         if len(lab) > maxlen: maxlen = len(lab)

         # note run info
         if nruns == 0 and matlist[ind].nruns > 1:
            nruns = matlist[ind].nruns
            rlen = matlist[ind].run_len

      for ind in range(nmats):
         amat = matlist[ind]

         if ind == 0: tstr = title
         else:        tstr = ''

         ax = self.figure.add_subplot(nmats,1,ind+1,title=tstr)
         #if ftcolors and amat.ncols == 2:   # use blue over black?
         #   ax.plot(amat.mat[:,0], 'k')
         #   ax.plot(amat.mat[:,1], 'b')
         ax.plot(amat.mat)

         # apply y tickmarks
         ymin, ymax = ax.get_ylim()
         width = ymax - ymin
         if ymin*ymax < 0.0: ymean = 0.0
         else: ymean = round((ymin+ymax)/2.0,2)

         ax.grid(True)

         ax.yaxis.set_major_formatter(yformat)

         # if there are many graphs, reduce the number of yticks
         if nmats > 10:  ax.set_yticks(N.array([ymin,ymax]))
         elif nmats > 2: ax.set_yticks(N.array([ymin,ymean,ymax]))

         if rlen > 0: ax.set_xticks(N.array([r*rlen for r in range(nruns+1)]))
         if ind < nmats - 1: ax.set_xticklabels([])
         else:               ax.set_xlabel('TRs')

         if labels:
            if nmats > 1:
               ax.set_ylabel('%-*s ' % (maxlen,labels[ind]),
                             rotation='horizontal')
               rv, plist = self.axis_posn(ax)
               if rv == 0: ax.set_position((0.15, plist[1], 0.7, 0.7/nmats))
            else:
               ax.set_ylabel('%s' % labels[ind])
               ax.set_position((0.15, 0.2, 0.7, 0.7))

      self.canvas.draw()

      matplotlib.rcParams['lines.linewidth'] = 1   # reset

   def axis_posn(self, ax):
      """return error code, posn array"""

      # first attempt is to look for simple array return
      try: posn = ax.get_position()
      except:
         if self.verb > 1: print '** failed ax.get_position()'
         return 1, None

      # have list, ready for some return
      if type(posn) == type([]):
         if len(posn) < 4:
            if self.verb > 1:
               print '** get_position returns len %d list' % len(posn)
            return 1, None
         if self.verb > 2: print '-- get_position returns list %s' % \
                         UTIL.float_list_string(posn)
         return 0, posn

      # no list, assume Bbox and expect get_points() to return 2x2 numpy array
      try: plist = posn.get_points()
      except:
         if self.verb > 1: print '** failed posn.get_points()'
         return 1, None

      if type(plist) != type(N.array([])):
         if self.verb > 1: print '** posn.get_points() does not return N.array?'
         return 1, None

      try: pnlist = [plist[0][0], plist[0][1], plist[1][0], plist[1][1]]
      except:
         if self.verb > 1: print '** bad plist shape %s' % plist.shape
         return 1, None

      if self.verb > 2: print '-- get_position returns Bbox list: %s' % \
                        UTIL.float_list_string(pnlist)

      return 0, pnlist

   def plot_mat_by_cols(self, amat, cols=[]):
      """plot the given columns from a single AfniMatrix"""

      if not isinstance(amat, AM.AfniXmat):
         print '** plot_MBC: instance is not AfniXmat'
         return

      if not cols:
         if amat.verb > 1: print '-- plotting default of %d cols' % amat.ncols
         cols = [i for i in range(amat.ncols)]

      ncols = len(cols)
      if ncols < 1: return

      # create reduced matrix and column labels
      mat = amat.mat[:,cols]

      yformat = FormatStrFormatter('%5.1f')
      if amat.labels:
         labels = N.array(amat.labels)
         labels = labels[cols]
         labels = labels.tolist()

         # create axis formatter string

         striplabs = 1
         for label in labels: # strip trailing '#' if indices are all 0
            ind = regress_index(label)
            if ind > 0: striplabs = 0

         maxlen = 0
         for l in range(len(labels)):
            label = labels[l]
            if striplabs:
               labels[l] = labels[l][0:labels[l].rfind('#')]
            if len(labels[l]) > maxlen: maxlen = len(labels[l])

      for i in range(ncols):
         if i == 0: title = '%s [%s]' % (os.path.basename(amat.fname),
                                         UTIL.encode_1D_ints(cols))
         else     : title = ''

         if self.as_one:
            # then only create one axis
            if i == 0: ax = self.figure.add_subplot(1,1,1,title=title)
         else:
            ax = self.figure.add_subplot(ncols,1,i+1,title=title)
         data = mat[:,i]
         ax.plot(data)

         # apply y tickmarks
         ymin, ymax = ax.get_ylim()
         width = ymax - ymin
         if ymin*ymax < 0.0: ymean = 0.0
         else: ymean = round((ymin+ymax)/2.0,2)

         ax.grid(True)

         ax.yaxis.set_major_formatter(yformat)
         amax = round(data.max(),1)

         if self.as_one:                   pass # use default yticks
         elif amax == 1.0 and ymin == 0.0: ax.set_yticks(N.array([ymin,amax]))
         elif ncols > 10:                  ax.set_yticks(N.array([ymin,ymax]))
         else:                       ax.set_yticks(N.array([ymin,ymean,ymax]))

         # now that yticks are set, prevent tight limits
         if not self.as_one:
            if ymin == data.min():
               ymin -= 0.15 * width
               ax.set_ylim((ymin, ymax))

            if ymax == data.max():
               ymax += 0.15 * width
               ax.set_ylim((ymin, ymax))

            if amat.run_len > 10 and amat.nruns > 1:
               ax.set_xticks(N.array([r*amat.run_len
                                      for r in range(amat.nruns+1)]))

         if i < ncols - 1: ax.set_xticklabels([])
         else:             ax.set_xlabel('TRs')

         if self.as_one: ax.set_ylabel('')
         elif amat.labels:
            ax.set_ylabel('%-*s ' % (maxlen,labels[i]), rotation='horizontal')
            rv, plist = self.axis_posn(ax)
            # rcr - fix
            #if rv == 0: ax.set_position((0.2, plist[1], plist[2], plist[3]))
            if rv == 0: ax.set_position((0.15, plist[1], 0.7, 0.7/ncols))

      self.Fit()
      self.canvas.draw()

def regress_index(label):
   """return integer after trailing '#' if found, else -1"""

   ind = label.rfind('#')
   if ind < 0: return -1

   try:    val = int(label[ind+1:-1])
   except: val = -1

   return val

