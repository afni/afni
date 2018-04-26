#!/usr/bin/env python

import sys

# verify system libraries
import module_test_lib
g_testlibs = ['wx']
if module_test_lib.num_import_failures(g_testlibs,details=0):
   print """
     -- for details, consider xmat_tool -test_libs
     -- also, many computations do not require the GUI
        (e.g. 'xmat_tool -load_xmat X.xmat.1D -show_cormat_warnings')
   """
   sys.exit(1)


import wx
import wx.grid


# ======================================================================
# text window combo class
class TextCombo(wx.Panel):
   def __init__(self, parent, id):
      wx.Panel.__init__(self, parent, id)

      # ----------------------------------------
      # create TextCtrl portion
      self.ctrl = wx.TextCtrl(self, -1, style = wx.TE_MULTILINE)

      # ----------------------------------------

      # add a horizontal line
      hline = wx.StaticLine(self, -1, (-1,-1), (-1,-1), wx.LI_HORIZONTAL)

      # fill a sizer with buttons
      
      hsizer = wx.BoxSizer(wx.HORIZONTAL)
      b1 = wx.Button(self, wx.NewId(), "&OK", (-1, -1), wx.DefaultSize)
      b2 = wx.Button(self, wx.NewId(), "&Cancel", (-1, -1), wx.DefaultSize)
      hsizer.Add(b1, 0)
      hsizer.Add(b2, 0, wx.LEFT, 10)

      # ----------------------------------------
      #  put it all in a vertical sizer

      vspace = 5
      vsizer = wx.BoxSizer(wx.VERTICAL)
      vsizer.Add(self.ctrl, 0, wx.GROW | wx.ALL, vspace)
      vsizer.Add(hline, 0, wx.GROW | wx.ALL, vspace)
      vsizer.Add(hsizer, 0, wx.ALIGN_RIGHT | wx.ALIGN_BOTTOM, vspace)

      self.SetSizerAndFit(vsizer)

# ======================================================================
# matrix grid (show matrix in a spreadsheet format)
#

class MatFrame(wx.Frame):
   def __init__(self, mat, title='', rlabels=[], rind=0, clabels=[],
                corr_colors=0):
      wx.Frame.__init__(self, None, -1, title, size=(700,500))
      self.grid = MatGrid(self, mat, rlabels, rind, clabels, corr_colors)

class MatGrid(wx.grid.Grid):
   def __init__(self, parent, mat, rlabels=[], rind=0,
                clabels=[], corr_colors=0):
      """display a Numpy matrix in a spreadsheet
            required parameters:
                parent      : parent widget
                mat         : Numpy matrix
            optional parameters:
                title       : window title
                rlabels     : row labels
                rind        : add indices to row labels
                clabels     : column labels
                corr_colors : display correlation matrix colors
      """

      wx.grid.Grid.__init__(self, parent, wx.ID_ANY)
      self.parent = parent

      # set rows and cols
      self.nrows, self.ncols = mat.shape
      self.CreateGrid(self.nrows, self.ncols)

      # general attributes
      self.attr = wx.grid.GridCellAttr()
      self.attr.SetTextColour('black')

      if rlabels:
         self.SetRowLabelAlignment(wx.ALIGN_LEFT, wx.ALIGN_CENTRE)
         if rind:
            for r in range(len(rlabels)):
               rlabels[r] = '%3d : %s' % (r, rlabels[r])
         lmax = max([len(name) for name in rlabels])
         self.SetRowLabelSize(10*lmax)
         for r in range(len(rlabels)):
            self.SetRowLabelValue(r, rlabels[r])

      if clabels:
         lmax = max([len(name) for name in clabels])
         if lmax < 6: lmax = 6
         for c in range(self.ncols):
            self.SetColSize(c, 10*lmax)
         for c in range(len(clabels)):
           self.SetColLabelValue(c, clabels[c])
      else:
         for c in range(self.ncols):
            self.SetColSize(c, 6*10)
            self.SetColLabelValue(c, str(c))

      self.fill_vals(mat, corr_colors)

   def fill_vals(self, mat, corr_colors=0):
      """fill values from Numpy matrix, color if corr_colors is set"""
      for row in range(self.nrows):
         self.SetRowAttr(row, self.attr)
         for col in range(self.ncols):
            val = mat[row,col]
            if corr_colors:  # show on lower triangle
               if row == col:
                  self.SetCellTextColour(row, col, 'blue')
               elif row > col:
                  self.SetCellTextColour(row, col, self.corr_color(val))
            self.SetCellValue(row, col, '%.3f' % val)
            self.SetReadOnly(row, col, True)
            self.SetCellAlignment(row, col, wx.ALIGN_RIGHT, wx.ALIGN_CENTRE)

   def corr_color(self, value):
      aval = abs(value)
      if aval < 0.4: return 'black'
      if aval < 0.6: return 'green'
      return 'red'
         
