#!/usr/bin/env python

import sys

# verify system libraries
import module_test_lib
g_testlibs = ['os', 'gc', 'numpy', 'wx', 'matplotlib']
if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)

import os, gc

import numpy as N

import wx
import wx.grid

# must use matplotlib with wx, not pylab
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
import afni_xmat as AM
import ui_xmat as UIX

# ----------------------------------------------------------------------
# globals

# --------------------------------------------------
# global widget IDs
ID_CLOSE            = 111
ID_EXIT             = 112
ID_ABOUT            = 121
ID_HELP             = 122
ID_HELP_CMD         = 123
ID_HIST             = 131
ID_SAVE             = 151

ID_LOAD_XMAT        = 201
ID_LOAD_1D          = 202

ID_SHOW_XMAT        = 301
ID_SHOW_XMAT_CONDS  = 302
ID_SHOW_CORMAT      = 303
ID_SHOW_CORMAT_EVIL = 304
ID_SHOW_1D          = 321
ID_SHOW_FIT_BETAS   = 331

ID_PLOT_XMAT        = 401
ID_PLOT_1D          = 402
ID_PLOT_BEST_FIT    = 403
ID_PLOT_CORMAT      = 411

ID_APPLY_CHOICE     = 501

ID_PLOT_AS_ONE      = 1001      # check item in menu

# --------------------------------------------------
# values for global text display list
gSTR_BLANK              = ''

gSTR_1D_FILE            = ' 1D timeseries file: '
gSTR_XMAT_FILE          = ' X-matrix file: '
gSTR_COLS_HDR           = '   -- COLUMNS (regressors) by type-- '
gSTR_COLS_CHOSEN        = '   cols of chosen regressors: '
gSTR_COLS_ALL           = '   cols of ALL regressors: '
gSTR_COLS_MAIN          = '   cols of main regressors: '
gSTR_COLS_BASE          = '   cols of baseline regressors: '
gSTR_COLS_MOTION        = '   cols of motion regressors: '
gSTR_COND_HDR           = '   -- CONDITION NUMBERS -- '
gSTR_COND_CHOSEN        = '   cond of chosen columns: '
gSTR_COND_ALL           = '   cond of all regressors: '
gSTR_COND_MAIN          = '   cond of main regressors: '
gSTR_COND_M_BASE        = '   cond of main+baseline: '
gSTR_COND_M_MOT         = '   cond of main+motion: '
gSTR_COND_BASE_MOT      = '   cond of baseline+motion: '

g_textnames = [ 
        gSTR_1D_FILE,                                           gSTR_BLANK,
        gSTR_XMAT_FILE,                                         gSTR_BLANK,
        gSTR_COLS_HDR, gSTR_COLS_CHOSEN, gSTR_COLS_ALL,
        gSTR_COLS_MAIN,                                         gSTR_BLANK,
        gSTR_COLS_BASE, gSTR_COLS_MOTION,                       gSTR_BLANK,
        gSTR_COND_HDR, gSTR_COND_CHOSEN, gSTR_COND_ALL,
        gSTR_COND_MAIN,                                         gSTR_BLANK,
        gSTR_COND_M_BASE, gSTR_COND_M_MOT, gSTR_COND_BASE_MOT ]


# global text display list
g_textlist = [
   # leading string, flag (takes value), value string, call-back function
   [gSTR_1D_FILE,       1, '', None], [gSTR_BLANK,         1, '', None],
   [gSTR_XMAT_FILE,     1, '', None], [gSTR_BLANK,         1, '', None],
   [gSTR_COLS_HDR,      0, '', None],
   [gSTR_COLS_CHOSEN,   1, '', None],
   [gSTR_COLS_ALL,      1, '', None],
   [gSTR_COLS_MAIN,     1, '', None], [gSTR_BLANK,         1, '', None],
   [gSTR_COLS_BASE,     1, '', None],
   [gSTR_COLS_MOTION,   1, '', None], [gSTR_BLANK,         1, '', None],
   [gSTR_COND_HDR,      0, '', None],
   [gSTR_COND_CHOSEN,   1, '', None],
   [gSTR_COND_ALL,      1, '', None],
   [gSTR_COND_MAIN,     1, '', None], [gSTR_BLANK,         1, '', None],
   [gSTR_COND_M_BASE,   1, '', None],
   [gSTR_COND_M_MOT,    1, '', None],
   [gSTR_COND_BASE_MOT, 1, '', None]
]

# ======================================================================
# main graphical user interface class
# 
# Most call-back (cb_*) functions immediately call GUI (gui_*) function
# to do the work.  The GUI functions do not require an event parameter,
# and can therefore be called any time, once the main XmatGUI is running,
# and not just from user call-backs.

class XmatGUI(wx.App):
   def OnInit(self):
      return True

   def init_gui(self, XM=None, verb=1):
      """init the gui, possibly given XmatInterface and verb values
     
         Note that the top-level GUI will contain nothing but the MainFrame,
         which will store the GUI and XmatInterface data."""

      if not XM: XM = UIX.XmatInterface(verb)

      self.Gframe = MainFrame(None, -1, "xmat_tool.py", xinter=XM)
      self.Gframe.Show(True)
      self.SetTopWindow(self.Gframe)

class MainFrame(wx.Frame):
   """GUI information will be stored in this MainFrame, while user
      information will be stored in the (contained) XmatInterface, XM."""

   def __init__(self, parent, ID, title, xinter=None):
      """initialize with XmatInterface """
      wx.Frame.__init__(self, parent, ID, title, wx.DefaultPosition, (500,400))

      if not xinter:
         print '** cannot init MainFrame without XmatInterface'
         return False

      # init all local variables
      self.XM = xinter                  # XmatInterface
      self.help_frame     = None        # Help window
      self.hist_frame     = None        # History window
      self.plotx_frame    = None
      self.plot_as_one    = 0
      self.plotx_cols     = []
      self.plot1D_frame   = None
      self.plot1D_redo    = 0           # do we need to re-create the plot
      self.plotfit_frame  = None
      self.cormat_grid    = None

      self.fixed_font     = None        # facename of fixed-width font
      self.textwin_size   = None        # compute default size for text window

      self.choicectrl     = None        # column choice textctrl

      self.CreateStatusBar()

      self.add_menubar()
      self.assign_fixed_font()
      self.add_text_widgets()

      self.Bind(wx.EVT_CLOSE, self.cb_exit)

      if self.XM.verb > 2: print '-- MainFrame initialized'

   # ------------------------------------------------------------
   # fill the main frame with descriptive text widgets
   #
   # column choice line, and then an array of strings and values
   def add_text_widgets(self):

      panel1 = wx.Panel(self, -1)
      st = wx.StaticText(panel1, -1, '  Choose Matrix Columns : ')
      self.choicectrl = wx.TextCtrl(panel1, -1, size=(160,-1))
      button = wx.Button(panel1, ID_APPLY_CHOICE, 'apply')
      button.SetForegroundColour('red')
      panel1.Bind(wx.EVT_BUTTON, self.cb_apply_col_choice, id=ID_APPLY_CHOICE)
      
      box = wx.BoxSizer(wx.HORIZONTAL)
      box.Add(st, 1, wx.CENTRE)
      box.Add(self.choicectrl, 1, wx.CENTRE)
      box.Add(button, wx.CENTRE)
      panel1.SetSizer(box)
      panel1.Refresh()
      
      # make GridSizer for the rest of the table
      panel2 = wx.Panel(self, -1)
      self.tw_stlist = []
      tlen = len(g_textlist)
      if tlen != len(g_textnames): # be sure
         print '*** textlist lengths do not match'
         sys.exit(1)

      gs = wx.GridSizer(tlen, 2, 5, 5)
      for data in g_textlist:
         # add label
         st = wx.StaticText(panel2, -1, data[0])
         st.SetFont(self.fixed_font)
         gs.Add(st, flag=wx.EXPAND, border=4)

         # add and store data section
         st = wx.StaticText(panel2, -1, '')
         self.tw_stlist.append(st)
         gs.Add(st, flag=wx.EXPAND, border=4)
      panel2.SetSizer(gs)

      # and fill in sizer
      sizer = wx.BoxSizer(wx.VERTICAL)
      sizer.AddSpacer((20,20))
      sizer.Add(panel1, 0)
      sizer.AddSpacer((20,20))
      sizer.Add(panel2, 0)
      self.SetSizerAndFit(sizer)

      # per line
      # self.CreateStatusBar(1)
     

   # ------------------------------------------------------------
   # apply user-specified options
   def apply_options(self):
      """call this to update GUI from UIX data"""

      for opt in self.XM.gui_opts:
         if opt.name == '-gui_plot_xmat_as_one':
            self.plot_as_one = 1
         else:
            print "** unknown -gui option '%s'" % opt.name

      # in case either has previousely been set...
      self.update_textlist_from_xmat()
      self.update_textlist_from_1D()
      self.check_plot_as_one.Check(self.plot_as_one)

   # ------------------------------------------------------------
   # main callback to apply new choice of columns
   def cb_apply_col_choice(self, event):

      ret = self.XM.set_cols_from_string(self.choicectrl.GetValue())
      if ret == None:
         self.update_textlist_from_xmat()
      else:
         self.popup_warning(ret)

      return

   # ------------------------------------------------------------
   # convert system font to fixed-width, and save as self.fixed_font
   def assign_fixed_font(self):
      font = wx.SystemSettings_GetFont(wx.SYS_SYSTEM_FONT)
      self.set_fixed_width_font(font) # and set

   # ------------------------------------------------------------
   # menu bar
   def add_menubar(self):
      menubar = wx.MenuBar()
      # File menu
      menu = wx.Menu()
      menu.Append(ID_LOAD_XMAT, "load X &matrix", "load X matrix from file")
      menu.Append(ID_LOAD_1D, "load &1D file", "load 1d time series")
      menu.Append(ID_EXIT, "E&xit", "terminate program")
      menubar.Append(menu, "&File")
      # Show menu
      menu = wx.Menu()
      menu.Append(ID_SHOW_XMAT, "Show X&mat", "Show info about Xmat")
      menu.Append(ID_SHOW_XMAT_CONDS,
                  "Show Xmat co&nds", "Show Xmat condition numbers")
      menu.Append(ID_SHOW_CORMAT, "Show X-&Cormat",
                                  "Show correlation matrix (of X-matrix)")
      menu.Append(ID_SHOW_CORMAT_EVIL, "Show X-Cormat &Warnings",
                         "Show problematic entries in correlation matrix")
      menu.Append(ID_SHOW_1D, "Show &1D", "Show info about time series")
      menu.Append(ID_SHOW_FIT_BETAS, "Show &Fit Betas",
                        "Show info about the Betas of the fitts")
      menubar.Append(menu, "&Show")
      # Plot menu
      menu = wx.Menu()
      menu.Append(ID_PLOT_XMAT, "Plot X&mat", "Graph selected X matrix columns")
      self.check_plot_as_one =  \
           menu.Append(ID_PLOT_AS_ONE, "   plot Xmat as &one",
                  help='plot all regressors in one graph', kind=wx.ITEM_CHECK)
      # maintain the check box in case this is updated externally
      self.check_plot_as_one.Check(self.plot_as_one)
      menu.Append(ID_PLOT_1D, "Plot &1D", "Graph 1D time series")
      menu.Append(ID_PLOT_BEST_FIT, "Plot Best &Fit",
            "Graph best fit of selected X matrix columns against 1D array")
      menu.Append(ID_PLOT_CORMAT, "Graph &Corr Mat", "Graph Correlation Matrix")
      menubar.Append(menu, "&Plot")
      # Help menu
      menu = wx.Menu()
      menu.Append(ID_ABOUT, "&About", "information about this program")
      menu.Append(ID_HELP, "Help (&GUI)", "help using graphical user interface")
      menu.Append(ID_HELP_CMD, "Help (&Command-line)", "help for command-line")
      menu.Append(ID_HIST, "H&istory", "program history")
      menubar.Append(menu, "&Help")

      self.SetMenuBar(menubar)

      # ------------------------------
      # set menu call-backs
      wx.EVT_MENU(self, ID_EXIT, self.cb_exit)
      wx.EVT_MENU(self, ID_ABOUT, self.cb_about)
      wx.EVT_MENU(self, ID_HELP, self.cb_help)
      wx.EVT_MENU(self, ID_HELP_CMD, self.cb_help_cmd)
      wx.EVT_MENU(self, ID_HIST, self.cb_history)

      wx.EVT_MENU(self, ID_LOAD_XMAT, self.cb_load_mat)
      wx.EVT_MENU(self, ID_LOAD_1D, self.cb_load_mat)

      wx.EVT_MENU(self, ID_SHOW_XMAT, self.cb_show_mat)
      wx.EVT_MENU(self, ID_SHOW_XMAT_CONDS, self.cb_show_xmat_conds)
      wx.EVT_MENU(self, ID_SHOW_CORMAT, self.cb_show_xmat_cormat)
      wx.EVT_MENU(self, ID_SHOW_CORMAT_EVIL, self.cb_show_xmat_cormat_warns)
      wx.EVT_MENU(self, ID_SHOW_1D, self.cb_show_mat)
      wx.EVT_MENU(self, ID_SHOW_FIT_BETAS, self.cb_show_fit_betas)

      wx.EVT_MENU(self, ID_PLOT_XMAT, self.cb_plot_mat)
      wx.EVT_MENU(self, ID_PLOT_AS_ONE, self.cb_plot_as_one)
      wx.EVT_MENU(self, ID_PLOT_1D, self.cb_plot_1D)
      wx.EVT_MENU(self, ID_PLOT_BEST_FIT, self.cb_plot_fit)
      wx.EVT_MENU(self, ID_PLOT_CORMAT, self.cb_graph_corr_mat)

   # ----------------------------------------------------------------------
   # In general, the cb_ functions are the callbacks that call the respective
   # action functions.  Those action functions, which depend only on the main
   # class (and not the event, say) are driving functions that can be called
   # from elsewhere.  For example, to close a window, plot, or possibly even
   # drive the program from the command line (not sure if I'll do that).

   def cb_show_mat(self, event):        # gui event

      id = event.GetId() # xmat or 1D
      if id == ID_SHOW_XMAT:
         self.gui_show_xmat()
      elif id == ID_SHOW_1D:
         self.gui_show_1D()

   def gui_show_xmat(self):
      """create a text window containing show() text for the X matrix"""
      if self.XM.matX:
         mstr = self.XM.matX.make_show_str()
         title = os.path.basename(self.XM.fname_mat)
      else:
         self.popup_warning("please load an X matrix, first")
         return
      self.show_text_window(mstr, title)

   def gui_show_1D(self):
      """create a text window containing show() text for the 1D time series"""
      if self.XM.mat1D:
         mstr = self.XM.mat1D.make_show_str()
         title = os.path.basename(self.XM.fname_1D)
      else:
         self.popup_warning("please load a 1D time series, first")
         return
      self.show_text_window(mstr, title)

   def cb_show_fit_betas(self, event):
      self.gui_show_fit_betas()

   def gui_show_fit_betas(self):
      """create a text window describing the fit betas"""

      clist = self.XM.col_list
      if not clist: clist = range(len(self.XM.col_list))

      # ignore failure, to show error string in text window
      rv, rstr = self.XM.make_matrix_fit_string(clist)
      title = 'Matrix to 1D Fit Betas'
      self.show_text_window(rstr, title)

   def cb_show_xmat_cormat(self, event):
      self.gui_show_xmat_cormat()

   def gui_show_xmat_cormat(self):

      mat = self.XM.matX

      if not mat:
         self.popup_warning("please load an X matrix, first")
         return
      elif not mat.ready:
         return

      if not mat.cormat_ready:    # then set it
         mat.set_cormat()

      # mstr = 'Correlation Matrix:\n\n%s\n' % str(mat.cormat)
      rv, mstr = self.XM.make_cormat_string()

      title = os.path.basename(self.XM.fname_mat) + ' correlation matrix'

      self.show_text_window(mstr, title, wrap=0)

   def cb_show_xmat_cormat_warns(self, event):
      self.gui_show_xmat_cormat_warns()

   def gui_show_xmat_cormat_warns(self):

      mat = self.XM.matX

      if not mat:
         self.popup_warning("please load an X matrix, first")
         return
      elif not mat.ready:
         return

      rv, mstr = self.XM.make_cormat_warnings_string()

      title = os.path.basename(self.XM.fname_mat) + ' correlation warnings'

      self.show_text_window(mstr, title, wrap=0)

   def cb_show_xmat_conds(self, event): # gui event
      self.gui_show_xmat_conds()

   def gui_show_xmat_conds(self):

      mat = self.XM.matX

      if not mat:
         self.popup_warning("please load an X matrix, first")
         return

      mstr = mat.make_show_conds_str()

      title = os.path.basename(self.XM.fname_mat) + ' condition numbers'

      self.show_text_window(mstr, title)

   def cb_graph_corr_mat(self, event):        # gui event
      self.gui_graph_corr_mat()

   def gui_graph_corr_mat(self):

      mat = self.XM.matX

      if not mat:
         self.popup_warning("please load an X matrix, first")
         return

      if self.cormat_grid :
         self.cormat_grid.Raise()
         return

      if not mat.cormat_ready: mat.set_cormat() # initialize?

      if self.XM.verb > 1:
         print '++ showing cormat, size = %s' % str(mat.cormat.shape)

      self.cormat_grid = MatFrame(mat.cormat,'correlation matrix',
                                  rlabels=mat.labels, rind=1, corr_colors=1)
      self.cormat_grid.Bind(wx.EVT_CLOSE, self.cb_close_grid)

      self.cormat_grid.Show(True)

   def cb_close_grid(self, event):
      if self.cormat_grid:
         if self.XM.verb > 2: print '-- destroying cormat_grid'
         self.cormat_grid.Destroy()
         self.cormat_grid = None
         self.XM.cleanup_memory()

   def cb_plot_as_one(self, event):        # gui event
      self.plot_as_one = 1 - self.plot_as_one
      if self.XM.verb > 1:
         print '++ toggling plot_as_one to %d' % self.plot_as_one

   def cb_plot_mat(self, event):        # gui event
      self.gui_plot_xmat()

   def gui_plot_xmat(self):

      if not self.XM.matX:
         self.popup_warning("please load an X matrix, first")
         return

      cols = self.XM.col_list

      if not UTIL.lists_are_same(self.plotx_cols, cols):
         self.gui_close_plot()    # destroy old one
         
      elif self.plotx_frame:      # otherwise, if already open, just raise
         self.plotx_frame.Raise()
         return

      if self.XM.verb > 1: print '++ showing xmat with cols : %s' % str(cols)

      if len(cols) < 1:
         self.popup_warning("no columns chosen for X matrix")
         return

      self.plotx_frame = CanvasFrame(as_one=self.plot_as_one)
      self.plotx_cols  = cols           # keep track of what was used

      self.plotx_frame.Show(True)

      self.plotx_frame.plot_mat_by_cols(self.XM.matX, cols)

      self.plotx_frame.Bind(wx.EVT_CLOSE, self.cb_close_plot)

      if self.XM.verb > 1: print '-- xmat done'

   def cb_plot_fit(self, event):
      self.gui_plot_fit()

   def gui_plot_fit(self):

      if not self.XM.matX:
         self.popup_warning("please load an Xmatrix, first")
         return

      if not self.XM.mat1D:
         self.popup_warning("please load a 1D timeseries, first")
         return

      self.gui_close_plot_fit() # nuke any existing frame, regardless
         
      rv, mesg = self.XM.fit_xmat_to_1D(self.XM.col_list)
      if rv:
         self.popup_warning("X-matrix fitting failed\n\n(%s)" % mesg)
         return

      bothmats = self.XM.mat1D.copy()
      bothmats.nruns = self.XM.matX.nruns
      bothmats.run_len = self.XM.matX.run_len
      rv = bothmats.append([self.XM.matfit])
      if rv:
         self.popup_warning("X-matrix appending failed")
         return

      if self.XM.verb > 1: print '++ showing fit plot'

      self.plotfit_frame = CanvasFrame()
      self.plotfit_redo = 0

      self.plotfit_frame.Show(True)

      self.plotfit_frame.plot_matlist([bothmats], 'time series and fit',
                                      ftcolors=1)
      del(bothmats)

      self.plotfit_frame.Bind(wx.EVT_CLOSE, self.cb_close_plot_fit)

      if self.XM.verb > 1: print '-- plot fit done'

   def cb_plot_1D(self, event):
      self.gui_plot_1D()

   def gui_plot_1D(self):

      if not self.XM.mat1D:
         self.popup_warning("please load a 1D timeseries, first")
         return

      if self.plot1D_redo:          # need update, so destroy old old
         self.gui_close_plot_1D()
         
      elif self.plot1D_frame:     # otherwise, if already open, just raise
         self.plot1D_frame.Raise()
         return

      if self.XM.verb > 1: print '++ showing 1D plot'

      self.plot1D_frame = CanvasFrame()
      self.plot1D_redo = 0

      self.plot1D_frame.Show(True)

      self.plot1D_frame.plot_mat_by_cols(self.XM.mat1D, [0])

      self.plot1D_frame.Bind(wx.EVT_CLOSE, self.cb_close_plot_1D)

      if self.XM.verb > 1: print '-- plot 1D done'

   def cb_load_mat(self, event):        # gui event
      fname = wx.FileSelector(default_path=os.getcwd(),default_extension='1D',
                                                       wildcard='*.1D')
      if fname == '': return

      id = event.GetId()
      if id == ID_LOAD_XMAT:
         self.gui_load_xmat(fname)
      else:
         self.gui_load_1D(fname)

   def gui_load_xmat(self, fname):
      if self.XM.verb > 2:
         print '-- gui_load_xmat, file = %s' % fname

      rv = self.XM.set_xmat(fname)

      if rv or not self.XM.matX:  # if failure
         self.popup_warning("failed to load X matrix from '%s'" % fname)
         return

      # fill screen display data for this matrix
      self.update_textlist_from_xmat()

      badlist = self.XM.matX.list_cormat_warnings(cutoff=1.0)
      if badlist:
         self.popup_warning("duplicate regressors found in matrix\n\n"  \
                         "'Show: X-Cormat Warnings' for complete list")
         del(badlist)

   def gui_load_1D(self, fname):
      if self.XM.verb > 2:
         print '-- gui_load_xmat, file = %s' % fname

      rv = self.XM.set_1D(fname)

      if rv or not self.XM.mat1D:
         self.popup_warning("failed to load 1D file '%s'" % fname)
         return

      self.plot1D_redo = 1      # might need to redo the plot window

      # fill screen display data for the choice
      self.update_textlist_from_1D()

   def update_textlist_from_1D(self):
      """given the 1D file, fill screen data list"""
      if not self.XM.mat1D: return

      mat = self.XM.mat1D

      ind = self.textlist_index(gSTR_1D_FILE)
      if ind >= 0: self.tw_stlist[ind].SetLabel(os.path.basename(mat.fname))


   def update_textlist_from_xmat(self):
      """given the X matrix and columns, fill screen data list"""
      if not self.XM.matX: return

      matx = self.XM.matX

      ccols = self.XM.col_list
      if self.XM.verb > 1: print '++ updating text for columns: %s' % ccols

      ind = self.textlist_index(gSTR_XMAT_FILE)
      if ind >= 0: self.tw_stlist[ind].SetLabel(os.path.basename(matx.fname))

      # show columns

      self.set_textlist_label(self.textlist_index(gSTR_COLS_CHOSEN),
                              str=UTIL.encode_1D_ints(ccols))

      self.set_textlist_label(self.textlist_index(gSTR_COLS_ALL),
                              str=UTIL.encode_1D_ints(range(matx.ncols)))

      self.set_textlist_label(self.textlist_index(gSTR_COLS_MAIN),
                  str=UTIL.encode_1D_ints(matx.cols_by_group_list([],allroi=1)))

      self.set_textlist_label(self.textlist_index(gSTR_COLS_BASE),
                  str=UTIL.encode_1D_ints(matx.cols_by_group_list([-1])))

      self.set_textlist_label(self.textlist_index(gSTR_COLS_MOTION),
                  str=UTIL.encode_1D_ints(matx.cols_by_group_list([0])))

      # show select condition numbers

      cond = matx.cond_num_by_cols(ccols)
      self.set_textlist_label(self.textlist_index(gSTR_COND_CHOSEN), val=cond)

      cond = matx.cond_num_by_cols(range(matx.ncols))
      self.set_textlist_label(self.textlist_index(gSTR_COND_ALL), val=cond)

      cond = matx.cond_num_by_cols(matx.cols_by_group_list([],allroi=1))
      self.set_textlist_label(self.textlist_index(gSTR_COND_MAIN), val=cond)

      cond = matx.cond_num_by_cols(matx.cols_by_group_list([-1],allroi=1))
      self.set_textlist_label(self.textlist_index(gSTR_COND_M_BASE), val=cond)

      cond = matx.cond_num_by_cols(matx.cols_by_group_list([0],allroi=1))
      self.set_textlist_label(self.textlist_index(gSTR_COND_M_MOT), val=cond)

      cond = matx.cond_num_by_cols(matx.cols_by_group_list([-1,0]))
      self.set_textlist_label(self.textlist_index(gSTR_COND_BASE_MOT), val=cond)

   def set_textlist_label(self, ind, str='', val=-666.6, vlimit=666):
      """set the Label for the given index to the string or condition number
            ind         : index int tw_stlist
            str         : string to set
            val         : float value to fill with
            vlimit      : if val > vlimit, color in red
         If str='' and val=-666.6, then clear the label."""

      if ind < 0 or ind > len(self.tw_stlist):
         if self.XM.verb > 1: print '** STL: index %d out of range' % ind
         return

      if val != -666.6:
         if val > 10e6: self.tw_stlist[ind].SetLabel('infinite?')
         else:          self.tw_stlist[ind].SetLabel('%.1f' % val)
         if val > vlimit: self.tw_stlist[ind].SetForegroundColour('red')
         else:            self.tw_stlist[ind].SetForegroundColour('black')
      else: self.tw_stlist[ind].SetLabel(str)

   def textlist_index(self, name):
      """return the index of g_textlist for the given name (-1 on failure)"""
      for ind in range(len(g_textlist)):
         if g_textlist[ind][0] == name: return ind
      return -1

   def cb_close_plot(self, event):
      self.gui_close_plot()

   def gui_close_plot(self):
      if self.plotx_frame:
         if self.XM.verb > 2: print '-- destroying plotx'
         self.plotx_frame.Destroy()
         self.plotx_frame = None
         del(self.plotx_cols)
         self.plotx_cols = []
         self.XM.cleanup_memory()

   def cb_close_plot_1D(self, event):
      self.gui_close_plot_1D()

   def gui_close_plot_1D(self):
      if self.plot1D_frame:
         if self.XM.verb > 2: print '-- destroying plot1D'
         self.plot1D_frame.Destroy()
         self.plot1D_frame = None
         self.plot1D_redo = 0

   def cb_close_plot_fit(self, event):
      self.gui_close_plot_fit()

   def gui_close_plot_fit(self):
      if self.plotfit_frame:
         if self.XM.verb > 2: print '-- destroying plotfit'
         self.plotfit_frame.Destroy()
         self.plotfit_frame = None

   def cb_exit(self, event):
      if self.XM.verb > 1: print 'exiting GUI...'
      if self.plotx_frame:
         self.cb_close_plot(1)
      self.Destroy()
      sys.exit(0)

   def cb_about(self, event):           # gui event
      dlg = wx.MessageDialog(self,
                "%s\n\n"
                "This program is for inspecting\n"
                "and evaluating X matrices." % UIX.g_version,
                "About Program", wx.OK | wx.ICON_INFORMATION)
      dlg.ShowModal()
      dlg.Destroy()

   def cb_help(self, event):
      self.gui_show_help()

   def gui_show_help(self):
      self.show_text_window(UIX.gui_help_string, 'Program Help (for GUI)')

   def cb_help_cmd(self, event):
      self.gui_show_help_cmd()

   def gui_show_help_cmd(self):
      self.show_text_window(UIX.g_help_string,'Program Help (for command line)')

   def cb_history(self, event):
      self.gui_show_history()

   def gui_show_history(self):
      self.show_text_window(UIX.g_history, 'Program History')

   def popup_warning(self, mesg):
      dlg = wx.MessageDialog(self, mesg, "warning: ", wx.OK|wx.ICON_EXCLAMATION)
      dlg.ShowModal()
      dlg.Destroy()

   def show_text_window(self, text, title='', wrap=1, wrapstr='\n'):
      if not text: return
      if len(text) < 1: return

      if wrap: text = UTIL.add_line_wrappers(text, wrapstr)

      text_frame = wx.Frame(self, -1, title)

      # ----------------------------------------
      # create TextCtrl portion
      ctrl = wx.TextCtrl(text_frame, -1, style = wx.TE_MULTILINE)
      ctrl.SetValue(text)

      # adjust the font style to be fixed-width
      ctrl.SetFont(self.fixed_font)

      # note the size for 80 character lines
      size = self.get_textwin_size(ctrl)

      # move to top
      ctrl.SetInsertionPoint(0)

      text_frame.ctrl = ctrl    # save it
      # ----------------------------------------

      # add a horizontal line
      # hline=wx.StaticLine(text_frame, -1, (-1,-1), (-1,-1), wx.LI_HORIZONTAL)

      # fill a sizer with buttons
      
      hsizer = wx.BoxSizer(wx.HORIZONTAL)  # was wx.NewId()
      b1 = wx.Button(text_frame, ID_SAVE, "&Save Text", (-1,-1), wx.DefaultSize)
      b2 = wx.Button(text_frame, ID_CLOSE, "&Close", (-1,-1), wx.DefaultSize)
      hsizer.Add(b1, 0)
      hsizer.Add(b2, 0, wx.LEFT, 10)

      # and set button callbacks

      text_frame.Bind(wx.EVT_BUTTON, self.cb_save_text, id=ID_SAVE)
      text_frame.Bind(wx.EVT_BUTTON, self.cb_close_win, id=ID_CLOSE)

      # ----------------------------------------
      #  put it all in a vertical sizer

      vspace = 5
      vsizer = wx.BoxSizer(wx.VERTICAL)
      vsizer.Add(hsizer, 0, wx.ALIGN_LEFT | wx.ALL, vspace)
      # vsizer.Add(hline, 0, wx.GROW | wx.ALL, vspace)
      vsizer.Add(ctrl, 1, wx.GROW | wx.ALL, vspace)

      text_frame.SetSizerAndFit(vsizer)

      text_frame.SetSize(self.textwin_size)

      text_frame.Show(True)

   def cb_tw_keydown(self, event):      # rcr - unused for now
      keycode = event.GetKeyCode()
      # maybe close the window
      if keycode == wx.WXK_ESCAPE or keycode == 'q' or keycode == 'Q':
         win = event.GetEventObject()
         win = win.GetTopLevelParent()
         win.Close()

   def get_textwin_size(self, ctrl):
      """given a TextCtrl window, set and return self.textwin_size for an
         80 column window (multiply default height be 1.5)"""

      if self.textwin_size: return self.textwin_size

      # else, compute and set it
      str80 = '0123456789012345678901234567890123456789'        \
              '0123456789012345678901234567890123456789'
      width,height,d,l = ctrl.GetFullTextExtent(str80)

      width  *= 1.05    # add 4 chars for scrollbar
      height *= 25      # 25 lines of text

      if self.XM.verb > 2:
         print "-- setting default textwin size to (%d,%d)" % (width,height)

      self.textwin_size = (width,height)

      ctrl.SetSize((width,height))

   def cb_save_text(self, event):
      win = event.GetEventObject()
      win = win.GetTopLevelParent()
      if self.XM.verb > 3:
         print "-- cb: save text from window '%s' ..." % win.GetTitle()
      try:
         ctrl = win.ctrl
         text = ctrl.GetValue()
      except:
         print '** no ctrl/text for save_text window'
         return

      fname = wx.SaveFileSelector('text', '')
      if fname == '': return

      if self.XM.verb > 1:
         print "-- saving text from window '%s' to file '%s'" %   \
               (win.GetTitle(), fname)
      UTIL.write_text_to_file(fname, text)

   def cb_close_win(self, event):       # no gui_ for generic close
      win = event.GetEventObject()
      win = win.GetTopLevelParent()
      if self.XM.verb > 2:
         print "-- closing window '%s' ..." % win.GetTitle()
      win.Destroy()

   def set_fixed_width_font(self, font):
      """find some fixed-width font, and store it as self.fixed_font"""

      if self.fixed_font: return self.fixed_font    # been here, did this

      if self.XM.verb > 1: print '-- testing for fixed-width font...'

      if font.IsFixedWidth():   # nothing to do but save for later
         return self.set_fixed_font(font)

      if self.XM.verb > 1: print '-- trying specific families ...'

      for family in [wx.NORMAL, wx.TELETYPE]:   # try different families
         font.SetFamily(family)
         if font.IsFixedWidth():
            return self.set_fixed_font(font)

      if self.XM.verb > 1: print '-- trying specific facenames ...'

      for name in ['Courier', 'Courier New', 'MiscFixed', 'Fixed',
                   'Monospace', 'LucidaTypewriter']:
         if not font.SetFaceName(name):         # doesn't exist, continue
            continue
         if font.IsFixedWidth():                # verify
            return self.set_fixed_font(font)

      if self.XM.verb > 1: print '-- searching all facenames for first...'

      # no luck, search through faces and take the first one
      # (but don't further corrupt passed font)
      fontenum = wx.FontEnumerator()
      fontenum.EnumerateFacenames()
      facenames = fontenum.GetFacenames()
      for name in facenames:
         tfont = wx.Font(10, wx.MODERN, wx.NORMAL, wx.NORMAL,
                         underline=False, face=name)
         if tfont.IsFixedWidth():
            return self.set_fixed_font(tfont)

      # failure, use what we have
      print '** cannot find fixed-width font, sorry...'
      return self.set_fixed_font(font)

   def set_fixed_font(self, font):
      if self.XM.verb > 1:
         print '++ setting font to face=%s, family=%s, style=%s, fixed=%s' % \
               (font.GetFaceName(), font.GetFamily(), font.GetStyle(),
                font.IsFixedWidth())
      self.fixed_font = font
      return font

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
         

# ======================================================================
# plotting canvas class
#
# This canvas is only used for plotting graphs of matrices.

class CanvasFrame(wx.Frame):
   def __init__(self, as_one=0):
      wx.Frame.__init__(self, None, -1, 'CanvasFrame', size=(400,300))
      self.figure = Figure()
      self.canvas = FigureCanvas(self, -1, self.figure)
      self.sizer = wx.BoxSizer(wx.VERTICAL)
      self.sizer.Add(self.canvas, 1, wx.LEFT | wx.TOP | wx.GROW)
      self.SetSizer(self.sizer)

      self.canvas.mpl_connect('key_press_event', self.cb_keypress)
      self.as_one  = as_one

      self.toolbar = NavigationToolbar2Wx(self.canvas)
      self.toolbar.Realize()
      self.sizer.Add(self.toolbar, 0, wx.LEFT | wx.EXPAND)
      self.toolbar.update()

   def cb_keypress(self, event):
      if event.key == 'q':
         self.Close()

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
            if nmats > 3:
               ax.set_ylabel('%-*s ' % (maxlen,labels[ind]),
                             rotation='horizontal')
               pl, pb, pw, ph = ax.get_position()
               ax.set_position((0.2, pb, pw, ph))
            else: ax.set_ylabel('%s' % labels[ind])

      self.Fit()
      self.canvas.draw()

      matplotlib.rcParams['lines.linewidth'] = 1   # reset

   def plot_mat_by_cols(self, amat, cols):

      if not cols: return
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
            pl, pb, pw, ph = ax.get_position()
            ax.set_position((0.2, pb, pw, ph))

      self.Fit()
      self.canvas.draw()

def regress_index(label):
   """return integer after trailing '#' if found, else -1"""

   ind = label.rfind('#')
   if ind < 0: return -1

   try:    val = int(label[ind+1:-1])
   except: val = -1

   return val

