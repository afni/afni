#!/usr/bin/env python

import sys, os
from PyQt4 import QtCore, QtGui

# allow ctrl-c its default behavior
import signal
signal.signal(signal.SIGINT, signal.SIG_DFL)

import lib_vars_object as VO
import lib_uber_subject as USUBJ
import lib_qt_gui as QLIB
import gui_uber_subj as GUS    # single subject GUI

U_STATUS_UBER_DIR       = 1     # status bit for valid results directory
__version__ = "0.0.1"

# for each subject (Subject class), store:
#   - ready flag, SID, local flag, results dir, labels, regress method
# 
#   - have Subject and SubjectAnalysis classes?
# 

g_help_string = """
help for uber_proc.py           - still a lot to do

1. Run from the directory where you would like the results to be placed.
   By default, a new 'uber_results' directory will be placed there, under
   which might be 'subjects', 'group_results', etc.

2. Panic into error.
"""

class MainWindow(QtGui.QMainWindow):
   """the main points of this class are to 
        - start up other GUIs to process data
        - keep track of data necessary for analyzing multiple subjects
          and runnning group analyses"""

   def __init__(self, parent=None):
      super(MainWindow, self).__init__(parent)

      self.gvars = VO.VarsObject('uber_proc gui vars')
      self.vars = VO.VarsObject('uber_proc vars')

      # rcr -delete
      self.SSD = None

      self.gvars.centralW = QtGui.QWidget()

      self.init_proc_vars()

      self.gvars.wlist = []
      self.add_initial_widgets()
      self.add_single_subj_widgets()
      self.add_group_widgets()
      self.add_subject_list_widget()

      # then add all of these into a main layout
      self.gvars.vbox = QtGui.QVBoxLayout(self.gvars.centralW)
      for w in self.gvars.wlist:
          self.gvars.vbox.addWidget(w)
      self.setCentralWidget(self.gvars.centralW)

      self.vars.stat_mesg = 'Ready'
      if self.vars.verb > 1: self.vars.show(self.vars.stat_mesg)
      self.update_status_bar()

   def init_proc_vars(self, infile=None):
      """initialize main variables, possibly from a file
         - separate GUI vars from UI vars
           (UI vars are those for control of other interfaces)"""

      self.vars.status     = 0    # Ready (errors are form U_STATUS bits)
      self.vars.subjects   = []   # array of Subject instances
      self.vars.verb       = 3    # verbose level


      # want functions to reset_vars() and init_from_results_dir()
      # might want to garbage collect

      # GUI vars first
      self.init_dirs()
      # self.settings_file = 'uber_analyze'       # will be .conf, if used
      # self.read_settings_file()

   def init_dirs(self, results_dir=None):
      """work from the uber_results directory"""

      self.vars.results_dir = USUBJ.get_uber_results_dir(results_dir)

      # rcr - when should we create the results directory?

      #try: os.chdir(self.vars.results_dir)
      #except:
      #   QtGui.QMessageBox.warning(self, "Error entering results directory",
      #         "dir = %s" % self.vars.results_dir)
      #   self.vars.status |= U_STATUS_UBER_DIR

   def read_settings_file(self):
      if not in_home_dir(): # get any local settings file
         QtCore.QSettings.setPath(QtCore.QSettings.NativeFormat,
                               QtCore.QSettings.UserScope, self.vars.results_dir)

      settings = QtCore.QSettings(self.settings_file)
      self.ss = settings.value("TestVal").toString()
      print ('settings value = %s' % self.ss)
      print ('cur dir = %s' % os.getcwd())

      return

   def add_initial_widgets(self):
      """add status bar, menu bar and central widget"""

      self.add_status_bar()
      self.add_menu_bar()

      return

   def add_status_bar(self):
      """add status bar with init message, and init N subjects status text"""

      self.vars.stat_mesg = 'initializing...'
      self.vars.nsubj = 0

      self.nsubjLab = QtGui.QLabel()
      self.nsubjLab.setFrameStyle(QtGui.QFrame.StyledPanel|QtGui.QFrame.Sunken)
      statusbar = self.statusBar()
      statusbar.showMessage(self.vars.stat_mesg)
      statusbar.addPermanentWidget(self.nsubjLab)

      self.update_status_bar(self.vars.stat_mesg, self.vars.nsubj)

   def update_status_bar(self, stat_mesg=None, nsubj=-1):
      """update the status bar with a new message, # subjects or both"""
      statusbar = self.statusBar()
      if stat_mesg != None: statusbar.showMessage(stat_mesg)
      else:                 statusbar.showMessage(self.vars.stat_mesg)
      if nsubj >= 0: self.nsubjLab.setText("%d subjects" % nsubj)

   def add_menu_bar(self):

      self.MBM_file = self.menuBar().addMenu("&File")
      self.MBM_help = self.menuBar().addMenu("&Help")
     
      fileSaveAction = self.createAction("&Save", slot=self.fileSave,
                shortcut=QtGui.QKeySequence.Save, tip="save group info")
      fileQuitAction = self.createAction("&Quit", slot=self.close,
                shortcut="Ctrl+Q", tip="close the application")
      helpHelpAction = self.createAction("&Help", slot=self.uberHelp,
          shortcut=QtGui.QKeySequence.HelpContents, tip="help for uber_proc.py")
      self.addActions(self.MBM_file, (fileSaveAction, fileQuitAction))
      self.addActions(self.MBM_help, [helpHelpAction])
      
      return

   def createAction(self, text, slot=None, shortcut=None, tip=None,
                    checkable=False, icon=None, signal="triggered()"):
      action = QtGui.QAction(text, self)
      if shortcut is not None:
         action.setShortcut(shortcut)
      if tip is not None:
         action.setToolTip(tip)
         action.setStatusTip(tip)
      if slot is not None:
         self.connect(action, QtCore.SIGNAL(signal), slot)
      if checkable:
         action.setCheckable(True)
      if icon is not None:
         action.setIcon(QtGui.QIcon("%s.png" & icon))
      return action

   def addActions(self, target, actions):
      for action in actions:
         if action is None:
            target.addSeparator()
         else:
            target.addAction(action)

   def uberHelp(self):

      # rcr - make permanent?  (allow edit, save, reset text?)
      win = QLIB.TextWindow(text=g_help_string, title='uber_proc.py help',
                      parent=self)
      if win.status: del(win)
      else:          win.show()

   def fileSave(self):

      print '-- would save something'


   def add_single_subj_widgets(self):

      ssbox = QtGui.QGroupBox("Single Subject Analysis")

      h1 = QtGui.QHBoxLayout(ssbox)     # main horizontal box

      # ------------------------------------------------------------
      # first the options boxes
      # processing defaults
      self.gvars.SSdef_lables = ["defaults", "from prior subject"]
      self.gvars.SSdef = QLIB.radio_group_box(ssbox,
                "new subject defaults", self.gvars.SSdef_lables)

      h1.addWidget(self.gvars.SSdef.mainw)    # and add to main horizontal box

      # ------------------------------------------------------------
      # then the action button list
      self.gvars.SSbuttons_labels = \
                ["New Subject", "Reprocess Subject", "Import Subject"]
      self.gvars.SSbuttons = QLIB.button_list_widget(ssbox,
                self.gvars.SSbuttons_labels, self.cb_new_subj, ltype=0)
      h1.addWidget(self.gvars.SSbuttons.mainw)


      self.gvars.wlist.append(ssbox)

      return

   def cb_new_subj(self):
      """generic callback for all '* Subject' buttons (e.g. Add)"""

      button = self.sender()
      if button is None or not isinstance(button, QtGui.QPushButton):
         return

      # note which radio button is set
      bind = self.gvars.SSdef.get_checked()
      if self.vars.verb > 2:
         print("++ Def button '%s' is set"%self.gvars.SSdef.blist[bind].text())

      self.act_new_subj(self.gvars.SSbuttons.get_button_text(button),
                        rbindex=bind)

   def act_new_subj(self, label, rbindex=-1):
      """action function for Add/Reprocess/Import Subject operations"""

      # rcr - these would set options and call new interfaces
      # This interface should be able to setup and analyze a subject.
      # Create this interface and only call show() when the user requests it.
      if label.startswith("New Subj"):
         print("pushed 'New Subject' button")
         if self.SSD == None:
            self.SSD = GUS.SingleSubjectWindow(set_sdir=1)
            self.SSD.show()
         else:
            print '-- showing SSD...'
            self.SSD.reset_vars(set_sdir=1)
            self.SSD.show()
            self.SSD.raise_()

      elif label.startswith("Re"):
         print("pushed 'Reprocess Subject' button")
      elif label.startswith("Import"):
         print("pushed 'Import Subject' button")
      else: print("** pushed unknown button, text = %s" % label)

   def rcr_p(self):
      print '== pushed external dialog button'

   def add_group_widgets(self):

      grbox = QtGui.QGroupBox("Group Analysis")
      v = QtGui.QVBoxLayout(grbox)     # main vertical box

      self.gvars.GR_labels = ['3dANOVA2', '3dANOVA3', '3dMEMA', '3dLME.R',
                              '3dttest']
      self.gvars.GR_exec = QLIB.label_opt_exec_widget(grbox, "Program: ",
                self.gvars.GR_labels, 'RUN', self.cb_group_exec)

      v.addWidget(self.gvars.GR_exec.mainw)
      self.gvars.wlist.append(grbox)

   def cb_group_exec(self):

      self.group_exec(self.gvars.GR_exec.menu_choice())

   def group_exec(self, index):

      pname = self.gvars.GR_labels[index]
      if self.vars.verb > 1:
         print('++ trying to execute Group Analysis Program #%d: %s' % \
               (index, pname))

      if index == 2:
         print 'ready to start 3dMEMA interface'
      else:
         QtGui.QMessageBox.warning(self, "Not Yet Ready",
               "GUI for program '%s' is not yet ready" % pname)

   def add_subject_list_widget(self):

      return

   def closeEvent(self, event):
      # settings = QtCore.QSettings(self.settings_file)
      # if self.ss == '': settings.setValue("TestVal", 'no value set')
      # else:             settings.setValue("TestVal", self.ss)
      if self.SSD != None: self.SSD.close()
      return

def directory_exists(dirname):
   """return whether dirname exists as a directory"""
   return 0

def in_home_dir():
   """return whether we are in the user's home dir, according to $HOME"""
   homedir = os.getenv('HOME')
   if homedir == None: return 0
   if os.getcwd() == homedir: return 1
   return 0

def main():
   app = QtGui.QApplication(sys.argv)
   QtGui.QApplication.setStyle(QtGui.QStyleFactory.create("Cleanlooks"))
   form = MainWindow()
   form.show()
   app.exec_()

   return 0

if __name__ == '__main__':
   if '-help' in sys.argv:
      print g_help_string
      sys.exit(0)
   sys.exit(main())
