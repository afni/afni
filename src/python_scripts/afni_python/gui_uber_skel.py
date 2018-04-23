#!/usr/bin/env python

# basically, a GUI to write an afni_proc.py command

import sys, os, math

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['signal', 'PyQt4']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import signal

# allow ctrl-c its default behavior
signal.signal(signal.SIGINT, signal.SIG_DFL)

from PyQt4 import QtCore, QtGui

import afni_base as BASE
import afni_util as UTIL
import lib_subjects as SUBJ
import lib_vars_object as VO
import lib_uber_skel as USKEL
import lib_qt_gui as QLIB

# allow users to play with style
g_styles = ["windows", "motif", "cde", "plastique", "cleanlooks"]
g_style_index = 0
g_style_index_def = 4

g_LineEdittype = None                   # set this type later


# ======================================================================
# main module class
class MainWindow(QtGui.QMainWindow):
   """class for a main window

         parent         parent Widget, can be None if this is main Widget
         cvars          VarsObject for init of control vars
         uvars          VarsObject for init of user vars
         set_pdir       upon GUI exec, set uvars.proc_dir bases on sid
   """

   def __init__(self, parent=None, cvars=None, uvars=None, set_pdir=0):
      super(MainWindow, self).__init__(parent)

      # ------------------------------
      # init main vars structs
      self.atest  = None        # AlignTest class element
      self.gvars  = VO.VarsObject('uber_subject gui vars')

      # init verb and try to update from uvars
      self.verb = 1
      if uvars != None:     # try to update verb from uvars
         if uvars.valid('verb'):
            try: self.verb = int(uvars.verb)
            except: print '** UAT Main: bad uvars.verb = %s' % uvars.verb

      # initialize the subject variables to defaults, update at the end
      self.cvars  = USKEL.g_cdef_strs.copy('GUI control vars')
      self.uvars  = USKEL.g_udef_strs.copy('GUI user vars')

      self.set_pdir = set_pdir

      # ------------------------------
      # L1 - main menubar and layout
      self.add_menu_bar()
      self.add_tool_bar()
      self.add_status_bar()
      self.gvars.Wcentral = QtGui.QWidget()
      mainlayout = QtGui.QVBoxLayout(self.gvars.Wcentral)

      # ------------------------------
      # L2 - make top Group Box and Vertical Layout; add to main Layout
      self.make_l2_widgets()

      # ------------------------------
      # L3 - fill m2_vlayout with group boxes
      self.make_l3_group_boxes()

      # ------------------------------------------------------------
      # finish level 2 and then level 1
      mainlayout.addWidget(self.gvars.gbox_datasets)
      mainlayout.addWidget(self.gvars.m2_scroll)

      # ------------------------------
      # save this for last to ensure it is all visible
      self.gvars.m2_scroll.setWidget(self.gvars.m2_gbox_inputs)

      self.gvars.Wcentral.setMinimumSize(350, 400)
      self.gvars.Wcentral.setLayout(mainlayout)
      self.setCentralWidget(self.gvars.Wcentral)

      self.make_extra_widgets()

      self.gvars.style = g_styles[g_style_index_def]

      # widgets are done, so apply pass subject vars
      self.reset_vars(cvars=cvars, uvars=uvars, set_pdir=set_pdir)

      # status : 0 = must create script, 1 = have script, ready to execute
      #         -1 = script failure?
      self.status = 0

      if self.verb > 1: print '-- finished Single Subject Dialog setup'

   def reset_vars(self, cvars=None, uvars=None, set_pdir=0):
      """replace old uvars with new"""

      self.set_pdir = set_pdir

      del(self.cvars)
      del(self.uvars)

      self.cvars = USKEL.g_cdef_strs.copy()
      self.uvars = USKEL.g_udef_strs.copy()

      self.apply_cvars(cvars)
      self.apply_uvars(uvars)

   def make_l2_widgets(self):
      """create 'dataset inputs' box plus a scroll area for the options

         QGroupBox(dataset inputs)
         QScrollArea
            QGroupBox(input data and options) with VBox layout
               elsewhere:
                  QGroupBox(costs)
                  QGroupBox(align_centers)
                  QGroupBox(other_options)
                  ...
      """

      self.make_l2_group_box()  # for general subject info

      self.gvars.m2_scroll = QtGui.QScrollArea()
      self.gvars.m2_scroll.setWidgetResizable(True)

      # create a containing GroupBox for everything in m2_scroll
      gbox = self.get_styled_group_box("all options")

      # the layout for the 'all options' QGroupBox is vertical
      self.gvars.m2_vlayout = QtGui.QVBoxLayout(gbox)
      gbox.setLayout(self.gvars.m2_vlayout)
      self.gvars.m2_gbox_inputs = gbox

   def get_styled_group_box(self, title):

      gbox = QtGui.QGroupBox(title)

      # set box color 
      color = QtGui.QColor('blue').light(50)
      palette = QtGui.QPalette(gbox.palette())
      palette.setColor(QtGui.QPalette.Active, QtGui.QPalette.Mid, color)
      gbox.setPalette(palette)

      return gbox

   def make_l2_group_box(self):
      """create a group box with a VBox layout:

            GridLayout:
               Label(anat)     QPushB(choose) LineEdit()
               Label(EPI)      QPushB(choose) LineEdit()
               Label(EPI base)                LineEdit()

         for uvars: anat, epi, epi_base
      """

      # updating:
      # Line_anat.finished would set uvars.anat
      # CB_gbox_PushB callback would fill Line_anat and set uvars.anat

      gbox = self.get_styled_group_box("dataset inputs")

      layout = QtGui.QGridLayout(gbox)

      # --------------------------------------------------
      # anat line
      label = QtGui.QLabel("anat")              # label
      pb = QtGui.QPushButton(gbox)              # pushB
      pb.setText("browse anat")
      line = QtGui.QLineEdit()  # lineEdit
      line.setText(self.uvars.anat)
      layout.addWidget(label, 0, 0)
      layout.addWidget(pb, 0, 1)
      layout.addWidget(line, 0, 2, 1, 3)

      # add callbacks
      pb.connect(pb, QtCore.SIGNAL("clicked()"), self.CB_gbox_PushB)
      line.connect(line, QtCore.SIGNAL("editingFinished()"), self.CB_line_text)

      # store what we need in gvars
      self.gvars.PB_anat = pb           # for sender in CB_gbox_PushB
      self.gvars.Line_anat = line

      # --------------------------------------------------
      # epi line
      label = QtGui.QLabel("EPI")               # label
      pb = QtGui.QPushButton(gbox)              # pushB
      pb.setText("browse EPI")
      line = QtGui.QLineEdit()  # lineEdit
      line.setText(self.uvars.anat)
      layout.addWidget(label, 1, 0)
      layout.addWidget(pb, 1, 1)
      layout.addWidget(line, 1, 2, 1, 3)

      # add callbacks
      pb.connect(pb, QtCore.SIGNAL("clicked()"), self.CB_gbox_PushB)
      line.connect(line, QtCore.SIGNAL("editingFinished()"), self.CB_line_text)

      # store what we need in gvars
      self.gvars.PB_epi = pb           # for sender in CB_gbox_PushB
      self.gvars.Line_epi = line

      # --------------------------------------------------
      gbox.setLayout(layout)
      self.gvars.gbox_datasets = gbox

   def CB_line_text(self):
      """call-back for text updates in the level 3 gbox"""
      obj = self.sender()
      if obj == self.gvars.Line_anat:
         self.update_textLine_check(obj, obj.text(), 'anat', 'anatomical dset',
                                    QLIB.valid_as_filepath)

      elif obj == self.gvars.Line_epi:
         self.update_textLine_check(obj, obj.text(), 'epi', 'EPI dataset',
                                    QLIB.valid_as_filepath)

      else: print '** CB_line_text: unknown sender'

   def make_l3_group_boxes(self):
      """create anat, EPI, stim, etc. group boxes, and add to m2_vlayout"""

      self.gvars.gbox_costs = self.group_box_costs()

      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_costs)

   def group_box_costs(self):
      """create a group box with *some* layout:
                group box of QCheckBox's to choose from a default cost list
                clear and apply buttons
                label and line text for list

         for controlling uvar: costs
      """

      gbox = self.get_styled_group_box("cost functions")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      # rcr - do some stuff
      layout = QtGui.QVBoxLayout(frame) # now a child of frame
      # layout.addWidget(self.gvars.Line_anat)

      frame.setLayout(layout)
      glayout.addWidget(frame)
      gbox.setLayout(glayout)
      return gbox

   def init_gbox_viewable(self, box, view):
      box.setCheckable(True)
      box.setChecked(view)
      if view: box.frame.show()
      else:    box.frame.hide()

   def gbox_toggle_frame(self):
      obj = self.sender()
      if obj.isChecked(): obj.frame.show()
      else: obj.frame.hide()

   def gbox_clicked(self):
      obj = self.sender()
      if obj.isChecked(): obj.frame.show()
      else: obj.frame.hide()

   def make_file_dialog(self, title, dir, filter):
      fileD = QtGui.QFileDialog(self)
      fileD.setWindowTitle(QtCore.QString(title))
      fileD.setFilter(QtCore.QString(filter))
      fileD.setDirectory(QtCore.QString(dir))
      return fileD

   def CB_checkbox(self):
      """call-back for any check boxes"""
      obj = self.sender()
      if   obj == self.gvars.gbox_anat.checkBox:
         if obj.isChecked(): self.set_uvar('get_tlrc', 'yes')
         else:               self.set_uvar('get_tlrc', 'no')
      else: print "** CB_checkbox: unknown sender"

   def update_textLine_check(self, obj, text, attr, button_name, check_func):
      """check text against check_func(text, bname, 1, self, 1)
         - this warns the user on error
         - if valid, update the attr and textLine with the text
         - else, clear object and set focus to it (and return)

         return 1/0 to specify whether value was applied
      """
      
      # be sure we have a type to compare against for setting the text
      global g_LineEdittype
      if g_LineEdittype == None: g_LineEdittype = type(QtGui.QLineEdit())

      rtext = str(text) # in case of QString

      if check_func(rtext, button_name, 1, self, 1):
         self.set_uvar(attr, rtext)
         if type(obj) == g_LineEdittype: obj.setText(text)
         else: print '** update_textLine_check: not a LineEdit type'
         return 1
      else:
         # error, reset to previous attribute
         # obj.clear()
         obj.setText(self.uvars.val(attr))
         obj.setFocus()
         return 0

   def cb_show_script(self):

      if self.update_uvars_from_gui(warn=1): return

      # if we have a subject directory, make backup scripts
      if self.cvars.is_non_trivial_dir('proc_dir'):
         self.set_uvar('copy_scripts', 'yes')

      self.atest = USKEL.AlignTest(cvars=self.cvars, uvars=self.uvars)
      nwarn, wstr = self.atest.get_warnings()
      status, mesg = self.atest.get_script()

      if status: # show errors
         self.update_error_window(mesg)
         return

      self.status = 1   # have script
      self.atest.write_script()

      fname = self.atest.proc_dir_filename('file_proc')
      if not fname:
         self.update_warn_window('** no script to show')
         return
      elif not os.path.isfile(fname):
         self.update_warn_window('** script not found\n' \
                                 '   (should be file %s)' % fname)
         return

      self.update_result_window(title="Success!  align script:", fname=fname)
      if nwarn > 0: self.update_warn_window(wstr)

      # if we have a proc dir that exists, save the UA.py command there
      self.write_uber_command()

      self.gvars.act_exec_script.setEnabled(True)

   def cb_exec_script(self):

      fmesg = ''
      if not self.atest:  fmesg = '** no script class for running script'
      elif self.status<1: fmesg = '** need to first generate processing script'
      if fmesg != '':
         QLIB.guiError('Error', fmesg, self)
         return

      # make sure there is a script to execute
      fname = self.atest.proc_dir_filename('file_proc')
      if not fname:
         self.update_AP_warn_window('** script file not set')
         return
      elif not os.path.isfile(fname):
         self.update_AP_warn_window('** script file not found: %s\n' % fname)
         return

      self.atest.nuke_old_results()    # get rid of any previous results

      # start a new window?
      if self.gvars.valid('SPW'):
         self.gvars.SPW.close()
         del(self.gvars.SPW)

      # actually run script, piping output if we have an output file
      sname = self.atest.rvars.file_proc
      oname = self.atest.rvars.output_proc
      command= 'tcsh -xef %s' % sname
      if oname: command = '%s |& tee %s' % (command, oname)
      self.gvars.SPW = QLIB.TcshCommandWindow(command,
                           dir=self.atest.cvars.proc_dir, parent=self)
      self.gvars.SPW.show()
      self.status = 2

   def write_uber_command(self):
      pdir = self.atest.cvars.val('proc_dir')
      if os.path.isdir(pdir):
         sstr = self.make_uber_command()
         UTIL.write_text_to_file('%s/.orig.cmd.uat' % pdir, sstr)

   def cb_command_window(self, cmd):
      print '++ python exec command: %s' % cmd
      exec(cmd)

   def cb_show_py_command_window(self):
      """show the python command window"""
      if not self.gvars.valid('PCW'):
         self.gvars.PCW = QLIB.PyCommandWindow(callback=self.cb_command_window)
      self.gvars.PCW.show()
      self.gvars.PCW.raise_()

   def cb_show_command_window(self):
      """show the shell command window"""
      # start a new window?
      if not self.gvars.valid('CW'):
         self.gvars.CW = QLIB.ProcessWindow(parent=self)
      self.gvars.CW.show()
      self.gvars.CW.raise_()

   def CB_gbox_PushB(self):
      """these buttons are associated with anat/EPI/stim file group boxes
         - the sender (button) text must be unique"""

      try:
         sender = self.sender()
         text = str(sender.text())
      except:
         print '** CB_gbox_PushB: no text'
         return

      # anat
      if text == 'help: anat':
         self.update_help_window(g_help_anat, title='anatomical datasets')

      elif text == 'browse anat':
         fname = QtGui.QFileDialog.getOpenFileName(self,
                   "load anatomical dataset", self.pick_base_dir('anat'),
                   "datasets (*.HEAD *.nii);;all files (*)")
         self.update_textLine_check(self.gvars.Line_anat,
                fname, 'anat', 'anatomical dset', QLIB.valid_as_filepath)

      elif text == 'browse EPI':
         fname = QtGui.QFileDialog.getOpenFileName(self,
                   "load EPI dataset", self.pick_base_dir('epi'),
                   "datasets (*.HEAD *.nii);;all files (*)")
         self.update_textLine_check(self.gvars.Line_epi,
                fname, 'epi', 'EPI dset', QLIB.valid_as_filepath)

      else: print "** unexpected button text: %s" % text

   def resize_table(self, table, countLabel=None):
      nrows = table.rowCount()
      if nrows > 0: rheight = table.rowHeight(0)
      else        : rheight = 0
      if self.verb > 2: print '-- resize_table: using row height %d' % rheight
      table.setAlternatingRowColors(True)
      table.setFixedHeight(self.max_table_size(nrows, rheight=rheight))
      table.resizeRowsToContents()
      QLIB.resize_table_cols(table)

      if countLabel != None: countLabel.setText('%d' % nrows)

   def remove_blank_table_rows(self, table, countLabel=None):

      # --------------------------------------------------
      # gltsym table (get labels and GLTs)
      table = self.gvars.Table_gltsym           # convenience
      nrows = table.rowCount()
      ncols = table.columnCount()

      # work from row = nrows-1 down to 0, so deletion does not affect indices
      for row in range(nrows-1, -1, -1):
         found = 0
         # search for something in this row
         for col in range(ncols):
            item = table.item(row, col)
            if item != None:
               if str(item.text()) != '':
                  found = 1
                  break
         if not found: table.removeRow(row)

      self.resize_table(table, countLabel)

      return 0

   def pick_base_dir(self, dtype):
      """return something useful or an empty string"""
      anat = self.uvars.anat    # for ease of typing
      epi  = self.uvars.epi
      if dtype == 'top':     # common dir to all input files
         return UTIL.common_dir([anat, epi])
      elif dtype == 'anat':
         if anat != '':      return os.path.dirname(anat)
         elif epi != '':     return os.path.dirname(epi)
      elif dtype == 'epi':
         if epi != '':       return os.path.dirname(epi)
         elif anat != '':    return os.path.dirname(anat)
      else:
         print '** pick_base_dir: bad dtype = %s' % dtype

      return ''

   def add_menu_bar(self):

      # ----------------------------------------------------------------------
      # main process actions
      act1 = QLIB.createAction(self, "gen: align script",
        slot=self.cb_show_script,
        tip="generate align_epi_anat.py align script",
        icon=self.style().standardIcon(QtGui.QStyle.SP_FileDialogDetailedView))

      act2 = QLIB.createAction(self, "exec: align script",
        slot=self.cb_exec_script,
        tip="execute align script",
        icon=self.style().standardIcon(QtGui.QStyle.SP_DialogYesButton))

      act3 = QLIB.createAction(self, "reset all options",
          slot=self.cb_clear_options,
          tip="keep datasets: restore all other GUI options to defaults",
          icon=self.style().standardIcon(QtGui.QStyle.SP_TrashIcon))

      act4 = QLIB.createAction(self, "reset all fields",
          slot=self.cb_clear_fields,
          tip="restore all GUI fields to defaults",
          icon=self.style().standardIcon(QtGui.QStyle.SP_TrashIcon))


      self.gvars.act_show_script = act1
      self.gvars.act_exec_script = act2
      self.gvars.act_clear_opts = act3
      self.gvars.act_clear_all  = act4

      self.gvars.act_exec_script.setEnabled(False)

      # ----------------------------------------------------------------------
      # File menu
      self.gvars.MBar_file = self.menuBar().addMenu("&File")
      actFileQuit = QLIB.createAction(self,"Quit", slot=self.close,
          shortcut="Ctrl+q", tip="close the application")
      QLIB.addActions(self, self.gvars.MBar_file,
                      [act1, act2, None, act3, act4, None, actFileQuit])

      # ----------------------------------------------------------------------
      # View menu - all for static view windows
      self.gvars.MBar_view = self.menuBar().addMenu("&View")

      act1 = QLIB.createAction(self, "resulting align script",
        slot=self.cb_view,
        tip="display script created via GUI")

      act2 = QLIB.createAction(self, "output from align script",
        slot=self.cb_view,
        tip="display text output from execution of align script")

      act3 = QLIB.createAction(self,"view: uber_skel.py command",
          slot=self.cb_view,
          tip="show command that would populate this interface")

      QLIB.addActions(self, self.gvars.MBar_view, [act1, act2, None, act3])

      self.gvars.act_view_script = act1
      self.gvars.act_view_output = act2
      self.gvars.act_view_cmd = act3

      # ----------------------------------------------------------------------
      # Hidden menu
      self.gvars.MBar_hidden = self.menuBar().addMenu("Hidde&n")

      act1 = QLIB.createAction(self,"view: user vars",
        slot=self.cb_view,
        tip="display user option variables")

      act2 = QLIB.createAction(self,"view: ctrl vars",
        slot=self.cb_view,
        tip="display control variables")

      act3 = QLIB.createAction(self,"view: GUI vars",
        slot=self.cb_view,
        tip="display GUI variables")

      QLIB.addActions(self, self.gvars.MBar_hidden, [act1, None, act2, act3])
      self.gvars.act_view_uvars = act1
      self.gvars.act_view_cvars = act2
      self.gvars.act_view_gvars = act3

      # ----------------------------------------------------------------------
      # Help menu
      self.gvars.MBar_help = self.menuBar().addMenu("&Help")
      actHelpUAS = QLIB.createAction(self,"uber_skel.py -help'",
          slot=self.cb_help_main, shortcut=QtGui.QKeySequence.HelpContents,
          tip="help for uber_skel.py")
      QLIB.addActions(self, self.gvars.MBar_help, [actHelpUAS])

      # add browse actions to browse sub-menu
      self.gvars.Menu_browse = self.gvars.MBar_help.addMenu("&Browse")
      act1 = QLIB.createAction(self,"web: all AFNI programs",
          slot=self.cb_help_browse, tip="browse AFNI program help")
      act2 = QLIB.createAction(self,"web: afni_proc.py help",
          slot=self.cb_help_browse, tip="browse afni_proc.py help")
      act3 = QLIB.createAction(self,"web: AFNI Message Board",
          slot=self.cb_help_browse, tip="browse Message Board")

      QLIB.addActions(self, self.gvars.Menu_browse, [act1, act2, act3])

      self.gvars.act_browse_all_progs = act1
      self.gvars.act_browse_AP_help   = act2
      self.gvars.act_browse_MB        = act3

      actHelpAbout = QLIB.createAction(self,"about uber_skel.py",
          slot=self.cb_help_about, tip="about uber_skel.py")
      QLIB.addActions(self, self.gvars.MBar_help, [actHelpAbout])

   def add_tool_bar(self):
      self.gvars.toolbar = self.addToolBar('uber_skel.py')

      self.gvars.toolbar.addAction(self.gvars.act_show_script)
      self.gvars.toolbar.addAction(self.gvars.act_exec_script)

   def add_status_bar(self):
      self.gvars.statusbar = self.statusBar()
      self.gvars.statusbar.showMessage("Ready")

   def make_extra_widgets(self):
      """create - ULIB.TextWindow for help messages, scrip and warnings
                - gvars.browser for web links"""

      # text window (if None (for now), new windows will be created)
      # self.gvars.Text_help = None
      self.gvars.Text_help = QLIB.TextWindow(parent=self)
      self.gvars.Text_script = QLIB.TextWindow(parent=self)

      # note whether we have a browser via import
      self.gvars.browser = None
      try: 
         import webbrowser
         self.gvars.browser = webbrowser
         if self.verb > 1: print '++ have browser'
      except:
         if self.verb > 1: print '-- NO browser'

   def open_web_site(self, site):
      if self.gvars.browser == None:
         QLIB.guiWarning('Error', 'no browser to use for site: %s'%site,self)
      else: self.gvars.browser.open(site)

   def update_result_window(self, win=None, text='', title='', fname=''):
      """default window is Text_script
         - if fname, read file
           else use text"""
      if win: window = win
      else:   window = self.gvars.Text_script

      if title: window.setWindowTitle(title)
      if fname: # then read from file
         window.filename = fname
         window.readfile()
      else: window.editor.setText(text)
      window.show()
      window.raise_()

   def update_error_window(self, text, title='ERROR - cannot proceed'):
      QLIB.guiError(title, text, self)

   def update_warn_window(self, text, title='WARNING'):
      QLIB.guiWarning(title, text, self)

   def update_help_window(self, text, title=''):
      # if no permanent window, make a new one each time
      if self.gvars.Text_help == None:
         if self.verb > 2: print '++ opening new TextWindow'
         win = QLIB.TextWindow(text=text, title=title, parent=self)
         win.setAttribute(QtCore.Qt.WA_DeleteOnClose)
         win.show()
      else:
         if title != '': self.gvars.Text_help.setWindowTitle(title)
         self.gvars.Text_help.editor.setText(text)
         self.gvars.Text_help.show()

   def cb_help_main(self):
      """display helpstr_usubj_gui in Text_Help window"""
      self.update_help_window(USKEL.helpstr_gui,
                              title='uber_skel.py: GUI help')

   def cb_help_about(self):
      """display version info in Text_Help window"""
      text = "uber_skel.py, version %s" % USKEL.g_version
      QLIB.guiMessage('about uber_skel.py', text, self)

   def cb_help_browse(self):
      obj = self.sender()
      if   obj == self.gvars.act_browse_all_progs:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/index.html')
      elif obj == self.gvars.act_browse_AP_help:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/afni_proc.py.html')
      elif obj == self.gvars.act_browse_MB:
         self.open_web_site('https://afni.nimh.nih.gov/afni/community/board')
      else: print '** cb_help_browse: invalid sender'

   def update_uvars_from_gui(self, warn=0):
      """set what we can, if warn, report error
         return 0 on success, 1 on error"""

      # rcr - any first variables to require?
      #if self.uvars.is_empty('sid') or self.uvars.is_empty('gid'):
      #   if warn: QLIB.guiError('Error', 
      #                          "** subject and group IDs must be set", self)
      #   return 1

      # rcr - maybe process tables
      # if self.update_uvars_from_tables(): return 1

      if self.set_pdir:
         # proc dir should read: tool_results/tool.0001.align_test
         pdir = SUBJ.get_def_tool_path('align_test', top_dir='tool_results',
                   prefix='tool', keep_if_missing=self.uvars.val('results_dir'))
         if self.set_cvar('proc_dir', pdir):
            print '-- setting proc_dir to %s' % pdir

      self.gvars.act_exec_script.setEnabled(False)

      return 0

   def cb_clear_options(self):
      """set uvars from defaults and redisplay GUI
         EXCEPT: keep dataset fields from subject"""

      uvars = VO.VarsObject()
      for atr in ['anat', 'epi', 'epi_base']:
         uvars.set_var(atr, self.uvars.val(atr))
      
      self.reset_vars(uvars=uvars, set_pdir=self.set_pdir)

   def cb_clear_fields(self):
      """set uvars from defaults and redisplay GUI"""
      self.reset_vars(set_pdir=self.set_pdir)

   def cb_view(self):
      """create permanent windows with given text"""
      obj = self.sender()

      if obj == self.gvars.act_view_script:
         self.show_static_file('file_proc', 'align script:')

      elif obj == self.gvars.act_view_output:
         self.show_static_file('output_proc', 'script output:')

      elif obj == self.gvars.act_view_cmd:
         sstr = self.make_uber_command()
         QLIB.static_TextWindow(title='corresp. uber_skel.py command',
                                text=sstr, parent=self)

      elif obj == self.gvars.act_view_uvars:
         sstr = self.uvars.make_show_str('current align test', name=0)
         QLIB.static_TextWindow(title='user vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_cvars:
         sstr = self.cvars.make_show_str('control vars', name=0, all=0)
         QLIB.static_TextWindow(title='control vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_gvars:
         sstr = self.gvars.make_show_str('GUI vars', name=0, all=1)
         QLIB.static_TextWindow(title='GUI vars', text=sstr, parent=self)

      else: print '** unhandled object in cb_view'

   def make_uber_command(self):
      """generate a script that would invoke the uber_subject.py interface
         with control and user vars set (and so fields filled)

         Put any key elements in quotes:
            basis functions
            gltsym
      """

      # first apply subject variables
      self.update_uvars_from_gui()

      cmd = 'uber_skel.py'

      # apply each uvar with -uvar option
      prefix = ' \\\n    -uvar '     # append before next command
      for atr in self.uvars.attributes():
         if atr == 'name': continue     # skip
         if self.uvars.vals_are_equal(atr, USKEL.g_udef_strs): continue
         # show this one
         val = self.uvars.val(atr)
         
         # special cases first: stim_basis, gltsym
         #if atr == 'gltsym':            # special case
         #   val = ["'%s'" % v for v in val]
         #elif atr == 'stim_basis':      # special case
         #   val = ["'%s'" % v for v in val]
         #   cmd += (prefix + '%s %s' % (atr, ' '.join(val)))

         if self.uvars.has_simple_type(atr):
            cmd += (prefix + '%s %s' % (atr, val))
         elif type(val) == list:
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))
         else:
            print '** make_uber_command: bad attr %s' % atr

      return UTIL.add_line_wrappers(cmd + '\n')

   def show_static_file(self, var_name, var_desc):
      """display the var according to var_name
         - use description in var_desc if any error"""
      if self.atest == None:        # check for generated command
         self.update_warn_window('** script must be generated first')
         return
      file = self.atest.proc_dir_filename(var_name)
      if not file:
         self.update_warn_window('** file not set: %s' % var_desc)
         return
      elif not os.path.isfile(file):
         self.update_warn_window('** file not found: %s\n' \
                                 '   (used for %s)'%(file, var_desc))
         return
      else:
         title = '%s %s' % (var_desc, file)
         QLIB.static_TextWindow(title=title, fname=file, parent=self)

   def set_cvar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if not self.cvars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print "++ set_cvar: update [%s] to '%s'"%(name,newval)

      # even for cvars, since proc_dir is part of script
      self.gvars.act_exec_script.setEnabled(False)

   def set_uvar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if not self.uvars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print "++ set_uvar: update [%s] to '%s'"%(name,newval)

      self.gvars.act_exec_script.setEnabled(False)

   def apply_cvars(self, cvars=None):
      """apply to the cvars object

         first init to defaults
         if cvars is passed, make further updates"""

      # merge with current vars and apply everything to GUI
      self.cvars.merge(cvars)

      if self.verb > 2: self.uvars.show("post reset control vars")

   def apply_uvars(self, uvars=None):
      """apply to the uvars object and to the gui

         first init to defaults
         if uvars is passed, make further updates"""

      # merge with current vars and apply everything to GUI
      self.uvars.merge(uvars)
      self.set_uvar('verb', str(self.verb))     # to pass verb along
      for var in self.uvars.attributes():
         self.apply_uvar_in_gui(var)

      if self.verb > 2: self.uvars.show("post reset user vars")

   def apply_uvar_in_gui(self, uvar):
      """this is a single interface to apply any user variable in the GUI

         if a variable is not handled in the interface, ignore it

         return 1 if processed
      """

      rv = 1
      if   uvar == 'uber_dir':             rv = 0       # todo
      elif uvar == 'anat':      self.gvars.Line_anat.setText(self.uvars.anat)
      elif uvar == 'epi':       self.gvars.Line_epi.setText(self.uvars.epi)
      else:
         if self.verb > 1: print '** apply_uvar_in_gui: unhandled %s' % uvar
         rv = 0

      if rv and self.verb > 2: print '++ apply_uvar_in_gui: process %s' % uvar

      return rv

# --- post MainWindow class

# ===========================================================================
# help strings


g_help_eg = """
   goals:

   description:

"""

# end: help strings
# ===========================================================================

def main():

   print '** this is not a main program'
   return 1

if __name__ == '__main__':
   sys.exit(main())
