#!/usr/bin/env python

# python3 status: compatible

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
import lib_uber_ttest as LTT
import lib_qt_gui as QLIB

# allow users to play with style
g_styles = ["windows", "motif", "cde", "plastique", "cleanlooks"]
g_style_index = 0
g_style_index_def = 4

g_LineEdittype = None                   # set this type later

show_func_seq = 0

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
      self.ttobj  = None        # TTest class element
      self.gvars  = VO.VarsObject('uber_ttest gui vars')

      # init verb and try to update from uvars
      self.verb = 1
      if cvars != None:     # try to update verb from uvars
         if cvars.valid('verb'):
            try: self.verb = int(cvars.verb)
            except: print('** UAT Main: bad cvars.verb = %s' % cvars.verb)

      # initialize the subject variables to defaults, update at the end
      self.cvars  = LTT.g_cdef_strs.copy('GUI control vars')
      self.uvars  = LTT.g_udef_strs.copy('GUI user vars')

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
      mainlayout.addWidget(self.gvars.gbox_program)
      mainlayout.addWidget(self.gvars.m2_scroll)

      # ------------------------------
      # save this for last to ensure it is all visible
      self.gvars.m2_scroll.setWidget(self.gvars.m2_gbox_inputs)

      self.gvars.Wcentral.setMinimumSize(100, 100)
      self.gvars.Wcentral.setLayout(mainlayout)
      self.setCentralWidget(self.gvars.Wcentral)

      self.make_extra_widgets()

      self.gvars.style = g_styles[g_style_index_def]

      # widgets are done, so apply pass subject vars
      self.reset_vars(cvars=cvars, uvars=uvars, set_pdir=set_pdir)

      # status : 0 = must create script, 1 = have script, ready to execute
      #         -1 = script failure?
      self.status = 0
      self.resize(500, 700)

      if self.verb > 1: print('-- finished Single Subject Dialog setup')

   def reset_vars(self, cvars=None, uvars=None, set_pdir=0):
      """replace old uvars with new"""

      if show_func_seq: print('=== A ===')
      self.set_pdir = set_pdir

      del(self.cvars)
      del(self.uvars)

      self.cvars = LTT.g_cdef_strs.copy()
      self.uvars = LTT.g_udef_strs.copy()

      self.apply_cvars(cvars)
      self.apply_uvars(uvars)

   def make_l2_widgets(self):
      """create 'main inputs' box plus a scroll area for the options

         QGroupBox(dataset inputs)
         QScrollArea
            QGroupBox(input data and options) with VBox layout
               elsewhere:
                  QGroupBox(datasets list A)
                  QGroupBox(datasets list B)
                  QGroupBox(other_options)
                  ...
      """
      if show_func_seq: print('=== B ===')

      self.make_l2_group_box()  # for general subject info

      self.gvars.m2_scroll = QtGui.QScrollArea()
      self.gvars.m2_scroll.setWidgetResizable(True)

      # create a containing GroupBox for everything in m2_scroll
      gbox = QLIB.get_styled_group_box("all options")

      # the layout for the 'all options' QGroupBox is vertical
      self.gvars.m2_vlayout = QtGui.QVBoxLayout(gbox)
      gbox.setLayout(self.gvars.m2_vlayout)
      self.gvars.m2_gbox_inputs = gbox

   def get_styled_group_box(self, title):

      if show_func_seq: print('=== C ===')

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

         for uvars: program
      """
      if show_func_seq: print('=== D ===')


      gbox = QLIB.get_styled_group_box('')

      layout = QtGui.QGridLayout(gbox)

      lposn = 0 # current line index

      # --------------------------------------------------
      # program line
      label = QLIB.make_label('program', 'group ttest program run by script')
      blist = ['prog: %s' % prog for prog in LTT.g_prog_list]
      pb = QLIB.create_menu_button(gbox, "choose", blist,
                call_back=self.CB_gbox_PushB)
      line = QLIB.make_line(LTT.g_prog_list[0], self.CB_line_text)
      layout.addWidget(label, lposn, 0)
      layout.addWidget(pb, lposn, 1)
      layout.addWidget(line, lposn, 2)

      # store what we need in gvars
      self.gvars.PB_program = pb        # for sender in CB_gbox_PushB
      self.gvars.Line_program = line
      lposn += 1

      # --------------------------------------------------
      # output script name

      label = QLIB.make_label('script name', 'name of resulting program script')
      line = QLIB.make_line(cb=self.CB_line_text)
      self.gvars.Line_script = line
      layout.addWidget(label, lposn, 0)
      layout.addWidget(line, lposn, 2)
      lposn += 1

      # --------------------------------------------------
      # dset prefix

      label=QLIB.make_label('dset prefix','prefix for dataset output by script')
      line = QLIB.make_line(cb=self.CB_line_text)
      self.gvars.Line_prefix = line
      layout.addWidget(label, lposn, 0)
      layout.addWidget(line, lposn, 2)
      lposn += 1

      # --------------------------------------------------
      # mask

      label= QLIB.make_label('mask dset','compute results only over mask')
      pb   = QLIB.make_button("browse",
                              tip="browse file system for mask dataset",
                              cb=self.CB_gbox_PushB, hstr=0)
      line = QLIB.make_line(cb=self.CB_line_text)
      self.gvars.Line_mask = line
      self.gvars.Pushb_mask = pb
      layout.addWidget(label, lposn, 0)
      layout.addWidget(pb,    lposn, 1)
      layout.addWidget(line,  lposn, 2)
      lposn += 1

      # --------------------------------------------------
      # paired test toggle button
      cbox = QtGui.QCheckBox("paired test")
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)
      cbox.setStatusTip("perform a paired test")
      self.gvars.CB_paired = cbox
      layout.addWidget(cbox,  lposn, 0, 1, 2)
      lposn += 1

      # --------------------------------------------------
      layout.setSpacing(QLIB.g_layout_spacing)
      gbox.setLayout(layout)
      self.gvars.gbox_program = gbox

   def CB_line_text(self):
      """call-back for text updates in the level 3 gbox"""
      if show_func_seq: print('=== E ===')

      # updates for top-level Line widgets
      obj = self.sender()
      if obj == self.gvars.Line_program:
         self.check_line_against_list(obj, 'program', LTT.g_prog_list)

      elif obj == self.gvars.Line_script:
         self.update_textLine_check(obj, obj.text(), 'script', 'script name',
                                    QLIB.valid_as_filename)

      elif obj == self.gvars.Line_prefix:
         self.update_textLine_check(obj, obj.text(), 'prefix', 'dset prefix',
                                    QLIB.valid_as_filename)

      elif obj == self.gvars.Line_mask:
         self.update_textLine_check(obj, obj.text(), 'mask', 'mask dset',
                                    QLIB.valid_as_filepath)

      # updates for dataset group box Line widgets
      elif obj == self.gvars.gbox_dsets_A.Line_name:
         self.update_textLine_check(obj, obj.text(), 'set_name_A', 'set name A',
                                    QLIB.valid_as_sub_brick)
      elif obj == self.gvars.gbox_dsets_A.Line_dind:
         self.update_textLine_check(obj, obj.text(), 'beta_A', 'data index A',
                                    QLIB.valid_as_sub_brick)
      elif obj == self.gvars.gbox_dsets_A.Line_tind:
         self.update_textLine_check(obj, obj.text(), 'tstat_A', 't-stat inex A',
                                    QLIB.valid_as_sub_brick)
      elif obj == self.gvars.gbox_dsets_B.Line_name:
         self.update_textLine_check(obj, obj.text(), 'set_name_B', 'set name B',
                                    QLIB.valid_as_sub_brick)
      elif obj == self.gvars.gbox_dsets_B.Line_dind:
         self.update_textLine_check(obj, obj.text(), 'beta_B', 'data index B',
                                    QLIB.valid_as_sub_brick)
      elif obj == self.gvars.gbox_dsets_B.Line_tind:
         self.update_textLine_check(obj, obj.text(), 'tstat_B', 't-stat inex B',
                                    QLIB.valid_as_sub_brick)

      else: print('** CB_line_text: unknown sender')

   def make_l3_group_boxes(self):
      """create anat, EPI, stim, etc. group boxes, and add to m2_vlayout"""

      if show_func_seq: print('=== F ===')

      self.gvars.gbox_dsets_A = self.group_box_dset_table('datasets A')
      self.gvars.gbox_dsets_B = self.group_box_dset_table('datasets B')
      self.gvars.gbox_ttest   = self.group_box_ttest()
      self.gvars.gbox_MEMA    = self.group_box_MEMA()

      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_dsets_A)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_dsets_B)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_ttest)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_MEMA)

   def group_box_ttest(self):
      """create a group box with layout
         ... for controlling uvars: dsetsA, labelA, betasA
      """

      prog = '3dttest++'
      gbox = QLIB.get_checked_group_box('%s options'%prog, ltype='G',
                                        cb_check=self.gbox_clicked)
      mainlayout = gbox.mainlayout

      label= QLIB.make_label('other options:',
                             'any other %s options to include in command'%prog)
      te = QtGui.QTextEdit()

      mainlayout.addWidget(label, 0, 0)
      mainlayout.addWidget(te, 0, 1, 3, 3)

      self.gvars.TE_ttest = te

      return gbox

   def group_box_MEMA(self):
      """create a group box with layout
         ... for controlling uvars: dsetsA, labelA, betasA
      """

      prog = '3dMEMA'
      gbox = QLIB.get_checked_group_box('%s options'%prog, ltype='G',
                                        cb_check=self.gbox_clicked)
      mainlayout = gbox.mainlayout

      label= QLIB.make_label('other options:',
                             'any other %s options to include in command'%prog)
      te = QtGui.QTextEdit()

      mainlayout.addWidget(label, 0, 0)
      mainlayout.addWidget(te, 0, 1, 3, 3)

      self.gvars.TE_MEMA = te

      return gbox

   def group_box_dset_table(self, title='datasets'):
      """create a group box with layout
            QPushB(get subject dsets)   QPushB(clear dsets)   QPushB(help)
            QLIB.DatasetTableWidget(sid, dset): common dir, wildard, #dsets

         save in gbox.PB_get, PB_clear, PB_help
                      STable_dsets
                      Label_num_entries
                      Label_top_dir
                      Label_group_name
         have gbox.frame
         for controlling uvars: dsetsA/B, labelA/B, betasA/B
      """
      gbox = QLIB.get_checked_group_box(title, cb_check=self.gbox_clicked)
      mainlayout = gbox.mainlayout

      # --------------------------------------------------
      # create buttons
      labels = ['get subj dsets', 'copy other table', 'clear', 'help']
      tips   = ['populate table with subject dataset list',
                'copy datasets/sids from other table',
                'delete all table entries', 'display help for this section']
      bwidget = QLIB.create_button_list_widget(labels, cb=self.CB_gbox_PushB,
                                               tips=tips)
      bwidget.bdict['help'].setIcon(self.style().standardIcon(
                            QtGui.QStyle.SP_MessageBoxQuestion))
      # assign buttons to main variables
      gbox.PB_get   = bwidget.bdict['get subj dsets']
      gbox.PB_copy  = bwidget.bdict['copy other table']
      gbox.PB_clear = bwidget.bdict['clear']
      gbox.PB_help  = bwidget.bdict['help']

      # create a DatasetTableWidget with those buttons
      dtw = QLIB.DatasetTableWidget(gbox, button_widgets=[bwidget],
                                          verb=self.verb)
      gbox.dset_table = dtw
      mainlayout.addWidget(dtw)

      # add a grid layout for group name, data index, t-stat index
      labels = ['set name (group or class)',
                'data index/label',
                't-stat index/label (MEMA)']
      tips = ['specify a group or category name (e.g. adults or houses)',
              'specify a sub-brick index or label for data volume',
              'specify index or label for t-stat volume (only for 3dMEMA)']
      widget = QLIB.create_label_line_grid(labels, tips, cb=self.CB_line_text)
      gbox.Line_name = widget.gvars.line_list[0]
      gbox.Line_dind = widget.gvars.line_list[1]
      gbox.Line_tind = widget.gvars.line_list[2]

      mainlayout.addWidget(widget)

      return gbox

   def init_gbox_viewable(self, box, view):
      if show_func_seq: print('=== I ===')

      box.setCheckable(True)
      box.setChecked(view)
      if view: box.frame.show()
      else:    box.frame.hide()

   def gbox_clicked(self):
      """show or hide frame, and set corresponding comment in title"""
      if show_func_seq: print('=== K ===')

      obj = self.sender()
      QLIB.toggle_checked_gbox(obj)

   def make_file_dialog(self, title, dir, filter):
      if show_func_seq: print('=== L ===')  # rcr - not called

      fileD = QtGui.QFileDialog(self)
      fileD.setWindowTitle(QtCore.QString(title))
      fileD.setFilter(QtCore.QString(filter))
      fileD.setDirectory(QtCore.QString(dir))
      return fileD

   def CB_checkbox(self):
      """call-back for any check boxes"""
      if show_func_seq: print('=== M ===')

      obj = self.sender()
      if obj == self.gvars.CB_paired:
         if obj.isChecked(): self.set_uvar('paired', 'yes')
         else:               self.set_uvar('paired', 'no')
      else: print("** CB_checkbox: unknown sender")

   def check_line_against_list(self, line, lname, tlist, tdef=0):
      """if line text is not in tlist, show error and reset to tlist[tdef]
      """
      if show_func_seq: print('=== N ===')

      txt = str(line.text())
      if txt not in tlist:
         reset_val = tlist[tdef]
         emesg = "%s '%s' not in %s\n(resetting to %s)" \
                 % (lname, txt, tlist, reset_val)
         QLIB.guiError('Error', emesg, self)
         line.setText(reset_val)
         return 1
      else: self.set_uvar(lname, txt)

      return 0

   def update_textLine_check(self, obj, text, attr, button_name, check_func):
      """check text against check_func(text, bname, 1, self, 1)
         - this warns the user on error
         - if valid, update the attr and textLine with the text
         - else, clear object and set focus to it (and return)

         return 1/0 to specify whether value was applied
      """
      
      if show_func_seq: print('=== O ===')

      # be sure we have a type to compare against for setting the text
      global g_LineEdittype
      if g_LineEdittype == None: g_LineEdittype = type(QtGui.QLineEdit())

      rtext = str(text) # in case of QString

      if check_func(rtext, button_name, 1, self, 1):
         if attr: self.set_uvar(attr, rtext)
         if type(obj) == g_LineEdittype: obj.setText(text)
         else: print('** update_textLine_check: not a LineEdit type')
         return 1
      else:
         # error, reset to previous attribute
         # obj.clear()
         if attr:
            try: obj.setText(self.uvars.val(attr))
            except: print("** UTC: failed to reset uvar attr '%s'" % attr)
         else:    obj.setText('')
         obj.setFocus()
         return 0

   def cb_show_script(self):

      if show_func_seq: print('=== P ===')

      if self.update_uvars_from_gui(warn=1): return

      # if we have a subject directory, make backup scripts
      if self.cvars.is_non_trivial_dir('proc_dir'):
         self.set_cvar('copy_scripts', 'yes')

      self.ttobj = LTT.TTest(cvars=self.cvars, uvars=self.uvars)
      nwarn, wstr = self.ttobj.get_warnings()
      status, mesg = self.ttobj.get_script()

      if status: # show errors
         self.update_error_window(mesg)
         return

      self.status = 1   # have script
      self.ttobj.write_script(self.uvars.val('script'))

      fname = self.ttobj.proc_dir_filename('file_proc')
      if not fname:
         self.update_warn_window('** no script to show')
         return
      elif not os.path.isfile(fname):
         self.update_warn_window('** script not found\n' \
                                 '   (should be file %s)' % fname)
         return

      self.update_result_window(title="Success!  ttest script:", fname=fname)
      if nwarn > 0: self.update_warn_window(wstr)

      # if we have a proc dir that exists, save the UA.py command there
      self.write_uber_command()

      self.gvars.act_exec_script.setEnabled(True)

   def cb_exec_script(self):

      if show_func_seq: print('=== Q ===')  # rcr - not called

      fmesg = ''
      if not self.ttobj:  fmesg = '** no script class for running script'
      elif self.status<1: fmesg = '** need to first generate processing script'
      if fmesg != '':
         QLIB.guiError('Error', fmesg, self)
         return

      # make sure there is a script to execute
      fname = self.ttobj.proc_dir_filename('file_proc')
      if not fname:
         self.update_AP_warn_window('** script file not set')
         return
      elif not os.path.isfile(fname):
         self.update_AP_warn_window('** script file not found: %s\n' % fname)
         return

      self.ttobj.nuke_old_results()    # get rid of any previous results

      # start a new window?
      if self.gvars.valid('SPW'):
         self.gvars.SPW.close()
         del(self.gvars.SPW)

      # actually run script, piping output if we have an output file
      sname = self.ttobj.rvars.file_proc
      oname = self.ttobj.rvars.output_proc
      command= 'tcsh -xef %s' % sname
      if oname: command = '%s |& tee %s' % (command, oname)
      self.gvars.SPW = QLIB.TcshCommandWindow(command,
                           dir=self.ttobj.cvars.proc_dir, parent=self)
      self.gvars.SPW.show()
      self.status = 2

   def write_uber_command(self):
      if show_func_seq: print('=== R ===')  # rcr - not called

      pdir = self.ttobj.cvars.val('proc_dir')
      if os.path.isdir(pdir):
         sstr = self.make_uber_command()
         UTIL.write_text_to_file('%s/.orig.cmd.utt' % pdir, sstr)

   def cb_command_window(self, cmd):
      if show_func_seq: print('=== S ===')  # rcr - not called

      print('++ python exec command: %s' % cmd)
      exec(cmd)

   def cb_show_py_command_window(self):
      """show the python command window"""
      if show_func_seq: print('=== T ===')  # rcr - not called

      if not self.gvars.valid('PCW'):
         self.gvars.PCW = QLIB.PyCommandWindow(callback=self.cb_command_window)
      self.gvars.PCW.show()
      self.gvars.PCW.raise_()

   def cb_show_command_window(self):
      """show the shell command window"""
      if show_func_seq: print('=== U ===')  # rcr - not called

      # start a new window?
      if not self.gvars.valid('CW'):
         self.gvars.CW = QLIB.ProcessWindow(parent=self)
      self.gvars.CW.show()
      self.gvars.CW.raise_()

   def CB_gbox_PushB(self):
      """these buttons are associated with anat/EPI/stim file group boxes
         - the sender (button) text must be unique"""

      if show_func_seq: print('=== V ===')

      try:
         sender = self.sender()
         text = str(sender.text())
      except:
         print('** CB_gbox_PushB: no text')
         return

      # program name
      if text[0:6] == 'prog: ':
         prog = text[6:]
         if self.verb > 1: print('++ setting program to %s' % prog)
         self.gvars.Line_program.setText(prog)
         self.uvars.program = prog

      elif text == 'browse':
         if sender == self.gvars.Pushb_mask:
            fname = QtGui.QFileDialog.getOpenFileName(self,
                   "load mask dataset", '',
                   "datasets (*.HEAD *.nii *.nii.gz);;all files (*)")
            if fname:
               self.update_textLine_check(self.gvars.Line_mask,
                           fname, 'mask', 'mask dset', QLIB.valid_as_filepath)

      elif text == 'get subj dsets':
         if sender == self.gvars.gbox_dsets_A.PB_get:
            chooser = QLIB.DsetChooser('Dset Chooser A',self.gvars.gbox_dsets_A)
            chooser.show()
            chooser.set_populate_cb(self.populate_table_A)
         elif sender == self.gvars.gbox_dsets_B.PB_get:
            chooser = QLIB.DsetChooser('Dset Chooser B',self.gvars.gbox_dsets_B)
            chooser.show()
            chooser.set_populate_cb(self.populate_table_B)
         else: print("** unknown source for PB '%s'" % text)

      elif text == 'copy other table':
         if sender == self.gvars.gbox_dsets_A.PB_copy:
            self.populate_table_A(None, None, copy=1)
         elif sender == self.gvars.gbox_dsets_B.PB_copy:
            self.populate_table_B(None, None, copy=1)
         else: print("** unknown source for PB '%s'" % text)

      elif text == 'clear':
         if sender == self.gvars.gbox_dsets_A.PB_clear:
            self.populate_table_A([])
         elif sender == self.gvars.gbox_dsets_B.PB_clear:
            self.populate_table_B([])
         else: print("** unknown source for PB '%s'" % text)

      elif text == 'help':
         if sender == self.gvars.gbox_dsets_A.PB_help or \
            sender == self.gvars.gbox_dsets_B.PB_help:
            self.update_help_window(g_help_dataset_tables,
                                    title='help for: datasets A/B')
         else: print("** unknown source for PB '%s'" % text)

      else: print("** unexpected button text: %s" % text)

   def populate_dset_table(self, table, dsets, sids=[], copytable=None):
      cstr = ''
      if copytable != None:
         cstr = ' (via copy)'
         data = copytable.get_data()
         if len(data) == 0:
            dsets = []
            sids = []
         else:
            if len(data[0]) != 2:
               print('** copy: table data not of len 2, data %s' % data)
               return 1
            dsets = [row[1] for row in data]
            sids  = [row[0] for row in data]

      if self.verb > 1:
         print('++ populating table with %d datasets%s' % (len(dsets), cstr))

      make_sids = 1
      if len(sids) == len(dsets):
         dlist = [[sids[ind],dsets[ind]] for ind in range(len(dsets))]
         make_sids = 0
      else: dlist = [['',d] for d in dsets]
      headers = ['subj ID', 'dataset']
      table.populate(dlist, col_headers=headers, sid_col=0, dset_col=1,
                     make_sids=make_sids)
      table.set_stretch_col(1)

   def populate_table_A(self, dsets, sids=[], copy=0):
      """populate self.gvars.gbox_dsets_A.dset_table from 'dsets'"""
      if self.verb > 1: print('++ populating table A ...')
      if copy: copytable = self.gvars.gbox_dsets_B.dset_table
      else:    copytable = None
      self.populate_dset_table(self.gvars.gbox_dsets_A.dset_table, dsets, sids,
                               copytable=copytable)

   def populate_table_B(self, dsets, sids=[], copy=0):
      """populate self.gvars.gbox_dsets_B.dset_table from 'dsets'
         copy : duplicate other table
      """
      if self.verb > 1: print('++ populating table B ...')
      if copy: copytable = self.gvars.gbox_dsets_A.dset_table
      else:    copytable = None
      self.populate_dset_table(self.gvars.gbox_dsets_B.dset_table, dsets, sids,
                               copytable=copytable)

   def resize_table(self, table, countLabel=None):
      if show_func_seq: print('=== W ===')  # rcr - not called

      nrows = table.rowCount()
      if nrows > 0: rheight = table.rowHeight(0)
      else        : rheight = 0
      if self.verb > 2: print('-- resize_table: using row height %d' % rheight)
      table.setAlternatingRowColors(True)
      table.setFixedHeight(self.max_table_size(nrows, rheight=rheight))
      table.resizeRowsToContents()
      QLIB.resize_table_cols(table)

      if countLabel != None: countLabel.setText('%d' % nrows)

   def remove_blank_table_rows(self, table, countLabel=None):
      if show_func_seq: print('=== X ===')  # rcr - not called


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

   def add_menu_bar(self):

      if show_func_seq: print('=== Z ===')

      # ----------------------------------------------------------------------
      # main process actions
      act1 = QLIB.createAction(self, "gen: ttest script",
        slot=self.cb_show_script,
        tip="generate processing script",
        icon=self.style().standardIcon(QtGui.QStyle.SP_FileDialogDetailedView))

      act2 = QLIB.createAction(self, "exec: ttest script",
        slot=self.cb_exec_script,
        tip="execute processing script",
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

      act1 = QLIB.createAction(self, "resulting ttest script",
        slot=self.cb_view,
        tip="display script created via GUI")

      act2 = QLIB.createAction(self, "output from ttest script",
        slot=self.cb_view,
        tip="display text output from execution of ttest script")

      act3 = QLIB.createAction(self,"view: uber_ttest.py command",
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
      actHelpUAS = QLIB.createAction(self,"uber_ttest.py -help'",
          slot=self.cb_help_main, shortcut=QtGui.QKeySequence.HelpContents,
          tip="help for uber_ttest.py")
      QLIB.addActions(self, self.gvars.MBar_help, [actHelpUAS])

      # add browse actions to browse sub-menu
      self.gvars.Menu_browse = self.gvars.MBar_help.addMenu("&Browse")
      act1 = QLIB.createAction(self,"web: all AFNI programs",
          slot=self.cb_help_browse, tip="browse AFNI program help")
      act2 = QLIB.createAction(self,"web: 3dttest++.py help",
          slot=self.cb_help_browse, tip="browse 3dttest++ help")
      act3 = QLIB.createAction(self,"web: 3dMEMA help",
          slot=self.cb_help_browse, tip="browse 3dMEMA help")
      act4 = QLIB.createAction(self,"web: AFNI Message Board",
          slot=self.cb_help_browse, tip="browse Message Board")

      QLIB.addActions(self, self.gvars.Menu_browse, [act1, act2, act3, act4])

      self.gvars.act_browse_all_progs  = act1
      self.gvars.act_browse_ttest_help = act2
      self.gvars.act_browse_mema_help  = act3
      self.gvars.act_browse_MB         = act4

      actHelpAbout = QLIB.createAction(self,"about uber_ttest.py",
          slot=self.cb_help_about, tip="about uber_ttest.py")
      QLIB.addActions(self, self.gvars.MBar_help, [actHelpAbout])

   def add_tool_bar(self):
      if show_func_seq: print('=== xA ===')

      self.gvars.toolbar = self.addToolBar('uber_ttest.py')

      self.gvars.toolbar.addAction(self.gvars.act_show_script)
      self.gvars.toolbar.addAction(self.gvars.act_exec_script)

   def add_status_bar(self):
      if show_func_seq: print('=== xB ===')

      self.gvars.statusbar = self.statusBar()
      self.gvars.statusbar.showMessage("Ready")

   def make_extra_widgets(self):
      """create - ULIB.TextWindow for help messages, scrip and warnings
                - gvars.browser for web links"""

      if show_func_seq: print('=== xC ===')

      # text window (if None (for now), new windows will be created)
      # self.gvars.Text_help = None
      self.gvars.Text_help = QLIB.TextWindow(parent=self)
      self.gvars.Text_script = QLIB.TextWindow(parent=self)

      # note whether we have a browser via import
      self.gvars.browser = None
      try: 
         import webbrowser
         self.gvars.browser = webbrowser
         if self.verb > 1: print('++ have browser')
      except:
         if self.verb > 1: print('-- NO browser')

   def open_web_site(self, site):
      if show_func_seq: print('=== xD ===')

      if self.gvars.browser == None:
         QLIB.guiWarning('Error', 'no browser to use for site: %s'%site,self)
      else: self.gvars.browser.open(site)

   def update_result_window(self, win=None, text='', title='', fname=''):
      """default window is Text_script
         - if fname, read file
           else use text"""
      if show_func_seq: print('=== xE ===')  # rcr - not called

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
      if show_func_seq: print('=== xF ===')  # rcr - not called

      QLIB.guiError(title, text, self)

   def update_warn_window(self, text, title='WARNING'):
      if show_func_seq: print('=== xG ===')

      QLIB.guiWarning(title, text, self)

   def update_help_window(self, text, title=''):
      if show_func_seq: print('=== xH ===')

      # if no permanent window, make a new one each time
      if self.gvars.Text_help == None:
         if self.verb > 2: print('++ opening new TextWindow')
         win = QLIB.TextWindow(text=text, title=title, parent=self)
         win.setAttribute(QtCore.Qt.WA_DeleteOnClose)
         win.show()
      else:
         if title != '': self.gvars.Text_help.setWindowTitle(title)
         self.gvars.Text_help.editor.setText(text)
         self.gvars.Text_help.show()

   def cb_help_main(self):
      """display helpstr_usubj_gui in Text_Help window"""
      if show_func_seq: print('=== xI ===')

      self.update_help_window(LTT.helpstr_gui,
                              title='uber_ttest.py: GUI help')

   def cb_help_about(self):
      """display version info in Text_Help window"""
      if show_func_seq: print('=== xJ ===')

      text = "uber_ttest.py, version %s" % LTT.g_version
      QLIB.guiMessage('about uber_ttest.py', text, self)

   def cb_help_browse(self):
      obj = self.sender()
      if   obj == self.gvars.act_browse_all_progs:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/index.html')
      elif obj == self.gvars.act_browse_ttest_help:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/3dttest++.html')
      elif obj == self.gvars.act_browse_mema_help:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/3dMEMA.html')
      elif obj == self.gvars.act_browse_MB:
         self.open_web_site('https://afni.nimh.nih.gov/afni/community/board')
      else: print('** cb_help_browse: invalid sender')

   def update_uvars_from_gui(self, warn=0):
      """set what we can, if warn, report error
         note that most uvars are Lines, and are updated upon edit
         return 0 on success, 1 on error"""
      if show_func_seq: print('=== xK ===')

      # any first variables to require?
      if self.uvars.is_empty('program'):
         if warn: QLIB.guiError('Error', "** program name must be set", self)
         return 1

      # then process tables
      if self.update_uvars_from_tables(): return 1

      # ---------------------------------------------------------------
      # then any other vars (most are Lines, which are applied on edit)
      
      # extra tt and MM options:
      otext = str(self.gvars.TE_ttest.toPlainText())
      self.uvars.tt_options = otext.split()

      otext = str(self.gvars.TE_MEMA.toPlainText())
      self.uvars.MM_options = otext.split()

      if self.set_pdir:
         # proc dir should read: tool_results/tool.0001.align_test
         pdir =  SUBJ.get_def_tool_path(self.uvars.program,
                        top_dir='group_results', prefix='test',
                        keep_if_missing=self.cvars.val('results_dir'))
         if self.set_cvar('proc_dir', pdir):
            print('-- setting proc_dir to %s' % pdir)

      self.gvars.act_exec_script.setEnabled(False)

      return 0

   def update_uvars_from_tables(self):
      """fill dsets_A/B, sids_A/B from the tables
         return 0 on success
      """
      data = self.gvars.gbox_dsets_A.dset_table.get_data()
      if len(data) == 0:
         self.uvars.dsets_A = []
         self.uvars.sids_A = []
      else:
         if len(data[0]) != 2:
            print('** table A data not of len 2, data %s' % data)
            return 1
         self.uvars.dsets_A = [row[1] for row in data]
         self.uvars.sids_A  = [row[0] for row in data]

      data = self.gvars.gbox_dsets_B.dset_table.get_data()
      if len(data) == 0:
         self.uvars.dsets_B = []
         self.uvars.sids_B = []
      else:
         if len(data[0]) != 2:
            print('** table B data not of len 2, data %s' % data)
            return 1
         self.uvars.dsets_B = [row[1] for row in data]
         self.uvars.sids_B  = [row[0] for row in data]

      return 0

   def cb_clear_options(self):
      """set uvars from defaults and redisplay GUI
         EXCEPT: keep dataset fields from subject"""
      if show_func_seq: print('=== xL ===')


      uvars = VO.VarsObject()
      for atr in ['anat', 'epi', 'epi_base']:
         uvars.set_var(atr, self.uvars.val(atr))
      
      self.reset_vars(uvars=uvars, set_pdir=self.set_pdir)

   def cb_clear_fields(self):
      """set uvars from defaults and redisplay GUI"""
      if show_func_seq: print('=== xM ===')  # rcr - not called

      self.reset_vars(set_pdir=self.set_pdir)

   def cb_view(self):
      """create permanent windows with given text"""
      if show_func_seq: print('=== xN ===')

      obj = self.sender()

      if obj == self.gvars.act_view_script:
         self.show_static_file('file_proc', 'ttest script:')

      elif obj == self.gvars.act_view_output:
         self.show_static_file('output_proc', 'script output:')

      elif obj == self.gvars.act_view_cmd:
         sstr = self.make_uber_command()
         QLIB.static_TextWindow(title='corresp. uber_ttest.py command',
                                text=sstr, parent=self)

      elif obj == self.gvars.act_view_uvars:
         sstr = self.uvars.make_show_str('current ttest test', name=0)
         QLIB.static_TextWindow(title='user vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_cvars:
         sstr = self.cvars.make_show_str('control vars', name=0, all=0)
         QLIB.static_TextWindow(title='control vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_gvars:
         sstr = self.gvars.make_show_str('GUI vars', name=0, all=1)
         QLIB.static_TextWindow(title='GUI vars', text=sstr, parent=self)

      else: print('** unhandled object in cb_view')

   def make_uber_command(self):
      """generate a script that would invoke the uber_ttest.py interface
         with control and user vars set (and so fields filled)

         Put any key elements in quotes:
            basis functions
            gltsym
      """
      if show_func_seq: print('=== xO ===')


      # first apply subject variables
      self.update_uvars_from_gui()

      cmd = 'uber_ttest.py'

      # apply each uvar with -uvar option
      prefix = ' \\\n    -uvar '     # append before next command
      for atr in self.uvars.attributes():
         if atr == 'name': continue     # skip
         if self.uvars.vals_are_equal(atr, LTT.g_udef_strs): continue
         # show this one
         val = self.uvars.val(atr)
         
         # special cases first: stim_basis, gltsym
         #if atr == 'gltsym':            # special case
         #   val = ["'%s'" % v for v in val]
         #elif atr == 'stim_basis':      # special case
         #   val = ["'%s'" % v for v in val]
         #   cmd += (prefix + '%s %s' % (atr, ' '.join(val)))

         # some options might need quotes


         if self.uvars.has_simple_type(atr):
            val = UTIL.quotize_list([val], quote_chars='#%<>')[0]
            cmd += (prefix + '%s %s' % (atr, val))
         elif type(val) == list:
            val = UTIL.quotize_list(val, quote_chars='#%<>')
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))
         else:
            print('** make_uber_command: bad attr %s' % atr)

      return UTIL.add_line_wrappers(cmd + '\n')

   def show_static_file(self, var_name, var_desc):
      """display the var according to var_name
         - use description in var_desc if any error"""
      if show_func_seq: print('=== xP ===')

      if self.ttobj == None:        # check for generated command
         self.update_warn_window('** script must be generated first')
         return
      file = self.ttobj.proc_dir_filename(var_name)
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
      if show_func_seq: print('=== xQ ===')


      if not self.cvars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print("++ set_cvar: update [%s] to '%s'"%(name,newval))

      # even for cvars, since proc_dir is part of script
      self.gvars.act_exec_script.setEnabled(False)

   def set_uvar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if show_func_seq: print('=== xR ===')

      if not self.uvars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print("++ set_uvar: update [%s] to '%s'"%(name,newval))

      self.gvars.act_exec_script.setEnabled(False)

   def apply_cvars(self, cvars=None):
      """apply to the cvars object

         first init to defaults
         if cvars is passed, make further updates"""

      if show_func_seq: print('=== xS ===')

      # merge with current vars and apply everything to GUI
      self.cvars.merge(cvars)
      self.set_cvar('verb', str(self.verb))     # to pass verb along

      if self.verb > 2: self.uvars.show("post reset control vars")

   def apply_uvars(self, uvars=None):
      """apply to the uvars object and to the gui

         first init to defaults
         if uvars is passed, make further updates"""

      if show_func_seq: print('=== xT ===')

      # merge with current vars and apply everything to GUI
      self.uvars.merge(uvars)
      for var in self.uvars.attributes():
         self.apply_uvar_in_gui(var)

      if self.verb > 2: self.uvars.show("post reset user vars")

   def apply_uvar_in_gui(self, uvar):
      """this is a single interface to apply any user variable in the GUI

         if a variable is not handled in the interface, ignore it

         return 1 if processed
      """

      if show_func_seq: print('=== xU ===')

      if not self.uvars.valid(uvar):
         print('** AUIG: invalid uvar %s' % uvar)
         return 0

      val = self.uvars.val(uvar)

      if self.verb > 2:
         print("++ applying uvar '%s' in GUI as %s" % (uvar, val))

      rv = 1
      # top of GUI
      if   uvar == 'program': self.gvars.Line_program.setText(val)
      elif uvar == 'script' : self.gvars.Line_script.setText(val)
      elif uvar == 'prefix' : self.gvars.Line_prefix.setText(val)
      elif uvar == 'mask'   : self.gvars.Line_mask.setText(val)
      elif uvar == 'paired' : self.gvars.CB_paired.setChecked(val=='yes')

      # datasets A
      elif uvar == 'dsets_A':
         sids = self.uvars.val('sids_A')
         self.populate_table_A(val, sids=sids)
      elif uvar == 'sids_A':     pass  # do along with dsets
      elif uvar == 'set_name_A': self.gvars.gbox_dsets_A.Line_name.setText(val)
      elif uvar == 'beta_A':     self.gvars.gbox_dsets_A.Line_dind.setText(val)
      elif uvar == 'tstat_A':    self.gvars.gbox_dsets_A.Line_tind.setText(val)

      # datasets B
      elif uvar == 'dsets_B':
         sids = self.uvars.val('sids_B')
         self.populate_table_B(val, sids=sids)
      elif uvar == 'sids_B':     pass  # do along with dsets
      elif uvar == 'set_name_B': self.gvars.gbox_dsets_B.Line_name.setText(val)
      elif uvar == 'beta_B':     self.gvars.gbox_dsets_B.Line_dind.setText(val)
      elif uvar == 'tstat_B':    self.gvars.gbox_dsets_B.Line_tind.setText(val)

      # extra options for ttest and MEMA
      elif uvar == 'tt_options':self.gvars.TE_ttest.setPlainText(' '.join(val))
      elif uvar == 'MM_options':self.gvars.TE_MEMA.setPlainText(' '.join(val))

      else:
         if self.verb > 1: print('** apply_uvar_in_gui: unhandled %s' % uvar)
         rv = 0

      if rv and self.verb > 2: print('++ apply_uvar_in_gui: process %s' % uvar)

      return rv

# --- post MainWindow class

# ===========================================================================
# help strings


g_help_eg = """
   goals:

   description:

"""

g_help_dataset_tables = """
Populate the table with a list of dataset (which will hopefully include
associated subject IDs, automatically).  Then specify the set name (group
or condition), the associated data index or label for the beta, and one
for the t-stat (if using 3dMEMA).


1. Populate the datasets table

   a. click on 'get subj dsets'

      This will open an interface that should allow one to get
      the desired list.  Please see the included help for details.

   b. click on 'copy other table'

      This will populate the table with the same data in the other
      table (between A and B).  This is a common choice in a paired
      test, where the 2 types of betas come from different sub-bricks
      of the same datasets.

      At this point, the information fields 'common directory,
      'wildcard form' and 'dataset count' should be automatically
      filled.

2. Assign a name for this list of datasets

   The set name could be the group name for this list of subjects,
   or perhaps the condition or class (likely in the case of a paired
   test).  For example, groups might be 'horses' or 'zombies', while
   classes/conditions might be 'houses' or 'faces'.

   With the AFNI_data6/group_results data, one might use Vrel, for
   the 'visual reliable' condition.

3. Assign a data index or label

   Specify a sub-brick index or label for the particular volume that
   should be used for this set.  Using AFNI_data6/group_results as an
   example, one might use index '0' or label 'Vrel#0_Coef' (without
   the quotes) to specify the Vrel beta weight.

4. If using 3dMEMA, assign a t-stat index or label

   Specify the sub-brick of the t-statistic that corresonds to the
   data index, above (presumably the beta weight).  For example, use
   index '1' or label 'Vrel#0_Tstat' for the REML datasets under the
   AFNI_data6/group_results directory.
"""

# end: help strings
# ===========================================================================

def main():

   print('** this is not a main program')
   return 1

if __name__ == '__main__':
   sys.exit(main())
