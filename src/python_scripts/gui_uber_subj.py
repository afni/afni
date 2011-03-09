#!/usr/bin/python

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
import lib_uber_subject as USUBJ
import lib_qt_gui as QLIB

# allow users to play with style
g_styles = ["windows", "motif", "cde", "plastique", "cleanlooks"]
g_style_index = 0
g_style_index_def = 4

g_LineEdittype = None                   # set this type later

# default subj dir should read like: subject_results/group_A/SUBJ
g_def_subj_parent  = 'subject_results'

# ======================================================================
# main module class
class SingleSubjectWindow(QtGui.QMainWindow):
   """class for a single subject main window

         parent         parent Widget, can be None if this is main Widget
         subj_vars      VarsObject for init of gui fields
         ctrl_vars      VarsObject for init of control vars
         set_sdir       upon GUI exec, set cvars.subj_dir bases on sid
         verb           verbose level, default of 1
   """

   def __init__(self, parent=None, subj_vars=None, ctrl_vars=None, 
                set_sdir=0, verb=-1):
      super(SingleSubjectWindow, self).__init__(parent)

      # ------------------------------
      # init main vars structs
      self.verb   = verb
      self.apsubj = None        # AP_Subject class element
      self.gvars  = SUBJ.VarsObject('uber_subject gui vars')

      # initialize the subject variables as empty, update at the end
      self.svars  = USUBJ.g_subj_defs.copy('uber_subject subject vars')
      self.cvars  = USUBJ.g_ctrl_defs.copy('uber_subject control vars')
      if self.verb < 0: self.verb = self.cvars.verb
      self.set_sdir = set_sdir

      # ------------------------------
      # L1 - main menubar and layout
      self.add_menu_bar()
      self.add_tool_bar()
      self.add_status_bar()
      self.gvars.Wcentral = QtGui.QWidget()
      mainlayout = QtGui.QVBoxLayout(self.gvars.Wcentral)

      # ------------------------------
      # L2 - make Group Box and Vertical Layout; add to main Layout
      self.make_l2_widgets()

      # ------------------------------
      # L3 - fill m2_vlayout with group boxes
      self.make_l3_group_boxes()

      # ------------------------------------------------------------
      # finish level 2 and then level 1
      mainlayout.addWidget(self.gvars.gbox_general)
      mainlayout.addWidget(self.gvars.m2_scroll)

      # ------------------------------
      # save this for last to ensure it is all visible
      self.gvars.m2_scroll.setWidget(self.gvars.m2_gbox_inputs)

      self.gvars.Wcentral.setMinimumSize(150, 200)
      self.gvars.Wcentral.setLayout(mainlayout)
      self.setCentralWidget(self.gvars.Wcentral)

      self.update_all_gbox_styles()

      self.make_extra_widgets()

      self.gvars.style = g_styles[g_style_index_def]

      # widgets are done, so apply pass subject vars
      self.reset_vars(svars=subj_vars, cvars=ctrl_vars,
                      set_sdir=set_sdir, verb=verb)

      # ap_status : 0 = must create ap command, 1 = have ap, need proc script,
      #             2 = have proc script, ready to execute
      # (meaning user must have first generated (and viewed) the command)
      self.gvars.ap_status = 0

      if self.verb > 1: print '-- finished Single Subject Dialog setup'

   def reset_vars(self, svars=None, cvars=None, set_sdir=0, verb=-1):
      """replace old svars/cvars with new"""

      if self.verb < 0: self.verb = self.cvars.verb
      self.set_sdir = set_sdir

      del(self.svars)
      del(self.cvars)

      self.svars = USUBJ.g_subj_defs.copy()
      self.cvars = USUBJ.g_ctrl_defs.copy()

      self.apply_svars(svars)
      self.apply_cvars(cvars)

   def make_l2_widgets(self):
      """create 'general subject info' box and scroll area for the
         rest of the inputs

         QGroupBox(general)
         QScrollArea
            QGroupBox(input data and options) with VBox layout
               elsewhere:
                  QGroupBox(anat)
                  QGroupBox(epi)
                  QGroupBox(stim)
                  ...
      """

      self.make_l2_group_box()  # for general subject info

      self.gvars.m2_scroll = QtGui.QScrollArea()
      self.gvars.m2_scroll.setWidgetResizable(True)

      # create a containing GroupBox for everything in m2_scroll
      gbox = self.get_styled_group_box("input data and options")

      # the layout for the 'input datasets' QGroupBox is vertical
      self.gvars.m2_vlayout = QtGui.QVBoxLayout(gbox)
      gbox.setLayout(self.gvars.m2_vlayout)

      self.gvars.m2_gbox_inputs = gbox

   def update_all_gbox_styles(self):
      for gbox in [ self.gvars.gbox_general, self.gvars.gbox_anat,
                    self.gvars.gbox_epi,     self.gvars.gbox_stim ]:
         self.update_gbox_style(gbox)

   def update_gbox_style(self, gbox):

      # rcr - maybe consider these later

      # color:                      font
      # background-color:           entire box, and child widgets
      # selection-color:            font of selected characters
      # selection-background-color: background of selected characters

      return

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
            HBox: Label(sid) LineEdit()  Label(gid) LineEdit()

         for controlling sujbect vars: sid, gid
      """
      gbox = self.get_styled_group_box("general subject info")

      mainlayout = QtGui.QVBoxLayout()

      # --------------------------------------------------
      # Widget/HBox: subject and group ID fields
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()

      # add subject ID stuff, init with sid
      label = QtGui.QLabel("subject ID")
      self.gvars.Line_sid = QtGui.QLineEdit()
      self.gvars.Line_sid.setText(self.svars.sid)
      layout.addWidget(label)
      layout.addWidget(self.gvars.Line_sid)

      # add group ID stuff, init with sid
      label = QtGui.QLabel("group ID")
      self.gvars.Line_gid = QtGui.QLineEdit()
      self.gvars.Line_gid.setText(self.svars.gid)
      layout.addWidget(label)
      layout.addWidget(self.gvars.Line_gid)

      # note callbacks for buttons
      self.gvars.Line_sid.editingFinished.connect(self.CB_line_text)
      self.gvars.Line_gid.editingFinished.connect(self.CB_line_text)

      # and set layout
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # and put main widgets into main VBox layout
      gbox.setLayout(mainlayout)
      self.gvars.gbox_general = gbox

   def CB_line_text(self):
      """call-back for text updates in the level 3 gbox"""
      obj = self.sender()
      if obj == self.gvars.Line_sid:
         self.update_textLine_check(obj, obj.text(), 'sid', 'subject ID',
                                    QLIB.valid_as_identifier)
      elif obj == self.gvars.Line_gid:
         self.update_textLine_check(obj, obj.text(), 'gid', 'group ID',
                                    QLIB.valid_as_identifier)
      elif obj == self.gvars.Line_anat:
         self.update_textLine_check(obj, obj.text(), 'anat', 'anatomical dset',
                                    QLIB.valid_as_filepath)
      elif obj == self.gvars.Line_apply_basis:
         self.update_basis_function(obj.text())

      elif obj == self.gvars.Line_tcat_nfirst:
         self.update_textLine_check(obj, obj.text(), 'tcat_nfirst',
                                    'first TRs to remove', QLIB.valid_as_int)

      # maybe we need to write a valid_in_list validator...
      elif obj == self.gvars.Line_volreg_base:
         text = str(obj.text())
         if text == '' or text in USUBJ.g_vreg_base_list:
            self.set_svar('volreg_base', text)
         else: # reset to previous value
            self.gvars.Line_volreg_base.setText(self.svars.volreg_base)
            QLIB.guiWarning("Error: invalid volreg base",
                            "base '%s' not in %s\n\n" \
                            % (text, ', '.join(USUBJ.g_vreg_base_list)), obj)
          
      elif obj == self.gvars.Line_motion_limit:

         self.update_textLine_check(obj, obj.text(), 'motion_limit',
                                    'motion censor limit', QLIB.valid_as_float)

      else: print '** CB_line_text: unknown sender'

   def make_l3_group_boxes(self):
      """create anat, EPI, stim, etc. group boxes, and add to m2_vlayout"""

      self.gvars.gbox_anat = self.group_box_anat()
      self.gvars.gbox_epi  = self.group_box_epi()
      self.gvars.gbox_stim = self.group_box_stim()
      self.gvars.gbox_expected = self.group_box_expected()

      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_anat)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_epi)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_stim)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_expected)

   def group_box_expected(self):
      """create a group box with a VBox layout:
         for controlling sujbect vars: tcat_nfirst, volreg_base, motion_limit
      """

      gbox = self.get_styled_group_box("expected options")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      gbox.setCheckable(1)
      gbox.toggled.connect(self.gbox_toggle_frame)

      layout = QtGui.QGridLayout(frame)         # now a child of frame

      # --------------------------------------------------
      # tcat_nfirst
      label = QtGui.QLabel("first TRs to remove (per run)")
      label.setToolTip("the number of pre-steady state TRs to remove")
      self.gvars.Line_tcat_nfirst = QtGui.QLineEdit()
      self.gvars.Line_tcat_nfirst.setText('%d'%self.svars.tcat_nfirst)
      self.gvars.Line_tcat_nfirst.editingFinished.connect(self.CB_line_text)

      layout.addWidget(label, 0, 0)
      layout.addWidget(self.gvars.Line_tcat_nfirst, 0, 2)

      # --------------------------------------------------
      # volreg_base
      label = QtGui.QLabel("volume registration base")
      label.setToolTip("EPI volume to use as registration base")
      self.gvars.Line_volreg_base = QtGui.QLineEdit()
      # choose button
      blist = ['vr base: %s' % base for base in USUBJ.g_vreg_base_list]
      pbut = QLIB.create_menu_button(frame, "choose", blist,
                call_back=self.CB_gbox_PushB)
      self.gvars.Line_volreg_base.setText(self.svars.volreg_base)
      self.gvars.Line_volreg_base.editingFinished.connect(self.CB_line_text)

      layout.addWidget(label, 1, 0)
      layout.addWidget(pbut, 1, 1)
      layout.addWidget(self.gvars.Line_volreg_base, 1, 2)

      # --------------------------------------------------
      # motion_limit
      label = QtGui.QLabel("motion censor limit (per TR)")
      label.setToolTip("censor TRs with motion exceeding this mm distance")
      self.gvars.Line_motion_limit = QtGui.QLineEdit()
      self.gvars.Line_motion_limit.setText('%g'%self.svars.motion_limit)
      self.gvars.Line_motion_limit.editingFinished.connect(self.CB_line_text)
      layout.addWidget(label, 2, 0)
      layout.addWidget(self.gvars.Line_motion_limit, 2, 2)

      frame.setLayout(layout)
      glayout.addWidget(frame)
      gbox.setLayout(glayout)
      return gbox

   def group_box_anat(self):
      """create a group box with a VBox layout:
                HBox:  QPushB(browse anat)  QPushB(clear anat)
                LineEdit(anatomical dataset)
                QCheckBox (inlude tlrc)

         for controlling sujbect vars: anat, get_tlrc
      """

      gbox = self.get_styled_group_box("anatomical dataset")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      gbox.setCheckable(1)
      gbox.toggled.connect(self.gbox_toggle_frame)

      layout = QtGui.QVBoxLayout(frame) # now a child of frame

      # create an HBox Widget with 2 buttons
      labels = ['browse anat', 'clear anat', 'help: anat']
      bwidget = QLIB.create_button_list_widget(labels, self.CB_gbox_PushB,
                        dir=0, hstr=0)
      gbox.blist = bwidget.blist
      gbox.blist[2].setIcon(self.style().standardIcon(
                QtGui.QStyle.SP_MessageBoxQuestion))
      layout.addWidget(bwidget)

      # create the anat file browsing dialog
      gbox.FileD = self.make_file_dialog("load anatomical dataset", "",
                     "*.HEAD;;*.nii;;*")
                     #".HEAD files (*.HEAD);;.nii files (*.nii);;all files (*)")

      # add a line for the anat name, init to anat
      self.gvars.Line_anat = QtGui.QLineEdit()
      self.gvars.Line_anat.setText(self.svars.anat)
      self.gvars.Line_anat.editingFinished.connect(self.CB_line_text)
      layout.addWidget(self.gvars.Line_anat)

      # add a checkbox for including tlrc, init with get_tlrc
      gbox.checkBox = QtGui.QCheckBox("include copy of anat+tlrc")
      gbox.checkBox.setChecked(self.svars.get_tlrc)
      gbox.checkBox.clicked.connect(self.CB_checkbox)
      layout.addWidget(gbox.checkBox)

      frame.setLayout(layout)
      glayout.addWidget(frame)
      gbox.setLayout(glayout)
      return gbox

   def gbox_toggle_frame(self):
      obj = self.sender()
      if obj.isChecked(): obj.frame.show()
      else: obj.frame.hide()

   def group_box_epi(self):
      """create a group box with a VBox layout for EPI datasets:
                HBox:  QPushB(browse EPI)  QPushB(clear EPI)
                QTableWidget(EPI datasets)
                QWidget, 3x2 Grid Layout
                    QLabel(EPI directory)   QLabel(shadow text)
                    QLabel(wildcard form)   QLabel(shadow text)
                    QLabel(dataset count)   QLabel(shadow text)
                QCheckBox (use wildcard form)

         for controlling sujbect vars: epi, epi_wildcard
      """

      # --------------------------------------------------
      gbox = self.get_styled_group_box("EPI datasets")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      glayout.addWidget(frame)
      gbox.frame = frame
      gbox.setCheckable(1)
      gbox.toggled.connect(self.gbox_toggle_frame)

      mainlayout = QtGui.QVBoxLayout(frame)     # now a child of frame

      # --------------------------------------------------
      # create an HBox Widget with 2 buttons
      labels = ['browse EPI', 'clear EPI', 'help: EPI']
      bwidget = QLIB.create_button_list_widget(labels, self.CB_gbox_PushB, 0)
      gbox.blist = bwidget.blist
      gbox.blist[2].setIcon(self.style().standardIcon(
                            QtGui.QStyle.SP_MessageBoxQuestion))
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a table for the epi names, init to epi
      self.make_epi_table(gbox)
      mainlayout.addWidget(self.gvars.Table_epi)

      # --------------------------------------------------
      # add lines for the number of datasets and wildcard form
      bwidget = QtGui.QWidget()
      # layout = QtGui.QHBoxLayout()
      layout = QtGui.QGridLayout()

      nlabel, tlabel = QLIB.create_display_label_pair('EPI directory', '')
      layout.addWidget(nlabel, 0, 0)
      layout.addWidget(tlabel, 0, 1)
      self.gvars.Label_epi_dir = tlabel

      nlabel, tlabel = QLIB.create_display_label_pair('wildcard form', '')
      layout.addWidget(nlabel, 1, 0)
      layout.addWidget(tlabel, 1, 1)
      self.gvars.Label_epi_wildcard = tlabel

      nlabel, tlabel = QLIB.create_display_label_pair('dataset count',
                                        '%s' % len(self.svars.epi))
      layout.addWidget(nlabel, 2, 0)
      layout.addWidget(tlabel, 2, 1)
      self.gvars.Label_epi_ndsets = tlabel

      # basically fix column 0 and let column 1 grow
      layout.setColumnStretch(0, 1)
      layout.setColumnStretch(1, 20)

      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a checkbox for use wildcard form
      gbox.checkBox_wildcard = QtGui.QCheckBox("use wildcard form")
      gbox.checkBox_wildcard.clicked.connect(self.CB_checkbox)
      gbox.checkBox_wildcard.setChecked(self.svars.epi_wildcard)
      mainlayout.addWidget(gbox.checkBox_wildcard)

      # --------------------------------------------------
      frame.setLayout(mainlayout)
      return gbox

   def group_box_stim(self):
      """create a group box with a VBox layout for stimulus timing files:
                HBox:  QPushB(browse stim)  QPushB(clear stim)
                QTableWidget(stimulus timing files)
                QWidget, 3x2 Grid Layout
                    QLabel(stim directory)  QLabel(shadow text)
                    QLabel(wildcard form)   QLabel(shadow text)
                    QLabel(dataset count)   QLabel(shadow text)
                QCheckBox (use wildcard form)

         for controlling sujbect vars: stim, stim_wildcard
      """

      # --------------------------------------------------
      gbox = self.get_styled_group_box("stimulus timing files")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      glayout.addWidget(frame)
      gbox.frame = frame
      gbox.setCheckable(1)
      gbox.toggled.connect(self.gbox_toggle_frame)

      mainlayout = QtGui.QVBoxLayout(frame)     # now a child of frame

      # --------------------------------------------------
      # create an HBox Widget with 2 buttons
      labels = ['browse stim', 'clear stim', 'help: stim']
      bwidget = QLIB.create_button_list_widget(labels, self.CB_gbox_PushB, 0)
      gbox.blist = bwidget.blist
      gbox.blist[2].setIcon(self.style().standardIcon(
                QtGui.QStyle.SP_MessageBoxQuestion))
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a table for the stim names, init to stim
      self.make_stim_table(gbox)
      mainlayout.addWidget(self.gvars.Table_stim)

      # --------------------------------------------------
      # add lines for the number of datasets and wildcard form
      bwidget = QtGui.QWidget()
      layout = QtGui.QGridLayout()

      nlabel, tlabel = QLIB.create_display_label_pair('stim directory', '')
      layout.addWidget(nlabel, 0, 0)
      layout.addWidget(tlabel, 0, 1)
      self.gvars.Label_stim_dir = tlabel

      nlabel, tlabel = QLIB.create_display_label_pair('wildcard form', '')
      layout.addWidget(nlabel, 1, 0)
      layout.addWidget(tlabel, 1, 1)
      self.gvars.Label_stim_wildcard = tlabel

      nlabel, tlabel = QLIB.create_display_label_pair('stim file count',
                                            '%s' % len(self.svars.stim))
      layout.addWidget(nlabel, 2, 0)
      layout.addWidget(tlabel, 2, 1)
      self.gvars.Label_stim_ndsets = tlabel

      # basically fix column 0 and let column 1 grow
      layout.setColumnStretch(0, 1)
      layout.setColumnStretch(1, 20)

      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a new line for setting basis functions:
      # Label  PushButton.Menu   LineEdit
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()

      nlabel = QtGui.QLabel("init basis funcs:")
      nlabel.setToolTip("initialization for all stim files")
      layout.addWidget(nlabel)

      blist = ['GAM', 'BLOCK(5,1)', 'BLOCK(5)', 'TENT(0,15,6)', 'SPMG2']
      blist = ['basis: %s'%basis for basis in blist]
      pbut = QLIB.create_menu_button(bwidget, "choose", blist,
                call_back=self.CB_gbox_PushB)
      layout.addWidget(pbut)

      self.gvars.Line_apply_basis  = QtGui.QLineEdit()
      if len(self.svars.stim_basis) > 0: basis = self.svars.stim_basis[0]
      else:                              basis = ''
      self.gvars.Line_apply_basis.setText(basis)
      self.gvars.Line_apply_basis.editingFinished.connect(self.CB_line_text)
      layout.addWidget(self.gvars.Line_apply_basis)

      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a checkbox for use wildcard form
      gbox.checkBox_wildcard = QtGui.QCheckBox("use wildcard form")
      gbox.checkBox_wildcard.clicked.connect(self.CB_checkbox)
      gbox.checkBox_wildcard.setChecked(self.svars.stim_wildcard)
      mainlayout.addWidget(gbox.checkBox_wildcard)

      # --------------------------------------------------
      frame.setLayout(mainlayout)

      return gbox

   def make_file_dialog(self, title, dir, filter):
      fileD = QtGui.QFileDialog(self)
      fileD.setWindowTitle(QtCore.QString(title))
      fileD.setFilter(QtCore.QString(filter))
      fileD.setDirectory(QtCore.QString(dir))
      return fileD

   def make_epi_table(self, parent=None):
      """init table from epi array
         - only update epi array on directory scan and 'update AP command'
      """
      col_heads = ['scan index', 'EPI dataset']  # column headings
      stretch_cols = [1]                         # columns that should stretch
      nrows = len(self.svars.epi)                # one row per EPI dataset
      ncols = len(col_heads)

      table = QtGui.QTableWidget(nrows, ncols)

      table.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
      table.setHorizontalHeaderLabels(col_heads)

      for col in stretch_cols:
         if col < 0 or col >= ncols:
            print ("** Table_epi: stretch_cols outside range of headings" \
                   "   num headings = %d, col = %d" % (ncols, col))
            continue
         table.horizontalHeader().setResizeMode(col, QtGui.QHeaderView.Stretch)

      self.gvars.Table_epi = table

      self.epi_list_to_table()

   def epi_list_to_table(self, epilist=None):
      """update Table_epi from epi array"""
      table = self.gvars.Table_epi              # convenience
      if epilist == None: epi = self.svars.epi
      else:               epi = epilist
      nrows = len(epi)

      table.setRowCount(0)                      # init, add rows per file
      table.setSortingEnabled(False)            # sort only after filling table

      if nrows <= 0: return

      # parse the EPI list into directory, short names, glob string
      epi_dir, short_names, globstr = USUBJ.flist_to_table_pieces(epi)

      # ------------------------------------------------------------
      # note wildcard form and try to create index list
      indlist = UTIL.list_minus_glob_form(short_names)

      # indlist list is either list of string integers or empty strings
      haveinds = 0
      if len(indlist) < nrows: indlist = ['' for ind in range(nrows)]
      else:
         try:
            indlist = [int(val) for val in indlist]
            haveinds = 1
         except: indlist = ['' for ind in range(nrows)]

      # ------------------------------------------------------------
      # get max index for zero padding
      digits = 0
      if haveinds:
         maxind = UTIL.maxabs(indlist)
         digits = UTIL.ndigits_lod(maxind)

      if self.verb > 2:
         print "== epi table, ndsets = %d, dir = %s" % (nrows, epi_dir)
         print "   wildcard string = %s" % globstr
         print "   indlist  = %s" % indlist

      # ------------------------------------------------------------
      # now fill table with indlist and epi (short_names)

      for ind, dset in enumerate(short_names):
         nameItem = QtGui.QTableWidgetItem(dset)
         if haveinds:
            indItem = QtGui.QTableWidgetItem('%0*d' % (digits, indlist[ind]))
         else: indItem = QtGui.QTableWidgetItem('')
         indItem.setTextAlignment(QtCore.Qt.AlignHCenter|QtCore.Qt.AlignVCenter)
         table.insertRow(ind)           # insert at end
         table.setItem(ind, 0, indItem)
         table.setItem(ind, 1, nameItem)

      table.resizeRowsToContents()
      table.setAlternatingRowColors(True)
      # table.setDragDropMode(QtGui.QAbstractItemView.InternalMove)

      # set min size base on ~25 per row, with min of 200 and max of 325
      table.setMinimumSize(100, self.max_table_size(nrows))

      # if we have a scan index, default to using it for sorting
      table.setSortingEnabled(True)
      if haveinds: table.sortItems(0)
      else:        table.sortItems(1)

      # ------------------------------------------------------------
      # and fill in Label_epi_ndsets, epi_dir, and epi_wildcard (form)
      self.gvars.Label_epi_ndsets.setText('%d' % nrows)
      self.gvars.Label_epi_dir.setText(epi_dir)
      self.gvars.Label_epi_wildcard.setText(globstr)

   def make_stim_table(self, parent=None):
      """init table from stim array
         - only update stim array on directory scan and 'update AP command'
         - 3 columns: index, label, filename
      """
      col_heads = ['index', 'label', 'basis func', 'stim timing file']
      stretch_cols = [3]                         # columns that should stretch
      nrows = len(self.svars.stim)               # one row per stim file
      ncols = len(col_heads)

      table = QtGui.QTableWidget(nrows, ncols)

      table.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
      table.setHorizontalHeaderLabels(col_heads)

      self.resize_table_cols(table, stretch_cols)

      # set and fill the table
      self.gvars.Table_stim = table
      self.stim_list_to_table()

   def resize_table_cols(self, table, stretch_cols):
      """resize to column contents, unless it is in strech_cols"""

      ncols = table.columnCount()

      # resize columns to fit data
      for col in range(ncols):
         if col in stretch_cols:
           if col < 0 or col >= ncols: continue # be safe
           table.horizontalHeader().setResizeMode(col,QtGui.QHeaderView.Stretch)
         else:
           table.horizontalHeader().setResizeMode(col,
                                    QtGui.QHeaderView.ResizeToContents)

   def stim_list_to_table(self, stimlist=None, make_labs=0):
      """update Table_stim from stim array

         Try to parse filenames into the form PREFIX.INDEX.LABEL.SUFFIX,
         where the separators can be '.' or '_'.
      """
      table = self.gvars.Table_stim              # convenience
      if stimlist == None: stim = self.svars.stim
      else:                stim = stimlist
      nrows = len(stim)

      table.setRowCount(0)                      # init, add rows per file
      table.setSortingEnabled(False)            # sort only after filling table

      if nrows <= 0: return

      # parse the stim list into directory, short names, glob string
      stim_dir, short_names, globstr = USUBJ.flist_to_table_pieces(stim)

      # ------------------------------------------------------------

      # get index and label lists
      stim_table = UTIL.parse_as_stim_list(short_names)
      indlist = [entry[0] for entry in stim_table]

      # if we don't have the correct number of labels, override make_labs
      if len(self.svars.stim_label) != nrows: make_labs = 1
      if make_labs: lablist = [entry[1] for entry in stim_table]
      else: lablist = self.svars.stim_label

      if self.verb > 2:
         print "== stim table, ndsets = %d, dir = %s" % (nrows, stim_dir)
         print "   wildcard string = %s" % globstr
         print "   stim_table: %s" % stim_table

      haveinds = 1
      for ind, val in enumerate(indlist):
         if val < 0:
            if self.verb > 1:
               print '-- bad stim_table index %d = %d, skipping indices' \
                     % (ind, val)
            haveinds = 0
            break

      # get max index for zero padding
      digits = 0
      if haveinds:
         maxind = UTIL.maxabs(indlist)
         digits = UTIL.ndigits_lod(maxind)

      # ------------------------------------------------------------
      # make basis function list
      nbases = len(self.svars.stim_basis)
      if nbases == 0:
         bases = ['GAM' for i in range(nrows)]
      elif nbases == 1:
         bases = [self.svars.stim_basis[0] for i in range(nrows)]
      elif nbases != nrows:
         print '** len(stim_basis) == %d, but have %d stim' % (nbases, nrows)
         bases = [self.svars.stim_basis[0] for i in range(nrows)]
      else:
         bases = self.svars.stim_basis

      # ------------------------------------------------------------
      # now fill table with index, label and filename (short_names)

      for ind, dset in enumerate(short_names):
         if haveinds:
            indItem = QtGui.QTableWidgetItem('%0*d'%(digits, indlist[ind]))
         else:
            indItem = QtGui.QTableWidgetItem('')
         indItem.setTextAlignment(QtCore.Qt.AlignHCenter|QtCore.Qt.AlignVCenter)

         labItem = QtGui.QTableWidgetItem(lablist[ind])
         basisItem = QtGui.QTableWidgetItem(bases[ind])
         nameItem = QtGui.QTableWidgetItem(dset)
         table.insertRow(ind)           # insert at end
         table.setItem(ind, 0, indItem)
         table.setItem(ind, 1, labItem)
         table.setItem(ind, 2, basisItem)
         table.setItem(ind, 3, nameItem)

      table.resizeRowsToContents()
      table.setAlternatingRowColors(True)
      # table.setDragDropMode(QtGui.QAbstractItemView.InternalMove)
      # table.setDragEnabled(True)

      # set min size base on ~25 per row, with min of 75 and max of 200
      table.setMinimumSize(100, self.max_table_size(nrows))

      # if we have a stim index, default to using it for sorting
      table.setSortingEnabled(True)
      if haveinds: table.sortItems(0)
      else:        table.sortItems(2)

      # ------------------------------------------------------------
      # and fill in Label_stim_ndsets, stim_dir, and stim_wildcard (form)
      self.gvars.Label_stim_ndsets.setText('%d' % nrows)
      self.gvars.Label_stim_dir.setText(stim_dir)
      self.gvars.Label_stim_wildcard.setText(globstr)

   def max_table_size(self, nrows):
      """return the number of pixels to use based on the number of rows
         (consider scaling this by the font size)"""
      if nrows <= 6:   show_rows = nrows
      elif nrows < 18: show_rows = 6+0.5*(nrows-6)  # after 6, add half, per
      else:            show_rows = 12

      return min(200, max(75, 30+25*show_rows))

   def CB_checkbox(self):
      """call-back for any check boxes"""
      obj = self.sender()
      if   obj == self.gvars.gbox_anat.checkBox:
         if obj.isChecked(): self.set_svar('get_tlrc', 1)
         else:               self.set_svar('get_tlrc', 0)
      elif obj == self.gvars.gbox_epi.checkBox_wildcard:
         if obj.isChecked(): self.set_svar('epi_wildcard', 1)
         else:               self.set_svar('epi_wildcard', 0)
      elif obj == self.gvars.gbox_stim.checkBox_wildcard:
         if obj.isChecked(): self.set_svar('stim_wildcard', 1)
         else:               self.set_svar('stim_wildcard', 0)
      else: print "** CB_checkbox: unknown sender"

   def update_textLine_check(self, obj, text, attr, button_name, check_func):
      """check text against check_func(text, bname, 1, self, 1)
         - this warns the user on error
         - if valid, update the attr and textLine with the text
         - else, clear object and set focus to it (and return)
      """
      
      # be sure we have a type to compare against for setting the text
      global g_LineEdittype
      if g_LineEdittype == None: g_LineEdittype = type(QtGui.QLineEdit())

      rtext = str(text) # in case of QString

      if check_func(rtext, button_name, 1, self, 1):
         self.set_svar(attr, rtext)
         if type(obj) == g_LineEdittype: obj.setText(text)
         else: print '** update_textLine_check: not a LineEdit type'
      else:
         # error, reset to previous attribute
         # obj.clear()
         obj.setText(self.svars.val(attr))
         obj.setFocus()

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

      elif text == 'clear anat':
         self.gvars.Line_anat.clear()
         self.set_svar('anat','')

      # EPI
      elif text == 'help: EPI':
         self.update_help_window(g_help_epi, title='EPI datasets')

      elif text == 'browse EPI':
         fnames = QtGui.QFileDialog.getOpenFileNames(self,
                        "load EPI datasets", self.pick_base_dir('epi'),
                        "datasets (*.HEAD *.nii);;all files (*)")
         if len(fnames) > 0:
            self.set_svar('epi', [str(name) for name in fnames])
            self.epi_list_to_table()

      elif text == 'clear EPI':
            self.set_svar('epi', [])
            self.epi_list_to_table()

      # stim
      elif text == 'help: stim':
         self.update_help_window(g_help_stim, title='stim times files')

      elif text == 'browse stim':
         fnames = QtGui.QFileDialog.getOpenFileNames(self,
                     "load stimulus timing files", self.pick_base_dir('stim'),
                     "files (*.txt *.1D);;all files (*)")
         if len(fnames) > 0:
            # if reading from disk, clear existing variables
            self.set_svar('stim', [str(name) for name in fnames])
            self.set_svar('stim_label', [])
            self.set_svar('stim_basis', [])
            self.stim_list_to_table(make_labs=1)

      elif text == 'clear stim':
         self.set_svar('stim', [])
         self.stim_list_to_table()

      elif text[0:7] == 'basis: ':
         self.update_basis_function(text[7:])

      # expected
      elif text[0:9] == 'vr base: ':
         base = text[9:]
         self.set_svar('volreg_base', base)
         self.gvars.Line_volreg_base.setText(base)

      else: print "** unexpected button text: %s" % text

   def pick_base_dir(self, dtype):
      """return something useful or an empty string"""
      anat = self.svars.anat    # for ease of typing
      epi  = self.svars.epi
      stim = self.svars.stim
      if dtype == 'top':     # common dir to all input files
         all_files = []
         if anat != '': all_files.append(anat)
         all_files.extend(epi)
         all_files.extend(stim)
         return UTIL.common_dir(all_files)
      elif dtype == 'anat':
         if anat != '':      return os.path.dirname(anat)
         elif len(epi) > 0:  return os.path.dirname(epi[0])
         elif len(stim) > 0: return os.path.dirname(stim[0])
      elif dtype == 'epi':
         if len(epi) > 0:    return os.path.dirname(epi[0])
         elif anat != '':    return os.path.dirname(anat)
         elif len(stim) > 0: return os.path.dirname(stim[0])
      elif dtype == 'stim':
         if len(stim) > 0:   return os.path.dirname(stim[0])
         elif len(epi) > 0:  return os.path.dirname(epi[0])
         elif anat != '':    return os.path.dirname(anat)
      else:
         print '** pick_base_dir: bad dtype = %s' % dtype

      return ''

   def update_basis_function(self, basis):
      if len(basis) > 0 and not self.basis_func_is_current(basis):
         if self.verb > 1: print '++ applying basis function %s' % basis
         self.gvars.Line_apply_basis.setText(basis)
         nstim = len(self.svars.stim)
         self.set_svar('stim_basis',[basis for i in range(nstim)])
         self.stim_list_to_table()

   def basis_func_is_current(self, basis):
      """check a few things:
           - stim_basis must have length 1 or len(stim)
           - each entry must match 'basis'"""
      nstim = len(self.svars.stim)
      nbasis = len(self.svars.stim_basis)

      if nbasis == 0:   # empty is special, since we cannot access entries
         if nstim == 0: return 1
         else:          return 0

      # next check for matching lengths (or unit)
      if nbasis > 1 and nbasis != nstim: return 0

      # finally, check the entries
      for sbasis in self.svars.stim_basis:
         if basis != sbasis: return 0

      # they seem to match
      return 1

   def add_menu_bar(self):

      # ----------------------------------------------------------------------
      # main process actions

      act1 = self.createAction("gen: afni_proc.py command",
        slot=self.cb_show_ap_command,
        tip="generate afni_proc.py command",
        icon=self.style().standardIcon(QtGui.QStyle.SP_FileDialogDetailedView))
      act2 = self.createAction("view: processing script",
        slot=self.cb_exec_ap_command,
        tip="show proc script: display output from afni_proc.py",
        icon=self.style().standardIcon(QtGui.QStyle.SP_FileDialogContentsView))
      act3 = self.createAction("process this subject",
          slot=self.cb_exec_proc_script,
          tip="process this subject: execute proc script",
          icon=self.style().standardIcon(QtGui.QStyle.SP_DialogYesButton))

      self.gvars.act_show_ap = act1
      self.gvars.act_exec_ap = act2
      self.gvars.act_exec_proc = act3

      self.gvars.act_exec_ap.setEnabled(False)
      self.gvars.act_exec_proc.setEnabled(False)

      # ----------------------------------------------------------------------
      # File menu
      self.gvars.MBar_file = self.menuBar().addMenu("&File")
      actFileQuit = self.createAction("&Quit", slot=self.close,
          shortcut="Ctrl+q", tip="close the application")
      self.addActions(self.gvars.MBar_file, [act1, act2, act3, actFileQuit])

      # ----------------------------------------------------------------------
      # View menu - all for static view windows
      self.gvars.MBar_view = self.menuBar().addMenu("&View")

      act1 = self.createAction("afni_proc.py command",
        slot=self.cb_view,
        tip="display current afni_proc.py command")

      act2 = self.createAction("resulting proc script",
        slot=self.cb_view,
        tip="display script output by afni_proc.py")

      act3 = self.createAction("output from proc script",
        slot=self.cb_view,
        tip="display text output from execution of proc script")

      act4 = self.createAction("view: uber_subject.py command",
          slot=self.cb_view,
          tip="show command to populate this interface")

      self.addActions(self.gvars.MBar_view, [act1, act2, act3, act4])

      self.gvars.act_view_ap_cmd   = act1
      self.gvars.act_view_proc     = act2
      self.gvars.act_view_outproc  = act3
      self.gvars.act_view_uber_cmd = act4

      # ----------------------------------------------------------------------
      # Hidden menu
      self.gvars.MBar_hidden = self.menuBar().addMenu("Hidde&n")

      # create command window, and set call back for it
      act1 = self.createAction("shell command window",
          slot=self.cb_show_command_window,
          tip="open command window for shell commands")

      act2 = self.createAction("python command window",
          slot=self.cb_show_py_command_window,
          tip="open command window for local python commands")

      act3 = self.createAction("view: subject vars",
        slot=self.cb_view,
        tip="display current subject option variables")

      act4 = self.createAction("view: control vars",
        slot=self.cb_view,
        tip="display control option variables")

      act5 = self.createAction("view: result vars",
        slot=self.cb_view,
        tip="display variables resulting from actions")

      act6 = self.createAction("show: Icon keys",
          slot=QLIB.print_icon_names,
          tip="display standard Icon names")

      self.gvars.act_view_svars   = act3
      self.gvars.act_view_cvars   = act4
      self.gvars.act_view_rvars   = act5

      self.addActions(self.gvars.MBar_hidden, [act3, act4, act5])

      # add show sub-menu
      self.gvars.Menu_commands = self.gvars.MBar_hidden.addMenu("&Commands")
      self.addActions(self.gvars.Menu_commands, [act1, act2])

      # create Style sub-menu
      self.gvars.Menu_format = self.gvars.MBar_hidden.addMenu("set Style")
      actlist = []
      for style in g_styles:
         tip = 'set GUI style to %s...' % style
         act = self.createAction(style, slot=self.cb_setStyle, tip=tip)
         actlist.append(act)
      actlist.append(self.createAction("Default", slot=self.cb_setStyle,
                                tip="revert Sylte to default 'cleanlooks'"))
      self.addActions(self.gvars.Menu_format, actlist)

      self.addActions(self.gvars.MBar_hidden, [act6])

      # ----------------------------------------------------------------------
      # Help menu
      self.gvars.MBar_help = self.menuBar().addMenu("&Help")
      actHelpUS = self.createAction("'uber_subject.py -help'",
          slot=self.cb_help_main, shortcut=QtGui.QKeySequence.HelpContents,
          tip="help for uber_subject.py")
      self.addActions(self.gvars.MBar_help, [actHelpUS])

      # add browse actions to browse sub-menu
      self.gvars.Menu_browse = self.gvars.MBar_help.addMenu("&Browse")
      act1 = self.createAction("web: all AFNI programs",
          slot=self.cb_help_browse_progs, tip="browse AFNI program help")
      act2 = self.createAction("web: afni_proc.py help",
          slot=self.cb_help_browse_AP, tip="browse afni_proc.py help")
      act3 = self.createAction("web: tutorial-single subject analysis",
          slot=self.cb_help_browse_tutorial, tip="browse AFNI_data6 tutorial")
      act4 = self.createAction("web: AFNI Message Board",
          slot=self.cb_help_browse_MBoard, tip="browse Message Board")
      self.addActions(self.gvars.Menu_browse, [act1, act2, act3, act4])

      actHelpAbout = self.createAction("about uber_subject.py",
          slot=self.cb_help_about, tip="about uber_subject.py")
      self.addActions(self.gvars.MBar_help, [actHelpAbout])

   def add_tool_bar(self):
      self.gvars.toolbar = self.addToolBar('afni_proc.py')
      self.gvars.toolbar.addAction(self.gvars.act_show_ap)
      self.gvars.toolbar.addAction(self.gvars.act_exec_ap)
      self.gvars.toolbar.addAction(self.gvars.act_exec_proc)

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
         action.setIcon(icon)
      return action

   def addActions(self, target, actions):
      for action in actions:
         if action is None:
            target.addSeparator()
         else:
            target.addAction(action)

   def add_status_bar(self):
      self.gvars.statusbar = self.statusBar()
      self.gvars.statusbar.showMessage("Ready")

   def make_extra_widgets(self):
      """create - ULIB.TextWindow for help messages, AP result and warnings
                - gvars.browser for web links"""

      # text window (if None (for now), new windows will be created)
      # self.gvars.Text_help = None
      self.gvars.Text_help      = QLIB.TextWindow(parent=self)
      self.gvars.Text_AP_result = QLIB.TextWindow(parent=self)

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

   def update_AP_result_window(self, win=None, text='', title='', fname=''):
      """default window is Text_AP_result
         - if fname, read file
           else use text"""
      if win: window = win
      else:   window = self.gvars.Text_AP_result

      if title: window.setWindowTitle(title)
      if fname: # then read from file
         window.filename = fname
         window.readfile()
      else: window.editor.setText(text)
      window.show()
      window.raise_()

   def update_AP_error_window(self, text, title='ERROR - cannot proceed'):
      QLIB.guiError(title, text, self)

   def update_AP_warn_window(self, text, title='WARNING'):
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
      self.update_help_window(USUBJ.helpstr_usubj_gui,
                              title='uber_subject.py: main help')

   def cb_help_about(self):
      """display version info in Text_Help window"""
      text = "uber_subject.py, version %s" % USUBJ.g_version
      self.update_help_window(text, title='about uber_subject.py')

   def cb_help_browse_progs(self):
      self.open_web_site('http://afni.nimh.nih.gov/afni/doc'    \
                         '/program_help/index.html/view')

   def cb_help_browse_MBoard(self):
      self.open_web_site('http://afni.nimh.nih.gov/afni/community/board')

   def cb_help_browse_AP(self):
      self.open_web_site(
        'http://afni.nimh.nih.gov/pub/dist/doc/program_help/afni_proc.py.html')

   def cb_help_browse_tutorial(self):
      self.open_web_site('http://afni.nimh.nih.gov/pub/dist/edu/data' \
                         '/CD.expanded/AFNI_data6/FT_analysis/tutorial')

   def cb_show_ap_command(self):
      self.update_svars_from_tables()

      if self.svars.is_empty('sid') or self.svars.is_empty('gid'):
         QLIB.guiError('Error', "** subject and group IDs must be set", self)
         return

      if self.set_sdir and self.cvars.is_empty('subj_dir'):
         sdir =  '%s/%s/%s'%(g_def_subj_parent, self.cvars.gid, self.cvars.sid)
         print '-- setting subj_dir to %s' % sdir
         self.set_cvar('subj_dir', sdir)
        
      # create the subject, check warnings and either a command or errors
      self.apsubj = USUBJ.AP_Subject(self.svars, self.cvars)
      nwarn, wstr = self.apsubj.get_ap_warnings()
      status, mesg = self.apsubj.get_ap_command()

      if status:        # then only mention errors
         self.update_AP_error_window(mesg)
         return

      self.gvars.ap_status = 1       # now have afni_proc.py command script
      self.apsubj.write_ap_command()

      fname = self.apsubj.subj_dir_filename('file_ap')
      if not fname:
         self.update_AP_warn_window('** no proc script to show')
         return
      elif not os.path.isfile(fname):
         self.update_AP_warn_window('** proc script not found\n' \
                                    '   (should be file %s)' % fname)
         return

      self.update_AP_result_window(title="Success!  afni_proc.py command:",
                                   fname=fname)
      if nwarn > 0: self.update_AP_warn_window(wstr)
      self.gvars.act_exec_ap.setEnabled(True)
      self.gvars.act_exec_proc.setEnabled(True)

   def update_svars_from_tables(self):
      """get updates from the EPI and stim tables"""
      # get the EPI directory and the number of rows
      # --> for each row append dir/entry to svars.epi
      dir = str(self.gvars.Label_epi_dir.text())
      table = self.gvars.Table_epi              # convenience
      nrows = table.rowCount()
      dlist = []
      for row in range(nrows):
         item = table.item(row, 1)
         dset = str(item.text())
         if dir and dir != '.': pre = '%s/' % dir
         else:                  pre = ''
         dlist.append('%s%s' % (pre, dset))
      self.svars.epi = dlist    # replace the old list

      dir = str(self.gvars.Label_stim_dir.text())
      table = self.gvars.Table_stim             # convenience
      nrows = table.rowCount()
      llist = []
      blist = []
      dlist = []
      for row in range(nrows):
         # get label, basis, stim file
         item = table.item(row, 1)
         llist.append(str(item.text()))

         item = table.item(row, 2)
         blist.append(str(item.text()))

         item = table.item(row, 3)
         dset = str(item.text())
         if dir and dir != '.': pre = '%s/' % dir
         else:                  pre = ''
         dlist.append('%s%s' % (pre, dset))
      self.svars.stim_label = llist
      self.svars.stim_basis = blist
      self.svars.stim = dlist 

   def cb_exec_ap_command(self):
      """execute afni_proc.py command script"""
      self.exec_ap_command()

   def exec_ap_command(self):
      """execute afni_proc.py command script

         return 0 on success, 1 on error"""

      # are we ready?
      fmesg = ''        # init fail message to empty
      if not self.apsubj:
         fmesg = '** no subject class for running AP command'
      elif self.gvars.ap_status < 1:
         fmesg = '** need to first generate command'

      if fmesg != '':
         QLIB.guiError('Error', fmesg, self)
         return 1

      status, mesg = self.apsubj.exec_ap_command()
      if status:
         QLIB.guiError('Error', mesg, self)
         return 1

      self.update_AP_result_window(text=mesg,
                                   title="output text from afni_proc.py")

      self.status = 2

      return 0

   def cb_exec_proc_script(self):
      """execute the proc script in a new terminal window"""
      # if we are not ready, return
      fmesg = ''
      if not self.apsubj:
         fmesg = '** no subject class for running proc script'
      elif self.gvars.ap_status < 1:
         fmesg = '** need to first generate AP command and script'

      if fmesg != '':
         QLIB.guiError('Error', fmesg, self)
         return

      if self.gvars.ap_status == 1:
         # execute afni_proc.py command, return on failure
         if self.exec_ap_command(): return

      # make sure there is a script to execute

      fname = self.apsubj.subj_dir_filename('file_proc')
      if not fname:
         self.update_AP_warn_window('** proc file not set')
         return
      elif not os.path.isfile(fname):
         self.update_AP_warn_window('** proc file not found: %s\n' % fname)
         return

      self.apsubj.nuke_old_results()    # get rid of any previous results

      # start a new window?
      if self.gvars.valid('SPW'):
         self.gvars.SPW.close()
         del(self.gvars.SPW)

      # actually run script, piping output if we have an output file
      sname = self.apsubj.rvars.file_proc
      oname = self.apsubj.rvars.output_proc
      command= 'tcsh -xef %s' % sname
      if oname: command = '%s |& tee %s' % (command, oname)
      self.gvars.SPW = QLIB.TcshCommandWindow(command,
                           dir=self.apsubj.cvars.subj_dir, parent=self)
      self.gvars.SPW.show()
      self.gvars.ap_status = 3 

   def cb_view(self):
      """create permanent windows with given text"""
      obj = self.sender()

      if obj == self.gvars.act_view_ap_cmd:
         self.show_static_file('file_ap', 'afni_proc.py script')

      elif obj == self.gvars.act_view_proc:
         self.show_static_file('file_proc', 'proc script')

      elif obj == self.gvars.act_view_outproc:
         self.show_static_file('output_proc', 'proc output')

      elif obj == self.gvars.act_view_svars:
         sstr = self.svars.make_show_str('current subject', name=0)
         QLIB.static_TextWindow(title='subject vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_cvars:
         sstr = self.cvars.make_show_str('current subject', name=0)
         QLIB.static_TextWindow(title='control vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_rvars:
         if self.apsubj == None:
            QLIB.guiWarning('Error', '** result vars not yet set', self)
            return
         sstr = self.apsubj.rvars.make_show_str('current subject', name=0)
         QLIB.static_TextWindow(title='return vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_uber_cmd:
         sstr = self.make_uber_command()
         QLIB.static_TextWindow(title='corresponding uber_subject.py command',
                                text=sstr, parent=self)

   def make_uber_command(self):
      """generate a script that would invoke the uber_subject.py interface
         with subject and control vars set (and so fields filled)

         Put any key elements in quotes, such as basis functions.
      """

      cmd = 'uber_subject.py'

      # control vars first
      prefix = ' \\\n    -cvar '     # append before next command
      for atr in self.cvars.attributes():
         if atr == 'name': continue     # skip
         if self.cvars.vals_are_equal(atr, USUBJ.g_ctrl_defs): continue
         # show this one
         val = self.cvars.val(atr)
         if self.cvars.has_simple_type(atr):
            cmd += (prefix + '%s %s' % (atr, val))
         elif type(val) == list:
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))
         else:
            print '** make_uber_command: bad attr %s' % atr

      # then subject vars
      prefix = ' \\\n    -svar '     # append before next command
      for atr in self.svars.attributes():
         if atr == 'name': continue     # skip
         if self.svars.vals_are_equal(atr, USUBJ.g_subj_defs): continue
         # show this one
         val = self.svars.val(atr)
         
         if atr == 'stim_basis':        # special case
            val = ["'%s'" % v for v in val]
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))

         elif self.svars.has_simple_type(atr):
            cmd += (prefix + '%s %s' % (atr, val))
         elif type(val) == list:
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))
         else:
            print '** make_uber_command: bad attr %s' % atr

      return UTIL.add_line_wrappers(cmd + '\n')

   def show_static_file(self, var_name, var_desc):
      """display the var according to var_name
         - use description in var_desc if any error"""
      if self.apsubj == None:        # check for generated command
         self.update_AP_warn_window('** afni_proc.py command must be\n' \
                                    '   executed first')
         return
      file = self.apsubj.subj_dir_filename(var_name)
      if not file:
         self.update_AP_warn_window('** file not set: %s' % var_desc)
         return
      elif not os.path.isfile(file):
         self.update_AP_warn_window('** file not found: %s\n' \
                                    '   (used for %s)'%(file, var_desc))
         return
      else:
         title = '%s %s' % (var_desc, file)
         QLIB.static_TextWindow(title=title, fname=file, parent=self)

   def cb_setStyle(self):
      text = ''
      try:
         sender = self.sender()
         text = str(sender.text())
         print '-- cb_setSyle: text = %s' % text
         if text == 'Default': text = g_styles[g_style_index_def]
      except:
         print '** cb_setSyle: cannot get text'
         return

      if text in g_styles:
         try:
            self.gvars.style = text
            QtGui.QApplication.setStyle(QtGui.QStyleFactory.create(text))
         except: print "** failed to set style '%s'" % text
      else: print "** style '%s' not in style list" % text

   def set_cvar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if not self.cvars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print "++ set_cvar: update [%s] to '%s'"%(name,newval)

   def set_svar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if not self.svars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print "++ set_svar: update [%s] to '%s'"%(name,newval)

      self.gvars.ap_status = 0  # no longer ready for execution
      self.gvars.act_exec_ap.setEnabled(False)
      self.gvars.act_exec_proc.setEnabled(False)

   def apply_svars(self, svars=None):
      """apply to the svars object and to the gui

         first init to defaults
         if svars is passed, make further updates"""

      if svars == None: return

      # merge with current vars and apply everything to GUI
      self.svars.merge(svars)
      for var in self.svars.attributes():
         self.apply_svar_in_gui(var)

      if self.verb > 2: self.svars.show("post reset subject vars")

   def apply_svar_in_gui(self, svar):
      """this is a single interface to apply any subject variable in the GUI

         if a variable is not handled in the interface, ignore it

         return 1 if processed
      """

      rv = 1
      if   svar == 'uber_dir':             rv = 0       # todo
      elif svar == 'blocks':               rv = 0       # todo
      elif svar == 'sid':         self.gvars.Line_sid.setText(self.svars.sid)
      elif svar == 'gid':         self.gvars.Line_gid.setText(self.svars.gid)
      elif svar == 'anat':        self.gvars.Line_anat.setText(self.svars.anat)
      elif svar == 'get_tlrc':
                                  obj = self.gvars.gbox_anat.checkBox
                                  obj.setChecked(self.svars.get_tlrc)
      elif svar == 'epi':         self.epi_list_to_table()
      elif svar == 'epi_wildcard':         
                                  obj = self.gvars.gbox_epi.checkBox_wildcard
                                  obj.setChecked(self.svars.epi_wildcard)
      elif svar == 'stim':        self.stim_list_to_table()
      elif svar == 'stim_wildcard':        
                                  obj = self.gvars.gbox_stim.checkBox_wildcard
                                  obj.setChecked(self.svars.stim_wildcard)
      elif svar == 'label':       self.stim_list_to_table()
      elif svar == 'basis':       self.stim_list_to_table()

      elif svar == 'tcat_nfirst': 
                                   obj = self.gvars.Line_tcat_nfirst
                                   obj.setText('%d'%self.svars.tcat_nfirst)
      elif svar == 'volreg_base':  
                                   obj = self.gvars.Line_volreg_base
                                   obj.setText(self.svars.volreg_base)
      elif svar == 'motion_limit':
                                   obj = self.gvars.Line_motion_limit
                                   obj.setText('%g'%self.svars.motion_limit)
      else:
         if self.verb > 1: print '** apply_svar_in_gui: unhandled %s' % svar
         rv = 0

      if rv and self.verb > 2: print '++ apply_svar_in_gui: process %s' % svar

      return rv

   def apply_cvars(self, cvars=None):
      """apply to the cvars object and to the gui

         first init to defaults
         if cvars is passed, make further updates"""

      if cvars == None: return

      # merge with current vars and apply everything to GUI
      self.cvars.merge(cvars)
      self.set_cvar('verb', self.verb)          # to pass verb along
      for var in self.cvars.attributes():
         self.apply_cvar_in_gui(var)

      if self.verb > 2: self.cvars.show("post reset control vars")

   def apply_cvar_in_gui(self, cvar):
      """this is a single interface to apply any control variable in the GUI

         if a variable is not handled in the interface, ignore it

         return 1 if processed
      """

      rv = 1
      if   cvar == 'uber_dir':             rv = 0       # todo
      else:
         if self.verb > 1: print '** apply_cvar_in_gui: unhandled %s' % cvar
         rv = 0

      if rv and self.verb > 2: print '++ apply_cvar_in_gui: process %s' % cvar

      return rv

# --- post SingleSubjectWindow class

# ===========================================================================
# help strings

g_help_gui_design = """
todo:  

1. update EPI datasets and stim files from tables when edits are made

2. *** update this ...

     - QLIB: add QGroupBox containing 4x4 grid of:
             buttons (browse, etc), QTableWidget,
             QCheckBob,             Label/LineEdit HBox

QDialog
   QVBoxLayout
      QGroupBox (general subject info)
      QGroupBox (other)
         QVBoxLayout (scrollabel)
            QGroupBox (anatomy)
               QGridLayout (2x2)
                  buttons (browse, clear,...)      QTableWidget
                  QCheckBox                        Label/LinEdit HBox
            QGroupBox (EPI)
            QGroupBox (stim files)
            .
            .
            .

for example, consider widgets/character_map.py

"""

# other help strings
g_help_anat = """
Specifying the anatomical dataset:

   goals:

      1. choose an anatomical dataset
      2. decide whether to include a copy of an existing +tlrc version

   description:

      Use 'browse anat' to pick an anatomical dataset that corresponds to
      the EPI datasets.  If 'include copy' is set, then if the anat is in
      +orig space and a +tlrc version already exists (from either a manual
      transformation or @auto_tlrc), the +tlrc version will be included.

   typical use in processing:

      1. copy anat (and possibly +tlrc version) into results directory
      2. if no +tlrc anatomy, create one via @auto_tlrc
      3. align EPI to anat (via align_epi_anat.py)
      4. transform EPI to +tlrc space (according to anat transformation)

      note: the EPI transformations are applied in the 'volreg' block
"""

g_help_epi = """
Specifying the EPI datasets:

   goals:

      1. choose a set of EPI datasets (from a single directory)
      2. decide whether to use the 'wildcard form' in the afni_proc.py
         command (rather than listing individual EPI datasets)

   description:

      Use 'browse EPI' to choose a list of EPI datasets from some directory.
      When the names are chosen, the directory will be separated from the
      dataset names, with the resulting names formed as a wildcard string.
      If the names do not have a fixed prefix and suffix, the wildcard string
      will probably not be appropriate.

      The default order is the 'scan index' order, if indices are found.
      The datasets can be sored by either column by clicking on the column
      header (e.g. 'scan index').

   typical use in processing:

      1. remove pre-steady state TRs (specified farther down the GUI)
      2. pre-process the EPI: time shift, align to other EPI, align to anat,
           warp to standard space, blur, scale to percent of mean
      3. compute motion parameters based on EPI to EPI alignment (volreg)
      4. regress against model

   file naming habits:

      suggested example: epi_r07+orig.HEAD

      It is a good habit to have the EPI dataset names be the same except for
      the run index.  It is also preferable (though not necessary) to zero-pad
      those indicies so they have the same number of digits.  That makes the
      numeric scan order equal to the alphabetical order.

      For example, ordering these files by run (scan) index gives:

        epi_r1+orig.HEAD
        epi_r7+orig.HEAD
        epi_r9+orig.HEAD
        epi_r10+orig.HEAD
        epi_r11+orig.HEAD

      But if they were sorted alphabetically (which would happen when using
      the wildcard form), the order would be (likely incorrect):

        epi_r1+orig.HEAD
        epi_r10+orig.HEAD
        epi_r11+orig.HEAD
        epi_r7+orig.HEAD
        epi_r9+orig.HEAD

     While this GUI would figure out the 'scan index' order of 1, 7, 9, 10, 11,
     it is a safer habit to zero pad the index values: 01, 07, 09, 10, 11.
     That would make the index order the same as the alphabetical order, which
     would also allow for the safe use of wildcards.  In that case, the order
     would be (for either scan index or alphabetical order):

        epi_r01+orig.HEAD
        epi_r07+orig.HEAD
        epi_r09+orig.HEAD
        epi_r10+orig.HEAD
        epi_r11+orig.HEAD

    Again, the zero padded order would be allow for wildcard use, specifying
    all five datasets by: "epi_r*+orig.HEAD".

    Note that the wildcard use would also be inappropriate if there were some
    dataset that is not supposed to be used, such as if epi_r08+orig existed,
    but was for an aborted scan.
"""

g_help_stim = """
Specifying the stimulus timing files:

   goals:

      1. choose a set of stimulus timing files (from a single directory)
      2. decide whether to use the 'wildcard form' in the afni_proc.py
         command (rather than listing individual files)
      3. choose a basis function (possibly for each timing file)
  
   description:

      Use 'browse stim' to choose a list of timing files from some directory.
      When the names are chosen, the directory will be separated from each
      dataset name, with the resulting names formed as a wildcard string
      (which might not be appropriate).  The GUI will attempt to separate the
      file names into a list of: prefix, index, label, suffix.
      If this is possible, the index and label fields will be populated.

      The default sort order is by 'index', if indices are found.  The files
      can be sorted by any column by clicking on the column header ('index').

      If the wildcard form seems appropriate, it can be applied in the
      afni_proc.py script by setting 'use wildcard form'.

      A basis functions will applied to each timing file (stimulus class).
      They can all be set at once via 'init basis funcs', or they can be
      modified individually in the table.

   ** Note: no stimulus should be given during the pre-steady state TRs.
            The stimulus times should match times after the pre-SS TRs that
            are removed from the EPI data.

   typical use in processing:

      1. use the timing files and basis functions to create regressors of
         interest to be used in the regress processing block (by 3dDeconvolve).
      2. if the basis functions are fixed shapes (e.g. GAM/BLOCK), generate
         'ideal' curves per stimulus class (from the X-matrix columns)

   file naming habits:

      suggested example: stim_07_pizza.txt

      a. To group files together, start them with the same prefix.
      b. To order them expectedly, add a zero-padded (as needed) index number,
         so the numerical order matches the alphabetical order.
      c. To be clear about what files they are, add the exact label that should
         be used in the 3dDeconvolve command.
      d. Optionally, add a useful file suffix.

      See the 'help: EPI' section for details about numerical vs. alphabetical
      sorting and wildcard use.

      Other reasonable naming examples:

            stim7_pizza.txt
            7pizza
            stim07.1D

      Bad examples:

            faces.txt    (alphabetical order comes from labels)
            stim7        (it is not clear what stimulus class this is)
"""

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
