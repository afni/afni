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
import lib_uber_subject as USUBJ
import lib_qt_gui as QLIB

# allow users to play with style
g_styles = ["windows", "motif", "cde", "plastique", "cleanlooks"]
g_style_index = 0
g_style_index_def = 4

g_spacing = 3
g_glayout_spacing = 2
g_grid_spacing = 6

g_LineEdittype = None                   # set this type later


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
      self.gvars  = VO.VarsObject('uber_subject gui vars')

      # initialize the subject variables to defaults, update at the end
      self.svars  = USUBJ.g_sdef_strs.copy('subject vars')
      self.cvars  = USUBJ.g_cdef_strs.copy('control vars')
      if self.verb < 0: self.verb = int(self.cvars.verb)
      self.set_sdir = set_sdir
      if ctrl_vars and set_sdir:
          if ctrl_vars.is_not_empty('subj_dir'):
              if self.verb:
                 print('++ have passed subj_dir %s, keeping it' \
                       % ctrl_vars.val('subj_dir'))
              set_sdir = 0
              self.set_sdir = 0

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
      mainlayout.addWidget(self.gvars.Widget_ID)
      mainlayout.addWidget(self.gvars.m2_scroll)

      # ------------------------------
      # save this for last to ensure it is all visible
      self.gvars.m2_scroll.setWidget(self.gvars.m2_gbox_inputs)

      self.gvars.Wcentral.setMinimumSize(200, 300)
      self.gvars.Wcentral.resize(400, 800)
      self.gvars.Wcentral.setLayout(mainlayout)
      self.setCentralWidget(self.gvars.Wcentral)

      self.update_all_gbox_styles()

      self.make_extra_widgets()

      self.gvars.style = g_styles[g_style_index_def]

      # widgets are done, so apply pass subject vars
      self.reset_vars(svars=subj_vars, cvars=ctrl_vars,
                      set_sdir=set_sdir, verb=verb)

      # ap_status : 0 = must create ap command, 1 = have ap, need proc script,
      #             2 = have proc script, ready to execute, 3 = executing/ed
      # (meaning user must have first generated (and viewed) the command)
      self.gvars.ap_status = 0

      if self.verb > 1: print('-- finished Single Subject Dialog setup')

   def reset_vars(self, svars=None, cvars=None, set_sdir=0, verb=-1):
      """replace old svars/cvars with new"""

      # get verb from one of 4 places: passed, cvars, self, init to 1
      vv = -1
      if cvars != None:
         vv = cvars.val('verb')
         if vv == None: vv = -1
         else:
            try: vv = int(vv)
            except:
               '** reset vars bad verb %s %s' % (vv, type(vv))
               vv = -1
      if   verb >= 0:     self.verb = verb
      elif vv   >= 0:     self.verb = vv
      elif self.verb > 0: pass                  # leave unchanged
      else:               self.verb = 1
       
      self.set_sdir = set_sdir

      del(self.svars)
      del(self.cvars)

      self.svars = USUBJ.g_sdef_strs.copy()
      self.cvars = USUBJ.g_cdef_strs.copy()

      self.apply_svars(svars)
      self.apply_cvars(cvars)

      # since might not come from cvars, cvars are all strings
      self.set_cvar('verb', str(self.verb))

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

      self.make_subj_group_line()  # sid/gid

      self.gvars.m2_scroll = QtGui.QScrollArea()
      self.gvars.m2_scroll.setWidgetResizable(True)

      # create a containing GroupBox for everything in m2_scroll
      gbox = self.get_styled_group_box("input data and options")

      # the layout for the 'input datasets' QGroupBox is vertical
      self.gvars.m2_vlayout = QtGui.QVBoxLayout(gbox)
      gbox.setLayout(self.gvars.m2_vlayout)
      self.gvars.m2_vlayout.addSpacing(2)

      self.gvars.m2_gbox_inputs = gbox

   def update_all_gbox_styles(self):
      for gbox in [ self.gvars.gbox_anat, self.gvars.gbox_epi,
                    self.gvars.gbox_stim ]:
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

   def make_subj_group_line(self):
      """add a single line at the top level for subject and group IDs

         create Widget_ID to add to top level

         for controlling vars sid, gid
      """

      # --------------------------------------------------
      # Widget/HBox: subject and group ID fields
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()

      # add subject ID stuff, init with sid
      label = QLIB.make_label("subject ID", 
                        tip='identifier to use in file and directory names')
      self.gvars.Line_sid = QtGui.QLineEdit()
      self.gvars.Line_sid.setText(self.svars.sid)
      layout.addWidget(label)
      layout.addWidget(self.gvars.Line_sid)

      # add group ID stuff, init with sid
      label = QLIB.make_label("group ID",
                        tip='identifier to use in file and directory names')
      self.gvars.Line_gid = QtGui.QLineEdit()
      self.gvars.Line_gid.setText(self.svars.gid)
      layout.addWidget(label)
      layout.addWidget(self.gvars.Line_gid)

      # note callbacks for buttons
      # ** cannot use new method on Ubuntu 9.04:
      #    self.gvars.Line_sid.editingFinished.connect(self.CB_line_text)
      self.connect(self.gvars.Line_sid, QtCore.SIGNAL('editingFinished()'),
                   self.CB_line_text)
      self.connect(self.gvars.Line_gid, QtCore.SIGNAL('editingFinished()'),
                   self.CB_line_text)

      # and set layout
      layout.setMargin(g_spacing)
      bwidget.setLayout(layout)

      # --------------------------------------------------
      # and put main widgets into main VBox layout
      self.gvars.Widget_ID = bwidget

      return

   def make_basic_gbox(self, title):
      """make gbox with vars of frame and frame_layout
         - to be called at top of group_box_XXX
      """
              
      gbox = self.get_styled_group_box(title)

      # put frame inside gbox, which we can hide via toggled button
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      layout = QtGui.QVBoxLayout(frame)

      gbox.frame_layout = layout

      return gbox

   def set_basic_gbox_layout(self, gbox):
      """set layouts for basic gbox
         - to be called at bottom of group_box_XXX
         - matches make_basic_gbox
      """

      glayout = QtGui.QVBoxLayout(gbox)

      gbox.frame_layout.setMargin(g_spacing)
      gbox.frame_layout.setSpacing(g_glayout_spacing)
      gbox.frame.setLayout(gbox.frame_layout)
      glayout.addWidget(gbox.frame)
      glayout.setSpacing(g_spacing)
      glayout.setMargin(g_glayout_spacing)
      gbox.setLayout(glayout)

   def group_box_analysis(self):
      """create a group box with a VBox layout:
            HBox: Label(anal type) Chooser() Label(domain) Chooser() but(Apply)
                  Label(process blocks) LineEdit()

         for controlling sujbect vars: anal_type, anal_domain
                                       blocks

          ** no longer a group box
             -> just leave a line for subj/group
             -> move general analysis info down to input data and options
      """

      # make gbox with frame layout
      gbox = self.make_basic_gbox("analysis initialization")
      self.init_gbox_viewable(gbox, False)      # default to hidden
      flayout = gbox.frame_layout

      # --------------------------------------------------
      # analysis type HBox for type and domain

      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()

      # add analysis type
      label = QLIB.make_label("type",
                tip='analysis type to perfom: task or resting state')
      pbut = QLIB.create_menu_button(bwidget, self.svars.anal_type,
                USUBJ.g_def_anal_types, call_back=self.CB_gbox_PushB)
      self.gvars.PushB_anal_type = pbut
      layout.addWidget(label)
      layout.addWidget(pbut)

      # add analysis domain
      layout.addStretch()
      label = QLIB.make_label("domain",
                tip='data domain for analysis')
      pbut = QLIB.create_menu_button(bwidget, self.svars.anal_domain,
                USUBJ.g_def_anal_domains, call_back=self.CB_gbox_PushB)
      self.gvars.PushB_anal_domain = pbut
      layout.addWidget(label)
      layout.addWidget(pbut)

      # add Apply button
      layout.addStretch()
      pb = QLIB.make_button("APPLY",
                tip="set defaults that differ across types and domains",
                cb=self.CB_gbox_PushB)
      pb.setIcon(self.style().standardIcon(QtGui.QStyle.SP_MediaPlay))
      layout.addWidget(pb)
      self.gvars.PushB_anal_apply = pb

      # add help button
      layout.addStretch()
      pb = QLIB.make_button("help",
                tip="analysis initialization: type and data domain",
                cb=self.CB_gbox_PushB)
      pb.setIcon(self.style().standardIcon(QtGui.QStyle.SP_MessageBoxQuestion))
      layout.addWidget(pb)
      self.gvars.PushB_anal_help = pb

      bwidget.setLayout(layout)

      # and finally, add current widget to frame layout
      flayout.addWidget(bwidget)

      # --------------------------------------------------
      # processing blocks HBox
      bwidget = QLIB.create_label_lineedit_widget('processing blocks',
                        ltip='list of processing blocks in analysis',
                        etext=' '.join(self.svars.val('blocks')),
                        ecb=self.CB_line_text)
      self.gvars.Line_blocks = bwidget.LineEdit
      flayout.addWidget(bwidget)

      # --------------------------------------------------
      # and put main widgets into main VBox layout
      self.set_basic_gbox_layout(gbox)

      return gbox

   def CB_line_text(self):
      """call-back for text updates in the level 3 gbox"""
      obj = self.sender()
      if obj == self.gvars.Line_sid:
         self.update_textLine_check(obj, obj.text(), 'sid', 'subject ID',
                                    QLIB.valid_as_identifier)
      elif obj == self.gvars.Line_gid:
         self.update_textLine_check(obj, obj.text(), 'gid', 'group ID',
                                    QLIB.valid_as_identifier)
      elif obj == self.gvars.Line_blocks:
         self.set_blocks(str(obj.text()))
      elif obj == self.gvars.Line_anat:
         self.update_textLine_check(obj, obj.text(), 'anat', 'anatomical dset',
                                    QLIB.valid_as_filepath)
      elif obj == self.gvars.Line_apply_basis:
         self.update_basis_function(obj.text())
      elif obj == self.gvars.Line_apply_stype:
         self.update_stim_type(obj.text())

      elif obj == self.gvars.Line_tcat_nfirst:
         self.update_textLine_check(obj, obj.text(), 'tcat_nfirst',
                                    'first TRs to remove', QLIB.valid_as_int)

      # maybe we need to write a valid_in_list validator...
      elif obj == self.gvars.Line_volreg_base:
         text = str(obj.text())
         if text == '': text = USUBJ.g_def_vreg_base
         if text in USUBJ.g_vreg_base_list:
            self.set_svar('volreg_base', text)
         else: # reset to previous value
            self.gvars.Line_volreg_base.setText(self.svars.volreg_base)
            QLIB.guiWarning("Error: invalid volreg base",
                            "base '%s' not in %s\n\n" \
                            % (text, ', '.join(USUBJ.g_vreg_base_list)), obj)
          
      elif obj == self.gvars.Line_blur_size:
         self.update_textLine_check(obj, obj.text(), 'blur_size',
                                    'FWHM blur size', QLIB.valid_as_float)

      elif obj == self.gvars.Line_motion_limit:
         self.update_textLine_check(obj, obj.text(), 'motion_limit',
                                    'motion censor limit', QLIB.valid_as_float)

      elif obj == self.gvars.Line_outlier_limit:
         self.update_textLine_check(obj, obj.text(), 'outlier_limit',
                                 'outlier fraction limit', QLIB.valid_as_float)

      elif obj == self.gvars.Line_regress_jobs:
         self.update_textLine_check(obj, obj.text(), 'regress_jobs',
                                 'CPU jobs in regression', QLIB.valid_as_int)

      elif obj == self.gvars.Line_regress_GOFORIT:
         self.update_textLine_check(obj, obj.text(), 'regress_GOFORIT',
                                 'GOFORIT warning override', QLIB.valid_as_int)

      elif obj == self.gvars.Line_regress_bandpass:
         self.set_bandpass(str(obj.text()))

      elif obj == self.gvars.Line_align_cost:
         text = str(obj.text())
         if text == '': text = USUBJ.g_def_align_cost
         if text in USUBJ.g_align_cost_list:
            self.set_svar('align_cost', text)
         else: # reset to previous value
            # keep self.gvars.Line_align_cost.setText(self.svars.align_cost)
            self.set_svar('align_cost', text)  # apply anyway
            QLIB.guiWarning("Warning: unknown alignment cost function",
                            "cost '%s' not in %s\n\n" \
                            % (text, ', '.join(USUBJ.g_align_cost_list)), obj)

      elif obj == self.gvars.Line_tlrc_base:
         text = str(obj.text())
         if text == '': text = USUBJ.g_def_tlrc_base
         if text in USUBJ.g_tlrc_base_list:
            self.set_svar('tlrc_base', text)
         else: # reset to previous value
            # keep self.gvars.Line_tlrc_base.setText(self.svars.tlrc_base)
            self.set_svar('tlrc_base', text)  # apply anyway
            QLIB.guiWarning("Warning: unknown template",
                            "tlrc_base '%s' not in %s\n\n" \
                            % (text, ', '.join(USUBJ.g_tlrc_base_list)), obj)

      else: print('** CB_line_text: unknown sender')

   def set_blocks(self, bstring):
      blist = bstring.split()
      if UTIL.lists_are_same(blist, self.svars.blocks): return

      if len(blist) == 0: return self.apply_svar_in_gui('blocks')

      # something to do
      errstr = ''
      for block in blist:
         if not block in USUBJ.g_def_blocks_all:
            errstr += "** invalid processing block: %s\n" % block
      if errstr != '':
         QLIB.guiError('Error', errstr+'\nresetting to previous blocks...',self)
         return self.apply_svar_in_gui('blocks')

      self.set_svar('blocks', blist)

   def set_bandpass(self, bstring):
      flist = UTIL.string_to_float_list(bstring)
      if bstring == None:
         self.set_svar('regress_bandpass', [])
         return
      if flist == None:
         QLIB.guiError('Error',
                       "** invalid bandpass values\n"   \
                       "   (2 floats are required)\n\n" \
                       "   e.g. 0.01 0.1\n\n"           \
                       "   resetting to previous...",
                       self)
         bpstr = ' '.join(self.svars.val('regress_bandpass'))
         self.gvars.Line_regress_bandpass.setText(bpstr)
         return

      if len(flist) == 0: self.set_svar('regress_bandpass', [])
      elif len(flist) == 2: self.set_svar('regress_bandpass', bstring.split())
      else:
         QLIB.guiError('Error',
                       "** invalid bandpass values\n"           \
                       "   (exactly 2 floats are required)\n\n" \
                       "   e.g. 0.01 0.1\n\n"                   \
                       "   resetting to previous...",
                       self)
         bpstr = ' '.join(self.svars.val('regress_bandpass'))
         self.gvars.Line_regress_bandpass.setText(bpstr)

   def make_l3_group_boxes(self):
      """create anat, EPI, stim, etc. group boxes, and add to m2_vlayout"""

      self.gvars.gbox_analysis = self.group_box_analysis()
      self.gvars.gbox_anat = self.group_box_anat()
      self.gvars.gbox_epi  = self.group_box_epi()
      self.gvars.gbox_stim = self.group_box_stim()
      self.gvars.gbox_gltsym   = self.group_box_gltsym()
      self.gvars.gbox_expected = self.group_box_expected()
      self.gvars.gbox_regress  = self.group_box_regress_opts()
      self.gvars.gbox_align    = self.group_box_align()
      self.gvars.gbox_tlrc     = self.group_box_tlrc()

      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_analysis)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_anat)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_epi)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_stim)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_gltsym)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_expected)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_regress)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_align)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_tlrc)

   def group_box_align(self):
      """create a group box with a VBox layout:
            align_giant_move, align_cost,
            anat_has_skull, tlrc_base, tlrc_ok_maxite
            (rcr - later: maybe general align_opts_aea, tlrc_opts_at)
      """

      gbox = self.get_styled_group_box("extra align options")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, False)      # default to hidden
      # gbox.toggled.connect(self.gbox_toggle_frame)
      # no QtCore.SIGNAL('toggled()')?
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      layout = QtGui.QGridLayout(frame)         # now a child of frame
      voffset = 0                               # vertical position in layout

      # --------------------------------------------------
      # align_cost, with chooser
      label = QtGui.QLabel("align: cost function")
      label.setStatusTip("cost funtion for aligning EPI to anat")
      LE = QtGui.QLineEdit()
      # choose button
      blist = ['cost: %s' % cost for cost in USUBJ.g_align_cost_list]
      pbut = QLIB.create_menu_button(frame, "choose", blist,
                                     call_back=self.CB_gbox_PushB)
      LE.setText(self.svars.align_cost)
      self.connect(LE, QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, voffset, 0)
      layout.addWidget(pbut, voffset, 1)
      layout.addWidget(LE, voffset, 2)
      self.gvars.Line_align_cost = LE
      voffset += 1

      # --------------------------------------------------
      # checkbox: giant_move
      cbox = QtGui.QCheckBox("align: use giant_move")
      cbox.setStatusTip("use -giant_move in align_epi_anat.py")
      cbox.setChecked(self.svars.align_giant_move=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)

      layout.addWidget(cbox, voffset, 0)
      gbox.checkBox_align_giant_move = cbox
      voffset += 1

      # --------------------------------------------------
      # and finish group box
      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      glayout.addWidget(frame)
      glayout.setSpacing(g_glayout_spacing)
      gbox.setLayout(glayout)
      return gbox

   def group_box_tlrc(self):
      """create a group box with a VBox layout:
      """

      gbox = self.get_styled_group_box("extra tlrc options")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, False)      # default to hidden
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      layout = QtGui.QGridLayout(frame)         # now a child of frame
      voffset = 0                               # vertical position in layout

      # --------------------------------------------------
      # tlrc_base, with chooser
      label = QtGui.QLabel("tlrc: template")
      label.setStatusTip("anatomical group registration template")
      LE = QtGui.QLineEdit()
      # choose button
      blist = ['template: %s' % tt for tt in USUBJ.g_tlrc_base_list]
      pbut = QLIB.create_menu_button(frame, "choose", blist,
                                     call_back=self.CB_gbox_PushB)
      LE.setText(self.svars.tlrc_base)
      self.connect(LE, QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, voffset, 0)
      layout.addWidget(pbut, voffset, 1)
      layout.addWidget(LE, voffset, 2)
      self.gvars.Line_tlrc_base = LE
      voffset += 1

      # --------------------------------------------------
      # checkbox: anat_has_skull
      #cbox = QtGui.QCheckBox("anat has skull")
      #cbox.setStatusTip("check if input anat has skull, clear if not")
      #cbox.setChecked(self.svars.anat_has_skull=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      #self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)

      #layout.addWidget(cbox, voffset, 0)
      #gbox.checkBox_anat_has_skull = cbox
      #voffset += 1

      # --------------------------------------------------
      # checkbox: OK maxite
      cbox = QtGui.QCheckBox("tlrc: OK maxite")
      cbox.setStatusTip("allow max iterations in @auto_tlrc skull strip")
      cbox.setChecked(self.svars.tlrc_ok_maxite=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)

      layout.addWidget(cbox, voffset, 0)
      gbox.checkBox_tlrc_ok_maxite = cbox
      voffset += 1

      # --------------------------------------------------
      # and finish group box
      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      glayout.addWidget(frame)
      glayout.setSpacing(g_glayout_spacing)
      gbox.setLayout(glayout)
      return gbox

   def group_box_regress_opts(self):
      """create a group box with a VBox layout:
         for controlling sujbect vars:
            outlier_limit, regress_jobs, regress_GOFORIT, compute_fitts,
            reml_exec, run_clustsim, regress_opts_3dD
      """

      gbox = self.get_styled_group_box("extra regress options")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, False)      # default to hidden
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      layout = QtGui.QGridLayout(frame)         # now a child of frame
      lineno = 0

      # --------------------------------------------------

      # rcr - add help buttons for expected and extra regress options

      # outlier_limit
      label = QtGui.QLabel("outlier censor limit (per TR)")
      label.setStatusTip("censor TRs exceeding this fraction (range [0,1])")
      self.gvars.Line_outlier_limit = QtGui.QLineEdit()
      self.gvars.Line_outlier_limit.setText(self.svars.outlier_limit)
      self.connect(self.gvars.Line_outlier_limit,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, lineno, 0)
      layout.addWidget(self.gvars.Line_outlier_limit, lineno, 1)
      lineno += 1

      # jobs
      label = QtGui.QLabel("jobs for regression (num CPUs)")
      label.setStatusTip("number of CPUs to use in 3dDeconvolve")
      self.gvars.Line_regress_jobs = QtGui.QLineEdit()
      self.gvars.Line_regress_jobs.setText(self.svars.regress_jobs)
      self.connect(self.gvars.Line_regress_jobs,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, lineno, 0)
      layout.addWidget(self.gvars.Line_regress_jobs, lineno, 1)
      lineno += 1

      # GOFORIT
      label = QtGui.QLabel("GOFORIT level (override 3dD warnings)")
      label.setStatusTip("number of 3dDeconvolve warnings to override")
      self.gvars.Line_regress_GOFORIT = QtGui.QLineEdit()
      self.gvars.Line_regress_GOFORIT.setText(self.svars.regress_GOFORIT)
      self.connect(self.gvars.Line_regress_GOFORIT,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, lineno, 0)
      layout.addWidget(self.gvars.Line_regress_GOFORIT, lineno, 1)
      lineno += 1

      # regress bandpass (entry takes 2 values)
      label = QLIB.make_label("bandpass in regression (2 floats)",
                tip="give bottom/top bandpass frequencies (e.g. 0.01 0.1)")
      if self.svars.val_len('regress_bandpass') == 2:
         bptext = ' '.join(self.svars.val('regress_bandpass'))
      else: bptext = ''
      self.gvars.Line_regress_bandpass = \
              QLIB.make_line(bptext, cb=self.CB_line_text)

      layout.addWidget(label, lineno, 0)
      layout.addWidget(self.gvars.Line_regress_bandpass, lineno, 1)
      lineno += 1

      # checkbox : regress_mot_deriv
      cbox = QtGui.QCheckBox("regress motion derivatives")
      cbox.setStatusTip("regress motion derivatives (in addition to motion)")
      cbox.setChecked(self.svars.regress_mot_deriv=='yes')
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)
      layout.addWidget(cbox, lineno, 0)
      lineno += 1
      gbox.checkBox_mot_deriv = cbox

      # checkbox : reml_exec
      cbox = QtGui.QCheckBox("execute 3dREMLfit")
      cbox.setStatusTip("execute 3dREMLfit regression script")
      cbox.setChecked(self.svars.reml_exec=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)
      layout.addWidget(cbox, lineno, 0)
      lineno += 1
      gbox.checkBox_reml_exec = cbox

      # checkbox : run_clustsim
      cbox = QtGui.QCheckBox("run cluster simulation")
      cbox.setStatusTip("store 3dClustSim table in stats results")
      cbox.setChecked(self.svars.run_clustsim=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)
      layout.addWidget(cbox, lineno, 0)
      lineno += 1
      gbox.checkBox_run_clustsim = cbox

      # checkbox : compute_fitts
      cbox = QtGui.QCheckBox("compute fitts dataset")
      cbox.setStatusTip("save RAM in 3dD by computing fitts afterwards")
      cbox.setChecked(self.svars.compute_fitts=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)
      layout.addWidget(cbox, lineno, 0)
      lineno += 1
      gbox.checkBox_compute_fitts = cbox

      # --------------------------------------------------
      # and finish group box
      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      glayout.addWidget(frame)
      glayout.setSpacing(g_glayout_spacing)
      gbox.setLayout(glayout)
      return gbox

   def group_box_expected(self):
      """create a group box with a VBox layout:
         for controlling sujbect vars: tcat_nfirst, volreg_base, blur_size
                                       motion_limit
      """

      gbox = self.get_styled_group_box("expected options")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      layout = QtGui.QGridLayout(frame)         # now a child of frame
      posn = 0

      # --------------------------------------------------
      # tcat_nfirst
      label = QtGui.QLabel("first TRs to remove (per run)")
      label.setStatusTip("the number of pre-steady state TRs to remove")
      self.gvars.Line_tcat_nfirst = QtGui.QLineEdit()
      self.gvars.Line_tcat_nfirst.setText(self.svars.tcat_nfirst)
      self.connect(self.gvars.Line_tcat_nfirst,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, posn, 0)
      layout.addWidget(self.gvars.Line_tcat_nfirst, posn, 2)
      posn += 1

      # --------------------------------------------------
      # volreg_base
      label = QtGui.QLabel("volume registration base")
      label.setStatusTip("EPI registration volume (after TR removal)")
      self.gvars.Line_volreg_base = QtGui.QLineEdit()
      # choose button
      blist = ['vr base: %s' % base for base in USUBJ.g_vreg_base_list]
      pbut = QLIB.create_menu_button(frame, "choose", blist,
                call_back=self.CB_gbox_PushB)
      self.gvars.Line_volreg_base.setText(self.svars.volreg_base)
      self.connect(self.gvars.Line_volreg_base,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)

      layout.addWidget(label, posn, 0)
      layout.addWidget(pbut, posn, 1)
      layout.addWidget(self.gvars.Line_volreg_base, posn, 2)
      posn += 1

      # --------------------------------------------------
      # blur size
      label = QtGui.QLabel("blur size (FWHM in mm)")
      label.setStatusTip("Full Width at Half Max of gaussian blur to apply")
      self.gvars.Line_blur_size = QtGui.QLineEdit()
      self.gvars.Line_blur_size.setText(self.svars.blur_size)
      self.connect(self.gvars.Line_blur_size,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)
      layout.addWidget(label, posn, 0)
      layout.addWidget(self.gvars.Line_blur_size, posn, 2)
      posn += 1

      # --------------------------------------------------
      # motion_limit
      label = QtGui.QLabel("motion censor limit (mm, per TR)")
      label.setStatusTip("censor TRs with motion exceeding this mm distance")
      self.gvars.Line_motion_limit = QtGui.QLineEdit()
      self.gvars.Line_motion_limit.setText(self.svars.motion_limit)
      self.connect(self.gvars.Line_motion_limit,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)
      layout.addWidget(label, posn, 0)
      layout.addWidget(self.gvars.Line_motion_limit, posn, 2)
      posn += 1

      frame.setLayout(layout)
      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      glayout.addWidget(frame)
      glayout.setSpacing(g_glayout_spacing)
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
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      layout = QtGui.QVBoxLayout(frame) # now a child of frame

      # create an HBox Widget with 2 buttons
      labels = ['browse anat', 'clear anat', 'help']
      tips = ['browse file system for anatomical dataset',
              'clear anatomical dataset entry',
              'display help for this section' ]
      bwidget = QLIB.create_button_list_widget(labels, cb=self.CB_gbox_PushB,
                                         tips=tips, hind=2, style=self.style())
      gbox.bdict = bwidget.bdict
      layout.addWidget(bwidget)

      # create the anat file browsing dialog
      gbox.FileD = self.make_file_dialog("load anatomical dataset", "",
                     "*.HEAD;;*.nii;;*.nii.gz;;*")
                     #".HEAD files (*.HEAD);;.nii files (*.nii);;all files (*)")

      # add a line for the anat name, init to anat
      self.gvars.Line_anat = QtGui.QLineEdit()
      self.gvars.Line_anat.setText(self.svars.anat)
      self.connect(self.gvars.Line_anat,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)
      layout.addWidget(self.gvars.Line_anat)

      # --------------------------------------------------
      # checkbox: anat_has_skull
      cbox = QtGui.QCheckBox("anat has skull")
      cbox.setStatusTip("check if input anat has skull, clear if not")
      cbox.setChecked(self.svars.anat_has_skull=='yes')
      # cbox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)

      layout.addWidget(cbox)
      gbox.checkBox_anat_has_skull = cbox

      # --------------------------------------------------
      # add a checkbox for including tlrc, init with get_tlrc
      cbox = QtGui.QCheckBox("include copy of anat+tlrc")
      cbox.setChecked(self.svars.get_tlrc=='yes')
      # gbox.checkBox.clicked.connect(self.CB_checkbox)
      self.connect(cbox, QtCore.SIGNAL('clicked()'), self.CB_checkbox)
      layout.addWidget(cbox)
      gbox.checkBox = cbox

      # --------------------------------------------------
      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      frame.setLayout(layout)
      glayout.addWidget(frame)
      glayout.setSpacing(g_glayout_spacing)
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
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      mainlayout = QtGui.QVBoxLayout(frame)     # now a child of frame

      # --------------------------------------------------
      # create an HBox Widget with 2 buttons
      labels = ['browse EPI', 'clear EPI', 'help']
      tips = ['browse file system for EPI datasets',
              'clear EPI dataset entries',
              'display help for this section' ]
      bwidget = QLIB.create_button_list_widget(labels, cb=self.CB_gbox_PushB,
                                         tips=tips, hind=2, style=self.style())
      gbox.bdict = bwidget.bdict
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

      layout.setMargin(g_spacing)
      layout.setSpacing(g_grid_spacing)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a checkbox for use wildcard form
      gbox.checkBox_wildcard = QtGui.QCheckBox("use wildcard form")
      # gbox.checkBox_wildcard.clicked.connect(self.CB_checkbox)
      self.connect(gbox.checkBox_wildcard, QtCore.SIGNAL('clicked()'),
                   self.CB_checkbox)
      gbox.checkBox_wildcard.setChecked(self.svars.epi_wildcard=='yes')
      mainlayout.addWidget(gbox.checkBox_wildcard)

      # --------------------------------------------------
      mainlayout.setMargin(g_spacing)
      mainlayout.setSpacing(g_spacing)
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
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      mainlayout = QtGui.QVBoxLayout(frame)     # now a child of frame

      # --------------------------------------------------
      # create an HBox Widget with 2 buttons
      labels = ['browse stim', 'clear stim', 'help']
      tips = ['browse file system for stimulus timing files',
              'clear stim file entries',
              'display help for this section' ]
      bwidget = QLIB.create_button_list_widget(labels, cb=self.CB_gbox_PushB,
                                         tips=tips, hind=2, style=self.style())
      gbox.bdict = bwidget.bdict
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a table for the stim names, init to stim
      self.make_stim_table(gbox)

      # put table in its own HBox
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()
      layout.addWidget(self.gvars.Table_stim)
      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)
      # mainlayout.addWidget(self.gvars.Table_stim)

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

      layout.setMargin(g_spacing)
      layout.setSpacing(g_grid_spacing)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a new line for setting basis functions:
      # Label  PushButton.Menu   LineEdit
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()

      nlabel = QtGui.QLabel("init basis funcs:")
      nlabel.setStatusTip("initialization for all stim files")
      layout.addWidget(nlabel)

      blist = USUBJ.g_def_stim_basis_list
      blist = ['basis: %s'%basis for basis in blist]
      pbut = QLIB.create_menu_button(bwidget, "choose", blist,
                call_back=self.CB_gbox_PushB)
      layout.addWidget(pbut)

      self.gvars.Line_apply_basis  = QtGui.QLineEdit()
      if len(self.svars.stim_basis) > 0: basis = self.svars.stim_basis[0]
      else:                              basis = ''
      self.gvars.Line_apply_basis.setText(basis)
      self.connect(self.gvars.Line_apply_basis,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)
      layout.addWidget(self.gvars.Line_apply_basis)

      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a new line for setting file types:
      # Label  PushButton.Menu   LineEdit
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()

      nlabel = QtGui.QLabel("init file types:")
      nlabel.setStatusTip("initialization for all stim files")
      layout.addWidget(nlabel)

      blist = USUBJ.g_def_stim_types_list
      blist = ['stype: %s'%tt for tt in blist]
      pbut = QLIB.create_menu_button(bwidget, "choose", blist,
                call_back=self.CB_gbox_PushB)
      layout.addWidget(pbut)

      self.gvars.Line_apply_stype = QtGui.QLineEdit()
      if len(self.svars.stim_type) > 0: stype = self.svars.stim_type[0]
      else:                             stype = ''
      self.gvars.Line_apply_stype.setText(stype)
      self.connect(self.gvars.Line_apply_stype,
                   QtCore.SIGNAL('editingFinished()'), self.CB_line_text)
      layout.addWidget(self.gvars.Line_apply_stype)

      layout.setMargin(g_spacing)
      layout.setSpacing(g_spacing)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a checkbox for use wildcard form
      gbox.checkBox_wildcard = QtGui.QCheckBox("use wildcard form")
      # gbox.checkBox_wildcard.clicked.connect(self.CB_checkbox)
      self.connect(gbox.checkBox_wildcard, QtCore.SIGNAL('clicked()'),
                   self.CB_checkbox)
      gbox.checkBox_wildcard.setChecked(self.svars.stim_wildcard=='yes')
      mainlayout.addWidget(gbox.checkBox_wildcard)

      # --------------------------------------------------
      mainlayout.setMargin(g_spacing)
      mainlayout.setSpacing(g_spacing)
      frame.setLayout(mainlayout)

      return gbox

   def group_box_gltsym(self):
      """create a group box with a VBox layout for symbolic GLTs
                HBox:  QPushB(clear GLTs)  QPushB(help: gltsym)
                QTableWidget(label SYM:)
                QWidget, 3x2 Grid Layout
                    QLabel(dataset count)   QLabel(shadow text)

         for controlling sujbect vars: gltsym, gltsym_label
      """

      # --------------------------------------------------
      gbox = self.get_styled_group_box("symbolic GLTs")

      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      glayout.addWidget(frame)
      glayout.setSpacing(g_glayout_spacing)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, False)      # default to hidden
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      mainlayout = QtGui.QVBoxLayout(frame)     # now a child of frame

      # --------------------------------------------------
      # create an HBox Widget with 3 action buttons
      labels = ['insert glt row', 'init with examples']
      tips = ['append a blank row to the table',
              'initialize table with GLTs from stim labels' ]
      bwidget = QLIB.create_button_list_widget(labels, cb=self.CB_gbox_PushB,
                                               tips=tips, style=self.style())
      gbox.bdict = bwidget.bdict
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # create an HBox Widget with 2 buttons
      labels = ['resize glt table', 'clear glt table', 'help']
      tips = ['delete any blank rows from table',
              'clear all entries from table',
              'display help for this section' ]
      bwidget = QLIB.create_button_list_widget(labels, cb=self.CB_gbox_PushB,
                                         tips=tips, hind=2, style=self.style())
      for key in list(bwidget.bdict.keys()):  # copy additional keys
         gbox.bdict[key] = bwidget.bdict[key]
      mainlayout.addWidget(bwidget)

      ##### should we use a grid instead?
      # labels = ['insert glt row', 'init with glt examples', None,
      #           'resize glt table', 'clear glt table', 'help: gltsym']
      # tips   = ['append a blank row to the table',
      #           'initialize table with GLTs from stim labels', None,
      #           'delete any blank rows from table',
      #           'clear all entries from table',
      #           'display help for this section' ]
      # bwidget =QLIB.create_button_grid(labels,tips=tips,cb=self.CB_gbox_PushB)
      # icon = self.style().standardIcon(QtGui.QStyle.SP_MessageBoxQuestion)
      # gbox.blist = bwidget.blist
      # gbox.blist[4].setIcon(icon)
      # mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      # add a table for the stim names, init to stim
      self.make_gltsym_table(gbox)

      # put table in its own HBox
      bwidget = QtGui.QWidget()
      layout = QtGui.QHBoxLayout()
      layout.addWidget(self.gvars.Table_gltsym)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)
      # mainlayout.addWidget(self.gvars.Table_gltsym)

      # --------------------------------------------------
      # add lines for the number of datasets and wildcard form
      bwidget = QtGui.QWidget()
      layout = QtGui.QGridLayout()

      nlabel, tlabel = QLIB.create_display_label_pair('GLT count',
                                            '%s' % len(self.svars.gltsym))
      layout.addWidget(nlabel, 0, 0)
      layout.addWidget(tlabel, 0, 1)
      self.gvars.Label_gltsym_len = tlabel

      # basically fix column 0 and let column 1 grow
      layout.setColumnStretch(0, 1)
      layout.setColumnStretch(1, 20)

      layout.setMargin(0)
      bwidget.setLayout(layout)
      mainlayout.addWidget(bwidget)

      # --------------------------------------------------
      mainlayout.setMargin(g_spacing)
      mainlayout.setSpacing(g_spacing)
      frame.setLayout(mainlayout)

      return gbox

   def make_gltsym_table(self, parent=None):
      """init table from gltsym and _label arrays
         - 3 columns: index, label, SYM:
      """
      col_heads = ["label", "symbolic GLT"]
      nrows = len(self.svars.gltsym)             # one row per gltsym
      ncols = len(col_heads)

      table = QtGui.QTableWidget(nrows, ncols)
      table.stretch_cols = [1]                   # columns that should stretch

      table.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
      table.setHorizontalHeaderLabels(col_heads)

      self.resize_table_cols(table)

      # set and fill the table
      self.gvars.Table_gltsym = table
      self.gltsym_list_to_table()

   def gltsym_list_to_table(self, symlist=None, symlabs=None):
      """update Table_gltsym from gltsym and _label arrays
      """
      table = self.gvars.Table_gltsym           # convenience
      if symlist == None: sym  = self.svars.gltsym
      else:               sym  = symlist
      if symlabs == None: labs = self.svars.gltsym_label
      else:               labs = symlabs

      nrows = len(sym)

      table.setRowCount(0)                      # init, add rows per file
      table.setSortingEnabled(False)            # sort only after filling table

      if nrows <= 0:    # clear extra fields (if created) and return
         if self.gvars.valid('Label_gltsym_len'):
            self.gvars.Label_gltsym_len.setText('0')
            self.resize_table(table)
         return

      # if we don't have the correct number of labels, start from scratch
      if len(labs) != nrows:
         labs = ['' for glt in sym]

      if self.verb > 2: print("== gltsym table, nGLT = %d" % (nrows))

      # ------------------------------------------------------------
      # now fill table with label and GLTs

      for ind, glt in enumerate(sym):
         labItem = QtGui.QTableWidgetItem(labs[ind])
         nameItem = QtGui.QTableWidgetItem(glt)
         table.insertRow(ind)           # insert at end
         table.setItem(ind, 0, labItem)
         table.setItem(ind, 1, nameItem)

      table.resizeRowsToContents()
      table.setAlternatingRowColors(True)

      # set min size base on ~25 per row, with min of 75 and max of 200
      # table.setMinimumSize(100, self.max_table_size(nrows))
      table.setMinimumSize(100, 75)
      self.resize_table(table)

      # no sorting here
      table.setSortingEnabled(False)

      # ------------------------------------------------------------
      # and fill in Label_stim_ndsets, stim_dir, and stim_wildcard (form)
      self.gvars.Label_gltsym_len.setText('%d' % nrows)

   def make_file_dialog(self, title, dirname, filtname):
      fileD = QtGui.QFileDialog(self)
      # QString no longer exists in python3
      try:
         tstr = QtCore.QString(title)
         fstr = QtCore.QString(filtname)
         dstr = QtCore.QString(dirname)
      except:
         tstr = title
         fstr = filtname
         dstr = dirname
      fileD.setWindowTitle(tstr)
      fileD.setFilter(fstr)
      fileD.setDirectory(dstr)
      return fileD

   def make_epi_table(self, parent=None):
      """init table from epi array
         - only update epi array on directory scan and 'update AP command'
      """
      col_heads = ['scan index', 'EPI dataset']  # column headings
      nrows = len(self.svars.epi)                # one row per EPI dataset
      ncols = len(col_heads)

      table = QtGui.QTableWidget(nrows, ncols)
      table.stretch_cols = [1]                  # columns that should stretch

      table.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
      table.setHorizontalHeaderLabels(col_heads)

      self.resize_table_cols(table)

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

      if nrows <= 0:    # clear extra fields (if created) and return
         if self.gvars.valid('Label_epi_ndsets'):
            self.gvars.Label_epi_ndsets.setText('0')
            self.gvars.Label_epi_dir.setText('')
            self.gvars.Label_epi_wildcard.setText('')
            self.resize_table(table)
         return

      # parse the EPI list into directory, short names, glob string
      epi_dir, short_names, globstr = UTIL.flist_to_table_pieces(epi)

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
         print("== epi table, ndsets = %d, dir = %s" % (nrows, epi_dir))
         print("   wildcard string = %s" % globstr)
         print("   indlist  = %s" % indlist)

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
      # table.setMinimumSize(100, self.max_table_size(nrows))
      table.setMinimumSize(100, 75)
      self.resize_table(table)

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
      col_heads = ['index', 'label', 'basis', 'type', 'stim (timing) file']
      nrows = len(self.svars.stim)              # one row per stim file
      ncols = len(col_heads)

      table = QtGui.QTableWidget(nrows, ncols)
      table.stretch_cols = [4]                  # columns that should stretch

      table.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
      table.setHorizontalHeaderLabels(col_heads)

      self.resize_table_cols(table)

      # set and fill the table
      self.gvars.Table_stim = table
      self.stim_list_to_table()

   def resize_table_cols(self, table):
      """resize to column contents, unless it is in strech_cols"""

      ncols = table.columnCount()

      # resize columns to fit data
      for col in range(ncols):          # to be safe, work on column indices
         if col in table.stretch_cols:
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

      if nrows <= 0:    # clear extra fields (if created) and return
         if self.gvars.valid('Label_stim_ndsets'):
            self.gvars.Label_stim_ndsets.setText('0')
            self.gvars.Label_stim_dir.setText('')
            self.gvars.Label_stim_wildcard.setText('')
            self.resize_table(table)
         return

      # parse the stim list into directory, short names, glob string
      stim_dir, short_names, globstr = UTIL.flist_to_table_pieces(stim)

      # ------------------------------------------------------------

      # get index and label lists
      stim_table = UTIL.parse_as_stim_list(short_names)
      indlist = [entry[0] for entry in stim_table]

      # if we don't have the correct number of labels, override make_labs
      if len(self.svars.stim_label) != nrows: make_labs = 1
      if make_labs: lablist = [entry[1] for entry in stim_table]
      else: lablist = self.svars.stim_label

      if self.verb > 2:
         print("== stim table, ndsets = %d, dir = %s" % (nrows, stim_dir))
         print("   wildcard string = %s" % globstr)
         print("   stim_table: %s" % stim_table)

      haveinds = 1
      for ind, val in enumerate(indlist):
         if val < 0:
            if self.verb > 1:
               print('-- bad stim_table index %d = %d, skipping indices' \
                     % (ind, val))
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
         tt = '** len(stim_basis) == %d, but have %d stim\n\n' \
              '   clearning list, please fill...' % (nbases, nrows)
         update_AP_warn_window(tt)
         bases = ['' for i in range(nrows)]
      else:
         bases = self.svars.stim_basis

      # ------------------------------------------------------------
      # make stim_type list
      ntypes = len(self.svars.stim_type)
      if ntypes == 0:
         stypes = ['times' for i in range(nrows)]
      elif ntypes == 1:
         stypes = [self.svars.stim_type[0] for i in range(nrows)]
      elif ntypes != nrows:
         tt = '** len(stim_type) == %d, but have %d stim\n\n' \
              '   clearning list, please fill...' % (ntypes, nrows)
         update_AP_warn_window(tt)
         stypes = ['' for i in range(nrows)]
      else:
         stypes = self.svars.stim_type

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
         typeItem = QtGui.QTableWidgetItem(stypes[ind])
         nameItem = QtGui.QTableWidgetItem(dset)
         table.insertRow(ind)           # insert at end
         table.setItem(ind, 0, indItem)
         table.setItem(ind, 1, labItem)
         table.setItem(ind, 2, basisItem)
         table.setItem(ind, 3, typeItem)
         table.setItem(ind, 4, nameItem)

      table.resizeRowsToContents()
      table.setAlternatingRowColors(True)
      # table.setDragDropMode(QtGui.QAbstractItemView.InternalMove)
      # table.setDragEnabled(True)

      # set min size base on ~25 per row, with min of 75 and max of 200
      # table.setMinimumSize(100, self.max_table_size(nrows))
      table.setMinimumSize(100, 75)
      self.resize_table(table)

      # if we have a stim index, default to using it for sorting
      table.setSortingEnabled(True)
      if haveinds: table.sortItems(0)
      else:        table.sortItems(4)

      # ------------------------------------------------------------
      # and fill in Label_stim_ndsets, stim_dir, and stim_wildcard (form)
      self.gvars.Label_stim_ndsets.setText('%d' % nrows)
      self.gvars.Label_stim_dir.setText(stim_dir)
      self.gvars.Label_stim_wildcard.setText(globstr)

   def max_table_size(self, nrows, rheight=0):
      """return the number of pixels to use based on the number of rows
         (scale by any passed row height)"""
      if rheight > 0: rowheight = rheight
      else:           rowheight = 25

      if nrows <= 6:   show_rows = nrows
      elif nrows < 18: show_rows = 6+0.5*(nrows-6)  # after 6, add half, per
      else:            show_rows = 12

      return min(200, max(75, rheight*(show_rows+1.25)))

   def CB_checkbox(self):
      """call-back for any check boxes"""
      obj = self.sender()
      if   obj == self.gvars.gbox_anat.checkBox:
         if obj.isChecked(): self.set_svar('get_tlrc', 'yes')
         else:               self.set_svar('get_tlrc', 'no')
      elif obj == self.gvars.gbox_anat.checkBox_anat_has_skull:
         if obj.isChecked(): self.set_svar('anat_has_skull', 'yes')
         else:               self.set_svar('anat_has_skull', 'no')
      elif obj == self.gvars.gbox_epi.checkBox_wildcard:
         if obj.isChecked(): self.set_svar('epi_wildcard', 'yes')
         else:               self.set_svar('epi_wildcard', 'no')
      elif obj == self.gvars.gbox_stim.checkBox_wildcard:
         if obj.isChecked(): self.set_svar('stim_wildcard', 'yes')
         else:               self.set_svar('stim_wildcard', 'no')
      elif obj == self.gvars.gbox_regress.checkBox_mot_deriv:
         if obj.isChecked(): self.set_svar('regress_mot_deriv', 'yes')
         else:               self.set_svar('regress_mot_deriv', 'no')
      elif obj == self.gvars.gbox_regress.checkBox_reml_exec:
         if obj.isChecked(): self.set_svar('reml_exec', 'yes')
         else:               self.set_svar('reml_exec', 'no')
      elif obj == self.gvars.gbox_regress.checkBox_run_clustsim:
         if obj.isChecked(): self.set_svar('run_clustsim', 'yes')
         else:               self.set_svar('run_clustsim', 'no')
      elif obj == self.gvars.gbox_regress.checkBox_compute_fitts:
         if obj.isChecked(): self.set_svar('compute_fitts', 'yes')
         else:               self.set_svar('compute_fitts', 'no')
      elif obj == self.gvars.gbox_align.checkBox_align_giant_move:
         if obj.isChecked(): self.set_svar('align_giant_move', 'yes')
         else:               self.set_svar('align_giant_move', 'no')
      elif obj == self.gvars.gbox_tlrc.checkBox_tlrc_ok_maxite:
         if obj.isChecked(): self.set_svar('tlrc_ok_maxite', 'yes')
         else:               self.set_svar('tlrc_ok_maxite', 'no')
      else: print("** CB_checkbox: unknown sender")

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
         self.set_svar(attr, rtext)
         if type(obj) == g_LineEdittype: obj.setText(text)
         else: print('** update_textLine_check: not a LineEdit type')
         return 1
      else:
         # error, reset to previous attribute
         # obj.clear()
         obj.setText(self.svars.val(attr))
         obj.setFocus()
         return 0

   def cb_command_window(self, cmd):
      print('++ python exec command: %s' % cmd)
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

   def apply_PushB_action(self, sender, vname):
      """if sender is an action in PushB_VNAME.act_dict, apply
            svars.vname = applied key
            set new text in button

         return 1 if applied, -1 if canceled, and 0 otherwise
      """
      pbobj = self.gvars.val('PushB_%s' % vname)
      if pbobj == None: return 0

      for key in list(pbobj.act_dict.keys()):
         act = pbobj.act_dict[key]
         if sender == act:      # found!
            if key == 'CANCEL': return -1
            self.svars.set_var(vname, key)
            pbobj.setText(key)
            return 1

      return 0

   def CB_gbox_PushB(self):
      """these buttons are associated with anat/EPI/stim file group boxes
         - the sender (button) text must be unique"""

      try:
         sender = self.sender()
         text = str(sender.text())
      except:
         print('** CB_gbox_PushB: no text')
         return

      # analysis init: help and APPLY
      if self.apply_PushB_action(sender, 'anal_type'): return
      elif self.apply_PushB_action(sender, 'anal_domain'): return

      elif sender == self.gvars.PushB_anal_help:
         
         cstr = USUBJ.g_rdef_strs.changed_attrs_str(USUBJ.g_sdef_strs,
                           skiplist='name', showskip=0, showdel=0)
         hstr  = "%s\n\nAnalysis type 'rest': %s" % (g_help_init, cstr)
         self.update_help_window(hstr, title='analysis initialization')

      elif sender == self.gvars.PushB_anal_apply:
         self.init_analysis_defaults() # apply anal_type and anal_domain

      # anat
      elif sender == self.gvars.gbox_anat.bdict['help']:
         self.update_help_window(g_help_anat, title='anatomical datasets')

      elif text == 'browse anat':
         fname = QtGui.QFileDialog.getOpenFileName(self,
                   "load anatomical dataset", self.pick_base_dir('anat'),
                   "datasets (*.HEAD *.nii *.nii.gz);;all files (*)")
         self.update_textLine_check(self.gvars.Line_anat,
                fname, 'anat', 'anatomical dset', QLIB.valid_as_filepath)

      elif text == 'clear anat':
         self.gvars.Line_anat.clear()
         self.set_svar('anat','')

      # EPI
      # elif text == 'help: EPI':
      elif sender == self.gvars.gbox_epi.bdict['help']:
         self.update_help_window(g_help_epi, title='EPI datasets')

      elif text == 'browse EPI':
         fnames = QtGui.QFileDialog.getOpenFileNames(self,
                        "load EPI datasets", self.pick_base_dir('epi'),
                        "datasets (*.HEAD *.nii *.nii.gz);;all files (*)")
         if len(fnames) > 0:
            self.set_svar('epi', [str(name) for name in fnames])
            self.epi_list_to_table()

      elif text == 'clear EPI':
            self.set_svar('epi', [])
            self.epi_list_to_table()

      # stim
      elif sender == self.gvars.gbox_stim.bdict['help']:
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
            self.set_svar('stim_type', [])
            self.stim_list_to_table(make_labs=1)

      elif text == 'clear stim':
         self.set_svar('stim', [])
         self.stim_list_to_table()

      elif text[0:7] == 'basis: ':
         self.update_basis_function(text[7:])

      elif text[0:7] == 'stype: ':
         self.update_stim_type(text[7:])

      # expected
      elif text[0:9] == 'vr base: ':
         base = text[9:]
         self.set_svar('volreg_base', base)
         self.gvars.Line_volreg_base.setText(base)

      # GLT table buttons

      elif text == 'insert glt row':
         table = self.gvars.Table_gltsym
         nrows = table.rowCount()
         table.insertRow(nrows)
         self.resize_table(table, self.gvars.Label_gltsym_len)

      elif text == 'init with examples':

         # get labels to apply
         if self.update_stim_from_table(): return
         if len(self.svars.stim_label) < 2: 
            QLIB.guiWarning('Sorry', '** need at least 2 stim labels for GLTs',
                            self)
            return
         gltsym, gltlabs = USUBJ.make_gltsym_examples(self.svars.stim_label)
         self.gltsym_list_to_table(gltsym, gltlabs)
         self.resize_table(self.gvars.Table_gltsym, self.gvars.Label_gltsym_len)

      # elif text == 'help: gltsym':
      elif sender == self.gvars.gbox_gltsym.bdict['help']:
         self.update_help_window(g_help_gltsym, title='symbolic GLTs')

      elif text == 'resize glt table':
         """delete any rows without text"""
         self.remove_blank_table_rows(self.gvars.Table_gltsym,
                                      self.gvars.Label_gltsym_len)

      elif text == 'clear glt table':
         self.set_svar('gltsym', [])
         self.set_svar('gltsym_label', [])
         self.gltsym_list_to_table()
         self.resize_table(self.gvars.Table_gltsym, self.gvars.Label_gltsym_len)

      elif text[0:6] == 'cost: ':
         self.update_align_cost(text[6:])

      elif text[0:10] == 'template: ':
         self.update_tlrc_base(text[10:])

      else: print("** unexpected button text: %s" % text)

   def resize_table(self, table, countLabel=None):
      nrows = table.rowCount()
      if nrows > 0: rheight = table.rowHeight(0)
      else        : rheight = 0
      if self.verb > 2: print('-- resize_table: using row height %d' % rheight)
      table.setAlternatingRowColors(True)
      table.setFixedHeight(self.max_table_size(nrows, rheight=rheight))
      table.resizeRowsToContents()
      self.resize_table_cols(table)

      if countLabel != None: countLabel.setText('%d' % nrows)

   def remove_blank_table_rows(self, table, countLabel=None):

      # --------------------------------------------------
      # gltsym table (get labels and GLTs)
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
         print('** pick_base_dir: bad dtype = %s' % dtype)

      return ''

   def update_align_cost(self, cost):
      if len(cost) == 0: return
      if self.verb > 1: print('++ applying cost function %s' % cost)
      self.gvars.Line_align_cost.setText(cost)

      if self.svars.val('align_cost') != cost:  # update on change
         self.set_svar('align_cost', cost)

   def update_tlrc_base(self, base):
      if len(base) == 0: return
      if self.verb > 1: print('++ applying tlrc_base %s' % base)
      self.gvars.Line_tlrc_base.setText(base)

      if self.svars.val('tlrc_base') != base:  # update on change
         self.set_svar('tlrc_base', base)

   def update_basis_function(self, basis):
      if len(basis) > 0 and not self.basis_func_is_current(basis):
         self.update_stim_from_table() # apply any updates to variables
         if self.verb > 1: print('++ applying basis function %s' % basis)
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

   def update_stim_type(self, stype):
      if len(stype) > 0 and not self.stim_type_is_current(stype):
         self.update_stim_from_table() # apply any updates to variables
         if self.verb > 1: print('++ applying stim_type %s' % stype)
         self.gvars.Line_apply_stype.setText(stype)
         nstim = len(self.svars.stim)
         self.set_svar('stim_type',[stype for i in range(nstim)])
         self.stim_list_to_table()

   def stim_type_is_current(self, stype):
      """check a few things:
           - stim_type must have length 1 or len(stim)
           - each entry must match 'stype'"""
      nstim = len(self.svars.stim)
      ntypes = len(self.svars.stim_type)

      if ntypes == 0:   # empty is special, since we cannot access entries
         if nstim == 0: return 1
         else:          return 0

      # next check for matching lengths (or unit)
      if ntypes > 1 and ntypes != nstim: return 0

      # finally, check the entries
      for stim_type in self.svars.stim_type:
         if stim_type != stype: return 0

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

      act4 = self.createAction("reset all options",
          slot=self.cb_clear_options,
          tip="keep datasets: restore all other GUI options to defaults",
          icon=self.style().standardIcon(QtGui.QStyle.SP_TrashIcon))
      act5 = self.createAction("reset all fields",
          slot=self.cb_clear_fields,
          tip="restore all GUI fields to defaults",
          icon=self.style().standardIcon(QtGui.QStyle.SP_TrashIcon))

      self.gvars.act_show_ap    = act1
      self.gvars.act_exec_ap    = act2
      self.gvars.act_exec_proc  = act3
      self.gvars.act_clear_opts = act4
      self.gvars.act_clear_all  = act5

      self.gvars.act_exec_ap.setEnabled(False)
      self.gvars.act_exec_proc.setEnabled(False)

      # ----------------------------------------------------------------------
      # File menu
      self.gvars.MBar_file = self.menuBar().addMenu("&File")
      actFileQuit = self.createAction("&Quit", slot=self.close,
          shortcut="Ctrl+q", tip="close the application")
      self.addActions(self.gvars.MBar_file, [act1, act2, act3, None,
                                             act4, act5, None, actFileQuit])

      # ----------------------------------------------------------------------
      # View menu - all for static view windows
      self.gvars.MBar_view = self.menuBar().addMenu("&View")

      act0 = self.createAction("updated variables",
        slot=self.cb_view,
        tip="display changes from defaults variables")

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

      self.addActions(self.gvars.MBar_view, [act0, None,
                                             act1, act2, act3, None, act4])

      self.gvars.act_view_vars     = act0
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

      act7 = self.createAction("view: GUI vars",
        slot=self.cb_view,
        tip="display GUI variables")

      self.gvars.act_view_svars   = act3
      self.gvars.act_view_cvars   = act4
      self.gvars.act_view_rvars   = act5
      self.gvars.act_view_gvars   = act7

      self.addActions(self.gvars.MBar_hidden, [act3, act4, act5, None])

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

      self.addActions(self.gvars.MBar_hidden, [None, act6, act7])

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
          slot=self.cb_help_browse, tip="browse AFNI program help")
      act2 = self.createAction("web: afni_proc.py help",
          slot=self.cb_help_browse, tip="browse afni_proc.py help")
      act3 = self.createAction("web: current class handouts",
          slot=self.cb_help_browse, tip="browse current AFNI bootcamp handouts")
      act4 = self.createAction("web: tutorial-single subject analysis",
          slot=self.cb_help_browse, tip="browse AFNI_data6 tutorial")
      act5 = self.createAction("web: AFNI Message Board",
          slot=self.cb_help_browse, tip="browse Message Board")
      act6 = self.createAction("web: gltsym and stim times",
          slot=self.cb_help_browse, tip="2004 update describing GLT usage")

      self.addActions(self.gvars.Menu_browse, [act1, act2, act3, act4, act5,
                                               act6])

      self.gvars.act_browse_all_progs = act1
      self.gvars.act_browse_AP_help   = act2
      self.gvars.act_browse_class_notes = act3
      self.gvars.act_browse_SS_tutor  = act4
      self.gvars.act_browse_MB        = act5
      self.gvars.act_browse_D2004_glt = act6

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
      # for help text
      self.gvars.Text_help         = QLIB.TextWindow(parent=self)
      # for output messages from AP_Subject class processing
      self.gvars.Text_AP_result    = QLIB.TextWindow(parent=self)
      # to show applied subject variables
      self.gvars.Text_applied_vars = QLIB.TextWindow(parent=self)

      # note whether we have a browser via import
      self.gvars.browser = None
      try: 
         import webbrowser
         self.gvars.browser = webbrowser
         if self.verb > 1: print('++ have browser')
      except:
         if self.verb > 1: print('-- NO browser')

   def open_web_site(self, site):
      if self.gvars.browser == None:
         QLIB.guiWarning('Error', 'no browser to use for site: %s'%site,self)
      else: self.gvars.browser.open(site)

   def get_changed_attrs_string(self):
      """show changes from defaults
         note: this may depend on analysis type (surf? rest?)
      """

      return self.svars.changed_attrs_str(USUBJ.g_sdef_strs,
                        skiplist=USUBJ.g_svars_not_opt, showskip=1)

   def update_applied_vars_window(self, win=None, text='', title='', fname=''):
      """default window is Text_AP_result
         - if fname, read file
           else if text, use text
           else (likely case), show changed_attrs
      """
      if win: window = win
      else:   window = self.gvars.Text_applied_vars

      if text == '': text = self.get_changed_attrs_string()

      if title: window.setWindowTitle(title)
      if fname: # then read from file
         window.filename = fname
         window.readfile()
      else: window.editor.setText(text)
      window.show()
      window.raise_()

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
      self.update_help_window(USUBJ.helpstr_usubj_gui,
                              title='uber_subject.py: main help')

   def cb_help_about(self):
      """display version info in Text_Help window"""
      text = "uber_subject.py, version %s" % USUBJ.g_version
      QLIB.guiMessage('about uber_subject.py', text, self)

   def cb_help_browse(self):
      obj = self.sender()
      if   obj == self.gvars.act_browse_all_progs:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/index.html')
      elif obj == self.gvars.act_browse_AP_help:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/afni_proc.py.html')
      elif obj == self.gvars.act_browse_SS_tutor:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/edu/data' \
                            '/CD.expanded/AFNI_data6/FT_analysis/tutorial')
      elif obj == self.gvars.act_browse_class_notes:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/edu' \
                            '/latest/afni_handouts')
      elif obj == self.gvars.act_browse_MB:
         self.open_web_site('https://afni.nimh.nih.gov/afni/community/board')
      elif obj == self.gvars.act_browse_D2004_glt:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/misc/Decon/DeconSummer2004.html')
      else: print('** cb_help_browse: invalid sender')

   def update_svars_from_gui(self, warn=0):
      """set what we can, if warn, report error
         return 0 on success, 1 on error"""

      # first process sid and gid
      if self.svars.is_empty('sid') or self.svars.is_empty('gid'):
         if warn: QLIB.guiError('Error', 
                                "** subject and group IDs must be set", self)
         return 1

      # then process tables
      if self.update_svars_from_tables(): return 1

      # if still default, create new subj_dir name
      if self.set_sdir:
         # subj dir should read: subject_results/group.gA/subj.SUBJ
         sdir =  USUBJ.get_def_subj_path(gid=self.svars.gid, sid=self.svars.sid)
         if sdir != self.cvars.val('subj_dir'):
            if self.verb: print('-- setting subj_dir to %s' % sdir)
            self.set_cvar('subj_dir', sdir)

      return 0

   def cb_show_ap_command(self):

      if self.update_svars_from_gui(warn=1): return

      # if we have a subject directory, make backup scripts
      if self.cvars.is_non_trivial_dir('subj_dir'):
         self.set_cvar('copy_scripts', 'yes')
        
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

      self.update_applied_vars_window(title="Applied Variables")

      self.update_AP_result_window(title="Success!  afni_proc.py command:",
                                   fname=fname)
      if nwarn > 0: self.update_AP_warn_window(wstr)

      # if we have a subject dir that exists, save the US.py command there
      self.write_uber_subj_command()

      self.gvars.act_exec_ap.setEnabled(True)
      self.gvars.act_exec_proc.setEnabled(True)

   def write_uber_subj_command(self):
      sdir = self.apsubj.cvars.val('subj_dir')
      if os.path.isdir(sdir):
         sstr = self.make_uber_command()
         sid = self.apsubj.svars.val('sid')
         UTIL.write_text_to_file('%s/.orig.cmd.usubj.%s'%(sdir,sid), sstr)

   def update_svars_from_tables(self):
      """get updates from the EPI, stim and gltsym tables
         
         return 0 on success, 1 on error"""

      if self.update_EPI_from_table(): return 1
      if self.update_stim_from_table(): return 1
      if self.update_gltsym_from_table(): return 1

      return 0

   def update_EPI_from_table(self):

      # --------------------------------------------------
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

      return 0

   def update_stim_from_table(self):
      """have 5 columns: index, label, basis, type, file

         note: index is used only as a sorting option
      """

      # --------------------------------------------------
      # stim table
      dir = str(self.gvars.Label_stim_dir.text())
      table = self.gvars.Table_stim             # convenience
      nrows = table.rowCount()
      llist = []        # labels
      blist = []        # bases
      tlist = []        # types
      dlist = []        # dsets (files)
      for row in range(nrows):
         # get label, basis, stim file
         item = table.item(row, 1)
         llist.append(str(item.text()))

         item = table.item(row, 2)
         blist.append(str(item.text()))

         item = table.item(row, 3)
         tlist.append(str(item.text()))

         item = table.item(row, 4)
         dset = str(item.text())
         if dir and dir != '.': pre = '%s/' % dir
         else:                  pre = ''
         dlist.append('%s%s' % (pre, dset))
      self.svars.stim_label = llist
      self.svars.stim_basis = blist
      self.svars.stim_type  = tlist
      self.svars.stim = dlist 

      return 0

   def update_gltsym_from_table(self):

      # --------------------------------------------------
      # gltsym table (get labels and GLTs)
      table = self.gvars.Table_gltsym           # convenience
      nrows = table.rowCount()
      llist = []
      dlist = []
      err = ''
      for row in range(nrows):
         # get label, gltsym
         item = table.item(row, 0)
         if item != None: lab = str(item.text())
         else:            lab = ''
         item = table.item(row, 1)
         if item != None: glt = str(item.text())
         else:            glt = ''

         # nuke any leading 'SYM: ' from the glt
         if glt[0:5] == 'SYM: ': glt = glt[5:]

         if lab == '' or glt == '':
            if lab != '':
               err += '** GLT #%d, skipping label without GLT...\n' % (row+1)
            if glt != '':
               err += '** GLT #%d, skipping GLT without label...\n' % (row+1)
            continue

         llist.append(lab)
         dlist.append(glt)

      if err != '':
         QLIB.guiError('GLT Errors', err, self)
         return 1

      rv, rstr = self.run_GLTsymtest(dlist)
      if rv:
         QLIB.guiError('GLTsymtest errors', rstr, self)
         return 1

      self.svars.gltsym_label = llist
      self.svars.gltsym = dlist

      return 0

   def run_GLTsymtest(self, glist):
      """run GLTsymtest, returning the number of errors and a display string"""

      llist = self.svars.stim_label

      if len(llist) == 0 and len(glist) == 0: return 0, ''

      lstr = "'%s'" % ' '.join(llist)
      cmd = "GLTsymtest -badonly %s" % lstr

      elist = []
      for gind, glt in enumerate(glist):
         cstr = "%s '%s'" % (cmd, glt)

         st, so, se = UTIL.limited_shell_exec(cstr)
         if self.verb > 1:
            print("++ GLTsymtest command (stat %d):\n   %s" % (st, cstr))
         if len(so) > 0 and self.verb > 2:
            print("\n%s" % ('   ' + '\n   '.join(so)))

         # if no errors, continue on
         if st == 0: continue

         emesg = "** GLT ERROR for -gltsym %d: '%s'\n" % (gind+1, glt)
         for soline in so:
            if soline.startswith('** ERROR:'):
               emesg += ('%s\n' % soline[3:])

         elist.append('%s' % emesg)

      if len(elist) > 0:
         emesg = "valid GLT labels are: %s\n\n%s" % (lstr, '\n'.join(elist))
         if self.verb > 3: print('failure emesg: %s' % emesg)
      else:
         emesg = ''

      return len(elist), emesg

   def init_analysis_defaults(self):
      """initialize the svar default based on anal_type and anal_domain
      """
      atype = self.svars.val('anal_type')
      adomain = self.svars.val('anal_domain')

      if atype == 'rest': cobj = USUBJ.g_rdef_strs
      else:               cobj = USUBJ.g_tdef_strs
      changestr = cobj.changed_attrs_str(self.svars, skiplist='name',
                                         showskip=0, showdel=0)
      self.apply_svars(cobj)

      title = "Applied Variables: to type '%s', domain '%s'" % (atype, adomain)
      self.update_applied_vars_window(title=title, text=changestr)

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

      self.gvars.ap_status = 2

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
         print('===== ++++ ===== re-running ap_command....')
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

   def cb_clear_options(self):
      """set cvars, svars from defaults and redisplay GUI
         EXCEPT: keep dataset fields from subject"""

      svars = VO.VarsObject()
      for atr in ['sid', 'gid', 'anat', 'epi', 'stim']:
         svars.set_var(atr, self.svars.val(atr))
      
      self.reset_vars(svars=svars, set_sdir=self.set_sdir)

   def cb_clear_fields(self):
      """set cvars, svars from defaults and redisplay GUI"""
      self.reset_vars(set_sdir=self.set_sdir)

   def cb_view(self):
      """create permanent windows with given text"""
      obj = self.sender()

      if obj == self.gvars.act_view_vars:
         self.update_applied_vars_window(title="Applied Variables")

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

      elif obj == self.gvars.act_view_gvars:
         sstr = self.gvars.make_show_str('GUI vars', name=0, all=1)
         QLIB.static_TextWindow(title='GUI vars', text=sstr, parent=self)

   def make_uber_command(self):
      """generate a script that would invoke the uber_subject.py interface
         with subject and control vars set (and so fields filled)

         Put any key elements in quotes:
            basis functions
            gltsym
      """

      # first apply subject variables
      self.update_svars_from_gui()

      cmd = 'uber_subject.py'

      # control vars first
      prefix = ' \\\n    -cvar '     # append before next command
      for atr in self.cvars.attributes():
         if atr == 'name': continue     # skip
         if self.cvars.vals_are_equal(atr, USUBJ.g_cdef_strs): continue
         # show this one
         val = self.cvars.val(atr)
         if self.cvars.has_simple_type(atr):
            cmd += (prefix + '%s %s' % (atr, val))
         elif type(val) == list:
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))
         else:
            print('** make_uber_command: bad attr %s' % atr)

      # then subject vars
      prefix = ' \\\n    -svar '     # append before next command
      for atr in self.svars.attributes():
         if atr == 'name': continue     # skip
         if self.svars.vals_are_equal(atr, USUBJ.g_sdef_strs): continue
         # show this one
         val = self.svars.val(atr)
         
         # special cases first: stim_basis, gltsym
         if atr == 'gltsym':            # special case
            val = ["'%s'" % v for v in val]
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))

         elif atr == 'stim_basis':      # special case
            val = ["'%s'" % v for v in val]
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))

         elif self.svars.has_simple_type(atr):
            cmd += (prefix + '%s %s' % (atr, val))
         elif type(val) == list:
            cmd += (prefix + '%s %s' % (atr, ' '.join(val)))
         else:
            print('** make_uber_command: bad attr %s' % atr)

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
         print('-- cb_setSyle: text = %s' % text)
         if text == 'Default': text = g_styles[g_style_index_def]
      except:
         print('** cb_setSyle: cannot get text')
         return

      if text in g_styles:
         try:
            self.gvars.style = text
            QtGui.QApplication.setStyle(QtGui.QStyleFactory.create(text))
         except: print("** failed to set style '%s'" % text)
      else: print("** style '%s' not in style list" % text)

   def set_cvar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if not self.cvars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print("++ set_cvar: update [%s] to '%s'"%(name,newval))

   def set_svar(self, name, newval):
      """if the value has changed (or is not a simple type), update it
         - use deepcopy (nuke it from orbit, it's the only way to be sure)"""

      if not self.svars.set_var(name, newval): return

      # so the value has changed...

      if self.verb > 3 : print("++ set_svar: update [%s] to '%s'"%(name,newval))

      self.gvars.ap_status = 0  # no longer ready for execution
      self.gvars.act_exec_ap.setEnabled(False)
      self.gvars.act_exec_proc.setEnabled(False)

   def set_PushB_from_svar(self, vname):
      """set PushB_vname text from corresponding svars val"""
      pb = self.gvars.val('PushB_%s' % vname)
      btext = self.svars.val(vname)

      if not pb:
         print('** invalid PushB %s for text %s' % (vname, text))
         return
      if not btext:
         print('** invalid svar %s for PushB' % vname)
         return
      
      pb.setText(btext)

   def apply_svars(self, svars=None):
      """apply to the svars object and to the gui

         first init to defaults
         if svars is passed, make further updates"""

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
      elif svar == 'anal_type':   self.set_PushB_from_svar('anal_type')
      elif svar == 'anal_domain': self.set_PushB_from_svar('anal_domain')
      elif svar == 'blocks':
                                  bstr = ' '.join(self.svars.val('blocks'))
                                  self.gvars.Line_blocks.setText(bstr)
      elif svar == 'sid':         self.gvars.Line_sid.setText(self.svars.sid)
      elif svar == 'gid':         self.gvars.Line_gid.setText(self.svars.gid)
      elif svar == 'anat':        self.gvars.Line_anat.setText(self.svars.anat)
      elif svar == 'get_tlrc':
                                  obj = self.gvars.gbox_anat.checkBox
                                  obj.setChecked(self.svars.get_tlrc=='yes')
      elif svar == 'epi':         self.epi_list_to_table()
      elif svar == 'epi_wildcard':         
                                  var = self.svars.epi_wildcard
                                  obj = self.gvars.gbox_epi.checkBox_wildcard
                                  obj.setChecked(var=='yes')
      elif svar == 'stim':        self.stim_list_to_table()
      elif svar == 'stim_wildcard':        
                                  var = self.svars.stim_wildcard
                                  obj = self.gvars.gbox_stim.checkBox_wildcard
                                  obj.setChecked(var=='yes')
      elif svar == 'label':       self.stim_list_to_table()
      elif svar == 'basis':       self.stim_list_to_table()
      elif svar == 'stim_type':   self.stim_list_to_table()

      elif svar == 'tcat_nfirst': 
                                   obj = self.gvars.Line_tcat_nfirst
                                   obj.setText(self.svars.tcat_nfirst)
      elif svar == 'volreg_base':  
                                   obj = self.gvars.Line_volreg_base
                                   obj.setText(self.svars.volreg_base)
      elif svar == 'blur_size':
                                   obj = self.gvars.Line_blur_size
                                   obj.setText(self.svars.blur_size)
      elif svar == 'motion_limit':
                                   obj = self.gvars.Line_motion_limit
                                   obj.setText(self.svars.motion_limit)
      elif svar == 'gltsym':       self.gltsym_list_to_table()
      elif svar == 'gltsym_label': self.gltsym_list_to_table()

      elif svar == 'outlier_limit':
                                   obj = self.gvars.Line_outlier_limit
                                   obj.setText(self.svars.outlier_limit)
      elif svar == 'regress_jobs':
                                   obj = self.gvars.Line_regress_jobs
                                   obj.setText(self.svars.regress_jobs)
      elif svar == 'regress_GOFORIT':
                                   obj = self.gvars.Line_regress_GOFORIT
                                   obj.setText(self.svars.regress_GOFORIT)
      elif svar == 'regress_bandpass':
                          obj = self.gvars.Line_regress_bandpass
                          obj.setText(' '.join(self.svars.regress_bandpass))
      elif svar == 'regress_mot_deriv':        
                          var = self.svars.regress_mot_deriv
                          obj = self.gvars.gbox_regress
                          obj.checkBox_mot_deriv.setChecked(var=='yes')
      elif svar == 'reml_exec':        
                          var = self.svars.reml_exec
                          obj = self.gvars.gbox_regress
                          obj.checkBox_reml_exec.setChecked(var=='yes')
      elif svar == 'run_clustsim':        
                          var = self.svars.run_clustsim
                          obj = self.gvars.gbox_regress
                          obj.checkBox_run_clustsim.setChecked(var=='yes')
      elif svar == 'compute_fitts':        
                          var = self.svars.compute_fitts
                          obj = self.gvars.gbox_regress
                          obj.checkBox_compute_fitts.setChecked(var=='yes')
      elif svar == 'align_cost':        
                          obj = self.gvars.Line_align_cost
                          obj.setText(self.svars.align_cost)
      elif svar == 'tlrc_base':        
                          obj = self.gvars.Line_tlrc_base
                          obj.setText(self.svars.tlrc_base)
      elif svar == 'align_giant_move':        
                          var = self.svars.align_giant_move
                          obj = self.gvars.gbox_align
                          obj.checkBox_align_giant_move.setChecked(var=='yes')
      elif svar == 'anat_has_skull':        
                          var = self.svars.anat_has_skull
                          obj = self.gvars.gbox_anat
                          obj.checkBox_anat_has_skull.setChecked(var=='yes')
      elif svar == 'tlrc_ok_maxite':        
                          var = self.svars.tlrc_ok_maxite
                          obj = self.gvars.gbox_tlrc
                          obj.checkBox_tlrc_ok_maxite.setChecked(var=='yes')

      else:
         if self.verb > 1: print('** apply_svar_in_gui: unhandled %s' % svar)
         rv = 0

      if rv and self.verb > 2: print('++ apply_svar_in_gui: process %s' % svar)

      return rv

   def apply_cvars(self, cvars=None):
      """apply to the cvars object and to the gui

         first init to defaults
         if cvars is passed, make further updates"""

      # merge with current vars and apply everything to GUI
      self.cvars.merge(cvars)
      self.set_cvar('verb', str(self.verb))     # to pass verb along
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
         if self.verb > 1: print('** apply_cvar_in_gui: unhandled %s' % cvar)
         rv = 0

      if rv and self.verb > 2: print('++ apply_cvar_in_gui: process %s' % cvar)

      return rv

# --- post SingleSubjectWindow class

# ===========================================================================
# help strings

g_help_init = """
Analysis initialization:

   goals:

      1. specify analysis type
      2. specify data domain
      3. specify main processing blocks

   description:

      Specifying the analysis type and data domain allows for more appropriate
      initialization of processing defaults.

      Setting the 'type' has no effect until the 'APPLY' button is pressed, at
      which point the relevant defaults are changed and applied.

      This implies that 'analysis initialization' should be done first (as one
      might expect), otherwise it could undo user changes.  For example, if
      the user sets the motion censor limit to 0.7 and then initializes the
      'type' to "rest", the 0.7 will be changed to 0.2, the default for resting
      state analysis.
"""

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
      those indices so they have the same number of digits.  That makes the
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
      4. possibly alter the stim types
  
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

      A stimulus type will applied to each file.  They can be initialzed at
      once via 'init file types', or they can be modified individually in the
      table.  The stimulus file types are shown in the 'choose' menu and
      correspond to those in afni_proc.py.

      Most types are for timing files, while 'file' is for a simple regressor.

   ** Note: no stimulus should be given during the pre-steady state TRs.
            The stimulus times should match times after the pre-SS TRs that
            are removed from the EPI data.
            Similarly, 'file' types should be of length #TRs minus all #pre-SS.

   typical use in processing:

      1. use the timing files and basis functions to create regressors of
         interest to be used in the regress processing block (by 3dDeconvolve).
      2. if the basis functions are fixed shapes (e.g. GAM/BLOCK), generate
         'ideal' curves per stimulus class (from the X-matrix columns)
      3. the 'type' entries control the -stim_times* or -stim_file options in
         the 3dDeconvolve command:

            times       : -stim_times
            AM1         : -stim_times_AM1
            AM2         : -stim_times_AM2
            IM          : -stim_times_IM
            file        : -stim_file

         See the '-regress_stim_types' option from afni_proc.py or the given
         options from 3dDeconvolve for more details.

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

g_help_gltsym = """

   goals:

      1. specify symbolic GLTs (general linear tests)
      2. specify corresponding labels (used for sub-brick selection in results)

   description:

      To add a symbolic GLT, either "insert glt row" to add a blank row to the
      table, or "init with glt examples" to add some samples from the above
      stimulus list.  Since these are symbolic GLTs, they should be expressed
      as computations on the stimulus labels.

      If the table is initialized with examples, GLT labels.

      buttons:

         insert glt row         : append a blank row
         init with glt examples : initialize table with GLTs from stim labels
         resize glt table       : delete blank rows (neither label nor GLT)
         clear glt table        : delete all table rows
         help: gltsym           : show this help

   typical use in processing:

      GLTs in this table will be passed along directly to the 3dDeconvolve
      command in the single subject processing script.  Each GLT label will
      become the sub-brick label of the resulting stats dataset, output from
      the linear regression.

      For example, suppose one has stimulus labels: houses, faces and donuts.
      One might want to compare houses to donuts, or create a contrast which
      compares donuts with the mean of houses and faces.  Such labels and
      symbolic GLTs could be:

         label          symbolic GLT
         -----          ------------
         H-D            houses -donuts
         D-HF           donuts -0.5*houses -0.5*faces

      These would eventually be given to 3dDeconvolve as:

        -gltsym 'SYM: houses -donuts' -glt_label 1 H-D
        -gltsym 'SYM: donuts -0.5*houses -0.5*faces' -glt_label 2 D-HF

      Note that the leading SYM: in the symbolic GLT is applied by the program,
      and need not be specified by the user.
"""

# this example should be displayed if we do not yet have stim labels set
g_help_gltsym_eg = """
"""

g_help_eg = """
   goals:

   description:

"""

# end: help strings
# ===========================================================================

def main():

   print('** this is not a main program')
   return 1

if __name__ == '__main__':
   sys.exit(main())
