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
import lib_uber_align as UALIGN
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
      self.cvars  = UALIGN.g_cdef_strs.copy('GUI control vars')
      self.uvars  = UALIGN.g_udef_strs.copy('GUI user vars')

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

      self.cvars = UALIGN.g_cdef_strs.copy()
      self.uvars = UALIGN.g_udef_strs.copy()

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

      label = QLIB.make_label('anat')
      pb = QLIB.make_button("browse anat", tip="browse file system for anat",
                            cb=self.CB_gbox_PushB, hstr=0)
      line = QLIB.make_line(self.uvars.anat, cb=self.CB_line_text)

      layout.addWidget(label, 0, 0)
      layout.addWidget(pb, 0, 1)
      layout.addWidget(line, 0, 2, 1, 4)

      # store what we need in gvars
      self.gvars.PB_anat = pb           # for sender in CB_gbox_PushB
      self.gvars.Line_anat = line

      # --------------------------------------------------
      # epi line
      label = QLIB.make_label('EPI')
      pb = QLIB.make_button("browse EPI", tip="browse file system for EPI",
                            cb=self.CB_gbox_PushB, hstr=0)
      line = QLIB.make_line(self.uvars.epi, cb=self.CB_line_text)
      layout.addWidget(label, 1, 0)
      layout.addWidget(pb, 1, 1)
      layout.addWidget(line, 1, 2, 1, 4)

      # store what we need in gvars
      self.gvars.PB_epi = pb           # for sender in CB_gbox_PushB
      self.gvars.Line_epi = line

      # --------------------------------------------------
      # epi base line
      label = QLIB.make_label('EPI base',
              tip='0-based EPI volume index might include pre-steady state TRs')
      line = QLIB.make_line(self.uvars.epi_base, cb=self.CB_line_text, hstr=0)
      layout.addWidget(label, 2, 0)
      layout.addWidget(line, 2, 2, 1, 1)

      self.gvars.Line_epi_base = line

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

      elif obj == self.gvars.Line_epi_base:
         self.update_textLine_check(obj, obj.text(), 'epi_base', 'EPI index',
                                    QLIB.valid_as_int)

      elif obj == self.gvars.Line_cost_list:
         self.cost_text_to_uvar()

      elif obj == self.gvars.Line_center_base: # no check
         self.uvars.set_var('center_base', str(obj.text()))

      elif obj == self.gvars.Line_epi_strip_meth: # no check, rcr - in list?
         self.uvars.set_var('epi_strip_meth', str(obj.text()))

      elif obj == self.gvars.Line_aea_opts: # no check
         opstr = str(obj.text())
         self.uvars.set_var('aea_opts', opstr.split())

      else: print '** CB_line_text: unknown sender'

   def make_l3_group_boxes(self):
      """create anat, EPI, stim, etc. group boxes, and add to m2_vlayout"""

      self.gvars.gbox_costs = self.group_box_costs()
      self.gvars.gbox_align = self.group_box_align()
      self.gvars.gbox_other = self.group_box_other()

      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_costs)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_align)
      self.gvars.m2_vlayout.addWidget(self.gvars.gbox_other)

   def group_box_other(self):
      """create a group box for other options

                checkBox(giant move)
                checkBox(add edge)
                checkBox(anat has skull)
                label(EPI strip method) button_list(pick) lineE(method)
                label(other AEA.py opts)  lineEdit(opts)
      """

      gbox = self.get_styled_group_box("other alignment options")

      # ------------------------------------------------------------
      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      # main layout to hold frame contents
      layout = QtGui.QGridLayout(frame)

      # ------------------------------------------------------------
      # fill frame 

      # giant_move check box
      gbox.checkBox_giant_move = QLIB.make_checkbox('giant move',
                checked=(self.uvars.val('giant_move') == 'yes'),
                tip='might alignment be far apart to begin with',
                cb=self.CB_checkbox)

      # add_edge check box
      gbox.checkBox_add_edge = QLIB.make_checkbox('add edge',
                checked=(self.uvars.val('add_edge') == 'yes'),
                tip='make AddEdge output (can use only one cost function)',
                cb=self.CB_checkbox)

      # anat_has_skull check box
      gbox.checkBox_anat_has_skull = QLIB.make_checkbox('anat has skull',
                checked=(self.uvars.val('anat_has_skull') == 'yes'),
                tip='uncheck if anat has already been skull-stripped',
                cb=self.CB_checkbox)

      # EPI strip method, probably pick from list
      label = QLIB.make_label('EPI strip method:',
                              tip='AEA.py method for EPI skull-strip')
      blist = ['E strip: %s' % base for base in UALIGN.g_epi_strip_list]
      pb = QLIB.create_menu_button(frame, "pick method", blist,
                                     call_back=self.CB_gbox_PushB)
      gbox.PB_tbase = pb
      line = QLIB.make_line(self.uvars.epi_strip_meth,
                            cb=self.CB_line_text, hstr=0)
      self.gvars.Line_epi_strip_meth = line

      self.gvars.PB_otherHelp = QLIB.make_button("help: other",
             tip="display help for this section", cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_otherHelp.setIcon(self.style().standardIcon(
                                     QtGui.QStyle.SP_MessageBoxQuestion))

      # add everything to the layout
      layout.addWidget(gbox.checkBox_giant_move, 0, 0)
      layout.addWidget(gbox.checkBox_add_edge, 1, 0)
      layout.addWidget(gbox.checkBox_anat_has_skull, 2, 0)

      layout.addWidget(label, 3, 0)
      layout.addWidget(gbox.PB_tbase, 3, 1)
      layout.addWidget(self.gvars.Line_epi_strip_meth, 3, 2)

      layout.addWidget(self.gvars.PB_otherHelp, 0, 2)

      label = QLIB.make_label('other AEA opts:',
                              tip='other options to pass to align_epi_anat.py')
      line = QLIB.make_line('', cb=self.CB_line_text)
      self.gvars.Line_aea_opts = line
      layout.addWidget(label, 4, 0)
      layout.addWidget(line, 4, 1, 1, 3)

      # ------------------------------------------------------------
      # finish up
      frame.setLayout(layout)
      glayout.addWidget(frame)
      gbox.setLayout(glayout)

      return gbox

   def group_box_align(self):
      """create a group box for aligning centers, with:
                checkBox(align)   button_list(pick)  button(browse dset)
                label(center dset)  lineEdit(actual filename text)
      """

      gbox = self.get_styled_group_box("align dataset centers")

      # ------------------------------------------------------------
      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      # main layout to hold frame contents
      layout = QtGui.QGridLayout(frame)

      # ------------------------------------------------------------
      # fill frame 

      lindex = 0

      # --- controlling checkbox
      gbox.checkBox_align_centers = QLIB.make_checkbox('align centers: Y/N',
                checked=(self.uvars.val('align_centers') == 'yes'),
                tip='choose whether to align dataset centers to a base',
                cb=self.CB_checkbox)
      layout.addWidget(gbox.checkBox_align_centers, lindex, 0)

      lindex += 1

      # --- make base textLine first, since other widgets may affect it
      label = QLIB.make_label('center base:')
      line = QLIB.make_line(self.uvars.center_base, cb=self.CB_line_text)
      self.gvars.Line_center_base = line

      # --- check centers and help buttons
      pb = QLIB.make_button("check center dist",
                            tip="compute distance between volume centers",
                            cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_bbase = pb
      layout.addWidget(pb, lindex, 1)

      self.gvars.PB_Centers = QLIB.make_button("help: align centers",
             tip="display help for this section", cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_Centers.setIcon(self.style().standardIcon(
                                     QtGui.QStyle.SP_MessageBoxQuestion))
      layout.addWidget(self.gvars.PB_Centers, lindex, 3)

      lindex += 1

      # --- chooser for template bases
      blist = ['A. base: %s' % base for base in UALIGN.g_center_base_list]
      pb = QLIB.create_menu_button(frame, "template center", blist,
                                     call_back=self.CB_gbox_PushB)
      gbox.PB_tbase = pb
      layout.addWidget(pb, lindex, 1)

      pb = QLIB.make_button("browse center",tip="browse file system for center",
                            cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_bbase = pb
      layout.addWidget(pb, lindex, 3)

      lindex += 1

      layout.addWidget(label, lindex, 1)
      layout.addWidget(line, lindex, 1, 1, 4)

      # ------------------------------------------------------------
      # finish up
      frame.setLayout(layout)
      glayout.addWidget(frame)
      gbox.setLayout(glayout)

      return gbox

   def group_box_costs(self):
      """create a group box with *some* layout:
                group box of QCheckBox's to choose from a default cost list
                clear and apply buttons
                label and line text for list

         for controlling uvar: costs
      """

      gbox = self.get_styled_group_box("cost functions")

      # ------------------------------------------------------------
      # put frame inside gbox, which we can hide via toggled button
      glayout = QtGui.QVBoxLayout(gbox)
      frame = QtGui.QFrame(gbox)
      frame.setFrameShape(QtGui.QFrame.NoFrame)
      gbox.frame = frame
      self.init_gbox_viewable(gbox, True)       # default to viewable
      # gbox.toggled.connect(self.gbox_toggle_frame)
      self.connect(gbox, QtCore.SIGNAL('clicked()'), self.gbox_clicked)

      # main layout to hold frame contents
      layout = QtGui.QGridLayout(frame)

      # ------------------------------------------------------------
      # fill frame 

      # make box of cost options
      self.gvars.gbox_cost_items = self.cost_func_group_box(frame)
      ncost = len(self.gvars.gbox_cost_items.check_boxes)
      layout.addWidget(self.gvars.gbox_cost_items, 0, 0, ncost, 1)

      # make cost list label and Line
      label = QLIB.make_label('costs to apply:',
                              tip='list of cost functions to try in script')
      line = QLIB.make_line(' '.join(self.uvars.cost_list),cb=self.CB_line_text)
      self.gvars.Line_cost_list = line
      layout.addWidget(label, ncost+1, 0)
      layout.addWidget(line, ncost+1, 1, 1, 3)

      self.init_cost_options()  # init check boxes and cost line

      # add buttons to clear, reset and apply

      self.gvars.PB_costClear = QLIB.make_button("clear costs",
             tip="clear checked cost list", cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_costReset = QLIB.make_button("reset costs",
             tip="reset checked cost list to defaults",
             cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_costApply = QLIB.make_button("apply costs",
             tip="apply checked costs to list", cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_costHelp = QLIB.make_button("help: costs",
             tip="display help for this section", cb=self.CB_gbox_PushB, hstr=0)
      self.gvars.PB_costHelp.setIcon(self.style().standardIcon(
                                     QtGui.QStyle.SP_MessageBoxQuestion))
      layout.addWidget(self.gvars.PB_costClear, 0, 1)
      layout.addWidget(self.gvars.PB_costReset, 1, 1)
      layout.addWidget(self.gvars.PB_costApply, 3, 1)
      layout.addWidget(self.gvars.PB_costHelp,  0, 3)

      # ------------------------------------------------------------
      # finish up
      frame.setLayout(layout)
      glayout.addWidget(frame)
      gbox.setLayout(glayout)

      return gbox

   def cost_func_group_box(self, parent):
      """make a single group box that contains a vertical list of checkboxes,
         one for each main cost"""
      gbox = QtGui.QGroupBox()
      layout = QtGui.QVBoxLayout(gbox)

      gbox.check_boxes = []
      for cost in UALIGN.g_def_main_costs:
         cbox = QtGui.QCheckBox(cost)
         layout.addWidget(cbox)
         gbox.check_boxes.append(cbox)

      policy = gbox.sizePolicy()
      policy.setHorizontalPolicy(0)
      policy.setVerticalPolicy(0)
      gbox.setSizePolicy(policy)

      gbox.setLayout(layout)

      return gbox

   def init_cost_options(self, cost_defs=None):
      """check the boxes for any passed costs (if they have checkboxes),
         then apply the list to gvars.Line_cost_list
      """
      if cost_defs == None: cost_defs = self.uvars.val('cost_list')
      cboxes = self.gvars.gbox_cost_items.check_boxes
      for cbox in cboxes:
         cost = str(cbox.text())
         cbox.setChecked(cost in cost_defs)
      self.gvars.Line_cost_list.setText(' '.join(cost_defs))

   def apply_checked_costs(self):
      """make a string of cost functions from the checked boxes in
         gbox_cost_items and set gvars.Line_cost_list with the text
         --> then apply the text to user vars
      """
      costs = []
      cboxes = self.gvars.gbox_cost_items.check_boxes
      for cbox in cboxes:
         if cbox.isChecked(): costs.append(str(cbox.text()))
      self.gvars.Line_cost_list.setText(' '.join(costs))
      self.cost_text_to_uvar()

   def cost_text_to_uvar(self):
      """apply the text from Line_cost_list to the cost_list user var"""
      obj = self.gvars.Line_cost_list
      text = str(obj.text())
      clist = text.split()
      self.uvars.set_var('cost_list', clist)

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
      if obj == self.gvars.gbox_align.checkBox_align_centers:
         if obj.isChecked(): self.set_uvar('align_centers', 'yes')
         else:               self.set_uvar('align_centers', 'no')
      elif obj == self.gvars.gbox_other.checkBox_giant_move:
         if obj.isChecked(): self.set_uvar('giant_move', 'yes')
         else:               self.set_uvar('giant_move', 'no')
      elif obj == self.gvars.gbox_other.checkBox_add_edge:
         if obj.isChecked(): self.set_uvar('add_edge', 'yes')
         else:               self.set_uvar('add_edge', 'no')
      elif obj == self.gvars.gbox_other.checkBox_anat_has_skull:
         if obj.isChecked(): self.set_uvar('anat_has_skull', 'yes')
         else:               self.set_uvar('anat_has_skull', 'no')

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

      self.atest = UALIGN.AlignTest(cvars=self.cvars, uvars=self.uvars)
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

      elif text == 'clear costs':
         self.init_cost_options([])
         self.apply_checked_costs()
      elif text == 'reset costs':
         self.init_cost_options(UALIGN.g_user_defs.cost_list)
         self.apply_checked_costs()
      elif text == 'apply costs':
         self.apply_checked_costs()
      elif text == 'help: costs':
         self.update_help_window(g_help_costs, title='cost functions')

      elif text[0:9] == 'A. base: ':
         base = text[9:]
         self.set_uvar('center_base', base)
         self.gvars.Line_center_base.setText(base)
      elif text == 'help: align centers':
         self.update_help_window(g_help_align_centers, title='center alignment')
      elif text == 'check center dist':
         status, dstr = self.make_center_dist_str()
         if not status: self.update_help_window(dstr, title='center distance')
         else:
            if dstr == '': return
            else: QLIB.guiWarning('Error', dstr, self)

      elif text[0:9] == 'E strip: ':
         base = text[9:]
         self.set_uvar('epi_strip_meth', base)
         self.gvars.Line_epi_strip_meth.setText(base)
      elif text == 'help: other':
         self.update_help_window(g_help_other, title='other options')

      elif text == 'browse center':
         fname = QtGui.QFileDialog.getOpenFileName(self,
                   "load center dataset", self.pick_base_dir('anat'),
                   "datasets (*.HEAD *.nii);;all files (*)")
         self.update_textLine_check(self.gvars.Line_center_base,
                fname, 'center_base', 'center dataset', QLIB.valid_as_filepath)

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

      act1 = QLIB.createAction(self, "results from align test",
        slot=self.cb_view,
        tip="show afni command for viewing alignment results")

      act2 = QLIB.createAction(self, "resulting align script",
        slot=self.cb_view,
        tip="display script created via GUI")

      act3 = QLIB.createAction(self, "output from align script",
        slot=self.cb_view,
        tip="display text output from execution of align script")

      act4 = QLIB.createAction(self,"view: uber_align_test.py command",
          slot=self.cb_view,
          tip="show command that would populate this interface")

      QLIB.addActions(self, self.gvars.MBar_view, [act1, act2, act3, None,act4])

      self.gvars.act_view_results = act1
      self.gvars.act_view_script = act2
      self.gvars.act_view_output = act3
      self.gvars.act_view_cmd = act4

      # ----------------------------------------------------------------------
      # Hidden menu
      self.gvars.MBar_hidden = self.menuBar().addMenu("Hidde&n")

      act1 = QLIB.createAction(self,"view: user vars",
        slot=self.cb_view,
        tip="display user option variables")

      act2 = QLIB.createAction(self,"view: ctrl vars",
        slot=self.cb_view,
        tip="display control variables")

      act3 = QLIB.createAction(self,"view: result vars",
        slot=self.cb_view,
        tip="display result variables")

      act4 = QLIB.createAction(self,"view: GUI vars",
        slot=self.cb_view,
        tip="display GUI variables")

      self.gvars.act_view_uvars = act1
      self.gvars.act_view_cvars = act2
      self.gvars.act_view_rvars = act3
      self.gvars.act_view_gvars = act4

      # first add view of user, ctrl and result vars
      QLIB.addActions(self, self.gvars.MBar_hidden, [act1, act2, act3])

      act5 = QLIB.createAction(self, "shell command window",
          slot=self.cb_show_command_window,
          tip="open command window for shell commands")

      act6 = QLIB.createAction(self, "python command window",
          slot=self.cb_show_py_command_window,
          tip="open command window for local python commands")

      QLIB.addActions(self, self.gvars.MBar_hidden, [None, act4, act5, act6])

      # ----------------------------------------------------------------------
      # Help menu
      self.gvars.MBar_help = self.menuBar().addMenu("&Help")
      actHelpUAS = QLIB.createAction(self,"uber_align_test.py -help'",
          slot=self.cb_help_main, shortcut=QtGui.QKeySequence.HelpContents,
          tip="help for uber_align_test.py")
      QLIB.addActions(self, self.gvars.MBar_help, [actHelpUAS])

      # add browse actions to browse sub-menu
      self.gvars.Menu_browse = self.gvars.MBar_help.addMenu("&Browse")
      act1 = QLIB.createAction(self,"web: all AFNI programs",
          slot=self.cb_help_browse, tip="browse AFNI program help")
      act2 = QLIB.createAction(self,"web: align_epi_anat.py help",
          slot=self.cb_help_browse, tip="browse align_epi_anat.py help")
      act3 = QLIB.createAction(self,"web: afni_proc.py help",
          slot=self.cb_help_browse, tip="browse afni_proc.py help")
      act4 = QLIB.createAction(self,"web: AFNI Message Board",
          slot=self.cb_help_browse, tip="browse Message Board")

      QLIB.addActions(self, self.gvars.Menu_browse, [act1, act2, act3, act4])

      self.gvars.act_browse_all_progs = act1
      self.gvars.act_browse_AEA_help  = act2
      self.gvars.act_browse_AP_help   = act3
      self.gvars.act_browse_MB        = act4

      actHelpAbout = QLIB.createAction(self,"about uber_align_test.py",
          slot=self.cb_help_about, tip="about uber_align_test.py")
      QLIB.addActions(self, self.gvars.MBar_help, [actHelpAbout])

   def add_tool_bar(self):
      self.gvars.toolbar = self.addToolBar('uber_align_test.py')

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
      self.update_help_window(UALIGN.helpstr_gui,
                              title='uber_align_test.py: GUI help')

   def cb_help_about(self):
      """display version info in Text_Help window"""
      text = "uber_align_test.py, version %s" % UALIGN.g_version
      QLIB.guiMessage('about uber_align_test.py', text, self)

   def cb_help_browse(self):
      obj = self.sender()
      if   obj == self.gvars.act_browse_all_progs:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/index.html')
      elif obj == self.gvars.act_browse_AEA_help:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/align_epi_anat.py.html')
      elif obj == self.gvars.act_browse_AP_help:
         self.open_web_site('https://afni.nimh.nih.gov/pub/dist/doc'     \
                            '/program_help/afni_proc.py.html')
      elif obj == self.gvars.act_browse_MB:
         self.open_web_site('https://afni.nimh.nih.gov/afni/community/board')
      else: print '** cb_help_browse: invalid sender'

   def update_uvars_from_gui(self, warn=0, set_pdir=1, disable_exec=1):
      """set what we can, if warn, report error
         return 0 on success, 1 on error"""

      if self.uvars.is_empty('anat') or self.uvars.is_empty('epi'):
         if warn: QLIB.guiError('Error', 
                       "** anat and EPI datasets must be specified", self)
         return 1

      # maybe process tables
      # if self.update_uvars_from_tables(): return 1

      if self.set_pdir and set_pdir:
         # proc dir might read: tool_results/tool.0001.align_test
         pdir = SUBJ.get_def_tool_path('align_test', top_dir='tool_results',
                   prefix='tool', keep_if_missing=self.uvars.val('results_dir'))
         if self.set_cvar('proc_dir', pdir):
            print '-- setting proc_dir to %s' % pdir

      if disable_exec: self.gvars.act_exec_script.setEnabled(False)

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

      if obj == self.gvars.act_view_results:
         self.show_howto_view_results()

      elif obj == self.gvars.act_view_script:
         self.show_static_file('file_proc', 'align script:')

      elif obj == self.gvars.act_view_output:
         self.show_static_file('output_proc', 'script output:')

      elif obj == self.gvars.act_view_cmd:
         sstr = self.make_uber_command()
         QLIB.static_TextWindow(title='corresp. uber_align_test.py command',
                                text=sstr, parent=self)

      elif obj == self.gvars.act_view_uvars:
         sstr = self.uvars.make_show_str('current align test', name=0)
         QLIB.static_TextWindow(title='user vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_cvars:
         sstr = self.cvars.make_show_str('control vars', name=0, all=0)
         QLIB.static_TextWindow(title='control vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_rvars:
         if self.atest == None:
            QLIB.guiError('Error','** must first generate processing script',
                          self)
         else:
            sstr = self.atest.rvars.make_show_str('result vars', name=0, all=0)
            QLIB.static_TextWindow(title='control vars', text=sstr, parent=self)

      elif obj == self.gvars.act_view_gvars:
         sstr = self.gvars.make_show_str('GUI vars', name=0, all=1)
         QLIB.static_TextWindow(title='GUI vars', text=sstr, parent=self)

      else: print '** unhandled object in cb_view'

   def show_howto_view_results(self):
      """display (in help window) how to view the processing results"""
      if self.atest == None or self.status < 1:
         self.update_warn_window(                                 \
                "** the alignment script is not ready, so the\n"  \
                "   processing directory is currently unknown")
         return

      rdir = self.atest.uvars.val('results_dir')
      if rdir != None: rdir = self.atest.cvars.file_under_dir('proc_dir', rdir)
      if UTIL.is_trivial_dir(rdir):
         self.update_warn_window('** results_dir not set')
         return

      mesg = "==>  how to view the results (when they are ready)  <==\n\n"  \
             "From within the %s directory, the current results\n"          \
             "can be viewed by telling afni where the result dir is\n\n"    \
             "     afni %s\n" % (UALIGN.DEF_UBER_DIR, rdir)

      self.update_help_window(mesg, 'viewing the current alignment results')

   def make_center_dist_str(self):
      """return status and a string describing the distance between
         volume centers

         status = 0 on success, 1 on error
      """
      if self.update_uvars_from_gui(set_pdir=0, disable_exec=0): return 1, ''

      cmd = '@Center_Distance -dset %s %s' % (self.uvars.anat, self.uvars.epi)
      status, output = UTIL.exec_tcsh_command(cmd)

      if status:
         text = '** ERROR: failure for command:\n\n%s\n\n' % cmd
         text += '-'*60 + ('\noutput :\n%s' % output)
         return status, text

      if self.verb > 2:
         print '-- executed center command:\n      %s' % cmd
         print '   status = %d' % status
         print '   output = %s' % output

      # distance should be first line of output, but strip the '\n'
      output = output.strip()
      text =  "current distance between volume centers = %s mm\n\n" % output

      text += "It is common for centers to be far apart, since antomical\n"  \
              "volumes tend to cover more space.  So a better test of\n"     \
              "whether to align centers is to view the datasets together\n"  \
              "in afni.  If both datasets are not entirely visible in the\n" \
              "image windows, then 'align centers' might be a good idea.\n\n"\
              "To view these datasets together, try the afni command:\n\n"   \
              "    afni %s %s\n\n" % (self.uvars.anat, self.uvars.epi)

      text += "Consider viewing the anat as Underlay and the EPI as Overlay."

      return status, text

   def make_uber_command(self):
      """generate a script that would invoke the uber_subject.py interface
         with control and user vars set (and so fields filled)

         Put any key elements in quotes:
            basis functions
            gltsym
      """

      # first apply subject variables
      if self.update_uvars_from_gui(set_pdir=0, disable_exec=0): return ''

      cmd = 'uber_align_test.py'

      # apply each uvar with -uvar option
      prefix = ' \\\n    -uvar '     # append before next command
      for atr in self.uvars.attributes():
         if atr == 'name': continue     # skip
         if self.uvars.vals_are_equal(atr, UALIGN.g_udef_strs): continue
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
         - use deepcopy (nuke it from orbit, it's the only way to be sure)
         return 1 if canged
      """

      if not self.cvars.set_var(name, newval): return 0

      # so the value has changed...

      if self.verb > 3 : print "++ set_cvar: update [%s] to '%s'"%(name,newval)

      # even for cvars, since proc_dir is part of script
      self.gvars.act_exec_script.setEnabled(False)

      return 1

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
      if   uvar == 'anat':      self.gvars.Line_anat.setText(self.uvars.anat)
      elif uvar == 'epi':       self.gvars.Line_epi.setText(self.uvars.epi)
      elif uvar == 'epi_base':
                self.gvars.Line_epi_base.setText(self.uvars.epi_base)

      elif uvar == 'cost_list': self.init_cost_options(self.uvars.cost_list)

      elif uvar == 'align_centers':
                check = (self.uvars.align_centers=='yes')
                self.gvars.gbox_align.checkBox_align_centers.setChecked(check)
      elif uvar == 'center_base':
                self.gvars.Line_center_base.setText(self.uvars.center_base)

      elif uvar == 'giant_move':
                check = (self.uvars.giant_move=='yes')
                self.gvars.gbox_other.checkBox_giant_move.setChecked(check)
      elif uvar == 'add_edge':
                check = (self.uvars.add_edge=='yes')
                self.gvars.gbox_other.checkBox_add_edge.setChecked(check)
      elif uvar == 'anat_has_skull':
                check = (self.uvars.anat_has_skull=='yes')
                self.gvars.gbox_other.checkBox_anat_has_skull.setChecked(check)
      elif uvar == 'epi_strip_meth':
               self.gvars.Line_epi_strip_meth.setText(self.uvars.epi_strip_meth)
      elif uvar == 'aea_opts':
                self.gvars.Line_aea_opts.setText(' '.join(self.uvars.aea_opts))

      else:
         if self.verb > 1: print '** apply_uvar_in_gui: unhandled %s' % uvar
         rv = 0

      if rv and self.verb > 2: print '++ apply_uvar_in_gui: process %s' % uvar

      return rv


# --- post MainWindow class

# ===========================================================================
# help strings


g_help_costs = """
Specifying a list of cost functions to test:

   goals:

      Fill the 'costs to apply:' box with a list of cost functions to test.
      They can come from the list of check boxes or can be typed directly.

   description:

      To choose cost functions from the check boxes, check the desired costs
      and then "apply costs".  The boxes can be cleared with "clear costs" or
      reset to defaults via "reset costs".

      Checked cost functions will not be applied unless they are in the
      "costs to apply" box.

      Each chosen cost function will correspond to one "aligned" anatomical
      dataset output by the script (aligned according to that cost function).

   typical use in processing:

      The script created by this program will use the first given cost function
      with the '-cost' option to align_epi_anat.py.  All other cost functions
      will be given using the '-multi_cost' option.

      Once a "good" cost funtion is found (along with any other options),
      that cost function might be provided to afni_proc.py (or uber_subject.py)
      for use in single subject analysis.

   output file names:

      For the first cost, the resulting anatomical dataset will simply be
      anat_al+orig.  For each other cost function (given to AEA.py via the
      -multi_cost option), there will be a resulting anat_al_COST+orig dataset
      created.
"""

g_help_align_centers = """
Optionally choose whether to align the dataset volume centers to match that of
another dataset.

This is usually not necessary.

   goals:

      If desired, give the anat and EPI datasets the same volume center as that
      of another dataset, as specified by 'center base:'.

   description of interface:

      Aligning centers means chaning the coordinates in the dataset headers so
      that the center of each dataset volume is the same (as the center base).

      To align centers:

        1. check the box labeled 'align centers: Y/N'
        2. choose a 'center base'

      The center base can be one of the 'template center' datasets, or one
      chosen from the file system via 'browse center'.

   background:

      When the input anat and EPI datasets are overlayed together in afni, if
      the brains are not nearly in the same place, or worse, if only one can
      be viewed at a time, then alignment may not be possible.  Even if it is
      possible, part of the aligned brain might be chopped off because the
      datasets do not occupy the same coordinate space.

      Note that such a thing might happen if the proper coordinates are not in
      one or both of the datasets.  It should not happen if original scanner
      coordinates are properly used.  So this is usually a symptom of a faulty
      processing stream.

      In any case, alignment should still be possible, but the datasets should
      start by occupying approximately the same space.  It is generally a safe
      idea to make the centers the same as a template, since it is likely the
      data will be aligned with such a standard template anyway.

      Note that while altering the centers means a shift in space, it does not
      require any resampling of the data.  It is just an alteration of the
      coordinates in the header files.
"""

g_help_other = """
Choose from a few other options.

   giant move:

      This option is useful if the datasets do not start of reasonably close,
      such as when the subject makes a large move between the anatomical and
      EPI sequences, or if the anatomy and EPI datasets are from different
      days.

   add edge:

      This option is to make colored 'edge detection' datasets after alignment.
      The purpose is to give users a different view of how well the alignment
      works.

      Note that only one cost function may be used when 'add edge' is selected.

   anat has skull:

      By default, the anatomical dataset is expected to have a skull, which 
      will be stripped of during the processing of align_epi_anat.py.  If there
      is no skull, this option should be turned off.

   EPI strip method:

      Similarly, the EPI volume will be stripped to remove non-brain voxels.
      The default method is using 3dSkullStrip.

      One can change this to either 3dAutomask (in place of 3dSkullStrip) or
      None (in the case that no EPI stripping should be performed).

   other AEA opts:

      If the user wishes to add any other options to align_epi_anat.py, they
      can be listed here.  If some options seem common, please feel free to
      suggest adding them to the main inteface.
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
