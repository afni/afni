#!/usr/bin/env python

# python3 status: compatible
# (need to modify TcshCommandWindow to wrap text output from process)

import sys, os
from PyQt4 import QtCore, QtGui
import copy, glob

import lib_vars_object as VO
import afni_util as UTIL

g_history = """
  ...         : previous version
  Mar 02, 2011:
     - added static_TextWindow function
     - added open and save actions to TextWindow
     - process local vars in string format, not QString
     - current font is courier bold
  May 11, 2011: added createAction, addActions and resize_table_cols
  May 13, 2011: added make_button, lable, checkbox, line

- glt box:
   - write make_gltsym_table -> to self.gvars.Table_gltsym
   - wrte self.gltsym_list_to_table
   - set as global (call group_box_gltsym)

   - gbox_toggle_frame (close symbolic GLTs box)
   - handle push buttons in CB_gbox_PushB callback
   - be able to update self.gvars.Label_gltsym_len
   - update g_help_gltsym (add info based on sample stim labels, or those
                           for the given subject)
      
"""

g_layout_spacing = 1

# ---------------------------------------------------------------------------
class TextWindow(QtGui.QMainWindow):

    def __init__(self, fname='', text='', title='', viewonly=False,
                 getfile=0, parent=None):
        """open a general text window

                fname   : name of file to read into window
                text    : text to display in window
                title   : window title
                viewonly: can still saveas, but ignore modified flag on close
                          (for viewing output that is already in a file)
                getfile : if fname='', open a file loader widget

           If fname and text are both empty, open a file chooser.
        """

        if title == '': title = 'text view'

        super(TextWindow, self).__init__(parent)
        # self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

        self.status = 0                 # non-zero means error
        self.filename = ''              # associated file name
        self.editor = QtGui.QTextEdit()
        self.setCentralWidget(self.editor)

        OpenAct = self.new_action("&Open", self.openfile,
                shortcut=QtGui.QKeySequence.Open, tip="read new file")
        SaveAct = self.new_action("&Save", self.save,
                shortcut=QtGui.QKeySequence.Save, tip="write file to disk")
        CloseAct = self.new_action("&Close", self.close,
                shortcut=QtGui.QKeySequence.Close, tip="close this window")
        try:
           SaveAsAct = self.new_action("Save &As", self.saveas,
                shortcut=QtGui.QKeySequence.SaveAs, tip="write out to new file")
           actlist = [OpenAct, SaveAct, SaveAsAct, CloseAct]
        except:
           actlist = [OpenAct, SaveAct, CloseAct]

        fileMenu = self.menuBar().addMenu("&File")
        self.add_actions(fileMenu, actlist)

        # try to set a fixed-width font
        font = self.editor.currentFont()
        font.setFamily('courier')
        font.setBold(True)
        self.editor.setCurrentFont(font)

        # open the window with:
        #   - content from given file
        #   - given text
        #   - an 'open file' widget
        #   - or nothing

        # store filename in QString format
        self.filename = fname
        if self.filename != '':
            self.editor.document().setModified(False)
            if not self.readfile(): self.status = 1
        elif text != '':
            self.editor.setPlainText(text)
            self.editor.document().setModified(False)
        elif getfile:
            if not self.openfile(): self.status = 2

        self.resize(700,500)

        self.setWindowTitle(title)

    def new_action(self, text, slot=None, shortcut=None,
                     tip=None, signal="triggered()"):
        action = QtGui.QAction(text, self)
        if slot is not None:
            self.connect(action, QtCore.SIGNAL(signal), slot)
        if shortcut is not None:
            action.setShortcut(shortcut)
        if tip is not None:
            action.setToolTip(tip)
            action.setStatusTip(tip)
        return action

    def add_actions(self, target, actions):
        for action in actions:
            if action is None:
                target.addSeparator()
            else:
                target.addAction(action)

    def closeEvent(self, event):
        if self.editor.document().isModified():
           ans = QtGui.QMessageBox.question(self,
                   "Unsaved Changes",
                   "Save unsaved changes in %s?" % self.filename,
                   QtGui.QMessageBox.Yes|QtGui.QMessageBox.No)
           if ans == QtGui.QMessageBox.Yes:
                self.saveas()

    def openfile(self):
        filename = QtGui.QFileDialog.getOpenFileName(self, "Open File")
        if filename.isEmpty(): return False

        self.filename = str(filename)
        return self.readfile()

    def readfile(self):
        if self.filename == '':
           print('** readfile: no filename set')
           return False
        fp = None
        ret = True
        try:
            fp = QtCore.QFile(self.filename)
            if not fp.open(QtCore.QIODevice.ReadOnly):
                raise IOError(str(fp.errorString()))
        except (IOError, OSError) as e:
           QtGui.QMessageBox.warning(self, "Text Editor: Load Error",
                    "Failed to load %s: %s" % (self.filename, e))
           ret = False

        if not ret: return ret  # bail on error

        stream = QtCore.QTextStream(fp)
        stream.setCodec("UTF-8")
        self.editor.setPlainText(stream.readAll())
        self.editor.document().setModified(False)
        fp.close()

        self.setWindowTitle("file: %s" % \
                QtCore.QFileInfo(self.filename).fileName())

        return ret

    def saveas(self):
        filename = QtGui.QFileDialog.getSaveFileName(self,
                        "Save File As", self.filename,
                        "Text files (*.txt *.* )")
        if not filename.isEmpty():
            self.filename = str(filename)
            self.setWindowTitle("Text Editor: %s" % \
                    QtCore.QFileInfo(self.filename).fileName())
            return self.writefile()
        return False

    def save(self):
        print('++ TW save: "%s"' % self.filename)
        if self.filename == '':
           guiWarning("Error", "** no file set for 'Save'", self)
           return False
        return self.writefile()

    def writefile(self):
        if self.filename == '':
           guiWarning("Error", "** no file set for 'Write'", self)
           return False
        print('++ writing as file %s ...' % self.filename)
        fp = None
        try:
            fp = QtCore.QFile(self.filename)
            if not fp.open(QtCore.QIODevice.WriteOnly):
                raise IOError(str(fp.errorString()))
            stream = QtCore.QTextStream(fp)
            stream << self.editor.toPlainText()
            self.editor.document().setModified(False)
        except (IOError, OSError) as e:
            QtGui.QMessageBox.warning(self, "File Save Error",
                "Error saving text file %s : %s" % (self.filename, e))
        finally:
            if fp is not None: fp.close()
        return True

# end TextWindow class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
def static_TextWindow(fname='', text='', title='', parent=None):
   """create a 'delete on exit' TextWindow"""
   stitle = '(static) %s' % title
   win = TextWindow(text=text, title=stitle, fname=fname, parent=parent)
   if win.status:
      print('** failure creating TextWindow')
      del(win)
      return None
   else:
      win.setAttribute(QtCore.Qt.WA_DeleteOnClose)
      win.show()
      return win

def create_button_list_widget(labels, tips=None, cb=None, dir=0, hstr=0,
                              hind=-1, style=None):
   """create a layout of buttons within a QWidget
        - buttons will be stored as 'bdict' within the returned QWidget
           - bdict[label] = button
        - if tips is set (length should match), setStatusTip
        - if cb is set, connect all call-backs to it
        - if dir = 1, layout direction is vertical, else horizontal
        - hstr is for Horizontal stretch policy, 1 to stretch
        - hind is index for help widget: add spacer and setIcon
           - if style: use for help icon
      return a QWidget (with layout and buttons in dict)
   """

   # main widget to return
   bwidget = QtGui.QWidget()

   if tips != None:
      if len(tips) != len(labels):
         print('** CBLW: %d labels does not match %d tips, discarding...' \
               % (len(labels), len(tips)))
         tips = None

   if dir: layout  = QtGui.QVBoxLayout()
   else:   layout  = QtGui.QHBoxLayout()

   bwidget.bdict = {}
   for ind, lab in enumerate(labels):
      button = QtGui.QPushButton(lab)
      # if cb: button.clicked.connect(cb)
      if cb: button.connect(button, QtCore.SIGNAL('clicked()'), cb)
      policy = button.sizePolicy()
      policy.setHorizontalPolicy(hstr)
      button.setSizePolicy(policy)
      if tips != None: button.setStatusTip(tips[ind])
      _set_button_style(button)
      bwidget.bdict[lab] = button
      if ind > 0: layout.addStretch()
      if ind == hind and style:
         button.setIcon(style.standardIcon(QtGui.QStyle.SP_MessageBoxQuestion))
      layout.addWidget(button)

   bwidget.setLayout(layout)
   layout.setMargin(1)

   return bwidget

def make_button(label, tip=None, cb=None, hstr=0):
   """create a single button, possibly with a tip, callback and stretch
      policy"""
   button = QtGui.QPushButton(label)
   if cb: button.connect(button, QtCore.SIGNAL('clicked()'), cb)

   policy = button.sizePolicy()
   policy.setHorizontalPolicy(hstr)
   button.setSizePolicy(policy)

   if tip != None: button.setStatusTip(tip)
   _set_button_style(button)

   return button

def make_label(text, tip='', hstr=0):
   """create a label that does not stretch"""
   label = QtGui.QLabel(text)
   policy = label.sizePolicy()
   policy.setHorizontalPolicy(hstr)
   label.setSizePolicy(policy)
   if tip != '': label.setStatusTip(tip)
   return label

def make_checkbox(text, checked=0, tip=None, cb=None, hstr=0):
   cbox = QtGui.QCheckBox(text)
   cbox.setChecked(checked)
   if tip != None: cbox.setStatusTip(tip)
   if cb  != None: cbox.connect(cbox, QtCore.SIGNAL('clicked()'), cb)

   policy = cbox.sizePolicy()
   policy.setHorizontalPolicy(hstr)
   cbox.setSizePolicy(policy)

   return cbox

def make_line(text='', cb=None, hstr=1):

   line = QtGui.QLineEdit()
   line.setText(text)
   if cb != None: line.connect(line, QtCore.SIGNAL("editingFinished()"), cb)

   policy = line.sizePolicy()
   policy.setHorizontalPolicy(hstr)
   line.setSizePolicy(policy)

   return line

def create_button_grid(labels, tips=None, cb=None, rlen=4):
   """create a layout of buttons within a QWidget
        - buttons will be stored as 'blist' within the returned QWidget
        - if tips is set (length should match), setStatusTip
        - if cb is set, connect all call-backs to it
        - rlen = number of buttons per row
          (if any label is None: start a new row)
      return a QWidget (with layout and buttons in blist)
   """

   # main widget to return
   bwidget = QtGui.QWidget()

   if tips != None:
      if len(tips) != len(labels):
         print('** CBG: %d labels does not match %d tips, discarding...' \
               % (len(labels), len(tips)))
         tips = None

   layout = QtGui.QGridLayout()

   # bwidget.blist = [QtGui.QPushButton(lab) for lab in labels]
   bwidget.bdict = {}
   row, col = 0, 0
   for ind, label in enumerate(labels):
      if label == None:         # start a new row
         col = 0
         row += 1
         continue
      if col == rlen: # but keep going
         col = 0
         row += 1

      button = QtGui.QPushButton(label)
      bwidget.bdict[label] = button

      # if cb: button.clicked.connect(cb)
      if cb: bwidget.connect(button, QtCore.SIGNAL('clicked()'), cb)
      policy = button.sizePolicy()
      policy.setHorizontalPolicy(0)
      button.setSizePolicy(policy)
      if tips != None: button.setStatusTip(tips[ind])
      _set_button_style(button)
      layout.addWidget(button, row, col)
      col += 1

   layout.setSpacing(g_layout_spacing)
   bwidget.setLayout(layout)

   return bwidget

def create_label_line_grid(labels, tips=None, cb=None, rlen=2):
   """create a layout of buttons within a QWidget
        - labels will be stored as gvars.lab_list within the returned QWidget
        - lines will be stored as gvars.line_list within the returned QWidget
        - if tips is set (length should match), setStatusTip
        - rlen = number of items per row
          (line is always extendable and last on grid row)
      return a QWidget
   """

   # main widget to return
   widget = QtGui.QWidget()
   widget.gvars = VO.VarsObject(name='label_line_grid')

   if tips:
      if len(tips) != len(labels):
         print('** CLLG: %d labels does not match %d tips, discarding...' \
               % (len(labels), len(tips)))
         tips = None

   layout = QtGui.QGridLayout()

   widget.gvars.lab_list = []
   widget.gvars.line_list = []

   for ind, label in enumerate(labels):
      if tips: tip = tips[ind]
      else:    tip = ''
      qlab = make_label(label, tip)
      qline = make_line('', cb)
      if tip: qline.setStatusTip(tip)

      widget.gvars.lab_list.append(qlab)
      widget.gvars.line_list.append(qline)

      layout.addWidget(qlab, ind, 0)
      layout.addWidget(qline, ind, rlen-1)

   layout.setSpacing(g_layout_spacing)
   widget.setLayout(layout)

   return widget

def _set_button_style(button):

   # maybe...
   # button.setProperty('color', QtGui.QColor('green'))
   return

def print_icon_names():
   print('=== Icon keys: ')
   for key in list(QtGui.QStyle.__dict__.keys()):
      if key[0:3] == 'SP_': print('key = %s' % key)

def create_menu_button(parent, name, menu_list, call_back=None):
   """with a menu button, the call_back is applied to the menu action
      (note that the text will come from the list)"""
   pushb = make_button(name)

   menu = QtGui.QMenu(parent)

   act_dict = {}
   for item in menu_list:
      action = menu.addAction(item)
      # if call_back: action.triggered.connect(call_back)
      if call_back: menu.connect(action,QtCore.SIGNAL('triggered()'),call_back)
      act_dict[item] = action
   pushb.setMenu(menu)
   pushb.act_dict = act_dict

   return pushb

def create_label_lineedit_widget(ltext, ltip='', etext='', ecb=None):
   """create a widget with H Layout containing label and lineedit
        QLabel    QLineEdit
        (name)    (display text)                        16 Oct 2012

      return widget"""

   widget = QtGui.QWidget()
   layout = QtGui.QHBoxLayout()
   label  = make_label(ltext, ltip)
   line   = make_line(etext, cb=ecb)

   layout.addWidget(label)
   layout.addWidget(line)

   widget.setLayout(layout)
   widget.LineEdit = line

   return widget

def create_display_label_pair(name, text, tip=''):
   """create a non-editable label pair (sunken panel) with the given text
        QLabel    QLabel
        (name)    (display text)

      if tip, set a StatusTip for both labels

      return 2 labels"""
   name_label = QtGui.QLabel(name)
   text_label = QtGui.QLabel(text)

   text_label.setFrameStyle(QtGui.QFrame.Panel)
   text_label.setFrameShadow(QtGui.QFrame.Sunken)

   if tip:
      name_label.setStatusTip(tip)
      text_label.setStatusTip(tip)

   return name_label, text_label

def createAction(parent, text, slot=None, shortcut=None, tip=None,
                 checkable=False, icon=None, signal="triggered()"):
   """create an action item to add to a menu or tool bar

      possibly: - add a shortcut
                - set tool tip and status tip
                - set call-back (slot)
                - make checkable
                - set an icon
   """

   action = QtGui.QAction(text, parent)
   if shortcut is not None:
      action.setShortcut(shortcut)
   if tip is not None:
      action.setToolTip(tip)
      action.setStatusTip(tip)
   if slot is not None:
      parent.connect(action, QtCore.SIGNAL(signal), slot)
   if checkable:
      action.setCheckable(True)
   if icon is not None:
      action.setIcon(icon)
   return action

def addActions(parent, target, actions):
   """add a list of actions to the target

      if an action is None, add as a separator
   """

   for action in actions:
      if action is None:
         target.addSeparator()
      else:
         target.addAction(action)

def valid_as_int(text, name, warn=0, wparent=None, empty_ok=1):
   """the text can be either empty (if empty_ok) or be an int
      if not and 'warn' is set, show a warning message
      return 1 if valid, 0 otherwise
   """
   # search for valid cases
   valid = 1
   if len(text) == 0:
      if empty_ok: return 1  
      extext = "<empty>"
      valid = 0
   else:
      try: val = int(text)
      except:
         valid = 0
         extext = '   <not valid as a int>'

   if valid: return 1

   if warn: guiWarning(                                                 \
               "Error: invalid identifier",                             \
               "bad text: %s%s\n\n"                                     \
               "Characters in field '%s' must be alphabetic, numeric\n" \
               "or '_' (underscore), starting with alphabetic."         \
               % (text, extext, name), wparent)

   return 0

def valid_as_float(text, name, warn=0, wparent=None, empty_ok=1):
   """the text can be either empty (if empty_ok) or be a float
      if not and 'warn' is set, show a warning message
      return 1 if valid, 0 otherwise
   """
   # search for valid cases
   valid = 1
   if len(text) == 0:
      if empty_ok: return 1  
      extext = "<empty>"
      valid = 0
   else:
      try: val = float(text)
      except:
         valid = 0
         extext = '   <not valid as a float>'

   if valid: return 1

   if warn: guiWarning(                                                 \
               "Error: invalid identifier",                             \
               "bad text: %s%s\n\n"                                     \
               "Expected float in field '%s'.\n"                        \
               % (text, extext, name), wparent)

   return 0

def valid_as_float_list(text, name, warn=0, wparent=None, empty_ok=1, clen=0):
   """the text can be either empty (if empty_ok) or be a float
      - if not and 'warn' is set, show a warning message
      - if clen > 0 and length does not match, report an error
      return 1 if valid, 0 otherwise
   """
   # search for valid cases
   valid = 1
   if len(text) == 0:
      if empty_ok: return 1  
      extext = "<empty>"
      valid = 0
   else:
      slist = text.split()
      if clen > 0 and len(slist) != clen:
         extext = '   <%d floats are required>'
         valid = 0
      else:
         try: flist = [float(sval) for sval in slist]
         except:
            extext = '   <not valid as list of floats>'
            valid = 0

   if valid: return 1

   if clen == 0: fstr = 'float list'
   else:         fstr = '%d float entries' % clen

   if warn: guiWarning(                                                 \
               "Error: invalid identifier",                             \
               "bad text: %s%s\n\n"                                     \
               "Expected %s in field '%s'.\n"                           \
               % (text, extext, fstr, name), wparent)

   return 0

def valid_as_identifier(text, name, warn=0, wparent=None, empty_ok=1):
   """the text can be either empty (if empty_ok) or be of the form:
        alpha, (alphanum|_)*

      if not and 'warn' is set, show a warning message

      return 1 if valid, 0 otherwise
   """

   # in a bad case, display ttext in error message

   # search for valid cases
   if len(text) == 0:
      if empty_ok: return 1  
      extext = "<empty>"
      valid = 0
   else:
      # check for valid characters
      # replace [._] with alpha, then check s[0].isalpha and rest isalphanum
      # scopy = copy.deepcopy(text)
      scopy = str(text)
      if scopy[0].isalpha():                    # first character is good
         scopy = scopy.replace('_', 'x')        # swap out '_' to use isalnum()
         scopy = scopy.replace('.', 'x')        # swap out '.' to use isalnum()
         if scopy.isalnum(): return 1           # then VALID
      
      if ' ' in text or '\t' in text: extext = '     <contains whitespace>'
      else:                           extext = ''

   # if here, invalid

   if warn: guiWarning(                                                 \
               "Error: invalid identifier",                             \
               "bad text: %s%s\n\n"                                     \
               "Characters in field '%s' must be alphabetic, numeric\n" \
               "or '_' (underscore), starting with alphabetic."         \
               % (text, extext, name), wparent)

   return 0

def valid_as_sub_brick(text, name, warn=0, wparent=None, empty_ok=1,
                      goodlist=[], badlist=[]):
   """be simple for now, just append # in goodlist for valid_as_filename
   """
   return valid_as_filename(text, name, warn=warn, wparent=wparent,
                            empty_ok=empty_ok, goodlist=['#'])

def valid_as_filename(text, name, warn=0, wparent=None, empty_ok=1,
                      goodlist=[], badlist=[]):
   """the text can be either empty (if empty_ok) or consist of:
        alphanum, [@%-_+=,.:/]

      goodlist: items will be added to punctlist
      badlist:  items will be removed to punctlist

      if not and 'warn' is set, show a warning message

      return 1 if valid, 0 otherwise
   """

   punctlist = ['@', '%', '-', '_', '+', '=', ',', '.', ':', '/']
   # add good items and remove bad ones
   for item in goodlist:
      if not item in punctlist: punctlist.append(item)
   for item in badlist:
      if item in punctlist: punctlist.remove(item)
   # puncttext = '@%-_+=,.:/'
   puncttext = ''.join(punctlist)

   # search for valid cases
   if len(text) == 0:
      if empty_ok: return 1  
      extext = "<empty>"
   else:
      # replace punctlist chars with 'x' and call isalphanum
      scopy = str(text)
      for p in punctlist:
         scopy = scopy.replace(p, 'x')  # swap out any punctuation
      if scopy.isalnum(): return 1      # if alphanumeric, we're good ...
      
      if ' ' in text or '\t' in text: extext = '     <contains whitespace>'
      else:                           extext = ''

   # if here, invalid

   if warn: guiWarning(                                         \
               "Error: invalid name",                           \
               "bad text: %s%s\n\n"                             \
               "Characters in field '%s' must be alphabetic,\n" \
               "numeric, or in : %s"                            \
               % (text, extext, name, puncttext), wparent)

   return 0

def valid_as_filepath(text, name, warn=0, wparent=None, empty_ok=1):
   """text should look like a path to an existing file
      (do not allow spaces or tabs in name)

      if not and 'warn' is set, show a warning message

      return 1 if valid, 0 otherwise
   """

   # in a bad case, display ttext in error message

   # search for valid cases
   if len(text) == 0:
      if empty_ok: return 1     # VALID
      extext = "<empty>"
   else:
      # just check for whitespace, then existence
      if ' ' in text or '\t' in text: extext = '     <contains whitespace>'
      elif not os.path.isfile(text):  extext = '     <does not exist>'
      else: return 1            # VALID

   # if here, invalid

   if warn: guiWarning(                                                    \
               "Error: invalid as filename",                               \
               "bad text: %s%s\n\n"                                        \
               "Name in field '%s' must exist and not contain whitespace." \
               % (text, extext, name), wparent)

   return 0

def guiMessage(title, text, parent):
   mbox = QtGui.QMessageBox(QtGui.QMessageBox.Information,
                            title, text, QtGui.QMessageBox.Ok, parent)
   mbox.show()

def guiWarning(title, text, parent):
   mbox = QtGui.QMessageBox(QtGui.QMessageBox.Warning,
                            title, text, QtGui.QMessageBox.Ok, parent)
   mbox.show()

def guiError(title, text, parent):
   mbox = QtGui.QMessageBox(QtGui.QMessageBox.Critical,
                            title, text, QtGui.QMessageBox.Ok, parent)
   mbox.show()

def resize_table_cols(table):
   """resize to column contents, unless it is in strech_cols"""

   ncols = table.columnCount()

   # resize columns to fit data
   for col in range(ncols):          # to be safe, work on column indices
      if col in table.stretch_cols:
        table.horizontalHeader().setResizeMode(col,QtGui.QHeaderView.Stretch)
      else:
        table.horizontalHeader().setResizeMode(col,
                                 QtGui.QHeaderView.ResizeToContents)

# StringTable: todo
#    - table is entirely composed of strings
#    - delete row/subject (delete column?)
#    - add row
#    - sort by any column
#       - set 'sort-by' column
#    - init with 2D array
#    - init with Var list and list of var attributes
#
#
class StringTable(QtGui.QTableWidget):
   """sized widget containing a grid of entries
        Label_count is optionally set to track table length
   """

   def __init__(self, name='no name',headings=[], stretch_cols=[], parent=None,
                verb=1):

      super(StringTable, self).__init__(parent)

      self.lvars = VO.VarsObject(name=name)
      self.lvars.verb = verb

      self.Label_count = None           # track table length

      self.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)

      self.set_col_headers(headings)
      self.set_stretch_cols(stretch_cols)
      self.setSortingEnabled(True)

   def set_count_label(self, label):
      """store label to track number of table entries"""
      self.Label_count = label

   def set_col_headers(self, headers):
      if not headers: return 0

      ncols = len(headers)
      self.setColumnCount(ncols)
      self.setHorizontalHeaderLabels(headers)
      if self.lvars.verb > 2: print('++ ST set headers: %s' % headers)
      return 0

   def set_stretch_cols(self, stretch_cols):
      if not stretch_cols: return 0

      ncols = self.columnCount()
      self.lvars.stretch_cols = stretch_cols[:]

      for col in stretch_cols:
         if col < 0 or col >= ncols: continue   # maybe table was cleared
         self.horizontalHeader().setResizeMode(col, QtGui.QHeaderView.Stretch)

      return 0

   def populate(self, data, col_headers=None):
      """populate with strings, maybe set column headers
            - each row must have the same length
            - entries of None or '' are okay
         return 0 on success, 1 on error
      """
      self.clearContents()
      self.setRowCount(0)
      self.setColumnCount(0)
      if not data:
         self.resize_table()
         return 0

      if col_headers: self.set_col_headers(col_headers)

      nrows = len(data)
      ncols = len(data[0])

      self.setColumnCount(ncols)

      # check ncols per row
      for row in data:
         if len(row) != ncols:
            guiError('Bad Table entries',
                'inconsistent column lengths: %s' % [len(row) for row in data],
                self)
            return 1

      # fill table with text rows
      for row, rdata in enumerate(data):
         self.insertRow(row)
         for col, val in enumerate(rdata):
            self.setItem(row, col, QtGui.QTableWidgetItem(val))

      if self.lvars.verb > 2:
         print('-- table populated: %d x %d table with %d x %d data' \
               % (self.rowCount(), self.columnCount(), nrows, ncols))

      self.resize_table()

      return 0

   def resize_cols(self):
      """resize to column contents, unless it is in stretch_cols"""
      ncols = self.columnCount()

      scols = self.lvars.val('stretch_cols')
      if scols == None: scols = []

      for col in range(ncols):
         if col in scols:
            self.horizontalHeader().setResizeMode(col,QtGui.QHeaderView.Stretch)
         else: self.horizontalHeader().setResizeMode(col,
                                    QtGui.QHeaderView.ResizeToContents)

   def show_size(self, mesg = ''):
      # rcr - maybe remove all such calls?
      if self.lvars.verb > 2:
         print('-- table size %s: %d x %d' \
               % (mesg, self.rowCount(), self.columnCount()))

   def resize_table(self):
      """resize row heights and column widths
         if self.Label_count is set, update the text with #rows
      """

      lv = self.lvars

      self.resizeRowsToContents()
      self.remove_blank_table_rows()

      nrows = self.rowCount()
      if nrows > 0: rheight = self.rowHeight(0)
      else:         rheight = 0

      if lv.verb > 2:
         print('-- resize table (%s) to row height %d' % (lv.name, rheight))

      self.setAlternatingRowColors(True)
      self.setFixedHeight(self.max_size(nrows, rheight=rheight))
      self.resize_cols()

      self.show_size('resize C')
      if self.Label_count:
         self.Label_count.setText('%d' % nrows)

   def max_size(self, nrows, rheight=0):
      """return the number of pixels to use based on the number of rows
         (scale by any passed row height)"""
      if rheight > 0: rowheight = rheight
      else:           rowheight = 25

      if nrows <= 6:   show_rows = nrows
      elif nrows < 18: show_rows = 6+0.5*(nrows-6)  # after 6, add half, per
      else:            show_rows = 12

      return min(200, max(75, rheight*(show_rows+1.25)))

   def delete_selected_rows(self):
      """delete all table rows that are selected"""
      sel_items = self.selectedItems()
      if len(sel_items) == 0: return
      for item in sel_items:
         item.setText('')
      self.resize_table()

   def remove_blank_table_rows(self):
      """delete rows that are blank"""

      nrows = self.rowCount()
      ncols = self.columnCount()

      # work from row = nrows-1 down to 0, so deletion does not affect indices
      for row in range(nrows-1, -1, -1):
         found = 0
         # search for something in this row
         for col in range(ncols):
            item = self.item(row, col)
            if item != None:
               if str(item.text()) != '':
                  found = 1
                  break
         if not found: self.removeRow(row)

      return 0

# end StringTable class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# class to fill a table with datasets (in many ways)
class DatasetTableWidget(QtGui.QWidget):
   """dataset table widget class:
                control buttons (via create_button_list_widget)
                dataset table
                descriptive labels (no edit)
                   - common directory
                   - wildcard form
                   - dataset count

      inputs:
        - parent
        - list of button list widgets (from create_button_list_widget)
            - maybe init, add, clear, help

      output class contains (in self.gvars):
         button_widgets, table, Label_common_dir, Label_wildcard, Label_ndsets
      output class contains (in self.lvars):
         dset_col, verb, cb_edited

      todo: methods to write
         - populate, delete selected rows?
         - set_edited_callback
         - get_table_data
   """

   def __init__(self, parent, button_widgets=[], verb=1):

      super(DatasetTableWidget, self).__init__(parent)

      self.lvars = VO.VarsObject(name='DatasetTableWidget local vars')
      self.lvars.dset_col  = 0          # column with dataset names
      self.lvars.sid_col   = 0          # column with subject IDs
      self.lvars.sort_col  = 0          # column to sort by, unless specified
      self.lvars.cb_edited = None       # callback for when table is edited
      self.lvars.verb      = verb

      self.gvars = VO.VarsObject(name='DatasetTableWidget GUI vars')
      vlayout = QtGui.QVBoxLayout(self)

      # add any button widgets
      for bwid in button_widgets: vlayout.addWidget(bwid)
      self.gvars.button_widgets = button_widgets

      # add an empty table
      self.gvars.table = StringTable(parent=self, verb=verb)
      vlayout.addWidget(self.gvars.table)

      # now add non-edit labels for common dir, wildcard and dataset count

      bwidget = QtGui.QWidget()
      layout = QtGui.QGridLayout()

      nlabel, tlabel = create_display_label_pair('common directory', '')
      layout.addWidget(nlabel, 0, 0)
      layout.addWidget(tlabel, 0, 1)
      self.gvars.Label_common_dir = tlabel

      nlabel, tlabel = create_display_label_pair('wildcard form', '')
      layout.addWidget(nlabel, 1, 0)
      layout.addWidget(tlabel, 1, 1)
      self.gvars.Label_wildcard = tlabel

      nlabel, tlabel = create_display_label_pair('dataset count', '0')
      layout.addWidget(nlabel, 2, 0)
      layout.addWidget(tlabel, 2, 1)
      self.gvars.Label_ndsets = tlabel

      # and add count label to table
      self.gvars.table.set_count_label(tlabel)

      # basically fix column 0 and let column 1 grow
      layout.setColumnStretch(0, 1)
      layout.setColumnStretch(1, 20)

      bwidget.setLayout(layout)
      vlayout.addWidget(bwidget)
      vlayout.setSpacing(g_layout_spacing)

   def set_generic_col(self, name, col):
      ncols = self.gvars.table.columnCount()
      default = 0
      if col < 0 or col >= ncols: col = default
      cname = '%s_col' % name
      self.lvars.set_var(cname, col)
      if self.lvars.verb > 2:
         val = self.lvars.val(cname)
         print('++ set %s to %s %s' % (cname, val, type(val)))

   def get_generic_col(self, name, default=0):
      ncols = self.gvars.table.columnCount()
      cname = '%s_col' % name
      val = self.lvars.val(cname)
      if val < 0 or (val >= ncols and ncols > 0):
         if default < 0: default = 0
         print('** GDC: bad %s = %d (ncols = %d), resetting to %d...' \
               % (cname, val, ncols, default))
         self.lvars.set_var(cname, default)
         val = default
      if self.lvars.verb > 2:
         print('++ get %s as %s %s' % (cname, val, type(val)))
      return val

   def set_dset_col(self, col):
      self.set_generic_col('dset', col)
   def set_sid_col(self, col):
      self.set_generic_col('sid', col)

   def get_dset_col(self):
      return self.get_generic_col('dset', self.gvars.table.columnCount()-1)
   def get_sid_col(self):
      return self.get_generic_col('sid', 0)

   def get_data(self):
      """return 2D array of data from table"""
      table = self.gvars.table
      nrows = table.rowCount()
      ncols = table.columnCount()
      if nrows == 0: return []

      dcol = self.get_dset_col()
      common_dir = str(self.gvars.Label_common_dir.text())

      if self.lvars.verb > 2:
         print('-- GD: return col %d from %dx%d table' % (dcol, nrows, ncols))

      dlist = []
      for row in range(nrows):
         drow = []
         for col in range(ncols):
            item = table.item(row, col)
            dd = str(item.text())
            if col == dcol:   # possibly prepend common_dir to datasets
               if common_dir: dd = '%s/%s' % (common_dir, dd)
            drow.append(dd)
         dlist.append(drow)

      return dlist

   def get_dset_list(self):
      """return simple array of datasets, including any common directory"""
      table = self.gvars.table
      nrows = table.rowCount()
      if nrows == 0: return []

      dcol = self.get_dset_col()
      common_dir = str(self.gvars.Label_common_dir.text())

      if self.lvars.verb > 2:
         print('-- GDList: return col %d from %dx%d table'%(dcol, nrows, ncols))

      dlist = []
      for row in range(nrows):
         item = table.item(row, dcol)
         dset = str(item.text())
         if common_dir: dset = '%s/%s' % (common_dir, dset)
         dlist.append(dset)

      return dlist

   def get_short_dset_list(self):
      """return simple array of datasets, without any common directory"""
      table = self.gvars.table
      nrows = table.rowCount()
      if nrows == 0: return []

      dcol = self.get_dset_col()

      if self.lvars.verb > 2:
         print('-- GSDList: return col %d from %dx%d table'%(dcol, nrows, ncols))

      dlist = []
      for row in range(nrows):
         item = table.item(row, dcol)
         dlist.append(str(item.text()))

      return dlist

   def update_table_column(self, darray, col):
      """modify just the given column of the table
         (e.g. short dataset names or subject IDs)
      """
      table = self.gvars.table
      nrows = table.rowCount()
      if nrows == 0: return

      if self.lvars.verb > 2:
         print('++ update table column %d with %d elements' % (col,len(darray)))
         if self.lvars.verb > 3:
            print('   nrows %d, elements: %s' % (nrows, darray))

      for row in range(nrows):
         item = table.item(row, col)
         item.setText(darray[row])

   def set_sort_col(self, scol):
      """if the passed column is >= 0, apply, else apply 0"""
      if scol >= 0: self.lvars.sort_col = scol
      else:         self.lvars.sort_col = 0

   def set_stretch_col(self, scol):
      """if the passed column is >= 0, apply, else apply 0"""
      if scol >= 0: self.gvars.table.set_stretch_cols([scol])
      else:         self.gvars.table.set_stretch_cols([0])

   def simple_populate(self, dsets):
      """populate with datasets, assuming to go after sids and dsets"""
      dlist = [['', dd] for dd in dsets]
      self.populate(dlist, col_headers=['sid','dataset'], dset_col=1, sid_col=0)
      self.set_stretch_col(1)
      if self.lvars.sort_col >= 0:
         self.gvars.table.sortItems(self.lvars.sort_col)

   def populate(self, data, col_headers=None, dset_col=-1, sid_col=-1,
                make_sids=1):
      """fill the table with data, breaking datasets into pieces
         if dset_col or sid_col, set them
         if make_sids, try to create them from the dataset list
      """
      if self.lvars.verb > 2:
         # rcr - keep?
         nr = len(data)
         nc = 0
         if nr > 0: nc = len(data[0])
         t = self.gvars.table
         print('++ populate %d x %d table with %d x %d data, dc=%d, sc=%d' \
               % (t.rowCount(), t.columnCount(), nr, nc, dset_col, sid_col))
         
      self.gvars.table.populate(data, col_headers)

      self.set_dset_col(dset_col)
      self.set_sid_col(sid_col)
      dcol = self.get_dset_col()
      scol = self.get_sid_col()

      dsets = [row[dcol] for row in data]
      cdir, snames, globstr = UTIL.flist_to_table_pieces(dsets)
      self.update_table_column(snames, dcol)
      self.gvars.Label_common_dir.setText(cdir)
      self.gvars.Label_wildcard.setText(globstr)
      self.set_sort_col(dcol)

      # and try to set subject IDs
      if make_sids and scol >= 0:
         sids = UTIL.get_ids_from_dsets(snames)
         # rcr : if failure, clear table and whine???
         if not sids: sids = ['' for name in snames]
         self.update_table_column(sids, scol)
         # if there is something to sort by, exptect to
         if not UTIL.vals_are_constant(sids, ''): self.set_sort_col(scol)

      if self.lvars.verb>1: print('-- populated, scol %d, dcol %d'%(scol, dcol))

# end DatasetTableWidget class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# class to fill a table with datasets (in many ways)
class DsetChooser(QtGui.QDialog):
   """class to fill a table with datasets, in any of many ways

        - list of buttons, for ways to init or add to table
        - 'delete selected rows' and 'clear' buttons
        - DatasetTable
   """

   def __init__(self, name='Dset Chooser', parent=None, verb=1,
                helpstr=None):

      super(DsetChooser, self).__init__(parent)

      self.gvars = VO.VarsObject(name='DsetChooser GUI vars')
      self.lvars = VO.VarsObject(name='DsetChooser local vars')
      self.lvars.verb = verb
      self.lvars.pop_cb = None

      # make help window
      global helpstr_dset_chooser
      if helpstr == None: helpstr = helpstr_dset_chooser
      self.gvars.help_win = TextWindow(parent=self)
      self.gvars.help_win.setWindowTitle('Dataset Chooser Help')
      self.gvars.help_win.editor.setText(helpstr)

      vlayout = QtGui.QVBoxLayout(self)

      # create button list for 
      gbox = self.make_input_group_box()
      vlayout.addWidget(gbox)

      # create button list widget
      labels = ['delete selected entries', 'clear table', 'help']
      tips   = ['delete highlighted rows from table',
                'delete all entries in table', 'display help for this section']
      bwidget = create_button_list_widget(labels, cb=self.cb_pushb, tips=tips,
                                          hind=2)

      self.gvars.table_widget = DatasetTableWidget(self,
                        button_widgets=[bwidget], verb=self.lvars.verb)

      vlayout.addWidget(self.gvars.table_widget)

      # and add OK and QUIT buttons
      labels = ['OK', 'Quit']
      tips   = ['apply dataset list to calling window',
                'close this window and cancel the operation']
      bwidget = create_button_list_widget(labels, cb=self.cb_pushb, tips=tips)
      vlayout.addWidget(bwidget)

      vlayout.setSpacing(g_layout_spacing)
      self.setWindowTitle(name)

   def make_input_group_box(self):
      gbox = get_styled_group_box("init table via wildcard pattern")
      glayout = QtGui.QGridLayout(gbox)

      label1  = make_label('1. select representative file',
                           'choose a file to alter into a wildcard pattern')
      button1 = make_button('choose file',
                            'choose a file to alter into a wildcard pattern',
                            cb=self.cb_pushb)
      label2  = make_label('2. alter name into desired wildcard pattern',
                           'modify the filename text into a wildcard string')
      line2   = make_line ('')
      label3  = make_label('3. apply wildcard pattern to fill table',
                           'fill table with files matching pattern')
      button3 = make_button('apply pattern',
                            'fill table with files matching pattern',
                            cb=self.cb_pushb)
      self.gvars.Line_wild_rep = line2
      self.gvars.PushB_apply   = button3
      button3.setFocus()
      self.connect(line2, QtCore.SIGNAL('returnPressed()'),
                   self.wildcard_return_press)

      glayout.addWidget(label1,  0, 0)
      glayout.addWidget(button1, 0, 1)
      glayout.addWidget(label2,  1, 0)
      glayout.addWidget(line2,   2, 0, 1, 2)
      glayout.addWidget(label3,  3, 0)
      glayout.addWidget(button3, 3, 1)

      glayout.setColumnStretch(0, 20)
      glayout.setColumnStretch(1, 1)

      return gbox

   def set_populate_cb(self, callback):
      """the calling object should specify what to do once the user clicks
         OK in this dataset chooser"""

      self.lvars.pop_cb = callback

   def cb_okay(self):
      if not self.lvars.pop_cb:
         print('** no callback for datasets')
         return
      dlist = self.gvars.table_widget.get_dset_list()
      self.lvars.pop_cb(dlist)
      self.cb_quit()

   def cb_quit(self):
      self.done(0)

   def wildcard_return_press(self):
      self.gvars.PushB_apply.setFocus()

   def apply_pattern(self):
      pattern = str(self.gvars.Line_wild_rep.text())
      if not pattern: 
         guiWarning("Error", "** no pattern to apply", self)
         return
      glist = glob.glob(pattern)
      if len(glist) == 0:
         guiWarning("Error", "** no files match wildcard pattern", self)
         return
      self.gvars.table_widget.simple_populate(glist)

   def cb_pushb(self):
      try:
         sender = self.sender()
         text = str(sender.text())
      except:
         print('** DsetChooser:cb_pushb: no sender text')
         return

      if text == 'delete selected entries':
         self.gvars.table_widget.gvars.table.delete_selected_rows()
      elif text == 'clear table':
         self.gvars.table_widget.populate([])
      elif text == 'help':
         self.gvars.help_win.show()
      elif text == 'OK':   self.cb_okay()
      elif text == 'Quit': self.cb_quit()

      # wildcard application buttons
      elif text == 'choose file':
         # fill gvars.Line_wild_rep
         fname = QtGui.QFileDialog.getOpenFileName(self,
                    "choose representative dataset file", '',
                    'datasets (*.HEAD *.nii *.nii.gz);;all files (*)')
         if fname == None: return
         fname = str(fname)
         if not fname: return
         if not valid_as_filepath(fname, 'choose file', 1, self, 1): return
         self.gvars.Line_wild_rep.setText(fname)
         self.gvars.Line_wild_rep.setFocus()
      elif text == 'apply pattern':
         self.apply_pattern()

      else: print("** DC:cb_pushb: unexpected button text '%s'" % text)

# end DsetChooser class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class label_opt_exec_widget(object):
   """QWidget containing row of label, item menu, pushbutton

        parent   : parent widget
        label    : left-most label widget
        menulist : list of menu item names
        pbtext   : pushbutton text
        cb       : callback to apply to PB 'clicked()' event, if set
                  (otherwise, callbacks might be applied via bdict)
        name     : optional name to apply to object
   """

   def __init__(self, parent, label, menulist, pbtext, cb=None, name=None):

      self.mainw = QtGui.QWidget(parent)                # main widget
      if name == None: self.name = 'QWidget with label, item menu, pushbutton'
      else:            self.name = name

      # widget layout
      self.layout = QtGui.QHBoxLayout(self.mainw)

      # add label
      self.label  = QtGui.QLabel(self.mainw)
      self.label.setText(label)
      self.layout.addWidget(self.label)

      # add menu comboBox
      self.menu = QtGui.QComboBox(self.mainw)
      for item in menulist:
         self.menu.addItem(item)
      self.layout.addWidget(self.menu)

      # and finally the pushbutton
      self.pb = QtGui.QPushButton(self.mainw)
      self.pb.setText(pbtext)
      if cb != None: self.pb.connect(self.pb, QtCore.SIGNAL("clicked()"), cb)
      self.layout.addWidget(self.pb)

   def menu_choice(self):
      """return the selected menu index (comboBox)"""
      return self.menu.currentIndex()

# ---------------------------------------------------------------------------
class button_list_widget(object):
   """QWidget containing array of buttons

        parent : parent widget
        labels : list for the names
        cb     : callback to apply to all 'clicked()' events, if set
                 (otherwise, callbacks might be applied via bdict)
        ltype  : layout type: 0=vertical, 1=horizontal"""

   def __init__(self, parent, labels, cb=None, ltype=0):

      self.mainw = QtGui.QWidget(parent)                # main widget
      self.name = 'QWidget with button list'
      self.bdict = {}                                   # button dictionary

      if ltype == 0: layout = QtGui.QVBoxLayout(self.mainw)
      else:          layout = QtGui.QHBoxLayout(self.mainw)

      for label in labels:
         b = QtGui.QPushButton(self.mainw)
         b.setText(label)
         if cb: b.connect(b, QtCore.SIGNAL("clicked()"), cb)
         self.bdict[label] = b
         layout.addWidget(b)

   def get_button_text(self, button):
      """return text for button
            locate this button and return text

         verify that the button is in this widget, then
         return button.text().toAscii().data()

        if no button is found, return 'NO_SUCH_BUTTON'"""

      keys = list(self.bdict.keys())
      for key in keys:
         if self.bdict[key] == button:
            return button.text().toAscii().data()
 
      return 'NO_SUCH_BUTTON'

# end button_list_widget class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class radio_group_box(object):
   """QGroupBox with radio buttons of the labels

        parent  : parent widget
        title   : QGroupBox title
        labels  : list for the names
        default : radiobutton index to set
        ltype   : layout type: 0=vertical, 1=horizontal"""

   def __init__(self, parent, title, labels, default=0, ltype=0):

      self.mainw = QtGui.QGroupBox(title, parent)       # main widget
      self.name = 'QGroupBox of radio buttons'
      self.title = title
      self.blist = []                                   # button list

      if ltype == 0: layout = QtGui.QVBoxLayout(self.mainw)
      else:          layout = QtGui.QHBoxLayout(self.mainw)

      for label in labels:
         rb = QtGui.QRadioButton(label, self.mainw)
         self.blist.append(rb)
         layout.addWidget(rb)

      self.set_checked(default)

   def set_checked(self, index):
      """check the radio button in the blist"""
      if index < 0 or index >= len(self.blist): index = 0
      self.blist[index].setChecked(True)

   def get_checked(self):
      """return the index of the checked radio button"""
      for ind, rb in enumerate(self.blist):
         if rb.isChecked(): return ind
      print("** no button in radio_group_box '%s' isChecked" % self.title)
      return 0  

# end radio_group_box class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class TcshCommandWindow(QtGui.QMainWindow):

   def __init__(self, command, dir='', outfile='', sepwin=0, parent=None):
      """Create a window to display the processing of a given tcsh command.

         The command is executed via 'tcsh -c command'.

         command        - command to execute
         dir            - if set, use as processing directory
         sepwin         - if set, use separate windows for stdout and stderr

      """

      super(TcshCommandWindow, self).__init__(parent)

      # set up widgets
      self.widget = QtGui.QWidget()
      self.Edit_out = QtGui.QTextEdit()   # stdout, and maybe stderr
      self.Bstart = QtGui.QPushButton('Start')
      self.Bstop = QtGui.QPushButton('Stop')

      self.Bstart.setEnabled(False)     # no use in this context?

      # create layout, for either 1 or 2 sub-windows
      layout = QtGui.QGridLayout()

      # try to apply a fixed-with font
      set_font_family(self.Edit_out, "courier", bold=True)

      if sepwin:
         offset = 8
         self.Edit_err = QtGui.QTextEdit()
         set_font_family(self.Edit_err, "courier", bold=True)
         layout.addWidget(self.Edit_out, 0, 0, offset, 5)
         layout.addWidget(self.Edit_err, offset, 0, offset, 5)
         offset += 8
      else:
         offset = 16
         layout.addWidget(self.Edit_out, 0, 0, offset, 5)
         self.Edit_err = self.Edit_out  # same edit window

      if dir != '':
         clabel, slabel = create_display_label_pair('exec directory:', dir)
         layout.addWidget(clabel, offset, 0)
         layout.addWidget(slabel, offset, 1, 1, 4)
         offset += 1

      clabel, slabel = create_display_label_pair('exec command:', command)
      layout.addWidget(clabel, offset, 0)
      layout.addWidget(slabel, offset, 1, 1, 4)
      offset += 1

      clabel, slabel = create_display_label_pair('status:', 'starting...')
      layout.addWidget(clabel, offset, 0)
      layout.addWidget(slabel, offset, 1, 1, 4)
      self.Label_status = slabel
      offset += 1

      layout.addWidget(self.Bstart, offset, 2, 1, 1)
      layout.addWidget(self.Bstop, offset, 3, 1, 1)
      offset += 1

      self.widget.setLayout(layout)

      self.setCentralWidget(self.widget)
      self.resize(700,500)

      # updates to QTextEdit widgets
      self.Edit_out.setLineWrapMode(QtGui.QTextEdit.NoWrap)
      self.Edit_err.setLineWrapMode(QtGui.QTextEdit.NoWrap)

      # set callbacks
      # self.Bstart.clicked.connect(self.cb_start)
      # self.Bstop.clicked.connect(self.cb_stop)
      self.connect(self.Bstart, QtCore.SIGNAL('clicked()'), self.cb_start)
      self.connect(self.Bstop,  QtCore.SIGNAL('clicked()'), self.cb_stop)

      self.process = QtCore.QProcess()
      # self.process.readyReadStandardError.connect(self.readstderr)
      # self.process.readyReadStandardOutput.connect(self.readstdout)
      # self.process.finished.connect(self.finished)
      self.connect(self.process, QtCore.SIGNAL('readyReadStandardError()'),
                   self.readstderr)
      self.connect(self.process, QtCore.SIGNAL('readyReadStandardOutput()'),
                   self.readstdout)

      # self.connect(self.process, QtCore.SIGNAL('finished()'), self.finished)
      # 'finished' does not work via QtCore.SIGNAL, but funtion is not so
      # necessary, so just 'try' the new way...
      try: self.process.finished.connect(self.finished)
      except: pass

      self.status  = 0

      self.setWindowTitle('processing command: %s' % command)

      self.command = command
      self.dir = dir

      self.cb_start()

   def update_status(self, status):
      self.Label_status.setText(status)

   def finished(self):
      self.readstderr()
      self.readstdout()
      status = self.process.exitStatus()
      print('-- processed finished, state = %d, status = %d' \
            % (self.process.state(), status))
      
      if status: self.update_status('process finished: FAILURE')
      else:      self.update_status('process finished: SUCCESS')

   def readstdout(self):
      text = str(self.process.readAllStandardOutput())
      if len(text) == 0: return
      editor = self.Edit_out
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.insertPlainText(text)
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.ensureCursorVisible()

   def readstderr(self):
      text = str(self.process.readAllStandardError())
      if len(text) == 0: return
      editor = self.Edit_err
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.insertPlainText(text)
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.ensureCursorVisible()

   def cb_start(self, cmd=''):
      if cmd == '' or cmd == False: cmd = self.command
      if cmd == '' or cmd == False:
         print('** SPW: no command to execute')
         return

      # note directory
      if self.dir != '':
         print('-- setting working dir to %s' % self.dir)
         self.process.setWorkingDirectory(self.dir)

      print('starting command: %s\n' % cmd)
      args = ['-c', cmd]

      self.process.start('tcsh', args)
      if self.process.waitForStarted():
         print('++ process is started, pid = %d' % self.process.pid())
         self.update_status('process running, pid = %d' % self.process.pid())
         self.status = 0
      else:
         print('** failed to start')
         self.update_status('** error: failed to start')
         self.status = -1

   def cb_stop(self):
      print('stopping command %s ...' % self.command)
      self.process.kill()

# end TcshCommandWindow class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class ProcessWindow(QtGui.QMainWindow):

   def __init__(self, command='', dir='', title='', parent=None, sepwin=0):
      """Create a window to display the processing of a passed command, or of
         those specified by the user.

         command        - if set, execute the command
         dir            - if set, use as processing directory
         sepwin         - if set, use separate windows for stdout and stderr

      """
      if title == '': title = 'tcsh: shell command window'

      super(ProcessWindow, self).__init__(parent)

      # track commands
      self.commandlist = []

      # set up widgets
      self.widget = QtGui.QWidget()
      self.Edit_out = QtGui.QTextEdit()   # stdout, and maybe stderr
      self.Edit_hist = QtGui.QTextEdit()
      self.lineedit = QtGui.QLineEdit()
      self.Bstart = QtGui.QPushButton('Start')
      self.Bstop = QtGui.QPushButton('Stop')
      clabel = QtGui.QLabel('tcsh command: ')

      # create layout, for either 1 or 2 sub-windows
      layout = QtGui.QGridLayout()

      set_font_family(self.Edit_out, "courier")
      set_font_family(self.Edit_hist, "courier")

      if sepwin:
         offset = 8
         self.Edit_err = QtGui.QTextEdit()
         set_font_family(self.Edit_err, "courier")
         layout.addWidget(self.Edit_out, 0, 0, offset, 5)
         layout.addWidget(self.Edit_err, offset, 0, offset, 5)
         offset += 8
      else:
         offset = 16
         layout.addWidget(self.Edit_out, 0, 0, offset, 5)
         self.Edit_err = self.Edit_out  # same edit window

      layout.addWidget(clabel, offset, 0)
      layout.addWidget(self.lineedit, offset, 1, 1, 4)
      offset += 1
      layout.addWidget(self.Bstart, offset, 2, 1, 1)
      layout.addWidget(self.Bstop, offset, 3, 1, 1)
      offset += 1

      nlines = 3
      self.Edit_hist.setMaximumSize(700,60)
      layout.addWidget(self.Edit_hist, offset, 0, nlines, 5)
      offset += nlines

      self.widget.setLayout(layout)


      self.setCentralWidget(self.widget)
      self.resize(700,500)

      # updates to QTextEdit widgets
      self.Edit_out.setLineWrapMode(QtGui.QTextEdit.NoWrap)
      self.Edit_err.setLineWrapMode(QtGui.QTextEdit.NoWrap)

      # set callbacks
      # self.lineedit.returnPressed.connect(self.cb_newcommand)
      # self.Bstart.clicked.connect(self.cb_start)
      # self.Bstop.clicked.connect(self.cb_stop)
      self.connect(self.lineedit, QtCore.SIGNAL('returnPressed()'),
                   self.cb_newcommand)
      self.connect(self.Bstart, QtCore.SIGNAL('clicked()'), self.cb_start)
      self.connect(self.Bstop,  QtCore.SIGNAL('clicked()'), self.cb_stop)

      self.process = QtCore.QProcess()
      # self.process.readyReadStandardError.connect(self.readstderr)
      # self.process.readyReadStandardOutput.connect(self.readstdout)
      # self.process.finished.connect(self.finished)
      self.connect(self.process, QtCore.SIGNAL('readyReadStandardError()'),
                   self.readstderr)
      self.connect(self.process, QtCore.SIGNAL('readyReadStandardOutput()'),
                   self.readstdout)

      # self.connect(self.process, QtCore.SIGNAL('finished()'), self.finished)
      # 'finished' does not work via QtCore.SIGNAL, but funtion is not so
      # necessary, so just 'try' the new way...
      try: self.process.finished.connect(self.finished)
      except: pass

      self.status  = 0

      if title != '': self.setWindowTitle(title)
      else:           self.setWindowTitle('command processing')

      self.command = command
      self.dir = dir

      if command != '':
         self.lineedit.setText(command)
         self.cb_start()

   def finished(self):
      self.readstderr()
      self.readstdout()
      print('-- processed finished, state = %d, status = %d' \
            % (self.process.state(), self.process.exitStatus()))

   def readstdout(self):
      text = str(self.process.readAllStandardOutput())
      if len(text) == 0: return
      editor = self.Edit_out
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.insertPlainText(text)
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.ensureCursorVisible()

   def readstderr(self):
      text = str(self.process.readAllStandardError())
      if len(text) == 0: return
      editor = self.Edit_err
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.insertPlainText(text)
      editor.moveCursor(QtGui.QTextCursor.End, QtGui.QTextCursor.MoveAnchor)
      editor.ensureCursorVisible()

   def cb_newcommand(self):
      cmd = str(self.lineedit.text())
      if cmd == '': return

      pstate = self.process.state()
      print('-- trying command %d' % len(self.commandlist))
      if pstate == QtCore.QProcess.NotRunning:
         return self.cb_start()
      else:
         return
         print('** will not run new command until old one is finished')
         print('== state is %s\n' % pstate)

   def cb_start(self, cmd=''):
      if cmd == '' or cmd == False:
         cmd = str(self.lineedit.text())
      if cmd == '': return

      # execute command via "tcsh -c COMMAND"
      args = ['-c', cmd]

      # note directory
      if self.dir != '':
         print('-- setting working dir to %s' % self.dir)
         self.process.setWorkingDirectory(self.dir)

      print('starting command: %s\n' % str(cmd))
      self.commandlist.append(cmd)
      self.Edit_hist.append(cmd)
      self.lineedit.clear()

      self.process.start('tcsh', args)
      if self.process.waitForStarted():
         print('++ process is started, pid = %d' % self.process.pid())
         self.status = 0
      else:
         print('** failed to start')
         self.status = -1

   def cb_stop(self):
      print('stopping')
      self.process.kill()

# end ProcessWindow class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class PyCommandWindow(QtGui.QMainWindow):

   def __init__(self, title='', callback=None, parent=None):
      """open a text window for executing internal python commands

         The callback function should be set.  Note that variables used will
         be with respect to that function.

         The window will be used for a history of commands.
      """

      if title == '': title = 'text view'

      super(PyCommandWindow, self).__init__(parent)
      # self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

      self.widget = QtGui.QWidget()           # main widget
      self.editor = QtGui.QTextEdit()         # history frame
      self.lineedit = QtGui.QLineEdit()       # new command line
      clabel = QtGui.QLabel('command: ')

      layout = QtGui.QVBoxLayout()
      self.editor.resize(300,300)
      layout.addWidget(self.editor)
      
      hwidget = QtGui.QWidget()
      hlayout = QtGui.QHBoxLayout()
      hlayout.addWidget(clabel)
      hlayout.addWidget(self.lineedit)
      hwidget.setLayout(hlayout)

      layout.addWidget(hwidget)

      self.widget.setLayout(layout)
      self.setCentralWidget(self.widget)

      self.callback = callback

      if callback != None:
         # self.lineedit.returnPressed.connect(self.process_command)
         self.connect(self.lineedit, QtCore.SIGNAL('returnPressed()'),
                      self.process_command)

   def process_command(self):
      command = str(self.lineedit.text())
      self.lineedit.clearFocus()
      if self.callback != None: self.callback(command)
      self.editor.append(command)

# end PyCommandWindow class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# generic functions

def get_checked_group_box(title, view=True, ltype='V', cb_check=None):
   """return a GroupBox with:
        gbox.frame = main frame
        gbox.mainlayout = main layout

        ltype:    G, H, V = grid, horizontal or vertical layout
        view:     whether frame is viewable (if so, it will be checked)
        cb_check: callback for when check is toggled
   """
   gbox = get_styled_group_box(title)

   # put frame inside gbox, which we can hide via toggled button
   glayout = QtGui.QVBoxLayout(gbox)
   frame = QtGui.QFrame(gbox)
   frame.setFrameShape(QtGui.QFrame.NoFrame)
   glayout.addWidget(frame)
   gbox.frame = frame

   gbox.setCheckable(True)
   gbox.setChecked(view)
   if view: gbox.frame.show()
   else:    gbox.frame.hide()

   if cb_check: gbox.connect(gbox, QtCore.SIGNAL('clicked()'), cb_check)

   # make the main layout a child of frame
   if   ltype == 'G': mainlayout = QtGui.QGridLayout(frame)
   elif ltype == 'H': mainlayout = QtGui.QHBoxLayout(frame)
   else:              mainlayout = QtGui.QVBoxLayout(frame)

   mainlayout.setSpacing(g_layout_spacing)
   mainlayout.setMargin(0)
   glayout.setMargin(0)

   gbox.mainlayout = mainlayout

   return gbox

def toggle_checked_gbox(gbox):
   """toggle group box between checked/shown and unchecked"""
   if gbox.isChecked():
      tstr = str(gbox.title())
      posn = tstr.find(' (hidden')
      if posn > 0: gbox.setTitle(tstr[0:posn])

      gbox.frame.show()
   else:
      tstr = str(gbox.title())
      gbox.setTitle('%s (hidden but applied)' % tstr)

      gbox.frame.hide()

def get_styled_group_box(title):

   gbox = QtGui.QGroupBox(title)

   # set box color 
   color = QtGui.QColor('blue').light(50)
   palette = QtGui.QPalette(gbox.palette())
   palette.setColor(QtGui.QPalette.Active, QtGui.QPalette.Mid, color)
   gbox.setPalette(palette)

   return gbox

def set_font_family(obj, family, bold=False):
   """set an object's font class and bold type"""

   # none of this seems to work, what am I missing?

   font = obj.currentFont()
   font.setFamily(family)
   font.setBold(bold)
   obj.setCurrentFont(font)

   # obj.setCurrentFont(QtGui.QFont('fixed'))

   #qtf = QtGui.QTextCharFormat()
   #qtf.setFontFixedPitch(True)
   #obj.setCurrentCharFormat(qtf)

   return

# ===========================================================================
# help strings for various classes
# ===========================================================================
helpstr_dset_chooser = """
                choosing a list of datasets

1. select a representative file

      Choose a single dataset file to be included in the list.
      For example, OLSQ.FT.betas+tlrc.HEAD from the AFNI_data6 class data:

         .../AFNI_data6/group_results/OLSQ.FT.betas+tlrc.HEAD
                                          ^^^^

2. alter name into desired wildcard pattern

      Replace part of the name with '*', or other wildcard characters, so
      that it matches the desired list of datasets.  If it matches too many,
      selected datasets can be deleted later.

      For example, alter subject FT's dataset, replacing 'FT' with '*':

         .../AFNI_data6/group_results/OLSQ.*.betas+tlrc.HEAD
                                          ^^^
      
3. hit <enter> 

   or click on 'apply pattern'

      This will fill the table with a list of datasets matching the wildcard
      pattern.

"""



if __name__ == '__main__':
   if '-help' in sys.argv:
      print('this is not helpful')
      sys.exit(0)
   sys.exit(0)
   app = QtGui.QApplication(sys.argv)
   win1 = TextWindow()
   win1.show()
   win2 = TextWindow(text = 'hello, this is\nsome text!')
   win2.show()
   app.exec_()

