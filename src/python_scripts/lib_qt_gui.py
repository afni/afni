
import sys, os
from PyQt4 import QtCore, QtGui
import copy

g_history = """
  ...         : previous version
  Mar 02, 2011:
     - added static_TextWindow function
     - added open and save actions to TextWindow
     - process local vars in string format, not QString
     - current font is courier bold
"""

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
        SaveAsAct = self.new_action("Save &As", self.saveas,
                shortcut=QtGui.QKeySequence.SaveAs, tip="write out to new file")
        CloseAct = self.new_action("&Close", self.close,
                shortcut=QtGui.QKeySequence.Close, tip="close this window")

        fileMenu = self.menuBar().addMenu("&File")
        self.add_actions(fileMenu, [OpenAct, SaveAct, SaveAsAct, CloseAct])

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
           print '** readfile: no filename set'
           return False
        fp = None
        ret = True
        try:
            fp = QtCore.QFile(self.filename)
            if not fp.open(QtCore.QIODevice.ReadOnly):
                raise IOError, unicode(fp.errorString())
        except (IOError, OSError), e:
           QtGui.QMessageBox.warning(self, "Text Editor: Load Error",
                    "Failed to load %s: %s" % (self.filename, e))
           ret = False

        if not ret: return ret  # bail on error

        stream = QtCore.QTextStream(fp)
        stream.setCodec("UTF-8")
        print '-- reading file %s...' % self.filename
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
        print '++ TW save: "%s"' % self.filename
        if self.filename == '':
           guiWarning("Error", "** no file set for 'Save'", self)
           return False
        return self.writefile()

    def writefile(self):
        if self.filename == '':
           guiWarning("Error", "** no file set for 'Write'", self)
           return False
        print '++ writing as file %s ...' % self.filename
        fp = None
        try:
            fp = QtCore.QFile(self.filename)
            if not fp.open(QtCore.QIODevice.WriteOnly):
                raise IOError, unicode(fp.errorString())
            stream = QtCore.QTextStream(fp)
            stream << self.editor.toPlainText()
            self.editor.document().setModified(False)
        except (IOError, OSError), e:
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
      print '** failure creating TextWindow'
      del(win)
      return None
   else:
      win.setAttribute(QtCore.Qt.WA_DeleteOnClose)
      win.show()
      return win

def create_button_list_widget(labels, cb=None, dir=0, hstr=0):
   """create a layout of buttons within a QWidget
        - buttons will be stored as 'blist' within the returned QWidget
        - if cb is set, connect all call-backs to it
        - if dir = 1, layout direction is vertical, else horizontal
        - hstr is for Horizontal stretch policy, 1 to stretch
      return a QWidget (with layout and buttons in blist)
   """

   # main widget to return
   bwidget = QtGui.QWidget()

   if dir: layout  = QtGui.QVBoxLayout()
   else:   layout  = QtGui.QHBoxLayout()

   bwidget.blist = [QtGui.QPushButton(lab) for lab in labels]
   for ind, button in enumerate(bwidget.blist):
      if cb: button.clicked.connect(cb)
      policy = button.sizePolicy()
      policy.setHorizontalPolicy(hstr)
      button.setSizePolicy(policy)
      _set_button_style(button)
      layout.addWidget(button)

   bwidget.setLayout(layout)

   return bwidget

def _set_button_style(button):

   # maybe...
   # button.setProperty('color', QtGui.QColor('green'))
   return

def print_icon_names():
   print '=== Icon keys: '
   for key in QtGui.QStyle.__dict__.keys():
      if key[0:3] == 'SP_': print 'key = %s' % key

def create_menu_button(parent, name, menu_list, call_back=None):
   """with a menu button, the call_back is applied to the menu action
      (note that the text will come from the list)"""
   pushb = QtGui.QPushButton(name)

   menu = QtGui.QMenu(parent)
   for item in menu_list:
      action = menu.addAction(item)
      if call_back: action.triggered.connect(call_back)
   pushb.setMenu(menu)

   return pushb

def create_display_label_pair(name, text):
   """create a non-editable label pair (sunken panel) with the given text
        QLabel    QLabel
        (name)    (display text)

      return 2 labels"""
   name_label = QtGui.QLabel(name)
   text_label = QtGui.QLabel(text)

   text_label.setFrameStyle(QtGui.QFrame.Panel)
   text_label.setFrameShadow(QtGui.QFrame.Sunken)

   return name_label, text_label

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
   else:
      # rcr - here
      try: fval = int(text)
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
   else:
      try: fval = float(text)
      except:
         valid = 0
         extext = '   <not valid as a float>'

   if valid: return 1

   if warn: guiWarning(                                                 \
               "Error: invalid identifier",                             \
               "bad text: %s%s\n\n"                                     \
               "Characters in field '%s' must be alphabetic, numeric\n" \
               "or '_' (underscore), starting with alphabetic."         \
               % (text, extext, name), wparent)

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
   else:
      # check for valid characters
      # replace '_' with alpha, then check s[0].isalpha and rest isalphanum
      # scopy = copy.deepcopy(text)
      scopy = str(text)
      if scopy[0].isalpha():                    # first character is good
         scopy = scopy.replace('_', 'x')        # swap out '_' to use isalnum()
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

def guiWarning(title, text, parent):
   mbox = QtGui.QMessageBox(QtGui.QMessageBox.Warning,
                            title, text, QtGui.QMessageBox.NoButton, parent)
   mbox.show()

def guiError(title, text, parent):
   mbox = QtGui.QMessageBox(QtGui.QMessageBox.Critical,
                            title, text, QtGui.QMessageBox.NoButton, parent)
   mbox.show()

class TableWidget(QtGui.QTableWidget):
   """sized widget containing a grid of entries"""

   def __init__(self, headings=[], stretch_cols=[], parent=None):

      super(TableWidget, self).__init__(parent)

      ncols = len(headings)

      self.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
      self.setColumnCount(ncols)
      self.setHorizontalHeaderLabels(headings)

      for col in stretch_cols:
         if col < 0 or col >= ncols:
            print ("** TableWidget: stretch_cols outside range of headings" \
                   "   num headings = %d, col = %d" % (ncols, col))
            continue
         self.horizontalHeader().setResizeMode(col, QtGui.QHeaderView.Stretch)

   def populate(self, entries):
      """populate the table with entries
        
            entries     : 2D set of values (any # rows, but cols should match)
      """

# end TableWidget class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class label_opt_exec_widget(object):
   """QWidget containing row of label, item menu, pushbutton

        parent   : parent widget
        label    : left-most label widget
        menulist : list of menu item names
        pbtext   : pushbutton text
        cb       : callback to apply to PB 'clicked()' event, if set
                  (otherwise, callbacks might be applied via blist)
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
                 (otherwise, callbacks might be applied via blist)
        ltype  : layout type: 0=vertical, 1=horizontal"""

   def __init__(self, parent, labels, cb=None, ltype=0):

      self.mainw = QtGui.QWidget(parent)                # main widget
      self.name = 'QWidget with button list'
      self.blist = []                                   # button list

      if ltype == 0: layout = QtGui.QVBoxLayout(self.mainw)
      else:          layout = QtGui.QHBoxLayout(self.mainw)

      for label in labels:
         b = QtGui.QPushButton(self.mainw)
         b.setText(label)
         if cb: b.connect(b, QtCore.SIGNAL("clicked()"), cb)
         self.blist.append(b)
         layout.addWidget(b)

   def get_button_text(self, index=-1, button=None):
      """return text for button
            index  : if apppropriate, return text for this button index
            button : else, locate this button and return text

        if no button is found, return 'NO_SUCH_BUTTON'"""

      if index >= 0 and index < len(self.blist):
         return blist[index].text().toAscii().data()
      elif button != None:
         for b in self.blist:
            if b == button:
               return b.text().toAscii().data()
 
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

      # create layout, for either 1 or 2 sub-windows
      layout = QtGui.QGridLayout()

      if sepwin:
         offset = 8
         self.Edit_err = QtGui.QTextEdit()
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
      self.Bstart.clicked.connect(self.cb_start)
      self.Bstop.clicked.connect(self.cb_stop)

      self.process = QtCore.QProcess()
      self.process.readyReadStandardError.connect(self.readstderr)
      self.process.readyReadStandardOutput.connect(self.readstdout)
      self.process.finished.connect(self.finished)
      self.status  = 0

      self.setWindowTitle('processing command: %s' % command)

      self.command = command
      self.dir = dir

      self.cb_start()

   def finished(self):
      self.readstderr()
      self.readstdout()
      print '-- processed finished, state = %d, status = %d' \
            % (self.process.state(), self.process.exitStatus())

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
         print '** SPW: no command to execute'
         return

      # note directory
      if self.dir != '':
         print '-- setting working dir to %s' % self.dir
         self.process.setWorkingDirectory(self.dir)

      print 'starting command: %s\n' % cmd
      args = ['-c', cmd]

      self.process.start('tcsh', args)
      if self.process.waitForStarted():
         print '++ process is started, pid = %d' % self.process.pid()
         self.status = 0
      else:
         print '** failed to start'
         self.status = -1

   def cb_stop(self):
      print 'stopping command %s ...' % self.command
      self.process.kill()

# end TcshCommandWindow class
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
class ProcessWindow(QtGui.QMainWindow):

   def __init__(self, command='', dir='', title='', parent=None, sepwin=1):
      """Create a window to display the processing of a passed command, or of
         those specified by the user.

         command        - if set, execute the command
         dir            - if set, use as processing directory
         sepwin         - if set, use separate windows for stdout and stderr

      """
      if title == '': title = 'command process'

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
      clabel = QtGui.QLabel('command: ')

      # create layout, for either 1 or 2 sub-windows
      layout = QtGui.QGridLayout()

      if sepwin:
         offset = 8
         self.Edit_err = QtGui.QTextEdit()
         layout.addWidget(self.Edit_out, 0, 0, offset, 5)
         layout.addWidget(self.Edit_err, offset, 0, offset, 5)
         offset += 8
      else:
         offest = 16
         layout.addWidget(self.Edit_out, 0, 0, offest, 5)
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
      self.lineedit.editingFinished.connect(self.cb_newcommand)
      self.Bstart.clicked.connect(self.cb_start)
      self.Bstop.clicked.connect(self.cb_stop)

      self.process = QtCore.QProcess()
      self.process.readyReadStandardError.connect(self.readstderr)
      self.process.readyReadStandardOutput.connect(self.readstdout)
      self.process.finished.connect(self.finished)
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
      print '-- processed finished, state = %d, status = %d' \
            % (self.process.state(), self.process.exitStatus())

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
      self.lineedit.clearFocus()
      pstate = self.process.state()
      print '-- trying command %d' % len(self.commandlist)
      if pstate == QtCore.QProcess.NotRunning:
         return self.cb_start()
      else:
         return
         print '** will not run new command until old one is finished'
         print '== state is %s\n' % pstate

   def cb_start(self, cmd=''):
      if cmd == '' or cmd == False:
         cmd = self.lineedit.text()
         if cmd == '': return
         cmd = str(cmd)

      targs = cmd.split(' ')
      if len(targs) < 1: return

      # note directory
      if self.dir != '':
         print '-- setting working dir to %s' % self.dir
         self.process.setWorkingDirectory(self.dir)

      print 'starting command: %s\n' % str(cmd)
      self.commandlist.append(cmd)
      self.Edit_hist.append(cmd)
      self.lineedit.clear()

      self.process.start(targs[0], targs[1:])
      if self.process.waitForStarted():
         print '++ process is started, pid = %d' % self.process.pid()
         self.status = 0
      else:
         print '** failed to start'
         self.status = -1

   def cb_stop(self):
      print 'stopping'
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
         self.lineedit.editingFinished.connect(self.process_command)

   def process_command(self):
      command = str(self.lineedit.text())
      self.lineedit.clearFocus()
      if self.callback != None: self.callback(command)
      self.editor.append(command)

# end PyCommandWindow class
# ---------------------------------------------------------------------------

if __name__ == '__main__':
   app = QtGui.QApplication(sys.argv)
   win1 = TextWindow()
   win1.show()
   win2 = TextWindow(text = 'hello, this is\nsome text!')
   win2.show()
   app.exec_()
