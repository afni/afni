#!/usr/bin/env python

import sys, os

import lib_uber_proc as UPROC

def run_gui():
   try: from PyQt4 import QtCore, QtGui
   except:
      print('\n**** failed to import PyQt4.QtGui ****\n\n'                \
            '   PyQt4 must be installed to run the uber_proc.py GUI\n' \
            '   --> see the output of: uber_subject.py -help_install\n')
      return 1

   import gui_uber_proc as GUP

   app = QtGui.QApplication(sys.argv)
   QtGui.QApplication.setStyle(QtGui.QStyleFactory.create("Cleanlooks"))
   form = GUP.MainWindow()
   form.show()
   app.exec_()

   return 0

def main():
   if '-help' in sys.argv:
      print("%s" % UPROC.g_help_string)
      return 0
   if '-hist' in sys.argv:
      print("%s" % UPROC.g_history)
      return 0
   return run_gui()

if __name__ == '__main__':
   sys.exit(main())
