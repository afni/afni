#!/usr/bin/python

# basically, a GUI to write an afni_proc.py command

import sys, os, copy, math

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['copy', 'signal']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import copy

import afni_util as UTIL
import lib_subjects as SUBJ
import lib_uber_subject as USUBJ
import option_list as OPT

g_command_help = """
===========================================================================
uber_subject.py               - graphical interface to afni_proc.py

This help describes only the command-line options to this program, which
enables use without the GUI (graphical user interface).

- R Reynolds  Feb, 2011
===========================================================================
"""

def get_valid_opts():
   """return an OptionsList of valid program options"""

   # terminal, informative options
   vopts = OPT.OptionList('uber_subject.py options')
   vopts.add_opt('-help', 0, [], helpstr='show this help')
   vopts.add_opt('-help_gui', 0, [], helpstr='show help for GUI')
   vopts.add_opt('-hist', 0, [], helpstr='show revision history')
   vopts.add_opt('-show_valid_opts',0,[],helpstr='show all valid options')
   vopts.add_opt('-ver', 0, [], helpstr='show module version')

   vopts.add_opt('-verb', 1, [], helpstr='set verbose level')

   vopts.add_opt('-no_gui', 0, [], helpstr='do not open graphical interface')
   vopts.add_opt('-print_ap_command',0,[],helpstr='show afni_proc.py script')
   vopts.add_opt('-svar', -2, [], helpstr='set subject variable to value')

   vopts.trailers = 0   # do not allow unknown options

   return vopts

def process_options(valid_opts, argv):
   """return status and a VarsObject struct of subject variables

        - given list of valid options, read and process the user options
        - if terminal option or -no_gui, return 0 (succesful quit)

      return  1 : on success and terminate
              0 : on success and continue with GUI
             -1 : on error condition
   """

   # a quick out
   if len(argv) == 0: return 0, None

   # process any optlist_ options
   valid_opts.check_special_opts(argv)

   # ------------------------------------------------------------
   # check for terminal options before processing the rest
   if '-help' in sys.argv:
      print g_command_help
      return 1, None

   if '-help_gui' in sys.argv:
      print USUBJ.helpstr_usubj_gui
      return 1, None

   if '-hist' in sys.argv:
      print USUBJ.g_history
      return 1, None

   if '-show_valid_opts' in sys.argv:
      valid_opts.show('', 1)
      return 1, None

   if '-ver' in sys.argv:
      print 'uber_subject.py: version %s' % USUBJ.g_version
      return 1, None

   # ------------------------------------------------------------
   # read and process user options (no check for terminal opts)
   uopts = OPT.read_options(argv, valid_opts)
   if not uopts: return -1, None

   # init subject options struct
   svars = SUBJ.VarsObject('subject vars from command line')
   defs  = USUBJ.g_subj_defs

   # first set verbose level
   val, err = uopts.get_type_opt(int, '-verb')
   if val != None and not err: verb = val
   else: verb = 1

   use_gui = 1 # assume GUI unless we hear otherwise

   # first process all setup options
   errs = 0
   for opt in uopts.olist:
      # skip -verb and any terminal option (though they should not be here)
      if opt.name == '-help':              continue
      elif opt.name == '-help_gui':        continue
      elif opt.name == '-hist':            continue
      elif opt.name == '-show_valid_opts': continue
      elif opt.name == '-ver':             continue

      elif opt.name == '-verb':            continue

      # and skip any post-setup options ...
      elif opt.name == '-print_ap_command':continue

      # now go after "normal" options

      if opt.name == '-no_gui':
         use_gui = 0
         continue

      # svar requires at least 2 parameters, name and value
      elif opt.name == '-svar':
         val, err = uopts.get_string_list('', opt=opt)
         if val != None and err: return -1, None
         # and set it from the form name = [value_list]
         if set_svar_from_def(val[0], val[1:], svars, defs, verb=verb):
            errs += 1
            continue

   if not errs:         # then we can handle any processing options
      if uopts.find_opt('-print_ap_command'):
         print_ap_command(svars)

   if errs:    return -1, None
   if use_gui: return  0, svars
   else:       return  1, svars

def set_svar_from_def(name, vlist, svars, defs, verb=1):
   """try to set name = value based on vlist
      if name is not known by the defaults, return failure

      return 0 on success, else 1
   """

   if not defs.valid(name):
      print '** invalid subject variable: %s' % name
      return 1

   dtype = type(defs.val(name))
   if dtype not in SUBJ.g_valid_atomic_types:
      print '** unknown subject variable type for %s' % name
      return 1

   # if simple type but have list, fail
   if dtype != list and len(vlist) > 1:
      print "** have list for simple type, name='%s', dtype=%s, list=%s" \
            % (name, dtype, vlist)
      return 1

   # ----------------------------------------
   # try to apply the value (list)

   val = None

   # process only simple int, float, str and strlist
   if dtype == int:
      try: val = int(vlist[0])
      except:
         print "** failed to set svar %s to int value from '%s'"%(name,vlist[0])
         return 1
   elif dtype == float:
      try: val = float(vlist[0])
      except:
         print "** failed to set svar %s to float value from '%s'" \
               % (name, vlist[0])
         return 1
   elif dtype == str: # easy case
      val = vlist[0]
   elif dtype == list: # another easy case
      val = vlist
   else:
      print '** set_svar_from_def: unprocessed type %s for %s' % (dtype, name)
      return 1

   # actually set the value
   rv = svars.set_var(name, val)
   if verb > 1:
      if rv: print '++ svar: updating %s to %s' % (name, val)
      else:  print '++ svar: no update for %s to %s' % (name, val)

   # if no update, we're outta here
   if rv == 0: return 0

   # ----------------------------------------------------------------------
   # handle some special cases, such as indices and labels, which might
   # come with file name lists

   USUBJ.update_svars_from_special(name, svars, check_sort=1)

   return 0

def print_ap_command(svars):

   # create command, save it (init directory tree?), show it
   # status, wstr, mesg = USUBJ.ap_command_from_svars(svars)

   # create the subject, check warnings and either a command or errors
   apsubj = USUBJ.AP_Subject(svars)
   nwarn, wstr = apsubj.get_ap_warnings()
   status, mesg = apsubj.get_ap_command()


   if status:  # then only mention errors
      print '%s\nERRORS:\n\n%s\n' % (75*'*', mesg)
   else:
      if wstr: print '%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-', wstr, 75*'-')
      print '### afni_proc.py script:\n\n%s\n' % mesg

def run_gui(svars=None):
   try: from PyQt4 import QtGui
   except:
      print '** failed to import PyQt4.QtGui **' \
            '   (PyQt4 must be installed to run the uber_subject.py GUI)'
      return 1

   # if the above worked, let any GUI import errors show normally
   import gui_uber_subj as GUS

   app = QtGui.QApplication(sys.argv)
   dialog = GUS.SingleSubjectWindow(subj_vars=svars)
   QtGui.QApplication.setStyle(QtGui.QStyleFactory.create(dialog.gvars.style))
   dialog.show()
   app.exec_()

   return 0

def main():

   # - set any subject variables or options
   # - run the gui unless -no_gui is given

   valid_opts = get_valid_opts()
   rv, svars  = process_options(valid_opts, sys.argv)

   if   rv > 0: return 0        # terminal success
   elif rv < 0: return 1        # terminal failure
   # else rv == 0, so continue with GUI

   return run_gui(svars)

if __name__ == '__main__':
   sys.exit(main())
