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

The expected use of this program is to simply run it without any options.
That will start the graphical user interface (GUI), which has its own set
of help and help tools.

        usage:  uber_subject.py

---

This help describes only the command line options to this program, which
enables one to:

        - run without the GUI
        - initialize subject variables in the GUI
        - initialize control variables for control of execution
        - pass PyQt4 options directly to the GUI

----------------------------------------------------------------------
Examples:

   GUI examples:

      uber_subject.py
      uber_subject.py -qt_opts -style=motif
      uber_subject.py -svar sid FT -svar gid idiots  \\
                      -svar anat FT_anat+orig.HEAD   \\
                      -svar epi FT_epi_r*.HEAD       \\
                      -svar stim AV*.txt             \\
                      -svar stim_basis 'BLOCK(15,1)'
      uber_subject.py -cvar subj_dir my/subject/dir

   Informational examples:

      uber_subject.py -help
      uber_subject.py -help_gui
      uber_subject.py -hist
      uber_subject.py -show_valid_opts

   Non-GUI examples (all have -no_gui):

      uber_subject.py -no_gui -print_ap_command                   \\
          -svar sid FT -svar gid idiots                           \\
          -svar anat FT_anat+orig.HEAD -svar epi FT_epi_r*.HEAD   \\
          -svar stim AV*.txt -svar stim_basis 'BLOCK(15,1)'       \\
          -cvar subj_dir my/subject/dir -exec_ap_command
----------------------------------------------------------------------

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
   vopts.add_opt('-qt_opts', -1, [], helpstr='pass the given options to PyQt')
   vopts.add_opt('-print_ap_command',0,[],helpstr='show afni_proc.py script')
   vopts.add_opt('-exec_ap_command',0,[],helpstr='run afni_proc.py command')
   vopts.add_opt('-exec_proc_script',0,[],helpstr='run proc script')
   vopts.add_opt('-cvar', -2, [], helpstr='set control variable to value')
   vopts.add_opt('-svar', -2, [], helpstr='set subject variable to value')

   vopts.trailers = 0   # do not allow unknown options

   return vopts

def process_options(valid_opts, argv):
   """return status and VarsObject structs of subject and control variables

        - given list of valid options, read and process the user options
        - if terminal option or -no_gui, return 0 (succesful quit)

      return  1 : on success and terminate
              0 : on success and continue with GUI
             -1 : on error condition
   """

   # a quick out
   if len(argv) == 0: return 0, None, None, None

   # process any optlist_ options
   valid_opts.check_special_opts(argv)

   # ------------------------------------------------------------
   # check for terminal options before processing the rest
   if '-help' in sys.argv:
      print g_command_help
      return 1, None, None, None

   if '-help_gui' in sys.argv:
      print USUBJ.helpstr_usubj_gui
      return 1, None, None, None

   if '-hist' in sys.argv:
      print USUBJ.g_history
      return 1, None, None, None

   if '-show_valid_opts' in sys.argv:
      valid_opts.show('', 1)
      return 1, None, None, None

   if '-ver' in sys.argv:
      print 'uber_subject.py: version %s' % USUBJ.g_version
      return 1, None, None, None

   # ------------------------------------------------------------
   # read and process user options (no check for terminal opts)
   uopts = OPT.read_options(argv, valid_opts)
   if not uopts: return -1, None, None, None

   # init subject options struct
   svars = SUBJ.VarsObject('subject vars from command line')
   cvars = SUBJ.VarsObject('control vars from command line')
   guiopts = ['uber_subject.py'] 

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
      elif opt.name == '-exec_ap_command': continue
      elif opt.name == '-exec_proc_script':continue

      # now go after "normal" options

      if opt.name == '-no_gui':
         use_gui = 0
         continue

      # get any PyQt4 options
      elif opt.name == '-qt_opts':
         val, err = uopts.get_string_list('', opt=opt)
         if val != None and err: return -1, None, None, None
         guiopts.extend(val)

      # cvar requires at least 2 parameters, name and value
      elif opt.name == '-cvar':
         val, err = uopts.get_string_list('', opt=opt)
         if val != None and err: return -1, None, None, None
         # and set it from the form name = [value_list]
         if set_var_from_def('cvars', val[0], val[1:], cvars, verb=verb):
            errs += 1
            continue

      # svar requires at least 2 parameters, name and value
      elif opt.name == '-svar':
         val, err = uopts.get_string_list('', opt=opt)
         if val != None and err: return -1, None, None, None
         # and set it from the form name = [value_list]
         if set_var_from_def('svars', val[0], val[1:], svars, verb=verb):
            errs += 1
            continue

   if not errs:         # then we can handle any processing options
      if uopts.find_opt('-print_ap_command'):
         print_ap_command(svars, cvars)

   if not errs:         # then we can handle any processing options
      if uopts.find_opt('-exec_ap_command'):
         subj = run_ap_command(svars, cvars)
         if subj != None and uopts.find_opt('-exec_proc_script'):
            subj.exec_proc_script()

   if errs:    return -1, None, None, None
   if use_gui: return  0, svars, cvars, guiopts
   else:       return  1, svars, cvars, guiopts

def set_var_from_def(obj, name, vlist, vars, verb=1):
   """try to set name = value based on vlist
      if name is not known by the defaults, return failure

      return 0 on success, else 1
   """

   if   obj == 'svars': defs = USUBJ.g_subj_defs
   elif obj == 'cvars': defs = USUBJ.g_ctrl_defs
   else:
      print '** set_var_from_def: invalid obj name: %s' % obj
      return 1

   if not defs.valid(name):
      print '** invalid %s variable: %s' % (obj, name)
      return 1

   dtype = type(defs.val(name))
   if dtype not in SUBJ.g_valid_atomic_types:
      print '** unknown %s variable type for %s' % (obj, name)
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
         print "** failed to set %s %s to int value from '%s'" \
               % (obj, name,vlist[0])
         return 1
   elif dtype == float:
      try: val = float(vlist[0])
      except:
         print "** failed to set %s %s to float value from '%s'" \
               % (obj, name, vlist[0])
         return 1
   elif dtype == str: # easy case
      val = vlist[0]
   elif dtype == list: # another easy case
      val = vlist
   else:
      print '** set_var_from_def: unprocessed type %s for %s' % (dtype, name)
      return 1

   # actually set the value
   rv = vars.set_var(name, val)
   if verb > 1:
      if rv: print '++ %s: updating %s to %s %s' % (obj, name, val, type(val))
      else:  print '++ %s: no update for %s to %s' % (obj, name, val)

   # if no update, we're outta here
   if rv == 0: return 0

   # ----------------------------------------------------------------------
   # handle some special cases, such as indices and labels, which might
   # come with file name lists

   USUBJ.update_vars_from_special(obj, name, vars, check_sort=1)

   return 0

def run_ap_command(svars, cvars):
   """create and run the afni_proc.py command
      return the subject object, or None on failure"""

   # create the subject, check warnings and either a command or errors
   subj = USUBJ.AP_Subject(svars=svars, cvars=cvars)
   nwarn, wstr = subj.get_ap_warnings()
   status, mesg = subj.get_ap_command()

   if status:  # then only mention errors
      print '%s\nERRORS:\n\n%s\n' % (75*'*', mesg)
      return None

   # first show any warnings
   if wstr: print '%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-',wstr,75*'-')

   # write command to file
   if subj.write_ap_command():
      print '** failed to write afni_proc.py command to disk'
      return None

   # in any case, execute the command, showing the output
   status, mesg = subj.exec_ap_command()

   if status:
      print '%s\nAP Errors:\n\n%s\n' % (75*'*', mesg)
      return None
   else:
      print '%s\noutput from afni_proc.py:\n\n%s\n' % (75*'-', mesg)
      return subj

   return subj

def print_ap_command(svars, cvars):
   """run and optionally display the afni_proc.py command"""

   # create the subject, check warnings and either a command or errors
   apsubj = USUBJ.AP_Subject(svars=svars, cvars=cvars)

   nwarn, wstr = apsubj.get_ap_warnings()
   status, mesg = apsubj.get_ap_command()

   if status:  # then only mention errors
      print '%s\nERRORS:\n\n%s\n' % (75*'*', mesg)
   else:
      if wstr: print '%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-',wstr,75*'-')
      print '### afni_proc.py script:\n\n%s\n' % mesg

g_install_str = """
   ------------------------------------------------------------------
   A. Linux install:

      1. yum install PyQt4

   B. OS X 10.6 install (from nokia and riverbank computing):

      0. XCode and python (2.6?) should already be installed
      1. Qt SDK for mac:
         - http://qt.nokia.com/downloads
      2. SIP (interface between C++ and python)
         - http://www.riverbankcomputing.co.uk/software/sip/download
         - cd sip-4.12.1    (for example)
         - python configure.py -d /Library/Python/2.6/site-packages
         - make
         - sudo make install
      3. PyQt:
         - http://www.riverbankcomputing.co.uk/software/pyqt/download
   ------------------------------------------------------------------
"""

def run_gui(svars=None, cvars=None, guiopts=[]):
   try: from PyQt4 import QtGui
   except:
      print '\n**** failed to import PyQt4.QtGui ****\n\n' \
            '   (PyQt4 must be installed to run the uber_subject.py GUI)\n'
      print g_install_str
      
      return 1

   # if the above worked, let any GUI import errors show normally
   import gui_uber_subj as GUS

   app = QtGui.QApplication(guiopts)
   dialog = GUS.SingleSubjectWindow(subj_vars=svars, ctrl_vars=cvars,
                                    set_sdir=1)
   QtGui.QApplication.setStyle(QtGui.QStyleFactory.create(dialog.gvars.style))
   dialog.show()
   app.exec_()

   return 0

def main():

   # - set any subject variables or options
   # - run the gui unless -no_gui is given

   valid_opts = get_valid_opts()
   rv, svars, cvars, guiopts  = process_options(valid_opts, sys.argv)

   if   rv > 0: return 0        # terminal success
   elif rv < 0: return 1        # terminal failure
   # else rv == 0, so continue with GUI

   return run_gui(svars, cvars, guiopts)

if __name__ == '__main__':
   sys.exit(main())
