#!/usr/bin/env python

# basically, a GUI to write an afni_proc.py command

import sys, os, copy, math

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['copy', 'signal']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import copy

import afni_util as UTIL
import lib_vars_object as VO
import lib_uber_align as UALIGN
import option_list as OPT

g_command_help = """
===========================================================================
uber_align_test.py      - generate script to test anat/EPI alignment

        usage:  uber_align_test.py

---

This help describes only the command line options to this program, which
enables one to:

        - initialize user variables (for GUI or command line)
        - initialize control variables (for GUI or command line)
        - pass PyQt4 options directly to the GUI
        - run without the GUI

----------------------------------------------------------------------
Examples:

   GUI examples:

      uber_align_test.py
      uber_align_test.py -qt_opts -style=motif

   Informational examples:

      uber_align_test.py -help
      uber_align_test.py -help_gui
      uber_align_test.py -hist
      uber_align_test.py -show_valid_opts
      uber_align_test.py -ver

   Non-GUI examples (all have -no_gui):

      uber_align_test.py -no_gui -print_script            \\
         -uvar anat FT/FT_anat+orig                       \\
         -uvar epi  FT/FT_epi_r1+orig

      uber_align_test.py -no_gui -save_script align.test  \\
         -uvar anat FT/FT_anat+orig                       \\
         -uvar epi  FT/FT_epi_r1+orig                     \\
         -uvar epi_base 2                                 \\
         -uvar epi_strip_meth 3dAutomask                  \\
         -uvar align_centers yes                          \\
         -uvar giant_move yes                             \\
         -uvar cost ls                                    \\
         -uvar multi_list lpc lpc+ lpc+ZZ lpa

----------------------------------------------------------------------

- R Reynolds  Apr, 2011
===========================================================================
"""

class AlignInterface(object):
   def __init__(self):

      self.verb       = 1
      self.valid_opts = self.get_valid_opts()


   def get_valid_opts(self):
      """return an OptionsList of valid program options"""

      # terminal, informative options
      vopts = OPT.OptionList('uber_subject.py options')
      vopts.add_opt('-help', 0, [], helpstr='show this help')
      vopts.add_opt('-help_gui', 0, [], helpstr='show help for GUI')
      vopts.add_opt('-help_todo', 0, [], helpstr='show todo list')
      vopts.add_opt('-help_howto_program',0,[],helpstr='programming overview')
      vopts.add_opt('-hist', 0, [], helpstr='show revision history')
      vopts.add_opt('-show_default_vars',0,[],helpstr='show variable defaults')
      vopts.add_opt('-show_valid_opts',0,[],helpstr='show all valid options')
      vopts.add_opt('-ver', 0, [], helpstr='show module version')

      vopts.add_opt('-verb', 1, [], helpstr='set verbose level')

      vopts.add_opt('-no_gui', 0, [], helpstr='do not open graphical interface')
      vopts.add_opt('-qt_opts',-1, [],helpstr='pass the given options to PyQt')

      vopts.add_opt('-print_script', 0, [], helpstr='print align test script')
      vopts.add_opt('-save_script', 1, [], helpstr='save align test script')
      vopts.add_opt('-cvar', -2, [], helpstr='set control variable')
      vopts.add_opt('-uvar', -2, [], helpstr='set user variable to value')

      vopts.trailers = 0   # do not allow unknown options

      return vopts

   def process_options(self):
      """return  1 on valid and exit
                 0 on valid and continue
                -1 on invalid
      """

      argv = sys.argv

      # a quick out (no help, continue and open GUI)
      if len(argv) == 0: return 0

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # ------------------------------------------------------------
      # check for terminal options before processing the rest
      if '-help' in argv:
         print g_command_help
         return 1

      if '-help_gui' in argv:
         print UALIGN.helpstr_gui
         return 1

      if '-help_howto_program' in argv:
         print UALIGN.helpstr_create_program
         return 1

      if '-help_todo' in argv:
         print UALIGN.helpstr_todo
         return 1

      if '-hist' in argv:
         print UALIGN.g_history
         return 1

      if '-show_default_vars' in argv:
         UALIGN.g_user_defs.show('default uvars :')
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         print 'uber_align_test.py: version %s' % UALIGN.g_version
         return 1

      # ------------------------------------------------------------
      # read and process user options (no check for terminal opts)
      self.uopts = OPT.read_options(argv, self.valid_opts)
      if not self.uopts: return -1
      uopts = self.uopts # convenience

      # init subject options struct
      self.cvars = VO.VarsObject('control vars from command line')
      self.uvars = VO.VarsObject('user vars from command line')
      self.guiopts = ['uber_align_test.py']

      # first set verbose level
      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val
      else: self.verb = 1

      # changed from SUBJ.set_var_str_from_def
      self.uvars.set_var_with_defs('verb', ['%d'%self.verb], UALIGN.g_user_defs,
                                   oname='uvars')

      use_gui = 1 # assume GUI unless we hear otherwise

      # first process all setup options
      errs = 0
      for opt in uopts.olist:
         # skip -verb (any terminal option should block getting here)
         if opt.name == '-verb':                continue

         # and skip and post-setup options (print command, save, etc.)
         elif opt.name == '-print_script':      continue
         elif opt.name == '-save_script':       continue

         # now go after "normal" options

         if opt.name == '-no_gui':
            use_gui = 0
            continue

         # get any PyQt4 options
         elif opt.name == '-qt_opts':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err: return -1
            self.guiopts.extend(val)

         # cvar requires at least 2 parameters, name and value
         elif opt.name == '-cvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err: return -1
            # and set it from the form name = [value_list]
            if self.cvars.set_var_with_defs(val[0], val[1:], UALIGN.g_ctrl_defs,
                                            oname='cvars', verb=self.verb) < 0:
               errs += 1
               continue

         # uvar requires at least 2 parameters, name and value
         elif opt.name == '-uvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err: return -1
            # and set it from the form name = [value_list]
            if self.uvars.set_var_with_defs(val[0], val[1:], UALIGN.g_user_defs,
                                            oname='uvars', verb=self.verb) < 0:
               errs += 1
               continue

      if not errs:         # then we can handle any processing options
         if uopts.find_opt('-print_script'): self.print_script()

      if not errs:         # then we can handle any processing options
         opt = uopts.find_opt('-save_script')
         if opt != None:
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and not err: self.save_script(val)

      if errs:    return -1
      if use_gui: return  0     # continue and open GUI
      else:       return  1     # no error, but terminate on return

   def print_script(self):
      """create alignment script and print to terminal"""

      atest, cmd = self.get_script()
      print cmd

   def save_script(self, fname):
      atest, cmd = self.get_script()
      if cmd == '': return

      if atest.write_script(fname):
         print '** failed to write afni_proc.py command to disk'

   def get_script(self):
      """return the AlignTest object and script
         (print warnings and errors to screen)"""

      atest = UALIGN.AlignTest(self.cvars, self.uvars)

      nwarn, wstr = atest.get_warnings()
      status, mesg = atest.get_script()

      if status:        # only show errors
         print '%s\nERRORS:\n\n%s\n' % (75*'*', mesg)
         cmd = ''
      else:
         if wstr: print '%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-',wstr,75*'-')
         cmd = '### alignment test script:\n\n%s\n' % mesg

      return atest, cmd


   def run_gui(self):
      try: from PyQt4 import QtGui
      except:
         print '\n**** failed to import PyQt4.QtGui ****\n\n'                \
               '   PyQt4 must be installed to run the uber_subject.py GUI\n' \
               '   --> see the output of: uber_subject.py -help_install\n'
         return 1

      # if the above worked, let any GUI import errors show normally
      import gui_uber_align_test as GUT

      app = QtGui.QApplication(self.guiopts)
      D = GUT.MainWindow(cvars=self.cvars, uvars=self.uvars, set_pdir=1)
      QtGui.QApplication.setStyle(QtGui.QStyleFactory.create("cleanlooks"))
      D.show()
      app.exec_()

      return 0

def main():

   aint = AlignInterface()
   if not aint: return 1

   rv = aint.process_options()
   if   rv > 0: return 0        # terminal success
   elif rv < 0: return 1        # terminal failure
   # else rv == 0, so continue with GUI

   return aint.run_gui()

if __name__ == '__main__':
   sys.exit(main())
