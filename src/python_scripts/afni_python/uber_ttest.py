#!/usr/bin/env python

# python3 status: compatible

# basically, a GUI to write an afni_proc.py command

import sys, os, copy, math

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['copy', 'signal']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import copy

import afni_util as UTIL
import lib_subjects as SUBJ
import lib_vars_object as VO
import lib_uber_ttest as LTT
import option_list as OPT

g_command_help = """
===========================================================================
uber_ttest.py      - GUI for group ttest

        usage:  uber_ttest.py

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

      uber_ttest.py
      uber_ttest.py -qt_opts -style=motif

   Informational examples:

      uber_ttest.py -help
      uber_ttest.py -help_gui
      uber_ttest.py -hist
      uber_ttest.py -show_valid_opts
      uber_ttest.py -ver

   Non-GUI examples (all have -no_gui):

      uber_ttest.py -no_gui -print_script               \\
         -dsets_A $ddir/OLSQ.*.HEAD

      uber_ttest.py -no_gui -save_script cmd.ttest      \\
        -mask mask+tlrc                                 \\
        -set_name_A vrel                                \\
        -set_name_B arel                                \\
        -dsets_A REML.*.HEAD                            \\
        -dsets_B REML.*.HEAD                            \\
        -beta_A 0                                       \\
        -beta_B 2                                       \\
        -results_dir ''

      Note that the 3dMEMA command should have t-stat indices as well.

      uber_ttest.py -no_gui -save_script cmd.MEMA       \\
        -program 3dMEMA                                 \\
        -mask mask+tlrc                                 \\
        -set_name_A vrel                                \\
        -set_name_B arel                                \\
        -dsets_A REML.*.HEAD                            \\
        -dsets_B REML.*.HEAD                            \\
        -beta_A 0 -tstat_A 1                            \\
        -beta_B 2 -tstat_B 3                            \\
        -results_dir ''

----------------------------------------------------------------------

- R Reynolds  Aug, 2011
===========================================================================
"""

class MainInterface(object):
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
      vopts.add_opt('-hist', 0, [], helpstr='show revision history')
      vopts.add_opt('-show_default_vars',0,[],helpstr='show variable defaults')
      vopts.add_opt('-show_valid_opts',0,[],helpstr='show all valid options')
      vopts.add_opt('-show_cvar_dict',0,[],helpstr='show control var dict')
      vopts.add_opt('-show_uvar_dict',0,[],helpstr='show user var dictionary')
      vopts.add_opt('-ver', 0, [], helpstr='show module version')

      vopts.add_opt('-verb', 1, [], helpstr='set verbose level')

      vopts.add_opt('-no_gui', 0, [], helpstr='do not open graphical interface')
      vopts.add_opt('-qt_opts',-1, [],helpstr='pass the given options to PyQt')

      vopts.add_opt('-print_script', 0, [], helpstr='print align test script')
      vopts.add_opt('-save_script', 1, [], helpstr='save align test script')
      vopts.add_opt('-cvar', -2, [], helpstr='set control variable')
      vopts.add_opt('-uvar', -2, [], helpstr='set user variable to value')

      vopts.trailers = 0   # do not allow unknown options

      # add user and control vars directly
      for dict in [LTT.g_cvar_dict, LTT.g_uvar_dict]:
         keys = list(dict.keys())
         keys.sort()
         for name in keys:
            if name == 'verb': continue # already included
            vopts.add_opt('-'+name, -1, [], helpstr=dict[name])

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
         print(g_command_help)
         return 1

      if '-help_gui' in argv:
         print(LTT.helpstr_gui)
         return 1

      if '-help_todo' in argv:
         print(LTT.helpstr_todo)
         return 1

      if '-hist' in argv:
         print(LTT.g_history)
         return 1

      if '-show_default_vars' in argv:
         LTT.g_user_defs.show('default uvars :')
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-show_cvar_dict' in sys.argv:
         dict = LTT.g_cvar_dict
         keys = list(dict.keys())
         keys.sort()
         for key in keys:
            print('   %-20s : %s' % (key, dict[key]))
         return 1

      if '-show_uvar_dict' in sys.argv:
         dict = LTT.g_uvar_dict
         keys = list(dict.keys())
         keys.sort()
         for key in keys:
            print('   %-20s : %s' % (key, dict[key]))
         return 1

      if '-ver' in argv:
         print('uber_ttest.py: version %s' % LTT.g_version)
         return 1

      # ------------------------------------------------------------
      # read and process user options (no check for terminal opts)
      self.uopts = OPT.read_options(argv, self.valid_opts)
      if not self.uopts: return -1
      uopts = self.uopts # convenience

      # init subject options struct
      self.cvars = VO.VarsObject('control vars from command line')
      self.uvars = VO.VarsObject('user vars from command line')
      self.guiopts = ['uber_ttest.py']

      # first set verbose level
      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val
      else: self.verb = 1

      SUBJ.set_var_str_from_def('cvars', 'verb', ['%d'%self.verb], self.cvars,
                                 defs=LTT.g_ctrl_defs)

      use_gui = 1 # assume GUI unless we hear otherwise
      cvar_keys = list(LTT.g_cvar_dict.keys())
      uvar_keys = list(LTT.g_uvar_dict.keys())

      # we already processed terminal options
      term_opts = ['-help', '-help_gui', '-help_todo',
                   '-hist', 'show_default_vars', '-ver',
                   '-show_cvar_dict', '-show_uvar_dict']

      # skip post-setup options, to be checked later
      post_opts = ['-print_script', '-save_script']

      # first process all setup options
      errs = 0
      for opt in uopts.olist:

         # skip -verb (any terminal option should block getting here)
         if opt.name in term_opts: continue

         # and skip and post-setup options (print command, save, etc.)
         if opt.name in post_opts: continue

         vname = opt.name[1:]

         # now go after "normal" options

         if opt.name == '-no_gui':
            use_gui = 0
            continue

         # get any PyQt4 options
         elif opt.name == '-qt_opts':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err:
               errs += 1
               continue
            self.guiopts.extend(val)

         # cvar requires at least 2 parameters, name and value
         elif opt.name == '-cvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err:
               errs += 1
               continue
            if self.cvars.set_var_with_defs(val[0], val[1:], LTT.g_ctrl_defs,
                        oname='cvars', verb=self.verb) < 0:
               errs += 1
               continue

         # uvar requires at least 2 parameters, name and value
         elif opt.name == '-uvar':
            val, err = uopts.get_string_list('', opt=opt)
            if val != None and err:
               errs += 1
               continue
            if self.uvars.set_var_with_defs(val[0], val[1:], LTT.g_user_defs,
                                oname='uvars', verb=self.verb) < 0:
               errs += 1
               continue

         # maybe this is a control variable key
         elif vname in cvar_keys:
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err:
               errs += 1
               continue
            if self.cvars.set_var_with_defs(vname, val, LTT.g_ctrl_defs,
                        oname='cvars', verb=self.verb) < 0:
               errs += 1
               continue

         # maybe this is a user variable key
         elif vname in uvar_keys:
            val, err = uopts.get_string_list('', opt=opt)
            if val == None or err:
               errs += 1
               continue
            if self.uvars.set_var_with_defs(vname, val, LTT.g_user_defs,
                        oname='uvars', verb=self.verb) < 0:
               errs += 1
               continue

         else:
            print('** invalid option: %s' % opt.name)
            errs += 1
            continue

      if not errs:         # then we can handle any processing options
         if uopts.find_opt('-print_script'):
            self.print_script()
            use_gui = 0

         #opt = uopts.find_opt('-save_script')
         #if opt != None:
         #   val, err = uopts.get_string_opt('', opt=opt)
         val, err = uopts.get_string_opt('-save_script')
         if val != None and not err:
            self.save_script(val)
            use_gui = 0

      if errs:    return -1
      if use_gui: return  0     # continue and open GUI
      else:       return  1     # no error, but terminate on return

   def print_script(self):
      """create alignment script and print to terminal"""

      atest, cmd = self.get_script()
      print(cmd)

   def save_script(self, fname):
      atest, cmd = self.get_script()
      if cmd == '': return

      if atest.write_script(fname):
         print('** failed to write afni_proc.py command to disk')

   def get_script(self):
      """return the TTest object and script
         (print warnings and errors to screen)"""

      atest = LTT.TTest(self.cvars, self.uvars)

      nwarn, wstr = atest.get_warnings()
      status, mesg = atest.get_script()

      if status:        # only show errors
         print('%s\nERRORS:\n\n%s\n' % (75*'*', mesg))
         cmd = ''
      else:
         if wstr: print('%s\n**** Warnings:\n\n%s\n%s\n' % (75*'-',wstr,75*'-'))
         cmd = '### alignment test script:\n\n%s\n' % mesg

      return atest, cmd


   def run_gui(self):
      try: from PyQt4 import QtGui
      except:
         print('\n**** failed to import PyQt4.QtGui ****\n\n'                \
               '   PyQt4 must be installed to run the uber_subject.py GUI\n' \
               '   --> see the output of: uber_subject.py -help_install\n')
         return 1

      # if the above worked, let any GUI import errors show normally
      import gui_uber_ttest as GUT

      app = QtGui.QApplication(self.guiopts)
      D = GUT.MainWindow(cvars=self.cvars, uvars=self.uvars, set_pdir=1)
      QtGui.QApplication.setStyle(QtGui.QStyleFactory.create("cleanlooks"))
      D.show()
      app.exec_()

      return 0

def main():

   mi = MainInterface()
   if not mi: return 1

   rv = mi.process_options()
   if   rv > 0: return 0        # terminal success
   elif rv < 0: return 1        # terminal failure
   # else rv == 0, so continue with GUI

   return mi.run_gui()

if __name__ == '__main__':
   sys.exit(main())
