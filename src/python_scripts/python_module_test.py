#!/usr/bin/env python

# ---------------------------------------------------------------------------
# test python modules

import sys
import module_test_lib

g_help_string = """
===========================================================================
python_module_test.py   - test the loading of python modules

   The default behavior of this program is to verify whether a 'standard'
   list of python modules can be loaded.  The 'standard' list amounds to
   what is needed for the python programs in AFNI.

   The user may specify a list of python modules to test.

------------------------------------------------------------
examples:

   a. Use the default behavior to test modules in standard list.

      python_module_test.py

   b. Test a specific list of modules in verbose mode.

      python_module_test.py -test_modules sys os numpy scipy R wx -verb 2

   c. Show the python version and platform information.

      python_module_test.py -python_ver -platform_info

   d. Perform a complete test (applies commands a and c).

      python_module_test.py -full_test

------------------------------------------------------------
informational options:

   -help                        : display this help
   -hist                        : display the modification history
   -show_valid_opts             : display all valid options (short format)
   -ver                         : display the version number

----------------------------------------
other options:

   -full_test                   : perform all of the standard tests

      This option applies -platform_info, -python_ver and -test_defaults.

   -platform_info               : display system information

      Platform information can include the OS and version, along with the
      CPU type.

   -python_ver                  : display the version of python in use

      Show which version of python is being used by the software.

   -test_defaults               : test the default module list

      The default module list will include (hopefully) all python modules
      used by AFNI programs.

      Note that most programs will not need all of these python libraries.

    -test_modules MOD1 MOD2 ... : test the specified module list

      Perform the same test, but on the modules specified with this option.

    -verb LEVEL                 : specify a verbose level

----------------------------------------
R Reynolds  30 Oct 2008
===========================================================================
"""

g_history = """
    python_module_test.py history:

    0.1  Oct 30, 2008: submitted
    0.2  Nov 06, 2008: added -full_test
    0.3  Nov 21, 2008:
         - removed R from basic test list
         - in base usage, set verb level to 2
"""

g_version = "version 0.3, November 21, 2008"


# main module for defining and processing use options
class ModuleTest:
   # local so not in basic usage stream
   def __init__(self):
      self.valid_opts     = None
      self.user_opts      = None
      self.verb           = 1

      self.modlist        = []
      self.show_modtest   = 0
      self.show_platform  = 0
      self.show_pyver     = 0

      self.OL             = None        # store option_list module locally

   def init_opts(self):
      import option_list
      self.OL = option_list

      self.valid_opts = self.OL.OptionList('valid options')

      # terminal arguments
      self.valid_opts.add_opt('-help', 0, [],                           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],                           \
                      helpstr='display program history')
      self.valid_opts.add_opt('-ver', 0, [],                            \
                      helpstr='display program version')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],                \
                      helpstr='display valid program options')

      # options
      self.valid_opts.add_opt('-full_test', 0, [],                      \
                      helpstr='test default modules and get python info')

      self.valid_opts.add_opt('-platform_info', 0, [],                  \
                      helpstr='display platform information')

      self.valid_opts.add_opt('-python_ver', 0, [],                     \
                      helpstr='display python version')

      self.valid_opts.add_opt('-test_defaults', 0, [],                  \
                      helpstr='test default module list')

      self.valid_opts.add_opt('-test_modules', -1, [],                  \
                      helpstr='test listed modules')

      self.valid_opts.add_opt('-verb', 1, [],                           \
                      helpstr='set verbose level')

   def read_opts(self):
      """check for terminal arguments, then read user options"""

      # ------------------------------------------------------------
      # terminal arguments, first

      # cannot have len(argv) <= 1 here, but be consistent with other progs
      if len(sys.argv) <= 1 or '-help' in sys.argv:
         print g_help_string
         return 0

      if '-hist' in sys.argv:
         print g_history
         return 0

      if '-ver' in sys.argv:
         print g_version
         return 0

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 0

      # ------------------------------------------------------------
      # read all user options

      self.user_opts = self.OL.read_options(sys.argv, self.valid_opts)
      if not self.user_opts: return 1         # error condition

      return None     # normal completion

   def process_opts(self):
      """apply each option"""

      # ----------------------------------------
      # set verb first
      self.verb, err = self.user_opts.get_type_opt(int, '-verb')
      if self.verb == None: self.verb = 1
      elif err: return 1

      if self.user_opts.find_opt('-full_test'):
         self.modlist = [] # pass nothing to use defaults
         self.show_modtest = 1
         self.show_platform = 1
         self.show_pyver = 1

      if self.user_opts.find_opt('-test_defaults'):
         self.modlist = [] # pass nothing to use defaults
         self.show_modtest = 1
      else:
         self.modlist, err = self.user_opts.get_string_list('-test_modules')
         if self.modlist: self.show_modtest = 1  # then show it

      if self.user_opts.find_opt('-platform_info'):
         self.show_platform = 1

      if self.user_opts.find_opt('-python_ver'):
         self.show_pyver = 1

      return None     # normal completion

   def execute(self):

      if self.show_pyver:
         if self.verb <= 1:
            vlist = sys.version.split()
            vstr = vlist[0]
         else:
            vstr = sys.version
         print 'python version: %s' % vstr

      if self.show_platform:
         import platform
         print 'platform: %s' % platform.platform()

      if self.show_modtest:
         nfail = module_test_lib.num_import_failures(self.modlist,
                                     details=1,verb=self.verb)
         print "\nnumber of python import failures = %d\n" % nfail

      return None

   def show_failed_command(self):
      import afni_util as UTIL
      UTIL.show_args_as_command(sys.argv,"** failed command:")

def process():
   if len(sys.argv) <= 1:       # default behavior, run general test
      print ""
      nfail = module_test_lib.num_import_failures(verb=2)
      print "\nnumber of python import failures = %d\n" % nfail
      return nfail

   # create elemnnt and initialize options
   MT = ModuleTest()
   MT.init_opts()

   # read options, return on terminal option or bad usage
   rv = MT.read_opts()
   if rv != None:
      if rv: MT.show_failed_command()
      return rv

   # apply options
   rv = MT.process_opts()
   if rv != None:
      if rv: MT.show_failed_command()
      return rv

   # do stuff
   rv = MT.execute()
   if rv != None:
      if rv: MT.show_failed_command()
      return rv

if __name__ == '__main__':
   rv = process()
   sys.exit(rv)
