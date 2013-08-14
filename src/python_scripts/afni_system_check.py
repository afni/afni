#!/usr/bin/env python

# system libraries
import sys, os

# AFNI libraries
import option_list as OL
import afni_util as UTIL        # not actually used, but probably will be
import lib_system_check as SC


g_help_string = """
=============================================================================
afni_system_check.py    - perform various system checks

This program is intended to be helpful for figuring out AFNI installation
issues.  This is just a start.

current usage (run without options):

        afni_system_check.py

-----------------------------------------------------------------------------
details displayed (just run to see):

   general information:
      - CPU, operating system and version, # CPUs, login shell

   AFNI and related tests:
      - which afni, python, R and tcsh, along with versions
      - check for multiple afni packages in PATH
      - check that various AFNI programs run

   python libs:
      - check that various python libraries are found and loaded

   path vars:
      - show some environment variables related to the PATH

-----------------------------------------------------------------------------
options:

   -help                : show this help
   -hist                : show program history
   -show_valid_opts     : show valid options for program
   -todo                : show current todo list
   -ver                 : show current version


-----------------------------------------------------------------------------
R Reynolds    July, 2013
=============================================================================
"""

g_todo = """
todo: afni_system_check.py

   - add some help
   - for Unix or OSX, run ldd or 'otool -L' to check for shared libs in:
      - afni, suma, 3dSkullStrip, 
      - NOTE: 'otool -L' does not show whether libraries resolve
   - check for gcc for mac?  can we tell whether openMP is supported?

   - check for .afnirc/.sumarc .afni/help
   - check for data under any passed -data_root
   - check disk space
"""

g_history = """
   afni_system_check.py history:

   0.0  Jul 11, 2013    - initial version
   0.1  Jul 16, 2013    - added a couple of checks for early python versions
   0.2  Aug 02, 2013    - check for multiple R and python programs in PATH
   0.3  Aug 14, 2013
                - PYTHON_PATH should be PYTHONPATH
                - added a look at any /sw/bin/python* programs
"""

g_version = "afni_system_check.py version 0.3, August 14, 2013"


class CmdInterface:
   """interface class"""

   def __init__(self, verb=1):
      # main variables
      self.status          = 0                  # exit value
      self.valid_opts      = None
      self.user_opts       = None

      self.sinfo           = None               # system info class

      # general variables
      self.verb            = 1

      # initialize valid_opts
      self.init_options()

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # terminal options
      self.valid_opts.add_opt('-help', 0, [],           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-todo', 0, [],            \
                      helpstr='display the current "todo list"')
      self.valid_opts.add_opt('-ver', 0, [],            \
                      helpstr='display the current version number')

      return 0

   def process_options(self, argv=sys.argv):

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, apply -help
      if '-help' in argv:
         print g_help_string
         return 0

      if '-hist' in argv:
         print g_history
         return 0

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 0

      if '-todo' in argv:
         print g_todo
         return 0

      if '-ver' in argv:
         print g_version
         return 0

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # process options, go after -verb first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      for opt in uopts.olist:

         # an unhandled option
         print '** option %s not yet supported' % opt.name
         return 1

      return None

   def show_system_info(self):

      print UTIL.section_divider('system check', hchar='=')
      print 

      self.sinfo = SC.SysInfo()

      self.sinfo.show_general_sys_info()
      self.sinfo.show_general_afni_info()
      self.sinfo.show_python_lib_info(['PyQt4'], verb=3)
      self.sinfo.show_path_vars()

   def execute(self):

      self.show_system_info()

def main():
   me = CmdInterface()
   if not me: return 1

   rv = me.process_options()
   if rv != None: return rv

   rv = me.execute()
   if rv != None: return rv

   return me.status

if __name__ == '__main__':
   sys.exit(main())


