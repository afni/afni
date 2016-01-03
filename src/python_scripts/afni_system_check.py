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

examples

   1.  afni_system_check.py -check_all
   2a. afni_system_check.py -find_prog python
   2b. afni_system_check.py -find_prog python -exact yes

-----------------------------------------------------------------------------
terminal options:

   -help                : show this help
   -hist                : show program history
   -show_valid_opts     : show valid options for program
   -todo                : show current todo list
   -ver                 : show current version

   NOTE: either a terminal or an action option is required

action options:

   -check_all           : perform all system checks
                          - see section, "details displayed via -check_all"
   -data_root DDIR      : search for class data under DDIR
   -find_prog PROG      : search PATH for PROG
                          - default is *PROG*, case-insensitive
                          - see also -casematch, -exact

other options:

   -casematch yes/no    : match case in -find_prog
   -exact yes/no        : search for PROG without wildcards in -find_prog

-----------------------------------------------------------------------------
details displayed via -check_all (just run to see):

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
R Reynolds    July, 2013
=============================================================================
"""

g_todo = """
todo: afni_system_check.py

   - mac: check for gcc?  can we tell whether openMP is supported?
          fink?  homebrew?  macports?

   - check for .afnirc/.sumarc .afni/help
   - check for data under any passed -data_root
        - this was started
   - check disk space
"""

g_history = """
   afni_system_check.py history:

   0.0  Jul 11, 2013 - initial version
   0.1  Jul 16, 2013 - added a couple of checks for early python versions
   0.2  Aug 02, 2013 - check for multiple R and python programs in PATH
   0.3  Aug 14, 2013
        - PYTHON_PATH should be PYTHONPATH
        - added a look at any /sw/bin/python* programs
   0.4  Aug 19, 2013 - update to match that of afni_util.search_path_dirs()
   0.5  Aug 26, 2013
        - system check is now run via -check_all
        - added -find_prog to search for PROG in PATH
        - added -casematch and -exact as options for -find_prog
   0.6  Mar 14, 2014 - added some data and OS-specific tests
   0.7  Mar 21, 2014
        - improved class data search
        - added -data_root for class data search
   0.8  May 16, 2014
        - if no AFNI binaries found, try path to this program
        - look for history files in data directories
        - print comments at the end, so they are easier to notice
   0.9  May 20, 2014 - macs: look for PyQt4 from homebrew and fink
   0.10 Aug 12, 2014 - 'afni -ver' is now only 1 line
   0.11 Apr 08, 2015 - check for FATCAT_DEMO
   0.12 Aug 27, 2015 - run rPkgsInstall -pkgs ALL -check
   0.13 Sep 09, 2015 - fix sequence of program check from exec dir
   0.14 Dec 29, 2015 - catch empty atlas dir list
   0.15 Jan 03, 2016 - truncate 'top history' text
"""

g_version = "afni_system_check.py version 0.15, January 3, 2015"


class CmdInterface:
   """interface class"""

   def __init__(self, verb=1):
      # main variables
      self.status          = 0          # exit value
      self.valid_opts      = None
      self.user_opts       = None

      self.sinfo           = None       # system info class

      # action variables
      self.find_prog       = ''         # program name to find
      self.sys_check       = 0

      # general variables
      self.act             = 0          # perform SOME action
      self.casematch       = -1         # >= 0 means user-specified
      self.data_root       = ''
      self.exact           = 0          # use exact matching or not
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

      # action options
      self.valid_opts.add_opt('-casematch', 1, [],      \
                      acplist=['yes','no'],
                      helpstr='yes/no: specify case sensitivity in -find_prog')
      self.valid_opts.add_opt('-check_all', 0, [],      \
                      helpstr='perform all system checks')
      self.valid_opts.add_opt('-data_root', 1, [],      \
                      helpstr='directory to check for class data')
      self.valid_opts.add_opt('-exact', 1, [],          \
                      acplist=['yes','no'],
                      helpstr='yes/no: use exact matching in -find_prog')
      self.valid_opts.add_opt('-find_prog', 1, [],      \
                      helpstr='search path for *PROG*')
      self.valid_opts.add_opt('-verb', 1, [],            \
                      helpstr='set verbosity level (default=1)')

      return 0

   def process_options(self, argv=sys.argv):

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, apply -help
      if '-help' in argv or len(argv) < 2:
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

         if opt.name == '-casematch':
            if OL.opt_is_yes(opt): self.casematch = 1
            else:                  self.casematch = 0
            continue

         if opt.name == '-check_all':
            self.act = 1
            self.sys_check = 1
            continue

         if opt.name == '-data_root':
            self.data_root = opt.parlist[0]
            continue

         if opt.name == '-exact':
            if OL.opt_is_yes(opt):
               self.exact = 1
               # default of casematching is 1 for this
               if self.casematch < 0: self.casematch = 1
            else: self.exact = 0
            continue

         if opt.name == '-find_prog':
            self.act = 1
            self.find_prog = opt.parlist[0]
            continue

         # already processing options: just continue

         if opt.name == '-verb': continue

         # an unhandled option
         print '** option %s not yet supported' % opt.name
         return 1

      if not self.act:
         print '** no action option found, please see -help output\n'
         return 1

      return None

   def show_system_info(self):

      self.sinfo = SC.SysInfo(verb=self.verb, data_root=self.data_root)

      self.sinfo.show_all_sys_info()

   def execute(self):

      if self.sys_check: self.show_system_info()
      if self.find_prog:
         # note casematching
         cm = self.casematch
         if cm < 0: cm = 0
         UTIL.show_found_in_path(self.find_prog, mtype=self.exact, casematch=cm)

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


