#!/usr/bin/env python

# python3 status: started

# system libraries
import sys, os

# AFNI libraries (test first)
from afnipy import module_test_lib
g_testlibs = ['afnipy.option_list', 'afnipy.afni_util']

if module_test_lib.num_import_failures(g_testlibs,details=0,verb=1):
   print("\n** failed to load standard AFNI python libraries")
   print("   python version = %s" % sys.version.split()[0])
   sys.exit(1)

# now load AFNI libraries by name
from afnipy import option_list as OL
from afnipy import afni_util as UTIL
from afnipy import lib_system_check as SC

g_dotfiles = ['.profile', '.bash_profile', '.bashrc', '.bash_dyld_vars',
              '.cshrc', '.tcshrc', '.login', '.zshrc' ]

g_help_string = """
=============================================================================
afni_system_check.py    - perform various system checks

This program is intended to be helpful for figuring out AFNI installation
issues.

examples

   1.  afni_system_check.py -check_all
   2a. afni_system_check.py -find_prog python
   2b. afni_system_check.py -find_prog python -exact yes
   3a. afni_system_check.py -disp_R_ver_for_lib $R_LIBS
   3b. afni_system_check.py -disp_abin

-----------------------------------------------------------------------------
terminal options:

   -help                : show this help
   -help_dot_files      : show help on shell setup files
   -help_rc_files       : SAME
   -hist                : show program history
   -show_valid_opts     : show valid options for program
   -todo                : show current todo list
   -ver                 : show current version

   NOTE: either a terminal or an action option is required

action options:

   -check_all           : perform all system checks
                          - see section, "details displayed via -check_all"
   -disp_num_cpu        : display number of CPUs available
   -disp_R_ver_for_lib  : display the R version used when building an R library
                          - this refers to those installed by rPkgsInstall,
                            most likely under $R_LIBS
   -disp_abin           : display directory containing 'afni' (or this)
   -disp_ver_afni       : display AFNI package version (else "None")
   -disp_ver_matplotlib : display matplotlib version (else "None")
   -disp_ver_pylibs LIB LIB ... :
                          display versions of given python libraries (else NONE)
                          - use 'ALL' to include the default test list
   -dot_file_list       : list all found dot files (startup files)
   -dot_file_show       : display contents of all found dot files
   -dot_file_pack NAME  : create a NAME.tgz package containing dot files
   -find_prog PROG      : search PATH for PROG
                          - default is *PROG*, case-insensitive
                          - see also -casematch, -exact

other options:

   -casematch yes/no    : match case in -find_prog
   -data_root DDIR      : search for class data under DDIR
   -exact yes/no        : search for PROG without wildcards in -find_prog
   -use_asc_path        : prepend ASC dir to PATH
                          (to test programs in same directory as ASC.py)
   -verb LEVEL          : set the verbosity level

-----------------------------------------------------------------------------
details displayed via -check_all (just run to see):

   general information:
      - CPU, operating system and version, # CPUs, login shell

   AFNI and related tests:
      - which afni, python, R and tcsh, along with versions
      - check for multiple afni packages in PATH
      - check that various AFNI programs run
      - check for AFNI $HOME dot files (.afnirc, .sumarc, etc.)
      - warn on tcsh version 6.22.03

   python libs:
      - check that various python libraries are found and loaded

   environment vars:
      - show PATH, PYTHONPATH, R_LIBS, LD_LIBRARY_PATH, DYLD_LIBRARY_PATH, etc.

   evaluation of dot files:
      - show the output of "init_user_dotfiles -test", restricted
        to shells of interest (user shells plus tcsh)

   data checks:
      - check for AFNI bootcamp data directories and atlases

   OS specific:
      - on linux, check for programs and version of dnf, yum
      - on macs, check for homebrew, fink, flat_namespace, etc.

   final overview:
      - report anything that seems to need fixing for a bootcamp
        (details shown earlier)

-----------------------------------------------------------------------------
R Reynolds    July, 2013
=============================================================================
"""

g_help_rc_files = """
        RC (run commands) background

RC files applied in a new shell are based on the way the shell is invoked
and the actual shell being used.  There are 3 ways a shell can be invoked,
as a login shell, and interactive shell, or a non-interactive shell.

   a. A login shell is one where a user first logs in on a machine, e.g.,

         - at a console login
         - when login is via ssh

      In many cases, login shells are also interactive, but they do not need
      to be.

   b. An interactive shell is meant for reading commands from stdin (standard
      input), such as when a user opens a terminal, or simply types a shell
      name and hits <enter> to start a new shell, e.g. "bash"<enter>.  It is
      meant to continue processing new commands until the input stream ends
      (e.g. via "exit" or ctrl-d).

   c. A non-interactive shell is one that a user does not interact with, such
      as when running a shell script.


This help section focuses on commonly used user controlled RC files, omitting
system files like /etc/csh.cshrc.  It also does not cover every possibility
of dot files for each shell.  There are often many files that are searched
for in each case, but we stick to what might be reasonably typical.

The noted RC files are generally kept within a user's $HOME directory, though
they are not necessarily required to be.


   1. csh/tcsh RC files: .tcshrc .cshrc

      1a. csh/tcsh login shell (e.g. ssh login):

         .tcshrc (else .cshrc)
         .login

      1b. csh/tcsh non-login shell (e.g. opening a new terminal):

         .tcshrc (else .cshrc)

      1c. csh/tcsh non-interactive shell (e.g. running a script)

         .tcshrc (else .cshrc)

         This is the same as for an interactive shell.


   2. bash RC files: .bashrc .bash_profile 

      2a. bash login shell (e.g. ssh login):

         .bash_profile (else .bash_login) (else .profile)

         Note that via ssh, the shell is both a login and an interactive one.
      

      2b. bash interactive, non-login shell
          (e.g. opening a new terminal):
         
         .bashrc

      2c. bash non-interactive, non-login shell
          (e.g. from running a script)
         
          NOTHING*  (no dot files are processed by default)

          * If needed (such as for a bash script that runs afni or suma),
            consider setting BASH_ENV to a file that contains needed
            variables, such as $DYLD_LIBRARY_PATH.  For example:

            ~/.bashrc:
                export BASH_ENV=~/.bash_dyld_vars

            ~/.bash_dyld_vars:
                export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace

   3. sh RC files: .profile

      3a. sh (bash as sh) login shell:

         .profile

      3b. sh (bash as sh) non-login shell:
      3c. sh (bash as sh) non-innteractive shell:

       * nothing is read

   4. zsh rc files: .zshenv, .zshrc, .zlogin

      4a. zsh login shell:

         .zshenv
         .zprofile
         .zlogin

      4b. zsh interactive shell:

         .zshenv
         .zshrc

      4c. zsh non-interactive shell

         .zshenv


      To put this another way, the following is a sequence of files read at
      startup, depending on the type of shell.

        .zshenv         - always
        .zprofile       - if login
        .zshrc          - if interactive
        .zlogin         - if login

"""


g_todo = """
todo: afni_system_check.py

   - check for data under any passed -data_root
        - this was started
   - check disk space
   - report RAM
   - if R failures and no R_LIBS: check 'find ~ -maxdepth 3 -name afex'
   - fail if python3?  show output from "ls -ld `which python`*"
      - consider setting VERSIONER_PYTHON_VERSION to 2.7
      - make permanent: defaults write com.apple.versioner.python Version 2.7
   - warn on old python version?
   - warn on old AFNI version
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
   0.16 Feb 16, 2016 - many new checks
        - have 'summary comments' describe issues that may require attention
        - see whether homebrew is installed
        - whine if OS X version is pre-10.7
        - report contents of AFNI_version.txt
   0.17 Mar 18, 2016 - new checks
        - added -help_rc_files
        - make comments about shell RC files, given login shell
   0.18 Mar 25, 2016 - tiny update
   0.19 May 20, 2016 - added -dot_file_list/_pack/_show
   0.20 Jul  7, 2016 - check for partial PyQt4 (for OS X 10.11)
   0.21 Aug 29, 2016 - added a few more FALLBACK tests
   0.22 Nov  2, 2016 - handle OS X 10.12 version string
   0.23 Nov 17, 2016 - look for flat_namespace on macs
   0.24 Dec  7, 2016 - check for python2 and python3
   0.25 Jan 25, 2017 - new OS X, check any DYLD vars via sub-shells
   0.26 Feb  1, 2017 - updates for brew/fink, and check for fink python link
   0.27 Jun 16, 2017 - check for libXt.7.dylib without flat_namespace
   0.28 Jul 12, 2017 - idiot; forget libXt.7 checks
   0.29 Aug 15, 2017 
        - check 3dClustSim, for OpenMP library
        - for mac, force cheating variable check via interactive shell
   0.30 Sep 27, 2017 - PyQt4 is no longer needed for an AFNI bootcamp
   1.00 Nov  7, 2017 - python3 compatible
   1.01 Nov 27, 2017 - warn user on python 3 (!= 2.7 even)
   1.02 Oct 11, 2018 - check for consistency between python and PyQt4 paths
   1.03 Oct 16, 2018 - if no AFNI prog errors, skip library linking warnings
   1.04 Oct 29, 2018 - check for .zshenv (changed to .zshrc) if zsh
   1.05 Aug  6, 2019 - check for matplotlib.pyplot; use simpl_import_test
   1.06 Aug 27, 2019
        - added check for dyn.load error in R_io.so via 3dMVM
        - show output of command "R RHOME"
   1.07 Sep 13, 2019 - report XQuartz/X11 version
   1.08 Sep 16, 2019 - check /usr/local/bin/python files, along with /sw/bin
   1.09 Nov 13, 2019 - unless asked for, omit any final mention of PyQt4
   1.10 Sep  4, 2020
        - for zsh, look for .zshrc rather than .zshenv
        - remove 'some progs need 2.7.x' warning for python 3+
   1.11 Sep 15, 2020
        - whine if .zshrc references all_progs.COMP.bash
        - some python 3.8 distribution do not come with distro
   1.12 Feb 18, 2021 - check for reasonable XQuartz version
   1.13 Oct 27, 2021 - warn if less than 5 GB disk space available
   1.14 Oct 29, 2021 - on mac, check for standard R not in PATH
   1.15 Nov 13, 2021 - add -disp_num_cpu opt to show number of available cpus
   1.16 Jan 05, 2022 - check for having both .cshrc and .tcshrc
   1.17 Jan 11, 2022
        - add -disp_ver_matplotlib
        - matplotlib version >= 2.2 is now required in AFNI
   1.18 Apr 15, 2022 
        - fix .bashrc help, it is not read in non-interactive shell
        - look for .zshrc
   1.19 Dec  9, 2022 - minor update to help_rc_files
   1.20 Feb  6, 2023 - include output from init_user_dotfiles.py -test
   1.21 Jun  7, 2023 - start looking for missing binary libraries
   1.22 Jun 13, 2023 - turn off check for PyQt4 (add option)
   1.23 Jun 20, 2023 - under linux: check for R_io.so shared dependencies
   1.24 Sep 18, 2023 - add -use_asc_path
   1.25 Sep 21, 2023 - capture the R platform with its version
   1.26 Sep 28, 2023 - add option -disp_R_ver_for_lib
   1.27 Oct 12, 2023 - only check flat_namespace on 10.7/12_local
   1.28 Nov 24, 2023
        - check for flask and flask_cors
        - add -disp_ver_pylibs, to show library version for a specified list
   1.29 Jan  2, 2024 - warn on matplotlib 3.1.2
   1.30 Feb 22, 2024 - check for conda
   1.31 Mar  4, 2024 - add option -disp_ver_afni (do include build source)
   1.32 Mar 21, 2024 - add option -disp_abin
   1.33 Apr 25, 2024 - warn if tcsh version is 6.22.03
   1.34 Jun 24, 2024 - warn if CPU differs between platform and uname -m
   1.35 Sep  4, 2024 - just get CPU from uname -m
   1.36 Sep 16, 2024
        - get .login with other dotfiles
        - if no Xvfb in PATH, check whether file exists
        - report fewer link suggestions
   1.37 Oct 24, 2024 - do away with "have python3 but not python2"
   1.38 Jan  6, 2025 - warn user of ARM mac using macos_10.12_local
   1.39 Jan 13, 2025 - updates for OS version, gcc and CLT SDK
   1.40 Jan 23, 2025 - more checks for gcc, check rPkgsInstall
   1.41 Feb 26, 2025 - note if afni is owned by root or not user
   1.42 Feb 27, 2025 - add a more detailed get_macos_mdls_val()
"""

g_version = "afni_system_check.py version 1.42, February 27, 2025"


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
      self.sys_disp        = []         # list of keywords for disp
      self.dot_file_list   = 0          # list found dot files
      self.dot_file_pack   = ''         # package dot files
      self.dot_file_show   = 0          # display dot files

      # general variables
      self.act             = 0          # perform SOME action
      self.casematch       = -1         # >= 0 means user-specified
      self.data_root       = ''
      self.exact           = 0          # use exact matching or not
      self.verb            = 1

      # disp_* helpers
      self.R_ver_lib_path  = ''         # path to R libraries
      self.py_lib_vers     = SC.g_python_vtest_libs # python libs for versions

      # initialize valid_opts
      self.init_options()

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # terminal options
      self.valid_opts.add_opt('-help', 0, [],
                      helpstr='display program help')
      self.valid_opts.add_opt('-help_dot_files', 0, [],
                      helpstr='display help on shell setup files')
      self.valid_opts.add_opt('-help_rc_files', 0, [],
                      helpstr='display help on shell setup files')
      self.valid_opts.add_opt('-hist', 0, [],
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-todo', 0, [],
                      helpstr='display the current "todo list"')
      self.valid_opts.add_opt('-ver', 0, [],
                      helpstr='display the current version number')

      # action options
      self.valid_opts.add_opt('-casematch', 1, [],
                      acplist=['yes','no'],
                      helpstr='yes/no: specify case sensitivity in -find_prog')
      self.valid_opts.add_opt('-check_all', 0, [],
                      helpstr='perform all system checks')
      self.valid_opts.add_opt('-data_root', 1, [],
                      helpstr='directory to check for class data')
      self.valid_opts.add_opt('-disp_num_cpu', 0, [],
                      helpstr='display number of CPUs available')
      self.valid_opts.add_opt('-disp_R_ver_for_lib', 1, [],
                      helpstr='display R version library was built against')
      self.valid_opts.add_opt('-disp_abin', 0, [],
                      helpstr='display directory containing afni (or this)')
      self.valid_opts.add_opt('-disp_ver_afni', 0, [],
                      helpstr='display AFNI package version (else None)')
      self.valid_opts.add_opt('-disp_ver_matplotlib', 0, [],
                      helpstr='display matplotlib version (else None)')
      self.valid_opts.add_opt('-disp_ver_pylibs', -1, [],
                      helpstr='display python library versions (else NONE)')
      self.valid_opts.add_opt('-dot_file_list', 0, [],
                      helpstr='list found dot files')
      self.valid_opts.add_opt('-dot_file_pack', 1, [],
                      helpstr='package dot files into given tgz package')
      self.valid_opts.add_opt('-dot_file_show', 0, [],
                      helpstr='display contents of dot files')
      self.valid_opts.add_opt('-exact', 1, [],
                      acplist=['yes','no'],
                      helpstr='yes/no: use exact matching in -find_prog')
      self.valid_opts.add_opt('-find_prog', 1, [],
                      helpstr='search path for *PROG*')
      self.valid_opts.add_opt('-use_asc_path', 0, [],
                      helpstr='immediately prepend ASC dir to PATH')
      self.valid_opts.add_opt('-verb', 1, [],
                      helpstr='set verbosity level (default=1)')

      return 0

   def process_options(self, argv=sys.argv):

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, apply -help
      if '-help' in argv or len(argv) < 2:
         print(g_help_string)
         return 0

      if '-help_rc_files' in argv or '-help_dot_files' in argv:
         print(g_help_rc_files)
         return 0

      if '-hist' in argv:
         print(g_history)
         return 0

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 0

      if '-todo' in argv:
         print(g_todo)
         return 0

      if '-ver' in argv:
         print(g_version)
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

         if opt.name == '-disp_num_cpu':
            self.act = 1
            self.sys_disp.append('num_cpu')
            continue

         if opt.name == '-disp_R_ver_for_lib':
            self.act = 1
            self.sys_disp.append('R_ver_for_lib')
            self.R_ver_lib_path = opt.parlist[0]
            continue

         if opt.name == '-disp_abin':
            self.act = 1
            self.sys_disp.append('abin')
            continue

         if opt.name == '-disp_ver_afni':
            self.act = 1
            self.sys_disp.append('ver_afni')
            continue

         if opt.name == '-disp_ver_matplotlib':
            self.act = 1
            self.sys_disp.append('ver_matplotlib')
            continue

         if opt.name == '-disp_ver_pylibs':
            self.act = 1
            self.sys_disp.append('ver_pylibs')
            self.py_lib_vers = opt.parlist
            continue

         if opt.name == '-data_root':
            self.data_root = opt.parlist[0]
            continue

         if opt.name == '-dot_file_list':
            self.dot_file_list = 1
            self.act = 1
            continue

         if opt.name == '-dot_file_pack':
            self.dot_file_pack = opt.parlist[0]
            self.act = 1
            continue

         if opt.name == '-dot_file_show':
            self.dot_file_show = 1
            self.act = 1
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

         # apply to PATH immediately
         if opt.name == '-use_asc_path':
            ascdir = UTIL.executable_dir()
            if self.verb > 1:
               print("++ prepending %s to PATH" % ascdir)
            os_path = os.environ.get("PATH")
            os.environ["PATH"] = ascdir + ":" + os_path
            continue

         # already processed options: just continue

         if opt.name == '-verb': continue

         # an unhandled option
         print('** option %s not yet supported' % opt.name)
         return 1

      if not self.act:
         print('** no action option found, please see -help output\n')
         return 1

      return None

   def show_system_info(self):

      self.sinfo = SC.SysInfo(verb=self.verb, data_root=self.data_root)

      self.sinfo.show_all_sys_info()

   def show_system_info_items(self, items=[]):
      '''Show a subset of items that would be in the full check_all, as
      displayed by show_system_info().

      items is a list of strings, each being a keyword to display part
      of the full system check info.  Keywords in this list can be
      built up over time.

      '''

      self.sinfo = SC.SysInfo(verb=self.verb, data_root=self.data_root)

      # check for any known item strings
      for x in items:
          if x == 'num_cpu':
              print(self.sinfo.get_cpu_count())
          if x == 'abin':
              # check this rather than afni?
              print(SC.get_prog_dir('afni_system_check.py'))
          if x == 'ver_afni':
              print(self.sinfo.get_ver_afni())
          if x == 'ver_matplotlib':
              print(self.sinfo.get_ver_matplotlib())
          if x == 'ver_pylibs':
              # have this verbosity default to 0
              if self.verb > 1: vv = self.verb
              else:             vv = 0
              self.sinfo.show_python_lib_versions(self.py_lib_vers, verb=vv)
          if x == 'R_ver_for_lib':
              print(self.sinfo.get_R_ver_for_lib(self.R_ver_lib_path))

   def check_dotfiles(self, show=0, pack=0):
      global g_dotfiles

      home = os.environ['HOME']

      # get list of existing files
      dfound = []
      for dfile in g_dotfiles:
         if os.path.isfile('%s/%s' % (home, dfile)):
            dfound.append(dfile)
            print('found under $HOME : %s' % dfile)

      if show:
         for dfile in dfound:
            print(UTIL.section_divider(dfile, hchar='='))
            print('%s\n' % UTIL.read_text_file('%s/%s' % (home, dfile), lines=0))

      if pack:
         import shutil
         package = self.dot_file_pack
         pgz = '%s.tgz' % package
         # maybe user included the extension
         ext = package.find('.tgz')
         if ext >= 0:
            pgz = package
            package = package[0:ext]
         if os.path.exists(package) or os.path.exists('%s.tgz'%package):
            print("** error: package dir '%s' or file '%s' already exists"\
                  % (package, pgz))
            return 1

         try: os.mkdir(package)
         except:
            print("** failed to make dot file package dir '%s'"  % package)
            return 1
         for dfile in dfound:
            shutil.copy2('%s/%s' % (home, dfile), package)
         os.system("tar cfz %s %s" % (pgz, package))
         shutil.rmtree(package)
         if os.path.exists(pgz): print('++ dot file package is in %s' % pgz)
         else: print('** failed to make dot file package %s' % pgz)

      return 0

   def execute(self):

      if len(self.sys_disp):  self.show_system_info_items(self.sys_disp)
      if self.sys_check: self.show_system_info()
      if self.find_prog:
         # note casematching
         cm = self.casematch
         if cm < 0: cm = 0
         UTIL.show_found_in_path(self.find_prog, mtype=self.exact, casematch=cm)
      if self.dot_file_list or self.dot_file_show or self.dot_file_pack:
         show = self.dot_file_show
         pack = self.dot_file_pack != ''
         self.check_dotfiles(show=show, pack=pack)

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


