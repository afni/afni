#!/usr/bin/env python

# python3 status: started

# library for performing various system checks

import os, sys
from afnipy import module_test_lib as MT

# test dependency libs before proceeding
# note: platform came with python 2.3
testlibs = ['platform', 'afnipy.afni_base', 'afnipy.afni_util']
if MT.num_import_failures(testlibs):
   sys.exit(1)

import platform, glob
from afnipy import afni_base as BASE
from afnipy import afni_util as UTIL

# ------------------------------ globals ------------------------------

# ---- sites   e.g. https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/
#                           background_install/install_instructs/steps_mac.html

g_site_afnidoc      = 'https://afni.nimh.nih.gov/pub/dist/doc/htmldoc'
g_site_install_root = '%s/background_install/install_instructs' % g_site_afnidoc
g_site_install_mac  = '%s/steps_mac.html' % g_site_install_root

# only run 'df' once per root directory
g_fs_space_checked  = []
g_fs_space_whine    = 1         # do we whine about fs space? (only once)

# ------------------------------ main class  ------------------------------

class SysInfo:
   """system info class"""

   def __init__(self, data_root='', verb=1):

      self.system          = platform.system()
      self.home_dir        = os.environ['HOME']
      self.data_root       = data_root
      self.verb            = verb

      self.afni_ver        = ''
      self.afni_label      = ''
      self.afni_dir        = ''
      self.python_prog     = '' # path to program
      self.os_dist         = ''
      self.comments        = [] # comments to print at the end
      self.afni_fails      = 0

      # shell stuff
      self.cur_shell       = ''
      self.login_shell     = ''
      self.rc_file         = ''

      self.repo_prog       = '' # e.g. yum or brew
      self.have_matplotlib = 0
      self.have_pyqt4      = 0
      self.warn_pyqt       = 0  # should we add PyQt(4?) message to 'comments'
      self.ok_openmp       = 0  # does 3dAllineate work, for example?

   def get_afni_dir(self):
      s, so, se = BASE.simple_shell_exec('which afni', capture=1)
      if s: return ''
      adir = so.strip()
      if adir[-5:] == '/afni': return adir[0:-5]
      else:                    return ''

   def show_general_sys_info(self, header=1):
      if header: print(UTIL.section_divider('general', hchar='-'))

      print('architecture:         %s' % tup_str(platform.architecture()))
      print('system:               %s' % platform.system())
      print('release:              %s' % platform.release())
      print('version:              %s' % platform.version())

      # check distributions by type - now all over the place
      self.os_dist = distribution_string() # save for later
      print('distribution:         %s' % self.os_dist)
         
      print('number of CPUs:       %s' % self.get_cpu_count())

      # note shell, and if we are not in login shell
      logshell = UTIL.get_login_shell()
      curshell = UTIL.get_current_shell()
      if logshell == curshell: note = ''
      else:                    note = '  (current shell is %s)' % curshell

      if logshell not in ['csh', 'tcsh']:
         self.comments.append("just be aware: login shell '%s', but our code" \
                              " examples use 'tcsh'" % logshell)

      print('apparent login shell: %s%s' % (logshell, note))

      self.cur_shell       = curshell
      self.login_shell     = logshell

      self.set_shell_rc_file([logshell, curshell])
      if self.home_file_exists(self.rc_file): fstr = 'exists'
      else:                                   fstr = 'does not exist'
      print('shell RC file:        %s (%s)' % (self.rc_file, fstr))
      print('')

   def set_shell_rc_file(self, slist):
      """and many any useful comments"""

      cc = []
      self.rc_file = 'NONE'

      # ordered from esoteric to mainstream/preferred
      # (self.rc_file might be overwritten by later calls)

      if 'sh' in slist:
         # non-login shell ref: NONE
         # login shell ref: .profile"
         fname = '.profile'
         self.rc_file = fname
         if not os.path.isfile('%s/%s' % (self.home_dir,fname)):
            cc.append("shell sh : MISSING login shell setup file %s" % fname)
         elif self.verb > 1: 
            print("shell sh : good: found login shell setup file %s"%fname)
         
      if 'zsh' in slist:
         # general env file: .zshenv
         # login shell file: .zprofile
         # interactive file: .zshrc
         # order of files: .zshenv  .zprofile  .zshrc  .zlogin
         fname = '.zshrc'
         self.rc_file = fname
         if not os.path.isfile('%s/%s' % (self.home_dir,fname)):
            cc.append("shell zsh : MISSING env shell setup file %s" % fname)
         elif self.verb > 1: 
            print("shell zsh : good: found env shell setup file %s" %fname)
         

      if 'bash' in slist:
         # non-login shell ref: .bashrc
         # login shell ref, first of: .bash_profile, .bash_login, .profile
         f1name = '.bash_profile'
         f2name = '.bashrc'
         self.rc_file = f2name
         f1found = 1
         f2found = 1
         if not self.home_file_exists(f1name):
            f1name = '.bash_login'
         if not self.home_file_exists(f1name):
            f1name = '.profile'
         if not self.home_file_exists(f1name):
            f1name = '.bash_profile' # call this the default
            cc.append("shell bash: MISSING login setup file, e.g. %s" % f1name)
            f1found = 0

         if not self.home_file_exists(f2name):
            cc.append("shell bash: MISSING non-login setup file %s" % f2name)
            f2found = 0

         gfound = 0
         if f1found and f2found:
            # does f1name reference f2name?
            st, so, se = UTIL.limited_shell_exec("\grep %s $HOME/%s" \
                                                 % (f2name, f1name))
            if not st: gfound = 1

         if not f1found or not f2found or not gfound:
            ss="shell bash: consider sourcing (non-login) %s from (login) %s" \
               % (f2name, f1name)
            cc.append(ss)

      # choose between tcsh and csh, if either is used
      sname = ''
      if 'tcsh' in slist: sname = 'tcsh'
      if sname == '' and 'csh' in slist: sname = 'csh'
      if sname != '':
         f1name = '.tcshrc'
         if not self.home_file_exists(f1name): f1name = '.cshrc'
         self.rc_file = f1name

         if not self.home_file_exists(f1name):
            cc.append('shell %-4s: missing setup file %s' % (sname, f1name))

         self.check_multi_tcsh_startup_files()

      self.comments.extend(cc)

   def check_multi_tcsh_startup_files(self):
      """if both .cshrc and .tcshrc exist and .t does not source .c, warn
      """
      cfile = '.cshrc'
      tfile = '.tcshrc'
      if not self.home_file_exists(cfile) or not self.home_file_exists(tfile):
         return
      if self.verb > 1:
         print("-- found both %s and %s" % (cfile,tfile))

      found = 0
      st, so, se = UTIL.limited_shell_exec("\grep %s $HOME/%s" % (cfile,tfile))
      # if we find something, test to see if it is valid
      if st == 0:
         for line in so:
            words = line.split()
            if words[0] in ['.', 'source'] and 'cshrc' in words[1]:
               found = 1
               break
            if self.verb > 2:
               print("-- have unapplied %s in %s:" % (cfile, tfile))
               print("   %s" % line)

      if not found:
         m0 = "have both %s and %s, with former not applied" % (cfile, tfile)
         m1 = "(consider adding 'source $HOME/%s' to $HOME/%s)" % (cfile, tfile)
         print("** %s" % m0)
         print("   %s" % m1)
         self.comments.append(m0)
         self.comments.append(" " + m1)
      elif self.verb > 2:
         print("-- %s appears to be sourced from %s" % (cfile, tfile))

   def home_file_exists(self, fname):
      return(os.path.isfile('%s/%s' % (self.home_dir, fname)))

   def show_top_line(self, fname, prefix='', last=0):
      htxt = UTIL.read_top_lines(fname, nlines=1, strip=1, verb=0)
      if len(htxt) == 0: htxt = 'NONE FOUND'
      else:              htxt = htxt[0]
      # possibly truncate to '...' and final 'last' characters
      if last > 0 and len(htxt) > last:
         htxt = '...' + htxt[-last:]
      print('%s%s' % (prefix, htxt))

   def show_data_dir_info(self, ddir, histfile=''):
      """try to locate and display the given data directory
         if histfile, display the top line

         return 0 if found, else 1
      """
      global g_fs_space_whine

      status, droot = self.find_data_root(ddir, hvar=0)
      if status:
         print('data dir : missing %s' % ddir)
         return 1

      # have a directory, show it (and check file system space)
      m_min = 5000
      status, space = self.get_partition_space(droot, m_min=m_min)
      if space:
         sstr = ' (%s Avail)' % space
      else:
         sstr = ''
      dhome = droot.replace(self.home_dir, '$HOME')
      print('data dir : found %-12s under %s%s' % (ddir, dhome, sstr))
      # if we detect insufficient space, warn (but only once)
      if status and g_fs_space_whine:
         self.comments.append('possibly insufficient disk space for bootcamp')
         self.comments.append(' (prefer >= %d MB for data analysis)' % m_min)
         g_fs_space_whine = 0

      # possibly show histfile
      if histfile == '': return 0

      prefix = '           top history: '
      hname = '%s/%s/%s' % (droot, ddir, histfile)
      try:
         self.show_top_line(hname, prefix=prefix, last=76-len(prefix))
      except UnicodeDecodeError:
         print("Unicode decoding error occurred when trying to read %s." \
               " No info retrieved"%hname)

      return 0

   def get_partition_space(self, dname, m_min=-1):
      """run df -hm $dname, and return the value in '1M-blocks' column
                            (if no such column, use Avail)

            dname   : directory name to run df on
            m_min   : if 1M-blocks and m_min >= 0, the min cutoff for failure

         - find offset of header in first line of text
         - starting at that position in the last line, return the first word
           (it might not be the second line, in case Filesystem is too wide

         Only run this once per dname, so track in g_fs_space_checked list.

         return status and the size string
                status: 1 on insufficient space, else 0
      """

      # do not check any dname more than once
      global g_fs_space_checked
      if dname in g_fs_space_checked:
         return 0, ''
      g_fs_space_checked.append(dname)

      # try for an integral number of 1M-blocks
      m = 1
      cmd = 'df -hm %s' % dname
      s, so, se = UTIL.limited_shell_exec(cmd, nlines=3)
      hsearch = '1M-blocks'

      # if we fail at first, just get a value, probably in G
      if s:
         cmd = 'df -h %s' % dname
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=3)
         hsearch = 'Avail'
      if s: return 0, ''
    
      if len(so) < 2: return 0, ''

      hstr = so[0]
      astr = so[-1]

      posn = hstr.find(hsearch)
      if posn < 0: return 0, ''
      if len(astr) <= posn: return 0, ''

      # start from position of desired header string
      astr = astr[posn:]
      alist = astr.split()
      if len(alist) == 0:
         return 0, ''
      astr = alist[0]

      # if 'm' and this is an int, check for insufficient space
      status = 0
      if m and m_min >= 0:
         try:
            nmeg = int(astr)
         except:
            nmeg = -1
         # failure
         if nmeg >= 0 and nmeg < m_min:
            status = 1
      # if m, append a unit
      if m:
         astr = '%sM' % astr
            
      return status, astr

   def show_data_info(self, header=1):
      """checks that are specific to data
            - class data existence and tree root
               - assume $HOME if not found
            - disk space under data root
               - maybe check for mounted file system
            - atlases (maybe just @Find TT_N27+tlrc?)
      """

      if header: print(UTIL.section_divider('data checks', hchar='-'))

      # locate various data trees, and possibly show recent history
      rv = 0
      rv += self.show_data_dir_info('AFNI_data6', 'history.txt')
      rv += self.show_data_dir_info('AFNI_demos', 'history.txt')
      rv += self.show_data_dir_info('suma_demo', 'README.archive_creation')
      rv += self.show_data_dir_info('afni_handouts')

      if rv:
        self.comments.append('insufficient data for AFNI bootcamp')
        self.comments.append(' (see "Prepare for Bootcamp" on install pages)')

      evar = 'AFNI_ATLAS_DIR'
      tryenv = 0                        # might suggest setting evar
      haveenv = evar in os.environ
      if haveenv: edir = os.environ[evar]
      else:       edir = ''

      # look for atlases in multiple directories
      atlas = 'TT_N27+tlrc'
      if os.path.isfile('%s/%s.HEAD'%(edir,atlas)): glist = [edir]
      else: glist = []

      cmd = '@FindAfniDsetPath %s' % atlas
      s, so, se = UTIL.limited_shell_exec(cmd, nlines=1)
      if s: tryenv = 1  # failed
      elif len(so) > 0: glist.append(so[0])

      for ddir in ['/usr/share/afni/atlases', '/usr/local/afni/atlases']:
         if os.path.isfile('%s/%s.HEAD'%(ddir,atlas)):
            glist.append(ddir)
            if tryenv:
               self.comments.append('consider setting %s to %s' % (evar,ddir))

      # fix to work with found after the fact
      glist = UTIL.get_unique_sublist(glist)

      if len(glist) == 0:
         print('atlas    : did not find %s' % atlas)
         self.comments.append('possibly missing atlases')
      else:
         for ddir in glist:
            print('atlas    : found %-12s under %s' % (atlas, ddir))

      if haveenv: print("\natlas var: %s = %s" % (evar, edir))

      print('')

   def find_data_root(self, ddir, hvar=1):
      """try to find ddir in common locations

         if hvar, use $HOME for home directory

         return status (0 = success) and path to (parent of) ddir
      """

      # if we do not have a data root, check in a few places
      if self.data_root != '':
         hdir = self.data_root
         plist = [self.data_root]
         root_str = hdir
      else: 
         hdir = self.home_dir
         plist = [ hdir, '%s/Desktop'%hdir, '%s/*data*' % hdir]
         root_str = '$HOME'

      dpath = self.find_data_dir(ddir=ddir, gdirs=plist)

      if dpath != None:
         if hvar: return 0, dpath.replace(self.home_dir, '$HOME')
         else:    return 0, dpath

      # and check the current directory
      dpath = self.find_data_dir(ddir=ddir, gdirs=['.'])
      if dpath == None: return 1, ''
      else:
         dpath = os.path.realpath(dpath)
         if hvar: return 0, dpath.replace(self.home_dir, '$HOME')
         else:    return 0, dpath

   def find_data_dir(self, ddir, gdirs=[], depth=2):
      """search under a list of glob directories for the given ddir"""
      if ddir == '' or len(gdirs) == 0: return None

      dlist = []
      for pdir in gdirs:
         droot = pdir
         for d in range(depth+1):
            dlist.extend(glob.glob('%s/%s' % (droot, ddir)))
            droot += '/*'
      if self.verb > 3: print('-- found %s dirs %s' % (ddir, dlist))
      dlist = UTIL.get_unique_sublist(dlist)
      if self.verb > 2: print('-- found trimmed %s dirs %s' % (ddir, dlist))
      
      if len(dlist) == 0: return None
      dlen = len(ddir)+1
      return dlist[0][0:-dlen]

   def show_os_specific(self, header=1):
      """checks that are specific to one OS or another"""

      if self.system not in ['Linux', 'Darwin']: return

      if header: print(UTIL.section_divider('OS specific', hchar='-'))

      if   self.system == 'Linux':  self.show_spec_linux()
      elif self.system == 'Darwin': self.show_spec_macos()

      print('')

   def show_spec_linux(self):
      """linux specific checks
            - is Ubuntu
               - has Ubuntu AFNI installed
               - atlas directory
      """

      # check for repositories
      self.check_for_progs(['dnf', 'yum', 'apt-get'], repos=1)

      if self.os_dist.find('buntu') >= 0:
         print('have Ubuntu system: %s' % self.os_dist)
         if self.afni_ver.find('buntu') >= 0:
            print('have Ubuntu afni  : %s' % self.afni_ver)

      # add PyQt4 comment, if missing
      if not self.have_pyqt4:
         # if no check requested, do not mention PyQt4 in self.comments list
         if not self.warn_pyqt:
            return

         # but if we want to comment on PyQt4 ...
         repo = self.repo_prog
         package = ''
         if repo in ['yum', 'dnf']: package = 'PyQt4'
         elif repo == 'apt-get': package = 'python-qt4'
         
         if package != '':
            self.comments.append('consider running: %s install %s' \
                                 % (repo, package))
         else:
            self.comments.append('consider installing PyQt4')

   def show_spec_macos(self):
      """look for fink, macports, homebrew, PyQt4"""

      # first check on XQuartz (and Xcode?)
      self.check_for_progs(['XQuartz'], show_missing=1)

      # check for repositories
      nfound = self.check_for_progs(['fink', 'brew', 'port'], repos=1)
      if nfound == 0:
         # since we do not require PyQt4 during class, do not warn
         # self.comments.append('consider installing fink')
         print('** no package manager found (okay for bootcamp)')
      self.hunt_for_homebrew()
      if self.get_macos_ver() < 7:
         self.comments.append('OS X version might be old')

      # add PyQt4 comment, if missing (check for brew and fink packages)
      if not self.have_pyqt4:
         glist = glob.glob('/usr/local/lib/python2*/site-packages/PyQt4')
         if len(glist) == 0:
            glist = glob.glob('/sw/lib/qt4*/lib/python2*/site-packages/PyQt4')
         if len(glist) > 0:
            gdir = glist[-1]
            ghead = os.path.dirname(gdir)
            print('++ found PyQt4 under %s' % ghead)
            if self.warn_pyqt:
               self.comments.append('consider adding %s to PYTHONPATH' % ghead)
            # if fink, see whether that python exists
            if ghead.startswith('/sw'):
               ppath = '/sw/bin/python'
               if os.path.isfile(ppath+'2.7'):
                  if not os.path.isfile(ppath) and not os.path.islink(ppath):
                     cs = 'consider linking to fink python2.7:'
                     ls = '   sudo ln -s %s %s' % (ppath+'2.7', ppath)
                     print('** seem to be using fink python2.7 but need python')
                     print('   consider:%s' % ls)
                     if self.warn_pyqt:
                        self.comments.append(cs)
                        self.comments.append(ls)

            # warn user if PyQt4 does not match python
            cs = ''
            if self.python_prog and ghead.startswith('/sw'):
               if not self.python_prog.startswith('/sw/bin'):
                  cs = 'have fink PyQt4, but non-fink python %s' % \
                       self.python_prog
            elif self.python_prog and ghead.startswith('/usr/local'):
               if not self.python_prog.startswith('/usr/local'):
                  cs = 'have brew? PyQt4, but non-brew python %s' % \
                       self.python_prog
            if cs:
               print("** warning: %s" % cs)
               if self.warn_pyqt:
                  self.comments.append(cs)
               
         elif self.repo_prog == 'fink':
            fcmd = 'sudo fink install pyqt4-mac-py27'
            print('-- for PyQt4 under %s, consider running:\n   %s' \
                  % (self.repo_prog, fcmd))
         elif self.repo_prog == 'brew':
            fcmd = 'brew install cartr/qt4/pyqt'
            print('-- for PyQt4 under %s, consider running:\n   %s' \
                  % (self.repo_prog, fcmd))

      self.check_for_pre_11_dylib()

      # in 10.11, check for gcc under homebrew
      self.check_for_10_11_lib('libgomp.1.dylib', wpath='gcc/*/lib/gcc/*')
      self.check_for_10_11_lib('libglib-2.0.dylib', wpath='glib/*/lib')
      self.check_for_flat_namespace()

      self.check_for_macos_R_in_path()

      # forget this function - I forgot that the problem was a non-flat version
      #                        of libXt6, not a 6 vs 7 issue...
      # self.check_for_libXt7()

   def hunt_for_homebrew(self):
      """assuming it was not found, just look for the file"""
      # if already found, do not bother
      if self.repo_prog == 'brew': return 0 

      bdir = '/usr/local/bin'
      bfile = 'brew'
      bpath = '%s/%s' % (bdir,bfile)
      if os.path.isfile(bpath):
         print("++ found '%s' at %s" % (bfile, bpath))
         return 1

      return 0
            
   def check_for_pre_11_dylib(self):
      """in 10.X where 7 <= X <= 10, DYLD_FALLBACK_LIBRARY_PATH
         might be needed (unless homebrew is installed and 10.10?)

         - if AFNI prog failures and if not set:
            suggest setting to abin
            (comment if homebrew is installed)
      """

      # if 0 or 1 AFNI failures, we are gone
      if self.afni_fails < 2: return
            
      # this check only applis to OS X 10.7 through 10.10 (and if that)
      osver = self.get_macos_ver()
      if osver < 7 or osver > 10:
         return

      # count AFNI dylib files
      dfiles = glob.glob('%s/*.dylib' % self.afni_dir)
      nadylib = len(dfiles)

      # if set, check if any dylibs exist
      fvar = 'DYLD_FALLBACK_LIBRARY_PATH'
      if fvar not in os.environ:
         print('** AFNI program failures' \
               ' and DYLD_FALLBACK_LIBRARY_PATH not set')
         if nadylib > 0:
            self.comments.append('consider setting DYLD_FALLBACK_LIBRARY_PATH'\
                                 ' to abin, e.g.\n   '                        \
                                 'setenv DYLD_FALLBACK_LIBRARY_PATH %s'       \
                                 % self.afni_dir)
         else:
            self.comments.append('DYLD_FALLBACK_LIBRARY_PATH not set and no'  \
                                 ' abin/*.dylib')
      else:
         fdir = os.environ[fvar]
         # count FALLBACK dylib files
         dfiles = glob.glob('%s/*.dylib' % fdir)
         nfdylib = len(dfiles)
         if nfdylib == 0:
            print('** no dylib files under %s directory' % fvar)
         if fvar != self.afni_dir and nadylib > 0:
            self.comments.append('consider changing DYLD_FALLBACK_LIBRARY_PATH'\
                                 ' to abin, e.g.\n   '                        \
                                 'setenv DYLD_FALLBACK_LIBRARY_PATH %s'       \
                                 % self.afni_dir)
         elif fvar != self.afni_dir:
            self.comments.append('not sure about DYLD_FALLBACK_LIBRARY_PATH')

   def check_for_macos_R_in_path(self):
      """if R is not in PATH, but it exists, suggest the directory to the user

         check:
            /Library/Frameworks/R.framework/Versions/3.6/Resources/bin
            /Library/Frameworks/R.framework/Versions/Current/Resources/bin

         this function is just to possibly add to self.comments
      """
      # if R is in PATH, we are done
      if UTIL.num_found_in_path('R', mtype=1) > 0:
         return

      # so no R in PATH, but check in case it actually exists
      rroot = '/Library/Frameworks/R.framework/Versions'
      rbin_36 = '%s/3.6/Resources/bin' % rroot
      rbin_cur = '%s/Current/Resources/bin' % rroot
      ex36 = os.path.exists('%s/R'%rbin_36)
      excur = os.path.exists('%s/R'%rbin_cur)

      if ex36:
         self.comments.append("have R, but need to add dir to PATH")
         self.comments.append(" add dir: %s" % rbin_36)
      elif excur:
         self.comments.append("have R, but need to add dir to PATH")
         self.comments.append(" add dir: %s" % rbin_cur)

   def check_for_10_11_lib(self, libname, wpath='gcc/*/lib/gcc/*'):
      """in 10.11, check for library under homebrew

         wpath = wildcard path to library name

         return 0 if no issue was detected
      """
      # if no homebrew, do not bother
      if self.repo_prog != 'brew':
         return 0

      # require 10.11, unless being verbose
      if self.get_macos_ver() < 11 and self.verb <= 1:
         return 0

      sname   = wpath.split('/')[0]    # short name, e.g. gcc
      libdir  = '/usr/local/lib'
      libpath = '%s/%s' % (libdir, libname)

      croot = '/usr/local/Cellar'
      clibs = glob.glob('%s/%s/%s' % (croot, wpath, libname))
      clibs.sort(reverse=True)
      # first check for any homebrew gomp libraries, at all
      if len(clibs) == 0:
         if self.afni_fails > 0:
             self.comments.append('consider installing %s under homebrew'%sname)
         else:
             print('-- consider installing %s under homebrew' % sname)
         return 1

      # if the library exists (as link or file), we are good to go
      if os.path.exists(libpath):
         if os.path.islink(libpath):
            lname = os.readlink(libpath)
            print('++ found valid link %s\n   to %s' % (libpath, lname))
         else:
            print('++ found existent library %s' % libpath)
         return 0

      # ** does not exist: so either no link or a bad one **

      # if no link, suggest making one (only if there were AFNI program errors)
      if not os.path.islink(libpath):
         mesg = 'consider linking %s under %s' % (clibs[0],libdir)
         if self.afni_fails > 0:
            self.comments.append(mesg)
         print("** %s" % mesg)
         return 1

      # huston, we have a bad link, say something useful
      print('** bad link %s, probably to old version' % libpath)
      print('   --> points to missing %s' % os.readlink(libpath))
      print('   --> consider instead: %s' % clibs[0])
      print('   for example:\n' \
            '       rm -f %s\n' \
            '       ln -s %s %s' % (libpath, clibs[0], libpath))
      mesg = 'consider fixing link %s \n   to point to %s' % (libpath,clibs[0])

      if self.afni_fails > 0:
         self.comments.append(mesg)
      print("** %s" % mesg)

      return 1

   def check_for_flat_namespace(self, fnames=['libXt']):
      """note whether /opt/X11/lib/flat_namespace exists and is non-empty

         in particular, check for any libraries in fnames list

         return 0 if no error was detected
      """

      # require 10.9, unless being verbose (nah, just check...)
      # if self.get_macos_ver() < 9 and self.verb <= 1:
      #    return 0

      flatdir = '/opt/X11/lib/flat_namespace'

      # if the directory exists and is non-empty, note it
      flibs = glob.glob('%s/*dylib*' % flatdir)
      # first check for any homebrew gomp libraries, at all
      if len(flibs) > 0:
         print("++ found %d dylib files under '%s'" % (len(flibs), flatdir))
      else:
         if self.verb > 1: print('-- no flat_namespace libraries exist')
         return 0

      found = 0
      for name in fnames:
         flibs = glob.glob('%s/%s*dylib*' % (flatdir, name))
         if len(flibs) > 0:
            print("   -- found '%s' dylib files:" % name)
            print('      ' + '\n      '.join(flibs))
            found += 1

      # if no libraries are found, we are done
      if not found:
         if self.verb > 1: print('-- no checked flat_namespace libraries found')
         return 0

      # so there is something here that we might care about

      edir = 'DYLD_LIBRARY_PATH'
      if flatdir in self.split_env_var(edir):
         print('++ yay, env var %s contains %s' % (edir, flatdir))
      elif edir in os.environ:
         print('** env var %s does not contain %s' % (edir, flatdir))
         print('   (so afni and suma might fail)')
         self.comments.append('consider appending %s with %s' % (edir,flatdir))
      else:
         if self.get_macos_ver() >= 11:
            self.check_evar_path_for_val(edir, flatdir)
            if self.cur_shell.find('csh') < 0:
               self.check_evar_path_for_val(edir, flatdir, shell='tcsh')
         else:
            print('** env var %s is not set to contain %s' % (edir, flatdir))
            print('   (so afni and suma may fail)')
            self.comments.append('consider setting %s to %s' % (edir, flatdir))

      return 1

   def check_for_libXt7(self):
      """check for /opt/X11/lib/libXt.7.dylib directly, and any link to it
        (only do anything if it exists)
      """

      odir = '/opt/X11/lib'
      fname = 'libXt.7.dylib'
      fpath = '%s/%s' % (odir, fname)
      lname = 'libXt.dylib'

      # if it is not here, we are done
      if not os.path.exists(fpath):
         return 0

      print('++ found without flat_namespace: %s' % fpath)

      # let the user know what the link points to
      lpath = '%s/%s' % (odir, lname)
      realpath = os.path.realpath(lpath)
      if os.path.islink(lpath):
         if realpath.find('libXt.6') > 0:
            gstr = '(good!)'
         elif realpath.find('libXt.7') > 0:
            gstr = '(bad)'
         else:
            gstr = '(bad - unknown)'
         print('   link %s points to %s %s' % (lname, realpath, gstr))

         if realpath.find('libXt.7') > 0:
            msg = '%s link points to version 7, should point to 6' % lname
            print('** %s' % msg)
            self.comments.append(msg)
      else:
         print('** %s is not a link, listing all such files...' % lname)
         os.system('ls -l %s/libXt.*dylib' % odir)

      return 1

   def check_evar_path_for_val(self, evar, val, shell=''):
    
      if shell == '':
         shell = self.cur_shell
         print("-- recent OS X, cheating to check %s in cur shell '%s'..." \
               % (evar, shell))
      else:
         print("-- recent OS X, cheating to check %s in shell '%s'..." \
               % (evar, shell))

      s, so = self.get_shell_value(shell, evar)

      # if not even set, fail
      if s or not so:
         print('** env var %s not set to contain %s' % (evar, val))
         self.comments.append('please set %s to %s in %s' % (evar, val, shell))
         return 0

      # convert ':' delimited val list to array, and search for val
      vals = so.split(':')
      # if not found, fail
      if not val in vals:
         print('** env var %s is set, but without %s' % (evar, val))
         self.comments.append('please set %s to include %s'%(evar,val))
         return 0

      print('++ found evar %s = %s' % (evar, so))

      return 1

   def get_shell_value(self, shell, evar):
      """really cheap way to grab a value from a new shell"""
      cmd = "%s -ci 'echo $%s'" % (shell, evar)
      s, so, se = UTIL.limited_shell_exec(cmd)

      if len(so) > 0: so = so[-1]
      else: so = ''

      if self.verb > 1:
         print('++ status = %s for command: %s' % (s, cmd))
         print('   stdout = %s' % so)
         se = '\n'.join(se)
         if se: print('   stderr = %s' % se)

      return s, so

   def get_macos_ver(self):
      if self.system != "Darwin": return 0
      verlist = self.os_dist.split()
      if len(verlist) < 1: return 0
      verlist = verlist[0].split('.')
      if len(verlist) < 2: return 0
      if verlist[0] != '10': return 0

      # get version
      try:
         vint = int(verlist[1])
      except:
         print("** OSX ver: have Darwin, but dist = %s" % self.os_dist)
         return 0

      # have something useful
      return vint

   def check_for_progs(self, plist, repos=0, show_missing=0):
      """see whether the programs seem to be found, and show version

         repos:        if set, append any found to self.repo_prog list
         show_missing: if set, show empty prog line to indicate failed test
      """
      # check for programs
      nfound = 0
      for prog in plist:
         # the version file is treated specially here
         if prog == 'AFNI_version.txt':
            vinfo = UTIL.read_AFNI_version_file()
            if vinfo != '':
               nfound += 1
            else:
               self.comments.append('missing %s, maybe package is old'%prog)
            print('%-20s : %s' % (prog, vinfo))
            continue

         # as is afni
         elif prog == 'afni label':
            nfound += 1   # do not call this an error yet
            s, v = self.get_prog_version('afni')
            print('%-20s : %s' % ('', self.afni_label))
            continue

         # XQuartz/X11?
         elif prog in ['XQuartz', 'X11']:
            s, v = self.get_prog_version(prog)
            print('%-20s : %s' % ('%s version'%prog, v))
            continue

         # and python - add a comment if they are using a version < 2.7
         elif prog == 'python':
            s, vstr = self.get_prog_version(prog)
            vf = self.get_python_ver_float()
            mesg = ''
            # (removed old warning for 3.0+)
            if vf < 2.7:
               mesg = 'have python version %s, consider using 2.7+' % vstr
            if mesg != '':
               self.comments.append(mesg)

         # now run the normal test

         cmd = 'which %s' % prog
         s, so, se = BASE.simple_shell_exec(cmd, capture=1)
         if not s: # found one
            # if we do not yet know of a repo program, mark as this one
            if repos and self.repo_prog == '': self.repo_prog = prog

            progpath = so.strip()
            print('%-20s : %s' % (cmd, progpath))
            s, v = self.get_prog_version(prog)
            if s: print('%-20s : %s' % ('%s version'%prog, v))

            # save some results
            if prog == 'afni': self.afni_ver = v
            if prog == 'python': self.python_prog = progpath
               
            nfound += 1
         elif show_missing:
            print('%-20s : %s' % (cmd, se))

      print('')

      return nfound

   def test_python_lib_matplotlib(self, verb=2):
      """check for existence of matplotlib.pyplot and min matplotlib version
         (>= 2.2)

         return 0 if happy
      """
      # actual lib test
      plib = 'matplotlib.pyplot'
      rv = self.test_python_lib(plib, mesg='required', verb=verb)

      # if missing, we are done
      if rv:
         self.comments.append('python library matplotlib is required')
         self.comments.append(' (see AFNI install docs for details)')
         return 1

      # we have matplotlib, try to show and parse version
      warn = 1
      mver = self.get_ver_matplotlib()
      if mver == 'None':
         print("** failed to get matplotlib version")
      else:
         print("   matplotlib version : %s" % mver)
         try:
            vlist = mver.split('.')
            # if version is high enough, turn off warn
            if int(vlist[0]) > 2:
               warn = 0
            elif int(vlist[0]) == 2 and int(vlist[1]) >= 2:
               warn = 0
         except:
            print("** failed to check matplotlib version")

      if warn:
         wstr = 'need maptplotlib version 2.2+ for APQC'
         print("** %s\n" % wstr)
         self.comments.append('check for partial install of PyQt4')

   def get_ver_matplotlib(self):
      """simply return a matplotlib version string, and "None" on failure.
      """
      try:
         import matplotlib as MP
         ver = MP.__version__
      except:
         ver = 'None'

      return ver

   def test_python_lib_pyqt4(self, verb=2):
      # actual lib test
      libname = 'PyQt4'
      rv = MT.simple_import_test(libname, verb=verb)

      # if failure, no biggie for now
      if rv:
         print('-- %s is no longer needed for an AFNI bootcamp' % libname)
         return 1

      # it loads, but how about QtCore and QtGui
      if not MT.test_import(libname, verb=0):
         self.have_pyqt4 = 1

         # but check for partial install
         cmd = 'from PyQt4 import QtCore, QtGui'
         try:
            exec(cmd)
         except:
            print('\n** have PyQt4, but cannot load QtCore, QtGui; error is:' \
                  '\n\n'                                                      \
                  '   **************************************************')
            os.system('python -c "%s"' % cmd)
            print('   **************************************************\n')
            self.comments.append('check for partial install of PyQt4')
            self.have_pyqt4 = 0

            return 1

      return 0

   def test_python_lib(self, pylib, mesg='', verb=2):
      # actual lib test
      rv = MT.simple_import_test(pylib, verb=verb)

      if mesg : pmesg = mesg
      else:     pmesg = 'not required, but is desirable'

      # if failure, no biggie, but warn
      if rv:
         print('-- %s is %s' % (pylib, pmesg))
         return 1

      if pylib.startswith('matplotlib'):
         self.have_matplotlib = 1

      return 0

   def show_python_lib_info(self, header=1):

      # any extra libs to test beyone main ones
      # (empty for now, since matplotlib got its own function)
      extralibs = []
      verb = 3

      if header: print(UTIL.section_divider('python libs', hchar='-'))

      # itemize special libraries to test: PyQt4
      self.test_python_lib_pyqt4(verb=verb)
      print('')

      # itemize special libraries to test: matplotlib.pyplot
      self.test_python_lib_matplotlib(verb=verb)
      print('')

      # then go after any others
      for plib in extralibs:
         self.test_python_lib(plib, verb=verb)
         print('')

      for rootdir in ['/sw/bin', '/usr/local/bin']:
         # hard to avoid related files, so search for expected names
         pdirs = glob.glob('%s/python' % rootdir)
         p1dirs = glob.glob('%s/python[0-9]' % rootdir)
         p2dirs = glob.glob('%s/python[0-9].[0-9]' % rootdir)
         pdirs.extend(p1dirs)
         pdirs.extend(p2dirs)
         if len(pdirs) > 0:
            print('-- python binaries under %s:' % rootdir)
            for pdir in pdirs:
               if os.path.islink(pdir):
                  rstr = ' (sym link to %s)' % os.path.realpath(pdir)
               else: rstr = ''
               print('    %-20s%s' % (pdir, rstr))
            print('')

   def show_env_vars(self, header=1):
      print(UTIL.section_divider('env vars', hchar='-'))
      for evar in ['PATH', 'PYTHONPATH', 'R_LIBS',
                   'LD_LIBRARY_PATH',
                   'DYLD_LIBRARY_PATH', 'DYLD_FALLBACK_LIBRARY_PATH']:
         if evar in os.environ:
            if self.verb > 2: print("-- SEV: getting var from current env ...")
            print("%s = %s\n" % (evar, os.environ[evar]))
         elif evar.startswith('DY') and self.get_macos_ver() >= 11:
            if self.verb > 2:
               print("-- SEV: get DY var from macos child env (cur shell)...")
            s, so = self.get_shell_value(self.cur_shell, evar)
            print("%s (sub-shell) = %s" % (evar, so))
         else:
            if self.verb > 2:
               print("-- SEV: env var not set ...")
            print("%s = " % evar)
      print('')

      self.check_for_bash_complete_under_zsh()

   def check_for_bash_complete_under_zsh(self):
      """check for source of all_progs.COMP.bash in .zshrc and similar files

         whine if:
            all_progs.COMP.bash is referenced on a line before a comment char
      """
      badfile = 'all_progs.COMP.bash'
      goodfile = 'all_progs.COMP.zsh'
      for zfile in ['.zshrc', '.zshenv', '.zprofile']:
         zpath = '%s/%s' % (self.home_dir, zfile)

         # if the file is missing, skip
         if not os.path.isfile(zpath):
            continue

         # otherwise, search the lines of the found file
         lines = UTIL.read_text_file(zpath, lines=1, verb=0)

         for zline in lines:
            # note any bad file name or comment character
            badposn = zline.find(badfile)
            composn = zline.find('#')

            # if there is no bad text, skip line
            if badposn < 0:
               continue

            # if there is a comment character to the left of the bad word, skip
            if composn >= 0 and composn < badposn:
               continue

            # -----------------------------------------------------------
            # so we have found the bad file name without any comment char

            whine = 'reference to %s in %s, might comment or remove' \
                    % (badfile, zfile)
            self.comments.append(whine)

            if self.verb > 1:
               print("** %s" % whine)
               print("   (please reference %s, instead)" % goodfile)

               zcomp = '%s/.afni/help/%s' % (self.home_dir, goodfile)
               if not os.path.isfile(zcomp):
                  print("** also, missing %s" % zcomp)
                  print("   consider running: apsearch -update_all_afni_help")

            # finished whining, we are done with this file
            break

   def split_env_var(self, evar, sep=':'):
      """get env var and split on sep, returning list"""

      try: evalue = os.environ[evar]
      except: return []

      return evalue.split(sep)

   def show_general_afni_info(self, header=1):
      print(UTIL.section_divider('AFNI and related program tests', hchar='-'))

      self.afni_dir = self.get_afni_dir()
      check_list = ['afni', 'afni label', 'AFNI_version.txt',
                    'python', 'R', 'tcsh']
      nfound = self.check_for_progs(check_list, show_missing=1)
      if nfound < len(check_list):
         self.comments.append('failure under initial ' \
                              '"AFNI and related program tests"')

      # make generic but pretty
      print("instances of various programs found in PATH:")
      proglist = ['afni', 'R', 'python', 'python2', 'python3']
      ml = UTIL.max_len_in_list(proglist)
      for prog in proglist:
         rv, files = UTIL.search_path_dirs(prog, mtype=1)
         if not rv:
            if len(files) > 1:   fstr = '\n      '+'\n      '.join(files)
            elif len(files) > 0: fstr = '  (%s)' % files[0]
            else:                fstr = ''
            print('    %-*s : %d %s' % (ml, prog, len(files), fstr))

            if prog == 'afni':
               if len(files) > 1:
                  self.comments.append("have multiple versions of AFNI in PATH")
               if len(files) > 0:
                  if os.stat(files[0]).st_uid == 0:
                     self.comments.append("'afni' executable is owned by root")

      print('')

      # explicit python2 vs python3 check    7 Dec 2016
      n2 = UTIL.num_found_in_path('python2', mtype=1)
      n3 = UTIL.num_found_in_path('python3', mtype=1)
      if n3 > 0 and n2 <= 0:
         print("** have python3 but not python2")
      print('')

      # try select AFNI programs
      print('testing ability to start various programs...')
      ind = '%8s' % ' '
      indn = '\n%8s' % ' '
      proglist = ['afni', 'suma', '3dSkullStrip', 'uber_subject.py',
                   '3dAllineate', '3dRSFC', 'SurfMesh', '3dClustSim', '3dMVM']
      fcount = 0
      for prog in proglist:
         st, so, se = BASE.shell_exec2('%s -help'%prog, capture=1)
         # if 3dMVM, status will be 0 on failed library load (fix that, too)
         if prog == '3dMVM' and not st:
            mesg = ''.join(se)
            if mesg.find('Error in dyn.load') >= 0:
               st = 1
         if st:
            print('    %-20s : FAILURE' % prog)
            print(ind + indn.join(se))
            fcount += 1
         else:
            print('    %-20s : success' % prog)

            # no OpenMP problem
            if prog == '3dAllineate': self.ok_openmp = 1
      print('')
      pfailure = fcount == len(proglist)
      if fcount > 0:
         self.afni_fails = fcount
         self.comments.append('AFNI programs show FAILURE')

      # if complete failure, retry from exec dir
      ascdir = UTIL.executable_dir()
      if pfailure and self.afni_dir != ascdir:
         fcount = 0
         print('none working, testing programs under implied %s...' % ascdir)
         for prog in proglist:
            st, so, se = BASE.shell_exec2('%s/%s -help'%(ascdir,prog),capture=1)
            if st:
               print('    %-20s : FAILURE' % prog)
               print(ind + indn.join(se))
               fcount += 1
            else: print('    %-20s : success' % prog)
         print('')
         if fcount < len(proglist):
            self.comments.append('consider adding %s to your PATH' % ascdir)
      # if afni_dir is not set, use ascdir
      if self.afni_dir == '': self.afni_dir = ascdir

      print('checking for R packages...')
      cmd = 'rPkgsInstall -pkgs ALL -check'
      st, so, se = BASE.shell_exec2(cmd, capture=1)
      if st or len(se) < 2: okay = 0
      else:
         if se[1].startswith('++ Note:'): se = se[2:]
         okay = 1
         # do not require "verified", but fail on "not installed"
         # (to avoid failing on 'unknown timezone' warnings)
         for estr in se:
            if estr != '' and estr.find('not installed') >= 0:
               okay = 0   # any failure is terminal
               break
      if okay:
         print('    %-20s : success' % cmd)
      else:
         print('    %-20s : FAILURE' % cmd)
         print(ind + indn.join(se))
         self.comments.append('missing R packages (see rPkgsInstall)')
      print('')

      status, cout = UTIL.exec_tcsh_command("R RHOME")
      print('R RHOME : %s' % cout.strip())
      print('')

      print('checking for $HOME files...')
      flist = ['.afnirc', '.sumarc', '.afni/help/all_progs.COMP']
      for ff in flist:
         if os.path.isfile('%s/%s'%(self.home_dir, ff)): fstr = 'found'
         else:                                           fstr = 'missing'
         print('    %-25s : %s' % (ff, fstr))

      # add to comments
      if self.afni_dir:
         ccc = 'run: cp %s/AFNI.afnirc ~/.afnirc' % self.afni_dir
      else:
         ccc = 'copy AFNI.afnirc to ~/.afnirc'
      self.add_file_comment(None, '.afnirc', 'please %s' % ccc)

      self.add_file_comment(None, '.sumarc',
                            'please run: "suma -update_env" for .sumarc')
      self.add_file_comment(None, '.afni/help/all_progs.COMP',
                            'please run: apsearch -update_all_afni_help')

      print('')

   def add_file_comment(self, fdir, fname, comment):
      """if fname is not found in 'pre' dir, add comment
      """
      if   fdir == None: pre = '%s/' % self.home_dir
      elif fdir:         pre = '%s/' % fdir
      else:              pre = ''

      if not os.path.isfile('%s%s' % (pre, fname)):
         self.comments.append(comment)

   def get_python_ver_float(self):
      """just return the python version in A.B format
         (ignore lower order terms)
         return 0.0 on error
      """
      vstr = platform.python_version()
      try:
         posn = vstr.find('.')
         if posn > 0:
            posn = vstr.find('.', posn+1)
            pvs = vstr[0:posn]
         else:
            pvs = vstr
         vf = float(pvs)
      except:
         vf = 0.0

      return vf

   def get_prog_version(self, prog):
      """return a simple string with program version

         each program might be handled a little differently
         ** but a common method might be PROG --version

         return status and string
            - status > 0 means to use, == 0 means not, < 0 means error
      """
      if prog == 'afni':
         cmd = 'afni -ver'
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=2)
         if s:
            if len(se) > 0: return 1, se[0]
            else:           return 1, ''
         if len(so) > 1:
            off1 = so[1].find('[[')
            off2 = so[1].find(']]')
            if off1 >= 0 and off2 >= 0: return 1, so[1][off1+2:off2]
            else: return 1, so[1]
         else:
            off1 = so[0].find('(')
            if off1 > 0:
               vstr = so[0][0:off1]
               off2 = so[0].find('AFNI_')
               if off2 > off1:
                  ll = so[0][off2:]
                  self.afni_label = ll.split(')')[0]
            else:
               vstr = so[0]
            return 1, vstr

      elif prog == 'python':
         return 1, platform.python_version()

      elif prog == 'tcsh':      # no version
         return 0, ''

      elif prog == 'port':      # no dashes for version
         cmd = '%s version' % prog
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=1)
         if s: return 1, se[0]
         else: return 1, so[0]

      elif prog in ['XQuartz', 'X11']:
         droot = '/Applications/Utilities'
         pname = '%s.app' % prog
         app   = '%s/%s' % (droot, pname)
         dstr  = ''

         # get version string
         vstr = self.get_kmdi_version(app)

         # on failure, check the other app
         if vstr == '':
            if prog == 'XQuartz': pname = 'X11.app'
            else:                 pname = 'XQuartz.app'
            dstr  = '(%s) ' % pname
            app = '%s/%s' % (droot, pname)
            vstr = self.get_kmdi_version(app)
            # clear on failure
            if vstr == '': dstr = ''

         # some vesions are not considered good
         if self.check_xquartz_version(vstr, warn=1): 
            print("  ** for macos install instructions, see:\n\n    %s\n" \
                  % g_site_install_mac)

         return 1, (dstr+vstr)

      elif prog in ['dnf', 'yum', 'apt-get', 'brew', 'port', 'fink', 'R']:
         cmd = '%s --version' % prog
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=1)
         if s: return 1, se[0]
         else: return 1, so[0]

      else:
         print('** no version method for prog : %s' % prog)
         return -1, ''

   def check_xquartz_version(self, vstr, warn=1):
      """bad versions:
             <= 2.6*
             == 2.8.0_alpha*
             == 2.8.0_beta[12] (so 3+ is good)

         return 0 on ok, 1 on bad version or problem
      """
      # -------- set vnlist and vtype, and fail on errors
      try:
         vlist = vstr.split('_')
         vnlist = [int(v) for v in vlist[0].split('.')]
      except:
         print("** failed to parse X version string, '%s'" % vstr)
         self.comments.append("strange XQuartz version: %s" % vstr)
         return 1

      if len(vnlist) != 3:
         print("** failed to parse X version levels in '%s'" % vstr)
         self.comments.append("strange XQuartz version: %s" % vstr)
         return 1

      # and note any sub-type, e.g. 'alpha*'
      if len(vlist) < 2:
         vtype = ""
      else:
         vtype = vlist[1]

      # -------- check for major version 2, (warn if less)
      if vnlist[0] < 2:
         self.comments.append("strange XQuartz major version: %s" % vstr)
         return 1

      if vnlist[0] > 2:
         return 0

      # -------- so the major version is 2

      # check minor version 7 or 8
      if vnlist[1] < 7:
         estr = "early XQuartz version, but might be okay: %s" % vstr
         self.comments.append(estr)
         return 0

      if vnlist[1] == 7:
         return 0

      # now only worry about 2.8.0, can expand later
      if vnlist[1] > 8 or vnlist[2] > 0:
         return 0

      # -------- now have 2.8.0, check vtype

      if vtype == "":
         return 0

      if vtype.startswith('alpha'):
         estr = "XQuartz is an alpha release, and should be updated"
         print("** %s" % estr)
         self.comments.append(estr)
         return 1

      if vtype.startswith('beta'):
         # get trailer
         btype = vtype[4:]

         # see if there is a number attached
         if btype in ['1', '2']:
            estr = "XQuartz is an early beta release, and should be updated"
            print("** %s" % estr)
            self.comments.append(estr)
            return 1

         # beta 3+ (or empty?) is okay, for now
         return 0

      # currently another future condition: neither alpha nor beta
      return 0

   def get_kmdi_version(self, path, verb=1):
      """for the given program, run: mdls -name kMDItemVersion

         return found version string, or an empty one on failure
      """

      cmd = 'mdls -name kMDItemVersion %s' % path
      s, soe = UTIL.exec_tcsh_command(cmd, lines=1)

      # return on failure
      if len(soe) == 0: return ''

      rstr = soe[0]
      rposn = rstr.find('kMDItemVersion')
      if rposn < 0: return ''
      
      rlist = rstr[rposn:].split()
      if verb > 1: print("-- kmdi_v: list is %s" % rlist)
      
      if len(rlist) < 3: return ''
      if rlist[0] != 'kMDItemVersion' or rlist[1] != '=':
         return ''

      # found something, remove any quotes, tabs and spaces, and return
      rstr = rlist[2].strip('\'" \t')

      return rstr
   
   def get_cpu_count(self):
       """
       Number of virtual or physical CPUs on this system, i.e. user/real as
       output by time(1) when called with an optimally scaling userspace-only
       program.

       from phihag @ stackoverflow.com, partially via other users
       """
       import re,subprocess

       # SLURM cluster (e.g., Biowulf)
       if 'SLURM_JOB_CPUS_PER_NODE' in os.environ:
           return os.environ['SLURM_JOB_CPUS_PER_NODE']
       if 'SLURM_CPUS_PER_TASK' in os.environ:
           return os.environ['SLURM_CPUS_PER_TASK']

       # Python 2.6+
       try:
           import multiprocessing
           return multiprocessing.cpu_count()
       except (ImportError,NotImplementedError):
           pass

       # http://code.google.com/p/psutil/
       try:
           import psutil
           return psutil.NUM_CPUS
       except (ImportError, AttributeError):
           pass

       # POSIX
       try:
           res = int(os.sysconf('SC_NPROCESSORS_ONLN'))
           if res > 0: return res
       except (AttributeError,ValueError):
           pass

       # Linux
       try:
           res = open('/proc/cpuinfo').read().count('processor\t:')
           if res > 0: return res
       except IOError:
           pass

       # Solaris
       try:
           pseudoDevices = os.listdir('/devices/pseudo/')
           expr = re.compile('^cpuid@[0-9]+$')
           res = 0
           for pd in pseudoDevices:
               if expr.match(pd) != None: res += 1
           if res > 0: return res
       except OSError:
           pass

       # BSD
       try:
           sysctl = subprocess.Popen(['sysctl', '-n', 'hw.ncpu'],
                                         stdout=subprocess.PIPE)
           scStdout = sysctl.communicate()[0]
           res = int(scStdout)
           if res > 0: return res
       except (OSError, ValueError):
           pass

       # Other UNIXes (heuristic)
       try:
           try:
               dmesg = open('/var/run/dmesg.boot').read()
           except IOError:
               dmesgProcess = subprocess.Popen(['dmesg'],
                                               stdout=subprocess.PIPE)
               dmesg = dmesgProcess.communicate()[0]
           res = 0
           while '\ncpu' + str(res) + ':' in dmesg:
               res += 1
           if res > 0: return res
       except OSError:
           pass

       # Windows
       try:
           res = int(os.environ['NUMBER_OF_PROCESSORS'])
           if res > 0: return res
       except (KeyError, ValueError):
           pass

       # jython
       try:
           from java.lang import Runtime
           runtime = Runtime.getRuntime()
           res = runtime.availableProcessors()
           if res > 0: return res
       except ImportError:
           pass

       print('** Can not determine number of CPUs on this system')
       return 1

   def show_comments(self):
      # check for a good result, first
      if len(self.comments) == 0:
          print(UTIL.section_divider(' nothing to fix, yay! ', hchar='='))
          print('')
          return

      print(UTIL.section_divider(' summary, please fix: ', hchar='='))
      for cc in self.comments: 
         if len(cc) == 0: print('')
         else:
            if cc[0] == ' ': print('  %s' % cc)
            else:            print('*  %s' % cc)
      print('')

   def show_all_sys_info(self):

      self.show_general_sys_info()
      self.show_general_afni_info()
      self.show_python_lib_info()
      self.show_env_vars()
      self.show_data_info()
      self.show_os_specific()

      self.show_comments()

# non-class functions

def tup_str(some_tuple):
   """just listify some string tuple"""
   return ' '.join(list(some_tuple))

def distribution_string():
   """check distributions by type - now a bit messy"""
   import platform
   sysname = platform.system()
   checkdist = 0        # set in any failure case
   dstr = 'bad pizza'
   dtest = 'NOT SET'

   if sysname == 'Linux':
      # through python 3.7 (if they still use mac_ver, why not linux?)
      try: dstr = tup_str(platform.linux_distribution())
      except: 
         try:
            # python 3.4+
            import distro
            dtest = distro.linux_distribution(full_distribution_name=False)
            dstr = tup_str(dtest)
         except:
            # deprecated since 2.6, but why not give it a try?
            try:
               dtest = platform.dist()
               dstr = tup_str(dtest)
            except:
               checkdist = 1
      # backup plan for linux checkdist
      if checkdist:
         dstr = linux_dist_from_os_release()
         if dstr != '':
            checkdist = 0
   elif sysname == 'Darwin':
      try:
         dtest = platform.mac_ver()
         dstr = tup_str(dtest)
      except:
         try:
            dtest = list(platform.mac_ver())
            dstr = dtest[0]
            if type(dstr) != str:
               checkdist = 1
            elif dstr == '':
               checkdist = 1
         except:
            checkdist = 1
   else:
      try:
         dtest = platform.dist()
         dstr = tup_str(dtest)
      except: checkdist = 1

   # backup plan
   if checkdist:
      dstr = 'unknown %s (%s)' % (sysname, dtest)

   return dstr

def linux_dist_from_os_release():
   """try to form a useful OS version string from /etc/os-release
   """
   release_files = ['/etc/os-release']
   for rfile in release_files:
      dstr = linux_dist_from_rfile(rfile)
      if dstr != '':
         return dstr

   return 'LDFOR: bad pizza'
   
def linux_dist_from_rfile(rfile):
   """try to form a useful OS version string from given file,
      e.g., /etc/os-release
   """
   rv, td = UTIL.read_text_dictionary(rfile, mjdiv='=', mndiv='=', compact=1,
                                      qstrip=1)  
   if rv:
      return ''

   # okay, we have a dictionary, what is in it?

   # try single options first
   singles = ['PRETTY_NAME']
   for sname in singles:
      if sname in td:
         return td[sname]
   
   # try to build something
   dstr = ''
   if 'NAME' in td:
      dstr += td['NAME']
   if 'VERSION' in td:
      dstr += td['VERSION']

   if dstr != '': return dstr

   if 'ID' in td:
      dstr += td['ID']
   if 'VERSION_ID' in td:
      dstr += td['VERSION_ID']

   if dstr != '': return dstr

   if 'PLATFORM' in td:
      dstr += td['PLATFORM']
   if 'PLATFORM_ID' in td:
      dstr += td['PLATFORM_ID']

   # return whatever we have
   return dstr

if __name__ == '__main__':
   print('lib_system_check.py: not intended as a main program')

