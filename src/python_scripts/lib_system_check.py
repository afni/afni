#!/usr/bin/env python

# library for performing various system checks

import os, sys
import module_test_lib as MT

# test dependency libs before proceeding
# note: platform came with python 2.3
testlibs = ['platform', 'afni_base', 'afni_util']
if MT.num_import_failures(testlibs):
   sys.exit(1)

import platform, glob
import afni_base as BASE
import afni_util as UTIL

class SysInfo:
   """system info class"""

   def __init__(self, data_root='', verb=1):

      self.system          = platform.system()
      self.home_dir        = os.environ['HOME']
      self.data_root       = data_root
      self.verb            = verb

      self.afni_ver        = ''
      self.afni_dir        = ''
      self.os_dist         = ''
      self.rc_file         = ''
      self.comments        = [] # comments to print at the end
      self.afni_fails      = 0

      self.repo_prog       = '' # e.g. yum or brew
      self.have_pyqt4      = 0
      self.ok_openmp       = 0  # does 3dAllineate work, for example?

   def get_afni_dir(self):
      s, so, se = BASE.simple_shell_exec('which afni', capture=1)
      if s: return ''
      adir = so.strip()
      if adir[-5:] == '/afni': return adir[0:-5]
      else:                    return ''

   def show_general_sys_info(self, header=1):
      if header: print UTIL.section_divider('general', hchar='-')

      def tostr(some_tuple):
         if type(some_tuple) == str: return some_tuple
         tlist = [t for t in list(some_tuple) if type(t) == str]
         return ' '.join(tlist)

      print 'architecture:         %s' % tostr(platform.architecture())
      print 'system:               %s' % platform.system()
      print 'release:              %s' % platform.release()
      print 'version:              %s' % platform.version()

      # check distributions by type
      checkdist = 0
      dstr = ''
      if   self.system == 'Linux':
         try:    dstr = tostr(platform.linux_distribution())
         except: checkdist = 1
      elif self.system == 'Darwin':
         try: dstr = tostr(platform.mac_ver())
         except: checkdist = 1
      else: checkdist = 1
      if checkdist: dstr = tostr(platform.dist())
      self.os_dist = dstr       # save result
      print 'distribution:         %s' % dstr
         
      print 'number of CPUs:       %s' % self.get_cpu_count()

      # note shell, and if we are not in login shell
      logshell = UTIL.get_login_shell()
      curshell = UTIL.get_current_shell()
      if logshell == curshell: note = ''
      else:                    note = '  (current shell is %s)' % curshell

      if logshell not in ['csh', 'tcsh']:
         self.comments.append("login shell '%s', trusting user to translate" \
                              " from 'tcsh'" % logshell)

      print 'apparent login shell: %s%s' % (logshell, note)

      self.set_shell_rc_file([logshell, curshell])
      if self.home_file_exists(self.rc_file): fstr = 'exists'
      else:                                   fstr = 'does not exist'
      print 'shell RC file:        %s (%s)' % (self.rc_file, fstr)
      print

   def set_shell_rc_file(self, slist):
      """and many any useful comments"""

      cc = []
      self.rc_file = 'NONE'

      if 'sh' in slist:
         # non-login shell ref: NONE
         # login shell ref: .profile"
         fname = '.profile'
         self.rc_file = fname
         if os.path.isfile('%s/%s' % (self.home_dir,fname)):
            cc.append("shell sh  : found login shell setup file %s" % fname)
         else: 
            cc.append("shell sh  : MISSING login shell setup file %s" % fname)
         
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
            st, so, se = UTIL.limited_shell_exec("\grep %s %s"%(f2name,f1name))
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

      self.comments.extend(cc)

   def home_file_exists(self, fname):
      return(os.path.isfile('%s/%s' % (self.home_dir, fname)))

   def show_top_line(self, fname, prefix='', last=0):
      htxt = UTIL.read_top_lines(fname, nlines=1, strip=1, verb=0)
      if len(htxt) == 0: htxt = 'NONE FOUND'
      else:              htxt = htxt[0]
      # possibly truncate to '...' and final 'last' characters
      if last > 0 and len(htxt) > last:
         htxt = '...' + htxt[-last:]
      print '%s%s' % (prefix, htxt)

   def show_data_dir_info(self, ddir, histfile=''):
      """try to locate and display the given data directory
         if histfile, display the top line

         return 0 if found, else 1
      """

      status, droot = self.find_data_root(ddir, hvar=0)
      if status:
         print 'data dir : missing %s' % ddir
         return 1

      # have a directory, show it
      dhome = droot.replace(self.home_dir, '$HOME')
      print 'data dir : found %-12s under %s' % (ddir, dhome)

      # possibly show histfile
      if histfile == '': return 0

      prefix = '           top history: '
      hname = '%s/%s/%s' % (droot, ddir, histfile)
      self.show_top_line(hname, prefix=prefix, last=76-len(prefix))

      return 0

   def show_data_info(self, header=1):
      """checks that are specific to data
            - class data existence and tree root
               - assume $HOME if not found
            - disk space under data root
               - maybe check for mounted file system
            - atlases (maybe just @Find TT_N27+tlrc?)
      """

      if header: print UTIL.section_divider('data checks', hchar='-')

      # locate various data trees, and possibly show recent history
      rv = 0
      rv += self.show_data_dir_info('AFNI_data6', 'history.txt')
      rv += self.show_data_dir_info('suma_demo', 'README.archive_creation')
      rv += self.show_data_dir_info('FATCAT_DEMO', 'README.timestamp')
      rv += self.show_data_dir_info('afni_handouts')

      if rv: self.comments.append('insufficient data for AFNI bootcamp')

      evar = 'AFNI_ATLAS_DIR'
      tryenv = 0                        # might suggest setting evar
      haveenv = os.environ.has_key(evar)
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
         print 'atlas    : did not find %s' % atlas
         self.comments.append('possibly missing atlases')
      else:
         for ddir in glist:
            print 'atlas    : found %-12s under %s' % (atlas, ddir)

      if haveenv: print "\natlas var: %s = %s" % (evar, edir)

      print

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
      if self.verb > 3: print '-- found %s dirs %s' % (ddir, dlist)
      dlist = UTIL.get_unique_sublist(dlist)
      if self.verb > 2: print '-- found trimmed %s dirs %s' % (ddir, dlist)
      
      if len(dlist) == 0: return None
      dlen = len(ddir)+1
      return dlist[0][0:-dlen]

   def show_os_specific(self, header=1):
      """checks that are specific to one OS or another"""

      if self.system not in ['Linux', 'Darwin']: return

      if header: print UTIL.section_divider('OS specific', hchar='-')

      if   self.system == 'Linux':  self.show_spec_linux()
      elif self.system == 'Darwin': self.show_spec_osx()

      print

   def show_spec_linux(self):
      """linux specific checks
            - is Ubuntu
               - has Ubuntu AFNI installed
               - atlas directory
      """

      # check for repositories
      self.check_for_progs(['dnf', 'yum', 'apt-get'], repos=1)

      if self.os_dist.find('buntu') >= 0:
         print 'have Ubuntu system: %s' % self.os_dist
         if self.afni_ver.find('buntu') >= 0:
            print 'have Ubuntu afni  : %s' % self.afni_ver

      # add PyQt4 comment, if missing
      if not self.have_pyqt4:
         repo = self.repo_prog
         package = ''
         if repo in ['yum', 'dnf']: package = 'PyQt4'
         elif repo == 'apt-get': package = 'python-qt4'
         
         if package != '':
            self.comments.append('consider running: %s install %s' \
                                 % (repo, package))
         else:
            self.comments.append('consider installing PyQt4')

   def show_spec_osx(self):
      """look for fink, macports, homebrew, PyQt4"""

      # check for repositories
      nfound = self.check_for_progs(['brew', 'port', 'fink'], repos=1)
      if nfound == 0:
         self.comments.append('consider installing homebrew')
      self.hunt_for_homebrew()
      if self.get_osx_ver() < 7:
         self.comments.append('OS X version might be old')

      # add PyQt4 comment, if missing (check for brew and fink packages)
      if not self.have_pyqt4:
         glist = glob.glob('/usr/local/lib/python2*/site-packages/PyQt4')
         if len(glist) == 0:
            glist = glob.glob('/sw/lib/qt4*/lib/python2*/site-packages/PyQt4')
         if len(glist) > 0:
            gdir = glist[-1]
            ghead = os.path.dirname(gdir)
            print '++ found PyQt4 under %s' % ghead
            self.comments.append('consider adding %s to PYTHONPATH' % ghead)
         elif self.repo_prog == 'brew':
            self.comments.append('consider running: brew install pyqt')
         else:
            self.comments.append('consider installing PyQt4')

      self.check_for_pre_11_dylib()

      # in 10.11, check for gcc under homebrew
      self.check_for_10_11_lib('libgomp.1.dylib', wpath='gcc/*/lib/gcc/*')
      self.check_for_10_11_lib('libglib-2.0.dylib', wpath='glib/*/lib')
      self.check_for_flat_namespace()

   def hunt_for_homebrew(self):
      """assuming it was not found, just look for the file"""
      # if already found, do not bother
      if self.repo_prog == 'brew': return 0 

      bdir = '/usr/local/bin'
      bfile = 'brew'
      bpath = '%s/%s' % (bdir,bfile)
      if os.path.isfile(bpath):
         print "++ found '%s' at %s" % (bfile, bpath)
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
      osver = self.get_osx_ver()
      if osver < 7 or osver > 10:
         return

      # count AFNI dylib files
      dfiles = glob.glob('%s/*.dylib' % self.afni_dir)
      nadylib = len(dfiles)

      # if set, check if any dylibs exist
      fvar = 'DYLD_FALLBACK_LIBRARY_PATH'
      if not os.environ.has_key(fvar):
         print '** AFNI program failures and DYLD_FALLBACK_LIBRARY_PATH not set'
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
            print '** no dylib files under %s directory' % fvar
         if fvar != self.afni_dir and nadylib > 0:
            self.comments.append('consider changing DYLD_FALLBACK_LIBRARY_PATH'\
                                 ' to abin, e.g.\n   '                        \
                                 'setenv DYLD_FALLBACK_LIBRARY_PATH %s'       \
                                 % self.afni_dir)
         elif fvar != self.afni_dir:
            self.comments.append('not sure about DYLD_FALLBACK_LIBRARY_PATH')


   def check_for_10_11_lib(self, libname, wpath='gcc/*/lib/gcc/*'):
      """in 10.11, check for library under homebrew

         wpath = wildcard path to library name

         return 0 if no issue was detected
      """
      # if no homebrew, do not bother
      if self.repo_prog != 'brew':
         return 0

      # require 10.11, unless being verbose
      if self.get_osx_ver() < 11 and self.verb <= 1:
         return 0

      sname   = wpath.split('/')[0]    # short name, e.g. gcc
      libdir  = '/usr/local/lib'
      libpath = '%s/%s' % (libdir, libname)

      croot = '/usr/local/Cellar'
      clibs = glob.glob('%s/%s/%s' % (croot, wpath, libname))
      clibs.sort(reverse=True)
      # first check for any homebrew gomp libraries, at all
      if len(clibs) == 0:
         self.comments.append('consider installing %s under homebrew'%sname)
         return 1

      # if the library exists (as link or file), we are good to go
      if os.path.exists(libpath):
         if os.path.islink(libpath):
	    lname = os.readlink(libpath)
            print '++ found valid link %s\n   to %s' % (libpath, lname)
         else:
            print '++ found existent library %s' % libpath
         return 0

      # ** does not exist: so either no link or a bad one **

      # if no link, suggest making one
      if not os.path.islink(libpath):
         self.comments.append('consider linking %s under %s'%(clibs[0],libdir))
         return 1

      # huston, we have a bad link, say something useful
      print '** bad link %s, probably to old version' % libpath
      print '   --> points to missing %s' % os.readlink(libpath)
      print '   --> consider instead: %s' % clibs[0]
      print '   for example:\n' \
            '       rm -f %s\n' \
            '       ln -s %s %s' % (libpath, clibs[0], libpath)
      self.comments.append('consider fixing link %s \n   to point to %s'\
                           %(libpath,clibs[0]))

      return 1

   def check_for_flat_namespace(self, fnames=['libXt']):
      """note whether /opt/X11/lib/flat_namespace exists and is non-empty

         in particular, check for any libraries in fnames list

         return 0 if no error was detected
      """

      # require 10.9, unless being verbose
      if self.get_osx_ver() < 9 and self.verb <= 1:
         return 0

      flatdir = '/opt/X11/lib/flat_namespace'

      # if the directory exists and is non-empty, note it
      flibs = glob.glob('%s/*dylib*' % flatdir)
      # first check for any homebrew gomp libraries, at all
      if len(flibs) > 0:
         print "++ found %d dylib files under '%s'" % (len(flibs), flatdir)
      else:
         if self.verb > 1: print '-- no flat_namespace libraries exist'
         return 0

      found = 0
      for name in fnames:
         flibs = glob.glob('%s/%s*dylib*' % (flatdir, name))
         if len(flibs) > 0:
            print "   -- found '%s' dylib files, e.g. %s" % (name, flibs[0])
            found += 1

      # if no libraries are found, we are done
      if not found:
         if self.verb > 1: print '-- no checked flat_namespace libraries found'
         return 0

      # so there is something here that we might care about

      edir = 'DYLD_LIBRARY_PATH'
      if flatdir in self.split_env_var(edir):
         print '++ yay, env var %s contains %s' % (edir, flatdir)
      elif os.environ.has_key(edir):
         print '** env var %s does not contain %s' % (edir, flatdir)
         print '   (so afni and suma might fail)'
         self.comments.append('consider appending %s with %s' % (edir,flatdir))
      else:
         print '** env var %s is not set to contain %s' % (edir, flatdir)
         print '   (so afni and suma may fail)'
         if self.get_osx_ver() >= 11:
            self.comments.append('cannot tell if %s includes %s'%(edir,flatdir))
         else:
            self.comments.append('consider setting %s to %s' % (edir, flatdir))

      return 1

   def get_osx_ver(self):
      if self.system != "Darwin": return 0
      verlist = self.os_dist.split()
      if len(verlist) < 1: return 0
      verlist = verlist[0].split('.')
      if len(verlist) < 2: return 0
      if verlist[0] != '10': return 0
      try: vint = int(verlist[1])
      except: return 0

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
            print '%-20s : %s' % (prog, vinfo)
            continue

         cmd = 'which %s' % prog
         s, so, se = BASE.simple_shell_exec(cmd, capture=1)
         if not s: # found one
            # if we do not yet know of a repo program, mark as this one
            if repos and self.repo_prog == '': self.repo_prog = prog

            print '%-20s : %s' % (cmd, so.strip())
            s, v = self.get_prog_version(prog)
            if s: print '%-20s : %s' % ('%s version'%prog, v)

            if prog == 'afni': self.afni_ver = v # save result
            nfound += 1
         elif show_missing:
            print '%-20s : %s' % (cmd, se)

      print

      return nfound

   def show_python_lib_info(self, plibs, header=1, verb=2):
      if header: print UTIL.section_divider('python libs', hchar='-')
      for lib in plibs: MT.test_import(lib, verb=verb)
      # explicitly note whether we have PyQt4
      if not MT.test_import('PyQt4', verb=0):
         self.have_pyqt4 = 1

         # check for partial install
         cmd = 'from PyQt4 import QtCore, QtGui'
         try: exec cmd
         except:
            print '\n** have PyQt4, but cannot load QtCore, QtGui; error is:' \
                  '\n\n'                                                      \
                  '   **************************************************'
            os.system('python -c "%s"' % cmd)
            print '   **************************************************\n'
            self.comments.append('check for partial install of PyQt4')
      print

      pdirs = glob.glob('/sw/bin/python*')
      if len(pdirs) > 0: pdirs = [dd for dd in pdirs if dd.find('config')<0]
      if len(pdirs) > 0:
         print 'python binaries under /sw/bin:'
         for pdir in pdirs:
            if os.path.islink(pdir):
               rstr = ' (sym link to %s)' % os.path.realpath(pdir)
            else: rstr = ''
            print '    %-20s%s' % (pdir, rstr)
         print

   def show_path_vars(self, header=1):
      print UTIL.section_divider('env vars', hchar='-')
      for evar in ['PATH', 'PYTHONPATH', 'R_LIBS',
                   'LD_LIBRARY_PATH',
                   'DYLD_LIBRARY_PATH', 'DYLD_FALLBACK_LIBRARY_PATH']:
         if os.environ.has_key(evar):
            print "%s = %s\n" % (evar, os.environ[evar])
         else:
            print "%s = " % evar
      print

   def split_env_var(self, evar, sep=':'):
      """get env var and split on sep, returning list"""

      try: evalue = os.environ[evar]
      except: return []

      return evalue.split(sep)

   def show_general_afni_info(self, header=1):
      print UTIL.section_divider('AFNI and related program tests', hchar='-')

      self.afni_dir = self.get_afni_dir()
      check_list = ['afni', 'AFNI_version.txt', 'python', 'R', 'tcsh']
      nfound = self.check_for_progs(check_list, show_missing=1)
      if nfound < len(check_list):
         self.comments.append('failure under initial ' \
                              '"AFNI and related program tests"')

      # make generic but pretty
      print "instances of various programs found in PATH:"
      proglist = ['afni', 'R', 'python', 'python2', 'python3']
      ml = UTIL.max_len_in_list(proglist)
      for prog in proglist:
         rv, files = UTIL.search_path_dirs(prog, mtype=1)
         if not rv:
            if len(files) > 1:   fstr = '\n      '+'\n      '.join(files)
            elif len(files) > 0: fstr = '  (%s)' % files[0]
            else:                fstr = ''
            print '    %-*s : %d %s' % (ml, prog, len(files), fstr)

            if prog == 'afni':
               if len(files) > 1:
                  self.comments.append("have multiple versions of AFNI in PATH")
               if len(files) > 0:
                  if os.stat(files[0]).st_uid == 0:
                     self.comments.append("'afni' executable is owned by root")
      print

      # explicit python2 vs python3 check    7 Dec 2016
      n2 = UTIL.num_found_in_path('python2', mtype=1)
      n3 = UTIL.num_found_in_path('python3', mtype=1)
      if n3 > 0 and n2 <= 0:
         self.comments.append("have python3 but not python2")
      print

      # try select AFNI programs
      print 'testing ability to start various programs...'
      ind = '%8s' % ' '
      indn = '\n%8s' % ' '
      proglist = ['afni', 'suma', '3dSkullStrip', 'uber_subject.py',
                   '3dAllineate', '3dRSFC', 'SurfMesh']
      fcount = 0
      for prog in proglist:
         st, so, se = BASE.shell_exec2('%s -help'%prog, capture=1)
         if st:
            print '    %-20s : FAILURE' % prog
            print ind + indn.join(se)
            fcount += 1
         else:
            print '    %-20s : success' % prog

            # no OpenMP problem
            if prog == '3dAllineate': self.ok_openmp = 1
      print
      pfailure = fcount == len(proglist)
      if fcount > 0:
         self.afni_fails = fcount
         self.comments.append('AFNI programs show FAILURE')

      # if complete failure, retry from exec dir
      ascdir = UTIL.executable_dir()
      if pfailure and self.afni_dir != ascdir:
         fcount = 0
         print 'none working, testing programs under implied %s...' % ascdir
         for prog in proglist:
            st, so, se = BASE.shell_exec2('%s/%s -help'%(ascdir,prog),capture=1)
            if st:
               print '    %-20s : FAILURE' % prog
               print ind + indn.join(se)
               fcount += 1
            else: print '    %-20s : success' % prog
         print
         if fcount < len(proglist):
            self.comments.append('consider adding %s to your PATH' % ascdir)
      # if afni_dir is not set, use ascdir
      if self.afni_dir == '': self.afni_dir = ascdir

      print 'checking for R packages...'
      cmd = 'rPkgsInstall -pkgs ALL -check'
      st, so, se = BASE.shell_exec2(cmd, capture=1)
      if st or len(se) < 2: okay = 0
      else:
         if se[1].startswith('++ Note:'): se = se[2:]
         okay = 1
         # require every subsequent string to say verified
         for estr in se:
            if estr != '' and estr.find('has been verified') < 0:
               okay = 0   # any failure is terminal
               break
      if okay:
         print '    %-20s : success' % cmd
      else:
         print '    %-20s : FAILURE' % cmd
         print ind + indn.join(se)
         self.comments.append('missing R packages (see rPkgsInstall)')
      print

      print 'checking for $HOME files...'
      flist = ['.afnirc', '.sumarc', '.afni/help/all_progs.COMP']
      for ff in flist:
         if os.path.isfile('%s/%s'%(self.home_dir, ff)): fstr = 'found'
         else:                                           fstr = 'missing'
         print '    %-25s : %s' % (ff, fstr)

      # add to comments
      if self.afni_dir:
         ccc = 'running: cp %s/AFNI.afnirc ~/.afnirc' % self.afni_dir
      else:
         ccc = 'copying AFNI.afnirc to ~/.afnirc'
      self.add_file_comment(None, '.afnirc', 'consider %s' % ccc)

      self.add_file_comment(None, '.sumarc',
                            'consider running "suma -update_env" for .sumarc')
      self.add_file_comment(None, '.afni/help/all_progs.COMP',
                            'consider running: apsearch -update_all_afni_help')

      print

   def add_file_comment(self, fdir, fname, comment):
      """if fname is not found in 'pre' dir, add comment
      """
      if   fdir == None: pre = '%s/' % self.home_dir
      elif fdir:         pre = '%s/' % fdir
      else:              pre = ''

      if not os.path.isfile('%s%s' % (pre, fname)):
         self.comments.append(comment)

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
            if off1 > 0: return 1, so[0][0:off1]
            else:        return 1, so[0]

      elif prog == 'python':
         return 1, platform.python_version()

      elif prog == 'tcsh':      # no version
         return 0, ''

      elif prog in ['dnf', 'yum', 'apt-get', 'brew', 'port', 'fink', 'R']:
         cmd = '%s --version' % prog
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=1)
         if s: return 1, se[0]
         else: return 1, so[0]

      else:
         print '** no version method for prog : %s' % prog
         return -1, ''

   def get_cpu_count(self):
       """
       Number of virtual or physical CPUs on this system, i.e. user/real as
       output by time(1) when called with an optimally scaling userspace-only
       program.

       from phihag @ stackoverflow.com, partially via other users
       """
       import re,subprocess

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
               dmesgProcess = subprocess.Popen(['dmesg'], stdout=subprocess.PIPE)
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

       print '** Can not determine number of CPUs on this system'
       return 1

   def show_comments(self):
      print UTIL.section_divider('summary comments', hchar='=')
      for cc in self.comments: 
         if len(cc) == 0: print ''
         else:
            if cc[0] == ' ': print '  %s' % cc
            else:            print '*  %s' % cc
      print ''

   def show_all_sys_info(self):

      self.show_general_sys_info()
      self.show_general_afni_info()
      self.show_python_lib_info(['PyQt4'], verb=3)
      self.show_path_vars()
      self.show_data_info()
      self.show_os_specific()

      self.show_comments()

if __name__ == '__main__':
   print 'lib_system_check.py: not intended as a main program'

