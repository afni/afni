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
      self.os_dist         = ''
      self.comments        = [] # comments to print at the end

   def get_afni_dir(self):
      s, so, se = BASE.simple_shell_exec('which afni', capture=1)
      if s: return ''
      return so.strip()

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

      print 'apparent login shell: %s%s' % (logshell, note)
      print

   def show_top_line(self, fname, prefix=''):
      htxt = UTIL.read_top_lines(fname, nlines=1, strip=1, verb=0)
      if len(htxt) == 0: htxt = 'NONE FOUND'
      else:              htxt = htxt[0]
      print '%s%s' % (prefix, htxt)

   def show_data_dir_info(self, ddir, histfile=''):
      """try to locate and display the given data directory
         if histfile, display the top line
      """

      status, droot = self.find_data_root(ddir, hvar=0)
      if status:
         print 'data dir : missing %s' % ddir
         return

      # have a directory, show it
      dhome = droot.replace(self.home_dir, '$HOME')
      print 'data dir : found %-12s under %s' % (ddir, dhome)

      # possibly show histfile
      if histfile == '': return

      prefix = '           top history: '
      hname = '%s/%s/%s' % (droot, ddir, histfile)
      self.show_top_line(hname, prefix=prefix)

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
      self.show_data_dir_info('AFNI_data6', 'history.txt')
      self.show_data_dir_info('suma_demo', 'README.archive_creation')

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
      else: glist.append(so[0])

      for ddir in ['/usr/share/afni/atlases', '/usr/local/afni/atlases']:
         if os.path.isfile('%s/%s.HEAD'%(ddir,atlas)):
            glist.append(ddir)
            if tryenv:
               self.comments.append('consider setting %s to %s' % (evar,ddir))

      # fix to work with found after the fact
      glist = UTIL.get_unique_sublist(glist)

      if len(glist) == 0:
         print 'atlas    : did not find %s' % atlas
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
      elif self.system == 'Darwin': self.show_spec_mac()

      print

   def show_spec_linux(self):
      """linux specific checks
            - is Ubuntu
               - has Ubuntu AFNI installed
               - atlas directory
      """

      if self.os_dist.find('buntu') < 0: return

      print 'have Ubuntu system: %s' % self.os_dist
      if self.afni_ver.find('buntu') >= 0:
         print 'have Ubuntu afni  : %s' % self.afni_ver

   def show_spec_mac(self):
      """look for fink, macports, homebrew"""
      # if no pyqt4, check for brew and fink packages
      if MT.test_import('PyQt4', verb=0):
         glist = glob.glob('/usr/local/lib/python2*/site-packages/PyQt4')
         if len(glist) == 0:
            glist = glob.glob('/sw/lib/qt4*/lib/python2*/site-packages/PyQt4')
         if len(glist) > 0:
            gdir = glist[-1]
            ghead = os.path.dirname(gdir)
            print '++ found PyQt4 under %s' % ghead
            print '   (consider adding %s to PYTHONPATH)' % ghead

   def show_python_lib_info(self, plibs, header=1, verb=2):
      if header: print UTIL.section_divider('python libs', hchar='-')
      for lib in plibs: MT.test_import(lib, verb=verb)
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
      print UTIL.section_divider('path vars', hchar='-')
      for evar in ['PATH', 'PYTHONPATH',
                   'LD_LIBRARY_PATH',
                   'DYLD_LIBRARY_PATH', 'DYLD_FALLBACK_LIBRARY_PATH']:
         if os.environ.has_key(evar): print "%s = %s\n" % (evar, os.environ[evar])
         else: print "%s = " % evar
      print

   def show_general_afni_info(self, header=1):
      print UTIL.section_divider('AFNI and related program tests', hchar='-')
      for prog in ['afni', 'python', 'R', 'tcsh']:
         cmd = 'which %s' % prog
         s, so, se = BASE.simple_shell_exec(cmd, capture=1)
         if s: print '%-20s : %s' % (cmd, se)
         else:
            print '%-20s : %s' % (cmd, so.strip())
            s, v = self.get_prog_version(prog)
            if s:
                print '%-20s : %s' % ('%s version'%prog, v)
                if prog == 'afni': self.afni_ver = v # save result
      print

      # make generic but pretty
      print "instances of various programs found in PATH:"
      proglist = ['afni', 'R', 'python']
      ml = UTIL.max_len_in_list(proglist)
      for prog in proglist:
         rv, files = UTIL.search_path_dirs(prog, mtype=1)
         if not rv:
            if len(files) > 1:   fstr = '\n      '+'\n      '.join(files)
            elif len(files) > 0: fstr = '  (%s)' % files[0]
            else:                fstr = ''
            print '    %-*s : %d %s' % (ml, prog, len(files), fstr)

            if prog == 'afni' and len(files) > 1:
               self.comments.append("consider only 1 version of AFNI in PATH")
      print

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
         else: print '    %-20s : success' % prog
      print

      ascdir = UTIL.executable_dir()
      if fcount == len(proglist) and self.get_afni_dir() != ascdir:
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

      print 'checking for $HOME files...'
      flist = ['.afnirc', '.sumarc', '.afni/help/all_progs.COMP']
      for ff in flist:
         if os.path.isfile('%s/%s'%(self.home_dir, ff)): fstr = 'found'
         else:                                           fstr = 'missing'
         print '    %-25s : %s' % (ff, fstr)

      # add to comments
      self.add_file_comment(None, '.afnirc',
                            'consider copying AFNI.afnirc to ~/.afnirc')
      self.add_file_comment(None, '.sumarc',
                            'consider running "suma -update_env" for .sumarc')
      self.add_file_comment(None, '.afni/help/all_progs.COMP',
                            'consider running "apsearch -update_all_afni_help"')

      print

   def add_file_comment(self, fdir, fname, comment):
      if   fdir == None: pre = '%s/' % self.home_dir
      elif fdir:         pre = '%s/' % fdir
      else:              pre = ''

      if not os.path.isfile('%s%s' % (pre, fname)):
         self.comments.append(comment)

   def get_prog_version(self, prog):
      """return a simple string with program version

         each program might be handled a little differently

         return status and string
            - status > 0 means to use, == 0 means not, < 0 means error
      """
      if prog == 'afni':
         cmd = 'afni -ver'
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=2)
         if s:
            if len(se) > 0: return 1, se[0]
            else:           return 1, ''
         off1 = so[1].find('[[')
         off2 = so[1].find(']]')
         if off1 >= 0 and off2 >= 0: return 1, so[1][off1+2:off2]
         else: return 1, so[1]

      elif prog == 'python':
         return 1, platform.python_version()

      elif prog == 'R':
         cmd = 'R --version'
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=1)
         if s: return 1, se[0]
         else: return 1, so[0]

      elif prog == 'tcsh':      # no version
         return 0, ''

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
      print UTIL.section_divider('general comments', hchar='-')
      for cc in self.comments: print '*  %s' % cc
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

