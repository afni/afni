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

   def show_data_info(self, header=1):
      """checks that are specific to data
            - class data existence and tree root
               - assume $HOME if not found
            - disk space under data root
               - maybe check for mounted file system
            - atlases (maybe just @Find TT_N27+tlrc?)
      """

      if header: print UTIL.section_divider('data checks', hchar='-')

      for ddir in ['AFNI_data6', 'suma_demo']:
          if self.show_data_dir(ddir): break

      evar = 'AFNI_ATLAS_DIR'
      if os.environ.has_key(evar):
         print "atlas var %s = %s" % (evar, os.environ[evar])
      atlas = 'TT_N27+tlrc'
      cmd = '@FindAfniDsetPath %s' % atlas
      s, so, se = UTIL.limited_shell_exec(cmd, nlines=1)
      if not s:
         print 'found atlas %s under %s' % (atlas, so[0])

      print

   def show_data_dir(self, ddir):
      """check for and show the existence of ddir in common locations
         return status (0 = success)
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
      if dpath == None:
         print '** did not find data dir %s' % ddir
         return 1
      else:
         dpath = '%s/%s' % (root_str, dpath[len(hdir)+1:])
         print 'found data dir %s' % dpath
         return 0

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
      return dlist[0]

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
      pass

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
      print

      ind = '%8s' % ' '
      indn = '\n%8s' % ' '
      print 'testing ability to start various programs...'
      for prog in ['afni', 'suma', '3dSkullStrip', 'uber_subject.py',
                   '3dAllineate', '3dRSFC', 'SurfMesh']:
         st, so, se = BASE.shell_exec2('%s -help'%prog, capture=1)
         if st:
            print '    %-20s : FAILURE' % prog
            print ind + indn.join(se)
         else: print '    %-20s : success' % prog

      print

   def get_prog_version(self, prog):
      """return a simple string with program version

         each program might be handled a little differently

         return status and string
            - status > 0 means to use, == 0 means not, < 0 means error
      """
      if prog == 'afni':
         cmd = 'afni -ver'
         s, so, se = UTIL.limited_shell_exec(cmd, nlines=2)
         if s: return 1, se[0]
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


if __name__ == '__main__':
   print 'lib_system_check.py: not intended as a main program'

