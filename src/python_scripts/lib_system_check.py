#!/usr/bin/env python

# library for performing various system checks

import os, sys
import module_test_lib as MT

# test dependency libs before proceeding
# note: platform came with python 2.3
testlibs = ['platform', 'afni_base', 'afni_util']
if MT.num_import_failures(testlibs):
   sys.exit(1)

import platform
import afni_base as BASE
import afni_util as UTIL

class SysInfo:
   """system info class"""

   def __init__(self):

      self.system          = platform.system()
      self.home_dir        = os.environ['HOME']

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
      if   self.system == 'Linux':
         try:    dstr = tostr(platform.linux_distribution())
         except: checkdist = 1
      elif self.system == 'Darwin':
         try: dstr = tostr(platform.mac_ver())
         except: checkdist = 1
      else: checkdist = 1
      if checkdist: dstr = tostr(platform.dist())
      print 'distribution:         %s' % dstr
         
      print 'number of CPUs:       %s' % self.get_cpu_count()

      # note shell, and if we are not in login shell
      logshell = UTIL.get_login_shell()
      curshell = UTIL.get_current_shell()
      if logshell == curshell: note = ''
      else:                    note = '  (current shell is %s)' % curshell

      print 'apparent login shell: %s%s' % (logshell, note)
      print

   def show_python_lib_info(self, plibs, header=1, verb=2):
      if header: print UTIL.section_divider('python libs', hchar='-')
      for lib in plibs: MT.test_import(lib, verb=verb)
      print

   def show_path_vars(self, header=1):
      print UTIL.section_divider('path vars', hchar='-')
      for evar in ['PATH', 'PYTHON_PATH',
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
            if s: print '%-20s : %s' % ('%s version'%prog, v)
      print

      # make generic but pretty
      print "instances of various programs found in PATH:"
      proglist = ['afni', 'R', 'python']
      ml = UTIL.max_len_in_list(proglist)
      for prog in proglist:
         rv, files = UTIL.search_path_dirs(prog, exact=1)
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
                   '3dAllineate']:
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

