#!/usr/bin/env python

# ---------------------------------------------------------------------------
# These are routines for checking whether libraries can be imported
# (and getting python version).
#
# Consider starting a python file (as library or main program) with
# something like the following:
#
#       import sys
#       import module_test_lib
#       g_testlibs = ['numpy', 'wx', 'matplotlib', 'scipy']
#       if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
#
# --------------------------------------------------
#
# main functions:
#
#   get_py_ver()          : return list of version indices: like ['2','4','1']
#                           e.g. verlist = get_py_ver()
#
#   get_py_ver_float()    : return python version as float: like 2.4
#                           e.g. if get_py_ver_float() < 2.5: PANIC()
#
#   num_import_failures() : return the number of failures importing libraries
#                           e.g. num_errs = test_libs(['sys','os','R','scipy'])
#
#   test_import()         : test a single library
#                           e.g. if test_import('numpy'): print '** failure'
#
# R Reynolds 28 Oct 2008
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# todo:
#
# - fromlibs = { 'scipy': ['linalg', 'signal', 'stats'], 'fish': ['tuna'] }
# - add to 

import imp, sys

# add 'R' if needed
genlibs  = ['os', 'sys', 'string', 'glob', 'copy', 'gc']
mathlibs = ['math', 'random', 'numpy', 'scipy']
guilibs  = ['wx', 'matplotlib']

alllibs  = []
alllibs.extend(genlibs)
alllibs.extend(mathlibs)
alllibs.extend(guilibs)


# ---------------------------------------------------------------------------
# library failure message list (for select library packages)
g_mesglist = [
             [ 'numpy', 
"""
     -> consider downloading 'numpy' from www.scipy.org
           or on Linux: "sudo yum install numpy"
           or on OS X:  "sudo fink install numpy-py25"
""" ],
             [ 'matplotlib', 
"""
     -> consider downloading 'matplotlib' from matplotlib.sourceforge.net
           or on Linux: "sudo yum install python-matplotlib"
           or on OS X:  "sudo fink install matplotlib-py25"
""" ],
             [ 'R', 
"""
     -> consider downloading 'R' from a site in the mirror list:
        http://cran.r-project.org/mirrors.html
           or on Linux: "sudo yum install R"

        consider also installing the developer sub-package 'R-devel'
           "sudo yum install R-devel"
""" ],
             [ 'scipy', 
"""
     -> consider downloading 'scipy' from www.scipy.org
           or on Linux: "sudo yum install scipy"
           or on OS X:  "sudo fink install scipy-py25"
""" ],
             [ 'wx', 
"""
     -> consider downloading 'wxpython' from www.wxpython.org
           or on Linux: "sudo yum install wxPython"
           or on OS X:  "sudo fink install wxpython-py25"
                        Note: wxpython-py25 is not available on OS X 10.4
""" ]
]



# ---------------------------------------------------------------------------
# main functions

def get_py_ver():
   "return the python version as a list, allow for older versions"
   vlist = ['0', '0']
   try:
      import platform
      vlist = platform.python_version_tuple()
   except:
      if sys.version[0] == '1':
         import string
         ver = string.split(sys.version)
         vlist = string.split(ver[0], '.')
      else:
         ver = sys.version.split()
         vlist = ver[0].split('.')

   return vlist

def get_py_ver_float():
   pv = get_py_ver()
   if pv[0] == '1': return float('%c.%c' % (pv[0],pv[1]))
   else:            return float('%s.%s' % (pv[0],pv[1]))

# not for general use: return message for libname
def _get_mesg(libname):
   for mpair in g_mesglist:
      if mpair[0] == libname:
         return mpair[1]
   return ''


# function definition string for if we are running pythong 2.5 +
# (this version produces a syntax error in 2.4.x python)
import_find_test_25_def =       \
"""def import_find_test_25(libname, details=1, verb=1):
   # return loaded library or None (on failure)

   try:    mod = sys.modules[libname]
   except: pass
   else:
      if verb>1: print "++ module already loaded: %s" % libname
      return mod

   try: fp, pname, desc = imp.find_module(libname)
   except:
      if verb > 0:
         if details: mesg = _get_mesg(libname)
         else:       mesg = ''
         if mesg:
            print "---------------------------------------------------------"
         print "** python module not found: %s" % libname
         if mesg: print mesg
            
      return None

   if verb>2: print "++ module '%s' found at %s" % (libname, pname)

   # continue and try to load it
   mod = None
   try: mod = imp.load_module(libname, fp, pname, desc)
   except:
      if verb>0: print "** failed to load module: %s" % libname
      mod = None  # be sure of return value
   else:
      if verb>1: print "++ module loaded: %s" % (libname)
   finally:
      if fp:
         if verb>3: print "-- close file for module: %s" % libname
         fp.close()
   return mod
"""

# function definition string for if we are running pythong 2.4.x
# (the 2.5 version is a syntax error in 2.4.x python)
import_find_test_24_def =       \
"""def import_find_test_24(libname, details=1, verb=1):
   # return loaded library or None (on failure)

   try:    mod = sys.modules[libname]
   except: pass
   else:
      if verb>1: print "++ module already loaded: %s" % libname
      return mod

   try: fp, pname, desc = imp.find_module(libname)
   except:
      if verb > 0:
         if details: mesg = _get_mesg(libname)
         else:       mesg = ''
         if mesg:
            print "---------------------------------------------------------"
         print "** python module not found: %s" % libname
         if mesg: print mesg
      return None

   if verb>2: print "++ module '%s' found at %s" % (libname, pname)

   # continue and try to load it
   mod = None
   try:
      try: mod = imp.load_module(libname, fp, pname, desc)
      except:
         if verb>0: print "** failed to load module: %s" % libname
         mod = None  # be sure of return value
      else:
         if verb>1: print "++ module loaded: %s" % (libname)
   finally:
      if fp:
         if verb>3: print "-- close file for module '%s'" % libname
         fp.close()

   return mod
"""

def test_import(libname, details=1, verb=1):
   """try to import a single library, specified as a string
        e.g. if test_import('numpy'): print '** failure'

        details: if libname is in g_mesglist, print message
      return 0 on success, 1 on failure"""

   # note python version
   pv = get_py_ver_float()

   if pv < 2.5: # use 2.4 version (might fail)
      exec(import_find_test_24_def)
      imptest = import_find_test_24
   else:        # use 2.5 version
      exec(import_find_test_25_def)
      imptest = import_find_test_25

   if imptest(libname, details, verb): return 0
   else:                               return 1

def num_import_failures(liblist=[], details=1, verb=1):
   """try to import a list of library names as strings

        liblist: e.g. ['wx', 'numpy']
        details: if libname is in g_mesglist, print message
      return the number of failures (0=success)"""

   # note python version
   pv = get_py_ver_float()

   if verb > 2: print '-- python version %s' % pv

   if pv < 2.5: # use 2.4 version (might fail)
      exec(import_find_test_24_def)
      imptest = import_find_test_24
   else:        # use 2.5 version
      exec(import_find_test_25_def)
      imptest = import_find_test_25

   if liblist: libs = liblist
   else:       libs = alllibs

   errs = 0
   for lib in libs:
      rlib = imptest(lib, details, verb)
      if not rlib: errs = errs + 1

   return errs

