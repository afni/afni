#!/usr/bin/env python

# python3 status: started

# ---------------------------------------------------------------------------
# These are routines for checking whether libraries can be imported
# (and getting python version).
#
# Consider starting a python file (as library or main program) with
# something like the following:
#
#       import sys
#       from afnipy import module_test_lib
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

import os, sys
IL = None

# add 'R' if needed
genlibs  = ['os', 'sys', 'string', 'glob', 'copy', 'gc', 'time', 'webbrowser']
mathlibs = ['math', 'random', 'numpy', 'scipy']
guilibs  = ['PyQt4', 'wx', 'matplotlib']

alllibs  = []
alllibs.extend(genlibs)
alllibs.extend(mathlibs)
alllibs.extend(guilibs)


# ---------------------------------------------------------------------------
# library failure message list (for select library packages)
# 
# These install descriptions are old.  Just mention the libraries for now.
#                                                       16 Sep 2016
g_mesglist = [
             [ 'numpy', '' ],
#"""
#     -> consider downloading 'numpy' from www.scipy.org
#           or on Linux: "sudo yum install numpy"
#           or on OS X:  "sudo fink install numpy-py25"
#""" ],
             [ 'matplotlib', '' ],
#"""
#     -> consider downloading 'matplotlib' from matplotlib.sourceforge.net
#           or on Linux: "sudo yum install python-matplotlib"
#           or on OS X:  "sudo fink install matplotlib-py25"
#""" ],
             [ 'R', '' ],
#"""
#     -> consider downloading 'R' from a site in the mirror list:
#        http://cran.r-project.org/mirrors.html
#           or on Linux: "sudo yum install R"
#
#        consider also installing the developer sub-package 'R-devel'
#           "sudo yum install R-devel"
#""" ],
             [ 'scipy', '' ],
#"""
#     -> consider downloading 'scipy' from www.scipy.org
#           or on Linux: "sudo yum install scipy"
#           or on OS X:  "sudo fink install scipy-py25"
#""" ],
             [ 'PyQt4','' ],
#"""
#     -> please see the output of "uber_subject.py -help_install"
#""" ],
             [ 'wx', '' ],
#"""
#     -> consider downloading 'wxpython' from www.wxpython.org
#           or on Linux: "sudo yum install wxPython"
#           or on OS X:  "sudo fink install wxpython-py25"
#                        Note: wxpython-py25 is not available on OS X 10.4
#""" ]
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

def compare_py_ver(vstr):
   """return -1, 0, 1 comparing the current version to input vstr
   """
   # get current and input version lists, as ints
   pvc = [int(v) for v in list(get_py_ver())]
   pvi = [int(v) for v in vstr.split('.')]

   lenc = len(pvc)
   leni = len(pvi)
   
   dmin = min(lenc,leni)

   # check up to where they are equal
   for dind in range(dmin):
      if pvc[dind] < pvi[dind]: return -1
      if pvc[dind] > pvi[dind]: return  1

   # if still equal return the longer list
   if lenc < leni: return -1
   if lenc > leni: return  1

   # else equal
   return 0

# not for general use: return message for libname
def _get_mesg(libname):
   for mpair in g_mesglist:
      if mpair[0] == libname:
         return mpair[1]
   return ''


# function definition string for if we are running python 3.12+
# (actually okay for 3.4+)
import_find_test_312_def =       \
"""
def import_find_test_312(libname, details=1, verb=1):
   # return loaded library or None (on failure)
   import importlib

   if verb > 3: print("-- running test_312 on library %s" % libname)

   try:    mod = sys.modules[libname]
   except: pass
   else:
      if verb>1: print("++ module already loaded: %s" % libname)
      return mod

   try:
      spec = importlib.util.find_spec(libname)
   except:
      if verb > 0:
         if details: mesg = _get_mesg(libname)
         else:       mesg = ''
         if mesg:
            print("---------------------------------------------------------")
         print("** python module not found: %s" % libname)
         if mesg: print(mesg)
            
      return None

   if verb>2: print("++ module '%s' found at %s" % (libname, spec.origin))

   # continue and try to load it (just use import)
   mod = None
   try: mod = importlib.import_module(libname)
   except:
      if verb>0: print("** failed to load module: %s" % libname)
      mod = None  # be sure of return value
   else:
      if verb>1: print("++ module loaded: %s" % (libname))

   return mod
"""


# function definition string for if we are running python 2.5 +
# (this version produces a syntax error in 2.4.x python)
# (old string method, forget python 2.4) import_find_test_25_def =     \
import_find_test_25_def =       \
"""
def import_find_test_25(libname, details=1, verb=1):
   # return loaded library or None (on failure)
   import imp

   try:    mod = sys.modules[libname]
   except: pass
   else:
      if verb>1: print("++ module already loaded: %s" % libname)
      return mod

   try: fp, pname, desc = imp.find_module(libname)
   except:
      if verb > 0:
         if details: mesg = _get_mesg(libname)
         else:       mesg = ''
         if mesg:
            print("---------------------------------------------------------")
         print("** python module not found: %s" % libname)
         if mesg: print(mesg)
            
      return None

   if verb>2: print("++ module '%s' found at %s" % (libname, pname))

   # continue and try to load it
   mod = None
   try: mod = imp.load_module(libname, fp, pname, desc)
   except:
      if verb>0: print("** failed to load module: %s" % libname)
      mod = None  # be sure of return value
   else:
      if verb>1: print("++ module loaded: %s" % (libname))
   finally:
      if fp:
         if verb>3: print("-- close file for module: %s" % libname)
         fp.close()
   return mod
"""


# function definition string for if we are running python 2.4.x
# (the 2.5 version is a syntax error in 2.4.x python)
import_find_test_24_def =       \
"""
def import_find_test_24(libname, details=1, verb=1):
   # return loaded library or None (on failure)

   import imp

   try:    mod = sys.modules[libname]
   except: pass
   else:
      if verb>1: print("++ module already loaded: %s" % libname)
      return mod

   try: fp, pname, desc = imp.find_module(libname)
   except:
      if verb > 0:
         if details: mesg = _get_mesg(libname)
         else:       mesg = ''
         if mesg:
            print("---------------------------------------------------------")
         print("** python module not found: %s" % libname)
         if mesg: print(mesg)
      return None

   if verb>2: print("++ module '%s' found at %s" % (libname, pname))

   # continue and try to load it
   mod = None
   try:
      try: mod = imp.load_module(libname, fp, pname, desc)
      except:
         if verb>0: print("** failed to load module: %s" % libname)
         mod = None  # be sure of return value
      else:
         if verb>1: print("++ module loaded: %s" % (libname))
   finally:
      if fp:
         if verb>3: print("-- close file for module '%s'" % libname)
         fp.close()

   return mod
"""

def simple_import(libname, details=1, verb=1):
   # return loaded library or None (on failure)

   global IL

   old_python = (get_py_ver_float() < 2.7)

   if IL is None:
      try:
         import importlib as LLL
         IL = LLL
      except:
         # failure, unless this is old python
         if not old_python:
            print("** simple_import_test: failed to get importlib to test %s" \
                  % libname)
            return None

   # exec waaaay simple backup plan for older python without importlib
   # --> simply attempt to import
   if old_python:
      try:
         exec('import %s as mod' % libname)
         if verb > 1:
            print("++ module loaded: %s" % libname)
      except:
         if verb:
             print("** failed to load module %s" % libname)
         return None
   else:
      try:
         mod = IL.import_module(libname)
         if verb > 1:
            print("++ module loaded: %s" % libname)
      except:
         if verb:
             print("** failed to load module %s" % libname)
         return None

   if details and verb > 1:
      if hasattr(mod, '__file__'):
         print("   module file : %s" % mod.__file__)
      elif hasattr(mod, '__path__'):
         print("   module path : %s" % mod.__path__)

   return mod

def get_version(libname, vfield='__version__',verb=0):
   """if this is an importable library, return the LIB.vfield element
      return 'NONE' on error
   """
   rstr = 'NONE'
   if verb > 0: details = 1
   else:        details = 0
   mod = simple_import(libname, details=details, verb=verb)
   if mod is None:
      return rstr

   # return the given attribute
   if hasattr(mod, vfield):
      return getattr(mod, vfield)

   return rstr

def simple_import_test(libname, details=1, verb=1):
   """test whether the library can be imported, return 0 on success
   """
   mod = simple_import(libname, details=details, verb=verb)
   if mod is None: return 1  # failure
   else:           return 0  # success

def test_import(libname, details=1, verb=1):
   """try to import a single library, specified as a string
        e.g. if test_import('numpy'): print('** failure')

        details: if libname is in g_mesglist, print(message)
      return 0 on success, 1 on failure"""

   # run test based on python version
   # break as less than 2.5, less than 3.10, and otherwise
   if compare_py_ver('2.5') < 0: # use 2.4 version (might fail)
      exec(import_find_test_24_def)
      imptest = eval('import_find_test_24')
   elif compare_py_ver('3.10') < 0: # might go up to 3.12, but overlap
      exec(import_find_test_25_def)
      imptest = eval('import_find_test_25')
   else:
      exec(import_find_test_312_def)
      imptest = eval('import_find_test_312')

   if imptest(libname, details, verb): return 0
   else:                               return 1

def num_import_failures(liblist=[], details=1, verb=1):
   """try to import a list of library names as strings

        liblist: e.g. ['wx', 'numpy']
        details: if libname is in g_mesglist, print(message)
      return the number of failures (0=success)"""

   # note python version
   pv = get_py_ver_float()

   if verb > 2: print('-- python version %s' % pv)

   if pv < 2.5: # use 2.4 version (might fail)
      exec(import_find_test_24_def)
      imptest = eval('import_find_test_24')
   else:        # use 2.5 version
      
      if liblist: libs = liblist
      else:       libs = alllibs
   
      errs = 0
      for lib in libs:
         # use simple_import to handle A.B form
         rlib = simple_import(lib, details, verb)
         if rlib == None:
            errs += 1

   return errs

