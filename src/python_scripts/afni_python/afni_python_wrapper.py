#!/usr/bin/env python

# python3 status: started

# afni_python_wrapper.py : interface for calling python libs from the shell
#                          (based on afni_util.py)

import sys, os

# ======================================================================
# get the main import done here

# ------------------------------------------------------------
# only proceed with imports in the context of main,
# in case this happens to imported
if __name__ == '__main__':
   argv = sys.argv
   narg = len(argv)

   # decide which module to import (default is afni_util)
   argbase = 1
   module = 'afni_util'

   # ------------------------------------------------------------
   # if enough options are passed, choose which module to import
   if narg > 2:
      if '-module' == argv[argbase]:
         module = argv[argbase+1]
         argbase += 2

   # terminal option: module is set, see if user wants a listing
   # if -module_dir, import differently
   if '-module_dir' in argv:
      try:
         exec('import %s as MM' % module)
      except:
         try:
            exec('import afni_python.%s as MM' % module)
         except:
            print("** failed to import module '%s'" % module)
            sys.exit(1)

      print('dir(%s):\n   %s' % (module, '\n   '.join(dir(MM))))
      sys.exit(0)

   # ------------------------------------------------------------
   # if there are options to process, actually try to import a module

   if narg > 2:
      # import the module in question and proceed
      # (if this fails, try to import from afni_python)
      try:
         exec('from %s import *' % module)
      except:
         try:
            exec('from afni_python.%s import *' % module)
         except:
            print("** failed to import module '%s'" % module)
            sys.exit(1)

   # we have a module imported, or plan help.n.exit, proceed...


# ----------------------------------------------------------------------

_g_main_help = """
afni_python_wrapper.py: use to call afni_python functions from the shell

   By default, this loads module afni_util, and attempts use functions from it.
   To use a funtion from a different module, apply -module.

   MODULE will subsequently to the imported module.


   options:

      -help             : show this help

      -module MODULE    : specify the python module to import

         By default, functions to process are imported from afni_util.  To
         import a different module, apply this option.

         Example:

            afni_python_wrapper.py -module afni_base ...

      -module_dir       : show the elements returned by dir()

         This option is useful to get a list of all module functions.

         Examples:

            afni_python_wrapper.py -module_dir
            afni_python_wrapper.py -module afni_base -module_dir

      -eval STRING      : evaluate STRING in context of MODULE
                          (i.e. STRING can be function calls or other)

         This option is used to simply execute the code in STRING.

         Examples for eval:

            afni_python_wrapper.py -eval "show_process_stack()"
            afni_python_wrapper.py -eval "show_process_stack(verb=2)"
            afni_python_wrapper.py -eval "show_process_stack(pid=1000)"

            # display out.ss_review.FT.txt as a json dictionary
            afni_python_wrapper.py -eval                  \\
                'write_data_as_json(read_text_dictionary( \\
                "out.ss_review.FT.txt")[1])'
            afni_python_wrapper.py -eval                  \\
                'write_data_as_json(read_text_dictionary( \\
                "out.ss_review.FT.txt", compact=1)[1])'

      -exec STRING      : execute STRING in context of MODULE

         This option is used to simply execute the code in STRING.

         Examples for exec:

            afni_python_wrapper.py -exec "y = 3+4 ; print y"
            afni_python_wrapper.py -exec "import PyQt4"
            afni_python_wrapper.py -exec "show_process_stack()"

      -funchelp FUNC    : print the help for module function FUNC

         Pring the FUNC.__doc__ text, if any.

         Example:

            afni_python_wrapper.py -funchelp wrap_file_text

      -print STRING     : print the result of executing STRING

         Akin to -eval, but print the results of evaluating STRING.

         Examples for print:

            afni_python_wrapper.py                      \\
                -print "get_last_history_ver_pack('DSET+tlrc')"
            afni_python_wrapper.py                      \\
                -print "get_last_history_version('DSET+tlrc')"
            afni_python_wrapper.py -print 'gaussian_at_fwhm(3,5)'
            afni_python_wrapper.py -print 'gaussian_at_hwhm_frac.__doc__'

      -lprint STRING    : line print: print result list, one element per line

         The 'l' stands for 'line' (or 'list').  This is akin to -print,
         but prints a list with one element per line.

      -listfunc [SUB_OPTS] FUNC LIST ... : execute FUNC(LIST)

         With this option, LIST is a list of values to be passed to FUNC().
         Note that LIST can be simply '-' or 'stdin', in which case the
         list values are read from stdin.

         This is similar to eval, but instead of requiring:
            -eval "FUNC([v1,v2,v3,...])"
         the list values can be left as trailing arguments:
            -listfunc FUNC v1 v2 v3 ...
         (where LIST = v1 v2 v3 ...).

         SUB_OPTS sub-options:

                -float  : convert the list to floats before passing to FUNC()
                -print  : print the result
                -join   : print the results join()'d together

         Examples for listfunc:

           afni_python_wrapper.py -listfunc        min_mean_max_stdev 1 2 3 4 5
           afni_python_wrapper.py -listfunc -print min_mean_max_stdev 1 2 3 4 5
           afni_python_wrapper.py -listfunc -join  min_mean_max_stdev 1 2 3 4 5

           afni_python_wrapper.py -listfunc -join -float demean 1 2 3 4 5

           afni_python_wrapper.py -listfunc -join shuffle       \\
                                  `count -digits 4 1 124`

           count -digits 4 1 124 | afni_python_wrapper.py -listfunc \\
                                  -join shuffle -

           afni_python_wrapper.py -listfunc glob2stdout 'EPI_run1/8*'

           afni_python_wrapper.py -listfunc -joinc list_minus_glob_form *HEAD

           afni_python_wrapper.py -listfunc -join -float linear_fit \\
                                  2 3 5 4 8 5 8 9


         Also, if LIST contains -list2, then 2 lists can be input to do
         something like:
            -eval "FUNC([v1,v2,v3], [v4,v5,v6])"

         Examples with -list2:

            afni_python_wrapper.py -listfunc -print -float ttest 1 2 3 4 5 \\
                                                 -list2 2 2 4 6 8

            afni_python_wrapper.py -listfunc -print -float ttest_paired   \\
                                1 2 3 4 5 -list2 2 4 5 6 8

            afni_python_wrapper.py -listfunc -join -float linear_fit      \\
                                `cat y.1D` -list2 `cat x.1D`

   Author: R Reynolds  Feb, 2020  (moved from afni_util.py)
"""

def process_listfunc(argv, argbase=1):
   """see the -help description"""

   if argv[argbase] != '-listfunc': return 1

   if len(argv) <= (2+argbase) :
      print('** -listfunc usage requires at least 3 args')
      return 1

   do_join = 0
   do_joinc = 0 # join with commas
   do_float = 0
   do_print = 0
   argbase += 1

   while argv[argbase] in ['-join', '-joinc', '-print', '-float']:
      if argv[argbase] == '-join':
         do_join = 1
         argbase += 1
      elif argv[argbase] == '-joinc':
         do_joinc = 1
         argbase += 1
      elif argv[argbase] == '-print':
         do_print = 1
         argbase += 1
      elif argv[argbase] == '-float':
         do_float = 1
         argbase += 1
      else: break # should not happen

   # note function
   func = eval(argv[argbase])

   # get args, and check for -list2
   # (allow for - to read all data into array)
   args1 = argv[argbase+1:]
   if len(args1) == 1:
      if args1[0] == '-' or args1[0] == 'stdin':
         fvals = read_text_file()
         args1 = []
         for fv in fvals: args1.extend(fv.split())

   args2 = []
   if '-list2' in args1:
      l2ind = args1.index('-list2')
      args2 = args1[l2ind+1:]
      args1 = args1[0:l2ind]

   if do_float:
      try: vals1 = [float(v) for v in args1]
      except:
         print('** list1 is not all float')
         return 1
      try: vals2 = [float(v) for v in args2]
      except:
         print('** list2 is not all float')
         return 1
   else:
      vals1 = args1
      vals2 = args2

   if len(vals2) > 0: ret = func(vals1, vals2)
   else:              ret = func(vals1)
   
   if   do_join:  print(' '.join(str(v) for v in ret))
   elif do_joinc: print(','.join(str(v) for v in ret))
   elif do_print: print(ret)
   # else do nothing special
   return 0

def show_function_help(flist):
   import afni_util as UTIL
   for func in flist:
      print(UTIL.section_divider('help for: %s' % func))
      try:
         fn = eval(func)
         print(fn.__doc__)
      except:
         print("** not a valid function '%s'" % func)

def process_args():
   argv = sys.argv
   if '-help' in argv or len(argv) < 2:
      print(_g_main_help)
      return 0

   global argbase

   # ------------------------------------------------------------
   # main module has already been imported in global scope

   # ------------------------------------------------------------
   if argv[argbase] == '-eval':
      eval(' '.join(argv[argbase+1:]))
      return 0
   elif argv[argbase] == '-exec':
      exec(' '.join(argv[argbase+1:]))
      return 0
   elif argv[argbase] == '-funchelp':
      import afni_util as UTIL
      for func in argv[argbase+1:]:
         print(UTIL.section_divider('help for: %s' % func))
         try:
            fn = eval(func)
            print(fn.__doc__)
         except:
            print("** not a valid function '%s'" % func)
      # show_function_help(argv[argbase+1:])
      return 0
   elif argv[argbase] == '-lprint':
      ret = eval(' '.join(argv[argbase+1:]))
      print('\n'.join(['%s'%rent for rent in ret]))
      return 0
   elif argv[argbase] == '-print':
      print(eval(' '.join(argv[argbase+1:])))
      return 0
   elif argv[argbase] == '-listfunc':
      return process_listfunc(argv, argbase)
   else:
      print('** please see "afni_python_wrapper.py -help" for usage')
      return 1


if __name__ == '__main__':
   argv = sys.argv
   narg = len(argv)
   if '-help' in argv or narg < 2:
      print(_g_main_help)
      sys.exit(0)

   # ------------------------------------------------------------
   # from here on, we need at least 2 options
   if narg <= 2:
      print(_g_main_help)
      sys.exit(0)

   # we have a module imported, proceed...
   sys.exit(process_args())
