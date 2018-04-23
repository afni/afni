#!/usr/bin/env python

# system libraries
import sys, os

if 1 :  # for testing, might add the current dir and ~/abin to the PATH
   try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
   except: pass

# AFNI libraries
import option_list as OL
import afni_util as UTIL        # not actually used, but probably will be

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
eg_main_chrono.py - a sample main python program, to run on top of some library

                  o  lib_1D.py is used for demonstration
                  o  most options are processed chronologically

------------------------------------------

   terminal options:

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    March 2009
=============================================================================
"""

g_history = """
   eg_main_chrono.py history:

   0.0  Mar 18, 2009    - initial version
   0.1  Mar 19, 2009    - added -verbose_opts
   0.2  Apr 11, 2009    - removed -verbose_opts (see -optlist_ options)
"""

g_version = "eg_main_chrono.py version 0.2, Apr 11, 2009"


class MyInterface:
   """interface class for MyLibrary (whatever that is)
     
      This uses lib_1D.py as an example."""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # timing variables
      self.ad              = None

      # general variables
      self.verb            = verb

      # initialize valid_opts
      self.init_options()

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # short, terminal arguments
      self.valid_opts.add_opt('-help', 0, [],           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-ver', 0, [],            \
                      helpstr='display the current version number')

      # required parameters
      self.valid_opts.add_opt('-infile', 1, [], 
                      helpstr='read the given 1D file')

      # general options
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self):

      # process any optlist_ options
      self.valid_opts.check_special_opts(sys.argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, apply -help
      if len(sys.argv) <= 1 or '-help' in sys.argv:
         print g_help_string
         return 0

      if '-hist' in sys.argv:
         print g_history
         return 0

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 0

      if '-ver' in sys.argv:
         print g_version
         return 0

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # process non-chronological options, verb comes first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script

      for opt in uopts.olist:

         # main options
         if opt.name == '-infile':
            if self.a1d != None:
               print '** only 1 -infile option allowed'
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1
            if self.init_from_file(val): return 1

         # general options

         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val != None and err: return 1
            else: self.verb = val
            continue

      return 0

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1:
         print '-- processing...'

   def ready_for_action(self):
      """perform any final tests before execution"""

      ready = 1

      return ready

   def init_from_file(self, fname):
      """load a 1D file, and init the main class elements"""

      self.status = 1 # init to failure


      self.status = 0 # update to success

      return 0

   def test(self, verb=3):
      print '------------------------ initial tests -----------------------'
      self.verb = verb

      print '------------------------ reset files -----------------------'

      print '------------------------ should fail -----------------------'

      print '------------------------ more tests ------------------------'

      return None

def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 1

   rv = me.execute()
   if rv > 0: return 1

   return me.status

if __name__ == '__main__':
   sys.exit(main())


