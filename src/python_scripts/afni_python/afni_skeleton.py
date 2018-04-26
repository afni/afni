#!/usr/bin/env python

# system libraries
import sys, os, glob

# AFNI libraries
import afni_util as UTIL
import option_list as OL

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
skeleton.py     - skeleton of a basic python program

   This is merely a reasonable place to start a new program.

------------------------------------------
examples:

------------------------------------------
terminal options:

   -help                : show this help
   -hist                : show the revision history
   -ver                 : show the version number

------------------------------------------
process options:

   -infiles             : specify input files

------------------------------------------
R Reynolds    January 2015
=============================================================================
"""

g_todo = """
   todo list:

        - write actual program
"""

g_history = """
   skeleton.py history:

   0.0  Jan 13, 2015 - initial version
"""

g_version = "skeleton.py version 0.0, Jan 13, 2015"


class MyInterface:
   """interface class for MyLibrary (whatever that is)
   """
   def __init__(self, verb=1):
      # main variables
      self.valid_opts      = None
      self.user_opts       = None

      # control
      self.verb            = 1

      # infile name parsing
      self.infiles         = []

      # initialize valid_opts
      self.valid_opts = self.get_valid_opts()

   def get_valid_opts(self):
      vopts = OL.OptionList('valid opts')

      # short, terminal arguments
      vopts.add_opt('-help', 0, [], helpstr='display program help')
      vopts.add_opt('-hist', 0, [], helpstr='display the modification history')
      vopts.add_opt('-ver', 0, [], helpstr='display the current version number')

      # general options
      vopts.add_opt('-infiles', -1, [],
                    helpstr='specify input files')
      vopts.add_opt('-verb', 1, [], helpstr='set the verbose level (def=1)')

      vopts.sort()

      return vopts

   def process_options(self):
      """return  1 on valid and exit
         return  0 on valid and continue
         return -1 on invalid
      """

      argv = sys.argv

      # process any optlist_ options
      self.valid_opts.check_special_opts(argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)

      # if no arguments are given, do default processing
      if '-help' in argv or len(argv) < 2:
         print g_help_string
         return 1

      if '-hist' in argv:
         print g_history
         return 1

      if '-show_valid_opts' in argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in argv:
         print g_version
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process verb first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script
      errs = 0
      for opt in self.user_opts.olist:
         # check for anything to skip
         if opt.name == '-verb': pass

         elif opt.name == '-infiles':
            self.infiles, err = uopts.get_string_list('', opt=opt)
            if self.infiles == None or err:
               print '** failed to read -infiles list'
               errs +=1

      # allow early and late error returns
      if errs: return -1

      # ------------------------------------------------------------
      # apply any trailing logic

      if len(self.infiles) < 1:
         print '** missing -infiles option'
         errs += 1

      if errs: return -1

      return 0

   def do_stuff(self):
      """main function to process input
      """

      print '-- have %d files to process' % len(self.infiles)

      errs = 0
      # check file existence first
      for ifile in self.infiles:
         if ifile in ['-', 'stdin']: pass
         elif not os.path.isfile(ifile):
            print '** input file not found: %s' % ifile
            errs += 1
      if errs: return 1

      return 0
         
def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # exit with success
   if rv < 0:           # exit with error status
      print '** failed to process options...'
      return 1

   if me.do_stuff(): return 1

   return 0

if __name__ == '__main__':
   sys.exit(main())


