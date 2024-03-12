#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
from afnipy import option_list as OL
from afnipy import afni_util as UTIL
from afnipy import lib_roi_stats as LRS

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
roi_stats_warnings.py - evaluate the output of compute_ROI_stats.tcsh
                      - colorize the stats reports for HTML display

------------------------------------------
examples: ~1~

   0. standard usage, just provide an input

        roi_stats_warnings.py -input tsnr_stats_regress/stats_CAEZ_ML.txt

------------------------------------------
terminal options: ~1~

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

main parameters:

      -input  INPUT             : input ROI stats text file
      -prefix PREFIX            : prefix for output HTML version

other options:
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    March 2024
=============================================================================
"""

g_history = """
   roi_stats_warnings.py history:

   0.0  Mar  7, 2024    - ...
   0.1  Mar  8, 2024    - tweak in usage, in case no prefix provided
"""

g_version = "roi_stats_warnings.py version 0.0, March 7, 2024"


class MyInterface:
   """interface class for MyLibrary"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # main data variables, based on -input
      self.input           = None
      self.prefix          = None

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
      self.valid_opts.add_opt('-input', 1, [], 
                      helpstr='ROI stats text file')

      self.valid_opts.add_opt('-prefix', 1, [], 
                      helpstr='prefix for output HTML version of stats file')

      # general options
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self):
      """return  1 on valid and exit        (e.g. -help)
         return  0 on valid and continue    (e.g. do main processing)
         return -1 on invalid               (bad things, panic, abort)
      """

      # process any optlist_ options
      self.valid_opts.check_special_opts(sys.argv)

      # process terminal options without the option_list interface
      # (so that errors are not reported)
      # return 1 (valid, but terminal)

      # if no arguments are given, apply -help
      if len(sys.argv) <= 1 or '-help' in sys.argv:
         print(g_help_string)
         return 1

      if '-hist' in sys.argv:
         print(g_history)
         return 1

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in sys.argv:
         print(g_version)
         return 1

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return -1           # error condition

      # ------------------------------------------------------------
      # process non-chronological options, verb comes first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # process options sequentially, to make them like a script

      for opt in uopts.olist:

         # main options
         if opt.name == '-input':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: return -1
            self.input = val

         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: return -1
            self.prefix = val

         # general options

         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val != None and err: return -1
            else: self.verb = val
            continue

      return 0

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1:
          print("-- processing input '%s'" % self.input)

      # process input
      self.text_lines = UTIL.read_text_file(self.input, strip=0)
      if len(self.text_lines) > 0:
          self.status = 0 # update to success

      # create HTML table
      self.roi_table = LRS.all_comp_roi_dset_table(self.text_lines,
                                                   fname=self.input)

      # if we have no prefix, base it on input
      if self.prefix is None or self.prefix == '':
         dind = self.input.rfind('.')
         if dind > 0:
            self.prefix = self.input[:dind]
         else:
            self.prefix = 'no_name'

      # write out table
      self.roi_table.write_out_table_file(self.prefix)

      return 0

   def ready_for_action(self):
      """perform any final tests before execution"""

      # require -input
      if self.input is None:
         print("** missing -input option")
         return 0

      ready = 1

      return ready

   def test(self, verb=3):
      """one might want to be able to run internal tests,
         alternatively, test from the shell
      """
      print('------------------------ initial tests -----------------------')
      self.verb = verb

      print('------------------------ reset files -----------------------')

      print('------------------------ should fail -----------------------')

      print('------------------------ more tests ------------------------')

      return None

def main():
   me = MyInterface()
   if not me: return 1

   rv = me.process_options()
   if rv > 0: return 0  # exit with success (e.g. -help)
   if rv < 0:           # exit with error status
      print('** failed to process options...')
      return 1

   # else: rv==0, continue with main processing ...

   rv = me.execute()
   if rv > 0: return 1

   return me.status

if __name__ == '__main__':
   sys.exit(main())


