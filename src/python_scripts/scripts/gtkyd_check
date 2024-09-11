#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
from afnipy import option_list as OL
from afnipy import afni_util as UTIL
from afnipy import afni_base as BASE
from afnipy import lib_gtkyd as lgtk

# ----------------------------------------------------------------------
# globals

g_help_string = """
This program is for Getting To Know Your Data (GTKYD). Provide a list
of datasets, and this program will check their header (and possibly a
few data) properties. Properties are checked with 3dinfo, nifti_tool
and 3dBrickStat. 

This program creates the following useful outputs:

+ A compiled spreadsheet-like table file, for reference, with 1 row
  per input dataset and one column per measured property. This is
  actually made using gen_ss_review_table.py. 
  (name: OUT.xls)

+ For each item checked, there will also be a detailed report file (N
  lines of data for N input datasets)
  (name: OUT/rep_gtkyd_detail_*.dat)

+ For each item checked, there will be a "uniqueness" report file,
  which will have 1 line of data for each unique value present across
  all input datasets. So, if there is only 1 line of data, then that
  property is consistent across all dsets; otherwise, there is some
  variability in it.
  (name: OUT/rep_gtkyd_unique_*.dat)

+ For each input dataset, a colon-separated dictionary of basic
  properties. These can be further queried with gen_ss_review_table.py.
  (name: OUT/dset_*.txt)


ver  = ${version}
auth = PA Taylor (SSCC, NIMH, NIH, USA), but no doubt also including 
       the valuable insights of RC Reynolds and DR Glen

------------------------------------------------------------------------
Overview ~1~

------------------------------------------------------------------------
Usage ~1~

-infiles FILE1 [FILE2 FILE3 ...]
               :(req) name of one or more file to input

-outdir ODIR   :(req) name of output "report directory", for more the 
                reports of details and uniqueness of each property.

-do_minmax     :include dataset min and max value info, which can be 
                slow (uses '3dBrickStat -slow ...' to calculate it 
                afresh)

-overwrite     :overwrite any preexisting outdir and corresponding XLS 
                file

-help, -h      :display program help file

-echo          :run very verbosely, by echoing each part of script  
                before executing it

-hist          :display program history

-ver           :display program version number

-verb  VVV     :control verbosity (def: 1)

-show_valid_opts :show valid options for this program

------------------------------------------------------------------------
Examples ~1~

1) Basic example, running on a set of EPI:
    gtkyd                                                \\
        -infiles  group_study/*task*.nii.gz              \\
        -outdir   group_summary

2) Include (possibly slow) min/max info, and check anatomical dsets:
    gtkyd_check                                          \\
        -infiles    group_study2/*T1w*.nii.gz *T1w*HEAD  \\
        -do_minmax                                       \\
        -outdir     group_summary2


"""

g_history = """
   gtkyd_check history:

   0.0  Mar 21, 2024    - started in tcsh
   0.1  Sep 10, 2024    - migrated to Python
"""

g_version = "gtkyd_check version 0.2, June 18, 2024"


class MyInterface:
   """interface class for MyLibrary"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # main data variables
      self.infiles         = None
      self.outdir          = None
      self.do_ow           = False
      self.do_minmax       = False

      # general variables
      self.verb            = verb
      self.ver             = verb

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
      self.valid_opts.add_opt('-infiles', -1, [], 
                      helpstr='name of one or more file to input files')

      self.valid_opts.add_opt('-outdir', 1, [], 
                      helpstr='name of output "report directory" and XLS')

      # optional parameters
      self.valid_opts.add_opt('-do_minmax', 0, [], 
                      helpstr='include dset min and max info (can be slow)')
      self.valid_opts.add_opt('-overwrite', 0, [], 
                      helpstr='overwrite preexisting outputs')

      # general options
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 0)')

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
         if opt.name == '-infiles':
            self.infiles, err = uopts.get_string_list('', opt=opt)
            if self.infiles == None or err:
               print('** failed to read -infiles list')
               errs +=1

         elif opt.name == '-outdir':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: return -1
            self.outdir = val

         elif opt.name == '-do_minmax':
            self.do_minmax = True

         elif opt.name == '-overwrite':
            self.do_ow = True

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
         BASE.IP("Begin processing options")

      # all work and writing is basically done here.
      gtkyd_obj = lgtk.GtkydInfo( self.infiles,
                                  outdir = self.outdir,
                                  do_minmax = self.do_minmax,
                                  do_ow = self.do_ow,
                                  verb=self.verb )

      return 0

   def ready_for_action(self):
      """perform any final tests before execution"""

      # require -input
      if self.infiles is None:
         print("** missing -infiles option")
         return 0

      if self.outdir is None:
         print("** missing -outdir option")
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


