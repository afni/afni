#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
from afnipy import option_list   as OL
from afnipy import afni_util     as UTIL
from afnipy import afni_base     as BASE
from afnipy import lib_cbar_pbar as lcp

# ----------------------------------------------------------------------
# globals

g_help_string = """Overview ~1~

This program is for working with AFNI-style colorbar (cbar) and
palette bar (pbar) files. It might also be fairly general-purpose for
PNG, JPG and other rasterized colorbar files, as long as they have a
pretty straightforward formatting.  Particularly, this program is
meant for putting in threshold information, both opaque (=strict)
thresholds and transparent (=alpha fading, AKA subthreshold fading)
ones.

In AFNI, the colorbar goes with the overlay dataset, which may or may
not be the same as the threshold dataset. 
+ In cases where they *are* the same, this program can be used to:
  - add in (striped) threshold boundary lines
  - replace subthreshold regions with a dull/null gray color
  - put in alpha-based fading (either Linear or Quadratic)
  - use values from a JSON file output by @chauffeur_afni to efficiently
    gain useful knowledge about relevant cbar info, like min/max, threshold
    values, ON/OFFness of alpha fading, etc.
+ In cases where they differ, this program might be useful for:
  - representing alpha-fading as an orthogonal (=perpendicular to the
    the color gradient) melding of the colorbar with a null/dull gray
+ In all cases, this program can:
  - add a boundary of chosen thickness and color.
More functionality will likely be added over time.

Notation note:  For simplicity, we mostly just refer to the colorbar or
palette as a 'cbar', which should be synonymous here with 'pbar'.

auth = PA Taylor (SSCC, NIMH, NIH, USA)

------------------------------------------------------------------------
Overview ~1~

------------------------------------------------------------------------
Usage ~1~

-infile FILE   :(req) name of the cbar file, which can be in one of the
                following formats: JPG, PNG, TIF.

-prefix PREFIX :(req) name of output file.

*** add more ***

-help, -h      :display program help file

-echo          :run very verbosely, by echoing each part of script  
                before executing it

-hist          :display program history

-ver           :display program version number

-verb  VVV     :control verbosity (def: 1)

-show_valid_opts :show valid options for this program

------------------------------------------------------------------------
Examples ~1~



"""

g_history = """
   gtkyd_check.py history:

   0.1  Jan 26, 2025    - started this command line interface for lib_cbar_pbar
"""

g_version = "cbar_pbar.py version 0.1  Jan 26, 2025"


class MyInterface:
   """interface class for MyLibrary"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # main data variables
      self.in_cbar         = None
      self.prefix          = None

      self.in_json         = None
      self.alpha           = None

      self.thr_do          = True
      self.thr_width       = None
      self.thr_num_osc     = None
      self.thr_colors      = None

      self.tick_num_int    = None
      self.tick_frac       = None
      self.tick_color      = None

      self.orth_do         = True
      self.orth_frac       = None

      self.outline_width   = None
      self.outline_color   = None

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
      self.valid_opts.add_opt('-in_cbar', 1, [], 
                      helpstr='name of input cbar file')

      self.valid_opts.add_opt('-prefix', 1, [], 
                      helpstr='name of output cbar file')

      # optional parameters
      self.valid_opts.add_opt('-in_json', 1, [], 
                      helpstr='name of JSON file for cbar file')

      self.valid_opts.add_opt('-alpha', 1, [], 
                      helpstr="setting for transparent thresholding")

      self.valid_opts.add_opt('-thr_width', 1, [], 
                      helpstr="threshold line: width")

      self.valid_opts.add_opt('-thr_num_osc', 1, [], 
                      helpstr="threshold line: number of oscillations")

      self.valid_opts.add_opt('-thr_colors', -1, [], 
                      helpstr="threshold line: one or two colors to use")

      self.valid_opts.add_opt('-thr_off', 0, [], 
                      helpstr="threshold line: off")

      self.valid_opts.add_opt('-tick_num_int', 1, [], 
                      helpstr="tick lines: number of intervals")

      self.valid_opts.add_opt('-tick_frac', 1, [], 
                      helpstr="tick lines: length as fraction of cbar width")

      self.valid_opts.add_opt('-tick_color', 1, [], 
                      helpstr="tick lines: color")

      self.valid_opts.add_opt('-orth_on', 0, [], 
                      helpstr="orthogonal fade: on (=fade perp to color grad)")

      self.valid_opts.add_opt('-orth_frac', 0, [], 
                      helpstr="orthogonal fade: fraction at which to start")

      self.valid_opts.add_opt('-outline_width', 1, [], 
                      helpstr="outline: add outline of this integer width")

      self.valid_opts.add_opt('-outline_color', 1, [], 
                      helpstr="outline: specify color")

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

      err_base = "Problem interpreting use of opt: "

      for opt in uopts.olist:

         # main options
         if opt.name == '-in_cbar':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.in_cbar = val

         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.prefix = val

         # general options

         elif opt.name == '-in_json':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.in_json = val

         elif opt.name == '-alpha':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.alpha = val

         elif opt.name == '-thr_width':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.thr_width = val

         elif opt.name == '-thr_num_osc':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.thr_num_osc = val

         elif opt.name == '-thr_colors':
            val, err = uopts.get_string_list('', opt=opt)
            if is None or err:
                BASE.EP(err_base + opt.name)
            self.thr_colors = val

         elif opt.name == '-thr_off':
            self.thr_do = False

         elif opt.name == '-tick_num_int':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.tick_num_int = val

         elif opt.name == '-tick_frac':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.tick_frac = val

         elif opt.name == '-tick_color':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.tick_color = val

         elif opt.name == '-orth_on':
            self.orth_do = True

         elif opt.name == '-orth_frac':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.orth_frac = val

         elif opt.name == '-outline_width':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.outline_width = val

         elif opt.name == '-outline_color':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP(err_base + opt.name)
            self.outline_color = val

         elif opt.name == '-overwrite':
            self.do_ow = True

      return 0

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1:
         BASE.IP("Begin processing options")

      # all work and writing is basically done here.
      gtkyd_obj = lgtk.GtkydInfo( self.infiles,
                                  outdir = self.outdir,
                                  do_minmax = self.do_minmax,
                                  id_keeps_dirs = self.id_keeps_dirs,
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


