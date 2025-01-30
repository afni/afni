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

-in_pbar PBAR  :(req) name of the pbar file, which can be in one of the
                following formats: JPG, PNG, TIF

-prefix PREFIX :(req) name of output file, including file extension

-in_json  JSON :name of a JSON file with known keys that describe relevant
                pbar values; in particular, JSONs output by @chauffeur_afni
                are good to use here. An efficient way to provide pbar_min,
                pbar_max, alpha, thr_val (and perhaps more over time)

-pbar_min MIN  :lower/minimum/bottom value of pbar

-pbar_max MAX  :upper/maximum/top value of pbar

-alpha  ALPHA  :keyword setting for specifying alpha transparency for
                thresholding.  Must be one of the following values:
                  {all_alpha}

-thr_val TVAL  :threshold value, applied as an absolute value

-thr_width TWID :when displaying the threshold line in the output pbar,
                this controls the width, as an integer number of pixels
                (def: {thr_wid})

-thr_num_osc TNO :by default, the threshold line oscillates between two
                colors for increased visibility. This integer specifies  
                the number of oscillations (def: {thr_no})

-thr_colors TCOL1 [TCOL2]
               :by default, the threshold line oscillates between two
                colors for increased visibility. Users can put 1 color
                name here, for a solid line, or two of their own color
                choices (def: 'black' 'white')

-thr_off       :turn off displaying the threshold line, even if
                thresholding is being applied

-tick_num_int TNI :add tick lines, where TNI is the integer number of 
                intervals to use; specifying 0 turns of tick display
                (def: {tick_ni})

-tick_frac TF  :when tick lines are used, how far should they extend, as 
                a fraction of the pbar width (def: {tick_frac})

-tick_color TCOL :specify the color of the tick lines (def: 'black')

-orth_on       :by default, the alpha fading is applied _along_ the 
                pbar gradient. Using this flag means it will be applied
                orthogonally/perpendicularly to that gradient. This is 
                most useful in cases when the overlay and threshold 
                data differ

-orth_frac OF  :specify at what fraction of the pbar width the fading
                should start (def: {orth_frac})

-outline_width OUTWID
               :add an outline to the output pbar, whose width is
                an integer OUTWID number of pixels at each edge
                (def: {outwid})

-outline_color OUTCOL
               :choose the color of any added outline (def: 'black')

-bkgd_color BC :background color that appears when thresholding is
                applied (def: '{bkgd_color}')

-help, -h      :display program help file

-hist          :display program history

-ver           :display program version number

-verb  VVV     :control verbosity (def: 1)

-show_valid_opts :show valid options for this program

------------------------------------------------------------------------

Notes ~1~

These are some notes about using this program.

Orientation
Input colorbars can be either vertically oriented (assumes pbar_max is
at the top) or horizontally oriented (assumes pbar_max is to the
right).

File formats
Input formats that are known to be valid include *.jpg, *.tiff and
*.png.  Valid output formats include those, as well as *.svg and
likely any other that Python's Matplotlib module can write.

JSONs
This program is meant to work smoothly with *.json files exported via
'@chauffeur_afni -pbar_saveim ..'.  That is, this program only looks
for certain keys there (pbar_bot, pbar_top, thr_val and olay_alpha).
JSON files from other sources can work, but these would have to use
those same key names.

Precedence
When both JSON files and command line options are used, the latter
will take precedence if there is any overlap/conflict. This makes it
easier to tweak values. NB: they JSON keys and command line options
are often not the exact same name, but something similar (see previous
note on JSONs).

Colors
As with many AFNI programs, the color values that are recognized can
be any of those in the GUI menu (yellow, yell-oran, oran-yell,
rbgyr20_01, etc.), or hex-defined colors (likely putting these in some
quotes), or Matplotlib-recognized colors (e.g., see:
https://matplotlib.org/stable/gallery/color/named_colors.html).

------------------------------------------------------------------------

Examples ~1~

The following examples make reference to the AFNI Bootcamp data, and
can be run directly with copy+paste in the following directory:
    ~/AFNI_data6/FT_analysis/FT.results/QC_FT/media

The following two palettes/colorbars star in the examples:
  + qc_06_vstat_Full_Fstat.pbar.jpg, which is 'unidirectional', likely
    going from pbar_min = 0 to some positive pbar_max
  + qc_07_vstat_vis_0_Coef.pbar.jpg, which is 'bidirectional', likely
    going from some pbar_min = -VAL to pbar_max = VAL
Each *.jpg has an associated *.json file, which was created by
@chauffeur_afni and contains useful parameters associated with the
pbar (e.g., pbar_min, pbar_max, thr_val and alpha state).

    1) Use the JSON information to place threshold and set fading:

    cbar_pbar.py                                                 \\
        -in_pbar  qc_06_vstat_Full_Fstat.pbar.jpg                \\
        -in_json  qc_06_vstat_Full_Fstat.pbar.json               \\
        -prefix   qc_06_vstat_Full_Fstat.pbar_FADE1.jpg            

    cbar_pbar.py                                                 \\
        -in_pbar  qc_07_vstat_vis_0_Coef.pbar.jpg                \\
        -in_json  qc_07_vstat_vis_0_Coef.pbar.json               \\
        -prefix   qc_07_vstat_vis_0_Coef.pbar_FADE1.jpg            

    2) Use orthogonal fading (no min/max/etc. info needed):

    cbar_pbar.py                                                 \\
        -in_pbar  qc_06_vstat_Full_Fstat.pbar.jpg                \\
        -prefix   qc_06_vstat_Full_Fstat.pbar_FADE2.jpg          \\
        -alpha    Yes                                            \\
        -orth_on                                                   

    cbar_pbar.py                                                 \\
        -in_pbar  qc_07_vstat_vis_0_Coef.pbar.jpg                \\
        -prefix   qc_07_vstat_vis_0_Coef.pbar_FADE2.jpg          \\
        -alpha    Yes                                            \\
        -orth_on                                                   

    3) Implement fading, and add an outline

    cbar_pbar.py                                                 \\
        -in_pbar        qc_06_vstat_Full_Fstat.pbar.jpg          \\
        -in_json        qc_06_vstat_Full_Fstat.pbar.json         \\
        -prefix         qc_06_vstat_Full_Fstat.pbar_FADE3.jpg    \\
        -outline_color  green                                    \\
        -outline_width  4                                          

    cbar_pbar.py                                                 \\
        -in_pbar        qc_07_vstat_vis_0_Coef.pbar.jpg          \\
        -in_json        qc_07_vstat_vis_0_Coef.pbar.json         \\
        -prefix         qc_07_vstat_vis_0_Coef.pbar_FADE3.jpg    \\
        -outline_color  darkmagenta                              \\
        -outline_width  6                                          

    4) Implement fading, and adjust the threshold line properties, 
       tick properties and min/max; NB: options directly entered on
       the command line have precedence over JSON values:

    cbar_pbar.py                                                 \\
        -in_pbar      qc_06_vstat_Full_Fstat.pbar.jpg            \\
        -in_json      qc_06_vstat_Full_Fstat.pbar.json           \\
        -prefix       qc_06_vstat_Full_Fstat.pbar_FADE4.jpg      \\
        -thr_colors   '#f5b041' 'tab:brown'                      \\
        -thr_num_osc  2                                          \\
        -thr_width    8                                            

    cbar_pbar.py                                                 \\
        -in_pbar       qc_07_vstat_vis_0_Coef.pbar.jpg           \\
        -in_json       qc_07_vstat_vis_0_Coef.pbar.json          \\
        -prefix        qc_07_vstat_vis_0_Coef.pbar_FADE4.jpg     \\
        -pbar_max      10                                        \\
        -pbar_min      -10                                       \\
        -tick_num_int  4                                         \\
        -tick_color    antiquewhite

    5) Implement linear fading, for various min/max/thr values:

    cbar_pbar.py                                                 \\
        -in_pbar   qc_06_vstat_Full_Fstat.pbar.jpg               \\
        -prefix    qc_06_vstat_Full_Fstat.pbar_FADE5.jpg         \\
        -pbar_min  0                                             \\
        -pbar_max  10                                            \\
        -thr_val   7                                             \\
        -alpha     Linear                                             

    cbar_pbar.py                                                 \\
        -in_pbar   qc_07_vstat_vis_0_Coef.pbar.jpg               \\
        -prefix    qc_07_vstat_vis_0_Coef.pbar_FADE5.jpg         \\
        -pbar_min  -5                                            \\
        -pbar_max  5                                             \\
        -thr_val   3                                             \\
        -alpha     Linear                                          

    6) Same examples as #5 above, but with simple thresholding (i.e.,
       fading turned off)

    cbar_pbar.py                                                 \\
        -in_pbar   qc_06_vstat_Full_Fstat.pbar.jpg               \\
        -prefix    qc_06_vstat_Full_Fstat.pbar_FADE6.jpg         \\
        -pbar_min  0                                             \\
        -pbar_max  10                                            \\
        -thr_val   7                                             \\
        -alpha     No                                             

    cbar_pbar.py                                                 \\
        -in_pbar   qc_07_vstat_vis_0_Coef.pbar.jpg               \\
        -prefix    qc_07_vstat_vis_0_Coef.pbar_FADE6.jpg         \\
        -pbar_min  -5                                            \\
        -pbar_max  5                                             \\
        -thr_val   3                                             \\
        -alpha     No                                          


""".format(all_alpha=lcp.list_alpha_str, thr_wid=lcp.DOPTS['thr_width'],
           thr_no=lcp.DOPTS['thr_num_osc'], tick_ni=lcp.DOPTS['tick_num_int'],
           tick_frac=lcp.DOPTS['tick_frac'], orth_frac=lcp.DOPTS['orth_frac'],
           outwid=lcp.DOPTS['outline_width'], 
           bkgd_color=lcp.DOPTS['bkgd_color'])

g_history = """
  gtkyd_check.py history:

  0.1   Jan 26, 2025 :: started this command line interface for lib_cbar_pbar
  0.2   Jan 30, 2025 :: beta version complete (with options)
"""

g_ver     = g_history.split("\n")[-2].split("::")[0].strip()
g_version = "cbar_pbar.py version " + g_ver

class InOpts:
   """Object for storing any/all command line inputs, and just checking
that any input files do, in fact, exist.  Option parsing and other
checks happen in a subsequent object.

See lcp.CbarPbar() for the set of things that are populated for the actual
   cbar editing.

   """

   def __init__(self):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # main data variables
      self.in_pbar         = None
      self.prefix          = None

      # the JSON from @chauffeur_afni, or all the keys that can be in it;
      # see method combine_chauffeur_json_opts()
      self.in_json         = None
      self.pbar_min        = None
      self.pbar_max        = None
      self.thr_val         = None
      self.alpha           = None

      # threshold line
      self.thr_on          = True
      self.thr_width       = None
      self.thr_num_osc     = None
      self.thr_colors      = []

      # tick properties
      self.tick_num_int    = None
      self.tick_frac       = None
      self.tick_color      = None

      # control orthogonality (e.g., when olay and thr are diff dsets)
      self.orth_on         = False
      self.orth_frac       = None

      # outline properties
      self.outline_width   = None
      self.outline_color   = None

      # background properties
      self.bkgd_color      = None

      # general variables
      self.verb            = None

      # initialize valid_opts
      tmp1 = self.init_options()

   # ----------------------------

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
      self.valid_opts.add_opt('-in_pbar', 1, [], 
                      helpstr='name of input cbar file')

      self.valid_opts.add_opt('-prefix', 1, [], 
                      helpstr='name of output cbar file')

      # optional parameters
      self.valid_opts.add_opt('-in_json', 1, [], 
                      helpstr='name of JSON file for cbar file')

      self.valid_opts.add_opt('-pbar_min', 1, [], 
                      helpstr='pbar: minimum value in palette bar')

      self.valid_opts.add_opt('-pbar_max', 1, [], 
                      helpstr='pbar: maximum value in palette bar')

      self.valid_opts.add_opt('-thr_val', 1, [], 
                      helpstr="threshold line: absolute value for threshold")

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

      self.valid_opts.add_opt('-bkgd_color', 1, [], 
                      helpstr="background: specify color")

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
         if opt.name == '-in_pbar':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP1(err_base + opt.name)
            self.in_pbar = val

         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP1(err_base + opt.name)
            self.prefix = val

         # general options

         elif opt.name == '-in_json':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP1(err_base + opt.name)
            self.in_json = val

         elif opt.name == '-pbar_min':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.pbar_min = val

         elif opt.name == '-pbar_max':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.pbar_max = val

         elif opt.name == '-thr_val':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.thr_val = val

         elif opt.name == '-alpha':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.alpha = val

         elif opt.name == '-thr_width':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.thr_width = val

         elif opt.name == '-thr_num_osc':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err:
                BASE.EP1(err_base + opt.name)
            self.thr_num_osc = val

         elif opt.name == '-thr_colors':
            val, err = uopts.get_string_list('', opt=opt)
            if val is None or err:
                BASE.EP1(err_base + opt.name)
            self.thr_colors = val

         elif opt.name == '-thr_off':
            self.thr_on = False

         elif opt.name == '-tick_num_int':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.tick_num_int = val

         elif opt.name == '-tick_frac':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.tick_frac = val

         elif opt.name == '-tick_color':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.tick_color = val

         elif opt.name == '-orth_on':
            self.orth_on = True

         elif opt.name == '-orth_frac':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.orth_frac = val

         elif opt.name == '-outline_width':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.outline_width = val

         elif opt.name == '-outline_color':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.outline_color = val

         elif opt.name == '-bkgd_color':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err: 
                BASE.EP1(err_base + opt.name)
            self.bkgd_color = val

         elif opt.name == '-overwrite':
            self.do_ow = True

         # might have to merge some options
         tmp1 = self.combine_chauffeur_json_opts()
         if tmp1 :
            return -1

      return 0

   def combine_chauffeur_json_opts(self):
       """Some values can come from the command line individually, or from an
       @chauffeur_afni-created JSON, or both. This method checks and merges
       them, giving overwrite priority to the command line entered ones
       (i.e., it assumes the user wants to be editing). The opts processed
       here might likely get updated over time"""

       # nothing to do here, if JSON not provided
       if not(self.in_json) :
           return 0

       # go through and use JSON values if there isn't a value there
       # already; note that some of the dictionary keys and object
       # attributes have similar but distinct names

       D     = lcp.read_json(self.in_json)
       dkeys = D.keys()

       if 'pbar_bot' in dkeys :
           if self.pbar_min == None :
               self.pbar_min = float(D['pbar_bot'])
           elif self.verb :
               ab.WP("Using user-specified value of '{}', rather than JSON's "
                     "'{}'".format('pbar_min', 'pbar_bot'))

       if 'pbar_top' in dkeys :
           if self.pbar_max == None :
               self.pbar_max = float(D['pbar_top'])
           elif self.verb :
               ab.WP("Using user-specified value of '{}', rather than JSON's "
                     "'{}'".format('pbar_max', 'pbar_top'))

       if 'vthr' in dkeys :
           if self.thr_val == None :
               self.thr_val = float(D['vthr'])
           elif self.verb :
               ab.WP("Using user-specified value of '{}', rather than JSON's "
                     "'{}'".format('thr_val', 'vthr'))

       if 'olay_alpha' in dkeys :
           if self.alpha == None :
               self.alpha   = D['olay_alpha']
           elif self.verb :
               ab.WP("Using user-specified value of '{}', rather than JSON's "
                     "'{}'".format('thr_val', 'vthr'))

       return 0

   def execute(self):

      if not self.ready_for_action(): return 1

      if self.verb > 1:
         BASE.IP("Begin processing options")

      # all work and writing is basically done here
      # ... in other objects, but not here

      return 0

   def ready_for_action(self):

       """perform any final tests before execution"""

       # require -input
       if self.infiles is None :
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

   # init option-reading obj
   inobj = InOpts()
   if not inobj :  return 1, None

   # ... and read opts in
   rv = inobj.process_options()
   if rv > 0: 
       # exit with success (e.g. -help)
       return 0, None
   if rv < 0:
       # exit with error status
       print('** failed to process options...')
       return 1, None

   # create actual cbar object to use
   cbarobj = lcp.CbarPbar(user_inobj=inobj)
   if not cbarobj :  return 1

   ### ... and populate+run it from the InOpts obj
   ###rv = cbarobj.load_from_inopts(user_inobj=inobj)
   ###if rv > 0: return 1

   return 0, cbarobj

if __name__ == '__main__':

    stat, cbarobj = main()
    sys.exit(stat)


