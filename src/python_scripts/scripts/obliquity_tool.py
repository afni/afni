#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
from afnipy import option_list   as OL
from afnipy import afni_util     as UTIL
from afnipy import afni_base     as BASE
from afnipy import lib_obl_tool  as LOT

# ----------------------------------------------------------------------
# globals

g_help_string = """Overview ~1~

This program is for dealing with obliquity in datasets.  This includes:
+ purging it in various ways (without regridding/interpolating data),
+ applying it (hence, regridding/interpolating data), 
+ transferring it between datasets, 
+ extracting and displaying it,
and more.

auth = PA Taylor (SSCC, NIMH, NIH, USA)

------------------------------------------------------------------------

Usage ~1~

-inset INSET   :(req) name of the input dataset

-prefix PREFIX :(req) name of output dset

-purge_obliquity : purge (= remove) obliquity from inset's header,
                using the following 2 key properties: 
                _without_ regridding the data, and _with_ preserving
                the coordinate origin, (x, y, z) = (0, 0, 0).
                NB: Using this option is a primary purpose function of 
                this program.

-heir_dsets HD1 [HD2 HD3 ...] 
               :one or more datasets that can inherit the obliquity 
                that gets purged from the inset.

     ... and if using '-heir_dsets', at least one of the following
         '-heir_*' options must be included to specify their output
         naming:

-heir_prefixes HP :when '-heir_dsets ..' is used, then this is an
                option for specifying the output path+name for each
                one. So, the number of entries here must match number
                of heir dsets.  This canNOT be combined with
                '-heir_outdir' or '-heir_suffix'

-heir_outdir HO :when '-heir_dsets ..' is used, users can specify a
                single output directory for all output heirs.  If
                '-heir_suffix ..' is not also provided, then the each
                output file will have the same name as its input (and
                in such a case, the heir_outdir should be different
                than each heir dset's directory, unless '-overwrite'
                is used)

-heir_suffix HS :when '-heir_dsets ..' is used, users can specify a
                suffix to be inserted just before each file's
                extension.  Typically, uses will want to start the
                suffix with '_'.
                Users can simultaneously use '-heir_outdir ..' to
                specify a single output dir for all heir dsets;
                otherwise, each heir will be output in each of the
                original heir_dset's directories

-do_qc DQ      :state whether to make QC images when using '-heir_dsets ..',
                which means showing the overlap of each heir dset with the
                main inset both before and after obliquity changes;
                allowed values are:  Yes, 1, No, 0
                (def: '{do_qc}')

-workdir WD    : working directory name, without path; the working dir
                will be subdirectory of the output location
                (def: name with random chars)

-do_clean DC   :state whether to clean up any intermediate files;
                allowed values are:  Yes, 1, No, 0
                (def: '{do_clean}')

-do_log        :add this opt to turn on making a text log of all the
                shell commands that are run when this program is
                executed.  Mainly for debugging purposes.

-help, -h      :display program help file

-hist          :display program history

-ver           :display program version number

-verb  VVV     :control verbosity (def: {verb})

-show_valid_opts :show valid options for this program

------------------------------------------------------------------------

Notes ~1~

Dealing with obliquity can take take several forms:

+ Some involve changing header information, without regridding (and
  therefore interpolating and smoothing) the data itself but
  effectively changing the coordinate locations of data.  

+ Others involve applying the obliquity to the data, thereby
  regridding/smoothing the data, but preserving the locations of the 
  data in the original scanner coordinates.  

Each can have its own use cases.

Removing obliquity from the dataset header ~2~

****

------------------------------------------------------------------------

Examples ~1~

 ****

           

""".format(**LOT.DOPTS)

g_history = """
  gtkyd_check.py history:

  0.1   Sep 25, 2025 :: started this command line interface 
"""

g_ver     = g_history.split("\n")[-2].split("::")[0].strip()
g_version = "obliquity_tool.py version " + g_ver

class InOpts:
   """Object for storing any/all command line inputs, and just checking
that any input files do, in fact, exist.  Option parsing and other
checks happen in a subsequent object.

See lct.CbarPbar() for the set of things that are populated for the actual
   cbar editing.

   """

   def __init__(self):

      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None
      self.argv            = None

      # general variables
      self.verb            = LOT.DOPTS['verb']
      self.do_clean        = None
      self.overwrite       = None
      self.do_log          = None

      # main data variables
      self.inset           = None
      self.prefix          = None

      self.heir_dsets      = []
      self.heir_prefixes   = None
      self.heir_outdir     = None
      self.heir_suffix     = None

      # control variables
      self.workdir         = None
      self.remove_obl      = None
      self.do_qc           = None

      # initialize valid_opts
      tmp1 = self.init_options()

   # ----------------------------

   def init_options(self):
      """
      Prepare the set of all options, with very short help descriptions
      for each.
      """

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

      self.valid_opts.add_opt('-inset', 1, [], 
                      helpstr='name of input dataset')

      self.valid_opts.add_opt('-prefix', 1, [], 
                      helpstr='name of output dataset')

      # optional parameters

      self.valid_opts.add_opt('-purge_obliquity', 0, [], 
                      helpstr='remove obliquity: *no* blur, *keep* XYZ origin')

      self.valid_opts.add_opt('-heir_dsets', -1, [], 
                      helpstr='dset(s) to inherit trimmed obliquity')

      self.valid_opts.add_opt('-heir_prefixes', 1, [], 
                      helpstr='heir output method: direct prefix for each')

      self.valid_opts.add_opt('-heir_outdir', 1, [], 
                      helpstr='heir output method: output dir to hold all')

      self.valid_opts.add_opt('-heir_suffix', 1, [], 
                      helpstr='heir output method: suffix to add for all')

      self.valid_opts.add_opt('-do_qc', 1, [], 
                      helpstr="turn on/off QC images for heir dsets")

      self.valid_opts.add_opt('-workdir', 1, [], 
                      helpstr='name of workdir (no path)')


      # general options

      self.valid_opts.add_opt('-do_clean', 1, [], 
                      helpstr="turn on/off removal of intermediate files")

      self.valid_opts.add_opt('-do_log', 0, [], 
                      helpstr="turn on/off logging shell cmd execution")

      self.valid_opts.add_opt('-overwrite', 0, [], 
                      helpstr='overwrite preexisting outputs')

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

         if opt.name == '-inset':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.inset = val

         elif opt.name == '-prefix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.prefix = val

         # optional parameters

         elif opt.name == '-purge_obliquity':
            self.do_purge_obl = True

         elif opt.name == '-heir_dsets':
            val, err = uopts.get_string_list('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.heir_dsets = val

         elif opt.name == '-heir_prefixes':
            val, err = uopts.get_string_list('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.heir_prefixes = val

         elif opt.name == '-heir_outdir':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.heir_outdir = val

         elif opt.name == '-heir_suffix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.heir_suffix = val

         elif opt.name == '-do_qc':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.do_qc = val

         elif opt.name == '-workdir':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.workdir = val

         # general options

         elif opt.name == '-do_clean':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.do_clean = val

         elif opt.name == '-do_log':
            self.do_log = True

         elif opt.name == '-overwrite':
            self.overwrite = '-overwrite'

         # ... verb has already been checked above

         # save this, for reporting in history 
         self.argv = sys.argv

      return 0

   def check_options(self):
      """perform any final tests before execution"""

      if self.verb > 1:
         BASE.IP("Begin processing options")

      # required opt
      if self.inset is None :
         BASE.EP1("missing -inset option")
         return -1

      if self.prefix is None:
         BASE.EP1("missing -prefix option")
         return -1

      # check various requirements/restrictions on -heir_* opts
      if self.nheirs :
          tmp = LOT.check_heir_opt_usage(self.nheirs, self.heir_prefixes,
                                         self.heir_outdir, self.heir_suffix)
          return tmp

      ### only using one default method now; disabling this
      #if not(self.remove_obl in LOT.LIST_remove_obl_header_keys + ['']) :
      #   txt = "Invalid arg after '-purge_obl': " + self.remove_obl
      #   txt+= '\n Valid args are:\n ' + LOT.STR_remove_obl_header_keys
      #   BASE.EP1(txt)
      #   return -1

      return 0

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

   # ----- decorators

   @property
   def nheirs(self):
       """number of heir_dsets"""
       return len(self.heir_dsets)


# ----------------------------------------------------------------------------

def main():

   # init option-reading obj
   inobj = InOpts()
   if not(inobj) :  
       return 1, None

   # process (= read) options
   rv = inobj.process_options()
   if rv > 0: 
       # exit with success (e.g. -help)
       return 0, None
   if rv < 0:
       # exit with error status
       BASE.EP1('failed to process options')
       return 1, None

   # check the options
   rv2 = inobj.check_options()
   if rv2 :
       # exit with error status
       BASE.EP1('failed whilst checking options')
       return rv2, None

   # use options to create main object
   mainobj = LOT.MainObj( user_inobj=inobj )
   if not mainobj :  
       return 1

   # write out log/history of what has been done (not done by default, to
   # save some time, bc this takes a mini-while)
   if inobj.do_log :
      olog = 'log_obliquity_tool.txt'
      UTIL.write_afni_com_log(olog)

   return 0, mainobj

# ============================================================================

if __name__ == '__main__':

    stat, mainobj = main()
    sys.exit(stat)

