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

This program is primarily for removing oblquity from an input
dataset's header, with the two important properties:

+ **not** regridding (= smoothing/interpolating) the data itself

+ preserving the coordinate origin, (x, y, z) = (0, 0, 0).

**Also** note this important bonus feature: the removed obliquity can
also be transferred to other datasets (called "child" datasets).
Doing this preserves the relative overlap of the main input and child
datasets, in the real terms of where they are in real/scanner space
when obliquity is taken into account.

This program makes a convenient step in preparing FMRI data
collections to be processed (e.g., before sswarper2, FreeSurfer,
afni_proc.py, etc.).  Users can provide the T1w anatomical reference
as the inset to have its obliquity removed, *while also* providing the
EPIs from the same session as child datasets.  In this way, the
relative overlap of the EPI and anatomical will be preserved during
processing.

auth = PA Taylor (SSCC, NIMH, NIH, USA)

------------------------------------------------------------------------

Usage ~1~

-inset INSET   :(req) name of the input dataset

-prefix PREFIX :(req) name of output dset

-child_dsets CD1 [CD2 CD3 ...] 
               :one or more datasets that can inherit the obliquity 
                that gets purged from the inset.

     ... and if using '-child_dsets', at least one of the following
         '-child_*' options must be included to specify their output
         naming:

-child_prefixes CP1 [CP2 CP3 ...] 
               :when using '-child_dsets ..', users can specify the
                output path+name for each child dset here. The number 
                of entries here must match number of child dsets.  
                This option canNOT be combined with '-child_outdir' 
                or '-child_suffix'

-child_outdir CO :when using '-child_dsets ..', users can specify a
                single output directory for all output childs.  If
                '-child_suffix ..' is not also provided, then the each
                output file will have the same name as its input (and
                in such a case, the CO should differ from each child 
                dset's original directory, unless '-overwrite' is used)

-child_suffix CS :when using '-child_dsets ..', users can specify a
                suffix to be inserted just before each output child's file
                extension.  Typically, uses will want to start the
                suffix with '_'.
                This option can be used along with '-child_outdir ..';
                otherwise, each child will be output in same directory as
                its original child dset

-do_qc DQ      :state whether to make QC images when using '-child_dsets ..',
                which means showing the overlap of each child dset with the
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

Obliquity in general ~2~

+ Some involve changing header information, without regridding (and
  therefore interpolating and smoothing) the data itself but
  effectively changing the coordinate locations of data.  

+ Others involve applying the obliquity to the data, thereby
  regridding/smoothing the data, but preserving the locations of the 
  data in the original scanner coordinates.  

Each can have its own use cases.

Why do we recommend removing anatomical dataset's obliquity like this? ~2~

It is generally convenient for the anatomical dataset to have its
obliquity removed in the way this program does it.

+ First, after removing obliquity like this, the anatomical should
  overlap better with any reference template dataset (since it will
  "sit more squarely" within the FOV).

+ Second, different software deal with obliquity differently (e.g.,
  ignoring or applying it), and so when integrating different tools
  (like FS, SUMA and AFNI), this can be minorly annoying.  

+ Third, *applying* obliquity leads to resampling and interpolation,
  and hence blurring of the anatomical dataset. So, *purging* it
  before processing can remove an unnecessary
  resampling/interpolation/blur procedure.

+ Fourth, transferring the anatomical's removed obliquity to the child
  datasets (like, EPI or other volumes acquired in the same session)
  will help preserve the relative angle of overlap.  While in many
  cases AFNI alignment programs can overcome a fair bit of relative
  rotation, some cases are more extreme (looking at you, slab EPI
  datasets!) and so really benefit from maintaining original
  overlap. Also, the stability of all alignment procedures (AFNI's or
  other tools') benefits from closer starting overlap of datasets, and
  who needs to add more uncertainty into data processing?


------------------------------------------------------------------------

Examples ~1~

 1) Remove obliquity from a dset:

    obliquity_remover.py                                                  \\
        -inset            sub-017_T1w.nii.gz                              \\
        -prefix           sub-017_T1w_DEOB.nii.gz

 2) Remove obliquity from a dset, and pass it along to its associated 
    EPI datasets:

    obliquity_remover.py                                                  \\
        -inset            anat/sub-017_T1w.nii.gz                         \\
        -prefix           anat/sub-017_T1w_DEOB.nii.gz                    \\
        -child_dsets      func/sub-017_task-rest_run-01_bold.nii.gz       \\
                          func/sub-017_task-rest_run-02_bold.nii.gz       \\
                          func/sub-017_task-rest_run-03_bold.nii.gz       \\
        -child_prefixes   func/sub-017_task-rest_run-01_bold_DEOB.nii.gz  \\
                          func/sub-017_task-rest_run-02_bold_DEOB.nii.gz  \\
                          func/sub-017_task-rest_run-03_bold_DEOB.nii.gz

 3) Same as #2, but with a succinct method of adding a suffix to each child:
           
    obliquity_remover.py                                                  \\
        -inset            anat/sub-017_T1w.nii.gz                         \\
        -prefix           anat/sub-017_T1w_DEOB.nii.gz                    \\
        -child_dsets      func/sub-017_task-rest_run-01_bold.nii.gz       \\
                          func/sub-017_task-rest_run-02_bold.nii.gz       \\
                          func/sub-017_task-rest_run-03_bold.nii.gz       \\
        -child_suffix      _DEOB

 4) Same as #3, but putting each output child into a new dir:

    obliquity_remover.py                                                  \\
        -inset            anat/sub-017_T1w.nii.gz                         \\
        -prefix           anat/sub-017_T1w_DEOB.nii.gz                    \\
        -child_dsets      func/sub-017_task-rest_run-01_bold.nii.gz       \\
                          func/sub-017_task-rest_run-02_bold.nii.gz       \\
                          func/sub-017_task-rest_run-03_bold.nii.gz       \\
        -child_suffix     _DEOB                                           \\
        -child_outdir     func_deob

 5) Same as #4, but renaming in the outputs in a particular way: all
    output datasets have the same filenames but different paths. This
    might be quite useful when doing group processing in a way that
    each major step outputs the data in parallel directory trees:

    obliquity_remover.py                                                  \\
        -inset            p1/anat/sub-017_T1w.nii.gz                      \\
        -prefix           p2/anat/sub-017_T1w.nii.gz                      \\
        -child_dsets      p1/func/sub-017_task-rest_run-*_bold.nii.gz     \\
        -child_outdir     p2/func


""".format(**LOT.DOPTS)

g_history = """
  obliquity_remover.py history:

  0.1   Sep 25, 2025 :: started this command line interface 
  1.01  Dec 08, 2025 :: fully functional first version, with first fixes
"""

g_ver     = g_history.split("\n")[-2].split("::")[0].strip()
g_version = "obliquity_remover.py version " + g_ver

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

      self.child_dsets     = []
      self.child_prefixes  = None
      self.child_outdir    = None
      self.child_suffix    = None

      # control variables
      self.workdir         = None
      self.remove_obl      = None
      self.do_qc           = None
      self.do_purge_obl    = True                    # now const

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

      self.valid_opts.add_opt('-child_dsets', -1, [], 
                      helpstr='dset(s) to inherit obliquity removed from inset')

      self.valid_opts.add_opt('-child_prefixes', 1, [], 
                      helpstr='child output method: direct prefix for each')

      self.valid_opts.add_opt('-child_outdir', 1, [], 
                      helpstr='child output method: output dir to hold all')

      self.valid_opts.add_opt('-child_suffix', 1, [], 
                      helpstr='child output method: suffix to add for all')

      self.valid_opts.add_opt('-do_qc', 1, [], 
                      helpstr="turn on/off QC images for child dsets")

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

         elif opt.name == '-child_dsets':
            val, err = uopts.get_string_list('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.child_dsets = val

         elif opt.name == '-child_prefixes':
            val, err = uopts.get_string_list('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.child_prefixes = val

         elif opt.name == '-child_outdir':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.child_outdir = val

         elif opt.name == '-child_suffix':
            val, err = uopts.get_string_opt('', opt=opt)
            if val is None or err:
                BASE.EP(err_base + opt.name)
            self.child_suffix = val

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

      # check various requirements/restrictions on -child_* opts
      if self.nchild :
          tmp = LOT.check_child_opt_usage(self.nchild, self.child_prefixes,
                                          self.child_outdir, self.child_suffix)
          return tmp

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
   def nchild(self):
       """number of child_dsets"""
       return len(self.child_dsets)


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
      olog = 'log_obliquity_remover.txt'
      UTIL.write_afni_com_log(olog)

   return 0, mainobj

# ============================================================================

if __name__ == '__main__':

    stat, mainobj = main()
    sys.exit(stat)

