#!/usr/bin/env python

# system libraries
import sys, os

if 1 :  # for testing, might add the current dir and ~/abin to the PATH
   try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
   except: pass

import gc, math

# AFNI libraries
import option_list as OL
import lib_afni1D as LAD
import afni_util as UTIL
import afni_base as BASE

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
1d_tool.py      - for manipulating and evaluating 1D files

   This program is meant to read/manipulate/write/diagnose 1D datasets.
   Input can be specified using AFNI sub-brick[]/time{} selectors.

---------------------------------------------------------------------------
examples (very basic for now):

   1. Select by rows and columns, akin to 1dcat.

      1d_tool.py -infile 'data/X.xmat.1D[0..3]{0..5}' -write t1.1D

   2. Compare with selection by separate options.

      1d_tool.py -infile data/X.xmat.1D                  \\
                 -select_cols '0..3' -select_rows '0..5' \\
                 -write t2.1D
      diff t1.1D t2.1D

   3. Transpose a dataset, akin to 1dtranspose.

      1d_tool.py -infile t3.1D -transpose -write ttr.1D

   4. Pad a file of regressors for a single run (#2) with zeros, so
      that it becomes run 2 of 7 (runs are 1-based).

      1d_tool.py -infile ricor_r02.1D -pad_into_many_runs 2 7 \\
                 -write ricor_r02_all.1D

   5. Display rows and columns for a 1D dataset.

      1d_tool.py -infile ricor_r02.1D -show_rows_cols

   6. Show correlation matrix warnings for this matrix.

      1d_tool.py -infile X.xmat.1D -show_cormat_warnings

---------------------------------------------------------------------------
basic informational options:

   -help                        : show this help
   -hist                        : show the module history
   -show_valid_opts             : show all valid options
   -ver                         : show the version number

----------------------------------------
required input:

   -infile DATASET.1D           : specify input 1D file

----------------------------------------
general options:

   -add_cols NEW_DSET.1D        : extend dset to include these columns
   -cormat_cutoff CUTOFF        : set cutoff for cormat warnings (in [0,1])
   -overwrite                   : allow overwriting of any output dataset
   -reverse                     : reverse data over time
   -select_cols SELECTOR        : apply AFNI column selectors, [] is optional
                                  e.g. '[5,0,7..21(2)]'
   -select_rows SELECTOR        : apply AFNI row selectors, {} is optional
                                  e.g. '{5,0,7..21(2)}'
   -show_cormat_warnings        : display correlation matrix warnings
   -show_rows_cols              : display the number of rows and columns
   -sort                        : sort data over time (smallest to largest)
                                  - sorts EVERY vector
                                  - consider the -reverse option
   -transpose                   : transpose the matrix (rows for columns)
   -write FILE                  : write the current 1D data to FILE
   -verb LEVEL                  : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    March 2009
=============================================================================
"""

g_history = """
   1d_tool.py history:    (also refers to lib_afni1D.py)

   0.0  Mar 18, 2009 - initial library (lib_1D.py) version
   0.1  Mar 19, 2009 - added some options and basic help
   0.2  Mar 26, 2009 - small array fix for older python in write()
   0.3  Mar 31, 2009 - added -pad_to_many_runs, -reverse
   0.4  Apr  8, 2009 - added -show_rows_cols
   0.5  Apr  9, 2009 - added -show_cormat_warnings and -cormat_cutoff
   0.6  Apr 10, 2009 - update for old python versions (e.g. on solaris)
        - each of copy.deepcopy, sum, and sort(reverse=True) failed
        - note all-zero vectors in -show_cormat_warnings
"""

g_version = "1d_tool.py version 0.6, Apr 10, 2009"


class A1DInterface:
   """interface class for Afni1D"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0          # exit value
      self.valid_opts      = None
      self.user_opts       = None

      self.adata           = None       # main Afni1D class instance

      # general variables
      self.infile          = None       # main input file
      self.add_cols_file   = None       # filename to add cols from
      self.overwrite       = 0          # whether to allow overwriting
      self.pad_to_runs     = []         # whether to allow overwriting
      self.reverse         = 0          # reverse data over time
      self.select_cols     = ''         # column selection string
      self.select_rows     = ''         # row selection string

      self.cormat_cutoff   = -1         # if > 0, apply to show_cormat_warns
      self.show_cormat_warn= 0          # show cormat warnings
      self.show_rows_cols  = 0          # show the number of rows and columns

      self.sort            = 0          # sort data over time
      self.transpose       = 0          # transpose the matrix
      self.write_file      = None       # output filename
      self.verb            = verb       # verbose level

      # initialize valid_opts
      self.init_options()

   def init_from_file(self, fname):
      """load a 1D file, and init the main class elements"""

      self.status = 1 # init to failure
      adata = LAD.Afni1D(fname, verb=self.verb)
      if not adata.ready:
         print "** failed to read 1D data from '%s'" % fname
         return 1

      if self.verb > 1: print "++ read 1D data from file '%s'" % fname

      self.fname = fname
      self.adata = adata
      self.status = 0

      return 0

   def write_1D(self, fname):
      """write the current 1D data out (return status)"""
      if not self.adata:
         print '** no 1D data to write'
         return 1
      return self.adata.write(fname, overwrite=self.overwrite)

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

      # required parameter
      self.valid_opts.add_opt('-infile', 1, [], req=1,
                      helpstr='read the given 1D file')

      # general options
      self.valid_opts.add_opt('-add_cols', 1, [],
                      helpstr='extend dataset with columns from new file')

      self.valid_opts.add_opt('-cormat_cutoff', 1, [], 
                      helpstr='set the cutoff for cormat warnings')

      self.valid_opts.add_opt('-overwrite', 0, [], 
                      helpstr='allow overwriting any output files')

      self.valid_opts.add_opt('-pad_into_many_runs', 2, [], 
                      helpstr='make data run #k of N runs (pad with 0)')

      self.valid_opts.add_opt('-reverse', 0, [], 
                      helpstr='reverse the data per column (over time)')

      self.valid_opts.add_opt('-select_cols', 1, [], 
                      helpstr='select the list of columns from the dataset')

      self.valid_opts.add_opt('-select_rows', 1, [], 
                      helpstr='select the list of rows from the dataset')

      self.valid_opts.add_opt('-show_cormat_warnings', 0, [], 
                      helpstr='display warnings for the correlation matrix')

      self.valid_opts.add_opt('-show_rows_cols', 0, [], 
                      helpstr='display the number of rows and columns')

      self.valid_opts.add_opt('-sort', 0, [], 
                      helpstr='sort the data per column (over time)')

      self.valid_opts.add_opt('-transpose', 0, [], 
                      helpstr='transpose the data')

      self.valid_opts.add_opt('-write', 1, [], 
                      helpstr='write 1D data to the given file')

      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self):
      """return None on completion, else error code (0 being okay)"""

      # process terminal options without the option_list interface

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
      # ============================================================
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # process verb first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # process all other options:

      for opt in uopts.olist:

         # ---- main options -----
         if opt.name == '-infile':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.infile = val

         # ----- general options -----

         elif opt.name == '-add_cols':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.add_cols_file = val

         elif opt.name == '-cormat_cutoff':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if err: return 1
            if val >= 0 and val < 1.0: self.cormat_cutoff = val
            else: print '** -cormat_cutoff must be in [0,1)'

         elif opt.name == '-overwrite':
            self.overwrite = 1

         elif opt.name == '-pad_into_many_runs':
            vals, err = uopts.get_type_list(int, '', 
                              len_name='-pad_into_many_runs', opt=opt)
            if err: return 1
            self.pad_to_runs = vals

         elif opt.name == '-reverse':
            self.reverse = 1

         elif opt.name == '-select_cols':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.select_cols = val

         elif opt.name == '-select_rows':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.select_rows = val

         elif opt.name == '-show_cormat_warnings':
            self.show_cormat_warnings = 1

         elif opt.name == '-show_rows_cols':
            self.show_rows_cols = 1

         elif opt.name == '-sort':
            self.sort = 1

         elif opt.name == '-transpose':
            self.transpose = 1

         elif opt.name == '-write':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.write_file = val

      return

   def process_data(self):
      """return None on completion, else error code (0 being okay)"""

      # ---- main options -----
      if not self.infile:
         print '** missing -infile option'
         return 1
      elif self.init_from_file(self.infile): return 1

      if self.add_cols_file:
         newrd = LAD.Afni1D(self.add_cols_file,verb=self.verb)
         if not newrd.ready: return 1
         if self.adata.append_vecs(newrd): return 1

      if self.select_cols:
         ilist=UTIL.decode_1D_ints(self.select_cols, verb=self.verb,
                                                     max=self.adata.nvec-1)
         if ilist == None: return 1
         if self.adata.reduce_by_vec_list(ilist): return 1

      if self.select_rows:
         ilist = UTIL.decode_1D_ints(self.select_rows, verb=self.verb,
                                                       max=self.adata.nt-1)
         if ilist == None: return 1
         if self.adata.reduce_by_tlist(ilist): return 1

      if self.sort:
         self.adata.sort(reverse=self.reverse)  # steal any reverse option
         self.reverse = 0

      if self.reverse:
         if self.adata.reverse(): return 1

      if len(self.pad_to_runs) > 0:
         if self.transpose:
            print '** -transpose is illegal with -pad_into_many_runs'
            return 1
         val = self.pad_to_runs
         if self.adata.pad_into_many_runs(val[0], val[1]): return 1

      if self.transpose:
         if self.adata.transpose(): return 1

      # any 'show' options come after all other processing
      if self.show_rows_cols: self.adata.show_rows_cols(verb=self.verb)

      if self.show_cormat_warnings:
         err, str = self.adata.make_cormat_warnings_string(self.cormat_cutoff,
                                                           name=self.infile)
         print str

      if self.write_file:
         if self.write_1D(self.write_file): return 1

      return

def test(self, verb=3):
      # init
      print '------------------------ initial reads -----------------------'
      self.verb = verb
      # these should not fail, so quit if they do
      # first try AFNI_data4, then regression data

      # reset
      print '------------------------ reset files -----------------------'

      # failures
      print '------------------------ should fail -----------------------'

      # more tests
      return None

def main():
   aint = A1DInterface()
   if not aint: return 1

   rv = aint.process_options()
   if rv != None: return rv

   rv = aint.process_data()
   if rv != None: return rv

   return aint.status

if __name__ == '__main__':
   sys.exit(main())


