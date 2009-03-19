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

   Options are processed in the order they are read.

---------------------------------------------------------------------------
examples (very basic for now):

   1. 1d_tool.py -infile 'data/X.xmat.1D[0..3]{0..5}' -write t1.1D

   2. 1d_tool.py -infile data/X.xmat.1D                  \
                 -select_cols '0..3' -select_rows '0..5' \
                 -write t2.1D
      diff t1.1D t2.1D

   3. 1d_tool.py -infile t3.1D -transpose -write ttr.1D

---------------------------------------------------------------------------
basic informational options:

   -help                        : show this help
   -hist                        : show the module history
   -show_valid_opts             : show all valid options
   -ver                         : show the version number

----------------------------------------
verbose options:

   -verb LEVEL                  : set the verbosity level
   -verbose_opts                : process options in verbose mode

----------------------------------------
required input:

   -infile DATASET.1D           : specify input 1D file

----------------------------------------
general options:

   -add_cos NEW_DSET.1D         : extend dset to include these columns
   -overwrite                   : allow overwriting of any output dataset
   -select_cols SELECTOR        : apply AFNI column selectors, [] is optional
                                  e.g. '[5,0,7..21(2)]'
   -select_rows SELECTOR        : apply AFNI row selectors, {} is optional
                                  e.g. '{5,0,7..21(2)}'
   -sort                        : sort data over time
   -transpose                   : transpose the matrix (rows for columns)
   -write FILE                  : write the current 1D data to FILE

-----------------------------------------------------------------------------
R Reynolds    March 2009
=============================================================================
"""

g_history = """
   1d_tool.py history:    (also refers to lib_afni1D.py)

   0.0  Mar 18, 2009    - initial library (lib_1D.py) version
   0.1  Mar 19, 2009    - added some options and basic help
"""

g_version = "1d_tool.py version 0.1, Mar 19, 2009"


class A1DInterface:
   """interface class for Afni1D"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0          # exit value
      self.valid_opts      = None
      self.user_opts       = None

      self.adata           = None       # main Afni1D class instance

      # general variables
      self.overwrite       = 0          # whether to allow overwriting
      self.verb            = verb

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

      self.valid_opts.add_opt('-overwrite', 0, [], 
                      helpstr='allow overwriting any output files')

      self.valid_opts.add_opt('-select_cols', 1, [], 
                      helpstr='select the list of columns from the dataset')

      self.valid_opts.add_opt('-select_rows', 1, [], 
                      helpstr='select the list of rows from the dataset')

      self.valid_opts.add_opt('-sort', 0, [], 
                      helpstr='sort the data per column (over time)')

      self.valid_opts.add_opt('-transpose', 0, [], 
                      helpstr='transpose the data')

      self.valid_opts.add_opt('-write', 1, [], 
                      helpstr='write 1D data to the given file')

      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      self.valid_opts.add_opt('-verbose_opts', 0, [], 
                      helpstr='make option processing verbose')

      return 0

   def process_options(self):

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

      # ------------------------------------------------------------
      # simple access to verbose processing of arguments
      if '-verbose_opts' in sys.argv: opt_verb = 4
      else:                           opt_verb = 1

      # ============================================================
      # read options specified by the user
      # ============================================================
      self.user_opts = OL.read_options(sys.argv, self.valid_opts, opt_verb)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # process non-chronological options, verb comes first

      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err: self.verb = val

      # ------------------------------------------------------------
      # selection and process options:
      #    process sequentially, to make them like a script

      for opt in uopts.olist:

         # ---- main options -----
         if opt.name == '-infile':
            if self.adata != None:
               print '** only 1 -infile option allowed'
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1
            if self.init_from_file(val): return 1

         # ----- general options -----

         # checking again allows it to change (because that's sooooo likely)
         elif opt.name == '-verb':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if val != None and err: return 1
            else: self.verb = val
            continue

         elif opt.name == '-add_cols':
            if not self.adata:
               print "** '%s' requires -infile" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1

            newrd = LAD.Afni1D(val,verb=self.verb)
            if not newrd.ready: return 1

            self.adata.append_vecs(newrd)

         elif opt.name == '-overwrite':
            self.overwrite = 1

         elif opt.name == '-select_cols':
            if not self.adata:
               print "** '%s' requires -infile" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1
            self.adata.reduce_by_vec_list(UTIL.decode_1D_ints(val))

         elif opt.name == '-select_rows':
            if not self.adata:
               print "** '%s' requires -infile" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1
            self.adata.reduce_by_tlist(UTIL.decode_1D_ints(val))

         elif opt.name == '-sort':
            if not self.adata:
               print "** '%s' requires -infile" % opt.name
               return 1
            self.adata.sort()

         elif opt.name == '-transpose':
            if not self.adata:
               print "** '%s' requires -infile" % opt.name
               return 1
            self.adata.transpose()

         elif opt.name == '-write':
            if not self.adata:
               print "** '%s' requires -infile" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1
            self.write_1D(val)

      return 0

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
   if rv > 0: return 1

   return aint.status

if __name__ == '__main__':
   sys.exit(main())


