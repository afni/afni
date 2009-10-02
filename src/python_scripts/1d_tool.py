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

   7. Output temporal derivative of motion regressors.  There are 9 runs in
      dfile.rall.1D, and derivatives are applied per run.

         1d_tool.py -infile dfile.rall.1D -set_nruns 9 \\
                    -derivative -write motion.deriv.1D

   8. Verify whether labels show slice-major ordering (where all slice0
      regressors come first, then all slice1 regressors, etc).  Either
      show the labels and verify visually, or print whether it is true.

         1d_tool.py -infile scan_2.slibase.1D'[0..12]' -show_labels
         1d_tool.py -infile scan_2.slibase.1D -show_labels
         1d_tool.py -infile scan_2.slibase.1D -show_label_ordering

   9. Given motion.1D, take the derivative (ignoring run breaks) and the
      Euclidean Norm, and write as e.norm.1D.  This might be plotted to show
      show sudden motion as a single time series.

         1d_tool.py -infile motion.1D -set_nruns 9              \\
                    -derivative  -collapse_cols euclidean_norm  \\
                    -write e.norm.1D

  10. Given motion.1D, create censor files to use in 3dDeconvolve, where a
      TR is censored if the derivative values have a Euclidean Norm above 1.2.

      The file created by -write_censor can be used with 3dD's -censor option.
      The file created by -write_CENSORTR can be used with -CENSORTR.  They
      should have the same effect in 3dDeconvolve.  The CENSORTR file is more
      readable, but the censor file is better for plotting against the data.

         1d_tool.py -infile motion.1D -set_nruns 9 -set_tr 3.0     \\
                    -derivative -collapse_cols euclidean_norm      \\
                    -extreme_mask -1.2 1.2                         \\
                    -show_censor_count                             \\
                    -write_censor subjA_censor.1D                  \\
                    -write_CENSORTR subjA_CENSORTR.txt

      The -censor_motion option is available, which implies '-derivative',
      '-collapse_cols euclidean_norm', 'extreme_mask -LIMIT LIMIT', and the
      prefix for '-write_censor' and '-write_CENSORTR' output files.  This
      option will also result in subjA_enorm.1D being written, which is the
      euclidean norm of the derivative, before the extreme mask is applied.

         1d_tool.py -infile motion.1D -set_nruns 9 -set_tr 3.0  \\
                    -show_censor_count                          \\
                    -censor_motion 1.2 subjA

      Consider also '-censor_prev_TR'.

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
   -collapse_cols METHOD        : collapse multiple columns into one, where

        METHOD is one of: min, max, minabs, maxabs, euclidean_norm.

   -censor_motion LIMIT PREFIX  : create censor files

        This option implies '-derivative', '-collapse_cols euclidean_norm',
        'extreme_mask -LIMIT LIMIT' and applies PREFIX for '-write_censor'
        and '-write_CENSORTR' output files.  It also outputs the derivative
        of the euclidean norm, before the limit it applied.

        The temporal derivative is taken with run breaks applied (derivative
        of the first run of a TR is 0), then the columns are collapsed into
        one via each TR's vector length (Euclidean Norm: sqrt(sum of squares)).
        After that, a mask time series is made from TRs with values outside
        (-LIMIT,LIMIT), i.e. if >= LIMIT or <= LIMIT, result is 1.

        This binary time series is then written out in -CENSORTR format, with
        the moderate TRs written in -censor format (either can be applied in
        3dDeconvolve).  The output files will be named PREFIX_censor.1D,
        PREFIX_CENSORTR.txt and PREFIX_enorm.1D (e.g. subj123_censor.1D,
        subj123_CENSORTR.txt and subj123_enorm.1D).

        The other information necessary besides an input motion file (-infile)
        is the number of runs (-set_nruns) and the TR (-set_tr).

        Consider also '-censor_prev_TR'.
        See example 10.

   -censor_prev_TR              : for each censored TR, also censor previous
   -cormat_cutoff CUTOFF        : set cutoff for cormat warnings (in [0,1])
   -derivative                  : take the temporal derivative of each vector
   -extreme_mask MIN MAX        : mask extreme values

        Convert to a 0/1 mask, where 1 means the given value is in [MIN,MAX],
        and 0 means otherwise.  This is useful for censoring motion outliers.

   -overwrite                   : allow overwriting of any output dataset
   -reverse                     : reverse data over time
   -select_cols SELECTOR        : apply AFNI column selectors, [] is optional
                                  e.g. '[5,0,7..21(2)]'
   -select_rows SELECTOR        : apply AFNI row selectors, {} is optional
                                  e.g. '{5,0,7..21(2)}'
   -set_nruns NRUNS             : treat the input data as if it has nruns
                                  (applies to -derivative, for example)
   -set_tr TR                   : set the TR (in seconds) for the data
   -show_censor_count           : display the total number of censored TRs
   -show_cormat_warnings        : display correlation matrix warnings
   -show_label_ordering         : display the labels
   -show_labels                 : display the labels
   -show_rows_cols              : display the number of rows and columns
   -sort                        : sort data over time (smallest to largest)
                                  - sorts EVERY vector
                                  - consider the -reverse option
   -transpose                   : transpose the matrix (rows for columns)
   -write FILE                  : write the current 1D data to FILE
   -write_censor FILE           : write as boolean censor.1D

        e.g. -write_censor subjA_censor.1D

        This file can be given to 3dDeconvolve to censor TRs with excessive
        motion, applied with the -censor option.

            e.g. 3dDeconvolve -censor subjA_censor.1D

        This file works well for plotting against the data, where the 0 entries
        are removed from the regression of 3dDeconvolve.  Alternatively, the
        file created with -write_CENSORTR is probably more human readable.

   -write_CENSORTR FILE         : write censor times as CENSORTR string

        e.g. -write_CENSORTR subjA_CENSORTR.txt

        This file can be given to 3dDeconvolve to censor TRs with excessive
        motion, applied with the -CENSORTR option.

            e.g. 3dDeconvolve -CENSORTR `cat subjA_CENSORTR.txt`

        Which might expand to:

                 3dDeconvolve -CENSORTR '1:16..19,44 3:28 4:19,37..39'

        Note that the -CENSORTR option requires the text on the command line.

        This file is in the easily readable format applied with -CENSORTR.
        It has the same effect on 3dDeconvolve as the sister file from
        -write_censor, above.

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
   0.7  Apr 11, 2009
        - added -derivative and -set_nruns
        - fixed typo in use of -show_cormat_warnings
   0.8  Jul 27, 2009 - added -show_labels and -show_label_ordering
   0.9  Aug 20, 2009
        - added motion censoring, motivated by L Thomas and B Bones
        - added -censor_motion, -censor_prev_TR,  -collapse_cols,
                -extreme_mask, -set_tr, -write_censor, -write_CENSORTR
   0.10 Aug 21, 2009 - added -show_censor_count
   0.11 Aug 25, 2009 - with -censor_motion, also output PREFIX_enorm.1D
   0.12 Oct  2, 2009 also output cosines with -show_cormat_warnings
"""

g_version = "1d_tool.py version 0.12, Oct 2, 2009"


class A1DInterface:
   """interface class for Afni1D"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0          # exit value
      self.valid_opts      = None
      self.user_opts       = None

      self.infile          = None       # main input file
      self.adata           = None       # main Afni1D class instance

      # action variables
      self.add_cols_file   = None       # filename to add cols from
      self.censor_prev_TR  = 0          # if censor, also censor previous TR
      self.collapse_method = ''         # method for collapsing columns
      self.derivative      = 0          # take temporal derivative
      self.overwrite       = 0          # whether to allow overwriting
      self.pad_to_runs     = []         # pad as run #A out of #B runs
      self.reverse         = 0          # reverse data over time
      self.select_cols     = ''         # column selection string
      self.select_rows     = ''         # row selection string
      self.set_extremes    = 0          # apply extreme limits
      self.set_nruns       = 0          # pretend the input is over N runs
      self.set_tr          = 0          # set the TR of the data

      self.cormat_cutoff   = -1         # if > 0, apply to show_cormat_warns
      self.show_censor_count= 0         # show count of censored TRs
      self.show_cormat_warn= 0          # show cormat warnings
      self.show_label_ord  = 0          # show the label ordering
      self.show_labels     = 0          # show the labels
      self.show_rows_cols  = 0          # show the number of rows and columns

      self.sort            = 0          # sort data over time
      self.transpose       = 0          # transpose the matrix
      self.censor_file     = None       # output as 1D censor file
      self.censortr_file   = None       # output as CENSORTR string
      self.collapse_file   = None       # output as 1D collapse file
      self.write_file      = None       # output filename

      # general variables
      self.extreme_min     = 0          # minimum for extreme limit
      self.extreme_max     = 0          # maximum for extreme limit
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
      if self.verb > 1: print '++ writing 1D file %s' % fname
      if not self.adata:
         print '** no 1D data to write'
         return 1
      return self.adata.write(fname, overwrite=self.overwrite)

   def write_timing(self, fname, invert=0):
      """write the current 1D data out as a timing file, where a time
         is written if data at the current TR is set (non-zero)"""
      if not self.adata:
         print '** no 1D data to write as timing'
         return 1
      return self.adata.write_timing(fname, invert=invert)

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

      self.valid_opts.add_opt('-censor_motion', 2, [], 
                      helpstr='censor motion data with LIMIT and PREFIX')

      self.valid_opts.add_opt('-censor_prev_TR', 0, [], 
                      helpstr='if censoring a TR, also censor previous one')

      self.valid_opts.add_opt('-collapse_cols', 1, [], 
                      acplist=['min','max','minabs','maxabs','euclidean_norm'],
                      helpstr='collapse into one column via supplied METHOD')

      self.valid_opts.add_opt('-cormat_cutoff', 1, [], 
                      helpstr='set the cutoff for cormat warnings')

      self.valid_opts.add_opt('-derivative', 0, [], 
                      helpstr='take temporal derivative of each column')

      self.valid_opts.add_opt('-extreme_mask', 2, [], 
                      helpstr='create mask for when values are in [MIN,MAX]')

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

      self.valid_opts.add_opt('-set_nruns', 1, [], 
                      helpstr='specify the number of runs in the input')

      self.valid_opts.add_opt('-set_tr', 1, [], 
                      helpstr='specify the TR (in seconds) of the data')

      self.valid_opts.add_opt('-show_censor_count', 0, [], 
                      helpstr='display the total number of censored TRs')

      self.valid_opts.add_opt('-show_cormat_warnings', 0, [], 
                      helpstr='display warnings for the correlation matrix')

      self.valid_opts.add_opt('-show_label_ordering', 0, [], 
                      helpstr='show whether labels are in slice-major order')

      self.valid_opts.add_opt('-show_labels', 0, [], 
                      helpstr='display the labels from the file')

      self.valid_opts.add_opt('-show_rows_cols', 0, [], 
                      helpstr='display the number of rows and columns')

      self.valid_opts.add_opt('-sort', 0, [], 
                      helpstr='sort the data per column (over time)')

      self.valid_opts.add_opt('-transpose', 0, [], 
                      helpstr='transpose the data')

      self.valid_opts.add_opt('-write', 1, [], 
                      helpstr='write 1D data to the given file')

      self.valid_opts.add_opt('-write_censor', 1, [], 
                      helpstr='write as 1D censor file')

      self.valid_opts.add_opt('-write_CENSORTR', 1, [], 
                      helpstr='write CENSORTR format string to file')

      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self):
      """return None on completion, else error code (0 being okay)"""

      # process terminal options without the option_list interface

      self.valid_opts.check_special_opts(sys.argv)

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

         elif opt.name == '-set_nruns':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if err: return 1
            if val > 0: self.set_nruns = val
            else:
               print '** -set_nruns must be positive'
               return 1

         elif opt.name == '-set_tr':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if err: return 1
            if val > 0: self.set_tr = val
            else:
               print '** -set_tr must be positive'
               return 1

         elif opt.name == '-censor_motion':
            val, err = uopts.get_string_list('', opt=opt)
            if err: return 1
            try: limit = float(val[0])
            except:
               print "** -censor_motion: bad limit '%s'" % val[0]
               return 1
            if limit < 0:
               print "** -censor_motion: LIMIT must be positive, have %g"%limit
               return 1
            # check for redundant options
            errors = 0
            olist = ['-derivative', '-collapse_cols', '-extreme_mask',
                     '-write_censor', '-write_CENSORTR']
            for oname in olist:
               if uopts.find_opt(oname):
                  print "** option %s is redundant with -censor_motion" % oname
                  errors += 1
            if errors:
               ss = "\n** -censor_motion implies each of: %s"%', '.join(olist)
               print UTIL.add_line_wrappers(ss, wrapstr='\n')
               return 1
            # set implied options
            self.derivative = 1
            self.collapse_method = 'euclidean_norm'
            self.set_extremes    = 1
            self.extreme_min     = -limit
            self.extreme_max     = limit
            self.censor_file     = '%s_censor.1D' % val[1]
            self.censortr_file   = '%s_CENSORTR.txt' % val[1]
            self.collapse_file   = '%s_enorm.1D' % val[1]

         elif opt.name == '-censor_prev_TR':
            self.censor_prev_TR = 1

         elif opt.name == '-collapse_cols':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.collapse_method = val

         elif opt.name == '-cormat_cutoff':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if err: return 1
            if val >= 0 and val < 1.0: self.cormat_cutoff = val
            else:
               print '** -cormat_cutoff must be in [0,1)'
               return 1

         elif opt.name == '-derivative':
            self.derivative = 1

         elif opt.name == '-extreme_mask':
            val, err = uopts.get_type_list(float, '', opt=opt)
            if err: return 1
            if val[0]<=val[1]:
               self.set_extremes = 1
               self.extreme_min = val[0]
               self.extreme_max = val[1]
            else:
               print '** -extreme_mask: must have min <= max'
               return 1

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
            self.show_cormat_warn = 1

         elif opt.name == '-show_censor_count':
            self.show_censor_count = 1

         elif opt.name == '-show_label_ordering':
            self.show_label_ord = 1

         elif opt.name == '-show_labels':
            self.show_labels = 1

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

         elif opt.name == '-write_censor':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.censor_file = val

         elif opt.name == '-write_CENSORTR':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.censortr_file = val

      return

   def process_data(self):
      """return None on completion, else error code (0 being okay)"""

      # ---- data input options -----

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

      # ---- processing options -----

      if self.set_nruns > 0:
         if self.adata.set_nruns(self.set_nruns): return 1

      if self.set_tr > 0: self.adata.tr = self.set_tr

      if self.derivative:
         if self.adata.derivative(): return 1

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

      if self.collapse_method:
         if self.adata.collapse_cols(self.collapse_method): return 1
         if self.collapse_file:
            if self.write_1D(self.collapse_file): return 1

      if self.set_extremes:
         if self.adata.extreme_mask(self.extreme_min, self.extreme_max):
            return 1

      if self.censor_prev_TR:
         if self.adata.mask_prior_TRs(): return 1

      # ---- 'show' options come after all other processing ----

      if self.show_label_ord: self.adata.show_major_order_of_labels()
      if self.show_labels: self.adata.show_labels()

      if self.show_rows_cols: self.adata.show_rows_cols(verb=self.verb)

      if self.show_censor_count: self.adata.show_censor_count()

      if self.show_cormat_warn:
         err, str = self.adata.make_cormat_warnings_string(self.cormat_cutoff,
                                                           name=self.infile)
         print str

      # ---- possibly write: last option -----

      if self.censortr_file:
         if self.adata.write_censortr(self.censortr_file): return 1

      if self.censor_file:
         if self.adata.bool_negate(): return 1
         if self.write_1D(self.censor_file): return 1

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


