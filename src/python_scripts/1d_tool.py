#!/usr/bin/env python

# system libraries
import sys, os

if 0 :  # for testing, might add the current dir and ~/abin to the PATH
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

   1.  Select by rows and columns, akin to 1dcat.

         1d_tool.py -infile 'data/X.xmat.1D[0..3]{0..5}' -write t1.1D

   2.  Compare with selection by separate options.

         1d_tool.py -infile data/X.xmat.1D                  \\
                    -select_cols '0..3' -select_rows '0..5' \\
                    -write t2.1D
         diff t1.1D t2.1D

   3.  Transpose a dataset, akin to 1dtranspose.

         1d_tool.py -infile t3.1D -transpose -write ttr.1D

   4a. Pad a file of regressors for a single run (#2) with zeros, so
       that it becomes run 2 of 7 (runs are 1-based).

         1d_tool.py -infile ricor_r02.1D -pad_into_many_runs 2 7 \\
                    -write ricor_r02_all.1D

   4b. Similar to 4a, but specify varying TRs per run.  The number of
       runs must match the number of run_lengths parameters.

         1d_tool.py -infile ricor_r02.1D -pad_into_many_runs 2 7 \\
                    -set_run_lengths 64 61 67 61 67 61 67        \\
                    -write ricor_r02_all.1D

   5.  Display small details about a 1D dataset:

       a. Display number of rows and columns for a 1D dataset.

         1d_tool.py -infile ricor_r02.1D -show_rows_cols

       b. Display indices of regressors of interest.

         1d_tool.py -infile X.xmat.1D -show_indices_interest

   6.  Show correlation matrix warnings for this matrix.

         1d_tool.py -infile X.xmat.1D -show_cormat_warnings

   7a. Output temporal derivative of motion regressors.  There are 9 runs in
       dfile.rall.1D, and derivatives are applied per run.

         1d_tool.py -infile dfile.rall.1D -set_nruns 9 \\
                    -derivative -write motion.deriv.1D

   7b. Similar to 7a, but let the run lengths vary.  The sum of run lengths
       should equal the number of time points.

         1d_tool.py -infile dfile.rall.1D                       \\
                    -set_run_lengths 64 64 64 64 64 64 64 64 64 \\
                    -derivative -write motion.deriv.rlens.1D

   8.  Verify whether labels show slice-major ordering (where all slice0
       regressors come first, then all slice1 regressors, etc).  Either
       show the labels and verify visually, or print whether it is true.

         1d_tool.py -infile scan_2.slibase.1D'[0..12]' -show_labels
         1d_tool.py -infile scan_2.slibase.1D -show_labels
         1d_tool.py -infile scan_2.slibase.1D -show_label_ordering

   9a. Given motion.1D, take the derivative (ignoring run breaks) and the
       Euclidean Norm, and write as e.norm.1D.  This might be plotted to show
       show sudden motion as a single time series.

         1d_tool.py -infile motion.1D -set_nruns 9              \\
                    -derivative  -collapse_cols euclidean_norm  \\
                    -write e.norm.1D

   9b. Similar to 9a, but supposing the run lengths vary (still 576 TRs).

         1d_tool.py -infile motion.1D                           \\
                    -set_run_lengths 64 61 67 61 67 61 67 61 67 \\
                    -derivative  -collapse_cols euclidean_norm  \\
                    -write e.norm.rlens.1D

  10.  Given motion.1D, create censor files to use in 3dDeconvolve, where a
       TR is censored if the derivative values have a Euclidean Norm above 1.2.

       The file created by -write_censor can be used with 3dD's -censor option.
       The file created by -write_CENSORTR can be used with -CENSORTR.  They
       should have the same effect in 3dDeconvolve.  The CENSORTR file is more
       readable, but the censor file is better for plotting against the data.
 
       a. general example

          1d_tool.py -infile motion.1D -set_nruns 9                 \\
                     -derivative -collapse_cols euclidean_norm      \\
                     -extreme_mask -1.2 1.2                         \\
                     -show_censor_count                             \\
                     -write_censor subjA_censor.1D                  \\
                     -write_CENSORTR subjA_CENSORTR.txt

       b. using -censor_motion

          The -censor_motion option is available, which implies '-derivative',
          '-collapse_cols euclidean_norm', 'extreme_mask -LIMIT LIMIT', and the
          prefix for '-write_censor' and '-write_CENSORTR' output files.  This
          option will also result in subjA_enorm.1D being written, which is the
          euclidean norm of the derivative, before the extreme mask is applied.

          1d_tool.py -infile motion.1D -set_nruns 9     \\
                     -show_censor_count                 \\
                     -censor_motion 1.2 subjA

       c. allow the run lengths to vary

          1d_tool.py -infile motion.1D                           \\
                     -set_run_lengths 64 61 67 61 67 61 67 61 67 \\
                     -show_censor_count                          \\
                     -censor_motion 1.2 subjA_rlens

       Consider also '-censor_prev_TR' and '-censor_first_trs'.

  11.  Demean the data.  Use motion parameters as an example.

       The demean operation is done per run (default is 1 when 1d_tool.py
       does not otherwise know).

       a. across all runs (if runs are not known from input file)

         1d_tool.py -infile dfile.rall.1D -demean -write motion.demean.a.1D

       b. per run, over 9 runs of equal length

         1d_tool.py -infile dfile.rall.1D -set_nruns 9      \\
                -demean -write motion.demean.b.1D

       c. per run, over 9 runs of varying length

         1d_tool.py -infile dfile.rall.1D                   \\
                -set_run_lengths 64 61 67 61 67 61 67 61 67 \\
                -demean -write motion.demean.c.1D

  12.  "Uncensor" the data, zero-padding previously censored TRs.

       Note that an X-matrix output by 3dDeconvolve contains censor
       information in GoodList, which is the list of uncensored TRs.

       a. if the input dataset has censor information

         1d_tool.py -infile X.xmat.1D -censor_fill -write X.uncensored.1D

       b. if censor information needs to come from a parent

         1d_tool.py -infile sum.ideal.1D -censor_fill_parent X.xmat.1D \\
                    -write sum.ideal.uncensored.1D

  13. Show whether the input file is valid as a numeric data file.

       a. as any generic 1D file

          1d_tool.py -infile data.txt -looks_like_1D

       b. as a 1D stim_file, of 3 runs of 64 TRs (TR is irrelevant)

          1d_tool.py -infile data.txt -looks_like_1D \\
                     -set_run_lengths 64 64 64

       c. as a stim_times file with local times

          1d_tool.py -infile data.txt -looks_like_local_times \\
                     -set_run_lengths 64 64 64 -set_tr 2

       d. as a 1D or stim_times file with global times

          1d_tool.py -infile data.txt -looks_like_global_times \\
                     -set_run_lengths 64 64 64 -set_tr 2

       e. perform all tests, reporting all errors

          1d_tool.py -infile data.txt -looks_like_test_all \\
                     -set_run_lengths 64 64 64 -set_tr 2

   14. Split motion parameters across runs, but keep them at the original
       length so they apply to the same multi-run regression.  Each file will
       be the same as the original for the run it applies to, but zero across
       all other runs.

       Note that -split_into_pad_runs takes the output prefix as a parameter.

         1d_tool.py -infile motion.1D                   \\
                    -set_run_lengths 64 64 64           \\
                    -split_into_pad_runs mot.padded

       The output files are:
          mot.padded.r01.1D   mot.padded.r02.1D   mot.padded.r03.1D

       If the run lengths are the same -set_nruns is shorter...

         1d_tool.py -infile motion.1D                   \\
                    -set_nruns 3                        \\
                    -split_into_pad_runs mot.padded

   15. Show the maximum pairwise displacement in the motion parameter file.
       So over all TRs pairs, find the biggest displacement.

       In one direction it is easy (AP say).  If the minimum AP shift is -0.8
       and the maximum is 1.5, then the maximum displacement is 2.3 mm.  It
       is less clear in 6-D space, and instead of trying to find an enveloping
       set of "coordinates", distances between all N choose 2 pairs are
       evaluated (brute force).

        1d_tool.py -infile dfile.rall.1D -show_max_displace

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

        Besides an input motion file (-infile), the number of runs is needed
        (-set_nruns or -set_run_lengths).

        Consider also '-censor_prev_TR' and '-censor_first_trs'.
        See example 10.

   -censor_fill                 : expand data, filling censored TRs with zeros
   -censor_fill_parent PARENT   : similar, but get censor info from a parent

        The output of these operation is a longer dataset.  Each TR that had
        previously been censored is re-inserted as a zero.

        The purpose of this is to make 1D time series data properly align
        with the all_runs dataset, for example.  Otherwise, the ideal 1D data
        might have missing TRs, and so will align worse with responses over
        the duration of all runs (it might start aligned, but drift earlier
        and earlier as more TRs are censored).

        See example 12.

   -censor_first_trs N          : when censoring motion, also censor the first
                                  N TRs of each run
   -censor_prev_TR              : for each censored TR, also censor previous
   -cormat_cutoff CUTOFF        : set cutoff for cormat warnings (in [0,1])
   -demean                      : demean each run (new mean of each run = 0.0)
   -derivative                  : take the temporal derivative of each vector
   -extreme_mask MIN MAX        : mask extreme values

        Convert to a 0/1 mask, where 1 means the given value is in [MIN,MAX],
        and 0 means otherwise.  This is useful for censoring motion outliers.

   "Looks like" options:

        These are terminal options that check whether the input file seems to
        be of type 1D, local stim_times or global stim_times formats.  The only
        associated options are currently -input, -set_run_lens, -set_tr and
        -verb.

        They are terminal in that no other 1D-style actions are performed.
        See 'timing_tool.py -help' for details on stim_times operations.

   -looks_like_1D               : is the file in 1D format

        Does the input data file seem to be in 1D format?

            - must be rectangular (same number of columns per row)
            - duration must match number of rows (if run lengths are given)

   -looks_like_local_times      : is the file in local stim_times format

        Does the input data file seem to be in the -stim_times format used by
        3dDeconvolve (and timing_tool.py)?  More specifically, is it the local
        format, with one scanning run per row.

            - number of rows must match number of runs
            - times cannot be negative
            - times must be unique per run (per row)
            - times cannot exceed the current run time

   -looks_like_global_times     : is the file in global stim_times format

        Does the input data file seem to be in the -stim_times format used by
        3dDeconvolve (and timing_tool.py)?  More specifically, is it the global
        format, either as one long row or one long line?

            - must be one dimensional (either a single row or column)
            - times cannot be negative
            - times must be unique
            - times cannot exceed total duration of all runs

   -looks_like_test_all         : run all -looks_like tests

        Applies all "looks like" test options: -looks_like_1D,
        -looks_like_local_times and -looks_like_global_times.

   -overwrite                   : allow overwriting of any output dataset
   -reverse                     : reverse data over time
   -select_cols SELECTOR        : apply AFNI column selectors, [] is optional
                                  e.g. '[5,0,7..21(2)]'
   -select_rows SELECTOR        : apply AFNI row selectors, {} is optional
                                  e.g. '{5,0,7..21(2)}'
   -set_nruns NRUNS             : treat the input data as if it has nruns
                                  (e.g. applies to -derivative and -demean)

        See examples 7a, 10a and b, and 14.

   -set_run_lengths N1 N2 ...   : treat as if data has run lengths N1, N2, etc.
                                  (applies to -derivative, for example)

        Notes:  o  option -set_nruns is not allowed with -set_run_lengths
                o  the sum of run lengths must equal NT

        See examples 7b, 10c and 14.

   -set_tr TR                   : set the TR (in seconds) for the data
   -show_censor_count           : display the total number of censored TRs
   -show_cormat_warnings        : display correlation matrix warnings
   -show_label_ordering         : display the labels
   -show_labels                 : display the labels
   -show_indices_baseline       : display column indices for baseline
   -show_indices_motion         : display column indices for motion regressors
   -show_indices_interest       : display column indices for regs of interest
   -show_max_displace           : display max displacement (from motion params)
                                  - the maximum pairwise distance (enorm)
   -show_rows_cols              : display the number of rows and columns
   -sort                        : sort data over time (smallest to largest)
                                  - sorts EVERY vector
                                  - consider the -reverse option

   -split_into_pad_runs PREFIX  : split input into one padded file per run

        e.g. -split_into_pad_runs motion.pad

        This option is used for breaking a set of regressors up by run.  The
        output would be one file per run, where each file is the same as the
        input for the run it corresponds to, and is padded with 0 across all
        other runs.

        Assuming the 300 row input dataset spans 3 100-TR runs, then there
        would be 3 output datasets created, each still be 300 rows:

            motion.pad.r01.1D   : 100 rows as input, 200 rows of 0
            motion.pad.r02.1D   : 100 rows of 0, 100 rows as input, 100 of 0
            motion.pad.r03.1D   : 200 rows of 0, 100 rows as input

        This option requires either -set_nruns or -set_run_lengths.

        See example 14.

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
   0.12 Oct  2, 2009 - also output cosines with -show_cormat_warnings
   0.13 Oct  6, 2009 - added -set_run_lengths option
   0.14 Oct 15, 2009 - added -demean
   0.15 Oct 23, 2009 - added -censor_fill and -censor_fill_par
   0.16 Nov 16, 2009 - allow motion censoring with varying run lengths
   0.17 Mar 25, 2010 - small help update
   0.18 Mar 25, 2010 - added -censor_first_trs for A Barbey
   0.19 Jul 30, 2010 - added "Looks like" options
        - added -looks_like_1D, -looks_like_local_times,
                -looks_like_global_times, -looks_like_test_all
   0.20 Aug 02, 2010
        - small change to looks_like text formatting
        - removed useless TR from looks_like_1D function
   0.21 Oct 29, 2010 - added -show_indices_baseline, _motion and _interest
   0.22 Nov  4, 2010 - fixed print vs. return problem in -show_indices
   0.23 Dec 16, 2010 - updates to file type (looks like) errors and warnings
   0.24 May 27, 2010 - added -split_into_pad_runs (for regress motion per run)
   1.00 Jul 14, 2011
        - call this a release version, kept forgetting
          (maybe release v2 can be when dealing with married timing is robust)
        - added -show_max_displace (see example 15)
"""

g_version = "1d_tool.py version 1.00, July 14, 2011"


class A1DInterface:
   """interface class for Afni1D"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0          # exit value
      self.valid_opts      = None
      self.user_opts       = None

      self.infile          = None       # main input file
      self.adata           = None       # main Afni1D class instance
      self.dtype           = 0          # 1=Afni1D, 2=AfniData

      # action variables
      self.add_cols_file   = None       # filename to add cols from
      self.censor_fill     = 0          # zero-fill censored TRs
      self.censor_fill_par = ''         # same, but via this parent dset
      self.censor_first_trs= 0          # number of first TRs to also censor
      self.censor_prev_TR  = 0          # if censor, also censor previous TR
      self.collapse_method = ''         # method for collapsing columns
      self.demean          = 0          # demean the data
      self.derivative      = 0          # take temporal derivative
      self.overwrite       = 0          # whether to allow overwriting
      self.pad_to_runs     = []         # pad as run #A out of #B runs
      self.reverse         = 0          # reverse data over time
      self.select_cols     = ''         # column selection string
      self.select_rows     = ''         # row selection string
      self.set_extremes    = 0          # apply extreme limits
      self.set_nruns       = 0          # assume input is over N runs
      self.set_run_lengths = []         # assume input has these run lengths
      self.set_tr          = 0          # set the TR of the data
      self.split_into_pad_runs = ''     # prefix, for splitting into many runs

      self.cormat_cutoff   = -1         # if > 0, apply to show_cormat_warns
      self.show_censor_count= 0         # show count of censored TRs
      self.show_cormat_warn= 0          # show cormat warnings
      self.show_displace   = 0          # max_displacement (0,1,2)
      self.show_indices    = 0          # bitmask for index lists to show
                                        # (base, motion, regs of interest)
      self.show_label_ord  = 0          # show the label ordering
      self.show_labels     = 0          # show the labels
      self.show_rows_cols  = 0          # show the number of rows and columns
                                

      self.sort            = 0          # sort data over time
      self.transpose       = 0          # transpose the matrix
      self.censor_file     = None       # output as 1D censor file
      self.censortr_file   = None       # output as CENSORTR string
      self.collapse_file   = None       # output as 1D collapse file
      self.write_file      = None       # output filename

      # test variables
      self.looks_like      = 0          # 1,2,4,8 = TEST,1D,local,global

      # general variables
      self.extreme_min     = 0          # minimum for extreme limit
      self.extreme_max     = 0          # maximum for extreme limit
      self.verb            = verb       # verbose level

      # initialize valid_opts
      self.init_options()

   def init_from_file(self, fname):
      """load a 1D file, and init the main class elements"""

      self.status = 1 # init to failure

      # the looks_like options imply AfniData, else use Afni1D
      if self.looks_like:
         adata = LAD.AfniData(fname, verb=self.verb)
         self.dtype = 2
      else:
         adata = LAD.Afni1D(fname, verb=self.verb)
         self.dtype = 1

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

   def write_as_timing(self, fname, invert=0):
      """write the current 1D data out as a timing file, where a time
         is written if data at the current TR is set (non-zero)"""
      if not self.adata:
         print '** no 1D data to write as timing'
         return 1
      return self.adata.write_as_timing(fname, invert=invert)

   def write_split_into_pad_runs(self, prefix):
      """break input file into one file per run, where each output file has
         all non-current runs as 0 (so file lengths stay the same)"""
      if not self.adata:
         print '** no 1D data to write as timing'
         return 1
      if self.set_nruns == 0 and len(self.set_run_lengths) == 0:
         print '** -split_into_pad_runs requires -set_nruns or -set_run_lengths'
         return 1
      return self.adata.split_into_padded_runs(prefix, overwrite=self.overwrite)

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

      self.valid_opts.add_opt('-censor_fill', 0, [], 
                      helpstr='zero-fill previously censored TRs')

      self.valid_opts.add_opt('-censor_fill_parent', 1, [], 
                      helpstr='-censor_fill, but via this parent dataset')

      self.valid_opts.add_opt('-censor_first_trs', 1, [], 
                      helpstr='number of initial TRs to censor, per run')

      self.valid_opts.add_opt('-censor_motion', 2, [], 
                      helpstr='censor motion data with LIMIT and PREFIX')

      self.valid_opts.add_opt('-censor_prev_TR', 0, [], 
                      helpstr='if censoring a TR, also censor previous one')

      self.valid_opts.add_opt('-collapse_cols', 1, [], 
                      acplist=['min','max','minabs','maxabs','euclidean_norm'],
                      helpstr='collapse into one column via supplied METHOD')

      self.valid_opts.add_opt('-cormat_cutoff', 1, [], 
                      helpstr='set the cutoff for cormat warnings')

      self.valid_opts.add_opt('-demean', 0, [], 
                      helpstr='demean each run of each column')

      self.valid_opts.add_opt('-derivative', 0, [], 
                      helpstr='take temporal derivative of each column')

      self.valid_opts.add_opt('-extreme_mask', 2, [], 
                      helpstr='create mask for when values are in [MIN,MAX]')

      self.valid_opts.add_opt('-looks_like_1D', 0, [], 
                      helpstr='show whether file has 1D format')

      self.valid_opts.add_opt('-looks_like_local_times', 0, [], 
                      helpstr='show whether file has local stim_times format')

      self.valid_opts.add_opt('-looks_like_global_times', 0, [], 
                      helpstr='show whether file has global stim_times format')

      self.valid_opts.add_opt('-looks_like_test_all', 0, [], 
                      helpstr='test file for all 1D and timing formats')

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

      self.valid_opts.add_opt('-set_run_lengths', -1, [], 
                      helpstr='specify the lengths of all runs')

      self.valid_opts.add_opt('-set_tr', 1, [], 
                      helpstr='specify the TR (in seconds) of the data')

      self.valid_opts.add_opt('-show_censor_count', 0, [], 
                      helpstr='display the total number of censored TRs')

      self.valid_opts.add_opt('-show_cormat_warnings', 0, [], 
                      helpstr='display warnings for the correlation matrix')

      self.valid_opts.add_opt('-show_indices_baseline', 0, [], 
                      helpstr='display index list for baseline regressors')

      self.valid_opts.add_opt('-show_indices_interest', 0, [], 
                      helpstr='display index list for regressors of interest')

      self.valid_opts.add_opt('-show_indices_motion', 0, [], 
                      helpstr='display index list for motion regressors')

      self.valid_opts.add_opt('-show_label_ordering', 0, [], 
                      helpstr='show whether labels are in slice-major order')

      self.valid_opts.add_opt('-show_labels', 0, [], 
                      helpstr='display the labels from the file')

      self.valid_opts.add_opt('-show_max_displace', 0, [], 
                      helpstr='display maximum displacements over TRs')

      self.valid_opts.add_opt('-show_rows_cols', 0, [], 
                      helpstr='display the number of rows and columns')

      self.valid_opts.add_opt('-sort', 0, [], 
                      helpstr='sort the data per column (over time)')

      self.valid_opts.add_opt('-split_into_pad_runs', 1, [], 
                      helpstr='write input as one zero-padded file per run')

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
            if len(self.set_run_lengths) > 0:
               print '** cannot use both -set_nruns and -set_run_lengths'
               return 1

         elif opt.name == '-set_run_lengths':
            val, err = uopts.get_type_list(int, '', opt=opt)
            if err: return 1
            self.set_run_lengths = val
            if self.set_nruns > 0:
               print '** cannot use both -set_nruns and -set_run_lengths'
               return 1

         elif opt.name == '-set_tr':
            val, err = uopts.get_type_opt(float, '', opt=opt)
            if err: return 1
            if val > 0: self.set_tr = val
            else:
               print '** -set_tr must be positive'
               return 1

         elif opt.name == '-censor_fill':
            self.censor_fill = 1

         elif opt.name == '-censor_fill_parent':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.censor_fill_par = val

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

         elif opt.name == '-censor_first_trs':
            val, err = uopts.get_type_opt(int, '', opt=opt)
            if err: return 1
            self.censor_first_trs = val

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

         elif opt.name == '-demean':
            self.demean = 1

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

         # looks_like options, to test AfniData (not Afni1D)
         elif opt.name == '-looks_like_1D':
            self.looks_like |= 2
         elif opt.name == '-looks_like_local_times':
            self.looks_like |= 4
         elif opt.name == '-looks_like_global_times':
            self.looks_like |= 8
         elif opt.name == '-looks_like_test_all':
            self.looks_like = -1

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

         elif opt.name == '-show_indices_baseline':
            self.show_indices |= 1

         elif opt.name == '-show_indices_motion':
            self.show_indices |= 2

         elif opt.name == '-show_indices_interest':
            self.show_indices |= 4

         elif opt.name == '-show_label_ordering':
            self.show_label_ord = 1

         elif opt.name == '-show_labels':
            self.show_labels = 1

         elif opt.name == '-show_max_displace':
            self.show_displace = 3

         elif opt.name == '-show_rows_cols':
            self.show_rows_cols = 1

         elif opt.name == '-sort':
            self.sort = 1

         elif opt.name == '-split_into_pad_runs':
            val, err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.split_into_pad_runs = val

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

   def process_afnidata(self):
      """return None on completion, else error code (0 being okay)"""

      if not self.adata.ready and self.dtype != 2:
         print '** not ready to process AfniData'
         return 1

      if self.verb > 1:
         print '++ process_afnidata: looks_like = %d' %  self.looks_like

      if not self.looks_like:
         print '** no looks_like action to perform on AfniData'
         return 1

      # use verb of at least 1 to print result
      verb = self.verb
      if verb < 1: verb = 1

      if self.looks_like & 2:
         self.adata.looks_like_1D(run_lens=self.set_run_lengths, verb=verb)

      if self.looks_like & 4:
         self.adata.looks_like_local_times(run_lens=self.set_run_lengths,
                                           tr=self.set_tr, verb=verb)

      if self.looks_like & 8:
         self.adata.looks_like_global_times(run_lens=self.set_run_lengths,
                                            tr=self.set_tr, verb=verb)

      return 0

   def process_data(self):
      """return None on completion, else error code (0 being okay)"""

      # ---- data input options -----

      if not self.infile:
         print '** missing -infile option'
         return 1
      elif self.init_from_file(self.infile): return 1

      # process AfniData separately
      if self.dtype == 2: return self.process_afnidata()

      if self.add_cols_file:
         newrd = LAD.Afni1D(self.add_cols_file,verb=self.verb)
         if not newrd.ready: return 1
         if self.adata.append_vecs(newrd): return 1

      if self.select_cols:
         ilist=UTIL.decode_1D_ints(self.select_cols, verb=self.verb,
                                                     imax=self.adata.nvec-1)
         if ilist == None: return 1
         if self.adata.reduce_by_vec_list(ilist): return 1

      if self.select_rows:
         ilist = UTIL.decode_1D_ints(self.select_rows, verb=self.verb,
                                                       imax=self.adata.nt-1)
         if ilist == None: return 1
         if self.adata.reduce_by_tlist(ilist): return 1

      # ---- processing options -----

      if self.set_nruns > 0:
         if self.adata.set_nruns(self.set_nruns): return 1

      # pad_to_runs is special case, do not set run info
      if len(self.set_run_lengths) > 0 and not self.pad_to_runs:
         if self.adata.set_nruns(run_lens=self.set_run_lengths): return 1

      if self.set_tr > 0: self.adata.tr = self.set_tr

      if self.derivative:
         if self.adata.derivative(): return 1

      if self.demean:
         if self.adata.demean(): return 1

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
         rlens = self.set_run_lengths # pass empty or not
         # if run_len list, verify number of runs
         if len(rlens) > 0 and len(rlens) != val[1]:
            print '** -pad_into_many_runs: nruns=%d != length of run list: %s'\
                  % (val[1], rlens)
            return 1
         if self.adata.pad_into_many_runs(val[0], val[1], rlens): return 1

      if self.transpose:
         if self.adata.transpose(): return 1

      if self.collapse_method:
         if self.adata.collapse_cols(self.collapse_method): return 1
         if self.collapse_file:
            if self.write_1D(self.collapse_file): return 1

      if self.set_extremes:
         if self.adata.extreme_mask(self.extreme_min, self.extreme_max):
            return 1

      if self.censor_fill:
         if self.adata.apply_goodlist(padbad=1): return 1

      if self.censor_fill_par:
         parent = LAD.Afni1D(self.censor_fill_par, verb=self.verb)
         if self.adata.apply_goodlist(padbad=1, parent=parent): return 1

      if self.censor_prev_TR:
         if self.adata.mask_prior_TRs(): return 1

      if self.censor_first_trs:
         if self.censor_file == None: cval = 0  # censor by nuking
         else:                        cval = 1  # will invert later
         if self.adata.mask_first_TRs(self.censor_first_trs, cval): return 1

      # ---- 'show' options come after all other processing ----

      if self.show_label_ord: self.adata.show_major_order_of_labels()
      if self.show_labels: self.adata.show_labels()

      if self.show_indices:
         istr = self.adata.get_indices_str(self.show_indices)
         print istr

      if self.show_displace:
         print self.adata.get_max_displacement_str(verb=self.verb)

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

      if self.split_into_pad_runs:
         if self.write_split_into_pad_runs(self.split_into_pad_runs): return 1

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


