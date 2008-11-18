#!/usr/bin/env python

# system libraries
import sys
import gc, math

# AFNI libraries
import option_list as OL
import afni_xmat as AM
import afni_util as UTIL

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
xmat_tool     - a tool for evaluating an AFNI X-matrix

   This program gives the user the ability to evaluate a regression matrix
   (often referred to as an X-matrix).  With an AFNI X-matrix specified via
   -load_xmat, optionally along with an MRI time series specified via
   -load_1D, this program can display the:

         o  matrix condition numbers
         o  correlation matrix
         o  warnings regarding the correlation matrix
         o  cosine matrix (normalized XtX)
         o  warnings regarding the cosine matrix
         o  beta weights for fit against 1D time series
         o  fit time series

   --------------------------------------------------------------------------
   examples:

      Note that -no_gui is applied in each example, so that the program
      performs any requested actions and terminates, without opening a GUI
      (graphical user interface).

      1. Load an X-matrix and display the condition numbers.

         xmat_tool.py -no_gui -load_xmat X.xmat.1D -show_conds

      2. Load an X-matrix and display correlation and cosine warnings.

         xmat_tool.py -no_gui -load_xmat X.xmat.1D      \\
             -show_cormat_warnings -show_cosmat_warnings

      3. Load an X-matrix and a 1D time series.  Display beta weights for
         the best fit to all regressors (specifed as columns 0 to the last).

         xmat_tool.py -no_gui -load_xmat X.xmat.1D -load_1D norm.ts.1D \\
             -choose_cols '0..$' -show_fit_betas

      4. Similar to 3, but show the actual fit time series.  Also, redirect
         the output to save the results in a 1D file.

         xmat_tool.py -no_gui -load_xmat X.xmat.1D -load_1D norm.ts.1D \\
             -choose_cols '0..$' -show_fit_ts > fitts.1D

      5. Show many things.  Load an X-matrix and time series, and display
         conditions and warnings (but setting own cutoff values), as well as
         fit betas.

         xmat_tool.py -no_gui -load_xmat X.xmat.1D -load_1D norm.ts.1D  \\
             -choose_cols '0..$'                                        \\
             -show_conds                                                \\
             -cormat_cutoff 0.3 -cosmat_cutoff 0.25                     \\
             -show_cormat_warnings -show_cosmat_warnings                \\
             -show_fit_betas

      6. Script many operations.  Load a sequence of X-matrices, and display
         condition numbers and warnings for each.

         Note that with -chrono, options are applied chronologically.

         xmat_tool.py -no_gui -chrono                                   \\
                -load_xmat X.1.xmat.1D                                  \\
                -show_conds -show_cormat_warnings -show_cosmat_warnings \\
                -load_xmat X.2.xmat.1D                                  \\
                -show_conds -show_cormat_warnings -show_cosmat_warnings \\
                -load_xmat X.3.xmat.1D                                  \\
                -show_conds -show_cormat_warnings -show_cosmat_warnings \\
                -load_1D norm.ts.1D                                     \\
                -show_fit_betas                                         \\
                -choose_cols '0..$'                                     \\
                -show_fit_betas                                         \\
                -choose_cols '0..26,36..$'                              \\
                -show_fit_betas                                         \\
                -load_xmat X.2.xmat.1D                                  \\
                -choose_cols '0..$'                                     \\
                -show_fit_betas

   --------------------------------------------------------------------------
   basic informational options:

      -help                           : show this help
      -help_gui                       : show the GUI help
      -hist                           : show the module history
      -show_valid_opts                : show all valid options
      -test                           : run a basic test
                               (requires X.xmat.1D and norm.022_043_012.1D)
      -ver                            : show the version number

   ------------------------------------------
   general options:

      -choose_cols 'COLUMN LIST'      : select columns to fit against

          e.g. -choose_cols '0..$'
          e.g. -choose_cols '1..19(3),26,29,40..$'

          These columns will be used as the basis for the top condition
          number, as well as the regressor columns for fit computations.

          The column selection string should not contain spaces, and should
          be in the format of AFNI sub-brick selection.  Consider these
          examples

              2..13           : 2,3,4,5,6,7,8,9,10,11,12,13
              2..13(3)        : 2,5,8,11
              3,7,11          : 3,7,11
              20..$(4)        : 20,24,28,32 (assuming 33 columns, say)

      -chrono                         : apply options chronologically

          By default, the general options are applied before the show
          options, with the show options being in order.

          When the -chrono option is applied, all options are chronological,
          allowing the options to be applied as in a script.

          For example, a matrix could be loaded, and then a series of fit
          betas could be displayed by alternating a sequence of -choose_cols
          and -show_fit_betas options.

          Consider example 6.

      -cormat_cutoff CUTOFF           : set min cutoff for cormat warnings

          e.g. -cormat_cutoff 0.5

          By default, any value in the correlation matrix that is greater
          than or equal to 0.4 generates a warning.  This option can be used
          to override that minumum cutoff.

      -cosmat_cutoff CUTOFF           : set min cutoff for cosmat warnings

          e.g. -cosmat_cutoff 0.5

          By default, any value in the cosine matrix that is greater than or
          equal to 0.3827 generates a warning.  This option can be used to
          override that minumum cutoff.

          Note a few cosine values, relative to 90 degrees (PI/2):

              cos(.50 *PI/2) = .707
              cos(.75 *PI/2) = .3827
              cos(.875*PI/2) = .195

      -cosmat_motion                  : include motion in cosmat warnings

          In the cosine matrix, motion regressors are often pointing in a
          direction close to that of either baseline or other motion
          regressors.  By default, such warnings are not displayed.

          Use this option to include all such warnings.

      -load_xmat XMAT.xmat.1D         : load the AFNI X-matrix

          e.g. -load_xmat X.xmat.1D

          Load the X-matrix, as the basis for most computations.

      -load_1D DATA.1D                : load the 1D time series

          e.g. -load_1D norm_ts.1D

          Load the 1D time series, for which fit betas and a fit time series
          can be generated.

      -no_gui                         : do not start the GUI

          By default, this program runs a graphical interface.  If the user
          wishes to perform some actions and terminate without starting the
          GUI, this option can be applied.

      -verb LEVEL                     : set the verbose level

          Specify how much extra text should be displayed regarding the
          internal operations.  Valid levels are currently 0..5, with 0
          meaning 'quiet', 1 being the default, and 5 being the most verbose.

 ------------------------------------------
 GUI (graphical user interface) options:

      -gui_plot_xmat_as_one           : plot Xmat columns on single axis

 ------------------------------------------
 show options:

      -show_col_types                 : display columns by regressor types

          Show which columns are considered 'main', 'chosen', 'baseline'
          and 'motion'.  This would correspond to condition numbers.

      -show_conds                     : display a list of condition numbers

          The condition number is the ratio of the largest eigen value to
          the smallest.  It can provide an indication of how correlated the
          regressors are.

          This option requests to display condition numbers for the X-matrix,
          restricted to the given sets of columns (regressors):

              - all regressors
              - chosen regressors (if there are any)
              - main regressors (non-baseline, non-motion)
              - main + baseline (non-motion)
              - main + motion   (non-baseline)

              - motion + baseline
              - baseline
              - motion

      -show_cormat                    : show the correlation matrix

          Display the entire correlation matrix as text.

          For an N-regressor (N columns) matrix, the NxN correlation matrix
          has as its i,j entry the Pearson correlation between regressors
          i and j.  It is computed as the de-meaned, normalized XtX.

          Values near +/-1.0 are highly correlated (go up and down together,
          or in reverse).  A value of 0.0 would mean they are orthogonal.

      -show_cosmat                    : show the cosine matrix

          Display the entire cosine matrix as text.

          This is similar to the correlation matrix, but the values show the
          cosines of the angles between pairs of regressor vectors.  Values
          near 1 mean the regressors are "pointed in the same direction" (in
          M-dimensional space).  A value of 0 means they are at right angles,
          which is to say orthogonal.
         
      -show_cormat_warnings           : show correlation matrix warnings

          Correlations for regressor pairs that are highly correlated
          (abs(r) >= 0.4, say) are displayed, unless it is for a motion
          regressor with either another motion regressor or a baseline
          regressor.

      -show_cosmat_warnings           : show cosine matrix warnings

          Cosines for regressor pairs that are pointed similar directions
          (abs(cos) >= 0.3827, say) are displayed.

      -show_fit_betas                 : show fit betas

          If a 1D time series is specified, beta weights will be displayed as
          best fit parameters of the model (X-matrix) to the data (1D time
          series).  These values are the scalars by which the corresponding
          regressors are multiplied, in order to fit the data as closely as
          possibly (minimizing the sum of squared errors).

          Only chosen columns are fit to the data.

              see -choose_cols

      -show_fit_ts                    : show fit time series

          Similar to showing beta weights, the actual fit time series can
          be displayed with this option.  The fit time series is the sum of
          each regressor multiplied by its corresponding beta weight.

          Only chosen columns are fit to the data.

              see -choose_cols

      -show_xmat                      : display general X-matrix information

          This will display some general information that is stored in the
          .xmat.1D file.

      -show_1D                        : display general 1D information

          This will display some general information from the 1D time series
          file.

-----------------------------------------------------------------------------
R Reynolds    October 2008
=============================================================================
"""

gui_help_string = """
=============================================================================
xmat_tool: GUI help     (displayed via -help_gui or from the GUI Help)

for option help, please see "xmat_tool.py -help"
=============================================================================
"""

g_history = """
   xmat_tool.py history:

   0.1  Oct 24, 2008: and then there was xmat_tool.py ...
   0.2  Oct 26, 2008:
        - renamed xmat_comp.py to ui_xmat.py
        - upon loading X-matrix, warn user of duplicate regressors
   0.3  Oct 27, 2008: test imports via module_test_lib
   0.4  Oct 30, 2008:
        - modified the correlation matrix so that a constant regressor is not
          zeroed out (de-meaned), so not quite a correlation matrix
   0.5  Nov 04 2008:
        - added many initial command-line options
        - added plot_xmat_as_one toggle button
        - compute cosine matrix and cosmat_warnings
   0.6  Nov 07 2008:
        - scipy is only tested for when necessary
        - compute norms locally if no scipy
        - solve_against_1D, linear_combo: return error string instead of code
        - added -chrono option, to make all options chronological
          (so options are essentially scriptable)
   0.7  Nov 18 2008:
        - added options -test, -show_col_types, -show_cosmat,
                        -show_fit_ts and -cormat_cutoff
        - wrote main help
"""

g_version = "xmat_tool version 0.7, November 18, 2008"


class XmatInterface:
   """interface class for X matrix"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None
      self.gui_opts        = []
      self.use_gui         = 0
      self.fname_mat       = 'no Xmat selected'      # X matrix
      self.fname_1D        = 'no 1D file selected'   # 1D regressor
      self.col_list        = []

      # resulting variables
      self.matX            = None
      self.mat1D           = None
      self.matfit          = None
      self.matbetas        = None

      # user options
      self.chrono          = 0          # are options processed chronologically
      self.verb            = verb
      self.cormat_cut      = -1         # if >= 0, apply for cormat warnings
      self.cosmat_cut      = -1         # if >= 0, apply for cosmat warnings
      self.cosmat_motion   = 0          # check_mot_base in cosmat warnings

      # initialize valid_opts
      self.init_options()

      # process options - if a failure occurs, block gui
      self.status = self.process_options()
      if self.status: self.use_gui = 0

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # short, terminal arguments
      self.valid_opts.add_opt('-help', 0, [],           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-help_gui', 0, [],       \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-test', 0, [],           \
                      helpstr='run a basic test with known files')
      self.valid_opts.add_opt('-ver', 0, [],            \
                      helpstr='display the current version number')

      # computational options
      self.valid_opts.add_opt('-load_xmat', 1, [], 
                      helpstr='load the given X-matrix')
      self.valid_opts.add_opt('-load_1D', 1, [], 
                      helpstr='load the given 1D time series')

      self.valid_opts.add_opt('-choose_cols', 1, [], 
                      helpstr='choose X-matrix columns')

      self.valid_opts.add_opt('-show_col_types', 0, [], 
                      helpstr='show the types of columns in the X-matrix')
      self.valid_opts.add_opt('-show_conds', 0, [], 
                      helpstr='show some condition numbers from the X-matrix')
      self.valid_opts.add_opt('-show_cormat', 0, [], 
                      helpstr='show the XtX correlation matrix')
      self.valid_opts.add_opt('-show_cosmat', 0, [], 
                      helpstr='show the XtX cosine matrix')
      self.valid_opts.add_opt('-show_cormat_warnings', 0, [], 
                      helpstr='show warnings for the XtX correlation matrix')
      self.valid_opts.add_opt('-show_cosmat_warnings', 0, [], 
                      helpstr='show warnings for the XtX cosine matrix')
      self.valid_opts.add_opt('-show_fit_betas', 0, [], 
                      helpstr='show fit betas of X-matrix to time series')
      self.valid_opts.add_opt('-show_fit_ts', 0, [], 
                      helpstr='show fit time series of X-matrix to time series')
      self.valid_opts.add_opt('-show_xmat', 0, [], 
                      helpstr='display info about the given X-matrix')
      self.valid_opts.add_opt('-show_1D', 0, [], 
                      helpstr='display info about the given 1D time series')

      # general options
      self.valid_opts.add_opt('-chrono', 0, [], 
                      helpstr='process options chronologically')
      self.valid_opts.add_opt('-cormat_cutoff', 1, [], 
                      helpstr='set the correlation matrix warning cutoff')
      self.valid_opts.add_opt('-cosmat_cutoff', 1, [], 
                      helpstr='set the cosine matrix warning cutoff')
      self.valid_opts.add_opt('-cosmat_motion', 0, [], 
                      helpstr='include motion vs base/mot in cosmat warnings')
      self.valid_opts.add_opt('-verb', 1, [], 
                      helpstr='set the verbose level (default is 1)')

      # GUI options
      self.valid_opts.add_opt('-no_gui', 0, [], 
                      helpstr='do not start the graphical user interface')

      self.valid_opts.add_opt('-gui_plot_xmat_as_one', 0, [], 
                      helpstr='plot Xmat columns on single axis')


      return 0

   def process_options(self):

      # process terminal options without the option_list interface

      if '-help' in sys.argv:
         print g_help_string
         return 0

      if '-help_gui' in sys.argv:
         print gui_help_string
         return 0

      if '-hist' in sys.argv:
         print g_history
         return 0

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 0

      if '-test' in sys.argv:
         self.test()
         return 0

      if '-ver' in sys.argv:
         print g_version
         return 0

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # check general options, esp. chrono

      if uopts.find_opt('-chrono'): self.chrono = 1

      # if options are not chronological, process general options now
      # (so -show options are still in order)
      if not self.chrono:

         # general options might affect selection options
         val, err = self.user_opts.get_type_opt(float, '-cormat_cutoff')
         if val != None and not err: self.cormat_cut = val

         val, err = self.user_opts.get_type_opt(float, '-cosmat_cutoff')
         if val != None and not err: self.cosmat_cut = val

         val, err = self.user_opts.get_type_opt(float, '-cosmat_motion')
         if val != None and not err: self.cosmat_motion = 1

         val, err = self.user_opts.get_type_opt(int, '-verb')
         if val != None and not err: self.verb = val

         # selection options
         val, err = uopts.get_string_opt('-load_xmat')
         if val and not err:
            if self.set_xmat(val): return 1

         val, err = uopts.get_string_opt('-load_1D')
         if val and not err:
            if self.set_1D(val): return 1

         val, err = uopts.get_string_opt('-choose_cols')
         if val and not err:
            rstr = self.set_cols_from_string(val)
            if rstr:
               print "** failed to apply '-choose_cols':\n%s" % rstr
               return 1

      # ------------------------------------------------------------
      # selection and process options:
      #    process sequentially, to make them like a script

      for opt in uopts.olist:
         # if all options are chronological, check load and general, too
         if self.chrono:
            # selection options
            if opt.name == '-load_xmat':
               if self.set_xmat(opt.parlist[0]):   return 1
            elif opt.name == '-load_1D':
               if self.set_1D(opt.parlist[0]):     return 1
            elif opt.name == '-choose_cols':
               rstr = self.set_cols_from_string(opt.parlist[0])
               if rstr:
                  print "** failed to apply '-choose_cols':\n%s" % rstr
                  return 1

            # general options
            elif opt.name == '-cormat_cutoff':
               val, err = self.user_opts.get_type_opt(float, '', opt=opt)
               if val != None and err: return 1
               else: self.cormat_cut = val

            elif opt.name == '-cosmat_cutoff':
               val, err = self.user_opts.get_type_opt(float, '', opt=opt)
               if val != None and err: return 1
               else: self.cosmat_cut = val

            elif opt.name == '-cosmat_motion':
               self.cosmat_motion = 1

            elif opt.name == '-verb':
               val, err = self.user_opts.get_type_opt(int, '', opt=opt)
               if val != None and err: return 1
               else: self.verb = val

         # 'show' options (allow these to fail?)
         if opt.name == '-show_col_types':
            if self.matX:
               err, str = self.make_col_type_string()
               print str
            else: print "** -show_col_types: needs -load_xmat"
         elif opt.name == '-show_conds':
            if self.matX: print self.matX.make_show_conds_str(self.col_list)
            else: print "** -show_conds: needs -load_xmat"
         elif opt.name == '-show_cormat':
            err, str = self.make_cormat_string()
            print '-- Correlation matrix for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_cormat_warnings':
            err, str = self.make_cormat_warnings_string()
            print '-- Correlation matrix warnings for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_cosmat':
            err, str = self.make_cosmat_string()
            print '-- Cosine matrix for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_cosmat_warnings':
            err, str = self.make_cosmat_warnings_string()
            print '-- Cosine matrix warnings for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_fit_betas':
            err, rstr = self.make_matrix_fit_betas_string()
            print rstr
         elif opt.name == '-show_fit_ts':
            err, rstr = self.make_matrix_fit_string()
            print rstr
         elif opt.name == '-show_xmat':
            if self.matX: self.matX.show()
            else: print "** -show_xmat: needs -load_xmat"
         elif opt.name == '-show_1D':
            if self.matX: self.mat1D.show()
            else: print "** -show_1D: needs -load_1D"

         # store gui options in a separate list
         elif opt.name[0:5] == '-gui_': self.gui_opts.append(opt)

      # ------------------------------------------------------------
      # process GUI options

      # ------------------------------------------------------------
      # last test before returning
      if uopts.find_opt('-no_gui'):     self.use_gui = 0
      else:                             self.use_gui = 1

      return 0

   def set_cols_from_string(self, cstr):
      """decode the string into a 1D column selection and apply it to col_list
         return an error message string, or None on success"""

      if not self.matX: return "please load an X matrix, first"

      ncols = self.matX.ncols
      clist = UTIL.decode_1D_ints(cstr, max=ncols-1)
      if not clist:
         return "invalid column list\n\n--> please use AFNI sub-brick notation"
      elif not AM.list2_is_in_list1(range(ncols), clist):
         return "column list outside xmat cols 0..%d" % (ncols-1)

      self.col_list = clist

      return None       # be explicit

   def fit_xmat_to_1D(self, cols=[]):
      """compute the best model fit of the matrix columns to the time series
         return error code (0 on success), error string"""
      if not self.matX or not self.mat1D:
         return 1, 'cannot fit without matrix and 1D data'
      if not self.matX.ready or not self.mat1D.ready:
         return 1, 'matrix and 1D data not ready for fit'

      if not cols: cols = self.col_list
      if not cols:
         return 1, "no cols to fit to (neither 'cols' or 'col_list')"

      if self.matfit:   del(self.matfit)
      if self.matbetas: del(self.matbetas)
      self.matfit = None

      emesg, self.matbetas = self.matX.solve_against_1D(self.mat1D, acols=cols)
      if emesg: return 1, emesg

      emesg, self.matfit = self.matX.linear_combo(self.matbetas, acols=cols)

      if emesg: return 1, emesg

      return 0, ''

   def make_matrix_fit_betas_string(self, clist=[]):
      """return a string describing best model fit to 1D time series
            clist : optional list of X-matrix columns to use for fit
                    (use self.col_list if empty)
         return error code (0=success) and message
      """

      # compute fit and return if it fails
      rv, mesg = self.fit_xmat_to_1D(clist)
      if rv: return 1, '** X-matrix fitting failed\n\n   (%s)\n' % mesg

      if not clist: clist = self.col_list       # need cols for labels

      # create return string
      rstr = 'Beta Weights of chosen columns fit to time series:\n\n'
      labs = self.matX.labels
      betas = self.matbetas.mat

      maxlab = 0
      if labs: maxlab = max([len(lab) for lab in labs])

      for ind in range(len(clist)):
         col = clist[ind]
         if labs: rstr += 'col % 3d:   %-*s   : %9.3f\n' %    \
                          (col,maxlab, labs[col],betas[ind][0])
         else   : rstr += 'col %03d: %s\n' % (ind, betas[ind][0])

      return 0, rstr

   def make_matrix_fit_string(self, clist=[]):
      """return a string of the time series representing the best model fit
         to the given 1D time series
            clist : optional list of X-matrix columns to use for fit
                    (use self.col_list if empty)
         return error code (0=success) and message
      """

      # compute fit and return if it fails
      rv, mesg = self.fit_xmat_to_1D(clist)
      if rv: return 1, '** X-matrix fitting failed\n\n   (%s)\n' % mesg

      if not clist: clist = self.col_list       # need cols for labels

      # create return string
      rstr = ''
      ts = self.matfit.mat

      for val in ts:
         rstr += '%f\n' % val

      return 0, rstr

   def set_xmat(self, fname):
      """read and store the X matrix from file 'fname'
         return 0 on success, 1 on error"""
      mat = AM.AfniXmat(fname, verb=self.verb)
      if not mat.ready:
         del(mat)
         return 1

      if self.verb > 1: print "++ read xmat from '%s'" % fname

      # delete any old copy
      if self.matX:
         if self.verb > 1:
            print "-- replacing X matrix '%s' with that in '%s'" % \
                      (self.fname_mat, fname)
         del(self.matX)
      self.matX = mat
      self.fname_mat = fname

      # init col_list to regressors of interest
      if mat.groups:
         self.col_list = mat.cols_by_group_list([g for g in mat.groups if g>0])
         #self.col_list = range(mat.ncols)
      else:
         self.col_list = range(mat.ncols)

      return 0

         
   def set_1D(self, fname):
      """read and store the timeseries from file 'fname'
         return 0 on success, 1 on error"""
      mat = AM.AfniXmat(fname, verb=self.verb)
      if not mat.ready:
         print "** failed to read timeseries from '%s'" % fname
         del(mat)
         return 1

      if self.verb > 1: print "++ read timeseries from '%s'" % fname

      # delete any old copy
      if self.mat1D:
         if self.verb > 1:
            print "-- replacing timeseries '%s' with that in '%s'" % \
                      (self.fname_1D, fname)
         del(self.mat1D)
      self.mat1D = mat
      self.fname_1D = fname
         
      return 0

   def make_cormat_string(self):
      """return a string of the correlation matrix
         (return error code (0=success) and cormat string)"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready:    # then set it
         mat.set_cormat()

      mstr = ''
      for r in range(mat.ncols):
         for c in range(mat.ncols):
             mstr += '%.3f  ' % mat.cormat[r,c]
         mstr += '\n'

      return 0, mstr

   def make_cormat_warnings_string(self):
      """make a string for any entires at or above cutoffs:
            cut0=1.0, cut1=(1.0+cormat_cut)/2.0, cut2=cormat_cut

            cut0, cut1, cut2 are cutoff levels (cut0=highest)
            that determine the severity of the correlation
            (anything below cut2 is ignored)

         return error code (0=success) and 'warnings' string"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready: mat.set_cormat() # create cormat
      if not mat.cormat_ready: # then failure
         return 1, '** cormat_warnings_string: failed to create cormat'

      # assign base cutoff
      if self.cormat_cut < 0.0: cutoff = 0.4
      else:                     cutoff = self.cormat_cut

      cut0 = 1.0
      cut1 = (1.0 + cutoff)/2.0
      cut2 = cutoff

      badlist = mat.list_cormat_warnings(cutoff=cut2)
      blen = len(badlist)

      if blen == 0:
         return 0, '-- no warnings for correlation matrix (cut = %.3f) --'%cut2

      mstr = 'Warnings regarding Correlation Matrix:\n\n'               \
        '(cosine is of the angle between the regresssors)\n\n\n'        \
        '  severity   correlation   cosine   regressor pair\n'          \
        '  --------   -----------   ------   ' + 40*'-' + '\n'
      cutstrs = [ '  IDENTICAL: ', '  high:      ', '  medium:    ' ]

      # note the maximum label length
      if mat.labels:
         mlab = max([len(mat.labels[col]) for val, cval, row, col in badlist])

      for val, cval, row, col in badlist:
         if   abs(val) >= cut0: cs = cutstrs[0]
         elif abs(val) >= cut1: cs = cutstrs[1]
         else:                  cs = cutstrs[2]

         # we have an appropriately evil entry...
         if mat.labels:
            mstr += '%s  %5.3f       %5.3f   (%2d vs. %2d)  %*s  vs.  %s\n' % \
                    (cs, val, abs(cval), col, row,
                     mlab, mat.labels[col], mat.labels[row])
         else:
            mstr += '%s  %5.3f       %5.3f   #%2d  vs.  #%2d\n' %        \
                    (cs, val, abs(cval), col, row)

      return 0, mstr

   def make_cosmat_string(self):
      """return a string of the cosine matrix
         (return error code (0=success) and cormat string)"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready:    # then set it
         mat.set_cormat()

      mstr = ''
      for r in range(mat.ncols):
         for c in range(mat.ncols):
             mstr += '%.3f  ' % mat.cosmat[r,c]
         mstr += '\n'

      return 0, mstr

   def make_cosmat_warnings_string(self):
      """make a string for any entires at or above self.cosmat_cut
         if self.cosmat_motion is set, check motion against mot/base

         note that cos(.50 *PI/2) = .707
               and cos(.75 *PI/2) = .3827
               and cos(.875*PI/2) = .195

         return error code (0=success) and 'warnings' string"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready: mat.set_cormat() # create cormat
      if not mat.cormat_ready: # then failure
         return 1, '** cosmat_warnings_string: failed to create cormat'

      # assign base cutoff
      if self.cosmat_cut < 0.0: cutoff = 0.3827
      else:                     cutoff = self.cosmat_cut

      badlist = mat.list_cosmat_warnings(cutoff=cutoff,
                                         check_mot_base=self.cosmat_motion)
      blen = len(badlist)

      if blen == 0:
         return 0, '-- no warnings for cosine matrix (cut = %.3f) --' % cutoff

      mstr = 'Warnings regarding Cosine Matrix (cut = %.3f):\n\n' % cutoff
      mstr +=                                                           \
        "(corr is the Pearson's Correlation value)\n\n\n"               \
        '  angle (deg)   cosine    corr    regressor pair\n'            \
        '  -----------   ------   ------   ' + 40*'-' + '\n'

      # note the maximum label length
      if mat.labels:
         mlab = max([len(mat.labels[col]) for val, cval, row, col in badlist])

      for val, cval, row, col in badlist:
         angle = round(180/math.pi*math.acos(abs(val)),1)

         # we have an appropriately evil entry...
         if mat.labels:
            mstr += '     %4.1f       %6.3f    %5.3f   (%2d vs. %2d)'   \
                    '  %*s  vs.  %s\n' % \
                    (angle, val, abs(cval), col, row,
                     mlab, mat.labels[col], mat.labels[row])
         else:
            mstr += '     %4.1f       %6.3f    %5.3f   #%2d  vs.  #%2d\n' % \
                    (angle, val, abs(cval), col, row)

      return 0, mstr

   def make_col_type_string(self):
      """return a string showing which columns are main, baseline or motion
         (return error code (0=success) and cormat string)"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to get col types from'

      mstr  = 'Columns by regressor type:\n\n'
      mstr += '    main regressor columns     : %s\n' % \
                   UTIL.encode_1D_ints(range(self.matX.ncols))
      mstr += '    chosen regressor columns   : %s\n' % \
                   UTIL.encode_1D_ints(self.col_list)
      mstr += '    baseline regressor columns : %s\n' % \
                   UTIL.encode_1D_ints(self.matX.cols_by_group_list([-1]))
      mstr += '    motion regressor columns   : %s\n' % \
                   UTIL.encode_1D_ints(self.matX.cols_by_group_list([0]))

      return 0, mstr

   def cleanup_memory(self):
      gc.collect()
      # sys.exc_clear()
      # sys.exc_traceback = sys.last_traceback = None

   def test(self, verb=3):
      # init
      print '------------------------ initial reads -----------------------'
      self.verb = verb
      # these should not fail, so quit if they do
      # first try AFNI_data4, then regression data
      if self.set_xmat('X.xmat.1D') and self.set_xmat('X.AD4.xmat.1D'):
         return None
      if self.set_1D('norm.022_043_012.1D') and   \
         self.set_1D('norm.AD4.022_043_012.1D'):
         return None

      # reset
      print '------------------------ reset files -----------------------'
      if self.set_xmat('X.xmat.1D') and self.set_xmat('X.AD4.xmat.1D'):
         return None
      if self.set_1D('norm.022_043_012.1D') and \
         self.set_1D('norm.AD4.022_043_012.1D'):
         return None

      # failures
      print '------------------------ should fail -----------------------'
      self.set_xmat('noxmat')
      self.set_1D('no1D')

      # more tests
      print '------------------------- xmatrix --------------------------'
      self.matX.show()
      print '------------------------- 1D file --------------------------'
      self.mat1D.show()

      print '------------------------ matrix fit ------------------------'
      rv, fstr = self.make_matrix_fit_string(range(self.matX.ncols))
      print fstr

      print '------------------------ fit betas -------------------------'
      rv, fstr = self.make_matrix_fit_betas_string(range(self.matX.ncols))
      print fstr

      print '-------------------------- cormat --------------------------'
      rv, fstr = self.make_cormat_string()
      print fstr
      print '-------------------------- cosmat --------------------------'
      rv, fstr = self.make_cosmat_string()
      print fstr

      print '--------------------- cormat warnings ----------------------'
      rv, fstr = self.make_cormat_warnings_string()
      print fstr
      print '---------------------- cosmat warnings ---------------------'
      self.cosmat_motion = 1
      err, str = self.make_cosmat_warnings_string()
      print '-- Cosine matrix warnings for %s :' % self.fname_mat
      print str

      print '-------------------------- conds ---------------------------'
      self.matX.show_conds()

      return None

if __name__ == '__main__':
   print '** this is not a main program'


