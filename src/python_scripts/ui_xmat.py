#!/usr/bin/env python

import sys
import gc, math
import option_list
import afni_xmat as AM
import afni_util as UTIL

# ----------------------------------------------------------------------
# globals

g_help_string = """
   ========================================================================
   xmat_tool     - a tool to evaluate an AFNI X-matrix

        help is on the way...

   R Reynolds    24 October 2008
   ========================================================================
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
"""

g_version = "xmat_tool version 0.5, November 6, 2008"


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
      self.show_gui_help   = 0
      self.verb            = verb
      self.cosmat_cut      = -1         # if >= 0, apply for cosmat warnings
      self.cosmat_motion   = 0          # check_mot_base in cosmat warnings

      # initialize valid_opts
      self.init_options()

      # process options - if a failure occurs, block gui
      self.status = self.process_options()
      if self.status: self.use_gui = 0

   def init_options(self):
      self.valid_opts = option_list.OptionList('valid opts')

      # short, terminal arguments
      self.valid_opts.add_opt('-help', 0, [],           \
                      helpstr='display program help')
      self.valid_opts.add_opt('-help_gui', 0, [],       \
                      helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                      helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                      helpstr='display all valid options')
      self.valid_opts.add_opt('-ver', 0, [],            \
                      helpstr='display the current version number')

      # computational options
      self.valid_opts.add_opt('-load_xmat', 1, [], 
                      helpstr='load the given X-matrix')
      self.valid_opts.add_opt('-load_1D', 1, [], 
                      helpstr='load the given 1D time series')

      self.valid_opts.add_opt('-choose_cols', 1, [], 
                      helpstr='choose X-matrix columns')

      self.valid_opts.add_opt('-show_1D_fit', 0, [], 
                      helpstr='show fit betas of X-matrix to time series')
      self.valid_opts.add_opt('-show_conds', 0, [], 
                      helpstr='show some condition numbers from the X-matrix')
      self.valid_opts.add_opt('-show_cormat', 0, [], 
                      helpstr='show the XtX correlation matrix')
      self.valid_opts.add_opt('-show_cormat_warnings', 0, [], 
                      helpstr='show warnings for the XtX correlation matrix')
      self.valid_opts.add_opt('-show_cosmat_warnings', 0, [], 
                      helpstr='show warnings for the XtX cosine matrix')
      self.valid_opts.add_opt('-show_xmat', 0, [], 
                      helpstr='display info about the given X-matrix')
      self.valid_opts.add_opt('-show_1D', 0, [], 
                      helpstr='display info about the given 1D time series')

      # general options
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
         self.show_gui_help = 1
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
      self.user_opts = option_list.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # selection and process options:
      #    process sequentially, to make them like a script

      for opt in uopts.olist:
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
         elif opt.name == '-cosmat_cutoff':
            val, err = self.user_opts.get_type_opt(float, '', opt=opt)
            if err: return 1
            else: self.cosmat_cut = val

         elif opt.name == '-cosmat_motion':
            self.cosmat_motion = 1

         elif opt.name == '-verb':
            val, err = self.user_opts.get_type_opt(int, '', opt=opt)
            if err: return 1
            else: self.verb = val

         # 'show' options (allow these to fail?)
         elif opt.name == '-show_1D_fit':
            err, rstr = self.make_matrix_fit_string()
            print rstr
         elif opt.name == '-show_conds':
            if self.matX: self.matX.show_conds()
            else: print "** -show_conds: needs -load_xmat"
         elif opt.name == '-show_cormat':
            err, str = self.make_cormat_string()
            print '-- Correlation matrix for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_cormat_warnings':
            err, str = self.make_cormat_warnings_string()
            print '-- Correlation matrix warnings for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_cosmat_warnings':
            if self.cosmat_cut < 0.0:
               err, str = self.make_cosmat_warnings_string(
                                                     motion=self.cosmat_motion)
            else:
               err, str = self.make_cosmat_warnings_string(
                             cutoff=self.cosmat_cut, motion=self.cosmat_motion)
            print '-- Cosine matrix warnings for %s :' % self.fname_mat
            print str
         elif opt.name == '-show_1D':
            if self.matX: self.mat1D.show()
            else: print "** -show_1D: needs -load_1D"
         elif opt.name == '-show_xmat':
            if self.matX: self.matX.show()
            else: print "** -show_xmat: needs -load_xmat"

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

      try:
         err, self.matbetas = self.matX.solve_against_1D(self.mat1D, acols=cols)
      except:
         return 1, 'matfit solver failed!'

      if err != 0:
         return 1, 'matfit solver failed!'

      err, self.matfit = self.matX.linear_combo(self.matbetas, acols=cols)

      if err != 0:
         return 1, 'fit_xmat: linear combo failed (err = %s)!' % err

      return 0, ''

   def make_matrix_fit_string(self, clist=[]):
      """return a string describing best model fit to 1D time series
            clist : optional list of X-matrix columns to use for fit
                    (use self.col_list if empty)
         return error code (0=success) and message
      """

      # compute fit and return if it fails
      rv, mesg = self.fit_xmat_to_1D(clist)
      if rv: return 1, '** X-matrix fitting failed\n\n(%s)' % mesg

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

   def set_xmat(self, fname):
      """read and store the X matrix from file 'fname'
         return 0 on success, 1 on error"""
      mat = AM.AfniXmat(fname, verb=self.verb)
      if not mat.ready:
         del(mat)
         return 1

      if self.verb > 0: print "++ read xmat from '%s'" % fname

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
      mat = AM.AfniXmat(fname)
      if not mat.ready:
         print "** failed to read timeseries from '%s'" % fname
         del(mat)
         return 1

      if self.verb > 0: print "++ read timeseries from '%s'" % fname

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

   def make_cormat_warnings_string(self, cut0=1.0, cut1=0.7, cut2=0.4):
      """make a string for any entires at or above cutoffs
            cut0, cut1, cut2 are cutoff levels (cut0=highest) that
            determine the severity of the correlation
            (anything below cut2 is ignored)

         return error code (0=success) and 'warnings' string"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready: mat.set_cormat() # create cormat
      if not mat.cormat_ready: # then failure
         return 1, '** cormat_warnings_string: failed to create cormat'

      badlist = mat.list_cormat_warnings(cutoff=cut2)
      blen = len(badlist)

      if blen == 0: return 0, '-- no warnings for correlation matrix --'

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

   def make_cosmat_warnings_string(self, cutoff=0.3827, motion=0):
      """make a string for any entires at or above the cutoff

         note that cos(.50 *PI/2) = .707
               and cos(.75 *PI/2) = .3827
               and cos(.875*PI/2) = .195

          cutoff        : cuttoff to apply to cosines
          motion        : whether to check motion against mot/base

         return error code (0=success) and 'warnings' string"""

      mat = self.matX

      if not mat.ready:
         return 1, '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready: mat.set_cormat() # create cormat
      if not mat.cormat_ready: # then failure
         return 1, '** cosmat_warnings_string: failed to create cormat'

      badlist = mat.list_cosmat_warnings(cutoff=cutoff, check_mot_base=motion)
      blen = len(badlist)

      if blen == 0: return 0, '-- no warnings for cosine matrix --'

      mstr = 'Warnings regarding Cosine Matrix:\n\n'                    \
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

   def cleanup_memory(self):
      gc.collect()
      # sys.exc_clear()
      # sys.exc_traceback = sys.last_traceback = None

def test(verb=3):
   # init
   print '------------------------ initial reads -----------------------'
   xi = XmatInterface()
   xi.verb = verb
   xi.set_xmat('X.xmat.1D')
   xi.set_1D('norm.022_043_012.1D')

   # reset
   print '------------------------ reset files -----------------------'
   xi.set_xmat('X.xmat.1D')
   xi.set_1D('norm.022_043_012.1D')

   # failures
   print '------------------------ should fail -----------------------'
   xi.set_xmat('noxmat')
   xi.set_1D('no1D')

   # more tests
   print '------------------------- xmatrix --------------------------'
   xi.matX.show()
   print '------------------------- 1D file --------------------------'
   xi.mat1D.show()

   print '------------------------ matrix fit ------------------------'
   rv, fstr = xi.make_matrix_fit_string()
   print fstr

   print '-------------------------- cormat --------------------------'
   rv, fstr = xi.make_cormat_string()
   print fstr

   print '--------------------- cormat warnings ----------------------'
   rv, fstr = xi.make_cormat_warnings_string()
   print fstr

   return None

if __name__ == '__main__':
   print '** this is not a main program'


