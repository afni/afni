#!/usr/bin/env python

import sys
import gc
import afni_xmat as AM

# ----------------------------------------------------------------------
# globals

class XmatInterface:
   """interface class for X matrix"""
   def __init__(self, verb=1):
      # main variables
      self.valid_opts      = None
      self.user_opts       = None
      self.fname_mat       = 'no Xmat selected'      # X matrix
      self.fname_1D        = 'no 1D file selected'   # 1D regressor
      self.col_list        = []

      # resulting variables
      self.matX            = None
      self.mat1D           = None
      self.matfit          = None
      self.matbetas        = None

      # other variables
      self.verb            = verb

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
         print "** failed to read xmat from '%s'" % fname
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
      if mat.ready:
         print "++ read timeseries from '%s'" % fname
      else:
         print "** failed to read timeseries from '%s'" % fname
         del(mat)
         return 1

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
      """make a string for any entires above cutoffs
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

      if blen == 0: return 0, '-- no warnings for correlation matrix--'

      mstr = 'Warnings regarding Correlation Matrix:\n\n'         \
             '    severity      correlation  regressor pair\n'    \
             '    --------      -----------  --------------------------------\n'
      cutstrs = [ '    IDENTICAL:   ', '    high:        ', '    medium:      ']

      for val, row, col in badlist:
         if   val >= cut0: cs = cutstrs[0]
         elif val >= cut1: cs = cutstrs[1]
         else:             cs = cutstrs[2]

         # we have an appropriately evil entry...
         if mat.labels:
            mstr += '%-8s    %5.3f     (%d vs. %d)  %s  vs.  %s\n' % \
                    (cs, val, col, row, mat.labels[col], mat.labels[row])
         else:
            mstr += '%-8s    %5.3f     #%d  vs.  #%d\n' % (cs, val, col, row)

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


