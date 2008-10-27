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

   def fit_xmat_to_1D(self, cols):
      """compute the best model fit of the matrix columns to the time series"""
      if not self.matX or not self.mat1D:
         print '** cannot fit without matrix and 1D data'
         return 1
      if not self.matX.ready or not self.mat1D.ready:
         print '** matrix and 1D data not ready for fit'
         return 1

      if not cols: cols = range(self.matX.ncols)

      if self.matfit:   del(self.matfit)
      if self.matbetas: del(self.matbetas)
      self.matfit = None

      try:
         err, self.matbetas = self.matX.solve_against_1D(self.mat1D, acols=cols)
      except:
         print '** fit_xmat: matfit solver failed!'
         return 1

      if err != 0:
         print '** fit_xmat: matfit solver failed!'
         return 1

      err, self.matfit = self.matX.linear_combo(self.matbetas, acols=cols)

      if err != 0:
         print '** fit_xmat: linear combo failed (err = %s)!' % err
         return 1

      return 0

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

      mat = self.matX

      if not mat.ready:
         return '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready:    # then set it
         mat.set_cormat()

      mstr = ''
      for r in range(mat.ncols):
         for c in range(mat.ncols):
             mstr += '%.3f  ' % mat.cormat[r,c]
         mstr += '\n'

      return mstr

   def make_cormat_problems_string(self, cut0=1.0, cut1=0.7, cut2=0.4):
      """make a string for any entires above cutoffs
            cut0, cut1, cut2 are cutoff levels (cut0=highest) that
            determine the severity of the correlation
            (anything below cut2 is ignored)

         return the 'problem' string"""

      mat = self.matX

      if not mat.ready:
         return '** no X-matrix to compute correlation matrix from'

      if not mat.cormat_ready: mat.set_cormat() # create cormat
      if not mat.cormat_ready: # then failure
         return '** cormat_problems_string: failed to create cormat'

      badlist = mat.list_cormat_problems(cutoff=cut2)
      blen = len(badlist)

      if blen == 0: return '-- no innapropriate values in correlation matrix--'

      mstr = 'evil values in correlation matrix:\n'

      cuts = [cut0, cut1, cut2]
      cutstrs = [ '\n    --- IDENTICAL   pairs  (epitome of evil) ---\n\n',
                  '\n    --- very evil   pairs ---\n\n',
                  '\n    --- mildly evil pairs ---\n\n' ]
      ind = 0
      for cind in range(len(cuts)):             # for each cutoff point...
         mstr += cutstrs[cind]
         cut = cuts[cind]
         while ind < blen:
            val, row, col = badlist[ind]        # get next entry
            if val < cut: break                 # to outer loop

            # otherwise, we have an appropriately evil entry...
            if self.matX.labels:
               lr = self.matX.labels[row]
               lc = self.matX.labels[col]
               mstr += '        %5.3f :  %s  vs.  %s\n' % (val, lr, lc)
            else:
               mstr += '        %5.3f :  #%d  vs.  #%d\n' % (val, row, col)

            ind += 1    # increment in badlist

      return mstr

   def cleanup_memory(self):
      gc.collect()
      # sys.exc_clear()
      # sys.exc_traceback = sys.last_traceback = None

def test():
   # init
   print '------------------------ initial reads -----------------------'
   xi = XmatInterface()
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
   print '------------------------ xmatrix -----------------------'
   xi.matX.show()
   print '------------------------ 1D file -----------------------'
   xi.mat1D.show()

   return None

if __name__ == '__main__':
   print '** this is not a main program'


