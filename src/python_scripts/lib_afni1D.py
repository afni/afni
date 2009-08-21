#!/usr/bin/env python

# currently, the explicitly does _not_ depend on scipy or numpy

import os, sys
import module_test_lib
g_testlibs = ['sys', 'math']
if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
   

# import libraries
import math
import copy

import afni_util as UTIL
import afni_base as BASE

class Afni1D:
   def __init__(self, filename="", from_mat=0, matrix=None, verb=1):
      """1D matrix class (2-D array)

         matrix is stored transposed from 1D file, each row as a time series

         init from filename or matrix,
            filename   : 1D file to read in as matrix
            from_mat   : use matrix for initialization ([])
            matrix     : 2-D matrix to use if from_mat is set
            name       : some text to associate with the datase
            verb       : verbose level of operations

         mat           : data
         fname         : filename
         nvec, nt      : dimensions
         labels        : simple text labels
         groups        : list: -1 (baseline), 0 (motion), else reg index
         tr            : in seconds
      """

      # main variables
      self.mat     = None       # actual data (2-D array [[]])
      self.name    = "NoName"   # more personal and touchy-feely...
      self.fname   = filename   # name of data file
      self.aname   = None       # afni_name, if parsing is useful

      # .xmat.1D variables
      self.nvec     = 0         # one vector per column in file
      self.nt       = 0         # length of each column (# rows)
      self.tr       = 1.0
      self.nrowfull = 0
      self.nruns    = 1
      self.run_len  = 0
      self.nroi     = 1

      # list variables
      self.labels   = None      # label per vector
      self.groups   = None      
      self.goodlist = None      # good time points

      self.verb     = verb
      self.ready    = 0         # matrix is ready

      # computed variables
      self.cormat      = None   # correlation mat (normed xtx)
      self.cormat_ready = 0     # correlation mat is set

      # initialize...
      if self.fname:
         if self.init_from_general_name(self.fname): return None
      elif from_mat:
         if self.init_from_matrix(matrix): return None

      self.update_group_info()

   def reduce_by_tlist(self, tlist):
      """reduce by time list, similiar to afni's {} selector
         this affects run_len and runs, so try to fix those

         return 0 on success"""

      if not self.ready:
         print '** append: Afni1D is not ready'
         return 1
      if not UTIL.is_valid_int_list(tlist, 0, self.nt-1, whine=1): return 1
      if len(tlist) < 1: return 1

      self.mat = [[row[t] for t in tlist] for row in self.mat]

      if self.nrowfull == self.nt: self.nrowfull = len(tlist)
      else:                        self.nrowfull = 0  # cannot tell
      self.nt         = len(tlist)

      if self.goodlist: self.goodlist = [self.goodlist[t] for t in tlist]

      self.update_group_info()  # since nruns or run_len may have changed

      return 0

   def reduce_by_vec_list(self, vlist):
      """reduce the dataset according to the vector list
         return 0 on success"""

      if not self.ready:
         print '** copy vec: Afni1D is not ready'
         return 1
      if not UTIL.is_valid_int_list(vlist, 0, self.nvec-1, whine=1): return 1
      if len(vlist) < 1: return 1

      self.mat = [self.mat[i] for i in vlist]

      self.nvec = len(vlist)
      self.nroi = self.nvec     # don't know, so assume all

      # update lists
      if self.labels: self.labels = [self.labels[i] for i in vlist]
      if self.groups: self.groups = [self.groups[i] for i in vlist]

      return 0

   def derivative(self):
      """change each value to its derivative (cur val - previous)
         new time[0] will always be 0

         process the data per run, so derivatives do not cross boundaries

         return 0 on success"""

      if self.verb > 3: print '-- Afni1D derivative...'

      if not self.ready:
         print '** append: Afni1D is not ready'
         return 1

      # verify nruns and run_len
      if self.nruns > 0 and self.run_len > 0:
         nruns = self.nruns
         rlen = self.run_len
         if nruns * rlen != self.nt:
            print '** derivative over runs, nruns*rlen != nt (%d, %d, %d)' \
                  % (nruns, rlen, self.nt)
            return 1
         if self.verb > 1:
            print '-- derivative: over %d runs of len %d' % (nruns, rlen)
      else:
         nruns = 1
         rlen = self.nt

      for ind in range(self.nvec):
         newvec = []
         for rind in range(nruns):
            vec = self.mat[ind][rind*rlen:(rind+1)*rlen]
            newvec += [0] + [vec[t+1]-vec[t] for t in range(rlen-1)]
         self.mat[ind] = newvec

      return 0

   def abs(self):
      """take the absolute value of each entry
         return 0 on success, 1 on error"""

      if self.verb > 3: print '-- Afni1D abs...'

      if not self.ready:
         print '** abs: Afni1D is not ready'
         return 1

      for ind in range(self.nvec):
         newvec = []
         for rind in range(self.nt):
            self.mat[ind][rind] = abs(self.mat[ind][rind])

      return 0

   def bool_negate(self):
      """set clear values and clear set values
         return 0 on success, 1 on error"""

      if self.verb > 3: print '-- bool_negate: old zeros = %d'  \
                              % self.mat[0].count(0)

      if not self.ready:
         print '** bool_negate: Afni1D is not ready'
         return 1

      for v in range(self.nvec):
         for t in range(self.nt):
            if self.mat[v][t]: self.mat[v][t] = 0
            else:              self.mat[v][t] = 1

      if self.verb > 3: print '-- negate: new zeros = %d' % self.mat[0].count(0)

      return 0

   def collapse_cols(self, method):
      """collapsed the matrix to a single array of length nt (nvec will = 1)

         collapsing will apply 'method' across the nvec per time point

             method = 'min'             : min across nvec
             method = 'minabs'          : min abs across nvec
             method = 'max'             : max across nvec
             method = 'maxabs'          : max abs across nvec
             method = 'euclidean_norm'  : sqrt(sum squares)

         Note: the result will still be a trivial 2-D array, where element 0
               is the collapsed time series.  This allows other functionality
               to still work.

         return 0 on success, 1 on error"""

      if self.verb > 3: print '-- collapse cols, method = %s' % method

      if not self.ready:
         print '** collapse: Afni1D is not ready'
         return 1

      # either create a new mat and apply later, or modify mat and recur

      if method == 'min':
         mat = [min([self.mat[v][t] for v in range(self.nvec)]) \
                                    for t in range(self.nt)]
      elif method == 'max':
         mat = [max([self.mat[v][t] for v in range(self.nvec)]) \
                                    for t in range(self.nt)]
      elif method == 'minabs':  # take abs and recur
         if self.abs(): return 1
         return self.collapse_cols('min')
      elif method == 'maxabs':  # take abs and recur
         if self.abs(): return 1
         return self.collapse_cols('max')
      elif method == 'euclidean_norm':
         mat = [UTIL.euclidean_norm([self.mat[v][t] for v in range(self.nvec)])\
                                                    for t in range(self.nt)]
      else:
         print "** collapse_cols: unknown method:", method
         return 1

      # note that there is only a single column left
      self.nvec = 1
      self.clear_cormat()

      del(self.mat)
      self.mat = [mat]
      return 0

   def extreme_mask(self, emin, emax, inclusive=1):
      """convert to a time series mask of 0/1 values where the result is
         1, if value is extreme (>emax or <emin, with = if inclusive)
         0, otherwise (moderate)

         For example, these would be the TRs to omit in a censor.1D file.
        
         return 0 on success"""

      if self.verb > 3:
         if inclusive: print '-- extreme_mask: excluding (%g,%g)'%(emin,emax)
         else:         print '-- extreme_mask: excluding [%g,%g]'%(emin,emax)

      if not self.ready:
         print '** extremem_mask: Afni1D is not ready'
         return 1

      if emin > emax:
         print '** extreme_mask: emin > emax (', emin, emax, ')'
         return 1

      self.mat = [UTIL.vec_extremes(vec,emin,emax,inclusive)[1] \
                        for vec in self.mat]

      if self.verb > 1:
         count = sum([val for val in self.mat[0] if val == 1])
         print '++ extreme_mask: removing %d of %d vals' % (count,self.nt)

      return 0

   def mask_prior_TRs(self):
      """if one TR is set, also set the prior one"""

      if self.verb > 3: print '-- masking prior TRs...'

      if not self.ready:
         print '** mask_prior_TRs: Afni1D is not ready'
         return 1

      for v in range(self.nvec):
         for t in range(self.nt-1):
            if self.mat[v][t+1] and not self.mat[v][t]: self.mat[v][t] = 1

      return 0

   def pad_into_many_runs(self, rindex, nruns):
      """pad over time so that this is run #rindex out of nruns runs
         (rindex should be 1-based)
     
         return 0 on success"""

      if self.verb > 3: print '-- pad_into_many_runs: rindex=%d, nruns=%d' \
                              % (rindex,nruns)

      if not self.ready:
         print '** pad into runs: Afni1D is not ready'
         return 1

      if nruns < 1:
         print '** pad into runs: bad nruns (%d)' % nruns
         return 1
      if rindex < 1 or rindex > nruns:
         print '** pad into runs: run index (%d) out of range [1,%d]' \
               % (rindex, nruns)
         return 1

      # first just stick in a 0 at time t=0
      for row in range(self.nvec):
         self.mat[row] = [0] + self.mat[row]
      self.nt += 1

      # now create a time list that sticks t0 in as padding
      rlen = self.nt - 1
      r0 = rindex-1   # 0-based
      tlist = [0 for i in range(r0*rlen)]         # pad first rindex runs
      tlist.extend([i+1 for i in range(rlen)])    # insert original run
      tlist.extend([0 for i in range((nruns-rindex)*rlen)]) # finish

      if self.reduce_by_tlist(tlist): return 1
      
      # update run info
      self.nruns   = nruns
      self.run_len = rlen
      self.nt      = rlen * nruns

      return 0

   def sort(self, reverse=0):
      """sort data over time axis (possibly reverse order)"""

      if self.verb > 3: print '-- Afni1D sorting...'

      for ind in range(self.nvec):
         self.mat[ind].sort(reverse=reverse)

      return 0

   def reverse(self):
      """reverse data over time axis"""

      if self.verb > 3: print '-- Afni1D reverse...'

      ilist = UTIL.decode_1D_ints('$..0(-1)',verb=self.verb,max=self.nt-1)
      if self.reduce_by_tlist(ilist): return 1

      return 0

   def show_censor_count(self, invert=0, column=0):
      """display the total number of TRs censored (set) in the given column

         If multi-column data, can choose one.

            invert   : invert column count
            column   : which column to count values from

         return status"""

      if self.verb > 3:
         print '++ showing censor count (inv=%d, col=%d)' % (invert, column)

      if not self.ready:
         print "** Afni1D not ready for write_timing to '%s'" % fname
         return 1

      total = self.mat[0].count(0)              # start with inverted count
      if not invert: total = self.nt - total

      print 'total number of censored TRs = %d' % total

      return 0

   def write(self, fname, sep=" ", overwrite=0):
      """write the data to a new .1D file

            fname       : filename is required
            sep         : separator between elements
            overwrite   : whether to allow overwrite

         return status"""

      if self.verb > 3: print '-- Afni1D write to %s, o=%d'%(fname,overwrite)

      if not self.ready:
         print "** Afni1D not ready for write to '%s'" % fname
         return 1
      if not fname:
         print "** missing filename for write"
         return 1
      if os.path.exists(fname) and not overwrite:
         print "** output file '%s' exists and 'overwrite' not set..." % fname
         return 1

      fp = open(fname, 'w')
      if not fp:
         print "** failed to open '%s' for writing" % fname
         return 1

      for row in range(self.nt):
         fp.write(sep.join(['%g' % self.mat[i][row] for i in range(self.nvec)]))
         fp.write('\n')

      fp.close()

      return 0

   def write_timing(self, fname, invert=0, column=0):
      """write set TRs to a timing file, one run per run, using '*' for
         empty runs (two for first run)

         If multi-column data, can choose one.

            fname    : required filename
            invert   : write times for zero data TRs
            column   : which column to write times as

         return status"""

      if self.verb > 3: print '-- Afni1D write_timing to %s, inv=%d, col=%d' \
                              %(fname, invert, column)

      if not self.ready:
         print "** Afni1D not ready for write_timing to '%s'" % fname
         return 1

      err, tstr = UTIL.make_timing_string(self.mat[column],
                                          self.nruns, self.tr, invert)
      if err: return 1

      try: fp = open(fname, 'r')
      except:
         print "** failed to open file '%s'" % fname
         return 1

      fp.write(tstr)
      fp.close()

      return 0

   def write_censortr(self, fname, invert=0, column=0):
      """write set TRs to a timing file, one run per run, using '*' for
         empty runs (two for first run)

         If multi-column data, can choose one.

            fname    : required filename
            invert   : write times for zero data TRs
            column   : which column to write times as

         return status"""

      if self.verb > 3: print '-- Afni1D write_censortr to %s, inv=%d, col=%d' \
                              %(fname, invert, column)

      if not self.ready:
         print "** Afni1D not ready for write_timing to '%s'" % fname
         return 1

      err,cstr = UTIL.make_CENSORTR_string(self.mat[column],self.nruns,asopt=1)

      if err: return 1

      try: fp = open(fname, 'w')
      except:
         print "** failed to open file '%s'" % fname
         return 1

      fp.write(cstr)
      fp.write('\n')    # add a newline
      fp.close()

   def append_vecs(self, matlist, newname=''):
      """append each Afni1D to the current one"""
      # test before trashing current matrix
      if not self.ready:
         print '** append: Afni1D is not ready'
         return 1

      if self.verb > 3: print '-- Afni1D append_vecs...'

      # allow matlist to be a simple mat
      if type(matlist) == type(self):
         newmats = [matlist]
      elif type(matlist) == type([]):
         if type(matlist[0]) == type(self): newmats = matlist
         else:
            print '** append_vecs: matlist elements not Afni1D'
            return 1
      else:
         print '** append_vecs: matlist must be list of Afni1D'
         return 1

      for mat in newmats:
         if not mat.ready:
            print '** append: Afni1D is not ready'
            return 1
         if mat.nt != self.nt:
            print '** append: nt differs (%d != !%d)' % (mat.nt, self.nt)
            return 2

      # update labels
      empty = 1
      if not self.labels: self.labels = ['' for ind in range(self.nvec)]
      else:               empty = 0
      for mat in newmats:
         if not mat.labels:
            self.labels.extend(['' for ind in range(mat.nvec)])
         else:
            self.labels.extend(mat.labels)
            empty = 0
      if empty:
         del(self.labels)
         self.labels = None

      # actual appending...
      for newdset in newmats:
         self.mat.extend(newdset.mat)
         self.nvec += newdset.nvec

      # update filename
      if newname:                           self.fname = newname
      elif self.fname.find('appended') < 0: self.fname += ' (appended)'

      # nuke things that no longer apply
      del(self.groups)
      self.groups = None
      del(self.goodlist)
      self.goodlist = None
      del(self.cormat)
      self.cormat = None
      self.cormat_ready = 0

      return 0

   def transpose(self):
      """transpose the matrix and swap nrows and ncols"""
      if self.verb > 3: print '-- Afni1D transpose...'
      if not self.ready:
         print "** matrix '%s' is not ready for transposing" % self.name
         return
      self.mat  = [[row[i] for row in self.mat] for i in range(self.nt)]
      newnt     = self.nvec
      self.nvec = self.nt
      self.nt   = newnt

   def simplecopy(self):
      return Afni1D(from_mat=1, matrix=self.mat, verb=self.verb)

   def show_labels(self):
      print '++ labels are:', self.labels

   def show_major_order_of_labels(self):
      """be picky and verify that labels look like sSLICE.NAME, where
         SLICE increments (slowly) over slice index"""

      if self.verb > 3: print '-- Afni1D show_major_order_of_labels...'

      if not self.labels:
         print '** no labels to test for ordering'
         return

      try:
         # nuke '.' and leading 's', remainder should be sorted ints
         vallist = [l.split('.')[0] for l in self.labels]
         vallist = [l.split('s')[1] for l in vallist]
         vallist = [int(i) for i in vallist]
         sorted = 1
         for ind in range(len(vallist)-1):
            if vallist[ind] > vallist[ind+1]:
               sorted = 0
               break
         if sorted: print '++ labels in row-major order: YES'
         else:      print '++ labels in row-major order: NO'
      except:
         print '++ labels are not of expected format, cannot determine ordering'
         self.show_labels()

   def clear_cormat(self):
      """nuke any existing cormat"""
      if self.verb > 3: print '-- Afni1D clear_cormat...'
      if not self.ready: return
      if self.cormat_ready:
         if not update: return
         del(self.cormat)
         self.cormat = None
         self.cormat_ready = 0

   def set_cormat(self, update=0):
      """set cormat (the correlation matrix) and cormat.ready

         Note that the (Pearson's) correlations are de-meaned, and so the
         constant baseline terms can be highly correlated.

         The cosine matrix might also be quite helpful.
      """
      if self.verb > 3: print '-- Afni1D set_cormat...'
      if not self.ready: return
      if self.cormat_ready:
         if not update: return
         # otherwise, nuke the old stuff
         del(self.cormat)
         self.cormat = None
         self.cormat_ready = 0

      if self.nvec < 2 or self.nt < 2: return

      # make copy to abuse
      try: cmat = copy.deepcopy(self)
      except:
         print '... deepcopy failure, using simplecopy()...'
         cmat = self.simplecopy()

      # demean each vector (for cormat), unless it is all 1's
      means = [UTIL.loc_sum(vec)/cmat.nt for vec in cmat.mat]
      for v in range(cmat.nvec):
         lmin = min(cmat.mat[v])
         lmax = max(cmat.mat[v])
         if lmin != 1.0 or lmax != 1.0:
            for ind in range(cmat.nt):
               cmat.mat[v][ind] -= means[v]

      # and normalize
      norms = [norm(row) for row in cmat.mat]
      for v in range(cmat.nvec):
         for ind in range(cmat.nt):
            if norms[v] == 0: cmat.mat[v][ind] = 0
            else:             cmat.mat[v][ind] /= norms[v]

      # finally, assign cormat
      self.cormat = [[dotprod(r1,r2) for r2 in cmat.mat] for r1 in cmat.mat]
      self.cormat_ready = 1

      # nuke temporary (normalized) matrices
      del(cmat)
      del(means)
      del(norms)

   def make_cormat_warnings_string(self, cutoff=0.4, name=''):
      """make a string for any entires at or above cutoffs:
            cut0=1.0, cut1=(1.0+cutoff)/2.0, cut2=cutoff

            cut0, cut1, cut2 are cutoff levels (cut0=highest)
            that determine the severity of the correlation
            (anything below cut2 is ignored)

         return error code (0=success) and 'warnings' string"""

      if self.verb > 3: print "-- make_cormat_warn_str for '%s', cut=%g" \
                              % (name, cutoff)

      # assign base cutoff
      if cutoff < 0.0 or cutoff >= 1.0 : cutoff = 0.4

      cut0 = 1.0
      cut1 = (1.0 + cutoff)/2.0
      cut2 = cutoff

      err, errstr, badlist = self.list_cormat_warnings(cutoff=cut2)
      if err: return err, errstr

      blen = len(badlist)

      if blen == 0:
         return 0, '-- no warnings for correlation matrix (cut = %.3f) --'%cut2

      mstr = '\nWarnings regarding Correlation Matrix: %s\n\n' % name

      # check for any constant 0 vectors here, too
      nfound = 0
      for ind in range(self.nvec):
         if UTIL.vals_are_constant(self.mat[ind], 0):
            mstr += '  ** vector #%02d is all ZEROs\n' % ind
            nfound += 1
      if nfound > 0: mstr += '\n'

      mstr = mstr +                                             \
        '  severity   correlation   regressor pair\n'           \
        '  --------   -----------   ' + 40*'-' + '\n'

      cutstrs = [ '  IDENTICAL: ', '  high:      ', '  medium:    ' ]

      # note the maximum label length
      if self.labels:
         mlab = max([len(self.labels[col]) for val, row, col in badlist])

      for val, row, col in badlist:
         if   abs(val) >= cut0: cs = cutstrs[0]
         elif abs(val) >= cut1: cs = cutstrs[1]
         else:                  cs = cutstrs[2]

         # we have an appropriately evil entry...
         if self.labels:
            mstr += '%s  %6.3f       (%2d vs. %2d)  %*s  vs.  %s\n' % \
                    (cs, val, col, row,
                     mlab, self.labels[col], self.labels[row])
         else:
            mstr += '%s  %6.3f       #%2d  vs.  #%2d\n' %        \
                    (cs, val, col, row)

      return 0, mstr

   def list_cormat_warnings(self, cutoff=0.4):
      """return an error code, error string and a list of corval, vec, index
         for each cormat value with abs() > cutoff"""

      if self.verb > 3: print '-- Afni1D list_cormat_warnings, cut=%g'%cutoff

      if not self.ready:
         return 1, '** no X-matrix to compute correlation matrix from', None

      if not self.cormat_ready: self.set_cormat() # create cormat
      if not self.cormat_ready: # then failure
         return 1, '** cormat_warnings: failed to create cormat', None

      cmat = self.cormat

      basecols = self.cols_by_group_list([-1])
      motcols  = self.cols_by_group_list([0])
      roicols  = self.cols_by_group_list([],allroi=1)

      if self.verb > 1:
         print '-- LCP: len(base, mot, roi) = (%d, %d, %d), cut = %.2f' % \
              (len(basecols), len(motcols), len(roicols), cutoff)

      # make a list of (abs(val),val,r,c) tuples in lower triangle
      clist = []
      for row in range(1,self.nvec):
         for col in range(row):
            clist.append((abs(cmat[row][col]), cmat[row][col], row, col))

      # clist.sort(reverse=True) # fails on old versions
      clist.sort() # smallest to largest, so process from end

      # now make a list of evil-doers
      badlist = []

      # process list as smallest to largest, since old sort had no reverse
      clen = len(clist)
      for ind in range(clen):
         aval, val, r, c = clist[clen-1-ind]

         if aval == 1.0:
            badlist.append((val, r, c)) # flag duplication
            continue

         # skip motion against either motion or baseline
         rbase = r in basecols
         cbase = c in basecols
         rmot  = r in motcols
         cmot  = c in motcols

         if cmot and (rbase or rmot): continue
         if cbase and rmot: continue

         if aval < cutoff: break

         badlist.append((val, r, c))       # so keep this one

      if self.verb > 1:
         print '-- LCP: badlist length = %d' % len(badlist)

      del(clist)

      return 0, '', badlist

   def labs_matching_str(self, mstr):
      if type(self.labels) != type([]): return []
      return [lab for lab in self.labels if lab.find(mstr) >= 0]

   def update_group_info(self):
      """if self.groups, try to set nruns and nroi"""

      if self.groups:
         if self.nvec: self.set_nruns()
         if len(self.groups) > 0:
            self.nroi = max(self.groups)
            if self.nroi < 0: self.nroi = 0

   def set_nruns(self, nruns=0):
      """if nruns is positive, try to apply it, else:
            find the first column in group 0, verify that it looks like
            one run of 1s (with remaining 0s), and use it to set the length
         return 0 on success
      """

      if self.verb > 3: print '-- Afni1D set_nruns (new nruns = %d)' % nruns

      # try to apply any passed nruns, first
      if nruns > 0:
         rlen = self.nt // nruns
         if rlen * nruns == self.nt:
             self.nruns = nruns
             self.run_len = rlen
             if self.verb > 1: print '++ successful update: nruns = %d' % nruns
         else:
             print '** nvalid nruns = %d (does not divide nt = %d)'  \
                   % (nruns, self.nt)
             return 1
         return 0

      # rcr - also, test against new RunStart comment line

      self.nruns = 1
      if not self.groups or not self.nvec: return 0

      try:
         base_ind = self.groups.index(-1)
         if base_ind < 0:
            if self.verb > 1: print '-- no baseline group to set runs with'
            return 1

         b0 = self.mat[base_ind]

         # skip 0s, count 1s, rest must be 0s
         for val in b0:
            if val != 0 or val != 1:
               if self.verb > 1:
                  print '-- baseline vals not just 0,1: %s' % val
            return 1

         # looks good, find run length and the number of runs
         first = b0.index(1)              # find first 1
         try:    next = b0.index(0,first+1)  # find next 0
         except: next = self.nt
         if next <= first:
            if self.verb > 1: print '-- odd run_len check...'
            return 1

         # we have a run length
         rlen = next - first
         nruns = self.nt//rlen          # integral division
         if rlen*nruns != self.nt:
            print '** nruns failure: rlen = %d, nruns = %d, len = %d' % \
                 (rlen, nruns, len(base0))
            return 1

         # success!

         self.run_len = rlen
         self.nruns   = nruns
         
      except:
         return 1

      return 0

   def show(self):
      print self.make_show_str()

   def show_rows_cols(self, mesg='', verb=1):
      """display the number of rows (nt) and columns (nvec)

         mesg: if set print before output
         verb: if 0, no text"""

      if mesg:     print '%s' % mesg,
      if verb > 0: print 'rows = %d, cols = %d' % (self.nt, self.nvec)
      else:        print '%d %d' % (self.nt, self.nvec)

   def make_show_str(self):
      if self.ready: rstr = 'ready'
      else:          rstr = 'not ready'

      mstr = "++ mat name : %s (%s)\n" \
             "++ fname    : %s\n" \
             "++ nvec     : %d\n" \
             "++ nt       : %d\n" \
             "++ labels   : %s\n" \
             "++ groups   : %s\n" \
             "++ goodlist : %s\n" \
             "++ tr       : %s\n" \
             "++ nrowfull : %d\n" \
             "++ nruns    : %d\n" \
             "++ run_len  : %d\n" % \
             (self.name, rstr, self.fname, self.nvec, self.nt,
             self.labels, self.groups, self.goodlist, self.tr, self.nrowfull,
             self.nruns, self.run_len)

      return mstr

   def cols_by_group_list(self, groups, allroi=0):
      """return a list of columns, given a list of groups
         - if 'allroi' is 1, append to 'groups' all that are positive
         - if 'allroi' is 1, then 'group' must not contain any positive
          group indices"""
      if not self.groups: return []

      # append positive groups, but fail if some already exist
      if allroi:
         for val in groups:
           if val > 0:
             print "** CBGL: cannot pass positive groups with 'allroi'"
             return []
         groups.extend([g for g in self.groups if g > 0])
      if len(groups) < 1 or len(self.groups) < 1: return []
      # if not list2_is_in_list1(self.groups, groups, "groups"): return []
      return [val for val in range(self.nvec) if self.groups[val] in groups]
      
   def cols_by_label_list(self, labels):
      """return a list of columns, given a list of labels"""
      if not self.labels or not labels: return []
      if not list2_is_in_list1(self.labels, labels, "labels"): return []
      return [val for val in range(self.nvec) if val in cols]

   def init_from_matrix(self, matrix):
      """initialize Afni1D from a 2D (or 1D) array"""
      if type(matrix) != type([]):
         print '** matrix for init must be [[float]] format'
         return 1
      elif type(matrix[0]) != type([]):
         if type(matrix[0]) == type(1.0):

            # init from vector
            self.mat   = [matrix]
            self.nvec  = 1
            self.nt    = len(matrix)
            self.ready = 1
            return 1

         else:
            print '** matrix for init must be [[float]] format'
            return 1

      self.mat   = matrix
      self.nvec  = len(matrix)
      self.nt    = len(matrix[0])
      self.ready = 1

      return 0

   def init_from_general_name(self, name):
      """might contain [] or {} selection"""
      aname = BASE.afni_name(self.fname)

      if self.init_from_1D(aname.rpve()): return 1 # failure

      self.aname = aname
      self.fname = aname.rpve()
      self.name  = aname.pve()

      # apply column and/or row selectors
      if aname.colsel:
         ilist = UTIL.decode_1D_ints(aname.colsel, verb=self.verb,
                                     max=self.nvec-1)
         if ilist == None: return 1
         if self.reduce_by_vec_list(ilist): return 1
      if aname.rowsel:
         ilist = UTIL.decode_1D_ints(aname.rowsel,verb=self.verb,max=self.nt-1)
         if ilist == None: return 1
         if self.reduce_by_tlist(ilist): return 1

      return 0

   def init_from_1D(self, fname):
      """initialize Afni1D from a 1D file (return err code)"""

      if self.verb > 3: print "-- Afni1D: init_from_1D '%s'" % fname

      mat, clines = read_1D_file(fname)
      if not mat: return 1
      self.mat   = mat
      self.nvec  = len(mat)
      self.nt    = len(mat[0])
      self.ready = 1

      for line in clines:
         label, data = c1D_line2labelNdata(line)
         if not label: continue

         verb_level = 3     # cutoff for verbose level

         try:      # to process some of the data
            if label == 'ni_type':
               ncols, type = data.split('*')
               ncols = int(ncols)
               if self.verb > verb_level:
                  print "-- label %s: cols = %d, type = %s" % \
                       (label, ncols, type)
               if ncols != self.nvec:
                  print "** matrix vecs %d != %s cols %d" % \
                       (self.nvec, label, ncols)
            elif label == 'ni_dimen':
               nrows = int(data)
               if self.verb > verb_level:
                  print "-- label %s: rows = %d" % (label, nrows)
               if nrows != self.nt:
                  print "** matrix nt %d != %s rows %d" % \
                       (self.nt, label, nrows)
            elif label == 'ColumnLabels':
               self.labels = [str.strip() for str in data.split(';')]
               if self.verb > verb_level:
                  print "-- label %s: labels = %s" % (label, self.labels)
               if self.nvec != len(self.labels):
                  print "** %d ColumnLabels but %d columns" %    \
                       (len(self.labels), self.nvec)
                  self.labels = None
            elif label == 'ColumnGroups':
               self.groups = UTIL.decode_1D_ints(data)
               if self.groups != None:
                  if len(self.groups) != self.nvec:
                     print "** ColumnGroups len %d != nvec %d" % \
                          (len(self.groups), self.nvec)
               if self.verb > verb_level:
                  print "-- label %s: groups %s" % (label,self.groups)
            elif label == 'RowTR':
               self.tr = float(data)
               if self.verb > verb_level:
                  print "-- label %s: TR %s" % (label,self.tr)
            elif label == 'GoodList':
               self.goodlist = UTIL.decode_1D_ints(data)
               if self.goodlist != None:
                  if len(self.goodlist) != self.nt:
                     print "** GoodList missing %d rows" % \
                          self.nt-len(self.goodlist)
               if self.verb > verb_level:
                  print "-- label %s: goodlist %s" % (label,self.goodlist)
            elif label == 'NRowFull':
               self.nrowfull = int(data)
               if self.verb > verb_level:
                  print "-- label %s: nrowfull %s" % (label,self.nrowfull)
            elif self.verb > 2:
               print "** unknown comment label '%s'" % label
         except:
            print "** failed to process comment label '%s'" % label

      return 0

def norm(vec):
   """return the euclidean norm"""

   if len(vec) < 1: return 0.0
   return math.sqrt(dotprod(vec,vec))

def dotprod(v1,v2):
   """compute the dot product of 2 vectors"""
   try: nsum = UTIL.loc_sum([v1[i]*v2[i] for i in range(len(v1))])
   except:
      print '** cannot take dotprod() of these elements'
      nsum = 0
   return nsum

def c1D_line2labelNdata(cline, verb=1):
   """expect cline to be of the form: '# LABEL = "DATA"'
   returning LABEL, DATA"""
   sline = cline.split()

   # check for problems
   if len(sline) < 4 or sline[0] != "#" or sline[2] != "=":
      if verb > 2: print "-- skipping useless comment line: '%s'" % cline
      return None, None

   label = sline[1]   # store the line label

   dline = ' '.join(sline[3:]) # put the line back together

   # again, check for problems
   if len(dline) < 2 or dline[0] != '"' or dline[-1] != '"':
      if verb > 1: print "** missing quotes in comment line: '%s'" % cline
      return None, None

   data = dline[1:-1]

   if verb > 2: print "++ line2labelNdata returns '%s', '%s'" % (label,data)

   return label, data

def read_1D_file(fname):
   """read 1D file, returning the data in a matrix, and comments in clines
      (matrix is transposed so the columns over time are stored as rows)"""

   try: fp = open(fname, 'r')
   except:
      print "** failed to open file '%s'" % fname
      return None, None

   fmat = []            # data lines
   clines = []          # comment lines

   lind = 0
   nts = 0              # number of time series (cols in file, rows in fmat)
   for line in fp.readlines():
      lind += 1
      lary = line.split()
      if len(lary) == 0: continue
      if lary[0] == '#':
         clines.append(line)
         continue

      # so this should be data

      # maybe initialize fmat
      if nts == 0:
         nts = len(lary)
         fmat = [[] for ind in range(nts)]

      if len(lary) != nts:
         print "** matrix is not square at line %d of %s" % (lind,fname)
         del(fmat)
         del(clines)
         return None, None

      try:
         for ind in range(nts): fmat[ind].append(float(lary[ind]))
      except:
         print "** failed to convert line '%s' to floats in %s" % (line,fname)
         del(fmat)
         del(clines)
         return None, None

   return fmat, clines

def list2_is_in_list1(list1, list2, label=''):
   """return 0 or 1, based on whether every element in list2 exists
      in list1"""

   if not list1: return 0
   if not list2: return 1

   for item in list2:
      if not item in list1:
         if label: print "-- list '%s' not subset in Afni1D" % label
         return 0

   return 1

if __name__ == '__main__':
   print '** this is not a main module'
   sys.exit(1)

