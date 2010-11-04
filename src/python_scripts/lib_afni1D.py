#!/usr/bin/env python

# currently, this explicitly does _not_ depend on scipy or numpy

import os, sys
import module_test_lib
g_testlibs = ['math', 'copy']
if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
   

# import libraries
import math
import copy

import afni_util as UTIL
import afni_base as BASE
import lib_textdata as TD

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
      self.nvec      = 0        # one vector per column in file
      self.nt        = 0        # length of each column (# rows)
      self.tr        = 1.0
      self.nrowfull  = 0
      self.nruns     = 1
      self.run_len   = [0]      # len(run_len) is number of runs
      self.nroi      = 1
      self.GLapplied = 0        # was goodlist applied (1=yes, 2=zero-padded)

      # list variables (from attributes)
      self.labels   = []        # label per vector       (from ColumnLabels)
      self.groups   = []        # integral column groups (from ColumnGroups)
      self.goodlist = []        # good time points       (from GoodList)
      self.runstart = []        # start indices          (from RunStart)

      self.verb     = verb
      self.ready    = 0         # matrix is ready

      # computed variables
      self.cormat      = None   # correlation mat (normed xtx)
      self.cosmat      = None   # cosine mat (scaled xtx)
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

   def run_info_is_consistent(self, whine=1):
      """verify consistency of nruns/run_len/nt"""
      if not self.ready:
         if whine: print '** RIIC: not ready (to test consistency)'
         return 0
      if len(self.run_len) == 0:
         if whine: print '** RIIC: ready be len(run_len) == 0!'
         return 0
      nt = sum(self.run_len)
      if nt != self.nt:
         if whine:
            print '** RIIC: nt=%d != sum of run_len: %s'%(self.nt,self.run_len)
         return 0
      return 1

   def demean(self):
      """demean each vector per run (mean of each run will be zero)

         return 0 on success"""

      if self.verb > 3: print '-- Afni1D demean...'

      if not self.ready:
         print '** demean: Afni1D is not ready'
         return 1

      # verify nruns and run_len
      if not self.run_info_is_consistent(whine=1):
         if self.verb > 1: print '** runs inconsistent for demean'
         return 1

      if self.verb > 1:
          print "-- demean: over %d runs of lengths %s" \
                % (self.nruns, self.run_len)

      # foreach run of each vector, demean
      for ind in range(self.nvec):
         offset = 0
         for t in self.run_len:
            UTIL.demean(self.mat[ind], offset, offset+t-1)
            offset += t

      return 0

   def derivative(self):
      """change each value to its derivative (cur val - previous)
         new time[0] will always be 0

         process the data per run, so derivatives do not cross boundaries

         return 0 on success"""

      if self.verb > 3: print '-- Afni1D derivative...'

      if not self.ready:
         print '** derivative: Afni1D is not ready'
         return 1

      # verify nruns and run_len
      if not self.run_info_is_consistent(whine=1):
         if self.verb > 1: print '** runs inconsistent for derivative'
         return 1

      if self.verb > 1:
          print "-- derivative: over %d runs of lengths %s" \
                % (self.nruns, self.run_len)

      # apply derivative to each vector as one run, then clear run breaks
      for ind in range(self.nvec):
         UTIL.derivative(self.mat[ind], in_place=1)
         offset = 0
         for t in self.run_len:
            self.mat[ind][offset] = 0
            offset += t

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

   def mask_first_TRs(self, nfirst, cval=0):
      """set the first nfirst TRs to cval"""

      if self.verb > 3:
         print '-- masking first %d TRs, cval = %d ...' % (nfirst, cval)

      if not self.ready:
         print '** mask_first_TRs: Afni1D is not ready'
         return 1

      # apply derivative to each vector as one run, then clear run breaks
      for ind in range(self.nvec):
         offset = 0
         for run, rlen in enumerate(self.run_len):
            if nfirst > rlen:
               print '** mask_first_TRs, nfirst %d > run len %d (of run %d)' \
                     % (nfirst, rlen, run+1)
               return 1
            # apply censor val for first nfirst TRs
            for tr in range(nfirst): self.mat[ind][offset+tr] = cval
            offset += rlen

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

   def pad_into_many_runs(self, rindex1, nruns, rlengths=[]):
      """pad over time so that this is run #rindex1 out of nruns runs
         (rindex1 should be 1-based)

         if rlengths is not set, assume all runs are of a set length
         if rlengths IS set, nruns is ignored
     
         Only one of nruns/rlengths is is applied.

         return 0 on success"""

      if self.verb > 3: print '-- pad_into_many_runs: rindex1=%d, nruns=%d' \
                              % (rindex1,nruns)

      if not self.ready:
         print '** pad into runs: Afni1D is not ready'
         return 1

      # ---- set NR (nruns) and rlens (list of lengths)

      # decide whether to use rlengths or nruns
      if len(rlengths) > 0:
         NR = len(rlengths)
         rlens = rlengths
      else:     # assume nruns runs of length self.nt
         if nruns < 1:
            print '** pad into runs: bad nruns (%d)' % nruns
            return 1
         NR = nruns
         rlens = [self.nt for r in range(NR)]

      # ---- check for consistency

      # verify rindex1 (using 1-based run index)
      if rindex1 < 1 or rindex1 > NR:
         print '** pad into runs: run index (%d) out of range [1,%d]' \
               % (rindex1, NR)
         return 1

      # apply rind as 0-based run index
      rind = rindex1-1

      # if rlengths, verify match for run #rindex1
      if self.nt != rlens[rind]:
         print "** cannot pad into many runs, nt (%d) != rlens[%d] (%d)" \
               % (self.nt, rind, rlens[rind])
         return 1

      # ---- and do the work

      # first just stick in a 0 at time t=0
      for row in range(self.nvec):
         self.mat[row] = [0] + self.mat[row]
      self.nt += 1

      # now create a time list that sticks t0 in as padding
      # (fill runs before rind, then rind, then after)
      tlist = []
      for run in range(rind):
         tlist.extend([0 for t in range(rlens[run])])
      tlist.extend([t+1 for t in range(rlens[rind])]) # skipping pad at t=0
      for run in range(NR-1-rind):
         tlist.extend([0 for t in range(rlens[run+rind+1])])

      # and apply tlist
      if self.reduce_by_tlist(tlist): return 1
      
      # update run info
      self.nruns   = NR
      self.run_len = rlens
      self.nt      = sum(rlens)

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

   def write_as_timing(self, fname, invert=0, column=0):
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
         err = 1

      if err: return 1

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
         print "** Afni1D not ready for write_censortr to '%s'" % fname
         return 1

      if not self.run_info_is_consistent(whine=1): return 1

      err,cstr = UTIL.make_CENSORTR_string(self.mat[column],
                               rlens=self.run_len, asopt=1, verb=self.verb)

      if err: return 1

      try: fp = open(fname, 'w')
      except:
         print "** failed to open file '%s'" % fname
         err = 1

      if err: return 1

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
         self.labels = []

      # actual appending...
      for newdset in newmats:
         self.mat.extend(newdset.mat)
         self.nvec += newdset.nvec

      # update filename
      if newname:                           self.fname = newname
      elif self.fname.find('appended') < 0: self.fname += ' (appended)'

      # nuke things that no longer apply
      del(self.groups)
      self.groups = []
      del(self.goodlist)
      self.goodlist = []
      del(self.cormat)
      del(self.cosmat)
      self.cormat = None
      self.cosmat = None
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
         del(self.cormat)
         del(self.cosmat)
         self.cormat = None
         self.cosmat = None
         self.cormat_ready = 0

   def set_cormat(self, update=0):
      """set cormat (the correlation matrix) and cormat.ready
         set cosmat (cosine matrix) as a follower

         Note that the (Pearson's) correlations are de-meaned, and so the
         constant baseline terms can be highly correlated.

         The cosine matrix might also be quite helpful.
      """
      if self.verb > 3: print '-- Afni1D set_cormat...'
      if not self.ready: return
      if self.cormat_ready:
         if not update: return
         self.clear_cormat() # otherwise, nuke the old stuff and re-generate

      if self.nvec < 2 or self.nt < 2: return

      # make copy to abuse
      try: cmat = copy.deepcopy(self)
      except:
         print '... deepcopy failure, using simplecopy()...'
         cmat = self.simplecopy()

      # demean each vector (for cormat), unless it is constant
      means = [UTIL.loc_sum(vec)/cmat.nt for vec in cmat.mat]
      for v in range(cmat.nvec):
         lmin = min(cmat.mat[v])
         lmax = max(cmat.mat[v])
         if lmin != lmax:
            for ind in range(cmat.nt):
               cmat.mat[v][ind] -= means[v]

      # and normalize
      norms = [UTIL.euclidean_norm(row) for row in cmat.mat]
      for v in range(cmat.nvec):
         for ind in range(cmat.nt):
            if norms[v] == 0.0: cmat.mat[v][ind] = 0.0
            else:               cmat.mat[v][ind] /= norms[v]

      # finally, assign cormat
      self.cormat =[[UTIL.dotprod(r1,r2) for r2 in cmat.mat] for r1 in cmat.mat]
      self.cormat_ready = 1

      # and generate cosmat (dot product and scale)
      cnorm = [ UTIL.euclidean_norm(v) for v in self.mat ]
      cosmat = [[UTIL.dotprod(r1,r2) for r2 in self.mat] for r1 in self.mat]
      for v1 in range(len(cosmat)):
         for v2 in range(len(cosmat)):
             prod = cnorm[v1]*cnorm[v2]
             if prod != 0.0: cosmat[v1][v2] /= prod
      self.cosmat = cosmat

      # nuke temporary (normalized) matrices
      del(cmat)
      del(means)
      del(norms)
      del(cnorm)

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

      # badlist holds cov, cos, row, col
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
        '  severity   correlation   cosine  regressor pair\n'           \
        '  --------   -----------   ------  ' + 40*'-' + '\n'

      cutstrs = [ '  IDENTICAL: ', '  high:      ', '  medium:    ' ]

      # note the maximum label length
      if self.labels:
         mlab = max([len(self.labels[col]) for val, s, row, col in badlist])

      for val, s, row, col in badlist:
         if   abs(val) >= cut0: cs = cutstrs[0]
         elif abs(val) >= cut1: cs = cutstrs[1]
         else:                  cs = cutstrs[2]

         # we have an appropriately evil entry...
         if self.labels:
            mstr += '%s  %6.3f      %6.3f  (%2d vs. %2d)  %*s  vs.  %s\n' % \
                    (cs, val, s, col, row,
                     mlab, self.labels[col], self.labels[row])
         else:
            mstr += '%s  %6.3f      %6.3f  #%2d  vs.  #%2d\n' %        \
                    (cs, val, s, col, row)

      return 0, mstr

   def list_cormat_warnings(self, cutoff=0.4):
      """return an error code, error string and a list of corval, cosval,
         vec, index for each cormat value with abs() > cutoff"""

      if self.verb > 3: print '-- Afni1D list_cormat_warnings, cut=%g'%cutoff

      if not self.ready:
         return 1, '** no X-matrix to compute correlation matrix from', None

      if not self.cormat_ready: self.set_cormat() # create cormat
      if not self.cormat_ready: # then failure
         return 1, '** cormat_warnings: failed to create cormat', None

      cmat = self.cormat
      smat = self.cosmat

      basecols = self.cols_by_group_list([-1])
      motcols  = self.cols_by_group_list([0])
      roicols  = self.cols_by_group_list([],allroi=1)

      if self.verb > 1:
         print '-- LCP: len(base, mot, roi) = (%d, %d, %d), cut = %.2f' % \
              (len(basecols), len(motcols), len(roicols), cutoff)

      # make a list of (abs(val),val,cosine,r,c) tuples in lower triangle
      clist = []
      for r in range(1,self.nvec):
         for c in range(r):
            clist.append((abs(cmat[r][c]), cmat[r][c], smat[r][c], r, c))

      # clist.sort(reverse=True) # fails on old versions
      clist.sort() # smallest to largest, so process from end

      # now make a list of evil-doers
      badlist = []

      # process list as smallest to largest, since old sort had no reverse
      clen = len(clist)
      for ind in range(clen):
         aval, val, s, r, c = clist[clen-1-ind]

         if aval == 1.0:
            badlist.append((val, s, r, c)) # flag duplication
            continue

         # skip motion against either motion or baseline
         rbase = r in basecols
         cbase = c in basecols
         rmot  = r in motcols
         cmot  = c in motcols

         if cmot and (rbase or rmot): continue
         if cbase and rmot: continue

         if aval < cutoff: break

         badlist.append((val, s, r, c))       # so keep this one

      if self.verb > 1:
         print '-- LCP: badlist length = %d' % len(badlist)

      del(clist)

      return 0, '', badlist

   def labs_matching_str(self, mstr):
      if type(self.labels) != type([]): return []
      return [lab for lab in self.labels if lab.find(mstr) >= 0]

   def update_group_info(self):
      """if self.groups, try to set nruns and nroi"""

      self.run_len = [self.nt]  # init to 1 run

      if self.groups:
         if self.nvec: self.set_nruns()
         if len(self.groups) > 0:
            self.nroi = max(self.groups)
            if self.nroi < 0: self.nroi = 0

   def set_nruns(self, nruns=0, run_lens=[]):
      """if nruns is positive, apply
         else if len(run_lens) > 0, apply
         else if have RunStart list, apply
         else, try to apply from a single run:
            find the first column in group 0, verify that it looks like
            one run of 1s (with remaining 0s), and use it to set the length

         return 0 on success, 1 on error
      """

      if self.verb > 3:
         print '-- LAD:set_nruns (nruns = %d, run_lens = %s)' % (nruns,run_lens)
         print '       len(goodlist) = %d, runstart = %s' \
               % (len(self.goodlist), self.runstart)

      # try to apply any passed nruns, first
      if nruns > 0:
         rlen = self.nt // nruns
         if rlen * nruns == self.nt:
             self.nruns = nruns
             self.run_len = [rlen for run in range(nruns)]
             if self.verb > 1: print '++ set_nruns: nruns = %d' % nruns
         else:
             print '** nvalid nruns = %d (does not divide nt = %d)'  \
                   % (nruns, self.nt)
             return 1
         return 0

      # next, try run_lens (if not set, try runstart data from RunStart label)
      if type(run_lens) != type([]):
         print "** set_runs: run_lens is not a list type: %s" % run_lens
         return 1

      # if no run_lens but have self.runstart, convert and apply same logic
      if len(run_lens) == 0 and len(self.runstart) > 0:
         # first try to set run lengths taking goodlist into account
         if not self.apply_goodlist(): return 0   # all is well
         else:
            rlens = self.runstart[:]
            for ind in range(len(rlens)-1):
                rlens[ind] = rlens[ind+1] - rlens[ind]
            rlens[-1] = self.nt - rlens[-1]
            if self.verb > 1:
               print '++ setting run_len based on RunStart list:' , rlens

      rlens = run_lens

      if len(rlens) > 0:
         if not UTIL.vals_are_positive(rlens):
            print "** set_runs: non-positive run length in list: %s" % rlens
            return 1
         if sum(rlens) != self.nt:
            print "** set_runs: sum of run lengths != nt (%d): %s" \
                  % (self.nt, rlens)
            return 1
         self.run_len = rlens[:]     # make a copy, to be safe
         self.nruns = len(rlens)
         if self.verb > 1:
            print '++ set_nruns: nruns = %d, run_len = %s' % (nruns, rlens)
         return 0

      # otherwise, init to 1 run and look for other labels
      self.nruns = 1
      if not self.groups or not self.nvec: return 0

      # rcr : update poor use of baselines?
      errs = 0
      try:
         base_ind = self.groups.index(-1)
         if base_ind < 0:
            if self.verb > 1: print '-- no baseline group to set runs with'
            return 1

         b0 = self.mat[base_ind]

         # skip 0s, count 1s, rest must be 0s
         for val in b0:
            if val != 0 and val != 1:
               if self.verb > 1: print '-- baseline vals not just 0,1: %s' % val
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
         if rlen > 0: nruns = self.nt//rlen          # integral division
         else:
            if self.verb > 1:
               print '** failed to set rlen from baseline (%d,%d)'%(first,next)
            return 1
         if rlen*nruns != self.nt:
            if self.verb>1:
               print '** nruns failure: rlen %d, nruns %d, len %d' % \
                     (rlen, nruns, len(b0))
               if self.nrowfull > self.nt:
                  print '++ nrowfull (%d) > nt (%d) ==> censored TRs' \
                        % (self.nrowfull, self.nt)
            return 1

         # success!

         self.run_len = [rlen for i in range(nruns)]
         self.nruns   = nruns
         
      except:
         if self.verb > 1: print '** unknown exception in LD:set_nruns'
         errs = 1

      return errs

   def apply_goodlist(self, parent=None, padbad=0):
      """try to apply goodlist, runstart, nt and nrowfull
            -> set nruns and run_lens[]

         if padbad is set, pad the data with zero over all TRs NOT
            in goodlist (so run_lens should come from RunStart)

         if parent is set (as an Afni1D instance),
            use it for goodlist, runstart, nrowfull

         return 0 on success"""

      if isinstance(parent, Afni1D):
         if self.verb > 2: print '-- apply_goodlist: run info from parent'
         runstart = parent.runstart
         goodlist = parent.goodlist
         nrowfull = parent.nrowfull
      else:
         if self.verb > 2: print '-- apply_goodlist: run info from self'
         runstart = self.runstart
         goodlist = self.goodlist
         nrowfull = self.nrowfull

      # first see if we have the necessary fields
      rv = 1
      try:
         if len(runstart) > 0 and len(goodlist) > 0 and \
               self.nt > 0 and nrowfull > 0: rv = 0
      except:
         if self.verb > 1: print '** bad internals for apply_goodlist'

      if rv == 1: return 1     # bail

      # other tests
      if not UTIL.vals_are_sorted(goodlist):
         if self.verb > 1: print '** LAD: goodlist not sorted'
         return 1
      if len(goodlist) != self.nt:
         if self.verb > 1: print '** LAD: goodlist length (%d) != nt (%d)' \
                                 % (len(goodlist), self.nt)
         return 1
      if not UTIL.vals_are_sorted(runstart):
         if self.verb > 1: print '** LAD: runstart not sorted'
         return 1
      if goodlist[-1] >= nrowfull:
         if self.verb > 1: print '** LAD: max goodlist value exceeds rowfull'
         return 1
      if goodlist[0] < 0:
         if self.verb > 1: print '** LAD: goodlist has negative value'
         return 1
      if runstart[-1] >= nrowfull:
         if self.verb > 1: print '** LAD: max runstart value exceeds rowfull'
         return 1
      if runstart[0] < 0:
         if self.verb > 1: print '** LAD: runstart has negative value'
         return 1

      # ---- now try to sort things out ----

      # if padding 'bad' TRs run_len is basic, but mat needs to be zero-filled
      if padbad:
         if self.GLapplied == 2:
            if self.verb > 0: print '** padbad: run already padded!'
            return 1

         # first note the run lengths
         rlens = runstart[:]
         for ind in range(len(rlens)-1):
             rlens[ind] = rlens[ind+1] - rlens[ind]
         rlens[-1] = nrowfull - rlens[-1]

         # then pad the data (create zero vectors, and copy good data)
         for vind in range(len(self.mat)):
            oldvec = self.mat[vind]
            newvec = [0 for t in range(nrowfull)]
            for gind in range(len(goodlist)):
               newvec[goodlist[gind]] = oldvec[gind]
            self.mat[vind] = newvec
            del(oldvec)

         self.nt        = nrowfull      # now the lists are full
         self.nruns     = len(runstart) # 
         self.run_len   = rlens         # steal reference
         self.GLapplied = 2             # goodlist was applied

         return 0

      # --- not zero padding, so adjust run lengths ---

      # set rnext list to be index *after* the current run
      rnext = runstart[1:]
      rnext.append(nrowfull)
      if self.verb > 3: print '-- set rnext list to %s' % rnext
      nruns = len(runstart)

      # accumulate run counts over goodlist
      run = 0
      rcount = [0 for r in runstart]  # count entries per run
      for gval in goodlist:
         # maybe adjust the run index (note that rnext[-1] > goodlist vals)
         while gval > rnext[run]: run += 1
         rcount[run] += 1

      # and verify that we have accounted for everything
      gtot = UTIL.loc_sum(rcount)
      glen = len(goodlist)
      if gtot != glen:
         print '** apply_goodlist: gtot error: %d != %d' % (gtot, glen)
         return 1
      if gtot != self.nt:
         print '** apply_goodlist: nt (%d) != goodtot (%d)' % (self.nt, gtot)
         return 1

      if self.verb > 1:
         print '++ from apply_goodlist: run_len[%d] = %s' % (nruns, rcount)

      self.nruns = nruns
      self.run_len = rcount     # steal reference
      self.GLapplied = 1        # goodlist was applied

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

   def get_indices_str(self, ind_types):
      """return an index list (sub-brick selector form) for the
         following groups:

            1: baseline (group  -1)
            2: motion   (group   0)
            4: interest (group > 0)

         Do not return an empty list.  If the groups do not exist or are
         not found, return '0..$'."""

      default = '0..$'

      if self.verb > 1: print '-- show indices, types = %d, groups = %s' \
                              % (ind_types, self.groups)

      bmask = ind_types & 7
      if not self.ready:           return default
      if bmask == 0 or bmask == 7: return default
      if len(self.groups) < 1:     return default

      ilist = []
      allind = range(len(self.groups))
      if ind_types & 1:
         ilist += [ind for ind in allind if self.groups[ind] == -1]
      if ind_types & 2:
         ilist += [ind for ind in allind if self.groups[ind] == 0]
      if ind_types & 4:
         ilist += [ind for ind in allind if self.groups[ind] > 0]
      ilist.sort()

      elist = UTIL.encode_1D_ints(ilist)
      if elist == '':
         if self.verb > 1: print '-- replacing empty encode list with full'
         elist = default
      return elist

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
             "++ run_len  : %s\n" % \
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

      tmat, clines = TD.read_data_file(fname, verb=self.verb)
      if not TD.data_is_rect(tmat):
         print "** data is not rectangular in %s" % fname
         return 1
      if not tmat: return 1

      # treat columns as across time
      mat = UTIL.transpose(tmat)
      del(tmat)
      
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
               nrows = int(data.split(',')[0])     # 3D datasets have commas
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
                  self.labels = []
            elif label == 'ColumnGroups':
               self.groups = UTIL.decode_1D_ints(data)
               if self.groups:
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
               if self.goodlist:
                  if len(self.goodlist) != self.nt:
                     print "** GoodList missing %d rows" % \
                          self.nt-len(self.goodlist)
               if self.verb > verb_level:
                  print "-- label %s: goodlist %s" % (label,self.goodlist)
            elif label == 'NRowFull':
               self.nrowfull = int(data)
               if self.verb > verb_level:
                  print "-- label %s: nrowfull %s" % (label,self.nrowfull)
            elif label == 'RunStart':
               self.runstart = UTIL.decode_1D_ints(data)
               if not UTIL.vals_are_sorted(self.runstart):
                  print "** RunStart values not sorted?  data = %s" % data
                  self.runstart = []
               if self.verb > verb_level:
                  print "-- label %s: runstart %s" % (label,self.runstart)
            elif self.verb > 2:
               print "** unknown comment label '%s'" % label
         except:
            print "** failed to process comment label '%s'" % label

      return 0

# end Afni1D
# ===========================================================================

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

# end Afni1D helper functions
# ===========================================================================


# ===========================================================================
# begin AfniData - generic numeric sparse 2D float class

g_AfniData_hist = """
   30 Jul, 2010 : added AfniData class
   17 Aug, 2010 : get data via lib_textdata.read_data_file()
                  - this includes modulation and duration data
"""

# error constants for file tests
ERR_ANY_MISC      =  1       # apply to errors that are not accumulated
ERR_ST_NEGATIVES  =  2       # some times are negatives
ERR_ST_NON_UNIQUE =  4       # times are not unique
ERR_ST_NUM_RUNS   =  8       # number of runs mismatch
ERR_ST_TOO_BIG    = 16       # val >= run length


class AfniData:
   def __init__(self, filename="", verb=1):
      """akin to a 2D float class, but do not require a square matrix

         init from filename
            filename   : 1D/timing file to read
            verb       : verbose level of operations
      """

      # main variables
      self.fname   = filename   # name of data file
      self.name    = "NoName"   # more personal and touchy-feely...
      self.data    = None       # actual data (array of arrays [[]])
      self.mdata   = None       # married data (elements are [time [mods] dur])
      self.clines  = None       # comment lines from file

      # descriptive variables, set from data
      self.nrows     = 0
      self.ncols     = 0        # non-zero if rectangular
      self.row_lens  = []       # row lengths, if not rectangular

      self.rect      = 0        # rectangular?
      self.square    = 0        # square?
      self.binary    = 0        # 0/1 file?
      self.empty     = 0        # no data at all
      self.married   = 0        # data has modulators or durations

      self.ready     = 0        # data is ready

      # passed in variables
      self.tr        = 0        # non-zero, if it applies
      self.nruns     = 0        # non-zero, if known
      self.run_lens  = []       # run lengths, in seconds or TRs
      self.verb      = verb
      self.hist      = g_AfniData_hist

      # computed variables
      self.cormat      = None   # correlation mat (normed xtx)
      self.cosmat      = None   # cosine mat (scaled xtx)
      self.cormat_ready = 0     # correlation mat is set

      # initialize...
      if self.init_from_filename(self.fname): return None


   def show(self):
      print self.make_show_str()

   def make_show_str(self):
      if self.ready: rstr = 'ready'
      else:          rstr = 'not ready'

      mstr = "++ name     : %s (%s)\n" \
             "   fname    : %s\n" \
             "++ nrows    : %d\n" \
             "   ncols    : %d\n" \
             "   row_lens : %s\n" \
             "++ rect     : %d\n" \
             "   square   : %d\n" \
             "   binary   : %d\n" \
             "++ tr       : %g\n" \
             "   nruns    : %d\n" \
             % (self.name, rstr, self.fname,
                self.nrows, self.ncols, self.row_lens,
                self.rect, self.square, self.binary, self.tr, self.nruns)

      return mstr

   # some accessor functions to match Afni1D
   def set_nruns(nruns): self.nruns = nruns

   def set_run_lengths(run_lengths):
      self.row_lens = run_lengths

   def looks_like_1D(self, run_lens=[], nstim=0, verb=1):
      """return whether data looks like 1D (stim_file) format
                - data should be rectangular
                  (what else can we even check?)
         if run_lens is passed,
                - nrows should be at least sum(run_lens)
                - warn if too many and verb > 0
         if nstim is passed,
                - number of columns should equal nstim

      """

      if not self.ready:
         print '** looks_like_1D: data not ready'
         return 0

      if self.empty:
         if verb > 1: print "-- empty file %s okay as 1D file" % self.fname
         return 1

      # error occur only as singles, to test against verb > 1

      errors = 0
      if not self.rect:
         errors |= ERR_ANY_MISC
         if verb > 1: print "** file %s is not rectangular" % self.fname

      # keep same local variable
      rlens = run_lens

      nruns = len(rlens)
      if nruns > 0:
         # if TR, scale it in
         tot_dur = sum(rlens)

         # if nrows is too small, error -- if too big, just warn
         if tot_dur > self.nrows:
            errors |= ERR_ANY_MISC
            if verb > 1:
               print "** file %s: nrows too small for run dur: %d < %d" \
                               % (self.fname, self.nrows, tot_dur)
         elif tot_dur < self.nrows:
            if verb > 0:
               print "** warning for 1D file %s, more rows than TRs: %d > %d" \
                     % (self.fname, self.nrows, tot_dur)

      if nstim > 0 and nstim != self.ncols:
         errors |= ERR_ANY_MISC
         if verb > 1: print "** file %s: ncols %d != nstim %d" \
                            % (self.fname, self.ncols, nstim)

      if errors == 0:
         if verb > 0: print '== GOOD: %s looks like 1D' % self.fname
         return 1
      else:
         if verb > 0: print '== BAD: %s does not look like 1D' % self.fname
         return 0

   def looks_like_local_times(self, run_lens=[], tr=0.0, verb=1):
      """return whether data looks like local stim_times format
                - times should be non-negative
                - times should be unique per run
         if run_lens is passed, 
                - number of runs should match nrows
                - maximum should be less than current run_length
         if tr is passed, scale the run lengths
      """

      # possibly scale run_lengths
      if tr > 0.0: rlens = [tr*rl for rl in run_lens]
      else:        rlens = run_lens

      nruns = len(rlens)

      if not self.ready:
         print '** looks_like_local_times: data not ready'
         return 0

      if self.empty:
         if verb > 1: print "-- empty file %s okay as local_times" % self.fname
         return 1

      # in case we know nothing, just check that values are non-negative
      # and unique
      # - if we have run lengths, check the number of runs and maximums, too
      errors = 0
      if nruns > 0:
         if nruns != self.nrows:
            errors |= ERR_ST_NUM_RUNS
            if verb > 2: print "** %d rows does not match %d runs" \
                               % (self.nrows, nruns)
      for rind in range(len(self.data)):
         # start with row copy
         row = self.data[rind][:]
         if len(row) == 0: continue
         row.sort()
         first = row[0]
         last = row[-1]
         if first < 0:
            errors |= ERR_ST_NEGATIVES
            if verb > 2: print "** row %d has negative time %g" % (rind, first)
         if not UTIL.vals_are_increasing(row):
            errors |= ERR_ST_NON_UNIQUE
            if verb > 2: print "** row %d has repetitive times" % rind
         if nruns > 0 and rind < nruns:
            if last >= rlens[rind]:
               errors |= ERR_ST_TOO_BIG
               if verb > 2:
                  print "** row %d has time %g exceeding run duration %g" \
                               % (rind, last, rlens[rind])

         del(row)

      if verb > 1:
         if errors:
            print "** file '%s' is not in -local_times format" % self.fname
            if errors & ERR_ST_NEGATIVES:  print '   - has negative times'
            if errors & ERR_ST_NON_UNIQUE: print '   - times are not unique'
            if errors & ERR_ST_NUM_RUNS:   print '   - num rows != num runs'
            if errors & ERR_ST_TOO_BIG:    print '   - times exceed run lengths'
         else: print '++ data looks like stim times'

      if errors == 0:
         if verb>0: print '== GOOD: %s looks like local stim_times'%self.fname
         return 1
      else:
         if verb > 0:
            print '== BAD: %s does not look like local stim_times' % self.fname
         return 0


   def looks_like_global_times(self, run_lens=[], tr=0.0, verb=1):

      if not self.ready:
         print '** looks_like_1D: data not ready'
         return 0

      if self.empty:
         if verb > 1: print "-- empty file %s okay as global_times" % self.fname
         return 1

      errors = 0

      # must be one row or column
      if self.ncols != 1 and self.nrows != 1:
         errors |= ERR_ANY_MISC
         if verb > 1: print "** file %s is not a single column" % self.fname

      # must rectangular
      if not self.rect:
         errors |= ERR_ANY_MISC
         if verb > 1: print "** file %s is not a rectangular" % self.fname

      # get a single sequence of numbers, depending on the direction
      if self.nrows == 1: data = self.data[0]
      else:data = [row[0] for row in self.data if len(row)>0]

      data.sort()

      if data[0] < 0.0:
         errors |= ERR_ANY_MISC
         if verb > 1: print "** file %s has negative time %g" \
                            % (self.fname, data[0])

      # possibly scale run_lengths
      if tr > 0.0: rlens = [tr*rl for rl in run_lens]
      else:        rlens = run_lens

      # note the total duration (tr == -1 implies just count TRs)
      endoftime = sum(rlens)
      if tr > 0.0: endoftime *= tr

      if data[-1] >= endoftime:
         errors |= ERR_ANY_MISC
         if verb > 1: print "** file %s has time %g after all runs, %g" \
                            % (self.fname, data[-1], endoftime)

      if not UTIL.vals_are_increasing(data):
            errors |= ERR_ANY_MISC
            if verb > 1: print "** file %s has repeat times" % self.fname

      if errors == 0:
         if verb > 0: print '== GOOD: %s looks like global stim_times' \
                            % self.fname
         return 1
      else:
         if verb > 0: print '== BAD: %s does not look like global stim_times' \
                            % self.fname
         return 0

   def init_from_filename(self, fname):
      """file could be 1D, timing or married timing data
        
         For now, store complete result but focus on times only.
      """

      mdata, clines = TD.read_married_file(fname, verb=self.verb)
      if mdata == None:
         print '** A1D: failed to read data file %s' % fname
         return 1

      # note whether the data is married (modulation or duration)
      if TD.married_type(mdata):
         self.married = 1

      # data will ignore any married information
      self.data     = [[val[0] for val in row] for row in mdata]
      self.mdata    = mdata
      self.clines   = clines
      self.fname    = fname
      
      self.nrows    = len(self.data)
      self.row_lens = [len(row) for row in self.data]

      # accept an empty file?
      if self.nrows == 0:
         self.empty = 1
         self.ready = 1
         return 0

      # if row lengths are all the same, use ncols, instead
      if UTIL.vals_are_constant(self.row_lens):
         self.ncols = self.row_lens[0]
         del(self.row_lens)
         self.row_lens = []
         self.rect  = 1

      if self.rect and (self.nrows == self.ncols): self.square = 1

      # check to see if it is a 0/1 file
      self.binary = 1
      for row in self.data:
         if not UTIL.vals_are_0_1(row):
            self.binary = 0
            break

      self.ready = 1
      
      return 0

if __name__ == '__main__':
   print '** this is not a main module'
   sys.exit(1)


