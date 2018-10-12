#!/usr/bin/env python

# python3 status: started

# currently, this explicitly does _not_ depend on scipy or numpy

from __future__ import print_function


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

MTYPE_NONE = 0   # no modulation        (these work as a bit mask)
MTYPE_AMP  = 1   # amplitude modulation
MTYPE_DUR  = 2   # duration modulation

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

         mat           : data (transposed, as vectors of data over time)
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
      self.nrowfull  = 0        # length without censoring
      self.nruns     = 1
      self.run_len   = [0]      # len(run_len) is number of runs
      self.run_len_nc= [0]      # run lengths without censoring
      self.nroi      = 1
      self.GLapplied = 0        # was goodlist applied (1=yes, 2=zero-padded)

      # misc variables (from attributes)
      self.command   = ''       # from CommandLine
      self.header    = []       # array of extra header (comment) lines
      self.csimobj   = None     # ClustSim object
      self.csimstat  = -1       # -1, 0, 1: undef, not csim, is csim

      # list variables (from attributes)
      self.havelabs = 0         # did we find any labels
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

      self.VO          = None   # VarsObject, if set

      # initialize...
      if self.fname:
         if self.init_from_general_name(self.fname): return None
      elif from_mat:
         if self.init_from_matrix(matrix): return None

      self.update_group_info()

      if self.verb > 2: self.show()

   def reduce_by_tlist(self, tlist):
      """reduce by time list, similiar to afni's {} selector
         this affects run_len and runs, so try to fix those

         return 0 on success"""

      if not self.ready:
         print('** append: Afni1D is not ready')
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

   def reduce_by_run_list(self, rlist):
      """extract the list of (1-based) runs from the dataset
         return 0 on success"""

      if not self.ready:
         print('** reduce_by_run_list: Afni1D is not ready')
         return 1

      # are all of the runs valid?
      errs = 0
      for run in rlist:
         if run < 1:
            print('** reduce_by_run_list: min run index is 1')
            errs += 1
         if run > self.nruns:
            print('** reduce_by_run_list: %d exceeds max run index %d' \
                  % (run, self.nruns))
            errs += 1
      if errs: return 1

      if len(self.runstart) != self.nruns:
         print('** run_list: missing %d run positions (-set_run_lengths?)' \
               % self.nruns)
         errs += 1
      if len(self.run_len) != self.nruns:
         print('** run_list: missing %d run lengths (-set_run_lengths?)' \
               % self.nruns)
         errs += 1
      if errs: return 1

      # make useful runstart list to begin with, which will be trashed anyway
      runstart = []
      ntotal = 0
      for rlen in self.run_len:
         runstart.append(ntotal)
         ntotal += rlen
      self.runstart = runstart

      # finally ready?  
      # make tlist, clear groups, reduce_by_tlist
      # and then update nruns, run_len, runstart
      self.groups = []
      tlist = []
      run_len = []      # make new lists as we go
      runstart = []     # make new lists as we go
      ntotal = 0        # for runstart list
      for run in rlist:
         r0 = run - 1
         nt = self.run_len[r0]
         tlist.extend([self.runstart[r0]+i for i in range(nt)])
         run_len.append(nt)
         runstart.append(ntotal)
         ntotal += nt

      if self.reduce_by_tlist(tlist): return 1

      self.nruns = len(rlist)
      self.run_len = run_len
      self.runstart = runstart

      return 0

   def reduce_by_vec_list(self, vlist):
      """reduce the dataset according to the vector list
         return 0 on success"""

      if not self.ready:
         print('** reduce_by_vec_list: Afni1D is not ready')
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

   def reduce_by_group_list(self, glist):
      """reduce the dataset according to the list of groups
         return 0 on success"""

      if not self.ready:
         print('** reduce_by_group_list: Afni1D is not ready')
         return 1

      glen = len(glist)
      if glen < 1: return 0

      if len(self.groups) < 1:
         print('** no groups to select in %s' % self.name)
         return 1

      # do not all repeats in group list
      groups = UTIL.get_unique_sublist(self.groups)

      gnew = glist[:]

      # convert to an integral group list
      for ind in range(glen-1,-1,-1):
         if gnew[ind] == 'POS':
            gnew[ind:ind+1] = [g for g in groups if g > 0]
         elif gnew[ind] == 'NEG':
            gnew[ind:ind+1] = [g for g in groups if g < 0]
         else:
            try: gnew[ind] = int(gnew[ind])
            except:
                print("** invalid selection group '%s'" % gnew[ind])
                return 1

      if self.verb > 2: print('-- red. by glist: groups %s' % gnew)
      cols = self.ordered_cols_by_group_list(gnew)
      if self.verb > 1: print('-- red. by glist: cols %s' % cols)
      return self.reduce_by_vec_list(cols)

   def show_header(self):
      print('\n'.join(self.header))

   def show_group_labels(self):
      show_groups = (len(self.groups) == self.nvec)
      show_labs = (len(self.labels) == self.nvec)
      if not show_groups and not show_labs:
         print('** no label info to show')
         return

      for ind in range(self.nvec):
         if self.verb:
            if show_groups: gstr = ', group %-3s' % self.groups[ind]
            else:           gstr = ''
            if show_labs:   lstr = ', label %s' % self.labels[ind]
            else:           lstr = ''
            print('index %3d%s%s' % (ind, gstr, lstr))
         elif show_labs: print('%s' % self.labels[ind])

   def reduce_by_label_prefix(self, keep_pre=[], drop_pre=[]):

      rv, newlist = self.label_prefix_to_ints(keep_pre, drop_pre)
      if rv: return 1

      self.reduce_by_vec_list(newlist)

      if self.verb>1: print('-- reduce_by_label_prefix, labels: %s'%self.labels)

      return 0

   def label_prefix_to_ints(self, keep_pre=[], drop_pre=[]):
      """return a list of label indices, based on what is kept or dropped
            keep labels in their original order

            if keep_pre and one of keep_pre matches, keep
            if drop_pre and one of drop_pre matches, remove

         return 0 on success, along with the int list"""

      if not self.ready:
         print('** reduce_by_label_prefix: Afni1D is not ready')
         return 1, []

      if len(self.labels) == 0:
         print('** no dataset labels to reduce by...')
         return 0, list(range(self.nvec))

      # make a list of label indices to keep, first, before removing
      if len(keep_pre) > 0:
         newlist = []
         for ind, lab in enumerate(self.labels):
            for pre in keep_pre:
               if UTIL.starts_with(lab, pre):
                  newlist.append(ind)
                  break
         newlist = UTIL.get_unique_sublist(newlist)
         if self.verb > 2: print('++ applied keep_pre to produce: %s' % newlist)
      else:
         newlist = list(range(len(self.labels)))

      if len(drop_pre) > 0:
         new2 = []
         for ind in newlist:
            keep = 1
            for pre in drop_pre:
               if UTIL.starts_with(self.labels[ind], pre):
                  keep = 0
                  break
            if keep: new2.append(ind)
         newlist = new2
         if self.verb > 2: print('++ applied drop_pre to produce %s' % newlist)

      return 0, newlist

   def run_info_is_consistent(self, whine=1):
      """verify consistency of nruns/run_len/nt"""
      if not self.ready:
         if whine: print('** RIIC: not ready (to test consistency)')
         return 0
      if len(self.run_len) == 0:
         if whine: print('** RIIC: ready be len(run_len) == 0!')
         return 0
      nt = UTIL.loc_sum(self.run_len)
      if nt != self.nt:
         if whine:
            print('** RIIC: nt=%d != sum of run_len: %s'%(self.nt,self.run_len))
         return 0
      return 1

   # --- begin: mathematical manipulations ---

   def transpose(self):
      """transpose the matrix and swap nrows and ncols"""
      if self.verb > 3: print('-- Afni1D transpose...')
      if not self.ready:
         print("** matrix '%s' is not ready for transposing" % self.name)
         return
      self.mat  = [[row[i] for row in self.mat] for i in range(self.nt)]
      newnt     = self.nvec
      self.nvec = self.nt
      self.nt   = newnt
      self.run_len = [self.nt]  # init to 1 run

   def demean(self):
      """demean each vector per run (mean of each run will be zero)

         return 0 on success"""

      if self.verb > 3: print('-- Afni1D demean...')

      if not self.ready:
         print('** demean: Afni1D is not ready')
         return 1

      # verify nruns and run_len
      if not self.run_info_is_consistent(whine=1):
         if self.verb > 1: print('** runs inconsistent for demean')
         return 1

      if self.verb > 1:
          print("-- demean: over %d runs of lengths %s" \
                % (self.nruns, self.run_len))

      # foreach run of each vector, demean
      for ind in range(self.nvec):
         offset = 0
         for t in self.run_len:
            UTIL.demean(self.mat[ind], offset, offset+t-1)
            offset += t

      return 0

   def derivative(self, direct=0):
      """change each value to its derivative (cur val - previous)
         new time[0] will always be 0

         process the data per run, so derivatives do not cross boundaries

         direct: 0 means backward difference, 1 means forward

         return 0 on success"""

      if self.verb > 3: print('-- Afni1D derivative...')

      if not self.ready:
         print('** derivative: Afni1D is not ready')
         return 1

      # verify nruns and run_len
      if not self.run_info_is_consistent(whine=1):
         if self.verb > 1: print('** runs inconsistent for derivative')
         return 1

      if self.verb > 1:
          print("-- derivative: over %d runs of lengths %s" \
                % (self.nruns, self.run_len))

      # apply derivative to each run of each vector
      for ind in range(self.nvec):
         UTIL.derivative(self.mat[ind], in_place=1, direct=direct)
         # if forward diff, clear final indexes, else clear initial
         if direct:
            offset = -1
            for t in self.run_len:
               offset += t
               self.mat[ind][offset] = 0
         else:
            offset = 0
            for t in self.run_len:
               self.mat[ind][offset] = 0
               offset += t

      return 0

   def abs(self):
      """take the absolute value of each entry
         return 0 on success, 1 on error"""

      if self.verb > 3: print('-- Afni1D abs...')

      if not self.ready:
         print('** abs: Afni1D is not ready')
         return 1

      for ind in range(self.nvec):
         newvec = []
         for rind in range(self.nt):
            self.mat[ind][rind] = abs(self.mat[ind][rind])

      return 0

   def bool_negate(self):
      """set clear values and clear set values
         return 0 on success, 1 on error"""

      if self.verb > 3: print('-- bool_negate: old zeros = %d'  \
                              % self.mat[0].count(0))

      if not self.ready:
         print('** bool_negate: Afni1D is not ready')
         return 1

      for v in range(self.nvec):
         for t in range(self.nt):
            if self.mat[v][t]: self.mat[v][t] = 0
            else:              self.mat[v][t] = 1

      if self.verb > 3: print('-- negate: new zeros = %d' % self.mat[0].count(0))

      return 0

   def volreg_2_allineate(self):
      """convert a 6-parameter time series from 3dvolreg to a 12-parameter
         time series for 3dAllineate

         params (3dvolreg -1Dfile):    roll, pitch, yaw,    dS,     dL,   dP
                (3dAllineate 1Dapply):
           
         3dvolreg params:    v0  v1  v2   v3  v4  v5
         3dAllieate params: -v4 -v5 -v3   v0  v1  v2
         
         Permute and negate vectors, and append [0] vectors.

         return 0 on success
      """
      if self.verb > 3: print('-- volreg_2_allineate...')
      if not self.ready:
         print("** matrix '%s' is not ready for volreg2allin" % self.name)
         return 1
      if self.nvec != 6:
         print("** matrix '%s' does not have 6 columns" % self.name)
         return 1

      mm = self.mat     # for convenience

      # negate second triplet of vectors
      for col in range(3,6):
         mm[col] = [-val for val in mm[col]]

      # permute and append 6 [0] vectors
      zvec = [0] * self.nt
      self.mat  = [mm[4], mm[5], mm[3], mm[0], mm[1], mm[2],
                   zvec,  zvec,  zvec,  zvec,  zvec,  zvec ]
      self.nvec = 12

      return 0

   def collapse_cols(self, method, weight=None):
      """collapsed the matrix to a single array of length nt (nvec will = 1)

         collapsing will apply 'method' across the nvec per time point

             method = 'min'             : min across nvec
             method = 'minabs'          : min abs across nvec
             method = 'max'             : max across nvec
             method = 'maxabs'          : max abs across nvec
             method = 'euclidean_norm'  : sqrt(sum squares)
                 or = 'enorm'
             method = 'weighted_enorm'  : sqrt(sum weighted squares)

         Note: the result will still be a trivial 2-D array, where element 0
               is the collapsed time series.  This allows other functionality
               to still work.

         return 0 on success, 1 on error"""

      if self.verb > 3: print('-- collapse cols, method = %s' % method)

      if not self.ready:
         print('** collapse: Afni1D is not ready')
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
      elif method == 'enorm' or method == 'euclidean_norm':
         mat = [UTIL.euclidean_norm([self.mat[v][t] for v in range(self.nvec)])\
                                                    for t in range(self.nt)]
      elif method == 'weighted_enorm':
         if weight == None:
            print('** missing weight vector for weighted_enorm')
            return 1
         if len(weight) != self.nvec:
            print('** weighted_enorm weight vector length (%d) != nvec (%d)' \
                  % (len(weight), self.nvec))
            return 1
         mat = [UTIL.weighted_enorm(
                  [self.mat[v][t] for v in range(self.nvec)], weight) \
                  for t in range(self.nt)]
      else:
         print("** collapse_cols: unknown method:", method)
         return 1

      # note that there is only a single column left
      self.nvec = 1
      self.clear_cormat()

      del(self.mat)
      self.mat = [mat]
      return 0

   def mat_times_vec(self, vec):
      """modify matrix as self.mat * vec"""
      lv = len(vec)
      if lv == 0 or self.nt == 0 or self.nvec == 0:
         if self.verb > 1: print('** have mat_times_vec with empty inputs')
         return

      for col, mvec in enumerate(self.mat):
         val = vec[col]
         for ind in range(self.nt):
            mvec[ind] *= val

   def unitize(self):
      """make every vector unit length, use existing memory"""

      for ind, vec in enumerate(self.mat):
         vlen = UTIL.L2_norm(vec)
         if vlen == 0.0:
            for ind in range(self.nt): vec[ind] = 0
         else:
            for ind in range(self.nt): vec[ind] /= vlen

   def project_out_vec(self, vec=None, unit=0):
      """project a vector out of the matrix vectors, if None, use mean

         for each vector y, subtract <vT,y>/<vT.v> * v

         if unit: subtract mean of unit vectors

         return 0 on success, else 1
      """
      if vec == None:
          if unit: vec = self.get_mean_unit_vec()
          else:    vec = self.get_mean_vec()
      if len(vec) != self.nt:
         print('** project_out_vec: bad length %d != %d' % (len(vec), self.nt))
         return 1

      vlen = UTIL.L2_norm(vec)
      if vlen == 0: # then nothing to do
         if self.verb > 0: print('-- project_out_vec: empty vector to remove')
         return 0
      vunit = UTIL.lin_vec_sum(1.0/vlen, vec, 0, None)

      self.mat = [UTIL.proj_out_vec(mv, vunit, unit_v2=1) for mv in self.mat]

   # --- end: mathematical manipulations ---

   # --- these functions return some aspect of the matrix ---

   def get_mean_vec(self):
      """return a single vector as the mean of all matrix vectors"""
      v = [0] * self.nt
      if self.nt == 0: return []
      for ind in range(self.nt):
         v[ind] = UTIL.loc_sum([self.mat[i][ind] for i in range(self.nvec)])
      return UTIL.lin_vec_sum(1.0/self.nvec, v, 0, None)

   def get_mean_unit_vec(self, demean=1):
      """return a single vector as the mean of all matrix vectors
         AFTER the vectors have been (demeaned? and) scaled to unit lengths
      """
      adcopy = self.copy()
      if demean: adcopy.demean()
      adcopy.unitize()
      gu = adcopy.get_mean_vec()
      del(adcopy)
      return gu

   def show_gcor_all(self):
      for ind in range(1,6):
         print('----------------- GCOR test %d --------------------' % ind)
         exec('val = self.gcor%d()' % ind)
         exec('print("GCOR rv = %s" % val)')

   def show_gcor_doc_all(self):
      for ind in range(1,6):
         print('----------------- GCOR doc %d --------------------' % ind)
         exec('val = self.gcor%d.__doc__' % ind)
         exec('print("%s" % val)')

   # basically, a link to the one we really want to call
   def gcor(self): return self.gcor2()

   def show_gcor(self, verb=-1):
      gc = self.gcor()
      nv = self.nvec
      nt = self.nt
      if verb < 0: verb = self.verb
      if verb: print("GCOR for %d time series of length %d = %g" % (nv, nt, gc))
      else:    print("%g" % gc)

   def gcor1(self):
      """GOLD STANDARD

         compute GCOR via:
           - fill cormat (correlation matrix)
           - return average
      """
      self.set_cormat()
      if not self.cormat_ready: return 0

      ss = 0.0
      for r in range(self.nvec):
         for c in range(self.nvec):
            ss += self.cormat[r][c]

      return float(ss)/(self.nvec*self.nvec)

   def gcor2(self):
      """PLATNUM STANDARD (method applied in afni_proc.py)

         compute GCOR via:
           - demean
           - unitize
           - get average unit time series gu
           - return sum_of_squares(gu) = length(gu)^2
      """
      adcopy = self.copy()
      adcopy.demean()
      adcopy.unitize()
      gu = adcopy.get_mean_vec()
      
      en = UTIL.dotprod(gu,gu)

      return en

   def gcor3(self):
      """compute GCOR via:
           - demean
           - unitize
           - get average unit time series gu
           - compute average correlation of gu and each unit vector
           - return |gu| times ave corr with gu
      """

      adcopy = self.copy()
      adcopy.demean()
      adcopy.unitize()

      gu = copy.deepcopy(adcopy.mat[0])
      for ind in range(1, adcopy.nvec):
         gu = UTIL.lin_vec_sum(1, gu, 1, adcopy.mat[ind])
      gu = [val/float(adcopy.nvec) for val in gu]

      ss = 0.0
      for r in range(adcopy.nvec):
         ss += UTIL.correlation_p(gu, adcopy.mat[r])

      ss /= adcopy.nvec

      return UTIL.euclidean_norm(gu)*ss

   def gcor4(self):
      """compute GMEAN via:
           - get average time series gmean
           - get average correlation of gmean and each unit vector
           - return square (akin to above, but with gmean, not gu)
      """

      ss = self.get_ave_correlation_w_vec(self.get_mean_vec())

      print("ave corr = %s, square = %s" % (ss, ss*ss))

      return ss*ss

   def gcor5(self):
      """compute GCOR via:
           - get average time series mean, gmean
           - get average unit time series mean, gu
           - return (gu.gmean/|gmean|)^2
      """

      adcopy = self.copy()
      adcopy.demean()

      m0 = adcopy.get_mean_vec()

      adcopy.unitize()
      m1 = adcopy.get_mean_vec()

      dd = UTIL.dotprod(m0, m1)
      l0 = UTIL.euclidean_norm(m0)
      l1 = UTIL.euclidean_norm(m1)
      c  = UTIL.correlation_p(m0, m1)

      print("len(gmean) = %s, square = %s" % (l0, l0*l0))
      print("len(gunit) = %s, square = %s" % (l1, l1*l1))
      print("corr(gm, gu)            = %s" % (c))
      print("corr(gm, gu)*len(gu)    = %s" % (c*l1))
      print("squared                 = %s" % (c*c*l1*l1))

      return dd*dd/(l0*l0)

   def get_ave_correlation_w_vec(self, vec):
      """return the average correlation of each vector with vec"""
      gu = self.get_mean_vec()
      ss = 0
      for r in range(self.nvec):
         ss += UTIL.correlation_p(vec, self.mat[r])
      ss /= self.nvec

      return ss

   def get_gcor_wout_gmean(self, unitize=0):
      """project out global mean, then compute gcor
         unitize: unitize before projection
      """
      adcopy = self.copy()
      if unitize: adcopy.unitize()
      adcopy.project_out_vec()
      gc = adcopy.gcor()
      del(adcopy)
      return gc

   # --- end: functions returning some aspect of the matrix ---

   def copy(self):
      """return a complete (deep)copy of the current object"""
      return copy.deepcopy(self)

   def extreme_mask(self, emin, emax, inclusive=1):
      """convert to a time series mask of 0/1 values where the result is
         1, if value is extreme (>emax or <emin, with = if inclusive)
         0, otherwise (moderate)

         For example, these would be the TRs to omit in a censor.1D file.
        
         return 0 on success"""

      if self.verb > 3:
         if inclusive: print('-- extreme_mask: excluding (%g,%g)'%(emin,emax))
         else:         print('-- extreme_mask: excluding [%g,%g]'%(emin,emax))

      if not self.ready:
         print('** extreme_mask: Afni1D is not ready')
         return 1

      if emin > emax:
         print('** extreme_mask: emin > emax (', emin, emax, ')')
         return 1

      self.mat = [UTIL.vec_extremes(vec,emin,emax,inclusive)[1] \
                        for vec in self.mat]

      if self.verb > 1:
         count = UTIL.loc_sum([val for val in self.mat[0] if val == 1])
         print('++ extreme_mask: removing %d of %d vals' % (count,self.nt))

      return 0

   def moderate_mask(self, emin, emax, inclusive=1):
      """convert to a time series mask of 0/1 values where the result is
         1, if value is moderate (within (emin,emask), with = if inclusive)
         0, otherwise (extreme)

         For example, these would be the TRs to keep in a censor.1D file.
        
         return 0 on success"""

      if self.verb > 3:
         if inclusive: print('-- moderate_mask: keeping (%g,%g)'%(emin,emax))
         else:         print('-- moderate_mask: keeping [%g,%g]'%(emin,emax))

      if not self.ready:
         print('** moderate_mask: Afni1D is not ready')
         return 1

      if emin > emax:
         print('** moderate_mask: emin > emax (', emin, emax, ')')
         return 1

      self.mat = [UTIL.vec_moderates(vec,emin,emax,inclusive)[1] \
                        for vec in self.mat]

      if self.verb > 1:
         count = UTIL.loc_sum([val for val in self.mat[0] if val == 1])
         print('++ moderate_mask: keeping %d of %d vals' % (count,self.nt))

      return 0

   def set_first_TRs(self, nfirst, newval=1):
      """set the first nfirst TRs to newval"""

      if self.verb > 3:
         print('-- setting first %d TRs, newval = %d ...' % (nfirst, newval))

      if not self.ready:
         print('** set_first_TRs: Afni1D is not ready')
         return 1

      # apply 'newval' to first 'nfirst' in each run
      for ind in range(self.nvec):
         offset = 0
         for run, rlen in enumerate(self.run_len):
            if nfirst > rlen:
               print('** mask_first_TRs, nfirst %d > run len %d (of run %d)' \
                     % (nfirst, rlen, run+1))
               return 1
            # apply censor val for first nfirst TRs
            for tr in range(nfirst): self.mat[ind][offset+tr] = newval
            offset += rlen

      return 0

   def mask_prior_TRs(self):
      """if one TR is set, also set the prior one"""

      if self.verb > 3: print('-- masking prior TRs...')

      if not self.ready:
         print('** mask_prior_TRs: Afni1D is not ready')
         return 1

      for v in range(self.nvec):
         for t in range(self.nt-1):
            if self.mat[v][t+1] and not self.mat[v][t]: self.mat[v][t] = 1

      return 0

   def clear_next_TRs(self):
      """if one TR is clear, also clear the next one"""

      if self.verb > 3: print('-- clearing next TRs...')

      if not self.ready:
         print('** clear_next_TRs: Afni1D is not ready')
         return 1

      for v in range(self.nvec):
         for t in range(self.nt-2, -1, -1):
            if not self.mat[v][t] and self.mat[v][t+1]: self.mat[v][t+1] = 0

      return 0

   def clear_prior_TRs(self):
      """if one TR is clear, also clear the prior one"""

      if self.verb > 3: print('-- clearing prior TRs...')

      if not self.ready:
         print('** clear_prior_TRs: Afni1D is not ready')
         return 1

      for v in range(self.nvec):
         for t in range(self.nt-1):
            if not self.mat[v][t+1] and self.mat[v][t]: self.mat[v][t] = 0

      return 0

   def pad_into_many_runs(self, rindex1, nruns, rlengths=[]):
      """pad over time so that this is run #rindex1 out of nruns runs
         (rindex1 should be 1-based)

         if rlengths is not set, assume all runs are of a set length
         if rlengths IS set, nruns is ignored
     
         Only one of nruns/rlengths is is applied.

         return 0 on success"""

      if self.verb > 3: print('-- pad_into_many_runs: rindex1=%d, nruns=%d' \
                              % (rindex1,nruns))

      if not self.ready:
         print('** pad into runs: Afni1D is not ready')
         return 1

      # ---- set NR (nruns) and rlens (list of lengths)

      # decide whether to use rlengths or nruns
      if len(rlengths) > 0:
         NR = len(rlengths)
         rlens = rlengths
      else:     # assume nruns runs of length self.nt
         if nruns < 1:
            print('** pad into runs: bad nruns (%d)' % nruns)
            return 1
         NR = nruns
         rlens = [self.nt for r in range(NR)]

      # ---- check for consistency

      # verify rindex1 (using 1-based run index)
      if rindex1 < 1 or rindex1 > NR:
         print('** pad into runs: run index (%d) out of range [1,%d]' \
               % (rindex1, NR))
         return 1

      # apply rind as 0-based run index
      rind = rindex1-1

      # if rlengths, verify match for run #rindex1
      if self.nt != rlens[rind]:
         print("** cannot pad into many runs, nt (%d) != rlens[%d] (%d)" \
               % (self.nt, rind, rlens[rind]))
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
      self.nt      = UTIL.loc_sum(rlens)

      return 0

   def randomize_trs(self, seed=0):
      """reorder the matrix columns randomly"""
      if self.verb > 1: print('-- randomizing %d trs (seed=%d)' \
                              % (self.nt, seed))
      if seed:
         import random
         random.seed(seed)
      for vec in self.mat: UTIL.shuffle(vec)

   def sort(self, reverse=0):
      """sort data over time axis (possibly reverse order)"""

      if self.verb > 3: print('-- Afni1D sorting...')

      for ind in range(self.nvec):
         self.mat[ind].sort(reverse=reverse)

      return 0

   def reverse(self):
      """reverse data over time axis"""

      if self.verb > 3: print('-- Afni1D reverse...')

      ilist = UTIL.decode_1D_ints('$..0(-1)',verb=self.verb,imax=self.nt-1)
      if self.reduce_by_tlist(ilist): return 1

      return 0

   def get_censored_trs(self, run_index=-1):
      """return a list of TRs that were censored
         (basically, return an inverted goodlist)

         if run_index >= 0: restrict to that 0-based run
            (apply runstart info after invert_int_list)

         return status (0=success) and the TR index list
      """

      rv, ulist = self.get_uncensored_trs()  # no run_index
      if rv: return 1, []

      if self.nrowfull > 0: nt = self.nrowfull
      else: nt = self.nt

      tr_list = UTIL.invert_int_list(ulist, top=nt-1)

      return self.restrict_tr_list_to_run(tr_list, run_index)

   def get_uncensored_trs(self, run_index=-1):
      """return a list of TRs that were used, i.e. we not censored
         (basically, return goodlist)

         there are 2 valid types of inputs:

           1. X-matrix format
                len(goodlist) > 0 and nrowfull >= len(goodlist)

              if run_index >= 0, limit goodlist indices base on run_len[]
              (run index is 0-based)

           2. binary format
                nvec == 1 and UTIL.vals_are_0_1(mat[0])

         return status (0=success) and the TR index list
      """

      if not self.ready:
         print("** Afni1D not ready for get_uncensored_trs")
         return 1, []

      # either have goodlist from xmat or take indices of non-zero values
      if len(self.goodlist) > 0:
         tr_list = self.goodlist
      else:
         tr_list = [i for i,v in enumerate(self.mat[0]) if v]

      # and restrict to run_index
      return self.restrict_tr_list_to_run(tr_list, run_index)

   def uncensor_from_vector(self, cvec):
      """zero-pad list to match length of cvec
         current list length must match uncensored cved list
      """
      if self.nruns > 1:
         print('** cannot uncensor multiple runs from vector')
         return 1

      clen = len(cvec)
      ncen = cvec.count(0)
      if self.nt != clen - ncen:
         print('** uncensor from vec: nt = %d, but nocen len = %d' \
               (self.nt, clen-ncen))
         return 1

      # explicitly set newvec, including with zeros
      newmat = []
      for mvec in self.mat:
         newvec = cvec[:]
         goodind = 0
         for ind, val in enumerate(cvec):
            if val:
               newvec[ind] = mvec[goodind]
               goodind += 1
            else:
               newvec[ind] = 0
         newmat.append(newvec)

      if self.verb > 1:
         print('++ uncensored %d values from vector' % ncen)
         if self.verb > 2:
            cenlist = ['%s'%i for i in range(clen) if not cvec[i]]
            print('   zero-pad list: %s' % (', '.join(cenlist)))
            del(cenlist)

      del(self.mat)
      self.mat     = newmat
      self.nt      = clen
      self.run_len = [self.nt]

      return 0

   def restrict_tr_list_to_run(self, tr_list, run_index=-1):
      """rely on runstart to restrict tr_list
         return status and restricted list
      """
     
      # maybe there is nothing to do
      if run_index < 0: return 0, tr_list

      if run_index >= self.nruns:
         print('** cannot restrict TR list for (1-based) run %d, have %d' \
               % (run_index+1, self.nruns))
         return 1, []

      if self.nrowfull > 0: nt = self.nrowfull
      else: nt = self.nt

      # restrict return values to [bot, top)
      bot = self.runstart[run_index]
      if run_index == self.nruns - 1: top = nt
      else:                           top = self.runstart[run_index+1]

      if self.verb > 2:
         print('-- restricting %d TRs between %d and %d' \
               % (len(tr_list),bot,top-1))

      return 0, [v for v in tr_list if v >= bot and v < top]

   def show_censor_count(self, invert=0, column=0):
      """display the total number of TRs censored (clear) in the given column

         If multi-column data, can choose one.

            invert   : invert column count
            column   : which column to count values from

         return status"""

      if self.verb > 3:
         print('++ showing censor count (inv=%d, col=%d)' % (invert, column))

      if not self.ready:
         print("** Afni1D not ready for write_timing to '%s'" % fname)
         return 1

      ccount = self.mat[0].count(0)             # start by counting 0
      lstr = '(simple form)'

      # if it is a valid x-mat, override (should be a separate function)
      if self.havelabs:
         lstr = '(from xmat header)'
         rv, tplist = self.get_censored_trs()
         if not rv:
            ccount = len(tplist)

      if invert: ccount = self.nt - ccount

      if self.verb: print('total number of censored TRs %s = %d'%(lstr, ccount))
      else:         print(ccount)

      return 0

   # ----------------------------------------------------------------------
   # cluster table functions

   def is_csim_type(self):
      """test whether this element seems to be a clustsim table"""
      if len(self.header) < 1:  return 0
      if len(self.mat) <= 1:    return 0

      if self.csim_fill_obj():  return 0 # if csim, status -> 1
      if self.csimstat < 1:     return 0

      return 1

   def csim_show_min_clust_size(self, pthr=0.001, alpha=0.05, verb=1):
      """given pthr/alpha, show min cluster size and cluster details,
         depending on verb

         verb 0: just print size
              1: also so attributes
              2: all debug attributes
      """

      csize = self.csim_clust_size(pthr, alpha)
      if verb == 0:
         print("%s" % csize)
         return 0

      # else, show more verbose output
      self.csim_show_attrs(verb=verb)
      print("    pthr           : %s" % pthr)
      print("    alpha          : %s" % alpha)
      print("    min clust nvox : %s" % csize)

      return 0

   def csim_clust_size(self, pthr=0.001, alpha=0.05):
      """given pthr/alpha, return min clust size
         return 0 on error"""

      if self.csim_fill_obj():          return 0
      if not self.csim_has_all_attrs(): return 0
  
      # get to work
      try:
         pind = self.mat[0].index(pthr)
      except:
         print('** uncorrected pthr = %s not in ClustSim table' % pthr)
         return 0

      try:
         aind = self.csimobj.avals.index(alpha)
      except:
         print('** corrected alpha = %s not in ClustSim table' % alpha)
         return 0

      try:
         csize = int(math.ceil(self.mat[aind+1][pind]))
      except:
         print("** csim_clust_size: unknown index error")
         return 0

      return csize

   def csim_show_attrs(self, verb=1):
      if not self.csim_has_vo():
         print("** ClustSim: no attribute object")
         return 1

      if not self.csim_has_all_attrs(verb=verb):
         print("** ClustSim attributes are missing\n")

      print("")

      if verb > 1:
         self.csimobj.show()

      print("ClustSim attributes:")
      print("    neighbors      : NN-%s" % self.csimobj.val('NN'))
      print("    sidedness      : %s" % self.csimobj.val('sided'))
      print("    blur est type  : %s" % self.csimobj.val('btype'))
      print("    grid voxels    : %s" % self.csimobj.val('grid_nvox'))
      print("    grid vox size  : %s" % self.csimobj.val('grid_vsize'))
      print("    mask N voxels  : %s" % self.csimobj.val('mask_nvox'))
      print("")

      return 0

   def csim_has_vo(self):
      if self.csimstat !=  1:                               return 0
      if self.VO == None:                                   return 0
      if not isinstance(self.csimobj, self.VO.VarsObject):  return 0
      return 1

   def csim_has_all_attrs(self, verb=1):
      if not self.csim_has_vo(): return 0

      alist = ['command', 'btype', 'bvals', 'sided', 'grid_nvox', 'grid_vsize',
               'mask_nvox', 'NN', 'avals']
      cobj = self.csimobj
      attrs = cobj.attributes()
      hasall = 1
      for aname in alist:
         if not aname in attrs:
            hasall = 0
            if verb and not cobj.whined:
               print('** csim obj, missing attribute %s' % aname)

      # only whine once
      if verb and not cobj.whined and not hasall:
         cobj.whined = 1

      return hasall

   # if we like this, formalize via class ClusterTable, say
   def csim_fill_obj(self, refill=0):
      # if not refill and we have set status, there is nothing to do
      if not refill and self.csimstat != -1:
         return 0

      # fill as if we have not been here before
      self.csimstat = 0

      if len(self.header) < 1: return 0
      if len(self.mat) <= 1:   return 0
      if not UTIL.starts_with(self.header[0], '# 3dClustSim '): return 0

      try:
         # try this out, it is separate, so make_random_timing need not import
         import lib_vars_object as VO
         self.VO = VO
         cobj = self.VO.VarsObject()
         cobj.whined = 0
         for cline in self.header:
            csplit = cline.split()
            if UTIL.starts_with(cline, '# 3dClustSim '):
               # command, btype, bvals[3]
               cobj.command = cline[2:]
               for ind in range(len(csplit)):
                  # get blur option
                  if csplit[ind] == '-acf':
                     cobj.btype = 'ACF'
                     cobj.bvals = [float(v) for v in csplit[ind+1:ind+4]]
                  elif csplit[ind] == '-fwhm':
                     cobj.btype = 'FWHM'
                     cobj.bvals = [float(v) for v in csplit[ind+1:ind+4]]
                  elif csplit[ind] == '-fwhmxyz':
                     cobj.btype = 'acf'
                     bval = float(csplit[ind+1])
                     cobj.bvals = [bval, bval, bval]
                  elif csplit[ind] == '-insdat':
                     cobj.btype = 'NONE'
                     cobj.bvals = []
                  else:
                     continue
                  
            elif cline.find('thresholding') > 1:
               cobj.sided = csplit[1]

            elif UTIL.starts_with(cline, '# Grid: '):
               # grid_nvox, grid_vsize, mask_nvox
               gline = cline[8:]
               vind = gline.find(' (')
               gvals = gline[0:vind].split()
               cobj.grid_nvox = gvals[0]
               cobj.grid_vsize = ' '.join(gvals[1:3])
               msplit = gline[vind+2:].split()
               cobj.mask_nvox = int(msplit[0])
               
            elif UTIL.starts_with(cline, '# -NN '):
               cobj.NN = int(csplit[2])

            elif UTIL.starts_with(cline, '#  pthr '):
               cobj.avals = [float(csplit[i]) for i in range(3,len(csplit))]
         self.csimobj = cobj
      except:
         print('** failed to convert 3dClustSim header to object')
         return 1

      self.csimstat = 1
      return 0

   # ----------------------------------------------------------------------

   def write(self, fname, sep=" ", overwrite=0):
      """write the data to a new .1D file

            fname       : filename is required
            sep         : separator between elements
            overwrite   : whether to allow overwrite

         return status"""

      if self.verb > 2: print('-- Afni1D write to %s, o=%d'%(fname,overwrite))

      if not self.ready:
         print("** Afni1D not ready for write to '%s'" % fname)
         return 1
      if not fname:
         print("** missing filename for write")
         return 1

      if fname == '-' or fname == 'stdout': fp = sys.stdout
      else:
         # normal file name: check existence and open
         if os.path.exists(fname) and not overwrite:
            print("** output file '%s' exists and 'overwrite' not set..."%fname)
            return 1

         fp = open(fname, 'w')
         if not fp:
            print("** failed to open '%s' for writing" % fname)
            return 1

      for row in range(self.nt):
         fp.write(sep.join(['%g' % self.mat[i][row] for i in range(self.nvec)]))
         fp.write('\n')

      fp.close()

      return 0

   def split_into_padded_runs(self, prefix, overwrite=0):
      """write one 1D file per run, where each is the same as the input, but
         is zero for every non-current run (i.e. only 1 non-zero run each)
      """

      if self.verb > 1:
         print('++ splitting into %d padded runs (rows=%d, cols=%d, prefix=%s)'\
               % (self.nruns, self.nt, self.nvec, prefix))

      # if we don't have multiple runs, there is little to do
      if self.nruns <= 1:
         return self.write('%s.r01.1D' % prefix, overwrite=overwrite)

      # store the data (self.mat) separately and start with 0-filled matrix
      orig_mat = self.mat
      self.mat = [[0 for row in range(self.nt)] for col in range(self.nvec)]

      # per run: copy orig data, write, zero copied data data (back to all-0)
      end = 0
      for rind, rlen in enumerate(self.run_len):
         start = end
         end += self.run_len[rind]
         fname = '%s.r%02d.1D' % (prefix,rind+1)
         if self.verb>2: print('-- write %s, rows %d..%d' % (fname,start,end-1))
         # copy orig data
         for col in range(self.nvec):
            for row in range(start, end):
               self.mat[col][row] = orig_mat[col][row]
         # write
         if self.write(fname, overwrite=overwrite): return 1
         # re-zero copied data
         for col in range(self.nvec):
            for row in range(start, end):
               self.mat[col][row] = 0

      # and re-insert matrix
      del(self.mat)
      self.mat = orig_mat

      return 0

   def write_as_timing(self, fname, invert=0, column=0):
      """write set TRs to a timing file, one run per run, using '*' for
         empty runs (two for first run)

         If multi-column data, can choose one.

            fname    : required filename
            invert   : write times for zero data TRs
            column   : which column to write times as

         return status"""

      if self.verb > 3: print('-- Afni1D write_timing to %s, inv=%d, col=%d' \
                              %(fname, invert, column))

      if not self.ready:
         print("** Afni1D not ready for write_timing to '%s'" % fname)
         return 1

      err, tstr = UTIL.make_timing_string(self.mat[column],
                                          self.nruns, self.tr, invert)
      if err: return 1

      if fname == '-' or fname == 'stdout':
         fp = sys.stdout
      else:
         try: fp = open(fname, 'r')
         except:
            print("** failed to open file '%s'" % fname)
            err = 1

         if err: return 1

      fp.write(tstr)
      if fp != sys.stdout:
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

      if self.verb > 3: print('-- Afni1D write_censortr to %s, inv=%d, col=%d' \
                              %(fname, invert, column))

      if not self.ready:
         print("** Afni1D not ready for write_censortr to '%s'" % fname)
         return 1

      if not self.run_info_is_consistent(whine=1): return 1

      err,cstr = UTIL.make_CENSORTR_string(self.mat[column],
                               rlens=self.run_len, asopt=1, verb=self.verb)

      if err: return 1

      try: fp = open(fname, 'w')
      except:
         print("** failed to open file '%s'" % fname)
         err = 1

      if err: return 1

      fp.write(cstr)
      fp.write('\n')    # add a newline
      fp.close()

   def append_vecs(self, matlist, newname=''):
      """append each Afni1D to the current one"""
      # test before trashing current matrix
      if not self.ready:
         print('** append: Afni1D is not ready')
         return 1

      if self.verb > 3: print('-- Afni1D append_vecs...')

      # allow matlist to be a simple mat
      if type(matlist) == type(self):
         newmats = [matlist]
      elif type(matlist) == type([]):
         if type(matlist[0]) == type(self): newmats = matlist
         else:
            print('** append_vecs: matlist elements not Afni1D')
            return 1
      else:
         print('** append_vecs: matlist must be list of Afni1D')
         return 1

      for mat in newmats:
         if not mat.ready:
            print('** append: Afni1D is not ready')
            return 1
         if mat.nt != self.nt:
            print('** append: nt differs (%d != !%d)' % (mat.nt, self.nt))
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

   def simplecopy(self):
      return Afni1D(from_mat=1, matrix=self.mat, verb=self.verb)

   def show_group_labels(self):
      show_groups = (len(self.groups) == self.nvec)
      show_labs = (len(self.labels) == self.nvec)
      if not show_groups and not show_labs:
         print('** no label info to show')
         return

      for ind in range(self.nvec):
         if self.verb:
            if show_groups: gstr = ', group %-3s' % self.groups[ind]
            else:           gstr = ''
            if show_labs:   lstr = ', label %s' % self.labels[ind]
            else:           lstr = ''
            print('index %3d%s%s' % (ind, gstr, lstr))
         elif show_labs: print('%s' % self.labels[ind])

   def show_labels(self):
      print('++ labels are:', self.labels)

   def show_major_order_of_labels(self):
      """be picky and verify that labels look like sSLICE.NAME, where
         SLICE increments (slowly) over slice index"""

      if self.verb > 3: print('-- Afni1D show_major_order_of_labels...')

      if not self.labels:
         print('** no labels to test for ordering')
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
         if sorted: print('++ labels in row-major order: YES')
         else:      print('++ labels in row-major order: NO')
      except:
         print('++ labels are not of expected format, cannot determine ordering')
         self.show_labels()

   def clear_cormat(self):
      """nuke any existing cormat"""
      if self.verb > 3: print('-- Afni1D clear_cormat...')
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
      if self.verb > 3: print('-- Afni1D set_cormat...')
      if not self.ready: return
      if self.cormat_ready:
         if not update: return
         self.clear_cormat() # otherwise, nuke the old stuff and re-generate

      if self.nvec < 2 or self.nt < 2: return

      # make copy to abuse
      try: cmat = copy.deepcopy(self)
      except:
         print('... deepcopy failure, using simplecopy()...')
         cmat = self.simplecopy()

      # demean each vector (for cormat), unless it is constant
      means = [UTIL.loc_sum(vec)/float(cmat.nt) for vec in cmat.mat]
      for v in range(cmat.nvec):
         lmin = min(cmat.mat[v])
         lmax = max(cmat.mat[v])
         # rcr - why avoid this and leave constant terms?
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

   def show_cormat_diff_wout_gmean(self, unit=0, dp=3, spaces=2):
      ccopy = self.get_cormat_diff_wout_gmean(unit=unit, dp=dp, spaces=spaces)
      ccopy.show_cormat(dp=dp, spaces=spaces)
      del(ccopy)

   def get_cormat_diff_wout_gmean(self, unit=0, dp=3, spaces=2):
      adcopy = self.copy()
      adcopy.project_out_vec(unit=unit)
      self.set_cormat(update=1)
      adcopy.set_cormat(update=1)

      for rind, row in enumerate(adcopy.cormat):
         for ind in range(len(row)):
            row[ind] = self.cormat[rind][ind] - row[ind]
      return adcopy

   def show_cormat(self, dp=3, spaces=2):
      """print the correlation matrix, to the given number of places
            dp > 0      : use that to right of decimal
            dp = 0      : use %g
            dp = -1     : use %f
      """
      self.set_cormat()
      self.show_mat(self.cormat, dp=dp, spaces=spaces)

   def show_mat(self, mat, dp=3, spaces=2):
      """print the correlation matrix, to the given number of places
            dp > 0      : use that to right of decimal
            dp = 0      : use %g
            dp = -1     : use %f
      """
      ss = ' '*spaces
      for v, vec in enumerate(mat):
         for val in vec:
            if dp > 0:    ps = "%.*f%s" % (dp, val, ss)
            elif dp == 0: ps = "%g%s" % (val, ss)
            else:         ps = "%f%s" % (val, ss)
            print(ps, end='')
         print("")

   def make_cormat_warnings_string(self, cutoff=0.4, name=''):
      """make a string for any entires at or above cutoffs:
            cut0=1.0, cut1=(1.0+cutoff)/2.0, cut2=cutoff

            cut0, cut1, cut2 are cutoff levels (cut0=highest)
            that determine the severity of the correlation
            (anything below cut2 is ignored)

         return error code (0=success) and 'warnings' string"""

      if self.verb > 3: print("-- make_cormat_warn_str for '%s', cut=%g" \
                              % (name, cutoff))

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

   def list_cormat_warnings(self, cutoff=0.4, skip_expected=1):
      """return an error code, error string and a list of corval, cosval,
         vec, index for each cormat value with abs() > cutoff

         if skip_expected, skip: mot or base against mot or base
      """

      if self.verb > 3: print('-- Afni1D list_cormat_warnings, cut=%g'%cutoff)

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
         print('-- LCP: len(base, mot, roi) = (%d, %d, %d), cut = %.2f' % \
              (len(basecols), len(motcols), len(roicols), cutoff))

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

         # also, skip baseline against baseline (from 2-3 run polort 0)
         # 30 Dec 2013
         if skip_expected and (cmot or cbase) and (rbase or rmot): continue

         if aval < cutoff: break

         badlist.append((val, s, r, c))       # so keep this one

      if self.verb > 1:
         print('-- LCP: badlist length = %d' % len(badlist))

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

         --> set nruns, run_len and runstart

         return 0 on success, 1 on error
      """

      if self.verb > 3:
         print('-- LAD:set_nruns (nruns = %d, run_lens = %s)' % (nruns,run_lens))
         print('       len(goodlist) = %d, runstart = %s' \
               % (len(self.goodlist), self.runstart))

      # try to apply any passed nruns, first
      if nruns > 0:
         rlen = self.nt // nruns
         if rlen * nruns == self.nt:
             self.nruns = nruns
             self.run_len = [rlen for run in range(nruns)]

             # update non-censored version
             if self.run_len_nc[0] == 0:
                self.run_len_nc = [0 for ll in self.run_len]
             elif self.run_len_nc[0] < self.nt:
                print('** cannot reset nruns in face of censored TRs')
                return 1
             else:
                self.run_len_nc = self.run_len[:]

             if self.verb > 1: print('++ set_nruns: nruns = %d' % nruns)
         else:
             print('** nvalid nruns = %d (does not divide nt = %d)'  \
                   % (nruns, self.nt))
             return 1
         # and set runstart
         self.runstart = [r*rlen for r in range(nruns)]
         if self.verb > 1:
            print('++ set_nruns 0: nruns = %d, run_len = %s, runstart = %s' \
                  % (nruns, self.run_len, self.runstart))
         return 0

      # next, try run_lens (if not set, try runstart data from RunStart label)
      if type(run_lens) != type([]):
         print("** set_runs: run_lens is not a list type: %s" % run_lens)
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
               print('++ setting run_len based on RunStart list:' , rlens)

      rlens = run_lens

      if len(rlens) > 0:
         if not UTIL.vals_are_positive(rlens):
            print("** set_runs: non-positive run length in list: %s" % rlens)
            return 1
         if UTIL.loc_sum(rlens) != self.nt:
            print("** set_runs: sum of run lengths != nt (%d): %s" \
                  % (self.nt, rlens))
            return 1
         self.run_len = rlens[:]     # make a copy, to be safe
         self.nruns = len(rlens)
         rsum = 0
         self.runstart = []
         for ll in rlens:
            self.runstart.append(rsum)
            rsum += ll
         if self.verb > 1:
            print('++ set_nruns 1: nruns = %d, run_len = %s, runstart = %s' \
                  % (nruns, rlens, self.runstart))
         return 0

      # otherwise, init to 1 run and look for other labels
      self.nruns = 1
      if not self.groups or not self.nvec: return 0

      # rcr : update poor use of baselines?
      errs = 0
      try:
         base_ind = self.groups.index(-1)
         if base_ind < 0:
            if self.verb > 1: print('-- no baseline group to set runs with')
            return 1

         b0 = self.mat[base_ind]

         # skip 0s, count 1s, rest must be 0s
         for val in b0:
            if val != 0 and val != 1:
               if self.verb > 1: print('-- baseline vals not just 0,1: %s' % val)
               return 1

         # looks good, find run length and the number of runs
         first = b0.index(1)              # find first 1
         try:    next = b0.index(0,first+1)  # find next 0
         except: next = self.nt
         if next <= first:
            if self.verb > 1: print('-- odd run_len check...')
            return 1

         # we have a run length
         rlen = next - first
         if rlen > 0: nruns = self.nt//rlen          # integral division
         else:
            if self.verb > 1:
               print('** failed to set rlen from baseline (%d,%d)'%(first,next))
            return 1
         if rlen*nruns != self.nt:
            if self.verb>1:
               print('** nruns failure: rlen %d, nruns %d, len %d' % \
                     (rlen, nruns, len(b0)))
               if self.nrowfull > self.nt:
                  print('++ nrowfull (%d) > nt (%d) ==> censored TRs' \
                        % (self.nrowfull, self.nt))
            return 1

         # success!

         self.run_len = [rlen for i in range(nruns)]
         self.nruns   = nruns
         
      except:
         if self.verb > 1: print('** unknown exception in LD:set_nruns')
         errs = 1

      return errs

   def apply_goodlist(self, parent=None, padbad=0):
      """try to apply goodlist, runstart, nt and nrowfull
            -> set nruns and run_lens[]
            -> also, set run_lens_nc[] (not censored)

         if padbad is set, pad the data with zero over all TRs NOT
            in goodlist (so run_lens should come from RunStart)

         if parent is set (as an Afni1D instance),
            use it for goodlist, runstart, nrowfull

         return 0 on success"""

      if isinstance(parent, Afni1D):
         if self.verb > 2: print('-- apply_goodlist: run info from parent')

         # allow for processing simple 1D
         if not parent.havelabs:
            return self.uncensor_from_vector(parent.mat[0])

         runstart = parent.runstart
         goodlist = parent.goodlist
         nrowfull = parent.nrowfull
      else:
         if self.verb > 2: print('-- apply_goodlist: run info from self')
         runstart = self.runstart
         goodlist = self.goodlist
         nrowfull = self.nrowfull

      # first see if we have the necessary fields
      rv = 1
      try:
         if len(runstart) > 0 and len(goodlist) > 0 and \
               self.nt > 0 and nrowfull > 0: rv = 0
      except:
         if self.verb > 1: print('** bad internals for apply_goodlist')

      if rv == 1: return 1     # bail

      # other tests
      if not UTIL.vals_are_sorted(goodlist):
         if self.verb > 1: print('** LAD: goodlist not sorted')
         return 1
      if len(goodlist) != self.nt:
         if self.verb > 1: print('** LAD: goodlist length (%d) != nt (%d)' \
                                 % (len(goodlist), self.nt))
         return 1
      if not UTIL.vals_are_sorted(runstart):
         if self.verb > 1: print('** LAD: runstart not sorted')
         return 1
      if goodlist[-1] >= nrowfull:
         if self.verb > 1: print('** LAD: max goodlist value exceeds rowfull')
         return 1
      if goodlist[0] < 0:
         if self.verb > 1: print('** LAD: goodlist has negative value')
         return 1
      if runstart[-1] >= nrowfull:
         if self.verb > 1: print('** LAD: max runstart value exceeds rowfull')
         return 1
      if runstart[0] < 0:
         if self.verb > 1: print('** LAD: runstart has negative value')
         return 1

      # ---- now try to sort things out ----

      # if padding 'bad' TRs run_len is basic, but mat needs to be zero-filled
      if padbad:
         if self.GLapplied == 2:
            if self.verb > 0: print('** padbad: run already padded!')
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
      if self.verb > 3: print('-- set rnext list to %s' % rnext)
      nruns = len(runstart)

      # accumulate run counts over goodlist
      run = 0
      rcount = [0 for r in runstart]  # count entries per run
      for gval in goodlist:
         # maybe adjust the run index (note that rnext[-1] > goodlist vals)
         while gval >= rnext[run]: run += 1
         rcount[run] += 1

      # and verify that we have accounted for everything
      gtot = UTIL.loc_sum(rcount)
      glen = len(goodlist)
      if gtot != glen:
         print('** apply_goodlist: gtot error: %d != %d' % (gtot, glen))
         return 1
      if gtot != self.nt:
         print('** apply_goodlist: nt (%d) != goodtot (%d)' % (self.nt, gtot))
         return 1

      # also, generate an uncensored run length list from runstart
      self.run_len_nc = [runstart[i+1]-runstart[i] for i in range(nruns-1)]
      self.run_len_nc.append(self.nrowfull-self.runstart[-1])

      if self.verb > 1:
         print('++ from apply_goodlist: run_len[%d] = %s' % (nruns, rcount))
         print('   run_len_nc = %s' % self.run_len_nc)
         print('   run_start = %s' % self.runstart)

      self.nruns = nruns
      self.run_len = rcount     # steal reference
      self.GLapplied = 1        # goodlist was applied

      return 0

   def show(self):
      print(self.make_show_str())

   def show_rows_cols(self, mesg='', verb=1):
      """display the number of rows (nt) and columns (nvec)

         mesg: if set print before output
         verb: if 0, no text"""

      if mesg:     print('%s' % mesg, end='')
      if verb > 0: print('rows = %d, cols = %d' % (self.nt, self.nvec))
      else:        print('%d %d' % (self.nt, self.nvec))

   def get_tr_counts(self):
      """return status, self.run_len, self.run_len_nc

         check for consistency
         if vector lengths are not nruns and 
      """

      rv = self.check_tr_count_consistency(self.verb)
      return rv, self.run_len, self.run_len_nc

   def check_tr_count_consistency(self, verb=1):
      """check the consistency of TR counts

            - matrix is ready
            - nruns >= 1
            - len(run_len) == len(run_len_nc) == nruns
            - sum(run_len) == nt
            - sun(run_len_nc) == nrowfull

         return 0 if consistent, 1 otherwise
      """

      if not self.ready:
         if verb: print('** bad run lists: not ready')
         return 1

      nr = self.nruns
      if nr < 1:
         if verb: print('** bad run lists: nruns = %d' % nr)
         return 1

      if len(self.run_len) != nr:
         if verb: print('** bad run lists: len(run_len) = %d, nruns = %d' \
                        % (len(self.run_len), self.nruns))
         return 1

      if len(self.run_len_nc) != nr:
         if verb: print('** bad run lists: len(run_len_nc) = %d, nruns = %d' \
                        % (len(self.run_len_nc), self.nruns))
         return 1

      ntr = UTIL.loc_sum(self.run_len)
      if ntr != self.nt:
         if verb: print('** bad run lists: sum(run_len) = %d, nt = %d' \
                        % (ntr, self.nt))
         return 1

      ntr = UTIL.loc_sum(self.run_len_nc)
      if ntr != self.nrowfull:
         if verb: print('** bad run lists: sum(run_len_nc) = %d, nrf = %d' \
                        % (ntr, self.nrowfull))
         return 1

      return 0

   def apply_censor_dset(self, cset=None):
      """remove censored TRs from self.mat

         ** for now, it becomes 1 combined run
      """

      mat = self.mat
      nt = self.nt

      if self.verb > 1: print('-- censoring input data...')

      if cset == None:   return
      if cset.nvec == 0: return
      if self.nvec == 0: return

      if cset.nt != nt:
         print('** ACD: censort length (%d) does not match NT (%d)' \
               % (cset.nt, nt))
         return

      clist = cset.mat[0]       # get censor list for ease of typing

      if self.verb > 2:
         nkeep = UTIL.loc_sum([v for v in clist if v])
         print('-- censoring from length %d to length %d' % (nt, nkeep))

      # if nothing is censored out, we're done
      if UTIL.vals_are_constant(clist, 1): return

      # else, make a new matrix
      cmat = [[col[ind] for ind in range(nt) if clist[ind]] for col in mat]

      del(self.mat)
      self.mat = cmat

      # now clean up some extra fields

      self.nt = len(cmat[0])
      self.nrowfull = self.nt

      # could deal with run lengths here, but save for later
      self.nruns = 1
      self.run_len = [self.nt]
      self.goodlist = []
      self.runstart = []

      del(self.cormat)
      del(self.cosmat)
      self.cormat = []
      self.cosmat = []
      self.cormat_ready = 0

      return

   def get_censored_mat(self, cset=None):
      """return self.mat, restricted to uncensored TRs"""

      mat = self.mat
      nt = self.nt

      if cset == None:   return mat
      if cset.nvec == 0: return mat

      if cset.nt != nt:
         print('** GCM: censort length (%d) does not match NT (%d)' \
               % (cset.nt, nt))
         return None

      clist = cset.mat[0]       # get censor list for ease of typing

      # if nothing is censored out, return the original
      if UTIL.vals_are_constant(clist, 1): return mat

      # else, make a copy
      return [[col[ind] for ind in range(nt) if clist[ind]] for col in mat]

   def get_max_displacement(self, dtype=1, cset=None):
      """compute the maximum pairwise displacement among the
         N-dimensional coordinates (e.g. motion params)

         dtype = 1: enorm

            - so compute max(dist(x,y)) over all x, y coordinate pairs

         dtype = 0: max abs

            compute max absolue pairwise displacement among the N dimensions

            - so compute max(abs(v_i,j - v_i,k)) over all all dimensions i
              and j,k index pairs
            - or compute as max(max(v_i)-min(v_i)) over dimensions i

          cset is an optional censor dataset (1=keep, 0=censor)
      """

      maxdiff = 0

      mat = self.get_censored_mat(cset)
      if mat == None: mat = self.mat

      if self.nvec > 0: nt = len(mat[0])
      else:             nt = 0

      if dtype == 0:    # max vector displacement
         maxdiff = max([max(vec)-min(vec) for vec in mat])

      else:             # max euclidean distance

         # get all distances, a 2D list of the form [[DIST, i, j]]
         # (so length is NT choose 2 = (NT*(NT-1)/2), which is possibly long)

         en = UTIL.euclidean_norm
         dlist = [[en([mat[i][r1]-mat[i][r2] for i in range(self.nvec)]),
                     r1, r2] for r1 in range(nt) for r2 in range(r1)]

         # and grab the biggest
         dlist.sort()
         maxdiff = dlist[-1][0]
         
         if self.verb > 1:
            i = dlist[-1][1]
            j = dlist[-1][2]
            print('++ max edisp %s between indices %d and %d' % (maxdiff, i, j))

         del(dlist)

      return maxdiff

   def get_max_displacement_str(self, mesg='', enorm=2, verb=1):
      """display the maximum displacement based on enorm
            enorm = 1   : absolute displacement over dims only
            enorm = 2   : enorm only
            enorm = 3   : both
      """

      rstr = ''         # return string

      if enorm == 1 or enorm == 3:
         maxabval = self.get_max_displacement(0)
      if enorm:
         maxenorm = self.get_max_displacement(1)

      if verb > 0:
         if enorm == 1:   rstr = 'max_param_diff = %g' % maxabval
         elif enorm == 3: rstr = 'max_param_diff = %g, max_enorm_diff = %g' \
                                 % (maxabval, maxenorm)
         else:            rstr = 'max_enorm_diff = %g' % maxenorm
      else:
         if enorm == 1:   rstr = '%g' % maxabval
         elif enorm == 3: rstr = '%g %g' % (maxabval, maxenorm)
         else:            rstr = '%g' % maxenorm

      if mesg: return "%s %s" % (mesg, rstr)
      else: return rstr

   def show_min_mean_max_stdev(self, col=-1, verb=1):
      """show min, mean, max, stdev for each column (unless col specified)"""

      if verb: print("file %s (len %d)" % (self.fname, self.nt))
      for cind, col in enumerate(self.mat):
         if verb:
            ps = "    col %d: " % cind
            form = "min = %7.4f, mean = %7.4f, max = %7.4f, stdev = %7.4f"
         else:
            ps = ''
            form = "%7.4f %7.4f %7.4f %7.4f"
         print(ps + form % UTIL.min_mean_max_stdev(col))

   def show_trs_to_zero(self, col=-1, verb=1):
      """show number of TRs until constant zero
         (i.e. search from end for first non-zero value)
      """

      if verb: print("file %s (len %d)" % (self.fname, self.nt))
      for cind, col in enumerate(self.mat):
         # set mind = index of last non-zero element, and increment
         mind = -1
         for ind in range(len(col)-1,-1,-1):
            if col[ind] != 0:
               mind = ind
               break
         mind += 1 # becomes a count to zero
         
         if verb: print("    col %d: response length = %d" % (cind, mind))
         else:    print("%d" % mind, end='')
      print('')

   def slice_order_to_times(self, verb=1):
      """given a slice order, resort index list to slice times

         e.g. If TR=2 and the slice order is:  0  2  4  6  8  1  3  5  7  9
                      Then the slices/times ordered by time (as input) are:

           slices: 0    2    4    6    8    1    3    5    7    9
           times:  0.0  0.2  0.4  0.6  0.8  1.0  1.2  1.4  1.6  1.8

        And the slices/times ordered instead by slice index are:

           slices: 0    1    2    3    4    5    6    7    8    9
           times:  0.0  1.0  0.2  1.2  0.4  1.4  0.6  1.6  0.8  1.8

        It is this final list of times that is output.
      """

      # maybe we need to transpose
      trans = 0
      if self.nt == 1 and self.nvec > 1:
         self.transpose()
         trans = 1

      # create a matrix of index/slice time pairs
      data = [[val,0] for val in self.mat[0]]
      for ind, val in enumerate(data):
         val[1] = self.tr * ind * 1.0 / self.nt

      # sort on the indices
      data.sort()

      # and return the times
      self.mat[0] = [val[1] for val in data]

      if trans: self.transpose()

      return 0

   def add_offset(self, offset):
      """add offset value to every value in matrix"""

      if not self.ready:
         print('** add_affset: data not ready')
         return 1

      for row in self.mat:
         for ind in range(len(row)): row[ind] += offset

      return 0

   def rank(self, style='dense', reverse=0, base1=0, verb=1):
      """convert to the min to max rank
         i.e. for each value, apply its ordered index
         e.g. 3.4 -0.3 4.9 2.0   ==>   2 0 3 1

         style :  rank style ('dense' or 'competition')
         reverse: sort largest to smallest
         base1:   apply 1-based ranks

         If there is one row or col, use it.  Otherwise, use column 0.

         return status (0=success)
      """

      if not self.ready:
         print('** rank: data not ready')
         return 1, []

      if self.nvec == 0 or self.nt == 0: return 0       # nothing to do

      # if nt == 1, use first value per vector (apply as transpose)
      if self.nt == 1 and self.nvec > 1:
         data = [self.mat[ind][0] for ind in range(self.nvec)]
         rv, data = UTIL.get_rank(data, style=style, reverse=reverse)
         if rv: return 1
         for ind in range(self.nvec):
            self.mat[ind][0] = data[ind]

      # else nt > 1, process all vectors
      else:
         for ind, row in enumerate(self.mat):
            rv, data = UTIL.get_rank(row, style=style, reverse=reverse)
            if rv: return 1
            self.mat[ind] = data

      if base1: return self.add_offset(1)
      else:     return 0

   def get_indices_str(self, ind_types):
      """return an index list (sub-brick selector form) for the
         following groups:

            1: baseline (group  -1)
            2: motion   (group   0)
            4: interest (group > 0)

         Do not return an empty list.  If the groups do not exist or are
         not found, return '0..$'."""

      default = '0..$'

      if self.verb > 1: print('-- show indices, types = %d, groups = %s' \
                              % (ind_types, self.groups))

      bmask = ind_types & 7
      if not self.ready:           return default
      if bmask == 0 or bmask == 7: return default
      if len(self.groups) < 1:     return default

      ilist = []
      allind = list(range(len(self.groups)))
      if ind_types & 1:
         ilist += [ind for ind in allind if self.groups[ind] == -1]
      if ind_types & 2:
         ilist += [ind for ind in allind if self.groups[ind] == 0]
      if ind_types & 4:
         ilist += [ind for ind in allind if self.groups[ind] > 0]
      ilist.sort()

      elist = UTIL.encode_1D_ints(ilist)
      if elist == '':
         if self.verb > 1: print('-- replacing empty encode list with full')
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
             "++ runstart : %s\n" \
             "++ tr       : %s\n" \
             "++ nrowfull : %d\n" \
             "++ nruns    : %d\n" \
             "++ run_len  : %s\n" \
             "++ run_len_nc: %s\n" % \
             (self.name, rstr, self.fname, self.nvec, self.nt,
             self.labels, self.groups, self.goodlist, self.runstart,
             self.tr, self.nrowfull,
             self.nruns, self.run_len, self.run_len_nc)

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
             print("** CBGL: cannot pass positive groups with 'allroi'")
             return []
         groups.extend([g for g in self.groups if g > 0])
      if len(groups) < 1 or len(self.groups) < 1: return []
      return [val for val in range(self.nvec) if self.groups[val] in groups]
      
   def ordered_cols_by_group_list(self, groups):
      """return a list of columns, given a list of groups

         for each group, insert all columns from that group
         (i.e. keep group order)
      """
      if not self.groups: return []

      if len(groups) < 1 or len(self.groups) < 1: return []

      clist = []
      for g in groups:
         clist.extend([v for v in range(self.nvec) if self.groups[v] == g])

      return clist
      
   def cols_by_label_list(self, labels):
      """return a list of columns, given a list of labels"""
      if not self.labels or not labels: return []
      if not list2_is_in_list1(self.labels, labels, "labels"): return []
      # make a working function, suggested by I Schwabacher
      return [self.labels.index(lab) for lab in labels]

   def init_from_matrix(self, matrix):
      """initialize Afni1D from a 2D (or 1D) array"""
      if type(matrix) != type([]):
         print('** matrix for init must be [[float]] format')
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
            print('** matrix for init must be [[float]] format')
            return 1

      self.mat   = matrix
      self.nvec  = len(matrix)
      self.nt    = len(matrix[0])
      self.ready = 1

      return 0

   def init_from_general_name(self, name):
      """might contain [] or {} selection
         might be '-' or stdin"""

      aname = BASE.afni_name(self.fname)

      if self.verb > 3: print("-- Afni1D: init_from_gen name '%s'" % name)

      if self.init_from_1D(aname.rpve()): return 1 # failure

      self.aname = aname
      self.fname = aname.rpve()
      self.name  = aname.pve()

      # apply column and/or row selectors
      if aname.colsel:
         ilist = UTIL.decode_1D_ints(aname.colsel, verb=self.verb,
                                     imax=self.nvec-1)
         if ilist == None: return 1
         if self.reduce_by_vec_list(ilist): return 1
      if aname.rowsel:
         ilist = UTIL.decode_1D_ints(aname.rowsel,verb=self.verb,imax=self.nt-1)
         if ilist == None: return 1
         if self.reduce_by_tlist(ilist): return 1

      return 0

   def init_from_1D(self, fname):
      """initialize Afni1D from a 1D file (return err code)"""

      if self.verb > 3: print("-- Afni1D: init_from_1D '%s'" % fname)

      tmat, clines = TD.read_data_file(fname, verb=self.verb)
      if not TD.data_is_rect(tmat):
         print("** data is not rectangular in %s" % fname)
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
         if not label:
            # store all unprocessed comment lines   16 Apr 2018
            self.header.append(line)
            continue

         verb_level = 3     # cutoff for verbose level

         self.havelabs = 1

         try:      # to process some of the data
            if label == 'ni_type':
               if data.find('*') >= 0:
                  ncols, dtype = data.split('*')
               else:
                  ncols = 1
                  dtype = data
               ncols = int(ncols)
               if self.verb > verb_level:
                  print("-- label %s: cols = %d, dtype = %s" % \
                       (label, ncols, dtype))
               if ncols != self.nvec:
                  print("** matrix vecs %d != %s cols %d" % \
                       (self.nvec, label, ncols))
            elif label == 'ni_dimen':
               nrows = int(data.split(',')[0])     # 3D datasets have commas
               if self.verb > verb_level:
                  print("-- label %s: rows = %d" % (label, nrows))
               if nrows != self.nt:
                  print("** matrix nt %d != %s rows %d" % \
                       (self.nt, label, nrows))
            elif label == 'ColumnLabels':
               self.labels = [str.strip() for str in data.split(';')]
               if self.verb > verb_level:
                  print("-- label %s: labels = %s" % (label, self.labels))
               if self.nvec != len(self.labels):
                  print("** %d ColumnLabels but %d columns" %    \
                       (len(self.labels), self.nvec))
                  self.labels = []
            elif label == 'ColumnGroups':
               self.groups = UTIL.decode_1D_ints(data)
               if self.groups:
                  if len(self.groups) != self.nvec:
                     print("** ColumnGroups len %d != nvec %d" % \
                          (len(self.groups), self.nvec))
               if self.verb > verb_level:
                  print("-- label %s: groups %s" % (label,self.groups))
            elif label == 'CommandLine':
               self.command = data
               if self.verb > verb_level:
                  print("-- label %s: command %s" % (label,self.command))
            elif label == 'RowTR':
               self.tr = float(data)
               if self.verb > verb_level:
                  print("-- label %s: TR %s" % (label,self.tr))
            elif label == 'GoodList':
               self.goodlist = UTIL.decode_1D_ints(data)
               if self.goodlist:
                  if len(self.goodlist) != self.nt:
                     print("** GoodList missing %d rows" % \
                          self.nt-len(self.goodlist))
               if self.verb > verb_level:
                  print("-- label %s: goodlist %s" % (label,self.goodlist))
            elif label == 'NRowFull':
               self.nrowfull = int(data)
               if self.verb > verb_level:
                  print("-- label %s: nrowfull %s" % (label,self.nrowfull))
            elif label == 'RunStart':
               self.runstart = UTIL.decode_1D_ints(data)
               if not UTIL.vals_are_sorted(self.runstart):
                  print("** RunStart values not sorted?  data = %s" % data)
                  self.runstart = []
               if self.verb > verb_level:
                  print("-- label %s: runstart %s" % (label,self.runstart))
            elif self.verb > 2:
               print("** unknown comment label '%s'" % label)
         except:
            print("** failed to process comment label '%s'" % label)

      return 0

# end Afni1D
# ===========================================================================

def c1D_line2labelNdata(cline, verb=1):
   """expect cline to be of the form: '# LABEL = "DATA"'
   returning LABEL, DATA"""
   sline = cline.split()

   # check for problems
   if len(sline) < 4 or sline[0] != "#" or sline[2] != "=":
      if verb > 2: print("-- skipping useless comment line: '%s'" % cline)
      return None, None

   label = sline[1]   # store the line label

   dline = ' '.join(sline[3:]) # put the line back together

   # again, check for problems
   if len(dline) < 2 or dline[0] != '"' or dline[-1] != '"':
      if verb > 1: print("** missing quotes in comment line: '%s'" % cline)
      return None, None

   data = dline[1:-1]

   if verb > 2: print("++ line2labelNdata returns '%s', '%s'" % (label,data))

   return label, data

def list2_is_in_list1(list1, list2, label=''):
   """return 0 or 1, based on whether every element in list2 exists
      in list1"""

   if not list1: return 0
   if not list2: return 1

   for item in list2:
      if not item in list1:
         if label: print("-- list '%s' not subset in Afni1D" % label)
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


class AfniData(object):
   def __init__(self, filename="", mdata=None, fsl_flist=[], verb=1):
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
      self.alist   = None       # list of '*' counts per row

      # descriptive variables, set from data
      self.nrows     = 0
      self.ncols     = 0        # non-zero if rectangular
      self.row_lens  = []       # row lengths, if not rectangular

      self.binary    = 0        # 0/1 file?
      self.empty     = 0        # no data at all
      self.married   = 0        # data has modulators or durations
      self.mtype     = 0        # married type (bits, amp, dur)
      self.minlen    = 0        # min and max row lengths
      self.maxlen    = 0
      self.dur_len   = 0.0

      self.ready     = 0        # data is ready

      # passed in variables
      self.tr        = 0        # non-zero, if it applies
      self.nruns     = 0        # non-zero, if known
      self.run_lens  = []       # run lengths, in seconds or TRs
      self.verb      = verb
      # self.hist    = g_AfniData_hist (why did I do this?)
      self.write_dm  = 1        # if found include durations when printing

      # computed variables
      self.cormat      = None   # correlation mat (normed xtx)
      self.cosmat      = None   # cosine mat (scaled xtx)
      self.cormat_ready = 0     # correlation mat is set

      # initialize...
      if self.fname: 
         if self.init_from_filename(self.fname): return None
      elif mdata:
         if self.init_from_mdata(mdata): return None
      elif len(fsl_flist) > 0:
         if self.init_from_fsl_flist(fsl_flist): return None

   # some accessor functions to match Afni1D
   def set_nruns(nruns):
      self.nruns = nruns

   def set_run_lengths(run_lengths):
      self.row_lens = run_lengths

   def is_rect(self):
      return TD.data_is_rect(self.mdata)

   def is_square(self):
      if not self.is_rect(): return 0

      rlen = len(self.mat)

      if rlen == 0: return 1
      if rlen == len(self.mat[0]): return 1
      
      return 0

   def get_duration(self):
      """return 0       : if empty
                dur     : if constant
                -1      : otherwise (existing, but not constant)
      """

      if self.mtype == 0 or len(self.mdata) == 0: return 0

      # have actual married data

      dur = -2
      for row in self.mdata:
         for entry in row:
            if dur == -2: dur = entry[2]
            elif dur != entry[2]:
               dur = -1
               break
         if dur == -1: break
      if dur == -2: return 0    # uninitialized means 0
      return dur                # -1 or good value

   def get_min_max_duration(self):
      """return -1, -1  : if empty
                min, max: otherwise
      """

      if self.mtype == 0 or len(self.mdata) == 0: return 0

      # have actual married data

      dmin = -1
      dmax = -1
      for row in self.mdata:
         for entry in row:
            dur = entry[2]
            if dmax == -1: dmin = dur # init, but not to a small thing
            if dur < dmin: dmin = dur
            if dur > dmax: dmax = dur
      return dmin, dmax

   def extend_data_rows(self, newdata, promote_mtype=1, promote_rows=0):
      """extend each row by the corresponding row of newdata
         if promote_mtypes and they differ, promote so they are the same
         if promote_rows and they differ, promote so they are the same
      """
      
      if not self.ready or not newdata.ready:
         print('** timing elements not ready for extending rows (%d,%d)' % \
               (self.ready, newdata.ready))
         return 1

      if self.nrows != newdata.nrows:
         print('** timing nrows differ for extending (%d, %d)' % \
               (self.nrows,newdata.nrows))
         if not promote_rows: return 1
         print('   promoting nrows between %s and %s'%(self.name, newdata.name))
         self.promote_nrows(newdata)

      if self.mtype != newdata.mtype:
         print('** timing elements differ in married type (%s, %s)' \
               % (self.married_type_string(self.mtype),
                  self.married_type_string(newdata.mtype)))
         if not promote_mtype: return 1
         print('   promoting mtypes between %s and %s'%(self.name, newdata.name))
         self.promote_mtypes(newdata)

      if self.verb > 1: print('++ MTiming: extending %d rows' % self.nrows)

      for ind in range(self.nrows):
         if self.verb > 3:
            print('++ edr m #%d: extending %d rows from %d cols by %d' \
               % (ind, self.nrows, len(self.data[ind]), len(newdata.data[ind])))
         self.data[ind].extend(newdata.data[ind])

      for ind in range(self.nrows):
         if self.verb > 3:
            print('++ EDR M #%d: extending %d rows from %d cols by %d' \
               % (ind, self.nrows, len(self.mdata[ind]),len(newdata.mdata[ind])))
         self.mdata[ind].extend(newdata.mdata[ind])

      # and update ncols
      self.ncols = min([len(r) for r in self.data])

      if self.verb > 3:
         print(self.make_data_string(nplaces=1, flag_empty=0, check_simple=0,
                        mesg='\nnew mdata for %s'%self.name))
         print('-- min, max dur = %s %s' % self.get_min_max_duration())

      return 0

   def apply_end_times_as_durs(self, newdata):
      """treat newdata as ending times for the events in self
         - sort both to be sure
         - whine on negatives
         - result will include MTYPE_DUR
      """
      
      if not self.ready or not newdata.ready:
         print('** timing elements not ready for end_times (%d,%d)' % \
               (self.ready, newdata.ready))
         return 1

      if self.nrows != newdata.nrows:
         print('** timing nrows differ for end_times (%d, %d)' % \
               (self.nrows,newdata.nrows))

      # check that each row is the same length
      okay = 1
      for rind in range(self.nrows):
         l0 = len(self.data[rind])
         l1 = len(newdata.data[rind])
         if l0 != l1:
            print('** num events differs for run %d' % (rind+1))
            okay = 0
      if not okay:
         return 1

      if self.verb > 1: print('++ MTiming: applying end_times as durs')

      # sort both to be sure
      self.sort()
      newdata.sort()

      # apply newdata as end times, whine if negative
      for rind in range(self.nrows):
         for eind in range(len(self.data[rind])):
             dur = newdata.mdata[rind][eind][0] - self.mdata[rind][eind][0]
             if dur < 0 and self.verb:
                print('** end_times as durs: have negative duration %f' % dur)
             self.mdata[rind][eind][2] = dur

      # set mtype to include MTYPE_DUR (might already)
      self.mtype |= MTYPE_DUR

      return 0

   def promote_nrows(self, newdata, dataval=None, maryval=None):
      """if differing nrows, extend to match

         if dataval == None: new rows are empty
         else:               pad by ncols vector of dataval elements
      """

      diff = self.nrows - newdata.nrows

      if diff == 0: return 0

      if dataval == None: df = []
      else:               df = [dataval for i in range(self.ncols)]
      if maryval == None: mf = []
      else:               mf = [maryval for i in range(self.ncols)]

      if diff < 0: updater = self
      else       : updater = newdata

      for r in range(abs(diff)):
         updater.data.append(df)
         updater.mdata.append(mf)

      return 0

   def promote_mtypes(self, newdata):
      """if married types differ, promote data to match"""

      # if they are the same, there is nothing to do
      if self.mtype == newdata.mtype: return 0

      # else they will certainly be married after this,
      # so set both to married, and mtype to the combined one
      self.married = 1
      newdata.married = 1
      new_mtype = self.mtype | newdata.mtype
      self.mtype = new_mtype
      newdata.mtype = new_mtype

      if self.verb > 2:
         print('++ promoting married type to %d (%s from %s)' \
               % (new_mtype, self.name, newdata.name))

      # try to find some data
      sval = []
      for v in self.mdata:
         if len(v) > 0:
            sval = v[0]
            break
      nval = []
      for v in newdata.mdata:
         if len(v) > 0:
            nval = v[0]
            break

      # if either has no data, we are done
      if len(sval) == 0 or len(nval) == 0: return 0

      # so we have data, see who has more amplitude modulators

      adiff = len(sval[1]) - len(nval[1])
      if adiff < 0:   self.pad_amp_mods(-adiff)
      elif adiff > 0: newdata.pad_amp_mods(adiff)
      if adiff != 0: # promote mtypes
         self.mtype = (self.mtype | MTYPE_AMP)
         newdata.mtype = (newdata.mtype | MTYPE_AMP)

      # nothing to do for MTYPE_DUR, except promote mtype, done above

      return 0

   def pad_amp_mods(self, ext_len):
      """extend the amplitudes by length ext_len list of 1"""
      if ext_len <= 0: return
      if self.verb > 2:
         print('++ pad_amp_mods(%d) for %s' % (ext_len, self.name))
      elist = [1] * ext_len
      for row in self.mdata:
         for val in row:
            val[1].extend(elist)

   def show_married_info(self):
      print('-- modulation type: %s' % self.married_info_string())

   def married_info_string(self):
      if   self.mtype == MTYPE_NONE: return 'not a married format'

      namp = self.num_amplitudes()
      adur = self.ave_dur_modulation()

      if self.mtype == MTYPE_AMP:
         return 'amplitude modulation (%d modulators)' % namp

      if self.mtype == MTYPE_DUR:
         return 'duration modulation (average duration %g)' % adur

      if self.mtype == MTYPE_AMP|MTYPE_DUR:
         return 'amp and dur modulation (%d amp mods, ave dur %g)'%(namp, adur)

      return '** invalid modulation type %d' % self.mtype

   def num_amplitudes(self):
      if not self.mtype & MTYPE_AMP: return 0
      if not self.mdata: return 0
      for row in self.mdata:
         if len(row) == 0: continue
         return len(row[0][1])  # have amplitudes, return the length
      return 0 # no valid rows found

   def ave_dur_modulation(self):
      if not self.mtype & MTYPE_DUR: return 0
      if not self.mdata: return 0
      lsum, count = 0.0, 0
      for row in self.mdata:
         if len(row) == 0: continue
         count += len(row)
         lsum += UTIL.loc_sum([entry[2] for entry in row])
      if count == 0: return 0
      return lsum*1.0/count # be wary of integers

   def married_type_string(self, mtype=None):
      if mtype == None: mtype = self.mtype

      if   mtype == MTYPE_NONE:            return 'None'
      elif mtype == MTYPE_AMP:             return 'Amp Mod'
      elif mtype == MTYPE_DUR:             return 'Dur Mod'
      elif mtype == MTYPE_AMP | MTYPE_DUR: return 'Amp/Dur Mod'
      else:                                return 'Unknown'

   def randomize_trs(self, seed=0):
      """reorder the matrix rows randomly"""
      if self.verb > 1: print('-- randomizing %d trs (seed=%d)' \
                              % (len(self.data, seed)))
      if seed:
         import random
         random.seed(seed)
      UTIL.shuffle(self.data)

   def sort(self, rev=0):
      """sort each row (optionally reverse order)"""
      if not self.ready: return 1

      if self.verb > 1: print('-- sorting AfniData ...')

      for row in self.data:
         if rev: row.sort(reverse=True)
         else:   row.sort()

      for row in self.mdata:
         if rev: row.sort(reverse=True)
         else:   row.sort()

      return 0

   def transpose(self):
      """the tranpose operation requires rectangular data"""
      if not self.ready: return 1

      # allow transpose if max row length is 1
      if self.maxlen > 1 and not self.is_rect():
         print('** cannot take transpose, data is not rectangular')
         return 1

      if self.verb > 1: print('-- AData: taking transpose...')

      if self.empty: return 0

      # get mdata and data

      # if column (maybe non-rectagular), allow transpose
      # (akin to when 3dDeconvolve might mis-interpret local times as global)
      if self.maxlen == 1 and not self.is_rect():
         newdata = []
         for row in self.data: newdata.extend(row)
         self.data = [newdata]
         newdata = []
         for row in self.mdata: newdata.extend(row)
         self.mdata = [newdata]
      else: # typical rectagular case
         newdata = []
         for col in range(len(self.data[0])):
            newdata.append([self.data[row][col] for row in range(self.nrows)])
         del(self.data)
         self.data = newdata

         newdata = []
         for col in range(len(self.mdata[0])):
            newdata.append([self.mdata[row][col] for row in range(self.nrows)])
         del(self.mdata)
         self.mdata = newdata

      self.nrows = len(self.mdata)
      self.ncols = len(self.mdata[0])

      return 0

   def copy(self):
      """return a complete (deep)copy of the current AfniTiming"""
      return copy.deepcopy(self)

   def is_empty(self):
      """empty is either nrows == 0 or each row is empty"""
      if not self.ready: return 1       # seems safer

      if self.nrows < 1: return 1

      # now check each row
      for row in self.mdata:
         if len(row) > 0: return 0      # found something

      return 1

   def make_single_row_string(self, row=-1, nplaces=3, mplaces=-1,
                              flag_empty=0, simple=1):
      """return a string of row data, to the given number of decimal places
         if row is non-negative, return a string for the given row

         if not simple, show_durs controls display of durations
      """
      if not self.ready: return ''
      if row < 0 or row >= self.nrows:
         if self.verb > 0: print('** row %d out of range for printing' % row)
         return ''

      data = self.mdata[row]
      rstr = ''
      if self.verb > 2 and not flag_empty: rstr += 'run %02d : ' % (row+1)

      # if flagging an empty run, use '*' characters
      # (little gain in trying to usually put one '*')
      if len(data) == 0 and flag_empty: rstr += '* *'

      for val in data:
         if simple:
            if nplaces >= 0: rstr += '%.*f ' % (nplaces, val[0])
            else:            rstr += '%g ' % (val[0])
         else:
            if self.mtype & MTYPE_AMP and len(val[1]) > 0:
               if mplaces >= 0: alist = ['*%.*f'%(nplaces, a) for a in val[1]]
               else:            alist = ['*%g'%a for a in val[1]]
               astr = ''.join(alist)
            else: astr = ''

            # if married and want durations, include them
            if self.write_dm and (self.mtype & MTYPE_DUR):
               if mplaces >= 0: dstr = ':%.*f' % (nplaces, val[2])
               else:            dstr = ':%g' % val[2]
            else: dstr = ''

            if nplaces >= 0: rstr += '%.*f%s%s ' % (nplaces, val[0], astr, dstr)
            else: rstr += '%g%s%s ' % (val[0], astr, dstr)

      return rstr + '\n'

   def make_data_string(self, row=-1, nplaces=3, mplaces=-1,
                        flag_empty=0, check_simple=1, mesg=''):
      """return a string of row data, to the given number of decimal places
         if row is non-negative, return a string for the given row, else
         return a string of all rows
            row          : make a string for just the single row
            nplaces      : number of decimal places to show
            mplaces      : number of decimal places to show in married fields
            flag_empty   : if empty row, use the '*' format
            check_simple : if set and dur_len=0, use timing format
            mesg         : display the message before data
      """

      if check_simple and self.dur_len == 0.0: simple = 1
      elif self.mtype == 0:                    simple = 1
      else:                                    simple = 0

      # init return string based on message
      if len(mesg) > 0: rstr = "%s :\n" % mesg
      else:             rstr = ''

      if self.verb > 2:
         print('-- writing %s, simple=%d, wdm=%d, const=%d, dur_len=%d' \
               % (self.name, simple, self.write_dm, self.durs_are_constant(),
                  self.dur_len))

      if row >=0:
         return rstr+self.make_single_row_string(row, nplaces=nplaces,
                    mplaces=mplaces, flag_empty=flag_empty, simple=simple)

      # make it for all rows
      for ind in range(self.nrows):
         rstr += self.make_single_row_string(ind, nplaces=nplaces,
                    mplaces=mplaces, flag_empty=flag_empty, simple=simple)

      return rstr

   def durs_are_constant(self):
      """just check mdata"""
      if self.mdata == None: return 1
      dall = []
      for mrow in self.mdata:
         dall.extend([e[2] for e in mrow])
      return UTIL.vals_are_constant(dall)

   def check_constant_duration(self):
      """return whether constant, and duration if so
         (akin to durs_are_constant(), but also return that duration)
         return -1 for duration, if unknown
      """
      cdur = -1
      if self.mdata == None: return 1, cdur
      dall = []
      for mrow in self.mdata:
         dall.extend([e[2] for e in mrow])

      if UTIL.vals_are_constant(dall):
         if len(dall) > 0: cdur = dall[0]
         rv = 1
      else:
         rv = 0

      return rv, cdur

   def write_as_timing(self, fname='', nplaces=-1, mplaces=-1, check_simple=1):
      """write the current M timing out, with nplaces right of the decimal"""
      if not self.ready:
         print('** M Timing: not ready to write')
         return 1

      if fname == '': fname = self.fname

      if fname == '-' or fname == 'stdout':
         fp = sys.stdout
      else:
         fp = open(fname, 'w')
         if not fp:
            print("** failed to open '%s' for writing Mtiming" % fname)
            return 1

      if self.verb > 1:
         print("++ writing %d MTiming rows to %s" % (self.nrows, fname))

      fp.write(self.make_data_string(nplaces=nplaces, mplaces=mplaces,
                                     flag_empty=1, check_simple=check_simple))

      if fp != sys.stdout:
         fp.close()

      return 0

   def looks_like_1D(self, run_lens=[], nstim=0, verb=1):
      """check if the file looks like 1D
         - if so and verb, show warnings, too"""

      # negative result is terminal, positive can continue with warnings
      errs = self.file_type_errors_1D(run_lens=run_lens, nstim=nstim, verb=verb)
      if errs < 0:
         if verb > 0: print('== BAD: %s does not look like 1D' % self.fname)
         return 0
      else:
         if verb > 0:
            self.file_type_warnings_1D(run_lens=run_lens, nstim=nstim)
            print('== GOOD: %s looks like 1D' % self.fname)
         if errs > 0: return 0
         return 1


   def file_type_errors_1D(self, run_lens=[], nstim=0, verb=1):
      """return whether errors exist for 1D (stim_file) format
                - data should be rectangular
                  (what else can we even check?)
         if run_lens is passed,
                - nrows should be at least sum(run_lens)
         if nstim is passed,
                - number of columns should equal nstim

         return -1 on fatal error
                 0 on OK
                 1 on non-fatal error
      """

      if not self.ready:
         print('** looks_like_1D: data not ready')
         return -1

      if self.empty:
         if verb > 1: print("-- empty file %s okay as 1D file" % self.fname)
         return 0

      # error occur only as singles, to test against verb > 1

      errors = 0
      if not self.is_rect():
         errors |= ERR_ANY_MISC
         if verb > 1: print("** file %s is not rectangular" % self.fname)

      # keep same local variable
      rlens = run_lens

      nruns = len(rlens)
      if nruns > 0:
         # if TR, scale it in
         tot_dur = UTIL.loc_sum(rlens)

         # if nrows is too small, error -- if too big, just warn
         if tot_dur > self.nrows:
            errors |= ERR_ANY_MISC
            if verb > 1:
               print("** file %s: nrows too small for run dur: %d < %d" \
                               % (self.fname, self.nrows, tot_dur))

      if nstim > 0 and nstim != self.ncols:
         errors |= ERR_ANY_MISC
         if verb > 1: print("** file %s: ncols %d != nstim %d" \
                            % (self.fname, self.ncols, nstim))

      if errors: return -1
      else:      return  0

   def file_type_warnings_1D(self, run_lens=[], nstim=0):
      """akin to ft_errors_1D, but just show any warnings
         if run_lens is passed,
                - warn if too many
         if nstim is passed,
                - number of columns should equal nstim
         return whether warnings are shown
      """

      if self.file_type_errors_1D(run_lens=run_lens, nstim=nstim, verb=0) < 0:
         print('** file %s is not valid as a 1D format' % self.name)
         return 1

      if self.empty:
         print("-- empty file %s okay as 1D file" % self.fname)
         return 0

      # keep same local variable
      rlens = run_lens

      nruns = len(rlens)
      if nruns > 0:
         # if TR, scale it in
         tot_dur = UTIL.loc_sum(rlens)

         if tot_dur < self.nrows:
            print("** warning for 1D file %s, more rows than TRs: %d > %d" \
                  % (self.fname, self.nrows, tot_dur))
            return 1

      return 0

   def looks_like_local_times(self, run_lens=[], tr=0.0, verb=1):
      """check if the file looks like local times
         - if so and verb, show warnings, too"""

      # negative result is terminal, positive can continue with warnings
      errs = self.file_type_errors_local(run_lens=run_lens, tr=tr, verb=verb)
      if errs < 0:
         if verb > 0:
            print('== BAD: %s does not look like local stim_times' % self.fname)
         return 0
      else:
         if verb > 0:
            self.file_type_warnings_local(run_lens=run_lens, tr=tr)
            print('== GOOD: %s looks like local stim_times' % self.fname)
         if errs > 0: return 0
         return 1

   def file_type_errors_local(self, run_lens=[], tr=0.0, verb=1):
      """return whether data has errors for local stim_times format
                - times should be non-negative
                - times should be unique per run
         if run_lens is passed, 
                - number of runs should match nrows
                - maximum should be less than current run_length
         if tr is passed, scale the run lengths

         return -1 on fatal erros
                 0 on OK
                 1 on non-fatal errors
      """

      # possibly scale run_lengths
      if tr > 0.0: rlens = [tr*rl for rl in run_lens]
      else:        rlens = run_lens

      nruns = len(rlens)

      if not self.ready:
         print('** looks_like_local_times: data not ready')
         return -1

      if self.empty:
         if verb > 1: print("-- empty file %s okay as local_times" % self.fname)
         return 0

      # in case we know nothing, just check that values are non-negative
      # and unique
      # - if we have run lengths, check the number of runs and maximums, too
      errors = 0

      # allow bad nruns (but warn)

      for rind in range(len(self.data)):
         # start with row copy
         row = self.data[rind][:]
         if len(row) == 0: continue
         row.sort()
         first = row[0]
         last = row[-1]
         # allow negatives (but warn)
         if not UTIL.vals_are_increasing(row):
            errors |= ERR_ST_NON_UNIQUE
            if verb > 1: print("** file %s, row %d has repetitive times" \
                               % (self.fname, rind))
         # allow times after end of run (but warn)

         del(row)

      # no fatal errors yet
      if errors: return 1
      else:      return 0

   def looks_local_but_3dD_global(self, warn=0, maxruns=20):
      """return 1 if the timing looks local (has '*' anywhere),
         but would be read as global by 3dDeconvolve (max cols == 1)

         true if '*' entries exist, and:
            - max entries (per row) is 1 (including '*')
            - there is some time after row 1

         true if '*' entries do not exist, and:
            - there is exactly one time per row
            - either of:
               - len(run_lens) == nrows
               - there are <= maxruns rows

         if verb: warn user
      """

      if not self.ready:
         print('** LLB3G: data not ready')
         return 0

      if self.empty: return 0

      # has_alist will imply it exists, is valid and there are '*' entries
      has_alist = (self.alist != None)
      if has_alist: has_alist = (len(self.alist) == len(self.data))
      if has_alist: has_alist = (UTIL.loc_sum(self.alist) > 0)

      minntimes = 2 # at which point we would not care
      maxent = 0
      late_times = 0

      # track max entries per row, including '*' chars
      for rind, row in enumerate(self.data):
         nent = len(row)
         if rind > 0 and nent > 0: late_times = 1 # stim exist after row 0
         if nent < minntimes: minntimes = nent    # min stim times per row
         if has_alist: nent += self.alist[rind]   # total events per row
         if nent > maxent: maxent = nent          # max events across rows

         # save a little time if we are safe
         if maxent > 1: return 0

      # if maxent is not 1, there is no problem
      if maxent != 1: return 0

      # if there is no event after row 1, there is no problem
      if not late_times: return 0

      # we have at most 1 entry per row, and times after row 1,
      # get to the stated tests

      if has_alist:
         if warn:
            print("** timing file %s looks like local times from '*', but\n" \
                  "   might be interpreted as global times by 3dDeconvovle\n"\
                  "   because it has only one column\n"                      \
                  "   (consider adding one '*', giving that row 2 entries)\n"\
                  % self.fname)
         return 1

      if len(self.run_lens) > 0:
         if len(self.run_lens) == self.nrows:
            if warn:
               print("** timing file %s looks like global times, but\n" \
                     "   Nruns == Nstim, so maybe it is local\n"        \
                     "   (if local, add '*' to some row)\n"             \
                     % self.fname)
            return 1
      elif self.nrows > 0 and self.nrows < maxruns:
         if warn:
            print("** timing file %s looks like global times, but\n" \
                  "   has very few stim, so might be local\n"        \
                  "   (if local, add '*' to some row)\n"             \
                        % self.fname)
         return 1

      return 0

   def file_type_warnings_local(self, run_lens=[], tr=0.0):
      """warn about any oddities in local timing
                - times should be non-negative
                - times should be unique per run
         if run_lens is passed, 
                - number of runs should match nrows
                - maximum should be less than current run_length
         if tr is passed, scale the run lengths
         return whether any warnings are printed
      """

      # possibly scale run_lengths
      if tr > 0.0: rlens = [tr*rl for rl in run_lens]
      else:        rlens = run_lens

      nruns = len(rlens)

      if not self.ready:
         print('** looks_like_local_times_warn: data not ready')
         return 1

      if self.file_type_errors_local(run_lens=run_lens,tr=tr,verb=0) < 0:
         print('** file %s is not valid as local times format' % self.name)
         return 1

      if self.empty:
         print("-- empty file %s okay as local_times" % self.fname)
         return 0

      # make a list of warnings to print at the end
      warnings = []

      # - if we have run lengths, check the number of runs and maximums, too
      if nruns > 0:
         if nruns != self.nrows:
            warnings.append("   - %d rows does not match %d runs" \
                           % (self.nrows, nruns))

      check_alist = (self.alist != None)
      if check_alist: check_alist = len(self.alist) == len(self.data)
      maxent = 0

      wcount = [0,0,0]  # limit warnings of each type (to 2 for now)
      for rind in range(len(self.data)):
         # start with row copy
         row = self.data[rind][:]

         # track max entries per row, including '*' chars
         rlen = len(row)
         nent = rlen
         if check_alist: nent += self.alist[rind]
         if nent > maxent: maxent = nent

         if rlen == 0: continue

         row.sort()
         first = row[0]
         last = row[-1]
         if first < 0:
            wcount[0] += 1
            if wcount[0] <= 2:
               warnings.append("   - row %d has negative time %g"%(rind, first))
         if not UTIL.vals_are_increasing(row):
            wcount[1] += 1
            if wcount[1] <= 2:
               warnings.append("   - row %d has repetitive times" % (rind))
         if nruns > 0 and rind < nruns:
            if last >= rlens[rind]:
               wcount[2] += 1
               if wcount[2] <= 2:
                  warnings.append("   - row %d : time %g exceeds run dur %g" \
                                  % (rind, last, rlens[rind]))
         del(row)

      # if any type exceeded the limit (of 2), print a general count
      if wcount[0] > 2:
         warnings.append("   * %d rows with negative times ..."%wcount[0])
      if wcount[1] > 2:
         warnings.append("   * %d rows with repetitive times ..."%wcount[1])
      if wcount[2] > 2:
         warnings.append("   * %d row times exceed run dur %g ..." \
                         % (wcount[2], rlens[rind]))

      if maxent == 1 and self.nrows > 1:
         awarn = 0
         if check_alist:
            if UTIL.loc_sum(self.alist) > 0: awarn = 1
         if awarn: warnings.append(                             \
            "   * single column looks local from '*',\n"        \
            "     but 3dDeconvolve would interpret as global")
         else: warnings.append(                                 \
            "   * 3dDeconvolve would interpret single column as global")

      if len(warnings) > 0:
         print('** warnings for local stim_times format of file %s' % self.fname)
         for w in warnings: print(w)
         return 1
      else: return 0


   def looks_like_global_times(self, run_lens=[], tr=0.0, verb=1):
      """check if the file looks like global times
         - if so and verb, show warnings, too"""

      # negative result is terminal, positive can continue with warnings
      errs = self.file_type_errors_global(run_lens=run_lens, tr=tr, verb=verb)
      if errs < 0:
         if verb > 0:
            print('== BAD: %s does not look like global stim_times' % self.fname)
         return 0
      else:
         if verb > 0:
            self.file_type_warnings_global(run_lens=run_lens, tr=tr)
            print('== GOOD: %s looks like global stim_times' % self.fname)
         if errs > 0: return 0
         return 1

   def file_type_errors_global(self, run_lens=[], tr=0.0, verb=1):
      """ return -1 on fatal erros
                  0 on OK
                  1 on non-fatal errors
      """

      if not self.ready:
         print('** looks_like_1D: data not ready')
         return -1

      if self.empty:
         if verb > 1: print("-- empty file %s okay as global_times" % self.fname)
         return 0

      errors = 0
      ferrors = 0

      # must be one row or column
      if self.ncols != 1 and self.nrows != 1:
         errors |= ERR_ANY_MISC
         if verb > 1: print("** file %s is not a single column" % self.fname)

      # must be rectangular
      if not self.is_rect():
         errors |= ERR_ANY_MISC
         if verb > 1: print("** file %s is not a rectangular" % self.fname)

      # this is fatal, as opposed to nrows/ncols
      if self.maxlen > 1:
         ferrors |= ERR_ANY_MISC
         if verb: print('** file %s has rows longer than 1' % self.fname)
         

      # negative times are not errors, but warnings

      # possibly scale run_lengths

      # note the total duration (tr == -1 implies just count TRs)

      # late times are not errors, but warnings

      # repetition times are not errors, but warnings

      if  ferrors: return -1
      elif errors: return 1
      else:        return 0

   def file_type_warnings_global(self, run_lens=[], tr=0.0, verb=1):

      if not self.ready:
         print('** looks_like_1D: data not ready')
         return 1

      if self.empty:
         if verb > 1: print("-- empty file %s okay as global_times" % self.fname)
         return 0

      if self.file_type_errors_global(run_lens=run_lens,tr=tr,verb=0) < 0:
         print('** file %s is not valid as global times format' % self.name)
         return 1

      # make a list of warnings to print at the end
      warnings = []

      # get a single sequence of numbers, depending on the direction
      if self.nrows == 1: data = self.data[0]
      else:data = [row[0] for row in self.data if len(row)>0]

      data.sort()

      if data[0] < 0.0:
         warnings.append("   - negative stim time %g" % (data[0]))

      # possibly scale run_lengths
      if tr > 0.0: rlens = [tr*rl for rl in run_lens]
      else:        rlens = run_lens

      # note the total duration (tr == -1 implies just count TRs)
      endoftime = UTIL.loc_sum(rlens)
      if tr > 0.0: endoftime *= tr

      if data[-1] >= endoftime:
         warnings.append("   - time %g after all runs, %g" 
                            % (data[-1], endoftime))

      if not UTIL.vals_are_increasing(data):
            warnings.append("   - has repeated times")

      if len(warnings) > 0:
         print('** warnings for global stim_times format of file %s'%self.fname)
         for w in warnings: print(w)
         return 1
      else: return 0

   def init_from_filename(self, fname):
      """file could be 1D, timing or married timing data
        
         For now, store complete result but focus on times only.
      """

      mdata, clines, alist = TD.read_married_file(fname, verb=self.verb)
      if mdata == None:
         if self.verb > 0: print('** A1D: failed to read data file %s' % fname)
         return 1

      # init name from filename
      aname = BASE.afni_name(self.fname)
      self.name  = aname.pve()

      # note whether the data is married (modulation or duration)
      self.mtype = TD.married_type(mdata)
      if self.mtype: self.married = 1

      # data will ignore any married information
      self.data     = [[val[0] for val in row] for row in mdata]
      self.mdata    = mdata
      self.clines   = clines
      self.fname    = fname
      self.alist    = alist

      self.nrows    = len(self.data)
      self.row_lens = [len(row) for row in self.data]

      # empty data includes existing but empty runs
      if len(self.data) == 0:
         self.maxlen = 0
         self.minlen = 0
      else:
         self.maxlen = max([len(drow) for drow in self.data])
         self.minlen = min([len(drow) for drow in self.data])

      # accept an empty file?
      if self.nrows == 0 or self.maxlen == 0:
         self.empty = 1
         self.ready = 1
         return 0

      # if row lengths are all the same, use ncols, instead
      if UTIL.vals_are_constant(self.row_lens):
         self.ncols = self.row_lens[0]
         del(self.row_lens)
         self.row_lens = []

      # check to see if it is a 0/1 file
      self.binary = 1
      for row in self.data:
         if not UTIL.vals_are_0_1(row):
            self.binary = 0
            break

      self.ready = 1
      
      return 0

   def init_from_fsl_flist(self, flist):
      """Convert FSL flist files into a married data structure,
         and then init from that.

         Files in flist are considered to be in the FSL format:
            time  duration  amplitude
         and are considered to be one run per file.

         --> So basically just swap amp and dur, and put amp in list.

         Output is a array of mdata lists, of the form:
            [
               [ [time [AM list] duration] [] [] ... ]
               [ [time [AM list] duration] [] [] ... ]
            ]

         mdata[0] is the event list for the first run
         mdata[0][0] is the first event for the first run
         mdata[0][0][0] is the first event start time
         mdata[0][0][1] is the array of first event amplitudes
         mdata[0][0][1][0] is the first amplitude of the first event
         mdata[0][0][2] is the first event duration

         FSL files have only single amplitudes
      """

      if len(flist) < 1: return 1

      mdata = []
      amp_all = [] # track all amplitudes

      if self.verb > 1:
         print('-- reading FSL timing for %d runs' % len(flist))

      for find, fname in enumerate(flist):
         try: td = TD.read_1D_file(fname)
         except:
            print("** failed to read FSL timing file %d, '%s'" % (find, fname))
            return 1
         if td == None:
            print("** failed to read FSL timing file %d, '%s'" % (find, fname))
            return 1

         # check for an empty run, either nothing in the file,
         # or the special case of 0, 0, 0
         if len(td) == 0:
            if self.verb > 2:
               print('-- FSL timing file %s has no events' % fname)
            mdata.append([])
            continue
         elif len(td) == 1:
            if UTIL.vals_are_constant(td[0], cval=0):
               if self.verb > 2:
                  print('-- FSL timing file %s shows 1 empty event' % fname)
               mdata.append([])
               continue

         # 1 entry is time, 2 includes duration, 3 includes amplitude
         if len(td[0]) == 0:
            if self.verb > 2:
               print('-- FSL timing file %s missing events' % fname)
            elist = []
         elif len(td[0]) == 1:
            elist = [[ev[0], [1], 0] for ev in td]
         elif len(td[0]) == 2:
            elist = [[ev[0], [1], ev[1]] for ev in td]
         else:
            elist = [[ev[0], [ev[2]], ev[1]] for ev in td]
            # and track all modulators
            amp_all.extend([ev[2] for ev in td])

         mdata.append(elist)

      # if all amplitudes are constand 0 or 1, remove them
      if   UTIL.vals_are_constant(amp_all, cval=0): cval = 0
      elif UTIL.vals_are_constant(amp_all, cval=1): cval = 1
      else:                                         cval = 2
      if cval <= 1:
          if self.verb > 1:
             print('-- FSL amplitudes are all %s, removing...' % cval)
          for mrow in mdata:
             for event in mrow:
                event[1] = []

      return self.init_from_mdata(mdata)

   def init_from_mdata(self, mdata):
      """mdata should be of the form:
            one row per run, where each row is a list of events:
               [time [AM list] duration]
      """

      if not self.mdata_looks_valid(mdata):
         return 1

      self.mdata    = mdata

      # note whether the data is married (modulation or duration)
      self.mtype = TD.married_type(mdata)
      if self.mtype: self.married = 1

      # if durs, but constant, set dur_len instead
      if self.mtype & MTYPE_DUR:
         isconst, dlen = self.check_constant_duration()
         if isconst:
            self.write_dm = 0
            self.dur_len = dlen
         else:
            self.dur_len = -1

      # data will ignore any married information
      self.data     = [[val[0] for val in row] for row in mdata]
      self.clines   = None

      # init alist to be 0, 1 or 2, for each run so at least 2 "events"
      self.alist    = [0] * len(mdata)
      for rind, run in enumerate(mdata):
         if len(run) == 0:
            self.alist[rind] = 2
         elif len(run) == 1:
            self.alist[rind] = 1

      self.nrows    = len(self.data)
      self.row_lens = [len(row) for row in self.data]

      # empty data includes existing but empty runs
      if len(self.data) == 0:
         self.maxlen = 0
         self.minlen = 0
      else:
         self.maxlen = max([len(drow) for drow in self.data])
         self.minlen = min([len(drow) for drow in self.data])

      # accept an empty file?
      if self.nrows == 0 or self.maxlen == 0:
         self.empty = 1
         self.ready = 1
         return 0

      # if row lengths are all the same, use ncols, instead
      if UTIL.vals_are_constant(self.row_lens):
         self.ncols = self.row_lens[0]
         del(self.row_lens)
         self.row_lens = []

      # check to see if it is a 0/1 file
      self.binary = 1
      for row in self.data:
         if not UTIL.vals_are_0_1(row):
            self.binary = 0
            break

      self.ready = 1
      
      return 0

   def mdata_looks_valid(self, mdata, verb=0):
      """show be rows of [float [float list] float]"""
      errs = 0
      for rind, row in enumerate(mdata):
         for eind, event in enumerate(row):
            # check for bad event
            if len(event) != 3:
               if verb <= 0: return 0   # quiet failure
               errs += 1
               print('** (0-based) run %d, event %d: bad form: %s' \
                     % (rind, eind, event))
               if verb < 2: return 0
      if errs: return 0
      return 1

   def select_runs(self, rlist):
      """modify the timing element where the new one is based off the
         listed runs of the old one

         Elements of rlist are 1-based run indices from the old timing,
         itemizing the runs of the new timing.  A run index of zero means
         to make it empty.

         So rlist should contain integers in [0, old_num_runs].

         return 0 on success
      """

      nnew = len(rlist)

      if not self.ready:
         print("** select_runs: not ready")
         return 1

      if nnew < 1:
         print("** select_runs: missing run inputs (use 0 for empty)")
         return 1

      # --------------------------------------------------
      # limit rlist elements to [0, nold]
      # (but allow an index of zero to mean an empty run)

      # note the limit on rlist entries
      nold = 0
      if self.data != None:
         nold = len(self.data)

      if max(rlist) > nold or min(rlist) < 0:
         print("** select_runs, bad rlist limits in: %s" % rlist)
         return 1

      # update number of runs to match new list
      self.nruns = nnew
      self.nrows = nnew

      # --------------------------------------------------
      # then just perform the same operation on every list
      # (base variable should appear at top and bottom)
      # (else:append() value will depend on the list)

      # self.data
      lorig = self.data
      if lorig != None:
         d = []
         if self.verb > 3: print("++ padding data")
         for rind in rlist:
            # if > 0, get a full copy of an old run, else use an empty one
            if rind > 0: d.append(lorig[rind-1][:])
            else:        d.append([])
         # and replace
         self.data = d

      # self.mdata
      lorig = self.mdata
      if lorig != None:
         # if length mis-match, skip (probably empty)
         if len(lorig) == nold:
            if self.verb > 3: print("++ padding mdata")
            d = []
            # 1-based counting over runs
            for rind in rlist:
               # if > 0, get a full copy of an old run, else use an empty one
               if rind > 0: d.append(lorig[rind-1][:])
               else:        d.append([])
            # and replace
            self.mdata = d

      # self.alist
      lorig = self.alist
      if lorig != None:
         # if length mis-match, skip (probably empty)
         if len(lorig) == nold:
            if self.verb > 3: print("++ padding alist")
            d = []
            # 1-based counting over runs
            for rind in rlist:
               # if > 0, get a full copy of an old run, else use an empty one
               if rind > 0: d.append(lorig[rind-1])
               else:        d.append(2) # 2 '*' for empty run
            # and replace
            self.alist = d

      # self.run_lens
      lorig = self.run_lens
      if lorig != None:
         # if length mis-match, skip (probably empty)
         if len(lorig) == nold:
            if self.verb > 3: print("++ padding run_lens")
            d = []
            # 1-based counting over runs
            for rind in rlist:
               # if > 0, get a full copy of an old run, else use an empty one
               if rind > 0: d.append(lorig[rind-1])
               else:        d.append(0) # empty run
            # and replace
            self.run_lens = d

      if self.verb > 3: self.show("select_runs")

      return 0

   def show_duration_stats(self, per_run=0):
      """show min, mean, max, stdev for each column (unless col specified)"""

      if self.verb:
         form = "min = %g, mean = %g, max = %g, stdev = %g"
      else:
         form = "%g %g %g %g"

      # apply newdata as end times, whine if negative
      dlist = []
      for rind, run in enumerate(self.mdata):
         if per_run:
            mmms = form % UTIL.min_mean_max_stdev([event[2] for event in run])
            print(('run %d : ' % rind) + mmms)
         else:
            dlist.extend([event[2] for event in run])

      # if per_run, we are finished
      if per_run: return 0

      print(form % UTIL.min_mean_max_stdev(dlist))

      return 0

   def show(self, mesg=''):
      print(self.make_show_str(mesg=mesg))

   def make_show_str(self, mesg=''):
      if mesg: mstr = '%s : ' % mesg
      else:    mstr = ''

      # report data elements via their top-level lengths
      ldata = -1
      if self.data != None:
         ldata = len(self.data)
      lmdata = -1
      if self.mdata != None:
         lmdata = len(self.mdata)
      lclines = -1
      if self.clines != None:
         lclines = len(self.clines)
      lalist = -1
      if self.alist != None:
         lalist = len(self.alist)

      mstr = "--- %sAfniData element ---\n" \
             "   name (ready?)                   : %s (%d)\n"      \
             "   fname                           : %s\n" \
             "   len(data, mdata, clines, alist) : %d, %d, %d, %d\n" \
             "   nrows, ncols, is_rect()         : %d, %d, %d\n" \
             "   row_lens                        : %s\n" \
             "   binary, empty                   : %d, %d\n" \
             "   married, mtype                  : %d, %d\n" \
             "   minlen, maxlen, dur_len         : %d, %d, %g\n" \
             "   tr, nruns                       : %g, %d\n" \
             "   run_lens                        : %s\n" \
             "   write_dm, cormat_ready          : %d, %d\n" \
             "   verb                            : %d\n" \
             % ( mstr, self.name, self.ready, self.fname,
                ldata, lmdata, lclines, lalist,
                self.nrows, self.ncols, self.is_rect(), self.row_lens,
                self.binary, self.empty, self.married, self.mtype,
                self.minlen, self.maxlen, self.dur_len,
                self.tr, self.nruns, self.run_lens,
                self.write_dm, self.cormat_ready,
                self.verb)

      return mstr

def show_multi_isi_stats(adlist, run_lens, tr, verb=0):
   import lib_timing as LT

   nad = len(adlist)
   if nad == 0:
      print('** show_multi_isi_stats: no elements to list')
      return 1

   AD0 = adlist[0]
   MT = LT.AfniTiming(mdata=AD0.mdata)
   for ind in range(1, nad):
      tt = LT.AfniTiming(mdata=adlist[ind].mdata)
      MT.extend_rows(tt)

   if verb:
     MT.show('multistim timing')

   MT.show_isi_stats(mesg='%d elements'%nad, run_len=run_lens, tr=tr)

if __name__ == '__main__':
   print('** this is not a main module')
   sys.exit(1)


