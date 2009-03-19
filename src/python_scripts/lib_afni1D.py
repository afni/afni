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
      self.run_len  = 1
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
      if self.fname: self.init_from_general_name(self.fname)
      elif from_mat: self.init_from_matrix(matrix)

      self.update_group_info()

   def reduce_by_tlist(self, tlist):
      """reduce by time list, similiar to afni's {} selector
         this affects run_len and runs, so try to fix those"""

      if not self.ready:
         print '** append: Afni1D is not ready'
         return
      if not UTIL.is_valid_int_list(tlist, 0, self.nt-1, whine=1): return
      if len(tlist) < 1: return

      self.mat = [[row[t] for t in tlist] for row in self.mat]

      if self.nrowfull == self.nt: self.nrowfull = len(tlist)
      else:                        self.nrowfull = 0  # cannot tell
      self.nt         = len(tlist)

      if self.goodlist: self.goodlist = [self.goodlist[t] for t in tlist]

      self.update_group_info()  # since nruns or run_len may have changed

   def reduce_by_vec_list(self, vlist):
      """reduce the dataset according to the vector list"""

      if not self.ready:
         print '** copy vec: Afni1D is not ready'
         return 
      if not UTIL.is_valid_int_list(vlist, 0, self.nvec-1, whine=1): return
      if len(vlist) < 1: return

      self.mat = [self.mat[i] for i in vlist]

      self.nvec = len(vlist)
      self.nroi = self.nvec     # don't know, so assume all

      # update lists
      if self.labels: self.labels = [self.labels[i] for i in vlist]
      if self.groups: self.groups = [self.groups[i] for i in vlist]

   def sort(self, reverse=0):
      """sort data over time axis (possibly reverse order)"""
      for ind in range(self.nvec):
         self.mat[ind].sort(reverse=reverse)

   def write(self, fname, sep=" ", overwrite=0):
      """write the data to a new .1D file

            fname       : filename is required
            sep         : separator between elements
            overwrite   : whether to allow overwrite

         return status"""
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
         fp.write(sep.join('%f' % self.mat[i][row] for i in range(self.nvec)))
         fp.write('\n')

      fp.close()

      return 0

   def append_vecs(self, newmats, newname=''):
      """append each Afni1D to the current one"""
      # test before trashing current matrix
      if not self.ready:
         print '** append: Afni1D is not ready'
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
      else:            empty = 0
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
      if not self.ready:
         print "** matrix '%s' is not ready for transposing" % self.name
         return
      self.mat  = [[row[i] for row in self.mat] for i in range(self.nt)]
      newnt     = self.nvec
      self.nvec = self.nt
      self.nt   = newnt

   def set_cormat(self, update=0):
      """set cormat (the correlation matrix) and cormat.ready

         Note that the (Pearson's) correlations are de-meaned, and so the
         constant baseline terms can be highly correlated.

         The cosine matrix might also be quite helpful.
      """
      if not self.ready: return
      if self.cormat_ready:
         if not update: return
         # otherwise, nuke the old stuff
         del(self.cormat)
         self.cormat = None
         self.cormat_ready = 0

      if self.nvec < 2 or self.nt < 2: return

      # make copy to abuse
      cmat = copy.deepcopy(self)

      # demean each vector (for cormat), unless it is all 1's
      means = [sum(vec)/cmat.nt for vec in cmat.mat]
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
            cmat.mat[v][ind] /= norms[v]

      # finally, assign cormat
      self.cormat = [[dotprod(r1,r2) for r2 in cmat.mat] for r1 in cmat.mat]
      self.cormat_ready = 1

      # nuke temporary (normalized) matrices
      del(cmat)
      del(means)
      del(norms)

   def list_cormat_warnings(self, cutoff=0.4):
      """return a list of corval, vec, index
         for each cormat value with abs() > cutoff"""

      if not self.ready:
         return '** no X-matrix to compute correlation matrix from'

      if not self.cormat_ready: self.set_cormat() # create cormat
      if not self.cormat_ready: # then failure
         print '** cormat_warnings: failed to create cormat'
         return 1

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

      clist.sort(reverse=True)  # largest to smallest

      # now make a list of evil-doers
      badlist = []
      for aval, val, r, c in clist:
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

      return badlist

   def update_group_info(self):
      """if self.groups, try to set nruns and nroi"""

      if self.groups:
         if self.nvec: self.set_nruns()
         if len(self.groups) > 0:
            self.nroi = max(self.groups)
            if self.nroi < 0: self.nroi = 0

   def set_nruns(self):
      """find the first column in group 0, verify that it looks like
         one run of 1s (with remaining 0s), and use it to set the length"""

      # rcr - also, test against new RunStart comment line

      self.nruns = 1
      if not self.groups or not self.nvec: return

      try:
         base_ind = self.groups.index(-1)
         if base_ind < 0:
            if self.verb > 1: print '-- no baseline group to set runs with'
            return

         b0 = self.mat[base_ind]

         # skip 0s, count 1s, rest must be 0s
         for val in b0:
            if val != 0 or val != 1:
               if self.verb > 1:
                  print '-- baseline vals not just 0,1: %s' % val
            return

         # looks good, find run length and the number of runs
         first = b0.index(1)              # find first 1
         try:    next = b0.index(0,first+1)  # find next 0
         except: next = self.nt
         if next <= first:
            if self.verb > 1: print '-- odd run_len check...'
            return

         # we have a run length
         rlen = next - first
         nruns = self.nt//rlen          # integral division
         if rlen*nruns != self.nt:
            print '** nruns failure: rlen = %d, nruns = %d, len = %d' % \
                 (rlen, nruns, len(base0))
            return

         # success!

         self.run_len = rlen
         self.nruns   = nruns
         
      except:
         return

   def show(self):
      print self.make_show_str()

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
      return [val for val in range(self.ncols) if self.groups[val] in groups]
      
   def cols_by_label_list(self, labels):
      """return a list of columns, given a list of labels"""
      if not self.labels or not labels: return []
      if not list2_is_in_list1(self.labels, labels, "labels"): return []
      return [val for val in range(self.ncols) if val in cols]

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

   def init_from_general_name(self, name):
      """might contain [] or {} selection"""
      aname = BASE.afni_name(self.fname)
      if self.init_from_1D(aname.rpve()): return 1 # failure

      self.aname = aname
      self.fname = aname.rpve()
      self.name  = aname.pve()

      if self.init_from_1D(self.fname): return

      # apply column and/or row selectors
      if aname.colsel:self.reduce_by_vec_list(UTIL.decode_1D_ints(aname.colsel))
      if aname.rowsel:self.reduce_by_tlist(UTIL.decode_1D_ints(aname.rowsel))

   def init_from_1D(self, fname):
      """initialize Afni1D from a 1D file (return err code)"""
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
   try: nsum = sum([v1[i]*v2[i] for i in range(len(v1))])
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

