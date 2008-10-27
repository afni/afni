#!/usr/bin/env python

g_has_SL = 1

# import libraries
import sys, math
import copy

try:  import numpy as N
except:
      print "** failed to import numpy"
      print "   may want to install the package: numpy"
      sys.exit(1)
try:  from scipy import linalg as SL
except:
      print "** failed to import scipy.linalg, will not evaluate matrices..."
      print "   may want to install the package: scipy"
      g_has_SL = 0
    

class AfniXmat:
    def __init__(self, filename="", from_mat=0, matrix=None, verb=1):
        """numpy matrix class

            init from filename or matrix,
                filename    : 1D file to read in as matrix
                from_mat    : use matrix for initialization
                matrix      : Numpy matrix to use if from_mat is set
                verb        : verbose level of operations

            mat             : data
            fname           : filename
            rows, cols      : dimensions
            labels          : simple text labels
            groups          : list: -1 (baseline), 0 (motion), else reg index
            tr              : in seconds
        """

        # main variables
        self.mat      = None            # actual data (numpy maxtirx)
        self.fname    = filename        # name of data file
        self.ready    = 0               # have a matrix

        # .xmat.1D variables
        self.nrows    = 0
        self.ncols    = 0
        self.labels   = None
        self.groups   = None
        self.goodlist = None
        self.tr       = 1.0
        self.nrowfull = 0
        self.nruns    = 1
        self.run_len  = 1
        self.nroi     = 1

        self.verb     = verb

        # computed variables
        self.cormat       = None        # correlation mat (normed xtx)
        self.cormat_ready = 0           # correlation mat is set

        # initialize...
        if self.fname: self.init_from_1D(self.fname)
        elif from_mat: self.init_from_matrix(matrix)

        if self.groups:
            if self.ncols: self.set_nruns()
            if len(self.groups) > 0:
                self.nroi = max(self.groups)
                if self.nroi < 0: self.nroi = 0

    def append(self, newmats, newname=''):
        """append each AfniMat to the current one"""
        # test before trashing current matrix
        if not self.ready:
            print '** append: AfniMat is not ready'
            return 1

        for mat in newmats:
            if not mat.ready:
                print '** append: AfniMat is not ready'
                return 1
            if mat.nrows != self.nrows:
                print '** append: nrows is different'
                return 2

        # update labels
        empty = 1
        if not self.labels: self.labels = ['' for ind in range(self.ncols)]
        else:               empty = 0
        for mat in newmats:
            if not mat.labels:
                self.labels.extend(['' for ind in range(mat.ncols)])
            else:
                self.labels.extend(mat.labels)
                empty = 0
        if empty:
           del(self.labels)
           self.labels = None

        # actual appending...
        matlist = [self.mat]
        for mat in newmats: matlist.append(mat.mat)
        self.mat = N.hstack(matlist)

        # update filename and shape
        self.nrows, self.ncols = self.mat.shape

        if newname: self.fname = newname
        else:       self.fname += ' (appended)'

        # nuke things that no longer apply
        del(self.groups)
        self.groups = None
        del(self.goodlist)
        self.goodlist = None
        del(self.cormat)
        self.cormat = None
        self.cormat_ready = 0

        return 0

    def copy(self):
        """return a complete (deep)copy of the current AfniMatrix"""
        return copy.deepcopy(self)

    def solve_against_1D(self, mat1D, acols=[], col1D=0):
        """solve Ax = b for x, where b is passed as a 1D matrix, and
           matrix A is restricted to column list acols
           - return error code and vector
                mat1D   - AfniXmat to solve for as b (using col1D)
                acols   - optional column subset to set A to
                col1D   - optional column of mat1D to choose as b
           error codes 0 = success, 1 = failure"""
        if not self.ready:
            if self.verb > 1: print '** cannot solve, matrix is not initialized'
            return 1, None
        if not mat1D.ready:
            if self.verb > 1: print '** cannot solve, 1D is not initialized'
            return 1, None
        if self.nrows != mat1D.nrows:
            if self.verb > 1: print '** cannot solve, col lengths do not match'
            return 1, None

        if not acols: acols = range(self.ncols)

        if not list2_is_in_list1(range(self.ncols), acols):
            if self.verb > 1: print '** cannot solve, Xcols outside of range'
            return 1, None
        if col1D < 0 or col1D >= mat1D.ncols:
            if self.verb > 1: print '** cannot solve, col1D outside of range'
            return 1, None

        if self.verb > 1: print '++ solving Ax=b for x, shapes = %s, %s' % \
                                (self.mat.shape, mat1D.mat.shape)

        # pass XtX and Xtb to SL.solve
        x = self.mat[:,acols]
        a = N.matrix(x.T) * N.matrix(x)
        b = mat1D.mat[:,col1D]
        b = b.reshape(self.nrows, 1)
        b = N.matrix(x.T) * N.matrix(b)

        if self.verb > 1: sdebug = self.verb
        else:             sdebug = 0
        soln = SL.solve(a, b, debug=sdebug)

        del(x)
        del(a)
        del(b)

        return 0, AfniXmat(from_mat=1, matrix=soln, verb=self.verb)

    def linear_combo(self, factmat, acols=[]):
        """compute the sum of factormat times the appropriate columns
           return an error code and the summed array
                factormat : an AfniXmat array of scalars
                acols     : optional list of matrix columns
                            (the length must match len(factors))"""

        if not self.ready:
            if self.verb > 1: print '** linear_combo: mat not initialized'
            return 1, None

        if not acols: acols = range(self.ncols)

        if len(acols) != factmat.nrows:
            if self.verb > 1: print '** linear_combo, col lengths do not match'
            return 1, None

        if not list2_is_in_list1(range(self.ncols), acols):
            if self.verb > 1: print '** linear_combo, Xcols outside of range'
            return 1, None

        if self.verb > 1:
            print '++ summing Ax over cols: %s' % acols

        factors = factmat.mat.flatten()

        total = N.zeros((self.nrows,))  # vector
        for ind in range(len(acols)):
            total += factors[ind] * self.mat[:,acols[ind]]

        return 0, AfniXmat(from_mat=1, matrix=total, verb=self.verb)
            

    def set_cormat(self):
        """set xt, norms and cormat, the correlation matrix"""
        if not self.ready: return
        if self.cormat_ready: return

        if not g_has_SL:
           print '** missing scipy.linalg, cannot compute correlation matrix'
           return

        if self.nrows < 2 or self.ncols < 2: return

        # make normalized transpose (unit vector lengths) ...
        mat = copy.deepcopy(self.mat)
        xtn = N.transpose(mat)

        # demean (the stupid, ugly, loser transpose)
        means = [N.mean(col) for col in xtn]
        for c in range(self.ncols):     # demean each row
            xtn[c] -= means[c]

        # and normalize
        norms = [SL.norm(col) for col in xtn]
        for c in range(self.ncols):     # normalize each row
            xtn[c] /= norms[c]

        xn = N.transpose(xtn)           # transpose back to normalized xmat

        # finally, assign cormat
        self.cormat = N.matrix(xtn) * N.matrix(xn)
        self.cormat_ready = 1

        # nuke temporary (normalized) matrices
        del(xtn)
        del(xn)
        del(mat)
        del(means)
        del(norms)

    def list_cormat_warnings(self, cutoff=0.4):

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

        if self.verb > 2:
            print '-- LCP: len(base, mot, roi) = (%d, %d, %d)' % \
                  (len(basecols), len(motcols), len(roicols))

        # make a list of (values,r,c) triples in the lower triangle
        clist = []
        for row in range(1,self.ncols-1):
            for col in range(row):
                clist.append((cmat[row, col], row, col))

        clist.sort(reverse=True)  # largest to smallest

        # now make a list of evil-doers
        badlist = []
        for val, r, c in clist:
            if val == 1.0:
                badlist.append((val, r, c)) # flag duplication
                continue

            # skip motion against either motion or baseline
            rbase = r in basecols
            cbase = c in basecols
            rmot  = r in motcols
            cmot  = c in motcols

            if cmot and (rbase or rmot): continue
            if cbase and rmot: continue

            if val < cutoff: break

            badlist.append((val, r, c))         # so keep this one

        if self.verb > 1:
            print '-- LCP: badlist length = %d' % len(badlist)

        del(clist)

        return badlist

    def set_nruns(self):
        """find the first column in group 0, verify that it looks like
           one run of 1s (with remaining 0s), and use it to set the length"""

        # rcr - also, test against new RunStart comment line

        self.nruns = 1
        if not self.groups or not self.ncols: return

        try:
            base_ind = self.groups.index(-1)
            if base_ind < 0:
                if self.verb > 1: print '-- no baseline group to set runs with'
                return

            b0 = self.mat[:,base_ind]
            base0 = b0.tolist()

            # first be sure that there is nothing but 0s and 1s
            vals = N.unique(b0)
            vals.sort()
            vals = vals.tolist()
            if len(vals) != 2:
                if self.verb > 1:
                    print '-- baseline without exactly 2 values : %s' % vals
                return
            if vals[0] != 0 or vals[1] != 1:
                if self.verb > 1:
                    print '-- baseline vals not just 0,1: %s' % vals
                return

            # looks good, find run length and the number of runs
            first = base0.index(1)              # find first 1
            try: next = base0.index(0,first+1)  # find next 0
            except:     # base until end....
                next = len(base0)
            if next <= first:
                if self.verb > 1: print '-- odd run_len check...'
                return

            # we have a run length
            rlen = next - first
            nruns = len(base0)/rlen
            if rlen*nruns != len(base0):
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
        if not self.ready: return "++ mat       : <unset>"

        mstr = "++ mat.shape : %s\n" \
               "++ fname     : %s\n" \
               "++ rows      : %d\n" \
               "++ cols      : %d\n" \
               "++ labels    : %s\n" \
               "++ groups    : %s\n" \
               "++ goodlist  : %s\n" \
               "++ tr        : %s\n" \
               "++ nrowfull  : %d\n" % \
               (str([self.mat.shape]), self.fname, self.nrows, self.ncols,
                self.labels, self.groups, self.goodlist, self.tr, self.nrowfull)

        return mstr

    def show_conds(self):
        print self.make_show_conds_str()

    def make_show_conds_str(self):
        """return a string that displays condition numbers for matrix of:
                - all columns
                - main regressors
                - main + baseline
                - main + motion
                - baseline + motion
                - baseline
                - motion"""
        if not g_has_SL:
            return '** missing scipy.linalg, cannot evaluate X matrix'

        mesg = 'Condition Numbers:\n\n'                       \
               '    all regressors      : %.1f\n\n'  \
               '    main regressors     : %.1f\n'    \
               '    main + baseline     : %.1f\n'    \
               '    main + motion       : %.1f\n\n'  \
               '    motion + baseline   : %.1f\n'    \
               '    baseline            : %.1f\n'    \
               '    motion              : %.1f\n'    \
              % ( self.cond_num_by_cols(range(self.ncols)),
                  self.cond_num_by_cols(self.cols_by_group_list([],allroi=1)),
                  self.cond_num_by_cols(self.cols_by_group_list([-1],allroi=1)),
                  self.cond_num_by_cols(self.cols_by_group_list([0],allroi=1)),
                  self.cond_num_by_cols(self.cols_by_group_list([-1,0])),
                  self.cond_num_by_cols(self.cols_by_group_list([-1])),
                  self.cond_num_by_cols(self.cols_by_group_list([0])))

        return mesg

    def cond_num_by_cols(self, cols):
        """given a column list, return the matrix condition number"""

        if not g_has_SL:
           print '** missing scipy.linalg, cannot get condition numbers'
           return 0.0

        if len(cols) < 2: return 0.0
        if min(cols) < 0 or max(cols) >= self.ncols:
            print '** column indices ouside range [0,%d]' % (self.ncols-1)
        mat = self.mat[:,cols]
        u,s,vh = SL.svd(mat)
        return s[0]/s[-1]

    def eval_xmat(self, cols=[], labs=[], groups=[], mesg=None):
        """evaluate the X matrix (condition number)
           the matrix can be reduced given:
                cols    : a list of matrix columns to choose from
                labs    : a list of column labels to choose from
                groups  : a list of column groups to choose from"""
        if not g_has_SL:
            print '** missing scipy.linalg, cannot evaluate X matrix'
            return
        sublist = []
        # start with cols, and make ordered list (order wrt self.ncols)
        if len(cols) > 0:
            if not list2_is_in_list1(range(self.ncols), cols, "columns"):
                return
            sublist.extend([val for val in range(self.ncols) if val in cols])

        if len(labs) > 1:
            sublist.extend(self.cols_by_label_list(labs))

        if len(groups) > 0:
            sublist.extend(self.cols_by_group_list(groups))

        # by default, choose all columns
        if len(sublist) == 0: sublist = range(self.ncols)

        # and restrict sublist to sorted, unique list
        sublist.sort()
        newlist = []
        for val in sublist:     # yeah, this is slow...
            if not val in newlist: newlist.append(val)
        if len(newlist) < 2:    # verify that there is something to do
            if mesg: print "** (%s) cannot get cond for short column list %s" \
                           % (mesg, newlist)
            else:    print "** cannot get cond for short column list %s" \
                           % (newlist)
            return

        mat = self.mat[:,newlist]
        u,s,vh = SL.svd(mat)

        if self.verb > 1:
            print "-- matrix from sublist: %s" % (newlist)
            print mat
        cond = s[0]/s[-1]
        if mesg: print '%s : condition number %f' % (mesg, cond.item())
        else:    print 'condition number %f' % cond.item()

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
        if not list2_is_in_list1(self.groups, groups, "groups"): return []
        return [val for val in range(self.ncols) if self.groups[val] in groups]
        
    def cols_by_label_list(self, labels):
        """return a list of columns, given a list of labels"""
        if not self.labels or not labels: return []
        if not list2_is_in_list1(self.labels, labels, "labels"): return []
        return [val for val in range(self.ncols) if val in cols]

    def init_from_matrix(self, matrix):
        """initialize AfniXmat from a Numpy matrix"""
        try: shape = list(matrix.shape)
        except:
            print '** matrix for init must be Numpy of format'
            return 1
        if len(shape) < 1 or len(shape) > 2:
            print '** cannot init AfniXmat from %d-D matrix' % len(shape)
            return 1

        if len(shape) == 1: matrix.shape = (shape[0], 1)

        self.mat = matrix
        self.nrows, self.ncols = matrix.shape
        self.ready = 1

    def init_from_1D(self, fname):
        """initialize AfniXmat from a 1D file"""
        mat, clines = read_1D_file(fname)
        if not mat: return
        self.mat   = N.array(mat)
        self.nrows  = len(mat)
        self.ncols  = len(mat[0])
        self.ready = 1

        for line in clines:
            label, data = c1D_line2labelNdata(line)
            if not label: continue

            try:        # to process some of the data
                if label == 'ni_type':
                    ncols, type = data.split('*')
                    ncols = int(ncols)
                    if self.verb > 1:
                        print "-- label %s: cols = %d, type = %s" % \
                              (label, ncols, type)
                    if ncols != self.ncols:
                        print "** matrix cols %d != %s cols %d" % \
                              (self.ncols, label, ncols)
                elif label == 'ni_dimen':
                    nrows = int(data)
                    if self.verb > 1:
                        print "-- label %s: rows = %d" % (label, nrows)
                    if nrows != self.nrows:
                        print "** matrix rows %d != %s rows %d" % \
                              (self.nrows, label, nrows)
                elif label == 'ColumnLabels':
                    self.labels = [str.strip() for str in data.split(';')]
                    if self.verb > 1:
                        print "-- label %s: labels = %s" % (label, self.labels)
                    if self.ncols != len(self.labels):
                        print "** %d ColumnLabels but %d columns" %     \
                              (len(self.labels), self.ncols)
                        self.labels = None
                elif label == 'ColumnGroups':
                    self.groups = decode_1D_ints(data)
                    if self.groups:
                        if len(self.groups) != self.ncols:
                            print "** ColumnGroups len %d != ncols %d" % \
                                  (len(self.groups), self.ncols)
                    if self.verb > 1:
                        print "-- label %s: groups %s" % (label,self.groups)
                elif label == 'RowTR':
                    self.tr = float(data)
                    if self.verb > 1:
                        print "-- label %s: TR %s" % (label,self.tr)
                elif label == 'GoodList':
                    self.goodlist = decode_1D_ints(data)
                    if self.goodlist:
                        if len(self.goodlist) != self.nrows:
                            print "** GoodList missing %d rows" % \
                                  self.nrows-len(self.goodlist)
                    if self.verb > 1:
                        print "-- label %s: goodlist %s" % (label,self.goodlist)
                elif label == 'NRowFull':
                    self.nrowfull = int(data)
                    if self.verb > 1:
                        print "-- label %s: nrowfull %s" % (label,self.nrowfull)
                elif self.verb > 1:
                    print "** unknown comment label '%s'" % label
            except:
                print "** failed to process comment label '%s'" % label

# convert columns to encoded int string
def encode_1D_ints(cols):
   """convert a list of columns to a ',' and '..' separated string"""
   if not cols: return ''
   if len(cols) < 1: return ''

   text = '%d' % cols[0]
   prev = cols[0]
   ind  = 1
   while ind < len(cols):
      ncontinue = consec_len(cols, ind-1) - 1
      if ncontinue <= 1:     # then no '..' continuation, use ','
         text = text + ', %d' % cols[ind]
         ind += 1
      else:
         text = text + '..%d' % cols[ind+ncontinue-1]
         ind += ncontinue

   return text

def consec_len(cols, start):
   """return the length of consecutive numbers - always at least 1"""
   prev = cols[start]
   length = len(cols)
   ind  = start
   for ind in range(start+1, length+1):
      if ind == length: break
      if cols[ind] != prev + 1:
         break
      prev = cols[ind]
   if ind == start:  length = 1
   else:             length = ind-start        

   return length


# encoded int string to column list
def decode_1D_ints(str, verb=1, max=-1):
    """Decode a comma-delimited string of ints, ranges and A@B syntax,
       and AFNI-style sub-brick selectors (including A..B(C)).
       If the A..B format is used, and B=='$', then B gets 'max'.
       - return a list of ints"""
    slist = str.split(',')
    if len(slist) == 0:
        if verb > 1: print "-- empty 1D_ints from string '%s'" % str
        return None
    ilist = []                  # init return list
    for s in slist:
        try:
            if s.find('@') >= 0:        # then expect "A@B"
                [N, val] = [int(n) for n in s.split('@')]
                ilist.extend([val for i in range(N)])
            elif s.find('..') >= 0:     # then expect "A..B"
                pos = s.find('..')
                if s.find('(', pos) > 0:    # look for "A..B(C)"
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = int(v1)
                   [v2, step] = v2.split('(')
                   if v2 == '$': v2 = max
                   else:         v2 = int(v2)
                   # have start and end values, get step
                   step, junk = step.split(')')
                   step = int(step)
                   ilist.extend([i for i in range(v1, v2+1, step)])
                else:
                   [v1, v2] = [n for n in s.split('..')]
                   v1 = int(v1)
                   if v2 == '$': v2 = max
                   else:         v2 = int(v2)
                   ilist.extend([i for i in range(v1, v2+1)])
            else:
                ilist.extend([int(s)])
        except:
            print "** cannot decode_1D '%s' in '%s'" % (s, str)
            return None
    return ilist

def c1D_line2labelNdata(cline, verb=1):
    """expect cline to be of the form: '# LABEL = "DATA"'
    returning LABEL, DATA"""
    sline = cline.split()

    # check for problems
    if len(sline) < 4 or sline[0] != "#" or sline[2] != "=":
        if verb > 2: print "-- skipping useless comment line: '%s'" % cline
        return None, None

    label = sline[1]    # store the line label

    dline = ' '.join(sline[3:]) # put the line back together

    # again, check for problems
    if len(dline) < 2 or dline[0] != '"' or dline[-1] != '"':
        if verb > 1: print "** missing quotes in comment line: '%s'" % cline
        return None, None

    data = dline[1:-1]

    if verb > 2: print "++ line2labelNdata returns '%s', '%s'" % (label,data)

    return label, data

def read_1D_file(fname):
    """read 1D file, returning the data in a matrix, and comments in clines"""
    try: fp = open(fname, 'r')
    except:
        print "** failed to open 1D file '%s'" % fname
        return None, None

    fmat = []           # data lines
    clines = []         # comment lines

    lind = 0
    for line in fp.readlines():
        lind += 1
        lary = line.split()
        if len(lary) == 0: continue
        if lary[0] == '#':
            clines.append(line)
            continue

        # so this should be data
        try:
            fmat.append([float(x) for x in lary])
        except:
            print "** failed to convert line '%s' to floats" % line
            return None, None
        if len(lary) != len(fmat[0]):
            print "** matrix is not square at line %d" % lind
            return None, None

    return fmat, clines

def list2_is_in_list1(list1, list2, label=''):
    """return 0 or 1, based on whether every element in list2 exists
       in list1"""

    if not list1: return 0
    if not list2: return 1

    for item in list2:
        if not item in list1:
            if label: print "-- list '%s' not subset in AfniXmat" % label
            return 0

    return 1

