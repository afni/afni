#!/usr/bin/env python

import sys
import module_test_lib
g_testlibs = ['sys', 'math', 'numpy']
if module_test_lib.num_import_failures(g_testlibs): sys.exit(1)
    

# import libraries
import math
import copy
import numpy as N

import afni_util as UTIL
import lib_textdata as TD

class AfniXmat:
    def __init__(self, filename="", from_mat=0, matrix=None, verb=1):
        """numpy matrix class

            init from filename or matrix,
                filename    : 1D file to read in as matrix
                from_mat    : use matrix for initialization (numpy or [])
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
        self.cosmat       = None        # cosine matrix (no de-meaned xtx)
        self.cormat_ready = 0           # correlation mat is set

        # scipy: keep it local
        self.SL           = None
        self.SLfirst      = 1           # first try to test SL (see have_SL())

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
        del(self.cosmat)
        self.cosmat = None
        self.cormat_ready = 0

        return 0

    def copy(self):
        """return a complete (deep)copy of the current AfniMatrix"""
        return copy.deepcopy(self)

    def solve_against_1D(self, mat1D, acols=[], col1D=0):
        """solve Ax = b for x, where b is passed as a 1D matrix, and
           matrix A is restricted to column list acols
           - return error string and vector
                mat1D   - AfniXmat to solve for as b (using col1D)
                acols   - optional column subset to set A to
                col1D   - optional column of mat1D to choose as b
           an empty error string will be considered success"""
        if not self.ready:
            return '** cannot solve, matrix is not initialized', None
        if not mat1D.ready:
            return '** cannot solve, 1D is not initialized', None
        if self.nrows != mat1D.nrows:
            return '** cannot solve, col lengths do not match', None
        if not self.have_SL():
            return '** cannot solve, need scipy package', None

        if not acols: acols = range(self.ncols)

        if not list2_is_in_list1(range(self.ncols), acols):
            return '** cannot solve, Xcols outside of range', None
        if col1D < 0 or col1D >= mat1D.ncols:
            return '** cannot solve, col1D outside of range', None

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
        try: soln = self.SL.solve(a, b, debug=sdebug)
        except: return '** matrix is not solvable', None

        del(x)
        del(a)
        del(b)

        return '', AfniXmat(from_mat=1, matrix=soln, verb=self.verb)

    def linear_combo(self, factmat, acols=[]):
        """compute the sum of factormat times the appropriate columns
           return an error string and the summed array
                factormat : an AfniXmat array of scalars
                acols     : optional list of matrix columns
                            (the length must match len(factors))
           on success, the error string should be empty
        """

        if not self.ready:
            return '** linear_combo: mat not initialized', None

        if not acols: acols = range(self.ncols)

        if len(acols) != factmat.nrows:
            return '** linear_combo, col lengths do not match', None

        if not list2_is_in_list1(range(self.ncols), acols):
            return '** linear_combo, Xcols outside of range', None

        if self.verb > 1:
            print '++ summing Ax over cols: %s' % acols

        factors = factmat.mat.flatten()

        total = N.zeros((self.nrows,))  # vector
        for ind in range(len(acols)):
            total += factors[ind] * self.mat[:,acols[ind]]

        return '', AfniXmat(from_mat=1, matrix=total, verb=self.verb)
            

    def set_cormat(self):
        """set cormat (the correlation matrix) and cosmat (cos0 matrix),
           and finally cormat.ready

           Note that the (Pearson's) correlations are de-meaned, and so the
           constant baseline terms can be highly correlated.

           The cosine matrix might also be quite helpful.
        """
        if not self.ready: return
        if self.cormat_ready: return

        if self.nrows < 2 or self.ncols < 2: return

        # make normalized transpose (unit vector lengths) ...
        mat = copy.deepcopy(self.mat)
        xtn = N.transpose(mat)          # for cormat (correlations)
        ctn = copy.deepcopy(xtn)        # for cosmat (cosines)

        # demean each row (for cormat), unless it is all 1's
        means = [N.mean(col) for col in xtn]
        for c in range(self.ncols):
            min = xtn[c].min()
            max = xtn[c].max()
            if min != 1.0 or max != 1.0:
               xtn[c] -= means[c]

        # and normalize
        norms = [self.norm(col) for col in xtn]
        for c in range(self.ncols):     # normalize each row
            xtn[c] /= norms[c]
        norms = [self.norm(col) for col in ctn]
        for c in range(self.ncols):     # normalize each cos0 row
            ctn[c] /= norms[c]

        xn = N.transpose(xtn)           # transpose back to normalized xmat
        cn = N.transpose(ctn)          # transpose back to normalized xmat

        # finally, assign cormat and cosmat
        self.cormat = N.matrix(xtn) * N.matrix(xn)
        self.cosmat = N.matrix(ctn) * N.matrix(cn)
        self.cormat_ready = 1

        # nuke temporary (normalized) matrices
        del(xtn)
        del(xn)
        del(mat)
        del(means)
        del(norms)

    def have_SL(self):
        """determine whether we have scipy.linalg"""
        
        # if we have not done so before, try to import it
        if self.SLfirst:
           try:  from scipy import linalg as SL
           except:
               print "** failed to import scipy.linalg"
               print "   may want to install the package: scipy"
           else:
               if self.verb > 1: print '-- have scipy...'
               self.SL = SL
           self.SLfirst = 0

        return (self.SL != None)

    def SLnorm(self, vec):
        """return scipy.linalg norm"""
        return self.SL.norm(vec)

    def norm(self, vec):
        """return the euclidean norm, either from scipy or computed"""

        if self.have_SL():
           if self.verb > 4: print '-- SL: using scipy norm'
           return self.SLnorm(vec)

        if self.verb > 4: print '-- SL: using local norm'

        if len(vec) < 1: return 0.0
        sum = 0.0
        for val in vec: sum += val*val
        return math.sqrt(sum)

    def list_cormat_warnings(self, cutoff=0.4):
        """return a list of corval, cosval, row, col
           for each cormat value with abs() > cutoff"""

        if not self.ready:
            return '** no X-matrix to compute correlation matrix from'

        if not self.cormat_ready: self.set_cormat() # create cormat
        if not self.cormat_ready: # then failure
            print '** cormat_warnings: failed to create cormat'
            return 1

        cmat = self.cormat
        cosmat = self.cosmat

        basecols = self.cols_by_group_list([-1])
        motcols  = self.cols_by_group_list([0])
        roicols  = self.cols_by_group_list([],allroi=1)

        if self.verb > 1:
            print '-- LCP: len(base, mot, roi) = (%d, %d, %d), cut = %.2f' % \
                  (len(basecols), len(motcols), len(roicols), cutoff)

        # make a list of (abs(val),val,cos,r,c) tuples in lower triangle
        clist = []
        for row in range(1,self.ncols):
            for col in range(row):
                clist.append((abs(cmat[row, col]), cmat[row,col],
                             cosmat[row, col], row, col))

        clist.sort(reverse=True)  # largest to smallest

        # now make a list of evil-doers
        badlist = []
        for aval, val, cval, r, c in clist:
            if aval == 1.0:
                badlist.append((val, cval, r, c)) # flag duplication
                continue

            # skip motion against either motion or baseline
            rbase = r in basecols
            cbase = c in basecols
            rmot  = r in motcols
            cmot  = c in motcols

            if cmot and (rbase or rmot): continue
            if cbase and rmot: continue

            if aval < cutoff: break

            badlist.append((val, cval, r, c))         # so keep this one

        if self.verb > 1:
            print '-- LCP: badlist length = %d' % len(badlist)

        del(clist)

        return badlist

    def list_cosmat_warnings(self, cutoff=0.4, check_mot_base=0):
        """return a list of cosval, corval, row, col
           for each cosmat value with abs(cosval) >= cutoff

                cutoff         : cutoff for what is considered bad
                check_mot_base : flag to compare motion vs mot/base
        """

        if not self.ready:
            return '** no X-matrix to compute cosine matrix from'

        if not self.cormat_ready: self.set_cormat() # create cormat
        if not self.cormat_ready: # then failure
            print '** cosmat_warnings: failed to create cormat'
            return 1

        cmat = self.cormat
        cosmat = self.cosmat

        basecols = self.cols_by_group_list([-1])
        motcols  = self.cols_by_group_list([0])
        roicols  = self.cols_by_group_list([],allroi=1)

        if self.verb > 1:
            print '-- LCosW: len(base, mot, roi) = (%d, %d, %d), cut = %.2f, ' \
                  'mot = %d' % (len(basecols), len(motcols),
                                len(roicols), cutoff, check_mot_base)

        # make a list of (abs(cos),cos,cor,r,c) tuples in lower triangle
        clist = []
        for row in range(1,self.ncols):
            for col in range(row):
                clist.append((abs(cosmat[row, col]), cosmat[row,col],
                             cmat[row, col], row, col))

        clist.sort(reverse=True)  # largest to smallest

        # now make a list of evil-doers
        badlist = []
        for aval, val, rval, r, c in clist:
            if aval == 1.0:
                badlist.append((val, rval, r, c)) # flag duplication
                continue

            # maybe we do not check motion vs mot/base
            if not check_mot_base:
               rbase = r in basecols
               cbase = c in basecols
               rmot  = r in motcols
               cmot  = c in motcols

               if cmot and (rbase or rmot): continue
               if cbase and rmot: continue

            if aval < cutoff: break

            badlist.append((val, rval, r, c))         # so keep this one

        if self.verb > 1:
            print '-- LCosW: badlist length = %d' % len(badlist)

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

    def make_show_conds_str(self, extra_cols=[]):
        """return a string that displays condition numbers for matrix of:
                - all columns
                - (optional) chosen columns
                - main regressors
                - main + baseline
                - main + motion
                - baseline + motion
                - baseline
                - motion"""
        if not self.have_SL():
            return '** missing scipy.linalg, cannot evaluate X matrix'

        if len(extra_cols) > 0:
            extra_str = '    chosen regressors   : %.1f\n\n' %  \
                    self.cond_num_by_cols(extra_cols)
        else:
            extra_str = ''

        mesg = 'Condition Numbers:\n\n'              \
               '    all regressors      : %.1f\n\n'  \
               '%s'                                  \
               '    main regressors     : %.1f\n'    \
               '    main + baseline     : %.1f\n'    \
               '    main + motion       : %.1f\n\n'  \
               '    motion + baseline   : %.1f\n'    \
               '    baseline            : %.1f\n'    \
               '    motion              : %.1f\n'    \
              % ( self.cond_num_by_cols(range(self.ncols)),
                  extra_str,
                  self.cond_num_by_cols(self.cols_by_group_list([],allroi=1)),
                  self.cond_num_by_cols(self.cols_by_group_list([-1],allroi=1)),
                  self.cond_num_by_cols(self.cols_by_group_list([0],allroi=1)),
                  self.cond_num_by_cols(self.cols_by_group_list([-1,0])),
                  self.cond_num_by_cols(self.cols_by_group_list([-1])),
                  self.cond_num_by_cols(self.cols_by_group_list([0])))

        return mesg

    def cond_num_by_cols(self, cols):
        """given a column list, return the matrix condition number"""

        if not self.have_SL():
           print '** missing scipy.linalg, cannot get condition numbers'
           return 0.0

        if len(cols) < 2: return 0.0
        if min(cols) < 0 or max(cols) >= self.ncols:
            print '** column indices ouside range [0,%d]' % (self.ncols-1)
        mat = self.mat[:,cols]
        try: u,s,vh = self.SL.svd(mat)
        except:
           print '** failed to compute condition for selected columns'
           return 0.0
        return s[0]/s[-1]

    def eval_xmat(self, cols=[], labs=[], groups=[], mesg=None):
        """evaluate the X matrix (condition number)
           the matrix can be reduced given:
                cols    : a list of matrix columns to choose from
                labs    : a list of column labels to choose from
                groups  : a list of column groups to choose from"""
        if not self.have_SL():
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
        u,s,vh = self.SL.svd(mat)

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
        # if not list2_is_in_list1(self.groups, groups, "groups"): return []
        return [val for val in range(self.ncols) if self.groups[val] in groups]
        
    def cols_by_label_list(self, labels):
        """return a list of columns, given a list of labels"""
        if not self.labels or not labels: return []
        if not list2_is_in_list1(self.labels, labels, "labels"): return []
        return [val for val in range(self.ncols) if val in cols]

    def init_from_matrix(self, matrix):
        """initialize AfniXmat from a Numpy matrix or 2D array"""
        if type(matrix) == type([]):        mat = N.array(matrix)
        elif isinstance(matrix, N.ndarray): mat = matrix
        else:
            print '** matrix for init must be Numpy or [[float]] format'
            return 1
        shape = list(mat.shape)
        if len(shape) < 1 or len(shape) > 2:
            print '** cannot init AfniXmat from %d-D matrix' % len(shape)
            return 1

        if len(shape) == 1: mat.shape = (shape[0], 1)

        self.mat = mat
        self.nrows, self.ncols = mat.shape
        self.ready = 1

    def init_from_1D(self, fname):
        """initialize AfniXmat from a 1D file"""
        mat, clines = TD.read_data_file(fname)
        if not mat: return
        if not TD.data_is_rect(mat):
            print '** matrix is not rectangular in %s' % fname
            return
        self.mat   = N.array(mat)
        self.nrows  = len(mat)
        self.ncols  = len(mat[0])
        self.ready = 1

        for line in clines:
            label, data = c1D_line2labelNdata(line)
            if not label: continue

            verb_level = 3      # cutoff for verbose level

            try:        # to process some of the data
                if label == 'ni_type':
                    ncols, type = data.split('*')
                    ncols = int(ncols)
                    if self.verb > verb_level:
                        print "-- label %s: cols = %d, type = %s" % \
                              (label, ncols, type)
                    if ncols != self.ncols:
                        print "** matrix cols %d != %s cols %d" % \
                              (self.ncols, label, ncols)
                elif label == 'ni_dimen':
                    nrows = int(data)
                    if self.verb > verb_level:
                        print "-- label %s: rows = %d" % (label, nrows)
                    if nrows != self.nrows:
                        print "** matrix rows %d != %s rows %d" % \
                              (self.nrows, label, nrows)
                elif label == 'ColumnLabels':
                    self.labels = [str.strip() for str in data.split(';')]
                    if self.verb > verb_level:
                        print "-- label %s: labels = %s" % (label, self.labels)
                    if self.ncols != len(self.labels):
                        print "** %d ColumnLabels but %d columns" %     \
                              (len(self.labels), self.ncols)
                        self.labels = None
                elif label == 'ColumnGroups':
                    self.groups = UTIL.decode_1D_ints(data)
                    if self.groups:
                        if len(self.groups) != self.ncols:
                            print "** ColumnGroups len %d != ncols %d" % \
                                  (len(self.groups), self.ncols)
                    if self.verb > verb_level:
                        print "-- label %s: groups %s" % (label,self.groups)
                elif label == 'RowTR':
                    self.tr = float(data)
                    if self.verb > verb_level:
                        print "-- label %s: TR %s" % (label,self.tr)
                elif label == 'GoodList':
                    self.goodlist = UTIL.decode_1D_ints(data)
                    if self.goodlist:
                        if len(self.goodlist) != self.nrows:
                            print "** GoodList missing %d rows" % \
                                  self.nrows-len(self.goodlist)
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

