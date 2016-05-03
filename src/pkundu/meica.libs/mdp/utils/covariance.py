import mdp
import warnings

# import numeric module (scipy, Numeric or numarray)
numx = mdp.numx

def _check_roundoff(t, dtype):
    """Check if t is so large that t+1 == t up to 2 precision digits"""
    # limit precision
    limit = 10.**(numx.finfo(dtype).precision-2)
    if int(t) >= limit:
        wr = ('You have summed %e entries in the covariance matrix.'
              '\nAs you are using dtype \'%s\', you are '
              'probably getting severe round off'
              '\nerrors. See CovarianceMatrix docstring for more'
              ' information.' % (t, dtype.name))
        warnings.warn(wr, mdp.MDPWarning)

class CovarianceMatrix(object):
    """This class stores an empirical covariance matrix that can be updated
    incrementally. A call to the 'fix' method returns the current state of
    the covariance matrix, the average and the number of observations, and
    resets the internal data.

    Note that the internal sum is a standard __add__ operation. We are not
    using any of the fancy sum algorithms to avoid round off errors when
    adding many numbers. If you want to contribute a CovarianceMatrix class
    that uses such algorithms we would be happy to include it in MDP.
    For a start see the Python recipe by Raymond Hettinger at
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/393090
    For a review about floating point arithmetic and its pitfalls see
    http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
    """

    def __init__(self, dtype=None, bias=False):
        """If dtype is not defined, it will be inherited from the first
        data bunch received by 'update'.
        All the matrices in this class are set up with the given dtype and
        no upcast is possible.
        If bias is True, the covariance matrix is normalized by dividing
        by T instead of the usual T-1.
        """
        if dtype is None:
            self._dtype = None
        else:
            self._dtype = numx.dtype(dtype)
        self._input_dim = None  # will be set in _init_internals
        # covariance matrix, updated during the training phase
        self._cov_mtx = None
        # average, updated during the training phase
        self._avg = None
        # number of observation so far during the training phase
        self._tlen = 0

        self.bias = bias

    def _init_internals(self, x):
        """Init the internal structures.

        The reason this is not done in the constructor is that we want to be
        able to derive the input dimension and the dtype directly from the
        data this class receives.
        """
        # init dtype
        if self._dtype is None:
            self._dtype = x.dtype
        dim = x.shape[1]
        self._input_dim = dim
        type_ = self._dtype
        # init covariance matrix
        self._cov_mtx = numx.zeros((dim, dim), type_)
        # init average
        self._avg = numx.zeros(dim, type_)

    def update(self, x):
        """Update internal structures.

        Note that no consistency checks are performed on the data (this is
        typically done in the enclosing node).
        """
        if self._cov_mtx is None:
            self._init_internals(x)
        # cast input
        x = mdp.utils.refcast(x, self._dtype)
        # update the covariance matrix, the average and the number of
        # observations (try to do everything inplace)
        self._cov_mtx += mdp.utils.mult(x.T, x)
        self._avg += x.sum(axis=0)
        self._tlen += x.shape[0]

    def fix(self, center=True):
        """Returns a triple containing the covariance matrix, the average and
        the number of observations. The covariance matrix is then reset to
        a zero-state.

        If center is false, the returned matrix is the matrix of the second moments,
        i.e. the covariance matrix of the data without subtracting the mean."""
        # local variables
        type_ = self._dtype
        tlen = self._tlen
        _check_roundoff(tlen, type_)
        avg = self._avg
        cov_mtx = self._cov_mtx

        ##### fix the training variables
        # fix the covariance matrix (try to do everything inplace)
        if self.bias:
            cov_mtx /= tlen
        else:
            cov_mtx /= tlen - 1

        if center:
            avg_mtx = numx.outer(avg, avg)
            if self.bias:
                avg_mtx /= tlen*(tlen)
            else:
                avg_mtx /= tlen*(tlen - 1)
            cov_mtx -= avg_mtx

        # fix the average
        avg /= tlen

        ##### clean up
        # covariance matrix, updated during the training phase
        self._cov_mtx = None
        # average, updated during the training phase
        self._avg = None
        # number of observation so far during the training phase
        self._tlen = 0

        return cov_mtx, avg, tlen


class DelayCovarianceMatrix(object):
    """This class stores an empirical covariance matrix between the signal and
    time delayed signal that can be updated incrementally.

    Note that the internal sum is a standard __add__ operation. We are not
    using any of the fancy sum algorithms to avoid round off errors when
    adding many numbers. If you want to contribute a CovarianceMatrix class
    that uses such algorithms we would be happy to include it in MDP.
    For a start see the Python recipe by Raymond Hettinger at
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/393090
    For a review about floating point arithmetic and its pitfalls see
    http://docs.sun.com/source/806-3568/ncg_goldberg.html
    """

    def __init__(self, dt, dtype=None, bias=False):
        """dt is the time delay. If dt==0, DelayCovarianceMatrix equals
        CovarianceMatrix. If dtype is not defined, it will be inherited from
        the first data bunch received by 'update'.
        All the matrices in this class are set up with the given dtype and
        no upcast is possible.
        If bias is True, the covariance matrix is normalized by dividing
        by T instead of the usual T-1.
        """

        # time delay
        self._dt = int(dt)

        if dtype is None:
            self._dtype = None
        else:
            self._dtype = numx.dtype(dtype)

        # clean up variables to spare on space
        self._cov_mtx = None
        self._avg = None
        self._avg_dt = None
        self._tlen = 0

        self.bias = bias

    def _init_internals(self, x):
        """Inits some internals structures. The reason this is not done in
        the constructor is that we want to be able to derive the input
        dimension and the dtype directly from the data this class receives.
        """

        # init dtype
        if self._dtype is None:
            self._dtype = x.dtype
        dim = x.shape[1]
        self._input_dim = dim
        # init covariance matrix
        self._cov_mtx = numx.zeros((dim, dim), self._dtype)
        # init averages
        self._avg = numx.zeros(dim, self._dtype)
        self._avg_dt = numx.zeros(dim, self._dtype)

    def update(self, x):
        """Update internal structures."""
        if self._cov_mtx is None:
            self._init_internals(x)

        # cast input
        x = mdp.utils.refcast(x, self._dtype)

        dt = self._dt

        # the number of data points in each block should be at least dt+1
        tlen = x.shape[0]
        if tlen < (dt+1):
            err = 'Block length is %d, should be at least %d.' % (tlen, dt+1)
            raise mdp.MDPException(err)

        # update the covariance matrix, the average and the number of
        # observations (try to do everything inplace)
        self._cov_mtx += mdp.utils.mult(x[:tlen-dt, :].T, x[dt:tlen, :])
        totalsum = x.sum(axis=0)
        self._avg += totalsum - x[tlen-dt:, :].sum(axis=0)
        self._avg_dt += totalsum - x[:dt, :].sum(axis=0)
        self._tlen += tlen-dt

    def fix(self, A=None):
        """The collected data is adjusted to compute the covariance matrix of
        the signal x(1)...x(N-dt) and the delayed signal x(dt)...x(N),
        which is defined as <(x(t)-<x(t)>)*(x(t+dt)-<x(t+dt)>)> .
        The function returns a tuple containing the covariance matrix,
        the average <x(t)> over the first N-dt points, the average of the
        delayed signal <x(t+dt)> and the number of observations. The internal
        data is then reset to a zero-state.

        If A is defined, the covariance matrix is transformed by the linear
        transformation Ax . E.g. to whiten the data, A is the whitening matrix.
        """

        # local variables
        type_ = self._dtype
        tlen = self._tlen
        _check_roundoff(tlen, type_)
        avg = self._avg
        avg_dt = self._avg_dt
        cov_mtx = self._cov_mtx

        ##### fix the training variables
        # fix the covariance matrix (try to do everything inplace)
        avg_mtx = numx.outer(avg, avg_dt)
        avg_mtx /= tlen

        cov_mtx -= avg_mtx
        if self.bias:
            cov_mtx /= tlen
        else:
            cov_mtx /= tlen - 1

        if A is not None:
            cov_mtx = mdp.utils.mult(A, mdp.utils.mult(cov_mtx, A.T))

        # fix the average
        avg /= tlen
        avg_dt /= tlen

        ##### clean up variables to spare on space
        self._cov_mtx = None
        self._avg = None
        self._avg_dt = None
        self._tlen = 0

        return cov_mtx, avg, avg_dt, tlen


class MultipleCovarianceMatrices(object):
    """Container class for multiple covariance matrices to easily
    execute operations on all matrices at the same time.
    Note: all operations are done in place where possible."""
    def __init__(self, covs):
        """Insantiate with a sequence of covariance matrices."""
        # swap axes to get the different covmat on to the 3rd axis
        self.dtype = covs[0].dtype
        self.covs = (numx.array(covs, dtype=self.dtype)).transpose([1, 2, 0])
        self.ncovs = len(covs)

    def __getitem__(self, item):
        return self.covs[:, :, item]

    def symmetrize(self):
        """Symmetrize matrices: C -> (C+C^T)/2 ."""
        # symmetrize cov matrices
        covs = self.covs
        covs = 0.5*(covs+covs.transpose([1, 0, 2]))
        self.covs = covs

    def weight(self, weights):
        """Apply a weighting factor to matrices.
        Argument can be a sequence or a single value. In the latter case
        the same weight is applied to all matrices."""
        # apply a weighting vector to cov matrices
        err = ("len(weights)=%d does not match number "
               "of matrices (%d)" % (len(weights), self.ncovs))
        assert len(weights) == self.ncovs, err
        self.covs *= mdp.utils.refcast(weights, self.dtype)

    def rotate(self, angle, indices):
        """Rotate matrices by angle in the plane defined by indices [i,j]."""
        covs = self.covs
        [i, j] = indices
        cos_ = numx.cos(angle)
        sin_ = numx.sin(angle)
        # rotate columns
        # you need to copy the first column that is modified
        covs_i = covs[:, i, :] + 0
        covs_j = covs[:, j, :]
        covs[:, i, :] =  cos_*covs_i - sin_*covs_j
        covs[:, j, :] =  sin_*covs_i + cos_*covs_j
        # rotate rows
        # you need to copy the first row that is modified
        covs_i = covs[i, :, :] + 0
        covs_j = covs[j, :, :]
        covs[i, :, :] =  cos_*covs_i - sin_*covs_j
        covs[j, :, :] =  sin_*covs_i + cos_*covs_j
        self.covs = covs

    def permute(self, indices):
        """Swap two columns and two rows of all matrices, whose indices are
        specified as [i,j]."""
        covs = self.covs
        [i, j] = indices
        covs[i, :, :], covs[j, :, :] = covs[j, :, :], covs[i, :, :] + 0
        covs[:, i, :], covs[:, j, :] = covs[:, j, :], covs[:, i, :] + 0
        self.covs = covs

    def transform(self, trans_matrix):
        """Apply a linear transformation to all matrices, defined by the
        transformation matrix."""
        trans_matrix = mdp.utils.refcast(trans_matrix, self.dtype)
        for cov in range(self.ncovs):
            self.covs[:, :, cov] = mdp.utils.mult(
                mdp.utils.mult(trans_matrix.T, self.covs[:, :, cov]),
                trans_matrix)

    def copy(self):
        """Return a deep copy of the instance."""
        return MultipleCovarianceMatrices(self.covs.transpose([2, 0, 1]))


class CrossCovarianceMatrix(CovarianceMatrix):

    def _init_internals(self, x, y):
        if self._dtype is None:
            self._dtype = x.dtype
            if y.dtype != x.dtype:
                err = 'dtype mismatch: x (%s) != y (%s)'%(x.dtype,
                                                          y.dtype)
                raise mdp.MDPException(err)
        dim_x = x.shape[1]
        dim_y = y.shape[1]
        type_ = self._dtype
        self._cov_mtx = numx.zeros((dim_x, dim_y), type_)
        self._avgx = numx.zeros(dim_x, type_)
        self._avgy = numx.zeros(dim_y, type_)


    def update(self, x, y):
        # check internal dimensions consistency
        if x.shape[0] != y.shape[0]:
            err = '# samples mismatch: x (%d) != y (%d)'%(x.shape[0],
                                                          y.shape[0])
            raise mdp.MDPException(err)

        if self._cov_mtx is None:
            self._init_internals(x, y)

        # cast input
        x = mdp.utils.refcast(x, self._dtype)
        y = mdp.utils.refcast(y, self._dtype)

        self._cov_mtx += mdp.utils.mult(x.T, y)
        self._avgx += x.sum(axis=0)
        self._avgy += y.sum(axis=0)
        self._tlen += x.shape[0]

    def fix(self):
        type_ = self._dtype
        tlen = self._tlen
        _check_roundoff(tlen, type_)
        avgx = self._avgx
        avgy = self._avgy
        cov_mtx = self._cov_mtx

        ##### fix the training variables
        # fix the covariance matrix (try to do everything inplace)
        avg_mtx = numx.outer(avgx, avgy)

        if self.bias:
            avg_mtx /= tlen*(tlen)
            cov_mtx /= tlen
        else:
            avg_mtx /= tlen*(tlen - 1)
            cov_mtx /= tlen - 1
        cov_mtx -= avg_mtx
        # fix the average
        avgx /= tlen
        avgy /= tlen

        ##### clean up
        # covariance matrix, updated during the training phase
        self._cov_mtx = None
        # average, updated during the training phase
        self._avgx = None
        self._avgy = None
        # number of observation so far during the training phase
        self._tlen = 0

        return cov_mtx, avgx, avgy, tlen
