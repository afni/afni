import mdp
from mdp import numx, numx_linalg

class SymeigException(mdp.MDPException):
    pass

# the following functions and classes were part of the scipy_emulation.py file

_type_keys = ['f', 'd', 'F', 'D']
_type_conv = {('f','d'): 'd', ('f','F'): 'F', ('f','D'): 'D',
              ('d','F'): 'D', ('d','D'): 'D',
              ('F','d'): 'D', ('F','D'): 'D'}

def _greatest_common_dtype(alist):
    """
    Apply conversion rules to find the common conversion type
    dtype 'd' is default for 'i' or unknown types
    (known types: 'f','d','F','D').
    """
    dtype = 'f'
    for array in alist:
        if array is None:
            continue
        tc = array.dtype.char
        if tc not in _type_keys:
            tc = 'd'
        transition = (dtype, tc)
        if transition in _type_conv:
            dtype = _type_conv[transition]
    return dtype

def _assert_eigenvalues_real_and_positive(w, dtype):
    tol = numx.finfo(dtype.type).eps * 100
    if abs(w.imag).max() > tol:
        err = "Some eigenvalues have significant imaginary part: %s " % str(w)
        raise mdp.SymeigException(err)
    #if w.real.min() < 0:
    #    err = "Got negative eigenvalues: %s" % str(w)
    #    raise SymeigException(err)

def wrap_eigh(A, B = None, eigenvectors = True, turbo = "on", range = None,
              type = 1, overwrite = False):
    """Wrapper for scipy.linalg.eigh for scipy version > 0.7"""
    args = {}
    args['a'] = A
    args['b'] = B
    args['eigvals_only'] = not eigenvectors
    args['overwrite_a'] = overwrite
    args['overwrite_b'] = overwrite
    if turbo == "on":
        args['turbo'] = True
    else:
        args['turbo'] = False
    args['type'] = type
    if range is not None:
        n = A.shape[0]
        lo, hi = range
        if lo < 1:
            lo = 1
        if lo > n:
            lo = n
        if hi > n:
            hi = n
        if lo > hi:
            lo, hi = hi, lo
        # in scipy.linalg.eigh the range starts from 0
        lo -= 1
        hi -= 1
        range = (lo, hi)
    args['eigvals'] = range
    try:
        return numx_linalg.eigh(**args)
    except numx_linalg.LinAlgError, exception:
        raise SymeigException(str(exception))

def _symeig_fake(A, B = None, eigenvectors = True, turbo = "on", range = None,
                 type = 1, overwrite = False):
    """Solve standard and generalized eigenvalue problem for symmetric
(hermitian) definite positive matrices.
This function is a wrapper of LinearAlgebra.eigenvectors or
numarray.linear_algebra.eigenvectors with an interface compatible with symeig.

    Syntax:

      w,Z = symeig(A)
      w = symeig(A,eigenvectors=0)
      w,Z = symeig(A,range=(lo,hi))
      w,Z = symeig(A,B,range=(lo,hi))

    Inputs:

      A     -- An N x N matrix.
      B     -- An N x N matrix.
      eigenvectors -- if set return eigenvalues and eigenvectors, otherwise
                      only eigenvalues
      turbo -- not implemented
      range -- the tuple (lo,hi) represent the indexes of the smallest and
               largest (in ascending order) eigenvalues to be returned.
               1 <= lo < hi <= N
               if range = None, returns all eigenvalues and eigenvectors.
      type  -- not implemented, always solve A*x = (lambda)*B*x
      overwrite -- not implemented

    Outputs:

      w     -- (selected) eigenvalues in ascending order.
      Z     -- if range = None, Z contains the matrix of eigenvectors,
               normalized as follows:
                  Z^H * A * Z = lambda and Z^H * B * Z = I
               where ^H means conjugate transpose.
               if range, an N x M matrix containing the orthonormal
               eigenvectors of the matrix A corresponding to the selected
               eigenvalues, with the i-th column of Z holding the eigenvector
               associated with w[i]. The eigenvectors are normalized as above.
    """

    dtype = numx.dtype(_greatest_common_dtype([A, B]))
    try:
        if B is None:
            w, Z = numx_linalg.eigh(A)
        else:
            # make B the identity matrix
            wB, ZB = numx_linalg.eigh(B)
            _assert_eigenvalues_real_and_positive(wB, dtype)
            ZB = ZB.real / numx.sqrt(wB.real)
            # transform A in the new basis: A = ZB^T * A * ZB
            A = mdp.utils.mult(mdp.utils.mult(ZB.T, A), ZB)
            # diagonalize A
            w, ZA = numx_linalg.eigh(A)
            Z = mdp.utils.mult(ZB, ZA)
    except numx_linalg.LinAlgError, exception:
        raise SymeigException(str(exception))

    _assert_eigenvalues_real_and_positive(w, dtype)
    w = w.real
    Z = Z.real

    idx = w.argsort()
    w = w.take(idx)
    Z = Z.take(idx, axis=1)

    # sanitize range:
    n = A.shape[0]
    if range is not None:
        lo, hi = range
        if lo < 1:
            lo = 1
        if lo > n:
            lo = n
        if hi > n:
            hi = n
        if lo > hi:
            lo, hi = hi, lo

        Z = Z[:, lo-1:hi]
        w = w[lo-1:hi]

    # the final call to refcast is necessary because of a bug in the casting
    # behavior of Numeric and numarray: eigenvector does not wrap the LAPACK
    # single precision routines
    if eigenvectors:
        return mdp.utils.refcast(w, dtype), mdp.utils.refcast(Z, dtype)
    else:
        return mdp.utils.refcast(w, dtype)

