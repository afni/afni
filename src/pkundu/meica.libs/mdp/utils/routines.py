import mdp

# import numeric module (scipy, Numeric or numarray)
numx, numx_rand, numx_linalg  = mdp.numx, mdp.numx_rand, mdp.numx_linalg
numx_description = mdp.numx_description
import random
import itertools

def timediff(data):
    """Returns the array of the time differences of data."""
    # this is the fastest way we found so far
    return data[1:]-data[:-1]

def refcast(array, dtype):
    """
    Cast the array to dtype only if necessary, otherwise return a reference.
    """
    dtype = mdp.numx.dtype(dtype)
    if array.dtype == dtype:
        return array
    return array.astype(dtype)

def scast(scalar, dtype):
    """Convert a scalar in a 0D array of the given dtype."""
    return numx.array(scalar, dtype=dtype)

def rotate(mat, angle, columns=(0, 1), units='radians'):
    """
    Rotate in-place data matrix (NxM) in the plane defined by the columns=[i,j]
    when observation are stored on rows. Observations are rotated
    counterclockwise. This corresponds to the following matrix-multiplication
    for each data-point (unchanged elements omitted):

     [  cos(angle) -sin(angle)     [ x_i ]
        sin(angle)  cos(angle) ] * [ x_j ]

    If M=2, columns=[0,1].
    """
    if units is 'degrees':
        angle = angle/180.*numx.pi
    cos_ = numx.cos(angle)
    sin_ = numx.sin(angle)
    [i, j] = columns
    col_i = mat[:, i] + 0.
    col_j = mat[:, j]
    mat[:, i] = cos_*col_i - sin_*col_j
    mat[:, j] = sin_*col_i + cos_*col_j

def permute(x, indices=(0, 0), rows=0, cols=1):
    """Swap two columns and (or) two rows of 'x', whose indices are specified
    in indices=[i,j].
    Note: permutations are done in-place. You'll lose your original matrix"""
    ## the nicer option:
    ## x[i,:],x[j,:] = x[j,:],x[i,:]
    ## does not work because array-slices are references.
    ## The following would work:
    ## x[i,:],x[j,:] = x[j,:].tolist(),x[i,:].tolist()
    ## because list-slices are copies, but you get 2
    ## copies instead of the one you need with our method.
    ## This would also work:
    ## tmp = x[i,:].copy()
    ## x[i,:],x[j,:] = x[j,:],tmp
    ## but it is slower (for larger matrices) than the one we use.
    [i, j] = indices
    if rows:
        x[i, :], x[j, :] = x[j, :], x[i, :] + 0
    if cols:
        x[:, i], x[:, j] = x[:, j], x[:, i] + 0

def hermitian(x):
    """Compute the Hermitian, i.e. conjugate transpose, of x."""
    return x.T.conj()

def symrand(dim_or_eigv, dtype="d"):
    """Return a random symmetric (Hermitian) matrix.

    If 'dim_or_eigv' is an integer N, return a NxN matrix, with eigenvalues
        uniformly distributed on (-1,1).

    If 'dim_or_eigv' is  1-D real array 'a', return a matrix whose
                      eigenvalues are 'a'.
    """
    if isinstance(dim_or_eigv, int):
        dim = dim_or_eigv
        d = (numx_rand.random(dim)*2) - 1
    elif isinstance(dim_or_eigv,
                    numx.ndarray) and len(dim_or_eigv.shape) == 1:
        dim = dim_or_eigv.shape[0]
        d = dim_or_eigv
    else:
        raise mdp.MDPException("input type not supported.")

    v = random_rot(dim)
    #h = mdp.utils.mult(mdp.utils.mult(hermitian(v), mdp.numx.diag(d)), v)
    h = mdp.utils.mult(mult_diag(d, hermitian(v), left=False), v)
    # to avoid roundoff errors, symmetrize the matrix (again)
    h = 0.5*(h.T+h)
    if dtype in ('D', 'F', 'G'):
        h2 = symrand(dim_or_eigv)
        h = h + 1j*(numx.triu(h2)-numx.tril(h2))
    return refcast(h, dtype)

def random_rot(dim, dtype='d'):
    """Return a random rotation matrix, drawn from the Haar distribution
    (the only uniform distribution on SO(n)).
    The algorithm is described in the paper
    Stewart, G.W., "The efficient generation of random orthogonal
    matrices with an application to condition estimators", SIAM Journal
    on Numerical Analysis, 17(3), pp. 403-409, 1980.
    For more information see
    http://en.wikipedia.org/wiki/Orthogonal_matrix#Randomization"""
    H = mdp.numx.eye(dim, dtype=dtype)
    D = mdp.numx.ones((dim,), dtype=dtype)
    for n in range(1, dim):
        x = mdp.numx_rand.normal(size=(dim-n+1,)).astype(dtype)
        D[n-1] = mdp.numx.sign(x[0])
        x[0] -= D[n-1]*mdp.numx.sqrt((x*x).sum())
        # Householder transformation
        Hx = ( mdp.numx.eye(dim-n+1, dtype=dtype)
               - 2.*mdp.numx.outer(x, x)/(x*x).sum() )
        mat = mdp.numx.eye(dim, dtype=dtype)
        mat[n-1:, n-1:] = Hx
        H = mdp.utils.mult(H, mat)
    # Fix the last sign such that the determinant is 1
    D[-1] = (-1)**(1-dim%2)*D.prod()
    # Equivalent to mult(numx.diag(D), H) but faster
    H = (D*H.T).T
    return H

def norm2(v):
    """Compute the 2-norm for 1D arrays.
    norm2(v) = sqrt(sum(v_i^2))"""

    return numx.sqrt((v*v).sum())

def cov2(x, y):
    """Compute the covariance between 2D matrices x and y.
    Complies with the old scipy.cov function: different variables
    are on different columns."""

    mnx = x.mean(axis=0)
    mny = y.mean(axis=0)
    tlen = x.shape[0]
    return mdp.utils.mult(x.T, y)/(tlen-1) - numx.outer(mnx, mny)

def cov_maxima(cov):
    """Extract the maxima of a covariance matrix."""
    dim = cov.shape[0]
    maxs = []
    if dim >= 1:
        cov=abs(cov)
        glob_max_idx = (cov.argmax()//dim, cov.argmax()%dim)
        maxs.append(cov[glob_max_idx[0], glob_max_idx[1]])
        cov_reduce = cov.copy()
        cov_reduce = cov_reduce[numx.arange(dim) != glob_max_idx[0], :]
        cov_reduce = cov_reduce[:, numx.arange(dim) != glob_max_idx[1]]
        maxs.extend(cov_maxima(cov_reduce))
        return maxs
    else:
        return []


def mult_diag(d, mtx, left=True):
    """Multiply a full matrix by a diagonal matrix.
    This function should always be faster than dot.

    Input:
      d -- 1D (N,) array (contains the diagonal elements)
      mtx -- 2D (N,N) array

    Output:
      mult_diag(d, mts, left=True) == dot(diag(d), mtx)
      mult_diag(d, mts, left=False) == dot(mtx, diag(d))
    """
    if left:
        return (d*mtx.T).T
    else:
        return d*mtx

def comb(N, k):
    """Return number of combinations of k objects from a set of N objects
    without repetitions, a.k.a. the binomial coefficient of N and k."""
    ret = 1
    for mlt in xrange(N, N-k, -1):
        ret *= mlt
    for dv in xrange(1, k+1):
        ret //= dv
    return ret

# WARNING numpy.linalg.eigh does not support float sizes larger than 64 bits,
# and complex numbers of size larger than 128 bits.
# Also float16 is not supported either.
# This is not a problem for MDP, as long as scipy.linalg.eigh is available.
def get_dtypes(typecodes_key, _safe=True):
    """Return the list of dtypes corresponding to the set of
    typecodes defined in numpy.typecodes[typecodes_key].
    E.g., get_dtypes('Float') = [dtype('f'), dtype('d'), dtype('g')].

    If _safe is True (default), we remove large floating point types
    if the numerical backend does not support them.
    """
    types = []
    for c in numx.typecodes[typecodes_key]:
        try:
            type_ = numx.dtype(c)
            if (_safe and not mdp.config.has_symeig == 'scipy.linalg.eigh'
                and type_ in _UNSAFE_DTYPES):
                continue
            types.append(type_)
        except TypeError:
            pass
    return types

_UNSAFE_DTYPES = [numx.typeDict[d] for d in
                  ['float16', 'float96', 'float128', 'complex192', 'complex256']
                  if d in numx.typeDict]

def nongeneral_svd(A, range=None, **kwargs):
    """SVD routine for simple eigenvalue problem, API is compatible with
    symeig."""
    Z2, w, Z = mdp.utils.svd(A)
    # sort eigenvalues and corresponding eigenvectors
    idx = w.argsort()
    w = w.take(idx)
    Z = Z.take(idx, axis=0).T
    if range is not None:
        lo, hi = range
        Z = Z[:, lo-1:hi]
        w = w[lo-1:hi]
    return w, Z

def sqrtm(A):
    """This is a symmetric definite positive matrix sqrt function"""
    d, V = mdp.utils.symeig(A)
    return mdp.utils.mult(V, mult_diag(numx.sqrt(d), V.T))

# replication functions
def lrep(x, n):
    """Replicate x n-times on a new first dimension"""
    shp = [1]
    shp.extend(x.shape)
    return x.reshape(shp).repeat(n, axis=0)

def rrep(x, n):
    """Replicate x n-times on a new last dimension"""
    shp = x.shape + (1,)
    return x.reshape(shp).repeat(n, axis=-1)

def irep(x, n, dim):
    """Replicate x n-times on a new dimension dim-th dimension"""
    x_shape = x.shape
    shp = x_shape[:dim] + (1,) + x_shape[dim:]
    return x.reshape(shp).repeat(n, axis=dim)
# /replication functions

try:
    # product exists only in itertools >= 2.6
    from itertools import product
except ImportError:
    def product(*args, **kwds):
        """Cartesian product of input iterables.
        """
        # taken from python docs 2.6
        # product('ABCD', 'xy') --> Ax Ay Bx By Cx Cy Dx Dy
        # product(range(2), repeat=3) --> 000 001 010 011 100 101 110 111
        pools = map(tuple, args) * kwds.get('repeat', 1)
        result = [[]]
        for pool in pools:
            result = [x+[y] for x in result for y in pool]
        for prod in result:
            yield tuple(prod)

def orthogonal_permutations(a_dict):
    """
    Takes a dictionary with lists as keys and returns all permutations
    of these list elements in new dicts.

    This function is useful, when a method with several arguments
    shall be tested and all of the arguments can take several values.

    The order is not defined, therefore the elements should be
    orthogonal to each other.

    >>> for i in orthogonal_permutations({'a': [1,2,3], 'b': [4,5]}):
            print i
    {'a': 1, 'b': 4}
    {'a': 1, 'b': 5}
    {'a': 2, 'b': 4}
    {'a': 2, 'b': 5}
    {'a': 3, 'b': 4}
    {'a': 3, 'b': 5}
    """
    pool = dict(a_dict)
    args = []
    for func, all_args in pool.items():
        # check the size of the list in the second item of the tuple
        args_with_fun = [(func, arg) for arg in all_args]
        args.append(args_with_fun)
    for i in product(*args):
        yield dict(i)


def izip_stretched(*iterables):
    """Same as izip, except that for convenience non-iterables are repeated ad infinitum.

    This is useful when trying to zip input data with respective labels
    and allows for having a single label for all data, as well as for
    havning a list of labels for each data vector.
    Note that this will take strings as an iterable (of course), so
    strings acting as a single value need to be wrapped in a repeat
    statement of their own.

    Thus,
    >>> for zipped in izip_stretched([1, 2, 3], -1):
            print zipped
    (1, -1)
    (2, -1)
    (3, -1)

    is equivalent to
    >>> for zipped in izip([1, 2, 3], [-1] * 3):
            print zipped
    (1, -1)
    (2, -1)
    (3, -1)
    """
    def iter_or_repeat(val):
        try:
            return iter(val)
        except TypeError:
            return itertools.repeat(val)

    iterables= map(iter_or_repeat, iterables)
    while iterables:
        # need to care about python < 2.6
        yield tuple([it.next() for it in iterables])


def weighted_choice(a_dict, normalize=True):
    """Returns a key from a dictionary based on the weight that the value suggests.
    If 'normalize' is False, it is assumed the weights sum up to unity. Otherwise,
    the algorithm will take care of normalising.

    Example:
    >>> d = {'a': 0.1, 'b': 0.5, 'c': 0.4}
    >>> weighted_choice(d)
    # draws 'b':'c':'a' with 5:4:1 probability

    TODO: It might be good to either shuffle the order or explicitely specify it,
    before walking through the items, to minimise possible degeneration.
    """
    if normalize:
        d = a_dict.copy()
        s = sum(d.values())
        for key, val in d.items():
            d[key] = d[key] / s
    else:
        d = a_dict
    rand_num = random.random()
    total_rand = 0
    for key, val in d.items():
        total_rand += val
        if total_rand > rand_num:
            return key
    return None

def bool_to_sign(an_array):
    """Return -1 for each False; +1 for each True"""
    return numx.sign(an_array - 0.5)

def sign_to_bool(an_array, zero=True):
    """Return False for each negative value, else True.

    The value for 0 is specified with 'zero'.
    """
    if zero:
        return numx.array(an_array) >= 0
    else:
        return numx.array(an_array) > 0

def gabor(size, alpha, phi, freq, sgm, x0=None, res=1, ampl=1.):
    """Return a 2D array containing a Gabor wavelet.

    Input arguments:
    size -- (height, width) (pixels)
    alpha -- orientation (rad)
    phi -- phase (rad)
    freq -- frequency (cycles/deg)
    sgm -- (sigma_x, sigma_y) standard deviation along the axis
           of the gaussian ellipse (pixel)
    x0 -- (x,y) coordinates of the center of the wavelet (pixel)
          Default: None, meaning the center of the array
    res -- spatial resolution (deg/pixel)
           Default: 1, so that 'freq' is measured in cycles/pixel
    ampl -- constant multiplying the result
            Default: 1.
    """

    # init
    w, h = size
    if x0 is None: x0 = (w//2, h//2)
    y0, x0 = x0

    # some useful quantities
    freq *= res
    sinalpha = numx.sin(alpha)
    cosalpha = numx.cos(alpha)
    v0, u0 = freq*cosalpha, freq*sinalpha

    # coordinates
    #x = numx.mgrid[-x0:w-x0, -y0:h-y0]
    x = numx.meshgrid(numx.arange(w)-x0, numx.arange(h)-y0)
    x = (x[0].T, x[1].T)
    xr = x[0]*cosalpha - x[1]*sinalpha
    yr = x[0]*sinalpha + x[1]*cosalpha

    # gabor
    im = ampl*numx.exp(-0.5*(xr*xr/(sgm[0]*sgm[0]) + yr*yr/(sgm[1]*sgm[1]))) \
             *numx.cos(-2.*numx.pi*(u0*x[0]+v0*x[1]) - phi)

    return im

def residuals(app_x, y_noisy, exp_funcs, x_orig, k=0.0):
    """Function used internally by invert_exp_funcs2 to approximate
    inverses in ConstantExpansionNode. """
    app_x = app_x.reshape((1,len(app_x)))
    app_exp_x =  numx.concatenate([func(app_x) for func in exp_funcs],axis=1)

    div_y = numx.sqrt(len(y_noisy))
    div_x = numx.sqrt(len(x_orig))
    return numx.append( (1-k)*(y_noisy-app_exp_x[0]) / div_y, k * (x_orig - app_x[0])/div_x )

def invert_exp_funcs2(exp_x_noisy, dim_x, exp_funcs, use_hint=False, k=0.0):
    """Approximates a preimage app_x of exp_x_noisy.

    Returns an array app_x, such that each row of exp_x_noisy is close
    to each row of exp_funcs(app_x).

    use_hint: determines the starting point for the approximation of the
    preimage. There are three possibilities.
    if it equals False: starting point is generated with a normal distribution
    if it equals True: starting point is the first dim_x elements of exp_x_noisy
    otherwise: use the parameter use_hint itself as the first approximation

    k: weighting factor in [0, 1] to balance between approximation error and
       closeness to the starting point. For instance:
       objective function is to minimize:
           (1-k) * |exp_funcs(app_x) - exp_x_noisy|/output_dim +
               k * |app_x - starting point|/input_dim

    Note: this function requires scipy.
    """
    if numx_description != 'scipy':
        raise NotImplementedError('This function requires scipy.')
    else:
        import scipy.optimize
    num_samples = exp_x_noisy.shape[0]

    if isinstance(use_hint, numx.ndarray):
        app_x = use_hint.copy()
    elif use_hint == True:
        app_x = exp_x_noisy[:,0:dim_x].copy()
    else:
        app_x = numx.random.normal(size=(num_samples,dim_x))

    for row in range(num_samples):
        plsq = scipy.optimize.leastsq(residuals, app_x[row],
                                      args=(exp_x_noisy[row], exp_funcs,
                                            app_x[row], k), maxfev=50*dim_x)
        app_x[row] = plsq[0]

    app_exp_x = numx.concatenate([func(app_x) for func in exp_funcs],axis=1)
    return app_x, app_exp_x
