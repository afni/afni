"""These are test functions for MDP utilities.
"""
import py.test
from _tools import *
from mdp import Node, nodes

class BogusClass(object):
    def __init__(self):
        self.x = numx_rand.random((2,2))

class BogusNode(Node):
    x = numx_rand.random((2,2))
    y = BogusClass()
    z = BogusClass()
    z.z = BogusClass()


def test_introspection():
    bogus = BogusNode()
    arrays, string = utils.dig_node(bogus)
    assert len(arrays.keys()) == 4, 'Not all arrays where caught'
    assert sorted(arrays.keys()) == ['x', 'y.x',
                                     'z.x', 'z.z.x'], 'Wrong names'
    sizes = [x[0] for x in arrays.values()]
    assert sorted(sizes) == [numx_rand.random((2,2)).itemsize*4]*4, \
           'Wrong sizes'
    sfa = nodes.SFANode()
    sfa.train(numx_rand.random((1000, 10)))
    a_sfa, string = utils.dig_node(sfa)
    keys = ['_cov_mtx._avg', '_cov_mtx._cov_mtx',
            '_dcov_mtx._avg', '_dcov_mtx._cov_mtx']
    assert sorted(a_sfa.keys()) == keys, 'Wrong arrays in SFANode'
    sfa.stop_training()
    a_sfa, string = utils.dig_node(sfa)
    keys = ['_bias', 'avg', 'd', 'davg', 'sf']
    assert sorted(a_sfa.keys()) == keys, 'Wrong arrays in SFANode'

def test_random_rot():
    dim = 20
    tlen = 10
    for i in xrange(tlen):
        x = utils.random_rot(dim, dtype='f')
        assert x.dtype.char=='f', 'Wrong dtype'
        y = utils.mult(x.T, x)
        assert_almost_equal(numx_linalg.det(x), 1., 4)
        assert_array_almost_equal(y, numx.eye(dim), 4)

def test_random_rot_determinant_sign():
    x = utils.random_rot(4)
    assert_almost_equal(numx_linalg.det(x), 1., 4)
    x = utils.random_rot(5)
    assert_almost_equal(numx_linalg.det(x), 1., 4)


def test_casting():
    x = numx_rand.random((5,3)).astype('d')
    y = 3*x
    assert_type_equal(y.dtype, x.dtype)
    x = numx_rand.random((5,3)).astype('f')
    y = 3.*x
    assert_type_equal(y.dtype, x.dtype)
    x = (10*numx_rand.random((5,3))).astype('i')
    y = 3.*x
    assert_type_equal(y.dtype, 'd')
    y = 3L*x
    assert_type_equal(y.dtype, 'i')
    x = numx_rand.random((5,3)).astype('f')
    y = 3L*x
    assert_type_equal(y.dtype, 'f')

def test_mult_diag():
    dim = 20
    d = numx_rand.random(size=(dim,))
    dd = numx.diag(d)
    mtx = numx_rand.random(size=(dim, dim))

    res1 = utils.mult(dd, mtx)
    res2 = utils.mult_diag(d, mtx, left=True)
    assert_array_almost_equal(res1, res2, 10)
    res1 = utils.mult(mtx, dd)
    res2 = utils.mult_diag(d, mtx, left=False)
    assert_array_almost_equal(res1, res2, 10)

def test_symeig_fake_integer():
    a = numx.array([[1,2],[2,7]])
    b = numx.array([[3,1],[1,5]])
    w,z = utils._symeig._symeig_fake(a)
    w,z = utils._symeig._symeig_fake(a,b)

def test_symeig_fake_LAPACK_bug():
    # bug. when input matrix is almost an identity matrix
    # but not exactly, the lapack dgeev routine returns a
    # matrix of eigenvectors which is not orthogonal.
    # this bug was present when we used numx_linalg.eig
    # instead of numx_linalg.eigh .
    # Note: this is a LAPACK bug.
    y = numx_rand.random((4,4))*1E-16
    y = (y+y.T)/2
    for i in xrange(4):
        y[i,i]=1
    val, vec = utils._symeig._symeig_fake(y)
    assert_almost_equal(abs(numx_linalg.det(vec)), 1., 12)

def test_QuadraticForm_extrema():
    # TODO: add some real test
    # check H with negligible linear term
    noise = 1e-8
    tol = 1e-6
    x = numx_rand.random((10,))
    H = numx.outer(x, x) + numx.eye(10)*0.1
    f = noise*numx_rand.random((10,))
    q = utils.QuadraticForm(H, f)
    xmax, xmin = q.get_extrema(utils.norm2(x), tol=tol)
    assert_array_almost_equal(x, xmax, 5)
    # check I + linear term
    H = numx.eye(10, dtype='d')
    f = x
    q = utils.QuadraticForm(H, f=f)
    xmax, xmin = q.get_extrema(utils.norm2(x), tol=tol)
    assert_array_almost_equal(f, xmax, 5)

def test_QuadraticForm_invariances():
    #nu = numx.linspace(2.,-3,10)
    nu = numx.linspace(6., 1, 10)
    H = utils.symrand(nu)
    E, W = mdp.utils.symeig(H)
    q = utils.QuadraticForm(H)
    xmax, xmin = q.get_extrema(5.)
    e_w, e_sd = q.get_invariances(xmax)
    #print e_sd,nu[1:]-nu[0]
    assert_array_almost_equal(e_sd,nu[1:]-nu[0],6)
    assert_array_almost_equal(abs(e_w),abs(W[:,-2::-1]),6)
    e_w, e_sd = q.get_invariances(xmin)
    assert_array_almost_equal(e_sd,nu[-2::-1]-nu[-1],6)
    assert_array_almost_equal(abs(e_w),abs(W[:,1:]),6)

def test_QuadraticForm_non_symmetric_raises():
    """Test the detection of non symmetric H!
    """
    H = numx_rand.random((10,10))
    py.test.raises(mdp.utils.QuadraticFormException,
                   utils.QuadraticForm, H)

def test_nongeneral_svd_bug():
    a = numx.array([[ 0.73083003,  0.        ,  0.7641788 ,  0.        ],
                    [ 0.        ,  0.        ,  0.        ,  0.        ],
                    [ 0.7641788 ,  0.        ,  0.79904932,  0.        ],
                    [ 0.        ,  0.        ,  0.        ,  0.        ]])
    w, z = utils.nongeneral_svd(a)
    diag = numx.diagonal(utils.mult(utils.hermitian(z),
                                    utils.mult(a, z))).real
    assert_array_almost_equal(diag, w, 12)
