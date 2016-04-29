from __future__ import with_statement
from _tools import *

TESTDECIMALS = {numx.dtype('d'): 12,
                numx.dtype('f'): 3,
                numx.dtype('D'): 12,
                numx.dtype('F'): 3,
                }

def test_eigenproblem(dtype, range, func):
    """Solve a standard eigenvalue problem."""
    dtype = numx.dtype(dtype)
    dim = 5
    if range:
        range = (2, dim -1)
    else:
        range = None
    a = utils.symrand(dim, dtype)+numx.diag([2.1]*dim).astype(dtype)
    w,z = func(a, range=range)
    # assertions
    assert_type_equal(z.dtype, dtype)
    w = w.astype(dtype)
    diag = numx.diagonal(utils.mult(utils.hermitian(z),
                                    utils.mult(a, z))).real
    assert_array_almost_equal(diag, w, TESTDECIMALS[dtype])

def test_geneigenproblem(dtype, range, func):
    """Solve a generalized eigenvalue problem."""
    dtype = numx.dtype(dtype)
    dim = 5
    if range:
        range = (2, dim -1)
    else:
        range = None
    a = utils.symrand(dim, dtype)
    b = utils.symrand(dim, dtype)+numx.diag([2.1]*dim).astype(dtype)
    w,z = func(a,b,range=range)
    # assertions
    assert z.dtype == dtype
    w = w.astype(dtype)
    diag1 = numx.diagonal(utils.mult(utils.hermitian(z),
                                     utils.mult(a, z))).real
    assert_array_almost_equal(diag1, w, TESTDECIMALS[dtype])
    diag2 = numx.diagonal(utils.mult(utils.hermitian(z),
                                     utils.mult(b, z))).real
    assert_array_almost_equal(diag2, numx.ones(diag2.shape[0]),
                              TESTDECIMALS[dtype])

test_geneigenproblem.funcs = [utils._symeig._symeig_fake]
if mdp.utils.symeig is utils._symeig.wrap_eigh:
    test_geneigenproblem.funcs.append(utils._symeig.wrap_eigh)

test_eigenproblem.funcs = test_geneigenproblem.funcs + [utils.nongeneral_svd]

def pytest_generate_tests(metafunc):
    for testtype in ('d', 'f'):
        for therange in (False, True):
            for func in metafunc.function.funcs:
                funcargs = dict(dtype=testtype,
                                range=therange,
                                func=func)
                theid = "%s, %s, %s" % (func.__name__, testtype, therange)
                metafunc.addcall(funcargs, id=theid)
