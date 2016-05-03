"""Tools for the test- and benchmark functions."""

import sys
import time
import itertools
from functools import wraps

import py.test

import mdp
from mdp import numx, numx_rand, numx_fft, numx_linalg, utils
from numpy.testing import (assert_array_equal, assert_array_almost_equal,
                           assert_equal, assert_almost_equal)

mean = numx.mean
std = numx.std
normal = mdp.numx_rand.normal
uniform = mdp.numx_rand.random
testtypes = [numx.dtype('d'), numx.dtype('f')]
testtypeschar = [t.char for t in testtypes]
testdecimals = {testtypes[0]: 12, testtypes[1]: 6}
decimal = 7
mult = mdp.utils.mult

#### test tools
def assert_array_almost_equal_diff(x,y,digits,err_msg=''):
    x,y = numx.asarray(x), numx.asarray(y)
    msg = '\nArrays are not almost equal'
    assert 0 in [len(numx.shape(x)),len(numx.shape(y))] \
           or (len(numx.shape(x))==len(numx.shape(y)) and \
               numx.alltrue(numx.equal(numx.shape(x),numx.shape(y)))),\
               msg + ' (shapes %s, %s mismatch):\n\t' \
               % (numx.shape(x),numx.shape(y)) + err_msg
    maxdiff = max(numx.ravel(abs(x-y)))/\
              max(max(abs(numx.ravel(x))),max(abs(numx.ravel(y))))
    if numx.iscomplexobj(x) or numx.iscomplexobj(y): maxdiff = maxdiff/2
    cond =  maxdiff< 10**(-digits)
    msg = msg+'\n\t Relative maximum difference: %e'%(maxdiff)+'\n\t'+\
          'Array1: '+str(x)+'\n\t'+\
          'Array2: '+str(y)+'\n\t'+\
          'Absolute Difference: '+str(abs(y-x))
    assert cond, msg

def assert_type_equal(act, des):
    assert act == numx.dtype(des), \
           'dtype mismatch: "%s" (should be "%s") '%(act,des)

def get_random_mix(mat_dim = None, type = "d", scale = 1,\
                    rand_func = uniform, avg = 0, \
                    std_dev = 1):
    if mat_dim is None:
        mat_dim = (500, 5)
    T = mat_dim[0]
    N = mat_dim[1]
    d = 0
    while d < 1E-3:
        #mat = ((rand_func(size=mat_dim)-0.5)*scale).astype(type)
        mat = rand_func(size=(T,N)).astype(type)
        # normalize
        mat -= mean(mat,axis=0)
        mat /= std(mat,axis=0)
        # check that the minimum eigenvalue is finite and positive
        d1 = min(mdp.utils.symeig(mdp.utils.mult(mat.T, mat), eigenvectors = 0))
        if std_dev is not None: mat *= std_dev
        if avg is not None: mat += avg
        mix = (rand_func(size=(N,N))*scale).astype(type)
        matmix = mdp.utils.mult(mat,mix)
        matmix_n = matmix - mean(matmix, axis=0)
        matmix_n /= std(matmix_n, axis=0)
        d2 = min(mdp.utils.symeig(mdp.utils.mult(matmix_n.T,matmix_n),
                                  eigenvectors=0))
        d = min(d1, d2)
    return mat, mix, matmix

def verify_ICANode(icanode, rand_func = uniform, vars=3, N=8000, prec=3):
    dim = (N, vars)
    mat,mix,inp = get_random_mix(rand_func=rand_func,mat_dim=dim)
    icanode.train(inp)
    act_mat = icanode.execute(inp)
    cov = mdp.utils.cov2((mat-mean(mat,axis=0))/std(mat,axis=0), act_mat)
    maxima = numx.amax(abs(cov), axis=0)
    assert_array_almost_equal(maxima,numx.ones(vars), prec)

def verify_ICANodeMatrices(icanode, rand_func=uniform, vars=3, N=8000):
    dim = (N, vars)
    mat,mix,inp = get_random_mix(rand_func=rand_func, mat_dim=dim, avg=0)
    icanode.train(inp)
    # test projection matrix
    act_mat = icanode.execute(inp)
    T = icanode.get_projmatrix()
    exp_mat = mdp.utils.mult(inp, T)
    assert_array_almost_equal(act_mat,exp_mat,6)
    # test reconstruction matrix
    out = act_mat.copy()
    act_mat = icanode.inverse(out)
    B = icanode.get_recmatrix()
    exp_mat = mdp.utils.mult(out, B)
    assert_array_almost_equal(act_mat,exp_mat,6)

class BogusNode(mdp.Node):
    @staticmethod
    def is_trainable(): return False
    def _execute(self,x): return 2*x
    def _inverse(self,x): return 0.5*x

class BogusNodeTrainable(mdp.Node):
    def _train(self, x):
        pass
    def _stop_training(self):
        self.bogus_attr = 1

class BogusExceptNode(mdp.Node):
    def _train(self,x):
        self.bogus_attr = 1
        raise Exception, "Bogus Exception"

    def _execute(self,x):
        raise Exception, "Bogus Exception"

class BogusMultiNode(mdp.Node):

    def __init__(self):
        super(BogusMultiNode, self).__init__()
        self.visited = []

    def _get_train_seq(self):
        return [(self.train1, self.stop1),
                (self.train2, self.stop2)]

    def train1(self, x):
        self.visited.append(1)
    def stop1(self):
        self.visited.append(2)
    def train2(self, x):
        self.visited.append(3)
    def stop2(self):
        self.visited.append(4)

#_spinner = itertools.cycle((' /\b\b', ' -\b\b', ' \\\b\b', ' |\b\b'))
_spinner = itertools.cycle((' .\b\b', ' o\b\b', ' 0\b\b', ' O\b\b',
                            ' 0\b\b', ' o\b\b'))
#_spinner = itertools.cycle([" '\b\b"]*2 + [' !\b\b']*2 + [' .\b\b']*2 +
#                           [' !\b\b']*2)

def spinner():
    sys.stderr.write(_spinner.next())
    sys.stderr.flush()

class skip_on_condition(object):
    """Skip a test if the eval(condition_str, namespace) returns True.

    namespace contains sys, os, and the mdp module.
    """
    def __init__(self, condition_str, skipping_msg=None):
        self.condition_str = condition_str
        if skipping_msg is None:
            self.skipping_msg = "Condition %s not met." % condition_str
        else:
            self.skipping_msg = skipping_msg

    def __call__(self, f):
        import sys, os
        @wraps(f)
        def wrapped_f(*args, **kwargs):
            namespace = {'sys': sys,
                         'os': os,
                         'mdp': mdp}
            if eval(self.condition_str, namespace):
                py.test.skip(self.skipping_msg)
            f(*args, **kwargs)
        return wrapped_f
