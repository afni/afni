from __future__ import with_statement
from _tools import *
mult = mdp.utils.mult

def testSFANode():
    dim=10000
    freqs = [2*numx.pi*1, 2*numx.pi*5]
    t =  numx.linspace(0,1,num=dim)
    mat = numx.array([numx.sin(freqs[0]*t), numx.sin(freqs[1]*t)]).T
    mat = ((mat - mean(mat[:-1,:], axis=0))
           / std(mat[:-1,:],axis=0))
    des_mat = mat.copy()
    mat = mult(mat,uniform((2,2))) + uniform(2)
    sfa = mdp.nodes.SFANode()
    sfa.train(mat)
    out = sfa.execute(mat)
    correlation = mult(des_mat[:-1,:].T,out[:-1,:])/(dim - 2)
    assert sfa.get_eta_values(t=0.5) is not None, 'get_eta is None'
    assert_array_almost_equal(abs(correlation),
                              numx.eye(2), decimal-3)
    sfa = mdp.nodes.SFANode(output_dim = 1)
    sfa.train(mat)
    out = sfa.execute(mat)
    assert out.shape[1]==1, 'Wrong output_dim'
    correlation = mult(des_mat[:-1,:1].T,out[:-1,:])/(dim - 2)
    assert_array_almost_equal(abs(correlation),
                              numx.eye(1), decimal - 3)

def testSFANode_range_argument():
    node = mdp.nodes.SFANode()
    x = numx.random.random((100,10))
    node.train(x)
    node.stop_training()
    y = node.execute(x, n=5)
    assert y.shape[1] == 5

def testSFANode_one_time_samples():
    # when training with x.shape = (1, n), stop_training
    # was failing with a ValueError: array must not contain infs or NaNs
    # because with only one samples no time difference can be computed and
    # the covmatrix is updated with zeros!
    node = mdp.nodes.SFANode()
    x = numx.random.random((1,5))
    with py.test.raises(mdp.TrainingException):
        node.train(x)

def testSFANode_include_last_sample():
    # check that the default behaviour is True
    node = mdp.nodes.SFANode()
    x = numx.random.random((100,10))
    node.train(x)
    node.stop_training()
    assert node.tlen == 100
    assert node.dtlen == 99

    # check that you can set it explicitly
    node = mdp.nodes.SFANode(include_last_sample=True)
    x = numx.random.random((100,10))
    node.train(x)
    node.stop_training()
    assert node.tlen == 100
    assert node.dtlen == 99

    # check the old behaviour
    node = mdp.nodes.SFANode(include_last_sample=False)
    x = numx.random.random((100,10))
    node.train(x)
    node.stop_training()
    assert node.tlen == 99
    assert node.dtlen == 99

    # check that we can change it during training
    node = mdp.nodes.SFANode(include_last_sample=False)
    x = numx.random.random((100,10))
    node.train(x, include_last_sample=True)
    node.stop_training()
    assert node.tlen == 100
    assert node.dtlen == 99
    
def testSFANode_derivative_bug1D():
    # one dimensional worst case scenario
    T = 100
    x = numx.zeros((T,1))
    x[0,:] =  -1.
    x[-1,:] = +1.
    x /= x.std(ddof=1)
    sfa = mdp.nodes.SFANode(include_last_sample=True)
    sfa.train(x)
    sfa.stop_training(debug=True)
    xdot = sfa.time_derivative(x)
    tlen = xdot.shape[0]
    correct_dcov_mtx = (xdot*xdot).sum()/(tlen-1)
    sfa_dcov_mtx = sfa.dcov_mtx
    # quantify the error
    error = abs(correct_dcov_mtx-sfa_dcov_mtx)[0,0]
    assert error < 10**(-decimal)
    # the bug was that we were calculating the covariance matrix
    # of the derivative, i.e.
    # sfa_dcov-mtx = (xdot*xdot).sum()/(tlen-1) - xdot.sum()**2/(tlen*(tlen-1))
    # so that the error in the estimated matrix was exactly
    # xdot.sum()**2/(tlen*(tlen-1))

def testSFANode_derivative_bug2D():
    T = 100
    x = numx.zeros((T,2))
    x[0,0] =  -1.
    x[-1,0] = +1.
    x[:,1] = numx.arange(T)
    x -= x.mean(axis=0)
    x /= x.std(ddof=1, axis=0)    
    sfa = mdp.nodes.SFANode(include_last_sample=True)
    sfa.train(x)
    sfa.stop_training(debug=True)
    xdot = sfa.time_derivative(x)
    tlen = xdot.shape[0]
    correct_dcov_mtx = mdp.utils.mult(xdot.T, xdot)/(tlen-1)
    sfa_dcov_mtx = sfa.dcov_mtx
    # the bug was that we were calculating the covariance matrix
    # of the derivative, i.e.
    # sfa_dcov_mtx = mdp.utils.mult(xdot.T, xdot)/(tlen-1) - \
    #                numx.outer(xdot.sum(axis=0),
    #                           xdot.sum(axis=0))/(tlen*(tlen-1)))
    # so that the error in the estimated matrix was exactly
    # numx.outer(xdot.sum(axis=0),xdot.sum(axis=0))/(tlen*(tlen-1))
    error = abs(correct_dcov_mtx-sfa_dcov_mtx)
    assert_array_almost_equal(numx.zeros(error.shape), error, decimal)

def testSFANode_derivative_bug2D_eigen():
    # this is a copy of the previous test, where we
    # quantify the error in the estimated eigenvalues
    # and eigenvectors
    T = 100
    x = numx.zeros((T,2))
    x[0,0] =  -1.
    x[-1,0] = +1.
    x[:,1] = numx.arange(T)
    x -= x.mean(axis=0)
    x /= x.std(ddof=1, axis=0)    
    sfa = mdp.nodes.SFANode(include_last_sample=True)
    sfa.train(x)
    sfa.stop_training(debug=True)
    xdot = sfa.time_derivative(x)
    tlen = xdot.shape[0]
    correct_dcov_mtx = mdp.utils.mult(xdot.T, xdot)/(tlen-1)
    eigvalues, eigvectors = sfa._symeig(correct_dcov_mtx,
                                        sfa.cov_mtx,
                                        range=None,
                                        overwrite=False)
    assert_array_almost_equal(eigvalues, sfa.d, decimal)
    assert_array_almost_equal(eigvectors, sfa.sf, decimal)
