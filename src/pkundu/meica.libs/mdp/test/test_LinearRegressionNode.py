import py.test
from _tools import *

INDIM, OUTDIM, TLEN = 5, 3, 10000

def train_LRNode(inp, out, with_bias):
    lrnode = mdp.nodes.LinearRegressionNode(with_bias)
    for i in xrange(len(inp)):
        lrnode.train(inp[i], out[i])
    lrnode.stop_training()
    return lrnode

def test_LinearRegressionNode():
    # 1. first, without noise
    # 1a   without bias term
    # regression coefficients
    beta = numx_rand.uniform(-10., 10., size=(INDIM, OUTDIM))
    # input data
    x = numx_rand.uniform(-20., 20., size=(TLEN, INDIM))
    # output of the linear model
    y = mult(x, beta)
    # train
    lrnode = train_LRNode([x], [y], False)
    # test results
    assert_array_almost_equal(lrnode.beta, beta, decimal)
    res = lrnode(x)
    assert_array_almost_equal(res, y, decimal)

def test_LinearRegressionNode_with_bias():
    # 1b with bias
    beta = numx_rand.uniform(-10., 10., size=(INDIM+1, OUTDIM))
    x = numx_rand.uniform(-20., 20., size=(TLEN, INDIM))
    y = mult(x, beta[1:,:]) + beta[0,:]
    lrnode = train_LRNode([x], [y], True)
    assert_array_almost_equal(lrnode.beta, beta, decimal)
    res = lrnode(x)
    assert_array_almost_equal(res, y, decimal)

def test_LinearRegressionNode_with_noise():
    # 2. with noise, multiple sets of input
    beta = numx_rand.uniform(-10., 10., size=(INDIM+1, OUTDIM))
    x = numx_rand.uniform(-20., 20., size=(TLEN, INDIM))
    y = mult(x, beta[1:,:]) + beta[0,:]

    inp = [numx_rand.uniform(-20., 20., size=(TLEN, INDIM))
           for i in xrange(5)]
    out = [mult(x, beta[1:,:]) + beta[0,:] +
           numx_rand.normal(size=y.shape)*0.1 for x in inp]
    lrnode = train_LRNode(inp, out, True)
    assert_array_almost_equal(lrnode.beta, beta, 2)
    res = lrnode(inp[0])
    assert_array_almost_equal_diff(res, out[0], 2)

def test_LinearRegressionNode_raises_on_linearly_dependent_input():
    # 3. test error for linearly dependent input
    beta = numx_rand.uniform(-10., 10., size=(INDIM, OUTDIM))
    x = numx.linspace(-20,20,TLEN)
    x = mdp.utils.rrep(x, INDIM)
    x[:,-1] = 2.*x[:,0]
    y = mult(x, beta)
    py.test.raises(mdp.NodeException, train_LRNode, [x], [y], False)

def test_LinearRegressionNode_raises_on_wrong_output_size():
    # 4. test wrong output size
    beta = numx_rand.uniform(-10., 10., size=(INDIM, OUTDIM))
    x = numx_rand.uniform(-20., 20., size=(TLEN, INDIM))
    x[:,-1] = 2.*x[:,0]
    y = mult(x, beta)
    y = y[:10,:]
    py.test.raises(mdp.TrainingException, train_LRNode, [x], [y], False)
