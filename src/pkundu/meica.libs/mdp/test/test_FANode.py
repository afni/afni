from _tools import *

def test_FANode():
    d = 10
    N = 5000
    k = 4

    mu = uniform((1, d))*3.+2.
    sigma = uniform((d,))*0.01
    #A = utils.random_rot(d)[:k,:]
    A = numx_rand.normal(size=(k,d))

    # latent variables
    y = numx_rand.normal(0., 1., size=(N, k))
    # observations
    noise = numx_rand.normal(0., 1., size=(N, d)) * sigma

    x = mult(y, A) + mu + noise

    fa = mdp.nodes.FANode(output_dim=k, dtype='d')
    fa.train(x)
    fa.stop_training()

    # compare estimates to real parameters
    assert_array_almost_equal(fa.mu[0,:], mean(x, axis=0), 5)
    assert_array_almost_equal(fa.sigma, std(noise, axis=0)**2, 2)
    # FA finds A only up to a rotation. here we verify that the
    # A and its estimation span the same subspace
    AA = numx.concatenate((A,fa.A.T),axis=0)
    u,s,vh = utils.svd(AA)
    assert sum(s/max(s)>1e-2)==k, \
           'A and its estimation do not span the same subspace'

    y = fa.execute(x)
    fa.generate_input()
    fa.generate_input(10)
    fa.generate_input(y)
    fa.generate_input(y, noise=True)

    # test that noise has the right mean and variance
    est = fa.generate_input(numx.zeros((N, k)), noise=True)
    est -= fa.mu
    assert_array_almost_equal(numx.diag(numx.cov(est, rowvar=0)),
                              fa.sigma, 3)
    assert_almost_equal(numx.amax(abs(numx.mean(est, axis=0)), axis=None), 0., 3)

    est = fa.generate_input(100000)
    assert_array_almost_equal_diff(numx.cov(est, rowvar=0),
                                   mdp.utils.mult(fa.A, fa.A.T), 1)

def test_FANode_indim():
    # FANode uses two slightly different initialization for input_dims
    # larger or smaller than 200
    x = numx_rand.normal(size=(5000, 10))
    mdp.nodes.FANode(output_dim=1)(x)
    x = numx_rand.normal(size=(5000, 500))
    mdp.nodes.FANode(output_dim=1)(x)

def test_FANode_singular_cov():
    x = numx.array([[ 1., 1., 0., 0., 0.],
                    [ 0., 1., 1., 0., 0.],
                    [ 0., 1., 0., 0., 0.],
                    [ 0., 1., 1., 0., 0.],
                    [ 0., 1., 0., 0., 1.],
                    [ 0., 1., 0., 1., 0.],
                    [ 0., 1., 0., 0., 0.],
                    [ 1., 1., 0., 0., 0.],
                    [ 1., 1., 0., 0., 0.],
                    [ 0., 1., 0., 1., 1.]])



    fanode = mdp.nodes.FANode(output_dim=3)
    fanode.train(x)
    # the matrix x is singular
    py.test.raises(mdp.NodeException, "fanode.stop_training()")

