from _tools import *

def testPCANode():
    line_x = numx.zeros((1000,2),"d")
    line_y = numx.zeros((1000,2),"d")
    line_x[:,0] = numx.linspace(-1,1,num=1000,endpoint=1)
    line_y[:,1] = numx.linspace(-0.2,0.2,num=1000,endpoint=1)
    mat = numx.concatenate((line_x,line_y))
    des_var = std(mat,axis=0)
    utils.rotate(mat,uniform()*2*numx.pi)
    mat += uniform(2)
    pca = mdp.nodes.PCANode()
    pca.train(mat)
    act_mat = pca.execute(mat)
    assert_array_almost_equal(mean(act_mat,axis=0),\
                              [0,0],decimal)
    assert_array_almost_equal(std(act_mat,axis=0),\
                              des_var,decimal)
    # test that the total_variance attribute makes sense
    est_tot_var = ((des_var**2)*2000/1999.).sum()
    assert_almost_equal(est_tot_var, pca.total_variance, decimal)
    assert_almost_equal(1, pca.explained_variance, decimal)
    # test a bug in v.1.1.1, should not crash
    pca.inverse(act_mat[:,:1])

##     # test segmentation fault with symeig, see
##     # http://projects.scipy.org/scipy/numpy/ticket/551
##     def testPCANode_pickled():
##         for i in xrange(2,100):
##             mat, mix, inp = get_random_mix(mat_dim=(200, i))

##             pca = mdp.nodes.PCANode()
##             pca.train(mat)
##             s = cPickle.dumps(pca)
##             pca = cPickle.loads(s)
##             act_mat = pca.execute(mat)

def testPCANode_total_variance():
    mat, mix, inp = get_random_mix(mat_dim=(1000, 3))
    des_var = ((std(mat, axis=0)**2)*1000/999.).sum()
    pca = mdp.nodes.PCANode(output_dim=2)
    pca.train(mat)
    pca.execute(mat)
    assert_almost_equal(des_var, pca.total_variance, decimal)

def testPCANode_desired_variance():
    mat, mix, inp = get_random_mix(mat_dim=(1000, 3))
    # first make them white
    pca = mdp.nodes.WhiteningNode()
    pca.train(mat)
    mat = pca.execute(mat)
    # set the variances
    mat *= [0.6,0.3,0.1]
    #mat -= mat.mean(axis=0)
    pca = mdp.nodes.PCANode(output_dim=0.8)
    pca.train(mat)
    out = pca.execute(mat)
    # check that we got exactly two output_dim:
    assert pca.output_dim == 2, '%s'%pca.output_dim
    assert out.shape[1] == 2
    # check that explained variance is > 0.8 and < 1
    assert (pca.explained_variance > 0.8 and pca.explained_variance < 1)


def testPCANode_desired_variance_after_train():
    mat, mix, inp = get_random_mix(mat_dim=(1000, 3))
    # first make them white
    pca = mdp.nodes.WhiteningNode()
    pca.train(mat)
    mat = pca.execute(mat)
    # set the variances
    mat *= [0.6,0.3,0.1]
    #mat -= mat.mean(axis=0)
    pca = mdp.nodes.PCANode()
    pca.train(mat)
    # this was not working before the bug fix
    pca.output_dim = 0.8
    out = pca.execute(mat)
    # check that we got exactly two output_dim:
    assert pca.output_dim == 2
    assert out.shape[1] == 2
    # check that explained variance is > 0.8 and < 1
    assert (pca.explained_variance > 0.8 and pca.explained_variance < 1)

def testPCANode_range_argument():
    node = mdp.nodes.PCANode()
    x = numx.random.random((100,10))
    node.train(x)
    node.stop_training()
    y = node.execute(x, n=5)
    assert y.shape[1] == 5

def testPCANode_SVD():
    # it should pass atleast the same test as PCANode
    line_x = numx.zeros((1000,2),"d")
    line_y = numx.zeros((1000,2),"d")
    line_x[:,0] = numx.linspace(-1,1,num=1000,endpoint=1)
    line_y[:,1] = numx.linspace(-0.2,0.2,num=1000,endpoint=1)
    mat = numx.concatenate((line_x,line_y))
    des_var = std(mat,axis=0)
    utils.rotate(mat,uniform()*2*numx.pi)
    mat += uniform(2)
    pca = mdp.nodes.PCANode(svd=True)
    pca.train(mat)
    act_mat = pca.execute(mat)
    assert_array_almost_equal(mean(act_mat,axis=0),\
                              [0,0],decimal)
    assert_array_almost_equal(std(act_mat,axis=0),\
                              des_var,decimal)
    # Now a more difficult test, create singular cov matrices
    # and test that PCANode crashes whereas PCASVDNode doesn't
    mat, mix, inp = get_random_mix(mat_dim=(1000, 100), avg=1E+15)
    # now create a degenerate input
    for i in xrange(1,100):
        inp[:,i] = inp[:,1].copy()
    # check that standard PCA fails
    pca = mdp.nodes.PCANode()
    pca.train(inp)
    try:
        pca.stop_training()
        raise Exception, "PCANode didn't catch singular covariance matrix: degenerate"
    except mdp.NodeException:
        pass
    # now try the SVD version
    pca = mdp.nodes.PCANode(svd=True)
    pca.train(inp)
    pca.stop_training()

    # now check the undetermined case
    mat, mix, inp = get_random_mix(mat_dim=(500, 2))
    inp = inp.T
    pca = mdp.nodes.PCANode()
    pca.train(inp)
    try:
        pca.stop_training()
        raise Exception, "PCANode didn't catch singular covariance matrix: undetermined"
    except mdp.NodeException:
        pass
    # now try the SVD version
    pca = mdp.nodes.PCANode(svd=True)
    pca.train(inp)
    pca.stop_training()

    # try using the automatic dimensionality reduction function
    mat, mix, inp = get_random_mix(mat_dim=(1000, 3))
    # first make them decorellated
    pca = mdp.nodes.PCANode()
    pca.train(mat)
    mat = pca.execute(mat)
    mat *= [1E+5,1E-3, 1E-4]
    mat -= mat.mean(axis=0)
    pca = mdp.nodes.PCANode(svd=True,reduce=True, var_rel=1E-2)
    pca.train(mat)
    out = pca.execute(mat)
    # check that we got the only large dimension
    assert_array_almost_equal(mat[:,0].mean(axis=0),out.mean(axis=0),
                              decimal)
    assert_array_almost_equal(mat[:,0].std(axis=0),out.std(axis=0),
                              decimal)

    # second test for automatic dimansionality reduction
    # try using the automatic dimensionality reduction function
    mat, mix, inp = get_random_mix(mat_dim=(1000, 3))
    # first make them decorellated
    pca = mdp.nodes.PCANode()
    pca.train(mat)
    mat = pca.execute(mat)
    mat *= [1E+5,1E-3, 1E-18]
    mat -= mat.mean(axis=0)
    pca = mdp.nodes.PCANode(svd=True,reduce=True,
                            var_abs=1E-8, var_rel=1E-30)
    pca.train(mat)
    out = pca.execute(mat)
    # check that we got the only large dimension
    assert_array_almost_equal(mat[:,:2].mean(axis=0),out.mean(axis=0),
                              decimal)
    assert_array_almost_equal(mat[:,:2].std(axis=0),out.std(axis=0),
                              decimal)


def mock_symeig(x, range=None, overwrite=False):
    if range is None:
        N = x.shape[0]
    else:
        N = range[1]-range[0] + 1
    y = numx.zeros((N,))
    z = numx.zeros((N,N))
    y[0] = -1
    y[-1] = 1
    return y, z

def testPCANode_negative_eigenvalues():
    # should throw an Exception if reduce=False and
    # svd = False and output_dim=None
    pca = mdp.nodes.PCANode(output_dim=None, svd=False, reduce=False)
    pca._symeig = mock_symeig
    pca.train(uniform((10,10)))
    try:
        pca.stop_training()
        assert False, "PCA did not catch negative eigenvalues!"
    except mdp.NodeException, e:
        if "Got negative eigenvalues" in str(e):
            pass
        else:
            raise Exception("PCA did not catch negative eigenvalues!\n"+
                            str(e))
    # if reduce=True, should not throw any Exception,
    # and return output_dim = 1
    pca = mdp.nodes.PCANode(output_dim=None, svd=False, reduce=True)
    pca._symeig = mock_symeig
    pca.train(uniform((10,10)))
    pca.stop_training()
    assert pca.output_dim == 1, 'PCA did not remove non-positive eigenvalues!'
    # if svd=True, should not throw any Exception,
    # and return output_dim = 10
    pca = mdp.nodes.PCANode(output_dim=None, svd=True, reduce=False)
    pca._symeig = mock_symeig
    pca.train(uniform((10,10)))
    pca.stop_training()
    assert pca.output_dim == 10, 'PCA did not remove non-positive eigenvalues!'
    # if output_dim is set, should not throw any Exception,
    # and return the right output_dim
    pca = mdp.nodes.PCANode(output_dim=1, svd=False, reduce=False)
    pca._symeig = mock_symeig
    pca.train(uniform((10,10)))
    pca.stop_training()
    assert pca.output_dim == 1, 'PCA did not remove non-positive eigenvalues!'

def test_PCANode_no_eigenvalues_left():
    mat = numx.zeros((100,4), dtype='d')
    pca = mdp.nodes.PCANode(svd=True, reduce=True)
    pca.train(mat)
    py.test.raises(mdp.NodeException, 'pca.stop_training()')
    
