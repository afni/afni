"""These are test functions for MDP contributed nodes.
"""

from _tools import *
from test_ICANode import verify_ICANode, verify_ICANodeMatrices

requires_joblib = skip_on_condition(
    "not mdp.config.has_caching",
    "This test requires the 'joblib' module.")

def _s_shape(theta):
    """
    returns x,y
      a 2-dimensional S-shaped function
      for theta ranging from 0 to 1
    """
    t = 3*numx.pi * (theta-0.5)
    x = numx.sin(t)
    y = numx.sign(t)*(numx.cos(t)-1)
    return x,y

def _s_shape_1D(n):
    t = numx.linspace(0., 1., n)
    x, z = _s_shape(t)
    y = numx.linspace(0., 5., n)
    return x, y, z, t

def _s_shape_2D(nt, ny):
    t, y = numx.meshgrid(numx.linspace(0., 1., nt),
                         numx.linspace(0., 2., ny))
    t = t.flatten()
    y = y.flatten()
    x, z = _s_shape(t)
    return x, y, z, t

def _compare_neighbors(orig, proj, k):
    n = orig.shape[0]
    err = numx.zeros((n,))
    # compare neighbors indices
    for i in xrange(n):
        # neighbors in original space
        dist = orig - orig[i,:]
        orig_nbrs = numx.argsort((dist**2).sum(1))[1:k+1]
        orig_nbrs.sort()
        # neighbors in projected space
        dist = proj - proj[i,:]
        proj_nbrs = numx.argsort((dist**2).sum(1))[1:k+1]
        proj_nbrs.sort()
        for idx in orig_nbrs:
            if idx not in proj_nbrs:
                err[i] += 1
    return err

def test_JADENode():
    trials = 3
    for i in xrange(trials):
        try:
            ica = mdp.nodes.JADENode(limit = 10**(-decimal))
            ica2 = ica.copy()
            verify_ICANode(ica, rand_func=numx_rand.exponential)
            verify_ICANodeMatrices(ica2)
            return
        except Exception:
            if i == trials - 1:
                raise

def test_NIPALSNode():
    line_x = numx.zeros((1000,2),"d")
    line_y = numx.zeros((1000,2),"d")
    line_x[:,0] = numx.linspace(-1,1,num=1000,endpoint=1)
    line_y[:,1] = numx.linspace(-0.2,0.2,num=1000,endpoint=1)
    mat = numx.concatenate((line_x,line_y))
    des_var = std(mat,axis=0)
    utils.rotate(mat,uniform()*2*numx.pi)
    mat += uniform(2)
    pca = mdp.nodes.NIPALSNode(conv=1E-15, max_it=1000)
    pca.train(mat)
    act_mat = pca.execute(mat)
    assert_array_almost_equal(mean(act_mat,axis=0),\
                              [0,0],decimal)
    assert_array_almost_equal(std(act_mat,axis=0),\
                              des_var,decimal)
    # test a bug in v.1.1.1, should not crash
    pca.inverse(act_mat[:,:1])
    # try standard PCA on the same data and compare the eigenvalues
    pca2 = mdp.nodes.PCANode()
    pca2.train(mat)
    pca2.stop_training()
    assert_array_almost_equal(pca2.d, pca.d, decimal)

def test_NIPALSNode_desired_variance():
    mat, mix, inp = get_random_mix(mat_dim=(1000, 3))
    # first make them white
    pca = mdp.nodes.WhiteningNode()
    pca.train(mat)
    mat = pca.execute(mat)
    # set the variances
    mat *= [0.6,0.3,0.1]
    #mat -= mat.mean(axis=0)
    pca = mdp.nodes.NIPALSNode(output_dim=0.8)
    pca.train(mat)
    out = pca.execute(mat)
    # check that we got exactly two output_dim:
    assert pca.output_dim == 2
    assert out.shape[1] == 2
    # check that explained variance is > 0.8 and < 1
    assert (pca.explained_variance > 0.8 and pca.explained_variance < 1)

def test_LLENode():
    # 1D S-shape in 3D
    n, k = 50, 2
    x, y, z, t = _s_shape_1D(n)
    data = numx.asarray([x,y,z]).T

    res = mdp.nodes.LLENode(k, output_dim=1, svd=False)(data)
    # check that the neighbors are the same
    err = _compare_neighbors(data, res, k)
    assert err.max() == 0

    # with svd=True
    res = mdp.nodes.LLENode(k, output_dim=1, svd=True)(data)
    err = _compare_neighbors(data, res, k)
    assert err.max() == 0
    return

    #TODO: fix this test!
    # 2D S-shape in 3D
    nt, ny = 40, 15
    n, k = nt*ny, 8
    x, y, z, t = _s_shape_2D(nt, ny)
    data = numx.asarray([x,y,z]).T
    res = mdp.nodes.LLENode(k, output_dim=2, svd=True)(data)
    res[:,0] /= res[:,0].std()
    res[:,1] /= res[:,1].std()

    # test alignment
    yval = y[::nt]
    tval = t[:ny]
    for yv in yval:
        idx = numx.nonzero(y==yv)[0]
        err = abs(res[idx,1]-res[idx[0],1]).max()
        assert err<0.01,\
               'Projection should be aligned as original space: %s'%(str(err))
    for tv in tval:
        idx = numx.nonzero(t==tv)[0]
        err = abs(res[idx,0]-res[idx[0],0]).max()
        assert err<0.01,\
               'Projection should be aligned as original space: %s'%(str(err))

def test_LLENode_outputdim_float_bug():
    # 1D S-shape in 3D, output_dim
    n, k = 50, 2
    x, y, z, t = _s_shape_1D(n)
    data = numx.asarray([x,y,z]).T

    res = mdp.nodes.LLENode(k, output_dim=0.9, svd=True)(data)
    # check that the neighbors are the same
    err = _compare_neighbors(data, res, k)
    assert err.max() == 0

def test_HLLENode():
    # 1D S-shape in 3D
    n, k = 250, 4
    x, y, z, t = _s_shape_1D(n)
    data = numx.asarray([x,y,z]).T

    res = mdp.nodes.HLLENode(k, r=0.001, output_dim=1, svd=False)(data)
    # check that the neighbors are the same
    err = _compare_neighbors(data, res, k)
    assert err.max() == 0

    # with svd=True
    res = mdp.nodes.HLLENode(k, r=0.001, output_dim=1, svd=True)(data)
    err = _compare_neighbors(data, res, k)
    assert err.max() == 0

    # 2D S-shape in 3D
    nt, ny = 40, 15
    n, k = nt*ny, 8
    x, y, z, t = _s_shape_2D(nt, ny)
    data = numx.asarray([x,y,z]).T
    res = mdp.nodes.HLLENode(k, r=0.001, output_dim=2, svd=False)(data)
    res[:,0] /= res[:,0].std()
    res[:,1] /= res[:,1].std()

    # test alignment
    yval = y[::nt]
    tval = t[:ny]
    for yv in yval:
        idx = numx.nonzero(y==yv)[0]
        assert numx.all(res[idx,1]-res[idx[0],1]<1e-2),\
               'Projection should be aligned as original space'
    for tv in tval:
        idx = numx.nonzero(t==tv)[0]
        assert numx.all(res[idx,0]-res[idx[0],0]<1e-2),\
               'Projection should be aligned as original space'

def test_XSFANode():
    T = 5000
    N = 3
    src = numx_rand.random((T, N))*2-1
    # create three souces with different speeds
    fsrc = numx_fft.rfft(src, axis=0)

    for i in xrange(N):
        fsrc[(i+1)*(T/10):, i] = 0.

    src = numx_fft.irfft(fsrc,axis=0)
    src -= src.mean(axis=0)
    src /= src.std(axis=0)

    #mix = sigmoid(numx.dot(src, mdp.utils.random_rot(3)))
    mix = src

    flow = mdp.Flow([mdp.nodes.XSFANode()])
    # let's test also chunk-mode training
    flow.train([[mix[:T/2, :], mix[T/2:, :]]])

    out = flow(mix)
    #import bimdp
    #tr_filename = bimdp.show_training(flow=flow,
    #                                  data_iterators=[[mix[:T/2, :], mix[T/2:, :]]])
    #ex_filename, out = bimdp.show_execution(flow, x=mix)

    corrs = mdp.utils.cov_maxima(mdp.utils.cov2(out, src))
    assert min(corrs) > 0.8, ('source/estimate minimal'
                              ' covariance: %g' % min(corrs))

