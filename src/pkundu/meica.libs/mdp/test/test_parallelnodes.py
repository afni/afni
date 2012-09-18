import mdp.parallel as parallel
from _tools import *

def test_PCANode():
    """Test Parallel PCANode"""
    precision = 6
    x = numx_rand.random([100,10])
    x_test = numx_rand.random([20,10])
    # set different variances (avoid numerical errors)
    x *= numx.arange(1,11)
    x_test *= numx.arange(1,11)
    pca_node = mdp.nodes.PCANode()
    parallel_pca_node = parallel.ParallelPCANode()
    chunksize = 25
    chunks = [x[i*chunksize : (i+1)*chunksize]
                for i in xrange(len(x)//chunksize)]
    for chunk in chunks:
        pca_node.train(chunk)
        forked_node = parallel_pca_node.fork()
        forked_node.train(chunk)
        parallel_pca_node.join(forked_node)
    assert_array_almost_equal(pca_node._cov_mtx._cov_mtx,
                              parallel_pca_node._cov_mtx._cov_mtx,
                              precision)
    pca_node.stop_training()
    y1 = pca_node.execute(x_test)
    parallel_pca_node.stop_training()
    y2 = parallel_pca_node.execute(x_test)
    assert_array_almost_equal(abs(y1), abs(y2), precision)

def test_SFANode():
    """Test Parallel SFANode"""
    precision = 6
    x = numx_rand.random([100,10])
    x_test = numx_rand.random([20,10])
    # set different variances (avoid numerical errors)
    x *= numx.arange(1,11)
    x_test *= numx.arange(1,11)
    sfa_node = mdp.nodes.SFANode()
    parallel_sfa_node = parallel.ParallelSFANode()
    chunksize = 25
    chunks = [x[i*chunksize : (i+1)*chunksize]
                for i in xrange(len(x)//chunksize)]
    for chunk in chunks:
        sfa_node.train(chunk)
        forked_node = parallel_sfa_node.fork()
        forked_node.train(chunk)
        parallel_sfa_node.join(forked_node)
    assert_array_almost_equal(sfa_node._cov_mtx._cov_mtx,
                              parallel_sfa_node._cov_mtx._cov_mtx,
                              precision)
    sfa_node.stop_training()
    y1 = sfa_node.execute(x_test)
    parallel_sfa_node.stop_training()
    y2 = parallel_sfa_node.execute(x_test)
    assert_array_almost_equal(abs(y1), abs(y2), precision)

def test_FDANode():
    """Test Parallel FDANode."""
    # this test code is an adaption of the FDANode test
    precision = 4
    mean1 = [0., 2.]
    mean2 = [0., -2.]
    std_ = numx.array([1., 0.2])
    npoints = 50000
    rot = 45
    # input data: two distinct gaussians rotated by 45 deg
    def distr(size):
        return numx_rand.normal(0, 1., size=(size)) * std_
    x1 = distr((npoints,2)) + mean1
    utils.rotate(x1, rot, units='degrees')
    x2 = distr((npoints,2)) + mean2
    utils.rotate(x2, rot, units='degrees')
    # labels
    cl1 = numx.ones((x1.shape[0],), dtype='d')
    cl2 = 2.*numx.ones((x2.shape[0],), dtype='d')
    flow = parallel.ParallelFlow([parallel.ParallelFDANode()])
    flow.train([[(x1, cl1), (x2, cl2)]], scheduler=parallel.Scheduler())
    fda_node = flow[0]
    assert fda_node.tlens[1] == npoints
    assert fda_node.tlens[2] == npoints
    m1 = numx.array([mean1])
    m2 = numx.array([mean2])
    utils.rotate(m1, rot, units='degrees')
    utils.rotate(m2, rot, units='degrees')
    assert_array_almost_equal(fda_node.means[1], m1, 2)
    assert_array_almost_equal(fda_node.means[2], m2, 2)
    y = flow.execute([x1, x2], scheduler=parallel.Scheduler())
    assert_array_almost_equal(numx.mean(y, axis=0), [0., 0.], precision)
    assert_array_almost_equal(numx.std(y, axis=0), [1., 1.], precision)
    assert_almost_equal(utils.mult(y[:,0], y[:,1].T), 0., precision)
    v1 = fda_node.v[:,0]/fda_node.v[0,0]
    assert_array_almost_equal(v1, [1., -1.], 2)
    v1 = fda_node.v[:,1]/fda_node.v[0,1]
    assert_array_almost_equal(v1, [1., 1.], 2)

def test_ParallelHistogramNode_nofraction():
    """Test HistogramNode with fraction set to 1.0."""
    node = parallel.ParallelHistogramNode()
    x1 = numx.array([[0.1, 0.2], [0.3, 0.5]])
    x2 = numx.array([[0.3, 0.6], [0.2, 0.1]])
    x = numx.concatenate([x1, x2])
    chunks = [x1, x2]
    for chunk in chunks:
        forked_node = node.fork()
        forked_node.train(chunk)
        node.join(forked_node)
    assert numx.all(x == node.data_hist)
    node.stop_training()

def test_ParallelHistogramNode_fraction():
    """Test HistogramNode with fraction set to 0.5."""
    node = parallel.ParallelHistogramNode(hist_fraction=0.5)
    x1 = numx.random.random((1000, 3))
    x2 = numx.random.random((500, 3))
    chunks = [x1, x2]
    for chunk in chunks:
        forked_node = node.fork()
        forked_node.train(chunk)
        node.join(forked_node)
    assert len(node.data_hist) < 1000


class TestDerivedParallelMDPNodes(object):
    """Test derived nodes that use the parallel node classes."""

    def setup_method(self, method):
        if "parallel" in mdp.get_active_extensions():
            self.set_parallel = False
        else:
            mdp.activate_extension("parallel")
            self.set_parallel = True

    def teardown_method(self, method):
        if self.set_parallel:
            mdp.deactivate_extension("parallel")

    def test_WhiteningNode(self):
        """Test Parallel WhiteningNode"""
        x = numx_rand.random([100,10])
        x_test = numx_rand.random([20,10])
        # set different variances (avoid numerical errors)
        x *= numx.arange(1,11)
        x_test *= numx.arange(1,11)
        node = mdp.nodes.WhiteningNode()
        chunksize = 25
        chunks = [x[i*chunksize : (i+1)*chunksize]
                    for i in xrange(len(x)//chunksize)]
        for chunk in chunks:
            forked_node = node.fork()
            forked_node.train(chunk)
            node.join(forked_node)
        node.stop_training()
        node.execute(x_test)

    def test_SFA2Node(self):
        """Test Parallel SFA2Node"""
        x = numx_rand.random([100,10])
        x_test = numx_rand.random([20,10])
        # set different variances (avoid numerical errors)
        x *= numx.arange(1,11)
        x_test *= numx.arange(1,11)
        node = mdp.nodes.SFA2Node()
        chunksize = 25
        chunks = [x[i*chunksize : (i+1)*chunksize]
                    for i in xrange(len(x)//chunksize)]
        for chunk in chunks:
            forked_node = node.fork()
            forked_node.train(chunk)
            node.join(forked_node)
        node.stop_training()
        node.execute(x_test)

