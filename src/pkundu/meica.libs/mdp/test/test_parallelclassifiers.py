import mdp.parallel as parallel
from _tools import *

def test_ParallelGaussianClassifier():
    """Test ParallelGaussianClassifier."""
    precision = 6
    xs = [numx_rand.random([4,5]) for _ in range(8)]
    labels = [1,2,1,1,2,3,2,3]
    node = mdp.nodes.GaussianClassifier()
    pnode = parallel.ParallelGaussianClassifier()
    for i, x in enumerate(xs):
        node.train(x, labels[i])
    node.stop_training()
    pnode1 = pnode.fork()
    pnode2 = pnode.fork()
    for i, x in enumerate(xs):
        if i % 2:
            pnode1.train(x, labels[i])
        else:
            pnode2.train(x, labels[i])
    pnode.join(pnode1)
    pnode.join(pnode2)
    pnode.stop_training()
    # check that results are the same for all object classes
    for i in range(3):
        assert_array_almost_equal(node.inv_covs[i], pnode.inv_covs[i],
                                  precision)
        assert_array_almost_equal(node.means[i], pnode.means[i],
                                  precision)
        assert node.p[i] == pnode.p[i]
        assert node.p[i] == pnode.p[i]
        assert node.labels[i] == pnode.labels[i]
        

def test_ParallelNearestMeanClassifier():
    """Test ParallelGaussianClassifier."""
    precision = 6
    xs = [numx_rand.random([4,5]) for _ in range(8)]
    labels = [1,2,1,1,2,3,2,3]
    node = mdp.nodes.NearestMeanClassifier()
    pnode = parallel.ParallelNearestMeanClassifier()
    for i, x in enumerate(xs):
        node.train(x, labels[i])
    node.stop_training()
    pnode1 = pnode.fork()
    pnode2 = pnode.fork()
    for i, x in enumerate(xs):
        if i % 2:
            pnode1.train(x, labels[i])
        else:
            pnode2.train(x, labels[i])
    pnode.join(pnode1)
    pnode.join(pnode2)
    pnode.stop_training()
    # check that results are the same for all object classes
    assert_array_almost_equal(node.ordered_means, pnode.ordered_means,
                              precision)
    for key in node.label_means:
        assert_array_almost_equal(node.label_means[key],
                                  pnode.label_means[key], precision)
        assert node.n_label_samples[key] == pnode.n_label_samples[key]
        

def test_ParallelKNNClassifier():
    """Test ParallelGaussianClassifier."""
    precision = 6
    xs = [numx_rand.random([3,2]) for _ in range(8)]
    labels = [1,2,1,1,2,3,2,3]
    node = mdp.nodes.KNNClassifier()
    pnode = parallel.ParallelKNNClassifier()
    for i, x in enumerate(xs):
        node.train(x, labels[i])
    node.stop_training()
    pnode1 = pnode.fork()
    pnode2 = pnode.fork()
    for i, x in enumerate(xs):
        if i < 4:
            pnode1.train(x, labels[i])
        else:
            pnode2.train(x, labels[i])
    pnode.join(pnode1)
    pnode.join(pnode2)
    pnode.stop_training()
    # check that results are the same for all object classes
    assert_array_almost_equal(node.samples, pnode.samples,
                              precision)
    assert node.n_samples == pnode.n_samples
