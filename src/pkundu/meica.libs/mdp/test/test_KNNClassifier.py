from _tools import *

# These tests are basically taken from the GaussianClassifier.

def testKNNClassifier_train():
    nclasses = 10
    dim = 4
    npoints = 10000
    covs = []
    means = []
    node = mdp.nodes.KNNClassifier()
    for i in xrange(nclasses):
        cov = utils.symrand(uniform((dim,))*dim+1)
        mn = uniform((dim,))*10.
        x = normal(0., 1., size=(npoints, dim))
        x = mult(x, utils.sqrtm(cov)) + mn
        x = utils.refcast(x, 'd')
        cl = numx.ones((npoints,))*i
        mn_estimate = mean(x, axis=0)
        means.append(mn_estimate)
        covs.append(numx.cov(x, rowvar=0))
        node.train(x, cl)
    try:
        node.train(x, numx.ones((2,)))
        assert False, 'No exception despite wrong number of labels'
    except mdp.TrainingException:
        pass
    node.stop_training()

def testKNNClassifier_label():
    mean1 = [0., 2.]
    mean2 = [0., -2.]
    std_ = numx.array([1., 0.2])
    npoints = 100
    rot = 45
    # input data: two distinct gaussians rotated by 45 deg
    def distr(size): return normal(0, 1., size=(size)) * std_
    x1 = distr((npoints,2)) + mean1
    utils.rotate(x1, rot, units='degrees')
    x2 = distr((npoints,2)) + mean2
    utils.rotate(x2, rot, units='degrees')
    x = numx.concatenate((x1, x2), axis=0)
    # labels
    cl1 = numx.ones((x1.shape[0],), dtype='i')
    cl2 = 2*numx.ones((x2.shape[0],), dtype='i')
    classes = numx.concatenate((cl1, cl2))
    # shuffle the data
    perm_idx = numx_rand.permutation(classes.shape[0])
    x = numx.take(x, perm_idx, axis=0)
    classes = numx.take(classes, perm_idx, axis=0)
    node = mdp.nodes.KNNClassifier()
    node.train(x, classes)
    classification = node.label(x)
    assert_array_equal(classes, classification)
