import py.test
from _tools import *

def test_FDANode():
    mean1 = [0., 2.]
    mean2 = [0., -2.]
    std_ = numx.array([1., 0.2])
    npoints = 50000
    rot = 45

    # input data: two distinct gaussians rotated by 45 deg
    def distr(size):
        return normal(0, 1., size=(size)) * std_

    x1 = distr((npoints,2)) + mean1
    utils.rotate(x1, rot, units='degrees')
    x2 = distr((npoints,2)) + mean2
    utils.rotate(x2, rot, units='degrees')
    x = numx.concatenate((x1, x2), axis=0)

    # labels
    cl1 = numx.ones((x1.shape[0],), dtype='d')
    cl2 = 2.*numx.ones((x2.shape[0],), dtype='d')
    classes = numx.concatenate((cl1, cl2))

    # shuffle the data
    perm_idx = numx_rand.permutation(classes.shape[0])
    x = numx.take(x, perm_idx, axis=0)

    classes = numx.take(classes, perm_idx)

    flow = mdp.Flow([mdp.nodes.FDANode()])
    py.test.raises(mdp.TrainingException,
                   flow[0].train, x, numx.ones((2,)))

    flow.train([[(x, classes)]])
    fda_node = flow[0]

    assert fda_node.tlens[1] == npoints
    assert fda_node.tlens[2] == npoints

    m1 = numx.array([mean1])
    m2 = numx.array([mean2])
    utils.rotate(m1, rot, units='degrees')
    utils.rotate(m2, rot, units='degrees')
    assert_array_almost_equal(fda_node.means[1], m1, 2)
    assert_array_almost_equal(fda_node.means[2], m2, 2)

    y = flow.execute(x)
    assert_array_almost_equal(mean(y, axis=0), [0., 0.], decimal-2)
    assert_array_almost_equal(std(y, axis=0), [1., 1.], decimal-2)
    assert_almost_equal(mult(y[:,0], y[:,1].T), 0., decimal-2)

    v1 = fda_node.v[:,0]/fda_node.v[0,0]
    assert_array_almost_equal(v1, [1., -1.], 2)
    v1 = fda_node.v[:,1]/fda_node.v[0,1]
    assert_array_almost_equal(v1, [1., 1.], 2)
