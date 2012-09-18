from _tools import *

def _uniform(min_, max_, dims):
    return uniform(dims)*(max_-min_)+min_

def test_NeuralGasNode():
    ### test 1D distribution in a 10D space
    # line coefficients
    dim = 10
    npoints = 1000
    const = _uniform(-100,100,[dim])
    dir = _uniform(-1,1,[dim])
    dir /= utils.norm2(dir)
    x = _uniform(-1,1,[npoints])
    data = numx.outer(x, dir)+const
    # train the ng network
    num_nodes = 10
    ng = mdp.nodes.NeuralGasNode(start_poss=[data[n,:]
                                             for n in range(num_nodes)],
                                 max_epochs=10)
    ng.train(data)
    ng.stop_training()
    # control that the nodes in the graph lie on the line
    poss = ng.get_nodes_position()-const
    norms = numx.sqrt(numx.sum(poss*poss, axis=1))
    poss = (poss.T/norms).T
    assert max(numx.minimum(numx.sum(abs(poss-dir),axis=1),
                            numx.sum(abs(poss+dir),axis=1))) < 1e-7, \
           'At least one node of the graph does lies out of the line.'
    # check that the graph is linear (no additional branches)
    # get a topological sort of the graph
    topolist = ng.graph.topological_sort()
    deg = numx.asarray(map(lambda n: n.degree(), topolist))
    idx = deg.argsort()
    deg = deg[idx]
    assert_equal(deg[:2],[1,1])
    assert_array_equal(deg[2:], [2 for i in xrange(len(deg)-2)])
    # check the distribution of the nodes' position is uniform
    # this node is at one of the extrema of the graph
    x0 = numx.outer(numx.amin(x, axis=0), dir)+const
    x1 = numx.outer(numx.amax(x, axis=0), dir)+const
    linelen = utils.norm2(x0-x1)
    # this is the mean distance the node should have
    dist = linelen / poss.shape[0]
    # sort the node, depth first
    nodes = ng.graph.undirected_dfs(topolist[idx[0]])
    poss = numx.array(map(lambda n: n.data.pos, nodes))
    dists = numx.sqrt(numx.sum((poss[:-1,:]-poss[1:,:])**2, axis=1))
    assert_almost_equal(dist, mean(dists), 1)

def test_NeuralGasNode_nearest_neighbor():
    # test the nearest_neighbor function
    start_poss = [numx.asarray([2.,0]), numx.asarray([-2.,0])]
    ng = mdp.nodes.NeuralGasNode(start_poss=start_poss, max_epochs=4)
    x = numx.asarray([[2.,0]])
    ng.train(x)
    nodes, dists = ng.nearest_neighbor(numx.asarray([[3.,0]]))
    assert_almost_equal(dists[0], 1., 7)
    assert_almost_equal(nodes[0].data.pos, numx.asarray([2., 0.]), 7)
