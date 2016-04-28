from _tools import *


def _uniform(min_, max_, dims):
    return uniform(dims)*(max_-min_)+min_

def test_GrowingNeuralGasNode():
    ### test 1D distribution in a 10D space
    # line coefficients
    dim = 10
    npoints = 1000
    const = _uniform(-100,100,[dim])
    dir = _uniform(-1,1,[dim])
    dir /= utils.norm2(dir)
    x = _uniform(-1,1,[npoints])
    data = numx.outer(x, dir)+const
    # train the gng network
    gng = mdp.nodes.GrowingNeuralGasNode(start_poss=[data[0,:],data[1,:]])
    gng.train(data)
    gng.stop_training()
    # control that the nodes in the graph lie on the line
    poss = gng.get_nodes_position()-const
    norms = numx.sqrt(numx.sum(poss*poss, axis=1))
    poss = (poss.T/norms).T
    assert max(numx.minimum(numx.sum(abs(poss-dir),axis=1),
                             numx.sum(abs(poss+dir),axis=1)))<1e-7, \
           'At least one node of the graph does lies out of the line.'
    # check that the graph is linear (no additional branches)
    # get a topological sort of the graph
    topolist = gng.graph.topological_sort()
    deg = map(lambda n: n.degree(), topolist)
    assert_equal(deg[:2],[1,1])
    assert_array_equal(deg[2:], [2 for i in xrange(len(deg)-2)])
    # check the distribution of the nodes' position is uniform
    # this node is at one of the extrema of the graph
    x0 = numx.outer(numx.amin(x, axis=0), dir)+const
    x1 = numx.outer(numx.amax(x, axis=0), dir)+const
    linelen = utils.norm2(x0-x1)
    # this is the mean distance the node should have
    dist = linelen/poss.shape[0]
    # sort the node, depth first
    nodes = gng.graph.undirected_dfs(topolist[0])
    poss = numx.array(map(lambda n: n.data.pos, nodes))
    dists = numx.sqrt(numx.sum((poss[:-1,:]-poss[1:,:])**2, axis=1))
    assert_almost_equal(dist, mean(dists), 1)
    #
    # test the nearest_neighbor function
    start_poss = [numx.asarray([2.,0]), numx.asarray([-2.,0])]
    gng = mdp.nodes.GrowingNeuralGasNode(start_poss=start_poss)
    x = numx.asarray([[2.,0]])
    gng.train(x)
    nodes, dists = gng.nearest_neighbor(numx.asarray([[1.,0]]))
    assert_equal(dists[0],1.)
    assert_array_equal(nodes[0].data.pos,numx.asarray([2,0]))

