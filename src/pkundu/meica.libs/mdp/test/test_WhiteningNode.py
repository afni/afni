from _tools import *

def testWhiteningNode():
    vars = 5
    dim = (10000,vars)
    mat,mix,inp = get_random_mix(mat_dim=dim, avg=uniform(vars))
    w = mdp.nodes.WhiteningNode()
    w.train(inp)
    out = w.execute(inp)
    assert_array_almost_equal(mean(out, axis=0),
                              numx.zeros(dim[1]), decimal)
    assert_array_almost_equal(std(out, axis=0),
                              numx.ones(dim[1]), decimal - 3)

def testWhiteningNode_SVD():
    vars = 5
    dim = (10000,vars)
    mat,mix,inp = get_random_mix(mat_dim=dim, avg=uniform(vars))
    w = mdp.nodes.WhiteningNode(svd=True)
    w.train(inp)
    out = w.execute(inp)

    assert_array_almost_equal(mean(out, axis=0),
                              numx.zeros(dim[1]), decimal)
    assert_array_almost_equal(std(out, axis=0),
                              numx.ones(dim[1]), decimal - 3)

