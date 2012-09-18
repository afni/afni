from _tools import *

def testHistogramNode_nofraction():
    """Test HistogramNode with fraction set to 1.0."""
    node = mdp.nodes.HistogramNode()
    x1 = numx.array([[0.1, 0.2], [0.3, 0.5]])
    x2 = numx.array([[0.3, 0.6], [0.2, 0.1]])
    x = numx.concatenate([x1, x2])
    node.train(x1)
    node.train(x2)
    assert numx.all(x == node.data_hist)

def testHistogramNode_fraction():
    """Test HistogramNode with fraction set to 0.5."""
    node = mdp.nodes.HistogramNode(hist_fraction=0.5)
    x1 = numx_rand.random((1000, 3))
    x2 = numx_rand.random((500, 3))
    node.train(x1)
    node.train(x2)
    assert len(node.data_hist) < 1000
