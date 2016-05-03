from _tools import *


def test_AdaptiveCutoffNode_smalldata():
    """Test AdaptiveCutoffNode thoroughly on a small data set."""
    # values from 0.1 to 0.6 and 0.2 to 0.7
    x1 = numx.array([[0.1, 0.3], [0.3, 0.5], [0.5, 0.7]])
    x2 = numx.array([[0.4, 0.6], [0.2, 0.4], [0.6, 0.2]])
    x = numx.concatenate([x1, x2])
    node = mdp.nodes.AdaptiveCutoffNode(lower_cutoff_fraction= 0.2, # clip first
                                    upper_cutoff_fraction=0.4) # last two
    node.train(x1)
    node.train(x2)
    node.stop_training()
    assert numx.all(x == node.data_hist)
    # test bound values
    assert numx.all(node.lower_bounds == numx.array([0.2, 0.3]))
    assert numx.all(node.upper_bounds == numx.array([0.4, 0.5]))
    # test execute
    x_test = (numx.array([[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]]))
    x_clip = node.execute(x_test)
    x_goal = numx.array([[0.2, 0.3], [0.3, 0.4], [0.4, 0.5]])
    assert (x_clip == x_goal).all()

def test_AdaptiveCutoffNode_randomdata():
    """Test AdaptiveCutoffNode on a large random data."""
    node = mdp.nodes.AdaptiveCutoffNode(lower_cutoff_fraction= 0.2,
                                    upper_cutoff_fraction=0.4,
                                    hist_fraction=0.5)
    x1 = numx_rand.random((1000, 3))
    x2 = numx_rand.random((500, 3))
    x = numx.concatenate([x1, x2])
    node.train(x1)
    node.train(x2)
    node.stop_training()
    node.execute(x)

