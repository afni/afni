from _tools import *

def test_CutoffNode():
    node = mdp.nodes.CutoffNode(-1.5, 1.2)
    x = numx.array([[0.1, 0, -2, 3, 1.2, -1.5, -3.33]])
    y_ref = numx.array([[0.1, 0, -1.5, 1.2, 1.2, -1.5, -1.5]])
    y = node.execute(x)
    assert numx.all(y==y_ref)
