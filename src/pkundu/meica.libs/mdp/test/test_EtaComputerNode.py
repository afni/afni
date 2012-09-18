import mdp
from _tools import *

def testEtaComputerNode():
    tlen = 1e5
    t = numx.linspace(0,2*numx.pi,tlen)
    inp = numx.array([numx.sin(t), numx.sin(5*t)]).T
    # create node to be tested
    ecnode = mdp.nodes.EtaComputerNode()
    ecnode.train(inp)
    #
    etas = ecnode.get_eta(t=tlen)
    # precision gets better with increasing tlen
    assert_array_almost_equal(etas, [1, 5], decimal=4)

