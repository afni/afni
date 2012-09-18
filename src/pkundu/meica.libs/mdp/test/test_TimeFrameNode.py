import mdp
from _tools import *

def test_TimeFramesNode():
    length = 14
    gap = 6
    time_frames = 3
    inp = numx.array([numx.arange(length), -numx.arange(length)]).T
    # create node to be tested
    tf = mdp.nodes.TimeFramesNode(time_frames,gap)
    out = tf.execute(inp)
    # check last element
    assert_equal(out[-1,-1], -length+1)
    # check horizontal sequence
    for i in xrange(1,time_frames):
        assert_array_equal(out[:,2*i],out[:,0]+i*gap)
        assert_array_equal(out[:,2*i+1],out[:,1]-i*gap)
    # check pseudo-inverse
    rec = tf.pseudo_inverse(out)
    assert_equal(rec.shape[1], inp.shape[1])
    block_size = min(out.shape[0], gap)
    for i in xrange(0,length,gap):
        assert_array_equal(rec[i:i+block_size], inp[i:i+block_size])

def test_TimeFramesNodeBugInputDim():
    mdp.nodes.TimeFramesNode(time_frames=10, gap=1, input_dim=1)
