from _tools import *


def testNoiseNode():
    def bogus_noise(mean, size=None):
        return numx.ones(size)*mean

    node = mdp.nodes.NoiseNode(bogus_noise, (1.,))
    out = node.execute(numx.zeros((100,10),'d'))
    assert_array_equal(out, numx.ones((100,10),'d'))
    node = mdp.nodes.NoiseNode(bogus_noise, (1.,), 'multiplicative')
    out = node.execute(numx.zeros((100,10),'d'))
    assert_array_equal(out, numx.zeros((100,10),'d'))

def testNormalNoiseNode():
    node = mdp.nodes.NormalNoiseNode(noise_args=(2.1, 0.1))
    x = numx.zeros((20000, 10))
    y = node.execute(x)
    assert numx.allclose(y.mean(0), 2.1, atol=1e-02)
    assert numx.allclose(y.std(0), 0.1, atol=1e-02)

def testNoiseNodePickling():
    node = mdp.nodes.NoiseNode()
    node.copy()
    node.save(None)
