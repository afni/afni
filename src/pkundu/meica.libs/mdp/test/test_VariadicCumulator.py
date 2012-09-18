import mdp
from _tools import *

def test_VariadicCumulator():
    # create random data
    ONELEN = 101
    NREP = 7
    x = [numx_rand.rand(ONELEN, 3) for _ in range(NREP)]
    y = [numx_rand.rand(ONELEN, 3) for _ in range(NREP)]
    
    ABCumulator = mdp.VariadicCumulator('a', 'b')
    
    class TestABCumulator(ABCumulator):
        def _stop_training(self, *args, **kwargs):
            super(TestABCumulator, self)._stop_training(*args, **kwargs)
            # verify that the attributes are there
            assert hasattr(self, 'a')
            assert hasattr(self, 'b')
            # test tlen
            tlen = ONELEN*NREP
            assert self.tlen == tlen
            assert self.a.shape == (tlen, 3)
            assert self.b.shape == (tlen, 3)
            # test content
            for i in range(NREP):
                assert numx.all(self.a[i*ONELEN:(i+1)*ONELEN,:] == x[i])
                assert numx.all(self.b[i*ONELEN:(i+1)*ONELEN,:] == y[i])

    ab = TestABCumulator()
    for i in range(NREP):
        ab.train(x[i], y[i])
    ab.stop_training()
