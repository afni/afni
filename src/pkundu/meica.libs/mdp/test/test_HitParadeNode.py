import mdp
from _tools import *

def testHitParadeNode():
    signal = uniform((300,3))
    gap = 5
    signal[10,0], signal[120,1], signal[230,2] = 4,3,2
    signal[11,0], signal[121,1], signal[231,2] = -4,-3,-2
    hit = mdp.nodes.HitParadeNode(1,gap,3)
    hit.train(signal[:100,:])
    hit.train(signal[100:200,:])
    hit.train(signal[200:300,:])
    maxima, max_ind = hit.get_maxima()
    minima, min_ind = hit.get_minima()
    assert_array_equal(maxima,numx.array([[4,3,2]]))
    assert_array_equal(max_ind,numx.array([[10,120,230]]))
    assert_array_equal(minima,numx.array([[-4,-3,-2]]))
    assert_array_equal(min_ind,numx.array([[11,121,231]]))
    # test integer type:
    signal = (uniform((300,3))*10).astype('i')
    gap = 5
    signal[10,0], signal[120,1], signal[230,2] = 40,30,20
    signal[11,0], signal[121,1], signal[231,2] = -40,-30,-20
    hit = mdp.nodes.HitParadeNode(1,gap,3)
    hit.train(signal[:100,:])
    hit.train(signal[100:200,:])
    hit.train(signal[200:300,:])
    maxima, max_ind = hit.get_maxima()
    minima, min_ind = hit.get_minima()
    assert_array_equal(maxima,numx.array([[40,30,20]]))
    assert_array_equal(max_ind,numx.array([[10,120,230]]))
    assert_array_equal(minima,numx.array([[-40,-30,-20]]))
    assert_array_equal(min_ind,numx.array([[11,121,231]]))

def testOneDimensionalHitParade():
    signal = (uniform(300)-0.5)*2
    gap = 5
    # put some maxima and minima
    signal[0] , signal[10] , signal[50] = 1.5, 1.4, 1.3
    signal[1] , signal[11] , signal[51] = -1.5, -1.4, -1.3
    # put two maxima and two minima within the gap
    signal[100], signal[103] = 2, 3
    signal[110], signal[113] = 3.1, 2
    signal[120], signal[123] = -2, -3.1
    signal[130], signal[133] = -3, -2
    hit = mdp.nodes._OneDimensionalHitParade(5,gap)
    hit.update((signal[:100],numx.arange(100)))
    hit.update((signal[100:200],numx.arange(100,200)))
    hit.update((signal[200:300],numx.arange(200,300)))
    maxima,ind_maxima = hit.get_maxima()
    minima,ind_minima = hit.get_minima()
    assert_array_equal(maxima,[3.1,3,1.5,1.4,1.3])
    assert_array_equal(ind_maxima,[110,103,0,10,50])
    assert_array_equal(minima,[-3.1,-3,-1.5,-1.4,-1.3])
    assert_array_equal(ind_minima,[123,130,1,11,51])
