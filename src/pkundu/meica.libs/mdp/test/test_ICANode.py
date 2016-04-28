from _tools import *

def verify_ICANode(icanode, rand_func = uniform, vars = 3, N=8000,
                 prec = 3):
    dim = (N,vars)
    mat,mix,inp = get_random_mix(rand_func=rand_func,mat_dim=dim)
    icanode.train(inp)
    act_mat = icanode.execute(inp)
    cov = utils.cov2((mat-mean(mat,axis=0))/std(mat,axis=0), act_mat)
    maxima = numx.amax(abs(cov), axis=0)
    assert_array_almost_equal(maxima,numx.ones(vars),prec)

def verify_ICANodeMatrices(icanode, rand_func = uniform, vars = 3, N=8000):
    dim = (N,vars)
    mat,mix,inp = get_random_mix(rand_func=rand_func,
                                       mat_dim=dim, avg = 0)
    icanode.train(inp)
    # test projection matrix
    act_mat = icanode.execute(inp)
    T = icanode.get_projmatrix()
    exp_mat = mult(inp, T)
    assert_array_almost_equal(act_mat,exp_mat,6)
    # test reconstruction matrix
    out = act_mat.copy()
    act_mat = icanode.inverse(out)
    B = icanode.get_recmatrix()
    exp_mat = mult(out, B)
    assert_array_almost_equal(act_mat,exp_mat,6)

def rand_with_timestruct(size=None):
    T, N = size
    # do something special only if T!=N, otherwise
    # we were asked to generate a mixing matrix
    if T == N:
        return uniform(size=size)
    # create independent sources
    src = uniform((T,N))*2-1
    fsrc = numx_fft.rfft(src,axis=0)
    # enforce different speeds
    for i in xrange(N):
        fsrc[(i+1)*(T//20):,i] = 0.
    src = numx_fft.irfft(fsrc,axis=0)
    return src

def test_CuBICANode_batch():
    ica = mdp.nodes.CuBICANode(limit=10**(-decimal))
    ica2 = ica.copy()
    verify_ICANode(ica)
    verify_ICANodeMatrices(ica2)

def test_CuBICANode_telescope():
    ica = mdp.nodes.CuBICANode(limit=10**(-decimal), telescope=1)
    ica2 = ica.copy()
    verify_ICANode(ica)
    verify_ICANodeMatrices(ica2)


def test_TDSEPNode():
    ica = mdp.nodes.TDSEPNode(lags=20, limit=1e-10)
    ica2 = ica.copy()
    verify_ICANode(ica, rand_func=rand_with_timestruct, vars=2, N=2**14, prec=2)
    verify_ICANodeMatrices(ica2, rand_func=rand_with_timestruct, vars=2, N=2**14)
