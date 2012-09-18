from _tools import *

def _std(x):
    return x.std(axis=0)
    # standard deviation without bias
    mx = mean(x, axis=0)
    mx2 = mean(x*x, axis=0)
    return numx.sqrt((mx2-mx)/(x.shape[0]-1))

def _cov(x,y=None):
    #return covariance matrix for x and y
    if y is None:
        y = x.copy()
    x = x - mean(x,0)
    x = x / _std(x)
    y = y - mean(y,0)
    y = y  / _std(y)
    #return mult(numx.transpose(x),y)/(x.shape[0]-1)
    return mult(numx.transpose(x),y)/(x.shape[0])

def testISFANodeGivensRotations():
    ncovs = 5
    dim = 7
    ratio = uniform(2).tolist()
    covs = [uniform((dim,dim)) for j in xrange(ncovs)]
    covs= mdp.utils.MultipleCovarianceMatrices(covs)
    covs.symmetrize()
    i = mdp.nodes.ISFANode(range(1, ncovs+1),sfa_ica_coeff=ratio,
                           icaweights=uniform(ncovs),
                           sfaweights=uniform(ncovs),
                           output_dim = dim-1, dtype="d")
    i._adjust_ica_sfa_coeff()
    ratio = i._bica_bsfa
    # case 2: only one axis within output space
    # get contrast using internal function
    phi, cont1, min_, dummy =\
               i._givens_angle_case2(dim-2,dim-1,covs,ratio,complete=1)
    # get contrast using explicit rotations
    cont2 = []
    for angle in phi:
        cp = covs.copy()
        cp.rotate(angle,[dim-2,dim-1])
        cont2.append(numx.sum(i._get_contrast(cp,ratio)))
    assert_array_almost_equal(cont1,cont2,decimal)
    # case 1: both axes within output space
    # get contrast using internal function
    phi,cont1, min_ , dummy =\
               i._givens_angle_case1(0,1,covs,ratio,complete = 1)
    # get contrast using explicit rotations
    cont2 = []
    for angle in phi:
        cp = covs.copy()
        cp.rotate(angle,[0,1])
        cont2.append(numx.sum(i._get_contrast(cp,ratio)))
    assert abs(min_) < numx.pi/4, 'Estimated Minimum out of bounds'
    assert_array_almost_equal(cont1,cont2,decimal)

def testISFANode_SFAPart():
    # create independent sources
    mat = uniform((100000,3))*2-1
    fmat = numx_fft.rfft(mat,axis=0)
    # enforce different speeds
    for i in xrange(3):
        fmat[(i+1)*5000:,i] = 0.
    mat = numx_fft.irfft(fmat,axis=0)
    _sfanode = mdp.nodes.SFANode()
    _sfanode.train(mat)
    src = _sfanode.execute(mat)
    # test with unmixed signals (i.e. the node should make nothing at all)
    out = mdp.nodes.ISFANode(lags=1,
                             whitened=True,
                             sfa_ica_coeff=[1.,0.])(src)
    max_cv = numx.diag(abs(_cov(out,src)))
    assert_array_almost_equal(max_cv, numx.ones((3,)),5)
    # mix linearly the signals
    mix = mult(src,uniform((3,3))*2-1)
    out = mdp.nodes.ISFANode(lags=1,
                             whitened=False,
                             sfa_ica_coeff=[1.,0.])(mix)
    max_cv = numx.diag(abs(_cov(out,src)))
    assert_array_almost_equal(max_cv, numx.ones((3,)),5)

def testISFANode_ICAPart():
    # create independent sources
    src = uniform((100000,3))*2-1
    fsrc = numx_fft.rfft(src,axis=0)
    # enforce different speeds
    for i in xrange(3):
        fsrc[(i+1)*5000:,i] = 0.
    src = numx_fft.irfft(fsrc,axis=0)
    # enforce time-lag-1-independence
    src = mdp.nodes.ISFANode(lags=1, sfa_ica_coeff=[1.,0.])(src)
    out = mdp.nodes.ISFANode(lags=1,
                             whitened=True,
                             sfa_ica_coeff=[0.,1.])(src)
    max_cv = numx.diag(abs(_cov(out,src)))
    assert_array_almost_equal(max_cv, numx.ones((3,)),5)
    # mix linearly the signals
    mix = mult(src,uniform((3,3))*2-1)
    out = mdp.nodes.ISFANode(lags=1,
                             whitened=False,
                             sfa_ica_coeff=[0.,1.])(mix)
    max_cv = numx.diag(abs(_cov(out,src)))
    assert_array_almost_equal(max_cv, numx.ones((3,)),5)

def testISFANode_3Complete():
    # test transition from ica to sfa behavior of isfa
    # use ad hoc sources
    lag = 25
    src = numx.zeros((1001,3),"d")
    idx = [(2,4),(80,1),(2+lag,6)]
    for i in xrange(len(idx)):
        i0, il = idx[i]
        src[i0:i0+il,i] = 1.
        src[i0+il:i0+2*il,i] = -1.
        src[:,i] -= mean(src[:,i])
        src[:,i] /= std(src[:,i])
    # test extreme cases
    # case 1: ICA
    out = mdp.nodes.ISFANode(lags=[1,lag],
                             icaweights=[1.,1.],
                             sfaweights=[1.,0.],
                             output_dim=2,
                             whitened=True,
                             sfa_ica_coeff=[1E-4,1.])(src)
    cv = abs(_cov(src,out))
    idx_cv = numx.argmax(cv,axis=0)
    assert_array_equal(idx_cv,[2,1])
    max_cv = numx.amax(cv,axis=0)
    assert_array_almost_equal(max_cv, numx.ones((2,)),5)
    # case 2: SFA
    out = mdp.nodes.ISFANode(lags=[1,lag],
                             icaweights=[1.,1.],
                             sfaweights=[1.,0.],
                             output_dim=2,
                             whitened=True,
                             sfa_ica_coeff=[1.,0.])(src)
    cv = abs(_cov(src,out))
    idx_cv = numx.argmax(cv,axis=0)
    assert_array_equal(idx_cv,[2,0])
    max_cv = numx.amax(cv,axis=0)
    assert_array_almost_equal(max_cv, numx.ones((2,)),5)

def _ISFA_analytical_solution( nsources, nmat, dim, ica_ambiguity):
    # build a sequence of random diagonal matrices
    matrices = [numx.eye(dim, dtype='d')]*nmat
    # build first matrix:
    #   - create random diagonal with elements
    #     in [0, 1]
    diag = uniform(dim)
    #   - sort it in descending order (in absolute value)
    #     [large first]
    diag = numx.take(diag, numx.argsort(abs(diag)))[::-1]
    #   - save larger elements [sfa solution]
    sfa_solution = diag[:nsources].copy()
    #   - modify diagonal elements order to allow for a
    #     different solution for isfa:
    #     create index array
    idx = range(0,dim)
    #     take the second slowest element and put it at the end
    idx = [idx[0]]+idx[2:]+[idx[1]]
    diag = numx.take(diag, idx)
    #   - save isfa solution
    isfa_solution = diag[:nsources]
    #   - set the first matrix
    matrices[0] = matrices[0]*diag
    # build other matrices
    diag_dim = nsources+ica_ambiguity
    for i in xrange(1,nmat):
        # get a random symmetric matrix
        matrices[i] = mdp.utils.symrand(dim)
        # diagonalize the subspace diag_dim
        tmp_diag = (uniform(diag_dim)-0.5)*2
        matrices[i][:diag_dim,:diag_dim] = numx.diag(tmp_diag)
    # put everything in MultCovMat
    matrices = mdp.utils.MultipleCovarianceMatrices(matrices)
    return matrices, sfa_solution, isfa_solution

def _ISFA_unmixing_error( nsources, goal, estimate):
    check = mult(goal[:nsources,:], estimate[:,:nsources])
    error = (abs(numx.sum(numx.sum(abs(check),axis=1)-1))+
             abs(numx.sum(numx.sum(abs(check),axis=0)-1)))
    error /= nsources*nsources
    return error

def testISFANode_AnalyticalSolution():
    nsources = 2
    # number of time lags
    nmat = 20
    # degree of polynomial expansion
    deg = 3
    # sfa_ica coefficient
    sfa_ica_coeff = [1., 1.]
    # how many independent subspaces in addition to the sources
    ica_ambiguity = 2
    # dimensions of expanded space
    dim = mdp.nodes._expanded_dim(deg, nsources)
    assert (nsources+ica_ambiguity) < dim, 'Too much ica ambiguity.'
    trials = 20
    for trial in xrange(trials):
        # get analytical solution:
        # prepared matrices, solution for sfa, solution for isf
        covs,sfa_solution,isfa_solution=_ISFA_analytical_solution(
            nsources,nmat,dim,ica_ambiguity)
        # get contrast of analytical solution
        # sfasrc, icasrc = _get_matrices_contrast(covs, nsources, dim,
        #                                         sfa_ica_coeff)
        # set rotation matrix
        R = mdp.utils.random_rot(dim)
        covs_rot = covs.copy()
        # rotate the analytical solution
        covs_rot.transform(R)
        # find the SFA solution to initialize ISFA
        eigval, SFARP = mdp.utils.symeig(covs_rot.covs[:,:,0])
        # order SFA solution by slowness
        SFARP = SFARP[:,-1::-1]
        # run ISFA
        isfa = mdp.nodes.ISFANode(lags = covs_rot.ncovs, whitened=True,
                                  sfa_ica_coeff = sfa_ica_coeff,
                                  eps_contrast = 1e-7,
                                  output_dim = nsources,
                                  max_iter = 500,
                                  verbose = False,
                                  RP = SFARP)
        isfa.train(uniform((100,dim)))
        isfa.stop_training(covs = covs_rot.copy())
        # check that the rotation matrix found by ISFA is R
        # up to a permutation matrix.
        # Unmixing error as in Tobias paper
        error = _ISFA_unmixing_error(nsources, R, isfa.RPC)
        if error < 1E-4:
            break
    assert error < 1E-4, 'None out of the %d trials succeded.' % trials
