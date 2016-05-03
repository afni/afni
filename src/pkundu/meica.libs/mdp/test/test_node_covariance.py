from _tools import *

TESTYPES = [numx.dtype('d'), numx.dtype('f')]

def testCovarianceMatrix():
    mat,mix,inp = get_random_mix()
    des_cov = numx.cov(inp, rowvar=0)
    des_avg = mean(inp,axis=0)
    des_tlen = inp.shape[0]
    act_cov = utils.CovarianceMatrix()
    act_cov.update(inp)
    act_cov,act_avg,act_tlen = act_cov.fix()
    assert_array_almost_equal(act_tlen,des_tlen, decimal)
    assert_array_almost_equal(act_avg,des_avg, decimal)
    assert_array_almost_equal(act_cov,des_cov, decimal)

def testDelayCovarianceMatrix():
    dt = 5
    mat,mix,inp = get_random_mix()
    des_tlen = inp.shape[0] - dt
    des_avg = mean(inp[:des_tlen,:],axis=0)
    des_avg_dt = mean(inp[dt:,:],axis=0)
    des_cov = utils.cov2(inp[:des_tlen,:], inp[dt:,:])
    act_cov = utils.DelayCovarianceMatrix(dt)
    act_cov.update(inp)
    act_cov,act_avg,act_avg_dt,act_tlen = act_cov.fix()
    assert_array_almost_equal(act_tlen,des_tlen, decimal-1)
    assert_array_almost_equal(act_avg,des_avg, decimal-1)
    assert_array_almost_equal(act_avg_dt,des_avg_dt, decimal-1)
    assert_array_almost_equal(act_cov,des_cov, decimal-1)

def testCrossCovarianceMatrix():
    mat,mix,inp1 = get_random_mix(mat_dim=(500,5))
    mat,mix,inp2 = get_random_mix(mat_dim=(500,3))
    des_tlen = inp1.shape[0]
    des_avg1 = mean(inp1, axis=0)
    des_avg2 = mean(inp2, axis=0)
    des_cov = utils.cov2(inp1, inp2)
    act_cov = utils.CrossCovarianceMatrix()
    act_cov.update(inp1, inp2)
    act_cov, act_avg1, act_avg2, act_tlen = act_cov.fix()
    assert_almost_equal(act_tlen,des_tlen, decimal-1)
    assert_array_almost_equal(act_avg1,des_avg1, decimal-1)
    assert_array_almost_equal(act_avg2,des_avg2, decimal-1)
    assert_array_almost_equal(act_cov,des_cov, decimal-1)

def testdtypeCovarianceMatrix():
    for type in TESTYPES:
        mat,mix,inp = get_random_mix(type='d')
        cov = utils.CovarianceMatrix(dtype=type)
        cov.update(inp)
        cov,avg,tlen = cov.fix()
        assert_type_equal(cov.dtype,type)
        assert_type_equal(avg.dtype,type)

def testdtypeDelayCovarianceMatrix():
    for type in TESTYPES:
        dt = 5
        mat,mix,inp = get_random_mix(type='d')
        cov = utils.DelayCovarianceMatrix(dt=dt, dtype=type)
        cov.update(inp)
        cov,avg,avg_dt,tlen = cov.fix()
        assert_type_equal(cov.dtype,type)
        assert_type_equal(avg.dtype,type)
        assert_type_equal(avg_dt.dtype,type)

def testdtypeCrossCovarianceMatrix():
    for type in TESTYPES:
        mat,mix,inp = get_random_mix(type='d')
        cov = utils.CrossCovarianceMatrix(dtype=type)
        cov.update(inp, inp)
        cov,avg1,avg2,tlen = cov.fix()
        assert_type_equal(cov.dtype,type)
        assert_type_equal(avg1.dtype,type)
        assert_type_equal(avg2.dtype,type)

def testRoundOffWarningCovMatrix():
    import warnings
    warnings.filterwarnings("error",'.*',mdp.MDPWarning)
    for type in ['f','d']:
        inp = uniform((1,2))
        cov = utils.CovarianceMatrix(dtype=type)
        cov._tlen = int(1e+15)
        cov.update(inp)
        try:
            cov.fix()
            assert False, 'RoundOff warning did not work'
        except mdp.MDPWarning:
            pass
    # hope to reset the previous state...
    warnings.filterwarnings("once",'.*',mdp.MDPWarning)

def testMultipleCovarianceMatricesDtypeAndFuncs():
    for type in TESTYPES:
        dec = testdecimals[type]
        res_type = _MultipleCovarianceMatrices_funcs(type,dec)
        assert_type_equal(type,res_type)


def _MultipleCovarianceMatrices_funcs(dtype, decimals):
    def assert_all(des,act, dec=decimals):
        # check list of matrices equals multcov array
        for x in xrange(nmat):
            assert_array_almost_equal_diff(des[x],act.covs[:,:,x],dec)

    def rotate(mat,angle,indices):
        # perform a givens rotation of a single matrix
        [i,j] = indices
        c, s = numx.cos(angle), numx.sin(angle)
        mat_i, mat_j = mat[:,i].copy(), mat[:,j].copy()
        mat[:,i], mat[:,j] = c*mat_i-s*mat_j, s*mat_i+c*mat_j
        mat_i, mat_j = mat[i,:].copy(), mat[j,:].copy()
        mat[i,:], mat[j,:] = c*mat_i-s*mat_j, s*mat_i+c*mat_j
        return mat.copy()

    def permute(mat,indices):
        # permute rows and cols of a single matrix
        [i,j] = indices
        mat_i, mat_j = mat[:,i].copy(), mat[:,j].copy()
        mat[:,i], mat[:,j] = mat_j, mat_i
        mat_i, mat_j = mat[i,:].copy(), mat[j,:].copy()
        mat[i,:], mat[j,:] = mat_j, mat_i
        return mat.copy()

    dim = 7
    nmat = 13
    # create mult cov mat
    covs = [uniform((dim,dim)).astype(dtype) for x in xrange(nmat)]
    mult_cov = mdp.utils.MultipleCovarianceMatrices(covs)
    assert_equal(nmat,mult_cov.ncovs)
    # test symmetrize
    sym_covs = [0.5*(x+x.T) for x in covs]
    mult_cov.symmetrize()
    assert_all(sym_covs,mult_cov)
    # test weight
    weights = uniform(nmat)
    w_covs = [weights[x]*sym_covs[x] for x in xrange(nmat)]
    mult_cov.weight(weights)
    assert_all(w_covs,mult_cov)
    # test rotate
    angle = uniform()*2*numx.pi
    idx = numx_rand.permutation(dim)[:2]
    rot_covs = [rotate(x,angle,idx) for x in w_covs]
    mult_cov.rotate(angle,idx)
    assert_all(w_covs,mult_cov)
    # test permute
    per_covs = [permute(x,idx) for x in rot_covs]
    mult_cov.permute(idx)
    assert_all(per_covs,mult_cov)
    # test transform
    trans = uniform((dim,dim))
    trans_covs = [mult(mult(trans.T,x),trans) for x in per_covs]
    mult_cov.transform(trans)
    assert_all(trans_covs,mult_cov)
    # test copy
    cp_mult_cov = mult_cov.copy()
    assert_array_equal(mult_cov.covs,cp_mult_cov.covs)
    # check that we didn't got a reference
    mult_cov[0][0,0] = 1000
    assert int(cp_mult_cov[0][0,0]) != 1000
    # return dtype
    return mult_cov.covs.dtype

def testMultipleCovarianceMatricesTransformations():
    def get_mult_covs(inp,nmat):
        # return delayed covariance matrices
        covs = []
        for delay in xrange(nmat):
            tmp = mdp.utils.DelayCovarianceMatrix(delay)
            tmp.update(inp)
            cov,avg,avg_dt,tlen = tmp.fix()
            covs.append(cov)
        return mdp.utils.MultipleCovarianceMatrices(covs)
    dim = 7
    nmat = 13
    angle = uniform()*2*numx.pi
    idx = numx_rand.permutation(dim)[:2]
    inp = uniform((100*dim,dim))
    rot_inp, per_inp = inp.copy(), inp.copy()
    # test if rotating or permuting the cov matrix is equivalent
    # to rotate or permute the sources.
    mdp.utils.rotate(rot_inp,angle,idx)
    mdp.utils.permute(per_inp,idx,rows=0,cols=1)
    mcov = get_mult_covs(inp, nmat)
    mcov2 = mcov.copy()
    mcov_rot = get_mult_covs(rot_inp, nmat)
    mcov_per = get_mult_covs(per_inp, nmat)
    mcov.rotate(angle,idx)
    mcov2.permute(idx)
    assert_array_almost_equal_diff(mcov.covs, mcov_rot.covs, decimal)
    assert_array_almost_equal_diff(mcov2.covs, mcov_per.covs, decimal)
