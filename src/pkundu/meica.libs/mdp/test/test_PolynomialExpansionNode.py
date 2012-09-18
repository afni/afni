from _tools import *

def hardcoded_expansion(x, degree):
    nvars = x.shape[1]
    exp_dim = mdp.nodes._expanded_dim(degree, nvars)
    exp = numx.zeros((x.shape[0], exp_dim), 'd')
    # degree 1
    exp[:,:nvars] = x.copy()
    # degree 2
    k = nvars
    if degree>=2:
        for i in xrange(nvars):
            for j in xrange(i,nvars):
                exp[:,k] = x[:,i]*x[:,j]
                k += 1
    # degree 3
    if degree>=3:
        for i in xrange(nvars):
            for j in xrange(i,nvars):
                for l in xrange(j,nvars):
                    exp[:,k] = x[:,i]*x[:,j]*x[:,l]
                    k += 1
    # degree 4
    if degree>=4:
        for i in xrange(nvars):
            for j in xrange(i,nvars):
                for l in xrange(j,nvars):
                    for m in xrange(l,nvars):
                        exp[:,k] = x[:,i]*x[:,j]*x[:,l]*x[:,m]
                        k += 1
    # degree 5
    if degree>=5:
        for i in xrange(nvars):
            for j in xrange(i,nvars):
                for l in xrange(j,nvars):
                    for m in xrange(l,nvars):
                        for n in xrange(m,nvars):
                            exp[:,k] = \
                                     x[:,i]*x[:,j]*x[:,l]*x[:,m]*x[:,n]
                            k += 1
    return exp

def test_expansion():
    for degree in xrange(1,6):
        for dim in xrange(1,5):
            expand = mdp.nodes.PolynomialExpansionNode(degree=degree)
            mat,mix,inp = get_random_mix((10,dim))
            des = hardcoded_expansion(inp, degree)
            exp = expand.execute(inp)
            assert_array_almost_equal(exp, des, decimal)
