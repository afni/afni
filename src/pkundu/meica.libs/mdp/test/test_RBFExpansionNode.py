import mdp
from _tools import *

def testRBFExpansionNode():
    rrep = mdp.utils.rrep
    dim, n = 2, 10
    centers = numx_rand.random((n, dim))
    # grid of points to numerically compute the integral
    grid = numx.meshgrid(numx.linspace(-3., 4., 100),
                         numx.linspace(-3., 4., 100))
    grid = numx.array([grid[0].flatten(), grid[1].flatten()]).T
    # compute covariance for each point of the grid
    grid_cov = numx.zeros((grid.shape[0], dim, dim))
    for i in xrange(dim):
        for j in xrange(dim):
            grid_cov[:,i,j] = grid[:,i]*grid[:,j]

    def check_mn_cov(rbf, real_covs):
        y = rbf(grid)
        # verify means, sizes
        for i in xrange(n):
            p = y[:,i]/y[:,i].sum()
            # check mean
            mn = (rrep(p,dim)*grid).sum(0)
            assert_array_almost_equal(mn, centers[i,:], 2)
            # check variance
            vr = ((rrep(rrep(p,2),2)*grid_cov).sum(0)
                  - numx.outer(mn, mn))
            assert_array_almost_equal(vr, real_covs[i], 2)

    def scalar_to_covs(x, n):
        if numx.isscalar(x):
            x = [x]*n
        return [numx.array([[x[i],0],[0,x[i]]]) for i in xrange(n)]

    # 1: sizes is a scalar
    sizes = 0.32
    rbf = mdp.nodes.RBFExpansionNode(centers, sizes)
    check_mn_cov(rbf, scalar_to_covs(sizes, n))

    # 2: sizes is many scalars
    sizes = 0.3 + numx_rand.random(n)*0.2
    rbf = mdp.nodes.RBFExpansionNode(centers, sizes)
    check_mn_cov(rbf, scalar_to_covs(sizes, n))

    # 3: sizes is one covariance
    sizes = mdp.utils.symrand(numx.array([0.2, 0.4]))
    rbf = mdp.nodes.RBFExpansionNode(centers, sizes)
    check_mn_cov(rbf, [sizes]*n)

    # 4: sizes is many covariances
    sizes = [mdp.utils.symrand(numx.array([0.2, 0.4]))
             for i in xrange(n)]
    rbf = mdp.nodes.RBFExpansionNode(centers, sizes)
    check_mn_cov(rbf, sizes)

