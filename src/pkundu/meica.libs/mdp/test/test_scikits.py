from _tools import *

requires_scikits = skip_on_condition(
    "not mdp.config.has_sklearn or mdp.numx_description != 'scipy'",
    "This test requires sklearn and SciPy")

requires_pcasikitslearnnode = skip_on_condition(
    "'PCAScikitsLearnNode' not in dir(mdp.nodes)",
    "This test requires sklearn.decomposition.pca.PCA to be available")

@requires_scikits
@requires_pcasikitslearnnode
def test_scikits_PCANode_training():
    """Check functionality of scikits' PCANode."""
    node = mdp.nodes.PCAScikitsLearnNode(n_components=2)

    # the first two principal components are the second and fourth axes
    T = 50000
    x = numx_rand.randn(T, 4)
    x[:,1] *= 10.
    x[:,3] *= 100.

    node.train(x)
    node.stop_training()
    y = node.execute(x)

    # check dimensionality
    assert y.shape[1] == 2
    assert y.shape[0] == T

    # arrays should be equal up to sign
    if (y[:,0]*x[:,3]).mean() < 0.: y[:,0] *= -1.
    if (y[:,1]*x[:,1]).mean() < 0.: y[:,1] *= -1.
    assert_array_almost_equal(y[:,0]/100., x[:,3]/100., 1)
    assert_array_almost_equal(y[:,1]/10., x[:,1]/10., 1)

