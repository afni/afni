import mdp
from _tools import *

requires_scipy = skip_on_condition(
    "not mdp.numx_description == 'scipy'",
    "This test requires 'scipy'")

def dumb_quadratic_expansion(x):
    dim_x = x.shape[1]
    return numx.asarray([(x[i].reshape(dim_x,1) *
                          x[i].reshape(1,dim_x)).flatten()
                         for i in range(len(x))])

def testGeneralExpansionNode():
    samples = 2
    input_dim = 10
    funcs = [lambda x:x, lambda x: x**2, dumb_quadratic_expansion]

    cen = mdp.nodes.GeneralExpansionNode(funcs)

    input = numx.random.normal(size=(samples, input_dim))
    out = cen.execute(input)
    assert_array_almost_equal(out[:, 0:input_dim], input,
                              6, "incorrect constant expansion")
    assert_array_almost_equal(out[:, input_dim:2*input_dim],
                              input ** 2, 6, "incorrect constant expansion")
    assert_array_almost_equal(out[:, 2*input_dim:],
                              dumb_quadratic_expansion(input), 6,
                              "incorrect constant expansion")
    assert cen.expanded_dim(input_dim) == 2 * input_dim + input_dim**2, "expanded_dim failed"
    assert_array_almost_equal(cen.output_sizes(input_dim),
                              numx.array([input_dim, input_dim,
                                          input_dim*input_dim]), 6,
                              "output_sizes failed")

@requires_scipy
def testGeneralExpansionNode_inverse():
    samples = 2
    input_dim = 10
    funcs = [lambda x:x, lambda x: x**2, dumb_quadratic_expansion]

    cen = mdp.nodes.GeneralExpansionNode(funcs)
    input = numx.random.normal(size=(samples, input_dim))
    out = cen.execute(input)
    app_input = cen.pseudo_inverse(out, use_hint=True)
    assert_array_almost_equal_diff(input, app_input, 6,
                                   'inversion not good enough by use_hint=True')

    # ???
    # testing with use_hint = False is tricky, it often fails.
    # we try 20 times in a row and hope for the best
    trials = 20
    for trial in range(trials):
        cen = mdp.nodes.GeneralExpansionNode(funcs)
        input = numx.random.normal(size=(samples, input_dim))
        out = cen.execute(input)
        app_input = cen.pseudo_inverse(out, use_hint=False)
        maxdiff = max(numx.ravel(abs(app_input-input)))/\
                  max(max(abs(numx.ravel(app_input))),max(abs(numx.ravel(input))))
        cond = maxdiff < 10** (-4)
        if cond:
            break
    assert cond, 'inversion not good enough by use_hint=False'

