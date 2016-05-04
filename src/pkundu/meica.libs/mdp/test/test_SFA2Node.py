from _tools import *

def test_basic_training():
    dim = 10000
    freqs = [2*numx.pi*100.,2*numx.pi*500.]
    t =  numx.linspace(0,1,num=dim)
    mat = numx.array([numx.sin(freqs[0]*t),numx.sin(freqs[1]*t)]).T
    mat += normal(0., 1e-10, size=(dim, 2))
    mat = (mat - mean(mat[:-1,:],axis=0))\
          /std(mat[:-1,:],axis=0)
    des_mat = mat.copy()
    mat = mult(mat,uniform((2,2))) + uniform(2)
    sfa = mdp.nodes.SFA2Node()
    sfa.train(mat)
    out = sfa.execute(mat)
    assert out.shape[1]==5, "Wrong output_dim"
    correlation = mult(des_mat[:-1,:].T,
                       numx.take(out[:-1,:], (0,2), axis=1))/(dim-2)
    assert_array_almost_equal(abs(correlation),
                              numx.eye(2), decimal-3)
    for nr in xrange(sfa.output_dim):
        qform = sfa.get_quadratic_form(nr)
        outq = qform.apply(mat)
        assert_array_almost_equal(outq, out[:,nr], decimal)

    sfa = mdp.nodes.SFANode(output_dim = 2)
    sfa.train(mat)
    out = sfa.execute(mat)
    assert out.shape[1]==2, 'Wrong output_dim'
    correlation = mult(des_mat[:-1,:1].T,out[:-1,:1])/(dim-2)
    assert_array_almost_equal(abs(correlation),
                              numx.eye(1), decimal-3)

def test_range_argument():
    node = mdp.nodes.SFA2Node()
    x = numx.random.random((100,10))
    node.train(x)
    node.stop_training()
    y = node.execute(x, n=5)
    assert y.shape[1] == 5

def test_input_dim_bug():
    dim = 10000
    freqs = [2*numx.pi*100.,2*numx.pi*500.]
    t =  numx.linspace(0,1,num=dim)
    mat = numx.array([numx.sin(freqs[0]*t),numx.sin(freqs[1]*t)]).T
    mat += normal(0., 1e-10, size=(dim, 2))
    mat = (mat - mean(mat[:-1,:],axis=0))\
          /std(mat[:-1,:],axis=0)
    mat = mult(mat,uniform((2,2))) + uniform(2)
    sfa = mdp.nodes.SFA2Node(input_dim=2)
    sfa.train(mat)
    sfa.execute(mat)

def test_output_dim_bug():
    dim = 10000
    freqs = [2*numx.pi*100.,2*numx.pi*500.]
    t =  numx.linspace(0,1,num=dim)
    mat = numx.array([numx.sin(freqs[0]*t),numx.sin(freqs[1]*t)]).T
    mat += normal(0., 1e-10, size=(dim, 2))
    mat = (mat - mean(mat[:-1,:],axis=0)) \
          / std(mat[:-1,:],axis=0)
    mat = mult(mat,uniform((2,2))) + uniform(2)
    sfa = mdp.nodes.SFA2Node(output_dim=3)
    sfa.train(mat)
    out = sfa.execute(mat)
    assert out.shape[1] == 3
