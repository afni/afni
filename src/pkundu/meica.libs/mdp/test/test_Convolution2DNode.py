from __future__ import with_statement
from _tools import *
import py.test

requires_signal = skip_on_condition(
    "not hasattr(mdp.nodes, 'Convolution2DNode')",
    "This test requires the 'scipy.signal' module.")

@requires_signal
def testConvolution2Dsimple():
    # copied over from convolution_nodes.py
    im = numx_rand.rand(4, 3,3)
    node = mdp.nodes.Convolution2DNode(numx.array([[[1.]]]))
    node.execute(im)

@requires_signal
def testConvolution2DNodeFunctionality():
    filters = numx.empty((3,1,1))
    filters[:,0,0] = [1.,2.,3.]
    x = numx_rand.random((10,3,4))
    
    for mode in ['valid', 'same', 'full']:
        for boundary in ['fill', 'wrap', 'symm']:
            node = mdp.nodes.Convolution2DNode(filters, approach='linear', mode=mode,
                                               boundary=boundary, output_2d=False)
            y = node.execute(x)
            
            assert_equal(y.shape, (x.shape[0], 3, x.shape[1], x.shape[2]))
            for n_flt in range(3):
                assert_array_equal(x*(n_flt+1.), y[:,n_flt,:,:])

@requires_signal
def testConvolution2DNode_2D3Dinput():
    filters = numx.empty((3,1,1))
    filters[:,0,0] = [1.,2.,3.]
    
    # 1) input 2D/3D
    x = numx_rand.random((10,12))        
    node = mdp.nodes.Convolution2DNode(filters, approach='linear',
                                       input_shape=(3,4), output_2d=False)
    y = node.execute(x)
    assert_equal(y.shape, (x.shape[0], 3, 3, 4))
    
    x = numx.random.random((10,3,4)) 
    node = mdp.nodes.Convolution2DNode(filters, output_2d=False)
    y = node.execute(x)
    assert_equal(y.shape, (x.shape[0], 3, 3, 4))
        
    # 2) output 2D/3D
    x = numx.random.random((10,12))        
    node = mdp.nodes.Convolution2DNode(filters, approach='linear',
                                       input_shape=(3,4), output_2d=True)
    y = node.execute(x)
    assert_equal(y.shape, (x.shape[0], 3*3*4))
    for i in range(3):
        assert_array_equal(x*(i+1.), y[:,i*12:(i+1)*12])

@requires_signal
def testConvolution2DNode_fft():
    filters = numx.empty((3,1,1))
    filters[:,0,0] = [1.,2.,3.]
    x = numx.random.random((10,3,4))
    
    for mode in ['valid', 'same', 'full']:
        node = mdp.nodes.Convolution2DNode(filters, approach='fft', mode=mode,
                                           output_2d=False)
        y = node.execute(x)

        assert_equal(y.shape, (x.shape[0], 3, x.shape[1], x.shape[2]))
        for n_flt in range(3):
            assert_array_almost_equal(x*(n_flt+1.), y[:,n_flt,:,:], 6)

    # with random filters
    x = numx.random.random((10,30,20))
    filters = numx.random.random((3,5,4))
    for mode in ['valid', 'same', 'full']:
        node_fft = mdp.nodes.Convolution2DNode(filters, approach='fft', mode=mode,
                                               output_2d=False)
        node_lin = mdp.nodes.Convolution2DNode(filters, approach='linear', mode=mode,
                                               boundary='fill', output_2d=False)
        y_fft = node_fft.execute(x)
        y_lin = node_lin.execute(x)
        
        assert_array_almost_equal(y_fft, y_lin, 6)

@requires_signal
def testConvolution2DNode_in_Flow():
    filters = numx.empty((3,1,1))
    filters[:,0,0] = [1.,2.,3.]
    
    # with 3D input
    x = numx.random.random((10,3,4))   
    node = mdp.nodes.Convolution2DNode(filters, output_2d=True)
    flow = mdp.Flow([node, mdp.nodes.PCANode(output_dim=3)])
    flow.train(x)
    flow.execute(x)
    
    # with 2D input
    x = numx.random.random((10,12))
    node = mdp.nodes.Convolution2DNode(filters, input_shape=(3,4), output_2d=True)
    flow = mdp.Flow([mdp.nodes.IdentityNode(),
                     node,
                     mdp.nodes.PCANode(output_dim=3)])
    flow.train(x)
    flow.execute(x)

@requires_signal
def testConvolution2DNode_arguments():
    # filters must be 3D
    filters = numx.random.random((5,4))
    py.test.raises(mdp.NodeException,
                   "mdp.nodes.Convolution2DNode(filters)")
    filters = numx.random.random((5,4,2,2))
    py.test.raises(mdp.NodeException,
                   "mdp.nodes.Convolution2DNode(filters)")
    # filters must be array
    filters = [[[2.]]]
    py.test.raises(mdp.NodeException,
                   "mdp.nodes.Convolution2DNode(filters)")
 
    filters = numx.random.random((1,1,1))
    with py.test.raises(mdp.NodeException):
        mdp.nodes.Convolution2DNode(filters, approach='bug')
    with py.test.raises(mdp.NodeException):
        mdp.nodes.Convolution2DNode(filters, mode='bug')
    with py.test.raises(mdp.NodeException):
        mdp.nodes.Convolution2DNode(filters, boundary='bug')

@requires_signal
def testConvolution2DNode_shape_mismatch():
    x = numx.random.random((10,60))
    filters = numx.random.random((3,5,4))
    node = mdp.nodes.Convolution2DNode(filters, input_shape=(3,2))
    with py.test.raises(mdp.NodeException):
        node.execute(x)
