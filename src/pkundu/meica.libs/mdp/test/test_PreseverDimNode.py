import py.test
from _tools import *


class DummyPreserveDimNode(mdp.PreserveDimNode):
    """Non-abstract dummy version of PreserveDimNode."""
    
    def is_trainable(self):
        return False
    

def testPreserveDimNode():
    """Test the different dimension setting options."""
    dim = 3
    node = DummyPreserveDimNode(input_dim=dim, output_dim=dim)
    assert node.output_dim == dim
    assert node.input_dim == dim
    node = DummyPreserveDimNode(input_dim=3)
    assert node.output_dim == dim
    assert node.input_dim == dim
    node = DummyPreserveDimNode(output_dim=3)
    assert node.output_dim == dim
    assert node.input_dim == dim
    node = DummyPreserveDimNode(output_dim=3)
    node.input_dim = dim
    assert node.output_dim == dim
    assert node.input_dim == dim
    def get_node():
        DummyPreserveDimNode(input_dim=dim, output_dim=dim+1)
    py.test.raises(mdp.InconsistentDimException, get_node)
