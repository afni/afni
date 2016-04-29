import mdp

def test_Node_deepcopy_lambda():
    """Copying a node with a lambda member function
    should not throw an Exception"""
    generic_node = mdp.Node()
    generic_node.lambda_function = lambda: 1
    generic_node.copy()

def test_Flow_deepcopy_lambda():
    """Copying a Flow with a lambda member function
    should not throw an Exception"""
    generic_node = mdp.Node()
    generic_node.lambda_function = lambda: 1
    generic_flow = mdp.Flow([generic_node])
    generic_flow.copy()
