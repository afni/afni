from __future__ import with_statement
import mdp.hinet as mh
from _tools import *
from test_nodes_generic import (
    generic_test_factory,
    test_dtype_consistency,
    # this test fails due to the checks in _set_output_dim
    #test_outputdim_consistency,
    test_dimdtypeset,
    #test_inverse,
    # ???: test_inverse is not on the list, because it would
    # skip all the nodes in NODES anyway, because they're not
    # always invertible
)

def _get_new_flow():
    return mdp.Flow([mdp.nodes.NoiseNode(),
                     mdp.nodes.SFANode()])

def _get_new_nodes():
    return [mdp.nodes.CuBICANode(input_dim=1, whitened=True),
            mdp.nodes.CuBICANode(input_dim=2, whitened=True),
            mdp.nodes.CuBICANode(input_dim=1, whitened=True)]

def _get_single_node():
    return mdp.nodes.CuBICANode(input_dim=2, whitened=True)

def hinet_get_random_mix():
    return get_random_mix(type='d', mat_dim=(500,4))[2]

NODES = [dict(klass=mh.FlowNode,
              inp_arg_gen=hinet_get_random_mix,
              init_args = [_get_new_flow]),
         dict(klass=mh.Layer,
              inp_arg_gen=hinet_get_random_mix,
              init_args = [_get_new_nodes]),
         dict(klass=mh.CloneLayer,
              inp_arg_gen=hinet_get_random_mix,
              init_args = [_get_single_node, 2]),
         ]

def pytest_generate_tests(metafunc):
    generic_test_factory(NODES, metafunc)
