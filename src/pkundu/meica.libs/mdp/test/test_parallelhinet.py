from _tools import *

import mdp.parallel as parallel
import mdp.hinet as hinet
n = numx

class TestParallelHinetNodes():
    """Tests for ParallelFlowNode."""

    def setup_method(self, method):
        if "parallel" in mdp.get_active_extensions():
            self.set_parallel = False
        else:
            mdp.activate_extension("parallel")
            self.set_parallel = True

    def teardown_method(self, method):
        if self.set_parallel:
            mdp.deactivate_extension("parallel")

    def test_flownode(self):
        """Test ParallelFlowNode."""
        flow = mdp.Flow([mdp.nodes.SFANode(output_dim=5),
                         mdp.nodes.PolynomialExpansionNode(degree=2),
                         mdp.nodes.SFANode(output_dim=3)])
        flownode = mdp.hinet.FlowNode(flow)
        x = n.random.random([100,50])
        chunksize = 25
        chunks = [x[i*chunksize : (i+1)*chunksize]
                    for i in xrange(len(x)//chunksize)]
        while flownode.get_remaining_train_phase() > 0:
            for chunk in chunks:
                forked_node = flownode.fork()
                forked_node.train(chunk)
                flownode.join(forked_node)
            flownode.stop_training()
        # test execution
        flownode.execute(x)
        
    def test_flownode_forksingle(self):
        """Test that ParallelFlowNode forks only the first training node."""
        flow = mdp.Flow([mdp.nodes.SFANode(output_dim=5),
                         mdp.nodes.PolynomialExpansionNode(degree=2),
                         mdp.nodes.SFANode(output_dim=3)])
        flownode = mdp.hinet.FlowNode(flow)
        forked_flownode = flownode.fork()
        assert flownode._flow[0] is not forked_flownode._flow[0]
        assert flownode._flow[1] is forked_flownode._flow[1]
        assert flownode._flow[2] is forked_flownode._flow[2]
        # Sabotage joining for the second SFANode, which should not be joined,
        # causing AttributeError: 'NoneType' ... when it is joined.
        flownode._flow[2]._cov_mtx = None
        flownode.join(forked_flownode)

    def test_parallelnet(self):
        """Test a simple parallel net with big data.

        Includes ParallelFlowNode, ParallelCloneLayer, ParallelSFANode
        and training via a ParallelFlow.
        """
        noisenode = mdp.nodes.NormalNoiseNode(input_dim=20*20,
                                              noise_args=(0,0.0001))
        sfa_node = mdp.nodes.SFANode(input_dim=20*20, output_dim=10)
        switchboard = hinet.Rectangular2dSwitchboard(in_channels_xy=100,
                                                     field_channels_xy=20,
                                                     field_spacing_xy=10)
        flownode = mdp.hinet.FlowNode(mdp.Flow([noisenode, sfa_node]))
        sfa_layer = mdp.hinet.CloneLayer(flownode,
                                                switchboard.output_channels)
        flow = parallel.ParallelFlow([switchboard, sfa_layer])
        data_iterables = [None,
                          [n.random.random((10, 100*100)) for _ in xrange(3)]]
        scheduler = parallel.Scheduler()
        flow.train(data_iterables, scheduler=scheduler)

    def test_layer(self):
        """Test Simple random test with three nodes."""
        node1 = mdp.nodes.SFANode(input_dim=10, output_dim=5)
        node2 = mdp.nodes.SFANode(input_dim=17, output_dim=3)
        node3 = mdp.nodes.SFANode(input_dim=3, output_dim=1)
        layer = mdp.hinet.Layer([node1, node2, node3])
        flow = parallel.ParallelFlow([layer])
        data_iterables = [[n.random.random((10, 30)) for _ in xrange(3)]]
        scheduler = parallel.Scheduler()
        flow.train(data_iterables, scheduler=scheduler)


