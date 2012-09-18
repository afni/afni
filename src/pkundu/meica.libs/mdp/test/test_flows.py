from __future__ import with_statement

import tempfile
import pickle
import cPickle
import os
from _tools import *

uniform = numx_rand.random

def _get_default_flow(flow_class=mdp.Flow, node_class=BogusNode):
    flow = flow_class([node_class(),node_class(),node_class()])
    return flow

# CheckpointFunction used in testCheckpointFunction
class _CheckpointCollectFunction(mdp.CheckpointFunction):
    def __init__(self):
        self.classes = []

    # collect the classes of the nodes it checks
    def __call__(self, node):
        self.classes.append(node.__class__)


def testFlow():
    inp = numx.ones((100,3))
    flow = _get_default_flow()
    for i in xrange(len(flow)):
        assert not flow.flow[i].is_training(), \
               'Training of node #%d has not been closed.' % i

    out = flow(inp)
    assert_array_equal(out,(2**len(flow))*inp)
    rec = flow.inverse(out)
    assert_array_equal(rec,inp)

def testFlow_copy():
    dummy_list = [1,2,3]
    flow = _get_default_flow()
    flow[0].dummy_attr = dummy_list
    copy_flow = flow.copy()
    assert flow[0].dummy_attr == copy_flow[0].dummy_attr, \
           'Flow copy method did not work'
    copy_flow[0].dummy_attr[0] = 10
    assert flow[0].dummy_attr != copy_flow[0].dummy_attr, \
           'Flow copy method did not work'

def test_Flow_copy_with_lambda():
    generic_node = mdp.Node()
    generic_node.lambda_function = lambda: 1
    generic_flow = mdp.Flow([generic_node])
    generic_flow.copy()
    
def testFlow_save():
    dummy_list = [1,2,3]
    flow = _get_default_flow()
    flow[0].dummy_attr = dummy_list
    # test string save
    copy_flow_pic = flow.save(None)
    copy_flow = cPickle.loads(copy_flow_pic)
    assert flow[0].dummy_attr == copy_flow[0].dummy_attr, \
           'Flow save (string) method did not work'
    copy_flow[0].dummy_attr[0] = 10
    assert flow[0].dummy_attr != copy_flow[0].dummy_attr, \
           'Flow save (string) method did not work'
    # test file save
    dummy_file = tempfile.mktemp(prefix='MDP_', suffix=".pic",
                                 dir=py.test.mdp_tempdirname)
    flow.save(dummy_file, protocol=1)
    dummy_file = open(dummy_file, 'rb')
    copy_flow = cPickle.load(dummy_file)
    assert flow[0].dummy_attr == copy_flow[0].dummy_attr, \
           'Flow save (file) method did not work'
    copy_flow[0].dummy_attr[0] = 10
    assert flow[0].dummy_attr != copy_flow[0].dummy_attr, \
           'Flow save (file) method did not work'

def testFlow_container_privmethods():
    mat,mix,inp = get_random_mix(mat_dim=(100,3))
    flow = _get_default_flow()
    # test __len__
    assert_equal(len(flow), len(flow.flow))
    # test __?etitem__, integer key
    for i in xrange(len(flow)):
        assert flow[i]==flow.flow[i], \
               '__getitem__  returned wrong node %d' % i
        new_node = BogusNode()
        flow[i] = new_node
        assert flow[i]==new_node, '__setitem__ did not set node %d' % i
    # test __?etitem__, normal slice -> this fails for python < 2.2 and
    # if Flow is a subclassed from builtin 'list'
    flowslice = flow[0:2]
    assert isinstance(flowslice,mdp.Flow), \
           '__getitem__ slice is not a Flow instance'
    assert len(flowslice) == 2, '__getitem__ returned wrong slice size'
    new_nodes_list = [BogusNode(), BogusNode()]
    flow[:2] = new_nodes_list
    assert (flow[0] == new_nodes_list[0]) and \
           (flow[1] == new_nodes_list[1]), '__setitem__ did not set slice'
    # test__?etitem__, extended slice
    flowslice = flow[:2:1]
    assert isinstance(flowslice,mdp.Flow), \
           '__getitem__ slice is not a Flow instance'
    assert len(flowslice) == 2, '__getitem__ returned wrong slice size'
    new_nodes_list = [BogusNode(), BogusNode()]
    flow[:2:1] = new_nodes_list
    assert (flow[0] == new_nodes_list[0]) and \
           (flow[1] == new_nodes_list[1]), '__setitem__ did not set slice'
    # test __delitem__, integer key
    copy_flow = mdp.Flow(flow[:])
    del copy_flow[0]
    assert len(copy_flow) == len(flow)-1, '__delitem__ did not del'
    for i in xrange(len(copy_flow)):
        assert copy_flow[i] == flow[i+1], '__delitem__ deleted wrong node'
    # test __delitem__, normal slice
    copy_flow = mdp.Flow(flow[:])
    del copy_flow[:2]
    assert len(copy_flow) == len(flow)-2, \
           '__delitem__ did not del normal slice'
    assert copy_flow[0] == flow[2], \
           '__delitem__ deleted wrong normal slice'
    # test __delitem__, extended slice
    copy_flow = mdp.Flow(flow[:])
    del copy_flow[:2:1]
    assert len(copy_flow) == len(flow)-2, \
           '__delitem__ did not del extended slice'
    assert copy_flow[0] == flow[2], \
           '__delitem__ deleted wrong extended slice'
    # test __add__
    newflow = flow + flow
    assert len(newflow) == len(flow)*2, '__add__ did not work'

def testFlow_container_listmethods():
    # for all methods try using a node with right dimensionality
    # and one with wrong dimensionality
    flow = _get_default_flow()
    length = len(flow)
    # we test __contains__ and __iter__ with the for loop
    for node in flow:
        node.input_dim = 10
        node.output_dim = 10
    # append
    newnode = BogusNode(input_dim=10, output_dim=10)
    flow.append(newnode)
    assert_equal(len(flow), length+1)
    length = len(flow)
    try:
        newnode = BogusNode(input_dim=11)
        flow.append(newnode)
        raise Exception, 'flow.append appended inconsistent node'
    except ValueError:
        assert_equal(len(flow), length)
    # extend
    newflow = flow.copy()
    flow.extend(newflow)
    assert_equal(len(flow), 2*length)
    length = len(flow)
    try:
        newflow = _get_default_flow()
        for idx in xrange(len(newflow)):
            if idx == 0:
                newflow[idx].input_dim = 11
            else:
                newflow[idx].input_dim = 10
            newflow[idx].output_dim = 10
        flow.extend(newflow)
        raise Exception, 'flow.extend appended inconsistent flow'
    except ValueError:
        assert_equal(len(flow), length)
    # insert
    newnode = BogusNode(input_dim=10, output_dim=None)
    flow.insert(2, newnode)
    assert_equal(len(flow), length+1)
    length = len(flow)
    try:
        newnode = BogusNode(output_dim=11)
        flow.insert(2, newnode)
        raise Exception, 'flow.insert inserted inconsistent node'
    except ValueError:
        assert_equal(len(flow), length)
    # pop
    oldnode = flow[5]
    popnode = flow.pop(5)
    assert oldnode == popnode, 'flow.pop popped wrong node out'
    assert_equal(len(flow), length-1)
    # pop - test Flow._check_nodes_consistency
    flow = _get_default_flow() + _get_default_flow()
    length = len(flow)
    flow[3].output_dim = 2
    flow[4].input_dim = 2
    flow[4].output_dim = 3
    flow[5].input_dim = 3
    flow._check_nodes_consistency(flow.flow)
    try:
        nottobepopped = flow.pop(4)
        raise Exception, 'flow.pop left inconsistent flow'
    except ValueError:
        assert_equal(len(flow), length)

def testFlow_append_node_copy():
    # when appending a node to a flow,
    # we don't want the flow to be a copy!
    node1 = BogusNode()
    node2 = BogusNode()
    flow = mdp.Flow([node1])
    flow += node2
    assert flow[0] is node1
    
def testFlow_iadd():
    # check that in-place adding to flow does not return new flow
    node1 = BogusNode()
    node2 = BogusNode()
    node3 = BogusNode()
    flow = mdp.Flow([node1])
    oldflow = flow
    flow += node2
    assert oldflow is flow
    flow += mdp.Flow([node3])
    assert oldflow is flow

def testFlow_as_sum_of_nodes():
    node1 = BogusNode()
    node2 = BogusNode()
    flow = node1+node2
    assert type(flow) is mdp.Flow
    assert len(flow) == 2
    node3 = BogusNode()
    flow = node1+node2+node3
    assert type(flow) is mdp.Flow
    assert len(flow) == 3
    node4 = BogusNode()
    flow = node4 + flow
    assert type(flow) is mdp.Flow
    assert len(flow) == 4

def testFlowWrongItarableException():
    samples = mdp.numx_rand.random((100,10))
    labels = mdp.numx.arange(100)
    flow = mdp.Flow([mdp.nodes.PCANode(), mdp.nodes.FDANode()])
    try:
        flow.train([[samples], [samples, labels]])
        # correct line would be (note the second iterable):
        #    flow.train([[[samples]], [[samples, labels]]])
        # should trigger exception for missing train argument for FDANode
        err = "Flow did not raise FlowException for wrong iterable."
        raise Exception(err)
    except mdp.FlowException:
        pass
    try:
        # try to give one argument too much!
        flow.train([[[samples]], [[samples, labels, labels]]])
        err = "Flow did not raise FlowException for wrong iterable."
        raise Exception(err)
    except mdp.FlowException:
        pass


def testCheckpointFlow():
    lst = []
    # checkpoint function, it collects a '1' for each call
    def cfunc(node, lst = lst):
        lst.append(1)
    mat,mix,inp = get_random_mix(mat_dim=(100,3))
    flow = _get_default_flow(flow_class = mdp.CheckpointFlow,
                                  node_class = BogusNodeTrainable)
    flow.train(inp, cfunc)
    #
    assert len(lst)==len(flow), \
           'The checkpoint function has been called %d times instead of %d times.' % (len(lst), len(flow))

def testCheckpointFunction():
    cfunc = _CheckpointCollectFunction()
    mat,mix,inp = get_random_mix(mat_dim=(100,3))
    flow = _get_default_flow(flow_class = mdp.CheckpointFlow,
                                  node_class = BogusNodeTrainable)
    flow.train(inp, cfunc)
    #
    for i in xrange(len(flow)):
        assert flow[i].__class__==cfunc.classes[i], 'Wrong class collected'

def testCrashRecovery():
    flow = mdp.Flow([BogusExceptNode()])
    flow.set_crash_recovery(1)
    try:
        flow.train(mdp.numx.zeros((1,2), 'd'))
    except Exception, e:
        assert isinstance(e,mdp.FlowExceptionCR)
        with open(e.filename, 'rb') as fl:
            pic_flow = pickle.load(fl)
        os.remove(e.filename)
        assert flow[0].bogus_attr == pic_flow[0].bogus_attr
    flow.set_crash_recovery(0)
    try:
        flow.execute([None])
    except Exception, e:
        assert isinstance(e,mdp.FlowExceptionCR)
        assert not hasattr(e,'filename')

def testCrashRecoveryException():
    a = 3
    try:
        raise mdp.CrashRecoveryException('bogus errstr', a, StandardError())
    except mdp.CrashRecoveryException, e:
        filename1 = e.dump()
        filename2 = e.dump(tempfile.mkstemp(prefix='MDP_',
                                            dir=py.test.mdp_tempdirname)[1])
        assert isinstance(e.parent_exception, StandardError)

    for fname in filename1, filename2:
        fl = open(fname, 'rb')
        obj = pickle.load(fl)
        fl.close()
        try:
            os.remove(fname)
        except Exception:
            pass
        assert obj == a

def testMultiplePhases():
    # test basic multiple phase sequence
    flow = mdp.Flow([BogusMultiNode()])
    flow.train(mdp.numx.zeros((1,2), 'd'))
    assert flow[0].visited == [1,2,3,4]
    # try to use an iterable to train it, check for rewinds
    class TestIterable:
        def __init__(self):
            self.used = 0
        def __iter__(self):
            self.used += 1
            yield mdp.numx.zeros((1,2), 'd')
    flow = mdp.Flow([BogusMultiNode()])
    iterable = TestIterable()
    flow.train([iterable])
    assert iterable.used == 2
    # should not work with an iterator
    def testgenerator():
        yield mdp.numx.zeros((1,2), 'd')
    flow = mdp.Flow([BogusMultiNode()])
    try:
        flow.train([testgenerator()])
        raise Exception('Expected mdp.FlowException')
    except mdp.FlowException:
        pass
