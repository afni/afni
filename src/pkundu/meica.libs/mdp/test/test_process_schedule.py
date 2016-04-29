from __future__ import with_statement
from _tools import *

import mdp.parallel as parallel
n = numx

def test_process_scheduler_shutdown():
    """Test that we can properly shutdown the subprocesses"""
    scheduler = parallel.ProcessScheduler(verbose=False,
                                          n_processes=1,
                                          source_paths=None,
                                          cache_callable=False)
    scheduler.shutdown()

def test_process_scheduler_order():
    """Test the correct result order in process scheduler."""
    scheduler = parallel.ProcessScheduler(verbose=False,
                                          n_processes=3,
                                          source_paths=None)
    max_i = 8
    for i in xrange(max_i):
        scheduler.add_task((n.arange(0,i+1), (max_i-1-i)*1.0/4),
                           parallel.SleepSqrTestCallable())
    results = scheduler.get_results()
    scheduler.shutdown()
    # check result
    results = n.concatenate(results)
    assert n.all(results ==
                     n.concatenate([n.arange(0,i+1)**2
                                    for i in xrange(max_i)]))

def test_process_scheduler_no_cache():
    """Test process scheduler with caching turned off."""
    scheduler = parallel.ProcessScheduler(verbose=False,
                                          n_processes=2,
                                          source_paths=None,
                                          cache_callable=False)
    for i in xrange(8):
        scheduler.add_task(i, parallel.SqrTestCallable())
    results = scheduler.get_results()
    scheduler.shutdown()
    # check result
    results = n.array(results)
    assert n.all(results == n.array([0,1,4,9,16,25,36,49]))
    
def test_process_scheduler_manager():
    """Test process scheduler with context manager itnerface."""
    with parallel.ProcessScheduler(n_processes=2,
                                   source_paths=None) as scheduler:
        for i in xrange(8):
            scheduler.add_task(i, parallel.SqrTestCallable())
        results = scheduler.get_results()
    # check result
    results = n.array(results)
    assert n.all(results == n.array([0,1,4,9,16,25,36,49]))

def test_process_scheduler_flow():
    """Test process scheduler with real Nodes."""
    precision = 6
    node1 = mdp.nodes.PCANode(output_dim=20)
    node2 = mdp.nodes.PolynomialExpansionNode(degree=1)
    node3 = mdp.nodes.SFANode(output_dim=10)
    flow = mdp.parallel.ParallelFlow([node1, node2, node3])
    parallel_flow = mdp.parallel.ParallelFlow(flow.copy()[:])
    input_dim = 30
    scales = n.linspace(1, 100, num=input_dim)
    scale_matrix = mdp.numx.diag(scales)
    train_iterables = [n.dot(mdp.numx_rand.random((5, 100, input_dim)),
                             scale_matrix)
                       for _ in xrange(3)]
    x = mdp.numx.random.random((10, input_dim))
    with parallel.ProcessScheduler(verbose=False,
                                   n_processes=3,
                                   source_paths=None) as scheduler:
        parallel_flow.train(train_iterables, scheduler=scheduler)
        # test that parallel execution works as well
        # note that we need more chungs then processes to test caching
        parallel_flow.execute([x for _ in xrange(8)], scheduler=scheduler)
    # compare to normal flow
    flow.train(train_iterables)
    assert parallel_flow[0].tlen == flow[0].tlen
    y1 = flow.execute(x)
    y2 = parallel_flow.execute(x)
    assert_array_almost_equal(abs(y1), abs(y2), precision)

def test_process_scheduler_mdp_version():
    """Test that we are running the same mdp in subprocesses"""
    scheduler = parallel.ProcessScheduler(verbose=False,
                                          n_processes=2,
                                          source_paths=None,
                                          cache_callable=False)
    for i in xrange(2):
        scheduler.add_task(i, parallel.MDPVersionCallable())
    out = scheduler.get_results()
    scheduler.shutdown()
    # check that we get 2 identical dictionaries
    assert out[0] == out[1], 'Subprocesses did not run '\
        'the same MDP as the parent:\n%s\n--\n%s'%(out[0], out[1])
