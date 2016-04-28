from __future__ import with_statement
from _tools import *

import mdp.parallel as parallel
n = numx

# TODO: add test that the callable is forked exactly once before a call?

def test_scheduler():
    """Test scheduler with 6 tasks."""
    scheduler = parallel.Scheduler()
    for i in xrange(6):
        scheduler.add_task(i, lambda x: x**2)
    results = scheduler.get_results()
    scheduler.shutdown()
    # check result
    results = n.array(results)
    assert n.all(results == n.array([0,1,4,9,16,25]))
    
def test_scheduler_manager():
    """Test context manager interface for scheduler."""
    with parallel.Scheduler() as scheduler:
        for i in xrange(6):
            scheduler.add_task(i, lambda x: x**2)
        results = scheduler.get_results()
    assert n.all(results == n.array([0,1,4,9,16,25]))
    
def test_scheduler_manager_exception():
    """Test context manager interface for scheduler in case of an exception."""
    log = []
    class TestSchedulerException(Exception): pass
    class TestScheduler(parallel.Scheduler):
        def _shutdown(self):
            log.append("shutdown")
        def _process_task(self, data, task_callable, task_index):
            raise TestSchedulerException()
    try:
        with TestScheduler() as scheduler:
            for i in xrange(6):
                scheduler.add_task(i, lambda x: x**2)
            scheduler.get_results()
    except TestSchedulerException:
        pass
    assert log == ["shutdown"]

def test_cpu_count():
    """Test the cpu_count helper function."""
    n_cpus = parallel.cpu_count()
    assert isinstance(n_cpus, int)


def test_thread_scheduler_flow():
    """Test thread scheduler with real Nodes."""
    precision = 6
    node1 = mdp.nodes.PCANode(output_dim=20)
    node2 = mdp.nodes.PolynomialExpansionNode(degree=1)
    node3 = mdp.nodes.SFANode(output_dim=10)
    flow = mdp.parallel.ParallelFlow([node1, node2, node3])
    parallel_flow = mdp.parallel.ParallelFlow(flow.copy()[:])
    scheduler = parallel.ThreadScheduler(verbose=False,
                                         n_threads=3)
    input_dim = 30
    scales = n.linspace(1, 100, num=input_dim)
    scale_matrix = mdp.numx.diag(scales)
    train_iterables = [n.dot(mdp.numx_rand.random((5, 100, input_dim)),
                             scale_matrix)
                       for _ in xrange(3)]
    parallel_flow.train(train_iterables, scheduler=scheduler)
    x = mdp.numx.random.random((10, input_dim))
    # test that parallel execution works as well
    # note that we need more chungs then processes to test caching
    parallel_flow.execute([x for _ in xrange(8)], scheduler=scheduler)
    scheduler.shutdown()
    # compare to normal flow
    flow.train(train_iterables)
    assert parallel_flow[0].tlen == flow[0].tlen
    y1 = flow.execute(x)
    y2 = parallel_flow.execute(x)
    assert_array_almost_equal(abs(y1), abs(y2), precision)

