import mdp.parallel as parallel
from _tools import *

requires_parallel_python = skip_on_condition(
    "not mdp.config.has_parallel_python",
    "This test requires Parallel Python")


@requires_parallel_python
def test_reverse_patching():
    # revert pp patching
    # XXX This is needed to avoid failures of the other
    # XXX pp tests when run more then once in the same interpreter
    # XXX session
    if hasattr(mdp.config, 'pp_monkeypatch_dirname'):
        import pp
        pp._Worker.command = mdp._pp_worker_command[:]
        parallel.pp_support._monkeypatch_pp(mdp.config.pp_monkeypatch_dirname)


@requires_parallel_python
def test_simple():
    """Test local pp scheduling."""
    scheduler = parallel.pp_support.LocalPPScheduler(ncpus=2,
                                                     max_queue_length=0,
                                                     verbose=False)
    # process jobs
    for i in range(50):
        scheduler.add_task(i, parallel.SqrTestCallable())
    results = scheduler.get_results()
    scheduler.shutdown()
    # check result
    results.sort()
    results = numx.array(results[:6])
    assert numx.all(results == numx.array([0,1,4,9,16,25]))

@requires_parallel_python
def test_scheduler_flow():
    """Test local pp scheduler with real Nodes."""
    precision = 10**-6
    node1 = mdp.nodes.PCANode(output_dim=20)
    node2 = mdp.nodes.PolynomialExpansionNode(degree=1)
    node3 = mdp.nodes.SFANode(output_dim=10)
    flow = mdp.parallel.ParallelFlow([node1, node2, node3])
    parallel_flow = mdp.parallel.ParallelFlow(flow.copy()[:])
    scheduler = parallel.pp_support.LocalPPScheduler(ncpus=3,
                                                     max_queue_length=0,
                                                     verbose=False)
    input_dim = 30
    scales = numx.linspace(1, 100, num=input_dim)
    scale_matrix = mdp.numx.diag(scales)
    train_iterables = [numx.dot(mdp.numx_rand.random((5, 100, input_dim)),
                             scale_matrix)
                       for _ in range(3)]
    parallel_flow.train(train_iterables, scheduler=scheduler)
    x = mdp.numx.random.random((10, input_dim))
    # test that parallel execution works as well
    # note that we need more chungs then processes to test caching
    parallel_flow.execute([x for _ in range(8)], scheduler=scheduler)
    scheduler.shutdown()
    # compare to normal flow
    flow.train(train_iterables)
    assert parallel_flow[0].tlen == flow[0].tlen
    y1 = flow.execute(x)
    y2 = parallel_flow.execute(x)
    assert_array_almost_equal(abs(y1 - y2), precision)
