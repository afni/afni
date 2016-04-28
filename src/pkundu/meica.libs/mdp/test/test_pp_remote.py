## import mdp.parallel as parallel
## from _tools import *

## from test_pp_local import requires_parallel_python

## remote_slaves = [("localhost", 2)]

## @requires_parallel_python
## def test_simple():
##     scheduler = parallel.pp_support.NetworkPPScheduler(
##         remote_slaves=remote_slaves,
##         timeout=60,
##         verbose=False)
##     # process jobs
##     for i in range(30):
##         scheduler.add_task(i, parallel.SqrTestCallable())
##     results = scheduler.get_results()
##     scheduler.shutdown()
##     # check result
##     results.sort()
##     results = numx.array(results)
##     assert numx.all(results[:6] == numx.array([0,1,4,9,16,25]))
