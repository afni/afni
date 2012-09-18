# global hooks for py.test
import tempfile
import os
import shutil
import glob
import mdp
import py.test

_err_str = """
IMPORTANT: some tests use random numbers. This could
occasionally lead to failures due to numerical degeneracies.
To rule this out, please run the tests more than once.
If you get reproducible failures please report a bug!
"""

def pytest_configure(config):
    seed = config.getvalue("seed")
    # if seed was not set by the user, we set one now
    if seed is None or seed == ('NO', 'DEFAULT'):
        config.option.seed = int(mdp.numx_rand.randint(2**31-1))

def pytest_unconfigure(config):
    # remove garbage created during tests
    # note that usage of TemporaryDirectory is not enough to assure
    # that all garbage is removed, expacially because we use subprocesses
    shutil.rmtree(py.test.mdp_tempdirname, ignore_errors=True)
    # if pp was monkey-patched, remove any stale pp4mdp directories
    if hasattr(mdp.config, 'pp_monkeypatch_dirname'):
        monkey_dirs = os.path.join(mdp.config.pp_monkeypatch_dirname,
                                   mdp.parallel.pp_support.TEMPDIR_PREFIX) 
        [shutil.rmtree(d, ignore_errors=True) for d in glob.glob(monkey_dirs+'*')]

def pytest_runtest_setup(item):
    # set random seed before running each test
    # so that a failure in a test can be reproduced just running
    # that particular test. if this was not done, you would need
    # to run the whole test suite again
    mdp.numx_rand.seed(item.config.option.seed)

def pytest_addoption(parser):
    """Add random seed option to py.test.
    """
    parser.addoption('--seed', dest='seed', type=int, action='store',
                     help='set random seed')

def pytest_report_header(config):
    # report the random seed before and after running the tests
    return '%s\nRandom Seed: %d\n' % (mdp.config.info(), config.option.seed)

def pytest_terminal_summary(terminalreporter):
    # add a note about error due to randomness only if an error or a failure
    # occured
    t = terminalreporter
    t.write_sep("=", "NOTE")
    t.write_line("%s\nRandom Seed: %d" % (mdp.config.info(),
                                          t.config.option.seed))
    if 'failed' in t.stats or 'error' in t.stats:
        t.write_line(_err_str)

def pytest_namespace():
    # get temporary directory to put temporary files
    # will be deleted at the end of the test run
    dirname = tempfile.mkdtemp(suffix='.tmp', prefix='MDPtestdir_')
    return dict(mdp_tempdirname=dirname)
