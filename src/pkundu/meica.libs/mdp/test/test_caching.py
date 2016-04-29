"""Test caching extension."""
from __future__ import with_statement
import tempfile
from _tools import *

requires_joblib = skip_on_condition(
    "not mdp.config.has_joblib",
    "This test requires the 'joblib' module.")


_counter = 0
class _CounterNode(mdp.Node):
    def __init__(self):
        super(_CounterNode, self).__init__()

    def is_trainable(self):
        return False

    def _execute(self, x):
        """The execute method has the side effect of increasing
        a global counter by one."""
        global _counter
        _counter += 1
        return x

@requires_joblib
def test_caching_extension():
    """Test that the caching extension is working at the global level."""

    global _counter
    _counter = 0
    node = _CounterNode()

    # before decoration the global counter is incremented at every call
    k = 0
    for i in range(3):
        x = mdp.numx.array([[i]], dtype='d')
        for j in range(2):
            k += 1
            assert mdp.numx.all(node.execute(x) == x)
            assert _counter == k

    # reset counter
    _counter = 0
    # activate the extension
    cachedir = tempfile.mkdtemp(prefix='mdp-tmp-joblib-cache.',
                                dir=py.test.mdp_tempdirname)
    mdp.caching.activate_caching(cachedir=cachedir)
    assert mdp.get_active_extensions() == ['cache_execute']

    # after decoration the global counter is incremented for each new 'x'
    for i in range(3):
        x = mdp.numx.array([[i]], dtype='d')
        for _ in range(2):
            assert mdp.numx.all(node.execute(x) == x)
            assert _counter == i + 1

    # after deactivation
    mdp.caching.deactivate_caching()
    assert mdp.get_active_extensions() == []
    # reset counter
    _counter = 0

    k = 0
    for i in range(3):
        x = mdp.numx.array([[i]], dtype='d')
        for j in range(2):
            k += 1
            assert mdp.numx.all(node.execute(x) == x)
            assert _counter == k

@requires_joblib
def test_different_instances_same_content():
    global _counter
    x = mdp.numx.array([[100.]], dtype='d')

    cachedir = tempfile.mkdtemp(prefix='mdp-tmp-joblib-cache.',
                                dir=py.test.mdp_tempdirname)
    mdp.caching.activate_caching(cachedir=cachedir)
    node = _CounterNode()
    _counter = 0

    # add attribute to make instance unique
    node.attr = 'unique'

    # cache x
    node.execute(x)
    assert _counter == 1
    # should be cached now
    node.execute(x)
    assert _counter == 1

    # create new instance, make it also unique and check that
    # result is still cached
    _counter = 0
    node = _CounterNode()
    node.attr = 'unique and different'
    node.execute(x)
    assert _counter == 1

    mdp.caching.deactivate_caching()

@requires_joblib
def test_caching_context_manager():
    global _counter
    node = _CounterNode()
    _counter = 0

    assert mdp.get_active_extensions() == []
    cachedir = tempfile.mkdtemp(prefix='mdp-tmp-joblib-cache.',
                                dir=py.test.mdp_tempdirname)
    with mdp.caching.cache(cachedir=cachedir):
        assert mdp.get_active_extensions() == ['cache_execute']

        for i in range(3):
            x = mdp.numx.array([[i]], dtype='d')
            for _ in range(2):
                assert mdp.numx.all(node.execute(x) == x)
                assert _counter == i + 1
    assert mdp.get_active_extensions() == []

@requires_joblib
def test_class_caching():
    """Test that we can cache individual classes."""
    cached = mdp.nodes.PCANode()
    notcached = mdp.nodes.SFANode()
    with mdp.caching.cache(cache_classes=[mdp.nodes.PCANode]):
        assert cached.is_cached()
        assert not notcached.is_cached()

@requires_joblib
def test_class_caching_functionality():
    """Test that cached classes really cache."""
    global _counter
    x = mdp.numx.array([[210]], dtype='d')

    node = _CounterNode()

    # here _CounterNode is not cached
    _counter = 0
    with mdp.caching.cache(cache_classes=[mdp.nodes.PCANode]):
        node.execute(x)
        assert _counter == 1
        node.execute(x)
        assert _counter == 2

    # here _CounterNode is cached
    _counter = 0
    with mdp.caching.cache(cache_classes=[_CounterNode]):
        node.execute(x)
        assert _counter == 1
        node.execute(x)
        assert _counter == 1

@requires_joblib
def test_instance_caching():
    """Test that we can cache individual instances."""
    cached = mdp.nodes.PCANode()
    notcached = mdp.nodes.PCANode()
    with mdp.caching.cache(cache_instances=[cached]):
        assert cached.is_cached()
        assert not notcached.is_cached()

@requires_joblib
def test_instance_caching_functionality():
    """Test that cached instances really cache."""
    global _counter
    x = mdp.numx.array([[130]], dtype='d')

    node = _CounterNode()
    othernode = _CounterNode()

    # here _CounterNode is not cached
    _counter = 0
    with mdp.caching.cache(cache_instances=[othernode]):
        node.execute(x)
        assert _counter == 1
        node.execute(x)
        assert _counter == 2

    # here _CounterNode is cached
    _counter = 0
    with mdp.caching.cache(cache_instances=[node]):
        node.execute(x)
        assert _counter == 1
        node.execute(x)
        assert _counter == 1

@requires_joblib
def test_preexecution_problem():
    """Test that automatic setting of e.g. input_dim does not stop
    the caching extension from caching on the first run."""
    global _counter
    x = mdp.numx.array([[102.]])

    node = _CounterNode()

    # here _CounterNode is cached
    _counter = 0
    with mdp.caching.cache():
        # on the first execution, input_dim and dtype are set ...
        node.execute(x)
        assert _counter == 1
        # ... yet the result is cached
        node.execute(x)
        assert _counter == 1

@requires_joblib
def test_switch_cache():
    """Test changing cache directory while extension is active."""
    global _counter

    dir1 = tempfile.mkdtemp(prefix='mdp-tmp-joblib-cache.',
                            dir=py.test.mdp_tempdirname)
    dir2 = tempfile.mkdtemp(prefix='mdp-tmp-joblib-cache.',
                            dir=py.test.mdp_tempdirname)
    x = mdp.numx.array([[10]], dtype='d')

    mdp.caching.activate_caching(cachedir=dir1)

    node = _CounterNode()
    _counter = 0
    node.execute(x)
    assert _counter == 1
    node.execute(x)
    assert _counter == 1

    # now change path
    mdp.caching.set_cachedir(cachedir=dir2)
    node.execute(x)
    assert _counter == 2
    node.execute(x)
    assert _counter == 2

    mdp.caching.deactivate_caching()


@requires_joblib
def test_execute_magic():
    """Test calling execute with magic while caching."""
    x = mdp.numx_rand.rand(100, 10)

    node = mdp.nodes.PCANode()

    with mdp.caching.cache():
        y = node(x)
        y2 = node(x)
        assert_array_equal(y, y2)
