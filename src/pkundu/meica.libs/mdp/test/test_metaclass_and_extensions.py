from __future__ import with_statement

import mdp
import inspect
import py.test
X = mdp.numx_rand.random(size=(500,5))

def get_signature(func):
    regargs, varargs, varkwargs, defaults = inspect.getargspec(func)
    return inspect.formatargspec(regargs, varargs, varkwargs, defaults,
                                 formatvalue=lambda value: "")[1:-1]

def teardown_function(function):
    """Deactivate all extensions and remove testing extensions."""
    mdp.deactivate_extensions(mdp.get_active_extensions())
    for key in mdp.get_extensions().copy():
        if key.startswith("__test"):
            del mdp.get_extensions()[key]

def test_signatures_same_no_arguments():
    class AncestorNode(mdp.Node):
        def _train(self, x, foo2=None):
            self.foo2 = None
    class ChildNode(AncestorNode):
        def _train(self, x, foo=None):
            self.foo = foo
    cnode = ChildNode()
    assert get_signature(cnode.train) == 'self, x, foo'
    assert get_signature(cnode._train) == 'self, x, foo'
    cnode.train(X, foo=42)
    assert cnode.foo == 42
    py.test.raises(AttributeError, 'cnode.foo2')

def test_signatures_more_arguments():
    class AncestorNode(mdp.Node):
        def _train(self, x):
            self.foo2 = None
    class ChildNode(AncestorNode):
        def _train(self, x, foo=None):
            self.foo = foo
    cnode = ChildNode()
    assert get_signature(cnode.train) == 'self, x, foo'
    assert get_signature(cnode.train._undecorated_) == 'self, x, *args, **kwargs'
    assert get_signature(cnode._train) == 'self, x, foo'
    # next two lines should give the same:
    cnode.train._undecorated_(cnode, X, foo=42)
    cnode.train(X, foo=42)
    assert cnode.foo == 42
    py.test.raises(AttributeError, 'cnode.foo2')

def test_signatures_less_arguments():

    class AncestorNode(mdp.Node):
        def _train(self, x, foo=None):
            self.foo = None

    class ChildNode(AncestorNode):
        def _train(self, x):
            self.moo = 3

    cnode = ChildNode()
    assert get_signature(cnode.train) == 'self, x'
    assert get_signature(cnode.train._undecorated_) == 'self, x, *args, **kwargs'
    assert get_signature(cnode._train) == 'self, x'

    # next two lines should give the same:
    cnode.train._undecorated_(cnode, X)
    cnode.train(X)
    assert cnode.moo == 3
    py.test.raises(AttributeError, 'cnode.foo')

def test_simple_extension():

    class TestExtensionNode(mdp.ExtensionNode, mdp.nodes.IdentityNode):
        extension_name = "__test"
        def execute(self, x):
            self.foo = 42
            return self._non_extension_execute(x)

    class Dummy(mdp.nodes.IdentityNode):
        def _execute(self, x):
            return 42

    node = mdp.nodes.IdentityNode()
    assert mdp.numx.all(node.execute(X) == X)
    assert not hasattr(node,'foo')

    with mdp.extension("__test"):
        assert mdp.numx.all(node.execute(X) == X)
        assert hasattr(node,'foo')

    node = Dummy()
    assert not hasattr(node,'foo')
    assert node.execute(X) == 42

    with mdp.extension("__test"):
        assert node.execute(X) == 42
        assert hasattr(node,'foo')
