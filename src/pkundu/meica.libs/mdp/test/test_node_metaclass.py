from __future__ import with_statement

import mdp
import inspect
X = mdp.numx_rand.random(size=(500,5))

def get_signature(func):
    regargs, varargs, varkwargs, defaults = inspect.getargspec(func)
    return inspect.formatargspec(regargs, varargs, varkwargs, defaults,
                                 formatvalue=lambda value: "")[1:-1]

def test_docstrings():
    # first try on a subclass of Node if
    # the docstring is exported to the public method
    class AncestorNode(mdp.Node):
        def _train(self, x):
            """doc ancestor"""
            self.foo = 42
    anode = AncestorNode()
    assert anode.train.__doc__ == "doc ancestor"
    anode.train(X)
    assert anode.foo == 42
    assert get_signature(anode.train) == 'self, x'


    # now try on a subclass of it
    class ChildNode(AncestorNode):
        def _train(self, x):
            """doc child"""
            self.foo2 = 42
    cnode = ChildNode()
    assert cnode.train.__doc__ == "doc child"
    cnode.train(X)
    assert cnode.foo2 == 42
    assert get_signature(cnode.train) == 'self, x'

def test_signatures_no_doc():
    # first try on a subclass of Node if
    # the signature is exported to the public method
    class AncestorNode(mdp.Node):
        def _train(self, x, foo=None):
            self.foo = 42
    anode = AncestorNode()
    anode.train(X, foo='abc')
    assert anode.foo == 42
    assert get_signature(anode.train) == 'self, x, foo'
    
    # now try on a subclass of it
    class ChildNode(AncestorNode):
        def _train(self, x, foo2=None):
            self.foo2 = 42
    cnode = ChildNode()
    cnode.train(X, foo2='abc')
    assert cnode.foo2 == 42
    assert get_signature(cnode.train) == 'self, x, foo2'

def test_signatures_with_doc_in_both():
    # first try on a subclass of Node if
    # the signature and the docstring are exported to
    # the public method
    class AncestorNode(mdp.Node):
        def _train(self, x, foo=None):
            """doc ancestor"""
            self.foo = 42
    anode = AncestorNode()
    assert anode.train.__doc__ == "doc ancestor"
    anode.train(X, foo='abc')
    assert anode.foo == 42
    assert get_signature(anode.train) == 'self, x, foo'
    
    # now try on a subclass of it
    class ChildNode(AncestorNode):
        def _train(self, x, foo2=None):
            """doc child"""
            self.foo2 = 42
    cnode = ChildNode()
    assert cnode.train.__doc__ == "doc child"
    cnode.train(X, foo2='abc')
    assert cnode.foo2 == 42
    assert get_signature(cnode.train) == 'self, x, foo2'

def test_signatures_with_doc_in_ancestor():
    # first try on a subclass of Node if
    # the signature and the docstring are exported to
    # the public method
    class AncestorNode(mdp.Node):
        def _train(self, x, foo=None):
            """doc ancestor"""
            self.foo = 42
    anode = AncestorNode()
    assert anode.train.__doc__ == "doc ancestor"
    anode.train(X, foo='abc')
    assert anode.foo == 42
    assert get_signature(anode.train) == 'self, x, foo'

    # now try on a subclass of it
    class ChildNode(AncestorNode):
        def _train(self, x, foo2=None):
            self.foo2 = 42
    cnode = ChildNode()
    assert cnode.train.__doc__ == "doc ancestor"
    cnode.train(X, foo2='abc')
    assert cnode.foo2 == 42
    assert get_signature(cnode.train) == 'self, x, foo2'

def test_signatures_with_doc_in_child():
    # first try on a subclass of Node if
    # the signature and the docstring are exported to
    # the public method
    class AncestorNode(mdp.Node):
        def _train(self, x, foo=None):
            self.foo = 42
    anode = AncestorNode()
    anode.train(X, foo='abc')
    assert anode.foo == 42
    assert get_signature(anode.train) == 'self, x, foo'

    # now try on a subclass of it
    class ChildNode(AncestorNode):
        def _train(self, x, foo2=None):
            """doc child"""
            self.foo2 = 42
    cnode = ChildNode()
    assert cnode.train.__doc__ == "doc child"
    cnode.train(X, foo2='abc')
    assert cnode.foo2 == 42
    assert get_signature(cnode.train) == 'self, x, foo2'
