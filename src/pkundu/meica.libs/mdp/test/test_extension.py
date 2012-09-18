from __future__ import with_statement

import mdp
import sys
import py.test

def teardown_function(function):
    """Deactivate all extensions and remove testing extensions."""
    mdp.deactivate_extensions(mdp.get_active_extensions())
    for key in mdp.get_extensions().copy():
        if key.startswith("__test"):
            del mdp.get_extensions()[key]

def testSimpleExtension():
    """Test for a single new extension."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _testtest(self):
            pass
        _testtest_attr = 1337
    class TestSFANode(TestExtensionNode, mdp.nodes.SFANode):
        def _testtest(self):
            return 42
        _testtest_attr = 1338
    sfa_node = mdp.nodes.SFANode()
    mdp.activate_extension("__test")
    assert sfa_node._testtest() == 42
    assert sfa_node._testtest_attr == 1338
    mdp.deactivate_extension("__test")
    assert not hasattr(mdp.nodes.SFANode, "_testtest")

def testContextDecorator():
    """Test the with_extension function decorator."""

    class Test1ExtensionNode(mdp.ExtensionNode):
        extension_name = "__test1"
        def _testtest(self):
            pass

    @mdp.with_extension("__test1")
    def test():
        return mdp.get_active_extensions()

    # check that the extension is activated
    assert mdp.get_active_extensions() == []
    active = test()
    assert active == ["__test1"]
    assert mdp.get_active_extensions() == []

    # check that it is only deactiveted if it was activated there
    mdp.activate_extension("__test1")
    active = test()
    assert active == ["__test1"]
    assert mdp.get_active_extensions() == ["__test1"]

def testContextManager1():
    """Test that the context manager activates extensions."""

    class Test1ExtensionNode(mdp.ExtensionNode):
        extension_name = "__test1"
        def _testtest(self):
            pass
    class Test2ExtensionNode(mdp.ExtensionNode):
        extension_name = "__test2"
        def _testtest(self):
            pass

    assert mdp.get_active_extensions() == []
    with mdp.extension('__test1'):
        assert mdp.get_active_extensions() == ['__test1']
    assert mdp.get_active_extensions() == []
    # with multiple extensions
    with mdp.extension(['__test1', '__test2']):
        active = mdp.get_active_extensions()
        assert '__test1' in active
        assert '__test2' in active
    assert mdp.get_active_extensions() == []
    mdp.activate_extension("__test1")
    # Test that only activated extensions are deactiveted.
    with mdp.extension(['__test1', '__test2']):
        active = mdp.get_active_extensions()
        assert '__test1' in active
        assert '__test2' in active
    assert mdp.get_active_extensions() == ["__test1"]

def testDecoratorExtension():
    """Test extension decorator with a single new extension."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _testtest(self):
            pass
    @mdp.extension_method("__test", mdp.nodes.SFANode, "_testtest")
    def _sfa_testtest(self):
        return 42
    @mdp.extension_method("__test", mdp.nodes.SFA2Node)
    def _testtest(self):
        return 42 + _sfa_testtest(self)
    sfa_node = mdp.nodes.SFANode()
    sfa2_node = mdp.nodes.SFA2Node()
    mdp.activate_extension("__test")
    assert sfa_node._testtest() == 42
    assert sfa2_node._testtest() == 84
    mdp.deactivate_extension("__test")
    assert not hasattr(mdp.nodes.SFANode, "_testtest")
    assert not hasattr(mdp.nodes.SFA2Node, "_testtest")

def testDecoratorInheritance():
    """Test inhertiance with decorators for a single new extension."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _testtest(self):
            pass
    @mdp.extension_method("__test", mdp.nodes.SFANode, "_testtest")
    def _sfa_testtest(self):
        return 42
    @mdp.extension_method("__test", mdp.nodes.SFA2Node)
    def _testtest(self):
        return 42 + super(mdp.nodes.SFA2Node, self)._testtest()
    sfa_node = mdp.nodes.SFANode()
    sfa2_node = mdp.nodes.SFA2Node()
    mdp.activate_extension("__test")
    assert sfa_node._testtest() == 42
    assert sfa2_node._testtest() == 84

def testExtensionInheritance():
    """Test inheritance of extension nodes."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _testtest(self):
            pass
    class TestSFANode(TestExtensionNode, mdp.nodes.SFANode):
        def _testtest(self):
            return 42
        _testtest_attr = 1337
    class TestSFA2Node(TestSFANode, mdp.nodes.SFA2Node):
        def _testtest(self):
            if sys.version_info[0] < 3:
                return TestSFANode._testtest.im_func(self)
            else:
                return TestSFANode._testtest(self)
    sfa2_node = mdp.nodes.SFA2Node()
    mdp.activate_extension("__test")
    assert sfa2_node._testtest() == 42
    assert sfa2_node._testtest_attr == 1337

def testExtensionInheritance2():
    """Test inheritance of extension nodes, using super."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _testtest(self):
            pass
    class TestSFANode(TestExtensionNode, mdp.nodes.SFANode):
        def _testtest(self):
            return 42
    class TestSFA2Node(mdp.nodes.SFA2Node, TestSFANode):
        def _testtest(self):
            return super(mdp.nodes.SFA2Node, self)._testtest()
    sfa2_node = mdp.nodes.SFA2Node()
    mdp.activate_extension("__test")
    assert sfa2_node._testtest() == 42

def testExtensionInheritance3():
    """Test explicit use of extension nodes and inheritance."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _testtest(self):
            pass
    class TestSFANode(TestExtensionNode, mdp.nodes.SFANode):
        def _testtest(self):
            return 42
    # Note the inheritance order, otherwise this would not work.
    class TestSFA2Node(mdp.nodes.SFA2Node, TestSFANode):
        def _testtest(self):
            return super(mdp.nodes.SFA2Node, self)._testtest()
    sfa2_node = TestSFA2Node()
    assert sfa2_node._testtest() == 42

def testMultipleExtensions():
    """Test behavior of multiple extensions."""
    class Test1ExtensionNode(mdp.ExtensionNode, mdp.Node):
        extension_name = "__test1"
        def _testtest1(self):
            pass
    class Test2ExtensionNode(mdp.ExtensionNode, mdp.Node):
        extension_name = "__test2"
        def _testtest2(self):
            pass
    mdp.activate_extension("__test1")
    node = mdp.Node()
    node._testtest1()
    mdp.activate_extension("__test2")
    node._testtest2()
    mdp.deactivate_extension("__test1")
    assert not hasattr(mdp.nodes.SFANode, "_testtest1")
    mdp.activate_extension("__test1")
    node._testtest1()
    mdp.deactivate_extensions(["__test1", "__test2"])
    assert not hasattr(mdp.nodes.SFANode, "_testtest1")
    assert not hasattr(mdp.nodes.SFANode, "_testtest2")

def testExtCollision():
    """Test the check for method name collision."""
    class Test1ExtensionNode(mdp.ExtensionNode, mdp.Node):
        extension_name = "__test1"
        def _testtest(self):
            pass
    class Test2ExtensionNode(mdp.ExtensionNode, mdp.Node):
        extension_name = "__test2"
        def _testtest(self):
            pass
    py.test.raises(mdp.ExtensionException,
                   mdp.activate_extensions, ["__test1", "__test2"])
    # none of the extension should be active after the exception
    assert not hasattr(mdp.Node, "_testtest")

def testExtensionInheritanceInjection():
    """Test the injection of inherited methods"""
    class TestNode(object):
        def _test1(self):
            return 0
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _test1(self):
            return 1
        def _test2(self):
            return 2
        def _test3(self):
            return 3
    class TestNodeExt(TestExtensionNode, TestNode):
        def _test2(self):
            return "2b"
    @mdp.extension_method("__test", TestNode)
    def _test4(self):
        return 4
    test_node = TestNode()
    mdp.activate_extension("__test")
    assert test_node._test1() == 1
    assert test_node._test2() == "2b"
    assert test_node._test3() == 3
    assert test_node._test4() == 4
    mdp.deactivate_extension("__test")
    assert test_node._test1() == 0
    assert not hasattr(test_node, "_test2")
    assert not hasattr(test_node, "_test3")
    assert not hasattr(test_node, "_test4")

def testExtensionInheritanceInjectionNonExtension():
    """Test non_extension method injection."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _execute(self):
            return 0
    class TestNode(mdp.Node):
        # no _execute method
        pass
    class ExtendedTestNode(TestExtensionNode, TestNode):
        pass
    test_node = TestNode()
    mdp.activate_extension('__test')
    assert hasattr(test_node, "_non_extension__execute")
    mdp.deactivate_extension('__test')
    assert not hasattr(test_node, "_non_extension__execute")
    assert not hasattr(test_node, "_extension_for__execute")
    # test that the non-native _execute has been completely removed
    assert "_execute" not in test_node.__class__.__dict__

def testExtensionInheritanceInjectionNonExtension2():
    """Test non_extension method injection."""
    class TestExtensionNode(mdp.ExtensionNode):
        extension_name = "__test"
        def _execute(self):
            return 0
    class TestNode(mdp.Node):
        def _execute(self):
            return 1
    class ExtendedTestNode(TestExtensionNode, TestNode):
        pass
    test_node = TestNode()
    mdp.activate_extension('__test')
    # test that non-extended attribute has been added as well
    assert hasattr(test_node, "_non_extension__execute")
    mdp.deactivate_extension('__test')
    assert not hasattr(test_node, "_non_extension__execute")
    assert not hasattr(test_node, "_extension_for__execute")
    # test that the native _execute has been preserved
    assert "_execute" in test_node.__class__.__dict__

def testExtensionInheritanceTwoExtensions():
    """Test non_extension injection for multiple extensions."""
    class Test1ExtensionNode(mdp.ExtensionNode):
        extension_name = "__test1"
        def _execute(self):
            return 1
    class Test2ExtensionNode(mdp.ExtensionNode):
        extension_name = "__test2"
    class Test3ExtensionNode(mdp.ExtensionNode):
        extension_name = "__test3"
        def _execute(self):
            return "3a"
    class TestNode1(mdp.Node):
        pass
    class TestNode2(TestNode1):
        pass
    class ExtendedTest1Node2(Test1ExtensionNode, TestNode2):
        pass
    class ExtendedTest2Node1(Test2ExtensionNode, TestNode1):
        def _execute(self):
            return 2
    class ExtendedTest3Node1(Test3ExtensionNode, TestNode1):
        def _execute(self):
            return "3b"
    test_node = TestNode2()
    mdp.activate_extension('__test2')
    assert test_node._execute() == 2
    mdp.deactivate_extension('__test2')
    # in this order TestNode2 should get execute from __test1,
    # the later addition by __test1 to TestNode1 doesn't matter
    mdp.activate_extensions(['__test1', '__test2'])
    assert test_node._execute() == 1
    mdp.deactivate_extensions(['__test2', '__test1'])
    # now activate in inverse order
    # TestNode2 already gets _execute from __test2, but that is still
    # overriden by __test1, thats how its registered in _extensions
    mdp.activate_extensions(['__test2', '__test1'])
    assert test_node._execute() == 1
    mdp.deactivate_extensions(['__test2', '__test1'])
    ## now the same with extension 3
    mdp.activate_extension('__test3')
    assert test_node._execute() == "3b"
    mdp.deactivate_extension('__test3')
    # __test3 does not override, since the _execute slot for Node2
    # was first filled by __test1
    mdp.activate_extensions(['__test3', '__test1'])
    assert test_node._execute() == 1
    mdp.deactivate_extensions(['__test3', '__test1'])
    # inverse order
    mdp.activate_extensions(['__test1', '__test3'])
    assert test_node._execute() == 1
    mdp.deactivate_extensions(['__test2', '__test1'])
