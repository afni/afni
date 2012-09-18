import sys
from _tools import *

def _list_module(module):
    try:
        names = module.__all__
    except AttributeError:
        names = dir(module)
    for name in names:
        if name.startswith('_'):
            continue
        item = getattr(module, name)
        try:
            modname = getattr(item, '__module__')
        except AttributeError:
            continue
        if hasattr(item, '__module__'):
            yield modname, name, item

MODULES = ['mdp',
           'mdp.nodes',
           'mdp.hinet',
           'mdp.parallel',
           'mdp.graph',
           'mdp.utils',
           ]

def pytest_generate_tests(metafunc):
    generate_calls(MODULES, metafunc)

def generate_calls(modules, metafunc):
    for module in modules:
        metafunc.addcall(funcargs=dict(parentname=module), id=module)

def test_exports(parentname):
    rootname = parentname.split('.')[-1]
    module = sys.modules[parentname]
    for modname, itemname, item in _list_module(module):
        parts = modname.split('.')
        assert (parts[0] != rootname or
                modname == parentname), \
                '%s.%s.__module_ == %s != %s' % (
                    parentname, itemname, item.__module__, parentname)
