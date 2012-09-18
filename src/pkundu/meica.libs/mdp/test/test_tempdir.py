from __future__ import with_statement
import tempfile
import os
import py.test

def test_tmpdir_exists():
    assert os.path.exists(py.test.mdp_tempdirname)

def test_tmpdir_writable1():
    with open(os.path.join(py.test.mdp_tempdirname, 'empty'), 'w'):
        pass

def test_tmpdir_writable2():
    with tempfile.NamedTemporaryFile(prefix='MDP_', suffix='.testfile',
                                     dir=py.test.mdp_tempdirname):
        pass
