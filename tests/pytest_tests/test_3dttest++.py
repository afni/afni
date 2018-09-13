import subprocess
import sys

def test_3dttest__plus____plus__():
    cp = subprocess.run('cd pytest_tests/test_dirs/3dttest++ && tcsh runit',
                        check=True,
                        close_fds=True,
                        shell=True)
