import subprocess
import sys

def test_3dSkullStrip():
    cp = subprocess.run('cd pytest_tests/test_dirs/3dSkullStrip && tcsh runit',
                        check=True,
                        close_fds=True,
                        shell=True)
