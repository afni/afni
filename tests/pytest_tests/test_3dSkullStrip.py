#! /usr/bin/env python

import subprocess
import sys
from pathlib import Path

def test_3dSkullStrip():
    wd = Path(__file__).parent.as_posix()
    print(wd)
    cp = subprocess.run('cd %s/test_dirs/3dSkullStrip && tcsh runit'% wd,
                        check=True,
                        close_fds=True,
                        shell=True)
