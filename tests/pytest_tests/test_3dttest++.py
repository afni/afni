#! /usr/bin/env python

import subprocess
import sys
from pathlib import Path

def test_3dttest__plus____plus__():
    wd = Path(__file__).parent.as_posix()
    print(wd)
    cp = subprocess.run('cd %s/test_dirs/3dttest++ && tcsh runit'% wd,
                        check=True,
                        close_fds=True,
                        shell=True)
