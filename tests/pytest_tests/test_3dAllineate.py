#! /usr/bin/env python

import subprocess
import sys
from pathlib import Path
from test_utils.diff import diff_parser


def test_3dAllineate():
    wd = Path(__file__).parent.as_posix()
    print(wd)
    cp = subprocess.run('cd %s/test_dirs/3dAllineate && tcsh runit'% wd,
                        check=True,
                        close_fds=True,
                        shell=True)
