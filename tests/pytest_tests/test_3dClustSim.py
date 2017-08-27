#! /usr/bin/env python

import subprocess
import sys
from pathlib import Path
import pandas as pd
import numpy as np
from test_utils.diff import diff_parser


def test_3dClustSim():
    wd = Path(__file__).parent.as_posix()
    print(wd)
    cp = subprocess.run('cd %s/test_dirs/3dClustSim && tcsh runit'% wd,
                        check=True,
                        close_fds=True,
                        shell=True)
    # Actually test the values output by runit

    # Grab diff path for OMP_NO
    # This is expected to be identicle
    dr = diff_parser('%s/test_dirs/3dClustSim/OMP_NO_diff.out')
    assert (not dr)

    # Grab diff path for OMP_YES
    # This is expected to have small differences
    dr = pd.DataFrame(diff_parser('%s/test_dirs/3dClustSim/OMP_YES_diff.out'))
    bl_vals = np.array(
        list(
            dr
            .query('source == "baseline" & ln >= 9')
            .line
            .str
            .split()
            .apply(lambda x: np.array(x).astype(float)).values))
    test_vals = np.array(
        list(
            dr
            .query('source == "test" & ln >= 9')
            .line
            .str
            .split()
            .apply(lambda x: np.array(x).astype(float)).values))
    dif_vals = bl_vals - test_vals

    # Make sure the tested pvals are not different between baseline and test
    assert (dif_vals[:, 0].sum() == 0)

    # Assert that none of the values have difference of more than 2 voxels
    assert (np.abs(dif_vals[:, ]) < 2).all()
