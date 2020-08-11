from afni_test_utils.misc import is_omp
from afni_test_utils import tools
import pytest

# check for omp compilation
OMP = is_omp("3dClustSim")

# Define Data
data_paths = {
    "anat1": "mini_data/anat_3mm_no_skull.nii.gz",
    "epi": "AFNI_data6/afni/epi_r1+orig.BRIK",
}


# import subprocess
# from pathlib import Path
# import pandas as pd
# import numpy as np
# from test_utils.diff import diff_parser


@pytest.mark.slow
@pytest.mark.parametrize("add_env_vars", [({}), ({"OMP_NUM_THREADS": "2"})])
def test_3dClustSim_basic(data, add_env_vars):
    seedval = 31416

    kwargs_log = {"append_to_ignored": ["Clock time", "but max simulated alpha="]}

    outfile_prefix = data.outdir / "clust_sim_out"
    cmd = """
    3dClustSim
        -nxyz 16 8 4
        -dxyz 3 3 3
        -BALL
        -acf 0.7 3 3
        -LOTS
        -seed {seedval}
        -prefix {outfile_prefix}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        kwargs_1d={"all_close_kwargs": {"rtol": 0.15}},
        kwargs_log=kwargs_log,
        add_env_vars=add_env_vars,
    )
    differ.run()
