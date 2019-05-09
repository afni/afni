from .utils.misc import is_omp
from .utils import tools

# check for omp compilation
OMP = is_omp("3dAllineate")


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


def test_3dClustSim_basic(data, run_cmd):
    seedval = 31416
    outfile_prefix = data.outdir / ("clust_sim_out")
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

    proc_1 = run_cmd(cmd, locals())

    # If compiled with OpenMP, additionally run the command with 2 threads
    if OMP:
        outfile_prefix = outfile_prefix.parent / (outfile_prefix.name + "_with_omp")
        proc_2 = run_cmd(cmd, locals(), add_env_vars={"OMP_NUM_THREADS": "2"})

    # Test all outputs match
    tools.assert_all_files_equal(
        data,
        kwargs_1d={"rtol": 0.15},
        kwargs_log={"append_to_ignored": ["Clock time", "but max simulated alpha="]},
    )
