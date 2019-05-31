# Debugging dask workflows

from pathlib import Path
import pandas as pd
import os
import time
from distutils.spawn import find_executable
import subprocess

# Imports for dask
from dask.distributed import Client, LocalCluster
from dask import delayed

# Imports for using AFNI
import afni_python
import afni_python.afni_base as ab
from afni_python import construct_template_graph
from afni_python.pipeline_utils import TemplateConfig
import sys
import pytest

base_path = Path("ds000002")
subjects = [f"sub-{x:02d}" for x in range(1, 11)]
data_paths = {
    "scans": [(base_path / s / "anat" / (s + "_T1w.nii.gz")) for s in subjects]
}

@pytest.fixture(scope='function')
def afni_dir():
    ## Setup for troubleshooting AFNI pipeline

    afni_dir = Path("~").expanduser() / "abin"
    if not afni_dir.exists():
        afni_path = Path(find_executable("afni"))
        if not afni_path.exists():
            raise EnvironmentError
        else:
            afni_dir = afni_path.parent

    return afni_dir

def test_construct_template_graph(afni_dir,data):
    # This is a hack because the AFNI TemplateConfig class parses
    # input from a system call as part of its initialization
    sys.argv = [
        Path(ab.__file__).with_name("make_template_dask.py"),
        "-ok_to_exist",
        "-outdir",
        data.outdir,
        "-dsets",
        *[str(f) for f in data.scans][:2],
        "-init_base",
        afni_dir / "MNI152_2009_template.nii.gz",
        "-ex_mode",
        "dry_run",
        "-bokeh_port",
        "8791",
    ]


    ## Setup dask cluster
    client = Client(processes=False, n_workers=1, threads_per_worker=2)


    ps = TemplateConfig("make_template_dask.py")
    ps.init_opts()
    if ps.get_user_opts("help"):
        raise ValueError
    ps.process_input()
    task_graph_dict = construct_template_graph.get_task_graph(ps, delayed)
    task_graph_dict['nl_mean_brain'].visualize(str(data.outdir / 'compute_graph.svg'))
    template_futures = client.compute(task_graph_dict['nl_mean_brain'])
    result = client.gather(template_futures)
    print("Really finished making template")


@pytest.mark.veryslow
def test_template_graph_execution(afni_dir,data):
    # This is a hack because the AFNI TemplateConfig class parses
    # input from a system call as part of its initialization
    sys.argv = [
        Path(ab.__file__).with_name("make_template_dask.py"),
        "-ok_to_exist",
        "-outdir",
        data.outdir,
        "-dsets",
        *[str(f) for f in data.scans][:4],
        "-init_base",
        afni_dir / "MNI152_2009_template.nii.gz",
        "-bokeh_port",
        "8791",
    ]

    ## Setup dask cluster
    client = Client(processes=False, n_workers=4, threads_per_worker=4)

    ps = TemplateConfig("make_template_dask.py")
    ps.init_opts()
    if ps.get_user_opts("help"):
        raise ValueError
    ps.process_input()
    task_graph_dict = construct_template_graph.get_task_graph(ps, delayed)
    task_graph_dict['nl_mean_brain'].visualize(str(data.outdir / 'compute_graph.svg'))

    template_futures = client.compute(task_graph_dict['nl_mean_brain'])
    result = client.gather(template_futures)
    print("Really finished making template")
