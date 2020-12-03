import attr
from pathlib import Path
import os
import time
from distutils.spawn import find_executable
import subprocess
import pytest
import re
import shutil

pd = pytest.importorskip("pandas", reason="pandas is not installed")

# Imports for dask
from dask.distributed import Client, LocalCluster
from dask import delayed

# Imports for using AFNI
import afnipy
import afnipy.afni_base as ab
from afnipy.pipeline_utils import TemplateConfig
import sys

# Imports for template making
construct_template_graph = pytest.importorskip("afni_template_maker.construct_template_graph", reason="afni_template_maker is not installed")

def get_current_test_name():
    name_str = os.environ.get("PYTEST_CURRENT_TEST").split(":")[-1].split(" ")[0]
    name_str = name_str.replace(".", "")
    name_str = re.sub(r"[-\[\]\(\)\*]", "_", name_str).strip("_")
    if not name_str:
        raise ValueError(
            "After tidying the test name "
            f"{os.environ.get('PYTEST_CURRENT_TEST')}"
            "for later use an empty "
            "string was returned "
        )
    return name_str

@pytest.fixture(scope="function")
def test_data(pytestconfig, request):
    # Get paths used for data
    base_path = Path(__file__).parent.parent.parent.parent.parent.parent
    base_data = base_path /"tests" / "afni_ci_test_data"
    ds17 = base_data / "ds000117_subset" 
    if not base_data.exists():
        print(
            f"afni_ci_test_data ({base_data} should exist. You can install it"
            "there using datalad."
                )
        raise EnviromentError
    subjects = [f"sub-{x:02d}" for x in range(1, 5)]
    data_paths = {
        "t1ws":
            [ds17 / f"{s}_T1.nii.gz" for s in subjects],
        "asegs":
            [ds17 / f"{s}_aseg.nii.gz" for s in subjects],
        "template":
            base_data / "afni_data/atlases/MNI152_2009_template.nii.gz",

    }
    # Create an output directory
    test_name = get_current_test_name()
    outdir = Path(
        pytestconfig.rootdir / "output_of_tests" / test_name
    )
    shutil.rmtree(outdir,ignore_errors=True)
    outdir.mkdir(parents=True)

    # add the test directory and output name
    data_paths.update(
        {
            "outdir": outdir,
            "test_name": test_name,
        }
    )
    # convert dict so that keys are attributes instead
    DataClass = attr.make_class(
        test_name + "_data", [k for k in data_paths.keys()], slots=True
    )
    data = DataClass(*[v for v in data_paths.values()])
    return data

@pytest.fixture(scope="function")
def afni_dir():
    ## Setup for troubleshooting AFNI pipeline
    bin_path = shutil.which("3dinfo")

    afni_dir = Path("~").expanduser() / "abin"
    if not afni_dir.exists():
        binary_dir = Path(find_executable("3dinfo"))
        if not afni_path.exists():
            raise EnvironmentError
        else:
            afni_dir = afni_path.parent

    return afni_dir


def test_construct_template_graph(test_data):
    # This is a hack because the AFNI TemplateConfig class parses
    # input from a system call as part of its initialization
    sys.argv = [
        Path(ab.__file__).with_name("make_template_dask.py"),
        "-ok_to_exist",
        "-outdir",
        test_data.outdir,
        "-dsets",
        *[str(f) for f in test_data.t1ws][:2],
        "-init_base",
        test_data.template,
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
    task_graph_dict["nl_mean_brain"].visualize(str(test_data.outdir / "compute_graph.svg"))
    template_futures = client.compute(task_graph_dict["nl_mean_brain"])
    result = client.gather(template_futures)
    print("Really finished making template")


def test_construct_fpm_graph(test_data):
    # This is a hack because the AFNI TemplateConfig class parses
    # input from a system call as part of its initialization
    sys.argv = [
        Path(ab.__file__).with_name("make_dask_template.py"),
        "-ok_to_exist",
        "-outdir",
        test_data.outdir,
        "-dsets",
        *[str(f) for f in test_data.t1ws][:2],
        "-fs_seg_sets",
        *[str(f) for f in test_data.asegs][:2],
        "-init_base",
        test_data.template,
        "-ex_mode",
        "dry_run",
        "-do_freesurf_mpm",
        "-findtypical_final",
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
    val = task_graph_dict["freesurf_mpm"]["fs_segs_out"]["fs_segs_out"][0]
    val.visualize(str(test_data.outdir / "compute_graph.svg"))
    template_futures = client.compute(val.compute())
    result = client.gather(template_futures)
    print("Really finished making template")


@pytest.mark.veryslow
def test_template_graph_execution(test_data):
    # This is a hack because the AFNI TemplateConfig class parses
    # input from a system call as part of its initialization
    sys.argv = [
        Path(ab.__file__).with_name("make_template_dask.py"),
        "-ok_to_exist",
        "-outdir",
        test_data.outdir,
        "-dsets",
        *[str(f) for f in test_data.t1ws][:4],
        "-init_base",
        test_data.template,
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
    task_graph_dict["nl_mean_brain"].visualize(str(test_data.outdir / "compute_graph.svg"))

    template_futures = client.compute(task_graph_dict["nl_mean_brain"])
    result = client.gather(template_futures)
    print("Really finished making template")
