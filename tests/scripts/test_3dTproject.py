import pytest
from afni_test_utils import tools

# Define Data
data_paths = {"epi": "AFNI_data6/afni/epi_r1+orig.HEAD"}


@pytest.mark.slow
@pytest.mark.parametrize("add_env_vars", [({}), ({"OMP_NUM_THREADS": "2"})])
def test_3dTproject_basic(data, add_env_vars):
    kwargs_log = {"append_to_ignored": ["clock time", "OpenMP"]}
    kwargs_scans = {"data_kwargs": {"atol": 0.0001}}

    outfile = data.outdir / "out.nii.gz"
    cmd = """
    3dTproject
        -input {data.epi}
        -prefix {outfile}
        -polort 2
        -passband 0.01 0.1
        -automask
        -dt 2.0
        -blur 2.0
        -norm
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        kwargs_log=kwargs_log,
        kwargs_scans=kwargs_scans,
        add_env_vars=add_env_vars,
    )
    differ.run()
