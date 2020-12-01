import pytest
from afni_test_utils.misc import is_omp
from afni_test_utils import tools

# check for omp compilation
OMP = is_omp("3dAllineate")

# Define Data
data_paths = {
    "anat1": "mini_data/anat_3mm_no_skull.nii.gz",
    "epi": "AFNI_data6/afni/epi_r1+orig.BRIK",
    "epi_head": "AFNI_data6/afni/epi_r1+orig.HEAD",
}


# TESTS:
@pytest.mark.slow
@pytest.mark.skip(
    "reason=The comparison of to affines should use the expected "
    "distance of movement for a point drawn from a 3D cloud. "
)
def test_3dAllineate_basic(data, python_interpreter):
    outname = "aligned"
    if OMP:
        outname += "_with_omp"
    outfile = data.outdir / (outname + ".nii.gz")
    out_1d = outfile.parent / (outfile.stem.split(".")[0] + ".1D")

    cmd = """
    3dAllineate
        -base {data.anat1}
        -source {data.epi}'[0]'
        -prefix {outfile}
        -1Dparam_save {out_1d}
        -maxrot 2
        -maxshf 1
        -nmatch 20
        -conv 2
        -cost lpc
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        python_interpreter=python_interpreter,
        kwargs_log={
            "append_to_ignored": [
                "Output dataset",
                "++ Wrote -1Dparam_save",
                "total CPU time",
            ]
        },
    )
    differ.run()
