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

@pytest.mark.parametrize(
    "test_case,params",
    [
        (
            "basic",
            {
                "extra_args": "",
            },
        ),
        (
            "e2a",
            {
                "extra_args": "-epi2anat",
            },
        ),
        (
            "giant_move",
            {
                "extra_args": "-giant_move",
            },
        ),

    ],
)
@pytest.mark.veryslow
def test_align_epi_anat(data, test_case, params):
    outname = "aligned"

    cmd = f"""
    align_epi_anat.py
        -anat {data.anat1}
        -epi {data.epi}
        -epi_base 0
        -suffix _aligned
        {params["extra_args"]}
    """
    cmd = " ".join(cmd.split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        workdir=data.outdir,
        ignore_file_patterns=None,
        kwargs_log={
            "append_to_ignored": [
                "3dAttribute DELTA",
            ]
        },
    )
    differ.run(timeout=60 * 15)


@pytest.mark.veryslow
def test_align_epi_anat_with_outdir(data):
    """Test for output dir option - not typically used
    because typically used in current working directory
    """

    subdir = "test_aea_subdir"
    cmd = f"""
    align_epi_anat.py
        -anat {data.anat1}
        -epi {data.epi}
        -epi_base 0
        -suffix _aligned
        -output_dir {subdir}
    """
    cmd = " ".join(cmd.split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        workdir=data.outdir,
        ignore_file_patterns=None,
        kwargs_log={
            "append_to_ignored": [
                "3dAttribute DELTA",
            ]
        },
    )
    differ.run(timeout=60 * 15)


@pytest.mark.parametrize(
    "test_case,params",
    [
        (
            "basic",
            {
                "extra_args": "",
            },
        ),
        (
            "e2a",
            {
                "extra_args": "-epi2anat",
            },
        ),
        (
            "giant_move",
            {
                "extra_args": "-giant_move",
            },
        ),
    ],
)
def test_align_epi_anat_dryrun(data, test_case, params):
    outname = "aligned"

    cmd = f"""
    align_epi_anat.py
        -anat {data.anat1}
        -epi {data.epi}
        -epi_base 0
        -suffix _aligned
        -ex_mode dry_run
        {params["extra_args"]}
    """
    cmd = " ".join(cmd.split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        workdir=data.outdir,
        ignore_file_patterns=None,
        kwargs_log={
            "append_to_ignored": [
                "3dAttribute DELTA",
            ]
        },
    )
    differ.run()


# @pytest.mark.veryslow
# def test_align_epi_anat_with_exec(data):
# outname = "aligned"
# if OMP:
# outname += "_with_omp"
# outfile = data.outdir / (outname + ".nii.gz")
# out_1d = outfile.parent / (outfile.stem.split(".")[0] + ".1D")

# cmd = """
# align_epi_anat.py
# -anat {data.anat1}
# -epi {data.epi}
# -epi_base 0
# -suffix _aligned
# -anat_has_skull no
# -epi_strip 3dAutomask
# """
# cmd = " ".join(cmd.format(**locals()).split())

# # Run command and test all outputs match
# differ = tools.OutputDiffer(
# data,
# cmd,
# ignore_file_patterns=None,
# )
# differ.run()


# @pytest.mark.veryslow
# def test_align_epi_anat_with_outdir(data):
# outname = "aligned"
# if OMP:
# outname += "_with_omp"
# outfile = data.outdir / (outname + ".nii.gz")
# out_1d = outfile.parent / (outfile.stem.split(".")[0] + ".1D")

# cmd = """
# align_epi_anat.py
# -anat {data.anat1}
# -epi {data.epi}
# -epi_base 0
# -suffix _aligned
# -anat_has_skull no
# -epi_strip 3dAutomask
# -outdir
# """
# cmd = " ".join(cmd.format(**locals()).split())

# # Run command and test all outputs match
# differ = tools.OutputDiffer(
# data,
# cmd,
# ignore_file_patterns=None,
# )
# differ.run()
