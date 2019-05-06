from .utils import tools
import filecmp

# Define Data
data_paths = {
    "anatomical": "mini_data/anat_3mm.nii.gz",
    "no_skull": "mini_data/anat_3mm_no_skull_zero_padded.nii.gz",
}


def test_3dcalc_basic(data, run_cmd):
    outfile = data.outdir / "outside_brain.nii.gz"
    cmd = """
    3dcalc -a {data.anatomical} -b {data.no_skull} -expr 'a*not(b)' -prefix {outfile}
    """

    proc = run_cmd(cmd, current_vars=locals())

    tools.assert_all_files_equal(
        data, kwargs_log={"append_to_ignored": ["Output dataset"]}
    )
