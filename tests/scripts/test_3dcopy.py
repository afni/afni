from .utils import tools
import filecmp

# Define Data
data_paths = {"anatomical": "mini_data/anat_3mm.nii.gz"}


def test_3dcopy_basic(data, run_cmd):
    outfile = data.outdir / "copied.nii.gz"
    cmd = """
    3dcopy {data.anatomical} {outfile}
    """

    proc = run_cmd(cmd, current_vars=locals())

    tools.assert_all_files_equal(data, kwargs_scans={"data_kwargs": {"atol": 1e-6}})
