from .utils import tools
import filecmp

# Define Data
data_paths = {"anatomical": "mini_data/anat_3mm.nii.gz"}


def test_3dinfo_basic(data, run_cmd):
    cmd = """
    3dinfo {data.anatomical}
    """

    proc = run_cmd(cmd, current_vars=locals())

    tools.assert_all_files_equal(data)
