from .utils import tools

# Define Data
data_paths = {"anat": "mini_data/anat_3mm_no_skull.nii.gz"}


def test_3dAutomask_basic(data, run_cmd):

    outfile_prefix = data.outdir / ("masked_image.nii.gz")
    cmd = """
    3dAutomask -prefix {outfile_prefix} {data.anat}
    """

    proc = run_cmd(cmd, locals())
    # Test all outputs match
    tools.assert_all_files_equal(data)
