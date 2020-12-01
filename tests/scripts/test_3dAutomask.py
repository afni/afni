from afni_test_utils import tools

# Define Data
data_paths = {"anat": "mini_data/anat_3mm_no_skull.nii.gz"}


def test_3dAutomask_basic(data):

    outfile_prefix = data.outdir / ("masked_image.nii.gz")
    cmd = """
    3dAutomask -prefix {outfile_prefix} {data.anat}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
