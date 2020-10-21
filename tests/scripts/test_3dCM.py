from afni_test_utils import tools

# Define Data
data_paths = {"anat": "mini_data/anat_3mm_no_skull.nii.gz"}


def test_3dCM_basic(data):
    cmd = """
    3dCM
        {data.anat}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
