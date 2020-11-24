from afni_test_utils import tools
import filecmp

# Define Data
data_paths = {"anatomical": "mini_data/anat_3mm.nii.gz"}


def test_3dinfo_basic(data):
    cmd = """
    3dinfo {data.anatomical}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
