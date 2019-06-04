import pytest
from .utils import tools

data_paths = {"anat": "mini_data/anat_3mm.nii.gz"}


@pytest.mark.veryslow
def test_3dSkullStrip_basic(data):
    cmd = """3dSkullStrip -input {data.anat}"""
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
