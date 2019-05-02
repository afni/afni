import pytest

data_paths = {"anat": "mini_data/anat_3mm.nii.gz"}


@pytest.mark.slow
def test_3dSkullStrip_basic(data, run_cmd):
    cmd = """3dSkullStrip -input {data.anat}"""
    run_cmd(cmd, locals())
