import pytest
from afni_test_utils import tools

data_paths = {
    "anat": "mini_data/anat_3mm.nii.gz",
    "anatrpi": "mini_data/anat_3mm_RPI.nii.gz",
}


@pytest.mark.veryslow
@pytest.mark.parametrize("dset_name", ["anat", "anatrpi"])
@pytest.mark.xfail
def test_3dSkullStrip_basic(data, dset_name):
    ifile = getattr(data, dset_name)
    ofile = data.outdir / "out_ss.nii.gz"
    cmd = """3dSkullStrip -prefix {ofile} -input {ifile}"""
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
