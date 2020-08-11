import pytest
import sys
from afni_test_utils import tools

# Define Data
data_paths = {"mask": "mini_data/mask_3mm.nii.gz", "epi": "mini_data/aligned.nii.gz"}


@pytest.mark.skipif(
    sys.platform == "darwin",
    reason=("3dmaskdump removes all voxels on MacOS for some reason."),
)
def test_3dmaskdump_basic(data):

    outfile_path = data.outdir / ("Vrel_tstats.txt")
    cmd = """
    3dmaskdump
        -noijk
        -mask {data.mask} {data.epi}
        > {outfile_path}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
