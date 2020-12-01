# define data
from afni_test_utils import tools

data_paths = {"epi": "AFNI_data6/afni/epi_r1+orig.HEAD"}


def test_3dTcat_basic(data):

    outfile = data.outdir / "out.nii.gz"

    cmd = """
    3dTcat -prefix {outfile} {data.epi}'[150..$]'
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
