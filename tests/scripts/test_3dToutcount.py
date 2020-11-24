# define data
from afni_test_utils import tools

data_paths = {"epi": "AFNI_data6/roi_demo/func_slim+orig.HEAD"}


def test_3dToutcount_basic(data):

    outfile = data.outdir / "outcount_1D"

    cmd = """
    3dToutcount
        -automask
        -fraction
        -polort 3
        -legendre {data.epi}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data, cmd, kwargs_log={"append_to_ignored": ["3dToutcount: AFNI version="]}
    )
    differ.run()
