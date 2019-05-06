# define data
from .utils import tools

data_paths = {"epi": "AFNI_data6/roi_demo/func_slim+orig.HEAD"}


def test_3dToutcount_basic(data, run_cmd):

    outfile = data.outdir / "outcount_1D"

    cmd = """
    3dToutcount
        -automask
        -fraction
        -polort 3
        -legendre {data.epi}
    """

    proc = run_cmd(cmd, locals())
    tools.assert_all_files_equal(
        data, kwargs_log={"append_to_ignored": ["3dToutcount: AFNI version="]}
    )
