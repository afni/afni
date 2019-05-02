# define data
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

    run_cmd(cmd, locals())
