# define data
data_paths = {"epi": "AFNI_data6/afni/epi_r1+orig.HEAD"}


def test_3dTcat_basic(data, run_cmd):

    outfile = data.outdir / "out.nii.gz"

    cmd = """
    3dTcat -prefix {outfile} {data.epi}'[150..$]'
    """

    run_cmd(cmd, locals())
