import pytest
from afni_test_utils import tools

data_paths = {"bvec": "AFNI_demos/FATCAT_DEMO/bvec"}


def test_1dDW_Grad_o_Mat__plus____plus___basic(data):

    outfile = data.outdir / "GRADS_30.dat"

    cmd = """
    1dDW_Grad_o_Mat++ 
    -in_row_vec {data.bvec}'[2..32]' 
    -flip_y  
    -out_col_vec {outfile}
    """

    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
