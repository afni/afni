import pytest
from afni_test_utils import tools

# data_paths = {"bvec": "AFNI_demos/FATCAT_DEMO/bvec"}


def test_1d_tool(data):

    outfile = data.outdir / "1d_tool_output.1d"

    cmd = """
    1d_tool.py
    -set_run_lengths 150 150 150
    -index_to_run_tr 324
    > {outfile}
    """

    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd)
    differ.run()
