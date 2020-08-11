import pytest
from afni_test_utils import tools

# Define Data
data_paths = {"epi": "AFNI_data6/afni/epi_r1+orig.HEAD"}

# Parametrize over different potential statistics; we will have a _lot_ of
# tests here, however error messages will be a bit more interpretable and we'll
# be able to better figure out which stat measure is causing issues without
# having to trudge through the stderr output
@pytest.mark.parametrize(
    "statistic",
    [
        "mean",
        "sum",
        "abssum",
        "slope",
        "sos",
        "stdev",
        "l2norm",
        "cvar",
        "cvarinv",
        "tsnr",
        "MAD",
        "DW",
        "median",
        "nzmedian",
        "nzstdev",
        "bmv",
        "MSSD",
        "MSSDsqrt",
        "MASDx",
        "min",
        "max",
        "absmax",
        "signed_absmax",
        "argmin",
        "argmin1",
        "argmax",
        "argmax1",
        "argabsmax",
        "argabsmax1",
        "duration",
        "onset",
        "offset",
        "centroid",
        "nzmean",
        "zcount",
        "nzcount",
        "centromean",
        "percentile 10",
        "autoreg 1",
        pytest.param("accumulate", marks=pytest.mark.slow),
        pytest.param("autocorr 1", marks=pytest.mark.slow),
    ],
)
def test_3dTstat_basic(data, statistic):
    outfile = data.outdir / "stat.nii.gz"
    cmd = """
    3dTstat
        -prefix {outfile}
        -{statistic}
        {data.epi}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # If this is known to cause difficulties, reduce the rtol
    kwargs_scans = {"data_kwargs": {}, "header_kwargs": {}}

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd, kwargs_scans=kwargs_scans)
    differ.run()
