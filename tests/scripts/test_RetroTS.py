from .utils import tools
import pytest

# Define Data
data_paths = {
    "ECG_epiRTslt_scan_4": "retro_ts/ECG_epiRTslt_scan_4.1D",
    "Resp_epiRTslt_scan_4": "retro_ts/Resp_epiRTslt_scan_4.1D",
}


@pytest.mark.parametrize("vol_tr", [(2), (2.5)])
def test_RetroTS_basic(data, vol_tr, python_interpreter):
    seedval = 31416

    kwargs_log = {"append_to_ignored": ["Clock time", "but max simulated alpha="]}

    out_prefix = data.outdir / f"reg.01.a.{vol_tr}"
    cmd = """
    RetroTS.py
        -c {data.ECG_epiRTslt_scan_4}
        -r {data.Resp_epiRTslt_scan_4}
        -v {vol_tr}
        -p 50
        -n 30
        -prefix {out_prefix}
    """

    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(
        data,
        cmd,
        python_interpreter=python_interpreter,
        kwargs_1d={"all_close_kwargs": {"rtol": 0.15}},
        # kwargs_log=kwargs_log,
        # add_env_vars=add_env_vars,
    )
    differ.run()
