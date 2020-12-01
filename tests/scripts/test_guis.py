from afni_test_utils import misc, tools
import logging
from pathlib import Path
import pytest
import os
import subprocess as sp
from unittest.mock import patch
import shutil
import sys
import tempfile


suma_data = Path("mini_data/suma_test_files")
data_paths = {
    "bbr_cmap": suma_data / "bbr.1D.cmap",
    "cubo_curv_1D_dset": suma_data / "cubo.curv.1D.dset",
    "cubo_ply": suma_data / "cubo.ply",
    "cubo_curv_1D_do": suma_data / "cubo.curv.1D.do",
    "cubo_curv_niml_dset": suma_data / "cubo.curv.niml.dset",
    "cubo_spec": suma_data / "cubo.spec",
    "gii_dset": Path("AFNI_data6/FT_analysis/FT/SUMA/std.60.lh.inflated.gii"),
}

# Suma does not raise errors, so check stdout for failure patterns instead
SUMA_FAILURE_PATTERNS = ["ERROR", "Bottom of Debug Stack", "Failed in"]

# GLX extension for Xvfb does not work for Xquartz. Use
# physical display instead
WRAP_SUMA = "display" if sys.platform == "darwin" else "xvfb"


@pytest.mark.skipif(
    not shutil.which("xeyes"),
    reason=("Missing xeyes for basic gui testing."),
)
def test_xeyes(data, monkeypatch):
    """
    Some basic checks/demos for x execution, no point in testing guis if these fail...
    """

    # easy situation, processes behave and clean up after themselves
    tools.run_cmd(data, "xeyes & sleep 1; kill %1 ", x_execution_mode="xvfb")

    # A hanging background will hang unless run synchronously
    tools.run_cmd(
        data,
        "xeyes & sleep 0.1",
        x_execution_mode="xvfb",
        timeout=3,
        use_asynchronous_execution=False,
    )


def test_xeyes_hang_times_out(data, monkeypatch):
    # A hanging background process will raise a timeout error (default 30s)
    with pytest.raises(TimeoutError):
        # silence warning logs for this test
        data.logger = logging
        monkeypatch.setattr(logging, "warn", lambda x: None)
        tools.run_cmd(data, "xeyes & sleep 0.1", x_execution_mode="xvfb", timeout=1)


def test_afni_gui_basic(data, unique_gui_port):
    outfile = Path(tempfile.mkdtemp()) / "test"
    cmd = """
    afni -no_detach -npb {unique_gui_port} -com "OPEN_WINDOW axialimage; SAVE_JPEG axialimage {outfile}; QUIT"
    """
    cmd = cmd.format(**locals())

    stdout_log, stderr_log = tools.run_cmd(
        data, cmd, x_execution_mode="xvfb", timeout=60
    )
    stdout = stdout_log.read_text()
    assert "Fatal Signal 11" not in stdout
    assert "FATAL ERROR" not in stdout


def test_afni_gui_plugin_search(data, monkeypatch, unique_gui_port):
    with monkeypatch.context() as m:
        m.setattr(os, "environ", os.environ.copy())
        # clear pluging path vars
        for k in "AFNI_PLUGINPATH AFNI_PLUGIN_PATH".split():
            if k in os.environ:
                del os.environ[k]
        afni_path = Path(shutil.which("afni"))
        exe_dir = afni_path.parent.resolve()
        rel_libdir = exe_dir / "../lib"
        cmd = """
        afni -no_detach -npb {unique_gui_port} -com "QUIT"
        """
        stdout_log, stderr_log = tools.run_cmd(
            data, cmd, x_execution_mode="xvfb", timeout=60
        )
        stdout = stdout_log.read_text()

    assert f"Path(s) to be searched for plugins: \n{exe_dir} {rel_libdir}" in stdout


def test_afni_gui_plugin_search_with_env_var(data, monkeypatch, unique_gui_port):
    with monkeypatch.context() as m:
        m.setattr(tools.os, "environ", os.environ.copy())

        os.environ["AFNI_PLUGINPATH"] = "/tmp"
        # Run AFNI with AFNI_PLUGINPATH defined
        afni_path = Path(shutil.which("afni"))
        cmd = """
        afni -no_detach -npb {unique_gui_port} -com "QUIT"
        """
        stdout_log, stderr_log = tools.run_cmd(
            data, cmd, x_execution_mode="xvfb", timeout=60
        )
        stdout = stdout_log.read_text()
        assert f"Path(s) to be searched for plugins: \n/tmp" in stdout


def test_suma_gii_read(data, unique_gui_port):
    cmd = """
    suma -npb {unique_gui_port} -i_gii {data.gii_dset} -drive_com '-com kill_suma'
    """
    cmd = cmd.format(**locals())

    stdout_log, stderr_log = tools.run_cmd(data, cmd, x_execution_mode=WRAP_SUMA)
    stdout = stdout_log.read_text()
    assert not any(pat in stdout for pat in SUMA_FAILURE_PATTERNS)


def test_suma_gui_basic(data, unique_gui_port):
    cmd = """
    suma -npb {unique_gui_port} -drive_com '-com kill_suma'
    """
    stdout_log, stderr_log = tools.run_cmd(data, cmd, x_execution_mode=WRAP_SUMA)
    stdout = stdout_log.read_text()
    assert not any(pat in stdout for pat in SUMA_FAILURE_PATTERNS)


@pytest.mark.slow
def test_suma_driving_basic(data, unique_gui_port):

    cmd = """
    export SUMA_DriveSumaMaxWait=5;
    suma -niml -npb {unique_gui_port} & \
    echo suma started;
    sleep 6;
    echo recording image;
    DriveSuma -npb {unique_gui_port} -com viewer_cont -key 'Ctrl+r';
    DriveSuma -npb {unique_gui_port} -com 'kill_suma';
    echo image recorded;
    echo "++ Done";
    """

    differ = tools.OutputDiffer(
        data,
        cmd,
        merge_error_with_output=True,
        skip_output_diff=True,
    )
    stdout_log, stderr_log = differ.run(x_execution_mode=WRAP_SUMA, workdir=data.outdir)
    stdout = stdout_log.read_text()
    assert not any(pat in stdout for pat in SUMA_FAILURE_PATTERNS)


@pytest.mark.slow
def test_suma_driving(data, unique_gui_port):

    ico_prefix = data.outdir / "CreateIco"
    ico_asc = ico_prefix.with_suffix(".asc")
    cmd = """
    export SUMA_DriveSumaMaxWait=5;
    suma -npb {unique_gui_port} -niml  & \
    sleep 3;
    DriveSuma -npb {unique_gui_port} \
      -echo_edu \
      -com 'show_surf -surf_label {data.cubo_curv_niml_dset} \
      -i_ply {data.cubo_ply} -surf_winding cw \
      -surf_state elcubo';
      CreateIcosahedron \
          -rd 4 \
          -prefix {ico_prefix};
    echo driven 1;
    DriveSuma -npb {unique_gui_port} \
        -echo_edu \
        -com 'show_surf -label ICO  -i_fs {ico_asc}';
    echo driven 2;
    sleep 3 ;
    DriveSuma -npb {unique_gui_port} -com 'kill_suma';
    echo Driving suma finished,even if it does say Broken pipe...;
    echo Whether it encountered errors all the way is another story!
    """
    cmd = cmd.format(**locals())

    differ = tools.OutputDiffer(
        data,
        cmd,
        merge_error_with_output=True,
        skip_output_diff=True,
    )
    stdout_log, stderr_log = differ.run(x_execution_mode=WRAP_SUMA, workdir=data.outdir)
    stdout = stdout_log.read_text()
    assert not any(pat in stdout for pat in SUMA_FAILURE_PATTERNS)
