from .utils import misc
from pathlib import Path
import pytest

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

suma_failure_patterns = ["ERROR", "Bottom of Debug Stack", "Failed in"]


def test_suma_gii_read(data):
    cmd = f"suma -i_gii {data.gii_dset} -drive_com '-com kill_suma'"
    res = misc.run_x_prog(cmd)
    assert not any(pat in res for pat in suma_failure_patterns)


def test_suma_gui_basic():
    cmd = "suma -drive_com '-com kill_suma'"
    res = misc.run_x_prog(cmd)
    assert not any(pat in res for pat in suma_failure_patterns)


@pytest.mark.veryslow
def test_suma(data):

    cmd = """
    set -e;
    export SUMA_DriveSumaMaxWait=5;
    suma -niml &
    sleep 3;
    DriveSuma -echo_edu   \
      -com show_surf -surf_label {data.cubo_curv_niml_dset} \
      -i_ply {data.cubo_ply} -surf_winding cw \
      -surf_state elcubo &&
      CreateIcosahedron -rd 4 &&
    DriveSuma -echo_edu   \
      -com show_surf -label ICO \
      -i_fs CreateIco.asc &&
    sleep 3 ;
    DriveSuma -com kill_suma
           """
    cmd = cmd.format(**locals())

    import subprocess as sp

    res = sp.run(cmd, cwd=data.outdir, shell=True, stdout=sp.PIPE, stderr=sp.STDOUT)
    res.check_returncode()
    if any(pat in res.stdout.decode() for pat in suma_failure_patterns):

        raise ValueError(
            f"""
        {cmd}
        Command executed, but output contains an error {res.stdout}
        """
        )
