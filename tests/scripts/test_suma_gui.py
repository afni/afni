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
}


@pytest.mark.veryslow
def test_suma(data):

    cmd = """
    export SUMA_DriveSumaMaxWait=5;
    xvfb-run suma -niml &
    sleep 3;
    DriveSuma -echo_edu   \
      -com show_surf -surf_label {data.cubo_curv_niml_dset} \
      -i_ply {data.cubo_ply} -surf_winding cw \
      -surf_state elcubo &&
    DriveSuma -echo_edu   \
      -com show_surf -label ICO \
      -i_fs CreateIco.asc &&
    sleep 3 ;
    DriveSuma -com kill_suma
           """

    res = misc.run_x_prog(cmd)
