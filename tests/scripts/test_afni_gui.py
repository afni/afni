from afni_test_utils import misc
from pathlib import Path
import shutil
import pytest
import os
import subprocess as sp


def test_afni_gui_basic():

    cmd = 'afni -no_detach -com "OPEN_WINDOW axialimage; SAVE_JPEG axialimage test1; QUIT"'

    res = misc.run_x_prog(cmd)
    assert "Fatal Signal 11" not in res
    assert "FATAL ERROR" not in res


@pytest.mark.skipif(
    "AFNI_PLUGIN_PATH" in os.environ or "AFNI_PLUGINPATH" in os.environ,
    reason="plugin search behavior is overwritten",
)
def test_afni_gui_plugin_search():
    afni_path = Path(shutil.which("afni"))
    exe_dir = afni_path.parent.resolve()
    rel_libdir = exe_dir / "../lib"
    cmd = 'afni -no_detach -com "QUIT"'
    print(cmd)
    res = sp.check_output(cmd)
    assert f"Path(s) to be searched for plugins: \n{exe_dir} {rel_libdir}" in res


def test_afni_gui_plugin_search_with_env_var():
    afni_path = Path(shutil.which("afni"))
    cmd = 'afni -no_detach -com "QUIT"'
    # Run AFNI with AFNI_PLUGINPATH defined
    os.environ["AFNI_PLUGINPATH"] = "/tmp"
    res = misc.run_x_prog(cmd)
    assert f"Path(s) to be searched for plugins: \n/tmp" in res
