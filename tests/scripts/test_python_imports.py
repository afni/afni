import shutil
from pathlib import Path
import sys
import subprocess
from .utils.tools import run_cmd

# Until python dependencies are importable in the typical way use the
# following to make them importable instead:
afni_binary: str = shutil.which("afni")  # type: ignore
AFNI_ROOT = str(Path(afni_binary).parent)
sys.path.append(AFNI_ROOT)


def test_script_imports(data):
    binary_dir = Path(shutil.which("afni")).parent

    py_files = list(binary_dir.glob("*.py"))
    possible_pymods = [f for f in py_files if not f.name[0] in "@ 1 2 3".split()]

    # "@DoPerRoi.py",
    known_py2 = [
        "afni_restproc.py",
        "afni_skeleton.py",
        "afni_xmat.py",
        "eg_main_chrono.py",
        "fat_mat_sel.py",
        "fat_mvm_gridconv.py",
        "fat_mvm_prep.py",
        "fat_mvm_review.py",
        "fat_mvm_scripter.py",
        "fat_roi_row.py",
        "gui_uber_skel.py",
        "gui_xmat.py",
        "lib_dti_sundry.py",
        "lib_fat_funcs.py",
        "lib_fat_plot_sel.py",
        "lib_surf_clustsim.py",
        "lib_uber_align.py",
        "lib_uber_skel.py",
        "lpc_align.py",
        "make_pq_script.py",
        "make_stim_times.py",
        "meica.py",
        "neuro_deconvolve.py",
        "parse_fs_lt_log.py",
        "python_module_test.py",
        "quick.alpha.vals.py",
        "read_matlab_files.py",
        "RetroTS.py",  # requires scipy
        "slow_surf_clustsim.py",
        "uber_align_test.py",
        "uber_proc.py",
        "uber_skel.py",
        "ui_xmat.py",
        "unWarpEPI.py",
        "xmat_tool.py",
        "gui_uber_ttest.py",
        "gui_uber_align_test.py",
        "lib_qt_gui.py",
        "gui_uber_subj.py",
        "demoExpt.py",
        "gui_xmat.py",  # wx required
        "lib_matplot.py",  # wx required
        "lib_RR_plot.py",  # wx required
        "lib_wx.py",  # wx required
    ]

    not_importable = [
        "abids_json_tool.py",
        "ClustExp_HistTable.py",
        "quick.alpha.vals.py",
        "abids_json_info.py",
        "tedana_wrapper.py",
        "BayesianGroupAna.py",
        "abids_tool.py",
        "ClustExp_StatParse.py",
    ]

    other_problems = [
        # requires rpy2 with py2. need a rewrite
        "lib_fat_Rfactor.py",
        "fat_lat_csv.py",
    ]

    # for script in possible_pymods:
    #     print(script)
    #     if script.name not in (known_py2 + not_importable + other_problems):
    #         __import__(script.stem)
    # Goal here is to import all python modules (not scripts meant as
    # executables) in both python2 and python3. For now all imports are forced
    # to happen in python2 until such a state is achieved
    broken_imports = {}
    for script in possible_pymods:
        if script.name in other_problems + not_importable:
            continue
        print(script)
        module_name = script.stem
        try:
            run_cmd("python2 -c 'import %s'" % module_name, data, workdir=binary_dir)
        except subprocess.CalledProcessError as e:
            broken_imports[script.name] = e

    if broken_imports:
        failed_scripts = ", ".join([p for p in broken_imports.keys()])
        raise ValueError(
            "The following scripts could not be imported: {failed_scripts}"
        )
