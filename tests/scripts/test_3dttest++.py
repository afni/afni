from pathlib import Path
import shutil
from .utils import tools

base_path = Path("old_test_data_repo") / "3dttest++"
data_paths = {
    "Zovar": base_path / "Zovar",
    "Tovar": base_path / "Tovar",
    "cov_files_u": [(str(base_path / "U{:02d}+orig.HEAD")).format(n) for n in range(8)],
    "cov_files_G": [(str(base_path / "G{:02d}+orig.HEAD")).format(n) for n in range(8)],
    "set_a": [(str(base_path / "R{:02d}+orig.HEAD")).format(n) for n in range(4)],
    "set_b": [(str(base_path / "R{:02d}+orig.HEAD")).format(n) for n in range(4, 8)],
}


def test_3dttest__plus____plus___basic(data, run_cmd):
    for file in [data.Zovar, data.Tovar] + data.cov_files_G + data.cov_files_u:
        shutil.copy(file, data.outdir / file.name)
    copied_Tovar = data.outdir / data.Tovar
    copied_Zovar = data.outdir / data.Zovar
    set_a = " ".join([str(f) for f in data.set_a])
    set_b = " ".join([str(f) for f in data.set_b])
    zov_out = data.outdir / "TTest_zov.nii.gz"
    tov_out = data.outdir / "TTest_tov.nii.gz"

    cmd = """
    3dttest++ -setA {set_a} -setB {set_b} -prefix {zov_out} -covariates {copied_Zovar}
    """
    proc_1 = run_cmd(cmd, locals(), workdir=data.Tovar.parent)

    cmd = """
    3dttest++ -setA {set_a} -setB {set_b} -prefix {tov_out} -covariates {copied_Tovar}
    """
    proc_2 = run_cmd(cmd, locals(), workdir=data.Tovar.parent)

    # test outputs if above commands ran
    tools.assert_scans_equal(data.comparison_dir, [zov_out, tov_out])
