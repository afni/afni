from pathlib import Path
import shutil


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

    cmd = """
    3dttest++ -setA {set_a} -setB {set_b} -prefix TTest_zov -covariates {copied_Zovar}
    """
    run_cmd(cmd, locals(), workdir=data.Tovar.parent)

    cmd = """
    3dttest++ -setA {set_a} -setB {set_b} -prefix TTest_tov -covariates {copied_Tovar}
    """
    run_cmd(cmd, locals(), workdir=data.Tovar.parent)
