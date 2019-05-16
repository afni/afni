from pathlib import Path
import shutil
from .utils import tools
import pytest

base_path = Path("old_test_data_repo") / "3dttest++"
data_paths = {
    "Zovar": base_path / "Zovar",
    "Tovar": base_path / "Tovar",
    "cov_files_u": [(str(base_path / "U{:02d}+orig.HEAD")).format(n) for n in range(8)],
    "cov_files_G": [(str(base_path / "G{:02d}+orig.HEAD")).format(n) for n in range(8)],
    "set_a": [(str(base_path / "R{:02d}+orig.HEAD")).format(n) for n in range(4)],
    "set_b": [(str(base_path / "R{:02d}+orig.HEAD")).format(n) for n in range(4, 8)],
}


@pytest.mark.parametrize("covariance", ["Tovar", "Zovar"])
def test_3dttest__plus____plus___basic(data, covariance):
    covar = getattr(data, covariance)
    for file in [covar] + data.cov_files_G + data.cov_files_u:
        shutil.copy(file, data.outdir / file.name)
    copied_var_file = data.outdir / covar.name
    set_a = " ".join([str(f) for f in data.set_a])
    set_b = " ".join([str(f) for f in data.set_b])
    outfile = data.outdir / ("TTest_" + covar.name + ".nii.gz")

    cmd = """
    3dttest++ -setA {set_a} -setB {set_b} -prefix {outfile} -covariates {copied_var_file}
    """
    cmd = " ".join(cmd.format(**locals()).split())

    # Run command and test all outputs match
    differ = tools.OutputDiffer(data, cmd, workdir=covar.parent, file_list=[outfile])
    differ.run()
