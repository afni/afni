#!/usr/bin/env python3
"""Check installed paths and a tiny NIfTI arithmetic round trip, without test data."""
import importlib
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

import nibabel as nib
import numpy as np


def main(build_type):
    """Raise on incorrect install paths, unresolved symbols, or changed voxel values."""
    expected = Path("/opt/afni/install")
    if build_type == "cmake_build":
        expected /= "usr/local/bin"
    for name in ("3dcalc", "3dinfo", "afni", "suma", "align_epi_anat.py"):
        found = shutil.which(name)
        if not found or Path(found).parent != expected:
            raise RuntimeError(f"{name}: expected {expected}, found {found}")
    # Resolve GUI and non-GUI shared-library references without opening X11.
    for name in ("3dcalc", "3dinfo", "afni", "suma"):
        result = subprocess.run(
            ["ldd", "-r", str(expected / name)], capture_output=True, text=True
        )
        diagnostics = result.stdout + result.stderr
        if (
            result.returncode
            or "not found" in diagnostics
            or "undefined symbol:" in diagnostics
        ):
            raise RuntimeError(
                f"Unresolved libraries/symbols for {name}:\n{diagnostics}"
            )
    sys.path.insert(0, str(expected))
    module = importlib.import_module("afnipy")
    if not Path(module.__file__).resolve().is_relative_to(expected.resolve()):
        raise RuntimeError(f"Testing the wrong Python module: {module.__file__}")
    print(f"Testing installed binaries in {expected}; afnipy: {module.__file__}")
    with tempfile.TemporaryDirectory() as directory:
        source = Path(directory) / "input.nii"
        result = Path(directory) / "output.nii"
        values = np.arange(24, dtype=np.float32).reshape(2, 3, 4)
        affine = np.diag([2.0, 3.0, 4.0, 1.0])
        nib.save(nib.Nifti1Image(values, affine), source)
        subprocess.run(
            [
                str(expected / "3dcalc"),
                "-a",
                str(source),
                "-expr",
                "a*2+1",
                "-datum",
                "float",
                "-prefix",
                str(result),
            ],
            check=True,
        )
        output = nib.load(result)
        np.testing.assert_array_equal(output.get_fdata(), values * 2 + 1)
        np.testing.assert_allclose(output.affine, affine, rtol=0, atol=1e-6)
        subprocess.run([str(expected / "3dinfo"), "-n4", str(result)], check=True)


if __name__ == "__main__":
    main(sys.argv[1])
