#!/usr/bin/env python3
"""Compare 3dReHo outputs produced by the CMake and Make CI images."""

import argparse
from pathlib import Path

import nibabel as nib
import numpy as np


OUTPUTS = {
    "test_3dReHo": "o.3dReHo+orig.HEAD",
    "test_3dReHo_with_box": "o.3dReHo_ROI+orig.HEAD",
}


def find_output(root: Path, test_name: str, filename: str) -> Path:
    matches = list(root.glob(f"**/ptaylor/{test_name}/{filename}"))
    if len(matches) != 1:
        raise RuntimeError(
            f"Expected one {filename} output for {test_name} beneath {root}; "
            f"found {len(matches)}: {matches}"
        )
    return matches[0]


def compare_outputs(cmake_root: Path, make_root: Path) -> None:
    for test_name, filename in OUTPUTS.items():
        cmake_file = find_output(cmake_root, test_name, filename)
        make_file = find_output(make_root, test_name, filename)

        cmake_data = nib.load(str(cmake_file)).get_fdata()
        make_data = nib.load(str(make_file)).get_fdata()
        if cmake_data.shape != make_data.shape:
            raise AssertionError(
                f"{test_name}: shape differs: {cmake_data.shape} != "
                f"{make_data.shape}"
            )

        mismatch = np.not_equal(cmake_data, make_data)
        if np.any(mismatch):
            difference = np.abs(cmake_data - make_data)
            raise AssertionError(
                f"{test_name}: CMake and Make outputs differ in "
                f"{np.count_nonzero(mismatch)} / {mismatch.size} values; "
                f"maximum absolute difference is {difference.max():.9g}"
            )

        print(f"{test_name}: CMake and Make outputs are exactly equal")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cmake_output", type=Path)
    parser.add_argument("make_output", type=Path)
    args = parser.parse_args()
    compare_outputs(args.cmake_output, args.make_output)


if __name__ == "__main__":
    main()
