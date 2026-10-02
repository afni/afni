#!/usr/bin/env python3
"""Regression-test old, grid, and classic MapIcosahedron implementations."""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Sequence


MESHES = (("-ld", "60"), ("-ld", "141"), ("-rd", "7"))
HEMISPHERES = ("lh", "rh")


@dataclass
class Timing:
    test: str
    hemi: str
    mesh: str
    wall_old_ms: int
    wall_new_ms: int
    wall_classic_ms: int
    map_old_s: float
    map_new_s: float
    map_classic_s: float


class Reporter:
    def __init__(self, path: Path) -> None:
        self.path = path
        self.path.write_text("")

    def line(self, message: str = "") -> None:
        print(message, flush=True)
        with self.path.open("a") as stream:
            print(message, file=stream)


def default_new_program() -> Path:
    repo = Path(__file__).resolve().parents[2]
    cmake_binary = repo / "build" / "targets_built" / "MapIcosahedron"
    if cmake_binary.is_file():
        return cmake_binary
    return repo / "src" / "SUMA" / "MapIcosahedron"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run MapIcosahedron with an installed old binary, the rebuilt grid "
            "search, and the rebuilt -classic search. New and classic outputs "
            "must be exactly equal; old and new floating-point outputs are "
            "compared with a configurable tolerance."
        )
    )
    parser.add_argument(
        "subject_dir",
        nargs="?",
        type=Path,
        default=Path("fsaverage"),
        help="FreeSurfer subject directory containing SUMA/ (default: ./fsaverage)",
    )
    parser.add_argument(
        "sid",
        nargs="?",
        help="subject ID used by SUMA/SID_lh.spec (default: subject directory name)",
    )
    parser.add_argument(
        "--old-program",
        type=Path,
        default=Path.home() / "abin" / "MapIcosahedron",
        help="pre-update MapIcosahedron executable (default: ~/abin/MapIcosahedron)",
    )
    parser.add_argument(
        "--new-program",
        type=Path,
        default=default_new_program(),
        help="rebuilt MapIcosahedron executable",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="new directory for results (default: ./odir_test-00-MapIcosahedron-SID)",
    )
    parser.add_argument(
        "--tolerance",
        type=float,
        default=1.0e-4,
        help="old-vs-new floating-point tolerance (default: 1e-4)",
    )
    return parser.parse_args()


def require_file(path: Path, description: str) -> Path:
    resolved = path.expanduser().resolve()
    if not resolved.is_file():
        raise RuntimeError(f"cannot find {description}: {resolved}")
    return resolved


def require_tool(name: str) -> str:
    path = shutil.which(name)
    if path is None:
        raise RuntimeError(f"cannot find required command on PATH: {name}")
    return path


def run_capture(
    command: Sequence[str], cwd: Optional[Path] = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(item) for item in command],
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )


def run_map(
    command: Sequence[str],
    cwd: Path,
    log_path: Path,
    output_dir: Path,
    output_stem: str,
) -> tuple[int, float]:
    start = time.perf_counter()
    result = run_capture(command, cwd=cwd)
    wall_ms = round(1000.0 * (time.perf_counter() - start))
    log_path.write_text(result.stdout)

    surfaces = list(output_dir.glob(f"{output_stem}.*.gii"))
    if result.returncode != 0 or not surfaces:
        raise RuntimeError(
            f"MapIcosahedron failed (status {result.returncode}); see {log_path}"
        )

    match = re.search(r"SUMA_MapSurface took\s+([0-9.eE+-]+)\s+seconds", result.stdout)
    if match is None:
        raise RuntimeError(f"could not find SUMA_MapSurface timing in {log_path}")
    return wall_ms, float(match.group(1))


def numeric_rows(path: Path) -> list[list[float]]:
    rows: list[list[float]] = []
    with path.open() as stream:
        for line in stream:
            fields = line.split()
            if not fields:
                continue
            try:
                int(fields[0])
            except ValueError:
                continue
            rows.append([float(value) for value in fields])
    if not rows:
        raise RuntimeError(f"no numeric rows found in {path}")
    return rows


def compare_columns(
    first: Path,
    second: Path,
    columns: range,
    tolerance: float,
) -> tuple[bool, str]:
    rows_a = numeric_rows(first)
    rows_b = numeric_rows(second)
    if len(rows_a) != len(rows_b):
        return False, f"row counts differ: {len(rows_a)} vs {len(rows_b)}"

    differences = 0
    maximum = 0.0
    for row_number, (row_a, row_b) in enumerate(zip(rows_a, rows_b), start=1):
        if max(columns) >= len(row_a) or max(columns) >= len(row_b):
            return False, f"row {row_number} does not contain the requested columns"
        for column in columns:
            difference = abs(row_a[column] - row_b[column])
            maximum = max(maximum, difference)
            if difference > tolerance:
                differences += 1

    if differences:
        return False, f"{differences} values differ; max abs diff {maximum:.9g}"
    return True, f"max abs diff {maximum:.9g}"


def three_d_diff(
    tool: str,
    first: Path,
    second: Path,
    tolerance: Optional[float] = None,
) -> tuple[bool, str]:
    command = [tool, "-q"]
    if tolerance is not None:
        command.extend(("-tol", str(tolerance)))
    command.extend(("-a", str(first), "-b", str(second)))
    result = run_capture(command)
    values = [line.strip() for line in result.stdout.splitlines() if re.fullmatch(r"-?[0-9]+", line.strip())]
    if not values:
        return False, f"3dDiff returned no result: {result.stdout.strip()}"
    status = int(values[-1])
    if status == -1:
        return False, f"3dDiff error: {result.stdout.strip()}"
    return status == 0, "no differences" if status == 0 else "data differ"


def gifti_data_equal(tool: str, first: Path, second: Path) -> tuple[bool, str]:
    result = run_capture(
        (tool, "-compare_gifti", "-compare_data", "-infiles", str(first), str(second))
    )
    equal = "no data differences" in result.stdout
    return equal, "no data differences" if equal else "GIFTI data differ"


def report_result(reporter: Reporter, label: str, result: tuple[bool, str]) -> bool:
    passed, detail = result
    reporter.line(f"{'PASS' if passed else 'FAIL'}: {label} ({detail})")
    return passed


def corresponding(path: Path, old: str, new: str) -> Path:
    return path.with_name(path.name.replace(old, new, 1))


def compare_case(
    reporter: Reporter,
    output_dir: Path,
    test_name: str,
    tolerance: float,
    diff_tool: str,
    gifti_tool: str,
    convert_surface: str,
) -> bool:
    passed = True
    reporter.line(f"---- comparisons for {test_name} ----")

    mi_new = output_dir / f"{test_name}-new-MI_num.1D"
    mi_classic = output_dir / f"{test_name}-cla-MI_num.1D"
    mi_old = output_dir / f"{test_name}-old-MI_num.1D"

    passed &= report_result(
        reporter,
        "exact new vs classic MI node indices",
        compare_columns(mi_new, mi_classic, range(1, 4), 0.0),
    )
    passed &= report_result(
        reporter,
        "exact new vs classic MI weights",
        compare_columns(mi_new, mi_classic, range(4, 7), 0.0),
    )

    for new_surface in sorted(output_dir.glob(f"{test_name}-new.*.gii")):
        classic_surface = corresponding(new_surface, f"{test_name}-new.", f"{test_name}-cla.")
        passed &= report_result(
            reporter,
            f"exact new vs classic surface {new_surface.name}",
            gifti_data_equal(gifti_tool, new_surface, classic_surface),
        )

    for new_dset in sorted(output_dir.glob(f"{test_name}-new.*.niml.dset")):
        classic_dset = corresponding(new_dset, f"{test_name}-new.", f"{test_name}-cla.")
        passed &= report_result(
            reporter,
            f"exact new vs classic dset {new_dset.name}",
            three_d_diff(diff_tool, new_dset, classic_dset),
        )

    passed &= report_result(
        reporter,
        "exact old vs new MI node indices",
        compare_columns(mi_old, mi_new, range(1, 4), 0.0),
    )
    passed &= report_result(
        reporter,
        f"old vs new MI weights, tolerance {tolerance:g}",
        compare_columns(mi_old, mi_new, range(4, 7), tolerance),
    )

    for old_surface in sorted(output_dir.glob(f"{test_name}-old.*.gii")):
        new_surface = corresponding(old_surface, f"{test_name}-old.", f"{test_name}-new.")
        with tempfile.TemporaryDirectory(prefix="mapico-surface-") as temp_name:
            temp_dir = Path(temp_name)
            old_coord = temp_dir / "old.1D.coord"
            old_topo = temp_dir / "old.1D.topo"
            new_coord = temp_dir / "new.1D.coord"
            new_topo = temp_dir / "new.1D.topo"
            for source, coord, topo in (
                (old_surface, old_coord, old_topo),
                (new_surface, new_coord, new_topo),
            ):
                result = run_capture(
                    (convert_surface, "-i_gii", str(source), "-o_1D", str(coord), str(topo))
                )
                if result.returncode != 0:
                    raise RuntimeError(f"ConvertSurface failed for {source}: {result.stdout}")

            passed &= report_result(
                reporter,
                f"old vs new surface coordinates {old_surface.name}, tolerance {tolerance:g}",
                three_d_diff(diff_tool, old_coord, new_coord, tolerance),
            )
            passed &= report_result(
                reporter,
                f"exact old vs new surface triangles {old_surface.name}",
                three_d_diff(diff_tool, old_topo, new_topo),
            )

    for old_dset in sorted(output_dir.glob(f"{test_name}-old.*.niml.dset")):
        new_dset = corresponding(old_dset, f"{test_name}-old.", f"{test_name}-new.")
        passed &= report_result(
            reporter,
            f"old vs new dset {old_dset.name}, tolerance {tolerance:g}",
            three_d_diff(diff_tool, old_dset, new_dset, tolerance),
        )

    return passed


def write_timings(path: Path, timings: list[Timing]) -> None:
    with path.open("w") as stream:
        print(
            f"{'test':8} {'hemi':4} {'mesh':8}   "
            f"{'wall_old':>9} {'wall_new':>9} {'wall_cla':>9}   "
            f"{'map_old':>9} {'map_new':>9} {'map_cla':>9}",
            file=stream,
        )
        for item in timings:
            print(
                f"{item.test:8} {item.hemi:4} {item.mesh:8}   "
                f"{item.wall_old_ms:9d} {item.wall_new_ms:9d} {item.wall_classic_ms:9d}   "
                f"{item.map_old_s:9.3f} {item.map_new_s:9.3f} {item.map_classic_s:9.3f}",
                file=stream,
            )


def main() -> int:
    args = parse_args()
    if args.tolerance < 0.0:
        raise RuntimeError("--tolerance must be non-negative")

    subject_dir = args.subject_dir.expanduser().resolve()
    sid = args.sid or subject_dir.name
    spec_dir = subject_dir / "SUMA"
    for hemi in HEMISPHERES:
        require_file(spec_dir / f"{sid}_{hemi}.spec", f"{hemi} spec file")

    old_program = require_file(args.old_program, "old MapIcosahedron")
    new_program = require_file(args.new_program, "new MapIcosahedron")
    diff_tool = require_tool("3dDiff")
    gifti_tool = require_tool("gifti_tool")
    convert_surface = require_tool("ConvertSurface")

    help_result = run_capture((str(new_program), "-help"))
    if "-classic" not in help_result.stdout:
        raise RuntimeError(f"new program does not support -classic: {new_program}")

    output_dir = args.output_dir or Path(f"odir_test-00-MapIcosahedron-{sid}")
    output_dir = output_dir.expanduser().resolve()
    if output_dir.exists():
        raise RuntimeError(f"output directory already exists: {output_dir}")
    output_dir.mkdir(parents=True)

    reporter = Reporter(output_dir / "all_diffs.txt")
    timing_path = output_dir / "all_times.txt"
    reporter.line(f"old program: {old_program}")
    reporter.line(f"new program: {new_program}")
    reporter.line(f"old-vs-new floating-point tolerance: {args.tolerance:g}")

    relative_output = os.path.relpath(output_dir, spec_dir)
    timings: list[Timing] = []
    all_passed = True
    test_number = 0

    for hemi in HEMISPHERES:
        for mesh_option, mesh_value in MESHES:
            test_number += 1
            test_name = f"test-{test_number:02d}"
            mesh_name = f"{mesh_option}{mesh_value}"
            reporter.line(f"==== {test_name}: {hemi}, {mesh_name} ====")

            dset_args: list[str] = []
            thickness = spec_dir / f"{hemi}.thickness.gii.dset"
            if thickness.is_file():
                dset_args = ["-dset_map", thickness.name]

            run_results: dict[str, tuple[int, float]] = {}
            for version in ("old", "new", "cla"):
                program = old_program if version == "old" else new_program
                command = [str(program)]
                if version == "cla":
                    command.append("-classic")
                output_stem = f"{test_name}-{version}"
                prefix = f"{relative_output}/{output_stem}."
                command.extend(
                    (
                        "-verb",
                        "-spec",
                        f"{sid}_{hemi}.spec",
                        mesh_option,
                        mesh_value,
                        *dset_args,
                        "-prefix",
                        prefix,
                    )
                )
                run_results[version] = run_map(
                    command,
                    cwd=spec_dir,
                    log_path=output_dir / f"{output_stem}-log.txt",
                    output_dir=output_dir,
                    output_stem=output_stem,
                )

                mi_source = output_dir / f"{output_stem}.{sid}_{hemi}.MI.1D"
                mi_numeric = output_dir / f"{output_stem}-MI_num.1D"
                rows = numeric_rows(mi_source)
                mi_numeric.write_text(
                    "".join(" ".join(f"{value:g}" for value in row) + "\n" for row in rows)
                )

            all_passed &= compare_case(
                reporter,
                output_dir,
                test_name,
                args.tolerance,
                diff_tool,
                gifti_tool,
                convert_surface,
            )

            timings.append(
                Timing(
                    test=test_name,
                    hemi=hemi,
                    mesh=mesh_name,
                    wall_old_ms=run_results["old"][0],
                    wall_new_ms=run_results["new"][0],
                    wall_classic_ms=run_results["cla"][0],
                    map_old_s=run_results["old"][1],
                    map_new_s=run_results["new"][1],
                    map_classic_s=run_results["cla"][1],
                )
            )
            write_timings(timing_path, timings)

    reporter.line()
    reporter.line("PASS: all comparisons succeeded" if all_passed else "FAIL: comparisons failed")
    print(f"comparison report: {reporter.path}")
    print(f"timing report: {timing_path}")
    return 0 if all_passed else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        raise SystemExit(2)
