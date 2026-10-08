#!/usr/bin/env python3
"""Small reproducible throughput benchmark for 3dInSync.

The generated .1D datasets use voxels as rows and time points as columns.
Results are descriptive timings, not pass/fail regression thresholds.
"""

import argparse
import math
import os
import random
import subprocess
import tempfile
import time
from pathlib import Path


def write_fixture(root: Path, nsub: int, ntime: int, nvox: int) -> Path:
    rng = random.Random(314159)
    table = root / "subjects.txt"
    rows = ["Subj Group InputFile"]
    for subject in range(nsub):
        path = root / f"s{subject:03d}.1D"
        with path.open("w", encoding="utf-8") as stream:
            for voxel in range(nvox):
                phase = 0.07 * voxel
                values = []
                for sample in range(ntime):
                    shared = math.sin(0.08 * sample + phase)
                    values.append(shared + 0.35 * rng.gauss(0.0, 1.0))
                stream.write(" ".join(f"{value:.7g}" for value in values) + "\n")
        group = "A" if subject < nsub // 2 else "B"
        rows.append(f"s{subject:03d} {group} {path}")
    table.write_text("\n".join(rows) + "\n", encoding="utf-8")
    return table


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--program", default="./3dInSync")
    parser.add_argument("--subjects", type=int, default=20)
    parser.add_argument("--time", type=int, default=200)
    parser.add_argument("--voxels", type=int, default=500)
    parser.add_argument("--threads", default="1,4")
    args = parser.parse_args()

    if args.subjects < 6 or args.subjects % 2:
        parser.error("--subjects must be even and at least 6")
    threads = [int(value) for value in args.threads.split(",")]

    print("estimator\tcorrelation\tthreads\tseconds\tvoxels_per_second")
    with tempfile.TemporaryDirectory(prefix="insync-benchmark-") as tmp:
        root = Path(tmp)
        table = write_fixture(root, args.subjects, args.time, args.voxels)
        for estimator in ("pairwise", "loo"):
            for correlation in ("pearson", "spearman"):
                for nthr in threads:
                    output = root / f"out-{estimator}-{correlation}-{nthr}"
                    env = os.environ.copy()
                    env["OMP_NUM_THREADS"] = str(nthr)
                    env["AFNI_NOMMAP"] = "YES"
                    command = [
                        args.program,
                        "-quiet",
                        "-progress",
                        "off",
                        "-prefix",
                        str(output),
                        "-isc_method",
                        estimator,
                        "-correlation",
                        correlation,
                        "-dataTableFile",
                        str(table),
                    ]
                    started = time.perf_counter()
                    subprocess.run(command, env=env, check=True,
                                   stdout=subprocess.DEVNULL,
                                   stderr=subprocess.DEVNULL)
                    elapsed = time.perf_counter() - started
                    print(
                        f"{estimator}\t{correlation}\t{nthr}\t{elapsed:.4f}\t"
                        f"{args.voxels / elapsed:.2f}"
                    )


if __name__ == "__main__":
    main()
