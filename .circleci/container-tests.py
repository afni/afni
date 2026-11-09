#!/usr/bin/env python3
"""Inside-container runner: prepare pinned data, run one shard, retain coverage."""
import json
from pathlib import Path
import subprocess
import sys


def run(settings):
    """Pin the container dataset, run selected tests, and return their status."""
    import datalad.api as datalad

    tests = Path("/opt/afni/src/tests")
    data = tests / "afni_ci_test_data"
    reports = tests / "test-results"
    reports.mkdir(exist_ok=True)
    if not (data / ".git").is_dir():
        datalad.install(
            path=str(data),
            source="https://github.com/afni/afni_ci_test_data.git",
            on_failure="stop",
        )
    # Preserve the dataset's registered annex remote: GitHub carries Git
    # metadata, while afni_ci_test_data points to the actual data server.
    subprocess.run(
        ["git", "-C", str(data), "config", "--get", "remote.afni_ci_test_data.url"],
        check=True,
    )
    subprocess.run(
        ["git", "-C", str(data), "fetch", "origin", settings["data_revision"]],
        check=True,
    )
    subprocess.run(
        ["git", "-C", str(data), "checkout", "--detach", settings["data_revision"]],
        check=True,
    )
    args = [
        sys.executable,
        "-m",
        "pytest",
        *settings["files"],
        "--runveryslow",
        "--tb=auto",
        "-r",
        "fEsxX",
        "--junitxml=test-results/junit.xml",
        "-o",
        "junit_family=xunit1",
        "-o",
        "faulthandler_timeout=600",
    ]
    # No -n: CircleCI shards provide concurrency; individual AFNI commands can
    # use two OpenMP threads. Explicit multi-thread numerical tests still run.
    if settings["coverage"]:
        args += ["--cov=afnipy", "--cov-report=xml:test-results/python-coverage.xml"]
    status = subprocess.run(args, cwd=tests).returncode
    if settings["coverage"]:
        coverage_status = subprocess.run(
            [
                "gcovr",
                "--root",
                "/opt/afni/src",
                "--object-directory",
                "/opt/afni/build",
                "--xml",
                "--output",
                str(reports / "c-coverage.xml"),
            ],
            cwd="/opt/afni/build",
        ).returncode
        if status == 0:
            status = coverage_status
    return status


if __name__ == "__main__":
    raise SystemExit(run(json.loads(Path(sys.argv[1]).read_text())))
