#!/usr/bin/env python3
"""Run an explicit shard in remote Docker and recover reports on test failure.

No bind mounts, registry credentials, or nested pytest worker pools are needed.
An empty shard succeeds without accidentally collecting the whole suite.
"""
import argparse
import json
from pathlib import Path
import subprocess
import os
import stat
import tempfile
import tarfile
import xml.etree.ElementTree as ET


def selected_files(path):
    """Read relative pytest paths; reject traversal and missing files."""
    files = Path(path).read_text().splitlines()
    for name in files:
        relative = Path(name)
        if (
            relative.is_absolute()
            or ".." in relative.parts
            or not name.startswith("scripts/")
        ):
            raise ValueError(f"Invalid test path: {name!r}")
        if not (Path("tests") / relative).is_file():
            raise ValueError(f"Test file not found: {name!r}")
    return list(dict.fromkeys(files))


def writable_directories(path):
    """Allow removal of a private temporary annex cache without changing file data."""
    for directory, _, _ in os.walk(path):
        os.chmod(directory, os.stat(directory).st_mode | stat.S_IWUSR | stat.S_IXUSR)


def cache_filter(member, destination):
    """Safely extract cache directories as writable, retaining read-only files."""
    result = tarfile.data_filter(member, destination)
    if result is not None and member.isfile() and not member.mode & stat.S_IWUSR:
        result.mode &= ~stat.S_IWUSR
    return result


def recover_test_data(container):
    """Replace the cache only after a complete copy; annex objects are read-only."""
    data = Path("tests/afni_ci_test_data")
    with tempfile.TemporaryDirectory(
        prefix=".afni-data-", dir=data.parent
    ) as directory:
        staging = Path(directory) / "current"
        backup = Path(directory) / "previous"
        staging.mkdir()
        try:
            # Docker's local archive extractor applies directory modes before
            # their contents, which fails on annex's 0555 object directories.
            # Python's data filter defers directory permissions and rejects
            # paths/symlinks escaping the staging directory.
            with subprocess.Popen(
                [
                    "docker",
                    "cp",
                    f"{container}:/opt/afni/src/tests/afni_ci_test_data/.",
                    "-",
                ],
                stdout=subprocess.PIPE,
            ) as process:
                try:
                    with tarfile.open(fileobj=process.stdout, mode="r|") as archive:
                        archive.extractall(staging, filter=cache_filter)
                finally:
                    process.stdout.close()
                if process.wait():
                    print(
                        "Could not refresh test-data cache; retaining the previous cache."
                    )
                    return
            if data.exists():
                data.rename(backup)
            try:
                staging.rename(data)
            except OSError:
                if backup.exists():
                    backup.rename(data)
                raise
        finally:
            # git-annex protects object directories with mode 0555. Copying over
            # an existing cache or removing it without this step fails for CI users.
            writable_directories(staging)
            writable_directories(backup)


def run(build_type, files, coverage=False):
    """Return pytest's failure status, or a transfer error if tests succeeded."""
    reports = Path("tests/test-results")
    reports.mkdir(parents=True, exist_ok=True)
    if not files:
        ET.ElementTree(ET.Element("testsuite", name="empty-shard", tests="0")).write(
            reports / "junit.xml"
        )
        print("No files assigned to this shard.")
        return 0
    revision = subprocess.check_output(
        ["git", "ls-tree", "HEAD", "tests/afni_ci_test_data"], text=True
    ).split()[2]
    config = {
        "files": files,
        "data_revision": revision,
        "coverage": coverage,
        "build_type": build_type,
    }
    container = None
    status = 1
    try:
        container = subprocess.check_output(
            [
                "docker",
                "create",
                "--user",
                "root",
                "-e",
                "CHOWN_EXTRA=/opt/afni/src/tests",
                "-e",
                "CHOWN_EXTRA_OPTS=-R",
                "-e",
                "OMP_NUM_THREADS=2",
                "-e",
                "OPENBLAS_NUM_THREADS=1",
                "-e",
                "MKL_NUM_THREADS=1",
                f"afni/afni_{build_type}",
                "python",
                "/opt/afni/src/.circleci/container-tests.py",
                "/tmp/afni-test-config.json",
            ],
            text=True,
        ).strip()
        with tempfile.TemporaryDirectory() as directory:
            settings = Path(directory) / "settings.json"
            settings.write_text(json.dumps(config))
            subprocess.run(
                [
                    "docker",
                    "cp",
                    str(settings),
                    f"{container}:/tmp/afni-test-config.json",
                ],
                check=True,
            )
        data = Path("tests/afni_ci_test_data")
        if (data / ".git").is_dir():
            subprocess.run(
                ["docker", "cp", str(data), f"{container}:/opt/afni/src/tests/"],
                check=True,
            )
        subprocess.run(["docker", "start", "--attach", container])
        status = int(
            subprocess.check_output(
                ["docker", "inspect", "--format", "{{.State.ExitCode}}", container],
                text=True,
            )
        )
    finally:
        if container:
            # docker cp without -a makes recovered files owned by the CI user.
            # Never replace a test failure with a secondary copy/cleanup error.
            for name in ("test-results", "output_of_tests"):
                Path(f"tests/{name}").mkdir(parents=True, exist_ok=True)
                result = subprocess.run(
                    [
                        "docker",
                        "cp",
                        f"{container}:/opt/afni/src/tests/{name}/.",
                        f"tests/{name}",
                    ]
                )
                if result.returncode and name == "test-results" and status == 0:
                    status = result.returncode
            try:
                recover_test_data(container)
            except (OSError, tarfile.TarError) as error:
                print(f"Could not refresh test-data cache: {error}")
            finally:
                subprocess.run(["docker", "rm", "-f", container])
    return status


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build_type", choices=["cmake_build", "make_build"])
    parser.add_argument("file_list", nargs="?")
    parser.add_argument("--coverage", action="store_true")
    args = parser.parse_args()
    if args.coverage:
        files = sorted(
            str(p.relative_to("tests"))
            for p in Path("tests/scripts").rglob("*.py")
            if (p.name.startswith("test_") or p.name.endswith("_test.py"))
            and p.name != "test_python_style.py"
        )
    elif args.file_list:
        files = selected_files(args.file_list)
    else:
        parser.error("file_list is required unless collecting coverage")
    raise SystemExit(run(args.build_type, files, args.coverage))
