"""Small checks for CI selection, compiler flags, and release guards."""

import importlib.util
import io
import stat
import json
from pathlib import Path
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch
from types import SimpleNamespace

CI = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(CI))


def module(name):
    """Import a CI script without executing its command-line entry point."""
    spec = importlib.util.spec_from_file_location(
        name.replace("-", "_"), CI / (name + ".py")
    )
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


checks = module("check-build")
runner = module("run-tests")


def copy_failure(command, **kwargs):
    """Simulate a failed report recovery after successful container setup."""
    recovering = any(str(arg).startswith("test-container:/") for arg in command)
    return SimpleNamespace(returncode=1 if recovering else 0)


class Checks(unittest.TestCase):
    def test_new_target_drift_fails_but_resolved_drift_is_allowed(self):
        baseline = {"make_only": ["legacy"], "cmake_only": [], "unpackaged_cmake": []}
        current = {"make_only": [], "cmake_only": [], "unpackaged_cmake": []}
        self.assertEqual(checks.target_drift(current, baseline), [])
        current["make_only"] = ["legacy", "removed_from_cmake"]
        current["unpackaged_cmake"] = ["new_program"]
        failures = checks.target_drift(current, baseline)
        self.assertEqual(len(failures), 2)
        self.assertTrue(any("removed_from_cmake" in item for item in failures))
        self.assertTrue(any("new_program" in item for item in failures))

    def test_compiler_flags_preserve_definitions_and_includes(self):
        result = checks.syntax_command(
            {
                "command": 'ccache gcc -DFOO=1 -I"some directory" -MD -MT foo.o -MF foo.d -o foo.o -c foo.c'
            }
        )
        self.assertEqual(result[:4], ["ccache", "gcc", "-DFOO=1", "-Isome directory"])
        self.assertIn("foo.c", result)
        self.assertIn("-fsyntax-only", result)
        self.assertNotIn("-c", result)
        self.assertNotIn("foo.o", result)
        self.assertNotIn("foo.d", result)

    def test_empty_shard_never_starts_docker(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(
            runner, "Path", wraps=Path
        ) as paths:
            paths.return_value = Path(directory)
            with patch.object(runner.subprocess, "check_output") as process:
                self.assertEqual(runner.run("cmake_build", []), 0)
                process.assert_not_called()
            self.assertTrue((Path(directory) / "junit.xml").exists())

    def test_failed_tests_keep_status_and_copy_all_reports(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(
            runner, "Path", wraps=Path
        ) as paths:
            paths.side_effect = lambda value: Path(directory) / value
            cache_patch = patch.object(runner, "recover_test_data")
            cache_patch.start()
            self.addCleanup(cache_patch.stop)
            # gitlink revision, container ID, actual pytest exit status
            with patch.object(
                runner.subprocess,
                "check_output",
                side_effect=[
                    "160000 commit abc123 tests/afni_ci_test_data",
                    "test-container",
                    "5",
                ],
            ), patch.object(
                runner.subprocess, "run", side_effect=copy_failure
            ) as process:
                self.assertEqual(
                    runner.run("cmake_build", ["scripts/test_example.py"]), 5
                )
            commands = [call.args[0] for call in process.call_args_list]
            self.assertTrue(
                any(
                    "test-container:/opt/afni/src/tests/test-results/." in cmd
                    for cmd in commands
                )
            )
            self.assertTrue(
                any(
                    "test-container:/opt/afni/src/tests/output_of_tests/." in cmd
                    for cmd in commands
                )
            )
            self.assertEqual(commands[-1], ["docker", "rm", "-f", "test-container"])

    def test_successful_tests_require_report_transfer(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(
            runner, "Path", wraps=Path
        ) as paths:
            paths.side_effect = lambda value: Path(directory) / value
            cache_patch = patch.object(runner, "recover_test_data")
            cache_patch.start()
            self.addCleanup(cache_patch.stop)
            with patch.object(
                runner.subprocess,
                "check_output",
                side_effect=[
                    "160000 commit abc123 tests/afni_ci_test_data",
                    "test-container",
                    "0",
                ],
            ), patch.object(runner.subprocess, "run", side_effect=copy_failure):
                self.assertEqual(
                    runner.run("cmake_build", ["scripts/test_example.py"]), 1
                )

    def test_annex_cache_extracts_read_only_directories(self):
        with tempfile.TemporaryDirectory() as directory:
            buffer = io.BytesIO()
            with tarfile.open(fileobj=buffer, mode="w") as archive:
                folder = tarfile.TarInfo("objects")
                folder.type = tarfile.DIRTYPE
                folder.mode = 0o555
                archive.addfile(folder)
                member = tarfile.TarInfo("objects/data")
                member.mode = 0o444
                member.size = 4
                archive.addfile(member, io.BytesIO(b"data"))
            buffer.seek(0)
            with tarfile.open(fileobj=buffer) as archive:
                archive.extractall(directory, filter=runner.cache_filter)
            path = Path(directory) / "objects/data"
            self.assertEqual(path.read_bytes(), b"data")
            self.assertFalse(path.stat().st_mode & stat.S_IWUSR)
            self.assertTrue(path.parent.stat().st_mode & stat.S_IWUSR)

    def test_selection_rejects_traversal(self):
        with tempfile.NamedTemporaryFile(mode="w") as stream:
            stream.write("scripts/../../outside.py\n")
            stream.flush()
            with self.assertRaises(ValueError):
                runner.selected_files(stream.name)

    def test_archive_existing_drift_is_reported_new_omission_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            archive = root / "sources.tgz"
            with tarfile.open(archive, "w:gz"):
                pass
            changes = {"tracked": ["src/new.c", "src/legacy.c"], "added": []}
            checks.archive_check(root, archive, changes, root, {})
            self.assertEqual(
                json.loads((root / "archive-missing.json").read_text()),
                ["src/legacy.c", "src/new.c"],
            )
            changes["added"] = ["src/new.c"]
            with self.assertRaisesRegex(RuntimeError, "src/new.c"):
                checks.archive_check(root, archive, changes, root, {})
            checks.archive_check(
                root, archive, changes, root, {"src/new.c": "Standalone example"}
            )

    def test_workflow_gates(self):
        import yaml

        config = yaml.safe_load((CI / "config.yml").read_text())
        jobs = config["workflows"]["afni_tests"]["jobs"]
        invocations = {}
        for entry in jobs:
            job, options = next(iter(entry.items()))
            invocations[options.get("name", job)] = options
        self.assertEqual(invocations["make_build"]["requires"], ["lint"])
        self.assertEqual(invocations["cmake_build"]["requires"], ["lint"])
        self.assertEqual(invocations["macos_build"]["requires"], ["cmake_build"])
        self.assertEqual(
            set(invocations["deploy"]["requires"]),
            {"test_cmake_build", "test_make_build", "macos_build", "containers_check"},
        )
        self.assertEqual(config["jobs"]["test"]["parallelism"], 2)
        for name in invocations:
            self.assertIn("tags", invocations[name]["filters"])


if __name__ == "__main__":
    unittest.main()
