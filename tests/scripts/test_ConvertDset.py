"""Synthetic regression checks for ConvertDset's plain numeric output.

Run directly with --bin /path/to/ConvertDset, or through pytest using PATH.
No downloaded datasets or third-party Python modules are required.
"""

import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


data_paths = {}


class TestConvertDset(unittest.TestCase):
    executable = None

    def setUp(self):
        self.program = self.executable or shutil.which("ConvertDset")
        self.assertIsNotNone(self.program, "ConvertDset must be available on PATH")
        work = tempfile.TemporaryDirectory()
        self.addCleanup(work.cleanup)
        self.work = Path(work.name)
        self.values = [[1.25, -2.0], [3.5, 4.0], [-5.0, 6.75]]
        self.nodes = [2, 7, 11]
        self.input = self.work / "values.1D"
        self.input.write_text("1.25 -2\n3.5 4\n-5 6.75\n")
        self.indices = self.work / "nodes.1D"
        self.indices.write_text("2\n7\n11\n")

    def check_output(self, destination):
        """Check shape, values and index placement, including nonempty streams."""
        for transpose in (False, True):
            for add_index in (False, True):
                with self.subTest(transpose=transpose, add_index=add_index):
                    mode = "1Dpt" if transpose else "1Dp"
                    output = self.work / (mode + str(add_index) + ".1D.dset")
                    command = [
                        self.program,
                        "-input",
                        str(self.input),
                        "-node_index_1D",
                        str(self.indices),
                        "-o_" + mode + ("_" + destination if destination else ""),
                    ]
                    if add_index:
                        command.append("-prepend_node_index_1D")
                    if not destination:
                        command.extend(["-prefix", str(output)])
                    result = subprocess.run(
                        command,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        universal_newlines=True,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    if destination == "stdout":
                        text = result.stdout
                        self.assertEqual(result.stderr, "")
                    elif destination == "stderr":
                        text = result.stderr
                        self.assertEqual(result.stdout, "")
                    else:
                        text = output.read_text()
                    # Do not strip comment headers: these modes promise pure numbers.
                    actual = [
                        list(map(float, row.split()))
                        for row in text.splitlines()
                        if row.strip()
                    ]
                    if transpose:
                        expected = [list(col) for col in zip(*self.values)]
                        if add_index:
                            expected = [self.nodes] + expected
                    else:
                        expected = [row[:] for row in self.values]
                        if add_index:
                            expected = [
                                [node] + row for node, row in zip(self.nodes, expected)
                            ]
                    self.assertEqual(actual, expected)

    def test_file(self):
        self.check_output("")

    def test_stdout(self):
        self.check_output("stdout")

    def test_stderr(self):
        self.check_output("stderr")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bin", required=True, help="ConvertDset executable to test")
    args = parser.parse_args()
    TestConvertDset.executable = str(Path(args.bin).resolve())
    unittest.main(argv=[__file__])
