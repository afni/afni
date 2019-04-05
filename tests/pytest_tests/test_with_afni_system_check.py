"""Show the help messages of all AFNI programs. Fail test if showing help
message fails or if program is not found.

To run:
    python3 test_with_afni_system_check.py
"""

import os
import subprocess

here = os.path.realpath(os.path.dirname(__file__))


def test_with_afni_system_check():
    cmd = "afni_system_check.py -check_all"
    proc = subprocess.run(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    proc.check_returncode()


def main():
    test_with_afni_system_check()


if __name__ == "__main__":
    main()
