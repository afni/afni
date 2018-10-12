"""Show the help messages of all AFNI programs. Fail test if showing help
message fails or if program is not found.

To run:
    python3 test_help_messages.py
"""

import os
import pathlib
import shutil
import subprocess

here = os.path.realpath(os.path.dirname(__file__))

# This assumes we are in afni_root/tests/pytest_tests directory.
AFNI_ROOT = os.path.join(here, '..', '..')

SHOULD_NOT_BE_EXECUTABLE = [
    'afni_fs_aparc+aseg_2000.txt',
    'afni_fs_aparc+aseg_2009.txt',
    'demo.fixed.niml.do',
    'demo.mobile.niml.do']

KNOWN_BROKEN_HELP = [
    'fat_proc_grad_plot']


def _get_programs(afni_root):
    """Return list of AFNI programs, given path to AFNI's root directory,
    right above src.
    """
    p = pathlib.Path(afni_root).absolute()
    # Create list of AFNI programs.
    subprocess.run(
        ['make', 'prog_list'], stdout=subprocess.DEVNULL, cwd=str(p / 'src'))
    # Parse and return list of AFNI programs.
    prog_list = p / 'src' / 'prog_list.txt'
    with prog_list.open() as fp:
        progs = fp.readlines()
    progs = [j.strip() for j in progs]
    return [j for j in progs if j and not j.startswith('#')]


def test_prog_list_helps():
    programs = _get_programs(AFNI_ROOT)
    not_found = []
    no_success = []

    for prog in programs:
        if prog in SHOULD_NOT_BE_EXECUTABLE or prog in KNOWN_BROKEN_HELP:
            continue
        if shutil.which(prog) is None:  # Program does not exist.
            not_found.append(prog)
            continue

        # Run program's help.
        process = subprocess.run(
            [prog, '-help'], stderr=subprocess.PIPE, stdout=subprocess.DEVNULL)
        if process.returncode != 0:
            msg = "return code {}".format(process.returncode)
            if process.stderr:
                msg = process.stderr.splitlines()[-1].decode()
            no_success.append(
                "{} ({})".format(prog, msg))

    if not_found:
        print("PROGRAMS NOT FOUND:")
        print("    " + "\n    ".join(not_found))

    if no_success:
        print("PROGRAMS THAT FAILED:")
        print("    " + "\n    ".join(no_success))

    if not_found or no_success:
        assert False


def main():
    test_prog_list_helps()


if __name__ == '__main__':
    main()
