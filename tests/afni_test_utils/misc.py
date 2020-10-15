from subprocess import PIPE, run
import importlib
import contextlib
import os
import pytest


def is_omp(toolname):

    OMP = "is compiled using OpenMP" in run(
        [toolname, "-help"], stdout=PIPE
    ).stdout.decode("utf-8")

    # for now skip all files that want use omp if that was not used in the build
    if not OMP:
        pytestmark = pytest.mark.skip("skipping entire module")
    return OMP


def try_to_import_afni_module(mod):
    """
    This function is no longer needed because afnipy is added to sys.path in
    the conftest.py

    Old description:
    Function returns an imported object from AFNI's python modules.
    Currently this is required because AFNI's python code is not installed. It
    is on the system path. When on the path, the .py files can be run as an
    executable from anywhere but not imported. It's use should be limited with
    a view to eventually eradicate its need.

    Args:
        mod (str): A module name to attempt to import from AFNI's installation directory

    Returns:
        module: A python module imported from AFNI's installation directory

    Raises:
        EnvironmentError: If AFNI is not installed this error is raised.
    """
    return importlib.import_module(mod)


@contextlib.contextmanager
def remember_cwd():
    curdir = os.getcwd()
    try:
        yield
    finally:
        os.chdir(curdir)
