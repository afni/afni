from subprocess import PIPE, run
import sys
from pathlib import Path
import importlib
import shutil
import logging
import contextlib
import os
import datalad.api as datalad
from datalad.support.exceptions import IncompleteResultsError, CommandError
import pytest
from time import sleep
import random

from multiprocessing import Lock, Process
from xvfbwrapper import Xvfb

lock = Lock()


def run_x_prog(cmd, run_kwargs=None):
    import subprocess as sp

    run_kwargs = run_kwargs or {}
    with Xvfb() as xvfb:
        res = sp.run(cmd, shell=True, stdout=sp.PIPE, stderr=sp.STDOUT, **run_kwargs)

    res.check_returncode()
    if "ERROR" in res.stdout.decode():

        raise ValueError(
            f"""
        {cmd}
        Command executed, but output contains an error {res.stdout}
        """
        )

    return res.stdout.decode("utf")


def is_omp(toolname):

    OMP = "is compiled using OpenMP" in run(
        [toolname, "-help"], stdout=PIPE
    ).stdout.decode("utf-8")

    # for now skip all files that want use omp if that was not used in the build
    if not OMP:
        pytestmark = pytest.mark.skip("skipping entire module")
    return OMP


def try_to_import_afni_module(mod):
    """Function returns an imported object from AFNI's python modules.
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
    if mod not in ["1d_tool", "afni_base"]:
        raise ImportError(
            """This functionality is removed. If the afnipy package is
                      not installed into the current python interpretter and
                      you do not wish to do this you can instead set PYTHONPATH
                      to AFNI's installation directory."""
        )

    # For now the only place checked is the parent directory of a python executable from afni:
    executable = shutil.which("align_epi_anat.py")
    if not executable:
        raise EnvironmentError(
            "Cannot find align_epi_anat.py, an executable file that should be on your path if you have afni installed"
        )

    possible_installation_dir = Path(executable).parent
    sys.path.append(str(possible_installation_dir))
    initfile = possible_installation_dir / "__init__.py"
    if not initfile.exists():
        logging.info(f"Creating init file to make afni code importable {initfile}")
        initfile.touch()
    imported_mod = importlib.__import__(mod)

    return imported_mod


@contextlib.contextmanager
def remember_cwd():
    curdir = os.getcwd()
    try:
        yield
    finally:
        os.chdir(curdir)


def check_file_exists(file_path, test_data_dir):
    full_path = test_data_dir / file_path
    no_file_error = (
        f"Could not find {full_path}. You have specified the path "
        f"{file_path} for an input datafile but this path does not exist "
        "in the test data directory that has been created in "
        f"{test_data_dir} "
    )

    if not (full_path.exists() or full_path.is_symlink()):
        if "sample_test_output" in full_path.parts:
            raise ValueError(
                "Cannot specify input data that is located in the "
                "sample_test_output directory. "
            )

        else:
            raise ValueError(no_file_error)


def generate_fetch_list(path_obj, test_data_dir):
    """Provided with path_obj, a list of pathlib.Path objects, resolves to a
    list containing 1 or more pathlib.Path objects.

    Args:
        path_obj (TYPE): may be a list of paths, a path, or a path that
    contains a glob pattern at the end
        test_data_dir (TYPE): Description

    Returns:
        List: List of paths as str type (including HEAD files if BRIK is used)
        Bool: needs_fetching, True if all data has not been downloaded

    Raises:
        TypeError: Description
    """
    needs_fetching = False
    fetch_list = []
    for p in path_obj:
        with_partners = add_partner_files(test_data_dir, p)
        for pp in with_partners:
            # fetch if any file does not "exist" (is a broken symlink)
            needs_fetching = needs_fetching or not (test_data_dir / pp).exists()
        fetch_list += with_partners

    return [str(f) for f in fetch_list], needs_fetching


def glob_if_necessary(test_data_dir, path_obj):
    """
    Check that path/paths exist in test_data_dir. Paths  may be a
    glob, so tries globbing before raising an error that it doesn't exist. Return
    the list of paths.
    """
    if type(path_obj) == str:
        path_obj = [Path(path_obj)]
    elif isinstance(path_obj, Path):
        path_obj = [path_obj]
    elif iter(path_obj):
        path_obj = [Path(p) for p in path_obj]
    else:
        raise TypeError(
            "data_paths must contain paths (values that are of type str, pathlib.Path) or a "
            "non-str iterable type containing paths. i.e. list, tuple... "
        )

    outfiles = []

    for file_in in path_obj:

        try:
            # file should be found even if just as an unresolved symlink
            check_file_exists(file_in, test_data_dir)
            outfiles.append(file_in)
        except ValueError as e:
            outfiles += [f for f in (test_data_dir / file_in.parent).glob(file_in.name)]
            if not outfiles:
                raise e

    return outfiles


def add_partner_files(test_data_dir, path_in):
    """
    If the path is a brikor a head file the pair is returned for the purposes
    of fetching the data via datalad
    """
    try:
        from afnipy import afni_base as ab
    except ImportError:
        ab = try_to_import_afni_module("afni_base")
    files_out = [path_in]
    brik_pats = [".HEAD", ".BRIK"]
    if any(pat in path_in.name for pat in brik_pats):
        parsed_obj = ab.parse_afni_name(str(test_data_dir / path_in))
        if parsed_obj["type"] == "BRIK":
            globbed = Path(parsed_obj["path"]).glob(parsed_obj["prefix"] + "*")
            files_out += list(globbed)
            files_out = list(set(files_out))

    return files_out


def process_path_obj(path_obj, test_data_dir):
    """
    This function is used to process paths that have been defined in the
    data_paths dictionary of test modules. Globs are resolved, and the data is
    fetched using datalad. If HEAD files are provided, the corresponding BRIK
    files are also downloaded.

    Args: path_obj (str/pathlib.Path or iterable): Paths as
        strings/pathlib.Path  or non-str iterables with elements of these
        types can be passed as arguments for conversion to Path objects.
        Globbing at the final element of the path is also supported and will
        be resolved before being returned.

        test_data_dir (pathlib.Path): An existing datalad repository
        containing the test data.
    Returns:

        Iterable of Paths: Single pathlib.Path object or list of pathlib Paths
        fetched as required.
    """

    # Resolve all globs and return a list of pathlib objects
    path_obj = glob_if_necessary(test_data_dir, path_obj)
    # Search for any files that might be missing eg HEAD for a BRIK
    files_to_fetch, needs_fetching = generate_fetch_list(path_obj, test_data_dir)

    # Fetching the data
    if needs_fetching:
        # fetch data with a global lock
        try_data_download(files_to_fetch, test_data_dir)

    path_obj = [test_data_dir / p for p in path_obj]
    if len(path_obj) == 1:
        return path_obj[0]
    else:
        return path_obj


def try_data_download(file_fetch_list, test_data_dir):
    global lock
    dl_dset = datalad.Dataset(str(test_data_dir))
    attempt_count = 0
    lock.acquire()
    while attempt_count < 2:
        try:
            # Fetching the data
            process_for_fetching_data = Process(
                target=dl_dset.get, kwargs={"path": [str(p) for p in file_fetch_list]}
            )

            # attempts should be timed-out to deal with of unpredictable stalls.
            process_for_fetching_data.start()
            process_for_fetching_data.join(timeout=30)
            if process_for_fetching_data.is_alive():
                # terminate the process.
                process_for_fetching_data.terminate()
                raise IncompleteResultsError(
                    f"Data fetching timed out for {file_fetch_list}"
                )
            elif process_for_fetching_data.exitcode != 0:
                raise ValueError(f"Data fetching failed for {file_fetch_list}")
            else:
                lock.release()
                return
        except (IncompleteResultsError, CommandError) as e:
            # Try another loop
            attempt_count += 1
            # make sure datalad repo wasn't updated to git annex version 8. Not sure why this is happening
            git_config_file = Path(test_data_dir) / ".git" / "config"
            git_config_file.write_text(
                git_config_file.read_text().replace("version = 8", "version = 7")
            )
            continue

    # datalad download attempts failed
    pytest.exit(
        "Datalad download failed 5 times, you may not be connected to the internet"
    )
