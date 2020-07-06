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
        pytestmark = pytest.mark.skip('skipping entire module')
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
    """Summary
    
    Args:
        path_obj (TYPE): Description
        test_data_dir (TYPE): Description
    
    Returns:
        List: List of paths as str type (including HEAD files if BRIK is used)
        Bool: needs_fetching, True if all data has not been downloaded
    
    Raises:
        TypeError: Description
    """
    if type(path_obj) == str:
        path_obj = [Path(path_obj)]
    elif isinstance(path_obj, Path):
        path_obj = [path_obj]
    elif iter(path_obj):
        return_val = [Path(p) for p in path_obj]
    else:
        raise TypeError(
            "data_paths must contain values that are of type str or a "
            "non-str iterable type. i.e. list, tuple... "
        )
    needs_fetching = False
    fetch_list = []
    for p in path_obj:
        # add HEAD files if BRIK is given
        current_file_list = get_extra_files(p, test_data_dir)
        for pp in current_file_list:
            # file should be found even if just as an unresolved symlink
            check_file_exists(pp, test_data_dir)
            # fetch if any file does not "exist" (is a broken symlink)
            needs_fetching = needs_fetching or not (test_data_dir / pp).exists()
        fetch_list += current_file_list

    return fetch_list, needs_fetching


def get_extra_files(input_file, test_data_dir):
    """Given a file this function globs for similar files and returns a list
    containing Paths.

    Args:
        input_file (pathlib.Path): A Path object for an relative reference to
        an existing file within the test data directory.
        test_data_dir (pathlib.Path): The test data directory

    Returns:
        list of paths: A list of files for datalad to fetch.
    """
    # skip tests without afnipy module but this function is required for all tests
    try:
        from afnipy import afni_base as ab
    except ImportError:
        ab = try_to_import_afni_module("afni_base")
    parsed_obj = ab.parse_afni_name(str(test_data_dir / input_file))
    if parsed_obj["type"] == "BRIK":
        globbed_files = list(Path(parsed_obj["path"]).glob(parsed_obj["prefix"] + "*"))
        return [f for f in globbed_files]
    else:
        return [input_file]


def process_path_obj(path_obj, test_data_dir):
    """
    Get the data for test_data_dir,
    a datalad repository.

    Args: path_obj (str/pathlib.Path or iterable): Paths as
        strings/pathlib.Path  or non-str iterables with elements of these
        types can be passed as arguments for conversion to Path objects

        test_data_dir (pathlib.Path): An existing datalad repository containing the test data.
    Returns:
        Iterable of Paths: iterable pathlib Paths fetched as required.
    """
    files_to_fetch, needs_fetching = generate_fetch_list(path_obj, test_data_dir)

    # Fetching the data
    if needs_fetching:
        # fetch data with a global lock
        try_data_download(files_to_fetch, test_data_dir)

    if isinstance(path_obj, list):
        return [str(test_data_dir / p) for p in path_obj]
    else:
        return str(test_data_dir / path_obj)


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
