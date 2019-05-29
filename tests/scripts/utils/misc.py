from subprocess import PIPE, run
import sys
from pathlib import Path
import importlib
import shutil
import logging
import contextlib
import os
import datalad
import pytest


def is_omp(toolname):

    OMP = "is compiled using OpenMP" in run(
        [toolname, "-help"], stdout=PIPE
    ).stdout.decode("utf-8")
    return OMP


def try_to_import_afni_module(mod):
    """Function returns an imported object from AFNI's python modules.
    Currently this is required because AFNI's python code is not installed. It
    is on the system path. When on the path, the .py files can be run as an
    executable from anywhere but not imported.

    Args:
        mod (str): A module name to attempt to import from AFNI's installation directory

    Returns:
        module: A python module imported from AFNI's installation directory

    Raises:
        EnvironmentError: If AFNI is not installed this error is raised.
    """
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
                "Cannot specify input data that is located in the sample_test_output directory."
            )
        else:
            raise ValueError(no_file_error)


def generate_fetch_list(input_file, test_data_dir):
    """Given a file this function globs for similar files and returns a list
    containing paths as strings.

    Args:
        input_file (pathlib.Path): A Path object for an relative reference to
        an existing file within the test data directory.
        test_data_dir (pathlib.Path): The test data directory

    Returns:
        list of paths-as-strings: A list of files for datalad to fetch.
    """
    ab = try_to_import_afni_module("afni_base")

    parsed_obj = ab.parse_afni_name(str(test_data_dir / input_file))
    if parsed_obj["type"] == "BRIK":
        globbed_files = list(Path(parsed_obj["path"]).glob(parsed_obj["prefix"] + "*"))
        return [str(f) for f in globbed_files]
    else:
        return [str(input_file)]


def process_path_obj(path_obj, test_data_dir):
    """
    Convert paths to the pathlib Path type and get the data for test_data_dir,
    a datalad repository.

    Args: path_obj (str/pathlib.Path or iterable): Paths as
        strings/pathlib.Path  or non-str iterables with elements of these
        types can be passed as arguments for conversion to Path objects

        test_data_dir (pathlib.Path): An existing datalad repository containing the test data.
    Returns:
        Path or iterable of Paths: path_obj appropriately converted to pathlib Paths
        objects with files in test_data_dir data fetched as required.
    """

    if type(path_obj) == str:
        path_obj = Path(path_obj)

    if isinstance(path_obj, Path):
        check_file_exists(path_obj, test_data_dir)
        file_fetch_list = generate_fetch_list(path_obj, test_data_dir)
        datalad.api.get(dataset=str(test_data_dir), path=file_fetch_list)
        return test_data_dir / path_obj
    elif iter(path_obj):
        file_fetch_list = []
        for input_file in path_obj:
            input_file = Path(input_file)
            check_file_exists(input_file, test_data_dir)
            file_fetch_list = file_fetch_list + generate_fetch_list(
                input_file, test_data_dir
            )

        datalad.api.get(dataset=str(test_data_dir), path=file_fetch_list)

        return [test_data_dir / p for p in path_obj]
    else:

        raise TypeError(
            "data_paths must contain values that are of type str or a "
            "non-str iterable type. i.e. list, tuple... "
        )
