from datalad.support.exceptions import IncompleteResultsError, CommandError
from filelock import Timeout, FileLock
from multiprocessing import Process
from pathlib import Path
from time import sleep
from typing import Union
from unittest.mock import Mock
import attr
import datalad.api as datalad
import datetime as dt
import logging
import os
import pytest
import random
import shutil
import tempfile

from afni_test_utils import misc, tools
from afni_test_utils.tools import get_current_test_name

DATA_FETCH_LOCK_PATH = Path(tempfile.gettempdir()) / "afni_tests_data.lock"
dl_lock = FileLock(DATA_FETCH_LOCK_PATH, timeout=300)


def get_test_data_path(config_obj):
    if hasattr(config_obj, "rootdir"):
        return Path(config_obj.rootdir) / "afni_ci_test_data"
    elif hasattr(config_obj, "config"):
        return Path(config_obj.config.rootdir) / "afni_ci_test_data"
    else:
        raise ValueError("A pytest config object was expected")


def get_tests_data_dir(config_obj):
    """Get the path to the test data directory. If the test data directory
    does not exist or is not populated, install with datalad.
    """
    logger = logging.getLogger("Test data setup")

    tests_data_dir = get_test_data_path(config_obj)

    # remote should be configured or something is badly amiss...
    dl_dset = datalad.Dataset(str(tests_data_dir))
    if (
        dl_dset.is_installed()
        and "remote.afni_ci_test_data.url" not in dl_dset.config.keys()
    ):
        for f in dl_dset.pathobj.glob("**/*"):
            try:
                f.chmod(0o700)
            except FileNotFoundError:
                # missing symlink, nothing to worry about
                pass
        logger.warn("Not sure about test data, perhaps you should try removing...")
        raise ValueError("Not sure about test data, perhaps you should try removing...")
        # shutil.rmtree(dl_dset.pathobj)

    # datalad is required and the datalad repository is used for data.
    if not (tests_data_dir / ".datalad").exists():
        try:
            global dl_lock
            dl_lock.acquire()
            if not (tests_data_dir / ".datalad").exists():
                logger.warn("Installing test data")
                datalad.install(
                    str(tests_data_dir),
                    "https://github.com/afni/afni_ci_test_data.git",
                    recursive=True,
                    on_failure="stop",
                )
        finally:
            dl_lock.release()
    # Needs to be user writeable:
    some_files = [".git/logs/HEAD"]
    for f in some_files:
        data_file = tests_data_dir / f
        if not data_file.exists():
            raise ValueError(
                f"{f} does not exist (parent existences: {f.parent.exists()}"
            )
        if not os.access(data_file, os.W_OK):
            raise ValueError(f"{f} is not user writeable ({os.getuid()})")
    return tests_data_dir


def save_output_to_repo(config_obj):
    base_comparison_dir_path = get_base_comparison_dir_path(config_obj)

    update_msg = "Update data with test run on {d}".format(
        d=dt.datetime.today().strftime("%Y-%m-%d")
    )

    result = datalad.save(update_msg, str(base_comparison_dir_path), on_failure="stop")

    sample_test_output = get_test_data_path() / "sample_test_output"
    data_message = (
        "New sample output was saved to {sample_test_output} for "
        "future comparisons. Consider publishing this new data to "
        "the publicly accessible servers.. "
    )
    print(data_message.format(**locals()))


def get_test_comparison_dir_path(base_comparison_dir_path, mod: Union[str or Path]):
    """Get full path full comparison directory for a specific test"""
    return base_comparison_dir_path / mod.name / get_current_test_name()


def get_base_comparison_dir_path(config_obj):
    """If the user does not provide a comparison directory a default in the
    test data directory is used. The user can specify a directory containing
    the output of a previous test run or the "sample" output that is created
    by a previous test run when the "--create-sample-output" flag was provided.
    """
    comparison_dir = config_obj.getoption("--diff-with-sample")
    if comparison_dir is not None:
        return Path(comparison_dir).absolute()
    else:
        return get_test_data_path(config_obj) / "sample_test_output"


def get_data_fixture(pytestconfig, request, output_dir):
    """A function-scoped test fixture used for AFNI's testing. The fixture
    sets up output directories as required and provides the named tuple "data"
    to the calling function. The data object contains some fields convenient
    for writing tests like the output directory. Finally the data fixture
    handles test input data.files  listed in a data_paths dictionary (if
    defined within the test module) the fixture will download them to a local
    datalad repository as required. Paths should be listed relative to the
    repository base-directory.

    Args: request (pytest.fixture): A function level pytest request object
        providing information about the calling test function.

    Returns:
        collections.NameTuple: A data object for conveniently handling the specification
    """
    test_name = get_current_test_name()
    tests_data_dir = get_test_data_path(pytestconfig)

    current_test_module = Path(request.module.__file__)
    module_outdir = output_dir / current_test_module.stem.replace("test_", "")
    test_logdir = module_outdir / get_current_test_name() / "captured_output"
    if not test_logdir.exists():
        os.makedirs(test_logdir, exist_ok=True)

    # Add stream and file logging as requested
    logger = logging.getLogger(test_name)
    logger = tools.logger_config(
        logger,
        file=output_dir / "all_tests.log",
        log_file_level=pytestconfig.getoption("--log-file-level"),
    )

    # Set module specific values:
    try:
        data_paths = request.module.data_paths
    except AttributeError:
        data_paths = {}
    # start creating output dict, downloading test data as required
    out_dict = {
        k: process_path_obj(v, tests_data_dir, logger) for k, v in data_paths.items()
    }

    # This will be created as required later
    sampdir = convert_to_sample_dir_path(test_logdir.parent)

    # Get the comparison directory and check if it needs to be downloaded
    base_comparison_dir_path = get_base_comparison_dir_path(pytestconfig)
    comparison_dir = get_test_comparison_dir_path(
        base_comparison_dir_path, module_outdir
    )
    # Define output for calling module and get data as required:
    out_dict.update(
        {
            "module_outdir": module_outdir,
            "logger": logger,
            "current_test_module": current_test_module,
            "outdir": module_outdir / get_current_test_name(),
            "sampdir": sampdir,
            "logdir": test_logdir,
            "comparison_dir": comparison_dir,
            "base_comparison_dir": base_comparison_dir_path,
            "base_outdir": output_dir,
            "tests_data_dir": tests_data_dir,
            "test_name": test_name,
            "rootdir": pytestconfig.rootdir,
            "create_sample_output": pytestconfig.getoption("--create-sample-output"),
            "save_sample_output": pytestconfig.getoption("--save-sample-output"),
        }
    )

    DataClass = attr.make_class(
        test_name + "_data", [k for k in out_dict.keys()], slots=True
    )
    return DataClass(*[v for v in out_dict.values()])


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
        ab = misc.try_to_import_afni_module("afni_base")
    files_out = [path_in]
    brik_pats = [".HEAD", ".BRIK"]
    if any(pat in path_in.name for pat in brik_pats):
        parsed_obj = ab.parse_afni_name(str(test_data_dir / path_in))
        if parsed_obj["type"] == "BRIK":
            globbed = Path(parsed_obj["path"]).glob(parsed_obj["prefix"] + "*")
            files_out += list(globbed)
            files_out = list(set(files_out))

    return files_out


def process_path_obj(path_obj, test_data_dir, logger=None):
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
        attempt_count = 0
        while attempt_count < 2:
            # fetch data with a global dl_lock
            fetch_status = try_data_download(files_to_fetch, test_data_dir, logger)
            if fetch_status:
                break
            else:
                attempt_count += 1
        else:
            # datalad download attempts failed
            pytest.exit(
                f"Datalad download failed {attempt_count} times, you may "
                "not be connected to the internet "
            )
        logger.info(f"Downloaded data for {test_data_dir}")
    path_obj = [test_data_dir / p for p in path_obj]
    if len(path_obj) == 1:
        return path_obj[0]
    else:
        return path_obj


def try_data_download(file_fetch_list, test_data_dir, logger):
    try:
        global dl_lock
        dl_lock.acquire(poll_intervall=1)
        dl_dset = datalad.Dataset(str(test_data_dir))
        # Fetching the data
        process_for_fetching_data = Process(
            target=dl_dset.get, kwargs={"path": [str(p) for p in file_fetch_list]}
        )

        # attempts should be timed-out to deal with unpredictable stalls.
        process_for_fetching_data.start()
        # logger.debug(f"Fetching data for {test_data_dir}")
        process_for_fetching_data.join(timeout=60)
        if process_for_fetching_data.is_alive():
            # terminate the process.
            process_for_fetching_data.terminate()
            # logger.warn(f"Data fetching timed out for {file_fetch_list}")
            return False
        elif process_for_fetching_data.exitcode != 0:
            # logger.warn(f"Data fetching failed for {file_fetch_list}")
            return False
        else:
            return True
    except (
        IncompleteResultsError,
        ValueError,
        CommandError,
        TimeoutError,
        Timeout,
    ) as err:
        logger.warn(
            f"Datalad download failure ({type(err)}) for {test_data_dir}. Will try again"
        )

        return False

    finally:
        dl_lock.release()
        sleep(random.randint(1, 10))


def convert_to_sample_dir_path(output_dir):
    sampdir = Path(str(output_dir).replace("output_", "sample_output_"))
    return sampdir
