import subprocess
import os
from pathlib import Path
import pytest
import shutil
from collections import namedtuple
import inspect
import functools
import datetime as dt
import atexit
from scripts.utils import misc


missing_dependencies = (
    "In order to download data an installation of datalad, wget, or "
    "curl is required. Datalad is recommended to restrict the amount of "
    "data downloaded. "
)

try:
    import datalad.api

except ImportError:
    raise NotImplementedError("Currently datalad is a dependency for testing.")

CURRENT_TIME = dt.datetime.strftime(dt.datetime.today(), "%Y_%m_%d_%H%M%S")


def get_output_dir():
    return Path(pytest.config.rootdir) / ("output_" + CURRENT_TIME)


def get_test_dir_path():
    return Path(pytest.config.rootdir) / "afni_ci_test_data"


def get_comparison_dir_path():
    comparison_dir = pytest.config.getoption("--diff_with_outdir") or (
        get_test_dir_path() / "sample_test_output"
    )
    comparison_dir = Path(comparison_dir).absolute()
    if not comparison_dir.exists():
        raise ValueError(f"The test output directory: {comparison_dir} does not exist")
    return comparison_dir


def get_test_dir():
    # Define hard-coded paths for now
    test_data_dir = get_test_dir_path()
    race_error_msg = (
        "A failed attempt and datalad download occurred. Running the "
        "tests sequentially once may help "
    )

    # datalad is required and the datalad repository is used for data.
    if not test_data_dir.exists():
        try:
            datalad.api.install(
                str(test_data_dir), "https://github.com/afni/afni_ci_test_data.git"
            )
        except FileExistsError as e:
            # likely a race condition
            print(e)
            raise FileExistsError(race_error_msg)
        except FileNotFoundError:
            raise FileNotFoundError(race_error_msg)

    return test_data_dir


@pytest.fixture(scope="module")
def data(request):
    test_data_dir = get_test_dir()

    # Read values from calling module:
    data_paths = request.module.data_paths
    test_outdir = get_output_dir() / Path(request.module.__file__).stem.replace(
        "test_", ""
    )

    # Make appropriate directories if they don't already exist
    if not test_outdir.parent.exists():
        test_outdir.parent.mkdir(exist_ok=True)
    if not test_outdir.exists():
        test_outdir.mkdir(exist_ok=True)

    # Define output for calling module and get data as required:
    out_dict = {
        k: misc.process_path_obj(v, test_data_dir) for k, v in data_paths.items()
    }

    out_dict.update(
        {
            "test_data_dir": test_data_dir,
            "outdir": test_outdir,
            "comparison_dir": get_comparison_dir_path(),
        }
    )
    return namedtuple("DataTuple", out_dict.keys())(**out_dict)


@pytest.fixture(scope="module")
def run_cmd():
    def command_runner(
        cmd,
        current_vars={},
        add_env_vars={},
        merge_error_with_output=False,
        workdir=None,
    ):
        """run_cmd is initialized for all test functions that list it as an
        argument. It is used as a callable function to run command line arguments.
        The cmd string may require a formatting step where the values contained in
        'current_vars' are injected into the command string.

        Technical note: check_cmd is not a standard function. It is a
        module-scoped pytest fixture that returns a callable function
        (command_runner) that takes the arguments from the user writing a test.
        Args:
            cmd (str): A string that requires execution and error checking.
            Variables will be substituted into the string as required. Following
            python's f-strings syntax, variables are wrapped in braces.

            current_vars (dict, optional):  If variable substitution is required,
            the user must provide a dictionary of variables for this substitution
            process. Passing the dictionary from 'locals()' is the most convenient
            method of doing this.

        Returns:
            subprocess.CompletedProcess: An object that among other useful
            attributes contains stdout, stderror of the executed command
        """

        # Set working directory for command execution if not set explicitly
        if not workdir:
            workdir = Path.cwd()

        base_outdir = get_output_dir()
        # try to extract the name of the calling function to label captured
        # sys output from the command execution.
        calling_function = inspect.stack()[1].function
        if not calling_function.startswith("test_"):
            calling_function = "unknown"
        log_file_path = (
            base_outdir
            / "captured_output"
            / (calling_function + "_" + cmd.split()[0] + ".log")
        )
        log_file_path.parent.mkdir(exist_ok=True)

        # Set environment variables for the command execution
        os.environ["OMP_NUM_THREADS"] = "1"
        for k, v in add_env_vars.items():
            os.environ[k] = v

        # Tidy whitespace and sub variables into command
        cmd = " ".join(cmd.format(**current_vars).split())
        print(cmd)
        with misc.remember_cwd():
            os.chdir(workdir)
            # Execute the command and log output
            if merge_error_with_output:
                proc = subprocess.run(
                    cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT
                )
                log_file_path.write_text(proc.stdout.decode("utf-8"))
            else:
                proc = subprocess.run(
                    cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
                )
                log_file_path.with_suffix(".out.log").write_text(
                    proc.stdout.decode("utf-8")
                )
                log_file_path.with_suffix(".err.log").write_text(
                    proc.stderr.decode("utf-8")
                )

            # Raise error if there was a non-zero exit code.
            proc.check_returncode()

        return proc

    return command_runner


# configure keywords that alter test collection
def pytest_addoption(parser):
    parser.addoption(
        "--runslow",
        action="store_true",
        default=False,
        help="run slow tests whose execution time is on the order of many seconds)",
    )
    parser.addoption(
        "--runveryslow",
        action="store_true",
        default=False,
        help="run very slow tests whose execution time is on the order "
        "of many minutes to hours ",
    )
    parser.addoption(
        "--diff_with_outdir",
        default=None,
        help="Specify a previous output directory with which the output "
        "of this test session is compared.",
    )


def pytest_collection_modifyitems(config, items):
    # more and more tests are skipped as each premature return is not executed:
    if config.getoption("--runveryslow"):
        # --runveryslow given in cli: do not skip slow tests
        return
    else:
        skip_veryslow = pytest.mark.skip(reason="need --runveryslow option to run")
        for item in items:
            if "veryslow" in item.keywords:
                item.add_marker(skip_veryslow)

    if config.getoption("--runslow"):
        # --runslow given in cli: do not skip slow tests
        return
    skip_slow = pytest.mark.skip(reason="need --runslow option to run")
    for item in items:
        if "slow" in item.keywords:
            item.add_marker(skip_slow)


def report():
    print("Test output is written to: ", get_output_dir().absolute())


atexit.register(report)
