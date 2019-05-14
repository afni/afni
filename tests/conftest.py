import subprocess
import os
from pathlib import Path
from typing import Union
import pytest
import shutil
import inspect
import functools
import datetime as dt
import atexit
from scripts.utils import misc
import scripts.utils.tools as tools
import attr

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
    if hasattr(pytest, "config"):
        outdir = (
            Path(pytest.config.rootdir) / "output_of_tests" / ("output_" + CURRENT_TIME)
        )
        return outdir


def get_test_data_path():
    return Path(pytest.config.rootdir) / "afni_ci_test_data"


def get_test_comparison_dir(current_test_module: Union[str or Path]):
    """Apart from returning the directory for compariing the test output
    (user-specified or a default), this fetches the annex files for the
    appropriate paths in the comparison directory if required. This would be
    needed for the default output that is stored in the test data directory.

    Args: test_name str: The name of the test in the test_module. This is
        used to fetch the appropriate directory in the sample output if
        required

    Returns:
        pathlib.Path:
    """
    test_name = get_current_test_name()
    cmpr_path = get_comparison_dir_path()
    test_compare_dir = cmpr_path / current_test_module.name / test_name

    # Aside from two user-defined conditions the comparison directory should exist
    comparison_data_needs_to_exist = not (
        pytest.config.getoption("--create_sample_output")
        or pytest.config.getoption("--save_sample_output")
    )

    if not test_compare_dir.exists() and comparison_data_needs_to_exist:
        raise ValueError(
            f"The appropriate output for the test {test_name} does not "
            f"exist in the comparison directory {test_compare_dir}. You "
            "may wish to run the test with the --create_sample_output "
            "flag. "
        )

    if cmpr_path.name == "sample_test_output" and comparison_data_needs_to_exist:
        datalad.api.get(str(test_compare_dir))

    return test_compare_dir


def get_comparison_dir_path():
    comparison_dir = pytest.config.getoption("--diff_with_outdir") or (
        get_test_data_path() / "sample_test_output"
    )
    comparison_dir = Path(comparison_dir).absolute()
    if not comparison_dir.exists():
        raise ValueError(f"The test output directory: {comparison_dir} does not exist")
    return comparison_dir


def get_test_data_dir():
    # Define hard-coded paths for now
    test_data_dir = get_test_data_path()
    race_error_msg = (
        "A failed attempt and datalad download occurred. Running the "
        "tests sequentially once may help "
    )

    # datalad is required and the datalad repository is used for data.
    if not (test_data_dir / ".datalad").exists():
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


def get_current_test_name():
    return os.environ.get("PYTEST_CURRENT_TEST").split(":")[-1].split(" ")[0]


@pytest.fixture(scope="function")
def data(request):
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
    module_data_dir = get_test_data_dir()

    # Set module specific values:
    try:
        data_paths = request.module.data_paths
    except AttributeError:
        data_paths = {}

    module_outdir = get_output_dir() / Path(request.module.__file__).stem.replace(
        "test_", ""
    )
    test_logdir = module_outdir / get_current_test_name() / "captured_output"
    if not test_logdir.exists():
        os.makedirs(test_logdir, exist_ok=True)

    # start creating output dict, downloading test data as required
    out_dict = {
        k: misc.process_path_obj(v, module_data_dir) for k, v in data_paths.items()
    }

    # Get the comparison directory and check if it needs to be downloaded
    comparison_dir = get_test_comparison_dir(module_outdir)

    # Define output for calling module and get data as required:
    out_dict.update(
        {
            "module_data_dir": module_data_dir,
            "outdir": module_outdir / get_current_test_name(),
            "logdir": test_logdir,
            "comparison_dir": comparison_dir,
            "test_name": test_name,
        }
    )

    DataClass = attr.make_class(
        test_name + "_data", [k for k in out_dict.keys()], slots=True
    )
    return DataClass(*[v for v in out_dict.values()])


@pytest.fixture(scope="function")
def run_cmd():
    def command_runner(
        cmd,
        current_vars,
        add_env_vars={},
        merge_error_with_output=False,
        workdir=None,
        force_python2=False,
    ):
        """run_cmd is initialized for all test functions that list it as an
        argument. It is used as a callable function to run command line
        arguments. In conjunction with the data fixture defined in this file
        it handles the test output logging in a consistent way. The cmd string
        may require a formatting step where the values contained in
        'current_vars' are injected into the command string.

        Technical note: run_cmd is not a standard function. It is a
        function-scoped pytest fixture that returns a callable function
        (command_runner) that takes the arguments from the user writing a test.
        Args:
            cmd (str): A string that requires execution and error checking.
            Variables will be substituted into the string as required. Following
            python's f-strings syntax, variables are wrapped in braces.

            current_vars (dict):  The current variables (one of which must be
            the data fixture) in the test function scope (accessed by getting
            the values returned by 'locals()') must be provided to this
            callable. Among other things, this uses the data fixture to
            perform variable substitution for the command string

        Returns:
            subprocess.CompletedProcess: An object that among other useful
            attributes contains stdout, stderror of the executed command
        """
        # Get the data object created by the data test fixture
        data = current_vars.get("data", None)
        if not data:
            raise ValueError(
                "When using run_cmd you should use the data fixture for "
                "the test and pass the local variables accessed by "
                "calling 'locals' to the run_cmd callable using the "
                "'current_vars' argument "
            )

        # Set working directory for command execution if not set explicitly
        if not workdir:
            workdir = Path.cwd()

        # Define log file paths
        stdout_log = data.logdir / (data.test_name + "_stdout.log")
        # Make the appropriate output directories
        os.makedirs(stdout_log.parent, exist_ok=True)

        # Confirm that the file does not exist, otherwise append a number:
        stdout_log = tools.uniquify(stdout_log)
        stderr_log = Path(str(stdout_log).replace("_stdout", "_stderr"))

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
                error = subprocess.STDOUT
            else:
                error = subprocess.PIPE

            proc = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=error)
            # log the output
            stdout_log.write_text(proc.stdout.decode("utf-8"))
            if proc.stderr:
                err_text = proc.stderr.decode("utf-8")
                stderr_log.write_text(err_text)

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
        help="Specify a previous tests output directory with which the output "
        "of this test session is compared.",
    )
    parser.addoption(
        "--create_sample_output",
        action="store_true",
        default=False,
        help=(
            "During many of the tests, sample output is required to "
            "assess changes in the output files. With this flag the "
            "fact that the comparison data is missing is ignored so "
            "that a test will run to completion and create all of the "
            "required files for future comparison. "
        ),
    )
    parser.addoption(
        "--save_sample_output",
        action="store_true",
        default=False,
        help=(
            "Tests that required the output of a previous test- session "
            "to compare against will search in the sample_test_output "
            "directory of the afni_ci_test_data repository by default. "
            "This flag runs the tests and updates the 'sample output' "
            "for each respective test. "
        ),
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


def update_sample_output(odir, sample_test_output):

    if not shutil.which("rsync"):
        raise EnvironmentError(
            "Updating sample output requires  a working rsync installation."
        )
    cmd = f"rsync -a {odir}/ {sample_test_output}/"
    with misc.remember_cwd():
        os.chdir(Path(pytest.config.rootdir))

        subprocess.check_call(cmd, shell=True)
        update_msg = "Update data with test run at %s" % odir.name.replace(
            "output_", ""
        )
        result = datalad.api.rev_save(sample_test_output, update_msg, on_failure="stop")
        if not result[0]["status"] == "notneeded":
            print(f"Updating sample data in {sample_test_output}")


def report():
    output_directory = get_output_dir().absolute()
    sample_test_output = get_test_data_path() / "sample_test_output"
    if pytest.config.getoption("--save_sample_output"):
        update_sample_output(output_directory, sample_test_output)

    print("Test output is written to: ", output_directory)


atexit.register(report)
