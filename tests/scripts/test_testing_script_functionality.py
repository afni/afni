from argparse import Namespace
from collections import namedtuple, defaultdict
import pathlib
from pathlib import Path
from unittest.mock import Mock, MagicMock
import importlib
import logging
import os
import pytest
import runpy
import shlex
import shutil
import subprocess
import subprocess as sp
import signal
import sys
import tempfile
import time
import io
from contextlib import redirect_stdout
import contextlib

docker = pytest.importorskip("docker", reason="python docker is not installed")

# import afni_test_utils as atu
from afni_test_utils import run_tests_func
from afni_test_utils import run_tests_examples
from afni_test_utils import container_execution as ce
from afni_test_utils import tools
from afni_test_utils import minimal_funcs_for_run_tests_cli as minfuncs
import afnipy

# import the whole package for mocking purposes
import afni_test_utils

TESTS_DIR = Path(__file__).parent.parent
SCRIPT = TESTS_DIR.joinpath("run_afni_tests.py")
# The default args to pytest will likely change with updates
DEFAULT_ARGS = "scripts --tb=no -r fEs --show-capture=no"
PYTEST_COV_FLAGS = "--cov=afnipy --cov-report xml:$PWD/coverage.xml"
RETCODE_0 = Mock(**{"returncode": 0, "stdout": b"", "stderr": b""})

try:
    docker.from_env()
    DOCKER_AVAILABLE = True
except:
    DOCKER_AVAILABLE = False


data_paths = {"uni_text": "mini_data/unicode_text.txt"}


@pytest.fixture()
def mocked_script(monkeypatch):
    temp_dir = tempfile.mkdtemp()
    exe_dir = Path(temp_dir) / "tests"
    exe_dir.mkdir()
    monkeypatch.syspath_prepend(exe_dir)

    script = Path(
        tempfile.mkstemp(dir=exe_dir, prefix="script_test_", suffix=".py")[-1]
    )
    return script


@pytest.fixture()
def sp_with_successful_execution():

    RUN_WITH_0 = MagicMock(
        **{
            "run.return_value": RETCODE_0,
            "check_output.return_value": b"",
            "returncode": 0,
        }
    )

    return RUN_WITH_0


def run_main_func(script_obj, sys_exit):
    if sys_exit:
        with pytest.raises(SystemExit) as err:
            script_obj.main()
        assert err.typename == "SystemExit"
        assert err.value.code == 0
    else:
        script_obj.main()


def run_script_and_check_imports(
    script_path,
    argslist,
    expected,
    not_expected,
    monkeypatch,
    sys_exit=True,
    no_output=True,
):
    """
    This needs to be used with the mocked_script fixture.

    This is very hacky. It is testing something hacky though. The overall goal
    is to try to only import dependencies of the core testing script as
    needed. In order for that behavior to be robust it must be tested here.

    I tried runpy as a way to execute the script but it didn't work (it was
    modifying sys.argv in a way I could not decipher).

    sys.argv is temporarily modified to test different "user" arguments during
    the import and subsequent execution. Subsequently we check if the
    appropriate imports have occurred.
    """
    with monkeypatch.context() as m:
        m.setattr(sys, "argv", [script_path.name, *argslist])

        script = importlib.import_module(script_path.stem)

        for func_or_mod in not_expected:
            if func_or_mod:
                assert not getattr(script, func_or_mod, None)
        for func_or_mod in expected:
            if func_or_mod:
                assert getattr(script, func_or_mod, None)

        if no_output:
            with contextlib.redirect_stdout(io.StringIO()):
                run_main_func(script, sys_exit)
        else:
            run_main_func(script, sys_exit)


@pytest.mark.parametrize(
    "params",
    [
        {
            "test_case": "quick and simple",
            "cmd_args": ["sleep", "0.1"],
            "timeout": 1,
            "expected_to_timeout": False,
        },
        {
            "test_case": "timeout",
            "cmd_args": ["sleep", "5"],
            "timeout": 1,
            "expected_to_timeout": True,
        },
    ],
)
def test_execute_cmd_args(params):
    try:
        proc = tools.__execute_cmd_args(
            params["cmd_args"],
            logging,
            "text.txt",
            "other_text.txt",
            tempfile.gettempdir(),
            timeout=params["timeout"],
        )
        timed_out = False
    except (TimeoutError, subprocess.TimeoutExpired):
        timed_out = True

    assert timed_out == params["expected_to_timeout"]


def test_run_cmd_timeout(data, monkeypatch):
    data.logger = logging
    monkeypatch.setattr(logging, "warn", lambda x: None)

    # Can slow this down for debugging purposes
    t_unit = 0.2
    # # easy situation, processes behave and clean up after themselves or they
    # # timeout
    tools.run_cmd(
        data,
        f"sleep {t_unit *10} & sleep {t_unit}; kill %1",
        timeout=t_unit * 2,
    )

    # timeout error should be raised
    start = time.time()
    with pytest.raises(TimeoutError):
        tools.run_cmd(
            data,
            f"sleep {t_unit} & sleep {t_unit}",
            timeout=t_unit * 0.5,
        )

    # Backgrounded commands will be killed quickly upon timeout
    start = time.time()
    with pytest.raises(TimeoutError):
        tools.run_cmd(
            data,
            f"sleep {t_unit * 500} & sleep {t_unit / 2}",
            timeout=t_unit,
        )
    delta_t = time.time() - start
    # Command should return quickly (with some wiggle for sleeping)
    assert delta_t < t_unit + 1.1

    # should not timeout a background process until the timeout so this should
    # work fine
    stdout_log, stderr_log = tools.run_cmd(
        data,
        f"sleep {t_unit / 2}; echo hello & sleep {t_unit / 5}",
        timeout=t_unit * 1,
    )
    assert stdout_log.read_text() == "hello\n"


def test_rewrite_paths_in_logs(data):
    # run a command that will contain a path that should be rewritten
    cmd = f"echo some output with a path... {data.outdir / 'sample.txt'}"
    differ = tools.OutputDiffer(data, cmd)
    differ.run()

    cmd = f"echo some output with a path... {data.outdir / 'sample.txt'}"
    differ = tools.OutputDiffer(data, cmd)
    differ.run()


def test_command_logger_setup(data, monkeypatch):
    # logs should be in data.logdir and logger should be logger_in
    logger_in = data.logger
    logger_out, cmd_log, stdout_log, stderr_log = tools.setup_logging(data, logger_in)
    assert logger_out == logger_in
    assert all(p.parent == data.logdir for p in [cmd_log, stdout_log, stderr_log])

    # logs should be in data.logdir and logger should be data.logger
    logger_out, cmd_log, stdout_log, stderr_log = tools.setup_logging(data, None)
    assert logger_out == data.logger
    assert all(p.parent == data.logdir for p in [cmd_log, stdout_log, stderr_log])

    # logs should be in data.logdir and logger should be root logger
    data.logger = None
    logger_out, cmd_log, stdout_log, stderr_log = tools.setup_logging(data, None)
    assert logger_out == logging
    assert all(p.parent == data.logdir for p in [cmd_log, stdout_log, stderr_log])


def test_run_cmd_handles_unicode(data):
    # not sure why but at one point Path().write_text() uses the encoding
    # scheme ascii when None was specified and so one would expect utf-8 to be
    # used. A lesson for those not adding explicitly defining encoding (me).

    # unicode in stdout should not cause any problems
    differ = tools.OutputDiffer(
        data,
        f"""cat {data.uni_text}""",
    )
    differ.run()


def test_check_git_config(
    monkeypatch,
):
    mocked_run_output = Mock()
    mocked_run_output.stdout = b""
    mocked_sp = Mock()
    mocked_sp.run.return_value = mocked_run_output
    mocked_sp.check_output.return_value = b"a_value"

    monkeypatch.setattr(
        afni_test_utils.minimal_funcs_for_run_tests_cli,
        "sp",
        mocked_sp,
    )
    monkeypatch.setattr(
        afni_test_utils.minimal_funcs_for_run_tests_cli,
        "is_containerized",
        Mock(return_value=False),
    )
    # if credentials are unset should raise error
    with monkeypatch.context() as m:
        m.setattr(os, "environ", {})
        with pytest.raises(EnvironmentError):
            minfuncs.check_git_config()

    # env variables should be sufficient
    with monkeypatch.context() as m:
        m.setattr(
            os,
            "environ",
            {"GIT_AUTHOR_NAME": "user", "GIT_AUTHOR_EMAIL": "user@mail.com"},
        )
        minfuncs.check_git_config()

    # credentials should be sufficient
    with monkeypatch.context() as m:
        m.setattr(os, "environ", {})
        # simulate existing credentials
        mocked_run_output.stdout = b"a_value"
        minfuncs.check_git_config()


def test_check_git_config_containerized(sp_with_successful_execution, monkeypatch):
    def existing_dockerenv(*args, **kwargs):
        """
        local function to help pretend this test is containerized
        """
        if "/.dockerenv" in args:
            mocked_path = Mock()
            mocked_path.exists.return_value = True
            return mocked_path
        else:
            return Path(*args, **kwargs)

    monkeypatch.setattr(minfuncs, "sp", sp_with_successful_execution)
    # Pretend this is running in a container
    monkeypatch.setattr(
        minfuncs,
        "Path",
        existing_dockerenv,
    )
    # if containerized local setting might still return credentials

    # if containerized env vars should still work unless they were not sent in

    # if containerized if all else fails set a default

    # if credentials are unset should be fine if containerized and set to a default
    with monkeypatch.context() as m:
        m.setattr(os, "environ", {})
        # sp_with_successful_execution.check_output.return_value = b'a_value'
        u, e = minfuncs.check_git_config()
        sp_with_successful_execution.check_output.assert_any_call(
            "git config --global user.name 'AFNI CircleCI User'",
            shell=True,
        )
        sp_with_successful_execution.check_output.assert_any_call(
            "git config --global user.email 'johnleenimh+circlecigitconfig@gmail.com'",
            shell=True,
        )
        assert u == "AFNI CircleCI User"
        assert e == "johnleenimh+circlecigitconfig@gmail.com"

    # env variables should set the value if not set on  the system
    with monkeypatch.context() as m:
        m.setattr(
            os,
            "environ",
            {"GIT_AUTHOR_NAME": "user", "GIT_AUTHOR_EMAIL": "user@mail.com"},
        )
        # credentials should be sufficient, and overwrite env vars (they
        # can leak into container via local config in mounted source)
        credential_returned = Mock(**{"stdout": b"a_value"})
        sp_with_successful_execution.run.return_value = credential_returned
        gituser, gitemail = minfuncs.check_git_config()
        assert gituser == "a_value"
        assert gitemail == "a_value"

        # If missing credentials, fall back to vars
        empty_run = Mock(**{"stdout": b"", "returncode": 1})
        sp_with_successful_execution.run.return_value = empty_run
        gituser, gitemail = minfuncs.check_git_config()
        assert gituser == "user"
        assert gitemail == "user@mail.com"


def get_mocked_docker():
    container = Mock(
        **{
            "logs.return_value": [b"success"],
            "wait.return_value": {"StatusCode": False},
        }
    )
    image = Mock(
        **{
            "tags": ["an_image:latest"],
        }
    )
    client = Mock(
        **{
            "containers": Mock(
                **{
                    "run.return_value": container,
                    "list.return_value": [container],
                }
            ),
            "images": Mock(**{"list.return_value": [image]}),
        }
    )
    mocked_docker = Mock(**{"from_env.return_value": client})
    return mocked_docker, client, container


def test_run_containerized(monkeypatch):
    mocked_docker, client, _ = get_mocked_docker()
    monkeypatch.setattr(ce, "docker", mocked_docker)
    monkeypatch.setattr(ce, "get_docker_image", Mock())
    monkeypatch.setenv("GIT_AUTHOR_NAME", "user")
    monkeypatch.setenv("GIT_AUTHOR_EMAIL", "user@mail.com")
    # Calling with coverage=True should result in --coverage being in the
    # docker run call
    ce.run_containerized(
        TESTS_DIR,
        **{
            "image_name": "afni/afni_cmake_build",
            "only_use_local": True,
            "coverage": True,
        },
    )
    run_calls = client.containers.run.call_args_list
    assert "--coverage" in run_calls[0][0][1]


@pytest.mark.skipif(
    minfuncs.is_containerized(),
    reason=("This test is not run inside the container."),
)
@pytest.mark.skipif(
    not DOCKER_AVAILABLE,
    reason=("Failed to find a running docker service."),
)
def test_run_containerized_fails_with_unknown_image():
    # The image needs to exist locally with only_use_local
    with pytest.raises(ValueError):
        ce.run_containerized(
            TESTS_DIR,
            **{
                "image_name": "unknown_image",
                "only_use_local": True,
            },
        )


@pytest.mark.parametrize(
    "help_option",
    [
        *"-h --help -help examples --installation-help".split(),
    ],
)
def test_run_tests_help_works(mocked_script, monkeypatch, help_option):
    """
    Various calls of run_afni_tests.py should have no dependencies to
    correctly execute: basically help can be displayed without dependencies
    installed
    """
    # help should work even if pythonpath is set
    monkeypatch.setenv("PYTHONPATH", "a_path")
    not_expected = "datalad docker pytest afnipy run_tests".split()
    expected = ""
    argslist = [help_option]

    # Write run_afni_tests.py to an executable/importable path
    mocked_script.write_text(SCRIPT.read_text())
    # ./README.md needs to exist
    (mocked_script.parent / "README.md").write_text("some content")
    run_script_and_check_imports(
        mocked_script, argslist, expected, not_expected, monkeypatch
    )


def test_installation_help_from_anywhere(mocked_script, monkeypatch):
    """
    Various calls of run_afni_tests.py should have no dependencies to
    correctly execute: basically help can be displayed without dependencies
    installed
    """
    # Write run_afni_tests.py to an executable/importable path
    mocked_script.write_text(SCRIPT.read_text())
    # Run from any directory other than tests
    os.chdir(tempfile.mkdtemp())

    with monkeypatch.context() as m:
        m.setattr(sys, "argv", ["script_path", "--installation-help"])

        script_imported = importlib.import_module(mocked_script.stem)
        with pytest.raises(FileNotFoundError):
            run_main_func(script_imported, sys_exit=True)

        (mocked_script.parent / "README.md").write_text("some content")
        run_main_func(script_imported, sys_exit=True)


@pytest.mark.parametrize(
    "params",
    [
        {
            # until afni's typical installation includes an afnipy installed
            # into the python interpreter this should raise and error when no
            # additional args are passed and afnipy is installed
            "test_case": "no additional args",
            "argslist": ["local"],
            "expected": [""],
            "not_expected": ["local", "afnipy"],
        },
        {
            "test_case": "--abin should mean afnipy is not imported",
            "argslist": f"--abin {tempfile.mkdtemp()} local".split(),
            "expected": [""],
            "not_expected": ["afnipy", "docker"],
        },
        {
            "test_case": "--abin could be passed with equals",
            "argslist": f"--abin={tempfile.mkdtemp()} local".split(),
            "expected": [""],
            "not_expected": ["docker"],
        },
        {
            "test_case": "--build-dir",
            "argslist": f"--build-dir={tempfile.mkdtemp()} local".split(),
            "expected": [],
            "not_expected": ["docker"],
        },
    ],
)
def test_run_tests_local_subparsers_works(monkeypatch, params, mocked_script):
    """
    This runs some basic checks for the command line parsing of the
    run_afni_test.py tool. There is a bit of complicated stuff going on to
    make sure only the subparsing behavior is tested (as opposed to actually
    running the test suite each time!). Roughly speaking, this magic consists
    of mocking the run_tests function and writing the contents of
    run_afni_test.py to a test specific path that can be imported
    from/executed for each test parameter in an isolated manner.
    """
    monkeypatch.setattr(afni_test_utils.run_tests_func, "run_tests", RETCODE_0)
    # env check not needed
    monkeypatch.setattr(
        afni_test_utils.minimal_funcs_for_run_tests_cli,
        "modify_path_and_env_if_not_using_cmake",
        lambda *args, **kwargs: None,
    )

    # Write run_afni_tests.py to an executable/importable path
    mocked_script.write_text(SCRIPT.read_text())
    run_script_and_check_imports(
        mocked_script,
        params["argslist"],
        params["expected"],
        params["not_expected"],
        monkeypatch,
        sys_exit=False,
    )


def test_write_command_info():
    tests_data_dir = Path(tempfile.mkdtemp())
    outdir = tests_data_dir / "output_of_tests" / "output_2029_11_07"
    logfile = outdir / "testname" / "file.log"
    logfile.parent.mkdir(parents=True)

    tools.write_command_info(
        logfile,
        {
            "outdir": outdir,
            "tests_data_dir": tests_data_dir,
        },
    )


@pytest.mark.parametrize(
    "params",
    [
        {
            "test_case": "no change",
            "expected": "some output text without paths",
        },
        {
            "test_case": "abs paths removed",
            "rootdirs": ["/a/base/path"],
            "outdirs": ["/a/base/path/afni_ci_test_data/sample_test_output/dir"],
            "txt": "/a/base/path/afni_ci_test_data/sample_test_output/dir other text",
            "expected": "afni_ci_test_data/sample_test_output/dir other text",
        },
        {
            "test_case": "outdir is normalized",
            "rootdirs": ["/a/base/path"],
            "outdirs": [
                "output_of_tests/output_2020_11_12_174542/testing_script_functionality/test_rewrite_paths_in_logs"
            ],
            "txt": "output: output_of_tests/output_2020_11_12_174542/testing_script_functionality/test_rewrite_paths_in_logs/sample.txt",
            "expected": "output: afni_ci_test_data/sample_test_output/testing_script_functionality/test_rewrite_paths_in_logs/sample.txt",
        },
    ],
)
def test_rewrite_paths_for_line(data, params):
    txt = params.get("txt") or "some output text without paths"

    modified_line = tools.rewrite_paths_for_line(
        txt,
        params.get("rootdir") or ["/a/base/path"],
        params.get("outdirs") or ["/a/base/path/afni_ci_test_data/sample_test_output"],
        params.get("replacements_dict") or {},
    )
    assert modified_line == params["expected"]


def test_rewrite_paths_for_line_error(data, monkeypatch):
    """
    If the the stdout or stderr stream  output directories returned from get_command_info_dicts
    """
    cmd_info = defaultdict(lambda: "")
    cmd_info["outdir"] = "/a/base/path/output_of_tests/output_2020_11_12_154136"
    cmd_info["workdir"] = "/a/base/path"
    cmd_info["tests_data_dir"] = "/a/base/path"
    monkeypatch.setattr(
        tools,
        "get_command_info_dicts",
        Mock(return_value=[cmd_info, defaultdict(lambda: "")]),
    )
    txt = ["/a/base/path/output_of_tests/output_2020_11_12_154142/subdir other text"]
    # should fail when output directory in log has wrong timestamp
    with pytest.raises(ValueError):
        modified_line = tools.rewrite_paths_for_cleaner_diffs(
            data,
            [txt],
        )

    # should fail when outdirs are not in tests_dir
    cmd_info[
        "outdir"
    ] = "/a/different/base/path/output_of_tests/output_2020_11_12_154136"
    with pytest.raises(ValueError):
        modified_line = tools.rewrite_paths_for_cleaner_diffs(
            data,
            [txt],
        )


def test_parser_with_relative_test_module(monkeypatch):
    """
    --file can exist relative to tests dir
    """
    # Write run_afni_tests.py to an executable/importable path
    tdir = Path(tempfile.mkdtemp())
    rel_file_path = "scripts/fictitious_module.py"
    (tdir / "scripts").mkdir()
    (tdir / rel_file_path).touch()
    # set "argv" so that --file only exists relative to the tests_dir
    monkeypatch.setattr(
        minfuncs.sys, "argv", f"scriptname --file={rel_file_path} local".split()
    )
    minfuncs.parse_user_args(tests_dir=tdir)


@pytest.mark.parametrize(
    "argslist",
    [
        "container --source-mode=host".split(),
        ["container"],
    ],
)
def test_run_tests_container_subparsers_works(monkeypatch, argslist, mocked_script):
    """
    This runs some basic checks for the command line parsing of the
    run_afni_test.py tool when the container option is used. There is a bit of
    complicated stuff going on to make sure only the subparsing behavior is
    tested (as opposed to actually running the test suite each time!). Roughly
    speaking, this magic consists of mocking the run_containerized function
    and writing the contents of run_afni_test.py to a test specific path that
    can be imported from/executed for each test parameter in an isolated
    manner.
    """
    mocked_run_containerized = RETCODE_0
    monkeypatch.setattr(
        afni_test_utils.container_execution,
        "run_containerized",
        mocked_run_containerized,
    )
    not_expected = "datalad docker pytest afnipy run_tests".split()
    expected = ""

    # Write run_afni_tests.py to an executable/importable path
    mocked_script.write_text(SCRIPT.read_text())

    run_script_and_check_imports(
        mocked_script, argslist, expected, not_expected, monkeypatch, sys_exit=False
    )


@pytest.mark.parametrize(
    "params",
    [
        {
            "test_case": "default",
            "args_in": {"verbosity": "normal"},
            "expected_call_template": "{sys.executable} -m pytest {DEFAULT_ARGS}",
        },
        {
            "test_case": "with_coverage",
            "args_in": {
                "verbosity": "normal",
                "coverage": True,
                "build_dir": tempfile.mkdtemp(),
            },
            "expected_call_template": (
                "cd {params['args_in']['build_dir']};"
                "cmake -GNinja {TESTS_DIR.parent};"
                "ARGS='{DEFAULT_ARGS} {PYTEST_COV_FLAGS}' "
                "ninja pytest;"
                " gcovr -s --xml -o {TESTS_DIR}/gcovr_output.xml -r {params['args_in']['build_dir']}/src;"
                " bash -c 'bash <(curl -s https://codecov.io/bash)'"
            ),
        },
    ],
)
def test_run_tests_with_args(monkeypatch, params, sp_with_successful_execution):
    template = params["expected_call_template"]
    # All substituted variables should be defined in this scope
    expected_call = eval(f'f"""{template}"""')

    # Should not fail with missing credentials
    monkeypatch.setenv("GIT_AUTHOR_NAME", "user")
    monkeypatch.setenv("GIT_AUTHOR_EMAIL", "user@mail.com")

    # Create a mock so that subprocess.run calls return 0 exit status
    monkeypatch.setattr(
        afni_test_utils.run_tests_func,
        "subprocess",
        sp_with_successful_execution,
    )
    # mock os.environ so that race conditions do not occur during parallel testing
    monkeypatch.setattr(
        os,
        "environ",
        os.environ.copy(),
    )
    monkeypatch.setattr(
        afni_test_utils.run_tests_func,
        "check_test_data_repo",
        Mock(),
    )
    with pytest.raises(SystemExit) as err:
        afni_test_utils.run_tests_func.run_tests(TESTS_DIR, **params["args_in"])
        assert err.typename == "SystemExit"
        assert err.value.code == 0

    assert sp_with_successful_execution.run.call_args_list[0][0][0] == expected_call


def test_handling_of_binary_locations_and_afnipy_when_cmake_build_is_used(
    monkeypatch, mocked_abin
):
    """
    Testing situation 1. of modify_path_and_env_if_not_using_cmake: the cmake
    manages the details and masks any system state. afnipy should already be
    installed into the python interpretter when executing tests in this mode.
    All scripts/binaries are prepended to the path on the fly.

    Might want to consider having  a text file during installation that points
    to the appropariate interpreter and an accurate error of such a missing
    interpreter is raised? This would solve issues with the wrong environment
    being activated.
    """

    # create a mock import to control whether afnipy is "imported correctly" or not
    mocked_import = Mock()
    mocked_import.__file__ = "mocked_path_for_imported_module"
    # afnipy may or may not be importable in this situation, lets begin with
    # not importable
    mocked_import_module = MagicMock(
        side_effect=ImportError, return_value=mocked_import
    )
    with monkeypatch.context() as m:
        # Create mocks so that os.environ and importing simulate situation 1. and
        # can be set and modified safely
        m.setattr(run_tests_func.importlib, "import_module", mocked_import_module)

        with pytest.raises(EnvironmentError):
            # Run function to check no error is raised without afnipy
            minfuncs.modify_path_and_env_if_not_using_cmake(
                os.getcwd(),
                build_dir="a_directory",
            )

        mocked_import_module.side_effect = None
        # should work when afnipy is importable
        minfuncs.modify_path_and_env_if_not_using_cmake(
            os.getcwd(),
            build_dir="a_directory",
        )


def test_handling_of_binary_locations_and_afnipy_for_a_heirarchical_installation(
    monkeypatch, mocked_hierarchical_installation
):
    """
    Testing situation 2. of modify_path_and_env_if_not_using_cmake: cmake
    installation. AFNI binaries are on the PATH but shared object libraries
    are in ../lib and afnipy has been installed into the python interpreter.
    """
    # Get a directory and its bin dir that has the GNU installation pattern
    idir = mocked_hierarchical_installation
    fake_bin = str(idir / "bin")

    # create a mock import to control whether afnipy is "imported correctly"
    # or not
    mocked_import = Mock()
    mocked_import.__file__ = "mocked_path_for_imported_module"
    mocked_import_module = MagicMock(return_value=mocked_import)
    with monkeypatch.context() as m:
        # Create mocks so that os.environ and importing situation 2. and
        # can be set/modified safely
        m.setattr(run_tests_func.importlib, "import_module", mocked_import_module)
        filtered_path = minfuncs.filter_afni_from_path()
        m.setattr(
            os,
            "environ",
            {"PATH": f"{fake_bin}:{filtered_path}"},
        )
        m.setattr(sys, "path", sys.path.copy())
        # make 3dinfo available
        sys.path.insert(0, fake_bin)
        # make fake afnipy importable (via reload)
        sys.path.insert(0, str(idir / "fake_site_packages"))

        # With the current mocking "3dinfo" should be on the path and afnipy
        # should be importable
        assert sp.run("3dinfo")
        assert Path(shutil.which("3dinfo")).parent == Path(fake_bin)
        assert importlib.reload(afnipy)
        assert Path(afnipy.__file__).parent.parent.parent == idir

        # Run function to check that no error is raised spuriously
        minfuncs.modify_path_and_env_if_not_using_cmake(os.getcwd())


def test_handling_of_binary_locations_and_afnipy_for_default_run(
    monkeypatch, mocked_abin
):
    """
    Testing situation 3. of modify_path_and_env_if_not_using_cmake: typical
    abin installation. AFNI binaries are no the PATH. afnipy should not be
    importable until after function is executed.

    By default when the test suite is run, it is expected that abin is on the
    PATH (the standard flat directory structure of the make build distribution
    is being used). This means that python binaries should be in the same
    directory as c binaries and on the PATH. Python binaries in abin have no
    problem importing from afnipy since it sits in the same directory but
    python code outside of this will (including the test suite code). The
    python search path and os.environ are modified to remedy any issues caused
    by this for the test suite code. If afnipy is installed in this situation
    an error should be raised to prompt for its removal: this state is
    unsupported because it is ambiguous as to which version of python code
    will be used in all the various calling patterns that can occur
    (subprocess calls, subprocess calls to shell scripts that attempt to
    execute python binaries, imports of python modules etc.)
    """

    # create a mock import to control whether afnipy is "imported correctly"
    # or not
    mocked_import = Mock()
    mocked_import.__file__ = "mocked_path_for_imported_module"
    mocked_import_module = MagicMock(
        side_effect=ImportError, return_value=mocked_import
    )
    with monkeypatch.context() as m:
        # Create mocks so that os.environ and importing situation 3. and
        # can be set and set/modified safely
        m.setattr(run_tests_func.importlib, "import_module", mocked_import_module)

        filtered_path = minfuncs.filter_afni_from_path()
        m.setattr(
            os,
            "environ",
            {"PATH": f"{mocked_abin}:{filtered_path}"},
        )

        # With the current mocking "3dinfo" should be on the path,
        sp.run("3dinfo")

        # Run function to check that a setup for a testing session correctly
        # modifies the environment and sys.path
        minfuncs.modify_path_and_env_if_not_using_cmake(
            os.getcwd(),
        )

        # The current python interpreter should now be able to import afnipy
        # without issue (and it should be imported from the mocked abin)
        mocked_afnipy = importlib.reload(afnipy)
        assert Path(mocked_afnipy.__file__).parent.parent == mocked_abin

        # If import afnipy does not fail, an error should be raised
        mocked_import_module.side_effect = None
        with pytest.raises(EnvironmentError):
            minfuncs.modify_path_and_env_if_not_using_cmake(
                os.getcwd(),
            )


def test_handling_of_binary_locations_and_afnipy_when_abin_as_flag(
    monkeypatch, mocked_abin
):
    """
    Testing situation 4. of modify_path_and_env_if_not_using_cmake: flat
    directory installation but not necessarily on PATH (passed as a flag).
    afnipy should not be importable until after function is executed.
    """

    # create a mock import to control whether afnipy is "imported correctly" or not
    mocked_import = Mock()
    mocked_import.__file__ = "mocked_path_for_imported_module"
    mocked_import_module = MagicMock(
        side_effect=ImportError, return_value=mocked_import
    )
    with monkeypatch.context() as m:
        # Create mocks so that os.environ and importing simulate situation 4. and
        # can be set and modified safely
        m.setattr(run_tests_func.importlib, "import_module", mocked_import_module)

        filtered_path = minfuncs.filter_afni_from_path()
        m.setattr(
            os,
            "environ",
            {"PATH": f"{filtered_path}"},
        )

        # With the current mocking "3dinfo" should not be on the path. If this
        # does fail in finding 3dinfo you can add the afni installation
        # directory that is found on the path to 'abin_patterns' in
        # minfuncs.filter_afni_from_path.
        with pytest.raises(FileNotFoundError):
            sp.run("3dinfo")

        # Run function to check that a setup for a testing session correctly
        # modifies the environment and sys.path
        minfuncs.modify_path_and_env_if_not_using_cmake(
            os.getcwd(),
            abin=str(mocked_abin),
        )
        # The fake binary should now be able to executed with no error
        sp.run("3dinfo")

        # The current python interpreter should now be able to import afnipy
        # without issue (and it should be imported from the mocked abin)
        mocked_afnipy = importlib.reload(afnipy)
        assert Path(mocked_afnipy.__file__).parent.parent == mocked_abin

        # If the import of afnipy does not fail when running the function in
        # this context (abin flag is passed but afnipy is importable), an
        # error should be raised
        mocked_import_module.side_effect = None
        with pytest.raises(EnvironmentError):
            minfuncs.modify_path_and_env_if_not_using_cmake(
                os.getcwd(),
                abin=str(mocked_abin),
            )


def test_examples_parse_correctly(monkeypatch):
    # dir_path needs to be mocked to prevent errors being raise for
    # non-existent paths
    monkeypatch.setattr(
        afni_test_utils.minimal_funcs_for_run_tests_cli,
        "dir_path",
        lambda x: str(Path(x).expanduser()),
    )
    stdout_ = sys.stdout  # Keep track of the previous value.
    for name, example in run_tests_examples.examples.items():
        # Generate the 'sys.argv' for the example
        arg_list = shlex.split(example.splitlines()[-1])[1:]
        # Execute the script so that it can be run.
        res = runpy.run_path(str(SCRIPT))

        res["sys"].argv = [SCRIPT.name, *arg_list]
        res["main"].__globals__["run_tests"] = Mock(side_effect=SystemExit(0))
        res["main"].__globals__["run_containerized"] = Mock(side_effect=SystemExit(0))
        res["main"].__globals__[
            "minfuncs"
        ].modify_path_and_env_if_not_using_cmake = lambda *args, **kwargs: None
        with pytest.raises(SystemExit) as err:
            # Run main function while redirecting to /dev/null
            sys.stdout = open(os.devnull, "w")
            res["main"]()
            sys.stdout = stdout_  # restore the previous stdout.

        assert err.typename == "SystemExit"
        assert err.value.code == 0

        if "local" in arg_list:
            res["main"].__globals__["run_tests"].assert_called_once()
        elif "container" in arg_list:
            res["main"].__globals__["run_containerized"].assert_called_once()

    sys.stdout = stdout_  # restore the previous stdout.


@pytest.mark.parametrize(
    "params",
    [
        # basic usage
        {"image_name": "an_image"},
        # test-data-volume is a valid value for source_mode
        {
            "image_name": "an_image",
            "only_use_local": False,
            "source_mode": "test-data-volume",
        },
        # Build dir outside of source should work
        {
            "image_name": "an_image",
            "only_use_local": False,
            "source_mode": "host",
            "build_dir": tempfile.mkdtemp(),
        },
    ],
)
def test_check_user_container_args(params):
    ce.check_user_container_args(TESTS_DIR, **params)


@pytest.mark.parametrize(
    "params",
    [
        # Value error should be raised if build is in source
        {
            "image_name": "an_image",
            "only_use_local": False,
            "source_mode": "host",
            "build_dir": str(TESTS_DIR),
        },
        # reuse-build conflicts with build-dir because reuse implies container build dir
        {
            "image_name": "an_image",
            "only_use_local": False,
            "source_mode": "host",
            "build_dir": tempfile.mkdtemp(),
            "reuse_build": True,
        },
        # test-code mounting conflicts with build-dir and reuse-build because test-code implies
        # using installed version of afni
        {
            "image_name": "an_image",
            "only_use_local": False,
            "source_mode": "test-code",
            "build_dir": tempfile.mkdtemp(),
        },
        # image needs to exist
        {
            "image_name": "an_image",
            "only_use_local": False,
            "source_mode": "test-code",
            "reuse_build": True,
        },
    ],
)
def test_check_user_container_args_failures(params):
    with pytest.raises(ValueError):
        ce.check_user_container_args(TESTS_DIR, **params)


def test_check_user_container_args_with_root(monkeypatch):
    # this contains a bit of a hack because I couldn't figure out how to patch
    # minfuncs.os on a test specific basis

    with monkeypatch.context() as m:

        m.setattr(os, "getuid", Mock(return_value=0))

        with pytest.raises(ValueError):
            ce.check_user_container_args(
                TESTS_DIR,
                image_name="afni/afni_cmake_build",
                only_use_local=False,
                source_mode="host",
            )
        with pytest.raises(ValueError):
            ce.check_user_container_args(
                TESTS_DIR,
                image_name="afni/afni_cmake_build",
                only_use_local=False,
                build_dir=tempfile.mkdtemp(),
            )


def test_get_test_cmd_args():
    cmd_args = minfuncs.get_test_cmd_args(overwrite_args="")
    assert not cmd_args

    # Check default commands
    cmd_args = minfuncs.get_test_cmd_args()
    assert cmd_args == DEFAULT_ARGS.split()

    cmd_args = minfuncs.get_test_cmd_args(verbosity="traceback")
    assert "--showlocals" in cmd_args


def test_configure_parallelism_parallel(monkeypatch):
    # define a mock sp.check_output for testing purposes
    def mocked_output(*args, **kwargs):
        """
        local function to help pretend pytest is run with pytest-parallel
        available (regardless of its installation status)
        """
        if any("pytest" in a for a in args):
            return bytes("pytest-parallel", "utf-8")
        else:
            proc = sp.run(*args, **kwargs, stdout=sp.PIPE)
            if proc.returncode:
                raise ValueError(
                    "This command should not have failed. This is testing something else."
                )
            return proc.stdout

    # When the user has requested to run the tests in parallel, the --workers
    # option is used for the pytest call and omp var set to 1
    monkeypatch.setattr(minfuncs.sp, "check_output", mocked_output)
    with monkeypatch.context() as m:
        m.setattr(os, "environ", {})
        assert not os.environ.get("OMP_NUM_THREADS")
        # check use_all_cores works with current plugin
        cmd_args = minfuncs.configure_parallelism([], use_all_cores=True)
        flag_in_args = "-n" in cmd_args
        assert flag_in_args
        assert os.environ["OMP_NUM_THREADS"] == "1"


def test_configure_parallelism_serial(monkeypatch):
    # for serial testing workers option not passed and OMP var set
    with monkeypatch.context() as m:
        m.setattr(os, "environ", {})
        assert not os.environ.get("OMP_NUM_THREADS")
        cmd_args = minfuncs.configure_parallelism([], use_all_cores=False)
        assert "-n" not in cmd_args
        assert os.environ.get("OMP_NUM_THREADS")


def test_configure_for_coverage(monkeypatch):
    cmd_args = ["scripts"]

    with monkeypatch.context() as m:
        m.setattr(os, "environ", os.environ.copy())
        # Coverage should fail without a build directory
        with pytest.raises(ValueError):
            out_args = minfuncs.configure_for_coverage(
                TESTS_DIR, cmd_args, coverage=True
            )

        if "CFLAGS" in os.environ:
            del os.environ["CFLAGS"]
        if "LDFLAGS" in os.environ:
            del os.environ["LDFLAGS"]
        if "CXXFLAGS" in os.environ:
            del os.environ["CXXFLAGS"]

        # Check vars are not inappropriately set when not requested
        out_args = minfuncs.configure_for_coverage(
            TESTS_DIR, cmd_args, coverage=False, build_dir="something"
        )
        assert not any([os.environ.get(x) for x in "CFLAGS CXXFLAGS LDFLAGS".split()])

        # Check coverage flags are added when requested
        out_args = minfuncs.configure_for_coverage(
            TESTS_DIR, cmd_args, coverage=True, build_dir="something"
        )
        assert all(x in out_args for x in PYTEST_COV_FLAGS.split())
        assert (
            os.environ.get("CXXFLAGS")
            == "-g -O0 -Wall -W -Wshadow -Wunused-variable -Wunused-parameter -Wunused-function -Wunused -Wno-system-headers -Wno-deprecated -Woverloaded-virtual -Wwrite-strings -fprofile-arcs -ftest-coverage"
        )
        assert (
            os.environ.get("CFLAGS") == "-g -O0 -Wall -W -fprofile-arcs -ftest-coverage"
        )
        assert os.environ.get("LDFLAGS") == "-fprofile-arcs -ftest-coverage"


def test_generate_cmake_command_as_required():
    adict = {"build_dir": tempfile.mkdtemp()}
    output = minfuncs.generate_cmake_command_as_required(TESTS_DIR, adict)
    assert "cmake -GNinja" in output


def test_unparse_args_for_container():
    user_args = {}
    expected = """ local"""
    converted = ce.unparse_args_for_container(TESTS_DIR, **user_args)
    assert converted == expected

    user_args = {
        "build_dir": "/saved/afni/build",
        "debug": True,
        "extra_args": None,
        "ignore_dirty_data": False,
        "image_name": "afni/afni_cmake_build",
        "source_mode": "host",
        "only_use_local": True,
        "use_all_cores": False,
        "coverage": True,
        "verbose": False,
    }
    expected = """ --build-dir=/opt/afni/build --debug --coverage local"""
    converted = ce.unparse_args_for_container(TESTS_DIR, **user_args)
    assert converted == expected

    user_args = {
        "debug": False,
        "extra_args": "-k hello --trace",
        "use_all_cores": False,
        "coverage": True,
        "verbose": False,
    }
    expected = """ --extra-args="-k hello --trace" --coverage local"""
    converted = ce.unparse_args_for_container(TESTS_DIR, **user_args)
    assert converted == expected

    # underscores should be converted for any kwargs passed through (when
    # their value is True at least)
    user_args = {"arbitrary_kwarg_with_underscores": True}

    converted = ce.unparse_args_for_container(TESTS_DIR, **user_args)
    assert "--arbitrary-kwarg-with-underscores local" in converted

    # --reuse-build should
    user_args = {"reuse_build": True}

    converted = ce.unparse_args_for_container(TESTS_DIR, **user_args)
    assert "--build-dir=/opt/afni/build" in converted


def test_setup_test_data_container(monkeypatch):
    """
    Check that when source mode is test-data-volume that setup_test_data_vol is called
    """
    mocked_docker, client, _ = get_mocked_docker()

    with monkeypatch.context() as m:
        mocked_test_data_setup = Mock()
        m.setattr(
            afni_test_utils.container_execution,
            "setup_test_data_vol",
            mocked_test_data_setup,
        )
        m.setattr(os, "getuid", Mock(return_value=2000))
        m.setattr(os, "getgid", Mock(return_value=2000))
        ce.setup_docker_env_and_volumes(
            client,
            TESTS_DIR,
            **{"source_mode": "test-data-volume"},
        )
        mocked_test_data_setup.assert_called_once()

    with monkeypatch.context() as m:
        m.setattr(os, "getuid", Mock(return_value=0))
        m.setattr(os, "getgid", Mock(return_value=0))
        mocked_test_data_setup = Mock()
        m.setattr(
            afni_test_utils.container_execution,
            "setup_test_data_vol",
            mocked_test_data_setup,
        )

        ce.setup_docker_env_and_volumes(
            client,
            TESTS_DIR,
            **{"source_mode": "test-data-volume"},
        )
        mocked_test_data_setup.assert_called_once()


def test_setup_test_data_vol():
    # check default user is the dev image id
    mocked_docker, client, _ = get_mocked_docker()
    ce.setup_test_data_vol(client, {}, {}, tempfile.mkdtemp(), "/mnt")
    init_cmd = client.api.create_container.call_args_list[0][0][1][2]
    assert "-u 1000 -g 100" in init_cmd

    # Check that the id is changed by changing docker_kwargs
    mocked_docker, client, _ = get_mocked_docker()
    ce.setup_test_data_vol(
        client,
        {},
        {
            "CONTAINER_UID": 1007,
            "CONTAINER_GID": 3000,
        },
        tempfile.mkdtemp(),
        "/mnt",
    )
    init_cmd = client.api.create_container.call_args_list[0][0][1][2]
    assert "-u 1007 -g 3000" in init_cmd


def test_setup_docker_env_and_volumes(monkeypatch):
    # Should not fail with missing credentials
    monkeypatch.setenv("GIT_AUTHOR_NAME", "user")
    monkeypatch.setenv("GIT_AUTHOR_EMAIL", "user@mail.com")
    mocked_docker, client, _ = get_mocked_docker()

    # basic usage
    ce.setup_docker_env_and_volumes(
        client,
        TESTS_DIR,
    )

    # Confirm source directory is mounted
    source_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_volumes(
        client,
        TESTS_DIR,
        **{"source_mode": "host"},
    )
    assert docker_kwargs.get("volumes").get(source_dir)
    expected = "/opt/user_pip_packages,/opt/afni/build"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert docker_kwargs["environment"].get("CONTAINER_UID")

    # build should not be chowned if it is mounted
    source_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_volumes(
        client,
        TESTS_DIR,
        **{"source_mode": "host", "build_dir": "a_directory"},
    )
    assert docker_kwargs.get("volumes").get(source_dir)
    expected = "/opt/user_pip_packages"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert docker_kwargs["environment"].get("CONTAINER_UID")

    with monkeypatch.context() as m:
        m.setattr(os, "getuid", Mock(return_value=2000))

        # Confirm test-data volume is mounted when not root
        _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
        docker_kwargs = ce.setup_docker_env_and_volumes(
            client,
            TESTS_DIR,
            **{"source_mode": "test-data-volume"},
        )
        expected = ["test_data"]
        result = docker_kwargs.get("volumes_from")
        assert expected == result
        # test data is mounted using a volume and so can't be chowned within
        # the test container. Build is chowned in case coverage testing is
        # being performed in which case --reuse-build will have to be used
        expected = "/opt/afni/install,/opt/user_pip_packages,/opt/afni/build"
        assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
        assert docker_kwargs["environment"].get("CONTAINER_UID")

    # Confirm test-data volume is mounted correctly when root
    with monkeypatch.context() as m:
        m.setattr(os, "getuid", Mock(return_value=0))
        _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
        docker_kwargs = ce.setup_docker_env_and_volumes(
            client,
            TESTS_DIR,
            **{"source_mode": "test-data-volume"},
        )
        expected = ["test_data"]
        result = docker_kwargs.get("volumes_from")
        assert expected == result

        # when root, the only permissions altered are the test-data to that
        # of the container
        assert not docker_kwargs.get("environment").get("CHOWN_EXTRA")
        assert not docker_kwargs["environment"].get("CONTAINER_UID")

    # Confirm tests directory is mounted and file permissions is set correctly
    _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_volumes(
        client,
        TESTS_DIR,
        **{"source_mode": "test-code"},
    )
    assert docker_kwargs.get("volumes").get(str(TESTS_DIR))
    expected = "/opt/afni/install,/opt/user_pip_packages"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert docker_kwargs["environment"].get("CONTAINER_UID")

    # Confirm build directory is mounted
    _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_volumes(
        client,
        TESTS_DIR,
        **{"build_dir": data_dir},
    )
    assert docker_kwargs.get("volumes").get(data_dir)
    assert not docker_kwargs["environment"].get("CONTAINER_UID")


def test_check_if_cmake_configure_required():
    # empty dir should work and require configure
    build_dir = Path(tempfile.mkdtemp())
    result = minfuncs.check_if_cmake_configure_required(build_dir)
    assert True == result

    # this should work. No actual pre-existing build but its assessed by a
    # cached file and the existence of build.ninja so fine for our purposes
    # here.
    ninja_dir = build_dir / "build.ninja"
    ninja_dir.mkdir()
    cache_file = build_dir / "CMakeCache.txt"
    cache_file.write_text(f"For build in directory: {build_dir}")
    result = minfuncs.check_if_cmake_configure_required(build_dir)
    assert False == result

    # this should work, weird path but is actually same directory
    unresolved_build_dir = build_dir.parent.joinpath(
        f"../{build_dir.parent.name}/{build_dir.name}"
    )
    result = minfuncs.check_if_cmake_configure_required(unresolved_build_dir)
    assert False == result

    # there is a cache but missing build.ninja so needs to be configured
    ninja_dir.rmdir()
    result = minfuncs.check_if_cmake_configure_required(build_dir)
    assert True == result

    # this should pass, missing dir but will be in container
    cache_file.write_text("For build in directory: /opt/afni/build")
    minfuncs.check_if_cmake_configure_required(build_dir, within_container=True)


def test_build_dir_not_writeable_error():
    build_dir = Path(tempfile.mkdtemp(), "build_dir")
    build_dir.mkdir()

    # make sure that a directory that is not writeable raises the appropriate
    # error
    for i in range(1, 7):
        try:
            build_dir.chmod(i * 100)
            (build_dir / f"test_{i}").touch()
            print(f"writeable for {i * 100}")
            minfuncs.check_if_cmake_configure_required(build_dir)
        except PermissionError:
            print(f"not writeable for {i * 100}")
            with pytest.raises(PermissionError):
                minfuncs.check_if_cmake_configure_required(build_dir)


def test_wrong_build_dir_raise_file_not_found(monkeypatch):
    build_dir = "/opt/afni/build"
    mocked_path_instance = Mock()
    mocked_path_instance.exists.return_value = False
    mocked_path = Mock()
    mocked_path.return_value = mocked_path_instance

    # mock non existent build dir
    monkeypatch.setattr(
        afni_test_utils.minimal_funcs_for_run_tests_cli, "Path", mocked_path
    )
    # mock local execution
    monkeypatch.setattr(
        afni_test_utils.minimal_funcs_for_run_tests_cli,
        "is_containerized",
        lambda x: False,
    )

    # this should fail, as /opt/build/afni is mocked to not exist, simulating
    # a local execution for which /opt/afni/build is specificed as the build
    # dir
    with pytest.raises(NotADirectoryError):
        afni_test_utils.minimal_funcs_for_run_tests_cli.check_if_cmake_configure_required(
            build_dir
        )
