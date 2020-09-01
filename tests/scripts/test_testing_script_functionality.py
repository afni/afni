from argparse import Namespace
from collections import namedtuple
from pathlib import Path
from unittest.mock import MagicMock, patch, Mock
import importlib
import os
import pytest
import runpy
import shlex
import shutil
import subprocess
import subprocess as sp
import sys
import tempfile
import io

# import afni_test_utils as atu
from afni_test_utils import run_tests_func
from afni_test_utils import run_tests_examples
from afni_test_utils import container_execution as ce
from afni_test_utils import minimal_funcs_for_run_tests_cli as minfuncs

# import the whole package for mocking purposes
import afni_test_utils

TESTS_DIR = Path(__file__).parent.parent
SCRIPT = TESTS_DIR.joinpath("run_afni_tests.py")
# The default args to pytest will likely change with updates
DEFAULT_ARGS = "scripts --tb=no --no-summary --show-capture=no"
PYTEST_COV_FLAGS = "--cov=targets_built --cov-report xml:$PWD/coverage.xml"


RETCODE_0 = MagicMock()
RETCODE_0.returncode = 0
from contextlib import redirect_stdout
import contextlib

# @contextlib.contextmanager
# def stdout_redirected(new_stdout):
#     save_stdout = sys.stdout
#     sys.stdout = new_stdout
#     try:
#         yield None
#     finally:
#         sys.stdout = save_stdout


@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.is_containerized")
@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.sp")
def test_check_git_config(mocked_sp, mocked_containerized):
    mocked_run_output = MagicMock()
    mocked_run_output.stdout = b""
    mocked_sp.run.return_value = mocked_run_output
    mocked_sp.check_output.return_value = b"a_value"
    mocked_containerized.return_value = False

    # if credentials are unset should raise error
    with patch.object(os, "environ", {}):
        with pytest.raises(EnvironmentError):
            minfuncs.check_git_config()

    # env variables should be sufficient
    with patch.object(
        os, "environ", {"GIT_AUTHOR_NAME": "user", "GIT_AUTHOR_EMAIL": "user@mail.com"}
    ):
        minfuncs.check_git_config()

    # credentials should be sufficient
    with patch.object(os, "environ", {}):
        # simulate existing credentials
        mocked_run_output.stdout = b"a_value"
        minfuncs.check_git_config()


@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.is_containerized")
@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.sp")
def test_check_git_config_containerized(mocked_sp, mocked_containerized):
    mocked_run_output = MagicMock()
    mocked_sp.run.return_value = mocked_run_output
    mocked_sp.check_output.return_value = b"a_value"
    mocked_containerized.return_value = True
    # if containerized local setting might still return credentials

    # if containerized env vars should still work unless they were not sent in

    # if containerized if all else fails set a default

    # if credentials are unset should be fine if containerized
    with patch.object(os, "environ", {}):
        mocked_run_output.stdout = b""
        minfuncs.check_git_config()

    # env variables should set the value if not set on system
    with patch.object(
        os, "environ", {"GIT_AUTHOR_NAME": "user", "GIT_AUTHOR_EMAIL": "user@mail.com"}
    ):
        mocked_run_output.stdout = b""
        gituser, gitemail = minfuncs.check_git_config()
        assert gituser == "user"
        assert gitemail == "user@mail.com"

        # credentials should be sufficient, and overwrite env vars (and they
        # can leak into container via local config in mounted source)
        mocked_run_output.stdout = b"a_value"
        gituser, gitemail = minfuncs.check_git_config()
        assert gituser == "a_value"
        assert gitemail == "a_value"


def run_main_func(script, sys_exit):
    if sys_exit:
        with pytest.raises(SystemExit) as err:
            script.main()
        assert err.typename == "SystemExit"
        assert err.value.code == 0
    else:
        script.main()


def run_script_and_check_imports(
    SCRIPT, argslist, expected, not_expected, sys_exit=True, no_output=True
):
    """
    This is very hacky. It is testing something hacky though. The overall goal
    is to try to only import dependencies of the core testing script as
    needed. In order for that behavior to be robust it must be tested here.

    I tried runpy as a way to execute the script but it didn't work (it was
    modifying sys.argv in a way I could not decipher).

    So the solution here is to temporarily modify sys.path/PATH and mock
    sys.argv  so that the run_afni_tests.py script is importable and the
    appropriate driving "user args" are defined during the import.
    Subsequently we check if the appropriate imports have occurred.
    """

    _environ = dict(os.environ)  # or os.environ.copy()
    syspath = sys.path.copy()
    sysmodules = sys.modules.copy()
    modname = SCRIPT.stem
    try:
        sys.path.insert(0, str(SCRIPT.parent))
        os.environ["PATH"] = f"{SCRIPT.parent}:{os.environ['PATH']}"
        with patch.object(sys, "argv", [SCRIPT.name, *argslist]) as mocked_argv:
            if SCRIPT.stem in sys.modules:
                script = importlib.reload(sys.modules[SCRIPT.stem])
            else:
                script = importlib.import_module(SCRIPT.stem)

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

    finally:
        os.environ.clear()
        os.environ.update(_environ)
        sys.path = syspath
        sys.modules = sysmodules


@patch("afni_test_utils.container_execution.get_docker_image")
@patch("afni_test_utils.container_execution.docker")
def test_run_containerized(mocked_docker, mocked_image_fetch):
    container = Mock()
    container.logs.return_value = [b"success"]
    client = Mock()
    client.containers.run.return_value = container
    mocked_docker.from_env.return_value = client
    # Calling with coverage=True should result in --coverage being in the
    # docker run call
    afni_test_utils.container_execution.run_containerized(
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
    minfuncs.is_containerized(), reason=("This test is not run inside the container."),
)
def test_run_containerized_fails_with_unknown_image():
    # The image needs to exist locally with only_use_local
    with pytest.raises(ValueError):
        ce.run_containerized(
            TESTS_DIR, **{"image_name": "unknown_image", "only_use_local": True,}
        )


@pytest.mark.parametrize("help_option", [*"-h --help -help examples".split(),])
def test_run_tests_help_works(help_option):
    """
    Various calls of run_afni_tests.py should have no dependencies to
    correctly execute: basically help can be displayed without dependencies
    installed
    """
    not_expected = "datalad docker pytest afnipy run_tests".split()
    expected = ""
    argslist = [help_option]
    run_script_and_check_imports(SCRIPT, argslist, expected, not_expected)


@pytest.mark.parametrize(
    "params",
    [
        {
            "test_case": "no additional args",
            "argslist": ["local"],
            "expected": ["afnipy"],
            "not_expected": ["local"],
        },
        {
            "test_case": "--abin should mean afnipy is not imported",
            "argslist": f"--abin {tempfile.mkdtemp()} local".split(),
            "expected": [""],
            "not_expected": ["afnipy", "docker"],
        },
    ],
)
@patch("afni_test_utils.run_tests_func.run_tests")
def test_run_tests_local_subparsers_works(mocked_run_tests, params):
    # TODO parametrize the expected/not expected for more precision
    not_expected = "docker".split()
    expected = ""
    run_script_and_check_imports(
        SCRIPT,
        params["argslist"],
        params["expected"],
        params["not_expected"],
        sys_exit=False,
    )


@pytest.mark.parametrize(
    "argslist", ["container --source-mode=host".split(), ["container"],]
)
@patch("afni_test_utils.container_execution.run_containerized")
def test_run_tests_container_subparsers_works(mocked_run_containerize, argslist):
    mocked_run_containerized = RETCODE_0
    not_expected = "datalad docker pytest afnipy run_tests".split()
    expected = ""
    run_script_and_check_imports(
        SCRIPT, argslist, expected, not_expected, sys_exit=False
    )


@pytest.mark.parametrize(
    "params",
    [
        {
            "test_case": "default",
            "args_in": {},
            "expected_call_template": "pytest {DEFAULT_ARGS}",
        },
        {
            "test_case": "with_coverage",
            "args_in": {"coverage": True, "build_dir": tempfile.mkdtemp()},
            "expected_call_template": (
                "cd {params['args_in']['build_dir']};"
                "cmake -GNinja {TESTS_DIR.parent};"
                "ARGS='{DEFAULT_ARGS} {PYTEST_COV_FLAGS}' "
                "ninja pytest"
            ),
        },
    ],
)
@patch("afni_test_utils.run_tests_func.subprocess")
def test_run_tests(mocked_sp, params):
    template = params["expected_call_template"]
    # All substituted variables should be defined in this scope
    expected_call = eval(f'f"""{template}"""')

    mocked_sp.run.return_value = RETCODE_0
    with pytest.raises(SystemExit) as err:
        run_tests_func.run_tests(TESTS_DIR, **params["args_in"])
    assert err.typename == "SystemExit"
    assert err.value.code == 0
    mocked_sp.run.assert_called_with(expected_call, shell=True)


@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.dir_path")
def test_examples_parse_correctly(mocked_dirpath):

    # dir_path needs to be mocked to prevent errors being raise for
    # non-existent paths
    mocked_dirpath.side_effect = lambda x: str(Path(x).expanduser())
    stdout_ = sys.stdout  # Keep track of the previous value.
    for name, example in run_tests_examples.examples.items():
        # Generate the 'sys.argv' for the example
        arg_list = shlex.split(example.splitlines()[-1])[1:]

        # Execute the script so that it can be run.
        res = runpy.run_path(str(SCRIPT))

        res["sys"].argv = [SCRIPT.name, *arg_list]
        res["main"].__globals__["run_tests"] = MagicMock(side_effect=SystemExit(0))
        res["main"].__globals__["run_containerized"] = MagicMock(
            side_effect=SystemExit(0)
        )
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


def test_check_user_container_args():

    # basic usage
    ce.check_user_container_args(TESTS_DIR)

    # test-data-only is a valid value for source_mode
    ce.check_user_container_args(
        TESTS_DIR,
        image_name="an_image",
        only_use_local=False,
        source_mode="test-data-only",
    )

    # Build dir outside of source should work
    dir_outside_src = tempfile.mkdtemp()
    ce.check_user_container_args(
        TESTS_DIR,
        image_name="an_image",
        only_use_local=False,
        source_mode="host",
        build_dir=dir_outside_src,
    )
    # Value error should be raised if build is in source
    with pytest.raises(ValueError):
        ce.check_user_container_args(
            TESTS_DIR,
            image_name="an_image",
            only_use_local=False,
            source_mode="host",
            build_dir=str(TESTS_DIR),
        )

    # reuse-build conflicts with build-dir because reuse implies container build dir
    with pytest.raises(ValueError):
        ce.check_user_container_args(
            TESTS_DIR,
            image_name="an_image",
            only_use_local=False,
            source_mode="host",
            build_dir=dir_outside_src,
            reuse_build=True,
        )

    # test-code mounting conflicts with build-dir and reuse-build because test-code implies
    # using installed version of afni
    with pytest.raises(ValueError):
        ce.check_user_container_args(
            TESTS_DIR,
            image_name="an_image",
            only_use_local=False,
            source_mode="test-code",
            build_dir=dir_outside_src,
        )
    with pytest.raises(ValueError):
        ce.check_user_container_args(
            TESTS_DIR,
            image_name="an_image",
            only_use_local=False,
            source_mode="test-code",
            reuse_build=True,
        )


def test_check_user_container_args_with_root():
    # this contains a bit of a hack because I couldn't figure out how to patch
    # minfuncs.os on a test specific basis
    from os import getuid, listdir

    os.getuid = MagicMock(return_value="0")
    try:
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
    finally:
        os.getuid = getuid


def test_get_test_cmd_args():
    cmd_args = minfuncs.get_test_cmd_args(overwrite_args="")
    assert not cmd_args

    # Check default commands
    cmd_args = minfuncs.get_test_cmd_args()
    assert cmd_args == ["scripts", "--tb=no", "--no-summary", "--show-capture=no"]

    cmd_args = minfuncs.get_test_cmd_args(verbose=3)
    assert "--showlocals" in cmd_args


def test_configure_parallelism():
    _environ = dict(os.environ)  # or os.environ.copy()
    try:
        # check use_all_cores works
        if "OMP_NUM_THREADS" in os.environ:
            del os.environ["OMP_NUM_THREADS"]
        cmd_args = minfuncs.configure_parallelism([], use_all_cores=True)
        assert "--workers" in cmd_args
        assert os.environ["OMP_NUM_THREADS"] == "1"

        if "OMP_NUM_THREADS" in os.environ:
            del os.environ["OMP_NUM_THREADS"]
        cmd_args = minfuncs.configure_parallelism([], use_all_cores=False)
        assert os.environ.get("OMP_NUM_THREADS")

    finally:
        os.environ.clear()
        os.environ.update(_environ)


def test_configure_for_coverage():
    cmd_args = ["scripts"]
    # Coverage should fail without a build directory
    with pytest.raises(ValueError):
        out_args = minfuncs.configure_for_coverage(cmd_args, coverage=True)
    _environ = dict(os.environ)  # or os.environ.copy()
    try:

        if "CFLAGS" in os.environ:
            del os.environ["CFLAGS"]
        if "LDFLAGS" in os.environ:
            del os.environ["LDFLAGS"]
        if "CXXFLAGS" in os.environ:
            del os.environ["CXXFLAGS"]

        # Check coverage flags are added
        out_args = minfuncs.configure_for_coverage(
            cmd_args, coverage=True, build_dir="something"
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

        # Check vars are not inappropriately set
        if "CFLAGS" in os.environ:
            del os.environ["CFLAGS"]
        if "LDFLAGS" in os.environ:
            del os.environ["LDFLAGS"]
        if "CXXFLAGS" in os.environ:
            del os.environ["CXXFLAGS"]
        out_args = minfuncs.configure_for_coverage(
            cmd_args, coverage=False, build_dir="something"
        )
        assert not any([os.environ.get(x) for x in "CFLAGS CXXFLAGS LDFLAGS".split()])

    finally:
        os.environ.clear()
        os.environ.update(_environ)


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


def test_setup_docker_env_and_vol_settings():
    # basic usage
    ce.setup_docker_env_and_vol_settings(TESTS_DIR,)

    # Confirm source directory is mounted
    source_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_vol_settings(
        TESTS_DIR, **{"source_mode": "host"},
    )
    assert docker_kwargs.get("volumes").get(source_dir)
    expected = "/opt/user_pip_packages,/opt/afni/build"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert docker_kwargs["environment"].get("CONTAINER_UID")

    # build should not be chowned if it is mounted
    source_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_vol_settings(
        TESTS_DIR, **{"source_mode": "host", "build_dir": "a_directory"},
    )
    assert docker_kwargs.get("volumes").get(source_dir)
    expected = "/opt/user_pip_packages"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert docker_kwargs["environment"].get("CONTAINER_UID")

    # Confirm test-data directory is mounted
    _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_vol_settings(
        TESTS_DIR, **{"source_mode": "test-data-only"},
    )
    assert docker_kwargs.get("volumes").get(data_dir)
    expected = "/opt/afni/src/tests/afni_ci_test_data"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert not docker_kwargs["environment"].get("CONTAINER_UID")

    # Confirm tests directory is mounted and file permissions is set correctly
    _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_vol_settings(
        TESTS_DIR, **{"source_mode": "test-code"},
    )
    assert docker_kwargs.get("volumes").get(str(TESTS_DIR))
    expected = "/opt/afni/install,/opt/user_pip_packages"
    assert docker_kwargs.get("environment")["CHOWN_EXTRA"] == expected
    assert docker_kwargs["environment"].get("CONTAINER_UID")

    # Confirm build directory is mounted
    _, data_dir, *_ = ce.get_path_strs_for_mounting(TESTS_DIR)
    docker_kwargs = ce.setup_docker_env_and_vol_settings(
        TESTS_DIR, **{"build_dir": data_dir},
    )
    assert docker_kwargs.get("volumes").get(data_dir)
    assert not docker_kwargs["environment"].get("CONTAINER_UID")


def test_check_build_directory():
    build_dir = Path(tempfile.mkdtemp())
    tmpfile = build_dir / "a_file"
    cache_file = build_dir / "CMakeCache.txt"
    cache_file.write_text(f"For build in directory: {build_dir}")

    # this should work
    minfuncs.check_build_directory(build_dir)

    # this should work, weird path but is actually same directory
    unresolved_build_dir = build_dir.parent.joinpath(
        f"../{build_dir.parent.name}/{build_dir.name}"
    )
    minfuncs.check_build_directory(unresolved_build_dir)

    # this should fail, not empty but no cache
    with pytest.raises(ValueError):
        cache_file.unlink()
        tmpfile.touch()
        minfuncs.check_build_directory(build_dir)
    tmpfile.unlink()

    # this should pass, missing dir but will be in container
    cache_file.write_text("For build in directory: /opt/afni/build")
    minfuncs.check_build_directory(build_dir, within_container=True)

@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.is_containerized")
@patch("afni_test_utils.minimal_funcs_for_run_tests_cli.Path")
def test_wrong_build_dir_raise_file_not_found(mocked_path,mocked_containerized):
    build_dir = "/opt/afni/build"
    mocked_path_instance = Mock()
    mocked_path_instance.exists.return_value = False
    mocked_path.return_value = mocked_path_instance

    # this should fail, as /opt/build/afni is mocked to not exist, simulating
    # a local execution for which /opt/afni/build is specificed as the build
    # dir
    with pytest.raises(NotADirectoryError):
        afni_test_utils.minimal_funcs_for_run_tests_cli.check_build_directory(
            build_dir
        )
