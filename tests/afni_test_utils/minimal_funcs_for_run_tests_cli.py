import subprocess
import argparse
import functools
import importlib
import os
import sys
from pathlib import Path
from itertools import compress
import subprocess as sp
import shutil

from afni_test_utils.run_tests_examples import EXAMPLES

PYTEST_GROUP_HELP = "pytest execution modifiers"
PYTEST_MANUAL_HELP = (
    "Manual pytest management (conflicts with pytest execution modifiers)"
)
VALID_MOUNT_MODES = "host test-code test-data-only test-data-volume".split()
VALID_VERBOSITY_MODES = [
    "quiet",
    "normal",
    "summary",
    "verbose",
    "traceback",
    "diarrhetic",
]


def get_parser(tests_dir=None, return_subparsers=False):
    parser = argparse.ArgumentParser(
        description="""
        run_afni_tests.py is a wrapper script to help run tests for the AFNI
        suite of tools. This wrapping is an attempt to reduce the burden of
        executing tests and to facilitate the various usage patterns. Such
        usage patterns include: running the tests using dependencies installed
        on the local host; using a container to encapsulate most build/testing
        dependencies; making use of the cmake build system to make the
        iterative process of changing code and running tests easier; running
        the tests while making use of all the cores on the computer;
        subsetting the tests that are executed during a test run.""",
        add_help=False,
    )
    parser.add_argument(
        "--help",
        "-help",
        "-h",
        dest="help",
        action="store_true",
        help="show this help message and exit (-help is more verbose)",
    )
    parser.add_argument(
        "--installation-help",
        action="store_true",
        help="Show details regarding the setup for tests execution.",
    )

    dir_for_build_type = parser.add_mutually_exclusive_group()
    dir_for_build_type.add_argument(
        "--build-dir",
        "-b",
        metavar="DIR",
        type=dir_path,
        help=(
            "This is convenient because it enables testing within the "
            "build tree so you don't have to install the afni suite to "
            "test it and you don't accidentally test the wrong programs "
            "due to incorrect PATH etc. \n\nNote, in order to use this "
            "option you must have afnipy installed. "
        ),
    )
    dir_for_build_type.add_argument(
        "--abin",
        metavar="DIR",
        type=dir_path,
        help=(
            "Provide the path to the installation directory of AFNI "
            "produced by the make build system."
        ),
    )

    # Options that affect threading
    thread_management = parser.add_mutually_exclusive_group()
    thread_management.add_argument(
        "--debug",
        "-d",
        action="store_true",
        dest="debug",
        help=(
            "Do not catch exceptions and show exception traceback Drop "
            "into pdb debugger. As well as passing the --pdb flag to "
            "pytest this enables an exception hook for code executed "
            "prior to the execution of the pytest command "
        ),
    )
    thread_management.add_argument(
        "--use-all-cores",
        "-u",
        action="store_true",
        dest="use_all_cores",
        help="Make use of all cpus for tests (requires pytest-parallel).",
    )

    thread_management.add_argument(
        "--trace",
        help=("Immediately drop into the call stack (the pdb debugger) for each test."),
        action="store_true",
    )

    parser.add_argument(
        "--ignore-dirty-data",
        action="store_true",
        dest="ignore_dirty_data",
        help="Do not fail if the CI test data repository has uncommitted modifications",
    )
    parser.add_argument(
        "--coverage",
        action="store_true",
        help="Use codecov for test coverage (needs --build-dir).",
    )

    pytest_mod = parser.add_argument_group("pytest execution modifiers")
    pytest_mod.add_argument(
        "--extra-args",
        "-e",
        metavar="PYTEST_ARGS",
        help=(
            "This should be a quoted string that is passed directly "
            "through to pytest. e.g. --extra-args='-k gui --trace'. "
            "Passing --help through to pytest will give you a sense "
            "of all the possibilities... "
        ),
    )
    pytest_mod.add_argument(
        "--verbosity",
        "-v",
        choices=VALID_VERBOSITY_MODES,
        default="normal",
        help=(
            "Increase the verbosity of reporting. Levels other than "
            "quiet and normal conflict with --debug. "
        ),
    )
    pytest_mod.add_argument(
        "--log-file-level",
        choices="DEBUG INFO WARNING ERROR CRITICAL".split(),
        help="Set the verbosity of the output recorded in the log file. There is no log file by default.",
    )
    pytest_mod.add_argument(
        "--filter-expr",
        "-k",
        metavar="EXPR",
        help=(
            "Expression for pytest to use to filter tests. Equivalent to passing "
            "--extra-args='-k EXPR'. "
        ),
    )
    pytest_mod.add_argument(
        "--file",
        "-f",
        help=("Relative path to test module to run. e.g. scripts/test_3dcopy.py"),
        type=functools.partial(path_of_test_module, tests_dir=tests_dir),
    )
    pytest_mod.add_argument(
        "--lf",
        "-l",
        help=("Only run tests that failed on the last test run."),
        action="store_true",
    )

    pytest_mod.add_argument(
        "--runslow",
        help=("Run default tests and tests marked with 'slow'."),
        action="store_true",
    )

    pytest_mod.add_argument(
        "--runveryslow",
        help=("Run default tests and tests marked with 'slow' or 'veryslow'."),
        action="store_true",
    )

    pytest_mod.add_argument(
        "--runall",
        help=("Ignore all test markers and run everything."),
        action="store_true",
    )

    pytest_mod.add_argument(
        "--create-sample-output",
        "-c",
        help=(
            "Create sample output instead of running a diff with pre- "
            "existing output. This is a required step when initially "
            "writing a test. "
        ),
        action="store_true",
    )

    pytest_mod.add_argument(
        "--diff-with-sample",
        help=(
            "Provide a path of pre-existing output that you wish to "
            "compare against that is not the default data saved in the "
            "datalad repository afni_ci_test_data. "
        ),
    )

    pytest_mod.add_argument(
        "--marker-expression",
        "-m",
        metavar="EXPR",
        help=(
            "Provide an expression for filtering tests using markers. "
            "This should be quoted if more than one word. It is a "
            "boolean expression; 'A and B' would run all tests that are "
            "marked with the A AND B. If you wished to run both types "
            "of tests you would use 'A or B'. See the pytest help "
            "documentation for more information."
        ),
    )

    pytest_mod_manual = parser.add_argument_group(PYTEST_MANUAL_HELP)
    pytest_mod_manual.add_argument(
        "--overwrite-args",
        metavar="PYTEST_ARGS",
        help=(
            "This should be a quoted string that is passed directly "
            "through to pytest"
        ),
    )

    subparsers = parser.add_subparsers(
        dest="subparser",
        title="Sub Commands",
        description="One is required",
    )
    # CLI group for running tests in a container
    container = subparsers.add_parser(
        "container",
        prog="run_afni_tests.py [general options] container",
        help="Running tests in an AFNI development container. This requires a "
        "docker installation and the python docker package",
    )

    container.add_argument(
        "--source-mode",
        choices=VALID_MOUNT_MODES,
        help=(
            "This defines how much of the source code on the host is "
            "used when running tests in the container. 'host' mounts "
            "the whole repo from the local filesystem during the tests. "
            "'test-code' only includes the tests sub-directory but is "
            "useful as it allows a different version of the test tree "
            "to be used from code that is installed in the container. "
            "'test-data-only' and 'test-data-volume, should rarely be "
            "needed, but only mount the tests/afni_ci_test_data sub- "
            "directory into the container or volume provided by running "
            "docker container. Mounting directories into the container "
            "has implications for file permissions. If this option is "
            "not used, no issues occurs as all files are owned by the "
            "user in the container. If 'host' is chosen, AFNI's source "
            "code is mounted into the container. The source directory "
            "is owned by the host user, so the container user is "
            "changed to this id and the build and home directory are "
            "appropriately chowned. If 'test-data-only' is used, "
            "privileged permissions will be required to transfer "
            "ownership of the test data back to the host user."
        ),
    )
    container.add_argument(
        "--image-name",
        help="Image used for testing container. Default is likely something like afni/afni_cmake_build",
    )
    container.add_argument(
        "--container-name",
        help="Name used for testing container. Default is randomly generated.",
    )
    container.add_argument(
        "--no-rm",
        help="Do not delete containers after execution of the tests.",
        action="store_true",
    )
    container.add_argument(
        "--intermediate",
        action="store_true",
        help=(
            "Search intermediate image layers for the image-name. This  can sometimes "
            "fail if layers have been pruned etc. but can be used with untagged images."
        ),
    )
    container.add_argument(
        "--only-use-local",
        action="store_true",
        help="Raise error if image name does not exist locally.",
    )
    container.add_argument(
        "--reuse-build",
        action="store_true",
        help="Use build dir in container (conflicts with --build-dir).",
    )
    container.add_argument(
        "--do-not-forward-git-credentials",
        action="store_true",
        help=(
            "Prevent git name and email being passed into the "
            "container. Instead AFNI CIRCLECI user will be used for any "
            "commits. "
        ),
    )

    local = subparsers.add_parser(
        "local",
        help=(
            "Running tests on the host requires additional "
            "dependencies. See run_afni_tests.py --installation-help"
        ),
    )
    examples = subparsers.add_parser(
        "examples",
        help=("Show usage examples"),
    )
    examples.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Include a verbose explanation along with the examples",
    )
    if return_subparsers:
        return parser, subparsers, local, container, examples
    else:
        return parser


def parse_user_args(user_args=None, tests_dir=None):
    parser, subparsers, local, container, examples = get_parser(
        tests_dir=tests_dir, return_subparsers=True
    )
    args = parser.parse_args(user_args or sys.argv[1:])
    if args.help:
        parser.print_help()
        print("SUB COMMAND: container")
        container.print_help()
        print("SUB COMMAND: examples")
        examples.print_help()
        if "-help" in sys.argv:
            print(EXAMPLES)
        sys.exit(0)
    if args.installation_help:
        print((tests_dir / "README.rst").read_text())
        sys.exit(0)
    if not args.subparser:
        print(
            "Unless requesting help you must specify a subcommand "
            f"one of {list(subparsers.choices.keys())} "
        )
        sys.exit(2)

    # verbosity breaks exception hook so check not used together
    if args.debug and args.verbosity not in ["normal", "quiet"]:
        raise ValueError(
            "If you wish to use debugging, turn off verbosity set to "
            "normal or quiet. "
        )

    # Make sure manual option and pytest help options were not both used
    for group in parser._action_groups:
        if group.title == PYTEST_GROUP_HELP:
            defaults = [x.default for x in group._group_actions]
            pytest_vals = [getattr(args, a.dest, None) for a in group._group_actions]
            pytest_args_passed = defaults != pytest_vals
        elif group.title == PYTEST_MANUAL_HELP:
            pytest_manual_arg_passed = bool(getattr(args, group._group_actions[0].dest))

    if pytest_args_passed and pytest_manual_arg_passed:
        raise ValueError(
            "Use pytest execution modifiers or manual pytest management, not both"
        )

    # runslow and runveryslow options are shortcuts... more extensive
    # selection should use -m and -k options (marker and keyword expressions).
    # The latter pair cannont be combined with the shortcuts.
    if (args.runall or args.runslow or args.runveryslow) and (
        args.marker_expression or args.filter_expr
    ):
        print(
            "ERROR: Cannot use marker or keyword filtering with the "
            "--runslow, --runveryslow, --runall options. You can instead "
            "include the slow and veryslow markers in the expression "
            "passed to --marker-expression. e.g. run_afni_tests.py -m 'slow and "
            "combinations' "
        )
        sys.exit(1)

    return args


def get_cache_path(build_dir):
    # check info about possible previous build
    cache_filepath = Path(build_dir, "CMakeCache.txt")
    # The choice is accidentally filling directories with a cmake build or
    # raising an error. The problem with the latter is that a failed build
    # will trigger this error, which seems unnecessary and irritating. Going
    # with the former for now and commenting this out...
    # if not cache_filepath.exists():
    #     raise ValueError(
    #         "The build appears to have contents but not that of a "
    #         "previously successful build "
    #     )
    if not os.listdir(build_dir) or not cache_filepath.exists():
        # empty directory or at least one without a cmake build
        return None, None

    cache_info = cache_filepath.read_text()[:500].splitlines()
    cache_path_entry = [
        line for line in cache_info if "For build in directory:" in line
    ][0]
    cache_implied_dir = Path(cache_path_entry.split(":")[1].strip())
    return cache_filepath, cache_implied_dir


def check_if_cmake_configure_required(build_dir, within_container=False):
    if build_dir:
        if not within_container:
            if not Path(build_dir).exists():
                raise NotADirectoryError
        # check dir is writeable
        if not os.access(build_dir, os.W_OK | os.X_OK):
            raise PermissionError(
                f"The build directory {build_dir} is not writeable by "
                f"the current user, {os.getuid} "
            )

        cache_filepath, cache_implied_dir = get_cache_path(build_dir)
        if not cache_implied_dir:
            # It seems there is not a pre-existing build. Will try configure
            # cmake and build in build_dir...
            return True

        # Deal with a pre-existing is inappropriate for use:
        mismatch_err = (
            "It appears that you are trying to use a build directory "
            "that was created in a different context ({build_dir} vs "
            "{cache_implied_dir}). Consider using an empty directory instead. "
        )
        # within container is used because this check needs to happen outside
        # the container for a directory that needs to work within the
        # container
        if within_container:
            if "/opt/afni/build" != str(cache_implied_dir):
                raise ValueError(
                    mismatch_err.format(
                        build_dir="/opt/afni/build", cache_implied_dir=cache_implied_dir
                    )
                )

        else:
            if cache_implied_dir and not cache_implied_dir.exists():
                raise FileNotFoundError(
                    f"Could not find {cache_implied_dir}, the directory "
                    "reported to be the location of the previous cmake "
                    f"build defined by {cache_filepath} "
                )

            if not Path(build_dir).samefile(cache_implied_dir):
                raise ValueError(
                    mismatch_err.format(
                        build_dir=build_dir,
                        cache_path=cache_implied_dir,
                    )
                )

            if not (Path(build_dir) / "build.ninja").exists():
                return True

        return False


def get_dependency_requirements(tests_dir):
    """
    Different dependencies are required depending on how the run_afni_tests.py
    script is being used.

    Almost no requirements are used for displaying help/examples.

    Very few requirements are required for execution of tests in a docker container.

    'Full' has various dependencies described in the accompanying conda yml
    files. afnipy may or may not need to be installed dependending on the
    usage situation. This can be that afni has been installed into the current
    PATH (using the make or cmake build system), the cmake build is being used
    (by way of the pytest target), the --abin flag is being used, or if the
    --build-dir flag is being used.
    """

    if len(sys.argv) == 1 or any(
        x in sys.argv for x in ["-h", "-help", "--help", "--installation-help", "", " "]
    ):
        return "minimal"

    subparser_patterns = ["container", "examples"]
    if any(x in sys.argv for x in subparser_patterns):
        # Not sure if this is the sub parser so need to parse the
        # arguments to check
        parsed = parse_user_args(tests_dir=tests_dir)
        if parsed.subparser == "examples":
            return "minimal"
        if parsed.subparser == "container":
            return "container_execution"

    return "full"


def try_to_get_git_credentials():
    gituser = (
        sp.run("git config user.name".split(), stdout=sp.PIPE).stdout.decode().strip()
    )
    gituser = gituser or os.environ.get("GIT_AUTHOR_NAME")
    gitemail = (
        sp.run("git config user.email".split(), stdout=sp.PIPE).stdout.decode().strip()
    )
    gitemail = gitemail or os.environ.get("GIT_AUTHOR_EMAIL")
    return gituser, gitemail


def check_git_config():
    gituser, gitemail = try_to_get_git_credentials()
    gituser, gitemail = set_default_git_config_if_containerized(gituser, gitemail)

    # basic config for git for circleci, assuming everyone else has configured git...
    if not gituser:
        raise EnvironmentError("Need to set git user name")
    if not gitemail:
        raise EnvironmentError("Need to set git user email")
    return gituser, gitemail


def set_default_git_config_if_containerized(gituser, gitemail):
    if is_containerized():
        if not gituser:
            gituser = "AFNI CircleCI User"
            sp.check_output(
                f"git config --global user.name '{gituser}'",
                shell=True,
            )
        if not gitemail:
            gitemail = "johnleenimh+circlecigitconfig@gmail.com"
            sp.check_output(
                f"git config --global user.email '{gitemail}'",
                shell=True,
            )
    return gituser, gitemail


def is_containerized():
    env_file = Path("/.dockerenv")
    cgroup_info = Path("/proc/1/cgroup")
    if env_file.exists() or cgroup_info.exists():
        return True
    # if env_file_exists or any(pat in cgroup_info for pat in ['lxb','docker']):
    else:
        return False


def path_of_test_module(string, tests_dir=None):
    file_in = Path(string).expanduser()
    if file_in.exists():
        return str(file_in)
    elif tests_dir:
        relative_to_tests_dir = tests_dir / file_in
        if relative_to_tests_dir.exists():
            return str(relative_to_tests_dir)

    raise FileNotFoundError(f"{file_in}")


def dir_path(string):
    dir_in = Path(string).expanduser()
    if dir_in.exists():
        return str(dir_in)
    else:
        raise NotADirectoryError(string)


def make_dir_args_absolute(args_dict):
    for k in ["abin", "build_dir", "file"]:
        if k in args_dict:
            args_dict[k] = str(Path(args_dict[k]).expanduser().absolute())


def configure_parallelism(cmd_args, use_all_cores):
    # Configure testing parallelism
    NCPUS = sp.check_output("getconf _NPROCESSORS_ONLN".split()).decode().strip()
    if use_all_cores:
        os.environ["OMP_NUM_THREADS"] = "1"
        # pytest sometimes hangs for an unknown reason at higher worker
        # counts when using pytest-parallel
        cmd_args += f" -n {NCPUS}".split()
    else:
        # tests will be executed serially
        os.environ["OMP_NUM_THREADS"] = NCPUS

    return cmd_args


def add_coverage_args(tests_dir, cmd_args):
    cov_options = f"--cov=afnipy --cov-report xml:$PWD/coverage.xml".split()
    cmd_args += cov_options
    return cmd_args


def get_container_dir():
    return Path("/opt/afni/src/tests")


def configure_for_coverage(tests_dir, cmd_args, **kwargs):
    out_args = cmd_args.copy()
    if kwargs.get("coverage"):
        # This will run correctly if the build has performed using the
        # appropriate build flags to enable coverage.
        if not (kwargs.get("build_dir") or kwargs.get("reuse_build")):
            raise ValueError(
                "If you want to test coverage, use the --build-dir "
                "option and/or the container subcommand with --reuse-build. "
                "Running coverage testing with the make build is not "
                "supported "
            )

        # check that the pytest-cov plugin is installed... This causes
        # problems with the session cache (lock file is removed upon pytest
        # removal and the pytest cache didn't seem to provide process safe
        # setting of a variable.) res = sp.run("pytest --help".split(),
        # stdout=sp.PIPE, stderr=sp.STDOUT) if "coverage reporting" not in
        # res.stdout.decode("utf-8"): raise EnvironmentError( "It seems pytest
        # is missing the pytest-cov plugin used " "for python coverage. " )
        out_args = add_coverage_args(tests_dir, out_args)
        os.environ[
            "CXXFLAGS"
        ] = "-g -O0 -Wall -W -Wshadow -Wunused-variable -Wunused-parameter -Wunused-function -Wunused -Wno-system-headers -Wno-deprecated -Woverloaded-virtual -Wwrite-strings -fprofile-arcs -ftest-coverage"
        os.environ["CFLAGS"] = "-g -O0 -Wall -W -fprofile-arcs -ftest-coverage"
        os.environ["LDFLAGS"] = "-fprofile-arcs -ftest-coverage"

    return out_args


def get_test_cmd_args(**kwargs):
    if kwargs.get("overwrite_args") is not None:
        cmd_args = kwargs["overwrite_args"].split()
        return cmd_args

    if kwargs.get("file"):
        cmd_args = [kwargs["file"]]
    else:
        cmd_args = ["scripts"]

    if not kwargs.get("verbosity"):
        kwargs["verbosity"] = "normal"

    if kwargs["verbosity"] == "quiet":
        verb_args = "--tb=no -r fE --show-capture=no"
    elif kwargs.get("verbosity") == "normal":
        verb_args = "--tb=no  -r fEs --show-capture=no"
    elif kwargs.get("verbosity") == "summary":
        verb_args = "--tb=no -r a"
    elif kwargs.get("verbosity") == "verbose":
        verb_args = "--tb=no -r fEsxX -v --log-cli-level=INFO  -s"
    elif kwargs.get("verbosity") == "traceback":
        verb_args = "--tb=auto -r fEsxX --log-cli-level=INFO --showlocals -s"
    elif kwargs.get("verbosity") == "diarrhetic":
        verb_args = "--tb=long -r fEsxX -vv --log-cli-level=DEBUG --showlocals -s"

    cmd_args += verb_args.split()

    if kwargs.get("debug"):
        cmd_args.append("--pdb")

    if kwargs.get("filter_expr"):
        cmd_args.append(f"""-k='{kwargs["filter_expr"]}'""")

    if kwargs.get("log_file_level"):
        cmd_args.append(f"--log-file-level={kwargs['log_file_level']}")

    if kwargs.get("trace"):
        cmd_args.append("--trace")

    if kwargs.get("runslow"):
        cmd_args.append("--runslow")

    if kwargs.get("runveryslow"):
        cmd_args.append("--runveryslow")

    if kwargs.get("runall"):
        cmd_args.append("--runall")

    if kwargs.get("create_sample_output"):
        cmd_args.append("--create-sample-output")

    if kwargs.get("diff_with_sample"):
        cmd_args.append(f"--diff-with-sample={kwargs.get('diff_with_sample')}")

    if kwargs.get("marker_expression"):
        cmd_args.append(f"""-m='{kwargs.get("marker_expression")}'""")

    if kwargs.get("lf"):
        cmd_args.append("--lf")

    cmd_args += (kwargs.get("extra_args") or "").split()
    return cmd_args


def generate_cmake_command_as_required(tests_dir, args_dict):
    """
    When a build dir has been defined, check whether is empty or sensibly
    populated. Return a command for both situations
    """
    cmake_configure_needed = check_if_cmake_configure_required(
        args_dict["build_dir"],
    )
    cmd = f"cd {args_dict['build_dir']}"
    if cmake_configure_needed:
        cmd += f";cmake -GNinja {tests_dir.parent}"
    return cmd


def make_sure_afnipy_not_importable():
    try:
        # This should fail in situation 3 and 4
        afnipy_mod = importlib.import_module("afnipy")
    except ImportError:
        # this is what should happen
        return

    raise EnvironmentError(
        "afnipy is importable when it should not be. When "
        "testing the default distribution abin on the PATH or "
        "when using the abin flag, the afnipy package should "
        "not be importable... instead afni python binaries "
        "should import it from the local afnipy directory in "
        f"abin. An installed afnipy was found at: {afnipy_mod.__file__}"
    )


def modify_path_and_env_if_not_using_cmake(**args_dict):
    """
    This function does some path/environment modifications to deal with the
    different installation configurations that the tests might be run under.
    The following cases are supported:

    1) --build-dir: The cmake build is used in combination with the pytest
       target. The pytest target modifies PATH variable prior to executing
       tests so that all built binaries, and scripts in the source directory
       are on the path and discoverable and afnipy is installed as required.

    2) hierarchical installation: The standard gnu directory installation structure
       (binaries in bin which is on the PATH, c libraries are stored in ../lib
       etc.).

    3) Typical installation: abin, built by the make build system with a flat
       directory structure, is on the PATH.

    4) --abin flag used: Similar to 3. except that the "installation" directory
       has been passed explicitly.



    In situation 1. afnipy and all binaries will appropriately mask system
    versions of afnipy/binaries.

    In situation 2. afnipy should be importable using the current python
    interpreter, python binaries will be in the python interpreter's
    directories, other binaries should be on the PATH.

    In situation 3., and 4. afnipy (directory containing python modules/python
    package containing AFNI's python code) should be not be importable from
    outside the abin directory before execution of this function and should be
    after its execution.
    """
    test_bin_path = shutil.which("3dinfo")
    # Do a check for the 4 four supported test path/setup situations
    if "build_dir" in args_dict:
        # situation 1 (cmake build)...
        try:
            importlib.import_module("afnipy")
        except ImportError as err:
            print(err)
            raise EnvironmentError(
                "Usage of cmake build for testing was "
                "inferred from the usage of --build-dir. "
                "Currently afnipy is not importable... you should be install it "
                "into the current python interpreter. (something like pip install -e ../src/python_scripts)"
            )
        return
    elif test_bin_path:
        libmri_so_paths = list(Path(test_bin_path).parent.parent.glob("lib/libmri.*"))
        if libmri_so_paths:
            # situation 2 (hierarchical installation)...
            if not importlib.import_module("afnipy"):
                raise EnvironmentError(
                    "Hierarchical installation for testing was inferred "
                    f"from the presence of {libmri_so_paths} when the "
                    f"binary {test_bin_path} was found. Cannot import "
                    "afnipy. This should be installed into the current "
                    "python interpreter. "
                )

            return

    # Now just situation 3 and 4 remaining.
    make_sure_afnipy_not_importable()
    if not args_dict.get("abin") and not test_bin_path:
        raise EnvironmentError("Cannot find local AFNI binaries. ")
    abin = args_dict.get("abin") or str(Path(test_bin_path).parent)

    # Modify sys.path and os.environ. Makes afnipy importable
    sys.path.insert(0, abin)

    # Also needs to work for shell subprocesses when using the abin flag so
    # added it to PATH
    os.environ["PATH"] = f"{abin}:{os.environ['PATH']}"

    # In situation 4, binaries should be located in the user defined directory
    if args_dict.get("abin"):
        test_bin_path = Path(shutil.which("3dinfo"))
        if not test_bin_path.parent == Path(args_dict.get("abin")):
            raise EnvironmentError(
                f"With --abin set to {abin} the expection is that "
                f"binaries are found there. Instead {test_bin_path} is "
                "found after an attempt to set up the environment "
                "correctly for the tests. Perhaps this is not a binary directory?"
            )

    libmri_so_paths = list(Path(test_bin_path).parent.glob("libmri.*"))
    if not libmri_so_paths:
        raise (ValueError("This should not be reached. Not sure what has happened."))


def _filter_afni_from_path(path_var):
    # Get a PATH variable that should not have afni on it
    afni_path = shutil.which("afni", path=path_var)
    abin_dir = str(Path(afni_path).parent) if afni_path else "undefined_path_for_abin"
    abin_patterns = ["abin", "targets_built", abin_dir]
    filtered_path = ":".join(
        [x for x in path_var.split(":") if not any(pat in x for pat in abin_patterns)]
    )
    return filtered_path


def filter_afni_from_path():
    filtered_path = os.environ["PATH"]
    attempt_count = 0
    while shutil.which("afni", path=filtered_path):
        filtered_path = _filter_afni_from_path(filtered_path)
        attempt_count += 1
        if attempt_count > 9:
            raise ValueError(
                "An error has occurred trying to filter afni out of the "
                f"PATH. {shutil.which('afni',path=filtered_path)} "
                f"found in {filtered_path} "
            )

    return filtered_path
