import subprocess
import argparse
import os
import sys
from pathlib import Path
from itertools import compress
import subprocess as sp

PYTEST_GROUP_HELP = "pytest execution modifiers"
PYTEST_MANUAL_HELP = (
    "Manual pytest management (conflicts with pytest execution modifiers)"
)
VALID_MOUNT_MODES = "host test-code test-data-only".split()


def parse_user_args():
    parser = argparse.ArgumentParser(
        description=""" run_afni_tests.py is a wrapper script to help run
        tests for the AFNI suite of tools. This wrapping is an attempt to
        reduce the burden of executing tests and to facilitate the various
        usage patterns. Such usage patterns include: running the tests using
        dependencies installed on the local host; using a container to
        encapsulate most build/testing dependencies; making use of the cmake
        build system to make the iterative process of changing code and
        running tests easier; running the tests while making use of all the
        cores on the computer; subsetting the tests that are executed during a
        test run""",
        add_help=False,
    )
    parser.add_argument(
        "--help",
        "-help",
        "-h",
        dest="help",
        action="store_true",
        help="show this help message and exit",
    )

    dir_for_build_type = parser.add_mutually_exclusive_group()
    dir_for_build_type.add_argument(
        "--build-dir",
        metavar="DIR",
        type=dir_path,
        help=(
            "This can be an empty directory. Use the 'pytest' target "
            "from the cmake build system at this location. Requires "
            "ninja. This is convenient because it enables within-build-"
            "tree-testing so you don't have to install and you don't "
            "accidentally test the wrong programs due to incorrect PATH "
            "etc "
        ),
    )
    dir_for_build_type.add_argument(
        "--abin",
        metavar="DIR",
        type=dir_path,
        help=(
            "Provide the path to the installation directory of AFNI "
            "produced by the make build system "
        ),
    )

    # Options that affect threading
    thread_management = parser.add_mutually_exclusive_group()
    thread_management.add_argument(
        "--debug",
        action="store_true",
        dest="debug",
        help="Do not catch exceptions and show exception traceback (Drop into pdb debugger).",
    )
    thread_management.add_argument(
        "--use-all-cores",
        action="store_true",
        dest="use_all_cores",
        help="Make use of all cpus for tests (requires pytest-parallel).",
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
        metavar="PYTEST_ARGS",
        help=(
            "This should be a quoted string that is passed directly "
            "through to pytest. e.g. --extra-args='-k gui --trace'. "
            "Passing --help through to pytest will give you a sense "
            "of all the possibilities... "
        ),
    )
    pytest_mod.add_argument(
        "--verbose",
        "-v",
        action="count",
        default=0,
        help="Increase the verbosity of reporting (conflicts with --debug).",
    )
    pytest_mod.add_argument(
        "--log-file-level",
        choices="DEBUG INFO WARNING ERROR CRITICAL".split(),
        help="Set the verbosity of the output recorded in the log file.",
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
        type=dir_path,
    )
    pytest_mod.add_argument(
        "--lf",
        help=("Only run tests that failed on the last test run."),
        action="store_true",
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
        dest="subparser", title="Sub Commands", description="One is required",
    )
    # CLI group for running tests in a container
    container = subparsers.add_parser(
        "container",
        prog="run_afni_tests.py [general options] container",
        help="Running tests in an AFNI development container requires a "
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
            "'test-data-only', should rarely be needed, but only mounts the "
            "tests/afni_ci_test_data sub-directory into the container. "
            "Mounting directories into the container has implications "
            "for file permissions. If this option is not used, no "
            "issues occurs as all files are owned by the user in the "
            "container. If 'host' is chosen, AFNI's source code is "
            "mounted into the container. The source directory is owned "
            "by the host user, so the container user is changed to this "
            "id and the build and home directory are appropriately "
            "chowned. If 'test-data-only' is used, privileged permissions "
            "will be required to transfer ownership of the test data "
            "back to the host user. "
        ),
    )
    container.add_argument(
        "--image-name",
        help="Image used for testing container. Default is likely something like afni/afni_cmake_build",
    )
    container.add_argument(
        "--intermediate",
        action="store_true",
        help=(
            "Search intermediate image layers for the image-name. This  can sometimes "
            "fail if layers have been pruned etc."
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
            "dependencies. Consider something like the following "
            "command 'conda env create -f environment.yml;conda "
            "activate afni_dev' "
        ),
    )
    examples = subparsers.add_parser("examples", help=("Show usage examples"),)
    examples.add_argument(
        "--verbose",
        action="store_true",
        help="Include a verbose explanation along with the examples",
    )

    args = parser.parse_args()
    if args.help:
        parser.print_help()
        print("SUB COMMAND: container")
        container.print_help()
        print("SUB COMMAND: examples")
        examples.print_help()
        sys.exit(0)
    if not args.subparser:
        sys.exit(
            ValueError(
                f"You must specify a subcommand (one of {list(subparsers.choices.keys())})"
            )
        )

    # verbosity breaks exception hook so check not used together
    if args.debug and args.verbose:
        raise ValueError("If you wish to use debugging, turn off verbosity.")

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

    return args


def check_build_directory(build_dir, within_container=False):
    if build_dir:
        if not within_container:
            if not Path(build_dir).exists():
                raise NotADirectoryError
        # check the ownership
        diruser = Path(build_dir).stat().st_uid
        is_local_gid = diruser == os.getuid()
        dirgroup = Path(build_dir).stat().st_gid
        is_local_uid = dirgroup == os.getgid()
        if not (is_local_uid or is_local_gid):
            raise ValueError(
                "The build directory is assumed to be "
                "user writeable. The directory itself has ownership."
                f"{diruser}:{dirgroup}"
            )

        # check info about possible previous build
        cache_file = Path(build_dir, "CMakeCache.txt")
        if not os.listdir(build_dir):
            # empty directory
            return
        if not cache_file.exists():
            raise ValueError(
                "The build appears to have contents but not that of a "
                "previously successful build "
            )

        cache_info = cache_file.read_text()[:500].splitlines()
        mismatch_err = (
            "It appears that you are trying to use a build directory "
            "that was created in a different context ({build_dir} vs "
            "{cache_path}). Consider using an empty directory instead. "
        )
        cache_path_entry = [
            line for line in cache_info if "For build in directory:" in line
        ][0]
        cache_path = Path(cache_path_entry.split(":")[1].strip())

        # within container is used because this check needs to happen outside
        # the container for a directory that needs to work within the
        # container
        if within_container:
            if "For build in directory: /opt/afni/build" not in str(cache_info):
                raise ValueError(
                    mismatch_err.format(
                        build_dir="/opt/afni/build", cache_path=cache_path
                    )
                )

        else:
            if not cache_path.exists():
                raise FileNotFoundError(
                    f"Could not find {cache_path},the directory reported "
                    "to be the location of the previous cmake build "
                )

            if not Path(build_dir).samefile(cache_path):
                raise ValueError(
                    mismatch_err.format(build_dir=build_dir, cache_path=cache_path,)
                )


def get_dependency_requirements():

    if len(sys.argv) == 1 or any(x in sys.argv for x in ["-h", "-help", "--help"]):
        return "minimal"

    subparser_patterns = ["container", "examples"]
    if any(x in sys.argv for x in subparser_patterns):
        # Not sure if this is the sub parser so need to parse the
        # arguments to check
        parsed = parse_user_args()
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
            gituser = sp.check_output(
                "git config --global user.name 'AFNI CircleCI User'", shell=True,
            ).decode("utf-8")
        if not gitemail:
            gitemail = sp.check_output(
                "git config --global user.email 'johnleenimh+circlecigitconfig@gmail.com'",
                shell=True,
            ).decode("utf-8")
    return gituser, gitemail


def is_containerized():
    env_file = Path("/.dockerenv")
    cgroup_info = Path("/proc/1/cgroup")
    if env_file.exists() or cgroup_info.exists():
        return True
    # if env_file_exists or any(pat in cgroup_info for pat in ['lxb','docker']):
    else:
        return False


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
        # this requires pytest-parallel to work
        cmd_args += f" --workers {NCPUS}".split()
        os.environ["OMP_NUM_THREADS"] = "1"
    else:
        os.environ["OMP_NUM_THREADS"] = NCPUS

    return cmd_args


def add_coverage_args(cmd_args):
    cov_options = "--cov=targets_built --cov-report xml:$PWD/coverage.xml".split()
    cmd_args += cov_options
    return cmd_args


def get_container_dir():
    return Path("/opt/afni/src/tests")


def configure_for_coverage(cmd_args, **kwargs):
    out_args = cmd_args.copy()
    if kwargs.get("coverage"):
        # This will run correctly if the build has performed using the
        # appropriate build flags to enable coverage.
        if not (kwargs.get("build_dir") or kwargs.get("reuse_build")):
            raise ValueError(
                "If you want to test coverage, use the --build-dir option or the container subcommand with  --reuse-build."
            )

        # check that the pytest-cov plugin is installed
        res = sp.run("pytest --help".split(), stdout=sp.PIPE, stderr=sp.STDOUT)
        if "coverage reporting" not in res.stdout.decode("utf-8"):
            raise EnvironmentError("It seems pytest is missing the pytest-cov plugin.")

        out_args = add_coverage_args(out_args)
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

    if not kwargs.get("verbose"):
        verb_args = "--tb=no --no-summary --show-capture=no"
    elif kwargs.get("verbose") == 1:
        verb_args = "--tb=no --no-summary -v --log-cli-level=INFO"
    elif kwargs.get("verbose") == 2:
        verb_args = "--tb=short -r Exs -v --log-cli-level=INFO"
    elif kwargs.get("verbose") == 3:
        verb_args = "--tb=auto -r Exs -v --log-cli-level=INFO --showlocals  -s"
    elif kwargs.get("verbose") > 3:
        verb_args = "--tb=long -r Exs -v --log-cli-level=DEBUG --showlocals -s"

    cmd_args += verb_args.split()

    if kwargs.get("debug"):
        cmd_args.append("--pdb")

    if kwargs.get("filter_expr"):
        cmd_args.append(f"-k={kwargs['filter_expr']}")

    if kwargs.get("log_file_level"):
        cmd_args.append(f"--log-file-level={kwargs['log_file_level']}")

    if kwargs.get("lf"):
        cmd_args.append("--lf")

    cmd_args += (kwargs.get("extra_args") or "").split()
    return cmd_args


def generate_cmake_command_as_required(tests_dir, args_dict):
    """
    When a build dir has been defined, check whether is empty or sensibly
    populated. Return a command for both situations
    """
    check_build_directory(args_dict["build_dir"])
    cmd = f"cd {args_dict['build_dir']}"
    if not os.listdir(args_dict["build_dir"]):
        cmd += f";cmake -GNinja {tests_dir.parent}"
    return cmd
