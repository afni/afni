#!/usr/bin/env python3
# This script is convoluted in an attempt to:
# a) provide help/examples without dependencies
# b) correctly execute tests in a container with minimal dependencies outside
#    the container
# c) provide informative errors outside of these circumstances for beginners
#    to the python world
# If it fails in the above, refactoring the code to a more traditional pattern
# would be desirable.
import os
import sys
from pathlib import Path
import importlib

AFNIPY_ERR = """
Tried and failed to import from afnipy. To solve this you can:
A: install afnipy... something like:
    pip install -e afni/src/python_scripts
B: Make sure an installation of the AFNI suite of tools is present on your
PATH (if not using --build-dir)
C: Use the --abin flag to define the location of a build that was created by
the make build system.
"""

NEED_DEV_INSTALLATION = """

Unsupported usage... The parent directory of this script should be 'tests'.
Instead this script is located in {parent_dir}. Full installation of
afni-test-utils should not be performed (or run the script from the tests
subdirectory in the afni source repository). You can use a development
install. Reinstall in development mode with something like: pip install -e
afni/tests
"""
IMPORT_FAILURE_HELP = """

If you are having issues importing you may want to confirm you have installed
the required dependencies and activated the appropriate environment if you are
using a tool for managing isolated environements (see 'run_afni_test.py
--installation-help'). This will be something like... \nconda activate afni_dev
"""

# Check the installation mode
parent_dir = Path(__file__).parent.resolve()
if not parent_dir.name == "tests":
    raise EnvironmentError(NEED_DEV_INSTALLATION.format(parent_dir=parent_dir))

# Perform a minimal import
minfuncs = importlib.import_module(
    "afni_test_utils.minimal_funcs_for_run_tests_cli",
)

# Check that PYTHONPATH is not set
if os.environ.get("PYTHONPATH"):
    raise ValueError(
        "Using PYTHONPATH is not supported. Unset this and rely on the "
        "other mechanisms for importing afnipy let the errors guide "
        "you! . "
    )

from afni_test_utils.exceptionhook import setup_exceptionhook
from afni_test_utils.run_tests_examples import EXAMPLES, examples

# Make imports when the user is doing something other than requesting help
dep_reqs = minfuncs.get_dependency_requirements()
if dep_reqs != "minimal":
    # If using the cmake build afnipy needs to be installed
    if "--build-dir" in "".join(sys.argv):
        try:
            afnipy = importlib.import_module("afnipy")
        except ImportError as err:
            print(err)
            print(AFNIPY_ERR)
            raise sys.exit(1)
    try:
        if dep_reqs == "container_execution":
            from afni_test_utils.container_execution import run_containerized
        else:
            from afni_test_utils.run_tests_func import run_tests
    except ImportError as err:
        print(err)
        print(IMPORT_FAILURE_HELP)
        raise sys.exit(1)


def main(user_args=None):

    print(
        "Running tests... if you have not run them before the first time will"
        " take a while because the test data has to be downloaded"
    )
    # parse user args:
    if not user_args:
        user_args = minfuncs.parse_user_args()

    args_dict = {k: v for k, v in vars(user_args).items() if v is not None}

    minfuncs.make_dir_args_absolute(args_dict)

    # Everything should be run from within the tests directory of the afni
    # source repository
    tests_dir = Path(__file__).resolve().parent
    os.chdir(tests_dir)

    if args_dict.get("debug"):
        setup_exceptionhook()

    if args_dict["subparser"] == "container":
        # Execute the tests in a container
        run_containerized(tests_dir, **args_dict)
    elif args_dict["subparser"] == "examples":
        if args_dict.get("verbose"):
            print(EXAMPLES)
        else:
            print("\n".join(f"{k}:\n    {v}" for k, v in examples.items()))
        sys.exit(0)
    else:
        # Modify path and sys.path as required
        minfuncs.modify_path_and_env_if_not_using_cmake(tests_dir, **args_dict)
        # Execute the tests in the local environment
        run_tests(tests_dir, **args_dict)


if __name__ == "__main__":
    main()
