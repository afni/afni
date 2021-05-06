import datalad.api as datalad
import importlib
import logging
import os
import subprocess
import sys

from afni_test_utils.minimal_funcs_for_run_tests_cli import (
    check_git_config,
    configure_for_coverage,
    configure_parallelism,
    generate_cmake_command_as_required,
    get_test_cmd_args,
)

# Other imports to fail early with missing dependencies
importlib.import_module("xvfbwrapper")


logger = logging.getLogger("afni_test_utils")


def check_test_data_repo(test_data, ignore_dirty_data):

    if (test_data.pathobj / "test_data_version.txt").exists():
        # For circleci Make sure test data is checked out correctly
        test_data.repo.update_submodule(test_data.path)
    else:
        if test_data.repo.dirty and not ignore_dirty_data:
            logger.info("checking if test data repo is dirty")
            raise ValueError(
                "The test data is in a dirty state. You should commit any changes, clean the repository, or run tests with the ignore-dirty-data flag"
            )


def run_tests(tests_dir, **args_dict):

    check_git_config()
    test_data = datalad.Dataset(str(tests_dir / "afni_ci_test_data"))
    if test_data.repo:
        check_test_data_repo(
            test_data, ignore_dirty_data=args_dict.get("ignore_dirty_data")
        )

    cmd_args = get_test_cmd_args(**args_dict)
    cmd_args = configure_parallelism(cmd_args, args_dict.get("use_all_cores"))
    cmd_args = configure_for_coverage(tests_dir, cmd_args, **args_dict)
    if args_dict.get("build_dir"):
        cmd = generate_cmake_command_as_required(tests_dir, args_dict)
        cmd += f""";ARGS='{' '.join(x for x in cmd_args)}' ninja pytest"""
    else:
        cmd = f"""{sys.executable} -m pytest {' '.join(x for x in cmd_args)}"""

    if args_dict.get("coverage"):
        # append gcovr to assemble coverage report for C code
        cmd += f"; gcovr -s --xml -o {tests_dir}/gcovr_output.xml -r {args_dict['build_dir']}/src"
        # append command for compiling and uploading codecov report

        # apparently there is a security issue here, must investigate
        # cmd += "; bash -c 'bash <(curl -s https://codecov.io/bash)'"
        sys.exit(1)

    print(f"Executing: {cmd}")
    res = subprocess.run(cmd, shell=True, env=os.environ.copy())
    sys.exit(res.returncode)
