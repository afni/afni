from itertools import combinations
from subprocess import PIPE, run
import importlib
import contextlib
import os
import pytest


def is_omp(toolname):

    OMP = "is compiled using OpenMP" in run(
        [toolname, "-help"], stdout=PIPE
    ).stdout.decode("utf-8")

    # for now skip all files that want use omp if that was not used in the build
    if not OMP:
        pytestmark = pytest.mark.skip("skipping entire module")
    return OMP


def try_to_import_afni_module(mod):
    """
    This function is no longer needed because afnipy is added to sys.path in
    the conftest.py

    Old description:
    Function returns an imported object from AFNI's python modules.
    Currently this is required because AFNI's python code is not installed. It
    is on the system path. When on the path, the .py files can be run as an
    executable from anywhere but not imported. It's use should be limited with
    a view to eventually eradicate its need.

    Args:
        mod (str): A module name to attempt to import from AFNI's installation directory

    Returns:
        module: A python module imported from AFNI's installation directory

    Raises:
        EnvironmentError: If AFNI is not installed this error is raised.
    """
    return importlib.import_module(mod)


@contextlib.contextmanager
def remember_cwd():
    curdir = os.getcwd()
    try:
        yield
    finally:
        os.chdir(curdir)


def verify_parameters_structure(params, option_key="extra_args"):
    """
    Used with get_param_combinations. Checks that the params arg is formatted correctly
    """
    if not all(len(param) == 2 for param in params):
        raise ValueError(
            "Each element of params must be length two. They "
            "contain a label and a dictionary containing an command "
            f"line option. Parameters provided were instead: {params} "
        )

    for param in params:
        option_keys = list(param[1].keys())
        if not all(k == option_key for k in option_keys):
            raise ValueError(
                "All keys should match. The key is used as a variable "
                "name to expand paremeters in the test function. Param with "
                f"error {param} "
            )


def get_combinations_labels(params, options_combined, option_key):
    labels_out = []
    # create lookup for labels (when you have a cmd flag)
    rev_lookup = {opt_dict[option_key]: label for label, opt_dict in params}

    for combination in options_combined:
        # get each label associated with each cmd flag
        labels = [rev_lookup[x] for x in combination]
        comb_labels = "_and_".join(labels)
        # create a label prefix for combination groups
        if len(combination) not in [1, len(params)]:
            comb_labels = comb_labels + " combined"

        # Append the label constructed for this cmd flag combination
        labels_out.append(comb_labels)
    return labels_out


def get_param_combinations(*params, r_list=None):
    """
    Given  parameters as args, returns a list of parameters combined.
    Args:
        params (list of tuples containing label and option dict): Each
        parameter takes the form:

            (label,{var_name:option})

        Where label is a tag to describe the option, var_name is a string that
        will be expanded in the command executed in the tests function, and the
        option is a command line option that can be passed to the AFNI tool that
        is being tested.

        r_list (list of ints): The 'r' parameter defines the group size of the
        combination function in itertools. By default group sizes of 1, n. and n-1
        are used. This behavior can be overwritten with an explicit list of group
        sizes to use.
    """
    option_key = "extra_args"
    # Set default r_list if none is provided.
    if not r_list:
        r_list = [1, len(params) - 1, len(params)]

    # Check that the input parameters follow the expected format (there should
    # only be a single key used for the options)
    verify_parameters_structure(params)

    # Sort the param tuples lexigraphically by their cmd flag option.
    params = sorted(params, key=lambda x: x[1][option_key])
    cmd_flags = [y[1][option_key] for y in params]

    output_list = []
    for group_size in r_list:

        flag_combinations = list(combinations(cmd_flags, group_size))

        labels = get_combinations_labels(params, flag_combinations, option_key)
        flag_combinations = [" ".join(x) for x in flag_combinations]

        # combine the cmd flag combinations with the computed labels
        params_combined = list(
            zip(labels, [{option_key: flags} for flags in flag_combinations])
        )

        if group_size not in [1, len(params)]:
            # Mark all combinations  with the combinations marker so that they
            # are only run when someone knows they exist and wants to run them
            # (except for groups when all or 1 are used).
            params_combined = [
                pytest.param(x[0], x[1], marks=pytest.mark.combinations)
                for x in params_combined
            ]

        # append to list of combinations
        output_list += params_combined
    return output_list
