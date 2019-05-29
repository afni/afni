import filecmp
from pathlib import Path
import nibabel as nib  # type: ignore
import difflib
import subprocess
import shutil
import sys
import numpy as np
import socket
import getpass

try:
    import misc
except ImportError:
    from . import misc


import tempfile
import itertools as IT
import os
import pytest
import datalad.api as datalad

from numpy.testing import assert_allclose  # type: ignore

from typing import Dict, List, Any, Union
import re
import stat
import datetime as dt


def get_output_name():
    CURRENT_TIME = dt.datetime.strftime(dt.datetime.today(), "%Y_%m_%d_%H%M%S")
    return "output_" + CURRENT_TIME


def convert_to_sample_dir_path(output_dir):
    sampdir = Path(str(output_dir).replace("output_", "sample_output_"))
    return sampdir


def remove_w_perms(dirname):

    os.chmod(dirname, stat.S_IREAD + stat.S_IEXEC)
    for root, dirs, files in os.walk(dirname):
        for momo in dirs:
            os.chmod(Path(root, momo), stat.S_IREAD + stat.S_IEXEC)
        for file in files:
            os.chmod(Path(root, file), stat.S_IREAD)


def get_current_test_name():
    name_str = os.environ.get("PYTEST_CURRENT_TEST").split(":")[-1].split(" ")[0]
    return re.sub(r"[\[\]\(\)\*]", "_", name_str).strip("_")


def compute_expected_euclidean_distance_for_affine(affine):
    """
    Computes a metric of how far an affine matrix deviates from the identity
    matrix (no shear,rotation,translation). In order to do this it computes
    the expected value of the norm of the distance between a point X, drawn from
    a 3 dimensional distribution, the location of X after the affine has been
    applied:
    E || (AX + b ) - X || ^ 2

        where A and B are the linear map and translation components of the
        affine map.

    This can be reformulated as:
    tr((A - I)^2) + norm(b) ^ 2
    """
    return np.matrix.trace(affine[:3, :3] - np.eye(3)) + np.linalg.norm(affine[:3, 3])


def test_compute_expected_euclidean_distance_for_affine():
    identity = np.eye(4)
    assert 0 == compute_expected_euclidean_distance_for_affine(identity)
    assert 3 == compute_expected_euclidean_distance_for_affine(identity * 2)


def assert_affines_equal(affine_a, affine_b, distance_tolerance=5):
    """
    Checks that affine_a and affine_b transforms an image approximately
    equally
    """
    merged_affines = np.linalg.inv(affine_a) @ affine_b
    diff_effect = compute_expected_euclidean_distance_for_affine(merged_affines)
    return diff_effect


def uniquify(path, sep="_"):
    # SO: questions/13852700/python-create-file-but-if-name-exists-add-number
    def name_sequence():
        count = IT.count()
        next(count)
        next(count)
        yield ""
        while True:
            yield "{s}{n:d}".format(s=sep, n=next(count))

    orig = tempfile._name_sequence
    with tempfile._once_lock:
        tempfile._name_sequence = name_sequence()
        path = os.path.normpath(path)
        dirname, basename = os.path.split(path)
        filename, ext = os.path.splitext(basename)
        fd, filename = tempfile.mkstemp(dir=dirname, prefix=filename, suffix=ext)
        tempfile._name_sequence = orig
    return Path(filename)


def update_sample_output(
    data,
    create_sample_output=None,
    save_sample_output=None,
    file_list=None,
    files_with_diff=None,
):

    # Get the directory to be used for the sample output
    if create_sample_output:
        savedir = data.sampdir
        sync_files = file_list

    elif save_sample_output:
        savedir = data.comparison_dir
        # Create rsync pattern for all files that need to be synced
        sync_files = files_with_diff
    else:
        raise ValueError

    # The results directory
    outdir = data.outdir

    if not savedir.exists():
        os.makedirs(savedir, exist_ok=True)

    files_pattern = " ".join(
        ['-f"+ %s"' % Path(fname).relative_to(outdir) for fname in sync_files]
    )

    if not shutil.which("rsync"):
        raise EnvironmentError(
            "Updating sample output requires a working rsync "
            "installation, which cannot currently be found. "
        )
    cmd = """
            rsync
                -a
                --delete
                -f "+ */"
                {files_pattern}
                -f "- *"
                {outdir}/
                {savedir}/
            """
    cmd = " ".join(cmd.format(**locals()).split())
    proc = subprocess.check_call(cmd, shell=True, cwd=pytest.config.rootdir)


def run_cmd(
    data,
    cmd,
    add_env_vars={},
    merge_error_with_output=False,
    workdir=None,
    python_interpreter="python3",
):
    """Run the provided command and check it's output. In conjunction with the data
    fixture this function handles the test output logging in a consistent way.

    Args:
        data : An object created by the data fixture that is used in test
        functions.
        cmd (str): A string that requires execution and error checking.
        add_env_vars (dict):  Variable/value pairs with which to modify the
        environment for the executed command
        workdir (pathlib.Path or str): Working directory to execute the
        command. Should be the root directory for tests (default) unless
        otherwise required
        python_interpreter (str): If set to 'python2', the command will be
        executed using the python 2 interpretter. This only has relevance to
        commands that use a python executable and it currently only works on
        Linux. This argument will be removed once all code is ported to python3.

    Returns:
        subprocess.CompletedProcess: An object that among other useful
        attributes contains stdout, stderror of the executed command
    """

    # Set working directory for command execution if not set explicitly
    if not workdir:
        workdir = Path.cwd()

    # If requested merge stderr and stdout
    if merge_error_with_output:
        error = subprocess.STDOUT
    else:
        error = subprocess.PIPE

    # Define log file paths, make the appropriate directories and check that
    # the log files do not exist, otherwise (using uniquize) append a number:
    stdout_log = data.logdir / (data.test_name + "_stdout.log")
    os.makedirs(stdout_log.parent, exist_ok=True)
    stdout_log = uniquify(stdout_log)
    stderr_log = Path(str(stdout_log).replace("_stdout", "_stderr"))
    cmd_log = Path(str(stdout_log).replace("_stdout", "_cmd"))

    # If linux and interpreter is python2 alter env. This is a bit of a hack
    # but will allow python3 to run the test suite while allowing CI to check
    # the output of afni tools when they are run in python2. Test code will
    # not be back-ported to python 2 but once it is no longer supported we can
    # eliminate this hack, or figure out a cleaner way of doing it.
    if python_interpreter == "python2":
        if "linux" in sys.platform:
            add_env_vars["_"] = shutil.which("python2")
        else:
            pytest.skip("unsupported configuration")

    # If not defined, set number of threads used.
    if "OMP_NUM_THREADS" not in add_env_vars:
        add_env_vars["OMP_NUM_THREADS"] = "1"

    # Set environment variables for the command execution
    for k, v in add_env_vars.items():
        os.environ[k] = v

    # Make substitutions in the command string  to remove unnecessary absolute
    # paths:
    rel_data_path = os.path.relpath(data.base_outdir, workdir)
    cmd = cmd.replace(str(data.base_outdir), rel_data_path)
    cmd = cmd.replace(str(workdir), ".")

    # Print and execute the command and log output
    print(f"cd {workdir};{cmd}")
    proc = subprocess.run(
        cmd, shell=True, stdout=subprocess.PIPE, stderr=error, cwd=workdir
    )
    stdout_log.write_text(proc.stdout.decode("utf-8"))
    if proc.stderr:
        err_text = proc.stderr.decode("utf-8")
        stderr_log.write_text(err_text)
    # cmd execution report
    hostname = socket.gethostname()
    user = getpass.getuser()
    cmd_record = f"{hostname}: {user} executed in {workdir} \n{cmd}"
    cmd_log.write_text(cmd_record)
    # Raise error if there was a non-zero exit code.
    proc.check_returncode()
    return proc


class OutputDiffer:
    def __init__(
        self,
        data: Any,
        cmd,
        add_env_vars: Dict = {},
        merge_error_with_output: bool = False,
        workdir: Union[str or Path] = None,
        python_interpreter: str = "python3",
        ignore_file_patterns: List = [],
        text_file_patterns: List = [],
        kwargs_1d: Dict = {},
        kwargs_log: Dict = {},
        kwargs_text_files: Dict = {},
        kwargs_scans: Dict = {"header_kwargs": {}, "data_kwargs": {}},
        kwargs_byte: Dict = {},
        create_sample_output: bool = False,
        save_sample_output: bool = False,
        file_list: List = [],
    ):
        self._data = data

        # Tune command execution
        self.cmd = cmd
        self.add_env_vars = add_env_vars
        self.merge_error_with_output = merge_error_with_output
        self.workdir = workdir or Path.cwd()
        self.python_interpreter = python_interpreter
        self.executed = False

        # Tune output saving behavior
        self.create_sample_output = create_sample_output or pytest.config.getoption(
            "--create_sample_output"
        )
        self.save_sample_output = save_sample_output or pytest.config.getoption(
            "--save_sample_output"
        )

        # Tune the output comparison
        self.require_sample_output = (
            self.create_sample_output or self.save_sample_output
        )
        self._ignore_file_patterns = ignore_file_patterns
        self._comparison_dir = data.comparison_dir
        self._text_file_patterns = text_file_patterns
        self._kwargs_log = set_default_kwargs_log_as_required(kwargs_log)
        self._kwargs_1d = kwargs_1d
        self._kwargs_log = kwargs_log
        self._kwargs_text_files = kwargs_text_files
        self._kwargs_scans = kwargs_scans
        self._kwargs_byte = kwargs_byte
        # If empty this is overwritten when the command is executed
        self.file_list = file_list
        # If saving output data as future comparison this is modified:
        self.files_with_diff = {}

    def run(self):
        proc = self.run_cmd()
        self.assert_all_files_equal()
        if self.require_sample_output:
            self.__update_sample_output()

    def run_cmd(self):
        if self.executed:
            raise ValueError(
                "The differ object has already been run as defined by "
                "the 'executed' attribute. "
            )

        # Call run_cmd defined in module scope.
        proc = run_cmd(
            self.data,
            self.cmd,
            add_env_vars=self.add_env_vars,
            merge_error_with_output=self.merge_error_with_output,
            workdir=self.workdir,
            python_interpreter=self.python_interpreter,
        )

        self.get_file_list()
        self.executed = True
        return proc

    def get_file_list(self):
        if not self.file_list:
            self.file_list = [
                f
                for f in self.data.outdir.glob("**/*")
                if f.is_file()
                and not any(pat in str(f) for pat in self.ignore_file_patterns)
            ]
        self.file_list = [
            f for f in self.file_list if not Path(f).name.endswith("_cmd.log")
        ]

    def __update_sample_output(self):
        update_sample_output(
            self.data,
            create_sample_output=self.create_sample_output,
            save_sample_output=self.save_sample_output,
            file_list=self.file_list,
            files_with_diff=self.files_with_diff,
        )

    def assert_all_files_equal(self):
        """A convenience wrapping function to compare the output files of a test
        command. 1D files, log files, and files sppecified as text files are
        compared using custom assert logic. If nibabel can read other files as
        images their header data and numeric data is compared, otherwise the byte
        contents of the files are compared.

        Args:
            data: Fixture object used in tests

            ignore_file_patterns: List of substrings that if
            found in a filename marks it for exclusion

            text_file_patterns: List of substrings that if
            found in a filename marks it comparison using string diffing of its contents

            kwargs_...: Keyword arguments passed to
            the corresponding assert function.
        """
        file_list = self.file_list
        for fname in file_list:
            assert fname.exists()
            try:
                fname = Path(fname)
                # compare 1D files
                if fname.suffix == ".1D":
                    self.assert_1dfiles_equal([fname])
                elif fname.suffix == ".log":
                    # compare logs
                    self.assert_logs_equal([fname])
                elif any(pat in fname.name for pat in self.text_file_patterns):
                    self.assert_textfiles_equal([fname])
                else:
                    # Try to compare it as a scan
                    try:
                        self.assert_scans_equal([fname])
                    except nib.filebasedimages.ImageFileError:
                        # Not sure how to treat this file so just do a byte comparison
                        self.assert_files_by_byte_equal([fname])
            except AssertionError as error:
                if self.require_sample_output:
                    self.files_with_diff[str(fname)] = error
                    continue
                else:
                    raise error
            except FileNotFoundError as error:
                if self.require_sample_output:
                    self.files_with_diff[str(fname)] = error
                else:
                    raise error

    # def test_get_equivalent_name():
    #     test = 'a_path'

    #     with pytest.raises('FileNotFoundError'):

    def get_equivalent_name(self, fname):
        # Given  a file in the output directory, returns a path to the file in
        # the comparison directory
        orig = Path(fname).relative_to(self.data.outdir)
        equivalent_file = self.comparison_dir / orig
        if not equivalent_file.exists():
            raise FileNotFoundError
        return equivalent_file

    def assert_scans_equal(self, files_scans: List):
        for fname in files_scans:
            equivalent_file = self.get_equivalent_name(fname)
            image = nib.load(str(fname))
            equiv_image = nib.load(str(equivalent_file))
            self.assert_scan_headers_equal(image.header, equiv_image.header)
            self.assert_scan_data_equal(image.get_fdata(), equiv_image.get_fdata())

    def assert_files_by_byte_equal(self, files_bytes: List):
        """Compare list of files written as part of a test output with a
        pre-existing output directory

        Args:
            files_bytes : Description
            comparison_dir : Description
        """

        for fname in files_bytes:
            equivalent_file = self.get_equivalent_name(fname)
            assert filecmp.cmp(fname, equivalent_file)

    def assert_logs_equal(self, files_logs):
        self.assert_textfiles_equal(files_logs, **self.kwargs_log)

    def assert_textfiles_equal(
        self,
        textfiles: List,
        old_has: List = [],
        new_has: List = [],
        append_to_ignored: List = [],
        ignore_patterns: List = [],
    ):
        if ignore_patterns:
            # This occurs if called from assert_logs_equal
            if append_to_ignored:
                ignore_patterns += append_to_ignored
        else:
            ignore_patterns = [
                x
                for x in (
                    self.kwargs_text_files["ignore_patterns"]
                    + [self.kwargs_text_files.get("append_to_ignored")]
                )
                if x
            ]

        for fname in textfiles:
            equivalent_file = self.get_equivalent_name(fname)
            text_old = [
                x
                for x in Path(equivalent_file).read_text().splitlines()
                if not any(pat in x for pat in ignore_patterns)
            ]

            text_new = [
                x
                for x in Path(fname).read_text().splitlines()
                if not any(pat in x for pat in ignore_patterns)
            ]

            diff = difflib.unified_diff(text_old, text_new)
            diff_str = "\n".join(diff)

            # Check for a difference for the text file
            assert diff_str == ""

    def assert_1dfiles_equal(self, files_1d, fields=None):
        """Summary

        Args:
            comparison_dir (pathlib.Path): Directory path containing previous
            output that will be compared against.
            files_1d (Iterable[Union[str,int]]): List of AFNI 1D files to compare.
            fields (list, optional): Only the matrix of numbers is comparied by
            default. Fields allows the user to specify a list containing the
            elements to compare. Possible values include: 'mat', 'name', 'fname',
            'aname', 'nvec', 'nt', 'tr', 'nrowfull', 'nruns', 'run_len',
            'run_len_nc', 'nroi', 'GLapplied', 'command', 'header', 'csimobj',
            'csimstat', 'havelabs', 'labels', 'groups', 'goodlist', 'runstart',
            'verb', 'ready', 'cormat', 'cosmat', 'cormat_ready', 'VO'
            rtol (float, optional): Used to set tolerance of matrix comparison
            atol (float, optional): Used to set tolerance of matrix comparison
        """
        kwargs_1d = self.kwargs_1d.copy()
        if "all_close_kwargs" in kwargs_1d:
            all_close_kwargs = kwargs_1d.pop("all_close_kwargs")
        else:
            all_close_kwargs = {}
        if fields:
            raise NotImplementedError

        for fname in files_1d:
            tool_1d = misc.try_to_import_afni_module("1d_tool")
            equivalent_file = self.get_equivalent_name(fname)

            # Load 1D file data
            obj_1d = tool_1d.A1DInterface()
            obj_1d.init_from_file(fname)
            data = obj_1d.adata.__dict__["mat"]

            # Load template 1D file data
            obj_1d_equiv = tool_1d.A1DInterface()
            obj_1d_equiv.init_from_file(equivalent_file)
            data_equiv = obj_1d_equiv.adata.__dict__["mat"]

            # Test the data is equal
            assert_allclose(data, data_equiv, **all_close_kwargs)

    def assert_scan_headers_equal(self, header_a, header_b):
        # assert_image_headers_equal(
        #     [outfile], data.comparison_dir, test_list=[], ignore_list=[],
        assert header_a == header_b

    def assert_scan_data_equal(self, data_a, data_b):
        assert_allclose(data_a, data_b, **self.kwargs_scans["data_kwargs"])

    def text_has(self, text, substrings):
        """Given a body of text and a list of substrings, raises an error if any
        substring does not exist.

        Args:
            text (str): Text to search through.
            substrings (list of str): A list of substrings to search for

        """
        for substr in substrings:
            assert substr in text

    def text_does_not_have(self, text, substrings):
        """Given a body of text and a list of substrings, raises an error if any
        substring does  exist.

        Args:
            text (str): Text to search through.
            substrings (list of str): A list of substrings to search for

        """
        for substr in substrings:
            assert substr not in text

    @property
    def data(self):
        return self._data

    @property
    def ignore_file_patterns(self):
        return self._ignore_file_patterns

    @property
    def comparison_dir(self):
        return self._comparison_dir

    @property
    def text_file_patterns(self):
        return self._text_file_patterns

    @property
    def kwargs_1d(self):
        return self._kwargs_1d

    @property
    def kwargs_log(self):
        return self._kwargs_log

    @property
    def kwargs_text_files(self):
        return self._kwargs_text_files

    @property
    def kwargs_scans(self):
        return self._kwargs_scans

    @property
    def kwargs_byte(self):
        return self._kwargs_byte

    def __repr__(self):
        try:
            return f"""\
        {self.__class__.__name__}(
            data = data, # <conftest.data generated within {self.data.test_name}>
            "{self.cmd}",
            add_env_vars = {self.add_env_vars},
            merge_error_with_output = {self.merge_error_with_output},
            workdir = {self.workdir},
            python_interpreter = {self.python_interpreter},
            ignore_file_patterns = {self.ignore_file_patterns},
            text_file_patterns = {self.text_file_patterns},
            kwargs_1d = {self.kwargs_1d},
            kwargs_log = {self.kwargs_log},
            kwargs_text_files = {self.kwargs_text_files},
            kwargs_scans = {self.kwargs_scans},
            kwargs_byte = {self.kwargs_byte},
            file_list = {self.file_list},
            executed = {self.executed},
        )
        Hex_id: {hex(id(self))}\
        """
        except AttributeError:
            return "<%s %s>" % (self.__class__.__name__, hex(id(self)))


def set_default_kwargs_log_as_required(kwargs_log):

    if "ignore_patterns" not in kwargs_log:
        # Concatenate ignore patterns together taking care not to include
        # empty strings or None
        kwargs_log["ignore_patterns"] = [
            x
            for x in (
                [os.environ.get("USER")]
                + [
                    "AFNI version=",
                    "Clock time now",
                    "elapsed time",
                    "auto-generated by",
                    "CPU time =",
                    "++ Output dataset",
                ]
            )
            if x
        ]

    return kwargs_log
