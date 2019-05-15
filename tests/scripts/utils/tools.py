import filecmp
from pathlib import Path
import nibabel as nib  # type: ignore
import difflib
import subprocess

# import misc

from . import misc
import tempfile
import itertools as IT
import os
import pytest

from numpy.testing import assert_allclose  # type: ignore

from typing import Dict, List, Any, Union


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


class OutputDiffer:
    def __init__(
        self,
        data: Any,
        cmd,
        add_env_vars: Dict = {},
        merge_error_with_output: bool = False,
        workdir: Union[str or Path] = None,
        force_python2: bool = False,
        ignore_file_patterns: List = [],
        text_file_patterns: List = [],
        kwargs_1d: Dict = {},
        kwargs_log: Dict = {},
        kwargs_text_files: Dict = {},
        kwargs_scans: Dict = {"header_kwargs": {}, "data_kwargs": {}},
        kwargs_byte: Dict = {},
        file_list: List = [],
    ):
        self._data = data

        # Tune command execution
        self.cmd = cmd
        self.add_env_vars = add_env_vars
        self.merge_error_with_output = merge_error_with_output
        self.workdir = workdir or Path.cwd()
        self.force_python2 = force_python2

        # Tune the output comparison
        self._ignore_file_patterns = ignore_file_patterns
        self._comparison_dir = data.comparison_dir
        self._text_file_patterns = text_file_patterns
        self._kwargs_1d = kwargs_1d
        self._kwargs_log = kwargs_log
        self._kwargs_text_files = kwargs_text_files
        self._kwargs_scans = kwargs_scans
        self._kwargs_byte = kwargs_byte

        if file_list:
            self._file_list = file_list
        else:
            self._file_list = [
                f
                for f in data.outdir.glob("**/*")
                if f.is_file()
                and not any(pat in str(f) for pat in ignore_file_patterns)
            ]

    def run(self):
        proc = self.run_cmd()
        self.assert_all_files_equal()

    def run_cmd(self):
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
        data = self.data

        # Define log file paths
        stdout_log = data.logdir / (data.test_name + "_stdout.log")
        # Make the appropriate output directories
        os.makedirs(stdout_log.parent, exist_ok=True)

        # Confirm that the file does not exist, otherwise append a number:
        stdout_log = uniquify(stdout_log)
        stderr_log = Path(str(stdout_log).replace("_stdout", "_stderr"))

        # Set environment variables for the command execution
        os.environ["OMP_NUM_THREADS"] = "1"
        for k, v in self.add_env_vars.items():
            os.environ[k] = v

        with misc.remember_cwd():
            os.chdir(self.workdir)
            # Execute the command and log output
            if self.merge_error_with_output:
                error = subprocess.STDOUT
            else:
                error = subprocess.PIPE

            proc = subprocess.run(
                self.cmd, shell=True, stdout=subprocess.PIPE, stderr=error
            )
            # log the output
            stdout_log.write_text(proc.stdout.decode("utf-8"))
            if proc.stderr:
                err_text = proc.stderr.decode("utf-8")
                stderr_log.write_text(err_text)

            # Raise error if there was a non-zero exit code.
            proc.check_returncode()
        return proc

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
        comparison_dir = self.comparison_dir
        file_list = self.file_list
        for fname in file_list:

            fname = Path(fname)
            # compare 1D files
            if fname.suffix == ".1D":
                self.assert_1dfiles_equal(comparison_dir, [fname])
            elif fname.suffix == ".log":
                # compare logs
                self.assert_logs_equal(comparison_dir, [fname])
            elif any(pat in fname.name for pat in text_file_patterns):
                self.assert_textfiles_equal(comparison_dir, [fname])
            else:
                # Try to compare it as a scan
                try:
                    self.assert_scans_equal(comparison_dir, [fname])
                except nib.filebasedimages.ImageFileError:
                    # Not sure how to treat this file so just do a byte comparison
                    self.assert_files_by_byte_equal(comparison_dir, [fname])

    def assert_scans_equal(self, scans: List):
        comparison_dir = self.comparison_dir

        # If no useful comparison is expected then just return
        if pytest.config.getoption("--create_sample_output") or pytest.config.getoption(
            "--save_sample_output"
        ):
            return

        for fname in scans:
            equivalent_file = comparison_dir / fname
            image = nib.load(str(fname))
            equiv_image = nib.load(str(equivalent_file))
            self._assert_scan_headers_equal(
                image.header, equiv_image.header, **header_kwargs
            )
            self._assert_scan_data_equal(
                image.get_fdata(), equiv_image.get_fdata(), **data_kwargs
            )

    def assert_files_by_byte_equal(self, file_list: List):
        """Compare list of files written as part of a test output with a
        pre-existing output directory

        Args:
            file_list : Description
            comparison_dir : Description
        """
        # If no useful comparison is expected then just return
        if pytest.config.getoption("--create_sample_output") or pytest.config.getoption(
            "--save_sample_output"
        ):
            return

        for fname in file_list:
            equivalent_file = self.comparison_dir / fname
            assert filecmp.cmp(fname, equivalent_file)

    def assert_logs_equal(
        self,
        comparison_dir: Path,
        flist: List,
        append_to_ignored: List = [],
        ignore_patterns: List = ["AFNI version="],
    ):

        ignore_patterns = (
            append_to_ignored
            + ignore_patterns
            + [str(comparison_dir)]
            + [str(f) for f in flist]
        )

        self.assert_textfiles_equal(
            comparison_dir, flist, ignore_patterns=ignore_patterns
        )

    def assert_textfiles_equal(
        self,
        comparison_dir: Path,
        textfiles: List,
        old_has: List = [],
        new_has: List = [],
        ignore_patterns: List = [],
    ):
        # If no useful comparison is expected then just return
        # pytest.config.getoption("--create_sample_output") or pytest.config.getoption(
        #     "--save_sample_output"
        # ):
        # return

        for fname in textfiles:
            equivalent_file = comparison_dir / fname
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
            try:
                assert diff_str == ""
            except AssertionError as e:
                if pytest.config.getoption("--save_sample_output"):
                    continue
                elif pytest.config.getoption("--create_sample_output"):
                    # If the file is different from pre-existing output it should be updated
                    raise NotImplementedError
                else:
                    raise e

    def assert_1dfiles_equal(
        self, comparison_dir, file_list, fields=None, **all_close_kwargs
    ):
        """Summary

        Args:
            comparison_dir (pathlib.Path): Directory path containing previous
            output that will be compared against.
            file_list (Iterable[Union[str,int]]): List of AFNI 1D files to compare.
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
        # If no useful comparison is expected then just return
        if pytest.config.getoption("--create_sample_output") or pytest.config.getoption(
            "--save_sample_output"
        ):
            return

        if fields:
            raise NotImplementedError

        for fname in file_list:
            tool_1d = misc.try_to_import_afni_module("1d_tool")
            equivalent_file = comparison_dir / fname
            if not equivalent_file.exists():
                raise FileNotFoundError

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

    def _assert_scan_headers_equal(self, header_a, header_b, **kwargs):
        # tools.assert_image_headers_equal(
        #     [outfile], data.comparison_dir, test_list=[], ignore_list=[],
        assert header_a == header_b

    def _assert_scan_data_equal(self, data_a, data_b, **kwargs):
        assert_allclose(data_a, data_b, **kwargs)

    def _text_has(self, text, substrings):
        """Given a body of text and a list of substrings, raises an error if any
        substring does not exist.

        Args:
            text (str): Text to search through.
            substrings (list of str): A list of substrings to search for

        """
        for substr in substrings:
            assert substr in text

    def _text_does_not_have(self, text, substrings):
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

    @property
    def file_list(self):
        return self._file_list

    def __repr__(self):
        return f"""\
        {self.__class__.__name__}(
            data = data, # <conftest.data generated within {self.data.test_name}>
            "{self.cmd}",
            add_env_vars = {self.add_env_vars},
            merge_error_with_output = {self.merge_error_with_output},
            workdir = {self.workdir},
            force_python2 = {self.force_python2},
            ignore_file_patterns = {self.ignore_file_patterns},
            text_file_patterns = {self.text_file_patterns},
            kwargs_1d = {self.kwargs_1d},
            kwargs_log = {self.kwargs_log},
            kwargs_text_files = {self.kwargs_text_files},
            kwargs_scans = {self.kwargs_scans},
            kwargs_byte = {self.kwargs_byte},
            file_list = {self.file_list},
        )
        Hex_id: {hex(id(self))}\
        """
