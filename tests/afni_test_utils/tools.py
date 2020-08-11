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
import json
import attr

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


def get_command_info(outdir):
    cmd_log = next((outdir / "captured_output").glob("*_cmd.log"))
    return json.loads(cmd_log.read_text())


def convert_to_sample_dir_path(output_dir):
    sampdir = Path(str(output_dir).replace("output_", "sample_output_"))
    return sampdir


def get_lines(filename, ignore_patterns):
    with open(filename) as f:
        lines = [line.rstrip("\n \\") for line in f.readlines()]
        lines = [
            line.strip()
            for line in lines
            if not any(pat in line for pat in ignore_patterns)
            and not line.replace(" ", "") is ""
        ]
        return lines


def remove_w_perms(dirname):

    os.chmod(dirname, stat.S_IREAD + stat.S_IEXEC)
    for root, dirs, files in os.walk(dirname):
        for momo in dirs:
            os.chmod(Path(root, momo), stat.S_IREAD + stat.S_IEXEC)
        for file in files:
            os.chmod(Path(root, file), stat.S_IREAD)


def get_current_test_name():
    name_str = os.environ.get("PYTEST_CURRENT_TEST").split(":")[-1].split(" ")[0]
    name_str = name_str.replace(".", "")
    return re.sub(r"[-\[\]\(\)\*]", "_", name_str).strip("_")


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
    proc = subprocess.check_call(cmd, shell=True, cwd=data.rootdir)


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
    command_info = {
        "host": hostname,
        "user": user,
        "workdir": workdir,
        "cmd": cmd,
        "add_env_vars": add_env_vars,
        "rootdir": data.rootdir,
    }
    command_info.update(attr.asdict(data))

    write_command_info(cmd_log, command_info)
    # Raise error if there was a non-zero exit code.
    proc.check_returncode()
    return proc


def write_command_info(path, cmd_info):
    out_dict = {}
    for k, v in cmd_info.items():
        v = str(v)
        out_dict[k] = v
    Path(path).write_text(json.dumps(out_dict))


def get_workdir(data):
    cmd_path = next(data.logdir.glob("*cmd.log"))


def get_equivalent_name(data, fname):
    # Given  a file in the output directory, returns a path to the file in
    # the comparison directory
    orig = Path(fname).relative_to(data.outdir)
    equivalent_file = data.comparison_dir / orig
    if not equivalent_file.exists():
        raise FileNotFoundError
    return equivalent_file


def rewrite_paths_for_cleaner_diffs(data, text_list, create_sample_output=False):
    """Given  a list of texts,  which are in turn lists, this function
    attempts to normalize paths, user, hostname in order to  reduce the rate
    of false positive diffs observed.

    This function should probably be rewritten to take a single text and
    single data. The wdir check is not that important.

    Args:
        data (TYPE): Description

        text_list (List): A list containing texts that have been broken into
        their respective lines.

        create_sample_output (bool, optional): If true then no diff is required.

    Returns:
        TYPE: Description

    Raises:
        ValueError: Description
    """
    cmd_info = get_command_info(data.outdir)
    wdir = cmd_info["workdir"]
    if not create_sample_output:
        cmd_info_orig = get_command_info(data.comparison_dir)
        wdir_orig = cmd_info_orig["workdir"]
        if wdir.split("/")[-1] != wdir_orig.split("/")[-1]:
            raise ValueError(
                "Comparison with previous test output cannot be performed "
                "if a different working directory was used. "
            )

    outlist = []
    for txt in text_list:
        # make paths in the test data directory relative in reference to the outdir.
        rel_testdata = os.path.relpath(data.tests_data_dir, data.outdir)
        txt = [x.replace(str(data.tests_data_dir), rel_testdata) for x in txt]

        # replace output directories so that they look the same
        fake_outdir = str(Path(rel_testdata) / "sample_test_output")
        txt = [x.replace(str(data.outdir), fake_outdir) for x in txt]
        txt = [x.replace(str(data.outdir.relative_to(wdir)), fake_outdir) for x in txt]
        # replace user and hostnames:
        txt = [x.replace(cmd_info["host"], "hostname") for x in txt]
        txt = [x.replace(cmd_info["user"], "user") for x in txt]
        # make absolute references to workdir relative.
        txt = [x.replace(wdir, ".") for x in txt]

        # Do the same for previous files being compared against:
        if not create_sample_output:
            rel_orig = str(Path(cmd_info_orig["outdir"]).relative_to(wdir_orig))
            txt = [x.replace(cmd_info_orig["outdir"], fake_outdir) for x in txt]
            txt = [x.replace(wdir_orig, ".") for x in txt]
            txt = [x.replace(rel_orig, fake_outdir) for x in txt]
            # replace user and hostnames:
            txt = [x.replace(cmd_info_orig["host"], "hostname") for x in txt]
            txt = [x.replace(cmd_info_orig["user"], "user") for x in txt]

        outlist.append(txt)

    return outlist


def set_default_kwargs_log_as_required(kwargs_log):

    if not kwargs_log:
        kwargs_log = {}

    if "ignore_patterns" not in kwargs_log:
        # Concatenate ignore patterns together taking care not to include
        # empty strings or None
        kwargs_log["ignore_patterns"] = [
            x
            for x in (
                [os.environ.get("USER")]
                + [
                    "AFNI version=",
                    "Version",
                    "Clock time now",
                    "clock time",
                    "elapsed time",
                    "auto-generated by",
                    "CPU time =",
                    "++ Output dataset",
                    " OMP",
                ]
            )
            if x
        ]

    return kwargs_log


class OutputDiffer:
    """
    Args:
    data: Fixture object used in tests

    ignore_file_patterns: List of substrings that if
    found in a filename marks it for exclusion

    text_file_patterns: List of substrings that if
    found in a filename marks it comparison using string diffing of its contents

    kwargs_...: Keyword arguments passed to
    the corresponding assert function.
    """

    def __init__(
        self,
        data: Any,
        cmd,
        add_env_vars: Dict = None,
        merge_error_with_output: bool = False,
        workdir: Union[str or Path] = None,
        python_interpreter: str = "python3",
        ignore_file_patterns: List = None,
        text_file_patterns: List = None,
        kwargs_1d: Dict = None,
        kwargs_log: Dict = None,
        kwargs_text_files: Dict = None,
        kwargs_scans: Dict = None,
        kwargs_byte: Dict = None,
        create_sample_output: bool = False,
        save_sample_output: bool = False,
        file_list: List = None,
    ):
        self._data = data

        # Tune command execution
        self.cmd = cmd
        self.add_env_vars = add_env_vars or {}
        self.merge_error_with_output = merge_error_with_output
        self.workdir = workdir
        self.python_interpreter = python_interpreter
        self.executed = False

        # Tune output saving behavior
        self.create_sample_output = create_sample_output or data.create_sample_output
        self.save_sample_output = save_sample_output or data.save_sample_output
        # Tune the output comparison
        self.require_sample_output = (
            self.create_sample_output or self.save_sample_output
        )
        self._ignore_file_patterns = ignore_file_patterns or ["_stdout"]
        if isinstance(ignore_file_patterns, str):
            raise TypeError("ignore_file_patterns should not be type 'str'")
        self._comparison_dir = data.comparison_dir
        self._text_file_patterns = text_file_patterns or []
        self._kwargs_log = set_default_kwargs_log_as_required(kwargs_log)
        self._kwargs_1d = kwargs_1d or {}
        self._kwargs_text_files = kwargs_text_files or {}
        self._kwargs_scans = kwargs_scans or {"header_kwargs": {}, "data_kwargs": {}}
        self._kwargs_byte = kwargs_byte or {}
        # If empty this is overwritten when the command is executed
        self.file_list = file_list or []
        # If saving output data as future comparison this is modified:
        self.files_with_diff = {}

    def run(self):
        proc = self.run_cmd()
        if not self.create_sample_output:
            self.check_comparison_dir()
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
        always_ignore = ["gmon.out"]
        if not self.file_list:
            for f in list(self.data.outdir.glob("**/*")):
                if not f.is_file():
                    continue
                if f.name in always_ignore:
                    continue
                if any(pat in str(f) for pat in self.ignore_file_patterns):
                    continue

                self.file_list.append(f)

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
                    # compare stdout and stderr logs
                    if fname.name.endswith("_cmd.log"):
                        # command log diff is expected but ignored. The file
                        # should be included in sample output when created as
                        # it is used to determine the working directory for
                        # command execution.
                        self.files_with_diff[str(fname)] = "command log"
                        continue
                    self.assert_logs_equal([fname])
                elif any(pat in fname.name for pat in self.text_file_patterns):
                    self.assert_textfiles_equal([fname])
                elif fname.name.endswith(".niml.dset"):
                    raise NotImplementedError
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

    def assert_scans_equal(self, files_scans: List):
        for fname in files_scans:
            equivalent_file = get_equivalent_name(self.data, fname)
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
            equivalent_file = get_equivalent_name(self.data, fname)
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
        if ignore_patterns or append_to_ignored:
            # This occurs if called from assert_logs_equal
            ignore_patterns += append_to_ignored
        else:
            ignore_patterns = [
                x
                for x in (
                    self.kwargs_text_files.get("ignore_patterns", [])
                    + self.kwargs_text_files.get("append_to_ignored", [])
                )
                if x
            ]

        for fname in textfiles:

            # load the old lines of text
            equivalent_file = get_equivalent_name(self.data, fname)
            text_old = get_lines(equivalent_file, ignore_patterns)

            # load the current lines of text
            text_new = get_lines(fname, ignore_patterns)

            text_old, text_new = rewrite_paths_for_cleaner_diffs(
                self.data, [text_old, text_new], self.create_sample_output
            )

            # Check for diffs (with paths normalized)
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
            equivalent_file = get_equivalent_name(self.data, fname)

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

    def check_comparison_dir(self):
        cmpr_path = self.data.comparison_dir
        dl_dset = datalad.Dataset(str(self.data.tests_data_dir))
        if not cmpr_path.exists():
            raise ValueError(
                "The following path does not exist but is required to "
                f"perform a test:{cmpr_path}.\n You may wish to run the "
                "test with the --create_sample_output flag or generate "
                "output for future test sessions with "
                "--save_sample_output. "
            )
        cmpr_files = list(cmpr_path.glob("**/*"))
        cmpr_files_rel = [f.relative_to(cmpr_path) for f in cmpr_files]

        files_required = [f.relative_to(self.data.outdir) for f in self.file_list]
        missing_files = []
        for f in files_required:
            if not f in cmpr_files_rel:
                missing_files.append(str(cmpr_path / f))
        if missing_files:
            m_str = " ".join(missing_files)
            raise ValueError(
                "The following files are missing and are required to "
                f"fully complete the test: {m_str} "
            )

        need_data = any(p.is_symlink() and not p.exists() for p in cmpr_files)
        if need_data:
            dl_dset.get(str(cmpr_path))

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
