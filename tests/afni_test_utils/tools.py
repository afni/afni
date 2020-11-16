from afni_test_utils import data_management as dm
from asyncio.subprocess import PIPE
from numpy.testing import assert_allclose  # type: ignore
from pathlib import Path
from typing import Dict, List, Any, Union
from xvfbwrapper import Xvfb
import asyncio
import attr
import datalad.api as datalad
import datetime as dt
import difflib
import filecmp
import filelock
import functools
import getpass
import importlib
import itertools as IT
import json
import logging
import nibabel as nib  # type: ignore
import numpy as np
import os
import pytest
import re
import shlex
import shutil
import socket
import stat
import subprocess
import sys
import tempfile

try:
    LAD = importlib.import_module("afnipy.lib_afni1D")
    AFNI_1D_SUPPORT = True
except ImportError:
    print(
        "Import from afnipy failed. OutputDiffer class will not provide "
        "support for comparing afni's 1d files "
    )
    AFNI_1D_SUPPORT = False


DISPLAY_LOCK_PATH = Path(tempfile.gettempdir()) / "afni_tests_display.lock"
DISPLAY = filelock.FileLock(DISPLAY_LOCK_PATH)


def logger_config(logger, file=None, stream_log_level="WARNING", log_file_level=None):
    # Create handlers
    c_handler = logging.StreamHandler()
    c_handler.setLevel(getattr(logging, stream_log_level))

    # Create formatters and add it to handlers
    c_format = logging.Formatter("%(name)s - %(levelname)s - %(message)s")
    c_handler.setFormatter(c_format)
    logger.addHandler(c_handler)

    if log_file_level:
        if not file:
            file = "log.txt"
        f_handler = logging.FileHandler(file)
        f_handler.setLevel(getattr(logging, log_file_level))
        f_format = logging.Formatter(
            "%(levelname)s - %(name)s - %(asctime)s :\n %(message)s"
        )
        f_handler.setFormatter(f_format)
        logger.addHandler(f_handler)

    return logger


def get_output_name():
    CURRENT_TIME = dt.datetime.strftime(dt.datetime.today(), "%Y_%m_%d_%H%M%S")
    return "output_" + CURRENT_TIME


def get_command_info(outdir):
    cmd_log = next((outdir / "captured_output").glob("*_cmd.log"))
    return json.loads(cmd_log.read_text())


def get_lines(filename, ignore_patterns):
    with open(filename, encoding="utf-8") as f:
        lines = [line.rstrip("\n \\") for line in f.readlines()]
        lines = [
            line.strip()
            for line in lines
            if not any(pat in line for pat in ignore_patterns)
            and not line.replace(" ", "") == ""
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


def get_cmd_env(add_env_vars, python_interpreter):
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

    if sys.platform == "darwin":
        add_env_vars["DYLD_LIBRARY_PATH"] = "/opt/X11/lib/flat_namespace"
    # Set environment variables for the command execution
    cmd_environ = os.environ.copy()
    for k, v in add_env_vars.items():
        cmd_environ[k] = v
    return cmd_environ


def log_command_info(data, cmd_args, logger, add_env_vars, workdir, cmd_log):
    # cmd execution report
    cmd = " ".join(cmd_args)
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
    write_command_info(cmd_log, {**command_info, **attr.asdict(data)})


def setup_logging(data, logger):

    if logger:
        pass
    elif data.logger:
        logger = data.logger
    else:
        logger = logging

    # Set log path
    stdout_log = data.logdir / (data.test_name + "_stdout.log")

    # Make the appropriate directories and check that
    # the log files do not exist, otherwise (using uniquize) append a number:
    os.makedirs(stdout_log.parent, exist_ok=True)
    stdout_log = uniquify(stdout_log)

    stderr_log = Path(str(stdout_log).replace("_stdout", "_stderr"))
    cmd_log = Path(str(stdout_log).replace("_stdout", "_cmd"))
    return logger, cmd_log, stdout_log, stderr_log


def pid_exists(pid, check_pgid=False):

    if pid < 0:
        return False  # NOTE: pid == 0 returns True
    try:
        if check_pgid:
            os.killpg(pid, 0)
        else:
            os.kill(pid, 0)
    except ProcessLookupError:  # errno.ESRCH
        return False  # No such process
    except PermissionError:  # errno.EPERM
        return True  # Operation not permitted (i.e., process exists)
    else:
        return True  # no error, we can send a signal to the process


def get_output_file_handles(stdout_log, stderr_log, merge_error_with_output):
    # If requested merge stderr and stdout
    stdout = open(stdout_log, "wb")
    if merge_error_with_output:
        stderr = stdout
    else:
        stderr = open(stderr_log, "wb")
    return stdout, stderr


async def watch(stream, logger, output_handle, prefix=""):
    async for line in stream:
        logger.debug(f"{prefix}{line.decode('utf-8').strip()}")
        output_handle.write(line)


async def __execute_cmd_args_asynchronous(
    cmd_args,
    logger,
    stdout_log,
    stderr_log,
    workdir,
    merge_error_with_output=True,
    cmd_environ=None,
    shell=False,
    timeout=None,
    name=None,
):
    stdout, stderr = get_output_file_handles(
        stdout_log,
        stderr_log,
        merge_error_with_output,
    )
    cmd = " ".join(cmd_args)
    p = await asyncio.create_subprocess_shell(
        cmd,
        env=cmd_environ,
        stdout=PIPE,
        stderr=PIPE,
        cwd=workdir,
    )
    done, pending = await asyncio.wait(
        (
            watch(p.stdout, logger, stdout),
            watch(p.stderr, logger, stderr, "E:"),
        ),
        timeout=timeout,
    )
    if not len(pending) == 0:
        stdoutdir = "/".join([stdout_log.parent.name, stderr_log.name])
        raise TimeoutError(f"stdout: {stdoutdir} in {stdout_log.parent.parent}")


def __execute_cmd_args(
    cmd_args,
    logger,
    stdout_log,
    stderr_log,
    workdir,
    merge_error_with_output=False,
    cmd_environ=None,
    shell=False,
    timeout=None,
    name=None,
):
    """
    Synchronous command execution to help with debugging when odd behavior is
    observed
    """
    stdout, stderr = get_output_file_handles(
        stdout_log, stderr_log, merge_error_with_output
    )
    try:
        proc = subprocess.Popen(
            cmd_args,
            stdout=stdout,
            stderr=stderr,
            cwd=workdir,
            env=cmd_environ,
            shell=False,
            preexec_fn=os.setsid,
        )
        proc.communicate(timeout=timeout)
    finally:
        stdout.close()
        if stderr is not stdout:
            stderr.close()

    return proc.returncode


def run_cmd(
    data,
    cmd,
    logger=None,
    add_env_vars=None,
    merge_error_with_output=False,
    workdir=None,
    python_interpreter="python3",
    x_execution_mode=None,
    timeout=30,
    shell=False,
    use_asynchronous_execution=True,
):
    """Run the provided command and check it's output. In conjunction with the data
    fixture this function handles the test output logging in a consistent way.
    Args:
        data: An object created by the data fixture that is used in test
        cmd (str): A string that requires execution and error checking.
        logger (logging.Logger, optional): Logger object to write to.
        add_env_vars (dict): Variable/value pairs with which to modify the
        environment for the executed command.
        merge_error_with_output (bool, optional): Merge stdout and stderr.
        workdir (pathlib.Path or str): Working directory to execute the
        command. Should be the root directory for tests (default) unless
        otherwise required.
        x_execution_mode (None or str, optional): Run with Xvfb ('xvfb'), on
        the physical display ('display') or without any management for
        graphics events (None)
        timeout (int, optional): Timeout parameter in seconds.
        shell (bool, optional): Kwarg subprocess execution.
        use_asynchronous_execution (bool, optional): Setting this to fault may
        aid debugging in certain circumstances, otherwise the default of True
        enables superior execution behavior.
    """

    # Set working directory for command execution if not set explicitly
    if not workdir:
        workdir = Path.cwd()
    if not add_env_vars:
        add_env_vars = {}

    logger, cmd_log, stdout_log, stderr_log = setup_logging(data, logger)

    # Make substitutions in the command string  to remove unnecessary absolute
    # paths:
    rel_data_path = os.path.relpath(data.base_outdir, workdir)
    cmd = cmd.replace(str(data.base_outdir), rel_data_path)
    cmd = cmd.replace(str(workdir), ".")

    # log command that will be used (not quite true for the following cases
    # though for which a script is used to wrap the command)
    logger.info(f"cd {workdir};{cmd}")

    # For more tricky commands just write them to a temporary script and
    # execute that
    if any(x in cmd for x in ("&", ";", "'", "`", ">")):
        script = tempfile.mktemp()
        Path(script).write_text(
            "#/bin/bash -exu\nset -o pipefail\n" + cmd, encoding="utf-8"
        )
        cmd_args = ["bash", script]
    else:
        cmd_args = shlex.split(cmd)

    cmd_environ = get_cmd_env(add_env_vars, python_interpreter)
    log_command_info(data, cmd_args, logger, add_env_vars, workdir, cmd_log)

    # User can switch between synchronous and asynchronous execution.
    # asynchronous is much more difficult to debug but has lots of desirable
    # properties... output is in real-time, timeouts can be managed
    # intelligently, sleeping processes don't cause issues (a proc.wait call
    # waits forever so a proc.communicate call must be used... which kills
    # the subprocess upon timeout)
    if use_asynchronous_execution:
        exec_func = __execute_cmd_args_asynchronous
        loop = asyncio.get_event_loop()
        # asyncio.set_event_loop(loop)
    else:
        exec_func = __execute_cmd_args
        loop = None

    # Using the execution pattern defined above make a partial function call.
    # Setting this up here removes duplication; depending on the context
    # (what virtual/physic display is used) the execution is then triggered below.
    cmd_callable = functools.partial(
        exec_func,
        cmd_args,
        logger,
        stdout_log,
        stderr_log,
        merge_error_with_output=merge_error_with_output,
        workdir=workdir,
        shell=shell,
        timeout=timeout,
        name=data.test_name,
    )
    global DISPLAY
    if x_execution_mode is None:
        # Not testing a gui so no need for any display shenanigans
        logger.debug(f"cmd_args:{cmd_args}")
        if use_asynchronous_execution:
            returncode = loop.run_until_complete(cmd_callable(cmd_environ=cmd_environ))
        else:
            returncode = cmd_callable(cmd_environ=cmd_environ)
        # proc, output_blend = cmd_callable(cmd_environ=cmd_environ)
    elif x_execution_mode == "xvfb":
        try:
            # set a default display but another will be determined as required
            xvfb = Xvfb(width=800, height=680, environ=cmd_environ)
            # some suma stuff requires the glx extension on linux
            xvfb.extra_xvfb_args += ["+iglx", "+extension", "DOUBLE-BUFFER"]
            xvfb.start()
            logger.debug(f"Virtual display being used: {xvfb.new_display}")
            # Set the display variable for the execution environment and then
            # restore the original value
            logger.debug(f"cmd_args:{cmd_args}")
            if use_asynchronous_execution:
                returncode = loop.run_until_complete(
                    cmd_callable(cmd_environ=cmd_environ)
                )
            else:
                returncode = cmd_callable(cmd_environ=cmd_environ)
        finally:
            # Remove the lock file
            xvfb.stop()

    elif x_execution_mode == "display":
        # For when one does not wish to use xvfb for gui testing
        try:
            DISPLAY.acquire()
            logger.debug(f"Physical Display: {cmd_environ['DISPLAY']}")
            logger.debug(f"cmd_args:{cmd_args}")
            if use_asynchronous_execution:
                returncode = loop.run_until_complete(
                    cmd_callable(cmd_environ=cmd_environ)
                )
            else:
                returncode = cmd_callable(cmd_environ=cmd_environ)
        finally:
            DISPLAY.release()
    else:
        raise ValueError(f"Unknown display mode {x_execution_mode}")

    # Raise error if there was a non-zero exit code.
    if returncode:
        raise ValueError(f"{cmd}\n Command returned a non-zero exit code.")

    return stdout_log, stderr_log


def write_command_info(path, cmd_info):
    out_dict = {}
    for k, v in cmd_info.items():
        v = str(v)
        out_dict[k] = v
    Path(path).write_text(json.dumps(out_dict, indent=0), encoding="utf-8")


def get_equivalent_name(data, fname):
    # Given  a file in the output directory, returns a path to the file in
    # the comparison directory
    orig = Path(fname).relative_to(data.outdir)
    equivalent_file = data.comparison_dir / orig
    if not equivalent_file.exists():
        raise FileNotFoundError
    return equivalent_file


def _rewrite_paths_for_lines(txt, tests_data_dir, outdir, wdir, hostname, user):
    # make paths in the test data directory relative in reference to the outdir.
    rel_testdata = os.path.relpath(tests_data_dir, outdir)
    txt = [x.replace(str(tests_data_dir), rel_testdata) for x in txt]
    # replace output directories so that they look the same
    fake_outdir = str(Path(rel_testdata) / "sample_test_output")
    txt = [x.replace(str(outdir), fake_outdir) for x in txt]
    # do same for relative outdir paths
    rel_outdir = str(Path(outdir).relative_to(wdir))
    if rel_outdir != ".":
        txt = [x.replace(rel_outdir, fake_outdir) for x in txt]
    # replace user and hostnames:
    txt = [x.replace(hostname, "hostname") for x in txt]
    txt = [x.replace(user, "user") for x in txt]

    # make absolute references to workdir relative.
    txt = [x.replace(wdir, ".") for x in txt]
    return txt


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
        txt = _rewrite_paths_for_lines(
            txt,
            data.tests_data_dir,
            data.outdir,
            wdir,
            cmd_info["host"],
            cmd_info["user"],
        )
        if not create_sample_output:
            # Do the same for previous files being compared against:
            txt = _rewrite_paths_for_lines(
                txt,
                data.tests_data_dir,
                cmd_info_orig["outdir"],
                wdir_orig,
                cmd_info_orig["host"],
                cmd_info_orig["user"],
            )

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
        kwargs_for_run: Dict = None,
        create_sample_output: bool = False,
        save_sample_output: bool = False,
        skip_output_diff: bool = False,
        file_list: List = None,
        logger=None,
    ):
        self._data = data

        # use a logger: defined by user, conftest.py, or default root logger
        self.logger = logger or getattr(data, "logger", None) or logging

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
        self._ignore_file_patterns = ignore_file_patterns or []
        if isinstance(ignore_file_patterns, str):
            raise TypeError("ignore_file_patterns should not be type 'str'")
        self._comparison_dir = data.comparison_dir
        self._text_file_patterns = text_file_patterns or []
        self._kwargs_log = set_default_kwargs_log_as_required(kwargs_log)
        self._kwargs_1d = kwargs_1d or {}
        self._kwargs_text_files = kwargs_text_files or {}
        self._kwargs_scans = kwargs_scans or {"header_kwargs": {}, "data_kwargs": {}}
        self._kwargs_byte = kwargs_byte or {}
        self._kwargs_for_run = kwargs_for_run or {}
        self.skip_output_diff = skip_output_diff
        # If empty this is overwritten when the command is executed
        self.file_list = file_list or []
        # If saving output data as future comparison this is modified:
        self.files_with_diff = {}

    def run(self, **kwargs):
        """
        method to wrap run_cmd. Keyword arguments can be passed to run_cmd by
        defining them in the kwargs_for_run dict when creating the instance of
        OutputDiffer, (or alternatively for some kwargs, by passing the
        keyword arguments to the run method of the OutputDiffer instance
        """
        # Migrated kwargs to instance attributes
        dups = [key for key in kwargs if hasattr(self, key)]
        for key in dups:
            setattr(self, key, kwargs.pop(key))
        # Update the run dict with remaining keys
        self.kwargs_for_run.update(kwargs)
        stdout, stderr = self.run_cmd(**self.kwargs_for_run)

        # Shortcut to skip all file comparisons
        if self.skip_output_diff:
            return stdout, stderr

        if not self.create_sample_output:
            self.check_comparison_dir()
            self.assert_all_files_equal()

        if self.require_sample_output:
            self.__update_sample_output()
        return stdout, stderr

    def run_cmd(self, **run_kwargs):
        if self.executed:
            raise ValueError(
                "The differ object has already been run as defined by "
                "the 'executed' attribute. "
            )

        # Call run_cmd defined in module scope.
        stdout, stderr = run_cmd(
            self.data,
            self.cmd,
            self.logger,
            add_env_vars=self.add_env_vars,
            merge_error_with_output=self.merge_error_with_output,
            workdir=self.workdir,
            python_interpreter=self.python_interpreter,
            **run_kwargs,
        )

        self.get_file_list()
        self.executed = True
        return stdout, stderr

    def get_file_list(self):
        always_ignore = ["gmon.out", ".DS_Store"]
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
                if fname.suffix == ".1D" and AFNI_1D_SUPPORT:
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
            equivalent_file = get_equivalent_name(self.data, fname)

            # Load 1D file data
            data = LAD.Afni1D(fname).__dict__["mat"]

            # Load template 1D file data
            data_equiv = LAD.Afni1D(fname).__dict__["mat"]

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
            if f not in cmpr_files_rel:
                missing_files.append(str(cmpr_path / f))
        if missing_files:
            m_str = " ".join(missing_files)
            raise ValueError(
                "The following files are missing and are required to "
                f"fully complete the test: {m_str} "
            )

        need_data = any(p.is_symlink() and not p.exists() for p in cmpr_files)
        if need_data:
            dm.try_data_download([cmpr_path], dl_dset.path, self.data.logger)

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
    def kwargs_for_run(self):
        return self._kwargs_for_run

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
