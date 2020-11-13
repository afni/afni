from pathlib import Path
import contextlib
import datetime as dt
import filelock
import importlib
import inspect
import os
import pytest
import shutil
import sys
import tempfile
import time
import xvfbwrapper

# make sure afni_test_utils importable (it will always either be not installed
# or installed in development mode)
sys.path.append(str(Path(__file__).parent))

try:
    import datalad.api as datalad  # noqa: F401

except ImportError:
    raise NotImplementedError("Currently datalad is a dependency for testing.")

try:
    importlib.import_module("afnipy")
except ImportError as err:
    # installation may be a typical "abin" install. In this case make afnipy
    # importable fo pytest
    bin_path = shutil.which("3dinfo")
    if bin_path:
        abin = Path(bin_path).parent
        sys.path.insert(0, str(abin))
    else:
        raise err

pytest.register_assert_rewrite("afni_test_utils.tools")
from afni_test_utils import data_management as dm  # noqa: E40

if "environ" not in inspect.signature(xvfbwrapper.Xvfb).parameters.keys():
    raise EnvironmentError(
        "Version of xvfbwrapper does not have the environ keyword. "
        "Consider installing one that does. e.g. 'pip install git+git://"
        "github.com/leej3/xvfbwrapper.git@add_support_for_xquartz_and_m"
        "ulti_threading' "
    )

PORT_LOCKS_DIR = Path(tempfile.mkdtemp("port_locks"))

cached_outdir_lock = filelock.FileLock(
    Path("/tmp") / "session_output_directory.lock", timeout=1
)

OUTPUT_DIR_NAME_CACHE = Path("/tmp") / "test_session_outdir_name.txt"
try:
    cached_outdir_lock.acquire()
    if OUTPUT_DIR_NAME_CACHE.exists():
        OUTPUT_DIR_NAME_CACHE.unlink()
    CURRENT_TIME = dt.datetime.strftime(dt.datetime.today(), "%Y_%m_%d_%H%M%S")
    OUTPUT_DIR_NAME_CACHE.write_text(f"output_{CURRENT_TIME}")
    time.sleep(5)
    cached_outdir_lock.release()
except filelock.Timeout:
    OUTPUT_DIR_NAME_CACHE.read_text()

for pipe in "stdout stderr stdin".split():
    encodings_patterns = [x.upper() for x in ["utf-8", "utf8"]]
    if not any(p in getattr(sys, pipe).encoding.upper() for p in encodings_patterns):
        raise EnvironmentError(
            "Only utf-8 should be used for character encoding. Please "
            "change your locale settings as required... LANG_C,LANG_ALL "
            "etc. "
        )


def pytest_sessionstart(session):
    """called after the ``Session`` object has been created and before performing collection
    and entering the run test loop.

    :param _pytest.main.Session session: the pytest session object
    """
    dm.get_tests_data_dir(session)
    print("\n\n The output is being written to:", get_output_dir(session))


def pytest_generate_tests(metafunc):

    # Do some environment tweaks to homogenize behavior across systems
    os.environ["PYTHONDONTWRITEBYTECODE"] = "1"
    os.environ["MPLBACKEND"] = "Agg"
    if sys.platform == "darwin":
        os.environ["DYLD_LIBRARY_PATH"] = "/opt/X11/lib/flat_namespace"

    os.environ["AFNI_SPLASH_ANIMATE"] = "NO"
    unset_vars = [
        "AFNI_PLUGINPATH",
        "AFNI_PLUGIN_PATH",
        "BRIKCOMPRESSOR",
        "AFNI_MODELPATH",
        "AFNI_COMPRESSOR",
        "AFNI_NOREALPATH",
        "AFNI_AUTOGZIP",
        "AFNI_GLOBAL_SESSION",
        "AFNI_ATLAS_LIST",
        "AFNI_TEMPLATE_SPACE_LIST",
        "AFNI_ATLAS_PATH",
        "AFNI_SUPP_ATLAS",
        "AFNI_LOCAL_ATLAS",
        "AFNI_SUPP_ATLAS_DIR",
    ]
    for var in unset_vars:
        if var in os.environ:
            del os.environ[var]

    if "python_interpreter" in metafunc.fixturenames:
        if metafunc.config.option.testpython2:
            metafunc.parametrize("python_interpreter", ["python3", "python2"])
        else:
            metafunc.parametrize("python_interpreter", ["python3"])


@pytest.fixture(scope="function")
def data(pytestconfig, request):
    """A function-scoped test fixture used for AFNI's testing. The fixture
    sets up output directories as required and provides the named tuple "data"
    to the calling function. The data object contains some fields convenient
    for writing tests like the output directory. Finally the data fixture
    handles test input data.files  listed in a data_paths dictionary (if
    defined within the test module) the fixture will download them to a local
    datalad repository as required. Paths should be listed relative to the
    repository base-directory.

    Args: request (pytest.fixture): A function level pytest request object
        providing information about the calling test function.

    Returns:
        collections.NameTuple: A data object for conveniently handling the specification
    """
    data = dm.get_data_fixture(pytestconfig, request, get_output_dir(pytestconfig))
    return data


# configure keywords that alter test collection
def pytest_addoption(parser):
    parser.addoption(
        "--runslow",
        action="store_true",
        default=False,
        help="run slow tests whose execution time is on the order of many seconds)",
    )
    parser.addoption(
        "--runveryslow",
        action="store_true",
        default=False,
        help="run very slow tests whose execution time is on the order "
        "of many minutes to hours ",
    )
    parser.addoption(
        "--diff_with_outdir",
        default=None,
        help="Specify a previous tests output directory with which the output "
        "of this test session is compared.",
    )
    parser.addoption(
        "--create_sample_output",
        action="store_true",
        default=False,
        help=(
            "During many of the tests, sample output is required to "
            "assess changes in the output files. This flag creates all "
            "of the required files for a future comparison and no "
            "comparison is made during the test session. "
        ),
    )
    parser.addoption(
        "--save_sample_output",
        action="store_true",
        default=False,
        help=(
            "By default, the afni_ci_test_data repository is used for "
            "all output data comparisons during testing. This flag "
            "updates the 'sample output' for each test run. Note that "
            "the output that is saved may be different from the output "
            "typically created because only files tested for "
            "differences are included though, by default, this is all "
            "files generated. If previous output exists, only the files "
            "that have differences, as defined by the tests, will be "
            "updated. Uploading updates to the publicly available "
            "repository must be done separately. "
        ),
    )

    parser.addoption(
        "--testpython2",
        action="store_true",
        help=(
            "For tests that use the python_interpreter fixture they are "
            "tested in both python 3 and python 2 "
        ),
    )


def pytest_collection_modifyitems(config, items):
    # more and more tests are skipped as each premature return is not executed:
    if config.getoption("--runveryslow"):
        # --runveryslow given in cli: do not skip slow tests
        return
    else:
        skip_veryslow = pytest.mark.skip(reason="need --runveryslow option to run")
        for item in items:
            if "veryslow" in item.keywords:
                item.add_marker(skip_veryslow)

    if config.getoption("--runslow"):
        # --runslow given in cli: do not skip slow tests
        return
    skip_slow = pytest.mark.skip(reason="need --runslow option to run")
    for item in items:
        if "slow" in item.keywords:
            item.add_marker(skip_slow)


def pytest_sessionfinish(session, exitstatus):
    output_dir = get_output_dir(session)
    # When configured to save output and test session was successful...
    saving_desired = session.config.getoption("--save_sample_output")
    user_wants = session.config.getoption("--create_sample_output")
    create_samp_out = user_wants and not bool(exitstatus)
    if saving_desired and not bool(exitstatus):
        dm.save_output_to_repo()

    if output_dir.exists():
        print("\n\nTest output is written to: ", output_dir)

    full_log = output_dir / "all_tests.log"
    if full_log.exists():
        print("\nLog is written to: ", full_log)

    if create_samp_out:
        print(
            "\n Sample output is written to:",
            dm.convert_to_sample_dir_path(output_dir),
        )
    if saving_desired and bool(exitstatus):
        print(
            "Sample output not saved because the test failed. You may "
            "want to clean this up with 'cd afni_ci_test_data;git reset "
            "--hard HEAD; git clean -df' \n Use this with caution though!"
        )


@pytest.fixture()
def ptaylor_env(monkeypatch):
    with monkeypatch.context() as m:
        isolated_env = os.environ.copy()
        isolated_env["AFNI_COMPRESSOR"] = "GZIP"
        m.setattr(os, "environ", isolated_env)
        try:
            yield
        finally:
            pass


@pytest.fixture()
def unique_gui_port():

    PORT_NUM = 0
    while True:
        try:
            PORT_NUM += 1
            gui_port_lock_path = PORT_LOCKS_DIR / f"gui_port_{PORT_NUM}.lock"
            GUI_PORT_LOCK = filelock.FileLock(gui_port_lock_path)
            GUI_PORT_LOCK.acquire(timeout=1, poll_intervall=0.5)
            return PORT_NUM
        except filelock.Timeout:
            continue


@pytest.fixture(autouse=True)
def _use_test_dir(request):
    tests_dir = Path(__file__).parent
    with working_directory(tests_dir):
        yield


@contextlib.contextmanager
def working_directory(path):
    """Changes working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.fixture()
def mocked_abin():
    """
    Fixture to supply 'mocked_abin', a directory containing trivial
    executables called 3dinfo and align_epi_anat.py and afnipy/afni_base.py (an
    importable python module)
    """
    temp_dir = tempfile.mkdtemp()
    abin_dir = Path(temp_dir) / "abin"
    abin_dir.mkdir()

    (abin_dir / "3dinfo").touch(mode=0o777)
    (abin_dir / "libmri.so").touch(mode=0o444)
    (abin_dir / "3dinfo").write_text("#!/usr/bin/env bash\necho success")
    (abin_dir / "align_epi_anat.py").touch(mode=0o777)
    (abin_dir / "align_epi_anat.py").write_text("#!/usr/bin/env bash\necho success")
    (abin_dir / "afnipy").mkdir()
    (abin_dir / "afnipy/afni_base.py").touch()
    (abin_dir / "afnipy/__init__.py").touch()
    return abin_dir


@pytest.fixture()
def mocked_hierarchical_installation():
    """
    Creates a fake installation directory containing trivial executables
    called 3dinfo and align_epi_anat.py and afnipy/afni_base.py (an importable
    python module) along with libmri in the appropriate organization defined
    by the GNU installation guidelines. To use this you should add the
    fake_site_packages directory to sys.path to simulate the afnipy package
    being installed into the current interpreter.
    """
    temp_dir = tempfile.mkdtemp()
    # Create an installation directory
    instd = Path(temp_dir) / "abin_hierarchical"
    instd.mkdir()
    bindir = instd / "bin"
    bindir.mkdir()

    (bindir / "3dinfo").touch(mode=0o777)
    (bindir / "3dinfo").write_text("#!/usr/bin/env bash\necho success")
    (instd / "lib/").mkdir()
    (instd / "lib/libmri.so").touch(mode=0o444)
    (instd / "fake_site_packages").mkdir()
    (instd / "fake_site_packages/align_epi_anat.py").touch(mode=0o777)
    (instd / "fake_site_packages/align_epi_anat.py").write_text(
        "#!/usr/bin/env bash\necho success"
    )
    (instd / "fake_site_packages/afnipy").mkdir()
    (instd / "fake_site_packages/afnipy/afni_base.py").touch()
    (instd / "fake_site_packages/afnipy/__init__.py").touch()
    return instd


def get_output_dir(config_obj):
    if hasattr(config_obj, "config"):
        conf = config_obj.config
    elif hasattr(config_obj, "rootdir"):
        conf = config_obj
    else:
        print(config_obj)
        raise TypeError("A pytest config object was expected")

    rootdir = conf.rootdir

    output_dir = Path(rootdir) / "output_of_tests" / OUTPUT_DIR_NAME_CACHE.read_text()
    if not output_dir.exists():
        output_dir.mkdir(parents=True)

    return output_dir
