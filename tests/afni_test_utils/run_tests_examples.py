# These arguments are parsed as part of tests. The last line of each example
# should be a valid call to run_afni_tests.py
examples = {
    "most basic example": "run_afni_tests.py local",
    "specifying the installation directory": "run_afni_tests.py --abin=~/abin local",
    "help for pytest": "run_afni_tests.py --extra-args='--help' local",
    "altering behavior of pytest": "run_afni_tests.py --extra-args='-k mask' local",
    "making use of cmake": "\n    ".join(
        ["mkdir /tmp/build", "run_afni_tests.py --build-dir=/tmp/build local",]
    ),
    "tuning resources": "run_afni_tests.py --extra-args='--workers=4' local",
    "using all cores": "run_afni_tests.py --use-all-cores local",
    "execute tests by filename": "run_afni_tests.py --file scripts/test_ptaylor.py local",
    "execute tests by pattern": "run_afni_tests.py --extra-args='-k mask' local",
    "execute tests by pattern shortcut": "run_afni_tests.py -k mask local",
    "execute tests by previous failure": "run_afni_tests.py --extra-args='--lf' local",
    "specifying pytest args without run_afni_tests.py defaults": "run_afni_tests.py --overwrite-args='--verbose' local",
    "be verbose": "run_afni_tests.py -v local",
    "be especially verbose": "run_afni_tests.py -vvvv local",
    "use debugger": "run_afni_tests.py --debug local",
    "most basic example for container": "run_afni_tests.py container",
    "examples with explanation ": "run_afni_tests.py examples --verbose",
}

EXAMPLES = f"""
=============================================================================
run_afni_tests.py examples      - usage examples for run_afni_tests.py

---------------------------------------------------------------------------
purpose: ~1~

   The run_afni_tests.py program is an internal tool (not distributed with
   AFNI) to help run the test suite.

---------------------------------------------------------------------------
Basic examples (testing on the local host): ~1~

The most basic example ~2~

    {examples['most basic example']}

Under the hood this is running something like:
"cd /path/to/tests/directory
pytest scripts -r=Exs  --show-capture=no --tb=no --verbose -s"
(where scripts is a directory containing python test scripts, and pytest is a
python tool used for running a test suite)

The above test run (execution of tests) implicitly uses the default
installation of AFNI contained in the PATH variable. This might have issues.
Most likely if you set the environment variable PYTHONPATH to the directory
containing the afni binaries, it should work though.

Explicitly defining the installation directory ~2~

    {examples['specifying the installation directory']}

The above will make sure that the intended version of AFNI programs are found
(including the python libraries contained in afnipy).

Making use of the cmake build system ~2~

In order to facilitate the iterative process of changing code and running
tests you can run the tests directly on the executables/scripts in the
build/source trees. When you run the following it will initially take a long
time because it needs to build the full suite of AFNI tools. Subsquently,
minimal rebuilds occur: i.e. any changes in the source tree will trigger the
appropriate rebuilding but otherwise no rebuilding is required.

    {examples["making use of cmake"]}

The above defines an empty directory as the build directory. The cmake tool
will write its output to this directory. This output consists of the output of
the three stages of cmake execution: configure time, generate time, build
time.

In brief, configure time is when cmake detects the details of your system in
order to adapt its output appropriately, generate time is when cmake actually
generates a build system (this can be a make, ninja, or some other build
system), and finally build time makes use of the aforementioned build system
to generate the projects executables.


Important: It's pytest under the hood ~1~

While this wrapper script interacts with the build systems and facilitates
testing with containers it is ultimately just a wrapper around the pytest tool
that is used to execute AFNI's tests. Keeping this in mind will greatly aid
you in customizing test runs. Displaying the help for the pytest tool (including the
options specifically defined for AFNI in the "custom-options section) can be
done by running:

    {examples['help for pytest']}

In the above call for help you will hopefully notice that passing arguments to
pytest is done by using a quoted string to the --extra-args option. All
options that you might think to pass are displayed in pytest's help output.
Just as there is a custom options section for AFNI, other plugins modify the
available options for the pytest tool. The next example shows this.


Tuning resource usage ~2~

The pytest-parallel package is a plugin that works with pytest to enable
parallel execution of the test suite. If installed then the following pattern
can be used to tune the resources used for testing:

    {examples['tuning resources']}

Assuming you just want to use every core the following allows you to easily use
all cores for your testing.

    {examples['using all cores']}


Subsetting the tests that are executed during a test run ~2~

There are different ways of tuning the tests that are executed. The most basic
is to specify the exact test file that should be run. The file path should be
specified relative to the tests directory in the AFNI repo:

    {examples['execute tests by filename']}

Another way of subsetting the tests executed is to use the -k flag of the pytest tool:

    {examples['execute tests by pattern']}

For convenience this functionality can be accessed directly by the -k flag of run_afni_tests.py

    {examples['execute tests by pattern shortcut']}

Another nice trick worth knowing, is that you can re-execute all failed tests from the last test run:

    {examples['execute tests by previous failure']}


Manually specifying pytest arguments ~2~

You can drop all behavior that is enforced by run_afni_tests.py and specify
your own pytest options:

    {examples['specifying pytest args without run_afni_tests.py defaults']}


Modifying the reporting of a tests run ~1~

Altering test reporting verbosity ~2~

Verbosity at the terminal can be altered as described below but remember that
for any given test the stdout, stderr, as well as the context for the executed
are all recorded in the captured_output subdirectory for each test.

You can alter the verbosity of the test output:

    {examples['be verbose']}

This can be increased a few levels of verbosity:

    {examples['be especially verbose']}


Debugging failing tests ~1~

Using more verbose options can provide shell commands that can be executed
outside of the test infrastructure for inspection of the failure but be careful
about pre-existing paths! Additionally it is worth noting that there are some
python debugging tools that can help. For example the --debug option will drop
you into a pdb console (python debugger console) at the failure giving you
access to the full call stack. This can be very useful for figuring what went
wrong and where:

    # Note that this does not work in combination with the container subcommand
    {examples['use debugger']}

The above command makes use of the --pdb pytest option under the hood. --trace
will allow you to step line by line through tests. It might be useful for your
purposes...


Performing testing in a container: ~1~

Encapsulating all build/testing dependencies ~2~

    {examples['most basic example for container']}


"""


# EXAMPLES = eval("f'{}'".format(EXAMPLES))
