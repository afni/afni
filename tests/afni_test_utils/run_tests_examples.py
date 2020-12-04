# These arguments are parsed as part of tests. The last line of each example
## should be a valid call to run_afni_tests.py
examples = {
    "most basic example": "run_afni_tests.py local",
    "specifying the installation directory": "run_afni_tests.py --abin=~/abin local",
    "help for pytest": "run_afni_tests.py --extra-args='--help' local",
    "altering behavior of pytest": "run_afni_tests.py --extra-args='-k mask' local",
    "making use of cmake": "\n    ".join(
        [
            "mkdir /tmp/build",
            "run_afni_tests.py --build-dir=/tmp/build local",
        ]
    ),
    "tuning resources": "run_afni_tests.py --extra-args='-n=4' local",
    "using all cores": "run_afni_tests.py --use-all-cores local",
    "execute tests by filename": "run_afni_tests.py --file scripts/test_ptaylor.py local",
    "execute tests by pattern": "run_afni_tests.py --extra-args='-k mask' local",
    "execute tests by pattern shortcut": "run_afni_tests.py -k mask local",
    "execute tests by previous failures": "run_afni_tests.py --lf local",
    "specifying pytest args without run_afni_tests.py defaults": "run_afni_tests.py --overwrite-args='--verbose' local",
    "be verbose": "run_afni_tests.py -v verbose local",
    "be especially verbose": "run_afni_tests.py -v diarrhetic local",
    "use debugger": "run_afni_tests.py --debug local",
    "most basic example for container": "run_afni_tests.py container",
    "container with source and build dirs mounted": "run_afni_tests.py --build-dir /path/to/existing/directory container --source-mode=host",
    "container reusing build dir": "run_afni_tests.py container --reuse-build",
    "container reusing test dir": "run_afni_tests.py container --source-mode=test-code",
    "help with installation": "./run_afni_tests.py --installation-help",
    "examples with explanation ": "./run_afni_tests.py examples --verbose",
}

EXAMPLES = f"""
## Basic examples

### Simplest

    {examples['most basic example']}

Under the hood this is running something like:
"cd /path/to/tests/directory
pytest scripts --tb=no --no-summary --show-capture=no"
(where scripts is a directory containing python test scripts, and pytest is a
python tool used for running a test suite)

The above test run (by this I mean 'execution of tests') implicitly uses the
AFNI software found
first on the PATH.

### Install dir: Explicitly defining the installation directory

    {examples['specifying the installation directory']}

The above will make sure that the intended version of AFNI programs are found
(including the python libraries contained in the afnipy subdirectory). It will
raise an error if the
afnipy package is installed into your current interpreter.

### CMake: Making use of the cmake build system

In order to facilitate the iterative process of changing code and running
tests you can run the tests directly on the executables/scripts in the
build/source trees. When you run the following it will initially take a long
time because it needs to build the full suite of AFNI tools. Subsquently,
minimal rebuilds occur: i.e. any changes in the source tree will trigger a
rebuild only of binaries that are affected by the change (or at least that is
the hope).

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


## pytest: Important! It's pytest under the hood

While this wrapper script interacts with the build systems and facilitates
testing with containers it is ultimately just a wrapper around the pytest tool
that is used to execute AFNI's tests. Keeping this in mind will greatly aid
you in customizing test runs. Displaying the help for the pytest tool
(including the options specifically defined for AFNI in the "custom-options"
section) can be done by running:

    {examples['help for pytest']}

In the above call for help you will hopefully notice that passing arguments to
pytest is done by using a quoted string to the --extra-args option. All
options that you might think to pass are displayed in pytest's help output.
Just as there is a custom options section for AFNI, other plugins modify the
available options for the pytest tool. The next example shows this.

### Resources: Tuning resource usage

The pytest-parallel package is a plugin that works with pytest to enable
parallel execution of the test suite. If installed then the following pattern
can be used to tune the resources used for testing:

    {examples['tuning resources']}

Assuming you just want to use every core the following convenience of
run_afni_tests.py allows you do this.

    {examples['using all cores']}

### Test selection: Subsetting the tests that are executed

There are different ways of tuning the tests that are executed. The most basic
is to specify the exact test file that should be run. The file path should be
specified relative to the tests directory in the AFNI repo:

    {examples['execute tests by filename']}

Another way of subsetting the tests executed is to use the -k flag of the
pytest tool:

    {examples['execute tests by pattern']}

For convenience this functionality can be accessed directly by the -k flag of
run_afni_tests.py

    {examples['execute tests by pattern shortcut']}

Another nice trick worth knowing  is that you can re-execute all failed tests
from the last test run (like -k, it is an option of pytest and
run_afni_tests.py):

    {examples['execute tests by previous failures']}

### Manual pytest: Manually specifying pytest arguments

You can drop all behavior that is enforced by run_afni_tests.py and specify
your own pytest options:

    {examples['specifying pytest args without run_afni_tests.py defaults']}


## Reporting: Modifying the reporting of a tests run

### Verbosity: Altering test reporting verbosity

Verbosity at the terminal can be altered as described below but remember that
for any given test the stdout, stderr, as well as the context for the executed
are all recorded in the captured_output subdirectory for each test.

You can alter the verbosity of the test output:

    {examples['be verbose']}

This can be increased a few levels of verbosity:

    {examples['be especially verbose']}


## Debugging: When you have failing tests

There are some python debugging tools that can be useful for exploring test
failures. For example the --debug option will drop you into a pdb console
(python debugger console) at the test failure giving you access to the full
call stack. This can be very useful for figuring what went wrong and where:

    # Note that --debug does not work in combination with the container subcommand
    {examples['use debugger']}

The above command makes use of the --pdb pytest option under the hood.
Additionally --trace will allow you to step line by line through tests. It
might be useful for your purposes...

### Shell: Debugging using the shell

For many of the tests in the suite, the actual test boils down to the
execution of a simple shell command (with some subsequent comparisons with
known expected output). In these situations it can be useful to take a step
out off pytest/run_afni_tests.py and reproduce the problem using a simple
shell command. There are various bits of trickiness that run_afni_tests.py and
the pytest configuration attempts to avoid for the user so this will not
always work but the basic approach to doing this is:

1. Run the command in verbose mode. It is a good idea to try to be specific
   here. Use the approaches in a previous section on subsetting the tests that are
   executed to reduce the amount of relevant output. For our purposes let's
   pretend we have a single test that is failing and we can rerun only that
   using the --lf selector for last failed tests.

    run_afni_tests.py --lf -vvvvv local

2. You may have to modify the PATH to reproduce the test. For example, when
   using the --build-dir option, you will have to modify the PATH variable
   according to what is printed to stdout during typical execution with
   run_afni_tests.py.

3. Search for the command executed. This is logged prior to execution. It will
   start with a command to cd into the tests directory.

4. Consider whether output already exists. You may want to create a new output
   directory to avoid issues here. Or just remove files that have been
   generated already by the test execution that failed.


## Using docker for environment

This provides immmense advantages including:

+ encapsulation of almost all dependencies (besides docker itself, python 3,
  and the python docker package)
+ reliable comparisons between linux system and MacOS
+ having independence between the test run and previous activity

It's usage can be a little confusing. There is a lot going on... reading this
section to familiarize yourself with the higher level details will be
extremely helpful.

### Local dirs: Host source and build directory

This is ideal. The source and build directory on your local host are used
within the container. It means that small changes to the source will trigger
minimal rebuilds within the container because the build directory timestamps
will be closer to realtime (as opposed to when the image was originally
built). Caveats here are that you need to initially mount an empty build
directory (not one from a build in the local environment) and that on OSX
using this configuration will take a substantial performance hit due to IO on
mounted volumes. You should only suffer the first time though. After that,
minimal rebuilds will make this issue less significant.

    {examples['container with source and build dirs mounted']}

### Pre-existing: Reuse a build in the container.

This will reuse the build directory in the container to build everything from
the source on the local host. It will be slow because  likely a substantial
rebuilding of the binaries will occur. For a once off test, it allows you to
test your most recent source tree inside a container though, and you may still
get some speed up from using the pre-existing build (compared to the previous
configuration described).

    {examples['container reusing build dir']}

### Fully contained: Build/test the image locally

This is a nice sanity check that your current code passes tests independent of
your current environment. To do this you would run the
following from the root directory in the afni git repository:

    docker build -f .docker/afni_dev_base.dockerfile -t afni/afni_dev_base .
    docker build -f .docker/cmake_build.dockerfile -t my_image .
    run_afni_test.py container --image-name my_image

### Mount tests: Running current test suite in pre-existing container

This avoids permissions/performance issues but still allows for some
flexibility. It is useful for iterating on test code while testing the
binaries that were installed into the container.

    {examples['container reusing test dir']}

## Getting help with the setup for testing

The readme in the tests directory for setting up your environment for testing
can be viewed with the following command:

    {examples['help with installation']}

"""


## EXAMPLES = eval("f'{}'".format(EXAMPLES))
