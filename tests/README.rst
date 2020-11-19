Running tests (Overview)
========================

At its core, the tests suite for AFNI is a set of python files,
contained in the tests/scripts subdirectory, that each contain functions
that start with the set of characters “test\_”. With the appropriate
environment set up, you can run them by executing the following command
from the tests directory:

::

   pytest

There are lots of way in which we may want to run the tests though. We
may want to run them in a docker container (with or without the local
source repository mounted). We may want to test a recent build rather
than the system installed version of AFNI. There is a run_afni_tests.py
program that is provided in order to handle all of these details and let
you, the developer, get on with what you love most… writing and running
tests!

Setup for running tests
=======================

The following sections describe the installation of dependencies to
avail of increasing levels of functionality wrapped by the
run_afni_tests.py helper.

Quickest and minimal setup
--------------------------

1. Install `conda <https://docs.conda.io/projects/conda/en/latest/user-guide/install/>`__

2. Execute the following commands:

::

   # Install conda first!
   git clone https://github.com/afni/afni.git
   cd afni/tests
   conda env create -f environment.yml
   conda activate afni_dev

This will install the dependencies required for basic testing into a
conda environment and then activates the environment. Each time you wish
to run the tests on a fresh terminal you must activate this environment.

**N.B.** You cannot set the environment variable PYTHONPATH when using this
tool

Setup for containerized execution
---------------------------------

These instructions must be followed if you wish to use the container
subcommand of the run_afni_tests.py tool.

1. Complete the setup steps listed in the previous  section for a minimal setup.
2. Install the `docker <https://docs.docker.com/get-docker/>`__ software

Running tests against your own AFNI build
-----------------------------------------

1. You must have already completed the steps in the previous minimal setup
   instructions.

2. You will need to have the system dependencies installed. Consider following the
   `installation
   instructions <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/install_instructs/index.html>`__
   for AFNI now.

3. You will either need to follow the instructions option 'a' or 'b'.

a) If you are using the –build option you will need to install afnipy into your current
   python interpreter. From the base directory of AFNI source code:

::

   pip install src/python_scripts

b) If you are testing a system installed AFNI or testing a build directory
   when using the --abin flag you cannot have afnipy installed. You will
   encounter an error if this is the case. It is easily resolved:

::

   pip uninstall afnipy

**N.B.** AFNI also has a build that uses CMake for improved cross-platform
compatibility. It has various other advantages. You may wish to check it out
`here <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/index.html>`__


Other help
----------

A lot of documentation is embedded in the run_afni_tests.py tool itself.
If you have python3 installed consider running the following from the
tests directory:

::

   ./run_afni_tests.py -h
   #or
   ./run_afni_tests.py examples -v

Alternatively if you have created and activated the conda environment in
the minimal setup step you can simply run:

::

   run_afni_tests.py -h
