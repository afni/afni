# Setup for running tests

The tests directory in the afni source repository contains the code for
continuous integration testing. The tests themselves are contained in the
scripts subdirectory. These tests can be executed directly using pytest but the
run_afni_tests.py script wraps this along with other details to help make
running tests a little easier.

## Quickest and minimal setup

For a basic working setup for running these tests execute the following commands (note you need
[conda installed](https://docs.conda.io/projects/conda/en/latest/user-guide/install/)):

	git clone https://github.com/afni/afni.git
	cd afni/tests
	conda env create -f environment.yml

This will install the dependencies required for basic testing into a conda
environment. This environment can be accessed by running the following each
time you wish to use it:

	conda activate afni_dev

Additionally, if you intend to use the cmake build, AFNI's python code must be installed:

	# from the base directory of AFNI source code
	pip install src/python_scripts

AFNI's python code must NOT be installed if you intend to use the typical afni build/distribution:

	pip uninstall afnipy

 See subsequent sections for alternative setups.

## Setup for containerized execution

Install the docker software in order to use avail of testing in an isolated
container, i.e. using the container subcommand. This will supplement the
previous setup instructions but you can also skip the above environment
creation and only run tests in a container. With docker installed the only
things you need to run the tests are python > 3.6 and the docker python api
(called docker when installing with pip or docker-py when using conda). Note
that the python docker api software is installed as part of the basic setup
environment.

## Using the cmake build with testing

Using the cmake build (with the option --build-dir) is an excellent way to
execute the tests but you need to have all of AFNI's development dependencies
installed. A more in depth description of installing these can be found
[here](https://docs.google.com/document/d/1VOgukIUzNZU75WQIV-2_z1BOvz9JcOakeSPZifT5A_Q/edit#heading=h.172g37gyjtvj)
or
[here](https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/main_toc.html).
Alternatively you can try:

	git clone https://github.com/afni/afni.git
	cd afni/tests
	conda env create -f environment_full.yml

The above will attempt to encapsulate all dependencies (including compilers) in
the conda environment. This is not entirely possible for X on OSX (use homebrew
to install XQuartz and OpenMotif).


## Other help

A lot of documentation is embedded in the run_afni_tests.py tool itself. If you
have python3 installed consider running the following from the tests directory:

```
./run_afni_tests.py -h
#or
./run_afni_tests.py examples -v
```
