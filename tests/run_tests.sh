#!/bin/bash
# For the most robust results this script is used for continuous integration
# testing and should be executed within the developer docker container (see
# .circleci/config.yml)

# Exit on any errors.
set -e

# Configure testing parallelism
export NCPUS=$(getconf _NPROCESSORS_ONLN)
if [ ! -z "$USE_ALL_CORES" ];then
    # this requires pytest-parallel to work
    echo Will run tests with $NCPUS workers
    WORKERS_FLAG="--workers $NCPUS"
    export OMP_NUM_THREADS=1
else
    export OMP_NUM_THREADS=$NCPUS
fi

# basic config for git for circleci, assuming everyone else has configured git...
gituser="git config user.name"
if [ ! -z "$gituser" ];then
  echo Setting a temporary git user name for testing
  git config --global user.name 'AFNI CircleCI User'
fi
gitemail="git config user.email"
if [ ! -z "$gitemail" ];then
  echo Setting a temporary git email for testing
  git config --global user.email 'johnleenimh+circlecigitconfig@gmail.com'
fi

# Check the current directory is tests
if [ ! $(basename $PWD) == 'tests' ];then
    if [ -d /opt/afni/src/tests ];then
        # We are likely in the development containers and we know where the
        # tests directory is...
        cd /opt/afni/src/tests
    else
    echo Error: You need to execute tests from the tests directory in the \
    source repository. When in the development container afni/afni_cmake_build \
    this can be accessed using the symlink /opt/afni/build/afni_tests
    exit 1
    fi
fi

# For circleci Make sure test data is checked out correctly
if [ -f afni_ci_test_data/test_data_version.txt ];then
    cd afni_ci_test_data
    git checkout $(cat test_data_version.txt)
    datalad update
    cd ..
fi

# Run tests
if [ "$AFNI_WITH_COVERAGE" != "0" ];then
    # This will run correctly if AFNI_WITH_COVERAGE is set in the environment
    # (to something other than 0) and the build has performed using the
    # appropriate build flags to enable coverage
    echo Running tests with codecov
    # Get the installation directory:
    install_dir=$(dirname $(which afni))
    pytest scripts $WORKERS_FLAG --runveryslow --cov=$install_dir --cov-report xml:$PWD/../coverage.xml
    # For debugging
    # pytest scripts  --runveryslow --cov=$install_dir --cov-report xml:$PWD/../coverage.xml --pdb
    cd ..
    codecov
else
    pytest scripts $WORKERS_FLAG --runveryslow -vv --showlocals -r Esx
fi

# For python2 there is a --testpython2 option. Note though that this is not
# supported in the testing infrastructure. For clean execution of tests the
# python interpretter needs to have various dependencies and afnipy installed.
# The minimum supported version for this is python 3.6
