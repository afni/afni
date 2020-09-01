#!/bin/bash
# Exit on any errors.
set -e
export NCPUS=$(getconf _NPROCESSORS_ONLN)
export OMP_NUM_THREADS=$NCPUS

# basic config for git
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

# Make sure test data is checked out correctly
cd /opt/afni/src/tests/afni_ci_test_data
if [ ! -d .git ];then
    mv test_data_version.txt ..
    cd ..
    datalad install https://github.com/afni/afni_ci_test_data.git
    cd afni_ci_test_data
    mv ../test_data_version.txt .
else
    git fetch origin
fi
git checkout $(cat test_data_version.txt)


# Run tests
cd /opt/afni/src/tests
pytest --runveryslow -v -r Esx
# For coverage
# pytest --runveryslow --cov=/opt/afni/src/abin --cov-report xml:/opt/afni/src/coverage.xml --workers $NCPUS
# cd /opt/afni/src
# codecov

# For python2 there is a --testpython2 option. Note though that this is not
# supported in the testing infrastructure. For clean execution of tests the
# python interpretter needs to have various dependencies and afnipy installed.
# The minimum supported version for this is python 3.6
