#!/bin/bash

#This script is used for continuous integration testing and should be executed
# within the developer docker container (see .circleci/config.yml)

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
git checkout $(cat test_data_version.txt)
datalad update

# Run tests
cd /opt/afni/src/tests
pytest scripts --runveryslow -vv --showlocals -r Esx
