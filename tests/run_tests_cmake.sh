#!/bin/bash
# Exit on any errors.
set -e

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


# Read in paths to be added to the PATH variable. This is created by the build
# system
TESTING_PATHS="$(< TESTING_PATHS.txt)"

# Run tests
export PATH="$TESTING_PATHS:$PATH"
export ARGS="scripts --runveryslow --testpython2 -vv"
ARGS=$ARGS ninja pytest
