#!/bin/bash
# Exit on any errors.
set -e
export NCPUS=$(getconf _NPROCESSORS_ONLN)
export OMP_NUM_THREADS=1

# Run tests
cd /opt/afni/src/tests
pytest --runveryslow -v -r Esx --workers $NCPUS
# For coverage
# pytest --runveryslow --cov=/opt/afni/src/abin --cov-report xml:/opt/afni/src/coverage.xml --workers $NCPUS
# cd /opt/afni/src
# codecov

# For python2 there is a --testpython2 option. Note though that this is not
# supported in the testing infrastructure. For clean execution of tests the
# python interpretter needs to have various dependencies and afnipy installed.
# The minimum supported version for this is python 3.6
