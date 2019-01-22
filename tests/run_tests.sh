#!/bin/bash
#
# usage: run_tests.sh

# Exit on any errors.
set -ex

here="$(dirname $(readlink -f $0))"
AFNI_ROOT="$(readlink -f $here/..)"
AFNI_TEST_DATA_PATH="$here/pytest_tests/afni_test_data"
echo "AFNI test data path: $AFNI_TEST_DATA_PATH"

# Download data.
if [ ! -d $AFNI_TEST_DATA_PATH ]; then
  git clone https://github.com/nih-fmrif/afni_test_data $AFNI_TEST_DATA_PATH
fi
if [ ! -d $AFNI_TEST_DATA_PATH/AFNI_data6 ]; then
  echo "Downloading AFNI data 6 ..."
  curl -fsSL https://afni.nimh.nih.gov/pub/dist/edu/data/AFNI_data6.tgz | tar xzC $AFNI_TEST_DATA_PATH
fi

# Run tests.
cd $here  # Just in case test script is run from some other directory.
pytest
afni_system_check.py -check_all
cd $AFNI_ROOT/src
gcov *.c
codecov
