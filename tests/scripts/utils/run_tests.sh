#!/bin/bash

# Exit on any errors.
set -e

# Get afni root directory
POSSIBLE_SCRIPT_DIR="${BASH_SOURCE%/*}" || exit
if [ $POSSIBLE_SCRIPT_DIR == run_tests.sh ];then
    POSSIBLE_SCRIPT_DIR=$PWD
fi
echo Test script directory used: $POSSIBLE_SCRIPT_DIR



source $POSSIBLE_SCRIPT_DIR/download_test_data.sh $AFNI_TEST_DATA_PATH

if [ ! -z ${VERBOSE_STR+x} ]; then
    echo Running verbosely
    set -x
fi

if [ ! -z ${ONLY_DOWNLOADING+x} ]; then
    echo Data present. No tests being run
    exit 0
fi
# Run tests.
cd $(dirname $AFNI_TEST_DATA_PATH)
AFNI_ROOT=$(realpath "$POSSIBLE_SCRIPT_DIR/..")

pytest $DEBUG_STR $VERBOSE_STR $POSSIBLE_SCRIPT_DIR -k proc # --workers 4


cd $AFNI_ROOT/src
gcov *.c
codecov
