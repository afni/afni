#!/bin/bash
usage=" run_test.sh [-h] [-v verbose] [-d debug] [-D download][path_to_dir_containing_test_data]
This script will run tests and write output to a sub-directory of
'path_to_dir_containing_test_data'. It uses test data that must either be
contained in this directory or downloaded to it if it does not already exist.
If 'path_to_dir_containing_test_data' is not provided as an argument the
current directory is used. If -D is specified the tests are not run.
"
# Exit on any errors.
set -e

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

# Initialize our own variables:
START_DIR=$PWD
DEBUG_STR=""

# Parse arguments and options
while getopts "h?vdD" opt; do
    case "$opt" in
    h|\?)
        echo $usage
        exit 0
        ;;
    v)  VERBOSE_STR="-v"
        ;;
    D)  ONLY_DOWNLOADING=1
        ;;
    d)  DEBUG_STR="--pdb"
        ;;
    esac
done

shift $((OPTIND-1))

# [ "${1:-}" = "--" ] && shift

# Exit if more than one positional argument:
if [ "$#" -gt 1 ]; then
    echo "More than one argument provided..."
    echo "$usage"
    exit 1
fi
# If provided use the argument
if [ $# == 1 ];then
    path_to_dir_containing_test_data="$1"
else
    path_to_dir_containing_test_data=${START_DIR}
fi
    AFNI_TEST_DATA_PATH="${path_to_dir_containing_test_data}/afni_test_data"
    echo "AFNI test data path: $AFNI_TEST_DATA_PATH"

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
