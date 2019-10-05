#!/bin/bash
usage=" run_test.sh [-h] [-v verbose] [-d debug] [path_to_dir_containing_test_data] This
script will run tests and write output to a sub-directory of
'path_to_dir_containing_test_data' using the test data that will be
downloaded, if it does not already exist, to that same directory. If
'path_to_dir_containing_test_data' is not provided as an argument the current
directory is used.
"
# Exit on any errors.
set -e

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

# Initialize our own variables:
START_DIR=$PWD
VERBOSE_STR=""
DEBUG_STR=""

# Parse arguments and options
while getopts "h?vd" opt; do
    case "$opt" in
    h|\?)
        echo $usage
        exit 0
        ;;
    v)  VERBOSE_STR="-v"
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


##### Add functionality to implement readlink -f in a more general way as described here:
# https://stackoverflow.com/questions/1055671/how-can-i-get-the-behavior-of-gnus-readlink-f-on-a-mac
realpath() {
    canonicalize_path "$(resolve_symlinks "$1")"
}

resolve_symlinks() {
    local dir_context path
    path=$(readlink -- "$1")
    if [ $? -eq 0 ]; then
        dir_context=$(dirname -- "$1")
        resolve_symlinks "$(_prepend_path_if_relative "$dir_context" "$path")"
    else
        printf '%s\n' "$1"
    fi
}

_prepend_path_if_relative() {
    case "$2" in
        /* ) printf '%s\n' "$2" ;;
         * ) printf '%s\n' "$1/$2" ;;
    esac 
}

canonicalize_path() {
    if [ -d "$1" ]; then
        _canonicalize_dir_path "$1"
    else
        _canonicalize_file_path "$1"
    fi
}   

_canonicalize_dir_path() {
    (cd "$1" 2>/dev/null && pwd -P) 
}           

_canonicalize_file_path() {
    local dir file
    dir=$(dirname -- "$1")
    file=$(basename -- "$1")
    (cd "$dir" 2>/dev/null && printf '%s/%s\n' "$(pwd -P)" "$file")
}
####################

# Get afni root directory
POSSIBLE_SCRIPT_DIR="${BASH_SOURCE%/*}" || exit
if [ $POSSIBLE_SCRIPT_DIR == run_tests.sh ];then
    POSSIBLE_SCRIPT_DIR=$PWD
fi
echo $POSSIBLE_SCRIPT_DIR
AFNI_ROOT=$(realpath "$POSSIBLE_SCRIPT_DIR/..")
# echo $AFNI_ROOT


# Download data.
if [ ! -d $AFNI_TEST_DATA_PATH ]; then
  git clone https://github.com/nih-fmrif/afni_test_data $AFNI_TEST_DATA_PATH
fi
if [ ! -d $AFNI_TEST_DATA_PATH/AFNI_data6 ]; then
  echo "Downloading AFNI data 6 ..."
  curl -fsSL https://afni.nimh.nih.gov/pub/dist/edu/data/AFNI_data6.tgz | tar xzC $AFNI_TEST_DATA_PATH
fi

# Run tests.
# cd $here  # Just in case test script is run from some other directory.
if [ -z ${VERBOSE_STR+x} ]; then
    set -x
fi

pytest $DEBUG_STR $VERBOSE_STR

afni_system_check.py -check_all
cd $AFNI_ROOT/src
gcov *.c
codecov
