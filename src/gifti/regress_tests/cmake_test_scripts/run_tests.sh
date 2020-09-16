#!/bin/bash
set -e

HELP='
Usage: run_tests.sh [-v verbose] <path_to_gifti_tool> <path_to_gifti_test_data>

The test-data can be downloaded using the command:
git clone https://github.com/NIFTI-Imaging/gifti-test-data.git

Note: You can build this project and run all tests from an empty directory using the command:
cmake -DTEST_GIFTI=ON <path_to_local_gifti_clib_repository>;cmake --build .;ctest
'

# Parse verbose flag
VERBOSE=false
while getopts ":v" option;do
    case "${option}" in
        v ) VERBOSE=true
        ;;
        \? ) echo "Usage: cmd [-h] [-t]"
esac
done
shift $((OPTIND -1))

# Check argument number
if [ $# != 2 ]; then
    echo "$HELP"
    exit 1
fi

POSSIBLE_SCRIPT_DIR="${BASH_SOURCE%/*}" || exit
if [ $POSSIBLE_SCRIPT_DIR == run_tests.sh ];then
    POSSIBLE_SCRIPT_DIR=$PWD
fi

for script in $(ls ${POSSIBLE_SCRIPT_DIR}/*.sh)
do
    [[ $script == "${POSSIBLE_SCRIPT_DIR}/run_tests.sh" ]] && continue
    echo Running $script
    if "$VERBOSE";then
        bash $script $1 $2
    else 
        bash $script $1 $2 2>&1 > /dev/null 
    fi
done

echo Tests finished successfully

