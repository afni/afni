#!/bin/bash
# Exit on any errors.
set -e

# script should be run from the tests directory
if [ ! -f test.sh ];then
    exit 1
fi

# Run tests
pytest --testpython2
cd ../src

# Check code coverage
gcov *.c
codecov
