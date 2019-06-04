#!/bin/bash
# Exit on any errors.
set -e

# Run tests
coverage run -m pytest --runveryslow
# pytest --testpython2 # add this for testing python 2
cd ../src

# Check code coverage
gcov *.c
codecov
