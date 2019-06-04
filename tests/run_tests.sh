#!/bin/bash
# Exit on any errors.
set -e

# Run tests
pytest --runveryslow
# pytest --testpython2
cd ../src

# Check code coverage
gcov *.c
codecov
