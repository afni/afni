#!/bin/bash
# Exit on any errors.
set -e

# Run tests
pytest --runveryslow --cov=/opt/afni/abin
# pytest --testpython2 # add this for testing python 2
cd ../src

# Check code coverage
codecov -y /opt/afni/tests/codecov.yml
