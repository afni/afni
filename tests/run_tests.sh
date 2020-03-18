#!/bin/bash
# Exit on any errors.
set -e

# Run tests
cd /opt/src/afni/tests
pytest --runveryslow --cov=/opt/src/afni/abin --cov-report xml:/opt/src/afni/coverage.xml
# pytest --testpython2 # add this for testing python 2

# Check code coverage
cd /opt/src/afni
codecov
