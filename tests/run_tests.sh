#!/bin/bash
# Exit on any errors.
set -e

# Run tests
cd /opt/afni/tests
pytest --runveryslow --cov=/opt/afni/abin --cov-report xml:/opt/afni/coverage.xml
# pytest --testpython2 # add this for testing python 2

# Check code coverage
cd /opt/afni
codecov
