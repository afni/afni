#!/bin/bash
# Exit on any errors.
set -e

# Run tests
cd /opt/afni/tests
pytest --runveryslow --cov=/opt/afni/abin
# pytest --testpython2 # add this for testing python 2

# Check code coverage
cd /opt/afni
bash <(curl -s https://codecov.io/bash) -a "-r" -y /opt/afni/tests/codecov.yml
