#! /bin/bash
git clone https://github.com/nih-fmrif/afni_test_data /usr/afni_build_dir/tests/pytest_tests/afni_test_data
curl -fL https://afni.nimh.nih.gov/pub/dist/edu/data/AFNI_data6.tgz | tar xzC /usr/afni_build_dir/tests/pytest_tests/afni_test_data
cd /usr/afni_build_dir/tests
pytest
afni_system_check.py -check_all
cd /usr/afni_build_dir/src/afni_src
gcov *.c
coverage
