#! /usr/bin/env bash
# This script used as part of AFNI's cmake build is executed from within the
# root directory of AFNI's python package

#  Get python interpretter
if [ $# -lt 1 ]
 then
     export py_interp=$(which python)
 else
     export py_interp=$1
fi

# Check script is being run from correct location
if [ ! -f setup.py ]
then
    echo This script should be run from the python_scripts directory
    exit 1
fi

# Get return code from running a check to see if afnipy imports correctly (in
# a subshell)
check_installed="cd /tmp;$py_interp -c 'import afnipy'"
$(eval "$check_installed")
export install_check=$?
# echo Install check return code: $install_check

# If the import failed, install afnipy
if [ $install_check -ne 0 ]
then
  echo Installing afnipy...
  # check if DO_NOT_USE_PIP is set:
  if [ -z ${DO_NOT_USE_PIP+x} ]
  then
   # variable not set 
    eval "$py_interp -m pip install -e ."
  else 
    eval "$py_interp setup.py develop"
  fi
fi

