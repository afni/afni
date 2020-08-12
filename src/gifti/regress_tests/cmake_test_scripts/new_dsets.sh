#!/bin/bash

if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


# All commands should run:
$GT -infile MAKE_IM -write_gifti defaults.gii        

$GT                                                                           \
-infile MAKE_IM                                                               \
-new_data                                                                     \
-new_dims 11 0 0 0 0 0                                                        \
-new_dtype NIFTI_TYPE_INT16                                                   \
-new_intent NIFTI_INTENT_TTEST                                                \
-new_ndim 1                                                                   \
-new_numDA 3                                                                  \
-write_gifti create.gii                                                       

$GT                                                                           \
-infile MAKE_IM                                                               \
-new_data                                                                     \
-new_dims 11 0 0 0 0 0                                                        \
-new_dtype NIFTI_TYPE_INT16                                                   \
-new_intent NIFTI_INTENT_TTEST                                                \
-new_ndim 1                                                                   \
-new_numDA 3                                                                  \
-write_1D create.1D.                                                          


# Cleanup after tests
rm -rf $OUT_DATA
