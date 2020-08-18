#!/bin/bash
if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


# create a new dataset, and modify some things
$GT                                                                           \
-infile MAKE_IM                                                               \
-new_numDA 3                                                                  \
-new_intent NIFTI_INTENT_TTEST                                                \
-new_dtype NIFTI_TYPE_INT16                                                   \
-new_ndim 1                                                                   \
-new_dims 11 0 0 0 0 0                                                        \
-mod_add_data                                                                 \
-mod_DA_atr  Dim0 17                                                          \
-mod_DA_atr  Intent NIFTI_INTENT_FTEST                                        \
-mod_DA_meta  Name 'sub-brick #42 from pickle dataset'                        \
-mod_gim_atr Version 0.0.11                                                   \
-mod_gim_meta Date "`date`"                                                   \
-mod_gim_meta Description "gifti_tool test surface"                           \
-write_gifti $OUT_DATA/mt0.gii                                                

# fix descriptions for indices 1 and 2 (leave 0)
$GT                                                                           \
-mod_DAs 1                                                                    \
-infile $OUT_DATA/mt0.gii                                                     \
-mod_DA_meta Name 'sub-brick #43 from pickle dataset'                         \
-write_gifti $OUT_DATA/mt1.gii                                                

$GT                                                                           \
-mod_DAs 2                                                                    \
-infile $OUT_DATA/mt1.gii                                                     \
-mod_DA_meta Name 'sub-brick #44 from pickle dataset'                         \
-write_gifti $OUT_DATA/mod_test_3.gii                                         

$GT                                                                           \
-infile $DATA/ascii.misc.gii                                                  \
-no_updates                                                                   \
-write_gifti $OUT_DATA/copy.misc.gii                                          

# show some output
# $GT -verb 2 -infile $OUT_DATA/mod_test_3.gii -show_gifti 
# && $GT -verb 2 -infile $OUT_DATA/mod_test_3.gii -write_1D mod_test.1D

# try converting some dataset to type float
$GT                                                                           \
-infile $OUT_DATA/copy.misc.gii                                               \
-mod_to_float                                                                 \
-write_gifti $OUT_DATA/float.gii                                              

grep -q DataType $OUT_DATA/float.gii

# # make very simple dataset for comparison
$GT                                                                           \
-infile MAKE_IM                                                               \
-new_numDA 3                                                                  \
-new_intent NIFTI_INTENT_TTEST                                                \
-new_dtype NIFTI_TYPE_INT16                                                   \
-new_ndim 1                                                                   \
-new_dims 17 0 0 0 0 0                                                        \
-write_gifti $OUT_DATA/comp_test0.gii                                         


# # compare it to more complex one
# Command should return nonzero exit status and print that 3 diffs were found
! output=$(                                                                   \
$GT                                                                           \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 3                                                               \
-infiles $OUT_DATA/mt0.gii $OUT_DATA/comp_test0.gii                           \
2>&1)

echo $output | grep -q 'found data diffs in 3 DataArrays'

# other comparison
# command should return nonzero exit status and print EXPECTED_OUT
EXPECTED_OUT='no data differences between gifti_images'
! output=$(                                                                   \
$GT                                                                           \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 3                                                               \
-infiles $OUT_DATA/mt0.gii $OUT_DATA/mod_test_3.gii                           
2>&1)

echo $output | grep -q "$EXPECTED_OUT"

# Cleanup after test
rm -rf $OUT_DATA

