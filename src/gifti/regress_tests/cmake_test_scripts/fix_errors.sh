#!/bin/bash
# File uses tests from c17 and c20
if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`

$GT                                                                           \
-infile $DATA/b64gz.ts.3.gii                                                  \
-gifti_test                                                                   


$GT                                                                           \
-infile $DATA/b64gz.ts.3.gii                                                  \
-gifti_test                                                                   \
-mod_DA_meta tagStudyMetaDataLinkSet SOME_STUDY_METADATA                      \
-no_updates                                                                   \
-write_gifti $OUT_DATA/ts3.fixed.gii                                          

# Command output should contain SOME_STUDY
diff $DATA/b64gz.ts.3.gii  $OUT_DATA/ts3.fixed.gii | grep -q SOME_STUDY


# Generate some files for update comparison
$GT                                                                           \
-infile $OUT_DATA/ts3.fixed.gii                                               \
-no_updates                                                                   \
-write_gifti $OUT_DATA/update.none.ts3.gii                                    

$GT                                                                           \
-infile $OUT_DATA/ts3.fixed.gii                                               \
-write_gifti $OUT_DATA/update.ts3.gii                                         

# Command should run with no error and give no output.
output=$(diff $OUT_DATA/ts3.fixed.gii $OUT_DATA/update.none.ts3.gii 2>&1)
if [[ ! -z "$output" ]];then exit 1;fi


# Command should run with nonzero exit status and provide text containing gifticlib-version
EXPECTED_OUT='gifticlib-version'
! output=$(diff $OUT_DATA/ts3.fixed.gii $OUT_DATA/update.ts3.gii 2>&1)
echo $output | grep -q ${EXPECTED_OUT}

#Cleanup after testing
rm -rf $OUT_DATA
