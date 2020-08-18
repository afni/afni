#!/bin/bash
if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`
cd ${OUT_DATA}

$GT                                                                           \
-infile $DATA/ascii.misc.gii                                                  \
-no_updates                                                                   \
-write_gifti $OUT_DATA/new.gii                                                


# Test command should return nonzero exit status and print EXPECTED_OUT
EXPECTED_OUT='XtraDAattribute'
! output=$(diff $DATA/ascii.misc.gii $OUT_DATA/new.gii 2>&1)
echo $output | grep -q "$EXPECTED_OUT"

# Cleanup after tests
rm -rf $OUT_DATA


