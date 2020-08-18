#!/bin/bash

if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`

$GT                                                                           \
-infile $DATA/ascii.pial.short.gii                                            \
-no_data                                                                      \
-no_updates                                                                   \
-write_gifti $OUT_DATA/new.gii                                                

cmp $DATA/ascii.pial.no_data.gii $OUT_DATA/new.gii 

rm -rf $OUT_DATA

