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
-infile $DATA/b64gz.ts.3.gii                                                  \
-read_DAs 2 0 0 1                                                             \
-write_gifti $OUT_DATA/b64gz.ts.4.gii                                         \
-verb 2                                                                       \
-no_updates                                                                   


$GT                                                                           \
-infile $DATA/b64gz.ts.3.gii'[2,0,0,1]'                                       \
-write_gifti $OUT_DATA/b64gz.ts.4.2.gii                                       \
-verb 2                                                                       \
-no_updates                                                                   



rm -rf $OUT_DATA

