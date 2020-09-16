#!/bin/bash

if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


for indent in  0 3 8
do
    $GT                                                                       \
    -infile $DATA/b64gz.ts.3.gii                                              \
    -indent $indent                                                           \
    -no_updates                                                               \
    -no_data                                                                  \
    -write_gifti $OUT_DATA/new_$indent.gii                                    
done 

rm -rf $OUT_DATA
