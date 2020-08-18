#!/bin/bash

if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


for size in  42 43 1000 12345 66666
do

    $GT                                                                       \
    -infile $DATA/b64gz.ts.3.gii                                              \
    -buf_size $size                                                           \
    -no_updates                                                               \
    -write_gifti $OUT_DATA/new.gii                                            

    cmp $DATA/b64gz.ts.3.gii $OUT_DATA/new.gii
    
done 

rm -rf $OUT_DATA

