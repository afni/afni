#!/bin/bash

if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


max_verb=7
for level in  {1..$max_verb}
do
    output=$($GT -infile $DATA/ascii.misc.gii -no_updates -verb $level)
done 

rm -rf $OUT_DATA
