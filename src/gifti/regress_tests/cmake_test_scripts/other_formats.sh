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
-verb 3                                                                       \
-write_asc $OUT_DATA/pial_short.asc                                           


 $GT                                                                          \
 -infile $DATA/b64gz.ts.3.gii                                                 \
 -verb 3                                                                      \
 -write_1D $OUT_DATA/ts3.1D.                                                  


 rm -rf $OUT_DATA
