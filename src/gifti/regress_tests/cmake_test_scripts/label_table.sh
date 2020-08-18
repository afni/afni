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


f1=$DATA/LT.Index.gii
f2=$OUT_DATA/LT.Key.gii

# read Index and write Key
$GT -infile $f1 -no_updates -write_gifti $f2 -verb 4

# compare_gifti
$GT -compare_gifti -compare_data -compare_verb 3 -infiles $f1 $f2

# diff
diff $f1 $f2 | grep -q 'Label Key="17"'
diff $f1 $f2 | grep -q 'Label Index="17"'

rm -rf $OUT_DATA