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


f1=$DATA/good.color.gii
f2=good.2.gii

# test: missing DA
head -121 $f1 | sed 's/NumberOfDataArrays=\"3/NumberOfDataArrays=\"2/' > $f2
echo '</GIFTI>' >> $f2

! output=$($GT -approx_gifti -infiles $f1 $f2 -compare_verb 3 2>&1)
echo $output | grep -q 'differ in numDA'

# test: different endian
sed 's/LittleEndian/BigEndian/' $f1 > $f2
$GT -approx_gifti -infiles $f1 $f2 -compare_verb 3

# test: remove second label
head -24 $f1 > $f2
tail -126 $f1 >> $f2
! output=$($GT -approx_gifti -infiles $f1 $f2 -compare_verb 3 2>&1)
echo $output | grep -q 'gifti labeltables not approx. equal'

# test: modify label 0, key 1, green 2
sed 's/PickleCenter/Pickles/' $f1 | sed 's/\"23/\"24/'  | sed 's/Green=\"0\"/Green=\"0.1\"/' > $f2

! output=$($GT -approx_gifti -infiles $f1 $f2 -compare_verb 3 2>&1)
echo $output | grep -q 'gifti labeltables not approx. equal'
echo $output | grep -q 'labeltable Key diff'

# test: modify Transform and space
sed 's/NIFTI_XFORM_TALAIRACH/NIFTI_XFORM_MNI_152/' $f1  \
    | sed 's/0 0.000000 0/0 1.010000 0/' > $f2
! output=$($GT -approx_gifti -infiles $f1 $f2 -compare_verb 3 2>&1)
echo $output | grep -q 'dspace diff: NIFTI_XFORM_TALAIRACH vs. NIFTI_XFORM_MNI_152'
echo $output | grep -q 'diff in DA coordsys'

# test: modify POINTSET and TRIANGE data
sed 's/-65.592651/-65.6/' $f1 | sed 's/141604/141605/' > $f2
! output=$($GT -approx_gifti -infiles $f1 $f2 -compare_verb 3 2>&1)
echo $output | grep -q 'triange diff'
echo $output | grep -q 'not approximately equal'

rm -rf $OUT_DATA