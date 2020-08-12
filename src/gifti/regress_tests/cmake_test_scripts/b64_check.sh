#!/bin/bash
set -eu
if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


# The following should remove bad b64 chars.
for check in  NONE DETECT COUNT SKIP SKIPnCOUNT
do

    $GT                                                                       \
    -infile $DATA/b64gz.ts.3.errors.gii                                       \
    -b64_check $check                                                         \
    -no_updates                                                               \
    -write_gifti $OUT_DATA/errs.gii                                           
done

# The above loop reports errors but writes output which should differ:
# command should raise a nonzero exit code
! output=$(cmp $DATA/b64gz.ts.3.gii $OUT_DATA/errs.gii 2>&1)
# output should contain the word differ
echo $output | grep  'differ' 

rm -rf $OUT_DATA

