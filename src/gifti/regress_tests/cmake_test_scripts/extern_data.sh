#!/bin/bash
# Currently not run because the data has not been added to the test data repo
if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`


# make a copy of b64gz.ts.3.gii, but set the data to point to
# external files (and let the write create them)

$GT                                                                           \
-infiles $DATA/b64gz.ts.3.gii                                                 \
-no_updates                                                                   \
-set_extern_filelist ext_data.ts3.{1,2,3}.bin                                 \
-write_gifti ts3.ext.gii                                                      

# test a new copy, with no data (and no errors)
$GT                                                                           \
-infiles $DATA/b64gz.ts.3.gii                                                 \
-no_updates                                                                   \
-set_extern_filelist ext_data.ts3.{1,2,3}.bin                                 \
-no_data                                                                      \
-write_gifti ts3.ext0.gii                                                     

# test the differences in data

$GT -compare_data -compare_verb 3 -infiles $DATA/b64gz.ts.3.gii ts3.ext.gii

# now copy data back into dataset
$GT                                                                           \
-infiles ts3.ext.gii                                                          \
-no_updates                                                                   \
-encoding BASE64GZIP                                                          \
-write_gifti ts3.int.gii                                                      

# test all differences

$GT                                                                           \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 3                                                               \
-infiles $DATA/b64gz.ts.3.gii ts3.int.gii                                           

rm -rf $OUT_DATA
