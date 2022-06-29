#!/bin/sh

if [ $# -lt 2 ]
then
echo Missing nifti tool and Binary directory name
exit 1
fi

NT=$1
DATA=$2
OUT_DATA=$(dirname ${DATA}) #Need to write to separate directory
cd ${OUT_DATA}

# note the main input file and prefix for all output files
infile=$DATA/e4.60005.nii.gz
prefix=out.c22

rm -f $prefix*

# ------------------------------------------------------------
# tests with -copy_image and -clb, along with data conversion
# options -convert2dtype, -convert_fail_choice, -convert_verify
#
# keep simple: convert between types and test for binary match
# test with both -cbl and -copy_image


# dupe initially, to get local endian (for binary diff tests),
# and change to 4 dims, rather than 5 (time series has 5th dim)
if ${NT} -cbl -infile $infile -prefix $prefix.0.i16.nii.gz
then
echo ""
else
echo $prefix initial cbl failed
exit 1
fi

# back and forth to i64, -copy_image
${NT} -copy_image -infile ${prefix}.0.i16.nii.gz \
                  -prefix ${prefix}.1.i64.nii.gz \
                  -convert2dtype NIFTI_TYPE_INT64 -convert_verify
${NT} -copy_image -infile ${prefix}.1.i64.nii.gz   \
                  -prefix ${prefix}.2.0.i16.nii.gz \
                  -convert2dtype NIFTI_TYPE_INT16 -convert_verify
if cmp ${prefix}.0.i16.nii.gz ${prefix}.2.0.i16.nii.gz
then
echo ""
else
echo "** not a binary match: ${prefix}.0.i16.nii.gz ${prefix}.2.0.i16.nii.gz"
exit 1
fi

# back and forth to f32, -cbl
${NT} -cbl -infile ${prefix}.0.i16.nii.gz \
           -prefix ${prefix}.1.f32.nii.gz \
           -convert2dtype NIFTI_TYPE_FLOAT32 -convert_fail_choice fail
${NT} -copy_image -infile ${prefix}.1.f32.nii.gz   \
                  -prefix ${prefix}.2.1.i16.nii.gz \
                  -convert2dtype NIFTI_TYPE_INT16 -convert_fail_choice warn
if cmp ${prefix}.0.i16.nii.gz ${prefix}.2.1.i16.nii.gz
then
echo ""
else
echo "** not a binary match: ${prefix}.0.i16.nii.gz ${prefix}.2.1.i16.nii.gz"
fi

