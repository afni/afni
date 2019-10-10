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
# prefix=out.c21.d

# rm -f $prefix*

# --------------------------------------------------
# display basic nifti info stuff from existing dset

if $NT -run_misc_tests -debug 2 -infiles ${infile}
then
echo "=== run_misc_tests succeeded"
else
echo === run_misc_tests failed
exit 1
fi

