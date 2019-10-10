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
infile=$DATA/Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii
prefix=out.c21.b

rm -f $prefix*

# --------------------------------------------------
# display basic nifti info stuff from existing dset

if $NT -disp_hdr -infiles ${infile} -debug 3
then
echo "=== disp_hdr succeeded"
else
echo === disp_hdr failed
exit 1
fi

if $NT -disp_nim -infiles ${infile} -debug 5
then
echo "=== disp_nim succeeded"
else
echo === disp_nim failed
exit 1
fi

if $NT -disp_hdr2 -infiles ${infile} -debug 3
then
echo "=== disp_hdr2 succeeded"
else
echo === disp_hdr2 failed
exit 1
fi

# allow failure?
if $NT -disp_hdr1 -infiles ${infile}
then
echo === disp_hdr1 incorrectly succeeded
exit 1
else
echo === good: disp_hdr1 failed
fi

if $NT -disp_cext -infiles ${infile}
then
echo === disp_cext succeeded
else
echo === disp_cext failed
exit 1
fi

# display some data
if $NT -infiles ${infile} -disp_ci 0 0 0 0 -1 23456 0
then
echo === disp_ci succeeded
else
echo === disp_ci failed
exit 1
fi


# --------------------------------------------------
# modify an existing dset and compare

echo "============== modify an hdr2 field"
if $NT -mod_hdr2 -mod_field slice_start 3 -mod_field slice_duration 0.04 \
       -infiles ${infile} -prefix ${prefix}.01.a.nii
then
echo === mod_hdr2 mod_nim succeeded
else
echo === mod_hdr2 mod_nim failed
exit 1
fi

echo "============== modify an image field"
if $NT -debug 2 -infiles ${prefix}.01.a.nii -prefix ${prefix}.01.b.nia \
              -mod_nim -mod_field qform_code 2 -mod_field sform_code 2
then
echo === mod_nim succeeded
else
echo === mod_nim failed
exit 1
fi

echo "============== collapse 6th (last) dim of CIFTI input, compress output to test znz"
if $NT -cci 0 0 0 0 -1 17 0 -infiles ${prefix}.01.a.nii \
              -debug 3 -prefix ${prefix}.02.nii.gz
then
echo === cci succeeded
else
echo === cci failed
exit 1
fi

# store result for other commands
tfile01=${prefix}.02.nii.gz

echo "============== look for subsequent dimension changes (NIFTI types vary)"
if $NT -diff_hdr2 -infiles ${infile} ${tfile01}
then
echo === diff_hdr2 incorrectly succeeded
exit 1
else
echo === good: diff_hdr2 showed a difference
fi

echo "============== look for subsequent dim changes (NIFTI types vary)"
if $NT -diff_nim -infiles ${infile} ${tfile01}
then
echo === diff_nim incorrectly succeeded
exit 1
else
echo === good: diff_nim showed a difference
fi

echo "============== use volume index select, which requires sorting and selecting"
if $NT -copy_brick_list -infiles ${infile}'[2,3,3,3,1]' \
              -debug 3                                         \
              -prefix ${prefix}.03.nii
then
echo === copy_brick_list succeeded
else
echo === copy_brick_list failed
exit 1
fi

echo "============== test CAPS file extension"
if $NT -mod_hdr2 -mod_field slice_code 2 -infiles ${infile} \
              -prefix ${prefix}.04.NII
then
echo === mod_hdr2 succeeded
else
echo === mod_hdr2 failed
exit 1
fi

# store result for other commands
tfile02=${prefix}.04.NII

echo "============== differ in slice_code? (nim)"
if $NT -diff_nim -infiles ${infile} ${tfile02}
then
echo === second diff_nim incorrectly succeeded
exit 1
else
echo === good: second diff_nim showed a difference
fi

echo "============== differ in slice_code? (hdr)"
if $NT -diff_hdr -infiles ${infile} ${tfile02}
then
echo === diff_hdr incorrectly succeeded
exit 1
else
echo === good: diff_hdr showed a difference
fi

echo "============== differ in slice_code? (hdr2)"
if $NT -diff_hdr2 -infiles ${infile} ${tfile02}
then
echo === diff_hdr2 incorrectly succeeded
exit 1
else
echo === good: diff_hdr2 showed a difference
fi

echo "============== swap hdr as NIFTI (failes for N-2, but still debug)"
if $NT -swap_as_nifti -debug 3 -infiles ${infile} -prefix ${prefix}.05.swap.nii
then
echo === swap_as_nifti incorrectly succeeded
exit 1
else
echo === good: swap_as_nifti whines for NIFTI-2 hdr as NIFTI-1
fi

