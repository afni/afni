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

rm -f mod.hdr2*

echo "============== 1 : diff_nim"
if $NT -diff_nim  -infiles $DATA/Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii \
                           $DATA/ones.dscalar.nii
then
echo diff_nim failed '(no difference seen)'
exit 1
else
echo ""
fi

echo "============== 2 : diff_hdr2"
if $NT -diff_hdr2 -infiles $DATA/Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii \
                           $DATA/ones.dscalar.nii
then
echo diff_hdr failed '(no difference seen)'
exit 1
else
echo ""
fi

echo "============== 3 : mod_hdr2"
if $NT -infiles $DATA/Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii \
       -mod_hdr2 -mod_field slice_code 2 -prefix mod.hdr2.nii
then
echo ""
else
echo mod_hdr2 failed '(no difference seen)'
exit 1
fi

echo "============== 4 : diff_nim on mod"
if $NT -diff_nim  -infiles $DATA/Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii \
                           mod.hdr2.nii
then
echo diff_nim failed on mod '(no difference seen)'
exit 1
else
echo ""
fi

echo "============== 5 : diff_hdr2 on mod"
if $NT -diff_hdr2 -infiles $DATA/Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii \
                           mod.hdr2.nii
then
echo diff_hdr2 failed on mod '(no difference seen)'
exit 1
else
echo ""
fi

