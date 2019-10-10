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


# print simple info and terminate

echo "============== version, NIFTI version, with zlib"
$NT -ver
$NT -nifti_ver
$NT -with_zlib


echo "============== history (NT), NIFTI history"
$NT -hist
$NT -nifti_hist


echo "============== help, datatypes, hdr, nim"
$NT -help
$NT -help_datatypes
$NT -help_datatypes D
$NT -help_datatypes N
$NT -help_datatypes T
$NT -help_hdr1
$NT -help_hdr2
$NT -help_ana
$NT -help_nim

