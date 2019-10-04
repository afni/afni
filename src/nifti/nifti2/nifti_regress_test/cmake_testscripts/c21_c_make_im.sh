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
prefix=out.c21.c

rm -f $prefix*


# --------------------------------------------------
# create a new image, and note outfile for other commands
if $NT -mod_hdr -mod_field descrip 'dataset with mods' \
        -new_dim 3 10 20 30 0 0 0 0                    \
        -infiles MAKE_IM -prefix $prefix.10.nii
then
echo "=== mod_hdr 1 succeeded"
else
echo === mod_hdr 1 failed
exit 1
fi

# store result for future use
tfile11=$prefix.10.nii


# create ASCII based image as alternate
if $NT -mod_nim -mod_field descrip 'try ASCII' \
       -infiles $tfile11 -prefix $prefix.11.nia -debug 3
then
echo "=== disp_nim 2 succeeded"
else
echo === disp_nim 2 failed
exit 1
fi

# store result for future use
tfile12=$prefix.11.nia


# add some extensions, then display them
if $NT -add_ext 6 'adding extension type 6 (comment)' \
       -infiles $tfile12 -overwrite
then
echo "=== add_ext 3 succeeded"
else
echo === add_ext 3 failed
exit 1
fi

if $NT -add_comment_ext 'and now for a formal comment' \
        -infiles $tfile12 -overwrite
then
echo "=== add_comment_ext 4 succeeded"
else
echo === add_comment_ext 4 failed
exit 1
fi

if $NT -disp_exts -infiles $tfile12
then
echo "=== disp_exts 5 succeeded"
else
echo === disp_exts 5 failed
exit 1
fi


# and compare (as nim, we are not allowed to diff different hdr types)
if $NT -diff_nim -debug 3 -infiles $tfile11 $tfile12
then
echo === diff_nim 6 failed
exit 1
else
echo "=== good: diff_nim 6 showed a diff"
fi


# try swapping (as nifti, analzye, old)
# nifti
if $NT -swap_as_nifti -debug 3 -infiles $tfile11 \
       -prefix $prefix.12.a.swap.nii
then
echo "=== swap_as_nifti 7.a succeeded"
else
echo === swap_as_nifti 7.a failed
exit 1
fi

# try swapping (as nifti, analzye, old)
# analyze
if $NT -swap_as_analyze -debug 3 -infiles $tfile11 \
       -prefix $prefix.12.b.swap.ana.nii
then
echo "=== swap_as_nifti 7.b succeeded"
else
echo === swap_as_nifti 7.b failed
exit 1
fi

# try swapping (as nifti, analzye, old)
# old (AND ... make nifti_tool hunt for the input file)
if $NT -swap_as_old -debug 3 -infiles $prefix.12.b.swap.ana \
       -prefix $prefix.13.c.swap.old.nii
then
echo "=== swap_as_nifti 7.c succeeded"
else
echo === swap_as_nifti 7.c failed
exit 1
fi



# collapse third dimension, writing as ascii
if $NT -debug 3 -infiles $tfile12 -cci -1 -1 17 0 0 0 0 -prefix $prefix.13.nia
then
echo "=== cci 8 succeeded"
else
echo === cci 8 failed
exit 1
fi


