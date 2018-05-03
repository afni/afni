#!/bin/tcsh

if( $#argv < 1 )then
  echo "Need results directory name as command line argument"
  exit 0
endif

set ddir = $argv[1]

pushd $ddir

set aa = ( anat_final.*+tlrc.HEAD )
if( $#aa != 1 )then
  echo "Can't find anat_final.*+tlrc.HEAD" ; exit 1
endif

set subj = `echo $aa[1] | sed -e 's/+tlrc.HEAD//' -e 's/anat_final.//'`

echo "----- Plotting motion"

foreach ddd ( dfile.r[0-9]*.1D )
  1d_tool.py -infile $ddd -derivative -collapse_cols euclidean_norm -write enorm.$ddd
end

cat enorm.dfile.r*.1D > enorm.dfile.all.1D
\rm enorm.dfile.r*.1D

set ccc = censor_${subj}_combined_2.1D
if( ! -f $ccc )then
  set ccc = motion_${subj}_censor.1D
  if( ! -f $ccc )then
    set ccc = outcount_${subj}_censor.1D
    if( ! -f $ccc )then
      unset ccc
    endif
  endif
endif

if( $?ccc )then
  1deval -a enorm.dfile.all.1D -b $ccc -expr 'a*b' | \
    1dplot -stdin -nopush -naked -pnms 1000 enorm.dfile.all.ppm -aspect 10
else
  1dplot -nopush -naked -pnms 1000 enorm.dfile.all.ppm -aspect 10 enorm.dfile.all.1D
endif

echo "----- Segmenting $aa[1]"

3dSeg -anat $aa[1] -mask AUTO -blur_meth BIM -classes 'CSF; GM; WM'

if( -d Segsy )then
  3dcalc -a Segsy/Classes+tlrc -expr 'ifelse(equals(a,1),4,a)' \
         -datum byte -nscale -prefix GmaskA.nii
##  3dresample -master mask_epi_anat.${subj}+tlrc.HEAD \
##             -rmode NN -input GmaskA.nii -prefix GmaskB.nii
  3dfractionize -template mask_epi_anat.${subj}+tlrc.HEAD \
                -input GmaskA.nii -prefix GmaskB.nii -vote
  3dcalc -a GmaskB.nii -b mask_epi_anat.${subj}+tlrc.HEAD \
         -expr 'a*b' -prefix GmaskC.nii
  set ggg = GmaskC.nii
  \rm -rf GmaskA.nii GmaskB.nii Segsy
else
  set ggg = mask_epi_anat.${subj}+tlrc.HEAD
endif

echo "----- Grayplot errts"

3dGrayplot -dimen 1000 500                  \
           -mask $ggg                       \
           -input errts.${subj}+tlrc.HEAD   \
           -prefix G.errts.pgm

echo "----- Combining images"

pnmcat -tb enorm.dfile.all.ppm G.errts.pgm | pnmtopng - > Grayplot.errts.${subj}.png

\rm enorm.dfile.all.* G.errts.pgm
if( -f GmaskC.nii ) \rm GmaskC.nii

echo "----- Result in $cwd/Grayplot.errts.${subj}.png"
exit 0
