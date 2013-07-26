#!/bin/tcsh

if( $#argv < 2 ) then
  echo " "
  echo "Script to take a collection of datasets and transform them"
  echo "to 1x1x1 mm MNI space with an affine transformation."
  echo "These datasets should already have been skull-stripped."
  echo " "
  echo "Usage: @toMNI_Awarp dirname dataset1 dataset2 ..."
  echo " "
  echo "where 'dirname' is the name of the directory which will be created and"
  echo "then get the results, and 'dataset1 dataset2 ...' is a list of datasets"
  echo "to be transformed."
  echo " "
  echo "The results can further be nonlinearly registered to form a template"
  echo "using script @toMNI_Qwarpar (which will take a long time)."
  exit 0
endif

set basename = MNI152_1mm_uni+tlrc

## max number of jobs to run in parallel (less than or equal to number of CPUS)
## (these are single-threaded jobs)
set numjob   = 7

## check if output directory is already there

set dname = $argv[1]
if( -e $dname ) then
  echo "***** @toMNI_Awarp: Directory $dname already exists -- cannot continue"
  exit 1
endif

## find the base template

set tpath = `@FindAfniDsetPath $basename`
if( $tpath == '' ) then
  echo "***** @toMNI_Awarp: Failed to find template $basename -- cannot continue"
  exit 1
endif

## create the list of input datasets

set dlist = ( )
set nerr  = 0
foreach nnn ( `count -dig 1 2 $#argv` )
  set dlist = ( $dlist $argv[$nnn] )
  if( ! -f $argv[$nnn] ) then
    echo "  *** @toMNI_Awarp: Dataset file $argv[$nnn] does not exist"
    @ nerr++
  endif
end

if( $nerr > 0 ) then
  echo "***** @toMNI_Awarp: Cannot continue after such flagrant problems"
  exit 1
endif

## create the output directory

\mkdir -pv $dname
if( ! -d $dname ) then
  echo "***** @toMNI_Awarp: Failed to create directory $dname -- cannot continue"
  exit 1
endif

set ndset = $#dlist
set flist = ( )

echo " "
echo "===== @toMNI_Awarp: copying input datasets to $dname ====="
echo " "

foreach nnn ( `count -dig 1 1 $ndset` )
  set pan = `@parse_afni_name $dlist[$nnn]`
  set isn = `3dinfo -is_nifti $dlist[$nnn]`
  if( $isn ) then
    set nip = `echo $pan[2] | sed 's/.nii.gz$//g' | sed 's/.nii$//g'`
    3dcopy $dlist[$nnn] $dname/$nip
    set fname = `echo $dname/${nip}+????.HEAD`
  else
    3dcopy $dlist[$nnn] $dname/$pan[2]
    set fname = `echo $dname/$pan[2]+????.HEAD`
  endif
  set flist = ( $flist $fname:t )
end

cd $dname

echo " "
echo "===== @toMNI_Awarp: beginning 3dUnifize loop ====="
echo " "

setenv OMP_NUM_THREADS 1
foreach nnn ( `count -dig 1 1 $ndset` )
  set pan = `@parse_afni_name $flist[$nnn]`
  set pre = $pan[2]
  set vvv = $pan[3]
## work for the nnn-th file is done in the Temp${nnn} directory
  \mkdir -pv        Temp${nnn}
  cp ${pre}${vvv}.* Temp${nnn}
  cd                Temp${nnn}
  nice 3dUnifize -prefix ${pre}_uni -GM -input ${pre}${vvv}.HEAD &
  cd ..
  @ rrr = $nnn % $numjob
  if( $rrr == 0 ) wait
end
unsetenv OMP_NUM_THREADS
wait

echo " "
echo "===== @toMNI_Awarp: beginning @auto_tlrc loop ====="
echo " "

foreach nnn ( `count -dig 1 1 $ndset` )
  set pan = `@parse_afni_name $flist[$nnn]`
  set pre = $pan[2]
  set vvv = $pan[3]
  cd Temp${nnn}
  nice @auto_tlrc -base $basename                \
                  -input ${pre}_uni${vvv}        \
                  -no_ss -init_xform AUTO_CENTER \
                  -rmode quintic                   &
  cd ..
  @ rrr = $nnn % $numjob
  if( $rrr == 0 ) wait
end
wait
mv Temp*/*_uni+tlrc.* .
\rm -rf Temp*

echo " "
echo "===== @toMNI_Awarp compressing output BRIK files ====="
echo " "
which pigz >& /dev/null
if ( $status == 0 ) then
  set GZIP = pigz
else
  set GZIP = gzip
endif

nice $GZIP -9v *.BRIK
cd ..

echo " "
echo "===== @toMNI_Awarp is wrapped up, done, finished, and over ====="
echo " "
exit 0
