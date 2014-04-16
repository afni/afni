#!/bin/tcsh

if( $#argv > 0 ) then
  echo " "
  echo "** This script is similar to @toMNI_Qwarp -- but it spawns **"
  echo "** jobs in parallel (on the same system).  To use it, you  **"
  echo "** must edit the script and set the 2 variables            **"
  echo " "
  echo "Script to take a collection of datasets and transform them"
  echo "to MNI space, then collectively re-transform them to produce"
  echo "a more refined average.  This script is usually used AFTER"
  echo "running @toMNI_Awarp to do the affine alignments, and that"
  echo "script is run AFTER skull-stripping the original volumes."
  echo " "
  echo "This script spawns jobs to run in parallel (on the same system)."
  echo "Before using it, copy it into the data directory, and edit it"
  echo "to set the 2 variables:"
  echo "     set numcpu = TOTAL NUMBER OF CPUS TO USE"
  echo "     set numjob = MAX NUMBER OF JOBS TO USE"
  echo "numcpu should not exceed the number of CPUs (cores) on the system;"
  echo "it is often simplest to set numjob to the same value as numcpu,"
  echo "so that 1 dataset is processed in 1 core, and numcpu jobs are"
  echo "run at a time."
  echo " "
  echo "Usage: @toMNI_Qwarpar    (... and then wait a long time)"
  echo " "
  echo "It should be run inside the directory created by @toMNI_Awarp, and"
  echo "will process the *_uni+tlrc.HEAD datasets created by that script."
  exit 0
endif

##### Set these 2 variables for your system ##################################
#####   Do NOT make numjob > numcpu or make numcpu > actual number of CPUs
#####   Each job will get numcpu/numjob CPUs.
#####   For speed, the number of input datasets (*_uni+tlrc.HEAD) should
#####     be an even multiple of numjob.

set numcpu = 8
set numjob = 8

##### Set this variable to YES if you want to do the F level (finest, slowest)

set doFFF = NO

##### Only change this affine level template name if you truly grok, my brother

set basename = MNI152_1mm_uni+tlrc

################### Don't alter the script below this line! ###################
###################  (unless you are Zhark or a myrmidon)   ###################

##### Find the A level template

set tpath = `@FindAfniDsetPath $basename`
if( $tpath == '' ) then
  echo "** @toMNI_Qwarp: Failed to find template $basename -- cannot continue"
  exit 1
endif

set AAbasename = ${tpath}/${basename}

##### Create the list of 3dUnifize-d and affine MNI-ized datasets to process

set dlist = ( *_uni+tlrc.HEAD )

if( $#dlist < 9 ) then
  echo "** @toMNI_Qwarp: Not enough input datasets (need at least 9) -- cannot continue"
  exit 1
endif

##### A level (affine) #####

if( -f Qwarp_meanA+tlrc.HEAD ) then
  echo " "
  echo "========== @toMNI_Qwarp: skipping Qwarp_meanA =========="
  echo " "
  goto AAA_done ;
endif

echo " "
echo "========== @toMNI_Qwarp: Compute A level mean (affine) =========="
echo " "

nice 3dMean -prefix Qwarp_meanA -datum float $dlist

AAA_done:

##### B level (coarsest scale nonlinear) #####

if( -f Qwarp_meanB+tlrc.HEAD ) then
  echo " "
  echo "========== @toMNI_Qwarp: skipping Qwarp_meanB =========="
  echo " "
  goto BBB_done ;
endif

echo " "
echo "========== @toMNI_Qwarp: Compute B level (minimum patch = 101) =========="
echo " "

# make a list of those warps that have NOT been done yet at this level,
# then parcel it out into parts to be done in different jobs

set qlist = ( )
foreach fred ( $dlist )
  set bbb  = `basename $fred _uni+tlrc.HEAD`
  if( ! -e ${bbb}_uniB_WARP+tlrc.HEAD ) set qlist = ( $qlist $fred )
end
if( $#qlist == 0 ) then
  echo "=== All B warps computed -- skipping to averaging step ==="
  goto BBB_domean
endif
set qjob = $numjob
if( `@ tmp = ( $#qlist < $qjob ) ; echo $tmp` ) set qjob = $#qlist
@ qper  = $#qlist / $qjob
@ qext  = $#qlist % $qjob
@ qcpu  = $numcpu / $qjob
@ qplus = $qjob + 1
set top = 0
set botlist = ( )
set toplist = ( )
foreach part ( `count -dig 1 1 $qjob` )
  @ bot = $top + 1
  @ top = $top + $qper
  if ( $qext > 0 ) then
    @ top++ ; @ qext--
  endif
  set botlist = ( $botlist $bot )
  set toplist = ( $toplist $top )
end
echo "=== Processing $#qlist datasets in $qjob jobs"
echo "=== Bottom index list = $botlist"
echo "=== Top    index list = $toplist"

# create the script to run 3dQwarp over a bunch of datasets

echo "setenv OMP_NUM_THREADS $qcpu"                         >  BBwarp_script
echo 'echo $argv'                                           >> BBwarp_script
echo 'foreach lucy ( $argv )'                               >> BBwarp_script
echo '   set bbb = `basename $lucy _uni+tlrc.HEAD`'         >> BBwarp_script
echo '   nice 3dQwarp -prefix ${bbb}_uniB  -blur 0 9    \'  >> BBwarp_script
echo '                -minpatch 101            -nodset  \'  >> BBwarp_script
echo "                $AAbasename" '$lucy'                  >> BBwarp_script
echo 'end'                                                  >> BBwarp_script
echo 'touch BBwarp_$argv[1]_Done'                           >> BBwarp_script
echo 'exit 0'                                               >> BBwarp_script

# and actually do all the big work

foreach part ( `count -dig 1 1 $qjob` )
  set bot = $botlist[$part]
  set top = $toplist[$part]
  set plist = ( $qlist[$bot-$top] )
  tcsh ./BBwarp_script $plist &
  sleep 9
end

# wait until the number of markers for scripts being done is complete

while(1)
  sleep 60
  set fdone = ( BBwarp_* )
  if( $#fdone == $qplus ) break
end

# remove the script and the markers

\rm BBwarp_*

# adjust warps (remove mean warp) and then compute mean warped dataset

BBB_domean:
nice 3dNwarpAdjust -nwarp *_uniB_WARP+tlrc.HEAD -source $dlist -prefix Qwarp_meanB

BBB_done:

##### C level (finer level nonlinear) #####

if( -f Qwarp_meanC+tlrc.HEAD ) then
  echo " "
  echo "========== @toMNI_Qwarp: skipping Qwarp_meanC =========="
  echo " "
  goto CCC_done ;
endif

echo " "
echo "========== @toMNI_Qwarp: Compute C level (minimum patch = 49) =========="
echo " "

# make a list of those warps that have NOT been done yet at this level,
# then parcel it out into parts to be done in different jobs

set qlist = ( )
foreach fred ( $dlist )
  set bbb  = `basename $fred _uni+tlrc.HEAD`
  if( ! -e ${bbb}_uniC_WARP+tlrc.HEAD ) set qlist = ( $qlist $fred )
end
if( $#qlist == 0 ) then
  echo "=== All C warps computed -- skipping to averaging step ==="
  goto CCC_domean
endif
set qjob = $numjob
if( `@ tmp = ( $#qlist < $qjob ) ; echo $tmp` ) set qjob = $#qlist
@ qper  = $#qlist / $qjob
@ qext  = $#qlist % $qjob
@ qcpu  = $numcpu / $qjob
@ qplus = $qjob + 1
set top = 0
set botlist = ( )
set toplist = ( )
foreach part ( `count -dig 1 1 $qjob` )
  @ bot = $top + 1
  @ top = $top + $qper
  if ( $qext > 0 ) then
    @ top++ ; @ qext--
  endif
  set botlist = ( $botlist $bot )
  set toplist = ( $toplist $top )
end
echo "=== Processing $#qlist datasets in $qjob jobs"
echo "=== Bottom index list = $botlist"
echo "=== Top    index list = $toplist"

echo "setenv OMP_NUM_THREADS $qcpu"                                    >  CCwarp_script
echo 'foreach fred ( $argv )'                                          >> CCwarp_script
echo '   set bbb = `basename $fred _uni+tlrc.HEAD`'                    >> CCwarp_script
echo '   nice 3dQwarp -prefix ${bbb}_uniC  -blur 1 6               \'  >> CCwarp_script
echo '                -inilev 2  -minpatch 49 -nodset              \'  >> CCwarp_script
echo '                -iniwarp ${bbb}_uniB_WARP+tlrc.HEAD          \'  >> CCwarp_script
echo '                Qwarp_meanB+tlrc.HEAD $fred'                     >> CCwarp_script
echo 'end'                                                             >> CCwarp_script
echo 'touch CCwarp_$argv[1]_Done'                                      >> CCwarp_script
echo 'exit 0'                                                          >> CCwarp_script

foreach part ( `count -dig 1 1 $qjob` )
  tcsh ./CCwarp_script $qlist[$botlist[$part]-$toplist[$part]] &
  sleep 9
end

while(1)
  sleep 60
  set fdone = ( CCwarp_* )
  if( $#fdone == $qplus ) break
end

\rm CCwarp_*

CCC_domean:
nice 3dNwarpAdjust -nwarp *_uniC_WARP+tlrc.HEAD -source $dlist -prefix Qwarp_meanC

CCC_done:

##### D level (even finer nonlinear) #####

if( -f Qwarp_meanD+tlrc.HEAD ) then
  echo " "
  echo "========== @toMNI_Qwarp: skipping Qwarp_meanD =========="
  echo " "
  goto DDD_done ;
endif

echo " "
echo "========== @toMNI_Qwarp: Compute D level (minimum patch = 23) =========="
echo " "

# make a list of those warps that have NOT been done yet at this level,
# then parcel it out into parts to be done in different jobs

set qlist = ( )
foreach fred ( $dlist )
  set bbb  = `basename $fred _uni+tlrc.HEAD`
  if( ! -e ${bbb}_uniD_WARP+tlrc.HEAD ) set qlist = ( $qlist $fred )
end
if( $#qlist == 0 ) then
  echo "=== All D warps computed -- skipping to averaging step ==="
  goto DDD_domean
endif
set qjob = $numjob
if( `@ tmp = ( $#qlist < $qjob ) ; echo $tmp` ) set qjob = $#qlist
@ qper  = $#qlist / $qjob
@ qext  = $#qlist % $qjob
@ qcpu  = $numcpu / $qjob
@ qplus = $qjob + 1
set top = 0
set botlist = ( )
set toplist = ( )
foreach part ( `count -dig 1 1 $qjob` )
  @ bot = $top + 1
  @ top = $top + $qper
  if ( $qext > 0 ) then
    @ top++ ; @ qext--
  endif
  set botlist = ( $botlist $bot )
  set toplist = ( $toplist $top )
end
echo "=== Processing $#qlist datasets in $qjob jobs"
echo "=== Bottom index list = $botlist"
echo "=== Top    index list = $toplist"

echo "setenv OMP_NUM_THREADS $qcpu"                                       >  DDwarp_script
echo 'foreach fred ( $argv )'                                             >> DDwarp_script
echo '   set bbb = `basename $fred _uni+tlrc.HEAD`'                       >> DDwarp_script
echo '   nice 3dQwarp -prefix ${bbb}_uniD  -blur 0 4                 \'   >> DDwarp_script
echo '                -inilev 5  -minpatch 23 -nodset                \'   >> DDwarp_script
echo '                -iniwarp ${bbb}_uniC_WARP+tlrc.HEAD            \'   >> DDwarp_script
echo '                Qwarp_meanC+tlrc.HEAD $fred'                        >> DDwarp_script
echo 'end'                                                                >> DDwarp_script
echo 'touch DDwarp_$argv[1]_Done'                                         >> DDwarp_script
echo 'exit 0'                                                             >> DDwarp_script

foreach part ( `count -dig 1 1 $qjob` )
  tcsh ./DDwarp_script $qlist[$botlist[$part]-$toplist[$part]] &
  sleep 9
end

while(1)
  sleep 60
  set fdone = ( DDwarp_* )
  if( $#fdone == $qplus ) break
end

\rm DDwarp_*

DDD_domean:
nice 3dNwarpAdjust -nwarp *_uniD_WARP+tlrc.HEAD -source $dlist -prefix Qwarp_meanD

DDD_done:

##### E level (even finer nonlinear) #####

if( -f Qwarp_meanE+tlrc.HEAD ) then
  echo " "
  echo "========== @toMNI_Qwarp: skipping Qwarp_meanE =========="
  echo " "
  goto EEE_done ;
endif

# make a list of those warps that have NOT been done yet at this level,
# then parcel it out into parts to be done in different jobs

set qlist = ( )
foreach fred ( $dlist )
  set bbb  = `basename $fred _uni+tlrc.HEAD`
  if( ! -e ${bbb}_uniE_WARP+tlrc.HEAD ) set qlist = ( $qlist $fred )
end
if( $#qlist == 0 ) then
  echo "=== All E warps computed -- skipping to averaging step ==="
  goto EEE_domean
endif
set qjob = $numjob
if( `@ tmp = ( $#qlist < $qjob ) ; echo $tmp` ) set qjob = $#qlist
@ qper  = $#qlist / $qjob
@ qext  = $#qlist % $qjob
@ qcpu  = $numcpu / $qjob
@ qplus = $qjob + 1
set top = 0
set botlist = ( )
set toplist = ( )
foreach part ( `count -dig 1 1 $qjob` )
  @ bot = $top + 1
  @ top = $top + $qper
  if ( $qext > 0 ) then
    @ top++ ; @ qext--
  endif
  set botlist = ( $botlist $bot )
  set toplist = ( $toplist $top )
end
echo "=== Processing $#qlist datasets in $qjob jobs"
echo "=== Bottom index list = $botlist"
echo "=== Top    index list = $toplist"

echo " "
echo "========== @toMNI_Qwarp: Compute E level (minimum patch = 13) =========="
echo " "

echo "setenv OMP_NUM_THREADS $qcpu"                                      >  EEwarp_script
echo 'foreach fred ( $argv )'                                            >> EEwarp_script
echo '   set bbb = `basename $fred _uni+tlrc.HEAD`'                      >> EEwarp_script
echo '   nice 3dQwarp -prefix ${bbb}_uniE  -blur 0 -2                \'  >> EEwarp_script
echo '                -inilev 7  -minpatch 13 -nodset                \'  >> EEwarp_script
echo '                -iniwarp ${bbb}_uniD_WARP+tlrc.HEAD            \'  >> EEwarp_script
echo '                Qwarp_meanD+tlrc.HEAD $fred'                       >> EEwarp_script
echo 'end'                                                               >> EEwarp_script
echo 'touch EEwarp_$argv[1]_Done'                                        >> EEwarp_script
echo 'exit 0'                                                            >> EEwarp_script

foreach part ( `count -dig 1 1 $qjob` )
  tcsh ./EEwarp_script $qlist[$botlist[$part]-$toplist[$part]] &
  sleep 9
end

while(1)
  sleep 60
  set fdone = ( EEwarp_* )
  if( $#fdone == $qplus ) break
end

\rm EEwarp_*

EEE_domean:
nice 3dNwarpAdjust -nwarp *_uniE_WARP+tlrc.HEAD -source $dlist -prefix Qwarp_meanE

if( $doFFF != 'YES' )then
  foreach fred ( $dlist )
    set bbb = `basename $fred _uni+tlrc.HEAD`
    3dNwarpApply -nwarp ${bbb}_uniE_WARP+tlrc.HEAD \
                 -source $fred -prefix ${bbb}_uniE
  end
endif

EEE_done:

##### F level (optional) #####

if( $doFFF != 'YES' ) goto FFF_done

if( -f Qwarp_meanF+tlrc.HEAD ) then
  echo " "
  echo "========== @toMNI_Qwarp: skipping Qwarp_meanF =========="
  echo " "
  goto FFF_done ;
endif

echo " "
echo "========== @toMNI_Qwarp: Compute F level (minimum patch = 9) =========="
echo " "

set qlist = ( )
foreach fred ( $dlist )
  set bbb  = `basename $fred _uni+tlrc.HEAD`
  if( ! -e ${bbb}_uniF_WARP+tlrc.HEAD ) set qlist = ( $qlist $fred )
end
if( $#qlist == 0 ) then
  echo "=== All F warps computed -- skipping to averaging step ==="
  goto FFF_domean
endif
set qjob = $numjob
if( `@ tmp = ( $#qlist < $qjob ) ; echo $tmp` ) set qjob = $#qlist
@ qper  = $#qlist / $qjob
@ qext  = $#qlist % $qjob
@ qcpu  = $numcpu / $qjob
@ qplus = $qjob + 1
set top = 0
set botlist = ( )
set toplist = ( )
foreach part ( `count -dig 1 1 $qjob` )
  @ bot = $top + 1
  @ top = $top + $qper
  if ( $qext > 0 ) then
    @ top++ ; @ qext--
  endif
  set botlist = ( $botlist $bot )
  set toplist = ( $toplist $top )
end
echo "=== Processing $#qlist datasets in $qjob jobs"
echo "=== Bottom index list = $botlist"
echo "=== Top    index list = $toplist"

echo "setenv OMP_NUM_THREADS $qcpu"                                     >  FFwarp_script
echo 'foreach fred ( $argv )'                                           >> FFwarp_script
echo '   set bbb = `basename $fred _uni+tlrc.HEAD`'                     >> FFwarp_script
echo '   nice 3dQwarp -prefix ${bbb}_uniF  -blur 0 -2              \'   >> FFwarp_script
echo '                -inilev 9  -minpatch 9  -nodset              \'   >> FFwarp_script
echo '                -iniwarp ${bbb}_uniE_WARP+tlrc.HEAD          \'   >> FFwarp_script
echo '                Qwarp_meanE+tlrc.HEAD $fred'                      >> FFwarp_script
echo 'end'                                                              >> FFwarp_script
echo 'touch FFwarp_$argv[1]_Done'                                       >> FFwarp_script
echo 'exit 0'                                                           >> FFwarp_script

foreach part ( `count -dig 1 1 $qjob` )
  tcsh ./FFwarp_script $qlist[$botlist[$part]-$toplist[$part]] &
  sleep 9
end

while(1)
  sleep 60
  set fdone = ( FFwarp_* )
  if( $#fdone == $qplus ) break
end

\rm FFwarp_*

nice 3dNwarpAdjust -nwarp *_uniF_WARP+tlrc.HEAD -source $dlist -prefix Qwarp_meanF

foreach fred ( $dlist )
  set bbb = `basename $fred _uni+tlrc.HEAD`
  3dNwarpApply -nwarp ${bbb}_uniF_WARP+tlrc.HEAD \
               -source $fred -prefix ${bbb}_uniF
end

FFF_done:

################ finished ################

echo " "
echo "========== @toMNI_Qwarpar: compressing output BRIK files =========="
echo " "

which pigz >& /dev/null
if ( $status == 0 ) then
  set GZIP = pigz
else
  set GZIP = gzip
endif

# nice $GZIP -9v *.BRIK

echo " "
echo "========== @toMNI_Qwarpar: free at last =========="
echo " "

exit 0
