#!/bin/tcsh -ef

set version = "2.1"
set rev_dat = "Aug, 2016"

set here     = $PWD
set anat_dir = ""                   # will be output dir
set T1in     = ""                   # nec input T1w vol
set T1in_ss  = ""                   # optional, if SS'ing elsewhere
set outpref  = "out"                # for final output files
set temppref = "_tempZ43q6"         # for intermed files
set DoClean  = "0"

# ------------------- process options, a la rr ----------------------

if ( $#argv == 0 ) goto SHOW_HELP

set ac = 1
while ( $ac <= $#argv )
    # terminal options
    if ( ("$argv[$ac]" == "-h" ) || ("$argv[$ac]" == "-help" )) then
        goto SHOW_HELP
    endif
    if ( "$argv[$ac]" == "-ver" ) then
        goto SHOW_VERSION
    endif

   # required
   if ( "$argv[$ac]" == "-inset" ) then
      if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
      @ ac += 1
      set T1in = "$argv[$ac]"

   else if ( "$argv[$ac]" == "-in_ss" ) then
      if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
      @ ac += 1
      set T1in_ss = "$argv[$ac]"

   else if ( "$argv[$ac]" == "-outdir" ) then
      if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
      @ ac += 1
      set anat_dir = "$argv[$ac]"

   else if ( "$argv[$ac]" == "-prefix" ) then
      if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
      @ ac += 1
      set outpref = "$argv[$ac]"

   else if ( "$argv[$ac]" == "-do_clean" ) then
      set DoClean = 1

   else
      echo "** unexpected option #$ac = '$argv[$ac]'"
      exit 2

   endif
   @ ac += 1
end

# =======================================================================
# ============================ ** SETUP ** ==============================
# =======================================================================

echo "++ Start script version: $version"

# ============================= input file ==============================

set check = `3dinfo "$T1in"`
if ( "$#check" == "0" ) then
    echo "** ERROR: can't find file $T1in !"
    goto EXIT
endif

if ( $T1in_ss != "" ) then
    set check = `3dinfo "$T1in_ss"`
    if ( "$#check" == "0" ) then
        echo "** ERROR: can't find file $T1in_ss !"
        goto EXIT
    endif
endif

# ============================= output dir ==============================

# check output directory, use input one if nothing given

# default output dir, if nothing input.
if ( $anat_dir == "" ) then

    set anat_dir = `dirname $T1in`
    echo "\n++ No output directory specificied by the user."
    echo "++ Using the '-indir ...' directory by default:"
    echo "\t$anat_dir"

endif

# ============================== naming ================================

set wdir  = "$anat_dir"

# =======================================================================
# =========================== ** PROCESS ** =============================
# =======================================================================

set idx = 1 

set amask = "$wdir/${temppref}_${idx}_automask.nii.gz"
set fin = $T1in
echo "\nPre-brightening: ON\n"
3dAutomask  -echo_edu                            \
    -overwrite                                   \
    -prefix $amask                               \
    $fin 
@ idx += 1

# calc to-be max as 99percentile of in-brain distr: prob still
# generous?
set NUMS = `3dBrickStat -mask ${amask} -percentile 90 1 90  ${fin}`
set P_THR = "$NUMS[2]"

printf "\nThresh value will be: $P_THR \n"

set fout = "$wdir/${temppref}_${idx}_pre.nii.gz"
set fout_thr = $fout
3dcalc                                            \
    -echo_edu                                     \
    -a "$fin"                                     \
    -expr "maxbelow(${P_THR},a)"                  \
    -prefix "$fout"                               \
    -float                                        \
    -overwrite
@ idx += 1

if ( $T1in_ss == "" ) then
    set fin  = "$fout"
    set fout = "$wdir/${temppref}_${idx}_ani.nii.gz"
    3danisosmooth -echo_edu       \
        -overwrite                \
        -iters 2                  \
        -prefix $fout             \
        -3D                       \
        "$fin"
    @ idx += 1

    set fin  = "$fout"
    set fout = "$wdir/${temppref}_${idx}_ss.nii.gz"
    set T1in_ss = $fout
    3dSkullStrip -echo_edu               \
        -input $fin                      \
        -prefix $fout                    \
        -blur_fwhm 2                     \
        -orig_vol                        \
        -overwrite
    @ idx += 1
endif

# wherever SS brain came from, use it now as binary mask.
set finm  = "$T1in_ss"
set foutm = "$wdir/${temppref}_${idx}_mask1.nii.gz"
3dcalc                                \
    -echo_edu                         \
    -a $finm                          \
    -expr 'step(a)'                   \
    -prefix ${foutm}                  \
    -overwrite
@ idx += 1

set finm  = "$foutm"
set foutm = "$wdir/${temppref}_${idx}_mask2.nii.gz"
3dmask_tool                           \
    -echo_edu                         \
    -inputs ${finm}                   \
    -dilate_inputs 2 -1               \
    -prefix ${foutm}                  \
    -overwrite
@ idx += 1

# unifize the *unmasked* one
set fin  = "$fout_thr"
set fout = "$wdir/${temppref}_${idx}_uni.nii.gz"
3dUnifize                    \
    -echo_edu                \
    -prefix $fout            \
    -input $fin              \
    -overwrite
@ idx += 1

# calc to-be max as 99percentile of in-brain distr: prob still
# generous?
set NUMS = `3dBrickStat -mask ${foutm}  -percentile 90 1 90  ${fout}`
set P_THR = "$NUMS[2]"

printf "\nThresh value will be: $P_THR \n"

set fin1  = "$fout"
set fin2  = "$foutm"
set t1out  = "$wdir/${outpref}_t1w.nii"
3dcalc                                            \
    -echo_edu                                     \
    -a $fin1                                      \
    -b $fin2                                      \
    -expr "maxbelow(${P_THR},a)*(1*b+0.2*not(b))" \
    -prefix $t1out                                \
    -float                                        \
    -overwrite

set fin1  = "$t1out"
set fin2  = "$foutm"
set fout  = "$wdir/${outpref}_t1w_ss.nii"
3dcalc                                            \
    -echo_edu                                     \
    -a $fin1                                      \
    -b $fin2                                      \
    -expr "maxbelow(${P_THR},a)*(1*b)" \
    -prefix $fout                                 \
    -float                                        \
    -overwrite


# try making an imitation T2
set fin1  = "$t1out"
set fin2  = "$foutm"
set t2out = "$wdir/${outpref}_t2w.nii"
3dcalc                                            \
    -echo_edu                                     \
    -a $fin1                                      \
    -b $fin2                                      \
    -expr "(1.1*${P_THR}-a)*b+not(b)*a"           \
    -prefix $t2out                                \
    -float                                        \
    -overwrite

if ( $DoClean ) then
    echo "\n Removing temporary (${temppref}*) files.\n"
    \rm $wdir/${temppref}*
else
    echo "\n NOT Removing temporary files.\n"
endif

printf "\n DONE!\n View T1w file:\n    ${t1out}\n"
printf "\n DONE!\n View 'T2w' file:\n    ${t2out}\n"

goto EXIT

# ========================================================================
# ========================================================================

SHOW_HELP:
cat << EOF
-------------------------------------------------------------------------

  Some basic processing of T1w anatomical images, particularly for
  preparation in using as a reference structural in TORTOISE -> makes
  an imitation T2w-contrast image, in terms of relative tissue
  contrast.  Verify all results visually!

  This does: unifizing of brightness, anisosmoothing, some skull
  stripping, and also generates an imitation T2w-contrast image
  through **very** simple means.  The output T2w volume is *not* for
  quantitative use, but for registrative purposes.

  REQUIRES: AFNI.

  Ver. $version (PA Taylor, ${rev_dat})

  For use, example images, and citation, see (esp. Appendix A):
     Taylor PA, Alhamud A, van der Kouwe AJ, Saleh MG, Laughton B,
     Meintjes EM.  Assessing the performance of different DTI motion
     correction strategies in the presence of EPI distortion
     correction.  Hum Brain Mapp (in press).

-------------------------------------------------------------------------

  RUNNING:

  This script has one required argument ('-inset ...'), and the rest are
    optional:

  \$ tcsh fat_pre_t2w_from_t1w.tcsh            \
        -inset  T1_FILE                       \
        {-outdir DIR_NAME}                    \
        {-prefix PREFIX}                      \
        {-in_ss T1_SS}                        \
        {-do_clean}

  where: 
  -inset  T1_FILE  :is the full name of the input T1w volume;

  -outdir DIR_NAME :is the output directory (default is the directory
                    containing the input T1_SS file).
  -prefix PREFIX   :is the prefix of the output processed T1 name
                    (default is 'out').
  -in_ss  T1_SS    :an optional input of a pre-skullstripped T1_FILE
                    (this can be either a mask or a skullstripped volume).
                    This can be useful if the default skullstripping
                    options in this script ain't getting the job done
                    and other ones have to be done (skullstripping is
                    probably the slowest part of this set of steps).

  -do_clean        :is an optional switch to remove intermediate 
                    '${temppref}*' files; (default: not to do so).

 ------------------------------------------------------------------------

  OUTPUTS:

    PREFIX_t2w.nii        :a volume with T2w-like tissue contrast made
                           from a T1w one; the outside of the brain
                           has scaled skull and noise, for having a
                           non-zero SNR estimation.
    PREFIX_t1w.nii        :a somewhat cleaned/processed version of the
                           input T1w volume; it also has a scaled skull 
                           and noise outside the brain.
    PREFIX_t1w_ss.nii     :a skull-stripped version of PREFIX_t1w.nii.gz.

-------------------------------------------------------------------------

  EXAMPLE:
    
    \$ tcsh fat_pre_t2w_from_t1w.tcsh  -inset T1.nii
    
  or

    \$ tcsh fat_pre_t2w_from_t1w.tcsh              \
          -inset T1.nii                           \
          -in_ss mask_WB.nii.gz                   \
          -do_clean

-------------------------------------------------------------------------

EOF

    goto EXIT

SHOW_VERSION:
   echo "version  $version (${rev_dat})"
   goto EXIT

# send everyone here, in case there is any cleanup to do
EXIT:
   exit
