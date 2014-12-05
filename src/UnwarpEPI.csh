#!/bin/tcsh

#########-------------------------------------------------------------#########
######### Sample script for unwarping and registering multi-echo data #########
#########-------------------------------------------------------------#########

### Define the subject prefix for all datasets

set subj = HV002

echo
echo ".. Unwarping and Registering data for subject $subj .."

###------------------------------------------------
### set number of time points to ignore in 3dTshift
###------------------------------------------------

set tshift_ignore = 1

###--------------------------------------------------------
### set 3dNwarpApply interpolation mode (quintic or wsinc5)
### quintic is about twice as fast as wsinc5
###--------------------------------------------------------

set interp_mode = quintic

###--------------------------------------------------
### Set this to YES to skip unwarping the median-ized
### calibration runs (which aren't needed)
###--------------------------------------------------

set skip_medianized_unwarp = YES

###----------------------------------------------------------
### Define the standard space template to use at the end
### (set template = NONE or such nonsense to avoid this step)
###----------------------------------------------------------

# set template = MNI152_T1_2009c_uni+tlrc
set template = NONE

set template_minpatch = 17

#-- search for the template

set tpath = `@FindAfniDsetPath $template`
if( $tpath == '' ) then
  echo
  echo ".. Failed to find template $template -- Will not template-ize data .."
  set do_template = NO
  unset AAtemplate
else
  echo
  echo ".. Found template $template -- Will template-ize data .."
  set do_template = YES
  set AAtemplate = ${tpath}/${template}
endif

###-----------------------------------------------
### Create the output directory for all processing
###-----------------------------------------------

set outdir = ZZZ_${subj}
if ( -d $outdir ) then
  echo "***** Output directory $outdir already exists! Script ends now. *****"
  exit 1
endif

echo "..... Creating output directory $outdir ....."
mkdir $outdir

###-----------------------------------------------------------------------------
### These shell variables define
###  echolist = arry of filename suffixes for the multiple echoes
###  echoexpr = 3dcalc expression for combining the multiple echo files into one
###             later echoes are given more weight, since they are weaker
###  echoroot = particular suffix for the first echo
###  NOTE: This script is hard-coded in places for exactly 3 echoes!!!
###        With some effort, it could be generalized.
###-----------------------------------------------------------------------------

set echolist = ( E01 E02 E03 )
set echoexpr = 'a+2*b+3*c'
set eroot    = $echolist[1]

###----------------------------------------------------------------------
### These shell variables define the 'root' names for the EPI datasets
### -- that is, the filenames minus the echo suffix and the .nii
### Note that all datasets are assumed to be uncompressed NIfTI-1
###   calib_for_root = root name for Forward EPI calibration datasets
###   calib_rev_root = root name for Reverse EPI calibration datasets
###   other_for_root = array of root names for other Forward EPI datasets
###                    (that is, those to be corrected by this script)
###----------------------------------------------------------------------

# these _root variables are single names, so easy to compute;
# also, they are not actually used in this script at this time

set calib_for_root = ${subj}_Calib_Forward_${eroot}
set calib_rev_root = ${subj}_Calib_Reverse_${eroot}

# other_for_root array must be computed one element at a time;
# this array IS used below

set temp = ( ${subj}_Rest*_${eroot}.nii )
set other_for_root = ( )
foreach eee ( $temp )
  set other_for_root = ( $other_for_root `basename $eee _${eroot}.nii` )
end
unset temp

###--------------------------------------------------------------------
### Define shell array variables holding various subsets of the data
###   all_anat       = all anat datasets
###   calib_for_list = all Forward EPI calibration datasets
###   calib_ref_list = all Reverse EPI calibration datasets
###   other_for_list = all other Forward EPI datasets (the 'real' data)
###--------------------------------------------------------------------

set all_anat       = ( ${subj}_Anat_E??.nii )
set calib_for_list = ( ${subj}_Calib_Forward_E??.nii )
set calib_rev_list = ( ${subj}_Calib_Reverse_E??.nii )
set other_for_list = ( ${subj}_Rest*_E??.nii )

###-----------------------------------------------------------
### Copy all datasets into the output directory for processing
###-----------------------------------------------------------

echo "..... Copying input datasets into $outdir ....."

cp -p $calib_for_list $calib_rev_list $other_for_list $all_anat $outdir
cd $outdir

# ====================================================================
# ================ Process the EPI calibration images ================

#----------------------------------------------------------------------
# First, make median (over time) image for each EPI calibration run,
# for each echo.  These get filenames ending in _M.nii, and their names
# go into shell array variables for further looping later.

echo "..... Median of EPI Calibration runs ....."
sleep 1

#---- loop over forward calibration runs --------------

set calib_for_listM = ( )
foreach eee ( $calib_for_list )
# extract basename of input file to help create the prefix for the output
  set bbb = `basename $eee .nii`
  3dTstat -prefix ${bbb}_M.nii -median $eee
  set calib_for_listM = ( $calib_for_listM ${bbb}_M.nii )
end

#---- loop over reverse calibration runs --------------

set calib_rev_listM = ( )
foreach eee ( $calib_rev_list )
  set bbb = `basename $eee .nii`
  3dTstat -prefix ${bbb}_M.nii -median $eee
  set calib_rev_listM = ( $calib_rev_listM ${bbb}_M.nii )
end

#--------------------------------------------------------------------
# Second, combine the above medians of each echo for each calibration
# run.  The output dataset names end in _CForM for the forward blip
# case, and in _CRevM for the reverse blip case.

echo "..... Merge EPI Calibration multi-echo datasets ....."
sleep 1

#---- Single volume representing the Forward EPI Calibration state ----

3dcalc -prefix ${subj}_CForM.nii \
       -expr "$echoexpr"         \
       -a $calib_for_listM[1]    \
       -b $calib_for_listM[2]    \
       -c $calib_for_listM[3]

#---- Single volume representing the Reverse EPI Calibration state ----

3dcalc -prefix ${subj}_CRevM.nii \
       -expr "$echoexpr"         \
       -a $calib_rev_listM[1]    \
       -b $calib_rev_listM[2]    \
       -c $calib_rev_listM[3]

#-------------------------------------------------------------------
# Third, remove any residual skull stuff from these combined images;
# the filenames end in MS.nii = Median Skullstripped.
# This step is done to prevent stray stuff outside the brain from
# distorting the registration/warping results.

echo "..... Skull strip EPI Calibration datasets ....."

# These jobs are started in parallel, since 3dSkullStrip is slow,
# and is not multi-threaded

time 3dSkullStrip -prefix ${subj}_CForMS.nii -input ${subj}_CForM.nii &
time 3dSkullStrip -prefix ${subj}_CRevMS.nii -input ${subj}_CRevM.nii &

# the 'wait' command pauses until all bg jobs finish
wait

# make a mask from the skull stripped volumes, for later magical use

### 3dcalc -datum byte -nscale -expr 'step(step(a)+step(b))' \
###        -a ${subj}_CForMS.nii -b ${subj}_CRevMS.nii       \
###        -prefix ${subj}_SSmask.nii

#--------------------------------------------------------------
# Fourth, 3dQwarp the 2 calibration images together
#   MINUS => 'For' image goes with the base   = Forward dataset
#   PLUS  => 'Rev' image goes with the source = Reverse dataset
# The 'other' EPI datasets are assumed later to be warped
# in the same way that the 'For' datasets are warped.

echo
echo "..... 3dQwarp the Calibration datasets together ....."

# The parameters below were chosen by trial and error to give
# decent-looking results for the sample datasets provided by the Spaniard

# 3dQwarp options explained:
#   -plusminus  means find the intermediate position between the 2 inputs
#   -pmNAMES    means give the source warp-to-middle the name 'Rev',
#                 and give the base warp-to-middle the name 'For'
#   -noweight   means that a binary automask is used for weighting, rather
#                 than the weight being proportional to image intensity
#   -minpatch 9 means the smallest patch is 9x9x9 voxels (27 mm here)
#                 -- this is the smallest patch allowed by 3dQwarp
#                 -- a larger minpatch (say 13) runs faster but warping
#                    results don't look so good in a few places
#   -base and -source are the input dataset names
#   -prefix gives the output dataset names -- which will have
#           the -pmNAMES suffixes attached appropriately
#
# 3dQwarp does allow the user to specify warping only in one direction;
# I did not try that here, so the warps produced are true 3D warps.
#
# Each patch has 2 cubic polynomials (in each direction), so the
# "resolution" of the warp is 27/2 = 13.5 mm in some sense -- that is,
# there is one warp parameter (for each axis x,y,z) every 13.5 mm.

3dQwarp -plusminus -pmNAMES Rev For   \
        -pblur 0.05 0.05 -blur -1 -1  \
        -noweight -minpatch 9         \
        -source ${subj}_CRevMS.nii    \
        -base ${subj}_CForMS.nii      \
        -prefix ${subj}_WMid.nii

# At this point, the unwarped calibration datasets
# (which are also median-ized thru time and combined across echoes)
# are named
#   ${subj}_WMid_For.nii and its warp is ${subj}_WMid_For_WARP.nii
# and
#   ${subj}_WMid_Rev.nii and its warp is ${subj}_WMid_Rev_WARP.nii
# The 'Rev' calibration is not needed below, since there are
# no reverse EPI 'real' datasets to process.

# Make a mask from the above results, for later magical incantations

3dAutomask -prefix ${subj}_EPImask.nii               \
           -clfrac 0.2468                            \
           ${subj}_WMid_For.nii

#--------------------------------------------------------------------
# Fifth, unwarp the median-ized calibration datasets for each
# echo case, by using 3dNwarpApply on them.  This is not strictly
# necessary, since we don't need them for anything later, but someone
# might want to look at them?
# The output filenames here contain Wmid, indicating they are
# Warped to the mid-dle position (between the 2 blip cases)

#-----
if( $skip_medianized_unwarp == YES ) goto Register_Other_EPI
#-----

echo
echo ".......... Unwarp the median Calibration datasets .........."

#---- loop over forward calibration median datasets ----

foreach eee ( $calib_for_listM )
  set bbb = `basename $eee .nii`
  3dNwarpApply -nwarp ${subj}_WMid_For_WARP.nii \
               -source $eee                     \
               -prefix ${bbb}_WMid.nii -$interp_mode
end

#---- loop over reverse calibration median datasets ----

foreach eee ( $calib_rev_listM )
  set bbb = `basename $eee .nii`
  3dNwarpApply -nwarp ${subj}_WMid_Rev_WARP.nii \
               -source $eee                     \
               -prefix ${bbb}_WMid.nii -$interp_mode
end

#------------------
Register_Other_EPI:
#------------------

# =================================================================================
# ========== Process the other EPI datasets (i.e., in the Forward mode) ===========

echo
echo "..... Time-shifting, Unwarping, Allineate-ing the other EPI datasets ....."

#---- loop over other forward datasets = those that need fixing for analysis ----

# The procedure is
#  (1) time shift each 'other' dataset                       == suffix _H
#  (2) combine each set of echoes into a single time
#      series dataset, and also mask the result              == suffix _HC
#  (3) unwarp each of these combined datasets                == suffix _HCW
#  (4) volume register the _HCW datasets to get the
#      alignment parameters for each set of echoes
#      -- the purpose of doing things in this order is to
#         avoid doing separate registrations for each echo in
#         a set of datasets that were acquired simultaneously
#      -- we do NOT save this registered dataset, just the
#         registration matrix (.aff12.1D) for use below
#  (5) apply the warp AND the registration matrix
#      from (4) to get the individual echo aligned datasets == suffix _HWV

# this array is used for warping directly to a template at the end
set other_for_listB = ( )

#-- (1) time shift each EPI dataset [loop over all such datasets] --

foreach eee ( $other_for_list )
  set bbb = `basename $eee .nii`
  3dTshift -tzero 0 -quintic -ignore $tshift_ignore -prefix ${bbb}_H.nii $eee
end

#-- (2) combine echoes from a set of datasets --
#-- (3) then unwarp the result of (2)         --
#-- (4) then 3dAllineate the output of (3)    --
#-- (5) then 3dNwarpApply all echoes at once  --

set regbase = ${subj}_WMid_For.nii

#-- [loop over each root for a set of datasets] --

foreach rrr ( $other_for_root )
  3dcalc -prefix ${rrr}_HC.nii       \
         -expr "($echoexpr)*step(m)" \
         -a ${rrr}_$echolist[1].nii  \
         -b ${rrr}_$echolist[2].nii  \
         -c ${rrr}_$echolist[3].nii  \
         -m ${subj}_EPImask.nii

  3dNwarpApply -nwarp ${subj}_WMid_For_WARP.nii   \
               -source ${rrr}_HC.nii              \
               -prefix ${rrr}_HCW.nii -$interp_mode

# 3dAllineate is used here instead of 3dvolreg since
# it gives better results -- my guess is that the
# difference in contrast/shading between the base
# and source datasets gives 3dvolreg troubles

  time 3dAllineate -base $regbase                   \
              -source ${rrr}_HCW.nii                \
              -prefix NULL                          \
              -1Dmatrix_save ${rrr}_HWV.aff12.1D    \
              -1Dparam_save  ${rrr}_HWV.motion.1D   \
              -warp shift_rotate -onepass           \
              -fineblur 2 -lpa -norefinal           \
              -final quintic -automask+2 -quiet

#-- build a list of time_shifted datasets with this root --

  set temp = ( )
  foreach eee ( $echolist )
    set temp = ( $temp ${rrr}_${eee}_H.nii )
  end

#-- The next command warps all 3 echoes at once using the combined
#-- affine warp for each time point (from 3dAllineate) and
#-- nonlinear warp (from 3dQwarp); the output datasets get the
#-- same name as the inputs with the suffix '_HWV': these are
#-- one major result of this script!

#-- The order of warps is in order from final space to original space,
#-- since they are applied to the final space x-coordinates in the
#-- order given. In this case, the data are registered after bein
#-- unwarped, so the transformation back from output space to input
#-- space is un-register first, then un-unwarp.
#-- No '-master' option is given here, since the source 3D grid is
#-- reasonable for these outputs.

  3dNwarpApply -nwarp "${rrr}_HWV.aff12.1D ${subj}_WMid_For_WARP.nii" \
               -source $temp -suffix WV -$interp_mode
  unset temp

end

echo
echo "..... Time-shifted, Unwarped, Allineated EPI datasets finished ....."
echo *_HWV.nii

# ==============================================================
# ===== Process the anatomical datasets into a single one ======

#-- First, average them (the sample data had 4 Anats for some reason)

echo
echo "..... Average Anat datasets ....."

3dMean -prefix ${subj}_Anat_A.nii $all_anat

#-- Second, afffinely align this average anat to the Forward unwarped
#-- EPI calibration dataset

echo
echo "..... Affinely align Anat to un-warped EPI Forward dataset ....."

time align_epi_anat.py -anat ${subj}_Anat_A.nii          \
                       -epi ${subj}_WMid_For.nii         \
                       -epi_base 0 -epi_strip None       \
                       -big_move -cmass cmass            \
                       -Allineate_opts '-twobest 11'

#-- and make the result be in NIfTI-1 format, for G-d's sake!

3dcopy ${subj}_Anat_A_al+orig.HEAD ${subj}_Anat_A_al.nii
\rm ${subj}_Anat_A_al+orig.*

#--- Nonlinearly warp anat to template, if the template was provided

if( $do_template == YES )then
  echo
  echo "..... Warp Anat to Template $AAtemplate [this will be slow] ....."

#-- make a local copy of the template

  3dcopy $AAtemplate ./`basename $AAtemplate`

#-- uniform-ize the average anat (since the template is uniform-ized)

  time 3dUnifize -GM -prefix ${subj}_Anat_AU_al.nii -input ${subj}_Anat_A_al.nii

#-- warp the result to match the template (takes a while!)

  3dQwarp -base   $AAtemplate             \
          -source ${subj}_Anat_AU_al.nii  \
          -minpatch $template_minpatch    \
          -prefix ${subj}_Anat_Qtem.nii   \
          -allin -pblur

#-- transform all original EPI datasets directly to template space

  echo
  echo "..... and Warp EPI datasets directly to Template ....."

  foreach rrr ( $other_for_root )
# build list of time-shifted datasets with this root
    set temp = ( )
    foreach eee ( $echolist )
      set temp = ( $temp ${rrr}_${eee}_H.nii )
    end
    3dNwarpApply                                                                         \
       -nwarp "${subj}_Anat_Qtem_WARP.nii ${rrr}_HWV.aff12.1D ${subj}_WMid_For_WARP.nii" \
       -source $temp -suffix WV_Qtem -$interp_mode -master $AAtemplate -dxyz 2.0
    unset temp
  end

  echo
  echo "..... Templatized EPI datasets finished ....."
  echo *_HWV_Qtem.nii

endif

# ===== Vamoose the ranch ======

echo
echo "................................"
echo "..... Hasta la vista, baby ....."
echo "................................"
echo

cd ..
time
exit 0
