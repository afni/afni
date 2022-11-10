#!/bin/tcsh

@global_parse `basename $0` "$*" ; if ($status) exit 0

# ----------------------------------------------------------------------
# Look for bars of high TSNR that might suggest scanner interference.
#
# inputs: multiple runs of EPI datasets
# output: a directory containing
#         - stdandard deviation maps per run: original and scaled
#         - per column (voxel across slices) averaged
#         - cluster reports and x,y coordinates high averages
#
# steps:
#    - automask and erode
#    - detrend at regress polort level
#    - compute temporal standard deviation volume
#    - get 90th %ile in volume mask
#    - scale stdev to val/90%, with max of 1
#    - Localstat -mask mean over columns
#
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# main input variables ($run is the typical one to set)
set din_list   = ( )            # input datasets
set do_clean   = 1              # do we remove temporary files
set mask_in    = ''             # any input mask (possibly renamed)
set nfirst     = 3              # number of first time points to exclude
set nneeded    = 10             # minimum time series length (after nfirst)
set percentile = 90             # for stdev limit
set polort     = A              # polort for trend removal (A = auto)
set rdir       = tlines.result  # output directory
set thresh     = 0.95           # threshold for tscale average

set DO_IMG     = 1
set MAX_IMG    = 7

set prog = tsnr_line_hunter.tcsh

set version = "0.0, 10 Nov, 2022"

if ( $#argv < 1 ) goto SHOW_HELP

set ac = 1
while ( $ac <= $#argv )
   # look for terminal options, first
   if ( "$argv[$ac]" == "-help" ) then
      goto SHOW_HELP
   else if ( "$argv[$ac]" == "-hist" ) then
      goto SHOW_HIST
   else if ( "$argv[$ac]" == "-ver" ) then
      echo "version $version"
      exit
   else if ( "$argv[$ac]" == "-echo" ) then
      set echo

   # general processing options
   else if ( "$argv[$ac]" == "-nfirst" ) then
      @ ac += 1
      set nfirst = $argv[$ac]
   else if ( "$argv[$ac]" == "-mask" ) then
      @ ac += 1
      set mask_in = $argv[$ac]
   else if ( "$argv[$ac]" == "-percentile" ) then
      @ ac += 1
      set percentile = $argv[$ac]
   else if ( "$argv[$ac]" == "-polort" ) then
      @ ac += 1
      set polort = $argv[$ac]
   else if ( "$argv[$ac]" == "-rdir" ) then
      @ ac += 1
      set rdir = $argv[$ac]

   # otherwise, these should be the input datasets
   else
      # the rest are assumed to be input datasets
      set din_list = ( $argv[$ac-] )
      break
   endif
   @ ac += 1
end


# if there are input remaining datasets, bail
if ( $#din_list < 1 ) then
   echo "** missing input datasets (they should come as trailing arguments"
   echo ""
   exit
endif

# ----------------------------------------------------------------------
# check for sufficient NT, and note the number of slices
@ nneeded += $nfirst
set nk_list = ()
foreach dset ( $din_list )
   set nstuff = `3dinfo -nt -nk $dset`
   if ( $status || "$nstuff[1]" == "NO-DSET" ) then
      echo ""
      echo "** failed to get NT,NK from dset $dset"
      echo ""
      exit 1
   else if ( $nstuff[1] < $nneeded ) then
      echo ""
      echo "** require $nneeded TRs, but have $nstuff[1] in $dset, skipping..."
      echo "   (accounting for nfirst = $nfirst)"
      echo ""
      exit 1
   endif

   # dataset looks good, make note of nslices
   set nk_list = ( $nk_list $nstuff[2] )
end

echo "++ have nslices : $nk_list"

# ----------------------------------------------------------------------
# make results dir, enter it and remove old results
if ( -d $rdir ) then
   echo "** error, results dir already exists: $rdir"
   exit 
endif

mkdir $rdir
if ( $status ) then
   echo "** failed to create results directory, $rdir"
   exit
endif

# ----------------------------------------------------------------------
# copy inputs (minus pre-ss trs) to results dir

set dset_list = ()
foreach index ( `count -digits 1 1 $#din_list` )
   set inset = $din_list[$index]

   set ichr  = r     # r or d, for run or dset
   set ind02 = `ccalc -form '%02d' $index`

   set dnew = ts.0.orig.r$ind02.nii.gz
   echo "++ copying $inset:t[$nfirst.."\$] to $dnew
   3dTcat -prefix $rdir/$dnew $inset\[$nfirst..\$]
   set dset_list = ( $dset_list $dnew )

   if ( $status ) then
       echo "** failed to copy EPI dataset $inset, exiting..."
       exit
   endif
end

# if the mask is a dataset, copy to a local name
if ( $mask_in != "" && $mask_in != AUTO && $mask_in != NONE ) then
   3dcopy $mask_in $rdir/mask.nii.gz
   if ( $status ) then
       echo "** failed to copy -mask dataset $mask_in, exiting..."
       exit
   endif
   set mask_in = mask.nii.gz
endif

# ----------------------------------------------------------------------
# enter results dir
cd $rdir

# ----------------------------------------------------------------------
# main work: compute correlation datasets and surviving threshold fractions

# if the user wants an automask, make one
if ( $mask_in == AUTO ) then
   set mask_in = mask.nii.gz
   echo "++ creating eroded automask from first dset"
   3dAutomask -erode 1 -prefix $mask_in $dset_list[1]
   if ( $status ) then
      exit 1
   endif
endif

# set the masking option
# (mask is either NONE or mask.nii.gz)
if ( $mask_in == "" ) then
   set mask_in = NONE
endif

set mask_opt = ""
if ( $mask_in != NONE ) then
   echo "-- will apply mask $mask_in"
   set mask_opt = ( -mask $mask_in )
endif

# if detrending is A or AUTO, compute degree
if ( $polort == A || $polort == AUTO ) then
   set vals = ( `3dinfo -nt -tr $dset_list[1]` )
   if ( $status ) exit
   set polort = \
       `afni_python_wrapper.py -print "get_default_polort($vals[1],$vals[2])"`
   if ( $status ) exit
   echo "-- using AUTO polort $polort for detrending"
endif

# count the number of bad columnar regions per input
set bad_counts = ()

foreach index ( `count -digits 1 1 $#dset_list` )

   set ind02 = `ccalc -form '%02d' $index`
   set dset = $dset_list[$index]
   set nk = $nk_list[$index]

   # possibly detrend
   if ( $polort >= 0 ) then
      set newset = ts.1.det.r$ind02.nii.gz
      echo "-- detrend -polort $polort, new eset = $newset"
      3dTproject -quiet -polort $polort -prefix $newset -input $dset
      # and replace $dset with the new one
      set dset = $newset
   endif

   # compute stdev dset
   set sset = stdev.0.orig.r$ind02.nii.gz
   3dTstat -stdev -prefix $sset $dset

   # get 90%ile in mask
   set perc = ( `3dBrickStat $mask_opt -percentile 90 1 90 -perc_quiet $sset` )
   if ( $status ) exit

   # scale to fraction of 90%ile (max 1)
   set scaleset = stdev.1.scale.r$ind02.nii.gz
   3dcalc -a $sset -expr "min(1,a/$perc)" -prefix $scaleset
   set sset = $scaleset

   # project the masked mean across slices
   set pset = proj.r$ind02.nii.gz
   3dLocalstat $mask_opt -nbhd "Rect(0,0,-$nk)" -stat mean \
               -prefix $pset $sset

   # now threhold and cluster (slices are equal, so 1-D == 3-D)
   set cfile = bad_clust.r$ind02.txt
   3dClusterize -ithr 0 -idat 0 -NN 3 -inset $pset -2sided -1 $thresh \
                | tee $cfile

   set bfile = bad_coords.r$ind02.txt
   grep -v '#' $cfile | awk '{printf "%7.1f %7.1f %7.1f\n", $14, $15, $16}' \
        | tee $bfile

   set bad_counts = ( $bad_counts `cat $bfile | wc -l` )
end  # foreach index

if ( ${DO_IMG} ) then
   echo "++ Check about making images"
   set all_bfile = ( bad_coords*.txt )
   set count     = 0                         # count up to MAX_IMG

   foreach bfile ( ${all_bfile} ) 
      set run  = "${bfile:r:e}"              # get run string
      set nbad = `cat $bfile | wc -l`        # number of bad points
      set rfile = ( stdev.*.scale.${run}.nii.gz )

      if ( ${nbad} ) then
         foreach line ( `seq 1 1 ${nbad}` )
            if ( `echo "$count < $MAX_IMG" | bc` ) then
               set coords = `sed -n "${line}p" $bfile`

               # make the dset with the dashed lines to overlay
               set nk     = `3dinfo -nk "${rfile}"`
               set dash   = `echo "scale=0; ${nk}/8" | bc`
               set ad3    = `3dinfo -ad3 "${rfile}"`
               3dcalc                                     \
                  -overwrite                              \
                  -a    ${rfile}                          \
                  -expr "within(x,${coords[1]}-0.25*${ad3[1]},${coords[1]}+0.25*${ad3[1]})*within(y,${coords[2]}-0.25*${ad3[2]},${coords[2]}+0.25*${ad3[2]})*(step(${dash}-k)+step(k-${nk}+${dash}))" \
                  -prefix __tmp_dash_line.nii.gz          \
                  -datum byte

               # [PT] to do: change displayed text, change output
               #      image file numbering and a couple other niceities.
               #      - add in 2dcat command
               #      - dump other useful text info for APQC

               # make image
               @chauffeur_afni                                        \
                  -ulay               ${rfile}                        \
                  -olay               __tmp_dash_line.nii.gz          \
                  -pbar_posonly                                       \
                  -cbar               "Reds_and_Blues_Inv"            \
                  -func_range         1                               \
                  -set_subbricks      0 0 0                           \
                  -thr_olay           0.9                             \
                  -olay_alpha         Yes                             \
                  -olay_boxed         Yes                             \
                  -set_dicom_xyz      ${coords}                       \
                  -opacity            9                               \
                  -blowup             4                               \
                  -no_cor -no_axi                                     \
                  -montx 1 -monty 1                                   \
                  -prefix             img_${run}_${line}              \
                  -set_xhairs OFF                                     \
                  -label_mode 1 -label_size 4                         \

               \rm __tmp_dash_line.nii.gz 
               @ count += 1
            endif
         end
      endif
   end
endif


# ----------------------------------------------------------------------
# if do_clean, clean up and run away
if ( $do_clean == 1 ) then
   echo "++ have do_clean, removing time series..."
   echo ""

   \rm -f ts*
endif


# ----------------------------------------------------------------------
# babble about the results

echo ""
echo "== found questionable regions across inputs: $bad_counts"
echo ""
foreach file ( bad_coords.r*.txt )
   echo =============== $file ===============
   cat $file
   echo ""
end


# ===========================================================================
# DONE
# ===========================================================================

exit


# ===========================================================================
# begin terminal goto sections:

SHOW_HELP:
# ----------------------------------------------------------------------
# help string

cat << EOF
---------------------------------------------------------------------------
$prog       - look for high TSNR columns (across z) in time series data

   usage : $prog [options] datasets ..."

Look for bars of high TSNR that might suggest scanner interference.

   inputs: multiple runs of EPI datasets
   output: a directory containing
           - stdandard deviation maps per run: original and scaled
           - per column (voxel across slices) averaged
           - cluster reports and x,y coordinates high averages

   steps:
      - automask and erode
      - detrend at regress polort level
      - compute temporal standard deviation volume
      - get 90th %ile in volume mask
      - scale stdev to val/90%, with max of 1
      - Localstat -mask mean over columns

Blah blah blah, blah blah, blah.

- R Reynolds, P Taylor, D Glen
EOF

SHOW_HIST:

cat << EOF

-----------------------------------------------------------------
$prog modification history:

   0.0  10 Nov 2022 : something to play with

EOF
# check $version, at top

exit
