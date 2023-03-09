#!/bin/tcsh

@global_parse `basename $0` "$*" ; if ($status) exit 0

# ----------------------------------------------------------------------
# Look for bars of high variance that might suggest scanner interference.
#
# inputs: multiple runs of EPI datasets
# output: a directory containing
#         - stdandard deviation maps per run: original and scaled
#         - per column (voxel across slices) averaged
#         - cluster reports and x,y coordinates high averages
#
# steps:
#    - automask, erode and require min column height
#    - detrend at regress polort level
#    - compute temporal variance volume
#    - get 90th %ile in volume mask
#    - scale variance to val/90%, with max of 1
#    - Localstat -mask mean over columns
#    - 
#
#    * consider Color_circle_AJJ (tighter color ranges up top)
#
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# main input variables ($run is the typical one to set)
set din_list   = ( )            # input datasets
set do_clean   = 1              # do we remove temporary files
set do_img     = 1              # make images
set mask_in    = 'AUTO'         # any input mask (possibly renamed)
set max_img    = 7              # maximum number of high-var images to make
set min_cvox   = 5              # minimum voxels in a column
set min_nt     = 10             # minimum time series length (after nfirst)
set nerode     = 0              # number of mask erosions
set nfirst     = 0              # number of first time points to exclude
set perc       = 90             # percentile limit of variance
set polort     = A              # polort for trend removal (A = auto)
set rdir       = vlines.result  # output directory
set thresh     = 0.95           # threshold for tscale average


set prog = find_variance_lines.tcsh

set version = "0.4, 23 Nov, 2022"

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
      exit 0

   # general processing options
   else if ( "$argv[$ac]" == "-do_clean" ) then
      @ ac += 1
      set do_clean = $argv[$ac]
   else if ( "$argv[$ac]" == "-do_img" ) then
      @ ac += 1
      set do_img = $argv[$ac]
   else if ( "$argv[$ac]" == "-echo" ) then
      set echo
   else if ( "$argv[$ac]" == "-mask" ) then
      @ ac += 1
      set mask_in = $argv[$ac]
   else if ( "$argv[$ac]" == "-max_img" ) then
      @ ac += 1
      set max_img = $argv[$ac]
   else if ( "$argv[$ac]" == "-min_cvox" ) then
      @ ac += 1
      set min_cvox = $argv[$ac]
   else if ( "$argv[$ac]" == "-min_nt" ) then
      @ ac += 1
      set min_nt = $argv[$ac]
   else if ( "$argv[$ac]" == "-nerode" ) then
      @ ac += 1
      set nerode = $argv[$ac]
   else if ( "$argv[$ac]" == "-nfirst" ) then
      @ ac += 1
      set nfirst = $argv[$ac]
   else if ( "$argv[$ac]" == "-perc" ) then
      @ ac += 1
      set perc = $argv[$ac]
   else if ( "$argv[$ac]" == "-polort" ) then
      @ ac += 1
      set polort = $argv[$ac]
      if ( $polort == AUTO ) then
         set polort = A
      else if ( $polort == NONE ) then
         set polort = -1
      endif
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
   exit 0
endif

# ----------------------------------------------------------------------
# check for sufficient NT, and note the number of slices
@ min_nt += $nfirst
set nk_list = ()
foreach dset ( $din_list )
   set nstuff = `3dinfo -nt -nk $dset`
   if ( $status || "$nstuff[1]" == "NO-DSET" ) then
      echo ""
      echo "** failed to get NT,NK from dset $dset"
      echo ""
      exit 1
   else if ( $nstuff[1] < $min_nt ) then
      echo ""
      echo "** require $min_nt TRs, but have $nstuff[1] in $dset, skipping..."
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
   exit 0
endif

mkdir $rdir
if ( $status ) then
   echo "** failed to create results directory, $rdir"
   exit 0
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
       exit 1
   endif
end

# if the mask is a dataset, copy to a local name
if ( $mask_in != "" && $mask_in != AUTO && $mask_in != NONE ) then
   3dcopy $mask_in $rdir/mask.nii.gz
   if ( $status ) then
       echo "** failed to copy -mask dataset $mask_in, exiting..."
       exit 1
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
   echo "++ creating automask from first dset"
   3dAutomask -prefix $mask_in $dset_list[1]
   if ( $status ) then
      exit 1
   endif
   echo ""
endif

# --------------------------------------------------
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

# --------------------------------------------------
# if we have mask, possibly erode and trim short columns
if ( $mask_in != NONE ) then
   # if mask, verify consistent dimensions
   set nzero = `3dinfo -same_dim $mask_in $dset_list | grep 0 | wc -l`
   if ( $nzero != 0 ) then
      echo "** cannot have dimensionality difference with a mask"
      3dinfo -same_dim -prefix $mask_in $dset_list
      exit 1
   endif

   # apply any mask erosions
   if ( $nerode > 0 ) then
      echo "++ eroding $mask_in by $nerode voxels"
      \mv $mask_in tmp.mask.nii.gz
      3dmask_tool -prefix $mask_in -input tmp.mask.nii.gz \
                  -dilate_inputs -$nerode
      echo ""
   endif

   # apply min_cvox
   if ( $min_cvox > 0 ) then
      echo "++ requiring $min_cvox voxels in mask columns"
      set nk = $nk_list[1]
      set cset = tmp.mask.col.count.nii.gz
      3dLocalstat $mask_opt -nbhd "Rect(0,0,-$nk)" -stat sum \
               -prefix $cset $mask_in
      \mv $mask_in tmp.mask.nii.gz
      3dcalc -a tmp.mask.nii.gz -b $cset -expr "a*step(b+1-$min_cvox)" \
             -prefix $mask_in
      echo ""
   endif
endif

# --------------------------------------------------
# if detrending is A or AUTO, compute degree
if ( $polort == A ) then
   set vals = ( `3dinfo -nt -tr $dset_list[1]` )
   if ( $status ) exit 1
   set polort = \
       `afni_python_wrapper.py -print "get_default_polort($vals[1],$vals[2])"`
   if ( $status ) exit 1
   echo "-- using AUTO polort $polort for detrending"
endif

# --------------------------------------------------
# make note of a central z-coordinate, since 'z' will
# not matter in projection dsets
set zcoord = `3dinfo -dcz $dset_list[1]`

# ---------------------------------------------------------------------------
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
      echo ""
   endif

   # compute temporal variance dset (square stdev for now)
   set sset = var.0.orig.r$ind02.nii.gz
   3dTstat -stdevNOD -prefix tmp.stdev.nii.gz $dset
   3dcalc -prefix $sset -a tmp.stdev.nii.gz -expr 'a*a'
   \rm tmp.stdev.nii.gz

   # get 90%ile in mask
   set pp = ( `3dBrickStat $mask_opt -percentile $perc 1 $perc \
                           -perc_quiet $sset` )
   if ( $status ) exit 1

   # scale to fraction of 90%ile (max 1)
   set scaleset = var.1.scale.r$ind02.nii.gz
   3dcalc -a $sset -expr "min(1,a/$pp)" -prefix $scaleset
   set sset = $scaleset

   # project the masked mean across slices
   set pset = proj.r$ind02.nii.gz
   3dLocalstat $mask_opt -nbhd "Rect(0,0,-$nk)" -stat mean \
               -prefix $pset $sset

   # now threshold and cluster (slices are equal, so 1-D == 3-D)
   set cfile = bad_clust.r$ind02.txt
   3dClusterize -ithr 0 -idat 0 -NN 3 -inset $pset -2sided -1 $thresh \
                | tee $cfile

   set bfile = bad_coords.r$ind02.txt
   grep -v '#' $cfile                                                     \
        | awk '{ z='$zcoord'; printf "%7.2f %7.2f %7.2f\n", $14, $15, z}' \
        | tee $bfile

   set bad_counts = ( $bad_counts `cat $bfile | wc -l` )
end  # foreach index
echo ""

# ---------------------------------------------------------------------------
# report intersection clusters from projection dsets
echo "-- evaluating intersection..."
set pset = proj.min.nii.gz
3dMean -min -prefix proj.min.nii.gz proj.r*.nii.gz

set cfile = bad_clust.inter.txt
3dClusterize -ithr 0 -idat 0 -NN 3 -inset $pset -2sided -1 $thresh \
             | tee $cfile
set bfile = bad_coords.inter.txt
grep -v '#' $cfile                                                    \
     | awk '{z='$zcoord'; printf "%7.2f %7.2f %7.2f\n", $14, $15, z}' \
     | tee $bfile

# ---------------------------------------------------------------------------
# create images pointing to vlines
if ( $do_img ) then
   # for the user to see, but mainly to be used in APQC; just using
   # simple text recording here, leave most work for APQC prog

   echo "++ Check about making images"
   set all_bfile = ( bad_coords.inter.txt bad_coords.r*.txt )

   # if only 1 run, just refer to r01, not inter (which always exists)
   if ( "${#all_bfile}" == "2" ) then
      set all_bfile = ( bad_coords.r*.txt )
   endif

   # text for under image in APQC
   set text    = ""
   set count   = 0                            # count up to max_img

   foreach bb ( `seq 1 1 ${#all_bfile}` ) 
      set bfile = "${all_bfile[$bb]}"
      set name = "${bfile:r:e}"              # get inter/run string
      set nbad = `cat $bfile | wc -l`        # number of bad points

      if ( "${name}" == "inter" ) then
         set run = "r01"    # special case: use r01 to show intersection pts
      else
         set run = ${name}
      endif

      set rfile = ( var.*.scale.${run}.nii.gz )

      if ( ${nbad} ) then
         foreach ii ( `seq 1 1 ${nbad}` )
            if ( `echo "$count < $max_img" | bc` ) then
               set iii    = `printf "%02d" $ii`
               set coords = `sed -n "${ii}p" $bfile`
               set lab    = "${name}:${ii}"

               set text   = "${text:q} ${lab},"

               if ( `echo "${bb} % 2" | bc` ) then
                  set val = 1.0
               else
                  set val = 0.8
               endif

               # make the dset with the dashed lines to overlay
               set nk     = `3dinfo -nk "${rfile}"`         # nvox along k
               set dash   = `echo "scale=0; ${nk}/8" | bc`  # len of dash
               set ad3    = `3dinfo -ad3 "${rfile}"`
               3dcalc                                     \
                  -overwrite                              \
                  -a    ${rfile}                          \
                  -expr "${val}*within(x,${coords[1]}-0.25*${ad3[1]},${coords[1]}+0.25*${ad3[1]})*within(y,${coords[2]}-0.25*${ad3[2]},${coords[2]}+0.25*${ad3[2]})*(step(${dash}-k)+step(k-${nk}+${dash}))" \
                  -prefix __tmp_dash_line.nii.gz          \
                  -datum byte

               # make image
               @chauffeur_afni                                        \
                  -ulay               ${rfile}                        \
                  -olay               __tmp_dash_line.nii.gz          \
                  -pbar_posonly                                       \
                  -cbar               "Reds_and_Blues_Inv"            \
                  -func_range         1                               \
                  -set_subbricks      0 0 0                           \
                  -thr_olay           0.5                             \
                  -olay_alpha         Yes                             \
                  -olay_boxed         Yes                             \
                  -set_dicom_xyz      ${coords}                       \
                  -opacity            9                               \
                  -blowup             4                               \
                  -no_cor -no_axi                                     \
                  -montx 1 -monty 1                                   \
                  -prefix             img_${name}_${iii}              \
                  -set_xhairs OFF                                     \
                  -label_mode 0

               \rm __tmp_dash_line.nii.gz 
               @ count += 1
            endif   # end count<max_img
         end   # end ii loop
      endif   # end nbad>0
   end   # end bfile loop

   # finish the subtext string (remove last comma)
   set text = `echo ${text:q} | awk '{print substr($0,1,length($0)-1)}'`

   echo ${text} > QC_var_lines.txt

   # combine img files
   if ( ${count} ) then
       set opref = QC_var_lines
       2dcat                                                             \
           -overwrite                                                    \
           -zero_wrap                                                    \
           -gap         1                                                \
           -gap_col     0 0 0                                            \
           -nx ${max_img}                                                \
           -ny 1                                                         \
           -prefix ${opref}.jpg                                          \
           img_*png

      if ( $do_clean == 1 ) then
         \rm img_*png
      endif
   endif
endif # end of do_img

# ----------------------------------------------------------------------
# if do_clean, clean up and run away
if ( $do_clean == 1 ) then
   echo "++ have do_clean, removing time series..."
   echo ""

   \rm -f ts*
endif


# ----------------------------------------------------------------------
# babble about the results

set nbinter = `cat bad_coords.inter.txt | wc -l`
echo ""
echo "== found questionable regions across inputs: $bad_counts"
echo "   found questionable intersected regions  : $nbinter"
echo ""
foreach file ( bad_coords.*.txt )
   echo =============== $file ===============
   cat $file
   echo ""
end


# ===========================================================================
# DONE
# ===========================================================================

exit 0


# ===========================================================================
# begin terminal goto sections:

SHOW_HELP:
# ----------------------------------------------------------------------
# help string

cat << EOF
---------------------------------------------------------------------------
$prog   - look for high temporal variance columns

   usage : $prog [options] datasets ..."

Look for bars of high variance that might suggest scanner interference.

   inputs: multiple runs of EPI datasets
   output: a directory containing
           - variance maps per run: original and scaled
           - cluster reports and x,y coordinates at high averages
           - a JPEG image showing locations of high variance

This program takes one or more runs of (presumably) EPI time series data,
and looks for slice locations with consistently high temporal variance across
the (masked) slices.

   steps:
      - (possibly) automask, erode and require columns of $min_cvox voxels
      - (possibly) detrend at regress polort level, default = $polort
      - compute temporal variance volume
      - get p90 = 90th %ile in volume mask, default %ile = $perc
      - scale variance to val/p90, with max of 1
      - Localstat -mask mean over columns
      - find separate clusters of them

------------------------------------------------------------
Examples:

  1. Run using defaults.

        $prog epi_r1.nii epi_r2.nii epi_r3.nii
          OR
        $prog epi_r*.nii

  2. What would afni_proc.py do?

        $prog -rdir vlines.pb00.tcat -nerode 2 \\
            pb00*tcat*.HEAD |& tee out.vlines.pb00.tcat.txt

  3. Provide a mask (and do not erode).  Do not detrend time series.
     Use the default output directory, $rdir.

        $prog -mask my_mask.nii.gz -polort -1 \\
              epi_run*.nii.gz

------------------------------------------------------------
Options (terminal):

   -help                : show this help
   -hist                : show the version history
   -ver                 : show the current version

Options (processing):

   -do_clean VAL        : do we clean up a little? (def=$do_clean)

                             VAL in {0,1}

                          Remove likely unneeded datasets, particular the
                          large time series datasets.

   -do_img VAL          : make vline images? (def=$do_img)

                             VAL in {0,1}

                          Specify whether to make jpeg images of high
                          variance locations.

   -echo                : run script with shell 'echo' set (def=no)
                          (this is VERY verbose)

                          With this set, it is as if running the (tcsh) as in:

                             tcsh -x .../$prog ...

                          So all shell commands (including setting variables,
                          "if" evaluations, etc.) are shown.  This is useful
                          for debugging.

   -mask VAL            : mask for computations (def=$mask_in)

                             VAL in {AUTO, NONE, dataset}

                          Specify a mask dataset to restrict variance
                          computations to.  VAL should be a dataset, with
                          exception for special cases:

                             AUTO : generate automask with 3dAutomask
                             NONE : do not mask

   -min_cvox VAL        : min voxels for valid mask column (def=$min_cvox)

                             VAL in Z+ (positive integers)

                          In the input or automask, after any eroding, remove
                          voxels that do not have at least 'VAL' voxels in the
                          verticle column.  Otherwise, edge voxels might end
                          up in the result.

   -min_nt VAL          : minimum number of time points required (def=$min_nt)

                             VAL > 1 (integer)

                          This is just a minimum limit to be sure the input
                          time series are long enough to be reasonable.

   -nerode VAL          : how much to erode input or auto-mask (def=$nerode)

                             VAL >= 0 (integer)

                          Specify the number of levels to erode any mask by.
                          "3dmask_tool -dilate -VAL " is used.

   -nfirst VAL          : discard the first VAL time points (def=$nfirst)

                             VAL >= 0 (integer)

                          Specify the number of time points to discard from
                          the start of each run (pre-steady state, presumably).

   -perc VAL            : percentile of variance vals to scale to (def=$perc)

                             VAL in {0..99}

                          When looking for high variance, the values are scaled
                          by this percentile value, with a scaled limit of 1.
                          So if the 90%-ile of variance values were 876.5, then
                          variance would be scaled using v_new = v_old/876.5,
                          with v_new limited to the range [0,1].

                          This allows evaluation relative to a modestly extreme
                          value, without worrying about the exact numbers.

   -polort VAL          : polynomial detrending degree (def=$polort)

                             VAL >= -1 (integer), or in {A,AUTO,NONE}

                          Specify the polynomial degree to use for time series
                          detrending prior to the variance computation.  This
                          should be an integer >= -1 (or a special case).  The
                          default is the same as that used by afni_proc.py and
                          3dDeconvolve, which is based on the duration of the
                          run, in seconds.

                          Special cases or examples:

                                A       : auto = floor(run_duration/150)+1
                                AUTO    : auto = floor(run_duration/150)+1
                                NONE    : do not detrend (same as -1)
                                -1      : do not detrend
                                0       : only remove the mean
                                3       : remove a cubic polynomial trend

   -rdir VAL            : name of the output directory (def=$rdir)

                             VAL is a new directory name

                          All output is put into this results directory.


- R Reynolds, P Taylor, D Glen
  Nov, 2022
  version $version

EOF
exit 0


SHOW_HIST:

cat << EOF

-----------------------------------------------------------------
$prog modification history:

   0.0  10 Nov 2022 : something to play with
   0.1  11 Nov 2022 : [PT] make @chauffeur_afni images
   0.2  14 Nov 2022 : use variance; erode and trim; verify dims,
                      3dmask_tool; report at fixed z; help
   0.3  14 Nov 2022 : [PT] update images and text info
   0.4  23 Nov 2022 : [PT] shell calls not aliased;
                      all exits are belong to integers

EOF
# check $version, at top

exit 0
