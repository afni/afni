#!/usr/bin/env tcsh

# ===========================================================================
# compute ROI statistics (intended for TSNR dset)
#
#    inputs:    dset_ROI      - to restrict statistics to regions
#               dset_data     - to compute statisics from
#               label_roiset  - label for dset_ROI
#               out_dir       - directory to put all results in
#               rval_list     - ROI value list (ints), compute stats per rval
#    outputs:   stats_file    - text file with stats for these ROIs
#
# ===========================================================================

# note program name
set prog = `basename $0`

# ===========================================================================
# program history (skip this on first pass, but allow to jump in)
# ---------------
goto SKIP_HIST
SHOW_HIST:
cat << EOF

-----------------------------------------------------------------
$prog modification history:

   1.0  : Feb 15, 2024: initial version
   1.1  : Feb 20, 2024: actually print computed depth

   current version: $script_version
EOF
exit 0
SKIP_HIST:
set script_version = "version 1.1, February 20, 2024"


# ===========================================================================
# general parameters (have prog and script_version, above)

# ----------------------------------------------------------------------
# required user parameters
set dset_ROI    = ''    # dataset with possibly many ROIs
set dset_data   = ''    # dataset to compute statistics over
set out_dir     = ''    # directory to store work and results in
set rset_label  = ''    # label for ROI dataset
set rval_list   = ()    # ROI values to compute stats over
set stats_file  = ''    # file to store text results in

# ----------------------------------------------------------------------
# other user-controllable parameters
set verb        = 1

# ===========================================================================
# process options (no continue, increment ac after if-then-else block)

if ( $#argv < 1 ) then
   goto SHOW_HELP
endif

set ac = 1
set narg = $#argv
while ( $ac <= $narg )
   # -------------------- main terminal opts --------------------
   if ( "$argv[$ac]" == '-help' ) then
      goto SHOW_HELP

   else if ( "$argv[$ac]" == '-hist' ) then
      goto SHOW_HIST

   else if ( "$argv[$ac]" == '-ver' ) then
      echo "version $script_version"
      exit 0

   # -------------------- main user opts --------------------
   else if ( "$argv[$ac]" == '-dset_data' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -dset_data requires 1 parameter"
         exit 1
      endif
      set dset_data = $argv[$ac]

   else if ( "$argv[$ac]" == '-dset_ROI' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -dset_ROI requires 1 parameter"
         exit 1
      endif
      set dset_ROI = $argv[$ac]

   else if ( "$argv[$ac]" == '-out_dir' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -out_dir requires 1 parameter"
         exit 1
      endif
      set out_dir = $argv[$ac]

   else if ( "$argv[$ac]" == '-rset_label' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -rset_label (ROI dset label) requires 1 parameter"
         exit 1
      endif
      set rset_label = $argv[$ac]

   else if ( "$argv[$ac]" == '-rval_list' ) then
      @ ac ++
      set rval_list = ()

      # loop until the end, or until we see another option prefix '-'
      while ( $ac <= $narg )
         set rval = $argv[$ac]

         set b0 = `echo $rval | cut -b1`
         # quit loop if we seem to see another option
         if ( "$b0" == "-" ) then
            break
         endif

         # can we convert to an int?
         set rint = `ccalc -i $rval`
         if ( $rint <= 0 || $rint != $rval ) then
            echo "** illegal rval '$rval'"
            exit 1
         endif
         # okay, add to list and keep going
         set rval_list = ( $rval_list $rval )
         @ ac ++
      end

      if ( $#rval_list < 1 ) then
         echo "** -rval_list requires at least one parameter"
         exit 1
      endif

      # keep ac at last processed arg
      @ ac --

   else if ( "$argv[$ac]" == '-stats_file' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -stats_file (output stats results) requires 1 parameter"
         exit 1
      endif
      set stats_file = $argv[$ac]

   # -------------------- other opts --------------------
   # -echo is special case of -verb
   else if ( "$argv[$ac]" == '-echo' ) then
      set verb = 3
      set echo
   else if ( "$argv[$ac]" == '-verb' ) then
      @ ac ++
      if ( $ac > $narg ) then
         echo "** -verb requires 1 parameter"
         exit 1
      endif
      set verb = $argv[$ac]

      # test for integral value
      set vtmp = `ccalc -i $verb`
      if ( $vtmp != $verb ) then
         echo "** -verb requires an integral parameter"
         exit 1
      endif

      # on very verbose, turn on echo
      if ( $verb > 2 ) then
         set echo
      endif

   # bad things, man
   else
      echo "** unknown option '$argv[$ac]'"
      exit 1
   endif

   @ ac ++
end

# ---------------------------------------------------------------------------
#

if ( $verb > 1 ) then
   # display parameters?
cat << EOF

++ parameters for program $prog :
   dset_ROI    = $dset_ROI
   dset_data   = $dset_data
   out_dir     = $out_dir
   rset_label  = $rset_label
   rval_list   = $rval_list
   verb        = $verb
EOF
endif

# ---------------------------------------------------------------------------
# make sure required parameters are set
set proceed = 1
if ( $dset_ROI == "" ) then
   echo "** missing option -dset_ROI"
   set proceed = 0
endif
if ( $dset_data == "" ) then
   echo "** missing option -dset_data"
   set proceed = 0
endif
if ( $out_dir == "" ) then
   echo "** missing option -out_dir"
   set proceed = 0
endif
if ( $rset_label == "" ) then
   echo "** missing option -rset_label"
   set proceed = 0
endif
if ( $#rval_list < 1 ) then
   echo "** missing option -rval_list"
   set proceed = 0
endif

if ( ! $proceed ) then
   exit 1
endif

# ---------------------------------------------------------------------------
# test validity of some inputs
set tt = `3dinfo -nijk $dset_ROI`
if ( $tt == NO-DSET ) then
   echo "** invalid -dset_ROI: '$dset_ROI'"
   exit 1
endif

set tt = `3dinfo -nijk $dset_data`
if ( $tt == NO-DSET ) then
   echo "** invalid -dset_data: '$dset_data'"
   exit 1
endif

set tt = `3dinfo -same_grid $dset_ROI $dset_data | \grep 1 | wc -l`
if ( $tt != 2 ) then
   echo "** -dset_ROI and -dset_data do not seem to be on the same grid"
   exit 1
endif

# ===========================================================================
# why are we here?  oh, right, do some actual work
# ===========================================================================

# if we are not given a stats file name, make one up
if ( $stats_file == "" ) then
   set stats_file = $out_dir/stats_$rset_label.txt
endif

if ( $verb > 0 ) then
   echo "++ writing stats text to $stats_file"
endif

# ---------------------------------------------------------------------------
# make sure we have an output directory
if ( ! -d $out_dir ) then
   mkdir -p $out_dir
   if ( $status ) then
      echo "** failed to create out_dir '$out_dir'"
      exit 1
   endif
endif

# ---------------------------------------------------------------------------
# first, run 3dDepthMap on the ROI dataset
set depthmap = $out_dir/depth_$rset_label.nii.gz
set cmd = ( 3dDepthMap -overwrite -zeros_are_zero -input $dset_ROI \
                                  -prefix $depthmap )
$cmd >& /dev/null
if ( $status ) then
   echo "** failed command:\n   $cmd\n"
   exit 1
endif

# ---------------------------------------------------------------------------
# start by printing header (must match btext lines, below )
printf '%7s %7s %7s %6s %6s %6s %6s %6s %6s  %7s %7s %7s  %s\n' \
       "ROI_val" "Nvoxel" "Nzero" "depth"                       \
       "Tmin" "T25%" "Tmed" "T75%" "Tmax"                       \
       "coor_x" "coor_y" "coor_z"                               \
       "ROI_name"                                               \
       >! $stats_file
printf '%7s %7s %7s %6s %6s %6s %6s %6s %6s  %7s %7s %7s  %s\n' \
       "-------" "-------" "-------" "------"                   \
       "------"  "------"  "------"  "------"  "------"         \
       "-------" "-------" "-------" "-------"                  \
       >>! $stats_file

# ---------------------------------------------------------------------------
# process each ROI value in the ROI mask dataset
foreach rval ( $rval_list )
   # --------------------------------------------------
   # is there anything in the mask?
   set nvox = `3dBrickStat -mask $dset_ROI -non-zero -count $dset_ROI"<$rval>"`

   # --------------------------------------------------
   # get the ROI_name early, in case it does not actually exist in the dataset
   # (now via whereami -index_to_label instead of 3dinfo -labeltable and grep)
   set ROI_name = `whereami -index_to_label $rval -dset $dset_ROI`

   # --------------------------------------------------
   # handle the all-zero cases and move on
   # if nvox is zero or equals nzero, we are done

   if ( $nvox != 0 ) then
      set nzero = `3dBrickStat -mask $dset_ROI -mrange $rval $rval \
                               -zero -count $dset_data`
   else
      set nzero = 0
   endif

   if ( $nvox == 0 || $nvox == $nzero ) then
      set btext = "`printf '%7s %7s %7s %6.2f' $rval $nvox $nzero 0`"
      set qtext = "`printf '%6.1f %6.1f %6.1f %6.1f %6.1f ' 0 0 0 0 0`"
      set ctext = "`printf '%7.1f %7.1f %7.1f ' 0 0 0`"
      echo "$btext" "$qtext" "$ctext" "$ROI_name" >>! $stats_file

      continue
   endif

   # --------------------------------------------------
   # we have something to evaluate...
   # --------------------------------------------------

   # --------------------------------------------------
   # quartiles
   set cmd = ( 3dBrickStat -non-zero -mrange $rval $rval -mask $dset_ROI \
                           -percentile 0 25 100 -perc_quiet $dset_data )
   set quarts = ( `$cmd` )
   if ( $status || $#quarts != 5 ) then
      echo "** failed to get quartiles from command:\n   $cmd\n"
      exit 1
   endif

   # --------------------------------------------------
   # maximum ROI depth and coords
   # -closure to include boundaries, -partial to allow for value equality
   set cmd = ( 3dExtrema -volume -nbest 1 -closure -partial -quiet \
                         -mask_file $dset_ROI"<$rval>" $depthmap )

   # --------------------------------------------------
   # ** separate stdout and stderr for now, and read back from a file
   # (do we remove the 3dExtrema author text?)
   ( $cmd >! $out_dir/tmp.extrema.txt ) >& /dev/null
   if ( $status ) then
      echo "** failed to run command:\n   $cmd\n"
      exit 1
   endif
   set extrema = ( `cat $out_dir/tmp.extrema.txt` )
   if ( $#extrema != 7 ) then
      echo "** failed to get depth extrema from command:\n   $cmd\n"
      echo "   $#extrema vals: $extrema"
      exit 1
   endif
   \rm -f $out_dir/tmp.extrema.txt

   set depth = $extrema[2]
   set coords = ( $extrema[3-5] )


   # --------------------------------------------------
   # print out the results
   # --------------------------------------------------

   # too long for a line, so break up the pieces
   set btext = "`printf '%7s %7s %7s %6.2f' $rval $nvox $nzero $depth`"
   set qtext = "`printf '%6.1f %6.1f %6.1f %6.1f %6.1f ' \
                       $quarts[1] $quarts[2] $quarts[3] $quarts[4] $quarts[5]`"
   set ctext = "`printf '%7.1f %7.1f %7.1f '             \
                       $coords[1] $coords[2] $coords[3]`"
   echo "$btext" "$qtext" "$ctext" "$ROI_name" >>! $stats_file

end

# --------------------------------------------------
# finally, display the results
if ( $verb > 0 ) then
   cat $stats_file
   echo ""
endif


# ===========================================================================
# terminate script, to separate help
exit 0


# ===========================================================================
# display the -help output

SHOW_HELP:
cat << EOF

------------------------------------------------------------------------------
$prog  - compute per-ROI value statisics over a given dataset

   usage: $prog [options] many_required_parameters...

   given:
      dset_ROI    : an ROI dataset
      dset_data   : a dataset to compute statistics over (e.g. TSNR)
      out_dir     : a directory to store the results in
      rset_label  : a label for dset_ROI
      rval_list   : a list of ROI values to compute stats over (e.g. 2 41 99)

   and maybe:
      stats_file  : name for the resulting statisics text file

   create a stats (text) file:
      create a depth map for dset_ROI
      for each requested ROI value rval in rval_list (for dset_ROI)
         compute and store in stats file:
            ROI_val      : rval - ROI index value
            Nvoxel       : N voxels in dset_ROI rval region
            Nzero        : N ROI voxels that are 0 in dset_data
            Tmin, T25%, Tmed, T75%, Tmax
                         : multiples of 25%-iles (with min/max)
            coor_x, y, z : x, y and z coordinates at max ROI depth
                           (coordinates are in DICOM/RAI orientation)
            ROI_name     : dataset name of dset_ROI

------------------------------------------------------------------------------
example 0: based on afni_proc.py

   compute_ROI_stats.tcsh                         \\
       -out_dir    t.tsnr_stats_regress           \\
       -dset_ROI   ROI_import_CAEZ_ML_resam+tlrc  \\
       -dset_data  TSNR.FT+tlrc                   \\
       -rset_label CAEZ_ML                        \\
       -rval_list  4 41 99 999

------------------------------------------------------------------------------
terminal options:

   -help                   : show this help
   -hist                   : show the revision history
   -ver                    : show the program version

required parameters:
   -dset_ROI DSET_ROI      : ROI dataset containing regions of interest
                             This dataset should (probably) contain the index
                             values from -rval_list as regions of interest.

   -dset_data DSET_DATA    : volume to compute statistics over
                             This dataset is for computing ROI statistics over,
                             such as a TSNR volume.

   -out_dir OUT_DIR        : directory to put results into
                             The output directory will hold a depth map for all
                             DSET_ROI regions.

   -rset_label RSET_LABEL  : text label to refer to dset_ROI by

   -rval_list V1 V2 ...    : ROI index values
                             Each index with such voxels in DSET_ROI will be
                             used to compute statistics from DSET_DATA.

optional parameters:

   -verb VERB              : specify verbosity level (3 == -echo)
                             def: $verb
   -echo                   : same as -verb 3

------------------------------------------------------------------------------
R Reynolds Apr, 2021
version $script_version
------------------------------------------------------------------------------

EOF

# terminate after -help
exit
