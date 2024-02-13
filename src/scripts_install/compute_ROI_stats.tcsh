#!/usr/bin/env tcsh

# ===========================================================================
# compute ROI statistics (intended for TSNR dset)
#
#    inputs:    dset_ROI      - to restrict statistics to regions
#               dset_data     - to compute statisics from
#               label_roiset  - label for dset_ROI
#               out_dir       - directory to put all results in
#               rval_list     - ROI value list (ints), compute stats per rval
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

   0.0  : Feb 32, 2145: eat more cheese

   current version: $script_version
EOF
exit 0
SKIP_HIST:
set script_version = "version 0.0, February 32, 2145"


# ===========================================================================
# general parameters (have prog and script_version, above)

# ----------------------------------------------------------------------
# required user parameters
set dset_ROI    = ''
set dset_data   = ''
set out_dir     = ''
set rset_label  = ''
set rval_list   = ()

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

      # test for integral
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

set tt = `3dinfo -same_grid $dset_ROI $dset_data | grep 1 | wc -l`
if ( $tt != 2 ) then
   echo "** -dset_ROI and -dset_data do not seem to be on the same grid"
   exit 1
endif

# ===========================================================================
# why are we here?  oh, right, do some actual work
# ===========================================================================

echo getting to work....

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
set cmd = ( 3dDepthMap -zeros_are_zero -input $dset_ROI -prefix $depthmap )
$cmd
if ( $status ) then
   echo "** failed command:\n   $cmd\n"
   exit 1
endif

# ---------------------------------------------------------------------------
# process each ROI value in the ROI mask dataset
foreach rval ( $rval_list )
   # is there anything in the mask?
   set Nvox = `3dBrickStat -mask $dset_ROI -non-zero -count $dset_ROI"<$rval>"`

   # handle the all-zero case
   if ( $Nvox == 0 ) then
      echo "== RR: deal with this"
      continue
   endif

   # count number of zero voxels
   set Nzero = `3dBrickStat -mask $dset_ROI -mrange $rval $rval \
                            -zero -count $dset_data`

   # quartiles
   set cmd = ( 3dBrickStat -non-zero -mrange $rval $rval -mask $dset_ROI \
                           -percentile 0 25 100 -perc_quiet $dset_data )
   set quartiles = ( `$cmd` )
   if ( $status || $#quartiles != 5 ) then
      echo "** failed to get quartiles from command:\n   $cmd\n"
      exit 1
   endif

   # maximum ROI depth and coords
   # -closure to include boundaries, -partial to allow for value equality
   set cmd = ( 3dExtrema -mask_file $dset_ROI"<$rval>" -volume -nbest 1 \
                         -closure -partial -quiet $depthmap )
   set extrema = ( `$cmd` )
   if ( $status || $#extrema != 7 ) then
      echo "** failed to get depth extrema from command:\n   $cmd\n"
      exit 1
   endif
   set depth = $extrema[2]
   set coords = ( $extrema[3-5] )

   # finally the ROI_name (label for this mask index)
   set ltvals = ( `3dinfo -labeltable $dset_ROI | grep \"$rval\" | tr -d \"` )
   if ( $#ltvals == 2 ) then
      set ROI_name = $ltvals[1]
   else
      set ROI_name = UNKNOWN
   endif


   # okay, what should we do with this junk now...

   

end


# ===========================================================================
# terminate script, to separate help
exit 0


# ===========================================================================
# display the -help output

SHOW_HELP:
cat << EOF

------------------------------------------------------------------------------
$prog  - run a quick afni_proc.py analysis for QC

todo:
   - do we restrict depth map to dset_data?

   usage: $prog [options] something something...

   given:
      dset_ROI    : an ROI dataset
      dset_data   : a dataset to compute statistics over (e.g. TSNR)
      out_dir     : a directory to store the results in
      rset_label  : a label for dset_ROI
      rval_list   : a list of ROI values to compute stats over (e.g. 2 41 99)

   create a stats (text) file:
      create a depth map for dset_ROI
      for each requested ROI value rval in rval_list (for dset_ROI)
         compute and store in stats file:
            ROI_val      : rval - ROI index value
            Nvoxel       : N voxels in dset_ROI
            Nzero        : N ROI voxels that are 0 in dset_data
            Tmin, T25%, Tmed, T75%, Tmax
                         : multiples of 25%-iles (with min/max)
            coor_x, y, z : x, y and z coordinates at max ROI depth
            ROI_name     : dataset name of dset_ROI

------------------------------------------------------------------------------
example 0: something ...

   tcsh ~/afni/c/python/ap/compute_ROI_stats.tcsh   \\
       -dset_ROI    ROI_import_Glasser_resam+tlrc   \\
       -dset_data   TSNR.ROI.11+tlrc                \\
       -out_dir     tsnr_stats_regress              \\
       -rset_label  Glasser                         \\
       -rval_list   2 41 99


------------------------------------------------------------------------------
terminal options:

   -help                   : show this help

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
