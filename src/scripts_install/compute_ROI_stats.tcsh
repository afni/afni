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
# is the current mask 



# ===========================================================================
# terminate script, to separate help
exit 0


# ===========================================================================
# display the -help output

SHOW_HELP:
cat << EOF

------------------------------------------------------------------------------
$prog  - run a quick afni_proc.py analysis for QC

   usage: $prog [options] something something...

------------------------------------------------------------------------------
example 0: 

      $prog ...

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
