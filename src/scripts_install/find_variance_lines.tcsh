#!/usr/bin/env tcsh

@global_parse `basename $0` "$*" ; if ($status) exit 0

unalias grep

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
#    - bad columns are those with mean >= .97 (see -thresh)
#
#    * consider Color_circle_AJJ (tighter color ranges up top)
#
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# main input variables ($run is the typical one to set)
set din_list     = ( )            # input datasets
set do_clean     = 1              # do we remove temporary files
set do_img       = 1              # make images
set mask_in      = 'AUTO'         # any input mask (possibly renamed)
set max_img      = 7              # maximum number of high-var images to make
set min_cvox     = 7              # minimum voxels in a column
set min_nt       = 10             # minimum time series length (after nfirst)
set nerode       = 0              # number of mask erosions
set nfirst       = 0              # number of first time points to exclude
set perc         = 90             # percentile limit of variance
set polort       = A              # polort for trend removal (A = auto)
set ignore_edges = 1              # ignore lines clustering with edge voxels
set rdir         = vlines.result  # output directory
set sdpower      = 2              # power on stdev (2=default variance)
set thresh       = 0.90           # threshold for tscale average (was .97)
set num_pc       = 0              # number of PCs per vline to output
set do_pc_3dD    = 1              # if calc'ing PCs, create+run 3dDeconvolve
set do_pc_vstat  = 1              # if calc'ing PCs, create+run views of stats
set suffix       = ""             # extra str for QC* output files

# computed vars
set edge_mask  = ''             # edge voxel mask, if applied
set clust_pre  = 'clustset'     # prefix for cluster mask

# created scripts
set cmd_copy       = a_cmd_00_copy.tcsh 
set cmd_3dD_base   = a_cmd_01_3dD
set cmd_vstat_base = a_cmd_02_vstat

set prog = find_variance_lines.tcsh

set version = "1.5, 23 May, 2025"

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
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set do_clean = $argv[$ac]
   else if ( "$argv[$ac]" == "-do_img" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set do_img = $argv[$ac]
   else if ( "$argv[$ac]" == "-echo" ) then
      set echo
   else if ( "$argv[$ac]" == "-mask" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set mask_in = $argv[$ac]
   else if ( "$argv[$ac]" == "-max_img" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set max_img = $argv[$ac]
   else if ( "$argv[$ac]" == "-min_cvox" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set min_cvox = $argv[$ac]
   else if ( "$argv[$ac]" == "-min_nt" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set min_nt = $argv[$ac]
   else if ( "$argv[$ac]" == "-nerode" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set nerode = $argv[$ac]
   else if ( "$argv[$ac]" == "-nfirst" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set nfirst = $argv[$ac]
   else if ( "$argv[$ac]" == "-perc" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set perc = $argv[$ac]
   else if ( "$argv[$ac]" == "-polort" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set polort = $argv[$ac]
      if ( $polort == AUTO ) then
         set polort = A
      else if ( $polort == NONE ) then
         set polort = -1
      endif
   else if ( "$argv[$ac]" == "-ignore_edges" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set ignore_edges = $argv[$ac]
   else if ( "$argv[$ac]" == "-stdev_power" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set sdpower = $argv[$ac]
   else if ( "$argv[$ac]" == "-thresh" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set thresh = $argv[$ac]
   else if ( "$argv[$ac]" == "-rdir" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set rdir = $argv[$ac]
   else if ( "$argv[$ac]" == "-num_pc" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set num_pc = $argv[$ac]
   else if ( "$argv[$ac]" == "-suffix_qc" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set suffix = $argv[$ac]
   else if ( "$argv[$ac]" == "-do_pc_3dD" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set do_pc_3dD = $argv[$ac]
   else if ( "$argv[$ac]" == "-do_pc_vstat" ) then
      if ( $ac >= $#argv ) then
         echo "** missing parameter after $argv[$ac]"
         exit 1
      endif
      @ ac += 1
      set do_pc_vstat = $argv[$ac]

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
   echo "** missing input datasets (they should come as trailing arguments)"
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

cat <<EOF

++ have nslices : $nk_list
++ params: min_cvox $min_cvox, nerode $nerode, perc $perc, polort $polort,
           ignore_edges $ignore_edges, sdpower $sdpower, thresh $thresh,
           num_pc $num_pc, do_pc_3dD $do_pc_3dD, do_pc_vstat $do_pc_vstat

EOF

# ----------------------------------------------------------------------
# make results dir, enter it and remove old results
if ( -d $rdir ) then
   echo "** error, results dir already exists: $rdir"
   exit 0
endif

\mkdir $rdir
if ( $status ) then
   echo "** failed to create results directory, $rdir"
   exit 0
endif

# ----------------------------------------------------------------------
# copy inputs (minus pre-ss trs) to results dir

set dset_list = ()
foreach index ( `count_afni -digits 1 1 $#din_list` )
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

# store the command that was run, for reference
cat <<EOF > _tmp
# The command used to create this dir was:

find_variance_lines.tcsh $argv

EOF
# ... readably.
file_tool -wrap_lines -infiles _tmp > ${cmd_copy}
\rm _tmp

cat <<EOF>> ${cmd_copy}

#  Additional notes
# ------------------
# program version : $version
# have nslices    : $nk_list
# 
# Params (some/many likely set with default values)
# nfirst          : $nfirst
# min_cvox        : $min_cvox
# mask_in         : $mask_in
# nerode          : $nerode
# polort          : $polort
# perc            : $perc
# ignore_edges    : $ignore_edges
# sdpower         : $sdpower
# thresh          : $thresh
# num_pc          : $num_pc
# do_pc_3dD       : $do_pc_3dD
# do_pc_vstat     : $do_pc_vstat

EOF


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

   # If we do not want clusters touching the farthest front/back/sides of brain,
   # make an edge mask.  Grow the current mask vertically, take 1 minus.  Since
   # anything touching this would be bad, dilate by 1 and take only the dilated
   # voxels.  Any cluster that includes such voxels is unwanted.
   if ( $ignore_edges == "1" ) then
      set edge_mask = mask_edge.nii.gz
      set tset = tmp.edge.nii.gz
      set t2   = tmp.e2.nii.gz
      # grow vertically and take 1 minus
      3dLocalstat -nbhd "Rect(0,0,-$nk)" -stat max -prefix $tset $mask_in
      3dcalc -a $tset -expr '1-bool(a)' -prefix $t2 -datum byte
      \rm $tset
      # dilate and subtract out previous edge mask
      3dmask_tool -dilate_inputs 1 -input $t2 -prefix $tset
      3dcalc -a $tset -b $t2 -expr a-b -prefix $edge_mask
      \rm $tset $t2
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

# --------------------------------------------------
# make the prefix for stats dsets and scripts

set sname = stats${suffix}

# ---------------------------------------------------------------------------
# count the number of bad columnar regions per input
set bad_counts = ()
# make a list of dsets for (possible) PC calculation
set pc_list = ()

foreach index ( `count_afni -digits 1 1 $#dset_list` )

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

   # append to PC list
   set pc_list = ( $pc_list $dset )

   # compute temporal variance dset (square stdev for now)
   set sset = var.0.orig.r$ind02.nii.gz
   3dTstat -stdevNOD -prefix tmp.stdev.nii.gz $dset
   3dcalc -prefix $sset -a tmp.stdev.nii.gz -expr "a**$sdpower"
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
   # (show command before executing)
   # change NN3 to NN2, as this is really 2D clustering (could use NN 1)
   set cfile = bad_clust.r$ind02.txt
   set clust_mask = $clust_pre.r$ind02.nii.gz
   set cmd = ( 3dClusterize -ithr 0 -idat 0 -NN 2 -2sided -1 $thresh \
                  -inset $pset -pref_map $clust_mask -outvol_if_no_clust )
   echo $cmd
   $cmd | tee $cfile

   # and grab the coordinates of the bad clusters
   set bfile = bad_coords.r$ind02.txt
   grep -v '#' $cfile                                                     \
        | awk '{ z='$zcoord'; printf "%7.2f %7.2f %7.2f\n", $14, $15, z}' \
        | tee $bfile

   # if we have an edge_mask, possibly remove edge clusters from $bfile
   if ( $edge_mask != "" ) then
      # create inner_clust laster, either avoid edges or using full clust
      set inner_clust = $clust_pre.inner.r$ind02.nii.gz
      # store edge cluster (restricted to edges) and inner clusters
      set edge_clust = $clust_pre.edge.r$ind02.nii.gz
      3dcalc -a $clust_mask -b $edge_mask -expr 'a*b' -prefix $edge_clust
      # use 3dRank to get badlist: non-zero values in $edge_clust
      3dRank -prefix rank.$ind02 -input $edge_clust
      \rm rank.$ind02+*
      set edgelist = ( `1dcat rank.$ind02.rankmap.1D'[1]' | tail -n +2` )
      set nclust = ( `cat $bfile | wc -l` )

      # if there is an adjustment to make, do so
      set ecfile = edge_coords.r$ind02.txt
      set backfile = bad_coords.full.r$ind02.txt
      \cp $bfile $backfile
      echo "" > $ecfile    # start with an empty file
      if ( $nclust > 0 && $#edgelist > 0 ) then
         # note: line and cluster numbers are 1-based
         echo "++ have $nclust clusters but $#edgelist are at edges"
         set pedge = "[`echo $edgelist | tr ' ' ,`]"
         # find clusters that are not in edgelist
         set inlist = ( `python -c "for ind in [i+1 for i in range($nclust) \
                           if i+1 not in $pedge]: print(ind)"`)
         # create inner_clust dset: extract inlist clusters
         # (this might be empty)
         if ( $#inlist > 0 ) then
            echo "-- creating inner clusterset $inner_clust ($#inlist clusters)"
            set vinner = "<`echo $inlist | tr ' ' ,`>"
            3dbucket -prefix $inner_clust $clust_mask"$vinner"
         else
            echo "-- creating inner clusterset $inner_clust (but empty)"
            # still make an inner_clust dataset, but it is empty
            3dcalc -a $clust_mask -expr 0 -prefix $inner_clust
         endif

         echo "-- keeping clusters : $inlist"
         echo "   removing clusters: $edgelist"
         # create new files (new bfile from inlist, ecfile from edgelist)
         echo -n "" > $bfile
         foreach r ( $inlist )
            awk "{if(NR==$r) print}" $backfile >> $bfile
         end
         foreach r ( $edgelist )
            awk "{if(NR==$r) print}" $backfile >> $ecfile
         end
         echo "++ updated inner coord list:"
         cat $bfile
      else
         echo "-- no edge clusters to remove"
         # some runs might have inner, some not, so we "need" an inner dset
         # to wildcard on for the later intersection
         3dcopy $clust_mask $inner_clust
      endif
   endif

   set bad_counts = ( $bad_counts `cat $bfile | wc -l` )
end  # foreach index
echo ""

# ---------------------------------------------------------------------------
# report intersection clusters from projection dsets

# if ignoring edges, only use inner clusters
if ( $edge_mask != "" ) then
   set cform = "$clust_pre.inner.r*.nii.gz"
else
   set cform = "$clust_pre.r*.nii.gz"
endif
set clustsets = ( $cform )

echo "-- evaluating intersection (across $cform) ..."
set iset = clust.inter.nii.gz
3dmask_tool -inter -prefix $iset -input $clustsets

set cfile = bad_clust.inter.txt
# we are clustering a binary dataset, so use any threshold above zero
3dClusterize -ithr 0 -idat 0 -NN 2 -inset $iset -2sided 0 0.9         \
             -pref_map clust.inter.enum.nii.gz -outvol_if_no_clust    \
             | tee $cfile
set bfile = bad_coords.inter.txt
grep -v '#' $cfile                                                    \
     | awk '{z='$zcoord'; printf "%7.2f %7.2f %7.2f\n", $14, $15, z}' \
     | tee $bfile
set bad_count_inter = `cat $bfile | wc -l`

# ---------------------------------------------------------------------------
# create PCs per vline (check along the way if/where vlines exist)
if ( $num_pc ) then
   set vcount = 0

   # loop over each run's cluster map
   foreach index ( `count_afni -digits 1 1 $#pc_list` )
      # only do PCs if the run had vlines, which bad_counts keeps track of
      if ( ${bad_counts[$index]} ) then
         set ind02    = `ccalc -form '%02d' $index`
         set dset     = $pc_list[$index]
         set clustset = ( $clust_pre.inner.r$ind02.nii.gz )

         # if we are in this if-condition, nvline > 0
         set nvline = `3dBrickStat -slow -max $clustset`
         foreach nn ( `count_afni -digits 1 1 $nvline` )
             # check to make sure line still exists after ignoring edges
             # (nvox has: 2 values if line exists; 0 values if it was removed)
             set nvox = `3dROIstats -quiet -nzvoxels      \
                            -mask ${clustset}"<$nn>"      \
                            ${clustset}`
             if ( ${#nvox} ) then
                @ vcount+= 1
                set n02 = `ccalc -form '%02d' $nn`
                3dpc                                         \
                    -nscale                                  \
                    -pcsave  $num_pc                         \
                    -mask    ${clustset}"<$nn>"              \
                    -prefix  pc.inner.r$ind02.c$n02.val      \
                    $dset
             endif
         end 

         # do simple 3dD on the PCs here
         set all_pc = `find . -maxdepth 1 -name "pc.inner.r$ind02.c*.val_vec.1D" \
                            | cut -b3- | sort`
         set nall_pc = ${#all_pc}
         if ( ${nall_pc} && ${do_pc_3dD} ) then
            set num_stimts = `ccalc -i -eval "${nall_pc}*${num_pc}"`
            set polort     = -1    # already detrended dset

            # loop over all lines and components and build -stim_* opts
            set sss = ""
            set idx = 1 
            @ Mpc   = $num_pc - 1    # max index for PC selector; 0-based count
            foreach cc ( `seq 1 1 ${nall_pc}` ) 
               set pcname = ${all_pc[$cc]}
               set tmp1   = `basename ${pcname} .val_vec.1D`
               set pccore = ${tmp1:gas/pc.inner.//}
               foreach pp ( `seq 0 1 ${Mpc}` ) 
                  set sss = "${sss:q} -stim_file $idx ${pcname}'[$pp]' "
                  set sss = "${sss:q} -stim_label $idx ${pccore}.$pp "
                  @ idx += 1
               end
            end

cat <<EOF > _tmp_3dD_cmd.tcsh
#!/bin/tcsh

# 3dD command for already-detrended time series

3dDeconvolve                                             \
    -overwrite                                           \
    -input           ${dset}                             \
    -polort          ${polort}                           \
    -num_stimts      ${num_stimts}                       \
    ${sss:q}                                             \
    -tout                                                \
    -x1D             X.xmat.1D                           \
    -xjpeg           X.jpg                               \
    -x1D_uncensored  X.nocensor.xmat.1D                  \
    -bucket          ${sname}.r$ind02.nii.gz
EOF

            # make the command readable in its file
            set cmd_3dD = ${cmd_3dD_base}.r$ind02.tcsh
            file_tool -wrap_lines -infiles _tmp_3dD_cmd.tcsh > ${cmd_3dD}
            \rm _tmp_3dD_cmd.tcsh
            # ... and execute it
            tcsh ${cmd_3dD}
            # ... and rename the created REML cmd, per run
            \mv  ${sname}.REML_cmd  ${sname}.r$ind02.REML_cmd

            if ( $do_pc_vstat ) then
                # make images and run script to view stats file, via
                # @chauffeur_afni, using a preexisting template script
                set cmd_vstat = ${cmd_vstat_base}.r$ind02.tcsh

                # copy script template file from abin/ ...
                set dir_abin = "`afni_system_check.py -disp_abin`"
                \cp "${dir_abin}/afni_vlines_run_text.txt" ${cmd_vstat}

                # ... insert run number and stats name string...
                sed -i s/REPLACE_ME_WITH_RUN_NUM/r${ind02}/g ${cmd_vstat}
                sed -i s/REPLACE_ME_WITH_STAT_PREF/${sname}/g ${cmd_vstat}

                # ... and run the script, which creates an image of the
                # Full_Fstat, as well as a run_*.tcsh script to drive AFNI GUI 
                tcsh ${cmd_vstat}
            endif
         endif
      endif
   end

   # and PCs for cluster intersection, if such a dset exists and if
   # the intersection file has at least 1 vline
   if ( -f clust.inter.enum.nii.gz && $bad_count_inter ) then
      set clustset = clust.inter.enum.nii.gz

      set nvline = `3dBrickStat -slow -max $clustset`
      foreach nn ( `count_afni -digits 1 1 $nvline` )
          @ vcount+= 1
          set n02 = `ccalc -form '%02d' $nn`
          3dpc                                         \
              -nscale                                  \
              -pcsave  $num_pc                         \
              -mask    ${clustset}"<$nn>"              \
              -prefix  pc.inter.enum.c$n02.val         \
              $dset
      end
   endif

   if ( $vcount ) then
      if ( $do_clean == 1 ) then
         # clean up PC dsets: since they are only within mask, not so useful
         \rm -f pc.inner.*.BRIK*       pc.inner.*.HEAD       \
                pc.inter.enum.*.BRIK*  pc.inter.enum.*.HEAD
      endif

      # trim some 1D outputs (put ones we want in temp dir, then bring them back)
      set tdir = __tmp_dir_for_pc_1Ds
      \mkdir -p ${tdir}
      \mv pc.in*eig.1D pc.in*vec.1D ${tdir}/.
      \rm pc.in*.1D
      \mv ${tdir}/pc.in*.1D .
      \rm -rf ${tdir}
   endif
endif

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

   echo ${text} > QC_var_lines${suffix}.txt

   # combine img files
   if ( ${count} ) then
       set opref = QC_var_lines${suffix}
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
Overview: ~1~

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
      - find separate clusters of them,
        where a vline is a column with Localstat mean >= $thresh

------------------------------------------------------------
Examples: ~1~

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
Options (terminal): ~1~

   -help                : show this help
   -hist                : show the version history
   -ver                 : show the current version

Options (processing): ~1~

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
                          vertical column.  Otherwise, edge voxels might end
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

   -suffix_qc VAL       : string to append to QC* file outputs, as well as any
                          stats*.nii.gz file output if using -num_pc (def="")

                             VAL is a string appended to "QC_var_lines"
                             files;  it would also be appended to "stats" in
                             the NIFTI file output associated with any PCs;
                             it should likely start with "_".

                          Including the subject ID in the files in this way
                          might be useful at times.

   -ignore_edges VAL    : ignore vline clusters at edges (def=$ignore_edges)

                             VAL in {0,1}

                          Set this option to ignore clusters at the R,L,A,P
                          edges, so vlines near those edges are not reported.

                          If a vline cluster traces the outer edge of the brain
                          (in the j-axis direction), it is probably just due to
                          motion.  Use this option to ignore such clusters, and
                          therefore not report vlines connected to edges.

                          Such edges are defined as the outermost edges in the
                          i and j directions of the 3-D mask.  This is because
                          lines are along the k axis (usually I/S), and the
                          limits should be perpendicular to the vline axis.

   -stdev_power POW     : power on stdev to apply before ave/thresh

                             default :  -stdev_power $sdpower
                             example :  -stdev_power 4 -thresh 0.92

                          The is the power the stdandard deviation is taken to
                          before any subsequent computations.  Higher values
                          (powers) allow for better contrast when close to 1.0.
                          Higher values might allow for lower -thresh.

                          A value of 1 will lead to computations with stdev.
                          A value of 2 will imply variance.
                          Higher values continues the pattern.


   -thresh THRESH       : variance threshold to be considered a variance line

                             default : -thresh $thresh

                          This is the minimum 3dLocalstat variance average for
                          a column to be consider a variance line.  A value
                          just under 1.0 might be reasonable.

   -num_pc NUM          : number of PCs to calculate per variance line 

                             default : -num_pc $num_pc  (i.e., none estimated)

                          Preliminary tests with this have found 2 to be a 
                          reasonable value to use, if you want PCs output. 
                          As an example of naming, the info from component #3
                          in run 2 is named: pc.inner.r02.c03*.
                          The outputs from the intersection vline dset are 
                          named like: pc.inter.enum.c*.

   -do_pc_3dD VAL       : if '-num_pc ..' is used and variance lines are found
                          in a run, then by default this program will
                          build+execute a 3dDeconvolve command with those
                          PCs as '-stim_file ..' regressors; this opt controls
                          whether 3dD would be run or not (def=$do_pc_3dD)

                             VAL in {0,1}

                          This will help highlight where the variance
                          line influence appears to be more/less
                          across the dataset. See the Note on 'Outputs
                          when -num_pc is used' for more details.

   -do_pc_vstat VAL     : if '-num_pc ..' is used and variance lines are found
                          in a run (and 3dDeconvolve is _not_ turned off via
                          '-do_pc_3dD 0'), then by default this program will
                          build+execute an @chauffeur_afni command to make
                          images of the Full_Fstat volume in the stats dset
                          of each run with vlines (stats*.r*.image*jpg), as well
                          as an executable script to surf that dset and volume
                          in the AFNI GUI (run_stats*.r*_pc.tcsh); this opt
                          controls whether @chauffeur would be run or not 
                          (def=$do_pc_vstat)

                             VAL in {0,1}

                          This will help with quality control (QC)
                          checks of where the variance line influence
                          appears to be more/less across the dataset.
                          Note that image montage slices might miss
                          some of the variance lines themselves, so
                          executing the run script might be the most
                          useful. This can be done as follows (if no -suffix_qc
                          was used):
                          
                             tcsh run_stats.r01.tcsh

                          See the Note on 'Outputs when -num_pc is
                          used' for more details.

-----------------------------------------------------------------------------
Notes: ~1~

Outputs when -num_pc is used: ~2~

When '-num_pc ..' is used, 3dpc will be used to perform a principal
component analysis (PCA) decomposition of each variance line.  For
each line, the specified number of PCs will be saved in a text file
called pc.inner.*.val_vec.1D (one per column).

Additionally, a 3dDeconvolve command will be executed for each run
that has at least one variance line.  The full set of PCs for that run
will be used at '-stim_file ..' inputs to the command, and a Full
F-stat is calculated.  This shows the amount of variance explained in
the detrended input time series by the full set of the PC
components. That is, where the F is larger, some combination of PCs is
explaining more of the time series variability (i.e., having more
influence on the time series pattern).  The result is stored in the
stats*.nii.gz file.

A copy of the 3dDeconvolve command for each run is stored in a text
file within the output vlines directory, called
${cmd_3dD_base}.r*.tcsh.

Furthermore, an @chauffeur_afni command will be executed for each run
that has at least one variance line and for which a stats*.nii.gz has
been created.  This will make multislice image montages of the
Full_Fstat volume for each run (stats.r*.image*.jpg), which show
where any linear combination of the detected variance lines' PCs fit
the input time series data well.  The Full_Fstat volume is used for
both the overlay and thresholding datasets. The overlay color range
comes from a high percentile value of the data itself, and the
threshold value corresponds to p=0.01---of course, the transparent
alpha thresholding is also turned on.

The executed @chauffeur_afni also creates a run_stats.r*_pc.tcsh
script that will drive the AFNI GUI to load up the stats dataset with
the same data, to enable deeper checks of it in an efficient way. Each
script can be run just like:

    tcsh run_stats.r01.tcsh

A copy of the @chauffeur_afni command for each run is stored in a text
file within the output vlines directory, called
${cmd_vstat_base}.r*.tcsh.


------------------------------
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
   0.5  14 Dec 2024 : change thresh from 0.95 to 0.97 to restrict results
   0.6   8 Jan 2025 : min_cvox: 5->7
                    - add -thresh option
                    - add -stdev_power
   0.7  23 Apr 2025 : add -ignore_edges (default on)
                    - change corresponding thresh default from 0.97 to 0.90
   0.8  29 Apr 2025 : [PT] add optional PC output
   0.9  30 Apr 2025 : [PT] clean up PC-related functionality
   1.0   1 May 2025 : [PT] remove PC errors if edge-ignore removed line(s)
   1.1   7 May 2025 : [PT] add -suffix_qc so QC* files can be unique per subj
   1.2   8 May 2025 : [PT] add 3dDeconvolve cmd to PC calcs
   1.3  12 May 2025 : [PT] add more reporting output
   1.4  20 May 2025 : [PT] add more chauffeur script and image output
   1.5  23 May 2025 : [PT] fix stats dset/script behavior when suffix_qc is used

EOF
# check $version, at top

exit 0
