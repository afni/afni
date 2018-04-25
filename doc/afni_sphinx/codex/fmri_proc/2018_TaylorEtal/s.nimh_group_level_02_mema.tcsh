#!/bin/tcsh

# ----------------------------------------------------------------------
# Script: s.nimh_subject_level_02_ap.tcsh
# Run on: openfmri/ds001_R2.0.4
# Date  : April, 2018
#
# Take a group of subjects processed for task-based FMRI, run 3dMEMA
# on their final statistical results.  Clustering is also performed
# here, using 3dClustSim (with the ACF blur estimation).  Some
# individual volumes of t-statistics and effect estimate maps are also
# output.
#
# Used for "NIMH-AFNI" processing in:
#
#   Some comments and corrections on FMRI processing with AFNI in
#   "Exploring the Impact of Analysis Software on Task fMRI Results"
# 
#   Paul A. Taylor, Gang Chen, Daniel R. Glen, Justin K. Rajendra,
#   Richard C. Reynolds, Robert W. Cox.
#   Scientific and Statistical Computing Core, NIMH/NIH/DHHS,
#   Bethesda, MD, USA.
#
# NOTE: This set of commands includes some features that would have
# been chosen in preference to those of BMN, as described in the above
# text (e.g., using the '-missing_data 0' option in 3dMEMA).
# *However*, there are still some things that should be altered, such
# as using paired one-sided testing-- the search for clusters here is
# two-sided, and so the two-sided test should be used for correctness.
#
# ----------------------------------------------------------------------
#
# To run for a single subject, for example:
#
#   tcsh -ef s.nimh_group_level_02_mema.tcsh
#
# ----------------------------------------------------------------------

# ================ Set up paths and many params ======================

set here        = $PWD
set p_0         = /data/NIMH_SSCC
set path_main   = ${p_0}/openfmri/ds001_R2.0.4
set path_proc   = ${path_main}/data_proc_nimh
set path_mema   = ${path_proc}/GROUP_LEVEL_nimh

# Running 3dMEMA
set script_mema = do_group_level_nimh.tcsh # generated command file name
set omema       = mema_results.nii.gz      # output effect+stats filename

# Cluster parameters
set csim_NN     = "NN1"    # neighborhood; could be NN1, NN2, NN3
set csim_sided  = "1sided" # could be 1sided, 2sided or bisided
set csim_pthr   = 0.01     # voxelwise thr in 3dClustSim
set csim_alpha  = 0.05     # nominal FWE

# ================== Optional: Biowulf cluster =========================

# This section for possibly running on Biowulf cluster.

#### The following 2 lines could be uncommented to load these modules;
#### that would likely not be necessary outside of NIH's Biowulf cluster.
# source /etc/profile.d/modules.csh   
# module load afni R

# set thread count if we are running SLURM
if ( $?SLURM_CPUS_PER_TASK ) then
  setenv OMP_NUM_THREADS $SLURM_CPUS_PER_TASK
endif

# Set temporary output directory; then requires using something like
# this on the swarm command line: --sbatch '--gres=lscratch:50'.
# These variables used again *after* afni_proc.py command, if running
# on Biowulf.
if( $?SLURM_JOBID )then
  set tempdir = /lscratch/$SLURM_JOBID/GROUP_LEVEL_nimh
  set usetemp = 1
else
  set tempdir = $path_mema
  set usetemp = 0
endif

mkdir -p $tempdir

# ====================== Voxelwise modeling ==========================

# Create and execute 3dMEMA command.  Note the inclusion of the
# "-options -missing_data 0" flags, to avoid some numerical badness of
# possible FOV edge effects, etc.
gen_group_command.py                                           \
    -command      3dMEMA                                       \
    -write_script ${tempdir}/${script_mema}                    \
    -prefix       ${tempdir}/${omema}                          \
    -dsets        ${path_proc}/sub*/stats.sub*_REML+tlrc.HEAD  \
    -set_labels   "controls"                                   \
    -subs_betas   'pumps_demean_vs_ctrl_demean#0_Coef'         \
    -subs_tstats  'pumps_demean_vs_ctrl_demean#0_Tstat'        \
    -options      -missing_data 0

tcsh -ef ${tempdir}/${script_mema}
echo "++ MEMAed!"

cd ${tempdir}
echo "++ Now in the group stat dir: $PWD"

# ====================== Make group mask ==========================

# Make a group level mask for reporting results.  This also determines
# the volume over which multiple comparisons corrections for voxelwise
# stats are done.
3dmask_tool                                             \
    -prefix mask.nii.gz                                 \
    -input `ls ${path_proc}/sub-*/mask_epi_anat.*.HEAD` \
    -frac 1.0

# ==================== Calc average smoothness =======================

# Get the line from each blur estimate file that has our desired
# parameters; we are running REML here and using the ACF smoothness
# estimation, so the gpattern variable reflects both.  For N subjects,
# the output *1D file contains N rows of the 3 ACF parameters.
set gpattern = 'err_reml ACF'
grep -h "$gpattern" ${path_proc}/sub*/blur_est*   \
    | cut -d\  -f1-3                              \
    > group_ACF_ests.1D

# Get the group average ACF parameters.
set blur_est = ( `3dTstat -mean -prefix - group_ACF_ests.1D\'` )
echo "++ The group average ACF params are: $blur_est"

# ==================== Cluster simulations =======================

# Simulations for FWE corrected cluster-size inference 
3dClustSim                                       \
    -both                                        \
    -mask   mask.nii.gz                          \
    -acf    $blur_est                            \
    -prefix ClustSim 

# Get the volume threshold value from the ClustSim results, based on
# user's choice of parameters as set above.  The "-verb 0" means that
# just the threshold number of voxels is returned, so we can save that
# as a variable.
set clust_thrvol = `1d_tool.py -verb 0                                \
                        -infile ClustSim.${csim_NN}_${csim_sided}.1D  \
                        -csim_pthr   $csim_pthr                       \
                        -csim_alpha "$csim_alpha"`

# Get the statistic value equivalent to the desired voxelwise p-value,
# for thresholding purposes.  Using the same p-value and sidedness
# that were selected in the ClustSim results.  This program also gets
# the number of degrees of freedom (DOF) from the header of the volume
# containing the statistic. The "-quiet" means that only the
# associated statistic value is returned, so we can save it to a
# variable.
set voxstat_thr = `p2dsetstat -quiet                    \
                    -pval $csim_pthr                    \
                    "-${csim_sided}"                    \
                    -inset ${omema}'[controls:t]'`

echo "++ The final cluster volume threshold is:  $clust_thrvol"
echo "++ The voxelwise stat value threshold is:  $voxstat_thr"

# ================== Make cluster maps =====================

# Following the previous paper, these commands extract the "positive"
# and "negative" cluster info, respectively, into separate files.
# Each applies clustering parameters above and makes: a *map* of
# cluster ROIs (regions numbered 1, 2, 3, ...); a map of the effect
# estimate (EE) values within those clusters; and a table report of
# the clusters.  
# Technical note: '-1tindex 1' means that the threshold is applied to
# the [1]st volume of the input dset, which here holds the statistic
# values; '-1dindex 0' means that data values saved in '-prefix ...'
# come from the [0]th volume of the input dset, which here holds the
# EE values.  
set csim_pref = "clust_tpos"
3dclust                                                        \
    -1Dformat -nosum -1tindex 1 -1dindex 0                     \
    -2thresh -1e+09 $voxstat_thr  -dxyz=1                      \
    -savemask ${csim_pref}_map.nii.gz                          \
    -prefix   ${csim_pref}_EE.nii.gz                           \
    1.01 ${clust_thrvol} ${omema} > ${csim_pref}_table.1D
# Also make a mask of t-stats (not necessary, but matching previous
# work)
3dcalc \
    -a ${csim_pref}_map.nii.gz                \
    -b ${omema}'[1]'                          \
    -expr "step(a)*b"                         \
    -prefix ${csim_pref}_t.nii.gz             \
    -float

set csim_pref = "clust_tneg"
3dclust                                                        \
    -1Dformat -nosum -1tindex 1 -1dindex 0                     \
    -2thresh -$voxstat_thr 1e+09 -dxyz=1                       \
    -savemask ${csim_pref}_map.nii.gz                          \
    -prefix   ${csim_pref}_EE.nii.gz                           \
    1.01 ${clust_thrvol} ${omema} > ${csim_pref}_table.1D
# Also make a mask of t-stats (not necessary, but matching previous
# work)
3dcalc \
    -a ${csim_pref}_map.nii.gz                \
    -b ${omema}'[1]'                          \
    -expr "step(a)*b"                         \
    -prefix ${csim_pref}_t.nii.gz             \
    -float

cd $here

# ===================================================================

# Again, Biowulf-running considerations: if processing went fine and
# we were using a temporary directory, copy data back.
if( $usetemp && -d $tempdir )then
    echo "Copying data from $tempdir to $path_mema"
    mkdir -p $path_mema
    \cp -pr $tempdir/* $path_mema/
endif

# ===================================================================

echo "\n++ DONE with group level stats + clustering!\n"

time ; exit 0
