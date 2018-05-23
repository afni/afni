#!/bin/tcsh

# ----------------------------------------------------------------------
# Script: s.nimh_group_level_02_mema_bisided.tcsh
# Run on: openfmri/ds001_R2.0.4
# Date  : May, 2018
#
# Take a group of subjects processed for task-based FMRI, run 3dMEMA
# on their final statistical results.  Clustering is also performed
# here, using 3dClustSim (with the ACF blur estimation).  Some
# individual volumes of t-statistics and effect estimate maps are also
# output.  This provides an example of using the newest clusterizing
# program in AFNI, 3dClusterize (an update version of 3dclust).
#
# Used as an example of proper two-sided testing in:
#  
#   A tail of two sides: Artificially doubled false positive rates in
#   neuroimaging due to the sidedness choice with t-tests
#
#   by Chen G, Glen DR, Rajendra JK, Reynolds RC, Cox RW, Taylor PA.
#      Scientific and Statistical Computing Core, NIMH/NIH/DHHS, USA.
#
# ... as applied to an earlier processed dataset from *this* study:
#
#   Some comments and corrections on FMRI processing with AFNI in
#   "Exploring the Impact of Analysis Software on Task fMRI Results"
# 
#   by Taylor PA, Chen G, Glen DR, Rajendra JK, Reynolds RC, Cox RW.
#      Scientific and Statistical Computing Core, NIMH/NIH/DHHS, USA.
#
#   NOTE: The processing described in "Some comments..." includes sets
#   of processing commands correcting/improving upon some sets of
#   commands run in an earlier study by a different group (see therein
#   for more details).  However, even the "NIMH" set of commands still
#   includes some non-ideal features, as described there, for the
#   purposes of comparison with the other group's processing stream.
#   Please read the associated text carefully before using/adapting
#   those scripts.
#
# ----------------------------------------------------------------------
#
# To run for a single subject, for example:
#
#   tcsh -ef s.nimh_group_level_02_mema_bisided.tcsh
#
# On a cluster you might want to use scratch disk space for faster
# writing, and a lot of memory and CPUs, depending on the
# data/processing choices.  Syntax might be:
#
#   sbatch                       \
#      --partition=SOME_NAMES    \
#      --cpus-per-task=32        \
#      --mem=64g                 \
#      --gres=lscratch:80        \
#      s.nimh_group_level_02_mema_bisided.tcsh
#
# ----------------------------------------------------------------------

# ================ Set up paths and many params ======================

set here        = $PWD
set p_0         = /data/NIMH_SSCC
set path_main   = ${p_0}/openfmri/ds001_R2.0.4
set path_proc   = ${path_main}/data_proc_nimh
set odir        = GROUP_LEVEL_nimh
set path_mema   = ${path_proc}/${odir}

# Running 3dMEMA
set script_mema = do_group_level_nimh.tcsh # generated command file name
set omema       = mema_results.nii.gz      # output effect+stats filename
set groupid     = controls                 # volume label for stat result

# Cluster parameters
set csim_neigh  = 1         # neighborhood; could be NN=1,2,3
set csim_NN     = "NN${csim_neigh}"  # other form of neigh
set csim_sided  = "bisided" # test type; could be 1sided, 2sided or bisided
set csim_pthr   = 0.001     # voxelwise thr (was higher, 0.01, in orig study)
set csim_alpha  = 0.05      # nominal FWE
set csim_pref   = "clust_${csim_sided}" # prefix for outputting stuff

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
  set tempdir = /lscratch/$SLURM_JOBID/${odir}
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
    -set_labels   "$groupid"                                   \
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
# The "-frac 1.0" is to make an intersection mask.
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

# Simulations for FWE corrected cluster-size inference: make a cluster
# table based on the simulations, and *that* gets parsed and applied
# to the actual data.
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
#
# Note: actually, you can do this calculation within 3dClusterize
# directly now, specifying the threshold as a p-value.

set voxstat_thr = `p2dsetstat -quiet                    \
                    -pval $csim_pthr                    \
                    "-${csim_sided}"                    \
                    -inset "${omema}[${groupid}:t]"`

echo "++ The final cluster volume threshold is:  $clust_thrvol"
echo "++ The voxelwise stat value threshold is:  $voxstat_thr"

# ================== Make cluster maps =====================

# Run the 'clusterize' program to make maps of the clusters (sorted by
# size) and to output a cluster-masked map of the effect estimate (EE)
# data, in this case %BOLD fluctuation values-- i.e., the stuff we
# should report.
#
# The main inputs are: dataset contain the statistic volume to be
# thresholded, a mask within which to find clusters, and the ClustSim
# parameters+outputs.  A common addition to this list of inputs is to
# specify the location of the EE volume in the input dset, for
# reporting the EE info within any found clusters.
#
# SPECS:
#  3dClusterize \
#      -inset      :input dset to get info from; like both stat and eff data
#      -ithr       :str label of stat vol in inset; or, use vol index: 1
#      -idat       :(opt) str label EE vol in inset; or, use vol index: 0
#      -mask       :same mask input to 3dClustSim (here, whole brain)
#      -bisided    :specify *type* of test; followed by threshold limit info
#      -NN         :what neighborhood definition was used? 
#      -clust_nvox :cluster size threshold from 3dClustSim
#      -pref_map   :name for cluster map output vol
#      -pref_dat   :name for cluster-masked EE output vol
#      > ${csim_pref}_report.txt :dump text table of cluster report to file
#

3dClusterize                                   \
    -inset  ${omema}                           \
    -ithr   "${groupid}:t"                     \
    -idat   "${groupid}:b"                     \
    -mask   mask.nii.gz                        \
    -${csim_sided} -$voxstat_thr $voxstat_thr  \
    -NN             ${csim_neigh}              \
    -clust_nvox     ${clust_thrvol}            \
    -pref_map       ${csim_pref}_map.nii.gz    \
    -pref_dat       ${csim_pref}_EE.nii.gz     \
    > ${csim_pref}_report.txt

echo "++ Clusterizing complete."

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

