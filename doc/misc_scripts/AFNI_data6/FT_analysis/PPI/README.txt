
# ===========================================================================
# This README file shows how to walk through this sample PPI analysis.
# 
# This README file is also a script that can be simply run, or perferably
# cut-and-pasted slowly onto a command line (to review the process).
#
# It should be run on the *results* from the AFNI_data6 single subject
# class data, located under AFNI_data6/FT_analysis.
# 
# For example, consider doing the initial analysis by running:
# 
#    cd AFNI_data6/FT_analysis
#    tcsh s04.cmd.usubj
#
# which puts results under ~/subject_results/group.horses/subj.FT/FT.results
# referenced as $subjdir, below.
#
# Then one can apply this script (assuming the main directories are correct).
#
# ----------------------
# R Reynolds   Oct, 2016
# ===========================================================================


# note location of scripts and data
set scriptdir = ~/AFNI_data6/FT_analysis/PPI
set subjdir   = ~/subject_results/group.horses/subj.FT/FT.results



# ----------------------------------------
# do all of the work in the FT.results directory...
cd $subjdir



# ===========================================================================
# optional section: generate seed time series

# ----------------------------------------
# create errts time series, ppi.pre.errts.FT+tlrc

# adjust $data_root in and run...
tcsh $scriptdir/cmd.ppi.1.ap.pre

# which creates proc.3dd.ppi.pre (to be run from results)
tcsh proc.3dd.ppi.pre


# ----------------------------------------
# generate seed time series, ppi.seed.1D

# start with seed around Vrel peak @ 24, 86, -4 (24L, 86P, 4I)
# (this location has large visual and autidory t-stats but a low v-a contrast)
echo 24 86 -4 | 3dUndump -xyz -srad 5 -master stats.FT+tlrc -prefix ppi.mask -

# generate ppi.seed.1D (note that mask dset is unneeded, but visually useful)
3dmaskave -quiet -mask ppi.mask+tlrc ppi.pre.errts.FT+tlrc > ppi.seed.1D



# ===========================================================================
# generate PPI regressors from seed and timing files
# (script uses 'set seed = ppi.seed.1D')

tcsh $scriptdir/cmd.ppi.2.make.regs

# and copy the results into the stimuli directory
cp work.Laud/p6.* ppi.seed.1D stimuli

# and just to see consider:
#    1dplot -one ppi.seed.1D work.Laud/p7.Laud.sum.PPI.1D
#    1dplot ppi.seed.1D work.Laud/p6.*


# ===========================================================================
# create and run a 3dDeconvolve command for the PPI
# (still run from $subjdir)

# create the 3dDeconvolve command, proc.3dd.ppi.post.full
tcsh $scriptdir/cmd.ppi.3.ap.post

# and run it
tcsh proc.3dd.ppi.post.full



# ===========================================================================
# comments...

# - this data is not designed to capture a PPI effect
# - the results are in PPI.full.stats.FT+tlrc
# - looking at the PPI volume #20 (PPI:V-A_GLT#0_Tstat), and clustering
#   at a threshold of 3.314 (p<0.001), min volume of 20 voxels (just to see),
#   cluster #6 (peak t at 29 84 14) _might_ be interesting (see all_runs plot)
# - cluster #1 looks like a simple motion effect
