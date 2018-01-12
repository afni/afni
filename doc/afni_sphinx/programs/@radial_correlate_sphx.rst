.. contents:: 
    :depth: 4 

*****************
@radial_correlate
*****************

.. code-block:: none

    -----------------------------------------------------------------
    @radial_correlate       - check datasets for correlation artifact
    
        usage : @radial_correlate [options] datasets ...
    
    This program computes the correlation at each voxel with the average
    time series in a 20 mm radius (by default).  If there is basically
    one high-correlation cluster, it is suggestive of a coil artifact.
    
    Note that significant motion can also cause such an effect.  But
    while motion correlations will tend to follow the edge of the brain,
    coil artifacts will tend to appear in large, dense clusters.
    
    If people really care, I may add an option to see how large a sphere
    might fit within the biggest cluster.  A big sphere would be more
    suggestive of a coil artifact, rather than motion.  But adding such
    an option sounds suspiciously like work.
    
      inputs: a list of EPI datasets (after any options)
      output: a directory containing correlation volumes (and more)
    
    -----------------------------------------------------------------
    
    Common examples (note that datasets are always passed last):
    
      1a. Run default operation on a list of EPI datasets (so just create
          the correlation volumes).
    
              @radial_correlate pb00.FT.*.HEAD
    
      1b. Similar to 1a, but specify a results directory for correlations.
    
              @radial_correlate -rdir new.results pb00.FT.*.HEAD
    
      2.  Do a cluster test on existing correlation volumes.  Note that
          this still uses the results directory variable, rdir.
    
              @radial_correlate -do_corr no -do_clust yes pb00.FT.*.HEAD
    
      3.  Run a complete test, both creating the correlation volumes, and
          then looking for large clusters of high correlations.
          Specify a mask.
    
              @radial_correlate -do_clust yes -mask full_mask.FT+orig pb00.FT.*.HEAD
    
      4.  Run a complete test, but alter some clustering options.
            - threshold at 0.7 (instead of the default 0.9)
            - increase the minimum cluster size (frac of mask) to 0.05
            - decrease the correlation sphere radius (from 20 mm) to 10 mm
    
              @radial_correlate -do_clust yes                   \\
                  -cthresh 0.7 -frac_limit 0.05 -sphere_rad 10  \\
                  pb00.FT.*.HEAD
    
    -----------------------------------------------------------------
    
    Overview of processing steps: 
    
    0. The first 3 TRs are removed from the input (see -nfirst),
       and an automask is created (limiting all future computations).
       Any -mask overrides the automask operation.
       If -do_corr is 'no', this is skipped.
    
       (see -do_corr)
    
    1. The correlation dataset is created (unless -do_corr is 'no').
    
       (see -sphere_rad, -do_corr, -do_clust)
    
       At each voxel, compute the correlation either within a sphere
       or with the average masked time series.
    
       a. within a sphere (if -sphere_rad is not 0)
    
          At each voxel, compute the average time series within a
          sphere of radius 20 mm (see -sphere_rad), and correlate the
          time series with this averaged result.
    
       b. with the average masked time series (if -sphere_rad is 0)
    
          The demeaned data is scaled to have unit length (sumsq=1).
          Then compute the mean time series over the automask ROI
          (so across the expected brain).
          Correlate each voxel time series with the mean time series.
    
       If -do_clust is 'no', this is the last step.
    
    2. Threshold the result (if -do_clust is 'yes').
    
       (see -cthresh, -percentile, -do_clust)
    
       Threshold the correlations either at a static value (see -cthresh),
       or at a certain percentile (see -percentile).
    
       a. at r=cthresh (if -cthresh is not 0)
    
          Simply threshold the correlations at this value, maybe 0.9.
    
          (see -cthresh)
    
       b. at r=percentile (if -cthresh is 0)
    
          Compute the given percentile (maybe 80), and threshold at
          that value, whatever it turns out to be.
    
          Note that when using an 80-percent threshold, for example,
          then 20-percent of the voxels should survive the cutoff.
          Later, the question will be how they cluster.
    
          (see -percentile)
    
    3. if the percentile threshold is too small, considered the data okay
    
       (see -min_thr)
    
       In the case of -percentile above (meaning -cthresh is 0), if
       the resulting threshold is not large enough, then we do not
       expect the data to have a problem.
    
    4. compare largest cluster to mask volume
    
       (see -frac_limit)
    
       Compute the size of the largest correlation cluster above the
       previous threshold (either -cthresh or via -percentile).  Then
       compute the fraction of the mask volume that this cluster
       occupies.
    
       If the largest cluster is a large fraction of the mask, then
       we expect there might be a problem (because most of the high
       correlation voxels are in one cluster).
    
       Otherwise, if the high-correlation voxels are scattered about
       the volume, we do not expect any problem.
    
       For example, if the largest surviving cluster is more than 5%
       of the mask, the data is consider to FAIL (see -frac_limit).
    
    -----------------------------------------------------------------
    
        usage : @radial_correlate [options] datasets ...
    
    ---------------------------------------------
    
    general options:
    
       -help             : show this help
    
       -hist             : show modification history
    
       -do_clust yes/no  : clust correlation volumes? (yes or no)
                           default = no
    
                           If 'no', only create the correlation volumes.
                           Otherwise, run clustering and look for large
                           artifacts from bad coil channels.
    
       -do_corr yes/no   : create correlation volumes (yes or no)
                           default = yes
    
                           If 'yes', create the correlation volumes.
                           If 'no', simply assume they already exist.
                           This is for re-testing a previous execution.
    
       -rdir RESULTS_DIR : directory to do computations in
                           default = corr_test.results
    
       -ver              : show version number
    
    ---------------------------------------------
    
    computational options:
    
       -cthesh THRESH    : threshold on correlation values
                           (if 0, use percentile, else use this)
                           default = 0.9
    
       -frac_limit LIMIT : min mask fraction surviving cluster
                           default = 0.02
    
       -mask MASK_DSET   : specify a mask dataset to replace automask
                           default = automask
                           This mask is expected to cover the brain.
    
       -nfirst NFIRST    : number of initial TRs to remove
                           default = 3
    
       -min_thr THR      : min percentile threshold to be considered
                           default = 0.45
    
       -percentile PERC  : percentile to use as threshold
                           default = 80
    
       -sphere_rad RAD   : generate correlations within voxel spheres
                           (if 0, go against average time series)
                           default = 20
    
    R Reynolds, Aug, 2011
