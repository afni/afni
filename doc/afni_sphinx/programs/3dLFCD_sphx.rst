******
3dLFCD
******

.. _ahelp_3dLFCD:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dLFCD [options] dset
      Computes voxelwise local functional connectivity density and
      stores the result in a new 3D bucket dataset as floats to
      preserve their values. LFCD reflects the strength and
      extent of the correlation of a voxel with other voxels in
      its locally connected cluster.
    
      Conceptually the process involves: 
          1. Calculating the correlation between voxel time series for
             every pair of voxels in the brain (as determined by masking)
          2. Applying a threshold to the resulting correlations to exclude
             those that might have arisen by chance
          3. Find the cluster of above-threshold voxels that are spatially
             adjacent to the target voxel.
          4. Count the number of voxels in the local cluster.
       Practically the algorithm is ordered differently to optimize for
       computational time and memory usage.
    
    
    Options:
      -pearson  = Correlation is the normal Pearson (product moment)
                   correlation coefficient [default].
      -spearman AND -quadrant are disabled at this time :-(
    
      -thresh r = exclude correlations <= r from calculations
    
      -polort m = Remove polynomical trend of order 'm', for m=-1..3.
                   [default is m=1; removal is by least squares].
                   Using m=-1 means no detrending; this is only useful
                   for data/information that has been pre-processed.
    
      -autoclip = Clip off low-intensity regions in the dataset,
      -automask =  so that the correlation is only computed between
                   high-intensity (presumably brain) voxels.  The
                   mask is determined the same way that 3dAutomask works.
    
      -mask mmm = Mask to define 'in-brain' voxels. Reducing the number
                   the number of voxels included in the calculation will
                   significantly speedup the calculation. Consider using
                   a mask to constrain the calculations to the grey matter
                   rather than the whole brain. This is also preferrable
                   to using -autoclip or -automask.
    
      -prefix p = Save output into dataset with prefix 'p', this file will
                   contain bricks for both 'weighted' and 'binarized' lFCD
                   [default prefix is 'LFCD'].
    
    Notes:
     * The output dataset is a bucket type of floats.
     * The program prints out an estimate of its memory used
        when it ends.  It also prints out a progress 'meter'
        to keep you pacified.
    
    -- RWCox - 31 Jan 2002 and 16 Jul 2010
    -- Cameron Craddock - 13 Nov 2015 
    
     =========================================================================
    * This binary version of 3dLFCD is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system. You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful? That varies with the program, and how well
       it was coded. You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 12.
     =========================================================================
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
