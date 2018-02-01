******************
3dDegreeCentrality
******************

.. _3dDegreeCentrality:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dDegreeCentrality [options] dset
      Computes voxelwise weighted and binary degree centrality and
      stores the result in a new 3D bucket dataset as floats to
      preserve their values. Degree centrality reflects the strength and
      extent of the correlation of a voxel with every other voxel in
      the brain.
    
      Conceptually the process involves: 
          1. Calculating the correlation between voxel time series for
             every pair of voxels in the brain (as determined by masking)
          2. Applying a threshold to the resulting correlations to exclude
             those that might have arisen by chance, or to sparsify the
             connectivity graph.
          3. At each voxel, summarizing its correlation with other voxels
             in the brain, by either counting the number of voxels correlated
             with the seed voxel (binary) or by summing the correlation 
             coefficients (weighted).
       Practically the algorithm is ordered differently to optimize for
       computational time and memory usage.
    
       The threshold can be supplied as a correlation coefficient, 
       or a sparsity threshold. The sparsity threshold reflects the fraction
       of connections that should be retained after the threshold has been
       applied. To minimize resource consumption, using a sparsity threshold
       involves a two-step procedure. In the first step, a correlation
       coefficient threshold is applied to substantially reduce the number
       of correlations. Next, the remaining correlations are sorted and a
       threshold is calculated so that only the specified fraction of 
       possible correlations are above threshold. Due to ties between
       correlations, the fraction of correlations that pass the sparsity
       threshold might be slightly more than the number specified.
    
       Regardless of the thresholding procedure employed, negative 
       correlations are excluded from the calculations.
    
    Options:
      -pearson  = Correlation is the normal Pearson (product moment)
                   correlation coefficient [default].
      -spearman AND -quadrant are disabled at this time :-(
    
      -thresh r = exclude correlations <= r from calculations
      -sparsity s = only use top s percent of correlations in calculations
                    s should be an integer between 0 and 100. Uses an
                    an adaptive thresholding procedure to reduce memory.
                    The speed of determining the adaptive threshold can
                    be improved by specifying an initial threshold with
                    the -thresh flag.
    
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
                   contain bricks for both 'weighted' or 'degree' centrality
                   [default prefix is 'deg_centrality'].
    
      -out1D f = Save information about the above threshold correlations to
                  1D file 'f'. Each row of this file will contain:
                   Voxel1 Voxel2 i1 j1 k1 i2 j2 k2 Corr
                  Where voxel1 and voxel2 are the 1D indices of the pair of
                  voxels, i j k correspond to their 3D coordinates, and Corr
                  is the value of the correlation between the voxel time courses.
    
    Notes:
     * The output dataset is a bucket type of floats.
     * The program prints out an estimate of its memory used
        when it ends.  It also prints out a progress 'meter'
        to keep you pacified.
    
    -- RWCox - 31 Jan 2002 and 16 Jul 2010
    -- Cameron Craddock - 26 Sept 2015 
    
     =========================================================================
    * This binary version of 3dDegreeCentrality is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system.  You can control this value by setting environment variable
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
    * How many threads are useful?  That varies with the program, and how well
       it was coded.  You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 8.
     =========================================================================
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
