*****
3dMSE
*****

.. _3dMSE:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dMSE [options] dset
      Computes voxelwise multi-scale entropy.
    Options:
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
                   [default prefix is 'MSE'].
    
      -scales N = The number of scales to be used in the calculation.
                   [default is 5].
    
      -entwin w = The window size used in the calculation.
                   [default is 2].
    
      -rthresh r = The radius threshold for determining if values are the
                    same in the SampleEn calculation, in fractions of the
                    standard deviation.
                   [default is .5].
    
    Notes:
     * The output dataset is a bucket type of floats.
    
    -- RWCox - 31 Jan 2002 and 16 Jul 2010
    -- Cameron Craddock - 26 Sept 2015 
    
     =========================================================================
    * This binary version of 3dMSE is compiled using OpenMP, a semi-
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
    * The number of CPUs on this particular computer system is ...... 8.
    * The maximum number of CPUs that will be used is now set to .... 8.
     =========================================================================
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
