*********
3dTcorr1D
*********

.. _3dTcorr1D:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTcorr1D [options] xset y1D
    Computes the correlation coefficient between each voxel time series
    in the input 3D+time dataset 'xset' and each column in the 1D time
    series file 'y1D', and stores the output values in a new dataset.
    
    OPTIONS:
      -pearson  = Correlation is the normal Pearson (product moment)
                    correlation coefficient [this is the default method].
      -spearman = Correlation is the Spearman (rank) correlation
                    coefficient.
      -quadrant = Correlation is the quadrant correlation coefficient.
      -ktaub    = Correlation is Kendall's tau_b coefficient.
                  ++ For 'continuous' or finely-discretized data, tau_b and
                     rank correlation are nearly equivalent (but not equal).
    
      -Fisher   = Apply the 'Fisher' (inverse hyperbolic tangent) transformation
                    to the results.
                  ++ It does not make sense to use this with '-ktaub', but if
                     you want to do it, the program will not stop you.
    
      -prefix p = Save output into dataset with prefix 'p'
                   [default prefix is 'Tcorr1D'].
    
      -mask mmm = Only process voxels from 'xset' that are nonzero
                    in the 3D mask dataset 'mmm'.
                  ++ Other voxels in the output will be set to zero.
    
      -float    = Save results in float format [the default format].
      -short    = Save results in scaled short format [to save disk space].
    
    NOTES:
    * The output dataset is functional bucket type, with one sub-brick
       per column of the input y1D file.
    * No detrending, blurring, or other pre-processing options are available;
       if you want these things, see 3dDetrend or 3dTproject or 3dcalc.
       [In other words, this program presumes you know what you are doing!]
    * Also see 3dTcorrelate to do voxel-by-voxel correlation of TWO
       3D+time datasets' time series, with similar options.
    * You can extract the time series from a single voxel with given
       spatial indexes using 3dmaskave, and then run it with 3dTcorr1D:
        3dmaskave -quiet -ibox 40 30 20 epi_r1+orig > r1_40_30_20.1D
        3dTcorr1D -pearson -Fisher -prefix c_40_30_20 epi_r1+orig r1_40_30_20.1D
    * http://en.wikipedia.org/wiki/Correlation
    * http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
    * http://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
    * http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient
    
    -- RWCox - Apr 2010
             - Jun 2010: Multiple y1D columns; OpenMP; -short; -mask.
    
     =========================================================================
    * This binary version of 3dTcorr1D is compiled using OpenMP, a semi-
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
