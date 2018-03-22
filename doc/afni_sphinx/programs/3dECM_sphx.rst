*****
3dECM
*****

.. _3dECM:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dECM [options] dset
      Computes voxelwise local functional connectivity density and
      stores the result in a new 3D bucket dataset as floats to
      preserve their values. ECM reflects the strength and
      extent of a voxel's global connectivity as well as the
      importance of the voxels that it is directly connected to.
    
      Conceptually the process involves: 
          1. Calculating the correlation between voxel time series for
             every pair of voxels in the brain (as determined by masking)
          2. Calculate the eigenvector corresponding to the largest
             eigenvalue of the similarity matrix.
    
      Guaranteeing that this eigenvector is unique and all positive
      requires that the similarity matrix is strictly positive. This
      is enforced by either adding one to the correlations (Lohmann 
      et. al. 2010), or by adding one and dividing by two (Wink et al.
      2012).
    
      Practically the power iteration algorithm described in Wink et
      al. 2012) is used to optimize for computational time and memory
      usage.
    
      Lohmann G, Margulies DS, Horstmann A, Pleger B, Lepsien J, et al.
          (2010) Eigenvector Centrality Mapping for Analyzing
          Connectivity Patterns in fMRI Data of the Human Brain. PLoS
          ONE 5(4): e10232. doi: 10.1371/journal.pone.0010232
    
      Wink, A. M., de Munck, J. C., van der Werf, Y. D., van den Heuvel,
          O. A., & Barkhof, F. (2012). Fast Eigenvector Centrality
          Mapping of Voxel-Wise Connectivity in Functional Magnetic
          Resonance Imaging: Implementation, Validation, and
          Interpretation. Brain Connectivity, 2(5), 265-274.
          doi:10.1089/brain.2012.0087
    
    
    Options:
      -full       = uses the full power method (Lohmann et. al. 2010).
                    Enables the use of thresholding and calculating
                    thresholded centrality. Uses sparse array to reduce 
                    memory requirement. Automatically selected if 
                    -thresh, or -sparsity are used.
      -fecm       = uses a shortcut that substantially speeds up 
                    computation, but is less flexibile in what can be
                    done the similarity matrix. i.e. does not allow 
                    thresholding correlation coefficients. based on 
                    fast eigenvector centrality mapping (Wink et. al
                    2012). Default when -thresh, or -sparsity
                    are NOT used.
      -thresh r   = exclude connections with correlation < r. cannot be
                    used with FECM
      -sparsity p = only include the top p% connectoins in the calculation
                    cannot be used with FECM method. (default = 100)
      -shift s    = value that should be added to correlation coeffs to
                    enforce non-negativity, s >= 0. [default = 0.0, unless
                    -fecm is specified in which case the default is 1.0
                    (e.g. Wink et al 2012)].
      -scale x    = value that correlation coeffs should be multiplied by
                    after shifting, x >= 0 [default = 1.0, unless -fecm is
                    specified in which case the default is 0.5 (e.g. Wink et
                    al 2012)].
      -eps p      = sets the stopping criterion for the power iteration
                    l2|v_old - v_new| < eps*|v_old|. default = .001 (0.1%)
      -max_iter i = sets the maximum number of iterations to use in
                    in the power iteration. default = 1000
    
      -polort m   = Remove polynomical trend of order 'm', for m=0..3.
                    [default is m=1; removal is by least squares].
                    Using m=0 means that just the mean is removed.
    
      -autoclip   = Clip off low-intensity regions in the dataset,
      -automask   = so that the correlation is only computed between
                    high-intensity (presumably brain) voxels.  The
                    mask is determined the same way that 3dAutomask works.
    
      -mask mmm   = Mask to define 'in-brain' voxels. Reducing the number
                    the number of voxels included in the calculation will
                    significantly speedup the calculation. Consider using
                    a mask to constrain the calculations to the grey matter
                    rather than the whole brain. This is also preferrable
                    to using -autoclip or -automask.
    
      -prefix p   = Save output into dataset with prefix 'p'
                    [default prefix is 'ecm'].
    
      -memory G   = Calculating eignevector centrality can consume alot
                    of memory. If unchecked this can crash a computer
                    or cause it to hang. If the memory hits this limit
                    the tool will error out, rather than affecting the
                    system [default is 2G].
    
    Notes:
     * The output dataset is a bucket type of floats.
     * The program prints out an estimate of its memory used
        when it ends.  It also prints out a progress 'meter'
        to keep you pacified.
    
    -- RWCox - 31 Jan 2002 and 16 Jul 2010
    -- Cameron Craddock - 13 Nov 2015 
    -- Daniel Clark - 14 March 2016
    
     =========================================================================
    * This binary version of 3dECM is compiled using OpenMP, a semi-
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
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
