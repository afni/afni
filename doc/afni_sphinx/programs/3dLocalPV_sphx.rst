*********
3dLocalPV
*********

.. _ahelp_3dLocalPV:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dLocalPV [options] inputdataset
    * You may want to use 3dDetrend before running this program,
       or at least use the '-polort' option.
    * This program is highly experimental.  And slowish.
    * Computes the SVD of the time series from a neighborhood of each
       voxel.  An inricate way of 'smoothing' 3D+time datasets, sort of.
    * This is like 3dLocalSVD, except that the '-vproj' option doesn't
       allow anything but 1 and 2 dimensional projection.  This is because
       3dLocalPV uses a special method to compute JUST the first 1 or 2
       principal vectors -- faster than 3dLocalSVD, but less general.
    
    Options:
     -mask mset          = restrict operations to this mask
     -automask           = create a mask from time series dataset
     -prefix ppp         = save SVD vector result into this new dataset
                            [default = 'LocalPV']
     -prefix2 qqq        = save second principal vector into this new dataset
                            [default = don't save it]
     -evprefix ppp       = save singular value at each voxel into this dataset
                            [default = don't save]
     -input inputdataset = input time series dataset
     -nbhd nnn           = e.g., 'SPHERE(5)' 'TOHD(7)' etc.
     -despike            = remove time series spikes from input dataset
     -polort p           = detrending
     -vnorm              = normalize data vectors [strongly recommended]
     -vproj [2]          = project central data time series onto local SVD vector;
                            if followed by '2', then the central data time series
                            will be projected on the the 2-dimensional subspace
                            spanned by the first 2 principal SVD vectors.
                            [default: just output principal singular vector]
                            [for 'smoothing' purposes, '-vnorm -vproj' is an idea]
    
    Notes:
    * On my Mac Pro, about 30% faster than 3dLocalSVD computing the same thing.
    * If you're curious, the 'special method' used for the eigensolution is
      a variant of matrix power iteration, called 'simultaneous iteration'.
    * By contrast, 3dLocalSVD uses EISPACK functions for eigensolution-izing.
    
     =========================================================================
    * This binary version of 3dLocalPV is compiled using OpenMP, a semi-
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
