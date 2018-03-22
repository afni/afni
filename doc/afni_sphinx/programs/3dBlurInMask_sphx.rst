************
3dBlurInMask
************

.. _ahelp_3dBlurInMask:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dBlurInMask [options]
    Blurs a dataset spatially inside a mask.  That's all.  Experimental.
    
    OPTIONS
    -------
     -input  ddd = This required 'option' specifies the dataset
                   that will be smoothed and output.
     -FWHM   f   = Add 'f' amount of smoothness to the dataset (in mm).
                  **N.B.: This is also a required 'option'.
     -FWHMdset d = Read in dataset 'd' and add the amount of smoothness
                   given at each voxel -- spatially variable blurring.
                  ** EXPERIMENTAL EXPERIMENTAL EXPERIMENTAL **
     -mask   mmm = Mask dataset, if desired.  Blurring will
                   occur only within the mask.  Voxels NOT in
                   the mask will be set to zero in the output.
     -Mmask  mmm = Multi-mask dataset -- each distinct nonzero
                   value in dataset 'mmm' will be treated as
                   a separate mask for blurring purposes.
                  **N.B.: 'mmm' must be byte- or short-valued!
     -automask   = Create an automask from the input dataset.
                  **N.B.: only 1 masking option can be used!
     -preserve   = Normally, voxels not in the mask will be
                   set to zero in the output.  If you want the
                   original values in the dataset to be preserved
                   in the output, use this option.
     -prefix ppp = Prefix for output dataset will be 'ppp'.
                  **N.B.: Output dataset is always in float format.
     -quiet      = Don't be verbose with the progress reports.
     -float      = Save dataset as floats, no matter what the
                   input data type is.
                  **N.B.: If the input dataset is unscaled shorts, then
                          the default is to save the output in short
                          format as well.  In EVERY other case, the
                          program saves the output as floats. Thus,
                          the ONLY purpose of the '-float' option is to
                          force an all-shorts input dataset to be saved
                          as all-floats after blurring.
    
    NOTES
    -----
     * If you don't provide a mask, then all voxels will be included
         in the blurring.  (But then why are you using this program?)
     * Note that voxels inside the mask that are not contiguous with
         any other voxels inside the mask will not be modified at all!
     * Works iteratively, similarly to 3dBlurToFWHM, but without
         the extensive overhead of monitoring the smoothness.
     * But this program will be faster than 3dBlurToFWHM, and probably
         slower than 3dmerge.
     * Since the blurring is done iteratively, rather than all-at-once as
         in 3dmerge, the results will be slightly different than 3dmerge's,
         even if no mask is used here (3dmerge, of course, doesn't take a mask).
     * If the original FWHM of the dataset was 'S' and you input a value
         'F' with the '-FWHM' option, then the output dataset's smoothness
         will be about sqrt(S*S+F*F).  The number of iterations will be
         about (F*F/d*d) where d=grid spacing; this means that a large value
         of F might take a lot of CPU time!
     * The spatial smoothness of a 3D+time dataset can be estimated with a
         command similar to the following:
              3dFWHMx -detrend -mask mmm+orig -input ddd+orig
     * The minimum number of voxels in the mask is 9
     * Isolated voxels will be removed from the mask!
    
     =========================================================================
    * This binary version of 3dBlurInMask is compiled using OpenMP, a semi-
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
