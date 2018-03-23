.. _ahelp_3dAutoTcorrelate:

****************
3dAutoTcorrelate
****************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAutoTcorrelate [options] dset
    Computes the correlation coefficient between the time series of each
    pair of voxels in the input dataset, and stores the output into a
    new anatomical bucket dataset [scaled to shorts to save memory space].
    
    *** Also see program 3dTcorrMap ***
    
    Options:
      -pearson  = Correlation is the normal Pearson (product moment)
                   correlation coefficient [default].
      -eta2     = Output is eta^2 measure from Cohen et al., NeuroImage, 2008:
                   http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2705206/
                   http://dx.doi.org/10.1016/j.neuroimage.2008.01.066
                 ** '-eta2' is intended to be used to measure the similarity
                     between 2 correlation maps; therefore, this option is
                     to be used in a second stage analysis, where the input
                     dataset is the output of running 3dAutoTcorrelate with
                     the '-pearson' option -- the voxel 'time series' from
                     that first stage run is the correlation map of that
                     voxel with all other voxels.
                 ** '-polort -1' is recommended with this option!
                 ** Odds are you do not want use this option if the dataset
                    on which eta^2 is to be computed was generated with
                    options -mask_only_targets or -mask_source.
                    In this program, the eta^2 is computed between pseudo-
                    timeseries (the 4th dimension of the dataset).
                    If you want to compute eta^2 between sub-bricks then use
                    3ddot -eta2 instead.
      -spearman AND -quadrant are disabled at this time :-(
    
      -polort m = Remove polynomial trend of order 'm', for m=-1..3.
                   [default is m=1; removal is by least squares].
                   Using m=-1 means no detrending; this is only useful
                   for data/information that has been pre-processed.
    
      -autoclip = Clip off low-intensity regions in the dataset,
      -automask =  so that the correlation is only computed between
                   high-intensity (presumably brain) voxels.  The
                   mask is determined the same way that 3dAutomask works.
    
      -mask mmm = Mask of both 'source' and 'target' voxels.
                  ** Restricts computations to those in the mask.  Output
                      volumes are restricted to masked voxels.  Also, only
                      masked voxels will have non-zero output.
                  ** A dataset with 1000 voxels would lead to output of
                      1000 sub-bricks.  With a '-mask' of 50 voxels, the
                      output dataset have 50 sub-bricks, where the 950
                      unmasked voxels would be all zero in all 50 sub-bricks
                      (unless option '-mask_only_targets' is also used).
                  ** The mask is encoded in the output dataset header in the
                      attribute named 'AFNI_AUTOTCORR_MASK' (cf. 3dMaskToASCII).
    
      -mask_only_targets = Provide output for all voxels.
                  ** Used with '-mask': every voxel is correlated with each
                      of the mask voxels.  In the example above, there would
                      be 50 output sub-bricks; the n-th output sub-brick
                      would contain the correlations of the n-th voxel in
                      the mask with ALL 1000 voxels in the dataset (rather
                      than with just the 50 voxels in the mask).
    
      -mask_source sss = Provide ouput for voxels only in mask sss
                   ** For each seed in mask mm, compute correlations only with 
                       non-zero voxels in sss. If you have 250 non-zero voxels 
                       in sss, then the output will still have 50 sub-bricks, but
                       each n-th sub-brick will have non-zero values at the 250
                       non-zero voxels in sss
                       Do not use this option along with -mask_only_targets
    
      -prefix p = Save output into dataset with prefix 'p'
                   [default prefix is 'ATcorr'].
      -out1D FILE.1D = Save output in a text file formatted thusly:
                       Row 1 contains the 1D indices of non zero voxels in the 
                             mask from option -mask.
                       Column 1 contains the 1D indices of non zero voxels in the
                             mask from option -mask_source
                       The rest of the matrix contains the correlation/eta2 
                       values. Each column k corresponds to sub-brick k in 
                       the output volume p.
                       To see 1D indices in AFNI, right click on the top left
                       corner of the AFNI controller - where coordinates are
                       shown - and chose voxel indices.
                       A 1D index (ijk) is computed from the 3D (i,j,k) indices:
                           ijk = i + j*Ni + k*Ni*Nj , with Ni and Nj being the
                       number of voxels in the slice orientation and given by:
                           3dinfo -ni -nj YOUR_VOLUME_HERE
                       This option can only be used in conjunction with 
                       options -mask and -mask_source. Otherwise it makes little
                       sense to write a potentially enormous text file.
    
      -time     = Mark output as a 3D+time dataset instead of an anat bucket.
    
      -mmap     = Write .BRIK results to disk directly using Unix mmap().
                   This trick can speed the program up  when the amount
                   of memory required to hold the output is very large.
                  ** In many case, the amount of time needed to write
                     the results to disk is longer than the CPU time.
                     This option can shorten the disk write time.
                  ** If the program crashes, you'll have to manually
                     remove the .BRIK file, which will have been created
                     before the loop over voxels and written into during
                     that loop, rather than being written all at once
                     at the end of the analysis, as is usually the case.
                  ** If the amount of memory needed is bigger than the
                     RAM on your system, this program will be very slow
                     with or without '-mmap'.
                  ** This option won't work with NIfTI-1 (.nii) output!
    
    Example: correlate every voxel in mask_in+tlrc with only those voxels in
             mask_out+tlrc (the rest of each volume is zero, for speed).
             Assume detrending was already done along with other pre-processing.
             The output will have one volume per masked voxel in mask_in+tlrc.
             Volumes will be labeled by the ijk index triples of mask_in+tlrc.
    
       3dAutoTcorrelate -mask_source mask_out+tlrc -mask mask_in+tlrc \
                        -polort -1 -prefix test_corr clean_epi+tlrc
    
    Notes:
     * The output dataset is anatomical bucket type of shorts
        (unless '-time' is used).
     * Values are scaled so that a correlation (or eta-squared)
        of 1 corresponds to a value of 10000.
     * The output file might be gigantic and you might run out
        of memory running this program.  Use at your own risk!
       ++ If you get an error message like
            *** malloc error for dataset sub-brick
          this means that the program ran out of memory when making
          the output dataset.
       ++ If this happens, you can try to use the '-mmap' option,
          and if you are lucky, the program may actually run.
     * The program prints out an estimate of its memory usage
        when it starts.  It also prints out a progress 'meter'
        to keep you pacified.
     * This is a quick hack for Peter Bandettini. Now pay up.
     * OpenMP-ized for Hang Joon Jo.  Where's my baem-sul?
    
    -- RWCox - 31 Jan 2002 and 16 Jul 2010
    
     =========================================================================
    * This binary version of 3dAutoTcorrelate is compiled using OpenMP, a semi-
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
