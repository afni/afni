***********
3dLocalstat
***********

.. _ahelp_3dLocalstat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dLocalstat [options] dataset
    
    This program computes statistics at each voxel, based on a
    local neighborhood of that voxel.
     - The neighborhood is defined by the '-nbhd' option.
     - Statistics to be calculated are defined by the '-stat' option(s).
    
    OPTIONS
    -------
     -nbhd 'nnn' = The string 'nnn' defines the region around each
                   voxel that will be extracted for the statistics
                   calculation.  The format of the 'nnn' string are:
                   * 'SPHERE(r)' where 'r' is the radius in mm;
                     the neighborhood is all voxels whose center-to-
                     center distance is less than or equal to 'r'.
                     ** The distances are computed in 3 dimensions,
                        so a SPHERE(1) on a 1mm3 grid gives a 7 voxel-
                        neighborhood - the center voxel and the six
                        facing voxels, 4 in plane and 2 above and below.
                        A SPHERE(1.42) contains 19 voxels, the center voxel
                        with the 8 others in plane, and the 5 above and
                        below (all voxels sharing an edge with the center)
                        A SPHERE(1.74) contains 27 voxels, all voxels
                        sharing a face, edge or corner with the center
                     ** A negative value for 'r' means that the region
                        is calculated using voxel indexes rather than
                        voxel dimensions; that is, the neighborhood
                        region is a "sphere" in voxel indexes of
                        "radius" abs(r).
                   * 'RECT(a,b,c)' is a rectangular block which
                     proceeds plus-or-minus 'a' mm in the x-direction,
                     'b' mm in the y-direction, and 'c' mm in the
                     z-direction.  The correspondence between the
                     dataset xyz axes and the actual spatial orientation
                     can be determined by using program 3dinfo.
                     ** Note the a,b,c are not the full dimensions of
                        of the block. They are radially used - effectively
                        half the dimension of a side. So if one wanted to
                        compute a 5-slice projection on a 1mm3 volume,
                        then a RECT(0,0,2) would be appropriate, and 
                        the program would report 5 voxels used in the mask
                        Any dimension less than a voxel will avoid
                        voxels in that direction.
                     ** A negative value for 'a' means that the region
                        extends plus-and-minus abs(a) voxels in the
                        x-direction, rather than plus-and-minus a mm.
                        Mutatis mutandum for negative 'b' and/or 'c'.
                   * 'RHDD(a)' where 'a' is the size parameter in mm;
                     this is Kepler's rhombic dodecahedron [volume=2*a^3].
                   * 'TOHD(a)' where 'a' is the size parameter in mm;
                     this is a truncated octahedron. [volume=4*a^3]
                     ** This is the polyhedral shape that tiles space
                        and is the most 'sphere-like'.
                   * If no '-nbhd' option is given, the region extracted
                     will just be the voxel and its 6 nearest neighbors.
                   * Voxels not in the mask (if any) or outside the
                     dataset volume will not be used.  This means that
                     different output voxels will have different numbers
                     of input voxels that went into calculating their
                     statistics.  The 'num' statistic can be used to
                     get this count on a per-voxel basis, if you need it.
    
     -stat sss   = Compute the statistic named 'sss' on the values
                   extracted from the region around each voxel:
                   * mean   = average of the values
                   * stdev  = standard deviation
                   * var    = variance (stdev*stdev)
                   * cvar   = coefficient of variation = stdev/fabs(mean)
                   * median = median of the values
                   * MAD    = median absolute deviation
                   * min    = minimum
                   * max    = maximum
                   * absmax = maximum of the absolute values
                   * mode   = mode
                   * nzmode = non-zero mode
                   * num    = number of the values in the region:
                              with the use of -mask or -automask,
                              the size of the region around any given
                              voxel will vary; this option lets you
                              map that size.  It may be useful if you
                              plan to compute a t-statistic (say) from
                              the mean and stdev outputs.
                   * filled = 1 or fillvalue if all voxels in neighborhood
                              are within mask
                   * unfilled = 1 or unfillvalue if not all voxels in neighborhood
                              are within mask
                   * hasmask = unfillvalue if neighborhood contains a specified
                              mask value
                   * hasmask2 = unfillvalue if neighborhood contains an alternate
                              mask value
                   * sum    = sum of the values in the region
                   * FWHM   = compute (like 3dFWHM) image smoothness
                              inside each voxel's neighborhood.  Results
                              are in 3 sub-bricks: FWHMx, FWHMy, and FWHMz.
                              Places where an output is -1 are locations
                              where the FWHM value could not be computed
                              (e.g., outside the mask).
                   * FWHMbar= Compute just the average of the 3 FWHM values
                              (normally would NOT do this with FWHM also).
                   * perc:P0:P1:Pstep = 
                              Compute percentiles between P0 and P1 with a 
                              step of Pstep.
                              Default P1 is equal to P0 and default P2 = 1
                   * rank   = rank of the voxel's intensity
                   * frank  = rank / number of voxels in neighborhood
                   * P2skew = Pearson's second skewness coefficient
                               3 * (mean - median) / stdev 
                   * ALL    = all of the above, in that order 
                             (except for FWHMbar and perc).
                   * mMP2s  = Exactly the same output as:
                              -stat median -stat MAD -stat P2skew
                              but it a little faster
                   * mmMP2s  = Exactly the same output as:
                           -stat mean -stat median -stat MAD -stat P2skew
                   * diffs   = Compute differences between central voxel
                               and all neighbors. Values output are the 
                               average difference, followed by the min and max
                               differences.
                   * list    = Just output the voxel values in the neighborhood
                               The order in which the neighbors are listed 
                               depends on the neighborhood selected. Only
                               SPHERE results in a neighborhood list sorted by
                               the distance from the center.
                               Regardless of the neighborhood however, the first
                               value should always be that of the central voxel.
                   * hist:MIN:MAX:N[:IGN] = Compute the histogram in the voxel's
                               neighborhood. You must specify the min, max, and 
                               the number of bins in the histogram. You can also
                               ignore values outside the [min max] range by 
                               setting IGN to 1. IGN = 0 by default.
                               The histograms are scaled by the number 
                               of values that went into the histogram.
                               That would be the number of non-masked voxels
                               in the neighborhood if outliers are NOT
                               ignored (default).
                           For histograms of labeled datasets, use 3dLocalHistog
    
                   More than one '-stat' option can be used.
    
     -mask mset  = Read in dataset 'mset' and use the nonzero voxels
                   therein as a mask.  Voxels NOT in the mask will
                   not be used in the neighborhood of any voxel. Also,
                   a voxel NOT in the mask will have its statistic(s)
                   computed as zero (0) -- usually (cf. supra).
     -automask   = Compute the mask as in program 3dAutomask.
                   -mask and -automask are mutually exclusive: that is,
                   you can only specify one mask.
    
     -use_nonmask = Just above, I said that voxels NOT in the mask will
                    not have their local statistics computed.  This option
                    will make it so that voxels not in the mask WILL have
                    their local statistics computed from all voxels in
                    their neighborhood that ARE in the mask.
                   * You could use '-use_nonmask' to compute the average
                     local white matter time series, for example, even at
                     non-WM voxels.
    
     -prefix ppp = Use string 'ppp' as the prefix for the output dataset.
                   The output dataset is normally stored as floats.
    
     -datum type = Coerce the output data to be stored as the given type, 
                   which may be byte, short, or float.
                   Default is float
    
     -label_ext LABEXT = Append '.LABEXT' to each sub-brick label 
    
     -reduce_grid Rx [Ry Rz] = Compute output on a grid that is 
                               reduced by a factor of Rx Ry Rz in
                               the X, Y, and Z directions of the 
                               input dset. This option speeds up 
                               computations at the expense of 
                               resolution. You should only use it
                               when the nbhd is quite large with 
                               respect to the input's resolution,
                               and the resultant stats are expected
                               to be smooth. 
                               You can either set Rx, or Rx Ry and Rz.
                               If you only specify Rx the same value
                               is applied to Ry and Rz.
    
     -reduce_restore_grid Rx [Ry Rz] = Like reduce_grid, but also resample
                                       output back to input grid.
     -reduce_max_vox MAX_VOX = Like -reduce_restore_grid, but automatically
                               set Rx Ry Rz so that the computation grid is
                               at a resolution of nbhd/MAX_VOX voxels.
     -grid_rmode RESAM = Interpolant to use when resampling the output with
                         reduce_restore_grid option. The resampling method
                         string RESAM should come from the set 
                         {'NN', 'Li', 'Cu', 'Bk'}.  These stand for
                         'Nearest Neighbor', 'Linear', 'Cubic'
                         and 'Blocky' interpolation, respectively.
                         Default is Linear
     -quiet      = Stop the highly informative progress reports.
     -verb       = a little more verbose.
     -proceed_small_N = Do not crash if neighborhood is too small for 
                        certain estimates.
     -fillvalue x.xx = value used for filled statistic, default=1
     -unfillvalue x.xx = value used for unfilled statistic, default=1
     -maskvalue x.xx = value searched for with has_mask option
     -maskvalue2 x.xx = alternate value for has_mask2 option
    
    Author: RWCox - August 2005.  Instigator: ZSSaad.
    
     =========================================================================
    * This binary version of 3dLocalstat is compiled using OpenMP, a semi-
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
