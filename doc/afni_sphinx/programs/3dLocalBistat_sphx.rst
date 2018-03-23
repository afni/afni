.. _ahelp_3dLocalBistat:

*************
3dLocalBistat
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dLocalBistat [options] dataset1 dataset2
    
    This program computes statistics between 2 datasets,
    at each voxel, based on a local neighborhood of that voxel.
     - The neighborhood is defined by the '-nbhd' option.
     - Statistics to be calculated are defined by the '-stat' option(s).
     - The 2 input datasets should have the same number of sub-bricks.
     - OR dataset1 should have 1 sub-brick and dataset2 can have more than 1:
       - In which case, the statistics of dataset2 against dataset1 are
         calculated for the #0 sub-brick of dataset1 against each sub-brick
         of dataset2.
    
    OPTIONS
    -------
     -nbhd 'nnn' = The string 'nnn' defines the region around each
                   voxel that will be extracted for the statistics
                   calculation.  The format of the 'nnn' string are:
                   * 'SPHERE(r)' where 'r' is the radius in mm;
                     the neighborhood is all voxels whose center-to-
                     center distance is less than or equal to 'r'.
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
                     ** A negative value for 'a' means that the region
                        extends plus-and-minus abs(a) voxels in the
                        x-direction, rather than plus-and-minus a mm.
                        Mutatis mutandum for negative 'b' and/or 'c'.
                   * 'RHDD(r)' is a rhombic dodecahedron of 'radius' r.
                   * 'TOHD(r)' is a truncated octahedron of 'radius' r.
    
     -stat sss   = Compute the statistic named 'sss' on the values
                   extracted from the region around each voxel:
                   * pearson  = Pearson correlation coefficient
                   * spearman = Spearman correlation coefficient
                   * quadrant = Quadrant correlation coefficient
                   * mutinfo  = Mutual Information
                   * normuti  = Normalized Mutual Information
                   * jointent = Joint entropy
                   * hellinger= Hellinger metric
                   * crU      = Correlation ratio (Unsymmetric)
                   * crM      = Correlation ratio (symmetrized by Multiplication)
                   * crA      = Correlation ratio (symmetrized by Addition)
                   * L2slope  = slope of least-squares (L2) linear regression of
                                the data from dataset1 vs. the dataset2
                                (i.e., d2 = a + b*d1 ==> this is 'b')
                   * L1slope  = slope of least-absolute-sum (L1) linear regression
                                of the data from dataset1 vs. the dataset2
                   * num      = number of the values in the region:
                                with the use of -mask or -automask,
                                the size of the region around any given
                                voxel will vary; this option lets you
                                map that size.
                   * ALL      = all of the above, in that order
                   More than one '-stat' option can be used.
    
     -mask mset  = Read in dataset 'mset' and use the nonzero voxels
                   therein as a mask.  Voxels NOT in the mask will
                   not be used in the neighborhood of any voxel. Also,
                   a voxel NOT in the mask will have its statistic(s)
                   computed as zero (0).
     -automask   = Compute the mask as in program 3dAutomask.
                   -mask and -automask are mutually exclusive: that is,
                   you can only specify one mask.
     -weight ws  = Use dataset 'ws' as a weight.  Only applies to 'pearson'.
    
     -prefix ppp = Use string 'ppp' as the prefix for the output dataset.
                   The output dataset is always stored as floats.
    
    ADVANCED OPTIONS
    ----------------
     -histpow pp   = By default, the number of bins in the histogram used
                     for calculating the Hellinger, Mutual Information,
                     and Correlation Ratio statistics is n^(1/3), where n
                     is the number of data points in the -nbhd mask.  You
                     can change that exponent to 'pp' with this option.
     -histbin nn   = Or you can just set the number of bins directly to 'nn'.
     -hclip1 a b   = Clip dataset1 to lie between values 'a' and 'b'.  If 'a'
                     and 'b' end in '%', then these values are percentage
                     points on the cumulative histogram.
     -hclip2 a b   = Similar to '-hclip1' for dataset2.
    
    -----------------------------
    Author: RWCox - October 2006.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
