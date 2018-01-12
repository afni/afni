.. contents:: 
    :depth: 4 

*******
3dmerge
*******

.. code-block:: none

    Program 3dmerge 
    This program has 2 different functions:
     (1) To edit 3D datasets in various ways (threshold, blur, cluster, ...);
     (2) To merge multiple datasets in various ways (average, max, ...).
    Either or both of these can be applied.
    
    The 'editing' operations are controlled by options that start with '-1',
    which indicates that they apply to individual datasets
    (e.g., '-1blur_fwhm').
    
    The 'merging' operations are controlled by options that start with '-g',
    which indicate that they apply to the entire group of input datasets
    (e.g., '-gmax').
    
    ----------------------------------------------------------------------
    Usage: 3dmerge [options] datasets ...
    
    Examples:
    
      1. Apply a 4.0mm FWHM Gaussian blur to EPI run 7.
    
           3dmerge -1blur_fwhm 4.0 -doall -prefix e1.run7_blur run7+orig
    
    * These examples are based on a data grid of 3.75 x 3.75 x 3.5, in mm.
      So a single voxel has a volume of ~49.22 mm^3 (mvul), and a 40 voxel
      cluster has a volume of ~1969 mm^3 (as used in some examples).
    
      2. F-stat only:
    
         Cluster based on a threshold of F=10 (F-stats are in sub-brick #0),
         and require a volume of 40 voxels (1969 mm^3).  The output will be
         the same F-stats as in the input, but subject to the threshold and
         clustering.
    
           3dmerge -1clust 3.76 1969 -1thresh 10.0    \
                   -prefix e2.f10 stats+orig'[0]'
    
      3. F-stat only:
    
         Perform the same clustering (as in #2), but apply the radius and
         cluster size in terms of cubic millimeter voxels (as if the voxels
         were 1x1x1).  So add '-dxyz=1', and adjust rmm and mvul.
    
           3dmerge -dxyz=1 -1clust 1 40 -1thresh 10.0    \
                   -prefix e3.f10 stats+orig'[0]'
    
      4. t-stat and beta weight:
    
         For some condition, our beta weight is in sub-brick #4, with the
         corresponding t-stat in sub-brick #5.  Cluster based on 40 voxels
         and a t-stat threshold of 3.25.  Output the data from the beta
         weights, not the t-stats.
    
           3dmerge -dxyz=1 -1clust 1 40 -1thresh 3.25    \
                   -1tindex 5 -1dindex 4                 \
                   -prefix e4.t3.25 stats+orig
    
      5. t-stat mask:
    
         Apply the same threshold and cluster as in #4, but output a mask.
         Since there are 5 clusters found in this example, the values in
         the mask will be from 1 to 5, representing the largest cluster to
         the smallest.  Use -1clust_order on sub-brick 5.
    
           3dmerge -dxyz=1 -1clust_order 1 40 -1thresh 3.25    \
                   -prefix e5.mask5 stats+orig'[5]'
    
         Note: this should match the 3dclust output from:
    
           3dclust -1thresh 3.25 -dxyz=1 1 40 stats+orig'[5]'
    
    ----------------------------------------------------------------------
    EDITING OPTIONS APPLIED TO EACH INPUT DATASET:
      -1thtoin         = Copy threshold data over intensity data.
                           This is only valid for datasets with some
                           thresholding statistic attached.  All
                           subsequent operations apply to this
                           substituted data.
      -2thtoin         = The same as -1thtoin, but do NOT scale the
                           threshold values from shorts to floats when
                           processing.  This option is only provided
                           for compatibility with the earlier versions
                           of the AFNI package '3d*' programs.
      -1noneg          = Zero out voxels with negative intensities
      -1abs            = Take absolute values of intensities
      -1clip val       = Clip intensities in range (-val,val) to zero
      -2clip v1 v2     = Clip intensities in range (v1,v2) to zero
      -1uclip val      = These options are like the above, but do not apply
      -2uclip v1 v2        any automatic scaling factor that may be attached
                           to the data.  These are for use only in special
                           circumstances.  (The 'u' means 'unscaled'.  Program
                           '3dinfo' can be used to find the scaling factors.)
                   N.B.: Only one of these 'clip' options can be used; you cannot
                           combine them to have multiple clipping executed.
      -1thresh thr     = Use the threshold data to censor the intensities
                           (only valid for 'fith', 'fico', or 'fitt' datasets)
                           (or if the threshold sub-brick is set via -1tindex)
                   N.B.: The value 'thr' is floating point, in the range
                               0.0 < thr < 1.0  for 'fith' and 'fico' datasets,
                           and 0.0 < thr < 32.7 for 'fitt' datasets.
      -2thresh t1 t2   = Zero out voxels where the threshold sub-brick value
                           lies between 't1' and 't2' (exclusive).  If t1=-t2,
                           is the same as '-1thresh t2'.
      -1blur_sigma bmm = Gaussian blur with sigma = bmm (in mm)
      -1blur_rms bmm   = Gaussian blur with rms deviation = bmm
      -1blur_fwhm bmm  = Gaussian blur with FWHM = bmm
      -t1blur_sigma bmm= Gaussian blur of threshold with sigma = bmm(in mm)
      -t1blur_rms bmm  = Gaussian blur of threshold with rms deviation = bmm
      -t1blur_fwhm bmm = Gaussian blur of threshold with FWHM = bmm
      -1zvol x1 x2 y1 y2 z1 z2
                       = Zero out entries inside the 3D volume defined
                           by x1 <= x <= x2, y1 <= y <= y2, z1 <= z <= z2 ;
                   N.B.: The ranges of x,y,z in a dataset can be found
                           using the '3dinfo' program. Dimensions are in mm.
                   N.B.: This option may not work correctly at this time, but
                           I've not figured out why!
    
     CLUSTERING
      -dxyz=1  = In the cluster editing options, the spatial clusters
                 are defined by connectivity in true 3D distance, using
                 the voxel dimensions recorded in the dataset header.
                 This option forces the cluster editing to behave as if
                 all 3 voxel dimensions were set to 1 mm.  In this case,
                 'rmm' is then the max number of grid cells apart voxels
                 can be to be considered directly connected, and 'vmul'
                 is the min number of voxels to keep in the cluster.
           N.B.: The '=1' is part of the option string, and can't be
                 replaced by some other value.  If you MUST have some
                 other value for voxel dimensions, use program 3drefit.
     
      The following cluster options are mutually exclusive: 
      -1clust rmm vmul = Form clusters with connection distance rmm
                           and clip off data not in clusters of
                           volume at least vmul microliters
      -1clust_mean rmm vmul = Same as -1clust, but all voxel intensities 
                                within a cluster are replaced by the average
                                intensity of the cluster. 
      -1clust_max rmm vmul  = Same as -1clust, but all voxel intensities 
                                within a cluster are replaced by the maximum
                                intensity of the cluster. 
      -1clust_amax rmm vmul = Same as -1clust, but all voxel intensities 
                                within a cluster are replaced by the maximum
                                absolute intensity of the cluster. 
      -1clust_smax rmm vmul = Same as -1clust, but all voxel intensities 
                                within a cluster are replaced by the maximum
                                signed intensity of the cluster. 
      -1clust_size rmm vmul = Same as -1clust, but all voxel intensities 
                                within a cluster are replaced by the size 
                                of the cluster (in multiples of vmul).   
      -1clust_order rmm vmul= Same as -1clust, but all voxel intensities 
                                within a cluster are replaced by the cluster
                                size index (largest cluster=1, next=2, ...).
      -1clust_depth rmm vmul= Same as -1clust, but all voxel intensities 
                             are replaced by the number of peeling operations
                             needed to remove them from the cluster.
                             That number is an indication of how deep a voxel
                             is inside a cluster
      -isovalue   = Clusters will be formed only from contiguous (in the   
                     rmm sense) voxels that also have the same value.       
                                                                            
               N.B.:  The normal method is to cluster all contiguous        
                      nonzero voxels together.                              
                                                                            
      -isomerge   = Clusters will be formed from each distinct value       
                     in the dataset; spatial contiguity will not be         
                     used (but you still have to supply rmm and vmul        
                     on the command line).                                  
                                                                            
               N.B.:  'Clusters' formed this way may well have components   
                       that are widely separated!                           
                                                                            
     * If rmm is given as 0, this means to use the 6 nearest neighbors to
         form clusters of nonzero voxels.
     * If vmul is given as zero, then all cluster sizes will be accepted
         (probably not very useful!).
     * If vmul is given as negative, then abs(vmul) is the minimum number
         of voxels to keep.
     
      The following commands produce erosion and dilation of 3D clusters.  
      These commands assume that one of the -1clust commands has been used.
      The purpose is to avoid forming strange clusters with 2 (or more)    
      main bodies connected by thin 'necks'.  Erosion can cut off the neck.
      Dilation will minimize erosion of the main bodies.                   
      Note:  Manipulation of values inside a cluster (-1clust commands)    
             occurs AFTER the following two commands have been executed.   
      -1erode pv    For each voxel, set the intensity to zero unless pv %  
                    of the voxels within radius rmm are nonzero.           
      -1dilate      Restore voxels that were removed by the previous       
                    command if there remains a nonzero voxel within rmm.   
     
      The following filter options are mutually exclusive: 
      -1filter_mean rmm   = Set each voxel to the average intensity of the 
                              voxels within a radius of rmm. 
      -1filter_nzmean rmm = Set each voxel to the average intensity of the 
                              non-zero voxels within a radius of rmm. 
      -1filter_max rmm    = Set each voxel to the maximum intensity of the 
                              voxels within a radius of rmm. 
      -1filter_amax rmm   = Set each voxel to the maximum absolute intensity
                              of the voxels within a radius of rmm. 
      -1filter_smax rmm   = Set each voxel to the maximum signed intensity 
                              of the voxels within a radius of rmm. 
      -1filter_aver rmm   = Same idea as '_mean', but implemented using a
                              new code that should be faster.
     
      The following threshold filter options are mutually exclusive: 
      -t1filter_mean rmm   = Set each correlation or threshold voxel to the 
                              average of the voxels within a radius of rmm. 
      -t1filter_nzmean rmm = Set each correlation or threshold voxel to the 
                              average of the non-zero voxels within 
                              a radius of rmm. 
      -t1filter_max rmm    = Set each correlation or threshold voxel to the 
                              maximum of the voxels within a radius of rmm. 
      -t1filter_amax rmm   = Set each correlation or threshold voxel to the 
                              maximum absolute intensity of the voxels 
                              within a radius of rmm. 
      -t1filter_smax rmm   = Set each correlation or threshold voxel to the 
                              maximum signed intensity of the voxels 
                              within a radius of rmm. 
      -t1filter_aver rmm   = Same idea as '_mean', but implemented using a
                              new code that should be faster.
     
      -1mult factor    = Multiply intensities by the given factor
      -1zscore         = If the sub-brick is labeled as a statistic from
                         a known distribution, it will be converted to
                         an equivalent N(0,1) deviate (or 'z score').
                         If the sub-brick is not so labeled, nothing will
                         be done.
    
    The above '-1' options are carried out in the order given above,
    regardless of the order in which they are entered on the command line.
    
    N.B.: The 3 '-1blur' options just provide different ways of
          specifying the radius used for the blurring function.
          The relationships among these specifications are
             sigma = 0.57735027 * rms = 0.42466090 * fwhm
          The requisite convolutions are done using FFTs; this is by
          far the slowest operation among the editing options.
    
    OTHER OPTIONS:
      -nozero     = Do NOT write the output dataset if it would be all zero.
    
      -datum type = Coerce the output data to be stored as the given type,
                      which may be byte, short, or float.
              N.B.: Byte data cannot be negative.  If this datum type is chosen,
                      any negative values in the edited and/or merged dataset
                      will be set to zero.
    
      -keepthr    = When using 3dmerge to edit exactly one dataset of a
                      functional type with a threshold statistic attached,
                      normally the resulting dataset is of the 'fim'
                      (intensity only) type.  This option tells 3dmerge to
                      copy the threshold data (unedited in any way) into
                      the output dataset.
              N.B.: This option is ignored if 3dmerge is being used to
                      combine 2 or more datasets.
              N.B.: The -datum option has no effect on the storage of the
                      threshold data.  Instead use '-thdatum type'.
    
      -doall      = Apply editing and merging options to ALL sub-bricks 
                      uniformly in a dataset.
              N.B.: All input datasets must have the same number of sub-bricks
                      when using the -doall option. 
              N.B.: The threshold specific options (such as -1thresh, 
                      -keepthr, -tgfisher, etc.) are not compatible with 
                      the -doall command.  Neither are the -1dindex or
                      the -1tindex options.
              N.B.: All labels and statistical parameters for individual 
                      sub-bricks are copied from the first dataset.  It is 
                      the responsibility of the user to verify that these 
                      are appropriate.  Note that sub-brick auxiliary data 
                      can be modified using program 3drefit. 
    
      -quiet      = Reduce the number of messages shown
    
      -1dindex j  = Uses sub-brick #j as the data source , and uses sub-brick
      -1tindex k  = #k as the threshold source.  With these, you can operate
                      on any given sub-brick of the inputs dataset(s) to produce
                      as output a 1 brick dataset.  If desired, a collection
                      of 1 brick datasets can later be assembled into a
                      multi-brick bucket dataset using program '3dbucket'
                      or into a 3D+time dataset using program '3dTcat'.
              N.B.: If these options aren't used, j=0 and k=1 are the defaults
    
      The following option allows you to specify a mask dataset that
      limits the action of the 'filter' options to voxels that are
      nonzero in the mask:
    
      -1fmask mset = Read dataset 'mset' (which can include a
                      sub-brick specifier) and use the nonzero
                      voxels as a mask for the filter options.
                      Filtering calculations will not use voxels
                      that are outside the mask.  If an output
                      voxel does not have ANY masked voxels inside
                      the rmm radius, then that output voxel will
                      be set to 0.
             N.B.: * Only the -1filter_* and -t1filter_* options are
                     affected by -1fmask.
                   * Voxels NOT in the fmask will be set to zero in the
                     output when the filtering occurs.  THIS IS NEW BEHAVIOR,
                     as of 11 Oct 2007.  Previously, voxels not in the fmask,
                     but within 'rmm' of a voxel in the mask, would get a
                     nonzero output value, as those nearby voxels would be
                     combined (via whatever '-1f...' option was given).
                   * If you wish to restore this old behavior, where non-fmask
                     voxels can get nonzero output, then use the new option
                     '-1fm_noclip' in addition to '-1fmask'. The two comments
                     below apply to the case where '-1fm_noclip' is given!
                     * In the linear averaging filters (_mean, _nzmean,
                       and _expr), voxels not in the mask will not be used
                       or counted in either the numerator or denominator.
                       This can give unexpected results if you use '-1fm_noclip'.
                       For example, if the mask is designed to exclude the volume
                       outside the brain, then voxels exterior to the brain,
                       but within 'rmm', will have a few voxels inside the brain
                       included in the filtering.  Since the sum of weights (the
                       denominator) is only over those few intra-brain
                       voxels, the effect will be to extend the significant
                       part of the result outward by rmm from the surface
                       of the brain.  In contrast, without the mask, the
                       many small-valued voxels outside the brain would
                       be included in the numerator and denominator sums,
                       which would barely change the numerator (since the
                       voxel values are small outside the brain), but would
                       increase the denominator greatly (by including many
                       more weights).  The effect in this case (no -1fmask)
                       is to make the filtering taper off gradually in the
                       rmm-thickness shell around the brain.
                     * Thus, if the -1fmask is intended to clip off non-brain
                       data from the filtering, its use should be followed by
                       masking operation using 3dcalc:
       3dmerge -1filter_aver 12 -1fm_noclip -1fmask mask+orig -prefix x input+orig
       3dcalc  -a x -b mask+orig -prefix y -expr 'a*step(b)'
       rm -f x+orig.*
                     The desired result is y+orig - filtered using only
                     brain voxels (as defined by mask+orig), and with
                     the output confined to the brain voxels as well.
    
      The following option allows you to specify an almost arbitrary
      weighting function for 3D linear filtering:
    
      -1filter_expr rmm expr
         Defines a linear filter about each voxel of radius 'rmm' mm.
         The filter weights are proportional to the expression evaluated
         at each voxel offset in the rmm neighborhood.  You can use only
         these symbols in the expression:
             r = radius from center
             x = dataset x-axis offset from center
             y = dataset y-axis offset from center
             z = dataset z-axis offset from center
             i = x-axis index offset from center
             j = y-axis index offset from center
             k = z-axis index offset from center
         Example:
           -1filter_expr 12.0 'exp(-r*r/36.067)'
         This does a Gaussian filter over a radius of 12 mm.  In this
         example, the FWHM of the filter is 10 mm. [in general, the
         denominator in the exponent would be 0.36067 * FWHM * FWHM.
         This is one way to get a Gaussian blur combined with the
         -1fmask option.  The radius rmm=12 is chosen where the weights
         get smallish.]  Another example:
           -1filter_expr 20.0 'exp(-(x*x+16*y*y+z*z)/36.067)'
         which is a non-spherical Gaussian filter.
    
      ** For shorthand, you can also use the new option (11 Oct 2007)
      -1filter_blur fwhm
            which is equivalent to
      -1filter_expr 1.3*fwhm 'exp(-r*r/(.36067*fwhm*fwhm)'
            and will implement a Gaussian blur.  The only reason to do
            Gaussian blurring this way is if you also want to use -1fmask!
    
      The following option lets you apply a 'Winsor' filter to the data:
    
      -1filter_winsor rmm nw
         The data values within the radius rmm of each voxel are sorted.
         Suppose there are 'N' voxels in this group.  We index the
         sorted voxels as s[0] <= s[1] <= ... <= s[N-1], and we call the
         value of the central voxel 'v' (which is also in array s[]).
                     If v < s[nw]    , then v is replaced by s[nw]
           otherwise If v > s[N-1-nw], then v is replace by s[N-1-nw]
           otherwise v is unchanged
         The effect is to increase 'too small' values up to some
         middling range, and to decrease 'too large' values.
         If N is odd, and nw=(N-1)/2, this would be a median filter.
         In practice, I recommend that nw be about N/4; for example,
           -dxyz=1 -1filter_winsor 2.5 19
         is a filter with N=81 that gives nice results.
       N.B.: This option is NOT affected by -1fmask
       N.B.: This option is slow! and experimental.
    
      The following option returns a rank value at each voxel in 
      the input dataset.
      -1rank 
         If the input voxels were, say, 12  45  9  0  9  12  0
         the output would be             2   3  1  0  1   2  0
         This option is handy for turning FreeSurfer's segmentation
         volumes to ROI volumes that can be easily colorized with AFNI.
         For example:
         3dmerge -1rank -prefix aparc+aseg_rank aparc+aseg.nii 
         To view aparc+aseg_rank+orig, use the ROI_128 colormap
         and set the colorbar range to 128.
         The -1rank option also outputs a 1D file that contains 
         the mapping from the input dataset to the ranked output.
         Sub-brick float factors are ignored.
    
         This option only works on datasets of integral values or 
         of integral data types. 'float' values are typecast to 'int' 
         before being ranked.
    
         See also program 3dRank
    
    MERGING OPTIONS APPLIED TO FORM THE OUTPUT DATASET:
     [That is, different ways to combine results. The]
     [following '-g' options are mutually exclusive! ]
      -gmean     = Combine datasets by averaging intensities
                     (including zeros) -- this is the default
      -gnzmean   = Combine datasets by averaging intensities
                     (not counting zeros)
      -gmax      = Combine datasets by taking max intensity
                     (e.g., -7 and 2 combine to 2)
      -gamax     = Combine datasets by taking max absolute intensity
                     (e.g., -7 and 2 combine to 7)
      -gsmax     = Combine datasets by taking max signed intensity
                     (e.g., -7 and 2 combine to -7)
      -gcount    = Combine datasets by counting number of 'hits' in
                      each voxel (see below for defintion of 'hit')
      -gorder    = Combine datasets in order of input:
                    * If a voxel is nonzero in dataset #1, then
                        that value goes into the voxel.
                    * If a voxel is zero in dataset #1 but nonzero
                        in dataset #2, then the value from #2 is used.
                    * And so forth: the first dataset with a nonzero
                        entry in a given voxel 'wins'
      -gfisher   = Takes the arctanh of each input, averages these,
                      and outputs the tanh of the average.  If the input
                      datum is 'short', then input values are scaled by
                      0.0001 and output values by 10000.  This option
                      is for merging bricks of correlation coefficients.
    
      -nscale    = If the output datum is shorts, don't do the scaling
                      to the max range [similar to 3dcalc's -nscale option]
    
    MERGING OPERATIONS APPLIED TO THE THRESHOLD DATA:
     [That is, different ways to combine the thresholds.  If none of these ]
     [are given, the thresholds will not be merged and the output dataset  ]
     [will not have threshold data attached.  Note that the following '-tg']
     [command line options are mutually exclusive, but are independent of  ]
     [the '-g' options given above for merging the intensity data values.  ]
      -tgfisher  = This option is only applicable if each input dataset
                      is of the 'fico' or 'fith' types -- functional
                      intensity plus correlation or plus threshold.
                      (In the latter case, the threshold values are
                      interpreted as correlation coefficients.)
                      The correlation coefficients are averaged as
                      described by -gfisher above, and the output
                      dataset will be of the fico type if all inputs
                      are fico type; otherwise, the output datasets
                      will be of the fith type.
             N.B.: The difference between the -tgfisher and -gfisher
                      methods is that -tgfisher applies to the threshold
                      data stored with a dataset, while -gfisher
                      applies to the intensity data.  Thus, -gfisher
                      would normally be applied to a dataset created
                      from correlation coefficients directly, or from
                      the application of the -1thtoin option to a fico
                      or fith dataset.
    
    OPTIONAL WAYS TO POSTPROCESS THE COMBINED RESULTS:
     [May be combined with the above methods.]
     [Any combination of these options may be used.]
      -ghits count     = Delete voxels that aren't !=0 in at least
                           count datasets (!=0 is a 'hit')
      -gclust rmm vmul = Form clusters with connection distance rmm
                           and clip off data not in clusters of
                           volume at least vmul microliters
    
    The '-g' and '-tg' options apply to the entire group of input datasets.
    
    OPTIONS THAT CONTROL THE NAMES OF THE OUTPUT DATASET:
      -session dirname  = write output into given directory (default=./)
      -prefix  pname    = use 'pname' for the output dataset prefix
                           (default=mrg)
    
    NOTES:
     **  If only one dataset is read into this program, then the '-g'
           options do not apply, and the output dataset is simply the
           '-1' options applied to the input dataset (i.e., edited).
     **  A merged output dataset is ALWAYS of the intensity-only variety.
     **  You can combine the outputs of 3dmerge with other sub-bricks
           using the program 3dbucket.
     **  Complex-valued datasets cannot be merged.
     **  This program cannot handle time-dependent datasets without -doall.
     **  Note that the input datasets are specified by their .HEAD files,
           but that their .BRIK files must exist also!
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
     ** Input datasets using sub-brick selectors are treated as follows:
          - 3D+time if the dataset is 3D+time and more than 1 brick is chosen
          - otherwise, as bucket datasets (-abuc or -fbuc)
           (in particular, fico, fitt, etc. datasets are converted to fbuc)
     ** If you are NOT using -doall, and choose more than one sub-brick
         with the selector, then you may need to use -1dindex to further
         pick out the sub-brick on which to operate (why you would do this
         I cannot fathom).  If you are also using a thresholding operation
         (e.g., -1thresh), then you also MUST use -1tindex to choose which
         sub-brick counts as the 'threshold' value.  When used with sub-brick
         selection, 'index' refers the dataset AFTER it has been read in:
              -1dindex 1 -1tindex 3 'dset+orig[4..7]'
         means to use the #5 sub-brick of dset+orig as the data for merging
         and the #7 sub-brick of dset+orig as the threshold values.
     ** The above example would better be done with
              -1tindex 1 'dset+orig[5,7]'
         since the default data index is 0. (You would only use -1tindex if
         you are actually using a thresholding operation.)
     ** -1dindex and -1tindex apply to all input datasets.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
