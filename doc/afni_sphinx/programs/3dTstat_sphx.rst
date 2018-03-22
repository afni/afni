*******
3dTstat
*******

.. _ahelp_3dTstat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTstat [options] dataset
    Computes one or more voxel-wise statistics for a 3D+time dataset
    and stores them in a bucket dataset.  If no statistic option is
    given, computes just the mean of each voxel time series.
    Multiple statistics options may be given, and will result in
    a multi-volume dataset.
    
    Statistics Options:
     -mean   = compute mean of input voxels
     -sum    = compute sum of input voxels
     -abssum = compute absolute sum of input voxels
     -slope  = compute the slope of input voxels vs. time
     -sos    = compute sum of squares
     -stdev  = compute standard deviation of input voxels
                 [N.B.: this is computed after    ]
                 [      the slope has been removed]
     -l2norm = compute L2 norm (sqrt(sum squares))
     -cvar   = compute coefficient of variation of input
                 voxels = stdev/fabs(mean)
     -cvarinv= 1.0/cvar = 'signal to noise ratio' [for Vinai]
               **N.B.: You can add NOD to the end of the above 3
                       options only, to turn off detrending, as in
                         -stdevNOD  and/or  -cvarNOD  and/or  -cvarinvNOD
    
     -tsnr      = compute temporal signal to noise ratio
                    fabs(mean)/stdev NOT DETRENDED (same as -cvarinvNOD)
     -MAD       = compute MAD (median absolute deviation) of
                    input voxels = median(|voxel-median(voxel)|)
                    [N.B.: the trend is NOT removed for this]
     -DW        = compute Durbin-Watson Statistic of input voxels
                    [N.B.: the trend IS removed for this]
     -median    = compute median of input voxels  [undetrended]
     -nzmedian  = compute median of non-zero input voxels [undetrended]
     -nzstdev   = standard deviation of non-zero input voxel [undetrended]
     -bmv       = compute biweight midvariance of input voxels [undetrended]
                    [actually is 0.989*sqrt(biweight midvariance), to make]
                    [the value comparable to the standard deviation output]
     -min       = compute minimum of input voxels [undetrended]
     -max       = compute maximum of input voxels [undetrended]
     -absmax    = compute absolute maximum of input voxels [undetrended]
     -signed_absmax = (signed) value with absolute maximum [undetrended]
     -percentile P  = the P-th percentile point (0=min, 50=median, 100=max)
                      of the data in each voxel time series.
                      [this option can only be used once!]
     -argmin    = index of minimum of input voxels [undetrended]
     -argmin1   = index + 1 of minimum of input voxels [undetrended]
     -argmax    = index of maximum of input voxels [undetrended]
     -argmax1   = index + 1 of maximum of input voxels [undetrended]
     -argabsmax = index of absolute maximum of input voxels [undetrended]
     -argabsmax1= index +1 of absolute maximum of input voxels [undetrended]
     -duration  = compute number of points around max above a threshold
                  Use basepercent option to set limits
     -onset     = beginning of duration around max where value
                  exceeds basepercent
     -offset    = end of duration around max where value
                  exceeds basepercent
     -centroid  = compute centroid of data time curves
                  (sum(i*f(i)) / sum(f(i)))
     -centduration = compute duration using centroid's index as center
     -nzmean    = compute mean of non-zero voxels
     -zcount    = count number of zero values at each voxel
     -nzcount    = count number of non zero values at each voxel
    
     -autocorr n = compute autocorrelation function and return
                   first n coefficients
     -autoreg n = compute autoregression coefficients and return
                   first n coefficients
       [N.B.: -autocorr 0 and/or -autoreg 0 will return number
              coefficients equal to the length of the input data]
    
     -accumulate = accumulate time series values (partial sums)
                   val[i] = sum old_val[t] over t = 0..i
                   (output length = input length)
    
     -centromean = compute mean of middle 50% of voxel values [undetrended]
    
     -firstvalue = first value in dataset - typically just placeholder
    
     ** If no statistic option is given, then '-mean' is assumed **
    
    Other Options:
     -tdiff    = Means to take the first difference of each time
                   series before further processing.
     -prefix p = Use string 'p' for the prefix of the
                   output dataset [DEFAULT = 'stat']
     -datum d  = use data type 'd' for the type of storage
                   of the output, where 'd' is one of
                   'byte', 'short', or 'float' [DEFAULT=float]
     -nscale = Do not scale output values when datum is byte or short.
               Scaling is done by default.
    
     -basepercent nn = Percentage of maximum for duration calculation
    
     -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be printed from 'dataset'.  Note
                     that the mask dataset and the input dataset
                     must have the same number of voxels.
    
     -mrange a b  Means to further restrict the voxels from
                     'mset' so that only those mask values
                     between 'a' and 'b' (inclusive) will
                     be used.  If this option is not given,
                     all nonzero values from 'mset' are used.
                     Note that if a voxel is zero in 'mset', then
                     it won't be included, even if a < 0 < b.
    
     -cmask 'opts' Means to execute the options enclosed in single
                      quotes as a 3dcalc-like program, and produce
                      produce a mask from the resulting 3D brick.
           Examples:
            -cmask '-a fred+orig[7] -b zork+orig[3] -expr step(a-b)'
                      produces a mask that is nonzero only where
                      the 7th sub-brick of fred+orig is larger than
                      the 3rd sub-brick of zork+orig.
            -cmask '-a fred+orig -expr 1-bool(k-7)'
                      produces a mask that is nonzero only in the
                      7th slice (k=7); combined with -mask, you
                      could use this to extract just selected voxels
                      from particular slice(s).
           Notes: * You can use both -mask and -cmask in the same
                      run - in this case, only voxels present in
                      both masks will be dumped.
                  * Only single sub-brick calculations can be
                      used in the 3dcalc-like calculations -
                      if you input a multi-brick dataset here,
                      without using a sub-brick index, then only
                      its 0th sub-brick will be used.
                  * Do not use quotes inside the 'opts' string!
    
    
    If you want statistics on a detrended dataset and the option
    doesn't allow that, you can use program 3dDetrend first.
    
    The output is a bucket dataset.  The input dataset may
    use a sub-brick selection list, as in program 3dcalc.
    
    *** If you are trying to compute the mean or std.dev. of multiple
    datasets (not across time), use 3dMean or 3dmerge instead.
    
    ----------------- Processing 1D files with 3dTstat -----------------
    To analyze a 1D file and get statistics on each of its columns,
    you can do something like this:
      3dTstat -stdev -bmv -prefix stdout: file.1D\'
    where the \' means to transpose the file on input, since 1D files
    read into 3dXXX programs are interpreted as having the time direction
    along the rows rather than down the columns.  In this example, the
    output is written to the screen, which could be captured with '>'
    redirection.  Note that if you don't give the '-prefix stdout:'
    option, then the output will be written into a NIML-formatted 1D
    dataset, which you might find slightly confusing (but still usable).
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
