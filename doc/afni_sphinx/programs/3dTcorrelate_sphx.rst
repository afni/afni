************
3dTcorrelate
************

.. _ahelp_3dTcorrelate:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTcorrelate [options] xset yset
    Computes the correlation coefficient between corresponding voxel
    time series in two input 3D+time datasets 'xset' and 'yset', and
    stores the output in a new 1 sub-brick dataset.
    
    Options:
      -pearson    = Correlation is the normal Pearson (product moment)
                    correlation coefficient [this is the default method].
      -spearman   = Correlation is the Spearman (rank) correlation
                    coefficient.
      -quadrant   = Correlation is the quadrant correlation coefficient.
      -ktaub      = Correlation is Kendall's tau_b coefficient.
                    ++ For 'continuous' or finely-discretized data, tau_b
                       and rank correlation are nearly equivalent.
      -covariance = Covariance instead of correlation. That would be 
                    the Pearson correlation without scaling by the product
                    of the standard deviations.
      -ycoef      = Least squares coefficient that best fits y(t) to x(t),
                    after detrending.  That is, if yd(t) is the detrended
                    y(t) and xd(t) is the detrended x(t), then the ycoef
                    value is from the OLSQ fit to xd(t) = ycoef * y(t) + error.
    
      -Fisher     = Apply the 'Fisher' (inverse hyperbolic tangent) transformation
                    to (correlation) results.
                    ++ It does not make sense to use this with '-ktaub', but if
                        you want to do it, the program will not stop you.
                    ++ This option does not apply to '-covariance' or '-ycoef'.
    
      -polort m = Remove polynomical trend of order 'm', for m=-1..9.
                    [default is m=1; removal is by least squares].
                    Using m=-1 means no detrending; this is only useful
                    for data/information that has been pre-processed.
    
      -ort r.1D = Also detrend using the columns of the 1D file 'r.1D'.
                    Only one -ort option can be given.  If you want to use
                    more than one, create a temporary file using 1dcat.
    
      -autoclip = Clip off low-intensity regions in the two datasets,
      -automask =  so that the correlation is only computed between
                   high-intensity (presumably brain) voxels.  The
                   intensity level is determined the same way that
                   3dClipLevel works.
    
      -prefix p = Save output into dataset with prefix 'p'
                   [default prefix is 'Tcorr'].
    
    Notes:
    * The output dataset is functional bucket type, with just one
       sub-brick, stored in floating point format.
    * Because both time series are detrended prior to correlation,
       the results will not be identical to using FIM or FIM+ to
       calculate correlations (whose ideal vector is not detrended).
    * Also see 3dTcorr1D if you want to correlate each voxel time series
       in a dataset xset with a single 1D time series file.
    * http://en.wikipedia.org/wiki/Correlation
    * http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
    * http://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
    * http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient
    
    -- RWCox - Aug 2001
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
