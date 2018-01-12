.. contents:: 
    :depth: 4 

***********
3dToutcount
***********

.. code-block:: none

    Usage: 3dToutcount [options] dataset
    Calculates number of 'outliers' a 3D+time dataset, at each
    time point, and writes the results to stdout.
    
    Options:
     -mask mset = Only count voxels in the mask dataset.
     -qthr q    = Use 'q' instead of 0.001 in the calculation
                    of alpha (below): 0 < q < 1.
    
     -autoclip }= Clip off 'small' voxels (as in 3dClipLevel);
     -automask }=   you can't use this with -mask!
    
     -fraction  = Output the fraction of (masked) voxels which are
                  outliers at each time point, instead of the count.
    
     -range     = Print out median+3.5*MAD of outlier count with
                    each time point; use with 1dplot as in
                    3dToutcount -range fred+orig | 1dplot -stdin -one
     -save ppp  = Make a new dataset, and save the outlier Q in each
                    voxel, where Q is calculated from voxel value v by
                    Q = -log10(qg(abs((v-median)/(sqrt(PI/2)*MAD))))
                 or Q = 0 if v is 'close' to the median (not an outlier).
                    That is, 10**(-Q) is roughly the p-value of value v
                    under the hypothesis that the v's are iid normal.
                  The prefix of the new dataset (float format) is 'ppp'.
    
     -polort nn = Detrend each voxel time series with polynomials of
                    order 'nn' prior to outlier estimation.  Default
                    value of nn=0, which means just remove the median.
                    Detrending is done with L1 regression, not L2.
    
     -legendre  = Use Legendre polynomials (also allows -polort > 3).
    
    OUTLIERS are defined as follows:
     * The trend and MAD of each time series are calculated.
       - MAD = median absolute deviation
             = median absolute value of time series minus trend.
     * In each time series, points that are 'far away' from the
        trend are called outliers, where 'far' is defined by
          alpha * sqrt(PI/2) * MAD
          alpha = qginv(0.001/N) (inverse of reversed Gaussian CDF)
          N     = length of time series
     * Some outliers are to be expected, but if a large fraction of the
        voxels in a volume are called outliers, you should investigate
        the dataset more fully.
    
    Since the results are written to stdout, you probably want to redirect
    them to a file or another program, as in this example:
      3dToutcount -automask v1+orig | 1dplot -stdin
    
    NOTE: also see program 3dTqual for a similar quality check.
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
