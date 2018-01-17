*****
3dLSS
*****

.. _3dLSS:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dLSS [options]
    
     ** Least-Squares-Sum (LSS) estimation from a -stim_times_IM matrix, as      **
     *  described in the paper:                                                   *
     *    JA Mumford et al.  Deconvolving BOLD activation in event-related        *
     *    designs for multivoxel pattern classification analyses.                 *
     *    NeuroImage (2011) http://dx.doi.org/10.1016/j.neuroimage.2011.08.076    *
     *  LSS regression was first mentioned in this poster:                        *
     *    B Turner. A comparison of methods for the use of pattern classification *
     *    on rapid event-related fMRI data. Annual Meeting of the Society for     *
     **   Neuroscience, San Diego, CA (2010).                                    **
    
    ----------------------------------------
    Options (the first 'option' is mandatory)
    ----------------------------------------
     -matrix mmm = Read the matrix 'mmm', which should have been
                    output from 3dDeconvolve via the '-x1D' option, and
                    should have included exactly one '-stim_times_IM' option.
                 -->> The 3dLSS algorithm requires that at least 2 different
                      stimulus times be given in the -stim_times_IM option.
                      If you have only 1 stim time, this program will not run.
                      In such a case, the normal '-bucket' output from 3dDeconvolve
                      (or '-Rbuck' output from 3dREMLfit) will have the single
                      beta for the single stim time.
    
     -input ddd  = Read time series dataset 'ddd'
       ** OR **
     -nodata     = Just compute the estimator matrix -- to be saved with '-save1D'.
                   * The number of time points is taken from the matrix header.
                   * If neither '-input' nor '-nodata' is given, '-nodata' is used.
                   * If '-input' is used, the number of time points in the dataset
                     must match the number of time points in the matrix.
    
     -mask MMM   = Read dataset 'MMM' as a mask for the input; voxels outside
                    the mask will not be fit by the regression model.
     -automask   = If you don't know what this does by now, please don't use
                    this program.
                   * Neither of these options has any meaning for '-nodata'.
                   * If '-input' is used and neither of these options is given,
                     then all voxels will be processed.
    
     -prefix ppp = Prefix name for the output dataset;
                    this dataset will contain ONLY the LSS estimates of the
                    beta weights for the '-stim_times_IM' stimuli.
                   * If you don't use '-prefix', then the prefix is 'LSSout'.
    
     -save1D qqq = Save the estimator vectors (cf. infra) to a 1D formatted
                    file named 'qqq'.  Each column of this file will be
                    one estimator vector, the same length as the input
                    dataset timeseries (after censoring, if any).
                   * The j-th LSS beta estimate is the dot product of the j-th
                     column of this file with the data time series (duly censored).
                   * If you don't use '-save1D', then this file is not saved.
    
     -verb       = Write out progress reports, for fun fun fun in the sun sun sun.
    
    -------------------
    Method == EQUATIONS
    -------------------
     3dLSS is fast, since it uses a rank-1 bordering technique to pre-compute
     the estimator for each separate stimulus regressor from the fixed part of
     the matrix, then applies these estimators to each time series in the input
     dataset by a simple dot product.  If you wish to peruse the equations, see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/3dLSS/3dLSS_mathnotes.pdf 
     The estimator for each separate beta (as described at '-save1D') is the
     N-vector which, when dotted into the N-vector of a voxel's time series,
     gives the LSS beta estimate for that voxel.
    
    ---------------------
    Caveats == READ THIS!
    ---------------------
     The LSS method produces estimates that tend to have smaller variance than the
     LSA method that 3dDeconvolve would produce, but the LSS estimates have greater
     bias -- in principle, the LSA method is unbiased if the noise is symmetrically
     distributed.  For the purpose of using the beta estimates for MVPA (e.g., 3dsvm),
     the bias may not matter much and the variance reduction may help improve the
     classification, as illustrated in the Mumford paper.  For other purposes, the
     trade-off might well go the other way -- for ANY application of LSS vs. LSA,
     you need to assess the situation before deciding -- probably by the judicious
     use of simulation (as in the Mumford paper).
    
     The bias in the estimate of any given beta is essentially due to the fact
     that for any given beta, LSS doesn't use an estimator vector that is orthogonal
     to the regressors for other coefficients -- that is what LSA does, using the
     pseudo-inverse.  Typically, any given LSS-estimated beta will include a mixture
     of the betas from neighboring stimuli -- for example,
        beta8{LSS} = beta8{LSA} + 0.3*beta7{LSA} - 0.1*beta9{LSA} + smaller stuff
     where the weights of the neighbors are larger if the corresponding stimuli
     are closer (so the regressors overlap more).
    
     The LSS betas are NOT biased by including any betas that aren't from the
     -stim_times_IM regressors -- the LSS estimator vectors (what '-save1D' gives)
     are orthogonal to those 'nuisance' regression matrix columns.
    
     To investigate these weighting and orthogonality issues yourself, you can
     multiply the LSS estimator vectors into the 3dDeconvolve regression matrix
     and examine the result -- in the ideal world, the matrix would be all 0
     except for 1s on diagonal corresponding to the -stim_times_IM betas.  This
     calculation can be done in AFNI with commands something like the 'toy' example
     below, which has only 6 stimulus times:
    
      3dDeconvolve -nodata 50 1.0 -polort 1 -x1D R.xmat.1D -x1D_stop -num_stimts 1 \
                   -stim_times_IM 1 '1D: 12.7 16.6 20.1 26.9 30.5 36.5' 'BLOCK(0.5,1)'
      3dLSS -verb -nodata -matrix R.xmat.1D -save1D R.LSS.1D
      1dmatcalc '&read(R.xmat.1D) &transp &read(R.LSS.1D) &mult &write(R.mult.1D)'
      1dplot R.mult.1D &
      1dgrayplot R.mult.1D &
    
     * 3dDeconvolve is used to setup the matrix into file R.xmat.1D
     * 3dLSS is used to compute the LSS estimator vectors into file R.LSS.1D
     * 1dmatcalc is used to multiply the '-save1D' matrix into the regression matrix:
         [R.mult.1D] = [R.xmat.1D]' [R.LSS.1D]
       where [x] = matrix made from columns of numbers in file x, and ' = transpose.
     * 1dplot and 1dgrayplot are used to display the results.
     * The j-th column in the R.mult.1D file is the set of weights of the true betas
       that influence the estimated j-th LSS beta.
     * e.g., Note that the 4th and 5th stimuli are close in time (3.6 s), and that
       the result is that the LSS estimator for the 4th and 5th beta weights mix up
       the 'true' 4th, 5th, and 6th betas.  For example, looking at the 4th column
       of R.mult.1D, we see that
          beta4{LSS} = beta4{LSA} + 0.33*beta5{LSA} - 0.27*beta6{LSA} + small stuff
     * The sum of each column of R.mult.1D is 1 (e.g., run '1dsum R.mult.1D'),
       and the diagonal elements are also 1, showing that the j-th LSS beta is
       equal to the j-th LSA beta plus a weighted sum of the other LSA betas, where
       those other weights add up to zero.
    
    --------------------------------------------------------------------------
    -- RWCox - Dec 2011 - Compute fast, abend early, leave a pretty dataset --
    --------------------------------------------------------------------------
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
