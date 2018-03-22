*********
3dTfitter
*********

.. _3dTfitter:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTfitter [options]
    
    * At each voxel, assembles and solves a set of linear equations.
     ++ The matrix at each voxel may be the same or may be different.
     ++ This flexibility (for voxel-wise regressors) is one feature
        that makes 3dTfitter different from 3dDeconvolve.
     ++ Another distinguishing feature is that 3dTfitter allows for
        L2, L1, and L2+L1 (LASSO) regression solvers, and allows you
        to impose sign constraints on the solution parameters.
    
    * Output is a bucket dataset with the beta parameters at each voxel.
    
    * You can also get output of fitted time series at each voxel, and
      the error sum of squares (e.g., for generating statistics).
    
    * You can also deconvolve with a known kernel function (e.g., an HRF
      model in FMRI, or an arterial input function in DSC-MRI, et cetera),
      in which case the output dataset is a new time series dataset,
      containing the estimate of the source function that, when convolved
      with your input kernel function, fits the data (in each voxel).
    
    * The basic idea is to compute the beta_i so that the following
      is approximately true:
    
             RHS(t) = sum { beta_i * LHS_i(t) }
                       i>=1
    
      With the '-FALTUNG' (deconvolution) option, the model expands to be
    
             RHS(t) = sum { K(j)*S(t-j) } + sum { beta_i * LHS_i(t) }
                       j>=0                  i>=1
    
      where K() is the user-supplied causal kernel function, and S() is
      the source time series to be estimated along with the betas
      (which can be thought of as the 'baseline' fit).
    
    * The model basis functions LHS_i(t) and the kernel function K(t)
      can be .1D files (fixed for all voxels) and/or 3D+time datasets
      (different for each voxel).
    
    * The fitting approximation can be done in 4 different ways, minimizing
      the errors (differences between RHS(t) and the fitted equation) in
      the following ways:
       ++ L2 [-l2fit option]   = least sum of squares of errors
       ++ L1 [-l1fit option]   = least sum of absolute values of errors
       ++ L2 LASSO             = least sum of squares of errors, with an added
           [-l2lasso option]     L1 penalty on the size of the solution parameters
       ++ L2 Square Root LASSO = least square root of the sum of squared errors
           [-l2sqrtlasso option] with an added L1 penalty on the solution parameters
    
    ***** Which fitting method is better?
          The answer to that question depends strongly on what you are
          going to use the results for!  And on the quality of the data.
    
    ***** 3dTfitter is not for the casual user! *****
    
    ----------------------------------
    SPECIFYING THE EQUATIONS AND DATA:
    ----------------------------------
      -RHS rset = Specifies the right-hand-side 3D+time dataset.
                    ('rset' can also be a 1D file with 1 column)
                 * Exactly one '-RHS' option must be given to 3dTfitter.
    
      -LHS lset = Specifies a column (or columns) of the left-hand-side matrix.
                 * More than one 'lset' can follow the '-LHS' option, but each
                   input filename must NOT start with the '-' character!
                 * Or you can use multiple '-LHS' options, if you prefer.
                 * Each 'lset' can be a 3D+time dataset, or a 1D file
                   with 1 or more columns.
                 * A 3D+time dataset defines one column in the LHS matrix.
                  ++ If 'rset' is a 1D file, then you cannot input a 3D+time
                     dataset with '-LHS'.
                  ++ If 'rset' is a 3D+time dataset, then the 3D+time dataset(s)
                     input with '-LHS' must have the same voxel grid as 'rset'.
                 * A 1D file defines as many columns in the LHS matrix as
                   are in the file.
                  ++ For example, you could input the LHS matrix from the
                     .xmat.1D file output by 3dDeconvolve, if you wanted
                     to repeat the same linear regression using 3dTfitter,
                     for some bizarre unfathomable twisted psychotic reason.
                ** If you have a problem where some LHS vectors might be tiny,
                     causing stability problems, you can choose to omit them
                     by using the '-vthr' option.  By default, only all-zero
                     vectors will be omitted from the regression.
                ** Note that if the scales of the LHS vectors are grossly different
                     (e.g., 0 < vector#1 < 0.01  and  0 < vector#2 < 1000),
                     then numerical errors in the calculations might cause the
                     results to be unreliable.  To avoid this problem, you can
                     scale the vectors (before running 3dTfitter) so that they
                     have similar magnitudes.
                ** Note that if you are fitting a time series dataset that has
                     nonzero mean, then at least some of your basis vectors
                     should have nonzero mean, or you won't be able to get a
                     good fit.  If necessary, use '-polort 0' to fit the mean
                     value of the dataset, so that the zero-mean LHS vectors
                     can do their work in fitting the fluctuations in the data!
                     [This means you, HJJ!]
               *** Columns are assembled in the order given on the command line,
                   which means that LHS parameters will be output in that order!
               *** If all LHS inputs are 1D vectors AND you are using least
                   squares fitting without constraints, then 3dDeconvolve would
                   be more efficient, since each voxel would have the same set
                   of equations -- a fact that 3dDeconvolve exploits for speed.
                  ++ But who cares about CPU time?  Come on baby, light my fire!
    
      -polort p = Add 'p+1' Legendre polynomial columns to the LHS matrix.
                 * These columns are added to the LHS matrix AFTER all other
                   columns specified by the '-LHS' option, even if the '-polort'
                   option appears before '-LHS' on the command line.
                ** By default, NO polynomial columns will be used.
    
      -vthr v   = The value 'v' (between 0.0 and 0.09, inclusive) defines the
                   threshold below which LHS vectors will be omitted from
                   the regression analysis.  Each vector's L1 norm (sum of
                   absolute values) is computed.  Any vector whose L1 norm
                   is less than or equal to 'v' times the largest L1 norm
                   will not be used in the analysis, and will get 0 weight
                   in the output.  The purpose of this option is to let you
                   have tiny inputs and have them be ignored.
                  * By default, 'v' is zero ==> only exactly zero LHS columns
                    will be ignored.
                 ** Prior to 18 May 2010, the built-in (and fixed) value of
                    'v' was 0.000333.  Thus, to get the old results, you should
                    use option '-vthr 0.000333' -- this means YOU, Rasmus Birn!
    
    --------------
    DECONVOLUTION:
    --------------
      -FALTUNG fset fpre pen fac
                = Specifies a convolution (German: Faltung) model to be
                  added to the LHS matrix.  Four arguments follow the option:
    
             -->** 'fset' is a 3D+time dataset or a 1D file that specifies
                   the known kernel of the convolution.
                 * fset's time point [0] is the 0-lag point in the kernel,
                   [1] is the 1-lag into the past point, etc.
                  ++ Call the data Z(t), the unknown signal S(t), and the
                     known kernel H(t).  The equations being solved for
                     the set of all S(t) values are of the form
                       Z(t) = H(0)S(t) + H(1)S(t-1) + ... + H(L)S(t-L) + noise
                     where L is the last index in the kernel function.
                ++++ N.B.: The TR of 'fset' (the source of H) and the TR of the
                           RHS dataset (the source of Z) MUST be the same, or
                           the deconvolution results will be revoltingly
                            meaningless drivel (or worse)!
    
             -->** 'fpre' is the prefix for the output time series S(t) to
                   be created -- it will have the same length as the input
                   'rset' time series.
                  ++ If you don't want this time series (why?), set 'fpre'
                     to be the string 'NULL'.
                  ++ If you want to see the fit of the model to the data
                     (a very good idea), use the '-fitts' option, which is
                     described later.
    
             -->** 'pen' selects the type of penalty function to be
                   applied to constrain the deconvolved time series:
                  ++ The following penalty functions are available:
                       P0[s] = f^q * sum{ |S(t)|^q }
                       P1[s] = f^q * sum{ |S(t)-S(t-1)|^q }
                       P2[s] = f^q * sum{ |2*S(t)-S(t-1)-S(t+1)|^q }
                       P3[s] = f^q * sum{ |3*S(t)-3*S(t-1)-S(t+1)+S(t-2)|^q }
                     where S(t) is the deconvolved time series;
                     where q=1 for L1 fitting, q=2 for L2 fitting;
                     where f is the value of 'fac' (defined below).
                       P0 tries to keep S(t) itself small
                       P1 tries to keep point-to-point fluctuations
                          in S(t) small (1st derivative)
                       P2 tries to keep 3 point fluctuations
                          in S(t) small (2nd derivative)
                       P3 tries to keep 4 point fluctuations
                          in S(t) small (3nd derivative)
                  ++ Higher digits try to make the result function S(t)
                     smoother.  If a smooth result makes sense, then use
                     the string '012' or '0123' for 'pen'.
                  ++ In L2 regression, these penalties are analogous to Wiener
                     (frequency space) deconvolution, with noise spectra
                     proportional to
                       P0 ==> fac^2 * 1 (constant in frequency)
                       P1 ==> fac^2 * freq^2
                       P2 ==> fac^2 * freq^4
                       P3 ==> fac^2 * freq^6
                     However, 3dTfitter does deconvolution in the time
                     domain, not the frequency domain, and you can choose
                     to use L2, L1, or LASSO (L2+L1) regression.
                  ++ The value of 'pen' is a combination of the digits
                     '0', '1', '2', and/or '3'; for example:
                         0 = use P0 only
                         1 = use P1 only
                         2 = use P2 only
                         3 = use P3 only
                        01 = use P0+P1 (the sum of these two functions)
                        02 = use P0+P2
                        12 = use P1+P2
                       012 = use P0+P1+P2 (sum of three penalty functions)
                      0123 = use P0+P1+P2+P3 (et cetera)
                     If 'pen' does not contain any of the digits 0..3,
                     then '01' will be used.
    
             -->** 'fac' is the positive weight 'f' for the penalty function:
                  ++ if fac < 0, then the program chooses a penalty factor
                     for each voxel separately and then scales that by -fac.
                  ++ use fac = -1 to get this voxel-dependent factor unscaled.
                     (this is a very reasonable place to start, by the way :-)
                  ++ fac = 0 is a special case: the program chooses a range
                     of penalty factors, does the deconvolution regression
                     for each one, and then chooses the fit it likes best
                     (as a tradeoff between fit error and solution size).
                  ++ fac = 0 will be MUCH slower since it solves about 20
                     problems for each voxel and then chooses what it likes.
                     setenv AFNI_TFITTER_VERBOSE YES to get some progress
                     reports, if you want to see what it is doing.
                  ++ Instead of using fac = 0, a useful alternative is to
                     do some test runs with several negative values of fac,
                     [e.g., -1, -2, and -3] and then look at the results to
                     determine which one is most suitable for your purposes.
                  ++ It is a good idea to experiment with different fac values,
                     so you can see how the solution varies, and so you can get
                     some idea of what penalty level to use for YOUR problems.
                  ++ SOME penalty has to be applied, since otherwise the
                     set of linear equations for S(t) is under-determined
                     and/or ill-conditioned!
    
                ** If '-LHS' is used with '-FALTUNG', those basis vectors can
                   be thought of as a baseline to be regressed out at the
                   same time the convolution model is fitted.
                  ++ When '-LHS' supplies a baseline, it is important
                     that penalty type 'pen' include '0', so that the
                     collinearity between convolution with a constant S(t)
                     and a constant baseline can be resolved!
                  ++ Instead of using a baseline here, you could project the
                     baseline out of a dataset or 1D file using 3dDetrend,
                     before using 3dTfitter.
    
               *** At most one '-FALTUNG' option can be used!!!
    
               *** Consider the time series model
                     Z(t) = K(t)*S(t) + baseline + noise,
                   where Z(t) = data time series (in each voxel)
                         K(t) = kernel (e.g., hemodynamic response function)
                         S(t) = stimulus time series
                     baseline = constant, drift, etc.
                        and * = convolution in time
                   Then program 3dDeconvolve solves for K(t) given S(t), whereas
                   3dTfitter -FALTUNG solves for S(t) given K(t).  The difference
                   between the two cases is that K(t) is presumed to be causal and
                   have limited support, while S(t) is a full-length time series.
    
               *** Presumably you know this already, but deconvolution in the
                   Fourier domain          -1
                                   S(t) = F  { F[Z] / F[K] }
                   (where F[] is the Fourier transform) is a bad idea, since
                   division by small values F[K] will grotesquely amplify the
                   noise.  3dTfitter does NOT even try to do such a silly thing.
    
            ****** Deconvolution is a tricky business, so be careful out there!
                  ++ e.g., Experiment with the different parameters to make
                     sure the results in your type of problems make sense.
              -->>++ Look at the results and the fits with AFNI (or 1dplot)!
                     Do not blindly assume that the results are accurate.
                  ++ There is no guarantee that the automatic selection of
                     of the penalty factor will give usable results for
                     your problem!
                  ++ You should probably use a mask dataset with -FALTUNG,
                     since deconvolution can often fail on pure noise
                     time series.
                  ++ Unconstrained (no '-cons' options) least squares ('-lsqfit')
                     is normally the fastest solution method for deconvolution.
                     This, however, may only matter if you have a very long input
                     time series dataset (e.g., more than 1000 time points).
                  ++ For unconstrained least squares deconvolution, a special
                     sparse matrix algorithm is used for speed.  If you wish to
                     disable this for some reason, set environment variable
                     AFNI_FITTER_RCMAT to NO before running the program.
                  ++ Nevertheless, a FALTUNG problem with more than 1000 time
                     points will probably take a LONG time to run, especially
                     if 'fac' is chosen to be 0.
    
    ----------------
    SOLUTION METHOD:
    ----------------
      -lsqfit   = Solve equations via least squares [the default method].
                 * This is sometimes called L2 regression by mathematicians.
                 * '-l2fit' and '-L2' are synonyms for this option.
    
      -l1fit    = Solve equations via least sum of absolute residuals.
                 * This is sometimes called L1 regression by mathematicians.
                 * '-L1' is a synonym for this option.
                 * L1 fitting is usually slower than L2 fitting, but
                   is perhaps less sensitive to outliers in the data.
                  ++ L1 deconvolution might give nicer looking results
                     when you expect the deconvolved signal S(t) to
                     have large-ish sections where S(t) = 0.
                     [The LASSO solution methods can also have this property.]
                 * L2 fitting is statistically more efficient when the
                   noise is KNOWN to be normally (Gaussian) distributed
                   (and a bunch of other assumptions are also made).
                  ++ Where such KNOWLEDGE comes from is an interesting question.
    
      -l2lasso lam [i j k ...]
                = Solve equations via least squares with a LASSO (L1) penalty
                  on the coefficients.
                 * The positive value 'lam' after the option name is the
                   weight given to the penalty.
                  ++ As a rule of thumb, you can try lam = 2 * sigma, where
                     sigma = standard deviation of noise, but that requires
                     you to have some idea what the noise level is.
                  ++ If you enter 'lam' as a negative number, then the code
                     will CRUDELY estimate sigma and then scale abs(lam) by
                     that value -- in which case, you can try lam = -2 (or so)
                     and see if that works well for you.
                  ++ Or you can use the Square Root LASSO option (next), which
                     (in theory) does not need to know sigma when setting lam.
                 * Optionally, you can supply a list of parameter indexes
                   (after 'lam') that should NOT be penalized in the
                   the fitting process (e.g., traditionally, the mean value
                   is not included in the L1 penalty.)  Indexes start at 1,
                   as in 'consign' (below).
                  ++ In deconvolution ('-FALTUNG'), all baseline parameters
                     (from '-LHS' and/or '-polort') are automatically non-penalized,
                     so there is no point to using this un-penalizing feature.
                  ++ If you are NOT doing deconvolution, then you'll need this
                     option to un-penalize the '-polort' parameters (if desired).
                ** LASSO-ing herein should be considered experimental, and its
                   implementation is subject to change!  You should definitely
                   play with different 'lam' values to see how well they work
                   for your particular types of problems.  Algorithm is here:
                  ++ TT Wu and K Lange.
                     Coordinate descent algorithms for LASSO penalized regression.
                     Annals of Applied Statistics, 2: 224-244 (2008).
                     http://arxiv.org/abs/0803.3876
                 * '-LASSO' is a synonym for this option.
    
      -l2sqrtlasso lam [i j k ...]
                = Similar to above option, but uses 'Square Root LASSO' instead:
                 * Approximately speaking, LASSO minimizes E = Q2+lam*L1,
                   where Q2=sum of squares of residuals and L1=sum of absolute
                   values of all fit parameters, while Square Root LASSO minimizes
                   sqrt(Q2)+lam*L1; the method and motivation is described here:
                  ++ A Belloni, V Chernozhukov, and L Wang.
                     Square-root LASSO: Pivotal recovery of sparse signals via
                     conic programming (2010).  http://arxiv.org/abs/1009.5689
                  ++ A coordinate descent algorithm is also used for this optimization.
                ** A reasonable range of 'lam' to use is from 1 to 10 (or so);
                   I suggest you start with 2 and see how well that works.
                  ++ Unlike the pure LASSO option above, you do not need to give
                     give a negative value for lam here -- there is no need for
                     scaling by sigma.
                 * The theoretical advantange of Square Root LASSO over
                   standard LASSO is that a good choice of 'lam' doesn't
                   depend on knowing the noise level in the data (that is
                   what 'Pivotal' means in the paper's title).
                 * '-SQRTLASSO' is a synonym for this option.
    
      --------->>**** GENERAL NOTES ABOUT LASSO and SQUARE ROOT LASSO ****<<--------
                 * LASSO methods are the only way to solve a under-determined
                   system with 3dTfitter -- one with more vectors on the RHS
                   than time points.  However, a 'solution' to such a problem
                   doesn't necessarily mean anything -- be careful out there!
                 * LASSO methods will tend to push small coefficients down
                   to zero.  This feature can be useful when doing deconvolution,
                   if you expect the result to be zero over large-ish intervals.
                  ++ L1 regression ('-l1fit') has a similar property, of course.
                  ++ This difficult-to-estimate bias in the LASSO-computed coefficients
                     makes it nearly impossible to provide reliable estimates of statistical
                     significance for the fit (e.g., R^2, F, ...).
                 * The actual penalty factor lambda used for a given coefficient
                   is lam scaled by the the L2 norm of the corresponding regression
                   column. The purpose of this is to keep the penalties scale-free:
                   if a regression column were doubled, then the corresponding fit
                   coefficient would be cut in half; thus, to keep the same penalty
                   level, lambda should also be doubled.
                 * For '-l2lasso', a negative lam additionally means to scale
                   by the estimate of sigma, as described earlier.  This feature
                   does not apply to Square Root LASSO, however (if you give a
                   negative lam to '-l2sqrtlasso', its absolute value is used).
            -->>** There is no 'best' value of lam; if you are lucky, there is
                   is a range of lam values that give reasonable results. A good
                   procedure to follow would be to use several different values of
                   lam and see how the results vary; for example, the list
                   lam = -1, -2, -4, -7, -10 might be a good starting point.
                 * If you don't give ANY numeric value after the LASSO option
                   (i.e., the next argument on the command line is another option),
                   then the program will use '-3.1415926536' for the value of lam.
                 * A tiny value of lam (say 0.01) should give almost the same
                   results as pure L2 regression.
                 * Data with a smaller signal-to-noise ratio will probably need
                   larger values of lam -- you'll have to experiment.
                 * The number of iterations used for the LASSO solution will be
                   printed out for the first voxel solved, and for ever 10,000th
                   one following -- this is mostly for my personal edification.
            -->>** Recall: "3dTfitter is not for the casual user!"
                   This statement especially applies when using LASSO, which is a
                   powerful tool -- and as such, can be dangerous if not used wisely.
    
    ---------------------
    SOLUTION CONSTRAINTS:
    ---------------------
      -consign  = Follow this option with a list of LHS parameter indexes
                  to indicate that the sign of some output LHS parameters
                  should be constrained in the solution; for example:
                     -consign +1 -3
                  which indicates that LHS parameter #1 (from the first -LHS)
                  must be non-negative, and that parameter #3 must be
                  non-positive.  Parameter #2 is unconstrained (e.g., the
                  output can be positive or negative).
                 * Parameter counting starts with 1, and corresponds to
                   the order in which the LHS columns are specified.
                 * Unlike '-LHS or '-label', only one '-consign' option
                   can be used.
                 * Do NOT give the same index more than once after
                   '-consign' -- you can't specify that an coefficient
                   is both non-negative and non-positive, for example!
               *** Constraints can be used with any of the 4 fitting methods.
               *** '-consign' constraints only apply to the '-LHS'
                   fit parameters.  To constrain the '-FALTUNG' output,
                   use the option below.
                 * If '-consign' is not used, the signs of the fitted
                   LHS parameters are not constrained.
    
      -consFAL c= Constrain the deconvolution time series from '-FALTUNG'
                  to be positive if 'c' is '+' or to be negative if
                  'c' is '-'.
                 * There is no way at present to constrain the deconvolved
                   time series S(t) to be positive in some regions and
                   negative in others.
                 * If '-consFAL' is not used, the sign of the deconvolved
                   time series is not constrained.
    
    ---------------
    OUTPUT OPTIONS:
    ---------------
      -prefix p = Prefix for the output dataset (LHS parameters) filename.
                 * Output datasets from 3dTfitter are always in float format.
                 * If you don't give this option, 'Tfitter' is the prefix.
                 * If you don't want this dataset, use 'NULL' as the prefix.
                 * If you are doing deconvolution and do not also give any
                   '-LHS' options, then this file will not be output, since
                   it comprises the fit parameters for the '-LHS' vectors.
            -->>** If the input '-RHS' file is a .1D file, normally the
                   output files are written in the AFNI .3D ASCII format,
                   where each row contains the time series data for one
                   voxel.  If you want to have these files written in the
                   .1D format, with time represented down the column
                   direction, be sure to put '.1D' on the end of the prefix,
                   as in '-prefix Elvis.1D'.  If you use '-' or 'stdout' as
                   the prefix, the resulting 1D file will be written to the
                   terminal.  (See the fun fun fun examples, below.)
    
      -label lb = Specifies sub-brick labels in the output LHS parameter dataset.
                 * More than one 'lb' can follow the '-label' option;
                   however, each label must NOT start with the '-' character!
                 * Labels are applied in the order given.
                 * Normally, you would provide exactly as many labels as
                   LHS columns.  If not, the program invents some labels.
    
      -fitts ff = Prefix filename for the output fitted time series dataset.
                 * Which is always in float format.
                 * Which will not be written if this option isn't given!
               *** If you want the residuals, subtract this time series
                   from the '-RHS' input using 3dcalc (or 1deval).
    
      -errsum e = Prefix filename for the error sums dataset, which
                  is calculated from the difference between the input
                  time series and the fitted time series (in each voxel):
                 * Sub-brick #0 is the sum of squares of differences (L2 sum)
                 * Sub-brick #1 is the sum of absolute differences (L1 sum)
                 * The L2 sum value, in particular, can be used to produce
                   a statistic to measure the significance of a fit model;
                   cf. the 'Correlation Coefficient Example' far below.
    
    --------------
    OTHER OPTIONS:
    --------------
      -mask ms  = Read in dataset 'ms' as a mask; only voxels with nonzero
                  values in the mask will be processed.  Voxels falling
                  outside the mask will be set to all zeros in the output.
                 * Voxels whose time series are all zeros will not be
                   processed, even if they are inside the mask!
    
      -quiet    = Don't print the fun fun fun progress report messages.
                 * Why would you want to hide these delightful missives?
    
    ----------------------
    ENVIRONMENT VARIABLES:
    ----------------------
     AFNI_TFITTER_VERBOSE  =  YES means to print out information during
                              the fitting calculations.
                             ++ Automatically turned on for 1 voxel -RHS inputs.
     AFNI_TFITTER_P1SCALE  =  number > 0 will scale the P1 penalty by
                              this value (e.g., to count it more)
     AFNI_TFITTER_P2SCALE  =  number > 0 will scale the P2 penalty by
                              this value
     AFNI_TFITTER_P3SCALE  =  number > 0 will scale the P3 penalty by
                              this value
     You could set these values on the command line using the AFNI standard
     '-Dvariablename=value' command line option.
    
    ------------
    NON-Options:
    ------------
    * There is no option to produce statistical estimates of the
      significance of the parameter estimates.
      ++ 3dTcorrelate might be useful, to compute the correlation
         between the '-fitts' time series and the '-RHS' input data.
      ++ You can use the '-errsum' option to get around this limitation,
         with enough cleverness.
    * There are no options for censoring or baseline generation (except '-polort').
      ++ You could generate some baseline 1D files using 1deval, perhaps.
    * There is no option to constrain the range of the output parameters,
      except the semi-infinite ranges provided by '-consign' and/or '-consFAL'.
    * This program is NOT parallelized via OpenMP :-(
    
    ------------------
    Contrived Example:
    ------------------
    The dataset 'atm' and 'btm' are assumed to have 99 time points each.
    We use 3dcalc to create a synthetic combination of these plus a constant
    plus Gaussian noise, then use 3dTfitter to fit the weights of these
    3 functions to each voxel, using 4 different methods.  Note the use of
    the input 1D time series '1D: 99@1' to provide the constant term.
    
     3dcalc -a atm+orig -b btm+orig -expr '-2*a+b+gran(100,20)' -prefix 21 -float
     3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F2u -l2fit
     3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F1u -l1fit
     3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F1c -l1fit \
               -consign -1 +3
     3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F2c -l2fit \
               -consign -1 +3
    
    In the absence of noise and error, the output datasets should be
      #0 sub-brick = -2.0 in all voxels
      #1 sub-brick = +1.0 in all voxels
      #2 sub-brick = +100.0 in all voxels
    
    ----------------------
    Yet More Contrivances:
    ----------------------
    You can input a 1D file for the RHS dataset, as in the example below,
    to fit a single time series to a weighted sum of other time series:
    
     1deval -num 30 -expr 'cos(t)' > Fcos.1D
     1deval -num 30 -expr 'sin(t)' > Fsin.1D
     1deval -num 30 -expr 'cos(t)*exp(-t/20)' > Fexp.1D
     3dTfitter -quiet -RHS Fexp.1D -LHS Fcos.1D Fsin.1D -prefix -
    
    * Note the use of the '-' as a prefix to write the results
      (just 2 numbers) to stdout, and the use of '-quiet' to hide
      the divertingly funny and informative progress messages.
    * For the Jedi AFNI Masters out there, the above example can be carried
      out on using single complicated command line:
    
     3dTfitter -quiet -RHS `1deval -1D: -num 30 -expr 'cos(t)*exp(-t/20)'` \
                      -LHS `1deval -1D: -num 30 -expr 'cos(t)'`            \
                           `1deval -1D: -num 30 -expr 'sin(t)'`            \
                      -prefix - 
    
      resulting in the single output line below:
    
     0.535479 0.000236338
    
      which are respectively the fit coefficients of 'cos(t)' and 'sin(t)'.
    
    --------------------------------
    Contrived Deconvolution Example:
    --------------------------------
    (1) Create a 101 point 1D file that is a block of 'activation'
        between points 40..50, convolved with a triangle wave kernel
        (the '-iresp' input below):
           3dConvolve -input1D -polort -1 -num_stimts 1     \
                      -stim_file 1 '1D: 40@0 10@1 950@0'    \
                      -stim_minlag 1 0 -stim_maxlag 1 5     \
                      -iresp 1 '1D: 0 1 2 3 2 1' -nlast 100 \
                | grep -v Result | grep -v '^$' > F101.1D
    
    (2) Create a 3D+time dataset with this time series in each
        voxel, plus noise that increases with voxel 'i' index:
           3dUndump -prefix Fjunk -dimen 100 100 1
           3dcalc -a Fjunk+orig -b F101.1D     \
                  -expr 'b+gran(0,0.04*(i+1))' \
                  -float -prefix F101d
           /bin/rm -f Fjunk+orig.*
    
    (3) Deconvolve, then look what you get by running AFNI:
           3dTfitter -RHS F101d+orig -l1fit \
                     -FALTUNG '1D: 0 1 2 3 2 1' F101d_fal1 012 0.0
           3dTfitter -RHS F101d+orig -l2fit \
                     -FALTUNG '1D: 0 1 2 3 2 1' F101d_fal2 012 0.0
    
    (4) View F101d_fal1+orig, F101d_fal2+orig, and F101d+orig in AFNI,
        (in Axial image and graph viewers) and see how the fit quality
        varies with the noise level and the regression type -- L1 or
        L2 regression.  Note that the default 'fac' level of 0.0 was
        selected in the commands above, which means the program selects
        the penalty factor for each voxel, based on the size of the
        data time series fluctuations and the quality of the fit.
    
    (5) Add logistic noise (long tails) to the noise-free 1D time series, then
        deconvolve and plot the results directly to the screen, using L1 and L2
        and the two LASSO fitting methods:
      1deval -a F101.1D -expr 'a+lran(.5)' > F101n.1D
      3dTfitter -RHS F101n.1D -l1fit \
                -FALTUNG '1D: 0 1 2 3 2 1' stdout 01 -2 | 1dplot -stdin -THICK &
      3dTfitter -RHS F101n.1D -l2fit \
                -FALTUNG '1D: 0 1 2 3 2 1' stdout 01 -2 | 1dplot -stdin -THICK &
      3dTfitter -RHS F101n.1D -l2sqrtlasso 2 \
                -FALTUNG '1D: 0 1 2 3 2 1' stdout 01 -2 | 1dplot -stdin -THICK &
      3dTfitter -RHS F101n.1D -l2lasso -2 \
                -FALTUNG '1D: 0 1 2 3 2 1' stdout 01 -2 | 1dplot -stdin -THICK &
        For even more fun, add the '-consfal +' option to the above commands,
        to force the deconvolution results to be positive.
    
     ***N.B.: You can only use 'stdout' as an output filename when
              the output will be written as a 1D file (as above)!
    
    --------------------------------
    Correlation Coefficient Example:
    --------------------------------
    Suppose your initials are HJJ and you want to compute the partial
    correlation coefficient of time series Seed.1D with every voxel in
    a dataset Rest+orig once a spatially dependent 'artifact' time series
    Art+orig has been projected out.  You can do this with TWO 3dTfitter
    runs, plus 3dcalc:
    
    (1) Run 3dTfitter with ONLY the artifact time series and get the
        error sum dataset
           3dTfitter -RHS Rest+orig -LHS Art+orig -polort 2 -errsum Ebase
    
    (2) Run 3dTfitter again with the artifact PLUS the seed time series
        and get the error sum dataset and also the beta coefficents
           3dTfitter -RHS Rest+orig -LHS Seed.1D Art+orig -polort 2 \
                     -errsum Eseed -prefix Bseed
    
    (3) Compute the correlation coefficient from the amount of variance
        reduction between cases 1 and 2, times the sign of the beta
           3dcalc -a Eseed+orig'[0]' -b Ebase+orig'[0]' -c Bseed+orig'[0]' \
                  -prefix CorrSeed -expr '(2*step(c)-1)*sqrt(1-a/b)'
           3drefit -fbuc -sublabel 0 'SeedCorrelation' CorrSeed+orig
    
    More cleverness could be used to compute t- or F-statistics in a
    similar fashion, using the error sum of squares between 2 different fits.
    (Of course, these are assuming you use the default '-lsqfit' method.)
    
    --------------------------------
    PPI (psycho-physiological interaction) Example:
    --------------------------------
    Suppose you are running a PPI analysis and want to deconvolve a GAM
    signal from the seed time series, hoping (very optimistically) to
    convert from the BOLD time series (typical FMRI signal) to a
    neurological time series (an impulse signal, say).
    
    If the BOLD signal at the seed is seed_BOLD.1D and the GAM signal is
    GAM.1D, then consider this example for the deconvolution, in order to
    create the neuro signal, seed_neuro.1D:
    
      3dTfitter -RHS seed_BOLD.1D                    \
                -FALTUNG GAM.1D seed_neuro.1D 012 -2 \
                -l2lasso -6
    
    *************************************************************************
    ** RWCox - Feb 2008, et seq.                                           **
    ** Created for the glorious purposes of John A Butman, MD, PhD, Poobah **
    ** But might be useful for some other well-meaning souls out there     **
    *************************************************************************
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
