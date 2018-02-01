*********
3dREMLfit
*********

.. _3dREMLfit:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dREMLfit [options]
    
       **** Generalized least squares time series fit, with REML   ****
       **** estimation of the temporal auto-correlation structure. ****
    
    ---------------------------------------------------------------------
    ****  The recommended way to use 3dREMLfit is via afni_proc.py,  ****
    ****  which will pre-process the data, and also give some useful ****
    ****  diagnostic tools/outputs for assessing the data's quality. ****
    ****  [afni_proc.py will make your FMRI-analysis life happier!]  ****
    ---------------------------------------------------------------------
    
    * This program provides a generalization of 3dDeconvolve:
        it allows for serial correlation in the time series noise.
    
    * It solves the linear equations for each voxel in the generalized
        (prewhitened) least squares sense, using the REML estimation method
        to find a best-fit ARMA(1,1) model for the time series noise
        correlation matrix in each voxel (i.e., each voxel gets a separate
        pair of ARMA parameters).
      ++ Note that the 2-parameter ARMA(1,1) correlation model is hard-coded
         into this program -- there is no way to use a more elaborate model,
         such as the 5-parameter ARMA(3,2), in 3dREMLfit.
    
    * You MUST run 3dDeconvolve first to generate the input matrix
        (.xmat.1D) file, which contains the hemodynamic regression
        model, censoring and catenation information, the GLTs, etc.
        See the output of '3dDeconvolve -help' for information on
        using that program to setup the analysis.
    
    * If you don't want the 3dDeconvolve analysis to run, you can
        prevent that by using 3dDeconvolve's '-x1D_stop' option.
    
    * 3dDeconvolve also prints out a cognate command line for running
        3dREMLfit, which should get you going with relative ease.
    
    * The output datasets from 3dREMLfit are structured to resemble
        the corresponding results from 3dDeconvolve, to make it
        easy to adapt your scripts for further processing.
    
    * Is this type of analysis (generalized least squares) important?
        That depends on your point of view, your data, and your goals.
        If you really want to know the answer, you should run
        your analyses both ways (with 3dDeconvolve and 3dREMLfit),
        through to the final step (e.g., group analysis), and then
        decide if your neuroscience/brain conclusions depend strongly
        on the type of linear regression that was used.
    
    * If you are planning to use 3dMEMA for group analysis, then using
        3dREMLfit instead of 3dDeconvolve is a good idea.  3dMEMA uses
        the t-statistic of the beta weight as well as the beta weight
        itself -- and the t-values from 3dREMLfit are probably more
        more accurate than those from 3dDeconvolve, since the underlying
        variance estimate should be more accurate (less biased).
    
    -------------------------------------------
    Input Options (the first two are mandatory)
    -------------------------------------------
     -input ddd  = Read time series dataset 'ddd'.
    
     -matrix mmm = Read the matrix 'mmm', which should have been
                     output from 3dDeconvolve via the '-x1D' option.
    
                 ** N.B.: Actually, you can omit the '-matrix' option, but
                           then the program will fabricate a matrix consisting
                           of a single column with all 1s.  This option is
                           mostly for the convenience of the author; for
                           example, to have some fun with an AR(1) time series:
                     1deval -num 1001 -expr 'gran(0,1)+(i-i)+0.7*z' > g07.1D
                     3dREMLfit -input g07.1D'{1..$}'' -Rvar -.1D -grid 5 -MAXa 0.9
    
                 ** N.B.: 3dREMLfit now supports all zero columns, if you use
                            the '-GOFORIT' option. [Ides of March, MMX A.D.]
    
     -mask MMM   = Read dataset 'MMM' as a mask for the input; voxels outside
                     the mask will not be fit by the regression model.
     -automask   = If you don't know what this does by now, I'm not telling.
                *** If you don't specify ANY mask, the program will
                     build one automatically (from each voxel's RMS)
                     and use this mask SOLELY for the purpose of
                     computing the FDR curves in the bucket dataset's header.
                  * If you DON'T want this to happen, then use '-noFDR'
                     and later run '3drefit -addFDR' on the bucket dataset.
                  * To be precise, the FDR automask is only built if
                     the input dataset has at least 5 voxels along each of
                     the x and y axes, to avoid applying it when you run
                     3dREMLfit on 1D timeseries inputs.
    -STATmask ss = Build a mask from file 'ss', and use this for the purpose
                     of computing the FDR curves.
                  * The actual results ARE not masked with this option
                      (only with '-mask' or '-automask' options).
                  * If you don't use '-STATmask', then the mask from
                      '-mask' or '-automask' is used for the FDR work.
                      If neither of those is given, then the automatically
                      generated mask described just above is used for FDR.
    
    --------------------------------------------------------------------------
    Options to Add Baseline (Null Hypothesis) Columns to the Regression Matrix
    --------------------------------------------------------------------------
     -addbase bb = You can add baseline model columns to the matrix with
                     this option.  Each column in the .1D file 'bb' will
                     be appended to the matrix.  This file must have at
                     least as many rows as the matrix does.
                  * Multiple -addbase options can be used, if needed.
                  * More than 1 file can be specified, as in
                      -addbase fred.1D ethel.1D elvis.1D
                  * None of the .1D filename can start with the '-' character,
                      since that is the signal for the next option.
                  * If the matrix from 3dDeconvolve was censored, then
                      this file (and '-slibase' files) can either be
                      censored to match, OR 3dREMLfit will censor these
                      .1D files for you.
                   + If the column length (number of rows) of the .1D file
                       is the same as the column length of the censored
                       matrix, then the .1D file WILL NOT be censored.
                   + If the column length of the .1D file is the same
                       as the column length of the uncensored matrix,
                       then the .1D file WILL be censored -- the same
                       rows excised from the matrix in 3dDeconvolve will
                       be resected from the .1D file before the .1D file's
                       columns are appended to the matrix.
                   + The censoring information from 3dDeconvolve is stored
                       in the matrix file header, and you don't have to
                       provide it again on the 3dREMLfit command line.
    
     -dsort dset = Similar to -addbase in concept, BUT the dataset 'dset'
                     provides a different baseline regressor for every
                     voxel.  This dataset must have the same number of
                     time points as the input dataset, and have the same
                     number of voxels.                          [Added 22 Jul 2015]
                   + The REML (a,b) estimation is done WITHOUT this extra
                       voxel-wise regressor, and then the selected (a,b)
                       ARMA parameters are used to do the final regression for
                       the '-R...' output datasets.  This method is not ideal,
                       but the alternative of re-doing the (a,b) estimation with
                       a different matrix for each voxel would be VERY slow.
                       -- The -dsort estimation is thus different from the -addbase
                          and/or -slibase estimations, in that the latter cases
                          incorporate the extra regressors into the REML estimation
                          of the ARMA (a,b) parameters.  The practical difference
                          between these two methods is usually very small ;-)
                   + If any voxel time series from -dsort is constant through time,
                       the program will print a warning message, and peculiar things
                       might happen.  Gleeble, fitzwilly, blorten, et cetera.
                       -- Actually, if this event happens, the 'offending' -dsort voxel
                          time series is replaced by the mean time series from that
                          -dsort dataset.
                   + The '-Rbeta' (and/or '-Obeta') option will include the
                       fit coefficient for the -dsort regressor (last).
                   + There is no way to include the -dsort regressor beta in a GLT.
                   + You can use -dsort more than once.  Please don't go crazy.
                   + Using this option slows the program down in the GLSQ loop,
                       since a new matrix and GLT set must be built up and torn down
                       for each voxel separately.
                       -- And at this time, the GLSQ loop is not OpenMP-ized.
                   + The motivation for -dsort is to apply ANATICOR to task-based
                       FMRI analyses.  You might be clever and have a better idea!?
    
     -dsort_nods = If '-dsort' is used, the output datasets reflect the impact of the
                     voxel-wise regressor(s).  If you want to compare those results
                     to the case where you did NOT give the '-dsort' option, then
                     also use the '-dsort_nods' (nods is short for 'no dsort').
                     The linear regressions will be repeated without the -dsort
                     regressor(s) and the results put into datasets with the string
                     '_nods' added to the prefix.
    
     -slibase bb = Similar to -addbase in concept, BUT each .1D file 'bb'
                     must have an integer multiple of the number of slices
                     in the input dataset; then, separate regression
                     matrices are generated for each slice, with the
                     [0] column of 'bb' appended to the matrix for
                     the #0 slice of the dataset, the [1] column of 'bb'
                     appended to the matrix for the #1 slice of the dataset,
                     and so on.  For example, if the dataset has 3 slices
                     and file 'bb' has 6 columns, then the order of use is
                         bb[0] --> slice #0 matrix
                         bb[1] --> slice #1 matrix
                         bb[2] --> slice #2 matrix
                         bb[3] --> slice #0 matrix
                         bb[4] --> slice #1 matrix
                         bb[5] --> slice #2 matrix
    
                 ** If this order is not correct, consider -slibase_sm.
    
                  * Intended to help model physiological noise in FMRI,
                     or other effects you want to regress out that might
                     change significantly in the inter-slice time intervals.
                  * Slices are the 3rd dimension in the dataset storage
                     order -- 3dinfo can tell you what that direction is:
                       Data Axes Orientation:
                         first  (x) = Right-to-Left
                         second (y) = Anterior-to-Posterior
                         third  (z) = Inferior-to-Superior   [-orient RAI]
                     In the above example, the slice direction is from
                     Inferior to Superior, so the columns in the '-slibase'
                     input file should be ordered in that direction as well.
                  * '-slibase' will slow the program down, and make it use
                      a lot more memory (to hold all the matrix stuff).
                *** At this time, 3dSynthesize has no way of incorporating
                      the extra baseline timeseries from -addbase or -slibase.
    
     -slibase_sm bb = Similar to -slibase above, BUT each .1D file 'bb'
                     must be in slice major order (i.e. all slice0 columns
                     come first, then all slice1 columns, etc).
                     For example, if the dataset has 3 slices and file
                     'bb' has 6 columns, then the order of use is
                         bb[0] --> slice #0 matrix, regressor 0
                         bb[1] --> slice #0 matrix, regressor 1
                         bb[2] --> slice #1 matrix, regressor 0
                         bb[3] --> slice #1 matrix, regressor 1
                         bb[4] --> slice #2 matrix, regressor 0
                         bb[5] --> slice #2 matrix, regressor 1
    
                 ** If this order is not correct, consider -slibase.
    
     -usetemp    = Write intermediate stuff to disk, to economize on RAM.
                     Using this option might be necessary to run with
                     '-slibase' and with '-Grid' values above the default,
                     since the program has to store a large number of
                     matrices for such a problem: two for every slice and
                     for every (a,b) pair in the ARMA parameter grid.
                  * '-usetemp' can actually speed the program up, interestingly,
                       even if you have enough RAM to hold all the intermediate
                       matrices needed with '-slibase'.  YMMV :-)
                  * '-usetemp' also writes temporary files to store dataset
                       results, which can help if you are creating multiple large
                       dataset (e.g., -Rfitts and -Rerrts in the same program run).
                  * Temporary files are written to the directory given
                      in environment variable TMPDIR, or in /tmp, or in ./
                      (preference is in that order).
                     + If the program crashes, these files are named
                         REML_somethingrandom, and you might have to
                         delete them manually.
                     + If the program ends normally, it will delete
                         these temporary files before it exits.
                     + Several gigabytes of disk space might be used
                         for this temporary storage!
                  * If the program crashes with a 'malloc failure' type of
                      message, then try '-usetemp' (malloc=memory allocator).
                  * If you use '-verb', then memory usage is printed out
                      at various points along the way.
                  * '-usetemp' disables OpenMP multi-CPU usage.
                      Only use this option if you need to, since OpenMP should
                      speed the program up significantly on multi-CPU computers.
    
     -nodmbase   = By default, baseline columns added to the matrix
                     via '-addbase' or '-slibase' or '-dsort' will each have
                     their mean removed (as is done in 3dDeconvolve).  If you
                     do NOT want this operation performed, use '-nodmbase'.
                  * Using '-nodmbase' would make sense if you used
                     '-polort -1' to set up the matrix in 3dDeconvolve, and/or
                     you actually care about the fit coefficients of the extra
                     baseline columns (in which case, don't use '-nobout').
    
    ------------------------------------------------------------------------
    Output Options (at least one must be given; 'ppp' = dataset prefix name)
    ------------------------------------------------------------------------
     -Rvar  ppp  = dataset for REML variance parameters
     -Rbeta ppp  = dataset for beta weights from the REML estimation
                     [similar to the -cbucket output from 3dDeconvolve]
                   * This dataset will contain all the beta weights, for
                       baseline and stimulus regressors alike, unless the
                       '-nobout' option is given -- in that case, this
                       dataset will only get the betas for the stimulus
                       regressors.
     -Rbuck ppp  = dataset for beta + statistics from the REML estimation;
                     also contains the results of any GLT analysis requested
                     in the 3dDeconvolve setup.
                     [similar to the -bucket output from 3dDeconvolve]
                   * This dataset does NOT get the betas (or statistics) of
                       those regressors marked as 'baseline' in the matrix file.
                   * If the matrix file from 3dDeconvolve does not contain
                       'Stim attributes' (which will happen if all inputs
                       to 3dDeconvolve were labeled as '-stim_base'), then
                       -Rbuck won't work, since it is designed to give the
                       statistics for the 'stimuli' and there aren't any matrix
                       columns labeled as being 'stimuli'.
                   * In such a case, to get statistics on the coefficients,
                       you'll have to use '-gltsym' and '-Rglt'; for example,
                       to get t-statistics for all coefficients from #0 to #77:
                          -tout -Rglt Colstats -gltsym 'SYM: Col[[0..77]]' ColGLT
                     where 'Col[3]' is the generic label that refers to matrix
                     column #3, et cetera.
                   * FDR curves for so many statistics (78 in the example)
                       might take a long time to generate!
     -Rglt  ppp  = dataset for beta + statistics from the REML estimation,
                     but ONLY for the GLTs added on the 3dREMLfit command
                     line itself via '-gltsym'; GLTs from 3dDeconvolve's
                     command line will NOT be included.
                   * Intended to give an easy way to get extra contrasts
                       after an earlier 3dREMLfit run.
                   * Use with '-ABfile vvv' to read the (a,b) parameters
                       from the earlier run, where 'vvv' is the '-Rvar'
                       dataset output from that run.
                       [If you didn't save the '-Rvar' file, then it will]
                       [be necessary to redo the REML loop, which is slow]
    
     -fout       = put F-statistics into the bucket dataset
     -rout       = put R^2 statistics into the bucket dataset
     -tout       = put t-statistics into the bucket dataset
                     [if you use -Rbuck and do not give any of -fout, -tout,]
                     [or -rout, then the program assumes -fout is activated.]
     -noFDR      = do NOT add FDR curve data to bucket datasets
                     [FDR curves can take a long time if -tout is used]
    
     -nobout     = do NOT add baseline (null hypothesis) regressor betas
                     to the -Rbeta and/or -Obeta output datasets.
                     ['stimulus' columns are marked in the .xmat.1D matrix ]
                     [file; all other matrix columns are 'baseline' columns]
    
     -Rfitts ppp = dataset for REML fitted model
                     [like 3dDeconvolve, a censored time point gets]
                     [the actual data values from that time index!!]
    
     -Rerrts ppp = dataset for REML residuals = data - fitted model
                     [like 3dDeconvolve,  a censored time]
                     [point gets its residual set to zero]
     -Rwherr ppp = dataset for REML residual, whitened using the
                     estimated ARMA(1,1) correlation matrix of the noise
                     [Note that the whitening matrix used is the inverse  ]
                     [of the Choleski factor of the correlation matrix C; ]
                     [however, the whitening matrix isn't uniquely defined]
                     [(any matrix W with C=inv(W'W) will work), so other  ]
                     [whitening schemes could be used and these would give]
                     [different whitened residual time series datasets.   ]
    
     -gltsym g h = read a symbolic GLT from file 'g' and label it with
                     string 'h'
                    * As in 3dDeconvolve, you can also use the 'SYM:' method
                        to put the definition of the GLT directly on the
                        command line.
                    * The symbolic labels for the stimuli are as provided
                        in the matrix file, from 3dDeconvolve.
                  *** Unlike 3dDeconvolve, you supply the label 'h' for
                        the output coefficients and statistics directly
                        after the matrix specification 'g'.
                    * Like 3dDeconvolve, the matrix generated by the
                        symbolic expression will be printed to the screen
                        unless environment variable AFNI_GLTSYM_PRINT is NO.
                    * These GLTs are in addition to those stored in the
                        matrix file, from 3dDeconvolve.
                    * If you don't create a bucket dataset using one of
                        -Rbuck or -Rglt (or -Obuck / -Oglt), using
                        -gltsym is completely pointless and stupid!
                   ** Besides the stimulus labels read from the matrix
                        file (put there by 3dDeconvolve), you can refer
                        to regressor columns in the matrix using the
                        symbolic name 'Col', which collectively means
                        all the columns in the matrix.  'Col' is a way
                        to test '-addbase' and/or '-slibase' regressors
                        for significance; for example, if you have a
                        matrix with 10 columns from 3dDeconvolve and
                        add 2 extra columns to it, then you could use
                          -gltsym 'SYM: Col[[10..11]]' Addons -tout -fout
                        to create a GLT to include both of the added
                        columns (numbers 10 and 11).
                        -- 'Col' cannot be used to test the '-dsort'
                           regressor for significance!
    
    The options below let you get the Ordinary Least SQuares outputs
    (without adjustment for serial correlation), for comparisons.
    These datasets should be essentially identical to the results
    you would get by running 3dDeconvolve (with the '-float' option!):
    
     -Ovar   ppp = dataset for OLSQ st.dev. parameter (kind of boring)
     -Obeta  ppp = dataset for beta weights from the OLSQ estimation
     -Obuck  ppp = dataset for beta + statistics from the OLSQ estimation
     -Oglt   ppp = dataset for beta + statistics from '-gltsym' options
     -Ofitts ppp = dataset for OLSQ fitted model
     -Oerrts ppp = dataset for OLSQ residuals (data - fitted model)
                     [there is no -Owherr option; if you don't]
                     [see why, then think about it for a while]
    
    Note that you don't have to use any of the '-R' options; you could
    use 3dREMLfit just for the '-O' options if you want.  In that case,
    the program will skip the time consuming ARMA(1,1) estimation for
    each voxel, by pretending you used the option '-ABfile =0,0'.
    
    -------------------------------------------------------------------
    The following options control the ARMA(1,1) parameter estimation
    for each voxel time series; normally, you do not need these options
    -------------------------------------------------------------------
     -MAXa am   = Set max allowed AR a parameter to 'am' (default=0.8).
                    The range of a values scanned is   0 .. +am (-POScor)
                                               or is -am .. +am (-NEGcor).
    
     -MAXb bm   = Set max allow MA b parameter to 'bm' (default=0.8).
                    The range of b values scanned is -bm .. +bm.
                   * The largest value allowed for am and bm is 0.9.
                   * The smallest value allowed for am and bm is 0.1.
                   * For a nearly pure AR(1) model, use '-MAXb 0.1'
                   * For a nearly pure MA(1) model, use '-MAXa 0.1'
    
     -Grid pp   = Set the number of grid divisions in the (a,b) grid
                    to be 2^pp in each direction over the range 0..MAX.
                    The default (and minimum) value for 'pp' is 3.
                    Larger values will provide a finer resolution
                    in a and b, but at the cost of some CPU time.
                   * To be clear, the default settings use a grid
                       with 8 divisions in the a direction and 16 in
                       the b direction (since a is non-negative but
                       b can be either sign).
                   * If -NEGcor is used, then '-Grid 3' means 16 divisions
                       in each direction, so that the grid spacing is 0.1
                       if MAX=0.8.  Similarly, '-Grid 4' means 32 divisions
                       in each direction, '-Grid 5' means 64 divisions, etc.
                   * I see no reason why you would ever use a -Grid size
                       greater than 5 (==> parameter resolution = 0.025).
                   * In my limited experiments, there was little appreciable
                       difference in activation maps between '-Grid 3' and
                       '-Grid 5', especially at the group analysis level.
                   * The program is somewhat slower as the -Grid size expands.
                       And uses more memory, to hold various matrices for
                       each (a,b) case.
    
     -NEGcor    = Allows negative correlations to be used; the default
                    is that only positive correlations are searched.
                    When this option is used, the range of a scanned
                    is -am .. +am; otherwise, it is 0 .. +am.
                   * Note that when -NEGcor is used, the number of grid
                       points in the a direction doubles to cover the
                       range -am .. 0; this will slow the program down.
     -POScor    = Do not allow negative correlations.  Since this is
                    the default, you don't actually need this option.
                    [FMRI data doesn't seem to need the modeling  ]
                    [of negative correlations, but you never know.]
    
     -Mfilt mr  = After finding the best fit parameters for each voxel
                    in the mask, do a 3D median filter to smooth these
                    parameters over a ball with radius 'mr' mm, and then
                    use THOSE parameters to compute the final output.
                   * If mr < 0, -mr is the ball radius in voxels,
                       instead of millimeters.
                     [No median filtering is done unless -Mfilt is used.]
                   * This option is not recommended; it is just here for
                     experimentation.
    
     -CORcut cc = The exact ARMA(1,1) correlation matrix (for a != 0)
                    has no non-zero entries.  The calculations in this
                    program set correlations below a cutoff to zero.
                    The default cutoff is 0.001, but can be altered with
                    this option.  The usual reason to use this option is
                    to test the sensitivity of the results to the cutoff.
    
     -ABfile ff = Instead of estimating the ARMA(a,b) parameters from the
                    data, read them from dataset 'ff', which should have
                    2 float-valued sub-bricks.
                   * Note that the (a,b) values read from this file will
                       be mapped to the nearest ones on the (a,b) grid
                       before being used to solve the generalized least
                       squares problem.  For this reason, you may want
                       to use '-Grid 5' to make the (a,b) grid finer, if
                       you are not using (a,b) values from a -Rvar file.
                   * Using this option will skip the slowest part of
                       the program, which is the scan for each voxel
                       to find its optimal (a,b) parameters.
                   * One possible application of -ABfile:
                      + save (a,b) using -Rvar in 3dREMLfit
                      + process them in some way (spatial smoothing?)
                      + use these modified values for fitting in 3dREMLfit
                          [you should use '-Grid 5' for such a case]
                   * Another possible application of -ABfile:
                      + use (a,b) from -Rvar to speed up a run with -Rglt
                          when you want to run some more contrast tests.
                   * Special case:
                         -ABfile =0.7,-0.3
                       e.g., means to use a=0.7 and b=-0.3 for all voxels.
                       The program detects this special case by looking for
                       '=' as the first character of the string 'ff' and
                       looking for a comma in the middle of the string.
                       The values of a and b must be in the range -0.9..+0.9.
                     * The purpose of this special case is to facilitate
                         comparison with Software PrograMs that use the same
                         temporal correlation structure for all voxels.
    
     -GOFORIT   = 3dREMLfit checks the regression matrix for tiny singular
                    values (as 3dDeconvolve does).  If the matrix is too
                    close to being rank-deficient, then the program will
                    not proceed.  You can use this option to force the
                    program to continue past such a failed collinearity
                    check, but you MUST check your results to see if they
                    make sense!
                  ** '-GOFORIT' is required if there are all zero columns
                       in the regression matrix.  However, at this time
                       [15 Mar 2010], the all zero columns CANNOT come from
                       the '-slibase' inputs.
                     ** Nor from the '-dsort' inputs.
                  ** If there are all zero columns in the matrix, a number
                       of WARNING messages will be generated as the program
                       pushes forward in the solution of the linear systems.
    
    ---------------------
    Miscellaneous Options
    ---------------------
     -quiet = turn off most progress messages :(
     -verb  = turn on more progress messages  :)
                [including memory usage reports at various stages]
    
    ==========================================================================
    ===========  Various Notes (as if this help weren't long enough) =========
    ==========================================================================
    
    ------------------
    What is ARMA(1,1)?
    ------------------
    * The correlation coefficient r(k) of noise samples k units apart in time,
        for k >= 1, is given by r(k) = lam * a^(k-1)
        where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b)
        (N.B.: lam=a when b=0 -- AR(1) noise has r(k)=a^k for k >= 0)
        (N.B.: lam=b when a=0 -- MA(1) noise has r(k)=b for k=1, r(k)=0 for k>1)
    
    * lam can be bigger or smaller than a, depending on the sign of b:
        b > 0 means lam > a;  b < 0 means lam < a.
    
    * What I call (a,b) here is sometimes called (p,q) in the ARMA literature.
    
    * For a noise model which is the sum of AR(1) and white noise, 0 < lam < a
        (i.e., a > 0  and  -a < b < 0 ). Thus, the model 'AR(1)+white noise'
        is a proper subset of ARMA(1,1) -- and also a proper subset of the default
        -POScor setting (which also allows 0 < a < lam via b > 0).
    
    * The natural range of a and b is -1..+1.  However, unless -NEGcor is
        given, only non-negative values of a will be used, and only values
        of b that give lam > 0 will be allowed.  Also, the program doesn't
        allow values of a or b to be outside the range -0.9..+0.9.
    
    * The program sets up the correlation matrix using the censoring and run
        start information saved in the header of the .xmat.1D matrix file, so
        that the actual correlation matrix used will not always be Toeplitz.
    
    * The 'Rvar' dataset has 5 sub-bricks with variance parameter estimates:
        #0 = a = factor by which correlations decay from lag k to lag k+1
        #1 = b parameter
        #2 = lam (see the formula above) = correlation at lag 1
        #3 = standard deviation of ARMA(1,1) noise in that voxel
        #4 = -log(REML likelihood function) = optimized function at (a,b)
    
    * The 'Rbeta' dataset has the beta (model fit) parameters estimates
        computed from the prewhitened time series data in each voxel,
        as in 3dDeconvolve's '-cbucket' output, in the order in which
        they occur in the matrix.  -addbase and -slibase and -dsort beta
        values come last in this file.
       [The '-nobout' option will disable output of baseline parameters.]
    
    * The 'Rbuck' dataset has the beta parameters and their statistics
        mixed together, as in 3dDeconvolve's '-bucket' output.
    
    -------------------------------------------------------------------
    What is REML = REsidual (or REstricted) Maximum Likelihood, anyway?
    -------------------------------------------------------------------
    * Ordinary Least SQuares (which assumes the noise correlation matrix is
        the identity) is consistent for estimating regression parameters,
        but is not consistent for estimating the noise variance if the
        noise is significantly correlated in time ('serial correlation').
    
    * Maximum likelihood estimation (ML) of the regression parameters and
        variance/correlation together is asymptotically consistent as the
        number of samples goes to infinity, but the variance estimates
        might still have significant bias at a 'reasonable' number of
        data points.
    
    * REML estimates the variance/correlation parameters in a space
        of residuals -- the part of the data left after the model fit
        is subtracted.  The amusing/cunning part is that the model fit
        used to define the residuals is itself the generalized least
        squares fit where the variance/correlation matrix is the one found
        by the REML fit itself.  This feature makes REML estimation nonlinear,
        and the REML equations are usually solved iteratively, to maximize
        the log-likelihood in the restricted space.  In this program, the
        REML function is instead simply optimized over a finite grid of
        the correlation matrix parameters a and b.  The matrices for each
        (a,b) pair are pre-calculated in the setup phase, and then are
        re-used in the voxel loop.  The purpose of this grid-based method
        is speed -- optimizing iteratively to a highly accurate (a,b)
        estimation for each voxel would be very time consuming, and pretty
        pointless.  If you are concerned about the sensitivity of the
        results to the resolution of the (a,b) grid, you can use the
        '-Grid 5' option to increase this resolution and see if your
        activation maps change significantly.  In test cases, the resulting
        betas and statistics have not changed appreciably between '-Grid 3'
        and '-Grid 5'; however, you might want to test this on your own data
        (just for fun).
    
    * REML estimates of the variance/correlation parameters are still
        biased, but are generally significantly less biased than ML estimates.
        Also, the regression parameters (betas) should be estimated somewhat
        more accurately (i.e., with smaller variance than OLSQ).  However,
        this effect is generally small in FMRI data, and probably won't affect
        your group results noticeably (if you don't carry parameter variance
        estimates to the inter-subject analysis, as is done in 3dMEMA).
    
    * After the (a,b) parameters are estimated, then the solution to the
        linear system is available via Generalized Least SQuares; that is,
        via prewhitening using the Choleski factor of the estimated
        variance/covariance matrix.
    
    * In the case with b=0 (that is, AR(1) correlations), and if there are
        no time gaps (no censoring, no run breaks), then it is possible to
        directly estimate the a parameter without using REML.  This program
        does not implement such a method (e.g., the Yule-Walker equation).
        The reason why should be obvious.
    
    * If you like linear algebra, see my scanned math notes about 3dREMLfit:
        https://afni.nimh.nih.gov/pub/dist/doc/misc/3dREMLfit/3dREMLfit_mathnotes.pdf
    
    * I have been asked if 3dREMLfit prewhitens the design matrix as well as
        the data. The short answer is YES. The long answer follows:
    
    * Mathematically, the GLSQ solution is expressed as
        f = inv[ X' inv(R) X] X' inv(R) y
        where X = model matrix, R = symmetric correlation matrix
                  (of noise, depends on the a,b parameters),
              f = parameter estimates, and y = data vector.
        Notation: ' = transpose, inv() = inverse matrix.
        A symmetric matrix S such that SS = R is called a square root of R
        (there are many such matrices).  The matrix inv(S) is a prewhitening
        matrix. That is, if the noise vector q is such that E(q q') = R
        (here E = expected value), and vector t = inv(S) q, then
        E(t t') = E[ inv(S)q q'inv(S) ] = inv(S) S S inv(S) = I.
        Note that inv(R) = inv(S) inv(S), and we can rewrite the GLSQ solution as
        f = inv[ X' inv(S) inv(S) X ] X' inv(S) inv(S) y
          = inv[ (inv(S)X)' (inv(S)X) ] (inv(S)X)' (inv(S)y)
        so the GLSQ solution is equivalent to the OLSQ solution, with the model
        matrix X replaced by inv(S)X and the data vector y replaced by inv(S)y;
        that is, we prewhiten both of them.  In 3dREMLfit, this is done implicitly
        in the solution method outlined in the 7-step procedure on the fourth page
        of my math notes -- a procedure designed for efficient implementation
        with banded R. The prewhitened X matrix is never explicitly computed:
        it is not needed, since the goal is to compute vector f, not inv(S)X.
    
    * The idea of pre-whitening the data but NOT the matrix is a very bad plan.
        If you work through the linear algebra, you'll see that the resulting
        estimate for f is not statistically consistent with the underlying model!
        In other words, prewhitening only the data but not the matrix is WRONG.
    
    * The estimation method for (a,b) is nonlinear; that is, these parameters
        are NOT estimated by doing an initial OLSQ (or any other one-shot initial
        calculation), then fitting (a,b) to the resulting residuals. Rather,
        a number of different (a,b) values are tried out to find the parameter pair
        where the log-likelihood is optimized. To be precise, the function
        that is minimized (over the discrete a,b grid) is
          L(a,b) =  log(det(R(a,b))) + log(det(X' inv(R(a,b)) X))
                  + (n-m)log(y'P(a,b)y)   - log(det(X'X'))
        where R(a,b) = ARMA(1,1) correlation matrix (symetric n X n)
              n      = dimension of data vector = number of rows in X
              m      = number of columns in X = number of regressors
              y      = data vector for a given voxel
              P(a,b) = prewhitening projection matrix (symmetric n X n)
                     = inv(R) - inv(R)X inv(X' inv(R) X) X' inv(R)
        The first 2 terms in L only depend on the (a,b) parameters, and can be
          thought of as a penalty that favors some (a,b) values over others,
          independent of the data -- for ARMA(1,1), the a=b=0 white noise
          model is penalized somewhat relative to the non-white noise cases.
        The 3rd term uses the 2-norm of the prewhitened residuals.
        The 4th term depends only on X, and is not actually used herein.
    
    * Again, see the notes below for more fun math and algorithmic details:
        https://afni.nimh.nih.gov/pub/dist/doc/misc/3dREMLfit/3dREMLfit_mathnotes.pdf
    
    ----------------
    Other Commentary
    ----------------
    * Again: the ARMA(1,1) parameters 'a' (AR) and 'b' (MA) are estimated
        only on a discrete grid, for the sake of CPU time.
    
    * Each voxel gets a separate pair of 'a' and 'b' parameters.
        There is no option to estimate global values for 'a' and 'b'
        and use those for all voxels.  Such an approach might be called
        'kindergarten statistics' by the authors of Some People's Methods.
    
    * OLSQ = Ordinary Least SQuares; these outputs can be used to compare
             the REML/GLSQ estimations with the simpler OLSQ results
             (and to test this program vs. 3dDeconvolve).
    
    * GLSQ = Generalized Least SQuares = estimated linear system solution
             taking into account the variance/covariance matrix of the noise.
    
    * The '-matrix' file must be from 3dDeconvolve; besides the regression
        matrix itself, the header contains the stimulus labels, the GLTs,
        the censoring information, etc.
    
    * If you don't actually want the OLSQ results from 3dDeconvolve, you can
        make that program stop after the X matrix file is written out by using
        the '-x1D_stop' option, and then running 3dREMLfit; something like this:
          3dDeconvolve -bucket Fred -nodata 800 2.5 -x1D_stop ...
          3dREMLfit -matrix Fred.xmat.1D -input ...
        In the above example, no 3D dataset is input to 3dDeconvolve, so as to
        avoid the overhead of having to read it in for no reason.  Instead,
        the '-nodata 800 2.5' option is used to setup the time series of the
        desired length (corresponding to the real data's length, here 800 points),
        and the appropriate TR (here, 2.5 seconds).  This will properly establish
        the size and timing of the matrix file.
    
    * The bucket output datasets are structured to mirror the output
        from 3dDeconvolve with the default options below:
          -nobout -full_first
        Note that you CANNOT use options like '-bout', '-nocout', and
        '-nofull_first' with 3dREMLfit -- the bucket datasets are ordered
        the way they are and you'll just have to live with it.
    
    * If the 3dDeconvolve matrix generation step did NOT have any non-base
        stimuli (i.e., everything was '-stim_base'), then there are no 'stimuli'
        in the matrix file.  In that case, since by default 3dREMLfit doesn't
        compute statistics of baseline parameters, to get statistics you will
        have to use the '-gltsym' option here, specifying the desired column
        indexes with the 'Col[]' notation, and then use '-Rglt' to get these
        values saved somewhere (since '-Rbuck' won't work if there are no
        'Stim attributes').
    
    * All output datasets are in float format [i.e., no '-short' option].
        Internal calculations are done in double precision.
    
    * If the regression matrix (including any added columns from '-addbase'
        or '-slibase') is rank-deficient (e.g., has collinear columns),
        then the program will print a message something like
          ** ERROR: X matrix has 1 tiny singular value -- collinearity
        The program will NOT continue past this type of error, unless
        the '-GOFORIT' option is used.  You should examine your results
        carefully to make sure they are reasonable (e.g., look at
        the fitted model overlay on the input time series).
    
    * Despite my best efforts, this program is somewhat slow.
        Partly because it solves many linear systems for each voxel,
        trying to find the 'best' ARMA(1,1) prewhitening matrix.
        However, a careful choice of algorithms for solving the linear
        systems (QR method, sparse matrix operations, etc.) and some
        other code optimizations should make running 3dREMLfit tolerable.
        Depending on the matrix and the options, you might expect CPU time
        to be about 2..5 times that of the corresponding 3dDeconvolve run.
        (Slower than that if you use '-slibase' and/or '-Grid 5', however.)
    
    ---------------------------------------------------------------
    How 3dREMLfit handles all zero columns in the regression matrix
    ---------------------------------------------------------------
    * One salient (to the user) difference from 3dDeconvolve is how
        3dREMLfit deals with the beta weight from an all zero column when
        computing a statistic (e.g., a GLT).  The beta weight will simply
        be ignored, and its entry in the GLT matrix will be set to zero.
        Any all zero rows in the GLT matrix are then removed.  For example,
        the 'Full_Fstat' for a model with 3 beta weights is computed from
        the GLT matrix [ 1 0 0 ]
                       [ 0 1 0 ]
                       [ 0 0 1 ].  If the last beta weight corresponds to
        an all zero column, then the matrix becomes [ 1 0 0 ]
                                                    [ 0 1 0 ]
                                                    [ 0 0 0 ], and then
        then last row is omitted.  This excision reduces the number of
        numerator degrees of freedom in this test from 3 to 2.  The net
        effect is that the F-statistic will be larger than in 3dDeconvolve,
        which does not modify the GLT matrix (or its equivalent).
    
     * A similar adjustment is made to denominator degrees of freedom, which
        is usually n-m, where n=# of data points and m=# of regressors.
        3dDeconvolve counts all zero regressors in with m, but 3dREMLfit
        does not.  The net effect is again to (slightly) increase F-statistic
        values over the equivalent 3dDeconvolve computation.
    
    -----------------------------------------------------------
    To Dream the Impossible Dream, to Write the Uncodeable Code
    -----------------------------------------------------------
    * Add options for -iresp/-sresp for -stim_times.
    * Prevent Daniel Glen from referring to this program as 3dARMAgeddon.
    * Establish incontrovertibly the nature of quantum mechanical observation.
    * Create an iPad version of the AFNI software suite.
    
    ----------------------------------------------------------
    * For more information, please see the contents of
        https://afni.nimh.nih.gov/pub/dist/doc/misc/3dREMLfit/
      which includes comparisons of 3dDeconvolve and 3dREMLfit
      activations (individual subject and group maps), and an
      outline of the mathematics implemented in this program.
    ----------------------------------------------------------
    
    ============================
    == RWCox - July-Sept 2008 ==
    ============================
    
     =========================================================================
    * This binary version of 3dREMLfit is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system.  You can control this value by setting environment variable
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
    * How many threads are useful?  That varies with the program, and how well
       it was coded.  You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 8.
    * The REML matrix setup and REML voxel ARMA(1,1) estimation loops are
       parallelized, across (a,b) parameter sets and across voxels, respectively.
    * The GLSQ and OLSQ loops are not parallelized. They are usually much
       faster than the REML voxel loop, and so I made no effort to speed
       these up (now and forever, two and inseparable).
    * '-usetemp' disables OpenMP multi-CPU usage, since the file I/O for
       saving and restoring various matrices and results is not easily
       parallelized.  To get OpenMP speedup for large problems (just where
       you want it), you'll need a lot of RAM.
     =========================================================================
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
