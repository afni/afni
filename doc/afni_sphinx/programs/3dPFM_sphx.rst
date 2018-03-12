*****
3dPFM
*****

.. _3dPFM:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dPFM [options]
    ------ 
    
    Brief summary:
    ==============
    * 3dPFM is a program that identifies brief BOLD events (order of sec) in fMRI time series
       without prior knowledge of their timing. 3dPFM deconvolves a hemodynamic response 
       function for each fMRI voxel and estimates the neuronal-related signal that generates
       the  BOLD events according to the linear haemodynamic model. In many ways, 
       the neuronal-related signal could be understood as the stimulus signal defined by the 
       experimental paradigm in a standard GLM approach, where the onsets 
       and duration of the experimental conditions are known a-priori. Alternatively, 
       3dPFM does not assume such information and estimates the signal underlying the 
       BOLD events with NO PARADIGM INFORMATION, i.e. PARADIGM FREE MAPPING (PFM). For instance,
       this algorithm can be useful to identify spontaneous BOLD events in resting-state
       fMRI data.
    
    * The ideas behind 3dPFM are described in
        C Caballero-Gaudes, N Petridou, ST Francis, IL Dryden, and PA Gowland.
        Paradigm Free Mapping with Sparse Regression Automatically detects Single-Trial
        Functional Magnetic Resonance Imaging Blood Oxygenation Level Dependent Responses.
        Human Brain Mapping, 34(3):501-18, 2013.
        http://dx.doi.org/10.1002/hbm.21452
    
    
    * For the deconvolution, 3dPFM assumes a linear convolution model and that 
      the neuronal-related signal is sparse in time, i.e. it has a non-zero amplitude 
      in a relatively small number of time points. How relative depends on the number 
      of time points of the signal, i.e. the length of the signal, a.k.a. scans, volumes. 
    
    * In many ways, the rationale of 3dPFM is very similar to 3dTfitter with the -FALTUNG 
      (deconvolution) option. Both programs differ in the manner the deconvolution
      is solved and several other relevant and interesting options.
      **** I would also recommend you to read 3dTfitter -help for useful tips *****
      ************* !!!  3dPFM is neither for the casual user !!!! ****************
    
    * IMPORTANT. This program is written in R. Please follow the guidelines in 
        
          https://afni.nimh.nih.gov/sscc/gangc/Rinstall.html
    
      to install R and make AFNI compatible with R. In addition, you need to install 
      the following libraries with dependencies:
      
       install.packages("abind",dependencies=TRUE)
       install.packages("MASS",dependencies=TRUE)
       install.packages("lars",dependencies=TRUE)
    
      You can find a demo on how to run this program in @Install_3dPFM_Demo
    
    
    A brief in deconvolution and regularization
    ===========================================
    Only for the non-casual user !!!:
    ===========================================
    
     The basic idea of 3dPFM is to assume that the time series at each voxel y(t) 
     is given by the linear convolution model (e.g., a linear haemodynamic model)
     
                    y(t) = sum { h(j) * s(t-j) }  +  e(t)
                           j>=0
     
     where h(t) is an user-supplied kernel function (e.g., haemodynamic response 
     function (HRF)), s(t) is the neuronal-related time series to be estimated, and e(t) is 
     a noise term capturing all noisy components of the signal. In matrix notation, 
     the convolution model can be "simply" written as
    
                               y =  H*s + e 
    
     where y, s and e are the input voxel, the neuronal-related and the error time series, 
     respectively, and H is a matrix with time-shifted versions of the kernel function 
     across columns. The convolution model is defined such that the size of H is N x N, 
     where N is the length of the input time series and, accordingly, the estimated 
     neuronal-related time series has the same length as the input time series. 
     
     Assuming that the noise is random and following a Gaussian distribution, a very 
     sensible way to estimate the time series s would be to minimize the sum of squares 
     of the residuals (RSS), a.k.a. L2fit, Least-Squares (LS) fit, and so forth, i.e. 
     
                        s*  =  min || y - H*s ||_2^2
                                s
    
     Unfortunately, in our problem the least squares solution tends to overfit the 
     input time series (i.e. the input time series tend to produce a perfect fit of the
     input signal including the noise) since the number of variables to estimate is 
     equal to the number of observations in the original time series. In addition, 
     since the columns of the convolution matrix H are highly correlated, the LS estimates 
     can become poorly determined and exhibit high variance. 
    
     One solution to these drawbacks is to impose a regularization term on (or penalization of) 
     the coefficient estimates based on prior information about the input signal. Typically, 
     regularization terms based on the Lp-norm of the estimates are used, such that the estimate
     of s is computed by solving
      
                 s*  =  min || y - H*s ||_2^2   subject to  || s ||_p <= λ 
                         s
    
     or, similarly,
    
                 s*  =  min  || s ||_p  subject to  || y - H*s ||_2^2 <= λ 
                         s
    
     or, using Lagrangian multipliers,
      
                 s*  =  min || y - H*s ||_2^2   +  λ || s ||_p 
                         s
    
     The three optimization problems are relatively equivalent, where λ is  
     a positive regularization parameter that balance the tradeoff between the term 
     of the residuals sum of squares (RSS) and the regularization or penalty term.
     Note: The value of λ in the Lagrangian formulation is not equal (i.e. does
     not have one-to-one correspondence) to the value of λ in the constrained problems. 
    
     The L1-norm (p = 1) is a convex, and widely studied, regularization term that promotes 
     sparse estimates. Relevant for fMRI data analysis, if BOLD responses were generated 
     by brief (on the fMRI time scale) bursts of neuronal activation, it could be assumed 
     that the neuronal-related time series s is a sparse vector with few coefficients 
     whose amplitude are significantly different from zero. In fact, this is typically assumed 
     in event-related fMRI experiments where we assume that one voxel responds to brief stimuli 
     in some, but not all, conditions. 
    
     In 3dPFM, two regularized estimation problems are currently implemented based on the L1-norm:
     
     * LASSO: The least absolute shrinkage and selection operator (LASSO) [Tibshirani, 1996], 
     which is equivalent to basis pursuit denoising (BPDN) [Chen et al., 1998]: 
     
                 s*  =  min || y - H*s ||_2^2   subject to  || s ||_1 <=  λ
                         s
    
     * DS: The Dantzig Selector [Candes and Tao, 2007]
      
                 s*  =  min || s ||_1   subject to || H^T (y - H*s) ||_infty <= λ
                         s
     where the L_infty (infinity-norm) refers to the maximum absolute value of a vector.
     In practice, minimizing the error term subject to a constraint in the norm is often 
     equivalent to minimizing the norm subject to a constraint in the error term, 
     with a one-to-one correspondence between the regularization parameters of both problems. 
     All in all, one can see that the main difference between the LASSO and the DS relates 
     to the error term. The LASSO considers the residual sum of squares (RSS), whereas 
     the DS considers the maximum correlation (in absolute value) of the residuals with 
     the model. Very intelligent minds have shown that there are very strong links 
     between the DS and the LASSO  (see Bickel et al., 2009 
     http://projecteuclid.org/euclid.aos/1245332830; and James et al., 2009
     http://dx.doi.org/10.1111/j.1467-9868.2008.00668.x for more information). 
     For lesser mortals, it is enough to know that the L_infty norm term in the DS is 
     equivalent to the differentiation of the RSS term with respect to s in the LASSO. 
     Actually, in practice the results of 3dPFM with the DS are usually very similar
     to the ones obtained with the LASSO (and viceversa). 
    
     Algorithms for solving the LASSO and DS
     ---------------------------------------
     3dPFM relies on homotopy continuation procedures to solve the above optimization
     problems. These procedures are very useful since they compute the complete 
     set of solutions of the problem for all possible regularization parameters.
     This is known as the regularization path. In particular, 3dPFM employs an R-version 
     of homotopy continuation algorithms for the DS (L1-homotopy) developed by Asif and Romberg 
     (see http://dx.doi.org/10.1109/CISS.2010.5464890), and the R-package LARS for the LASSO.  
    
     Choice of regularization parameter
     ----------------------------------
     Once the regularization path with all solutions is computed, what is the optimal one?
     i.e., what is the optimal regularization parameter λ ??. This is a very difficult question. 
     In fact, it is nearly impossible to select the optimal λ unless one is aware of 
     the optimal solution in advance (i.e. be the ORACLE) (but then we would not need to 
     estimate anymore!!!). In 3dPFM, the choice of the regularization parameter is done 
     based on model selection criteria that balance the degrees of freedom (df) that are 
     employed to fit the signal and the RSS relative to the number of observations. 
     For instance, when we use the Least Squares estimator to fit a general linear model 
     (GLM), as in 3dDeconvolve, the value of df is approximately equal to number of 
     regressors that we define in the model. So, here is the key question in 3dPFM: 
     If the convolution model used in 3dPFM (i.e. the matrix) has as many columns as 
     the number of observations, is not the degrees of freedom equal or higher than 
     the number of time points of the signal? The answer is NO for the L1-norm 
     regularization problems as the LASSO. 
     The trick is that an unbiased estimate of the degrees of freedom of the LASSO is 
     the number of non-zero coefficients of the LASSO estimate (for demonstration see 
     http://projecteuclid.org/euclid.aos/1194461726) if the matrix H is orthogonal. 
     Unfortunately, the matrix H in 3dPFM is not orthogonal and this result is not 
     completely accurate.  Yet, we consider it valid as it works quite nicely 
     in our application, i.e. counting the number of non-zero coefficients in the solution is 
     a very good approximation of the degrees of freedom. Moreover, 3dPFM also uses this 
     approximation for the Dantzig Selector due to the close link with the LASSO.  
    
     Therefore, the unbiased estimate of the degrees of freedom can be used to construct 
     model selection criteria to select the regularization parameter. Two different 
     criteria are implemented in 3dPFM: 
     
      * -bic: (Bayesian Information Criterion, equivalent to Minimum Description Length)
    
              λ*  =  min  N*log(|| y - H*s(λ) ||_2^2) + log(N)*df(λ)
                      λ
    
      * -aic: (Akaike Information Criterion)
    
              λ*  =  min  N*log(|| y - H*s(λ) ||_2^2) + 2*df(λ)
                      λ
     
     where s(λ) and df(λ) denote that the estimate and df depend on the regularization 
     parameter λ. 
    
     As shown in (Caballero-Gaudes et al. 2013), the bayesian information criterion (bic) 
     typically gives better results than the akaike information crition (aic). 
    
     If you want the 3dPFM ORACLE (i.e. the author of this program) to implement other 
     criteria, such as AICc, MDLc, please write him an email. 
     
    
     Option -nonzeros Q:
     Alternatively, one could also select the regularization parameter such that 
     the estimate only includes Q coefficients with non-zero amplitude, where Q 
     is an arbitrary number given as input. In statistics, the set of nonzero coeffients 
     for a given regularization parameter is defined as the active (or support) set. 
     A typical use of this option would be that we hypothesize that our signal 
     only includes Q nonzero coefficients (i.e. haemodynamic events of TR duration)
     but we do not know when they ocurr.
     
    
     IMPORTANT: If two successive events are non-zero, do both coeffients represent one or
     two events? Intuitively, one could think that both coefficients model a single event
     that spans several coefficients and, thus, requires several non-zero coefficients to
     to be properly modelled. This case is NOT considered in the program. 
     To deal with this situation, 3dPFM should have an option like "-nevents Q", 
     where Q is the number of events or successive non-zero coefficients. Unfortunately, 
     this cannot be easily defined. For instance, an estimate where all coefficients are 
     non-zero would represent a SINGLE event!!!
     If you think of a sensible manner to implement this option, please contact THE ORACLE.
    
     VERY IMPORTANT: In practice, the regularization path could include 2 different solutions 
     for 2 different regularization parameters but with equal number of non-zero coefficients!!! 
     This occurs because in the process of computing the regularization path for decreasing values
     of the regularization parameter (i.e. λ1 > λ2 > λ3), the number of elements in the active set 
     (i.e. the set of coefficients with non-zero amplitide) can increase or decrease. In fact, 
     the knots of the regularization path are the points where one element of the active set changes
     (i.e. it is removed or added to the active set) as λ decreases to zero. Consequently, the 
     active set could include Q non-zero elements for λ1, Q+1 for λ2 < λ1, and Q for λ3 < λ2. 
     In that case, the estimate given by 3dPFM is the solution for the largest regularization 
     parameter.
    
     CAREFUL!! use option -nonzeros at your own risk!!
     - Not all voxels show neuronal related BOLD events. 
     - These options are appropriate for ROI or VOI analyses where there is a clear hypothesis 
       that a given number of BOLD events should exist but we have no clue of their timing.
    
    ------------
     References:
    ------------
    
      If you find 3dPFM useful, the papers to cite are:
           
           C Caballero-Gaudes, N Petridou, ST Francis, IL Dryden, and PA Gowland.
           Paradigm Free Mapping with Sparse Regression Automatically detects Single-Trial
           Functional Magnetic Resonance Imaging Blood Oxygenation Level Dependent Responses.
           Human Brain Mapping, 34(3):501-18, 2013.
           http://dx.doi.org/10.1002/hbm.21452
           
           C Caballero-Gaudes, N Petridou, IL Dryden, L Bai, ST Francis and PA Gowland.
           Detection and characterization of single-trial fMRI bold responses: 
           Paradigm free mapping. Human Brain Mapping, 32(9):1400-18, 2011
           http://dx.doi.org/10.1002/hbm.21116.
    
      If you find 3dPFM very useful for the analysis of resting state data and finding invisible 
      sponteneous BOLD events, the paper to cite is:
           N Petridou, C Caballero-Gaudes, IL Dryden, ST Francis and PA Gowland
           Periods of rest in fMRI contain individual spontaneous events which 
           are related to slowly fluctuating spontaneous activity. Human Brain Mapping, 
           34(6):1319-29, 2013.
           http://dx.doi.org/10.1002/hbm.21513
    
      If you use the Dantzig Selector in 3dPFM and want to know more about the homotopy algorithm 
      for solving it, the paper to read (and cite) is:
          M Salman Asif and J Romberg, On the LASSO and Dantzig selector equivalence, 
          Conference on Information Sciences and Systems (CISS), Princeton, NJ, March 2010.
          http://dx.doi.org/10.1109/CISS.2010.5464890
          
      Finally, additional references for the LASSO and the Dantzig Selector are:
    
          R Tibshirani. Regression Shrinkage and Selection via the Lasso. Journal of 
          the Royal Statistical Society. Series B (Methodological), 58(1): 267-288, 1996.
          http://www.jstor.org/stable/2346178
    
          H Zou, T Hastie, R Tibshirani. On the “degrees of freedom” of the lasso. 
          Annals of Statistics 35(5): 2173--2192, 2007. 
          http://projecteuclid.org/euclid.aos/1194461726.
    
          B Efron, T Hastie, I. Johnstone, R Tibshirani. Least Angle Regression. 
          Annals of Statistics 32(2): 407–-499, 2004.
          http://projecteuclid.org/euclid.aos/1083178935
    
          E Candes and T. Tao. The Dantzig selector: Statistical estimation when p is 
          much larger than n. The Annals of Statistics 35(6):2313--2351, 2007.
          http://projecteuclid.org/euclid.aos/1201012958.
    
          M Salman Asif and J Romberg, On the LASSO and Dantzig selector equivalence, 
          Conference on Information Sciences and Systems (CISS), Princeton, NJ, March 2010.
          http://dx.doi.org/10.1109/CISS.2010.5464890
    
    ---------------------------------------------------------------------------------------
    
      Author: C. Caballero Gaudes, THE ORACLE (c.caballero@bcbl.eu) (May 1st, 2015)
    
      (many thanks to Z. Saad, R.W. Cox, J. Gonzalez-Castillo, G. Chen, and N. Petridou for neverending support) 
    
    
    
    
    Example usage:
    -----------------------------------------------------------------------------
          3dPFM       -input epi.nii      
                      -mask mask.nii      
                      -algorithm dantzig  
                      -criteria bic       
                      -LHS regparam.1D    
                      -hrf SPMG1          
                      -jobs 1             
                      -outALL yes
    
    
    
    Options:
    --------
    
       -input DSET1                       
          Specify the dataset to analyze with Paradigm Free Mapping (3dPFM).
          It can be any of the formats available in AFNI. 
              e.g: -input Data+orig                       
          Also .1D files where each column is a voxel timecourse.
          If an .1D file is input, you MUST specify the TR with option -TR.
    
       -mask MASK: Process voxels inside this mask only. Default is no masking.
    
       -algorithm ALG:  Regularization (a.k.a. penalty) function used for HRF deconvolution. 
          * Available options for ALG are: 
              dantzig:  Dantzig Selector (default) 
              lasso:    LASSO                      
          * If you want other options, contact with the ORACLE (c.caballero@bcbl.eu). 
    
       -criteria CRIT:  Model selection criterion for HRF deconvolution. 
          * Available options are: 
              BIC:  Bayesian Information Criterion 
              AIC:  Akaike Information Criterion 
          * Default is BIC since it tends to produce more accurate deconvolution (see 3dPFM paper).
          * If you want other options, write to the ORACLE. 
          * This option is incompatible with -nonzeros. 
    
       -nonzeros XX:                                                                         
          * Choose the estimate of the regularization path with XX nonzero coefficients      
            as the output of the deconvolution.                                              
          * Since the regularization path could have several estimates with identical        
            number of nonzero coefficients, the program will choose the first one in the     
            regularization path, i.e. the solution with the largest regularization parameter.
          * This option is incompatible with -criteria. 
          * This option is not used by default.
    
       -maxiter MaxIter:                                                                   
          * Maximum number of iterations in the homotopy procedure (absolute value).     
          * Setting up MaxIter < 1 might be useful to speed up the program, e.g.         
            with the option -nonzeros Q, MaxIter = 2*Q is reasonable (default)           
    
       -maxiterfactor MaxIterFactor:                 
          * Maximum number of iterations in the homotopy procedure is relative to       
            the number of volumes of the input time series, i.e.  MaxIterFactor*nscans, 
          * Default value is MaxIterFactor = 1                                          
                                                                                        
      MaxIter OR MaxIterFactor                                                  
     --------------------------                                                 
       * If both MaxIterFactor and MaxIter are given for any mistaken reason,   
         the program will STOP. It only admits one of the two options.          
       * If none of them is given, the number of iterations is equal to nscans. 
       * The homotopy procedure adds or removes one coefficient from the active 
         set of non-zero coefficients in the estimate in each iteration.        
       * If you expect Q non-zero coefficients in the deconvolved time-series,  
         a reasonable choice is MaxIter = 2*Q  (default with -nonzero Q)        
       * If you want to speed up the program, choose MaxIterfactor = 1 or  0.5. 
    
       -TR tr:  Repetition time or sampling period of the input data           
          * It is required for the generation of the deconvolution HRF model.  
          * If input dataset is .1D file, TR must be specified in seconds.     
            If TR is not given, the program will STOP.                         
          * If input dataset is a 3D+time volume and tr is NOT given,          
            the value of TR is taken from the dataset header.                  
          * If TR is specified and it is different from the TR in the header   
            of the input dataset, the program will STOP.                       
            I am not sure know why you want to do that!!!                      
            but if you want, first change the TR of the input with 3drefit.    
    
       -hrf fhrf:   haemodynamic response function used for deconvolution       
          *  Since July 2015, fhrf can be any of the HRF models available in 3dDeconvolve. 
             Check https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dDeconvolve.html 
          *  I.e. 3dPFM calls 3dDeconvolve with the -x1D_stop and -nodata options 
             to create the HRF with onset at 0 (i.e. -stim_time 1 '1D:0' fhrf )   
          *  [Default] fhrf == 'GAM', the 1 parameter gamma variate  
                          (t/(p*q))^p * exp(p-t/q)                              
                         with p=8.6 q=0.547 if only 'GAM' is used               
                      ** The peak of 'GAM(p,q)' is at time p*q after            
                         the stimulus.  The FWHM is about 2.3*sqrt(p)*q.        
          *  Another option is fhrf == 'SPMG1', the SPM canonical HRF.          
                                                                                
          *  If fhrf is a .1D, the program will use it as the HRF model.        
                 ** It should be generated with the same TR as the input data   
                    to get sensible results (i.e. know what you are doing).     
                 ** fhrf must be column or row vector, i.e. only 1 hrf allowed. 
                    In future, this option might be changed to model the hrf as 
                    a linear combination of functions.                          
          * The HRF is normalized to maximum absolute amplitude equal to 1.     
    
       -hrf_vol hrf_DSET:   3D+time dataset with voxel/nodes/vertex -dependent HRFs.      
          * The grid and TR of hrf_DSET must be the same as the input dataset.          
          * This dataset can be the output of -iresp option in 3dDeconvolve,  which     
            contains the estimated HRF (a.k.a. impulse responses) for a given stimuli.  
          * In 3dPFM, the HRF response is assumed constant during the acquisition.      
          * See also -idx_hrf, an interesting option to use voxel dependent HRFs.       
    
       -idx_hrf idx_hrf_DSET:   3D dataset with voxel-dependent indexes that indicate        
           which column of the .1D file in option -hrf should be used for each voxel.      
          * Of course, the grid of idx_hrf_DSET must be the same as the input dataset.     
          * The number of HRFs in option -hrf must be <= maximum index in idx_hrf_DSET.    
            Otherwise, the program will STOP before starting any calculation.              
          * Only positive integers > 0 are allowed in this option.                         
          * For instance, this dataset can be created by clustering (e.g. with 3dKmeans)   
            the estimated HRF genereted with option -iresp in 3dDeconvolve.                
          * In 3dPFM, the HRF response is assumed constant during the acquisition          
          * An index equal to 1 will select the first column of the .1D fhrf,              
            which is usually column 0 in AFNI nomenclature.                                
    
       -LHS lset:                                     
          Options: file.1D or functional dataset(s) 
          * Additional regressors that will be fitted to the data after deconvolution.
          * Usually, these will be nuisance regressors that explain some variability  
          of the data, e.g. the realignment parameters estimated with 3dVolreg.
          * More than one 'lset' can follow the '-LHS' option and it can be any of the AFNI formats.
          * Each 'lset' can be a 3D+time dataset or a 1D file with 1 or more columns.
          * A 3D+time dataset defines one column in the LHS matrix.
                ++ If input is a 1D file, then you cannot input a 3D+time         
                    dataset with '-LHS'.                                           
                ++ If input is a 3D+time dataset, then the LHS 3D+time dataset(s)  
                   must have the same voxel grid as the input.                     
          * A 1D file will include all its columns in the LHS matrix.              
                ++ For example, you could input the LHS matrix from the            
                   .xmat.1D file output by 3dDeconvolve, if you wanted             
                   to repeat the same linear regression using 3dPFM.               
          * Columns are assembled in the order given on the command line,         
                  which means that LHS parameters will be output in that order!    
                                                                                   
         NOTE: These notes are ALMOST a copy of the -LHS option in 3dTfitter and     
               they are replicated here for simplicity and because it is difficult 
               to do it better !!                                                  
    
       -jobs NJOBS: On a multi-processor machine, parallel computing will speed
                  up the program significantly.                              
                  Choose 1 for a single-processor computer (DEFAULT).        
    
       -nSeg XX: Divide into nSeg segments of voxels to report progress,        
               e.g. nSeg 5 will report every 20% of proccesed voxels.         
               Default = 10                                                   
    
       -verb VERB: VERB is an integer specifying verbosity level.
                 0 for quiet, 1 (default) or more: talkative.
    
       -help: this help message
    
       -beta          Prefix for the neuronal-related (i.e. deconvolved) time series.  
                      It wil have the same length as the input time series.            
                      This volume is always saved with default name 'PFM' if not given.
                      ++ If you don't want this time series (why?), set it to NULL.    
                         This is another similarity with 3dTfitter.                    
    
       -betafitts     Prefix of the convolved neuronal-related time series.          
                      It wil have the same length as the input time series             
                      Default = NULL, which means that the program will not save it.   
    
       -fitts         Prefix for the fitted time series.                             
                      Default = NULL, although it's recommendable to save it           
                      to check the fit of the model to the data.                       
    
       -resid         Prefix for the residuals of the fit to the data.                
                      Default = NULL.                                                   
                      It could also be computed as input - ffitts with 3dcalc.          
    
       -mean          Prefix for the intercept of the model                           
                      Default = NULL.                                                   
    
       -LHSest        Prefix for the estimates of the LHS parameters.                 
                      Default = NULL.                                                   
    
       -LHSfitts      Prefix for the fitted time series of the LHS parameters.        
                      Default = NULL.                                                   
    
       -lambda        Prefix for output volume with the regularization parameter      
                      of the deconvolution of each voxel.                               
                      Default = NULL.                                                   
    
       -costs        Prefix for output volume of the cost function used to select the      
                      regularization parameter according to the selected criteria.       
                      Default = NULL.                                                    
                                                                                         
                                                                                         
      Output volumes of T-stats, F-stats and Z-stats                                     
      ==============================================                                     
    
       -Tstats_beta   Prefix for the T-statistics of beta at each time point         
                      according to a linear model including the nonzero coefficients   
                      of the deconvolved signal, plus LHS regressors and intercept     
                      It wil have the same length as the input time series             
                      Recommendation: Use -Tdf_beta too!!                              
                      Default = NULL.                                                  
    
       -Tdf_beta      Prefix for degrees of freedom of the T-statistics of beta.     
                      Useful if you want to check Tstats_beta since different voxels   
                      might have different degrees of freedom.                         
                      Default = NULL.                                                  
    
       -Z_Tstats_beta Prefix for (normalized) z-scores of the T-statistics of beta.  
                      Recommendable option to visualize the results instead of         
                      Tstats_beta and Tdf_beta since (again) different voxels          
                      might be fitted with different degrees of freedom.               
                      Default = NULL. 
    
       -Fstats_beta   Prefix for the F-statistics of the deconvolved component.      
                      Recommendation: Use -Fdf_beta too!! for the very same reasons.   
                      Default = NULL. 
    
       -Fdf_beta      Prefix for degrees of freedom of Fstats_beta.                  
                      Useful to check Fstats_beta for the very same reasons.           
                      Default = NULL.                                                  
    
       -Z_Fstats_beta Prefix for (normalized) z-scores of the Fstats_beta.           
                      Recomendable option instead of Fstats_beta and Fdf_beta.         
                      Default = NULL. 
    
       -Tstats_LHS    Prefix for T-statistics of LHS regressors at each time point.  
                      It wil have the same length as the total number of LHS regressors.
                      Recommendation: Use -Tdf_LHS too!!                                
                      Default = NULL. 
    
       -Tdf_LHS       Prefix for degrees of freedom of the Tstats_LHS.               
                      Useful if you want to check Tstats_LHS since different voxels    
                      might have different degrees of freedom.                         
                      Default = NULL. 
    
       -Z_Tstats_LHS  Prefix for (normalized) z-scores of the Tstats_LHS.           
                      Recommendable option instead of Tstats_LHS and Tdf_LHS.         
                      Default = NULL.                                                 
    
       -Fstats_LHS    Prefix for the F-statistics of the LHS regressors.            
                      Recommendation: Use -Fdf_LHS too!!                              
                      Default = NULL.                                                 
    
       -Fdf_LHS       Prefix for degrees of freedom of the Fstats_LHS.             
                      Default = NULL.                                                
    
       -Z_Fstats_LHS  Prefix for (normalized) z-scores of Fstats_LHS.              
                      Recommendable option instead of Fstats_LHS and Fdf_LHS.        
                      Default = NULL.                                                
    
       -Fstats_full   Prefix for the F-statistics of the full (deconvolved) model.
                      Default = NULL.                                               
    
       -Fdf_full      Prefix for the degrees of freedom of the Fstats_full.      
                      Default = NULL.                                              
    
       -Z_Fstats_full Prefix for (normalized) z-scores of Fstats_full.          
                      Default = NULL.                                             
    
       -R2_full       Prefix for R^2 (i.e. coefficient of determination) of the full model.
                      Default = NULL. 
    
       -R2adj_full    Prefix for Adjusted R^2 coefficient of the full model.   
                      Default = NULL. 
    
       -outALL suffix                                                                    
          * If -outALL is used, the program will save ALL output volumes.              
          * The names of the output volumes will be automatically generated as         
            outputname_suffix_input, e.g. if -input = TheEmperor+orig, and suffix is Zhark, 
            the names of the volumes will be beta_Zhark_TheEmperor+orig for -beta option,
            betafitts_Zhark_TheEmperor+orig for -betafitts option, and so forth.        
          * If suffix = 'yes', then no suffix will be used and the names will be just  
            outputname_input, i.e. beta_TheEmperor+orig.                               
          * If you want to specify a given name for an output volume, you must define  
            the name of the output volume in the options above. The program will use it 
            instead of the name automatically generated.
                     Default = NULL. 
    
       -outZAll suffix                                                                  
          * If -outZAll is used, the program will save ALMOST ALL output volumes.     
          * Similar to -outALL, but the program will only save the Z_Tstats_* and Z_Fstats_* volumes  
            i.e. it will not save the Tstats_*, Tdf_*, Fstats_* and Fdf_* volumes. 
          * This option is incompatible with -outALL. The program will STOP if both options are given.
                    Default = NULL. 
    
       -show_allowed_options: list of allowed options
