.. _ahelp_3dDeconvolve:

************
3dDeconvolve
************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    ------------------------------------------------------------------------
    -----                 DESCRIPTION and PROLEGOMENON                 -----
    ------------------------------------------------------------------------
    Program to calculate the deconvolution of a measurement 3D+time dataset 
    with a specified input stimulus time series.  This program can also     
    perform multiple linear regression using multiple input stimulus time   
    series. Output consists of an AFNI 'bucket' type dataset containing     
    (for each voxel)                                                        
     * the least squares estimates of the linear regression coefficients    
     * t-statistics for significance of the coefficients                    
     * partial F-statistics for significance of individual input stimuli    
     * the F-statistic for significance of the overall regression model     
    The program can optionally output extra datasets containing             
     * the estimated impulse response function                              
     * the fitted model and error (residual) time series                    
    ------------------------------------------------------------------------
    * Program 3dDeconvolve does Ordinary Least Squares (OLSQ) regression.   
    * Program 3dREMLfit can be used to do Generalized Least Squares (GLSQ)  
      regression (AKA 'pre-whitened' least squares) combined with REML      
      estimation of an ARMA(1,1) temporal correlation structure:            
       https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dREMLfit.html   
    * The input to 3dREMLfit is the .xmat.1D matrix file output by          
      3dDeconvolve, which also writes a 3dREMLfit command line to a file    
      to make it relatively easy to use the latter program.                 
    * 3dREMLfit also allows for voxel-specific regressors, unlike           
      3dDeconvolve. This feature is used with the '-fanaticor' option       
      to afni_proc.py, for example.                                         
    * Nonlinear time series model fitting can be done with program 3dNLfim: 
       https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dNLfim.html     
    * Preprocessing of the time series input can be done with various AFNI  
      programs, or with the 'uber-script' afni_proc.py:                     
       https://afni.nimh.nih.gov/pub/dist/doc/program_help/afni_proc.py.html
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    ****  The recommended way to use 3dDeconvolve is via afni_proc.py,  ****
    ****  which will pre-process the data, and also provide some useful ****
    ****  diagnostic tools/outputs for assessing the data's quality.    ****
    ****  It can also run 3dREMLfit for you 'at no extra charge'.       ****
    ****  [However, it will not wax your car or wash your windows.]     ****
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    Consider the time series model  Z(t) = K(t)*S(t) + baseline + noise,    
    where Z(t) = data                                                       
          K(t) = kernel (e.g., hemodynamic response function or HRF)        
          S(t) = stimulus time series                                       
      baseline = constant, drift, etc. [regressors of no interest]          
         and * = convolution                                                
    Then 3dDeconvolve solves for K(t) given S(t).  If you want to process   
    the reverse problem and solve for S(t) given the kernel K(t), use the   
    program 3dTfitter with the '-FALTUNG' option.  The difference between   
    the two cases is that K(t) is presumed to be causal and have limited    
    support, whereas S(t) is a full-length time series.  Note that program  
    3dTfitter does not have all the capabilities of 3dDeconvolve for        
    calculating output statistics; on the other hand, 3dTfitter can solve   
    a deconvolution problem (in either direction) with L1 or L2 regression, 
    and with sign constraints on the computed values (e.g., requiring that  
    the output S(t) or K(t) be non-negative):                               
     https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dTfitter.html     
    ------------------------------------------------------------------------
    The 'baseline model' in 3dDeconvolve (and 3dREMLfit) does not mean just 
    a constant (mean) level of the signal, or even just the slow drifts that
    happen in FMRI time series.  'Baseline' here also means the model that  
    forms the null hypothesis.  The Full_Fstat result is the F-statistic    
    of the full model (all regressors) vs. the baseline model.  Thus, it    
    it common to include irregular time series, such as estimated motion    
    parameters, in the baseline model via the -stim_file/-stim_base options,
    or by using the -ortvec option (to include multiple regressors at once).
    Thus, the 'baseline model' is really the 'null hypothesis model'.       
    ------------------------------------------------------------------------
    It is VERY important to realize that statistics (F, t, R^2) computed in 
    3dDeconvolve are MARGINAL (or partial) statistics.  For example, the    
    t-statistic for a single beta coefficient measures the significance of  
    that beta value against the regression model where ONLY that one column 
    of the matrix is removed; that is, the null hypothesis for that         
    t-statistic is the full regression model minus just that single         
    regressor.  Similarly, the F-statistic for a set of regressors measures 
    the significance of that set of regressors (eg, a set of TENT functions)
    against the full model with just that set of regressors removed.  If    
    this explanation or its consequences are unclear, you need to consult   
    with a statistician, or with the AFNI message board guru entities       
    (when they can be lured down from the peak of Mt Taniquetil or Kailash).
    ------------------------------------------------------------------------
    Regression Programs in the AFNI Package:                                
    * At its core, 3dDeconvolve solves a linear regression problem z = X b  
      for the parameter vector b, given the data vector z in each voxel, and
      given the SAME matrix X in each voxel.  The solution is calculated in 
      the Ordinary Least Squares (OLSQ) sense.                              
    * Program 3dREMLfit does something similar, but allows for ARMA(1,1)    
      serial correlation in the data, so the solution method is called      
      Generalized Least Squares (GLSQ).                                     
    * If you want to solve a problem where some of the matrix columns in X  
      (the regressors) are different in different voxels (spatially variable),
      then use program 3dTfitter, which uses OLSQ, or used 3dREMLfit.       
    * 3dTfitter can also use L1 and LASSO regression, instead of OLSQ; if you
      want to use such 'robust' fitting methods, this program is your friend.
      It can also impose sign constraints (positivity or negativity) on the 
      parameters b, and can (as mentioned above) do deconvolution.          
    * 3dBandpass and 3dTproject can do a sequence of 'time series cleanup'  
      operations, including 'regressing out' (via OLSQ) a set of nuisance   
      vectors (columns).                                                    
    * 3dLSS can be used to solve -stim_times_IM systems using an alternative
      linear technique that gives biased results, but with smaller variance.
    ------------------------------------------------------------------------
    
    Usage Details:                                                         
    3dDeconvolve command-line-arguments ...
                                                                           
    **** Input data and control options ****                               
    
    -input fname         fname = filename of 3D+time input dataset         
                           [more than  one filename  can  be  given]       
                           [here,   and  these  datasets  will   be]       
                           [auto-catenated in time; if you do this,]       
                           ['-concat' is not needed and is ignored.]       
                      ** You can input a 1D time series file here,         
                         but the time axis should run along the            
                         ROW direction, not the COLUMN direction as        
                         in the -input1D option.  You can automatically    
                         transpose a 1D file on input using the \'        
                         operator at the end of the filename, as in        
                           -input fred.1D\'                               
                       * This is the only way to use 3dDeconvolve          
                         with a multi-column 1D time series file.          
                       * The output datasets by default will then          
                         be in 1D format themselves.  To have them         
                         formatted as AFNI datasets instead, use           
                           -DAFNI_WRITE_1D_AS_PREFIX=YES                   
                         on the command line.                              
                       * You should use '-force_TR' to set the TR of       
                         the 1D 'dataset' if you use '-input' rather       
                         than '-input1D' [the default is 1.0 sec].         
    
    -sat OR -trans     * 3dDeconvolve can check the dataset time series    
                         for initial saturation transients, which should   
                         normally have been excised before data analysis.  
                         If you want to have it do this somewhat time      
                         consuming check, use the option '-sat'.           
                       * Or set environment variable AFNI_SKIP_SATCHECK to NO.
                       * Program 3dSatCheck does this check, also.         
    
    [-noblock]           Normally, if you input multiple datasets with     
                         '-input', then the separate datasets are taken to 
                         be separate image runs that get separate baseline 
                         models.  If you want to have the program consider 
                         these to be all one big run, use -noblock.        
                       * If any of the input dataset has only 1 sub-brick, 
                         then this option is automatically invoked!        
                       * If the auto-catenation feature isn't used, then   
                         this option has no effect, no how, no way.        
    
    [-force_TR TR]       Use this value of TR instead of the one in        
                         the -input dataset.                               
                         (It's better to fix the input using 3drefit.)     
    
    [-input1D dname]     dname = filename of single (fMRI) .1D time series 
                                 where time run downs the column.          
    
    [-TR_1D tr1d]        tr1d = TR for .1D time series [default 1.0 sec].  
                         This option has no effect without -input1D        
    
    [-nodata [NT [TR]]   Evaluate experimental design only (no input data) 
                       * Optional, but highly recommended: follow the      
                         '-nodata' with two numbers, NT=number of time     
                         points, and TR=time spacing between points (sec)  
    
    [-mask mname]        mname = filename of 3D mask dataset               
                          Only data time series from within the mask       
                          will be analyzed; results for voxels outside     
                          the mask will be set to zero.                    
    
    [-automask]          Build a mask automatically from input data        
                          (will be slow for long time series datasets)     
                      ** If you don't specify ANY mask, the program will   
                          build one automatically (from each voxel's RMS)  
                          and use this mask solely for the purpose of      
                          reporting truncation-to-short errors (if '-short'
                          is used) AND for computing the FDR curves in the 
                          bucket dataset's header (unless '-noFDR' is used,
                          of course).                                      
                       * If you don't want the FDR curves to be computed   
                          inside this automatically generated mask, then   
                          use '-noFDR' and later run '3drefit -addFDR' on  
                          the bucket dataset.                              
                       * To be precise, the above default masking only     
                          happens when you use '-input' to run the program 
                          with a 3D+time dataset; not with '-input1D'.     
    
    [-STATmask sname]    Build a mask from file 'sname', and use this      
                           mask for the purpose of reporting truncation-to 
                           float issues AND for computing the FDR curves.  
                           The actual results ARE not masked with this     
                           option (only with '-mask' or '-automask' options)
                           * If you don't use '-STATmask', then the mask   
                             from '-mask' or '-automask' is used for these 
                             purposes.  If neither of those is given, then 
                             the automatically generated mask described    
                             just above is used for these purposes.        
    
    [-censor cname]      cname = filename of censor .1D time series        
                       * This is a file of 1s and 0s, indicating which     
                         time points are to be included (1) and which are  
                         to be excluded (0).                               
                       * Option '-censor' can only be used once!           
                       * The option below may be simpler to use!           
    
    [-CENSORTR clist]    clist = list of strings that specify time indexes 
                           to be removed from the analysis.  Each string is
                           of one of the following forms:                  
                               37 => remove global time index #37          
                             2:37 => remove time index #37 in run #2       
                           37..47 => remove global time indexes #37-47     
                           37-47  => same as above                         
                         2:37..47 => remove time indexes #37-47 in run #2  
                         *:0-2    => remove time indexes #0-2 in all runs  
                          +Time indexes within each run start at 0.        
                          +Run indexes start at 1 (just be to confusing).  
                          +Multiple -CENSORTR options may be used, or      
                            multiple -CENSORTR strings can be given at     
                            once, separated by spaces or commas.           
                          +N.B.: 2:37,47 means index #37 in run #2 and     
                            global time index 47; it does NOT mean         
                            index #37 in run #2 AND index #47 in run #2.   
    
    [-concat rname]      rname = filename for list of concatenated runs    
                          * 'rname' can be in the format                   
                              '1D: 0 100 200 300'                          
                            which indicates 4 runs, the first of which     
                            starts at time index=0, second at index=100,   
                            and so on.                                     
    
    [-nfirst fnum]       fnum = number of first dataset image to use in the
                           deconvolution procedure. [default = max maxlag] 
    
    [-nlast  lnum]       lnum = number of last dataset image to use in the 
                           deconvolution procedure. [default = last point] 
    
    [-polort pnum]       pnum = degree of polynomial corresponding to the  
                           null hypothesis  [default: pnum = 1]            
                        ** For pnum > 2, this type of baseline detrending  
                           is roughly equivalent to a highpass filter      
                           with a cutoff of (p-2)/D Hz, where 'D' is the   
                           duration of the imaging run: D = N*TR           
                        ** If you use 'A' for pnum, the program will       
                           automatically choose a value based on the       
                           time duration D of the longest run:             
                             pnum = 1 + int(D/150)                         
                    ==>>** 3dDeconvolve is the ONLY AFNI program with the  
                           -polort option that allows the use of 'A' to    
                           set the polynomial order automatically!!!       
                        ** Use '-1' for pnum to specifically NOT include   
                           any polynomials in the baseline model.  Only    
                           do this if you know what this means!            
    
    [-legendre]          use Legendre polynomials for null hypothesis      
                           (baseline model)                                
    
    [-nolegendre]        use power polynomials for null hypotheses         
                           [default is -legendre]                          
                        ** Don't do this unless you are crazy!             
    
    [-nodmbase]          don't de-mean baseline time series                
                           (i.e., polort>0 and -stim_base inputs)          
    [-dmbase]            de-mean baseline time series [default if polort>=0]
    
    [-svd]               Use SVD instead of Gaussian elimination [default] 
    [-nosvd]             Use Gaussian elimination instead of SVD           
                           (only use for testing + backwards compatibility)
    
    [-rmsmin r]          r = minimum rms error to reject reduced model     
                           (default = 0; don't use this option normally!)  
    
    [-nocond]            DON'T calculate matrix condition number           
                          ** This value is NOT the same as Matlab!         
    
    [-singvals]          Print out the matrix singular values              
                          (useful for some testing/debugging purposes)     
                          Also see program 1dsvd.                          
    
    [-GOFORIT [g]]       Use this to proceed even if the matrix has        
                         bad problems (e.g., duplicate columns, large      
                         condition number, etc.).                          
                   *N.B.: Warnings that you should particularly heed have  
                          the string '!!' somewhere in their text.         
                   *N.B.: Error and Warning messages go to stderr and      
                          also to file 3dDeconvolve.err.               
                          ++ You can disable the creation of this .err     
                             file by setting environment variable          
                             AFNI_USE_ERROR_FILE to NO before running      
                             this program.                                 
                   *N.B.: The optional number 'g' that appears is the      
                          number of warnings that can be ignored.          
                          That is, if you use -GOFORIT 7 and 9 '!!'        
                          matrix warnings appear, then the program will    
                          not run.  If 'g' is not present, 1 is used.      
    
    [-allzero_OK]        Don't consider all zero matrix columns to be      
                          the type of error that -GOFORIT is needed to     
                          ignore.                                          
                         * Please know what you are doing when you use     
                           this option!                                    
    
    [-Dname=val]       = Set environment variable 'name' to 'val' for this 
                         run of the program only.                          
                                                                           
    ******* Input stimulus options *******                                 
                                                                           
    -num_stimts num      num = number of input stimulus time series        
                           (0 <= num)   [default: num = 0]                 
                   *N.B.: '-num_stimts' must come before any of the        
                          following '-stim' options!                       
                   *N.B.: Most '-stim' options have as their first argument
                          an integer 'k', ranging from 1..num, indicating  
                          which stimulus class the argument is defining.   
                   *N.B.: The purpose of requiring this option is to make  
                          sure your model is complete -- that is, you say  
                          you are giving 5 '-stim' options, and then the   
                          program makes sure that all of them are given    
                          -- that is, that you don't forget something.     
                                                                           
    -stim_file k sname   sname = filename of kth time series input stimulus
                   *N.B.: This option directly inserts a column into the   
                          regression matrix; unless you are using the 'old'
                          method of deconvolution (cf below), you would    
                          normally only use '-stim_file' to insert baseline
                          model components such as motion parameters.      
                                                                           
    [-stim_label k slabel] slabel = label for kth input stimulus           
                   *N.B.: This option is highly recommended, so that       
                          output sub-bricks will be labeled for ease of    
                          recognition when you view them in the AFNI GUI.  
                                                                           
    [-stim_base k]       kth input stimulus is part of the baseline model  
                   *N.B.: 'Baseline model' == Null Hypothesis model        
                   *N.B.: The most common baseline components to add are   
                          the 6 estimated motion parameters from 3dvolreg. 
    
    -ortvec fff lll      This option lets you input a rectangular array    
                         of 1 or more baseline vectors from file 'fff',    
                         which will get the label 'lll'.  Functionally,    
                         it is the same as using '-stim_file' on each      
                         column of 'fff' separately (plus '-stim_base').   
                         This method is just a faster and simpler way to   
                         include a lot of baseline regressors in one step. 
              -->>**N.B.: This file is NOT included in the '-num_stimts'   
                          count that you provide.                          
                   *N.B.: These regression matrix columns appear LAST      
                          in the matrix, after everything else.            
                   *N.B.: You can use column '[..]' and/or row '{..}'      
                          selectors on the filename 'fff' to pick out      
                          a subset of the numbers in that file.            
                   *N.B.: The q-th column of 'fff' will get a label        
                          like 'lll[q]' in the 3dDeconvolve results.       
                   *N.B.: This option is known as the 'Inati Option'.      
                   *N.B.: Unlike the original 'Inati' (who is unique), it  
                          is allowed to have more than one '-ortvec' option.
                   *N.B.: Program 1dBport is one place to generate a file  
                          for use with '-ortvec'; 1deval might be another. 
    
    **N.B.: You must have -num_stimts > 0  AND/OR                          
            You must use  -ortvec          AND/OR                          
            You must have -polort >= 0                                     
            Otherwise, there is no regression model!                       
            An example using -polort only:                                 
     3dDeconvolve -x1D_stop -polort A -nodata 300 2 -x1D stdout: | 1dplot -one -stdin
    
    **N.B.: The following 3 options are for the 'old' style of explicit    
            deconvolution.  For most purposes, their usage is no longer    
            recommended.  Instead, you should use the '-stim_times' options
            to directly input the stimulus times, rather than code the     
            stimuli as a sequence of 0s and 1s in this 'old' method!       
    
    [-stim_minlag k m]   m = minimum time lag for kth input stimulus       
                           [default: m = 0]                                
    [-stim_maxlag k n]   n = maximum time lag for kth input stimulus       
                           [default: n = 0]                                
    [-stim_nptr k p]     p = number of stimulus function points per TR     
                           Note: This option requires 0 slice offset times 
                           [default: p = 1]                                
                                                                           
    **N.B.: The '-stim_times' options below are the recommended way of     
            analyzing FMRI time series data now.  The options directly     
            above are only maintained for the sake of backwards            
            compatibility!  For most FMRI users, the 'BLOCK' and 'TENT'    
            (or 'CSPLIN') response models will serve their needs.  The     
            other models are for users with specific needs who understand  
            clearly what they are doing.                                   
                                                                           
    [-stim_times k tname Rmodel]                                           
       Generate the k-th response model from a set of stimulus times       
       given in file 'tname'.                                              
        *** The format of file 'tname' is one line per imaging run         
            (cf. '-concat' above), and each line contains the list of START
            times (in seconds) for the stimuli in class 'k' for its        
            corresponding run of data; times are relative to the start of  
            the run (i.e., sub-brick #0 occurring at time=0).              
        *** The DURATION of the stimulus is encoded in the 'Rmodel'        
            argument, described below. Units are in seconds, not TRs!      
            -- If different stimuli in the same class 'k' have different   
               durations, you'll have to use the dmBLOCK response model    
               and '-stim_times_AM1' or '-stim_times_AM2', described below.
        *** Different lines in the 'tname' file can contain different      
            numbers of start times.  Each line must contain at least 1 time.
        *** If there is no stimulus in class 'k' in a particular imaging   
            run, there are two ways to indicate that:                      
              (a) put a single '*' on the line, or                         
              (b) put a very large number or a negative number             
                  (e.g., 99999, or -1) on the line                         
                  -- times outside the range of the imaging run will cause 
                     a warning message, but the program will soldier on.   
        *** In the case where the stimulus doesn't actually exist in the   
            data model (e.g., every line in 'tname' is a '*'), you will    
            also have to use the '-allzero_OK' option to force 3dDeconvolve
            to run with regressor matrix columns that are filled with zeros.
                                                                           
       The response model is specified by the third argument after         
       '-stim_times' ('Rmodel'), and can be one of the following:          
        *** In the descriptions below, a '1 parameter' model has a fixed   
            shape, and only the estimated amplitude ('Coef') varies:       
              BLOCK GAM TWOGAM SPMG1 WAV MION                              
        *** Models with more than 1 parameter have multiple basis          
            functions, and the estimated parameters ('Coef') are their     
            amplitudes. The estimated shape of the response to a stimulus  
            will be different in different voxels:                         
              TENT CSPLIN SPMG2 SPMG3 POLY SIN EXPR                        
        *** Many models require the input of the start and stop times for  
            the response, 'b' and 'c'.  Normally, 'b' would be zero, but   
            in some cases, 'b' could be negative -- for example, if you    
            are concerned about anticipatory effects.  The stop time 'c'   
            should be based on how long you realistically expect the       
            hemodynamic response to last after the onset of the stimulus;  
            e.g., the duration of the stimulus plus 14 seconds for BOLD.   
        *** If you use '-tout', each parameter will get a separate         
            t-statistic.  As mentioned far above, this is a marginal       
            statistic, measuring the impact of that model component on the 
            regression fit, relative to the fit with that one component    
            (matrix column) removed.                                       
        *** If you use '-fout', each stimulus will also get an F-statistic,
            which is the collective impact of all the model components     
            it contains, relative to the regression fit with the entire    
            stimulus removed. (If there is only 1 parameter, then F = t*t.)
        *** Some models below are described in terms of a simple response  
            function that is then convolved with a square wave whose       
            duration is a parameter you give (duration is NOT a parameter  
            that will be estimated).  Read the descriptions below carefully:
            not all functions are (or can be) convolved in this way:       
             * ALWAYS convolved:      BLOCK  dmBLOCK  MION  MIONN          
             * OPTIONALLY convolved:  GAM    TWOGAM   SPMGx WAV            
             * NEVER convolved:       TENT   CSPLIN   POLY  SIN   EXPR     
            Convolution is specified by providing the duration parameter   
            as described below for each particular model function.         
    
         'BLOCK(d,p)'  = 1 parameter block stimulus of duration 'd'        
                        ** There are 2 variants of BLOCK:                  
                             BLOCK4 [the default] and BLOCK5               
                           which have slightly different delays:           
                             HRF(t) = int( g(t-s) , s=0..min(t,d) )        
                           where g(t) = t^q * exp(-t) /(q^q*exp(-q))       
                           and q = 4 or 5.  The case q=5 is delayed by     
                           about 1 second from the case q=4.               
                    ==> ** Despite the name, you can use 'BLOCK' for event-
                           related analyses just by setting the duration to
                           a small value; e.g., 'BLOCK5(1,1)'              
                        ** The 'p' parameter is the amplitude of the       
                           basis function, and should usually be set to 1. 
                           If 'p' is omitted, the amplitude will depend on 
                           the duration 'd', which is useful only in       
                           special circumstances!!                         
                        ** For bad historical reasons, the peak amplitude  
                           'BLOCK' without the 'p' parameter does not go to
                           1 as the duration 'd' gets large.  Correcting   
                           this oversight would break some people's lives, 
                           so that's just the way it is.                   
                        ** The 'UBLOCK' function (U for Unit) is the same  
                           as the 'BLOCK' function except that when the    
                           'p' parameter is missing (or 0), the peak       
                           amplitude goes to 1 as the duration gets large. 
                           If p > 0, 'UBLOCK(d,p)' and 'BLOCK(d,p)' are    
                           identical.                                      
    
         'TENT(b,c,n)' = n parameter tent function expansion from times    
                           b..c after stimulus time [piecewise linear]     
                           [n must be at least 2; time step is (c-b)/(n-1)]
        'CSPLIN(b,c,n)'= n parameter cubic spline function expansion       
                           from times b..c after stimulus time             
                           [n must be at least 4]                          
                         ** CSPLIN is a drop-in upgrade of TENT to a       
                            differentiable set of functions.               
                         ** TENT and CSPLIN are 'cardinal' interpolation   
                            functions: their parameters are the values     
                            of the HRF model at the n 'knot' points        
                              b , b+dt , b+2*dt , ... [dt = (c-b)/(n-1)]   
                            In contrast, in a model such as POLY or SIN,   
                            the parameters output are not directly the     
                            hemodynamic response function values at any    
                            particular point.                              
                     ==> ** You can also use 'TENTzero' and 'CSPLINzero',  
                            which means to eliminate the first and last    
                            basis functions from each set.  The effect     
                            of these omissions is to force the deconvolved 
                            HRF to be zero at t=b and t=c (to start and    
                            and end at zero response).  With these 'zero'  
                            response models, there are n-2 parameters      
                            (thus for 'TENTzero', n must be at least 3).   
                         ** These 'zero' functions will force the HRF to   
                            be continuous, since they will now be unable   
                            to suddenly rise up from 0 at t=b and/or drop  
                            down to 0 at t=c.                              
    
         'GAM(p,q)'    = 1 parameter gamma variate                         
                             (t/(p*q))^p * exp(p-t/q)                      
                           Defaults: p=8.6 q=0.547 if only 'GAM' is used   
                         ** The peak of 'GAM(p,q)' is at time p*q after    
                            the stimulus.  The FWHM is about 2.35*sqrt(p)*q;
                            this approximation is accurate for p > 0.3*q.  
                         ** To check this approximation, try the command   
                   1deval -num 100 -del 0.02 -xzero 0.02   \
                          -expr 'sqrt(gamp(x,1))/2.35/x' | \
                   1dplot -stdin -del 0.02 -xzero 0.02 -yaxis 1:1.4:4:10   
                            If the two functions gamp(x,1) and 2.35*x      
                            were equal, the plot would be constant y=1.    
                     ==> ** If you add a third argument 'd', then the GAM  
                            function is convolved with a square wave of    
                            duration 'd' seconds; for example:             
                              'GAM(8.6,.547,17)'                           
                            for a 17 second stimulus.  [09 Aug 2010]       
         'GAMpw(K,W)'  = Same as 'GAM(p,q)' but where the shape parameters 
                           are specified at time to peak 'K' and full      
                           width at half max (FWHM) 'W'. You can also      
                           add a third argument as the duration. The (K,W) 
                           parameters are converted to (p,q) values for    
                           the actual computations; the (p,q) parameters   
                           are printed to the text (stderr) output.        
                         ** Note that if you give weird values for K and W,
                            weird things will happen: (tcsh syntax)        
                             set pp = `ccalc 'gamp(2,8)'`                  
                             set qq = `ccalc 'gamq(2,8)'`                  
                             1deval -p=$pp -q=$qq -num 200 -del 0.1  \
                                    -expr '(t/p/q)^p*exp(p-t/q)'   | \
                                    1dplot -stdin -del 0.1                 
                            Here, K is significantly smaller than W,       
                            so a gamma variate that fits peak=2 width=8    
                            must be weirdly shaped. [Also note use of the  
                            'calc' functions gamp(K,W) and gamq(K,W) to    
                            calculate p and q from K and W in the script.] 
    
         'TWOGAM(p1,q1,r,p2,q2)'                                           
                       = 1 parameter (amplitude) model:                    
                       = A combination of two 'GAM' functions:             
                             GAM(p1,q1) - r*GAM(p2,q2)                     
                           This model is intended to let you use a HRF     
                           similar to BrainVoyager (e.g.). You can         
                           add a sixth argument as the duration.           
                         ** Note that a positive 'r' parameter means to    
                            subtract the second GAM function (undershoot). 
         'TWOGAMpw(K1,W1,r,K2,W2)'                                         
                       = Same as above, but where the peaks and widths     
                           of the 2 component gamma variates are given     
                           instead of the less intuitive p and q.          
                           For FMRI work, K2 > K1 is usual, as the         
                           second (subtracted) function is intended        
                           to model the 'undershoot' after the main        
                           positive part of the model. You can also        
                           add a sixth argument as the duration.           
                         ** Example (no duration given):                   
            3dDeconvolve -num_stimts 1 -polort -1 -nodata 81 0.5         \
                         -stim_times 1 '1D: 0' 'TWOGAMpw(3,6,0.2,10,12)' \
                         -x1D stdout: | 1dplot -stdin -THICK -del 0.5      
    
         'SPMG1'       = 1 parameter SPM gamma variate basis function      
                             exp(-t)*(A1*t^P1-A2*t^P2) where               
                           A1 = 0.0083333333  P1 = 5  (main positive lobe) 
                           A2 = 1.274527e-13  P2 = 15 (undershoot part)    
                           This function is NOT normalized to have peak=1! 
         'SPMG2'       = 2 parameter SPM: gamma variate + d/dt derivative  
                           [For backward compatibility: 'SPMG' == 'SPMG2'] 
         'SPMG3'       = 3 parameter SPM basis function set                
                     ==> ** The SPMGx functions now can take an optional   
                            (duration) argument, specifying that the primal
                            SPM basis functions should be convolved with   
                            a square wave 'duration' seconds long and then 
                            be normalized to have peak absolute value = 1; 
                            e.g., 'SPMG3(20)' for a 20 second duration with
                            three basis function.  [28 Apr 2009]           
                         ** Note that 'SPMG1(0)' will produce the usual    
                            'SPMG1' wavefunction shape, but normalized to  
                            have peak value = 1 (for example).             
    
         'POLY(b,c,n)' = n parameter Legendre polynomial expansion         
                           from times b..c after stimulus time             
                           [n can range from 1 (constant) to 20]           
    
         'SIN(b,c,n)'  = n parameter sine series expansion                 
                           from times b..c after stimulus time             
                           [n must be at least 1]                          
    
         'WAV(d)'      = 1 parameter block stimulus of duration 'd'.       
                          * This is the '-WAV' function from program waver!
                          * If you wish to set the shape parameters of the 
                            WAV function, you can do that by adding extra  
                            arguments, in the order                        
                             delay time , rise time , fall time ,          
                             undershoot fraction, undershoot restore time  
                          * The default values are 'WAV(d,2,4,6,0.2,2)'    
                          * Omitted parameters get the default values.     
                          * 'WAV(d,,,,0)' (setting undershoot=0) is        
                            very similar to 'BLOCK5(d,1)', for d > 0.      
                          * Setting duration d to 0 (or just using 'WAV')  
                            gives the pure '-WAV' impulse response function
                            from waver.                                    
                          * If d > 0, the WAV(0) function is convolved with
                            a square wave of duration d to make the HRF,   
                            and the amplitude is scaled back down to 1.    
    
         'EXPR(b,c) exp1 ... expn'                                         
                       = n parameter; arbitrary expressions from times     
                         b..c after stimulus time                          
                          * Expressions are separated by spaces, so        
                            each expression must be a contiguous block     
                            of non-whitespace characters                   
                          * Expressions use the same format as 3dcalc      
                          * Symbols that can be used in an expression:     
                             t = time in sec since stimulus time           
                             x = time scaled to be x= 0..1 for t=bot..top  
                             z = time scaled to be z=-1..1 for t=bot..top  
                          * Spatially dependent regressors are not allowed!
                          * Other symbols are set to 0 (silently).         
                     ==> ** There is no convolution of the 'EXPR' functions
                            with a square wave implied.  The expressions   
                            you input are what you get, evaluated over     
                            times b..c after each stimulus time.  To be    
                            sure of what your response model is, you should
                            plot the relevant columns from the matrix      
                            .xmat.1D output file.                          
    
         'MION(d)'     = 1 parameter block stimulus of duration 'd',       
                         intended to model the response of MION.           
                         The zero-duration impulse response 'MION(0)' is   
                           h(t) = 16.4486 * ( -0.184/ 1.5 * exp(-t/ 1.5)   
                                              +0.330/ 4.5 * exp(-t/ 4.5)   
                                              +0.670/13.5 * exp(-t/13.5) ) 
                         which is adapted from the paper                   
                          FP Leite, et al. NeuroImage 16:283-294 (2002)    
                          http://dx.doi.org/10.1006/nimg.2002.1110         
                      ** Note that this is a positive function, but MION   
                         produces a negative response to activation, so the
                         beta and t-statistic for MION are usually negative.
                   ***** If you want a negative MION function (so you get  
                         a positive beta), use the name 'MIONN' instead.   
                      ** After convolution with a square wave 'd' seconds  
                         long, the resulting single-trial waveform is      
                         scaled to have magnitude 1.  For example, try     
                         this fun command to compare BLOCK and MION:       
                   3dDeconvolve -nodata 300 1 -polort -1 -num_stimts 2   \
                                -stim_times 1 '1D: 10 150' 'MION(70)'    \
                                -stim_times 2 '1D: 10 150' 'BLOCK(70,1)' \
                                -x1D stdout: | 1dplot -stdin -one -thick   
                         You will see that the MION curve rises and falls  
                         much more slowly than the BLOCK curve.            
                  ==> ** Note that 'MION(d)' is already convolved with a   
                         square wave of duration 'd' seconds.  Do not      
                         convolve it again by putting in multiple closely  
                         spaced stimulus times (this mistake has been made)!
                      ** Scaling the single-trial waveform to have magnitude
                         1 means that trials with different durations 'd'  
                         will have the same magnitude for their regression 
                         models.                                           
                                                                           
     * 3dDeconvolve does LINEAR regression, so the model parameters are    
       amplitudes of the basis functions; 1 parameter models are 'simple'  
       regression, where the shape of the impulse response function is     
       fixed and only the magnitude/amplitude varies.  Models with more    
       free parameters have 'variable' shape impulse response functions.   
    
     * LINEAR regression means that each data time series (thought of as   
       a single column of numbers = a vector) is fitted to a sum of the    
       matrix columns, each one multiplied by an amplitude parameter to    
       be calculated ('Coef'). The purpose of the various options          
         '-stim_times', '-polort', '-ortvec', and/or '-stim_file'          
       is to build the columns of the regression matrix.                   
                                                                           
     * If you want NONLINEAR regression, see program 3dNLfim.              
                                                                           
     * If you want LINEAR regression with allowance for non-white noise,   
       use program 3dREMLfit, after using 3dDeconvolve to set up the       
       regression model (in the form of a matrix file).                    
                                                                           
    ** When in any doubt about the shape of the response model you are   **
    *  asking for, you should plot the relevant columns from the X matrix *
    *  to help develop some understanding of the analysis.  The 'MION'    *
    *  example above can be used as a starting point for how to easily    *
    *  setup a quick command pipeline to graph response models.  In that  *
    *  example, '-polort -1' is used to suppress the usual baseline model *
    *  since graphing that part of the matrix would just be confusing.    *
    *  Another example, for example, comparing the similar models         *
    ** 'WAV(10)', 'BLOCK4(10,1)', and 'SPMG1(10)':                       **
                                                                           
         3dDeconvolve -nodata 100 1.0 -num_stimts 3 -polort -1   \
                      -local_times -x1D stdout:                  \
                      -stim_times 1 '1D: 10 60' 'WAV(10)'        \
                      -stim_times 2 '1D: 10 60' 'BLOCK4(10,1)'   \
                      -stim_times 3 '1D: 10 60' 'SPMG1(10)'      \
          | 1dplot -thick -one -stdin -xlabel Time -ynames WAV BLOCK4 SPMG1
                                                                           
     * For the format of the 'tname' file, see the last part of            
     https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/DeconSummer2004.html 
       and also see the other documents stored in the directory below:     
     https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/                     
       and also read the presentation below:                               
     https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni05_regression.pdf
      ** Note Well:                                                        
       * The contents of the 'tname' file are NOT just 0s and 1s,          
         but are the actual times of the stimulus events IN SECONDS.       
       * You can give the times on the command line by using a string      
         of the form '1D: 3.2 7.9 | 8.2 16.2 23.7' in place of 'tname',    
         where the '|' character indicates the start of a new line         
         (so this example is for a case with 2 catenated runs).            
    => * You CANNOT USE the '1D:' form of input for any of the more        
         complicated '-stim_times_*' options below!!                       
       * The '1D:' form of input is mostly useful for quick tests, as      
         in the examples above, rather than for production analyses with   
         lots of different stimulus times and multiple imaging runs.       
                                                                           
    [-stim_times_AM1 k tname Rmodel]                                       
       Similar, but generates an amplitude modulated response model.       
       The 'tname' file should consist of 'time*amplitude' pairs.          
       As in '-stim_times', the '*' character can be used as a placeholder 
       when an imaging run doesn't have any stimulus of a given class.     
       *N.B.: What I call 'amplitude' modulation is called 'parametric'    
              modulation in Some other PrograM.                            
     ***N.B.: If NO run at all has a stimulus of a given class, then you   
              must have at least 1 time that is not '*' for -stim_times_*  
              to work (so that the proper number of regressors can be set  
              up).  You can use a negative time for this purpose, which    
              will produce a warning message but otherwise will be         
              ignored, as in:                                              
                 -1*37                                                     
                 *                                                         
              for a 2 run 'tname' file to be used with -stim_times_*.      
           ** In such a case, you will also need the -allzero_OK option,   
              and probably -GOFORIT as well.                               
    
    [-stim_times_AM2 k tname Rmodel]                                       
       Similar, but generates 2 response models: one with the mean         
       amplitude and one with the differences from the mean.               
      *** Please note that 'AM2' is the option you should probably use!    
      *** 'AM1' is for special cases, and normally should not be used      
          for FMRI task activation analyses!!                              
      *** 'AM2' will give you the ability to detect voxels that activate   
          but do not change proportional to the amplitude factor, as well  
          as provide a direct measure of the proportionality of the        
          activation to changes in the input amplitude factors.  'AM1'     
          will do neither of these things.                                 
      *** Normally, 3dDeconvolve removes the mean of the auxiliary         
          parameter(s) from the modulated regressor(s).  However, if you   
          set environment variable AFNI_3dDeconvolve_rawAM2 to YES, then   
          the mean will NOT be removed from the auxiliary parameter(s).    
          This ability is provided for users who want to center their      
          parameters using their own method.                               
      *** [12 Jul 2012] You can now specify the value to subtract from     
          each modulation parameter -- this value will replace the         
          subtraction of the average parameter value that usually happens. 
          To do this, add an extra parameter after the option, as in       
            -stim_times_AM2 1 timesAM.1D 'BLOCK(2,1)' :5.2:x:2.0           
          The extra argument must start with the colon ':' character, and  
          there should be as many different values (separated by ':') as   
          there are parameters in the timing file (timesAM.1D above).      
      ==> In the example above, ':5.2:x:2.0' means                         
            subtract 5.2 from each value of the first parameter in timesAM.1D
            subtract the MEAN from each value of the second parameter      
              (since 'x' doesn't translate to a number)                    
            subtract 2.0 from each value of the third parameter            
      ==> What is this option for, anyway?  The purpose is to facilitate   
          GROUP analysis the results from a collection of subjects, where  
          you want to treat each subject's analysis exactly the same       
          way -- and thus, the subtraction value for a parameter (e.g.,    
          reaction time) should then be the mean over all the reaction     
          times from all trials in all subjects.                           
                                                                           
    ** NOTE [04 Dec 2008] **                                               
     -stim_times_AM1 and -stim_times_AM2 now take files with more          
       than 1 amplitude attached to each time; for example,                
         33.7*9,-2,3                                                       
       indicates a stimulus at time 33.7 seconds with 3 amplitudes         
       attached (9 and -2 and 3).  In this example, -stim_times_AM2 would  
       generate 4 response models: 1 for the constant response case        
       and 1 scaled by each of the amplitude sets.                         
       ** Please don't carried away and use too many parameters!! **       
     For more information on modulated regression, see                     
       https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/AMregression.pdf   
                                                                           
    ** NOTE [08 Dec 2008] **                                               
     -stim_times_AM1 and -stim_times_AM2 now have 1 extra response model   
     function available:                                                   
       dmBLOCK (or dmBLOCK4 or dmBLOCK5)                                   
     where 'dm' means 'duration modulated'.  If you use this response      
     model, then the LAST married parameter in the timing file will        
     be used to modulate the duration of the block stimulus.  Any          
     earlier parameters will be used to modulate the amplitude,            
     and should be separated from the duration parameter by a ':'          
     character, as in '30*5,3:12' which means (for dmBLOCK):               
       a block starting at 30 s,                                           
       with amplitude modulation parameters 5 and 3,                       
       and with duration 12 s.                                             
     The unmodulated peak response of dmBLOCK depends on the duration      
     of the stimulus, as the BOLD response accumulates.                    
     If you want the peak response to be a set to a fixed value, use       
       dmBLOCK(p)                                                          
     where p = the desired peak value (e.g., 1).                           
     *** Understand what you doing when you use dmBLOCK, and look at  ***  
     *** the regression matrix!  Otherwise, you will end up confused. ***  
     *N.B.: The maximum allowed dmBLOCK duration is 999 s.                 
     *N.B.: You cannot use '-iresp' or '-sresp' with dmBLOCK!              
     *N.B.: If you are NOT doing amplitude modulation at the same time     
            (and so you only have 1 'married' parameter per time), use     
            '-stim_times_AM1' with dmBLOCK.  If you also want to do        
            amplitude modulation at the same time as duration modulation   
            (and so you have 2 or more parameters with each time), use     
            '-stim_times_AM2' instead.  If you use '-stim_times_AM2' and   
            there is only 1 'married' parameter, the program will print    
            a warning message, then convert to '-stim_times_AM1', and      
            continue -- so nothing bad will happen to your analysis!       
            (But you will be embarassed in front of your friends.)         
     *N.B.: If you are using AM2 (amplitude modulation) with dmBLOCK, you  
            might want to use 'dmBLOCK(1)' to make each block have native  
            amplitude 1 before it is scaled by the amplitude parameter.    
            Or maybe not -- this is a matter for fine judgment.            
     *N.B.: You can also use dmBLOCK with -stim_times_IM, in which case    
            each time in the 'tname' file should have just ONE extra       
            parameter -- the duration -- married to it, as in '30:15',     
            meaning a block of duration 15 seconds starting at t=30 s.     
     *N.B.: For bad historical reasons, the peak amplitude dmBLOCK without 
            the 'p' parameter does not go to 1 as the duration gets large. 
            Correcting this oversight would break some people's lives, so  
            that's just the way it is.                                     
     *N.B.: The 'dmUBLOCK' function (U for Unit) is the same as the        
            'dmBLOCK' function except that when the 'p' parameter is       
            missing (or 0), the peak amplitude goes to 1 as the duration   
            gets large.  If p > 0, 'dmUBLOCK(p)' and 'dmBLOCK(p)' are      
            identical                                                      
     For some graphs of what dmBLOCK regressors look like, see             
       https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/AMregression.pdf   
     and/or try the following command:                                     
        3dDeconvolve -nodata 350 1 -polort -1 -num_stimts 1 \
                     -stim_times_AM1 1 q.1D 'dmBLOCK'       \
                     -x1D stdout: | 1dplot -stdin -thick -thick            
     where file q.1D contains the single line                              
       10:1 40:2 70:3 100:4 130:5 160:6 190:7 220:8 250:9 280:30           
     Change 'dmBLOCK' to 'dmBLOCK(1)' and see how the matrix plot changes. 
                                                                           
     **************** Further notes on dmBLOCK [Nov 2013] **************** 
                                                                           
     Basically (IMHO), there are 2 rational choices to use:                
                                                                           
       (a) 'dmUBLOCK' = allow the amplitude of the response model to       
                        vary with the duration of the stimulus; getting    
                        larger with larger durations; for durations longer 
                        than about 15s, the amplitude will become 1.       
                   -->> This choice is equivalent to 'dmUBLOCK(0)', but    
                        is NOT equivalent to 'dmBLOCK(0)' due to the       
                        historical scaling issue alluded to above.         
                                                                           
       (b) 'dmUBLOCK(1)' = all response models will get amplitude 1,       
                           no matter what the duration of the stimulus.    
                      -->> This choice is equivalent to 'dmBLOCK(1)'.      
                                                                           
     Some users have expressed the desire to allow the amplitude to        
     vary with duration, as in case (a), BUT to specify the duration       
     at which the amplitude goes to 1.  This desideratum has now been      
     implemented, and provides the case below:                             
                                                                           
       (a1) 'dmUBLOCK(-X)' = set the amplitude to be 1 for a duration      
                             of 'X' seconds; e.g., 'dmBLOCK(-5)' means     
                             that a stimulus with duration 5 gets          
                             amplitude 1, shorter durations get amplitudes 
                             smaller than 1, and longer durations get      
                             amplitudes larger than 1.                     
                        -->> Please note that 'dmBLOCK(-X)' is NOT the     
                             same as this case (a1), and in fact it        
                             has no meaning.                               
                                                                           
     I hope this clarifies things and makes your life simpler, happier,    
     and more carefree. (If not, please blame Gang Chen, not me.)          
                                                                           
     An example to clarify the difference between these cases:             
        3dDeconvolve -nodata 350 1 -polort -1 -num_stimts 3 \
                     -stim_times_AM1 1 q.1D 'dmUBLOCK'      \
                     -stim_times_AM1 2 q.1D 'dmUBLOCK(1)'   \
                     -stim_times_AM1 3 q.1D 'dmUBLOCK(-4)'  \
                     -x1D stdout: |                         \
         1dplot -stdin -thick                               \
                -ynames 'dmUBLOCK' 'dmUB(1)' 'dmUB(-4)'                    
     where file q.1D contains the single line                              
       10:1 60:2 110:4 160:10 210:20 260:30                                
     Note how the 'dmUBLOCK(-4)' curve (green) peaks at 1 for the 3rd      
     stimulus, and peaks at larger values for the later (longer) blocks.   
     Whereas the 'dmUBLOCK' curve (black) peaks at 1 at only the longest   
     blocks, and the 'dmUBLOCK(1)' curve (red) peaks at 1 for ALL blocks.  
     ********************************************************************* 
                                                                           
    [-stim_times_FSL k tname Rmodel]                                       
       This option allows you to input FSL-style 3-column timing files,    
       where each line corresponds to one stimulus event/block; the        
       line '40 20 1' means 'stimulus starts at 40 seconds, lasts for      
       20 seconds, and is given amplitude 1'.  Since in this format,       
       each stimulus can have a different duration and get a different     
       response amplitude, the 'Rmodel' must be one of the 'dm'            
       duration-modulated options above ['dmUBLOCK(1)' is probably the     
       most useful].  The amplitude modulation is taken to be like         
       '-stim_times_AM1', where the given amplitude in the 'tname' file    
       multiplies the basic response shape.                                
     *** We DO NOT advocate the use of this '_FSL' option, but it's here   
         to make some scripting easier for some (unfortunate) people.      
     *** The results of 3dDeconvolve (or 3dREMLfit) cannot be expected     
         to be exactly the same as FSL FEAT, since the response model      
         shapes are different, among myriad other details.                 
     *** You can also use '-stim_times_FS1' to indicate that the           
         amplitude factor in the 'tname' file should be ignored and        
         replaced with '1' in all cases.                                   
     *** FSL FEAT only analyzes contiguous time series -- nothing like     
         '-concat' allowing for multiple EPI runs is possible in FSL       
         (AFAIK).  So the FSL stimulus time format doesn't allow for       
         this possibility.  In 3dDeconvolve, you can get around this       
         problem by using a line consisting of '* * *' to indicate the     
         break between runs, as in the example below:                      
             1 2 3                                                         
             4 5 6                                                         
             * * *                                                         
             7 8 9                                                         
         that indicates 2 runs, the first of which has 2 stimuli and       
         the second of which has just 1 stimulus.  If there is a run       
         that has NO copies of this type of stimulus, then you would       
         use two '* * *' lines in succession.                              
         Of course, a file using the '* * *' construction will NOT be      
         compatible with FSL!                                              
                                                                           
    [-stim_times_IM k tname Rmodel]                                        
       Similar, but each separate time in 'tname' will get a separate      
       regressor; 'IM' means 'Individually Modulated' -- that is, each     
       event will get its own amplitude estimated.  Presumably you will    
       collect these many amplitudes afterwards and do some sort of        
       statistics or analysis on them.                                     
     *N.B.: Each time in the 'tname' file will get a separate regressor.   
            If some time is outside the duration of the imaging run(s),    
            or if the response model for that time happens to hit only     
            censored-out data values, then the corresponding regressor     
            will be all zeros.  Normally, 3dDeconvolve will not run        
            if the matrix has any all zero columns.  To carry out the      
            analysis, use the '-allzero_OK' option.  Amplitude estimates   
            for all zero columns will be zero, and should be excluded      
            from any subsequent analysis.  (Probably you should fix the    
            times in the 'tname' file instead of using '-allzero_OK'.)     
                                                                           
    [-global_times]                                                        
    [-local_times]                                                         
       By default, 3dDeconvolve guesses whether the times in the 'tname'   
       files for the various '-stim_times' options are global times        
       (relative to the start of run #1) or local times (relative to       
       the start of each run).  With one of these options, you can force   
       the times to be considered as global or local for '-stim_times'     
       options that are AFTER the '-local_times' or '-global_times'.       
     ** Using one of these options (most commonly, '-local_times') is      
        VERY highly recommended.                                           
                                                                           
    [-stim_times_millisec]                                                 
     This option scales all the times in any '-stim_times_*' option by     
     0.001; the purpose is to allow you to input the times in ms instead   
     of in s.  This factor will be applied to ALL '-stim_times' inputs,    
     before or after this option on the command line.  This factor will    
     be applied before -stim_times_subtract, so the subtraction value      
     (if present) must be given in seconds, NOT milliseconds!              
                                                                           
    [-stim_times_subtract SS]                                              
     This option means to subtract 'SS' seconds from each time encountered 
     in any '-stim_times*' option.  The purpose of this option is to make  
     it simple to adjust timing files for the removal of images from the   
     start of each imaging run.  Note that this option will be useful      
     only if both of the following are true:                               
      (a) each imaging run has exactly the same number of images removed   
      (b) the times in the 'tname' files were not already adjusted for     
          these image removal (i.e., the times refer to the image runs     
          as acquired, not as input to 3dDeconvolve).                      
     In other words, use this option with understanding and care!          
     ** Note that the subtraction of 'SS' applies to ALL '-stim_times'     
        inputs, before or after this option on the command line!           
     ** And it applies to global times and local times alike!              
     ** Any time (thus subtracted) below 0 will be ignored, as falling     
        before the start of the imaging run.                               
     ** This option, and the previous one, are simply for convenience, to  
        help you in setting up your '-stim_times*' timing files from       
        whatever source you get them.                                      
                                                                           
    [-basis_normall a]                                                     
       Normalize all basis functions for '-stim_times' to have             
       amplitude 'a' (must have a > 0).  The peak absolute value           
       of each basis function will be scaled to be 'a'.                    
       NOTES:                                                              
        * -basis_normall only affect -stim_times options that              
            appear LATER on the command line                               
        * The main use for this option is for use with the                 
            'EXPR' basis functions.                                        
                                                                           
    ******* General linear test (GLT) options *******                      
                                                                           
    -num_glt num         num = number of general linear tests (GLTs)       
                           (0 <= num)   [default: num = 0]                 
                      **N.B.: You only need this option if you have        
                              more than 10 GLTs specified; the program     
                              has built-in space for 10 GLTs, and          
                              this option is used to expand that space.    
                              If you use this option, you should place     
                              it on the command line BEFORE any of the     
                              other GLT options.                           
    [-glt s gltname]     Perform s simultaneous linear tests, as specified 
                           by the matrix contained in file 'gltname'       
    [-glt_label k glabel]  glabel = label for kth general linear test      
    [-gltsym gltname]    Read the GLT with symbolic names from the file    
                           'gltname'; see the document below for details:  
      https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/DeconSummer2004.html
                                                                           
    ******* Options to create 3D+time datasets *******                     
                                                                           
    [-iresp k iprefix]   iprefix = prefix of 3D+time output dataset which  
                           will contain the kth estimated impulse response 
    [-tshift]            Use cubic spline interpolation to time shift the  
                           estimated impulse response function, in order to
                           correct for differences in slice acquisition    
                           times. Note that this effects only the 3D+time  
                           output dataset generated by the -iresp option.  
                 **N.B.: This option only applies to the 'old' style of    
                         deconvolution analysis.  Do not use this with     
                         -stim_times analyses!                             
    [-sresp k sprefix]   sprefix = prefix of 3D+time output dataset which  
                           will contain the standard deviations of the     
                           kth impulse response function parameters        
    [-fitts  fprefix]    fprefix = prefix of 3D+time output dataset which  
                           will contain the (full model) time series fit   
                           to the input data                               
    [-errts  eprefix]    eprefix = prefix of 3D+time output dataset which  
                           will contain the residual error time series     
                           from the full model fit to the input data       
    [-TR_times dt]                                                         
       Use 'dt' as the stepsize for output of -iresp and -sresp file       
       for response models generated by '-stim_times' options.             
       Default is same as time spacing in the '-input' 3D+time dataset.    
       The units here are in seconds!                                      
                                                                           
    **** Options to control the contents of the output bucket dataset **** 
                                                                           
    [-fout]            Flag to output the F-statistics for each stimulus   
                        ** F tests the null hypothesis that each and every 
                           beta coefficient in the stimulus set is zero    
                        ** If there is only 1 stimulus class, then its     
                           '-fout' value is redundant with the Full_Fstat  
                           computed for all stimulus coefficients together.
    [-rout]            Flag to output the R^2 statistics                   
    [-tout]            Flag to output the t-statistics                     
                        ** t tests a single beta coefficient against zero  
                        ** If a stimulus class has only one regressor, then
                           F = t^2 and the F statistic is redundant with t.
    [-vout]            Flag to output the sample variance (MSE) map        
    [-nobout]          Flag to suppress output of baseline coefficients    
                         (and associated statistics) [** DEFAULT **]       
    [-bout]            Flag to turn on output of baseline coefs and stats. 
                        ** Will make the output dataset larger.            
    [-nocout]          Flag to suppress output of regression coefficients  
                         (and associated statistics)                       
                        ** Useful if you just want GLT results.            
    [-full_first]      Flag to specify that the full model statistics will 
                         be first in the bucket dataset [** DEFAULT **]    
    [-nofull_first]    Flag to specify that full model statistics go last  
    [-nofullf_atall]   Flag to turn off the full model F statistic         
                         ** DEFAULT: the full F is always computed, even if
                         sub-model partial F's are not ordered with -fout. 
    [-bucket bprefix]  Create one AFNI 'bucket' dataset containing various 
                         parameters of interest, such as the estimated IRF 
                         coefficients, and full model fit statistics.      
                         Output 'bucket' dataset is written to bprefix.    
    [-nobucket]        Don't output a bucket dataset.  By default, the     
                         program uses '-bucket Decon' if you don't give    
                         either -bucket or -nobucket on the command line.  
    [-noFDR]           Don't compute the statistic-vs-FDR curves for the   
                         bucket dataset.                                   
                         [same as 'setenv AFNI_AUTOMATIC_FDR NO']          
                                                                           
    [-xsave]           Flag to save X matrix into file bprefix.xsave       
                         (only works if -bucket option is also given)      
    [-noxsave]         Don't save X matrix [this is the default]           
    [-cbucket cprefix] Save the regression coefficients (no statistics)    
                         into a dataset named 'cprefix'.  This dataset     
                         will be used in a -xrestore run instead of the    
                         bucket dataset, if possible.                      
                    ** Also, the -cbucket and -x1D output can be combined  
                         in 3dSynthesize to produce 3D+time datasets that  
                         are derived from subsets of the regression model  
                         [generalizing the -fitts option, which produces]  
                         [a 3D+time dataset derived from the full model].  
                                                                           
    [-xrestore f.xsave] Restore the X matrix, etc. from a previous run     
                         that was saved into file 'f.xsave'.  You can      
                         then carry out new -glt tests.  When -xrestore    
                         is used, most other command line options are      
                         ignored.                                          
                                                                           
    [-float]            Write output datasets in float format, instead of  
                        as scaled shorts [** now the default **]           
    [-short]            Write output as scaled shorts [no longer default]  
                                                                           
    ***** The following options control miscellanous outputs *****         
                                                                           
    [-quiet]             Flag to suppress most screen output               
    [-xout]              Flag to write X and inv(X'X) matrices to screen   
    [-xjpeg filename]    Write a JPEG file graphing the X matrix           
                         * If filename ends in '.png', a PNG file is output
    [-x1D filename]      Save X matrix to a .xmat.1D (ASCII) file [default]
                        ** If 'filename' is 'stdout:', the file is written 
                           to standard output, and could be piped into     
                           1dplot (some examples are given earlier).       
                         * This can be used for quick checks to see if your
                           inputs are setting up a 'reasonable' matrix.    
    [-nox1D]             Don't save X matrix [a very bad idea]             
    [-x1D_uncensored ff] Save X matrix to a .xmat.1D file, but WITHOUT     
                         ANY CENSORING.  Might be useful in 3dSynthesize.  
    [-x1D_stop]          Stop running after writing .xmat.1D files.        
                         * Useful for testing, or if you are going to      
                           run 3dREMLfit instead -- that is, you are just  
                           using 3dDeconvolve to set up the matrix file.   
    [-progress n]        Write statistical results for every nth voxel     
                         * To let you know that something is happening!    
    [-fdisp fval]        Write statistical results to the screen, for those
                           voxels whose full model F-statistic is > fval   
    [-help]              Oh go ahead, try it!                              
    
    **** Multiple CPU option (local CPUs only, no networking) ****
    
     -jobs J   Run the program with 'J' jobs (sub-processes).
                 On a multi-CPU machine, this can speed the
                 program up considerably.  On a single CPU
                 machine, using this option would be silly.
             * J should be a number from 1 up to the
                 number of CPUs sharing memory on the system.
             * J=1 is normal (single process) operation.
             * The maximum allowed value of J is 32.
             * Unlike other parallelized AFNI programs, this one
                 does not use OpenMP; it directly uses fork()
                 and shared memory to run multiple processes.
             * For more information on parallelizing, see
               https://afni.nimh.nih.gov/afni/doc/misc/afni_parallelize
             * Also use -mask or -automask to get more speed; cf. 3dAutomask.
    
    -virtvec   To save memory, write the input dataset to a temporary file
               and then read data vectors from it only as needed.  This option
               is for Javier and will probably not be useful for anyone else.
               And it only takes effect if -jobs is greater than 1.
    
    ** NOTE **
    This version of the program has been compiled to use
    double precision arithmetic for most internal calculations.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
