*******
3dNLfim
*******

.. _ahelp_3dNLfim:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program calculates a nonlinear regression for each voxel of the  
    input AFNI 3d+time data set.  The nonlinear regression is calculated  
    by means of a least squares fit to the signal plus noise models which 
    are specified by the user.                                            
                                                                          
    Usage:                                                                
    3dNLfim                                                               
    -input fname       fname = filename of 3d + time data file for input  
    [-mask mset]       Use the 0 sub-brick of dataset 'mset' as a mask    
                         to indicate which voxels to analyze (a sub-brick 
                         selector is allowed)  [default = use all voxels] 
    [-ignore num]      num   = skip this number of initial images in the  
                         time series for regresion analysis; default = 0  
                   ****N.B.: default ignore value changed from 3 to 0,    
                             on 04 Nov 2008 (BHO day).                    
    [-inTR]            set delt = TR of the input 3d+time dataset         
                         [The default is to compute with delt = 1.0 ]     
                         [The model functions are calculated using a      
                          time grid of: 0, delt, 2*delt, 3*delt, ... ]    
    [-TR delt]         directly set the TR of the time series model;      
                         can be useful if the input file is a .1D file    
                         (transposed with the \' operator)               
    [-time fname]      fname = ASCII file containing each time point      
                         in the time series. Defaults to even spacing     
                         given by TR (this option overrides -inTR).       
    -signal slabel     slabel = name of (non-linear) signal model         
    -noise  nlabel     nlabel = name of (linear) noise model              
    -sconstr k c d     constraints for kth signal parameter:              
                          c <= gs[k] <= d                                 
                     **N.B.: It is important to set the parameter         
                             constraints with care!                       
                     **N.B.: -sconstr and -nconstr options must appear    
                             AFTER -signal and -noise on the command line 
    -nconstr k c d     constraints for kth noise parameter:               
                          c+b[k] <= gn[k] <= d+b[k]                       
    [-nabs]            use absolute constraints for noise parameters:     
                         c <= gn[k] <= d  [default=relative, as above]    
    [-nrand n]         n = number of random test points [default=19999]      
    [-nbest b]         b = use b best test points to start [default=9]   
    [-rmsmin r]        r = minimum rms error to reject reduced model      
    [-fdisp fval]      display (to screen) results for those voxels       
                         whose f-statistic is > fval [default=999.0]       
    [-progress ival]   display (to screen) results for those voxels       
                         every ival number of voxels                      
    [-voxel_count]     display (to screen) the current voxel index        
                                                                          
    --- These options choose the least-square minimization algorithm ---  
                                                                          
    [-SIMPLEX]         use Nelder-Mead simplex method [default]           
    [-POWELL]          use Powell's NEWUOA method instead of the          
                         Nelder-Mead simplex method to find the           
                         nonlinear least-squares solution                 
                         [slower; usually more accurate, but not always!] 
    [-BOTH]            use both Powell's and Nelder-Mead method           
                         [slowest, but should be most accurate]           
                                                                          
    --- These options generate individual AFNI 2 sub-brick datasets ---   
    --- [All these options must be AFTER options -signal and -noise]---   
                                                                          
    [-freg fname]      perform f-test for significance of the regression; 
                         output 'fift' is written to prefix filename fname
    [-frsqr fname]     calculate R^2 (coef. of multiple determination);   
                         store along with f-test for regression;          
                         output 'fift' is written to prefix filename fname
    [-fsmax fname]     estimate signed maximum of signal; store along     
                         with f-test for regression; output 'fift' is     
                         written to prefix filename fname                 
    [-ftmax fname]     estimate time of signed maximum; store along       
                         with f-test for regression; output 'fift' is     
                         written to prefix filename fname                 
    [-fpsmax fname]    calculate (signed) maximum percentage change of    
                         signal from baseline; output 'fift' is           
                         written to prefix filename fname                 
    [-farea fname]     calculate area between signal and baseline; store  
                         with f-test for regression; output 'fift' is     
                         written to prefix filename fname                 
    [-fparea fname]    percentage area of signal relative to baseline;    
                         store with f-test for regression; output 'fift'  
                         is written to prefix filename fname              
    [-fscoef k fname]  estimate kth signal parameter gs[k]; store along   
                         with f-test for regression; output 'fift' is     
                         written to prefix filename fname                 
    [-fncoef k fname]  estimate kth noise parameter gn[k]; store along    
                         with f-test for regression; output 'fift' is     
                         written to prefix filename fname                 
    [-tscoef k fname]  perform t-test for significance of the kth signal  
                         parameter gs[k]; output 'fitt' is written        
                         to prefix filename fname                         
    [-tncoef k fname]  perform t-test for significance of the kth noise   
                         parameter gn[k]; output 'fitt' is written        
                         to prefix filename fname                         
                                                                          
    --- These options generate one AFNI 'bucket' type dataset ---         
                                                                          
    [-bucket n prefixname]   create one AFNI 'bucket' dataset containing  
                               n sub-bricks; n=0 creates default output;  
                               output 'bucket' is written to prefixname   
    The mth sub-brick will contain:                                       
    [-brick m scoef k label]   kth signal parameter regression coefficient
    [-brick m ncoef k label]   kth noise parameter regression coefficient 
    [-brick m tmax label]      time at max. abs. value of signal          
    [-brick m smax label]      signed max. value of signal                
    [-brick m psmax label]     signed max. value of signal as percent     
                                 above baseline level                     
    [-brick m area label]      area between signal and baseline           
    [-brick m parea label]     signed area between signal and baseline    
                                 as percent of baseline area              
    [-brick m tscoef k label]  t-stat for kth signal parameter coefficient
    [-brick m tncoef k label]  t-stat for kth noise parameter coefficient 
    [-brick m resid label]     std. dev. of the full model fit residuals  
    [-brick m rsqr  label]     R^2 (coefficient of multiple determination)
    [-brick m fstat label]     F-stat for significance of the regression  
    
    [-noFDR]                   Don't write the FDR (q vs. threshold)
                               curves into the output dataset.
                               (Same as 'setenv AFNI_AUTOMATIC_FDR NO')
                                                                          
         --- These options write time series fit for ---                  
         --- each voxel to an AFNI 3d+time dataset   ---                  
                                                                          
    [-sfit fname]      fname = prefix for output 3d+time signal model fit 
    [-snfit fname]     fname = prefix for output 3d+time signal+noise fit 
                                                                          
    
     -jobs J   Run the program with 'J' jobs (sub-processes).
                 On a multi-CPU machine, this can speed the
                 program up considerably.  On a single CPU
                 machine, using this option is silly.
                 J should be a number from 1 up to the
                 number of CPU sharing memory on the system.
                 J=1 is normal (single process) operation.
                 The maximum allowed value of J is 32.
             * For more information on parallelizing, see
                 https://afni.nimh.nih.gov/afni/doc/misc/parallize.html
             * Use -mask to get more speed; cf. 3dAutomask.
    
    ----------------------------------------------------------------------
    Signal Models (see the appropriate model_*.c file for exact details) :
    
      Null                     : No Signal
                                 (no parameters)
                                 see model_null.c
    
      SineWave_AP              : Sinusoidal Response
                                 (amplitude, phase)
                                 see model_sinewave_ap.c
    
      SquareWave_AP            : Square Wave Response
                                 (amplitude, phase)
                                 see model_squarewave_ap.c
    
      TrnglWave_AP             : Triangular Wave Response
                                 (amplitude, phase)
                                 see model_trnglwave_ap.c
    
      SineWave_APF             : Sinusoidal Wave Response
                                 (amplitude, phase, frequency)
                                 see model_sinewave_apf.c
    
      SquareWave_APF           : Sinusoidal Wave Response
                                 (amplitude, phase, frequency)
                                 see model_squarewave_apf.c
    
      TrnglWave_APF            : Sinusoidal Wave Response
                                 (amplitude, phase, frequency)
                                 see model_trnglwave_apf.c
    
      Exp                      : Exponential Function
                                 (a,b): a * exp(b * t)
                                 see model_exp.c
    
      DiffExp                  : Differential-Exponential Drug Response
                                 (t0, k, alpha1, alpha2)
                                 see model_diffexp.c
    
      GammaVar                 : Gamma-Variate Function Drug Response
                                 (t0, k, r, b)
                                 see model_gammavar.c
    
      Beta                     : Beta Distribution Model
                                 (t0, tf, k, alpha, beta)
                                 see model_beta.c
    
      ConvGamma2a              : Gamma Convolution with 2 Input Time Series
                                 (t0, r, b)
                                 see model_convgamma2a.c
    
      ConvGamma                : Gamma Vairate Response Model
                                 (t0, amp, r, b)
                                 see model_convgamma.c
    
      ConvDiffGam              : Difference of 2 Gamma Variates
                                 (A0, T0, E0, D0, A1, T1, E1, D1)
                                 see model_conv_diffgamma.c
                      for help : setenv AFNI_MODEL_HELP_CONVDIFFGAM YES
                                 3dNLfim -signal ConvDiffGam
    
      demri_3                  : Dynamic (contrast) Enhanced MRI
                                 (K_trans, Ve, k_ep)
                                 see model_demri_3.c
                      for help : setenv AFNI_MODEL_HELP_DEMRI_3 YES
                                 3dNLfim -signal demri_3
    
      ADC                      : Diffusion Signal Model
                                 (So, D)
                                 see model_diffusion.c
    
      michaelis_menton         : Michaelis/Menten Concentration Model
                                 (v, vmax, k12, k21, mag)
                                 see model_michaelis_menton.c
    
      Expr2                    : generic (3dcalc-like) expression with
                                 exactly 2 'free' parameters and using
                                 symbol 't' as the time variable;
                                 see model_expr2.c for details.
    
      ConvCosine4              : 4-piece Cosine Convolution Model
                                 (A, C1, C2, M1, M2, M3, M4)
                                 see model_conv_cosine4.c
                      for help : setenv AFNI_MODEL_HELP_CONV_COSINE4 YES
                                 3dNLfim -signal ConvCosine4
    
      Conv_PRF                 : 4-param Population Receptive Field Model
                                 (A, X, Y, sigma)
                                 see model_conv_PRF.c
                      for help : setenv AFNI_MODEL_HELP_CONV_PRF YES
                                 3dNLfim -signal bunnies
    
      Conv_PRF_6               : 6-param Population Receptive Field Model
                                 (A, X, Y, sigma, sigrat, theta)
                                 see model_conv_PRF_6.c
                      for help : setenv AFNI_MODEL_HELP_CONV_PRF_6 YES
                                 3dNLfim -signal bunnies
    
      Conv_PRF_DOG             : 6-param 'Difference of Gaussians' PRF Model
                                 (as Conv_PRF, but with second A and sigma)
                                 (A, X, Y, sig, A2, sig2)
                                 see model_conv_PRF_DOG.c
                      for help : setenv AFNI_MODEL_HELP_CONV_PRF_DOG YES
                                 3dNLfim -signal bunnies
    
    ----------------------------------------
    Noise Models (see the appropriate model_*.c file for exact details) :
    
      Zero                     : Zero Noise Model
                                 (no parameters)
                                 see model_zero.c
    
      Constant                 : Constant Noise Model
                                 (constant)
                                 see model_constant.c
    
      Linear                   : Linear Noise Model
                                 (constant, linear)
                                 see model_linear.c
    
      Linear+Ort               : Linear+Ort Noise Model
                                 (constant, linear, Ort)
                                 see model_linplusort.c
    
      Quadratic                : Quadratic Noise Model
                                 (constant, linear, quadratic)
                                 see model_quadratic.c
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
