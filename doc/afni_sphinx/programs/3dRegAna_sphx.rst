********
3dRegAna
********

.. _ahelp_3dRegAna:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program performs multiple linear regression analysis.          
    
    Usage: 
    3dRegAna 
    -rows n                             number of input datasets          
    -cols m                             number of X variables             
    -xydata X11 X12 ... X1m filename    X variables and Y observations    
      .                                   .                               
      .                                   .                               
      .                                   .                               
    -xydata Xn1 Xn2 ... Xnm filename    X variables and Y observations    
                                                                          
    -model i1 ... iq : j1 ... jr   definition of linear regression model; 
                                     reduced model:                       
                                       Y = f(Xj1,...,Xjr)                 
                                     full model:                          
                                       Y = f(Xj1,...,Xjr,Xi1,...,Xiq)     
                                                                          
    [-diskspace]       print out disk space required for program execution
    [-workmem mega]    number of megabytes of RAM to use for statistical  
                       workspace  (default = 750 (was 12))                
    [-rmsmin r]        r = minimum rms error to reject constant model     
    [-fdisp fval]      display (to screen) results for those voxels       
                       whose F-statistic is > fval                        
                                                                          
    [-flof alpha]      alpha = minimum p value for F due to lack of fit   
                                                                          
                                                                          
    The following commands generate individual AFNI 2 sub-brick datasets: 
                                                                          
    [-fcoef k prefixname]        estimate of kth regression coefficient   
                                   along with F-test for the regression   
                                   is written to AFNI `fift' dataset      
    [-rcoef k prefixname]        estimate of kth regression coefficient   
                                   along with coef. of mult. deter. R^2   
                                   is written to AFNI `fith' dataset      
    [-tcoef k prefixname]        estimate of kth regression coefficient   
                                   along with t-test for the coefficient  
                                   is written to AFNI `fitt' dataset      
                                                                          
                                                                          
    The following commands generate one AFNI 'bucket' type dataset:       
                                                                          
    [-bucket n prefixname]     create one AFNI 'bucket' dataset having    
                                 n sub-bricks; n=0 creates default output;
                                 output 'bucket' is written to prefixname 
    The mth sub-brick will contain:                                       
    [-brick m coef k label]    kth parameter regression coefficient       
    [-brick m fstat label]     F-stat for significance of regression      
    [-brick m rstat label]     coefficient of multiple determination R^2  
    [-brick m tstat k label]   t-stat for kth regression coefficient      
    
    [-datum DATUM]     write the output in DATUM format. 
                       Choose from short (default) or float.
    
    
    N.B.: For this program, the user must specify 1 and only 1 sub-brick  
          with each -xydata command. That is, if an input dataset contains
          more than 1 sub-brick, a sub-brick selector must be used, e.g.: 
          -xydata 2.17 4.59 7.18  'fred+orig[3]'                          
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
