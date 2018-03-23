.. _ahelp_3dfim+:

******
3dfim+
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Program to calculate the cross-correlation of an ideal reference waveform  
    with the measured FMRI time series for each voxel.                         
                                                                           
    Usage:                                                                 
    3dfim+                                                                 
    -input fname       fname = filename of input 3d+time dataset           
    [-input1D dname]   dname = filename of single (fMRI) .1D time series   
    [-mask mname]      mname = filename of 3d mask dataset                 
    [-nfirst fnum]     fnum = number of first dataset image to use in      
                         the cross-correlation procedure. (default = 0)    
    [-nlast  lnum]     lnum = number of last dataset image to use in       
                         the cross-correlation procedure. (default = last) 
    [-polort pnum]     pnum = degree of polynomial corresponding to the    
                         baseline model  (pnum = 0, 1, etc.)               
                         (default: pnum = 1). Use -1 for no baseline model.
    [-fim_thr p]       p = fim internal mask threshold value (0 <= p <= 1) 
                         to get rid of low intensity voxels.               
                         (default: p = 0.0999), set p = 0.0 for no masking.
    [-cdisp cval]      Write (to screen) results for those voxels          
                         whose correlation stat. > cval  (0 <= cval <= 1)  
                         (default: disabled)                               
    [-ort_file sname]  sname = input ort time series file name             
    -ideal_file rname  rname = input ideal time series file name           
                                                                           
                Note:  The -ort_file and -ideal_file commands may be used  
                       more than once.                                     
                Note:  If files sname or rname contain multiple columns,   
                       then ALL columns will be used as ort or ideal       
                       time series.  However, individual columns or        
                       a subset of columns may be selected using a file    
                       name specification like 'fred.1D[0,3,5]', which     
                       indicates that only columns #0, #3, and #5 will     
                       be used for input.                                  
    
    [-out param]       Flag to output the specified parameter, where       
                       the string 'param' may be any one of the following: 
                                                                           
        Fit Coef       L.S. fit coefficient for Best Ideal                
      Best Index       Index number for Best Ideal (count starts at 1)    
        % Change       P-P amplitude of signal response / Baseline        
        Baseline       Average of baseline model response                 
     Correlation       Best Ideal product-moment correlation coefficient  
      % From Ave       P-P amplitude of signal response / Average         
         Average       Baseline + average of signal response              
      % From Top       P-P amplitude of signal response / Topline         
         Topline       Baseline + P-P amplitude of signal response        
     Sigma Resid       Std. Dev. of residuals from best fit               
             All       This specifies all of the above parameters       
     Spearman CC       Spearman correlation coefficient                   
     Quadrant CC       Quadrant correlation coefficient                   
                                                                           
                Note:  Multiple '-out' commands may be used.               
                Note:  If a parameter name contains imbedded spaces, the   
                       entire parameter name must be enclosed by quotes,   
                       e.g.,  -out 'Fit Coef'                                   
                                                                           
    [-bucket bprefix]  Create one AFNI 'bucket' dataset containing the     
                       parameters of interest, as specified by the above   
                       '-out' commands.                                    
                       The output 'bucket' dataset is written to a file    
                       with the prefix name bprefix.                       
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
