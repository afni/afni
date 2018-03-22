**********
3dWavelets
**********

.. _ahelp_3dWavelets:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Program to perform wavelet analysis of an FMRI 3d+time dataset.        
                                                                           
    Usage:                                                                 
    3dWavelets                                                             
    -type wname          wname = name of wavelet to use for the analysis   
                         At present, there are only two choices for wname: 
                            Haar  -->  Haar wavelets                       
                            Daub  -->  Daubechies wavelets                 
    -input fname         fname = filename of 3d+time input dataset         
    [-input1D dname]     dname = filename of single (fMRI) .1D time series 
    [-mask mname]        mname = filename of 3d mask dataset               
    [-nfirst fnum]       fnum = number of first dataset image to use in    
                           the wavelet analysis. (default = 0)             
    [-nlast  lnum]       lnum = number of last dataset image to use in     
                           the wavelet analysis. (default = last)          
    [-fdisp fval]        Write (to screen) results for those voxels        
                           whose F-statistic is >= fval                    
                                                                           
    Filter options:                                                        
                                                                           
    [-filt_stop band mintr maxtr] Specify wavelet coefs. to set to zero    
    [-filt_base band mintr maxtr] Specify wavelet coefs. for baseline model
    [-filt_sgnl band mintr maxtr] Specify wavelet coefs. for signal model  
         where  band  = frequency band                                     
                mintr = min. value for time window (in TR)                 
                maxtr = max. value for time window (in TR)                 
                                                                           
    Output options:                                                        
                                                                           
    [-datum DTYPE]      Coerce the output data to be stored as type DTYPE, 
                           which may be short or float. (default = short)  
                                                                           
    [-coefts cprefix]   cprefix = prefix of 3d+time output dataset which   
                           will contain the forward wavelet transform      
                           coefficients                                    
                                                                           
    [-fitts  fprefix]   fprefix = prefix of 3d+time output dataset which   
                           will contain the full model time series fit     
                           to the input data                               
                                                                           
    [-sgnlts sprefix]   sprefix = prefix of 3d+time output dataset which   
                           will contain the signal model time series fit   
                           to the input data                               
                                                                           
    [-errts  eprefix]   eprefix = prefix of 3d+time output dataset which   
                           will contain the residual error time series     
                           from the full model fit to the input data       
                                                                           
    The following options control the contents of the bucket dataset:      
                                                                           
    [-fout]            Flag to output the F-statistics                     
    [-rout]            Flag to output the R^2 statistics                   
    [-cout]            Flag to output the full model wavelet coefficients  
    [-vout]            Flag to output the sample variance (MSE) map        
                                                                           
    [-stat_first]      Flag to specify that the full model statistics will 
                         appear prior to the wavelet coefficients in the   
                         bucket dataset output                             
                                                                           
    [-bucket bprefix]  bprefix = prefix of AFNI 'bucket' dataset containing
                         parameters of interest, such as the F-statistic   
                         for significance of the wavelet signal model.     
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
