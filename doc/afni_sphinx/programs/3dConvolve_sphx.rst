**********
3dConvolve
**********

.. _3dConvolve:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Program to calculate the voxelwise convolution of given impulse response   
    function (IRF) time series contained in a 3d+time dataset with a specified 
    input stimulus function time series.  This program will also calculate     
    convolutions involving multiple IRF's and multiple stimulus functions.     
    Input options include addition of system noise to the estimated output.    
    Output consists of an AFNI 3d+time dataset which contains the estimated    
    system response.  Alternatively, if all inputs are .1D time series files,  
    then the output will be a single .1D time series file.                     
                                                                           
    Usage:                                                                 
    3dConvolve                                                             
    -input fname         fname = filename of 3d+time template dataset      
    [-input1D]           flag to indicate all inputs are .1D time series   
    [-mask mname]        mname = filename of 3d mask dataset               
    [-censor cname]      cname = filename of censor .1D time series        
    [-concat rname]      rname = filename for list of concatenated runs    
    [-nfirst fnum]       fnum = number of first time point to calculate by 
                           convolution procedure.  (default = max maxlag)  
    [-nlast  lnum]       lnum = number of last time point to calculate by  
                           convolution procedure.  (default = last point)  
    [-polort pnum]       pnum = degree of polynomial corresponding to the  
                           baseline model  (default: pnum = 1)             
    [-base_file bname]   bname = file containing baseline parameters       
                                                                           
    -num_stimts num      num = number of input stimulus time series        
                           (default: num = 0)                              
    -stim_file k sname   sname = filename of kth time series input stimulus
    [-stim_minlag k m]   m = minimum time lag for kth input stimulus       
                           (default: m = 0)                                
    [-stim_maxlag k n]   n = maximum time lag for kth input stimulus       
                           (default: n = 0)                                
    [-stim_nptr k p]     p = number of stimulus function points per TR     
                           Note: This option requires 0 slice offset times 
                           (default: p = 1)                                
                                                                           
    [-iresp k iprefix]   iprefix = prefix of 3d+time input dataset which   
                           contains the kth impulse response function      
                                                                           
    [-errts eprefix]     eprefix = prefix of 3d+time input dataset which   
                           contains the residual error time series         
                           (i.e., noise which will be added to the output) 
                                                                           
    [-sigma s]           s = std. dev. of additive Gaussian noise          
                           (default: s = 0)                                
    [-seed d]            d = seed for random number generator              
                           (default: d = 1234567)                          
                                                                           
    [-xout]              flag to write X matrix to screen                  
    [-output tprefix]    tprefix = prefix of 3d+time output dataset which  
                           will contain the convolved time series data     
                           (or tprefix = prefix of .1D output time series  
                           if the -input1D option is used)                 
                                                                           
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
