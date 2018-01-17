*******
3dTSgen
*******

.. _3dTSgen:

.. contents:: 
    :depth: 4 

.. code-block:: none

    This program generates an AFNI 3d+time data set.  The time series for 
    each voxel is generated according to a user specified signal + noise  
    model.                                                              
    
    Usage:                                                                
    3dTSgen                                                               
    -input fname       fname = filename of prototype 3d + time data file  
    [-inTR]            set the TR of the created timeseries to be the TR  
                         of the prototype dataset                         
                         [The default is to compute with TR = 1.]         
                         [The model functions are called for a  ]         
                         [time grid of 0, TR, 2*TR, 3*TR, ....  ]         
    -signal slabel     slabel = name of (non-linear) signal model         
    -noise  nlabel     nlabel = name of (linear) noise model              
    -sconstr k c d     constraints for kth signal parameter:              
                          c <= gs[k] <= d                                 
    -nconstr k c d     constraints for kth noise parameter:               
                          c+b[k] <= gn[k] <= d+b[k]                       
    -sigma  s          s = std. dev. of additive Gaussian noise           
    [-voxel num]       screen output for voxel #num                       
    -output fname      fname = filename of output 3d + time data file     
                                                                          
                                                                          
    The following commands generate individual AFNI 1 sub-brick datasets: 
                                                                          
    [-scoef k fname]   write kth signal parameter gs[k];                  
                         output 'fim' is written to prefix filename fname 
    [-ncoef k fname]   write kth noise parameter gn[k];                   
                         output 'fim' is written to prefix filename fname 
                                                                          
                                                                          
    The following commands generate one AFNI 'bucket' type dataset:       
                                                                          
    [-bucket n prefixname]   create one AFNI 'bucket' dataset containing  
                               n sub-bricks; n=0 creates default output;  
                               output 'bucket' is written to prefixname   
    The mth sub-brick will contain:                                       
    [-brick m scoef k label]   kth signal parameter regression coefficient
    [-brick m ncoef k label]   kth noise parameter regression coefficient 
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
