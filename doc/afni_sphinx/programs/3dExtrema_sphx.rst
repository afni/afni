*********
3dExtrema
*********

.. _3dExtrema:

.. contents:: 
    :depth: 4 

.. code-block:: none

    This program finds local extrema (minima or maxima) of the input       
    dataset values for each sub-brick of the input dataset.  The extrema   
    may be determined either for each volume, or for each individual slice.
    Only those voxels whose corresponding intensity value is greater than  
    the user specified data threshold will be considered.                  
    
    Usage: 3dExtrema  options  datasets                                  
    where the options are:                                                 
    -prefix pname    = Use 'pname' for the output dataset prefix name.     
      OR                 [default = NONE; only screen output]              
    -output pname                                                          
                                                                           
    -session dir     = Use 'dir' for the output dataset session directory. 
                         [default='./'=current working directory]          
                                                                           
    -quiet           = Flag to suppress screen output                      
                                                                           
    -mask_file mname = Use mask statistic from file mname.                 
                       Note: If file mname contains more than 1 sub-brick, 
                       the mask sub-brick must be specified!               
    -mask_thr m        Only voxels whose mask statistic is >= m            
                       in absolute value will be considered.               
                       A default value of 1 is assumed.                    
                                                                           
    -data_thr d        Only voxels whose value (intensity) is greater      
                       than d in absolute value will be considered.        
                                                                           
    -nbest N           Only print the first N extrema.
                                                                           
    -sep_dist d        Min. separation distance [mm] for distinct extrema  
                                                                           
    Choose type of extrema (one and only one choice):                      
    -minima            Find local minima.                                  
    -maxima [default]  Find local maxima.                                  
                                                                           
    Choose form of binary relation (one and only one choice):              
    -strict [default]  >  for maxima,  <  for minima                       
    -partial           >= for maxima,  <= for minima                       
                                                                           
    Choose boundary criteria (one and only one choice):                    
    -interior [default]Extrema must be interior points (not on boundary)   
    -closure           Extrema may be boundary points                      
                                                                           
    Choose domain for finding extrema (one and only one choice):           
    -slice [default]   Each slice is considered separately                 
    -volume            The volume is considered as a whole                 
                                                                           
    Choose option for merging of extrema (one and only one choice):        
    -remove [default]  Remove all but strongest of neighboring extrema     
    -average           Replace neighboring extrema by average              
    -weight            Replace neighboring extrema by weighted average     
                                                                           
    Command line arguments after the above are taken to be input datasets. 
    
     Examples: 
      Compute maximum value in amygdala region of Talairach-transformed dataset
        3dExtrema -volume -closure -sep_dist 512 \ 
          -mask_file 'TT_Daemon::amygdala' func_slim+tlrc'[0]'
      Show minimum voxel values not on edge of mask, where the mask >= 0.95
        3dExtrema -minima -volume -mask_file 'statmask+orig' \ 
          -mask_thr 0.95 func_slim+tlrc'[0]'
      Get the maximum 3 values across the given ROI.
        3dExtrema -volume -closure -mask_file MY_ROI+tlrc \
                  -nbest 3 func_slim+tlrc'[0]'
    
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
