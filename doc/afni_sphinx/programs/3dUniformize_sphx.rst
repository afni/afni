************
3dUniformize
************

.. _ahelp_3dUniformize:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

       ***** NOTES *********************************************
       1) This program is superseded by 3dUnifize, and we don't
          recommend that you use it.
       2) This program will crash if you give it a multi-volume
          dataset.
       3) Neither 3dUniformize nor 3dUnifize can properly deal
          with EPI datasets at this time.
       *********************************************************
    
    This program corrects for T1-weighted image intensity nonuniformity.
    
    
    Usage: 
    3dUniformize  
    -anat filename    Filename of anat dataset to be corrected            
                                                                          
    [-clip_low LOW]   Use LOW as the voxel intensity separating           
                      brain from air.                                     
       NOTE: The historic clip_low value was 25.                          
          But that only works for certain types of input data and can     
          result in bad output depending on the range of values in        
          the input dataset.                                              
          The new default sets -clip_low via -auto_clip option.           
    [-clip_high HIGH] Do not include voxels with intensity higher         
                      than HIGH in calculations.                          
    [-auto_clip]      Automatically set the clip levels.                  
                      LOW in a procedure similar to 3dClipLevel,          
                      HIGH is set to 3*LOW. (Default since Jan. 2011)     
    [-niter NITER]    Set the number of iterations for concentrating PDF  
                      Default is 5.                                       
    [-quiet]          Suppress output to screen                           
                                                                          
    -prefix pname     Prefix name for file to contain corrected image     
    
    Versions of this program postdating Jan. 3rd 2010 can handle byte, short
    or float input and output the result in the data type as the input
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
