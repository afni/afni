**************
3dIntracranial
**************

.. _3dIntracranial:

.. contents:: 
    :depth: 4 

.. code-block:: none

    3dIntracranial - performs automatic segmentation of intracranial region.
                                                                            
       This program will strip the scalp and other non-brain tissue from a  
       high-resolution T1 weighted anatomical dataset.                      
                                                                            
    ** Nota Bene: the newer program 3dSkullStrip should also be considered  
    **            for this functionality -- it usually works better.        
                                                                            
    ----------------------------------------------------------------------- 
                                                                            
    Usage:                                                                  
    -----                                                                   
                                                                            
    3dIntracranial                                                          
       -anat filename   => Filename of anat dataset to be segmented         
                                                                            
       [-min_val   a]   => Minimum voxel intensity limit                    
                             Default: Internal PDF estimate for lower bound 
                                                                            
       [-max_val   b]   => Maximum voxel intensity limit                    
                             Default: Internal PDF estimate for upper bound 
                                                                            
       [-min_conn  m]   => Minimum voxel connectivity to enter              
                             Default: m=4                                   
                                                                            
       [-max_conn  n]   => Maximum voxel connectivity to leave              
                             Default: n=2                                   
                                                                            
       [-nosmooth]      => Suppress spatial smoothing of segmentation mask  
                                                                            
       [-mask]          => Generate functional image mask (complement)      
                             Default: Generate anatomical image            
                                                                            
       [-quiet]         => Suppress output to screen                        
                                                                            
       -prefix pname    => Prefix name for file to contain segmented image  
                                                                            
       ** NOTE **: The newer program 3dSkullStrip will probably give        
                   better segmentation results than 3dIntracranial!         
    ----------------------------------------------------------------------- 
                                                                            
    Examples:                                                               
    --------                                                                
                                                                            
       3dIntracranial -anat elvis+orig -prefix elvis_strip                 
                                                                            
       3dIntracranial -min_val 30 -max_val 350 -anat elvis+orig -prefix strip
                                                                            
       3dIntracranial -nosmooth -quiet -anat elvis+orig -prefix elvis_strip 
                                                                            
    ----------------------------------------------------------------------- 
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
