*********
3dbuc2fim
*********

.. _ahelp_3dbuc2fim:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program converts bucket sub-bricks to fim (fico, fitt, fift, ...)
    type dataset.                                                       
    
    Usage:                                                              
    
    3dbuc2fim  -prefix pname  d1+orig[index]                              
         This produces a fim dataset.                                   
    
     -or-                                                               
    
    3dbuc2fim  -prefix pname  d1+orig[index1]  d2+orig[index2]            
         This produces a fico (fitt, fift, ...) dataset,                  
         depending on the statistic type of the 2nd subbrick,             
         with   d1+orig[index1] -> intensity sub-brick of pname           
                d2+orig[index2] -> threshold sub-brick of pname         
    
     -or-                                                               
    
    3dbuc2fim  -prefix pname  d1+orig[index1,index2]                      
         This produces a fico (fitt, fift, ...) dataset,                  
         depending on the statistic type of the 2nd subbrick,             
         with   d1+orig[index1] -> intensity sub-brick of pname           
                d1+orig[index2] -> threshold sub-brick of pname         
    
    where the options are:
         -prefix pname = Use 'pname' for the output dataset prefix name.
     OR  -output pname     [default='buc2fim']
    
         -session dir  = Use 'dir' for the output dataset session directory.
                           [default='./'=current working directory]
         -verb         = Print out some verbose output as the program
                           proceeds 
    
    Command line arguments after the above are taken as input datasets.  
    A dataset is specified using one of these forms:
       'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.
    Sub-brick indexes start at 0. 
    
    N.B.: The sub-bricks are output in the order specified, which may
     not be the order in the original datasets.  For example, using
               fred+orig[5,3]
     will cause the sub-brick #5 in fred+orig to be output as the intensity
     sub-brick, and sub-brick #3 to be output as the threshold sub-brick 
     in the new dataset.
    
    N.B.: The '$', '(', ')', '[', and ']' characters are special to
     the shell, so you will have to escape them.  This is most easily
     done by putting the entire dataset plus selection list inside
     single quotes, as in 'fred+orig[5,9]'.
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
