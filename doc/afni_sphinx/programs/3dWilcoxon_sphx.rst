**********
3dWilcoxon
**********

.. _ahelp_3dWilcoxon:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program performs the nonparametric Wilcoxon signed-rank test 
    for paired comparisons of two samples. 
    
    Usage: 
    3dWilcoxon                                                          
    -dset 1 filename               data set for X observations          
     . . .                           . . .                              
    -dset 1 filename               data set for X observations          
    -dset 2 filename               data set for Y observations          
     . . .                           . . .                              
    -dset 2 filename               data set for Y observations          
                                                                        
    [-workmem mega]                number of megabytes of RAM to use    
                                     for statistical workspace          
    [-voxel num]                   screen output for voxel # num        
    -out prefixname                estimated population delta and       
                                     Wilcoxon signed-rank statistics are
                                     written to file prefixname         
    
    
    N.B.: For this program, the user must specify 1 and only 1 sub-brick  
          with each -dset command. That is, if an input dataset contains  
          more than 1 sub-brick, a sub-brick selector must be used, e.g.: 
          -dset 2 'fred+orig[3]'                                          
    
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
