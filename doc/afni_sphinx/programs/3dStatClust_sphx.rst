.. _ahelp_3dStatClust:

***********
3dStatClust
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Perform agglomerative hierarchical clustering for user specified 
    parameter sub-bricks, for all voxels whose threshold statistic   
    is above a user specified value.
    
    Usage: 3dStatClust options datasets 
    where the options are:
    -prefix pname    = Use 'pname' for the output dataset prefix name.
      OR                 [default='SC']
    -output pname
    
    -session dir     = Use 'dir' for the output dataset session directory.
                         [default='./'=current working directory]
    -verb            = Print out verbose output as the program proceeds.
    
    Options for calculating distance between parameter vectors: 
       -dist_euc        = Calculate Euclidean distance between parameters 
       -dist_ind        = Statistical distance for independent parameters 
       -dist_cor        = Statistical distance for correlated parameters 
    The default option is:  Euclidean distance. 
    
    -thresh t tname  = Use threshold statistic from file tname. 
                       Only voxels whose threshold statistic is greater 
                       than t in abolute value will be considered. 
                         [If file tname contains more than 1 sub-brick, 
                         the threshold stat. sub-brick must be specified!]
    -nclust n        = This specifies the maximum number of clusters for 
                       output (= number of sub-bricks in output dataset).
    
    Command line arguments after the above are taken as parameter datasets.
    
    
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
