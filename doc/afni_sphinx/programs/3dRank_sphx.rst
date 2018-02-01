******
3dRank
******

.. _3dRank:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dRank [-prefix PREFIX] <-input DATASET1 [DATASET2 ...]>
    
    Replaces voxel values by their rank in the set of
    values collected over all voxels in all input datasets
    If you input one dataset, the output should be identical
    to the -1rank option in 3dmerge
    
    This program only works on datasets of integral storage type, 
    and on integral valued data stored as floats.
    
      -input DATASET1 [DATASET2 ...]: Input datasets.
                                      Acceptable data types are:
                                      byte, short, and floats.
      -prefix PREFIX: Ouput prefix.
                      If you have multiple datasets on input
                      the prefix is preceded by r00., r01., etc.
                      If no prefix is given, the default is 
                      rank.DATASET1, rank.DATASET2, etc.
    
                      In addition to the ranked volume, a rank map
                      1D file is created. It shows the mapping from 
                      the rank (1st column) to the integral values 
                      (2nd column) in the input dataset. Sub-brick float 
                      factors are ignored.
    
      -ver = print author and version info
      -help = print this help screen
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
