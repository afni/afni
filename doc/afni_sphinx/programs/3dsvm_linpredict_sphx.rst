****************
3dsvm_linpredict
****************

.. _3dsvm_linpredict:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3ddot [options] w dset
    Output = linear prediction for w from 3dsvm 
             - you can use sub-brick selectors on the dsets
             - the result is a number printed to stdout
    Options:
      -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be averaged from 'dataset'.  Note
                     that the mask dataset and the input dataset
                     must have the same number of voxels.
    
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
