**************
3dTwotoComplex
**************

.. _3dTwotoComplex:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage #1: 3dTwotoComplex [options] dataset
    Usage #2: 3dTwotoComplex [options] dataset1 dataset2
    
    Converts 2 sub-bricks of input to a complex-valued dataset.
    * If you have 1 input dataset, then sub-bricks [0..1] are
        used to form the 2 components of the output.
    * If you have 2 input datasets, then the [0] sub-brick of
        each is used to form the components.
    * Complex datasets have two 32-bit float components per voxel.
    
    Options:
      -prefix ppp = Write output into dataset with prefix 'ppp'.
                      [default='cmplx']
      -RI         = The 2 inputs are real and imaginary parts.
                      [this is the default]
      -MP         = The 2 inputs are magnitude and phase.
                      [phase is in radians, please!]
      -mask mset  = Only output nonzero values where the mask
                      dataset 'mset' is nonzero.
    Notes:
    * Input datasets must be byte-, short-, or float-valued.
    * You might calculate the component datasets using 3dcalc.
    * At present, there is limited support for complex datasets.
        About the only thing you can do is display them in 2D
        slice windows in AFNI.
    
    -- RWCox - March 2006
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
