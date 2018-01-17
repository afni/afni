********
3dGetrow
********

.. _3dGetrow:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Program to extract 1 row from a dataset and write it as a .1D file
    Usage: 3dGetrow [options] dataset
    
    OPTIONS:
    -------
    Exactly ONE of the following three options is required:
     -xrow j k  = extract row along the x-direction at fixed y-index of j
                  and fixed z-index of k.
     -yrow i k  = similar for a row along the y-direction
     -zrow i j  = similar for a row along the z-direction
     -input ddd = read input from dataset 'ddd'
                  (instead of putting dataset name at end of command line)
     -output ff = filename for output .1D ASCII file will be 'ff'
                  (if 'ff' is '-', then output is to stdout, the default)
    
    N.B.: if the input dataset has more than one sub-brick, each
          sub-brick will appear as a separate column in the output file.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
