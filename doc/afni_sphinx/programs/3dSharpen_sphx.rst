*********
3dSharpen
*********

.. _ahelp_3dSharpen:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dSharpen [options] dataset
    
    Applies a simple 3D sharpening filter to the POSITIVE values
    in the #0 volume of the input dataset, and writes out a new
    dataset.
    
    Only operates on positive valued voxels in the dataset.
    Non-positive values will not be altered.
    
    Options:
    --------
    
     -phi fff       = Sharpening factor, between 0.1 and 0.9 (inclusive).
                      Larger means more sharpening. Default is 0.4.
    
     -input dataset = An option to input the dataset anywhere,
                      not just at the end of the command line.
    
     -prefix pref   = Select the name of the output dataset
                      (it will be in floating point format).
    
    * A quick hack for experimental purposes.
    * e.g., Cleaning up the results of brain template construction.
    * RWCox - Feb 2017.
