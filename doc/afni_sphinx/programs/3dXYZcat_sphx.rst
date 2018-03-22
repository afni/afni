********
3dXYZcat
********

.. _3dXYZcat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dXYZcat [options] dataset dataset ...
    * Catenates datasets spatially (for time cat-ing, cf. 3dTcat).
    * The input datasets must match, in the sense that the pieces
      fit together properly (spatially and in time).
    * Unlike in 3dZcat, all input datasets must be stored with the
      same data type (e.g., shorts, floats, ...); also, sub-brick scale
      factors are not allowed.  If you need to spatially catenate scaled
      short datasets, for example, convert them to float format using
      '3dcalc -float', then catenate THOSE datasets.
    
    Options:
    --------
      -prefix pname = Use 'pname' for the output dataset prefix name.
                        [default prefix = 'xyzcat']
      -verb         = Print out some verbositiness as the program proceeds.
      -dir Q        = Catenate along direction 'Q', which is one of
                        X or Y or Z (synonyms are I or J or K)
                      which are the STORAGE directions (not DICOM) of the
                      3D grid of the input datasets.
                        [default direction = 'X', for no good reason]
    
    Command line arguments after the above are taken as input datasets.
    
    Notes:
    ------
    * If the i-th input dataset has dimensions nx[i] X ny[i] X nz[i], then
        case Q = X | I ==> all ny[i] and nz[i] must be the same;
                           the output dataset has nx = sum{ nx[i] }
        case Q = Y | J ==> all nx[i] and nz[i] must be the same;
                           the output dataset has ny = sum{ ny[i] }
        case Q = Z | K ==> all nx[i] and ny[i] must be the same;
                           the output dataset has nz = sum{ nz[i] }
    * In all cases, the input datasets must have the same number of
      sub-bricks (time points) and the same data storage type.
    * You can use the '3dinfo' program to see the orientation and
        grid size of a dataset, to help you decide how to glue your
        inputs together.
    * There must be at least two datasets input (otherwise, the
        program doesn't make much sense, now does it?).
    * This is mostly useful for making side-by-side pictures from
        multiple datasets, for edification and elucidation.
    * If you have some other use for 3dXYZcat, let me know!
    
    ** Author: RW Cox [Dec 2010] **
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
