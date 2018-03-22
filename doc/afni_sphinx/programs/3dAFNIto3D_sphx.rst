**********
3dAFNIto3D
**********

.. _ahelp_3dAFNIto3D:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAFNIto3D [options] dataset
    Reads in an AFNI dataset, and writes it out as a 3D file.
    
    OPTIONS:
     -prefix ppp  = Write result into file ppp.3D;
                      default prefix is same as AFNI dataset's.
     -bin         = Write data in binary format, not text.
     -txt         = Write data in text format, not binary.
    
    NOTES:
    * At present, all bricks are written out in float format.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
