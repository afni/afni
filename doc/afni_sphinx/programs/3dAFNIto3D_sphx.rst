.. contents:: 
    :depth: 4 

**********
3dAFNIto3D
**********

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
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
