********************
3dExtractGroupInCorr
********************

.. _3dExtractGroupInCorr:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dExtractGroupInCorr [options] AAA.grpincorr.niml
    
    This program breaks the collection of images from a GroupInCorr
    file back into individual AFNI 3D+time datasets.
    
    Of course, only the data inside the mask used in 3dSetupGroupInCorr
    is stored in the .data file, so only those portions of the input
    files can be reconstructed :)
    
    The output datasets will be stored in float format, no matter what
    the storage type of the original datasets or of the .data file.
    
    OPTION:
    -------
     -prefix PPP The actual dataset prefix with be the internal dataset
                 label with the string 'PPP_' pre-prended.
                 ++ Use NULL to skip the use of the prefix.
    
    Author -- RWCox -- May 2012
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
