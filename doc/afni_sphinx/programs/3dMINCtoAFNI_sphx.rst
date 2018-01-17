************
3dMINCtoAFNI
************

.. _3dMINCtoAFNI:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dMINCtoAFNI [-prefix ppp] dataset.mnc
    Reads in a MINC formatted file and writes it out as an
    AFNI dataset file pair with the given prefix.  If the
    prefix option isn't used, the input filename will be
    used, after the '.mnc' is chopped off.
    
    NOTES:
    * Setting environment variable AFNI_MINC_FLOATIZE to Yes
       will cause MINC datasets to be converted to floats on
       input.  Otherwise, they will be kept in their 'native'
       data type if possible, which may cause problems with
       scaling on occasion.
    * The TR recorded in MINC files is often incorrect.  You may
       need to fix this (or other parameters) using 3drefit.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
