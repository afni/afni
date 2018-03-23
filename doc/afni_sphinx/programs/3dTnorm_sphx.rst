.. _ahelp_3dTnorm:

*******
3dTnorm
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTnorm [options] dataset
    Takes each voxel time series and normalizes it
    (by multiplicative scaling) -- in some sense.
    
    Options:
     -prefix p = use string 'p' for the prefix of the
                   output dataset [DEFAULT = 'tnorm']
     -norm2    = L2 normalize (sum of squares = 1) [DEFAULT]
     -normR    = normalize so sum of squares = number of time points
                 * e.g., so RMS = 1.
     -norm1    = L1 normalize (sum of absolute values = 1)
     -normx    = Scale so max absolute value = 1 (L_infinity norm)
     -polort p = Detrend with polynomials of order p before normalizing
                   [DEFAULT = don't do this]
                 * Use '-polort 0' to remove the mean, for example
     -L1fit    = Detrend with L1 regression (L2 is the default)
                 * This option is here just for the hell of it
    
    Notes:
    * Each voxel is processed separately
    * A voxel that is all zero will be unchanged (duh)
    * Output dataset is in float format, no matter what the input format
    * This program is for producing regressors to use in 3dTfitter
    * Also see programs 1dnorm and 3dcalc
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
