*********
3dTsmooth
*********

.. _ahelp_3dTsmooth:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTsmooth [options] dataset
    Smooths each voxel time series in a 3D+time dataset and produces
    as output a new 3D+time dataset (e.g., lowpass filter in time).
    
    *** Also see program 3dBandpass ***
    
    General Options:
      -prefix ppp  = Sets the prefix of the output dataset to be 'ppp'.
                       [default = 'smooth']
      -datum type  = Coerce output dataset to be stored as the given type.
                       [default = input data type]
    
    Three Point Filtering Options [07 July 1999]
    --------------------------------------------
    The following options define the smoothing filter to be used.
    All these filters  use 3 input points to compute one output point:
      Let a = input value before the current point
          b = input value at the current point
          c = input value after the current point
               [at the left end, a=b; at the right end, c=b]
    
      -lin = 3 point linear filter: 0.15*a + 0.70*b + 0.15*c
               [This is the default smoother]
      -med = 3 point median filter: median(a,b,c)
      -osf = 3 point order statistics filter:
               0.15*min(a,b,c) + 0.70*median(a,b,c) + 0.15*max(a,b,c)
    
      -3lin m = 3 point linear filter: 0.5*(1-m)*a + m*b + 0.5*(1-m)*c
                  Here, 'm' is a number strictly between 0 and 1.
    
    General Linear Filtering Options [03 Mar 2001]
    ----------------------------------------------
      -hamming N  = Use N point Hamming or Blackman windows.
      -blackman N     (N must be odd and bigger than 1.)
      -custom coeff_filename.1D (odd # of coefficients must be in a 
                                 single column in ASCII file)
       (-custom added Jan 2003)
        WARNING: If you use long filters, you do NOT want to include the
                 large early images in the program.  Do something like
                    3dTsmooth -hamming 13 'fred+orig[4..$]'
                 to eliminate the first 4 images (say).
     The following options determing how the general filters treat
     time points before the beginning and after the end:
      -EXTEND = BEFORE: use the first value; AFTER: use the last value
      -ZERO   = BEFORE and AFTER: use zero
      -TREND  = compute a linear trend, and extrapolate BEFORE and AFTER
     The default is -EXTEND.  These options do NOT affect the operation
     of the 3 point filters described above, which always use -EXTEND.
    
    Adaptive Mean Filtering option [03 Oct 2014]
    --------------------------------------------
      -adaptive N = use adaptive mean filtering of width N
                    (where N must be odd and bigger than 3).
                  * This filter is similar to the 'AdptMean9'
                    1D filter in the AFNI GUI, except that the
                    end points are treated differently.
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
