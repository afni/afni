.. contents:: 
    :depth: 4 

**********
3dUpsample
**********

.. code-block:: none

    Usage: 3dUpsample [options] n dataset
    
    * Upsamples a 3D+time dataset, in the time direction,
       by a factor of 'n'.
    * The value of 'n' must be between 2 and 320 (inclusive).
    * The output dataset is in float format by default.
    
    Options:
    --------
     -1 or -one = Use linear interpolation. Otherwise,
     or -linear   7th order polynomial interpolation is used.
    
     -prefix pp = Define the prefix name of the output dataset.
                  [default prefix is 'Upsam']
    
     -verb      = Be eloquently and mellifluosly verbose.
    
     -n n       = An alternate way to specify n
     -input dataset = An alternate way to specify dataset
    
     -datum ddd = Use datatype ddd at output. Choose from
                  float (default), short, byte.
    Example:
    --------
     3dUpsample -prefix LongFred 5 Fred+orig
    
    Nota Bene:
    ----------
    * You should not use this for files that were 3dTcat-ed across
       imaging run boundaries, since that will result in interpolating
       between non-contiguous time samples!
    * If the input has M time points, the output will have n*M time
       points.  The last n-1 of them will be past the end of the original
       time series.
    * This program gobbles up memory and diskspace as a function of n.
      You can reduce output file size with -datum option.
    
    --- RW Cox - April 2008
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
