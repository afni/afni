**********
1dBandpass
**********

.. _1dBandpass:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 1dBandpass [options] fbot ftop infile
    
     * infile is an AFNI *.1D file; each column is processed
     * fbot = lowest frequency in the passband, in Hz
              [can be 0 if you want to do a lowpass filter only,]
               but the mean and Nyquist freq are always removed ]
     * ftop = highest frequency in the passband (must be > fbot)
              [if ftop > Nyquist freq, then we have a highpass filter only]
     * You cannot construct a 'notch' filter with this program!
     * Output vectors appear on stdout; redirect as desired
     * Program will fail if fbot and ftop are too close for comfort
     * The actual FFT length used will be printed, and may be larger
       than the input time series length for the sake of efficiency.
    
    Options:
      -dt dd     = set time step to 'dd' sec [default = 1.0]
      -ort f.1D  = Also orthogonalize input to columns in f.1D
                   [only one '-ort' option is allowed]
      -nodetrend = Skip the quadratic detrending of the input
      -norm      = Make output time series have L2 norm = 1
    
    Example:
      1deval -num 1000 -expr 'gran(0,1)' > r1000.1D
      1dBandpass 0.025 0.20 r1000.1D  > f1000.1D
      1dfft f1000.1D - | 1dplot -del 0.000977 -stdin -plabel 'Filtered |FFT|'
    
    Goal:
     * Mostly to test the functions in thd_bandpass.c -- RWCox -- May 2009
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
