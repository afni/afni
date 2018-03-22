*****
3dDFT
*****

.. _3dDFT:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dDFT [options] dataset
           where 'dataset' is complex- or float-valued.
     * Carries out the DFT along the time axis.
     * To do the DFT along the spatial axes, use program 3dFFT.
     * The input dataset can be complex-valued or float-valued.
       If it is any other data type, it will be converted to floats
       before processing.
    
    OPTIONS:
    --------
     -abs     == output float dataset = abs(DFT)
                * Otherwise, the output file is complex-valued.
                  You can then use 3dcalc to extract the real part, the
                  imaginary part, the phase, etc.; see its '-cx2r' option:
                    3dcalc  -cx2r REAL -a cxset+orig-expr a -prefix rset+orig
                * Please note that if you view a complex dataset in AFNI,
                  the default operation is that you are looking at the
                  absolute value of the dataset.
                 ++ You can control the way a complex IMAGE appears via
                    the 'Disp' control panel (ABS, PHASE, REAL, IMAGE).
                 ++ You can control the way a complex TIME SERIES graph appears
                    via environment variable AFNI_GRAPH_CX2R (in 'EditEnv').
    
     -nfft N  == use 'N' for DFT length (must be >= #time points)
    
     -detrend == least-squares remove linear drift before DFT
                  [for more intricate detrending, use 3dDetrend first]
    
     -taper f == taper 'f' fraction of data at ends (0 <= f <= 1).
                  [Hamming 'raised cosine' taper of f/2 of the ]
                  [data length at each end; default is no taper]
                  [cf. 3dPeriodogam -help for tapering details!]
    
     -inverse == Do the inverse DFT:
                   SUM{ data[j] * exp(+2*PI*i*j/nfft) } * 1/nfft
                 instead of the forward transform
                   SUM{ data[j] * exp(-2*PI*i*j/nfft) }
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
