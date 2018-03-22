*********
3dFourier
*********

.. _ahelp_3dFourier:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    3dFourier 
    (c) 1999 Medical College of Wisconsin
    by T. Ross and K. Heimerl
    Version 0.8 last modified 8-17-99
    
    Usage: 3dFourier [options] dataset
    
    * Program to lowpass and/or highpass each voxel time series in a
      dataset, via the FFT.
    * Also see program 3dBandpass for something similar.
    
    The parameters and options are:
    	dataset		an afni compatible 3d+time dataset to be operated upon
    	-prefix name	output name for new 3d+time dataset [default = fourier]
    	-lowpass f 	low pass filter with a cutoff of f Hz
    	-highpass f	high pass filter with a cutoff of f Hz
    	-ignore n	ignore the first n images [default = 1]
    	-retrend	Any mean and linear trend are removed before filtering.
    			This will restore the trend after filtering.
    
    Note that by combining the lowpass and highpass options, one can construct
    bandpass and notch filters
    
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
