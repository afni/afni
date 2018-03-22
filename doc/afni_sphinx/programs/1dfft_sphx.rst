*****
1dfft
*****

.. _ahelp_1dfft:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dfft [options] infile outfile
    where infile is an AFNI *.1D file (ASCII list of numbers arranged
    in columns); outfile will be a similar file, with the absolute
    value of the FFT of the input columns.  The length of the file
    will be 1+(FFT length)/2.
    
    Options:
      -ignore sss = Skip the first 'sss' lines in the input file.
                    [default = no skipping]
      -use uuu    = Use only 'uuu' lines of the input file.
                    [default = use them all, Frank]
      -nfft nnn   = Set FFT length to 'nnn'.
                    [default = length of data (# of lines used)]
      -tocx       = Save Re and Im parts of transform in 2 columns.
      -fromcx     = Convert 2 column complex input into 1 column
                      real output.
      -hilbert    = When -fromcx is used, the inverse FFT will
                      do the Hilbert transform instead.
      -nodetrend  = Skip the detrending of the input.
    
    Nota Bene:
     * Each input time series has any quadratic trend of the
         form 'a+b*t+c*t*t' removed before the FFT, where 't'
         is the line number.
     * The FFT length will be a power-of-2 times at most one
         factor of 3 and one factor of 5.  The smallest such
         length >= to the specified FFT length will be used.
     * If the FFT length is longer than the file length, the
         data is zero-padded to make up the difference.
     * Do NOT call the output of this program the Power Spectrum!
         That is something else entirely.
     * If 'outfile' is '-', the output appears on stdout.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
