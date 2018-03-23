.. _ahelp_3dTsort:

*******
3dTsort
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTsort [options] dataset
    Sorts each voxel and produces a new dataset.
    
    Options:
     -prefix p = use string 'p' for the prefix of the
                   output dataset [DEFAULT = 'tsort']
     -inc      = sort into increasing order [default]
     -dec      = sort into decreasing order
     -rank     = output rank instead of sorted values
                 ranks range from 1 to Nvals
     -ind      = output sorting index. (0 to Nvals -1)
                 See example below.
     -val      = output sorted values (default)
     -random   = randomly shuffle (permute) the time points in each voxel
                 * Each voxel is permuted independently!
                 * Why is this here? Someone asked for it :)
     -ranFFT   = randomize each time series by scrambling the FFT phase
                 * Each voxel is treated separately!
                 * Why is this here? cf. Matthew 7:7-8 :)
     -ranDFT   = Almost the same as above, but:
                 * In '-ranFFT', the FFT length is taken
                   to be the next integer >= data length
                   for which the FFT algorithm is efficient.
                   This will result in data padding unless
                   the data length is exactly 'nice' for FFT.
                 * In '-ranDFT', the DFT length is exactly
                   the data length. If the data length is
                   a large-ish prime number (say 997), this
                   operation can be slow.
                 * The DFT/FFT algorithm is reasonably fast
                   when the data length prime factors contain
                   only 2s, 3s, and/or 5s.
                 * Using '-ranDFT' can preserve the spectral
                   (temporal correlation) structure of the
                   original data a little better than '-ranFFT'.
                 * The only reason to use '-ranFFT' instead of
                   '-ranDFT' is for speed. For example, with
                   997 time points, '-ranFFT' was about 13 times
                   faster (FFT length=1000) than '-ranDFT'.
     -datum D  = Coerce the output data to be stored as 
                 the given type D, which may be  
                 byte, short, or float (default).         
    
    Notes:
    * Each voxel is sorted (or processed) separately.
    * Sub-brick labels are not rearranged!
    * This program is useful only in limited cases.
       It was written to sort the -stim_times_IM
       beta weights output by 3dDeconvolve.
    * Also see program 1dTsort, for sorting text files of numbers.
    
    Examples:
    setenv AFNI_1D_TIME YES
    echo '8 6 3 9 2 7' > test.1D
        3dTsort -overwrite test.1D 
        1dcat tsort.1D
    
        3dTsort -overwrite -rank test.1D 
        1dcat tsort.1D
    
    
        3dTsort -overwrite -ind test.1D 
        1dcat tsort.1D
    
        3dTsort -overwrite -dec test.1D 
        1dcat tsort.1D
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
