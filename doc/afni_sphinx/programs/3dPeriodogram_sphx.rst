*************
3dPeriodogram
*************

.. _3dPeriodogram:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dPeriodogram [options] dataset
    Computes the periodogram of each voxel time series.
    (Squared FFT = a crude estimate of the power spectrum)
    
    --------
    Options:
    --------
     -prefix p = use string 'p' for the prefix of the
                   output dataset [DEFAULT = 'pgram']
     -taper    = fraction of data to taper [DEFAULT = 0.1]
     -nfft L   = set FFT length to 'L' points
                   (longer than the data ==> zero padding)
                   (shorter than the data ==> data pruning)
    ------
    Notes:
    ------
    * Output is in float format; number of sub-bricks will be
       half the FFT length; sub-brick #0 = FFT bin #1, etc.
    * Grid spacing in the frequency (sub-brick) dimension will
       be 1/(nfft*TR) where nfft=FFT length, TR=dataset timestep.
    * There is no '-mask' option.  The hyper-clever user could
       use something like
         '3dcalc( -a dset+orig -b mask+orig -expr a*b )'
       to apply a binary mask on the command line.
    * Data is not scaled exactly as in the AFNI Power plugin.
    * Each time series is linearly detrended prior to FFT-ization.
    * FFT length defaults to be the next legal length >= input dataset.
    * The program can only do FFT lengths that are factorable
       into a product of powers of 2, 3, and 5, and are even.
      ++ The largest power of 3 that is allowed is 3^3 = 27.
      ++ The largest power of 5 that is allowed is 5^3 = 125.
      ++ e.g., FFT of length 3*5*8=120 is possible.
      ++ e.g., FFT of length 4*31 =124 is not possible.
      ++ '-nfft' with an illegal value will cause the program to fail.
    * If you want to do smaller FFTs, then average the periodograms
       (to reduce random fluctuations), you can use 3dPeriodogram in
       a script with "[...]" sub-brick selectors, then average
       the results with 3dMean.
    * Or you could use the full-length FFT, then smooth that FFT
       in the frequency direction (e.g., with 3dTsmooth).
    * This is a really quick hack for DH and PB and SfN.
    
    * Author = RWCox -- who doesn't want any bribe at all for this!
                     -- http://ethics.od.nih.gov/topics/gifts.htm
    
    ---------------------------------------------------
    More Details About What 3dPeriodogram Actually Does
    ---------------------------------------------------
    * Tapering is done with the Hamming window (if taper > 0):
        Define npts   = number of time points analyzed (<= nfft)
                        (i.e., the length of the input dataset)
               ntaper = taper * npts / 2        (0 < taper <= 1)
                      = number of points to taper on each end
               ktop   = npts - ntaper
               phi    = PI / ntaper
        Then the k-th point (k=0..nfft-1) is tapered by
          w(k) = 0.54 - 0.46 * cos(k*phi)           0    <= k < ntaper
          w(k) = 0.54 + 0.46 * cos((k-ktop+1)*phi)  ktop <= k < npts
          w(k) = 1.0                                otherwise
        Also define P = sum{ w(k)*w(k) } from k=0..npts-1
        (if ntaper = 0, then P = npts).
    
    * The result is the squared magnitude of the FFT of w(k)*data(k),
      divided by P.  This division makes the result be the 'power',
      which is to say the data's sum-of-squares ('energy') per unit
      time (in units of 1/TR, not 1/sec) ascribed to each FFT bin.
    
    * Normalizing by P also means that the values output for different
      amounts of tapering or different lengths of data are comparable.
    
    * To be as clear as I can: this program does NOT do any averaging
      across multiple windows of the data (such as Welch's method does)
      to estimate the power spectrum.  This program:
      ++ tapers the data,
      ++ zero-pads it to the FFT length,
      ++ FFTs it (in time),
      ++ squares it and divides by the P factor.
    
    * The number of output sub-bricks is nfft/2:
         sub-brick #0 = FFT bin #1 = frequency 1/(nfft*dt)
                   #1 = FFT bin #2 = frequency 2/(nfft*dt)
      et cetera, et cetera, et cetera.
    
    * If you desire to implement Welch's method for spectrum estimation
      using 3dPeriodogram, you will have to run the program multiple
      times, using different subsets of the input data, then average
      the results with 3dMean.
      ++ http://en.wikipedia.org/wiki/Welch's_method
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
