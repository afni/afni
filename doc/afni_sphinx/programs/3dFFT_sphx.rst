.. _ahelp_3dFFT:

*****
3dFFT
*****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dFFT [options] dataset
    
    * Does the FFT of the input dataset in 3 directions (x,y,z) and
       produces the output dataset.
    
    * Why you'd want to do this is an interesting question.
    
    * Program 3dcalc can operate on complex-valued datasets, but
       only on one component at a time (cf. the '-cx2r' option).
    
    * Most other AFNI programs can only operate on real-valued
       datasets.
    
    * You could use 3dcalc (twice) to split a complex-valued dataset
       into two real-valued datasets, do your will on those with other
       AFNI programs, then merge the results back into a complex-valued
       dataset with 3dTwotoComplex.
    
    Options
    =======
     -abs       = Outputs the magnitude of the FFT [default]
     -phase     = Outputs the phase of the FFT (-PI..PI == no unwrapping!)
     -complex   = Outputs the complex-valued FFT
     -inverse   = Does the inverse FFT instead of the forward FFT
    
     -Lx xx     = Use FFT of length 'xx' in the x-direction
     -Ly yy     = Use FFT of length 'yy' in the y-direction
     -Lz zz     = Use FFT of length 'zz' in the z-direction
                  * Set a length to 0 to skip the FFT in that direction
    
     -altIN     = Alternate signs of input data before FFT, to bring
                   zero frequency from edge of FFT-space to center of grid
                   for cosmetic purposes.
     -altOUT    = Alternate signs of output data after FFT.  If you
                   use '-altI' on the forward transform, then you should
                   use '-altO' an the inverse transform, to get the
                   signs of the recovered image correct.
          **N.B.: You cannot use '-altIN' and '-altOUT' in the same run!
    
     -input dd  = Read the input dataset from 'dd', instead of
                   from the last argument on the command line.
    
     -prefix pp = Use 'pp' for the output dataset prefix.
    
    Notes
    =====
     * In the present avatar, only 1 sub-brick will be processed.
    
     * The program can only do FFT lengths that are factorable
        into a product of powers of 2, 3, and 5, and are even.
       + The largest power of 3 that is allowed is 3^3 = 27.
       + The largest power of 5 that is allowed is 5^3 = 125.
       + e.g., FFT of length 3*5*8=120 is possible.
       + e.g., FFT of length 4*31 =124 is not possible.
    
     * The 'x', 'y', and 'z' axes here refer to the order the
        data is stored, not DICOM coordinates; cf. 3dinfo.
    
     * If you force (via '-Lx' etc.) an FFT length that is not
        allowed, the program will stop with an error message.
    
     * If you force an FFT length that is shorter than an dataset
        axis dimension, the program will stop with an error message.
    
     * If you don't force an FFT length along a particular axis,
        the program will pick the smallest legal value that is
        greater than or equal to the corresponding dataset dimension.
       + e.g., 124 would be increased to 128.
    
     * If an FFT length is longer than an axis length, then the
        input data in that direction is zero-padded at the end.
    
     * For -abs and -phase, the output dataset is in float format.
    
     * If you do the forward and inverse FFT, then you should get back
        the original dataset, except for roundoff error and except that
        the new dataset axis dimensions may be longer than the original.
    
     * Forward FFT = sum_{k=0..N-1} [ exp(-2*PI*i*k/N) * data(k) ]
    
     * Inverse FFT = sum_{k=0..N-1} [ exp(+2*PI*i*k/N) * data(k) ] / N
    
     * Started a long time ago, but only finished in Aug 2009 at the
        request of John Butman, because he asked so nicely.  (Now pay up!)
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
