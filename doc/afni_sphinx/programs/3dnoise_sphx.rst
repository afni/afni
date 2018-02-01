*******
3dnoise
*******

.. _3dnoise:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dnoise [-blast] [-snr fac] [-nl x ] datasets ...
    Estimates noise level in 3D datasets, and optionally
    set voxels below the noise threshold to zero.
    This only works on datasets that are stored as shorts,
    and whose elements are all nonnegative.
      -blast   = Set values at or below the cutoff to zero.
                   In 3D+time datasets, a spatial location
                   is set to zero only if a majority of time
                   points fall below the cutoff; in that case
                   all the values at that location are zeroed.
      -snr fac = Set cutoff to 'fac' times the estimated
                   noise level.  Default fac = 2.5.  What to
                   use for this depends strongly on your MRI
                   system -- I often use 5, but our true SNR
                   is about 100 for EPI.
      -nl x    = Set the noise level to 'x', skipping the
                   estimation procedure.  Also sets fac=1.0.
                   You can use program 3dClipLevel to get an
                   estimate of a value for 'x'.
    Author -- RW Cox
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
