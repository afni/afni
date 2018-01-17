********
stimband
********

.. _stimband:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage: stimband [options] matrixfile ...
    
    The purpose of this program is to give a frequency band
    that covers at least 90% of the 'power' (|FFT|^2) of the
    stimulus columns taken from one or more X.nocensor.xmat.1D
    files output by 3dDeconvolve.  The band (2 frequencies
    in Hertz) are printed to stdout.  This program is meant
    to be used in a script to decide on the passband for
    various pre- and post-processing steps in AFNI.
    
    If the output band is '0 0', this indicates that the input
    matrices did not have any valid columns marked as stimuli;
    this would be the case, for example, if the matrices had
    been generated solely for use in resting-state FMRI denoising.
    
    Options:
    --------
     -verb          = print (to stderr) the power band for each
                      individual stimulus column from each matrix.
     -matrix mmm    = another way to read 1 or more matrix files.
     -min_freq aa   = set the minimum frequency output for the
                      band to 'aa' [default value = 0.01].
     -min_bwidth bb = set the minimum bandwidth output (top frequency
                      minus bottom frequency) to 'bb' [default = 0.03].
     -min_pow ff    = set the minimum power fraction to 'ff'% instead
                      of the default 90%; ff must be in the range
                      50..99 (inclusive).
    
