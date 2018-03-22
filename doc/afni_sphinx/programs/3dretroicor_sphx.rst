***********
3dretroicor
***********

.. _ahelp_3dretroicor:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dretroicor [options] dataset
    
    Performs Retrospective Image Correction for physiological
    motion effects, using a slightly modified version of the
    RETROICOR algorithm described in:
    
      Glover, G. H., Li, T., & Ress, D. (2000). Image-based method
    for retrospective correction of physiological motion effects in
    fMRI: RETROICOR. Magnetic Resonance in Medicine, 44, 162-167.
    
    Options (defaults in []'s):
    
     -ignore    = The number of initial timepoints to ignore in the
                  input (These points will be passed through
                  uncorrected) [0]
     -prefix    = Prefix for new, corrected dataset [retroicor]
    
     -card      = 1D cardiac data file for cardiac correction
     -cardphase = Filename for 1D cardiac phase output
     -threshold = Threshold for detection of R-wave peaks in input
                  (Make sure it's above the background noise level;
                  Try 3/4 or 4/5 times range plus minimum) [1]
    
     -resp      = 1D respiratory waveform data for correction
     -respphase = Filename for 1D resp phase output
    
     -order     = The order of the correction (2 is typical;
                  higher-order terms yield little improvement
                  according to Glover et al.) [2]
    
     -help      = Display this message and stop (must be first arg)
    
    Dataset: 3D+time dataset to process
    
    ** The input dataset and at least one of -card and -resp are
        required.
    
    NOTES
    -----
    
    The durations of the physiological inputs are assumed to equal
    the duration of the dataset. Any constant sampling rate may be
    used, but 40 Hz seems to be acceptable. This program's cardiac
    peak detection algorithm is rather simplistic, so you might try
    using the scanner's cardiac gating output (transform it to a
    spike wave if necessary).
    
    This program uses slice timing information embedded in the
    dataset to estimate the proper cardiac/respiratory phase for
    each slice. It makes sense to run this program before any
    program that may destroy the slice timings (e.g. 3dvolreg for
    motion correction).
    
    Author -- Fred Tam, August 2002
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
