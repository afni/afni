*******
3dTRfix
*******

.. _3dTRfix:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dTRfix [options]
    
    This program will read in a dataset that was sampled on an irregular time
    grid and re-sample it via linear interpolation to a regular time grid.
    
    NOTES:
    ------
    The re-sampling will include the effects of slice time offsets (similarly
    to program 3dTshift), if these time offsets are encoded in the input dataset's
    header.
    
    No other processing is performed -- in particular, there is no allowance
    (at present) for T1 artifacts resulting from variable TR.
    
    If the first 1 or 2 time points are abnormally bright due to the NMR
    pre-steady-state effect, then their influence might be spread farther
    into the output dataset by the interpolation process.  You can avoid this
    effect by excising these values from the input using the '[2..$]' notation
    in the input dataset syntax.
    
    If the input dataset is catenated from multiple non-contiguous imaging runs,
    the program will happily interpolate across the time breaks between the runs.
    For this reason, you should not give such a file (e.g., from 3dTcat) to this
    program -- you should use 3dTRfix on each run separately, and only later
    catenate the runs.
    
    The output dataset is stored in float format, regardless of the input format.
    
    ** Basically, this program is a hack for the Mad Spaniard.
    ** When are we going out for tapas y cerveza (sangria es bueno, tambien)?
    
    OPTIONS:
    --------
    
     -input iii    = Input dataset 'iii'. [MANDATORY]
    
     -TRlist rrr   = 1D columnar file of time gaps between sub-bricks in 'iii';
                     If the input dataset has N time points, this file must
                     have at least N-1 (positive) values.
                    * Please note that these time steps (or the time values in
                      '-TIMElist') should be in seconds, NOT in milliseconds!
                    * AFNI time units are seconds!!!
    
     -TIMElist ttt = Alternative to '-TRlist', where you give the N values of
                     the times at each sub-brick; these values must be monotonic
                     increasing and non-negative.
                    * You must give exactly one of '-TIMElist' or '-TRlist'.
                    * The TR value given in the input dataset header is ignored.
    
     -prefix ppp   = Prefix name for the output dataset.
    
     -TRout ddd    = 'ddd' gives the value for the output dataset's TR (in sec).
                     If '-TRout' is not given, then the average TR of the input
                     dataset will be used.
    
    November 2014 -- Zhark the Fixer
