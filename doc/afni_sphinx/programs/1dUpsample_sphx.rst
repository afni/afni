**********
1dUpsample
**********

.. _ahelp_1dUpsample:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Program 1dUpsample:
    Upsamples a 1D time series (along the column direction)
    to a finer time grid.
    Usage:  1dUpsample [options] n fred.1D > ethel.1D
    
    Where 'n' is the upsample factor (integer from 2..32)
    
    NOTES:
    ------
    * Interpolation is done with 7th order polynomials.
       (Why 7? It's a nice number, and the code already existed.)
    * The only option is '-1' or '-one', to use 1st order
       polynomials instead (i.e., linear interpolation).
    * Output is written to stdout.
    * If you want to interpolate along the row direction,
       transpose before input, then transpose the output.
    * Example:
       1dUpsample 5 '1D: 4 5 4 3 4' | 1dplot -stdin -dx 0.2 
    * If the input has M time points, the output will
       have n*M time points.  The last n-1 of them
       will be past the end of the original time series.
    * This program is a quick hack for Gang Chen.
       Where are my Twizzlers?
