*********
FIRdesign
*********

.. _FIRdesign:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: FIRdesign [options] fbot ftop ntap
    
    Uses the Remez algorithm to calculate the FIR filter weights
    for a bandpass filter; results are written to stdout in an
    unadorned (no header) column of numbers.
    Inputs are
      fbot = lowest freqency in the pass band.
      ftop = highest frequency in the pass band.
            * 0 <= fbot < ftop <= 0.5/TR
            * Unless the '-TR' option is given, TR=1.
      ntap = Number of filter weights (AKA 'taps') to use.
            * Define df = 1/(ntap*TR) = frequency resolution:
            * Then if fbot < 1.1*df, it will be replaced by 0;
              in other words, a pure lowpass filter.  This change
              is necessary since the duration ntap*TR must be longer
              than 1 full cycle of the lowest frequency (1/fbot) in
              order to filter out slower frequency components.
            * Similarly, if ftop > 0.5/TR-1.1*df, it will be
              replaced by 0.5/TR; in other words, a pure
              highpass filter.
            * If ntap is odd, it will be replaced by ntap+1.
            * ntap must be in the range 8..2000 (inclusive).
    
    OPTIONS:
    --------
     -TR dd          = Set time grid spacing to 'dd' [default is 1.0]
     -band fbot ftop = Alternative way to specify the passband
     -ntap nnn       = Alternative way to specify the number of taps
    
    EXAMPLES:
    ---------
      FIRdesign 0.01 0.10 180 | 1dplot -stdin
      FIRdesign 0.01 0.10 180 | 1dfft -nodetrend -nfft 512 stdin: - \
                | 1dplot -stdin -xaxis 0:0.5:10:10 -dt 0.001953
    
    The first line plots the filter weights
    The second line plots the frequency response (0.001953 = 1/512)
    
    NOTES:
    ------
    * http://en.wikipedia.org/wiki/Parks-McClellan_filter_design_algorithm
    * The Remez algorithm code is written and GPL-ed by Jake Janovetz
    * Multiple passbands could be designed this way; let me know if you
      need such an option; a Hilbert transform FIR is also possible
    * Don't try to be stupidly clever when using this program
