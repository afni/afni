.. _ahelp_1dBport:

*******
1dBport
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dBport [options]
    
    Creates a set of columns of sines and cosines for the purpose of
    bandpassing via regression (e.g., in 3dDeconvolve).  Various option
    are given to specify the duration and structure of the time series
    to be created.  Results are written to stdout, and usually should be
    redirected appropriately (cf. EXAMPLES, infra).  The file produced
    could be used with the '-ortvec' option to 3dDeconvolve, for example.
    
    OPTIONS
    -------
     -band fbot ftop  = Specify lowest and highest frequencies in the passband.
                        fbot can be 0 if you want to do a highpass filter only;
                        on the other hand, if ftop > Nyquist frequency, then
                        it's a lowpass filter only.
                      ** This 'option' is actually mandatory! (At least once.)
                       * For the un-enlightened, the Nyquist frequency is the
                         highest frequency supported on the given grid, and
                         is equal to 0.5/TR (units are Hz if TR is in s).
                       * The lowest nonzero frequency supported on the grid
                         is equaly to 1/(N*TR), where N=number of time points.
                      ** Multiple -band options can be used, if needed.
                         If the bands overlap, regressors will NOT be duplicated.
                       * That is, '-band 0.01 0.05 -band 0.03 0.08' is the same
                         as using '-band 0.01 0.08'.
                      ** Note that if fbot==0 and ftop>=Nyquist frequency, you
                         get a 'complete' set of trig functions, meaning that
                         using these in regression is effectively a 'no-pass'
                         filter -- probably not what you want!
                      ** It is legitimate to set fbot = ftop.
                      ** The 0 frequency (fbot = 0) component is all 1, of course.
                         But unless you use the '-quad' option, nothing generated
                         herein will deal well with linear-ish or quadratic-ish
                         trends, which fall below the lowest nonzero frequency
                         representable in a full cycle on the grid:
                            f_low = 1 / ( NT * TR )
                         where NT = number of time points.
                      ** See the fourth EXAMPLE to learn how to use 3dDeconvolve
                         to generate a file of polynomials for regression fun.
    
     -invert          = After computing which frequency indexes correspond to the
                        input band(s), invert the selection -- that is, output
                        all those frequencies NOT selected by the -band option(s).
                        See the fifth EXAMPLE.
    
     -nozero          } Do NOT generate the 0 frequency (constant) component
       *OR            } when fbot = 0; this has the effect of setting fbot to
     -noconst         } 1/(N*TR), and is essentially a convenient way to say
                        'eliminate all oscillations below the ftop frequency'.
    
     -quad            = Add regressors for linear and quadratic trends.
                        (These will be the last columns in the output.)
    
     -input dataset   } One of these options is used to specify the number of
       *OR*           } time points to be created, as in 3dDeconvolve.
     -input1D 1Dfile  } ** '-input' allow catenated datasets, as in 3dDeconvolve.
       *OR*           } ** '-input1D' assumes TR=1 unless you use the '-TR' option.
     -nodata NT [TR]  } ** One of these options is mandatory, to specify the length
                           of the time series file to generate.
    
     -TR del          = Set the time step to 'del' rather than use the one
                        given in the input dataset (if any).
                       ** If TR is not specified by the -input dataset or by
                          -nodata or by -TR, the program will assume it is 1.0 s.
    
     -concat rname    = As in 3dDeconvolve, used to specify the list of start
                        indexes for concatenated runs.
                       ** Also as in 3dDeconvolve, if the -input dataset is auto-
                          catenated (by providing a list of more than one dataset),
                          the run start list is automatically generated.  Otherwise,
                          this option is needed if more than one run is involved.
    
    EXAMPLES
    --------
     The first example provides basis functions to filter out all frequency
     components from 0 to 0.25 Hz:
       1dBport -nodata 100 1 -band 0 0.25   > highpass.1D
    
     The second example provides basis functions to filter out all frequency
     components from 0.25 Hz up to the Nyquist freqency:
       1dBport -nodata 100 1 -band 0.25 666 > lowpass.1D
    
     The third example shows how to examine the results visually, for fun:
       1dBport -nodata 100 1 -band 0.41 0.43 | 1dplot -stdin -thick
    
     The fourth example shows how to use 3dDeconvolve to generate a file of
     polynomial 'orts', in case you find yourself needing this ability someday
     (e.g., when stranded on a desert isle, with Gilligan, the Skipper, et al.):
       3dDeconvolve -nodata 100 1 -polort 2 -x1D_stop -x1D stdout: | 1dcat stdin: > pol3.1D
    
     The fifth example shows how to use 1dBport to generate a set of regressors to
     eliminate all frequencies EXCEPT those in the selected range:
       1dBport -nodata 100 1 -band 0.03 0.13 -nozero -invert | 1dplot -stdin
     In this example, the '-nozero' flag is used because the next step will be to
     3dDeconvolve with '-polort 2' and '-ortvec' to get rid of the undesirable stuff.
    
    ETYMOLOGICAL NOTES
    ------------------
     * The word 'ort' was coined by Andrzej Jesmanowicz, as a shorthand name for
       a timeseries to which you want to 'orthogonalize' your data.
     * 'Ort' actually IS an English word, and means 'a scrap of food left from a meal'.
       As far as I know, its only usage in modern English is in crossword puzzles,
       and in Scrabble.
     * For other meanings of 'ort', see http://en.wikipedia.org/wiki/Ort
     * Do not confuse 'ort' with 'Oort': http://en.wikipedia.org/wiki/Oort_cloud
    
    AUTHOR -- RWCox -- Jan 2012
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
