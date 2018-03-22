*****
waver
*****

.. _waver:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: waver [options] > output_filename
    Creates an ideal waveform timeseries file.
    The output goes to stdout, and normally would be redirected to a file.
    
    ---------
    Note Well
    ---------
    You should consider instead using program 3dDeconvolve to generate
    an ideal FMRI timeseries file.  For example:
    
      3dDeconvolve -polort -1 -nodata 100 1.0 -num_stimts 1     \
                   -stim_times 1 '1D: 10 30 50 70' 'BLOCK(5,1)' \
                   -x1D Ideal -x1D_stop
    
    will produce the file Ideal.xmat.1D, with 100 time points spaced
    at TR=1.0 seconds, with stimuli at 10, 30, 50, and 70 seconds,
    using the 'BLOCK' model with 5 seconds stimulus duration.
    
    The waver program is no longer being updated, since almost everything
    it does (and more) can be done in 3dDeconvolve -- RW Cox -- October 2010.
    
    --------
    Options: (# refers to a number; [xx] is the default value)
    --------
      -WAV = Sets waveform to Cox special                    [default]
               cf. AFNI FAQ list for formulas:
               https://afni.nimh.nih.gov/afni/doc/faq/17
      -GAM = Sets waveform to form t^b * exp(-t/c)
               (cf. Mark Cohen)
    
      -EXPR "expression" = Sets waveform to the expression given,
                             which should depend on the variable 't'.
         e.g.: -EXPR "step(t-2)*step(12-t)*(t-2)*(12-t)"
         N.B.: The peak value of the expression on the '-dt' grid will
               be scaled to the value given by '-peak'; if this is not
               desired, set '-peak 0', and the 'natural' peak value of
               the expression will be used.
    
      -FILE dt wname = Sets waveform to the values read from the file
                       'wname', which should be a single column .1D file
                       (i.e., 1 ASCII number per line).  The 'dt value
                       is the time step (in seconds) between lines
                       in 'wname'; the first value will be at t=0, the
                       second at t='dt', etc.  Intermediate time values
                       will be linearly interpolated.  Times past the
                       the end of the 'wname' file length will have
                       the waveform value set to zero.
                   *** N.B.: If the -peak option is used AFTER -FILE,
                             its value will be multiplied into the result.
    
    These options set parameters for the -WAV waveform.
      -delaytime #   = Sets delay time to # seconds                [2]
      -risetime #    = Sets rise time to # seconds                 [4]
      -falltime #    = Sets fall time to # seconds                 [6]
      -undershoot #  = Sets undershoot to # times the peak         [0.2]
                         (this should be a nonnegative factor)
      -restoretime # = Sets time to restore from undershoot        [2]
    
    These options set parameters for the -GAM waveform:
      -gamb #        = Sets the parameter 'b' to #                 [8.6]
      -gamc #        = Sets the parameter 'c' to #                 [0.547]
      -gamd #        = Sets the delay time to # seconds            [0.0]
    
    These options apply to all waveform types:
      -peak #        = Sets peak value to #                        [100]
      -dt #          = Sets time step of output AND input          [0.1]
      -TR #          = '-TR' is equivalent to '-dt'
    
    The default is just to output the waveform defined by the parameters
    above.  If an input file is specified by one the options below, then
    the timeseries defined by that file will be convolved with the ideal
    waveform defined above -- that is, each nonzero point in the input
    timeseries will generate a copy of the waveform starting at that point
    in time, with the amplitude scaled by the input timeseries value.
    
      -xyout         = Output data in 2 columns:
                         1=time 2=waveform (useful for graphing)
                         [default is 1 column=waveform]
    
      -input infile  = Read timeseries from *.1D formatted 'infile';
                         convolve with waveform to produce output
                  N.B.: you can use a sub-vector selector to choose
                        a particular column of infile, as in
                          -input 'fred.1D[3]'
    
      -inline DATA   = Read timeseries from command line DATA;
                         convolve with waveform to produce output
                         DATA is in the form of numbers and
                         count@value, as in
                         -inline 20@0.0 5@1.0 30@0.0 1.0 20@0.0 2.0
         which means a timeseries with 20 zeros, then 5 ones, then 30 zeros,
         a single 1, 20 more zeros, and a final 2.
         [The '@' character may actually be any of: '@', '*', 'x', 'X'.
          Note that * must be typed as \* to prevent the shell from
          trying to interpret it as a filename wildcard.]
    
      -tstim DATA    = Read discrete stimulation times from the command line
                         and convolve the waveform with delta-functions at
                         those times.  In this input format, the times do
                         NOT have to be at intervals of '-dt'.  For example
                           -dt 2.0 -tstim 5.6 9.3 13.7 16.4
                         specifies a TR of 2 s and stimuli at 4 times
                         (5.6 s, etc.) that do not correspond to integer
                         multiples of TR.  DATA values cannot be negative.
                       If the DATA is stored in a file, you can read it
                         onto the command line using something like
                           -tstim `cat filename`
                         where using the backward-single-quote operator
                         of the usual Unix shells.
       ** 12 May 2003: The times after '-tstim' can now also be specified
                         in the format 'a:b', indicating a continuous ON
                         period from time 'a' to time 'b'.  For example,
                           -dt 2.0 -tstim 13.2:15.7 20.3:25.3
                         The amplitude of a response of duration equal to
                         'dt' is equal the the amplitude of a single impulse
                         response (which is the special case a=b).  N.B.: This
                         means that something like '5:5.01' is very different
                         from '5' (='5:5').  The former will have a small amplitude
                         because of the small duration, but the latter will have
                         a large amplitude because the case of an instantaneous
                         input is special.  It is probably best NOT to mix the
                         two types of input to '-tstim' for this reason.
                         Compare the graphs from the 2 commands below:
                           waver -dt 1.0 -tstim 5:5.1 | 1dplot -stdin
                           waver -dt 1.0 -tstim 5     | 1dplot -stdin
                         If you prefer, you can use the form 'a%c' to indicate
                         an ON interval from time=a to time=a+c.
       ** 13 May 2005: You can now add an amplitude to each response individually.
                         For example
                           waver -dt 1.0 -peak 1.0 -tstim 3.2 17.9x2.0 23.1x-0.5
                         puts the default response amplitude at time 3.2,
                         2.0 times the default at time 17.9, and -0.5 times
                         the default at time 23.1.
    
      -when DATA     = Read time blocks when stimulus is 'on' (=1) from the
                         command line and convolve the waveform with with
                         a zero-one input.  For example:
                           -when 20..40 60..80
                         means that the stimulus function is 1.0 for time
                         steps number 20 to 40, and 60 to 80 (inclusive),
                         and zero otherwise.  (The first time step is
                         numbered 0.)
    
      -numout NN     = Output a timeseries with NN points; if this option
                         is not given, then enough points are output to
                         let the result tail back down to zero.
    
      -ver           = Output version information and exit.
    
    * Only one of the 3 timeseries input options above can be used at a time.
    * Using the AFNI program 1dplot, you can do something like the following,
      to check if the results make sense:
        waver -GAM -tstim 0 7.7 | 1dplot -stdin
    * Note that program 3dDeconvolve can now generate many different
      waveforms internally, markedly reducing the need for this program.
    * If a square wave is desired, see the 'sqwave' program.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
