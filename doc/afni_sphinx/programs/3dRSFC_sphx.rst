.. contents:: 
    :depth: 4 

******
3dRSFC
******

.. code-block:: none

    
      Program to calculate common resting state functional connectivity (RSFC)
      parameters (ALFF, mALFF, fALFF, RSFA, etc.) for resting state time
      series.  This program is **heavily** based on the existing
      3dBandPass by RW Cox, with the amendments to calculate RSFC
      parameters written by PA Taylor (July, 2012).
      This program is part of FATCAT (Taylor & Saad, 2013) in AFNI. Importantly,
      its functionality can be included in the `afni_proc.py' processing-script 
      generator; see that program's help file for an example including RSFC
      and spectral parameter calculation via the `-regress_RSFC' option.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      All options of 3dBandPass may be used here (with a couple other
      parameter options, as well): essentially, the motivation of this
      program is to produce ALFF, etc. values of the actual RSFC time
      series that you calculate.  Therefore, all the 3dBandPass processing
      you normally do en route to making your final `resting state time
      series' is done here to generate your LFFs, from which the
      amplitudes in the LFF band are calculated at the end.  In order to
      calculate fALFF, the same initial time series are put through the
      same processing steps which you have chosen but *without* the
      bandpass part; the spectrum of this second time series is used to
      calculate the fALFF denominator.
     
      For more information about each RSFC parameter, see, e.g.:   
      ALFF/mALFF -- Zang et al. (2007),
      fALFF --      Zou et al. (2008),
      RSFA --       Kannurpatti & Biswal (2008).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
     + USAGE: 3dRSFC [options] fbot ftop dataset
    
    * One function of this program is to prepare datasets for input
       to 3dSetupGroupInCorr.  Other uses are left to your imagination.
    
    * 'dataset' is a 3D+time sequence of volumes
       ++ This must be a single imaging run -- that is, no discontinuities
           in time from 3dTcat-ing multiple datasets together.
    
    * fbot = lowest frequency in the passband, in Hz
       ++ fbot can be 0 if you want to do a lowpass filter only;
           HOWEVER, the mean and Nyquist freq are always removed.
    
    * ftop = highest frequency in the passband (must be > fbot)
       ++ if ftop > Nyquist freq, then it's a highpass filter only.
    
    * Set fbot=0 and ftop=99999 to do an 'allpass' filter.
      ++ Except for removal of the 0 and Nyquist frequencies, that is.
    
    * You cannot construct a 'notch' filter with this program!
      ++ You could use 3dRSFC followed by 3dcalc to get the same effect.
      ++ If you are understand what you are doing, that is.
      ++ Of course, that is the AFNI way -- if you don't want to
         understand what you are doing, use Some other PrograM, and
         you can still get Fine StatisticaL maps.
    
    * 3dRSFC will fail if fbot and ftop are too close for comfort.
      ++ Which means closer than one frequency grid step df,
         where df = 1 / (nfft * dt) [of course]
    
    * The actual FFT length used will be printed, and may be larger
       than the input time series length for the sake of efficiency.
      ++ The program will use a power-of-2, possibly multiplied by
         a power of 3 and/or 5 (up to and including the 3rd power of
         each of these: 3, 9, 27, and 5, 25, 125).
    
    * Note that the results of combining 3dDetrend and 3dRSFC will
       depend on the order in which you run these programs.  That's why
       3dRSFC has the '-ort' and '-dsort' options, so that the
       time series filtering can be done properly, in one place.
    
    * The output dataset is stored in float format.
    
    * The order of processing steps is the following (most are optional), and
      for the LFFs, the bandpass is done between the specified fbot and ftop,
      while for the `whole spectrum' (i.e., fALFF denominator) the bandpass is:
      done only to exclude the time series mean and the Nyquist frequency:
     (0) Check time series for initial transients [does not alter data]
     (1) Despiking of each time series
     (2) Removal of a constant+linear+quadratic trend in each time series
     (3) Bandpass of data time series
     (4) Bandpass of -ort time series, then detrending of data
          with respect to the -ort time series
     (5) Bandpass and de-orting of the -dsort dataset,
          then detrending of the data with respect to -dsort
     (6) Blurring inside the mask [might be slow]
     (7) Local PV calculation     [WILL be slow!]
     (8) L2 normalization         [will be fast.]
     (9) Calculate spectrum and amplitudes, for RSFC parameters.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    --------
    OPTIONS:
    --------
     -despike        = Despike each time series before other processing.
                       ++ Hopefully, you don't actually need to do this,
                          which is why it is optional.
     -ort f.1D       = Also orthogonalize input to columns in f.1D
                       ++ Multiple '-ort' options are allowed.
     -dsort fset     = Orthogonalize each voxel to the corresponding
                        voxel time series in dataset 'fset', which must
                        have the same spatial and temporal grid structure
                        as the main input dataset.
                       ++ At present, only one '-dsort' option is allowed.
     -nodetrend      = Skip the quadratic detrending of the input that
                        occurs before the FFT-based bandpassing.
                       ++ You would only want to do this if the dataset
                          had been detrended already in some other program.
     -dt dd          = set time step to 'dd' sec [default=from dataset header]
     -nfft N         = set the FFT length to 'N' [must be a legal value]
     -norm           = Make all output time series have L2 norm = 1
                       ++ i.e., sum of squares = 1
     -mask mset      = Mask dataset
     -automask       = Create a mask from the input dataset
     -blur fff       = Blur (inside the mask only) with a filter
                        width (FWHM) of 'fff' millimeters.
     -localPV rrr    = Replace each vector by the local Principal Vector
                        (AKA first singular vector) from a neighborhood
                        of radius 'rrr' millimiters.
                       ++ Note that the PV time series is L2 normalized.
                       ++ This option is mostly for Bob Cox to have fun with.
    
     -input dataset  = Alternative way to specify input dataset.
     -band fbot ftop = Alternative way to specify passband frequencies.
    
     -prefix ppp     = Set prefix name of output dataset. Name of filtered time
                       series would be, e.g., ppp_LFF+orig.*, and the parameter
                       outputs are named with obvious suffices.
     -quiet          = Turn off the fun and informative messages. (Why?)
     -no_rs_out      = Don't output processed time series-- just output
                       parameters (not recommended, since the point of
                       calculating RSFC params here is to have them be quite
                       related to the time series themselves which are used for
                       further analysis). -un_bp_out      = Output the un-bandpassed series as well (default is not 
                       to).  Name would be, e.g., ppp_unBP+orig.* .
                       with suffix `_unBP'.
     -no_rsfa        = If you don't want RSFA output (default is to do so).
     -bp_at_end      = A (probably unnecessary) switch to have bandpassing be 
                       the very last processing step that is done in the
                       sequence of steps listed above; at Step 3 above, only 
                       the time series mean and nyquist are BP'ed out, and then
                       the LFF series is created only after Step 9.  NB: this 
                       probably makes only very small changes for most
                       processing sequences (but maybe not, depending usage).
    
     -notrans        = Don't check for initial positive transients in the data:
      *OR*             ++ The test is a little slow, so skipping it is OK,
     -nosat               if you KNOW the data time series are transient-free.
                       ++ Or set AFNI_SKIP_SATCHECK to YES.
                       ++ Initial transients won't be handled well by the
                          bandpassing algorithm, and in addition may seriously
                          contaminate any further processing, such as inter-
                          voxel correlations via InstaCorr.
                       ++ No other tests are made [yet] for non-stationary 
                          behavior in the time series data.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
    ____________________________________________________________________________
    
     =========================================================================
    * This binary version of  3dRSFC is NOT compiled using OpenMP, a
       semi-automatic parallelizer software toolkit, which splits the work
       across multiple CPUs/cores on the same shared memory computer.
    * However, the source code is modified for OpenMP, and can be compiled
       with an OpenMP-capable compiler, such as gcc 4.2+, Intel's icc, and
       Sun Studio.
    * If you wish to compile this program with OpenMP, see the man page for
       your C compiler, and (if needed) consult the AFNI message board, and
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
