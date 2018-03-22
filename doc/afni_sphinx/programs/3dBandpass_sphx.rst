**********
3dBandpass
**********

.. _3dBandpass:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    --------------------------------------------------------------------------
    ** NOTA BENE:  For the purpose of preparing resting-state FMRI datasets **
    ** for analysis (e.g., with 3dGroupInCorr),  this program is now mostly **
    ** superseded by the afni_proc.py script.  See the 'afni_proc.py -help' **
    ** section 'Resting state analysis (modern)' to get our current rs-FMRI **
    ** pre-processing recommended sequence of steps. -- RW Cox, et alii.    **
    --------------------------------------------------------------------------
    ** If you insist on doing your own bandpassing, I now recommend using   **
    ** program 3dTproject instead of this program.  3dTproject also can do  **
    ** censoring and other nuisance regression at the same time -- RW Cox.  **
    --------------------------------------------------------------------------
    
    Usage: 3dBandpass [options] fbot ftop dataset
    
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
      ++ You could use 3dBandpass followed by 3dcalc to get the same effect.
      ++ If you are understand what you are doing, that is.
      ++ Of course, that is the AFNI way -- if you don't want to
         understand what you are doing, use Some other PrograM, and
         you can still get Fine StatisticaL maps.
    
    * 3dBandpass will fail if fbot and ftop are too close for comfort.
      ++ Which means closer than one frequency grid step df,
         where df = 1 / (nfft * dt) [of course]
    
    * The actual FFT length used will be printed, and may be larger
       than the input time series length for the sake of efficiency.
      ++ The program will use a power-of-2, possibly multiplied by
         a power of 3 and/or 5 (up to and including the 3rd power of
         each of these: 3, 9, 27, and 5, 25, 125).
    
    * Note that the results of combining 3dDetrend and 3dBandpass will
       depend on the order in which you run these programs.  That's why
       3dBandpass has the '-ort' and '-dsort' options, so that the
       time series filtering can be done properly, in one place.
    
    * The output dataset is stored in float format.
    
    * The order of processing steps is the following (most are optional):
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
    
     -prefix ppp     = Set prefix name of output dataset.
     -quiet          = Turn off the fun and informative messages. (Why?)
    
     -notrans        = Don't check for initial positive transients in the data:
      *OR*             ++ The test is a little slow, so skipping it is OK,
     -nosat               if you KNOW the data time series are transient-free.
                       ++ Or set AFNI_SKIP_SATCHECK to YES.
                       ++ Initial transients won't be handled well by the
                          bandpassing algorithm, and in addition may seriously
                          contaminate any further processing, such as inter-voxel
                          correlations via InstaCorr.
                       ++ No other tests are made [yet] for non-stationary behavior
                          in the time series data.
    
     =========================================================================
    * This binary version of 3dBandpass is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system. You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful? That varies with the program, and how well
       it was coded. You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 12.
    * At present, the only part of 3dBandpass that is parallelized is the
      '-blur' option, which processes each sub-brick independently.
     =========================================================================
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
