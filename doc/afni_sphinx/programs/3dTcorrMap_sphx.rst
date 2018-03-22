**********
3dTcorrMap
**********

.. _3dTcorrMap:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTcorrMap [options]
    For each voxel time series, computes the correlation between it
    and all other voxels, and combines this set of values into the
    output dataset(s) in some way.
    
    Supposed to give a measure of how 'connected' each voxel is
    to the rest of the brain.  [[As if life were that simple.]]
    
    ---------
    WARNINGS:
    ---------
    ** This program takes a LONG time to run.
    ** This program will use a LOT of memory.
    ** Don't say I didn't warn you about these facts, and don't whine.
    
    --------------
    Input Options:
    --------------
      -input dd = Read 3D+time dataset 'dd' (a mandatory option).
                   This provides the time series to be correlated
                   en masse.
                ** This is a non-optional 'option': you MUST supply
                   and input dataset!
    
      -seed bb  = Read 3D+time dataset 'bb'.
                ** If you use this option, for each voxel in the
                    -seed dataset, its time series is correlated
                    with every voxel in the -input dataset, and
                    then that collection of correlations is processed
                    to produce the output for that voxel.
                ** If you don't use -seed, then the -input dataset
                    is the -seed dataset [i.e., the normal usage].
                ** The -seed and -input datasets must have the
                    same number of time points and the same number
                    of voxels!
                ** Unlike the -input dataset, the -seed dataset is not
                    preprocessed (i.e., no detrending/bandpass or blur).
                     (The main purpose of this -seed option is to)
                     (allow you to preprocess the seed voxel time)
                     (series in some personalized and unique way.)
    
      -mask mmm = Read dataset 'mmm' as a voxel mask.
      -automask = Create a mask from the input dataset.
                ** -mask and -automask are mutually exclusive!
                ** If you don't use one of these masking options, then
                   all voxels will be processed, and the program will
                   probably run for a VERY long time.
                ** Voxels with constant time series will be automatically
                   excluded.
    
    ----------------------------------
    Time Series Preprocessing Options: (applied only to -input, not to -seed)
    [[[[ In general, it would be better to pre-process with afni_proc.py ]]]]
    ----------------------------------
    TEMPORAL FILTERING:
    -------------------
      -polort m  = Remove polynomial trend of order 'm', for m=-1..19.
                    [default is m=1; removal is by least squares].
                 ** Using m=-1 means no detrending; this is only useful
                    for data/information that has been pre-processed
                    (e.g., using the 3dBandpass program).
    
      -bpass L H = Bandpass the data between frequencies L and H (in Hz).
                 ** If the input dataset does not have a time step defined,
                    then TR = 1 s will be assumed for this purpose.
               **** -bpass and -polort are mutually exclusive!
    
      -ort ref   = 1D file with other time series to be removed from -input
                    (via least squares regression) before correlation.
                 ** Each column in 'ref' will be regressed out of
                     each -input voxel time series.
                 ** -ort can be used with -polort and/or -bandpass.
                 ** You can use programs like 3dmaskave and 3dmaskSVD
                     to create reference files from regions of the
                     input dataset (e.g., white matter, CSF).
    
    SPATIAL FILTERING: (only for volumetric input datasets) 
    -----------------
      -Gblur ff  = Gaussian blur the -input dataset (inside the mask)
                    using a kernel width of 'ff' mm.
                ** Uses the same approach as program 3dBlurInMask.
    
      -Mseed rr  = When extracting the seed voxel time series from the
                    (preprocessed) -input dataset, average it over a radius
                    of 'rr' mm prior to doing the correlations with all
                    the voxel time series from the -input dataset.
                ** This extra smoothing is said by some mystics to
                    improve and enhance the results.  YMMV.
                ** Only voxels inside the mask will be used.
                ** A negative value for 'rr' means to treat the voxel
                    dimensions as all equal to 1.0 mm; thus, '-Mseed -1.0'
                    means to average a voxel with its 6 nearest
                    neighbors in the -input dataset 3D grid.
                ** -Mseed and -seed are mutually exclusive!
                   (It makes NO sense to use both options.)
    
    ---------------
    Output Options: (at least one of these must be given!)
    ---------------
      -Mean pp  = Save average correlations into dataset prefix 'pp'
                ** As pointed out to me by CC, '-Mean' is the same
                   as computing the correlation map with the 1D file
                   that is the mean of all the normalized time series
                   in the mask -- that is, a form of the global signal.
                   Such a calculation could be done much faster with
                   program 3dTcorr1D.
                ** Nonlinear combinations of the correlations, as done by
                   the options below, can't be done in such a simple way.
      -Zmean pp = Save tanh of mean arctanh(correlation) into 'pp'
      -Qmean pp = Save RMS(correlation) into 'pp'
      -Pmean pp = Save average of squared positive correlations into 'pp'
                  (negative correlations don't count in this calculation)
      -Thresh tt pp
                = Save the COUNT of how many voxels survived thresholding
                  at level abs(correlation) >= tt (for some tt > 0).
    
      -VarThresh t0 t1 dt pp
                = Save the COUNT of how many voxels survive thresholding
                  at several levels abs(correlation) >= tt, for
                  tt = t0, t0+dt, ..., t1.  This option produces
                  a multi-volume dataset, with prefix 'pp'.
      -VarThreshN t0 t1 dt pp
                = Like '-VarThresh', but the output counts are
                  'Normalized' (divided) by the expected number
                  of such supra-threshold voxels that would occur
                  from white noise timeseries.
               ** N.B.: You can't use '-VarThresh' and '-VarThreshN'
                        in the same run of the program!
      -CorrMap pp
             Output at each voxel the entire correlation map, into
             a dataset with prefix 'pp'.
           **  Essentially this does what 3dAutoTcorrelate would,
               with some of the additional options offered here.
           ** N.B.: Output dataset will be HUGE and BIG in most cases.
      -CorrMask
             By default, -CorrMap outputs a sub-brick for EACH
             input dataset voxel, even those that are NOT in
             the mask (such sub-bricks will be all zero).
             If you want to eliminate these sub-bricks, use
             this option.
           ** N.B.: The label for the sub-brick that was seeded
                    from voxel (i,j,k) will be of the form
                    v032.021.003 (when i=32, j=21, k=3).
    
      --** The following 3 options let you create a customized **--
      --** method of combining the correlations, if the above  **--
      --** techniques do not meet your needs.  (Of course, you **--
      --** could also use '-CorrMap' and then process the big  **--
      --** output dataset yourself later, in some clever way.) **--
    
      -Aexpr expr ppp
                = For each correlation 'r', compute the calc-style
                  expression 'expr', and average these values to get
                  the output that goes into dataset 'ppp'.
      -Cexpr expr ppp
                = As in '-Aexpr', but only average together nonzero
                  values computed by 'expr'.  Example:
                    -Cexpr 'step(r-0.3)*r' TCa03
                  would compute (for each voxel) the average of all
                  correlation coefficients larger than 0.3.
      -Sexpr expr ppp
                = As above, but the sum of the expressions is computed
                  rather than the average.  Example:
                    -Sexpr 'step(r-0.3)' TCn03
                  would compute the number of voxels with correlation
                  coefficients larger than 0.3.
               ** N.B.: At most one '-?expr' option can be used in
                        the same run of the program!
               ** N.B.: Only the symbols 'r' and 'z' [=atanh(r)] have any
                        meaning in the expression; all other symbols will
                        be treated as zeroes.
    
      -Hist N ppp
                = For each voxel, save a histogram of the correlation
                  coefficients into dataset ppp.
               ** N values will be saved per voxel, with the i'th
                  sub-brick containing the count for the range
                    -1+i*D <= r < -1+(i+1)*D  with D=2/N and i=0..N-1
               ** N must be at least 20, and at most 1000.
                * N=200 is good; then D=0.01, yielding a decent resolution.
               ** The output dataset is short format; thus, the maximum
                  count in any bin will be 32767.
               ** The output from this option will probably require further
                  processing before it can be useful -- but it is fun to
                  surf through these histograms in AFNI's graph viewer.
    
    ----------------
    Random Thoughts:
    ----------------
    -- In all output calculations, the correlation of a voxel with itself
       is ignored.  If you don't understand why, step away from the keyboard.
    -- This purely experimental program is somewhat time consuming.
       (Of course, it's doing a LOT of calculations.)
    -- For Kyle, AKA the new Pat (assuming such a thing were possible).
    -- For Steve, AKA the new Kyle (which makes him the newest Pat).
    -- RWCox - August 2008 et cetera.
    
     =========================================================================
    * This binary version of 3dTcorrMap is compiled using OpenMP, a semi-
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
     =========================================================================
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
