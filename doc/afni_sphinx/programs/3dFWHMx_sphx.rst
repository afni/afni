*******
3dFWHMx
*******

.. _ahelp_3dFWHMx:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dFWHMx [options] dataset
    
    **** NOTICE ****
     You should use the '-acf' option (which is what afni_proc.py uses now).
     The 'Classic' method giving just a Gaussian FWHM can no longer be
     considered reliable for FMRI statistical analyses!
    ****************
    
     >>>>> 20 July 2017: Results from the 'Classic' method are no longer output!
     >>>>>               If you want to see these values, you must give the
     >>>>>               command line option '-ShowMeClassicFWHM'.
     >>>>>               You no longer need to give the '-acf' option, as it
     >>>>>               is now the default method of calculation (and
     >>>>>               cannot be turned off). Note that if you need the
     >>>>>               FWHM estimate, the '-acf' method gives a value
     >>>>>               for that as its fourth output.
     >>>>> Options and comments that only apply to the 'Classic' FWHM estimation
     >>>>> method are now marked below with this '>>>>>' marker, to indicate that
     >>>>> they are obsolete, archaic, and endangered (as well as fattening).
    
    >>>>> Unlike the older 3dFWHM, this program computes FWHMs for all sub-bricks
    >>>>> in the input dataset, each one separately.  The output for each one is
    >>>>> written to the file specified by '-out'.  The mean (arithmetic or geometric)
    >>>>> of all the FWHMs along each axis is written to stdout.  (A non-positive
    >>>>> output value indicates something bad happened; e.g., FWHM in z is meaningless
    >>>>> for a 2D dataset; the estimation method computed incoherent intermediate results.)
    
    (Classic) METHOD: <<<<< NO LONGER OUTPUT -- SEE ABOVE >>>>>
     - Calculate ratio of variance of first differences to data variance.
     - Should be the same as 3dFWHM for a 1-brick dataset.
       (But the output format is simpler to use in a script.)
    
    **----------------------------------------------------------------------------**
    ************* IMPORTANT NOTE [Dec 2015] ****************************************
    **----------------------------------------------------------------------------**
    A completely new method for estimating and using noise smoothness values is
    now available in 3dFWHMx and 3dClustSim. This method is implemented in the
    '-acf' options to both programs.  'ACF' stands for (spatial) AutoCorrelation
    Function, and it is estimated by calculating moments of differences out to
    a larger radius than before.
    
    Notably, real FMRI data does not actually have a Gaussian-shaped ACF, so the
    estimated ACF is then fit (in 3dFWHMx) to a mixed model (Gaussian plus
    mono-exponential) of the form
      ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)
    where 'r' is the radius, and 'a', 'b', 'c' are the fitted parameters.
    The apparent FWHM from this model is usually somewhat larger in real data
    than the FWHM estimated from just the nearest-neighbor differences used
    in the 'classic' analysis.
    
    The longer tails provided by the mono-exponential are also significant.
    3dClustSim has also been modified to use the ACF model given above to generate
    noise random fields.
    
    **----------------------------------------------------------------------------**
    ** The take-away (TL;DR or summary) message is that the 'classic' 3dFWHMx and **
    ** 3dClustSim analysis, using a pure Gaussian ACF, is not very correct for    **
    ** FMRI data -- I cannot speak for PET or MEG data.                           **
    **----------------------------------------------------------------------------**
    
    OPTIONS:
      -mask mmm   = Use only voxels that are nonzero in dataset 'mmm'.
      -automask   = Compute a mask from THIS dataset, a la 3dAutomask.
                    [Default = use all voxels]
    
      -input ddd }=
        *OR*     }= Use dataset 'ddd' as the input.
      -dset  ddd }=
    
      -demed      = If the input dataset has more than one sub-brick
                    (e.g., has a time axis), then subtract the median
                    of each voxel's time series before processing FWHM.
                    This will tend to remove intrinsic spatial structure
                    and leave behind the noise.
                    [Default = don't do this]
      -unif       = If the input dataset has more than one sub-brick,
                    then normalize each voxel's time series to have
                    the same MAD before processing FWHM.  Implies -demed.
                    [Default = don't do this]
      -detrend [q]= Instead of demed (0th order detrending), detrend to
                    order 'q'.  If q is not given, the program picks q=NT/30.
                    -detrend disables -demed, and includes -unif.
            **N.B.: I recommend this option IF you are running 3dFWHMx on
                    functional MRI time series that have NOT been processed
                    to remove any activation and/or physiological artifacts.
               **** If you are running 3dFWHMx on the residual (errts) time
                    series from afni_proc.py, you don't need -detrend.
            **N.B.: This is the same detrending as done in 3dDespike;
                    using 2*q+3 basis functions for q > 0.
            ******* If you don't use '-detrend', the program checks
                    if a large number of voxels are have significant
                    nonzero means. If so, the program will print a warning
                    message suggesting the use of '-detrend', since inherent
                    spatial structure in the image will bias the estimation
                    of the FWHM of the image time series NOISE (which is usually
                    the point of using 3dFWHMx).
      -detprefix d= Save the detrended file into a dataset with prefix 'd'.
                    Used mostly to figure out what the hell is going on,
                    when strange results transpire.
    
    >>>>>
      -geom      }= If the input dataset has more than one sub-brick,
        *OR*     }= compute the final estimate as the geometric mean
      -arith     }= or the arithmetic mean of the individual sub-brick
                    FWHM estimates. [Default = -geom, for no good reason]
    
    >>>>>
      -combine    = combine the final measurements along each axis into
                    one result
    
    >>>>>
      -out ttt    = Write output to file 'ttt' (3 columns of numbers).
                    If not given, the sub-brick outputs are not written.
                    Use '-out -' to write to stdout, if desired.
    
    >>>>>
      -compat     = Be compatible with the older 3dFWHM, where if a
                    voxel is in the mask, then its neighbors are used
                    for differencing, even if they are not themselves in
                    the mask.  This was an error; now, neighbors must also
                    be in the mask to be used in the differencing.
                    Use '-compat' to use the older method.
                 ** NOT RECOMMENDED except for comparison purposes! **
    
      -ACF [anam] = ** new option Nov 2015 **
       *or*         The '-ACF' option computes the spatial autocorrelation
      -acf [anam]   of the data as a function of radius, then fits that
                    to a model of the form
                      ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)
                    and outputs the 3 model parameters (a,b,c) to stdout.
                  * The model fit assumes spherical symmetry in the ACF.
                  * The results shown on stdout are in the format
              >>>>> The first 2 lines below will only be output <<<<<
              >>>>> if you use the option '-ShowMeClassicFWHM'. <<<<<
              >>>>> Otherwise, the 'old-style' FWHM values will <<<<<
              >>>>> show up as all zeros (0 0 0 0).             <<<<<
      # old-style FWHM parameters
       10.4069  10.3441  9.87341     10.2053
      # ACF model parameters for a*exp(-r*r/(2*b*b))+(1-a)*exp(-r/c) plus effective FWHM
       0.578615  6.37267  14.402     16.1453
                    The lines that start with '#' are comments.
              >>>>> The first numeric line contains the 'old style' FWHM estimates,
              >>>>>   FWHM_x FWHM_y FHWM_z  FWHM_combined
                    The second numeric line contains the a,b,c parameters, plus the
                    combined estimated FWHM from those parameters.  In this example,
                    the fit was about 58% Gaussian shape, 42% exponential shape,
                    and the effective FWHM from this fit was 16.14mm, versus 10.21mm
                    estimated in the 'old way'.
                  * If you use '-acf' instead of '-ACF', then the comment #lines
                    in the stdout information will be omitted.  This might help
                    in parsing the output inside a script.
                  * The empirical ACF results are also written to the file
                    'anam' in 4 columns:
                       radius ACF(r) model(r) gaussian_NEWmodel(r)(r)
                    where 'gaussian_NEWmodel' is the Gaussian with the FWHM estimated
                    from the ACF, NOT via the 'classic' (Forman 1995) method.
                  * If 'anam' is not given (that is, another option starting
                    with '-' immediately follows '-acf'), then '3dFWHMx.1D' will
                    be used for this filename. If 'anam' is set to 'NULL', then
                    the corresponding output files will not be saved.
                  * By default, the ACF is computed out to a radius based on
                    a multiple of the 'classic' FWHM estimate.  If you want to
                    specify that radius (in mm), you can put that value after
                    the 'anam' parameter, as in '-acf something.1D 40.0'.
                  * In addition, a graph of these functions will be saved
                    into file 'anam'.png, for your pleasure and elucidation.
                  * Note that the ACF calculations are slower than the
                    'classic' FWHM calculations.
                    To reduce this sloth, 3dFWHMx now uses OpenMP to speed things up.
                  * The ACF modeling is intended to enhance 3dClustSim, and
                    may or may not be useful for any other purpose!
    
    >>>>> SAMPLE USAGE: (tcsh)
    >>>>>   set zork = ( `3dFWHMx -automask -input junque+orig` )
    >>>>> Captures the FWHM-x, FWHM-y, FWHM-z values into shell variable 'zork'.
    
    INPUT FILE RECOMMENDATIONS:
    * For FMRI statistical purposes, you DO NOT want the FWHM or ACF to reflect
      any spatial structure of the underlying anatomy.  Rather, you want
      the FWHM/ACF to reflect the spatial structure of the NOISE.  This means
      that the input dataset should not have anatomical (spatial) structure.
    * One good form of input is the output of '3dDeconvolve -errts', which is
      the dataset of residuals left over after the GLM fitted signal model is
      subtracted out from each voxel's time series.
    * If you don't want to go to that much trouble, use '-detrend' to approximately
      subtract out the anatomical spatial structure, OR use the output of 3dDetrend
      for the same purpose.
    * If you do not use '-detrend', the program attempts to find non-zero spatial
      structure in the input, and will print a warning message if it is detected.
    
     *** Do NOT use 3dFWHMx on the statistical results (e.g., '-bucket') from ***
     *** 3dDeconvolve or 3dREMLfit!!!  The function of 3dFWHMx is to estimate ***
     *** the smoothness of the time series NOISE, not of the statistics. This ***
     *** proscription is especially true if you plan to use 3dClustSim next!! ***
    
     *** -------------------                                                  ***
     *** NOTE FOR SPM USERS:                                                  ***
     *** -------------------                                                  ***
     *** If you are using SPM for your analyses, and wish to use 3dFHWMX plus ***
     *** 3dClustSim for cluster-level thresholds, you need to understand the  ***
     *** process that AFNI uses. Otherwise, you will likely make some simple  ***
     *** mistake (such as using 3dFWHMx on the statistical maps from SPM)     ***
     *** that will render your cluster-level thresholding completely wrong!   ***
    
    >>>>>
    IF YOUR DATA HAS SMOOTH-ISH SPATIAL STRUCTURE YOU CAN'T GET RID OF:
    For example, you only have 1 volume, say from PET imaging.  In this case,
    the standard estimate of the noise smoothness will be mixed in with the
    structure of the background.  An approximate way to avoid this problem
    is provided with the semi-secret '-2difMAD' option, which uses a combination of
    first-neighbor and second-neighbor differences to estimate the smoothness,
    rather than just first-neighbor differences, and uses the MAD of the differences
    rather than the standard deviation.  (If you must know the details, read the
    source code in mri_fwhm.c!)                    [For Jatin Vaidya, March 2010]
    
    ALSO SEE:
    * The older program 3dFWHM is now completely superseded by 3dFWHMx.
    * The program 3dClustSim takes as input the ACF estimates and then
      estimates the cluster sizes thresholds to help you get 'corrected'
      (for multiple comparisons) p-values.
    >>>>>
    * 3dLocalstat -stat FWHM will estimate the FWHM values at each voxel,
      using the same first-difference algorithm as this program, but applied
      only to a local neighborhood of each voxel in turn.
    * 3dLocalACF will estimate the 3 ACF parameters in a local neighborhood
      around each voxel.
    >>>>>
    * 3dBlurToFWHM will iteratively blur a dataset (inside a mask) to have
      a given global FWHM. This program may or may not be useful :)
    * 3dBlurInMask will blur a dataset inside a mask, but doesn't measure FWHM or ACF.
    
    -- Zhark, Ruler of the (Galactic) Cluster!
    
     =========================================================================
    * This binary version of 3dFWHMx is compiled using OpenMP, a semi-
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
