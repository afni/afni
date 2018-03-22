**********
3dClustSim
**********

.. _ahelp_3dClustSim:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dClustSim [options]
    
    Program to estimate the probability of false positive (noise-only) clusters.
    An adaptation of Doug Ward's AlphaSim, streamlined for various purposes.
    
    -----------------------------------------------------------------------------
    This program has several different modes of operation, each one involving
    simulating noise-only random volumes, thresholding and clustering them,
    and counting statistics of how often data 'survives' these processes at
    various threshold combinations (per-voxel and cluster-size).
    
    OLDEST method = simulate noise volume assuming the spatial auto-correlation
                    function (ACF) is given by a Gaussian-shaped function, where
                    this shape is specified using the FWHM parameter. The FWHM
                    parameter can be estimated by program 3dFWHMx.
              ** THIS METHOD IS NO LONGER RECOMMENDED **
    
    NEWER method  = simulate noise volume assuming the ACF is given by a mixed-model
                    of the form a*exp(-r*r/(2*b*b))+(1-a)*exp(-r/c), where a,b,c
                    are 3 parameters giving the shape, and can also be estimated
                    by program 3dFWHMx.
              ** THIS METHOD IS ACCEPTABLE **
    
    NEWEST method = program 3dttest++ simulates the noise volumes by randomizing
                    and permuting input datasets, and sending those volumes into
                    3dClustSim directly. There is no built-in math model for the
                    spatial ACF.
              ** THIS METHOD IS MOST ACCURATE AT CONTROLLING FALSE POSITIVE RATE **
              ** You invoke this method with the '-Clustsim' option in 3dttest++ **
    
    3dClustSim computes a cluster-size threshold for a given voxel-wise p-value
    threshold, such that the probability of anything surviving the dual thresholds
    is at some given level (specified by the '-athr' option).
    
    Note that this cluster-size threshold is the same for all brain regions.
    There is an implicit assumption that the noise spatial statistics are
    the same everywhere.
    
    Program 3dXClustSim introduces the idea of spatially variable cluster-size
    thresholds, which may be more useful in some cases. 3dXClustSim's method is
    invoked by using the '-ETAC' option in 3dttest++.
    -----------------------------------------------------------------------------
    
    **** NOTICE ****
    You should use the -acf method, NOT the -fwhm method, when determining
    cluster-size thresholds for FMRI data. The -acf method will give more
    accurate false positive rate (FPR) control.
    ****************
    
    In particular, this program lets you run with multiple p-value thresholds
    (the '-pthr' option) and only outputs the cluster size threshold at chosen
    values of the alpha significance level (the '-athr' option).
    
    In addition, the program allows the output to be formatted for inclusion
    into an AFNI dataset's header, whence it can be used in the AFNI Clusterize
    interface to show approximate alpha values for the displayed clusters, where
    the per-voxel p-value is taken from the interactive threshold slider in the
    AFNI 'Define Overlay' control panel, and then the per-cluster alpha value
    is interpolated in this table from 3dClustSim.  As you change the threshold
    slider, the per-voxel p-value (shown below the slider) changes, and then
    the interpolated alpha values are updated.
    
    ************* IMPORTANT NOTE [Dec 2015] ***************************************
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
    
    ** ---------------------------------------------------------------------------**
    ** IMPORTANT CHANGES -- February 2015 ******************************************
    ** ---------------------------------------------------------------------------**
    ** In the past, 3dClustSim did '1-sided' testing; that is, the random dataset
    ** of Gaussian noise-only values is generated, and then it is thresholded on
    ** the positive side so that the N(0,1) upper tail probability is pthr.
    **
    ** NOW, 3dClustSim does 3 different types of thresholding:
    **   1-sided: as above
    **   2-sided: where positive and negative values above the threshold
    **            are included, and then clustered together
    **            (in this case, the threshold on the Gaussian values is)
    **            (fixed so that the 1-sided tail probability is pthr/2.)
    **  bi-sided: where positive values and negative values above the
    **            threshold are clustered SEPARATELY (with the 2-sided threshold)
    ** For high levels of smoothness, the results from bi-sided and 2-sided are
    ** very similar -- since for smooth data, it is unlikely that large clusters of
    ** positive and negative values will be next to each other. With high smoothness,
    ** it is also true that the 2-sided results for 2*pthr will be similar to the
    ** 1-sided results for pthr, for the same reason. Since 3dClustSim is meant to be
    ** useful when the noise is NOT very smooth, we provide tables for all 3 cases.
    **
    ** In particular, note that when the AFNI GUI threshold is set to a t-statistic,
    ** 2-sided testing is what is usually appropriate -- in that case, the cluster
    ** size thresholds tend to be smaller than the 1-sided case, which means that
    ** more clusters tend to be significant than in the past.
    **
    ** In addition, the 3 different NN approaches (NN=1, NN=2, NN=3) are ALL
    ** always computed now.  That is, 9 different tables are produced, each
    ** of which has its proper place when combined with the AFNI Clusterize GUI.
    ** The 3 different NN methods are:
    **  1 = Use first-nearest neighbor clustering
    **      * above threshold voxels cluster together if faces touch
    **  2 = Use second-nearest neighbor clustering
    **      * voxels cluster together if faces OR edges touch
    **  3 = Use third-nearest neighbor clustering
    **      * voxels cluster together if faces OR edges OR corners touch
    ** The clustering method only makes a difference at higher (less significant)
    ** values of pthr.   At small values of pthr (more significant),  all three
    ** clustering methods will give very similar results.
    **
    **** PLEASE NOTE that the NIML outputs from this new version are not named the
    **** same as those from the older version. Thus, any script that takes the NIML
    **** format tables and inserts them into an AFNI dataset header must be modified
    **** to match the new names. The 3drefit command fragment output at the end of
    **** this program (and echoed into file '3dClustSim.cmd') shows the new form
    **** of the names involved.
    **** -------------------------------------------------------------------------**
    **** SMOOTHING CHANGE -- May 2015 **********************************************
    ** ---------------------------------------------------------------------------**
    ** It was pointed out to me (by Anders Eklund and Tom Nichols) that smoothing
    ** the simulated data over a finite volume introduces 2 artifacts, which might
    ** be called 'edge effects'.  To minimize these problems, this program now makes
    ** extra-large (padded) simulated volumes before blurring, and then trims those
    ** back down to the desired size, before continuing with the thresholding and
    ** cluster-counting steps.  To run 3dClustSim without this padding added, use
    ** the new '-nopad' option.
    **** -------------------------------------------------------------------------**
    
    -------
    OPTIONS  [at least 1 option is required, or you'll get this help message!]
    -------
     ******* Specify the volume over which the simulation will occur *******
    
      -----** (a) Directly give the spatial domain that will be used **-----
    
    -nxyz n1 n2 n3 = Size of 3D grid to use for simulation
                      [default values = 64 64 32]
    -dxyz d1 d2 d3 = give all 3 voxel sizes at once
                      [default values = 3.5 3.5 3.5]
    -BALL          = inside the 3D grid, mask off points outside a ball
                      at the center of the grid and touching the edges;
                      this will keep about 1/2 the points in the 3D grid.
                      [default = use all voxels in the 3D grid]
    
      -----** OR: (b) Specify the spatial domain using a dataset mask **-----
    
    -mask mset     = Use the 0 sub-brick of dataset 'mset' as a mask
                      to indicate which voxels to analyze (a sub-brick
                      selector '[]' is allowed) 
    
    -OKsmallmask   = Allow small masks. Normally, a mask volume must have
                      128 or more nonzero voxels.  However, IF you know what
                      you are doing, and IF you are willing to live life on
                      the edge of statistical catastrophe, then you can use
                      this option to allow smaller masks -- in a sense, this
                      is the 'consent form' for such strange shenanigans.
                     * If you use this option, it must come BEFORE '-mask'.
                     * Also read the 'CAUTION and CAVEAT' section, far below.
                -->>** This option is really only recommended for users who
                       understand what they are doing. Misuse of this option
                       could easily be construed as 'p-hacking'; for example,
                       finding results, but your favorite cluster is too small
                       to survive thresholding, so you post-hoc put a small mask
                       down in that region. DON'T DO THIS!
    
        ** '-mask' means that '-nxyz' & '-dxyz' & '-BALL' will be ignored. **
    
      -----** OR: (c) Specify the spatial domain by directly giving simulated volumes **-----
    
    -inset iset [iset ...] = Read the 'iset' dataset(s) and use THESE volumes
                              as the simulations to threshold and clusterize,
     [Feb 2016]               rather than create the simulations internally.
                             * For example, these datasets could come from
                               3dttest++ -toz -randomsign 1000 -setA ...
                             * This can be combined with '-mask'.
                             * Using '-inset' means that '-fwhm', '-acf', '-nopad',
                               '-niter', and '-ssave' are ignored as meaningless.
    
      ---** the remaining options control how the simulation is done **---
    
    -fwhm s         = Gaussian filter width (all 3 dimensions) in mm (non-negative)
                       [default = 0.0 = no smoothing]
                      * If you wish to set different smoothing amounts for each
                        axis, you can instead use option
                          -fwhmxyz sx sy sz
                        to specify the three values separately.
           **** This option is no longer recommended, since FMRI data    ****
           **** does not have a Gaussian-shaped spatial autocorrelation. ****
           **** Consider using '-acf' or '3dttest++ -Clustsim' instead.  ****
    
    -acf a b c      = Alternative to Gaussian filtering: use the spherical
                      autocorrelation function parameters output by 3dFWHMx
                      to do non-Gaussian (long-tailed) filtering.
                      * Using '-acf' will make '-fwhm' pointless!
                      * The 'a' parameter must be between 0 and 1.
                      * The 'b' and 'c' parameters (scale radii) must be positive.
                      * The spatial autocorrelation function is given by
                          ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)
      >>---------->>*** Combined with 3dFWHMx, the '-acf' method is now a
                        recommended way to generate clustering statistics in AFNI!
                    *** Alternative methods we also recommend:
                        3dttest++ with the -Clustsim and/or -ETAC options.
    
    -nopad          = The program now [12 May 2015] adds 'padding' slices along
                       each face to allow for edge effects of the smoothing process.
                       If you want to turn this feature off, use the '-nopad' option.
                      * For example, if you want to compare the 'old' (un-padded)
                        results with the 'new' (padded) results.
                      * '-nopad' has no effect when '-acf' is used, since that option
                        automatically pads the volume when creating it (via FFTs) and
                        then truncates it back to the desired size for clustering.
    
    -pthr p1 .. pn = list of uncorrected (per voxel) p-values at which to
                      threshold the simulated images prior to clustering.
                      [default = 0.05 0.02 0.01 0.005 0.002 0.001 0.0005 0.0002 0.0001]
    
    -athr a1 .. an = list of corrected (whole volume) alpha-values at which
                      the simulation will print out the cluster size
                      thresholds.  For each 'p' and 'a', the smallest cluster
                      size C(p,a) for which the probability of the 'p'-thresholded
                      image having a noise-only cluster of size C is less than 'a'
                      is the output (cf. the sample output, below)
                      [default = 0.10 0.05 0.02 0.01]
    
             ** It is possible to use only ONE value in each of '-pthr' and     **
             ** '-athr', and then you will get exactly one line of output       **
             ** for each sided-ness and NN case. For example:                   **
             **   -pthr 0.001 -athr 0.05                                        **
    
             ** Both lists '-pthr' and '-athr' (of values between 0 and 0.2)    **
             ** should be given in DESCENDING order.  They will be sorted to be **
             ** that way in any case, and such is how the output will be given. **
    
             ** The list of values following '-pthr' or '-athr' can be replaced **
             ** with the single word 'LOTS', which will tell the program to use **
             ** a longer list of values for these probabilities [try it & see!] **
             ** (i.e., '-pthr LOTS' and/or '-athr LOTS' are legal options)      **
    
    -LOTS          = the same as using '-pthr LOTS -athr LOTS'
    -MEGA          = adds even MORE values to the '-pthr' and '-athr' grids.
    
    -iter n        = number of Monte Carlo simulations [default = 10000]
    
    -nodec         = normally, the program prints the cluster size threshold to
                      1 decimal place (e.g., 27.2).  Of course, clusters only come
                      with an integer number of voxels -- this fractional value
                      is interpolated to give the desired alpha level.  If you
                      want no decimal places (so that 27.2 becomes 28), use '-nodec'.
    
    -seed S        = random number seed [default seed = 123456789]
                      * if seed=0, then program will quasi-randomize it
    
    -niml          = Output the table in an XML/NIML format, rather than a .1D format.
                      * This option is for use with other software programs;
                        see the NOTES section below for details.
                      * '-niml' also implicitly means '-LOTS'.
    
    -both          = Output the table in XML/NIML format AND in .1D format.
                      * You probably want to use '-prefix' with this option!
                        Otherwise, everything is mixed together on stdout.
                      * '-both' implies 'niml' which implies '-LOTS'.
                        So '-pthr' (if desired) should follow '-both'/'-niml'
    
    -prefix ppp    = Write output for NN method #k to file 'ppp.NNk_Xsided.1D',
                      for k=1, 2, 3, and for X=1sided, 2sided, bisided.
                      * If '-prefix is not used, all results go to standard output.
                        You will probably find this confusing.
                      * If '-niml' is used, the filename is 'ppp.NNk_Xsided.niml'.
                        To be clear, the 9 files that will be named
                          ppp.NN1_1sided.niml ppp.NN1_2sided.niml ppp.NN1_bisided.niml
                          ppp.NN2_1sided.niml ppp.NN2_2sided.niml ppp.NN2_bisided.niml
                          ppp.NN3_1sided.niml ppp.NN3_2sided.niml ppp.NN3_bisided.niml
                      * If '-niml' AND '-mask' are both used, then a compressed ASCII
                        encoding of the mask volume is stored into file 'ppp.mask'.
                        This string can be stored into a dataset header as an attribute
                        with name AFNI_CLUSTSIM_MASK, and will be used in the AFNI
                        Clusterize GUI, if present, to mask out above-threshold voxels
                        before the clusterizing is done (which is how the mask is used
                        here in 3dClustSim).
                      * If the ASCII mask string is NOT stored into the statistics dataset
                        header, then the Clusterize GUI will try to find the original
                        mask dataset and use that instead.  If that fails, then masking
                        won't be done in the Clusterize process.
    
     -cmd ccc      = Write command for putting results into a file's header to a file
                     named 'ccc' instead of '3dClustSim.cmd'.  This option is mostly
                     to help with scripting, as in
                       3dClustSim -cmd XXX.cmd -prefix XXX.nii ...
                       `cat XXX.cmd` XXX.nii
    
     -quiet        = Don't print out the progress reports, etc.
                     * Put this option first to silence most informational messages.
    
     -ssave:TYPE ssprefix = Save the un-thresholded generated random volumes into
                            datasets ('-iter' of them). Here, 'TYPE' is one of these:
                              * blurred == save the blurred 3D volume before masking
                              * masked  == save the blurred volume after masking
                            The output datasets will actually get prefixes generated
                            with the string 'ssprefix' being appended by a 6 digit
                            integer (the iteration index), starting at 000000.
                            (You can use SOMETHING.nii as a prefix; it will work OK.)
                            N.B.: This option will slow the program down a lot,
                                  and was intended to help just one specific user.
    
    ------
    NOTES:
    ------
    * This program is like running AlphaSim once for each '-pthr' value and then
      extracting the relevant information from its 'Alpha' output column.
     ++ One reason for 3dClustSim to be used in place of AlphaSim is that it will
        be much faster than running AlphaSim multiple times.
     ++ Another reason is that the resulting table can be stored in an AFNI
        dataset's header, and used in the AFNI Clusterize GUI to see estimated
        cluster significance (alpha) levels.
    
    * To be clear, the C(p,alpha) thresholds that are calculated are for
      alpha = probability of a noise-only smooth random field, after masking
      and then thresholding at the given per-voxel p value, producing a cluster
      of voxels at least this big.
     ++ So if your cluster is larger than the C(p,0.01) threshold in size (say),
        then it is very unlikely that noise BY ITSELF produced this result.
     ++ This statement does not mean that ALL the voxels in the cluster are
        'truly' active -- it means that at least SOME of them are (very probably)
        active.  The statement of low probability (0.01 in this example) of a
        false positive result applies to the cluster as a whole, not to each
        voxel within the cluster.
    
    * To add the cluster simulation C(p,alpha) table to the header of an AFNI
      dataset, something like the following can be done [tcsh syntax]:
         set fx = ( `3dFWHMx -detrend time_series_dataset+orig` )
         3dClustSim -mask mask+orig -acf $fx[5] $fx[6] $fx[7] -niml -prefix CStemp
         3drefit -atrstring AFNI_CLUSTSIM_NN1_1sided file:CStemp.NN1_1sided.niml \
                 -atrstring AFNI_CLUSTSIM_MASK file:CStemp.mask    \
                 statistics_dataset+orig
         rm -f CStemp.*
      AFNI's Clusterize GUI makes use of these attributes, if stored in a
      statistics dataset (e.g., something from 3dDeconvolve, 3dREMLfit, etc.).
    
       ** Nota Bene: afni_proc.py will automatically run 3dClustSim,  and **
      *** put the results  into the statistical results  dataset for you. ***
     **** Another reason to use afni_proc.py for single-subject analyses! ****
    
    * 3dClustSim will print (to stderr) a 3drefit command fragment, similar
      to the one above, that you can use to add cluster tables to any
      relevant statistical datasets you have lolling about.
    
    * The C(p,alpha) table will be used in Clusterize to provide the cluster
      level alpha value when the AFNI GUI is set so that the Overlay threshold
      sub-brick is a statistical parameter (e.g., a t- or F-statistic), from which
      a per-voxel p-value can be calculated, so that Clusterize can interpolate
      in the C(p,alpha) table.
     ++ To be clear, the per-voxel p-value is taken from the AFNI GUI threshold
        slider (the p-value is shown beneath the slider), and then the C(p,alpha)
        table is inverse-interpolated to find the per-cluster alpha value for
        each different cluster size.
     ++ As you move the AFNI threshold slider, the per-voxel (uncorrected for
        multiple comparisons) p-value changes, the cluster sizes change (as fewer
        or more voxels are included), and so the reported per-cluster alpha
        values change for both reasons -- different p and different cluster size.
     ++ The alpha values reported are 'per-cluster', and are not themselves
        corrected for multiple comparisons ACROSS clusters.  These alpha values
        are corrected for multiple comparisons WITHIN a cluster.
    
    * AFNI will use the NN1, NN2, NN3 tables as needed in its Clusterize
      interface if they are all stored in the statistics dataset header,
      depending on the NN level chosen in the Clusterize controller.
    
    * The blur estimates (provided to 3dClustSim via -acf) comes from using
      program 3dFWHMx.
    
    -------------------
    CAUTION and CAVEAT: [January 2011]
    -------------------
    * If you use a small ROI mask and also have a large blur, then it might happen
      that it is impossible to find a cluster size threshold C that works for a
      given (p,alpha) combination.
    
    * Generally speaking, C(p,alpha) gets smaller as p gets smaller and C(p,alpha)
      gets smaller as alpha gets larger.  As a result, in a small mask with small p
      and large alpha, C(p,alpha) might shrink below 1.  But clusters of size C
      less than 1 don't make any sense!
    
    * For example, suppose that for p=0.0005 that only 6% of the simulations
      have ANY above-threshold voxels inside the ROI mask.  In that case,
      C(p=0.0005,alpha=0.06) = 1.  There is no smaller value of C where 10%
      of the simulations have a cluster of size C or larger.  Thus, it is
      impossible to find the cluster size threshold for the combination of
      p=0.0005 and alpha=0.10 in this case.
    
    * 3dClustSim will report a cluster size threshold of C=1 for such cases.
      It will also print (to stderr) a warning message for all the (p,alpha)
      combinations that had this problem.
    
    -----------------------------
    ---- RW Cox -- July 2010 ----
    
    -------------
    SAMPLE OUTPUT from the command '3dClustSim -fwhm 7' [only the NN=1 1-sided results]
    -------------
    # 3dClustSim -fwhm 7
    # 1-sided thresholding
    # Grid: 64x64x32 3.50x3.50x3.50 mm^3 (131072 voxels)
    #
    # CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels
    # -NN 1  | alpha = Prob(Cluster >= given size)
    #  pthr  |  0.100  0.050  0.020  0.010
    # ------ | ------ ------ ------ ------
     0.050000   162.5  182.2  207.8  225.7
     0.020000    64.3   71.0   80.5   88.5
     0.010000    40.3   44.7   50.7   55.1
     0.005000    28.0   31.2   34.9   38.1
     0.002000    19.0   21.2   24.2   26.1
     0.001000    14.6   16.3   18.9   20.5
     0.000500    11.5   13.0   15.1   16.7
     0.000200     8.7   10.0   11.6   12.8
     0.000100     7.1    8.3    9.7   10.9
    
    e.g., for this sample volume, if the per-voxel p-value threshold is set
    at 0.005, then to keep the probability of getting a single noise-only
    cluster at 0.05 or less, the cluster size threshold should be 32 voxels
    (the next integer above 31.2).
    
    If you ran the same simulation with the '-nodec' option, then the last
    line above would be
     0.000100       8      9     10     11
    If you set the per voxel p-value to 0.0001 (1e-4), and want the chance
    of a noise-only false-positive cluster to be 5% or less, then the cluster
    size threshold would be 9 -- that is, you would keep all NN clusters with
    9 or more voxels.
    
    The header lines start with the '#' (commenting) character so that the result
    is a correctly formatted AFNI .1D file -- it can be used in 1dplot, etc.
    
     =========================================================================
    * This binary version of 3dClustSim is compiled using OpenMP, a semi-
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
