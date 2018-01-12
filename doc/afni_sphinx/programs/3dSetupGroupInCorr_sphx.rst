.. contents:: 
    :depth: 4 

******************
3dSetupGroupInCorr
******************

.. code-block:: none

    Usage: 3dSetupGroupInCorr [options] dataset dataset ...
    
    This program is used to pre-process a collection of AFNI
    3D+time datasets for use with Group InstaCorr (3dGroupInCorr).
    
    * By itself, this program just collects all its input datasets
      together for convenient processing later.  Pre-processing
      (e.g., detrending, bandpassing, despiking) must be done BEFORE
      running 3dSetupGroupInCorr -- for example, with 3dBandpass.
      The actual calculations of group t-tests of correlations is
      done AFTER running 3dSetupGroupInCorr, in program 3dGroupInCorr.
    
    * All the datasets input here will be treated as one sample
      for the t-test performed in 3dGroupInCorr.  If you are going
      to do a 2-sample t-test, then you will need to run this
      program twice, once for each collection of datasets
      (e.g., once for 'control subjects' and once for 'patients').
    
    * All datasets must have the same grid layout, since 3dGroupInCorr
      will do voxel-by-voxel comparisons.  Usually, this means that
      the datasets have been transformed to a standard space; for
      example, using the @auto_tlrc script.
    
    * All the datasets use the same mask -- only voxels inside
      this mask will be stored and processed.  If you do not give the
      '-mask' option, then all voxels will be processed -- not usually
      a good idea, since non-brain voxels will use up a LOT of memory
      and CPU time in 3dGroupInCorr.
      ++ If you use '-mask', you MUST use the same mask dataset
         in all runs of 3dSetupGroupInCorr that will be input
         at the same time to 3dGroupInCorr -- otherwise, the
         computations in that program will make no sense AT ALL!
      ++ This requirement is why there is no '-automask' option.
    
    * However, the datasets do NOT all have to have the same number
      of time points or time spacing.  But each dataset must have
      at least 9 points along the time axis!
    
    * The ONLY pre-processing herein for each time series is to L2
      normalize it (sum of squares = 1) and scale it to 8-bit bytes
      (or to 16-bit shorts).
      ++ You almost certainly want to use 3dBandpass and/or some other
         code to pre-process the datasets BEFORE input to this program.
      ++ See the SAMPLE SCRIPT below for a semi-reasonable way to
         pre-process a collection of datasets for 3dGroupInCorr.
      ++ [10 May 2012] The '-prep' option now allows for some limited
         pre-processing operations.
    
    * The outputs from this program are 2 files:
      ++ PREFIX.grpincorr.niml is a text file containing the header
         information that describes the data file.  This file is input
         to 3dGroupInCorr to define one sample in the t-test.
      ++ PREFIX.grpincorr.data is the data file, which contains
         all the time series (in the mask) from all the datasets.
      ++ The data file will usually be huge (gigabytes, perhaps).
         You need to be sure you have enough disk space and RAM.
      ++ If the output files already exist when you run this program,
         then 3dSetupGroupInCorr will exit without processing the datasets!
    
    * See the help for 3dGroupInCorr for information on running that program.
    * The PDF file
      https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/instastuff.pdf
      also has some information on the Group InstaCorr process (as well as all
      the other 'Insta' functions added to AFNI).
    
    * The program 3dExtractGroupInCorr can be used to reconstruct the
      input datasets from the .niml and .data files, if needed.
    
    -------
    OPTIONS
    -------
      -mask mset     = Mask dataset [highly recommended for volumetric e data!]
    
      -prefix PREFIX = Set prefix name of output dataset
    
      -short         = Store data as 16-bit shorts [used to be the default]
                     ++ This will double the amount of disk space and RAM needed.
                     ++ For most GroupInCorr purposes, you don't need this option,
                        since there is so much averaging going on that truncation
                        noise is washed away.
    
      -byte          = Store data as 8-bit bytes rather than 16-bit shorts.
                     ++ This will save memory in 3dGroupInCorr (and disk space),
                        which can be important when using large collections of
                        datasets.  Results will be very slightly less accurate
                        than with '-short', but you'll have a hard time finding
                        any place where this matters.
                     ++ This option is now the default [08 Feb 2010].
                     ++ The amount of data stored is (# of voxels in the mask)
                                                   * (# of time points per subject)
                                                   * (# of subjects)
                        For a 3x3x3 mm^3 grid in MNI space, there are typically
                        about 70,000 voxels in the brain.  If you have an average
                        of 200 time points per scan, then one subject's scan will
                        take up 7e4*2e2 = 14 MB of space; 100 subjects would thus
                        require about 1.4 GB of space.
    
      -labels fff    = File 'fff' should be a list of labels, a unique one for each
                       dataset input.  These labels can be used in 3dGroupInCorr to
                       select a subset of datasets to be processed therein.
                     ++ If you don't use this option, then the list of labels will
                        comprise the list of prefixes from the input datasets.
                     ++ Labels cannot contain a space character, a comma, or a semicolon.
                     ++ When using the -LRpairs option, you should specify only
                        one label for eah pair. 
                        If you don't use the -labels option with -LRpairs the 
                        labels are taken from the 'L' only dataset names, that
                        would be the first name of each LRpair.
    
      -DELETE        = Delete input datasets from disk after
                       processing them one at a time into the
                       output data file -- this very highly
                       destructive option is intended to let
                       you save disk space, if absolutely
                       necessary.  *** BE CAREFUL OUT THERE! ***
                     ++ If you are setting up for 3dGroupInCorr
                        in a script that first uses 3dBandpass
                        to filter the datasets, and then uses this
                        program to finish the setup, then you
                        COULD use '-DELETE' to remove the
                        temporary 3dBandpass outputs as soon
                        as they are no longer needed.
    
      -prep XXX      = Prepare (or preprocess) each data time series in some
                       fashion before L2 normalization and storing, where
                       'XXX' is one of these:
                     ++ SPEARMAN ==> convert data to ranks, so that the
                                     resulting individual subject correlations
                                     in 3dGroupInCorr are Spearman correlations.
                     ++ DEMEAN   ==> remove the mean
    
        Variations for surface-based data:
        ----------------------------------
        If you are working with one surface, no special options are needed.
        However, it is often the case that you want to perform correlations
        on both hemispheres. So in that case, you'll want to provide volume
        pairs (Left Hemi data, Right Hemi data). To help reduce the risk of
        user errors (the only kind we know of), you should also provide the
        domain parents for each of the hemispheres.
           -LRpairs L_SURF R_SURF: This option sets the domains for the left
                                   and right hemisphere surfaces, and 
                                   indicates that the datasets to follow
                                   are arranged in (Left, Right) pairs.
    -------------
    SAMPLE SCRIPT  (tcsh syntax)
    -------------
    * Assume datasets are named in the following scheme (sub01, sub02, ...)
     ++ T1-weighted anatomical  = sub01_anat+orig
     ++ Resting state EPI       = sub01_rest+orig
     ++ Standard space template = ~/abin/MNI_avg152T1+tlrc
    
    #!/bin/tcsh
    
    # MNI-ize each subject's anat, then EPIs (at 2 mm resolution)
    
    cp -f ~/abin/MNI_avg152T1+tlrc.* .
    foreach fred ( sub*_anat+orig.HEAD )
      set sub = `basename $fred _anat+orig.HEAD`
      @auto_tlrc -base MNI_avg152T1+tlrc.HEAD -input $fred
      adwarp -apar ${sub}_anat+tlrc.HEAD -dpar ${sub}_rest+orig.HEAD \
             -resam Cu -dxyz 2.0
      3dAutomask -dilate 1 -prefix ${sub}_amask ${sub}_rest+tlrc.HEAD
    end
    
    # Combine individual EPI automasks into a group mask
    
    3dMean -datum float -prefix ALL_amaskFULL *_amask+tlrc.HEAD
    3dcalc -datum byte -prefix ALL_amask5050 -a ALL_amaskFULL+tlrc -expr 'step(a-0.499)'
    /bin/rm -f *_amask+tlrc.*
    
    # Bandpass and blur each dataset inside the group mask
    #  * Skip first 4 time points.
    #  * If you want to remove the global mean signal, you would use the '-ort'
    #    option for 3dBandpass -- but we recommend that you do NOT do this:
    #    http://dx.doi.org/10.1089/brain.2012.0080
    
    foreach fred ( sub*_rest+tlrc.HEAD )
      set sub = `basename $fred _rest+tlrc.HEAD`
      3dBandpass -mask ALL_amask5050+tlrc -blur 6.0 -band 0.01 0.10 -prefix ${sub}_BP\
                 -input $fred'[4..$]'
    end
    
    # Extract data for 3dGroupInCorr
    
    3dSetupGroupInCorr -mask ALL_amask5050 -prefix ALLshort -short *_BP+tlrc.HEAD
    
    # OR
    
    3dSetupGroupInCorr -mask ALL_amask5050 -prefix ALLbyte -byte *_BP+tlrc.HEAD
    
    /bin/rm -f *_BP+tlrc.*
    
    ### At this point you could run (in 2 separate terminal windows)
    ###   afni -niml MNI_avg152T1+tlrc
    ###   3dGroupInCorr -setA ALLbyte.grpincorr.niml -verb
    ### And away we go ....
    
    ------------------
    CREDITS (or blame)
    ------------------
    * Written by RWCox, 31 December 2009.
    * With a little help from my friends: Alex Martin, Steve Gotts, Ziad Saad.
    * With encouragement from MMK.
    
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
