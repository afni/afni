.. _ahelp_@Install_RSFMRI_Motion_Group_Demo:

*********************************
@Install_RSFMRI_Motion_Group_Demo
*********************************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    -----------------------------------------------------------------
    Installs and sets up an AFNI InstaCorr demo archive, based on 190
    Cambridge subjects from FCON_1000.
    
    The pupose of this is to demonstrate an expected false positive
    group result when comparing groups of big and small movers (based
    on the average motion, per TR), and then a way to account for it
    based on inclusion of the motion as a covariate.
    
    It is also a nice demonstration of resting state analysis and the
    power of doing group comparisons of correlation maps on the fly.
    Two groups of 95 subjects are compared on the fly, as the seed voxel
    changes.
    
    This script will:
      - download and expand the AFNI_Demo_Motion_Groups archive (6.6 GB)
          o included data is unprocessed
      - preprocess 190 subjects
          -> despike, align, blur, regress (censor, bandpass)
      - assign subjects to 2 groups, based on average motion
      - prepare to run 3dGroupInCorr
    
    Once ready, you can run the 3dGroupInCorr script:
      - as you ctrl-shift and drag left mouse button:
          o for each subject, generate correlation map between current
            voxel time series and those across the entire brain
            (for all of 190 subjects!)
          o perform Fisher's Transform to normalize r-values
          o run a 2-sample t-test between 'big' and 'small' movers (95 ea)
          o display the results
    
    After setup, all you need to do is run the demo scripts this way:
       ============================================================
       cd AFNI_Demo_Motion_Groups
       tcsh run.stage.4.run.GIC.txt
       ============================================================
    
    It takes a while to download, unpack, and run the setup scripts.
    -----------------------------------------------------------------
