**********
3dNwarpXYZ
**********

.. _3dNwarpXYZ:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dNwarpXYZ [options] -nwarp 'warp specification' XYZfile.1D > Output.1D
    
    Transforms the DICOM xyz coordinates in the input XYZfile.1D (3 columns)
    based on the '-nwarp' specification -- which is as in 3dNwarpApply
    (e.g., allows inversion, catenation, et cetera).
    
    If this warp is the _WARP output from 3dQwarp, then it takes XYZ values
    from the base dataset and transforms them to the corresponding source
    dataset location.
    
    To do the reverse operation -- to take an XYZ in the source dataset
    and find out where it goes to in the base dataset -- do one of these:
      * use the _WARPINV output from 3dQwarp instead of the _WARP output;
      * use the 'INV(dataset)' form for '-nwarp' (will be slow);
      * use the '-iwarp' option described below.
    The first 2 choices should be equivalent.  The third choice will give
    slightly different results, since the method used for warp inversion
    for just a few discrete points is very different than the full warp
    inversion algorithm -- this difference is for speed.
    
    The mean Euclidean error between '-iwarp' and _WARPINV is about 0.006 mm
    in one test.  The largest error (using 1000 random points) in this test
    was about 0.05 mm.  About 95% of points had 0.015 mm error or less.
    For any 3D brain MRI purpose that Zhark can envision, this level of
    concordance should be adequately good-iful.
    
    ----------------------------------------------------------------
    CLARIFICATION about the confusing forward and inverse warp issue
    ----------------------------------------------------------------
    If the following is the correct command to take a source dataset to
    the place that you want it to go:
    
      3dNwarpApply -nwarp 'SOME_WARP' -source DATASET -prefix JUNK
    
    then the next command is the one to take coordinates in the source
    dataset to the same place
    
      3dNwarpXYZ -nwarp 'SOME_WARP' -iwarp XYZsource.1D > XYZwarped.1D
    
    For example, a command like the above has been used to warp (x,y,z)
    coordinates for ECOG sensors that were picked out manually on a CT volume.
    
    An AFNI nonlinear warp stores the displacements (in DICOM mm) from the
    base dataset grid to the source dataset grid.  For computing the source
    dataset warped to the base dataset grid, these displacements are needed,
    so that for each grid point in the output (warped) dataset, the corresponding
    location in the source dataset can be found.  That is, this 'forward' warp is
    good for finding where a given point in the base dataset maps to in the
    source dataset.
    
    However, for finding where a given point in the source dataset maps to
    in the base dataset, the 'inverse' warp is needed, which is why the
    '-iwarp' option was added to 3dNwarpXYZ.
    
    Zhark knows the above is confusing, and hopes that your distraction by
    this issue will aid him in his ruthless quest for Galactic Domination!
    (And for warm cranberry scones with fresh clotted cream.)
    
    -------------
    OTHER OPTIONS (i.e., besides the mandatory '-nwarp')
    -------------
     -iwarp    = Compute the inverse warp for each input (x,y,z) triple.
                 ++ As mentioned above, this program does NOT compute the
                    inverse warp over the full grid (unlike the 'INV()' method
                    and the '-iwarp' options to other 3dNwarp* programs), but
                    uses a different method that is designed to be fast when
                    applied to a relatively few input points.
                 ++ The upshot is that using '-iwarp' here will give slightly
                    different results than using 'INV()', but for any practical
                    application the differences should be negligible.
    
    July 2014 - Zhark the Coordinated
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
