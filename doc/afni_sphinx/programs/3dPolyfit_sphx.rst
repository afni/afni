*********
3dPolyfit
*********

.. _ahelp_3dPolyfit:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dPolyfit [options] dataset
    Fits a polynomial in space to the dataset and outputs that.
    
    Options:
      -nord n    = Maximum polynomial order (0..9) [default order=3]
      -blur f    = Gaussian blur (inside mask) with FWHM='f' (mm)
      -mrad r    = Radius (voxels) of preliminary median filter
                    [default is no blurring of either type; you can]
                    [do both types (Gaussian and median), but why??]
                    [N.B.: median blur is much slower than Gaussian]
      -prefix pp = Use 'pp' for prefix of output dataset (the fit).
      -resid  rr = Use 'rr' for the prefix of the residual dataset.
      -automask  = Create a mask (a la 3dAutomask)
      -mask mset = Create a mask from nonzero voxels in 'mset'.
      -mone      = Scale the mean value of the fit (inside the mask) to 1.
      -mclip     = Clip values outside the box containing the mask
                   to the edge of the box, to avoid weird artifacts.
      -meth mm   = Set 'mm' to 2 for least squares fit;
                   set it to 1 for L1 fit [default method=2]
                    [Note that L1 fitting is much slower than L2 fitting!]
      -base bb   = In addition to the polynomial fit, also use
                   the volumes in dataset 'bb' as extra basis functions.
                    [If you use a base dataset, then you can set]
                    [nord to -1, to skip using a polynomial fit.]
      -verb      = Print fun and useful progress reports :-)
    
    * Output dataset is always stored in float format.
    * If the input dataset has more than 1 sub-brick, only sub-brick #0
      is processed.
    * If the -base dataset has multiple sub-bricks, all of them are used.
    
    -- Dec 2010 - RWCox - beats workin' for a living
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
