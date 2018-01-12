.. contents:: 
    :depth: 4 

************
3dBlurToFWHM
************

.. code-block:: none

    Usage: 3dBlurToFWHM [options]
    
    Blurs a 'master' dataset until it reaches a specified FWHM
    smoothness (approximately).  The same blurring schedule is
    applied to the input dataset to produce the output.  The goal
    is to make the output dataset have the given smoothness, no
    matter what smoothness it had on input (however, the program
    cannot 'unsmooth' a dataset!).  See below for the METHOD used.
    
    OPTIONS
    -------
     -input      ddd = This required 'option' specifies the dataset
                       that will be smoothed and output.
     -blurmaster bbb = This option specifies the dataset whose
                       whose smoothness controls the process.
                      **N.B.: If not given, the input dataset is used.
                      **N.B.: This should be one continuous run.
                              Do not input catenated runs!
     -prefix     ppp = Prefix for output dataset will be 'ppp'.
                      **N.B.: Output dataset is always in float format.
     -mask       mmm = Mask dataset, if desired.  Blurring will
                       occur only within the mask.  Voxels NOT in
                       the mask will be set to zero in the output.
     -automask       = Create an automask from the input dataset.
                      **N.B.: Not useful if the input dataset has been
                              detrended or otherwise regressed before input!
     -FWHM       f   = Blur until the 3D FWHM is 'f'.
     -FWHMxy     f   = Blur until the 2D (x,y)-plane FWHM is 'f'.
                       No blurring is done along the z-axis.
                      **N.B.: Note that you can't REDUCE the smoothness
                              of a dataset.
                      **N.B.: Here, 'x', 'y', and 'z' refer to the
                              grid/slice order as stored in the dataset,
                              not DICOM ordered coordinates!
                      **N.B.: With -FWHMxy, smoothing is done only in the
                              dataset xy-plane.  With -FWHM, smoothing
                              is done in 3D.
                      **N.B.: The actual goal is reached when
                                -FWHM  :  cbrt(FWHMx*FWHMy*FWHMz) >= f
                                -FWHMxy:  sqrt(FWHMx*FWHMy)       >= f
                              That is, when the area or volume of a
                              'resolution element' goes past a threshold.
     -quiet            Shut up the verbose progress reports.
                      **N.B.: This should be the first option, to stifle
                              any verbosity from the option processing code.
    
    FILE RECOMMENDATIONS for -blurmaster:
    For FMRI statistical purposes, you DO NOT want the FWHM to reflect
      the spatial structure of the underlying anatomy.  Rather, you want
      the FWHM to reflect the spatial structure of the noise.  This means
      that the -blurmaster dataset should not have anatomical structure.  One
      good form of input is the output of '3dDeconvolve -errts', which is
      the residuals left over after the GLM fitted signal model is subtracted
      out from each voxel's time series.  You can also use the output of
      '3dREMLfit -Rerrts' or '3dREMLfit -Rwherr' for this purpose.
    You CAN give a multi-brick EPI dataset as the -blurmaster dataset; the
      dataset will be detrended in time (like the -detrend option in 3dFWHMx)
      which will tend to remove the spatial structure.  This makes it
      practicable to make the input and blurmaster datasets be the same,
      without having to create a detrended or residual dataset beforehand.
      Considering the accuracy of blurring estimates, this is probably good
      enough for government work [that is an insider's joke :-]. 
      N.B.: Do not use catenated runs as blurmasters. There should
      be no discontinuities in the time axis of blurmaster, which would
      make the simple regression detrending do peculiar things.
    
    ALSO SEE:
     * 3dFWHMx, which estimates smoothness globally
     * 3dLocalstat -stat FWHM, which estimates smoothness locally
     * This paper, which discusses the need for a fixed level of smoothness
       when combining FMRI datasets from different scanner platforms:
         Friedman L, Glover GH, Krenz D, Magnotta V; The FIRST BIRN. 
         Reducing inter-scanner variability of activation in a multicenter
         fMRI study: role of smoothness equalization.
         Neuroimage. 2006 Oct 1;32(4):1656-68.
    
    METHOD:
    The blurring is done by a conservative finite difference approximation
    to the diffusion equation:
      du/dt = d/dx[ D_x(x,y,z) du/dx ] + d/dy[ D_y(x,y,z) du/dy ]
                                       + d/dz[ D_z(x,y,z) du/dz ]
            = div[ D(x,y,z) grad[u(x,y,z)] ]
    where diffusion tensor D() is diagonal, Euler time-stepping is used, and
    with Neumann (reflecting) boundary conditions at the edges of the mask
    (which ensures that voxel data inside and outside the mask don't mix).
    * At each pseudo-time step, the FWHM is estimated globally (like '3dFWHMx')
      and locally (like '3dLocalstat -stat FWHM'). Voxels where the local FWHM
      goes past the goal will not be smoothed any more (D gets set to zero).
    * When the global smoothness estimate gets close to the goal, the blurring
      rate (pseudo-time step) will be reduced, to avoid over-smoothing.
    * When an individual direction's smoothness (e.g., FWHMz) goes past the goal,
      all smoothing in that direction stops, but the other directions continue
      to be smoothed until the overall resolution element goal is achieved.
    * When the global FWHM estimate reaches the goal, the program is done.
      It will also stop if progress stalls for some reason, or if the maximum
      iteration count is reached (infinite loops being unpopular).
    * The output dataset will NOT have exactly the smoothness you ask for, but
      it will be close (fondly we do hope).  In our Imperial experiments, the
      results (measured via 3dFWHMx) are within 10% of the goal (usually better).
    * 2D blurring via -FWHMxy may increase the smoothness in the z-direction
      reported by 3dFWHMx, even though there is no inter-slice processing.
      At this moment, I'm not sure why.  It may be an estimation artifact due
      to increased correlation in the xy-plane that biases the variance estimates
      used to calculate FWHMz.
    
    ADVANCED OPTIONS:
     -maxite  ccc = Set maximum number of iterations to 'ccc' [Default=variable].
     -rate    rrr = The value of 'rrr' should be a number between
                    0.05 and 3.5, inclusive.  It is a factor to change
                    the overall blurring rate (slower for rrr < 1) and thus
                    require more or less blurring steps.  This option should only
                    be needed to slow down the program if the it over-smooths
                    significantly (e.g., it overshoots the desired FWHM in
                    Iteration #1 or #2).  You can increase the speed by using
                    rrr > 1, but be careful and examine the output.
     -nbhd    nnn = As in 3dLocalstat, specifies the neighborhood
                    used to compute local smoothness.
                    [Default = 'SPHERE(-4)' in 3D, 'SPHERE(-6)' in 2D]
                   ** N.B.: For the 2D -FWHMxy, a 'SPHERE()' nbhd
                            is really a circle in the xy-plane.
                   ** N.B.: If you do NOT want to estimate local
                            smoothness, use '-nbhd NULL'.
     -ACF or -acf = Use the 'ACF' method (from 3dFWHMx) to estimate
                    the global smoothness, rather than the 'classic'
                    Forman 1995 method. This option will be somewhat
                    slower.  It will also set '-nbhd NULL', since there
                    is no local ACF estimation method implemented.
     -bsave   bbb = Save the local smoothness estimates at each iteration
                    with dataset prefix 'bbb' [for debugging purposes].
     -bmall       = Use all blurmaster sub-bricks.
                    [Default: a subset will be chosen, for speed]
     -unif        = Uniformize the voxel-wise MAD in the blurmaster AND
                    input datasets prior to blurring.  Will be restored
                    in the output dataset.
     -detrend     = Detrend blurmaster dataset to order NT/30 before starting.
     -nodetrend   = Turn off detrending of blurmaster.
                   ** N.B.: '-detrend' is the new default [05 Jun 2007]!
     -detin       = Also detrend input before blurring it, then retrend
                    it afterwards. [Off by default]
     -temper      = Try harder to make the smoothness spatially uniform.
    
    -- Author: The Dreaded Emperor Zhark - Nov 2006
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
