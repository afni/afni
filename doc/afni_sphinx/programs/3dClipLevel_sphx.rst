.. _ahelp_3dClipLevel:

***********
3dClipLevel
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dClipLevel [options] dataset
    Estimates the value at which to clip the anatomical dataset so
      that background regions are set to zero.
    
    The program's output is a single number sent to stdout.  This
      value can be 'captured' to a shell variable using the backward
      single quote operator; a trivial csh/tcsh example is
    
        set ccc = `3dClipLevel -mfrac 0.333 Elvis+orig`
        3dcalc -a Elvis+orig -expr "step(a-$ccc)" -prefix Presley
    
    Algorithm:
      (a) Set some initial clip value using wizardry (AKA 'variance').
      (b) Find the median of all positive values >= clip value.
      (c) Set the clip value to 0.50 of this median.
      (d) Loop back to (b) until the clip value doesn't change.
    This method was made up out of nothing, based on histogram gazing.
    
    Options:
    --------
      -mfrac ff = Use the number ff instead of 0.50 in the algorithm.
      -doall    = Apply the algorithm to each sub-brick separately.
                  [Cannot be combined with '-grad'!]
    
      -grad ppp = In addition to using the 'one size fits all routine',
                  also compute a 'gradual' clip level as a function
                  of voxel position, and output that to a dataset with
                  prefix 'ppp'.
                 [This is the same 'gradual' clip level that is now the
                  default in 3dAutomask - as of 24 Oct 2006.
                  You can use this option to see how 3dAutomask clips
                  the dataset as its first step.  The algorithm above is
                  is used in each octant of the dataset, and then these
                  8 values are interpolated to cover the whole volume.]
    Notes:
    ------
    * Use at your own risk!  You might want to use the AFNI Histogram
        plugin to see if the results are reasonable.  This program is
        likely to produce bad results on images gathered with local
        RF coils, or with pulse sequences with unusual contrasts.
    
    * For brain images, most brain voxels seem to be in the range from
        the clip level (mfrac=0.5) to about 3-3.5 times the clip level.
        - In T1-weighted images, voxels above that level are usually
          blood vessels (e.g., inflow artifact brightens them).
    
    * If the input dataset has more than 1 sub-brick, the data is
        analyzed on the median volume -- at each voxel, the median
        of all sub-bricks at that voxel is computed, and then this
        median volume is used in the histogram algorithm.
    
    * If the input dataset is short- or byte-valued, the output will
        be an integer; otherwise, the output is a float value.
    
    * Example -- Scaling a sequence of sub-bricks from a collection of
                 anatomicals from different sites to have about the
                 same numerical range (from 0 to 255):
           3dTcat -prefix input anat_*+tlrc.HEAD
           3dClipLevel -doall input+tlrc > clip.1D
           3dcalc -datum byte -nscale -a input+tlrc -b clip.1D \
                  -expr '255*max(0,min(1,a/(3.2*b)))' -verb -prefix scaled
    ----------------------------------------------------------------------
    * Author: Emperor Zhark -- Sadistic Galactic Domination since 1994!
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
