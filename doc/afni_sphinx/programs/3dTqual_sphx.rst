*******
3dTqual
*******

.. _ahelp_3dTqual:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTqual [options] dataset
    Computes a `quality index' for each sub-brick in a 3D+time dataset.
    The output is a 1D time series with the index for each sub-brick.
    The results are written to stdout.
    
    Note that small values of the index are 'good', indicating that
    the sub-brick is not very different from the norm.  The purpose
    of this program is to provide a crude way of screening FMRI
    time series for sporadic abnormal images, such as might be
    caused by large subject head motion or scanner glitches.
    
    Do not take the results of this program too literally.  It
    is intended as a GUIDE to help you find data problems, and no
    more.  It is not an assurance that the dataset is good, and
    it may indicate problems where nothing is wrong.
    
    Sub-bricks with index values much higher than others should be
    examined for problems.  How you determine what 'much higher' means
    is mostly up to you.  I suggest graphical inspection of the indexes
    (cf. EXAMPLE, infra).  As a guide, the program will print (stderr)
    the median quality index and the range median-3.5*MAD .. median+3.5*MAD
    (MAD=Median Absolute Deviation).  Values well outside this range might
    be considered suspect; if the quality index were normally distributed,
    then values outside this range would occur only about 1% of the time.
    
    OPTIONS:
      -spearman = Quality index is 1 minus the Spearman (rank)
                   correlation coefficient of each sub-brick
                   with the median sub-brick.
                   [This is the default method.]
      -quadrant = Similar to -spearman, but using 1 minus the
                   quadrant correlation coefficient as the
                   quality index.
    
      -autoclip = Clip off low-intensity regions in the median sub-brick,
      -automask =  so that the correlation is only computed between
                   high-intensity (presumably brain) voxels.  The
                   intensity level is determined the same way that
                   3dClipLevel works.  This prevents the vast number
                   of nearly 0 voxels outside the brain from biasing
                   the correlation coefficient calculations.
    
      -clip val = Clip off values below 'val' in the median sub-brick.
    
      -mask MSET = Compute correlation only across masked voxels.
    
      -range    = Print the median-3.5*MAD and median+3.5*MAD values
                   out with EACH quality index, so that they
                   can be plotted (cf. Example, infra).
         Notes: * These values are printed to stderr in any case.
                * This is only useful for plotting with 1dplot.
                * The lower value median-3.5*MAD is never allowed
                    to go below 0.
    
    EXAMPLE:
       3dTqual -range -automask fred+orig | 1dplot -one -stdin
    will calculate the time series of quality indexes and plot them
    to an X11 window, along with the median+/-3.5*MAD bands.
    
    NOTE: cf. program 3dToutcount for a somewhat different quality check.
    
    -- RWCox - Aug 2001
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
