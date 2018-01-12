.. contents:: 
    :depth: 4 

********
mritopgm
********

.. code-block:: none

    Converts an image to raw pgm format.
    Results go to stdout and should be redirected.
    Usage:   mritopgm [-pp] input_image
    Example: mritopgm fred.001 | ppmtogif > fred.001.gif
    
      The '-pp' option expresses a clipping percentage.
      That is, if this option is given, the pp%-brightest
      pixel is mapped to white; all above it are also white,
      and all below are mapped linearly down to black.
      The default is that pp=100; that is, the brightest
      pixel is white.  A useful operation for many MR images is
        mritopgm -99 fred.001 | ppmtogif > fred.001.gif
      This will clip off the top 1% of voxels, which are often
