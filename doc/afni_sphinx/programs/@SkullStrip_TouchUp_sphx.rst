*******************
@SkullStrip_TouchUp
*******************

.. _@SkullStrip_TouchUp:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
       ----------------------------------------------------------------------------
       @SkullStrip_TouchUp - helper program to touch up failed skull stripping
    
          By default, resample to 2mm voxel dimensions to speed up editing.
          Drives afni to the draw data set panel for manual editing.
          Then re-resamples back to the original voxel dimensions.
          You can quit and continue where you left off later.
          Creates a folder PREFIX_SS_touch_up.
    
       -----------------------------------------------------------------------------
       options:
    
          -prefix PREFIX    : output file and folder name
          -brain DSET       : skull stripped data set to touch up
          -head DSET        : whole head anatomical data set
          -mask_out         : output a binary mask in addition to actual data
          -orig_dim         : edit in the original image dimensions
          -help             : show this help
    
       -----------------------------------------------------------------------------
       examples:
    
       @SkullStrip_TouchUp -prefix disco -brain disco_brain+orig -head disco_anat+orig
    
       -----------------------------------------------------------------------------
       Justin Rajendra 07/05/2017
