**********************
@measure_erosion_thick
**********************

.. _ahelp_@measure_erosion_thick:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    @measure_erosion_thick - compute thickness of mask using erosion method
    usage:
    @measure_erosion_thick -maskset maskset -surfset surfacedset.gii -outdir thickdir
    
    where maskset is the dataset to find thickness
     using the largest non-zero value in the mask.
     If dataset has values -2,-1 and 1 for different regions, this script
     calculates the thickness only for voxels with a value of 1
    surfset is a surface to use to find normals into the volume
    output is in directory thickdir. If not specified, erosion_thickdir is used
    
    This script finds thickness by eroding voxels away, using facing voxels
    a layer at a time.
    
    Because of limitations in the growth of the spheres used in this method,
    it is recommended to use oversampled data, particularly when using 1mm data
    See -resample option below
    
    Main options:
      -maskset mydset      mask dataset for input
      -surfset mydset.gii  surface dataset onto which to map thickness
                           (probably a pial/gray matter surface)
      -outdir thickdir     output directory
    
    Other options:
    
      -resample mm   resample input to mm in millimeters (put a number here)
                     set this to half a voxel or \"auto\".
                     No resampling is done by default
                     Resampling is highly recommended for most 1mm data
      -surfsmooth mm smooth surface map of thickness by mm millimeters
                     default is 8 mm
      -smoothmm mm  smooth volume by mm FWHM in mask
                     default is 2*voxelsize of mask or resampled mask
      -maxthick mm   search for maximum thickness value of mm millimeters
                     default is 6 mm
      -depthsearch mm map to surface by looking for max along mm millimeter
                     normal vectors. default is 3 mm
      -keep_temp_files do not delete the intermediate files (for testing)
      -surfsmooth_method heattype heat method used for smoothing surfaces
                     default is HEAT_07 but HEAT_05 is also useful for models
    
    Output:
       erosion_depth.nii.gz - depth dataset
       erosion_thick.nii.gz - volumetric thickness dataset
       erosion_thick_smooth.nii.gz - smoothed volumetric thickness dataset
       erosion_thick.niml.dset - unsmoothed thickness mapped to surface nodes
       erosion_thick_smooth_nn_mm.niml.dset - smoothed thickness mapped to surface nodes
    
       Other datasets included in output:
       maskset.nii.gz, maskset_rs.nii.gz - mask and optional resampled mask
       anat.gii - surface representation of mask volume
       quick.spec - simple specification file for surface to use with suma commands
    
    See related scripts and programs for computing thickness:
