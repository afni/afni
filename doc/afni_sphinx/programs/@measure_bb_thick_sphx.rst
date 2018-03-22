*****************
@measure_bb_thick
*****************

.. _ahelp_@measure_bb_thick:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    @measure_bb_thick - compute thickness of mask using ball and box method
    usage:
    @measure_bb_thick -maskset maskset -surfset surfacedset.gii -outdir thickdir
    
    where maskset is the dataset to find thickness
     using the largest non-zero value in the mask.
     If dataset has values -2,-1 and 1 for different regions, this script
     calculates the thickness only for voxels with a value of 1
    surfset is a surface to use to find normals into the volume
    output is in directory thickdir. If not specified, bb_thickdir is used
    
    This script finds thickness by finding the largest sphere or cube that fits
    within the mask at each voxel. The cube search has little effect on
    surface mapping of thickness, affecting only some edges in the volume.
    If one is primarily interested in the surface mapping, then consider
    the -balls_only to skip the cube search.
    
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
      -increment mm  test thickness at increments of sub-voxel distance
                     default is 1/4 voxel minimum distance (in-plane)
      -surfsmooth mm smooth surface map of thickness by mm millimeters
                     default is 6 mm
      -smoothmm mm  smooth volume by mm FWHM in mask
                     default is 2*voxelsize of mask or resampled mask
      -maxthick mm   search for maximum thickness value of mm millimeters
                     default is 6 mm
      -depthsearch mm map to surface by looking for max along mm millimeter
                     normal vectors. default is 3 mm
      -keep_temp_files do not delete the intermediate files (for testing)
      -balls_only    calculate only with spheres and skip boxes
      -surfsmooth_method heattype heat method used for smoothing surfaces
                     default is HEAT_07 but HEAT_05 is also useful for models
    
    Output:
       maxfill.nii.gz - thickness/depth dataset
       bb_thick.nii.gz - volumetric thickness dataset
       bb_thick_smooth.nii.gz - smoothed volumetric thickness dataset
       bb_thick.niml.dset - unsmoothed thickness mapped to surface nodes
       bb_thick_smooth.niml.dset - smoothed thickness mapped to surface nodes
    
       Other datasets included in output:
       maskset.nii.gz, maskset_rs.nii.gz - mask and optional resampled mask
       anat.gii - surface representation of mask volume
       quick.spec - simple specification file for surface to use with suma commands
    
    See related scripts and programs for computing thickness:
