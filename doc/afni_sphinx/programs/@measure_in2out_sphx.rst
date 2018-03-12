***************
@measure_in2out
***************

.. _@measure_in2out:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    here
    @measure_in2out - compute thickness of mask using in2out method
    usage:
    @measure_in2out -maskset maskset -surfset surfacedset.gii -outdir thickdir
    
    where maskset is the dataset to find thickness
     with value of 1 for the mask value to find the thickness
     values of -1 and -2 for the inner and outer boundary values
     (inside and outside masks are treated equivalently)
    surfset is a surface to use to find normals into the volume
    output is in directory thickdir. If not specified, in2out_thickdir is used
    
    This script finds thickness by finding the shortest distance to "inside"
    and "outside" voxels for every voxel in a mask. The distance to the "inside"
    and the distance to the "outside" are added together to be "thickness".
    For example, cortical/gray matter thickness can be found using a mask dataset
    with white matter defined as an inside value and all other voxels
    assigned to be outside voxels.
    
    Because of limitations in the growth of the spheres used in this method,
    it is recommended to use oversampled data, particularly when using 1mm data
    See -resample option below
    
    The maskset must contain three distinct non-zero values
     the highest value is assumed the mask value, the lowest value is
     the outside value, and the inside value is that value+1.
     One example use might be "GM=1,WM=-1,Outside=-2"
    
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
      -maxthick mm   search for maximum thickness value of mm millimeters
                     default is 6 mm
      -depthsearch mm map to surface by looking for max along mm millimeter
                     normal vectors. default is 3 mm
      -maskinoutvals v1 v2 v3  use v1 for value of mask, v2 and v3 for inside
                     and outside mask values, e.g. "1 -2 -1"
      -keep_temp_files do not delete the intermediate files (for testing)
      -surfsmooth_method heattype heat method used for smoothing surfaces
                     default is HEAT_07 but HEAT_05 is also useful for some models
      -fs_cort_dir dirname use FreeSurfer SUMA directory from @SUMA_Make_Spec_FS
                     for processing
    
    Output:
       inout_dist.nii.gz - volumetric thickness/distance from in to out
       in_and_out.nii.gz - volumetric distance to inside and outside in 2 volumes
       inout_thick.niml.dset - unsmoothed thickness mapped to surface nodes
       inout_thick_smooth.niml.dset - smoothed thickness mapped to surface nodes
    
       Other datasets included in output:
       maskset.nii.gz, maskset_rs.nii.gz - mask and optional resampled mask
       anat.gii - surface representation of mask volume
       quick.spec - simple specification file for surface to use with suma commands
    
    See related scripts and programs for computing thickness:
