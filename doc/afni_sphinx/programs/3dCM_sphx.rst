****
3dCM
****

.. _3dCM:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dCM [options] dset
    Output = center of mass of dataset, to stdout.
        Note: by default, the output is (x,y,z) values in DICOM
              coordinates.  But as of Dec, 2016, there are now
              command line switches for other options (see -local*
              below).
    
      -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be averaged from 'dataset'.  Note
                     that the mask dataset and the input dataset
                     must have the same number of voxels.
      -automask    Generate the mask automatically.
      -set x y z   After computing the CM of the dataset, set the
                     origin fields in the header so that the CM
                     will be at (x,y,z) in DICOM coords.
      -local_ijk   Output values as (i,j,k) in local orienation.
      -roi_vals v0 v1 v2 ... : Compute center of mass for each blob
                               with voxel value of v0, v1, v2, etc.
                               This option is handy for getting ROI 
                               centers of mass.
      -all_rois     Don't bother listing the values of ROIs you want
                    the program will find all of them and produce a 
                    full list.
      NOTE: Masking options are ignored with -roi_vals and -all_rois
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
