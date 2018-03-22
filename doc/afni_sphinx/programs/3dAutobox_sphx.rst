*********
3dAutobox
*********

.. _ahelp_3dAutobox:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAutobox [options] DATASET
    Computes size of a box that fits around the volume.
    Also can be used to crop the volume to that box.
    
    OPTIONS:
    --------
    -prefix PREFIX = Crop the input dataset to the size of the box, and
                     write an output dataset with PREFIX for the name.
                   * If -prefix is not used, no new volume is written out,
                     just the (x,y,z) extents of the voxels to be kept.
    
    -input DATASET = An alternate way to specify the input dataset.
                     The default method is to pass DATASET as
                     the last parameter on the command line.
    
    -noclust       = Don't do any clustering to find box. Any non-zero
                     voxel will be preserved in the cropped volume.
                     The default method uses some clustering to find the
                     cropping box, and will clip off small isolated blobs.
    
    -extent: Write to standard out the spatial extent of the box
    -npad NNN      = Number of extra voxels to pad on each side of box,
                     since some troublesome people (that's you, LRF) want
                     this feature for no apparent reason.
                   * With this option, it is possible to get a dataset that
                     is actually bigger than the input.
                   * You can input a negative value for NNN, which will
                     crop the dataset even more than the automatic method.
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
