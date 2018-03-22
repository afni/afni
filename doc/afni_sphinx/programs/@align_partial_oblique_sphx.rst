**********************
@align_partial_oblique
**********************

.. _ahelp_@align_partial_oblique:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Parsing ...
    Usage 1: A script to align a full coverage T1 weighted non-oblique dataset
             to match a partial coverage T1 weighted non-oblique dataset 
             Alignment is done with a rotation and shift (6 parameters) transform
             only.
    
     Script is still in testing phase
    
       @align_partial_oblique [options] <-base FullCoverageT1> <-input PartialCoverageObliqueT1>
       Mandatory parameters:
          -base  FullCoverageT1:  Reference anatomical full coverage volume.
    
          -input  PartialCoverageObliqueT1:  The name says it all.
    
       Optional parameters:
          -suffix  SUF   :  Output dataset name is formed by adding SUF to
                            the prefix of the base dataset.
                            The default suffix is _alnd_PartialCoverageObliqueT1
          -keep_tmp      :  Keep temporary files.
          -clean         :  Clean all temp files, likely left from -keep_tmp
                            option then exit.
          -dxyz MM          : Cubic voxel size of output DSET in TLRC
                              space Default MM is 1. If you do not
                              want your output voxels to be cubic
                              Then use the -dx, -dy, -dz options below.
          -dx MX            : Size of voxel in the x direction
                              (Right-Left). Default is 1mm.
          -dy MY            : Size of voxel in the y direction
                              (Anterior-Posterior). Default is 1mm.
          -dz MZ            : Size of voxel in the z direction.
                              (Inferior-Superior).Default is 1mm.
       Example:
       @align_partial_oblique -base ah_SurfVol+orig. -input ah_T1W_anat+orig.
    
    
    Written by Ziad S. Saad, for Ikuko (saadz@mail.nih.gov)
                            SSCC/NIMH/NIH/DHHS
