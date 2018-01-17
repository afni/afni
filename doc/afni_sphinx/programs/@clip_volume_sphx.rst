************
@clip_volume
************

.. _@clip_volume:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage 1: A script to clip regions of a volume
    
       @clip_volume <-input VOL> <-below Zmm> [ [-and/-or] <-above Zmm> ]
    
       Mandatory parameters:
          -input VOL: Volume to clip
        + At least one of the options below:
          -below Zmm: Set to 0 slices below Zmm
                      Zmm (and all other coordinates) are in RAI
                      as displayed by AFNI on the top left corner
                      of the AFNI controller
          -above Zmm: Set to 0 slices above Zmm
          -left  Xmm: Set to 0 slices left of Xmm
          -right  Xmm: Set to 0 slices right of Xmm
          -anterior Ymm: Set to 0 slices anterior to Ymm
          -posterior Ymm: Set to 0 slices posterior to Ymm
        + Or the box option:
          -box Cx Cy Cz Dx Dy Dz: Clip the volume to a box
                                  centered at Cx, Cy, Cz (RAI mm),
                                  and of dimensions Dx Dy Dz (RAI mm)
          -mask_box Cx Cy Cz Dx Dy Dz: Same as -box, but set all values
                                  inside of box to 1.
          Example:
             @clip_volume -mask_box 20.671 -10.016 23.362 10 10 10 \
                          -input seg_no_spat.c+orig.BRIK \
                          -prefix small_box_volume -crop_greedy
    
          Note:
             If you are not cropping the output, you might consider
             using 3dUndump instead.
    
        Optional parameters:
          -and (default): Combine with next clipping planes using 'and'
          -or           : Combine with next clipping planes using 'or'
             Note: These two parameters affect the clipping options that
                   come after after them. Unfortunately they are used
                   to build a mask of what is to be kept in the end, rather
                   than what is to be removed, so they can be confusing.
                   A '-and' multiplies the mask by what is to be kept from
                   the next cut, and a '-or' adds to it.
          -verb         : Verbose, show command
          -crop_allzero : Crop the output volume with 3dAutobox -noclust
                          This would keep 3dAutobox from removing any
                          slices unless they are all zeros
          -crop_greedy  : Crop the output volume with 3dAutobox
                          In addition to what you specified for cropping,
                          slices with a few non zero voxels might also get
                          chopped off by 3dAutobox
          -crop : Same as -crop_greedy, kept for backward compatibility
          -prefix PRFX  : Use PRFX for output prefix. Default is the 
                          input prefix with _clp suffixed to it.
          -followers DSET1 DSET2 ...: Apply the same treatment to the
                          follower datasets. Note that cropped or clipped
                          versions are all named automatically by affixing
                          _clp to their prefix.
    
    Example:
    @clip_volume -below -30 -above 53 -left 20 -right -13 -anterior -15 \
                 -posterior 42 -input ABanat+orig. -verb -prefix sample
    
    Written by Ziad S. Saad (saadz@mail.nih.gov)
                            SSCC/NIMH/NIH/DHHS
