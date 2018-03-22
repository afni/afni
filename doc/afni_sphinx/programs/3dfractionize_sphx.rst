*************
3dfractionize
*************

.. _ahelp_3dfractionize:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dfractionize [options]
    
    * For each voxel in the output dataset, computes the fraction
        of it that is occupied by nonzero voxels from the input.
    * The fraction is stored as a short in the range 0..10000,
        indicating fractions running from 0..1.
    * The template dataset is used only to define the output grid;
        its brick(s) will not be read into memory.  (The same is
        true of the warp dataset, if it is used.)
    * The actual values stored in the input dataset are irrelevant,
        except in that they are zero or nonzero (UNLESS the -preserve
        option is used).
    
    The purpose of this program is to allow the resampling of a mask
    dataset (the input) from a fine grid to a coarse grid (defined by
    the template).  When you are using the output, you will probably
    want to threshold the mask so that voxels with a tiny occupancy
    fraction aren't used.  This can be done in 3dmaskave, by using
    3calc, or with the '-clip' option below.
    
    Options are [the first 2 are 'mandatory options']:
      -template tset  = Use dataset 'tset' as a template for the output.
                          The output dataset will be on the same grid as
                          this dataset.
    
      -input iset     = Use dataset 'iset' for the input.
                          Only the sub-brick #0 of the input is used.
                          You can use the sub-brick selection technique
                          described in '3dcalc -help' to choose the
                          desired sub-brick from a multi-brick dataset.
    
      -prefix ppp     = Use 'ppp' for the prefix of the output.
                          [default prefix = 'fractionize']
    
      -clip fff       = Clip off voxels that are less than 'fff' occupied.
                          'fff' can be a number between 0.0 and 1.0, meaning
                          the fraction occupied, can be a number between 1.0
                          and 100.0, meaning the percent occupied, or can be
                          a number between 100.0 and 10000.0, meaning the
                          direct output value to use as a clip level.
                       ** Some sort of clipping is desirable; otherwise,
                            an output voxel that is barely overlapped by a
                            single nonzero input voxel will enter the mask.
                          [default clip = 0.0]
    
      -warp wset      = If this option is used, 'wset' is a dataset that
                          provides a transformation (warp) from +orig
                          coordinates to the coordinates of 'iset'.
                          In this case, the output dataset will be in
                          +orig coordinates rather than the coordinates
                          of 'iset'.  With this option:
                       ** 'tset' must be in +orig coordinates
                       ** 'iset' must be in +acpc or +tlrc coordinates
                       ** 'wset' must be in the same coordinates as 'iset'
    
      -preserve       = When this option is used, the program will copy
         or               the nonzero values of input voxels to the output
      -vote               dataset, rather than create a fractional mask.
                          Since each output voxel might be overlapped
                          by more than one input voxel, the program 'votes'
                          for which input value to preserve.  For example,
                          if input voxels with value=1 occupy 10% of an
                          output voxel, and inputs with value=2 occupy 20%
                          of the same voxel, then the output value in that
                          voxel will be set to 2 (provided that 20% is >=
                          to the clip fraction).
                       ** Voting can only be done on short-valued datasets,
                            or on byte-valued datasets.
                       ** Voting is a relatively time-consuming option,
                            since a separate loop is made through the
                            input dataset for each distinct value found.
                       ** Combining this with the -warp option does NOT
                            make a general +tlrc to +orig transformer!
                            This is because for any value to survive the
                            vote, its fraction in the output voxel must be
                            >= clip fraction, regardless of other values
                            present in the output voxel.
    
    Sample usage:
    
      1. Compute the fraction of each voxel occupied by the warped input.
    
              3dfractionize -template grid+orig -input data+tlrc  \
                            -warp anat+tlrc -clip 0.2
    
      2. Apply the (inverse) -warp transformation to transform the -input
         from +tlrc space to +orig space, storing it according to the grid
         of the -template.
         A voxel in the output dataset gets the value that occupies most of
         its volume, providing that value occupies 20% of the voxel.
    
         Note that the essential difference from above is '-preserve'.
    
              3dfractionize -template grid+orig -input data+tlrc  \
                            -warp anat+tlrc -preserve -clip 0.2   \
                            -prefix new_data
    
         Note that 3dAllineate can also be used to warp from +tlrc to +orig
         space.  In this case, data is computed through interpolation, rather
         than voting based on the fraction of a voxel occupied by each data
         value.  The transformation comes from the WARP_DATA attribute directly.
         Nearest neighbor interpolation is used in this 'mask' example.
    
             cat_matvec -ONELINE anat+tlrc::WARP_DATA > tlrc.aff12.1D
             3dAllineate -1Dmatrix_apply tlrc.aff12.1D -source group_mask+tlrc \
                         -master subj_epi+orig -prefix subj_mask -final NN
    
    This program will also work in going from a coarse grid to a fine grid,
    but it isn't clear that this capability has any purpose.
    -- RWCox - February 1999
             - October 1999: added -warp and -preserve options
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
