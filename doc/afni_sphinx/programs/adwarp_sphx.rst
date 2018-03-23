.. _ahelp_adwarp:

******
adwarp
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: adwarp [options]
    Resamples a 'data parent' dataset to the grid defined by an
    'anat parent' dataset.  The anat parent dataset must contain
    in its .HEAD file the coordinate transformation (warp) needed
    to bring the data parent dataset to the output grid.  This
    program provides a batch implementation of the interactive
    AFNI 'Write' buttons, one dataset at a time.
    
      Example: create dataset func+tlrc (.HEAD and .BRIK) by applying
               the orig->tlrc transformation from the anat.
    
               adwarp -apar anat+tlrc -dpar func+orig
    
      Example: in the case of a manual tlrc transformation, maybe the
               anat+tlrc.BRIK does not exist (just the .HEAD file does).
               In such a case on might apply the anat+tlrc transformation
               to the anat+orig dataset.  But since the anat+tlrc.HEAD
               file already exists, the -overwrite option is needed.
    
               adwarp -apar anat+tlrc -dpar anat+orig -overwrite
    
    Options (so to speak):
    ----------------------
    -apar aset  = Set the anat parent dataset to 'aset'.  This
                    is a nonoptional option (must be present).
    
    -dpar dset  = Set the data parent dataset to 'dset'.  This
                    is a nonoptional option (must be present).
                  Note: dset may contain a sub-brick selector,
                  e.g.,  -dpar 'dset+orig[2,5,7]'             
    
    -prefix ppp = Set the prefix for the output dataset to 'ppp'.
                    The default is the prefix of 'dset'.
    
    -dxyz ddd   = Set the grid spacing in the output datset to
                    'ddd' mm.  The default is 1 mm.
    
    -verbose    = Print out progress reports.
    -force      = Write out result even if it means deleting
                    an existing dataset.  The default is not
                    to overwrite.
    
    -resam rrr  = Set resampling mode to 'rrr' for all sub-bricks
                         --- OR ---                              
    -thr   rrr  = Set resampling mode to 'rrr' for threshold sub-bricks
    -func  rrr  = Set resampling mode to 'rrr' for functional sub-bricks
    
    The resampling mode 'rrr' must be one of the following:
                     NN = Nearest Neighbor
                     Li = Linear Interpolation
                     Cu = Cubic Interpolation
                     Bk = Blocky Interpolation
    
    NOTE:  The default resampling mode is Li for all sub-bricks. 
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
