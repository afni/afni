***********
3dmask_tool
***********

.. _3dmask_tool:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
    3dmask_tool         - for combining/dilating/eroding/filling masks
    
    This program can be used to:
       1. combine masks, with a specified overlap fraction
       2. dilate and/or erode a mask or combination of masks
       3. fill holes in masks
    
    The outline of operations is as follows.
    
       - read all input volumes
          - optionally dilate/erode inputs (with any needed zero-padding)
       - restrict voxels to the fraction of overlap
       - optionally dilate/erode combination (with zero-padding)
       - optionally fill any holes
       - write result
    
    Note : a hole is defined as a fully connected set of zero voxels that
           does not contain an edge voxel.  For any voxel in such a set, it
           is not possible to find a path of voxels to reach an edge.
    
           Such paths are evaluated using 6 face neighbors, no diagonals.
    
    ----------------------------------------
    examples:
    
       a. dilate a mask by 5 levels
    
          3dmask_tool -input mask_anat.FT+tlrc -prefix ma.dilate \
                      -dilate_input 5
    
       b. dilate and then erode, which connects areas that are close
    
          3dmask_tool -input mask_anat.FT+tlrc -prefix ma.close.edges \
                      -dilate_input 5 -5
    
       b2. dilate and erode after combining many masks
    
          3dmask_tool -input mask_anat.*+tlrc.HEAD -prefix ma.close.result \
                      -dilate_result 5 -5
    
       c1. compute an intersection mask, this time with EPI masks
    
          3dmask_tool -input full_mask.*+tlrc.HEAD -prefix mask_inter \
                      -frac 1.0
    
       c2. compute a mask of 70% overlap
    
          3dmask_tool -input full_mask.*+tlrc.HEAD -prefix mask_overlap.7 \
                      -frac 0.7
       c3. simply count the voxels that overlap
    
          3dmask_tool -input full_mask.*+tlrc.HEAD -prefix mask.counts \
                      -count
    
       d. fill holes
    
          3dmask_tool -input mask_anat.FT+tlrc -prefix ma.filled \
                      -fill_holes
    
       e. fill holes per slice
    
          3dmask_tool -input mask_anat.FT+tlrc -prefix ma.filled.xy \
                      -fill_holes -fill_dirs xy
    
       f. read many masks, dilate and erode, restrict to 70%, and fill holes
    
          3dmask_tool -input mask_anat.*+tlrc.HEAD -prefix ma.fill.7 \
                      -dilate_input 5 -5 -frac 0.7 -fill_holes
    
    ----------------------------------------
    informational command arguments (execute option and quit):
    
        -help                   : show this help
        -hist                   : show program history
        -ver                    : show program version
    
    ----------------------------------------
    optional command arguments:
    
        -count                  : count the voxels that overlap
    
            Instead of created a binary 0/1 mask dataset, create one with.
            counts of voxel overlap, i.e each voxel will contain the number
            of masks that it is set in.
    
        -datum TYPE             : specify data type for output
    
                e.g: -datum short
                default: -datum byte
    
            Valid TYPEs are 'byte', 'short' and 'float'.
    
        -dilate_inputs D1 D2 ... : dilate inputs at the given levels
    
                e.g. -dilate_inputs 3
                e.g. -dilate_inputs -4
                e.g. -dilate_inputs 8 -8
                default: no dilation
    
            Use this option to dilate and/or erode datasets as they are read.
    
            Dilations are across the 18 voxel neighbors that share either a
            face or an edge (i.e. of the 26 neighbors in a 3x3x3 box, it is
            all but the outer 8 corners).
            
            An erosion is specified by a negative dilation.
            
            One can apply a list of dilations and erosions, though there
            should be no reason to apply more than one of each.
            
            Note: use -dilate_result for dilations on the combined masks.
    
        -dilate_result D1 D2 ... : dilate combined mask at the given levels
    
                e.g. -dilate_result 3
                e.g. -dilate_result -4
                e.g. -dilate_result 8 -8
                default: no dilation
    
            Use this option to dilate and/or erode the result of combining
            masks that exceed the -frac cutoff.
    
            See -dilate_inputs for details of the operation.
    
        -frac LIMIT             : specify required overlap threshold
    
                e.g. -frac 0    (same as -union)
                e.g. -frac 1.0  (same as -inter)
                e.g. -frac 0.6
                e.g. -frac 17
                default: union (-frac 0)
    
            When combining masks (across datasets and sub-bricks), use this
            option to restrict the result to a certain fraction of the set of
            volumes (or to a certain number of volumes if LIMIT > 1).
    
            For example, assume there are 7 volumes across 3 datasets.  Then
            at each voxel, count the number of masks it is in over the 7
            volumes of input.
    
                LIMIT = 0       : union, counts > 0 survive
                LIMIT = 1.0     : intersection, counts = 7 survive
                LIMIT = 0.6     : 60% fraction, counts >= 5 survive
                LIMIT = 5       : count limit, counts >= 5 survive  
    
            See also -inter and -union.
    
        -inter                  : intersection, this means -frac 1.0
        -union                  : union, this means -frac 0
    
        -fill_holes             : fill holes within the combined mask
    
            This option can be used to fill holes in the resulting mask, i.e.
            after all other processing has been done.
    
            A hole is defined as a connected set of voxels that is surrounded
            by non-zero voxels, and which contains no volume edge voxel, i.e.
            there is no connected voxels at a volume edge (edge of a volume
            meaning any part of any of the 6 volume faces).
    
            To put it one more way, a zero voxel is part of a hole if there
            is no path of zero voxels (in 3D space) to a volume face/edge.
            Such a path can be curved.
    
            Here, connections are via the 6 faces only, meaning a voxel could
            be consider to be part of a hole even if there were a diagonal
            path to an edge.  Please pester me if that is not desirable.
    
        -fill_dirs DIRS         : fill holes only in the given directions
    
                e.g. -fill_dirs xy
                e.g. -fill_dirs RA
                e.g. -fill_dirs XZ
    
            This option is for use with -fill holes.
    
            By default, a hole is a connected set of zero voxels that does
            not have a path to a volume edge.  By specifying fill DIRS, the
            filling is done restricted to only those axis directions.
    
            For example, to fill holes once slice at a time (in a sagittal
            dataset say, with orientation ASL), one could use any one of the
            options:
    
                -fill_dirs xy
                -fill_dirs YX
                -fill_dirs AS
                -fill_dirs ip
                -fill_dirs APSI
    
            DIRS should be a single string that specifies 1-3 of the axes
            using {x,y,z} labels (i.e. dataset axis order), or using the
            labels in {R,L,A,P,I,S}.  Such labels are case-insensitive.
    
        -inputs DSET1 ...       : specify the set of inputs (taken as masks)
    
                e.g. -inputs group_mask.nii
                e.g. -inputs full_mask.subj*+tlrc.HEAD
                e.g. -inputs amygdala_subj*+tlrc.HEAD
    
            Use this option to specify the input datasets to process.  Any
            non-zero voxel will be consider part of that volume's mask.
    
            An input dataset is allowed to have multiple sub-bricks.
    
        -prefix PREFIX          : specify a prefix for the output dataset
    
                e.g. -prefix intersect_mask
                default: -prefix combined_mask
    
            The resulting mask dataset will be named using the given prefix.
    
        -quiet                  : limit text output to errors
    
            Restrict text output.  This option is equivalent to '-verb 0'.
    
            See also -verb.
    
        -verb LEVEL             : specify verbosity level
    
            The default level is 1, while 0 is considered 'quiet'.
            The maximum level is currently 3, but most people don't care.
    
    -------------------------------
    R. Reynolds         April, 2012
    ----------------------------------------------------------------------
