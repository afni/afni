**************
@Align_Centers
**************

.. _ahelp_@Align_Centers:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @Align_Centers <-base BASE> <-dset DSET> [-no_cp] 
                         [-child CHILD_2 ... CHILD_N] [-echo]
    
       Moves the center of DSET to the center of BASE.
       By default, center refers to the center of the volume's voxel grid.
       Use -cm to use the brain's center of mass instead.
    
       AND/OR creates the transform matrix XFORM.1D needed for this shift.
       The transform can be used with 3dAllineate's -1Dmatrix_apply 
          3dAllineate   -1Dmatrix_apply XFORM.1D    \
                        -prefix PREFIX -master BASE \
                        -input DSET
    
       -echo: Echo all commands to terminal for debugging
       -overwrite: You know what
       -prefix PREFIX:  Result will be named using PREFIX, instead of the
                        current prefix with _shft appended.
                      * Does not work with -child or -no_cp.
       -1Dmat_only: Only output the transfrom needed to align
                    the centers. Do not shift any child volumes.
                    The transform is named DSET_shft.1D
       -1Dmat_only_nodset: Like above, but no dsets at all
                    are created or changed.
       -base BASE: Base volume, typically a template.
       -dset DSET: Typically an anatomical dset to be
                   aligned to BASE.
       -child CHILD_'*': A bunch of datasets, originally
                         in register with DSET, that
                         should be shifted in the same
                         way.
       -no_cp: Do not create new data, shift existing ones
               This is a good option if you know what you 
               are doing. It will save you a lot of space.
               See NOTE below before using it.
    
        DSET and CHILD_'*' are typically all the datasets 
        from a particular scanning session that
        you want to eventually align to BASE.
        Such an operation is needed when DSET and CHILD_'*'
        overlap very little, if at all with BASE
    
     Note that you can specify *.HEAD for the children even 
     if the wildcard substitution would contain DSET 
     and possibly even BASE. The script will not process
     a dataset twice in one execution.
    
     Center options:
       -grid: (default) Center is that of the volume's grid
       -cm : Center is the center of mass of the volume.
       -cm_no_amask : Implies -cm, but with no -automask.
    
    
       See also @Center_Distance
    
     NOTE: Running the script multiple times on the same data
           will cause a lot of trouble. That is why the default
           is to create new datasets as opposed to shifting the
           existing ones. Do not use -no_cp unless you know what
           you are doing.
           To undo errors caused by repeated executions
           look at the history of each dset and undo
           the excess 3drefit operations.
    
    Requires 3drefit newer than Oct. 02/02.
    
    Ziad Saad (saadz@mail.nih.gov)
    SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
