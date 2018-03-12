************************
fat_proc_align_anat_pair
************************

.. _fat_proc_align_anat_pair:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    # -----------------------------------------------------------------------
    
     This program is for aligning a T1w anatomical to a T2w anatomical
     using solid body parameters (i.e., only translation and rotation);
     this program does not clean or alter the T1w's brightness values
     (beyond minor smoothing from regridding).  If one is going to be
     entering the T1w volume into the realms of FreeSurfer (FS), one might
     want to do this just *before* that step.  If one wants axialized (or
     AC-PC-ized) anatomical volumes, one could perform that step on the
     T2w volume *before* using this function.
    
     This program mainly assumes that the T1w and T2w volume come from the
     same subject, and have similar contrasts expected for standard
     sequences and healthy adult brains.  This might still work for other
     applications, but caveat emptor (even more than usual!).  This would
     *not* be recommended for aligning brains that aren't from the same
     subject.
    
     As part of this alignment, the T1w volume will end up with the same
     orientation and a similar FOV as the T2w volume.  Additionally, by
     default, the anatomical will be prepped a bit with an eye toward
     using FS, to have properties favorable to using it: 
    
       + the T1w volume is resampled to isotropic spatial resolution of
         either 1 mm voxel edges or, if the input volume has any edge
         length smaller than this, to that value (i.e., resampled to 1 mm
         or the minimum input voxel edge length, whichever is less).  The
         user can adjust this with the '-newgrid ...' option, or decide to 
         match the grid of the T2w volume via '-out_t2w_grid'. 
    
       + the T1w will have a FOV matching or quite similar to the T2w
         volume (as well as matching orientation).
    
       + the output T1w will be checked to ensure having an even number of
         rows along each axis; this is because FS somehow seems to want
         this for best behavioring. A slice of zeros will be appended in
         any direction with with an odd number of slices; turned off
         by using '-no_fs_prep'.
    
     Note that, if you are preparing to use FS afterwards, then make sure
     to use their current help files, archives, etc. for all options and
     settings.  For example, while at present (March, 2017) FS does seem
     to prefer isotropic voxels with 1 mm edge length by default, one can
     use high resolution options for data acquired at higher resolution.
     Anyways, you can read more about that there.
    
      Ver. 1.9 (PA Taylor, Jan 12, 2018)
    
    # ----------------------------------------------------------------------
    
      OUTPUT:
    
         + NIFTI file: aligned T1w volume
    
         + QC snapshots of the T1w volume overlaying the T2w volume, and
           also the T1w edges overlaying the T2w volume.
    
    # ----------------------------------------------------------------------
    
      RUNNING:
    
        fat_proc_align_anat_pair \
          -in_t1w  T1W                          \
          -in_t2w  T2W                          \
          -prefix  PPP                          \
          {-newgrid RES}                        \
          {-out_t2w_grid}                       \
          {-in_t2w_mask MASK_T2W}               \
          {-matrix MMM}                         \
          {-workdir WWW}                        \
          {-no_fs_prep}                         \
          {-warp WAR}                           \
          {-no_cmd_out}                         \
          {-no_clean} 
    
      where:
    
       -in_t1w  T1W   :T1w volume (required).
       -in_t2w  T2W   :T2w volume (required; preferably from same subject as
                       T1W).
    
       -prefix  PPP   :output prefix for files and snapshots (required).
    
       -newgrid RES   :specify output T1w volume's final resolution; will be
                       isotropic in this value (default: 1 mm, or smallest voxel
                       edge length of input T1W if that value is < 1 mm).
       -out_t2w_grid  :final T1w volume is on the T2W volume's grid (with 
                       possible addition of padding with a slice of zeros if
                       prepping for FS).
    
       -no_fs_prep    :check output T1w volume to have an even number of slices
                       in all directions, because FS appears to like/require this
                       (at least at this slicetime of the universe, March, 2017).
    
       -in_t2w_mask MASK_T2W
                      :can input a mask to apply to the t2w volume for
                       alignment purposes; might help in times of aligning 
                       hardship.
    
       -warp WAR      :can choose which of the possible affine degrees of freedom
                       are employed in the warping, selecting them in the same
                       manner described in 3dAllineate's help;  that is, WAR can
                       be any of shift_only, shift_rotate, shift_rotate_scale, or
                       affine_general.  Default: WAR = shift_rotate.
    
       -matrix MMM    :one can apply a pre-made matrix that has been made by
                       3dAllineate previously.  With this option.  If you want.
    
       -workdir WWW   :specify a working directory, which can be removed;
                       (default name = '__WORKING_align_anat_pair')
    
       -no_cmd_out    :don't save the command line call of this program
                       and the location where it was run (otherwise, it is
                       saved by default in the ODIR/).                     
       -no_clean      :no not delete temporary working directory (default is to 
                       remove it to save disk space).
    
    # ----------------------------------------------------------------------
    
      EXAMPLE
    
        # have isotropic 1x1x1 mm final anat:
        fat_proc_align_anat_pair  \
            -in_t1w    MPRAGE.nii.gz        \
            -in_t2w    T2w_anat.nii.gz      \
            -newgrid   1.0                  \
            -prefix    t1w_alnd
    
        # match the final anat resolution to that of the t2w dset:
        fat_proc_align_anat_pair  \
            -in_t1w    MPRAGE.nii.gz        \
            -in_t2w    T2w_anat.nii.gz      \
            -out_t2w_grid                   \
            -prefix    t1w_alndb
    
    
    # -----------------------------------------------------------------------
