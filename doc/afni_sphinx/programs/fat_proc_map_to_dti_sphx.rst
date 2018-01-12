.. contents:: 
    :depth: 4 

*******************
fat_proc_map_to_dti
*******************

.. code-block:: none

    # -----------------------------------------------------------------------
    
     This program is for bringing data sets into DWI space, with the
     particular thought that bringing anatomically-defined ROI maps or EPI
     data that are aligned to a subject's anatomical might be useful.
     This might be useful after having run FreeSurfer, for example.
    
     An affine transformation matrix between, say, a subject's T1w volume
     and a DWI reference volume is calculated, and then applied to
     follower data sets.  The transformation can be applied either as 'NN'
     (-> for preserving integer values in sets) or as 'wsinc5' (-> if one
     has floating point values).  The final dsets will reside in the DWI
     space. Yay.
    
     At the moment this program *assumes* that the input source ('-source
     SSS') and reference base ('-base BBB') are from the same subject,
     because only 12 DOF affine alignment is calculated (using
     3dAllineate).  Maybe something could be done with 3dQwarp in the
     future.  Maybe.
    
     This program mainly assumes that the T1w and DWI reference volume
     have similar contrasts expected for standard sequences and healthy
     adult brains.  This might still work for other applications, but
     caveat emptor (even more than usual!).  This would *not* be
     recommended for aligning brains that aren't from the same subject.
    
      Ver. 1.9 (PA Taylor, Sep 22, 2017)
    
    # ----------------------------------------------------------------------
    
      OUTPUT:
    
         + NIFTI file: aligned T1w volume.
    
         + NIFTI files: each follower DSET* ends up in the DWI/DTI space
           and has a respective name PREFIX_DSET*.nii.gz.
    
         + QC snapshots of the T1w volume overlaying the DWI reference 
           volume, and also the T1w edges overlaying the ref vol.
    
         + QC snapshots of each of the follower dsets overlaying the DWI ref
           volume.
    
    # ----------------------------------------------------------------------
    
      RUNNING:
    
        fat_proc_map_to_dti \
          -source   SSS                                  \
          -base     DDD                                  \
          -prefix  PPP                                   \
          {-followers_NN      DSET01 DSET02 DSET03 ...}  \
          {-followers_wsinc5  DSET1 DSET2 DSET3 ...}     \
          {-followers_surf    SURF1 SURF2 SURF3 ...}     \
          {-followers_ndset   NDSET1 NDSET2 NDSET3 ...}  \
          {-followers_spec    SPEC1 SPEC2 SPEC3 ...}     \
          {-matrix MMM}                                  \
          {-workdir WWW}                                 \
          {-no_cmd_out}                                  \
          {-no_clean} 
    
      where:
    
       -source SSS    :T1w volume (required); 'source' volume from which we
                       are mapping, such as an anatomical volume in whose
                       space ROIs might have been defined.  SSS gets
                       mapped into the '-base BBB' volume's space.
       -base BBB      :DWI reference volume (required; should be from same
                       subject as SSS), such as the b=0 (or minimally DWed
                       volume), for aligning to; subbrick selections are
                       allowed, so that dwi_dwi.nii'[0]', for example,
                       would be allowed.  This is the base dset for the
                       alignment, with the purpose to bring other volumes
                       into the DWI/DTI space (see the '-followers* ...'
                       options, below).  **NOTE**: BBB and SSS should be
                       from the same subject by this function, because
                       only affine alignment with 3dAllineate is
                       performed!
    
       -prefix  PPP   :output prefix for files and snapshots.  Required.
    
       -followers_NN  DSET01 DSET02 DSET03 ...
                      :apply the same transformation to 'follower' data
                       sets; one or more dsets can be listed, with each
                       assumed to overlay on the T1W source set. The 'NN'
                       interpolation of 3dAllineate is applied to these
                       dsets, so that integer values remain integer
                       valued; thus, these might be dsets with ROI maps
                       already created.  NB: subbrick selectors are not
                       allowed on the DSETs here at present.  Labeltables 
                       attached to these dsets do get propagated, as well.
       -followers_wsinc5  DSET1 DSET2 DSET3 ...
                       similar to the above '-followers_NN ...', except in
                       this case the final applied mapping is 'wsinc5', which
                       is appropriate, for example, for floating point values.
                       Again, a list of one or more volumes (sans subbrick
                       selectors) can be provided here.  No labeltable is
                       propagated for these sets (I doubt they would have one,
                       anyways).
       -followers_surf    SURF1 SURF2 SURF3 ...
                      :similar to the above '-followers_* ...', except in 
                       this case the mapping is applied to surface dsets, such 
                       as '*.gii'.  Per usual, a list of one or more surfaces  
                       can be provided here. 
       -followers_ndset   NDSET1 NDSET2 NDSET3 ...
                      :similar to the above '-followers_* ...', except in 
                       this case the mapping is applied to '*.niml.dset' files,  
                       such as '*.annot.niml.dset'.  Per usual, a list of one or 
                       more surfaces can be provided here. Prob wouldn't make  
                       sense to use this without using '-followers_surf ...'.
       -followers_spec    SPEC1 SPEC2 SPEC3 ...
                      :similar to the above '-followers_* ...', except in 
                       this case the mapping is applied to '*.spec' files.
                       Per usual, a list of one or more surfaces can be 
                       provided here.  Wouldn't make sense to use this without 
                       using both '-followers_surf ...' and '-followers_ndset ...'
                       to map the dsets referred to in the file!
     
       -matrix MMM    :one can apply a pre-made matrix that has been made by
                       3dAllineate previously.  With this option.  If you want.
    
       -workdir WWW   :specify a working directory, which can be removed;
                       (default name = '__WORKING_map_to_dti')
    
       -no_cmd_out    :don't save the command line call of this program
                       and the location where it was run (otherwise, it is
                       saved by default in the ODIR/).                     
       -no_clean      :do not delete temporary working directory (default is 
                       to remove it to save disk space).
    
    # ----------------------------------------------------------------------
    
      EXAMPLE
    
        fat_proc_map_to_dti  \
            -source          brain.nii            \
            -base            dwi_dwi.nii.gz'[0]'  \
            -prefix          indt                 \
            -followers_NN    aparc*_REN_*.nii.gz  \
            -followers_surf  std.141.*gii         \
            -followers_ndset std.141.*niml.dset   \
            -followers_spec  std.141.*.spec
    
    # -----------------------------------------------------------------------
