********************
fat_proc_select_vols
********************

.. _fat_proc_select_vols:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    # -----------------------------------------------------------------------
    
     This program is for building a selector string for AFNI subbricks
     and/or 1D text files.  It makes use of J. Rajendra's
     '@djunct_dwi_selector.sh' script to make a list of 'bad' volume
     indices by clicking on individual volumes in a montage image.  Pretty
     cool.
    
     In the end, a selector string of volumes *to keep* (i.e., the
     complement of the set of indices chosen with clicks) is output to
     screen as well as stored in a text file.
    
      Ver. 1.5 (PA Taylor, Sep 04, 2017)
    
    # ----------------------------------------------------------------------
    
      OUTPUT:
    
        + PPP_bads.txt: text file with AFNI-usable selector string, which
                        can be put into either square brackets [] or curly
                        brackets {}, whichever is appropriate for a given
                        application.
    
    # ----------------------------------------------------------------------
    
      RUNNING:
    
        fat_proc_select_vols  \
            -in_dwi   DDD                  \
            -in_img   IM2D                 \
            {-in_bads TTT}                 \
            -prefix  PPP                   \
            {-apply_to_vols}               \
            {-do_movie AGIF|MPEG}          \
            {-workdir WWW}                 \
            {-no_cmd_out} 
    
      where:
    
       -in_dwi  DDD   :input DWI set (required).
       -in_img  IM2D  :2d image of DDD, such as made by a fat_proc* 
                       script, or @djunct_*imager directly (required).
       -in_bads TTT   :(optional) a single column file of integers,
                       such as made by a previous run of fat_proc_select_vols.
                       For example, if one has dual phase-encoded
                       DWI data, then one might make a list of bads
                       from the AP-acquired set and then add to it any
                       bad indices from the PA-acquired set.
    
       -prefix  PPP   :output prefix for files. Required.
    
       -apply_to_vols :switch to apply the created selection of good 
                       volumes to the DWI dset.  NB: if you are using 
                       this function to select out bad volumes from 
                       a dual phase encode set, then you *wouldn't* want 
                       to use this option, because you want to apply
                       the complete removal to *both* volumes.  Note also,
                       that once you apply this selection to the volumes,
                       you also need to apply it to any bval, bvec, bmatrix,
                       etc. text files!
    
        -do_movie AGIF | MPEG:
                       when "-apply_to_vols" is used, static images are 
                       output by default;  one can use this option with 
                       either of the given arguments to output a movie of the
                       newly created dset.  Only those arguments can be used
                       at present.
    
       -workdir WWW   :specify a working directory, which can be removed;
                       (default name = '__WORKING_select_vols').
    
       -no_cmd_out    :don't save the command line call of this program
                       and the location where it was run (otherwise, it is
                       saved by default in the ODIR/).                     
    
    # ----------------------------------------------------------------------
    
      EXAMPLE
    
        fat_proc_select_vols    \
            -in_dwi DWI.nii.gz               \
            -in_img QC/DWI_sepscl.sag.png    \
            -prefix DWI_trim
    
    
        fat_proc_select_vols    \
            -in_dwi DWI_ap.nii.gz             \
            -in_img QC/DWI_ap_sepscl.sag.png  \
            -in_bads DWI_trim_bads.txt        \
            -prefix DWI_trim_both 
    
    # -----------------------------------------------------------------------
