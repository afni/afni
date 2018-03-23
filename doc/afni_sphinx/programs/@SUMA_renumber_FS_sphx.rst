.. _ahelp_@SUMA_renumber_FS:

*****************
@SUMA_renumber_FS
*****************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ---------------------------------------------------------------------
    
        This script is now run at the end of modern @SUMA_Make_Spec_FS
        commands, or it can be run separately for data that had been
        processed using older versions of AFNI.
    
        Originally written and tested on FreeSurfer (FS) v5.3 output from
        default running of 'recon-all'. This should now work for FS v6.0
        default running of 'recon-all', as well.
    
        Written by PA Taylor (NIMH, NIH; 2016-7).
    
        PURPOSE:
    
        This program will take the aparc+aseg.nii.gz and
        aparc.a2009s+aseg.nii.gz parcellation files produced by FreeSurfer
        (FS) and converted to NIFTI by @SUMA_Make_Spec_FS, and make the
        following related data sets (with the same prefix) for each: 
    
            + A copy of the whole parcellation/segmentation and renumber the
              ROIs to be smaller (for colorbar representation); this file
              is called "*_REN_all.nii.gz".
    
            + Tissue segmentation maps (not binary, but containing the
              renumbered ROI values), based on our best guesses of of what
              each is, from both the 'mri_binarize' command in FS and our
              own supplementary reading of the ROI names. The following
              files are output:
    
                *_REN_gm.nii.gz    :gray matter
                *_REN_wmat.nii.gz  :white matter
                *_REN_csf.nii.gz   :cerebrospinal fluid
                *_REN_vent.nii.gz  :ventricles and choroid plexus
                *_REN_othr.nii.gz  :optic chiasm, non-WM-hypointens, etc.
                *_REN_unkn.nii.gz  :FS-defined "unknown", with voxel value >0
    
            + A labeltable of the new ROI values: "*_REN_all.niml.lt".
              This labeltable is attached to each of the *_REN_*.nii.gz
              files.
    
        RUNNING:
    
        At the moment, the function just takes a single, required
        argument, which is the location of the 'SUMA/' directory created
        by @SUMA_Make_Spec_FS.  The program also requires being able to
        see the two 'afni_fs_aparc+aseg_*.txt' files in the AFNI binary
        directory: that is where the information on renumbering the FS
        ROIs is).
    
            $ @SUMA_renumber_FS SUMA_DIR
    
        where SUMA_DIR is either the full or relative path to the 'SUMA/'
        directory (including that directory name).
    
        EXAMPLE:
        
            $ @SUMA_renumber_FS /data/study/SUBJ_01/FS/SUMA
    ___________________________________________________________________________
