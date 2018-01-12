.. contents:: 
    :depth: 4 

*********
@SSwarper
*********

.. code-block:: none

    
    Script to skull-strip and warp to the MNI 2009 template.
    
    Usage: @SSwarper T1dataset SubID [minp]
    
    T1dataset = a T1-weighted dataset, not skull-stripped,
                with resolution about 1 mm.
    SubID     = name code for output datasets (e.g., 'sub007').
    minp      = optional: minimum patch size on final 3dQwarp
                          (if not given, 11 is used)
    
    The template dataset MNI152_2009_template.nii.gz is supplied
    with AFNI binaries and is described later in this help.
    
    Outputs:
    --------
    Suppose the SubID is 'sub007' (because you scanned Bond, James Bond?).
    Then the outputs from this script will be
    
      anatU.sub007.nii        = intensity uniform-ized original dataset;
      anatS.sub007.nii        = first pass skull-stripped original dataset;
      anatSS.sub007.nii       = second pass skull-stripped original dataset;
      anatQQ.sub007.nii       = skull-stripped dataset nonlinearly warped to
                                MNI template space;
      anatQQ.sub007.aff12.1D  = affine matrix to transform original dataset
                                to MNI template space;
      anatQQ.sub007_WARP.nii  = incremental warp from affine transformation
                                to nonlinearly aligned dataset;
      AMsub007.jpg            = 3x3 snapshot image of the anatQQ.sub007.nii
                                dataset with the edges from the MNI template
                                overlaid -- to check the alignment;
      MAsub007.jpg            = similar to the above, with the roles of the
                                template and the anatomical datasets reversed.
    
    * The .aff12.1D and _WARP.nii transformations need to be catenated to get
      the full warp from orginal space to MNI space; example:
        3dNwarpApply -nwarp 'anatQQ.sub007_WARP.nii anatQQ.sub007.aff12.1D' ...
    
    * It is important to examine (at least) the two .jpg snapshot images to
      make sure that the skull-stripping and nonlinear warping worked well.
    
    * The inputs needed for the '-tlrc_NL_warped_dsets' option to afni_proc.py
      are (in this order):
        anatQQ.sub007.nii anatQQ.sub007.aff12.1D anatQQ.sub007_WARP.nii
    
    * When B-O-B uses this script for skull-stripping plus warping, He
      gives afni_proc.py these options (among others), after running
      @SSwarper successfully -- here, 'subj' is the subject identifier:
    
      set tpath = `@FindAfniDsetPath MNI152_2009_template.nii.gz`
      if( "$tpath" == "" ) exit 1
      afni_proc.py [...other stuff here...]                             \
        -copy_anat anatSS.${subj}.nii -anat_has_skull no                \
        -align_opts_aea -ginormous_move -deoblique on -cost lpc+ZZ      \
        -volreg_align_to MIN_OUTLIER -volreg_align_e2a                  \
        -volreg_tlrc_warp -tlrc_base $tpath/MNI152_2009_template.nii.gz \
        -tlrc_NL_warp                                                   \
        -tlrc_NL_warped_dsets                                           \
           anatQQ.${subj}.nii                                           \
           anatQQ.aff12.1D                                              \
           anatQQ.${subj}_WARP.nii
    
    The Template Dataset
    --------------------
    The file MNI152_2009_template.nii.gz has 5 volumes:
      [0] = skull-stripped template brain volume
      [1] = skull-on template brain volume
      [2] = weight mask for nonlinear registration, with the
            brain given greater weight than the skull
      [3] = binary mask for the brain
      [4] = binary mask for gray matter plus some CSF (slightly dilated)
            -- this volume is not used in this script
            -- it is intended for use in restricting FMRI analyses
               to the 'interesting' parts of the brain
            -- this mask should be resampled to your EPI spatial
               resolution (see program 3dfractionize), and then
               combined with a mask from your experiment reflecting
               your EPI brain coverage (see program 3dmask_tool).
    
    You Know My Methods, Watson
    ---------------------------
    #1: Uniform-ize the input dataset's intensity via 3dUnifize.
         ==> anatU.sub007.nii
    #2: Strip the skull with 3dSkullStrip, with mildly agressive settings.
         ==> anatS.sub007.nii
    #3: Nonlinearly warp (3dQwarp) the result from #1 to the skull-on
        template, driving the warping to a medium level of refinement.
    #4: Use a slightly dilated brain mask from the template to
        crop off the non-brain tissue resulting from #3 (3dcalc).
    #5: Warp the output of #4 back to original anatomical space,
        along with the template brain mask, and combine those
        with the output of #2 to get a better skull-stripped
        result in original space (3dNwarpApply and 3dcalc).
         ==> anatSS.sub007.nii
    #6  Restart the nonlinear warping, registering the output
        of #5 to the skull-off template brain volume (3dQwarp).
         ==> anatQQ.sub007.nii (et cetera)
    #7  Use @snapshot_volreg3 to make the pretty pictures.
         ==> AMsub007.jpg and MAsub007.jpg
    
    Temporary Files
    ---------------
    If the script crashes for some reason, it might leave behind files
    whose names start with 'junk.SSwarper' -- you should delete these
    files manually.
    
    -------------------------------------------------------
    Author: Bob, Bob, there is one Bob, He spells it B-O-B.
    -------------------------------------------------------
