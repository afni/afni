.. contents:: 
    :depth: 4 

**********************
fat_proc_axialize_anat
**********************

.. code-block:: none

    -------------------------------------------------------------------------
    
      This program is designed to help line up the major axes of an
      anatomical volume with those of the volumetric field of view in
      which it sits.  A benefit of this would be to have slices that are
      more parallel to standard viewing planes and/or a bit more
      uniform/standardized across a group.  This may be useful, for
      example, if you want to use an anatomical for registration of other
      data (e.g., diffusion data going to be processed using TORTOISE, esp
      if coloring by local orientation), and you want *that* data to be
      more regularly oriented for viewing, as well.
    
      This program works by registering (affinely, 9 DOF) the input volume
      to some user-defined reference image, but in the end then only
      applying the translation+rotation parts of the registration to the
      input volume.  Before the registration is done, some polishing of
      the input volume is performed, in order to remove outliers, but none
      of these steps are applied to the output volume itself. Thus, the
      idea is to distort the original brain as little as possible (NB:
      smoothing will occur as part of this process, e.g., due to rotation
      and any regridding), just to reorient it in space.  The output
      volume can be up/downsampled at the same time, if desired.
    
      You probably *wouldn't* want to use this if your anatomical data set
      really fills up its volume (i.e., has no space to rotate/resituation
      itself).  In that case, you might want to pre-zeropad the volume?
    
      REQUIRES: AFNI.
    
      Ver. 3.6 (PA Taylor, Sep 04, 2017)
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      This script has two *required* arguments ('-inset ...' and '-refset
      ...'), and the rest are optional:
    
      $ fat_proc_axialize_anat  \
            -inset   IN_FILE                      \
            -refset  REF_FILE                     \
            {-mode_t2w | -mode_t1w}               \
            -prefix PPP                           \
            {-workdir  WWW}                       \
            {-out_match_ref}                      \
            {-extra_al_wtmask WT }                \
            {-extra_al_cost CC}                   \
            {-extra_al_inps II}                   \
            {-extra_al_opts SS}                   \
            {-focus_mask MMM}                     \
            {-remove_inf_sli II}                  \
            {-post_lr_symm}                       \
            {-no_pre_lr_symm}                     \
            {-no_clean}                           \
            {-qc1_ulay_range UMIN UMAX}           \
            {-no_qc_view}                         \
            {-qc_prefix QCP}
    
      where: 
    
      -inset  IN_FILE  :is the full name of the input anatomical volume.
      -refset REF_FILE :is the full name of the reference volume, such as
                        TT or MNI or something (probably you want to match
                        the contrast of your anatomical INFILE, whether 
                        there is a skull or not, etc.).
    
      -prefix  PPP     :output prefix for files and snapshots (required).
    
      -out_match_ref   :switch to have the final output volume be in the same
                        'space' (FOV, spatial resolution) as the REF_FILE. 
                        Might be useful for standardizing the reference
                        output across a group, or at least centering the brain
                        in the FOV. (This applies a '-master REF_FILE' to the
                        final 3dAllineate in the script.)
    
      -mode_t2w        :switch to alter some intermediate steps (turn off
                        unifizing and raise voxel ceiling threshold).
                        This is particularly useful (= essential,
                        probably) when dealing with a (adult) T2w image,
                        which tends to be bright in the CSF and darker in
                        other tissues; default options are for dealing
                        with (adult) T1w brains, where the opposite is the
                        case.
      -mode_t1w        :similar to the preceding option, but specifying an 
                        image with (human, adult) t1w-like contrast
                        has been input.
         NB ---> one of these 'mode_*' setting MUST be picked.
    
      -extra_al_wtmask WT:
                        Axialization is generally based on an overall 
                        whole brain alignment.  If you want, however, you
                        can add extra emphasis to part of the weight mask WT
                        for deciding what is good alignment.  For example, 
                        you might make a WB mask of values ~1 and a make 
                        a subcortical volume have larger values ~5 or so, so
                        that that part of the brain's alignment carries more
                        weight (in this example, behaving more like AC-PC
                        alignment, potentially).
    
     -extra_al_cost CC :specify a cost function for 3dAllineate to work
                        with (default is 'lpa'; one might investigate
                        'lpc', esp. if contrasts differ between the
                        IN_FILE and REF_FILE, or 'nmi').
     -extra_al_inps II :specify extra options when *calculating* the warp
                        with 3dAllineate.  These could be any
                        option+argument combination from the 3dAllineate
                        helpfile (except the cost function would be done
                        with "-extra_al_cost CC").
     -extra_al_opts SS :specify extra output options when *applying* the
                        warp with 3dAllineate at the end.  One I could see
                        being useful would be having "-newgrid X", where X
                        is desired final resolution of the data.
    
      -focus_mask MMM  :input a mask of the inset that gets applied early
                        on to focus the processing+alignment; the final,
                        axialized volume will not have the mask applied,
                        it's just used to help get rid of non-brain 
                        garbage.  Note: before application, MMM gets binarized
                        to 1 where MMM>0 and 0 elsewhere.
    
    -remove_inf_sli II :sometimes data is acquired with lots of nonbrain
                        volume in the FOV, particularly neck and things like
                        that.  While necks are important, they also might
                        move the center of mass estimate of the brain
                        far lower than it should be.  You can get rid of this
                        by applying this option, to remove 'II' number of 
                        slices from the inferior part of the FOV.
    
      -no_pre_lr_symm  :a pre-alignment left-right symmetrization is
                        performed by default, but you can turn it off if you
                        desire (probably wouldn't want to in most cases, 
                        unless *weird* stuff were happening).
      -post_lr_symm    :a post-alignment left-right symmetrization can be 
                        added, if desired.
    
      -workdir WWW     :the name of the working subdirectory in the output
                        directory can be specified (default: __WORKING_axialize_anat).
    
      -no_clean        :is an optional switch to NOT remove working 
                        directory '__WORKING_axialize_anat'; (default: remove working dir).
      -no_cmd_out      :by default, a copy of the command and file location
                        from which it is run is dumped into the WORKDIR 
                        (file name: 'PREFIX*_cmd.txt'). 
                        If you don't want this to happen, then use this 
                        switch.
    
      -qc1_ulay_range UMIN UMAX
                       :provide a min (UMIN) and max (UMAX) range for 
                        underlay grayscale bar (black=UMIN; white=UMAX).
                        For QC visualization only-- does not affect the 
                        actual MRI data files. 
                        
      -no_qc_view      :turn off default+automatic QC image saving/viewing 
                        (whyyy would you do this?).
      -qc_prefix QCP   :provide a prefix for the QC stuff, separate from
                        the PREFIX above.
    
     ------------------------------------------------------------------------
    
      OUTPUTS:
    
        PREFIX.nii.gz   :an anatomical data set that is *hopefully*
                         regularly situated within its FOV volume.  Thus, 
                         the axial slices would sit nicely within a given
                         view window, etc.
    
        WORKDIR         :the working directory with intermediate files, so
                         you can follow along with the process and possibly
                         troubleshoot a bit if things go awry (what are the
                         odds of *that* happening?).
    
    -------------------------------------------------------------------------
    
      EXAMPLE:
        
       fat_proc_axialize_anat  \
            -inset  SUB001/ANATOM/T2.nii.gz                            \
            -refset /somewhere/mni_icbm152_t1_tal_nlin_sym_09a_MSKD.nii.gz \
            -mode_t1w                                                  \
            -extra_al_opts "-newgrid 1.0"                              \
            -prefix t2w_axlz
    
        or
    
       fat_proc_axialize_anat  \
         -inset  SUB001/ANATOM/T2.nii.gz                            \
         -refset /somewhere/mni_icbm152_t2_tal_nlin_sym_09a.nii.gz  \
         -extra_al_wtmask mni_icbm152_t2_relx_tal_nlin_sym_09a_ACPC_wtell.nii.gz \
         -mode_t2w                                                  \
         -prefix t2w_axlz
    
    -------------------------------------------------------------------------
      TIPS:
    
        + When analyzing adult T1w data, using the following option might
          be useful:
             -extra_al_inps "-nomask"
          Using this, 3dAllineate won't try to mask a subregion for 
          warping/alignment, and I often find this helpful for T1w volumes.
    
        + For centering data, using the '-out_match_ref' switch might be 
          useful; it might also somewhat, veeeery roughly help standardize
          a group of subjects' data in terms of spatial resolution, centering
          in FOV, etc.
    
        + To try to get something closer to AC-PC alignment, one can add in a 
          weight mask with '-extra_al_wtmask ...' that has the ~subcortical 
          region given extra weight. 
    
