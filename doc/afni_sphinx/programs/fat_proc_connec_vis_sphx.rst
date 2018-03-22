*******************
fat_proc_connec_vis
*******************

.. _fat_proc_connec_vis:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    # -----------------------------------------------------------------------
    
     This program is for visualizing the volumetric output of tracking,
     mostly for the '-dump_rois ...' from 3dTrackID.  These are basically
     the WMC (white matter connection) maps through which tract bundles run
     (for DET and MINIP tracking modes), or through which a suprathreshold
     number of tracts run in PROB mode.
    
     This program creates surface-ized views of the separate WMCs which can
     be viewed simultaneously in 3D with SUMA. 
    
      Ver. 1.3 (PA Taylor, Mar 5, 2018)
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      fat_proc_connec_vis  \
        -in_rois       NETROIS       \
        -prefix        PPP           \
       {-prefix_file   FFF}          \
       {-tsmoo_kpb     KPB}          \
       {-tsmoo_niter   NITER}        \
       {-iso_opt       ISO_OPT}      \
       {-trackid_no_or}              \
       {-output_tcat}                \
       {-output_tstat}               \
       {-wdir          WWW}          \
       {-no_clean}
    
    
      where
    
        -in_rois NETROIS   :list of separate files, each with single ROI
                            volume mask; can include wildcards, etc. to specify
                            the list
    
        -prefix      PPP   :directory to contain the output files: *cmd.txt and 
                            surface files such as *.gii and *.niml.dset; the
                            namebase of files within this directory will be the 
                            default for the program, "wmc".  The value PPP
                            can contain parts of a path in it. 
             or
        -prefix_file FFF   :prefix for the output files: *cmd.txt and surface
                            files such as *.gii and *.niml.dset; can include
                            path steps; and can make one level of a new directory.
                            For example, if FFF were "A/B", then the program
                            could make a new directory called "A" if it didn't
                            exist already and populate it with individual files
                            having the same prefix "B".
    
        -tsmoo_kpb   KPB   :"KPB" parameter in IsoSurface program;  default
                            value is 0.01.
        -tsmoo_niter NITER :"NITER" parameter in IsoSurface program;  default
                            value is 6.
        -iso_opt   ISO_OPT :input one of the "iso* options" from IsoSurface 
                            program, such as "isorois+dsets", "mergerois", etc. 
                            Quotations around the entry may be needed, esp
                            if something like the "-mergerois [LAB_OUT]" route
                            is being followed.
                            Default: isorois+dsets
    
        -trackid_no_or     :use this option to have the program recognize the 
                            naming convention of 3dTrackID output and to ignore
                            the OR-logic ROIs, including only the AND-logic (AKA
                            pairwise) connections.  This is mainly useful when 
                            wildcard expressions are using for '-in_rois NETROIS'.
    
        -output_tcat       :flag to output the multibrick file of concatenated
                            ROI masks; note that the [0]th brick will be all
                            zeros (it is just a place holder).  So, if there are
                            N ROI maps concatenated, there will be N+1 bricks
                            in the output dset, which has name PPP_tcat.nii.gz.
        -output_tstat      :flag to output the single brick file from the 3dTstat
                            operation on the tcat dset.  If there were N ROI maps
                            concatenated, then the largest value should be N.
                            The output file's name will be PPP_tstat.nii.gz.
    
        -wdir    WWW       :"__WDIR_connec_vis_PPP", where PPP is the input 
                            prefix.
        -no_clean          :is an optional switch to NOT remove working 
                            directory WWW; (default: remove working dir).
    
    # -----------------------------------------------------------------------
    
      EXAMPLE
    
        fat_proc_connec_vis \
            -in_rois o.prob/NET*       \
            -prefix surf_prob          \
            -trackid_no_or     
    
    # -----------------------------------------------------------------------
