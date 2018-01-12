.. contents:: 
    :depth: 4 

*******************
fat_proc_connec_vis
*******************

.. code-block:: none

    # -----------------------------------------------------------------------
    
     This program is for visualizing the volumetric output of tracking,
     mostly for the '-dump_rois ...' from 3dTrackID.  These are basically
     the WMC (white matter connection) maps through which tract bundles run
     (for DET and MINIP tracking modes), or through which a suprathreshold
     number of tracts run in PROB mode.
    
     This program creates surface-ized views of the separate WMCs which can
     be viewed simultaneously in 3D with SUMA. 
    
      Ver. 1.21 (PA Taylor, Oct 25, 2017)
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      fat_proc_connec_vis  \
        -in_rois       NETROIS       \
        -prefix        PPP           \
       {-tsmoo_kpb     KPB}          \
       {-tsmoo_niter   NITER}        \
       {-iso_opt       ISO_OPT}      \
       {-wdir          WWW}          \
       {-trackid_no_or}              \
       {-no_clean}
    
    
      where
    
        -in_rois NETROIS   :list of separate files, each with single ROI
                            volume mask; can include wildcards, etc. to specify
                            the list
        -prefix      PPP   :output prefix for the outputs: *cmd.txt and surface
                            files such as *.gii and *.niml.dset
    
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
