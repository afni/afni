******************
fat_proc_dwi_to_dt
******************

.. _fat_proc_dwi_to_dt:

.. contents:: 
    :depth: 4 

.. code-block:: none

    -------------------------------------------------------------------------
    
     This program is for doing tensor and DT parameter fitting, as well as
     the uncertainty of DT parameters that are needed for tractography.
    
      Ver. 2.5b (PA Taylor, Sep 06, 2017)
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      This script has two *required* arguments ('-in_dwi ...' and some
      kind of gradient/matrix file input.  
    
      The rest are optional, but it is highly recommended to input a
      reference data set ('-in_ref ...')  if you have used a processing
      tool that resets origin+orientation (such as TORTOISE), as well as
      using '-scale_out_1000' to make the output units of the physical DT
      measures nicer.
    
      fat_proc_dwi_to_dt  \
       -in_dwi       DWI                              \
       {-in_col_matA | -in_col_matT |                 \
        -in_col_vec | -in_row_vec} GRADMAT            \
       -prefix        PPP                             \
       {-in_bvals     BVAL}                           \
       {-mask         MASK}                           \
       {-mask_from_struc}                             \
       {-in_struc_res STRUC}                          \
       {-in_ref_orig  REF}                            \
       {-prefix_dti   PREFIX_D}                       \
       {-flip_x | -flip_y | -flip_z | -no_flip}       \
       {-no_scale_out_1000}                           \
       {-no_reweight}                                 \
       {-no_cumulative_wts}                           \
       {-qc_prefix    QCPREF}                         \
       {-qc_fa_thr    TTT}                            \
       {-qc_fa_max    MMM}                            \
       {-qc_fa_unc_max UM}                            \
       {-qc_v12_unc_max V}                            \
       {-no_qc_view}                                  \
       {-no_cmd_out}                                  \
       {-workdir WWW}                                 \
       {-no_clean}                                    \
       {-uncert_off}                                  \
       {-uncert_iters NN}                             \
       {-uncert_extra_cmds STR}
    
      where:
    
        -in_dwi  DWI     :4D volume of N DWIs. Required.
    
        -in_col_matA | 
        -in_col_matT | 
        -in_col_vec  | 
        -in_row_vec  GRADMAT
                         :input text file of N gradient vectors or 
                          bmatrices. By default, it is assumed that
                          these still have physical units in them (or that
                          there is an accompanying BVAL file input), so
                          scaling physical values by 1000 is on by default;
                          see turning this scaling off, if unnecessary, by 
                          using '-no_scale_out_1000', below.
    
        -prefix   PPP    :set prefix for output DWI data; required.
    
        -in_bvals   BVAL :optional, if bvalue information is
                          in a separate file from the b-vectors
                          or matrices; should have same number N as
                          volumes and vectors/matrices.
        -flip_x | 
        -flip_y | 
        -flip_z | 
        -no_flip         :can flip the DW grads, if needed; for example, 
                          based on the recommendation of @GradFlipTest.
    
       -mask    MASK     :optional whole brain mask can be input;
                          otherwise, automasking is performed for the 
                          region to be tensor and parameter fit.
       -mask_from_struc  :flag to make a mask using 3dSkullStrip+3dmask_tool
                          from the STRUC file.
            NB ---> If no "-mask*" option is given, then 3dAutomask is run on 
                    the DWI set.  This often ain't great, so if TORTOISE isn't
                    producing a mask, 1) email Okan and ask him about that, and
                    2) try '-mask_from_struc'.
                    ALSO, if you want the whole volume to be estimated
                    tensorially for some reason, then make a volume fully
                    filled with 1s and pass that in as the MASK, et voila
                    (but then calcs will likely be slooow).
    
       -in_ref_orig REF  :use another data set to adjust the DWI (and
                          subsequent parameter) dsets' orientation and
                          origin; for example, TORTOISE has default 
                          orientation and origin for all output DWIs-- it
                          would be very advisable to use the anatomical
                          volume that you had input into TORTOISE as REF,
                          so that the DWIs should be viewable overlaying
                          it afterwards; if an ANAT (below) that has been 
                          merely resampled is *not* used, then you really, 
                          really want REF to have the same contrast as the
                          b=0 DWI volume. *Highly recommended to include!*
     -in_struc_res STRUC :accomplish the alignment of the output DWI to the 
                          REF data set via ANAT: a version of the anatomical 
                          that has been resampled to match the DWI set (in 
                          both orientation and origin);  for example, in
                          TORTOISE there is a 'structural.nii' file that should
                          match this description.  Both ANAT and DWI should 
                          then be well aligned to the original REF (and to  
                          each other). *Highly recommended to include!*
             
     -prefix_dti PREFIX2 :set prefix for output DTI data; optional, 
                          default is 'dt'.
    
      -no_scale_out_1000 :by default, for tensor fitting it is assumed
                          that 1) the DW b-value information is included
                          in the gradient vectors or grads, and 2) you are
                          happy to have tiny numbers of physical
                          diffusion, which in standard units are like
                          MD~0.001 "mm^2/s", scaled by 1000 so that they
                          are returned as MD~1 "10^{-3} mm^2/s".  Isn't
                          that nicer?  I thought you'd agree-- therefore,
                          such a kind of scaling is *on* by default.  To
                          turn that *off*, use this option flag.
                          See the 3dDWItoDT help file for what this
                          entails.  Basically, you will likely have nicer
                          numeric values (from scaling physical length
                          units by 1000); otherwise, you might have small
                          numerical values leading to issues with
                          statistical modeling.
    
       -no_reweight      :by default, we *do* reweight+refit tensors during 
                          estimation; should improve fit.  But what do I
                          know?  This option turns that functionality *off*.
       -no_cumulative_wts:by default, show  overall weight factors for each 
                          gradient; may be useful as a quality control, but 
                          this option will turn that functionality *off*.
    
       -qc_fa_thr TTT    :set threshold for overlay FA volume in QC image
                          (default:  TTT=0.2, as for healthy adult human 
                          parenchyma).
       -qc_fa_max MMM    :set cbar max for overlay FA volume in QC image
                          (default:  MMM=0.9, a very large value even for 
                          healthy adult human parenchyma).
       -qc_fa_unc_max UM :set cbar max for overlay uncert (stdev) of FA 
                          in QC image (default:  UM=0.05).
       -qc_v12_unc_max V :set cbar max for overlay uncert (stdev) of V1 
                          towards the V2 direction for DTs, in QC image
                          (default:  UM=0.349 rads, which corresponds to  
                          20 deg).
    
       -qc_prefix QCPREF :can set the prefix of the QC image files separately
                          (default is '').
       -no_qc_view       :can turn off generating QC image files (why?)
       -no_cmd_out       :don't save the command line call of this program
                          and the location where it was run (otherwise, it is
                          saved by default in the ODIR/).
    
       -no_clean         :is an optional switch to NOT remove working 
                          directory '__WORKING_dwi_to_dt'; (default: remove working dir).
       -workdir WWW      :specify a working directory, which can be removed;
                          (default name = '__WORKING_dwi_to_dt').
    
       -uncert_off       :don't do uncertainty calc (default is to do so); 
                          perhaps if it is slow or you want *very* different
                          options.
       -uncert_iters NN  :set the number of Monte Carlo iterations for the
                          uncertainty calc (default NN=300).
    -uncert_extra_cmds STR:put in extra commands for the uncertainty calcs
                          (see the 3dDWUncert helpfile for more opts).
    
    # -----------------------------------------------------------------------
    
      EXAMPLE
    
        fat_proc_dwi_to_dt \
            -in_dwi       DWI.nii                \
            -in_col_matA  BMTXT_AFNI.txt         \
            -in_struc_res ../structural.nii      \
            -in_ref_orig  t2w.nii                \
            -mask         mask_DWI.nii.gz        \
            -prefix       OUTPUT/dwi
    
        or
    
        fat_proc_dwi_to_dt \
            -in_dwi        ap_proc_DRBUDDI_final.nii    \
            -in_col_matT   ap_proc_DRBUDDI_final.bmtxt  \
            -in_struc_res  structural.nii               \
            -in_ref_orig   t2w.nii                      \
            -mask_from_struc                            \
            -prefix        dwi_03/dwi
    
    
    -------------------------------------------------------------------------
