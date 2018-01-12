.. contents:: 
    :depth: 4 

****************
@simulate_motion
****************

.. code-block:: none

    ---------------------------------------------------------------------------
    @simulate_motion          - create simulated motion time series
    
       This program is meant to simulate motion in an EPI time series based only
       on the motion parameters and an input volume.
    
       The main action is to take the EPI (motion base) volume and (inverse) warp
       it according to the motion parameters.  In theory, the result could be run
       through 3dvolreg to generate a similar set of motion parameters.
    
       Note: if slice timing is provided (via the -epi or -epi_timing datasets),
             then slices will be generated individually at the interpolated offset
             into each TR.
    
       An "aligned" result could then be computed via -warp_method and related
       options.  Methods include:
    
            VOLREG:             run 3dvolreg on result
            VR_PARAMS:          apply the motion parameters, instead
            VOLREG_AND_WARP:    3dvolreg, then combine the transformations with
                                anat alignment and standard space transformation
            VR_PARAMS_AND_WARP: catenate volreg params with affine transformation
            WARP:               re-apply complete motion param/anat align/standard
                                space transformations
    
       How to use the result:
    
            The resulting time series can be used to create regressors of no
            interest, when trying to regress out motion artifacts (from either
            task or resting state analysis).  Ways it can be used:
    
            a. Grab the first N (e.g. 6) principle components, and use them along
               with other motion parameters.  To do this, just run 3dpc with the
               simulated time series and an appropriate mask.
    
            b. First make the time series orthogonal to the motion parameters, and
               only then take the first N principle components.  For example, run
               3dDeconvolve to remove the original motion parameters, and use the
               resulting errts dataset as input to 3dpc.
    
            c. Do voxel-wise regression with single, blurred or locally averaged
               time series via 3dTfitter.
    
               Note that if censoring is being done, such TRs would have to be
               removed, as 3dTfitter does not have a -censor option.
    
                 i) extract usable TRs with '1d_tool.py -show_trs_uncensored ...'
                ii) pass the X-matrix and extracted series to 3dTfitter
    
    
            Eventually these methods can be put into afni_proc.py.  Please pester
            Rick if you have interest in any method that has not been implemented.
    
    
    usage: @simulate_motion [options] -epi EPI_DSET -motion_file MOTION_PARAMS
    
          needed inputs: EPI volume, motion parameters
          output:        motion simulated EPI time series
    
    examples:
    
      1a. process in orig space, no slice timing
    
        Create a time series that has motion similar to what would include
        the given motion_file.
    
        @simulate_motion -epi pb01.FT.r01.tshift+orig"[2]" -prefix msim.1a \
                         -motion_file dfile_rall.1D
    
      1b. process in orig space, with slice timing
    
        @simulate_motion -epi pb01.FT.r01.tshift+orig"[2]" -prefix msim.1b \
                         -motion_file dfile_rall.1D                        \
                         -epi_timing pb00.FT.r01.tcat+orig
    
      1c. use post-tlrc volreg base (slice timing not an option, of course)
    
        @simulate_motion -epi pb02.FT.r01.volreg+tlrc"[2]" -prefix msim.1c \
                         -motion_file dfile_rall.1D                        \
    
    examples with -warp_method ...
    
      2. apply 3dvolreg to realign the time series
    
        Note that running 3dvolreg should produce a 1Dfile that is similar to the
        input motion file.
    
        @simulate_motion -epi pb01.FT.r01.tshift+orig"[2]" -prefix msim.2.vr \
                         -motion_file dfile_rall.1D                          \
                         -warp_method VOLREG
    
      3. instead of re-running 3dvolreg, apply the original volreg params
    
        Note that running 3dvolreg should procude a 1Dfile that is similar to the
        input motion file.
    
        @simulate_motion -epi pb01.FT.r01.tshift+orig"[2]" -prefix msim.3.vrp \
                         -motion_file dfile_rall.1D                           \
                         -warp_method VR_PARAMS
    
      4. like #2, but include a transformation that would align to the anatomy
         and warp to standard space
    
        The additional -warp_1D option requires a corresponding -warp_master for
        the resulting grid.
    
        @simulate_motion -epi pb01.FT.r01.tshift+orig"[2]" -prefix msim.4.vrw \
                         -motion_file dfile_rall.1D                           \
                         -warp_method VOLREG_AND_WARP                         \
                         -warp_1D anat_std.aff12.1D                           \
                         -warp_master pb02.FT.r01.volreg+tlrc
    
      5. no 3dvolreg or params, those transformations are already in -warp_1D
         (such as that used in the end by afni_proc.py, if align and std space)
    
         Also, include -wsync5 interpolation.
    
        @simulate_motion -epi pb01.FT.r01.tshift+orig"[2]" -prefix msim.5.warp \
                         -motion_file dfile_rall.1D                            \
                         -warp_method WARP                                     \
                         -warp_1D mat_rall.warp.aff12.1D                       \
                         -warp_master pb02.FT.r01.volreg+tlrc                  \
                         -wsinc5
    
    
    informational options:
    
       -help                : show this help
       -hist                : show program modification history
       -todo                : show current todo list
       -ver                 : show program version
    
    required parameters:
    
       -epi EPI             : provide input volume or time series
                              (only a volreg base is needed, though more is okay)
    
                              If slice timing is to be used, the number of slices
                              must match that of the -epi_timing dataset.  So it
                              should not be the case where one view is +orig and
                              the other +tlrc, for example.
    
       -motion_file MOTFILE : specify motion parameter file (as output by 3dvolreg)
    
    options:
    
       -epi_timing DSET     : provide EPI dataset with slice timing
                              (maybe -epi no longer has slice times)
    
       -prefix PREFIX       : prefix for data results
                              (default = motion_sim.NUM_TRS)
    
       -save_workdir        : do not remove 'work' directory
    
       -test                : only test running the program, do not actually
                              create a simulated motion dataset
                              (not so useful anymore)
    
       -verb LEVEL          : specify a verbose level (default = 1)
    
       -vr_base INDEX       : 0-based index of volreg base in EPI dataset
    
       -warp_method METHOD  : specify a METHOD for forward alignment/transform
    
                                e.g. -warp_method WARP
                                default: NONE
    
            NONE:            do nothing after inverse motion
            VOLREG:          run 3dvolreg on result
            VR_PARAMS:       re-apply the motion parameters on the result
            VOLREG_AND_WARP: apply both VOLREG and WARP methods
                             Run 3dvolreg on result, then combine the registration
                             transformations with those of anat alignment and
                             standard space transformation.
                           * requires -warp_1D and -warp_master
            VR_PARAMS_AND_WARP: catenate volreg params with affine transformation
                             (such as aligning to anat and going to standard space)
            WARP:            re-apply the complete motion param/anat align/standard
                             space transformations
                           * requires -warp_1D and -warp_master
    
       -warp_1D             : specify a 12 parameter affine transformation,
                              presumably to go from orig space to standard space,
                              or including a volreg transformation
    
                                e.g. -warp_1D mat_rall.warp.aff12.1D
    
                              This command must be paired with -warp_master, and
                              requires -warp_method WARP or VOLREG_AND_WARP.
    
       -warp_master DSET    : specify a grid master dataset for the -warp_1D xform
    
                                e.g. -warp_master pb02.FT.r01.volreg+tlrc
    
                              This DSET should probably be one of the volreg+tlrc
                              results from an afni_proc.py script.
    
       -wsinc5              : use wsinc5 interpolation in 3dAllineate
    
    -------------------------------------------------------
    R Reynolds  May, 2013
