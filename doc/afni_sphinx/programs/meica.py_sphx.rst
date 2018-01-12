.. contents:: 
    :depth: 4 

********
meica.py
********

.. code-block:: none

    Usage: meica.py [options]
    
    Options:
      -h, --help            show this help message and exit
      -e TES                ex: -e 14.5,38.5,62.5  Echo times (in ms)
      -d DSINPUTS           ex: -d RESTe1.nii.gz,RESTe2.nii.gz,RESTe3.nii.gz
      -a ANAT               ex: -a mprage.nii.gz  Anatomical dataset (optional)
      -b BASETIME           ex: -b 10s OR -b 10v  Time to steady-state
                            equilibration in seconds(s) or volumes(v). Default 0.
      --MNI                 Warp to MNI space using high-resolution template
    
      Additional processing options:
        --qwarp             Nonlinear anatomical normalization to MNI (or --space
                            template) using 3dQWarp, after affine
        --fres=FRES         Specify functional voxel dim. in mm (iso.) for
                            resampling during preprocessing. Default none. ex:
                            --fres=2.5
        --space=SPACE       Path to specific standard space template for affine
                            anatomical normalization
        --no_skullstrip     Anatomical is already intensity-normalized and skull-
                            stripped (if -a provided)
        --no_despike        Do not de-spike functional data. Default is to de-
                            spike, recommended.
        --no_axialize       Do not re-write dataset in axial-first order. Default
                            is to axialize, recommended.
        --mask_mode=MASK_MODE
                            Mask functional with help from anatomical or standard
                            space images: use 'anat' or 'template'
        --coreg_mode=COREG_MODE
                            Coregistration with Local Pearson and T2* weights
                            (default), or use align_epi_anat.py (edge method): use
                            'lp-t2s' or 'aea'
        --smooth=FWHM       Data FWHM smoothing (3dBlurInMask). Default off. ex:
                            --smooth 3mm
        --align_base=ALIGN_BASE
                            Explicitly specify base dataset for volume
                            registration
        --TR=TR             The TR. Default read from input dataset header
        --tpattern=TPATTERN
                            Slice timing (i.e. alt+z, see 3dTshift -help). Default
                            from header. (N.B. This is important!)
        --align_args=ALIGN_ARGS
                            Additional arguments to anatomical-functional co-
                            registration routine
        --ted_args=TED_ARGS
                            Additional arguments to TE-dependence analysis routine
    
      Run optipns:
        --prefix=PREFIX     Prefix for final ME-ICA output datasets.
        --cpus=CPUS         Maximum number of CPUs (OpenMP threads) to use.
                            Default 2.
        --label=LABEL       Label to tag ME-ICA analysis folder.
        --test_proc         Align and preprocess 1 dataset then exit, for testing
        --script_only       Generate script only, then exit
        --pp_only           Preprocess only, then exit.
        --keep_int          Keep preprocessing intermediates. Default delete.
        --skip_check        Skip dependency checks during initialization.
