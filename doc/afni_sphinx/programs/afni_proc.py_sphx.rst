.. contents:: 
    :depth: 4 

************
afni_proc.py
************

    
        afni_proc.py        - generate a tcsh script for an AFNI process stream
    

Purpose:
========

.. code-block:: none

    
           This program is meant to create single subject processing scripts for
           task, resting state or surface-based analyses.  The processing scripts
           are written in the tcsh language.
    
           The typical goal is to create volumes of aligned resopnse magnitudes
           (stimulus beta weights) to use as input for a group analysis.
    

Inputs (only EPI is required):
==============================

.. code-block:: none

    
           - anatomical dataset
           - EPI time series datasets
           - stimulus timing files
           - processing and design decisions:
               e.g. TRs to delete, blur size, censoring options, basis functions
    

Main outputs (many datasets are created):
=========================================

.. code-block:: none

    
           - for task-based analysis: stats dataset (and anat_final)
           - for resting-state analysis: errts datasets ("cleaned up" EPI)
    

Basic script outline:
=====================

.. code-block:: none

    
           - copy all inputs to new 'results' directory
           - process data: e.g. despike,tshift/align/tlrc/volreg/blur/scale/regress
           - leave all (well, most) results there, so user can review processing
           - create @ss_review scripts to help user with basic quality control
    
        The exact processing steps are controlled by the user, including which main
        processing blocks to use, and their order.  See the 'DEFAULTS' section for
        a description of the default options for each block.
    
        The output script (when executed) would create a results directory, copy
        input files into it, and perform all processing there.  So the user can
        delete the results directory and modify/re-run the script at their whim.
    
        Note that the user need not actually run the output script.  The user
        should feel free to modify the script for their own evil purposes, or to
        just compare the processing steps with those in their own scripts.  Also,
        even if a user is writing their own processing scripts, it is a good idea
        to get some independent confirmation of the processing, such as by using
        afni_proc.py to compare the results on occasion.
    
        The text interface can be accessed via the -ask_me option.  It invokes a
        question & answer session, during which this program sets user options on
        the fly.  The user may elect to enter some of the options on the command
        line, even if using -ask_me.  See "-ask_me EXAMPLES", below.
    
        ** However, -ask_me has not been touched in many years.  I suggest starting
           with the 'modern' examples (for task/rest/surface), or by using the
           uber_subject.py GUI (graphical user interface) to generate an initial
           afni_proc.py command script.
    
           See uber_subject.py -help (or just start the GUI) for details.
    

SECTIONS: order of sections in the "afni_proc.py -help" output
==============================================================

.. code-block:: none

    
            program introduction    : (above) basic overview of afni_proc.py
            PROCESSING BLOCKS       : list of possible processing blocks
            DEFAULTS                : basic default operations, per block
            EXAMPLES                : various examples of running this program
            NOTE sections           : details on various topics
                GENERAL ANALYSIS NOTE, RESTING STATE NOTE, FREESURFER NOTE,
                TIMING FILE NOTE, MASKING NOTE,
                ANAT/EPI ALIGNMENT CASES NOTE, ANAT/EPI ALIGNMENT CORRECTIONS NOTE,
                WARP TO TLRC NOTE,
                RETROICOR NOTE, RUNS OF DIFFERENT LENGTHS NOTE,
                SCRIPT EXECUTION NOTE
            OPTIONS                 : desriptions of all program options
                informational       : options to get quick info and quit
                general execution   : options not specific to a processing block
                block options       : specific to blocks, in default block order
    

PROCESSING BLOCKS (of the output script):
=========================================

.. code-block:: none

    
        The output script will go through the following steps, unless the user
        specifies otherwise.
    

automatic blocks (the tcsh script will always perform these):
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
            setup       : check subject arg, set run list, create output dir, and
                          copy stim files
            tcat        : copy input datasets and remove unwanted initial TRs
    

default blocks (the user may skip these, or alter their order):
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
            tshift      : slice timing alignment on volumes (default is -time 0)
            volreg      : volume registration (default to third volume)
            blur        : blur each volume (default is 4mm fwhm)
            mask        : create a 'brain' mask from the EPI data (dilate 1 voxel)
            scale       : scale each run mean to 100, for each voxel (max of 200)
            regress     : regression analysis (default is GAM, peak 1, with motion
                          params)
    

optional blocks (the default is to _not_ apply these blocks)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
            align       : align EPI anat anatomy (via align_epi_anat.py)
            despike     : truncate spikes in each voxel's time series
            empty       : placeholder for some user command (uses 3dTcat as sample)
            ricor       : RETROICOR - removal of cardiac/respiratory regressors
            tlrc        : warp anat to standard space
    

DEFAULTS: basic defaults for each block (blocks listed in default order)
========================================================================

.. code-block:: none

    
            A : denotes automatic block that is not a 'processing' option
            D : denotes a default processing block (others must be requested)
    
        A   setup:    - use 'SUBJ' for the subject id
                            (option: -subj_id SUBJ)
                      - create a t-shell script called 'proc_subj'
                            (option: -script proc_subj)
                      - use results directory 'SUBJ.results'
                            (option: -out_dir SUBJ.results)
    
        A   tcat:     - do not remove any of the first TRs
    
            despike:  - NOTE: by default, this block is _not_ used
                      - automasking is not done (requires -despike_mask)
    
            ricor:    - NOTE: by default, this block is _not_ used
                      - polort based on twice the actual run length
                      - solver is OLSQ, not REML
                      - do not remove any first TRs from the regressors
    
        D   tshift:   - align slices to the beginning of the TR
                      - use quintic interpolation for time series resampling
                            (option: -tshift_interp -quintic)
    
            align:    - align the anatomy to match the EPI
                        (also required for the option of aligning EPI to anat)
    
            tlrc:     - use TT_N27+tlrc as the base (-tlrc_base TT_N27+tlrc)
                      - no additional suffix (-tlrc_suffix NONE)
                      - use affine registration (no -tlrc_NL_warp)
    
        D   volreg:   - align to third volume of first run, -zpad 1
                            (option: -volreg_align_to third)
                            (option: -volreg_zpad 1)
                      - use cubic interpolation for volume resampling
                            (option: -volreg_interp -cubic)
                      - apply motion params as regressors across all runs at once
                      - do not align EPI to anat
                      - do not warp to standard space
    
        D   blur:     - blur data using a 4 mm FWHM filter with 3dmerge
                            (option: -blur_filter -1blur_fwhm)
                            (option: -blur_size 4)
                            (option: -blur_in_mask no)
    
        D   mask:     - create a union of masks from 3dAutomask on each run
                      - not applied in regression without -regress_apply_mask
                      - if possible, create a subject anatomy mask
                      - if possible, create a group anatomy mask (tlrc base)
    
        D   scale:    - scale each voxel to mean of 100, clip values at 200
    
        D   regress:  - use GAM regressor for each stim
                            (option: -regress_basis)
                      - compute the baseline polynomial degree, based on run length
                            (e.g. option: -regress_polort 2)
                      - do not censor large motion
                      - output fit time series
                      - output ideal curves for GAM/BLOCK regressors
                      - output iresp curves for non-GAM/non-BLOCK regressors
    
            empty:    - do nothing (just copy the data using 3dTcat)
    

EXAMPLES (options can be provided in any order):
================================================

.. code-block:: none

    

Example 1. Minimum use.
+++++++++++++++++++++++

.. code-block:: none

    
               Provide datasets and stim files (or stim_times files).  Note that a
               dataset suffix (e.g. HEAD) must be used with wildcards, so that
               datasets are not applied twice.  In this case, a stim_file with many
               columns is given, where the script to changes it to stim_times files.
    
                    afni_proc.py -dsets epiRT*.HEAD              \
                                 -regress_stim_files stims.1D
    
               or without any wildcard, the .HEAD suffix is not needed:
    
                    afni_proc.py -dsets epiRT_r1+orig epiRT_r2+orig epiRT_r3+orig \
                                 -regress_stim_files stims.1D
    
         *  New and improved!  Examples that apply to AFNI_data4.     *
         *  (were quickly OLD and OBSOLETE, as we now use AFNI_data6) *
    
            The following examples can be run from the AFNI_data4 directory, and
            are examples of how one might process the data for subject sb23.
    

Example 2. Very simple.  Use all defaults, except remove 3 TRs and use
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

               basis function BLOCK(30,1).  The default basis function is GAM.
    
                    afni_proc.py -subj_id sb23.e2.simple                       \
                            -dsets sb23/epi_r??+orig.HEAD                      \
                            -tcat_remove_first_trs 3                           \
                            -regress_stim_times sb23/stim_files/blk_times.*.1D \
                            -regress_basis 'BLOCK(30,1)'
    

Example 3. The current class example.  This may change of course.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               Copy the anatomy into the results directory, register EPI data to
               the last TR, specify stimulus labels, compute blur estimates, and
               provide GLT options directly to 3dDeconvolve.  The GLTs will be
               ignored after this, as they take up too many lines.
    
                    afni_proc.py -subj_id sb23.blk                             \
                            -dsets sb23/epi_r??+orig.HEAD                      \
                            -copy_anat sb23/sb23_mpra+orig                     \
                            -tcat_remove_first_trs 3                           \
                            -volreg_align_to last                              \
                            -regress_stim_times sb23/stim_files/blk_times.*.1D \
                            -regress_stim_labels tneg tpos tneu eneg epos      \
                                                 eneu fneg fpos fneu           \
                            -regress_basis 'BLOCK(30,1)'                       \
                            -regress_opts_3dD                                  \
                                -gltsym 'SYM: +eneg -fneg'                     \
                                -glt_label 1 eneg_vs_fneg                      \
                                -gltsym 'SYM: 0.5*fneg 0.5*fpos -1.0*fneu'     \
                                -glt_label 2 face_contrast                     \
                                -gltsym 'SYM: tpos epos fpos -tneg -eneg -fneg'\
                                -glt_label 3 pos_vs_neg                        \
                            -regress_est_blur_epits                            \
                            -regress_est_blur_errts
    

Example 4. Similar to the class example, but specify the processing
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

               blocks, adding despike and tlrc, and removing tshift.  Note that
               the tlrc block is to run @auto_tlrc on the anat.  Ignore the GLTs.
    
                    afni_proc.py -subj_id sb23.e4.blocks                       \
                            -dsets sb23/epi_r??+orig.HEAD                      \
                            -blocks despike volreg blur mask scale regress tlrc\
                            -copy_anat sb23/sb23_mpra+orig                     \
                            -tcat_remove_first_trs 3                           \
                            -regress_stim_times sb23/stim_files/blk_times.*.1D \
                            -regress_stim_labels tneg tpos tneu eneg epos      \
                                                 eneu fneg fpos fneu           \
                            -regress_basis 'BLOCK(30,1)'                       \
                            -regress_est_blur_epits                            \
                            -regress_est_blur_errts
    

Example 5a. RETROICOR example a, resting state data.
++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               Assuming the class data is for resting-state and that we have the
               appropriate slice-based regressors from RetroTS.py, apply the
               despike and ricor processing blocks.  Note that '-do_block' is used
               to add non-default blocks into their default positions.  Here the
               'despike' and 'ricor' processing blocks would come before 'tshift'.
    
               Remove 3 TRs from the ricor regressors to match the EPI data.  Also,
               since degrees of freedom are not such a worry, regress the motion
               parameters per-run (each run gets a separate set of 6 regressors).
    
               The regression will use 81 basic regressors (all of "no interest"),
               with 13 retroicor regressors being removed during pre-processing:
    
                     27 baseline  regressors ( 3 per run * 9 runs)
                     54 motion    regressors ( 6 per run * 9 runs)
    
               To example #3, add -do_block, -ricor_* and -regress_motion_per_run.
    
                    afni_proc.py -subj_id sb23.e5a.ricor            \
                            -dsets sb23/epi_r??+orig.HEAD           \
                            -do_block despike ricor                 \
                            -tcat_remove_first_trs 3                \
                            -ricor_regs_nfirst 3                    \
                            -ricor_regs sb23/RICOR/r*.slibase.1D    \
                            -regress_motion_per_run
    
               If tshift, blurring and masking are not desired, consider replacing
               the -do_block option with an explicit list of blocks:
    
                    -blocks despike ricor volreg regress
    

Example 5b. RETROICOR example b, while running a normal regression.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               Add the ricor regressors to a normal regression-based processing
               stream.  Apply the RETROICOR regressors across runs (so using 13
               concatenated regressors, not 13*9).  Note that concatenation is
               normally done with the motion regressors too.
    
               To example #3, add -do_block and three -ricor options.
    
                    afni_proc.py -subj_id sb23.e5b.ricor                       \
                            -dsets sb23/epi_r??+orig.HEAD                      \
                            -do_block despike ricor                            \
                            -copy_anat sb23/sb23_mpra+orig                     \
                            -tcat_remove_first_trs 3                           \
                            -ricor_regs_nfirst 3                               \
                            -ricor_regs sb23/RICOR/r*.slibase.1D               \
                            -ricor_regress_method 'across-runs'                \
                            -volreg_align_to last                              \
                            -regress_stim_times sb23/stim_files/blk_times.*.1D \
                            -regress_stim_labels tneg tpos tneu eneg epos      \
                                                 eneu fneg fpos fneu           \
                            -regress_basis 'BLOCK(30,1)'                       \
                            -regress_est_blur_epits                            \
                            -regress_est_blur_errts
    
               Also consider adding -regress_bandpass.
    

Example 5c. RETROICOR example c (modern): with censoring and bandpass
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

               filtering.
    
               This is an example of how we might currently suggest analyzing
               resting state data.  If no RICOR regressors exist, see example 9
               (or just remove any ricor options).
    
               Censoring due to motion has long been considered appropriate in
               BOLD FMRI analysis, but is less common for those doing bandpass
               filtering in RS FMRI because the FFT requires one to either break
               the time axis (evil) or to replace the censored data with something
               probably inapproprate.
    
               Instead, it is slow (no FFT, but maybe SFT :) but effective to
               regress frequencies within the regression model, where censoring
               is simple.
    
               Note: bandpassing in the face of RETROICOR processing is questionable.
                     To skip bandpassing, remove the -regress_bandpass option line.
    
               Also, align EPI to anat and warp to standard space.
    
                    afni_proc.py -subj_id sb23.e5a.ricor            \
                            -dsets sb23/epi_r??+orig.HEAD           \
                            -blocks despike ricor tshift align tlrc \
                                    volreg blur mask regress        \
                            -copy_anat sb23/sb23_mpra+orig          \
                            -tcat_remove_first_trs 3                \
                            -ricor_regs_nfirst 3                    \
                            -ricor_regs sb23/RICOR/r*.slibase.1D    \
                            -volreg_align_e2a                       \
                            -volreg_tlrc_warp                       \
                            -blur_size 6                            \
                            -regress_motion_per_run                 \
                            -regress_censor_motion 0.2              \
                            -regress_bandpass 0.01 0.1              \
                            -regress_apply_mot_types demean deriv   \
                            -regress_run_clustsim no                \
                            -regress_est_blur_epits                 \
                            -regress_est_blur_errts
    

Example 6. A modern example.  GOOD TO CONSIDER.
+++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               Align the EPI to the anatomy.  Also, process in standard space.
    
               For alignment in either direction, add the 'align' block, which
               aligns the anatomy to the EPI.  To then align the EPI to the anat
               using the lpc+ZZ cost function (instead of just lpc), apply
               -volreg_align_e2a, where that transform (inverse) is applied along
               with the motion alignment.
    
               On top of that, complete the processing in standard space by running
               @auto_tlrc on the anat (via the 'tlrc' block) and applying the same
               transformation to the EPI via -volreg_tlrc_warp.  Again, the EPI
               transformation is applied along with the motion alignment.
    
               So add the 2 processing blocks and 2 extra volreg warps to #3 via
               '-do_block align tlrc', '-volreg_align_e2a', '-volreg_tlrc_warp'.
    
               As an added bonus, censor TR pairs where the Euclidean Norm of the
               motion derivative exceeds 1.0.  Also, regress motion parameters
               separately for each run.
    
                    afni_proc.py -subj_id sb23.e6.align                        \
                            -dsets sb23/epi_r??+orig.HEAD                      \
                            -do_block align tlrc                               \
                            -copy_anat sb23/sb23_mpra+orig                     \
                            -tcat_remove_first_trs 3                           \
                            -align_opts_aea -cost lpc+ZZ                       \
                            -volreg_align_to last                              \
                            -volreg_align_e2a                                  \
                            -volreg_tlrc_warp                                  \
                            -regress_stim_times sb23/stim_files/blk_times.*.1D \
                            -regress_stim_labels tneg tpos tneu eneg epos      \
                                                 eneu fneg fpos fneu           \
                            -regress_basis 'BLOCK(30,1)'                       \
                            -regress_motion_per_run                            \
                            -regress_censor_motion 0.3                         \
                            -regress_opts_3dD                                  \
                                -gltsym 'SYM: +eneg -fneg'                     \
                                -glt_label 1 eneg_vs_fneg                      \
                            -regress_est_blur_epits                            \
                            -regress_est_blur_errts
    
               To process in orig space, remove -volreg_tlrc_warp.
               To apply manual tlrc transformation, use -volreg_tlrc_adwarp.
               To process as anat aligned to EPI, remove -volreg_align_e2a.
    
             * Also, consider '-volreg_align_to MIN_OUTLIER', to use the volume
               with the minimum outlier fraction as the registration base.
    
             * Also, one can use ANATICOR with task (-regress_anaticor_fast, say)
               in the case of -reml_exec.
    

Example 7. Similar to 6, but get a little more esoteric.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               a. Register EPI volumes to the one which has the minimum outlier
                  fraction (so hopefully the least motion), still with cost lpc+ZZ.
    
               b. Blur only within the brain, as far as an automask can tell.  So
                  add -blur_in_automask to blur only within an automatic mask
                  created internally by 3dBlurInMask (akin to 3dAutomask).
    
               c. Let the basis functions vary.  For some reason, we expect the
                  BOLD responses to the telephone classes to vary across the brain.
                  So we have decided to use TENT functions there.  Since the TR is
                  3.0s and we might expect up to a 45 second BOLD response curve,
                  use 'TENT(0,45,16)' for those first 3 out of 9 basis functions.
    
                  This means using -regress_basis_multi instead of -regress_basis,
                  and specifying all 9 basis functions appropriately.
    
               d. Use amplitude modulation.
    
                  We expect responses to email stimuli to vary proportionally with
                  the number of punctuation characters used in the message (in
                  certain brain regions).  So we will use those values as auxiliary
                  parameters 3dDeconvolve by marrying the parameters to the stim
                  times (using 1dMarry).
    
                  Use -regress_stim_types to specify that the epos/eneg/eneu stim
                  classes should be passed to 3dDeconvolve using -stim_times_AM2.
    
               e. Not only censor motion, but censor TRs when more than 10% of the
                  automasked brain are outliers.  So add -regress_censor_outliers.
    
               f. Include both de-meaned and derivatives of motion parameters in
                  the regression.  So add '-regress_apply_mot_types demean deriv'.
    
               g. Output baseline parameters so we can see the effect of motion.
                  So add -bout under option -regress_opts_3dD.
    
               h. Save on RAM by computing the fitts only after 3dDeconvolve.
                  So add -regress_compute_fitts.
    
               i. Speed things up.  Have 3dDeconvolve use 4 CPUs and skip the
                  single subject 3dClustSim execution.  So add '-jobs 4' to the
                  -regress_opts_3dD option and add '-regress_run_clustsim no'.
    
                    afni_proc.py -subj_id sb23.e7.esoteric                     \
                            -dsets sb23/epi_r??+orig.HEAD                      \
                            -do_block align tlrc                               \
                            -copy_anat sb23/sb23_mpra+orig                     \
                            -tcat_remove_first_trs 3                           \
                            -align_opts_aea -cost lpc+ZZ                       \
                            -volreg_align_to MIN_OUTLIER                       \
                            -volreg_align_e2a                                  \
                            -volreg_tlrc_warp                                  \
                            -blur_in_automask                                  \
                            -regress_stim_times sb23/stim_files/blk_times.*.1D \
                            -regress_stim_types times times times              \
                                                AM2   AM2   AM2                \
                                                times times times              \
                            -regress_stim_labels tneg tpos tneu                \
                                                 eneg epos eneu                \
                                                 fneg fpos fneu                \
                            -regress_basis_multi                               \
                               'BLOCK(30,1)' 'TENT(0,45,16)' 'BLOCK(30,1)'     \
                               'BLOCK(30,1)' 'TENT(0,45,16)' 'BLOCK(30,1)'     \
                               'BLOCK(30,1)' 'TENT(0,45,16)' 'BLOCK(30,1)'     \
                            -regress_apply_mot_types demean deriv              \
                            -regress_motion_per_run                            \
                            -regress_censor_motion 0.3                         \
                            -regress_censor_outliers 0.1                       \
                            -regress_compute_fitts                             \
                            -regress_opts_3dD                                  \
                                -bout                                          \
                                -gltsym 'SYM: +eneg -fneg'                     \
                                -glt_label 1 eneg_vs_fneg                      \
                                -jobs 4                                        \
                            -regress_run_clustsim no                           \
                            -regress_est_blur_epits                            \
                            -regress_est_blur_errts
    

Example 8. Surface-based analysis.
++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               This example is intended to be run from AFNI_data6/FT_analysis.
               It is provided with the class data in file s03.ap.surface.
    
               Add -surf_spec and -surf_anat to provide the required spec and
               surface volume datasets.  The surface volume will be aligned to
               the current anatomy in the processing script.  Two spec files
               (lh and rh) are provided, one for each hemisphere (via wildcard).
    
               Also, specify a (resulting) 6 mm FWHM blur via -blur_size.  This
               does not add a blur, but specifies a resulting blur level.  So
               6 mm can be given directly for correction for multiple comparisons
               on the surface.
    
               Censor per-TR motion above 0.3 mm.
    
               Note that no -regress_est_blur_errts option is given, since that
               applies to the volume only (and since the 6 mm blur is a resulting
               blur level, so the estimates are not needed).
    
               The -blocks option is provided, but it is the same as the default
               for surface-based analysis, so is not really needed here.  Note that
               the 'surf' block is added and the 'mask' block is removed from the
               volume-based defaults.
    
               important options:
    
                    -blocks         : includes surf, but no mask
                                      (default blocks for surf, so not needed)
                    -surf_anat      : volume aligned with surface
                    -surf_spec      : spec file(s) for surface
    
               Note: one would probably want to use standard mesh surfaces here.
                     This example will be udpated with them in the future.
    
                    afni_proc.py -subj_id FT.surf                            \
                        -blocks tshift align volreg surf blur scale regress  \
                        -copy_anat FT/FT_anat+orig                           \
                        -dsets FT/FT_epi_r?+orig.HEAD                        \
                        -surf_anat FT/SUMA/FTmb_SurfVol+orig                 \
                        -surf_spec FT/SUMA/FTmb_?h.spec                      \
                        -tcat_remove_first_trs 2                             \
                        -align_opts_aea -cost lpc+ZZ                         \
                        -volreg_align_to third                               \
                        -volreg_align_e2a                                    \
                        -blur_size 6                                         \
                        -regress_stim_times FT/AV1_vis.txt FT/AV2_aud.txt    \
                        -regress_stim_labels vis aud                         \
                        -regress_basis 'BLOCK(20,1)'                         \
                        -regress_motion_per_run                              \
                        -regress_censor_motion 0.3                           \
                        -regress_opts_3dD                                    \
                            -jobs 2                                          \
                            -gltsym 'SYM: vis -aud' -glt_label 1 V-A
    

Example 9. Resting state analysis (modern): with censoring and
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

               bandpass filtering.
    
               This is our suggested way to do pre-processing for resting state
               analysis, under the assumption that no cardio/physio recordings
               were made (see example 5 for cardio files).
    
               Censoring due to motion has long been considered appropriate in
               BOLD FMRI analysis, but is less common for those doing bandpass
               filtering in RS FMRI because the FFT requires one to either break
               the time axis (evil) or to replace the censored data with something
               probably inapproprate.
    
               Instead, it is slow (no FFT, but maybe SFT :) but effective to
               regress frequencies within the regression model, where censoring
               is simple.
    
               inputs: anat, EPI
               output: errts dataset (to be used for correlation)
    
               special processing:
                  - despike, as another way to reduce motion effect
                     (see block despike)
                  - censor motion TRs at the same time as bandpassing data
                     (see -regress_censor_motion, -regress_bandpass)
                  - regress motion parameters AND derivatives
                     (see -regress_apply_mot_types)
    
               Note: for resting state data, a more strict threshold may be a good
                     idea, since motion artifacts should play a bigger role than in
                     a task-based analysis.
    
                     So the typical suggestion of motion censoring at 0.3 for task
                     based analysis has been changed to 0.2 for this resting state
                     example, and censoring of outliers has also been added.
    
                     Outliers are typically due to motion, and may capture motion
                     in some cases where the motion parameters do not, because
                     motion is not generally a whole-brain-between-TRs event.
    
               Note: if regressing out regions of interest, either create the ROI
                     time series before the blur step, or remove blur from the list
                     of blocks (and apply any desired blur after the regression).
    
               Note: it might be reasonable to estimate the blur using epits rather
                     than errts in the case of bandpassing.  Both options are
                     included here.
    
               Note: scaling is optional here.  While scaling has no direct effect
                     on voxel correlations, it does have an effect on ROI averages
                     used for correlations.
    
               Other options to consider: -tlrc_NL_warp, -anat_uniform_method
    
                    afni_proc.py -subj_id subj123                                \
                      -dsets epi_run1+orig.HEAD                                  \
                      -copy_anat anat+orig                                       \
                      -blocks despike tshift align tlrc volreg blur mask regress \
                      -tcat_remove_first_trs 3                                   \
                      -volreg_align_e2a                                          \
                      -volreg_tlrc_warp                                          \
                      -regress_censor_motion 0.2                                 \
                      -regress_censor_outliers 0.1                               \
                      -regress_bandpass 0.01 0.1                                 \
                      -regress_apply_mot_types demean deriv                      \
                      -regress_est_blur_epits                                    \
                      -regress_est_blur_errts
    

Example 9b. Resting state analysis with ANATICOR.
+++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               Like example #9, but also regress out the signal from locally
               averaged white matter.  The only change is adding the option
               -regress_anaticor.
    
               Note that -regress_anaticor implies options -mask_segment_anat and
               -mask_segment_erode.
    
                    afni_proc.py -subj_id subj123                                \
                      -dsets epi_run1+orig.HEAD                                  \
                      -copy_anat anat+orig                                       \
                      -blocks despike tshift align tlrc volreg blur mask regress \
                      -tcat_remove_first_trs 3                                   \
                      -volreg_align_e2a                                          \
                      -volreg_tlrc_warp                                          \
                      -regress_anaticor                                          \
                      -regress_censor_motion 0.2                                 \
                      -regress_censor_outliers 0.1                               \
                      -regress_bandpass 0.01 0.1                                 \
                      -regress_apply_mot_types demean deriv                      \
                      -regress_est_blur_epits                                    \
                      -regress_est_blur_errts
    

Example 10. Resting state analysis, with tissue-based regressors.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
               Like example #9, but also regress the eroded white matter averages.
               The WMe mask come from the Classes dataset, created by 3dSeg via the
               -mask_segment_anat and -mask_segment_erode options.
    
            ** While -mask_segment_anat also creates a CSF mask, that mask is ALL
               CSF, not just restricted to the ventricles, for example.  So it is
               probably not appropriate for use in tissue-based regression.
    
               CSFe was previously used as an example of what one could do, but as
               it is not advised, it has been removed.
    
               Also, align to minimum outlier volume, and align to the anatomy
               using cost function lpc+ZZ.
    
               Note: it might be reasonable to estimate the blur using epits rather
                     than errts in the case of bandpassing.  Both options are
                     included here.
    
                    afni_proc.py -subj_id subj123                                \
                      -dsets epi_run1+orig.HEAD                                  \
                      -copy_anat anat+orig                                       \
                      -blocks despike tshift align tlrc volreg blur mask regress \
                      -tcat_remove_first_trs 3                                   \
                      -align_opts_aea -cost lpc+ZZ                               \
                      -volreg_align_to MIN_OUTLIER                               \
                      -volreg_align_e2a                                          \
                      -volreg_tlrc_warp                                          \
                      -mask_segment_anat yes                                     \
                      -mask_segment_erode yes                                    \
                      -regress_censor_motion 0.2                                 \
                      -regress_censor_outliers 0.1                               \
                      -regress_bandpass 0.01 0.1                                 \
                      -regress_apply_mot_types demean deriv                      \
                      -regress_ROI WMe                                           \
                      -regress_est_blur_epits                                    \
                      -regress_est_blur_errts
    

Example 10b. Resting state analysis, with tissue-based regressors and
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

                3dRSFC (for bandpassing and computation of ALFF, etc).
    
                Like example #10, but add -regress_RSFC to bandpass via 3dRSFC.
                Skip censoring and regression bandpassing because of the bandpass
                operation in 3dRSFC.
    
                To correspond to common tractography, this example stays in orig
                space (no 'tlrc' block, no -volreg_tlrc_warp option).  Of course,
                going to standard space is an option.
    
                    afni_proc.py -subj_id subj123                                \
                      -dsets epi_run1+orig.HEAD                                  \
                      -copy_anat anat+orig                                       \
                      -blocks despike tshift align volreg blur mask regress      \
                      -tcat_remove_first_trs 3                                   \
                      -volreg_align_e2a                                          \
                      -blur_size 6.0                                             \
                      -mask_apply epi                                            \
                      -mask_segment_anat yes                                     \
                      -mask_segment_erode yes                                    \
                      -regress_bandpass 0.01 0.1                                 \
                      -regress_apply_mot_types demean deriv                      \
                      -regress_ROI WMe                                           \
                      -regress_RSFC                                              \
                      -regress_run_clustsim no                                   \
                      -regress_est_blur_errts
    

Example 11. Resting state analysis (now even more modern :).
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
             o Yes, censor (outliers and motion) and despike.
             o Align the anatomy and EPI using the lpc+ZZ cost function, rather
               than the default lpc one.
             o Register EPI volumes to the one which has the minimum outlier
                  fraction (so hopefully the least motion).
             o Use non-linear registration to MNI template.
               * This adds a lot of processing time.
             o No bandpassing.
             o Use fast ANATICOR method (slightly different from default ANATICOR).
             o Use FreeSurfer segmentation for:
                 - regression of first 3 principal components of lateral ventricles
                 - ANATICOR white matter mask (for local white matter regression)
               * For details on how these masks were created, see "FREESURFER NOTE"
                 in the help, as it refers to this "Example 11".
             o Input anat is from FreeSurfer (meaning it is aligned with FS masks).
                 - output from FS is usually not quite aligned with input
             o Erode FS white matter and ventricle masks before application.
             o Bring along FreeSurfer parcellation datasets:
                 - aaseg : NN interpolated onto the anatomical grid
                 - aeseg : NN interpolated onto the EPI        grid
               * These 'aseg' follower datasets are just for visualization,
                 they are not actually required for the analysis.
             o Compute average correlation volumes of the errts against the
               the gray matter (aeseg) and ventricle (FSVent) masks.
    
               Note: it might be reasonable to use either set of blur estimates
                     here (from epits or errts).  The epits (uncleaned) dataset
                     has all of the noise (though what should be considered noise
                     in this context is not clear), while the errts is motion
                     censored.  For consistency in resting state, it would be
                     reasonable to stick with epits.  They will likely be almost
                     identical.
    
    
                    afni_proc.py -subj_id FT.11.rest                             \
                      -blocks despike tshift align tlrc volreg blur mask regress \
                      -copy_anat FT_SurfVol.nii                                  \
                      -anat_follower_ROI aaseg anat aparc.a2009s+aseg.nii        \
                      -anat_follower_ROI aeseg epi  aparc.a2009s+aseg.nii        \
                      -anat_follower_ROI FSvent epi FT_vent.nii                  \
                      -anat_follower_ROI FSWe epi FT_white.nii                   \
                      -anat_follower_erode FSvent FSWe                           \
                      -dsets FT_epi_r?+orig.HEAD                                 \
                      -tcat_remove_first_trs 2                                   \
                      -align_opts_aea -cost lpc+ZZ                               \
                      -tlrc_base MNI_caez_N27+tlrc                               \
                      -tlrc_NL_warp                                              \
                      -volreg_align_to MIN_OUTLIER                               \
                      -volreg_align_e2a                                          \
                      -volreg_tlrc_warp                                          \
                      -regress_motion_per_run                                    \
                      -regress_ROI_PC FSvent 3                                   \
                      -regress_make_corr_vols aeseg FSvent                       \
                      -regress_anaticor_fast                                     \
                      -regress_anaticor_label FSWe                               \
                      -regress_censor_motion 0.2                                 \
                      -regress_censor_outliers 0.1                               \
                      -regress_apply_mot_types demean deriv                      \
                      -regress_est_blur_epits                                    \
                      -regress_est_blur_errts
    

Example 11b. Similar to 11, but without FreeSurfer.
+++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
             AFNI currently does not have a good program to extract ventricles.
             But it can make a CSF mask that includes them.  So without FreeSurfer,
             one could import a ventricle mask from the template (e.g. for TT space,
             using TT_desai_dd_mpm+tlrc).  For example, assume Talairach space for
             the analysis, create a ventricle mask as follows:
    
                    3dcalc -a ~/abin/TT_desai_dd_mpm+tlrc                       \
                           -expr 'amongst(a,152,170)' -prefix template_ventricle
                    3dresample -dxyz 2.5 2.5 2.5 -inset template_ventricle+tlrc \
                           -prefix template_ventricle_2.5mm
    
             o Be explicit with 2.5mm, using '-volreg_warp_dxyz 2.5'.
             o Use template TT_N27+tlrc, to be aligned with the desai atlas.
             o No -anat_follower options, but use -mask_import to import the
               template_ventricle_2.5mm dataset (and call it Tvent).
             o Use -mask_intersect to intersect ventricle mask with the subject's
               CSFe mask, making a more reliable subject ventricle mask (Svent).
             o Ventrile principle components are created as per-run regressors.
             o Make WMe and Svent correlation volumes, which are just for
               entertainment purposes anyway.
             o Run the cluster simulation.
    
                    afni_proc.py -subj_id FT.11b.rest                            \
                      -blocks despike tshift align tlrc volreg blur mask regress \
                      -copy_anat FT_anat+orig                                    \
                      -dsets FT_epi_r?+orig.HEAD                                 \
                      -tcat_remove_first_trs 2                                   \
                      -align_opts_aea -cost lpc+ZZ                               \
                      -tlrc_base TT_N27+tlrc                                     \
                      -tlrc_NL_warp                                              \
                      -volreg_align_to MIN_OUTLIER                               \
                      -volreg_align_e2a                                          \
                      -volreg_tlrc_warp                                          \
                      -volreg_warp_dxyz 2.5                                      \
                      -mask_segment_anat yes                                     \
                      -mask_segment_erode yes                                    \
                      -mask_import Tvent template_ventricle_2.5mm+tlrc           \
                      -mask_intersect Svent CSFe Tvent                           \
                      -regress_motion_per_run                                    \
                      -regress_ROI_PC Svent 3                                    \
                      -regress_ROI_PC_per_run Svent                              \
                      -regress_make_corr_vols WMe Svent                          \
                      -regress_anaticor_fast                                     \
                      -regress_censor_motion 0.2                                 \
                      -regress_censor_outliers 0.1                               \
                      -regress_apply_mot_types demean deriv                      \
                      -regress_est_blur_epits                                    \
                      -regress_est_blur_errts                                    \
                      -regress_run_clustsim yes
    

-ask_me EXAMPLES:  ** NOTE: -ask_me is antiquated **
++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
            a1. Apply -ask_me in the most basic form, with no other options.
    
                    afni_proc.py -ask_me
    
            a2. Supply input datasets.
    
                    afni_proc.py -ask_me -dsets ED/ED_r*.HEAD
    
            a3. Same as a2, but supply the datasets in expanded form.
                No suffix (.HEAD) is needed when wildcards are not used.
    
                    afni_proc.py -ask_me                          \
                         -dsets ED/ED_r01+orig ED/ED_r02+orig     \
                                ED/ED_r03+orig ED/ED_r04+orig     \
                                ED/ED_r05+orig ED/ED_r06+orig     \
                                ED/ED_r07+orig ED/ED_r08+orig     \
                                ED/ED_r09+orig ED/ED_r10+orig
    
            a4. Supply datasets, stim_times files and labels.
    
                    afni_proc.py -ask_me                                    \
                            -dsets ED/ED_r*.HEAD                            \
                            -regress_stim_times misc_files/stim_times.*.1D  \
                            -regress_stim_labels ToolMovie HumanMovie       \
                                                 ToolPoint HumanPoint
    

Many NOTE sections:
===================

.. code-block:: none

    

GENERAL ANALYSIS NOTE:
++++++++++++++++++++++

.. code-block:: none

    
        How might one run a full analysis?  Here are some details to consider.
    
        0. Expect to re-run the full analysis.  This might be to fix a mistake, to
           change applied options or to run with current software, to name a few
           possibilities.  So...
    
             - keep permanently stored input data separate from computed results
               (one should be able to easily delete the results to start over)
             - keep scripts in yet another location
             - use file naming that is consistent across subjects and groups,
               making it easy to script with
    
        1. Script everything.  One should be able to carry out the full analysis
           just by running the main scripts.
    
           Learning is best done by typing commands and looking at data, including
           the input to and output from said commands.  But running an analysis for
           publication should not rely on typing complicated commands or pressing
           buttons in a GUI (graphical user interface).
    
             - it is easy to apply to new subjects
             - the steps can be clear and unambiguous (no magic or black boxes)
             - some scripts can be included with publication
               (e.g. an afni_proc.py command, with the AFNI version)
    
             - using a GUI relies on consistent button pressing, making it much
               more difficult to *correctly* repeat, or even understand
    
        2. Analyze and perform quality control on new subjects promptly.
    
             - any problems with the acquisition would (hopefully) be caught early
             - can compare basic quality control measures quickly
    
        3. LOOK AT YOUR DATA.  Quality control is best done by researchers.
           Software should not be simply trusted.
    
             - afni_proc.py processing scripts write guiding @ss_review_driver
               scripts for *minimal* per-subject quality control (i.e. at a
               minimum, run that for every subject)
             - initial subjects should be scrutinized (beyond @ss_review_driver)
    
             - concatenate anat_final datasets to look for consistency
             - concatenate final_epi datasets to look for consistency
             - run gen_ss_review_table.py on the out.ss_review*.txt files
               (making a spreadsheet to quickly scan for outlier subjects)
    
             - many issues can be detected by software, buy those usually just come
               as warnings to the researcher
             - similarly, some issues will NOT be detected by the software
             - for QC, software can assist the researcher, not replace them
    
             NOTE: Data from external sites should be heavily scrutinized,
                   including any from well known public repositories.
    
        3. Consider regular software updates, even as new subjects are acquired.
           This ends up requiring a full re-analysis at the end.
    
           If it will take a while (one year or more?) to collect data, update the
           software regularly (weekly?  monthly?).  Otherwise, the analysis ends up
           being done with old software.
    
              - analysis is run with current, rather than old software
              - will help detect changes in the software (good ones or bad ones)
              - at a minimum, more quality control tools tend to show up
              - keep a copy of the prior software version, in case comparisons are
                desired (@update.afni.binaries does keep one prior version)
              - the full analysis should be done with one software version, so once
                all datasets are collected, back up the current analysis and re-run
                the entire thing with the current software
              - keep a snapshot of the software package used for the analysis
              - report the software version in any publication
    

RESTING STATE NOTE:
+++++++++++++++++++

.. code-block:: none

    
        Resting state data should be processed with physio recordings (for typical
        single-echo EPI data).  Without such recordings, bandpassing is currently
        considered as the default.
    
        Comment on bandpassing:
    
            Bandpassing is the norm right now.  However most TRs may be too long
            for this process to be able to remove the desired components of no
            interest.  On the flip side, if the TRs are short, the vast majority
            of the degrees of freedom are sacrificed just to do it.  Perhaps
            bandpassing will eventually go away, but it is the norm right now.
    
            Also, there is a danger with bandpassing and censoring in that subjects
            with a lot of motion may run out of degrees of freedom (for baseline,
            censoring, bandpassing and removal of other signals of no interest).
            Many papers have been published where a lot of censoring was done,
            many regressors of no interest were projected out, and there was a
            separate bandpass operation.  It is likely that many subjects ended up
            with negative degrees of freedom, making the resulting signals useless
            (or worse, misleading garbage).  But without keeping track of it,
            researchers may not even know.
    
        Bandpassing and degrees of freedom:
    
            Bandpassing between 0.01 and 0.1 means, from just the lowpass side,
            throwing away frequencies above 0.1.  So the higher the frequency of
            collected data (i.e. the smaller the TR), the higher the fraction of
            DoF will be thrown away.
    
            For example, if TR = 2s, then the Nyquist frequency (the highest
            frequency detectable in the data) is 1/(2*2) = 0.25 Hz.  That is to
            say, one could only detect something going up and down at a cycle rate
            of once every 4 seconds (twice the TR).
    
            So for TR = 2s, approximately 40% of the DoF are kept (0.1/0.25) and
            60% are lost (frequencies from 0.1 to 0.25) due to bandpassing.
    
            To generalize, Nyquist = 1/(2*TR), so the fraction of DoF kept is
    
                fraction kept = 0.1/Nyquist = 0.1/(1/(2*TR)) = 0.1*2*TR = 0.2*TR
    
            For example,
    
                at TR = 2 s,   0.4  of DoF are kept (60% are lost)
                at TR = 1 s,   0.2  of DoF are kept (80% are lost)
                at TR = 0.5 s, 0.1  of DoF are kept (90% are lost)
                at TR = 0.1 s, 0.02 of DoF are kept (98% are lost)
    
            Consider also:
    
                Shirer WR, Jiang H, Price CM, Ng B, Greicius MD
                Optimization of rs-fMRI pre-processing for enhanced signal-noise
                    separation, test-retest reliability, and group discrimination
                Neuroimage. 2015 Aug 15;117:67-79.
    
                Gohel SR, Biswal BB
                Functional integration between brain regions at rest occurs in
                    multiple-frequency bands
                Brain connectivity. 2015 Feb 1;5(1):23-34.
    
                Caballero-Gaudes C, Reynolds RC
                Methods for cleaning the BOLD fMRI signal
                Neuroimage. 2017 Jul 1;154:128-49
    
        Application of bandpassing in afni_proc.py:
    
            In afni_proc.py, this is all done in a single regression model (removal
            of noise and baseline signals, bandpassing and censoring).  If some
            subject were to lose too many TRs due to censoring, this step would
            fail, as it should.
    
            There is an additional option of using simulated motion time series
            in the regression model, which should be more effective than higher
            order motion parameters, say.  This is done via @simulate_motion.
    
        There are 3 main steps (generate ricor regs, pre-process, group analysis):
    
            step 0: If physio recordings were made, generate slice-based regressors
                    using RetroTS.py.  Such regressors can be used by afni_proc.py
                    via the 'ricor' processing block.
    
                    RetroTS.m is Ziad Saad's MATLAB routine to convert the 2 time
                    series into 13 slice-based regressors.  RetroTS.m requires the
                    signal processing toolkit for MATLAB.
    
                    RetroTS.py is a conversion of RetroTS.m to python by J Zosky,
                    which depends on scipy.  See "RetroTS.py -help" for details.
    
            step 1: analyze with afni_proc.py
    
                    Consider these afni_proc.py -help examples:
                       5b.  case of ricor and no bandpassing
                       5c.  ricor and bandpassing and full registration
                       9.   no ricor, but with bandpassing
                       9b.  with WMeLocal (local white-matter, eroded) - ANATICOR
                       10.  also with tissue-based regressors
                       10b. apply bandpassing via 3dRSFC
                       soon: extra motion regs via motion simulated time series
                             (either locally or not)
                       11.  censor, despike, non-linear registration,
                            no bandpassing, fast ANATICOR regression,
                            FreeSurfer masks for ventricle/WM regression
                          * see "FREESURFER NOTE" for more details
    
                processing blocks:
    
                    despike (shrink large spikes in time series)
                    ricor   (if applicable, remove the RetroTS regressors)
                    tshift  (correct for slice timing)
                    align   (figure out alignment between anat and EPI)
                    tlrc    (figure out alignment between anat and template)
                    volreg  (align anat and EPI together, and to standard template)
                    blur    (apply desired FWHM blur to EPI data)
                    scale   (optional, e.g. before seed averaging)
                    regress (polort, motion, mot deriv, bandpass, censor)
                            (depending on chosen options)
                            soon: ANATICOR/WMeLocal
                                  extra motion regressors (via motion simulation)
    
                    ==> "result" is errts dataset, "cleaned" of known noise sources
    
            step 2: correlation analysis, hopefully with 3dGroupInCorr
    
                The inputs to this stage are the single subject errts datasets.
    
                Ignoring 3dGroupInCorr, the basic steps in a correlation analysis
                (and corresponding programs) are as follows.  This may be helpful
                for understanding the process, even when using 3dGroupInCorr.
    
                    a. choose a seed voxel (or many) and maybe a seed radius
    
                    for each subject:
    
                       b. compute time series from seed
                          (3dmaskave or 3dROIstats)
                       c. generate correlation map from seed TS
                          (3dTcorr1D (or 3dDeconvolve or 3dfim+))
                       d. normalize R->"Z-score" via Fisher's z-transform
                          (3dcalc -expr atanh)
    
                    e. perform group test, maybe with covariates
                       (3dttest++: 1-sample, 2-sample or paired)
    
                To play around with a single subject via InstaCorr:
    
                    a. start afni (maybe show images of both anat and EPI)
                    b. start InstaCorr plugin from menu at top right of afni's
                       Define Overlay panel
                    c. Setup Icorr:
                        c1. choose errts dataset
                           (no Start,End; no Blur (already done in pre-processing))
                        c2. Automask -> No; choose mask dataset: full_mask
                        c3. turn off Bandpassing (already done, if desired)
                    d. in image window, show correlations
                        d1. go to seed location, right-click, InstaCorr Set
                        OR
                        d1. hold ctrl-shift, hold left mouse button, drag
                    e. have endless fun
    
                To use 3dGroupInCorr:
    
                    a. run 3dSetupGroupIncorr with mask, labels, subject datasets
                       (run once per group of subjects), e.g.
    
                            3dSetupGroupInCorr                \
                                -labels subj.ID.list.txt      \
                                -prefix sic.GROUP             \
                                -mask EPI_mask+tlrc           \
                                errts_subj1+tlrc              \
                                errts_subj2+tlrc              \
                                errts_subj3+tlrc              \
                                    ...                       \
                                errts_subjN+tlrc
    
                        ==> sic.GROUP.grpincorr.niml (and .grpincorr.data)
    
                    b. run 3dGroupInCorr on 1 or 2 sic.GROUP datasets, e.g.
    
                       Here are steps for running 3dGroupInCorr via the afni GUI.
                       To deal with computers that have multiple users, consider
                       specifying some NIML port block that others are not using.
                       Here we use port 2 (-npb 2), just to choose one.
    
                       b1. start afni:
    
                            afni -niml -npb 2
    
                       b2. start 3dGroupInCorr
    
                            3dGroupInCorr -npb 2                    \
                                -setA sic.horses.grpincorr.niml     \
                                -setB sic.moths.grpincorr.niml      \
                                -labelA horses -labelB moths        \
                                -covaries my.covariates.txt         \
                                -center SAME -donocov -seedrad 5
    
                       b3. play with right-click -> InstaCorr Set or
                          hold ctrl-shift/hold left mouse and drag slowly
    
                       b4. maybe save any useful dataset via
                          Define Datamode -> SaveAs OLay (and give a useful name)
    
                    b'. alternative, generate result dataset in batch mode, by
                        adding -batch and some parameters to the 3dGIC command
    
                        e.g.  -batch XYZAVE GIC.HvsM.PFC 4 55 26
    
                        In such a case, afni is not needed at all.  The resulting
                        GIC.HvsM.PFC+tlrc dataset would be written out without any
                        need to start the afni GUI.  This works well since seed
                        coordinates for group tests are generally known in advance.
    
                        See the -batch option under "3dGroupInCorr -help" for many
                        details and options.
    
                    c. threshold/clusterize resulting datasets, just as with a
                       task analysis
    
                       (afni GUI, 3dclust, or 3dmerge)
    

FREESURFER NOTE:
++++++++++++++++

.. code-block:: none

    
        FreeSurfer output can be used for a few things in afni_proc.py:
    
            - simple skull stripping (i.e. instead of 3dSkullStrip)
            - running a surface-based analysis
            - using parcellation datasets for:
               - tissue-based regression
               - creating group probability maps
               - creating group atlases (e.g. maximum probability maps)
    
        This NOTE mainly refers to using FreeSurfer parcellations for tissue-based
        regression, as is done in Example 11.
    
    
        First run FreeSurfer, then import to AFNI using @SUMA_Make_Spec_FS, then
        make ventricle and white matter masks from the Desikan-Killiany atlas based
        parcellation dataset, aparc+aseg.nii.
    
        Note that the aparc.a2009s segmentations are based on the Destrieux atlas,
        which might be nicer for probability maps, though the Desikan-Killiany
        aparc+aseg segmentation is currently used for segmenting white matter and
        ventricles.  I have not studied the differences.
    
    
        Example 11 brings the aparc.a2009s+aseg segmentation along (for viewing or
        atlas purposes, aligned with the result), though the white matter and
        ventricle masks are based instead on aparc+aseg.nii.
    
            # run (complete) FreeSurfer on FT.nii
            recon-all -all -subject FT -i FT.nii
    
            # import to AFNI, in NIFTI format
            @SUMA_Make_Spec_FS -sid FT -NIFTI
    
            # create ventricle and white matter masks
            # ** warning: it would be good to convert these indices to labels
            #             in case the output from FreeSurfer is changed
    
            3dcalc -a aparc+aseg.nii -datum byte -prefix FT_vent.nii \
                   -expr 'amongst(a,4,43)'
            3dcalc -a aparc+aseg.nii -datum byte -prefix FT_WM.nii \
                   -expr 'amongst(a,2,7,41,46,251,252,253,254,255)'
    
            # note: 16 (brainstem) was incorrectly included from @ANATICOR
            #       and then in this help through 2016
    
        After this, FT_SurfVol.nii, FT_vent.nii and FT_WM.nii (along with the
        basically unused aparc.a2009s+aseg.nii) are passed to afni_proc.py.
    
    
      * Be aware that the output from FreeSurfer (e.g. FT_SurfVol.nii) will
        usually not quite align with the input (e.g. FT.nii).  So parcellation
        datasets will also not quite align with the input (FT.nii).  Therefore,
        when passing parcellation volumes to afni_proc.py for tissue-based
        regression, it is important to use the anatomy output from FreeSurfer
        as the subject anatomy (input to afni_proc.py).  That way, the anatomy
        and parcellation datasets will be in register, and therefore the EPI
        will eventually align with the parcellation datasets.
    
        If it is important to have the FreeSurfer output align with the input,
        it might help to pass a modified volume to FreeSurfer.  Use 3dresample
        and then 3dZeropad (if necessary) to make a volume with 1 mm^3 voxels
        and an even number voxels in each direction.  The @SUMA_Make_Spec_FS
        help provides some details on this.
    
        The exact 3dZeropad command depends on the grid output by 3dresample.
    
            3dresample -inset FT_anat+orig -dxyz 1 1 1 -prefix FT.1 -rmode Cu
            3dZeropad -L 1 -prefix FT.1.z.nii FT.1+orig
            recon-all -all -subject FT -i FT.1.z.nii
            @SUMA_Make_Spec_FS -sid FT -NIFTI
    

TIMING FILE NOTE:
+++++++++++++++++

.. code-block:: none

    
        One issue that the user must be sure of is the timing of the stimulus
        files (whether -regress_stim_files or -regress_stim_times is used).
    
        The 'tcat' step will remove the number of pre-steady-state TRs that the
        user specifies (defaulting to 0).  The stimulus files, provided by the
        user, must match datasets that have had such TRs removed (i.e. the stim
        files should start _after_ steady state has been reached).
    

MASKING NOTE:
+++++++++++++

.. code-block:: none

    
        The default operation of afni_proc.py has changed (as of 24 Mar, 2009).
        Prior to that date, the default was to apply the 'epi' mask.  As of
        17 Jun 2009, only the 'extents' mask is, if appropriate.
    
    
        There may be 4 masks created by default, 3 for user evaluation and all for
        possible application to the EPI data (though it may not be recommended).
        The 4th mask (extents) is a special one that will be applied at volreg when
        appropriate, unless the user specifies otherwise.
    
        If the user chooses to apply one of the masks to the EPI regression (again,
        not necessarily recommended), it is done via the option -mask_apply while
        providing the given mask type (epi, anat, group or extents).
    
        --> To apply a mask during regression, use -mask_apply.
    
        Mask descriptions (afni_proc.py name, dataset name, short description):
    
        1. epi ("full_mask") : EPI Automask
    
           An EPI mask dataset will be created by running '3dAutomask -dilate 1'
           on the EPI data after blurring.  The 3dAutomask command is executed per
           run, after which the masks are combined via a union operation.
    
        2. anat ("mask_anat.$subj") : anatomical skull-stripped mask
    
           If possible, a subject anatomy mask will be created.  This anatomical
           mask will be created from the appropriate skull-stripped anatomy,
           resampled to match the EPI (that is output by 3dvolreg) and changed into
           a binary mask.
    
           This requires either the 'align' block or a tlrc anatomy (from the
           'tlrc' block, or just copied via '-copy_anat').  Basically, it requires
           afni_proc.py to know of a skull-stripped anatomical dataset.
    
           By default, if both the anat and EPI masks exist, the overlap between
           them will be computed for evaluation.
    
        3. group ("mask_group") : skull-stripped @auto_tlrc base
    
           If possible, a group mask will be created.  This requires the 'tlrc'
           block, from which the @auto_tlrc -base dataset is chosen as the group
           anatomy.  It also requires '-volreg_warp_epi' so that the EPI is in
           standard space.  The group anatomy is then resampled to match the EPI
           and changed into a binary mask.
    
        4. extents ("mask_extents") : mask based on warped EPI extents
    
           In the case of transforming the EPI volumes to match the anatomical
           volume (via either -volreg_align_e2a or -volreg_tlrc_warp), an extents
           mask will be created.  This is to avoid a motion artifact that arises
           when transforming from a smaller volume (EPI) to a larger one (anat).
    
        ** Danger Will Robinson! **
    
           This EPI extents mask is considered necessary because the align/warp
           transformation that is applied on top of the volreg alignment transform
           (applied at once), meaning the transformation from the EPI grid to the
           anatomy grid will vary per TR.
    
           The effect of this is seen at the edge voxels (extent edge), where a
           time series could be zero for many of the TRs, but have valid data for
           the rest of them.  If this timing just happens to correlate with any
           regressor, the result could be a strong "activation" for that regressor,
           but which would be just a motion based artifact.
    
           What makes this particularly bad is that if it does happen, it tends to
           happen for *a cluster* of many voxels at once, possibly an entire slice.
           Such an effect is compounded by any additional blur.  The result can be
           an entire cluster of false activation, large enough to survive multiple
           comparison corrections.
    
           Thanks to Laura Thomas and Brian Bones for finding this artifact.
    
       --> To deal with this, a time series of all 1s is created on the original
           EPI grid space.  Then for each run it is warped with to the same list of
           transformations that is applied to the EPI data in the volreg step
           (volreg xform and either alignment to anat or warp to standard space).
           The result is a time series of extents of each original volume within
           the new grid.
    
           These volumes are then intersected over all TRs of all runs.  The final
           mask is the set of voxels that have valid data at every TR of every run.
           Yay.
    
        5. Classes and Classes_resam: GM, WM, CSF class masks from 3dSeg
    
           By default, unless the user requests otherwise (-mask_segment_anat no),
           and if anat_final is skull-stripped, then 3dSeg will be used to segment
           the anatomy into gray matter, white matter and CSF classes.
    
           A dataset named Classes is the result of running 3dSeg, which is then
           resampled to match the EPI and named Classes_resam.
    
           If the user wanted to, this dataset could be used for regression of
           said tissue classes (or eroded versions).
    
    
        --- masking, continued...
    
        Note that it may still not be a good idea to apply any of the masks to the
        regression, as it might then be necessary to intersect such masks across
        all subjects, though applying the 'group' mask might be reasonable.
    
     ** Why has the default been changed?
    
        It seems much better not to mask the regression data in the single-subject
        analysis at all, send _all_ of the results to group space, and apply an
        anatomically-based mask there.  That could be computed from the @auto_tlrc
        reference dataset or from the union of skull-stripped subject anatomies.
    
        Since subjects have varying degrees of signal dropout in valid brain areas
        of the EPI data, the resulting EPI intersection mask that would be required
        in group space may exclude edge regions that are otherwise desirable.
    
        Also, it is helpful to see if much 'activation' appears outside the brain.
        This could be due to scanner or interpolation artifacts, and is useful to
        note, rather than to simply mask out and never see.
    
        Rather than letting 3dAutomask decide which brain areas should not be
        considered valid, create a mask based on the anatomy _after_ the results
        have been warped to a standard group space.  Then perhaps dilate the mask
        by one voxel.  Example #11 from '3dcalc -help' shows how one might dilate.
    
     ** Note that the EPI data can now be warped to standard space at the volreg
        step.  In that case, it might be appropriate to mask the EPI data based
        on the Talairach template, such as what is used for -base in @auto_tlrc.
        This can be done via '-mask_apply group'.
    
    
     ** For those who have processed some of their data with the older method:
    
        Note that this change should not be harmful to those who have processed
        data with older versions of afni_proc.py, as it only adds non-zero voxel
        values to the output datasets.  If some subjects were analyzed with the
        older version, the processing steps should not need to change.  It is still
        necessary to apply an intersection mask across subjects in group space.
    
        It might be okay to create the intersection mask from only those subjects
        which were masked in the regression, however one might say that biases the
        voxel choices toward those subjects, though maybe that does not matter.
        Any voxels used would still be across all subjects.
    
    
        A mask dataset is necessary when computing blur estimates from the epi and
        errts datasets.  Also, since it is nice to simply see what the mask looks
        like, its creation has been left in by default.
    
        The '-regress_no_mask' option is now unnecessary.
    
    
        Note that if no mask were applied in the 'scaling' step, large percent
        changes could result.  Because large values would be a detriment to the
        numerical resolution of the scaled short data, the default is to truncate
        scaled values at 200 (percent), which should not occur in the brain.
    

BLIP NOTE:
++++++++++

.. code-block:: none

    
        application of reverse-blip (blip-up/blip-down) registration:
    
           o compute the median of the forward and reverse-blip data
           o align them using 3dQwarp -plusminus
              -> the main output warp is the square root of the forward warp
                 to the reverse, i.e. it warps the forward data halfway
              -> in theory, this warp should make the EPI anatomically accurate
    
        order of operations:
    
           o the blip warp is computed after all initial temporal operations
             (despike, ricor, tshift)
           o and before all spatial operations (anat/EPI align, tlrc, volreg)
    
        notes:
    
           o If no forward blip time series (volume?) is provided by the user,
             the first time points from the first run will be used (using the
             same number of time points as in the reverse blip time series).
           o As usual, all registration transformations are combined.
    
        differences with unWarpEPI.py (R Cox, D Glen and V Roopchansingh):
    
                            afni_proc.py            unWarpEPI.py
                            --------------------    --------------------
           tshift step:     before unwarp           after unwarp
                            (option: after unwarp)
    
           volreg program:  3dvolreg                3dAllineate
    
           volreg base:     as before               median warped dset
                            (option: MEDIAN_BLIP)   (same as MEDIAN_BLIP)
    
           unifize EPI?     no (option: yes)        yes
           (align w/anat)
    

ANAT/EPI ALIGNMENT CASES NOTE:
++++++++++++++++++++++++++++++

.. code-block:: none

    
        This outlines the effects of alignment options, to help decide what options
        seem appropriate for various cases.
    
        1. EPI to EPI alignment (the volreg block)
    
            Alignment of the EPI data to a single volume is based on the 3 options
            -volreg_align_to, -volreg_base_dset and -volreg_base_ind, where the
            first option is by far the most commonly used.
    
            Note that a good alternative is: '-volreg_align_to MIN_OUTLIER'.
    
            The logic of EPI alignment in afni_proc.py is:
    
                a. if -volreg_base_dset is given, align to that
                   (this volume is copied locally as the dataset ext_align_epi)
                b. otherwise, use the -volreg_align_to or -volreg_base_ind volume
    
            The typical case is to align the EPI to one of the volumes used in
            pre-processing (where the dataset is provided by -dsets and where the
            particular TR is not removed by -tcat_remove_first_trs).  If the base
            volume is the first or third (TR 0 or 2) from the first run, or is the
            last TR of the last run, then -volreg_align_to can be used.
    
            To specify a TR that is not one of the 3 just stated (first, third or
            last), -volreg_base_ind can be used.
    
            To specify a volume that is NOT one of those used in pre-processing
            (such as the first pre-steady state volume, which would be excluded by
            the option -tcat_remove_first_trs), use -volreg_base_dset.
    
        2. anat to EPI alignment cases (the align block)
    
            This is specific to the 'align' processing block, where the anatomy is
            aligned to the EPI.  The focus is on which EPI volume the anat gets
            aligned to.  Whether this transformation is inverted in the volreg
            block (to instead align the EPI to the anat via -volreg_align_e2a) is
            an independent consideration.
    
            The logic of which volume the anatomy gets aligned to is as follows:
                a. if -align_epi_ext_dset is given, use that for anat alignment
                b. otherwise, if -volreg_base_dset, use that
                c. otherwise, use the EPI base from the EPI alignment choice
    
            To restate this: the anatomy gets aligned to the same volume the EPI
            gets aligned to *unless* -align_epi_ext_dset is given, in which case
            that volume is used.
    
            The entire purpose of -align_epi_ext_dset is for the case where the
            user might want to align the anat to a different volume than what is
            used for the EPI (e.g. align anat to a pre-steady state TR but the EPI
            to a steady state one).
    
            Output:
    
               The result of the align block is an 'anat_al' dataset.  This will be
               in alignment with the EPI base (or -align_epi_ext_dset).
    
               In the default case of anat -> EPI alignment, the aligned anatomy
               is actually useful going forward, and is so named 'anat_al_keep'.
    
               Additionally, if the -volreg_align_e2a option is used (thus aligning
               the EPI to the original anat), then the aligned anat dataset is no
               longer very useful, and is so named 'anat_al_junk'.  However, unless
               an anat+tlrc dataset was copied in for use in -volreg_tlrc_adwarp,
               the skull-striped anat (anat_ss) becomes the current one going
               forward.  That is identical to the original anat, except that it
               went through the skull-stripping step in align_epi_anat.py.
    
               At that point (e2a case) the pb*.volreg.* datasets are aligned with
               the original anat or the skull-stripped original anat (and possibly
               in Talairach space, if the -volreg_tlrc_warp or _adwarp option was
               applied).
    
             Checking the results:
    
               The pb*.volreg.* volumes should be aligned with the anat.  If
               -volreg_align_e2a was used, it will be with the original anat.
               If not, then it will be with anat_al_keep.
    
               Note that at the end of the regress block, whichever anatomical
               dataset is deemed "in alignment" with the stats dataset will be
               copied to anat_final.$subj.
    
               So compare the volreg EPI with the final anatomical dataset.
    

ANAT/EPI ALIGNMENT CORRECTIONS NOTE:
++++++++++++++++++++++++++++++++++++

.. code-block:: none

    
        Aligning the anatomy and EPI is sometimes difficult, particularly depending
        on the contrast of the EPI data (between tissue types).  If the alignment
        fails to do a good job, it may be necessary to run align_epi_anat.py in a
        separate location, find options that help it to succeed, and then apply
        those options to re-process the data with afni_proc.py.
    
        1. If the anat and EPI base do not start off fairly close in alignment,
           the -giant_move option may be needed for align_epi_anat.py.  Pass this
           option to AEA.py via the afni_proc.py option -align_opts_aea:
    
                afni_proc.py ... -align_opts_aea -giant_move
    
        2. The default cost function used by align_epi_anat.py is lpc (local
           Pearson correlation).  If this cost function does not work (probably due
           to poor or unusual EPI contrast), then consider cost functions such as
           lpa (absolute lpc), lpc+ (lpc plus fractions of other cost functions) or
           lpc+ZZ (approximate with lpc+, but finish with pure lpc).
    
           The lpa and lpc+ZZ cost functions are common alternatives.  The
           -giant_move option may be necessary independently.
    
           Examples of some helpful options:
    
             -align_opts_aea -cost lpa
             -align_opts_aea -giant_move
             -align_opts_aea -cost lpc+ZZ -giant_move
             -align_opts_aea -check_flip
             -align_opts_aea -cost lpc+ZZ -giant_move -resample off
             -align_opts_aea -skullstrip_opts -blur_fwhm 2
    
        3. Testing alignment with align_epi_anat.py directly.
    
           When having alignment problems, it may be more efficient to copy the
           anat and EPI alignment base to a new directory, figure out a good cost
           function or other options, and then apply them in a new afni_proc.py
           command.
    
           For testing purposes, it helps to test many cost functions at once.
           Besides the cost specified by -cost, other cost functions can be applied
           via -multi_cost.  This is efficient, since all of the other processing
           does not need to be repeated.  For example:
    
             align_epi_anat.py -anat2epi                    \
                    -anat subj99_anat+orig                  \
                    -epi pb01.subj99.r01.tshift+orig        \
                    -epi_base 0 -volreg off -tshift off     \
                    -giant_move                             \
                    -cost lpc -multi_cost lpa lpc+ZZ mi
    
           That adds -giant_move, and uses the basic lpc cost function along with
           3 additional cost functions (lpa, lpc+ZZ, mi).  The result is 4 new
           anatomies aligned to the EPI, 1 per cost function:
    
                   subj99_anat_al+orig         - cost func lpc      (see -cost opt)
                   subj99_anat_al_lpa+orig     - cost func lpa         (additional)
                   subj99_anat_al_lpc+ZZ+orig  - cost func lpc+ZZ      (additional)
                   subj99_anat_al_mi+orig      - cost func mi          (additional)
    
           Also, if part of the dataset gets clipped in the case of -giant_move,
           consider the align_epi_anat.py option '-resample off'.
    

WARP TO TLRC NOTE:
++++++++++++++++++

.. code-block:: none

    
        afni_proc.py can now apply a +tlrc transformation to the EPI data as part
        of the volreg step via the option '-volreg_tlrc_warp'.  Note that it can
        also align the EPI and anatomy at the volreg step via '-volreg_align_e2a'.
    
        Manual Talairach transformations can also be applied, but separately, after
        volreg.  See '-volreg_tlrc_adwarp'.
    
        This tlrc transformation is recommended for many reasons, though some are
        not yet implemented.  Advantages include:
    
            - single interpolation of the EPI data
    
                Done separately, volume registration, EPI to anat alignment and/or
                the +tlrc transformation interpolate the EPI data 2 or 3 times.  By
                combining these transformations into a single one, there is no
                resampling penalty for the alignment or the warp to standard space.
    
                Thanks to D Glen for the steps used in align_epi_anat.py.
    
            - EPI time series become directly comparable across subjects
    
                Since the volreg output is now in standard space, there is already
                voxel correspondence across subjects with the EPI data.
    
            - group masks and/or atlases can be applied to the EPI data without
              additional warping
    
                It becomes trivial to extract average time series data over ROIs
                from standard atlases, say.
    
                This could even be done automatically with afni_proc.py, as part
                of the single-subject processing stream (not yet implemented).
                One would have afni_proc.py extract average time series (or maybe
                principal components) from all the ROIs in a dataset and apply
                them as regressors of interest or of no interest.
    
            - with 3dBlurToFWHM, using an AlphaSim look-up table might be possible
    
                Since the blur and data grid could both be isotropic and integral,
                and since the transformation could depend on a known anatomy (such
                as the N27 Colin brain or icbm_452), it would be easy to create a
                look-up table of AlphaSim results (so users would not actually need
                to run it).
    
                The known numbers would correspond to a cluster size (each for a
                given, common voxel-wise threshold).  This correction could then
                be applied automatically.  Again, not yet implemented...
    
            - no interpolation of statistics
    
                If the user wishes to include statistics as part of the group
                analysis (e.g. using 3dMEMA.R), this warping becomes more needed.
                Warping to standard space *after* statistics are generated is not
                terribly valid.
    

RETROICOR NOTE:
+++++++++++++++

.. code-block:: none

    
        ** Cardiac and respiratory regressors must be created from an external
           source, such as the RetroTS.m matlab program written by Z Saad.  The
           input to that would be the 2+ signals.  The output would be a single
           file per run, containing 13 or more regressors for each slice.  That
           set of output files would be applied here in afni_proc.py.
    
        Removal of cardiac and respiratory regressors can be done using the 'ricor'
        processing block.  By default, this would be done after 'despike', but
        before any other processing block.
    
        These card/resp signals would be regressed out of the MRI data in the
        'ricor' block, after which processing would continue normally. In the final
        'regress' block, regressors for slice 0 would be applied (to correctly
        account for the degrees of freedom and also to remove residual effects).
            --> This is now only true when using '-regress_apply_ricor yes'.
                The default as of 30 Jan 2012 is to not include them in the final
                regression (since degrees of freedom are really not important for a
                subsequent correlation analysis).
    
        Users have the option of removing the signal "per-run" or "across-runs".
    
        Example R1: 7 runs of data, 13 card/resp regressors, process "per-run"
    
            Since the 13 regressors are processed per run, the regressors can have
            different magnitudes each run.  So the 'regress' block will actually
            get 91 extra regressors (13 regressors times 7 runs each).
    
        Example R2: process "across-run"
    
            In this case the regressors are catenated across runs when they are
            removed from the data.  The major difference between this and "per-run"
            is that now only 1 best fit magnitude is applied per regressor (not the
            best for each run).  So there would be only the 13 catenated regressors
            for slice 0 added to the 'regress' block.
    
        Those analyzing resting-state data might prefer the per-run method, as it
        would remove more variance and degrees of freedom might not be as valuable.
    
        Those analyzing a normal signal model might prefer doing it across-runs,
        giving up only 13 degrees of freedom, and helping not to over-model the
        data.
    
        ** The minimum options would be specifying the 'ricor' block (preferably
           after despike), along with -ricor_regs and -ricor_regress_method.
    
        Example R3: afni_proc.py option usage:
    
            Provide additional options to afni_proc.py to apply the despike and
            ricor blocks (which will be the first 2 blocks by default), with each
            regressor named 'slibase*.1D' going across all runs, and where the
            first 3 TRs are removed from each run (matching -tcat_remove_first_trs,
            most likely).
    
                -do_block despike ricor
                -ricor_regs slibase*.1D
                -ricor_regress_method across-runs
                -ricor_regs_nfirst 3
    

RUNS OF DIFFERENT LENGTHS NOTE:
+++++++++++++++++++++++++++++++

.. code-block:: none

    
        In the case that the EPI datasets are not all of the same length, here
        are some issues that may come up, listed by relevant option:
    
            -volreg_align_to        OK, as of version 1.49.
    
            -ricor_regress_method   OK, as of version 3.05.
    
            -regress_polort         Probably no big deal.
                                    If this option is not used, then the degree of
                                    polynomial used for the baseline will come from
                                    the first run.  Only 1 polort may be applied.
    
            -regress_est_blur_epits OK, as of version 1.49.
    
         *  -regress_use_stim_files This may fail, as make_stim_times.py is not
                                    currently prepared to handle runs of different
                                    lengths.
    
            -regress_censor_motion  OK, as of version 2.14
    
         * probably will be fixed (please let me know of interest)
    

SCRIPT EXECUTION NOTE:
++++++++++++++++++++++

.. code-block:: none

    
        The suggested way to run the output processing SCRIPT is via...
    
            a) if you use tcsh:    tcsh -xef SCRIPT |& tee output.SCRIPT
    
            b) if you use bash:    tcsh -xef SCRIPT 2>&1 | tee output.SCRIPT
    
            c) if you use tcsh and the script is executable, maybe use one of:
    
                                ./SCRIPT |& tee output.SCRIPT
                                ./SCRIPT 2>&1 | tee output.SCRIPT
    
        Consider usage 'a' for example:  tcsh -xef SCRIPT |& tee output.SCRIPT
    
        That command means to invoke a new tcsh with the -xef options (so that
        commands echo to the screen before they are executed, exit the script
        upon any error, do not process the ~/.cshrc file) and have it process the
        SCRIPT file, piping all output to the 'tee' program, which will duplicate
        output back to the screen, as well as to the given output file.
    
        parsing the command: tcsh -xef SCRIPT |& tee output.SCRIPT
    
            a. tcsh
    
               The script itself is written in tcsh syntax and must be run that way.
               It does not mean the user must use tcsh.  Note uses 'a' and 'b'.
               There tcsh is specified by the user.  The usage in 'c' applies tcsh
               implicitly, because the SCRIPT itself specifies tcsh at the top.
    
            b. tcsh -xef
    
               The -xef options are applied to tcsh and have the following effects:
    
                    -x : echo commands to screen before executing them
                    -e : exit (terminate) the processing on any errors
                    -f : do not process user's ~/.cshrc file
    
               The -x option is very useful so one see not just output from the
               programs, but the actual commands that produce the output.  It
               makes following the output much easier.
    
               The -e option tells the shell to terminate on any error.  This is
               useful for multiple reasons.  First, it allows the user to easily
               see the failing command and error message.  Second, it would be
               confusing and useless to have the script try to continue, without
               all of the needed data.
    
               The -f option tells the shell not to process the user's ~/.cshrc
               (or ~/.tcshrc) file.  The main reason for including this is because
               of the -x option.  If there were any errors in the user's ~/.cshrc
               file and -x option were used, they would terminate the shell before
               the script even started, probably leaving the user confused.
    
            c. tcsh -xef SCRIPT
    
               The T-shell is invoked as described above, executing the contents
               of the specified text file (called 'SCRIPT', for example) as if the
               user had typed the included commands in their terminal window.
    
            d. |&
    
               These symbols are for piping the output of one program to the input
               of another.  Many people know how to do 'afni_proc.py -help | less'
               (or maybe '| more').  This script will output a lot of text, and we
               want to get a copy of that into a text file (see below).
    
               Piping with '|' captures only stdout (standard output), and would
               not capture errors and warnings that appear.  Piping with '|&'
               captures both stdout and stderr (standard error).  The user may not
               be able to tell any difference between those file streams on the
               screen, but since programs write to both, we want to capture both.
    
            e. tee output.SCRIPT
    
               Where do we want to send this captured stdout and stderr text?  Send
               it to the 'tee' program.  Like a plumber's tee, the 'tee' program
               splits the data (not water) stream off into 2 directions.
    
               Here, one direction that tee sends the output is back to the screen,
               so the user can still see what is happening.
    
               The other direction is to the user-specified text file.  In this
               example it would be 'output.SCRIPT'.  With this use of 'tee', all
               screen output will be duplicated in that text file.
    

OPTIONS: (information options, general options, block options)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

                 (block options are ordered by block)
    

------------ informational/terminal options ------------
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

    
            -help                   : show this help
            -hist                   : show the module history
    
            -requires_afni_version  : show AFNI date required by processing script
    
                Many updates to afni_proc.py are accompanied by corresponding
                updates to other AFNI programs.  So if the processing script is
                created on one computer but executed on another (with an older
                version of AFNI), confusing failures could result.
    
                The required date is adjusted whenever updates are made that rely
                on new features of some other program.  If the processing script
                checks the AFNI version, the AFNI package must be as current as the
                date output via this option.  Checks are controlled by the option
                '-check_afni_version'.
    
                The checking method compares the output of:
                    afni_proc.py -requires_afni_version
    
                against the most recent date in afni_history:
                    afni_history -past_entries 1
    
                See also '-requires_afni_hist'.
    
                See also '-check_afni_version'.
    
            -requires_afni_hist     : show history of -requires_afni_version
    
                List the history of '-requires_afni_version' dates and reasons.
    
            -show_valid_opts        : show all valid options (brief format)
            -ver                    : show the version number
    

------------ general execution and setup options ------------
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

    
            -anat_follower LABEL GRID DSET : specify anat follower dataset
    
                    e.g. -anat_follower GM anat FS_GM_MASK.nii
    
                Use this option to pass any anatomical follower dataset.  Such a
                dataset is warped by any transformations that take the original
                anat to anat_final.
    
                Anatomical follower datasets are resampled using wsinc5.  The only
                difference with -anat_follower_ROI is that such ROI datasets are
                resampled using nearest neighbor interpolation.
    
                   LABEL    : to name and refer to this dataset
                   GRID     : which grid should this be sampled on, anat or epi?
                   DSET     : name of input dataset, changed to copy_af_LABEL
    
                A default anatomical follower (in the case of skull stripping) is
                the original anat.  That is to get a warped version that still has
                a skull, for quality control.
    
                See also -anat_follower_ROI, anat_follower_erode.
    
            -anat_follower_erode LABEL LABEL ...: erode masks for given labels
    
                    e.g. -anat_follower_erode WMe
    
                Perform a single erosion step on the mask dataset for the given
                label.  This is done on the input ROI (anatomical?) grid.
    
                The erosion step is applied before any transformation, and uses the
                18-neighbor approach (6 face and 12 edge neighbors, not 8 corner
                neighbors) in 3dmask_tool.
    
                See also -regress_ROI_PC, -regress_ROI.
                Please see '3dmask_tool -help' for more information on eroding.
    
            -anat_follower_ROI LABEL GRID DSET : specify anat follower ROI dataset
    
                    e.g. -anat_follower_ROI aaseg anat aparc.a2009s+aseg.nii
                    e.g. -anat_follower_ROI FSvent epi FreeSurfer_ventricles.nii
    
                Use this option to pass any anatomical follower dataset.  Such a
                dataset is warped by any transformations that take the original
                anat to anat_final.
    
                Similar to -anat_follower, except that these anatomical follower
                datasets are resampled using nearest neighbor (NN) interpolation,
                to preserve data values (as opposed to -anat_follower, which uses
                wsinc5).  That is the only difference between these options.
    
                   LABEL    : to name and refer to this dataset
                   GRID     : which grid should this be sampled on, anat or epi?
                   DSET     : name of input dataset, changed to copy_af_LABEL
    
                Labels defined via this option may be used in -regress_ROI or _PC.
    
                See also -anat_follower, anat_follower_erode, -regress_ROI
                or -regress_ROI_PC.
    
            -anat_has_skull yes/no  : specify whether the anatomy has a skull
    
                    e.g. -anat_has_skull no
    
                Use this option to block any skull-stripping operations, likely
                either in the align or tlrc processing blocks.
    
            -anat_uniform_method METHOD : specify uniformity correction method
    
                    e.g. -anat_uniform_method unifize
    
                Specify the method for anatomical intensity uniformity correction.
    
                    none    : do not do uniformity correction at all
                    default : use 3dUnifize at whim of auto_warp.py
                    unifize : apply 3dUnifize early in processing stream
                              (so it affects more than auto_warp.py)
    
                Please see '3dUnifize -help' for details.
                See also -anat_opts_unif.
    
            -anat_opts_unif OPTS ... : specify extra options for unifize command
    
                    e.g. -anat_opts_unif -Urad 14
    
                Specify options to be applied to the command used for anatomical
                intensity uniformity correction, such as 3dUnifize.
    
                Please see '3dUnifize -help' for details.
                See also -anat_uniform_method.
    
            -anat_unif_GM yes/no    : also unifize gray matter (lower intensities)
                                      the default is 'no'
    
                    e.g. -anat_unif_GM yes
                    default: -anat_unif_GM no
    
                If this is set to yes, 3dUnifize will not only apply uniformity
                correction across the brain volume, but also to voxels that look
                like gray matter.  That is to say the option adds '-GM' to the
                3dUnifize command.
    
              * The default was changed from yes to no 2014, May 16.
    
                Please see '3dUnifize -help' for details.
                See also -anat_uniform_method, -anat_opts_unif.
    
            -ask_me                 : ask the user about the basic options to apply
    
                When this option is used, the program will ask the user how they
                wish to set the basic options.  The intention is to give the user
                a feel for what options to apply (without using -ask_me).
    
            -bash                   : show example execution command in bash form
    
                After the script file is created, this program suggests how to run
                it (piping stdout/stderr through 'tee').  If the user is running
                the bash shell, this option will suggest the 'bash' form of a
                command to execute the newly created script.
    
                example of tcsh form for execution:
    
                    tcsh -x proc.ED.8.glt |& tee output.proc.ED.8.glt
    
                example of bash form for execution:
    
                    tcsh -x proc.ED.8.glt 2>&1 | tee output.proc.ED.8.glt
    
                Please see "man bash" or "man tee" for more information.
    
            -blocks BLOCK1 ...      : specify the processing blocks to apply
    
                    e.g. -blocks volreg blur scale regress
                    e.g. -blocks despike tshift align volreg blur scale regress
                    default: tshift volreg blur mask scale regress
    
                The user may apply this option to specify which processing blocks
                are to be included in the output script.  The order of the blocks
                may be varied, and blocks may be skipped.
    
                See also '-do_block' (e.g. '-do_block despike').
    
            -check_afni_version yes/no : check that AFNI is current enough
    
                    e.g. -check_afni_version no
                    default: yes
    
                Check that the version of AFNI is recent enough for processing of
                the afni_proc.py script.
    
                For the version check, the output of:
                    afni_proc.py -requires_afni_version
    
                is tested against the most recent date in afni_history:
                    afni_history -past_entries 1
    
                In the case that newer features in other programs might not be
                needed by the given afni_proc.py script (depending on the options),
                the user is left with this option to ignore the AFNI version check.
    
                Please see 'afni_history -help' or 'afni -ver' for more information.
                See also '-requires_afni_version'.
    
            -check_results_dir yes/no : check whether dir exists before proceeding
    
                    e.g. -check_results_dir no
                    default: yes
    
                By default, if the results directory already exists, the script
                will terminate before doing any processing.  Set this option to
                'no' to remove that check.
    
            -check_setup_errors yes/no : terminate on setup errors
    
                    e.g. -check_setup_errors yes
                    default: no
    
                Have the script check $status after each command in the setup
                processing block.  It is preferable to run the script using the
                -e option to tcsh (as suggested), but maybe the user does not wish
                to do so.
    
            -copy_anat ANAT         : copy the ANAT dataset to the results dir
    
                    e.g. -copy_anat Elvis/mprage+orig
    
                This will apply 3dcopy to copy the anatomical dataset(s) to the
                results directory.  Note that if a +view is not given, 3dcopy will
                attempt to copy +acpc and +tlrc datasets, also.
    
                See also '3dcopy -help'.
    
            -copy_files file1 ...   : copy file1, etc. into the results directory
    
                    e.g. -copy_files glt_AvsB.txt glt_BvsC.1D glt_eat_cheese.txt
                    e.g. -copy_files contrasts/glt_*.txt
    
                This option allows the user to copy some list of files into the
                results directory.  This would happen before the tcat block, so
                such files may be used for other commands in the script (such as
                contrast files in 3dDeconvolve, via -regress_opts_3dD).
    
            -do_block BLOCK_NAME ...: add extra blocks in their default positions
    
                    e.g. -do_block despike ricor
                    e.g. -do_block align
    
                With this option, any 'optional block' can be applied in its
                default position.  This includes the following blocks, along with
                their default positions:
    
                    despike : first (between tcat and tshift)
                    ricor   : just after despike (else first)
                    align   : before tlrc, before volreg
                    tlrc    : after align, before volreg
                    empty   : NO DEFAULT, cannot be applied via -do_block
    
                Any block not included in -blocks can be added via this option
                (except for 'empty').
    
                See also '-blocks', as well as the "PROCESSING BLOCKS" section of
                the -help output.
    
            -dsets dset1 dset2 ...  : (REQUIRED) specify EPI run datasets
    
                    e.g. -dsets Elvis_run1+orig Elvis_run2+orig Elvis_run3+orig
                    e.g. -dsets Elvis_run*.HEAD
    
                The user must specify the list of EPI run datasets to analyze.
                When the runs are processed, they will be written to start with
                run 1, regardless of whether the input runs were just 6, 7 and 21.
    
                Note that when using a wildcard it is essential for the EPI
                datasets to be alphabetical, as that is how the shell will list
                them on the command line.  For instance, epi_run1+orig through
                epi_run11+orig is not alphabetical.  If they were specified via
                wildcard their order would end up as run1 run10 run11 run2 ...
    
                Note also that when using a wildcard it is essential to specify
                the datasets suffix, so that the shell doesn't put both the .BRIK
                and .HEAD filenames on the command line (which would make it twice
                as many runs of data).
    
            -execute                : execute the created processing script
    
                If this option is applied, not only will the processing script be
                created, but it will then be executed in the "suggested" manner,
                such as via:
    
                    tcsh -xef proc.sb23 |& tee output.proc.sb23
    
                Note that it will actually use the bash format of the command,
                since the system command (C and therefore python) uses /bin/sh.
    
                    tcsh -xef proc.sb23 2>&1 | tee output.proc.sb23
    
            -gen_epi_review SCRIPT_NAME : specify script for EPI review
    
                    e.g. -gen_epi_review review_orig_EPI.txt
    
                By default, the proc script calls gen_epi_review.py on the original
                EPI data (from the tcat step, so only missing pre-SS TRs).  This
                creates a "drive afni" script that the user can run to quickly scan
                that EPI data for apparent issues.
    
                Without this option, the script will be called @epi_review.$subj,
                where $subj is the subject ID.
    
                The script starts afni, loads the first EPI run and starts scanning
                through time (effectively hitting 'v' in the graph window).  The
                user can press <enter> in the prompting terminal window to go to
                each successive run.
    
                Note that the user has full control over afni, aside from a new run
                being loaded whey they hit <enter>.  Recall that the <space> key
                (applied in the graph window) can terminate the 'v' (video mode).
    
                See 'gen_epi_review.py -help' for details.
                See also 'no_epi_review', to disable this feature.
    
            -no_epi_review
    
                This option is used to prevent writing a gen_epi_review.py command
                in the processing script (i.e. do not create a script to review the
                EPI data).
    
                The only clear reason to want this option is if gen_epi_review.py
                fails for some reason.  It should not hurt to create that little
                text file (@epi_review.$subj, by default).
    
                See also '-gen_epi_review'.
    
            -keep_rm_files          : do not have script delete rm.* files at end
    
                    e.g. -keep_rm_files
    
                The output script may generate temporary files in a block, which
                would be given names with prefix 'rm.'.  By default, those files
                are deleted at the end of the script.  This option blocks that
                deletion.
    
            -move_preproc_files     : move preprocessing files to preproc.data dir
    
                At the end of the output script, create a 'preproc.data' directory,
                and move most of the files there (dfile, outcount, pb*, rm*).
    
                See also -remove_preproc_files.
    
            -no_proc_command        : do not print afni_proc.py command in script
    
                    e.g. -no_proc_command
    
                If this option is applied, the command used to generate the output
                script will be stored at the end of the script.
    
            -out_dir DIR            : specify the output directory for the script
    
                    e.g. -out_dir ED_results
                    default: SUBJ.results
    
                The AFNI processing script will create this directory and perform
                all processing in it.
    
            -outlier_count yes/no   : should we count outliers with 3dToutcount?
    
                    e.g. -outlier_count no
                    default: yes
    
                By default, outlier fractions are computed per TR with 3dToutcount.
                To disable outlier counting, apply this option with parameter 'no'.
                This is a yes/no option, meaning those are the only valid inputs.
    
                Note that -outlier_count must be 'yes' in order to censor outliers
                with -regress_censor_outliers.
    
                See "3dToutcount -help" for more details.
                See also -regress_censor_outliers.
    
            -outlier_legendre yes/no : use Legendre polynomials in 3dToutcount?
    
                    e.g. -outlier_legendre no
                    default: yes
    
                By default the -legendre option is passed to 3dToutcount.  Along
                with using better behaved polynomials, it also allows them to be
                higher than 3rd order (if desired).
    
                See "3dToutcount -help" for more details.
    
            -outlier_polort POLORT  : specify polynomial baseline for 3dToutcount
    
                    e.g. -outlier_polort 3
                    default: same degree that 3dDeconvolve would use:
                             1 + floor(run_length/150)
    
                Outlier counts come after detrending the data, where the degree
                of the polynomial trend defaults to the same that 3dDeconvolve
                would use.  This option will override the default.
    
                See "3dToutcount -help" for more details.
                See "3dDeconvolve -help" for more details.
                See also '-regress_polort' and '-outlier_legendre'.
    
            -radial_correlate yes/no : correlate each voxel with local radius
    
                    e.g. -radial_correlate yes
                    default: no
    
                With this option set, @radial_correlate will be run on the
                initial EPI time series datasets.  That creates a 'corr_test'
                directory that one can review, plus potential warnings (in text)
                if large clusters of high correlations are found.
    
                (very abbreviated) method for @radial_correlate:
                    for each voxel
                       compute average time series within 20 mm radius sphere
                       correlate central voxel time series with spherical average
                    look for clusters of high correlations
    
                This is a useful quality control (QC) dataset that helps one find
                scanner artifacts, particularly including coils going bad.
    
                To visually check the results, the program text output suggests:
    
                    run command: afni corr_test.results.postdata
                    then set:    Underlay  = epi.SOMETHING
                                 Overlay   = res.SOMETHING.corr
                                 maybe threshold = 0.9, maybe clusterize
    
                See "@radial_correlate -help" for details and a list of options.
    
            -remove_preproc_files   : delete pre-processed data
    
                At the end of the output script, delete the intermediate data (to
                save disk space).  Delete dfile*, outcount*, pb* and rm*.
    
                See also -move_preproc_files.
    
            -script SCRIPT_NAME     : specify the name of the resulting script
    
                    e.g. -script ED.process.script
                    default: proc_subj
    
                The output of this program is a script file.  This option can be
                used to specify the name of that file.
    
                See also -scr_overwrite, -subj_id.
    
            -scr_overwrite          : overwrite any existing script
    
                    e.g. -scr_overwrite
    
                If the output script file already exists, it will be overwritten
                only if the user applies this option.
    
                See also -script.
    
            -sep_char CHAR          : apply as separation character in filenames
    
                    e.g. -sep_char _
                    default: .
    
                The separation character is used in many output filenames, such as
                the default '.' in:
    
                    pb04.Nancy.r07.scale+orig.BRIK
    
                If (for some crazy reason) an underscore (_) character would be
                preferable, the result would be:
    
                    pb04_Nancy_r07_scale+orig.BRIK
    
                If "-sep_char _" is applied, so is -subj_curly.
    
                See also -subj_curly.
    
            -subj_curly             : apply $subj as ${subj}
    
                The subject ID is used in dataset names is typically used without
                curly brackets (i.e. $subj).  If something is done where this would
                result in errors (e.g. "-sep_char _"), the curly brackets might be
                useful to delimit the variable (i.e. ${subj}).
    
                Note that this option is automatically applied in the case of
                "-sep_char _".
    
                See also -sep_char.
    
            -subj_id SUBJECT_ID     : specify the subject ID for the script
    
                    e.g. -subj_id elvis
                    default: SUBJ
    
                The subject ID is used in dataset names and in the output directory
                name (unless -out_dir is used).  This option allows the user to
                apply an appropriate naming convention.
    
            -test_for_dsets yes/no  : test for existence of input datasets
    
                    e.g. -test_for_dsets no
                    default: yes
    
                This options controls whether afni_proc.py check for the existence
                of input datasets.  In general, they must exist when afni_proc.py
                is run, in order to get run information (TR, #TRs, #runs, etc).
    
            -test_stim_files yes/no : evaluate stim_files for appropriateness?
    
                    e.g. -test_stim_files no
                    default: yes
    
                This options controls whether afni_proc.py evaluates the stim_files
                for validity.  By default, the program will do so.
    
                Input files are one of local stim_times, global stim_times or 1D
                formats.  Options -regress_stim_files and -regress_extra_stim_files
                imply 1D format for input files.  Otherwise, -regress_stim_times is
                assumed to imply local stim_times format (-regress_global_times
                implies global stim_times format).
    
                Checks include:
    
                    1D              : # rows equals total reps
                    local times     : # rows equal # runs
                                    : times must be >= 0.0
                                    : times per run (per row) are unique
                                    : times cannot exceed run time
                    global times    : file must be either 1 row or 1 column
                                    : times must be >= 0.0
                                    : times must be unique
                                    : times cannot exceed total duration of all runs
    
                This option provides the ability to disable this test.
    
                See "1d_tool.py -help" for details on '-look_like_*' options.
                See also -regress_stim_files, -regress_extra_stim_files,
                -regress_stim_times, -regress_local_times, -regress_global_times.
    
            -verb LEVEL             : specify the verbosity of this script
    
                    e.g. -verb 2
                    default: 1
    
                Print out extra information during execution.
    
            -write_3dD_prefix PREFIX : specify prefix for outputs from 3dd_script
    
                    e.g. -write_3dD_prefix basis.tent.
                    default: test.
    
                If a separate 3dDeconvolve command script is generated via the
                option -write_3dD_script, then the given PREFIX will be used for
                relevant output files. in the script.
    
                See also -write_3dD_script.
    
            -write_3dD_script SCRIPT : specify SCRIPT only for 3dDeconvolve command
    
                    e.g. -write_3dD_script run.3dd.tent
    
                This option is intended to be used with the EXACT same afni_proc.py
                command (aside from any -write_3dD_* options).  The purpose is to
                generate a corresponding 3dDeconvolve command script which could
                be run in the same results directory.
    
                Alternatively, little things could be changed that would only
                affect the 3dDeconvolve command in the new script, such as the
                basis function(s).
    
                The new script should include a prefix to distinguish output files
                from those created by the original proc script.
    
              * This option implies '-test_stim_files no'.
    
                See also -write_3dD_prefix, -test_stim_files.
    
            -write_ppi_3dD_scripts  : flag: write 3dD scripts for PPI analysis
    
                    e.g. -write_ppi_3dD_scripts                        \
                         -regress_ppi_stim_files PPI_*.1D some_seed.1D \
                         -regress_ppi_stim_labels PPI_A PPI_B PPI_C seed
    
                Request 3dDeconvolve scripts for pre-PPI filtering (do regression
                without censoring) and post-PPI filtering (include PPI regressors
                and seed).
    
                This is a convenience method for creating extra 3dDeconvolve
                command scripts without having to run afni_proc.py multiple times
                with different options.
    
                Using this option, afni_proc.py will create the main proc script,
                plus :
    
                   A. (if censoring was done) an uncensored 3dDeconvolve command
                      pre-PPI filter script, to create an uncensored errts time
                      series.
    
                      This script is akin to using -write_3dD_* to output a
                      regression script, along with adding -regress_skip_censor.
                      The regression command should be identical to the original
                      one, except for inclusion of 3dDeconvolve's -censor option.
    
                   B. a 3dDeconvolve post-PPI filter script to include the PPI
                      and seed regressors.
    
                      This script is akin to using -write_3dD_* to output a
                      regression script, along with passing the PPI and seed
                      regressors via -regress_extra_stim_files and _labels.
    
                Use -regress_ppi_stim_files and -regress_ppi_stim_labels to
                specify the PPI (and seed) regressors and their labels.  These
                options are currently required.
    
                See also -regress_ppi_stim_files, -regress_ppi_stim_labels.
    

------------ block options (in default block order) ------------
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

    
            These options pertain to individual processing blocks.  Each option
            starts with the block name.
    
            -tcat_preSS_warn_limit LIMIT : TR #0 outlier limit to warn of pre-SS
    
                    e.g. -tcat_preSS_warn_limit 0.7
                    default: 0.4
    
                Outlier fractions are computed across TRs in the tcat processing
                block.  If TR #0 has a large fraction, it might suggest that pre-
                steady state TRs have been included in the analysis.  If the
                detected fraction exceeds this limit, a warning will be stored
                (and output by the @ss_review_basic script).
    
                The special case of limit = 0.0 implies no check will be done.
    
            -tcat_remove_first_trs NUM : specify how many TRs to remove from runs
    
                    e.g. -tcat_remove_first_trs 3
                    e.g. -tcat_remove_first_trs 3 1 0 0 3
                    default: 0
    
                Since it takes several seconds for the magnetization to reach a
                steady state (at the beginning of each run), the initial TRs of
                each run may have values that are significantly greater than the
                later ones.  This option is used to specify how many TRs to
                remove from the beginning of every run.
    
                If the number needs to vary across runs, then one number should
                be specified per run.
    
            -tcat_remove_last_trs NUM : specify TRs to remove from run ends
    
                    e.g. -tcat_remove_last_trs 10
                    default: 0
    
                For when the user wants a simple way to shorten each run.
    
                See also -ricor_regs_rm_nlast.
    
            -despike_mask           : allow Automasking in 3dDespike
    
                By default, -nomask is applied to 3dDespike.  Since anatomical
                masks will probably not be contained within the Automask operation
                of 3dDespike (which uses methods akin to '3dAutomask -dilate 4'),
                it is left up to the user to speed up this operation via masking.
    
                Note that the only case in which this should be done is when
                applying the EPI mask to the regression.
    
                Please see '3dDespike -help' and '3dAutomask -help' for more
                information.
    
            -despike_opts_3dDes OPTS... : specify additional options for 3dDespike
    
                    e.g. -despike_opts_3dDes -nomask -ignore 2
    
                By default, 3dDespike is used with only -prefix and -nomask
                (unless -despike_mask is applied).  Any other options must be
                applied via -despike_opts_3dDes.
    
                Note that the despike block is not applied by default.  To apply
                despike in the processing script, use either '-do_block despike'
                or '-blocks ... despike ...'.
    
                Please see '3dDespike -help' for more information.
                See also '-do_blocks', '-blocks', '-despike_mask'.
    
            -ricor_datum DATUM      : specify output data type from ricor block
    
                    e.g. -ricor_datum float
    
                By default, if the input is unscaled shorts, the output will be
                unscaled shorts.  Otherwise the output will be floats.
    
                The user may override this default with the -ricor_datum option.
                Currently only 'short' and 'float' are valid parameters.
    
                Note that 3dREMLfit only outputs floats at the moment.  Recall
                that the down-side of float data is that it takes twice the disk
                space, compared with shorts (scaled or unscaled).
    
                Please see '3dREMLfit -help' for more information.
    
            -ricor_polort POLORT    : set the polynomial degree for 3dREMLfit
    
                    e.g. -ricor_polort 4
                    default: 1 + floor(run_length / 75.0)
    
                The default polynomial degree to apply during the 'ricor' block is
                similar to that of the 'regress' block, but is based on twice the
                run length (and so should be almost twice as large).  This is to
                account for motion, since volreg has typically not happened yet.
    
                Use -ricor_polort to override the default.
    
            -ricor_regress_method METHOD    : process per-run or across-runs
    
                    e.g. -ricor_regress_method across-runs
                    default: NONE: this option is required for a 'ricor' block
    
                * valid METHOD parameters: per-run, across-runs
    
                The cardiac and respiratory signals can be regressed out of each
                run separately, or out of all runs at once.  The user must choose
                the method, there is no default.
    
                See "RETROICOR NOTE" for more details about the methods.
    
            -ricor_regress_solver METHOD    : regress using OLSQ or REML
    
                    e.g. -ricor_regress_solver REML
                    default: OLSQ
    
                * valid METHOD parameters: OLSQ, REML
    
                Use this option to specify the regression method for removing the
                cardiac and respiratory signals.  The default method is ordinary
                least squares, removing the "best fit" of the card/resp signals
                from the data (also subject to the polort baseline).
    
                To apply the REML (REstricted Maximum Likelihood) method, use this
                option.
    
                Note that 3dREMLfit is used for the regression in either case,
                particularly since the regressors are slice-based (they are
                different for each slice).
    
                Please see '3dREMLfit -help' for more information.
    
            -ricor_regs REG1 REG2 ...       : specify ricor regressors (1 per run)
    
                    e.g. -ricor_regs slibase*.1D
    
                This option is required with a 'ricor' processing block.
    
                The expected format of the regressor files for RETROICOR processing
                is one file per run, where each file contains a set of regressors
                per slice.  If there are 5 runs and 27 slices, and if there are 13
                regressors per slice, then there should be 5 files input, each with
                351 (=27*13) columns.
    
                This format is based on the output of RetroTS.m, included in the
                AFNI distribution (as part of the matlab package), by Z Saad.
    
            -ricor_regs_nfirst NFIRST       : ignore the first regressor timepoints
    
                    e.g. -ricor_regs_nfirst 2
                    default: 0
    
                This option is similar to -tcat_remove_first_trs.  It is used to
                remove the first few TRs from the -ricor_regs regressor files.
    
                Since it is likely that the number of TRs in the ricor regressor
                files matches the number of TRs in the original input dataset (via
                the -dsets option), it is likely that -ricor_regs_nfirst should
                match -tcat_remove_first_trs.
    
                See also '-tcat_remove_first_trs', '-ricor_regs', '-dsets'.
    
            -ricor_regs_rm_nlast NUM : remove the last NUM TRs from each regressor
    
                    e.g. -ricor_regs_rm_nlast 10
                    default: 0
    
                For when the user wants a simple way to shorten each run.
    
                See also -tcat_remove_last_trs.
    
            -tshift_align_to TSHIFT OP : specify 3dTshift alignment option
    
                    e.g. -tshift_align_to -slice 14
                    default: -tzero 0
    
                By default, each time series is aligned to the beginning of the
                TR.  This option allows the users to change the alignment, and
                applies the option parameters directly to the 3dTshift command
                in the output script.
    
                It is likely that the user will use either '-slice SLICE_NUM' or
                '-tzero ZERO_TIME'.
    
                Note that when aligning to an offset other than the beginning of
                the TR, and when applying the -regress_stim_files option, then it
                may be necessary to also apply -regress_stim_times_offset, to
                offset timing for stimuli to later within each TR.
    
                Please see '3dTshift -help' for more information.
                See also '-regress_stim_times_offset'.
    
            -tshift_interp METHOD   : specify the interpolation method for tshift
    
                    e.g. -tshift_interp -Fourier
                    e.g. -tshift_interp -cubic
                    default -quintic
    
                Please see '3dTshift -help' for more information.
    
            -tshift_opts_ts OPTS ... : specify extra options for 3dTshift
    
                    e.g. -tshift_opts_ts -tpattern alt+z
    
                This option allows the user to add extra options to the 3dTshift
                command.  Note that only one -tshift_opts_ts should be applied,
                which may be used for multiple 3dTshift options.
    
                Please see '3dTshift -help' for more information.
    
            -blip_forward_dset      : specify a forward blip dataset
    
                    e.g. -blip_forward_dset epi_forward_blip+orig'[0..9]'
    
                Without this option, the first TRs of the first input EPI time
                series would be used as the forward blip dataset.
    
                See also -blip_revers_dset.
    
                Please see '3dQwarp -help' for more information, and the -plusminus
                option in particular.
    
            -blip_reverse_dset      : specify a reverse blip dataset
    
                    e.g. -blip_reverse_dset epi_reverse_blip+orig
                    e.g. -blip_reverse_dset epi_reverse_blip+orig'[0..9]'
    
                EPI distortion correction can be applied via blip up/blip down
                acquisitions.  Unless specified otherwise, the first TRs of the
                first run of typical EPI data specified via -dsets is considered
                to be the forward direction (blip up, say).  So only the reverse
                direction data needs separate input.
    
                Please see '3dQwarp -help' for more information, and the -plusminus
                option in particular.
    
            -blip_opts_qw OPTS ...  : specify extra options for 3dQwarp
    
                    e.g. -blip_opts_qw -noXdis -noZdis
    
                This option allows the user to add extra options to the 3dQwarp
                command specific to the 'blip' processing block.
    
                There are many options (e.g. for blurring) applied in the 3dQwarp
                command by afni_proc.py by default, so review the resulting script.
    
                Please see '3dQwarp -help' for more information.
    
            -tlrc_anat              : run @auto_tlrc on '-copy_anat' dataset
    
                    e.g. -tlrc_anat
    
                Run @auto_tlrc on the anatomical dataset provided by '-copy_anat'.
                By default, warp the anat to align with TT_N27+tlrc, unless the
                '-tlrc_base' option is given.
    
                The -copy_anat option specifies which anatomy to transform.
    
             ** Note, use of this option has the same effect as application of the
                'tlrc' block.
    
                Please see '@auto_tlrc -help' for more information.
                See also -copy_anat, -tlrc_base, -tlrc_no_ss and the 'tlrc' block.
    
            -tlrc_base BASE_DSET    : run "@auto_tlrc -base BASE_DSET"
    
                    e.g. -tlrc_base TT_icbm452+tlrc
                    default: -tlrc_base TT_N27+tlrc
    
                This option is used to supply an alternate -base dataset for
                @auto_tlrc (or auto_warp.py).  Otherwise, TT_N27+tlrc will be used.
    
                Note that the default operation of @auto_tlrc is to "skull strip"
                the input dataset.  If this is not appropriate, consider also the
                '-tlrc_no_ss' option.
    
                Please see '@auto_tlrc -help' for more information.
                See also -tlrc_anat, -tlrc_no_ss.
    
            -tlrc_NL_warp           : use non-linear for template alignment
    
                    e.g. -tlrc_NL_warp
    
                If this option is applied, then auto_warp.py is applied for the
                transformation to standard space, rather than @auto_tlrc, which in
                turn applies 3dQwarp (rather than 3dWarpDrive in @auto_tlrc).
    
                The output datasets from this operation are:
    
                    INPUT_ANAT+tlrc         : standard space version of anat
                    anat.un.aff.Xat.1D      : affine xform to standard space
                    anat.un.aff.qw_WARP.nii : non-linear xform to standard space
                                              (displacement vectors across volume)
    
                The resulting ANAT dataset is copied out of the awpy directory
                back into AFNI format, and with the original name but new view,
                while the 2 transformation files (one text file of 12 numbers, one
                3-volume dataset vectors) are moved out with the original names.
    
                If -volreg_tlrc_warp is given, then the non-linear transformation
                will also be applied to the EPI data, sending the 'volreg' output
                directly to standard space.  As usual, all transformations are
                combined so that the EPI is only resampled one time.
    
                Options can be added to auto_warp.py via -tlrc_opts_at.
    
                Consider use of -anat_uniform_method along with this option.
    
                Please see 'auto_warp.py -help' for more information.
                See also -tlrc_opts_at, -anat_uniform_method.
    
            -tlrc_NL_warped_dsets ANAT WARP.1D NL_WARP: import auto_warp.py output
    
                    e.g. -tlrc_NL_warped_dsets anat.nii           \
                                               anat.un.aff.Xat.1D \
                                               anat.un.aff.qw_WARP.nii
    
                If the user has already run auto_warp.py on the subject anatomy
                to transform (non-linear) to standard space, those datasets can
                be input to save re-processing time.
    
                They are the same 3 files that would be otherwise created by
                running auto_warp_py from the proc script.
    
                When using this option, the 'tlrc' block will be empty of actions.
    
            -tlrc_NL_awpy_rm Y/N    : specify whether to remove awpy directory
    
                    e.g.     -tlrc_NL_awpy_rm no
                    default: -tlrc_NL_awpy_rm yes
    
                The auto_warp.py program does all its work in an sub-directory
                called 'awpy', which is removed by default.  Use this option with
                'no' to save the awpy directory.
    
            -tlrc_no_ss             : add the -no_ss option to @auto_tlrc
    
                    e.g. -tlrc_no_ss
    
                This option is used to tell @auto_tlrc not to perform the skull
                strip operation.
    
                Please see '@auto_tlrc -help' for more information.
    
            -tlrc_opts_at OPTS ...   : add additional options to @auto_tlrc
    
                    e.g. -tlrc_opts_at -OK_maxite
    
                This option is used to add user-specified options to @auto_tlrc,
                specifically those afni_proc.py is not otherwise set to handle.
    
                In the case of -tlrc_NL_warp, the options will be passed to
                auto_warp.py, instead.
    
                Please see '@auto_tlrc -help' for more information.
                Please see 'auto_warp.py -help' for more information.
    
            -tlrc_rmode RMODE       : apply RMODE resampling in @auto_tlrc
    
                    e.g. -tlrc_rmode NN
    
                This option is used to apply '-rmode RMODE' in @auto_tlrc.
    
                Please see '@auto_tlrc -help' for more information.
    
            -tlrc_suffix SUFFIX     : apply SUFFIX to result of @auto_tlrc
    
                    e.g. -tlrc_suffix auto_tlrc
    
                This option is used to apply '-suffix SUFFIX' in @auto_tlrc.
    
                Please see '@auto_tlrc -help' for more information.
    
            -align_epi_ext_dset DSET : specify dset/brick for align_epi_anat EPI
    
                    e.g. -align_epi_ext_dset subj10/epi_r01+orig'[0]'
    
                This option allows the user to specify an external volume for the
                EPI base used in align_epi_anat.py in the align block.  The user
                should apply sub-brick selection if the dataset has more than one
                volume.  This volume would be used for both the -epi and the
                -epi_base options in align_epi_anat.py.
    
                The user might want to align to an EPI volume that is not in the
                processing stream in the case where there is not sufficient EPI
                contrast left after the magnetization has reached a steady state.
                Perhaps volume 0 has sufficient contrast for alignment, but is not
                appropriate for analysis.  In such a case, the user may elect to
                align to volume 0, while excluding it from the analysis as part of
                the first volumes removed in -tcat_remove_first_trs.
    
                e.g. -dsets subj10/epi_r*_orig.HEAD
                     -tcat_remove_first_trs 3
                     -align_epi_ext_dset subj10/epi_r01+orig'[0]'
                     -volreg_align_to first
    
                Note that even if the anatomy were acquired after the EPI, the user
                might still want to align the anat to the beginning of some run,
                and align all the EPIs to a time point close to that.  Since the
                anat and EPI are being forcibly aligned, it does not make such a
                big difference whether the EPI base is close in time to the anat
                acquisition.
    
                Note that this option does not affect the EPI registration base.
    
                Note that without this option, the volreg base dataset (whether
                one of the processed TRs or not) will be applied for anatomical
                alignment, assuming the align block is applied.
    
                See also -volreg_base_dset.
                Please see "align_epi_anat.py -help" for more information.
    
            -align_opts_aea OPTS ... : specify extra options for align_epi_anat.py
    
                    e.g. -align_opts_aea -cost lpc+ZZ
                    e.g. -align_opts_aea -cost lpc+ZZ -check_flip
                    e.g. -align_opts_aea -Allineate_opts -source_automask+4
                    e.g. -align_opts_aea -giant_move -AddEdge -epi_strip 3dAutomask
                    e.g. -align_opts_aea -skullstrip_opts -blur_fwhm 2
    
                This option allows the user to add extra options to the alignment
                command, align_epi_anat.py.
    
                Note that only one -align_opts_aea option should be given, with
                possibly many parameters to be passed on to align_epi_anat.py.
    
                Note the second example.  In order to pass '-source_automask+4' to
                3dAllineate, one must pass '-Allineate_opts -source_automask+4' to
                align_epi_anat.py.
    
                Similarly, the fourth example passes '-blur_fwhm 2' down through
                align_epi_anat.py to 3dSkullStrip.
    
              * The -check_flip option to align_epi_anat.py is good for evaluating
                data from external sources.  Aside from performing the typical
                registration, it will compare the final registration cost to that
                of a left/right flipped version.  If the flipped version is lower,
                one should investigate whether the axes are correctly labeled, or
                even labeled at all.
    
                Please see "align_epi_anat.py -help" for more information.
                Please see "3dAllineate -help" for more information.
    
            -align_epi_strip_method METHOD : specify EPI skull strip method in AEA
    
                    e.g. -align_epi_strip_method 3dAutomask
                    default: 3dAutomask (changed from 3dSkullStrip, 20 Aug, 2013)
    
                When align_epi_anat.py is used to align the EPI and anatomy, it
                uses 3dSkullStrip to remove non-brain tissue from the EPI dataset.
                This option can be used to specify which method to use, one of
                3dSkullStrip, 3dAutomask or None.
    
                This option assumes the 'align' processing block is used.
    
                Please see "align_epi_anat.py -help" for more information.
                Please see "3dSkullStrip -help" for more information.
                Please see "3dAutomask -help" for more information.
    
            -volreg_align_e2a       : align EPI to anatomy at volreg step
    
                This option is used to align the EPI data to match the anatomy.
                It is done by applying the inverse of the anatomy to EPI alignment
                matrix to the EPI data at the volreg step.  The 'align' processing
                block is required.
    
                At the 'align' block, the anatomy is aligned to the EPI data.
                When applying the '-volreg_align_e2a' option, the inverse of that
                a2e transformation (so now e2a) is instead applied to the EPI data.
    
                Note that this e2a transformation is catenated with the volume
                registration transformations, so that the EPI data is still only
                resampled the one time.  If the user requests -volreg_tlrc_warp,
                the +tlrc transformation will also be applied at that step in a
                single transformation.
    
                See also the 'align' block and '-volreg_tlrc_warp'.
    
            -volreg_align_to POSN   : specify the base position for volume reg
    
                    e.g. -volreg_align_to last
                    e.g. -volreg_align_to MIN_OUTLIER
                    default: third
    
                This option takes 'first', 'third', 'last' or 'MIN_OUTLIER' as a
                parameter.  It specifies whether the EPI volumes are registered to
                the first or third volume (of the first run), the last volume (of
                the last run), or the volume that is consider a minimum outlier.
                The choice of 'first' or 'third' might correspond with when the
                anatomy was acquired before the EPI data.  The choice of 'last'
                might correspond to when the anatomy was acquired after the EPI
                data.
    
                The default of 'third' was chosen to go a little farther into the
                steady state data.
    
                Note that this is done after removing any volumes in the initial
                tcat operation.
    
              * A special case is if POSN is the string MIN_OUTLIER, in which
                case the volume with the minimum outlier fraction would be used.
    
                Since anat and EPI alignment tends to work very well, the choice
                of alignment base could even be independent of when the anatomy
                was acquired, making MIN_OUTLIER a good choice.
    
                Please see '3dvolreg -help' for more information.
                See also -tcat_remove_first_trs, -volreg_base_ind and
                -volreg_base_dset.
    
            -volreg_base_dset DSET  : specify dset/sub-brick for volreg base
    
                    e.g. -volreg_base_dset subj10/vreg_base+orig'[0]'
                    e.g. -volreg_base_dset MIN_OUTLIER
    
                This option allows the user to specify an external dataset for the
                volreg base.  The user should apply sub-brick selection if the
                dataset has more than one volume.
    
                For example, one might align to a pre-magnetic steady state volume.
    
                Note that unless -align_epi_ext_dset is also applied, this volume
                will be used for anatomical to EPI alignment (assuming that is
                being done at all).
    
              * A special case is if DSET is the string MIN_OUTLIER, in which
                case the volume with the minimum outlier fraction would be used.
    
                See also -align_epi_ext_dset, -volreg_align_to and -volreg_base_ind.
    
            -volreg_base_ind RUN SUB : specify run/sub-brick indices for base
    
                    e.g. -volreg_base_ind 10 123
                    default: 0 0
    
                This option allows the user to specify exactly which dataset and
                sub-brick to use as the base registration image.  Note that the
                SUB index applies AFTER the removal of pre-steady state images.
    
              * The RUN number is 1-based, matching the run list in the output
                shell script.  The SUB index is 0-based, matching the sub-brick of
                EPI time series #RUN.  Yes, one is 1-based, the other is 0-based.
                Life is hard.
    
                The user can apply only one of the -volreg_align_to and
                -volreg_base_ind options.
    
                See also -volreg_align_to, -tcat_remove_first_trs and
                -volreg_base_dset.
    
            -volreg_get_allcostX yes/no : compute all anat/EPI costs
    
                    e.g. -volreg_get_allcostX no
                    default: yes
    
                By default, given the final anatomical dataset (anat_final) and
                the the final EPI volreg base (final_epi), this option can be used
                to compute alignment costs between the two volumes across all cost
                functions from 3dAllineate.  Effectively, it will add the following
                to the proc script:
    
                    3dAllineate -base FINAL_EPI -input FINAL_ANAT -allcostX
    
                 The text output is stored in the file out.allcostX.txt.
    
                 This operation is informational only, to help evaluate alignment
                 costs across subjects.
    
                 Please see '3dAllineate -help' for more details.
    
            -volreg_compute_tsnr yes/no : compute TSNR datasets from volreg output
    
                    e.g. -volreg_compute_tsnr yes
                    default: no
    
                Use this option to compute a temporal signal to noise (TSNR)
                dataset at the end of the volreg block.  Both the signal and noise
                datasets are from the run 1 output, where the "signal" is the mean
                and the "noise" is the detrended time series.
    
                TSNR = average(signal) / stdev(noise)
    
                See also -regress_compute_tsnr.
    
            -volreg_interp METHOD   : specify the interpolation method for volreg
    
                    e.g. -volreg_interp -quintic
                    e.g. -volreg_interp -Fourier
                    default: -cubic
    
                Please see '3dvolreg -help' for more information.
    
            -volreg_motsim          : generate motion simulated time series
    
                Use of this option will result in a 'motsim' (motion simulation)
                time series dataset that is akin to an EPI dataset altered only
                by motion and registration (no BOLD, no signal drift, etc).
    
                This dataset can be used to generate regressors of no interest to
                be used in the regression block.
    
                rcr - note relevant options once they are in
    
                Please see '@simulate_motion -help' for more information.
    
            -volreg_opts_ms OPTS ... : specify extra options for @simulate_motion
    
                    e.g. -volreg_opts_ms -save_workdir
    
                This option can be used to pass extra options directly to the
                @simulate_motion command.
    
                See also -volreg_motsim.
                Please see '@simulate_motion -help' for more information.
    
            -volreg_opts_vr OPTS ... : specify extra options for 3dvolreg
    
                    e.g. -volreg_opts_vr -twopass
                    e.g. -volreg_opts_vr -noclip -nomaxdisp
    
                This option allows the user to add extra options to the 3dvolreg
                command.  Note that only one -volreg_opts_vr should be applied,
                which may be used for multiple 3dvolreg options.
    
                Please see '3dvolreg -help' for more information.
    
            -volreg_no_extent_mask  : do not create and apply extents mask
    
                    default: apply extents mask
    
                This option says not to create or apply the extents mask.
    
                The extents mask:
    
                When EPI data is transformed to the anatomical grid in either orig
                or tlrc space (i.e. if -volreg_align_e2a or -volreg_tlrc_warp is
                applied), then the complete EPI volume will only cover part of the
                resulting volume space.  Worse than that, the coverage will vary
                over time, as motion will alter the final transformation (remember
                that volreg, EPI->anat and ->tlrc transformations are all combined,
                to prevent multiple resampling steps).  The result is that edge
                voxels will sometimes have valid data and sometimes not.
    
                The extents mask is made from an all-1 dataset that is warped with
                the same per-TR transformations as the EPI data.  The intersection
                of the result is the extents mask, so that every voxel in the
                extents mask has data at every time point.  Voxels that are not
                are missing data from some or all TRs.
    
                It is called the extents mask because it defines the 'bounding box'
                of valid EPI data.  It is not quite a tiled box though, as motion
                changes the location slightly, per TR.
    
                See also -volreg_align_e2a, -volreg_tlrc_warp.
                See also the 'extents' mask, in the "MASKING NOTE" section above.
    
            -volreg_regress_per_run : regress motion parameters from each run
    
                === This option has been replaced by -regress_motion_per_run. ===
    
            -volreg_tlrc_adwarp     : warp EPI to +tlrc space at end of volreg step
    
                    default: stay in +orig space
    
                With this option, the EPI data will be warped to standard space
                (via adwarp) at the end of the volreg processing block.  Further
                processing through regression will be done in standard space.
    
                This option is useful for applying a manual Talairach transform,
                which does not work with -volreg_tlrc_warp.  To apply one from
                @auto_tlrc, -volreg_tlrc_warp is recommended.
    
                The resulting voxel grid is the minimum dimension, truncated to 3
                significant bits.  See -volreg_warp_dxyz for details.
    
                Note: this step requires a transformed anatomy, which can come from
                the -tlrc_anat option or from -copy_anat importing an existing one.
    
                Please see 'WARP TO TLRC NOTE' above, for additional details.
                See also -volreg_tlrc_warp, -volreg_warp_dxyz, -tlrc_anat,
                -copy_anat.
    
            -volreg_tlrc_warp       : warp EPI to +tlrc space at volreg step
    
                    default: stay in +orig space
    
                With this option, the EPI data will be warped to standard space
                in the volreg processing block.  All further processing through
                regression will be done in standard space.
    
                Warping is done with volreg to apply both the volreg and tlrc
                transformations in a single step (so a single interpolation of the
                EPI data).  The volreg transformations (for each volume) are stored
                and multiplied by the +tlrc transformation, while the volume
                registered EPI data is promptly ignored.
    
                The volreg/tlrc (affine or non-linear) transformation is then
                applied as a single concatenated warp to the unregistered data.
    
                Note that the transformation concatenation is not possible when
                using the 12-piece manual transformation (see -volreg_tlrc_adwarp
                for details).
    
                The resulting voxel grid is the minimum dimension, truncated to 3
                significant bits.  See -volreg_warp_dxyz for details.
    
                Note: this step requires a transformed anatomy, which can come from
                the -tlrc_anat option or from -copy_anat importing an existing one.
    
                Please see 'WARP TO TLRC NOTE' above, for additional details.
                See also -volreg_tlrc_adwarp, -volreg_warp_dxyz, -tlrc_anat,
                -copy_anat.
    
            -volreg_warp_dxyz DXYZ  : grid dimensions for _align_e2a or _tlrc_warp
    
                    e.g. -volreg_warp_dxyz 3.5
                    default: min dim truncated to 3 significant bits
                             (see description, below)
    
                This option allows the user to specify the grid size for output
                datasets from the -volreg_tlrc_warp and -volreg_align_e2a options.
                In either case, the output grid will be isotropic voxels (cubes).
    
                By default, DXYZ is the minimum input dimension, truncated to
                3 significant bits (for integers, starts affecting them at 9, as
                9 requires 4 bits to represent).
    
                Some examples:
                    ----------------------------  (integer range, so >= 4)
                    8.00   ...  9.99   --> 8.0
                    4.00   ...  4.99   --> 4.0
                    ----------------------------  (3 significant bits)
                    2.50   ...  2.99   --> 2.5
                    2.00   ...  2.49   --> 2.0
                    1.75   ...  1.99   --> 1.75
                    1.50   ...  1.74   --> 1.5
                    1.25   ...  1.49   --> 1.25
                    1.00   ...  1.24   --> 1.0
                    0.875  ...  0.99   --> 0.875
                    0.75   ...  0.874  --> 0.75
                    0.625  ...  0.74   --> 0.625
                    0.50   ...  0.624  --> 0.50
                    0.4375 ...  0.49   --> 0.4375
                    0.375  ...  0.4374 --> 0.375
    
            -volreg_zpad N_SLICES   : specify number of slices for -zpad
    
                    e.g. -volreg_zpad 4
                    default: -volreg_zpad 1
    
                This option allows the user to specify the number of slices applied
                via the -zpad option to 3dvolreg.
    
            -surf_anat ANAT_DSET    : specify surface volume dataset
    
                    e.g. -surf_anat SUMA/sb23_surf_SurfVol+orig
    
                This option is required in order to do surface-based analysis.
    
                This volumetric dataset should be the one used for generation of
                the surface (and therefore should be in perfect alignment).  It may
                be output by the surface generation software.
    
                Unless specified by the user, the processing script will register
                this anatomy with the current anatomy.
    
                Use -surf_anat_aligned if the surf_anat is already aligned with the
                current experiment.
    
                Use '-surf_anat_has_skull no' if the surf_anat has already been
                skull stripped.
    
                Please see '@SUMA_AlignToExperiment -help' for more details.
                See also -surf_anat_aligned, -surf_anat_has_skull.
                See example #8 for typical usage.
    
            -surf_spec spec1 [spec2]: specify surface specificatin file(s)
    
                    e.g. -surf_spec SUMA/sb23_?h_141_std.spec
    
                Use this option to provide either 1 or 2 spec files for surface
                analysis.  Each file must have lh or rh in the name (to encode
                the hemisphere), and that can be their only difference.  So if
                the files do not have such a naming pattern, they should probably
                be copied to new files that do.  For example, consider the spec
                files included with the AFNI_data4 sample data:
    
                    SUMA/sb23_lh_141_std.spec
                    SUMA/sb23_rh_141_std.spec
    
            -surf_A surface_A       : specify first surface for mapping
    
                    e.g. -surf_A smoothwm
                    default: -surf_A smoothwm
    
                This option allows the user to specify the first (usually inner)
                surface for use when mapping from the volume and for blurring.
                If the option is not given, the smoothwm surface will be assumed.
    
            -surf_B surface_B       : specify second surface for mapping
    
                    e.g. -surf_B pial
                    default: -surf_B pial
    
                This option allows the user to specify the second (usually outer)
                surface for use when mapping from the volume (not for blurring).
                If the option is not given, the pial surface will be assumed.
    
            -surf_blur_fwhm FWHM    :  NO LONGER VALID
    
                Please use -blur_size, instead.
    
            -blur_filter FILTER     : specify 3dmerge filter option
    
                    e.g. -blur_filter -1blur_rms
                    default: -1blur_fwhm
    
                This option allows the user to specify the filter option from
                3dmerge.  Note that only the filter option is set here, not the
                filter size.  The two parts were separated so that users might
                generally worry only about the filter size.
    
                Please see '3dmerge -help' for more information.
                See also -blur_size.
    
            -blur_in_automask       : apply 3dBlurInMask -automask
    
                This option forces use of 3dBlurInMask -automask, regardless of
                whether other masks exist and are being applied.
    
                Note that one would not want to apply -automask via -blur_opts_BIM,
                as that might result in failure because of multiple -mask options.
    
                Note that -blur_in_automask implies '-blur_in_mask yes'.
    
                Please see '3dBlurInMask -help' for more information.
                See also -blur_in_mask, -blur_opts_BIM.
    
            -blur_in_mask yes/no    : specify whether to restrict blur to a mask
    
                    e.g. -blur_in_mask yes
                    default: no
    
                This option allows the user to specify whether to use 3dBlurInMask
                instead of 3dmerge for blurring.
    
                Note that the algorithms are a little different, and 3dmerge comes
                out a little more blurred.
    
                Note that 3dBlurInMask uses only FWHM kernel size units, so the
                -blur_filter should be either -1blur_fwhm or -FWHM.
    
                Please see '3dBlurInMask -help' for more information.
                Please see '3dmerge -help' for more information.
                See also -blur_filter.
    
            -blur_opts_BIM OPTS ...  : specify extra options for 3dBlurInMask
    
                    e.g. -blur_opts_BIM -automask
    
                This option allows the user to add extra options to the 3dBlurInMask
                command.  Only one -blur_opts_BIM should be applied, which may be
                used for multiple 3dBlurInMask options.
    
                This option is only useful when '-blur_in_mask yes' is applied.
    
                Please see '3dBlurInMask -help' for more information.
                See also -blur_in_mask.
    
            -blur_opts_merge OPTS ... : specify extra options for 3dmerge
    
                    e.g. -blur_opts_merge -2clip -20 50
    
                This option allows the user to add extra options to the 3dmerge
                command.  Note that only one -blur_opts_merge should be applied,
                which may be used for multiple 3dmerge options.
    
                Please see '3dmerge -help' for more information.
    
            -blur_size SIZE_MM      : specify the size, in millimeters
    
                    e.g. -blur_size 6.0
                    default: 4
    
                This option allows the user to specify the size of the blur used
                by 3dmerge (or another applied smoothing program).  It is applied
                as the 'bmm' parameter in the filter option (such as -1blur_fwhm)
                in 3dmerge.
    
                Note the relationship between blur sizes, as used in 3dmerge:
    
                    sigma = 0.57735027 * rms = 0.42466090 * fwhm
                    (implying fwhm = 1.359556 * rms)
    
                Programs 3dmerge and 3dBlurInMask apply -blur_size as an additional
                gaussian blur.  Therefore smoothing estimates should be computed
                per subject for the correction for multiple comparisons.
    
                Programs 3dBlurToFWHM and SurfSmooth apply -blur_size as the
                resulting blur, and so do not requre blur estimation.
    
                Please see '3dmerge -help'      for more information.
                Please see '3dBlurInMask -help' for more information.
                Please see '3dBlurToFWHM -help' for more information.
                Please see 'SurfSmooth -help'   for more information.
                See also -blur_filter.
    
            -blur_to_fwhm           : blur TO the blur size (not add a blur size)
    
                This option changes the program used to blur the data.  Instead of
                using 3dmerge, this applies 3dBlurToFWHM.  So instead of adding a
                blur of size -blur_size (with 3dmerge), the data is blurred TO the
                FWHM of the -blur_size.
    
                Note that 3dBlurToFWHM should be run with a mask.  So either:
                    o  put the 'mask' block before the 'blur' block, or
                    o  use -blur_in_automask
                It is not appropriate to include non-brain in the blur estimate.
    
                Note that extra options can be added via -blur_opts_B2FW.
    
                Please see '3dBlurToFWHM -help' for more information.
                See also -blur_size, -blur_in_automask, -blur_opts_B2FW.
    
            -blur_opts_B2FW OPTS ... : specify extra options for 3dBlurToFWHM
    
                    e.g. -blur_opts_B2FW -rate 0.2 -temper
    
                This allows the user to add extra options to the 3dBlurToFWHM
                command.  Note that only one -blur_opts_B2FW should be applied,
                which may be used for multiple 3dBlurToFWHM options.
    
                Please see '3dBlurToFWHM -help' for more information.
    
            -mask_apply TYPE        : specify which mask to apply in regression
    
                    e.g. -mask_apply group
    
                If possible, masks will be made for the EPI data, the subject
                anatomy, the group anatomy and EPI warp extents.  This option is
                used to specify which of those masks to apply to the regression.
    
                Valid choices: epi, anat, group, extents.
    
                A subject 'anat' mask will be created if the EPI anat anatomy are
                aligned, or if the EPI data is warped to standard space via the
                anat transformation.  In any case, a skull-stripped anat will exist.
    
                A 'group' anat mask will be created if the 'tlrc' block is used
                (via the -block or -tlrc_anat options).  In such a case, the anat
                template will be made into a binary mask.
    
                This option makes -regress_apply_mask obsolete.
    
                See "MASKING NOTE" and "DEFAULTS" for details.
                See also -blocks.
    
            -mask_dilate NUM_VOXELS : specify the automask dilation
    
                    e.g. -mask_dilate 3
                    default: 1
    
                By default, the masks generated from the EPI data are dilated by
                1 step (voxel), via the -dilate option in 3dAutomask.  With this
                option, the user may specify the dilation.  Valid integers must
                be at least zero.
    
                Note that 3dAutomask dilation is a little different from the
                natural voxel-neighbor dilation.
    
                Please see '3dAutomask -help' for more information.
                See also -mask_type.
    
            -mask_import LABEL MSET : import a final grid mask with the given label
    
                    e.g. -mask_import Tvent template_ventricle_3mm+tlrc
    
                Use this option to import a mask that is aligned with the final
                EPI data _and_ is on the final grid.
    
                    o  this might be based on the group template
                    o  this should already be resampled appropriately
                    o  no warping or resampling will be done to this dataset
    
                This mask can be applied via LABEL as other masks, using options
                like: -regress_ROI, -regress_ROI_PC, -regress_make_corr_vols,
                      -regress_anaticor_label, -mask_intersect, -mask_union.
    
                For example, one might import a ventricle mask from the template,
                intersect it with the subject specific CSFe (eroded CSF) mask,
                and possibly take the union with WMe (eroded white matter), before
                using the result for principle component regression, as in:
    
                    -mask_import Tvent template_ventricle_3mm+tlrc \
                    -mask_intersect Svent CSFe Tvent               \
                    -mask_union WM_vent Svent WMe                  \
                    -regress_ROI_PC WM_vent 3                      \
    
                See also -regress_ROI, -regress_ROI_PC, -regress_make_corr_vols,
                         -regress_anaticor_label, -mask_intersect, -mask_union.
    
            -mask_intersect NEW_LABEL MASK_A MASK_B : intersect 2 masks
    
                    e.g. -mask_intersect Svent CSFe Tvent
    
                Use this option to intersect 2 known masks to create a new mask.
                NEW_LABEL will be the label of the result, while MASK_A and MASK_B
                should be labels for existing masks.
    
                One could use this to intersect a template ventricle mask with each
                subject's specific CSFe (eroded CSF) mask from 3dSeg, for example.
    
                See -mask_import for more details.
    
            -mask_union NEW_LABEL MASK_A MASK_B : take union of 2 masks
    
                    e.g. -mask_union WM_vent Svent WMe
    
                Use this option to take the union of 2 known masks to create a new
                mask.  NEW_LABEL will be the label of the result, while MASK_A and
                MASK_B should be labels for existing masks.
    
                One could use this to create union of CSFe and WMe for principle
                component regression, for example.
    
                See -mask_import for more details.
    
            -mask_rm_segsy Y/N  : choose whether to delete the Segsy directory
    
                    e.g. -mask_rm_segsy no
                    default: yes
    
                This option is a companion to -mask_segment_anat.
    
                In the case of running 3dSeg to segment the anatomy, a resulting
                Segsy directory is created.  Since the main result is a Classes
                dataset, and to save disk space, the Segsy directory is removed
                by default.  Use this option to preserve it.
    
                See also -mask_segment_anat.
    
            -mask_segment_anat Y/N  : choose whether to segment anatomy
    
                    e.g. -mask_segment_anat yes
                    default: no (if anat_final is skull-stripped)
    
                This option controls whether 3dSeg is run to segment the anatomical
                dataset.  Such a segmentation would then be resampled to match the
                grid of the EPI data.
    
                When this is run, 3dSeg creates the Classes dataset, which is a
                composition mask of the GM/WM/CSF (gray matter, white matter and
                cerebral spinal fluid) regions.  Then 3dresample is used to create
                Classes_resam, the same mask but at the resolution of the EPI.
    
                Such a dataset might have multiple uses, such as tissue-based
                regression.  Note that for such a use, the ROI time series should
                come from the volreg data, before any blur.
    
              * Mask labels created by -mask_segment_anat and -mask_segment_erode
                can be applied with -regress_ROI and -regress_ROI_PC.
    
              * The CSF mask is of ALL CSF (not just in the ventricles), and is
                therefore not very appropriate to use with tissue-based regression.
    
                Consider use of -anat_uniform_method along with this option.
    
                Please see '3dSeg -help' for more information.
                Please see '3dUnifize -help' for more information.
                See also -mask_rm_segsy, -anat_uniform_method -mask_segment_erode,
                 and -regress_ROI, -regress_ROI_PC.
    
            -mask_segment_erode Y/N
    
                    e.g. -mask_segment_erode Yes
                    default: yes (if -regress_ROI or -regress_anaticor)
    
                This option is a companion to -mask_segment_anat.
    
                Anatomical segmentation is used to create GM (gray matter), WM
                (white matter) and CSF masks.  When the _erode option is applied,
                eroded versions of those masks are created via 3dmask_tool.
    
                See also -mask_segment_anat, -regress_anaticor.
                Please see '3dmask_tool -help' for more information.
    
            -mask_test_overlap Y/N  : choose whether to test anat/EPI mask overlap
    
                    e.g. -mask_test_overlap No
                    default: Yes
    
                If the subject anatomy and EPI masks are computed, then the default
                operation is to run 3dABoverlap to evaluate the overlap between the
                two masks.  Output is saved in a text file.
    
                This option allows one to disable such functionality.
    
                Please see '3dABoverlap -help' for more information.
    
            -mask_type TYPE         : specify 'union' or 'intersection' mask type
    
                    e.g. -mask_type intersection
                    default: union
    
                This option is used to specify whether the mask applied to the
                analysis is the union of masks from each run, or the intersection.
                The only valid values for TYPE are 'union' and 'intersection'.
    
                This is not how to specify whether a mask is created, that is
                done via the 'mask' block with the '-blocks' option.
    
                Please see '3dAutomask -help', '3dMean -help' or '3dcalc -help'.
                See also -mask_dilate, -blocks.
    
            -scale_max_val MAX      : specify the maximum value for scaled data
    
                    e.g. -scale_max_val 1000
                    default 200
    
                The scale step multiples the time series for each voxel by a
                scalar so that the mean for that particular run is 100 (allowing
                interpretation of EPI values as a percentage of the mean).
    
                Values of 200 represent a 100% change above the mean, and so can
                probably be considered garbage (or the voxel can be considered
                non-brain).  The output values are limited so as not to sacrifice
                the precision of the values of short datasets.  Note that in a
                short (2-byte integer) dataset, a large range of values means
                bits of accuracy are lost for the representation.
    
                No max will be applied if MAX is <= 100.
    
                Please see 'DATASET TYPES' in the output of '3dcalc -help'.
                See also -scale_no_max.
    
            -scale_no_max           : do not apply a limit to the scaled values
    
                The default limit for scaled data is 200.  Use of this option will
                remove any limit from being applied.
    
                A limit on the scaled data is highly encouraged when working with
                'short' integer data, especially when not applying a mask.
    
                See also -scale_max_val.
    
            -regress_3dD_stop       : 3dDeconvolve should stop after X-matrix gen
    
                Use this option to tell 3dDeconvolve to stop after generating the
                X-matrix (via -x1D_stop).  This is useful if the user only wishes
                to run the regression through 3dREMLfit.
    
                See also -regress_reml_exec.
    
            -regress_anaticor       : generate errts using ANATICOR method
    
                Apply the ANATICOR method of HJ Jo, regressing out the WMeLocal
                time series, which varies across voxels.
    
                WMeLocal is the average time series from all voxels within 45 mm
                which are in the eroded white matter mask.
    
                The script will run the standard regression via 3dDeconvolve (or
                stop after setting up the X-matrix, if the user says to), and use
                that X-matrix, possibly censored, in 3dTproject.  The WMeLocal time
                series is applied along with the X-matrix to get the result.
    
                Note that other 4-D time series might be regressed out via the
                3dTproject step, as well.
    
                In the case of task-based ANATICOR, -regress_reml_exec is required,
                which uses 3dREMLfit to regress the voxel-wise ANATICOR regressors.
    
                This option implies -mask_segment_anat and -mask_segment_erode.
    
              * Consider use of -regress_anaticor_fast, instead.
    
                Please see "@ANATICOR -help" for more detail, including the paper
                reference for the method.
                See also -mask_segment_anat, -mask_segment_erode, -regress_3dD_stop.
                See also -regress_reml_exec.
    
            -regress_anaticor_label LABEL : specify LABEL for ANATICOR ROI
    
                To go with either -regress_anaticor or -regress_anaticor_fast,
                this option is used the specifiy an alternate label of an ROI
                mask to be used in the ANATICOR step.  The default LABEL is WMe
                (eroded white matter from 3dSeg).
    
                When this option is included, it is up to the user to make sure
                afni_proc.py has such a label, either by including options:
                    -mask_segment_anat (and possibly -mask_segment_erode),
                    -regress_ROI_PC, -regress_ROI, or -anat_follower_ROI.
    
                Any known label made via those options may be used.
    
                See also -mask_segment_anat, -mask_segment_erode, -regress_ROI_PC,
                    -anat_follower_ROI.
    
            -regress_anaticor_radius RADIUS : specify RADIUS for 3dLocalstat
    
                To go with -regress_anaticor, use this option to specify the radius
                of spheres within which local white matter is averaged.  A small
                radius means the white matter is more local.  It is also faster.
    
                If no white matter is found within the specified distance of some
                voxel, the effect is that ANATICOR will simply not happen at that
                voxel.  That is a reasonable "failure" case, in that it says there
                is simply no white matter close enough to regress out (again, at
                the given voxel).
    
                See also -regress_anaticor.
    
            -regress_anaticor_fast  : generate errts using fast ANATICOR method
    
                This applies basically the same method as with -regress_anaticor,
                above.  While -regress_anaticor creates WMeLocal dataset by
                getting the average white matter voxel within a fixed radius, the
                'fast' method computes it by instead integrating the white matter
                over a gaussian curve.
    
                There some basic effects of using the 'fast' method:
    
                    1. Using a Gaussian curve to compute each voxel-wise regressor
                       gives more weight to the white matter that is closest to
                       each given voxel.  The FWHM of this 3D kernel is specified
                       by -regress_anaticor_fwhm, with a default of 30 mm.
    
                    2. If there is no close white matter (e.g. due to a poor
                       segmentation), the Gaussian curve will likely find white
                       matter far away, instead of creating an empty regressor.
    
                    3. This is quite a bit faster, because it is done by creating
                       a time series of all desired white matter voxels, blurring
                       it, and then just regressing out that dataset.  The blur
                       operation is much faster than a localstat one.
    
                Please see "@ANATICOR -help" for more detail, including the paper
                reference for the method.
                See also -regress_anaticor_fwhm/
                See also -mask_segment_anat, -mask_segment_erode, -regress_3dD_stop.
                See also -regress_anaticor.
    
            -regress_anaticor_fwhm FWHM  : specify FWHM for 'fast' ANATICOR, in mm
    
                    e.g.     -regress_anaticor_fwhm 20
                    default: -regress_anaticor_fwhm 30
    
                This option applies to -regress_anaticor_fast.
    
                The 'fast' ANATICOR method blurs the time series of desired white
                matter voxels using a Gaussian kernel with the given FWHM (full
                width at half maximum).
    
                To understand the FWHM, note that it is essentially the diameter of
                a sphere where the contribution from points at that distance
                (FWHM/2) contribute half as much as the center point.  For example,
                if FWHM=10mm, then any voxel at a distance of 5 mm would contribute
                half as much as a voxel at the center of the kernel.
    
                See also -regress_anaticor_fast.
    
            -regress_apply_mask     : apply the mask during scaling and regression
    
                By default, any created union mask is not applied to the analysis.
                Use this option to apply it.
    
             ** This option is essentially obsolete.  Please consider -mask_apply
                as a preferable option to choose which mask to apply.
    
                See "MASKING NOTE" and "DEFAULTS" for details.
                See also -blocks, -mask_apply.
    
            -regress_apply_mot_types TYPE1 ... : specify motion regressors
    
                    e.g. -regress_apply_mot_types basic
                    e.g. -regress_apply_mot_types deriv
                    e.g. -regress_apply_mot_types demean deriv
                    default: demean
    
                By default, the motion parameters from 3dvolreg are applied in the
                regression, but after first removing the mean, per run.  This is
                the application of the 'demean' regressors.
    
                This option gives the ability to choose a combination of:
    
                    basic:  dfile_rall.1D - the parameters straight from 3dvolreg
                            (or an external motion file, see -regress_motion_file)
                    demean: 'basic' params with the mean removed, per run
                    deriv:  per-run derivative of 'basic' params (de-meaned)
    
             ** Note that basic and demean cannot both be used, as they would cause
                multi-collinearity with the constant drift parameters.
    
             ** Note also that basic and demean will give the same results, except
                for the betas of the constant drift parameters (and subject to
                computational precision).
    
             ** A small side effect of de-meaning motion parameters is that the
                constant drift terms should evaluate to the mean baseline.
    
                See also -regress_motion_file, -regress_no_motion_demean,
                -regress_no_motion_deriv, -regress_no_motion.
    
            -regress_apply_ricor yes/no : apply ricor regs in final regression
    
                    e.g.     -regress_apply_ricor yes
                    default: no
    
                This is from a change in the default behavior 30 Jan 2012.  Prior
                to then, the 13 (?) ricor regressors from slice 0 would be applied
                in the final regression (mostly accounting for degrees of freedom).
                But since resting state analysis relies on a subsequent correlation
                analysis, it seems cleaner not to regress them (a second time).
    
            -regress_bandpass lowf highf : bandpass the frequency range
    
                    e.g.  -regress_bandpass 0.01 0.1
    
                This option is intended for use in resting state analysis.
    
                Use this option to perform bandpass filtering during the linear
                regression.  While such an operation is slow (much slower than the
                FFT using 3dBandpass), doing it during the regression allows one to
                perform (e.g. motion) censoring at the same time.
    
                This option has a similar effect to running 3dBandpass, e.g. the
                example of '-regress_bandpass 0.01 0.1' is akin to running:
    
                    3dBandpass -ort motion.1D -band 0.01 0.1
    
                except that it is done in 3dDeconvolve using linear regression.
                And censoring is easy in the context of regression.
    
                Note that the Nyquist frequency is 0.5/TR.  That means that if the
                TR were >= 5 seconds, there would be no frequencies within the band
                range of 0.01 to 0.1 to filter.  So there is no point to such an
                operation.
    
                On the flip side, if the TR is 1.0 second or shorter, the range of
                0.01 to 0.1 would remove about 80% of the degrees of freedom (since
                everything above 0.1 is filtered/removed, up through 0.5).  This
                might result in a model that is overfit, where there are almost as
                many (or worse, more) regressors than time points to fit.
    
                So a 0.01 to 0.1 bandpass filter might make the most sense for a
                TR in [2.0, 3.0], or so.
    
                A different filter range would affect this, of course.
    
                See also -regress_censor_motion.
    
            -regress_basis BASIS    : specify the regression basis function
    
                    e.g. -regress_basis 'BLOCK(4,1)'
                    e.g. -regress_basis 'BLOCK(5)'
                    e.g. -regress_basis 'TENT(0,14,8)'
                    default: GAM
    
                This option is used to set the basis function used by 3dDeconvolve
                in the regression step.  This basis function will be applied to
                all user-supplied regressors (please let me know if there is need
                to apply different basis functions to different regressors).
    
             ** Note that use of dmBLOCK requires -stim_times_AM1 (or AM2).  So
                consider option -regress_stim_types.
    
             ** If using -regress_stim_types 'file' for a particular regressor,
                the basis function will be ignored.  In such a case, it is safest
                to use 'NONE' for the corresponding basis function.
    
                Please see '3dDeconvolve -help' for more information, or the link:
                    https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
                See also -regress_basis_normall, -regress_stim_times,
                         -regress_stim_types, -regress_basis_multi.
    
            -regress_basis_multi BASIS BASIS .. : specify multiple basis functions
    
                    e.g. -regress_basis_multi 'BLOCK(30,1)' 'TENT(0,45,16)' \
                                              'BLOCK(30,1)' dmUBLOCK
    
                In the case that basis functions vary across stim classes, use
                this option to list a basis function for each class.  The given
                basis functions should correspond to the listed -regress_stim_times
                files, just as the -regress_stim_labels entries do.
    
                See also -regress_basis.
    
            -regress_basis_normall NORM : specify the magnitude of basis functions
    
                    e.g. -regress_basis_normall 1.0
    
                This option is used to set the '-basis_normall' parameter in
                3dDeconvolve.  It specifies the height of each basis function.
    
                For the example basis functions, -basis_normall is not recommended.
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_basis.
    
            -regress_censor_extern CENSOR.1D : supply an external censor file
    
                    e.g. -regress_censor_extern censor_bad_trs.1D
    
                This option is used to provide an initial censor file, if there
                is some censoring that is desired beyond the automated motion and
                outlier censoring.
    
                Any additional censoring (motion or outliers) will be combined.
    
                 See also -regress_censor_motion, -regress_censor_outliers.
    
            -regress_censor_motion LIMIT : censor TRs with excessive motion
    
                    e.g. -regress_censor_motion 0.3
    
                This option is used to censor TRs where the subject moved too much.
                "Too much" is decided by taking the derivative of the motion
                parameters (ignoring shifts between runs) and the sqrt(sum squares)
                per TR.  If this Euclidean Norm exceeds the given LIMIT, the TR
                will be censored.
    
                This option will result in the creation of 3 censor files:
    
                    motion_$subj_censor.1D
                    motion_$subj_CENSORTR.txt
                    motion_$subj_enorm.1D
    
                motion_$subj_censor.1D is a 0/1 columnar file to be applied to
                3dDeconvolve via -censor.  A row with a 1 means to include that TR,
                while a 0 means to exclude (censor) it.
    
                motion_$subj_CENSORTR.txt is a short text file listing censored
                TRs, suitable for use with the -CENSORTR option in 3dDeconvolve.
                The -censor option is the one applied however, so this file is not
                used, but may be preferable for users to have a quick peek at.
    
                motion_$subj_enorm.1D is the time series that the LIMIT is applied
                to in deciding which TRs to censor.  It is the Euclidean norm of
                the derivatives of the motion parameters.  Plotting this will give
                users a visual indication of why TRs were censored.
    
                By default, the TR prior to the large motion derivative will also
                be censored.  To turn off that behavior, use -regress_censor_prev
                with parameter 'no'.
    
                If censoring the first few TRs from each run is also necessary,
                use -regress_censor_first_trs.
    
                Please see '1d_tool.py -help' for information on censoring motion.
                See also -regress_censor_prev and -regress_censor_first_trs.
    
            -regress_censor_first_trs N  : censor the first N TRs in each run
    
                    e.g.     -regress_censor_first_trs 3
                    default: N = 0
    
                If, for example, censoring the first 3 TRs per run is desired, a
                user might add "-CENSORTR '*:0-2'" to the -regress_opts_3dD option.
                However, when using -regress_censor_motion, these censoring options
                must be combined into one for 3dDeconvolve.
    
                The -regress_censor_first_trs censors those TRs along with any with
                large motion.
    
                See '-censor_first_trs' under '1d_tool.py -help' for details.
                See also '-regress_censor_motion'.
    
            -regress_censor_prev yes/no  : censor TRs preceding large motion
    
                    default: -regress_censor_prev yes
    
                Since motion spans two TRs, the derivative is not quite enough
                information to decide whether it is more appropriate to censor
                the earlier or later TR.  To error on the safe side, many users
                choose to censor both.
    
                Use this option to specify whether to include the previous TR
                when censoring.
    
                By default this option is applied as 'yes'.  Users may elect not
                not to censor the previous TRs by setting this to 'no'.
    
                See also -regress_censor_motion.
    
            -regress_censor_outliers LIMIT : censor TRs with excessive outliers
    
                    e.g. -regress_censor_outliers 0.15
    
                This option is used to censor TRs where too many voxels are flagged
                as outliers by 3dToutcount.  LIMIT should be in [0.0, 1.0], as it
                is a limit on the fraction of masked voxels.
    
                '3dToutcount -automask -fraction' is used to output the fraction of
                (auto)masked voxels that are considered outliers at each TR.  If
                the fraction of outlier voxels is greater than LIMIT for some TR,
                that TR is censored out.
    
                Depending on the scanner settings, early TRs might have somewhat
                higher intensities.  This could lead to the first few TRs of each
                run being censored.  To avoid censoring the first few TRs of each
                run, apply the -regress_skip_first_outliers option.
    
                Note that if motion is also being censored, the multiple censor
                files will be combined (multiplied) before 3dDeconvolve.
    
                See '3dToutcount -help' for more details.
                See also -regress_skip_first_outliers, -regress_censor_motion.
    
            -regress_compute_gcor yes/no : compute GCOR from unit errts
    
                    e.g. -regress_compute_gcor no
                    default: yes
    
                By default, the global correlation (GCOR) is computed from the
                masked residual time series (errts).
    
                GCOR can be thought of as the result of:
                    A1. compute the correlations of each voxel with every other
                        --> can be viewed as an NMASK x NMASK correlation matrix
                    A2. compute GCOR: the average of the NMASK^2 values
    
                Since step A1 would take a lot of time and disk space, a more
                efficient computation is desirable:
                    B0. compute USET: scale each voxel time series to unit length
                    B1. compute GMU: the global mean of this unit dataset
                    B2. compute a correlation volume (of each time series with GMU)
                    B3. compute the average of this volume
    
                The actual computation is simplified even further, as steps B2 and
                B3 combine as the L2 norm of GMU.  The result is:
                    B2'. length(GMU)^2  (or the sum of squares of GMU)
    
                The steps B0, B1 and B2' are performed in the proc script.
    
                Note: This measure of global correlation is a single number in the
                      range [0, 1] (not in [-1, 1] as some might expect).
    
                Note: computation of GCOR requires a residual dataset, an EPI mask,
                      and a volume analysis (no surface at the moment).
    
            -regress_compute_tsnr yes/no : compute TSNR datasets from errts
    
                    e.g. -regress_compute_tsnr no
                    default: yes
    
                By default, a temporal signal to noise (TSNR) dataset is created at
                the end of the regress block.  The "signal" is the all_runs dataset
                (input to 3dDeconvolve), and the "noise" is the errts dataset (the
                residuals from 3dDeconvolve).  TSNR is computed (per voxel) as the
                mean signal divided by the standard deviation of the noise.
    
                   TSNR = average(signal) / stdev(noise)
    
                The main difference between the TSNR datasets from the volreg and
                regress blocks is that the data in the regress block has been
                smoothed and "completely" detrended (detrended according to the
                regression model: including polort, motion and stim responses).
    
                Use this option to prevent the TSNR dataset computation in the
                'regress' block.
    
                See also -volreg_compute_tsnr.
    
            -regress_fout yes/no         : output F-stat sub-bricks
    
                    e.g. -regress_fout no
                    default: yes
    
                This option controls whether to apply -fout in 3dDeconvolve.  The
                default is yes.
    
            -regress_make_cbucket yes/no : add a -cbucket option to 3dDeconvolve
    
                    default: 'no'
    
                Recall that the -bucket dataset (no 'c') contains beta weights and
                various statistics, but generally not including baseline terms
                (polort and motion).
    
                The -cbucket dataset (with a 'c') is a little different in that it
                contains:
                    - ONLY betas (no t-stats, no F-stats, no contrasts)
                    - ALL betas (including baseline terms)
                So it has one volume (beta) per regressor in the X-matrix.
    
                The use is generally for 3dSynthesize, to recreate time series
                datasets akin to the fitts, but where the user can request any set
                of parameters to be included (for example, the polort and the main
                2 regressors of interest).
    
                Setting this to 'yes' will result in the -cbucket option being
                added to the 3dDeconvolve command.
    
                Please see '3dDeconvolve -help' for more details.
    
            -regress_make_corr_vols LABEL1 ... : create correlation volume dsets
    
                    e.g. -regress_make_corr_vols aeseg FSvent
                    default: one is made against full_mask
    
                This option is used to specify extra correlation volumes to compute
                based on the residuals (so generally for resting state analysis).
    
                What is a such a correlation volume?
    
                   Given: errts     : the residuals from the linear regression
                          a mask    : to correlate over, e.g. full_mask
    
                   Compute: for each voxel (in the errts, say), compute the average
                      correlation over all voxels within the given mask.  In some
                      sense, this is a measure of self correlation over a specified
                      region.
    
                   This is a mean correlation rather than a correlation with the
                   mean.
    
                The labels specified can be from any ROI mask, such as those coming
                via -anat_follower_ROI, -regress_ROI_PC, or from the automatic
                masks from -mask_segment_anat.
    
                See also -anat_follower_ROI, -regress_ROI_PC, -mask_segment_anat.
    
            -regress_mot_as_ort yes/no : regress motion parameters using -ortvec
    
                    default: no
    
                By default, motion parameters are applied to 3dvolreg using
                -stim_file and -stim_base.  Use this option to apply them using
                -ortvec, instead.
    
                One difference is in having a "cleaner" 3dDeconvolve command,
                without the many extra -stim_file options.  Another is a change in
                the labels associated with the individual parameters.  Otherwise,
                all results should be the same.
    
            -regress_motion_per_run : regress motion parameters from each run
    
                    default: regress motion parameters catenated across runs
    
                By default, motion parameters from the volreg block are catenated
                across all runs, providing 6 (assuming 3dvolreg) regressors of no
                interest in the regression block.
    
                With -regress_motion_per_run, the motion parameters from each run
                are used as separate regressors, providing a total of (6 * nruns)
                regressors.
    
                This allows for the magnitudes of the regressors to vary over each
                run, rather than using a single (best) magnitude over all runs.
                So more motion-correlated variance can be accounted for, at the
                cost of the extra degrees of freedom (6*(nruns-1)).
    
                This option will apply to all motion regressors, including
                derivatives (if requested).
    
                ** This option was previously called -volreg_regress_per_run. **
    
            -regress_skip_first_outliers NSKIP : ignore the first NSKIP TRs
    
                    e.g. -regress_skip_first_outliers 4
                    default: 0
    
                When using -regress_censor_outliers, any TR with too high of an
                outlier fraction will be censored.  But depending on the scanner
                settings, early TRs might have somewhat higher intensities, leading
                to them possibly being inappropriately censored.
    
                To avoid censoring any the first few TRs of each run, apply the
                -regress_skip_first_outliers option.
    
                See also -regress_censor_outliers.
    
            -regress_compute_fitts       : compute fitts via 3dcalc, not 3dDecon
    
                This option is to save memory during 3dDeconvolve, in the case
                where the user has requested both the fitts and errts datasets.
    
                Normally 3dDeconvolve is used to compute both the fitts and errts
                time series.  But if memory gets tight, it is worth noting that
                these datasets are redundant, one can be computed from the other
                (given the all_runs dataset).
    
                    all_runs = fitts + errts
    
                Using -regress_compute_fitts, -fitts is no longer applied in 3dD
                (though -errts is).  Instead, note that an all_runs dataset is
                created just after 3dDeconvolve.  After that step, the script will
                create fitts as (all_runs-errts) using 3dcalc.
    
                Note that computation of both errts and fitts datasets is required
                for this option to be applied.
    
                See also -regress_est_blur_errts, -regress_errts_prefix,
                -regress_fitts_prefix and -regress_no_fitts.
    
            -regress_cormat_warnings Y/N : specify whether to get cormat warnings
    
                    e.g. -mask_cormat_warnings No
                    default: Yes
    
                By default, '1d_tool.py -show_cormat_warnings' is run on the
                regression matrix.  Any large, pairwise correlations are shown
                in text output (which is also saved to a text file).
    
                This option allows one to disable such functionality.
    
                Please see '1d_tool.py -help' for more details.
    
            -regress_est_blur_epits      : estimate the smoothness of the EPI data
    
                This option specifies to run 3dFWHMx on each of the EPI datasets
                used for regression, the results of which are averaged.  These blur
                values are saved to the file blur_est.$subj.1D, along with any
                similar output from errts.
    
                These blur estimates may be input to AlphaSim, for any multiple
                testing correction done for this subject.  If AlphaSim is run at
                the group level, it is reasonable to average these estimates
                across all subjects (assuming they were scanned with the same
                protocol and at the same scanner).
    
                The mask block is required for this operation (without which the
                estimates are not reliable).
    
                Please see '3dFWHMx -help' for more information.
                See also -regress_est_blur_errts.
    
            -regress_est_blur_errts      : estimate the smoothness of the errts
    
                This option specifies to run 3dFWHMx on the errts dataset, output
                from the regression (by 3dDeconvolve).
    
                These blur estimates may be input to AlphaSim, for any multiple
                testing correction done for this subject.  If AlphaSim is run at
                the group level, it is reasonable to average these estimates
                across all subjects (assuming they were scanned with the same
                protocol and at the same scanner).
    
                Note that the errts blur estimates should be not only slightly
                more accurate than the epits blur estimates, but they should be
                slightly smaller, too (which is beneficial).
    
                The mask block is required for this operation (without which the
                estimates are not reliable).
    
                Please see '3dFWHMx -help' for more information.
                See also -regress_est_blur_epits.
    
            -regress_errts_prefix PREFIX : specify a prefix for the -errts option
    
                    e.g. -regress_fitts_prefix errts
    
                This option is used to add a -errts option to 3dDeconvolve.  As
                with -regress_fitts_prefix, only the PREFIX is specified, to which
                the subject ID will be added.
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_fitts_prefix.
    
            -regress_fitts_prefix PREFIX : specify a prefix for the -fitts option
    
                    e.g. -regress_fitts_prefix model_fit
                    default: fitts
    
                By default, the 3dDeconvolve command in the script will be given
                a '-fitts fitts' option.  This option allows the user to change
                the prefix applied in the output script.
    
                The -regress_no_fitts option can be used to eliminate use of -fitts.
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_no_fitts.
    
            -regress_global_times        : specify -stim_times as global times
    
                    default: 3dDeconvolve figures it out, if it can
    
                By default, the 3dDeconvolve determines whether -stim_times files
                are local or global times by the first line of the file.  If it
                contains at least 2 times (which include '*' characters), it is
                considered as local_times, otherwise as global_times.
    
                The -regress_global_times option is mostly added to be symmetric
                with -regress_local_times, as the only case where it would be
                needed is when there are other times in the first row, but the
                should still be viewed as global.
    
                See also -regress_local_times.
    
            -regress_local_times         : specify -stim_times as local times
    
                    default: 3dDeconvolve figures it out, if it can
    
                By default, the 3dDeconvolve determines whether -stim_times files
                are local or global times by the first line of the file.  If it
                contains at least 2 times (which include '*' characters), it is
                considered as local_times, otherwise as global_times.
    
                In the case where the first run has only 1 stimulus (maybe even
                every run), the user would need to put an extra '*' after the
                first stimulus time.  If the first run has no stimuli, then two
                would be needed ('* *'), but only for the first run.
    
                Since this may get confusing, being explicit by adding this option
                is a reasonable thing to do.
    
                See also -regress_global_times.
    
            -regress_iresp_prefix PREFIX : specify a prefix for the -iresp option
    
                    e.g. -regress_iresp_prefix model_fit
                    default: iresp
    
                This option allows the user to change the -iresp prefix applied in
                the 3dDeconvolve command of the output script.
    
                By default, the 3dDeconvolve command in the script will be given a
                set of '-iresp iresp' options, one per stimulus type, unless the
                regression basis function is GAM.  In the case of GAM, the response
                form is assumed to be known, so there is no need for -iresp.
    
                The stimulus label will be appended to this prefix so that a sample
                3dDeconvolve option might look one of these 2 examples:
    
                    -iresp 7 iresp_stim07
                    -iresp 7 model_fit_donuts
    
                The -regress_no_iresp option can be used to eliminate use of -iresp.
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_no_iresp, -regress_basis.
    
            -regress_make_ideal_sum IDEAL.1D : create IDEAL.1D file from regressors
    
                    e.g. -regress_make_ideal_sum ideal_all.1D
    
                By default, afni_proc.py will compute a 'sum_ideal.1D' file that
                is the sum of non-polort and non-motion regressors from the
                X-matrix.  This -regress_make_ideal_sum option is used to specify
                the output file for that sum (if sum_idea.1D is not desired).
    
                Note that if there is nothing in the X-matrix except for polort and
                motion regressors, or if 1d_tool.py cannot tell what is in there
                (if there is no header information), then all columns will be used.
    
                Computing the sum means adding a 1d_tool.py command to figure out
                which columns should be used in the sum (since mixing GAM, TENT,
                etc., makes it harder to tell up front), and a 3dTstat command to
                actually sum those columns of the 1D X-matrix (the X-matrix is
                output by 3dDeconvolve).
    
                Please see '3dDeconvolve -help', '1d_tool.py -help' and
                '3dTstat -help'.
                See also -regress_basis, -regress_no_ideal_sum.
    
            -regress_motion_file FILE.1D  : use FILE.1D for motion parameters
    
                    e.g. -regress_motion_file motion.1D
    
                Particularly if the user performs motion correction outside of
                afni_proc.py, they may wish to specify a motion parameter file
                other than dfile_rall.1D (the default generated in the volreg
                block).
    
                Note: such files no longer need to be copied via -copy_files.
    
                If the motion file is in a remote directory, include the path,
                e.g. -regress_motion_file ../subject17/data/motion.1D .
    
            -regress_no_fitts       : do not supply -fitts to 3dDeconvolve
    
                    e.g. -regress_no_fitts
    
                This option prevents the program from adding a -fitts option to
                the 3dDeconvolve command in the output script.
    
                See also -regress_fitts_prefix.
    
            -regress_no_ideal_sum      : do not create sum_ideal.1D from regressors
    
                By default, afni_proc.py will compute a 'sum_ideal.1D' file that
                is the sum of non-polort and non-motion regressors from the
                X-matrix.  This option prevents that step.
    
                See also -regress_make_ideal_sum.
    
            -regress_no_ideals      : do not generate ideal response curves
    
                    e.g. -regress_no_ideals
    
                By default, if the GAM or BLOCK basis function is used, ideal
                response curve files are generated for each stimulus type (from
                the output X matrix using '3dDeconvolve -x1D').  The names of the
                ideal response function files look like 'ideal_LABEL.1D', for each
                stimulus label, LABEL.
    
                This option is used to suppress generation of those files.
    
                See also -regress_basis, -regress_stim_labels.
    
            -regress_no_iresp       : do not supply -iresp to 3dDeconvolve
    
                    e.g. -regress_no_iresp
    
                This option prevents the program from adding a set of -iresp
                options to the 3dDeconvolve command in the output script.
    
                By default -iresp will be used unless the basis function is GAM.
    
                See also -regress_iresp_prefix, -regress_basis.
    
            -regress_no_mask        : do not apply the mask in regression
    
                ** This is now the default, making the option unnecessary.
    
                This option prevents the program from applying the mask dataset
                in the scaling or regression steps.
    
                If the user does not want to apply a mask in the regression
                analysis, but wants the full_mask dataset for other reasons
                (such as computing blur estimates), this option can be used.
    
                See also -regress_est_blur_epits, -regress_est_blur_errts.
    
            -regress_no_motion      : do not apply motion params in 3dDeconvolve
    
                    e.g. -regress_no_motion
    
                This option prevents the program from adding the registration
                parameters (from volreg) to the 3dDeconvolve command.
    
            -regress_no_motion_demean : do not compute de-meaned motion parameters
    
                    default: do compute them
    
                Even if they are not applied in the regression, the default is to
                compute de-meaned motion parameters.  These may give the user a
                better idea of motion regressors, since their scale will not be
                affected by jumps across run breaks or multi-run drift.
    
                This option prevents the program from even computing such motion
                parameters.  The only real reason to not do it is if there is some
                problem with the command.
    
            -regress_no_motion_deriv  : do not compute motion parameter derivatives
    
                    default: do compute them
    
                Even if they are not applied in the regression, the default is to
                compute motion parameter derivatives (and de-mean them).  These can
                give the user a different idea about motion regressors, since the
                derivatives are a better indication of per-TR motion.  Note that
                the 'enorm' file that is created (and optionally used for motion
                censoring) is basically made by collapsing (via the Euclidean Norm
                - the square root of the sum of the squares) these 6 derivative
                columns into one.
    
                This option prevents the program from even computing such motion
                parameters.  The only real reason to not do it is if there is some
                problem with the command.
    
                    See also -regress_censor_motion.
    
            -regress_opts_3dD OPTS ...   : specify extra options for 3dDeconvolve
    
                    e.g. -regress_opts_3dD -gltsym ../contr/contrast1.txt  \
                                           -glt_label 1 FACEvsDONUT        \
                                           -jobs 6                         \
                                           -GOFORIT 8
    
                This option allows the user to add extra options to the 3dDeconvolve
                command.  Note that only one -regress_opts_3dD should be applied,
                which may be used for multiple 3dDeconvolve options.
    
                Please see '3dDeconvolve -help' for more information, or the link:
                    https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
    
            -regress_opts_reml OPTS ...  : specify extra options for 3dREMLfit
    
                    e.g. -regress_opts_reml                                 \
                            -gltsym ../contr/contrast1.txt FACEvsDONUT      \
                            -MAXa 0.92
    
                This option allows the user to add extra options to the 3dREMLfit
                command.  Note that only one -regress_opts_reml should be applied,
                which may be used for multiple 3dREMLfit options.
    
                Please see '3dREMLfit -help' for more information.
    
            -regress_ppi_stim_files FILE FILE ... : specify PPI (and seed) files
    
                    e.g. -regress_ppi_stim_files PPI.1.A.1D PPI.2.B.1D PPI.3.seed.1D
    
                Use this option to pass PPI stimulus files for inclusion in
                3dDeconvolve command.  This list is essentially appended to
                (and could be replaced by) -regress_extra_stim_files.
    
              * These are not timing files, but direct regressors.
    
                Use -regress_ppi_stim_labels to specify the corresponding labels.
    
                See also -write_ppi_3dD_scripts, -regress_ppi_stim_labels.
    
            -regress_ppi_stim_labels LAB1 LAB2 ... : specify PPI (and seed) labels
    
                    e.g. -regress_ppi_stim_files PPI.taskA PPI.taskB PPI.seed
    
                Use this option to specify labels for the PPI stimulus files
                specified via -regress_ppi_stim_files.  This list is essentially
                appended to (and could be replaced by) -regress_extra_stim_labels.
    
                Use -regress_ppi_stim_labels to specify the corresponding labels.
    
                See also -write_ppi_3dD_scripts, -regress_ppi_stim_labels.
    
            -regress_polort DEGREE  : specify the polynomial degree of baseline
    
                    e.g. -regress_polort 2
                    default: 1 + floor(run_length / 150.0)
    
                3dDeconvolve models the baseline for each run separately, using
                Legendre polynomials (by default).  This option specifies the
                degree of polynomial.  Note that this will create DEGREE * NRUNS
                regressors.
    
                The default is computed from the length of a run, in seconds, as
                shown above.  For example, if each run were 320 seconds, then the
                default polort would be 3 (cubic).
    
                Please see '3dDeconvolve -help' for more information.
    
            -regress_reml_exec      : execute 3dREMLfit, matching 3dDeconvolve cmd
    
                3dDeconvolve automatically creates a 3dREMLfit command script to
                match the regression model of 3dDeconvolve.  Via this option, the
                user can have that command executed.
    
                Note that the X-matrix used in 3dREMLfit is actually generated by
                3dDeconvolve.  The 3dDeconvolve command generates both the X-matrix
                and the 3dREMLfit command script, and so it must be run regardless
                of whether it actually performs the regression.
    
                To terminate 3dDeconvolve after creation of the X-matrix and
                3dREMLfit command script, apply -regress_3dD_stop.
    
                See also -regress_3dD_stop.
    
            -regress_ROI R1 R2 ... : specify a list of mask averages to regress out
    
                    e.g. -regress_ROI WMe
                    e.g. -regress_ROI brain WMe CSF
                    e.g. -regress_ROI FSvent FSwhite
    
                Use this option to regress out one more more known ROI averages.
                ROIs that can be generated from -mask_segment_anat/_erode include:
    
                    name    description     source dataset    creation program
                    -----   --------------  --------------    ----------------
                    brain   EPI brain mask  full_mask         3dAutomask
                    CSF     CSF             mask_CSF_resam    3dSeg -> Classes
                    CSFe    CSF (eroded)    mask_CSFe_resam   3dSeg -> Classes
                    GM      gray matter     mask_GM_resam     3dSeg -> Classes
                    GMe     gray (eroded)   mask_GMe_resam    3dSeg -> Classes
                    WM      white matter    mask_WM_resam     3dSeg -> Classes
                    WMe     white (eroded)  mask_WMe_resam    3dSeg -> Classes
    
                Other ROI labels can come from -anat_follower_ROI options, i.e.
                imported masks.
    
              * Use of this option requires either -mask_segment_anat or labels
                defined via -anat_follower_ROI options.
    
                See also -mask_segment_anat/_erode, -anat_follower_ROI.
                Please see '3dSeg -help' for more information on the masks.
    
            -regress_ROI_PC LABEL NUM_PC    : regress out PCs within mask
    
                    e.g. -regress_ROI_PC vent 3
                         -regress_ROI_PC WMe 3
    
                Add the top principal components (PCs) over an anatomical mask as
                regressors of no interest.
    
                  - LABEL   : the class label given to this set of regressors
                  - NUM_PC  : the number of principal components to include
    
                The LABEL can apply to something defined via -mask_segment_anat
                maybe with -mask_segment_erode, or from -anat_follower_ROI
                (assuming 'epi' grid), or 'brain' (full_mask).  The -mask_segment*
                options define ROI labels implicitly (see above), while the user
                defines ROI labels in any -anat_follower_ROI options.
    
                Method (including 'follower' steps):
    
                  If -anat_follower_ROI is used to define the label, then the
                  follower ROI steps would first be applied to that dataset.
    
                  If ROIs are created 'automatically' via 3dSeg (-mask_segment_anat)
                  then the follower steps do not apply.
    
                  F1. if requested (-anat_follower_erode) erode the ROI mask
                  F2. apply all anatomical transformations to the ROI mask
                      a. catenate all anatomical transformations
                         i.   anat to EPI?
                         ii.  affine xform of anat to template?
                         iii. subsequent non-linear xform of anat to template?
                      b. sample the transformed mask on the EPI grid
                      c. use nearest neighbor interpolation, NN
    
               Method (post-mask alignment):
    
                  P1. extract the top NUM_PC principal components from the volume
                      registered EPI data, over the mask
                      a. detrend the volume registered EPI data at the polort level
                         to be used in the regression, per run
                      b. catenate the detrended volreg data across runs
                      c. compute the top PCs from the (censored?) time series
                      d. if censoring, zero-fill the time series with volumes of
                         zeros at the censored TRs, to maintain TR correspondence
                  P2. include those PCs as regressors of no interest
                      a. apply with: 3dDeconvolve -ortvec PCs LABEL
    
                Typical usage might start with the FreeSurfer parcellation of the
                subject's anatomical dataset, followed by ROI extraction using
                3dcalc (to make a new dataset of just the desired regions).  Then
                choose the number of components to extract and a label.
    
                That ROI dataset, PC count and label are then applied with this
                option.
    
              * The given MASK must be in register with the anatomical dataset,
                though it does not necessarily need to be on the anatomical grid.
    
              * Multiple -regress_ROI_PC options can be used.
    
                See also -anat_follower, -anat_follower_ROI, -regress_ROI_erode,
                and -regress_ROI.
    
            -regress_ROI_PC_per_run LABEL ... : regress these PCs per run
    
                    e.g. -regress_ROI_PC_per_run vent
                    e.g. -regress_ROI_PC_per_run vent WMe
    
                Use this option to create the given PC regressors per run.  So
                if there are 4 runs and 3 'vent' PCs were requested with the
                option "-regress_ROI_PC vent 3", then applying this option with
                the 'vent' label results in not 3 regressors (one per PC), but
                12 regressors (one per PC per run).
    
                Note that unlike the -regress_ROI_per_run case, this is not merely
                splitting one signal across runs.  In this case the principle
                components are be computed per run, almost certainly resulting in
                different components than those computed across all runs at once.
    
                See also -regress_ROI_PC, -regress_ROI_per_run.
    
            -regress_RSFC           : perform bandpassing via 3dRSFC
    
                Use this option flag to run 3dRSFC after the linear regression
                step (presumably to clean resting state data).  Along with the
                bandpassed data, 3dRSFC will produce connectivity parameters,
                saved in the RSFC directory by the proc script.
    
                The -regress_bandpass option is required, and those bands will be
                passed directly to 3dRSFC.  Since bandpassing will be done only
                after the linear regression, censoring is not advisable.
    
                See also -regress_bandpass, -regress_censor_motion.
                Please see '3dRSFC -help' for more information.
    
            -regress_RONI IND1 ...  : specify a list of regressors of no interest
    
                    e.g. -regress_RONI 1 17 22
    
                Use this option flag regressors as ones of no interest, meaning
                they are applied to the baseline (for full-F) and the corresponding
                beta weights are not output (by default at least).
    
                The indices in the list should match those given to 3dDeconvolve.
                They start at 1 first with the main regressors, and then with any
                extra regressors (given via -regress_extra_stim_files).  Note that
                these do not apply to motion regressors.
    
                The user is encouraged to check the 3dDeconvolve command in the
                processing script, to be sure they are applied correctly.
    
            -regress_stim_labels LAB1 ...   : specify labels for stimulus classes
    
                    e.g. -regress_stim_labels houses faces donuts
                    default: stim01 stim02 stim03 ...
    
                This option is used to apply a label to each stimulus type.  The
                number of labels should equal the number of files used in the
                -regress_stim_times option, or the total number of columns in the
                files used in the -regress_stim_files option.
    
                These labels will be applied as '-stim_label' in 3dDeconvolve.
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_stim_times, -regress_stim_labels.
    
            -regress_stim_times FILE1 ... : specify files used for -stim_times
    
                    e.g. -regress_stim_times ED_stim_times*.1D
                    e.g. -regress_stim_times times_A.1D times_B.1D times_C.1D
    
                3dDeconvolve will be run using '-stim_times'.  This option is
                used to specify the stimulus timing files to be applied, one
                file per stimulus type.  The order of the files given on the
                command line will be the order given to 3dDeconvolve.  Each of
                these timing files will be given along with the basis function
                specified by '-regress_basis'.
    
                The user must specify either -regress_stim_times or
                -regress_stim_files if regression is performed, but not both.
                Note the form of the files is one row per run.  If there is at
                most one stimulus per run, please add a trailing '*'.
    
                Labels may be specified using the -regress_stim_labels option.
    
                These two examples of such files are for a 3-run experiment.  In
                the second example, there is only 1 stimulus at all, occurring in
                run #2.
    
                    e.g.            0  12.4  27.3  29
                                    30 40 50
    
                    e.g.            *
                                    20 *
    
                Please see '3dDeconvolve -help' for more information, or the link:
                    https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
                See also -regress_stim_files, -regress_stim_labels, -regress_basis,
                         -regress_basis_normall, -regress_polort.
    
            -regress_stim_files FILE1 ... : specify TR-locked stim files
    
                    e.g. -regress_stim_files ED_stim_file*.1D
                    e.g. -regress_stim_files stim_A.1D stim_B.1D stim_C.1D
    
                Without the -regress_use_stim_files option, 3dDeconvolve will be
                run using '-stim_times', not '-stim_file'.  The user can still
                specify the 3dDeconvolve -stim_file files here, but they would
                then be converted to -stim_times files using the script,
                make_stim_times.py .
    
                It might be more educational for the user to run make_stim_times.py
                outside afni_proc.py (such as was done before example 2, above), or
                to create the timing files directly.
    
                Each given file can be for multiple stimulus classes, where one
                column is for one stim class, and each row represents a TR.  So
                each file should have NUM_RUNS * NUM_TRS rows.
    
                The stim_times files will be labeled stim_times.NN.1D, where NN
                is the stimulus index.
    
                Note that if the stimuli were presented at a fixed time after
                the beginning of a TR, the user should consider the option,
                -regress_stim_times_offset, to apply that offset.
    
    
                If the -regress_use_stim_files option is provided, 3dDeconvolve
                will be run using each stim_file as a regressor.  The order of the
                regressors should match the order of any labels, provided via the
                -regress_stim_labels option.
    
                Alternately, this can be done via -regress_stim_times, along
                with -regress_stim_types 'file'.
    
                Please see '3dDeconvolve -help' for more information, or the link:
                    https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
                See also -regress_stim_times, -regress_stim_labels, -regress_basis,
                         -regress_basis_normall, -regress_polort,
                         -regress_stim_times_offset, -regress_use_stim_files.
    
            -regress_extra_stim_files FILE1 ... : specify extra stim files
    
                    e.g. -regress_extra_stim_files resp.1D cardiac.1D
                    e.g. -regress_extra_stim_files regs_of_no_int_*.1D
    
                Use this option to specify extra files to be applied with the
                -stim_file option in 3dDeconvolve (as opposed to the more usual
                option, -stim_times).
    
                These files will not be converted to stim_times format.
    
                Corresponding labels can be given with -regress_extra_stim_labels.
    
                See also -regress_extra_stim_labels, -regress_ROI, -regress_RONI.
    
            -regress_extra_stim_labels LAB1 ... : specify extra stim file labels
    
                    e.g. -regress_extra_stim_labels resp cardiac
    
                If -regress_extra_stim_files is given, the user may want to specify
                labels for those extra stimulus files.  This option provides that
                mechanism.  If this option is not given, default labels will be
                assigned (like stim17, for example).
    
                Note that the number of entries in this list should match the
                number of extra stim files.
    
                See also -regress_extra_stim_files.
    
            -regress_stim_times_offset OFFSET : add OFFSET to -stim_times files
    
                    e.g. -regress_stim_times_offset 1.25
                    e.g. -regress_stim_times_offset -9.2
                    default: 0
    
                With -regress_stim_times:
    
                   If the -regress_stim_times option is uses, and if ALL stim files
                   are timing files, then timing_tool.py will be used to add the
                   time offset to each -regress_stim_times file as it is copied into
                   the stimuli directory (near the beginning of the script).
    
                With -regress_stim_files:
    
                   If the -regress_stim_files option is used (so the script would
                   convert -stim_files to -stim_times before 3dDeconvolve), the
                   user may want to add an offset to the times in the resulting
                   timing files.
    
                   For example, if -tshift_align_to is applied and the user chooses
                   to align volumes to the middle of the TR, it might be appropriate
                   to add TR/2 to the times of the stim_times files.
    
                   This OFFSET will be applied to the make_stim_times.py command in
                   the output script.
    
                Please see 'make_stim_times.py -help' for more information.
                See also -regress_stim_files, -regress_use_stim_files,
                         -regress_stim_times and -tshift_align_to.
    
            -regress_stim_types TYPE1 TYPE2 ... : specify list of stim types
    
                    e.g. -regress_stim_types times times AM2 AM2 times AM1 file
                    e.g. -regress_stim_types AM2
                    default: times
    
                If amplitude, duration or individual modulation is desired with
                any of the stimulus timing files provided via -regress_stim_files,
                then this option should be used to specify one (if all of the types
                are the same) or a list of stimulus timing types.  One can also use
                the type 'file' for the case of -stim_file, where the input is a 1D
                regressor instead of stimulus times.
    
                The types should be (possibly repeated) elements of the set:
                {times, AM1, AM2, IM}, where they indicate:
    
                    times:  a standard stimulus timing file (not married)
                            ==> use -stim_times in 3dDeconvolve command
    
                    AM1:    have one or more married parameters
                            ==> use -stim_times_AM1 in 3dDeconvolve command
    
                    AM2:    have one or more married parameters
                            ==> use -stim_times_AM2 in 3dDeconvolve command
    
                    IM:     NO married parameters, but get beta for each stim
                            ==> use -stim_times_IM in 3dDeconvolve command
    
                    file:   a 1D regressor, not a stimulus timing file
                            ==> use -stim_file in 3dDeconvolve command
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_stim_times.
                See also example 7 (esoteric options).
    
            -regress_use_stim_files : use -stim_file in regression, not -stim_times
    
                The default operation of afni_proc.py is to convert TR-locked files
                for the 3dDeconvolve -stim_file option to timing files for the
                3dDeconvolve -stim_times option.
    
                If the -regress_use_stim_times option is provided, then no such
                conversion will take place.  This assumes the -regress_stim_files
                option is applied to provide such -stim_file files.
    
                This option has been renamed from '-regress_no_stim_times'.
    
                Please see '3dDeconvolve -help' for more information.
                See also -regress_stim_files, -regress_stim_times,
                         -regress_stim_labels.
    

--------------- 3dClustSim options ------------------
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: none

    
            -regress_run_clustsim yes/no : add 3dClustSim attrs to stats dset
    
                    e.g. -regress_run_clustsim no
                    default: yes
    
                This option controls whether 3dClustSim will be executed after the
                regression analysis.  Since the default is 'yes', the effective use
                of this option would be to turn off the operation.
    
                3dClustSim is a more advanced version of AlphaSim, and generates a
                table of cluster sizes/alpha values that can be then stored in the
                stats dataset for a simple multiple comparison correction in the
                cluster interface of the afni GUI.
    
                The blur estimates and mask dataset are required, and so the
                option is only relevant in the context of blur estimation.
    
                Please see '3dClustSim -help' for more information.
                See also -regress_est_blur_epits, -regress_est_blur_epits and
                         -regress_opts_CS.
    
            -regress_CS_NN LEVELS   : specify NN levels for 3dClustSim command
    
                    e.g.     -regress_CS_NN 1
                    default: -regress_CS_NN 123
    
                This option allows the user to specify which nearest neighbors to
                consider when clustering.  Cluster results will be generated for
                each included NN level.  Using multiple levels means being able to
                choose between those same levels when looking at the statistical
                results using the afni GUI.
    
                The LEVELS should be chosen from the set {1,2,3}, where the
                respective levels mean "shares a face", "shares an edge" and
                "shares a corner", respectively.  Any non-empty subset can be used.
                They should be specified as is with 3dClustSim.
    
                So there are 7 valid subsets: 1, 2, 3, 12, 13, 23, and 123.
    
                Please see '3dClustSim -help' for details on its '-NN' option.
    
            -regress_opts_CS OPTS ...    : specify extra options for 3dClustSim
    
                    e.g. -regress_opts_CS -athr 0.05 0.01 0.005 0.001
    
                This option allows the user to add extra options to the 3dClustSim
                command.  Only 1 such option should be applied, though multiple
                options to 3dClustSim can be included.
    
                Please see '3dClustSim -help' for more information.
                See also -regress_run_clustsim.
    
        - R Reynolds  Dec, 2006                             thanks to Z Saad
