#!/usr/bin/env python

# note: in the script, runs are 1-based (probably expected)

import string, sys, os
from time import asctime

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *
from db_mod import *
import ask_me

# ----------------------------------------------------------------------
# globals

g_history = """
    afni_proc.py history:

    1.0  Dec 20, 2006 : initial release
    1.1  Dec 20, 2006 : added -regress_use_stim_files
    1.2  Dec 21, 2006 : help, start -ask_me, updated when to use -iresp/ideal
    1.3  Dec 22, 2006 : change help to assumme ED's stim_times files exist
    1.4  Dec 25, 2006 : initial -ask_me
    1.5  Dec 27, 2006 : ask_me help
    1.6  Dec 28, 2006 : -gylsym examples, min(a,b) in scale block
    1.7  Jan 03, 2007 : help updates, no blank '\\' line from -gltsym
    1.8  Jan 08, 2007 :
         - changed default script name to proc.SUBJ_ID, and removed -script
           from most examples
         - added options -bash, -copy_files, -volreg_zpad, -tlrc_anat,
           -tlrc_base, -tlrc_no_ss, -tlrc_rmode, -tlrc_suffix
    1.9  Jan 09, 2007 : added aligned line wrapping (see afni_util.py)
    1.10 Jan 12, 2007 : set subj = $argv[1], added index to -glt_label in -help
    1.11 Jan 12, 2007 :
         - added options -move_preproc_files, -regress_no_motion
         - use $output_dir var in script, and echo version at run-time
         - append .$subj to more output files 
    1.12 Jan 16, 2007 : allow no +view when using -tlrc_anat
    1.13 Jan 17, 2007 : if -tlrc_anat, apply default option '-tlrc_suffix NONE'
    1.14 Jan 26, 2007 :
         - if only 1 run, warn user, do not use 3dMean
         - changed all True/False uses to 1/0 (for older python versions)
    1.15 Feb 02, 2007 :
         - output float for -blur_size
         - put execution command at top of script
    1.16 Feb 21, 2007 :
         - added optional 'despike' block
         - added options -do_block and -despike_opts_3dDes
    1.17 Feb 27, 2007 :
         - volreg_align_to defaults to 'third' (was 'first')
         - added +orig to despike input
         - added 'empty' block type, for a placeholder
    1.18 Mar 15, 2007 : minor changes on the ides of March (oooooooh...)
         - x1D output file uses x1D suffix
         - removed now unneeded -full_first option in 3dDeconvolve
    1.19 Mar 19, 2007 : allow for dataset TR stored in depreciated ms
    1.20 Mar 25, 2007 : added -help for long-existing -regress_use_stim_files
    1.21 Apr 19, 2007 : apply +orig in 1-run mean using 3dcopy
    1.22 May 08, 2007 :
         - change read_options() to be compatible with python version 2.2
         - '-basis_normall 1' is no longer used by default
         - rename -regress_no_stim_times to -regress_use_stim_files
    1.23 Jun 01, 2007 :
         - changed name of Xmat to X.xmat.1D
         - by default, apply -xjpeg in 3dDeconvolve
    1.24 Jun 04 2007 : added -scale_no_max
    1.25 Jun 27 2007 : on error, display failed command
    1.26 Oct 03 2007 : set default polort based on run length (like 3dDecon)
    1.27 Nov 26 2007 : added -volreg_interp, default is -cubic (was Fourier)
    1.28 Jan 22 2008 : estimating smoothness
         - added -regress_est_blur_errts, -regress_est_blur_epits options
           for estimating the blur in the EPI and errts data
         - added -regress_no_mask, -regress_errts_prefix and -show_valid_opts
    1.29 Jun 12 2008 : move code to afni_util.get_dset_reps_tr
    1.30 Jun 30 2008 : added -gen_epi_review and -no_epi_review options
    1.31 Sep 23 2008 : added -remove_preproc_files
    1.32 Oct 27 2008 : added -regress_motion_file
    1.33 Dec 10 2008 :
        - allow NIfTI datasets as input (but process as AFNI)
        - added -regress_extra_stim_files and -regress_extra_stim_labels
        - added -regress_RONI and -volreg_base_dset (for Jill Weisberg)
        - moved unloved g_help_string to db_mod_py
    1.34 Feb 17 2009 : added -regress_reml_exec and -regress_3dD_stop
    1.35 Mar 12 2009 :
        - if despiking and no regression mask, apply -nomask
        - added 'MASKING NOTE', to suggest no regresion mask until group space
    1.36 Mar 24 2009 :
        * -regress_no_mask is now the default *
        - added -regress_apply_mask
    1.37 Mar 25 2009 : allow +tlrc processing (+view comes from data)
    1.38 Mar 26 2009 : added helpstr to options
    1.39 Apr 01 2009 :
        - by default, the script will now terminate on any error
        - added -exit_on_error, -check_setup_errors
        - whine about block order problems
    1.40 Apr 01 2009 :
        - added 'ricor' processing block, for RETROICOR regressor removal
          (per-run method only, still needs '-help')
          options: -ricor_regs, -ricor_regs_nfirst, -ricor_polort,
                   -ricor_regress_solver, -ricor_regress_method
        - small format changes
    1.41 Apr 09 2009 : ricor changes
        - added 'across-runs' ricor_regress_method
        - added ricor information and usage to help (see "RETROICOR NOTE")
        - maintain unscaled shorts if they are input
        - added -ricor_datum
    1.42 Apr 11 2009 :
        - added -volreg_regress_per_run
        - fixed use of -regress_errts_prefix with blur est
    1.43 Apr 23 2009 : updates to help and tcsh options
        - added -f as a recommended tcsh option
        - added help section 'SCRIPT EXECUTION NOTE'
        - reordered help: intro, BLOCKS, DEFAULTS, EXAMPLES, NOTEs, OPTIONS
        - shifted execution command to separate line
    1.44 May 08 2009 : added tlrc (anat) as a processing block
    1.45 May 12 2009 : fixed 'cat' of 'across-runs' ricor regressors
    1.46 May 14 2009 : no 'rm rm.*' if such files were not created
    1.47 May 15 2009 : added -volreg_tlrc_warp, to warp at volreg step
    1.48 May 21 2009 :
        - added 'align' processing block (anat to EPI)
        - added -volreg_align_e2a (do EPI to anat alignment in volreg step)
    1.49 May 27 2009 :
        - added -volreg_warp_dxyz
        - if align a2e, add -no_ss to @auto_tlrc
        - for varying run lengths, fixed application of '-volreg_align_to last'
          and the -regress_est_blur_* options
          (blur estimation loops were modified for this)
        - warping to new grid truncates to 2 significant bits (if < 2 mm)
    1.50 May 28 2009 : example updates for AFNI_data4 and new options
    1.51 May 29 2009 :
        - added -execute option (to execute processing script)
        - fail on block options that have no such block applied
    1.52 Jun 08 2009 :
        - added -despike_mask option
        - fixed missing block warning
    1.53 Jun 11 2009 :
        - in mask block, try to create anat and group masks
        - added -mask_apply option, for choosing mask to apply to regression
        - added -align_opts_aea, for extra opts to align_epi_anat.py
    2.00 Jun 26 2009 : process method update: suggest processing in group space
        - mask warped EPI by its extents (at volreg step)
        - added -volreg_no_extent_mask, to block this masking
        - added 'extents' to list of mask in -mask_apply
        - change block dividers to more visual '===' with block names
    2.01 Jul 07 2009 : added warning to edit script on use of dmBLOCK
    2.02 Jul 27 2009 : used -slibase_sm instead of -slibase in 3dREMLfit
    2.03 Jul 29 2009 : fixed creation of extents mask when only 1 run
    2.04 Aug 06 2009 : fixed problems found by I Mukai and K Bahadur:
        - fixed -volreg_align_to base as applied in align_epi_anat.py
        - fixed blur 'averages' computation when only one run
    2.05 Aug 10 2009 :
        - Changed default min grid truncation from 2 significant bits to 3
          when applying -volreg_tlrc_warp/-volreg_align_e2s.
    2.06 Aug 13 2009 : added -volreg_tlrc_adwarp, to apply manual tlrc xform
    2.07 Aug 14 2009 : added -align_epi_ext_dset, to align anat to external EPI
    2.08 Aug 21 2009 : added -regress_censor_motion and -regress_censor_prev
                       motivated by L Thomas and B Bones
    2.09 Aug 25 2009 :
        - change the censor_motion prefix from '$subj' to 'motion_$subj'
        - if volreg block, always create 'motion_${subj}_enorm.1D'
    2.10 Aug 26 2009 : explicitly nuke negatives in scale block
    2.11 Aug 27 2009 : added -regress_local_times/-regress_global_times
    2.12 Aug 27 2009 : fixed motion_ in '3dD -censor', found by B Bones
    2.13 Oct 19 2009 :
        - added -blur_in_mask, to apply 3dBlurInMask (a.o.t. 3dmerge)
        - added -blur_in_automask and -blur_opts_BIM
        - added -sep_char (which probably needs more work) for Jill Weisberg
        - added -subj_curly (applied when -sep_char is '_')
    2.14 Nov 16 2009 : allow motion censoring with varying run lengths
    2.15 Jan 15 2010 : added -regress_fout yes/no option
    2.16 Jan 21 2010 :
        - added -tlrc_opts_at for adding options to @auto_tlrc
        - changed max(0,...) to *step(a)*step(b) in scaling block
    2.17 Mar 03 2010 : when censoring, create uncensored ideals and sum
    2.18 Mar 08 2010 : minor: changed option order in some examples
    2.19 Mar 18 2010 : minor: help updates to alignment options
    2.20 Mar 18 2010 : deal with args having '\\n' (from quoted newlines)
    2.21 Mar 23 2010 : added -regress_compute_fitts, to save memory in 3dD
    2.22 Mar 28 2010 : applied fitts computation to REML case
    2.23 Apr 26 2010 : added -regress_opts_reml
    2.24 May 12 2010 : added -regress_censor_first_trs
    2.25 May 29 2010 :
        - fixed use of -volreg_regress_per_run and -regress_censor_motion pair
          (thanks to D Drake for noting the problem)
    2.26 Jun 04 2010 :
        - if only one regressor, use 1dcat for "sum" ideal
        - added -outlier_count, default to "yes"
        - outlier counting is now at end of tcat block
    2.27 Jun 09 2010 :
        - added -regress_censor_outliers and -regress_skip_first_outliers
        - specified 'auto block:' in block headers for those not chosen by user
    2.28 Jun 10 2010 : fixed copying EPI and anat as NIfTI
          (thanks to S Tambalo for noting the problem)
    2.29 Jun 17 2010 : apply default polort in 3dToutcount
    2.30 Jun 17 2010 :
        - 3dToutcount detrending now defaults to Legendre polynomials and
          can so exceed polort 3 (limit found by I Mukai and K Bahadur)
        - added options -outlier_legendre and -outlier_polort
    2.31 Jul 14 2010 : added -mask_test_overlap and -regress_cormat_warnigns
    2.32 Jul 19 2010 : added -check_afni_version and -requires_afni_version
    2.33 Jul 22 2010 : added -regress_run_clustsim and -regress_opts_CS
    2.34 Aug 02 2010 :
        - check that stim_file/_time files match datasets
        - check for existence of input datasets
        - added -test_stim_files and -test_for_dsets options
        - now depends on lib_afni1D
    2.35 Aug 04 2010 :
        - added -regress_CS_NN, default to 123
        - changed 3dClustSim to use -both instead of just -niml
        - changed prefix to ClustSim (so resulting .1D files are not removed)
        - if request for ClustSim, require blur estimation
    2.36 Aug 04 2010 :
        - allow married timing files (needed for -test_stim_files)
        - added -keep_script_on_err (NEW default: delete script on error)
    2.37 Oct 20 2010 : added -tcat_remove_last_trs, -ricor_regs_rm_nlast
    2.38 Nov 04 2010 :
        - create sum_ideal.1D is now the default
        - allow varying basis functions (affects ideals/iresp output)
        - added -regress_no_ideal_sum, -regress_basis_multi
    2.39 Nov 04 2010 : no X.full_length.xmat.1D, use X.uncensored.xmat.1D
    2.40 Nov 10 2010 : added new NOTE sections for ANAT/EPI ALIGNMENT to -help
    2.41 Nov 18 2010 :
        - fixed stim_files to stim_times conversion after multi_basis change
          (thanks to M Weber for noting the problem)
    2.42 Nov 22 2010 : improved line wrapping
    2.43 Dec 14 2010 :
        - fixed problem with timing file tests on 'empty' files with '*'
          (thanks to C Deveney and R Momenan for noting the problem)
    2.44 Dec 16 2010 : small changes to file type warnings
    2.45 Jan 13 2011 : small changes to warnings for missing stimulus files
    2.46 Mar 07 2011 : make proc script executable
    2.47 Mar 14 2011 : if no mask but have extents, apply in scale
    2.48 Mar 15 2011 : use X.nocensor.1D (just to save 2 spaces)
    2.49 Apr 22 2011 :
        - if manual tlrc and -volreg_tlrc_adwarp, also transform extents mask
          (thanks to J Britton for noting the problem)
        - if -regress_reml_exec, insert 3dClustSim table in stats_REML
          (thanks to R Momenan for noting the problem)
    2.50 Apr 29 2011 :
        - added -align_epi_strip_method for align_epi_anat.py skull strip
        - added help for -volreg_no_extent_mask
        - no EPI Automask is not a comment, not a warning
        - check that process blocks are unique (except for 'empty')
    2.51 May 31 2011 :
        - re-worked motion as prep for more motion options
        - replaced -volreg_regress_per_run with -regress_motion_per_run
        - made uniq_list_as_dsets() a warning, not an error (for J Britton)
    2.52 Jun 02 2011 :
        - by default, demean and deriv motion parameters are simply created
        - by default, demean motion parameters are applied in the regression
          (replacing the original 'basic' parameters, which should have no
          change in betas of interest, just the constant polort betas)
        - added -regress_apply_mot_types to specify motion types for regression
        - added -regress_no_motion_demean and -regress_no_motion_deriv
    2.53 Jun 02 2011 :
        - added -regress_make_cbucket
        - include view in 3dcopy for single run extents mask
          (so there are no missing view warnigns, done for J Jarcho)
        - make regress TSNR dataset by default (added option -compute_tsnr)
    2.54 Jun 03 2011: volreg tsnr is not default
        - added -volreg_compute_tsnr (def no), -regress_compute_tsnr (def yes)
        - so -compute_tsnr has been removed
    2.55 Jun 30 2011: rename aligned anat output (from align_epi_anat.py)
        - OLD_al_keep, if output anat is useful (want anat -> EPI  alignment)
        - OLD_al_junk, if output anat is not    (want EPI  -> anat alignment)
    2.56 Jul 06, 2011:
        - create anat_final dset, to be clear it is aligned with the stats
        - suggest uber_subject.py in -ask_me dialog
    2.57 Jul 13, 2011:
        - run gen_ss_review_scripts.py: generate single subject review scripts
        - and execute any resulting 'basic' review script
    2.58 Jul 15, 2011: save output from ss_review in out.ss_review.txt
    2.59 Jul 20, 2011:
        - fixed aea.py -epi_base in case of:
          'align' and '-volreg_align_to last' and run lengths vary
          (thanks to S Brislin and S White for noting the problem)
    2.60 Jul 26, 2011:
        - if e2a, update current anat to skull-stripped anat from align block
          (this would avoid a second skull-strip step in @auto_tlrc)
        - added details to comments in align block
        - replaced help for -compute_tsnr with -regress and -volreg versions
          (thanks to B Benson for asking about obsolete -compute_tsnr)
    2.61 Aug 03, 2011:
        - changed aea.py -save_skullstrip to -save_orig_skullstrip
        - previously the stripped anat would be warped to match any obliquity
          of the EPI, which would throw off the alignment
        - thanks to A Ellenstein for noting the problem and to Z Saad for
          helping to figure it out
    2.62 Aug 31, 2011:
        - if censoring motion or outliers, add options to gen_ss_r command
        - added help for -regress_make_cbucket
    2.63 Oct  4, 2011: added -anat_has_skull option, to avoid stripping
    3.00 Oct 14, 2011: now processes surface data
        - added 'surf' processing block, and corresponding '-surf_*' options:
           -surf_anat, -surf_spec, -surf_anat_aligned, -surf_anat_has_skull,
           -surf_A, -surf_B, -surf_blur_fwhm (now sticking with -blur_size)
        - compute errts and TSNR by default (had required option or blur est)
    3.01 Oct 17, 2011: added help for surface analysis and -surf options
    3.02 Nov  2, 2011: warn of odd timing if using TENT as basis function
    3.03 Nov  7, 2011: added -blur_to_fwhm and -blur_opts_B2FW
        - for E Nelson and J Jarcho
    3.04 Nov  9, 2011: -surf_blur_fwhm is no longer valid, use -blur_size
    3.05 Jan 12, 2012: fixed ricor block 3dcalc loop for varying run lengths
    3.06 Jan 18, 2012: force anat and children to be AFNI format after 3dcopy
    3.07 Jan 28, 2012: surface analysis updates for subject FT in AFNI_data6
        - added -atlas_followers to @SUMA_AlignToExperiment command
        - if surf analysis: no mask in scaling block (e.g. default extents)
        - updated help example #8 for surf analysis of AFNI_data6 subject FT
    3.08 Jan 31, 2012: ricor block: no longer apply in later 3dDeconvolve
        - added -regress_apply_ricor, with default of 'no'
        - added help updates for this
    3.09 Feb 01, 2012: check for pre-steady state outliers
        - added option -tcat_outlier_warn_limit
    3.10 Feb 10, 2012:
        - added -check_results_dir option for ZSS
        - changed -tcat_outlier_warn_limit to -tcat_preSS_warn_limit
    3.11 Mar  2, 2012: fixed $runs use with ricor of multiple runs
        - problem noted by I Mukai
        - output afni -ver in script
    3.12 Mar  9, 2012:
        - added $hemi to rm.mean dset during scaling
        - added new '-overwrite_resp S' to @SUMA_AlignToExperiement command
    3.13 Mar 14, 2012:
        - test for global timing before local
          (global timing would look like bad local timing)
        - problem noted by P Pallett
    3.14 Mar 21, 2012:
        - use run_lengths list for TRs per run
        - removed path from external motion file
    3.15 Apr 12, 2012: backport to python 2.2
        - thanks to L Broster for noting 2.2 problems
    3.16 Apr 16, 2012: added -regress_bandpass, to bandpass during regression
    3.17 May 10, 2012:
        - allow for processing more than 99 runs
        - catenated 'rall' files will use '_' until prefix
    3.18 May 19, 2012: small help update for resting state examples
    3.19 May 21, 2012: added -regress_stim_types
    3.20 Jun 03, 2012: suggest -regress_motion_censor of 0.2 for resting-state
    3.21 Jun 05, 2012: verify that married types match
    3.22 Jun 06, 2012: check for EPI +tlrc view in NIfTI datasets
    3.23 Jun 15, 2012: added -regress_censor_extern
    3.24 Jun 28, 2012: help mistake on IM, thanks to I Blair for noting
    3.25 Jul 10, 2012: let user know whether 3dClustSim will be run
    3.26 Jul 11, 2012: fill gaps and holes in anatomical masks
                       (now requires AFNI from 7 May, 2012)
    3.27 Jul 26, 2012: now requires AFNI from 8 May, 2012
        - added -mask_segment_anat and -mask_rm_segsy
        - if anat is stripped, create segmented anat unless user says not to
    3.28 Jul 30, 2012: if surf analysis, create run_suma script
    3.29 Jul 31, 2012: have -mask_segment_anat default to no
    3.30 Aug 08, 2012: do not update tlrc anat with strip if passed in
    3.31 Aug 14, 2012:
        - match default class order for 3dSeg
        - copy labeltable into resampled dataset
    3.32 Sep 04, 2012: added -regress_ROI, for tissue-based regression
    3.33 Sep 25, 2012: fixed 2 REML problems
        - if 3dD_stop and reml_exec, use errts_REML for blur estimation
          (thanks to P Molfese for noting the problem)
        - apply compute_fitts for non-reml case
    3.34 Oct 01, 2012: added 'file' type for -regress_stim_types
    3.35 Oct 03, 2012: make dashed parameters illegal for many options
    3.36 Oct 17, 2012: remove unneeded -set_tr from 1d_tool.py -censor_motion
    3.37 Jan 09, 2013: added -regress_compute_gcor
    3.38 Feb 05, 2013: minor help intro update
    3.39 Feb 14, 2013: update for -move_preproc_files for surfaces
    3.40 Feb 21, 2013: minor help update
    3.41 Apr 05, 2013: aea.py: revert -save_orig_skullstrip to -save_skullstrip
                       (requires align_epi_anat.py from 1 Apr 2013)
    3.42 Apr 09, 2013: fixed computed fitts for REML case
                       (thanks to G Pagnoni for noting the problem)
    3.43 Apr 15, 2013: added RESTING STATE NOTE to help
    3.44 Apr 23, 2013: added eroded ROIs for -regress_ROI: WMe, GMe, CSFe
    3.45 May 03, 2013: added options -regress_anaticor and -mask_segment_erode
    3.46 May 03, 2013:
        - added help example 9b, recommended resting state with ANATICOR
        - -regress_anaticor implies -mask_segment_anat and -mask_segment_erode
    3.47 May 09, 2013: small code reorg in prep for ...
    3.48 May 09, 2013: added options -write_3dD_script, -write_3dD_prefix
    3.49 Jun 25, 2013: added options -volreg_mosim, -volreg_opts_ms
    3.50 Jun 27, 2013: added option -regress_mot_as_ort
    4.00 Aug 14, 2013: added non-linear template registration via auto_warp.py
        - added options -tlrc_NL_warp and -tlrc_NL_awpy_rm
    4.01 Aug 20, 2013: make 3dAutomask the default EPI strip method
    4.02 Aug 20, 2013: added -regress_RSFC, to run 3dRSFC
                       (requires updated 3dRSFC, for input sub-brick selection)
    4.03 Sep 19, 2013: added help for -regress_RSFC, including example 10b.
    4.04 Oct 31, 2013: restrict blur estimation to uncensored TRs
    4.05 Nov 01, 2013: let all-1 input for extents mask vary per run
                       (TRs may vary per run)
    4.06 Dec 17, 2013: 3dDespike now defaults to -NEW
        - added -despike_new yes/no for control
    4.07 Feb 06, 2014: minor -help and -ask_me text changes
    4.08 Feb 18, 2014: minor -help update
    4.09 Feb 19, 2014: if AM2 or IM, terminate extraction of ideals
    4.10 Mar 11, 2014:
        - set errts_pre in anaticor block, e.g. for use in RSFC or blur est
        - if no scale block and gaussian blur, re-apply extents mask
        - quiet change to writing command to script
    4.11 Mar 21, 2014:
        - applied errts_REML where appropriate (over just errts)
        - if anaticor and censoring, do not remove censored TRs again
          for blur est
    4.12 Mar 24, 2014: added the -regress_anaticor_radius option
    4.13 Mar 24, 2014:
        - added options -anat_uniform_method and -anat_opts_unif
        - move toutcount to new (hidden) postdata block
    4.14 Mar 31, 2014:
        - added -anat_unif_GM (default is yes)
        - added detail to ricor slices warning/error
        - if anat_uniform_method of unifize, turn of in auto_warp.py
    4.15 April 16, 2014: internal re-org, should have no effect
    4.16 April 17, 2014:
        - allow a special case of MIN_OUTLIER as the -volreg_base_dset, as
          recommended by T. Ross
    4.17 May 12, 2014: added -regress_use_tproject
        - default to 'yes' if there are no stim files
    4.18 May 16, 2014: changed default of -anat_unif_GM from yes to no
    4.19 Jun 26, 2014:
        - full_mask (EPI mask) is now byte (via 3dmask_tool)
        - note correlation with anat mask (in out.mask_ae_corr.txt)
    4.20 Jul 11, 2014: fixed 1d_tool.py -pad_into_many_runs for bandpassing
          when run lengths vary
    4.21 Sep 08, 2014: grid dimensions are now rounded to 6 significant
          bits before being truncated to 3
    4.22 Nov 07, 2014: shift -affter warp to -warp for -tlrc_NLwarp
        - requires 3dNwarpApply from Nov 7 or later
    4.23 Nov 21, 2014:
        - changed -anat_uniform_method none to mean no correction at all
        - now 'default' means to apply it normally in auto_warp.py
        - done for B Benson
    4.24 Dec 2, 2014: -tlrc_NL_awpy_rm was not being applied
    4.25 Jan 2, 2015: small help change
"""

g_version = "version 4.25, January 2, 2015"

# version of AFNI required for script execution
g_requires_afni = "7 Nov 2014" # 3dNwarpApply

# ----------------------------------------------------------------------
# dictionary of block types and modification functions

BlockLabels  = ['tcat', 'postdata', 'despike', 'ricor', 'tshift', 'align',
                'volreg', 'surf', 'blur', 'mask', 'scale', 'regress', 'tlrc', 'empty']
BlockModFunc  = {'tcat'   : db_mod_tcat,     'postdata' : db_mod_postdata,
                 'despike': db_mod_despike,
                 'ricor'  : db_mod_ricor,    'tshift' : db_mod_tshift,
                 'align'  : db_mod_align,    'volreg' : db_mod_volreg,
                 'surf'   : db_mod_surf,     'blur'   : db_mod_blur,
                 'mask'   : db_mod_mask,     'scale'  : db_mod_scale,
                 'regress': db_mod_regress,  'tlrc'   : db_mod_tlrc,
                 'empty'  : db_mod_empty}
BlockCmdFunc  = {'tcat'   : db_cmd_tcat,     'postdata' : db_cmd_postdata,
                 'despike': db_cmd_despike,
                 'ricor'  : db_cmd_ricor,    'tshift' : db_cmd_tshift,
                 'align'  : db_cmd_align,    'volreg' : db_cmd_volreg,
                 'surf'   : db_cmd_surf,     'blur'   : db_cmd_blur,
                 'mask'   : db_cmd_mask,     'scale'  : db_cmd_scale,
                 'regress': db_cmd_regress,  'tlrc'   : db_cmd_tlrc,
                 'empty'  : db_cmd_empty}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# default block labels, and other labels (along with the label they follow)
DefLabels = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
OtherDefLabels = {'despike':'postdata', 'align':'postdata', 'ricor':'despike',
                  'surf':'volreg'}
OtherLabels    = ['empty']
DefSurfLabs    = ['tcat','tshift','align','volreg','surf','blur',
                  'scale','regress']

# names for blocks that do NOT process (make new) EPI data
#   --> these do not need index increments
EPInomodLabs = ['postdata', 'align', 'tlrc', 'mask']

# --------------------------------------------------------------------------
# data processing stream class
class SubjProcSream:
    def __init__(self, label):
        self.label      = label         # name for stream
                                        # copy argv, but nuke any newlines
        self.argv = [UTIL.replace_n_squeeze(arg, '\n', ' ') for arg in sys.argv]
        self.valid_opts = None          # list of possible user options
        self.user_opts  = None          # list of given user options
        self.sep_char   = '.'           # filename separator character

        self.blocks     = []            # list of ProcessBlock elements
        self.dsets      = []            # list of afni_name elements
        self.check_rdir = 'yes'         # check for existence of results dir
        self.stims_orig = []            # orig list of stim files to apply
        self.stims      = []            # list of stim files to apply
        self.extra_stims_orig = []      # orig list of extra_stims
        self.extra_stims      = []      # extra -stim_file list
        self.extra_labs       = []      # labels for extra -stim_file list

        self.vr_ext_base= None          # name of external volreg base 
        self.vr_ext_pre = 'external_volreg_base' # copied volreg base prefix
        self.vr_int_name= ''            # other internal volreg dset name
        self.volreg_prefix = ''         # prefix for volreg dataset ($run)
                                        #   (using $subj and $run)
        self.mot_labs   = []            # labels for motion params
        # motion parameter file (across all runs)
        self.mot_file   = 'dfile_rall.1D' # either mot_default or mot_extern
        # for regression, maybe just mot_file, maybe per run, external
        # or might include demean and/or derivatives
        self.mot_regs   = []            # motion files to use in regression
        self.mot_names  = []            # short names for motion files
        self.mot_per_run= 0             # motion regression per run
        self.mot_default= ''            # probably 'dfile_rall.1D', if set
        self.mot_extern = ''            # from -regress_motion_file
        self.mot_demean = ''            # from demeaned motion file
        self.mot_deriv  = ''            # motion derivatives
        self.mot_enorm  = ''            # euclidean norm of derivatives
        self.mot_simset = None          # motion simulation dset (afni_name)

        self.mot_cen_lim= 0             # motion censor limit, if applied
        self.out_cen_lim= 0             # outlier censor limit, if applied
        self.out_ss_lim = 0.4           # outlier pre-steady state warn limit
        self.out_wfile  = ''            # warnings file, for pre-SS
                                        # (set upon "creation")
        self.opt_src    = 'cmd'         # option source
        self.subj_id    = 'SUBJ'        # hopefully user will replace this
        self.subj_label = '$subj'       # replace this for execution
        self.out_dir    = ''            # output directory for use by script
        self.od_var     = ''            # output dir variable: $output_dir
        self.script     = None          # script name, default proc.SUBJ
        self.script_3dD = ''            # name of 3dDecon script, if desired
        self.prefix_3dD = 'test.'       # prefix for 3dD script output files
        self.make_main_script = 1       # do we actually create main script?
        self.overwrite  = 0             # overwrite script file?
        self.fp         = None          # file object
        self.all_runs   = ''            # prefix for final all_runs dataset
        self.anat       = None          # anatomoy to copy (afni_name class)
        self.anat_has_skull = 1         # does the input anat have a skull
                                        # also updated in db_cmd_align
        self.anat_unif_meth = 'default' # unifize method
        self.anat_unifized = 0          # has the anat been unifized
        self.anat_final = None          # anat assumed aligned with stats
        self.nlw_aff_mat= ''
        self.nlw_NL_mat = ''
        self.tlrcanat   = None          # expected name of tlrc dataset
        self.tlrc_base  = None          # afni_name dataset used in -tlrc_base
        self.tlrc_nlw   = 0             # are we using non-linear registration
        self.tlrc_ss    = 1             # whether to do skull strip in tlrc
        self.warp_epi   = 0             # xform bitmap: tlrc, adwarp, a2e, e2a
        self.a2e_mat    = None          # anat2epi transform matrix file
        self.e2final_mv = []            # matvec list takes epi base to final
        self.e2final    = ''            # aff12.1D file for e2final_mv
        self.errts_pre  = ''            # possibly changing errts prefix
        self.errts_reml = ''            # prefix for any REML errts
        self.errts_cen  = 0             # flag: current errts has censored TRs removed
        self.align_ebase= None          # external EPI for align_epi_anat.py
        self.align_epre = 'ext_align_epi' # copied align epi base prefix
        self.rm_rm      = 1             # remove rm.* files (user option)
        self.have_rm    = 0             # have rm.* files (such files exist)
        self.rm_dirs    = 0             # do we have dirs to remove?
        self.rm_list    = ['rm.*']      # array of items to nuke
        self.epi_review = '@epi_review.$subj' # filename for gen_epi_review.py
        self.made_ssr_scr = 0           # did we make subj review scripts
        self.ssr_basic    = '@ss_review_basic' # basic review script
        self.test_stims   = 1           # test stim_files for appropriateness
        self.test_dsets   = 1           # test datasets for existence

        self.ricor_apply  = 'no'        # apply ricor regs in 3dDeconvolve
        self.ricor_reg    = None        # ricor reg to apply in regress block
        self.ricor_nreg   = 0           # number of regs in ricor_reg
        self.ricor_regs   = []          # RETROICOR regressor files
        self.ricor_nfirst = 0           # number of TRs to remove
        self.ricor_nlast  = 0           # number of final TRs to remove

        self.check_setup_errors = 0     # count init setup errors
        self.exit_on_error      = 1     # exit on any encountered error
        self.verb               = 1     # verbosity level

        self.tr         = 0.0           # TR, in seconds
        self.reps       = 0             # TRs per run
        self.runs       = 0             # number of runs
        self.reps_all   = []            # number of TRs in each run
        self.reps_vary  = 0             # do the repetitions vary
        self.datatype   = -1            # 1=short, 3=float, ..., -1=uninit
        self.scaled     = -1            # if shorts, are they scaled?
        self.mask       = None          # mask dataset: one of the following
        self.mask_epi   = None          # mask dataset (from EPI)
        self.mask_anat  = None          # mask dataset (from subject anat)
        self.mask_group = None          # mask dataset (from tlrc base)
        self.mask_extents = None        # mask dataset (of EPI extents)
        self.mask_classes = None        # Segsy result at EPI resolution

        # options for tissue based time series
        self.roi_dict   = {}            # dictionary of ROI vs afni_name

        self.bandpass     = []          # bandpass limits
        self.censor_file  = ''          # for use as '-censor FILE' in 3dD
        self.censor_count = 0           # count times censoring
        self.censor_extern = ''         # from -regress_censor_extern
        self.exec_cmd   = ''            # script execution command string
        self.bash_cmd   = ''            # bash formatted exec_cmd
        self.tcsh_cmd   = ''            # tcsh formatted exec_cmd
        self.regmask    = 0             # apply any full_mask in regression
        self.regress_orts = []          # list of ortvec [file, label] pairs
        self.origview   = '+orig'       # view could also be '+tlrc'
        self.view       = '+orig'       # (starting and 'current' views)
        self.xmat       = 'X.xmat.1D'   # X-matrix file (might go uncensored)
        self.xmat_nocen = 'X.xmat.1D'   # X-matrix file (without censoring)

        # options for surface based script
        self.surf_spec  = []            # left and/or right spec files
        self.surf_anat  = None          # anat corresponding to surfaces
        self.surf_anat_aligned = 'no'   # yes/no
        self.surf_anat_has_skull='yes'  # yes/no

        self.surf_A     = 'smoothwm'
        self.surf_B     = 'pial'
        self.surf_blur_fwhm = 8.0       # target FWHM (from -blur_size)
        self.suma_cmd_file = 'run_suma' # script to contain suma command

        # computed surf variables
        self.surf_sv       = None       # either surf_anat or aligned version
        self.surf_sv_dir   = ''         # directory (for remote sv)
        self.surf_svd_var  = ''         # surf_vol directory variable
        self.surf_spec_dir = ''         # directory containing spec files
        self.surf_spd_var  = ''         # spec directory variable
        self.surf_spec_var = ''         # variable to use for spec file
                                        # (because of lh, rh)
        self.surf_spec_var_iter = ''    # iteration variable (e.g. hemi)
        self.surf_spec_base = ''        # basename of first spec
        self.surf_svi_ref  = ''         # iter var reference (e.g. ${hemi})
        self.surf_hemilist = ''         # e.g. ['lh', 'rh']

        # updated throughout processing...
        self.bindex     = 0             # current block index
        self.pblabel    = 'xxx'            # previous block label
        self.surf_names = 0             # make surface I/O dset names

        return

    def show(self, mesg):
        print '%sSubjProcSream: %s' % (mesg, self.label)
        if self.verb > 3:
            for block in self.blocks:
                block.show('    Block %d: ' % self.blocks.index(block))
        print '    Dsets : ',
        if len(self.dsets) < 1: print 'not yet set'
        else:
            if self.verb > 2:
                print
                for dset in self.dsets: dset.show()
            else:
                for dset in self.dsets: print dset.rel_input(),
                print
        if self.verb > 3: self.valid_opts.show('valid_opts: ')
        if self.verb > 1: self.user_opts.show('user_opts: ')

    def init_opts(self):
        self.valid_opts = OptionList('afni_proc.py options')

        # input style options  rcr - update
        # self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

        # terminal options
        self.valid_opts.add_opt('-help', 0, [],
                        helpstr="show this help")
        self.valid_opts.add_opt('-hist', 0, [],
                        helpstr="show revision history")
        self.valid_opts.add_opt('-requires_afni_version', 0, [],
                        helpstr='show which date is required of AFNI')
        self.valid_opts.add_opt('-show_valid_opts', 0, [],
                        helpstr="show all valid options")
        self.valid_opts.add_opt('-ver', 0, [],
                        helpstr="show module version")

        # general execution options
        self.valid_opts.add_opt('-blocks', -1, [], okdash=0,
                        helpstr='specify ordered list of blocks to apply')
        self.valid_opts.add_opt('-do_block', -1, [], okdash=0,
                        helpstr='add extra blocks to the default list')
        self.valid_opts.add_opt('-dsets', -1, [], okdash=0,
                        helpstr='EPI datasets to process, ordered by run')

        self.valid_opts.add_opt('-out_dir', 1, [],
                        helpstr='result directory, where script is run')
        self.valid_opts.add_opt('-scr_overwrite', 0, [],
                        helpstr='overwrite existing processing script')
        self.valid_opts.add_opt('-script', 1, [],
                        helpstr='specify processing script to generate')
        self.valid_opts.add_opt('-sep_char', 1, [],
                        helpstr="output filename separator char, def='.'")
        self.valid_opts.add_opt('-subj_curly', 0, [],
                        helpstr="always use {} around $subj")
        self.valid_opts.add_opt('-subj_id', 1, [],
                        helpstr='subject ID, used in most filenames')

        self.valid_opts.add_opt('-anat_has_skull', 1, [],
                        acplist=['yes','no'],
                        helpstr='does the anat have a skull (to be stripped)')
        self.valid_opts.add_opt('-anat_uniform_method', 1, [],
                        acplist=['none', 'default', 'unifize'],
                        helpstr='specify uniformity method (def=default)')
        self.valid_opts.add_opt('-anat_opts_unif', -1, [],
                        helpstr='additional options passed to 3dUnifize')
        self.valid_opts.add_opt('-anat_unif_GM', 1, [],
                        acplist=['yes','no'],
                        helpstr='also unifize gray matter (def=yes)')
        self.valid_opts.add_opt('-ask_me', 0, [],       # QnA session
                        helpstr='have afni_proc.py as the user for options')
        self.valid_opts.add_opt('-bash', 0, [],
                        helpstr='show execution help in bash syntax')
        self.valid_opts.add_opt('-check_afni_version', 1, [],
                        acplist=['yes','no'],
                        helpstr='check that AFNI is current enough')
        self.valid_opts.add_opt('-check_results_dir', 1, [],
                        acplist=['yes','no'],
                        helpstr='have script check for existing results dir')
        self.valid_opts.add_opt('-check_setup_errors', 1, [],
                        acplist=['yes','no'],
                        helpstr='terminate on setup errors')
        self.valid_opts.add_opt('-copy_anat', 1, [],
                        helpstr='anatomy to copy to results directory')
        self.valid_opts.add_opt('-copy_files', -1, [], okdash=0,
                        helpstr='list of files to copy to results directory')
        self.valid_opts.add_opt('-execute', 0, [],
                        helpstr='execute script as suggested to user')
        self.valid_opts.add_opt('-exit_on_error', 1, [],
                        acplist=['yes','no'],
                        helpstr='exit script on any command error')
        self.valid_opts.add_opt('-gen_epi_review', 1, [],
                        helpstr='generate a script to review orig EPI data')
        self.valid_opts.add_opt('-no_epi_review', 0, [],
                        helpstr='do not generate an EPI review script')
        self.valid_opts.add_opt('-keep_rm_files', 0, [],
                        helpstr='do not delete temporary rm.* files')
        self.valid_opts.add_opt('-keep_script_on_err', 1, [],
                        acplist=['yes','no'],
                        helpstr='do not delete script on failure')
        self.valid_opts.add_opt('-move_preproc_files', 0, [],
                        helpstr='move preprocessing files to preproc.data dir')
        self.valid_opts.add_opt('-outlier_count', 1, [],
                        acplist=['yes','no'],
                        helpstr='run 3dToutcount?  (default=yes)')
        self.valid_opts.add_opt('-outlier_legendre', 1, [],
                        acplist=['yes','no'],
                        helpstr='use -legendre in 3dToutcount?  (def=yes)')
        self.valid_opts.add_opt('-outlier_polort', 1, [],
                        helpstr='3dToutcount polort (default is as with 3dD)')
        self.valid_opts.add_opt('-remove_preproc_files', 0, [],
                        helpstr='remove pb0* preprocessing files')
        self.valid_opts.add_opt('-test_for_dsets', 1, [],
                        acplist=['yes','no'],
                        helpstr="test input datasets for existence (def=yes)")
        self.valid_opts.add_opt('-test_stim_files', 1, [],
                        acplist=['yes','no'],
                        helpstr="test stim_files for validity (default=yes)")
        self.valid_opts.add_opt('-write_3dD_prefix', 1, [],
                       helpstr="prefix for output files via -write_3dD_script")
        self.valid_opts.add_opt('-write_3dD_script', 1, [],
                       helpstr="only write 3dDeconvolve script (to given file)")
        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr="set the verbose level")

        # block options
        self.valid_opts.add_opt('-tcat_preSS_warn_limit', 1, [],
                        helpstr='set limit where TR #0 outliers suggest pre-SS')
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [],
                        helpstr='num TRs to remove from start of each run')
        self.valid_opts.add_opt('-tcat_remove_last_trs', 1, [],
                        helpstr='num TRs to remove from end of each run')

        self.valid_opts.add_opt('-despike_mask', 0, [],
                        helpstr="allow 3dDespike to automask (-dilate 4)")
        self.valid_opts.add_opt('-despike_opts_3dDes', -1, [],
                        helpstr='additional options directly for 3dDespike')
        self.valid_opts.add_opt('-despike_new', 1, [],
                        acplist=['yes','no'],
                        helpstr="(yes/no) run 3dDespike with -NEW method")

        self.valid_opts.add_opt('-ricor_datum', 1, [],
                        acplist=['short', 'float'],
                        helpstr='output datatype from ricor processing block')
        self.valid_opts.add_opt('-ricor_polort', 1, [],
                        helpstr='polort to apply for RETROICOR regression')
        self.valid_opts.add_opt('-ricor_regress_method', 1, [],
                        acplist=['per-run', 'across-runs'],
                        helpstr="use stimuli 'per-run' or 'across-runs'")
        self.valid_opts.add_opt('-ricor_regress_solver', 1, [],
                        helpstr="regression via 'OLSQ' or 'REML'")
        self.valid_opts.add_opt('-ricor_regs', -1, [], okdash=0,
                        helpstr='slice-based regressors for RETROICOR')
        self.valid_opts.add_opt('-ricor_regs_nfirst', 1, [],
                        helpstr='num first TRs to remove from ricor_regs')
        self.valid_opts.add_opt('-ricor_regs_rm_nlast', 1, [],
                        helpstr='num last TRs to remove from ricor_regs')

        self.valid_opts.add_opt('-tshift_align_to', -1, [],
                        helpstr='time alignment option given to 3dTshift')
        self.valid_opts.add_opt('-tshift_interp', 1, [],
                        helpstr='interpolation method used in 3dTshift')
        self.valid_opts.add_opt('-tshift_opts_ts', -1, [],
                        helpstr='additional options directly for 3dTshift')

        self.valid_opts.add_opt('-align_epi_ext_dset', 1, [],
                        helpstr='external EPI volume for align_epi_anat.py')
        self.valid_opts.add_opt('-align_opts_aea', -1, [],
                        helpstr='additional options for align_epi_anat.py')
        self.valid_opts.add_opt('-align_epi_strip_method', 1, [],
                        acplist=['3dSkullStrip','3dAutomask','None'],
                        helpstr="specify method for 'skull stripping' the EPI")

        self.valid_opts.add_opt('-tlrc_anat', 0, [],
                        helpstr='run @auto_tlrc on anat from -copy_anat')
        self.valid_opts.add_opt('-tlrc_base', 1, [],
                        helpstr='alternate @auto_tlrc base (not TT_N27, say)')
        self.valid_opts.add_opt('-tlrc_opts_at', -1, [],
                        helpstr='additional options supplied to @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_NL_awpy_rm', 1, [],
                        acplist=['yes','no'],
                        helpstr='use non-linear warping to template')
        self.valid_opts.add_opt('-tlrc_NL_warp', 0, [],
                        helpstr='use non-linear warping to template')
        self.valid_opts.add_opt('-tlrc_no_ss', 0, [],
                        helpstr='do not skull-strip during @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_rmode', 1, [],
                        helpstr='resample mode applied in @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_suffix', 1, [],
                        helpstr='suffix applied in @auto_tlrc (default: NONE)')

        self.valid_opts.add_opt('-volreg_align_e2a', 0, [],
                        helpstr="align EPI to anatomy (via align block)")
        self.valid_opts.add_opt('-volreg_align_to', 1, [],
                        acplist=['first','third', 'last'],
                        helpstr="align to 'first', 'third' or 'last' TR")
        self.valid_opts.add_opt('-volreg_base_dset', 1, [],
                        helpstr='external dataset to use as volreg base')
        self.valid_opts.add_opt('-volreg_base_ind', 2, [],
                        helpstr='run/sub-brick indices for volreg')
        self.valid_opts.add_opt('-volreg_compute_tsnr', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute TSNR datasets (yes/no) of volreg run1')
        self.valid_opts.add_opt('-volreg_interp', 1, [],
                        helpstr='interpolation method used in volreg')
        self.valid_opts.add_opt('-volreg_motsim', 0, [],
                        helpstr='create a motion simulated time series')
        self.valid_opts.add_opt('-volreg_no_extent_mask', 0, [],
                        helpstr='do not restrict warped EPI to extents')
        self.valid_opts.add_opt('-volreg_opts_ms', -1, [],
                        helpstr='add options directly to @simulate_motion')
        self.valid_opts.add_opt('-volreg_opts_vr', -1, [],
                        helpstr='additional options directly for 3dvolreg')
        self.valid_opts.add_opt('-volreg_regress_per_run', 0, [],
                        helpstr='apply separate motion regressors per run')
        self.valid_opts.add_opt('-volreg_tlrc_adwarp', 0, [],
                        helpstr='apply adwarp tlrc transformation after volreg')
        self.valid_opts.add_opt('-volreg_tlrc_warp', 0, [],
                        helpstr='warp volreg data to standard space')
        self.valid_opts.add_opt('-volreg_warp_dxyz', 1, [],
                        helpstr='output grid size for _tlrc_warp, _align_e2a')
        self.valid_opts.add_opt('-volreg_zpad', 1, [],
                        helpstr='number of slices to pad by in volreg')

        self.valid_opts.add_opt('-blur_filter', 1, [],
                        helpstr='blurring filter option (def: -1blur_fwhm)')
        self.valid_opts.add_opt('-blur_in_mask', 1, [],
                        acplist=['yes','no'],
                        helpstr='restrict blur to mask: yes/no')
        self.valid_opts.add_opt('-blur_in_automask', 0, [],
                        helpstr='applies -automask to 3dBlurInMask')
        self.valid_opts.add_opt('-blur_size', 1, [],
                        helpstr='size of blur kernel (def: 4)')
        self.valid_opts.add_opt('-blur_to_fwhm', 0, [],
                        helpstr='use 3dBlurToFWHM for blur operation')
        self.valid_opts.add_opt('-blur_opts_B2FW', -1, [],
                        helpstr='additional options directly for 3dBlurToFWHM')
        self.valid_opts.add_opt('-blur_opts_BIM', -1, [],
                        helpstr='additional options directly for 3dBlurInMask')
        self.valid_opts.add_opt('-blur_opts_merge', -1, [],
                        helpstr='additional options directly for 3dmerge')

        self.valid_opts.add_opt('-mask_apply', 1, [],
                        acplist=['epi', 'anat', 'group', 'extents'],
                        helpstr="select mask to apply in regression")
        self.valid_opts.add_opt('-mask_dilate', 1, [],
                        helpstr="dilation to be applied in automask")
        self.valid_opts.add_opt('-mask_rm_segsy', 1, [],
                        acplist=['yes', 'no'],
                        helpstr="remove Segsy directory (yes/no)")
        self.valid_opts.add_opt('-mask_segment_anat', 1, [],
                        acplist=['yes', 'no'],
                        helpstr="automatic segmentation using 3dSeg (yes/no)")
        self.valid_opts.add_opt('-mask_segment_erode', 1, [],
                        acplist=['yes', 'no'],
                        helpstr="also create eroded segmentation masks")
        self.valid_opts.add_opt('-mask_test_overlap', 1, [],
                        acplist=['yes','no'],
                        helpstr='test anat/EPI mask overlap (yes/no)')
        self.valid_opts.add_opt('-mask_type', 1, [],
                        acplist=['union','intersection'],
                        helpstr="specify a 'union' or 'intersection' mask type")

        self.valid_opts.add_opt('-scale_max_val', 1, [],
                        helpstr="maximum value for scaled data (def: 200)")
        self.valid_opts.add_opt('-scale_no_max', 0, [],
                        helpstr="do not limit scaled data")

        self.valid_opts.add_opt('-regress_3dD_stop', 0, [],
                        helpstr="stop 3dDeconvolve after matrix generation")
        self.valid_opts.add_opt('-regress_anaticor', 0, [],
                        helpstr="apply ANATICOR: regress WMeLocal time series")
        self.valid_opts.add_opt('-regress_anaticor_radius', 1, [],
                        helpstr="specify radius for WMeLocal extraction")
        self.valid_opts.add_opt('-regress_apply_mask', 0, [],
                        helpstr="apply the mask in regression")
        self.valid_opts.add_opt('-regress_apply_ricor', 1, [],
                        acplist=['yes','no'],
                        helpstr="apply ricor regs in regression (def no)")
        self.valid_opts.add_opt('-regress_bandpass', 2, [],
                        helpstr="bandpass in this range during regression")
        self.valid_opts.add_opt('-regress_basis', 1, [],
                        helpstr="basis function to use in regression")
        self.valid_opts.add_opt('-regress_basis_multi', -1, [], okdash=0,
                        helpstr="one basis function per stimulus class")
        self.valid_opts.add_opt('-regress_basis_normall', 1, [],
                        helpstr="specify magnitude of basis functions")
        self.valid_opts.add_opt('-regress_compute_gcor', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute global correlation in residuals')
        self.valid_opts.add_opt('-regress_compute_tsnr', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute TSNR datasets (yes/no) after regress')
        self.valid_opts.add_opt('-regress_make_cbucket', 1, [],
                        acplist=['yes','no'],
                        helpstr="request cbucket dataset of all betas (yes/no)")

        self.valid_opts.add_opt('-regress_censor_extern', 1, [],
                        helpstr="apply external censor file")
        self.valid_opts.add_opt('-regress_censor_motion', 1, [],
                        helpstr="censor TR if motion derivative exceeds limit")
        self.valid_opts.add_opt('-regress_censor_prev', 1, [],
                        acplist=['yes','no'],
                        helpstr="set whether to censor previous motion TR")
        self.valid_opts.add_opt('-regress_censor_first_trs', 1, [],
                        helpstr="censor first TRs per run (if censor motion)")
        self.valid_opts.add_opt('-regress_censor_outliers', 1, [],
                        helpstr="censor TR if outlier fraction exceeds limit")
        self.valid_opts.add_opt('-regress_skip_first_outliers', 1, [],
                        helpstr="ignore outliers in first few TRs of each run")

        self.valid_opts.add_opt('-regress_fout', 1, [],
                        acplist=['yes','no'],
                        helpstr="output individual F-stats? (def: yes)")
        self.valid_opts.add_opt('-regress_polort', 1, [],
                        helpstr="baseline polynomial degree per run")
        self.valid_opts.add_opt('-regress_stim_files', -1, [], okdash=0,
                        helpstr="0/1 or pre-convolved stimulus files")
        self.valid_opts.add_opt('-regress_stim_labels', -1, [], okdash=0,
                        helpstr="labels for specified regressors")
        self.valid_opts.add_opt('-regress_stim_times', -1, [], okdash=0,
                        helpstr="stimulus timing files")
        self.valid_opts.add_opt('-regress_no_stim_times', 0, [],
                        helpstr="do not convert stim_files to timing")
        self.valid_opts.add_opt('-regress_stim_times_offset', 1, [],
                        helpstr="add offset when converting to timing")
        self.valid_opts.add_opt('-regress_stim_types', -1, [], okdash=0,
                        acplist=['times', 'AM1', 'AM2', 'IM', 'file'],
                        helpstr="specify times/AM1/AM2/IM for each stim class")
        self.valid_opts.add_opt('-regress_use_stim_files', 0, [],
                        helpstr="do not convert stim_files to timing")
        self.valid_opts.add_opt('-regress_use_tproject', 1, [],
                        acplist=['yes','no'],
                        helpstr="use 3dTproject instead of 3dDeconvolve")

        self.valid_opts.add_opt('-regress_apply_mot_types', -1, [],
                        acplist=['basic','demean','deriv'],
                        helpstr="specify which motion parameters to apply")
        self.valid_opts.add_opt('-regress_mot_as_ort', 1, [],
                        acplist=['yes','no'],
                        helpstr="apply motion params via -ortvec")
        self.valid_opts.add_opt('-regress_motion_file', 1, [],
                        helpstr="external file to apply as motion regressors")
        self.valid_opts.add_opt('-regress_motion_per_run', 0, [],
                        helpstr="apply all motion parameters per run")
        self.valid_opts.add_opt('-regress_no_motion_demean', 0, [],
                        helpstr="do not compute demeaned motion params")
        self.valid_opts.add_opt('-regress_no_motion_deriv', 0, [],
                        helpstr="do not compute motion param derivatives")

        self.valid_opts.add_opt('-regress_extra_stim_files', -1, [], okdash=0,
                        helpstr="extra -stim_files to apply")
        self.valid_opts.add_opt('-regress_extra_stim_labels', -1, [], okdash=0,
                        helpstr="labels for extra -stim_files")

        self.valid_opts.add_opt('-regress_compute_fitts', 0, [],
                        helpstr="compute fitts only after 3dDeconvolve")
        self.valid_opts.add_opt('-regress_est_blur_epits', 0, [],
                        helpstr="estimate blur from scaled EPI time series")
        self.valid_opts.add_opt('-regress_est_blur_errts', 0, [],
                        helpstr="estimate blur from scaled error time series")
        self.valid_opts.add_opt('-regress_errts_prefix', 1, [],
                        helpstr="prefix to use for errts dataset")
        self.valid_opts.add_opt('-regress_fitts_prefix', 1, [],
                        helpstr="prefix to use for fitts dataset")
        self.valid_opts.add_opt('-regress_iresp_prefix', 1, [],
                        helpstr="prefix to use for iresp datasets")
        self.valid_opts.add_opt('-regress_global_times', 0, [],
                        helpstr="apply -global_times option to 3dDeconvolve")
        self.valid_opts.add_opt('-regress_local_times', 0, [],
                        helpstr="apply -local_times option to 3dDeconvolve")
        self.valid_opts.add_opt('-regress_make_ideal_sum', 1, [],
                        helpstr="filename for sum of ideal regressors")
        self.valid_opts.add_opt('-regress_no_ideal_sum', 0, [],
                        helpstr="do not compute the sum of regressors")
        self.valid_opts.add_opt('-regress_no_fitts', 0, [],
                        helpstr="do not output a fit timeseries dataset")
        self.valid_opts.add_opt('-regress_no_ideals', 0, [],
                        helpstr="do not generate ideal regressors")
        self.valid_opts.add_opt('-regress_no_iresp', 0, [],
                        helpstr="do not output impulse reponse datasets")
        self.valid_opts.add_opt('-regress_no_mask', 0, [],
                        helpstr="do not apply any mask during regression")
        self.valid_opts.add_opt('-regress_no_motion', 0, [],
                        helpstr="do not apply motion parameters in regression")
        self.valid_opts.add_opt('-regress_opts_3dD', -1, [],
                        helpstr='additional options directly to 3dDeconvolve')
        self.valid_opts.add_opt('-regress_opts_reml', -1, [],
                        helpstr='additional options directly to 3dREMLfit')
        self.valid_opts.add_opt('-regress_reml_exec', 0, [],
                        helpstr="execute 3dREMLfit command script")
        self.valid_opts.add_opt('-regress_ROI', -1, [], okdash=0,
                        helpstr="regress out known ROIs")
        self.valid_opts.add_opt('-regress_RONI', -1, [], okdash=0,
                        helpstr="1-based list of regressors of no interest")
        self.valid_opts.add_opt('-regress_RSFC', 0, [],
                        helpstr="use 3dRSFC to bandpass and output params")

        # surface options
        self.valid_opts.add_opt('-surf_anat', 1, [],
                        helpstr="specify SurfVol dataset")
        self.valid_opts.add_opt('-surf_spec', -1, [], okdash=0,
                        helpstr="list lh and/or rh surface spec file(s)")
        self.valid_opts.add_opt('-surf_anat_aligned', 1, [],
                        acplist=['yes','no'],
                        helpstr="is surface anat aligned to current session")
        self.valid_opts.add_opt('-surf_anat_has_skull', 1, [],
                        acplist=['yes','no'],
                        helpstr="does surface anat still have skull")
        self.valid_opts.add_opt('-surf_A', 1, [],
                        helpstr="list surf_A surface (e.g. smoothwm)")
        self.valid_opts.add_opt('-surf_B', 1, [],
                        helpstr="list surf_B surface (e.g. pial)")
        self.valid_opts.add_opt('-surf_blur_fwhm', 1, [],
                        helpstr="NO LONGER VALID, please use -blur_size")

        # 3dClustSim options
        self.valid_opts.add_opt('-regress_CS_NN', 1, [],
                        acplist=['1','2','3','12','13','23','123'],
                        helpstr='specify subset of {1,2,3} (default=123)')
        self.valid_opts.add_opt('-regress_opts_CS', -1, [],
                        helpstr='additional options directly to 3dClustSim')
        self.valid_opts.add_opt('-regress_run_clustsim', 1, [],
                        acplist=['yes','no'],
                        helpstr="add 3dClustSim attrs to regression bucket")

        self.valid_opts.trailers = 0   # do not allow unknown options
        
    def get_user_opts(self):
        self.valid_opts.check_special_opts(self.argv)
        self.user_opts = read_options(self.argv, self.valid_opts)
        if self.user_opts == None: return 1     # error condition
        if len(self.user_opts.olist) == 0:      # no options: apply -help
            print g_help_string
            return 0
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt: print "** seem to have trailers, but cannot find them!"
            else: print "** have invalid trailing args: %s", opt.show()
            return 1  # failure

        # maybe the users justs wants a complete option list
        if self.user_opts.find_opt('-show_valid_opts'):
            self.valid_opts.show('', 1)
            return 0  # gentle termination
        
        # apply the user options
        rv = self.apply_initial_opts(self.user_opts)
        if rv != None: return rv

        # update out_dir now (may combine option results)
        if self.out_dir == '': self.out_dir = '%s.results' % self.subj_label
        self.od_var = '$output_dir'

        if self.verb > 1: show_args_as_command(self.argv, "executing command:")
        if self.verb > 4: show_args_as_command(sys.argv, "system command:")
        if self.verb > 1: self.show('end get_user_opts ')

    # apply the general options - many terminate program
    def apply_initial_opts(self, opt_list):
        opt = opt_list.find_opt('-verb')   # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        if opt_list.find_opt('-help'):     # just print help
            print g_help_string
            return 0  # gentle termination
        
        if opt_list.find_opt('-hist'):     # print the history
            print g_history
            return 0  # gentle termination
        
        if opt_list.find_opt('-requires_afni_version'): # print required version
            print g_requires_afni
            return 0  # gentle termination
        
        if opt_list.find_opt('-ver'):      # show the version string
            print g_version
            return 0  # gentle termination
        
        # options which are NO LONGER VALID

        if opt_list.find_opt('-surf_blur_fwhm'):
            print '** option -surf_blur_fwhm is no longer valid\n' \
                  '   (please stick with -blur_size)\n'
            return 1

        # end terminal options

        # options that imply other options
        if opt_list.find_opt('-regress_anaticor'):
           opt_list.add_opt("-mask_segment_anat", 1, ["yes"], setpar=1)
           opt_list.add_opt("-mask_segment_erode", 1, ["yes"], setpar=1)

        # end options that imply other options

        # end terminal options

        opt = opt_list.find_opt('-check_results_dir')
        if opt_is_no(opt): self.check_rdir = 'no'

        opt = opt_list.find_opt('-check_setup_errors')
        if opt and opt.parlist[0] == 'yes': self.check_setup_errors = 1
        else:                               self.check_setup_errors = 0

        opt = opt_list.find_opt('-exit_on_error')
        if opt and opt.parlist[0] == 'no': self.exit_on_error = 0
        else:                              self.exit_on_error = 1

        opt = opt_list.find_opt('-anat_has_skull')      # 4 Oct, 2011
        if opt != None:
            if opt.parlist[0] == 'no': self.anat_has_skull = 0
            else:                      self.anat_has_skull = 1

        opt = opt_list.find_opt('-copy_anat')
        if opt != None:
            self.anat = afni_name(opt.parlist[0])
            # rcr - set only if no view in anat?  (though still would not know)
            self.tlrcanat = self.anat.new(new_view='+tlrc')

        opt = opt_list.find_opt('-gen_epi_review')  # name epi review script
        if opt != None: self.epi_review = opt.parlist[0]

        opt = opt_list.find_opt('-no_epi_review') # no epi review script
        if opt != None: self.epi_review = None

        opt = opt_list.find_opt('-keep_rm_files')
        if opt != None: self.rm_rm = 0

        opt = opt_list.find_opt('-out_dir')
        if opt != None: self.out_dir = opt.parlist[0]

        opt = opt_list.find_opt('-sep_char') # -- in default output names
        if opt != None:
            self.sep_char = opt.parlist[0]
            if self.sep_char == '_': self.subj_label = '${subj}'

        # if sep_char changes, might need this
        if opt_list.find_opt('-subj_curly'): self.subj_label = '${subj}'

        opt = opt_list.find_opt('-subj_id') # -- needs to be before script
        if opt != None: self.subj_id = opt.parlist[0]

        opt = opt_list.find_opt('-script')
        if opt != None: self.script = opt.parlist[0]
        else:           self.script = 'proc.%s' % self.subj_id

        opt = opt_list.find_opt('-write_3dD_prefix')
        if opt != None:
           self.prefix_3dD = opt.parlist[0]

        opt = opt_list.find_opt('-write_3dD_script')
        if opt != None:
           self.script_3dD = opt.parlist[0]
           self.make_main_script = 0

        opt = opt_list.find_opt('-scr_overwrite')
        if opt != None: self.overwrite = 1

        # do we check input datasets for existence?  default to yes
        opt = opt_list.find_opt('-test_for_dsets')
        if not opt or opt_is_yes(opt): self.test_dsets = 1
        else:                          self.test_dsets = 0

        # do we test stim files for validity?  default to yes
        opt = opt_list.find_opt('-test_stim_files')
        if not opt or opt_is_yes(opt): self.test_stims = 1
        else:                          self.test_stims = 0


    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        # first, note datasets
        opt = self.user_opts.find_opt('-dsets')
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))

            # possibly verify that all of the input datasets exist
            if self.test_dsets:
                missing = 0
                for dset in self.dsets:
                    if not dset.exist():
                        print '** missing dataset: %s' % dset.rpv()
                        missing = 1
                if missing: return 1

            # and check for EPI view
            if self.dsets[0].view and self.dsets[0].view != self.view:
                self.view = self.dsets[0].view
                self.origview = self.view
                if self.verb > 0: print '-- applying view as %s' % self.view
            elif self.dsets[0].view == '':
                view = dset_view(self.dsets[0].ppve())
                self.view = view
                self.origview = self.view
                if self.verb>0: print '-- applying orig view as %s' % self.view

        # next, check for -surf_anat, which defines whether to do volume
        # or surface analysis
        opt = self.user_opts.find_opt('-surf_anat')
        if opt != None:
           self.surf_anat = afni_name(opt.parlist[0])

        # init block either from DefLabels or -blocks
        opt = self.user_opts.find_opt('-blocks')
        if opt:  # then create blocklist from user opts (but prepend tcat)
            if opt.parlist[0] != 'tcat':
                blocks = ['tcat'] + opt.parlist
            else: blocks = opt.parlist
        elif self.surf_anat: blocks = DefSurfLabs  # surface defaults
        else:                blocks = DefLabels    # volume defaults

        # and insert automatic postdata block at postion 1 (after tcat)
        blocks.insert(1, 'postdata')

        # just do a quick check after all of the confusion
        if blocks[0] != 'tcat' or blocks[1] != 'postdata':
           print '** block list should start with tcat,postdata, have:\n   %s'\
                 % blocks
           return 1

        # check for -do_block options
        opt = self.user_opts.find_opt('-do_block')
        if opt and opt.parlist and len(opt.parlist) > 0:
            #if self.user_opts.find_opt('-blocks'):
            #    print '** error: -do_block invalid when using -blocks'
            #    return 1

            # ****
            # if no -blocks option, but a long list of -do_block, maybe start
            # from normal blocks subset of -do_block list

            # check additional blocks one by one
            errs = 0
            for bname in opt.parlist:
                err, blocks = self.add_block_to_list(blocks, bname)
                errs += err
            if errs > 0 : return 1

        # allow for -tlrc_anat option
        opt = self.user_opts.find_opt('-tlrc_anat')
        if opt and not self.find_block('tlrc'):
            if self.user_opts.find_opt('-volreg_tlrc_warp') or  \
               self.user_opts.find_opt('-volreg_tlrc_adwarp') :
                err, blocks = self.add_block_before_label(blocks,
                                        'tlrc', 'volreg')
            else: err, blocks = self.add_block_to_list(blocks, 'tlrc')
            if err: return 1

        # if user has supplied options for blocks that are not used, fail
        if self.opts_include_unused_blocks(blocks, 1): return 1

        # check for unique blocks (except for 'empty')
        if not self.blocks_are_unique(blocks):
            print '** blocks must be unique\n'  \
                  '   (is there overlap between -blocks and -do_block?)\n'
            return 1

        # call db_mod_functions

        self.bindex = 0
        self.pblabels = []
        for label in blocks:
            rv = self.add_block(label)
            if rv != None: return rv
            if label not in EPInomodLabs:
                self.pblabels.append(label)
                self.bindex += 1

        # maybe the user wants to be quizzed for options
        uopt = self.user_opts.find_opt('-ask_me')
        if uopt != None:
            if ask_me.ask_me_subj_proc(self):
                return 1
            for label in blocks:
                block = self.find_block(label)
                if not block:
                    print "** error: missing block '%s' in ask_me update"%label
                    return 1
                BlockModFunc[label](block, self, self.user_opts) # modify block

        # rcr - if there is another place to update options from, do it
        uopt = self.user_opts.find_opt('-opt_source')
        if uopt != None:
            print '** not ready to update options from %s' % str(uopt.parlist)
            return 1

        # do some final error checking
        if len(self.dsets) == 0:
            print 'error: dsets have not been specified (consider -dsets)'
            return 1

        # no errors, just warn the user (for J Britton)   25 May 2011
        uniq_list_as_dsets(self.dsets, 1)
        self.check_block_order()

    def add_block_to_list(self, blocks, bname, adj=None, dir=0):
        """given current block list, add a block for bname after that
           of prevlab or from OtherDefLabels if None

                blocks : current list of block labels
                bname  : label of block to insert
                adj    : name of adjacent block (if None, try to decide)
                dir    : if adj, dir is direction of bname to adj
                         (-1 : bname is before, 1: bname is after)
           
           return error code and new list"""

        # if we are not given an adjacent block, try to find one
        if not adj:
            dir, adj = self.find_best_block_posn(blocks, bname)
            if not dir: return 1, blocks

        # good cases
        if dir < 0: return self.add_block_before_label(blocks, bname, adj)
        if dir > 0: return self.add_block_after_label(blocks, bname, adj)

        # failure
        print "** ABTL: have adj=%s but no dir" % adj
        return 1, blocks


    def find_best_block_posn(self, blocks, bname):
        """decide where it is best to insert the block bname
           return dir, nextto
                dir    = -1,0,1 means before, error, after
                nextto = name of relevant adjacent block"""

        if bname == 'ricor':
            try: ind = blocks.index('despike')
            except: ind = -1
            if ind >= 0: return 1, 'despike'    # after despike

            return 1, 'postdata'                # else, just after postdata

        if bname == 'align':
            try: ind = blocks.index('tlrc')
            except: ind = -1
            if ind >= 0: return -1, 'tlrc'      # before tlrc
            try: ind = blocks.index('volreg')
            except: ind = -1
            if ind >= 0: return -1, 'volreg'    # before volreg
            try: ind = blocks.index('tshift')
            except: ind = -1
            if ind >= 0: return 1, 'tshift'     # after tshift

            return  1, 'postdata'               # else, stick at beginning

        if bname == 'tlrc':
            try: ind = blocks.index('volreg')
            except: ind = -1
            if ind >= 0: return -1, 'volreg'    # before volreg
            try: ind = blocks.index('align')
            except: ind = -1
            if ind >= 0: return 1, 'align'      # after align
            return 1, blocks[-1]                # stick it at the end


        # if those didn't apply, go with the OtherDefLabels array

        try: prevlab = OtherDefLabels[bname]
        except:
            print "** failed to find position for block '%s' in %s" \
                  % (bname, blocks)
            prevlab = ''
        
        if prevlab == '': return 0, prevlab     # failure
        else:             return 1, prevlab     # success

    def add_block_after_label(self, blocks, bname, prelab):
        """add a block for bname after that of prelab
           
           return error code and new list"""
        err = 0
        if not bname in BlockLabels:
            print "** ABAL error: block '%s' is invalid" % bname
            return 1, blocks
        if not prelab in BlockLabels:
            print "** ABAL error: prelab '%s' is invalid" % prelab
            return 1, blocks
        try: preindex = blocks.index(prelab)
        except:     
            print "** cannot find block '%s' to insert block '%s' after" \
                  % (prelab, bname)
            preindex = 0
            err = 1
        if preindex < 0:
            print "** error: blocks.index failure for '%s'" % prelab
            err = 1
        
        if err: return 1, blocks

        # else add the block to blocklist
        preindex += 1
        blocks[preindex:preindex] = [bname]

        return 0, blocks

    def add_block_before_label(self, blocks, bname, postlab):
        """add a block for bname before that of postlab
           
           return error code and new list"""
        err = 0
        if not bname in BlockLabels:
            print "** ABBL error: block '%s' is invalid" % bname
            return 1, blocks
        if not postlab in BlockLabels:
            print "** ABBL error: postlab '%s' is invalid" % postlab
            return 1, blocks
        try: postindex = blocks.index(postlab)
        except:     
            print "** cannot find block '%s' to insert block '%s' before" \
                  % (postlab, bname)
            postindex = 0
            err = 1
        if postindex < 0:
            print "** error: blocks.index failure for '%s'" % postlab
            err = 1
        
        if err: return 1, blocks

        # else add the block to blocklist
        blocks[postindex:postindex] = [bname]

        return 0, blocks

    # create script from blocks and options
    def create_script(self):
        if self.overwrite_error(): return 0

        rv = self.get_run_info()
        if rv != None: return rv

        rv = self.init_script()         # create the script file and header
        if rv != None: return rv

        errs = 0
        for block in self.blocks:
            cmd_str = BlockCmdFunc[block.label](self, block)
            if cmd_str == None:
                print "** script creation failure for block '%s'" % block.label
                errs += 1
            else:
                if block.post_cstr != '':
                   if self.verb > 2:
                      print '++ adding post_cstr to block %s:\n%s=======' \
                            (block.label, block.post_cstr)
                   cmd_str += block.post_cstr
                self.write_text(add_line_wrappers(cmd_str))
                if self.verb>3: block.show('+d post command creation: ')
                if self.verb>4: print '+d %s cmd: \n%s'%(block.label, cmd_str)

        if self.epi_review:
            cmd_str = db_cmd_gen_review(self)
            if cmd_str:
                self.write_text(add_line_wrappers(cmd_str))
                if self.verb > 1:
                    print "+d generated EPI review script %s" % self.epi_review
            else:
                errs += 1
                if self.verb > 1:
                    print '** failed to generate EPI review script'

        rv = self.finalize_script()     # finish the script
        if rv: errs += 1

        self.close_script()
        self.make_exec()

        if errs > 0:
            # default to removing any created script
            opt = self.user_opts.find_opt('-keep_script_on_err')
            if not opt or opt_is_no(opt):
                os.remove(self.script)
            return 1    # so we print all errors before leaving

        self.report_final_messages()

    def report_final_messages(self):
        """check over various conditions"""

        if self.verb > 0:
            # last warning, if user is masking EPI data...
            if self.mask != None:
                if self.regmask:
                  if self.mask != self.mask_group:
                    print "** masking single subject EPI is not recommended"
                    print "   (see 'MASKING NOTE' from the -help for details)"
                else:
                    print "-- using default: will not apply EPI Automask"
                    print "   (see 'MASKING NOTE' from the -help for details)"

            if self.ricor_nreg > 0 and self.ricor_apply == 'no':
                if not self.user_opts.find_opt('-regress_apply_ricor'):
                    print '** note: ricor regressors are no longer applied' \
                              ' in final regresion'

            if self.runs == 1:
                print "\n-------------------------------------\n" \
                        "** warning have only 1 run to analyze\n" \
                        "-------------------------------------"

            print "\n--> script is file: %s" % self.script
            print   '    consider the script execution command: \n\n' \
                    '      %s\n' % self.exec_cmd

        return

    def get_run_info(self):
        self.runs = len(self.dsets)
        if self.runs < 1:
            print "** have no dsets to analyze"
            return 1

        # updated by 'tcat' opteration (and -remove_trs option)
        # (use rpve to include NIfTI, etc.)
        dset = self.dsets[0].rpve()

        err, self.reps, self.tr = get_dset_reps_tr(dset, self.verb)
        if err: return 1   # check for failure

        # set reps in each run
        self.reps_all = []
        self.reps_vary = 0
        for dr in self.dsets:
            err, reps, tr = get_dset_reps_tr(dr.rpve(), self.verb)
            if err: return 1
            self.reps_all.append(reps)
            if reps != self.reps: self.reps_vary = 1
            if tr != self.tr:
                print '** TR of %g != run #1 TR %g' % (tr, self.tr)
                return 1

        # note data type and whether data is scaled
        err, vlist = get_typed_dset_attr_list(dset, "BRICK_TYPES", int, verb=0)
        if not err and len(vlist) >= 1:
            self.datatype = vlist[0]

        err, vlist = get_typed_dset_attr_list(dset, "BRICK_FLOAT_FACS", int,
                                              verb=0)
        if not err and len(vlist) >= 1:
            if vals_are_constant(vlist, 0) or vals_are_constant(vlist, 1):
                self.scaled = 0
            else:
                self.scaled = 1

        if self.verb > 1:
            print '-- reps = %g, tr = %g, datatype = %g, scaled = %d' \
                  % (self.reps, self.tr, self.datatype, self.scaled)

    def get_vr_base_indices(self, verb=1):
        """return 0-based run and TR indices for volreg base
           (return runs==-1 if they cannot be set)
           if verb > 0 and values cannot be set, print why not"""
        
        block = self.find_block('volreg')
        if not block:
            if verb > 0: print "** cannot get vr_base_indices: no volreg block"
            return -1, -1
        opt = block.opts.find_opt('-volreg_base_ind')

        if not opt:
            if self.verb > 2: print '-- no -volreg_base_ind opt for vr_base'
            if len(self.reps_all) == self.runs:
               return self.runs-1, self.reps_all[-1]-1  # defaults
            return self.runs-1, self.reps-1  # defaults

        # if parlist values are -1, set to last TR
        if opt.parlist[0] < 0 or opt.parlist[1] < 0:
            # if going after last volume, maybe run lengths vary
            if len(self.reps_all) == self.runs:
               return self.runs-1, self.reps_all[-1]-1  # defaults
            return self.runs-1, self.reps-1

        return opt.parlist[0], opt.parlist[1]

    # create a new block for the given label, and append it to 'blocks'
    def add_block(self, label):
        block = ProcessBlock(label, self)
        if not block.valid:
            print '** invalid block : %s' % block.label
            return 1
        if self.verb > 3: block.show('+d post init block: ')
        self.blocks.append(block)

    def find_block(self, label, sind=0):
        for bind in range(0, len(self.blocks)):
            block = self.blocks[bind]
            if block.label == label: return block
        return None

    def find_block_opt(self, label, opt_name):
        """return any found comompt instance in block.opts"""
        for block in self.blocks:
            if block.label == label: return block.opts.find_opt(opt_name)
        return None

    def find_block_index(self, label):
        block = self.find_block(label)
        if block: return self.blocks.index(block)
        return None

    def blocks_are_unique(self, blocks, exclude_list=['empty'], whine=1):
        """return whether the blocs are unique
                - ignore blocks in exclude_list
                - if whine, complain on repeats
           Process one by one, so we know what to complain about.
        """

        unique = 1
        checked = []
        for block in blocks:
            if block in exclude_list: continue
            if block in checked:
                if whine: print "** warning: block '%s' used multiple times" \
                                % block
                unique = 0
            else: checked.append(block)

        return unique

    def opts_include_unused_blocks(self, blocks, whine=1):
        """return whether options refer to blocks that are not being used"""

        # start with all BlockLabels and remove those in passed list
        badlist = BlockLabels[:]
        for label in blocks:
            if label in badlist: badlist.remove(label)

        if self.verb > 3: print '++ unused blocks: %s' % badlist

        # for speed, make an explicit list of option prefixes to search for
        badlist = ['-%s_' % label for label in badlist]

        errs = 0
        for opt in self.user_opts.olist:
            ind = opt.name.find('_')
            if ind < 0: continue
            if opt.name[0:ind+1] in badlist:
                if whine: print "** missing '%s' block for option '%s'" \
                                % (opt.name[1:ind], opt.name)
                errs += 1

        return errs

    def check_block_order(self):
        """whine about any disliked block orderings
                - despike < ricor < tshift/volreg
                - blur < scale
                - tshift/volreg/blur/scale < regress
           return the number of errors
        """
        errs = 0
        if self.find_block('align'):
            if not self.blocks_ordered('align', 'volreg'):
                errs += 1
                print "** warning: 'align' should preceed 'volreg'"
            if not self.blocks_ordered('align', 'tlrc'):
                errs += 1
                print "** warning: 'align' should preceed 'tlrc'"
        if self.find_block('ricor'):
            if not self.blocks_ordered('despike', 'ricor', must_exist=1):
                errs += 1
                print "** warning: 'despike' should preceed 'ricor'"
            if not self.blocks_ordered('ricor', 'tshift'):
                errs += 1
                print "** warning: 'tshift' should preceed 'ricor'"
            if not self.blocks_ordered('ricor', 'volreg'):
                errs += 1
                print "** warning: 'volreg' should preceed 'ricor'"
        if self.find_block('blur'):
            if not self.blocks_ordered('blur', 'scale'):
                errs += 1
                print "** warning: 'blur' should preceed 'scale'"
        if self.find_block('regress'):
            if not self.blocks_ordered('tshift', 'regress'):
                errs += 1
                print "** warning: 'tshift' should preceed 'regress'"
            if not self.blocks_ordered('volreg', 'regress'):
                errs += 1
                print "** warning: 'volreg' should preceed 'regress'"
            if not self.blocks_ordered('blur', 'regress'):
                errs += 1
                print "** warning: 'blur' should preceed 'regress'"
            if not self.blocks_ordered('scale', 'regress'):
                errs += 1
                print "** warning: 'scale' should preceed 'regress'"

        return errs

    def blocks_ordered(self, name0, name1, must_exist=0):
        """check that the name0 block comes before the name1 block

              - something missing means order is okay
              - must_exist implies both must exist, else print message
                (often one has been check already)

           return 0 only if both exist and are out of order"""

        bind0 = self.find_block_index(name0)
        bind1 = self.find_block_index(name1)
        if bind0 < 0 or bind1 < 0: # something is missing
            if must_exist:
                print '** warning: missing block',
                if bind0 < 0: print "'%s' (to preceed '%s')" % (name0,name1)
                if bind1 < 0: print "'%s' (to follow '%s')" % (name1,name0)
            return 1
        elif bind0 < bind1: return 1
        else:               return 0

    # set subj shell variable, check output dir, create and cd
    def init_script(self):
        if self.open_script(): return 1

        if self.exit_on_error: topt = ' -xef'
        else:                  topt = ' -x'
        self.write_text('#!/bin/tcsh%s\n\n' % topt)
        self.write_text('echo "auto-generated by afni_proc.py, %s"\n' \
                        % asctime())
        self.write_text('echo "(%s)"\n'% g_version)
        self.write_text('echo "execution started: `date`"\n\n')

        # include execution method in script
        if self.exit_on_error: opts = '-xef'
        else:                  opts = '-x'

        # store both tcsh and bash versions
        self.bash_cmd = 'tcsh %s %s 2>&1 | tee output.%s' % \
                        (opts, self.script, self.script)
        self.tcsh_cmd = 'tcsh %s %s |& tee output.%s'     % \
                        (opts, self.script, self.script)

        if self.user_opts.find_opt('-bash'): self.exec_cmd = self.bash_cmd
        else:                                self.exec_cmd = self.tcsh_cmd

        self.write_text('# execute via : \n'      \
                        '#   %s\n\n' % self.exec_cmd)

        # maybe the user want to check the status of the init operations
        if not self.check_setup_errors: stat_inc = ''
        else: stat_inc = '@ nerrors += $status      # accumulate error count\n'

        self.write_text('# %s\n'  \
                      '# script setup\n\n' % block_header('auto block: setup'))

        if len(stat_inc) > 0:
            self.write_text("# prepare to count setup errors\n" \
                            "set nerrors = 0\n\n")

        # possibly check the AFNI version (via afni_history)
        opt = self.user_opts.find_opt('-check_afni_version')
        if not opt or opt_is_yes(opt):
          self.write_text('# take note of the AFNI version\n' \
                          'afni -ver\n\n')
          self.write_text(                                                    \
          '# check that the current AFNI version is recent enough\n'          \
          'afni_history -check_date %s\n'                                     \
          'if ( $status ) then\n'                                             \
          '    echo "** this script requires newer AFNI binaries (than %s)"\n'\
          '    echo "   (consider: @update.afni.binaries -defaults)"\n'       \
          '    exit\n'                                                        \
          'endif\n\n' % (g_requires_afni, g_requires_afni) )

        self.write_text('# the user may specify a single subject to run with\n'\
                      'if ( $#argv > 0 ) then\n'                             \
                      '    set subj = $argv[1]\n'                            \
                      'else\n'                                               \
                      '    set subj = %s\n'                                  \
                      'endif\n\n' % self.subj_id )
        self.write_text('# assign output directory name\n'
                        'set output_dir = %s\n\n' % self.out_dir)
        if self.check_rdir == 'yes':
           self.write_text( \
                '# verify that the results directory does not yet exist\n'\
                'if ( -d %s ) then\n'                                     \
                '    echo output dir "$subj.results" already exists\n'    \
                '    exit\n'                                              \
                'endif\n\n' % self.od_var)
        self.write_text('# set list of runs\n')
        digs = 2
        if self.runs > 99: digs = 3
        self.write_text('set runs = (`count -digits %d 1 %d`)\n\n' \
                        % (digs,self.runs) )

        self.write_text('# create results and stimuli directories\n')
        self.write_text('mkdir %s\nmkdir %s/stimuli\n%s\n' \
                        % (self.od_var, self.od_var, stat_inc))

        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
            tstr = '# copy stim files into stimulus directory\ncp'
            for ind in range(len(self.stims)):
                tstr += ' %s' % self.stims_orig[ind]
            tstr += ' %s/stimuli\n' % self.od_var
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        if len(self.extra_stims) > 0: # copy extra stim files into stim dir
            tstr = '# copy extra stim files\n'   \
                  'cp %s %s/stimuli\n' %        \
                  (' '.join(self.extra_stims_orig), self.od_var)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        if self.anat:
            tstr = '# copy anatomy to results dir\n'     \
                  '3dcopy %s %s/%s\n' %                 \
                      (self.anat.rel_input(), self.od_var, self.anat.prefix)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

            # further use should assume AFNI format
            self.anat.to_afni(new_view=dset_view(self.anat.ppve()))
            self.tlrcanat.to_afni()
            self.anat_final = self.anat

        # possibly copy over any volreg base
        if self.vr_ext_base != None:
            tstr = "# copy over the external volreg base\n"  \
                  "3dbucket -prefix %s/%s '%s'\n" %         \
                  (self.od_var, self.vr_ext_pre, self.vr_ext_base)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        # possibly copy over any align EPI base
        if self.align_ebase != None:
            tstr = "# copy over the external align_epi_anat.py EPI volume\n" \
                  "3dbucket -prefix %s/%s '%s'\n" %         \
                  (self.od_var, self.align_epre, self.align_ebase)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        opt = self.user_opts.find_opt('-regress_motion_file')
        if opt and len(opt.parlist) > 0:
            tstr = '# copy external motion file into results dir\n' \
                  'cp %s %s\n' %                                   \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        opt = self.user_opts.find_opt('-regress_censor_extern')
        if opt and len(opt.parlist) > 0:
            fname = opt.parlist[0]
            tstr = '# copy external censor file into results dir\n' \
                  'cp %s %s\n' % (fname,self.od_var)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)
            self.censor_file = os.path.basename(fname)
            self.censor_count += 1
            if self.verb > 1:
                print '++ copying external censor file to %s'%self.censor_file

        opt = self.user_opts.find_opt('-copy_files')
        if opt and len(opt.parlist) > 0:
            tstr = '# copy extra files into results dir\n' \
                  'cp -rv %s %s\n' %                      \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        # copy ricor_regs last to possibly match 3dTcat TR removal
        if len(self.ricor_regs) > 0:
            tstr = copy_ricor_regs_str(self)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        if len(stat_inc) > 0:
            self.write_text("# check for any setup failures\n"
                            "if ( $nerrors > 0 ) then\n"
                            "    echo '** setup failure ($nerrors errors)'\n"
                            "    exit\n"
                            "endif\n\n")

        self.flush_script()

    # and last steps
    def finalize_script(self):
        tstr = '# %s\n\n' % block_header('auto block: finalize')
        self.write_text(tstr)

        if self.rm_rm and self.have_rm and len(self.rm_list) > 0:
            if self.rm_dirs: ropt = 'r'
            else:            ropt = ''
            # make a list of things to delete, starting with rm.*
            delstr = ' '.join(self.rm_list)
            self.write_text('# remove temporary files\n'
                            '\\rm -f%s %s\n\n' % (ropt, delstr))

        # move or remove pre-processing files (but not rm files)
        # (2 sets: removal and move)
        if self.surf_anat: proc_files = 'pb*.$subj.[lr]*'
        else:              proc_files = 'pb*.$subj.r*.*'

        rm_files = 'dfile.r*.1D %s' % proc_files
        mv_files = 'outcount.*'
        if self.user_opts.find_opt('-move_preproc_files'):
            cmd_str = \
              "# move preprocessing files to 'preproc.data' directory\n"   \
              "mkdir preproc.data\n"                                       \
              "mv %s %s preproc.data\n\n"                                  \
              % (rm_files, mv_files)
            self.write_text(add_line_wrappers(cmd_str))
        elif self.user_opts.find_opt('-remove_preproc_files'):
            cmd_str = "# remove preprocessing files to save disk space\n"   \
                      "\\rm %s\n\n" % rm_files
            self.write_text(add_line_wrappers(cmd_str))

        # at the end, if the basic review script is here, run it
        if self.epi_review:
           ss = '# if the basic subject review script is here, run it\n' \
                '# (want this to be the last text output)\n'             \
                'if ( -e %s ) ./%s |& tee out.ss_review.$subj.txt\n\n'   \
                % (self.ssr_basic, self.ssr_basic)
           self.write_text(ss)

        cmd_str = self.script_final_error_checks()
        if cmd_str: 
           if self.out_wfile:
              self.write_text(cmd_str)

        self.write_text('# return to parent directory\n'
                        'cd ..\n\n')

        self.write_text('echo "execution finished: `date`"\n\n')

        # and append execution command, for a record
        opt = self.user_opts.find_opt('-no_proc_command')
        if not opt:
            tstr = UTIL.get_command_str(args=self.argv)
            self.write_text('\n\n' + tstr)

        if self.user_opts.find_opt('-ask_me'):
            tstr = '#\n# all applied options: '
            for opt in self.user_opts.olist:
                if opt.name == '-ask_me': continue
                tstr += opt.name+' '
                tstr += ' '.join(quotize_list(opt.parlist,''))
            tstr += '\n'
            self.write_text(add_line_wrappers(tstr))

    def script_final_error_checks(self):
        """script for checking any errors that should be reported
           at the end of processing
        """
        cmd = ''

        # pre-steady state errors are checked in @ss_review_basic

        return cmd

    def write_text(self, tstr):
        """control write to output file

           If not actually creating a main script (e.g. just 3dD), return.
        """

        if not self.make_main_script: return 0

        self.fp.write(tstr)

        return 0

    def overwrite_error(self, report=1):
        """is it valid to create the output script?"""
        if self.make_main_script and os.path.isfile(self.script) \
                                 and not self.overwrite:
            if report:
               print "error: script file '%s' already exists" % self.script
            return 1

        return 0

    def open_script(self):
        """open script for writing

           - if we are not actually creating a script, return
           - if bad overwrite, fail
           - try to open
        """

        if not self.make_main_script: return 0

        if self.overwrite_error():
            print "exiting..." 
            return 1

        try: self.fp = open(self.script,'w')
        except:
            print "cannot open script file '%s' for writing" % self.script
            return 1

        return 0

    def close_script(self):
        """close self.fp, if it is open (and if we are making one)"""

        if not self.make_main_script: return 0

        if self.fp != None:
            self.fp.close()
            self.fp = None

        return 0

    def flush_script(self):
        """close self.fp, if it is open (and if we are making one)"""

        if not self.make_main_script: return 0

        if self.fp != None: self.fp.flush()

        return 0

    def make_exec(self):
        if not self.make_main_script: return 0

        if self.script and os.path.isfile(self.script):
            os.chmod(self.script, 0755)

    def prev_lab(self, block):
       if block.index <= 0:
          print '** asking for prev lab on block %s (ind %d)' \
                % (block.label, block.index)
       return self.block_index_label(block.index-1)

    def block_index_label(self, index):
       if index < 0 or index >= len(self.pblabels):
          print '** invalid index for block label: %d' % index
          return 'NO_LABEL'
       return self.pblabels[index]

    # given a block, run, return a prefix of the form: pNN.SUBJ.rMM.BLABEL
    #    NN = block index, SUBJ = subj label, MM = run, BLABEL = block label
    # if surf_names: pbNN.SUBJ.rMM.BLABEL.HEMI.niml.dset
    # (pass as 0/1, -1 for default)
    def prefix_form(self, block, run, view=0, surf_names=-1):
        if self.runs > 99: rstr = 'r%03d' % run
        else:              rstr = 'r%02d' % run
        if view: vstr = self.view
        else:    vstr = ''
        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        else: hstr = ''
        s = self.sep_char
        return 'pb%02d%s%s%s%s%s%s%s%s' %    \
               (block.index, s, self.subj_label, hstr, s, rstr, s,
                block.label, vstr)

    # same, but leave run as a variable
    def prefix_form_run(self, block, view=0, surf_names=-1):
        if view: vstr = self.view
        else:    vstr = ''
        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        else: hstr = ''
        if self.sep_char == '.': rvstr = '$run'
        else:                    rvstr = '${run}'
        s = self.sep_char
        return 'pb%02d%s%s%s%sr%s%s%s%s' %    \
            (block.index, s, self.subj_label, hstr, s, rvstr, s,
             block.label, vstr)

    # same as prefix_form, but use previous block values (index and label)
    # (so we don't need the block)
    # if self.surf_names: pbNN.SUBJ.rMM.BLABEL.HEMI.niml.dset
    def prev_prefix_form(self, run, block, view=0, surf_names=-1):
        if self.runs > 99: rstr = 'r%03d' % run
        else:              rstr = 'r%02d' % run
        if view: vstr = self.view
        else:    vstr = ''
        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        else: hstr = ''
        s = self.sep_char
        return 'pb%02d%s%s%s%s%s%s%s%s' %    \
               (block.index-1, s, self.subj_label, hstr, s, rstr, s,
               self.prev_lab(block), vstr)

    # same, but leave run as a variable
    def prev_prefix_form_run(self, block, view=0, surf_names=-1):
        if view: vstr = self.view
        else:    vstr = ''
        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        else: hstr = ''
        if self.sep_char == '.': rvstr = '$run'
        else:                    rvstr = '${run}'
        s = self.sep_char
        return 'pb%02d%s%s%s%sr%s%s%s%s' %    \
               (block.index-1, s, self.subj_label, hstr, s, rvstr, s,
               self.prev_lab(block), vstr)

    # same, but leave run wild
    def prev_dset_form_wild(self, block, view=0, surf_names=-1):
        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        else:      # view option is not really handled...
           vstr = '%s.HEAD' % self.view
           hstr = ''
        s = self.sep_char
        return 'pb%02d%s%s%s%sr*%s%s%s' %    \
             (block.index-1, s, self.subj_label,hstr, s, s,
             self.prev_lab(block), vstr)

    # like prefix, but list the whole dset form, in wildcard format
    def dset_form_wild(self, blabel, view=None, surf_names=-1):
        block = self.find_block(blabel)
        if not block:
            print "** DFW: failed to find block for label '%s'" % blabel
            return ''
        bind = block.index
        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        elif view:
           vstr = '%s.HEAD' % view
           hstr = ''
        else:
           vstr = '%s.HEAD' % self.view
           hstr = ''
        s = self.sep_char
        return 'pb%02d%s%s%s%sr*%s%s%s' %      \
            (bind, s, self.subj_label, hstr, s, s, blabel, vstr)

class ProcessBlock:
    def __init__(self, label, proc):
        self.label = label      # label is block type
        self.valid = 0          # block is not yet valid
        self.index = proc.bindex
        self.verb  = proc.verb  # verbose level
        self.post_cstr = ''     # extra commands to run at the end of the block
        if not label in BlockModFunc: return

        self.opts = OptionList(label)                   # init option list
        BlockModFunc[label](self, proc, proc.user_opts) # add block

        if self.verb > 2:
            if self.valid: self.show('+d successful add ')
            else:          print '+d invalid ProcessBlock: %s' % label

    def show(self, mesg):
        print '------- %sProcessBlock: %s -------' % (mesg, self.label)
        self.opts.show('new options: ')

def run_proc():

    ps = SubjProcSream('subject regression')
    ps.init_opts()

    rv = ps.get_user_opts()
    if rv != None:  # 0 is a valid return
        if rv != 0:
            show_args_as_command(ps.argv, "** failed command (get_user_opts):")
        return rv

    # run db_mod functions, and possibly allow other mods
    if ps.create_blocks():
        show_args_as_command(ps.argv, "** failed command (create_blocks):")
        return rv

    # run db_cm functions, to create the script
    rv = ps.create_script()
    if rv != None:  # terminal, but do not display command on 0
        if rv != 0:
            show_args_as_command(ps.argv, "** failed command (create_script):")
        return 1

    # finally, execute if requested
    if ps.user_opts.find_opt('-execute'): rv = os.system(ps.bash_cmd)

    return rv

# main
if __name__ == '__main__':

    rv = run_proc()
    sys.exit(rv)

