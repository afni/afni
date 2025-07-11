#!/usr/bin/env python

# python3 status: compatible

# note: in the script, runs are 1-based (probably expected)

# for end='' parameter to print()
from __future__ import print_function

import string, sys, os
from time import asctime

# AFNI modules
from afnipy.afni_base import *
from afnipy.afni_util import *
from afnipy.option_list import *
from afnipy.db_mod import *
from afnipy import lib_vars_object as VO
from afnipy import ask_me
from afnipy import lib_tedana_afni as TED
from afnipy import lib_format_cmd_str as FCS

# ----------------------------------------------------------------------
# globals

g_history = """
    afni_proc.py history:

    1.0  Dec 20, 2006 : initial release
    1.1  Dec 20, 2006 : added -regress_use_stim_files
    1.2  Dec 21, 2006 : help, start -ask_me, updated when to use -iresp/ideal
    1.3  Dec 22, 2006 : change help to assume ED's stim_times files exist
    1.4  Dec 25, 2006 : initial -ask_me
    1.5  Dec 27, 2006 : ask_me help
    1.6  Dec 28, 2006 : -gltsym examples, min(a,b) in scale block
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
        - added 'MASKING NOTE', to suggest no regression mask until group space
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
    2.31 Jul 14 2010 : added -mask_test_overlap and -regress_cormat_warnings
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
          (so there are no missing view warnings, done for J Jarcho)
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
    3.49 Jun 25, 2013: added options -volreg_motsim, -volreg_opts_ms
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
    4.22 Nov 07, 2014: shift -affter warp to -warp for -tlrc_NL_warp
        - requires 3dNwarpApply from Nov 7 or later
    4.23 Nov 21, 2014:
        - changed -anat_uniform_method none to mean no correction at all
        - now 'default' means to apply it normally in auto_warp.py
        - done for B Benson
    4.24 Dec 02, 2014: -tlrc_NL_awpy_rm was not being applied
    4.25 Jan 02, 2015: small help change
    4.26 Jan 12, 2015: compute global correlation volume (similar to GCOR)
    4.27 Jan 15, 2015: use 3ddot -demean for correlation of masks
    4.28 Feb 06, 2015: apply updates to 3dClustSim
    4.29 Feb 11, 2015: added -regress_anaticor_fast and -regress_anaticor_fwhm
    4.30 Feb 13, 2015:
        - make WMeLocal for fast anaticor a float dataset
        - generate WMeL_corr as a diagnostic volume (corr w/WMeLocal)
        - todo: add ability to make WMeL_corr without fast anaticor
    4.31 Feb 27, 2015: added -regress_WMeL_corr option (forgot that)
        - 27 July, 2015: option changed to -regress_make_corr_AIC
    4.32 Mar  2, 2015:
        - fixed 3dTproject call for resting state on surface
        - small change to get 3dSeg results using wildcard
    4.33 Mar  3, 2015:
        - allow MIN_OUTLIER as a parameter in -volreg_align_to
        - update requirement date (Feb 9 -> Nov 9)
    4.34 Apr  1, 2015: anat followers and ROI_PC
        - added ability to warp anat followers
           - anat is default if skull and align or tlrc blocks
        - added -regress_ROI_PC, -regress_ROI_maskave (for external masks)
        - added -regress_ROI_erode
        - warp anat w/skull for QC
    4.35 Apr  1, 2015:
        -tcat_remove_first_trs can take a list
        - done for P Hamilton
    4.36 Apr  2, 2015: added -tlrc_NL_warped_dsets to import 3dQwarp result
    4.37 Apr  9, 2015: fix for NIFTI NL anat; add a little help
    4.38 Apr 22, 2015:
        - help update for -regress_ROI_*
        - verify erode list use
        - added -todo to show current list
    4.39 Apr 22, 2015: add missed cat_matvec to create warp.all.anat.aff12.1D
    4.40 Apr 30, 2015: allow AM2 centering param via basis backdoor
        - e.g. use basis "BLOCK(2) :x:0.176"
    4.41 May 05, 2015:
        - added -anat_follower, -anat_follower_ROI, -regress_anaticor_label
        - followers are copied to copy_af_LABEL
        - added Example 11 - a more modern resting state example
        - added local WMe correlation diagnostic using -ort X-matrix
    4.42 May 07, 2015: replaced slow 3dTfitter with 3dTproject in anaticor
    4.43 May 08, 2015: added -regress_make_corr_vols
    4.44 May 18, 2015: apply ROIs more generally; allow local ROI PCs
        - external ROIs should now be passed via -anat_follower_ROI,
          rather than -regress_ROI_*, the latter no longer taking datasets
        - also changed -regress_ROI_erode to -anat_follower_erode
        - removed option -regress_ROI_maskave (use -regress_ROI)
    4.45 May 22, 2015: help clarifications
    4.46 Jun 15, 2015: applied -regress_stim_times_offset to typical timing
    4.47 Jul 01, 2015: clarified help for -anat_unif_GM, default = no
    4.48 Jul 27, 2015:
        - renamed -regress_WMeL_corr to -regress_make_corr_AIC
        - default is now 'no' (since it is a somewhat slow operation)
    4.49 Jul 28, 2015:
        - ** ANATICOR now includes zero volumes at censor points
        -    (this matches non-ANATICOR and fast ANATICOR cases)
    4.50 Jul 29, 2015:
        - ANATICOR now works for task analysis, using -regress_reml_exec
    4.51 Sep 02, 2015: if rest with REML, use REML errts
    4.52 Sep 10, 2015: fix resulting aligned SurfVol if input is NIFTI
    4.53 Sep 17, 2015: clarify incorrect dgrid error for anat follower
    4.54 Sep 24, 2015: allow 3dD to proceed with only extra_stim_files
    4.55 Oct 26, 2015: avoid censored TRs in TSNR computation
    4.56 Oct 28, 2015: output anat/epi mask Dice coef, rather than corr
    4.57 Dec 07, 2015: 
        - modified Example 11 to use FT_SurfVol.nii as the anat, not FT.nii
          (FT.nii was not perfectly aligned with parcellation)
        - added FREESURFER NOTE
    4.58 Jan 27, 2016: allow for tissue PC regression with only regress block
    4.59 Apr 27, 2016: always extract volreg base image vr_base*
    4.60 May  3, 2016: suggest -regress_est_blur_epits for resting state
    4.61 May 31, 2016: better -regress_anaticor warnings if no label
    4.62 Jun 10, 2016: added -blip_reverse_dset for blip up/down correction
    4.63 Jun 11, 2016: fixed blip order vs. view update
    4.64 Jun 13, 2016: added BLIP_BASE case for -volreg_align_to
        - use warped median forward blip volume as volreg alignment base
    4.65 Jun 13, 2016: added -align_unifize_epi: unifize EPI before alignment
    4.66 Jun 14, 2016:
        - if needed, pass along obliquity for all 3dNwarpApply results
        - added -blip_forward_dset
    4.67 Jun 16, 2016:
        - if NLwarp but EPI in orig space, do not apply (warn user)
        - fix refit of blip median datasets
    4.68 Jun 22, 2016: do nothing, but work really hard at it
        - apply EPI transformation steps using an array of transformations
          (to make future changes easier)
    4.69 Jun 24, 2016:
        - added -requires_afni_hist
        - if appropriate, warp vr_base dset as final_epi
    4.70 Jun 27, 2016: allow for blip datasets that are not time series
    4.71 Jun 29, 2016:
        - can modify blip order
        - BLIP_BASE is now MEDIAN_BLIP
        - added BLIP NOTE to -help output
    4.72 Jun 30, 2016:
        - allow for single volume EPI input (e.g. to test blip correction)
        - auto -blip_forward_dset should come from tcat output
          (obliquity test still be from existing -dsets, if appropriate)
    4.73 Jul 23, 2016: if empty regressor, check for -GOFORIT
    4.74 Aug 15, 2016: ACF blur estimation - run 3dFWHMx with -ACF
        - ACF and ClustSim files go into sub-directories, files_ACF/ClustSim
        - -regress_run_clustsim now prefers arguments, ACF, FWHM, both, no
        - default clustsim method is ACF (including -regress_run_clustsim yes)
    5.00 Aug 17, 2016: ACF blur estimation is ready
        - includes gen_ss_review_scripts/table.py
    5.01 Aug 22, 2016: save all final anat/EPI costs into out.allcostX.txt
    5.02 Aug 25, 2016:
        - fix output.proc prefix if -script has path
        - allow -mask_apply group in case of -tlrc_NL_warped_dsets
    5.03 Sep 13, 2016: added -blip_opts_qw
    5.04 Sep 16, 2016: added -radial_correlate
    5.05 Sep 28, 2016: added per run regression option
        - detrend with 3dTproject for PC regressors, to allow for censoring
        - added -regress_ROI_per_run    to apply -regress_ROI    per-run
        - added -regress_ROI_PC_per_run to apply -regress_ROI_PC per-run
    5.06 Oct  9, 2016:
        - added opts -mask_import, -mask_intersect, -mask_union
        - added corresponding Example 11b
    5.07 Oct 13, 2016: minor 11b update (PC_per_run)
    5.08 Oct 20, 2016: check -mask_import for reasonable voxel dimensions
    5.09 Oct 24, 2016:
        - bandpass notes and reference
        - stronger warning on missing -tlrc_base dataset
    5.10 Nov  1, 2016:
        - added -regress_skip_censor
        - added -write_ppi_3dD_scripts to go with:
        - added -regress_ppi_stim_files, -regress_ppi_stim_labels
    5.11 Dec 29, 2016:
        - removed case 16 (brainstem) from aparc+aseg.nii WM extraction in help
    5.12 Mar 21, 2017: allow for volreg-only script with MIN_OUTLIER
    5.13 Mar 27, 2017:
        - NL warps of all-1 volume uses -interp cubic for speed
        - for -mask_import, have lists_are_same compare abs() of dimensions
    5.14 Apr 11, 2017:
        - added GENERAL ANALYSIS NOTE to help
        - mentioned scaling as an option in resting state analysis
    5.15 Apr 25, 2017: fix follower warps for gzipped WARP datasets
    5.16 Sep  7, 2017: fix help typos for -regress_ROI_PC_per_run
    5.17 Sep 11, 2017: if no regress block, skip gen_ss_review_scripts.py
    5.18 Sep 12, 2017: use lpc+ZZ cost function in examples
    6.00 Nov  7, 2017: python3 compatible
    6.01 Nov 15, 2017: fixed -despike_mask (by D Plunkett)
    6.02 Dec 12, 2017: added "sample analysis script" to help
    6.03 Feb 12, 2018: can process multi-echo data
        - added -dsets_me_echo/_run and 'combine' processing block
        - this is a preliminary step, before running OC and ME-ICA
    6.04 Feb 16, 2018: compute epi_anat mask, intersecting the two
        - added -mask_epi_anat, to apply that in place of full_mask
    6.05 Feb 23, 2018: added OC combine method
    6.06 Feb 26, 2018: added -help_section option
    6.07 Feb 27, 2018: added help for a few option
                       (some old, some new, some red, some blue)
    6.08 Mar  1, 2018: added OC_methods OC_A and OC_B
    6.09 Apr  3, 2018: added -combine_tedana_path
    6.10 Apr 26, 2018:
        - run all tedana steps before copying results
        - adjust labels from combine to volreg
    6.11 May  7, 2018:
        - EPI automask (full_mask) is no longer dilated by default
        - added -show_process_changes, to report changes affecting results
    6.12 May  8, 2018:
        - NL-warp dsets need only end in _WARP
        - added -combine_methods: OC_tedort, tedana_OC, tedana_OC_tedort
        - this coincides with a fix to tedana_wrapper.py that, with scaling
          had led to betas cut in half
    6.13 May 14, 2018:
        - if tedana, suggest -blur_in_mask yes
        - added epi_anat as option for -mask_apply
    6.14 May 25, 2018: added -combine_opts_tedwrap
    6.15 May 29, 2018: 
        - fixed a couple of problems in error reporting
        - Thanks to J Reed for mentioning them.
    6.16 Jul  5, 2018: added -mask_opts_automask for L Atlas
    6.17 Aug  7, 2018: 3drefit for tedana results should include space
    6.18 Aug 13, 2018: actually apply -blur_opts_BIM
    6.19 Sep 18, 2018:
        - show sample BIDS-like directory struct under DIRECTORY STRUCTURE NOTE
    6.20 Oct 11, 2018: have gen_ss_r_s always write out.gen_ss_uvars.json
    6.21 Oct 17, 2018: pass -ss_review_dset to gen_ss_review_scripts.py
    6.22 Nov 19, 2018: added opt -html_review_style and run apqc_make_html.py
        - and check for respective dependencies
    6.23 Nov 26, 2018: added opt -volreg_warp_final_interp
    6.24 Dec  5, 2018: reduced dependency list for apqc HTML to just Xvfb
    6.25 Dec 10, 2018: run ss_review_html via tcsh instead of ./
    6.26 Dec 19, 2018: show exec command on both tcsh and bash syntax
    6.27 Jan  7, 2019:
        - added opt -volreg_method 3dvolreg|3dAllineate
        - added opts -volreg_allin_auto_stuff and -volreg_allin_cost
        - nest apqc_make_tcsh.py under @ss_review_basic block
    6.28 Jan 16, 2019:
        - have -regress_mot_as_ort default to yes
        - rename external_volreg_base to vr_base_external
    6.29 Jan 18, 2019:
        - run 1d_tool.py -show_df_info, unless -regress_show_df_info no
    6.30 Jan 22, 2019: added -regress_est_blur_detrend yes/no
    6.31 Jan 30, 2019: added -volreg_post_vr_allin and -volreg_pvra_base_index
    6.32 Feb 22, 2019: added -tlrc_NL_force_view
    6.33 Apr 11, 2019: added -combine_tedort_reject_midk
    6.34 Apr 25, 2019: -regress_bandpass takes any even number of frequencies
    6.35 May 14, 2019: added -radial_correlate_blocks and _opts
    6.36 May 22, 2019: modified -regress_anaticor_fast method
        - blurs are now truncated Gaussians by default, making them flat
          and very similar to original ANATICOR method
        - added -regress_anaticor_full_gaussian and -regress_anaticor_term_frac
    6.37 May 23, 2019: QC: save fanaticor_mask_coverage dataset
    6.38 May 30, 2019: 
        - have -regress_anaticor_full_gaussian default to yes
          (so one would now have to ask for a truncated Gaussian)
        - generate fanaticor_mask_coverage as float
    6.39 Jun  3, 2019: allow ricor in case of ME data
    6.40 Jun 18, 2019: no mask for TSNR on the surface
    6.41 Jun 19, 2019: fail on some blur option with surface analysis
    6.42 Jul  2, 2019: apply 1d_tool.py -write_xstim for X.stim.xmat.1D
    6.43 Jul  3, 2019: if no stim, make sum_baseline.1D, not sum_ideal.1D
    6.44 Jul  5, 2019: (useless) switch to 3dTcorr1D for dot product
    6.45 Jul 19, 2019: if template is multi-volume, get vol [0] for group_mask
    6.46 Jul 25, 2019: added -volreg_warp_master
    6.47 Aug 27, 2019: use $tr_counts for motion regressors and such
    6.48 Sep  9, 2019: added control for -NEW25
    6.49 Sep 18, 2019:
       - added file tracking and -show_tracked_files option
       - if -html_review_style pythonic, check for matplotlib
    7.00 Oct 24, 2019:
       - supports running tedana from MEICA group
         https://github.com/ME-ICA/tedana
       - added combine methods m_tedana, m_tedana_OC
       - allow -mask_epi_anat if -anat_has_skull no
    7.01 Oct 25, 2019: allow for selectors on -dset* options
    7.02 Oct 30, 2019: 3dcopy tedana results using current view
    7.03 Nov  1, 2019: create out.mask_at_corr.txt
    7.04 Nov 21, 2019: added more current FreeSurfer babble to help
    7.05 Nov 29, 2019: added option -volreg_opts_ewarp
    7.06 Jan 15, 2020: corr_* dsets are now correlations with ROI averages,
                       rather than average correlations across ROIs
    7.07 Feb  4, 2020: added help for some esoteric options
    7.08 Feb 12, 2020: initial -compare_opts functionality:
       - added -compare_opts, -compare_example_pair,
               -show_example, -show_example_names
    7.09 Feb 14, 2020:
       - added -compare_opts_vs_opts
       - modified s03.ap.surace, added s05.ap.uber
    7.10 Feb 18, 2020: modified Example 7; added Example 6b and EGS.12c
       - display all help examples, including ones outside of afni_proc.py
       - specify whether each example is reasonably recommended
    7.11 Mar 11, 2020: add details on why examples are not considered complete
    7.12 Apr 14, 2020: copy with absolute paths if inputs have them
    7.13 Feb 22, 2021:
       - add -regress_mask_tsnr; change default to no mask for TSNR dset
       - pass mask_dset explicitly to gen_ss_review_scripts.py
    7.14 Feb 24, 2021: -add -regress_extra_ortvec and _ortvec_labels
    7.15 May  1, 2021:
       - fixed niml.dset suffix in case of -regress_compute_fitts on surface
         (thanks to S Torrisi for noting the problem)
    7.16 May 19, 2021: fixed volreg TSNR in surface case (still in volume)
    7.17 Jul 16, 2021: unindent EOF
    7.18 Oct 18, 2021: allow -mask_apply "type" to be a user-specified mask
    7.19 Nov  7, 2021: add -regress_opts_fwhmx
    7.20 Nov  8, 2021: add -milestones (later changed to -hist_milestones)
    7.21 Nov 20, 2021:
       - update for current tedana with --convention orig
       - add -help_tedana_files for correspondence between file names
    7.22 Nov 27, 2021:
       - updates for current MEICA group tedana
       - add lib_tedana_afni
       - update m_tedana/m_tedana_OC combine methods to work with new tedana
         (use 3dcalc to copy results, preserving space/view, use float)
    7.23 Dec  3, 2021: add the m_tedana_tedort -combine_method
    7.24 Jan 11, 2022:
       - rename m_tedana_tedort to m_tedana_m_tedort
         (reserve m_tedana_tedort for AFNI tedort projection)
    7.25 Jan 24, 2022: pass copy_anat,combine_method to gen_ssrs for APQC
    7.26 Jan 29, 2022: allow execution only if we have made the main script
    7.27 Feb  7, 2022: write out.ap_uvars.txt,json, and use to init gssrs
    7.28 Feb  8, 2022: add -html_review_opts
    7.29 Feb 18, 2022: change -milestones to -hist_milestones
    7.30 Mar  1, 2022:
       - make pythonic the default html_review_style
         (since PT will run with basic if pythonic is not possible)
    7.31 Mar  1, 2022: fix space removal in -combine_opts_tedana
    7.32 Mar  3, 2022: minor rename of blocks; corresponding help update
    7.33 Mar  8, 2022: apply -polort in @radial_correlate
    7.34 Mar 10, 2022: run 3dAllineate for -align_epi_ext_dset to volreg base
    7.35 Mar 12, 2022: use align_epi_anat.py instead, in case of obliquity diff
    7.36 Mar 22, 2022:
       - fix help example: remove inappropriate -epi_strip from -align_opts_aea
       - add ap_uvars: dir_suma_spec, suma_specs
    7.37 Apr  6, 2022: allow REML errts on surface
    7.38 Apr 22, 2022: in proc script, check for tedana in PATH, if needed
    7.39 May 10, 2022: do not apply global line wrappers to QC block
    7.40 May 24, 2022: add -command_comment_style
    7.41 Jun  6, 2022:
       - add -align_unifize_epi local method, -align_opts_eunif
       - create final_epi_unif volume, in case of EPI uniformity correction
    7.42 Jun 13, 2022: remove final_epi_unif, as it is already final EPI
    7.43 Jul 26, 2022: copy label tables of anat followers
    7.44 Jul 30, 2022:
       - copy tlrc_base/template to results directory
       - add opt -tlrc_copy_base
    7.45 Aug 17, 2022: if 'tlrc' block and -regress_ROI*, req -volreg_tlrc_warp
    7.46 Aug 30, 2022:
       - make -show_example allow unique substring matching
       - pass final_epi_dset as a uvar if there is no warped version
    7.47 Oct  8, 2022: w/PT: reformat of help examples
    7.48 Nov 15, 2022: find_variance_lines.tcsh, and vlines_* uvars
    7.49 Nov 23, 2022: added examples simple_rest_QC, simple_rest_QC_na
    7.50 Dec 16, 2022: update example 6, 6b
    7.51 Jan 24, 2023: add QC to ricor block
    7.52 Feb  1, 2023: get SurfSmooth params from smrec file
    7.53 Feb  3, 2023: propagate error when num_echo is inconsistent
    7.54 Feb  6, 2023: propagate slice_pattern from -tshift_opts_ts -tpattern
    7.55 Mar  1, 2023: add -show_pretty_command, to print a more readable one
    7.56 Jun  2, 2023: fix -regress_errts_prefix with surface analysis
    7.57 Jun 14, 2023: default to -radial_correlate_blocks errts, if none given
    7.58 Jun 21, 2023: fix: pass tlrc_base as uvar template
    7.59 Jul 21, 2023: fix: update help for -regress_make_corr_vols
                            (it is now corr vs ave, rather than ave corr)
    7.60 Jul 24, 2023: if -tlrc_NL_warped_dsets, require -tlrc_base
    7.61 Aug 21, 2023: modify $ktrs to come from a text file, instead of shell
    7.62 Feb  1, 2024: 
       - add options -show_example_keywords, -show_pythonic_command
       - include keywords and modification date in examples
       - partially revamp examples, add demo, short and publish examples
    7.63 Feb  2, 2024: add -ROI_import (can regress PCs for now)
    7.64 Feb  8, 2024:
       - if radcor is after scaling, pass an EPI mask (warn if no mask)
       - block radcor once processing enters the surface domain
    7.65 Feb 15, 2024: add option -regress_compute_tsnr_stats
    7.66 Feb 15, 2024:
       - no dupe follower warning if grids differ
       - no FWHM->ACF warning
    7.67 Feb 21, 2024:
       - partial publish example updates
       - remove warning: 'ricor regressors are no longer applied in final reg'
    7.68 Feb 22, 2024:
       - use mask_epi_anat for more QC (over full_mask) and modify indentation
       - if appropriate, apply "-regress_compute_tsnr_stats brain 1"
    7.69 Mar 11, 2024:
       - add 3dTto1D -method 4095_warn command and out.4095_warn.txt uvar
    7.70 Mar 18, 2024:
       - locate() -ROI/mask_import datasets and get tlrc_base early
       - add auto-ROI_import of APQC atlas and regress tsnr_stats
       - add option -regress_compute_auto_tsnr_stats
    7.71 Mar 29, 2024: allow -regress_apply_mot_types none
    7.72 Apr  1, 2024: add reg_echo and echo_times as uvars
    7.73 Apr  7, 2024: the default warp vox dim will round up if very close
    7.74 Apr  8, 2024: add -anat_follower_erode_level
    7.75 Apr 25, 2024: add -uvar option, to pass user vars along
    7.76 May 25, 2024:
       - add -bids_deriv, to output a BIDS derivative tree
       - add -volreg_allin_warp (def shift_rotate)
    7.77 May 30, 2024: fix volreg TSNR for ME: use eind -> fave_echo
       - thanks to zhengchencai on MB for pointing it out
       - remove unneeded followers from example 'publish 3d'
    7.78 Aug  5, 2024: add option -blip_warp_dset to input a computed warp
    7.79 Aug 30, 2024:
       - make example option order more consistent (id, EPI, anat, blocks, ...)
       - minor updates to example comments and directory names
       - add example 'publish 3i', where 'i' corresponds to 9
       - add examples publish 3e, 3f, 3g, 3h, 3j and help section 'eshow'
         - exclude 'noshow' examples from default help output
       - compare_opts now considers file path differences separately
    7.80 Oct 10, 2024: make sure intersect datasets have views
    7.81 Oct 23, 2024:
       - redo default surface blur
       - add -volreg_no_volreg, the B Feige option
    7.82 Dec  6, 2024: subtract 1 from ricor QC vrat, for more a useful display
    7.83 Jan 27, 2025: fix -compare_opts display of fewer/more options applied
    7.84 Jan 28, 2025: add option -tlrc_affine_warped_dsets
    7.85 Jan 29, 2025: suggest OC_B if only 2 echoes
    7.86 Feb 27, 2025: add -show_merged_opts, -compare_merged_opts
    7.87 Mar 11, 2025: update examples class 3,5 per AD6
    7.88 Mar 21, 2025: add combine methods OC_m_tedort and m_tedana_OC_tedort
    7.89 Apr  2, 2025: parse/pass any opts_ts -tpattern @FILE to review_basic
    7.90 Apr 24, 2025: use updated find_variance_lines.tcsh (no -nerode 2)
    7.91 May  6, 2025: set ROI_import view in case of no volreg block
    7.92 May 16, 2025: add help for m_tedort combine methods
    7.93 Jun 17, 2025: pass blur_size uvar to PT for APQC
"""

g_version = "version 7.93, June 17, 2025"

# version of AFNI required for script execution
g_requires_afni = [ \
      [ "24 Apr 2025",  "find_variance_lines.tcsh -ignore_edges" ],
      [ " 7 Mar 2024",  "3dTto1D -method 4095_warn" ],
      [ "15 Feb 2024",  "compute_ROI_stats.tcsh, whereami -index_to_label" ],
      [ "14 Nov 2022",  "find_variance_lines.tcsh" ],
      [ " 3 Jun 2022",  "3dLocalUnifize" ],
      [ " 7 Mar 2022",  "@radial_correlate -polort" ],
      [ " 3 Feb 2022",  "gen_ss_review_scripts.py -init_uvas_json" ],
      [ "27 Jun 2019",  "1d_tool.py -write_xstim" ],
      [ "10 May 2019",  "@radial_correlate -do_clean" ],
      [ "17 Jan 2019",  "1d_tool.py -show_df_info" ],
      [ "19 Nov 2018",  "apqc_make_tcsh.py" ],
      [ " 3 May 2018",  "@extract_meica_ortvec" ],
      [ "23 Mar 2018",  "tedana_wrapper.py" ],
      [ "23 Feb 2018",  "@compute_OC_weights -echo_times" ],
      [ "23 Sep 2016",  "1d_tool.py -select_runs" ],
      [  "1 Dec 2015",  "3dClustSim -ACF" ],
      [ "28 Oct 2015",  "3ddot -dodice" ],
      [  "1 Sep 2015",  "gen_ss_review_scripts.py -errts_dset" ],
      [ "23 Jul 2015",  "3dREMLfit -dsort" ],
      [  "1 Apr 2015",  "1d_tool.py uncensor from 1D" ] ]

# milestones, for general reference
g_milestones = """
interesting milestones for afni_proc.py:

   2006.12 : initial release - basic processing blocks, can alter order
   2006.12 : -ask_me - interactive method for user options
   2007.05 : compatible for python 2.2 - 2.5
   2008.01 : compute smoothness estimates (for use in cluster correction)
   2008.12 : enable use of NIFTI inputs
   2009.03 : enable use of 3dREMLfit
   2009.03 : default change - do not mask EPI results
   2009.04 : add ricor block - for physiological regressors
   2009.05 : add tlrc block - EPI to standard space (catenated transformation)
   2009.05 : add align block - run align_epi_anat.py (catenated transformation)
   2009.05 : base examples on AFNI_data4
   2009.08 : censoring based on motion parameters
   2010.06 : censoring based on initial outliers
   2010.08 : allow amplitude modulation in the linear regression model
   2011.06 : TSNR dataset
   2011.07 : graphical interface - uber_subject.py
   2011.07 : @ss QC review scripts - via gen_ss_review_scripts.py
   2011.10 : surface analysis
   2012.01 : base examples on AFNI_data6
   2012.04 : enable bandpassing in linear regression model
   2012.05 : allow processing more than 99 runs
   2012.09 : tissue-based regression - via 3dSeg segmentation
   2013.01 : compute GCOR - average spatial pairwise correlation
   2013.05 : ANATICOR - and recommended resting state analysis pipeline
   2013.08 : non-linear registration to template
   2013.09 : 3dRSFC
   2014.04 : MIN_OUTLIER volreg base
   2015.02 : fast ANATICOR - Gaussian-weighted local mean, rather than flat
   2015.04 : anatomical followers and ROI/PC regression
   2016.05 : check left/right flip of EPI vs anat
   2016.06 : distortion correction - using reverse blip
   2016.08 : mixed-model ACF blur estimation
   2017.11 : python3 compatible (maintaining compatibility with python 2)
   2018.02 : combine block - for multi-echo data (OC and tedana)
   2018.11 : APQC HTML report
   2019.01 : EPI alignment across per-run bases (-volreg_post_vr_allin)
   2019.10 : tedana from MEICA group - https://github.com/ME-ICA/tedana
   2019.02 : compare options with examples and other afni_proc.py commands
   2021.04 : ap_run_simple_rest.tcsh: low-option afni_proc.py command for QC
   2021.11 : apply updates from MEICA group tedana
   2022.06 : local unifize option to assist inhomogeneous EPI for anat alignment
   2022.11 : find_variance_lines.tcsh - detect high-variance I/S lines in EPI
   2022.11 : run APQC HTML from local server, for interactive features
   2024.02 : new examples (demo, short, publish), with mod date
   2024.02 : compute TSNR stats across automatic or provided ROIs
   2024.04 : ap_run_simple_rest_me.tcsh: low-option afni_proc.py for multiecho
   2024.05 : enable output of BIDS derivative tree
   2024.08 : input external distortion warp dataset
"""


g_process_changes_str = """
---------- changes to afni_proc.py that might afftect results ----------

Miscellaneous older changes:

   24 Mar 2009 : mask is no longer applied to EPI data
      - preferable to see all results by default
      - reproduce with: -mask_apply brain

   17 Jun 2009 : EPI extents mask is applied
      - motion could cause strange edge effects
      - require data at every time point
      - reproduce with: -volreg_no_extent_mask

   17 Aug 2016 : blur estimates change from FWHM to ACF
      - FWHM values are now zero, to discourage use

More detailed changes, starting May, 2018.

   07 May 2018 : EPI full_mask: dilation is no longer the default
      - since mask is not (generally) applied to data, make more accurate
      - reproduce with: -mask_dilate 1

   22 May 2019 : ANATICOR changes (see -regress_anaticor*)
      - changed default radius from 45 to 30 mm

   15 Jan 2020 : corr_* volumes are correlatinos with averages, rather than
      average correlations (maps are similar by have better scales)

   10 Mar 2022 : run 3dAllineate for -align_epi_ext_dset to volreg base
      - apply an additional xform between anat2epi base and epi2epi one

   25 May 2024 : make volreg_method 3dAllinate default warp shift_rotate
      - add -volreg_allin_warp to control

"""

g_todo_str = """todo:
  - when replacing 'examples' help section, move -ask_me EXAMPLES section
  - allow listing examples by keyword (choose and/or remove)
  - example demo 2b should be added to APMD1 tree
  - ROI_import, anat_follower_ROI for ROI TSNR averages
     x handle in regress block: add -regress_compute_tsnr_stats
     - add -volreg_compute_tsnr_stats
  - ME:
     - handle MEICA tedana methods
        x m_tedana, m_tedana_OC, m_tedana_OC_tedort
        * WAS done, but soon-to-come tedana JSON output must be handled by AP
        - done again, but still might want OC_m_tedort
          (consider m_tedana_OC_m_tedort say, to have AP do the projections)
     - detrend (project others?) execute across runs
        - then break either data or regressors across runs
     - pre-ME: motion params?  censoring?
     x add help for new combine methods
     x add tedana orthogonalization combine methods
     x for LA: run all tedana steps before 3dcopy ones
     x update for (f)ANATICOR 
     x allow use of -mask_import
     - use combine result in -regress_ROI* options
        - see: rcr - todo combine
     - ** set_proc_vr_vall (and similar), choose between volreg and combine
        - test ROI PC
     - for DH, option to use mask based on last echo (prob int w/anat)
     - ** write AP regression tests
     x add help for -combine_tedana_path
     x do 'apply catenated xform'
     x compare OC inputs with those from Lauren
     - test only vreg, w/anat, aff std space, NL, blip
     - test all blocks: despike, tshift, blur, mask, scale
     - test radial_correlate
     x implement for despike, tshift, blur, scale, mask (fave)
     - implement for ricor
     - implement case for volreg (without align or tlrc)
        - sooo, initial 3dvolreg output will be changed to garbage?
     x after OC/MEICA, clear use_me
  - run 3dinfo -av_space to check if anat is already in standard
    space and the user is also trying to warp it
  - be able to run simple forms of @Align_Centers
  - implement multi-echo OC and possibly meica functionality
  - improve on distortion correction via gentle NL alignment with anat
  - finish @radial_correlate updates, like _opts and _volreg
     - maybe add to gen_ss_review_scripts.py
  x allow for 3dAllineate in place of 3dvolreg: -volreg_use_allineate
  - blip correction:
     - pass warp result dset(s)
  - add option to block anat from anat followers?
  - add AP test for varying remove_first_trs
  - add -4095_check and -4095_ok options?
     - maybe check means the 3dToutcount would fail, and ok would continue
     - or have 3dToc just report, maybe write a volume?
  - add warnings from any bad GLTsymtest output

  - warn of missing input dsets?  (-dsets, -copy_anat, any 3dcopy)
  - add -volreg_align_base_to_ext_dset (probably a small displacement)
    (to match volreg_base to -align_epi_ext_dset)
  - if no align/tlrc blocks, but -mask_segment_anat, run 3dSkullStrip
    see https://afni.nimh.nih.gov/afni/community/board/read.php?1,145004
  - add anaticor in surface analysis
  - update surface examples to use standard mesh surfaces

  - motsim regression: per voxel; PCs; combined with mot params?
     - is this extra useful in the case of no censoring?  e.g. PPI
  - add -regress_basis_AM2_offsets or _basis_multi_params (now hidden in basis)
     for something like -stim_times_AM2 'BLOCK(2)' :x:0.176
"""

# ----------------------------------------------------------------------
# dictionary of block types and modification functions

BlockLabels  = ['tcat', 'postdata', 'despike', 'ricor', 'tshift', 'blip',
                'align', 'volreg', 'combine', 'motsim', 'surf', 'blur',
                'mask', 'scale', 'regress', 'tlrc', 'empty']
BlockModFunc  = {'tcat'   : db_mod_tcat,     'postdata' : db_mod_postdata,
                 'despike': db_mod_despike,
                 'ricor'  : db_mod_ricor,    'tshift' : db_mod_tshift,
                 'blip'   : db_mod_blip,
                 'align'  : db_mod_align,    'volreg' : db_mod_volreg,
                 'combine': db_mod_combine,  'motsim' : db_mod_motsim,
                 'surf'   : db_mod_surf,     'blur'   : db_mod_blur,
                 'mask'   : db_mod_mask,     'scale'  : db_mod_scale,
                 'regress': db_mod_regress,  'tlrc'   : db_mod_tlrc,
                 'empty'  : db_mod_empty}
BlockCmdFunc  = {'tcat'   : db_cmd_tcat,     'postdata' : db_cmd_postdata,
                 'despike': db_cmd_despike,
                 'ricor'  : db_cmd_ricor,    'tshift' : db_cmd_tshift,
                 'blip'   : db_cmd_blip,
                 'align'  : db_cmd_align,    'volreg' : db_cmd_volreg,
                 'combine': db_cmd_combine,  'motsim' : db_cmd_motsim,
                 'surf'   : db_cmd_surf,     'blur'   : db_cmd_blur,
                 'mask'   : db_cmd_mask,     'scale'  : db_cmd_scale,
                 'regress': db_cmd_regress,  'tlrc'   : db_cmd_tlrc,
                 'empty'  : db_cmd_empty}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# default block labels, and other labels (along with the label they follow)
DefLabels = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
OtherDefLabels = {'despike':'postdata', 'align':'postdata', 'ricor':'despike',
                  'combine':'volreg', 'surf':'volreg'}
OtherLabels    = ['empty']
DefSurfLabs    = ['tcat','tshift','align','volreg','surf','blur',
                  'scale','regress']

# names for blocks that do NOT process (make new) EPI data
#   --> these do not need index increments
EPInomodLabs = ['postdata', 'align', 'tlrc', 'mask']

default_roi_keys = ['brain', 'GM', 'WM', 'CSF', 'GMe', 'WMe', 'CSFe']
stim_file_types  = ['times', 'AM1', 'AM2', 'IM', 'file']
# apply to apqc_make_tcsh.py
g_html_review_styles = ['none', 'basic', 'pythonic' ] # java?

# based on what is specified as a dataset, but focus on what
# is expected to vary
g_eg_skip_opts = [ 
   '-subj_id', '-script', '-out_dir', '-align_epi_ext_dset', 
   '-anat_follower', '-anat_follower_ROI', 
   '-blip_forward_dset', '-blip_reverse_dset', '-blip_warp_dset',
   '-copy_anat', '-dsets', '-dsets_me_echo', '-dsets_me_run', 
   '-surf_anat', '-surf_spec',
   '-tlrc_NL_warped_dsets', 
   # '-volreg_base_dset',   (not sure, so allow for now)
   '-regress_censor_extern', '-regress_extra_stim_files', 
   '-regress_extra_ortvec', '-regress_extra_ortvec_labels',
   '-regress_motion_file', 
   '-regress_ppi_stim_files', '-regress_stim_files', '-regress_stim_times', 
   '-ricor_regs'
   ] 

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
        self.EGS        = None          # reference to imported ap_examples lib

        self.blocks     = []            # list of ProcessBlock elements
        self.block_names= []            # list of block names, pre 'blocks'
        self.dsets      = []            # list of afni_name elements
        self.have_sels  = 0             # do the inputs have selectors
        self.dsets_obl  = 0             # are the -dsets oblique

        self.check_rdir = 'yes'         # check for existence of results dir
        self.stims_orig = []            # orig list of stim files to apply
        self.stims      = []            # list of stim files to apply
        self.extra_stims_orig  = []     # orig list of extra_stims
        self.extra_stims       = []     # extra -stim_file list
        self.extra_labs        = []     # labels for extra -stim_file list
        self.extra_ortvec_orig = []     # orig list of ortvec files to apply
        self.extra_ortvec      = []     # ortvec files to use in regression
        self.extra_ortvec_labs = []     # labels for reg_ort files

        self.have_task_regs   = 0       # any proc.stims or proc.extra_stims?

        # general file tracking
        self.uvars      = VO.VarsObject() # general uvars, for AP uvars
        self.ap_uv_file = 'out.ap_uvars.json' # JSON file to put AP uvars in
                                        # (ssr_uvars is for gen_ssrs)
        self.tlist      = None          # all files copied/tcat to results
                                        # list of [orig, result, descr]
        self.show_tfiles= ''            # files to show '', 'ALL', desc

        # multi-echo vars
        self.dsets_me   = []            # afni_name dsets, echoes x runs
                                        # (dsets = dsets_me[fave_echo])
        self.echo_times = []            # list of echo times
        self.num_echo   = 1             # applies regardless of have_me
        self.have_me    = 0             # do we have multi-echo data
        self.use_me     = 0             # use ME in current command (changes)
                                        # (clear this after any OC/MEICA step)
        self.reg_echo   = 0             # 1-based, echo to use for registration 
        self.echo_var   = '$eind'       # general echo var (e.g. '$eind')
        self.regecho_var= ''            # var for reg_echo (e.g. '$fave_echo')

        # blip variables
        self.blip_in_for  = None        # afni_name: input: forward blip
        self.blip_in_rev  = None        # afni_name: input: reverse blip
        self.blip_in_med  = None        # afni_name: input: blip align median
        self.blip_in_warp = None        # afni_name: input: blip NL warp dset
        self.blip_dset_for  = None      # afni_name: local blip_in_for dset
        self.blip_dset_rev  = None      # afni_name: local blip_in_rev dset
        self.blip_dset_med  = None      # afni_name: result: blip align median
        self.blip_dset_warp = None      # afni_name: result: blip NL warp dset
        self.blip_obl_warp  = 0         # is it oblique: warp
        self.blip_obl_for   = 0         # is it oblique: forward blip
        self.blip_obl_rev   = 0         # is it oblique: reverse blip

        self.vr_ext_base= None          # name of external volreg base 
        self.vr_ext_pre = 'vr_base_external' # copied volreg base prefix
        self.vr_int_name= ''            # other internal volreg dset name
        self.vr_base_dset = None        # afni_name for applied volreg base
        self.vr_warp_mast = None        # local -volreg_warp_master dset
        self.vr_wmast_in  = None        # input dset for warp_master
        self.vr_warp_fint = ''          # final interpolation for warped dsets
        self.vr_base_MO   = 0           # using MIN_OUTLIER volume for VR base
        self.epi_final    = None        # vr_base_dset or warped version of it
        self.volreg_prefix = ''         # prefix for volreg dataset ($run)
                                        #   (using $subj and $run)
        self.vr_vall    = None          # all runs from volreg block
        self.vr_vall_lab= 'volreg'      # label for vr_vall (volreg or combine)
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
        self.mot_simset = None  # ANTIQUATE: motion simulation dset (afni_name)
        self.motsim_dsets = {}          # dictionary of mstype:afni_name

        self.mot_cen_lim= 0             # motion censor limit, if applied
        self.out_cen_lim= 0             # outlier censor limit, if applied
        self.out_ss_lim = 0.4           # outlier pre-steady state warn limit
        self.out_wfile  = ''            # warnings file, for pre-SS
                                        # (set upon "creation")
        self.outl_rfile = ''            # outlier run file (outcount.r$run.1D)
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
        self.anat       = None          # main/orig anat - changes over time
        self.anat_orig  = None          # orig anat dset (afni_name)
        self.anat_has_skull = 1         # does the input anat have a skull
                                        # also updated in db_cmd_align
        self.anat_unif_meth = 'default' # unifize method
        self.anat_unifized = 0          # has the anat been unifized
        self.anat_final = None          # anat assumed aligned with stats
        self.anat_warps = []            # array of anat warp matrices
        self.nlw_aff_mat= ''            # imported affine or NL warp info
        self.nlw_NL_mat = ''            # (priors: warp, warped anat)
        self.nlw_priors = []            # afni_name list of 2,3 warp files
        self.nlw_type   = ''            # if set, affine or NL
        self.tlrcanat   = None          # expected name of tlrc dataset
        self.tlrc_base  = None          # afni_name dataset used in -tlrc_base
        self.tlrc_nlw   = 0             # are we using non-linear registration
        self.tlrc_ss    = 1             # whether to do skull strip in tlrc
        self.tlrc_space = ''            # 3dinfo -space for tlrc_base dset
        self.warp_epi   = 0             # xform bitmap: tlrc, adwarp, a2e, e2a
        self.a2e_mat    = None          # anat2epi transform matrix file
        self.e2final_mv = []            # matvec list takes epi base to final
        self.e2final    = ''            # aff12.1D file for e2final_mv
        self.pvr_allin_mat = ''         # affine, cross-run volreg base xform
        self.regress_inset = None       # afni_name: first input to regression
        self.anaticor   = 0             # 0/1/2 = no/slow/fast
        self.aic_lset   = None          # ANATICOR local WM time series dataset
        self.errts_final= ''            # final errts, for TSNR, etc
        self.errts_pre  = ''            # possibly changing errts prefix
        self.errts_pre_3dd = ''         # that used in 3dDeconvolve command
        self.errts_reml = ''            # prefix for any REML errts
        self.errts_cen  = 0             # flag: current errts has censored
                                        #       TRs removed
        self.keep_trs = ''              # might become '"[$ktrs]"'
        self.align_ebase= None          # external EPI for align_epi_anat.py
        self.align_epre = 'ext_align_epi' # copied align epi base prefix
        self.rm_rm      = 1             # remove rm.* files (user option)
        self.have_rm    = 0             # have rm.* files (such files exist)
        self.rm_dirs    = 0             # do we have dirs to remove?
        self.rm_list    = ['rm.*']      # array of items to nuke
        self.have_olsq  = 1             # do we run 3dD or corresponding Tproj
        self.have_3dd_stats = 1         # do we have 3dDeconvolve stats
        self.have_reml_stats = 0        # do we have 3dREMLfit stats
        self.epi_review = '@epi_review.$subj' # filename for gen_epi_review.py
        self.bids_deriv   = 'no'        # yes/no/BIDS derivative directory root
        self.html_rev_style = 'pythonic' # html_review_style
        self.html_rev_opts = []         # user opts for apqc_make_tcsh.py
        self.made_ssr_scr = 0           # did we make subj review scripts
        self.ssr_basic    = '@ss_review_basic'         # basic review script
        self.ssr_b_out    = 'out.ss_review.$subj.txt'  # text output from it
        self.ssr_uvars    = 'out.ss_review_uvars.json' # uvars output from it
        self.test_stims   = 1           # test stim_files for appropriateness
        self.test_dsets   = 1           # test datasets for existence

        self.afollowers   = []          # anat follower dataset VOs

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
        self.orig_delta = [0, 0, 0]     # dataset voxel size (initial)
        self.delta      = [0, 0, 0]     # dataset voxel size
        self.datatype   = -1            # 1=short, 3=float, ..., -1=uninit
        self.scaled     = -1            # if shorts, are they scaled?
        self.mask       = None          # mask dataset: one of the following
        self.mask_epi   = None          # mask dataset (from EPI)
        self.mask_epi_anat = None       # mask dataset (EPI anat intersection)
        self.mask_anat  = None          # mask dataset (from subject anat)
        self.mask_group = None          # mask dataset (from tlrc base)
        self.mask_extents = None        # mask dataset (of EPI extents)
        self.mask_classes = None        # Segsy result at EPI resolution

        # parameters for -combine_method
        self.combine_method = None      # actual -combine_method name

        # options for tissue based time series
        self.roi_dict   = {}            # dictionary of ROI vs afni_name
        self.def_roi_keys = default_roi_keys
        self.regress_auto_tsnr_rois = [] # ROI labels for auto tsnr_stats
                                         # None if not for use

        # parameters related to TSNR and ROIs
        self.tsnr_dset  = None          # for volumetric tsnr ROI stats

        # options related to ACF and clustsim
        self.ACFdir     = 'files_ACF'   # where to put 3dFWHMx -ACF files
        self.CSdir      = 'files_ClustSim' # and 3dClustSim files
        self.made_cdir  = 0             # has it been created

        self.bandpass     = []          # bandpass limits
        self.censor_file  = ''          # for use as '-censor FILE' in 3dD
        self.censor_count = 0           # count times censoring
        self.censor_extern= ''          # from -regress_censor_extern
        self.skip_censor  = 0           # for use as '-censor FILE' in 3dD
        self.exec_cmd     = ''          # script execution command string
        self.bash_cmd     = ''          # bash formatted exec_cmd
        self.tcsh_cmd     = ''          # tcsh formatted exec_cmd
        self.regmask      = 0           # apply any full_mask in regression
        self.regress_orts = []          # list of ortvec [file, label] pairs
        self.regress_polort = 0         # applied polort
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

        # external programs required for execution
        self.required_progs = []        # e.g. 'tedana'

        # updated throughout processing...
        self.bindex     = 0             # current block index
        self.pblabel    = 'xxx'         # previous block label
        self.surf_names = 0             # make surface I/O dset names

        return

    def show(self, mesg):
        print('%sSubjProcSream: %s' % (mesg, self.label))
        if self.verb > 3:
            for block in self.blocks:
                block.show('    Block %d: ' % self.blocks.index(block))
        print('    Dsets : ', end='')
        if len(self.dsets) < 1: print('not yet set')
        else:
            if self.verb > 2:
                print()
                for dset in self.dsets: dset.show()
            else:
                for dset in self.dsets: print(dset.rel_input(), end='')
                print()
        if self.verb > 3: self.valid_opts.show('valid_opts: ')
        if self.verb > 1: self.user_opts.show('user_opts: ')

    def init_opts(self):
        self.valid_opts = OptionList('afni_proc.py options')

        # input style options  rcr - update
        # self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

        # terminal options
        self.valid_opts.add_opt('-help', 0, [],
                        helpstr="show this help")
        self.valid_opts.add_opt('-help_section', 1, [],
                        helpstr="show help from the given section")
        self.valid_opts.add_opt('-help_tedana_files', 0, [],
                        helpstr="show tedana files: old vs orig names")
        self.valid_opts.add_opt('-hist', 0, [],
                        helpstr="show revision history")
        self.valid_opts.add_opt('-hist_milestones', 0, [],
                        helpstr="show interesting milestones")
        self.valid_opts.add_opt('-requires_afni_version', 0, [],
                        helpstr='show which date is required of AFNI')
        self.valid_opts.add_opt('-requires_afni_hist', 0, [],
                        helpstr='show history of -requires_afni_version')
        self.valid_opts.add_opt('-show_example', 1, [],
                        helpstr="show given help example by NAME")
        self.valid_opts.add_opt('-show_example_keywords', 0, [],
                        helpstr="show keywords from all examples")
        self.valid_opts.add_opt('-show_example_names', 0, [],
                        helpstr="show names of all examples")
        self.valid_opts.add_opt('-show_pretty_command', 0, [],
                        helpstr="display afni_proc.py command in a nice format")
        self.valid_opts.add_opt('-show_pythonic_command', 0, [],
                        helpstr="display afni_proc.py command as a python list")
        self.valid_opts.add_opt('-show_process_changes', 0, [],
                        helpstr="show afni_proc.py changes that affect results")
        self.valid_opts.add_opt('-show_tracked_files', 1, [],
                        helpstr="show tracked files of given type")
        self.valid_opts.add_opt('-show_valid_opts', 0, [],
                        helpstr="show all valid options")
        self.valid_opts.add_opt('-todo', 0, [],
                        helpstr="show current todo list")
        self.valid_opts.add_opt('-ver', 0, [],
                        helpstr="show module version")

        # compare options
        self.valid_opts.add_opt('afni_proc.py', 0, [],
                        helpstr="ignored, but passed through")
        self.valid_opts.add_opt('-compare_opts', 1, [],
                        helpstr="compare options against specified example")
        self.valid_opts.add_opt('-compare_opts_vs_opts', 0, [],
                        helpstr="compare earlier options vs. later ones")
        self.valid_opts.add_opt('-compare_example_pair', 2, [],
                        helpstr="compare the specified pair of examples")
        self.valid_opts.add_opt('-compare_merged_opts', 1, [],
                        helpstr="compare options against merged example")
        self.valid_opts.add_opt('-show_merged_opts', 1, [],
                        helpstr="show command with merged options")

        # general execution options
        self.valid_opts.add_opt('-blocks', -1, [], okdash=0,
                        helpstr='specify ordered list of blocks to apply')
        self.valid_opts.add_opt('-do_block', -1, [], okdash=0,
                        helpstr='add extra blocks to the default list')
        self.valid_opts.add_opt('-dsets', -1, [], okdash=0,
                        helpstr='EPI datasets to process, ordered by run')
        self.valid_opts.add_opt('-dsets_me_echo', -1, [], okdash=0,
                        helpstr='one echo, many runs of multi-echo data')
        self.valid_opts.add_opt('-dsets_me_run', -1, [], okdash=0,
                        helpstr='one run, many echoes of multi-echo data')

        self.valid_opts.add_opt('-command_comment_style', 1, [],
                        acplist=['none', 'compact', 'pretty'],
                        helpstr='define trailing AP command comment style')
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

        self.valid_opts.add_opt('-anat_follower', 3, [],
                        helpstr='specify label and anat follower dataset')
        self.valid_opts.add_opt('-anat_follower_erode', -1, [], okdash=0,
                        helpstr="erode follower datasets for given labels")
        self.valid_opts.add_opt('-anat_follower_erode_level', 2, [], okdash=0,
                        helpstr="erode this follower label at the given level")
        self.valid_opts.add_opt('-anat_follower_ROI', 3, [],
                        helpstr='specify label and anat follower ROI dataset')
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
                        helpstr='obsolete: show execution help in bash syntax')
        self.valid_opts.add_opt('-bids_deriv', 1, [],
                        helpstr='specify BIDS output directory (def=no)')
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
        self.valid_opts.add_opt('-echo_times', -1, [],
                        helpstr='list of echo times, one per echo')
        self.valid_opts.add_opt('-execute', 0, [],
                        helpstr='execute script as suggested to user')
        self.valid_opts.add_opt('-exit_on_error', 1, [],
                        acplist=['yes','no'],
                        helpstr='exit script on any command error')
        self.valid_opts.add_opt('-gen_epi_review', 1, [],
                        helpstr='generate a script to review orig EPI data')
        self.valid_opts.add_opt('-html_review_opts', -1, [],
                        helpstr='additional options for apqc_make_tcsh.py')
        self.valid_opts.add_opt('-html_review_style', 1, [],
                        acplist=g_html_review_styles,
                        helpstr='generate ss review HTML pages')
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
        self.valid_opts.add_opt('-find_var_line_blocks', -1, [],
                        helpstr="find_variance_lines.tcsh for given blocks")
        self.valid_opts.add_opt('-radial_correlate', 1, [],
                        acplist=['yes','no'],
                        helpstr="compute correlations with spherical averages")
        self.valid_opts.add_opt('-radial_correlate_blocks', -1, [],
                        helpstr="run @radial_correlate after the given blocks")
        self.valid_opts.add_opt('-radial_correlate_opts', -1, [],
                        helpstr="extra options for any @rad_cor command")
        self.valid_opts.add_opt('-reg_echo', 1, [],
                        helpstr='specify echo to use for registration')
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
        self.valid_opts.add_opt('-write_ppi_3dD_scripts', 0, [],
                       helpstr="flag: write no-censor and PPI extras scripts")
        self.valid_opts.add_opt('-uvar', -2, [],
                        helpstr="specify a uvar its value(s)")
        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr="set the verbose level")

        # block options
        self.valid_opts.add_opt('-tcat_preSS_warn_limit', 1, [],
                        helpstr='set limit where TR #0 outliers suggest pre-SS')
        self.valid_opts.add_opt('-tcat_remove_first_trs', -1, [],
                        helpstr='num TRs to remove from start of each run')
        self.valid_opts.add_opt('-tcat_remove_last_trs', 1, [],
                        helpstr='num TRs to remove from end of each run')

        self.valid_opts.add_opt('-despike_mask', 0, [],
                        helpstr="allow 3dDespike to automask (-dilate 4)")
        self.valid_opts.add_opt('-despike_opts_3dDes', -1, [],
                        helpstr='additional options directly for 3dDespike')
        self.valid_opts.add_opt('-despike_new', 1, [],
                        acplist=g_despike_new_opts,
                        helpstr="(yes/no/...) run 3dDespike with -NEW method")

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

        self.valid_opts.add_opt('-blip_forward_dset', 1, [],
                        helpstr='forward blip dset for blip up/down corretion')
        self.valid_opts.add_opt('-blip_reverse_dset', 1, [],
                        helpstr='reverse blip dset for blip up/down corretion')
        self.valid_opts.add_opt('-blip_opts_qw', -1, [],
                        helpstr='additional options for 3dQwarp in blip block')
        self.valid_opts.add_opt('-blip_warp_dset', 1, [],
                        helpstr='specify a precomputed distortion warp dset')

        self.valid_opts.add_opt('-align_epi_ext_dset', 1, [],
                        helpstr='external EPI volume for align_epi_anat.py')
        self.valid_opts.add_opt('-align_opts_aea', -1, [],
                        helpstr='additional options for align_epi_anat.py')
        self.valid_opts.add_opt('-align_opts_eunif', -1, [],
                        helpstr='additional opts for epi unformity correction')
        self.valid_opts.add_opt('-align_epi_strip_method', 1, [],
                        acplist=['3dSkullStrip','3dAutomask','None'],
                        helpstr="specify method for 'skull stripping' the EPI")
        self.valid_opts.add_opt('-align_unifize_epi', 1, [],
                        acplist=['yes', 'no', 'unif', 'local'],
                        helpstr='3dUnifize EPI base before passing to aea.py')

        self.valid_opts.add_opt('-tlrc_anat', 0, [],
                        helpstr='run @auto_tlrc on anat from -copy_anat')
        self.valid_opts.add_opt('-tlrc_base', 1, [],
                        helpstr='alternate @auto_tlrc base (not TT_N27, say)')
        self.valid_opts.add_opt('-tlrc_copy_base', 1, [],
                        acplist=['yes', 'no'],
                        helpstr='make a local copy of the template')
        self.valid_opts.add_opt('-tlrc_opts_at', -1, [],
                        helpstr='additional options supplied to @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_affine_warped_dsets', 2, [],
                        helpstr='pass dsets that have already been aff_warped')
        self.valid_opts.add_opt('-tlrc_NL_awpy_rm', 1, [],
                        acplist=['yes','no'],
                        helpstr='remove work dir from auto_warp.py')
        self.valid_opts.add_opt('-tlrc_NL_warp', 0, [],
                        helpstr='use non-linear warping to template')
        self.valid_opts.add_opt('-tlrc_NL_warped_dsets', 3, [],
                        helpstr='pass dsets that have already been NL_warped')
        self.valid_opts.add_opt('-tlrc_NL_force_view', 1, [],
                        acplist=['yes','no'],
                        helpstr='force view when copying awpy result (y/n)')
        self.valid_opts.add_opt('-tlrc_no_ss', 0, [],
                        helpstr='do not skull-strip during @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_rmode', 1, [],
                        helpstr='resample mode applied in @auto_tlrc')
        self.valid_opts.add_opt('-tlrc_suffix', 1, [],
                        helpstr='suffix applied in @auto_tlrc (default: NONE)')

        self.valid_opts.add_opt('-volreg_align_e2a', 0, [],
                        helpstr="align EPI to anatomy (via align block)")
        self.valid_opts.add_opt('-volreg_align_to', 1, [],
                        acplist=['first','third', 'last', 'MIN_OUTLIER',
                                 'MEDIAN_BLIP'],
                        helpstr="align to first, third, last or MIN_OUTILER TR")
        self.valid_opts.add_opt('-volreg_allin_auto_stuff', -1, [],
                        helpstr="specify -*auto* options for 3dAllineate")
        self.valid_opts.add_opt('-volreg_allin_cost', 1, [],
                        helpstr="specify -cost for 3dAllineate in volreg [lpa]")
        self.valid_opts.add_opt('-volreg_allin_warp', 1, [],
                        acplist=['shift_rotate', 'shift_rotate_scale',
                                 'affine_general'],
                        helpstr="specify -warp for 3dAllineate in volreg")
        self.valid_opts.add_opt('-volreg_no_volreg', 0, [],
                        helpstr="skip 3dvolreg step (maybe already aligned)")
        self.valid_opts.add_opt('-volreg_post_vr_allin', 1, [],
                        acplist=['yes','no'],
                        helpstr='do cross-run allin after within-run volreg')
        self.valid_opts.add_opt('-volreg_pvra_base_index', 1, [],
                        helpstr='specify base index or MIN_OUTLIER for align')
        self.valid_opts.add_opt('-volreg_base_dset', 1, [],
                        helpstr='external dataset to use as volreg base')
        self.valid_opts.add_opt('-volreg_base_ind', 2, [],
                        helpstr='run/sub-brick indices for volreg')
        self.valid_opts.add_opt('-volreg_compute_tsnr', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute TSNR datasets (yes/no) of volreg run1')
        self.valid_opts.add_opt('-volreg_get_allcostX', 1, [],
                        acplist=['yes','no'],
                        helpstr='compute all final EPI/anat alignment costs')
        self.valid_opts.add_opt('-volreg_interp', 1, [],
                        helpstr='interpolation method used in volreg')
        self.valid_opts.add_opt('-volreg_warp_final_interp', 1, [],
                        helpstr='final interpolation used when apply warps')
        self.valid_opts.add_opt('-volreg_warp_master', 1, [],
                        helpstr='grid master applied to volreg warp')
        self.valid_opts.add_opt('-volreg_method', 1, [],
                        acplist=['3dvolreg','3dAllineate'],
                        helpstr='specify program for EPI volume registration')
        # rcr - antiquate old motsim options
        self.valid_opts.add_opt('-volreg_motsim', 0, [],
                        helpstr='create a motion simulated time series')
        self.valid_opts.add_opt('-volreg_opts_ms', -1, [],
                        helpstr='add options directly to @simulate_motion')
        # new motsim option
        self.valid_opts.add_opt('-volreg_motsim_create', -1, [],
                        acplist=motsim_types,
                        helpstr='create motion simulated dsets of given types')
        # rcr - move this
        self.valid_opts.add_opt('-regress_motsim_PC', 2, [],
                        helpstr='regress given number of PCs from given TYPE')

        self.valid_opts.add_opt('-volreg_no_extent_mask', 0, [],
                        helpstr='do not restrict warped EPI to extents')
        self.valid_opts.add_opt('-volreg_opts_vr', -1, [],
                        helpstr='additional options directly for 3dvolreg')
        self.valid_opts.add_opt('-volreg_opts_ewarp', -1, [],
                        helpstr='additional opts for epi warp (allin/Nwarp)')
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

        self.valid_opts.add_opt('-combine_method', 1, [],
                        acplist=g_oc_methods,
                        helpstr='specify method for combining echoes per run')
        self.valid_opts.add_opt('-combine_opts_tedana', -1, [],
                        helpstr='specify extra options for tedana.py')
        self.valid_opts.add_opt('-combine_opts_tedwrap', -1, [],
                        helpstr='specify extra options for tedana_wrapper.py')
        self.valid_opts.add_opt('-combine_tedana_path', 1, [],
                        helpstr='specify path to tedana.py')
        self.valid_opts.add_opt('-combine_tedana_save_all', 1, [],
                        acplist=['yes','no'],
                        helpstr='save tedana preproc data (stack): yes/no')
        self.valid_opts.add_opt('-combine_tedort_reject_midk', 1, [],
                        acplist=['yes','no'],
                        helpstr='reject mid components: yes/no (def: yes)')

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

        # acplist=['epi', 'anat', 'epi_anat', 'group', 'extents'],
        # - allow user-specified masks as well [18 Oct 2021 rickr]
        self.valid_opts.add_opt('-mask_apply', 1, [],
                        helpstr="select mask to apply in regression")

        self.valid_opts.add_opt('-mask_dilate', 1, [],
                        helpstr="dilation to be applied in automask")
        self.valid_opts.add_opt('-mask_opts_automask', -1, [],
                        helpstr="additional options for 3dAutomask")
        self.valid_opts.add_opt('-mask_epi_anat', 1, [],
                        acplist=['yes','no'],
                        helpstr='use epi_anat rather than EPI mask (yes/no)')
        self.valid_opts.add_opt('-mask_import', 2, [],
                        helpstr="import mask as given label (label/mset)")
        self.valid_opts.add_opt('-mask_intersect', 3, [],
                        helpstr="create new mask by intersecting 2 others")
        self.valid_opts.add_opt('-mask_union', 3, [],
                        helpstr="create new mask by taking union of 2 others")
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
        self.valid_opts.add_opt('-ROI_import', 2, [],
                        helpstr="import ROI as given label (label/mset)")

        self.valid_opts.add_opt('-scale_max_val', 1, [],
                        helpstr="maximum value for scaled data (def: 200)")
        self.valid_opts.add_opt('-scale_no_max', 0, [],
                        helpstr="do not limit scaled data")

        self.valid_opts.add_opt('-regress_3dD_stop', 0, [],
                        helpstr="stop 3dDeconvolve after matrix generation")
        self.valid_opts.add_opt('-regress_anaticor', 0, [],
                        helpstr="apply ANATICOR: regress WMeLocal time series")
        self.valid_opts.add_opt('-regress_anaticor_fast', 0, [],
                        helpstr="fast ANATICOR: regress WMeLocal time series")
        self.valid_opts.add_opt('-regress_anaticor_fwhm', 1, [],
                        helpstr="specify FWHM for fast WMeLocal extraction")
        self.valid_opts.add_opt('-regress_anaticor_radius', 1, [],
                        helpstr="specify radius for WMeLocal extraction")
        self.valid_opts.add_opt('-regress_anaticor_term_frac', 1, [],
                        helpstr="specify termination fraction for blur radius")
        self.valid_opts.add_opt('-regress_anaticor_label', 1, [],
                        helpstr="specify ROI label for anaticor (default=WMe)")
        self.valid_opts.add_opt('-regress_anaticor_full_gaussian', 1, [],
                        acplist=['yes','no'],
                        helpstr="specify whether to truncate to flat Gaussian")
        self.valid_opts.add_opt('-regress_apply_mask', 0, [],
                        helpstr="apply the mask in regression")
        self.valid_opts.add_opt('-regress_apply_ricor', 1, [],
                        acplist=['yes','no'],
                        helpstr="apply ricor regs in regression (def no)")
        self.valid_opts.add_opt('-regress_bandpass', -2, [],
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
        self.valid_opts.add_opt('-regress_compute_tsnr_stats', -2, [],
                        helpstr='compute TSNR stats per ROI_dset and INDEX')
        self.valid_opts.add_opt('-regress_compute_auto_tsnr_stats', 1, [],
                        acplist=['yes','no'],
                        helpstr='auto-compute stats for APQC atlas (def=yes)')
        self.valid_opts.add_opt('-regress_mask_tsnr', 1, [],
                        acplist=['yes','no'],
                        helpstr="apply mask to TSNR dset (yes/no, def=no)")
        self.valid_opts.add_opt('-regress_cormat_warnings', 1, [],
                        acplist=['yes','no'],
                        helpstr='show any correl warns from Xmat (def=yes)')
        self.valid_opts.add_opt('-regress_show_df_info', 1, [],
                        acplist=['yes','no'],
                        helpstr='show DoF/regressor info from Xmat (def=yes)')
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
        self.valid_opts.add_opt('-regress_skip_censor', 0, [],
                        helpstr="process normally, but omit 3dD -censor option")

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
                        helpstr="add offset to timing")
        self.valid_opts.add_opt('-regress_stim_types', -1, [], okdash=0,
                        acplist=stim_file_types,
                        helpstr="specify times/AM1/AM2/IM for each stim class")
        self.valid_opts.add_opt('-regress_use_stim_files', 0, [],
                        helpstr="do not convert stim_files to timing")
        self.valid_opts.add_opt('-regress_use_tproject', 1, [],
                        acplist=['yes','no'],
                        helpstr="use 3dTproject instead of 3dDeconvolve")

        self.valid_opts.add_opt('-regress_apply_mot_types', -1, [],
                        acplist=['basic','demean','deriv','none'],
                        helpstr="specify which motion parameters to apply")
        self.valid_opts.add_opt('-regress_mot_as_ort', 1, [],
                        acplist=['yes','no'],
                        helpstr="apply motion params via -ortvec (def: yes)")
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
        self.valid_opts.add_opt('-regress_extra_ortvec', -1, [], okdash=0,
                        helpstr="extra -ortvec opts in regression")
        self.valid_opts.add_opt('-regress_extra_ortvec_labels',
                                -1, [], okdash=0,
                        helpstr="labels for extra -stim_files")
        self.valid_opts.add_opt('-regress_ppi_stim_files', -1, [], okdash=0,
                        helpstr="extra PPI -stim_files to apply")
        self.valid_opts.add_opt('-regress_ppi_stim_labels', -1, [], okdash=0,
                        helpstr="extra PPI -stim_labels to apply")

        self.valid_opts.add_opt('-regress_compute_fitts', 0, [],
                        helpstr="compute fitts only after 3dDeconvolve")
        self.valid_opts.add_opt('-regress_est_blur_detrend', 1, [],
                        acplist=['yes','no'],
                        helpstr="apply -detrend in blur_estimation (def=yes)")
        self.valid_opts.add_opt('-regress_est_blur_epits', 0, [],
                        helpstr="estimate blur from scaled EPI time series")
        self.valid_opts.add_opt('-regress_est_blur_errts', 0, [],
                        helpstr="estimate blur from scaled error time series")
        self.valid_opts.add_opt('-regress_opts_fwhmx', -1, [],
                        helpstr="additional options for 3dFWHMx")
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
        self.valid_opts.add_opt('-regress_make_corr_AIC', 0, [],
                        helpstr="if WMeLocal, compute correlation volume")
        self.valid_opts.add_opt('-regress_make_corr_vols', -1, [],
                        helpstr="get correlation volumes for given ROIs")
        self.valid_opts.add_opt('-regress_no_ideal_sum', 0, [],
                        helpstr="do not compute the sum of regressors")
        self.valid_opts.add_opt('-regress_no_fitts', 0, [],
                        helpstr="do not output a fit timeseries dataset")
        self.valid_opts.add_opt('-regress_no_ideals', 0, [],
                        helpstr="do not generate ideal regressors")
        self.valid_opts.add_opt('-regress_no_iresp', 0, [],
                        helpstr="do not output impulse response datasets")
        self.valid_opts.add_opt('-regress_no_mask', 0, [],
                        helpstr="do not apply any mask during regression")
        self.valid_opts.add_opt('-regress_no_motion', 0, [],
                        helpstr="do not apply motion parameters in regression")
        self.valid_opts.add_opt('-regress_opts_3dD', -1, [],
                        helpstr='additional options for 3dDeconvolve')
        self.valid_opts.add_opt('-regress_opts_reml', -1, [],
                        helpstr='additional options for 3dREMLfit')
        self.valid_opts.add_opt('-regress_reml_exec', 0, [],
                        helpstr="execute 3dREMLfit command script")
        self.valid_opts.add_opt('-regress_ROI', -1, [], okdash=0,
                        helpstr="regress out known ROIs")
        self.valid_opts.add_opt('-regress_ROI_per_run', -1, [], okdash=0,
                        helpstr="regress given ROIs averages per run")
        self.valid_opts.add_opt('-regress_ROI_PC', 2, [], okdash=0,
                        helpstr="regress PCs from ROI (label num_pc)")
        self.valid_opts.add_opt('-regress_ROI_PC_per_run', -1, [], okdash=0,
                        helpstr="regress PCs of given ROIs per run")
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
                        acplist=clustsim_types,
                        helpstr="add 3dClustSim attrs to regression bucket")

        # PPI options

        self.valid_opts.trailers = 0   # do not allow unknown options
        
    def get_user_opts(self):
        self.valid_opts.check_special_opts(self.argv)
        self.user_opts = read_options(self.argv, self.valid_opts)
        if self.user_opts == None: return 1     # error condition
        if len(self.user_opts.olist) == 0:      # no options: apply -help
            show_program_help()
            return 0
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt: print("** seem to have trailers, but cannot find them!")
            else: print("** have invalid trailing args: %s" % opt.parlist)
            return 1  # failure

        # maybe the users just wants a complete option list
        if self.user_opts.find_opt('-show_valid_opts'):
            self.valid_opts.show('', 1)
            return 0  # gentle termination
        
        # apply the user options
        rv = self.apply_initial_opts(self.user_opts)
        if rv != None: return rv

        # update out_dir now (may combine option results)
        if self.out_dir == '': self.out_dir = '%s.results' % self.subj_label
        self.od_var = '$output_dir'

        # create empty file tracking list
        self.tlist = TrackedFlist(self, subj_id=self.subj_id)

        if self.verb > 1: show_args_as_command(self.argv, "executing command:")
        if self.verb > 4: show_args_as_command(sys.argv, "system command:")
        if self.verb > 1: self.show('end get_user_opts ')

    # apply the general options - many terminate program
    def apply_initial_opts(self, opt_list):
        opt = opt_list.find_opt('-verb')   # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        if opt_list.find_opt('-help'):     # just print help
            show_program_help()
            return 0  # gentle termination
        
        if opt_list.find_opt('-help_section'):     # just print help
            section, rv = opt_list.get_string_opt('-help_section')
            if section == 'EGafni':
               EGS = self.egs()
               EGS.display_eg_all(aphelp=1,verb=2)
            elif section == 'EGall':
               EGS = self.egs()
               EGS.display_eg_all(aphelp=-1,verb=2)
            else:
               show_program_help(section=section)
            return 0  # gentle termination
        
        if opt_list.find_opt('-help_tedana_files'):
            self.show_tedana_files()
            return 0  # gentle termination
        
        if opt_list.find_opt('-hist'):     # print the history
            print(g_history)
            return 0  # gentle termination
        
        if opt_list.find_opt('-hist_milestones'):     # print the history
            print(g_milestones)
            return 0  # gentle termination
        
        if opt_list.find_opt('-requires_afni_version'): # print required version
            print(g_requires_afni[0][0])
            return 0  # gentle termination
        
        if opt_list.find_opt('-requires_afni_hist'): # print required history
            hlist = ['   %11s, for : %s' % (h[0],h[1]) for h in g_requires_afni]
            print('%s' % '\n'.join(hlist))
            return 0  # gentle termination
        
        if opt_list.find_opt('-show_process_changes'):
            print(g_process_changes_str)
            return 0
        
        if opt_list.find_opt('-show_pretty_command'):
            tstr = self.get_ap_command_str(style='pretty', lstart='')
            print(tstr)
            return 0
        
        if opt_list.find_opt('-show_pythonic_command'):
            tstr = self.get_ap_pythonic_cmd_str()
            print(tstr)
            return 0
        
        if opt_list.find_opt('-show_tracked_files'):
            self.show_tfiles,rv = opt_list.get_string_opt('-show_tracked_files')
        
        if opt_list.find_opt('-todo'):     # print "todo" list
            print(g_todo_str)
            return 0
        
        if opt_list.find_opt('-ver'):      # show the version string
            print(g_version)
            return 0  # gentle termination

        # ------------------------------------------------------------
        # example, compare and merge options - to compare option lists
        if opt_list.find_opt('-compare_opts'):
           comp, rv = opt_list.get_string_opt('-compare_opts')
           self.compare_vs_opts(opt_list.olist, comp)
           return 0
        
        # compare early vs later opts (2 commands)
        if opt_list.find_opt('-compare_opts_vs_opts'):
           # separate option lists at index of passed opt
           oind = opt_list.find_opt_index('-compare_opts_vs_opts')
           self.compare_opts_vs_opts(opt_list.olist, oind)
           return 0
        
        if opt_list.find_opt('-compare_example_pair'):
           pair, rv = opt_list.get_string_list('-compare_example_pair')
           self.compare_example_pair(pair)
           return 0
        
        if opt_list.find_opt('-compare_merged_opts'):
           target, rv = opt_list.get_string_opt('-compare_merged_opts')
           self.compare_merged_opts(opt_list.olist, target)
           return 0

        if opt_list.find_opt('-show_example'):
           eg, rv = opt_list.get_string_opt('-show_example')
           self.show_example(eg, verb=self.verb)
           return 0

        if opt_list.find_opt('-show_example_keywords'):
           self.show_example_keywords(verb=self.verb)
           return 0
        
        if opt_list.find_opt('-show_example_names'):
           # if -verb was passed, use it, else let default apply
           # (ugly, but lets the default be more of what would be expected)
           if opt_list.find_opt('-verb'):
              self.show_example_names(verb=self.verb)
           else:
              self.show_example_names()
           return 0
        
        if opt_list.find_opt('-show_merged_opts'):
           target, rv = opt_list.get_string_opt('-show_merged_opts')
           return self.show_merged_command(target, verb=self.verb)

        # options which are NO LONGER VALID

        if opt_list.find_opt('-surf_blur_fwhm'):
            print('** option -surf_blur_fwhm is no longer valid\n' \
                  '   (please stick with -blur_size)\n')
            return 1

        # end terminal options

        # --------------------------------------------------
        # options that imply options in _prior_ blocks

        # anaticor implies segment, unless a label is specified
        if opt_list.find_opt('-regress_anaticor') \
           or opt_list.find_opt('-regress_anaticor_fast'):
           if not opt_list.find_opt('-regress_anaticor_label'):
              if opt_list.find_opt('-anat_follower_ROI'):
                 print('** applying anaticor without label, using 3dSeg mask')
              elif self.verb:
                 print('++ applying anaticor without label, using 3dSeg mask')
              opt_list.add_opt("-mask_segment_anat", 1, ["yes"], setpar=1)
              opt_list.add_opt("-mask_segment_erode", 1, ["yes"], setpar=1)

           # rcr - what if label is from auto segment anyway?  so...
           # else: possibly add anyway

        # end options that imply other options
        # --------------------------------------------------

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

        opt = opt_list.find_opt('-bids_deriv') # make BIDS deriv?
        if opt != None: self.bids_deriv = opt.parlist[0]

        opt = opt_list.find_opt('-copy_anat')
        if opt != None:
            self.anat = gen_afni_name(opt.parlist[0])
            # rcr - set only if no view in anat?  (though still would not know)
            self.tlrcanat = self.anat.new(new_view='+tlrc')

        opt = opt_list.find_opt('-gen_epi_review')  # name epi review script
        if opt != None: self.epi_review = opt.parlist[0]

        opt = opt_list.find_opt('-no_epi_review') # no epi review script
        if opt != None: self.epi_review = None

        opt = opt_list.find_opt('-html_review_style') # for apqc_make_tcsh.py
        if opt != None: self.html_rev_style = opt.parlist[0]

        opt = opt_list.find_opt('-html_review_opts') # for apqc_make_tcsh.py
        if opt != None: self.html_rev_opts = opt.parlist

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

        opt = opt_list.find_opt('-uvar')
        olist = self.user_opts.find_all_opts('-uvar')
        for opt in olist:
           self.uvars.set_var(opt.parlist[0], opt.parlist[1:])

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

    def _have_me(self):
       """return 1 if we have multi-echo data, else 0"""
       if len(self.dsets_me)    == 0: return 0
       if len(self.dsets_me[0]) == 0: return 0
       if self.reg_echo == 0:         return 0
       if self.num_echo  < 2:         return 0

       return 1

    def get_dsets(self):
        """get list of datasets
        
           if -dsets,         there should be one list of runs
                              (populate self.dsets)
           if -dsets_me_echo, there should be one option list of runs per echo
                              (populate self.dsets_me, and other fields)
           if -dsets_me_run,  there should be one option list of echoes per run
                              (populate self.dsets_me, and other fields)

           populate:
             self.dsets       all, or of favorite echo
             self.dsets_me    in echo-major order (dsets = dsets_me[fave])
             self.num_echo
             self.have_me, use_me

           test:
             some -dsets* option
             dataset existence (if self.test_dsets)
             rectangular list

           return 0 on success, >0 on error
        """

        o0 = self.user_opts.find_opt('-dsets')
        o1 = self.user_opts.find_opt('-dsets_me_echo')
        o2 = self.user_opts.find_opt('-dsets_me_run')

        errs = 0

        # simple form: just populate (and check for other forms)
        if o0:
           if o1 or o2:
              print("** cannot use -dsets_me_* with -dsets")
              return 1
           self.dsets = []
           for rind, dset in enumerate(o0.parlist):
              aname = gen_afni_name(dset)
              if aname.selectors() != '': self.have_sels = 1
              aname.selquote = "'" # allow for $ in selector (default??)
              self.dsets.append(aname)
              if self.test_dsets and not aname.exist():
                 print("** missing run %d dataset: %s"%(rind+1, dset))
                 errs += 1
           return errs
           
        # now there should be some ME option

        if o1 and o2:
           print("** cannot use both -dsets_me_echo and -dsets_me_run")
           return 1

        if not o1 and not o2:
           print("** missing input: some -dsets* option is required")
           return 1

        # okay, we have an ME condition

        # populate dme as rectangular echo-major list of datasets
        # (save tests for later, except to make sure it is rectangular)
        dme = []

        if o1:
           oname = '-dsets_me_echo'
           olist = self.user_opts.find_all_opts(oname)
           nruns = len(olist[0].parlist)
           for eind, opt in enumerate(olist):
              nr = len(opt.parlist)
              if nr != nruns:
                 print("** echo 1 has %d runs, but echo %d has %d runs" \
                       % (nruns, eind+1, nr))
                 errs += 1

              dsets = []
              for rind, dset in enumerate(opt.parlist):
                 dsets.append(gen_afni_name(dset))
              if len(dsets) == 0:
                 print("** have echo %d option %s without any dataset list" \
                       % (eind, oname))
                 errs += 1

              # and populate dme array, per echo, for now
              dme.append(dsets)

           # already echo-major order, no need to transpose

        if o2:
           # input is run-major order, will convert to echo-major at end
           oname = '-dsets_me_run'
           olist = self.user_opts.find_all_opts(oname)
           necho = len(olist[0].parlist)
           for rind, opt in enumerate(olist):
              ne = len(opt.parlist)
              if ne != necho:
                 print("** run 1 has %d echoes, but run %d has %d echoes" \
                       % (necho, rind+1, ne))
                 errs += 1

              dsets = []
              for eind, dset in enumerate(opt.parlist):
                 dsets.append(gen_afni_name(dset))
              if len(dsets) == 0:
                 print("** have run %d option %s without any dataset list" \
                       % (rind, oname))
                 errs += 1

              # and populate dme array, per echo, for now
              dme.append(dsets)

           # cannot transpose if not rectangular (errs > 0)
           if errs: return 1

           # now convert dme from run major order to echo major order
           nruns = len(dme)
           rlist = [[dme[rind][eind] for rind in range(nruns)] \
                                     for eind in range(necho) ]
           dme = rlist

        # set selector quote and possibly check for existence
        for eind, elist in enumerate(dme):
           for rind, dset in enumerate(elist):
              if dset.selectors() != '': self.have_sels = 1
              dset.selquote = "'" # allow for $ in selector (default??)
              if self.test_dsets:
                 if not dset.exist():
                    print('** missing run %d echo %d dataset: %s' \
                          % (rind+1, eind+1, dset.rpv()))
                    errs += 1

        # populate everything
        self.dsets_me = dme
        self.dsets = self.dsets_me[0]
        self.have_me = 1
        self.use_me = self.have_me
        self.num_echo = len(dme)

        # return the status
        return errs

    def init_dsets(self):
        """process -dsets or -dsets_me* options

           return 0 on success, 1 on error
        """

        # process any -dsets* option
        # (populate self.dsets, self.dsets_me, self.num_echo, self.have_me)
        if self.get_dsets(): return 1

        errs = 0

        # if ME, then fill reg_echo via -reg_echo (registration echo)
        # also, try to get echo times
        if self.have_me:
           oname = '-reg_echo'
           self.reg_echo = 2
           self.regecho_var = '$fave_echo'
           val, err = self.user_opts.get_type_opt(int, oname)
           if err: errs += 1
           if val != None:
              self.reg_echo = val

           # set as a uvar (values are strings)
           self.uvars.set_var('reg_echo', [str(self.reg_echo)])

           if self.reg_echo < 1 or self.reg_echo > self.num_echo:
              print("** %s: registration echo must be between 1 and %d" \
                    % (oname, self.num_echo))
              return 1

           oname = '-echo_times'
           elist, err = self.user_opts.get_type_list(float, oname)
           if not err and elist != None:
              self.echo_times = elist
              if len(elist) != self.num_echo:
                 errs += 1
                 print("** have %d echoes, but %d echo times" \
                       % (self.num_echo, len(elist)))
              # and add echo times as a uvar, using the original strings
              opt = self.user_opts.find_opt(oname)
              self.uvars.set_var('echo_times', opt.parlist)

        # set view and dimensions based on dsets

        # init view
        view = self.dsets[0].view
        if view and view != self.view:
           self.view = view
           self.origview = view
           if self.verb > 0: print('-- applying view as %s' % view)
        elif not view:
           view = dset_view(self.dsets[0].rel_input())
           self.view = view
           self.origview = view
           if self.verb > 0: print('-- applying input view as %s' % view)


        # get voxel dimensions
        dims = UTIL.get_3dinfo_val_list(self.dsets[0].rel_input(),
                                        'd3', float, verb=1)
        if dims == None: return 1
        self.orig_delta = dims
        self.delta = dims

        # note obliquity of first EPI dset
        self.dsets_obl = dset_is_oblique(self.dsets[0], self.verb)

        return errs

    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        # first, note datasets
        if self.init_dsets(): return 1

        # next, check for -surf_anat, which defines whether to do volume
        # or surface analysis
        opt = self.user_opts.find_opt('-surf_anat')
        if opt != None:
           self.surf_anat = gen_afni_name(opt.parlist[0])

        # init block either from DefLabels or -blocks
        opt = self.user_opts.find_opt('-blocks')
        if opt:  # then create blocklist from user opts (but prepend tcat)
            if opt.parlist[0] != 'tcat':
                blocks = ['tcat'] + opt.parlist
            else: blocks = opt.parlist
        elif self.surf_anat: blocks = DefSurfLabs  # surface defaults
        else:                blocks = DefLabels    # volume defaults

        # and insert automatic postdata block at position 1 (after tcat)
        blocks.insert(1, 'postdata')

        # just do a quick check after all of the confusion
        if blocks[0] != 'tcat' or blocks[1] != 'postdata':
           print('** block list should start with tcat,postdata, have:\n   %s'\
                 % blocks)
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

        # do we need motsim block?
        if self.need_motsim_block(blocks):
           err, blocks = self.add_block_after_label(blocks, 'motsim', 'volreg')
           if err: return 1

        # do we want the blip block?
        if (    self.user_opts.find_opt('-blip_warp_dset')      \
             or self.user_opts.find_opt('-blip_reverse_dset') ) \
             and not 'blip' in blocks:
           err, blocks = self.add_block_to_list(blocks, 'blip')
           if err: return 1

        # if user has supplied options for blocks that are not used, fail
        if self.opts_include_unused_blocks(blocks, 1): return 1

        # check for unique blocks (except for 'empty')
        if not self.blocks_are_unique(blocks):
            print('** blocks must be unique\n'  \
                  '   (is there overlap between -blocks and -do_block?)\n')
            return 1

        # make the list of block names available to the mod functions
        self.block_names = blocks

        # call db_mod_functions

        self.bindex = 0
        self.pblabels = []
        for label in blocks:
            # create ProcessBlock instance, calling BlockModFunc
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
                    print("** error: missing block '%s' in ask_me update"%label)
                    return 1
                BlockModFunc[label](block, self, self.user_opts) # modify block

        # rcr - if there is another place to update options from, do it
        uopt = self.user_opts.find_opt('-opt_source')
        if uopt != None:
            print('** not ready to update options from %s' % str(uopt.parlist))
            return 1

        # process any options that fill block.post_funcs, to modify post_cmd
        # (e.g. -radial_correlate_blocks)
        if self.set_post_funcs(): return 1

        # do some final error checking
        if len(self.dsets) == 0:
            print('error: dsets have not been specified (consider -dsets)')
            return 1

        # no errors, just warn the user (for J Britton)   25 May 2011
        uniq_list_as_dsets(self.dsets, 1)
        self.check_block_order()

    def set_post_funcs(self):
        """process any options that should set post_funcs for some blocks

              - applied post_funcs are called as FUNC(proc, block)

           return 0 on success
        """

        post_func_list = [
            #   option name                  function name (as string)
            ['-radial_correlate_blocks',    'run_radial_correlate'],
            ['-find_var_line_blocks',       'run_qc_var_line_blocks'],
        ]

        for fstuff in post_func_list:
            oname = fstuff[0]           # option name
            fname = fstuff[1]           # function as string
            func =  eval(fname)         # actual function

            rcblocks, rv = self.user_opts.get_string_list(oname)
            if rv: return 1

            # for radcor, make 'regress' the default
            if oname == '-radial_correlate_blocks' and rcblocks is None:
               if self.find_block('regress') and not self.surf_anat:
                 print("-- including default: -radial_correlate_blocks regress")
                 rcblocks = ['regress']

            # for vlines, make 'tcat' the default
            if oname == '-find_var_line_blocks' and rcblocks is None:
               print("-- including default: -find_var_line_blocks tcat")
               rcblocks = ['tcat']

            # was this option used?
            if rcblocks != None:
               valid_blocks = ['tcat', 'tshift', 'volreg', 'blur',
                               'scale', 'regress']

               # special case: block NONE turns off the option
               if "NONE" in rcblocks:
                  if self.verb > 1:
                     print("-- block NONE, so skipping option %s" % oname)
                  continue

               for blabel in rcblocks:
                   block = self.find_block(blabel)
                   if not block:
                      print("** no '%s' block for option %s" % (blabel, oname))
                      return 1
                   if not blabel in valid_blocks:
                      print("** block '%s' is not valid for %s" \
                            % (blabel, oname))
                      print("   (valid blocks are %s)" % \
                            ', '.join(valid_blocks))
                      return 1

                   if self.verb > 3:
                      print('++ appending %s to block.post_funcs in block %s' \
                            % (fname, blabel))
                   block.post_funcs.append(func)

        return 0

    def need_motsim_block(self, blocks):
        """want motsim if there are any -volreg_motsim_create options
              or -regress_motsim options
           but do not add duplicate block
        """
        if 'motsim' in blocks: return 0

        if self.user_opts.find_opt('-volreg_motsim_create'): return 1
        if self.user_opts.find_opt('-regress_motsim_PC'): return 1

        return 0

    def add_block_to_list(self, blocks, bname, adj=None, direct=0):
        """given current block list, add a block for bname after that
           of prevlab or from OtherDefLabels if None

                blocks : current list of block labels
                bname  : label of block to insert
                adj    : name of adjacent block (if None, try to decide)
                direct : if adj, direct is direction of bname to adj
                         (-1 : bname is before, 1: bname is after)
           
           return error code and new list"""

        # if we are not given an adjacent block, try to find one
        if not adj:
            direct, adj = self.find_best_block_posn(blocks, bname)
            if not direct: return 1, blocks

        # good cases
        if direct < 0: return self.add_block_before_label(blocks, bname, adj)
        if direct > 0: return self.add_block_after_label(blocks, bname, adj)

        # failure
        print("** ABTL: have adj=%s but no direct" % adj)
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

        if bname == 'blip':
            try: vind = blocks.index('volreg')
            except: vind = -1
            try: aind = blocks.index('align')
            except: aind = -1
            try: tind = blocks.index('tlrc')
            except: tind = -1

            # if volreg, put before first of align, tlrc, volreg
            if vind >= 0:
               if aind >= 0:
                  if vind > aind: return -1, 'align'    # before align
                  else:           return -1, 'volreg'   # before volreg
               if tind >= 0 and vind > tind: return -1, 'tlrc' 
               return -1, 'volreg'                      # before volreg

            # so no volreg

            if aind >= 0: return -1, 'align'    # before align
            if tind >= 0: return -1, 'tlrc'     # before tlrc

            # work our way back
            if self.find.block('tshift'):  return 1, 'tshift'
            if self.find.block('ricor'):   return 1, 'ricor'
            if self.find.block('despike'): return 1, 'despike'

            return 1, 'tcat'

        # if those didn't apply, go with the OtherDefLabels array

        try: prevlab = OtherDefLabels[bname]
        except:
            print("** failed to find position for block '%s' in %s" \
                  % (bname, blocks))
            prevlab = ''
        
        if prevlab == '': return 0, prevlab     # failure
        else:             return 1, prevlab     # success

    def add_block_after_label(self, blocks, bname, prelab):
        """add a block for bname after that of prelab
           
           return error code and new list"""
        err = 0
        if not bname in BlockLabels:
            print("** ABAL error: block '%s' is invalid" % bname)
            return 1, blocks
        if not prelab in BlockLabels:
            print("** ABAL error: prelab '%s' is invalid" % prelab)
            return 1, blocks
        try: preindex = blocks.index(prelab)
        except:     
            print("** cannot find block '%s' to insert block '%s' after" \
                  % (prelab, bname))
            preindex = 0
            err = 1
        if preindex < 0:
            print("** error: blocks.index failure for '%s'" % prelab)
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
            print("** ABBL error: block '%s' is invalid" % bname)
            return 1, blocks
        if not postlab in BlockLabels:
            print("** ABBL error: postlab '%s' is invalid" % postlab)
            return 1, blocks
        try: postindex = blocks.index(postlab)
        except:     
            print("** cannot find block '%s' to insert block '%s' before" \
                  % (postlab, bname))
            postindex = 0
            err = 1
        if postindex < 0:
            print("** error: blocks.index failure for '%s'" % postlab)
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
               print("** script creation failure for block '%s'" % block.label)
               errs += 1
               break

            # allow for early termination
            if cmd_str == 'DONE': return None

            # run any post_funcs for this block
            for post_func in block.post_funcs:
               rv, cstr = post_func(self, block)
               if rv:
                  print('** failed block.post_funcs for block %s' % block.label)
                  return None
               block.post_cstr += cstr

            if block.post_cstr != '':
               if self.verb > 2:
                  print('++ adding post_cstr to block %s:\n%s=======' \
                        % (block.label, block.post_cstr))
               cmd_str += block.post_cstr
            self.write_text(add_line_wrappers(cmd_str))
            if self.verb>3: block.show('+d post command creation: ')
            if self.verb>4: print('+d %s cmd: \n%s'%(block.label, cmd_str))

        if self.epi_review:
            cmd_str = db_cmd_gen_review(self)
            if cmd_str:
                # no wrappers for QC block (out.ap_uvars.txt) [10 May 2022]
                # self.write_text(add_line_wrappers(cmd_str))
                self.write_text(cmd_str)
                if self.verb > 1:
                    print("+d generated EPI review script %s" % self.epi_review)
            else:
                errs += 1
                if self.verb > 1:
                    print('** failed to generate EPI review script')

        rv = self.finalize_script()     # finish the script
        if rv: errs += 1

        self.close_script()
        self.make_exec()

        if errs > 0:
            # default to removing any created script
            opt = self.user_opts.find_opt('-keep_script_on_err')
            if not opt or opt_is_no(opt):
                if os.path.isfile(self.script): os.remove(self.script)
            return 1    # so we print all errors before leaving

        self.report_final_messages()

    def report_final_messages(self):
        """check over various conditions"""

        if self.verb > 0:
            # last warning, if user is masking EPI data...
            if self.mask != None:
                if self.regmask:
                  if self.mask != self.mask_group:
                    print("** masking single subject EPI is not recommended")
                    print("   (see 'MASKING NOTE' from the -help for details)")
                else:
                    print("-- using default: will not apply EPI Automask")
                    print("   (see 'MASKING NOTE' from the -help for details)")

            # no longer warn on this
            # if self.ricor_nreg > 0 and self.ricor_apply == 'no':
            #     if not self.user_opts.find_opt('-regress_apply_ricor'):
            #         print('** note: ricor regressors are no longer applied' \
            #                   ' in final regression')

            if self.runs == 1:
                print("\n-------------------------------------\n" \
                        "** warning have only 1 run to analyze\n" \
                        "-------------------------------------")

            print("\n--> script is file: %s\n" % self.script)
            print('    to execute via tcsh:\n'            \
                  '         %s\n\n'                       \
                  '    to execute via bash:\n'            \
                  '         %s\n' % (self.tcsh_cmd, self.bash_cmd))

        return

    def get_run_info(self):
        self.runs = len(self.dsets)
        if self.runs < 1:
            print("** have no dsets to analyze")
            return 1

        # updated by 'tcat' opteration (and -remove_trs option)
        # (use rpve to include NIfTI, etc.)
        dset = self.dsets[0].rpve(sel=1)

        err, self.reps, self.tr = get_dset_reps_tr(dset, verb=self.verb)
        if err: return 1   # check for failure

        # set reps in each run
        self.reps_all = []
        self.reps_vary = 0
        for dr in self.dsets:
            err, reps, tr = get_dset_reps_tr(dr.rpve(sel=1), verb=self.verb)
            if err: return 1
            self.reps_all.append(reps)
            if reps != self.reps: self.reps_vary = 1
            if tr != self.tr:
                print('** TR of %g (in %s) != run #1 TR %g' \
                      % (tr, dr.shortinput(), self.tr))
                return 1

        # check for consistency
        if self.have_me:
           for eind, esets in enumerate(self.dsets_me):
               for rind, dset in enumerate(esets):
                  err, reps, tr = get_dset_reps_tr(dset.rpve(sel=1),
                                                   verb=self.verb)
                  if err: return 1
                  if reps != self.reps_all[rind]:
                     print("** run %d reps differ between echo 1 (%d)" \
                           " and echo %d (%d)"                         \
                           % (rind+1, self.reps_all[rind], eind+1, reps))
                     return 1
                  if tr != self.tr:
                      print('** TR of %g (in %s) != run 1 echo 1 TR %g' \
                            %(tr, dset.shortinput(), self.tr))
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
            print('-- reps = %g, tr = %g, datatype = %g, scaled = %d' \
                  % (self.reps, self.tr, self.datatype, self.scaled))

    def get_vr_base_indices(self, verb=1):
        """return 0-based run and TR indices for volreg base
           (return runs==-1 if they cannot be set)
           if verb > 0 and values cannot be set, print why not"""
        
        block = self.find_block('volreg')
        if not block:
            if verb>0: print("** warning: no volreg block for vr_base_indices")
            return -1, -1
        opt = block.opts.find_opt('-volreg_base_ind')

        if not opt:
            if self.verb > 2: print('-- no -volreg_base_ind opt for vr_base')
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
            print('** invalid block : %s' % block.label)
            return 1
        if self.verb > 3: block.show('+d post init block: ')
        self.blocks.append(block)

    def find_block(self, label, mn=0, mx=-1):
        """mn = min index, mx = max index"""

        for block in self.blocks:
            if mn > 0 and block.index < mn:
               continue         # skip early blocks

            if block.label == label:
               return block     # found

            if mx >= 0 and block.index > mx:
               return None      # index too big, fail

        return None

    def find_block_order(self, b0, b1):
        """find order between blocks b0 and b1
           return:
               -2 : some block is not found
               -1 : b0 is first
                0 : they are the same
                1 : b0 is last
        """

        i0 = self.find_block_index(b0)
        i1 = self.find_block_index(b1)
        # not found
        if i0 < 0 or i1 < 0:
           return -2

        # return order
        if i0 < i1: return -1
        if i0 > i1: return 1

        return 0

    def find_block_opt(self, label, opt_name):
        """return any found comompt instance in block.opts"""
        for block in self.blocks:
            if block.label == label: return block.opts.find_opt(opt_name)
        return None

    def find_block_index(self, label):
        block = self.find_block(label)
        if block: return self.blocks.index(block)
        return -1

    def find_latest_block(self, lablist):
        """return the label of the latest block
           (i.e. with the highest index)

           if no label is found, return 'NONE'
        """
        
        # get maximum label index
        imax = UTIL.argmax([self.find_block_index(lab) for lab in lablist])

        # if actually found, return corresponding label
        rlabel = lablist[imax]
        if self.find_block_index(rlabel) >= 0: return lablist[imax]
    
        # failure
        return 'NONE'

    def find_block_or_prev(self, blabel, cblock):
       """find block of given label, or some prior block

          blabel = block label to search for
          cblock = current block

          - possibly use a search list
             - if volreg: fall back to surf/blur/scale/current block
          - fall back to previous block

          return labeled block or some prior block
       """

       # attempt a simple find, restricting index to current
       bind = cblock.index
       rblock = self.find_block(blabel, mx=bind)
       if rblock: return rblock

       if self.verb > 2:
          print('== FBOP: no block %s, searching before %d (%s)' \
                % (blabel, bind, cblock.label))

       # ** not found, search for a block and return the prior one **

       # if we know how to, search through a list
       slist = []
       if blabel == 'volreg': slist = ['surf', 'blur', 'scale']

       for sname in slist:
          rblock = self.find_block(sname, mx=bind)
          if rblock: break

       # if nothing found, return prior to current block
       if not rblock: rblock = cblock

       return self.find_block(self.prev_lab(rblock), mx=bind)

    def blocks_are_unique(self, blocks, exclude_list=['empty'], whine=1):
        """return whether the blocks are unique
                - ignore blocks in exclude_list
                - if whine, complain on repeats
           Process one by one, so we know what to complain about.
        """

        unique = 1
        checked = []
        for block in blocks:
            if block in exclude_list: continue
            if block in checked:
                if whine: print("** warning: block '%s' used multiple times" \
                                % block)
                unique = 0
            else: checked.append(block)

        return unique

    def opts_include_unused_blocks(self, blocks, whine=1,
                                   allow=['-volreg_base_dset']):
        """return whether options refer to blocks that are not being used"""

        # start with all BlockLabels and remove those in passed list
        badlist = BlockLabels[:]
        for label in blocks:
            if label in badlist: badlist.remove(label)

        if self.verb > 3: print('++ unused blocks: %s' % badlist)

        # for speed, make an explicit list of option prefixes to search for
        badlist = ['-%s_' % label for label in badlist]

        errs = 0
        for opt in self.user_opts.olist:
            ind = opt.name.find('_')
            if ind < 0: continue
            if opt.name in allow: continue
            if opt.name[0:ind+1] in badlist:
                if whine: print("** missing '%s' block for option '%s'" \
                                % (opt.name[1:ind], opt.name))
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
                print("** warning: 'align' should precede 'volreg'")
            if not self.blocks_ordered('align', 'tlrc'):
                errs += 1
                print("** warning: 'align' should precede 'tlrc'")
        if self.find_block('ricor'):
            if not self.blocks_ordered('despike', 'ricor', must_exist=1):
                errs += 1
                print("** warning: 'despike' should precede 'ricor'")
            if not self.blocks_ordered('ricor', 'tshift'):
                errs += 1
                print("** warning: 'tshift' should precede 'ricor'")
            if not self.blocks_ordered('ricor', 'volreg'):
                errs += 1
                print("** warning: 'volreg' should precede 'ricor'")
        if self.find_block('blur'):
            if not self.blocks_ordered('blur', 'scale'):
                errs += 1
                print("** warning: 'blur' should precede 'scale'")
        if self.find_block('regress'):
            if not self.blocks_ordered('tshift', 'regress'):
                errs += 1
                print("** warning: 'tshift' should precede 'regress'")
            if not self.blocks_ordered('volreg', 'regress'):
                errs += 1
                print("** warning: 'volreg' should precede 'regress'")
            if not self.blocks_ordered('blur', 'regress'):
                errs += 1
                print("** warning: 'blur' should precede 'regress'")
            if not self.blocks_ordered('scale', 'regress'):
                errs += 1
                print("** warning: 'scale' should precede 'regress'")

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
                print('** warning: missing block', end='')
                if bind0 < 0: print("'%s' (to precede '%s')" % (name0,name1))
                if bind1 < 0: print("'%s' (to follow '%s')" % (name1,name0))
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
        # - script might have a path, so set output file by modifying prefix
        outputname = UTIL.change_path_basename(self.script, 'output.', append=1)

        self.bash_cmd = 'tcsh %s %s 2>&1 | tee %s' % \
                        (opts, self.script, outputname)
        self.tcsh_cmd = 'tcsh %s %s |& tee %s'     % \
                        (opts, self.script, outputname)

        #if self.user_opts.find_opt('-bash'): self.exec_cmd = self.bash_cmd
        if self.user_opts.find_opt('-bash'):
           print("** -bash now unneeded: both tcsh and bash syntax are shown")

        self.write_text('# to execute via tcsh: \n'      \
                        '#   %s\n' % self.tcsh_cmd)
        self.write_text('# to execute via bash: \n'      \
                        '#   %s\n\n' % self.bash_cmd)

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
          'endif\n\n' % (g_requires_afni[0][0], g_requires_afni[0][0]) )

        # if we need to run external programs, make sure they are in the PATH
        if len(self.required_progs) > 0:
          tstr = "# will run external programs, so be sure they are in PATH\n"
          for prog in self.required_progs:
            tstr += "which %s\n"                                             \
                    "if ( $status ) then\n"                                  \
                    "   echo '** missing required external program: %s'\n"   \
                    "   echo '   (perhaps a conda environment is needed)'\n" \
                    "   exit 1\n"                                            \
                    "endif\n"                                                \
                    % (prog, prog)
          tstr += "\n"
          self.write_text(tstr)

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
        self.write_text('set runs = (`count_afni -digits %d 1 %d`)\n\n' \
                        % (digs,self.runs) )

        if self.have_me:
           self.write_text('# note %d echoes and registration echo index\n' \
                           % self.num_echo)
           self.write_text('set echo_list = (`count_afni -digits 2 1 %d`)\n' \
                           % self.num_echo)
           if len(self.echo_times) > 0:
              etimes = ['%s' % et for et in self.echo_times]
              self.write_text('set echo_times = ( %s )\n' % ' '.join(etimes))
           # self.regecho_var starts with '$', so strip it when assigning
           self.write_text("set %s = '%02d'\n\n" \
                            % (self.regecho_var[1:], self.reg_echo))

        self.write_text('# create results and stimuli directories\n')
        self.write_text('mkdir -p %s\nmkdir %s/stimuli\n%s\n' \
                        % (self.od_var, self.od_var, stat_inc))

        # start copying files and datasets into the results directory

        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
          oname = '-regress_stim_times_offset'
          val, err = self.user_opts.get_type_opt(float, oname)
          if err: return 1

          # if normal timing and want offset, apply it right away
          if val != None and self.have_all_stim_times(): 
            tstr = '# copy stim files into stimulus directory ' \
                   '(times offset by %g s)\n' % val
            for ind in range(len(self.stims_orig)):
              oldfile = self.stims_orig[ind]
              newfile = self.stims[ind]
              tstr += 'timing_tool.py -add_offset %g -timing %s \\\n'   \
                      '               -write_timing %s/%s\n'            \
                      % (val, oldfile, self.od_var, newfile)
              self.tlist.add(oldfile, newfile, 'stim')

          # otherwise, have either regular timing files or no offset
          else:
            tstr = '# copy stim files into stimulus directory\ncp'
            for ind in range(len(self.stims)):
                tstr += ' %s' % self.stims_orig[ind]
            tstr += ' %s/stimuli\n' % self.od_var
            self.tlist.add_many(self.stims_orig, 'stim', pre='stimuli/')
          self.write_text(add_line_wrappers(tstr))
          self.write_text("%s\n" % stat_inc)

        if len(self.extra_stims) > 0: # copy extra stim files into stim dir
            tstr = '# copy extra stim files\n'   \
                  'cp %s %s/stimuli\n' %        \
                  (' '.join(self.extra_stims_orig), self.od_var)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)
            self.tlist.add_many(self.extra_stims_orig, 'stim', pre='stimuli/')

        opt = self.user_opts.find_opt('-regress_extra_ortvec')
        if opt and len(opt.parlist) > 0:
            tstr = '# copy external ortvec files into stimuli dir\n' \
                  'cp %s %s/stimuli\n' %                             \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.tlist.add_many(opt.parlist, 'ortvec', ftype='1D')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        if self.anat:
            oanat = self.anat.nice_input()
            tstr = '# copy anatomy to results dir\n'     \
                  '3dcopy %s %s/%s\n' % (oanat, self.od_var, self.anat.prefix)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

            # further use should assume AFNI format
            self.anat.to_afni(new_view=dset_view(self.anat.nice_input()))

            # save original anat, since self.anat will change
            self.anat_orig = self.anat.new()

            # track with original format, possibly changing to AFNI
            self.tlist.add(oanat, self.anat.shortinput(), 'anat', ftype='dset')
            self.tlrcanat.to_afni()
            self.anat_final = self.anat

        # possibly copy template into results directory
        # (todo: add option to NOT copy template)
        docopy, rv = self.user_opts.get_string_opt('-tlrc_copy_base',
                                                   default='yes')
        if self.tlrc_base and docopy == 'yes':
            tmp_orig = self.tlrc_base.nice_input(head=1)
            tmp_local = self.tlrc_base.shortinput(head=1)
            tstr = '# copy template to results dir (for QC)\n' \
                  '3dcopy %s %s/%s\n' % (tmp_orig, self.od_var, tmp_local)
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

            # note: making a local copy should not affect other processing
            #       (e.g. still might count sub-bricks, so must exist now),
            #       so do not update to self.tlrc_base to a local version

        # possibly copy over any volreg base
        if self.vr_ext_base != None:
            tstr = "# copy over the external volreg base\n"  \
                  "3dbucket -prefix %s/%s '%s'\n" %         \
                  (self.od_var, self.vr_ext_pre, self.vr_ext_base)
            self.tlist.add(self.vr_ext_base, self.vr_ext_pre, 'volreg_base',
                           ftype='dset')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        # possibly copy over any volreg warp master
        if self.vr_warp_mast != None:
            tstr = "# copy volreg warp master dset as %s\n"  \
                  "3dbucket -prefix %s/%s '%s'\n" %          \
                  (self.vr_warp_mast.prefix, self.od_var,
                   self.vr_warp_mast.prefix, self.vr_wmast_in)
            self.tlist.add(self.vr_wmast_in, self.vr_warp_mast.prefix,
                                   'warp_master', ftype='dset')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        # possibly copy over any align EPI base
        if self.align_ebase != None:
            tstr = "# copy over the external align_epi_anat.py EPI volume\n" \
                  "3dbucket -prefix %s/%s '%s'\n" %         \
                  (self.od_var, self.align_epre, self.align_ebase)
            self.tlist.add(self.align_ebase, self.align_epre,
                           'epi_align_base', ftype='dset')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        opt = self.user_opts.find_opt('-regress_motion_file')
        if opt and len(opt.parlist) > 0:
            tstr = '# copy external motion file into results dir\n' \
                  'cp %s %s\n' %                                   \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.tlist.add_many(opt.parlist, 'motion', ftype='1D')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        opt = self.user_opts.find_opt('-regress_censor_extern')
        if opt and len(opt.parlist) > 0:
            fname = opt.parlist[0]
            tstr = '# copy external censor file into results dir\n' \
                  'cp %s %s\n' % (fname,self.od_var)
            self.tlist.add(fname, '', 'censor', ftype='1D')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)
            self.censor_file = os.path.basename(fname)
            self.censor_count += 1
            if self.verb > 1:
                print('++ copying external censor file to %s'%self.censor_file)

        # copy any -regress_ROI_* datasets; possibly convert to AFNI
        if len(self.afollowers) > 0:
           tstr = '# copy anatomical follower datasets into the results dir\n'
           for af in self.afollowers:
              tstr += '3dcopy %s %s/%s\n' % \
                   (af.aname.nice_input(), self.od_var, af.cpname.out_prefix())
              # update current name, in case we switch to AFNI format
              af.cname = af.cpname
              if af.cname.view == '': af.cname.new_view(self.view)
              self.tlist.add(af.aname.nice_input(), af.cname.shortinput(),
                             'anat_follower', ftype='dset')
           self.write_text(add_line_wrappers(tstr))
           self.write_text("%s\n" % stat_inc)

        # copy any -mask_import/ROI_import datasets as mask_import_LABEL
        tstr = ''
        oname = '-mask_import'
        for oname in ['-mask_import', '-ROI_import']:
           olist = self.user_opts.find_all_opts(oname)
           if len(olist) == 0:
              continue
           tstr += '# copy any %s datasets as %s_LABEL\n' % (oname, oname[1:])
           for opt in olist:
              # get label and dset params
              label = opt.parlist[0]
              dset  = opt.parlist[1]
              dname = gen_afni_name(dset)
              dname.locate() # in case we need to find it
              # find in ROI dict
              aname = self.get_roi_dset(label)
              if not aname:
                 print("** no %s label '%s' dataset to copy" % (oname, label))
                 return 1
              tstr += '3dcopy %s %s/%s\n' \
                      % (dname.nice_input(), self.od_var, aname.prefix)
              self.tlist.add(dset, aname.shortinput(), 'mask_import',
                             ftype='dset')
        if tstr:
           self.write_text(add_line_wrappers(tstr+'\n'))

        # ------------------------------------------------------------------
        # copy any -tlrc_NL_warped_dsets files (self.nlw_priors dsets)
        if len(self.nlw_priors) == 3:
           tstr = '# copy external -tlrc_NL_warped_dsets datasets\n'

           # copy anat, setting its file type to AFNI
           an = self.nlw_priors[0]
           tstr += '3dcopy %s %s/%s\n'%(an.nice_input(), self.od_var, an.prefix)
           anorig = an.nice_input()

           # if priors[0].type == NIFTI, convert to AFNI   9 Apr 2015
           # (priors[0] is anat in standard space)
           if self.nlw_priors[0].type == 'NIFTI':
              an = self.nlw_priors[0]
              an = gen_afni_name('%s+tlrc' % an.prefix)
              self.nlw_priors[0] = an

           self.tlist.add(anorig, an.shortinput(), 'NL_warp', ftype='dset')

           # break this up one by one, just for tlist.add ftype distinction...
           # get aff12.1D file
           an = self.nlw_priors[1]
           tstr += '3dcopy %s %s/%s\n' % \
                   (an.nice_input(), self.od_var, an.out_prefix())
           self.tlist.add(an.nice_input(), an.shortinput(), 'NL_warp',
                          ftype='1D')

           # get WARP.nii (specify tlrc view, since they stay nifti)
           an = self.nlw_priors[2]
           tstr += '3dcopy %s %s/%s\n' % \
                   (an.nice_input(), self.od_var, an.out_prefix())
           self.tlist.add(an.nice_input(), an.shortinput(),'NL_warp',
                          ftype='dset', view='+tlrc')

           self.write_text(add_line_wrappers(tstr))
           self.write_text("%s\n" % stat_inc)

        # ------------------------------------------------------------------
        # copy any -tlrc_affine_warped_dsets files (self.nlw_priors dsets)
        if len(self.nlw_priors) == 2:
           tstr = '# copy external -tlrc_affine_warped_dsets datasets\n'

           # copy anat, setting its file type to AFNI
           an = self.nlw_priors[0]
           tstr += '3dcopy %s %s/%s\n'%(an.nice_input(), self.od_var, an.prefix)
           anorig = an.nice_input()

           # if priors[0].type == NIFTI, convert to AFNI   9 Apr 2015
           # (priors[0] is anat in standard space)
           if self.nlw_priors[0].type == 'NIFTI':
              an = self.nlw_priors[0]
              an = gen_afni_name('%s+tlrc' % an.prefix)
              self.nlw_priors[0] = an

           self.tlist.add(anorig, an.shortinput(), 'aff_warp', ftype='dset')

           # get aff12.1D file
           an = self.nlw_priors[1]
           tstr += '3dcopy %s %s/%s\n' % \
                   (an.nice_input(), self.od_var, an.out_prefix())
           self.tlist.add(an.nice_input(), an.shortinput(), 'aff_warp',
                          ftype='1D')

           self.write_text(add_line_wrappers(tstr))
           self.write_text("%s\n" % stat_inc)

        # ------------------------------------------------------------------
        # copy any -blip datasets (convert to AFNI)
        #
        # input  datasets are blip_in_*
        # output datasets are blip_dset_*

        bstr = ''
        if isinstance(self.blip_in_for, afni_name):
           self.blip_dset_for = gen_afni_name('blip_forward', view=self.view)
           tstr = '# copy external -blip_forward_dset dataset\n' \
                  '3dTcat -prefix %s/%s %s\n' %                  \
                  (self.od_var, self.blip_dset_for.prefix,
                  self.blip_in_for.nice_input(sel=1))
           self.tlist.add(self.blip_in_for.nice_input(),
                self.blip_dset_for.shortinput(), 'blip_fwd', ftype='dset')
           bstr += tstr

        if isinstance(self.blip_in_rev, afni_name):
           # if copying rev but not forward, add a comment
           if self.blip_in_for == None:
              bstr += '# will extract automatic -blip_forward_dset in tcat ' \
                      'block, below\n\n'

           self.blip_dset_rev = gen_afni_name('blip_reverse', view=self.view)
           tstr = '# copy external -blip_reverse_dset dataset\n' \
                  '3dTcat -prefix %s/%s %s\n' %                  \
                  (self.od_var, self.blip_dset_rev.prefix,
                  self.blip_in_rev.nice_input(sel=1))
           bstr += tstr
           self.tlist.add(self.blip_in_rev.nice_input(),
                self.blip_dset_rev.shortinput(), 'blip_rev', ftype='dset')

        if isinstance(self.blip_in_med, afni_name):
           if self.blip_in_med.prefix == 'NONE':
              tstr = "# median dset is 'NONE', skipping...\n"
           else:
              self.blip_dset_med = gen_afni_name('blip_median_base',view=self.view)
              tstr = '# copy external blip median warped dataset\n' \
                     '3dcopy %s %s/%s\n' %                          \
                     (self.blip_in_med.nice_input(), self.od_var,
                     self.blip_dset_med.prefix)
              self.tlist.add(self.blip_in_med.nice_input(),
                   self.blip_dset_med.shortinput(), 'blip_med', ftype='dset')
           bstr += tstr

        if isinstance(self.blip_in_warp, afni_name):
           self.blip_dset_warp = gen_afni_name('distortion_warp',view=self.view)
           tstr = '# copy external blip NL warp (transformation) dataset\n' \
                  '3dcopy %s %s/%s\n' %                                     \
                  (self.blip_in_warp.nice_input(), self.od_var,
                  self.blip_dset_warp.prefix)
           self.tlist.add(self.blip_in_warp.nice_input(),
                self.blip_dset_warp.shortinput(), 'blip_warp', ftype='dset')
           bstr += tstr

        if bstr != '':
           self.write_text(add_line_wrappers(bstr))
           self.write_text("%s\n" % stat_inc)

        # ------------------------------------------------------------------

        opt = self.user_opts.find_opt('-copy_files')
        if opt and len(opt.parlist) > 0:
            tstr = '# copy extra files into results dir\n' \
                  'cp -rv %s %s\n' %                      \
                      (' '.join(quotize_list(opt.parlist,'')),self.od_var)
            self.tlist.add_many(opt.parlist, 'copy')
            self.write_text(add_line_wrappers(tstr))
            self.write_text("%s\n" % stat_inc)

        # copy ricor_regs last to possibly match 3dTcat TR removal
        if len(self.ricor_regs) > 0:
            tstr = copy_ricor_regs_str(self)
            self.tlist.add_many(self.ricor_regs, 'ricor_regs', ftype='1D')
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

        # do we write output to a BIDS derivate tree?
        blower = self.bids_deriv.lower()
        if blower != 'no': # then the user wants BIDS
           # require yes, no, or absolute path (check using lower case)
           if blower not in ['no', 'yes']:
              # any path is required to be absolute
              if not self.bids_deriv.startswith('/'):
                print("** -bids_deriv path must be absolute (start with /)")
                return 1
           if blower == 'yes': bdir = '.'
           else              : bdir = self.bids_deriv
           if bdir == '.'    : bstr = ''
           else              : bstr = ' \\\n    -deriv_dir %s' % bdir
           ss = '# --------------------------------------------------\n' \
                '# generate BIDS derivative output\n'                    \
                'map_ap_to_deriv.py -subj_dir .%s\n\n' % bstr
           self.write_text(ss)

        # at the end, if the basic review script is here, run it
        if self.epi_review:
           # maybe we will have an html sub-section
           htmlstr = ''
           if self.html_rev_style in g_html_review_styles \
                and self.html_rev_style != 'none':
              htmlstr = '\n' + self.run_html_review(istr='    ')
              # warn user if pythonic does not seem valid
              if self.html_rev_style == 'pythonic':
                 from afnipy import module_test_lib as MT
                 rv = MT.simple_import_test('matplotlib.pyplot', verb=self.verb)
                 if rv: print("** warning: -html_review_style pythonic:" \
                              " missing matplotlib library")
              # or if they could use it, and are not
              else:
                 from afnipy import module_test_lib as MT
                 rv = MT.simple_import_test('matplotlib.pyplot', verb=0)
                 if rv == 0:
                    print("+- consider use of: -html_review_style pythonic")
                
           ss = '# --------------------------------------------------\n' \
                '# if the basic subject review script is here, run it\n' \
                '# (want this to be the last text output)\n'             \
                'if ( -e %s ) then\n'                                    \
                '    ./%s |& tee %s\n'                                   \
                '%s'                                                     \
                'endif\n\n'                                              \
                % (self.ssr_basic, self.ssr_basic, self.ssr_b_out, htmlstr)

           self.write_text(ss)

        cmd_str = self.script_final_error_checks()
        if cmd_str: 
           if self.out_wfile:
              self.write_text(cmd_str)

        # move or remove pre-processing files (but not rm files)
        # (2 sets: removal and move)
        # ** this should happen after any 'review' scripts are run
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

        # finished with all data processing
        self.write_text('# return to parent directory (just in case...)\n'
                        'cd ..\n\n')

        self.write_text('echo "execution finished: `date`"\n\n')

        # and append execution command, for a record
        # let user choose style: none, compact, pretty
        style, rv = self.user_opts.get_string_opt('-command_comment_style',
                                                  default='compact')
        opt = self.user_opts.find_opt('-no_proc_command')
        if not opt and style != 'none':
            tstr = self.get_ap_command_str(style=style)
            self.write_text('\n\n' + tstr)

        if self.user_opts.find_opt('-ask_me'):
            tstr = '#\n# all applied options: '
            for opt in self.user_opts.olist:
                if opt.name == '-ask_me': continue
                tstr += opt.name+' '
                tstr += ' '.join(quotize_list(opt.parlist,''))
            tstr += '\n'
            self.write_text(add_line_wrappers(tstr))

        return 0

    def get_ap_command_str(self, args=None, valid=None, style='compact',
                           lstart='# '):
       """return a commented command string, depending on the desired style

            args    : None (use self.argv) or a list of command line arguments
            valid   : None (all_opt_names) or a list of valid option names
            style   : either 'compact' or 'pretty'
            cstart  : line start string, usually comment '# ', else ''
       """
       if style == 'none':
          return ''

       if args is None:
          args = self.argv

       if style not in ['compact', 'pretty']:
          print("** AP.get_ap_command_str: invalid style %s" % style)
       if style == 'pretty':
          # get a straight command, and prettify it
          tstr = UTIL.get_command_str(args=args, preamble=0, comment=0, wrap=0)
          # and run PT's niceify on it
          if valid is not None:
             allopts = valid
          else:
             allopts = self.valid_opts.all_opt_names()
          rv, tstr = FCS.afni_niceify_cmd_str(tstr, comment_start=lstart,
                                              list_cmd_args=allopts)
       else:
          tstr = UTIL.get_command_str(args=args)

       return tstr

    def get_ap_pythonic_cmd_str(self):
        """return a string showing the command in a python list structure
           (of the form found in lib_ap_examples.py)
           (remove any -show_pythonic_command option)
        """
        # remove any -show_pythonic_command option(s)
        args = self.argv[:]
        while '-show_pythonic_command' in args:
           oind = args.index('-show_pythonic_command')
           args.pop(oind)

        allopts = self.valid_opts.all_opt_names()
        arglist = FCS.make_big_list_from_args(args, list_cmd_args=allopts)
        
        # make an indentation list (add space for 2 quotes and a comma)
        maxlen = max([len(s[0]) for s in arglist]) + 3
        for s in arglist:
           tstr = "'%s'," % s[0]
           s[0] = '%-*s' % (maxlen, tstr)

        return '\n'.join(["[%s %s]," % (s[0], str(s[1:])) \
                         for s in arglist])

    def script_final_error_checks(self):
        """script for checking any errors that should be reported
           at the end of processing
        """
        cmd = ''

        # pre-steady state errors are checked in @ss_review_basic

        return cmd

    def run_html_review(self, istr=''):
        """run apqc_make_tcsh.py"""
        if self.html_rev_style not in g_html_review_styles: return ''
        if self.html_rev_style == 'none':                   return ''
        if self.ssr_uvars == '':                            return ''

        needed = [ 'Xvfb' ]
        missing = []
        for prog in needed:
           nfound = UTIL.num_found_in_path(prog, mtype=1)
           if nfound < 1:
              missing.append(prog)
        if len(missing) > 0:
           print("** will not run QC html program, apqc_make_tcsh.py\n" \
                 "   (missing: %s)\n" % ', '.join(missing))
           return "%s# ** missing program '%s', skipping HTML QC\n\n" \
                  % (istr, missing[0])

        # possibly pass user options
        user_opts = ' '.join(self.html_rev_opts)
        if user_opts != '':
           user_opts = '%s    %s \\\n' % (istr, user_opts)

        # [PT: Mar 13, 2023] remove '@ss_review_html' line; no longer created
        cmd = '%s# generate html ss review pages\n'                           \
              '%s# (akin to static images from running @ss_review_driver)\n'  \
              '%sapqc_make_tcsh.py -review_style %s -subj_dir . \\\n'         \
              '%s'                                                            \
              '%s    -uvar_json %s\n'                                         \
              '%sapqc_make_html.py -qc_dir QC_$subj\n\n'                      \
              % (istr, istr,
                 istr, self.html_rev_style,
                 user_opts,
                 istr, self.ssr_uvars,
                 istr)

        cmd = add_line_wrappers(cmd)

        if self.out_dir:
           ocmd = 'afni_open -b %s/QC_$subj/index.html' % self.out_dir
        else:
           ocmd = 'afni_open -b QC_$subj/index.html'

        cmd += '%secho "\\nconsider running: \\n"\n' \
               '%secho "   %s"\n'                    \
               '%secho ""\n' % (istr, istr, ocmd, istr)

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
               print("error: script file '%s' already exists" % self.script)
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
            print("exiting...") 
            return 1

        try: self.fp = open(self.script,'w')
        except:
            print("cannot open script file '%s' for writing" % self.script)
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
            try: code = eval('0o755')
            except: code = eval('0755')
            try:
                os.chmod(self.script, code)
            except:
                print("** failed: chmod %s %s" % (code, self.script))

    def prev_lab(self, block):
       if block.index <= 0:
          print('** asking for prev lab on block %s (ind %d)' \
                % (block.label, block.index))
       return self.block_index_label(block.index-1)

    def block_index_label(self, index):
       if index < 0 or index >= len(self.pblabels):
          print('** invalid index for block label: %d' % index)
          return 'NO_LABEL'
       return self.pblabels[index]

    def have_all_stim_times(self):
        """return whether all stim files are with regular timing"""

        # must have timing files
        sopt = self.user_opts.find_opt('-regress_stim_times')
        if not sopt: return 0
        if len(sopt.parlist) < 1: return 0

        # must not have regular stim files
        if self.user_opts.find_opt('-regress_stim_files'): return 0

        # must not have 'file' type
        stypes, rv = self.user_opts.get_string_list('-regress_stim_types')
        if rv: return 0
        if stypes != None:
           if 'file' in stypes: return 0

        # okay, I think that is basically everything
        return 1

    def have_some_stim_times(self):
        """return whether there exist stim files are with regular timing"""

        # must have timing files
        sopt = self.user_opts.find_opt('-regress_stim_times')
        if not sopt: return 0
        if len(sopt.parlist) < 1: return 0

        # must have something besides 'file' type
        stypes, rv = self.user_opts.get_string_list('-regress_stim_types')
        if rv: return 0
        for stype in stypes:
           if stype != 'file': return 1

        # did not find anything
        return 0

    def looks_like_qwarp_name(self, fname, warn=0):
        """Start gently, but possibly get more complicated.
           Determine, maybe just by the name, whether this seems to be a
           warp dataset, generated by 3dQwarp.
        """
        if fname.endswith('_WARP.nii') or fname.endswith('_WARP.nii.gz'):
           return 1
        if warn:
           print("** dataset '%s' is used as non-linear warp," \
                 "   but does not appear to be one" % fname)
           print("   (expecting @SSw name suffix _WARP.nii[.gz]")
        return 0

    def anat_follower(self, name='', aname=None, dgrid='epi', label='',
                      NN=0, num_pc=0, mave=0):
        """provide either a name/prefix or an afni_name,
           dgrid : 'anat','epi','orig', the destination grid
           label : applied for any regression ort
           NN    : 0/1, should apply NN interpolation
           num_pc: if > 0, use 3dpc to generate regressors
           mave  : if set, use 3dmaskave
        """

        # force an aname element
        if aname == None:
           if name == '':
              print('** new_anat_follower requires name or aname')
              return None 
           aname = gen_afni_name(name)

        if label:
           lname = gen_afni_name(label)
           if lname.exist():
              print("** ERROR: anat_follower label exists as dataset: '%s'" \
                    % label)
              return None

        dgridtypes = ['epi', 'anat', 'self']
        if dgrid not in dgridtypes:
           print("** error: invalid dgrid '%s' for %s" \
                 % (dgrid, aname.rel_input()))
           print('   (must be one of: %s)' % ', '.join(dgridtypes))
           return None

        vo = VO.VarsObject()

        # set_var is not needed, but is a reminder
        vo.set_var('aname',  aname)
        vo.set_var('cname',  aname)     # cname is current name
        if label: cppre = 'copy_af_%s' % label
        else:     cppre = aname.prefix
        vo.set_var('cpname', gen_afni_name(cppre))
        vo.set_var('dgrid',  dgrid)
        vo.set_var('label',  label)
        vo.set_var('erode',  0)         # 0=no, 1=pre, 2=post
        vo.set_var('NN',     NN)
        vo.set_var('num_pc', num_pc)
        vo.set_var('mave',   mave)
        vo.set_var('final_prefix', '')
        vo.set_var('is_warped', 0)      # has it been warped?

        # anything that needs shell input

        vo.set_var('exists', aname.exist()) # does the dataset exist?

        # is it an atlas or label table?  (3dinfo -is_atlas_or_labeltable)
        iopt = 'is_atlas_or_labeltable'
        is_alt = UTIL.get_3dinfo_val(aname.nice_input(), iopt, int, verb=0)
        vo.set_var('is_alt', is_alt)

        return vo

    # ======================================================================
    # notes regarding anat followers:
    #    - assume they start out in register with anatomy
    #    - apply any warps that are applied to anat
    #    - erode? global PC option? per ROI?
    # *     - IF ERODE (pre-erode), use 3dmask_tool instead of 3dcopy?
    #          - ponder post-erode separately?
    #
    # warp cases (3 in volreg, 3 in align, 1 in postdata):
    #    1. volreg: affine (align and/or tlrc) (standard case)
    #    2. volreg: NL     (align and/or tlrc)
    #    3. volreg: none   (no tlrc, no a2e (either no align or e2a))
    #
    #    4. align (no vr): affine
    #    5. align (no vr): NL
    #    6. align (no vr): none:  e2a -> identity warp
    #
    #    7. postdata (no vr, no align): just resample
    #
    # warp options:
    #    - erode (before or after warps)
    #       -regress_ROI_PC_pre_erode Label1 Label2 ...
    #          - process in postdata block
    #       -regress_ROI_PC_post_erode Label1 Label2 ...
    #          - process in warp_anat_followers()
    #    - dgrid (warp to grid: anat, epi, self)
    #    - NN (interpolation: NN or default=wsinc5)
    #
    # test (esp, check anat/ROI align before and after):
    #    - no anat included, else always include anat for reference
    #    - all 7 warp cases
    #    - erode option (require -regress_ROI_PC_erode LABEL1 LABEL2 ...)
    #       - let LABEL = ALL be a special option
    #       - apply in post block, where 3dresample/3dmask_calc would be done
    # 
    # warps:
    #    volreg dowarp:   -tlrc_anat or -copy_anat w/tlrc
    #           doe2a:    epi -> anat
    #           doadwarp: adwarp on anat
    #       a. anat -> EPI
    #       b. anat -> EPI and tlrc
    #       c. e2a and tlrc
    #       d. tlrc (adwarp or follow)
    #
    # PC option:   -regress_ROI_PC LABEL NUM_PCs
    #
    # checks:
    #  - labels must be unique (and cannot match those in -regress_ROI)
    #
    # ROI overview:
    #  - masks can come from -mask_segment_anat, 'full'? (so EPI),
    #    or -anat_follower_ROI XXXX epi XXXX
    #    (or -mask_import or -ROI_import, but these are not followers)
    #  - regression can come from -regress_ROI or -regress_ROI_PC
    #    or -regress_anaticor[_fast], maybe with _label
    #
    # steps:
    #  - 3dcopy into results directory (3dmask_tool if pre-erode?)
    #  * any anat follower being warped to the epi grid gets its label
    #    added to proc.roi_dict
    #  - if volreg block, warp them there
    #    else if align block, do it there
    #    else resample?
    #     - if no volreg, dest grid is via run1 dset from prior block
    #        - proc.volreg_prefix would not be set, so get prior prefix
    #          from proc.prefix_form_run(prior block)
    #  - if maskave, just add label to proc.roi_dict
    #  - generate orts (follow db_cmd_regress_ROI())
    #     - follow db_cmd_regress_ROI():
    #     - detrend volreg data at 3dD polort, per run
    #         3dDetrend -polort 2 -prefix t.3.det pb02.FT.ospace.r03.volreg
    #     - catenate runs
    #         3dTcat -prefix rm.vr.det.all.runs t.?.det*.HEAD
    #     - create censored time series via $keep_trs (file)
    #         keep = 1d_tool.py -infile $mfile -show_trs_uncensored encoded
    #         3dpc -mask $mset -pcsave 3 -prefix P3_ det.all.runs+orig"[$keep]"
    #
    #     - use 3dpc or 3dmaskave to extract time series
    #     - uncensor: 1d_tool.py -censor_fill_parent
    #
    # to do:
    #  - basic
    #  - 3dROIstats?
    #  - use -stim_file instead of -ortvec to get betas and stats
    #     - start by extracting each column as file under stimuli/
    #     - then it would be less special to insert
    # ======================================================================


    # track datasets that should follow anat transformations
    # (e.g. anat w/skull or ROIs)
    # anat follows if: -copy_anat, -anat_has_skull, align2epi OR tlrc
    def add_anat_follower(self, name='', aname=None, dgrid='epi', label='',
                          NN=0, num_pc=0, mave=0, check=1):
        """add object only if shortinput does not match any existing one 

           check: check for existence

           return follower object, even if already existing
        """

        if aname == None and name == '':
           print('** new_anat_follower requires name or aname')
           return None

        if aname == None: aname = gen_afni_name(name)
        si = aname.shortinput()

        # warn user if dupe is seen (and on the same grid)
        for af in self.afollowers:
           if af.aname.shortinput() == si and af.dgrid == dgrid:
              print('** warning: have duplicate anat follower: %s' % si)

        # not yet in list
        af = self.anat_follower(aname=aname, dgrid=dgrid, label=label,
                                NN=NN, num_pc=num_pc, mave=mave)
        self.afollowers.append(af)

        # warn if it does not seem to exist
        if check and not aname.exist():
           print('** WARNING: anat follower does not seem to exist: %s' \
                 % aname.rel_input())
           print('   originally from %s' % aname.initname)
           if self.verb > 1: aname.show()

        return af

    def get_anat_follower(self, label):
       for af in self.afollowers:
          if af.label == label: return af
       return None

    def add_roi_dict_key(self, key, aname=None, overwrite=0):
       """set roi_dict[key], but check for existence
          return non-zero on error
       """
       if isinstance(aname, afni_name): newname = aname.shortinput()
       elif aname: newname = aname
       else: newname = 'NOT_YET_SET'

       if key in self.roi_dict:
          oname = self.roi_dict[key]
          if isinstance(oname, afni_name): oldname = oname.shortinput()
          else: oldname = 'NOT_YET_SET'

          if not overwrite:
             print("** failing to overwrite roi_dict['%s'] = %s with %s" \
                   % (key, oldname, newname))
             if key in self.def_roi_keys: 
                print("** ROI key '%s' in default list, consider renaming"%key)
                print("   (default list comes from 3dSeg result)")
             return 1

          if self.verb > 1:
             print("++ will overwrite roi_dict['%s'] = %s\n" \
                   "   with %s"                              \
                   % (key, oldname, newname))

       elif self.verb > 1:
            print("++ setting roi_dict['%s'] = %s" % (key, newname))

       self.roi_dict[key] = aname

       return 0

    def show_roi_dict_keys(self, verb=0):
       keys = list(self.roi_dict.keys())
       nkeys = len(keys)
       if nkeys <= 0: return
       print('-- have %d ROI dict entries ...' % nkeys)
       if verb <= 0: return

       # get max key string length, with 2 positions for surrounding quotes
       maxlen = max((len(key)+2) for key in keys)

       for key in keys:
          kstr = "'%s'" % key
          mesg = 'ROI key %-*s' % (maxlen, kstr)
          aname = self.roi_dict[key]
          if verb:
             if isinstance(aname, afni_name):
                if verb > 1: aname.show(mesg=mesg, verb=verb-1)
                else:        print("   %s : %s" % (mesg, aname.shortinput()))
             else:
                print("   %s : %s" % (mesg, aname))
       if verb>1: print()

    def get_roi_dset(self, label):
       """check roi_dict and afollowers list for label"""

       if label in self.roi_dict:
          return self.roi_dict[label]

       af = self.get_anat_follower(label)
       if af: return af.cname

       return None

    def have_roi_label(self, label):
       """check roi_dict and afollowers list for label"""

       if label in self.roi_dict: return 1

       af = self.get_anat_follower(label)
       if not af: return 0

       # have a follower, require EPI grid 
       if af.dgrid == 'epi': return 1

       return 0

    # ----------------------------------------------------------------------
    # APExample functions, based on EGS
    def egs(self, keys_rm=[]):
        """return imported EGS library, so it is hidden if not used"""
        if self.EGS == None:
           from afnipy import lib_ap_examples as EGS
           self.EGS = EGS
           self.EGS.populate_examples(keys_rm=keys_rm)
        return self.EGS

    def show_example(self, ename, verb=1):
        EGS = self.egs()
        eg = EGS.find_eg(ename)
        if not eg:
           print("** no (unique?) example found for name '%s'" % ename)
           print("   consider: afni_proc.py -show_example_names")
           return
        eg.display(verb=verb, sphinx=0)
        
    def show_example_keywords(self, verb=1):
        EGS = self.egs()
        EGS.show_example_keywords(['ALL'], verb=verb)

    def show_example_names(self, verb=2):
        EGS = self.egs()
        EGS.show_enames(verb=verb)
        
    def show_tedana_files(self):
        tedlist = TED.g_m_tedana_files
        # see how long the names are
        max0 = max([len(p[0]) for p in tedlist])
        max1 = max([len(p[1]) for p in tedlist])

        # header
        print()
        print("   %-*s   %-*s" % (max0, "orig", max1, "bids"))
        print("   %-*s   %-*s" % (max0, "----", max1, "----"))

        for pair in tedlist:
           print("   %-*s   %-*s" % (max0, pair[0], max1, pair[1]))
        print()
        
    def show_merged_command(self, target, verb=1):
        """merge opt example with self.user_opts and display pretty command
        """
        newlist = self.merge_opts(self.user_opts.olist, target)
        if newlist is None:
           return 1

        # create a new argument list to override self.args
        args = [self.argv[0]]
        for opt in newlist:
            # skip anything we don't want to include
            if opt[0] in ['-show_merged_opts']:
               continue
            args.append(opt[0])
            args.extend(opt[1])

        # start with valid opts, and additionally prepend '-CHECK' to them
        valid_opts = self.valid_opts.all_opt_names()
        valid_opts.extend([('-CHECK'+oname) for oname in valid_opts])

        tstr = self.get_ap_command_str(args=args, valid=valid_opts,
                                       style='pretty', lstart='')
        print(tstr)
        return 0

    def merge_opts(self, olist, comp, check_skip=1):
        """compare given option list (list of BASE.comopt elements)
           vs. comp (some APExample name)

           olist        : option list to merge into
           comp         : target to merge
           check_skip   : flag: add -CHECK to data options

           - remove any -compare* options
        """
        EGS = self.egs()
        olist = [ [opt.name, opt.parlist]
                  for opt in olist if not opt.name.startswith('-compare')]
        eg = EGS.APExample('command', olist)

        # if checking skip options, pass them, else empty
        if check_skip:
           eskip = g_eg_skip_opts
        else:
           eskip = []

        # can pass a noelemnt list, of opts not to compare elements of
        if eg.merge_w_instance(comp, eskip=eskip, verb=self.verb):
           return None

        return eg.olist

    def compare_merged_opts(self, olist, target, verb=1):
        """merge opt example with self.user_opts, then compare with
           that original target
        """
        # get rid of -compare in option list, and merge
        olist = [o for o in olist if not o.name.startswith('-compare')]
        newlist = self.merge_opts(olist, target, check_skip=0)

        # but we need a comopt list, so use an OptionList
        newolist = OL.OptionList('merged')
        for newo in newlist:
           newolist.add_opt(newo[0], 1, newo[1], setpar=1)

        self.compare_vs_opts(newolist.olist, target)

        return 0

    def compare_vs_opts(self, olist, comp):
        """compare given option list (list of BASE.comopt elements)
           vs. comp (some APExample name)

           - remove any -compare* options
        """
        EGS = self.egs()
        # replace olist, removing any compare opts
        olist = [ [opt.name, opt.parlist]
                  for opt in olist if not opt.name.startswith('-compare')]
        eg = EGS.APExample('command', olist)
        # can pass a noelemnt list, of opts not to compare elements of
        eg.compare(comp, eskip=g_eg_skip_opts, verb=self.verb)

    def compare_opts_vs_opts(self, olist, sep_ind):
        """compare first part of option list vs. second part,
           split around sep_ind (separation index)
           - ignore -compare* options
        """
        EGS = self.egs()
        # get first sep_ind opts, removing all -compare opts
        olist1 = []
        for oind in range(sep_ind):
           opt = olist[oind]
           if opt.name.startswith('-compare'): continue
           olist1.append([opt.name, opt.parlist])

        # get second set of opts, after sep_ind
        olist2 = []
        for oind in range(sep_ind, len(olist)):
           opt = olist[oind]
           if opt.name.startswith('-compare'): continue
           if opt.name == 'afni_proc.py': continue
           olist2.append([opt.name, opt.parlist])

        # create instances and compare them
        eg1 = EGS.APExample('command_1', olist1)
        eg2 = EGS.APExample('command_2', olist2)
        eg1.compare(eg2, eskip=g_eg_skip_opts, verb=self.verb)

    def compare_example_pair(self, pair):
        """compare given the given pair of known examples
        """
        EGS = self.egs()
        if len(pair) != 2:
           print("** compare_example_pair: needs to example names")
        EGS.compare_eg_pair(pair[0], pair[1],
                            eskip=g_eg_skip_opts, verb=self.verb)

    # ----------------------------------------------------------------------
    # PPI regression script functions
    def want_ppi_reg_scripts(self):
        if self.user_opts.find_opt('-write_ppi_3dD_scripts'):
           return 1
        return 0

    def do_nocensor(self):
        """if censoring, make regression script without censoring
           adjust self.script_3dD and self.prefix_3dD
        """

        # do not modify censor options, so filenames are as expected,
        # just set flag to clear actual censor operation in regress block
        self.skip_censor = 1

        # do no make main script
        self.make_main_script = 0

        if self.script_3dD:
           self.script_3dD += '.0.nocensor'
        else:
           self.script_3dD = 'ppi_3dD.%s.0.nocensor' % self.subj_id

        if self.prefix_3dD: self.prefix_3dD += '0.nocensor.'
        else:               self.prefix_3dD =  'ppi.0.nocensor.'

        return 0

    def ppi_add_regs(self):
        """append PPI regressors and labels to extras and labels
           (self.test_stims is cleared via script_3dD)
        """

        errs = 0

        # get files and labels

        oname = '-regress_ppi_stim_files'
        pregs, rv = self.user_opts.get_string_list(oname)
        if pregs == None or len(pregs) < 1:
           print('** missing %s list' % oname)
           errs += 1
        
        oname = '-regress_ppi_stim_labels'
        plabs, rv = self.user_opts.get_string_list(oname)
        if plabs == None or len(plabs) < 1:
           print('** missing %s list' % oname)
           errs += 1

        if errs: return 1

        # append to extras

        oname = '-regress_extra_stim_files'
        self.user_opts.append_to_opt(oname, pregs)

        oname = '-regress_extra_stim_labels'
        self.user_opts.append_to_opt(oname, plabs)

        # make 3dD scripts
        self.make_main_script = 0

        if self.script_3dD: self.script_3dD += '.1.ppi'
        else:               self.script_3dD =  'ppi_3dD.%s.1.ppi'%self.subj_id

        if self.prefix_3dD: self.prefix_3dD += '1.ppi.'
        else:               self.prefix_3dD =  'ppi.1.ppi.'

        return 0

    # given a block, run, return a prefix of the form: pNN.SUBJ.rMM.BLABEL
    #    NN = block index, SUBJ = subj label, MM = run, BLABEL = block label
    # if surf_names: pbNN.SUBJ.rMM.BLABEL.HEMI.niml.dset
    # if echo index eind  >  0,  use ...SUBJ.eEE.rRR...
    #               eind == -1,  use .e$fave_echo
    #               eind == -2,  use .e*          (wildcard)
    #               eind == -9,  use ''           (nothing)
    #    (else)     eind ==  0,  use .e$eind
    # (pass as 0/1, -1 for default)
    def prefix_form(self, block, run, view=0, surf_names=-1, eind=0, use_me=0):
        if self.runs > 99: rstr = 'r%03d' % run
        else:              rstr = 'r%02d' % run

        # maybe we have an echo index
        sstr = '' # if going wild (and not surf), need .HEAD suffix
        estr = ''
        if use_me or self.use_me:
           if eind > 0:     estr = '%se%02d' % (self.sep_char, eind)
           elif eind == -1: estr = '%se%s' % (self.sep_char, self.regecho_var)
           elif eind == -2:
                            sstr = '.HEAD'
                            estr = '%se*'  % (self.sep_char)
           elif eind == -9: estr = ''
           else:            estr = '%se%s' % (self.sep_char, self.echo_var)

        if view: vstr = '%s%s' % (self.view, sstr)
        else:    vstr = ''

        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
           sstr = '' # force no suffix
        else: hstr = ''

        s = self.sep_char
        return 'pb%02d%s%s%s%s%s%s%s%s%s' %    \
               (block.index, s, self.subj_label, hstr, s, rstr, estr, s,
                block.label, vstr)

    # same, but leave run as a variable
    def prefix_form_run(self, block, view=0, surf_names=-1, eind=0, use_me=0):

        # maybe we have an echo index
        sstr = '' # if going wild (and not surf), need .HEAD suffix
        estr = ''
        if use_me or self.use_me:
           if eind > 0:     estr = '%se%02d' % (self.sep_char, eind)
           elif eind == -1: estr = '%se%s' % (self.sep_char, self.regecho_var)
           elif eind == -2:
                            sstr = '.HEAD'
                            estr = '%se*'  % (self.sep_char)
           elif eind == -9: estr = ''
           else:            estr = '%se%s' % (self.sep_char, self.echo_var)

        if view: vstr = '%s%s' % (self.view, sstr)
        else:    vstr = ''

        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
           sstr = '' # force no suffix
        else: hstr = ''
        if self.sep_char == '.': rvstr = '$run'
        else:                    rvstr = '${run}'
        s = self.sep_char
        return 'pb%02d%s%s%s%sr%s%s%s%s%s' %    \
            (block.index, s, self.subj_label, hstr, s, rvstr, estr, s,
             block.label, vstr)

    # same as prefix_form, but use previous block values (index and label)
    # (so we don't need the block)
    # if self.surf_names: pbNN.SUBJ.rMM.BLABEL.HEMI.niml.dset
    def prev_prefix_form(self, run, block, view=0, surf_names=-1,
                               eind=0, use_me=0):
        if self.runs > 99: rstr = 'r%03d' % run
        else:              rstr = 'r%02d' % run

        # maybe we have an echo index
        sstr = '' # if going wild (and not surf), need .HEAD suffix
        estr = ''
        if use_me or self.use_me:
           if eind > 0:     estr = '%se%02d' % (self.sep_char, eind)
           elif eind == -1: estr = '%se%s' % (self.sep_char, self.regecho_var)
           elif eind == -2:
                            sstr = '.HEAD'
                            estr = '%se*'  % (self.sep_char)
           elif eind == -9: estr = ''
           else:            estr = '%se%s' % (self.sep_char, self.echo_var)

        if view: vstr = '%s%s' % (self.view, sstr)
        else:    vstr = ''

        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
           sstr = '' # force no suffix
        else: hstr = ''
        s = self.sep_char
        return 'pb%02d%s%s%s%s%s%s%s%s%s' %    \
               (block.index-1, s, self.subj_label, hstr, s, rstr, estr, s,
               self.prev_lab(block), vstr)

    # same, but leave run as a variable
    def prev_prefix_form_run(self, block, view=0, surf_names=-1,
                                   eind=0, use_me=0):
        # maybe we have an echo index
        sstr = '' # if going wild (and not surf), need .HEAD suffix
        estr = ''
        if use_me or self.use_me:
           if eind > 0:     estr = '%se%02d' % (self.sep_char, eind)
           elif eind == -1: estr = '%se%s' % (self.sep_char, self.regecho_var)
           elif eind == -2:
                            sstr = '.HEAD'
                            estr = '%se*'  % (self.sep_char)
           elif eind == -9: estr = ''
           else:            estr = '%se%s' % (self.sep_char, self.echo_var)

        if view: vstr = '%s%s' % (self.view, sstr)
        else:    vstr = ''

        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
           sstr = '' # force no suffix
        else: hstr = ''
        if self.sep_char == '.': rvstr = '$run'
        else:                    rvstr = '${run}'
        s = self.sep_char
        return 'pb%02d%s%s%s%sr%s%s%s%s%s' %    \
               (block.index-1, s, self.subj_label, hstr, s, rvstr, estr, s,
               self.prev_lab(block), vstr)

    # same, but leave run wild
    def prev_dset_form_wild(self, block, view=0, surf_names=-1,
                                  eind=0, use_me=0):
        # maybe we have an echo index
        estr = ''
        if use_me or self.use_me:
           if eind > 0:     estr = '%se%02d' % (self.sep_char, eind)
           elif eind == -1: estr = '%se%s' % (self.sep_char, self.regecho_var)
           elif eind == -2: estr = '%se*'  % (self.sep_char)
           elif eind == -9: estr = ''
           else:            estr = '%se%s' % (self.sep_char, self.echo_var)

        # if surface, change view to hemisphere and dataset suffix
        if surf_names == -1: surf_names = self.surf_names
        if surf_names:
           vstr = '.niml.dset'
           hstr = '%s%s' % (self.sep_char, self.surf_svi_ref)
        else:      # view option is not really handled...
           vstr = '%s.HEAD' % self.view
           hstr = ''
        s = self.sep_char
        return 'pb%02d%s%s%s%sr*%s%s%s%s' %    \
             (block.index-1, s, self.subj_label,hstr, s, estr, s,
             self.prev_lab(block), vstr)

    # like prefix, but list the whole dset form, in wildcard format
    def dset_form_wild(self, blabel, view=None, surf_names=-1,
                             eind=0, use_me=0):
        block = self.find_block(blabel)
        if not block:
            print("** DFW: failed to find block for label '%s'" % blabel)
            return ''
        bind = block.index

        # maybe we have an echo index
        estr = ''
        if use_me or self.use_me:
           if eind > 0:     estr = '%se%02d' % (self.sep_char, eind)
           elif eind == -1: estr = '%se%s' % (self.sep_char, self.regecho_var)
           elif eind == -2: estr = '%se*'  % (self.sep_char)
           elif eind == -9: estr = ''
           else:            estr = '%se%s' % (self.sep_char, self.echo_var)

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
        return 'pb%02d%s%s%s%sr*%s%s%s%s' %      \
            (bind, s, self.subj_label, hstr, s, estr, s, blabel, vstr)

class TrackedFlist:
    """class for sorting/printing list of tracked files (VarsObject's)"""
    def __init__(self, proc, subj_id='Steve'):
       self.tfiles = []
       self.subj_id = subj_id

       self.proc = proc
       self.verb = proc.verb

    def add(self, oldname, newname, desc, ftype='unknown', pre='', view=''):
       """to track external files that are copied in, e.g.,
             oldname, newname, desc, ftype
               - desc might be like 'epi', 'anat', 'epi base', 'anat follower'
               - ftype should be in {'dset', '1D', 'text', 'unknown'}
             if newname == '', use trail of oldname
             if pre, apply to newname
             if view and dset, override any dset view
       """
       vo = VO.VarsObject()

       # make sure ftype is valid
       if ftype not in ['dset', '1D', 'text', 'unknown']:
          print("** TrackedFile: illegal ftype %s for infle %s" \
                % (ftype, oldname))
          ftype = 'unknown'

       # possibly init newname to old without path
       if newname == '':
          newname = os.path.basename(oldname)
       # possibly add any output prefix
       if pre: newname = pre+newname

       # init with passed params
       vo.oldname  = oldname
       vo.newname  = newname
       vo.desc     = desc
       vo.ftype    = ftype
       vo.pre      = pre
       vo.view     = view

       vo.nos_oldn = ''
       vo.nos_newn = ''

       # ----- prepare to track dsets -----
       vo.in_an      = None
       vo.out_an     = None
       vo.in_view    = ''
       vo.out_view   = ''
       vo.short_in   = ''
       vo.short_out  = ''

       # make afni_name for any dset
       vo.in_an  = gen_afni_name(vo.oldname)
       vo.out_an = gen_afni_name(vo.newname)
       vo.short_in = vo.in_an.shortinput()
       vo.short_out = vo.out_an.shortinput()

       # possibly let user know of this glorious addition
       if self.verb > 2:
          if self.verb == 3: fname = vo.in_an.shortinput()
          else:              fname = vo.in_an.rel_input()
          print("-- TrackedFile %s, exists = %s" % (fname, vo.in_an.exist()))

       if vo.ftype == 'dset':
          # set and check input view
          # (run locate in case we need to search for the dset)
          vo.in_an.locate()
          vo.in_view = dset_view(vo.in_an.rel_input())
          if vo.in_view not in ['+orig', '+tlrc']:
             # do we fail?
             if not vo.in_an.exist():
                print("** cannot evaluate view of missing input: %s" \
                      % vo.in_an.rel_input())
             else:
                print("** bad view %s for input %s" \
                      % (vo.in_view, vo.in_an.rel_input()))

          # track output view, based on passed or input
          if view:
             vo.out_view = view
          elif vo.in_view in ['+orig', '+tlrc']:
             vo.out_view = vo.in_view
          else:
             vo.out_view = self.proc.view

          # and use the newly created view as a backup for the afni_name
          # if vo.in_an.type == 'BRIK' and vo.in_an.view == '':
          #    vo.in_an.view = vo.view


          # some old name tests...
          if self.verb > 0 and (vo.in_view != vo.out_view):
             print("** in/out view change: %s -> %s: %s" % \
                   (vo.in_view, vo.out_view, vo.out_an.shortinput()))

       # ----- done tracking dsets -----

       # replace any $subj/${subj} strings with actual subject ID
       nn = oldname.replace('$subj', self.subj_id)
       vo.nos_oldn = nn.replace('${subj}', self.subj_id)

       nn = newname.replace('$subj', self.subj_id)
       vo.nos_newn = nn.replace('${subj}', self.subj_id)

       self.tfiles.append(vo)

    def add_many(self, oldnames, desc, ftype='unknown', pre=''):
       """like add, but take a list and assume no directory"""
       for oname in oldnames:
          nname = os.path.basename(oname)
          self.add(oname, nname, desc, ftype=ftype, pre=pre)

    def sort(self, sublist=[], keys=['desc']):
       """either sort in place, or sort the passed list of instances"""
       if sublist: slist = sublist
       else:       slist = self.tfiles

       if len(sublist) == 0:
          return []

       for key in keys:
          if not sublist[0].valid(key):
             print("** invalid TrackFile sort key, %s" % key)
             return

       VO.g_sort_keys = keys
       sublist.sort()
       return(sublist)

    def show_vo(self, mesg=''):
       """show via lib_vars_object
       """
       if mesg: print(mesg)
       for tfile in self.tfiles:
          tfile.show()
       print()

    def show(self, order='sort', rfield='desc', rval='', dfields=[]):
       """display the list of tracked files

          order       : sort or not (so order added)
          rfield/rval : if both set, only show files where tfiles.rfield=rval
       """

       # dupe list (usually needed, and makes life easier for sorting)

       if rfield != '' and rval != '' and rval != 'ALL':
          showlist = [tf for tf in self.tfiles if tf.val(rfield)==rval]
       else:
          showlist = self.tfiles[:]

       if len(showlist) == 0:
          return

       if order == 'sort':
          showlist = self.sort(showlist)

       if len(dfields) == 0:
          dfields = ['desc', 'ftype', 'newname']
       nfields = len(dfields)

       sizelist = []
       dashlist = []
       for field in dfields:
          size = max([len(tf.val(field)) for tf in showlist])
          # include the field name
          if len(field) > size:
             size = len(field)
          sizelist.append(size)
          dashlist.append('-'*size)

       hdrlist = ['%-*s' % (sizelist[i], dfields[i]) for i in range(nfields)]

       print('%s' % '  '.join(hdrlist))
       print('%s' % '  '.join(dashlist))
       for tf in showlist:
          plist = ['%-*s' % (sizelist[i], tf.val(dfields[i])) \
                   for i in range(nfields)]
          print('%s' % '  '.join(plist))

       print()

       del(showlist)


class ProcessBlock:
    def __init__(self, label, proc):
        self.label = label      # label is block type
        self.valid = 0          # block is not yet valid
        self.index = proc.bindex
        self.verb  = proc.verb  # verbose level
        self.post_cstr  = ''    # extra commands to run at the end of the block
        self.post_funcs = []    # post cmd functions to modify post_cstr
                                # (they should pass proc and block)
        if not label in BlockModFunc: return

        self.opts = OptionList(label)                   # init option list
        BlockModFunc[label](self, proc, proc.user_opts) # add block

        if self.verb > 2:
            if self.valid: self.show('+d successful add ')
            else:          print('+d invalid ProcessBlock: %s' % label)

    def show(self, mesg):
        print('------- %sProcessBlock: %s -------' % (mesg, self.label))
        self.opts.show('new options: ')

def make_proc(do_reg_nocensor=0, do_reg_ppi=0):
    """create proc instance

       do_reg_no_censor : if set, create non-censored regress command
       do_reg_ppi       : if set, pass PPI regs as extra stim files

       return status and instance (if None, quit)
    """

    proc = SubjProcSream('subject regression')
    proc.init_opts()

    rv = proc.get_user_opts()
    if rv != None:  # 0 is a valid return
       if rv != 0:
          show_args_as_command(proc.argv, "** failed command (get_user_opts):")
       return rv, None

    # ----------------------------------------------------------------------
    # possibly adjust options before any processing

    # if requested, omit censor options (implies -write_3dD_*)
    # (if no censoring already, does nothing)
    if do_reg_nocensor:
       if proc.do_nocensor():
          return 1, None

    # possibly add PPI regressors as extra stim files (implies -write_3dD_*)
    if do_reg_ppi:
       if proc.ppi_add_regs():
          return 1, None

    # run db_mod functions, and possibly allow other mods
    if proc.create_blocks():
       show_args_as_command(proc.argv, "** failed command (create_blocks):")
       return 1, None

    # ----------------------------------------------------------------------
    # run post mod functions (since we now know about most of the inputs)
    if db_mod_post_process(proc):
       return 1, None

    # ----------------------------------------------------------------------
    # run db_cmd functions, to create the script
    rv = proc.create_script()
    if rv != None:  # terminal, but do not display command on 0
       if rv > 0:
          show_args_as_command(proc.argv, "** failed command (create_script):")
       return rv, None

    # possibly display list of tracked files
    if proc.show_tfiles or proc.verb > 2:
       # proc.tlist.show_vo()
       rfield = ''  # in verbose mode, show all files
       rval = ''
       dfields = ['desc', 'ftype', 'out_view', 'newname']

       # else, user has requested something to show
       if proc.show_tfiles:
          rfield = 'desc'
          rval = proc.show_tfiles

       proc.tlist.show(order='sort', rfield=rfield, rval=rval, dfields=dfields)
                       # dfields = ['ftype', 'short_in'])

    return 0, proc

def run_proc():

    # create proc script
    rv, proc = make_proc()
    if proc == None: return rv

    # maybe make PPI regression scripts
    if proc.want_ppi_reg_scripts():

       # possibly make nocensor script
       rv, ppi_proc  = make_proc(do_reg_nocensor=1)
       if ppi_proc == None: return rv
       del(ppi_proc)

       # make PPI regression script
       rv, ppi_proc = make_proc(do_reg_ppi=1)
       if ppi_proc == None: return rv
       del(ppi_proc)

    # finally, execute if requested (only if we made the full script)
    if proc.make_main_script and proc.user_opts.find_opt('-execute'):
       rv = os.system(proc.bash_cmd)

    return rv

# main
if __name__ == '__main__':

    rv = run_proc()
    sys.exit(rv)

