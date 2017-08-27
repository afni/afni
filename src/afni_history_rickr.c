
/** cf. afni_history.h **/

#include "afni_history.h"

/*  basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long description with intermediate newlines
  
    copy entire section: { ... } ,
  
    Notes: - months are JAN ... DEC (see .h file)

           - levels are :
                    MICRO           - users don't see
                    MINOR           - small affect on users
                    MAJOR           - larger affect on users
                    SUPER           - important changes, like new programs
                    SUPERDUPER      - we expect users to know

           - types are:
                    TYPE_GENERAL    - unspecified update type
                    TYPE_NEW_PROG   - new program
                    TYPE_NEW_OPT    - new program option
                    TYPE_NEW_ENV    - new environment variable or change
                    TYPE_BUG_FIX    - bug fix
                    TYPE_MODIFY     - a change (not new, not a fix)
                    TYPE_ENHANCE    - general improvement

           - PLEASE, stick to what fits on an 80 column terminal
           - it may be nice to put the newest entries at the top
           - leave the last "99, NULL" entry as it is

 -- examples (newest at top) --

 { 26 , FEB , 2008 , RCR , "my_program" , MAJOR , TYPE_GENERAL ,
   "short description of change" ,
   "(optional) detailed description, or where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

 { 26, MAR, 2008, RCR, "sample", MICRO, TYPE_GENERAL,
   "blah",
   NULL
 } ,

*/

afni_history_struct rickr_history[] = {

 { 30, Jan, 2019, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -volreg_post_vr_allin and -volreg_pvra_base_index",
   "These are to run 3dvolreg (or 3dAllineate) to a base within\n"
   "each run, before concatenating a transformation from each\n"
   "per-run base to the global EPI registration base."
 } ,

 { 28, Jan, 2019, RCR, "3dttest++", MICRO, TYPE_ENHANCE,
   "output volume counts for -set options",
   NULL
 } ,

 { 28, Jan, 2019, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "updates to make target, prog_list",
   "   - grep out CMakeLists.txt\n"
   "   - add some ptaylor python scripts\n"
   "   - define and add DISCO_SCRIPTS"
 } ,

 { 28, Jan, 2019, RCR, "afni-general", MINOR, TYPE_NEW_OPT,
   "add -help to @djunct_*.py",
   NULL
 } ,

 { 22, Jan, 2019, RCR, "@update.afni.binaries", MINOR, TYPE_NEW_OPT,
   "add -show_obsoletes[_grep] and -show_system_progs",
   "Inspired by Z Saad."
 } ,

 { 22, Jan, 2019, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_est_blur_detrend",
   "We might change the default to no detrending here."
 } ,

 { 18, Jan, 2019, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "process df_info, and hand off new uvars",
   NULL
 } ,

 { 18, Jan, 2019, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "run 1d_tool.py -show_df_info, unless -regress_show_df_info no",
   NULL
 } ,

 { 17, Jan, 2019, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "add option -show_df_info, to partition degrees of freedom in X-matrix",
   NULL
 } ,

 { 16, Jan, 2019, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "-regress_mot_as_ort now defaults to yes; use vr_base_external",
   "This should not affect results, it is prep for other evil designs."
 } ,

 { 11, Jan, 2019, RCR, "dicom_hinfo", MICRO, TYPE_NEW_OPT,
   "add -sepstr option",
   "Done for ZXu on message board."
 } ,

 {  8, Jan, 2019, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add make targets prog_list_bin and _scripts, which generate prog_list.txt",
   "This is done to partition binaries vs scripts."
 } ,

 {  8, Jan, 2019, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "add -volreg_method, and corresponding options",
   "This allows one to use 3dAllineate for EPI motion registration.\n"
   "Options -volreg_allin_cost and -volreg_allin_auto_stuff (as well as\n"
   "the old -volreg_opts_vr) can be used to control -cost and other options."
 } ,

 {  4, Jan, 2019, RCR, "NIFTI", MINOR, TYPE_ENHANCE,
   "add regression testing scripts",
   NULL
 } ,

 {  4, Jan, 2019, RCR, "nifti_tool", MINOR, TYPE_MODIFY,
   "add -mod_hdr2, specific to NIFTI-2 headers",
   NULL
 } ,

 {  3, Jan, 2019, RCR, "nifti_tool", MINOR, TYPE_MODIFY,
   "have -mod_hdr/-swap_as_nifti fail on valid NIFTI-2 header",
   "Re-allow processing of ASCII headers (via NIFTI-2).\n"
   "Also, incorporate updates from H Johnson."
 } ,

 { 26, Dec, 2018, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "incorporate more updates from the NIFTI_Imaging/nifti_clib repo",
   NULL
 } ,

 { 20, Dec, 2018, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "incorporate updates from the NIFTI_Imaging/nifti_clib repo",
   "And update Makefile.INCLUDE."
 } ,

 { 19, Dec, 2018, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "show execution syntax in both tcsh and bash",
   "Done to appease the mighty P Taylor."
 } ,

 { 14, Dec, 2018, RCR, "1d_tool.py", MINOR, TYPE_ENHANCE,
   "include mask and params in -csim_show_clustsize",
   NULL
 } ,

 { 13, Dec, 2018, RCR, "@strip.whitespace", MINOR, TYPE_NEW_PROG,
   "stored under scripts_src; guess what it does",
   NULL
 } ,

 { 13, Dec, 2018, RCR, "Makefile.INCLUDE", MICRO, TYPE_MODIFY,
   "removed nift1-test from the distribution",
   NULL
 } ,

 { 13, Dec, 2018, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "merged NIFTI updates from NIFTI-Imaging/nifti_clib repo",
   NULL
 } ,

 { 11, Dec, 2018, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "use mkdir -p; reconcile write_as_nifti and NIFTI prefix",
   "Thanks to C Smith for noting the issue."
 } ,

 {  4, Dec, 2018, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "Makefiles: working towards using libmri.so",
   "Pass MRI_SHARED, and apply with SUMA_SHARED_LOPTS in SUMA."
 } ,

 {  3, Dec, 2018, RCR, "test.afni.prog.help", MINOR, TYPE_NEW_PROG,
   "test running -help on AFNI programs",
   "This is a build tool, not for the distribution."
 } ,

 {  3, Dec, 2018, RCR, "Makefile.INCLUDE", MICRO, TYPE_MODIFY,
   "use LIBMRI_OBJ = libmri.a, for future evil",
   NULL
 } ,

 { 27, Nov, 2018, RCR, "apqc_make_tcsh.py", MICRO, TYPE_ENHANCE,
   "python3 update for chmod code",
   "Thanks to L Dowdle for noting the issue."
 } ,

 { 26, Nov, 2018, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "add opt -volreg_warp_final_interp",
   "This controls final interpolation for all non-NN warps, including\n"
   "catenated EPI transforms (affine and non-linear), final EPI,\n"
   "and anatomical followers.\n"
   "Done for \"the boss\"."
 } ,

 { 21, Nov, 2018, RCR, "3dcopy", MICRO, TYPE_ENHANCE,
   "try to append HISTORY for non-AFNI datasets",
   NULL
 } ,

 { 19, Nov, 2018, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "add opt -html_review_style and run apqc_make_html.py",
   NULL
 } ,

 { 19, Nov, 2018, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "run any review scripts before possibly [re-]moving preproc data",
   NULL
 } ,

 { 17, Nov, 2018, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add epiphany and midori as browser candidates (11/21 google-chrome)",
   NULL
 } ,

 { 14, Nov, 2018, RCR, "3dDeconvolve", MICRO, TYPE_ENHANCE,
   "update 3dREMLfit command to handle surface data",
   "Strip off the .niml.dset extension, if found in the bucket name.\n"
   "Is it better to re-append it?  I am not sure."
 } ,

 {  5, Nov, 2018, RCR, "make_random_timing.py", MICRO, TYPE_ENHANCE,
   "enhance insufficient time warnings",
   NULL
 } ,

 { 29, Oct, 2018, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "zsh: check for .zshenv",
   NULL
 } ,

 { 29, Oct, 2018, RCR, "FSread_annot", MICRO, TYPE_ENHANCE,
   "fix crash and more clearly warn if missing FSColorLUT file",
   NULL
 } ,

 { 17, Oct, 2018, RCR, "gen_ss_review_scritps.py", MICRO, TYPE_ENHANCE,
   "add uvar xmat_stim",
   NULL
 } ,

 { 17, Oct, 2018, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "pass -ss_review_dset to gen_ss_review_scripts.py",
   NULL
 } ,

 { 16, Oct, 2018, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "if no AFNI errors, skip homebrew library linking warnings",
   NULL
 } ,

 { 16, Oct, 2018, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "added new uvar fields",
   "Move g_ss_uvar_fields to lib_ss_review.py.\n"
   "Add uvar fields: nt_applied, nt_orig, ss_review_dset,\n"
   "   pre_ss_warn_dset, decon_err_dset, tent_warn_dset."
 } ,

 { 16, Oct, 2018, RCR, "@update_afni_binaries", MICRO, TYPE_ENHANCE,
   "darwin defaults to 10.12; newline before dotfile appends",
   NULL
 } ,

 { 11, Oct, 2018, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "check for consistency between python and PyQt4 ",
   NULL
 } ,

 { 11, Oct, 2018, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "have gen_ss_review_scripts.py always write out.gen_ss_uvars.json",
   "This is to help occupy Paul."
 } ,

 {  9, Oct, 2018, RCR, "make_random_timing.py", MINOR, TYPE_BUG_FIX,
   "fix decay rest with non-zero min; block unlimited decay stim dur",
   "Thanks to D Plunkett for noting the problem."
 } ,

 {  5, Oct, 2018, RCR, "timing_tool.py", MINOR, TYPE_ENHANCE,
   "directly go after expected column headers in TSV files",
   NULL
 } ,

 {  4, Oct, 2018, RCR, "@update.afni.binaries", MICRO, TYPE_BUG_FIX,
   "wget/curl may remove execute permision, so re-add it",
   NULL
 } ,

 {  1, Oct, 2018, RCR, "gifti_tool", MICRO, TYPE_MODIFY,
   "link to nifti2_io.o, rather than nifti1_io.o",
   "Also, install nifti2_io.h rather than nifti2_io.h with other headers."
 } ,

 {  1, Oct, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "mri_dicom_hdr.c: init vars in DICOM reading functions",
   NULL
 } ,

 { 27, Sep, 2018, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "added macos_10.12 examples to help",
   NULL
 } ,

 { 27, Sep, 2018, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "handle FSL timing files with fewer than 3 columns",
   "For T Radman."
 } ,

 { 25, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on -help",
   "Adding: fat_lat_csv.py, fat_proc_grad_plot."
 } ,

 { 25, Sep, 2018, RCR, "prog_list.txt", MICRO, TYPE_MODIFY,
   "update prog_list.txt from 'make prog_list'; we might remove this file",
   NULL
 } ,

 { 25, Sep, 2018, RCR, "timing_tool.py", MICRO, TYPE_BUG_FIX,
   "fix first timediff in -multi_timing_to_event_list",
   NULL
 } ,

 { 24, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on terminal options, like -help",
   "Adding: mpeg_encode, cjpeg, djpeg, fat_roi_row.py, fat_mvm_scripter.py,\n"
   "        fat_mat_sel.py, fat_mvm_prep.py, fat_mvm_review.py."
 } ,

 { 21, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "separate testGL build target from SUMA_PROGS",
   "Again, for accuracy of 'make prog_list'."
 } ,

 { 21, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "add new text_install dir; move scripts_install txt files there",
   "Moved afni_fs_aparc+aseg_*.txt and demo*.niml.do there.\n"
   "This helps with the accuracy of 'make prog_list'."
 } ,

 { 21, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on terminal options, like -help",
   "Adding: fat_mvm_gridconv.py."
 } ,

 { 20, Sep, 2018, RCR, "3dttest++", MICRO, TYPE_BUG_FIX,
   "fix copy-and-paste error for processing of voxelwise covariates",
   "Use of voxelwize covariates when only using -setA was crashing.\n"
   "Thanks to S. Kippenhan for noting the problem."
 } ,

 { 18, Sep, 2018, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "outline BIDS-like analysis directory structure",
   "Add new DIRECTORY STRUCTURE NOTE section to -help output."
 } ,

 { 18, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on terminal options, like -help",
   "Adding: FD2, Ifile, MakeColorMap, ScaleToMap, SurfMeasures, afni_run_R,\n"
   "        balloon, imcat, inspec, myget, quickspec, qhull, rbox, qdelaunay."
 } ,

 { 17, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on terminal options, like -help",
   "Adding: @auto_align, @djunct_4d_slices_to_3d_vol, @djunct_vol_3slice_select,\n"
   "        @xyz_to_ijk, column_cat, get_afni_model_PRF.\n"
   "Bigger changes to Dimon, Dimon1."
 } ,

 { 14, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on terminal options, like -help",
   "Adding: @Install_D99_macaque, @Install_NIH_Marmoset."
 } ,

 { 13, Sep, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "have programs return 0 on terminal options, like -help",
   "So far: 3dSurf2Vol, 3dVol2Surf, 3dmaxima."
 } ,

 { 12, Sep, 2018, RCR, "3dClusterize.c", MICRO, TYPE_BUG_FIX,
   "fix limit check on -idat and -ithr; disable MMAP for input",
   NULL
 } ,

 { 10, Sep, 2018, RCR, "Makefile.INCLUDE", MICRO, TYPE_BUG_FIX,
   "add (copy of) Audio directory to afni_src.tgz build target",
   NULL
 } ,

 { 22, Aug, 2018, RCR, "tedana_wrapper.py", MINOR, TYPE_MODIFY,
   "change exec_or_error() to use afni_util.py, which returns strings",
   "Thanks to J Gonzalez-Castillo for noting this python3 update."
 } ,

 { 17, Aug, 2018, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_NEW_OPT,
   "add option -write_uvars_json; add a few more user vars",
   "Add afni_ver, afni_package and final_epi_dset to uvars.\n"
   "Add 'AFNI version' and 'AFNI package' to review basic output.\n"
   "Add afni_util:write_data_as_json(), lib_vars_object:get_attribute_dict()."
 } ,

 { 16, Aug, 2018, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_NEW_OPT,
   "add -show_computed_uvars; set template",
   NULL
 } ,

 { 15, Aug, 2018, RCR, "afni_util.py", MINOR, TYPE_NEW_OPT,
   "added functions for extracting version information from dataset HISTORY",
   "E.g. get_last_history_ver_pack(), get_last_history_version().\n"
   "This file now depends on 're'."
 } ,

 { 14, Aug, 2018, RCR, "model_conv_PRF_6", MINOR, TYPE_BUG_FIX,
   "return a zero array on invalid parameters",
   "Thanks to E Silson for noting the problem."
 } ,

 { 13, Aug, 2018, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "actually apply opt -blur_opts_BIM",
   "Thanks to D Zhu for noting the problem."
 } ,

 {  7, Aug, 2018, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "if converting tedana.py results to standard space, include -space",
   "Thanks to L Dowdle for noting the problem."
 } ,

 { 24, Jul, 2018, RCR, "dcm2niix_afni", MAJOR, TYPE_ENHANCE,
   "version v1.0.20180622, including fix for enhanced DICOM Philips bvec/bval",
   "Update from C Rorden."
 } ,

 {  5, Jul, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -mask_opts_automask",
   "Done for L Atlas."
 } ,

 {  5, Jul, 2018, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "use >! for writing rm.bpass.1D, in case of noclobber",
   "Thanks for D Handwerker for noting it."
 } ,

 {  2, Jul, 2018, RCR, "Makefile", MICRO, TYPE_MODIFY,
   "modify setup for Makefile.macos_10.12_local",
   "Use RLIB_CONVERT variable to apply libXm.a just for R_io.so."
 } ,

 { 22, Jun, 2018, RCR, "dcm2niix_afni", MAJOR, TYPE_ENHANCE,
   "version v1.0.20180614, including JPEG-LS support",
   "Update from C Rorden."
 } ,

 { 21, Jun, 2018, RCR, "get_afni_model_PRF_6_BAD", MICRO, TYPE_NEW_PROG,
   "wrapper for model Conv_PRF_6_BAD",
   "See model Conv_PRF_6_BAD in model_conv_PRF_6_BAD.c."
 } ,

 { 21, Jun, 2018, RCR, "model_PRF_6_BAD", MINOR, TYPE_NEW_PROG,
   "old model_PRF_6, but with version and gauss_file udpates",
   "This is for result comparison."
 } ,

 { 19, Jun, 2018, RCR, "model_conv_PRF_6", MINOR, TYPE_BUG_FIX,
   "fix rotation term, B should be 2*B",
   "Thanks to R Le, G Usabiaga and B Wandell for suggesting a review."
 } ,

 { 18, Jun, 2018, RCR, "model_conv_PRF_6", MINOR, TYPE_ENHANCE,
   "add AFNI_MODEL_PRF_GAUSS_FILE env var, to write image of Gaussian",
   "Done for model_conv_PRF and model_conv_PRF_6, should to _DOG, too."
 } ,

 { 18, Jun, 2018, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add build targets for get_afni_model_PRF/PRF_6",
   NULL
 } ,

 { 18, Jun, 2018, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "more status 0 updates: file_tool, serial_helper",
   NULL
 } ,

 { 15, Jun, 2018, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "update some programs so that -help is valid and return 0",
   "Update 3dGrayplot 3dresample 3dretroicor @2dwarper @FSlabel2dset tokens."
 } ,

 {  8, Jun, 2018, RCR, "3ddata.h", MICRO, TYPE_BUG_FIX,
   "have DUMP_MAT44 write to stdout again (might re-do stderr later)",
   "Need to fix align_epi_anat.py for stderr, but must check shell use.\n"
   "Thanks to T Radman for noting the problem."
 } ,

 {  6, Jun, 2018, RCR, "model_conv_PRF_6", MICRO, TYPE_MODIFY,
   "be clear that AFNI_MODEL_PRF_ON_GRID is not ready for this model",
   NULL
 } ,

 { 25, May, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add option -combine_opts_tedwrap, to pass to tedana_wrapper.py",
   "This is currently for passing -tedana_is_exec, say.\n"
   "Done for M Vaziri-Pashkam."
 } ,

 { 24, May, 2018, RCR, "@extract_meica_ortvec", MINOR, TYPE_ENHANCE,
   "remove any duplicates from merged lists",
   "The 4 categories (accepted/ignored, rejected/midk-rej) might not be\n"
   "a clean partition.\n"
   "Thanks to L Dowdle for noting the problem."
 } ,

 { 23, May, 2018, RCR, "get_afni_model_PRF", MINOR, TYPE_NEW_PROG,
   "this is just a wrapper for model parameter evaluation",
   "See model Conv_PRF in model_conv_PRF.c."
 } ,

 { 23, May, 2018, RCR, "get_afni_model_PRF_6", MINOR, TYPE_NEW_PROG,
   "this is just a wrapper for model parameter evaluation",
   "See model Conv_PRF_6 in model_conv_PRF_6.c."
 } ,

 { 23, May, 2018, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "add -ushort2float",
   "This will add the option to any to3d command via -gert_create_dataset.\n"
   "Done for H Brice."
 } ,

 { 18, May, 2018, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "handle '3dttest++ -Clustsim' files, with no blur",
   NULL
 } ,

 { 17, May, 2018, RCR, "3dNLfim", MICRO, TYPE_NEW_OPT,
   "add -help_models and -load_models",
   "This is easier than: '3dNLfim -DAFNI_MODEL_HELP_ALL=Y -signal eggs'."
 } ,

 { 16, May, 2018, RCR, "plug_vol2surf", MINOR, TYPE_NEW_OPT,
   "added map_all option for the main 'use vol2surf?' plugin menu",
   "The 'map_all' option applies plugin parameters to all mappable surfaces,\n"
   "rather than applying defaults to any surface not specified as surf_A/B.\n"
   "This allows one to use normals and the various mapping functions.\n"
   "Done for D Glen."
 } ,

 { 15, May, 2018, RCR, "tedana.py", MICRO, TYPE_NEW_OPT,
   "add --seed, update for modern numpy",
   "New option --seed can be used for regression testing.\n"
   "Use integer subscripts in arrays; replace some '== None' with 'is None'."
 } ,

 { 15, May, 2018, RCR, "tedana_wrapper.py", MICRO, TYPE_MODIFY,
   "back-port for python 2.6",
   "Use local check_output(), rather than from subprocess."
 } ,

 { 14, May, 2018, RCR, "afni.c", MICRO, TYPE_BUG_FIX,
   "check at 'only if do_css' should be css, not gss",
   "Without GLOBAL_SESSION, this blocked the All_Datasets session."
 } ,

 { 14, May, 2018, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "add epi_anat as opt for -mask_apply; if tedana, suggest -blur_in_mask yes",
   NULL
 } ,

 { 14, May, 2018, RCR, "@stim_analyze_modern", MINOR, TYPE_NEW_PROG,
   "added this sample script to doc/misc_scripts",
   "This uses the advanced form of make_random_timing.py."
 } ,

 {  8, May, 2018, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "add multi-echo combine methods: OC_tedort, tedana_OC, tedana_OC_tedort",
   "With this, afni_proc.py can run tedana.py and extract projection\n"
   "components (projecting good orts from bad, making cleaner bad orts).\n"
   "   OC_tedort        : like AFNI's OC, but also regress clean orts\n"
   "   tedana_OC        : tedana.py's OC\n"
   "   tedana_OC_tedort : tedana.py's OC, and regress clean orts\n"
   "The tedort (orthogonalized tedana projection components) terms are\n"
   "applied in the regress block, still as per-run terms."
 } ,

 {  8, May, 2018, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "3dQwarp warp datasets need only be named _WARP",
   "Thanks to dowdlelt on MB for bringing this up."
 } ,

 {  7, May, 2018, RCR, "@extract_meica_ortvec", MINOR, TYPE_NEW_OPT,
   "add -ver, -meica_dir, -work_dir, init history",
   NULL
 } ,

 {  7, May, 2018, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "EPI automask (full_mask) is no longer dilated by default",
   "Also, add -show_process_changes, to report changes affecting results."
 } ,

 {  3, May, 2018, RCR, "@extract_meica_ortvec", MINOR, TYPE_NEW_PROG,
   "new program to extract MEICA projection vectors",
   "These 'rejected' terms are orthogonalized to the 'accepted' ones.\n"
   "This was written to be called by afni_proc.py."
 } ,

 { 28, Apr, 2018, RCR, "3dSurf2Vol", MINOR, TYPE_NEW_OPT,
   "add option -stop_gap and map functions nzave, nzmode, median, nzmedian",
   NULL
 } ,

 { 25, Apr, 2018, RCR, "meica.py", MINOR, TYPE_BUG_FIX,
   "deal with numpy update that fails for 'array == None', use 'is None'",
   "Thanks to dowdlele on MB for noting this and pointing to the emdupre\n"
   "update on https://github.com/ME-ICA/me-ica."
 } ,

 { 25, Apr, 2018, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 1.0",
   NULL
 } ,

 { 24, Apr, 2018, RCR, "gen_epi_review.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 0.4",
   NULL
 } ,

 { 24, Apr, 2018, RCR, "auto_warp.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 0.4",
   NULL
 } ,

 { 23,  Apr, 2018, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "moved python_scripts/*.py down 1 level, under python_scripts/afni",
   "This is preparation for setup.py and __init__.py to install AFNI's\n"
   "python files using pip, and to then load them via 'module load afni'."
 } ,

 { 18,  Apr, 2018, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "add -csim_show_clustsize and helper options to report cluster requirements",
   "Given a cluster table output by 3dClustSim, use this option to extract\n"
   "the mininum cluster size, given uncorrected and corrected p-values.\n"
   "Use -csim_pthr and -csim_alpha to specify those respective p-values."
 } ,

 { 16,  Apr, 2018, RCR, "lib_afni1D.py", MICRO, TYPE_ENHANCE,
   "Afni1D: store array of comment lines in header; add show_header()",
   NULL
 } ,

 { 12,  Apr, 2018, RCR, "3dMVM.R", MICRO, TYPE_NEW_OPT,
   "add -verb option",
   NULL
 } ,

 { 12,  Apr, 2018, RCR, "dcm2niix_afni", MAJOR, TYPE_ENHANCE,
   "version v1.0.20180403, including support for Philips enhanced DICOMs",
   "Update from C Rorden."
 } ,

 {  5, Apr, 2018, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "always apply -regress_motion_per_run",
   NULL
 } ,

 {  4, Apr, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -combine_opts_tedana, to pass opts down to tedana.py",
   NULL
 } ,

 {  4, Apr, 2018, RCR, "tedana_wrapper.py", MICRO, TYPE_ENHANCE,
   "allow for newlines in -tedana_opts; flush tedana.py command",
   NULL
 } ,

 {  4, Apr, 2018, RCR, "@update.afni.binaries", MICRO, TYPE_BUG_FIX,
   "set sysname early - forgot to commit this weeks ago...",
   NULL
 } ,

 {  3, Apr, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "initial testing run with -combine_tedana_path",
   NULL
 } ,

 { 27, Mar, 2018, RCR, "make_random_timing.py", MICRO, TYPE_MODIFY,
   "modify help for sphinx formatting",
   NULL
 } ,

 { 26, Mar, 2018, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "modify defaults: VR base = MIN_OUTLIER, EPI/anat cost func = lpc+ZZ",
   NULL
 } ,

 {  7, Mar, 2018, RCR, "afni", MICRO, TYPE_BUG_FIX,
   "add do_css check around css access for making catenated session list",
   NULL
 } ,

 {  6, Mar, 2018, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "okay, if macos and no .bash_profile, source .bashrc from .bash_profile",
   NULL
 } ,

 {  1, Mar, 2018, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "add -combine_method cases of OC_A and OC_B",
   NULL
 } ,

 {  1, Mar, 2018, RCR, "@compute_OC_weights", MINOR, TYPE_NEW_OPT,
   "add -oc_method (OC_A, OC_B)",
   "OC_B: compute T2* from full log() time series, rather than log(mean(TS))."
 } ,

 { 26, Feb, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add option -help_section, and use it to add some missing option help",
   NULL
 } ,

 { 23, Feb, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add option for running OC combine method, use '-combine_method OC'",
   "This will run the current method implemented in @compute_OC_weights.\n"
   "This is probably a bit of a test, as I expect to modify the base 'OC'\n"
   "method, and therefore add other related names."
 } ,

 { 23, Feb, 2018, RCR, "@compute_OC_weights", MINOR, TYPE_NEW_OPT,
   "add option -echo_times, for convenient use by afni_proc.py",
   NULL
 } ,

 { 22, Feb, 2018, RCR, "3dMean", MINOR, TYPE_NEW_OPT,
   "add option -weightset to compute a weighted sum",
   "This N-volume weight dataset is used to apply voxelwise weights to the N\n"
   "input datasets, one volumetric weight to each dataset.  The motivated\n"
   "example is combining single runs (at a time) of multi-echo data with the\n"
   "weights generated by @compute_OC_weights."
 } ,

 { 21, Feb, 2018, RCR, "@compute_OC_weights", MICRO, TYPE_MODIFY,
   "apply T2* < 0 as limit",
   "Should have no effect on resulting weights, but make a prettier T2* map."
 } ,

 { 16, Feb, 2018, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add option -mask_epi_anat, to apply tigher mask in place of full_mask",
   "By default, create epi_anat intersection mask."
 } ,

 { 16, Feb, 2018, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "add combine block and ability to process multi-echo data",
   "Have new options -dsets_me_echo and -dsets_me_run for input.\n"
   "Still need to implement OC and ME-ICA.\n"
   "Thanks to L Atlas and J Gonzalez-Castillo."
 } ,

 { 15, Feb, 2018, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "add showproc option to exec_tcsh_command(), to show command and text",
   NULL
 } ,

 { 15, Feb, 2018, RCR, "@compute_OC_weights", MAJOR, TYPE_NEW_PROG,
   "compute voxelwise weights for optimally combining multi-echo data",
   "The equations are based on the summer 2017 talk by J Gonzalez-Castillo."
 } ,

 { 13, Feb, 2018, RCR, "3dbucket", MICRO, TYPE_MODIFY,
   "return success of dataset write",
   NULL
 } ,

 {  1, Feb, 2018, RCR, "3dTto1D", MINOR, TYPE_NEW_OPT,
   "add methods 4095_count/frac/warn",
   "Count 4095 values, or warn if datum is short and max is 4095."
 } ,

 { 31, Jan, 2018, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "in MT2_event_list 'part', if no run events, output '* *'",
   "Done for W Tseng."
 } ,

 { 30, Jan, 2018, RCR, "make_random_timing.py", MICRO, TYPE_NEW_OPT,
   "add -help_concerns, to describe some general concerns regarding timing",
   NULL
 } ,

 { 26, Jan, 2018, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "applied various NeuroDebian patches from M Hanke and Y Halchenko",
   "Includes up_include_right, up_condition_dset_unload, up_3dNetCorrFix,\n"
   "and up-fix_inflate_compare.\n"
   "Thanks to M Hanke and Y Halchenko for the fixes."
 } ,

 { 26, Jan, 2018, RCR, "dicom_hinfo", MICRO, TYPE_BUG_FIX,
   "fix crash on no input file",
   NULL
 } ,

 { 10, Jan, 2018, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "also set PATH in .bash_profile, if it exists",
   NULL
 } ,

 {  2, Jan, 2018, RCR, "realtime_receiver.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 1.0",
   "Also, add -write_text_data for L Morris and D Huynh."
 } ,

 { 29, Dec, 2017, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 1.0",
   NULL
 } ,

 { 28, Dec, 2017, RCR, "uber_ttest.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 2.0",
   NULL
 } ,

 { 28, Dec, 2017, RCR, "gen_ss_review_table.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 1.0",
   NULL
 } ,

 { 27, Dec, 2017, RCR, "gen_group_command.py", MICRO, TYPE_ENHANCE,
   "python3 compatible as of version 1.0",
   NULL
 } ,

 { 27, Dec, 2017, RCR, "RetroTS.py", MICRO, TYPE_BUG_FIX,
   "prevent slice_order = 'Cutsom' case from wiping out passed order",
   "Thanks to J. Ho for noting the problem in the Matlab version."
 } ,

 { 22, Dec, 2017, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "add -select_runs and -mplaces",
   "For B Benson and A Harrewijn, see Example 18d."
 } ,

 { 19, Dec, 2017, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "add -help for align_unifize_epi",
   NULL
 } ,

 { 19, Dec, 2017, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "uniq_list_as_dsets: (def) no restriction to prefix; dmUBLOCK known basis",
   NULL
 } ,

 { 12, Dec, 2017, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "added 'sample analysis script' to help (in GENERAL ANALYSIS NOTE)",
   NULL
 } ,

 { 11, Dec, 2017, RCR, "3dmask_tool", MICRO, TYPE_BUG_FIX,
   "in dilate, if pad but not convert, inset == dnew, so do not delete",
   "Thanks to nwlee (AFNI message board) for noting the problem."
 } ,

 { 10, Dec, 2017, RCR, "Dimon", MICRO, TYPE_ENHANCE,
   "apply -gert_to3d_prefix for GEMS I-files",
   "Done for M Kerich."
 } ,

 {  4, Dec, 2017, RCR, "3dDeconvolve", MICRO, TYPE_BUG_FIX,
   "fix gtmax in case of global times and only 1 input dset",
   "This just lead to an inappropriate warning.\n"
   "Thanks to P Bedard for noting the problem."
 } ,

 { 30, Nov, 2017, RCR, "afni_base.py", MICRO, TYPE_BUG_FIX,
   "fix problems with relative path to root directory",
   "This affected afni_proc.py, for example, adding '/' in path names.\n"
   "Thanks to D Nielson for noting the problem."
 } ,

 { 27, Nov, 2017, RCR, "afni", MICRO, TYPE_BUG_FIX,
   "fix imseq.c: driven SAVE_MPEG offset by 1",
   NULL
 } ,

 { 27, Nov, 2017, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "warn user for python version != 2.7 (3+ in particular)",
   NULL
 } ,

 { 21, Nov, 2017, RCR, "make_random_timing.py", MINOR, TYPE_ENHANCE,
   "add options -not_first and -not_last, to block tasks at run boundaries",
   "For C Smith."
 } ,

 { 15, Nov, 2017, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "-despike_mask had been tested with wrong option name",
   "Fix submitted by D Plunkett."
 } ,

 {  9, Nov, 2017, RCR, "make_random_timing.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 3.0",
   NULL
 } ,

 {  9, Nov, 2017, RCR, "timing_tool.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 3.00",
   NULL
 } ,

 {  7, Nov, 2017, RCR, "afni_proc.py", MAJOR, TYPE_ENHANCE,
   "python3 compatible as of version 6.00",
   NULL
 } ,

 {  7, Nov, 2017, RCR, "1d_tool.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 2.00",
   NULL
 } ,

 {  7, Nov, 2017, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "python3 compatible as of version 1.00",
   NULL
 } ,

 {  6, Nov, 2017, RCR, "afni-general", MAJOR, TYPE_ENHANCE,
   "first stab at making python program p2/p3 compatible",
   "Based on 2to3 and dglen mods, and tested in p2 and p3 environments."
 } ,

 {  1, Nov, 2017, RCR, "make_random_timing.py", MAJOR, TYPE_NEW_OPT,
   "implement the decay_fixed distribution type",
   "See make_random_timing.py -help_decay_fixed for details."
 } ,

 { 23, Oct, 2017, RCR, "3dDeconvolve", MICRO, TYPE_GENERAL,
   "add warnings if TR or run length is 0.0",
   NULL
 } ,

 { 20, Sep, 2017, RCR, "3dAutoTcorrelate", MICRO, TYPE_ENHANCE,
   "add help example",
   NULL
 } ,

 { 12, Sep, 2017, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "add -multi_timing_3col_tsv and -write_multi_timing",
   "Also, add -multi_show_duration_stats for married timing files.\n"
   "This is to process 3 column TSV (tab separated value) formatted timing\n"
   "files, as might be found in the OpenFMRI data."
 } ,

 { 12, Sep, 2017, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "modify main examples to use the lpc+ZZ cost function",
   NULL
 } ,

 { 11, Sep, 2017, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "if no regress block, omit gen_ss_review_scripts.py",
   NULL
 } ,

 { 6, Sep, 2017, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "re-apply cleaner xim use, so end of run signal has full image size",
   "Short end of run was hanging afni, but was not noticed since the\n"
   "communication was immediately terminated.  Fixes multi-run use."
 } ,

 { 6, Sep, 2017, RCR, "plug_realtime", MINOR, TYPE_MODIFY,
   "allow user control of registration and plots with multi-chan/echo data",
   "This previously required channel merging or registering.\n"
   "For W Luh."
 } ,

 { 30, Aug, 2017, RCR, "model_conv_PRF", MICRO, TYPE_BUG_FIX,
   "determine NT restriction based on reorg",
   "Was limited to NT.  Applies to PRF, PRF_6, PRF_DOG\n"
   "Thanks to E Silson for noting the problem."
 } ,

 { 30, Aug, 2017, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "add -fsl_timing_files and -write_as_married",
   "This is for converting FSL timing files to AFNI format."
 } ,

 { 22, Aug, 2017, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "add -apply_end_times_as_durations and -show_duration_stats",
   "For B Benson and P Vidal-Ribas."
 } ,

 { 18, Aug, 2017, RCR, "3dTto1D", MICRO, TYPE_MODIFY,
   "modify help",
   NULL
 } ,

 { 17, Aug, 2017, RCR, "3dcalc", MICRO, TYPE_MODIFY,
   "fix typos in help for minabove, maxbelow, acfwxm",
   "Thanks to A Wong for noting the minabove and maxbelow typos."
 } ,

 { 15, Aug, 2017, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "test 3dClustSim as well, to check for OpenMP library linking",
   NULL
 } ,

 { 27, Jul, 2017, RCR, "3dNwarpApply", MICRO, TYPE_BUG_FIX,
   "fix memory alloc for warp file names",
   "Thanks to L Lebois (and others on MB) for noting the problem."
 } ,

 { 24, Jul, 2017, RCR, "nifti_tool", MINOR, TYPE_MODIFY,
   "treat ANALYZE more as NIFTI-1; apply more PRId64 macros for I/O",
   NULL
 } ,

 { 21, Jul, 2017, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "relative -bindir test needs quotes to block eval",
   NULL
 } ,

 { 19, Jul, 2017, RCR, "3dTto1D", MAJOR, TYPE_NEW_PROG,
   "time series to 1D : e.g. compute enorm, DVARS(RMS), SRMS",
   "Given time series data such as EPI or motion parameters, compute\n"
   "things like Euclidean norm and RMS (a.k.a. DVARS)."
 } ,

 { 19, Jul, 2017, RCR, "3dDeconvolve", MICRO, TYPE_BUG_FIX,
   "adjust looking for good stim times to handle global timing",
   "Max time was previously that of the longest run.  For global timing,\n"
   "accumulate across runs.\n"
   "Thanks to B Callaghan, Michael, ace327 and Jeff for noting the problem."
 } ,

 { 12, Jul, 2017, RCR, "afni_system_check.py", MICRO, TYPE_MODIFY,
   "ignore /opt/X11/lib/libXt.dylib check",
   "Undo Xt.7 and /opt/X11/lib/libXt.6.dylib checks."
 } ,

 { 11, Jul, 2017, RCR, "afni_system_check.py", MICRO, TYPE_MODIFY,
   "check if /opt/X11/lib/libXt.dylib points to Xt.6",
   "This is useless, pulling it out."
 } ,

 { 10, Jul, 2017, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "if -bindir is relative, require -no_recur",
   NULL
 } ,

 {  5, Jul, 2017, RCR, "3dAllineate", MICRO, TYPE_MODIFY,
   "make quieter: only report sub-brick messages if verbose",
   "Applied to 3dAllineate and 3dmerge."
 } ,

 { 28, Jun, 2017, RCR, "afni-general", MINOR, TYPE_GENERAL,
   "add afni_src/other_builds files for Fedora 25 (Makefile, OS_notes)",
   NULL
 } ,

 { 23, Jun, 2017, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "unalias grep; update cur_afni only when not abin",
   NULL
 } ,

 { 19, Jun, 2017, RCR, "Dimon", MICRO, TYPE_NEW_OPT,
   "add -assume_dicom_mosaic to handle Siemens 3D format",
   "Done for A. Jahn."
 } ,

 { 16, Jun, 2017, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "check for libXt.7.dylib without flat_namespace",
   NULL
 } ,

 { 16, Jun, 2017, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add Makefile.linux_centos_7_64",
   "CentOS 7 and Ubuntu 16 will be new distribution build systems."
 } ,

 { 15, Jun, 2017, RCR, "uber_subject.py", MICRO, TYPE_BUG_FIX,
   "handle empty subj_dir",
   NULL
 } ,

 { 15, Jun, 2017, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add OS_notes.linux_centos_7.txt",
   NULL
 } ,

 { 15, Jun, 2017, RCR, "@SUMA_Make_Spec_FS", MICRO, TYPE_ENHANCE,
   "remove -f from top tcsh command; fix some bad tab indenting",
   NULL
 } ,

 { 5, Jun, 2017, RCR, "RetroTS.py", MINOR, TYPE_BUG_FIX,
   "peak_finder(): fix lengths of r['t'] and r['tR']",
   "Be more cautious with ratios that are very close to integers."
 } ,

 { 5, Jun, 2017, RCR, "RetroTS.py", MINOR, TYPE_BUG_FIX,
   "merge phase_base() fix by D Nielson",
   "range() is closed in matlab but half-open in python."
 } ,

 {  2, Jun, 2017, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "add Makefile.linux_ubuntu_16_64 and OS_notes.linux_ubuntu_16_64",
   "This is for setting up an Ubuntu 16 build machine."
 } ,

 { 30,  May, 2017, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "add volreg params to enorm/outlier plot",
   NULL
 } ,

 { 30,  May, 2017, RCR, "SUMA_IsoSurface", MICRO, TYPE_GENERAL,
   "remove non-ASCII characters in paper reference; remove tabs",
   NULL
 } ,

 { 26,  May, 2017, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add initial AFNI_digest_history.txt",
   NULL
 } ,

 { 25,  May, 2017, RCR, "make_random_timing.py", MINOR, TYPE_ENHANCE,
   "can now apply -save_3dd_cmd and -make_3dd_contrasts in advanced case",
   "Advanced usage can generate 3dDeconvolve command scripts, with contrasts."
 } ,

 { 23,  May, 2017, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "only warn 'ISI error: stimuli overlap' if olap > 0.0001",
   "Overlap could come from float->ascii->float conversion."
 } ,

 { 23,  May, 2017, RCR, "suma", MICRO, TYPE_GENERAL,
   "warn on NULL glwDrawingAreaWidgetClass",
   NULL
 } ,

 { 17,  May, 2017, RCR, "RetroTS.py", MICRO, TYPE_BUG_FIX,
   "D Nielson's fix in PeakFinder.py dealing with non-integer subscripts",
   "Required by latest version of numpy."
 } ,

 { 16,  May, 2017, RCR, "meica.py", MICRO, TYPE_GENERAL,
   "cast floor/ceil functions to int when used as subscripts",
   "Required by latest version of numpy."
 } ,

 { 16,  May, 2017, RCR, "meica.py", MINOR, TYPE_ENHANCE,
   "sync with https://bitbucket.org/prantikk/me-ica/src",
   "Update from version 2.5 beta9 to 2.5 beta11."
 } ,

 { 10,  May, 2017, RCR, "model_conv_PRF_DOG", MINOR, TYPE_NEW_PROG,
   "new model: same as PRF, but Difference of Gaussians",
   "For E Silson and C Baker."
 } ,

 {  9,  May, 2017, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "if to3d_prefix is NIFTI, clear write_as_nifti",
   "Thanks to A Nugent for noting the problem."
 } ,

 {  9,  May, 2017, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "apply -offset for advanced case (remove from todo list)",
   NULL
 } ,

 {  2,  May, 2017, RCR, "GIFTI", MINOR, TYPE_BUG_FIX,
   "properly handle column major order",
   "Convert to row major order on read; can control with gifti_tool.\n"
   "Thanks to JH Lee for noting the problem."
 } ,

 {  1,  May, 2017, RCR, "@diff.files", MINOR, TYPE_NEW_OPT,
   "add option -diff_prog, to use something besides xxdiff",
   NULL
 } ,

 { 25,  Apr, 2017, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "no COL_MAJOR changes for gifti_DA_rows_cols, write_*_file",
   "Also, init gifti_globs_t struct with verb=1."
 } ,

 { 25,  Apr, 2017, RCR, "suma", MICRO, TYPE_MODIFY,
   "SUMA_input.c: 'r'ecord with oversample - use tcsh -c to delete files",
   "To be explicit about shell in system(), sending errors to /dev/null."
 } ,

 { 25,  Apr, 2017, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fix follower warps for gzipped NL-WARP datasets",
   "Thanks to C Connely for noting the problem."
 } ,

 { 25,  Apr, 2017, RCR, "dcm2niix_afni", MICRO, TYPE_ENHANCE,
   "added to source for build testing",
   NULL
 } ,

 { 17,  Apr, 2017, RCR, "1d_tool.py", MICRO, TYPE_MODIFY,
   "clarify source in -show_censored_trs (if Xmat, use header info)",
   NULL
 } ,

 { 12,  Apr, 2017, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "remove some non-ASCII chars: a couple of bad dashes and some Unicode",
   "some of the Unicode characters are upsetting sed via apsearch"
 } ,

 { 12,  Apr, 2017, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "in suggest_best_prog_option(), do not search for -help, -h_* or similar",
   "A program without -help that tests it would otherwise lead to an\n"
   "infinitely recursive system call trying to use -help to suggest an option."
 } ,

 { 11,  Apr, 2017, RCR, "@djunct_dwi_selector.bash", MICRO, TYPE_MODIFY,
   "if bad args, show usage and exit",
   "Else -help would open afni GUI and wait, hanging the build."
 } ,

 { 11,  Apr, 2017, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "add GENERAL ANALYSIS NOTE; mentioned resting state scaling as optional",
   NULL
 } ,

 {  5,  Apr, 2017, RCR, "uber_subject.py", MICRO, TYPE_BUG_FIX,
   "apply subject dir again; allow -cvar subj_dir to override default",
   NULL
 } ,

 {  5,  Apr, 2017, RCR, "3dresample", MICRO, TYPE_MODIFY,
   "show -input as the typical usage, rather than -inset",
   NULL
 } ,

 {  3,  Apr, 2017, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "remove -f from @AddEdge, @DO.examples, @DriveAfni and @DriveSuma",
   "Since OS X is neglecting to pass DYLD variables to child shells,\n"
   "we need to rely on the rc files to reset them when driving GUIs."
 } ,

 { 30,  Mar, 2017, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "allow subj_dir to affect the GUI (so no subject_results)",
   "Also, apply user command-line variables align_opts_aea and tlrc_opts_at\n"
   "(still not part of the GUI).  Requested by J Rajendra."
 } ,

 { 30,  Mar, 2017, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "run 3dclust -DAFNI_ORIENT=RAI to match SET_DICOM_XYZ coordinate order",
   "Suggested by J Rajendra."
 } ,

 { 27,  Mar, 2017, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "NL warps of all-1 volume now uses -interp cubic for speed",
   "This applies only to interpolation of the warps via 3dNwarpApply.\n"
   "Also, use abs() in lists_are_same for -import_mask."
 } ,

 { 27,  Mar, 2017, RCR, "@SUMA_Make_Spec_FS", MICRO, TYPE_GENERAL,
   "add comment about distortions for -NIFTI",
   NULL
 } ,

 { 27,  Mar, 2017, RCR, "MapIcosahedron", MINOR, TYPE_NEW_OPT,
   "add -write_dist, for writing a distortion vector dataset",
   "After running something like:\n"
   "   MapIcosahedron ... -write_dist test.dist\n"
   "to create test.dist.lh.sphere.reg.gii.txt, get summaries with:\n"
   "   1d_tool.py -collapse_cols euclidean_norm -show_mmms \\\n"
   "              -infile test.dist.lh.sphere.reg.gii.txt"
 } ,

 { 21,  Mar, 2017, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "allow for volreg-only script with MIN_OUTLIER",
   NULL
 } ,

 { 21,  Mar, 2017, RCR, "MapIcosahedron", MINOR, TYPE_BUG_FIX,
   "fix projection of surfaces with non-zero centers",
   "Center each surface around 0,0,0 instead of leaving the offset in.\n\n"
   "Many thanks go to I Dewitt for even noticing this subtle issue,\n"
   "much less diagnosing where it might be coming from.  Not easy."
 } ,

 { 21,  Mar, 2017, RCR, "@FS_roi_label", MICRO, TYPE_BUG_FIX,
   "extra quote on line: choose one of $lbls",
   NULL
 } ,

 { 21,  Mar, 2017, RCR, "@SUMA_Make_Spec_FS", MICRO, TYPE_ENHANCE,
   "add -verb to MapIcosahedron if script is in verbose mode",
   NULL
 } ,

 { 16,  Mar, 2017, RCR, "RetroTS.py", MICRO, TYPE_MODIFY,
   "change peak_finder() to read data as floats",
   NULL
 } ,

 {  9,  Mar, 2017, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "from D Warren: put exception handling around os.chmod calls",
   NULL
 } ,

 {  7,  Mar, 2017, RCR, "RetroTS.py", MINOR, TYPE_BUG_FIX,
   "from J Zosky: default to using numpy.flipud()",
   "The flipud() function did not work in the original Matlab version,\n"
   "but it does in numpy.  Use the new -legacy_transform opt for old\n"
   "(and presumably incorrect) results."
 } ,

 {  6,  Mar, 2017, RCR, "RetroTS.py", MICRO, TYPE_MODIFY,
   "change -p and -v args to be read as floats; apply min(p/n_trace)",
   NULL
 } ,

 {  6,  Mar, 2017, RCR, "AFNI.afnirc", MICRO, TYPE_BUG_FIX,
   "AFNI_COMPRESSOR can be set to GZIP, not gzip",
   NULL
 } ,

 {  3,  Mar, 2017, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
   "add optimally combined 'Opt Comb' merge method",
   "Done with V Roopchansingh."
 } ,

 {  3,  Feb, 2017, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "decay timing class now follows better distribution; new decay_old class",
   NULL
 } ,

 {  2,  Feb, 2017, RCR, "steps_mac.rst", MINOR, TYPE_MODIFY,
   "rewrite OS X install instructions to use 10.7_local and fink",
   NULL
 } ,

 {  2,  Feb, 2017, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "default mac package is now macosx_10.7_local",
   NULL
 } ,

 {  1,  Feb, 2017, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "updates for fink and homebrew",
   NULL
 } ,

 { 27, Jan, 2017, RCR, "CA_EZ_atlas.csh", MICRO, TYPE_MODIFY,
   "with -help, do not dump TT_N27 in current directory",
   "Have script fail if any arguments are given (e.g. -help).\n"
   "Fix in all bin dirs and under atlases."
 } ,

 { 26,  Jan, 2017, RCR, "Makefile.macosx_10.7_local", MINOR, TYPE_NEW_OPT,
   "this is a new Makefile to prep for exec directory dynamic linking",
   NULL
 } ,

 { 25,  Jan, 2017, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "in 10.11+, check for DYLD variables via sub-shells",
   NULL
 } ,

 { 24,  Jan, 2017, RCR, "align_epi_anat.py", MICRO, TYPE_ENHANCE,
   "trap failure from main 3dAllineate call",
   NULL
 } ,

 { 23,  Jan, 2017, RCR, "make_random_timing.py", MICRO, TYPE_BUG_FIX,
   "allow automatic use of the INSTANT timing class",
   NULL
 } ,

 { 20,  Jan, 2017, RCR, "make_random_timing.py", MAJOR, TYPE_ENHANCE,
   "advanced usage, program version 2.00",
   "Essentially a new program.  The user may now define timing classes for\n"
   "stimulus and rest event types.\n"
   "   see: make_random_timing.py -help_advanced\n"
   "Motivated by K Kircanski and A Stringaris."
 } ,

 { 19,  Jan, 2017, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "fix for -final_anat",
   "Thanks to N Anderson for noting the problem."
 } ,

 { 19,  Jan, 2017, RCR, "make_random_timing.py", MICRO, TYPE_ENHANCE,
   "advanced version 2 mostly ready",
   "Have -max_consec and -ordered_stimuli implemented in new version."
 } ,

 { 18,  Jan, 2017, RCR, "3dmask_tool", MICRO, TYPE_MODIFY,
   "change example 3 to be with EPI masks",
   NULL
 } ,

 { 12,  Jan, 2017, RCR, "afni_system_check.py", MICRO, TYPE_MODIFY,
   "for 10.11+, make flat warning and summary comment consistent",
   NULL
 } ,

 { 11,  Jan, 2017, RCR, "make_random_timing.py", MICRO, TYPE_NEW_OPT,
   "added some help: -help_advanced, -help_todo",
   NULL
 } ,

 {  3,  Jan, 2017, RCR, "make_random_timing.py", MICRO, TYPE_ENHANCE,
   "merged mrt branch into master - prep for advanced timing",
   NULL
 } ,

 { 29,  Dec, 2016, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "remove case 16 (brainstem) from aparc+aseg.nii WM extraction in help",
   "Thanks to P Taylor for noting this."
 } ,

 { 15,  Dec, 2016, RCR, "column_cat", MICRO, TYPE_ENHANCE,
   "allow for reading from stdin via either '-' or 'stdin'",
   NULL
 } ,

 {  9,  Dec, 2016, RCR, "3dTsplit4D", MICRO, TYPE_ENHANCE,
   "allow for direct writing to NIFTI via prefix, e.g. result.nii",
   NULL
 } ,

 {  8,  Dec, 2016, RCR, "3dTsplit4D", MINOR, TYPE_NEW_OPT,
   "add -digits and -keep_datum; other minor updates",
   NULL
 } ,

 {  8,  Dec, 2016, RCR, "3dTsplit4D", MINOR, TYPE_NEW_PROG,
   "program to break 4D dataset into a set of 3D ones",
   "Authored by P Molfese."
 } ,

 {  8,  Dec, 2016, RCR, "plug_vol2surf", MICRO, TYPE_ENHANCE,
   "add -cmask option to correspond with any auto non-zero mask",
   NULL
 } ,

 {  7,  Dec, 2016, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "check for python2 and python3",
   NULL
 } ,

 {  5,  Dec, 2016, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "allow *:1 (or *ANYTHING) to mean no event",
   "This is in case someone also marries empty run events.\n"
   "Done for R Kampe."
 } ,

 { 30,  Nov, 2016, RCR, "afni-general", MAJOR, TYPE_ENHANCE,
   "<> range selector can now take a comma-delimited list of integers",
   "So for a dataset with integer values from 0 to 8, these commands\n"
   "should produce identical results:\n"
   "   3dcalc -a DSET+tlrc -expr 'a*amongst(a,3,4,5)' -prefix JELLO\n"
   "   3dbucket 'DSET+tlrc<4,3,5>'                    -prefix JELLO\n"
   "   3dbucket 'DSET+tlrc<3..5>'                     -prefix JELLO\n"
   "   3dbucket 'DSET+tlrc<2.3..5.86>'                -prefix JELLO\n"
   "Of course, this will probably get further enhanced to a list of\n"
   "float ranges.  We shall see.\n"
   "Comma-delimited labels should work now, with a plan to add general\n"
   "labels that might define all GM or similar in a FreeSurfer dataset, say."
 } ,

 { 18,  Nov, 2016, RCR, "@Align_Centers", MICRO, TYPE_NEW_OPT,
   "add -prefix option, to name output",
   NULL
 } ,

 { 17,  Nov, 2016, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "add some checks for flat_namespace on OS X",
   NULL
 } ,

 {  9,  Nov, 2016, RCR, "Dimon", MICRO, TYPE_NEW_OPT,
   "add -gert_chan_prefix",
   "Done for W Luh."
 } ,

 {  9,  Nov, 2016, RCR, "to3d", MICRO, TYPE_MODIFY,
   "and Dimon/Dimon1: siemens timing outside range is only a warning",
   NULL
 } ,

 {  8,  Nov, 2016, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "possibly invert slice order, as DICOM sorting might affect MRILIB_orients",
   "Thanks to W Luh for noting the problem."
 } ,

 {  2,  Nov, 2016, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "handle 10.12 version string",
   NULL
 } ,

 {  1,  Nov, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add PPI preparation options",
   "Add -regress_skip_censor to omit 3dD -censor option.\n"
   "Add -write_ppi_3dD_scripts with corresponding options\n"
   "    -regress_ppi_stim_files and -regress_ppi_stim_labels.\n"
   "These make PPI pre and post regression scripts, along with\n"
   "    the main analysis script.\n"
   "Done for S Haller."
 } ,

 { 24,  Oct, 2016, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "bandpass notes and reference; stronger warning on missing -tlrc_dset",
   "Thanks to P Taylor."
 } ,

 { 20,  Oct, 2016, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "check -mask_import for reasonable voxel dimensions",
   NULL
 } ,

 { 12,  Oct, 2016, RCR, "RetroTS.py", MAJOR, TYPE_NEW_PROG,
   "distribute RetroTS.py (RetroTS.m converted to python)",
   "This should behave almost exactly as the MATLAB version.\n"
   "See 'RetroTS.py -help' for details.\n"
   "Much thanks to J Zosky for this contribution!"
 } ,

 { 11,  Oct, 2016, RCR, "RetroTS.py", MICRO, TYPE_MODIFY,
   "make top-level executable, remove 'style' lib, quotes, use lib_RetroTS",
   NULL
 } ,

 {  9,  Oct, 2016, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "new options -mask_import, -mask_intersect and -mask_union",
   "For J Stoddard and J Jarcho."
 } ,

 {  5,  Oct, 2016, RCR, "afni-general", MAJOR, TYPE_NEW_PROG,
   "update from C Craddock and dclark87",
   "New Programs: 3dLFCD, 3dDegreeCentrality, 3dECM, 3dMSE, 3dsvm_linpredict."
 } ,

 { 28,  Sep, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -regress_ROI[_PC]_per_run options, to make per-run regressors",
   "Also, used 3dTproject to detrend PC regressors for appropriate censoring."
 } ,

 { 23,  Sep, 2016, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "add -select_runs option",
   NULL
 } ,

 { 19,  Sep, 2016, RCR, "@move.to.series.dirs", MICRO, TYPE_NEW_OPT,
   "add -glob, for processing glob forms via afni_util.py and xargs",
   NULL
 } ,

 { 19,  Sep, 2016, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "add glob2stdout, for converting glob forms to lists in stdout",
   NULL
 } ,

 { 16,  Sep, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -radial_correlate option, to run @radial_correlate in proc script",
   NULL
 } ,

 { 16,  Sep, 2016, RCR, "realtime_receiver.py", MICRO, TYPE_MODIFY,
   "continue even if requested GUI fails",
   NULL
 } ,

 { 13,  Sep, 2016, RCR, "3dANOVA3", MICRO, TYPE_MODIFY,
   "change 'illegal argument after' errors to something more specific",
   NULL
 } ,

 { 13,  Sep, 2016, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "add -blip_opts_qw to pass options to 3dQwarp in the blip block",
   NULL
 } ,

 { 29,  Aug, 2016, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "add a few more DYLD_FALLBACK_LIBRARY_PATH tests",
   NULL
 } ,

 { 25,  Aug, 2016, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fix output.proc prefix in case -script has a path",
   "Also, allow -mask_apply group in the case of -tlrc_NL_warped_dsets.\n"
   "Thanks to C Capistrano and P Kim for noting the output.proc problem.\n"
   "Thanks to C Connolly for noting the mask problem."
 } ,

 { 25,  Aug, 2016, RCR, "afni_util.py", MICRO, TYPE_ENHANCE,
   "add append flag to change_path_basename()",
   NULL
 } ,

 { 23,  Aug, 2016, RCR, "file_tool", MINOR, TYPE_NEW_OPT,
   "add -fix_rich_quotes; if fixing a script, convert rich quotes to ASCII",
   "Done for G Chen."
 } ,

 { 22,  Aug, 2016, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "save all '3dAllineate -allcostX' anat/EPI costs to out.allcostX.txt",
   "This is another quality control measure."
 } ,

 { 19,  Aug, 2016, RCR, "slow_surf_clustsim.py", MINOR, TYPE_ENHANCE,
   "can use surf_mask to restrict surface clustering",
   "Use '-uvar surf_mask' to restrict on_surface clustering to mask.\n"
   "Append SSCS command to end of script and handle NIFTI surf_vol.\n"
   "The surf_mask mas added for C Thomas."
 } ,

 { 16,  Aug, 2016, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "look for new ACF/FWHM blur estimates",
   NULL
 } ,

 { 15,  Aug, 2016, RCR, "afni_proc.py", MAJOR, TYPE_ENHANCE,
   "default clustsim method is now mixed model ACF",
   "This marks afni_proc.py version 5.00.\n"
   "  o run 3dFWHMx with -ACF\n"
   "  o ACF and ClustSim files go into sub-directories, files_ACF/ClustSim\n"
   "  o -regress_run_clustsim now prefers arguments, ACF, FWHM, both, no\n"
   "  o default clustsim method is now ACF (via -regress_run_clustsim yes)"
 } ,

 {  10,  Aug, 2016, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "in Makefile.INCLUDE, have afni depend first on libmri.a",
   NULL
 } ,

 {   6,  Aug, 2016, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "mri_read_ascii: check comment_buffer before strdup",
   NULL
 } ,

 {   5,  Aug, 2016, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "make -help_install more current",
   NULL
 } ,

 {   5,  Aug, 2016, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "add -marry_AM",
   "Added for J Wiggins."
 } ,

 {   1,  Aug, 2016, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "do more hunting and reporting on libgomp and libglib",
   NULL
 } ,

 {  25,  Jul, 2016, RCR, "3dDeconvolve", MICRO, TYPE_BUG_FIX,
   "fixed attaching lone '-' to following label",
   "No NI_malloc might lead to crash, and the '-' was overwritten."
 } ,

 {  23,  Jul, 2016, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "if empty regressor, check for -GOFORIT (only suggest if not found)",
   NULL
 } ,

 {  14,  Jul, 2016, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "THD_open_tcat: print error and return NULL on bad wildcard match",
   NULL
 } ,

 {  8,  Jul, 2016, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "add -read_all, in case it is useful for sorting (e.g. -order_as_zt)",
   "Added for K Vishwanath."
 } ,

 {  7,  Jul, 2016, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "add -order_as_zt to re-order from -time:tz to -time:zt",
   NULL
 } ,

 {  7,  Jul, 2016, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "check for partial install of PyQt4 (might be common on OS X 10.11)",
   NULL
 } ,

 { 30,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "allow single volume EPI input (e.g. to test blip correction)",
   "Also, auto -blip_forward_dset should come from tcat output."
 } ,

 { 29,  Jun, 2016, RCR, "3dfim", MICRO, TYPE_BUG_FIX,
   "fix crash for dset open error with long dset name",
   "Thanks to J Henry for reporting the problem."
 } ,

 { 29,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "can modify blip order; BLIP_BASE -> MEDIAN_BLIP; add BLIP_NOTE to help",
   NULL
 } ,

 { 27,  Jun, 2016, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "allow for blip datasets that are not time series",
   NULL
 } ,

 { 24,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -requires_afni_hist; warp vr_base to make final_epi dset",
   NULL
 } ,

 { 23,  Jun, 2016, RCR, "plug_realtime", MICRO, TYPE_ENHANCE,
   "show pop-up if user selects Mask Vals without 3D RT registration",
   "Clarify this in the plugin Help.  For L Li and V Roopchansingh."
 } ,

 { 23,  Jun, 2016, RCR, "@auto_tlrc", MICRO, TYPE_ENHANCE,
   "allow to work with NIFTI template",
   NULL
 } ,

 { 22,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "do nothing, but work really hard at it",
   "Rewrite EPI transformation steps by storing and applying an array\n"
   "of transformations: this should make future changes easier."
 } ,

 { 22,  Jun, 2016, RCR, "@diff.tree", MICRO, TYPE_NEW_OPT,
   "added -diff_opts; e.g. -diff_opts -w",
   NULL
 } ,

 { 22,  Jun, 2016, RCR, "@diff.files", MICRO, TYPE_NEW_OPT,
   "added -diff_opts; e.g. -diff_opts -w",
   NULL
 } ,

 { 22,  Jun, 2016, RCR, "auto_warp.py", MICRO, TYPE_BUG_FIX,
   "correctly check base.exists()",
   NULL
 } ,

 { 17,  Jun, 2016, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "w/dglen, dsets with HEAD in prefix (but no suffix) could not be opened",
   NULL
 } ,

 { 16,  Jun, 2016, RCR, "afni_proc.py", MAJOR, TYPE_ENHANCE,
   "EPI blip up/blip down distortion correction is ready",
   "Thanks to S Torrisi and D Glen."
 } ,

 { 16,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "if NLwarp but no EPI warp, no NL; fix refit of blip median datsets",
   "User probably forgot -volreg_tlrc_warp, so warn them."
 } ,

 { 14,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -blip_forward_dset; if needed copy along any obliquity information",
   NULL
 } ,

 { 13,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -align_unifize_epi : 3dUnifize EPI before anat alignment",
   "Thanks to D Glen and S Torrisi for suggesting it."
 } ,

 { 13,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "add BLIP_BASE case for -volreg_align_to",
   "Use warped median forward blip volume as volreg alignment base."
 } ,

 { 10,  Jun, 2016, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "add -blip_reverse_dset for blip up/blip down distortion correction",
   NULL
 } ,

 {  2,  Jun, 2016, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "w/dglen: read num slices from Siemens 3D acquisition DICOM image files",
   NULL
 } ,

 {  1,  Jun, 2016, RCR, "make_random_timing.py", MICRO, TYPE_ENHANCE,
   "minor updates to verbose output",
   NULL
 } ,

 { 23,  May, 2016, RCR, "parse_fs_lt_log.py", MINOR, TYPE_NEW_PROG,
   "get an index list from a FreeSurfer labletable log file",
   NULL
 } ,


 { 20,  May, 2016, RCR, "afni_system_check.py", MINOR, TYPE_NEW_OPT,
   "add options -dot_file_list/_pack/_show",
   "List, package (tgz) or show the contents of found 'dot' files."
 } ,

 { 19,  May, 2016, RCR, "dicom_hdr", MICRO, TYPE_BUG_FIX,
   "do not crash on missing input",
   NULL
 } ,

 {  5,  May, 2016, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "-defaults similarly implies -do_dotfiles and apsearch yes",
   NULL
 } ,

 {  4,  May, 2016, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "add -do_dotfiles and -do_extras",
   "Running apsearch and possibly editing dot files is only on request."
 } ,

 {  3,  May, 2016, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "suggest -regress_est_blur_epits for resting state",
   "Thanks to S Torrisi for bringing up the question."
 } ,

 { 28,  Apr, 2016, RCR, "@update.afni.binaries", MINOR, TYPE_NEW_OPT,
   "add -local_package, to use an already downloaded package",
   "Requested by P Taylor."
 } ,

 { 28,  Apr, 2016, RCR, "3dMVM.R", MICRO, TYPE_MODIFY,
   "prevent any unexpected writing of dbg.AFNI.args files",
   "Only write such files given -dbg_args.\n"
   "Affects 1dRplot, 3dLME, 3dMEMA, 3dMVM, 3dPFM, 3dRprogDemo\n"
   "        3dSignatures, AFNIio, ExamineXmat, rPkgsInstall.\n"
   "The dbg files no longer start with '.'."
 } ,

 { 28,  Apr, 2016, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "add initial #!prog to tops of some meica programs, and bricks_test.sh",
   "This corresponds with -x permission clearing by yarikoptic."
 } ,

 { 27,  Apr, 2016, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "prep for later: always extract volreg base, as vr_base*",
   NULL
 } ,

 { 26,  Apr, 2016, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "Makefile.INCLUDE: moved SCRIPTS into scripts_install",
   NULL
 } ,

 { 26,  Apr, 2016, RCR, "tokens", MICRO, TYPE_ENHANCE,
   "handle arbitrarily long tokens",
   NULL
 } ,

 { 19,  Apr, 2016, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "incorrectly reported 'IFM:RIF fatal error' for the case of no sorting",
   "Thanks to I Groen for reporting the problem."
 } ,

 { 18,  Apr, 2016, RCR, "3dbucket", MICRO, TYPE_ENHANCE,
   "if THD_open_one_dataset fails, fall back to THD_open_dataset",
   "As with 3dTcat, fall back rather than failing outright."
 } ,

 { 18,  Apr, 2016, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "THD_open_tcat: fix wildcard input lacking sub-brick selectors",
   "Forgot to regenerate catenated list, dlocal."
 } ,

 { 15,  Apr, 2016, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "added help macro CATENATE_HELP_STRING",
   "This displays a 'CATENATED AND WILDCARD DATASET NAMES' section in the\n"
   "help output from afni and 3dcalc (following 'INPUT DATASET NAMES')."
 } ,

 { 15,  Apr, 2016, RCR, "NIFTI-2", MICRO, TYPE_MODIFY,
   "print int64_t using PRId64 macro, which looks ugly, but avoids warnings",
   NULL
 } ,

 {  8,  Apr, 2016, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "auto-tcat inputs with spaces, wildcards and global selectors",
   "For example, input of 'DA*.HEAD DB*.HEAD DC*.HEAD[3,4]' would create\n"
   "a dataset with sub-bricks 3 and 4 from all D[ABC]*.HEAD datasets.\n"
   "If sub-brick selectors appear only at the end, they are global,\n"
   "otherwise they can be per input, as in 'DA*.HEAD[0,1] DB*.HEAD[2,3]'."
 } ,

 {  7,  Apr, 2016, RCR, "Makefile.INCLUDE", MICRO, TYPE_MODIFY,
   "switch to have cjpeg depend on libjpeg.a, so it does not get rebuilt",
   NULL
 } ,

 {  7,  Apr, 2016, RCR, "@update.afni.binaries", MINOR, TYPE_NEW_OPT,
   "add options -proto and -test_proto",
   "One can specify the download protocol as http, https or NONE."
 } ,

 {  5,  Apr, 2016, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "THD_open_tcat works as wildcard - can apply sub-brick selectors",
   NULL
 } ,

 {  4,  Apr, 2016, RCR, "3dTcat", MICRO, TYPE_ENHANCE,
   "if THD_open_one_dataset fails, fall back to THD_open_dataset",
   "Fall back rather than failing outright."
 } ,

 {  4,  Apr, 2016, RCR, "THD_load_tcat", MINOR, TYPE_BUG_FIX,
   "set factors and labels at open time, not at load time",
   "This fixes use with 3dcalc and 3dinfo, though stats are not yet loaded."
 } ,

 { 31,  Mar, 2016, RCR, "3dMVM.R", MICRO, TYPE_MODIFY,
   "do not create .dbg.AFNI.args files on -help usage",
   "Modified 1dRplot.R, 3dLME.R, 3dMEMA.R, 3dMVM.R, 3dPFM.R, 3dRprogDemo.R,\n"
   "         3dSignatures.R, ExamineXmat.R and rPkgsInstall.R."
 } ,

 { 30,  Mar, 2016, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "possibly update .bashrc if .cshrc is updated",
   NULL
 } ,

 { 29,  Mar, 2016, RCR, "@update.afni.binaries", MICRO, TYPE_NEW_OPT,
   "add option -no_cert_verify, to skip server certificate verification",
   NULL
 } ,

 { 29,  Mar, 2016, RCR, "gen_group_command.py", MICRO, TYPE_MODIFY,
   "3dMEMA no longer allows for a paied test",
   "One must now input contrast/stat from original regression."
 } ,

 { 29,  Mar, 2016, RCR, "afni_restproc.py", MICRO, TYPE_MODIFY,
   "suggest that users consider afni_proc.py",
   "This was requested by Rayus."
 } ,

 { 26,  Mar, 2016, RCR, "to3d", MICRO, TYPE_BUG_FIX,
   "another allowance for datasets exceeding 2 GB",
   "Thanks to SS Kim for noting the problem."
 } ,

 { 24,  Mar, 2016, RCR, "Dimon", MICRO, TYPE_NEW_OPT,
   "add option -use_obl_origin, to apply -oblique_origin to to3d command",
   "Also, run any to3d script via 'tcsh -x' so the users get to see."
 } ,

 { 22,  Mar, 2016, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "modularize version checking",
   "Update .afni.vctime even if URL read fails, to not flood server."
 } ,

 { 21,  Mar, 2016, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "get slightly more accurate motion ave via 3dTstat -nzmean",
   NULL
 } ,

 { 21,  Mar, 2016, RCR, "3dANOVA", MICRO, TYPE_ENHANCE,
   "allow for up to 666 observations",
   "Consider changing this to use a string_list or something similar."
 } ,

 { 21,  Mar, 2016, RCR, "GLTsymtest", MICRO, TYPE_NEW_OPT,
   "added -badonly, to avoid screen clutter from many good GLTs",
   NULL
 } ,

 { 21,  Mar, 2016, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "run GLTsymtest on specified GLTs",
   NULL
 } ,

 { 15,  Mar, 2016, RCR, "timing_tool.py", MICRO, TYPE_BUG_FIX,
   "-help_basis update: max convolved BLOCK() is ~5.1, not ~5.4",
   NULL
 } ,

 {  1,  Mar, 2016, RCR, "GIFTI", MICRO, TYPE_BUG_FIX,
   "applied R Vincent fix for GIFTI datasets with Windows-style newlines",
   NULL
 } ,

 {  1,  Mar, 2016, RCR, "tokens", MINOR, TYPE_NEW_PROG,
   "program to extract valid text entries from a file",
   NULL
 } ,

 { 29,  Feb, 2016, RCR, "3dANOVA3", MICRO, TYPE_ENHANCE,
   "show prior options to any 'Unrecognized command line option'",
   "Added disp_strings() to libmri.a."
 } ,

 { 24,  Feb, 2016, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "replace tlrc_no_ss with anat_has_skull toggle and move to anat block",
   NULL
 } ,

 { 24,  Feb, 2016, RCR, "timing_tool.py", MINOR, TYPE_BUG_FIX,
   "fix -warn_tr_stats crash on empty timing file",
   "Thanks to Z Reagh for noting the problem."
 } ,

 { 19,  Feb, 2016, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "add many tests and summarize potential issues",
   NULL
 } ,

 { 17,  Feb, 2016, RCR, "afni_util.py", MICRO, TYPE_ENHANCE,
   "add function affine_to_params_6: where input is 12 element sub-matrix",
   NULL
 } ,

 { 16,  Feb, 2016, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "add -ver and initial version reporting",
   NULL
 } ,

 { 10,  Feb, 2016, RCR, "@ANATICOR", MICRO, TYPE_BUG_FIX,
   "fixed -radius option parsing",
   "Thanks to A Frithsen for noting the problem."
 } ,

 { 10,  Feb, 2016, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "full update to use https://afni.nimh.nih.gov (no longer http)",
   NULL
 } ,

 {  9,  Feb, 2016, RCR, "@GradFlipTest", MICRO, TYPE_MODIFY,
   "trap for missing inputs (e.g. used with just -help)",
   NULL
 } ,

 {  9,  Feb, 2016, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "be sure isfinite is defined in FD2_inc.c, parser_int.c",
   NULL
 } ,

 {  8,  Feb, 2016, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "access afni site via https protocol",
   NULL
 } ,

 {  5,  Feb, 2016, RCR, "thd_niftiread", MICRO, TYPE_BUG_FIX,
   "NIFTI files with no *form_codes should default to +orig, not NIFTI_default",
   NULL
 } ,

 { 28,  Jan, 2016, RCR, "3dMean", MICRO, TYPE_MODIFY,
   "fix help to correctly report -stdev as sqrt(var)   {was var/(n-1)}",
   "Thanks to K Kerr (MB) for pointing out the mistake."
 } ,

 { 27,  Jan, 2016, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "allow for tissue based regression with only regress block",
   NULL
 } ,

 {  4,  Jan, 2016, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "add boundardy checks in get_1dcat_intlist and get_count_intlist",
   "Without the checks, using count or 1dcat as sub-brick selector method\n"
   "would lead to confusing crashes (if values exceeded #vols).\n"
   "Thanks to W Graves for reporting the problem."
 } ,

 {  3,  Jan, 2016, RCR, "afni_system_check.py", MICRO, TYPE_MODIFY,
   "truncate 'top history' text for data trees",
   NULL
 } ,

 { 31,  Dec, 2015, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "include AFNI_version_base.txt in build of afni_src.tgz",
   NULL
 } ,

 { 30,  Dec, 2015, RCR, "afni-general", MAJOR, TYPE_ENHANCE,
   "w/DRG implement new version system",
   "See https://afni.nimh.nih.gov/pub/dist/MOTD/MOTD_2015_12_30.txt ."
 } ,

 { 29,  Dec, 2015, RCR, "afni_system_check.py", MICRO, TYPE_BUG_FIX,
   "catch any empty directory listing from @FindAfniDsetPath",
   NULL
 } ,

 { 29,  Dec, 2015, RCR, "@FindAfniDsetPath", MICRO, TYPE_MODIFY,
   "0 or bad # args returns 1 rather than 0",
   NULL
 } ,

 { 29,  Dec, 2015, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "updated gitignore.src.txt and main 2 .gitignore files",
   NULL
 } ,

 { 28,  Dec, 2015, RCR, "@diff.files", MICRO, TYPE_ENHANCE,
   "allow diffs to include existence of directories",
   NULL
 } ,

 { 19,  Dec, 2015, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "removed CSFe from Example 10 of the help, to not lead people to use it",
   NULL
 } ,

 {  7,  Dec, 2015, RCR, "MatAFNI_Demo.m", MICRO, TYPE_BUG_FIX,
   "merge fix from J. Pfannmoller",
   "Done with G Chen."
 } ,

 {  7,  Dec, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "help update: modify example 11 to use SurfVol and add FREESURFER NOTE",
   NULL
 } ,

 { 17,  Nov, 2015, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "rename g_info to g_dicom_ctrl to avoid FreeBSD build conflict",
   "Thanks to J Bacon for noting the conflict."
 } ,

 {  6,  Nov, 2015, RCR, "to3d", MICRO, TYPE_ENHANCE,
   "allow for single volumes beyond 2^31-1 bytes",
   "Done for Z Saad."
 } ,

 {  6,  Nov, 2015, RCR, "@Align_Centers", MICRO, TYPE_NEW_OPT,
   "add option -cm_no_amask; like -cm but without -automask",
   NULL
 } ,

 {  4,  Nov, 2015, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "add -slice_order_to_times",
   "This converts a list of slice indices (sorted by acq time)\n"
   "to slice times (ordered by index)."
 } ,

 { 28,  Oct, 2015, RCR, "gen_ss_review_table.py", MICRO, TYPE_ENHANCE,
   "make 'a/E mask Dice coef' parent of 'mask correlation'",
   NULL
 } ,

 { 28,  Oct, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "look for dice coef file ae_dice, as well ae_corr",
   NULL
 } ,

 { 28,  Oct, 2015, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "output anat/EPI Dice coefficient, rather than correlation",
   NULL
 } ,

 { 28,  Oct, 2015, RCR, "3ddot", MINOR, TYPE_NEW_OPT,
   "add -dodice, to get the Dice coefficient",
   NULL
 } ,

 { 26,  Oct, 2015, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "compute TSNR restricted to uncensored TRs",
   NULL
 } ,

 { 26,  Oct, 2015, RCR, "afni", MICRO, TYPE_MODIFY,
   "move version to show_AFNI_version and call on -ver",
   NULL
 } ,

 { 23,  Oct, 2015, RCR, "afni", MICRO, TYPE_MODIFY,
   "move version output after some text requests",
   NULL
 } ,

 { 21,  Oct, 2015, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "added -exec",
   NULL
 } ,

 { 25,  Sep, 2015, RCR, "suma", MICRO, TYPE_MODIFY,
   "volume rendering is no longer the default for SUMA_VO_InitSlices",
   "So Do_06_VISdti_SUMA_visual_ex1.tcsh defaults to showing 3 volume slices."
 } ,

 { 24,  Sep, 2015, RCR, "ccalc", MICRO, TYPE_MODIFY,
   "make dependency on libmri explicit",
   "Some of these operations are for building on Fedora 22."
 } ,

 { 24,  Sep, 2015, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "inline func with static vars should be static",
   "Fails to link in Fedora 22."
 } ,

 { 24,  Sep, 2015, RCR, "vol2surf", MICRO, TYPE_ENHANCE,
   "restrict THD_extract_series error messages (e.g. for RGB datasets)",
   "Requested by P Taylor."
 } ,

 { 24,  Sep, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "allow 3dD to proceed with only extra_stim_files",
   NULL
 } ,

 { 16,  Sep, 2015, RCR, "suma", MICRO, TYPE_MODIFY,
   "w/dglen SUMA_find_any_object: fixed loss of isGraphDset result",
   NULL
 } ,

 { 11,  Sep, 2015, RCR, "3dBandpass", MICRO, TYPE_MODIFY,
   "do not propagate scalars",
   NULL
 } ,

 { 11,  Sep, 2015, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "edt_floatize.c: for NIFTI float->float with scale factors, apply them",
   "Also, fix determination of when to convert.\n"
   "Thanks to Pengmin (MB) for noting this problem."
 } ,

 { 10,  Sep, 2015, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fix resulting aligned SurfVol if input is NIFTI",
   NULL
 } ,

 {  3,  Sep, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "give REML priority in guessing stats_dset",
   NULL
 } ,

 {  2,  Sep, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "add -errts_dset to gen_ss_review_scripts.py command",
   NULL
 } ,

 {  2,  Sep, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "some option vars were being over-written",
   NULL
 } ,

 {  2,  Sep, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "if rest and REML, use REML errts",
   NULL
 } ,

 {  1,  Sep, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "track errts dset, and possibly use it for voxel dims",
   NULL
 } ,

 { 27,  Aug, 2015, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "check for R packages via 'rPkgsInstall -pkgs ALL -check'",
   NULL
 } ,

 { 26,  Aug, 2015, RCR, "Makefile.ptaylor.INCLUDE", MICRO, TYPE_ENHANCE,
   "put -L.. before $LFLAGS to link local libjpeg",
   NULL
 } ,

 { 25,  Aug, 2015, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "if initial install, update .cshrc",
   "If initial install (afni not in PATH) and PATH not set in .cshrc,\n"
   "update path (PATH) and do 'apsearch -afni_help_dir' update in .cshrc."
 } ,

 { 21,  Aug, 2015, RCR, "Makefile.INCLUDE", MICRO, TYPE_ENHANCE,
   "move gifti_tool/cifti_tool from EXPROGS to PROGRAM_LIST",
   "Modified 28 Makefiles."
 } ,

 { 21,  Aug, 2015, RCR, "Makefile.INCLUDE", MICRO, TYPE_ENHANCE,
   "make cifti_tool",
   NULL
 } ,

 { 21,  Aug, 2015, RCR, "cifti_tool", MAJOR, TYPE_NEW_PROG,
   "initial release: updated help and added -hist",
   "New program to evaluate CIFTI datasets."
 } ,

 { 21,  Aug, 2015, RCR, "timing_tool.py", MICRO, TYPE_BUG_FIX,
   "start-of-run fix to -multi_timing_to_event_list offsets",
   NULL
 } ,

 { 21,  Aug, 2015, RCR, "make_random_timing.py", MICRO, TYPE_ENHANCE,
   "add more help for 'NOTE: distribution of ISI', including a short script",
   NULL
 } ,

 { 20,  Aug, 2015, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "add -show_isi_pdf and -show_isi_f_pdf",
   NULL
 } ,

 { 19,  Aug, 2015, RCR, "gen_ss_review_table.py", MINOR, TYPE_NEW_OPT,
   "add -show_missing, to show all missing labels from all files",
   NULL
 } ,

 { 14,  Aug, 2015, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "SUMA_CIFTI_2_edset: use 'no suma' version: SUMA_WriteDset_ns",
   NULL
 } ,

 { 13,  Aug, 2015, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "megrged cifti-toy branch from Ziad, for viewing CIFTI dataset in suma",
   NULL
 } ,

 { 12,  Aug, 2015, RCR, "@radial_correlate", MICRO, TYPE_NEW_OPT,
   "add -mask option, to apply instead of automask",
   "Done for Giri."
 } ,

 { 12,  Aug, 2015, RCR, "gen_group_command.py", MINOR, TYPE_ENHANCE,
   "allow for generic/unknown commands via -command (e.g. ls, 3dTcat)",
   "Done for W-L Tseng."
 } ,

 {  7,  Aug, 2015, RCR, "3dhistog", MICRO, TYPE_NEW_OPT,
   "add -noempty option, to ignore empty bins",
   NULL
 } ,

 {  7,  Aug, 2015, RCR, "model_conv_PRF", MICRO, TYPE_MODIFY,
   "make everything static, to avoid confusion",
   NULL
 } ,

 {  7,  Aug, 2015, RCR, "model_conv_PRF_6", MICRO, TYPE_BUG_FIX,
   "make everything static, to avoid confusion; proto for conv_set_ref",
   NULL
 } ,

 {  5,  Aug, 2015, RCR, "nifti_tool", MICRO, TYPE_ENHANCE,
   "apply library updates for potentially writing NIFTI-2",
   NULL
 } ,

 {  5,  Aug, 2015, RCR, "NIFTI", MINOR, TYPE_ENHANCE,
   "if conversion to NIFTI-1 header fails on write, try NIFTI-2",
   NULL
 } ,

 {  5,  Aug, 2015, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "add some support for reading Voxel_Bucket datasets into AFNI",
   "That is a type that is currently specific to SUMA."
 } ,

 {  3,  Aug, 2015, RCR, "Dimon1", MICRO, TYPE_BUG_FIX,
   "fixed -drive_afni, -drive_wait and -rt_cmd command lists",
   NULL
 } ,

 {  3,  Aug, 2015, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "applied ACQUSITION_TYPE as 3d+timing",
   "Slice timing was lost (by the plugin) in the change to 3d+t ACQ TYPE.\n"
   "Thanks to H Mandelkow for bringing this up."
 } ,

 {  3,  Aug, 2015, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "added DTYPE_3DTM (3D+timing) ACQUSITION_TYPE",
   "Treats data as per volume, but with slice timing.  This is needed for\n"
   "num_chan>0, but when data does not come in num_chan slices at a time."
 } ,

 { 31,  Jul, 2015, RCR, "@FindAfniDsetPath", MICRO, TYPE_ENHANCE,
   "allow full paths to succeed ; no args gives help",
   NULL
 } ,

 { 30,  Jul, 2015, RCR, "@auto_tlrc", MICRO, TYPE_BUG_FIX,
   "check for template existence even given path",
   "Was failing with -init_xform."
 } ,

 { 29,  Jul, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "block any _REMLvar stats dset (was _REMLvar+)",
   "Might get stats*_REMLvar_nods, for example, via 3dREMLfit -dsort_nods."
 } ,

 { 29,  Jul, 2015, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "ANATICOR now works with task analysis, using -regress_reml_exec",
   "Done for R W Cox."
 } ,

 { 29,  Jul, 2015, RCR, "3dcalc", MICRO, TYPE_MODIFY,
   "clarify error about mis-match in number of volumes",
   NULL
 } ,

 { 28,  Jul, 2015, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "ANATICOR now includes zero volumes at censor points",
   "This matches non-ANATICOR and fast ANATICOR cases."
 } ,

 { 27,  Jul, 2015, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "renamed -regress_WMeL_corr to -regress_make_corr_AIC and default to 'no'",
   NULL
 } ,

 { 24,  Jul, 2015, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "GIFTI datasets should have NODE_INDEX list as first DataArray",
   "Thanks to N Oosterhof for pointing this out."
 } ,

 { 23,  Jul, 2015, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "allow Graph_Bucket niml.dsets to be read, but just as 1D",
   NULL
 } ,

 { 17,  Jul, 2015, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "Dimon->afni: small TCP buffers cause volumes to be passed slowly",
   "In iochan_recvall, increase nap time only if packets < 4K are received."
 } ,

 { 13,  Jul, 2015, RCR, "nifti_tool", MAJOR, TYPE_ENHANCE,
   "nifti_tool is now based on NIFTI-2, with many corresponding new options",
   "The old nifti_tool (based on NIFTI-1) is now nifti1_tool."
 } ,

 { 13,  Jul, 2015, RCR, "nifti1_tool", MINOR, TYPE_NEW_PROG,
   "nifti1_tool is the NIFTI-1 version of nifti_tool",
   NULL
 } ,

 { 13,  Jul, 2015, RCR, "NIFTI-2", MAJOR, TYPE_ENHANCE,
   "added NIFTI-2 support into AFNI",
   "Main source update: nifti/nifti2 tree, then applied it in mostly\n"
   "   thd_niftiread/write.c (plus gifti_io.h, 3ddata.h, mrilib.h).\n"
   "To compile into all of AFNI edit: Makefile.INCLUDE, SUMA_Makefile_NoDev,\n"
   "   Makefile.avovk.INCLUDE and Makefile.ptaylor.INCLUDE."
 } ,

 {  11,  Jul, 2015, RCR, "@diff.files", MICRO, TYPE_NEW_OPT,
   "added -longlist",
   NULL
 } ,

 {  1,  Jul, 2015, RCR, "cifti_tool", MINOR, TYPE_NEW_OPT,
   "reorg and more recur functions",
   NULL
 } ,

 {  1,  Jul, 2015, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "clarified help for -anat_unif_GM",
   NULL
 } ,

 { 24,  Jun, 2015, RCR, "afni_xml_tool", MINOR, TYPE_NEW_OPT,
   "afni_xml updates, and start to afni_xml_tool",
   NULL
 } ,

 { 18,  Jun, 2015, RCR, "3dExtrema", MINOR, TYPE_NEW_OPT,
   "added -nbest",
   "Output -nbest extrema; -quiet does not suppress extrema output."
 } ,

 { 17,  Jun, 2015, RCR, "GIFTI", MINOR, TYPE_NEW_OPT,
   "added functions for reading from a buffer",
   NULL
 } ,

 { 16,  Jun, 2015, RCR, "CIFTI", MINOR, TYPE_GENERAL,
   "added initial nifti/cifti tree",
   NULL
 } ,

 { 16,  Jun, 2015, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "applied -regress_stim_times_offset to typical timing files",
   "Allows for stim timing offset when copying to stimuli directory."
 } ,

 { 15,  Jun, 2015, RCR, "nifti_tool", MINOR, TYPE_NEW_OPT,
   "added -disp_cext",
   NULL
 } ,

 { 10,  Jun, 2015, RCR, "auto_warp.py", MICRO, TYPE_BUG_FIX,
   "clear any AFNI_COMPRESSOR variable, so that scripts do not get confused",
   "NIFTI is the default, so avoid script confusion with automatic nii.gz.\n"
   "In the future, maybe process as AFNI."
 } ,

 { 10,  Jun, 2015, RCR, "NIFTI", MICRO, TYPE_BUG_FIX,
   "THD_open_one_dataset: let THD_open_nifti look for alternate files",
   "CHECK_FOR_DATA() requires a file name match, but NIFTI is forgiving."
 } ,

 { 10,  Jun, 2015, RCR, "@diff.files", MAJOR, TYPE_NEW_PROG,
   "compare list of files with those in other directory",
   NULL
 } ,

 { 10,  Jun, 2015, RCR, "@diff.tree", MAJOR, TYPE_NEW_PROG,
   "look for differences between files in two directories",
   "Should merge @diff.files and @diff.tree, and change to python."
 } ,

 {  8,  Jun, 2015, RCR, "neuro_deconvolve.py", MICRO, TYPE_ENHANCE,
   "allow -inputs to include paths",
   NULL
 } ,

 {  6,  Jun, 2015, RCR, "timing_tool.py", MICRO, TYPE_NEW_OPT,
   "added -per_run_file",
   NULL
 } ,

 {  2,  Jun, 2015, RCR, "NIFTI", MICRO, TYPE_GENERAL,
   "NIFTI-1,2: added NIFTI_ECODE_CIFTI/VARIABLE_FRAME_TIMING/EVAL/MATLAB",
   NULL
 } ,

 {  1,  Jun, 2015, RCR, "nifti_tool", MINOR, TYPE_ENHANCE,
   "diff/disp_hdr detects type; diff_hdr1/2",
   NULL
 } ,

 {  1,  Jun, 2015, RCR, "3dttest++", MICRO, TYPE_NEW_OPT,
   "added -dupe_ok and more warnings when dataset labels match",
   NULL
 } ,

 { 27,  May, 2015, RCR, "@Install_TSrestMovieDemo", MICRO, TYPE_BUG_FIX,
   "set and applied $demo as Suma_TSrestMovieDemo",
   NULL
 } ,

 { 26,  May, 2015, RCR, "3dBlurToFWHM", MICRO, TYPE_MODIFY,
   "make -help output consistent in using FWHM (along with 3dLocalstat)",
   NULL
 } ,

 { 26,  May, 2015, RCR, "NIFTI", MINOR, TYPE_ENHANCE,
   "nifti_read_header returns generic pointer; rename N-1/2 header read funcs",
   NULL
 } ,

 { 22,  May, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "help clarifications for -regress_ROI* options",
   NULL
 } ,

 { 22,  May, 2015, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "allow for small differences when comparing oblique angles",
   "Define OBLIQ_ANGLE_THRESH=0.01 as a tolerance for the difference.\n"
   "This was done to fix registration to external dset in realtime.\n"
   "Thanks to V Roopchansingh for bringing up the problem."
 } ,

 { 19,  May, 2015, RCR, "3dClustSim", MICRO, TYPE_MODIFY,
   "do not allow -pthr to precede -both or -niml",
   "Otherwise -pthr values would be lost."
 } ,

 { 18,  May, 2015, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "allow ROI PC regression for local masks (not just external ones)",
   "External ROIs should now be passed via -anat_follower_ROI, rather than\n"
   "-regress_ROI_*, the latter no longer taking dataset parameters.\n"
   "Also changed -regress_ROI_erode to -anat_follower_erode and\n"
   "removed option -regress_ROI_maskave (use -regress_ROI)\n"
   "Done for R W Cox."
 } ,

 { 18,  May, 2015, RCR, "gen_ss_review_table.py", MICRO, TYPE_NEW_OPT,
   "mention gen_ss_review_scripts.py -help_fields in help",
   NULL
 } ,

 {  8,  May, 2015, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_make_corr_vols",
   "Use this to compute average correlation volumes for various masks."
 } ,

 {  7,  May, 2015, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "replaced slow 3dTfitter with 3dTproject in anaticor",
   "This should not affect the result, just the processing time."
 } ,

 {  5,  May, 2015, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "added help (inc Ex 11), follower modifications, WMe corr diag change",
   NULL
 } ,

 {  4,  May, 2015, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -anat_follower, -anat_follower_ROI, -regress_anaticor_label",
   NULL
 } ,

 {  1,  May, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "keep num regs of interest = 0 if num stim = 0",
   NULL
 } ,

 { 30,  Apr, 2015, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "allow AM2 centering param via basis backdoor (for now)",
   "For example, use basis function 'BLOCK(2) :x:0.176'\n"
   "Done for J Britton."
 } ,

 { 29,  Apr, 2015, RCR, "NIFTI", MINOR, TYPE_MODIFY,
   "allow reading and writing unknown extensions",
   NULL
 } ,

 { 28,  Apr, 2015, RCR, "clib_02.nifti2", MINOR, TYPE_NEW_PROG,
   "added clib_02.nifti2.c demo and Makefile under nifti2 dir",
   NULL
 } ,

 { 28,  Apr, 2015, RCR, "NIFTI", MAJOR, TYPE_ENHANCE,
   "apply updates to NIFTI-2 I/O library",
   "Also, include initial mods to nifti_tool, hidden under nifti2 dir."
 } ,

 { 28,  Apr, 2015, RCR, "NIFTI", MINOR, TYPE_GENERAL,
   "add nifti/nifti2 directory with current NIFTI-1 versions of 4 files",
   "This tracks initial changes to nifti2_io.[ch] nifti_tool.[ch]."
 } ,

 { 24,  Apr, 2015, RCR, "gen_group_command.py", MICRO, TYPE_ENHANCE,
   "tiny help update: examples of usage regarding subject IDs",
   NULL
 } ,

 { 23,  Apr, 2015, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_NEW_OPT,
   "add -help_fields[_brief], to describe the 'basic' output fields",
   NULL
 } ,

 { 22,  Apr, 2015, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "put in cat_matvec string to create warp.all.anat.aff12.1D",
   "Thanks to sgreen (MB) for noting the problem."
 } ,

 { 22,  Apr, 2015, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "add -todo; help update; verify use of erode list",
   NULL
 } ,

 { 22,  Apr, 2015, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "add fix for non-unix files; allow for multiple tests with -prefix",
   NULL
 } ,

 { 22,  Apr, 2015, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "Makefile.linux_fedora_19_64: alter -I dirs for glib to build on F21",
   NULL
 } ,

 { 14,  Apr, 2015, RCR, "uber_subject.py", MICRO, TYPE_NEW_OPT,
   "add MIN_OUTLIERS as an option for volreg base",
   NULL
 } ,

 { 14,  Apr, 2015, RCR, "3dDeconvolve", MICRO, TYPE_MODIFY,
   "PLOT_matrix_gray: add error messages to clarify malloc failures",
   NULL
 } ,

 {  9,  Apr, 2015, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fix -tlrc_NL_warped_dsets for NIFTI anat; add some -regress_ROI_PC help",
   NULL
 } ,

 {  8,  Apr, 2015, RCR, "@update.afni.binaries", MINOR, TYPE_ENHANCE,
   "updated to Ziad's new -revert option",
   NULL
 } ,

 {  8,  Apr, 2015, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "check for FATCAT_DEMO",
   NULL
 } ,

 {  7,  Apr, 2015, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "TLRC_warped_dsets: no view update if type != BRIK",
   NULL
 } ,

 {  7,  Apr, 2015, RCR, "afni_base.py", MICRO, TYPE_MODIFY,
   "ppves: no sel -> no quotes; dset_dims: check failures and return 4 vals",
   NULL
 } ,

 {  7,  Apr, 2015, RCR, "3dnvals", MICRO, TYPE_MODIFY,
   "have 3dnvals return status 1 if all dataset opens fail",
   NULL
 } ,

 {  2,  Apr, 2015, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -tlrc_NL_warped_dsets to import 3dQwarp result",
   "Added for P Molfese and others."
 } ,

 {  2,  Apr, 2015, RCR, "rickr/Makefile", MICRO, TYPE_BUG_FIX,
   "Imon and serial_helper should not use LLIBS",
   NULL
 } ,

 {  1,  Apr, 2015, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "anat followers and ROI_PC",
   "Datasets can follow the anatomical warps\n"
   "Added options -regress_ROI_PC, -regress_ROI_maskave, -regress_ROI_erode.\n"
   "PC allows for some number of principle components to be regressed, and\n"
   "   maskave is for mask averages to be regressed.\n"
   "The _erode option applies to either, and happens before xform.\n"
   "Also, any anat with skull is applied as a follower.\n"
   "Also, -tcat_remove_first_trs can now take a list."
 } ,

 {  1,  Apr, 2015, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "allow -censor_fill_parent with 2D files",
   NULL
 } ,

 { 31,  Mar, 2015, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "allow -censor_fill_parent with simple 1D files",
   "Done for 3dpc and censoring in afni_proc.py."
 } ,

 { 30,  Mar, 2015, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "update for selenium",
   "Includes: Makefile.linux_openmp*, xorg7*, osx_10.7*, fedora19_64,\n"
   "as well as Makefile.INCLUDE and rickr/Makefile for LLIBS."
 } ,

 { 23,  Mar, 2015, RCR, "python-general", MINOR, TYPE_ENHANCE,
   "broke VarsObject class out into separate file",
   NULL
 } ,

 { 19, Mar, 2015, RCR, "unix_tutorial", MINOR, TYPE_ENHANCE,
   "populated AFNI_data6/unix_tutorial with Sphinx version",
   "The previous tutorial was moved under 'old'."
 } ,

 { 18,  Mar, 2015, RCR, "sphinx", MAJOR, TYPE_ENHANCE,
   "added unix_tutorial to the doc tree",
   NULL
 } ,

 { 18,  Mar, 2015, RCR, "sphinx", MICRO, TYPE_MODIFY,
   "renamed tutorials.rst to SelfGuidedScripts.rst, along with tag",
   NULL
 } ,

 { 18,  Mar, 2015, RCR, "3dBandpass", MICRO, TYPE_ENHANCE,
   "let user know details of dimensionality reduction",
   NULL
 } ,

 { 13,  Mar, 2015, RCR, "Dimon", MICRO, TYPE_NEW_OPT,
   "added option -te_list to pass ECHO_TIMES to plug_realtime",
   NULL
 } ,

 { 13,  Mar, 2015, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "added code to receive and store ECHO_TIMES",
   "This is passed as control information and is stored in rtin->TE."
 } ,

 { 13,  Mar, 2015, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "added V Roopchansingh update for T2* est Merge function",
   NULL
 } ,

 { 12,  Mar, 2015, RCR, "3dDeconvolve.py", MICRO, TYPE_MODIFY,
   "allow for collinearity in regressor warnings",
   NULL
 } ,

 { 12,  Mar, 2015, RCR, "afni_base.py", MICRO, TYPE_BUG_FIX,
   "fixed capture in shell_exec2 for old python, where readlines() would hang",
   NULL
 } ,

 { 12,  Mar, 2015, RCR, "afni_util.py", MICRO, TYPE_ENHANCE,
   "implemented fast=0 in get/show_process_stack",
   NULL
 } ,

 { 11,  Mar, 2015, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "added covary and linear_fit; -listfunc takes -/stdin to read from stdin",
   NULL
 } ,

 {  3,  Mar, 2015, RCR, "powell_int.c", MICRO, TYPE_BUG_FIX,
   "multiple include directives got joined on one line",
   NULL
 } ,

 {  3,  Mar, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "added MIN_OUTLER as an option to -volreg_align_to",
   "Also, updated requirement data from Feb 9 to Nov 9."
 } ,

 {  2,  Mar, 2015, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed 3dTproject call for resting state on surface",
   "Thanks to Tara (message board) for noting the problem."
 } ,

 { 27,  Feb, 2015, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -regress_WMeL_corr option, which I forgot about last time",
   NULL
 } ,

 { 27,  Feb, 2015, RCR, "@compute_gcor", MICRO, TYPE_NEW_OPT,
   "added -corr_vol, to output a global correlation volume",
   "Note that afni_proc.py does these steps by default."
 } ,

 { 24,  Feb, 2015, RCR, "neuro_deconvolve.py", MINOR, TYPE_ENHANCE,
   "re-wrote method: new decon, upsample, multiple files, reconvolve",
   "This is partially for evaluation of the decon/recon PPI steps."
 } ,

 { 13,  Feb, 2015, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "make WMeLocal for fast anaticor a float dataset",
   "Also, generate WMeL_corr as a diagnostic volume."
 } ,

 { 13,  Feb, 2015, RCR, "3dcalc", MICRO, TYPE_ENHANCE,
   "allow for longer -prefix, to include what would be set via -session",
   "Done for P Kohn."
 } ,

 { 12,  Feb, 2015, RCR, "make_stim_times.py", MICRO, TYPE_NEW_OPT,
   "added -no_consec option, to block consecutive events",
   NULL
 } ,

 { 12,  Feb, 2015, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_anaticor_fast/-regress_anaticor_fwhm",
   "This implements the 'fast' ANATICOR method, computing the WMeLocal\n"
   "voxel-wise regressors via an FWHM Gaussian sum of WMe voxels, rather\n"
   "than a uniform sum within a radius."
 } ,

 { 11,  Feb, 2015, RCR, "model_conv_PRF", MICRO, TYPE_MODIFY,
   "use AFNI_MODEL_PRF_RAM_STATS to control RAM use reporting",
   "Maybe malloc_stats() is not available on macs."
 } ,

 { 10,  Feb, 2015, RCR, "make_stim_times.py", MICRO, TYPE_MODIFY,
   "clarify use of both -nruns, -nt",
   NULL
 } ,

 {  9,  Feb, 2015, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "warn on '\\' without preceding space",
   "Gang and J Rajendra ran into a problem on OS X 10.9.5."
 } ,

 {  9,  Feb, 2015, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "applied updates matching 3dClustSim (9 table output)",
   "Output from 3dClustSim is now 9 tables: NN=1,2,3 by 1-,2-,bi-sided tests."
 } ,

 {  3,  Feb, 2015, RCR, "model_conv_PRF", MINOR, TYPE_ENHANCE,
   "consolidate blur and reorg into one function, to reduce max memory usage",
   "This allows the program to run on weaker systems, cutting the max RAM\n"
   "usage by one half.  A prior step was to allocate main RAM early so that\n"
   "free() would release to the OS (Linux), but that is now moot.\n"
   "This change has no effect on the results (no binary diff)."
 } ,

 { 28,  Jan, 2015, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "in mri_fdrize, warn user if p->q is skipped because of < 20 voxels",
   NULL
 } ,

 { 28,  Jan, 2015, RCR, "Makefile.INCLUDE", MICRO, TYPE_MODIFY,
   "take SurfMesh out of SUMA_PROGS; use LC_COLLATE in sort for prog_list",
   "done to keep GTS progs out of macosx_10.6_Intel_64.no.fink build"
 } ,

 { 20,  Jan, 2015, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added option -show_trs_to_zero, to compute length of iresp",
   "This is to computer the number of TRs until a curve bottoms out at 0."
 } ,

 { 20,  Jan, 2015, RCR, "timing_tool.py", MICRO, TYPE_ENHANCE,
   "allow ',' as married timing separator (along with '*')",
   NULL
 } ,

 { 15,  Jan, 2015, RCR, "@update.afni.binaries", MICRO, TYPE_NEW_OPT,
   "added -hist; if unknown opt and cur version, fail after check",
   NULL
 } ,

 { 15,  Jan, 2015, RCR, "afni_skeleton.py", MICRO, TYPE_NEW_PROG,
   "just to save a starting point for other new programs",
   NULL
 } ,

 { 15,  Jan, 2015, RCR, "afni_util.py", MICRO, TYPE_BUG_FIX,
   "fixed () in case of r(A,B,unbiased=1), which matches correlation_p()",
   NULL
 } ,

 { 15,  Jan, 2015, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "include -demean when running 3ddot on masks",
   NULL
 } ,

 { 15,  Jan, 2015, RCR, "3ddot", MICRO, TYPE_ENHANCE,
   "explicitly state 'Pearson' correlation in help",
   NULL
 } ,

 { 14,  Jan, 2015, RCR, "read_matlab_files.py", MINOR, TYPE_NEW_PROG,
   "read and possibly convert MATLAB files to 1D format",
   NULL
 } ,

 {  2,  Jan, 2015, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "added MIN_OUTLIER to example 7",
   NULL
 } ,

 { 18,  Dec, 2014, RCR, "afni_base.py", MICRO, TYPE_ENHANCE,
   "in shell_com:val(), if no stdout but have stderr, display stderr",
   NULL
 } ,

 { 10,  Dec, 2014, RCR, "meica.py", MICRO, TYPE_BUG_FIX,
   "fixed 3dTshift input in case of --no_despike",
   "Thanks to M Plitt for the code fix."
 } ,

 {  2,  Dec, 2014, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "-tlrc_NL_awpy_rm was not being applied",
   NULL
 } ,

 {  2,  Dec, 2014, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "added floatscan to THD_load_nifti for case of double->float conversion",
   "Thanks to M Gregory."
 } ,

 { 25,  Nov, 2014, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "added get_process_depth()",
   "sample use: afni_util.py -print 'get_process_depth()'"
 } ,

 { 21,  Nov, 2014, RCR, "meica.py", MICRO, TYPE_MODIFY,
   "merged -affter into -nwarp in 5 3dNwarpApply calls",
   NULL
 } ,

 { 21,  Nov, 2014, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "-anat_unifize_method none now means to skip, default means to do in AW",
   "Basically, this adds the ability to skip 3dUnifize completely."
 } ,

 { 19,  Nov, 2014, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "do not allow num_suffix to be processed as octal",
   NULL
 } ,

 { 19,  Nov, 2014, RCR, "3dclust", MICRO, TYPE_ENHANCE,
   "clarify -mni in help (do not use if already MNI)",
   NULL
 } ,

 { 10,  Nov, 2014, RCR, "afni", MICRO, TYPE_ENHANCE,
   "added color maps Reds_and_Blues, and _w_Green",
   NULL
 } ,

 {  7,  Nov, 2014, RCR, "mri_nwarp.c", MICRO, TYPE_BUG_FIX,
   "applied update to free temporary warp on behalf of RWC",
   NULL
 } ,

 {  7,  Nov, 2014, RCR, "auto_warp.py", MICRO, TYPE_MODIFY,
   "moved -affter warp to -warp in 3dNwarpApply",
   NULL
 } ,

 {  7,  Nov, 2014, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "moved -affter warp to -warp in 3dNwarpApply",
   "This applies the 22 Oct, 2014 change to 3dNwarpApply."
 } ,

 {  7,  Nov, 2014, RCR, "model_conv_PRF_6", MINOR, TYPE_NEW_PROG,
   "6 parameter population receptive field estimate model",
   "Added sigrat (sigma ratio) and theta parameters.\n"
   "For E Silson and C Baker."
 } ,

 {  4, Nov, 2014, RCR, "CA_EZ_atlas.csh", MICRO, TYPE_MODIFY,
   "with unchecked -help this dumps TT_N27 in current directory",
   "Updated so that 'apearch -update_all_afni_help' does not dump dataset.\n"
   "Updated directly under pub/dist/bin.\n"
   "Should this script even be distributed?"
 } ,

 { 28, Oct, 2014, RCR, "timing_tool.py", MICRO, TYPE_ENHANCE,
   "expanded -help_basis",
   NULL
 } ,

 { 27, Oct, 2014, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "fixed strcmp trap in -sbns; have -sb_num_suffix look for any last integer",
   NULL
 } ,

 { 27, Oct, 2014, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "added 107 face images for 20 years",
   NULL
 } ,

 { 23, Oct, 2014, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "enhanced read_text_file and added shuffle_blocks",
   NULL
 } ,

 { 22, Oct, 2014, RCR, "3dmask_tool", MICRO, TYPE_BUG_FIX,
   "if padding for dilate/erode steps, preserve ijk_to_dicom_real",
   "Thanks to A Kurani for nothing the problem."
 } ,

 { 20, Oct, 2014, RCR, "imcat", MINOR, TYPE_BUG_FIX,
   "z and r: fixed y-padding",
   NULL
 } ,

 { 16, Oct, 2014, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added sort_methods: none, acq_time, default, num_suffix, zposn",
   NULL
 } ,

 {  8, Oct, 2014, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "added -save_errors and more recovery chances, fixed sb_num_suffix app",
   "Stage 3 of sorting broke stage 2 of sorting.\n"
   "Thanks to V Roopchansingh for noting the problem."
 } ,

 { 25, Sep, 2014, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "fixed use of altered add_to_string_list()",
   "The changed usage broke options -drive_afni, -drive_wait and -rt_cmd.\n"
   "Thanks to V Roopchansingh for noting the problem."
 } ,

 { 24, Sep, 2014, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "added -list2 case under -listfunc",
   NULL
 } ,

 { 23, Sep, 2014, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "cat_strings was missing trailing byte",
   "Thanks to Q Li for noting the problem."
 } ,

 { 23, Sep, 2014, RCR, "afni_util.py", MINOR, TYPE_NEW_OPT,
   "added some explicit -help and improved the few existing options",
   NULL
 } ,

 { 22, Sep, 2014, RCR, "3dexample1", MICRO, TYPE_ENHANCE,
   "made mention of 3dToyProg.c",
   NULL
 } ,

 { 22, Sep, 2014, RCR, "SUMA_Makefile_NoDev", MICRO, TYPE_MODIFY,
   "removed ../suma_*.o from clean directive",
   NULL
 } ,

 { 22, Sep, 2014, RCR, "thd_http.c", MICRO, TYPE_MODIFY,
   "changed mktemp() to mkstemp() to get rid of those compile warnings",
   NULL
 } ,

 { 22, Sep, 2014, RCR, "column_cat", MINOR, TYPE_BUG_FIX,
   "fixed implementation of -line, which messed up default operation",
   NULL
 } ,

 { 19, Sep, 2014, RCR, "3dexample1", MINOR, TYPE_NEW_PROG,
   "sample program to multiply a dataset by 2",
   "This is very basic example of reading/processing/writing AFNI datasets."
 } ,

 { 16, Sep, 2014, RCR, "3dmask_tool", MINOR, TYPE_NEW_OPT,
   "added -fill_dirs option, to specify directions for hole filling",
   "Added for D. Glen."
 } ,

 { 15, Sep, 2014, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "apply -prefix for -show_file_type (a dos2unix conversion)",
   NULL
 } ,

 { 10, Sep, 2014, RCR, "Dimon", MICRO, TYPE_ENHANCE,
   "handle num_chan > 1 in GERT_Reco scripts",
   NULL
 } ,

 {  8, Sep, 2014, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "round min dimension to 6 sig bits, then truncate to 3",
   "This helps catch cases where the dimension is just under\n"
   "some fairly 'round' number."
 } ,

 {  8, Sep, 2014, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "num_chan > 1 needs 3D+t ACQ type",
   "Thanks to V Roopchansingh for noting the problem."
 } ,

 {  8, Sep, 2014, RCR, "plug_realtime", MICRO, TYPE_BUG_FIX,
   "fixed free_PCOR_ivoxel_corr function call typos",
   "Thanks to Y Halchenko for noting the problem."
 } ,

 {  4, Sep, 2014, RCR, "r_idisp.o", MICRO, TYPE_MODIFY,
   "cast int64_t to long long to appease printf across multiple systems",
   NULL
 } ,

 {  3, Sep, 2014, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "merged in changes from C Craddock, with alterations",
   "This needs some more work."
 } ,

 {  2, Sep, 2014, RCR, "3dTcat", MICRO, TYPE_ENHANCE,
   "allow @filename format for -tpattern option",
   NULL
 } ,

 { 29, Aug, 2014, RCR, "slow_surf_clustsim.py", MICRO, TYPE_ENHANCE,
   "included blur in all help examples for clarity",
   NULL
 } ,

 { 28, Aug, 2014, RCR, "Dimon", MICRO, TYPE_ENHANCE,
   "test SOP IUID sorting",
   NULL
 } ,

 { 27, Aug, 2014, RCR, "3dcalc", MICRO, TYPE_ENHANCE,
   "applied AFNI_ORIENT for -LPI/-RAI",
   "Requested by Shane M. via the message board."
 } ,

 { 25, Aug, 2014, RCR, "gen_ss_review_table.py", MICRO, TYPE_BUG_FIX,
   "defined oind (for case that does not currently happen)",
   NULL
 } ,

 { 22, Aug, 2014, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -sort_method and -save_details",
   "Using the 'geme_index' sort method allows for real-time sorting\n"
   "of GE multi-echo data, before volumes are sent to 'afni'.\n"
   "Modification made for V Roopchansingh."
 } ,

 { 21, Aug, 2014, RCR, "model_conv_PRF", MICRO, TYPE_GENERAL,
   "minor details added to help output",
   NULL
 } ,

 { 13, Aug, 2014, RCR, "Dimon", MICRO, TYPE_GENERAL,
   "very minor update",
   NULL
 } ,

 { 12, Aug, 2014, RCR, "Dimon", MAJOR, TYPE_MODIFY,
   "this should basically work like the old version",
   "While no major change should be seen, this is an overhaul of\n"
   "the previous version, which should allow for realtime sorting."
 } ,

 { 12, Aug, 2014, RCR, "Dimon1", MINOR, TYPE_GENERAL,
   "Dimon1 is a fork of the previous working version of Dimon",
   "This can be a backup if there are troubles with the new Dimon."
 } ,

 { 12, Aug, 2014, RCR, "afni_system_check.py", MICRO, TYPE_GENERAL,
   "afni -ver is now only 1 line of output",
   NULL
 } ,

 {  5, Aug, 2014, RCR, "to3d", MICRO, TYPE_GENERAL,
   "added more comments about -ushort2float",
   "Requested by J Butman."
 } ,

 {  2, Aug, 2014, RCR, "make_stim_times.py", MINOR, TYPE_NEW_OPT,
   "added -run_trs, for cases when the TRs per run vary",
   "Requested on message board by Rebecca and later by Lisam."
 } ,

 { 15,  Jul, 2014, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "output average motion per stim over response",
   "This will probably be replaced by averages over stimulus only time.\n"
   "Requested by D Pine."
 } ,

 { 15,  Jul, 2014, RCR, "3dClustSim", MICRO, TYPE_GENERAL,
   "check for bad floats read for -fwhm[xyz]",
   "Requested by shanusmagnus."
 } ,

 { 11,  Jul, 2014, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed 1d_tool.py -pad_into_many_runs for bpass w/varying run lengths",
   "Thanks to d6anders for noting the problem."
 } ,

 {  3,  Jul, 2014, RCR, "model_conv_PRF", MINOR, TYPE_BUG_FIX,
   "fixed a name space problem on macs",
   NULL
 } ,

 {  2,  Jul, 2014, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "added model_conv_PRF in Makefile.INCLUDE for distribution",
   NULL
 } ,

 { 27,  Jun, 2014, RCR, "model_conv_PRF", MAJOR, TYPE_NEW_PROG,
   "population receptive field estimate model",
   "For E Silson and C Baker."
 } ,

 { 26,  Jun, 2014, RCR, "3dresample", MINOR, TYPE_NEW_OPT,
   "added -bound_type FOV/SLAB option (FOV is orig and default)",
   "FOV preserves the field of view, SLAB preserves the SLAB\n"
   "(so with SLAB the extents should not change)"
 } ,

 { 26,  Jun, 2014, RCR, "gen_ss_review_table.py", MICRO, TYPE_ENHANCE,
   "track 'degress of freedom' as 'degrees ...'",
   NULL
 } ,

 { 26,  Jun, 2014, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "note any anat/EPI mask correlation value; correct 'degress' as 'degrees'",
   "Typo noted by J Stoddard."
 } ,

 { 26,  Jun, 2014, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "full_mask is now byte (via 3dmask_tool); note correlation with anat mask",
   NULL
 } ,

 { 25,  Jun, 2014, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "removed SUMA/SUMA_MakeColorMap, SUMA/SUMA_MakeConsistent from source tree",
   "Thanks to Y Halchenko for bringing it up."
 } ,

 { 25,  Jun, 2014, RCR, "to3d", MINOR, TYPE_ENHANCE,
   "allow -zorigin with x/y SLAB/FOV, particularly in case of nz==1",
   NULL
 } ,

 {  2,  Jun, 2014, RCR, "slow_surf_clustsim.py", MICRO, TYPE_MODIFY,
   "niter defaults to 1000, to match recommendations and 'quick' example",
   NULL
 } ,

 { 30,  May, 2014, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "if PREFIX ends in .nii, all saved datasets will be in NIFTI format",
   "Added for V Roopchansingh."
 } ,

 { 20,  May, 2014, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "macs: look for PyQt4 from homebrew and fink",
   NULL
 } ,

 { 19,  May, 2014, RCR, "column_cat", MICRO, TYPE_NEW_OPT,
   "added -line option, e.g. to print only (0-based) line 17",
   NULL
 } ,

 { 16,  May, 2014, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "changed default of -anat_unif_GM to no",
   "Use of -GM in 3dUnifiize was leading to some skull stripping failures.\n"
   "Thanks to J Stoddard for noting the problem."
 } ,

 { 16,  May, 2014, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "make space in case compression programs have longer paths",
   "Thanks to D Thompson for finding the problematic code."
 } ,

 { 16,  May, 2014, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "a few updates:",
   "  - if no AFNI binaries in path, try path to ASC.py\n"
   "  - look for history files in data directories\n"
   "  - print comments at end, so they are easier to notice"
 } ,

 { 13,  May, 2014, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "allow for no stats dset",
   "With resting state and 3dTproject, afni_proc.py will not create stats."
 } ,

 { 12,  May, 2014, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_use_tproject, and made the default=yes",
   "This will apply 3dTproject instead of 3dDeconvolve for resting\n"
   "state analysis.  It is much faster, and creates the same result."
 } ,

 { 12,  May, 2014, RCR, "3dTproject", MICRO, TYPE_ENHANCE,
   "allow for multiple -input dataset, without requiring quotes around them",
   NULL
 } ,

 { 12,  May, 2014, RCR, "timing_tool.py", MICRO, TYPE_BUG_FIX,
   "-part_init 0 is not appropriate for -partition",
   "Text labels now apply, and the default is '-part_init INIT'."
 } ,

 {  9,  May, 2014, RCR, "timing_tool.py", MICRO, TYPE_NEW_OPT,
   "added -part_init option; removed -chrono option",
   NULL
 } ,

 {  1,  May, 2014, RCR, "@update.afni.binaries", MINOR, TYPE_BUG_FIX,
   "added -quick option; fixed recursive backups",
   NULL
 } ,

 { 29,  Apr, 2014, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "micro fix to clarify 'initialization' help",
   "Thanks to Ziad for noting it"
 } ,

 { 29,  Apr, 2014, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "update to run number display in case of -multi_timing_to_event_list",
   NULL
 } ,

 { 24,  Apr, 2014, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -multi_timing_to_event_list",
   "This allows one to generate simple or details event lists, or to\n"
   "partition one event class by its predecessors.\n"
   "Partitioning added for W Tseng."
 } ,

 { 24,  Apr, 2014, RCR, "afni_base.py", MICRO, TYPE_MODIFY,
   "shell_exec2() should always set so,se as arrays",
   NULL
 } ,

 { 24,  Apr, 2014, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "changed use of nlines in limited_shell_exec()",
   NULL
 } ,

 { 24,  Apr, 2014, RCR, "afni_history", MICRO, TYPE_BUG_FIX,
   "added proto for restrict_hlist()",
   NULL
 } ,

 { 16,  Apr, 2014, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "added MIN_OUTLIER parameter option for -volreg_base_dset",
   "Using '-volreg_base_dset MIN_OUTLIER' will result in the volume with\n"
   "the minimum outlier fraction to be extracted as the volreg base.\n"
   "Thanks to T Ross for the good suggestion, so long ago"
 } ,

 { 16,  Apr, 2014, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "internal re-org, should have no effect",
   NULL
 } ,

 { 15,  Apr, 2014, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "added optional 'pid' parameter to the get_process_stack() functions",
   NULL
 } ,

 { 10,  Apr, 2014, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "@afni.run.me never made it into Makefile.INCLUDE for distribution",
   NULL
 } ,

 { 10,  Apr, 2014, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -index_to_run_tr, intended for use by afni_proc.py",
   NULL
 } ,

 {  9,  Apr, 2014, RCR, "gen_ss_review_table.py", MAJOR, TYPE_NEW_PROG,
   "parse output from @ss_review_basic text into spreadsheet format",
   "This makes it easy to flag outlier subject values.\n"
   "Thanks to J Jarcho for encouragement."
 } ,

 {  9,  Apr, 2014, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "give priority to GCOR files with 'out' in the name",
   NULL
 } ,

 {  4,  Apr, 2014, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "fixed reading NIFTI obliquity w/dglen (lost Mar 22)",
   "Thanks to P Kundu for noting the problem."
 } ,

 { 31,  Mar, 2014, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -anat_unif_GM (def=yes); improved message for bad ricor input",
   NULL
 } ,

 { 31,  Mar, 2014, RCR, "auto_warp.py", MICRO, TYPE_MODIFY,
   "some help text indentation and fix for display of non-string defaults",
   NULL
 } ,

 { 26,  Mar, 2014, RCR, "auto_warp.py", MINOR, TYPE_BUG_FIX,
   "in 3dNwarpApply, use the base dataset as the -master, rather than WARP",
   "The WARP dataset is now often bigger, to handle warps to the dataset\n"
   "edges.  The result from auto_warp.py should match the template/base.\n"
   "Note: the problem applies to binaries from 3/21 until 3/25 (now).\n"
   "Thanks to V Zachariou for noting the problem."
 } ,

 { 25,  Mar, 2014, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options -anat_uniform_method and -anat_opts_unif",
   "This correction may be particularly useful along with either\n"
   "-tlrc_NL_warp or -mask_segment_anat."
 } ,

 { 24,  Mar, 2014, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_anaticor_radius",
   "This specifies the radius for the local white matter average.\n"
   "Option requested by S Torrisi."
 } ,

 { 21,  Mar, 2014, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "if anaticor and censor, do not use keep_trs for blur est from errts",
   "Thanks to J Stoddard for noting the problem."
 } ,

 { 21,  Mar, 2014, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "removed -e from 'tcsh -ef @ss_review_basic', for grep failures",
   "Macs terminate (correctly) when grep/wc return non-zero status, but\n"
   "Linux systems do not.  Maybe tcsh authors did not like grep killing\n"
   "scripts, either...\n"
 } ,

 { 21,  Mar, 2014, RCR, "afni_system_check.py", MINOR, TYPE_NEW_OPT,
   "added -data_root and enhancements for class data search",
   NULL
 } ,

 { 20,  Mar, 2014, RCR, "1dUpsample", MINOR, TYPE_BUG_FIX,
   "fix reporting of file name in error messages; enhance said messages",
   NULL
 } ,

 { 14,  Mar, 2014, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "added some data and OS-specific tests",
   NULL
 } ,

 { 12,  Mar, 2014, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "set errts_pre in anaticor block; apply extends in blur no scale",
   NULL
 } ,

 { 11,  Mar, 2014, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "added gen_ss_review_scripts.py command comment at bottom of _basic script",
   NULL
 } ,

 { 7,  Mar, 2014, RCR, "afni", MICRO, TYPE_NEW_OPT,
   "added -no_detach, to prevent detaching from the terminal",
   "Useful since -DAFNI_DETACH=NO cannot work as written."
 } ,

 {  6,  Mar, 2014, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_MODIFY,
   "changed some censoring and per-stim behavior",
   "- if censoring, create X.stim.xmat.1D from uncensored matrix\n"
   "- if no censor, still report num regs of interest and TRs per stim\n"
   "- report per-stim censoring only with stim classes"
 } ,

 { 24,  Feb, 2014, RCR, "realtime_receiver.py", MICRO, TYPE_ENHANCE,
   "added a little more detail to the demo example",
   NULL
 } ,

 { 20,  Feb, 2014, RCR, "3dClustSim", MICRO, TYPE_BUG_FIX,
   "break WARNING_message(amesg) up, until W_m gets enhanced",
   "Strings applied via the format are limited to 16K."
 } ,

 { 19,  Feb, 2014, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "if AM2 or IM, terminate extraction of ideals",
   "Ideal extraction should be done via 1d_tool.py, using the X-matrix."
 } ,

 { 19,  Feb, 2014, RCR, "3dDeconvolve", MICRO, TYPE_ENHANCE,
   "warn if GLOBAL times and 1 early stim per run (looks local)",
   "An early stim means t <= (NT_r-1)*TR, where NT_r is #TRs in run r.\n"
   "Negative times are included, as they may be fillers for empty runs."
 } ,

 { 18,  Feb, 2014, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -test_local_timing, to look for local vs. global timing issues",
   "- in some cases, promote married types to combine/compare them\n"
   "- keep track of '*' entries from timing files"
 } ,

 { 18,  Feb, 2014, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "minor help update",
   NULL
 } ,

 { 10,  Feb, 2014, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "show TRs per run, applied and censored",
   NULL
 } ,

 {  6,  Feb, 2014, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "-help examples start with 'Example', for searching",
   NULL
 } ,

 {  3,  Feb, 2014, RCR, "apsearch", MICRO, TYPE_NEW_OPT,
   "added -global_help/-gopts_help to print help for global options",
   NULL
 } ,

 { 15,  Jan, 2014, RCR, "3dLRflip", MICRO, TYPE_BUG_FIX,
   "used bad filename without -prefix",
   "Var ext was not initialized."
 } ,

 { 14,  Jan, 2014, RCR, "3dttest++", MICRO, TYPE_MODIFY,
   "make mask failure message more clear",
   "In THD_create_mask_from_string(), if string is short enough for a file\n"
   "check, report error with entire string."
 } ,

 { 14,  Jan, 2014, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "added more system programs to check",
   NULL
 } ,

 { 30,  Dec, 2013, RCR, "1d_tool.py", MICRO, TYPE_MODIFY,
   "skip polort against polort in -show_cormat_warnings",
   NULL
 } ,

 { 30,  Dec, 2013, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "mcw_malloc.c: moved mcw_malloc_dump_sort below _dump for solaris",
   "Apparently it does not like inconsistent declaration in same file,\n"
   "and mcw_malloc.h does not offer prototypes to many functions in the\n"
   "case of DONT_USE_MCW_MALLOC, including this one."
 } ,

 { 30,  Dec, 2013, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "for -show_bad_backslash, check for '\\' as the last file character",
   "The fix (with -prefix) is to delete the last '\\' and end with a newline."
 } ,

 { 27,  Dec, 2013, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "also output censored TRs per run, along with fractions",
   NULL
 } ,

 { 27,  Dec, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_tr_run_counts and -show_num_runs, for gen_ss_review_scripts.py",
   NULL
 } ,

 { 26,  Dec, 2013, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_MODIFY,
   "max and jump to cluster max are now based on masked dset, if possible",
   NULL
 } ,

 { 26,  Dec, 2013, RCR, "3dBrickStat", MICRO, TYPE_MODIFY,
   "removed extra mask size output when using -mask option",
   "Text output is the intention of the program, so limit to requested text."
 } ,

 { 18,  Dec, 2013, RCR, "@update.afni.binaries", MINOR, TYPE_MODIFY,
   "if system files seem to exist in the abin directory, block update",
   "If AFNI seems to be installed in a system directory (i.e. with OS level\n"
   "programs), default to not letting the update proceed.  Options -sys_ok\n"
   "and -help_system_progs were added to provide control and details."
 } ,

 { 17,  Dec, 2013, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "use -NEW by default with 3dDespike",
   "Added -despike_new to override the default behavior."
 } ,

 { 16,  Dec, 2013, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_BUG_FIX,
   "fixed use of num_trs in the case of censoring",
   "Thanks to K Kerr for nothing the problem."
 } ,

 { 16,  Dec, 2013, RCR, "auto_warp.py", MINOR, TYPE_NEW_OPT,
   "added -qblur option for P Molfese",
   NULL
 } ,

 {  9,  Dec, 2013, RCR, "afni_util.py", MINOR, TYPE_BUG_FIX,
   "added backup function for get_process_stack",
   "BASE.shell_com() might return a short process list, probably from\n"
   "limited buffer space (for cmd.stdout)."
 } ,

 {  4,  Dec, 2013, RCR, "@update.afni.binaries", MINOR, TYPE_BUG_FIX,
   "fixed ac++ condition and empty if",
   NULL
 } ,

 {  4,  Dec, 2013, RCR, "afni_runme", MINOR, TYPE_NEW_PROG,
   "added this (Ziad's) script to sysadmin/scripts",
   NULL
 } ,

 {  3,  Dec, 2013, RCR, "@update.afni.binaries", MINOR, TYPE_NEW_OPT,
   "added -prog_list for Ziad",
   NULL
 } ,

 { 12,  Nov, 2013, RCR, "3dTfitter", MICRO, TYPE_ENHANCE,
   "added help example for PPI analysis",
   NULL
 } ,

 { 5,  Nov, 2013, RCR, "@update.afni.binaries", MICRO, TYPE_BUG_FIX,
   "watch out of 'afni -ver' crashing because of missing libraries",
   "Trap check of $package, since it is included with $status.\n"
   "Thanks to CC Yen for noting the error."
 } ,

 { 5,  Nov, 2013, RCR, "@FindAfniDsetPath", MICRO, TYPE_ENHANCE,
   "check AFNI_ATLAS_PATH and $HOME/.afni/atlases for datasets",
   NULL
 } ,

 { 1,  Nov, 2013, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "OS X now defaults to 10.7_Intel package",
   NULL
 } ,

 { 1,  Nov, 2013, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "let all-1 input for extents mask vary per run (for diff # TRs)",
   NULL
 } ,

 { 31,  Oct, 2013, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "restrict blur estimation to uncensored TRs",
   NULL
 } ,

 { 31,  Oct, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_trs_run",
   "This will be used by afni_proc.py to restrict TRs for blur estimation\n"
   "to those that were not censored, per run."
 } ,

 { 30,  Oct, 2013, RCR, "gen_group_command.py", MINOR, TYPE_ENHANCE,
   "added -keep_dirent_pre, to expand subject ID to directory entry prefix",
   "Requested by  P Molfese."
 } ,

 { 24,  Oct, 2013, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "output global correlation, and DoF info from review_basic",
   NULL
 } ,

 { 17,  Oct, 2013, RCR, "3dDeconvolve", MICRO, TYPE_BUG_FIX,
   "avoid infinite loop on empty SYM: or SYM: rows",
   NULL
 } ,

 { 30,  Sep, 2013, RCR, "unix_tutorial", MINOR, TYPE_ENHANCE,
   "updates to installs/unix_commands/scripts/basic_*/bin/*",
   "These are for the 2 Dec 2013 bootcamp."
 } ,

 { 26,  Sep, 2013, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "added more .h files to install_lib for compiling outside of afni_src",
   "Added rcmat.h, misc_math.h, thd_atlas.h, thd_ttatlas_query.h\n"
   "and thd_ttatlas_CA_EZ.h."
 } ,

 { 19,  Sep, 2013, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "allow regress_polort -1; added help for -regress_RSFC",
   NULL
 } ,

 { 19,  Sep, 2013, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "allow AFNI_COMPRESSOR to init decompression tool between gzip/pigz",
   NULL
 } ,

 { 19,  Sep, 2013, RCR, "afni-general", MINOR, TYPE_NEW_OPT,
   "show label->sub-brick index conversion via AFNI_SHOW_LABEL_TO_INDEX",
   NULL
 } ,

 { 19,  Sep, 2013, RCR, "afni", MINOR, TYPE_NEW_OPT,
   "added options -get_processed_env[_afni] and -global_opts",
   NULL
 } ,

 { 17,  Sep, 2013, RCR, "mpeg_encode", MICRO, TYPE_MODIFY,
   "on fatal error, print message; added stdlib.h for free()/exit() protos",
   "Thanks to TheChymera (Message Board) for mentioning compile warnings."
 } ,

 { 13,  Sep, 2013, RCR, "3dNLfim", MICRO, TYPE_BUG_FIX,
   "report an error instead of crashing if no -input is given",
   "This allows for getting individual signal help without the crash.\n"
 } ,

 { 13,  Sep, 2013, RCR, "model_conv_cosine4", MICRO, TYPE_ENHANCE,
   "updated help with a usage example",
   NULL
 } ,

 { 12,  Sep, 2013, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "added SYSTEM_NAME to Makefile.linux_ubuntu_12_64",
   NULL
 } ,

 { 12,  Sep, 2013, RCR, "afni-general", MINOR, TYPE_GENERAL,
   "added P Taylor's Makefile and install notes",
   "Makefile.linux_ubuntu_12_64, OS_notes.linux_ubuntu_12_64"
 } ,

 { 12,  Sep, 2013, RCR, "afni-general", MINOR, TYPE_GENERAL,
   "added afni_src/other_builds directory",
   "This is for non-AFNI-build Makefiles and OS install notes.\n"
   "It has been initialized with:\n"
   "   Makefile.linux_fedora_19_64\n"
   "   OS_notes.linux_fedora_19_64.txt"
 } ,

 { 11,  Sep, 2013, RCR, "model_conv_cosine4", MAJOR, TYPE_NEW_PROG,
   "A four half-cosine convolvable model.",
   "Based on: Fully Bayesian Spatio-Temporal Modeling of FMRI Data\n"
   "          IEEE Transactions on Medical Imaging,\n"
   "          Volume 23, Issue 2, February 2004, Pages 213-231\n"
   "          Woolrich, M.W., Jenkinson, M., Brady, J.M., Smith, S.M.\n"
   "Requested by C Connolly and Felix."
 } ,

 {  3,  Sep, 2013, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "Dimon -rt: if im_is_volume and single volume, get dz from image",
   "Thanks to A Nilsen for reporting the problem.\n"
 } ,

 { 26,  Aug, 2013, RCR, "afni_system_check.py", MINOR, TYPE_NEW_OPT,
   "added -check_all, -find_prog, -casematch, -exact",
   "These changes are to add PATH searching for programs.\n"
 } ,

 { 20,  Aug, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_RSFS, to run 3dRSFC",
   "Would run 3dRSFC per run on errts, to bandpass and compute parameters."
 } ,

 { 20,  Aug, 2013, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "make 3dAutomask the default EPI strip method",
   "Suggested by D Glen.  I should have done so in the first place."
 } ,

 { 20,  Aug, 2013, RCR, "afni_system_check.py", MICRO, TYPE_ENHANCE,
   "update do search_path_dirs/show_found_in_path",
   NULL
 } ,

 { 14,  Aug, 2013, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "removed '_' from PYTHON_PATH; note any /sw/bin/python* files",
   NULL
 } ,

 { 14,  Aug, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added non-linear standard space registration via -tlrc_NL_warp",
   NULL
 } ,

 {  2,  Aug, 2013, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "check for multiple R and python programs in PATH",
   NULL
 } ,

 {  2,  Aug, 2013, RCR, "3dANOVA", MINOR, TYPE_BUG_FIX,
   "if AFNI_COMPRESSOR and input nii.gz, 'remove()' would not remove BRIK.gz",
   "Thanks to P Molfese for noting the problem."
 } ,

 {  1,  Aug, 2013, RCR, "3dmask_tool", MINOR, TYPE_BUG_FIX,
   "fixed apparent pointer step issue, which happens on only some systems",
   "Apparent problem with MMAP (memory mapping of files).\n"
   "Thanks to W Gaggl for pointing out the problem."
 } ,

 {  1,  Aug, 2013, RCR, "suma-general", MINOR, TYPE_BUG_FIX,
   "in suma_gifti.c, convert GIFTI's LPI to and from AFNI's RAI",
   "Done with Ziad.  Thanks to N Oosterhof for bringing this up."
 } ,

 { 31,  Jul, 2013, RCR, "3dmask_tool", MINOR, TYPE_BUG_FIX,
   "fixed failure to apply a negative dilation in non-convert case",
   "Thanks to W Gaggl for noting the problematic scenario."
 } ,

 { 22,  Jul, 2013, RCR, "nifti_tool", MICRO, TYPE_GENERAL,
   "re-applied 2012 change of originator to shorts (lost with ITK update)",
   NULL
 } ,

 { 19,  Jul, 2013, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "applied ITK compatibility updates from 11/2010 by H Johnson",
   NULL
 } ,

 { 19,  Jul, 2013, RCR, "3dDeconvolve", MICRO, TYPE_ENHANCE,
   "no options implies -h",
   NULL
 } ,

 { 18,  Jul, 2013, RCR, "@move.to.series.dirs", MINOR, TYPE_ENHANCE,
   "added -dprefix option, for output directory prefix",
   NULL
 } ,

 { 16,  Jul, 2013, RCR, "afni_system_check.py", MINOR, TYPE_ENHANCE,
   "added checks for early python versions; added a little help",
   NULL
 } ,

 { 12,  Jul, 2013, RCR, "suma", MICRO, TYPE_MODIFY,
   "return a good status (0) on -help",
   NULL
 } ,

 { 11,  Jul, 2013, RCR, "afni_system_check.py", MINOR, TYPE_NEW_PROG,
   "perform many checks to validate a system for AFNI use",
   NULL
 } ,

 {  9,  Jul, 2013, RCR, "Dimon", MINOR, TYPE_ENHANCE,
   "if unsigned shorts are detected, add -ushort2float to to3d command",
   NULL
 } ,

 {  9,  Jul, 2013, RCR, "to3d", MINOR, TYPE_NEW_OPT,
   "added -ushort2float, for converting unsinged shorts to floats",
   "Requested by D Handwerker."
 } ,

 {  9,  Jul, 2013, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "added more info for locating bad chars with -test",
   NULL
 } ,

 {  7,  Jul, 2013, RCR, "@Install_FATCAT_DEMO", MINOR, TYPE_NEW_PROG,
   "replaces @Install_PTaylor_TractDemo",
   NULL
 } ,

 {  6,  Jul, 2013, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "use NIFTI_INTENT_NONE for case of intent_code = FUNC_BUCK_TYPE",
   "3dbucket's FUNC_BUCK_TYPE went to intent_code for 1 vol dset"
 } ,

 {  1,  Jul, 2013, RCR, "afni-general", MICRO, TYPE_NEW_OPT,
   "added AFNI_INCLUDE_HISTORY: set to No to omit history from output",
   NULL
 } ,

 { 28,  Jun, 2013, RCR, "afni_util.py", MINOR, TYPE_NEW_OPT,
   "added get/show_process_stack(), get/show_login_shell()",
   "Can use these from command line, e.g. :\n"
   "   afni_util.py -eval 'show_login_shell()'\n"
   "   afni_util.py -eval 'show_login_shell(verb=1)'\n"
   "   afni_util.py -eval 'show_process_stack()'"
 } ,

 { 27,  Jun, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_mot_as_ort",
   "Applies motion regressors via -ortvec, a potential future change."
 } ,

 { 25,  Jun, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -volreg_motsim and -volreg_opts_ms",
   NULL
 } ,

 { 14,  Jun, 2013, RCR, "Makefile.NIH.openSUSE.11.4_64", MICRO, TYPE_ENHANCE,
   "added -fPIC to CCMIN (-fPIC is all over now, basically for R_io.so)",
   NULL
 } ,

 { 10,  Jun, 2013, RCR, "1d_tool.py", MINOR, TYPE_ENHANCE,
   "added -select_groups, -show_cormat, -volreg2allineate",
   NULL
 } ,

 { 10,  Jun, 2013, RCR, "@simulate_motion", MINOR, TYPE_ENHANCE,
   "added warp_methods, etc.",
   NULL
 } ,

 { 31, May, 2013, RCR, "@simulate_motion", MAJOR, TYPE_NEW_PROG,
   "program to create time series simulated by motion parameters",
   NULL
 } ,

 { 17, May, 2013, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "added -f to curl, so that failures propagate to $status",
   NULL
 } ,

 { 14, May, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added options -show_argmin/max",
   NULL
 } ,

 { 13, May, 2013, RCR, "@RenamePanga", MINOR, TYPE_BUG_FIX,
   "added -column to count commands writing listfile",
   "There is a 4096 byte limit in addto_args(), which could be made dynamic."
 } ,

 { 10, May, 2013, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "named glob functions as afni_*; R was using sys funcs, rather than local",
   NULL
 } ,

 {  9, May, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options -write_3dD_script and -write_3dD_prefix",
   NULL
 } ,

 {  8, May, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added options -rank, -rank_style, -reverse_rank",
   NULL
 } ,

 {  6, May, 2013, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "added -regress_anaticor example; opt implies -mask_segment_anat/_erode",
   NULL
 } ,

 {  6, May, 2013, RCR, "3dinfo", MINOR, TYPE_NEW_OPT,
   "added option -slice_timing",
   NULL
 } ,

 {  6, May, 2013, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added option -transpose_write",
   NULL
 } ,

 {  3, May, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options -regress_anaticor and -mask_segment_erode",
   "Use the -regress_anaticor option to regress the WMeLocal time series.\n"
   "This is the ANATICOR method of HJ Jo."
 } ,

 {  1, May, 2013, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "added -help example for -show_trs_uncensored",
   NULL
 } ,

 { 29, Apr, 2013, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "set AFNI_NO_OBLIQUE_WARNING in scripts",
   NULL
 } ,

 { 26, Apr, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_trs_censored/_uncensored (mostly for X-matrix datasets)",
   NULL
 } ,

 { 24, Apr, 2013, RCR, "@move.to.series.dirs", MINOR, TYPE_NEW_PROG,
   "partition a list of DICOM files by series number",
   "Done for I Shapira."
 } ,

 { 24, Apr, 2013, RCR, "3dinfo", MINOR, TYPE_BUG_FIX,
   "allow -space for nifti; actually exit if -view and result exists",
   "Thanks to I Schwabacher for noting the problem and fix."
 } ,

 { 24, Apr, 2013, RCR, "@2dwarper.Allin", MINOR, TYPE_BUG_FIX,
   "did not set 'ver' before goto START",
   "Thanks to I Schwabacher for noting the problem and fix."
 } ,

 { 24, Apr, 2013, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added -censor_next_TR",
   "Sticking with backward diff for deriv, as it makes sense for censoring."
 } ,

 { 23, Apr, 2013, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "added eroded ROIs for -regress_ROI: WMe, GMe, CSFe",
   NULL
 } ,

 { 22, Apr, 2013, RCR, "auto_warp.py", MINOR, TYPE_GENERAL,
   "modified afni_base.afni_name.new() with 2 cases of parse_pref=1",
   "This is currently the only application of that parameter."
 } ,

 { 17, Apr, 2013, RCR, "3dAFNItoNIFTI", MINOR, TYPE_BUG_FIX,
   "fixed old use of use of strcat() after strdup()",
   "Thanks to B Benson and J Stoddard for noting the problem."
 } ,

 { 16, Apr, 2013, RCR, "3dmaskave", MINOR, TYPE_NEW_OPT,
   "added -sumsq (sum squares) and -enorm (Euclidean norm) options",
   NULL
 } ,

 { 16, Apr, 2013, RCR, "3dmaxima", MINOR, TYPE_BUG_FIX,
   "modernize dataset coordinate reporting, using proper signs",
   "Thanks to G Pagnoni for reporting the issue."
 } ,

 { 15, Apr, 2013, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "added RESTING STATE NOTE to help",
   NULL
 } ,

 { 15, Apr, 2013, RCR, "3dSurf2Vol", MICRO, TYPE_BUG_FIX,
   "fixed crash when a surface was not found (struct init)",
   "Thanks to H Yang for noting the problem."
 } ,

 {  9, Apr, 2013, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed computed fitts for REML case (was from 3dDeconvolve)",
   "Thanks to G Pagnoni for noting the problem."
 } ,

 {  5, Apr, 2013, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "added Help web link to class handouts",
   NULL
 } ,

 {  5, Apr, 2013, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "revert -save_orig_skullstrip to -save_skullstrip",
   "This should have no effect on results, except for rename of anat_strip\n"
   "to anat_ns.  It also gets around a temporary name change from AEA.py."
 } ,

 { 27, Mar, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_group_labels, -label_prefix_keep/_drop",
   "Option -label_prefix_drop can be used to remove bandpass regs for 3dRSFC."
 } ,

 {  8, Mar, 2013, RCR, "3dTcat", MINOR, TYPE_NEW_OPT,
   "added -TR and -tpattern options",
   NULL
 } ,

 {  7, Mar, 2013, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "handle -prefix and -overwrite for -show_bad_backslash",
   "The combination can be used to 'fix' bad files."
 } ,

 { 27, Feb, 2013, RCR, "python-general", MICRO, TYPE_NEW_OPT,
   "added Ziad's apsearch global options: -all_opts, -h_find, -h_view",
   NULL
 } ,

 { 21, Feb, 2013, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "small help update to include tshift block in example 5c",
   "Thanks to J Gonzalez bringing it up."
 } ,

 { 14, Feb, 2013, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "handle surface data in -move_preproc_files",
   "Thanks to P Molfese for reporting the error."
 } ,

 { 13, Feb, 2013, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "inform user of subj_dir when writing AP command",
   NULL
 } ,

 { 13, Feb, 2013, RCR, "unix_tutorial", MAJOR, TYPE_NEW_PROG,
   "added tutorial to CVS tree, with processed files under AFNI_data6",
   NULL
 } ,

 { 12, Feb, 2013, RCR, "afni_util.py", MICRO, TYPE_BUG_FIX,
   "updated duplicate dataset error message to match older code updates",
   "Thanks to HJ Jo for reporting the error."
 } ,

 { 12, Feb, 2013, RCR, "@update.afni.binaries", MICRO, TYPE_BUG_FIX,
   "if 'afni -ver' fails from libraries and $status not set, check $package",
   NULL
 } ,

 { 11, Feb, 2013, RCR, "file_tool", MICRO, TYPE_ENHANCE,
   "help updates",
   NULL
 } ,

 {  5, Feb, 2013, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "updates to the help introduction",
   NULL
 } ,

 {  5, Feb, 2013, RCR, "python-general", MICRO, TYPE_BUG_FIX,
   "fixed (unused) cols_by_label_list functions",
   "Fix by I Schwabacher, who is actually using the function."
 } ,

 { 31, Jan, 2013, RCR, "uber_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed blist error that had not been converted to bdict",
   "Thanks to Piero C. for reporting the error."
 } ,

 { 30, Jan, 2013, RCR, "python-general", MINOR, TYPE_ENHANCE,
   "added less biased correlations and various gcor utility functions",
   NULL
 } ,

 { 24, Jan, 2013, RCR, "Dimon", MINOR, TYPE_ENHANCE,
   "be able to process a run of AFNI volumes (-file_type AFNI)",
   "added for Der-Yow Chen and Cecil Yen"
 } ,

 { 24, Jan, 2013, RCR, "3dinfo", MICRO, TYPE_MODIFY,
   "get -orient output via new THD_fill_orient_str_3",
   NULL
 } ,

 { 22, Jan, 2013, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -file_type, in prep for reading AFNI/NIfTI images",
   NULL
 } ,

 { 18, Jan, 2013, RCR, "@compute_gcor", MINOR, TYPE_NEW_PROG,
   "compute GCOR = global correlation of a dataset",
   NULL
 } ,

 { 18, Jan, 2013, RCR, "3dDeconvolve", MICRO, TYPE_BUG_FIX,
   "when jobs=1, only warn for -virtvec if the option was used",
   "The result was just a warning which did not affect processing.\n"
   "Thanks to J Britton and E Ronkin for reporting the warning."
 } ,

 { 16, Jan, 2013, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added option -show_gcor (and _all and _doc)",
   "compute GCOR (average correlation) on 1D files"
 } ,

 { 16, Jan, 2013, RCR, "realtime_receiver.py", MINOR, TYPE_NEW_OPT,
   "added option -dc_params",
   "To go with new scripts AFNI_data6/realtime.demos/demo.2.fback.*."
 } ,

 { 9, Jan, 2013, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added option -regress_compute_gcor",
   "If errts and EPI mask exist, GCOR is computed by default."
 } ,

 { 7, Jan, 2013, RCR, "3dTstat", MINOR, TYPE_NEW_OPT,
   "added option -l2norm, to compute L2 norm",
   NULL
 } ,

 { 2, Jan, 2013, RCR, "3dCM", MICRO, TYPE_BUG_FIX,
   "in THD_cmass(), if mask is NOT set, clear data value",
   "Found with dglen.  This is an old bug, ick."
 } ,

 { 31, Dec, 2012, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "is_in_labels(): search for longest match",
   "To fix failure in the case of both label and labelSUFFIX existing."
 } ,

 { 28, Dec, 2012, RCR, "suma-general", MICRO, TYPE_BUG_FIX,
   "mri_polyfit() now takes exar paramter, pass NULL",
   NULL
 } ,

 { 21, Dec, 2012, RCR, "@update.afni.binaries", MINOR, TYPE_BUG_FIX,
   "change check for recur download by looking for known string in script",
   "Thanks to S Lowell for reporting the error."
 } ,

 { 20, Dec, 2012, RCR, "uber_subject.py", MINOR, TYPE_BUG_FIX,
   "remove -volreg_tlrc_warp in case of no tlrc block",
   "Thanks to P Taylor for reporting the error."
 } ,

 { 19, Dec, 2012, RCR, "afni_restproc.py", MINOR, TYPE_GENERAL,
   "Update from Rayus for handling .nii files.",
   NULL
 } ,

 { 18, Dec, 2012, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "have THD_subbrick_minmax fall back to THD_slow_minmax_dset if no STAT",
   "This is a fix for 3dSkullStrip on NIfTI dsets.\n"
   "Thanks to kelvin for reporting the error."
 } ,

 { 5, Dec, 2012, RCR, "serial_helper", MICRO, TYPE_GENERAL,
   "added useless string specifier in snprintf to block compier warnings",
   "Requested by Y Halchenko."
 } ,

 { 29, Nov, 2012, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "add -f to 'tcsh -c' for getting output from commands",
   "Thanks to P Molfese for the suggestion to avoid .cshrc text output."
 } ,

 { 26, Nov, 2012, RCR, "align_epi_anat.py", MINOR, TYPE_NEW_OPT,
   "added -save_script option",
   "added script history in afni_com class"
 } ,

 { 15, Nov, 2012, RCR, "3dTqual", MINOR, TYPE_NEW_OPT,
   "added -mask option",
   "Requested by evangelou."
 } ,

 { 14, Nov, 2012, RCR, "make_random_timing.py", MINOR, TYPE_BUG_FIX,
   "fixed check for random space in -max_consec case",
   "Thanks to Kristina for reporting the error."
 } ,

 { 13, Nov, 2012, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "fixed fopen_maybe to check for .1D suffix on file streams",
   "Suffix might get added by EDIT_dset_items.\n"
   "Thanks to I Schwabacher for reporting the error."
 } ,

 { 23, OCT, 2012, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "forgot to leave show_size_n_offset set",
   "Thanks to J Jarcho for reporting the to3d failure."
 } ,

 { 19, OCT, 2012, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "added test for BOM bytes (byte order mark)",
   NULL
 } ,

 { 18, OCT, 2012, RCR, "file_tool", MINOR, TYPE_ENHANCE,
   "added convenience option -test",
   NULL
 } ,

 { 18, OCT, 2012, RCR, "uber_ttest.py", MINOR, TYPE_BUG_FIX,
   "small updates to correspond with library changes",
   NULL
 } ,

 { 17, OCT, 2012, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "removed unneeded -set_tr from 1d_tool.py -censor_motion",
   NULL
 } ,

 { 17, OCT, 2012, RCR, "dicom_hdr", MINOR, TYPE_NEW_OPT,
   "added -no_length option, which helps when running diffs on output",
   NULL
 } ,

 { 16, OCT, 2012, RCR, "uber_subject.py", MINOR, TYPE_NEW_OPT,
   "added analysis type and processing block list",
   NULL
 } ,

 { 12, OCT, 2012, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "included tshift block in example #9 - resting state analysis",
   "Thanks to D Drake for reminding me to add it."
 } ,

 { 12, OCT, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "added byte-swapping for complex numbers in thd_niml.c",
   NULL
 } ,

 { 12, OCT, 2012, RCR, "@RetinoProc", MICRO, TYPE_BUG_FIX,
   "set AEA_opt in quotes, as it might be a list",
   NULL
 } ,

 {  5, OCT, 2012, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added option -quick_censor_count",
   NULL
 } ,

 {  5, OCT, 2012, RCR, "dicom_hinfo", MINOR, TYPE_NEW_OPT,
   "added option -no_name",
   NULL
 } ,

 {  3, OCT, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "dashed parameters are now illegal for many options in many python programs",
   "Affects programs:\n"
   "   1d_tool.py, afni_proc.py, gen_group_command.py, make_random_timing.py,\n"
   "   make_stim_times.py, option_list.py, timing_tool.py"
 } ,

 {  2, OCT, 2012, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "added stim_type column to stim table",
   "This corresponds to the afni_proc.py option -regress_stim_types."
 } ,

 {  2, OCT, 2012, RCR, "model_conv_diffgamma", MICRO, TYPE_GENERAL,
   "small help update",
   NULL
 } ,

 {  1, OCT, 2012, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "added 'file' to list of -stim_types parameters",
   "The 'file' type would imply -stim_file in 3dDeconvolve, not timing."
 } ,

 { 26, SEP, 2012, RCR, "@update_afni_binaries", MINOR, TYPE_NEW_OPT,
   "added -apsearch; verify download for recursive step",
   NULL
 } ,

 { 26, SEP, 2012, RCR, "nifti_tool", MINOR, TYPE_BUG_FIX,
   "changed ana originator field from char to short",
   NULL
 } ,

 { 25, SEP, 2012, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "use errts_REML to compute blur if 3dD_stop; apply compute_fitts if no reml",
   "Thanks to P Molfese for reporting the problem."
 } ,

 { 21, SEP, 2012, RCR, "3dNLfim", MICRO, TYPE_GENERAL,
   "added ConvDiffGam to help",
   NULL
 } ,

 { 20, SEP, 2012, RCR, "3dClustSim", MICRO, TYPE_GENERAL,
   "added a note to the help about computing blur estimates",
   "Requested by J Weisberg."
 } ,

 { 20, SEP, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "added some projection function to python libraries",
   NULL
 } ,

 { 13, SEP, 2012, RCR, "@update.afni.binaries", MINOR, TYPE_ENHANCE,
   "download and run the current version on the web site",
   "Good idea, Bob."
 } ,

 { 13, SEP, 2012, RCR, "afni_util", MINOR, TYPE_ENHANCE,
   "can call list functions via -listfunc (to avoid input formatting)",
   "Also, use -join after the LISTFUNC to remove list format on output, e.g.\n"
   "\ncd AFNI_data6\n"
   "afni_util.py -listfunc list_minus_glob_form -join group_results/OLSQ*.HEAD"
 } ,

 { 6, SEP, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "if varying facs/types on NIfTI write, write floats instead of failing",
   NULL
 } ,

 { 6, SEP, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "print missing xmat error w/out debug, as it is fatal",
   NULL
 } ,

 { 6, SEP, 2012, RCR, "afni-general", MINOR, TYPE_NEW_OPT,
   "apply global opt -pad_to_node when going through AFNI format in thd_niml.c",
   NULL
 } ,

 { 4, SEP, 2012, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added option -regress_ROI",
   "This allows for tissue-based regression, with ROI averages from any of:\n"
   "     brain (from full_mask), GM, WM and CSF (from Classes_resam)\n"
   "The 'mask' block is required for all ROIs, and option -mask_segment_anat\n"
   "is required for the latter 3."
 } ,

 { 4, SEP, 2012, RCR, "gen_group_command.py", MICRO, TYPE_BUG_FIX,
   "fixed error message in case of different group sizes",
   "Error pointed out by Priyank."
 } ,

 { 31, AUG, 2012, RCR, "3dTstat", MINOR, TYPE_NEW_OPT,
   "added option -signed_absmax",
   "Requested by P Hamilton."
 } ,

 { 30, AUG, 2012, RCR, "1d_tool.py", MICRO, TYPE_MODIFY,
   "display -show_mmms output to 4 places",
   NULL
 } ,

 { 24, AUG, 2012, RCR, "column_cat", MAJOR, TYPE_NEW_PROG,
   "like 'cat', except horizontally (see recent Unix command, 'paste')",
   NULL
 } ,

 { 23, AUG, 2012, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_NEW_OPT,
   "can pass -censor_dset",
   NULL
 } ,

 { 21, AUG, 2012, RCR, "slow_surf_clustsim.py", MICRO, TYPE_NEW_OPT,
   "added 'sigma' uvar, for passing to SurfSmooth",
   NULL
 } ,

 { 17, AUG, 2012, RCR, "3dGroupInCorr", MICRO, TYPE_BUG_FIX,
   "pass 'batch mode' var to SUMA_init_GISET_setup to preserve dset",
   NULL
 } ,

 { 16, AUG, 2012, RCR, "gen_group_command.py", MICRO, TYPE_MODIFY,
   "show datasets names when a 'labels not unique' error occurs",
   NULL
 } ,

 { 14, AUG, 2012, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "match default class order for 3dSeg; copy labeltable into resampled dset",
   NULL
 } ,

 { 10, AUG, 2012, RCR, "afni_restproc.py", MINOR, TYPE_GENERAL,
   "Updates from Rayus.",
   "Fixed bugs with -outcensor and -snr.\n"
   "Added -bpassregs and -keepuncensored.\n"
   "Use variable detrending for -tsnr."
 } ,

 {  9, AUG, 2012, RCR, "afni_general", MICRO, TYPE_BUG_FIX,
   "definition after ENTRY in mri_genalign_util.c",
   NULL
 } ,

 {  8, AUG, 2012, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -use_slice_loc; fixed app of use_last_elem in mri_read_dicom",
   "g_info.use_last_elem has usurped the lone global"
 } ,

 {  8, AUG, 2012, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "do not update tlrc anat with strip if passed in",
   NULL
 } ,

 {  8, AUG, 2012, RCR, "slow_surf_clustsim.py", MICRO, TYPE_BUG_FIX,
   "currently need to pass -sv even for -on_surface; get rid of this later",
   NULL
 } ,

 { 31, JUL, 2012, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "have -mask_segment_anat default to no (libgsl is not quite so common)",
   NULL
 } ,

 { 31, JUL, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "speed up reading NIfTI files with long histories (THD_dblkatr_from_niml)",
   "Thanks to J Gonzalez for reporting the problem."
 } ,

 { 31, JUL, 2012, RCR, "3dresample", MINOR, TYPE_BUG_FIX,
   "update IJK_TO_DICOM and _REAL at end of resample library function",
   "Thanks to I Schwabacher for reporting the IJK_TO_DICOM discrepancy."
 } ,

 { 30, JUL, 2012, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "if surface analysis, create run_suma script",
   NULL
 } ,

 { 30, JUL, 2012, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_mmms",
   "Display min, mean, max, stdev of each column."
 } ,

 { 26, JUL, 2012, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -mask_segment_anat and -mask_rm_segsy",
   "If anat is stripped, create segmented anat unless user says not to."
 } ,

 { 26, JUL, 2012, RCR, "3dttest++", MINOR, TYPE_BUG_FIX,
   "K text columns (after label) would result in K lost float columns",
   "Thanks to Phoebe of Harvard for reporting the problem."
 } ,

 { 26, JUL, 2012, RCR, "realtime_receiver.py", MINOR, TYPE_NEW_OPT,
   "added -show_comm_times option to show communication times",
   "Added for J Evans (and to get it off an ancient todo list)."
 } ,

 { 23, JUL, 2012, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "allow programs to read auto-tcat datasets using filelist:DSETS.txt",
   "If DSETS.txt contains a list of datasets, they will be read in using\n"
   "THD_open_tcat(), as if they were listed separated by spaces.\n\n"
   "Added for C Connolly."
 } ,

 { 20, JUL, 2012, RCR, "apsearch", MICRO, TYPE_MODIFY,
   "exclude README.* from program list",
   "Executable README files can be troublesome..."
 } ,

 { 17, JUL, 2012, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_MODIFY,
   "added checks for volreg and uncensored X-mat; get view from volreg",
   NULL
 } ,

 { 17, JUL, 2012, RCR, "slow_surf_clustsim.py", MICRO, TYPE_MODIFY,
   "removed -Niter opt from SurfSmooth (let it decide)",
   NULL
 } ,

 { 11, JUL, 2012, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "fill gaps and holes in anatomical masks",
   "(now requires AFNI from 7 May, 2012)"
 } ,

 { 10, JUL, 2012, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "let the user know whether 3dClustSim will be run",
   NULL
 } ,

 { 9, JUL, 2012, RCR, "@auto_tlrc", MICRO, TYPE_MODIFY,
   "escape (unalias) every 'rm' command",
   NULL
 } ,

 { 9, JUL, 2012, RCR, "align_epi_anat.py", MICRO, TYPE_MODIFY,
   "escape (unalias) every 'rm' command",
   NULL
 } ,

 { 29, JUN, 2012, RCR, "Makefile.INCLUDE", MICRO, TYPE_MODIFY,
   "moved ptaylor_install dependency from 'vastness' to 'install'",
   NULL
 } ,

 { 29, JUN, 2012, RCR, "prompt_user", MICRO, TYPE_NEW_OPT,
   "if MESSAGE is '-', read from stdin",
   NULL
 } ,

 { 29, JUN, 2012, RCR, "@Install_RSFMRI_Motion_Group_Demo", MINOR, TYPE_NEW_PROG,
   "program is for installing demo of RSFMR on big and small motion groups",
   NULL
 } ,

 { 28, JUN, 2012, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed help error regarding IM",
   "Thanks to I Blair for reporting it."
 } ,

 { 27, JUN, 2012, RCR, "3dTstat", MICRO, TYPE_NEW_OPT,
   "added -nzmedian, requested on message board",
   NULL
 } ,

 { 25, JUN, 2012, RCR, "gen_group_command.py", MICRO, TYPE_ENHANCE,
   "added help for -factors and 3dANOVA3 -type 4 examples",
   NULL
 } ,

 { 25, JUN, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "fixed uninitialized cpad1,2 in the case of no censoring",
   NULL
 } ,

 { 22, JUN, 2012, RCR, "gen_group_command.py", MINOR, TYPE_NEW_OPT,
   "added commands 3dANOVA2 and 3dANOVA3; added option -factors",
   "Need to add help for -factors (i.e. for 3dANOVA3 -type 4)."
 } ,

 { 20, JUN, 2012, RCR, "plug_realtime", MICRO, TYPE_BUG_FIX,
   "comment out plot_ts_setthik() type calls for now",
   "When registering, finalize_dset() will result in white image window.\n"
   "Thanks to V Roopchansingh for reporting the problem."
 } ,

 { 15, JUN, 2012, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "make num_dim violation a warning, because of mris_convert",
   NULL
 } ,

 { 15, JUN, 2012, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_censor_extern",
   NULL
 } ,

 { 14, JUN, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "use afni -com instead of plugout_drive (for case of multiple users)",
   "Thanks to V Razdan and N Adleman for reporting the issue."
 } ,

 { 6, JUN, 2012, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "look for input of EPI datasets in standard space and NIfTI format",
   NULL
 } ,

 { 5, JUN, 2012, RCR, "3dmask_tool", MICRO, TYPE_BUG_FIX,
   "need to explicitly set DSET_BRICK_TYPE() on some systems",
   NULL
 } ,

 { 5, JUN, 2012, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "warn users if married types and files do not seem to match",
   NULL
 } ,

 { 3, JUN, 2012, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "for resting state analysis, suggest -regress_censor_motion 0.2",
   "Suggest a more strict limit for resting state than for task analysis."
 } ,

 { 3, JUN, 2012, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "for variable updates: actually show list if it is short enough",
   NULL
 } ,

 { 25, MAY, 2012, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "display modified options and subject defaults",
   NULL
 } ,

 { 22, MAY, 2012, RCR, "uber_subject.py", MINOR, TYPE_NEW_OPT,
   "added regress_bandpass and regress_mot_deriv (probably for resting state)",
   NULL
 } ,

 { 21, MAY, 2012, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added the long-desired-but-not-so-needed -regress_stim_types option",
   "This allows users to specify -stim_times/_AM1/_AM2/_IM."
 } ,

 { 19, MAY, 2012, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "added help examples for resting state analysis",
   NULL
 } ,

 { 19, MAY, 2012, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "allow for auto-tcat of 1D inputs that are separated by spaces",
   "For E Demir to use in 3dDeconovolve."
 } ,

 { 16, MAY, 2012, RCR, "@GetAfniOrient", MICRO, TYPE_MODIFY,
   "suppress 3dinfo version text",
   NULL
 } ,

 { 16, MAY, 2012, RCR, "@auto_tlrc", MICRO, TYPE_MODIFY,
   "do not ask for user input, even if centers are off by 80+ mm",
   NULL
 } ,

 { 11, MAY, 2012, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "also output average censored per-TR motion",
   NULL
 } ,

 { 10, MAY, 2012, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "allow processing of more than 99 runs",
   NULL
 } ,

 { 10, MAY, 2012, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "allow for a wider range of file names",
   "- handle case of more than 99 runs\n"
   "- look for files of the form *_rall.1D, as well as *.rall.1D"
 } ,

 { 7, MAY, 2012, RCR, "3dmask_tool", MINOR, TYPE_ENHANCE,
   "replaced THD_mask_erode with new THD_mask_erode_sym",
   "This change should make dilate and erosion operations symmetric."
 } ,

 { 7, MAY, 2012, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added weighted_enorm method for -collapse_cols; added -weight_vec",
   NULL
 } ,

 { 4, MAY, 2012, RCR, "afni_restproc.py", MINOR, TYPE_BUG_FIX,
   "submitting updates from Rayus",
   "Updates are in changelog."
 } ,

 { 3, MAY, 2012, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -backward_diff and -forward_diff",
   "Note, -backward_diff is the same as -derivative."
 } ,

 { 2, MAY, 2012, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "added AFNI_PATH_SPACES_OK, for input of datasets with spaces in path",
   "Added for V Roopchansingh."
 } ,

 { 1, MAY, 2012, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "added -prefix option; added censor coloring to 1dplot commands",
   NULL
 } ,

 { 1, MAY, 2012, RCR, "1d_tool.py", MINOR, TYPE_ENHANCE,
   "added -looks_like_AM",
   NULL
 } ,

 { 1, MAY, 2012, RCR, "make_random_timing.py", MINOR, TYPE_ENHANCE,
   "allowed -ordered_stimuli and -max_consec, together",
   "Requested by Liat."
 } ,

 { 30, APR, 2012, RCR, "afni_restproc.py", MAJOR, TYPE_NEW_PROG,
   "this program is by Rayus Kuplicki, please contact him for information",
   NULL
 } ,

 { 27, APR, 2012, RCR, "3dmask_tool", MAJOR, TYPE_NEW_PROG,
   "a program to manipulate mask datasets",
   NULL
 } ,

 { 24, APR, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "if surface data with generic prefix, append surf-type suffix",
   "Done to fix ANOVA commands on surface.\n"
   "Thanks to R Ray for bringing this up."
 } ,

 { 17, APR, 2012, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "added atlas/ROI label use with <> range selectors (MCW_get_angle_range)",
   NULL
 } ,

 { 16, APR, 2012, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -regress_bandpass, for bandpass filtering via regression",
   NULL
 } ,

 { 13, APR, 2012, RCR, "@radial_correlate.py", MICRO, TYPE_ENHANCE,
   "accept +tlrc datasets",
   NULL
 } ,

 { 12, APR, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "backport to python 2.2",
   NULL
 } ,

 { 12, APR, 2012, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "backport to python 2.2",
   "For files that should work on 2.2, avoid sum() and enumerate().\n"
   "Thanks to L Broster for reporting problems on python 2.2."
 } ,

 { 8, APR, 2012, RCR, "make_random_timing.py", MINOR, TYPE_ENHANCE,
   "-ordered_stimuli now takes labels",
   NULL
 } ,

 { 4, APR, 2012, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "if prefix shows STORAGE_UNDEFINED, use BRIK only if not potential surface",
   NULL
 } ,

 { 3, APR, 2012, RCR, "plug_realtime", MICRO, TYPE_MODIFY,
   "always print the name of the mask dataset in use (via GUI or env)",
   NULL
 } ,

 { 30, MAR, 2012, RCR, "plug_realtime", MICRO, TYPE_MODIFY,
   "let user know when AFNI_REALTIME_Mask_Dset is applied",
   NULL
 } ,

 { 30, MAR, 2012, RCR, "@auto_tlrc", MICRO, TYPE_NEW_OPT,
   "added option -inweight\n",
   "Added for S Horovitz and S Tinaz."
 } ,

 { 22, MAR, 2012, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "apply AFNI_REALTIME_SHOW_TIMES in non-RT feedback case",
   NULL
 } ,

 { 22, MAR, 2012, RCR, "Dimon", MICRO, TYPE_ENHANCE,
   "if RT comm fails with afni, show iochan_error_string()",
   "It occasionally fails at TR=0.125 s."
 } ,

 { 21, MAR, 2012, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "use run_lengths for TR list; removed path from external motion file",
   NULL
 } ,

 { 21, MAR, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "look for more motion files; minor changes to output format",
   NULL
 } ,

 { 21, MAR, 2012, RCR, "3dcalc", MICRO, TYPE_ENHANCE,
   "added -help description to -help output",
   "To get apsearch to enable <tab> completion of -help option."
 } ,

 { 15, MAR, 2012, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "added AFNI_REALTIME_Mask_Dset for per-run control over Mask",
   "Also added some missing vars to README.environment.\n"
   "Done for J Evans."
 } ,

 { 14, MAR, 2012, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -num_chan and -max_quiet_trs; default sleep = 1.1*TR, max of 2",
   "Added for J Evans and V Roopchansingh."
 } ,

 { 14, MAR, 2012, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "test for global timing before local, as it looks like bad local",
   "Thanks to P Pallett for reporting the problem."
 } ,

 { 13, MAR, 2012, RCR, "lib_qt_gui.py", MICRO, TYPE_MODIFY,
   "has main, so added trivial -help option",
   NULL
 } ,

 { 12, MAR, 2012, RCR, "@SUMA_AlignToExperiment", MICRO, TYPE_NEW_OPT,
   "added -overwrite_resp, so that processing does not have to stop and wait",
   "Also, used 'find' grab *.nii, to fix the failure reported by R Ray.\n"
   "Forgot to put this in with the afni_proc.py change..."
 } ,

 {  9, MAR, 2012, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "added $hemi to rm.mean dset during scaling; added -overwrite_resp to SATE",
   "Surface analysis would fail on 2nd hemi, as rm.mean dset would exist."
   "Also, added new '-overwrite_resp S' to @SUMA_AlignToExperiement command."
 } ,

 {  7, MAR, 2012, RCR, "@update.afni.binaries", MINOR, TYPE_ENHANCE,
   "existing package and install dir no longer required for -defaults",
   NULL
 } ,

 {  7, MAR, 2012, RCR, "GIFTI", MICRO, TYPE_BUG_FIX,
   "fixed sizeof in memset of gim (noted by B Cox)",
   NULL
 } ,

 {  6, MAR, 2012, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "move nokia help to -help_install_nokia (since it is not recommended)",
   NULL
 } ,

 {  5, MAR, 2012, RCR, "apsearch", MICRO, TYPE_MODIFY,
   "do not set shell variables",
   NULL
 } ,

 {  5, MAR, 2012, RCR, "uber_proc.py", MICRO, TYPE_MODIFY,
   "trivially apply -help option, for apsearch",
   NULL
 } ,

 {  5, MAR, 2012, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "EDIT_empty: only propagate writable storage_modes",
   "Added is_writable_storage_mode and DSET_STORAGE_MODE.\n"
   "Thanks to Eli for reporting the problem."
 } ,

 {  2, MAR, 2012, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "fixed $runs in multi-run ricor",
   "Thanks to I Mukai for reporting the problem."
 } ,

 { 27, FEB, 2012, RCR, "@update.afni.binaries", MINOR, TYPE_ENHANCE,
   "made a little more intelligent, e.g. make one backup by default",
   "Note: can run this without any existing AFNI binaries, e.g.\n"
   "      @update.afni.binaries -bindir ~/abin -package linux_openmotif"
 } ,

 { 24, FEB, 2012, RCR, "1d_tool.py", MINOR, TYPE_MODIFY,
   "added -moderate_mask, fixed -extreme_mask help",
   "Thanks to R Kuplicki for reporting the help inconsistency."
 } ,

 { 22, FEB, 2012, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "moved GLOBAL_browser def from afni.h to thd_ttatlas_query.c",
   "- declared in TTQ.h\n"
   "- deleted #include thd_atlas.h from most .c files\n"
   "- #include thd_atlas.h in mrilib.h"
 } ,

 { 22, FEB, 2012, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -randomize_trs and -seed",
   "Affected 1d_tool.py, afni_util.py, lib_afni1D.py and lib_textdata.py."
 } ,

 { 21, FEB, 2012, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "if destination directory is not writable, let the user know",
   NULL
 } ,

 { 16, FEB, 2012, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "more quick termination updates",
   "- added -max_images\n"
   "- do not init vol search state to 2, would limit volumes to 40\n"
   "- include fl_start in no_wait test\n"
   "- look for new vol worth of images, but no volume match"
 } ,

 { 14, FEB, 2012, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "if -no_wait, terminate on volume_match failure",
   "For F Ye."
 } ,

 { 14, FEB, 2012, RCR, "uber_ttest.py", MINOR, TYPE_ENHANCE,
   "release version 1.0: help, copy tables, scripts imply -no_gui",
   NULL
 } ,

 { 10, FEB, 2012, RCR, "uber_ttest.py", MINOR, TYPE_ENHANCE,
   "added 'paired' toggle box to GUI",
   NULL
 } ,

 { 10, FEB, 2012, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -check_results_dir for Z Saad",
   "Also, changed -tcat_outlier_warn_limit to -tcat_preSS_warn_limit."
 } ,

 { 10, FEB, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "make tcat files optional; apply prefix to 'basic' commands in driver",
   NULL
 } ,

 { 10, FEB, 2012, RCR, "slow_surf_clustsim.py", MICRO, TYPE_MODIFY,
   "tiny help update, as enforced by H Jo",
   NULL
 } ,

 {  6, FEB, 2012, RCR, "Dimon", MINOR, TYPE_ENHANCE,
   "added -no_wait option: never wait for new data",
   "Also, suppress new glob warnings.\n"
   "Done for F Ye and others."
 } ,

 {  6, FEB, 2012, RCR, "to3d", MICRO, TYPE_MODIFY,
   "tiny help update, as enforced by D Glen",
   NULL
 } ,

 {  6, FEB, 2012, RCR, "3dsvm", MICRO, TYPE_MODIFY,
   "applied Makefile.INCLUDE updates for J Lisinski and S LaConte",
   NULL
 } ,

 {  3, FEB, 2012, RCR, "align_epi_anat.py", MICRO, TYPE_BUG_FIX,
   "updated @AddEdge command to match change to afni_base:shell_com",
   "Done with D Glen."
 } ,

 {  2, FEB, 2012, RCR, "uber_ttest.py", MINOR, TYPE_ENHANCE,
   "added basic 3dMEMA capabilities",
   "This affected afni_util.py, ask_me.py, gui_uber_ttest.py, lib_qt_gui.py\n"
   "     lib_subjects.py, lib_uber_ttest.py, uber_subject.py, uber_ttest.py."
 } ,

 {  1, FEB, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_ENHANCE,
   "check for pre-steady state outlier warnings",
   NULL
 } ,

 {  1, FEB, 2012, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "check for pre-steady state outlier counts",
   "Added option -tcat_outlier_warn_limit."
 } ,

 { 31, JAN, 2012, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "look for aligned anat _al_junk/keep",
   NULL
 } ,

 { 31, JAN, 2012, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "also update check for '### ASCCONV BEGIN' in to3d...",
   "Problem noted by J Lewis."
 } ,

 { 31, JAN, 2012, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_apply_ricor",
   NULL
 } ,

 { 30, JAN, 2012, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "ricor block: no longer apply in later 3dDeconvolve",
   "The regressors for slice #0 from the ricor block were being applied in\n"
   "the final regression (to remove motion) for consistency in degrees of\n"
   "freedom.  But we might rather not do that, particularly since DOF are\n"
   "not important when proceeding with just correlation coefficients."
 } ,

 { 30, JAN, 2012, RCR, "uber_ttest.py", MICRO, TYPE_MODIFY,
   "all python files use '/usr/bin/env python' instead of '/usr/bin/python'",
   "modified afni_base.py, gui_uber_align_test.py, gui_uber_subj.py,\n"
   "         uber_skel.py, @DoPerRoi.py, gui_uber_skel.py, gui_uber_ttest.py,\n"
   "         uber_ttest.py"
 } ,

 { 28, JAN, 2012, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "look for TSNR* in case of surf analysis",
   NULL
 } ,

 { 28, JAN, 2012, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "updates for surf analysis of subject FT under AFNI_data6",
   "  - added -atlas_followers to @SUMA_AlignToExperiment\n"
   "  - if surf analysis: no scaling mask (e.g. extents)\n"
   "  - updated help example #8 for surf analysis of AFNI_data6 subject FT"
 } ,

 { 27, JAN, 2012, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "fix inf loop if some sSliceArray entries not set",
   "Also, now there might be junk between ASCCONV BEGIN and ###, grrrrr...\n"
   "Problem noted by J Lewis."
 } ,

 { 25, JAN, 2012, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "back out overzealous -quit changes for now",
   NULL
 } ,

 { 20, JAN, 2012, RCR, "to3d", MINOR, TYPE_MODIFY,
   "mri_read_dicom: if there is no VALID vrCode, skip explicitVR",
   "Done for Chad N."
 } ,

 { 19, JAN, 2012, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "made -quit more aggressive",
   NULL
 } ,

 { 18, JAN, 2012, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "force anat variable (and children) to be in AFNI format after 3dcopy",
   "Appropriate, and for compatibility with an afni_name.pv change."
 } ,

 { 17, JAN, 2012, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "-gert_create_dataset now implies -GERT_Reco and -quit",
   "For Ziad and Daniel."
 } ,

 { 12, JAN, 2012, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed ricor block 3dcalc loop for varying run lengths",
   NULL
 } ,

 { 11, JAN, 2012, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "dx and dy were reversed in mri_read_dicom.c",
   "Thanks to P Kaskan and F Ye for bringing this up."
 } ,

 { 28, NOV, 2011, RCR, "1dnorm", MICRO, TYPE_BUG_FIX,
   "re-enabled use of '-' for stdin/stdout",
   "Requested by R Birn."
 } ,

 { 22, NOV, 2011, RCR, "uber_subject.py", MINOR, TYPE_NEW_OPT,
   "allow for passing variables directly, not via -svar",
   NULL
 } ,

 { 21, NOV, 2011, RCR, "quick.alpha.vals.py", MINOR, TYPE_NEW_PROG,
   "a very simple program to tabulate the output from slow_surf_clustsim.py",
   "This ought to be improved, but at least it is now distributed..."
 } ,

 { 21, NOV, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "small update to help text",
   NULL
 } ,

 { 21, NOV, 2011, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "fixed -ynames in plot of motion/outliers",
   NULL
 } ,

 { 17, NOV, 2011, RCR, "@ROI_Corr_Mat", MICRO, TYPE_BUG_FIX,
   "fix complaint about unknown options for non-macs",
   NULL
 } ,

 { 17, NOV, 2011, RCR, "afni", MINOR, TYPE_BUG_FIX,
   "if dset+orig is anat/func and dset+tlrc is func/anat, 'BAD dataset', man",
   "Altered logic in thd_initsess.c (done with dglen)."
 } ,

 { 17, NOV, 2011, RCR, "3drefit", MICRO, TYPE_BUG_FIX,
   "changing 'type' should have an effect even if not a bucket",
   "Done with dglen."
 } ,

 {  9, NOV, 2011, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "-surf_blur_fwhm is no longer valid, please use -blur_size",
   NULL
 } ,

 {  8, NOV, 2011, RCR, "adwarp", MICRO, TYPE_ENHANCE,
   "added a help example for writing anat+tlrc.BRIK",
   NULL
 } ,

 {  7, NOV, 2011, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -blur_to_fwhm and -blur_opts_B2FW",
   "Added for E Nelson and J Jarcho."
 } ,

 {  4, NOV, 2011, RCR, "3dSurf2Vol", MINOR, TYPE_ENHANCE,
   "added 'mode' mapping function",
   "Requested by R Mruczek.  Also done for Z Puckett."
 } ,

 {  3, NOV, 2011, RCR, "@2dwarper.Allin", MINOR, TYPE_NEW_OPT,
   "added -prefix option; allow for 3dAllin failures; copy time info",
   "Updates by N Mei and A Messinger."
 } ,

 {  2, NOV, 2011, RCR, "Dimon", MINOR, TYPE_ENHANCE,
   "allow -save_file_list to apply even with -infile_list",
   NULL
 } ,

 {  2, NOV, 2011, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "if using TENT, run 'timing_tool.py -warn_tr_stats'",
   "Warnings are also saved in out.TENT_warn.txt."
 } ,

 {  2, NOV, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "added out.TENT_warn.txt to warning file review",
   NULL
 } ,

 { 31, OCT, 2011, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_tr_stats and -warn_tr_stats options",
   NULL
 } ,

 { 25, OCT, 2011, RCR, "timing_tool.py", MINOR, TYPE_ENHANCE,
   "process married files with current operations",
   "1. AfniMarriedTiming inherits from AfniData (instead of local copies)\n"
   "2. add all AfniTiming methods to AfniMarriedTiming (as married timing)\n"
   "3. rename AfniMarriedTiming back to AfniTiming (but now married)"
 } ,

 { 25, OCT, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "look for more file name variants, including with '_' separators",
   "Added for J Weisberg."
 } ,

 { 20, OCT, 2011, RCR, "afni_general", MINOR, TYPE_MODIFY,
   "changed most resampling programs to deoblique inputs upon read",
   "THD_open*_dataset() was followed by THD_make_cardinal().\n"
   "modified: 3dresample, 3dfractionize, 3drotate, adwarp, 3dLRflip\n"
   "          3dZeropad, 3dZcat, 3dAutobox\n"
   "not (yet) modified: 3dWarp(Drive), 3dAllineate"
 } ,

 { 19, OCT, 2011, RCR, "@2dwarper.Allin", MICRO, TYPE_NEW_OPT,
   "added a -mask option",
   "Added for A Messinger."
 } ,

 { 19, OCT, 2011, RCR, "GIFTI", MINOR, TYPE_ENHANCE,
   "can read/write ascii COMPLEX64, COMPLEX128, RGB24",
   "Requested by H Breman, J Mulders and N Schmansky."
 } ,

 { 18, OCT, 2011, RCR, "uber_subject.py", MINOR, TYPE_NEW_OPT,
   "added blur size control; removed requirement of stim timing files",
   NULL
 } ,

 { 18, OCT, 2011, RCR, "@radial_correlate", MICRO, TYPE_ENHANCE,
   "require enough volumes per dataset, store file name correspondence",
   NULL
 } ,

 { 17, OCT, 2011, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "added -help for new -surf_* options, along with example #8",
   NULL
 } ,

 { 17, OCT, 2011, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "abin now has subdir (funstuff), so change 'mv' to 'rsync'",
   NULL
 } ,

 { 14, OCT, 2011, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "now processes surface data",
   "- added 'surf' processing block, and corresponding '-surf_*' options:\n"
   "   -surf_anat, -surf_spec, -surf_anat_aligned, -surf_anat_has_skull,\n"
   "   -surf_A, -surf_B, -surf_blur_fwhm\n"
   "- compute errts and TSNR by default (had required option or blur est)"
 } ,

 { 14, OCT, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "small -help_install update",
   NULL
 } ,

 { 14, OCT, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "allow modest handling of surface results",
   NULL
 } ,

 {  6, OCT, 2011, RCR, "3dMean", MICRO, TYPE_BUG_FIX,
   "linux_xorg7_64 had optimizer error (and crashed), so altered loop method",
   "Thanks to P Kim for reporting the problem."
 } ,

 {  5, OCT, 2011, RCR, "uber_subject.py", MICRO, TYPE_BUG_FIX,
   "do not re-create proc script on proc execution",
   NULL
 } ,

 {  4, OCT, 2011, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -anat_has_skull option",
   NULL
 } ,

 {  4, OCT, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "changed basic script outputs",
   "- added 'max censored displacement', 'final anat dset' and\n"
   "        'final voxel resolution' to basic script\n"
   "- removed 'num stim files found'"
 } ,

 {  4, OCT, 2011, RCR, "to3d", MICRO, TYPE_MODIFY,
   "explicitly warn about illegal '/' characters in output filename",
   NULL
 } ,

 {  3, OCT, 2011, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "changed default polort time back to TR*NT, to match 3dDeconvolve",
   NULL
 } ,

 {  3, OCT, 2011, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -censor_infile (e.g. to remove TRs from motion params)",
   "Added for N Adleman."
 } ,

 {  3, OCT, 2011, RCR, "afni-general", MICRO, TYPE_ENHANCE,
   "update ADN_ONE_STEP to ten million, allowing that many output sub-bricks",
   "This affects programs like 3dbucket, when the output has many volumes.\n"
   "Done for HJ Jo, J Gonzalez-Castillo, M Robinson."
 } ,

 { 22, SEP, 2011, RCR, "uber_ttest.py", MAJOR, TYPE_NEW_PROG,
   "a graphical program for running either 3dttest++ or 3dMEMA",
   "Still under destruction."
 } ,

 { 22, SEP, 2011, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "various updates",
   "- updated quotize_list\n"
   "- added nuke_final_whitespace, flist_to_table_pieces, get_ids_from_dsets"
 } ,

 { 22, SEP, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_MODIFY,
   "added check_for_file and for_dset, updated find_x_mat, enorm, view_stats",
   NULL
 } ,

 { 22, SEP, 2011, RCR, "uber_align_test.py", MICRO, TYPE_MODIFY,
   "moved get_def_tool_path to library",
   NULL
 } ,

 { 22, SEP, 2011, RCR, "uber_subject.py", MINOR, TYPE_MODIFY,
   "altered spacing and made other minor changes",
   NULL
 } ,

 {  7, SEP, 2011, RCR, "Makefile.INCLUDE", MICRO, TYPE_MODIFY,
   "added @radial_correlate to the install scripts (forgot earlier)",
   NULL
 } ,

 {  6, SEP, 2011, RCR, "Dimon", MICRO, TYPE_NEW_OPT,
   "added -fast option, short for: -sleep_init 50 -sleep_vol 50",
   NULL
 } ,

 {  1, SEP, 2011, RCR, "afni_util.py", MICRO, TYPE_BUG_FIX,
   "get_default_polort: run time should be TR * (NT-1)",
   "This was changed back to TR*NT, to match 3dDeconvolve (3 Oct, 2011)."
 } ,

 {  1, SEP, 2011, RCR, "@radial_correlate", MINOR, TYPE_NEW_PROG,
   "compute voxelwise EPI correlations with local spherical averages",
   NULL
 } ,

 { 31, AUG, 2011, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "if censoring motion or outliers, add options to gen_ss_r command",
   NULL
 } ,

 { 30, AUG, 2011, RCR, "Dimon", MICRO, TYPE_BUG_FIX,
   "update volume delta to mean dz",
   "From text in DICOM files, initial dz values may not be sufficiently\n"
   "accurate, leaing to 'volume toasted' errors.\n"
   "Thanks to B Benson for reporting the problem."
 } ,

 { 19, AUG, 2011, RCR, "3dDeconvolve", MINOR, TYPE_MODIFY,
   "added the ability to output 1D sresp datasets",
   "Requested by S Baum."
 } ,

 { 17, AUG, 2011, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_BUG_FIX,
   "fixed some final anat dset assignments",
   NULL
 } ,

 { 15, AUG, 2011, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "altered SUMA_ParseFname for parsing of relative pathnames",
   "Thanks to Ryan of Princeton for reporting the problem."
 } ,

 { 12, AUG, 2011, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "gave volreg 3dAllineate command priority for final anat",
   NULL
 } ,

 {  4, AUG, 2011, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "wrote loc_strcpy(/cat)_realloc for MCW_file_expand",
   "This is to allow for long sub-brick selectors."
 } ,

 {  3, AUG, 2011, RCR, "align_epi_anat.py", MICRO, TYPE_NEW_OPT,
   "added -save_orig_skullstrip, to avoid oblique transforms",
   "This was added for afni_proc.py."
 } ,

 {  3, AUG, 2011, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "changed aea.py -save_skullstrip to -save_orig_skullstrip",
   "The -save_skullstrip dataset might have an oblique transformation\n"
   "applied (which would throw off EPI if it is then sent to standard\n"
   "space).  Apply the new option to grab a simple skull-stripped anat.\n"
   "Thanks to A Ellenstein for reporting it and Z Saad for help understanding."
 } ,

 {  2, AUG, 2011, RCR, "gen_ss_review_scripts.py", MICRO, TYPE_MODIFY,
   "added control var out_prefix, a prefix for output files",
   NULL
 } ,

 {  2, AUG, 2011, RCR, "uber_skel.py", MICRO, TYPE_MODIFY,
   "main class inherits object - for older versions of python",
   NULL
 } ,

 {  2, AUG, 2011, RCR, "uber_align_test.py", MICRO, TYPE_MODIFY,
   "main class inherits object - for older versions of python",
   NULL
 } ,

 { 29, JUL, 2011, RCR, "3dUniformize", MINOR, TYPE_BUG_FIX,
   "fixed checks against lower_limit in option processing (since -1 init)",
   "Thanks to A Waite for reporting the problem and cause in the code."
 } ,

 { 29, JUL, 2011, RCR, "make_pq_script.py", MINOR, TYPE_MODIFY,
   "changes to handle python 2.4, where shell output has extra blank lines",
   NULL
 } ,

 { 29, JUL, 2011, RCR, "slow_surf_clustsim.py", MINOR, TYPE_ENHANCE,
   "z.max files are now named by p-value",
   "And suggest a quick.alpha.vals.py command."
 } ,

 { 26, JUL, 2011, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "if e2a, update current anat to skull-stripped anat from align block",
   "This would avoid a second skull-strip step in @auto_tlrc."
 } ,

 { 25, JUL, 2011, RCR, "slow_surf_clustsim.py", MINOR, TYPE_ENHANCE,
   "added keepblocks var, to limit kept intermediate datasets",
   NULL
 } ,

 { 22, JUL, 2011, RCR, "slow_surf_clustsim.py", MINOR, TYPE_BUG_FIX,
   "after blur, rescale noise to be normally distributed",
   NULL
 } ,

 { 21, JUL, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_MODIFY,
   "changed TR counts to come via awk instead of grep",
   NULL
 } ,

 { 21, JUL, 2011, RCR, "edt_blur.c", MICRO, TYPE_BUG_FIX,
   "fixed nz/sigmay typo, found by Patryk (on message board)",
   NULL
 } ,

 { 20, JUL, 2011, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed aea.py -epi_base when: aea.py, -volreg_a2 last, variable run lens",
   "thanks to S Brislin and S White for reporting the problem"
 } ,

 { 20, JUL, 2011, RCR, "make_pq_script.py", MINOR, TYPE_NEW_PROG,
   "program will generate a script to produce a p-value/q-value curve pair",
   NULL
 } ,

 { 15, JUL, 2011, RCR, "@update.afni.binaries", MICRO, TYPE_ENHANCE,
   "applied -d as -defaults",
   NULL
 } ,

 { 15, JUL, 2011, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "save output from ss_review in out.ss_review.$subj.txt",
   NULL
 } ,

 { 14, JUL, 2011, RCR, "gen_ss_review_scripts.py", MINOR, TYPE_ENHANCE,
   "added 'max motion displacement' to basic script",
   NULL
 } ,

 { 14, JUL, 2011, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_max_displace, for maximum motion displacement",
   NULL
 } ,

 { 14, JUL, 2011, RCR, "slow_surf_clustsim.py", MICRO, TYPE_ENHANCE,
   "show date per iter block and add ./ to 3dcalc prefix",
   NULL
 } ,

 { 13, JUL, 2011, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "run gen_ss_review_scripts.py and any resulting 'basic' review script",
   NULL
 } ,

 { 13, JUL, 2011, RCR, "gen_group_command.py", MICRO, TYPE_NEW_OPT,
   "added -exit0 and babbled about possible artifact tests",
   NULL
 } ,

 { 11, JUL, 2011, RCR, "gen_group_command.py", MINOR, TYPE_BUG_FIX,
   "fixed case of partial path match to dsets",
   "Problem found by J Jarcho."
 } ,

 { 11, JUL, 2011, RCR, "gen_ss_review_scripts.py", MAJOR, TYPE_NEW_PROG,
   "for generating single subject review scripts",
   "To be run by the afni_proc.py proc script or directly by users."
 } ,

 { 8, JUL, 2011, RCR, "slow_surf_clustsim.py", MINOR, TYPE_MODIFY,
   "added -on_surface, which might not end up being so useful",
   NULL
 } ,

 { 6, JUL, 2011, RCR, "uber_align_test.py", MICRO, TYPE_MODIFY,
   "test use of SUBJ.set_var_with_defs",
   NULL
 } ,

 { 6, JUL, 2011, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "create anat_final dset, as one that is aligned with the stats",
   "Also, suggest use of uber_subject.py in the -ask_me dialog."
 } ,

 { 5, JUL, 2011, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "minor enhancements to 5 python files (prep for gen_ss_review_scripts.py)",
   NULL
 } ,

 { 30, JUN, 2011, RCR, "afni_proc.c", MICRO, TYPE_MODIFY,
   "renamed aligned anat output (from align_epi_anat.py)",
   "This should make it clear whether or not the output anat should be used"
 } ,

 { 27, JUN, 2011, RCR, "gen_group_command.py", MINOR, TYPE_NEW_OPT,
   "added -dset_index0_list/-dset_index1_list options, etc.",
   "- ttest++ and MEMA commands now apply directories to datasets\n"
   "- changed Subject.atrs to be VarsObject instance, not dictionary\n"
 } ,

 { 27, JUN, 2011, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "changed decode_1D_ints to take imax param",
   "Affects 1d_tool.py, xmat_tool.py and any utilities using lib_afni1D.py.\n"
   "Also added restrict_by_index_lists()."
 } ,

 { 27, JUN, 2011, RCR, "thd_table.c", MICRO, TYPE_MODIFY,
   "output warning hint on single column covariate file",
   NULL
 } ,

 { 24, JUN, 2011, RCR, "slow_surf_clustsim.py", MAJOR, TYPE_NEW_PROG,
   "a temporary program until we do this in C",
   NULL
 } ,

 { 20, JUN, 2011, RCR, "afni_util.py", MINOR, TYPE_NEW_OPT,
   "added eta2 function (alongside '3ddot -doeta2')",
   NULL
 } ,

 { 17, JUN, 2011, RCR, "SUMA_MiscFunc.c", MICRO, TYPE_BUG_FIX,
   "set 0-length BmP array to matching length 3",
   NULL
 } ,

 { 16, JUN, 2011, RCR, "3ddot", MINOR, TYPE_NEW_OPT,
   "added -doeta2 via new THD_eta_squared_masked",
   "Added for Shinchan."
 } ,

 { 16, JUN, 2011, RCR, "3dAutoTcorrelate", MICRO, TYPE_BUG_FIX,
   "allowed very small datasets",
   NULL
 } ,

 { 15, JUN, 2011, RCR, "gen_group_command.py", MINOR, TYPE_ENHANCE,
   "if constant dset names, extract SIDs from dir names",
   "Done for R Momenan."
 } ,

 { 8, JUN, 2011, RCR, "make_random_timing.py", MICRO, TYPE_BUG_FIX,
   "fixed print and added min_rest to durations in test of -tr_locked",
   NULL
 } ,

 { 3, JUN, 2011, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -volreg_compute_tsnr/-regress_compute_tsnr",
   "Volreg TSNR is no longer the default, but regress TSNR is."
 } ,

 { 3, JUN, 2011, RCR, "Makefile", MINOR, TYPE_NEW_OPT,
   "removed -lpng from Makefile.macosx_10.6_Intel_64",
   "We added -lpng because we were compiling our own OpenMotif\n"
   "(configure option?), but fink's version does not need it."
 } ,

 { 2, JUN, 2011, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "compute TSNR datasets (added -compute_tsnr); added -regress_make_cbucket",
   NULL
 } ,

 { 2, JUN, 2011, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "de-meaned motion regressors is now the default",
   "- added -regress_apply_mot_types to specify motion types for regression\n"
   "- added -regress_no_motion_demean and -regress_no_motion_deriv\n"
   "- by default, demean and deriv motion parameters are created\n"
   "- by default, demean motion parameters are applied in the regression\n"
   "  (replacing the original 'basic' parameters, which should have no\n"
   "  change in betas of interest, just the constant polort betas)"
 } ,

 { 27, MAY, 2011, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "re-work of motion, as prep for more motion options",
   "- replaced -volreg_regress_per_run with -regress_motion_per_run\n"
   "- made uniq_list_as_dsets() a warning, not an error (for J Britton)"
 } ,

 { 27, MAY, 2011, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -split_into_pad_runs (for regress motion per run)",
   NULL
 } ,

 { 25, MAY, 2011, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -global_to_local and -local_to_global for G Chen",
   NULL
 } ,

 { 24, MAY, 2011, RCR, "1dplot", MINOR, TYPE_BUG_FIX,
   "fixed plotting of varying length time series",
   NULL
 } ,

 { 20, MAY, 2011, RCR, "uber_subject.py", MINOR, TYPE_MODIFY,
   "execute via /usr/bin/env python",
   "Help now suggests fink as primary Mac source for PyQt4."
 } ,

 { 20, MAY, 2011, RCR, "uber_align_test.py", MINOR, TYPE_MODIFY,
   "execute via /usr/bin/env python",
   NULL
 } ,

 { 16, MAY, 2011, RCR, "uber_align_test.py", MINOR, TYPE_ENHANCE,
   "could be used as a release version",
   " - added 'check center dist' button, to display the current distance\n"
   " - added menu item to show afni command for viewing results\n"
   " - added menu items to show python and shell command windows\n"
   " - added much more help, including main and section buttons\n"
   " - added browsing of align_epi_anat.py help"
 } ,

 { 16, MAY, 2011, RCR, "afni_util.py", MICRO, TYPE_ENHANCE,
   "added exec_tcsh_command function",
   NULL
 } ,

 { 16, MAY, 2011, RCR, "@Center_Distance", MICRO, TYPE_ENHANCE,
   "return something to $status, so we can detect success or failure",
   NULL
 } ,

 { 13, MAY, 2011, RCR, "uber_align_test.py", MINOR, TYPE_ENHANCE,
   "added working GUI (for options, GUI help still needs ... help)",
   NULL
 } ,

 { 12, MAY, 2011, RCR, "uber_skel.py", MAJOR, TYPE_NEW_PROG,
   "A working skeleton for future uber programs.",
   "This is based on uber_align_test.py, version 0.2."
 } ,

 { 12, MAY, 2011, RCR, "uber_align_test.py", MINOR, TYPE_ENHANCE,
   "many small updates",
   "This set of 3 files was broken off set uber_skel.py, meant to be a\n"
   "resonable starting point for future uber programs."
 } ,

 { 11, MAY, 2011, RCR, "uber_align_test.py", MAJOR, TYPE_ENHANCE,
   "added basic graphical interface, still need to add variable fields",
   "o  also made single cost_list\n"
   "o  also added -help_howto_program, which might move to a skeleton program"
 } ,

 { 11, MAY, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "small help/todo update",
   NULL
 } ,

 { 9, MAY, 2011, RCR, "to3d", MICRO, TYPE_ENHANCE,
   "applied formal parsing for CSA Image Header Info for Siemens slice timing",
   "Process field (0x0029 1010) as little-endian CSA1 or 2 header, tags\n"
   "and items.  Get slice times from MosaicRefAcqTimes item.\n"
   "Thanks to I Souheil for finding NiBabel CSA format description."
 } ,

 { 4, MAY, 2011, RCR, "to3d", MICRO, TYPE_BUG_FIX,
   "fixed case of simult tpattern (so time_dep, but ui.tpattern is not set)",
   "Problem noted by J Ostuni."
 } ,

 { 2, MAY, 2011, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "added nul-termination and a.b[.d]+ rules for checking Siemens slice times",
   "Problem noted by D Kravitz and S Lee."
 } ,

 { 29, APR, 2011, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "check that processing blocks are unique",
   NULL
 } ,

 { 28, APR, 2011, RCR, "uber_align_test.py", MAJOR, TYPE_NEW_PROG,
   "for testing EPI/anat alignment with various align_epi_anat.py options",
   "This is a command-line version, with a GUI to come soon."
 } ,

 { 28, APR, 2011, RCR, "uber_subject.py", MINOR, TYPE_NEW_OPT,
   "reconcile LUS.py with LS.py in prep for uber_align_test.py",
   NULL
 } ,

 { 28, APR, 2011, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -align_epi_strip_method",
   NULL
 } ,

 { 25, APR, 2011, RCR, "Imon", MINOR, TYPE_MODIFY,
   "Imon is getting phased out of the distribution (see 'Dimon -use_imon')",
   "Requires compiling alterations to be put back in (if anyone wants it)."
 } ,

 { 25, APR, 2011, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "have Dimon send 'TPATTERN explicit' with slice timing to RT plugin",
   NULL
 } ,

 { 24, APR, 2011, RCR, "@Align_Centers", MINOR, TYPE_ENHANCE,
   "allow -base dset to be in PATH, AFNI_PLUGINPATH, etc.",
   NULL
 } ,

 { 22, APR, 2011, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "if manual tlrc and -volreg_tlrc_adwarp, also transform extents mask",
   "Noted by J Britton.\n"
   "Also, if -regress_reml_exec, insert 3dClustSim table in stats_REML.\n"
   "Noted by R Momenan."
 } ,

 { 15, APR, 2011, RCR, "Dimon", MINOR, TYPE_ENHANCE,
   "added FROM_IMAGE as default Siemens slice pattern in to3d command",
   NULL
 } ,

 { 15, APR, 2011, RCR, "dicom_hdr", MINOR, TYPE_NEW_OPT,
   "added -slice_times and -slice_times_verb, to show siemens slice timing",
   NULL
 } ,

 { 14, APR, 2011, RCR, "thd_atlas.c", MICRO, TYPE_BUG_FIX,
   "for solaris, apply #def strcasestr strstr",
   NULL
 } ,

 { 13, APR, 2011, RCR, "to3d", MINOR, TYPE_NEW_OPT,
   "added FROM_IMAGE timing pattern (for Siemens mosaic images)",
   NULL
 } ,

 { 11, APR, 2011, RCR, "uber_subject.py", MICRO, TYPE_BUG_FIX,
   "fixed lost warnings for no sid/gid",
   NULL
 } ,

 { 8, APR, 2011, RCR, "Makefile", MICRO, TYPE_MODIFY,
   "removed -lconverted_from_fortran from Makefile.INCLUDE, added to LLIBS in Makefile.*",
   "Also removed redundant -lmri from many Makefiles and Makefile.INCLUDE."
 } ,

 { 7, APR, 2011, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "backports for Ubuntu 9",
   "requested by J Bodurka"
 } ,

 { 6, APR, 2011, RCR, "uber_subject.py", MICRO, TYPE_MODIFY,
   "make table size depend on font",
   NULL
 } ,

 { 5, APR, 2011, RCR, "howto", MINOR, TYPE_ENHANCE,
   "updated the class setup and basic Linux instructions for PyQt4",
   NULL
 } ,

 { 5, APR, 2011, RCR, "python_module_test.py", MICRO, TYPE_NEW_OPT,
   "added PyQt4 to test list",
   NULL
 } ,

 { 29, MAR, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "changed subject directory to group.GROUP/subj.SUBJ",
   NULL
 } ,

 { 24, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "added align and tlrc option boxes, adjusted spacing",
   NULL
 } ,

 { 23, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "moved gltsym box to below stim, save AP output, small mac install update",
   NULL
 } ,

 { 22, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "processing status, clear options/fields menu items, etc...",
   NULL
 } ,

 { 22, MAR, 2011, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "mri_read_dicom: if there is no vrCode, skip explicitVR",
   NULL
 } ,

 { 21, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "many updates, including extra regress options box",
   NULL
 } ,

 { 20, MAR, 2011, RCR, "uber_subject.py", MAJOR, TYPE_ENHANCE,
   "handle symbolic GLTs, etc.",
   NULL
 } ,

 { 15, MAR, 2011, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "changed uncensored Xmat to X.nocensor.1D",
   NULL
 } ,

 { 15, MAR, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "added -regress_make_ideal_sum, subject variables, GUI text changes",
   NULL
 } ,

 { 14, MAR, 2011, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "if no mask but extents, apply in scale step",
   NULL
 } ,

 { 14, MAR, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "a handful of minor updates",
   NULL
 } ,

 {  9, MAR, 2011, RCR, "uber_subject.py", MICRO, TYPE_ENHANCE,
   "updates to uber_subject.py, how could I possibly remember what they are...",
   NULL
 } ,

 {  9, MAR, 2011, RCR, "make_random_timing.py", MICRO, TYPE_BUG_FIX,
   "fixed bug writing comment text in 3dD script",
   "Problem noted by Z Saad and P Kaskan."
 } ,

 {  8, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "uber_subject.py command menu item, ...",
   NULL
 } ,

 {  7, MAR, 2011, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "make proc script executable",
   NULL
 } ,

 {  7, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "updates: command and pycommand windows, new Process classes, ...",
   NULL
 } ,

 {  3, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "updates: control vars, subj dir, view actions, result vars, ...",
   NULL
 } ,

 {  2, MAR, 2011, RCR, "uber_subject.py", MINOR, TYPE_ENHANCE,
   "many updates, including write and exec of proc script",
   "There is still much to do before first release version."
 } ,

 { 22, FEB, 2011, RCR, "uber_subject.py", MINOR, TYPE_MODIFY,
   "added interfaces for 'expected' option",
   NULL
 } ,

 { 17, FEB, 2011, RCR, "3dDeconvolve", MICRO, TYPE_MODIFY,
   "make -CENSORTR run: warning more clear",
   NULL
 } ,

 { 17, FEB, 2011, RCR, "3dROIstats", MICRO, TYPE_MODIFY,
   "make unknown option error more clear",
   NULL
 } ,

 { 16, FEB, 2011, RCR, "uber_subject.py", MINOR, TYPE_MODIFY,
   "epi or stim list from command line can init order/labels; file reorg",
   "Also, initiated regression testing tree."
 } ,

 { 16, FEB, 2011, RCR, "howto", MINOR, TYPE_MODIFY,
   "updated the main page and basic Linux instructions",
   NULL
 } ,

 { 15, FEB, 2011, RCR, "uber_subject.py", SUPER, TYPE_NEW_PROG,
   "added CLI (command-line interface), generates basic afni_proc.py script",
   "Many enhancements yet to come."
 } ,

 { 14, FEB, 2011, RCR, "uber_proc.py", MINOR, TYPE_GENERAL,
   "moved uber program into main repository",
   NULL
 } ,

 { 12, FEB, 2011, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "updates for uber_subject.py",
   NULL
 } ,

 { 11, FEB, 2011, RCR, "lib_subjects.py", MICRO, TYPE_ENHANCE,
   "more updates for uber_subject.py",
   NULL
 } ,

 { 1, FEB, 2011, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "updates for parsing a stim file list",
   NULL
 } ,

 { 31, JAN, 2011, RCR, "afni_util.py", MICRO, TYPE_ENHANCE,
   "updates for uber_subj.py",
   NULL
 } ,

 { 25, JAN, 2011, RCR, "lib_subjects.py", MICRO, TYPE_ENHANCE,
   "updates to the VarsObject class",
   NULL
 } ,

 { 19, JAN, 2011, RCR, "lib_subjects.py", MINOR, TYPE_ENHANCE,
   "many updates to the VarsObject class",
   NULL
 } ,

 { 13, JAN, 2011, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "small changes to warnings for missing stimulus files",
   NULL
 } ,

 { 13, JAN, 2011, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -gert_write_as_nifti and -gert_create_dataset",
   "requested by V Roopchansingh"
 } ,

 { 10, JAN, 2011, RCR, "3dttest", MICRO, TYPE_GENERAL,
   "fail with error message when -set2 is not the final option",
   "It had already been assumed to be the final option."
 } ,

 {  7, JAN, 2011, RCR, "rickr/Makefile", MICRO, TYPE_BUG_FIX,
   "Dimon: forgot to reconcile use of expat (with LGIFTI)",
   NULL
 } ,

 {  6, JAN, 2011, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "ComputeObliquity() mosaic shift should be dcK*(nK-1)/2 in each direction",
   NULL
 } ,

 {  4, JAN, 2011, RCR, "Dimon", MAJOR, TYPE_ENHANCE,
   "version 3.0 : handle Siemens Mosaic formatted files",
   "- depend on libmri, return MRI_IMARR from mri_read_dicom, changes\n"
   "  for oblique and mosaic processing\n"
   "- mri_read_dicom.c: g_info (process control), g_image_info (Dimon)\n"
   "  replaced DEBUG_ON/debugprint with g_info.verb, many small changes\n"
   "- mri_dicom_elist.h: merged with dimon_afni.h\n"
   "- mcw_glob.[ch]: control sort direction via rglob_set_sort_dir()"
 } ,

 {  4, JAN, 2011, RCR, "afni", MICRO, TYPE_GENERAL,
   "do not open default windows in case of real-time",
   NULL
 } ,

 { 23, DEC, 2010, RCR, "to3d", MINOR, TYPE_NEW_OPT,
   "added -use_old_mosaic_code",
   "This is phase 1 of dealing with Siemens mosaic format in Dimon.\n"
   "Siemens mosaic functions we moved to new mri_process_siemens.c,\n"
   "with the ability to use the old code preserved with this option."
 } ,

 { 16, DEC, 2010, RCR, "@ANATICOR", MICRO, TYPE_GENERAL,
   "HJ change: small updates to the help",
   "changes were submitted for Hang Joon Jo"
 } ,

 { 16, DEC, 2010, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "updates to file type (looks like) errors and warnings",
   NULL
 } ,

 { 16, DEC, 2010, RCR, "1d_tool.py", MINOR, TYPE_ENHANCE,
   "updates to file type (looks like) errors and warnings",
   NULL
 } ,

 { 15, DEC, 2010, RCR, "timing_tool.py", MINOR, TYPE_ENHANCE,
   "use lib_textdata.py for reading timing files, allow empty file",
   "empty file update for C Deveney"
 } ,

 { 14, DEC, 2010, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed problem with timing file tests on 'empty' files with '*'",
   "problem noted by C Deveney and R Momenan"
 } ,

 { 30, NOV, 2010, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added option -final_sort_by_prog",
   NULL
 } ,

 { 22, NOV, 2010, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "small improvements to line wrapping",
   NULL
 } ,

 { 19, NOV, 2010, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "add -write_all_rest_times, moved write_to_timing_file to afni_util.py",
   "option added for J Poore"
 } ,

 { 18, NOV, 2010, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed stim_files to stim_times conversion after multi_basis change",
   "problem noted by M Weber"
 } ,

 { 18, NOV, 2010, RCR, "make_stim_times.py", MICRO, TYPE_BUG_FIX,
   "fix for '*' in max 1 stim per run case",
   NULL
 } ,

 { 10, NOV, 2010, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "added new NOTE sections for ANAT/EPI ALIGNMENT to -help output",
   NULL
 } ,

 {  8, NOV, 2010, RCR, "gen_group_command.py", MAJOR, TYPE_NEW_OPT,
   "can now generate 3dttest++ commands",
   NULL
 } ,

 {  4, NOV, 2010, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "use X.uncensored.xmat.1D instead of X.full_length.xmat.1D",
   NULL
 } ,

 {  4, NOV, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added regress_basis_multi, -regress_no_ideal_sum",
   "One can specify either one basis function or one per stim_times file."
 } ,

 {  4, NOV, 2010, RCR, "1d_tool.py", MICRO, TYPE_BUG_FIX,
   "fixed print problem in -show_indices",
   "Problem noted by Mingbo Cai."
 } ,

 {  2, NOV, 2010, RCR, "3dTstat", MINOR, TYPE_ENHANCE,
   "allow single volume input for functions mean, max, min, sum",
   "Other functions can be added to this list as needed."
 } ,

 { 29, OCT, 2010, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_indices_baseline, _motion and _interest",
   NULL
 } ,

 { 28, OCT, 2010, RCR, "3dMean", MICRO, TYPE_BUG_FIX,
   "do not proceed in case of no input datasets",
   NULL
 } ,

 { 27, OCT, 2010, RCR, "file_tool", MINOR, TYPE_NEW_OPT,
   "added -show_bad_char and -show_bad_all",
   NULL
 } ,

 { 26, OCT, 2010, RCR, "gen_group_command.py", MINOR, TYPE_NEW_OPT,
   "solidified 3dMEMA commands",
   "This is now used to generate AFNI_data6/group_results/s4.3dMEMA.V-A."
 } ,

 { 25, OCT, 2010, RCR, "gen_group_command.py", MAJOR, TYPE_NEW_PROG,
   "a program to generate group commands (3dMEMA for now)",
   "Commands to come: 3dttest(++), 3dANOVA*, GroupANA."
 } ,

 { 21, OCT, 2010, RCR, "timing_tool.py", MICRO, TYPE_NEW_OPT,
   "added -shift_to_run_offset",
   NULL
 } ,

 { 20, OCT, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -tcat_remove_last_trs, -ricor_regs_rm_nlast",
   "Added for J Czarapata."
 } ,

 { 20, OCT, 2010, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -sort_by_acq_time for -dicom_org on Philips data",
   "Added for Manjula."
 } ,

 { 20, OCT, 2010, RCR, "thd_niftiwrite.c", MICRO, TYPE_BUG_FIX,
   "brick stats to intent codes was off by 1 index",
   "Problem noted by P Kohn."
 } ,

 { 16, OCT, 2010, RCR, "timing_tool.py", MICRO, TYPE_BUG_FIX,
   "fixed timing_to_1D fractions",
   NULL
 } ,

 { 15, OCT, 2010, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -multi_timing_to_events, -multi_timing_to_event_pair, -per_run",
   "- Modified timing_tool.py, lib_timing.py, lib_textdata.py, afni_util.py.\n"
   "- Added for N Adleman."
 } ,

 { 12, OCT, 2010, RCR, "3dREMLfit", MICRO, TYPE_GENERAL,
   "small help update to clarify slice-based regressor warnings",
   "Requested by D Handwerker."
 } ,

 { 17, SEP, 2010, RCR, "3dttest++", MINOR, TYPE_BUG_FIX,
   "init workspace with 0",
   "Trouble noted by M Choi"
 } ,

 { 10, SEP, 2010, RCR, "@SUMA_Make_Spec_FS", MICRO, TYPE_BUG_FIX,
   "removed extra endif's in case of $label_dir",
   NULL
 } ,

 {  8, SEP, 2010, RCR, "afni_util.py", MICRO, TYPE_ENHANCE,
   "added wildcard construction functions",
   NULL
 } ,

 {  1, SEP, 2010, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "tiny changes to help output (e.g. 3dAllineate options)",
   NULL
 } ,

 { 30, AUG, 2010, RCR, "3dVol2Surf", MICRO, TYPE_BUG_FIX,
   "check for -sv dataset before proceeding",
   NULL
 } ,

 { 30, AUG, 2010, RCR, "@CheckForAfniDset", MICRO, TYPE_BUG_FIX,
   "replaced use of {$var} with ${var}",
   "Problem noted by R Mruczek."
 } ,

 { 25, AUG, 2010, RCR, "make_random_timing.py", MICRO, TYPE_MODIFY,
   "update polort and write -nodata TR using 3 decimal places",
   NULL
 } ,

 { 18, AUG, 2010, RCR, "@build_afni_Xlib", MICRO, TYPE_NEW_OPT,
   "added -lib32 for building 32-bit on a 64-bit Linux box",
   NULL
 } ,

 { 18, AUG, 2010, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "changed Makefile.linux_openmp (and _64) building on F10 (was F12)",
   NULL
 } ,

 { 17, AUG, 2010, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "allowed married timing files",
   "Also, delete output script on failure (have -keep_script_on_err option)."
 } ,

 { 16, AUG, 2010, RCR, "lib_textdata.py", MINOR, TYPE_ENHANCE,
   "new module to deal with reading/writing 1D/timing/married text files",
   "heading towards handling married timing in afni_proc.py"
 } ,

 { 16, AUG, 2010, RCR, "make_stim_times.py", MICRO, TYPE_MODIFY,
   "use lib_textdata.py for I/O",
   NULL
 } ,

 { 16, AUG, 2010, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "use lib_textdata.py for I/O",
   NULL
 } ,

 { 16, AUG, 2010, RCR, "lib_afni1D.py", MICRO, TYPE_MODIFY,
   "use lib_textdata.py for I/O (deleted read_1D_file)",
   NULL
 } ,

 { 16, AUG, 2010, RCR, "afni_xmat.py", MICRO, TYPE_MODIFY,
   "use lib_textdata.py for I/O (deleted read_1D_file)",
   NULL
 } ,

 { 16, AUG, 2010, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "rewrote and moved text data I/O routines into lib_textdata.py",
   NULL
 } ,

 { 13, AUG, 2010, RCR, "Makefile.INCLUDE", MICRO, TYPE_GENERAL,
   "explicitly link the math library for the balloon target",
   NULL
 } ,

 {  5, AUG, 2010, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "do not let THD_write_3dim_dataset fail silently",
   NULL
 } ,

 {  4, AUG, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_CS_NN, default to 123",
   "Also, changed -niml to -both so that 1D files are output, and changed\n"
   "the prefix to ClustSim (from rm.CS) so those files are not deleted.\n"
   "If ClustSim is explicitly requested, require blur estimation."
 } ,

 {  3, AUG, 2010, RCR, "afni_history", MICRO, TYPE_BUG_FIX,
   "fixed -check_date test to see if version is current",
   "This problem affects afni_proc.py script execution."
 } ,

 {  2, AUG, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "check that stim_file/_time files match datasets, and that dsets exist",
   "- default is to check that files are appropriate for the input data\n"
   "- default is to check that input datasets exist\n"
   "- added options -test_stim_files and -test_for_dsets\n"
   "- afni_proc.py now depends on lib_afni1D"
 } ,

 {  2, AUG, 2010, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "small looks_like text change and remove TR from look_like_1D",
   NULL
 } ,

 { 30, JUL, 2010, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added options to evaluate whether a file is valid as 1D or stim_times",
   "Added -looks_like_1D, -looks_like_local_times, -looks_like_global_times\n"
   "and -looks_like_test_all.\n"
   "The main purpose is to have tests that afni_proc.py can apply."
 } ,

 { 28, JUL, 2010, RCR, "zfun.c", MICRO, TYPE_BUG_FIX,
   "fixed small typos in the case of HAVE_ZLIB not being defined",
   "- zzb64_to_array (return) and array_to_zzb64 (missing arg)"
 } ,

 { 27, JUL, 2010, RCR, "thd_table.c", MICRO, TYPE_BUG_FIX,
   "strtod typo",
   NULL
 } ,

 { 27, JUL, 2010, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "propagate storage_mode in THD_open_tcat",
   "This is for non-AFNI formatted datasets, and fixes the problem where\n"
   "3dDeconvolve would not propagate nnodes/node_list of surface datasets.\n"
   "Problem noted by N Oosterhof."
 } ,

 { 23, JUL, 2010, RCR, "afni-general", MINOR, TYPE_ENHANCE,
   "added Makefile.linux_openmp (and _64) for building with OpenMP support",
   "Those distribution binaries will be built on a 64-bit Fedora 12 system."
 } ,

 { 22, JUL, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options -regress_run_clustsim and -regress_opts_CS",
   "This is to apply 3dClustSim results for multiple comparison correction\n"
   "to the stats dataset output from 3dDeconvolve."
 } ,

 { 20, JUL, 2010, RCR, "xmat_tool.py", MICRO, TYPE_MODIFY,
   "made small improvement out text formatting for cormat and cosmat", 
   NULL
 } ,

 { 19, JUL, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -check_afni_version and -requires_afni_version",
   "This will allow the processing script to verify that the AFNI version\n"
   "is recent enough for the enclosed commands."
 } ,

 { 19, JUL, 2010, RCR, "@DriveAfni", MICRO, TYPE_NEW_OPT,
   "added -help", 
   NULL
 } ,

 { 19, JUL, 2010, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added -check_date, to verify whether the distribution is current",
   NULL
 } ,

 { 19, JUL, 2010, RCR, "3dFWHMx", MICRO, TYPE_BUG_FIX,
   "fixed -arith mean",
   NULL
 } ,

 { 16, JUL, 2010, RCR, "afni", MICRO, TYPE_BUG_FIX,
   "added legendre to forced_loads array for plugin use",
   NULL
 } ,

 { 16, JUL, 2010, RCR, "3dMean", MINOR, TYPE_NEW_OPT,
   "added -mask_union and -mask_inter, for creation of mask datasets",
   NULL
 } ,

    { 14, JUL, 2010, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "added -mask_test_overlap and -regress_cormat_warnigns",
   "Unless the user sets these options to 'no', the processing script\n"
   "will now use 3dABoverlap to evaluate the anat/EPI mask overlap, and\n"
   "1d_tool.py to check the X-matrix for large pairwise correlations\n"
   "between any two regressors."
 } ,

 { 14, JUL, 2010, RCR, "3dABoverlap", MICRO, TYPE_ENHANCE,
   "added -no_automask to allow mask datasets as input",
   NULL
 } ,

 { 14, JUL, 2010, RCR, "Makefile.linux_gcc33_64", MICRO, TYPE_MODIFY,
   "use staic link of SUMA programs to Motif, as AFNI programs already do",
   NULL
 } ,

 { 13, JUL, 2010, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "added channel list selection, for choosing which channels to merge",
   NULL
 } ,

 { 12, JUL, 2010, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -truncate_times and -round_times for S Durgerian",
   NULL
 } ,

 { 11, JUL, 2010, RCR, "timing_tool.py", MINOR, TYPE_ENHANCE,
   "show TR offset stats if -tr and -show_isi_stats",
   NULL
 } ,

 { 7, JUL, 2010, RCR, "NIFTI", MICRO, TYPE_BUG_FIX,
   "fixed znzread/write to again return nmembers",
   "Also, added M Hanke's update to CMakeLists.txt for new realease number."
 } ,

 { 7, JUL, 2010, RCR, "nifti_tool", MICRO, TYPE_BUG_FIX,
   "fixed nt_read_bricks bsize computation for large files",
   NULL
 } ,

 { 7, JUL, 2010, RCR, "NIFTI", MINOR, TYPE_BUG_FIX,
   "fixes for large files (noted/investigated by M Hanke and Y Halchenko)",
   "- fixed znzread/write, noting example by M Adler\n"
   "- changed nifti_swap_* routines/calls to take size_t"
 } ,

 { 28, JUN, 2010, RCR, "GIFTI", MICRO, TYPE_ENHANCE,
   "applied CMakeLists.txt update from M Hanke for Debian release",
   NULL
 } ,

 { 28, JUN, 2010, RCR, "GIFTI", MICRO, TYPE_MODIFY,
   "the most significant dimension cannot be 1",
   "Requested by N Schmansky"
 } ,

 { 28, JUN, 2010, RCR, "3dAutoTcorrelate", MINOR, TYPE_NEW_OPT,
   "added -eta2 (Cohen eta squared) for HJ Jo",
   "Also added -mask and -mask_only_targets."
 } ,

 { 22, JUN, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "3dToutcount detrending now defaults to Legendre polynomials",
   "  Using Legendre polynomials, 3dToutcount polort can exceed 3\n"
   "  (limit noted by I Mukai and K Bahadur).\n"
   "  Added options -outlier_legendre and -outlier_polort."
 } ,

 { 22, JUN, 2010, RCR, "3dToutcount", MINOR, TYPE_NEW_OPT,
   "added -legendre option, which also allows polort > 3",
   NULL
 } ,

 { 17, JUN, 2010, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "apply default polort in 3dToutcount",
   NULL
 } ,

 { 17, JUN, 2010, RCR, "3dTcat", MICRO, TYPE_ENHANCE,
   "removed sub-brick length limit",
   NULL
 } ,

 { 10, JUN, 2010, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed copying EPI and anat as NIFTI",
   NULL
 } ,

 { 8, JUN, 2010, RCR, "timing_tool.py", MICRO, TYPE_BUG_FIX,
   "fixed partitioning without zeros",
   NULL
 } ,

 { 8, JUN, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_censor_outliers and -regress_skip_first_outliers",
   NULL
 } ,

 { 4, JUN, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "moved outlier counting outside of tshift block",
   "- if only one regressor, use 1dcat for 'sum' ideal\n"
   "- added -count_outliers, default to 'yes'\n"
   "- outlier counting is now at end of tcat block"
 } ,

 { 4, JUN, 2010, RCR, "3dToutcount", MICRO, TYPE_NEW_OPT,
   "added -fraction to output fraction of bad voxels, instead of count",
   "This will be used by afni_proc.py for censoring."
 } ,

 { 3, JUN, 2010, RCR, "plug_realtime", MAJOR, TYPE_ENHANCE,
   "added ability to register merged data and possibly all channels",
   "Via MergeRegister, one can request to register the ChannelMerge dataset.\n"
   "The individual channels can also be 'registered' via the same parameters\n"
   "as the ChannelMerge dataset.\n"
   "Requested by J Hyde, A Jesmanowicz, D Ward of MCW."
 } ,

 { 3, JUN, 2010, RCR, "afni_history", MICRO, TYPE_ENHANCE,
   "added TYPE_ENHANCE, often a more appropriate term",
   NULL
 } ,

 { 1, JUN, 2010, RCR, "afni_util.py", MINOR, TYPE_ENHANCE,
   "added variance and t-test routines (1-sample, paired, pooled, unpooled)",
   NULL
 } ,

 { 27, MAY, 2010, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed use of -volreg_regress_per_run and -regress_censor_motion pair",
   "Problem found by D Drake."
 } ,

 { 20, MAY, 2010, RCR, "Makefile.NIH.CentOS.5.3_64", MICRO, TYPE_MODIFY,
   "update CCOLD to compile with gcc verion 3.4",
   NULL
 } ,

 { 19, MAY, 2010, RCR, "Makefile.*", MINOR, TYPE_BUG_FIX,
   "add CCOLD to all for compiling nifticdf.o on linux_xorg7_64 using gcc v3",
   "This is a bug in the gcc compiler code, not in AFNI.  So the workaround\n"
   "is to compile nifticdf.o with a different version of the compiler.\n"
   "\n"
   "The gcc compiler versions 4.1-4.3 (at least) had an optimization bug\n"
   "when compiling nifticdf.o.  The result was an inaccurate conversion\n"
   "from F-stats to p-values (in some cases).\n"
   "Test with the command:     cdf -t2p fift 1.0 10 100\n"
   "   good result: 0.448817, bad result: 0.0472392\n"
   "Problem found by L Thomas and B Bones."
 } ,

 { 13, MAY, 2010, RCR, "3dbucket", MICRO, TYPE_GENERAL,
   "tiny help update to clarify -glueto and -aglueto",
   NULL
 } ,

 { 13, MAY, 2010, RCR, "ui_xmat.py", MICRO, TYPE_GENERAL,
   "tiny update: check for set_afni_xmat() failure",
   NULL
 } ,

 { 12, MAY, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_censor_first_trs for A Barbey",
   "This is needed when also using -regress_censor_motion."
 } ,

 { 12, MAY, 2010, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -censor_first_trs, to mix with -censor_motion results",
   NULL
 } ,

 { 10, MAY, 2010, RCR, "ktaub.c", MICRO, TYPE_BUG_FIX,
   "allow for build on SOLARIS_OLD",
   NULL
 } ,

 { 6, MAY, 2010, RCR, "Dimon", MINOR, TYPE_ENHANCE,
   "allow negatives in -sort_by_num_suffix, look for '0054 1330' in sorting",
   NULL
 } ,

 { 1, MAY, 2010, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added -max_consec for Liat of Cornell",
   NULL
 } ,

 { 29, APR, 2010, RCR, "@DriveAfni", MICRO, TYPE_ENHANCE,
   "minor updates",
   NULL
 } ,

 { 28, APR, 2010, RCR, "NIFTI", MICRO, TYPE_ENHANCE,
   "added NIFTI_ECODE_CARET for J. Harwell",
   NULL
 } ,

 { 26, APR, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_opts_reml",
   NULL
 } ,

 { 26, APR, 2010, RCR, "3dDeconvolve", MINOR, TYPE_ENHANCE,
   "add $* to end of 3dREMLfit script command, for additional arguments",
   "Finally getting around to afni_proc.py option -regress_opts_reml..."
 } ,

 { 28, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "applied fitts computation to REML case",
   NULL
 } ,

 { 25, MAR, 2010, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "small help update",
   NULL
 } ,

 { 25, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_ENHANCE,
   "small help update describing help sections",
   NULL
 } ,

 { 25, MAR, 2010, RCR, "plug_crender", MICRO, TYPE_GENERAL,
   "changed name in plugin list to original 'Render Dataset'",
   "Also changed plug_render to 'Render [old]', though it is no longer\n"
   "built by default."
 } ,

 { 25, MAR, 2010, RCR, "3dcopy", MICRO, TYPE_GENERAL,
   "on failure, warn user that sub-brick selection is not allowed",
   "Requested by T Nycum."
 } ,

 { 23, MAR, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_compute_fitts option, to save memory in 3dDeconvolve",
   NULL
 } ,

 { 19, MAR, 2010, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "round to 3 bits below 4 (above, truncate to int)",
   NULL
 } ,

 { 19, MAR, 2010, RCR, "3dfractionize", MICRO, TYPE_GENERAL,
   "added 3dAllineate example of inverse tlrc warp",
   NULL
 } ,

 { 18, MAR, 2010, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "handle args with '\\n' in them (probably from quoted newlines)",
   NULL
 } ,

 { 18, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "small updates to help for alignment options",
   NULL
 } ,

 { 17, MAR, 2010, RCR, "timing_tool.py", MINOR, TYPE_BUG_FIX,
   "fixed timing_to_1D when some runs are empty",
   "Problem found by L Thomas and B Bones."
 } ,

 { 16, MAR, 2010, RCR, "3dAttribute", MICRO, TYPE_ENHANCE,
   "set_dataset_attributes() on read - so can use on non-AFNI datasets",
   NULL
 } ,

 { 16, MAR, 2010, RCR, "3dbucket", MINOR, TYPE_BUG_FIX,
   "fixed getting incorrect FDR curves (noted by D Glen)",
   NULL
 } ,

 { 16, MAR, 2010, RCR, "NIFTI", MICRO, TYPE_ENHANCE,
   "added NIFTI_ECODE_VOXBO for D. Kimberg",
   NULL
 } ,

 { 11, MAR, 2010, RCR, "3dsvm_common.c", MICRO, TYPE_GENERAL,
   "some compilers choke on mid-block variable definitions",
   NULL
 } ,

 {  9, MAR, 2010, RCR, "3dNotes", MICRO, TYPE_GENERAL,
   "send -help output to stdout, not stderr (req by T Nycum)",
   NULL
 } ,

 {  9, MAR, 2010, RCR, "thd_gifti.c", MICRO, TYPE_MODIFY,
   "init ptr and clear accidental debug output",
   NULL
 } ,

 {  8, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "modified option order in some help examples",
   NULL
 } ,

 {  8, MAR, 2010, RCR, "thd_gifti.c", MICRO, TYPE_MODIFY,
   "corresponding update of lt->index to lt->key",
   NULL
 } ,

 {  8, MAR, 2010, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "GIfTI LabelTable format change: Index to Key",
   "modified gifti_xml.[ch], gifti_io.[ch]"
 } ,

 {  5, MAR, 2010, RCR, "thd_gifti.c", MICRO, TYPE_MODIFY,
   "try to read gifti UINT32 as INT32 (for FreeSurfer aparc files)",
   NULL
 } ,

 {  4, MAR, 2010, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "minor changes (also see NITRC IDs 4619 and 4644)",
   " - for integers, make default approx test to be equality\n"
   " - small changes to zlib failure strings\n"
   " - cast to avoid compile warning on some systems\n"
   " - made NITRC gifti.dtd link that will not change"
 } ,

 {  3, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "when censoring, create uncensored ideals and sum",
   NULL
 } ,

 { 20, FEB, 2010, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -timing_to_1D, -tr and -min_frac for PPI scripting (and B Benson)",
   NULL
 } ,

 { 18, FEB, 2010, RCR, "SUMA_Makefile_NoDev", MICRO, TYPE_GENERAL,
   "added '--includedir /usr/local/netpbm' for libgts.a build on new Linux",
   NULL
 } ,

 { 5, FEB, 2010, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "thd_gifti: if LabelTable use INTENT_LABEL, suma_gifti.c: no normals",
   "done with Ziad"
 } ,

 { 21, JAN, 2010, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -tlrc_opts_at; made tiny mod to scaling operation",
   NULL
 } ,

 { 15, JAN, 2010, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -regress_fout yes/no option for G. Pagnoni",
   NULL
 } ,

 { 14, JAN, 2010, RCR, "3dVol2Surf", MINOR, TYPE_BUG_FIX,
   "Fixed crash w/labels on '-map_func seg_vals' -> NIML",
   "Problem found by Swaroop at Dartmouth."
 } ,

 { 12, JAN, 2010, RCR, "2dImReg", MICRO, TYPE_BUG_FIX,
   "Fixed crash if ny > nx.  Go back to failure and ponder fix.",
   NULL
 } ,

 { 7, JAN, 2010, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "stdint.h should not be included on a SOLARIS_OLD system",
   NULL
 } ,

 { 6, JAN, 2010, RCR, "thd_intlist.c", MINOR, TYPE_BUG_FIX,
   "fixed sub-brick selection of datasets without labels (e.g. NIfTI)",
   NULL
 } ,

 { 24, DEC, 2009, RCR, "gifti_tool", MINOR, TYPE_NEW_OPT,
   "added -approx_gifti option",
   NULL
 } ,

 { 24, DEC, 2009, RCR, "GIFTI", MINOR, TYPE_ENHANCE,
   "added approximate difference functions",
   "- added gifti_approx_gifti_images, DA_pair, labeltables, diff_offset\n"
   "- added gifti_triangle_diff_offset\n"
   "- gifti_compare_coordsys takes comp_data param"
 } ,

 {  8, DEC, 2009, RCR, "GIFTI", MINOR, TYPE_ENHANCE,
   "added ability to read/write GIFTI LabelTables with colors in thd_gifti.c",
   NULL
 } ,

 {  4, DEC, 2009, RCR, "3dWarp", MICRO, TYPE_GENERAL,
   "add help example of going from +tlrc space to +orig space",
   NULL
 } ,

 { 30, NOV, 2009, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "afni crashes on short ANALYZE file from double fclose()",
   NULL
 } ,

 { 16, NOV, 2009, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "allow motion censoring with varying run lengths",
   "Also, if a max is applied in scaling, explicitly limit to [0,max].\n"
 } ,

 { 16, NOV, 2009, RCR, "1d_tool.py", MINOR, TYPE_ENHANCE,
   "allow motion censoring with varying run lengths",
   NULL
 } ,

 {  4, NOV, 2009, RCR, "Dimon", MICRO, TYPE_MODIFY,
   "small change to check on sort problems",
   NULL
 } ,

 { 27, OCT, 2009, RCR, "GIFTI", MINOR, TYPE_ENHANCE,
   "added support for optional LabelTable RGBA attributes",
   NULL
 } ,

 { 23, OCT, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -censor_fill and -censor_fill_par",
   "These options are to zero-pad TRs that were censored by 3dDeconvolve."
 } ,

 { 19, OCT, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options for using 3dBlurInMask, instead of 3dmerge",
   "- added -blur_in_mask, -blur_in_automask and -blur_opts_BIM\n"
   "- added -sep_char and -subj_curly"
 } ,

 { 16, OCT, 2009, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added -demean, to demean motion parameters, for example",
   "The polort 0 values should be more accurate baseline constants.\n"
   "Useful for creating a proper polort baseline w/3dSynthesize."
 } ,

 { 14, OCT, 2009, RCR, "3dTcat", MICRO, TYPE_ENHANCE,
   "allow creation of single volume dataset",
   "as requested by N Vack (among many others)"
 } ,

 { 6, OCT, 2009, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added -set_run_lengths option, for varying run lengths",
   "Added for motion censoring and run padding."
 } ,

 { 2, OCT, 2009, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "also output cosines with -show_cormat_warnings",
   NULL
 } ,

 { 1, OCT, 2009, RCR, "@Reorder", MICRO, TYPE_MODIFY,
   "minor changes:",
   "- changed warnings on varying incidence counts (JB's woriding)\n"
   "- discard indices which are not valid sub-bricks\n"
   "- do not call 3dTstat if only one sub-brick"
 } ,

 { 1, OCT, 2009, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "9/29, defined isblank() in case of SOLARIS_OLD ... and then removed it",
   NULL
 } ,

 { 29, SEP, 2009, RCR, "@Reorder", MINOR, TYPE_NEW_PROG,
   "script version of reorder plugin, for J. Bjork",
   NULL
 } ,

 { 16, SEP, 2009, RCR, "timing_tool.py", MICRO, TYPE_NEW_OPT,
   "added -scale_data for J Meltzer",
   NULL
 } ,

 { 16, SEP, 2009, RCR, "plug_vol2surf", MICRO, TYPE_ENHANCE,
   "can init debug level via AFNI_DEBUG_PLUG_VOL2SURF",
   NULL
 } ,

 { 16, SEP, 2009, RCR, "3ddot", MINOR, TYPE_BUG_FIX,
   "de-meaning data causes permission-based seg fault, apply means upon read",
   "Problem found by Giuseppe Pagnoni."
 } ,

 {  8, SEP, 2009, RCR, "realtime_receiver.py", MICRO, TYPE_MODIFY,
   "bind to open host, so a /etc/hosts entry is not required",
   NULL
 } ,

 {  4, SEP, 2009, RCR, "plug_realtime", MINOR, TYPE_ENHANCE,
   "registration can now be consistent across runs",
   "Reg Base can be 'Current': to set the base volume from the current run\n"
   "(call this the old way), 'Current & Keep': use current run, but then\n"
   "store that base and apply it to future runs, 'External Dataset': fix the\n"
   "base from some chosen dataset.\n"
   "\n"
   "Using 'Current & Keep' makes sense for realtime registration.\n"
 } ,

 {  4, SEP, 2009, RCR, "@update.afni.binaries", MICRO, TYPE_NEW_OPT,
   "if wget fails, try curl; added -curl and -testing options",
   NULL
 } ,

 { 28, AUG, 2009, RCR, "afni_util.py", MICRO, TYPE_BUG_FIX,
   "fixed make_CENSORTR_string, comma delimitation needs run: prefix",
   NULL
 } ,

 { 27, AUG, 2009, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed motion_ prefix in '3dDeconvolve -censor'",
   "Problem found by B Bones."
 } ,

 { 27, AUG, 2009, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -regress_local_times, -regress_global_times",
   "Since the -local_times and -global_times options in 3dDeconvolve must be\n"
   "processed before the stimuli they refer to, it does nothing to pass them\n"
   "via -regress_opts_3dD.  Hence, the options are needed.\n"
 } ,

 { 26, AUG, 2009, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "in scaling block, explicitly remove any negative data values",
   NULL
 } ,

 { 25, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_ENHANCE,
   "if volreg block, always create motion_${subj}_enorm.1D",
   NULL
 } ,

 { 25, AUG, 2009, RCR, "1d_tool.py", MICRO, TYPE_ENHANCE,
   "with -censor_motion, also output PREFIX_enorm.1D",
   NULL
 } ,

 { 21, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_censor_motion and -regress_censor_prev",
   "Motivated by L Thomas and B Bones."
 } ,

 { 21, AUG, 2009, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added -show_censor_count",
   NULL
 } ,

 { 20, AUG, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added motion censoring options",
   "Added -censor_motion, -censor_prev_TR,  -collapse_cols, -extreme_mask,\n"
   "      -set_tr, -write_censor and -write_CENSORTR.\n"
   "Also modified afni_util.py, lib_afni1D.py and option_list.py."
 } ,

 { 14, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -align_epi_ext_dset, to align anat to external EPI",
   "This may be important for multi-channel coil EPI data with low internal\n"
   "structural contrast.  Users might align to the first (pre-steady-state)\n"
   "TR, even though that volume is not used in the analysis."
 } ,

 { 13, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -volreg_tlrc_adwarp, to apply a manual Talairach transformation",
   NULL
 } ,

 { 10, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "truncate min dim to 3 sig bits for -volreg_tlrc_warp/-volreg_align_e2s",
   "The old default was 2 bits, -volreg_warp_dxyz overrides."
 } ,

 { 10, AUG, 2009, RCR, "3dSurf2Vol", MINOR, TYPE_ENHANCE,
   "allow processing of -overwrite and AFNI_DECONFLICT",
   NULL
 } ,

 {  6, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed problems found by I Mukai and K Bahadur",
   "- fixed -volreg_align_to base as applied in align_epi_anat.py\n"
   "- fixed blur 'averages' computation when only one run"
 } ,

 {  4, AUG, 2009, RCR, "realtime_receiver.py", MINOR, TYPE_ENHANCE,
   "added basic demo interface and itemized exception traps",
   NULL
 } ,

 { 31, JUL, 2009, RCR, "prompt_user", MICRO, TYPE_ENHANCE,
   "apply some escape sequences, mostly to display newlines",
   NULL
 } ,

 { 29, JUL, 2009, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed creation of extents mask when only 1 run",
   NULL
 } ,

 { 28, JUL, 2009, RCR, "3dREMLfit", MINOR, TYPE_MODIFY,
   "if known, require proper slice regressor ordering in -slibase* opts\n",
   NULL
 } ,

 { 27, JUL, 2009, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "use -slibase_sm instead of -slibase in 3dREMLfit",
   NULL
 } ,

 { 27, JUL, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_labels and -show_label_ordering",
   NULL
 } ,

 { 27, JUL, 2009, RCR, "3dREMLfit", MINOR, TYPE_NEW_OPT,
   "added -slibase_sm, for slice-major ordering of regressors",
   "RetroTS and afni_proc.py were incorrectly using this ordering.\n"
   "** Analysis done prior to this probably needs to be re-done."
 } ,

 { 27, JUL, 2009, RCR, "plug_realtime", MICRO, TYPE_ENHANCE,
   "added Y/N AFNI_REALTIME_reset_output_index, to start each dset at 001",
   "Also, changed prefix separator to double underscore '__'.",
 } ,

 { 27, JUL, 2009, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "added SOLARIS_OLD atanhf #define to machdep.h",
   NULL
 } ,

 { 23, JUL, 2009, RCR, "afni_run_R", MINOR, TYPE_ENHANCE,
   "allow any number of args, but where first is program, last is output",
   NULL
 } ,

 { 23, JUL, 2009, RCR, "timing_tool.py", MINOR, TYPE_NEW_OPT,
   "added -partition option",
   NULL
 } ,

 { 22, JUL, 2009, RCR, "realtime_receiver.py", MAJOR, TYPE_NEW_PROG,
   "python replacement for serial helper",
   "New 'data_choice' options can be added to compute_data_for_serial_port\n"
   "for sending results of a different computation to the serial port."
 } ,

 { 16, JUL, 2009, RCR, "@update.afni.binaries", MICRO, TYPE_MODIFY,
   "check for 'wget' and whine to user if missing",
   NULL
 } ,

 { 14, JUL, 2009, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added -max_rest, to limit the maximum duration of rest periods",
   NULL
 } ,

 { 7, JUL, 2009, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "warn users to modify script for _AM1 in case of basis function dmBLOCK",
   NULL
 } ,

 { 26, JUN, 2009, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "comment changes and mod to afni_util.py for line wrapping",
   NULL
 } ,

 { 25, JUN, 2009, RCR, "Dimon", MINOR, TYPE_BUG_FIX,
   "fixed dz sent to RT plugin for oblique datasets",
   NULL
 } ,

 { 25, JUN, 2009, RCR, "3dretroicor", MICRO, TYPE_BUG_FIX,
   "pass MRI_IMAGE structs without const",
   NULL
 } ,

 { 24, JUN, 2009, RCR, "afni-general", MINOR, TYPE_BUG_FIX,
   "applied print changes from B Feige (26 files):",
   "3dfim.c afni.c afni_niml.c mri_free.c mrilib.h 3dmatmult.c NLfit_model.c\n"
   "suma_datasets.h gifti/gifti_tool.c rickr/serial_helper.c\n"
   "SUMA/  SUMA_3dSurfMask.c SUMA_ConvertSurface.c SUMA_CreateIcosahedron.c\n"
   "       SUMA_Load_Surface_Object.c SUMA_MapIcosahedron.c SUMA_NikoMap.c\n"
   "       SUMA_ParseCommands.h SUMA_SphericalMapping.c\n"
   "       SUMA_Surf2VolCoord_demo.c SUMA_Surface_IO.c SUMA_SurfWarp.c\n"
   "       SUMA_compare_surfaces.c SUMA_xColBar.c\n"
   "svm/3dsvm.c svm/3dsvm_common.c volpack/vp_octree.c"
 } ,

 { 23, JUN, 2009, RCR, "NIFTI", MINOR, TYPE_BUG_FIX,
   "added 4 checks of alloc() returns",
   NULL
 } ,

 { 17, JUN, 2009, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added -make_3dd_contrasts and used general accuracy in block durations",
   NULL
 } ,

 { 17, JUN, 2009, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "version 2.0 : call e2a alignment and warp to standard space ready",
   "- mask warped EPI by its extents (at volreg step)\n"
   "- added -volreg_no_extent_mask, to block this masking\n"
   "- added 'extents' to list of mask in -mask_apply\n"
   "- change block dividers to more visual '===' with block names"
 } ,

 { 15, JUN, 2009, RCR, "3dmerge", MINOR, TYPE_ENHANCE,
   "allowed short/byte datasets to use FIR blur, so no Fourier interpolation",
   "Also added Y/N AFNI_BLUR_INTS_AS_OLD env var to use previous method."
 } ,

 { 12, JUN, 2009, RCR, "xmat_tool.py", MICRO, TYPE_GENERAL,
   "used some wx IDs, per Daniel's suggestion",
   NULL
 } ,

 { 11, JUN, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added masking abilities",
   "- in mask block, try to create anat and group masks\n"
   "- added -mask_apply option, for choosing mask to apply to regression\n"
   "- added -align_opts_aea, for extra opts to align_epi_anat.py"
 } ,

 { 8, JUN, 2009, RCR, "afni_proc.py", MICRO, TYPE_NEW_OPT,
   "added -despike_mask, fixed missing block warning, reordered terminal opts",
   NULL
 } ,

 { 8, JUN, 2009, RCR, "afni_base.py", MICRO, TYPE_GENERAL,
   "added many afni_name descripts to __doc__ lines, check error in dset_dims",
   NULL
 } ,

 { 3, JUN, 2009, RCR, "3dcopy", MICRO, TYPE_MODIFY,
   "changed 'missing dataset' ERROR to 'missing view dataset' WARNING",
   NULL
 } ,

 { 29, MAY, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -execute and now fail if block options have no corresponding blocks",
   NULL
 } ,

 { 29, MAY, 2009, RCR, "@build_afni_Xlib", MICRO, TYPE_MODIFY,
   "added -m64 if building for lib64 on a mac, fixed CFLAGS to allow a list",
   NULL
 } ,

 { 29, MAY, 2009, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "improved line wrapping",
   NULL
 } ,

 { 28, MAY, 2009, RCR, "strblast", MINOR, TYPE_BUG_FIX,
   "partial words had resulted in skipping ahead",
   "found by R Notestine of UCSD"
 } ,

 { 28, MAY, 2009, RCR, "afni_proc.py", MINOR, TYPE_GENERAL,
   "example updates for AFNI_data4 and new options",
   NULL
 } ,

 { 27, MAY, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "updates for alignment/warp/varying run lengths",
   "- added -volreg_warp_dxyz option\n"
   "- if align a2e, add -no_ss to @auto_tlrc\n"
   "- for varying run lengths, fixed application of '-volreg_align_to last'\n"
   "  and the -regress_est_blur_* options\n"
   "  (blur estimation loops were modified for this)\n"
   "- warping to new grid truncates to 2 significant bits (if < 2 mm)"
 } ,

 { 21, MAY, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added 'align' processing block and -volreg_align_e2a option",
   NULL
 } ,

 { 19, MAY, 2009, RCR, "3dbucket", MICRO, TYPE_GENERAL,
   "suggest -overwrite if -glueto is not allowed (for Mike B)",
   NULL
 } ,

 { 15, MAY, 2009, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "added -volreg_tlrc_warp option: can warp to standard space at volreg step",
   NULL
 } ,

 { 15, MAY, 2009, RCR, "afni_util", MICRO, TYPE_NEW_OPT,
   "added get_truncated_grid_dim",
   NULL
 } ,

 { 14, MAY, 2009, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "no 'rm rm.*' if such files were not created",
   NULL
 } ,

 { 14, MAY, 2009, RCR, "3dDeconvolve", MICRO, TYPE_BUG_FIX,
   "fixed -glt_label > -num_glt error message and -dmbase def for polort >= 0",
   NULL
 } ,

 { 12, MAY, 2009, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed 'cat' of 'across-runs' ricor regressors",
   NULL
 } ,

 { 8, MAY, 2009, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "tlrc (for anat) is now a processing block, for easy manipulation",
   NULL
 } ,

 { 8, MAY, 2009, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "small cut-n-paste errors in db_mod.py and afni_util.py",
   NULL
 } ,

 { 5, MAY, 2009, RCR, "@update.afni.binaries", MINOR, TYPE_NEW_PROG,
   "another script to update the AFNI package",
   NULL
 } ,

 { 5, MAY, 2009, RCR, "plug_render", MINOR, TYPE_MODIFY,
   "now built from plug_null.c, so it will no longer be available",
   NULL
 } ,

 { 1, MAY, 2009, RCR, "Makefile.linux_xorg7_64", MICRO, TYPE_MODIFY,
   "link to local libGLws.a, as with 32-bit xorg7 package",
   NULL
 } ,

 { 30, APR, 2009, RCR, "3dcalc", MINOR, TYPE_MODIFY,
   "changed atan2(y,x) to proceed if y OR x is non-zero",
   NULL
 } ,

 { 30, APR, 2009, RCR, "thd_niftiread", MINOR, TYPE_MODIFY,
   "be sure to warn users when nifti is converted to float (w/dglen)",
   NULL
 } ,

 { 30, APR, 2009, RCR, "vol2surf", MINOR, TYPE_NEW_OPT,
   "return a node v2s time series when afni changes xhair position",
   NULL
 } ,

 { 29, APR, 2009, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "when opening -geomparent, allow for known non-afni extensions",
   NULL
 } ,

 { 29, APR, 2009, RCR, "vol2surf", MICRO, TYPE_MODIFY,
   "prep to return node v2s time series to suma, just check-in for now",
   "modified: afni.h, afni_niml.c, afni_vol2surf.c, vol2surf.c, vol2surf.h"
 } ,

 { 28, APR, 2009, RCR, "NIFTI", MINOR, TYPE_MODIFY,
   "uppercase file extensions are now valid",
   NULL
 } ,

 { 27, APR, 2009, RCR, "3dresample", MICRO, TYPE_MODIFY,
   "show help if no arguments",
   NULL
 } ,

 { 23, APR, 2009, RCR, "3dresample", MICRO, TYPE_MODIFY,
   "small changes to help",
   NULL
 } ,

 { 23, APR, 2009, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "moved function comments into the functions as docstrings",
   NULL
 } ,

 { 23, APR, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "updates to help and tcsh options",
   "- added -f as a recommended tcsh option\n"
   "- added help section 'SCRIPT EXECUTION NOTE'\n"
   "- reordered help: intro, BLOCKS, DEFAULTS, EXAMPLES, NOTEs, OPTIONS\n"
   "- shifted execution command to separate line"
 } ,

 { 17, APR, 2009, RCR, "GIFTI", MICRO, TYPE_MODIFY,
   "more -set_extern_filelist help, allow DA size to vary over external files",
   NULL
 } ,

 { 14, APR, 2009, RCR, "afni_util.py", MICRO, TYPE_BUG_FIX,
   "possible to have truncation cause a negative variance in stdev_ub",
   NULL
 } ,

 { 14, APR, 2009, RCR, "NIFTI", MINOR, TYPE_NEW_OPT,
   "added sample nifticlib program: clib_01_read_write.c",
   NULL
 } ,

 { 11, APR, 2009, RCR, "afni-general", MICRO, TYPE_NEW_OPT,
   "added calls to opts.check_special_opts() in 7 more python programs:",
   "gen_epi_review.py, make_random_timing.py, make_stim_times.py\n"
   "neuro_deconvolve.py, python_module_test.py, timing_tool.py, ui_xmat.py"
 } ,

 { 11, APR, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -volreg_regress_per_run",
   "This is to apply the motion parameters of each run as separate regressors."
 } ,

 { 11, APR, 2009, RCR, "option_list.py", MICRO, TYPE_NEW_OPT,
   "enhanced special option processing",
   "- added check_special_opts(), to be called before any processing\n"
   "- renamed -verbose_opts to -optlist_verbose\n"
   "- added -optlist_no_show_count"
 } ,

 { 11, APR, 2009, RCR, "eg_main_chrono.py", MICRO, TYPE_MODIFY,
   "removed -verbose opts (see -optlist_ options)",
   NULL
 } ,

 { 11, APR, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -derivative and -set_nruns, fixed -show_cormat_warnings typo",
   NULL
 } ,

 { 11, APR, 2009, RCR, "afni_proc.py", MICRO, TYPE_BUG_FIX,
   "fixed use of -regress_errts_prefix with blur est",
   NULL
 } ,

 { 10, APR, 2009, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -use_last_elem option for setting DICOM elements",
   NULL
 } ,

 { 10, APR, 2009, RCR, "to3d", MINOR, TYPE_NEW_OPT,
   "added -use_last_elem option for setting DICOM elements",
   "Can also use AFNI_DICOM_USE_LAST_ELEMENT environment variable."
 } ,

 { 10, APR, 2009, RCR, "1d_tool.py", MINOR, TYPE_MODIFY,
   "fix for old versions of python, like on solaris",
   "Each of copy.deepcopy(), sum(), and sort(reverse=True) failed."
 } ,

 { 9, APR, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_cormat_warnings and -cormat_cutoff",
   NULL
 } ,

 { 9, APR, 2009, RCR, "afni_proc.py", MAJOR, TYPE_NEW_OPT,
   "'official' release with RETROICOR processing block: ricor",
   "o  added 'across-runs' ricor_regress_method\n"
   "o  added ricor information and usage to help (see 'RETROICOR NOTE')\n"
   "o  maintain unscaled shorts if they are input\n"
   "o  added -ricor_datum"
 } ,

 { 8, APR, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -show_rows_cols option",
   NULL
 } ,

 { 2, APR, 2009, RCR, "3dDeconvolve", MINOR, TYPE_MODIFY,
   "changed CHECK_NIFTI to CHECK_NEEDS_FLOATS, including other dset types",
   NULL
 } ,

 { 1, APR, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "added 'ricor' processing block, for RETROICOR regressor removal",
   NULL
 } ,

 { 1, APR, 2009, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "slight change in add_line_wrapper()",
   NULL
 } ,

 { 31, MAR, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "small changes, and prep for retroicor",
   "- by default, the script will now terminate on any error\n"
   "- added -exit_on_error, -check_setup_errors\n"
   "- whine about block order problems"
 } ,

 { 31, MAR, 2009, RCR, "1d_tool.py", MINOR, TYPE_NEW_OPT,
   "added -pad_to_many_runs, -reverse",
   NULL
 } ,

 { 31, MAR, 2009, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "added get_typed_dset_attr_list, enhanced decode_1D_ints",
   NULL
 } ,

 { 31, MAR, 2009, RCR, "option_list.py", MICRO, TYPE_NEW_OPT,
   "added global -verbose_opts option",
   NULL
 } ,

 { 26, MAR, 2009, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "added helpstr to options",
   NULL
 } ,

 { 26, MAR, 2009, RCR, "1d_tool.py", MICRO, TYPE_BUG_FIX,
   "small array fix for older python in write()",
   NULL
 } ,

 { 26, MAR, 2009, RCR, "option_list.py", MICRO, TYPE_MODIFY,
   "base 'name' size on max len in show()",
   NULL
 } ,

 { 25, MAR, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "+view now comes from data: so it works with +tlrc",
   NULL
 } ,

 { 24, MAR, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "by default now, no mask is applied in the scale and regression steps",
   "Also added -regress_apply_mask option."
 } ,

 { 20, MAR, 2009, RCR, "@build_afni_Xlib", MINOR, TYPE_NEW_PROG,
   "moved from X/@build.Xlib, for distribution",
   "and added to SCRIPTS for building afni_src.tgz in Makefile.INCLUDE"
 } ,

 { 19, MAR, 2009, RCR, "1d_tool.py", MAJOR, TYPE_NEW_PROG,
   "added lib_afni1D.py and 1d_tool.py",
   "This is a library and tool for manipulating 1D files.\n"
   "Many functions will still be added."
 } ,

 { 19, MAR, 2009, RCR, "afni_util.py", MINOR, TYPE_NEW_OPT,
   "a few additions and changes",
   "- allow container chars (e.g. []) in decode_1D_ints()\n"
   "- added is_valid_int_list()\n"
   "- changed str vars to istr (as str is a keyword)"
 } ,

 { 19, MAR, 2009, RCR, "eg_main_chrono.py", MINOR, TYPE_NEW_OPT,
   "added -verbose_opts option, for being verbose during option processing",
   NULL
 } ,

 { 18, MAR, 2009, RCR, "eg_main_chrono.py", MINOR, TYPE_NEW_PROG,
   "sample main python program using a library and chronological options",
   NULL
 } ,

 { 16, MAR, 2009, RCR, "3dBrickStat", MINOR, TYPE_BUG_FIX,
   "malloc extra val in case of percentile truncation",
   NULL
 } ,

 { 12, MAR, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "warn user about masking in orig space",
   "- if despiking and no regression mask, apply -nomask\n"
   "- added 'MASKING NOTE', to suggest no regression mask until group space"
 } ,

 { 12, MAR, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_reml_exec and -regress_3dD_stop",
   "One can execute 3dREMLfit and/or 3dDeconvolve.  Error blur is from each."
 } ,

 { 10, MAR, 2009, RCR, "NIFTI", MICRO, TYPE_MODIFY,
   "added NIFTI_ECODEs 18-28 for the LONI MiND group",
   NULL
 } ,

 { 9, MAR, 2009, RCR, "3dcalc", MICRO, TYPE_MODIFY,
   "added edge/erode/dilate example to 3dcalc -help",
   NULL
 } ,

 { 9, MAR, 2009, RCR, "suma-general", MICRO, TYPE_MODIFY,
   "removed r_sprintf_long_to_hex from SUMA_Color.[ch]",
   NULL
 } ,

 { 9, MAR, 2009, RCR, "suma", MICRO, TYPE_NEW_OPT,
   "added -motif_ver option",
   NULL
 } ,

 { 6, MAR, 2009, RCR, "lesstif-general", MICRO, TYPE_MODIFY,
   "motif/lesstif : put AFNI_MOTIF_TYPE in Xm.h.in : see 'afni -motif_ver'",
   NULL
 } ,

 { 6, MAR, 2009, RCR, "3dDeconvolve", MINOR, TYPE_BUG_FIX,
   "if mri_automask_image() input is not really 3D, only apply clip",
   "3dD uses automask for misfit warning, let this apply to niml.dset"
 } ,

 { 5, MAR, 2009, RCR, "lesstif-general", MICRO, TYPE_MODIFY,
   "init for every assignable argument to XtVaGetValues (12 files)",
   NULL
 } ,

 { 5, MAR, 2009, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "init for every assignable argument to XtVaGetValues (19 files)",
   NULL
 } ,

 { 5, MAR, 2009, RCR, "@build.Xlib", MICRO, TYPE_NEW_OPT,
   "change -noinstall option to -localinstall",
   NULL
 } ,

 { 5, MAR, 2009, RCR, "afni", MINOR, TYPE_BUG_FIX,
   "free vox_warp via KILL_list rather than directly when deleting dataset",
   "Fixes afni crash: set acpc markers -> acpc view -> orig view \n"
   "               -> new markers -> acpc view -> death ..."
 } ,

 { 4, MAR, 2009, RCR, "@build.Xlib", MICRO, TYPE_NEW_OPT,
   "added -noinstall option",
   NULL
 } ,

 { 4, MAR, 2009, RCR, "afni", MICRO, TYPE_NEW_OPT,
   "added -motif_ver option",
   NULL
 } ,

 { 4, MAR, 2009, RCR, "X-general", MINOR, TYPE_MODIFY,
   "added openmotif build tree, updated @build.Xlib and README under X",
   NULL
 } ,

 { 3, MAR, 2009, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "modified Makefile.linux_xorg7 and _64 for local X builds",
   NULL
 } ,

 { 3, MAR, 2009, RCR, "xutil.c", MICRO, TYPE_MODIFY,
   "another probably useless init (being cautious)",
   NULL
 } ,

 { 3, MAR, 2009, RCR, "@build.Xlib", MINOR, TYPE_NEW_PROG,
   "this is a build script for the local X packages",
   NULL
 } ,

 { 27, FEB, 2009, RCR, "X-general", MINOR, TYPE_MODIFY,
   "added lesstif and libXt trees to cvs",
   NULL
 } ,

 { 20, FEB, 2009, RCR, "afni-general", MICRO, TYPE_BUG_FIX,
   "many inits to appease lesstif and Xt (w/Ziad)",
   NULL
 } ,

 { 13, FEB, 2009, RCR, "dmat44.c", MICRO, TYPE_BUG_FIX,
   "cut-and-paste error ...",
   NULL
 } ,

 { 13, FEB, 2009, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "malloc changes: 5 more files",
   "Friday the 13th, oooooooo...  @ 18:31:30 EST: time will be 1234567890."
 } ,

 { 12, FEB, 2009, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "added memsets following some malloc calls, or used calloc (14 files)",
   NULL
 } ,

 { 11, FEB, 2009, RCR, "3dDeconvolve", MICRO, TYPE_MODIFY,
   "removed duplicate -Rerrts option in output 3dREMLfit command",
   NULL
 } ,

 { 9, FEB, 2009, RCR, "xmat_tool.py", MINOR, TYPE_MODIFY,
   "random updates, plus those for Fedora 10",
   NULL
 } ,

 { 9, FEB, 2009, RCR, "python-general", MICRO, TYPE_GENERAL,
   "added new beginning-stage libraries lib_matplot.py and lib_wx.py",
   NULL
 } ,

 { 6, FEB, 2009, RCR, "NIFTI", MICRO, TYPE_MODIFY,
   "added NIFTI_ECODE_PYPICKLE for MH; imported HJ's cast changes",
   NULL
 } ,

 { 5, FEB, 2009, RCR, "make_random_timing.py", MICRO, TYPE_MODIFY,
   "added timing_tool.py use to sort times in example #7",
   NULL
 } ,

 { 4, FEB, 2009, RCR, "vol2surf", MINOR, TYPE_BUG_FIX,
   "fixed norm reversal application and norm dir check computation",
   "Thanks to Xiaopeng Zong for finding these problems."
 } ,

 { 4, FEB, 2009, RCR, "suma-general", MICRO, TYPE_MODIFY,
   "update SUMA_paperplane.c and Makefile.solaris28_gcc for v1280 builds",
   "Makefile now uses PREREQ=suma, gmake, -L/usr/dt/lib."
 } ,

 { 3, FEB, 2009, RCR, "afni-general", MICRO, TYPE_MODIFY,
   "fix machdep.h Makefile.solaris28_gcc for v1280 builds",
   NULL
 } ,

 { 7, JAN, 2009, RCR, "plug_crender", MICRO, TYPE_MODIFY,
   "if lesstif, set threshold slider bar width",
   NULL
 } ,

 { 2, JAN, 2009, RCR, "Makefile", MICRO, TYPE_MODIFY,
   "do not build balloon in Makefile.macosx_10.5_Intel_64 - libgsl is 32-bit",
   NULL
 } ,

 { 2, JAN, 2009, RCR, "afni_environ.c", MICRO, TYPE_BUG_FIX,
   "fixed bad lvalue when USE_TRACING is not defined",
   "also fixed define for USE_TRACING in solaris and cygwin Makefiles"
 } ,

 { 31, DEC, 2008, RCR, "afni", MINOR, TYPE_BUG_FIX,
   "fix for lesstif crash on 'where am i', along with ziad",
   NULL
 } ,

 { 24, DEC, 2008, RCR, "timing_tool.py", MICRO, TYPE_MODIFY,
   "redefine 'sum' for older python versions",
   "This also affects afni_util.py and make_random_timing.py."
 } ,

 { 15, DEC, 2008, RCR, "Makefile", MICRO, TYPE_MODIFY,
   "added USE_LESSTIF directive",
   "Modified Makefile.linux_xorg7[_64], Makefile.macosx_10.5_Intel[_64]."
 } ,

 { 10, DEC, 2008, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added new options for extra stimuli, RONI and an external volreg base",
   "- allow NIfTI datasets as input (but process as AFNI)\n"
   "- added -regress_extra_stim_files and -regress_extra_stim_labels\n"
   "- added -regress_RONI and -volreg_base_dset (for Jill Weisberg)"
 } ,

 {  8, DEC, 2008, RCR, "xmat_tool.py", MICRO, TYPE_MODIFY,
   "allow -test_libs to proceed without numpy",
   NULL
 } ,

 {  8, DEC, 2008, RCR, "Makefile", MICRO, TYPE_GENERAL,
   "added Makefile.macosx_10.5_G4",
   NULL
 } ,

 {  4, DEC, 2008, RCR, "Makefile.INCLUDE", MINOR, TYPE_MODIFY,
   "added balloon target for M Belmonte",
   "Also modified Makefile.linux_xorg7_64 and macosx_10.4_G5/Intel and 5_Int*."
 } ,

 {  4, DEC, 2008, RCR, "balloon", MICRO, TYPE_NEW_PROG,
   "new program by M Belmonte",
   NULL
 } ,

 {  1, DEC, 2008, RCR, "timing_tool.py", MAJOR, TYPE_NEW_PROG,
   "a tool for manipulating and evaluating stimulus timing files",
   "This is useful for getting statistics on rest timing."
 } ,

 {  1, DEC, 2008, RCR, "option_list.py", MICRO, TYPE_MODIFY,
   "added 'opt' param to more get_* functions",
   NULL
 } ,

 {  1, DEC, 2008, RCR, "make_random_timing.py", MICRO, TYPE_MODIFY,
   "moved min_mean_max_stdev to afni_util.py and modified help examples",
   NULL
 } ,

 {  24, NOV, 2008, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added options -infile_list and -show_sorted_list",
   "The -show_sorted_list option will print a list of files by run/index."
 } ,

 {  21, NOV, 2008, RCR, "xmat_tool.py", MINOR, TYPE_NEW_OPT,
   "added Options menu, Show Cosmat and GUI help",
   "This is the initial release version, 1.0."
 } ,

 {  21, NOV, 2008, RCR, "xmat_tool.py", MINOR, TYPE_NEW_OPT,
   "added -test_libs option",
   NULL
 } ,

 {  21, NOV, 2008, RCR, "python_module_test.py", MINOR, TYPE_NEW_OPT,
   "removed 'R' from basic test list, and applied verb 2 to base usage",
   NULL
 } ,

 {  20, NOV, 2008, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
   "incorporated real-time volume writing from V. Roopchansingh of MCW",
   NULL
 } ,

 {  18, NOV, 2008, RCR, "xmat_tool.py", MINOR, TYPE_NEW_OPT,
   "added -test, -show_col_types, -show_cosmat, -show_fit_ts, -cormat_cutoff",
   "also added the main help"
 } ,

 {  18, NOV, 2008, RCR, "afni_xmat.py", MICRO, TYPE_MODIFY,
   "added extra_cols param to make_show_conds_str",
   NULL
 } ,

 {  7, NOV, 2008, RCR, "xmat_tool.py", MINOR, TYPE_MODIFY,
   "more updates:",
   "- scipy is only tested for when necessary\n"
   "- compute norms locally if no scipy\n"
   "- solve_against_1D, linear_combo: return error string instead of code\n"
   "- added -chrono option, to make all options chronological\n"
   "  (so options are essentially scriptable)"
 } ,

 {  6, NOV, 2008, RCR, "xmat_tool.py", MINOR, TYPE_NEW_OPT,
   "pre-release updates:",
   "  - added many initial command-line options\n"
   "  - added plot_xmat_as_one toggle button\n"
   "  - added computation of cosine matrix and cosmat_warnings\n"
   "  - separated GUI code into new file gui_xmat.py"
 } ,

 {  6, NOV, 2008, RCR, "option_list.py", MICRO, TYPE_NEW_OPT,
   "added opt param to get_type_opt and get_type_list",
   "had to modify calls in make_random_timing.py and gen_epi_review.py"
 } ,

 {  6, NOV, 2008, RCR, "python_module_test.py", MINOR, TYPE_NEW_OPT,
   "added option -full_test",
   NULL
 } ,

 {  4, NOV, 2008, RCR, "plug_vol2surf", MICRO, TYPE_MODIFY,
   "fail if NIML output dataset does end in .niml.dset",
   NULL
 } ,

 {  4, NOV, 2008, RCR, "3dVol2Surf", MICRO, TYPE_MODIFY,
   "fail if NIML output dataset does end in .niml.dset",
   NULL
 } ,

 {  4, NOV, 2008, RCR, "vol2surf", MICRO, TYPE_MODIFY,
   "only complain about statsym_string in debug mode",
   NULL
 } ,

 { 31, OCT, 2008, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "moved functions encode_1D_ints and decode_1D_ints here",
   NULL
 } ,

 { 31, OCT, 2008, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added -show_timing_stats option",
   "Also, made a small change affecting timing (old results will not match)."
 } ,

 { 29, OCT, 2008, RCR, "xmat_tool.py", MINOR, TYPE_MODIFY,
   "if the X-matrix has a constant regressor, do not de-mean it",
   "In such a case, the cormat would not exactly be a correlation matrix."
 } ,

 { 29, OCT, 2008, RCR, "python_module_test.py", MINOR, TYPE_NEW_PROG,
   "program to test python module imports (interface to module_test_lib.py)",
   NULL
 } ,

 { 28, OCT, 2008, RCR, "module_test_lib.py", MINOR, TYPE_NEW_PROG,
   "library to test python module imports",
   "One might want to apply this module at the top of any python file."
 } ,

 { 28, OCT, 2008, RCR, "xmat_tool.py", MICRO, TYPE_MODIFY,
   "use module_test_lib to test imports",
   NULL
 } ,

 { 27, OCT, 2008, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_motion_file option",
   NULL
 } ,

 { 27, OCT, 2008, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added -offset option",
   NULL
 } ,

 { 27, OCT, 2008, RCR, "make_random_timing.py", MINOR, TYPE_BUG_FIX,
   "actually applied -min_rest, sorry...",
   NULL
 } ,

 { 24, OCT, 2008, RCR, "xmat_tool.py", SUPER, TYPE_NEW_PROG,
   "program to inspect a .xmat.1D X-matrix, possibly against a time series",
   "This is a Graphical tool for plotting a design matrix, reviewing\n"
   "condition numbers or the correlation matrix, and fitting to a 1D\n"
   "time series."
 } ,

 { 23, OCT, 2008, RCR, "Makefile.INCLUDE", MINOR, TYPE_BUG_FIX,
   "removed reference to cdflib, for 'make afni_src.tgz'",
   NULL
 } ,

 { 23, OCT, 2008, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "added lists_are_same function",
   NULL
 } ,

 { 20, OCT, 2008, RCR, "afni_util.py", MINOR, TYPE_NEW_OPT,
   "added write_text_to_file function",
   NULL
 } ,

 { 20, OCT, 2008, RCR, "afni_driver.c", MINOR, TYPE_NEW_OPT,
   "added 'GETENV' to the list of DRIVE_AFNI commands",
   NULL
 } ,

 { 16, OCT, 2008, RCR, "thd_mastery", MICRO, TYPE_MODIFY,
   "THD_copy_dset_subs should not need to add a warp structure",
   NULL
 } ,

 { 14, OCT, 2008, RCR, "thd_mastery", MICRO, TYPE_MODIFY,
   "verify sub-brick list in THD_copy_dset_subs()",
   NULL
 } ,

 { 14, OCT, 2008, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "added wrap string param to add_line_wrappers (to wrap with newlines)",
   NULL
 } ,

 { 8, OCT, 2008, RCR, "NIFTI", MICRO, TYPE_MODIFY,
   "allow cbl with indices in 0..nt*nu*nv*nw-1",
   NULL
 } ,

 { 7, OCT, 2008, RCR, "NIFTI", MICRO, TYPE_MODIFY,
   "added nifti_NBL_matches_nim() check for write_bricks()",
   NULL
 } ,

 { 2, OCT, 2008, RCR, "GIFTI", MICRO, TYPE_MODIFY,
   "minor changes",
   "- separate diffs in DAs from those in gifti_image\n"
   "- decode additional data types: INT8, UINT16, INT64\n"
   "- add link flags to libgiftiio_la target"
 } ,

 { 29, SEP, 2008, RCR, "3dmatmult", MAJOR, TYPE_NEW_PROG,
   "program to multiply AFNI datasets slice-by-slice as matrices",
   NULL
 } ,

 { 23, SEP, 2008, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -remove_preproc_files option (akin to -move_preproc_files)",
   NULL
 } ,

 { 23, SEP, 2008, RCR, "gen_epi_review.py", MINOR, TYPE_MODIFY,
   "in script, check for existence of given datasets\n",
   NULL
 } ,

 { 17, SEP, 2008, RCR, "make_stim_times.py", MINOR, TYPE_NEW_OPT,
   "added -labels option, for including labels in filenames",
   NULL
 } ,

 { 16, SEP, 2008, RCR, "3drefit", MINOR, TYPE_BUG_FIX,
   "allow attribute editing of NIfTI datasets",
   NULL
 } ,

 { 10, SEP, 2008, RCR, "plug_realtime", MICRO, TYPE_BUG_FIX,
   "re-added sending of magic_bye string on MP socket close",
   NULL
 } ,

 { 3, SEP, 2008, RCR, "plug_realtime", MICRO, TYPE_MODIFY,
   "moved drive_wait execution to RT_tell_afni",
   NULL
 } ,

 { 2, SEP, 2008, RCR, "GIFTI", MICRO, TYPE_MODIFY,
   "have distribution Makefiles build with GIFTI/expat/zlib",
   NULL
 } ,

 { 29, AUG, 2008, RCR, "vol2surf", MINOR, TYPE_MODIFY,
   "fill in COLMS_STATSYM attribute when writing .niml.dset dataset",
   NULL
 } ,

 { 26, AUG, 2008, RCR, "3dAllineate", MINOR, TYPE_BUG_FIX,
   "initialized ntask in all cases",
   NULL
 } ,

 { 22, AUG, 2008, RCR, "Dimon", MICRO, TYPE_NEW_OPT,
   "added -drive_wait option",
   NULL
 } ,

 { 22, AUG, 2008, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
   "added DRIVE_WAIT command string",
   "The command will be executed after the first volume is processed,\n"
   "which is good for opening windows appropriate to a new dataset."
 } ,

 { 21, AUG, 2008, RCR, "afni", MINOR, TYPE_NEW_OPT,
   "added -disable_done option to safeguard real-time mode",
   NULL
 } ,

 { 21, AUG, 2008, RCR, "Dimon", MICRO, TYPE_MODIFY,
   "updated help and suggest -num_slices with -sleep_init",
   NULL
 } ,

 { 21, AUG, 2008, RCR, "afni-general", MINOR, TYPE_MODIFY,
   "in edt_dsetitems, if storage mode can be inferred from prefix, apply it",
   NULL
 } ,

 { 21, AUG, 2008, RCR, "ANOVA", MINOR, TYPE_MODIFY,
   "use DSET_BRIKNAME for dataset control, as that is updated for smode",
   NULL
 } ,

 { 18, AUG, 2008, RCR, "plug_realtime", MINOR, TYPE_MODIFY,
   "increase DRIVE_LIMIT to 4Kb, read env vars each run",
   "These variables can now be controlled through drive_afni 'SETENV'\n"
   "(either via 'plugout_drive' or 'Dimon -drive_afni'):\n"
   "    - AFNI_REALTIME_Mask_Vals  : specify what gets sent to serial_helper\n"
   "    - AFNI_REALTIME_SHOW_TIMES : specify whether to show data timestamps\n"
   "    - AFNI_REALTIME_SEND_VER   : specify whether to send comm version"
 } ,

 { 15, AUG, 2008, RCR, "afni", MINOR, TYPE_BUG_FIX,
   "init graph->grid_spacing, to prevent potential div by 0 via DRIVE",
   NULL
 } ,

 { 14, AUG, 2008, RCR, "Dimon", MICRO, TYPE_MODIFY,
   "moved num_slices check to separate function",
   NULL
 } ,

 { 14, AUG, 2008, RCR, "3dBrickStat", MINOR, TYPE_BUG_FIX,
   "do not automatically print -max along with -var",
   NULL
 } ,

 { 5, AUG, 2008, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "re-added the un16 fix from July 1",
   NULL
 } ,

 { 3, AUG, 2008, RCR, "nifti_tool", MINOR, TYPE_NEW_OPT,
   "added -help_ana, -disp_ana, -swap_as_analyze, -swap_as_nifti, -swap_as_old",
   NULL
 } ,

 { 3, AUG, 2008, RCR, "nifticlib", MINOR, TYPE_MODIFY,
   "added swap ability for ANALYZE 7.5 format, and made swapping complete",
   "- added nifti_analyze75 struct\n"
   "- modified swap_nifti_header to swap all fields (analyze or nifti)\n"
   "- added regression testing script c16.rand.swap\n"
   "These changes were motivated by C Burns."
 } ,

 { 31, JUL, 2008, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -num_slices option, and full real-time example E",
   NULL
 } ,

 { 31, JUL, 2008, RCR, "serial_helper", MINOR, TYPE_NEW_OPT,
   "added HELLO version 2 to work as -disp_all",
   "See 'HELLO versions' from 'serial_helper -help' for details.\n"
   "See 'example E' from 'Dimon -help' for a complete testing example."
 } ,

 { 31, JUL, 2008, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
   "enhancements to communication with serial helper",
   "- added 'Motion Only' to methods\n"
   "- parameter methods can easily be switched per run\n"
   "- SEND_VER replaces HELLO_VER as Y/N variable\n"
 } ,

 { 30, JUL, 2008, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
   "added HELLO version 1 and show_times option",
   "These are set via AFNI_REALTIME_SEND_VER and AFNI_REALTIME_SHOW_TIMES."
 } ,

 { 30, JUL, 2008, RCR, "serial_helper", MINOR, TYPE_NEW_OPT,
   "added HELLO version 1 and -show_times option",
   NULL
 } ,

 { 29, JUL, 2008, RCR, "plug_realtime", MINOR, TYPE_MODIFY,
   "print more socket error info, send MP vals w/out mask",
   NULL
 } ,

 { 29, JUL, 2008, RCR, "serial_helper", MINOR, TYPE_MODIFY,
   "captured and output more signal and error info, flushed output buffer",
   NULL
 } ,

 { 28, JUL, 2008, RCR, "plug_realtime", MINOR, TYPE_BUG_FIX,
   "alter check for bad socket: use tcp_alivecheck over tcp_writecheck",
   NULL
 } ,

 { 25, JUL, 2008, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "allow -sleep_vol to be very small without early run termination",
   NULL
 } ,

 { 23, JUL, 2008, RCR, "3dttest", MINOR, TYPE_NEW_OPT,
   "added -base1_dset option, where -base1 value can vary over voxels\n",
   "Added for M Beauchamp."
 } ,

 { 18, JUL, 2008, RCR, "3dNLfim", MINOR, TYPE_MODIFY,
   "listed signal and noise models in -help output",
   NULL
 } ,

 { 17, JUL, 2008, RCR, "3dNLfim", MINOR, TYPE_MODIFY,
   "warn the user if DSET_NVALS is not the same as DSET_NUMTIMES",
   "That would suggest the dataset has no time axis."
 } ,

 { 16, JUL, 2008, RCR, "serial_helper", MINOR, TYPE_NEW_OPT,
   "added -disp_all to give formatted display of 'all' mask data",
   "This was added for P Kundu.\n"
 } ,

 { 16, JUL, 2008, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
   "added choice of 'Vals to Send' to serial_helper",
   "Can now send index,i,j,k,x,y,z,value for every value in mask."
 } ,

 { 14, JUL, 2008, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added -sleep_init, -sleep_vol, -sleep_frac",
   "These options control the timeout periods between data checks."
 } ,

 { 14, JUL, 2008, RCR, "plug_realtime", MINOR, TYPE_MODIFY,
   "terminate TCP transmission to serial_helper if mask is bad\n",
   NULL
 } ,

 { 14, JUL, 2008, RCR, "afni_history", MINOR, TYPE_MODIFY,
   "a single integer option is interpreted as with -past_entries",
   NULL
 } ,

 { 11, JUL, 2008, RCR, "Dimon", MICRO, TYPE_MODIFY,
   "include last 4 elements of obl_matrix, even though probably useless",
   NULL
 } ,

 { 10, JUL, 2008, RCR, "plug_realtime", MAJOR, TYPE_MODIFY,
   "receive oblique transform matrix via new OBLIQUE_XFORM interface",
   NULL
 } ,

 { 10, JUL, 2008, RCR, "Dimon", MAJOR, TYPE_MODIFY,
   "if the data is oblique, pass the transformation matrix to plug_realtime",
   NULL
 } ,

 {  9, JUL, 2008, RCR, "plug_realtime", MICRO, TYPE_MODIFY,
   "if user closes graph window, allow comm with serial_helper to proceed",
   NULL
 } ,

 {  7, JUL, 2008, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "move extra newline from args_as_command to show_args_as_command",
   NULL
 } ,

 {  3, JUL, 2008, RCR, "plug_drawdset", MICRO, TYPE_BUG_FIX,
   "edt_dset_items.c: for .hdr, use .img brick file, storage_mode = BY_NIFTI",
   NULL
 } ,

 {  2, JUL, 2008, RCR, "Dimon", MICRO, TYPE_MODIFY,
   "provide suggestions in the case of a real-time TCP connection failure",
   NULL
 } ,

 {  1, JUL, 2008, RCR, "to3d", MINOR, TYPE_BUG_FIX,
   "fixed crash in case of mosaic and un16, no longer having im data",
   "Problem found by R. McColl."
 } ,

 {  1, JUL, 2008, RCR, "Makefile.INCLUDE", MINOR, TYPE_MODIFY,
   "modified the make system for building programs in the install directory",
   "- modified Makefile.INCLUDE's INFLAGS and ISFLAGS\n"
   "- modified SUMA_INPATH in SUMA_Makefile_NoDev.\n"
   "- removed 'rickr/' dirs from includes in mrilib.h, plug_crender.c and\n"
   "  3dAllineate.c\n\n"
   "Requested by V. Roopchansingh of MCW.\n"
 } ,

 { 30, JUN, 2008, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -gen_epi_review and -no_epi_review options",
   "By default, a drive_afni script to review EPI data is now generated."
 } ,

 { 30, JUN, 2008, RCR, "gen_epi_review.py", MINOR, TYPE_MODIFY,
   "make script executable, decrease sleep, add usage comment in script",
   NULL
 } ,

 { 27, JUN, 2008, RCR, "gen_epi_review.py", MAJOR, TYPE_NEW_PROG,
   "generate afni/drive_afni script to review initial EPI data",
   "This program was written to be called from the afni_proc.py output script."
 } ,

 { 27, JUN, 2008, RCR, "afni_util.py", MICRO, TYPE_MODIFY,
   "small modification to find_command_end",
   NULL
 } ,

 { 25, JUN, 2008, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added -past_entries option",
   NULL
 } ,

 { 25, JUN, 2008, RCR, "howto", MINOR, TYPE_MODIFY,
   "put disclaimers at the tops of HowTo #1, #2, and #5",
   "references to current AFNI class handouts were included"
 } ,

 { 24, JUN, 2008, RCR, "3dDeconvolve", MINOR, TYPE_MODIFY,
   "added the ability to output 1D iresp datasets",
   NULL
 } ,

 { 20, JUN, 2008, RCR, "libmri", MINOR, TYPE_MODIFY,
   "thd_niftiread: do not scale to float if scale=1 and inter=0",
   NULL
 } ,

 { 19, JUN, 2008, RCR, "file_tool", MICRO, TYPE_MODIFY,
   "removed printing of pointers in disp_ functions",
   NULL
 } ,

 { 19, JUN, 2008, RCR, "make_stim_times.py", MINOR, TYPE_MODIFY,
   "help update, added -show_valid_opts, use '*' as separator w/amplitudes",
   NULL
 } ,

 { 16, JUN, 2008, RCR, "file_tool", MINOR, TYPE_MODIFY,
   "show output for multiple bad files when using -show_bad_backslash",
   NULL
 } ,

 { 13, JUN, 2008, RCR, "3dclust", MINOR, TYPE_MODIFY,
   "in the help, Volume defaults to microliters, unless -dxyz=1 is used",
   NULL
 } ,

 { 13, JUN, 2008, RCR, "nifti_tool", MINOR, TYPE_NEW_OPT,
   "added -with_zlib, and ability to add extensions via 'file:FILENAME'",
   "extension update added for J. Gunter"
 } ,

 { 13, JUN, 2008, RCR, "nifticlib", MINOR, TYPE_NEW_OPT,
   "added nifti_compiled_with_zlib()",
   NULL
 } ,

 { 12, JUN, 2008, RCR, "neuro_deconvolve.py", MINOR, TYPE_NEW_PROG,
   "generate 3dTfitter script to deconvolve a BOLD signal into a neuro signal",
   NULL
 } ,

 { 12, JUN, 2008, RCR, "afni_util.py", MICRO, TYPE_NEW_OPT,
   "added get_dset_reps_tr, get_default_polort, get_dset_reps_tr, max_dim_1D",
   "also, updated find_last_space to deal with long strings"
 } ,

 { 12, JUN, 2008, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
   "shifted code to afni_util.get_dset_reps_tr and .get_default_polort",
   NULL
 } ,

 { 6, JUN, 2008, RCR, "plug_crender", MINOR, TYPE_BUG_FIX,
   "integral threshold was off by 1",
   NULL
 } ,

 { 6, JUN, 2008, RCR, "make_random_timing.py", MICRO, TYPE_MODIFY,
   "get_*_opt now returns an error code",
   NULL
 } ,

 { 2, JUN, 2008, RCR, "GIFTI", MICRO, TYPE_GENERAL,
   "added CMakeLists.txt and XMLCALL update from Simon Warfield",
   "also added LICENSE.gifti"
 } ,

 { 2, JUN, 2008, RCR, "model_demri_3", MICRO, TYPE_MODIFY,
   "small help update to clarify residual C curve input",
   NULL
 } ,

 { 29, MAY, 2008, RCR, "model_demri_3", MICRO, TYPE_BUG_FIX,
   "help update to clarify use of AFNI_MODEL_D3_R1I_DSET",
   NULL
 } ,

 { 22, MAY, 2008, RCR, "3dTshift", MINOR, TYPE_BUG_FIX,
   "with -rlt, slices without any time shift must still be processed",
   "problem noticed by Jie Huang"
 } ,

 { 21, MAY, 2008, RCR, "model_demri_3", MINOR, TYPE_BUG_FIX,
   "fixed incorrect scaling in Cp computation",
   "The error was introduced on April 8, 2008."
 } ,

 { 21, MAY, 2008, RCR, "make_stim_times.py", MICRO, TYPE_NEW_OPT,
   "added -amplitudes option (for Rutvik Desai)",
   NULL
 } ,

 { 18, MAY, 2008, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added options for TR-locking and storing '3dDeconvolve -nodata' examples",
   "- added shuffle() to replace that from random (cannot produce all perms)\n"
   "- added options -tr, -tr_locked and -save_3dd_cmd\n"
   "- changed -stim_time option to -stim_dur"
 } ,

 { 18, MAY, 2008, RCR, "afni_history", MICRO, TYPE_MODIFY,
   "sped up comparison (since histories have gotten long)",
   NULL
 } ,

 { 17, MAY, 2008, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "check result of 3dDeconvolve execution in output script",
   "If 3dDeconvolve fails, terminate the script so that the user can\n"
   "see what happened."
 } ,

 { 14, MAY, 2008, RCR, "model_demri_3", MINOR, TYPE_BUG_FIX,
   "fixed application of decay term",
   NULL
 } ,

 { 13, MAY, 2008, RCR, "GIFTI", MINOR, TYPE_NEW_OPT,
   "gifticlib-1.0.0: initial release",
   "includes support for (set/clear/read/write) external data files"
 } ,

 { 13, MAY, 2008, RCR, "gifti_tool", MINOR, TYPE_NEW_OPT,
   "added -set_extern_filelist option, and help for using external data files",
   NULL
 } ,

 { 9, MAY, 2008, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "gifticlib-0.0.18: giiCoordSystem is now an array of struct pointers",
   "modified GIFTI library, along with suma_gifti.c"
 } ,

 { 8, MAY, 2008, RCR, "model_demri_3", MINOR, TYPE_MODIFY,
   "updated help, NFIRST does not need to imply injection time",
   NULL
 } ,

 { 7, MAY, 2008, RCR, "option_list.py", MINOR, TYPE_NEW_OPT,
   "added get_type_list and other accessor functions",
   NULL
 } ,

 { 7, MAY, 2008, RCR, "plug_3Ddump_V2", MINOR, TYPE_BUG_FIX,
   "allow 4D datasets to be opened (so that buckets are again usable)",
   "PLUGIN_dset_check() now checks NVALS instead of NUM_TIMES..."
 } ,

 { 7, MAY, 2008, RCR, "make_random_timing.py", MAJOR, TYPE_NEW_PROG,
   "generate random stimulus timing files",
   "This generates random timing files suitable for use in 3dDeconvolve.\n"
   "The timing is not restricted to a TR grid, though that is possible.\n"
   "Consider use with '3dDeconvolve -nodata'."
 } ,

 { 1, MAY, 2008, RCR, "model_demri_3", MINOR, TYPE_BUG_FIX,
   "treat RESID_CT as Ct(t), not C(t)",
   NULL
 } ,

 { 30, APR, 2008, RCR, "make_stim_times.py", MICRO, TYPE_BUG_FIX,
   "replaced make_stim_files with make_stim_times.py in help",
   NULL
 } ,

 { 10, APR, 2008, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "updated the -help with information regarding runs of different lengths",
   NULL
 } ,

 {  8, APR, 2008, RCR, "2dImReg", MINOR, TYPE_BUG_FIX,
   "allow zero slices, passing input as result",
   "Choleski factorization would fail on an empty slice.  In this case,\n"
   "return the input slices as the result (instead of crashing).\n"
   "Done with D Glen."
 } ,

 {  8, APR, 2008, RCR, "3dNLfim", MICRO, TYPE_MODIFY,
   "only update output every 100 voxels",
   NULL
 } ,

 {  8, APR, 2008, RCR, "model_demri_3", MINOR, TYPE_NEW_ENV,
   "allow residual Ct values via AFNI_MODEL_D3_RESID_CT_DSET dataset",
   "e.g. setenv AFNI_MODEL_D3_RESID_CT_DSET residual_Ct+orig"
 } ,

 {  2, APR, 2008, RCR, "ANOVA", MINOR, TYPE_MODIFY,
   "extended maximum number of contrasts to 75",
   NULL
 } ,

 {  1, APR, 2008, RCR, "ANOVA", MINOR, TYPE_MODIFY,
   "increased internal memory for 3dbucket and 3drefit command creation",
   NULL
 } ,

 { 31, MAR, 2008, RCR, "ANOVA", MINOR, TYPE_MODIFY,
   "extended maximum number of means, diffs and contrasts to 50",
   NULL
 } ,

 { 28, MAR, 2008, RCR, "gifticlib", MICRO, TYPE_NEW_OPT,
   "added routines to copy MetaData",
   NULL
 } ,

 { 28, MAR, 2008, RCR, "gifti_tool", MINOR, TYPE_NEW_OPT,
   "added -copy_gifti_meta and -copy_DA_meta options",
   NULL
 } ,

 { 26, MAR, 2008, RCR, "gifticlib", MICRO, TYPE_MODIFY,
   "in compare, if comp_data is not set, state the fact",
   NULL
 } ,

 { 25, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "minor changes:",
   "  - NIFTI_INTENT_NONE is considered valid\n"
   "  - added compare_gifti_data functions\n"
   "  - LabelTables are now written using CDATA"
 } ,

 { 25, MAR, 2008, RCR, "gifti_tool", MINOR, TYPE_MODIFY,
   "the -compare_data option is not separate from -compare_gifti",
   NULL
 } ,

 { 24, MAR, 2008, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "added GERT_Reco options (request of D Glen)",
   "  -gert_filename    : specify a name for the GERT_Reco script\n"
   "  -gert_nz          : override nz=1 in mosaic image files\n"
   "  -gert_to3d_prefix : specify a dataset prefix for the to3d command"
 } ,

 { 24, MAR, 2008, RCR, "@Align_Centers", MINOR, TYPE_BUG_FIX,
   "applied proper follower dataset orientation and floating point shifts",
   "The shift applied to the child datasets was based on the parent's\n"
   "orientation.  The shifts were also being truncated to integers.\n"
   "Changed with D Glen.\n"
 } ,

 { 18, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_NEW_OPT,
   "added comparison functions to gifticlib",
   NULL
 } ,

 { 20, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_NEW_ENV,
   "AFNI_WRITE_1D_AS_PREFIX allows writing 1D or surface data given the prefix",
   "For example, setting this to YES will allow writing surface data to NIfTI."
 } ,

 { 18, MAR, 2008, RCR, "gifti_tool", MINOR, TYPE_NEW_OPT,
   "added -compare_gifti option",
   "See 'gifti_tool -help' for details, including example #7."
 } ,

 { 17, MAR, 2008, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "if 1 volume, GERT_Reco_dicom does not give (useless) timing to to3d",
   NULL
 } ,

 { 13, MAR, 2008, RCR, "3dmerge", MINOR, TYPE_GENERAL,
   "added some examples to the -help output",
   NULL
 } ,

 { 11, MAR, 2008, RCR, "model_demri_3", MINOR, TYPE_NEW_OPT,
   "added control of hematocrit via AFNI_MODEL_D3_HCT",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_NEW_ENV,
   "AFNI_GIFTI_VERB sets the verbose level in the gifti I/O library",
   "The default is 1, 0 is quiet, and values go up to 7."
 } ,

 { 10, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_GENERAL,
   "AFNI can read/write .gii.dset as with .gii",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "applied -gert_outdir in the case of dicom images",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "if only 1 run, GERT_Reco_dicom is named per run",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "SUMA_SurfMeasures", MAJOR, TYPE_BUG_FIX,
   "averages did not include nodes lost to -cmask",
   "Noticed by M Beauchamp."
 } ,

 {  7, MAR, 2008, RCR, "make_stim_times.py", MINOR, TYPE_BUG_FIX,
   "properly ignore empty lines, and exit on short files",
   NULL
 } ,

 {  6, MAR, 2008, RCR, "GIFTI", MICRO, TYPE_MODIFY,
   "allow functional control over GIFTI encoding" ,
   NULL
 } ,

 {  5, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_BUG_FIX,
   "fixed passing of INDEX_LIST" ,
   NULL
 } ,

 {  5, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "do not duplicate data when reading and writing GIFTI from AFNI",
   NULL
 } ,

 {  4, MAR, 2008, RCR, "3dTstat", MINOR, TYPE_NEW_OPT,
   "added -accumulate option, to output each partial sum" ,
   "for k = 0..N-1 : output[k] = sum(input[i]) over i = 0..k"
 } ,

 {  3, MAR, 2008, RCR, "website", MINOR, TYPE_MODIFY,
   "updated the AFNI History website pages, with a table of entries",
   NULL
 } ,

 { 29, FEB, 2008, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added a TYPE, -type, a new level, and a string to identify each level",
   NULL
 } ,

 { 28, FEB, 2008, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added -list_authors option and adjusted spacing",
   NULL
 } ,

 { 27, FEB, 2008, RCR, "afni_history", SUPER, TYPE_NEW_PROG,
   "program to display the history of AFNI updates" ,
   "This will be used to create a web page of AFNI updates.\n"
   "Please see 'afni_history -help' for more details."
 } ,

 { 27, FEB, 2008, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed -regress_use_stim_files typo (was -regress_use_stim_times)", 
   NULL
 } ,

 { 26, FEB, 2008, RCR, "afni_history", MICRO, TYPE_GENERAL,
   "checked in initial afni_history files", 
   NULL
 } ,

 { 25, FEB, 2008, RCR, "plug_vol2surf", MAJOR, TYPE_BUG_FIX,
   "fixed application of cluster for sending data to suma",

   "Previously, clustering was only applied when the Olay and Thr sub-bricks\n"
   "were the same."
 } ,

 { 24, FEB, 2008, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "GIFTI library now considers MetaData without Value as valid",

   NULL
 } ,

 /* pre-afni_history updates, mostly new programs and data formats */

 { 21, FEB, 2008, RCR, "GIFTI", SUPER, TYPE_GENERAL,
   "AFNI programs can now read and write GIFTI datasets",

   "GIFTI datasets are for data in the surface domain, with file suffix .gii.\n"
   "Support must be requested at compile time, and it requires libexpat.\n"
   "Please see http://www.nitrc.org/projects/gifti for many details."
 } ,

 {  6, FEB, 2008, RCR, "3dbucket", MINOR, TYPE_GENERAL,
   "modified to copy FDR curves",
   NULL
 } ,

 {  22, JAN, 2008, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options to estimate smoothness in data for use in AlphaSim",
   "See help options -regress_est_blur_epits and -regress_est_blur_errts."
 } ,

 {  28, DEC, 2007, RCR, "gifti_tool", MAJOR, TYPE_NEW_PROG,
   "program to read and write GIFTI datasets",
   NULL
 } ,

 {   3, DEC, 2007, RCR, "GIFTI", MAJOR, TYPE_GENERAL,
   "initial release of gifti I/O C API",
   NULL
 } ,

 {  31, AUG, 2007, RCR, "DECONFLICT", MAJOR, TYPE_MODIFY,
   "modified default behavior of programs from deconflict to no overwrite",
   "See AFNI_DECONFLICT in README.environment."
 } ,

 {  31, AUG, 2007, RCR, "model_conv_diffgamma", MINOR, TYPE_NEW_PROG,
   "NLfim model to compute the convolution of the difference of gammas",
   NULL
 } ,

 {  30, JUL, 2007, RCR, "regression_tests", MAJOR, TYPE_GENERAL,
   "added setup for regression testing to NIFTI package",
   "This can be used as a template for testing any command-line programs."
 } ,

 {  20, DEC, 2006, RCR, "afni_proc.py", SUPER, TYPE_NEW_PROG,
   "program to write complete single subject FMRI processing script", 
   NULL
 } ,

 {  11, DEC, 2006, RCR, "make_stim_times.py", MINOR, TYPE_NEW_PROG,
   "program to convert stim_files to stim_times files",
   NULL
 } ,

 {  22, OCT, 2006, RCR, "model_demri_3", MAJOR, TYPE_NEW_PROG,
   "NLfim model for Dynamic Enhanced MRI", 
   NULL
 } ,

 {  12, OCT, 2006, RCR, "serial_writer", MINOR, TYPE_NEW_PROG,
   "program to send data from a file, pipe or made up to a given serial port",
   NULL
 } ,

 {   8, AUG, 2006, RCR, "C++", MINOR, TYPE_MODIFY,
   "afni program compiles in C++ (effort with Rich and Greg Balls)",
   NULL
 } ,

 {   3, AUG, 2006, RCR, "NI_SURF_DSET", SUPER, TYPE_GENERAL,
   "added a new surface dataset format, with read/write ability in AFNI",
   NULL
 } ,

 {  25, JAN, 2006, RCR, "model_michaelis_menton", MAJOR, TYPE_NEW_PROG,
   "NLfim model function for ethanol studies",
   NULL
 } ,

 {   2, DEC, 2005, RCR, "ANOVA", SUPERDUPER, TYPE_MODIFY,
   "changed variance computations in 3dANOVA programs to not assume sphericity",
   "For details, see https://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html ."
 } ,

 {  11, OCT, 2005, RCR, "3dmaxima", MAJOR, TYPE_NEW_PROG,
   "command-line version of maxima plugin",
   NULL
 } ,

 {   5, JUL, 2005, RCR, "Dimon", SUPER, TYPE_NEW_PROG,
   "program to monitor real-time acquisition of DICOM images",
   NULL
 } ,

 {  25, APR, 2005, RCR, "NIFTI", SUPER, TYPE_GENERAL,
   "AFNI can read and write NIFTI datasets (effort with Bob and Rich)",
   NULL
 } ,

 {  7, JAN, 2005, RCR, "nifti_tool", SUPER, TYPE_NEW_PROG,
   "program to directly manipulate or compare NIFTI dataset headers",
   NULL
 } ,

 {  7, JAN, 2005, RCR, "NIFTI", SUPER, TYPE_GENERAL,
   "initial release of NIFTI library",
   NULL
 } ,

 {  4, OCT, 2004, RCR, "vol2surf", SUPER, TYPE_GENERAL,
   "added vol2surf interface for real-time mapping from afni to suma",
   NULL
 } ,

 { 31, MAR, 2004, RCR, "serial_helper", MAJOR, TYPE_NEW_PROG,
   "program to pass realtime registration params from TCP to serial port",
   NULL
 } ,

 {  1, DEC, 2003, RCR, "SurfMeasures", MAJOR, TYPE_NEW_PROG,
   "program to compute various measures over surfaces",
   NULL
 } ,

 {  5, AUG, 2003, RCR, "3dVol2Surf", SUPER, TYPE_NEW_PROG,
   "program to map data from the volume to domain to the surface domain",
   NULL
 } ,

 { 29, MAY, 2003, RCR, "3dSurf2Vol", SUPER, TYPE_NEW_PROG,
   "program to map data from the surface domain to the volume domain",
   NULL
 } ,

 { 15, JAN, 2003, RCR, "Imon", MAJOR, TYPE_NEW_OPT,
   "added connection to the realtime plugin in afni",
   NULL
 } ,

 { 27, NOV, 2002, RCR, "Imon", SUPER, TYPE_NEW_PROG,
   "program to monitor GE I-files as they are written to the scanner",
   NULL
 } ,

 {  1, OCT, 2002, RCR, "file_tool", SUPER, TYPE_NEW_PROG,
   "program to perform generic manipulations of binary files",
   NULL
 } ,

 { 20, JUN, 2002, RCR, "@make_stim_file", MINOR, TYPE_NEW_PROG,
   "script to create binary stim files",
   NULL
 } ,

 {  6, JUN, 2002, RCR, "@SUMA_Make_Spec_FS", MAJOR, TYPE_NEW_PROG,
   "script to import FreeSurfer surfaces into SUMA",
   NULL
 } ,

 { 21, MAY, 2002, RCR, "3dresample", SUPER, TYPE_NEW_PROG,
   "program to change a dataset orientation and/or grid spacing",
   NULL
 } ,

 {  8, MAR, 2002, RCR, "plug_crender", SUPER, TYPE_NEW_PROG,
   "added rendering plugin to afni",
   NULL
 } ,

 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
