
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

 { 20, MAY, 2010, RCR, "Makefile.NIH.CentOS.5.3_64", MICRO, TYPE_MODIFY,
   "update CCOLD to compile with gcc verion 3.4",
   NULL
 } ,

 { 19, MAY, 2010, RCR, "Makefile.*", MINOR, TYPE_MODIFY,
   "add CCOLD to all for compiling nifticdf.o on linux_xorg7_64 using gcc v3",
   "The gcc compiler versions 4.1-4.3 (at least) had an optimization bug\n"
   "when compiling nifticdf.o.  The result was an inaccurate conversion\n"
   "from F-stats to p-values (in some cases).\n"
   "Test with the command:     cdf -t2p fift 1.0 10 100\n"
   "   good result: 0.448817, bad result: 0.0472392\n"
   "Problem found by L Thomas and B Bones."
 } ,

 { 13, MAY, 2010, RCR, "3dbucket", MICRO, TYPE_MODIFY,
   "tiny help update to clarify -glueto and -aglueto",
   NULL
 } ,

 { 13, MAY, 2010, RCR, "ui_xmat.py", MICRO, TYPE_MODIFY,
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

 { 10, MAY, 2010, RCR, "ktaub.c", MICRO, TYPE_MODIFY,
   "allow for build on SOLARIS_OLD",
   NULL
 } ,

 { 6, MAY, 2010, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "allow negatives in -sort_by_num_suffix, look for '0054 1330' in sorting",
   NULL
 } ,

 { 1, MAY, 2010, RCR, "make_random_timing.py", MINOR, TYPE_NEW_OPT,
   "added -max_consec for Liat of Cornell",
   NULL
 } ,

 { 29, APR, 2010, RCR, "@DriveAfni", MICRO, TYPE_MODIFY,
   "minor updates",
   NULL
 } ,

 { 28, APR, 2010, RCR, "NIFTI", MICRO, TYPE_GENERAL,
   "added NIFTI_ECODE_CARET for J. Harwell",
   NULL
 } ,

 { 26, APR, 2010, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_opts_reml",
   NULL
 } ,

 { 26, APR, 2010, RCR, "3dDeconvolve", MINOR, TYPE_GENERAL,
   "add $* to end of 3dREMLfit script command, for additional arguments",
   "Finally getting around to afni_proc.py option -regress_opts_reml..."
 } ,

 { 28, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
   "applied fitts computation to REML case",
   NULL
 } ,

 { 25, MAR, 2010, RCR, "1d_tool.py", MICRO, TYPE_GENERAL,
   "small help update",
   NULL
 } ,

 { 25, MAR, 2010, RCR, "afni_proc.py", MICRO, TYPE_GENERAL,
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

 { 16, MAR, 2010, RCR, "3dAttribute", MICRO, TYPE_MODIFY,
   "set_dataset_attributes() on read - so can use on non-AFNI datasets",
   NULL
 } ,

 { 16, MAR, 2010, RCR, "3dbucket", MINOR, TYPE_BUG_FIX,
   "fixed getting incorrect FDR curves (noted by D Glen)",
   NULL
 } ,

 { 16, MAR, 2010, RCR, "NIFTI", MICRO, TYPE_GENERAL,
   "added NIFTI_ECODE_VOXBO for D. Kimberg",
   NULL
 } ,

 { 11, MAR, 2010, RCR, "3dsvm_common.c", MICRO, TYPE_GENERAL,
   "some compilers choke on mid-block variable definitions",
   NULL
 } ,

 {  9, MAR, 2010, RCR, "3dNotes", MICRO, TYPE_GENERAL,
   "send -help output to stderr (req by T Nycum)",
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

 { 24, DEC, 2009, RCR, "GIFTI", MINOR, TYPE_NEW_OPT,
   "added approximate difference functions",
   "- added gifti_approx_gifti_images, DA_pair, labeltables, diff_offset\n"
   "- added gifti_triangle_diff_offset\n"
   "- gifti_compare_coordsys takes comp_data param"
 } ,

 {  8, DEC, 2009, RCR, "GIFTI", MINOR, TYPE_NEW_OPT,
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

 { 16, NOV, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "allow motion censoring with varying run lengths",
   "Also, if a max is applied in scaling, explicitly limit to [0,max].\n"
 } ,

 { 16, NOV, 2009, RCR, "1d_tool.py", MINOR, TYPE_MODIFY,
   "allow motion censoring with varying run lengths",
   NULL
 } ,

 {  4, NOV, 2009, RCR, "Dimon", MICRO, TYPE_MODIFY,
   "small change to check on sort problems",
   NULL
 } ,

 { 27, OCT, 2009, RCR, "GIFTI", MINOR, TYPE_NEW_OPT,
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

 { 14, OCT, 2009, RCR, "3dTcat", MICRO, TYPE_MODIFY,
   "allow creation of single volume dataset",
   "as requested by N Vack (among many others)"
 } ,

 { 6, OCT, 2009, RCR, "1d_tool.py", MICRO, TYPE_NEW_OPT,
   "added -set_run_lengths option, for varying run lengths",
   "Added for motion censoring and run padding."
 } ,

 { 2, OCT, 2009, RCR, "1d_tool.py", MICRO, TYPE_MODIFY,
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

 { 16, SEP, 2009, RCR, "plug_vol2surf", MICRO, TYPE_NEW_OPT,
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

 {  4, SEP, 2009, RCR, "plug_realtime", MINOR, TYPE_NEW_OPT,
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

 { 25, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_MODIFY,
   "if volreg block, always create motion_${subj}_enorm.1D",
   NULL
 } ,

 { 25, AUG, 2009, RCR, "1d_tool.py", MICRO, TYPE_MODIFY,
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

 { 10, AUG, 2009, RCR, "3dSurf2Vol", MINOR, TYPE_MODIFY,
   "allow processing of -overwrite and AFNI_DECONFLICT",
   NULL
 } ,

 {  6, AUG, 2009, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed problems found by I Mukai and K Bahadur",
   "- fixed -volreg_align_to base as applied in align_epi_anat.py\n"
   "- fixed blur 'averages' computation when only one run"
 } ,

 {  4, AUG, 2009, RCR, "realtime_receiver.py", MINOR, TYPE_NEW_OPT,
   "added basic demo interface and itemized exception traps",
   NULL
 } ,

 { 31, JUL, 2009, RCR, "prompt_user", MICRO, TYPE_GENERAL,
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

 { 27, JUL, 2009, RCR, "plug_realtime", MICRO, TYPE_NEW_OPT,
   "added Y/N AFNI_REALTIME_reset_output_index, to start each dset at 001",
   "Also, changed prefix separator to double underscore '__'.",
 } ,

 { 27, JUL, 2009, RCR, "afni-general", MICRO, TYPE_GENERAL,
   "added SOLARIS_OLD atanhf #define to machdep.h",
   NULL
 } ,

 { 23, JUL, 2009, RCR, "afni_run_R", MINOR, TYPE_MODIFY,
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

 { 16, JUL, 2009, RCR, "realtime_receiver.py", MAJOR, TYPE_NEW_PROG,
   "replacement for serial_helper",
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

 { 15, JUN, 2009, RCR, "3dmerge", MINOR, TYPE_GENERAL,
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

 { 8, JUN, 2009, RCR, "afni_proc.py", MICRO, TYPE_MODIFY,
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

 { 14, APR, 2009, RCR, "NIfTI", MINOR, TYPE_NEW_OPT,
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
   "- added 'MASKING NOTE', to suggest no regresion mask until group space"
 } ,

 { 12, MAR, 2009, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added -regress_reml_exec and -regress_3dD_stop",
   "One can execute 3dREMLfit and/or 3dDeconvolve.  Error blur is from each."
 } ,

 { 10, MAR, 2009, RCR, "NIfTI", MICRO, TYPE_MODIFY,
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
   "For details, see http://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html ."
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
