
/** cf. afni_history.h **/

#include "afni_history.h"

/*  (for starting a new file, search for CHANGE)

    basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long descrption with intermediate newlines
  
    copy entire section: { ... } ,
  
    Notes: - months are JAN ... DEC (see afni_history.h)

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
           - it may be nice to put the newest entires at the top
           - leave the last "99, NULL" entry as it is

 -- example --

 { 26 , FEB , 2008 , ZSS , "my_program" , MAJOR ,
   "short description of change" ,
   "(optional) description of where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

/* Ponderosos:
   -----------
   - Every bone of my body shivers with excitement at the
   thought of adding a type variable :
   { NA, NEW_PROG, NEW_OPT, NEW_ENV, BUG_FIX }
   It would allow users to check for new things easily.
   And it might help eradicate misery from this world.
   Did I also mention that it can fit on the first line
   beautfully, right after the short string? WOW!
   - We should probably start building a program list. To make sure 
   that we have no typos there. We can have users add to that list 
   whenever they introduce a program for the first time. This way 
   nobody has to do it at once. 
   - Also, we need to have special 'program names' like: 
   "afni-general", "suma-general", "general"
   or perhaps
   "AFNI", "SUMA", "ALL"
   these would be used to refer to package-wide changes.
*/ 

afni_history_struct ptaylor_history[] = {
/*=====BELOW THIS LINE=====*/

{ 6, Nov , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_BUG_FIX,
   "Fixed delta-slice definition for 4D mode of imaging (occasional probs).",
   "Should have correct gapord values across all views now.\n"
},

{ 5, Nov , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt: '-olay_alpha' and '-olay_boxed' for new alpha/boxed driving.",
   "Keepin' up with changes to afni driving, via RWC work.\n"
},

{ 5, Nov , 2018 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_GENERAL,
   "Adjust to keep up with new afni alpha/boxed behavior.",
   "Update internal calls to @chauffeur_afni, which needed new opts for this.\n"
},

{ 5, Nov , 2018 , PT , "3dAllineate" , MICRO , TYPE_GENERAL,
   "Help file update: move *the* useful cost funcs lpa and lpc into main part.",
   "These are no longer listed as experimental!\n"
},

{ 1, Nov , 2018 , PT , "1dplotpy" , MAJOR , TYPE_NEW_PROG,
   "New plotting program for 1D files.",
   "Copies much of the fun 1dplot capability to some pythonic realm.\n"
},

{ 21, Oct , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt: '-pbar_saveim PBS' and '-pbar_dim PBD', to output color pbar.",
   "Just add in new AFNI driving functionality from RWC, to save colorbar.\n"
},

{ 21, Oct , 2018 , PT , "apqc_make_html.py" , MINOR , TYPE_NEW_PROG,
   "Helper program for afni_proc.py.",
   "Run @ss_review_html, build QC dir with html file for ss review.\n"
},

{ 21, Oct , 2018 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_NEW_PROG,
   "Helper program for afni_proc.py.",
   "Make @ss_review_html script for HTML version of AP QC.\n"
},

{ 16, Oct , 2018 , PT , "@FindAfniDsetPath" , MINOR , TYPE_BUG_FIX,
   "Maybe not really a bug, but this program wasn't work as it should have.",
   "It now should find NIFTI sets better, and use afnirc env vars.\n"
},

{ 15, Oct , 2018 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_NEW_PROG,
   "Helper program for @chauffeur_afni-- wrapper of it for QC stuff.",
   "It's for alignment checking, and it's... edgy.\n"
},

{ 15, Oct , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt: '-box_focus_slices REF', to avoid looking at empty slices.",
   "Can used a masked dset as REF to focus on certain slices only.\n"
},

{ 15, Oct , 2018 , PT , "@djunct_slice_space" , MINOR , TYPE_NEW_PROG,
   "Helper program for @chauffeur_afni.",
   "Calculate even spacing of slices for montaging.\n"
},

{ 15, Oct , 2018 , PT , "3dAutobox" , MINOR , TYPE_NEW_OPT,
   "More new options",
   "Also output midslices, more info to screen (on-demand), and xyz stuff.\n"
},

{ 15, Oct , 2018 , PT , "@xyz_to_ijk" , MICRO , TYPE_GENERAL,
   "Fixed help file to list all opts.",
   "Now '-prefix ...' appears with apsearch.\n"
},

{ 15, Oct , 2018 , PT , "3dAutobox" , MICRO , TYPE_GENERAL,
   "Allow for subbrick selection of input",
   "Tiny internal change-- moving where dset is loaded+checked.\n"
},

{ 15, Oct , 2018 , PT , "3dAutobox" , MINOR , TYPE_NEW_OPT,
   "New opt: '-extent_ijk_to_file FF'.",
   "Output IJK extents to a simple-formatted text file.\n"
},

{ 28, Aug , 2018 , PT , "@xyz_to_ijk" , MINOR , TYPE_NEW_PROG,
   "Helper program to be able to convert xyz coors to ijk indices.",
   "Supplementary program.\n"
},

{ 15, Mar , 2018 , PT , "fat_proc_convert_dwis" , MINOR , TYPE_NEW_OPT,
   "Can provide NIFTI+bvec+bval files as inp, not just a directory of dicoms.",
   "All niceifying steps can thus be applied to already-converted vol.\n"
},

{ 10, Aug , 2018 , PT , "3dClusterize" , MINOR , TYPE_BUG_FIX,
   "Allow non-stat bricks to be thresholded.",
   "Before, if the [ithr] brick was nonstat, crashing occurred.\n"
},

{ 1, Aug , 2018 , PT , "@chauffeur_afni" , MICRO , TYPE_BUG_FIX,
   "Deal correctly with percentile values for 4D ulay in non-4Dmode...",
   "... because user may specify with subbrick selectors.\n"
},

{ 31, July , 2018 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_NEW_OPT,
   "New opt: '-check_abs_min ..'.",
   "Just allows the same-named opt from 1dDW_Grad_o_Mat++ to be used.\n"
},

{ 31, July , 2018 , PT , "@GradFlipTest" , MINOR , TYPE_NEW_OPT,
   "New opt: '-check_abs_min ..'.",
   "Just allows the same-named opt from 1dDW_Grad_o_Mat++ to be used.\n"
},

{ 25, July , 2018 , PT , "@chauffeur_afni" , MAJOR , TYPE_GENERAL,
   "Several new options, as well as ability to deal with 4D images.",
   "Many new features, probably including minor bug fixes.\n"
},

{ 25, July , 2018 , PT , "@djunct_calc_mont_dims.py" , MINOR , TYPE_BUG_FIX,
   "Was excluding solution of a square set of dimensions.",
   "Tested; seems fine now.\n"
},

{ 23, July , 2018 , PT , "3dClusterize" , MICRO , TYPE_GENERAL,
   "Check about overwriting files before trying to write.",
   "This way, failure to write file exits nonzeroly.\n"
},

{ 17, July , 2018 , PT , "@djunct_select_str.py" , MICRO , TYPE_GENERAL,
   "Converted to python3 compatible, using 2to3.",
   "Tested; seems fine.\n"
},

{ 17, July , 2018 , PT , "@djunct_combine_str.py" , MICRO , TYPE_GENERAL,
   "Converted to python3 compatible, using 2to3.",
   "Tested; seems fine.\n"
},

{ 17, July , 2018 , PT , "@djunct_calc_mont_dims.py" , MICRO , TYPE_GENERAL,
   "Converted to python3 compatible, using 2to3.",
   "Tested; seems fine.\n"
},

{ 1, July , 2018 , PT , "@SSwarper" , MAJOR , TYPE_NEW_OPT,
   "New opt:  well, actually, it is new to *have* explicit options now!",
   "Same great functionality, but with more flexible options/names/outputs.\n"
},

{ 1, July , 2018 , PT , "@snapshot_volreg" , MINOR , TYPE_GENERAL,
   "Now respects including a path in the third argument (prefix/filename).",
   "Useful for scripting and selecting directory for output images.\n"
},

{ 26, June , 2018 , PT , "fat_proc_axialize_anat" , MINOR , TYPE_NEW_OPT,
   "New opt '-focus_by_ss' to do skullstripping before alignment stuff.",
   "Final dset is *not* skullstripped, but it helps with center of mass.\n"
},

{ 26, June , 2018 , PT , "fat_proc_select_vols" , MINOR , TYPE_BUG_FIX,
   "Bug fixed in supplementary program to *this* program.",
   "Used to get an error when no bad vols were selected.\n"
},

{ 26, June , 2018 , PT , "@djunct_select_str.py" , MINOR , TYPE_BUG_FIX,
   "Would return an error when *no* bad vols were selected.",
   "Note about fixing it in Jan, 2018; must have forgot to push that ver!\n"
},

{ 26, June , 2018 , PT , "fat_proc_convert_anat" , MINOR , TYPE_NEW_OPT,
   "Can provide a NIFTI file as input, not just a directory of dicoms.",
   "All niceifying steps can thus be applied to already-converted vol.\n"
},

{ 25, June , 2018 , PT , "fat_proc_select_vols" , MINOR , TYPE_GENERAL,
   "The adjunct program, @djunct_dwi_selector.bash, was changed to be tcsh.",
   "No output diffs; but bash one couldn't run on new Mac OS (bad Mac)...\n"
},

{ 1, June , 2018 , PT , "3dAmpToRSFC" , MINOR , TYPE_GENERAL,
   "Adapted to changes of 3dLombScargle.",
   "Simpler scaling to match Parseval.\n"
},

{ 1, June , 2018 , PT , "3dLombScargle" , MINOR , TYPE_GENERAL,
   "Change scaling of output.",
   "Simpler scaling to match Parseval.\n"
},

{ 1, June , 2018 , PT , "fat_proc_axialize_anat" , MINOR , TYPE_NEW_OPT,
   "New pre-alignment opt, -pre_align_center_mass.",
   "Probably more useful than older -pre_center_mass.\n"
},

{ 1, June , 2018 , PT , "3dClusterize" , MICRO , TYPE_NEW_OPT,
   "New opt to output vols even if no clusters are found.",
   "These would be empty vols-- juuuust if the user wants.\n"
},

{ 30, May , 2018 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_NEW_OPT,
   "User can specify matching cost and warp.",
   "How exciting is that??  (Well, mostly for test comparisons...).\n"
},

{ 30, May , 2018 , PT , "@suma_reprefixize_spec" , MICRO , TYPE_BUG_FIX,
   "Changing 'more' -> 'cat', internally.",
   "Think 'more' gave oddness at times- dumped weird chars and broke files.\n"
},

{ 29, May , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Can crop the saved images.",
   "See the '-crop_*' options.\n"
},

{ 27, May , 2018 , PT , "3dClusterize" , MINOR , TYPE_GENERAL,
   "Make report cleaner, and add in INT_MAP property to output clust map.",
   "Thanks, D. Glen for more useful suggestions.\n"
},

{ 23, May , 2018 , PT , "3dClusterize" , MINOR , TYPE_GENERAL,
   "Some bug fixes if dsets are left out, some new checks on what user asks.",
   "User can't run multi-sided tests on single-sided stats now...\n"
},

{ 22, May , 2018 , PT , "fat_proc_filter_dwis" , MAJOR , TYPE_BUG_FIX,
   "Was unioning, not intersecting, multiple selector strings.",
   "Fixed the issue in subprogram @djunct_combin_str.py.\n"
},

{ 21, May , 2018 , PT , "p2dsetstat" , MINOR , TYPE_NEW_OPT,
   "Include '-bisided' as a type of test, explicitly.",
   "Same behavior as '-2sided', just easier for scripting.\n"
},

{ 17, May , 2018 , PT , "3dClusterize" , MINOR , TYPE_GENERAL,
   "String subbrick selectors now work for -idat and -ithr.",
   "Also, the text report contains more (useful?) information.\n"
},

{ 13, May , 2018 , PT , "3dClusterize" , MINOR , TYPE_BUG_FIX,
   "Wouldn't work with extra dset entered- fixed now.",
   "Can enter extra beta/effect estimate set for clusterizing.\n"
},

{ 12, May , 2018 , PT , "3dClusterize" , MAJOR , TYPE_NEW_PROG,
   "Perform clusterizing (voxelwise and volume-wise thresholding) on a dset.",
   "Basically like 3dclust but has some new options and easier syntax.\n"
},

{ 22, Apr , 2018 , PT , "fat_proc_axialize_anat" , MINOR , TYPE_BUG_FIX,
   "When using '-remove_inf_sli', the wrong volume was being warped at end.",
   "Final warped volume had lower slice reduction, when it shouldn't have.\n"
},

{ 22, Apr , 2018 , PT , "3dSliceNDice" , MAJOR , TYPE_NEW_PROG,
   "Calculate Dice coefficients between volumes on a slicewise basis.",
   "Useful for comparing masks/maps of volumes.\n"
},

{ 16, Apr , 2018 , PT , "p2dsetstat" , MAJOR , TYPE_NEW_PROG,
   "Program to convert a p-value to a statistic, using params in dset header.",
   "Useful to calculate thresholds for a particular volume.\n"
},

{ 2, Apr , 2018 , PT , "@radial_correlate" , MICRO , TYPE_GENERAL,
   "Make -hview option work with the program.",
   "Didn't before, does now.\n"
},

{ 14, Mar , 2018 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_BUG_FIX,
   "Crashed no ref dset was used in mapping.",
   "Crashes no more under such circumstance.\n"
},

{ 14, Mar , 2018 , PT , "fat_proc_filter_dwis" , MINOR , TYPE_BUG_FIX,
   "Crashed when b-value file was input.",
   "Crashes no more under such circumstance.\n"
},

{ 6, Mar , 2018 , PT , "fat_proc_convert_anat" , MINOR , TYPE_MODIFY,
   "Default orientation for nifti files to be 'RAI' instead of 'RPI'.",
   "This will be more in line with TORTOISE (and AFNI DICOM-coor default).\n"
},

{ 6, Mar , 2018 , PT , "fat_proc_convert_dwis" , MINOR , TYPE_MODIFY,
   "Default orientation for nifti files to be 'RAI' instead of 'RPI'.",
   "This will be more in line with TORTOISE (and AFNI DICOM-coor default).\n"
},

{ 22, Feb , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Can now apply p-to-stat calcs for thresholding.",
   "User gives p-value, which gets made to appropriate stat for thresh.\n"
},

{ 13, Feb , 2018 , PT , "fat_proc_filter_dwis" , MINOR , TYPE_MODIFY,
   "Can now accept *multiple* selector strings that get merged.",
   "Multiple strings/files can be input, yay.\n"
},

{ 8, Feb , 2018 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_BUG_FIX,
   "When a mask was input, it wasn't resampled if needed to be.",
   "Now fixed, and added check that grid of mask is good. Good.\n"
},

{ 6, Feb , 2018 , PT , "fat_proc_axialize_anat" , MINOR , TYPE_NEW_OPT,
   "Can put a ceiling on the final output volume: -do_ceil_out.",
   "Reduce impact of tiny spikes (often at fringe) later on.\n"
},

{ 1, Feb , 2018 , PT , "@GradFlipTest" , MINOR , TYPE_MODIFY,
   "Internal change to allow subset selection in text files.",
   "Can now use subbrick selector notation with bvals/bvecs.\n"
},

{ 12, Jan , 2018 , PT , "@djunct_select_str.py" , MINOR , TYPE_BUG_FIX,
   "Would return an error when *no* bad vols were selected.",
   "Now updated to work fine with that; just an intermed program.\n"
},

{ 12, Jan , 2018 , PT , "3dRSFC" , MICRO , TYPE_GENERAL,
   "Deal with change elsewhere to definition of a function.",
   "New option added to function, just need another arg; shd be no change.\n"
},

{ 12, Jan , 2018 , PT , "fat_proc_align_anat_pair" , MICRO , TYPE_BUG_FIX,
   "Output 3dAllineate's weight vol to working dir, not present dir.",
   "Minor change, does not affect alignment/output.\n"
},

{ 22, Dec , 2017 , PT , "fat_proc_connec_vis" , MINOR , TYPE_NEW_OPT,
   "Can output the intermediate tstat or tcat files of ROI maps.",
   "Might be useful in subsequent volumetric analyses.\n"
},

{ 20, Dec , 2017 , PT , "fat_proc_connec_vis" , MINOR , TYPE_MODIFY,
   "Changing the way that outputting is specified.",
   "Make making a separate directory the default output; new opt for files.\n"
},

{ 29, Sep , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_MODIFY,
   "Now has help with list of options.",
   "Should be useful for users during processing.\n"
},

{ 29, Nov , 2017 , PT , "@djunct_4d_imager" , MAJOR , TYPE_NEW_PROG,
   "Helper function to make montages and movies of 4D dsets.",
   "Useful when proc'ing dsets, make record of them.\n"
},

{ 26, Oct , 2017 , PT , "fat_proc_connec_vis" , MAJOR , TYPE_NEW_PROG,
   "Visualize 'white matter connection' volumes output by 3dTrackID.",
   "Combine separate '-dump_rois ...' output into SUMAble surface maps.\n"
},

{ 12, Oct , 2017 , PT , "@GradFlipTest" , MINOR , TYPE_MODIFY,
   "Change output formatting and getting basename of prefix name.",
   "Easier output and reading in terminal/files.\n"
},

{ 04, Oct , 2017 , PT , "@GradFlipTest" , MINOR , TYPE_MODIFY,
   "Change the output directory naming/choosing options.",
   "Do more with just '-prefix ...', in standard AFNI fashion.\n"
},

{ 22, Sep , 2017 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_BUG_FIX,
   "On Macs, when not all types of 'follower' sets were used, this gave err.",
   "Have changed internal behavior to avoid this 'Mac'errorizing.\n"
},

{ 20, Sep , 2017 , PT , "1dDW_Grad_o_Mat++" , MINOR , TYPE_NEW_OPT,
   "New opt to push through tiny, negative diagonal elements in bmatrices.",
   "Useful-- but use this option cautiously, and look at your data...\n"
},

{ 20, Sep , 2017 , PT , "@GradFlipTest" , MICRO , TYPE_MODIFY,
   "Change way text is dumped to screen.",
   "Should prevent any need for user keypress if terminal is small.\n"
},

{ 19, Sep , 2017 , PT , "3dLombScargle" , MINOR , TYPE_BUG_FIX,
   "delF calculated correctly now.",
   "Had been at N-1 instead of N.  Better Parsevalling now.\n"
},

{ 14, Sep , 2017 , PT , "3dLombScargle" , MAJOR , TYPE_BUG_FIX,
   "Finally revisiting this-- fixed up lots of things.",
   "Good to go for basic functionality now.\n"
},

{ 11, Sep , 2017 , PT , "plugout_drive" , MICRO , TYPE_GENERAL,
   "Change level: actually nano.  Fixed Example 1 (missing apostrophe).",
   "It's the little things in life, though, sometimes.\n"
},

{ 06, Sep , 2017 , PT , "fat_proc_dwi_to_dt" , MICRO , TYPE_MODIFY,
   "Quick change: keep FOV same for b0 ulay comparison with anat-edge.",
   "Minor adjustment for keeping FOV consistent.\n"
},

{ 06, Sep , 2017 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_MODIFY,
   "Output a couple more types of QC images by default.",
   "Output b0 ulay with anat-edge olay;  also, some uncert images.\n"
},

{ 06, Sep , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_MODIFY,
   "Now gets output path as part of '-prefix' as opposed to sep '-outdir'.",
   "Now in line with most of AFNI funcs.\n"
},

{ 24, Aug , 2017 , PT , "@GradFlipTest" , MINOR , TYPE_MODIFY,
   "The file storing the flip recommendation will *overwrite* a previous one.",
   "Previous version of this would *append to*, which seems pointless.\n"
},

{ 17, Aug , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Some new labelling, etc. optioning.",
   "Make some new labels, locationing based on XYZ and more.\n"
},

{ 16,  Aug , 2017, PT, "afni", MICRO, TYPE_NEW_OPT,
   "Added color map (applies to both afni and suma): Reds_and_Blues_Inv",
   "So, new color opt readily available.\n"
} ,

{ 11, Aug , 2017 , PT , "fat_proc_align_anat_pair" , MINOR , TYPE_MODIFY,
   "Change a '>>' to '>' for wider compatability.",
   "Yup.\n"
},

{ 11, Aug , 2017 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_MODIFY,
   "Make range associated with ROI map images =256 for all.",
   "This provides better consistency in coloration with ROI_i256 cbar.\n"
},

{ 8, Aug , 2017 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_NEW_OPT,
   "Can have surfaces, niml.dsets and spec files move along with vols.",
   "Added capability to mapping volume dsets.\n"
},

{ 8, Aug , 2017 , PT , "@suma_reprefixize_spec" , MINOR , TYPE_NEW_PROG,
   "Helper function to copy a spec file whilst renaming files inside.",
   "Useful when copying a lot of *.gii or other surface files.\n"
},

{ 8, Aug , 2017 , PT , "3dTrackID" , MICRO , TYPE_BUG_FIX,
   "More specific glob for 3D vol files *only*; had gotten 1D text in list.",
   "Getting 1D text files would throw error.  More specific search now.\n"
},

{ 1, Aug , 2017 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_MODIFY,
   "Turn on reweighting and cumulative weight calc in 3dDWItoDT part.",
   "More useful fitting+output, hopefully.\n"
},

{ 1, Aug , 2017 , PT , "3dDWItoDT" , MINOR , TYPE_MODIFY,
   "Have the '-cumulative_wts' output also get dumped into a 1D file.",
   "Figured it was nice to not *only* have info in the terminal.\n"
},

{ 31, Jul , 2017 , PT , "@GradFlipTest" , MICRO , TYPE_MODIFY,
   "Echo the recommendations into a text file, as well.",
   "More useful/less lossy if scripting. New '-wdir *' opt, too.\n"
},

{ 3, Jul , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Some new subbrick-setting optioning.",
   "For utilizing 'SET_SUBBRICKS i j k' functionality in driving afni.\n"
},

{ 7, Jun , 2017 , PT , "@SUMA_renumber_FS" , MINOR , TYPE_MODIFY,
   "Added two more FS 'undetermined' regions to the list, ROIs 29 and 61.",
   "One voxel of one was in one subject once. Joy. Now part of tiss__unkn.\n"
},

{ 6, Jun , 2017 , PT , "3dROIMaker" , MINOR , TYPE_NEW_OPT,
   "New inflation opt:  '-skel_stop_strict'.",
   "Think this might be useful: don't expand at all *into* WM skel.\n"
},

{ 6, Jun , 2017 , PT , "@GradFlipTest" , MICRO , TYPE_MODIFY,
   "Internal call to 3dAutomask for DWI file now talks abs value of DWI[0].",
   "Useful bc TORTOISE now outputs negative DWIs... .\n"
},

{ 6, Jun , 2017 , PT , "@GradFlipTest" , MICRO , TYPE_GENERAL,
   "Change output summary dumped to screen: don't prepend './' on paths.",
   "Should have changed earlier with internal name changes... Easier now.\n"
},

{ 5, Jun , 2017 , PT , "3dTrackID" , MICRO , TYPE_GENERAL,
   "Allow longer path names input for some things.",
   "Paths to dti_in inputs can now be longer (300 chars).\n"
},

{ 30, May , 2017 , PT , "3dANOVA3" , MICRO , TYPE_GENERAL,
   "Removed warning/info message for using type 4 or 5.",
   "Apparently made loooong ago, no longer needed according to GC.\n"
},

{ 26, May , 2017 , PT , "3dReHo" , MINOR , TYPE_BUG_FIX,
   "Correct checking for null time series now.",
   "Earlier, only looked at [0]th point; now sums across all.\n"
},

{ 26, May , 2017 , PT , "3dNetCorr" , MINOR , TYPE_BUG_FIX,
   "Correct checking for null time series now.",
   "Earlier, only looked at [0]th point; now sums across all.\n"
},

{ 20, May , 2017 , PT , "@chauffeur_afni" , MICRO , TYPE_MODIFY,
   "Temporary files now have more unique names.",
   "Helps avoid confusion in parallel computations.\n"
},

{ 12, May , 2017 , PT , "3dDWItoDT" , MAJOR , TYPE_NEW_OPT,
   "Added goodness-of-fit measures to '-debug_brik' output.",
   "Two chi-sqs from Papadakis et al. (2003); thx, J Ipser for idea.\n"
},

{ 11, May , 2017 , PT , "3dDTtoDWI" , MAJOR , TYPE_BUG_FIX,
   "Fixed mismatch in multiplying DT and bmatrices.",
   "Element indices hadn't been sync'ed, now they are.\n"
},

{ 4, May , 2017 , PT , "3dReHo" , MINOR , TYPE_NEW_OPT,
   "Allow box-y neighborhoods.",
   "User can input values for cubic/prism neighborhoods now.\n"
},

{ 4, May , 2017 , PT , "3dDTtoDWI" , MINOR , TYPE_NEW_OPT,
   "Added in '-scale_out_1000' option, to match 3dDWItoDT.",
   "This allows it to be used with scaled tensors from 3dDWItoDT.\n"
},

{ 3, May , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_MODIFY,
   "The opacity in olays wasn't working with xvfb-run-- now it does.",
   "Pixel depth was not useful by default, I think.\n"
},

{ 2, May , 2017 , PT , "@GradFlipTest" , MICRO , TYPE_MODIFY,
   "If 'outdir' doesn't exist yet, create it (don't just exit with error).",
   "Simplifies some other fat_proc scripting.\n"
},

{ 27, Apr , 2017 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "With '-output_mask_nonnull', user can output mask of non-null ts.",
   "This was made to help those who need to finnd null time series here.\n"
},

{ 27, Apr , 2017 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "With '-ts_wb_strlabel', can use ROI string labels in WB output filenames.",
   "This was made expressly for The Rajendra Who Shall Not Be Named.\n"
},

{ 27, Apr , 2017 , PT , "3dNetCorr" , MINOR , TYPE_MODIFY,
   "More watchfulness for null time series from badly masked dsets.",
   "Count and report null time series, and possibly fail if too many.\n"
},

{ 30, Mar , 2017 , PT , "lib_fat_funcs.py" , MICRO , TYPE_BUG_FIX,
   "An error message in func called by fat_mvm_scripter.py was wrong.",
   "Fixed an indexing mistake which gave wrong ROI list-- thanks, E. Grodin!\n"
},

{ 29, Mar , 2017 , PT , "@chauffeur_afni" , MICRO , TYPE_MODIFY,
   "Change how xvfb is used to run in virtual environment.",
   "This should improve usage on biowulf-- thanks much, D. Godlove!\n"
},

{ 20, Mar , 2017 , PT , "@SUMA_renumber_FS" , MICRO , TYPE_MODIFY,
   "Changed an ls -> find, to search for either *.nii or *.nii.gz better.",
   "Necessary in case of problematic users (you know who you are!).\n"
},

{ 9, Feb , 2017 , PT , "@GradFlipTest" , MICRO , TYPE_BUG_FIX,
   "Some IF conditions gave problems; some option names were inconvenient.",
   "They are now ex-parrots.\n"
},

{ 6, Feb , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_MODIFY,
   "Should deal with subbrick selection now.",
   "Works for ulay and olay sets in usual AFNI way.\n"
},

{ 31, Jan , 2017 , PT , "@SUMA_renumber_FS" , MINOR , TYPE_MODIFY,
   "Update region list to work with new FS 6.0 that came out a week ago.",
   "Regions #3 and #42 (in FS file output) appear now; ~'leftover' GM.\n"
},

{ 27, Jan , 2017 , PT , "3dDWItoDT" , MICRO , TYPE_NEW_OPT,
   "Miniscule new option, '-bmatrix_FULL' to have clearer usage.",
   "Just copies functionality of cryptic '-bmatrix_Z'.\n"
},

{ 27, Jan , 2017 , PT , "@GradFlipTest" , MAJOR , TYPE_MODIFY,
   "Totally revamped-- have real options, better funcs, output text file.",
   "Meshes with other changes in 1dDW_Grad* and 3dDWItoDT.\n"
},

{ 26, Jan , 2017 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_PROG,
   "Simplish function for driving AFNI to make images/montages.",
   "Based on @snapshot_volreg; mainly intended for my selfish use.\n"
},

{ 26, Jan , 2017 , PT , "1dDW_Grad_o_Mat++" , MINOR , TYPE_NEW_PROG,
   "New program for changing/reformatting grads and things.",
   "Better defaults and simpler than original 1dDW_Grad_o_Mat++.\n"
},

{ 30, Dec , 2016 , PT , "@SUMA_renumber_FS" , MINOR , TYPE_NEW_PROG,
   "New program for renumbering FS output after @SUMA_Make_Spec_FS.",
   "Also conglomerates things into tissue maps.\n"
},

{ 30, Dec , 2016 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_MODIFY,
   "Output new data sets of renumb'd values, more consistent than 'rank' ones.",
   "Also output more tissue segmentation maps based on ROIs.\n"
},

{ 26, Dec , 2016 , PT , "thd_center" , MINOR , TYPE_NEW_OPT,
   "Extra argument in THD_cmass() and THD_roi_cmass().",
   "Allows for local ijk coordinate output; updated other calling functions.\n"
},

{ 23, Dec , 2016 , PT , "3dCM" , MINOR , TYPE_NEW_OPT,
   "Allow ijk coordinate output.",
   "Will be in local orientation.  Makes undumping after easier.\n"
},

{ 20, Dec , 2016 , PT , "fat_mvm_prep.py" , MICRO , TYPE_NEW_OPT,
   "New --unionize_rois option: affects GRID element selection.",
   "Now can select union of matrix elements across group for MVM_tbl.\n"
},

{ 23, Nov , 2016 , PT , "3dTrackID" , MINOR , TYPE_BUG_FIX,
   "Used to be able to have nans in sBL b/c of sqrt(neg-from-rounding).",
   "Now IF condition to prevent that.  Happy Thanksgiving.\n"
},

{ 23, Nov , 2016 , PT , "3dNetCorr" , MINOR , TYPE_BUG_FIX,
   "Z-score WB maps were all zeros-> now have values.",
   "Hopefully even the correct Z-values.\n"
},

{ 16, Nov , 2015 , PT , "3dTrackID" , MAJOR , TYPE_GENERAL,
   "Estimate mean and stdev of fiber lengths in bundles.",
   "These are now automatically output in *.grid file.\n"
},

{ 16, Nov , 2015 , PT , "3dTrackID" , MAJOR , TYPE_NEW_OPT,
   "Can limit tracts to 'between targets' in new ways.",
   "See '-targ_surf_stop' and '-targ_surf_twixt' in the help.\n"
},

{ 16, Nov , 2016 , PT , "1dDW_Grad_o_Mat" , MINOR , TYPE_GENERAL,
   "Output b-values are now floats, not ints.",
   "Seems necessary, depending on what user has input.\n"
},

{ 16, Nov , 2016 , PT , "1dDW_Grad_o_Mat" , MINOR , TYPE_BUG_FIX,
   "The -out_bval_col_sep used did nothing (after last changes).",
   "Have returned it to functionality.\n"
},

{ 16, Nov , 2016 , PT , "3dDWUncert" , MINOR , TYPE_GENERAL,
   "Check for singular values, so don't get crashes from GSL calcs.",
   "These pretty much occur outside mask, but can also be inside mask.\n"
},

{ 12, Oct , 2016 , PT , "3dDWItoDT" , MINOR , TYPE_GENERAL,
   "Now, automatically output RD if '-eigs' opt is used.",
   "And the users of 3dTrackID say, 'Yaaaay'. Or 'Wha'evah!'.\n"
},

{ 12, Oct , 2016 , PT , "3dDWUncert" , MINOR , TYPE_GENERAL,
   "Now output progress; also, only divvy up non-zeros to proc.",
   "Should be faster/better parallelized, also tell user about itself.\n"
},

{ 11, Oct , 2016 , PT , "map_TrackID" , MICRO , TYPE_GENERAL,
   "Put integer variables in to not get lame warnings when building.",
   "Things like 'pppp = fscan();', etc... Purely aesthetic changes.\n"
},

{ 11, Oct , 2016 , PT , "3dDWUncert" , MAJOR , TYPE_GENERAL,
   "Totally reprogrammed, mainly to use OpenMP and be fstr.",
   "Also should be more generalized if b0 != 0.\n"
},

{ 14, Sep , 2016 , PT , "3dDWItoDT" , MINOR , TYPE_NEW_OPT,
   "Have a new '-bmax_ref ...' option: if bref has b>0.",
   "Won't have much effective change *yet*, but will later. Possibly.\n"
},

{ 13, Sep , 2016 , PT , "1dDW_Grad_o_Mat" , MINOR , TYPE_NEW_OPT,
   "New opt -bref_mean_top to average over mean bref when b>0.",
   "Also, totally reprogrammed most of interior; had been too scraggly.\n"
},

{ 31, Aug , 2016 , PT , "3dSpaceTimeCorr" , MAJOR , TYPE_BUG_FIX,
   "Fixed bug in yet-unreleased function... and also changed a feature.",
   "Bug: ts = all0 -> GSL badness on some comp; now, exclude seedvox in corr.\n"
},

{ 31, Aug , 2016 , PT , "3dSpaceTimeCorr" , MAJOR , TYPE_NEW_PROG,
   "New function for calculating spatial corr of temporal corr maps.",
   "Calc spatial corr of WB/mask connectivity maps; useful for RSFC?\n"
},

{ 18, Aug , 2016 , PT , "3dReHo" , MINOR , TYPE_BUG_FIX,
   "Used to not allow subbrik selection on input.",
   "Now it does.  Thanks to Daniel H. for pointing it out.\n"
},

{ 1, Aug , 2016 , PT , "3dRSFC" , MINOR , TYPE_BUG_FIX,
   "In cases of *very large* N_pts, an error message appeared-- no more.",
   "Just changed default initialization of >f_N value.\n"
},

{ 21, Jun , 2016 , PT , "@fat_tract_colorize" , MAJOR , TYPE_NEW_PROG,
   "New function for coloring the volumetric tracking outputs.",
   "RGB coloration of local diffusion, esp. for PROB track output.\n"
},

{ 20, Jun , 2016 , PT , "3dLombScargle" , MINOR , TYPE_BUG_FIX,
   "Fixing bug in delta F calculation.",
   "What more needs to be said?\n"
},

{ 14, Jun , 2016 , PT , "3dAmptoRSFC" , MAJOR , TYPE_NEW_PROG,
   "New function for calculating RSFC params from one-side spectra.",
   "Complements 3dLombScargle. What an epithet.\n"
},

{ 14, Jun , 2016 , PT , "3dLombScargle" , MINOR , TYPE_MODIFY,
   "Making this output 'one-sided' spectra now.",
   "Easier for 3dAmpToRSFC calcs.\n"
},

{ 16, Jun , 2016 , PT , "3dLombScargle" , MINOR , TYPE_MODIFY,
   "Changed how number of output points/freqs is calc'ed.",
   "Should be more stable across group.).\n"
},

{ 13, Jun , 2016 , PT , "3dLombScargle" , MAJOR , TYPE_MODIFY,
   "Revamped LS program-- AGAIN**2!-- now has Welch windows+tapers.",
   "Scaling properly/consistently, couple bug fixes.\n"
},

{ 9, Jun , 2016 , PT , "3dLombScargle" , MAJOR , TYPE_MODIFY,
   "Revamped LS program-- AGAIN-- now has Welch windows+tapers.",
   "Several new options added (related to windows/tapers).\n"
},

{ 27, May , 2016 , PT , "3dDWItoDT" , MINOR , TYPE_NEW_OPT,
   "Have a new '-scale_out_1000' option: rescale output, if desired.",
   "Effectively: change output diff units of mm^2/s -> x10^{-3} mm^2/s.\n"
},

{ 24, May , 2016 , PT , "3dLombScargle" , MAJOR , TYPE_MODIFY,
   "Revamped LS program-- new implementation, directly from PR89.",
   "Several new options added (normalize, amplitudeize, etc.).\n"
},

{ 12, May , 2016 , PT , "3dLombScargle" , MAJOR , TYPE_NEW_PROG,
   "New function for calculating LS (normalized) periodogram from time series.",
   "Calculate magnitude spectrum from non-equisampled data.\n"
},

{ 3, May , 2016 , PT , "@GradFlipTest" , MINOR , TYPE_MODIFY,
   "Using '-out_grad_cols_bwt' for grad stuff now-- use weights.",
   "Can deal well with multiple DW factors in gradient list now.\n"
},

{ 2, May , 2016 , PT , "3dDWItoDT" , MINOR , TYPE_NEW_OPT,
   "Have a new '-min_bad_md' option: use to threshold badness.",
   "Also now detect bad DT fits if MD is crazy big.  Whoa.\n"
},

{ 8, Apr , 2016 , PT , "3dDTtoDWI" , MINOR , TYPE_MODIFY,
   "Work to deal with bvalue-weighted grads.",
   "This is useful esp. for new TORTOISE outputs.\n"
},

{ 5, Apr , 2016 , PT , "3dDWUncert" , MINOR , TYPE_NEW_OPT,
   "New inp format option-- for dealing with TORT export/import.",
   "-bmatrix_Z for reading in bmat in AFNI format; byebye -bmatr opt.\n"
},

{ 5, Apr , 2016 , PT , "1dDW_Grad_o_Mat" , MINOR , TYPE_NEW_OPT,
   "New I/O options-- for dealing with TORT export.",
   "Now have I/O of grad columns weighted by bvalues.\n"
},

{ 5, Jan , 2016 , PT , "3dVecRGB_to_HSL" , MAJOR , TYPE_NEW_PROG,
   "Take a 3-vec to a single index on RGB color scale, and glue FA brick.",
   "Replaces earlier version, 3dVec_to_RGBind.\n"
},

{ 4, Jan , 2016 , PT , "1dDW_Grad_o_Mat" , MICRO , TYPE_BUG_FIX,
   "Fixed backwards output messages.",
   "Should now be easier to see what went bad in a case of mistaken input.\n"
},

{ 16, Dec , 2015 , PT , "3ddot_beta" , MAJOR , TYPE_NEW_PROG,
   "Copy calc of 3ddot-- uses same functions-- just faster.",
   "Right now, can only calculate eta2; was asked for by user.\n"
},

{ 16, Nov , 2015 , PT , "fat_mat_sel.py" , MINOR , TYPE_MODIFY,
   "New default for x-axis labels: rot=45 deg, horiz align=right.",
   "Better than previous defaults (rot=37 deg, horiz align=center).\n"
},

{ 10, Nov , 2015 , PT , "3dVec_to_RGBind.c" , MAJOR , TYPE_NEW_PROG,
   "Take a 3-vec to a single index on RGB color scale, and glue FA brick.",
   "This will be useful in prob tract result plotting... script to come.\n"
},

{ 28, Sep , 2015 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_BUG_FIX,
   "Use list of ROIs to select subnetwork of analysis for 3dMVM.",
   "Previously, sublist only applied to post hocs, not 3dMVM models.\n"
},
   
{ 18, Sep , 2015 , PT , "@GradFlipTest" , MICRO , TYPE_MODIFY,
   "For DWI analysis: just linear fitting of tensor.",
   "Faster 3dDWItoDT usage, only do linear fit.\n"
},

{ 16, Sep , 2015 , PT , "@GradFlipTest" , MAJOR , TYPE_NEW_PROG,
   "For DWI analysis: test whether grads need to be flipped.",
   "Use a few tracking calls to estimate 'best' grad orientations.\n"
},

{ 10, Aug , 2015 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_NEW_OPT,
   "Minor new option: input list of ROIs with file.",
   "For minor convenience.\n"
},

{ 9, Aug , 2015 , PT , "3dROIMaker" , MINOR , TYPE_BUG_FIX,
   "Fixed minor bug when GM map has no ROIs/clusters.",
   "No more crashing... Won't produce GM or GMI volumes; message only.\n"
},

{ 5, Aug , 2015 , PT , "fat_mvm_prep.py" , MICRO , TYPE_BUG_FIX,
   "Micro ~bug fixed for inputting CSV headings.",
   "Now strip off lead/trail whitespace, then replace rest with underscore.\n"
},

{ 22, Jul , 2015 , PT , "3dROIMaker" , MINOR , TYPE_BUG_FIX,
   "Fixed minor bug when refset has negative values.",
   "No more crashing...\n"
},

{ 7, Jul , 2015 , PT , "fat_mat_sel.py" , MINOR , TYPE_NEW_OPT,
   "Simple new option to exclude x-axis labels.",
   "They might just be annoying.\n"
},

{ 21, May , 2015 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_BUG_FIX,
   "Minor bug fixed for inputting sublist of ROIs.",
   "Short option for doing so worked, but not the long one; fixed now.\n"
},

{ 21, May , 2015 , PT , "3dDWUncert" , MICRO , TYPE_NEW_OPT,
   "Can choose to analyze only high-FA voxels: don't waste time on GM/CSF.",
   "Option to ignore low-FA vox for uncert, leave them 0.\n"
},

{ 15, May , 2015 , PT , "1dDW_Grad_o_Mat" , MINOR , TYPE_NEW_OPT,
   "Can output separate bval file.",
   "Useful in some TORT preprocessing.\n"
},

{ 27, Apr , 2015 , PT , "3dROIMaker" , MINOR , TYPE_BUG_FIX,
   "Fixed output when byte/short insets were used.",
   "Had been not writing data; needed to null brick_facs in outsets.\n"
},

{ 9, Feb , 2015 , PT , "3dTrackID" , MINOR , TYPE_NEW_OPT,
   "Can threshold bundles with too few tracks; TRK files not default out.",
   "Useful to control false pos;  useful to save space outputting.\n"
},

{ 27, Jan , 2015 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_MODIFY,
   "Include main effects of interaction vars in post hoc tests.",
   "Hadn't been testing these previously.\n"
},

{ 26, Jan , 2015 , PT , "3dTrackID" , MINOR , TYPE_NEW_OPT,
   "Can dump output *maps*, not just masks, of each connection.",
   "See '-dump_rois AFNI_MAP' for how it works.\n"
},

{ 26, Jan , 2015 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_BUG_FIX,
   "Hadn't included part quantitative interaction term in qVars list.",
   "Program wouldn't run if interaction term had quant var.\n"
},

{ 26, Jan , 2015 , PT , "fat_mvm_prep.py" , MICRO , TYPE_GENERAL,
   "Ignore empty lines or whitespace lines in CSV file.",
   "Causes less hassle at times now.\n"
},

{ 23, Jan , 2015 , PT , "3dTrackID" , MINOR , TYPE_BUG_FIX,
   "Rare scenario of -nifti -dump_rois AFNI not working.",
   "Needed to add a mkdir() internally.  Itsafinenow.\n"
},

{ 22, Jan , 2015 , PT , "3dROIMaker" , MINOR , TYPE_BUG_FIX,
   "Fixed some issues when only a tiny number of voxels is in inset.",
   "Labelling wasn't correct when nvox < n_refset_roi.\n"
},

{ 7, Jan , 2015 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "Switch to output nifti files.",
   "For corr map or Z map files.\n"
},

{ 7, Jan , 2015 , PT , "3dROIMaker" , MINOR , TYPE_NEW_OPT,
   "Switch to output nifti files.",
   "For GM and GMI files.\n"
},

{ 7, Jan , 2015 , PT , "3dTrackID" , MINOR , TYPE_NEW_OPT,
   "Switch to output nifti files.",
   "For PAIRMAP, INDIMAP and -dump_rois output.\n"
},

{ 21, Dec , 2014 , PT , "3dTrackID" , MINOR , TYPE_GENERAL,
   "Change of string output in .niml.dset.",
   "Make the label match the ROI string labels.\n"
},

{ 21, Dec , 2014 , PT , "3dNetCorr" , MINOR , TYPE_GENERAL,
   "Output NIML dset automatically.",
   "This allows users to view connectivity matrix info in SUMA easily.\n"
},

{ 21, Dec , 2014 , PT , "fat_mat_sel.py" , SUPER , TYPE_NEW_PROG,
   "Plot, view and save matrix file info.",
   "Works for both 3dNetCorr and 3dTrackID info.\n"
},

{ 15, Dec , 2014 , PT , "3dROIMaker" , MAJOR , TYPE_NEW_OPT,
   "Make a subset of an ROI by choosing maximal neighoring values.",
   "Start with peak value, add neighboring max until N voxels selected.\n"
},

{ 5, Nov , 2014 , PT , "3dROIMaker" , MAJOR , TYPE_NEW_ENV,
   "Default neighborhoods now AFNI standard; labeltable functionality in.",
   "Default 'hoods more standard, can still do other; labels by default.\n"
},

{ 5, Nov , 2014 , PT , "3dTrackID" , MINOR , TYPE_NEW_OPT,
   "Switch to not output INDI and PAIR map files.",
   "In connectome examples, this might save a lot of space.\n"
},

{ 24, Oct , 2014 , PT , "3dTrackID" , MINOR , TYPE_BUG_FIX,
   "Fixed offset in track to volume coordinates ",
   "Effect of bug restricted to viewing of tracts rather than volume masks "
   "and connectivity matrices.\n"
   "Offset was by half a voxel in each of the three dims.\n"
},

{ 26, Sep , 2014 , PT , "3dNetCorr" , MAJOR , TYPE_NEW_OPT,
   "Allow labeltable reading and writing.",
   "This allows users to use labeltables, and output labelled values everywhere.\n"
},

{ 26, Sep , 2014 , PT , "3dTrackID" , MAJOR , TYPE_NEW_OPT,
   "Allow labeltable reading and writing.",
   "This allows users to use labeltables, and output labelled values everywhere.\n"
},

{ 18, Sep , 2014 , PT , "fat_mvm_prep.py" , MICRO , TYPE_GENERAL,
   "Change internal var/par names, and how helpfile is thrown.",
   "More consistent naming, easier helpfile usage.\n"
},

{ 18, Sep , 2014 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_NEW_OPT,
   "Allow interaction terms in the user-defined statistical model.",
   "Allow cat+quant or cat+cat variable interactions, and posthoc testing.\n"
},

{ 8, Sep , 2014 , PT , "fat_roi_row.py" , SUPER , TYPE_NEW_PROG,
   "Select out one row of a matrix file, at user request.",
   "Useful, for example, if wanting to view connectivity one-to-many.\n"
},

{ 8, Sep , 2014 , PT , "fat_mvm_prep.py" , SUPER , TYPE_NEW_PROG,
   "Connect FATCAT with 3dMVM-- combine CSV and matrix data.",
   "Build data table necessary for 3dMVM from MRI+other data.\n"
},

{ 8, Sep , 2014 , PT , "fat_mvm_scripter.py" , SUPER , TYPE_NEW_PROG,
   "Connect FATCAT with 3dMVM-- write a basic command call to 3dMVM.",
   "User specificies specific model, and awaaaay we go.\n"
},

{ 8, Sep , 2014 , PT , "fat_mvm_gridconv.py" , SUPER , TYPE_NEW_PROG,
   "Connect FATCAT with 3dMVM-- modernize format of old *.grid files.",
   "Prehistoric grid files had no labels. This updates them.\n"
},

{ 8, Sep , 2014 , PT , "3dROIMaker" , MINOR , TYPE_NEW_OPT,
   "Allow pre-inflation of an input ROI, at user request.",
   "Useful, for example, if wanting to go from WM->GM.\n"
},

{ 5, Aug , 2014 , PT , "3dTrackID" , MINOR , TYPE_MODIFY,
   "Less memory usage and a bit faster.",
   "More efficient internal handling of quantities.\n"
},

{ 4, Aug , 2014 , PT , "3dDWUncert" , MICRO , TYPE_NEW_OPT,
   "Internal options for testing uncertainty things.",
   "For internal testing only at this point.\n"
},

{ 4, Aug , 2014 , PT , "1dDW_Grad_o_Mat" , MAJOR , TYPE_NEW_OPT,
   "Can edit dataset with averaging B0s and DWIs.",
   "Should make life easier for dual processing of vecs and datasets.\n"
},

{ 4, Aug , 2014 , PT , "3dTrackID" , MINOR , TYPE_NEW_OPT,
   "New option for PAIRMAP labelling by X, not 2^X; new *.grid NT scaling.",
   "Make PAIRMAP easier to view; user wanted extra matrices.\n"
},

{ 19, Jun , 2014 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "Added new feature: output partial correlation matrices.",
   "Can output r-like and beta-like partial correlation matrices.\n"
},

{ 6, Jun , 2014 , PT , "3dTrackID" , MINOR , TYPE_GENERAL,
   "Changed how it runs, mainly under the hood; added '-thru_mask' option.",
   "Cleared some old arrays; make runable as function; user wanted thru_masks.\n"
},

{ 6, Jun , 2014 , PT , "3dEigsToDT" , MICRO , TYPE_BUG_FIX,
   "Make help file option match with actual usage.",
   "Fixed a minor mismatch of helpfile name and actual option name.\n"
},

{ 6, Jun , 2014 , PT , "3dEigsToDT" , MICRO , TYPE_MODIFY,
   "Helpfile micro correction.",
   "Need parentheses around a couple entries.\n"
},


{ 29, Apr , 2014 , PT , "3dROIMaker" , MINOR , TYPE_NEW_OPT,
   "Freedom in neighbor defs; also can keep just N peak values per ROI.",
   "User can specify face, edge or vertex ngbs. Also, search for N max vals.\n"
},

{ 29, Apr , 2014 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "Added new feature: output WB correlations as Zscores.",
   "Can output WB maps of ROI average time series correlations as Z-scores.\n"
},

{ 21, Apr , 2014 , PT , "1dDW_Grad_o_Mat" , MICRO , TYPE_NEW_OPT,
   "Output grads as rows-- new option switch.",
   "Done at user request.\n"
},

{ 21, Apr , 2014 , PT , "3dEigsToDT" , SUPER , TYPE_NEW_PROG,
   "New program: take in eigen{values,vectors} and calculate DT.",
   "This also allows flipping/rescaling to be done.\n"
},

{ 21, Apr , 2014 , PT , "TORTOISEtoHere" , SUPER , TYPE_NEW_PROG,
   "New program: convert TORTOISE DTs to AFNI format.",
   "This also allows flipping/rescaling to be done.\n"
},

{ 21, Apr , 2014 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "Added new feature: do whole brain correlations.",
   "Can output individual WB maps of ROI average time series correlations.\n"
},

{ 16, Apr , 2014 , PT , "3dNetCorr" , MINOR , TYPE_MODIFY,
   "Reformatted output a bit, added features for J. Rajendra.",
   "Can output time series with labels, and as individual files.\n"
},

{ 16, Apr , 2014 , PT , "3dROIMaker" , MICRO , TYPE_BUG_FIX,
   "Hadn't made a problem if user didn't input 'prefix'.",
   "Fixed aforementioned loophole..\n"
},

{ 16, Apr , 2014 , PT , "3dMatch" , MINOR , TYPE_BUG_FIX,
   "Bug when using mask on *some* files with Linux.",
   "Seems to be more stable criteria now.\n"
},

{ 6, Mar , 2014 , PT , "3dTrackID" , SUPERDUPER , TYPE_MODIFY,
   "Have Cordelialy unified the three kingdoms of tracking, cLearing usage.",
   "This program does all tracking, including HARDI and mini-probabilistic.\n"
},

{ 6, Mar , 2014 , PT , "3dProbTrackID" , SUPERDUPER , TYPE_MODIFY,
   "Put out to pasture.",
   "This program is now retired, with usage cleanly transferred to 3dTrackID.\n"
},

{ 6, Mar , 2014 , PT , "3dNetCorr" , MINOR , TYPE_MODIFY,
   "Reformatted output a bit.",
   "Make output easier to read, labelled, and matching *GRID files.\n"
},

{ 6, Mar , 2014 , PT , "3dROIMaker" , MICRO , TYPE_BUG_FIX,
   "Make parameter appear in help file correctly.",
   "Fixed silly Spoonerism in option names usage/help-representation.\n"
},

{ 6, Mar , 2014 , PT , "1dDW_Grad_o_Mat" , MAJOR , TYPE_NEW_PROG,
   "Manipulate gradient/bmatrix files.",
   "Convert row/col and grad/bmatrix/gmatrix, use bval info, filter or not.\n"
},

{ 6, Mar , 2014 , PT , "3dTrackID" , MINOR , TYPE_NEW_OPT,
   "Changes for reading in DTI files.",
   "Allow NIML-formatted input file, as well as globbing in ordinary case.\n"
},

{ 6, Mar , 2014 , PT , "3dDWUncert" , MINOR , TYPE_NEW_OPT,
   "Changes for reading in DTI files.",
   "Allow NIML-formatted input file, as well as globbing in ordinary case.\n"
},

{ 6, Mar , 2014 , PT , "3dDWUncert" , MINOR , TYPE_BUG_FIX,
   "Silly bug-- order of options not free.",
   "Changed how options were processed so they weren't order-dependent.\n"
},

{ 3, Mar , 2014 , PT , "3dROIMaker" , MICRO , TYPE_MODIFY,
  "Fixing option name agreement with help file.",
  "Modernizing language."
},

{ 28, Oct , 2013 , PT , "3dROIMaker" , MINOR , TYPE_MODIFY,
  "Allow multiple-brick masks.",
  "For N-brick data set, can input either 1- or N-brick mask."
},

{ 28, Oct , 2013 , PT , "3dMatch" , MINOR , TYPE_BUG_FIX,
  "Fixed subbrick labelling oddity.",
  "For some reason, subbrick selection with [i] was getting confused"
  "with i-th label (which was an integer). Solved by prefixing label"
  "designation with a short string of useful letters."
},

{ 24, Oct , 2013 , PT , "3dROIMaker" , MINOR , TYPE_BUG_FIX,
  "Fix segmentation error when not using -refset.",
  "Fixed error in defining/freeing a certain array."
},

{ 26, Sep , 2013 , PT , "3dProbTrackID" , MINOR , TYPE_MODIFY,
  "Improving ease of NOT-mask ROI inclusion and (internal) track handling.",
  "Instead of separate ROI NOT-masks, uses can build in ANTI-ROIs with"
  "negative-valued (=-1) voxels."
  "Under the hood track handling: smoother checking of track ends, as well as"
  "of possibly breaking up tracks in event of NOT regions; simpler passing"
  "to track bundles, as well."
},

{ 26, Sep , 2013 , PT , "3dROIMaker" , MINOR , TYPE_MODIFY,
  "Allow negative ROIs in refset.",
  "This is useful/necessary for handling new NOT-mask regionality in network"
  "files for tracking."
},

{ 26, Sep , 2013 , PT , "DoTrackit.c" , MINOR , TYPE_MODIFY,
  "Improving ease of NOT-mask ROI inclusion and (internal) track handling.",
  "This is useful/necessary for handling new NOT-mask regionality in network"
  "files for tracking; think it just streamlines various processing, as well."
},

{ 26, Sep , 2013 , PT , "TrackIO.c" , MINOR , TYPE_MODIFY,
  "Improving ease of track handling.",
  "Updated TrackCreate function, which has simpler inputs from 3dProbTrackID"
  "now; outputs unchanged."
},

{ 26, Aug , 2013 , PT , "DoTrackit.c" , MINOR , TYPE_BUG_FIX,
  "Fix handling of non-RPI datasets.",
  "No ostensible output change, except to not produce an error message.\n"
},

{ 21, Aug , 2013 , PT , "3dProbTrackID" , MAJOR , TYPE_MODIFY,
   "Putting together old deterministic and probabilistic programs into 1.",
   "Unifying tracking, will be easier to update/improve in future."
   "For deterministic tracking, allow networks of target ROIs for tracking,"
   "as well as bunding outputs for SUMA network/matrix viewing."
   "New option as well, `-mini_prob', to have some probabilistic aspect to"
   "deterministic/tract-based output."
},

{ 21, Aug , 2013 , PT , "3dTrackID" , MINOR , TYPE_MODIFY,
   "Minor changes in internal trackbundle handling/NIML-output.",
   "Temporary step to SUMAfication of program; this program will"
   "eventually be phased out in favor of 3dProbTrackID deterministic options."
},

{ 21, Aug , 2013 , PT , "3dROIMaker" , MICRO , TYPE_MODIFY,
   "Minor change to INFO_message.",
   NULL
},

{ 21, Aug , 2013 , PT , "3dNetcorr" , MICRO , TYPE_MODIFY,
   "Minor change to INFO_message.",
   NULL
},

{ 21, Aug , 2013 , PT , "3dMatch" , MICRO , TYPE_MODIFY,
   "Minor change to INFO_message.",
   NULL
},

{ 21, Aug , 2013 , PT , "3dRSFC" , MINOR , TYPE_BUG_FIX,
   "Allow subset of time series to be selected.",
   "Minor tweaking of internal workings for writing output.\n"
   "No quantitative change.\n"
},

{ 25, Mar , 2013 , PT , "3dReHo" , MINOR , TYPE_NEW_OPT,
   "More voxelwise neighborhood shapes available.",
   "Voxelwise neighborhoods can be any sphere/radius size, and even\n"
   "ellipsoidal. Some memory stuff should be better as well.\n"
},

{ 14, Mar , 2013 , PT , "3dProbTrackID" , MINOR , TYPE_MODIFY,
  "List ROI labels near start of *.grid; use floor to get NmNsThr",
  "This allows for more similarity with 3dNetCorr, and might be useful\n"
  "as well if the labeling of ROIs in a network is not just 1,..,max.\n"
  "The flooring vs ceiling is so that people don't have to use 0.00099\n"
  "as a relative fraction of MC iterations to get the number they want.\n"
},

{ 14, Mar , 2013 , PT , "3dNetCorr" , SUPER , TYPE_NEW_PROG,
  "New function: calculate correlat/Z of ROIs in a network.",
  "This program works on several networks as separate subbricks simultan-\n"
  "eously.\n"
},

{ 14, Mar , 2013 , PT , "rsfc" , MINOR , TYPE_MODIFY,
  "New functions; streamline some other functions.",
  "For addition of 3dNetCorr, mainly.\n"
},

{ 14, Mar , 2013 , PT , "3dDWUncert" , MINOR , TYPE_BUG_FIX,
   "Silly bug in e_{13}^* estimation.",
   "Mean and std of uncertainty of e1 toward e3 was buggy.\n"
},

{ 22, Feb , 2013 , PT , "3dProbTrackID" , MINOR , TYPE_NEW_OPT,
   "Instead of just individual ROI masks, can get map of Ntracks/voxel.",
   "This allows a posteriori thresholding/comparisons/analysis.\n"
},

{ 22, Feb , 2013 , PT , "3dDWUncert" , MICRO , TYPE_BUG_FIX,
   "Free as well as DELETE a dset.",
   "Ultraminor change.\n"
},

{ 22, Feb , 2013 , PT , "rsfc" , MICRO , TYPE_BUG_FIX,
   "Fixed potential float/double problem.",
   "When using 3dReHo to get ReHo for ROIs, could get *very* large numbers\n"
   "during part of calculations; floats were fine for 27 voxel neighborhood,\n"
   "but not with large ROIs. Thus, using doubles in the calc now.\n"
},

{ 22, Feb , 2013 , PT , "3dReHo" , MICRO , TYPE_BUG_FIX,
   "Fixed mask misread which could cause error in some nonmasked data.",
   NULL
},

 { 6, Feb , 2013 , PT , "3dRSFC" , MICRO , TYPE_BUG_FIX,
   "Fixed potential div by zero in 3dRSFC.",
   NULL
 },

 { 6, Feb , 2013 , PT , "3dTrackID" , MICRO , TYPE_BUG_FIX,
   "Small bug in 3dTrackID fixed.",
   "In post-run freeing of variables, had been error for a char string.\n"
 },

 { 6, Feb , 2013 , PT , "3dProbTrackID" , MINOR , TYPE_NEW_OPT,
   "Add ability to output each WM-region mask as an individual ROI.",
   "This should make it simpler to use an ROI as a mask than \n"
   "with the 2^(ROI number) labelling system within subbrick outputs.\n"
 },

 
 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
