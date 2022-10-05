
/** cf. afni_history.h **/

#include "afni_history.h"

/*  (for starting a new file, search for CHANGE)

    basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long description with intermediate newlines

    copy entire section: { ... } ,

    Notes: - months are JAN ... DEC (see afni_history.h)

           - levels are :
                    MICRO           - users don't see
                    MINOR           - small effect on users
                    MAJOR           - larger effect on users
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
                    TYPE_REMOVE     - deleted
                    TYPE_REINSTATE  - un-deleted

           - PLEASE, stick to what fits on an 80 column terminal
           - it may be nice to put the newest entries at the top
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

{ 5, Oct , 2022 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_ENHANCE,
   "Output a run_instacorr_errts.tcsh script in the results directory.",
   "Very useful for data QC. Use it.  Quick now, here, now, always...\n"
},

{ 22, Sep , 2022 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt: '-clusterize_wami ..', for Clustering functionality.",
   "Adds a 'whereami' table to the output dir.\n"
},

{ 1, Sep , 2022 , PT , "3dBrickStat" , MICRO , TYPE_BUG_FIX,
   "Fix bug: when using '-min' with '-positive' (or sim) and no mask.",
   "Tended to return 0 (now fixed).  Thanks for mentioning, Xiaowei!\n"
},

{ 30, Aug , 2022 , PT , "abids_json_tool.py" , MINOR , TYPE_NEW_OPT,
   "Add -values_stay_str flag, so num/str items stay as str type.",
   "Otherwise, by default they attempt to be int, then float, then str.\n"
},

{ 30, Aug , 2022 , PT , "abids_json_tool.py" , MINOR , TYPE_NEW_OPT,
   "Add -literal_keys flag, to turn off auto-replacement of spaces and [()].",
   "Also try to keep ints looking like ints.\n"
},

{ 25, Aug , 2022 , PT , "3ddelay" , MICRO , TYPE_BUG_FIX,
   "Make a tweak so that certain pathological cases don't scupper all others.",
   "Thanks, D. Schwartz for pointing out this behavior.\n"
},

{ 23, Aug , 2022 , PT , "p2dsetstat" , MICRO , TYPE_GENERAL,
   "Expand stataux code range to 6.",
   "Includes chi-square now.\n"
},

{ 23, Aug , 2022 , PT , "dsetstat2p" , MICRO , TYPE_GENERAL,
   "Expand stataux code range to 6.",
   "Includes chi-square now.\n"
},

{ 23, Aug , 2022 , PT , "3ddelay" , MICRO , TYPE_BUG_FIX,
   "Set correct dims being used from input dset in option_data struct.",
   "Fixes report, and some internal instances (like micro/no change?).\n"
},

{ 18, Aug , 2022 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_ENHANCE,
   "Display final EPI coverage mask on final space dset.",
   "Could be on template, final anatomical or vr_base.\n"
},

{ 18, Aug , 2022 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_ENHANCE,
   "In warns block check for 3dDeconvolve.err text file for warns.",
   "If exists, most warns go to 'undecided' at the moment. Thanks, RCR!\n"
},

{ 11, Aug , 2022 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt: '-button_press ..', for Norm/Colr/Swap buttons.",
   "Adds in functionality from driving AFNI GUI.\n"
},

{ 27, July , 2022 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_BUG_FIX,
   "In mecho block when using m_tedana on Mac OS: fix copying tedana QC HTML.",
   "Swap cp -> rsync, because Mac OS cp opts are diff than on Linux.\n"
},

{ 27, July , 2022 , PT , "dsetstat2p" , MICRO , TYPE_GENERAL,
   "Expand output precision and scale of calculability.",
   "Program used to run into trouble for large stat (bc of bc); now better.\n"
},
{ 22, Jan , 2022 , PT , "3dDepthMap" , MICRO , TYPE_GENERAL,
   "Add functionality to '-rimify RIM' opt.",
   "A negative RIM value now leads to creating an anti-rim (or core) ROI.\n"
},

{ 22, Jan , 2022 , PT , "3dDepthMap" , MICRO , TYPE_NEW_OPT,
   "Add in the '-rimify RIM' opt, where RIM is a numerical value.",
   "Transform ROIs into boundaries up to depth RIM.\n"
},

{ 4, July , 2022 , PT , "3dLocalUnifize" , MINOR , TYPE_NEW_OPT,
   "Well, a new arg for '-local_mask ..' opt.",
   "Use arg value 'None' to turn off the default automasking now.\n"
},

{ 7, June , 2022 , PT , "@djunct_edgy_align_check" , MICRO , TYPE_BUG_FIX,
   "Fix how the AMASK_FOCUS_* keywords apply for '-box_focus_slices ..'.",
   "They didn't work before, but now do/should/might/perhaps/pleeeez.\n"
},

{ 6, June , 2022 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Add new ve2a QC, via new uvar final_epi_unif_dset.",
   "Also scale ulay=EPI brightness better for EPI-to-anat align imgs.\n"
},

{ 6, June , 2022 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_NEW_OPT,
   "Some new opts to control ulay brightness, esp. for APQC HTML.",
   "New opts: '-ulay_range_am ..' and '-ulay_min_fac ..'\n"
},

{ 6, June , 2022 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Some new opts to control ulay brightness, esp. for APQC HTML.",
   "New opts: '-ulay_range_am ..' and '-ulay_min_fac ..'\n"
},

{ 3, June , 2022 , PT , "3dLocalUnifize" , MINOR , TYPE_NEW_PROG,
   "New program for unifizing brightness.",
   "Should be helpful for alignment.\n"
},

{ 11, May , 2022 , PT , "SurfLocalstat" , MINOR , TYPE_BUG_FIX,
   "The 'mean' stat was accumulating int, not float, values.",
   "This effective truncation/'digitizing' error should be fixed now.\n"
},

{ 10, May , 2022 , PT , "@djunct_overlap_check" , MICRO , TYPE_NEW_OPT,
   "Add -echo opt, and can propagate.",
   "For debugging\n"
},

{ 10, May , 2022 , PT , "@djunct_edgy_align_check" , MICRO , TYPE_NEW_OPT,
   "Add -echo opt, and can propagate.",
   "For debugging\n"
},

{ 10, May , 2022 , PT , "@SSwarper" , MICRO , TYPE_NEW_OPT,
   "Add -echo opt, and can propagate.",
   "For debugging\n"
},

{ 10, May , 2022 , PT , "@SSwarper" , MICRO , TYPE_GENERAL,
   "Update/fix to mask resampling if present and if deobliqueing with 3dWarp.",
   "Replace wsinc5 interp with NN interp---better for mask.  Thanks, RCR!\n"
},

{ 23, Apr , 2022 , PT , "@afni_refacer_make_master_addendum" , MINOR , TYPE_GENERAL,
   "Now creates a v2.0 of the shell.",
   "The new shell removes more face.  Asked for by A. Basavaraj.\n"
},
{ 23, Apr , 2022 , PT , "@afni_refacer_run" , MINOR , TYPE_NEW_OPT,
   "Can specify which shell to use, because there are newer shell(s).",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 22, Apr, 2022, PT , "afni" , MINOR, TYPE_BUG_FIX,
   "The '-bysub ..' opt wasn't working, because a 'find' cmd was bad.",
   "Reformatted the 'find', though it might still benefit from other tweaks.\n"
},

{ 14, Apr, 2022, PT , "@djunct_modal_smoothing_with_rep" , MAJOR, TYPE_BUG_FIX,
   "Replacement was not occuring if ROIs were purged by modal smoothing.",
   "Fixed that bug, hopefully improving robustness to such error in future.\n"
},

{ 1, Apr , 2022 , PT , "3dZipperZapper" , MINOR , TYPE_NEW_OPT,
   "Add '-disp_def_params' so the user can see the default params.",
   "The params will also now get displayed during runtime.\n"
},

{ 1, Apr , 2022 , PT , "3dZipperZapper" , MINOR , TYPE_NEW_OPT,
   "Add many more '-min_* ..' options for controlling badness criteria.",
   "Asked for by user nseider: hope these are helpful!\n"
},

{ 1, Apr , 2022 , PT , "3dZipperZapper" , MINOR , TYPE_MODIFY,
   "Actually output the number of bad slices per volume.",
   "Previously had some ~fancy encoding about criterion of badness (why?).\n"
},

{ 1, Apr , 2022 , PT , "3dZipperZapper" , MINOR , TYPE_NEW_OPT,
   "New opts '-dont_use_*' to turn off some of the drop criteria at will.",
   "Also put in help descriptions about drop criteria, in Notes.\n"
},

{ 20, Mar , 2022 , PT , "apqc_make_html.py" , MICRO , TYPE_BUG_FIX,
   "Fix display of subj ID from 'Top' button in case when subj ID starts num.",
   "In such cases, the unicode char for next line was misinterpreted.\n"
},

{ 10, Mar , 2022 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_BUG_FIX,
   "Fix bug in 'mecho' QC block when m_tedana used with multiple runs.",
   "All buttons used to point to r01; now fixed.\n"
},

{ 16, Feb , 2022 , PT , "ap_run_simple_rest.tcsh" , MINOR , TYPE_NEW_OPT,
   "Add opt '-compressor ..' so AFNI_COMPRESSOR env var can be set.",
   "Leads to created *.BRIK dsets getting compressed on disk.\n"
},

{ 10, Feb , 2022 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_BUG_FIX,
   "The 'pythonic' should be run if matplotlib ver is >=2.2, not just >2.2.",
   "Used incorrect comparison operator earlier. I know bc Biowulf told me so.\n"
},

{ 8, Feb , 2022 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_NEW_OPT,
   "AP now can pass some '-html_review_opts ..' values to this prog.",
   "First one: '-mot_grayplot_off', for envelope-pushing user S Torrisi.\n"
},

{ 7, Feb , 2022 , PT , "3dLocalstat" , MINOR , TYPE_NEW_OPT,
   "Add a new stat:  MCONEX, the Michelson Contrast of Extrema.",
   "mconex = |A-B|/(|A|+|B|), where A=max and B=min.\n"
},

{ 6, Feb , 2022 , PT , "3dedgedog" , MINOR , TYPE_GENERAL,
   "If -only2D opt is used, then don't blur in 3D, either.",
   "Also turn off optimized double blurring---essentially not time diff.\n"
},

{ 6, Feb , 2022 , PT , "3dedgedog" , MINOR , TYPE_BUG_FIX,
   "Fix some badness when 4D datasets are input.",
   "Also have a better scale value, based on 3dLocalstat sigma.\n"
},

{ 3, Feb , 2022 , PT , "@chauffeur_afni" , MICRO , TYPE_NEW_OPT,
   "Two new opts, using existing AFNI env vars",
   "Now have '-left_is_left ..' and '-left_is_posterior ..'.\n"
},

{ 1, Feb , 2021 , PT , "3dedge3" , MINOR , TYPE_NEW_OPT,
   "Adding -automask (and -automask+X) functionality.",
   "Mainly to help with comparisons with 3dedgedog.\n"
},

{ 1, Feb , 2022 , PT , "@SSwarper" , MICRO , TYPE_GENERAL,
   "Some clearer error messaging (esp. if not '-base ..' is used).",
   "Remove any non-programmatic exclamation marks--even from comments.\n"
},

{ 26, Jan , 2022 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Update QC block: vorig now shows the epi-anat overlap.",
   "Shows overlap with ignoring obliquity and applying it (if present).\n"
},

{ 26, Jan , 2022 , PT , "@djunct_overlap_check" , MICRO , TYPE_BUG_FIX,
   "Fix how -box_focus_slices opt works in another aspect.",
   "Now should actually be used (had to turn off internal coord selection).\n"
},

{ 26, Jan , 2022 , PT , "@djunct_overlap_check" , MINOR , TYPE_BUG_FIX,
   "Fix how -box_focus_slices opt works if one of the AMASK* keywords is used.",
   "Previously was producing error, not recognizing it wasn't a dset.\n"
},

{ 25, Jan , 2022 , PT , "@djunct_overlap_check" , MINOR , TYPE_NEW_OPT,
   "Add in existin chauffeur options: -no_cor, -no_axi, -no_sag.",
   "For APQC---vorig of initial overlap.\n"
},

{ 25, Jan , 2022 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Update QC block: vorig now shows the 'copy_anat' dset.",
   "Applies in all cases; AP just passes along uvar.  Thanks, RCR!\n"
},

{ 24, Jan , 2022 , PT , "3dDepthMap" , MICRO , TYPE_GENERAL,
   "Renaming some internal funcs, for clarity.",
   "Multi-ROI funcs get '_GEN' in name now, to distinguish from '_BIN' ones.\n"
},

{ 20, Jan , 2022 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Update QC block: vstat (for task-based FMRI cases).",
   "There will now be typically up to 5 stats dsets shown (GLT and other).\n"
},

{ 18, Jan , 2022 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Add a new QC block:  mecho.",
   "This is for multi-echo (ME) FMRI; mostly for m_tedana right now.\n"
},

{ 18, Jan , 2022 , PT , "apqc_make_html.py" , MINOR , TYPE_GENERAL,
   "New functions/functionality for the new QC block:  mecho.",
   "Also tweaked/updated the help.\n"
},

{ 13, Jan , 2022 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Do a check if the user asks for 'pythonic' APQC.",
   "If their system CAN'T HANDLE THE TRUTH, then downgrade kindly to 'basic'.\n"
},

{ 12, Jan , 2022 , PT , "3dDepthMap" , MINOR , TYPE_GENERAL,
   "Rename program:  3dEulerDist -> 3dDepthMap.",
   "The original name was odd for *Euclidean* Distance Transform, anyways...\n"
},

{ 12, Jan , 2022 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_BUG_FIX,
   "Add '-save_ftype ..' opt to this script, to make @animal_warper happy.",
   "Thanks, DRG, for pointing this out.\n"
},

{ 29, Dec , 2021 , PT , "3dedgedog" , MINOR , TYPE_GENERAL,
   "Change default sigma_rad to be 1.4, not 2.0, to capture more details.",
   "This is because results look much better in human T1w dset.\n"
},

{ 27, Dec , 2021 , PT , "balloon" , MICRO , TYPE_GENERAL,
   "Use printf(...) to display the program help, not fprintf(stderr, ...).",
   "In this way, the Sphinx help docs can see it.\n"
},

{ 26, Dec , 2021 , PT , "3dedgedog" , MINOR , TYPE_NEW_OPT,
   "The -automask (and -automask+X) functionality is now, well, functional.",
   "The '-mask ..' option appears to be working, too.\n"
},

{ 26, Dec , 2021 , PT , "3dEulerDist" , MINOR , TYPE_GENERAL,
   "Give correct name of opt in prog help: -bounds_are_not_zero.",
   "Had forgotten the '_not' part previously.  Whoops.\n"
},

{ 26, Dec , 2021 , PT , "3dedgedog" , MICRO , TYPE_GENERAL,
   "Full histories in each output dset now.",
   "Had not been passing argc and argv previously.\n"
},

{ 24, Dec , 2021 , PT , "3dedgedog" , MICRO , TYPE_GENERAL,
   "Because 3dEulerDist has new '-binary_only' opt, this prog is faster.",
   "This is because edgedog at the moment runs EDT on a binary dset.\n"
},

{ 24, Dec , 2021 , PT , "3dEulerDist" , MINOR , TYPE_NEW_OPT,
   "Can process some dsets much faster now, with opt '-binary_only'.",
   "This is to flag that the input is a binary mask.\n"
},

{ 9, Dec , 2021 , PT , "3dEulerDist" , MICRO , TYPE_BUG_FIX,
   "Fix new -only2D opt slice selection.",
   "Was not always getting correct planar direction; should be now.\n"
},
 
{ 9, Dec , 2021 , PT , "3dedgedog" , MICRO , TYPE_NEW_OPT,
   "Add in '-only2D ..' opt, similar to 3dEulerDist's one.",
   "Now can get planar edges, instead of always volumetric ones.\n"
},

{ 8, Dec , 2021 , PT , "3dEulerDist" , MINOR , TYPE_GENERAL,
   "Already change something internally about only2D calcs.",
   "Should just be simple change, being more general.\n"
},

{ 8, Dec , 2021 , PT , "3dEulerDist" , MINOR , TYPE_NEW_OPT,
   "Can run in 2D now, with opt '-only2D ..'.",
   "For Daniel Glen.\n"
},

{ 7, Dec , 2021 , PT , "3dedgedog" , MICRO , TYPE_NEW_OPT,
   "Add in optional scaling of edges, via '-edge_bnd_scale'. B",
   "Related but slightly different scaling based on '-edge_bnd_side' opt.\n"
},
 
{ 3, Dec , 2021 , PT , "3dedgedog" , MICRO , TYPE_NEW_PROG,
   "Calculate edges with the Difference of Gaussian (DOG) approach.",
   "Thanks to DR Glen and C Rorden for discussions/suggestions on this.\n"
},

{ 2, Dec , 2021 , PT , "afni" , MICRO , TYPE_GENERAL,
   "Adding the description of existing option(s) in the program help.",
   "The opts are the synonyms: '-notcsv', '-notsv', '-nocsv'.\n"
},

{ 1, Dec , 2021 , PT , "3dEulerDist" , MINOR , TYPE_BUG_FIX,
   "Was getting incorrect voxel scaling along a couple axes---fixed now.",
   "Also re-arrange functions to be easier to call from other funcs.\n"
},

{ 30, Nov , 2021 , PT , "3dEdu_01_scale" , MICRO , TYPE_GENERAL,
   "Added many more internal comments, e.g., codebase references.",
   "Thanks for the discussion+suggestions, J Teves!\n"
},

{ 30, Nov , 2021 , PT , "3dEulerDist" , MINOR , TYPE_NEW_PROG,
   "New C prog for Eulerian Distance Transform (EDT) for ROI-based dsets.",
   "Calculate distances to boundaries within a FOV.\n"
},

{ 26, Nov , 2021 , PT , "3dEdu_01_scale" , MICRO , TYPE_GENERAL,
   "Renaming of 3dEduProg.  Simplifying some inner workings",
   "Basic AFNI program example.\n"
},

{ 26, Nov , 2021 , PT , "3dEduProg" , MICRO , TYPE_NEW_PROG,
   "A new program for people to learn to write AFNI progs.",
   "Perhaps a bit more basic I/O and usage than 3dToyProg.\n"
},

{ 16, Nov , 2021 , PT , "afni_system_check.py" , MICRO , TYPE_GENERAL,
   "Add 'SLURM cluster'-specific check for number of CPUs.",
   "Phase two of secret plan to steal all of Rick's programs.  Bwahahahaha.\n"
},

{ 13, Nov , 2021 , PT , "afni_system_check.py" , MICRO , TYPE_NEW_OPT,
   "New '-disp_num_cpu' opt to display number of available CPUs.",
   "Phase one of secret plan to steal all of Rick's programs.  Bwahahaha.\n"
},

{ 29, Oct , 2021 , PT , "@Install_NMT" , MINOR , TYPE_GENERAL,
   "Update macaque template+atlas data.",
   "Now working/defaulting to NMT v2.1.\n"
},

{ 29, Oct , 2021 , PT , "@Install_MACAQUE_DEMO_REST" , MINOR , TYPE_GENERAL,
   "Update macaque demo for resting state FMRI processing.",
   "New scripts, now working/defaulting to NMT v2.1.\n"
},

{ 27, Oct , 2021 , PT , "@animal_warper" , MINOR , TYPE_BUG_FIX,
   "Opt '-extra_qw_opts ..' had wrong name in help file, '-qw_opts ..'.",
   "Corrected this, as well as usage.\n"
},

{ 25, Oct , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Improve QC outputs, and fix some chauffeur ranges.",
   "Hopefully easier to evaluate alignment now, among other features.\n"
},

{ 23, Oct , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "For ROI image QC, use ROI_glasbey_2048 now by default.",
   "CHARM has ROI values >256, so might as well go all in.\n"
},

{ 22, Oct , 2021 , PT , "apqc_make_html.py" , MICRO , TYPE_GENERAL,
   "Report a better output path in the 'done' message.",
   "The originally-output relative path was often not useful.\n"
},

{ 21, Oct , 2021 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "Max blowup factor is actually 8, not 4.",
   "So, allow that fuller range in the internal number check.\n"
},

{ 2, Oct , 2021 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "Copy input anat (and any mask_ss) into the output directory.",
   "Can be useful for checking if things went awry (do they ever?!?).\n"
},

{ 29, Sep , 2021 , PT , "3dAllineate" , MAJOR , TYPE_GENERAL,
   "3dAllineate: set -lpa+ to re-include 'ov' in its recipe---for stability.",
   "This makes it closer to historical form (but no 'mi' still).\n"
},

{ 29, Sep , 2021 , PT , "@MakeLabelTable" , MINOR , TYPE_BUG_FIX,
   "Fix behavior with longnames---just needed a quote around var.",
   "Should work now.  Also update help.\n"
},

{ 29, Sep , 2021 , PT , "lesion_align" , MINOR , TYPE_GENERAL,
   "Just running '-help' leads to lesion_outs.txt to be created and populated.",
   "... and it also got overwritten oddly.  Move those lines further down.\n"
},

{ 27, Sep , 2021 , PT , "lesion_align" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_align_anat_pair" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "adjunct_apqc_tsnr_general" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_convert_dcm_anat" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_imit2w_from_t1w" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@djunct_4d_imager" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "adjunct_suma_fs_mask_and_qc" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_axialize_anat" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@djunct_overlap_check" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "fat_proc_decmap" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@afni_refacer_run" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 27, Sep , 2021 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "On/about Aug 23, 2021, default label_sizes in image windows changed.",
   "That shrunk fonts down one size; now bump back up @chauffeur_afni calls.\n"
},

{ 23, Sep , 2021 , PT , "@epi_b0_corr.py" , MINOR , TYPE_NEW_OPT,
   "Had been missing the internal processing of option '-epi_pe_bwpp'.",
   "... which has now been added in.\n"
},

{ 23, Sep , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Can now perform clusterizing, with Alpha+Boxed on, like in GUI.",
   "New opt '-clusterize ..' for some commands; see help/NOTES for full info.\n"
},

{ 21, Sep , 2021 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_NEW_OPT,
   "Add in new opts to turn off images in particular view planes being made.",
   "These are '-no_cor', '-no_axi', '-no_sag'.  First will help APQC.\n"
},

{ 21, Sep , 2021 , PT , "adjunct_apqc_tsnr_general" , MINOR , TYPE_NEW_OPT,
   "Add in new opts to turn off images in particular view planes being made.",
   "These are '-no_cor', '-no_axi', '-no_sag'.  First will help APQC.\n"
},

{ 21, Sep , 2021 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_NEW_OPT,
   "Add in new opts to turn off images in particular view planes being made.",
   "These are '-no_cor', '-no_axi', '-no_sag'.  First will help APQC.\n"
},

{ 21, Sep , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Add in new opts to turn off images in particular view planes being made.",
   "These are '-no_cor', '-no_axi', '-no_sag'.  First will help APQC.\n"
},

{ 20, Sep , 2021 , PT , "@grayplot" , MINOR , TYPE_GENERAL,
   "apsearchize.",
   "Make help visible in editor with '@grayplot -hview'.\n"
},

{ 20, Sep , 2021 , PT , "3dGrayplot" , MINOR , TYPE_NEW_OPT,
   "Add '-raw_with_bounds ..' to display raw values in arbitrary interval.",
   "Maybe most useful if data have been scaled.\n"
},

{ 9, Sep , 2021 , PT , "@4Daverage" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@FindAfniDsetPath" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@Purify_1D" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@RenamePanga" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@djunct_edgy_align_check" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@djunct_glue_imgs_vert" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@djunct_overlap_check" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 9, Sep , 2021 , PT , "@djunct_anonymize" , MICRO , TYPE_GENERAL,
   "Shebang changed from csh to tcsh.",
   "For uniformity/simplicity, and to avoid issues on occasional system.\n"
},

{ 31, Aug , 2021 , PT , "@afni_refacer_make_master" , MINOR , TYPE_GENERAL,
   "Change default cost function to 'lpa', from 'ls'.",
   "Should be better? There is an opt to change, as necessary.\n"
},

{ 30, Aug , 2021 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "New opts: '-all_roi_zeros' and '-automask_off'.",
   "Basically, N ROIs can have NxN mat, even if ROI ave is all zeros.\n"
},

{ 22, Aug , 2021 , PT , "lib_afni1D.py" , MICRO , TYPE_BUG_FIX,
   "Afni1D.uncensor_from_vector() had a syntax error in one print call').",
   "Was missing a %, now fixed.\n"
},

{ 10, Aug , 2021 , PT , "fat_proc_align_anat_pair" , MINOR , TYPE_MODIFY,
   "No longer worry about even/odd slice output (and remove '-no_fs_prep').",
   "Dealt with an old non-issue.\n"
},

{ 29, July , 2021 , PT , "@djunct_overlap_check" , MINOR , TYPE_NEW_OPT,
   "Add in @chauffeur_afni functionality:  -edgy_ulay.",
   NULL
},

{ 27, Jul, 2021, PT, "afni-general", MINOR, TYPE_BUG_FIX,
   "fix typo in cubic resampling for viewer/3dresample (afni_slice.c)",
   "Copying Rick's fix. Thanks to user 'ymao' for raising this issue on the MB."
 } ,

{ 9, July , 2021 , PT , "@animal_warper" , MINOR , TYPE_NEW_OPT,
   "New: '-aff_move_opt ..' to use more than just giant_move in aff step.",
   "Also bug fix for when no followers were entered.\n"
},

{ 30, June, 2021 , PT , "adjunct_aw_tableize_roi_info.py", MINOR, TYPE_GENERAL,
   "Tweak column names *again*.",
   "Add in extra check that 3D vols are specified (e.g., with selectors).\n"
},

{ 28, June, 2021 , PT , "adjunct_aw_tableize_roi_info.py", MINOR, TYPE_GENERAL,
   "Reformat report*.1D tables a bit: match key and col names.",
   "Also, improve/simplify/clarify names of cols.  Thanks, Adam Messinger.\n"
},

{ 28, June , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Well, OK, not *really* a new opt---new values accepted for existing opt.",
   "The '-olay_alpha ..' can now take Linear or Quadratic (grazie, Bob).\n"
},

{ 24, June , 2021 , PT , "@djunct_overlap_check" , MINOR , TYPE_GENERAL,
   "Silence 3drefit warns if changing space of dsets (might confuse users).",
   "Unnecessary warning for these temp dsets within the script.\n"
},

{ 22, June , 2021 , PT , "3dNwarpCalc" , MINOR , TYPE_GENERAL,
   "Put in std includes to be able to build on Mac with Clang.",
   "Though this program isn't even built...\n"
},

{ 22, June , 2021 , PT , "SurfLayers" , MINOR , TYPE_GENERAL,
   "Capture verbose output from ConvertSurface.",
   "Also add -no_clean opt.\n"
},

{ 18, June , 2021 , PT , "@Install_AP_MULTI_DEMO1" , MINOR , TYPE_GENERAL,
   "Adding install script for afni_proc.py multi-echo FMRI demo (OHBM, 2021).",
   "Demo authors: RC Reynolds, SJ Gotts, AW Gilmore, DR Glen, PA Taylor.\n"
},

{ 18, June , 2021 , PT , "@Install_SURFLAYERS_DEMO1" , MINOR , TYPE_GENERAL,
   "Created by Sam Torrisi.  Help added, temp placeholder data now in place.",
   "Will add full demo data soon...\n"
},

{ 15, June , 2021 , PT , "@radial_correlate" , MINOR , TYPE_BUG_FIX,
   "Minor bug fix (never hit?), and avoid single line 'if' conds.",
   "Latter to avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@clean_help_dir" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@RetinoProc" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@AddEdge" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_Make_Spec_Caret" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@build_afni_Xlib" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@fix_FSsphere" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@toMNI_Qwarpar" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "ap_run_simple_rest.tcsh" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@Reorder" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_Make_Spec_SF" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@T1scale" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@afni.run.me" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@diff.files" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@get.afni.version" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@grayplot" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@parse_afni_name" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@parse_name" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@simulate_motion" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@update.afni.binaries" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@2dwarper.Allin" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@AddEdge" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@ExamineGenFeatDists" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@FS_roi_label" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@Install_InstaCorr_Demo" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@MakeLabelTable" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_Make_Spec_SF" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@ScaleVolume" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@afni_refacer_make_master" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@afni_refacer_make_onebigA12" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@auto_tlrc" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@clip_volume" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@compute_gcor" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@diff.tree" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@fix_FSsphere" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@move.to.series.dirs" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@snapshot_volreg" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@update.afni.binaries" , MINOR , TYPE_GENERAL,
   "Avoid single line 'if' conds.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_AlignToExperiment" , MINOR , TYPE_GENERAL,
   "Clean up some spacing; avoid single line 'if' conds; use unaliased rm.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "lesion_align" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_imit2w_from_t1w" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_dwi_to_dt" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_decmap" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_convert_dcm_anat" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_axialize_anat" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "fat_proc_align_anat_pair" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@toMNI_Awarp" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@radial_correlate" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@measure_in2out" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@measure_erosion_thick" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@measure_bb_thick" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@djunct_slice_space" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@diff.tree" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@diff.files" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@auto_tlrc" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@afni_refacer_make_master" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SUMA_AlignToExperiment" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@MakeLabelTable" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@Install_ClustScat_Demo" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@GradFlipTest" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@FindAfniDsetPath" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@FS_roi_label" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@Atlasize" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@AddEdge" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@toMNI_Qwarpar" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@snapshot_volreg" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@grayplot" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@global_parse" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@get.afni.version" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@djunct_glue_imgs_vert" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@afni_refacer_make_onebigA12" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@ROI_decluster" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@1dDiffMag" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 15, June , 2021 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "Put spaces in if-conditions after 'if' and before 'then'.",
   "To avoid badness in some (older?) tcsh versions.\n"
},

{ 14, June , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_BUG_FIX,
   "Space before 'then' in if-cond; also remove all exclamations in comments.",
   "Resolved *very* weird opt parsing on *some* old tcsh.\n"
},

{ 10, June , 2021 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "New opt '-echo', for odd-behavior-on-other-systems-investigtion-purposes.",
   "Also print AFNI and program version numbers at top.\n"
},

{ 2, June , 2021 , PT , "afni_proc.py" , MICRO , TYPE_GENERAL,
   "Undo previous tweak to db_mod.py (for auto_warp.py); back to using *.nii.",
   "Reverting, because AFNI_COMPRESSOR has been updated.\n"
},

{ 2, June , 2021 , PT , "auto_warp.py" , MINOR , TYPE_GENERAL,
   "Undo previous tweak to auto_warp.py; back to using *.nii.",
   "Reverting, because AFNI_COMPRESSOR has been updated.\n"
},

{ 30, May , 2021 , PT , "afni_proc.py" , MICRO , TYPE_GENERAL,
   "Tweak db_mod.py: prep for auto_warp.py to now always use *.nii.gz.",
   "Just mv *.nii.gz files, rather than *.nii, from auto_warp output dir.\n"
},

{ 30, May , 2021 , PT , "auto_warp.py" , MINOR , TYPE_GENERAL,
   "Use *.nii.gz, not *.nii, because of current AFNI_COMPRESSOR = GZIP.",
   "With current AFNI_COMPRESSOR = GZIP, get problems; now, no more.\n"
},

{ 30, May , 2021 , PT , "@auto_tlrc" , MINOR , TYPE_NEW_OPT,
   "New opt '-use_gz' to output gzipped NIFTI even with '-suffix ..'.",
   "Part of updating auto_warp.py to use *.nii.gz, not *.nii.\n"
},

{ 24, May , 2021 , PT , "@animal_warper" , MAJOR , TYPE_GENERAL,
   "Several small updates for convenience and organization: help updated...",
   "more QC images; split intermediate text desc; new cmd_log.\n"
},

{ 20, May , 2021 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "Clean up exiting from help and version checking.",
   "Doesn't go via the verbose GOOD_EXIT route anymore, which it shouldn't.\n"
},

{ 12, May , 2021 , PT , "@SUMA_Make_Spec_FS" , MICRO , TYPE_GENERAL,
   "Remove old/unnecessary comment from help of -NIFTI opt.",
   "Referred to earlier misconcept (need even mat dims for anatomical dset).\n"
},

{ 11, May , 2021 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "Set env var to turn off NIFTI warnings.",
   "That is, AFNI_NIFTI_TYPE_WARN -> NO.\n"
},

{ 11, May , 2021 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "1dplot.py improved for backward compatability to Python 2.7.",
   "So, for task FMRI, individual stim label plots work again in Py2.7.\n"
},

{ 11, May , 2021 , PT , "1dplot.py" , MICRO , TYPE_GENERAL,
   "Replace str.isnumeric() with str.isdigit(), for backward compatability.",
   "Python 2.7 didn't have that method for str type.\n"
},

{ 10, May , 2021 , PT , "@chauffeur_afni" , MICRO , TYPE_NEW_OPT,
   "New opt: '-set_xhair_gap ..', to allow setting crosshair gap.",
   "Default value is -1.\n"
},

{ 3, May , 2021 , PT , "3dClusterize" , MICRO , TYPE_GENERAL,
   "Add bracket to meta-text above table in case of abs value in table.",
   "Thanks, watchful AFNI user YurBoiRene.\n"
},

{ 29, Apr , 2021 , PT , "3dClusterize" , MINOR , TYPE_NEW_OPT,
   "Forgot to actually add in the new opt for data scaling in last change...",
   "Now opt '-abs_table_data' is in the code.\n"
},

{ 29, Apr , 2021 , PT , "3dClusterize" , MINOR , TYPE_BUG_FIX,
   "1) Now apply any scaling to 'data' in table (wasn't scaling, before).",
   "2) Change table def: don't abs val Mean and SEM; use opt for that.\n"
},

{ 23, Apr , 2021 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "1dplot.py improved, so stimulus labels on y-axis will wrap.",
   "Reduce/remove overlap of long stim labels.\n"
},

{ 23, Apr , 2021 , PT , "1dplot.py" , MINOR , TYPE_NEW_OPT,
   "Can force ylabels to wrap at a certain num of chars (-ylabels_maxlen ..).",
   "For APQC, so long stimulus labels don't run into each other.\n"
},

{ 16, Apr , 2021 , PT , "adjunct_apqc_tsnr_general" , MINOR , TYPE_NEW_OPT,
   "More options from @chauffeur_afni here.",
   "Tryin' to make nicer images.\n"
},

{ 16, Apr , 2021 , PT , "adjunct_apqc_tsnr_with_mask" , MINOR , TYPE_REMOVE,
   "This program has been superceded by: adjunct_apqc_tsnr_general.",
   "The new version is more... general.\n"
},

{ 16, Apr , 2021 , PT , "adjunct_apqc_tsnr_no_mask" , MINOR , TYPE_REMOVE,
   "This program has been superceded by: adjunct_apqc_tsnr_general.",
   "The new version is more... general.\n"
},

{ 16, Apr , 2021 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "Use newer adjunct_apqc_tsnr_general for TSNR images.",
   "Single/more adjunct general prog than previous separate ones.\n"
},

{ 16, Apr , 2021 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "Internal logic for making TSNR dsets tweaked.",
   "TSNR images in QC*/media/ dir get unique name, too (no change for user).\n"
},

{ 16, Apr , 2021 , PT , "adjunct_apqc_tsnr_general" , MINOR , TYPE_NEW_PROG,
   "Made to replace adjunct_apqc_tsnr_with_mask and adjunct_apqc_tsnr_no_mask.",
   "Also expands/generalizes this functionality.\n"
},

{ 16, Apr , 2021 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "New keyword EMPTY for '-topval ..' opt, make scripting easier.",
   "Corrected discrete cbar help example, too.\n"
},

{ 24, Mar , 2021 , PT , "3dinfo" , MICRO , TYPE_NEW_OPT,
   "New opt: -is_atlas_or_labeltable.",
   "1 if dset has an atlas or labeltable;  otherwise, 0.\n"
},

{ 24, Mar , 2021 , PT , "3dBrickStat" , MAJOR , TYPE_BUG_FIX,
   "Fix bug: having non-full-FOV mask + perc calcs affected other calcs.",
   "Calcs should now be consistent even with those opts used. Thanks, RCR.\n"
},

{ 23, Mar , 2021 , PT , "3dBrickStat" , MICRO , TYPE_GENERAL,
   "Uniformize internal spacing. Should be no change in behavior.",
   "Just a few comments stretch far still.\n"
},

{ 16, Mar , 2021 , PT , "@djunct_overlap_check" , MICRO , TYPE_NEW_OPT,
   "Set env AFNI_COMPRESSOR to NONE.",
   "Avoid minor badnesses occasionally.\n"
},

{ 16, Mar , 2021 , PT , "@djunct_4d_imager" , MICRO , TYPE_NEW_OPT,
   "Set env AFNI_COMPRESSOR to NONE.",
   "Avoid minor badnesses occasionally.\n"
},

{ 16, Mar , 2021 , PT , "@djunct_edgy_align_check" , MICRO , TYPE_NEW_OPT,
   "Set env AFNI_COMPRESSOR to NONE.",
   "Avoid minor badnesses occasionally.\n"
},

{ 10, Mar , 2021 , PT , "adjunct_apqc_tsnr_with_mask" , MINOR , TYPE_NEW_OPT,
   "Add in more control features, so can apply in more cases.",
   "Basically just allowing more chauffeur control.\n"
},

{ 8, Mar , 2021 , PT , "3dRSFC" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "map_TrackID" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dZipperZapper" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dNetCorr" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dDWUncert" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dLombScargle" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dMatch" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dROIMaker" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3ddot_beta" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dAmpToRSFC" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dReHo" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 8, Mar , 2021 , PT , "3dTrackID" , MICRO , TYPE_MODIFY,
   "I/O strings now up to THD_MAX_NAME length; requested by L Waller.",
   NULL
},

{ 6, Mar , 2021 , PT , "@fat_tract_colorize" , MINOR , TYPE_GENERAL,
   "Run a bit more quietly, setting ENV vars and GUI opts.",
   "Fewer warnings and messages.  And tweak help to be more useful.\n"
},

{ 6, Mar , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_GENERAL,
   "Run a bit more quietly, setting ENV vars and GUI opts.",
   "Fewer warnings and messages.\n"
},

{ 6, Mar , 2021 , PT , "@snapshot_volreg" , MINOR , TYPE_GENERAL,
   "Run a bit more quietly, setting ENV vars and GUI opts.",
   "Fewer warnings and messages.\n"
},

{ 5, Mar , 2021 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Have APQC copy the ss_review_basic text file into the QC dir.",
   "Might want this text info available for easy parsing at group level.\n"
},

{ 3, Mar , 2021 , PT , "adjunct_suma_fs_mask_and_qc" , MINOR , TYPE_BUG_FIX,
   "Had an early exit from earlier debugging.",
   "Ironic, really, that a debugging line became a bug.  Go figure.\n"
},

{ 3, Mar , 2021 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_ENHANCE,
   "Make nice new WB mask, make some QC images of mask/segs/ROIs.",
   "Also make tables of ROI info (size stuff).\n"
},

{ 3, Mar, 2021, PT , "gen_ss_review_scripts.py" , MINOR, TYPE_GENERAL,
   "Add sswarper2 to name of recognized progs for getting template dset.",
   "Can get uvar for APQC for this top secret NL alignment prog.\n"
},

{ 3, Mar, 2021, PT , "@djunct_modal_smoothing_with_rep" , MINOR, TYPE_BUG_FIX,
   "On one system an instrutable error message 'Unknown user: 1~.' occurred.",
   "This change (doublequote file name? remove EOL in backticks?) fixed it.\n"
},

{ 1, Mar , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Fix output dir of an intermediate QC image.",
   "The init*uaff* should now be in the usual QC/ dir.\n"
},

{ 25, Feb , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "With non-nonlinear warps, processing now goes all they way through.",
   "Bit more *.txt output, fixed mapping of anat follower non-ROI dset.\n"
},

{ 24, Feb , 2021 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Add more TSNR plotting: if vreg TSNR is calc'ed, or if no mask exists.",
   "Also a bug fix in HAVE_MASK definition; fix ranges in some plots.\n"
},

{ 24, Feb , 2021 , PT , "adjunct_apqc_tsnr_no_mask" , MINOR , TYPE_NEW_PROG,
   "Now used in APQC to make TSNR plot.",
   "Has mostly required olay/ulay args, for when *no* mask exists.\n"
},

{ 22, Feb , 2021 , PT , "adjunct_apqc_tsnr_with_mask" , MINOR , TYPE_NEW_PROG,
   "Now used in APQC to make TSNR plot.",
   "Has mostly required olay/ulay args, as well as mask.\n"
},

{ 22, Feb , 2021 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "New part of regr block: TSNR plot.",
   "Shows brain slices.\n"
},

{ 22, Feb , 2021 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt: -pbar_comm_gen, for APQC.",
   "Also remove warning about ffmpeg unless using MPEG.\n"
},

{ 10, Feb , 2021 , PT , "@SSwarper" , MINOR , TYPE_NEW_OPT,
   "Add in -mask_ss option, to replace skullstripping with a mask.",
   "For example, using fs*mask*nii from @SUMA_Make_Spec_FS after FS.\n"
},

{ 10, Feb , 2021 , PT , "adjunct_suma_fs_mask_and_qc" , MINOR , TYPE_GENERAL,
   "More QC images: WM and GM tissue, solo.",
   "Thanks for suggestions, P Molfese.\n"
},

{ 9, Feb , 2021 , PT , "adjunct_suma_roi_info" , MINOR , TYPE_GENERAL,
   "New column of ROI vol fraction, relative to fs_parc_wb_mask.nii.gz.",
   "This prog should always be run after adjunct_suma_fs_mask_and_qc.\n"
},

{ 9, Feb , 2021 , PT , "adjunct_suma_fs_mask_and_qc" , MINOR , TYPE_GENERAL,
   "Renamed, from adjunct_suma_fs_qc.tcsh.",
   "Removing extension.\n"
},

{ 9, Feb , 2021 , PT , "adjunct_suma_roi_info" , MINOR , TYPE_GENERAL,
   "Renamed, from adjunct_suma_rois_qc.tcsh.",
   "Removing extension.\n"
},

{ 9, Feb , 2021 , PT , "adjunct_suma_fs_qc.tcsh" , MINOR , TYPE_GENERAL,
   "Make new mask dset from parcellation.",
   "Add new image of new dset.\n"
},

{ 9, Feb , 2021 , PT , "adjunct_suma_rois_qc.tcsh" , MINOR , TYPE_GENERAL,
   "Add fractional volume info to the text file.",
   "This makes 2 new columns in the output *.1D files.\n"
},

{ 8, Feb , 2021 , PT , "adjunct_suma_rois_qc.tcsh" , MINOR , TYPE_NEW_PROG,
   "Will add to @SUMA_Make_Spec_FS for automatic QC output.",
   "This makes *.1D files of voxel counts of parcellations and segs.\n"
},

{ 8, Feb , 2021 , PT , "adjunct_suma_fs_qc.tcsh" , MINOR , TYPE_NEW_PROG,
   "Will add to @SUMA_Make_Spec_FS for automatic QC output.",
   "This makes images of the brain mask, tissue segs and parcellation.\n"
},

{ 5, Feb , 2021 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "Add in more intermediate QC snapshots (intermed align): init*jpg ",
   "Also add '-echo' opt for verbose terminal stuff.\n"
},

{ 3, Feb , 2021 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "New QC image, of affine warping.",
   "Also pass along '-echo' opt to modal smoo/report script.\n"
},

{ 27, Jan , 2021 , PT , "3dAttribute" , MINOR , TYPE_GENERAL,
   "Update help to have fancy sections and more examples.",
   "Also provide link to README.attributes file, for user reference.\n"
},

{ 27, Jan , 2021 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_ENHANCE,
   "New run script created (run_01*) in L_MAKE_DRIVE_SCRIPT block.",
   "Opens std.141*both*spec in SUMA and SurfVol in AFNI, and starts talking.\n"
},

{ 22, Dec , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "New default feature_size: 0.5.  (Old default: was unset).",
   "Made almost no dif in mac demo, but should be slightly more robust, in gen.\n"
},

{ 22, Dec , 2020 , PT , "3dmaskave" , MINOR , TYPE_BUG_FIX,
   "MRI_TYPE_maxval fixed for byte case-- thanks, C Rorden!",
   "Same fix applied in: plug_maskave.c and thd_makemask.c.\n"
},

{ 21, Dec , 2020 , PT , "3dClusterize" , MICRO , TYPE_GENERAL,
   "Tweak internal handling of reading inputs, prohibit hanging args.",
   "Now, hanging args should produce error (not just be silently ignored).\n"
},

{ 21, Dec , 2020 , PT , "3dROIMaker" , MICRO , TYPE_GENERAL,
   "Tweak internal handling of reading inputs, prohibit hanging args.",
   "Now, hanging args should produce error (not just be silently ignored).\n"
},

{ 21, Dec , 2020 , PT , "3dClusterize" , MINOR , TYPE_BUG_FIX,
   "The '-orient ..' opt wasn't working-- that has been fixed.",
   "Now user can specify table coords with this opt.\n"
},

{ 1, Dec , 2020 , PT , "@SSwarper" , MINOR , TYPE_NEW_OPT,
   "Tweaked default temp 'junk' filename to avoid low-probability badness ",
   "New opt '-tmp_name_nice' for, well, read opt name. Improved help, too.\n"
},

{ 12, Nov , 2020 , PT , "fat_proc_connec_vis" , MICRO , TYPE_GENERAL,
   "Set some env vars at top of script to turn off compression.",
   "Was causing odd error in one case.\n"
},

{ 6, Nov , 2020 , PT , "adjunct_tort_read_dp_align.py" , MINOR , TYPE_NEW_PROG,
   "Script to read TORTOISE-DIFFPREP *_transformations.txt files.",
   "Puts them into usable order for calc'ing enorm and plotting mot/pars.\n"
},

{ 6, Nov , 2020 , PT , "adjunct_tort_plot_dp_align" , MINOR , TYPE_NEW_PROG,
   "Script (tcsh) to translate TORTOISE-DIFFPREP *_transformations.txt files",
   "Wraps new adjunct_tort_read_dp_align.py and 1dplot* to make plots.\n"
},

{ 2, Nov , 2020 , PT , "1dplot.py" , MINOR , TYPE_GENERAL,
   "Can now output SVG files, and can use newline chars in labels.",
   "Had to deal with newline escape seq internally.\n"
},

{ 28, Oct , 2020 , PT , "fat_proc_map_to_dti" , MINOR , TYPE_GENERAL,
   "Extra QC image:  initial overlap of source and base dsets.",
   "Should help to know, in case anything goes awry later.\n"
},

{ 28, Oct , 2020 , PT , "fat_proc_align_anat_pair" , MINOR , TYPE_GENERAL,
   "Extra QC image:  initial overlap of T1w and T2w dsets.",
   "Should help to know, in case anything goes awry later.\n"
},

{ 19, Oct , 2020 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "Added new QC image: initial source-base alignment (@djunct_overlap_check)",
   "If obl, make 1 img ignoring it, and 1 3dWarp-deob'ed, with text report.\n"
},

{ 19, Oct , 2020 , PT , "@djunct_overlap_check" , MAJOR , TYPE_NEW_PROG,
   "Make of overlap of 2 datasets (esp for pre-align check, AW or SSW).",
   "Will make both non-obl and 3dWarp-deob'ed images of olap (and report).\n"
},

{ 19, Oct , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Added new QC image to QC/ dir: initial source-base alignment.",
   "If obl, make 1 img ignoring it, and 1 3dWarp-deob'ed, with text report.\n"
},

{ 19, Oct , 2020 , PT , "@animal_warper" , MAJOR , TYPE_GENERAL,
   "Simplifying output dir: Phase II and III.  Thanks again, B Jung!",
   "New intermediate dir, animal_outs update, helpfile rewritten.\n"
},

{ 16, Oct , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Simplifying output dir: Phase I.  Thanks, Ben Jung, for good suggestions!",
   "Put report*1D in QC/, and do*.tcsh and surfaces_* in new surfaces/ dir.\n"
},

{ 16, Oct , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Add in status checks after many afni progs, to exit at/near first failure.",
   "Should be no change in output for users (in successful runs).\n"
},

{ 16, Oct , 2020 , PT , "@animal_warper" , MINOR , TYPE_BUG_FIX,
   "Now, first cp+resample src to RAI; else, shft is bad for non-xyz orients.",
   "Output warps can still apply to original orient dset fine.\n"
},

{ 24, Sep , 2020 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "Put in status checks through script to exit with error if any step fails.",
   "Should provide nicer behavior if something gang agley.\n"
},

{ 2, Sep , 2020 , PT , "@SkullStrip_TouchUp" , MINOR , TYPE_GENERAL,
   "Replace '-e' at top with several later status checks; 'exit 0' after help.",
   "No effect on output, except being more general.\n"
},

{ 1, Sep , 2020 , PT , "@SSwarper" , MINOR , TYPE_BUG_FIX,
   "If '-skipwarp' was used, crashed at very end (sigh).",
   "Fixed that crash behavior; no changes in outputs.\n"
},

{ 1, Sep , 2020 , PT , "fat_roi_row.py" , MINOR , TYPE_ENHANCE,
   "Update to run in Python 3 (using 2to3, plus extra tweaks).",
   "Should now run in both Python 2 and 3.\n"
},

{ 1, Sep , 2020 , PT , "fat_mvm_prep.py" , MINOR , TYPE_ENHANCE,
   "Update to run in Python 3 (using 2to3, plus extra tweaks).",
   "Should now run in both Python 2 and 3.\n"
},

{ 1, Sep , 2020 , PT , "fat_mvm_scripter.py" , MINOR , TYPE_ENHANCE,
   "Update to run in Python 3 (using 2to3, plus extra tweaks).",
   "Should now run in both Python 2 and 3.\n"
},

{ 1, Sep , 2020 , PT , "fat_mvm_gridconv.py" , MINOR , TYPE_ENHANCE,
   "Update to run in Python 3 (using 2to3, plus extra tweaks).",
   "Should now run in both Python 2 and 3.\n"
},

{ 1, Sep , 2020 , PT , "fat_mat_sel.py" , MINOR , TYPE_ENHANCE,
   "Update to run in Python 3 (using 2to3, plus extra tweaks).",
   "Should now run in both Python 2 and 3.\n"
},

{ 1, Sep , 2020 , PT , "fat_lat_csv.py" , MINOR , TYPE_REMOVE,
   "Remove program from distribution, with lib: lib_fat_Rfactor.py.",
   "R deps are a mess between Py2 and Py3; might rewrite better in future.\n"
},

{ 1, Sep , 2020 , PT , "fat_mvm_review.py" , MINOR , TYPE_REMOVE,
   "Remove program from distribution.",
   "This program never even made it to full beta status.\n"
},

{ 1, Sep , 2020 , PT , "fat_proc_grad_plot" , MINOR , TYPE_REMOVE,
   "Remove program from distribution.",
   "Already have a better one (with fewer dependencies!) ready to go.\n"
},

{ 27, Aug , 2020 , PT , "@animal_warper" , MAJOR , TYPE_MODIFY,
   "Well, usage+output shouldn't really change, but it should be more stable.",
   "There is also a new opt: -align_centers_meth (read the help).\n"
},

{ 26, Aug , 2020 , PT , "@animal_warper" , MICRO , TYPE_BUG_FIX,
   "Fix case of running prog with no args.",
   "Should show help; now it DOES show help, with no error.\n"
},

{ 21, Aug , 2020 , PT , "3dTrackID" , MINOR , TYPE_BUG_FIX,
   "Fix header deps of underlying progs (namely, readglob.c).",
   "Was crashing on some NIML reading cases.\n"
},

{ 31, July , 2020 , PT , "@Install_MACAQUE_DEMO" , MAJOR , TYPE_NEW_PROG,
   "Install MACAQUE_DEMO_REST_1.0, for macaque resting state FMRI examples.",
   "Has a '-lite_version' opt for truncated EPI version, smaller download.\n"
},

{ 31, July , 2020 , PT , "fat_mat2d_plot.py" , MINOR , TYPE_BUG_FIX,
   "Fix behavior file path contained dots.",
   "Joining filenames for output now fixed.\n"
},

{ 30, July , 2020 , PT , "@Install_MACAQUE_DEMO" , MAJOR , TYPE_GENERAL,
   "Now install MACAQUE_DEMO_2.1, which should be the new normal.",
   "Script checks for things on install, makes recs, more full demo.\n"
},

{ 30, July , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Make easier to find template in case data has moved around.",
   "Also use wildcard to clean intermed file, in case auto GZIP is on.\n"
},

{ 27, July , 2019 , PT , "3dDWUncert" , MINOR , TYPE_GENERAL,
   "Insert a couple ifdefs around OMP functionality.",
   "This should allow program to compile even without OpenMP.\n"
},

{ 15, July , 2020 , PT , "3dNetCorr" , MINOR , TYPE_GENERAL,
   "Moved header dep of suma_suma.h -> suma_objs.h.",
   "Should be no output change.\n"
},

{ 15, July , 2020 , PT , "3dTORTOISEtoHere" , MINOR , TYPE_GENERAL,
   "Moved header dep of suma_suma.h -> suma_objs.h.",
   "Should be no output change.\n"
},

{ 15, July , 2020 , PT , "3ddot_beta" , MINOR , TYPE_GENERAL,
   "Moved header dep of suma_suma.h -> suma_objs.h.",
   "Should be no output change.\n"
},

{ 15, July , 2020 , PT , "3dTrackID" , MINOR , TYPE_GENERAL,
   "Moved header dep of suma_suma.h -> suma_objs.h.",
   "Should be no output change.\n"
},

{ 15, July , 2020 , PT , "3dEigsToDT" , MINOR , TYPE_GENERAL,
   "Moved header dep of suma_suma.h -> suma_objs.h.",
   "Should be no output change.\n"
},

{ 15, July , 2020 , PT , "3dDTtoNoisyDWI" , MINOR , TYPE_GENERAL,
   "New opt for controlling random seed is available (for testing).",
   "Also, moved dep of suma_suma.h -> suma_objs.h (shd be no output change).\n"
},

{ 15, July , 2020 , PT , "3dVecRGB_to_HSL" , MINOR , TYPE_BUG_FIX,
   "Would whine when outputting BRIK/HEAD dset if -in_scal was used; fixed.",
   "Also, moved dep of suma_suma.h -> suma_objs.h (shd be no output change).\n"
},

{ 1, July , 2020 , PT , "@Install_NMT" , MAJOR , TYPE_NEW_PROG,
   "Installer for the NIMH Macaque Template(s) v2, and the CHARM (atlases).",
   "Courtesy of Ben Jung, Adam Messinger, et al.\n"
},

{ 22, June , 2020 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_BUG_FIX,
   "The -monty opt input was being ignored.",
   "It now has a voice.\n"
},

{ 22, June , 2020 , PT , "convert_cdiflist_to_grads.py" , MINOR , TYPE_BUG_FIX,
   "Output col grads file was *not* scaled by bvalues, as help said it would.",
   "Fixed: now output col grads multiplied by bvalues.\n"
},

{ 17, June , 2020 , PT , "1dplot.py" , MINOR , TYPE_NEW_OPT,
   "Add legend functionality, along with opts for label and loc specifying.",
   "New opts: -legend_on, -legend_labels, -legend_locs.\n"
},

{ 17, June , 2020 , PT , "1dplot.py" , MICRO , TYPE_GENERAL,
   "Add -hview functionality.",
   "Where has this been all my life??\n"
},

{ 14, June , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "For vstat with seedbased corr (rest), use 0.3 as thr value of corr map.",
   "Returning value to what it had been for a long time, based on examples.\n"
},

{ 10, June , 2020 , PT , "convert_cdiflist_to_grads.py" , MAJOR , TYPE_NEW_PROG,
   "For GE scanners, we might want a cdiflist* file for DWI grad info.",
   "This prog converts such beasts into usable grad/bvalue files for proc.\n"
},

{ 9, June , 2020 , PT , "fat_mat2d_plot.py" , MINOR , TYPE_BUG_FIX,
   "Fix behavior when -xticks_off and/or -yticks_off are/is used.",
   "Now the specified axis will really be *empty*.\n"
},

{ 4, June , 2020 , PT , "fat_mat2d_plot.py" , MINOR , TYPE_GENERAL,
   "Improve couple things in help file; change def cbar.",
   "More useful 'divergent' class of cbar as default.\n"
},

{ 3, June , 2020 , PT , "epi_b0_correct.py" , MICRO , TYPE_BUG_FIX,
   "Programming badness if user forgot to add a nec arg to an opt.",
   "There should be no change in behavior when correct opts are added.\n"
},

{ 3, June , 2020 , PT , "fat_mat2d_plot.py" , MAJOR , TYPE_NEW_PROG,
   "FINALLY, a python3 program to plot 3dTrackID and 3dNetCorr output.",
   "Plots *.grid and *.netcc files; replaces fat_mat_sel.py.\n"
},

{ 3, June , 2020 , PT , "lib_mat2d_plot.py" , MAJOR , TYPE_GENERAL,
   "Many updates to functioning, defaults, reading argv, applying user opts.",
   "Help file added as well; works with main proc: fat_mat2d_plot.py.\n"
},

{ 1, June , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "For vstat with seedbased corr (rest), use 0.2 as thr value of corr map.",
   "The value 0.3 seemed pretty high (esp. if no smoothing is applied).\n"
},

{ 1, June , 2020 , PT , "lib_mat2d_base.py" , MINOR , TYPE_GENERAL,
   "Migrated from lib_mat2d.py; tweaks added.",
   "Add in few more mat2d attributes; rearrange methods.\n"
},

{ 1, June, 2020 , PT , "adjunct_aw_tableize_roi_info.py" , MINOR , TYPE_GENERAL,
   "Reformat report*.1D tables a bit.",
   "Add in a KEY; change U/W to A/B; minor format stuff.\n"
},

{ 1, June , 2020 , PT , "lib_mat2d.py" , MINOR , TYPE_GENERAL,
   "Start some new functionality for 2D matrices.",
   "In particular, these are for 3dTrackID and 3dNetCorr output.\n"
},

{ 1, June , 2020 , PT , "afni_base.py" , MINOR , TYPE_GENERAL,
   "Add new funcs for convenient message printing, in the AFNI style.",
   "IP(), EP() and WP(), which are wrappers to use APRINT().\n"
},

{ 31, May , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Change range of grayscale when EPI is ulay (ve2a and LR flipcheck).",
   "Now 2-98percent (nonzero).\n"
},

{ 31, May , 2020 , PT , "@animal_warper" , MINOR , TYPE_BUG_FIX,
   "Two bug fixes: 1) where src_prefix is defined.",
   "2) Make sure labels/atlases of ATL|SEG followers are passed along.\n"
},

{ 30, May , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Apply input_abbrev earlier in processing.",
   "Homogenize naming, I think, if it is being selected.\n"
},

{ 30, May , 2020 , PT , "@animal_warper" , MAJOR , TYPE_GENERAL,
   "Default modal smoothing now is with replacement of any lost ROIs.",
   "Uses @djunct_modal* script; opt to not replace. More QC images now, too.\n"
},

{ 30, May, 2020, PT , "@djunct_modal_smoothing_with_rep" , MINOR, TYPE_GENERAL,
   "Now use *.nii.gz files for all intermeds, not *.nii.",
   "Works better with @animal_warper this way.\n"
},

{ 30, May, 2020, PT , "@djunct_modal_smoothing_with_rep" , MAJOR, TYPE_NEW_PROG,
   "Perform modal smoothing, and go back and add in any ROIs that were lost.",
   "May be useful in @animal_warper;  may be good to add mask stuff, too.\n"
},

{ 30, May, 2020 , PT , "adjunct_aw_tableize_roi_info.py" , MINOR , TYPE_GENERAL,
   "String selector of lost ROIs now is only comma-separated list.",
   "Discovered couldn't have both comma- and '..'-separated list in selector.\n"
},

{ 28, May , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Now report DF information in vstat block.",
   "Needed to be able to interpret F-stat and t-stat values.\n"
},

{ 26, May , 2020 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Two major changes in output: ve2a and LR-flipcheck now have EPI as ulay.",
   "Most anats are SSed, so better edges?  Thanks for suggestion, O Esteban!\n"
},

{ 26, May , 2020 , PT , "@djunct_edgy_align_check" , MAJOR , TYPE_GENERAL,
   "Several changes to make this appropriate using EPI as ulay.",
   "New opts, couple small bug fixes, couple tweaks.\n"
},

{ 26, May, 2020 , PT , "adjunct_aw_tableize_roi_info.py" , MINOR , TYPE_GENERAL,
   "Now output an AFNI-style string selector of 'lost' ROI values.",
   "This might make it easier to see the diffs the volumes.\n"
},

{ 22, May , 2020 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_GENERAL,
   "Change this prog to do all work in a workdir that can be cleaned.",
   "Should not have any effect on the usage or outputs.\n"
},

{ 21, May , 2020 , PT , "afni_seeds_per_space.txt" , MAJOR , TYPE_GENERAL,
   "Keep up with change of macaque standard space naming: stereoNMT -> NMT2.",
   "'stereoNMT' is an ex-parrot.\n"
},

{ 21, May, 2020 , PT , "adjunct_aw_tableize_roi_info.py" , MINOR , TYPE_GENERAL,
   "Require mode_smooth_size as input, and include it in table.",
   "Thanks to D Glen and A Messinger for helpful feedback+inputs.\n"
},

{ 21, May , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Report now reports mode_smooth_size.",
   "Thanks to D Glen and A Messinger for helpful feedback+inputs.\n"
},

{ 21, May, 2020, PT , "adjunct_aw_tableize_roi_info.py" , MAJOR , TYPE_NEW_PROG,
   "Adjunct program for @animal_warper.py; build ROI report table.",
   "Thanks to D Glen and A Messinger for helpful feedback+inputs.\n"
},

{ 21, May , 2020 , PT , "@animal_warper" , MAJOR , TYPE_GENERAL,
   "Add reports of warped and unwarped ROIs, via adjunct_aw_tableize*.py.",
   "Thanks to D Glen and A Messinger for helpful feedback+inputs.\n"
},

{ 18, May , 2020 , PT , "afni_seeds_per_space.txt" , MINOR , TYPE_GENERAL,
   "Updated APQC seed locations for stereoNMT space.",
   "More centralized now in GM and in specific ROIs; aud away from vessel.\n"
},

{ 18, May , 2020 , PT , "@animal_warper" , MAJOR , TYPE_GENERAL,
   "Large number of under-the-hood changes, as well as new opts.",
   "More general handling of followers and choosing file abbrevs.\n"
},

{ 14, May , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Large number of under-the-hood changes, for readability/clarity.",
   "Change echo->printf, spacing, clear comments, etc. No output changes.\n"
},

{ 4, May , 2020 , PT , "@Install_IBT_DATASETS" , MINOR , TYPE_NEW_PROG,
   "Installer for the Indian Brain Templates. Enjoy.",
   "Courtesy of Dr. Bharath Holla, et al.\n"
},

{ 27, Apr , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Added a help example for integrating output into afni_proc.py.",
   "... because otherwise *I* forget how to use the outputs.\n"
},

{ 24, Apr , 2020 , PT , "3dLMEr" , MINOR , TYPE_GENERAL,
   "Updating this R file for GC. So I don't really know what the changes do.",
   "... though I reeeallly want to pretend the changes were mine, ALL MINE.\n"
},

{ 24, Apr , 2020 , PT , "3dClusterize" , MINOR , TYPE_GENERAL,
   "Sidedness of testing will no longer be checked for non-stat thr vols.",
   "It must be Daniel Glen's birthday today (two-sided, non-stat p<0.9999).\n"
},

{ 23, Apr , 2020 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Added new help example.",
   "Demonstrates useful colorbar-entry functionality.\n"
},

{ 23, Apr , 2020 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Use '-colorscale_idx_file ..' to control AFNI env var AFNI_COLORSCALE_xx.",
   "Provides a way for user-created cbar info to be input+used.\n"
},

{ 22, Apr , 2020 , PT , "1dplot.py" , MICRO , TYPE_BUG_FIX,
   "The '-xvals ..' opt was broken, but now is fixed.",
   "*Now* the brain can be solved.\n"
},

{ 16, Apr , 2020 , PT , "adjunct_simplify_cost.py" , MICRO , TYPE_NEW_PROG,
   "Adjunct program for (soon to be updated) @SSwarper.",
   "Convert cost name to simpler version, for some application(s).\n"
},

{ 16, Apr , 2020 , PT , "@djunct_ssw_intermed_edge_imgs" , MICRO , TYPE_NEW_PROG,
   "Adjunct program for (soon to be updated) @SSwarper.",
   "Generates images for intermediate QC/tracking.\n"
},

{ 27, Mar , 2020 , PT , "apqc_make_html.py" , MICRO , TYPE_GENERAL,
   "Rearrange variable/function definitions in afnipy libs (no more interdep).",
   "All changes just 'under the hood'---should be no output differences.\n"
},

{ 27, Mar , 2020 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "Rearrange variable/function definitions in afnipy libs (no more interdep).",
   "All changes just 'under the hood'---should be no output differences.\n"
},

{ 12, Mar , 2020 , PT , "3dReHo" , MINOR , TYPE_GENERAL,
   "Alter output format if ROI neighborhood values of ReHo are calc'ed.",
   "Make 2 col (ROI val; ReHo val). Output multiple text files, if nec, too.\n"
},

{ 12, Mar , 2020 , PT , "check_dset_for_fs.py" , MAJOR , TYPE_GENERAL,
   "This program has been deemed unnecessary.",
   "Thanks for the FS folks for discussions/clarifications on this.\n"
},

{ 12, Mar , 2020 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_BUG_FIX,
   "vstat image was generated even if 'surf' block was used in AP.",
   "Since stats_dset in this case was *.niml.dset, no image should be made.\n"
},

{ 11, Mar , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Swap ulay/olay in va2t (anat->ulay); clearer image, maybe.",
   "Make template (if used) ulay in most other QC blocks.\n"
},

{ 27, Feb , 2020 , PT , "@SSwarper" , MINOR , TYPE_NEW_OPT,
   "New opt '-warpscale' added; is a new opt in 3dQwarp, can be tweaked here now.",
   "Control flexibility of warps.  Right now testing different values.\n"
},

{ 26, Feb , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Crashing in cases of 'pythonic' APQC with no censoring.",
   "Have fixed now.\n"
},

{ 25, Feb , 2020 , PT , "check_dset_for_fs.py" , MINOR , TYPE_BUG_FIX,
   "Minor bug fix, caught by RCR.  Or was it first *added* by RCR????",
   "... Nope.  It was added by me.  Sigh.\n"
},

{ 25, Feb , 2020 , PT , "check_dset_for_fs.py" , MAJOR , TYPE_GENERAL,
   "New min|max range on vox size; update report text.",
   "Based on tests with FS data.\n"
},

{ 24, Feb , 2020 , PT , "adjunct_make_script_and_rst.py" , MICRO , TYPE_BUG_FIX,
   "Fix image caption processing.",
   "(This prog is just used in RST/documentation generation.)\n"
},

{ 22, Feb , 2020 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_GENERAL,
   "Temporary files now have a random string in their prefix.",
   "Thus, can have multiple runs in same directory simultaneously sans probleme.\n"
},

{ 22, Feb , 2020 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Can control AGIF frame rate, using opt (-agif_delay) to control AFNI env var.",
   "Thanks to RCR for pointing out the env var.\n"
},

{ 21, Feb , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Crashing in cases of 'basic' APQC with no outlier-based censoring.",
   "Have fixed now.\n"
},

{ 19, Feb , 2020 , PT , "DoPerRoi.py" , MINOR , TYPE_GENERAL,
   "Renamed from @DoPerRoi.py.",
   "Purge @ symbol in Python progs, for purpose of repackaging/distribution.\n"
},

{ 19, Feb , 2020 , PT , "adjunct_select_str.py" , MINOR , TYPE_GENERAL,
   "Renamed from @djunct_select_str.py.",
   "Purge @ symbol in Python progs, for purpose of repackaging/distribution.\n"
},

{ 19, Feb , 2020 , PT , "adjunct_make_script_and_rst.py" , MINOR , TYPE_GENERAL,
   "Renamed from @djunct_make_script_and_rst.py.",
   "Purge @ symbol in Python progs, for purpose of repackaging/distribution.\n"
},

{ 19, Feb , 2020 , PT , "adjunct_is_label.py" , MINOR , TYPE_GENERAL,
   "Renamed from @djunct_is_label.py.",
   "Purge @ symbol in Python progs, for purpose of repackaging/distribution.\n"
},

{ 19, Feb , 2020 , PT , "adjunct_combine_str.py" , MINOR , TYPE_GENERAL,
   "Renamed from @djunct_combine_str.py.",
   "Purge @ symbol in Python progs, for purpose of repackaging/distribution.\n"
},

{ 19, Feb , 2020 , PT , "adjunct_calc_mont_dims.py" , MINOR , TYPE_GENERAL,
   "Renamed from @djunct_calc_mont_dims.py.",
   "Purge @ symbol in Python progs, for purpose of repackaging/distribution.\n"
},

{ 19, Feb , 2020 , PT , "@SSwarper" , MINOR , TYPE_GENERAL,
   "New QC image outputs added.",
   "One for skullstripping (orig space) and one for warping (ref space).\n"
},

{ 17, Feb , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Reorganized under the hood, deal with censoring in a better way.",
   "Easier to tweak/update changes now.\n"
},

{ 17, Feb , 2020 , PT , "1dplot.py" , MINOR , TYPE_GENERAL,
   "Opt -censor_hline can now take a keyword NONE as an entry.",
   "Useful if looking at multiple inputs and only some have censor lines.\n"
},

{ 3, Feb , 2020 , PT , "@afni_refacer_run" , MINOR , TYPE_NEW_OPT,
   "Can anonymize output dsets:  -anonymize_output.",
   "Fairly self-explanatory opt.\n"
},

{ 3, Feb , 2020 , PT , "@djunct_anonymize" , MINOR , TYPE_NEW_PROG,
   "Anonymize files, and maybe add a note.",
   "Can either edit input directly, or make a copy + edit that.\n"
},

{ 29, Jan , 2020 , PT , "1dplot.py" , MINOR , TYPE_BUG_FIX,
   "The input opt '-xfile ..' was broken;  now it is fixed.",
   "Fix class inits, as well, under the hood.\n"
},

{ 29, Jan , 2020 , PT , "@djunct_montage_coordinator" , MINOR , TYPE_BUG_FIX,
   "Couldn't deal with volumes that had RGB type, which happens for DEC maps.",
   "Has been fixed now.\n"
},

{ 29, Jan , 2020 , PT , "fat_proc_dwi_to_dt" , MICRO , TYPE_GENERAL,
   "Try to make a couple output images (dwi*b0*.png) a bit clearer.",
   "Make olay use 95%ile value as cbar max, rather than 100%.\n"
},

{ 27, Jan , 2020 , PT , "@SSwarper" , MAJOR , TYPE_GENERAL,
   "Large set of updates; many new opts added, too; generally much improved warps.",
   "Heavily tested on 178 subj across studies; output fnames are same, though.\n"
},

{ 27, Jan , 2020 , PT , "@afni_refacer_run" , MAJOR , TYPE_GENERAL,
   "Program now outputs QC images automatically.",
   "These are output into a PREFIX_QC directory each run.\n"
},

{ 27, Jan , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Fixed bug in QC.",
   "Broke when there was one stim used (bad scalar -> list conv).\n"
},

{ 26, Jan , 2020 , PT , "@afni_refacer_run" , MAJOR , TYPE_GENERAL,
   "This program now requires specifying a mode for re/defacing.",
   "One can also output all types of re/defacing simultaneously.\n"
},

{ 24, Jan , 2020 , PT , "@afni_refacer_make_master" , MICRO , TYPE_GENERAL,
   "Updated with notes to look at @afni_refacer_make_master_addendum.",
   "The addendum program just tweaks the output of this prog.\n"
},

{ 24, Jan , 2020 , PT , "@afni_refacer_make_master_addendum" , MAJOR , TYPE_GENERAL,
   "This program just records additional tweaks to refacer ref vol.",
   "Not really meant to be run on its own; for future reference only.\n"
},

{ 24, Jan , 2020 , PT , "@afni_refacer_run" , MAJOR , TYPE_GENERAL,
   "This program has been revamped and updated, including having a new ref vol.",
   "Syntax for running this has totally changed (options exist).\n"
},

{ 21, Jan , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Increase thresholds and cbar range in vstat_seedcorr and regr_corr dsets.",
   "Clearer QC, methinks, based on several different group dsets.\n"
},

{ 14, Jan , 2020 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Change text of animal_outs.txt.",
   "Add in a couple new dsets to be listed.\n"
},

{ 13, Jan , 2020 , PT , "afni_util.py" , MINOR , TYPE_NEW_OPT,
   "New function to read in seed list text file.",
   "Returns list of seed objs for APQC.\n"
},

{ 17, Jan , 2020 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "New pieces of QC:  corr brain image in regr block.",
   "Shows corr of mean residual with everything.  Have a nice day.\n"
},

{ 13, Jan , 2020 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "New pieces of QC:  first, seedbased corr maps for non-task data.",
   "Second, censor-based warnings (general and per-stim).\n"
},

{ 13, Jan , 2020 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_NEW_OPT,
   "Couple new options.",
   "Can specify colorbar and center coords now.\n"
},

{ 27, Dec , 2019 , PT , "check_dset_for_fs.py" , MAJOR , TYPE_NEW_OPT,
   "New option(s) to not just *check* a dset for FS-ability, but to correct it.",
   "The '-fix_all' and accompanying options control this.  Bonne idee, DRG!\n"
},

{ 26, Dec , 2019 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "Simpler list of uvar dependencies for indiv stim plotting.",
   "... ergo, see indiv stims even if not censoring.\n"
},

{ 26, Dec , 2019 , PT , "check_dset_for_fs.py" , MINOR , TYPE_BUG_FIX,
   "Fix one of the test criteria (-is_mat_even).",
   "Thanks, S. Torrisi, for pointing this out.\n"
},

{ 14, Nov , 2019 , PT , "@SUMA_renumber_FS" , MINOR , TYPE_GENERAL,
   "New output: fs_ap* dsets for tissue-based reg in afni_proc.py;.",
   "New output: *REN_gmrois* dsets for tracking/corr mats in FATCAT.\n"
},

{ 14, Nov , 2019 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT,
   "New opt:  '-make_rank_dsets ..', bc *rank* dsets no longer make by def.",
   "The *REN* dsets should be used instead; opt just for back compatability.\n"
},

{ 23, Oct , 2019 , PT , "check_dset_for_fs.py" , MAJOR , TYPE_NEW_PROG,
   "Script to check properties of a dset, see if suitable for FS's recon-all.",
   "The check criteria have been built over time, empirically.\n"
},

{ 22, Oct , 2019 , PT , "@djunct_edgy_align_check" , MICRO , TYPE_GENERAL,
   "Adjusted help file.",
   "Had given wrong name for opt.\n"
},

{ 21, Oct , 2019 , PT , "afni" , MICRO , TYPE_NEW_OPT,
   "Display AFNI environment vars in the terminal, via new opt: -env.",
   "Makes good bedtime reading.\n"
},

{ 21, Oct , 2019 , PT , "afni" , MICRO , TYPE_BUG_FIX,
   "Had named an option differently than help file stated; now renaming.",
   "To show 'AFNI Tips', opt named: -tips.\n"
},

{ 17, Oct , 2019 , PT , "afni" , MICRO , TYPE_NEW_OPT,
   "Display AFNI Tips in the terminal, via new opt: -show_tips.",
   "Will be used+parsed for the HTML RST docs.\n"
},

{ 7, Oct , 2019 , PT , "afni" , MICRO , TYPE_NEW_OPT,
   "Simpler opts for package and version number.",
   "For scriptability.\n"
},

{ 7, Oct , 2019 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Change text of animal_outs.txt.",
   "Minor 'under the hood' changes, too.\n"
},

{ 3, Oct , 2019 , PT , "lib_gershgorin.py" , MINOR , TYPE_GENERAL,
   "Just divvied up the behavior of the functions better.",
   "Also have a general, NxN case .\n"
},

{ 3, Oct , 2019 , PT , "afni_util.py" , MICRO , TYPE_NEW_OPT,
   "Fancy new function to calculate if a list-matrix is square.",
   "ps: not that fancy.\n"
},

{ 3, Oct , 2019 , PT , "epi_b0_correct.py" , MICRO , TYPE_GENERAL,
   "Calculate oblique transform differently; use 3dWarp instead of cat_matvec.",
   "Probably negligible practical change.\n"
},

{ 2, Oct , 2019 , PT , "epi_b0_correct.py" , MAJOR , TYPE_GENERAL,
   "The naming convention of PE dist dir has been reversed; mask opts changed.",
   "PE dist dir should match with JSONs better; 3dmask_tool does masking now.\n"
},

{ 2, Oct , 2019 , PT , "1dDW_Grad_o_Mat++" , MINOR , TYPE_GENERAL,
   "Output more specific information about finding unexpected negative values.",
   "Tell user the [row, col] of potentially bad values, for easier QC.\n"
},

{ 13, Sep , 2019 , PT , "3dWarp" , MINOR , TYPE_NEW_OPT,
   "New opt: -disp_obl_xform_only.",
   "Better way to get transform between obl coords than cat_matvec trickery.\n"
},

{ 12, Sep , 2019 , PT , "epi_b0_correct.py" , MAJOR , TYPE_GENERAL,
   "Output QC directory of images now, as well. Useful for quick QC.",
   "Later, will add some checks for obl, to not smooth unnec.\n"
},

{ 12, Sep , 2019 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "Use 'mkdir -p' with odir now.",
   "Simplifies scripts using it.\n"
},

{ 10, Sep , 2019 , PT , "epi_b0_correct.py" , MICRO , TYPE_GENERAL,
   "Fix help descriptions (thanks L. Dowdle for fixes).",
   "Also add '-hview' capability.\n"
},

{ 10, Sep , 2019 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Unifize output in standard space.",
   "Better for visualization in afni_proc.py QC.\n"
},

{ 6, Sep , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Two minor changes: montages now separated by 1 gap line.",
   "... and show censor bars in VR6 plots, if censoring.\n"
},

{ 6, Sep , 2019 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_GENERAL,
   "Now use montgap=1 by default.",
   "This is for APQC applications, where subj data fills FOV.\n"
},

{ 6, Sep , 2019 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Output skullstripped version of template in std space.",
   "Also add 'notes' to that file, so gen_ss*script* can find template.\n"
},

{ 6, Sep , 2019 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "Put QC images into subdir called QC; output mask.",
   "Few other tiny changes/reorganizations internally.\n"
},

{ 4, Sep , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opts: -obl_resam_ulay OIU, -obl_resam_Olay OIO, -obl_resam_box OIB.",
   "Control resampling of dsets (ulay, olay, focus box) when applying obl.\n"
},

{ 4, Sep , 2019 , PT , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT,
   "New opt:  '-extra_fs_dsets ..', to translate more FS-created surf/ dsets.",
   "Allow more FS surf dsets to be brought into SUMA. For F. Lalonde.\n"
},

{ 3, Sep , 2019 , PT , "@animal_warper" , MINOR , TYPE_GENERAL,
   "New QC imaging with @chauffeur_afni; mask created, too.",
   "And a few minor changes under the hood, worked out with DRG.\n"
},

{ 30, Aug , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opts: -edge_enhance_ulay EE, -obliquify OBL.",
   "Different way to enhance edges of ulay, and apply obliquity info.\n"
},

{ 30, Aug , 2019 , PT , "epi_b0_correct.py" , MINOR , TYPE_BUG_FIX,
   "Fix input opt to change blur size; was broken before, crashing prog.",
   "Thanks, L. Dowdle for finding!\n"
},

{ 29, Aug , 2019 , PT , "@auto_tlrc" , MAJOR , TYPE_GENERAL,
   "RE-introducing the program '@auto_tlrc' to the distribution.",
   "It had been mistakenly deleted somehow.\n"
},

{ 29, Aug , 2019 , PT , "lib_gershgorin.py" , MICRO , TYPE_GENERAL,
   "Updated to change way aff12.1D files were read.",
   "No change in calculated outputs.\n"
},

{ 29, Aug , 2019 , PT , "afni_util.py" , MICRO , TYPE_GENERAL,
   "Remove function: read_aff12_to_mat34().",
   "'Twas unnecessary.\n"
},

{ 28, Aug , 2019 , PT , "lib_gershgorin.py" , MINOR , TYPE_NEW_PROG,
   "Funcs to answer question: is this aff12 matrix very different from I?",
   "Uses fun algebraic facts known to and shared by the inimitable RWC.\n"
},

{ 28, Aug , 2019 , PT , "afni_util.py" , MINOR , TYPE_NEW_OPT,
   "Matrix-y things: read_aff12_to_mat34(), matrix_multiply_2D().",
   "And supplements: matrix_sum_abs_val_ele_row(), calc_zero_dtype().\n"
},

{ 27, Aug , 2019 , PT , "epi_b0_correct.py" , MINOR , TYPE_GENERAL,
   "Added more fields to the output param text file.",
   "Also added to the help file (including *about* the params text file).\n"
},

{ 27, Aug , 2019 , PT , "3dSpaceTimeCorr" , MINOR , TYPE_NEW_OPT,
   "New opts: '-freeze* ..' that allow one to fix a location in dset A.",
   "Input for Zhihao Li.\n"
},

{ 26, Aug , 2019 , PT , "@chauffeur_afni" , MICRO , TYPE_NEW_OPT,
   "New opt, '-ulay_comm': provide comment on ulay vals in pbar json.",
   "Also, saving ulay min/max in pbar json is new behavior.\n"
},

{ 23, Aug , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "New opt, '-edgy_ulay': can turn ulay into edge-ified version of itself.",
   "Useful for showing alignments.\n"
},

{ 23, Aug , 2019 , PT , "epi_b0_correct.py" , MAJOR , TYPE_BUG_FIX,
   "Fixed calculation when PE effective echo spacing is input.",
   "The conversion to BWPP was wrong; led to almost no distortion corr.\n"
},

{ 20, Aug , 2019 , PT , "@SUMA_Make_Spec_FS" , MICRO , TYPE_GENERAL,
   "Indent properly-- loops/conditions were too hard to follow.",
   "Should have no change in output but facilitates code editing.\n"
},

{ 15, Aug , 2019 , PT , "epi_b0_correct.py" , MAJOR , TYPE_GENERAL,
   "This program has been pretty fully revamped, and might be worth using now.",
   "New scaling from Vinai, several updates/fixes/changes from last ver.\n"
},

{ 1, Aug , 2019 , PT , "epi_b0_correct.py" , MINOR , TYPE_GENERAL,
   "Rename internal vars and opt names.",
   "Improving internal notation-- still very much a beta program version.\n"
},

{ 16, July , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Fix incompatability with py2.",
   "Sigh.\n"
},

{ 25, July , 2019 , PT , "epi_b0_correct.py" , MAJOR , TYPE_NEW_PROG,
   "Program to apply freq volume to EPI for B0 distortion correction.",
   "An honor to translate this program from one by Vinai Roopchansingh!\n"
},

{ 23, July , 2019 , PT , "1dplot.py" , MICRO , TYPE_GENERAL,
   "Allow PDFs to be output directly.",
   "User just needs '.pdf' file extension on prefix.\n"
},

{ 18, July , 2019 , PT , "@djunct_make_script_and_rst.py", MICRO , TYPE_BUG_FIX,
   "Used to crash if output dir was PWD.",
   "Now fixed.\n"
},

{ 18, July , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Hadn't merged in updated library functions, so apqc_make_tcsh.py crashed.",
   "Updated library file in distribution now.\n"
},

{ 17, July , 2019 , PT , "apqc_make_html.py" , MICRO , TYPE_GENERAL,
   "Minorest of changes to closing message.",
   "No more double slash.  Wow.\n"
},

{ 15, July , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Add in obliquity values in vorig QC block.",
   "Also simplify text of radcorr block (fewer lines, less unnec repetition).\n"
},

{ 10, July , 2019 , PT , "@djunct_make_script_and_rst.py", MINOR , TYPE_GENERAL,
   "Can have text in the image tables now.",
   "Facilitates labelling, commenting, etc.\n"
},

{ 9, July , 2019 , PT , "@djunct_make_script_and_rst.py" , MINOR , TYPE_GENERAL,
   "Allow for multiple scripts to be executed, run and combined into 1 page.",
   "Single script tarball, single RST, can have multiple scripts/reflinks.\n"
},

{ 8, July , 2019 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "New default: '-do_clean' behavior on by default (clean up temp dir).",
   "New opt to not clean: -no_clean. -do_clean is fine to use, just boring.\n"
},

{ 8, July , 2019 , PT , "@djunct_make_script_and_rst.py" , MINOR , TYPE_GENERAL,
   "Allow wildcard chars in IMAGE descrip; SUBSECTIONS added.",
   "Minor tweaks for formatting help files.\n"
},

{ 3, July , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Expanded vstat QC block capabilities.",
   "Other tweaks, QC block IDs now in titles.\n"
},

{ 1, July , 2019 , PT , "dsetstat2p" , MICRO , TYPE_NEW_PROG,
   "Complement of p2dsetstat.",
   "Convenience tool for converting a dset's stat to a p-value.\n"
},

{ 1, July , 2019 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Labels on stim regressors, vorig QC block added, grayplot pbvorder/enorm.",
   "Help updated; output stats still if not align/tlrc blocks used.\n"
},

{ 1, July , 2019 , PT , "apqc_make_html.py" , MINOR , TYPE_GENERAL,
   "Some minor tweaks to APQC HTML: better pbar size control, spacing.",
   "Also can interpret pbar text more broadly.\n"
},

{ 1, July , 2019 , PT , "@djunct_glue_imgs_vert" , MICRO , TYPE_NEW_PROG,
   "New prog for APQC HTML stuff.  Glue two images together vertically.",
   "Used when pixel x-dimensions match (mainly for APQC HTML).\n"
},

{ 1, July , 2019 , PT , "@chauffeur_afni" , MICRO , TYPE_NEW_OPT,
   "New option '-pbar_for ..', which is mainly for APQC HTML.",
   "Can add a dict entry to txt file accompanying pbar output.\n"
},

{ 26, June , 2019 , PT , "3dNetCorr" , MINOR , TYPE_NEW_OPT,
   "New opt '-weight_ts WTS' to multiply ROI ave time series.",
   "Input at the behest of Colm C.  May it pour forth wondrous results.\n"
},

{ 25, June , 2019 , PT , "3dSkullStrip" , MAJOR , TYPE_MODIFY,
   "Dset orient should no longer affect results (b/c of var of init cond).",
   "Intermediate resampling now reduces/removes var due to start.\n"
},

{ 20, June , 2019 , PT , "@djunct_make_script_and_rst.py" , MICRO , TYPE_BUG_FIX,
   "Use the CAPTION feature on image tables in text blocks.",
   "Also fix help display.\n"
},

{ 19, June , 2019 , PT , "@djunct_make_script_and_rst.py" , MICRO , TYPE_NEW_PROG,
   "New prog for Sphinx doc generation (well, assistance).",
   "Somewhat simple markup scheme used to generate RST, images and scripts.\n"
},

{ 5, June , 2019 , PT , "3dTrackID" , MICRO , TYPE_NEW_OPT,
   "New opt (flag): -trk_opp_orient.  Applies only to TRK format output.",
   "Will oppositize the voxel_order for the TRK file.\n"
},

{ 23, May , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Would not run in python2, because of subprocess.run() call (only py3).",
   "Now updated to using afni_base.py functions to execute shell cmds.\n"
},

{ 22, May , 2019 , PT , "apqc_make_html.py" , MINOR , TYPE_GENERAL,
   "Improved help file (lists blocks, line to online help).",
   "Better formatting of a couple things; warn level coloring added.\n"
},

{ 22, May , 2019 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Somewhat big changes: warns block updated and radcor block added.",
   "Left-right flip and @radial_correlate checks now in; other tweaks.\n"
},

{ 22, May , 2019 , PT , "@djunct_json_value.py" , MINOR , TYPE_NEW_PROG,
   "Tiny program to extract values from JSONs.",
   "Just used by apqc_make_tcsh.py.\n"
},

{ 14, May , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_GENERAL,
   "Change some fields in pbar json, for greater utility.",
   "Also make new default ftype for output cbar (jpg).\n"
},

{ 14, May , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "More pbar control: put in afni's '-XXXnpane P' behavior.",
   "Same option name used in this prog.\n"
},

{ 13, May , 2019 , PT , "@DriveSuma" , MICRO , TYPE_GENERAL,
   "Some help output has non-UTF8 chars in it;  default help now *won't*.",
   "'MICRO' might be too strong a designation for this change...\n"
},

{ 13, May , 2019 , PT , "3dRprogDemo.R" , MICRO , TYPE_GENERAL,
   "Some help output has non-UTF8 chars in it;  default help now *won't*.",
   "'MICRO' might be too strong a designation for this change...\n"
},

{ 10, May , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Allow for comments about pbar ranges to be stored when saving pbar.",
   "Also, the pbar text info will now be stored in dict/JSON-able form.\n"
},

{ 1, May , 2019 , PT , "@djunct_is_label.py" , MICRO , TYPE_GENERAL,
   "Tiny program to see if input is an integer (-> index) or str (-> label).",
   "Just used by @chauffeur_afni for -set_subbricks reading.\n"
},

{ 1, May , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Allow -set_subbricks to take string labels for subbricks as usable args.",
   "Excellent idea, Rasmus!\n"
},

{ 19, Apr , 2019 , PT , "@Spharm.examples" , MINOR , TYPE_GENERAL,
   "Just updated paths/names: tarball getting used no longer exists.",
   "No change in functionality (j'espere).\n"
},

{ 18, Apr , 2019 , PT , "@SSwarper" , MAJOR , TYPE_NEW_OPT,
   "Include -deoblique and -giant_move opts.",
   "For oblique data, and/or heavily rotated, shifted, etc.\n"
},

{ 15, Mar , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_BUG_FIX,
   "Better behavioring of -box_focux_slices when ulay and refbox grids differ.",
   "Now checking grid similarity and resampling refbox if needbe.\n"
},

{ 6, Mar , 2019 , PT , "3dDWUncert" , MICRO , TYPE_GENERAL,
   "Change \% to %% in printf() function. No change to functionality.",
   "Amazingly spotted in stream of build messages by RWC.\n"
},

{ 27, Feb , 2019 , PT , "1dplot.py" , MINOR , TYPE_GENERAL,
   "Put a try/except at start, to set MPLBACKEND env if running w/o DISPLAY.",
   "Useful for current settings on Biowulf (and possibly elsewhere).\n"
},

{ 27, Feb , 2019 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Include grayplots in the APQC HTML file.",
   "Should add some extra info about residuals/modeling/the meaning of life.\n"
},

{ 21, Feb , 2019 , PT , "@SSwarper" , MICRO , TYPE_GENERAL,
   "Include '-Urad 30' in 3dUnifize step.",
   "Maybe slightly prettier/more unifized output.\n"
},

{ 19, Feb , 2019 , PT , "apqc_make_html.py" , SUPERDUPER , TYPE_GENERAL,
   "Much functionality changed/improved (hopefully).",
   "More output, better formats, help and HTML framework.\n"
},

{ 19, Feb , 2019 , PT , "apqc_make_tcsh.py" , SUPERDUPER , TYPE_GENERAL,
   "Much functionality changed/improved (hopefully).",
   "More output, better formats, help and HTML framework.\n"
},

{ 19, Feb , 2019 , PT , "1dplot.py" , MINOR , TYPE_GENERAL,
   "Line thickness of plots now adjusts with number of points.",
   "Useful as the number of time points increases (hopefully).\n"
},

{ 13, Feb , 2019 , PT , "@SSwarper" , MICRO , TYPE_NEW_OPT,
   "Renaming the non-pre-skullstripping option to -init_skullstr_off.",
   "Otherwise, might falsely seem like NO skullstripping would be done.\n"
},

{ 12, Feb , 2019 , PT , "@djunct_4d_slices_to_3d_vol" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 12, Feb , 2019 , PT , "@djunct_4d_imager" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 12, Feb , 2019 , PT , "@GradFlipTest" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 12, Feb , 2019 , PT , "@xyz_to_ijk" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 12, Feb , 2019 , PT , "p2dsetstat" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 12, Feb , 2019 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 12, Feb , 2019 , PT , "fat_procs" , MICRO , TYPE_GENERAL,
   "Change under the hood: new way to check for validity of input dset.",
   "Should be boring an have no effect on output; just more stable check.\n"
},

{ 11, Feb , 2019 , PT , "@SSwarper" , MINOR , TYPE_NEW_OPT,
   "... and can also turn off initial skullstripping and/or anisosmoothing.",
   "Options cleverly named: -skullstrip_off and -aniso_off.\n"
},

{ 11, Feb , 2019 , PT , "@SSwarper" , MINOR , TYPE_NEW_OPT,
   "Can turn off initial unifizing with -unifize_off.",
   "Useful if unifizing has been done to dset before using this cmd.\n"
},

{ 5, Feb , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_BUG_FIX,
   "Had been missing an endif.",
   "Now new and improved-- with endif!\n"
},

{ 30, Jan , 2019 , PT , "@djunct_edgy_align_check" , MINOR , TYPE_NEW_OPT,
   "Added '-montgap' and '-montcolor', for montage functionality.",
   "Users can now control montage borders (i.e., gaps) and color.\n"
},

{ 30, Jan , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Added '-montgap' and '-montcolor', for montage functionality.",
   "Users can now control montage borders (i.e., gaps) and color.\n"
},

{ 28, Jan , 2019 , PT , "@chauffeur_afni" , MINOR , TYPE_NEW_OPT,
   "Well, new functionality to existing opt: make focus box from ulay or olay.",
   "Keywords AMASK_FOCUS{O,U}LAY can be given to -box_focus_slices.\n"
},

{ 25, Jan , 2019 , PT , "@djunct_montage_coordinator" , MINOR , TYPE_BUG_FIX,
   "Couldn't deal with volumes that had subbrick selectors from @chauffeur*.",
   "Has been fixed now.\n"
},

{ 25, Jan , 2019 , PT , "@djunct_slice_space" , MINOR , TYPE_BUG_FIX,
   "Couldn't deal with volumes that had subbrick selectors from @chauffeur*.",
   "Has been fixed now.\n"
},

{ 19, Jan , 2019 , PT , "@SSwarper" , MINOR , TYPE_BUG_FIX,
   "Program wouldn't run with '-odir ..' opt.",
   "Now it will.\n"
},

{ 21, Dec , 2018 , PT , "@djunct_montage_coordinator" , MINOR , TYPE_BUG_FIX,
   "Adjusted coordinator for a couple situations.",
   "Should be more centered for both 3D and 4D applications.\n"
},

{ 19, Dec , 2018 , PT , "@djunct_montage_coordinator" , MAJOR , TYPE_BUG_FIX,
   "This montage coordinator was noooot picking the right vol to focus on.",
   "That should be fixed via magical incantations now.\n"
},

{ 5, Dec , 2018 , PT , "@chauffeur_afni" , MICRO , TYPE_GENERAL,
   "Reduce list of program dependencies to more accurate one.",
   "List is muuuuch shorter now; had just been relic of @snapshot_volreg.\n"
},

{ 5, Dec , 2018 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "When there is no warning message in a category, just say 'none'.",
   "Before, 'none' was padded with newline chars, but Mac doesn't like :(.\n"
},

{ 5, Dec , 2018 , PT , "@djunct_select_str.py" , MINOR , TYPE_GENERAL,
   "Have removed numpy dependency.",
   "Lighter installation/usage dependencies.\n"
},

{ 5, Dec , 2018 , PT , "@djunct_combine_str.py" , MINOR , TYPE_GENERAL,
   "Have removed numpy dependency.",
   "Lighter installation/usage dependencies.\n"
},

{ 5, Dec , 2018 , PT , "@djunct_calc_mont_dims.py" , MINOR , TYPE_GENERAL,
   "Have removed numpy dependency.",
   "Lighter installation/usage dependencies.\n"
},

{ 5, Dec , 2018 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Have removed numpy dependency.",
   "Now, default afni_proc.py will output APQC HTML without numpy on comp.\n"
},

{ 5, Dec , 2018 , PT , "1dplot.py" , MINOR , TYPE_GENERAL,
   "Have removed numpy dependency.",
   "Lighter installation/usage dependencies.\n"
},

{ 2, Dec , 2018 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_BUG_FIX,
   "Will work with resting state analyses now.",
   "Fixed minor issue when no stat dset (just NO_STATS str) was present.\n"
},

{ 28, Nov , 2018 , PT , "1dplot.py" , MINOR , TYPE_BUG_FIX,
   "In py3, having a censor line caused graphing issues.",
   "Those issues have been resolved.\n"
},

{ 27, Nov , 2018 , PT , "apqc_make_tcsh.py" , MINOR , TYPE_GENERAL,
   "Now make enorm and outlier plots even if no censor_dsets are in uvars.",
   "Also, on a more fun note, output censor frac below mot/outlier plots.\n"
},

{ 27, Nov , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_BUG_FIX,
   "Wasn't using user's specified delta_slices-- but now is!.",
   "Grazie, S. Torrisi!\n"
},

{ 27, Nov , 2018 , PT , "apqc_make_html.py" , MINOR , TYPE_GENERAL,
   "Make python3 compatible.",
   "updated.\n"
},

{ 25, Nov , 2018 , PT , "@FindAfniDsetPath" , MINOR , TYPE_BUG_FIX,
   "Dsets weren't found in places specified by env var.",
   "Fixed now.\n"
},

{ 23, Nov , 2018 , PT , "apqc_make_html.py" , MAJOR , TYPE_GENERAL,
   "Much better page formatting now, including navigation bar.",
   "User can jump to sections.\n"
},

{ 23, Nov , 2018 , PT , "apqc_make_tcsh.py" , MAJOR , TYPE_GENERAL,
   "Outputs JSON files now, for easier handling of information later.",
   "These provide more comprehensive info, as well as href linknames.\n"
},

{ 20, Nov , 2018 , PT , "apqc_make_html.py" , MINOR , TYPE_GENERAL,
   "Make subtxt fonts gray (oooh!) and uniformly bold.",
   "Also, made image links not be whole line (much more convenient).\n"
},

{ 20, Nov , 2018 , PT , "@djunct_montage_coordinator" , MINOR , TYPE_NEW_PROG,
   "For use with @chauffeur_afni: subroutine that used to be *in* it.",
   "More modular and useful now, better selection of montage xhair loc, too.\n"
},

{ 20, Nov , 2018 , PT , "@chauffeur_afni" , MINOR , TYPE_BUG_FIX,
   "Fixed the calc of the location of xhairs when box_focus_slices was used.",
   "Should have correct focal location in montages now.\n"
},

{ 21, Oct , 2018 , PT , "apqc_make_tcsh.py" , MICRO , TYPE_GENERAL,
   "Include 'enorm' and 'outlier' string labels in basic 1dplot.",
   "Clarify plot...\n"
},

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

{ 1, Nov , 2018 , PT , "1dplot.py" , MAJOR , TYPE_NEW_PROG,
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
