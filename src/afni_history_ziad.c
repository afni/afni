
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

afni_history_struct ziad_history[] = {
/*=====BELOW THIS LINE=====*/
 { 22 , MAY , 2009 , ZSS , "@FS_roi_label" , MINOR , TYPE_NEW_PROG ,
   "A script to get FreeSurfer parcellation and annotation labels",
   "The script is used to return a label associated with a FreeSurfer\n"
   "annotation or parcellation integer label."
 } ,

 { 21 , MAY , 2009 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "Additional check for caret-version string",
   "It seems that the syntax for caret-version has changed\n"
   "slightly and was being missed by SUMA. As a result, Caret\n"
   "surfaces were not being properly aligned with the volumes."
 } ,

 { 19 , MAY , 2009 , ZSS , "prompt_user" , MINOR , TYPE_NEW_PROG ,
   "Prompts user for input with an X window",
   NULL
 } ,

 { 19 , MAY , 2009 , ZSS , "DriveSuma" , MICRO , TYPE_NEW_OPT ,
   "Modified time out for DriveSuma to 5 minutes",
   "Time out can be controlled by environment variable SUMA_DriveSumaMaxWait"
 } ,

 { 19 , MAY , 2009 , ZSS , "@ROI_Corr_Mat" , MAJOR , TYPE_NEW_PROG ,
   "A script to calculate correlation matrices between ROI time series",
   "Calculates the correlation matrices between average time series from ROIs\n"
   "defined in a mask volume. Script was written with Hang Joon Jo."
 } ,

 { 19 , MAY , 2009 , ZSS , "3dROIstats" , MINOR , TYPE_NEW_OPT ,
   "Added -zerofill and -roisel options",
   "Added options to force output for empty ROIs."
 } ,

 { 19 , MAY , 2009 , ZSS , "1ddot" , MINOR , TYPE_NEW_OPT ,
   "Added -okzero to keep program from exiting with all zero input",
   "Added options to force output for empty ROIs."
 } ,
 
 { 8 , MAY , 2009 , ZSS , "@SUMA_Make_Spec_FS" , MINOR , TYPE_NEW_OPT ,
   "The script now automatically turns FS annot files to SUMA ROI files",
   NULL
 } ,
 
 { 8 , MAY , 2009 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "Better display of ROI contours on patches",
   "Suma now correctly handles ROI display when the ROIs\n"
   "contain nodes that are not in the patch. Also, on flat surfaces,\n"
   "ROI contours are drawn raised so that they float over flat surfaces."
 } ,

 { 5 , MAY , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_BUG_FIX ,
   "Added 'd' and 'j' keys",
   NULL
 } ,

 { 5 , MAY , 2009 , ZSS , "3dRank" , MINOR , TYPE_NEW_PROG ,
   "A program that substitutes a voxel's value by that value's rank",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "SurfSmooth" , MINOR , TYPE_BUG_FIX ,
   "-detprefix_* options now do something!",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "3dTcorrMap" , MINOR , TYPE_NEW_OPT ,
   "-VarThresh* options for obtaining counts at different thresholds",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "ConvertDset" , MINOR , TYPE_NEW_OPT ,
   "-node_select_1D now respects node ordering",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "ROI2dataset" , MINOR , TYPE_NEW_OPT ,
   "-nodelist* options to output node sequence forming ROI",
   NULL
 } ,
 
 { 29 , APR , 2009 , ZSS , "MapIcosahedron" , MINOR , TYPE_BUG_FIX ,
   "Fixed crash when using -it option for geometry smoothing",
   NULL
 } ,
 
 { 14 , APR , 2009 , ZSS , "suma-general" , MINOR , TYPE_BUG_FIX ,
   "niml ROI files with empty ROI no longer crash suma/ROI2dataset",
   NULL
 } ,
 
 { 6 , APR , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT ,
   "Added support for '.', ',', and space keys, and 'shw_0'",
   NULL
 } ,
 
 { 1 , APR , 2009 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Arrow keys based navigation along surface.",
   "See 'Alt+U-D' section in SUMA's interactive usage help.\n" 
 } ,
 
 { 1 , APR , 2009 , ZSS , "suma" , MICRO , TYPE_NEW_ENV ,
   "Added SUMA_KeyNodeJump variable \n",
   "This variable controls number of nodes to jump with arrow keys navigation."
   "See suma -environment for complete list and defaults.\n"
 } ,
 
 { 31 , MAR , 2009 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Added 'on the fly' dot product computation",
   "This feature is only available with SUMA's -dev\n"
   "option. It is still in testing mode and its \n"
   "interface may change. If intrigued, see 'D' key's\n"
   "help in SUMA's interactive usage help.\n" 
 } ,
 
 { 31 , MAR , 2009 , ZSS , "suma-general" , MINOR , TYPE_MODIFY ,
   "Handling of double precision datatypes.",
   NULL
 } ,
 
 { 31 , MAR , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT ,
   "Added support for '[' and ']' keys and -view_surf",
   NULL
 } ,
 
 { 26 , MAR , 2009 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT ,
   "Support for reading/writing MNI .obj surfaces",
   NULL
 } ,
 
 { 12 , MAR , 2009 , ZSS , "suma-general" , MINOR , TYPE_NEW_OPT ,
   "SUMA talks to matlab.",
   NULL
 } ,
 

 { 10 , FEB , 2009 , ZSS , "DriveSuma" , MICRO , TYPE_NEW_OPT ,
   "Added -anim_dup to slow down movies",
   NULL
 } ,

 { 3 , FEB , 2009 , ZSS , "suma" , MICRO , TYPE_BUG_FIX ,
   "No longer crashes with old format ROIs",
   NULL
 } ,

 { 3 , FEB , 2009 , ZSS , "suma" , MICRO , TYPE_BUG_FIX ,
   "Fixed free-related error messages on OS X",
   "These errors were generated because some pointers\n"
   "were allocated with MCW_MALLOC and freed without it.\n"
   "solution was to include mcw_malloc.h in coxplot and\n"
   "gifti_choice.c"
 } ,

 { 2 , FEB , 2009 , ZSS , "DriveSuma" , MINOR , TYPE_NEW_OPT ,
   "Added load_col option to DriveSuma",
   NULL
 } ,

 { 2 , FEB , 2009 , ZSS , "SurfDist" , MINOR , TYPE_NEW_OPT ,
   "SurfDist now outputs distances from one node to a set of other nodes",
   NULL
 } ,

 { 13 , JAN , 2009 , ZSS , "afni-matlab" , MINOR , TYPE_NEW_PROG ,
   "Added GS_orth_1D.m to perform Gram-Schmidt orthogonalization",
   NULL
 } ,

 { 13, JAN , 2009 , ZSS , "afni-general" , MINOR ,  TYPE_BUG_FIX,
   "AFNI should now be Lesstif compliant",
   "Made numerous changes with Rick Reynolds. The biggest modification\n"
   "regards buttons inside popup menus. Those buttons are no longer\n"
   "inside rowcolumn widgets and had to be directly managed by afni."
 },

 { 5, JAN , 2009 , ZSS , "MapIcosahedron" , MINOR ,  TYPE_NEW_OPT,
   "Program no longer confined to FreeSurfer surfaces",
   "The program now allows the user to specify any morph sphere,\n"
   "not just sphere.reg or sphere. This required rewriting much of\n"
   "of the program's main section but the algorithm for the mapping\n"
   "remains unchanged."
 },

 { 5, JAN , 2009 , ZSS , "CreateIcosahedron" , MICRO ,  TYPE_NEW_OPT,
   "Program uses new Spec writing function and writes different surface formats",
   NULL
 },

 { 13 , DEC , 2008 , ZSS , "afni-matlab" , MAJOR , TYPE_NEW_PROG ,
   "RetroTS creates regressors for cardiac, respiratory, and RVT filtering",
   "RetroTS.m and accompanying functions create slice-based regressors\n"
   "for regressing out the effects of cardiac, respiratory, and RVT effects\n"
   "from FMRI time series. The regressors generated are meant to be used \n"
   "with 3dREMLfit."
 } ,

 { 9, DEC , 2008 , ZSS , "@Align_Centers" , MINOR ,  TYPE_NEW_OPT,
   "Added -cm option to allow centering based on the center of mass",
   NULL
 },

 { 9, DEC , 2008 , ZSS , "suma" , MINOR ,  TYPE_BUG_FIX,
   "Fixed (potential) SUMA crash when Draw ROI interface is first opened",
   NULL
 },

 { 9, DEC , 2008 , ZSS , "3dCM" , MICRO ,  TYPE_MODIFY,
   "Added history note to 3dCM",
   NULL
 },
 
 { 8, DEC , 2008 , ZSS , "suma" , MICRO ,  TYPE_BUG_FIX,
   "SUMA works now with LESSTIF, interface is almost the same as in MOTIF",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "general" , MICRO ,  TYPE_BUG_FIX,
   "Environment vars. set in the shell override those in .afnirc or .sumarc",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "@NoisySkullStrip" , MAJOR ,  TYPE_NEW_PROG,
   "A script to improve skull stripping in noisy of heavily shaded data",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "@Spharm.examples" , MAJOR ,  TYPE_NEW_PROG,
   "A script to demonstrate the usage of SpharmDeco and SpharmReco",
   NULL
 },

 { 2, DEC , 2008 , ZSS , "SpharmReco" , MAJOR ,  TYPE_NEW_PROG,
   "Reconstructs data from spherical harmonics decomposition.",
   "See SpharmDeco -help and the demo script @Spharm.examples\n"
   "for details."
 },

 { 2, DEC , 2008 , ZSS , "SpharmDeco" , MAJOR ,  TYPE_NEW_PROG,
   "Performs spherical harmonics decomposition.",
   "This program performs spherical harmonics decomposition\n"
   "for surfaces' geometry and/or surface-based data\n"
   "See SpharmReco -help and the demo script @Spharm.examples\n"
   "for details."
 },
 
 { 21 , NOV , 2008 , ZSS , "@fast_roi" , MAJOR ,  TYPE_NEW_PROG,
   "Creates Atlas-based ROIs in original space for real-time experiments ",
   NULL
 },
 
 { 13 , NOV , 2008 , ZSS , "suma" , MINOR ,  TYPE_NEW_OPT,
   "Added a NIDO sphere as Displayable Object ",
   "See suma -help_nido for details."
 },
 
 { 13 , NOV , 2008 , ZSS , "@SUMA_AlignToExperiment" , MINOR ,  TYPE_NEW_OPT,
   "Added -al option to use 3dAllineate -lpa ",
   NULL
 },
 
 { 03 , NOV , 2008 , ZSS , "3dNLfim" , MICRO ,  TYPE_NEW_OPT,
   "Added Exp (single exponential) model ",
   NULL
 },
 
 { 17 , OCT , 2008 , ZSS , "imcat" , MICRO ,  TYPE_NEW_OPT,
   "-crop is a new option for cropping an image ",
   NULL
 },
 
 { 03 , OCT , 2008 , ZSS , "suma" , MICRO ,  TYPE_NEW_ENV,
   "SUMA_StartUpLocation to control initial window placement ",
   "See output for suma -environment for details."
 },
 
 { 03 , OCT , 2008 , ZSS , "@DO.examples" , MINOR ,  TYPE_NEW_PROG,
   "A script that demonstrates the use of Displayable Objects",
   "See SUMA's interactive help for ctrl+alt+s for more information\n"
   "on Displayable Objects (DOs). "
 },

 { 03 , OCT , 2008 , ZSS , "suma" , MINOR ,  TYPE_NEW_OPT,
   "SUMA accepts text, images, and textures as Displayable Objects",
   "See SUMA's interactive help for ctrl+alt+s for more information\n"
   "on Displayable Objects (DOs). The new DOs are in a simple \n"
   "NIML format. It is simplest to look at the script @DO.examples\n"
   "for illustrations of the various forms of DOs that SUMA supports.\n"
   "Sample NIML DOs (called NIDOs) are now provided with the distribution\n"
   "They are called demo.*.niml.do."
 },

 { 03 , OCT , 2008 , ZSS , "DriveSuma" , MINOR ,  TYPE_NEW_OPT,
   "-viewer_position/_width/_height/_size to control window placement",
   "See DriveSuma -help for details."
 },


 { 16 , SEP , 2008 , ZSS , "DriveSuma" , MINOR ,  TYPE_NEW_OPT,
   "-load_do sends SUMA Displayable Objects to be rendered",
   "See SUMA's interactive help for ctrl+alt+s for more information\n"
   "on Displayable Objects.\n"
 },

 { 16 , SEP , 2008 , ZSS , "SurfDist" , MINOR ,  TYPE_NEW_OPT,
   "-node_path_do outputs the shortest path between two nodes",
   "The shortest path(s) are stored as a SUMA Displayable Object\n"
   "which can be loaded into SUMA with ctrl+alt+s or using DriveSuma.\n"
 },

 { 2 , SEP , 2008 , ZSS , "3dSkullStrip" , MINOR ,  TYPE_BUG_FIX,
   "Starting sphere center was incorrectly initialized",
   NULL
 },
 
 { 29 , AUG , 2008 , ZSS , "afni-general" , MICRO,  TYPE_GENERAL,
   "Added toy programs 3dTsmoothR.c and toyR.c to test C<-->R interface",
   "The programs demonstrate how to call R functions from C.\n"
   "To build them one needs to run tcsh ./@RmakeOpts\n"
   "Add 'include Makefile.R.INCLUDE' to Makefile\n"
   "Then make 3dTsmoothR toyR\n"
   "The programs demonstrate time series processing and plotting in R."
 },
  
 { 29 , AUG , 2008 , ZSS , "ExamineXmatR" , MAJOR,  TYPE_NEW_PROG,
   "An interactive tool to examine a design matrix",
   NULL
 },
 
 { 29 , AUG , 2008 , ZSS , "3ddelay" , MINOR ,  TYPE_BUG_FIX,
   "Unitialized pointer in 3ddelay",
   NULL
 },

 { 29 , JUL , 2008 , ZSS , "afni" , MINOR , TYPE_MODIFY ,
   "Changed transform used to crete mni coord. in interactive whereami",
   "The transform from TLRC to MNI used to be via the manually TLRCed\n"
   "N27 brain. However this created inconsistency in the second line of the \n"
   "Focus point output (MNI Brain) on the order of a couple of mm\n"
   "with the command-line whereami program. \n"
   "Now both interactive and command-line whereami produce the same \n"
   "Focus Point output. Note that the rest of the whereami\n"
   "output remains unchanged."
 },
  
 { 29 , MAY , 2008 , ZSS , "3dfim+" , MINOR , TYPE_MODIFY ,
   "allowed for -polort -1 and for sub-brick selection with dataset names",
   NULL
 } ,

 { 15 , MAY , 2008 , ZSS , "3dmaskdump" , MINOR , TYPE_NEW_OPT ,
   "added -n_rand and -n_randseed",
   NULL
 } ,

 { 15 , MAY , 2008 , ZSS , "3dfim+" , MINOR , TYPE_BUG_FIX ,
   "Fixed memory corruption when using more than 20 regressors",
   NULL
 } ,

 { 9 , MAY , 2008 , ZSS , "3dROIstats" , MAJOR , TYPE_NEW_OPT ,
   "Added option -1Dformat to output results in 1D format",
   NULL
 } ,

 { 8 , MAY , 2008 , ZSS , "3dsvm" , MAJOR , TYPE_BUG_FIX ,
   "Fixed memory corruption caused by improper declaration of combName",
   NULL
 } ,

 { 24 , APR , 2008 , ZSS , "SurfSmooth" , MICRO , TYPE_BUG_FIX ,
   "Fixed leaky SurfSmooth.",
   "Leak was from one of fin_float pointers in Head07\n"
   "Also found leak in THD_extract_detrended_array and a few\n"
   "more small leaks in other SUMA function. Valgrind is good."
 } ,

 { 23 , APR , 2008 , ZSS , "afni-python" , MICRO , TYPE_MODIFY ,
   "Changed methods in afni_name class",
   ".path is now absolute\n"
   ".inp() and .out() should be used to specify input and output volumes\n"
   "Updated lpc_align.py and align_epi_anat.py to reflect changes."
 } ,

 { 2 , APR , 2008 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "Changed suma -environment's output to reflect user's current environment.",
   "With this change, users can always replace their .sumarc with\n"
   "the one output by suma -environment without worrying about loosing\n"
   "their preferred settings."
 } ,

 { 27 , MAR , 2008 , ZSS , "suma-general" , MICRO , TYPE_MODIFY ,
   "Changed internal handling of various MATVEC sources.",
   "Simplified handling of different sources of MATVECs from\n"
   "AFNI's headers. Added handling of ALLINEATE_MATVEC to the\n"
   "mix."
 } ,

 { 27 , MAR , 2008 , ZSS , "afni-matlab" , MINOR , TYPE_MODIFY ,
   "Added other return options for Read_1D.m and BrikLoad.m",
   NULL
 } ,

 { 26 , MAR , 2008 , ZSS , "3dvolreg" , MINOR , TYPE_BUG_FIX ,
   "3dvolreg's -1Dmatrix_save was not always accounting for centers of rotation"
   ,"Now the output of -1Dmatrix_save always matches what one would get for\n"
   "the first sub-brick from\n"
   "   cat_matvec FRED+orig::VOLREG_MATVEC_000000 -I "
 } ,

 { 26 , MAR , 2008 , ZSS , "cat_matvec" , MINOR , TYPE_MODIFY ,
   "cat_matvec can now output IJK_TO_CARD_DICOM and IJK_TO_DICOM_REAL" ,
   NULL
 } ,

 { 26 , MAR , 2008 , ZSS , "@Align_Centers" , MINOR , TYPE_MODIFY ,
   "Script now outputs a matrix that represents the shift" ,
   "This matrix can be used with 3dAllineate to perform the shift."
 } ,

 { 26 , MAR , 2008 , ZSS , "@SUMA_AlignToExperiment" , MINOR , TYPE_NEW_OPT ,
   "Added -align_centers as an option to deal with very large moves" ,
   "It used to be that users had to run @Align_Centers on all their\n"
   "data when big moves were needed to align the surface's antaomy\n"
   "to that of the experiment. This is no longer needed."
 } ,

 { 25 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_BUG_FIX ,
   "ROIs drawn on different surfaces now handled properly" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "suma-general" , MINOR , TYPE_MODIFY ,
   "Instead of SAME, one can use surface's name as LocalDomainParent" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "ConvertSurface" , MICRO , TYPE_NEW_OPT ,
   "Added option -ixmat_1D to apply inverse of affine transform" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "@auto_tlrc" , MICRO , TYPE_MODIFY ,
   "Improvements on -rigid_equiv output and .1D transform output" ,
   NULL
 } ,

 { 25 , MAR , 2008 , ZSS , "@SUMA_AlignToExperiment" , MICRO , TYPE_MODIFY ,
   "View is now changed even if rigid-body registration is used." ,
   NULL
 } ,

 { 21 , MAR , 2008 , ZSS , "3dnvals" , MICRO , TYPE_NEW_OPT ,
   "Added -all to 3dnvals to output all 4 dimensions. " ,
   NULL
 } ,

 { 17 , MAR , 2008 , ZSS , "lpc_align.py" , MICRO , TYPE_NEW_OPT ,
   "Added -big_move and -partial_coverage." ,
   "-big_move is for when large displacements are needed for alignment.\n"
   "-partial_coverage is for when EPI covers a portion of the brain."
 } ,

 { 12 , MAR , 2008 , ZSS , "suma" , MICRO , TYPE_BUG_FIX ,
   "Changed crash in SurfaceMetrics when -spec and -i options are mixed. " ,
   NULL
 } ,

 { 12 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_MODIFY ,
   "Changed surface controller font to 8. " ,
   "You can get the old font size with environment\n"
   "variable SUMA_SurfContFontSize BIG.\n"
   "The default is now SMALL.\n"
 } ,

 { 7 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "Color map in surface controller can be flipped and rotated" ,
   "* f key for flipping colormap\n"
   "* Up/Down keys for rotating color map\n"
   "* Home for home\n"
   "Hit ctrl+h on Surface Controller's colormap for more help."
 } ,

 { 6 , MAR , 2008 , ZSS , "suma" , MINOR , TYPE_NEW_ENV ,
   "Added three variables affecting the surface controller" ,
   "* SUMA_ShowOneOnly: Sets '1 Only' on or off. On by default\n"
   "* SUMA_GraphHidden: Update open graphs even if corresponding dset\n"
   "                    is hidden."
   "* SUMA_ColorMapRotationFraction: Fraction of colormap to rotate\n"
   "                    up or down with arrow keys.\n"
   "See suma -environment for a complete list."
 } ,

 { 5 , MAR , 2008 , ZSS , "suma-general" , MAJOR , TYPE_GENERAL ,
   "Support for GIFTI surface format writing" ,
   "GIFTI writing can now be done with SUMA programs.\n"
   "For example, see options -o_gii and -xml* in ConvertSurface program."
 } ,
 
 { 28 , FEB , 2008 , ZSS , "suma-general" , MAJOR , TYPE_NEW_ENV ,
   "Support for GIFTI surface format reading" ,
   NULL 
 } ,
 
 { 27 , FEB , 2008 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Addition of p and q values under threshold bar" ,
   "Use BHelp on p and q text in suma to get more info."
 } ,
 
 { 27 , FEB , 2008 , ZSS , "suma-general" , MICRO , TYPE_BUG_FIX ,
   "Another bout of initialization and leak fixes based on valgrind's output" ,
   "The hope is that some weird X behavior is caused by uninitialized\n"
   "variables."
 } ,
 
 { 14 , FEB , 2008 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Graphing of dset content with 'g'" ,
   "A graph of the column content at a node can be plotted\n"
   "This would be the surface equivalent to AFNI's graphing function.\n"
   "See suma's interactive help 'ctrl+h' for more info."
 } ,

 { 12 , FEB , 2008 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "An option to show all of suma's environment variables" ,
   "See help for -environment in suma -help."
 } ,
 
 { 6 , FEB , 2008 , ZSS , "SurfDist" , MAJOR , TYPE_NEW_PROG ,
   "Program to calculate geodesic internodal distances" ,
   NULL
 } ,
 
 { 12 , JAN , 2008 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Scroll lists for I T and B selectors in suma" ,
   "Right click on pulldown menu titles to get\n"
   "a scroll list instead. This makes selecting from\n"
   "a long list of options, or columns, much easier.\n"
   "Right click on 'I' to the left of suma's Intensity Selection\n"
   "button for an illustration."
 } ,
 
 { 19 , DEC , 2007 , ZSS , "suma-general" , MAJOR , TYPE_GENERAL ,
   "Use of '[i]' to select node index from surface dset" ,
   "Square bracket '[]' selection works for surface-based\n"
   "datasets much like it does for volume-based ones.\n"
   "In addition, one can use '[i]' to select the indices\n"
   "of nodes for which data are defined in a particular \n"
   "surface-based dataset.\n"
   "For more information, see 'SUMA dataset input options:' \n"
   "section in the output of ConvertDset -help ."
 } ,

 { 18 , DEC , 2007 , ZSS , "ROIgrow" , MINOR , TYPE_NEW_PROG ,
   "Grows regions separately, depending on labels" ,
   NULL
 } ,
 
 { 18 , DEC , 2007 , ZSS , "ROI2dataset" , MINOR , TYPE_GENERAL ,
   "Output full datasets if needed" ,
   NULL
 } ,
 
 { 17 , DEC , 2007 , ZSS , "ConvertDset" , MINOR , TYPE_GENERAL ,
   "Output of full dsets if needed" ,
   "This can be used to force a dataset with data\n"
   "defined on a few nodes to be written out with a\n"
   "complete list of nodes, using 0 where no data are defined."
 } ,
 
 { 19 , NOV , 2007 , ZSS , "Surf2VolCoord" , MINOR , TYPE_NEW_PROG ,
   "Program to show surface-node to voxel correspondence" ,
   "This can be used to understand how surface coordinates\n"
   "relate to voxel coordinates."
 } ,

 { 26 , SEP , 2007 , ZSS , "SurfSmooth" , MAJOR , TYPE_MODIFY ,
   "Big changes to data smoothing functions" ,
   "* HEAT_07 method does controlled blurring, with options\n"
   "to blur 'to' a particular FWHM. No guessing needed for\n"
   "iterative kernel bandwidth or number of iterations.\n"
   "* HEAT_05 method improved to reduce numerical precision\n"
   "problems."
 } ,
 
 { 26 , SEP , 2007 , ZSS , "SurfFWHM" , SUPER , TYPE_NEW_PROG ,
   "Program to estimate FWHM of data on surface" ,
   NULL
 } ,
 
 { 10 , MAR , 2007 , ZSS , "MapIcosahedron" , MINOR , TYPE_MODIFY ,
   "Better handling of surface centers" ,
   NULL
 } ,
 
 { 15 , FEB , 2007 , ZSS , "suma" , MINOR , TYPE_NEW_OPT ,
   "High resolution image saving with ctrl+r" ,
   "Very high resolution images can be created.\n"
   "See suma's interactive help 'ctrl+h' for more info."
 } ,

 { 5 , FEB , 2007 , ZSS , "SurfDsetInfo" , MINOR , TYPE_NEW_PROG ,
   "Program to display surface dataset information" ,
   "Output is crude at the moment."
 } ,

 { 15 , JAN , 2007 , ZSS , "suma" , MAJOR , TYPE_GENERAL ,
   "Allow replacement of pre-loaded DO and Dsets" ,
   "When a dataset is reloaded, it replaces the one\n"
   "already loaded in suma."
 } ,
 
 { 5 , JAN , 2007 , ZSS , "imcat" , MINOR , TYPE_NEW_PROG ,
   "Program to stitch images." ,
   NULL
 } ,

 { 30 , NOV , 2006 , ZSS , "suma" , MINOR , TYPE_GENERAL ,
   "Addition of new Displayable Objects (DO)(ctrl+Alt+s)" ,
   "Allows display of segments, spheres and other markers\n"
   "See suma's interactive help 'ctrl+h' for more info."
 } ,
 
 { 20 , SEP , 2006 , ZSS , "DriveSuma" , MAJOR , TYPE_NEW_PROG ,
   "Program to control SUMA from the command line" ,
   NULL
 } ,

 { 31 , AUG , 2006 , ZSS , "AnalyzeTrace" , MICRO , TYPE_NEW_PROG ,
   "Program to analyze the output of -trace option." ,
   NULL
 } ,

 
 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
