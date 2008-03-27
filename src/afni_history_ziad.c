
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
