
/** cf. afni_history.h **/

#include "afni_history.h"

/*  basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long descrption with intermediate newlines
  
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
           - it may be nice to put the newest entires at the top
           - leave the last "99, NULL" entry as it is

 -- examples --

 { 26 , FEB , 2008 , RCR , "my_program" , MAJOR ,
   "short description of change" ,
   "(optional) detailed description, or where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

afni_history_struct rwcox_history[] = {
/*=====BELOW THIS LINE=====*/

 { 29 , MAR , 2010 , RWC , "3dAllineate" , MICRO , TYPE_BUG_FIX ,
   "Make sure auto_tstring is set to something reasonable" ,
   NULL } ,

 { 29 , MAR , 2010 , RWC , "afni_history" , MICRO , TYPE_MODIFY ,
   "Add -dline option, alternative to -html option" ,
   NULL } ,

 { 25 , MAR , 2010 , RWC , "help_format" , MINOR , TYPE_MODIFY ,
   "Hyperlink refs to other program names in -help Web pages" ,
   NULL } ,

 { 24 , MAR , 2010 , RWC , "3dFWHMx" , MICRO , TYPE_NEW_OPT ,
   "Add -2difMAD option, for dealing with PET data, maybe." ,
   NULL } ,

 { 19 , MAR , 2010 , RWC , "3dGroupInCorr" , MICRO , TYPE_MODIFY ,
   "Unroll correlation inner loop by 2 == speedup of 30% for this part" ,
   NULL } ,

 { 18 , MAR , 2010 , RWC , "cs_misc.c" , MICRO , TYPE_GENERAL ,
   "Comma-ize function, and used in various places, for pretty print" ,
   NULL } ,

 { 18 , MAR , 2010 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "Added Shft+Ctrl+click/drag to dynamic Group InstaCorr as well" ,
   NULL } ,

 { 17 , MAR , 2010 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "Add Shift+Ctrl+Button1 seed dragging in individual InstaCorr" ,
   NULL } ,

 { 16 , MAR , 2010 , RWC , "3dbucket" , MICRO , TYPE_GENERAL ,
   "Make -glueto keep compressed form of the first dataset" ,
   "Same change made for 3dTcat" } ,

 { 16 , MAR , 2010 , RWC , "3dREMLfit" , MINOR , TYPE_MODIFY ,
   "Allow all zero columns in regression matrix, with -GOFORIT option" ,
   "* Use SVD to desingularize matrix when QR factorizing (not elsewhere)\n"
   "* Remove coefficients for all zero columns from GLT matrices\n"
   "* Adjust DOF to compensate\n"
   "* This is Thalia Wheatley's fault -- blame her for any problems" } ,

 { 5 , MAR , 2010 , RWC , "mri_read" , MICRO , TYPE_MODIFY ,
   "Modify to allow row and col selectors on stdin" ,
   NULL } ,

 { 4 , MAR , 2010 , RWC , "3dGroupInCorr" , MINOR , TYPE_BUG_FIX ,
   "Fixed crash with paired t-test opcode" ,
   "Didn't switch to 1-sample mode when opcode was for paired, but no second\n"
   "data vector was passed in.  This, of course, is Mike Beauchamp's fault." } ,

 { 3 , MAR , 2010 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "GroupInstaCorr dataset now can be +orig" ,
   "If user sets environment variable AFNI_GROUPINCORR_ORIG to YES, as in\n"
   "\n"
   "  afni -niml -DAFNI_GROUPINCORR_ORIG=YES" } ,

 { 2 , MAR , 2010 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "moved weight sum in LPC" ,
   "* Old way: count a BLOK even if it doesn't contribute to correlation sum\n"
   "* New way: don't count it\n"
   "* If'n you want the old way, setenv AFNI_LPC_OLDWSUM YES\n"
   "* Also: used OpenMP to speedup coordinate transformations" } ,

 { 2 , MAR , 2010 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "add overlap 'ov' to lpc+ functional" ,
   "Kind of slow -- OpenMP mabye?" } ,

 { 1 , MAR , 2010 , RWC , "InstaCorr" , MICRO , TYPE_NEW_OPT ,
   "Add Spearman and Quadrant correlation options" ,
   NULL } ,

 { 26 , FEB , 2010 , RWC , "afni InstaCorr" , MICRO , TYPE_GENERAL ,
   "Add polort option to InstaCorr setup" ,
   "Beware the frumious Bandersnatch, Ziad." } ,

 { 24 , FEB , 2010 , RWC , "3dAllineate" , MINOR , TYPE_NEW_OPT ,
   "New cost functional -lpc+" ,
   "Combination of lpc + hel + crA + nmi + mi.\n"
   "Also some micro changes to allow more -twobest starting points and the\n"
   "like." } ,

 { 19 , FEB , 2010 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "Turn up -twoblur limit from 7 to 11." ,
   NULL } ,

 { 18 , FEB , 2010 , RWC , "3dDespike" , MICRO , TYPE_MODIFY ,
   "Add printout of Laplace distribution percentages" ,
   "And fix normal CDF calculation" } ,

 { 8 , FEB , 2010 , RWC , "3dSatCheck" , MICRO , TYPE_NEW_PROG ,
   "Program to check for initial transients" ,
   "i.e., if the FMRI time series has non-saturated time points at the\n"
   "beginning (on average) -- uses the code thd_satcheck.c for the real work\n"
   "-- the same as used in 3dBandpass.  At this time, 3dSatCheck is not\n"
   "compiled in the binary distributions of AFNI." } ,

 { 8 , FEB , 2010 , RWC , "3dBandpass" , MICRO , TYPE_MODIFY ,
   "Check for initial transients" ,
   "i.e., non-saturated MRI signal in the first few time points" } ,

 { 8 , FEB , 2010 , RWC , "3dBandpass" , MICRO , TYPE_MODIFY ,
   "OpenMP-ize the -blur option" ,
   NULL } ,

 { 8 , FEB , 2010 , RWC , "3dSetupGroupInCorr" , MICRO , TYPE_MODIFY ,
   "Change default storage to -byte from -short" ,
   NULL } ,

 { 4 , FEB , 2010 , RWC , "Group InstaCorr" , MICRO , TYPE_GENERAL ,
   "Set more informative labels for results sub-brick" ,
   "With the addition of the -label[AB] options to 3dGroupInCorr.c, the\n"
   "sending of labels to AFNI, and the setting of labels in\n"
   "afni_pplug_instacorr.c" } ,

 { 3 , FEB , 2010 , RWC , "Group InstaCorr" , MINOR , TYPE_GENERAL ,
   "2-sample case now also sends back 1-sample results" ,
   "With this, you can look at the 2-sample difference in controller A, and\n"
   "the 2 1-sample results in controllers B and C.  This lets you see the\n"
   "difference AND similarities at the same time." } ,

 { 3 , FEB , 2010 , RWC , "Group InstaCorr" , MICRO , TYPE_GENERAL ,
   "-byte option to save memory" ,
   "3dSetupGroupInCorr and 3dGroupInCorr can now use bytes to store the huge\n"
   "datasets, which will save disk space and memory.  Results are virtually\n"
   "identical." } ,

 { 31 , DEC , 2009 , RWC , "many" , MICRO , TYPE_GENERAL ,
   "Remove 'cast ... different size' warnings" ,
   "Macros: ITOP and PTOI to cast without warnings.  For Z." } ,

 { 31 , DEC , 2009 , RWC , "3dGroupInCorr" , MAJOR , TYPE_NEW_PROG ,
   "Group InstaCorr" ,
   "With changes to afni and also new program 3dSetupGroupInCorr" } ,

 { 23 , DEC , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Minor changes to 'DataDir' to appease DRG" ,
   NULL } ,

 { 18 , DEC , 2009 , RWC , "3dBandpass" , MICRO , TYPE_NEW_PROG ,
   "Finally finished this!" ,
   NULL } ,

 { 15 , DEC , 2009 , RWC , "3dmaskSVD" , MINOR , TYPE_BUG_FIX ,
   "Fixed 2 problems with -ort option" ,
   "1) It didn't work, since the wrong array name was used :-(\n"
   "2) It could fail, since the detrend routine didn't like collinear orts\n"
   "-- replaced it with one that is happier with such things." } ,

 { 14 , DEC , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "New environment variable AFNI_RECENTER_VIEWING" ,
   NULL } ,

 { 4 , DEC , 2009 , RWC , "3dLocalPV" , MICRO , TYPE_MODIFY ,
   "OpenMP changes.  Speedup about factor of 3 using 6 CPUs." ,
   NULL } ,

 { 3 , DEC , 2009 , RWC , "3dLocalPV" , MINOR , TYPE_NEW_PROG ,
   "Speeded up version of 3dLocalSVD" ,
   "Speed comes with some small limitations.  About 30% faster." } ,

 { 3 , DEC , 2009 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Modify GUI to add a Read session button next to the Switch button" ,
   "Because of complaints that it's hard to figure out what to do if AFNI\n"
   "doesn't start in the right directory." } ,

 { 30 , NOV , 2009 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Modify license to specify GPL v2 OR ANY LATER VERSION" ,
   NULL } ,

 { 24 , NOV , 2009 , RWC , "thd_atr.c" , MICRO , TYPE_BUG_FIX ,
   "Allow 'count = 0' in attributes" ,
   "Formerly was a fatal error.  Now just skips to next attribute.  For\n"
   "Ziad." } ,

 { 23 , NOV , 2009 , RWC , "3dTcorrMap" , MICRO , TYPE_NEW_OPT ,
   "Add various options in the last few days" ,
   "* -seed = a different dataset for the seed time series\n"
   "* -bpass = instead of -polort\n"
   "* -Gblur = blurring\n"
   "* -Mseed = extra smoothing around the seed\n"
   "* -Hist = output the histogram of the correlations" } ,

 { 23 , NOV , 2009 , RWC , "3dmaskSVD" , MICRO , TYPE_NEW_OPT ,
   "Add -bpass option" ,
   NULL } ,

 { 18 , NOV , 2009 , RWC , "mri_read.c" , MICRO , TYPE_GENERAL ,
   "Fix Ziad's stupid error printout in reading .1D files" ,
   "To indicate which file is causing the trouble.\n"
   "To limit the number of such Failure messages to a reasonable level.\n"
   "Sheesh." } ,

 { 17 , NOV , 2009 , RWC , "3dTfitter" , MICRO , TYPE_GENERAL ,
   "Let FALTUNG kernel be longer that N/2" ,
   "Also add synonyms '-L2' and '-L1'" } ,

 { 3 , NOV , 2009 , RWC , "3dREMLfit" , MICRO , TYPE_GENERAL ,
   "Modify default -CORcut value from 0.0025 to 0.0011" ,
   "To reduce likelihood of Choleski failure." } ,

 { 21 , OCT , 2009 , RWC , "1dAstrip" , MICRO , TYPE_NEW_PROG ,
   "To remove Alpha characters from 1D-like files." ,
   "For my own ill-conceived plans for global domination.  Cheap and not\n"
   "perfect." } ,

 { 19 , OCT , 2009 , RWC , "3dBlurInMask" , MICRO , TYPE_NEW_OPT ,
   "add -preserve option, to keep Rick Reynolds from defecting to SPM" ,
   NULL } ,

 { 16 , OCT , 2009 , RWC , "cs_qmed.c" , MICRO , TYPE_GENERAL ,
   "added function to compute biweight midvariance" ,
   NULL } ,

 { 8 , OCT , 2009 , RWC , "3dPeriodogram" , MICRO , TYPE_NEW_PROG ,
   "Does what it sound like, more or less, I hope." ,
   NULL } ,

 { 7 , OCT , 2009 , RWC , "various" , MICRO , TYPE_MODIFY ,
   "Modify srand48() init to use time()+getpid()" ,
   "To make close-in-time runs have independent seeds." } ,

 { 7 , OCT , 2009 , RWC , "3dBlurInMask" , MINOR , TYPE_NEW_OPT ,
   "Add -Mmask option to allow multiple mask values" ,
   "For independent blurring (e.g., blur WM and GM separately)." } ,

 { 2 , OCT , 2009 , RWC , "calc" , MICRO , TYPE_MODIFY ,
   "Add mod(a,b) function to please RCR" ,
   NULL } ,

 { 2 , OCT , 2009 , RWC , "1dgenARMA11" , MICRO , TYPE_NEW_PROG ,
   "Generates ARMA(1,1) correlated noise time series" ,
   "For simulation purposes." } ,

 { 30 , SEP , 2009 , RWC , "3dDeconvolve" , MICRO , TYPE_MODIFY ,
   "Let user modulate amplitude of dmBLOCK" ,
   "In particular, dmBLOCK(0) means the program modulates the amplitude\n"
   "based on duration." } ,

 { 30 , SEP , 2009 , RWC , "3dDeconvolve" , MICRO , TYPE_MODIFY ,
   "Transpose row-wise 1D file on input when user screws up" ,
   "1D files input to 3dDeconvolve are expected to have 1 column and many\n"
   "rows.  If the user gets this backwards, the program now transposes the\n"
   "file internally, with an INFO message." } ,

 { 29 , SEP , 2009 , RWC , "afni InstaCorr" , MICRO , TYPE_GENERAL ,
   "Modify SeedBlur to SeedRad" ,
   "Flat average over a sphere of given radius, instead of a Gaussian blur. \n"
   "For Steve and Alex." } ,

 { 29 , SEP , 2009 , RWC , "afni" , MAJOR , TYPE_MODIFY ,
   "InstaCalc is more-or-less ready" ,
   "At least, I let Mike Beauchamp test it for his cunning plans." } ,

 { 25 , SEP , 2009 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "InstaCalc is starting to work! Sort of. Crudely." ,
   NULL } ,

 { 24 , SEP , 2009 , RWC , "thd_1Ddset.c" , MICRO , TYPE_GENERAL ,
   "Allow writing of 1D complex datasets instead of convert to float" ,
   "For Larry Frank, who I love like a brother.  Or a second cousin once\n"
   "removed." } ,

 { 17 , SEP , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Started writing InstaCalc" ,
   NULL } ,

 { 16 , SEP , 2009 , RWC , "All" , MICRO , TYPE_GENERAL ,
   "Oh, and did I mention that Ziad Saad is Trouble?" ,
   "Rasmus Birn isn't far behind, either." } ,

 { 16 , SEP , 2009 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "FIx recursive calls to AFNI_set_viewpoint()" ,
   "Caused by the UNCLUSTERIZE macro, which now checks to see if the\n"
   "function redisplay is really needed." } ,

 { 16 , SEP , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Make the AFNI_FLASH_VIEWSWITCH variable default to NO, not YES" ,
   "Sorry, Adam, but Ziad matters more to me than you do." } ,

 { 16 , SEP , 2009 , RWC , "parser,f" , MICRO , TYPE_MODIFY ,
   "Add 'NOT' function as a synonym for 'ISZERO'" ,
   NULL } ,

 { 9 , SEP , 2009 , RWC , "3dmaskave" , MICRO , TYPE_NEW_OPT ,
   "Add box and ball options to this program, per Kyle Simmons" ,
   NULL } ,

 { 9 , SEP , 2009 , RWC , "3dmaskdump" , MINOR , TYPE_NEW_OPT ,
   "Add 'ball' options for mask generation" ,
   NULL } ,

 { 2 , SEP , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Add ZeroToOne 1D transform function" ,
   NULL } ,

 { 27 , AUG , 2009 , RWC , "AFNI image viewer" , MICRO , TYPE_GENERAL ,
   "Keyboard Home key centers zoom window at crosshairs" ,
   "Doesn't work if cropping and zooming are mixed.  (The logistics are too\n"
   "hard for my feeble brain.)" } ,

 { 27 , AUG , 2009 , RWC , "AFNI image viewer" , MICRO , TYPE_MODIFY ,
   "More crop region features" ,
   "* Shift+Home centers crop region on the crosshairs\n"
   "* Shift/Ctrl+arrow now respect image flip status\n"
   "* Environment variable AFNI_IMAGE_CROPSTEP sets sign and size of crop\n"
   "region shifting with Shift+arrow keys\n"
   "* If this doesn't make John Butman happy, I don't know what will." } ,

 { 26 , AUG , 2009 , RWC , "3dFFT" , MINOR , TYPE_NEW_PROG ,
   "Compute spatial 3D FFT of a dataset" ,
   "* This is for John Butman\n"
   "* Was started a long time ago, but only finished today\n"
   "* Probably useless" } ,

 { 25 , AUG , 2009 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "Provide keystroke fine control over cropping in image viewer" ,
   "Shift+arrowkey = scroll crop window\n"
   "Ctrl+arrowkey  = resize crop window\n"
   "Right-click on crop button = menu to set size exactly\n"
   "Hint on crop button = shows crop parameters" } ,

 { 24 , AUG , 2009 , RWC , "mrilib.h" , MICRO , TYPE_BUG_FIX ,
   "CABS macro fails if complex number has huge components" ,
   "This is Larry Frank's fault, of course." } ,

 { 24 , AUG , 2009 , RWC , "3dAFNItoNIML" , MICRO , TYPE_NEW_OPT ,
   "Add -ascii option == way to dump dataset in plain text" ,
   NULL } ,

 { 11 , AUG , 2009 , RWC , "cs_symeig.c" , MICRO , TYPE_GENERAL ,
   "Speedup first_principal_vectors() -- for 3dmaskSVD and 3dLocalSVD" ,
   "By hand tweaking the normal matrix calculation loops" } ,

 { 11 , AUG , 2009 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "Make handedness warning more explicit" ,
   NULL } ,

 { 10 , AUG , 2009 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Fix crash reported by Ziad and Rick" ,
   "In afni_setup_viewing(), fim_now might not be valid -- so allow for\n"
   "that." } ,

 { 10 , AUG , 2009 , RWC , "3dABoverlap" , MICRO , TYPE_MODIFY ,
   "Modified to skip automask if dataset is byte-valued with 1 volume" ,
   NULL } ,

 { 27 , JUL , 2009 , RWC , "libmri" , MICRO , TYPE_MODIFY ,
   "Add function mri_get_tempfilename() to mri_purger.c" ,
   "To return a unique filename in a temp directory.  Sort of like a fancy\n"
   "version of the C library function tmpnam()." } ,

 { 24 , JUL , 2009 , RWC , "THD_open_3dcalc()" , MICRO , TYPE_GENERAL ,
   "Modify to use globally unique filename every time, fer shur." ,
   "cf. code in file thd_mastery.c, using the UNIQ_idcode() function in\n"
   "niml_uuid.c to create a unique filename" } ,

 { 24 , JUL , 2009 , RWC , "afni.h" , MICRO , TYPE_GENERAL ,
   "Replaced VERSION with AVERZHN to avoid conflicts with SVMlight" ,
   "So AFNI doesn't have a VERSION any more, it has an AVERZHN." } ,

 { 23 , JUL , 2009 , RWC , "3dTfitter" , MICRO , TYPE_NEW_OPT ,
   "Added -errsum option, to save error sums per voxel." ,
   "And a -help example showing how the error sum of squares can be used to\n"
   "compute partial correlation coefficients of a fit." } ,

 { 23 , JUL , 2009 , RWC , "3dLocalSVD" , MICRO , TYPE_BUG_FIX ,
   "Fix bug when all vectors are zero." ,
   NULL } ,

 { 21 , JUL , 2009 , RWC , "niml_feedme" , MINOR , TYPE_NEW_PROG ,
   "Test and demo program showing how to set datasets to AFNI via NIML" ,
   "An analog to rtfeedme.  Sends volumes to AFNI using VOLUME_DATA\n"
   "elements.  Pretty rudimentary." } ,

 { 21 , JUL , 2009 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "Update widgets and viewing when VOLUME_DATA is added to a dataset" ,
   "* Fix AFNI_setup_viewing() to update widgets properly when dataset nvals\n"
   "changes\n"
   "* Add function AFNI_update_dataset_viewing() to deal with viewing\n"
   "changes that might be needed if a dataset is altered" } ,

 { 17 , JUL , 2009 , RWC , "3dLocalstat" , MICRO , TYPE_MODIFY ,
   "speedup for OpenMP" ,
   "Modify mri_nstats.c to use pre-malloc-ed workspaces, instead of a new\n"
   "one for each calculation, which makes a big difference in OpenMP." } ,

 { 16 , JUL , 2009 , RWC , "3dmaskSVD" , MICRO , TYPE_MODIFY ,
   "Speedup (a lot for large masks) by using new SVD routine." ,
   NULL } ,

 { 15 , JUL , 2009 , RWC , "3dLocalSVD" , MICRO , TYPE_GENERAL ,
   "Modify to use more efficient SVD routine" ,
   NULL } ,

 { 13 , JUL , 2009 , RWC , "3dLocalstat" , MINOR , TYPE_MODIFY ,
   "OpenMP" ,
   "Also added option '-use_nonmask' to allow statistics to be computed for\n"
   "voxels not in the mask (but presumably whose neighbors are in the mask)." } ,

 { 1 , JUL , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Modify AFNI_START_SMALL to pick smallest dataset of all." ,
   "Rather than the smallest 'anat' and smallest 'func', separately." } ,

 { 30 , JUN , 2009 , RWC , "various 3D programs" , MICRO , TYPE_BUG_FIX ,
   "Remove keywords propagation stuff" ,
   "e.g., 3dTcat and 3dbucket\n"
   "Also, fix keywords printout buffer overflow in thd_info.c" } ,

 { 29 , JUN , 2009 , RWC , "3dREMLfit" , MICRO , TYPE_BUG_FIX ,
   "Fix memcpy bug in data extraction when using OpenMP.  Ugghh." ,
   NULL } ,

 { 26 , JUN , 2009 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Add 1D index jumping to \"Jump to (ijk)\"" ,
   NULL } ,

 { 25 , JUN , 2009 , RWC , "3dREMLfit" , MICRO , TYPE_MODIFY ,
   "More tweaks to the OpenMP-ization.  Only slightly better." ,
   NULL } ,

 { 24 , JUN , 2009 , RWC , "3dREMLfit" , MINOR , TYPE_GENERAL ,
   "Modify to use OpenMP more effectively." ,
   "Have to avoid use of Doug's matrix.c functions in the main loops, since\n"
   "they do so much malloc/free, which blocks other threads from running. \n"
   "Instead, rewrote versions of the needed functions that use pre-allocated\n"
   "workspace arrays.  Speedup is very good now for the REML setup and REML\n"
   "voxel loops.  Haven't decided whether to OpenMP-ize the GLSQ or OLSQ\n"
   "loops, since these usually take much less time." } ,

 { 23 , JUN , 2009 , RWC , "3dTcorrMap" , MICRO , TYPE_NEW_OPT ,
   "Add -Pmean option, based on poster I saw at HBM." ,
   NULL } ,

 { 17 , JUN , 2009 , RWC , "3dREMLfit" , MICRO , TYPE_GENERAL ,
   "OpenMP-ization" ,
   "Some speedup, but need to work on not doing malloc/free so much in the\n"
   "REML_func function!" } ,

 { 16 , JUN , 2009 , RWC , "1dplot" , MICRO , TYPE_NEW_OPT ,
   "Add '-ytran' option" ,
   "Apply an expression to the time series, to transform it prior to\n"
   "plotting.  To elide the use of 1deval." } ,

 { 15 , JUN , 2009 , RWC , "AlphaSim" , MICRO , TYPE_BUG_FIX ,
   "OpenMP: cdfnor() and other functions are not thread-safe" ,
   "Make use of cdfnor() 'critical'.  Longer term: should patch the nifti\n"
   "CDF functions to avoid static variables where possible." } ,

 { 11 , JUN , 2009 , RWC , "zgaussian" , MICRO , TYPE_BUG_FIX ,
   "'long' should be 'int' on 64 bit systems, when doing bit twiddling" ,
   NULL } ,

 { 11 , JUN , 2009 , RWC , "parser" , MICRO , TYPE_MODIFY ,
   "Added posval() function, and treat '[]' as '()' for clarity." ,
   NULL } ,

 { 11 , JUN , 2009 , RWC , "AlphaSim" , MINOR , TYPE_MODIFY ,
   "Added computation of analytic approximation of Alpha(i) for large i" ,
   "Uses a modified extreme value distribution, which looks pretty good." } ,

 { 9 , JUN , 2009 , RWC , "AlphaSim" , MICRO , TYPE_GENERAL ,
   "Modify to use OpenMP (parallelize across iterations)" ,
   NULL } ,

 { 3 , JUN , 2009 , RWC , "bbox.c" , MICRO , TYPE_MODIFY ,
   "Modify string list chooser to do Browse select callback via arrows" ,
   "To make consistent the ways of browsing thru the dataset choosers in\n"
   "AFNI." } ,

 { 1 , JUN , 2009 , RWC , "debugtrace.h" , MICRO , TYPE_MODIFY ,
   "Add ability to suspend/restore function traceback stack" ,
   "Disable stack when OpenMP parallel section is engaged." } ,

 { 26 , MAY , 2009 , RWC , "thd_1Ddset.c" , MICRO , TYPE_MODIFY ,
   "Change way names are used in output of .1D 'datasets'" ,
   "(a) If prefix starts with '-' or 'stdout', write results to stdout as a\n"
   "'pure' 1D formatted file (no NIML header),\n"
   "(b) Otherwise, if -prefix option had a directory name attached, use that\n"
   "instead of always using the current working directory." } ,

 { 22 , MAY , 2009 , RWC , "afni" , MICRO , TYPE_BUG_FIX ,
   "Check if im3d->fim_now is NULL in AFNI_func_overlay()" ,
   NULL } ,

 { 20 , MAY , 2009 , RWC , "realtime plugin" , MICRO , TYPE_NEW_ENV ,
   "Also modify it to limit the number of open controllers." ,
   "cf. AFNI_REALTIME_MAX_CONTROLLERS" } ,

 { 20 , MAY , 2009 , RWC , "realtime plugin" , MINOR , TYPE_MODIFY ,
   "Modified to allow realtime 3D registration on complex inputs" ,
   "Mostly a change in mri_3dalign.c, to take the movement estimation done\n"
   "on the magnitude image and apply it to the 2 component images." } ,

 { 18 , MAY , 2009 , RWC , "3dDespike" , MICRO , TYPE_GENERAL ,
   "Speedup by OpenMP" ,
   "Also required changes to mcw_malloc.c to mark various sections as\n"
   "'critical' and to cl1.c to remove 'static' from all variables generated\n"
   "from Fortran." } ,

 { 15 , MAY , 2009 , RWC , "afni InstaCorr" , MICRO , TYPE_GENERAL ,
   "SeedBlur + locked InstaCorr-ology" ,
   NULL } ,

 { 15 , MAY , 2009 , RWC , "afni InstaCorr" , MINOR , TYPE_GENERAL ,
   "Remove OpenMP; carry out setref on all locked controllers" ,
   NULL } ,

 { 13 , MAY , 2009 , RWC , "InstaCorr+3dBlurInMask" , MINOR , TYPE_GENERAL ,
   "Added OpenMP support for blurring to these programs" ,
   "Parallelization occurs across sub-bricks -- speedup is significant." } ,

 { 11 , MAY , 2009 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "InstaCorr changes" ,
   "* Save seed time series into the 1D timeseries library for graphicking\n"
   "* Modify progress printouts slightly" } ,

 { 8 , MAY , 2009 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "Instacorr updates" ,
   "* Can Write A_ICOR datasets using new allow_directwrite variable\n"
   "* Disable InstaCorr when switching sessions and views\n"
   "* Notify renderer (DRAWNOTICE) when changes made\n"
   "* Shift-Ctrl-Left-Click does crosshair shift + InstaCorr Set" } ,

 { 7 , MAY , 2009 , RWC , "afni" , MINOR , TYPE_GENERAL ,
   "FIxes/upgrades for InstaCorr" ,
   "* memory problem fixed by malloc-ing sizeof(float *)\n"
   "  -- not sizeof(float) -- for a float ** variable!\n"
   "* add dataset labels\n"
   "* add statistical parameters (including FDR)\n"
   "* add help" } ,

 { 6 , MAY , 2009 , RWC , "afni" , MAJOR , TYPE_NEW_OPT ,
   "First edition of InstaCorr!" ,
   "Rough around the edges, but gnarly-ific to the level of the first\n"
   "inaccessible cardinal!" } ,

 { 1 , MAY , 2009 , RWC , "3dBlurInMask" , MINOR , TYPE_NEW_PROG ,
   "Like 3dBlurToFWHM, but simpler." ,
   NULL } ,

 { 1 , MAY , 2009 , RWC , "thd_bandpass.c" , MICRO , TYPE_BUG_FIX ,
   "modified to make it actually work, including lowpass and highpass" ,
   NULL } ,

 { 30 , APR , 2009 , RWC , "1dBandpass" , MICRO , TYPE_NEW_PROG ,
   "for .. Bandpassing!" ,
   "Really just to test the new bandpassing functions for inclusion into\n"
   "AFNI's InstaCorr feature." } ,

 { 29 , APR , 2009 , RWC , "3dTcorrMap" , MICRO , TYPE_GENERAL ,
   "Add OpenMP support" ,
   NULL } ,

 { 29 , APR , 2009 , RWC , "3dTcorrMap" , MICRO , TYPE_GENERAL ,
   "unroll innermost loop -- makes little difference :-(" ,
   NULL } ,

 { 29 , APR , 2009 , RWC , "afni" , MINOR , TYPE_NEW_ENV ,
   "Make 'Where am I?' font size user controllable." ,
   "via AFNI_TTATLAS_FONTSIZE environment variable (editable)." } ,

 { 28 , APR , 2009 , RWC , "3dDeconvolve" , MINOR , TYPE_NEW_OPT ,
   "Add duration argument to SPMGx basis functions for deconvolution." ,
   "For Tracy Doty, apparently." } ,

 { 16 , APR , 2009 , RWC , "All" , MINOR , TYPE_BUG_FIX ,
   "Inadvertent recursion in afni_environ.c now blocked." ,
   NULL } ,

 { 15 , APR , 2009 , RWC , "1dplot" , MICRO , TYPE_NEW_OPT ,
   "Add -thick option" ,
   "Plus: modify plot_ts.c to thicken the lines used for plotting the labels\n"
   "(so the fancy characters are filled in a little)." } ,

 { 13 , APR , 2009 , RWC , "3dREMLfit" , MICRO , TYPE_BUG_FIX ,
   "fixed bug in printing censor message for -addbase and -slibase" ,
   NULL } ,

 { 10 , APR , 2009 , RWC , "3dREMLfit" , MINOR , TYPE_BUG_FIX ,
   "Error in processing -slibase file with censoring" ,
   "Typo in replacing input image with censored image caused the end of the\n"
   "world (SEGV crash)." } ,

 { 8 , APR , 2009 , RWC , "AFNI image viewer" , MINOR , TYPE_MODIFY ,
   "Set MPEG 'pattern' for frames to reflect the Anim_dup setting." ,
   "This small change will make MPEG-1 (.mpg) files that are slowed down by\n"
   "setting Anim_dup > 1 not significantly larger than full speed files, by\n"
   "setting the frame pattern to 'IPPPP' where the number of Ps is the\n"
   "number of duplicate frames (Anim_dup-1)." } ,

 { 8 , APR , 2009 , RWC , "thd_filestuff.c" , MINOR , TYPE_NEW_ENV ,
   "AFNI_ALLOW_ARBITRARY_FILENAMES" ,
   "Set this environment variable to YES to allow 'strange' characters into\n"
   "AFNI created filenames.  You should know what you are doing if you use\n"
   "this variable!" } ,

 { 6 , APR , 2009 , RWC , "3dLocalSVD" , MICRO , TYPE_NEW_OPT ,
   "Add -vproj option" ,
   "To project central voxel onto low-dimensional local SVD space." } ,

 { 6 , APR , 2009 , RWC , "3dmaskSVD" , MICRO , TYPE_NEW_OPT ,
   "Modify to allow output of more than 1 singular vector." ,
   "Also, make the help more helpful." } ,

 { 4 , APR , 2009 , RWC , "3dmaskSVD" , MINOR , TYPE_NEW_PROG ,
   "Like 3dmaskave, but does SVD principal vector instead of average" ,
   NULL } ,

 { 2 , APR , 2009 , RWC , "3dLocalSVD" , MICRO , TYPE_BUG_FIX ,
   "Fixed a little bug in 3dLocalSVD." ,
   NULL } ,

 { 2 , APR , 2009 , RWC , "3dTcorrMap" , MINOR , TYPE_NEW_OPT ,
   "Add -Thresh option" ,
   "To save a count of how many other voxels are above threshold correlated\n"
   "with each seed voxel." } ,

 { 1 , APR , 2009 , RWC , "realtime plugin" , MICRO , TYPE_GENERAL ,
   "Enhanced a few error messages." ,
   "I wish people would READ the damn things, instead of calling me to\n"
   "complain." } ,

 { 30 , MAR , 2009 , RWC , "3dABoverlap" , MINOR , TYPE_NEW_PROG ,
   "Computes various overlap and non-overlap statistics for 2 datasets." ,
   "Will resample dataset #B to match dataset #A, if needed.  This program\n"
   "is intended to check if two datasets are grossly not aligned, and has\n"
   "little other purpose." } ,

 { 27 , MAR , 2009 , RWC , "FDR calculations" , MINOR , TYPE_MODIFY ,
   "Changes/additions to mask operations for FDR curves." ,
   "3dREMLfit and 3dDeconvolve now generate an automask for the FDR curving\n"
   "purposes, if no other mask is used.  3drefit has a new -FDRmask option\n"
   "for computing the FDR curves correctly if no mask was used before." } ,

 { 26 , MAR , 2009 , RWC , "FDR" , MICRO , TYPE_MODIFY ,
   "Change the way m1 is estimated for MDF 'hint'." ,
   NULL } ,

 { 25 , MAR , 2009 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "Add -nobout option, to suppress baseline betas from -Rbeta/-Obeta." ,
   "Per the request of Michael S Beauchamp, University of Texas." } ,

 { 24 , MAR , 2009 , RWC , "3dDeconvolve" , MICRO , TYPE_GENERAL ,
   "Added -stim_times_subtract option" ,
   "To allow adjustment of stimulus times due to removal of some images at\n"
   "the start of each run.\n"
   "ALSO: added -stim_time_millisec option, to allow -stim_times inputs to\n"
   "be in milliseconds rather than seconds." } ,

 { 24 , MAR , 2009 , RWC , "3dDeconvolve" , MICRO , TYPE_GENERAL ,
   "Expand the -help output somewhat." ,
   "Based on feedback and confusion from the Dartmouth bootcamp." } ,

 { 11 , MAR , 2009 , RWC , "3dANOVA" , MINOR , TYPE_NEW_OPT ,
   "Add -mask option to 3dANOVA, 3dANOVA2, 3dANOVA3" ,
   NULL } ,

 { 9 , MAR , 2009 , RWC , "3dDeconvolve" , MICRO , TYPE_NEW_OPT ,
   "Add SPMG1 to -stim_times repertoire, and update help." ,
   NULL } ,

 { 6 , MAR , 2009 , RWC , "3dDeconvolve" , MINOR , TYPE_NEW_OPT ,
   "Add 'WAV' function (from waver) to -stim_times repertoire." ,
   NULL } ,

 { 4 , MAR , 2009 , RWC , "3ddata.h" , MICRO , TYPE_BUG_FIX ,
   "Fix usage of realpath() array dimension with RPMAX macro" ,
   NULL } ,

 { 2 , MAR , 2009 , RWC , "zfun" , MINOR , TYPE_GENERAL ,
   "add compression functions (using zlib) to libmri in zfun.c" ,
   NULL } ,

 { 13 , FEB , 2009 , RWC , "3dRBFdset" , MICRO , TYPE_GENERAL ,
   "Test program to make a dataset defined by RBF." ,
   "Mostly built to test the Radial Basis Function expansion functions in\n"
   "mri_rbfinterp.c." } ,

 { 11 , FEB , 2009 , RWC , "3dvolreg" , MINOR , TYPE_BUG_FIX ,
   "replace DMAT_svdrot_old with DMAT_svdrot_newer" ,
   "Old function fails sometimes, making the output rotation be garbage and\n"
   "producing junk image results; the newer one seems more robust." } ,

 { 10 , FEB , 2009 , RWC , "3dDeconvolve" , MICRO , TYPE_BUG_FIX ,
   "fixed premature mask free-ing bug" ,
   NULL } ,

 { 9 , FEB , 2009 , RWC , "imseq.c" , MINOR , TYPE_NEW_ENV ,
   "Add AFNI_ANIM_DUP environment variable." ,
   "Allows user to duplicate images when writting an animation (AGIF or\n"
   "MPEG) file.  A simple and stoopid way to slow down an MPEG." } ,

 { 5 , FEB , 2009 , RWC , "3dREMLfit" , MICRO , TYPE_BUG_FIX ,
   "typo ==> DOF params for Full_Fstat weren't in output dataset" ,
   NULL } ,

 { 2 , FEB , 2009 , RWC , "parser (calc programs)" , MINOR , TYPE_MODIFY ,
   "Add hrfbk4 and hrfbk5(t,T) functions to parser" ,
   "To imitate the BLOCK4 and BLOCK5 response functions in 3dDeconvolve." } ,

 { 8 , JAN , 2009 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "Added OpenMP directives as a test of multi-threading speedup." ,
   NULL } ,

 { 7 , JAN , 2009 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Added more references to the help page." ,
   NULL } ,

 { 5 , JAN , 2009 , RWC , "3dAllineate" , MICRO , TYPE_MODIFY ,
   "Change wsinc5 interpolation from radial to tensor product weight." ,
   "Speedup is about a factor of 6, which is worth the effort." } ,

 { 2 , JAN , 2009 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "Add '-final wsinc5' interpolation mode." ,
   "Slow but accurate.  Weight function is 'designed' to reduce the variance\n"
   "smoothing artifact." } ,

 { 31 , DEC , 2008 , RWC , "3dUndump" , MICRO , TYPE_MODIFY ,
   "Make sure NaN values don't get into the dataset!" ,
   NULL } ,

 { 30 , DEC , 2008 , RWC , "3dTfitter" , MINOR , TYPE_MODIFY ,
   "For FALTUNG, use sparse matrix operations for pure least squares." ,
   "Deconvolution + penalty matrix is sparse.  Uses the 'rcmat' functions\n"
   "originally developed for 3dREMLfit.  Speeds things up a lot when the\n"
   "time series is long." } ,

 { 29 , DEC , 2008 , RWC , "3dTfitter" , MICRO , TYPE_GENERAL ,
   "Added better error messages if program runs out of memory." ,
   NULL } ,

 { 23 , DEC , 2008 , RWC , "3dDeconvolve" , MICRO , TYPE_BUG_FIX ,
   "Program wouldn't read a -stim_times file that was all '*'" ,
   "Actual change was in mri_read.c." } ,

 { 19 , DEC , 2008 , RWC , "3dREMLfit" , MICRO , TYPE_MODIFY ,
   "Added condition number checking and -GOFORIT" ,
   "Also added floatscan checking for all output datasets, to be careful." } ,

 { 16 , DEC , 2008 , RWC , "3dREMLfit" , MICRO , TYPE_GENERAL ,
   "Fixed bug in linear solution when #columns%4==3 (unrolling)." ,
   "Actually in matrix.c, in function vector_multiply_transpose(), which is\n"
   "only  used in remla.c,  which is only used in 3dREMLfit.c." } ,

 { 11 , DEC , 2008 , RWC , "3dREMLfit" , MICRO , TYPE_MODIFY ,
   "Should behave better with ocllinear regression matrix." ,
   "Modified the QR decomposition to adjust 'tiny' diagonal elements of R,\n"
   "to avoid division by zero (or near-zero).  Prints a warning message when\n"
   "this adjustment is made." } ,

 { 11 , DEC , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_BUG_FIX ,
   "Fixed -slibase bug." ,
   "Oopsie.  -slibase didn't work properly with more than 1 added column per\n"
   "slice.  Also, per the request of ZSS, you can now input more than 1\n"
   "column set per image file, in repetitive slice order:\n"
   "  0 1 2 3 0 1 2 3 0 1 2 3\n"
   "if there are 4 slices in the dataset, for example." } ,

 { 9 , DEC , 2008 , RWC , "3dDeconvolve" , MINOR , TYPE_MODIFY ,
   "Added 'dmBLOCK' to the '-stim_times_IM' repertoire." ,
   NULL } ,

 { 8 , DEC , 2008 , RWC , "3dDeconvolve" , MICRO , TYPE_GENERAL ,
   "Expand -stim_times_AM modulation abilities even more." ,
   "Now allow 'duration modulation' via the 'dmBLOCK' response model.  A\n"
   "general facility for allowing up to 3 nonlinear function parameters has\n"
   "been built into the code, for future expansion.  'dmBLOCK' can also be\n"
   "amplitude modulated." } ,

 { 4 , DEC , 2008 , RWC , "1dMarry" , MINOR , TYPE_MODIFY ,
   "Allow multiple marriages, to go with 3dDeconvolve changes" ,
   NULL } ,

 { 4 , DEC , 2008 , RWC , "3dDeconvolve" , MINOR , TYPE_NEW_OPT ,
   "Extend -stim_times_AM2 option to allow multiple amplitudes" ,
   NULL } ,

 { 18 , NOV , 2008 , RWC , "3dANOVA programs" , MINOR , TYPE_NEW_OPT ,
   "Add option to output float-valued datasets." ,
   "Set the AFNI_FLOATIZE environment variable to YES, and the output of\n"
   "3dANOVA, 3dANOVA2, and 3dANOVA3 will be stored in float format instead\n"
   "of in scaled short format.  [Per the request of Paul Hamilton]" } ,

 { 14 , NOV , 2008 , RWC , "help_format" , MICRO , TYPE_NEW_PROG ,
   "For formatting -help output into Web pages with hyperlinks." ,
   "To be used with the dist_help script for making the help Web pages from\n"
   "the -help outputs of all AFNI programs." } ,

 { 11 , NOV , 2008 , RWC , "3dREMLfit" , MICRO , TYPE_GENERAL ,
   "-usetemp now also saves output dataset stuff to TMPDIR." ,
   NULL } ,

 { 10 , NOV , 2008 , RWC , "3dUndump" , MICRO , TYPE_MODIFY ,
   "Add warning if non-integer values are being saved to shorts/bytes." ,
   NULL } ,

 { 7 , NOV , 2008 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Write Clust_table.1D into dataset directory, not startup directory." ,
   NULL } ,

 { 6 , NOV , 2008 , RWC , "3dDeconvolve" , MINOR , TYPE_GENERAL ,
   "Add 'scale to shorts misfit' warning messages." ,
   "Also added to 3dcalc and some other programs." } ,

 { 5 , NOV , 2008 , RWC , "model_expr2.c" , MICRO , TYPE_GENERAL ,
   "Modified to allow up to 9 parameters -- see code for details." ,
   NULL } ,

 { 5 , NOV , 2008 , RWC , "3dREMLfit" , MICRO , TYPE_GENERAL ,
   "If it saves memory, convert dataset to new MRI_vectim format." ,
   "That is, an array of time series vectors, of voxels in the mask.  Will\n"
   "save memory if the number of voxels in the mask is less than 1/2 the\n"
   "volume." } ,

 { 4 , NOV , 2008 , RWC , "3dBlurToFWHM" , MINOR , TYPE_BUG_FIX ,
   "skip all zero sub-bricks in the blurmaster - for Tomski Rosski" ,
   NULL } ,

 { 4 , NOV , 2008 , RWC , "3dNLfim" , MINOR , TYPE_GENERAL ,
   "Added model_expr2.c" ,
   "Model that uses an arbitrary 3dcalc-like expression with 2 free\n"
   "parameters (any letters but 't', which is used for the time axis)." } ,

 { 3 , NOV , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "Several changes" ,
   "Finished -gltsym, -Rglt, and -Oglt options = add GLTs on the 3dREMLfit\n"
   "command line and output those exclusively to new files.\n"
   "\n"
   "Modified -addbase and -slibase to do censoring if input 1D files are the\n"
   "same length as the uncensored matrix.\n"
   "\n"
   "Also fixed bugs in -ABfile.  Oopsie." } ,

 { 28 , OCT , 2008 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "add '3' checkerboard (inverse stippling to '#' key)" ,
   NULL } ,

 { 27 , OCT , 2008 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Checkerboard underlay and overlay images" ,
   "For Ziad -- to help judge image alignment.  Use the # key to turn\n"
   "checkerboarding on and off.  The grayscale intensity bar popup menu has\n"
   "a new sub-menu to select the check size in units of underlay pixels.  At\n"
   "this time, checkerboarding does NOT work with image Save, RowGraphs, or\n"
   "SurfGraph, or just about any other feature.  If you want a picture of a\n"
   "checkerboarded image, you'll have to use a snapshot utility to grab the\n"
   "window." } ,

 { 23 , OCT , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "Added -rout option, by popular 'demand'" ,
   NULL } ,

 { 23 , OCT , 2008 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Add MDF estimate to FDR q-value" ,
   "MDF = Missed Detection Fraction = estimate of what fraction of true\n"
   "positives are below any given threshold (analogous to FDR = estimate of\n"
   "what fraction of above threshold voxels are true negatives).  Displays\n"
   "in the hint attached to the label below the threshold slider.  Purely\n"
   "experimental, since estimating the number of true positives in a given\n"
   "collection of p-values is not a well-defined concept by any means." } ,

 { 15 , OCT , 2008 , RWC , "fdrval" , MINOR , TYPE_NEW_PROG ,
   "Compute FDR value on command line, from dataset header" ,
   NULL } ,

 { 15 , OCT , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_BUG_FIX ,
   "Fixed errts (etc) outputs: censored values not set to zero!" ,
   NULL } ,

 { 14 , OCT , 2008 , RWC , "3dAllineate" , MINOR , TYPE_BUG_FIX ,
   "If source=scaled shorts, then output will be scaled as well." ,
   NULL } ,

 { 6 , OCT , 2008 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "FDR curves can now be fetched from warp_parent" ,
   "If a func dataset is missing FDR curves, then the program tries to get\n"
   "them from the warp_parent dataset.  Also, AFNI no longer allows you to\n"
   "add FDR curves to a dataset without actual bricks (warp-on-demand)." } ,

 { 30 , SEP , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "added -gltsym option" ,
   "Makes it easy for the user to add GLTs without using 3dDeconvolve." } ,

 { 25 , SEP , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "added -usetemp option" ,
   "Saves REML setup matrices for various cases to tmp disk files.  Is\n"
   "necessary for -slibase and -Grid 5 combined, if anyone ever actually\n"
   "wants to run such a case." } ,

 { 24 , SEP , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "-addbase and -slibase options to add baseline columns to matrix" ,
   "In particular, -slibase is intended for per-slice modeling of\n"
   "physiological noise effects.  Sucks up a lot of memory and CPU time." } ,

 { 23 , SEP , 2008 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "save last jumpto_xyz string, etc." ,
   NULL } ,

 { 22 , SEP , 2008 , RWC , "3dREMLfit" , MICRO , TYPE_GENERAL ,
   "got rid of some big memory leaks" ,
   NULL } ,

 { 19 , SEP , 2008 , RWC , "NIML library" , MICRO , TYPE_GENERAL ,
   "modify NI_alter_veclen to allow conversion to non-empty from empty" ,
   NULL } ,

 { 18 , SEP , 2008 , RWC , "Vecwarp" , MICRO , TYPE_BUG_FIX ,
   "Only require +orig dataset if user actually NEEDS it" ,
   "Program required the +orig version of the -apar dataset, which is needed\n"
   "for SureFit work, even if it wasn't actually going to be used.  Not any\n"
   "more." } ,

 { 16 , SEP , 2008 , RWC , "3dDeconvolve" , MICRO , TYPE_MODIFY ,
   "Made 3dREMLfit command echo more complete for user's convenience" ,
   NULL } ,

 { 15 , SEP , 2008 , RWC , "Draw Dataset plugin" , MINOR , TYPE_MODIFY ,
   "Keystrokes F3 and F3 now decrement/increment drawing value in plugin" ,
   NULL } ,

 { 10 , SEP , 2008 , RWC , "3dTfitter" , MICRO , TYPE_MODIFY ,
   "skip all zero voxels; add voxel ID to error messages" ,
   NULL } ,

 { 9 , SEP , 2008 , RWC , "3dAllineate" , MICRO , TYPE_MODIFY ,
   "add savehist to allcost output" ,
   NULL } ,

 { 2 , SEP , 2008 , RWC , "3dAllineate" , MICRO , TYPE_NEW_OPT ,
   "-allcostX1D option (for Chairman Z)" ,
   NULL } ,

 { 29 , AUG , 2008 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "More small changes, to speed the program up a little" ,
   "* reduce the number of function evals used in the coarse refinements\n"
   "* after coarse refinements, cast out parameter sets that are very close\n"
   "  to the best set, to avoid duplicative work at the fine pass" } ,

 { 28 , AUG , 2008 , RWC , "miscellaneous" , MICRO , TYPE_BUG_FIX ,
   "Fixed 'is used uninitialized' errors in several codes." ,
   "Via the new macro ZZME() in 3ddata.h, which zeros out a struct." } ,

 { 28 , AUG , 2008 , RWC , "3dAllineate" , MAJOR , TYPE_MODIFY ,
   "A number of changes to improve robustness." ,
   "* Don't smooth noise added to source image outside of the mask\n"
   "* Reduce default smoothing level for -lpc in coarse pass\n"
   "* Increase number of points used for matching in the coarse pass\n"
   "* More refinements of the twobest results in the coarse pass\n"
   "* Refinements (-num_rtb option) of the twobest results in the fine pass\n"
   "All this adds CPU time, but seems to make the program more reliably\n"
   "convergent.  Also:\n"
   "* Restored operation of the -check option, to restart the optimization\n"
   "  at the final solution with other methods, to see what results they\n"
   "  give compared to the original method." } ,

 { 26 , AUG , 2008 , RWC , "3dTcorrMap" , MINOR , TYPE_NEW_PROG ,
   "Average correlations with every other voxel time series." ,
   "Kind of slow.  For Kyle Simmons.  And I still don't recognize Missouri!" } ,

 { 25 , AUG , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "Added residual outputs to 3dREMLfit." ,
   NULL } ,

 { 22 , AUG , 2008 , RWC , "3dREMLfit" , MINOR , TYPE_NEW_OPT ,
   "Add FDR curves and -?fitts options." ,
   NULL } ,

 { 21 , AUG , 2008 , RWC , "3dREMLfit" , SUPER , TYPE_NEW_PROG ,
   "Program to mimic 3dDeconvolve, but with serial correlations" ,
   "Uses ARMA(1,1) model of noise, separately for each voxel." } ,

 { 18 , AUG , 2008 , RWC , "3dDeconvolve" , MICRO , TYPE_GENERAL ,
   "add -force_TR option to override input dataset TR from header" ,
   "Also added a printout of the dataset TR if the PSFB warning gets\n"
   "printed." } ,

 { 15 , JUL , 2008 , RWC , "count" , MICRO , TYPE_BUG_FIX ,
   "Make '-suffix' work correctly for last item (per Fred Tam)." ,
   "Also, make '-sep' and '-suffix' work as separate items,\n"
   "instead of as synonyms for the same thing, which is stoopid." } ,

 { 14 , JUL , 2008 , RWC , "3dDeconvolve" , MICRO , TYPE_MODIFY ,
   "Add 'RunStart' field to xmat.1D output, to indicate start of runs." ,
   NULL } ,

 { 3 , JUL , 2008 , RWC , "matrix.c" , MICRO , TYPE_MODIFY ,
   "Add QR function matrix_qrr() to matrix.c library file." ,
   NULL } ,

 { 24 , JUN , 2008 , RWC , "afni" , MINOR , TYPE_MODIFY ,
   "Extend max number of clusters reportable, for Shruti." ,
   NULL } ,

 { 10 , JUN , 2008 , RWC , "3dLocalCormat" , MINOR , TYPE_NEW_OPT ,
   "Add -ARMA option to estimate ARMA(1,1) parameters" ,
   NULL } ,

 { 6 , JUN , 2008 , RWC , "3dErrtsCormat" , MINOR , TYPE_NEW_PROG ,
   "Compute correlation matrix of a time series, globally." ,
   NULL } ,

 { 6 , JUN , 2008 , RWC , "3dLocalCormat" , MINOR , TYPE_NEW_PROG ,
   "Compute correlation matrix of a time series, averaged locally." ,
   NULL } ,

 { 3 , JUN , 2008 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Made AFNI_IMAGE_TICK_DIV_IN_MM editable (in 'Edit Environment')" ,
   "This is Rick's method for putting a physical scale around the edge of an\n"
   "image.  Of course, you have to use the grayscale intensity bar popup\n"
   "menu to actually put the tick marks on.  This just converts the\n"
   "'division' count to mm." } ,

 { 21 , MAY , 2008 , RWC , "afni" , MICRO , TYPE_BUG_FIX ,
   "Put -1dindex into '3dclust' output from Clusterize report.  Oops." ,
   NULL } ,

 { 20 , MAY , 2008 , RWC , "plugout_drive" , MICRO , TYPE_NEW_OPT ,
   "Make TCP/IP to 'localhost' the default.  Add option '-shm' if needed." ,
   NULL } ,

 { 20 , MAY , 2008 , RWC , "afni_cluster.c" , MICRO , TYPE_GENERAL ,
   "Added BHelp to a bunch of buttons." ,
   NULL } ,

 { 20 , MAY , 2008 , RWC , "afni" , MINOR , TYPE_GENERAL ,
   "Clusterize: '-' as the filename means write to stdout." ,
   NULL } ,

 { 15 , MAY , 2008 , RWC , "thd_initdblk.c" , MICRO , TYPE_NEW_ENV ,
   "AFNI_IGNORE_BRICK_FLTFAC = YES means ignore brick factors on input" ,
   "This is a quick hack for Ziad, and must be used with care!  Example:\n"
   "  3dBrickStat -DAFNI_IGNORE_BRICK_FLTFAC=YES -max -slow fred+orig" } ,

 { 9 , MAY , 2008 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Modify PUTENV macro to malloc new string for each variable" ,
   "Previously used an array str[256] that would go away, and that's\n"
   "actually not legal in Linux -- the array must be permanent, since its\n"
   "pointer is what gets put in the environment, not a copy of the string. \n"
   "That's why the PUTENV didn't work on Linux (but worked for some reason\n"
   "on Mac OS X)!  Sheesh." } ,

 { 8 , MAY , 2008 , RWC , "ROI drawing plugin" , MICRO , TYPE_GENERAL ,
   "Change info label to show BRIK filename rather than dataset prefix" ,
   NULL } ,

 { 8 , MAY , 2008 , RWC , "edt_dsetitems.c" , MICRO , TYPE_GENERAL ,
   "new prefix *.hdr gets a 2-file NIfTI format output" ,
   NULL } ,

 { 8 , MAY , 2008 , RWC , "3dAllineate" , MICRO , TYPE_GENERAL ,
   "small changes to bilinear optimization parameters" ,
   NULL } ,

 { 8 , MAY , 2008 , RWC , "afni" , MICRO , TYPE_GENERAL ,
   "Add 'u' or 'o' marker in titlebar to indicate what's the grayscale" ,
   NULL } ,

 { 2 , MAY , 2008 , RWC , "mcw_glob.c" , MICRO , TYPE_GENERAL ,
   "added message from Graham Wideman when readdir() fails" ,
   NULL } ,

 { 1 , MAY , 2008 , RWC , "afni_cluster.c" , MICRO , TYPE_GENERAL ,
   "Add SaveMask button to Clusterize report window." ,
   "Saves the clusters as a mask dataset (cluster #1 has value=1, etc.). \n"
   "Uses the prefix in the text field for the dataset name -- will overwrite\n"
   "existing dataset if the same name is used twice.  Equivalent 3dclust\n"
   "command is saved in History Note in output mask dataset (as if you had\n"
   "pressed the '3dclust' button)." } ,

 { 30 , APR , 2008 , RWC , "mcw_glob.c" , MINOR , TYPE_NEW_ENV ,
   "setenv AFNI_SHELL_GLOB YES == wildcard expansion via the shell" ,
   "For Graham Wideman and Mac OS X Server 10.5 problems." } ,

 { 8 , APR , 2008 , RWC , "3dTfitter" , MICRO , TYPE_BUG_FIX ,
   "Deal with ref (LHS) vectors that are very tiny." ,
   "Modify thd_fitter.c so that ref vectors that are very tiny compared to\n"
   "the largest one will not be included in the regression.  Per the\n"
   "unreasonable request of Rasmus Birn." } ,

 { 8 , APR , 2008 , RWC , "3dAllineate" , MICRO , TYPE_BUG_FIX ,
   "Weighting in LPC cost function" ,
   "Realized that weighting in computing the LPC was only done on the blok\n"
   "level computation of the PC.  All the blok PC values were averaged\n"
   "together without weighting.  Modified code to use sum of weights over a\n"
   "blok as a weight for its PC.  This can be turned off by setting\n"
   "environment variable AFNI_LPC_UNWTBLOK to YES (to restore the LPC\n"
   "function to its previous behavior)." } ,

 { 8 , APR , 2008 , RWC , "3dUpsample" , MINOR , TYPE_NEW_PROG ,
   "Upsample a 3D+time dataset (in the time direction)" ,
   NULL } ,

 { 7 , APR , 2008 , RWC , "1dUpsample" , MINOR , TYPE_NEW_PROG ,
   "Upsample a 1D time series" ,
   NULL } ,

 { 4 , APR , 2008 , RWC , "3dAllineate" , MINOR , TYPE_GENERAL ,
   "Added '-nwarp bilinear' option" ,
   "Experimental nonlinear warping comes to 3dAllineate at last. \n"
   "Preliminary test looks good, but more work is needed to be sure it's OK." } ,

 { 4 , APR , 2008 , RWC , "3dDeconvolve" , MICRO , TYPE_GENERAL ,
   "Check for ' ' option, which means a blank after a '\\' character" ,
   NULL } ,

 { 28 , MAR , 2008 , RWC , "3dcalc" , MICRO , TYPE_MODIFY ,
   "Treat auto-transposed .1D\\' files as datasets, not as timeseries." ,
   NULL } ,

 { 26 , MAR , 2008 , RWC , "1dplot" , MINOR , TYPE_NEW_OPT ,
   "Add -norm2 and -normx options: normalize time series before plotting" ,
   NULL } ,

 { 25 , MAR , 2008 , RWC , "3dDetrend" , MINOR , TYPE_BUG_FIX ,
   "-normalize didn't work -- what the hellll was I thinking?" ,
   "Also, added some help to explain how to use 3dDetrend on 1D files." } ,

 { 25 , MAR , 2008 , RWC , "1dnorm" , MINOR , TYPE_BUG_FIX ,
   "Forgot the sqrt()!  kudos to David Perlman." ,
   NULL } ,

 { 24 , MAR , 2008 , RWC , "3dTfitter" , MINOR , TYPE_MODIFY ,
   "deconvolution with penalty factor = 0 is modified" ,
   "Now the criterion for choosing the penalty factor is based on curvature\n"
   "rather than distance from the origin.  Seems to be more robust, but\n"
   "probably will require yet more tweaking." } ,

 { 21 , MAR , 2008 , RWC , "3dTfitter" , MICRO , TYPE_MODIFY ,
   "Modified operation of FALTUNG penalty=0 option" ,
   "Implemented the L-curve method for selecting the penalty factor, when\n"
   "user sets the factor to 0." } ,

 { 20 , MAR , 2008 , RWC , "3dTfitter" , MINOR , TYPE_NEW_OPT ,
   "Add -polort option, to keep Gang Chen happy." ,
   NULL } ,

 { 14 , MAR , 2008 , RWC , "3dROIstats" , MICRO , TYPE_NEW_OPT ,
   "Sub-brick label printing (and -nobriklab option)" ,
   "Per the noble Vince Hradil." } ,

 { 7 , MAR , 2008 , RWC , "Dataset#N" , MINOR , TYPE_NEW_ENV ,
   "AFNI_DATASETN_NMAX sets number of datasets allowed" ,
   "New environment variable AFNI_DATASETN_NMAX sets the number of datasets\n"
   "allowed in Dataset#N plugin, from 9..49.  This is for Shruti." } ,

 { 6 , MAR , 2008 , RWC , "3dcalc" , MICRO , TYPE_MODIFY ,
   "Add cbrt (cube root) function to parser; affects 1deval and ccalc" ,
   NULL } ,

 { 5 , MAR , 2008 , RWC , "afni-general" , MINOR , TYPE_MODIFY ,
   "Change the way 1D datasets are written to disk from 3D programs" ,
   "In programs that analyze time series files (such as 3dTfitter), you can\n"
   "input 1D files and make the column direction be the time axis by using\n"
   "suffixing the file with \\' -- but when it comes to writing the results\n"
   "dataset out, the standard AFNI I/O method is to write the time axis\n"
   "along the row direction.  With this change, if you input a 1D file in\n"
   "the place of a 3D dataset AND put '.1D' at the end of the output file\n"
   "prefix, then the output dataset will be written so that the time axis\n"
   "is along the column direction." } ,

 { 5 , MAR , 2008 , RWC , "3dTfitter" , MINOR , TYPE_NEW_OPT ,
   "added '-fitts' option to produce fitted time series dataset" ,
   NULL } ,

 { 5 , MAR , 2008 , RWC , "afni" , MICRO , TYPE_MODIFY ,
   "Added 'AFNI History' button to the Datamode->Misc menu" ,
   NULL } ,

 { 4 , MAR , 2008 , RWC , "3dDeconvolve" , MINOR , TYPE_BUG_FIX ,
   "couple of small changes to help the hapless users" ,
   "* add more informative error message if allocate_memory() fails\n"
   "* force '-float' if any output prefix is NIfTI-1 format (.nii)" } ,

 { 3 , MAR , 2008 , RWC , "++AFNI_History++ plugin" , MICRO , TYPE_NEW_OPT ,
   "Small changes to make onscreen wordwrap match printout wordwrap" ,
   NULL } ,

 { 1 , MAR , 2008 , RWC , "++AFNI_History++ plugin" , MICRO , TYPE_NEW_PROG ,
   "Provides a way to create and insert entries into afni_history_NAME.c" ,
   "User must set two environment variables:\n"
   " AFNI_HISTORY_PERSONAL_FILE = full path to your personal version of\n"
   "   afni_history_NAME.c\n"
   " AFNI_HISTORY_USERNAME = what you want for the username in your file\n"
   "My values of these are\n"
   " AFNI_HISTORY_PERSONAL_FILE = /Users/rwcox/AFNI/src/afni_history_rwcox.c\n"
   " AFNI_HISTORY_USERNAME      = RWC\n"
   "You also need to add one of two lines to your afni_history_NAME.c file:\n"
   "\n"
   "/*=====below THIS LINE=====*/\n"
   "or\n"
   "/*=====above THIS LINE=====*/\n"
   "\n"
   "as shown, with no blanks before or after on the same line (except that\n"
   "you must make 'BELOW' or 'ABOVE' all caps!).\n"
   "New history entries are placed below the 'BELOW' line (if present), or\n"
   "above the 'ABOVE' line.\n"
   "\n"
   "If you set AFNI_HISTORY_DONTSAVE to YES, then the afni_history_NAME.c\n"
   "file won't be edited, and the new entry is just written to stdout." } ,

 { 20 , NOV , 2007 , RWC , "3dTsort" , MAJOR , TYPE_NEW_PROG ,
   "new program = sorts voxel data along the time axis" ,
   "This might be useful for sorting the -stim_time_IM beta weights\n"
   "output be 3dDeconvolve.  Perhaps for something else, too?" } ,

 { 16 , FEB , 2008 , RWC , "3dTfitter" , SUPER , TYPE_NEW_PROG ,
   "new program = linear fits to voxel time series" ,
   "Uses L1 or L2 regression, with optional constraints to fit each voxel\n"
   "time series as a sum of basis time series, which can be 1D files or\n"
   "3D+time datasets.  Basis time series that are 1D time series are\n"
   "the same for all input voxels.  Basis time series that are 3D+time\n"
   "datasets are different for each voxel.\n"
   "Differences from 3dDeconvolve:\n"
   "* Basis time series can vary across voxels.\n"
   "* Fit coefficients can be found with L1 or L2 error functions, and\n"
   "  can be constrained to be positive or negative.\n"
   "* 3dTfitter does not compute goodness-of-fit statistics.\n" } ,

 { 20 , FEB , 2008 , RWC , "1deval" , MINOR , TYPE_NEW_OPT ,
   "add '-1D:' option, to write output that is usable on the command line" ,
   "Sample usage:\n"
   " 1dplot `1deval -1D: -num 71 -expr 'cos(t/2)*exp(-t/19)'`\n"
   "The backquotes `...` capture command's output and put this string on\n"
   "the command line.  The '-1D:' option formats the 1deval output so that\n"
   "it is ready to be used in this way.\n" } ,

 { 22 , FEB , 2008 , RWC , "3dpc" , MINOR , TYPE_NEW_OPT ,
   "add '-eigonly' and '-reduce' options; output eigenvalues to a 1D file"  ,
   "'-eigonly' causes 3dpc to print eigenvalues to stdout and stop there.\n"
   "'-reduce n pp' outputs a reduced dataset, using only the largest 'n'\n"
   "eigenvalues.\n" } ,

 { 25 , FEB , 2008 , RWC , "1dsvd" , MINOR , TYPE_NEW_OPT ,
   "add '-vmean' and '-vnorm' options, to mirror capabilities in 3dpc" ,
   NULL } ,

 { 27 , FEB , 2008 , RWC , "3dTfitter" , MAJOR , TYPE_NEW_OPT ,
   "add deconvolution via the '-FALTUNG' option" ,
   "Unlike 3dDeconvolve, this deconvolution is to find the input time\n"
   "series, given the impulse response function.\n" } ,

 { 28 , FEB , 2008 , RWC , "3dUndump" , MINOR , TYPE_GENERAL ,
   "allow input of NO text files, to produce an 'empty' dataset" ,
   NULL } ,

 { 28 , FEB , 2008 , RWC , "3dTfitter" , MINOR , TYPE_NEW_OPT ,
   "allow constraints on sign of deconvolved function" , NULL } ,

 { 28 , FEB , 2008 , RWC , "3dTfitter" , MINOR , TYPE_NEW_OPT ,
   "allow combination of penalty functions in deconvolution" , NULL } ,

 { 29 , FEB , 2008 , RWC , "afni_history" , MICRO , TYPE_GENERAL ,
   "with HTML output, put a rule between different dates" , NULL } ,

/*=====ABOVE THIS LINE=====*/
 /*-------------------------------------------------------------------------*/
 /*---------- The stuff below was converted from AFNI.changes.cox ----------*/

 { 24,JUL,1996, RWC, "Miscellaneous", SUPERDUPER, TYPE_GENERAL, "Oldest History stuff" ,
   "AFNI was created in summer 1994 (but some parts date to the 1980s).\n"
   "However, no formal log was made of changes until this date in 1996.\n"
   "So this is the beginning of AFNI historiography.\n"
   "  'Lately it occurs to me: What a long, strange trip it's been.'\n"
   } ,

 { 25,JUL,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added Button 2 click (time_index += 1 or -= 1) to afni_graph.c\n"
   "   [N.B.: this change was later removed with the drawing plugin.]\n"
   },

  { 29,JUL,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added shadow color changing code to 'colormenu' widgets in bbox.c --\n"
   "   this gives a little visual feedback when a color is selected.\n"
   "* Used 'glob' routines from tcsh-6.05 to allow filename globbing\n"
   "   on non-POSIX systems without the glob routines (like SGIs).\n"
   },

  { 30,JUL,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified MCW_file_expand routine to properly glob files specified\n"
   "   in the form 3D:a:b:c:d:e:fname, where only 'fname' will have the\n"
   "   wildcards.  To3d help printout now reflects this change.\n"
   "* Used fsync(fileno()) to ensure that writes of .BRIK files are\n"
   "   flushed to disk -- in afni_func.c and 3ddata.c.\n"
   "* Don't do shadow color changing in 'colormenus' unless the widget is\n"
   "   realized, since this causes BadDrawable error on Linux (Motif 2.0).\n"
   "* Changed most popup widgets to be of class 'menu', which means that\n"
   "   their background color can now be changed separately.\n"
   "* Changed operation of -R option in afni to limit levels of recursion.\n"
   "   Mostly involved changing the routine THD_get_all_subdirs in 3ddata.c.\n"
   },

  { 31,JUL,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed to3d to prevent creation of 3D+time functional datasets.\n"
   "   Modified to3d -help to reflect this, and added 'Nz = %d' to the\n"
   "   3D+time information label in the to3d widget panel.\n"
   },

  { 1,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c and afni.c to allow the user to toggle between\n"
   "   periodic montages and 'no wrap' montages.  Added a toggle button\n"
   "   'Wrap' next to the crosshair 'Gap' menu.\n"
   "* Modified crosshairs in afni.c so that in 'Single' mode with a\n"
   "   grapher active, then only the frame showing the graphed voxels\n"
   "   is drawn.  In 'Multi' mode, both the frame and the crosshairs\n"
   "   will be shown.\n"
   },

  { 2,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified FD2.c to show average pixel value over frame as well as time,\n"
   "   when 'AvIm' is being used.   Also added 'b' keypress to toggle\n"
   "   common baselines for graphs.\n"
   "* Added SEEK_ constants back to mri_read.c, since C compiler on sparky\n"
   "   doesn't have them in stdio.h.\n"
   "* Fixed 'afni -im' problem with inversion of top_form during waits --\n"
   "   the program didn't turn the inversion off correctly.  This error\n"
   "   was due to the 'dangling else' problem.  The addition of a {}\n"
   "   pair fixed it right up.  Moral of the story: don't be stupid.\n"
   },

  { 6,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in afni_slice.c about the new NN code.  The code\n"
   "   now doesn't use the 'parallel' fast mode unless both the\n"
   "   inner and outer axes are parallel.\n"
   "* Fixed a bug in 3ddata.c, where the taxis wasn't properly set on\n"
   "   input of a 3D+t dataset if no slice offset data was input.\n"
   "   This would cause a crash when trying to delete a dataset.\n"
   "* Added '-warp_4D' switch to afni to allow output of 3D+t datasets\n"
   "   in Talairach coordinate.  Consumes huge amounts of disk space\n"
   "   and CPU time.\n"
   "* Removed fsync() because of time penalty.\n"
   },

  { 7,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed another bug in afni_slice.c about the new NN code.  The\n"
   "   allocation macro MAKE_IBIG failed to take into account that\n"
   "   the array ib[] index would run from 0..'top', and it only\n"
   "   allocated 'top' entries, whereas it should do at least 'top+1'.\n"
   },

  { 8,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-gfisher' option to 3dmerge.c, for purpose of averaging\n"
   "   correlation coefficient bricks.  Fixed a bug in this program\n"
   "   that took the DSET_BRICK_FACTOR from the input dataset\n"
   "   before editing, which is a mistake, since editing might alter\n"
   "   this scaling factor.\n"
   "* Changed output format from %14.7g to %13.6g in 3dinfo.c.  This\n"
   "   tends to suppress the annoying roundoff error in the scaled\n"
   "   statistics report.\n"
   },

  { 9,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed scaling bug in editvol.c EDIT_one_dataset's clip code.\n"
   "   For shorts scaled to floats, clip values were computed as\n"
   "   integers, which gave truncation errors in some cases.  Now,\n"
   "   clip values are computed as floats, then converted to\n"
   "   integers, with appropriate min- and max-ing.\n"
   "* Also added -1uclip and -2uclip options to EDIT_.  See\n"
   "   '3dmerge -help' for information.\n"
   },

  { 13,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Altered autoRange and userRange controls (in Define Function\n"
   "   control panel) in afni to\n"
   "     a) always allow the user to set the range, even for thresholds\n"
   "     b) eliminate the inversion to indicate the autoRange is on\n"
   "     c) compute the autoRange for thresholds as well as fims\n"
   "   These changes also eliminate a bug (feature?) where the user could\n"
   "   set 'Func=Threshold' (SHOWFUNC_THR), which would formerly disable\n"
   "   these controls, then switch to another dataset where they would\n"
   "   not properly be re-enabled.\n"
   "* Added function AFNI_rescan_session to afni_func.c, which will close\n"
   "   all the datasets in a session, then re-read the session directory\n"
   "   to make a new set of datasets.  At present, this is actuated from\n"
   "   the 'hidden' popup menu.  Tricky points are catching all pointers\n"
   "   to datasets that are in the rescanned session, since they will\n"
   "   change, and dealing with the case when the user deletes some\n"
   "   dataset files.\n"
   },

  { 28,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed routine T3D_fix_dataset_dimen() in to3d.c to clear the\n"
   "   data brick pointers of the OLD number of bricks, not the\n"
   "   NEW number of bricks.  This error caused to3d to crash when\n"
   "   going from a dataset type with large nvals to small nvals,\n"
   "   since some sub-brick pointers were not being properly cleared.\n"
   "   [This error only manifested itself on SGI machines, and\n"
   "    was found by Christopher Wiggins.]\n"
   "* Made all routines in mri_write.c return a count of the number\n"
   "   of files they successfully wrote out (instead of returning void,\n"
   "   as before).  [This change was prompted by Doug Ward.]\n"
   },

  { 29,AUG,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* If a session directory has functions but no anatomies, then\n"
   "   afni.c now prints out a warning message instead of just\n"
   "   skipping it silently.  [Prompted by Chris Wiggins.]\n"
   "* If a dataset has parent IDCODEs, then the corresponding\n"
   "   parent name will not be set in 3ddata.c.  This is to prevent\n"
   "   confusion.\n"
   },

  { 1,SEP,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved rescan pushbutton from hidden menu to datamode control panel.\n"
   "* Modified 3dmerge.c to allow merger of thresholds in datasets as\n"
   "   well as intensities.  Uses a new '-t*' type of flag -- the only\n"
   "   one implemented so far is '-tfico', which requires all inputs\n"
   "   to be of the fico dataset type.  See the '-gfisher' merger mode\n"
   "   given earlier.\n"
   },

  { 7,SEP,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified -tfico option in 3dmerge.c to allow some of the datasets\n"
   "   to be of the fith type.  If all of them are fico, then the output\n"
   "   will be fico (with degrees-of-freedom parameters summed), otherwise\n"
   "   the output will just be fith.\n"
   "* Added '-q' == 'be quiet' option to fim2.\n"
   },

  { 30,SEP,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* After several false starts, finally begin creation of plugin code.\n"
   "   This is after conversations with Mike Beauchamp and Jay Kummer.\n"
   "   Yesterday and today\n"
   "     - afni_plugin.h has interface structures defined;\n"
   "     - afni_plugin.c has interface definition routines and\n"
   "         widget creation routines;\n"
   "     - machdep.h has new #define's to set the type of\n"
   "         dynamic library loading to use.\n"
   "   Much more to come.\n"
   "* Modified MCW_widget_geom in xutil.h to properly deal with\n"
   "   the case of unrealized widgets -- must use XtQueryGeometry\n"
   "   instead of XtGetValues.\n"
   },

  { 6,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed use of '==' in assignment statement in 3ddata.h.\n"
   "* afni_plugin.c now has\n"
   "   - working widget creation and popup routines\n"
   "   - plugin callable routines to extract values from\n"
   "       user selected options from the interface\n"
   },

  { 7,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed 3ddata.c to allow use of AFNI_TS_PATH as an alternate\n"
   "   to AFNI_TSPATH.  If neither environment variable exists,\n"
   "   then the path './' will be used as a last resort.\n"
   "* Something similar done in afni_plug.c with AFNI_PLUGIN_PATH.\n"
   "* Made the switchview callback in afni.c pop down the strlist\n"
   "   chooser.  This is because a plugin interface will only be\n"
   "   allowed to choose from current view datasets, and if such\n"
   "   a view switch is made, the list of choosable datasets must\n"
   "   be modified.  The simplest way to do this is to make the\n"
   "   user start the choice process over.\n"
   },

  { 9,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in afni_plugin.c that failed to check certain\n"
   "   datasets for inclusion in the dataset choosers.\n"
   "* Modified BHelp to include color and border controls.\n"
   "* Incorporated Doug Ward's changed version of editvol.[hc],\n"
   "   which adds various cluster editing and volume filtering\n"
   "   options to 3dmerge.c (et al.).\n"
   },

  { 11,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed (sort of) sizing problem in afni_plugin.c creation\n"
   "   of plugin interface windows.\n"
   "* Wrote routine for plugin to load a new dataset into the\n"
   "   current session of a controller window.\n"
   },

  { 12,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* afni_plugin.c changes\n"
   "   - Modified PLUGIN_prefix_ok in afni_plugin.c to check for\n"
   "       duplicate prefixes, as well as for illegal characters in\n"
   "       the prefix string.\n"
   "   - Routine PLUGIN_force_redisplay will make all im3d units\n"
   "       redraw their windows.\n"
   "   - Routine PLUGIN_copy_dset will copy a dataset, including\n"
   "       the bricks.\n"
   "   - Added helpstring to the plugin interface, and a 'Help'\n"
   "       button to popup a plugin-supplied message.\n"
   "* Modified afni to allow separate control of warp-on-demand for\n"
   "   anatomy and functional datasets.  When a plugin directly\n"
   "   modifies a Talairach dataset brick, for example, then if it\n"
   "   is set to warp-on-demand, the display won't be affected,\n"
   "   since the program always warps from the +orig brick.  Under\n"
   "   the old scheme, if the anat were w-o-d, then the func had\n"
   "   to be w-o-d as well.  In the situation where the anat dataset\n"
   "   brick does not exist in Talairach coords, then the effect of\n"
   "   the plugin would be invisible if the user couldn't force\n"
   "   the function to be view-brick independent of the anatomy.\n"
   "* Fixed an old bug in THD_dset_in_sessionlist (3ddata.c) that\n"
   "   returned the wrong session index.\n"
   },

  { 14,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3ddata.h macro FILECODE_TO_PREFIX so that the\n"
   "   last '+' in the filecode is used to find the prefix,\n"
   "   rather than the first '+'.  This fixes a problem with\n"
   "   datasets whose prefix contains a '+' character.\n"
   },

  { 18,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c so that '# line' comments in pgm files\n"
   "   can be read.  This lets AFNI programs read pgm files created\n"
   "   from programs like 'xv'.\n"
   "* Changed plugin callable functions/macros in afni_plugin.[ch]\n"
   "   to start with PLUTO (PLugin UTility Operation).\n"
   },

  { 20,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bugs in imseq.c\n"
   "   - During 'image processing' of complex images, one reference to\n"
   "       input image 'im' had not been changed to 'lim' (the locally\n"
   "       processed copy).\n"
   "   - If ISQ_make_image fails, the XImage 'seq->given_xim' would\n"
   "       be NULL.  This is now detected, and ISQ_show_image won't\n"
   "       try to put this NULL image to the screen (which would\n"
   "       cause a Segmentation error).\n"
   "* Minor changes to afni_plugin.c\n"
   "   - Added reminder of current 'view' at top of plugin dataset chooser.\n"
   "   - Added [c] reminder of current controller in plugin shell widget\n"
   "       titlebar and icon label strings.\n"
   "* Minor changes to afni_graph.c\n"
   "   - Changed time increment event from Button2 to Shift or Ctrl\n"
   "       with Button1.  This is to allow the eventual dedication of\n"
   "       Button2 events to plugins.\n"
   },

  { 21,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed FD_brick_to_timeseries (in 3ddata.c) to scale each element\n"
   "   by the brick_fac value appropriate for that time index, rather\n"
   "   than by the value for time index = 0.  This is done using the\n"
   "   new routine mri_mult_to_float (in mri_to_float.c).\n"
   "* Fixed bug in EDIT_dset_items (editvol.h) that used 'float' inputs.\n"
   "   Default type promotion (there can be no prototype here) turns all\n"
   "   float inputs to doubles, so they must be retrieved this way.\n"
   "   Also, fixed error when 'ADN_nsl' is passed as zero -- no longer\n"
   "   requires a 'ADN_toff_sl' array in this special case.  Also made\n"
   "   EDERR macro always print a message, even if not in debug mode.\n"
   "* Added DSET_TIMESTEP macro (and others) to 3ddata.h.\n"
   "* Modified PLUTO_add_dset (afni_plugin.c) to allow for other actions\n"
   "   when a dataset is set back to AFNI.\n"
   "* Added 'progress meter' PLUTO_ functions to afni_plugin.c; also\n"
   "   modified the meter code in xutil.c to check if the percent\n"
   "   value has changed before trying to update the meter progress.\n"
   "* Added 'units_type' to the 3D+time dataset format.  This lets the\n"
   "   'time axis' be expressed in milliseconds, seconds, or Hertz.\n"
   "   Changes were made to 3ddata.[ch], to3d.c, 3dinfo.c, editvol.[ch].\n"
   "* Power spectrum plugin 'plug_power.c' was made to work today.\n"
   },

  { 22,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added timeseries chooser to afni_plugin.c.\n"
   "* Added ability to apply a function to graph data (e.g., to take the\n"
   "   logarithm of each point).  This affected afni.[ch], afni_graph.[ch].\n"
   "* Fixed a bug in afni_fimmer.c, where routine AFNI_ts_in_library could\n"
   "   return the wrong value if the timeseries being searched for was\n"
   "   not actually found.\n"
   "* Modified directory scan in 3ddata.c (for timeseries) and afni_plugin.c\n"
   "   (for plugins) to skip directories that have already be scanned.\n"
   "   This is to avoid the situation where the PATH variable contains\n"
   "   duplicate entries.\n"
   },

  { 23,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added Shift/Ctrl/Alt-Button3 simulation of lower tier button presses\n"
   "   to imseq.c.\n"
   },

  { 25,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed imseq.c routine that re-places the 'dialog' window (for Disp\n"
   "   and Mont) on the screen when the image is resized -- somehow\n"
   "   the code that fixed the problem of forcing the dialog off the\n"
   "   edge of the screen lost an '=', and so nothing happened.\n"
   "* Added 'i' button to right edge of imseq.c windows -- allows the\n"
   "   user to down/up the fraction of the window that the image\n"
   "   takes up.\n"
   },

  { 27,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added PLUTO_popup_image() function to afni_plugin.c.  Also used\n"
   "   this to provide a 'hidden menu' popup of an image of me in\n"
   "   afni_func.c.\n"
   },

  { 30,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to apply function to each pixel of a displayed image\n"
   "   (cognate to the similar ability in graphs -- see 22 October).\n"
   "   This primarily affected imseq.c, but also a few other places.\n"
   "* Added new 'fift' dataset type, to deal with F-test sub-bricks.\n"
   "   - Incorporated cdflib into mrilib.  This is to use the 'cdff'\n"
   "       to compute the CDF for the F statistic, in mri_stats.c\n"
   "   - Changed the precision of the threshold scale (thr_scale)\n"
   "       from 0..99 to 0..999, and correspondingly changed the\n"
   "       scaling from the scale to func_threshold from 0.01 to\n"
   "       0.001.  Also changed the 'decim' factor for the scale.\n"
   },

  { 31,OCT,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified EDIT_substitute_brick in editvol.c to allow the input\n"
   "   array 'fim' to be NULL.  In that case, the routine will create it.\n"
   "   This is a convenience for the user (say, a plugin author).\n"
   },

  { 1,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routine PLUTO_4D_to_typed_fim to afni_plugin.c.  This takes\n"
   "   a user function and a 3D+time dataset, and returns fim dataset,\n"
   "   with the user function applied to each voxel timeseries.\n"
   },

  { 2,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a major bug with the multiple controller window ('New'\n"
   "   button) extension.  The problem is that the information about\n"
   "   how to extract images from a dataset is stored in the dataset,\n"
   "   in the 'wod_daxes' and 'vox_warp' sub-structs.  This causes\n"
   "   difficulties when two controllers view the same dataset at\n"
   "   different resolutions (e.g., overlay the same function on\n"
   "   two different anatomies).  The fix is to put the originals\n"
   "   of 'wod_daxes', 'vox_warp', and 'wod_flag' into the controller\n"
   "   (im3d) struct itself.  When the dataset is going to be used,\n"
   "   then this information is copied into the dataset struct.\n"
   "   This is a clumsy fix, but breaks the least code in the\n"
   "   afni_warp.c routines for extracting slices from a dataset.\n"
   },

  { 3,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved functional resample arrowval from the 'Define Function'\n"
   "   control panel to the 'Define Datamode' panel, where it will\n"
   "   be close to the analogous control from anatomy.\n"
   "* Added 1D and 2D transformation function registries.  Made up\n"
   "   some sample transformation functions (median filtering, etc.).\n"
   "* Added time units appendage to TR in to3d.c.\n"
   },

  { 4,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Lock' menu to Datamode panel.  This allows the user to\n"
   "   specify that certain controllers have their coordinates locked\n"
   "   together, so that jumping around in one dataset can be mirrored\n"
   "   by jumps in another.  At this time, the AFNI_transform_vector\n"
   "   function does not properly transform vectors from +tlrc coordinates\n"
   "   to +orig coordinates (say) if the two datasets are not in the\n"
   "   parent/child relationship.  This can be confusing if two such\n"
   "   datasets are locked, and they are not in the same 'view'.\n"
   "* Made pressing Button1 on the AFNI logo in a grapher window also\n"
   "   turn off/on the menubar widgets.  This enables a screen dump\n"
   "   of a graph without that extraneous stuff being present.\n"
   },

  { 6,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -unique option to afni.c to allow creation of unique\n"
   "   display contexts (MCW_DC's) for each AFNI controller window.\n"
   "   (This won't work on 8 bit displays.)  afni.c and afni_widg.c\n"
   "   were changed appropriately (and afni.h).\n"
   },

  { 10,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Wrote 'lsqfit' plugin.  This allows computation of least squares\n"
   "   fits to timeseries graphs.  Modified afni_graph.c to allow\n"
   "   'Double Plot' to overlay least squares fit graph (or other\n"
   "   output of a 'Tran 1D') on the data timeseries graph.\n"
   },

  { 12,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug with multiple displays and the 'lock' -- when\n"
   "   changing the dataset in an AFNI controller, you don't want\n"
   "   the coordinate lock to apply.\n"
   "* Started adding 'orts' to AFNI's interactive fimmery.\n"
   },

  { 19,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made afni.c (AFNI_setup_viewing routine) set the FIM-able dataset\n"
   "   to the newly activated anatomical, if possible.  In the old\n"
   "   version, if you switched anatomies, the FIM-able dataset didn't\n"
   "   switch with you.  This was confusing to the uninitiated masses\n"
   "   (that is to say, Mike Beauchamp).\n"
   },

  { 21,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated the f2c (Fortran-to-C) convertor into the AFNI96\n"
   "   distribution, in a separate directory (f2cdir).  This allows\n"
   "   the use of the old 'parser.f' routines to compile and execute\n"
   "   user created expressions.\n"
   "* Added macro AVOPT_columnize to bbox.h, which allows the setup\n"
   "   of an optmenu in a multicolumn layout.  Also setup the 'Plugins'\n"
   "   button to allow this (when the number of plugins grows past 20).\n"
   },

  { 22,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Adapted MCW_choose_strlist (in bbox.c) to allow selection of\n"
   "   multiple items from the list.\n"
   },

  { 23,NOV,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Rearranged Write buttons on Datamode control panel in AFNI.  Added\n"
   "   a write 'Many' button which lets the user pick lots of datasets\n"
   "   to write out, so he can go get a cup of coffee.  Put all 3 Write\n"
   "   buttons in a single row.\n"
   "* Added a Rescan All button to allow rescanning of all sessions.  Put\n"
   "   both Rescan buttons in a single row.  Also added a '*.1D' rescan\n"
   "   button to allow for re-reading of timeseries files.\n"
   "* Attached data type descriptors like [fim] and [epan:3D+t] to the\n"
   "   listings in the dataset choosers.\n"
   },

  { 10,DEC,1996 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed small bugs in parser_int.c, in the utility routines where\n"
   "   a pointer to a doublereal wasn't dereferenced before comparing\n"
   "   it to zero.\n"
   },

  { 1,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routines to libmri.a to allow reading in a 3D: image file\n"
   "   in 'delay' mode -- only the filename and offset into it are\n"
   "   stored in the image.  When access to the image is desired,\n"
   "   then it will be read from disk.\n"
   "* Added -delay switch to to3d.c to take advantage of this procedure.\n"
   "   This prevents duplicate malloc required for huge 3D: file\n"
   "   (once for the 3D: input and once for the dataset brick).  People\n"
   "   who do all 3D+time input data in one big file have run out of\n"
   "   memory otherwise.\n"
   "* Added '++' toggle to allow display of crosshairs in all slices of\n"
   "   an AFNI montage.  This is specifically for Jeff Binder.\n"
   "* Added RESET_AFNI_QUIT() calls to a bunch of routines in afni.c.\n"
   },

  { 2,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mcw_glob.c usage to FD2.c, to allow internal expansion of\n"
   "   wildcarded filename.  This overcomes the SGI limit on the\n"
   "   number of arguments on the command line.\n"
   },

  { 3,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Got program waver.c to work -- generation of an ideal waveform\n"
   "   with tunable parameters.\n"
   },

  { 13,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -subort option to fim2.c -- subtraction of orts from\n"
   "   an image time series.\n"
   },

  { 20,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Included '#include <string.h>' in mcw_glob.h, to prevent complaints\n"
   "   from stupid compilers.\n"
   "* Added Makefile.osf1, from a system at U Maryland.\n"
   "* Added gmovie, script to create a GIF movie from a bunch of PNM files.\n"
   },

  { 21,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made the 'fscale' element in a grapher (afni_graph.[hc]) be a float,\n"
   "   so that finer control over graph scaling is possible.\n"
   "* Changed 'Scale', 'Ignore', and 'Matrix' controls in graph window\n"
   "   to be optmenus.  Added a routine to allow optmenus to be refitted.\n"
   },

  { 22,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made the control buttons on image viewers (imseq.c) a little smaller.\n"
   },

  { 30,JAN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dcalc.c.\n"
   "* Changed STEP function in parser.f to be 1 only for x > 0.\n"
   },

  { 14,FEB,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated GNU malloc into afni.c, which will be enabled with\n"
   "   the #define-ition of USE_GNU_MALLOC (e.g., in machdep.h).\n"
   "* #define-ing NO_FRIVOLITIES in machdep.h will no disable the\n"
   "   picture and sonnets.\n"
   },

  { 16,FEB,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated GNU malloc checking into 'dafni'.  Now checks the\n"
   "   status of all malloc-ed blocks on entry and exit to every\n"
   "   routine using 'ENTRY' and 'RETURN' macros.  (Nevertheless,\n"
   "   this still doesn't find the bug I'm looking for.)\n"
   "* When a grapher window is being destroyed, its corresponding\n"
   "   image window needs to redraw the crosshairs.  This redraw\n"
   "   command could cause a crash (for reasons unknown) when\n"
   "   the dataset is being changed (that is, the grapher is being\n"
   "   trashed because the new dataset does not support graphs).\n"
   "   This is fixed in afni.c and afni_func.c so that when a\n"
   "   grapher is destroyed due to underlay switching, then the\n"
   "   corresponding image redraw commands will be ignored.\n"
   },

  { 18,FEB,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added some logical functions (and, or, mofn) to parser.f, and\n"
   "   so to 3dcalc.  Also added the -session option, and made the\n"
   "   default session = './'.\n"
   },

  { 20,FEB,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved a couple routines in afni_plugin.c outside the\n"
   "   '#ifdef ALLOW_PLUGINS ... #endif' code block since they\n"
   "   are used in non-plugin-specific parts of AFNI.\n"
   },

  { 23,FEB,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Wrote plug_imreg.c to do 2D registration on 3D+time datasets.\n"
   "* Modified mri_align.c, imreg.c, and fim2.c to recognize macro\n"
   "   ALLOW_DFTIME, if the user wants to compile the ability to\n"
   "   do -dftime or -dfspacetime registration.\n"
   },

  { 3,MAR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Allow to3d to create 3D+time FIM datasets.  Allow afni to display\n"
   "   them as functional overlays.\n"
   "* Add -noplugins option to afni, so that it is possible to skip\n"
   "   plugins initialization (also can setenv AFNI_NOPLUGINS).\n"
   "* In to3d.c, if any -[xyz]SLAB or -[xyz]FOV options are used, then\n"
   "   require that all 3 axes be given if the dataset is to be\n"
   "   written out correctly without opening the interactive window.\n"
   },

  { 4,MAR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a 'Read Sess' button to allow input of a new session into\n"
   "   afni.  Also added a function THD_equiv_files to 3ddata.c to\n"
   "   allow for checking if 2 filenames are equivalent (point to\n"
   "   the same inode on the same disk).\n"
   },

  { 5,MAR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a dummy dataset to afni.c, so that if there are none when\n"
   "   the user starts, he can read them in with the 'Read Sess' button.\n"
   "* Added a 'Read 1D' button to allow input of timeseries files.\n"
   },

  { 10,MAR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made the 'Selection' box in the new file selectors have the\n"
   "   'HOTCOLOR' as the background.  This is because of the importance\n"
   "   of the contents of this box.\n"
   },

  { 20,MAR,1997, RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Created script files to compile and distribute AFNI binaries\n"
   "   on and to various computers.\n"
   },

  { 2,APR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated the CLAMS library into the internal (MCW only) version\n"
   "   of AFNI.\n"
   },

  { 3,APR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Created the 'imcalc' program, analogous to 3dcalc, but for 2D images.\n"
   },

  { 21,APR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Started work on 'plug_realtime.c', including the '3T_toafni.c'\n"
   "   program to extract data from ParaVision and send it into AFNI.\n"
   },

  { 22,APR,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified registered functions to each have an int flag.  The only\n"
   "   flag value used now is bit 0 = RETURNS_STRING.  This can be used\n"
   "   in a 1D function (via PLUTO_register_1D_funcstr) to return an\n"
   "   extra string that will be displayed in graph's button 3 popup.\n"
   "* Modified the LSQfit plugin to return the fit parameters in the\n"
   "   extra string, so that the user can display them.\n"
   },

  { 17,JUN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Lots of changes in the last 2 months to make plug_realtime.c work.\n"
   "* Added a menu item to afni_graph.c to allow user to control graph\n"
   "   colors and line thicknesses.  The colors can also be initialized\n"
   "   from the .Xdefaults file.\n"
   "* Added a menu item to afni_graph.c to allow the graph window to\n"
   "   be dumped to an image file.  Had to fix xim.c to allow for\n"
   "   the XImage format returned by XGetImage on the graph Pixmap.\n"
   "* Modified imseq.c so that if the user types '.pnm' as the end\n"
   "   of the 'Save:one' filename, the program won't add another\n"
   "   '.pnm' to the end.\n"
   },

  { 18,JUN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Had to fix the xim.c routine XImage_to_mri to work correctly\n"
   "   with 12 bit Visuals.\n"
   "* Added stuff so that .Xdefaults can initialize the line thicknesses\n"
   "   as well as the colors.\n"
   },

  { 25,JUN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made afni_plugout.c and plugout_tt.c -- initial implementations\n"
   "   of the 'plugout' concept: programs that talk to AFNI using\n"
   "   IOCHANs to get T-T coordinates, etc.\n"
   "* Modified iochan.c to allow a socket connection to cutoff\n"
   "   abruptly when closed.  This was needed because I found that\n"
   "   a socket connection might hang around a while after close,\n"
   "   and this would prevent new connections on the same port #.\n"
   "   At present, this capability is only used when afni exits.\n"
   "* The plugout code (afni_plugout.c) is an Xt work process.\n"
   "   To prevent it soaking up too much CPU time, if nothing\n"
   "   happens, it will sleep for 10 msec.  This work process\n"
   "   is disabled if the realtime plugin is active.\n"
   },

  { 30,JUN,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routine THD_extract_series and macros PLUTO_extract_series*\n"
   "   to get time series from 3D+time datasets.  [per Ziad Saad]\n"
   "* Modified 3ddup.c to allow conversion of 3D+time anatomy into\n"
   "   3D+time fim.  This will allow the overlay of EPI time series\n"
   "   as 'function' onto the corresponding anatomy.\n"
   },

  { 2,JUL,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imstat.c to work with 3D image files, at least partly.\n"
   },

  { 16,JUL,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made -orient option (and AFNI_ORIENT environment) work to change\n"
   "   the order in which coordinates are displayed in the afni.c\n"
   "   crosshair label.\n"
   },

  { 22,JUL,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* -orient (and AFNI_ORIENT) now work with 3dclust.c.\n"
   "* The 'jump to' popup in afni.c now works with the orientation\n"
   "   code, so that you can paste coordinates out of 3dclust\n"
   "   into to jumpto window.\n"
   },

  { 23,JUL,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 6 new functional dataset types, with thresholds being\n"
   "   distributed as follows\n"
   "          normal    chi-square   incomplete-beta\n"
   "          binomial  gamma        Poisson\n"
   "* Gave user ability to change range of threshold slider -- before,\n"
   "   range was fixed by threshold functional dataset type.\n"
   "* Found problem on Linux (1.2.13) with 'dlopen' loading of plugins\n"
   "   and models -- seems to have problems at about the 20th library.\n"
   "   Not sure what to do about this.\n"
   "* Added routine PLUTO_report to allow a plugin to report status\n"
   "   information at startup.\n"
   },

  { 28,JUL,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added new utility program '3drefit.c' to allow user to change\n"
   "   the axes stored in a dataset header.  This is mostly to\n"
   "   fixup errors that may have occured at to3d time.\n"
   "* Added -orient option to to3d.c (not that it is much use).\n"
   "* Fixed bug in 3dinfo.c, for printout of sub-brick scaling\n"
   "   factors when no statistics are present in the header.\n"
   },

  { 30,JUL,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to include a rotation warp when using 3ddup.\n"
   "* Added ability to include empty markers in 3drefit.\n"
   "* Fixed AFNI_init_warp (in afni.c) where it give the name\n"
   "   of the new dataset based on the 'adam' dataset, rather\n"
   "   than the 'parent'.  This causes problems when transforming\n"
   "   a dataset that is itself warp-on-demand from 3ddup -- the\n"
   "   names would be based on the ultimate warp parent, not\n"
   "   the derived parent from 3ddup.\n"
   },

  { 1,AUG,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a label in the Define Function control panel to show\n"
   "   the crosshair pixel value, if all 3 image windows are\n"
   "   open (afni.c, afni_func.c, afni_widg.c).\n"
   "* Made Button3+Modifiers work even if CapsLock or NumLock\n"
   "   is pressed (imseq.c).\n"
   "* Added random Shakespearean insults.\n"
   "* Added AFNI_SESSTRAIL (3ddata.c) to control session 'lastname'.\n"
   },

  { 22,AUG,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Raoqiong Tong modified parser.f to make the vector evaluation\n"
   "   routine work again.\n"
   "* Also fixed 3dcalc.c to work with 3D+time datasets.\n"
   },

  { 26,AUG,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops!  Had to fix the EXP_0D plugin in plug_lsqfit.c because\n"
   "   the parser vector evaluation routine was changed.\n"
   },

  { 3,OCT,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops!!  Fixed a bug in mri_align.c, where the fine fit weight\n"
   "   image wasn't computed properly.  This affected fim2, imreg,\n"
   "   and plug_imreg.\n"
   },

  { 22,OCT,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops**2.  Fixed a bug in the bug fix above.\n"
   },

  { 27,OCT,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed csfft to csfft_cox in all places to avoid conflict\n"
   "   with code by AJ.\n"
   },

  { 30,OCT,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed DXY_THRESH to PHI_THRESH in one line of mri_align.c\n"
   "* Worked on adding popup 'hints' to AFNI.\n"
   },

  { 10,NOV,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added popup hints to to3d.\n"
   "* Added 'Clinical' disclaimer to startup message.\n"
   "* Remove scale hints (afni.c, imseq.c), since they are particularly\n"
   "   obnoxious.\n"
   },

  { 12,NOV,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added some SPARKY specific header declarations to fix problems\n"
   "   with SunOS compiling system (e.g., strtod).\n"
   },

  { 20,NOV,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Increased buffer sizes in count.c\n"
   "* Added Makefile.sparc5_2.5 for Suns.\n"
   },

  { 21,NOV,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Allowed brick dimensions to be specified as '120A' or 'A120'\n"
   "   in to3d.c.  This is to make it consistent with the way\n"
   "   scanners print on films.\n"
   },

  { 30,NOV,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the 'bucket' types (anat and func) to 3ddata.h, and\n"
   "   then modified afni, editvol, etc., to utilize this type\n"
   "   of dataset.\n"
   "* Created program 3dbucket.c to put buckets together out of\n"
   "   sub-bricks from other datasets.\n"
   "* Modified 3drefit.c to allow changes to bucket sub-brick\n"
   "   information.\n"
   },

  { 9,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a new chooser to let user pick interpolation method\n"
   "   for threshold (statistical) sub-bricks.\n"
   "* Fixed CUBIC_CLIP in afni_warp.c -- it was incorrectly\n"
   "   placed before the brick_fac scaling, not after.\n"
   "* Removed FIM menu from Define Function control panel.\n"
   },

  { 13,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added new file '3dmaker.c', for routines that make new\n"
   "   3D datasets from other datasets.  Moved the guts of\n"
   "   the PLUTO_4D_to_typed_* routines into this file.\n"
   "* Also fixed a 'float *' inside mallocs to be 'float'\n"
   "   in a couple of places in these routines.  This should\n"
   "   be innocuous, since most CPUs satisfy\n"
   "     sizeof(float) <= sizeof(float *)\n"
   },

  { 15,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Compute FIM+' to FIM menu.  This saves the best\n"
   "   time series index in a new sub-brick.\n"
   "* Added some macros to editvol.h to make changing sub-brick\n"
   "   auxiliary values simpler.\n"
   },

  { 17,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_set_viewpoint to skip graph redraw unless\n"
   "   REDISPLAY_ALL or unless a new (x,y,z) location is set.\n"
   "* Added menu to the pbar label to allow modification of\n"
   "   the colors and spacings.\n"
   "* Modified display.h to allocate overlay colors from\n"
   "   read-write cells.  This allows the colors to be\n"
   "   redefined.\n"
   },

  { 18,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -noqual option to AFNI to make it skip quality checks\n"
   "   when doing marker transformations.\n"
   "* Added -view option to 3drefit to let it change coordinate\n"
   "   systems.\n"
   },

  { 21,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to read in palettes and colors from .afnirc file,\n"
   "   and interactively from a user-specified file.  Also allow\n"
   "   user to reset the top and spacings on the color pbar.\n"
   "* Modified display.[ch] to put all overlay stuff into a shared\n"
   "   struct for all MCW_DCs.  This means that the -unique option\n"
   "   will only generate new grayscale colorcells for each controller,\n"
   "   but they will now share overlay colors.  This is intended to\n"
   "   prevent a problem when users add new colors dynamically\n"
   "   from external palette files.  This change affected files\n"
   "     afni_func.c afni_graph.c afni_graph.h afni_setup.c xim.c\n"
   "     afni_widg.c bbox.c display.c display.h imseq.c pbar.c\n"
   "* Modified pbar.c to keep the input pval's exactly when calling\n"
   "   alter_MCW_pbar -- formerly, after resizing the panes, the\n"
   "   values might be altered slightly.\n"
   "* Discovered that 17,DEC,97 change to AFNI_set_viewpoint could\n"
   "   make graph not be drawn correctly on startup.  Added\n"
   "   'never_drawn' variable to graphs and imseqs to allow\n"
   "   this condition to be detected.\n"
   },

  { 22,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed auto-change of threshold scale for bucket datasets\n"
   "   from afni.c.  Also unmanages/remanages this scale when\n"
   "   the pbar is being changed, due to horrible visual effects\n"
   "   on the SGI machine nimloth (if FIX_SCALE_SIZE_PROBLEM\n"
   "   is defined).\n"
   "* Modified pbar.c to store the summed pane heights, and then\n"
   "   recompute the value/label only when such a summed height\n"
   "   changes.  This prevents the modification of the value/label\n"
   "   pairs at other panes when resizing only one pane.\n"
   "* Modified AFNI_bucket_label_CB (afni_func.c) to put the\n"
   "   sub-brick index at the left of the option menu label.\n"
   },

  { 26,DEC,1997 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in palette write-out (afni_setup.c).\n"
   "* Added a scrollable text output utility (xutil.c).  Made\n"
   "   the plugin help use it if the help string has many\n"
   "   lines (afni_plugin.c).  Added a line counting routine\n"
   "   to 3ddata.c\n"
   },

  { 2,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to read compressed .BRIK files to 3ddata.c\n"
   "   (gzip -d, bzip2 -d, and uncompress are supported).\n"
   "* Added program '3dnoise' to zero out noise-like regions\n"
   "   of nonnegative short datasets.\n"
   "* Modified display of dataset names to include number of\n"
   "   points in a 3D+t dataset, and to include a 'z' flag\n"
   "   to indicate compression of dataset.\n"
   },

  { 5,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'New Stuff' entry to Misc menu, which just pops up\n"
   "   the README.changes file into a readonly text window.\n"
   "* Program 'quotize.c' will take a text file and make it\n"
   "   into a C array of strings, one line per element.  This\n"
   "   is used in afni_func.c to popup the README.changes file,\n"
   "   which is put into the file 'newstuff.hhh' by quotize and\n"
   "   in the Makefile.INCLUDE.\n"
   },

  { 7,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a routine to editvol.c to do local averaging of a\n"
   "   dataset faster than -1filter_mean -- it seems to run\n"
   "   about 6 times faster.  This is implemented as\n"
   "   -1filter_aver in 3dmerge, so that the older one is\n"
   "   still available.  Also modified plug_edit.c to add\n"
   "   this option to the Filter menu.\n"
   },

  { 8,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified scaling for output of 3dmerge.c so that the program\n"
   "   detects if all the inputs are integer types (byte or short,\n"
   "   unscaled).  If they are, and the merger type is consistent\n"
   "   with integer type, then the output will be unscaled, otherwise\n"
   "   it will be scaled by a float.\n"
   },

  { 9,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.[ch] to allow choice of a timeseries to\n"
   "   be used to define the x-axis graphing.  Thus, if x(t) is\n"
   "   the x-axis timeseries, and yij(t) are the data timeseries,\n"
   "   then each graph ij now shows x(t) vs. yij(t) rather than\n"
   "   t vs. yij(t).\n"
   "* Also modified the Button 1 press in the central graph to\n"
   "   jump to the time index point whose graph point is closest\n"
   "   to the button click.\n"
   "* Also allowed data plots to be shown as points instead of\n"
   "   lines.  'Points' are drawn using the '_circle' routine\n"
   "   ('filled' or 'hollow', as the line is 'thick' or 'thin').\n"
   },

  { 12,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Allow a gap between the sub-graph boxes.  This is chosen\n"
   "   from the 'Colors etc.' menu.\n"
   "* Raoqiong Tong fixed a bug in the new parser.f with the\n"
   "   AND, OR, and MOFN functions.\n"
   },

  { 14,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified symbol for current time index in afni_graph.c, so\n"
   "   that when data is graphed with points the current point\n"
   "   can be distinguished from the graph points.\n"
   },

  { 16,JAN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Percent Change' calculation to afni_fimmer.c, and\n"
   "   removed the 'real-time' update feature from those routines.\n"
   },

  { 1,FEB,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3ddata.c to avoid use of 'scandir' routine, which caused\n"
   "   no end of trouble on Solaris.\n"
   "* Moved the 'mcw_glob' routines into libmri.a.  These are used to\n"
   "   get filenames from the directory now.\n"
   },

  { 2,FEB,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a typo in plug_imreg.c to make the 'Fine Blur' parameter\n"
   "   scale to FWHM properly.\n"
   "* Broke 3ddata.c into 'thd_*.c' routines, and incorporated them\n"
   "   into libmri.a.  Also incorporated 3dmaker.c and iochan.c.\n"
   "   Files 3ddata.c, 3dmaker.[ch], and iochan.[ch] are no more!\n"
   },

  { 3,FEB,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Also put editvol.c into 'edt_*.c' routines, and thence into\n"
   "   librmi.a\n"
   "* Added an 'Autoscale' button to graph Opt menu, and also execute\n"
   "   the autoscale code when the graph is 1st drawn.\n"
   },

  { 4,FEB,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified dbtrace.h to enable tracing with usual afni run.  This\n"
   "   can be turned on/off using the 'Misc' menu, or the '-trace'\n"
   "   option (if the program is compiled with USE_TRACING).\n"
   },

  { 8,FEB,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c to display dataset indices in graph window,\n"
   "   rather than FD_brick indices.  (See the DONT_MANGLE_XYZ location.)\n"
   "* Modified imseq.[ch], afni_graph.[ch], afni.[ch], and the new\n"
   "   file afni_receive.c to allow transmission of mouse Button2\n"
   "   data to a calling routine.  Work is in progress -- more later.\n"
   },

  { 13,FEB,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Released drawing plugin to MCW users.  Includes open and closed\n"
   "   curves, filling, and undo.\n"
   "* Also added .BRIK output compression to thd_compress.[ch], and to\n"
   "   thd_writedblk.c.\n"
   },

  { 16,MAR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added internal globbing to 'afni -im'.\n"
   "* Modified function AFNI_follower_dataset (afni_func.c) to copy\n"
   "   datablock auxiliary data from the data parent, if available.\n"
   "* Modified 3drefit.c to allow -fbuc and -abuc to work no matter\n"
   "   how many sub-bricks are present.\n"
   "* Added program 3dmaskave.c to print out averages from dataset\n"
   "   sub-bricks, with ROI selected by a mask.\n"
   },

  { 18,MAR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made 'ptr' a byte variable rather than char in XImage_to_mri\n"
   "   (xim.c) -- this seems to fix a problem on some machines.\n"
   },

  { 20,MAR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed EDIT_add_bricklist -- the brick auxiliary data additions\n"
   "   would free data that hadn't been allocated.\n"
   "* Modified stuff so that Dummy dataset is not deleted -- this\n"
   "   seems to help.  (It only takes up 64K, so the loss is small.)\n"
   },

  { 21,MAR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmaskave to allow dumping of all voxels hit by the\n"
   "   mask, and to compute the standard deviation also.\n"
   },

  { 24,MAR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_copy.c to allow creation of a 1 sub-brick zero-filled\n"
   "   'copy' -- useful for making mask datasets.\n"
   "* Modified 3dnoise.c for 3D+time datasets, so that a voxel is zeroed\n"
   "   only if a majority of time points at that location fall below\n"
   "   the cutoff.\n"
   "* Modified plug_drawdset.c to recompute statistics after each edit,\n"
   "   no matter what.  Also changed the help message a little.\n"
   "* Wrote plug_maskave.c to do pretty much the same thing as 3dmaskave.c.\n"
   },

  { 17,APR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dcalc.c to allow input of *.1D time series\n"
   "   in place of datasets.\n"
   },

  { 25,APR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_*.c to store byte order (for short and int dsets) in the\n"
   "   .HEAD file.  Then when the file is read in, it will be byte swapped\n"
   "   if necessary (which will force it to be malloc-ed, not mmap-ed).\n"
   "* Also modified 3drefit to allow a byte order to be written to .HEAD\n"
   "   files.  Note that this does not affect the .BRIK file.\n"
   "* Added new environment variable AFNI_BYTEORDER to control byte order\n"
   "   of output datasets.  This can be 'LSB_FIRST' or 'MSB_FIRST'.  If not\n"
   "   present, this means to use the native order of the CPU.\n"
   "* Added enviroment variable 'AFNI_NOMMAP' to allow suppression of mmap.\n"
   "   If the value is 'YES', then all datasets will be malloc-ed.\n"
   "* Modified the 'Purge Memory' button to purge ALL datasets from memory.\n"
   "   Formerly, it only purged the unused ones.\n"
   },

  { 29,APR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* At the behest of Ted DeYoe, modified 3dcalc.c to allow operations\n"
   "   on bucket and other multi-brick datasets that aren't 3D+time.\n"
   "* Also modified 3drefit.c to allow conversion of 3D+time into a bucket.\n"
   "* This also required the ability to erase datablock attributes, since\n"
   "   they are persistent -- even if the data in the dataset is voided,\n"
   "   the attribute will remain to be written to disk.  So a routine\n"
   "   THD_erase_one_atr was added.  In files thd_writedset.c and\n"
   "   thd_writdblk.c, every attribute that DOESN'T get written now gets\n"
   "   erased.  This will still leave extra attributes (perhaps added by\n"
   "   a plugin) being persistent, but avoids attribute 'hangover' problem.\n"
   },

  { 30,APR,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dcalc.c to purge dset sub-bricks when finished with them,\n"
   "   and to allocate output buffer sub-bricks only when needed.  This is\n"
   "   to keep memory usage down when using compressed 3D+time datasets.\n"
   "* Also added the -verbose option to 3dcalc.c.\n"
   },

  { 1,MAY,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed plug_rename.c to work with compressed datasets.  Added a\n"
   "   routine COMPRESS_add_suffix to thd_compress.c to facilitate this.\n"
   },

  { 4,MAY,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routine THD_purge_one_brick to thd_purgedblk.c, to allow\n"
   "   freeing of one sub-brick from a malloc-ed dataset.  Also\n"
   "   defined macro DSET_unload_one(dset,iv).\n"
   },

  { 3,JUN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_make_descendants to allow descendancy to flow\n"
   "   from +acpc datasets, not just from +orig datasets.\n"
   "   However, this doesn't work just yet, due to warping issues.\n"
   },

  { 5,JUN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dnoise.c to have option to set noise level on command\n"
   "   line, rather than compute it.\n"
   },

  { 9,JUN,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_clust*.c routines to implement -1clust_order option.\n"
   "* Modified 3drefit.c to have -nowarp option.\n"
   },

  { 13,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated Doug Ward's erosion+dilation routines into the\n"
   "   clustering modules.\n"
   },

  { 14,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added option -skip_afnirc to allow .afnirc file to be skipped.\n"
   "* Fixed bug in afni_setup.c that didn't manage the palette chooser\n"
   "   menu when starting with 0 palettes and then later reading in some.\n"
   "* Fixed bug in plug_copy.c that put the anat type off by 1.  Also\n"
   "   made the 'Zero [One]' option not make the anat type always be omri.\n"
   "* Fixed bug in parser.f, whereby the vector versions of the boolean\n"
   "   functions (or, and, mofn) were evaluated incorrectly.\n"
   },

  { 15,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Change afni_pcor.c to remove use of DENEPS test for division\n"
   "   in computation of correlation coefficient.\n"
   },

  { 17,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program imstack.c to stack up a bunch of 2D images into\n"
   "   the stupid MGH format.  [For Kathleen Marie Donahue.]\n"
   },

  { 21,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added model_convgamma.c to represent a gamma variate convolved\n"
   "   with a reference time series.  The 'k' parameter of Doug Ward's\n"
   "   model_gammaver.c was modified to be called 'amp' here, since the\n"
   "   impulse response is reparameterized to have peak value = 'amp',\n"
   "   rather than 'k * (rb/e)**r', which is clumsy.\n"
   "* Modified Doug Ward's NLfit_model.h to ensure that certain routines\n"
   "   needed by model functions can be forced to be loaded.\n"
   "* Modified 3dTSgen.c to make the '-ncnstr' and '-scnstr' options\n"
   "   recognized, since there is a typo in the manual.\n"
   "* Modified Makefile.INCLUDE for 3dTSgen and 3dNLfim to use the\n"
   "   proper dynamic linking load flags $(PLFLAGS), and also to include\n"
   "   the proper dependencies.\n"
   },

  { 22,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routine THD_timeof_vox to thd_timeof.c.  This allows\n"
   "   computation of voxel time from voxel index, rather than voxel\n"
   "   coordinate.\n"
   "* Removed some redundant code from 3dTSgen.c -- it opened the\n"
   "   input dataset twice in a row, and loaded the datablock when\n"
   "   there was no need.\n"
   "* Modified 3dTSgen.c, 3dNLfim.c, and plug_nlfit.c to have new '-inTR'\n"
   "   option to allow computation of model functions with TR set from\n"
   "   the input dataset rather than fixed at TR=1.  Note that if the\n"
   "   units of the dataset time axis are msec, they will be converted\n"
   "   to sec instead (allowing msec to be used as a unit was a mistake).\n"
   },

  { 27,JUL,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in routine suck_file in afni_setup.c, which put the\n"
   "   terminating NUL character 1 place too far in the output array.\n"
   "* Wrote program uncomment.c, to remove comments from C files.\n"
   "* Added model_convgamma2a.c.\n"
   "* Modified NLfit.c to generate a fixed set of random samples instead\n"
   "   of starting over for each voxel.  Also fixed the algorithm that\n"
   "   keeps the best random samples -- it was not always keeping the\n"
   "   best one, just some of them.\n"
   },

  { 1,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added copyright information to some .c and .h files.\n"
   "* Modified mri_to_short.c to allow for special case of scale!=1.0.\n"
   "* Modified plug_realtime.c to allow for 2D image alignment.\n"
   "   (Continuation of work from,APR,that was unfinished then.)\n"
   "   First cut is to do all registration at end of acquisition.\n"
   "* Turned off 'frivolities' during a real-time run.\n"
   "* Added the ability to graph points+lines together in afni_graph.c\n"
   "   (also see changes of 09,JAN,1998).\n"
   },

  { 6,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added changes to thd_compress.[ch] made by Craig Stark/Paul Reber\n"
   "   of UCSD, to allow them to use their custom decompression\n"
   "   program 'brikcomp'.\n"
   "* Added realtime 2D registration to plug_realtime.c.\n"
   "* Modified 3dmaskave.c and plug_maskave.c to allow user to choose\n"
   "   a sub-brick of the mask dataset.  plug_maskave.c also now lets\n"
   "   the user save the mask average of a 3D+time dataset into the\n"
   "   timeseries library (for use as a FIM, for example).  Also\n"
   "   fixed an overflow bug in both programs when the mask range\n"
   "   is too big for a short or byte dataset.\n"
   },

  { 7,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified display.[ch] to store extra information about the X11\n"
   "   visual.  This isn't used yet, but is preparatory to allowing\n"
   "   support for using TrueColor visuals and installed colormaps.\n"
   },

  { 17,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Adapted old Fortran PLOTPAK to in-memory plotting, with routines\n"
   "   to graph to screen and to PostScript.\n"
   "* Used this to implement the 'Plot' button in the timeseries chooser,\n"
   "   and to do graphing of the realtime 2D motion parameters.\n"
   },

  { 22,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified display.c to support TrueColor visuals, and xim.c to\n"
   "   support arbitrary byte ordering, plus 3 and 4 bytes/pixel.\n"
   },

  { 23,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified dbtrace.h and most afni_*.c files to allow not only\n"
   "   debug tracing of routine entry/exit, but also the printout\n"
   "   of other debugging information that was formerly hidden\n"
   "   behind '#ifdef AFNI_DEBUG'.\n"
   "* A few more changes to make xim.c work properly with depth=24,\n"
   "   which can be either 3 or 4 bytes/pixel.\n"
   },

  { 25,AUG,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_pcor.c change of 15,JUL,1998 to check denominator\n"
   "   vs. DENEPS rather than 0.0.\n"
   },

  { 9,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_sarr.c to add routine to 'normalize' a list of\n"
   "   files using the C library routine 'realpath' and to cast out\n"
   "   duplicate files.\n"
   "* Used this in afni.c and thd_get1D.c.\n"
   "* Also added *.1Dv files to list of allowable 1D extensions in\n"
   "   thd_get1D.c.\n"
   "* Doug Ward provided me with the new 3dDeconvolve.c program, as\n"
   "   well as some changes to 2dImReg and 3dRegAna.\n"
   },

  { 14,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-install' option to afni.c to allow installation of a\n"
   "   new X11 Colormap.  Also affected display.[ch], afni_widg.c,\n"
   "   imseq.c, afni_graph.c, xutil.[ch], afni_plugin.c, to3d.c,\n"
   "   and plug_drawdset.c.\n"
   "* Added '-2swap' and '-4swap' options to to3d.c, to allow data\n"
   "   to be byte-swapped on input.  Also added a 'Byte Swap' button\n"
   "   to do the same thing interactively.\n"
   },

  { 16,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmaskave.c to allow selection of dataset sub-brick and\n"
   "   dataset value range.\n"
   },

  { 17,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-1zscore' option to 3dmerge.c (via edt_onedset.c, etc.).\n"
   "* Also completed the list of 'p2t' and 't2p' routines, as well\n"
   "   as adding 't2z' routines, in mri_stats.c and thd_statpval.c.\n"
   },

  { 18,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program cdf.c to provide a command line way to get results\n"
   "   from the 't2p', 'p2t', and 't2z' routines.\n"
   "* Modified afni_setup.c so that when a .pal file is input, the\n"
   "   last newly defined palette becomes the active one in the\n"
   "   AFNI controller which read the file in.\n"
   },

  { 22,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dTcat.c, to catenate multiple 3D+time datasets into\n"
   "   one big dataset (and possibly detrend them at the same time).\n"
   "   This was adapted from 3dbucket.c\n"
   },

  { 28,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_onedset.c to correctly deal with clipping a dataset\n"
   "   when the clip range is larger than the dataset range, and the\n"
   "   datum type is short.\n"
   },

  { 29,SEP,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routine mri_rota_shear() to mri_rota.c to do 2D image rotation\n"
   "   using the Paeth shearing method combined with FFTs.\n"
   },

  { 1,OCT,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_rota_shear to double up on FFTs -- speeds it up by\n"
   "   about 30%.\n"
   "* Modified mri_align.c and mri_2dalign.c to allow choice of\n"
   "   interpolation method at each stage of registration.\n"
   "* Modified imrotate.c, imreg.c, and plug_realtime.c to use the new\n"
   "   image alignment methods.\n"
   },

  { 9,OCT,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifed afni.c and thd_fdbrick.c to allow the user to control the\n"
   "   image flipping -- that is, to let left be displayed on the left.\n"
   },

  { 16,OCT,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Released 3dvolreg.c to Tom Ross for beta testing.\n"
   },

  { 21,OCT,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routines to afni_plugin.c to create/manipulate PLUGIN_strval's\n"
   "   [= label + textfield].\n"
   },

  { 26,OCT,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Finished plug_tag.c, and added tagset to a 3D dset.  Modified afni.c\n"
   "   to allow markers panel to open when there are no markers, so user can\n"
   "   control the color of the tags.\n"
   "* Modified mri_max.c to fix the initial values.\n"
   "* Modified 3dvolreg.c and mri_3dalign.c to add a clipping option.\n"
   },

  { 1,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added realtime 3D registration to plug_realtime.c.\n"
   "* Added quintic interpolation option to thd_rot3d.c and places that use\n"
   "   it (3dvolreg.c, mri_3dalign.c, 3drotate.c, and plug_realtime.c).\n"
   },

  { 3,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c, afni_widg.c, and afni.h to allow user to lock time\n"
   "   indexes of controllers, as well as the spatial locations.\n"
   },

  { 12,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmerge.c to have new -1dindex and -1tindex options.\n"
   },

  { 16,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_align.c, mri_2dalign.c, and mri_3dalign.c to ensure\n"
   "   that the weighting factors are non-negative.\n"
   "* Modified 3dvolreg.c to skip registration on the base volume.\n"
   "* Added plug_volreg.c to do the same job as 3dvolreg.c.\n"
   "* Fixed bug in 3drotate.c that caused -[ab]shift commands without\n"
   "   directional suffixes to be converted into zeros.\n"
   },

  { 18,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in plug_power.c in the untangling of FFT coefficients.\n"
   "* Modified afni_slice.c to properly clip short and byte interpolation\n"
   "   to avoid overflow.\n"
   },

  { 20,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified csfft.c to use unrolled routines for FFTs of length 16,\n"
   "   32, and 64.  Also added special routines using the fft64 code\n"
   "   to do lengths 128 and 256.\n"
   "* Modified mri_3dalign.c and 3dvolreg.c to allow specification of\n"
   "   a different interpolation method to be used at the final\n"
   "   rotation to the output brick.\n"
   },

  { 23,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a typo in plug_realtime.c and plug_volreg.c that made the\n"
   "   choice of 'Heptic' use quintic interpolation instead.\n"
   },

  { 27,NOV,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed some logical errors in 3dmerge.c with interaction of -1dindex,\n"
   "   -1doall, and the dataset I/O.\n"
   },

  { 3,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a problem in 3dmerge.c with '-datum float' and the sub-brick\n"
   "   scaling factor, when editing one dataset only.\n"
   },

  { 4,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added unrolled fft8() to csfft.c.\n"
   "* Modified thd_rot3d.c to start the FFT size calculation at least\n"
   "   at 95% of the actual width rather than 90%.  This reduces the\n"
   "   likelihood of wraparound effects.\n"
   },

  { 10,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a timeout for the child process in plug_realtime.c.\n"
   },

  { 13,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in the dataset -> time series routines that didn't\n"
   "   scale properly if some of the brick factors were zero and\n"
   "   others were not.  Files affected: mri_to_float.c, thd_dsetto1D.c,\n"
   "   thd_fdto1D.c, and thd_initdblk.c.\n"
   },

  { 16,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed TESTER code from afni_widg.c, and added some STATUS()\n"
   "   printouts to trace progress.\n"
   },

  { 17,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified xutil.c to use XmChangeColor to change the color\n"
   "   of a widget (routine MCW_set_widget_bg).\n"
   "* Added some changes by Doug Ward to speed up linear regression\n"
   "   calculations.\n"
   },

  { 22,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in plug_volreg.c, where the ax? variables were used\n"
   "   before they were defined, resulting in the wrong order of output\n"
   "   of the estimated movement parameters in the graphs and dfile.\n"
   },

  { 30,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added function Winsor9 to the 2D transformations, in imseq.c.\n"
   "* Added RowGraphs to imseq.c, also affecting plot_ts.c (in coxplot)\n"
   "   and afni.c.\n"
   },

  { 31,DEC,1998 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c crosshairs to allow display only along certain axes.\n"
   },

  { 3,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added routine MCW_widget_visible to xutil.c, and used it in imseq.c\n"
   "   and afni_graph.c.\n"
   },

  { 4,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed the time lock feature in afni.c so that it would not operate\n"
   "   if toggled off.  [Oops]\n"
   "* Added AFNI_ALWAYS_LOCK environment feature.\n"
   },

  { 5,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified the way imseq.c places the dialogs (Disp and Mont buttons),\n"
   "   so as to reduce the likelihood that some of the dialog will appear\n"
   "   offscreen.\n"
   "* Added HorZ ('h') selection to afni_graph.c 'Opt->Grid' submenu, which\n"
   "   will draw a dashed line at the y=0 level in each sub-graph.\n"
   },

  { 6,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c to try to avoid crashes when graphing window\n"
   "   is closed using 'Opt' menu 'Done' button.  Seems to be caused\n"
   "   by Expose events, but what exactly isn't clear -- happens on\n"
   "   SGI systems.  Using the 'Done' button now starts a 50 msec timeout\n"
   "   before the 'q' key is simulated.  Also, the Expose event count\n"
   "   is now checked before processing, and only if event->count == 0\n"
   "   is any work done.  Why these changes do the job is not obvious.\n"
   },

  { 7,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c to move placement on screen of the\n"
   "   'Opt->Colors Etc.' submenu if it ends up placed directly over\n"
   "   the 'Opt' menu.  This could happen on some versions of Motif\n"
   "   when the graph window is up against the right edge of the\n"
   "   screen.  A callback is executed when the submenu is mapped,\n"
   "   and if it is in a bad location, its XmNx value is changed.\n"
   },

  { 10,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_coerce.c to make EDIT_coerce_type work with doubles.\n"
   },

  { 11,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified the rest of edt_coerce.c and edt_volamax.c to allow doubles.\n"
   "* Added thd_mastery.c, which allows a dataset to be 'mastered' on input,\n"
   "   so that its bricks are a selection of bricks from a master dataset.\n"
   "   Adds the routine THD_open_dataset().  Modified 3ddata.h and a\n"
   "   bunch of other functions.\n"
   "* Modified 3dinfo.c, 3dcalc.c, rtfeedme.c, from3d.c, 3drotate.c, and\n"
   "   3dvolreg.c, to use the new opening routine, to allow for subset\n"
   "   selection.\n"
   },

  { 15,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed the old 3dpc.c to work with the new AFNI data storage,\n"
   "   and to use the EISPACK routines instead of LAPACK (with help\n"
   "   from Vinai Roopchansingh).\n"
   "* Made swap4 work with float input datasets as well as int.\n"
   },

  { 19,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-1ddum' argument to 3dpc.c.\n"
   },

  { 20,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in mri_stats.c, where 'p2t' conversion was done backwards\n"
   "   for the F statistic (p should have been q, and vice-versa).\n"
   },

  { 21,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added SurfGraph feature to imseq.c.\n"
   "* Added OSfilt9 to imseq.c and the 2D transformations menu (afni.c).\n"
   "* Modified coxplot/plot_topshell.c to store a handle to the form at top\n"
   "   of the graph window hierarchy.\n"
   "* Modified xutil.c to add the 'SaveUnder' property to the hints widget.\n"
   },

  { 24,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot (coxplot.h, plot_motif.c, plot_x11.c) to use the\n"
   "   X11 Double Buffer extension, if HAVE_XDBE is defined.  This makes\n"
   "   the redrawing of graphs look much smoother.\n"
   },

  { 25,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Created the 'AREN' routines (aren.[ch]) for volume rendering, on top\n"
   "   of VolPack.  Not sure what to do with them yet.\n"
   },

  { 26,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in initialization and usage of Xdbe library, in coxplot.\n"
   },

  { 27,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add 'UK Flag' location marker to RowGraphs and SurfGraphs.\n"
   },

  { 29,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed afni_func.c so that if the user presses 'See Function'\n"
   "   when there is no function, it turns the toggle button back\n"
   "   off.  Formerly, it turned off the internal flag, but didn't\n"
   "   change the toggle button state, so that the user was fooled.\n"
   },

  { 30,JAN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Flood->Zero' and 'Zero->Value' options to plug_drawdset.c.\n"
   },

  { 5,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added plug_render.c, the first version (grayscale only) of the\n"
   "   volume rendering code.\n"
   "* Changed name of AREN to MREN and put it in mri_render.[ch].\n"
   "* Cloned the mcw_graf.[ch] routines out of xv.\n"
   "* Added the coordinate stuff to 3dcalc.c.\n"
   },

  { 7,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added new program 3dfractionize.c (per Ziad Saad).\n"
   },

  { 9,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to imseq.c include putting the colorbar popup menu on\n"
   "   ctrl-Button3 if there is only 1 image in the sequence, and\n"
   "   changing the sharp-factor arrowval to run from 1-9 instead\n"
   "   of 1-99.\n"
   "* Added 'MCW_noactext' option to arrowvals (bbox.c), so that\n"
   "   the usual actions taken when the user leaves the window\n"
   "   are not taken.\n"
   "* Added many features to plug_render.c: automation, cutouts,\n"
   "   and accumulation.\n"
   "* Fixed 3drefit.c -view option.\n"
   },

  { 10,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Expr > 0' cutout to plug_render.c\n"
   "* Added SIND(), COSD(), and TAND() functions to parser.f\n"
   },

  { 11,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified many routines in mri_*.c to deal with MRI_rgb type\n"
   "   images, in preparation for modifying imseq.c to deal with such.\n"
   "* Modified display.[ch] to include a 'colordef' for conversion\n"
   "   between Pixel and RGB triples.\n"
   "* Added routine to xim.c to convert MRI_rgb image to an XImage.\n"
   "* Changed imseq.c to allow MRI_rgb images.\n"
   "* Created program 1dplot.c.\n"
   },

  { 15,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifed mri_render.c to change the way it deals with color volumes.\n"
   "* Modified plug_render.c to allow for color overlays.\n"
   "* Modified xim.c and display.c to deal with converting a RGB image\n"
   "   to an XImage in a more efficient way.\n"
   },

  { 16,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified bbox.[ch] to allow non-power-of-10 steps in an arrowval,\n"
   "   if the av->fstep structure component is modified by the user.\n"
   "* Some minor cosmetic changes to plug_render.c.\n"
   },

  { 18,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* In plug_render.c\n"
   "  + Replaced 'Remove Isolas' w/ 'Remove Small Clusters' in plug_render.c\n"
   "  + Added the slant cut planes.\n"
   "* In imseq.c, made it so that closing a rowgraph or surfgraph window\n"
   "   turns off the feature.\n"
   },

  { 22,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New routine 'addto_args' (addto_args.c) used to catenate stdin\n"
   "   to the (argc,argv) command line of a program.  Testbed is\n"
   "   3dRegAna.c (for D. Emge).\n"
   "* Added 'LOCK_ZORDER' command to plug_realtime (for Vinai).\n"
   "* Fixed bugs in plug_render.c having to do with the 'Expr > 0' cutout\n"
   "  + Combination with AND was incorrect (counted ncdone too many times);\n"
   "  + Test for evaluation to a pure number was incorrect if there were\n"
   "    any leading blanks.\n"
   },

  { 23,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in thd_trusthost.c.\n"
   },

  { 25,FEB,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the MEDIAN function to parser.f and 3dcalc.c.\n"
   },

  { 1,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the *_p2t, *_t2p, and *_t2z functions to parser.f, parser_int.c,\n"
   "    and 3dcalc.c.\n"
   "* Created ASCII PPM file gmove.ppmap to use in gmovie csh script.\n"
   "* Removed tapering from FFT interpolation in thd_rot3d.c.\n"
   },

  { 3,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_sarr.c to skip the realpath() expansion if the\n"
   "   environment variable AFNI_NOREALPATH is set.\n"
   },

  { 6,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed GNU malloc from afni.c and incorporated my own mcw_malloc.[ch]\n"
   "   functions/macros.\n"
   },

  { 8,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More changes to mcw_malloc.[ch].\n"
   "* Added macro FREE_AV() to bbox.h, because I found out that all places\n"
   "   where I used XtFree() on an arrowval would leave the string values\n"
   "   (sval) high and dry.\n"
   },

  { 9,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* In refit_MCW_optmenu (bbox.c), found I had to free the av->sval and\n"
   "   ->old_sval values, since they were being abandoned.\n"
   "* Added AFNI_NO_MCW_MALLOC environment variable to mcw_malloc.c,\n"
   "   to let user turn off these routines.\n"
   },

  { 10,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* In afni_graph.c, added deletion of XImage after XGetImage\n"
   "   is used to save the graph window to a file.\n"
   },

  { 12,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed 2 bugs in plug_render.c\n"
   "   - opacity_scale changed wasn't checked if number of cutouts == 0\n"
   "   - didn't invalidate overlay when loading new functional dataset\n"
   },

  { 22,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added SOLARIS_DIRENT_PATCH code to mcw_glob.c.\n"
   },

  { 23,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added call to reload_DC_colordef in imseq.c so that\n"
   "   Save:one works properly after palette adjustment.\n"
   },

  { 26,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a FREE_VOLUMES to plug_render.c after a new anatomical\n"
   "   sub-brick is selected.\n"
   },

  { 29,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_receive.c and others to allow for multiple receivers\n"
   "   for an IM3D.\n"
   "* Modified plug_render.c to auto-redraw when it receives notice of a\n"
   "   crosshair location move.\n"
   },

  { 31,MAR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_receive.c to allow for new types of transmission\n"
   "   DRAWNOTICE = notification that something was drawn\n"
   "   DSETCHANGE = notification that dataset pointers may have changed\n"
   "                (needed when rescanning sessions)\n"
   "* afni_receive.c now handles transmission to all the interested\n"
   "   viewers using the AFNI_process_* routines within.\n"
   "* Modified plug_drawdset.c to properly recover when rescan is used.\n"
   "* Modified plug_render.c to redraw when DRAWNOTICE is received,\n"
   "   and to recover when rescan is used.\n"
   "* Modified 3dcalc.c to scale each output sub-brick to shorts/bytes\n"
   "   separately, rather than globally [per request of KMD].\n"
   },

  { 1,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Finally centralized the computation of the '[A]' type labels for\n"
   "   AFNI controllers in afni_func.c, in the new routine\n"
   "   AFNI_controller_label(im3d).  Then modified afni_plugin.c,\n"
   "   afni_widg.c, plug_render.c, plug_drawdset.c, and plug_tag.c\n"
   "   to use this function.\n"
   },

  { 2,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_http.c to use directory $TMPDIR if defined, rather\n"
   "   than always rely on /tmp.\n"
   "* Also added routines to this file to allow transfer of 'ftp://'\n"
   "   URLs -- this is done using a shell script running the\n"
   "   ftp program.\n"
   },

  { 3,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to have the currently active dataset\n"
   "   'selected' when the user popups a new dataset chooser.\n"
   "* Removed the non-working rotation feature from 3ddup.c.\n"
   },

  { 5,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_http.c to allow change of user ident for ftp://\n"
   "   access to files.\n"
   "* Tested AFNI with LessTif v.0.89 -- seems to work.\n"
   },

  { 13,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to allow 3D: hglobal to be < -1 as\n"
   "   long as hglobal+himage >= 0 [per Gary Strangman of MGH].\n"
   "* Added mri_cut.c, function to cut out a sub-image.\n"
   },

  { 14,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to fix the pbar_sgn_* initialization, since\n"
   "   it incorrectly checked the thresholds against the range\n"
   "   0..1 rather than -1..1 [per Chad Moritz of UW Madison].\n"
   },

  { 15,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to also try ppm from mri_read() function.\n"
   "* Modified mri_write.c to write byte files as pgm.\n"
   "* Modified mri_to_rgb.c to have a 1 rgb image -> 3 byte image function,\n"
   "   and fixed a bug in the 3 images -> 1 rgb image function.\n"
   "* Added mri_dup.c, to upsample a 2D image; added program imupsam.c\n"
   "   to do this function from the command line.\n"
   },

  { 19,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c, afni.h, afni_widg.c to add 'Set All' button\n"
   "   to Lock menu.\n"
   },

  { 26,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed thd_info.c to report dataset axes dimensions correctly\n"
   "   (also fixed DAXES_NUM macro in 3ddata.h).\n"
   "* Put code in plug_render.c that allows non-axial datasets to\n"
   "   be rendered -- but disabled it for now.\n"
   "* New program 3daxialize.c will rewrite a dataset with BRIKs\n"
   "   into the RAI (axial) orientation.\n"
   },

  { 28,APR,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* In 3daxialize.c, made sure that the slice-wise time offsets\n"
   "   are nulled out.\n"
   },

  { 27,MAY,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added an X defaults initializer for the graph ggap.\n"
   },

  { 30,MAY,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a variable polort order to afni.c, afni.h, afni_graph.c,\n"
   "   and afni_fimmer.c.\n"
   "* Discovered a bug in afni_pcor.c in the % Change calculation,\n"
   "   where the last ort wasn't being used in the baseline estimation.\n"
   },

  { 2,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to draw partial crosshair sets like the\n"
   "   image viewers.\n"
   "* Modified afni.c to send a 'process viewpoint' message when the\n"
   "   partial crosshair status changes, so that the renderer will\n"
   "   get a notice and be able to redraw itself promptly.\n"
   "* Modified plug_realtime.c to use polort.\n"
   "* Added ability to change FIM background threshold percent (FIM_THR)\n"
   "   to afni_graph.[ch], afni.c, afni_fimmer.c, and plug_realtime.c\n"
   },

  { 3,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed thd_info.c where it refered to brick labels that might not\n"
   "   exist -- now it uses the DSET_BRICK_LAB macro, which always works.\n"
   "* Fixed plug_realtime.c to add brick labels to the FIM dataset.\n"
   },

  { 4,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ***ENVIRONMENT section to .afnirc file processing: changes to\n"
   "   afni_setup.c, and to afni.c to have environment variables setup\n"
   "   before other things.\n"
   "* Made AFNI_load_defaults() in afni.c look at environment variable\n"
   "   AFNI_name if X11 resource AFNI*name doesn't exist.\n"
   },

  { 7,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in edt_onedset.c that always applied zscore transformation\n"
   "   if possible!\n"
   "* Created afni_environ.[ch], which now process a ***ENVIRONMENT section\n"
   "   of .afnirc.  Also, in all programs, switched use of getenv() to\n"
   "   my_getenv(), which just makes sure that .afnirc's ***ENVIRONMENT has\n"
   "   been processed before using getenv().  In this way, the .afnirc setup\n"
   "   will be universal.\n"
   },

  { 8,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added MCW_new_idcode() to 3drotate.c -- was producing datasets with\n"
   "   identical idcodes, which is terrible!\n"
   "* Added function THD_check_idcodes() [thd_checkidc.c] to allow AFNI to\n"
   "   check the idcodes in all datasets for duplicates.\n"
   },

  { 15,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed afni_receive.c to return the proper 'key' value from function\n"
   "    AFNI_receive_init().\n"
   "* Modifed plug_render.c to redisplay func dataset statistics after\n"
   "    receiving a drawing notice.\n"
   "* Modified afni_plugin.[ch] to crosscheck each plugin's compilation\n"
   "    date with AFNI's, and print a warning if they differ.\n"
   },

  { 17,JUN,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 24swap.c.\n"
   },

  { 7,JUL,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dTsmooth.c.\n"
   "* Modified afni_plugin.c to skip date crosscheck on systems that don't\n"
   "    have the C function strptime().\n"
   "* Added -vnorm option to 3dpc.c.\n"
   },

  { 13,JUL,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Scripts' option to plug_render.c.\n"
   },

  { 14,JUL,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 'Scripts' in plug_render.c to allow save/restore of grafs.\n"
   "   Also modified mcw_graf.[ch].\n"
   },

  { 19,JUL,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3dcalc.c, where it tested CALC_type[ids] for > 0, whereas\n"
   "   it should have been >= 0.  The effect was that 3D+time byte valued\n"
   "   datasets were not loaded into the atoz array.\n"
   },

  { 29,JUL,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed afni_graph.c to not use XComposeStatus in XLookupString call,\n"
   "   which was causing trouble on SunOS.\n"
   "* Line 2707 of afni_graph.c had an '==' for assignment, instead of '='.\n"
   "   The effect was a possible failure of the x-axis (xax_tsim) graphing\n"
   "   mode.  I don't think this failure ever occured in practice.\n"
   },

  { 30,JUL,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program float_scan.c and function thd_floatscan.c to check\n"
   "   floating point files and arrays (respectively) for illegal values\n"
   "   (e.g., NaN and Infinity).  Incorporated thd_floatscan() into\n"
   "   thd_loaddblk.c (if AFNI_FLOATSCAN is set).\n"
   },

  { 1,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed 'Voxel Coords' and 'Show Hints' pushbutton on Misc menu to\n"
   "   toggle buttons.  Modified afni.h, afni_widg.c, and afni_func.c.\n"
   "* Added a 'writeownsize' option to the Misc menu, but then changed\n"
   "   my mind and #ifdef-ed it out.\n"
   },

  { 2,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the AFNI splash screen; see afni_splash.[ch].\n"
   },

  { 6,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugin.[ch] to allow plugins to set a sequence code,\n"
   "   which determines the order in which the plugins appear on the menu.\n"
   "* Received 4 plugins (plug_hemisub, plug_maskcalc, plug_roiedit,\n"
   "    plug_maxima) from the estate of Rick Reynolds.\n"
   },

  { 7,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a '[left is left]' or '[left is right]' label to the winfo text\n"
   "   in AFNI coronal and axial images displays.  Affected files\n"
   "   imseq.[ch], afni.c.\n"
   "* Removed the non-toggle code leftover from the 01,AUG,1999 changes.\n"
   },

  { 8,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the radix-3 routine to csfft.c.\n"
   },

  { 9,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the radix-5 routine to csfft.c.\n"
   "* Modified edt_blur.c, thd_rot3d.c, and plug_power.c to use new\n"
   "   FFT abilities.\n"
   },

  { 19,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -indump option to 3dmaskave.c.\n"
   "* Added 'Jump to (ijk)' button to image window popup: afni.[ch], afni_widg.c.\n"
   },

  { 23,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 1/N scaling option to csfft.c.  Modifed fftcheck.c accordingly.\n"
   },

  { 29,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified count.c to accept a '-scale' option.\n"
   "* Modified Tom Ross's 3dNotes.c software to be a set of routines (thd_notes.c),\n"
   "   for eventual use in a plugin.\n"
   },

  { 30,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_fimmer.c to accept an upper limit on the computable\n"
   "   percent change.\n"
   },

  { 31,AUG,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a History Note feature to thd_notes.c.\n"
   "* Added a routine to afni_plugin.c to produce a command string\n"
   "   summary of the inputs to a plugin.\n"
   "* Modified many programs and plugins to write to the History Note.\n"
   },

  { 1,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Wrote a Notes viewing/editing plugin.\n"
   },

  { 8,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified the error messages in some of the thd_*.c files to\n"
   "   be more verbose (Bharat Biswal couldn't understand one).\n"
   "* Modified afni_fimmer.c (and afni.h, afni_graph.c) to allow computation\n"
   "   of '% From Ave' = percent change computed from the average instead\n"
   "   of from the baseline [per AJ].\n"
   },

  { 11,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Got History changes from Doug Ward.\n"
   },

  { 14,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added complexscan function to thd_floatscan.c.\n"
   "* thd_loaddblk.c now scans complex inputs for errors, if requested.\n"
   "* to3d.c now scans float and complex input images for errors.\n"
   "* to3d.c now clips out all but a few of the input image files on\n"
   "   the command line for the History Note.\n"
   },

  { 15,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -slices option to 3dmaskave.c.\n"
   "* Added default values for i,j,k to 3dcalc.c.\n"
   "* Added thd_makemask.c.\n"
   "* Added -mask option to 3dpc.c.\n"
   },

  { 16,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed THD_open_one_dataset() to THD_open_dataset() in programs\n"
   "   3dFourier.c, 3dROIstats.c, 3dmaskave.c, 3dnvals.c, 3dproject.c, 3dttest.c.\n"
   "* Modified 3dclust.c to use -1dindex and -1tindex, as in 3dmerge.c\n"
   "* Modified 3dTcat.c to have options -rlt+ and -rlt++.\n"
   },

  { 19,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* If dataset header doesn't have BYTEORDER attribute, thd_info.c now prints\n"
   "   '{assumed}' next to the reported byte order.\n"
   "* Added hostname to History Note time stamp, so you can see what on what\n"
   "   machine a program was run.\n"
   },

  { 20,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* thd_initdblk.c: print out a warning if byte order is unspecified when\n"
   "   creating a dataset.\n"
   "* thd_notes.c: add the username to the History Note stamp.\n"
   },

  { 21,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* added message about 3drefit to thd_initdblk.c.\n"
   "* modified MEDIAN function in parser.f to handle N=3 case separately.\n"
   },

  { 24,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed wcsuffix bugs in afni_graph.c\n"
   "   - didn't mangle output name correctly!\n"
   "   - overwrote the end of the wcsuffix string!\n"
   },

  { 28,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added environment variable AFNI_PCOR_DENEPS to afni_pcor.c,\n"
   "   so that user can set the DENEPS test for the correlation\n"
   "   coefficient calculation.\n"
   },

  { 30,SEP,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added plug_histog.c (histogram plugin) and a histogram\n"
   "   plotting routine to afni_plugin.c.\n"
   },

  { 4,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added histogram of correlation coefficient to plug_histog.c.\n"
   },

  { 5,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in mcw_malloc.c, where it printed out the wrong\n"
   "   info when it found an overwrite error in a malloc()-ed\n"
   "   block of memory.\n"
   },

  { 6,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_histog.c and afni_plugin.[ch] to plot extra curves\n"
   "   in histograms.\n"
   "* Modified coxplot/plot_motif.c to look harder for the HOTCOLOR before\n"
   "   it gives up and uses the default.\n"
   },

  { 7,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_makemask.c to include a counting function.\n"
   "* Modified plug_histog.c to use THD_makemask() rather than create\n"
   "   its own mask the hard way (I just forgot about that routine).\n"
   "* Added program 1deval.c, to evaluate an expression at a bunch\n"
   "   of points and write it to disk - the goal is to simplify\n"
   "   creation of sample 1D files.\n"
   },

  { 8,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Increased buffer size in mri_read.c for ASCII file line length.\n"
   },

  { 9,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added AFNI_ENFORCE_ASPECT environment variable, to make imseq.c\n"
   "   enforce the image aspect ratio - this is to be used when the\n"
   "   window manager doesn't do this properly.\n"
   },

  { 13,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dTstat.c to compute the same stuff that plug_stats.c\n"
   "   does, but in a batch program.\n"
   },

  { 14,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added option -apar to 3drefit.c, to allow (re)setting of the anat parent.\n"
   "* Added option -warp to 3dfractionize.c, to allow inverse warping from +tlrc\n"
   "   coords back to +orig coords during the fractionation process.\n"
   },

  { 18,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added history copy to afni_func.c's creation of follower datasets.\n"
   "* Added -preserve option to 3dfractionize.c, to allow the output dataset\n"
   "   to keep the input values, rather than create a fractional mask.\n"
   "* Added program 3dmaskdump.c, to dump voxel values out to ASCII.\n"
   "* Added qsort_int() to mri_percents.c.\n"
   },

  { 19,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 1dtranspose.c.\n"
   "* Added option -noijk to 3dmaskdump.c.\n"
   "* Added option -tim to afni.c - also modified afni.h, afni_graph.[ch], and\n"
   "   afni_func.c to make it work.  This option is like -im, but interprets\n"
   "   the images as being spread thru time rather than space.\n"
   },

  { 20,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified -tim in afni.c to allow for 3D inputs (space-then-time=-zim,\n"
   "   or time-then-space=-tim).\n"
   },

  { 21,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a couple of small bugs in 1deval.c.\n"
   },

  { 28,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed 'experimental' message for TrueColor visuals in display.c.\n"
   "* Added csfft_nextup_one35() function to csfft.c.\n"
   "* Modified various programs to use csfft_nextup_one35() in place\n"
   "   of csfft_nextup(), for efficiency.\n"
   "* Moved shifting routines from thd_rot3d.c to thd_shift2.c, so that\n"
   "   they can be used in other programs.\n"
   },

  { 29,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_1Dtodset.c - provides the inverse of thd_dsetto1D.c\n"
   "   (inserts a vector into a dataset time series, rather than extracts it).\n"
   "* Add thd_detrend.c - detrend a timeseries and provide mean, slope.\n"
   },

  { 30,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_detrend.c to have linear and quadratic detrending,\n"
   "   and an L2 normalizing function.\n"
   },

  { 31,OCT,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Finished program 3dTshift.c - time shifting.\n"
   },

  { 1,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dclust.c and edt_onedset.c to have a -verbose option, so as to\n"
   "   print out progress report info.\n"
   "* Added MCW_hotcolor() to xutil.[ch], to get rid of HOTCOLOR macro usage.\n"
   "* Added function PLUTO_set_butcolor() to afni_plugin.[ch], to let plugin\n"
   "   set its menu button color.\n"
   },

  { 2,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dcalc.c to fix problem with using sub-brick selectors of the\n"
   "   form '-b3 zork+orig' -- if zork+orig was a 3D+time dataset, and it\n"
   "   was used as the template for the output, the output would be marked\n"
   "   as 3D+time even though it was not.  The solution was to mangle such\n"
   "   inputs to the form 'zork+orig[3]', which already worked fine.\n"
   },

  { 3,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed the -help output for 3drefit.c, in the description of '-apar'.\n"
   },

  { 9,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added function RWC_visibilize_widget() to xutil.[ch] - used to ensure\n"
   "   that popup widgets are fully onscreen.  Used this function in\n"
   "   afni_func.c, afni_graph.c, afni_setup.c, bbox.c.\n"
   "* Added missing 'void' to declaration of function setup_tmpdir() in\n"
   "   thd_http.c.\n"
   },

  { 16,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added more -help to 3dTshift.c, per MSB's comments.\n"
   "* Added cubic detrend routine to thd_detrend.c.\n"
   "* Added mri_read_1D() to mri_read.c.  This adds the ability to\n"
   "   do sub-vector selection, and does the transpose too.\n"
   "* Added mri_write_1D() to mri_write.c.  This just does the\n"
   "   transpose before called mri_write_ascii().\n"
   "* Modified 1dtranspose.c, 3dcalc.c, waver.c, afni_graph.c, and\n"
   "   model_convgamma*.c to use mri_read_1D().\n"
   "* Modified afni_graph.c to use mri_write_1D().\n"
   "* Added program 3dDetrend.c: remove time series trends.\n"
   "* Added predefined 't' and 'l' to 3dcalc.c.\n"
   },

  { 17,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed some -help from imcalc.c, since 3dcalc -help has the\n"
   "   same info.\n"
   "* Added function PARSER_1deval() to parser_int.c.\n"
   "* Added environment variable AFNI_MARKERS_NOQUAL to afni.c.\n"
   "* Fixed bug in 3dDetrend when -expr string had no variable symbols.\n"
   "* Modified thd_iochan.c to use SO_REUSEADDR to help close down\n"
   "   sockets quickly.  (But later commented this code out.)\n"
   },

  { 18,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified cs_addto_args.c to make the mangling of the arglist\n"
   "   more flexible.\n"
   "* Used the previous change in afni.c to add the environment variable\n"
   "   AFNI_OPTIONS that will always be added to the command line args.\n"
   "* Incorporated the OSF1 changes from Matthew Belmonte.\n"
   },

  { 22,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -histpar option to 3dcalc.c.\n"
   "* Added differential subscripting to 3dcalc.c.\n"
   "* Modified thd_intlist.c to allow for negative list elements.\n"
   },

  { 23,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dcalc.c differential subscripting to allow abbreviations\n"
   "   of the form a+j, a-k, etc.\n"
   "* Added -quiet option to 3dmaskave.c.\n"
   "* Added -normalize option to 3dDetrend.c.\n"
   "* Fixed error in afni_func.c (and adwarp.c) when writing out a dataset\n"
   "   that was told to have a byte order different from the native order -\n"
   "   it was written in native order and the header wouldn't know about\n"
   "   that - the result was the file was read in incorrectly later.\n"
   "* Also fixed same error in 3daxialize.c.\n"
   "* Also fixed thd_writedblk.c and thd_loaddblk.c to handle byte swapping\n"
   "   on complex data correctly.\n"
   },

  { 24,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 1dfft.c - to calculate abs(FFT(timeseries)).\n"
   "* Modified 1deval.c to allow input of time series.\n"
   "* Got some 3dDeconvolve.c changes from Doug Ward.\n"
   },

  { 25,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 1dcat.c - catenate *.1D files.\n"
   },

  { 28,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added function ZTONE to parser.f.\n"
   },

  { 29,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -tocx, -fromcx, and -nodetrend options to 1dfft.c.\n"
   "* Modified quadratic detrending calculation in thd_detrend.c.\n"
   "* Added -use option to 1dplot.c.\n"
   "* Added SHOWOFF macro to afni.c, distmake, and Makefile.INCLUDE.\n"
   "* Got some 3dDeconvolve.c fixes from Doug Ward.\n"
   },

  { 30,NOV,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTshift.c to allow '-tzero 0.0' and '-slice 0' as\n"
   "   options (I used <= instead of < in the original code).\n"
   "* Modified plug_render.c to reset func_cmap_set=0 if a render\n"
   "   handle is destroyed - the colormap wasn't being properly\n"
   "   reloaded when a new underlay dataset was selected.\n"
   "* Modified SHOWOFF handling in afni.c to always show compilation\n"
   "   date no matter what.\n"
   },

  { 1,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c and afni_func.c to turn on XSynchronize if -TRACE\n"
   "   option is used (or when DBG_trace gets set to 2).\n"
   "* Fixed bug in afni_func.c AFNI_force_adoption() routine: the\n"
   "   PRINT_TRACING output after the dataset scan would crash if no\n"
   "   dataset was found.\n"
   "* Modified imseq.[ch] and afni.c to set 'sides' strings for an image,\n"
   "   so that 'left=SIDE' can be displayed correctly in any orientation\n"
   "   of the image flipping process.\n"
   },

  { 3,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Print a warning message in afni_func.c when a forced adoption takes\n"
   "   place.\n"
   "* Disable 'sides' labels if an environment variable is present.\n"
   "* Incorporate '-quiet' options in 3dclust.c and 3dROIstats.c from\n"
   "   Mike Beauchamp.\n"
   "* Incorporate sub-dataset selection for various statistical programs\n"
   "   from Doug Ward.\n"
   },

  { 7,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3drename.c, to rename dataset .HEAD and .BRIK files\n"
   "   at the same time.\n"
   "* Added environment variable to control Winsor21 function in imseq.c.\n"
   },

  { 8,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops.  Took out some debugging statements in 3dTstat.c that somehow\n"
   "   survived until now.\n"
   "* Modified count of forced adoption potential conflicts in afni_func.c\n"
   "   to avoid some warning messages.\n"
   "* Added 'NULL' to return in plug_tag.c, where it had been forgotten.\n"
   "* Added program 1dnorm.c.\n"
   "* Added -byslice option to 3dDetrend.c.\n"
   },

  { 9,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -hilbert option to 1dfft.c, and -install option to 1dplot.c\n"
   "* Added 0 definition of DBG_trace to dbtrace.h in the case USE_TRACING\n"
   "   is not defined.\n"
   },

  { 13,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mode functions to parser.f, parser.inc, and 3dcalc.c.\n"
   "* Added -force option to adwarp.c; otherwise, program will not now\n"
   "   overwrite existing dataset.\n"
   "* Modified function qginv() in mri_stats.c to never return a value\n"
   "   greater than 13, no matter how absurd the value the user enters.\n"
   "* Modified edt_dsetitems.c, editvol.h to have a new editing item\n"
   "   ADN_anatpar_idcode - to let a program attach an anat parent idcode\n"
   "   to a file (vs. an anat parent dataset).\n"
   "* Modified afni_func.c to NOT print out a forced adoption message when\n"
   "   a dataset is set to be its own anatomy parent.\n"
   "* Modified plug_maskave.c to properly initialize sum=sigma=0 for EACH\n"
   "   separate sub-brick calculation.\n"
   },

  { 14,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_emptycopy.c, editvol.h, and afni_plugin.c to have newly\n"
   "   created datasets get assigned the same anatomy parent as their\n"
   "   progenitors.\n"
   "* Modified afni.c so that manually warped datasets become their own\n"
   "   anat parents.\n"
   "* Modified 3drefit.c to allow SELF and NULL to be valid arguments to\n"
   "   the -apar option.\n"
   },

  { 20,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified parser.f to remove the possibility of divison by zero in\n"
   "   the expression evaluation routines.\n"
   "* Modified display.[ch] and plug_render.c to allow 'non-mixing' of\n"
   "   colors displayed from rendering results.\n"
   },

  { 21,DEC,1999 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to put the 'non-mixing' stuff into a separate\n"
   "   menu on the 'Color' popup.\n"
   "* Took 3dROIstats.c mods from Mike Beauchamp for the -summary option.\n"
   },

  { 3,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3ddata.h to increase maximum number of datasets per directory\n"
   "   to 512 anats and funcs (each), and max number of directorys to 80.\n"
   "* Modified afni.h, afni_fimmer.c, afni_pcor.c, and afni_graph.c to\n"
   "   add '% From Top' option to FIM menus.\n"
   },

  { 4,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added gran(m,s) function to parser.f and 3dcalc.c.\n"
   "* Added 'Set All' and 'Set Defaults' buttons to FIM+ menu in afni_graph.[ch].\n"
   "* Removed contents of file README.changes.\n"
   "* Abbreviated 'Cannot find ...' error messages in thd_reconpar.c.\n"
   "* Added total dataset count report to afni.c input routine.\n"
   },

  { 5,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed default AFNI_SPLASHTIME to 1.0 seconds in afni.c.\n"
   "* Added 'static' to mixshade menu strings in plug_render.c.\n"
   "* Added MCHECK to DBG_SIGNALS in dbtrace.h.\n"
   "* Added routine mri_write_raw() to mri_write.c.\n"
   "* Modified from3d.c to have -raw and -float options.\n"
   },

  { 7,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to allow ShowThru color overlays: by doing two\n"
   "   renderings - underlay and overlay separately - and then compositing\n"
   "   the two images afterwards.\n"
   },

  { 10,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to add AFNI_RENDER_SHOWTHRU_FAC variable to\n"
   "   control the compositing of ShowThru images.\n"
   "* Added program afni_vcheck.c, to check the AFNI version against the\n"
   "   master copy back at the central AFNI web site.\n"
   },

  { 11,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Replace 'New Stuff' button under the Misc menu with 'Version Check',\n"
   "   which just runs afni_vcheck and puts the output into a popup.\n"
   "* Modified plug_render.c to make Cutout Overlay work properly with\n"
   "   ShowThru.\n"
   },

  { 13,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added plug_scatplot.c - scatter plotting.  Added PLUTO_scatterplot()\n"
   "   function to afni_plugin.[ch].\n"
   "* Fixed error in setup of Range choosers in plug_histog.c - they\n"
   "   were initialized to incorrect values.\n"
   },

  { 19,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified to3d.c to count and report number of negative voxels when\n"
   "   the input images are shorts - this is to provide a check for the\n"
   "   need for byte-swapping.\n"
   },

  { 20,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Also added a popup error message when floating point errors are\n"
   "   detected in inputs to to3d.c\n"
   "* Added '1xN' pattern to 24swap.c to allow for nonswapped data.\n"
   },

  { 24,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed error in -mask[n] option processing in 3dROIstats.c - that\n"
   "   fiend Tom Ross used argv[narg] instead of argv[narg-1] to check\n"
   "   for the presence of the 'n' option.\n"
   },

  { 27,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_sarr.c and thd_get1D.c to speed up (hopefully) reading\n"
   "   of *.1D files.\n"
   "* Modified afni.[ch] to allow use of '-no1D' option to skip reading\n"
   "   of *.1D files from the dataset directories.\n"
   },

  { 28,JAN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_x11.c to skip use of XDBE if AFNI_NO_XDBE\n"
   "   environment variable is set.\n"
   },

  { 1,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added file afni_fimfunc.c, which lets the 'user' add a function to\n"
   "   the FIM+ menu.  The Spearman rank correlation is included as a\n"
   "   sample.  Also included small changes to afni.[ch], afni_graph.[ch],\n"
   "   and a large-ish addition to afni_fimmer.c.\n"
   "* Removed useless 'break' statements in afni_graph.c that produced\n"
   "   some annoying 'unreachable code' compilation warnings.\n"
   "   Also modified bbox.c bbox and arrowval utility routines to check\n"
   "   if the input items are NULL before accessing them.\n"
   },

  { 2,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dvolreg.c to make it not print out the -help stuff when\n"
   "   argc==2.\n"
   "* Added uran() function to parser.\n"
   },

  { 3,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified GRAN() random number generator in parser.f to make it be\n"
   "   less obviously periodic.\n"
   },

  { 4,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added IRAN() integer deviate, ERAN() exponential deviate, and LRAN\n"
   "   logistic deviate generators to parser.f\n"
   "* Added THD_extract_many_series() function, and used in to speed up the\n"
   "   fimfunc work in afni_fimmer.c.\n"
   "* Fixed error in 3dbuc2fim.c, where stat_aux array was not malloc()-ed\n"
   "   large enough for the EDIT_dset_items() routine usage.\n"
   },

  { 6,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mcw_malloc_sizeof() to mcw_malloc.[ch], to return the size of\n"
   "   of a malloc()-ed region, if possible.\n"
   "* Added TRUNCATE_IMARR() macro to mrilib.h.\n"
   },

  { 10,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 3A.: image formats to allow input of unstructured ASCII files\n"
   "   into AFNI programs.  Routines changed in mri_read.c and mcw_glob.c.\n"
   },

  { 14,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -median and -MAD options to 3dTstat.c (but not to plug_stats.c).\n"
   },

  { 29,FEB,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Revived the program 3ddot.c and added masking options.\n"
   },

  { 2,MAR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added plug_nudge.c to move dataset origins around.\n"
   "* Added -dxorigin (etc.) option to 3drefit.c.\n"
   },

  { 6,MAR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -sagittal, -coronal switches to 3daxialize.c.\n"
   },

  { 7,MAR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c, to reject nzz=1 datasets, print out better\n"
   "   error messages, etc.\n"
   "* Modified rtfeedme.c to have -nzfake option, for testing the above.\n"
   "* Modified jp_afni.c to get orientations correct for Signa realtime.\n"
   },

  { 8,MAR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added sync() to plug_realtime.c after writing datasets.\n"
   },

  { 15,MAR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added AFNI_VIEW_ANAT_BRICK and AFNI_VIEW_FUNC_BRICK environment\n"
   "   variables to afni.c to force switching back to 'view brick'\n"
   "   mode when switching datasets.\n"
   "* Added '{warp}' string to titlebar when 'Warp Anat on Demand' mode\n"
   "   is engaged.\n"
   },

  { 16,MAR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -cmask option to 3dmaskdump.c (via new file edt_calcmask.c).\n"
   },

  { 4,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_base64.c routines for dealing with BASE64 encodings.\n"
   },

  { 7,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Included 3dWavelets code from Doug Ward.\n"
   },

  { 11,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3drotate.c when input was a master-ed file.  Also added\n"
   "   -clipit option to 3drotate.c.\n"
   "* Fixed bug in parser_int.c where stdlib.h wasn't included before\n"
   "   drand48() was called.\n"
   "* Added AFNI_GRAYSCALE_BOT to display.c.\n"
   },

  { 12,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added AFNI_SYSTEM_AFNIRC environment variable, to allow introduction\n"
   "   of a system wide .afnirc file.\n"
   "* Added the ability to change datasets in the renderer from script files\n"
   "   (using the saved ID code string).  Also, made the threshold slider\n"
   "   change when the script changes the threshold value (forgot this\n"
   "   visual detail before).\n"
   },

  { 14,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added options -1Dfile to 3dvolreg.c and plug_volreg.c to save movement\n"
   "   parameters to a 1D file for later ortologizing.\n"
   },

  { 16,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops.  Added AFNI_SYSTEM_AFNIRC stuff to afni_environ.[ch] as well.\n"
   },

  { 18,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Rewrote plug_nudge.c to do rotations and shifts.\n"
   "* Added mri_copy.c.\n"
   },

  { 21,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in qsort_floatfloat (cs_sort_ff.c).\n"
   "* First version of plug_betafit.c.\n"
   },

  { 28,APR,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c (and afni_plugin.[ch]) to do 'Image Only' realtime\n"
   "   acquisition - just show the images, don't do anything else.\n"
   },

  { 1,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_widg.c to disable Datamode->Misc->Version Check if in\n"
   "   realtime mode (due to long hang time while master version web page\n"
   "   is fetched across the network).\n"
   "* Added program 3dfim+.c from Doug Ward.\n"
   },

  { 9,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program imcutup.c and function mri_uncat2D.c to break 2D images\n"
   "   up into pieces.\n"
   },

  { 10,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_fimmer.c to print out an error message if nvox is zero.\n"
   "   This happens if the FIM Bkg Thresh is too high.  Also modified\n"
   "   afni.c to allow this value to be initialize to 0 (before, 1% was the\n"
   "   minimum).\n"
   },

  { 12,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated NLfim changes from Doug Ward, to include calculation\n"
   "   of R**2, per my request.\n"
   },

  { 18,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add plug_second_dataset.c and code in afni_graph.c to use it - a\n"
   "   '1D function' that returns the time series from another dataset,\n"
   "   for example to plot the -snfit output of 3dNLfim on top of the\n"
   "   original dataset.\n"
   },

  { 19,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added changes from Doug Ward for a '-mask' option to 3dNLfim.c.\n"
   },

  { 22,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added AFNI_USE_THD_open_dataset environment variable to allow\n"
   "    THD_open_one_dataset (in thd_opendset.c) to call THD_open_dataset\n"
   "    instead (in thd_mastery.c), if a '[' is also present in the\n"
   "    dataset pathname.\n"
   "    !!! This feature must be used with care, since some programs    !!!\n"
   "    !!! (e.g., 3dbucket) will break if it is used.  A program that  !!!\n"
   "    !!! writes to a pre-existing dataset MUST NOT open that dataset !!!\n"
   "    !!! with a sub-brick selector list.                             !!!\n"
   },

  { 23,MAY,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_idcode.c to use lowercase in dataset IDcodes.\n"
   },

  { 8,JUN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added new picture to splash screen (the MCW gang).  Also, added\n"
   "   AFNI_IMAGE_PGMFILE environment variable to afni_splash.c\n"
   },

  { 15,JUN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added option '-nscale' to 3dcalc.c [that damn Tom Ross again].\n"
   "* Added an SCO Makefile [from Jason Bacon] and a couple of patches\n"
   "   to go with it.\n"
   "* Added 'Save to PPM' button to color pbar popup menus, to let the\n"
   "   user save a representation of the color overlay scheme.\n"
   "* Fixed check_pixmap in pbar.c to be correct in TrueColor.\n"
   "* Added 'Tran 0D' to color pbar popup for AFNI controllers (but not\n"
   "   for the rendering plugin).\n"
   },

  { 16,JUN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Tran 2D' to color pbar popup for AFNI controllers.\n"
   "* Fixed 'Save to PPM' to save a check pattern for the 'none' color.\n"
   },

  { 19,JUN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to set initial string in a 'user-typein' string field\n"
   "   in PLUTO_add_string().\n"
   "* Created plug_environ.c.\n"
   },

  { 30,JUN,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved plug_environ.c into main afni, on the Misc menu.  Modified\n"
   "   places where environment variables are used to allow this\n"
   "   'Edit Environment' feature to work.\n"
   "* Added '|FFT()|' 1D function to afni.c.\n"
   "* Created program Xphace.c.\n"
   },

  { 3,JUL,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 2DChain pseudoplugin.\n"
   },

  { 11,JUL,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dZeropad.c.\n"
   },

  { 12,JUL,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added Left-Right mirror option to afni_graph.[ch] and afni.c to\n"
   "   make graph pixel layout correspond to images if left-is-left.\n"
   },

  { 17,JUL,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added new program 3dTagalign.c.\n"
   "* Fixed a bug in the 'Write' callback in plug_tag.c.\n"
   },

  { 20,JUL,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTagalign.c to write matrix+vector to a file and to\n"
   "   the output dataset header.\n"
   "* Modified 3drotate.c to read matrix+vector of transformation from\n"
   "   a file or from a dataset header.\n"
   },

  { 21,JUL,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added AFNI_ENFORCE_ASPECT to afni_pplug_env.c.\n"
   "* Added AFNI_IMAGE_PGMFILE_[1-9] to afni_splash.c.\n"
   },

  { 8,AUG,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Used thd_floatscan() to check/fix possible float errors from\n"
   "   user functions (in the thd_make*.c functions and in afni_fimmer.c).\n"
   "   Was prompted by errors produced in Ziad Saad's Hilbert plugin.\n"
   },

  { 9,AUG,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_floatscan.c to use finitef() on Linux.\n"
   "* Modified 3dcalc.c to use thd_floatscan() to check results.\n"
   "* Modified 3dmerge.c to have -1fmask option to restrict filtering,\n"
   "   and -1filter_expr option to do arbitrary linear filtering.\n"
   "   (Also changed editvol.h, edt_checkargv.c, edt_filtervol.c, and\n"
   "    edt_onedset.c)\n"
   },

  { 22,AUG,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'To Disk?' option to plug_maskave.c for the 'doall' case.\n"
   },

  { 24,AUG,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified most places where SESSTRAIL is used to use SESSTRAIL+1,\n"
   "   so that when filenames are shown, they show all the directories\n"
   "   that are shown in the 'Switch Session' dialog. [Per the suggestion\n"
   "   of Florian Hauch, Munich.]\n"
   "* Also modified afni_pplug_env.c to enable AFNI_SESSTRAIL to be\n"
   "   controlled interactively, and to have it modify the window titles\n"
   "   and session lastnames when AFNI_SESSTRAIL is altered.\n"
   },

  { 1,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated changes from Ziad Saad to the Hilbert delay plugin.\n"
   "* Modified plug_nudge.c to output a 3drotate command line to stderr\n"
   "   when the feeble user presses the new 'Print' button.\n"
   "* Added call to AFNI_imseq_clearstat() to afni_plugin.c function\n"
   "   PLUTO_dset_redisplay().\n"
   },

  { 4,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -rotcom output to 3dvolreg.c (and hidden -linear option).\n"
   "* Modified -verbose output of mri_3dalign.c.\n"
   },

  { 11,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -dxyz=1 option to edt_*.c, to allow clustering and filtering\n"
   "   in 3dmerge.c to use fake values of dx=dy=dz=1.\n"
   "* Added -1filter_winsor to 3dmerge.c and edt_filtervol.c.\n"
   "* Added 'IJK lock' to afni.[ch] and afni_widg.c (Lock menu).\n"
   "* Added -twopass option to 3dvolreg.c.\n"
   },

  { 13,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Finalized -twopass changes to 3dvolreg.c.\n"
   "* Add -duporigin option to 3drefit.c.\n"
   },

  { 14,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added check of grid spacings to 3dvolreg.c\n"
   },

  { 15,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -nscale option to 3dmerge.c\n"
   },

  { 21,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 32 to mri_nsize.c.\n"
   "* Added AFNI_SPLASH_XY to afni_splash.c.\n"
   },

  { 22,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added isqDR_setifrac and isqDR_setmontage to imseq.c.\n"
   "* Added graDR_setmatrix, graDR_setgrid, and graDR_setpinnum to afni_graph.c.\n"
   "* Added PLUTO_set_topshell() to afni_plugin.c, and made several\n"
   "   PLUGIN_CALL_IMMEDIATELY plugins use this to set the toplevel shell\n"
   "   for later AFNI manipulation.\n"
   "* Modified afni_graph.[ch] to allow a graph window to be opened for a\n"
   "   dataset of length 1.\n"
   "* Added textgraph mode to afni_graph.c, and also changed the baseline\n"
   "   pushbutton to a toggle.\n"
   },

  { 25,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -layout option to allow user to control initial window setup.\n"
   "   Most changes in afni_splash.c, but also in afni.c, afni_widg.c,\n"
   "   afni.h, etc.\n"
   "* Modified imseq.c to detect Expose events that have resized the\n"
   "   image display window -- this happens sometimes when using -layout.\n"
   },

  { 27,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Program xiner.c writes out Xinerama info about the display, if\n"
   "   it is available.  This can be used to generate an X11 resource\n"
   "   AFNI.xinerama.\n"
   "* If AFNI.xinerama is detected, then new routine RWC_xineramize (in\n"
   "   xutil.c) can be used to adjust location of a rectangle to be\n"
   "   sure it fits on a sub-screen.  Older routine RWC_visibilize_widget\n"
   "   now uses this.  New callback RWC_visibilize_CB can be used to make\n"
   "   sure menus pop up entirely on one sub-screen.\n"
   "* Many places now use visibilize to make sure dialogs and menus pop\n"
   "   up on 1 sub-screen.  See, for example, all uses of the function\n"
   "   RWC_visibilize_widget() and the macro VISIBILIZE_WHEN_MAPPED().\n"
   },

  { 29,SEP,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.[ch] to move FIM and Opt menus onto private\n"
   "   menubars.  (Got rid of grapher->option_mbar everywhere.)\n"
   "* Also put a Form inbetween the afni_graph shell and the drawing\n"
   "   area.  The option_rowcol that holds all the menu buttons is\n"
   "   now a child of the Form, not the drawing area.  This makes\n"
   "   it possible to popup the menus with Button3 (the Button3\n"
   "   popup on the drawing area interfered with this).\n"
   },

  { 1,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Wrote program 3dUndump.c (create a dataset from an ASCII list\n"
   "   of voxels, like an inverse to 3dmaskdump.c).\n"
   },

  { 4,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Put most of the work of 3dZeropad.c into function thd_zeropad.c\n"
   "   instead.\n"
   "* Added plug_zeropad.c.\n"
   },

  { 9,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Enabled startup of RT plugin, and control of all options, via\n"
   "   environment variables AFNI_REALTIME_Something.\n"
   "* Modified PLUTO_string_index() in afni_plugin.c to ignore blanks\n"
   "   and case.\n"
   },

  { 11,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Replaced XtAppAddWorkProcess in afni.c with PLUTO_register_workproc,\n"
   "   since on Mandrake 7.01, dual P-III, XFree86 4.01 system, the\n"
   "   realtime plugin workprocess doesn't start properly (some\n"
   "   interaction with the startup workprocess, which does not occur\n"
   "   on other systems).\n"
   "* Modified afni_splash.c to change controller crosshairs to Single\n"
   "   mode if montage spacing is 1.\n"
   "* Modified afni_graph.c to change grid when program alters pin_num\n"
   "   (but not when user does directly).\n"
   "* Modifed plug_realtime.c to start work process after a timeout.\n"
   "* Added PLUTO_register_timeout() afni_plugin.[ch], to execute a\n"
   "   function after a given number of ms.\n"
   },

  { 12,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c to redraw with autoscale when matrix or\n"
   "   length is changed by an external (isqDR_) command (not by\n"
   "   the user directly).\n"
   "* Modified coxplot/plot_motif.c to get initial geometry of graph\n"
   "   shell from an environment variable.\n"
   "* Minor changes to plug_realtime.c to make various things nicer.\n"
   },

  { 13,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c to insert its own Xt event loop to deal\n"
   "   with interface-freeze when the images are being slammed in as\n"
   "   fast as possible (function RT_process_xevents).\n"
   },

  { 16,OCT,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified fftest.c to allow use of FFTW; do 'make fftwest' and\n"
   "   run with nvec = -1.\n"
   },

  { 20,NOV,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in -rlt+ option of 3dTcat.c: qmid was 0.5*ns, but\n"
   "   should have been 0.5*(ns-1).  This makes the baseline wrong\n"
   "   in each voxel.\n"
   },

  { 24,NOV,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to eliminate duplicate rendering of datasets\n"
   "   when loading a script that changes dataset index.  This was caused\n"
   "   by the dataset index CB routine calling draw, and then the script\n"
   "   controller calling draw again.  Now, the routine that loads the\n"
   "   widgets from the rendering state sets a variable that stops drawing\n"
   "   if it should occur, then unsets this variable after widgets are\n"
   "   loaded.\n"
   "* Modified 3drotate.c to have new -points option, to rotate a set of\n"
   "   (x,y,z) triples using the same rotation as would be used for a\n"
   "   dataset brick.\n"
   "* Modified 3dUndump.c to check (x,y,z) coordinates for validity.\n"
   },

  { 27,NOV,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_nudge.c to reload sub-brick statistic after moving it.\n"
   },

  { 28,NOV,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_plugin_button() in afni_plugin.c to allow user to\n"
   "   have plugin buttons sorted alphabetically.\n"
   "* Fixed bug in plug_nudge.c where the interpolation mode was\n"
   "   set improperly.\n"
   },

  { 1,DEC,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Included Vinai Roopchansingh's modified plug_copy.c; this version\n"
   "   allows the user to change the datum type, when doing a zero fill.\n"
   "* Added the 'License Info' button to the Datamode->Misc menu.\n"
   },

  { 5,DEC,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Included Vinai Roopchansingh's modified 3dhistog.c; this version\n"
   "   adds the '-mask' option.\n"
   "* Fixed a bug in the PICTURE_ON/OFF macro (afni.h) so that it is\n"
   "   only meaningful for dataset viewing (not for the -im case).\n"
   },

  { 6,DEC,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_pcor.c routine PCOR_get_perc to zero out the results\n"
   "   if the last reference has no range (previously, it only zeroed\n"
   "   out the coef[] array, not bline[]).\n"
   "* Added GPL/MCW Copyright notices to many many *.[ch] files.\n"
   },

  { 9,DEC,2000 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dNotes.c and thd_notes.c to allow user to replace the History\n"
   "   note, rather than just append to it.\n"
   "* Modified 3dvolreg.c to make the -twopass feature more robust for\n"
   "   registering SPGR images.  Changes included a coarse grid search for\n"
   "   an initial shift, and fading out the weights along the edges.\n"
   },

  { 16,JAN,2001, RWC, "AFNI-general", SUPERDUPER, TYPE_GENERAL, "Older History stuff",
   "  ===========================================================\n"
   " == All changes from this date onwards were made at the NIH ==\n"
   "  ===========================================================\n"
   },

  { 23,JAN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c MAIN_workprocess() function to exit properly when\n"
   "   the 'nodown' variable is set.  Before, if a layout was set, then\n"
   "   the layout code did the splashdown, and then the MAIN_workprocess()\n"
   "   never signaled that it was done.\n"
   "* Modified thd_trusthost.c to use '192.168.' as the prefix for Class B\n"
   "   subnets, rather than '192.168.0.'.\n"
   "* Modified mrilib.h to change my e-mail address macro.\n"
   },

  { 24,JAN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dclust.c to use '-dxyz=1' option.\n"
   "* Modified afni.c so that graphs of anat buckets interact correctly when\n"
   "   the anat bucket chooser is changed, or the graph 'time index' is set.\n"
   },

  { 25,JAN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mcw_malloc.c to use a hash table instead of a linear table.\n"
   "   This speeds things up a lot.  Also added a serial number to each\n"
   "   allocation, to help in identifying the order -- the dump is now\n"
   "   sorted by serial number.\n"
   "* Incorporated Matthew Belmonte's codes plug_threshold.c and\n"
   "   plug_permtest.c into the system.\n"
   },

  { 26,JAN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Converted dbtrace.h to debugtrace.c and debugtrace.h.  This is the\n"
   "   start of an effort to incorporate traceback into all AFNI and\n"
   "   mrilib functions.  As part of this, removed THD_DEBUG stuff from\n"
   "   all library functions.\n"
   },

  { 29,JAN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added DEBUGTHISFILE macro to debugtrace.h, and used it in thd_shear3d.c.\n"
   "* Modified 3drotate.c so that input of something like\n"
   "    -ashift 10R 0 0\n"
   "   won't have the 0s clobber the 10R; 0s with no direction code suffix will\n"
   "   now be skipped in the computation of the dataset-coordinate shift.\n"
   "* Added a few words to README.copyright, and added a Q/A about it to the FAQ.\n"
   "* Added new program 3dMean.c.\n"
   },

  { 31,JAN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_zeropad.c to keep the slice-dependent time shifts (toff_sl)\n"
   "   when adding/removing slices in the z-direction.\n"
   },

  { 1,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_info.c to print dataset center (in addition to box edges).\n"
   "* Added thd_center.c (dataset center vector) and thd_mismatch.c (check if\n"
   "   2 datasets are matched for voxel-wise comparisons).\n"
   "* Added program 3dAttribute.c, for printing out values of attributes from\n"
   "   dataset header files (for use in scripts).\n"
   },

  { 2,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added function NIH_volpad (edt_volpad.c) to do zeropadding on 3D arrays\n"
   "   - adapted from thd_zeropad.c.\n"
   "* Added THD_rota_setpad to thd_rot3d.c to set zeropadding amount for\n"
   "   rotations.\n"
   },

  { 5,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mayo_analyze.h to define the structure of an ANALYZE 7.5 .hdr\n"
   "   file, and then added mri_read_analyze75() to mri_read.c.\n"
   "* Modified to3d.c to use image dimensions if they are set from the\n"
   "   image file (as is possible with ANALYZE .hdr files, for example).\n"
   "* Modified mri_new.c to set dw=-666 as a flag that the d? variables\n"
   "   defaults of 1.0 haven't been changed.\n"
   "* Modified thd_rot3d.c to allow setting of zeropad amount via\n"
   "   environment variable AFNI_ROTA_ZPAD.\n"
   "* Modified 3drotate.c and 3dvolreg.c to use new command line option\n"
   "   '-zpad n'.\n"
   "* Modified to3d.c to use -zpad option to add zero slices in the z-direction.\n"
   "   Also added attribute to header (TO3D_ZPAD) to mark this fact.  When such\n"
   "   a dataset is used with -geomparent, then it's zero padding will be\n"
   "   subtracted off when setting the new dataset's origin.\n"
   "* Modified 3dAttribute.c to have '-all' and '-name' options.\n"
   },

  { 6,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified vecmat.h to have separate types and macros for float and\n"
   "   double vectors/matrices.  Modified all places that used the old\n"
   "   'FLOAT_TYPE double' method to use the new types/macros (mostly\n"
   "   the 3D rotation codes).\n"
   "* Modified 3dvolreg.c to write rotation parameters, matrices, etc.\n"
   "   to the header of the output dataset.\n"
   },

  { 7,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dZeropad.c to have '-z' option to add slices (evenly) in\n"
   "   the dataset z-axis (slice) direction.\n"
   "* Modified 3drotate.c to deal with -rotparent and -gridparent options.\n"
   },

  { 8,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Debugged several stupid errors in 3drotate.c changes of yesterday.\n"
   "* Implemented program cat_matvec.c to catenate matrix+vector\n"
   "   transformations.\n"
   "* File thd_read_matvec.c reads a matrix+vector (THD_dvecmat; cf.\n"
   "   vecmat.h) from a file, or from a dataset attribute.\n"
   },

  { 12,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_volpad.c to allow for unsymmetric padding.\n"
   "* Modified thd_zeropad.c to allow for producing an empty copy of the\n"
   "   zero-padded dataset (so that you have to fill it up later).\n"
   "* Modified 3drotate.c and 3dvolreg.c to adjust the time-offset z-origin\n"
   "   when the new dataset axes z-origin is adjusted.\n"
   "* Added a rint() function to mri_free.c (why there? why not?) if NO_RINT\n"
   "   is defined in machdep.h -- that way I don't have to worry about it.\n"
   "* Modified 3drotate.c to shift time-offsets when the dz is large enough\n"
   "   to warrant it.\n"
   "* Modified 3drefit.c to add new '-notoff' option, to reset time-offsets\n"
   "   to zero.\n"
   "* Modified to3d.c to include 'AFNI' in fallback resources, per the\n"
   "   suggestion of J Bacon of MCW (copying what AFNI itself does).\n"
   },

  { 13,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_shear3d.c function rot_to_shear_matvec() to modify the\n"
   "   input matrix to make sure it is orthogonal (using the transpose of\n"
   "   DMAT_svdrot(q) as the orthogonal matrix closest to q).  User-input\n"
   "   matrices may not be precisely orthogonal, which leads to problems\n"
   "   in the shear calculation.  Ugh. Squared.\n"
   "* Added function THD_rotcom_to_matvec() to thd_rotangles.c -- this computes\n"
   "   the matrix/vector of a transformation, given the '-rotate .. -ashift ..'\n"
   "   string.\n"
   "* Modified thd_zeropad.c (and 3dZeropad.c) so that padding can be specified\n"
   "   in mm instead of slices.\n"
   },

  { 14,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dvolreg.c to include -rotparent/-gridparent options, a la\n"
   "   3drotate.c.\n"
   "* Edited edt_volpad.c to include new function MRI_zeropad().\n"
   "* Modified thd_read_vecmat.c to include '-rotate ...' input option.\n"
   "* Added new function THD_rota3D_matvec() to thd_rot3d.c.\n"
   "* Added multiply and inverse macros for double vecmats to vecmat.h\n"
   "* Moved function DBLE_mat_to_dicomm() from 3drotate.c to thd_rotangles.c.\n"
   "* Replaced all copies of axcode() and handedness() with the library\n"
   "   calls to THD_axcode() and THD_handedness() in thd_rotangles.c\n"
   "   (changes to 3drotate.c, 3dvolreg.c, plug_nudge.c, plug_realtime.c,\n"
   "   and plug_volreg.c).\n"
   },

  { 15,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_tshift.c, which does what 3dTshift.c does (in place).\n"
   "* Used this to add -tshift option to 3dvolreg.c.\n"
   "* Also added -ignore option to 3dTshift.c.\n"
   },

  { 16,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added traceback information to mcw_malloc.c.\n"
   "* Added program 3dOverlap.c.\n"
   "* Added function THD_newprefix() in thd_newprefix.c.\n"
   },

  { 20,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added EXIT and TRACEBACK macros to debugtrace.h.\n"
   },

  { 21,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to disable use of mmap() from malloc() on Linux\n"
   "   (malloc() would fail when memory was still available!).\n"
   "* Modified thd_mastery.c to force dset[] inputs to be relabeled\n"
   "   as bucket types if they are not time-dependent.  This is to\n"
   "   prevent interpretation as fico, etc.\n"
   "* Modified 3dmerge.c to allow use of sub-brick selectors on input\n"
   "   datasets.\n"
   "* Modified thd_trusthost.c to make it easier to add hosts by name\n"
   "   or by number, using a new function TRUST_addhost().\n"
   "* Added function PLUTO_turnoff_options() to afni_plugin.c; used this\n"
   "   in plug_realtime.c to turn off all input options after processing.\n"
   "* Added AFNI_TRUSTHOST to afni_pplug_env.c so that user can add trusted\n"
   "   hosts (for plugins/plugouts) interactively.\n"
   "* Modified thd_mastery.c and thd_loaddblk.c to allow for sub-ranging\n"
   "   on dataset input, using the <a..b> syntax on the command line.\n"
   },

  { 22,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dClipLevel.c to estimate the cutoff value to excise\n"
   "   background regions from anatomical datasets (especially EPI).\n"
   "* Added AFNI_CWD variable to afni_pplug_env.c, to let user change\n"
   "   working directory (where output files like 'Save:One' go).\n"
   "* Modified 3dOverlap.c to optionally save counts to a new dataset.\n"
   "* Fixed thd_mastery.c so that <a..a> works properly (<= vs. <).\n"
   },

  { 26,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plot_cox.c to allow copying, rescaling, and appending of\n"
   "   vector plots, and plot_x11.c to allow setting rendering to a\n"
   "   sub-box of the window (rather than the whole window).\n"
   "* Modified imseq.[ch] to allow fetching of a vector plot to be\n"
   "   rendered into the window.\n"
   "* Modified afni_splash.c to draw 'friends' stuff into the splash\n"
   "   screen (after the first popup).\n"
   "* Added function mri_zeropad_2D() in mri_zeropad.c.  Changed name\n"
   "   of MRI_zeropad() to mri_zeropad_3D() in edt_volpad.c.\n"
   },

  { 27,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added macro SAVEUNDERIZE() to xutil.h, and used it to make popup\n"
   "   menus have the SaveUnder property in imseq.c, afni_graph.c,\n"
   "   afni_widg.c, and plug_render.c.\n"
   "* Modified imseq.c to use AFNI_IMAGE_MINFRAC environment variable\n"
   "   to set minimum size for image windows.  Also added this to the\n"
   "   variables that can be controlled by afni_pplug_env.c.\n"
   "* Added fields does_saveunders and does_backingstore to display.[hc]\n"
   "   (but don't use them anywhere - yet).\n"
   "* Modified thd_mismatch.c to do MISMATCH_DELTA only if voxel sizes\n"
   "   differ by at least a factor of 0.001 (rather than perfect ==).\n"
   "   Also fixed a typobug where the datasets would always compare\n"
   "   as identical.\n"
   "* Modified 3dvolreg.c to fail if stupid users try to register dataset\n"
   "   to base that doesn't match.\n"
   },

  { 28,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dZregrid.c is for resampling a dataset along the slice-\n"
   "   direction to change the grid spacing, etc.  Mostly intended to\n"
   "   fixup user stupidities (e.g., not acquiring data on the same subject\n"
   "   using the same slice thickness; d'oh!).\n"
   "* Modified thd_filestuff.c to remove '/' from THD_filename_ok() function.\n"
   "   This lets the '-prefix' option of 3d* programs put the output dataset\n"
   "   into a new directory.  New function THD_filename_pure() also checks\n"
   "   for the '/'.\n"
   },

  { 29,FEB,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_initdkptr.c to properly treat directory components stored\n"
   "   in prefixname.  [Ugh]\n"
   },

  { 1,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Max Count' input to plug_histog.c.\n"
   "* Fixed incorrect error message for '-nrep' option in 3dWinsor.c.\n"
   "* Added -blackman and -hamming options to 3dTsmooth.c [per MSB].\n"
   },

  { 2,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_entropy16.c to compute entropy (bits/short) of a dataset.\n"
   "* Used entropy to force gzip of highly redundant datasets in\n"
   "   thd_writedblk.c -- if AFNI_AUTOGZIP is on.\n"
   "* Modified afni_environ.c so that yesenv() and noenv() use my_getenv().\n"
   "* Modified afni_pplug_env.c to include AFNI_AUTOGZIP.\n"
   "* Modified afni.c to put DBG_trace= statements inside #ifdef USE_TRACING.\n"
   "* Modified 3dZeropad.c to not overwrite existing datasets [Oopsie squared].\n"
   "* Modified 3dmerge.c to print warning message for default indexes\n"
   "   (if -1dindex and/or -1tindex is not used).\n"
   "* Added 3ddelay.c from Ziad Saad.\n"
   },

  { 3,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTsmooth (yet again) to allow for different options for\n"
   "   dealing with points past the beginning and end of time.\n"
   },

  { 4,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program ent16.c computes 16-bit entropy of stdin stream.  Can be\n"
   "   used in a script to decide when to gzip datasets.\n"
   },

  { 5,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified Ziad Saad's plug_delay_V2.h to use remainder() instead of\n"
   "   drem() for Solaris, since Solaris doesn't have that BSD function\n"
   "   for some stupid SysV reason.\n"
   },

  { 6,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified display.[ch] to store RGB bytes for the underlay and overlay\n"
   "   colors in the MCW_DC structure.  This is preparation for allowing\n"
   "   direct RGB overlays into imseq.c.\n"
   "* Modified mri_new.c (and mcw_glob.c) to allow use of the special filename\n"
   "   ALLZERO in 3D: input.  This will make it simple to create an empty\n"
   "   dataset using to3d, for example.\n"
   "* Added -min and -max options to 3dTstat.c.\n"
   "* Modified 3dClipLevel.c to use all sub-bricks instead of just the first.\n"
   "* Added function ISQ_overlay() into imseq.[ch], to do the overlaying of\n"
   "   underlay and color of MRI_short and MRI_rgb in all cases.\n"
   },

  { 7,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to add controls and functions for translucent color\n"
   "   overlays, if X11 Visual is TrueColor.\n"
   "* Modified to3d.c, afni_plugin.c, and plug_render.c to turn off the\n"
   "   overlay opacity arrowval for their particular image windows\n"
   "   (since they don't use overlays anyhoo).\n"
   "* Modified rwc.xbm logo bitmap to include NIH logo along with MCW logo.\n"
   },

  { 8,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added median+MAD function to cs_qmed.c.\n"
   "* Added display of median+MAD to popup stats in afni_graph.[ch].\n"
   "* Added thd_dsetrow.c, thd_rowfillin.c, 3dRowFillin.c to do row filling\n"
   "   between gaps (zeros) of like values.  Intended to complement the\n"
   "   drawing plugin.\n"
   "* Modified plug_render.c to allow user to display the xhairs in the\n"
   "   color overlay (meaning they can ShowThru).\n"
   },

  { 9,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed 1deval.c so that '-v' option becomes '-verb' (since '-v' had\n"
   "   another meaning in that program).  Also modified several other\n"
   "   programs so that '-verb' is accepted (instead of '-v' or '-verbose').\n"
   "* Modified imseq.c to de/re-sensitize overlay opacity arrowval when\n"
   "   'See Overlay' is turned off/on.\n"
   },

  { 12,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to read Siemens Vision format .ima files.\n"
   "* Modified mrilib.h to have global variables MRILIB_* that hold\n"
   "   orientation info from image files, if present.  Modified to3d.c\n"
   "   to use this information.\n"
   "* New program siemens_vision.c prints out info from the .ima format\n"
   "   files' header.\n"
   },

  { 15,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in thd_shift2.c: when the shift was larger than the data\n"
   "   line size, buffer overrun could occur.  In this case, the proper\n"
   "   result is all zeros, so that's what I substituted.\n"
   },

  { 19,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in thd_dsetrow.c, where putting a row back in for byte\n"
   "   or float cases didn't have break statements, so it would be\n"
   "   put twice, which is real bad (i.e., segment fault).\n"
   "* Added Linear Fillin to plug_drawdset.c.\n"
   },

  { 20,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 3dDeconvolve.c update from Doug Ward.\n"
   "* Modified plug_histog.c to add 'Aboot' feature.\n"
   },

  { 21,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot to draw filled rectangles when thickness is set\n"
   "   to -1.0.  Files affected are coxplot.h, plot_cox.c, plot_ps.c,\n"
   "   plot_ps2.c, and plot_x11.c.\n"
   "* Added program 1dgrayplot.c to plot timeseries files (vertically)\n"
   "   in grayscale, a la SPM.\n"
   "* Modified afni_plugin.c to remove the MCW_TIMER_KILL from the\n"
   "   ButtonPress frivolity in image popups.  Also, hid this stuff\n"
   "   behind the NO_FRIVOLITIES macro and NO_frivolities global variable.\n"
   "* Added program 3dToutcount.c to count outliers at each time point in\n"
   "   a 3D+time dataset (idea from Bill Eddy).\n"
   },

  { 22,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -save option to 3dToutcount.c, to leave markers of outliers\n"
   "   behind.\n"
   "* Added script moveAFNI to install changed files from afni98.tgz into\n"
   "   the current directory.\n"
   },

  { 23,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_fetchdset.c, to get a dataset from a Web link.  This is\n"
   "   called from THD_open_dataset() in thd_mastery.c.  Thus, you can\n"
   "   now do something like\n"
   "     afni -dset http://some.place/dir/anat+orig\n"
   "   and see the dataset!\n"
   },

  { 26,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c, afni_widg.c to add a 'Read Web' button.\n"
   "* Modified thd_fetchdset.c to allow fetch of *.1D files as well.\n"
   },

  { 30,MAR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Rota' buttons to afni_widg.c and plug_render.c, to rotate\n"
   "   the colors on the pbars.\n"
   "* Added range hints to the pbar in afni_func.c and plug_render.c.\n"
   },

  { 3,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified xim.[ch] to add a 'force rgb' option to function XImage_to_mri(),\n"
   "   so that one can always be sure of getting a MRI_rgb image (and so be\n"
   "   sure of writing a PPM file with mri_write_pnm()).  Modified imseq.c and\n"
   "   afni_graph.c to fit the new usage.\n"
   },

  { 10,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in 3dvolreg.c, where the '-base dset' option used a\n"
   "   sub-brick index 'bb' that was no longer initialized (bug reported\n"
   "   by Tom Ross of MCW).\n"
   },

  { 18,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added some more printouts to 3dcalc.c and thd_loaddblk.c when malloc()\n"
   "   fails on dataset input.\n"
   "* Added '-sum' and '-sqr' options to 3dMean.c.\n"
   "* Added program 1dsum.c.\n"
   "* Added machdep.c to do machine-dependent runtime startup (supplementing\n"
   "   machine-dependent compiletime stuff in machdep.h).\n"
   },

  { 20,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'addto_args' to a bunch of programs, to let them use the '-@'\n"
   "   command line switch.\n"
   "* Added call to machdep() in a bunch of programs.\n"
   },

  { 23,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to draw 'EMTPY IMAGE' string into image window if\n"
   "   a NULL image is returned.\n"
   },

  { 24,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'record' capability to imseq.[ch].\n"
   },

  { 25,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c recording a little.\n"
   "* Fixed a bug in 3dAttribute.c in which the tross_Expand_string()\n"
   "   result might be NULL, in which case printing it would crash\n"
   "   on some systems.  Also, free()-ed the data from that call.\n"
   },

  { 30,APR,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mcw_glob.c to print an message if an attempted expansion\n"
   "   produces no files.\n"
   },

  { 4,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_loaddblk.c to print a message if a mmap()-ed file isn't\n"
   "   big enough for the dataset byte count.\n"
   },

  { 9,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a scale-resize bug in Solaris (why does this keep happening,\n"
   "   and only on Solaris?!) in afni.c, afni_func.c, and plug_render.c.\n"
   },

  { 10,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in to3d.c with the Analyze/Siemens header geometry info\n"
   "   it was only processed AFTER the autosave test was executed.  It was\n"
   "   moved before the autosave.  Also added the -zorigin option to let\n"
   "   the user set the slice offset (a la 3drefit).\n"
   },

  { 16,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified waver.c to add new -tstim option (for specifying stimulus\n"
   "   times directly on command line).\n"
   },

  { 18,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugin.[ch] to STATUS() an error message if a\n"
   "   plugin library cannot be loaded into the system.\n"
   },

  { 22,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_iochan.[ch] to add function iochan_recvloop(), which\n"
   "   loops to receive as much data as possible without waiting.\n"
   },

  { 23,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_iochan.[ch] to add function iochan_fork_delay(),\n"
   "   which allows relays of data from a shm IOCHAN to a tcp IOCHAN\n"
   "   through a child process.\n"
   },

  { 24,MAY,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugin.c to fprintf(stderr) the error message if\n"
   "   a plugin library cannot be loaded into the system.  (This way\n"
   "   it always appears, even if trace is turned off.)\n"
   },

  { 4,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_iochan.c iochan_fork_delay() to retry communications,\n"
   "   and to print more error messages.\n"
   "* Added DONT_USE_DEBUGTHISFILE to machdep.h and debugtrace.h.\n"
   },

  { 5,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugin.c to move some routines into the 'always\n"
   "   compiled' section; and afni_plugin.h to reflect this change.\n"
   "   Small changes also to afni.h, afni_func.c, and afni_pplug_*.c.\n"
   "   (All of this is to get afni to compile again without plugins.)\n"
   },

  { 6,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added directory qhulldir/ to contain the qhull program from UMN.\n"
   "* Fixed afni_vcheck.c to reflect the new webserver.\n"
   },

  { 7,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add cs_qhull.c to drive qhull program and compute Voronoi areas\n"
   "   on sphere surface.\n"
   },

  { 18,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_ascii() in mri_read.c to skip lines that start\n"
   "   with '#' character.\n"
   },

  { 19,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3drotate.c to take the new -dfile/1Dfile options, per\n"
   "   the request of David Glahn of UCLA.\n"
   },

  { 22,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in 3dUndump.c where it checked the wrong value against\n"
   "   the z coordinate bounds - it would report illegal inputs when\n"
   "   the (x,y,z) values were in fact legal.\n"
   },

  { 26,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added function THD_is_executable() in thd_filestuff.c.\n"
   "* Added thd_getpathprogs.c to get list of all executables in the path.\n"
   },

  { 27,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to save images in various formats (.jpg, .gif, etc.)\n"
   "   to imseq.[ch], using external programs such as ppmtogif, etc.\n"
   },

  { 29,JUN,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program strblast.c to blast strings out of image files.\n"
   "* Modified 3dclust.c to use THD_open_dataset() instead of\n"
   "   THD_open_one_dataset().\n"
   },

  { 3,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to correct usage of pnmtotiff, which is NOT the\n"
   "   same as ppm2tiff (first one writes to stdout, second to a file\n"
   "   named on the command line).\n"
   },

  { 5,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.[ch] to include CM's from Talairach Daemon database,\n"
   "   and re-enabled the long-dormant 'Talairach to' button.\n"
   "* Added option '-nosum' to 3dclust.c.\n"
   "* Modified thd_loaddblk.c to skip byte order tests if all sub-bricks\n"
   "   have byte datum.\n"
   },

  { 6,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dUndump.c to print a message if a voxel is written to\n"
   "   more than once.\n"
   "* Added Doug Ward's changes to 3dDeconvolve.c, 3dConvolve.c, etc.\n"
   },

  { 9,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_transform_vector() in afni.c to use anat_parent\n"
   "   transformations if the datasets aren't directly related.\n"
   "* Used this to modify the 'Talairach to' feature (AFNI_talto_CB() in afni.c)\n"
   "   to allow jumping even if the dataset is not in Talairach view, but just\n"
   "   has a way of transforming the vector to Talairach view.\n"
   },

  { 10,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Where Am I?' Talairach Daemon feature to afni_widg.c, afni.[ch].\n"
   "   Actual calculations done in thd_ttatlas_query.c.\n"
   "* Modified xutil.[ch] to add function to alter contents of a textwin.\n"
   "* Added edt_sortmask.c to sort a MCW_cluster based on its contents.\n"
   },

  { 11,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified xutil.[ch] to add function MCW_unregister_hint().\n"
   "* Modified afni.c to add hint/help to 'Where Am I?' textwin (and to\n"
   "   remove help when the window is destroyed).\n"
   },

  { 12,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added afni_ttren.c, to control the colors of the display of the\n"
   "   Talairach atlas regions (when I get around to it, that is).\n"
   },

  { 13,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* OK, made afni_ttren.c work with the volume renderer.\n"
   "* Modified mcw_glob.c to only print warning message if allowed to.\n"
   "   Modifed various codes that use MCW_file_expand() to turn such\n"
   "   messages on or off, depending on what I feel like.\n"
   },

  { 24,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_ttatlas_query.c to return up to 9 results, and to print\n"
   "   a cautionary tail.\n"
   "* Modified plug_render.c to display TT atlas regions.\n"
   },

  { 25,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_render.c to histogram new dataset bricks and choose\n"
   "   99.5% point as the default upper level for scaling.\n"
   "* Modified afni.c, afni_func.c, afni_widg.c, afni_warp.c to enable\n"
   "   rendering of TT Atlas regions as overlays in 2D image viewers.\n"
   },

  { 26,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to allow 'Save One' to be independent of the\n"
   "   output filter (e.g., so can save a montage into a JPEG file).\n"
   },

  { 27,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to add 'Sav:aGif' to save a sequence of images\n"
   "   into animated GIF format (using gifsicle or whirlgif).\n"
   "* Included gifsicle directory into AFNI distribution, and added to\n"
   "   Makefile.INCLUDE to make gifsicle (whirlgif was already there).\n"
   "   However, neither of these program is made by default.\n"
   },

  { 29,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to add a range hint to the intensity bar.\n"
   },

  { 30,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed color pbar hintize alterations when user changes sub-bricks,\n"
   "   in plug_render.c.\n"
   },

  { 31,JUL,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified xutil.[ch] to add a routine to NULL out an arbitrary pointer\n"
   "   when a widget is destroyed.\n"
   "* Used the above in afni.c to NULL out the pointer to the 'Where Am I?'\n"
   "   text window when the text window is destroyed because the user\n"
   "   destroyed its parent imseq.\n"
   },

  { 1,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Realized that the TT atlas overlay was setup for the small Talairach\n"
   "   box, and most people now have large Talairach box datasets.  To make\n"
   "   the system work, modified thd_ttatlas_query.c to allow programs to\n"
   "   load an atlas with 141 I-S slices (the one on disk), or an atlas\n"
   "   with 151 I-S slices (created via zero-padding).  Then modified places\n"
   "   that retrieved the atlas dataset: afni_func.c, plug_render.c.  Ugh.\n"
   "* Also modified afni.c so that the 'Atlas Colors' popup button is only\n"
   "   enabled in +tlrc view.\n"
   "* Modified waver.c to add to -EXPR option.\n"
   "* Added -stdin option to 1dplot.c.\n"
   },

  { 2,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in waver.c -EXPR option, so that if waveform_EXPR(t) is\n"
   "   called with t < 0, it returns 0.\n"
   "* Included mpeg_encode directory into AFNI distribution, and added to\n"
   "   Makefile.INCLUDE to make mpeg_encode.\n"
   "   However, neither of these program is made by default.\n"
   "* Added Sav:mpeg to imseq.[ch].\n"
   },

  { 6,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_motif.c to have plotting window close when\n"
   "   user presses 'Q' or 'q' keystroke in the drawing area.\n"
   },

  { 7,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.[ch] and afni.c to extend graph baseline concept\n"
   "   to have a global baseline for all graphs (that doesn't change when\n"
   "   the user moves around).\n"
   "* Modified afni_graph.[ch] to add a new Double Plot mode Plus/Minus to\n"
   "   make the transformed function be added/subtracted from the dataset's\n"
   "   time series, so we can see error bars from 3dDeconvolve's\n"
   "   -iresp/-sresp outputs!\n"
   "* Added afni_pplug_1dfunc.c to make 1DChain pseudoplugin (also affected\n"
   "   afni_func.c and afni_widg.c).\n"
   },

  { 8,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c and xutil.c to use new environment variable\n"
   "   AFNI_DONT_MOVE_MENUS - if this is 'Yes', then the menu movement\n"
   "   functions will be skipped.  Also added this to afni_pplug_env.c.\n"
   "   (As usual, this is in response to a problem on Solaris.)\n"
   "* Added program 3dZcutup.c to cut slices out of a dataset.\n"
   "* Modified various functions to work with single-slice datasets.\n"
   "   Probably missing some still.\n"
   },

  { 9,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dZcat.c to put datasets together in the slice\n"
   "   direction.\n"
   "* (Re)modified to3d.c to allow creation of 1 slice datasets.  Hope it\n"
   "   works out this time.\n"
   },

  { 10,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added TENT() function to parser.f and parser.inc.\n"
   "* Added thd_dsetto3D.c, to extract/scale a float copy of a sub-brick.\n"
   "* New program 3dTqual.c computes a 'quality index' of each sub-brick\n"
   "   in a 3D+time dataset.\n"
   "* Modified 1dplot.c so that -stdin option can read more than 1 column.\n"
   },

  { 11,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_scatplot.c to print correlation coefficient.\n"
   },

  { 12,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3dClipLevel.c that used 1 past the end of the histogram\n"
   "   array (bad Bob, bad).\n"
   "* Added functions thd_median_brick() and thd_cliplevel() in files\n"
   "   THD_median.c and THD_cliplevel.c to the library.\n"
   "* Modified 3dTqual.c to use these library functions.\n"
   "* Modified 3dToutcount.c to have -autoclip option.\n"
   },

  { 13,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -dt option to 3dcalc.c.\n"
   "* Added routine AFNI_logger() in afni_logger.c.\n"
   },

  { 14,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified a bunch of programs to use AFNI_logger().\n"
   },

  { 15,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dToutcount.c to have a -range option, a la 3dTqual.c.\n"
   "* Added function THD_outlier_count() to do more or less what 3dToutcount.c does.\n"
   "* Used this in to3d.c to automatically check new 3D+time datasets for outliers.\n"
   },

  { 16,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to hintize the pbar in function AFNI_underlay_CB().\n"
   "* Modified the outlier stuff in to3d.c some.\n"
   },

  { 20,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated revised TD database from San Antone.  Also penciled in\n"
   "   the Nucleus Accumbens, since they seem to have missed it.\n"
   },

  { 22,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c to allow loading of overlay data from the\n"
   "   TTatlas+tlrc dataset, on a region-by-region basis.\n"
   },

  { 23,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program 3dTcorrelate.c to compute correlation coefficient of\n"
   "   2 3D+time datasets, voxel by voxel.\n"
   "* New file thd_correlate.c computes various correlation coefficients\n"
   "   between vectors.\n"
   "* Added constant detrending to thd_detrend.c.\n"
   },

  { 24,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTcorrelate.c to have '-polort -1' option.\n"
   "* Modified afni_friends.c.\n"
   "* Modified to3d.c, which tried to popup outlier message in batch mode.\n"
   "   Also, made it check for negatives again after 2swap; skip outlier\n"
   "   check if too many negatives (more than 1%); print percentage of\n"
   "   negatives in negatives report.\n"
   },

  { 26,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified to3d.c to have -save_outliers option.\n"
   },

  { 28,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_correlate.c Pearson routine to initialize sums (oops).\n"
   },

  { 29,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Created cs_sort_template.h, a file to generate quicksort functions from\n"
   "   a macro-defined type and macro-defined order.\n"
   "* Adapted 1st version of agni.[ch] to fit into afni, for surface display.\n"
   "   Changes also to afni.c (to draw the damn thing), imseq.c, and some\n"
   "   other minor junk.\n"
   },

  { 30,AUG,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_cox.c to have a flip_memplot() function, for use\n"
   "   in imseq.c graphing overlay.\n"
   "* Modified coxplot/plot_x11.c to draw a Point rather than a Rectangle\n"
   "   if the rectangle has width=height=0.\n"
   "* Modified afni.c to draw surface nodes in correct places, rather than\n"
   "   in center of their voxels.  Also involved changes to thd_coords.c,\n"
   "   to have new functions for floating point coords in FD_bricks.\n"
   },

  { 5,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified machdep.[ch] to provide some support for Mac OSX (Darwin).\n"
   "* Modified agni.c to do volume map (vmap) correctly, and faster.\n"
   },

  { 6,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugout.[ch] to have 'NO_ACK' option, so that plugout\n"
   "   messages aren't acknowledged.  This is necessary to avoid race\n"
   "   conditions with a plugout that both sends and receives messages.\n"
   "* Modified afni_plugout.[ch] to allow sending and receiving of SURFID\n"
   "   for surface node identifiers.\n"
   "* Wrote plugout_surf.c as a demo of the SURFID interchange.\n"
   },

  { 7,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified agni.[ch] to put a mask into the vmap to mark the level\n"
   "   of expansion at which the voxel was mapped.\n"
   "* Modified agni.c to using ENTRY/RETURN.\n"
   "* Modified agni.c to check for duplicate node id's when creating a surface.\n"
   "* Modified afni.c and afni_plugout.c to strip off the vmap mask when\n"
   "   querying this array.\n"
   "* Modified machdep.c to get the _Xsetlocale() fixup function; added\n"
   "   machdep() to a lot of programs (for Mac OS X compilation).\n"
   },

  { 11,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_render.[ch] and plug_render.c to allow depth cueing of\n"
   "   ShowThru overlays (new option ST+Dcue on overlay opacity menu).\n"
   },

  { 12,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Wrote thd_autonudge.c and 3dAnatNudge.c to try to fit EPI volume\n"
   "   on top of scalped SPGR volume.\n"
   },

  { 13,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugin.c and NLfit_model.c to properly load .so objects\n"
   "   on Mac OS X (DARWIN).\n"
   },

  { 17,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to add new function mri_read_ppm_header(),\n"
   "   and to make mri_read_ppm() scale a PPM image to maxval=255.\n"
   "* Modified afni_splash.c to look for .afnisplash*.ppm files for the\n"
   "   splash image override.\n"
   },

  { 18,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mri_drawing.c to draw things into RGB images, and mri_coxplot.c,\n"
   "   to use that to draw coxplot stuff into RGB images.\n"
   },

  { 19,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* In imseq.c, realized that the 'Empty Image' memplot was being re-created\n"
   "   multiple times, and this is stupid.  Now it is only created once, and\n"
   "   is re-used from then on.\n"
   "* Also in imseq.c, realized that if last image in a Save sequence is\n"
   "   NULL, and we are saving to an animation, then the animation won't be\n"
   "   written and the saved images will never be deleted.  At least they\n"
   "   will be deleted now (animation still won't be written, but at least\n"
   "   an error message will be output).\n"
   "* Also in imseq.c, added montage overlay plots to function\n"
   "   ISQ_make_montage().\n"
   },

  { 20,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to do the overlay plot montage more efficiently\n"
   "   (using less memory).\n"
   "* Modified imseq.c to draw labels returned by the get_image() function\n"
   "   for each slice, both in single and montage images.\n"
   "* Modified afni.c to return a label for each slice.\n"
   "* Modified coxplot/plot_cox.c to have new function, create_memplot_surely().\n"
   "   Modified a number of functions/programs to use this instead of looping\n"
   "   over create_memplot() a number of times.\n"
   },

  { 21,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to adjust the way labels are drawn.\n"
   "* Fixed ISQ_saver_CB() bug in imseq.c: when Save:one was active and images\n"
   "   were sent to an output filter, they needed to be forced into RGB mode\n"
   "   even if they were grayscale.\n"
   "* Changes to afni.c and imseq.c to allow label and agni overlay colors to be\n"
   "   controlled by environment variables.\n"
   "* Added function DC_parse_color() to display.[ch] to parse a color string into\n"
   "   a (float) RGB triple.\n"
   },

  { 23,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a setback environment variable to let image label placement be\n"
   "   adjusted.\n"
   "* Modified afni_widg.c to load a color pixmap for use with the 'wait'\n"
   "   picture, if the visual is TrueColor.\n"
   },

  { 24,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dcopy.c does what it sounds like: copies datasets.\n"
   "* Modified plug_drawdset.c to allow the user to make a copy of a\n"
   "   dataset on input, and to have a SaveAs button.\n"
   },

  { 25,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c to have a new drawing mode, 'Filled Curve'.\n"
   "* Modified mri_read.c to allow .HDR and .IMA suffixes for Analyze\n"
   "   and Siemens files, respectively (in addition to .hdr and .ima).\n"
   "* Modified mri_read_siemens() in mri_read.c so that if the environment\n"
   "   variable AFNI_SIEMENS_INTERLEAVE is set to 'Y' or 'y', then the\n"
   "   multi-images in a Siemens file are assumed to be interleaved rather\n"
   "   than presented in order.\n"
   "* Modified to3d.c to have new option -sinter: sets AFNI_SIEMENS_INTERLEAVE\n"
   "   to 'Yes'.\n"
   "* Modified plug_drawdset.c to do EVERYTHING_SHUTDOWN after Quit is pressed.\n"
   "   For some reason, this was blocking proper Undo-ing if the user did Quit,\n"
   "   then restarted the editor.\n"
   },

  { 27,SEP,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_md5.c to add UNIQ_idcode() function, which produces strings\n"
   "   like 'USA_mFs+P-tnCc1vQQM0UuL0Hg', using a modified Base64 encoding of\n"
   "   the MD5 sum of some system info and the current time.\n"
   "* Modified thd_base64.c to avoid use of mrilib.h (so it can be easily used\n"
   "   by non-AFNI programmers).\n"
   "* Modified thd_idcode.c to use UNIQ_idcode() instead of older method.\n"
   "* Modified 3ddata.h to extend length of MCW_idcode string to 32 (so can\n"
   "   use results of UNIQ_idcode()).\n"
   },

  { 1,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugin.c to make plugin alphabetization the default (that\n"
   "   is, the user has to 'setenv AFNI_PLUGINS_ALPHABETIZE NO' to get the\n"
   "   old behavior).\n"
   },

  { 16,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Took new FD2.c from Andrzej Jesmanowicz, with changes to run with X11\n"
   "   TrueColor visual.\n"
   },

  { 18,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_loaddblk.c to make THD_load_datablock() have only 1 argument;\n"
   "   the 'freeup' argument is moved to a file-scope variable, and is set\n"
   "   by calling new function THD_set_freeup().\n"
   "* Modified lots of programs to use modified THD_load_datablock() correctly.\n"
   "* Modified DSET_load() macro in 3ddata.h.\n"
   "* Modified afni.c to use THD_set_freeup() with AFNI_purge_unused_dsets().\n"
   "* Added macro mri_zero_image() to mrilib.h.\n"
   "* Modified thd_fdto1D.c to zero output, then only access non-NULL bricks;\n"
   "   this is to prevent crashes when a user tries to graph a dataset that\n"
   "   didn't load correctly (cf. Sally Durgerian).\n"
   "* On some Solaris systems, displayed to SGI systems (cf. Mike Beauchamp),\n"
   "   Button 3 doesn't always seem to get passed through.  The following\n"
   "   changes are to let Button 1 also popup 'hidden' menus\n"
   "     - imseq.c for wbar menu\n"
   "     - imseq.c for wimage menu (if Ctrl or Shift also pressed)\n"
   "     - afni_setup.c for Inten menu\n"
   "     - afni_func.c for Hidden menu (in logo square)\n"
   "     - plug_render.c for Inten menu and Xhair menu (latter is Button 2)\n"
   "   However, these changes don't seem to work - the mouse events aren't\n"
   "   received.  Ugh.\n"
   },

  { 19,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in imseq.c: if pnmtops was NOT found but epstopdf WAS found,\n"
   "   then the program would crash (this is the Ben Xu memorial bug).\n"
   "* Modified thd_winsor.c and 3dWinsor.c to have new '-clip' option.\n"
   },

  { 22,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3dvolreg.c about -twopass weight brick (noted by\n"
   "   William Gandler of the NIH).\n"
   },

  { 25,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-q' (quiet) option to afni.c (also affected afni.h, afni_widg.c,\n"
   "   and afni_plugin.c).\n"
   "* Fixed bug in AFNI_set_viewpoint() in afni.c, so that the AGNI node\n"
   "   is looked up only if we are in 'view brick' mode.\n"
   "* Added program Vecwarp.c, at the request of David van Essen of Wash U.\n"
   },

  { 26,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added THD_surefit_to_dicomm() to agni.[ch] (from Vecwarp.c).\n"
   "* Modified agni.c to allow SureFit coord files to be read directly\n"
   "   using <SureFit coord=filname IDadd=number/> in .SURF file.\n"
   },

  { 29,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_mincread.c to read MINC format files as AFNI datasets.\n"
   "   Also changed 3ddata.h, afni.c, thd_initsess.c, thd_mastery.c,\n"
   "   thd_opendset.c, etc., and include subdirectory netcdf-3.5.0/\n"
   "   that hold the NetCDF library.\n"
   },

  { 30,OCT,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified MINC stuff to use AFNI_MINC_FLOATIZE environment to\n"
   "   control conversion to internal floats, and to use\n"
   "   AFNI_MINC_DATASETS to control whether AFNI itself looks\n"
   "   at .mnc files.\n"
   "* Added program 3dMINCtoAFNI.c to re-write a MINC file into an AFNI\n"
   "   dataset; 3drefit may be useful afterwards.\n"
   },

  { 1,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_mincread.c to allow :step attribute of dimensions\n"
   "   to be missing (default=1), and even to allow the dimension\n"
   "   variables to be missing.\n"
   "* Modified mri_new.c to use calloc() instead of malloc().\n"
   "* Modified MCW_choose_string() in bbox.c to use length of 1st line\n"
   "   of label arg as size of text box, rather than strlen(label).\n"
   "* Modified afni_func.c to change way the 'Read Web' button presents\n"
   "   its chooser, and put some sample Web datasets on the server.\n"
   },

  { 2,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmerge.c to protest if an input dataset (for merge)\n"
   "   can't be loaded.  Also changed edt_onedset.c to use DSET_LOADED()\n"
   "   macro.\n"
   },

  { 5,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_dsetto1D.c to break THD_extract_series() into two\n"
   "   functions.  The new function THD_extract_array() returns data\n"
   "   in an array supplied by the caller.  This is used in thd_median.c\n"
   "   and thd_outlier_count.c to avoid the malloc/free cycle on\n"
   "   thousands of voxel time series.\n"
   "* Modified thd_cliplevel.c to check bounds on hist[] array when\n"
   "   loading it with shorts, and to increase size of hist[] by 1.\n"
   "   (This last problem was causing to3d to crash.)  Also modified\n"
   "   3dClipLevel.c in the same way.\n"
   "* Modified coxplot/plot_motif.c to disable 'PS->printer' button if\n"
   "   environment variable AFNI_PSPRINT isn't set.\n"
   "* Modified machdep.c to do enable_mcw_malloc() if environment variable\n"
   "   AFNI_FORCE_MCW_MALLOC is yessish.  Modified mcw_malloc.c to have\n"
   "   enable_mcw_malloc() return immediately if it is already enabled.\n"
   "* Modified qmedmad_float() in cs_qmed.c to free workspace array when\n"
   "   done with it (oopsie).\n"
   },

  { 7,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_plugout.c to call AFNI_driver() function in response\n"
   "   to 'DRIVE_AFNI' commands.\n"
   "* Added afni_driver.c and AFNI_driver() function to carry out some\n"
   "   user interface functionality from a plugout\n"
   "     - open windows, close windows\n"
   "     - switch sessions, datasets\n"
   "     - rescan this\n"
   "     - quit\n"
   "* Added a button to the Datamode->Misc menu to start plugouts.\n"
   },

  { 8,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_driver.c to allow the OPEN_WINDOW function to have\n"
   "   layout parameters geom=, ifrac=, mont=, matrix=, and pinnum=.\n"
   },

  { 9,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Replaced isblank() in afni_driver.c with isspace() - the former\n"
   "   is a GNU extension, the latter is standard.\n"
   },

  { 12,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imreg.c to have -cmass option for pre-alignment of\n"
   "   center of mass.\n"
   },

  { 13,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_driver.c to allow OPEN_WINDOW to open a plugin.\n"
   "* Modified afni_splash.c so that layout code doesn't check plugin\n"
   "   widgets if this is a custom plugin (which won't put widgets\n"
   "   into the 'plint' struct).\n"
   },

  { 14,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added OPEN_GRAPH_XY (etc.) to afni_driver.c, to display graphs\n"
   "   from plugout data.\n"
   },

  { 15,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added plot_strip.c to coxplot/, for doing timeseries plots with\n"
   "   recyling back to the left when the graph is full.\n"
   "* Added OPEN_GRAPH_1D (etc.) to afni_driver.c.\n"
   "* Added cutoff() to afni_plugout.c when it closes a socket.\n"
   },

  { 16,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_strip.c to add an X at the end of each\n"
   "    evolving graph.\n"
   "* Modified afni_driver.c to have command SET_GRAPH_GEOM.\n"
   },

  { 20,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_cox.c to have functions to convert between\n"
   "    user and memplot coordinates.\n"
   "* Modified afni_driver.c to remove debug print statements, and to\n"
   "    add some comments.\n"
   },

  { 21,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_iochan.[ch] to set an error string in some functions.\n"
   "    This can be retrieved with iochan_error_string().\n"
   "* Modified afni_plugout.[ch] to listen for connections on socket ports\n"
   "    7955..7959.\n"
   "* Modified afni_plugout.[ch] to break input command strings from plugouts\n"
   "    into substrings (separated by NULs) and execute them all.  This will\n"
   "    let AFNI catch up when a plugout races ahead during a dataset read\n"
   "    or a window resize, for example.\n"
   },

  { 27,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to apply the 'funused1' entry in the Analyze .hdr\n"
   "    file as a scale factor.\n"
   "* Added mri_scale_inplace() function (mri_scale.c).\n"
   },

  { 28,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to also allow floatizing of Analyze .img files,\n"
   "    and guessing at orientation via SPM.\n"
   },

  { 29,NOV,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* mri_write_analyze() function to write an MRI_IMAGE to Analyze files.\n"
   "* 3dAFNItoANALYZE program to write a dataset to Analyze files.\n"
   "* Added -prefix option to 3dclust.c.\n"
   },

  { 3,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to mri_read() in mri_read.c to understand GEMS headers.\n"
   "* Changes to to3d.c to understand the GEMS header stuff, including\n"
   "    a default TR (if user supplies TR=0).\n"
   },

  { 4,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Cleaned up mri_read.c and to3d.c a little.\n"
   "* Got some small changes to 3dIntracranial.c from Doug Ward (-nosmooth).\n"
   "* New program ge_header.c prints out GEMS header information.\n"
   },

  { 7,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3daxialize.c to allow arbitrary orientation of output\n"
   "    (-orient option).  Also changes to ORCODE() macro in thd.h\n"
   "    and to3d.h, and added new function to thd_fdbrick.c.\n"
   "* Modified imseq.c, afni_widg.c, afni.c, afni_graph.c, plug_render.c\n"
   "    and xutil.h to change cursor slightly when it moves over a\n"
   "    window that has a hidden Button-3 popup menu.\n"
   "* Modified 3dTstat.c to have NOD (no-detrend) versions of -stdev\n"
   "    and -cvar.\n"
   "* Modified afni_widg.c to implement AFNI_START_SMALL.\n"
   },

  { 11,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed stupid errors in the cursor stuff, and propagated the changes\n"
   "    to more places, so that most AFNI windows should now be properly\n"
   "    cursorized.\n"
   "* Fixed bug in to3d.c - 1 slice with TR=0 on command line would crash.\n"
   "* Modified bbox.[ch] to allow user to set menu column length via\n"
   "    environment variable AFNI_MENU_COLSIZE.\n"
   "* Modified bbox.c to allow user to use Button-3 to popup a list chooser\n"
   "    for optmenus.\n"
   },

  { 13,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified bbox.c to do XUngrabPointer if user presses Button-2 to try\n"
   "    to popup a list chooser for optmenus.  If the optmenu is inside a\n"
   "    popup menu, Motif never does an XUngrabPointer, thus freezing the\n"
   "    X server until afni is killed from outside.  This seems to avoid\n"
   "    that problem.\n"
   },

  { 20,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified ge_header.c to use -verb option to print out more stuff.\n"
   "* Modified to3d.c to set default dataset type to ANAT_EPI_TYPE (-epan)\n"
   "    if user is creating a 3D+time dataset.\n"
   "* Modified mri_read.c and to3d.c to correctly calculate and use\n"
   "    MRILIB_xoff and MRILIB_yoff for GEMS images.\n"
   "* Modified afni.h, afni.c, afni_func.c to implement AFNI_GLOBAL_SESSION\n"
   "    environment variable.\n"
   },

  { 21,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed up some rescan session problems with AFNI_GLOBAL_SESSION datasets.\n"
   "    (We don't want to let them be destroyed or descendant-ized.)\n"
   },

  { 28,DEC,2001 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified matrix.[ch] and RegAna.c to speed up some of Doug Ward's\n"
   "    matrix-vector calculations, to make 3dDeconvolve faster.  Makes\n"
   "    it about 22% faster on an Athlon/Linux box.\n"
   },

  { 8,JAN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifications to make AFNI work under CYGWIN (www.cygwin.com)\n"
   "   - Removed shm stuff from thd_iochan.c\n"
   "   - Compile plugins directly into afni.exe (no dlopen)\n"
   "   - Changing Makefile.cygwin and Makefile.INCLUDE to make\n"
   "     PLUGIN_init() function have different names for each\n"
   "     plugin compiled on CYGWIN\n"
   "   - At this time, 3dNLfim, 3dTSgen, plug_nlfit, and plug_wavelets\n"
   "     are not compiled for CYGWIN since they present difficulties.\n"
   },

  { 28,JAN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified cs_qhull.c to use centroid instead of normal for midpoint.\n"
   },

  { 29,JAN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c function AFNI_setup_viewing() to correct problem when\n"
   "   viewing the same functional bucket in two controllers - the bucket\n"
   "   widgets might not get set up correctly due to false memories.\n"
   },

  { 30,JAN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dAutoTcorrelate.c for PAB.\n"
   },

  { 31,JAN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to allow scrollbars on the Disp button dialog, if\n"
   "    AFNI_DISP_SCROLLBARS is yessish.\n"
   "* Modified imseq.[ch] and afni.[ch] to provide slice_proj projection\n"
   "    functionality.\n"
   },

  { 1,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Put most transform functions from afni.c and imseq.c into new file\n"
   "     afni_transforms.c.\n"
   "* Added 3dFDR.c from Doug Ward, and some changes to 3dDeconvolve.\n"
   },

  { 2,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added extreme_proj() to afni_transforms.c.\n"
   },

  { 4,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_getpathprogs.c to correctly skip searching the same\n"
   "   directory twice, rather than the cheap (strstr) way done before.\n"
   "   The old way caused problems when /usr/bin/ was ahead of /bin/,\n"
   "   for example.\n"
   "* Similar changes to NLfit_model.c, afni_plugin.c, and thd_get1D.c.\n"
   "* Removed the NULLIFY_ON_DESTROY() call for the whereami textwin in\n"
   "   afni.c, since the kill function for this window does the same\n"
   "   thing.  This may be the cause of the bug that Jill Weisberg\n"
   "   reported (that the whereami function stops working and then\n"
   "   crashes AFNI when the user presses the Quit button).\n"
   },

  { 5,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Put #undef USE_TRACING in FD2.c, since it uses STATUS() for something\n"
   "    else.\n"
   },

  { 6,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Adapted modified plug_histog.c from Vinai Roopchansingh (added the\n"
   "   'Output' option to write results to a file).\n"
   },

  { 19,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dAutoTcorrelate.c to add '-time' option.\n"
   },

  { 25,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Adapted modified 3dDeconvolve.c from Doug Ward.\n"
   "* Modified thd_getpathprogs.c to skip path elements that aren't directories\n"
   "   (some people have no clue, do they?).\n"
   },

  { 26,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to fix up the bucket widgets in AFNI_setup_viewing() again\n"
   "   (cf. 29,JAN,2002)\n"
   },

  { 27,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified various files to replace 'AGNI' with 'SUMA' (SUrface MApper).\n"
   "   For example, we now have afni_suma.[ch] instead of agni.[ch].\n"
   },

  { 28,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a small problem with thd_info.c (damn you, Jim Haxby).\n"
   "* Incorporated a few more 3dDeconvolve changes from Doug Ward.\n"
   "* First cut at putting niml.[ch] into AFNI, along with afni_niml.c.\n"
   },

  { 6,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Many changes over the last few weeks to include NIML support for\n"
   "   talking to Ziad Saad's SUMA program.\n"
   },

  { 7,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c (and afni_pplug_env.c) to make Button3 popup\n"
   "   text info window be a scrollable textwin rather than a menu popup\n"
   "   if the number of lines is too long; 'too long' is defined by new\n"
   "   environment variable AFNI_GRAPH_TEXTLIMIT.\n"
   "* Modified mrilib.h to add field 'was_swapped' to MRI_IMAGE struct.\n"
   "   Then modified mri_read.c to set this field if it auto-swaps the\n"
   "   image on input.  Then modified to3d.c to skip doing -2swap/-4swap\n"
   "   on such images.\n"
   },

  { 8,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c and afni.c to add 2 environment variables to\n"
   "   let user set the initial graph baseline parameters.\n"
   "* Modified afni_func.c, afni.c, and afni.h to allow the Define Function\n"
   "   value label to be recomputed/redisplayed even if only 1 image\n"
   "   window is open.\n"
   },

  { 10,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_cox.c, plot_x11.c, plot_ps2.c to allow circles.\n"
   "* Modified afni.c, afni_pplug_env.c to use environment variables to set\n"
   "   SUMA overlay box size and color.\n"
   "* Modified imseq.c so that 'q' keypress causes a window close (to make\n"
   "   this window like the various graph windows).\n"
   "* Modified afni_niml.c to receive SUMA_ijk triangles.\n"
   "* Modified afni.c to draw triangle/slice intersection lines.\n"
   "* Modified afni.c to allow specification of AFNI_FIM_IDEAL on startup.\n"
   "* Modified afni.c to set Dummy session directory to 1st argv directory.\n"
   },

  { 11,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to have SUMA overlay box and line color environment\n"
   "   strings set to 'none' mean to skip that overlay step.  (The lines\n"
   "   look better without the boxes.)\n"
   },

  { 12,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Implemented 'zoom' feature in imseq.c (with a little help from bbox.h).\n"
   "* Modified mri_dup.c to allow linear as well as heptic upsampling.\n"
   "   Use this for faster zooming in imseq.c.\n"
   },

  { 13,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to not reload the zoomed Pixmap if the image isn't new.\n"
   "   This speeds up scrolling the zoomed image quite a lot.\n"
   "* Modified coxplot/plot_x11.c to work properly if the first call to the\n"
   "   rendering function is into a Pixmap instead of a Window.\n"
   "* Fixed a bug in niml.c that created NIML_TRUSTHOST_%2d environment variable\n"
   "   names - should have been NIML_TRUSTHOST_%02d (D'oh).\n"
   "* Modified mri_dup.c to upsample byte-values images by 2/3/4 in special code.\n"
   "   This makes imseq.c zooming faster.\n"
   },

  { 14,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_dup.c to use 171/256, 85/256 as approximations to 2/3, 1/3 for\n"
   "   upsampling byte arrays by 3 - this avoids a division, and should be\n"
   "   faster.  Also a function to do upsample by 4 of 2D RGB images all at\n"
   "   once - this turns out to be worth the effort - speeds up nearly twice.\n"
   "* Modified thd_loaddblk.c to check if sub-bricks are all the same datum; if\n"
   "   not, always uses malloc() and also prints a warning to stderr.\n"
   "* Incorporated changes from Doug Ward\n"
   "   * The group statistical analysis programs (3dANOVA, 3dANOVA2, 3dANOVA3,\n"
   "     3dRegAna, 3dMannWhitney, 3dWilcoxon, 3dKruskalWallis, 3dFriedman), when\n"
   "     creating an AFNI 2-subbrick dataset or a bucket-type dataset, previously\n"
   "     used the following format for the output datum types\n"
   "        'intensity' sub-bricks -- same as input dataset\n"
   "        statistical sub-bricks -- scaled short integer\n"
   "     The above programs have been changed so that all output subbricks\n"
   "     will now have the scaled short integer format.\n"
   "   * Modified program 3dbucket, so that if there is more than one input\n"
   "   dataset, it will copy the command line history from the first input\n"
   "   to the output bucket dataset.\n"
   },

  { 15,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c ISQ_show_zoom() function to discard old zoomed image if\n"
   "   the pixmap had to be re-created.\n"
   "* Modified mri_dup.c to do 3x RGB upsample in special function, too.\n"
   "* Modified imseq.c to do panning with Button1 when the new 'pan' button\n"
   "   is on - the 'old' arrowpad buttons have been excised.\n"
   },

  { 16,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to not start NIML until the startup timeout is called.\n"
   "   Otherwise, very early data might try to popup a window before that\n"
   "   is possible.  Also, made NIML be turned on by default.\n"
   "* Modified afni_niml.c to have the popup messages include the I/O and\n"
   "   processing time for large data elements.\n"
   "* Modified imseq.c (and afni_pplug_env.c) to keep panning mode on if\n"
   "   AFNI_KEEP_PANNING is yessish.\n"
   "* Modified xim.c to speed up rgb_to_XImage(), by doing TrueColor and\n"
   "   PseudoColor in separate functions.\n"
   },

  { 17,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_niml.c to disable NIML listening if all potential sockets\n"
   "   are busy.  This is to prevent an endless series of error messages\n"
   "   when 2 AFNIs are running at once.\n"
   "* Modified afni.c to add option '-noniml'.\n"
   },

  { 18,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to make it necessary to use -niml or AFNI_NIML_START to\n"
   "   have NIML listening activated from the beginning.\n"
   },

  { 22,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_dup.c to correctly shift RGB images by 1/2 pixel.\n"
   "* Modified afni.c to correctly draw coordinates for lines shifted by 1/2 pixel.\n"
   "* Modified afni.c and afni_pplug_env.c to allow user to draw crosshairs with\n"
   "   lines instead of overlay pixels.\n"
   },

  { 23,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to zoom and draw overlays on Save One images.\n"
   },

  { 25,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to crop saved zoomed images if ordered by environment\n"
   "   variable AFNI_CROP_ZOOMSAVE.\n"
   },

  { 26,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to save zoomed images in filtered Save many code as well\n"
   "   (including animations).  Also set 'Save to .ppm' as the default save mode,\n"
   "   if possible.\n"
   },

  { 27,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dinfo.c, thd_info.c to have a -short option.\n"
   "* Modified imseq.c to have isqDR_options save the output filter, rather than\n"
   "   reset it. (Otherwise, Left-is-Left loses the new default 'Save to .ppm'.)\n"
   "* Modified parser.f and 3dcalc.c to include a mad() function.\n"
   },

  { 28,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added function mri_move_guts() to mri_free.c, in preparation for multi-\n"
   "   plotting in afni_graph.c.\n"
   },

  { 29,MAR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c to accept multi-plot timeseries.\n"
   "* Added plug_nth_dataset.c to generate multi-plot timeseries.\n"
   "* Fixed bug in to3d.c that disabled -4swap option (from 07,MAR,2002).\n"
   },

  { 5,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added (x,y,z) coord printout to 'Where Am I' window.\n"
   "* Modified imseq.[ch] to remove ALLOW_ZOOM conditional, and allow zoom/pan\n"
   "   from 'z', 'Z', and 'p' keystrokes.\n"
   },

  { 9,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Some minor changes to machdep.[ch] to make AFNI work on Mac OS X 10.1.3.\n"
   },

  { 10,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Remove malloc.h include from mpeg_encode files for OS X compatibility.\n"
   "* Modify thd_automask.c to only keep largest connected component.\n"
   "* New program 3dAutomask.\n"
   "* Modified a few programs to use -automask as a synonym for -autoclip\n"
   "   3dAutoTcorrelate.c, 3dTcorrelate.c, 3dToutcount.c, and 3dTqual.c.\n"
   },

  { 11,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dAFNItoMINC.c, and new function thd_mincwrite.c.\n"
   "* Fixed bug in thd_mincread.c: it formerly scaled float inputs, which\n"
   "   apparently is wrong.\n"
   },

  { 15,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to afni.c, afni_func.c, afni_slice.c, afni_warp.c, etc., to allow\n"
   "   MRI_rgb-valued datasets.  These can be created in to3d from ppm input\n"
   "   files.\n"
   },

  { 16,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_mincread.c to correctly use lower end of valid_range[].\n"
   "* Modified thd_mincwrite.c to use '-range -scan_range' flags with\n"
   "   rawtominc program.\n"
   "* Modified 3dvolreg.c and 3drotate.c to make -clipit the default.\n"
   "* New program 3dThreetoRGB.c to create RGB datasets from 3 bricks.\n"
   "* Modified mri_read.c to use new '3Dr:' input format for RGB files.\n"
   "* Modified 3dAutomask.c to add history note.\n"
   "* Modified afni_plugin.h and NLfit_model.h to read dlfcn.h from\n"
   "   dlcompat/ subdirectory on Darwin systems.\n"
   "* Modified afni.c to allow environment variables to set +tlrc box size.\n"
   },

  { 17,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_sumafunc.c to allow for MRI_rgb fim overlays.\n"
   },

  { 18,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_automask.c to erode/dilate the result, so as to clip off\n"
   "   tenuously connected blobs.\n"
   "* Modified edt_clust.c to make MCW_erode_clusters() return void, not\n"
   "   void *, since it doesn't actually return anything.\n"
   },

  { 19,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_automask.c to have fill in functions.\n"
   "* Modified 3dAutomask.c to have -fillin option.\n"
   "* Modified cox_render.c to remove memset() of new image to 0, since\n"
   "   mri_new() does this since 01,NOV,2001.\n"
   },

  { 22,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified debugtrace.h to include 'last_status' variable: keeps of copy of the\n"
   "   last STATUS() string, and prints it when the program crashes.\n"
   "* Modified thd_dsetdblk.c to deal with NULL dataset at very end - the SUMA stuff\n"
   "   didn't check for that, which caused a crash if the dataset couldn't be\n"
   "   constructed.\n"
   },

  { 26,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* At last seem to have figured out how to make the orientations change\n"
   "   w.r.t. spatial axes - added these changes to plug_crender.c.\n"
   },

  { 28,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the orientation changes to plug_render.c as well.\n"
   },

  { 29,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New functions in thd_mnicoords.c to translate TT atlas coords to/from\n"
   "   MNI template coords.\n"
   "* Used above in thd_ttatlas_query.c.\n"
   "* Samia Saad was born today!\n"
   },

  { 30,APR,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dclust.c to add -mni option.\n"
   "* Modified 3dclust.c, edt_clust.c, edt_clustarr.c, edt_onedset.c to allow\n"
   "   cluster rmm=0 to imply 6 NN clustering, vmul=0 to mean no volume editing,\n"
   "   and vmul<0 to mean min volume = fabs(vmul) voxels.\n"
   "* Modified plug_drawdset.c to add 'Flood->Val/Zer' option.  Also to turn\n"
   "   'See Function' on if the edited dataset is functional type.\n"
   "* Added edt_clust2.c (NIH_find_clusters) to implement ISOVALUE_MODE and\n"
   "   ISOMERGE_MODE.\n"
   "* Incorporated Ziad Saad's Ifile program into AFNI distribution.\n"
   },

  { 1,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Jump to (MNI)' button to image popup menu (afni_widg.c and afni.c).\n"
   },

  { 7,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   " Changes from Doug Ward\n"
   "* The -one_col option has been added to program RSFgen.  With this option,\n"
   "   the input stimulus functions are written as a single column of decimal integers\n"
   "   (rather than multiple columns of binary numbers).\n"
   "* The -stim_base option was added to Program 3dDeconvolve.  This modification\n"
   "   allows the user to specify which input stimulus functions are to be included\n"
   "   as part of the baseline model.  By default, the input stimulus functions are\n"
   "   not included in the baseline model.  This option will effect the output values\n"
   "   for the Full Model R^2 and Full Model F-stat, since these statistics indicate\n"
   "   the significance of the full regression model relative to the baseline model.\n"
   "   This option might be useful, for example, when the estimated motion parameters\n"
   "   are included as input stimulus functions.  In most cases, the user would want\n"
   "   the motion parameters to be part of the baseline model.  By indicating that\n"
   "   the motion parameters are part of the baseline model, they will not contribute\n"
   "   to the full regression model sum of squares.\n"
   "* The Deconvolution plugin was also modified to incorporate the above change.\n"
   "   On the far right of each stimulus function input line of the Deconvolution\n"
   "   plugin interface, there is a new option chooser labeled 'Base', which allows\n"
   "   the user to specify that this stimulus function is to be considered as part of\n"
   "   the baseline model.\n"
   "* The Deconvolution plugin was modified to allow a better graphical\n"
   "   representation of the estimated impulse response function when the user selects\n"
   "   option DC_IRF under Tran 1D of the graph options menu.  When using the DC_IRF\n"
   "   function, note that the Double Plot option should be set to 'Off'.\n"
   "* The 3dDeconvolve documentation was updated to reflect the above changes.\n"
   "   In particular, see Examples 1.4.3.2 and 2.3.2 of the Deconvolution manual in\n"
   "   file 3dDeconvolve.ps.\n"
   },

  { 11,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to put global session datasets into sessions as they\n"
   "  are read, rather than later - this allows the anats in the global\n"
   "  session to allow a session to be used, even if there are no funcs\n"
   "  in the directory.\n"
   },

  { 14,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dZeropad.c to have -master option.\n"
   "* Modified thd_zeropad.c to return full copy of dataset if input add/cut\n"
   "  values are all zero (used to return NULL).\n"
   },

  { 17,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to allow image fraction change with 'i', 'I' keys.\n"
   },

  { 28,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3drefit.c to add -clear_bstat option.\n"
   "* Modified 3dAutomask.c to remove -fillin option.\n"
   "* Modifed thd_automask.c to make fillin = brick size/60, and to make\n"
   "   final mask = complement of largest component of zeros.\n"
   },

  { 31,MAY,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Adapted shm stuff from thd_iochan.c to niml.c.\n"
   },

  { 4,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dAutomask.c to print out how many planes are cut off in the\n"
   "   the mask.\n"
   "* Modified thd_automask.c to be faster.\n"
   },

  { 6,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New programs 3dAutobox.c and 3dLRflip.c.\n"
   "* New function mri_cut_3D() in mri_cut.c.\n"
   "* Modified mri_3dalign.c to do trimming to save memory.\n"
   "* Modified 3dvolreg.c to add -wtrim and -wtinp options.\n"
   },

  { 7,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Refined default threshold for termination of 3dvolreg.c.\n"
   },

  { 10,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.h and afni.c to incorporate RGBCYC cyclic color map from\n"
   "   Ziad Saad.\n"
   },

  { 12,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to add image cropping facility (Shift+Button2).\n"
   "* Added function RWC_drag_rectangle() to xutil.c.\n"
   "* Put MRI_COPY_AUX() into mri_cut.c (oops).\n"
   },

  { 14,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_x11.c to remove offset of 0.5 pixels from\n"
   "   line drawing code.\n"
   "* Modified imseq.c to fix scaling of memplot overlays when cropping.\n"
   },

  { 17,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'crop' pushbutton to imseq.[ch].\n"
   "* Modified default font for imseq to 7x13 in afni.c, and\n"
   "   modified imseq.c to shrink button margins, to squish widgets together.\n"
   },

  { 19,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed clipping error in plot_cox.c (dangling else problem, d'oh).\n"
   },

  { 20,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_ascii() to catenate lines that end in '\\', and do\n"
   "   some other minor stuff ('//' as a comment line, etc.).\n"
   "* Modified thd_loaddblk.c (etc.) to support STORAGE_BY_VOLUMES.\n"
   },

  { 21,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_info.c to print out disk storage mode of dataset.\n"
   "* Started work on 3dANALYZEtoAFNI.c.\n"
   },

  { 24,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_analyze75() in mri_read.c to store funused1 scale\n"
   "   factor into dv MRI_IMAGE header field, for use in 3dANALYZEtoAFNI.c.\n"
   "* Modified thd_writedset.c to allow re-writing of a VOLUMES dataset\n"
   "   .HEAD file.\n"
   "* Modified plug_realtime.c to allow single slice dataset input (nzz=1).\n"
   },

  { 25,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified rtfeedme.c a little for debugging purposes.\n"
   "* Modified thd_iochan.c to print better messages with PERROR().\n"
   "* Modified plug_realtime.c to popup message when disk I/O is finished\n"
   "   after an acquisition ends.\n"
   },

  { 27,JUN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml.c to add debugging output option (NIML_DEBUG).\n"
   "* Fixed scan_for_angles() in niml.c to disable timeouts in case (b).\n"
   },

  { 5,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmerge.c to prevent use of -keepthr on fim and fbuc datasets.\n"
   },

  { 14,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed the dicomm<->surefit functions from Vecwarp.c since they are now\n"
   "   in libmri.a via afni_suma.c, and the Sun compiler doesn't like this.\n"
   },

  { 15,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mri_dicom_hdr.c and dicom_hdr.c - function and program to read/print\n"
   "   DICOM header info.  Adapted from dcm_dump_file.c from RSNA, per the\n"
   "   suggestion of Jack Lancaster.\n"
   },

  { 19,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New function mri_read_dicom() to read images from DICOM files.\n"
   },

  { 23,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to get slice orientation and offsets.\n"
   },

  { 24,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified UNIQ_idcode() in niml.c to get 4 extra bytes from /dev/urandom,\n"
   "   if it is readable.\n"
   },

  { 29,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_read_images() in afni.c to use im->dx,dy,dz in '-im' usage\n"
   "   of program, if images read have voxel spacing (e.g., Analyze).\n"
   "* Modified imseq.c to NOT turn off widgets if only 1 slice to display.\n"
   },

  { 30,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c to accept DRIVE_AFNI commands in the image prolog.\n"
   "* Modified afni.c to allow plugouts during realtime.\n"
   "* Modified rtfeedme.c to send DRIVE_AFNI commands with the -drive option.\n"
   },

  { 31,JUL,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New function EDIT_wod_copy() to create a warp-on-demand copy, like 3ddup.\n"
   "* Use this in afni.c when a directory has only functions, no anats.\n"
   "* Modified afni.c to allow -TRACE to work in realtime mode.\n"
   "* Modified afni.c, afni_widg.c to make plugouts not crash during realtime\n"
   "   mode (we hope).\n"
   },

  { 2,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c to deal with multiple input channels.\n"
   "* Modified rtfeedme.c to send multiple dataset channels.\n"
   },

  { 5,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed DEBUGTHISFILE macro from all places that used it.\n"
   "* Modified plug_realtime.c to deal with case when more channels than\n"
   "   controllers are in use.\n"
   },

  { 6,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_driver.c to allow 'iconify' option on OPEN_WINDOW commands.\n"
   "* Modified afni_driver.c to return controller index only if the input\n"
   "   string is only 1 character long or the 2nd character is a '.'.\n"
   "* Modified afni_func.c and bbox.c to deal with potential strlist[] overflow\n"
   "   problems.\n"
   },

  { 7,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed cl1.c and incorporated into libmri.a.\n"
   "* Added plugin plug_L1fit.c to provide a L1 analog to plug_lsqfit.c.\n"
   "* Modified 3dToutcount.c to use cl1_solve to remove trends before\n"
   "   outlier-ing.\n"
   },

  { 8,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added color save/load to afni_ttren.c.\n"
   },

  { 13,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to destroy dialog widget before imseq top widget.\n"
   "* Fixed array overflow by 1 bug in mri_percents.c.\n"
   "* Modified mri_read.c to check for .hdr files before DICOM.\n"
   "* Modified 3dToutcount.c to save result as a FIM, and to save history.\n"
   },

  { 14,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified various things (afni.h, afni.c, afni_niml.c, afni_suma.c)\n"
   "   to allow for multiple surfaces per dataset.\n"
   "* Modified niml.h to disable shm stuff for Cygwin.\n"
   },

  { 16,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] and afni.c to suppress multiple image redraws\n"
   "   when an image window is first opened.\n"
   "* Modified plug_nudge.c to extend range of angle and shift choosers.\n"
   "* Modified xutil.h WAIT_for_window() macro to wait a little longer.\n"
   "   This is in an attempt to get rid of random problems with graph\n"
   "   windows opening with bad Pixmap contents.\n"
   },

  { 19,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_sumafunc.c (afni.h, etc.) to have a control panel for\n"
   "   surface stuff.  At this time, lets user set colors.\n"
   },

  { 20,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifications to surface controls: hints, help, comments.\n"
   "* Added UUID functions to niml.[ch].\n"
   },

  { 21,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml.c to add NI_mktemp(), and change use of /dev/urandom.\n"
   "* Modified machdep.c to change use of mallopt() for Linux - seems to\n"
   "   make big malloc()'s work better (?).\n"
   "* Modified thd_fetchdset.c to use niml.c functions, instead of the\n"
   "   older thd_http.c functions.\n"
   },

  { 23,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_loaddblk.c to print (stderr) a message when loading a\n"
   "   large dataset.\n"
   "* Modified niml.[ch] to implement NI_reopen_stream() and make a first\n"
   "   cut at NI_do().\n"
   },

  { 26,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added Htable (string/pointer pair) hash table functions to niml.[ch].\n"
   "* Added mri_read3D_analyze75() to mri_read.c, to read an ANALYZE file\n"
   "   as an array of 3D images, rather than 2D images.\n"
   },

  { 27,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read*_analyze() functions to always floatize ANALYZE\n"
   "   data if the SPM funused1 scale factor is present.\n"
   "* Added ANALYZE (thd_analyzeread.c) file input to datasets.\n"
   },

  { 28,AUG,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_initsess.c and thd_analyzeread.c to support FSL/FEAT\n"
   "   input .map files.\n"
   },

  { 1,SEP,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Program 3dDespike.c, to patch a problem with the 3T-1 scanner.\n"
   },

  { 3,SEP,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dDespike.c to allow float datasets, print nicer messages, etc.\n"
   "* Fix my_tanh() bug in 3dDespike.c, which was returning NaN for very\n"
   "   large inputs.\n"
   },

  { 4,SEP,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More cosmetic changes to 3dDespike.c.\n"
   },

  { 9,SEP,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* ISHEADTYPE macro in 3ddata.h used '=' instead of '=='; this was bad.\n"
   "* 'Q' and 'q' quits in afni_widg.c and afni_func.c (hidden_EV).\n"
   },

  { 10,SEP,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to print out at most 9 warning messages of\n"
   "   each type.\n"
   "* Modified to3d.c to open X11 immediately when -nosave is used.  Also added\n"
   "   a bunch of ENTRY/RETURNs to ferret out a bug (it's still hidden).\n"
   "* Oops.  Forgot to fclose() the fopen()-ed file in mri_read_dicom.c.  This\n"
   "   was causing the problems in to3d mentioned above.\n"
   "* New program dicom_to_raw.c.\n"
   },

  { 30,SEP,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dAFNItoANALYZE.c (and mri_write_analyze.c) to allow output of\n"
   "   AFNI datasets into a 4D ANALYZE format file.\n"
   },

  { 1,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c to allow input of notes via a NOTE command.\n"
   "   Also modified rtfeedme.c with -note option to test this out.\n"
   },

  { 3,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to allow use of Shift+Button1 for drawing as well as\n"
   "   Button2.  Changes also to afni_graph.c and plug_drawdset.c (the\n"
   "   latter just to change the help text).\n"
   "* Modified 3dTcat.c to use last '+' as marker for '+orig' (etc.)\n"
   "   rather than 1st.\n"
   },

  { 4,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Took modified plug_deconvolve.c from Doug Ward, to fix bug when baseline\n"
   "   is disabled.\n"
   "* Modified thd_analyzeread.c to add AFNI_ANALYZE_AUTOCENTER option.\n"
   "* Modified 3drefit.c to add -xorigin_raw (etc.) options.\n"
   "* Modified thd_intlist.c to skip blanks in the [] sub-brick selector string.\n"
   },

  { 7,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c to add '2D Nbhd' and '3D Nbhd' drawing modes.\n"
   "* Also rearranged the Copy and Choose Dataset buttons.\n"
   },

  { 8,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c (and imseq.[ch], afni_receive.c, afni.h) to make\n"
   "   button2 drawing overlay have thicker lines, closer to what will actually\n"
   "   be filled.  Also added 1 larger '3D Nbhd' stencil.\n"
   },

  { 10,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_analyzeread.c to correct signs of origin when orientation has\n"
   "   some negative axes.\n"
   },

  { 16,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c to add '2D Circle' and '3D Sphere' drawing modes.\n"
   },

  { 17,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c sphere/circle insertion not to test for duplicate\n"
   "   insertions from the 1st input point - this speeds things up for large R.\n"
   },

  { 25,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_drawdset.c to use sorting to prevent vast numbers of duplicates\n"
   "   when inserting large R circles/spheres.\n"
   },

  { 28,OCT,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to mri_dicom_hdr.c and mri_read_dicom.c to deal with Siemens' stupid\n"
   "   mosaic format.\n"
   },

  { 1,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More changes for mosaic input.\n"
   },

  { 4,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added MRI_rgb type to thd_zeropad.c and to BRICK_*_MASK macros in afni_plugin.h.\n"
   "* Took changes from Rasmus Birn to add a '-gamd' delay option to waver.c.\n"
   },

  { 5,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added program rotcom.c, to print out matrix+vector from '-rotate ... -ashift ...'\n"
   "   options that would be input to 3drotate.\n"
   "* Fixed mri_read.c, mri_read_dicom.c, mri_dicom_hdr.c to stop annoying printout of\n"
   "   messages about bad DICOM files when trying to read GE I.* files.\n"
   },

  { 12,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changed MAX_CONTROLLERS in afni.h for Mike Beauchamp.\n"
   },

  { 13,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_splash.[ch] to allow color top overlays.  Incorporated SSCC group\n"
   "   picture.\n"
   },

  { 18,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml.[ch] to make the NI_malloc() package use tracking functions,\n"
   "   which I stole from mcw_malloc.c - of course, I wrote that, too, so 'stole'\n"
   "   may be too strong a verb.\n"
   },

  { 21,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added some extra programs (gifsicle, mpeg_encode, cjpeg, etc.) to the PROGRAMS\n"
   "   macro in Makefile.INCLUDE.\n"
   },

  { 22,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mri_read_stuff.c to filter input from JPEGs, TIFFs, BMPs, etc., into\n"
   "   mri_read.c.\n"
   "* Added afni_version.c to check AFNI version at startup.\n"
   "* Modified edt_dsetitems.c to strip '+orig' etc. from tail of new prefix.\n"
   },

  { 23,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_version.c to only do a check once every 12 hours.\n"
   },

  { 25,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_friends.c to add date-based trivia (also, afni.[ch]).\n"
   },

  { 27,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to allow for stupid GE case where slice spacing is\n"
   "  incorrectly set to slice gap instead.\n"
   },

  { 29,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_stuff.c to allow for case when initial 4096 byte buffer\n"
   "   contains all the image data (i.e., for tiny images).\n"
   "* Modified coxplot/plot_ps.c to allow output to stdout.\n"
   "* Modified 1dplot.c to allout PostScript output to stdout.\n"
   },

  { 30,NOV,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mcw_graf.[ch] to draw a coordinate label during drag of graf\n"
   "   handle with Button 3.\n"
   "* Modified mri_read_dicom.c to deal with GE's incorrect use of Slice Spacing\n"
   "   as gap, rather than center-to-center distance.  Ugh.\n"
   },

  { 2,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_crender.c to use mcw_graf.c stuff of 30,NOV,2002.\n"
   "* Modified mri_read_dicom.c to deal with Siemens Mosaic differently, to\n"
   "   accomodate variations from NYU's Allegra scanner (vs. NIDA's).\n"
   "* Modified to3d.c to show NX x NY along with Datum in GUI.\n"
   },

  { 3,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml.c to use setsockopt() to change socket buffer sizes only\n"
   "   if getsockopt() says they are too small.\n"
   },

  { 4,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_ctfread.c to read CTF MRI files as datasets.\n"
   "* Modified thd_initsess.s, thd_opendset.c, thd_loaddblk.c, 3ddata.h to\n"
   "   use the CTF functions.\n"
   },

  { 5,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added CTF SAM file input to thd_ctfread.c.\n"
   "* Modified 3dIntracranial.c to convert input dataset to shorts if it is\n"
   "   stored as bytes.  The output will still be shorts.\n"
   },

  { 7,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to change image number on '<' or '>' keys (like\n"
   "   the graph windows).\n"
   },

  { 9,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to save temporary files for animated GIF or MPEG\n"
   "   output with a random suffix, to avoid collisions if 2 copies\n"
   "   of AFNI (or aiv) are running.\n"
   "* Modified niml.[ch] to allow definiton of 'NI_rowtypes' to make it easier\n"
   "   to deal with structs (with all fixed length elements, alas).\n"
   "* Modified nids.[ch] to deal with vectors of arbitrary NI_rowtype.\n"
   },

  { 11,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c to allow termination of a dataset without\n"
   "   closing the data IOCHAN.  Also added new XYZFIRST metadata command,\n"
   "   to allow setting of all 3 axis offsets.\n"
   "* Modified rtfeedme.c to test the above features.\n"
   },

  { 12,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_niml.c and afni_sumafunc.c to create functional\n"
   "   colormaps for all surfaces on the anat dataset, not just the\n"
   "   first surface.  Also fixed it so that fim=0 is not overlaid.\n"
   "* Modified thd_iochan.c to use IOCHAN_DELAY_RMID environment variable\n"
   "   to indicate that deletion of shm segments should only occur when\n"
   "   no one is attached to them.  This is to get around a problem on\n"
   "   some Unices.\n"
   "* Modified Makefile.INCLUDE rule for cjpeg to remove old Makefile and\n"
   "   jconfig.h, since these are re-made by the configure script.\n"
   "* Modified niml.c to shmdt() before shmctl(IPC_RMID) instead of after.\n"
   "* Modified afni.c to skip version check if realtime is on.  Also modified\n"
   "   afni_version.c to add an atexit() handler to delete the child-parent\n"
   "   shared memory IOCHAN when the program exit()'s.\n"
   "* Modified rtfeedme.c to add a signal handler to close the data IOCHAN\n"
   "   if the program crashes.\n"
   },

  { 16,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved niml.[ch] into niml/ subdirectory, and modified Makefile.INCLUDE\n"
   "   accordingly.\n"
   },

  { 18,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_realtime.c to add ZGAP and XYZOFF features from Larry Frank.\n"
   "* Fixed bug in niml/niml_util.c decode_string_list() function where\n"
   "   the sar->str array was allocated with sizeof(char)*num rather than\n"
   "   sizeof(char *)*num.  Not good in the long run.\n"
   "* Modified niml/niml_rowtype.c to allow rowtypes to have 1D variable dimension\n"
   "   arrays.\n"
   },

  { 19,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added THD_mkdir(), THD_is_ondisk(), THD_cwd() to thd_filestuff.c.\n"
   "* Modified afni_driver.c to add commands SYSTEM and CHDIR, which call\n"
   "   system() and chdir() [pretty clever names, huh?].  Also modified\n"
   "   afni_driver.c to trim trailing blanks from the input command\n"
   "   before passing it to the handler functions.\n"
   },

  { 20,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml/niml_do.c to add verb 'close_this' to close a stream.\n"
   "   This is to let a stream be closed from the other end.\n"
   "* Modified niml/niml_stream.c to send a 'close_this' message when\n"
   "   a tcp: or shm: stream is closed.\n"
   "* Modified niml/niml_stream.c to mark NI_stream's for 'death' in\n"
   "   NI_stream_close_keep(), and then avoid using such streams in\n"
   "   other functions.  This is to let a stream be closed without\n"
   "   freeing its struct.\n"
   },

  { 23,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to correct error in z-axis orientation from\n"
   "   multiple 1-slice datasets - code had been copied from mri_read.c, but\n"
   "   that was for GE LPI coords, and DICOM is RAI.\n"
   "* Modified mri_read_dicom.c to use Rescale and Window tags, if turned on\n"
   "   by the relevant environment variables.\n"
   "* Modified aiv.c to use globbing on input filenames.\n"
   },

  { 24,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to save Siemens extra info string even if\n"
   "   file isn't a mosaic.\n"
   },

  { 27,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dttest.c to save DOF dataset if -unpooled is used.\n"
   },

  { 28,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_rescan_session() in afni_func.c to NOT clobber existing\n"
   "   dataset pointers when adding datasets to a session.\n"
   "* Removed all instances of OMIT_DATASET_IDCODES.\n"
   },

  { 29,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor change to afni_version.c to print out 'Latest News' web page\n"
   "   when version comparison fails.\n"
   },

  { 30,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor change to AFNI_rescan_session() users to print out number of\n"
   "   new datasets rows.\n"
   },

  { 31,DEC,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml/niml_rowtype.c to deal with String type and debugged\n"
   "   stuff with output of var dim arrays.\n"
   },

  { 2,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added a error message to mcw_malloc.c to note when an allocation fails.\n"
   },

  { 10,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_blur.c to clip output to input range.\n"
   },

  { 13,JAN,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified Makefile.solaris28_gcc on hador system - plugins would no longer\n"
   "   load.  Use of GNU ld instead of UCB ld fixes this, but for unknown\n"
   "   reasons.  Evil spirits?\n"
   },

  { 15,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to let user set SUMA line thickness via environment\n"
   "   variable.  Also changed afni_pplug_env.c to match.\n"
   "* Modified afni_func.c to purge all datasets in a session after rescan.\n"
   },

  { 16,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to let user setenv AFNI_AGIF_DELAY to control speed\n"
   "   of animated GIF output.\n"
   "* Modified afni_driver.c to allow remote changing of thresholds, the\n"
   "   addition of overlay colors, and the setting of pbar pane number.\n"
   },

  { 21,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More additions to afni_driver.c.\n"
   "* Changes to afni.[ch] to allow startup script to drive AFNI setup.\n"
   },

  { 22,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Run Script' button to Datamode->Misc menu.\n"
   "* Made 'Save Layout' button save setup script to .afni.startup_script\n"
   "   if no filename is entered.\n"
   },

  { 23,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made AFNI_VALUE_LABEL default be YES instead of NO.\n"
   "* Added AFNI_DEFAULT_OPACITY environment variable, for image overlay opacity.\n"
   "* Added AFNI_DEFAULT_IMSAVE environment variable, for .jpg, .tif, etc.\n"
   "* Fixed bug in afni_driver.c SETENV function - space used for putenv() must\n"
   "   be permanent memory, not temporary!\n"
   },

  { 24,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added special function key stuff to imseq.c (arrows, PgUp/PgDn).\n"
   },

  { 27,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added messages to afni_version.c when version checking is disabled, fails,\n"
   "   or when the current version is different than the last version.\n"
   },

  { 28,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_ctfread.c to correct nx,ny,nz calculation for SAM images.\n"
   "* Modified afni.c to do the time lock properly when the anat has no\n"
   "   time axis but the func does.\n"
   "* Modified 3dcopy.c to work with copying non-AFNI datasets.\n"
   },

  { 29,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3drefit.t to add -Torg option (for MEG guys).\n"
   },

  { 30,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified pbar.c, afni_func.c, etc., to add a 'big' mode to the pbar, with\n"
   "   128 colors.  More work is needed here for SUMA, rendering, scripting,\n"
   "   loading colormaps, etc.\n"
   },

  { 31,JAN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to de-sensitize threshold slider when not needed, rather\n"
   "   than hide it.  This is to get around a problem with the size of the pbar\n"
   "   being adjusted slightly incorrectly, for reasonse I don't understand.\n"
   "* Modified pbar.c to give choice of colormaps for 'big' mode.  Programmed\n"
   "   a startup set of 4 colormaps.\n"
   },

  { 2,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_sumafunc.c to use 'big' mode colorscales.\n"
   "* Modified afni.c, pbar.c (etc.) to read in user-defined colorscales\n"
   "   from files.\n"
   },

  { 3,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to afni_driver.c to support colorscales.\n"
   "* Saving .afni.startup_script now also saves the Autorange/funcrange setting.\n"
   },

  { 4,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved user-defined colorscale setup to pbar.c from afni.c.\n"
   "* In afni_widg.c, use AFNI_COLORSCALE_DEFAULT to setup initial colorscale.\n"
   },

  { 5,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'OPEN_PANEL' command to afni_driver.c, and to afni_splash.c.\n"
   },

  { 6,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* This time, modified 3ddata.h, afni.c, afni_func.c to ALWAYS keep threshold\n"
   "   slider active, and when there is no threshold, use the function for the\n"
   "   threshold.  (However, RGB overlays are not thresholded, so this is the\n"
   "   only case in which the threshold slider is desensitized.  Also, the first\n"
   "   time the user switches to a fim dataset, the threshold slider is set to 0.)\n"
   "* Modified to3d.c to deal with double input images (to be converted to floats).\n"
   "   Also changed mri_read.c, mcw_glob.c, mri_swapbytes.c, 3ddata.h to add\n"
   "   a '3Dd:' input format for reading doubles from arbitrary files.\n"
   "* Added some new default colorscales to pbar.c.\n"
   },

  { 7,FEB,2002 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to allow AFNI_RESCAN_METHOD environment variable to\n"
   "   choose old rescan method (cf. 28,DEC,2002).\n"
   },

  { 10,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.[ch] and afni.c to allow initializing graph matrix size\n"
   "   to value other than 3, through AFNI_graph_matrix environment variable.\n"
   "* Modified 3dcalc.c to allow RGB dataset input.\n"
   },

  { 11,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dfractionize.c to set default clip value to a very tiny positive\n"
   "   value, rather than 0.\n"
   "* Modified pbar.[ch] to use a popup menu to deal with Button-3 events, rather\n"
   "   than directly do stuff.  Included a value+color label on this menu.\n"
   },

  { 12,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml/ to read/write full type names rather than just abbreviations\n"
   "   to the 'ni_type' attribute of data elements.\n"
   "* Modified niml/niml_do.c to add a 'typedef' verb, and to let the user program\n"
   "   define its own verb/callback pairs.\n"
   "* Modified afni_niml.c to define a 'ni_do' verb 'DRIVE_AFNI', to let external\n"
   "   program (hint: SUMA) access this stuff.\n"
   },

  { 18,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to add a hint to the colorscale.\n"
   "* Modified NIML stuff to use rowtypes more properly (niml_element.c, etc.).\n"
   "* Modified various Makefile.* to define SHOWOFF macro (once again).\n"
   "* Modified afni_version.c to print out appropriate wget command for update.\n"
   "* Modified afni.c to printout precompiled version, if present.\n"
   },

  { 19,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a couple little points in niml_rowtype.c.\n"
   "* Modified afni_sumafunc.c (afni.h, etc.) to change 'Control Surface'\n"
   "   label for each surface into a toggle button, to make it easier to\n"
   "   turn surface on and off quickly.\n"
   },

  { 20,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to imseq.c, afni.[ch], afni_receive.c, and plug_drawdset.c to make\n"
   "   the keypad 'Delete' key(s) operate like the Undo button in the drawing\n"
   "   plugin.\n"
   "* Modified afni_receive.c to have a string name debug output for each\n"
   "   receive callback (and all the functions that call this).  Also fixed a\n"
   "   bug that would have functions registered for different receive cases\n"
   "   get inappropriate calls (forgot to enclose the call in {..} in an if).\n"
   "* Modified afni_suma*.c to send closest surface node ID to SUMA when viewpoint\n"
   "   change callback is invoked.\n"
   },

  { 21,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to use '-bpp 24' in ppmtobmp output to BMP files, avoiding\n"
   "   quantization problems.\n"
   "* Modified afni.c to add a '#NodeID' string to the Button-3 image viewer popup,\n"
   "   when a surface is present.\n"
   },

  { 23,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.[ch] and afni_sumafunc.c to create boxsize and linewidth\n"
   "   controls on the 'Control Surface' popup.\n"
   "* Modified coxplot/plot_x11.c to flip line segments if that will make\n"
   "   them join. Also, initialize thickness of plots to 0, to allow for\n"
   "   special case (circle, box, ...) that is first item plotted.\n"
   },

  { 24,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to draw a * in the box of the closest surface node.\n"
   "* Modified 3dhistog.c to print '#' at start of header lines, so that\n"
   "   result can be read by mri_read_1D(), etc.\n"
   "* Incorporated changes from KRH to fix mri_read_dicom.c for Siemens\n"
   "   mosaics with incomplete slice coordinates.\n"
   "* Modified afni_graph.c to fix problem with double plot introduced\n"
   "   when multiple timeseries graphing was introduced - forgot to\n"
   "   reset tsar pointer to original data when graphing double plot\n"
   "   in plus/minus mode.\n"
   },

  { 26,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.c and afni_sumafunc.c to allow drawing of triangle\n"
   "   intersections at edges of slice plane, as well as at center.\n"
   "* Modified 'view_setter' code to work more intuitively when only one\n"
   "   image viewer is open, etc.\n"
   },

  { 27,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c to get multiple timeseries at once, to reduce\n"
   "   cache thrashing.\n"
   "* Modified thd_notes.c to add function to append one dataset's history\n"
   "   to another's.  Used this in 3dcalc.c as a starter.\n"
   },

  { 28,FEB,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Per Lukas Pezawas of CBDB, modified 1dgrayplot.c to have -ps option,\n"
   "   like 1dplot.c.  While doing so, also fixed a bug in coxplot/plot_ps2.c\n"
   "   where the rectangle case didn't have a 'break', so fell through and\n"
   "   drew a circle as well.\n"
   "* Modified mritopgm.c to have a clipping option.\n"
   },

  { 3,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Created matrix_f.[ch] as float alternatives to matrix.[ch].  Then used this\n"
   "   in 3dDeconvolve.c and RegAna.c to create a program 3dDeconvolve_f compiled\n"
   "   from 3dDeconvolve.c when the FLOATIZE C macro is defined.  Speedup on a\n"
   "   Linux box is about 40% (due to less memory fetch).\n"
   "* Modified mri_read_dicom.c to allow user to skip stupid GE fixup entirely.\n"
   },

  { 4,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to get rid of bug when de-sensitizing thr_rowcol for RGB\n"
   "   images.  Now it is always sensitized.  Also, afni_func.c now will deal\n"
   "   with RGB images as thresholds (just converts them to floats, though).\n"
   "* Added thd_1Dtodset.c, functions to read and write 1D files as AFNI datasets.\n"
   "* Added niml/niml_stat.c to be a place to store statistics code for NIML.\n"
   },

  { 5,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed a bug in AFNI_setup_viewing() that crept in - assumed existence\n"
   "   of fim_now, which might not be true.\n"
   "* Incorporated fix of mri_read_dicom.c from Rich Hammett, to skip false\n"
   "   targets in the Siemens extra info.  (Will no one rid me of this\n"
   "   troublesome mosaic format?)\n"
   "* Modified 1dplot.c to accept multiple timeseries file inputs.\n"
   "* Modified thd_automask.c to have a mri_automask() function as well.\n"
   "* Modified 3dAutomask.c to do fillin and exterior-clipping if -dilate\n"
   "   option is used.\n"
   },

  { 6,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dWinsor.c to use -mask option.\n"
   },

  { 7,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dAnhist.c.\n"
   },

  { 10,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.[ch] to make F2 button have Button1 operate as Button2,\n"
   "   and to show cursor as a pencil shape for drawing when this mode is on.\n"
   },

  { 11,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified tagset.h to increase number of allowed tags.\n"
   },

  { 13,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to 3dAnhist to regress histogram and plot it.\n"
   "* Changes to coxplot/plot_ts.c to have it avoid 'pushing' data limits out.\n"
   "* Changes to 1dplot.c: -xzero and -nopush options.\n"
   "* Added THD_generic_detrend() to thd_detrend.c, and used this to add a\n"
   "   -ort option to 3dTcorrelate.\n"
   "* Modified thd_notes.c and 3dNotes.c to avoid escaping the '\\' character\n"
   "   for notes input from the command line.\n"
   },

  { 14,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* A few more changes to 3dAnhist.c.\n"
   "* Modified thd_opendset.c to also deal with .1D inputs.\n"
   },

  { 18,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed 1dplot.c -stdin option to work again (oops).\n"
   },

  { 19,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dAFNItoANALYZE.c to add -orient option.\n"
   "* Added mri_flip3D.c to flip 3D images around.\n"
   "* Added thd_getorient.c to get axis direction in a dataset for\n"
   "   a given orientation code.\n"
   "* Modified mri_copy.c to work if input image doesn't have data array.\n"
   "* Added environment variable AFNI_MINC_SLICESCALE to thd_mincread.c.\n"
   "* Fixed bug in thd_mincread.c in slice-scaling when datum is floats\n"
   "   (subtracted intop instead of inbot in scaling formula).\n"
   "* Modified thd_mincread.c to downscale short or byte datasets if\n"
   "   slice scaling would push them past the maxval for that data type.\n"
   "   Also, use calloc() on im_min and im_max arrays to avoid problems\n"
   "   when those arrays in the MINC file are incomplete.\n"
   "* Modified 3drefit.c, and thd_delete.c to skip CTF and 1D files.\n"
   "* Modified 3drotate.c to skip rotation if rotation matrix is identity.\n"
   "   Also modified 3dvolreg.c and 3drotate.c to use '%.4f' format when\n"
   "   writing command string to THD_rotcom_to_matvec().\n"
   },

  { 20,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c, 3ddata.h, afni_graph.c to allow graphing of\n"
   "   datasets with only 1 point along a spatial dimension (i.e., '.1D'\n"
   "   files).\n"
   "* Modified niml/elemio.c to allow writing and reading of elements\n"
   "   with the '# ...' line format for header/trailer fields.  Also\n"
   "   modified thd_1Ddset.c to write .1D dataset files out in this\n"
   "   format.\n"
   },

  { 21,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added thd_3Ddset.c to read AFNI datasets from NIML-formatted\n"
   "   .3D files.  Corresponding changes to 3ddata.h, etc.\n"
   "* Changes from Doug Ward\n"
   "   1) Program 3dDeconvolve:  Added -quiet option to suppress screen output.\n"
   "   2) Plugin Deconvolve:  Additional input error protection for -censor and\n"
   "    -concat options.  These options could cause afni to crash if the input\n"
   "    files were not set up correctly.\n"
   "   3) Program RSFgen:  Added -table option, to generate random permutations of\n"
   "    the rows of an input column or table of numbers.  Useful for randomization\n"
   "    studies of statistical thresholds, about which more later.\n"
   "   4) Libraries matrix.c and matrix_f.c:  The recently added/modified matrix\n"
   "    routines vector_multiply and vector_multiply_subtract would produce a\n"
   "    segmentation fault for certain input matrices (e.g., null baseline model).\n"
   "    This has now been corrected (hopefully).\n"
   },

  { 23,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -xyz option to 3dmaskdump.c.\n"
   },

  { 27,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified NIML to allow ni_dimen=0 on input, and then infer length of vectors\n"
   "   from input data.\n"
   },

  { 28,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to afni_splash.[ch] to include faces!\n"
   },

  { 29,MAR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_resize() [in mri_warp.c] to properly deal with images of\n"
   "   MRI_byte, MRI_short, and MRI_rgb types.\n"
   },

  { 9,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed thd_shear3d.h function DMAT_svdrot() to work properly when input matrix\n"
   "   is singular.\n"
   },

  { 11,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/plot_ts.c to allow setting of line colors using\n"
   "   AFNI_1DPLOT_COLOR_xx environment variables.\n"
   },

  { 12,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified waver.c -tstim option to skip argv[]'s that start with whitespace\n"
   "   -- this is to deal with evil Microsoft CR-LF line breaks.\n"
   },

  { 15,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in mri_overlay.c (using data from imover instead of imov!).\n"
   },

  { 16,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dUniformize.c to allow byte-valued datasets, and added to standard\n"
   "   distributions.  Also modified estpdf3.c and pdf.c to obey the global\n"
   "   'quiet' variable, if the USE_QUIET macro is defined.\n"
   },

  { 18,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dWarp.c (along with mri_warp3D.c).\n"
   "* Minor changes to 3dAnhist.c for Lukas Pezawas.\n"
   },

  { 22,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTagalign.c (and thd_shear3d.c) heavily to use THD_warp3D() instead\n"
   "   of rotation-only functions, and to allow different kinds of transformation\n"
   "   matrices to be used.\n"
   },

  { 24,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTshift.c and thd_tshift.c to negate time shift, since it seems have\n"
   "   been wrong all these years.\n"
   },

  { 28,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dcalc to add -taxis option.\n"
   "* Added mri_fromstring.c, to input 1D data in the form '1D:5@0,10@1,5@0', etc.\n"
   },

  { 29,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c and machdep.h to add ENFORCE_ASPECT #define (for Mac OS X).\n"
   },

  { 30,APR,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_intlist.c to let '{}' bound the list as well as '[]'.\n"
   "* Modifed mri_read_1D() to use intlist of the form '{..}' to do row sub-selection,\n"
   "   as well as the older '[..]' for column sub-selection.\n"
   "* Modified most programs that used mri_read_ascii() to read timeseries files to\n"
   "   use mri_read_1D() instead, so that the '{..}' feature can be supported.\n"
   },

  { 1,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified NLfit.c and plug_nlfit.c to have NLfit_error() be able to use longjmp()\n"
   "   to deal with errors, rather than exit().\n"
   "* Modified afni_func.c to rotate color bar in steps of 4 if Shift key is pressed.\n"
   },

  { 4,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Parallel computation (-jobs option) addtions to 3dDeconvolve.c.\n"
   },

  { 6,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor changes to 3dDeconvolve.c.\n"
   "* From Rich Hammett, AFNI_TRY_DICOM_LAST environment variable.\n"
   },

  { 7,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Parallel computation (-jobs option) addtions to 3dNLfim.c.\n"
   "* Mods to mri_dicom_hdr.c to subtract 1 from rwc_err for each error message.\n"
   "   This way, will normally only print 1 such message per to3d run, which will\n"
   "   make the users happier, I hope.\n"
   "* Add mri_possibly_dicom() function to mri_read_dicom.c, and use it to censor\n"
   "   which files get the full DICOM reading treatment.\n"
   },

  { 9,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AFNI_THRESH_BIGSTEP environment variable.\n"
   "* Boxes in 3dmaskdump.c.\n"
   },

  { 12,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_initdkptr.c to have prefixes that start with '/' override any\n"
   "   input directory name.\n"
   "* Modified waver to allow 'a:b' and 'a%c' durations for '-tstim' input.\n"
   },

  { 13,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-Fourier_nopad' option to 3drotate.c, plug_nudge.c, and thd_rot3d.c.\n"
   "* Modified afni.c to have arrowpad keys do wraparound when they hit the edge.\n"
   },

  { 14,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in thd_intlist.c, where ']' or '}' might not stop the scanning of\n"
   "   the string.  This was a problem when dealing with inputs that have both\n"
   "   types of selectors -- one might run over another in the parsing.\n"
   },

  { 29,MAY,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dttest.c to output t-statistic brick in floats if diff brick is\n"
   "   stored as floats.\n"
   "* Modified 3dcalc.c to floatize datasets that don't have constant sub-brick\n"
   "   datum.\n"
   "* Per the request of Ziad Saad, added function NI_add_column_stride() to\n"
   "   niml/niml_element.c.\n"
   },

  { 6,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml/niml_stream.c to disable reopen of tcp: stream as shm: if\n"
   "   AFNI_NOSHM environment is set to YES.\n"
   },

  { 11,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c NOT to call AFNI_set_thresh_top() in AFNI_setup_viewing()\n"
   "   when changing functional datasets.\n"
   },

  { 13,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to prevent resized windows from getting bigger than\n"
   "   AFNI_IMAGE_MAXFRAC (default=0.9) times the screen dimensions.\n"
   "* Modified niml/niml_elemio.c to make attribute string buffer size expand\n"
   "   when Ziad writes huge attributes, the fiend.  Also put a newline before\n"
   "   each attribute, whether we want it or not.\n"
   },

  { 16,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* SUMA+AFNI ROI stuff.\n"
   },

  { 20,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to add ISQ_snapshot(Widget) functionality.  Tested\n"
   "   in Xphace program.\n"
   "* Modified Makefile.INCLUDE to make libmrix.a that includes all the\n"
   "   X11 utilities (imseq.c, xutil.c, xim.c, etc.).\n"
   },

  { 25,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* ISQ_snapfile(Widget) added to imseq.c; other tweaks to ISQ_snapshot().\n"
   },

  { 26,JUN,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved some snapshot stuff to xim.c rather than imseq.c.\n"
   "* Modified afni.c to have it continue after fatal X11 errors.\n"
   "* New program 1ddot.c.\n"
   },

  { 1,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to change pbar hints when 'Func=Threshold' is pressed.\n"
   "* Added RWC_XtPopdown() to xutil.[ch], and modified most code to use this\n"
   "   rather than XtPopdown().\n"
   "* Added empty XtErrorHandler to afni.c to try to avoid crashes when an Xt\n"
   "   error occurs.\n"
   "* Added mri_equal.c, which checks if 2 images are equal.  Used in ISQ_snapshot()\n"
   "   to avoid saving duplicate images in succession.\n"
   },

  { 3,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ISQ_snapsave() to imseq.c, which lets the user supply the image to be\n"
   "   saved in a snapshot, rather than acquire it from a widget like ISQ_snapshot().\n"
   },

  { 6,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmaskave.c to add -median option.\n"
   },

  { 10,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in afni_graph.c of colors in dplots from Dataset#N plugin.\n"
   },

  { 15,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Included FreeBSD patches from Jason Bacon.\n"
   },

  { 18,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifed imseq.[ch] (etc.) to include a 'pen' box to turn on the 'pen drawing'\n"
   "   Button-1 mode (only available when drawing has been enabled).\n"
   },

  { 21,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified all uses of XmCreatePopupMenu() to make parent widget different than\n"
   "   the Button-3 popup widget when using Solaris -- some bug in Solaris seems to\n"
   "   cause a popup problem when the parent of the menu is also the one getting\n"
   "   the ButtonPress event.\n"
   "* Modified afni_graph.c, imseq.c, afni.c, plug_nth_dataset.c, etc., to have\n"
   "   the Dataset#N, Dataset#2, and Expr0D plugin windows open when these\n"
   "   transformations are selected from menus.\n"
   "* Modified the parser to take longer expressions.\n"
   },

  { 22,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More expansion in parser.f, etc.\n"
   "* Modified coxplot/plot_ts.c and 1dplot.c to let user control x- and y-axes\n"
   "   in more detail.\n"
   },

  { 23,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to thd_info.c to print more than 8000 characters from the History.\n"
   "* Changes to thd_notes.c to make Addto_History work right.\n"
   "* Changes to 3dcalc.c to use Addto_History correctly.\n"
   },

  { 28,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dcalc.c to force scaling for short and byte output datum when\n"
   "   non-integer values are found in the dataset.\n"
   },

  { 29,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Many many changes to make func and anat datasets work interchangeably in\n"
   "   the interactive AFNI.\n"
   "* 3dmerge.c now has -verb option.\n"
   },

  { 30,JUL,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_nudge.c to add NN interpolation option.\n"
   "* Modified THD_open_3dcalc() to make dataset directory './' after it is input,\n"
   "   so that EDIT_empty_copy() of it will not put new dataset into /tmp.\n"
   "* Modified afni_func.c and afni_sumafunc.c to threshold byte and short\n"
   "   overlays in float rather than int.\n"
   "* Modified FUNC_IS_STAT() and FUNC_HAVE_PVAL() macros in 3ddata.h to return 1\n"
   "   only if the code supplied corresponds to a valid statistic code.\n"
   "* Various fixes to the anat/func interchangeability stuff.\n"
   },

  { 5,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_1Ddset.c to read a multi-column .1D file as a 3D+time dataset\n"
   "   rather than a bucket, if AFNI_1D_TIME is set to YES.\n"
   "* Modified mri_write_ascii() to write the stdout if the filename is '-'.\n"
   "* Modified various *.c files to avoid warning messages on OS X compiles.\n"
   },

  { 6,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Somehow, a bug crept into the Read Session function (in afni_func.c) that\n"
   "   rejected new sessions with # datasets > 0 rather than # datasets == 0.\n"
   "* Added quintic interpolation to mri_warp3D.c, and 3dWarp.c.\n"
   "* Added -fsl_matvec option to 3dWarp.c.\n"
   "* plug_3ddup.c created (but not part of the binaries, yet).\n"
   },

  { 7,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed small typo in index in mri_warp3D.c quintic code.\n"
   "* Fixed CYGWIN bracket placement at end of thd_loaddblk.c.\n"
   },

  { 8,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Problem: when anat_now == fim_now, and fim_now got set to\n"
   "   'Warp Func on Demand', then trouble.  Solutions\n"
   "   - make AFNI_VIEW_FUNC_BRICK and AFNI_VIEW_ANAT_BRICK default to YES.\n"
   "   - make sure if anat_now == fim_now, Func WOD is turned off.\n"
   },

  { 11,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in afni_sumafunc.c, where func threshold image is bytes -- was\n"
   "   using index ar_thr[ii] instead of ar_thr[jj] -- not good.\n"
   },

  { 15,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -version option to afni.c, per Rick Reynolds.\n"
   },

  { 23,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added AFNI_MAX_OPTMENU environment variable (bbox.c, etc.).\n"
   "* Modified Makefile.INCLUDE to chmog ugo+x the ./configure files\n"
   "   in a couple of subdirectories.\n"
   },

  { 24,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dLRflip.c to give output dataset a new prefix (oops).\n"
   },

  { 26,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to skip printing adoption warnings unless user\n"
   "   explicitly requests them with an environment variable.\n"
   },

  { 28,AUG,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Widespread changes, including addition of thd_niftiread.c, to read\n"
   "   NIFTI-1 formatted files as datasets.\n"
   "* Modified afni.c to read datasets individually from command line argv's\n"
   "   if they can't be read as sessions.\n"
   },

  { 15,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added poetry.\n"
   "* Removed 'points'.\n"
   "* Added Dtables to niml.\n"
   },

  { 20,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed scaling bug in mri_warp3D.c -- datasets with scaling factors were\n"
   "   being scaled before warp, but not unscaled after warp, so that the\n"
   "   surviving scale factor would be applied twice.\n"
   "* Added labelizing to Draw Dataset plugin.\n"
   },

  { 21,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added label popup menu to Draw Dataset plugin.\n"
   "* Added Button1 click in intensity bar to re-aspect image window.\n"
   "   Also, skip attempt to reconfigure if happened before within last 33 ms.\n"
   "   This is to avoid getting into fights with the window manager.\n"
   },

  { 22,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Inten->Save Palette with colorscales now saves a colorscale file.\n"
   "* Inten->Read Palette with colorscales now reads a colorscale file.\n"
   "* AFNI_MPEG_FRAMERATE in imseq.c.\n"
   "* Extend short input time series in 3dDeconvolve.c.\n"
   },

  { 23,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified Button1 in intensity bar to always do re-aspect, even if free\n"
   "   aspect is on.\n"
   "* Added Button1 stroke right/left in image window to change contrast and\n"
   "   brightness together; changes to imseq.[ch], display.[ch], and\n"
   "   afni_pplug_env.c.\n"
   },

  { 24,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Graymap Plot' to imseq.c.\n"
   },

  { 27,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Auto-popup and auto-place graymap plot.\n"
   "* Change plug_drawdset.c to disable auto-popup of graymap plot when plugin\n"
   "   is open, and re-enable it when plugin closes.\n"
   },

  { 28,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to thd_automask.c and 3dAutomask.c to implement -eclip option.\n"
   },

  { 29,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Allow '# ' as a comment in .afnirc -- changes to afni_setup.c GETSTR macro.\n"
   },

  { 30,OCT,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Some changes (f2cdir/ and afni_plugin.c) for Mac OS X 10.3 compilation.\n"
   },

  { 4,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to move crosshair focus on Button1 release, rather than\n"
   "   press.  This makes the graylevel change via Button1 motion not have\n"
   "   the annoying focus change side effect.\n"
   "* Modified afni.c, etc., to implement new environment variables\n"
   "   AFNI_IMAGE_MINTOMAX and AFNI_IMAGE_GLOBALRANGE.\n"
   "* Modified afni_plugin.[ch] to allow plugins to change the 'Run' button\n"
   "   labels to something else.\n"
   },

  { 5,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to auto-popdown graymap plot if it was auto-popupped\n"
   "   in the first place.\n"
   "* Also added 'Edit Environment' button to image window popup menu.\n"
   },

  { 13,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added argmax() and argnum() functions to parser.f and 3dcalc.c.\n"
   "* Modified Makefile.solaris28_suncc per Vince Hradil's experience.\n"
   "* Split up load vector loops in PAREVEC() in parser.c, for speed.\n"
   },

  { 14,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More minor changes to 3dcalc.c and parser.f for slight speedups.\n"
   },

  { 18,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to prevent beep when user tries to zoom when Mont\n"
   "   is on, or vice-versa.\n"
   },

  { 19,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Multiple level undo/redo in plug_drawdset.c.\n"
   },

  { 20,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in arrowpad movement, in afni.c: must do LOAD_DSET_VIEWS(im3d).\n"
   "* Modified afni.c and afni_version.c to write UPDATER script, if possible.\n"
   },

  { 21,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ability to undo Linear Fillin to plug_drawdset.c.\n"
   },

  { 24,NOV,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fun with Apple's speech synthesis.\n"
   },

  { 1,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to add AFNI_IMAGE_ZEROCOLOR environment variable.\n"
   "* Modified mcw_glob.[ch] to add simple-to-use function MCW_wildcards().\n"
   "   Used this in afni_splash.c as a test.\n"
   },

  { 3,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Program mpegtoppm -- in mpegtoppm_dir/.\n"
   "* Read images and datasets from MPEG files, via mpegtoppm.\n"
   "* 'm' and 'M' keys in imseq.c.\n"
   },

  { 4,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Change 'm/M' to 'v/V', and also add to afni_graph.c.\n"
   },

  { 5,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Some tweaks to mpegtoppm and mri_read_mpeg.\n"
   "* Fixed bug in niml/niml_element.c -- NI_free_element() would fail if\n"
   "   nel->vec was NULL.\n"
   "* Similar problem in thd_3Ddset.c.\n"
   },

  { 7,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified ts.c to allow '#' comments in RWC_read_time_series().\n"
   },

  { 16,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to niml/ functions to make them g++ compatible.\n"
   "* Added 'r/R' to imseq.[ch] and afni_graph.[ch].\n"
   },

  { 17,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed Amalloc.h bug.\n"
   },

  { 23,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Checked in many changes to deal with compilation of afni with g++\n"
   "   (version 3.2 or later).\n"
   },

  { 30,DEC,2003 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed annoying bugs in NIML stream I/O.\n"
   "* Modified niml/niml_do.c to allow user to register callbacks that\n"
   "   supplement builtin verbs.\n"
   },

  { 2,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More annoying bugs in NIML stream I/O.  Also, when a socket is\n"
   "   closed, send 1 byte of OOB data so that the receiving process\n"
   "   will receive SIGURG.  The new SIGURG handler will then shut\n"
   "   the socket down on the other end, without the user having to\n"
   "   read the 'close_this' element.\n"
   },

  { 7,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifications to plugins and models to make them work with g++.\n"
   },

  { 8,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifications to mcw_malloc.c to print out traceback chain when\n"
   "   corruption is detected.\n"
   "* Added ENTRY()/RETURN() to a number of mri_*.c functions.\n"
   "* Modified afni.c to display surface overlay from other datasets in\n"
   "   the same directory, if the current underlay datasets doesn't have\n"
   "   any surfaces.\n"
   },

  { 10,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mrilib.h, mri_read.c, and to3d.c to allow use of inter-slice\n"
   "   spacing to override slice thickness, at least for GE I.* files.\n"
   },

  { 12,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to draw graymap in histogram style.  Also added\n"
   "   'ent=' entropy value to numerical range display.\n"
   },

  { 13,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to alter operation of AFNI_SLICE_SPACING_IS_GAP\n"
   "   so that 'NO' means use the Spacing attribute even if it is smaller than\n"
   "   the Thickness attribute.  Seems to be needed for Phillips.\n"
   },

  { 14,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified coxplot/pwritf.f to allow color changes in the text, and to\n"
   "   allow disabling of the escape mechanism (so filenames with '_' are OK).\n"
   "* Modified 3drotate.c, 3AnatNudge.c, 3dLRflip.c, 3dTagalign.c, 3copy.c to\n"
   "   allow processing of non-AFNI (e.g., MINC) datasets.  The problem was\n"
   "   that these program open/load a dataset, modify it in-place, rename it,\n"
   "   then write it out.  That won't quite work for non-AFNI datasets, since\n"
   "   the dataset is still marked as being MINC (say), and we can't write\n"
   "   those directly.  Solution: mark the dataset as AFNI-format, after\n"
   "   loading it and before changing its name.\n"
   "* Modified Makefile.* to use a 'MAKE' macro instead of the fixed 'make'\n"
   "   command.\n"
   },

  { 15,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c so that -skip_afnirc option works again (was being screwed\n"
   "   up in machdep() function).  Also added a couple friends.\n"
   "* When re-f2c-ing a .f file, must remove declarations of built-in functions\n"
   "   from the C code, since they cause the g++ compilation to fail to link.\n"
   "   Also, in coxplot/*.c functions, must manually change the COMMON struct\n"
   "   definitions to extern.\n"
   "* Added SHORTIZE() and BYTEIZE() to mri_to_short.c and mri_to_byte.c to\n"
   "   avoid integer overflow problems when scaling and/or changing data types.\n"
   },

  { 16,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dFDR.c to produce some output when -input1D option is used!\n"
   },

  { 23,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifications to put surfaces into sessions rather than directories.\n"
   "* Modified ISQ_show_zoom() in imseq.c to avoid infinite recursion that\n"
   "   seems to happen when user zooms, crops, then changes image fraction\n"
   "   (with the 'i' arrows). WTF?\n"
   "* Modified imseq.[ch] to NOT place dialog near changed window when closing\n"
   "   the Montage control dialog, since that hangs up for a while on the Mac.\n"
   "* Modified afni.c to make sure surface boxes are plotted with line\n"
   "   thickness zero.\n"
   },

  { 27,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'WinAver' feature to afni_graph.[ch] and afni.c.  This shows the\n"
   "   'ideal' waveform as the average of all the timeseries in a graphing\n"
   "   window.\n"
   "* Modified afni.[ch] and afni_widg.c to set a flag in each im3d, so that\n"
   "   if it is created when the dummy dataset is present, then when a real\n"
   "   dataset becomes available, the controller will get set to the middle\n"
   "   of THAT dataset's coordinates, rather than the dummy's middle.  This\n"
   "   is useful for realtime imaging (which is why the dummy is there).\n"
   },

  { 28,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added hints to various menu items that were lacking them in afni_graph.c.\n"
   },

  { 29,JAN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added the cute little diagonal 'AFNI' to various windows.\n"
   "* Modified rtfeedme.c to add the '-gyr' option to test GRAPH_[XY]RANGE.\n"
   },

  { 6,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added threshold locking (via environment variable AFNI_THRESH_LOCK).\n"
   "   Also, moved lock code from afni.c to new file afni_lock.c.\n"
   },

  { 7,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added pbar locking (via environment variable AFNI_PBAR_LOCK), and\n"
   "   threshold p-value locking.\n"
   "* Added AFNI_DISP_SCROLLBARS to afni_pplug_env.c, to control if Disp\n"
   "   menu in image viewer gets scrollbars.\n"
   },

  { 10,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made threshold locking move sliders during drag, not just end of drag.\n"
   "   Also change pval at bottom of locked sliders during drag.  Also put\n"
   "   Edit Environment button on top-of-pbar popup menu.\n"
   },

  { 11,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in afni_graph.c about average time series, when graph is\n"
   "   too short.  I think.  [cf. 27,JAN,2004]\n"
   },

  { 12,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oooops.  '\\noesc' should be '\\\\noesc' in several places.  My bad.\n"
   "* Modified mri_read.c to allow GE 'IMGF' flag to be anywhere in 1st 4K\n"
   "   of file, if file starts with 'GEMS' instead of 'IMGF'.\n"
   },

  { 19,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added -mask and -srad and 5th-value=radius features to 3dUndump.c.\n"
   },

  { 23,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added range locking to afni_lock.c and afni_func.c.\n"
   "* Added tick marks to imseq.c.\n"
   "* Rick Reynolds added NN interpolation option for zooming to mri_dup.c.\n"
   },

  { 24,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in thd_sheard3d.c, when input matrix to rot_to_shear_matvec()\n"
   "   is identity, could get a bad shear.  In this case, just manually put\n"
   "   the correct shear into place.\n"
   },

  { 29,FEB,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Re-fixed the thd_shear3d.c bug of 24,FEB,2004, since it was wrong.\n"
   },

  { 9,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'ms' time unit selection to 3dcalc.c -dt/-taxis options.\n"
   },

  { 11,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified use of SPM originator field in thd_analyzeread.c to subtract\n"
   "   1 from indexes.\n"
   "* Modified mri_warp3D.c and 3dWarp.c to do MNI<->TTA transforms.\n"
   "* Don't need '-eval' option on ccalc command line anymore.\n"
   },

  { 12,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.[ch], afni_widg.c, to put popup DICOM/SPM coordinate menu\n"
   "   on crosshair coordinate label.\n"
   "* 'Anatomy' -> 'Underlay' and 'Function' -> 'Overlay' in several places.\n"
   },

  { 15,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More 'Anatomy' -> 'Underlay' and 'Function' -> 'Overlay' stuff.\n"
   "* Added optmenu_EV_fixup() to bbox.c - change cursor arrow on optmenu popups.\n"
   },

  { 17,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AFNI_GRAPH_AUTOGRID variable in afni_graph.c.\n"
   "* Fixed memory estimate in 3dAutoTcorrelate.c.\n"
   },

  { 18,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed plug_nth_dataset.c to right fill short time series with WAY_BIG,\n"
   "   and then afni_graph.c to not plot these tails in the dplot overlay.\n"
   "* Modified afni_graph.[ch] to allow pin_bot as well as pin_top (ugh).\n"
   },

  { 19,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added vector chooser to bbox.[ch] and used in afni_graph.[ch] for choosing\n"
   "   graph pin top and bottom together.\n"
   },

  { 21,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AFNI_DISABLE_CURSORS in xutil.c.\n"
   },

  { 22,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AFNI_SLAVE_FUNCTIME in afni.c.\n"
   "* Modified 3dvolreg.c to make -wtrim always be on, and to scale init\n"
   "   for the twopass iteration.\n"
   },

  { 23,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* 3dZeropad.c gets new -RL, -AP, -SI options.\n"
   },

  { 24,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify Intracranial.c to deal with an optimizing bug on Mac OS X\n"
   "   -- doesn't work well with large auto arrays.\n"
   },

  { 31,MAR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Weird Mac problem: afni_graph.c crashes when destroying graph viewer\n"
   "   Widgets, but only after the timeseries chooser is popped up.  Stupid\n"
   "   solution is to only unrealize widgets if this chooser was opened by\n"
   "   the user.  WTF?\n"
   },

  { 2,APR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Mods to fix auto_grid()-ing in afni_graph.[ch].\n"
   },

  { 5,APR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixes to afni_graph.c to spackle over unexplainable crashes on Mac when\n"
   "   a timeseries chooser is opened and later the graph window is closed.\n"
   },

  { 8,APR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AFNI_X11_REDECORATE in xutil.h and afni_pplug_env.c, to replace MCW_isitmwm().\n"
   },

  { 9,APR,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed lack of fclose() in ts.c (a 10 year old bug!).\n"
   },

  { 11,MAY,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to apply Zero Color to RGB images.\n"
   },

  { 12,MAY,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dvolreg.c to make -sinit turn scale init off for -twopass.\n"
   },

  { 8,JUN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added call to THD_copy_auxdata() to mri_warp3D.c, and -copyaux option to\n"
   "   3drefit.c\n"
   "* Added AFNI_IMAGE_SAVESQUARE environment variable to imseq.c; added\n"
   "   functions to mri_warp.c to implement re-aspectizationing.\n"
   },

  { 21,JUN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dclust -help to print out info about coordinate systems.\n"
   "* Modified afni_func.c to add environment variable AFNI_BUCKET_LABELSIZE\n"
   "   to modify bucket label sizes.\n"
   },

  { 22,JUN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c to set various environment variables to new defaults\n"
   "  - crosshair lines are on instead of off\n"
   "  - controllers are locked instead of unlocked\n"
   "  - save square is on instead of off\n"
   },

  { 23,JUN,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified AFNI_leave_EV in bbox.c to avoid memory leak (must free up the\n"
   "   retrieved textfield string if an early exit is taken).\n"
   },

  { 8,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read.c to allow line lengths of up to 512K in *.1D files.\n"
   "* Modified coxplot/plot_ts.c to correctly remove labels from separate plot\n"
   "   boxes if input nnayy==0.\n"
   "* Modified 1dgrayplot.c to have new '-sep' option.\n"
   },

  { 14,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c to make some basic checks\n"
   "   - equal filenames for -stim_file options\n"
   "   - zero columns in X matrix\n"
   "   - collinear column pairs in X matrix\n"
   "   - compute matrix condition number\n"
   "* Modified matrix.[ch] and matrix_f.[ch] to support this stuff.\n"
   "* Disabled 3dDeconvolve_f binary, sort of.\n"
   },

  { 15,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c and Deconvolve.c to have -legendre option, for\n"
   "   use of better polynomials for the baseline estimation.\n"
   "* Modified matrix.c and matrix_f.c to have matrix_inverse_dsc() function,\n"
   "   to use diagonal scaling before/after the matrix inversion.  Modified\n"
   "   RegAna.c to use this function, to avoid stupid scaling issues.\n"
   "   Also modified condition number calculation to take this into account.\n"
   },

  { 16,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified Deconvolve.c to use correctly normalized Legendre polynomials,\n"
   "   for potential ease-of-use for people who want to know what the\n"
   "   baseline functions are.\n"
   },

  { 19,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified matrix.c and RegAna.c to do matrix solutions with pseudo-inverse\n"
   "   from SVD, rather than normal equations.\n"
   "* Adapted svd.f into eis_svd.c a little more, and also wrote a wrapper\n"
   "   function into cs_symeig.c, and a test program 1dsvd.\n"
   },

  { 20,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_get1D.c to limit loading of 1D files in AFNI to a max size\n"
   "   set by environment variable AFNI_MAX_1DSIZE (default 123K).\n"
   "* Modified mri_read_ascii() in mri_read.c to use the 'n@val' method for\n"
   "   value duplication.  Also modified my_fgets() to return a duplicate\n"
   "   of the previous line if the first two nonblank characters on the line\n"
   "   are ''.\n"
   },

  { 21,JUL,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modifed MCM_get_intlist() to print error messages when the user inputs\n"
   "   values off the top end of the range, instead of silently turning them\n"
   "   into the top (stupid users).\n"
   "* Modified afni_fimmer.c to correctly use scaling factors if present in\n"
   "   the time series data (oops, for about 8 years).\n"
   "* Added printout of pseudo-inverse to 1dsvd.c.\n"
   "* Added -svd option to 3dDeconvolve.  Also, if SVD is on, then DON'T\n"
   "   remove all zero stimuli from the list.\n"
   "* Added -xjpeg option to 3dDeconvolve: grayplot of X matrix.\n"
   },

  { 22,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_drawing.c and coxplot/plot_cox.c to allow for opacity.\n"
   "   Used this in -xjpeg option in 3dDeconvolve.c.\n"
   },

  { 28,JUL,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Finished (I hope) addition of -xsave and -xrestore options to 3dDeconvolve.\n"
   "* Fixed (I hope) bug in AFNI_setup_viewing() in afni.c, where the ULay\n"
   "   sub-brick chooser optmenu wouldn't be configured correctly in controller\n"
   "   [B] (etc.) due to anat_old check not also checking im3d_old.\n"
   "* Modified mri_read_ascii() and my_fgets() in mri_read.c to not malloc/free\n"
   "   line buffer for each line read.  Also, removed the '' feature.\n"
   "* Added mri_read_ascii_ragged() to mri_read.c.\n"
   },

  { 29,JUL,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added mri_symbolize.c, and -gltsym option to 3dDeconvolve.\n"
   },

  { 2,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified matrix_f.c to include loop unrolling found in matrix.c.\n"
   "* Modified mri_symbolize.c and 3dDeconvolve.c to use '[[...]]' subscripting\n"
   "   for -gltsym multi-row expansion.\n"
   },

  { 3,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified matrix_print() in matrix.c and matrix_f.c to print shorter strings\n"
   "   when the matrix comprises all 1 digit integers.\n"
   "* Modified 3dDeconvolve.c to print -gltsym matrices when AFNI_GLTSYM_PRINT\n"
   "   environment variable is YES.\n"
   "* Modified matrix_f.c to use Altivec on Mac for matrix-vector multiplies.\n"
   "   Adds about 10% to speed on G5, over the loop unrolling from yesterday.\n"
   },

  { 4,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* At long last, removed the ill-fated 'merger' stuff from 3ddata.h and\n"
   "   all AFNI functions.\n"
   "* Added THD_open_tcat() in thd_opentcat.c, to open a list of datasets as\n"
   "   on long 3D+time dataset.  Also modified 3ddata.h, THD_open_dataset(),\n"
   "   and so forth.\n"
   "* Modified 3dDeconvolve.c to use this feature to allow input catenation\n"
   "   (if the input_filename field has blanks in it).\n"
   },

  { 5,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Polished up the 3dDeconvolve.c changes.\n"
   "* Added 'README.environment' text to Datamode->Misc menu in AFNI.\n"
   },

  { 6,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3dDeconvolve.c where -cbucket bricks were colliding with\n"
   "   -bucket bricks, vi bar[] and attach_sub_brick().\n"
   },

  { 10,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Started work on 3dDeconvolve.c to add -stim_times option for direct\n"
   "   input of models to fit.\n"
   "* Modified mri_read_ascii_ragged() to read a '*' character as a fill,\n"
   "   thus allowing lines with no entries or intermediate missing entries.\n"
   },

  { 11,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added environment variable AFNI_3dDeconvolve_nodup to 3dDeconvolve.c,\n"
   "   to stop processing if duplicate columns are discovered.\n"
   },

  { 12,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified Deconvolve.c and 3dDeconvolve.c to remove mean from baseline\n"
   "   timeseries if polort>0, and -nodmbase option is not given.\n"
   "* 3dDeconvolve saga: Generate response vectors from stimulus timing and\n"
   "   user-specified model.\n"
   },

  { 19,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* eis_svd.c sometimes works wrong with 'gcc -O', so modified Makefile.INCLUDE\n"
   "   and eispack/Makefile to turn optimization off for this file.\n"
   },

  { 23,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug with polort=-1; program creates a baseline model matrix with\n"
   "   0 columns, and matrix_psinv() didn't like that.\n"
   "* Add basis_write_response() to 3dDeconvolve.c to actually write out the\n"
   "   computed IRF for symbolic models.\n"
   },

  { 29,AUG,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added EXPR(bot,top) basis function to 3dDeconvolve.c.\n"
   },

  { 30,AUG,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c basis_write_response() to be more efficient (don't\n"
   "   evaluate basis expressions so often).\n"
   "* Added basis_write_sresp() to 3dDeconvolve.c to write standard deviation\n"
   "   of the IRFs.\n"
   },

  { 2,SEP,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_symbolize.c to correctly use the intlist() function.\n"
   "* Patched thd_auxdata.c and thd_initdblk.c not to create brick labels\n"
   "   over 32 characters in length.  (Someone who shall remain nameless\n"
   "   created one several thousand characters long, and thd_info.c didn't\n"
   "   like that much.)\n"
   },

  { 7,SEP,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed some stuff in the -help output of afni.c.\n"
   },

  { 9,SEP,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_version.c to use TCP/IP to communicate with version\n"
   "   sub-process rather than shared memory.\n"
   },

  { 15,SEP,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New function THD_get_voxel() in thd_loaddblk.c, returns a float for a\n"
   "   single voxel (not very efficient to use this in mass).\n"
   "* Used the above in thd_fdto1D.c to deal with returning time series from\n"
   "   datasets with mismatched sub-brick types.\n"
   "* Also, print a warning for such datasets in thd_initdblk.c.\n"
   },

  { 5,OCT,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to write animated GIF files with a fixed colormap,\n"
   "   to avoid color flashing when (e.g.) rotating a volume rendering.\n"
   },

  { 6,OCT,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_graph.[ch] to add a chooser to set the thickness of\n"
   "   'Thick' lines in the graph viewers (on the 'Opt->Colors, Etc.' menu).\n"
   "   Also, afni.c to add AFNI_graph_gthick to initialize this value.\n"
   },

  { 20,OCT,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified parser.f to add NOTZERO, ISZERO, and EQUALS functions.\n"
   },

  { 21,OCT,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Also added ISPOSITIVE and ISNEGATIVE functions to parser.f\n"
   },

  { 22,OCT,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifed thd_shear3d.c to use true SVD in computing the rotation for\n"
   "   3dTagalign, rather than the symmetric eigensolution method.\n"
   },

  { 29,OCT,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dfim+.c to make Spearman and Quadrant CC bricks fico.\n"
   "* Modified niml/niml_rowtype.c to auto-define VEC_basictype_len types\n"
   "   when first referenced.\n"
   },

  { 3,NOV,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Some changes to 3dAnhist.c for Lukas and Katie.\n"
   },

  { 16,NOV,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to print warnings when it can't find Save filter programs.\n"
   },

  { 30,NOV,2004 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* First version of 3dWarpDrive released.\n"
   "* 3dcalc now prints a warning if outputing a byte-valued dataset when the\n"
   "   calculated results had some negative values.\n"
   },

  { 1,DEC,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added -twopass option to 3dWarpDrive, etc.\n"
   },

  { 6,DEC,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed '<a..b>' dataset input without '[...]' input to not print a warning\n"
   "   message about the lack of sub-brick subscripts.\n"
   "* Modified 3dmaskave.c to add '-mask SELF' option.\n"
   },

  { 9,DEC,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* PURGE_MEMORY addition to afni_driver.c.\n"
   },

  { 17,DEC,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AFNI_faceup() in afni_splash.c and on the hidden popup menu.\n"
   },

  { 20,DEC,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to allow user to threshold RGB-valued overlays.\n"
   },

  { 21,DEC,2004 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to allow dataset 'label2' field to be displayed in\n"
   "  window titlebars, instead of filenames.  Also affected: afni_pplug_env.c\n"
   "  and 3drefit.c (to let user change label2 field).\n"
   "* Replaced VERSION with AFNI_label.h header, generated by script Ctag.\n"
   },

  { 22,DEC,2004 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporated 3dMean.c changes from Mike Beauchamp to add standard deviation\n"
   "  option.\n"
   "* Fixed stupid scale_thr bug in afni_func.c.\n"
   },

  { 3,JAN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_imcount() in mri_read.c to not do '3D:' counting on a file\n"
   "   unless there is actually a colon in the filename!  D'oh.\n"
   },

  { 4,JAN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_warp3D_align.c and 3dWarpDrive.c to fix up some issues with\n"
   "   -twopass on small (EPI-sized) bricks and to add -1Dfile option.\n"
   },

  { 5,JAN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 1dsvd.c to add -1Dright option.\n"
   "* Fixed bug in 1dplot.c -stdin option: failed to skip leading blanks properly\n"
   "   when scanning input lines for '#' comment characters!  (Stupid)\n"
   "* Modified imseq.[ch] to add saving of overlay MEM_plotdata stuff with the\n"
   "   recorder, as well as the images.\n"
   },

  { 6,JAN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to free the pen_bbox and labsz_av when destroying an image\n"
   "   viewer -- somehow these got overlooked in ISQ_free_alldata().\n"
   },

  { 14,JAN,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added program whereami, from Mike Angstadt of U Chicago.\n"
   },

  { 24,JAN,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added environment variable AFNI_SLAVE_THRTIME to afni.c.\n"
   },

  { 1,FEB,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added -redo_bstat option to 3drefit.c.\n"
   },

  { 7,FEB,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in Deconvolve.c with removing mean of -stim_base columns.\n"
   },

  { 16,FEB,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dhistog.c to remove -thr stuff and add -doall stuff.\n"
   },

  { 18,FEB,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_driver.c, imseq.c, and afni_graph.c to add 'keypress='\n"
   "   modifiers to the OPEN_WINDOW commands for driving AFNI.\n"
   },

  { 22,FEB,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dTshift.c and to3d.c to have new 'alt+z2' option.\n"
   "* New program 3dMedianFilter.\n"
   "* Added I:*.1D stuff to 3dcalc.c.\n"
   },

  { 23,FEB,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified to3d.c to accept -Torg option.  Also to thd_info.c to print\n"
   "   out the ttorg field.\n"
   },

  { 24,FEB,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dmaskave.c to have -max option.\n"
   "* Modified niml/ functions to generalize NI_group elements so that any\n"
   "   element name is OK, provided attribute ni_form='group' is present.\n"
   },

  { 25,FEB,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add -min option to 3dmaskave.c.\n"
   "* Fixed memory leak in niml_dtable.c (forgot to free second copies of strings).\n"
   "* New niml_registry.c stuff, for allocating 'registered' structs.\n"
   },

  { 26,FEB,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_ctfread.c to seek backwards from end rather than forwards; this\n"
   "   makes it work with the new CTF svl format.\n"
   },

  { 28,FEB,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops -- used '&&' instead of '||' in byte swap test in new thd_ctfread.c.\n"
   "* More surgery on NIML registry and Htables.\n"
   },

  { 1,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified matrix.c and matrix_f.c to use BLAS-1 on the SGI Altix.\n"
   },

  { 2,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Further matrix.c changes for BLAS-1 on Solaris.\n"
   },

  { 4,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Further matrix.c changes: store as one array, not an array-of-arrays,\n"
   "   except on Solaris, where the array-of-array approach is faster(!?).\n"
   },

  { 7,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Ooops.  Have to '#include <machdep.h>' into matrix_f.h to make sure\n"
   "   DONT_USE_MATRIX_MAT is defined.\n"
   },

  { 8,MAR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Including retroicor stuff from Fred Tam.\n"
   },

  { 9,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New functions to (a) write dataset struct stuff into attributes (moved\n"
   "   out of THD_write_dataset and THD_write_datablock); to (b) convert\n"
   "   dataset attributes to/from a NIML group; to (c) populate datablock\n"
   "   from attributes, rather than do so on-the-fly as they are read in.\n"
   },

  { 11,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed up the NIML-ization of datasets, and their transmission into\n"
   "   AFNI.\n"
   },

  { 18,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Finished up NIML bulk transfer of datasets to AFNI, etc.\n"
   },

  { 21,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Edgize the overlay.\n"
   },

  { 22,MAR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* mean(), stdev(), and sem() for parser and 3dcalc.\n"
   "* Modify Ziad's plugout_drive.c '-com' option to execute commands in\n"
   "   order given, rather than the reverse order.\n"
   "* Fix REDISPLAY in afni_driver.c (oopsie).\n"
   "* Added '<MRI_IMAGE ...>' input to afni_niml.c, to store as .1D files.\n"
   "* Added '-Dname=val' option to afni.c (set environment variables).\n"
   },

  { 28,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to the AFNI update script written out in afni_version.c.\n"
   "* Modified total_bytes field in datablock structure to be int64_t rather\n"
   "   than int, to better deal with huge datasets.  Modified a lot of places\n"
   "   that refer to this field, especially places dealing with reading and\n"
   "   writing datasets.\n"
   "* Modified thd_loaddblk.c to auto-update brick statistics for non-AFNI\n"
   "   datasets.\n"
   },

  { 29,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to call AFNI_reset_func_range() at start of\n"
   "   AFNI_func_overlay() to make sure brick ranges are set properly\n"
   "   for display.\n"
   "* Modified fim+.c and 3dfim+.c to allow polort > 2, by using Legendre\n"
   "   polynomials for the baseline model.\n"
   "* Fixed bug in NIML, where the new 'outmode' field in elements wasn't\n"
   "   being initialized to -1 like it should have been.\n"
   },

  { 30,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in AFNI_range_label() in afni_func.c about brick range\n"
   "   setup change of yesterday (forgot to initialize stats_*_ok).\n"
   },

  { 31,MAR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops.  Fixed bug in afni_niml.c wrt <?drive_afni ... ?> processing\n"
   "   instruction: needed to put the command into an attribute.\n"
   "* Added a 1 ms wait to niml/niml_stream.c when a tcp: or shm: stream\n"
   "   is closed.  This is to let the application on the other end have\n"
   "   a decent interval to fetch any just-transmitted data.\n"
   "* Modified thd_opendset.c to NOT print an error message when trying to\n"
   "   open a non-existent file -- now just prints a message if the file\n"
   "   exists but has no data.\n"
   },

  { 4,APR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Pleg' function to parser and thence to 3dcalc, etc.\n"
   },

  { 6,APR,2005 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added '.nii' output feature to THD_writedset() function, and did\n"
   "   a few other similar things hither and yon.\n"
   },

  { 7,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_niftiread.c to use the NBL functions in rickr's\n"
   "   nifti1_io.c to read the data from a .nii file -- this makes\n"
   "   .nii.gz files work as well, automagically.\n"
   "* Fixed bug in imseq.c -- logic for taking the button box value\n"
   "   for animations to/from the integer flags was bad in the case\n"
   "   where an aGif filter doesn't exist.\n"
   "* Fixed bug in thd_niftiwrite.c -- 'if( nparam = 3)' was amended\n"
   "   to '=='.\n"
   "* Modified thd_niftiwrite.c to allow output of func bucket as\n"
   "   the 'u' dimension.\n"
   },

  { 13,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added one to the count of the basis function -iresp and -sresp\n"
   "   counts, to ensure getting the last point!\n"
   },

  { 15,APR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add -automask option in 3dDeconvolve.c.\n"
   "* Add mri_write_jpg() to mri_write.c.  Now mri_write() automatically\n"
   "   writes a .jpg file for RGB and BYTE images if the filename ends\n"
   "   in '.jpg'.\n"
   },

  { 18,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_write.c functions to write to stdout if the output\n"
   "   filename is the string '-'.  This mod affects mri_write_pnm(),\n"
   "   mri_write(), and mri_write_ascii().  Indirectly affects program\n"
   "   imupsam.c.\n"
   },

  { 19,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified niml/niml_element.c to add NI_alter_veclen() function, and\n"
   "   NI_insert_string() function.\n"
   "* Modified plug_tag.c and a couple others to allow func datasets as\n"
   "   inputs, as well as anats.\n"
   },

  { 20,APR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to let 0 values in the overlay image get color\n"
   "   if environment variable AFNI_OVERLAY_ZERO is set to YES.\n"
   },

  { 21,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Where AFNI_COLORSCALE_0x is allowed in pbar.c, also check for\n"
   "   AFNI_COLORSCALE_x and AFNI_COLORSCALE_Ox, to allow for stupid users.\n"
   "   Similar changes in a few other places, as well.\n"
   "* Modified niml/niml_rowtype.c and niml_stream.c to re-enable input\n"
   "   of Base64-encoded data elements (capability had been lost with\n"
   "   the advent of rowtypes and var-dim arrays).\n"
   },

  { 25,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor changes to NIML.\n"
   "* Gamma variation allowed for RGB display in imseq.c.\n"
   },

  { 26,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified imseq.c to apply 0D and 2D transforms to RGB images (that is,\n"
   "   in the intensity channel).  And afni_func.c to apply to RGB overlays.\n"
   },

  { 27,APR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c to allow -stim_times to be used with -nodata.\n"
   "   To do this, you use '-nodata NT TR' to specify the number of time\n"
   "   points and their TR.\n"
   "* Modified imseq.c to make up/down movement of stroking affect RGB,\n"
   "   additively, much as left/right does multiplicatively.\n"
   },

  { 28,APR,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve to add -basis_normall option, and a couple of\n"
   "   other minor tweaks.\n"
   },

  { 29,APR,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Moved extras printout in 3dDeconvolve -nodata to be optional with\n"
   "   an environment variable.\n"
   "* Fixed bug in range locking in afni_lock.c -- if range is locked but\n"
   "   only one controller open, couldn't turn autoRange on.\n"
   },

  { 2,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added plot of least squares overlay line to the Scatterplot plugin.\n"
   "* 3drotate.c now records the applied matvec into the AFNI header attributes.\n"
   },

  { 3,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* More changes to 3drotate.c along the same line.\n"
   "* Edit afni.c to set width of bucket label AVs (in Define Overlay) based on\n"
   "   max width of input sub-brick labels, for each dataset separately.\n"
   },

  { 4,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Oops.  Fix atexit() bug in niml/niml_stream.c -- had logic inverted on\n"
   "   when to remove a stream from the 'open list'.\n"
   },

  { 6,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* non-AFNI datasets (e.g., .nii files) now have ID code set via hashing\n"
   "   their realpath, rather than a random ID code.\n"
   "* Modify EDIT_dset_items() to change the brick_name correctly when the\n"
   "   new prefix ends in '.nii' or '.nii.gz'.\n"
   "* Modify a bunch of programs that print 'Writing dataset' messages to\n"
   "   always use the DSET_BRIKNAME() macro, for consistency.\n"
   },

  { 9,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify plug_nudge.c to deal with RGB-valued datasets.\n"
   },

  { 10,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify thd_initdblk.c, thd_niftiread.c, thd_niftiwrite.c to store AFNI\n"
   "   header data in a NIfTI-1.1 extension, making it more feasible to use\n"
   "   .nii datasets in AFNI as a primary storage mechanism.\n"
   },

  { 11,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify the NIfTI-1.1 extension stuff to be more robust.  Also add\n"
   "   environment variable AFNI_NIFTI_NOEXT to turn extensions off, and\n"
   "   use this to provide a '-pure' option to 3dAFNItoNIFTI.c.\n"
   },

  { 12,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'NIfTI_num' signature to thd_niftiwrite.c and thd_niftiread.c.\n"
   },

  { 13,MAY,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'xamplitude' option to waver -tstim option.\n"
   "* Oops.  Fixed bug in 3dDeconvolve for -iresp/-sresp options: malloc-ed\n"
   "   the 'hout' float** array with sizeof(float) not sizeof(float*), which\n"
   "   doesn't work too good on 64 bit systems.\n"
   "* Modified 1dplot.c to\n"
   "   (a) allow reading up to 10000 numbers on a line from stdin, and\n"
   "   (b) transpose input file if it has only 1 line, so that a long\n"
   "       single line of numbers becomes a decent plot.\n"
   },

  { 16,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Some minor changes to help AFNI compile on Tiger, as reported by Frank\n"
   "   Haist of UCSD.\n"
   },

  { 17,MAY,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Draw ROI plugin' menu item to image viewer popup, in afni.[ch].\n"
   },

  { 18,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add '-dicom' and '-spm' options to 3dcalc.c.\n"
   "* Add a couple of warnings for stupid new users.\n"
   "* Fix parser.f so that acos(1) and asin(1) work (tested .LT., not .LE.).\n"
   },

  { 23,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add checks for data axes mismatch when inputting multiple datasets to\n"
   "   3dcalc.c, 3dTcat.c, 3dbucket.c, and thd_opentcat.c.  Because of\n"
   "   stoopid lusers.\n"
   "* Add 'Ignore xxx' label to bottom of afni_graph.c window, for stupid\n"
   "   users like me.\n"
   },

  { 24,MAY,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add 'i' and 'I' keystrokes to afni_graph.c, to move init_ignore down/up.\n"
   },

  { 31,MAY,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified various things (like thd_nimlatr.c) to change names of some\n"
   "   NIML attributes, in concordance with the treaty reached today with\n"
   "   Ziad Saad.\n"
   },

  { 1,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifications to thd_atr.c, etc., to allow .HEAD files to be stored\n"
   "   using XML.\n"
   "* Warn user when ~/.afni.log file goes over 100 MB (the Kevin Murphy bug).\n"
   },

  { 2,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify thd_nimlatr.c to split large String attributes into multiple\n"
   "   substrings when using NIML, so as to avoid buffer size problems.\n"
   "* Fixed bug in niml/niml_stat.c decoding of strings such as 'Ftest(3,7)'\n"
   "   (forgot to skip the comma!).\n"
   "* Remove AFNI_niml_atexit() from afni_niml.c since it is now redundant\n"
   "   with the atexit stuff in niml/niml_stream.c (oopsie).\n"
   },

  { 3,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in 3dAFNIto3D; if input is a .1D file and no -prefix was given,\n"
   "   the output file would overwrite the input!\n"
   "* Modified the .3D I/O functions to\n"
   "   (a) allow binary format for the data;\n"
   "   (b) allow a time step to indicate 3D+time status;\n"
   "   (c) if the output prefix ends in '.3D', automatically write this format.\n"
   "   Binary vs text format is setup by environment variable AFNI_3D_BINARY.\n"
   "   prefix\n"
   "* Moved AFNI_setenv() function to afni_environ.c for librariness.\n"
   },

  { 6,JUN,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Brodmann Areas' image popup to the 'Hidden' popup menu.\n"
   },

  { 8,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* A number of small changes to get AFNI to compile on Tiger.\n"
   },

  { 9,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Warnings when adwarp.c or afni_func.c is about to write a .BRIK file\n"
   "   over 500 MB in size.\n"
   },

  { 10,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to niml/niml_stream.c to make atexit() stuff work properly\n"
   "   when NI_stream_reopen() is used (this is Ziad's fault, of course).\n"
   },

  { 17,JUN,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add overlay labels (accum_lab stuff) to plug_crender.c.  Added by the\n"
   "   right-click popup on the 'Accumulate' label.\n"
   },

  { 5,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dAFNItoANALYZE.c to scale TR by 0.001 if units are msec.\n"
   "* Modified thd_writeatr.c and thd_writedset.c to correctly write NIML-style\n"
   "   .HEAD file from to3d.c (the blk->parent pointer wasn't set correctly).\n"
   },

  { 7,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dWarpDrive.c to save affine matrix to output file header attributes\n"
   "   (named WARPDRIVE_MATVEC_*), and also to base the coordinates on the actual\n"
   "   dataset rather than the center of the volume.\n"
   "* Modified 3dWarp.c to read matrix from header attribute WARPDRIVE_MATVEC_*.\n"
   },

  { 8,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed Makefile.macosx_10.? to suppress multiply-defined symbol errors.\n"
   "* Modified 3drefit.c and 3dcopy.c to add '-denote' option, to remove notes\n"
   "   and other potentially identifying attributes.\n"
   },

  { 12,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify thd_1Ddset.c so that a 1D filename ending in a ' character will be\n"
   "   transposed on input (as a dataset), so that columnar datasets can be\n"
   "   treated as time, without using 1dtranspose and a temporary file.  Also,\n"
   "   if prefix starts with '-' character, will write .1D datasets to stdout.\n"
   },

  { 13,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify afni_widg.c and afni.c so that a Button-3 click on an Image or\n"
   "   Graph button will recall the open window from offscreen purgatory.\n"
   },

  { 19,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* 3dWarpdrive '-bilinear_general' now works.  Usefulness is another question.\n"
   "* Fixed 'EMPTY IMAGE' display problem in afni_warp.c -- DSET_INMEMORY()\n"
   "   macro in 3ddata.h needed to check for STORAGE_UNDEFINED.\n"
   },

  { 25,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program im2niml.c, and '-p' option to aiv.c.\n"
   },

  { 27,JUL,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Removed -ffast-math from Makefile.linux_gcc32, since it seems to cause\n"
   "   problems in eis_svd.c (at the least).\n"
   },

  { 28,JUL,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added stuff to afni_driver.c (SAVE_JPEG, SET_DICOM_XYZ, SET_SPM_XYZ,\n"
   "   SET_IJK, SET_XHAIRS), and imseq.[ch].\n"
   },

  { 29,JUL,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added '-com' option to afni.c.\n"
   "* Added grapher windows to SAVE_JPEG in afni_driver.c.\n"
   },

  { 3,AUG,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dWarp.c to save WARPDRIVE_MATVEC_ attributes into the header\n"
   "   of the output, assuming that they were used (with '-matparent') on input.\n"
   "* Modified 3drefit.c to add '-atrcopy' and '-atrstring' options to copy and\n"
   "   set attributes.\n"
   },

  { 8,AUG,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'AFNI Version Check!' logo to afni.c & afni_widg.c in case user is\n"
   "   out of date.\n"
   },

  { 10,AUG,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified cat_matvec.c to allow ::WARP_DATA input, and MATRIX() output.\n"
   },

  { 12,AUG,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Start editing 3dDeconvolve.c to have -slice_base option, for slice-dependent\n"
   "   baseline regressors. [never finished]\n"
   },

  { 15,AUG,2005 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* From now on, unless AFNI_ALLOW_MILLISECONDS is set to YES, AFNI programs will\n"
   "   convert MSEC time unit datasets to SEC on input and output.\n"
   },

  { 22,AUG,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dLocalstat.\n"
   "* In imseq.c: 'm' (toggle Min-to-Max), 'a' (fix aspect), 'l' (LR mirror).\n"
   "* '%s' in thd_compress.h means that you can now read .gz files with spaces\n"
   "   in their names.\n"
   },

  { 23,AUG,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* -FILE option for waver.c.\n"
   "* In imseq.c: 's' (sharpen), 'D', 'M', 'S' (Disp, Mont, Save).\n"
   },

  { 24,AUG,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Ugghh.  More SVD trouble.\n"
   },

  { 26,AUG,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* THD_check_AFNI_version() function in thd_vcheck.c.  Use this in a few\n"
   "   popular '3d' programs.\n"
   },

  { 1,SEP,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed 3drotate.c so that '-rotate 0 0 0' doesn't fail.\n"
   },

  { 21,SEP,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_writedset.c and 3dAFNItoNIFTI.c so that if AFNI_AUTOGZIP is\n"
   "   YES, then .nii.gz files will be written instead of .nii files.\n"
   },

  { 28,SEP,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to 3dWarpDrive.c to summarize results (-summ) and to prevent steps\n"
   "   that make the RMS error much worse.\n"
   },

  { 30,SEP,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* 2x2 and 3x3 special cases for cs_symeig.c.\n"
   },

  { 4,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_blur.c to do small Gaussian blurs in real-space, with the\n"
   "   fir_blur?() functions.  Also added FIR_blur_volume() function to allow\n"
   "   user to invoke FIR explicitly, rather than implicitly.\n"
   "* Add 'TRACE' command to afni_driver.c.\n"
   },

  { 6,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified edt_blur.c to skip min/max clip calculations when all directions\n"
   "   are done with FIR.\n"
   "* Modified thd_dsetatr.c to write BRICK_STATSYM attribute not just for\n"
   "   func bucket datasets but for fitt, fift (etc.) legacy types.\n"
   "* Fixed bug in 3dbucfim.c where the stat_aux array was possibly loaded\n"
   "   with illegal array access values.\n"
   },

  { 11,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_niftiwrite.c and 3dAFNItoNIFTI.c so that specifying a .hdr\n"
   "   output filename means you get a .hdr/.img NIfTI-1.1 file pair.  Note\n"
   "   that thd_analyzeread.c and thd_niftiread.c already allow for .hdr/.img\n"
   "   NIfTI-1.1 file pair inputs.\n"
   },

  { 18,OCT,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added -usetemp option to 3dcalc.c.\n"
   "* Added some timing delays to popup/popdowns.\n"
   },

  { 21,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dToutcount.c to check for float_scan type errors.\n"
   },

  { 24,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c to use mmap() with MAP_ANON instead of shmem\n"
   "   for inter-process storage.  Can go beyond 2 GB barrier this way,\n"
   "   on 64-bit compiles.\n"
   },

  { 25,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Makefiles for macosx_10.4_G5 and solaris29_suncc_64.\n"
   },

  { 26,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Mod to symeig_3 (in cs_symeig.c) to avoid problems that are invisible.\n"
   "* Mod to plot_x11.c (in coxplot/) and 1dgrayplot.c, to plot correctly.\n"
   },

  { 31,OCT,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Small changes to imseq.c, to display 'Min2Max' and 's=%d' modes, and\n"
   "   to update winfo label when Keypress 'l' is used.\n"
   },

  { 2,NOV,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Another small change to imseq.[ch] to the winfo label.\n"
   },

  { 8,NOV,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* rint(x) -> rint(x+.00001) in edt_coerce.c, to avoid an artifact in\n"
   "   3dMean.c when the number of averages is a small even integer.\n"
   },

  { 14,NOV,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to afni_splash.c to try to avoid the 'Mr Freeze' bug (e.g.,\n"
   "   unrealize image viewer rather than destroy it).\n"
   },

  { 18,NOV,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* AUTHOR AUTHOR.\n"
   },

  { 22,NOV,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* [l] in imseq.c.\n"
   },

  { 29,NOV,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Message of the Day (motd) stuff.\n"
   },

  { 30,NOV,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Change sub-bricks stuff in afni_driver.c.\n"
   },

  { 1,DEC,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* -coarserot in 3dvolreg.c\n"
   },

  { 6,DEC,2005 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* -coarserot in 3dWarpDrive.c, too.\n"
   },

  { 21,DEC,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* afni_broutext.h\n"
   },

  { 28,DEC,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* fixes to mri_warp3D_align.c and thd_automask.c to deal with problems\n"
   "   with -parfix, and with doing mask stuff on 2D images.\n"
   },

  { 30,DEC,2005 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Web browser stuff in afni.h, afni_func.c, afni_widg.c.\n"
   },

  { 9,JAN,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* mri_warp3D_align.c now will revert to 'best' fit if final fit is\n"
   "   much worse in RMS terms.\n"
   },

  { 8,MAR,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Bug fix in afni_version.c for MOTD failure when network down.\n"
   "* Modify PERROR() macro in thd_iochan.c to NOT print so many duplicate\n"
   "   messages.\n"
   "* Modified afni_environ.c so that setting AFNI_ALWAYS_LOCK immediately\n"
   "   changes the lock situation.\n"
   },

  { 9,MAR,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* A little more dynamical action when a few environment variables are\n"
   "   changed via external scripts.\n"
   "* WAV_duration in waver.c was an int, not a double (thanks, Rasmus!).\n"
   },

  { 10,MAR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Mods to 3dcalc.c to allow use of complex-valued dataset on input.\n"
   },

  { 13,MAR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Checks in afni_func.c, adwarp.c, and thd_writedblk.c for free disk\n"
   "   space, just before actual writing of .BRIK file.\n"
   "* 3dTwotoComplex.c from 3dThreetoRGB.c\n"
   },

  { 20,MAR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dAutomask.c and thd_automask.c to add new -clfrac option.\n"
   "* Modify matrix_f.c to use Solaris BLAS in single precision.\n"
   },

  { 24,MAR,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify thd_niftiread.c to prioritize sform over qform (to match\n"
   "   other packages), but to allow user to set environment variable\n"
   "   NIFTI_FORM_PRIORITY to 'Q' or 'S' to signify the priority.\n"
   "* Also, if both qform and sform are present, check to see if they\n"
   "   have the same handedness -- if not, print a warning message.\n"
   },

  { 25,MAR,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify matrix.c and matrix_f.c to unroll matrix-vector multiply by\n"
   "   four rather than two, after profiling with Shark on the MacIntel\n"
   "   (which showed that loop overhead was a significant factor).\n"
   },

  { 28,MAR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dDeconvolve.c to set ival=-1 on various inputs, before\n"
   "   sscanf()-ing it, so that bad values give error messages.\n"
   "* Also add -x1D option to 3dDeconvolve.c, to save X matrix to a .1D file.\n"
   },

  { 29,MAR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* First version of 3dInvFMRI.c.\n"
   },

  { 31,MAR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* to3d.c: if first filename is 'something.img', check if 'something.hdr'\n"
   "   exists, and if so, suggest that the .hdr file is more likely to be\n"
   "   what they want to input.\n"
   "* Added '-method' and smoothing options to 3dInvFMRI.c.\n"
   },

  { 4,APR,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to 3dInvFMRI.c: -mapwt, better help, God knows what else.\n"
   },

  { 5,APR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add -frugal option to 3dZcat.c.\n"
   },

  { 10,APR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add -polort option to 3dDetrend.c.\n"
   },

  { 13,APR,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* mri_matrix_evalrpn() ==> 1dmatcalc.c and 3dmatcalc.c.\n"
   "* Modify mri_fromstring.c to allow 1D:... generation of multiple columns.\n"
   },

  { 24,APR,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add -sum option to 3dTstat.c.\n"
   },

  { 1,JUN,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New AFNI splash photograph!\n"
   "* imseq.c now doesn't append '.jpg' if Save filename already ends in it.\n"
   },

  { 19,JUN,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed flip_memplot() error in coxplot/plot_cox.c, where non-line elements\n"
   "   were being flipped when they shouldn't be.\n"
   },

  { 2,JUL,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Inserted Powell's NEWUOA code into AFNI libmri.a.\n"
   },

  { 5,JUL,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_read_dicom.c to deal with big-endian file transfer syntax.\n"
   "* Also check for overflow in 16-bit unsigned integer DICOM images.\n"
   },

  { 17,JUL,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added options -keepcen and -xyzscale to 3drefit.c.\n"
   },

  { 18,JUL,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed -xyzsave option in 3drefit.c to make multiple datasets match.\n"
   },

  { 21,JUL,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* NEWUOA stuff into 3dNLfim.c and simplex.c.\n"
   },

  { 24,JUL,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* model_linplusort.c (Linear+Ort) for 3dNLfim.c.\n"
   },

  { 4,AUG,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add max displacement to 3dvolreg.c.\n"
   },

  { 14,AUG,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add pairmax() to parser.f.\n"
   },

  { 22,AUG,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add WARPDRIVE_ROTMAT_* attribute outputs to 3dWarpDrive.c.\n"
   },

  { 5,SEP,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Mod mri_read_1D() so that ending filename in ' character causes\n"
   "   transposition.  Removed this feature from thd_1Ddset.c to match.\n"
   "* Changes to AFNI to allow on-the-fly cluster editing.\n"
   },

  { 6,SEP,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to let Ziad/SUMA initialize colors for surfaces.\n"
   "* Check input datasets for the Mike Beauchamp syndrome.\n"
   },

  { 8,SEP,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Cosmetic changes to AlphaSim to make '-quiet -quiet' turn off all\n"
   "   stdout printing except numerical table at end.\n"
   },

  { 21,SEP,2006 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* Put 3dAllineate into the distribution.  But is not finished.\n"
   "* Modified plug_nlfit.c to allow AFNI_NLFIM_METHOD to select optimizer.\n"
   },

  { 27,SEP,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified correlation ratio in thd_correlate.c to use both Var(y|x) and\n"
   "   Var(x|y) for symmetry between base and target.  Seems to make\n"
   "   registration work better.\n"
   "* Added -check and -master options to 3dAllineate.c.\n"
   },

#if 0
  { 28,SEP,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* End of Res Publica Americana.\n"
   },
#endif

  { 29,SEP,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* 3dAllineate.c edited to make -linear the default interpolation during\n"
   "   alignment process, and -cubic for the output dataset.\n"
   },

  { 10,OCT,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dClipLevel.c to allow float input datasets.\n"
   "* Modified 3drefit.c to make '-TR' option add a timeaxis to a non-3D+time\n"
   "   dataset.\n"
   "* More changes to 3dAllineate.c.\n"
   "* Program 3dAcost.c to compute 3dAllineate costs on 2 bricks [now gone].\n"
   },

  { 18,OCT,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Ugh.\n"
   "* Modified afni_warp.c to only use warp_parent if the dataset being\n"
   "   sliced is an AFNI dataset (not NIfTI, MINC, etc.).\n"
   "* Modified afni_func.c to print warning when forced view change\n"
   "   when switching datasets or sessions - for Adam Thomas.\n"
   },

  { 24,OCT,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dAutomask.c (etc.) to add -peel and -nbhr options, with\n"
   "   also a fix to stupid error made a couple days before.\n"
   },

  { 30,OCT,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified plug_scatplot.c to copy 'Aboot' option from plug_histog.c.\n"
   "* Modified THD_pearson() stuff in thd_correlate.c to remove mean.\n"
   "* New program 3dLocalBistat.c -- joint local statistics between 2\n"
   "   datasets.\n"
   },

  { 31,OCT,2006 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dFWHMx -- does all sub-bricks.\n"
   },

  { 9,NOV,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dFWHM to do what 3dFWHMx does -- not use a voxel in a difference\n"
   "   unless it is in the mask as well.\n"
   },

  { 15,NOV,2006 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dBlurToFWHM.c.\n"
   },

  { 20,NOV,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Multitudinous changes to 3dBlurToFWHM.c.\n"
   },

  { 6,DEC,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added ISQ_snap_agif() and ISQ_snap_mpeg().\n"
   },

  { 7,DEC,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added movie saving commands to afni_driver.c.\n"
   "* Modified 3dFWHMx.c -geom option to add up logs rather than multiply up\n"
   "   results -- for large numbers of sub-bricks, can get overflow the old way.\n"
   "* Added THD_medmad_bricks() and THD_meansigma_bricks() functions to\n"
   "   thd_median.c -- get location and dispersion statistics at same time,\n"
   "   for speed.\n"
   },

  { 8,DEC,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added -dem option to 1ddot.c.\n"
   },

  { 11,DEC,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* More changes to 3dBlurToFWHM.c -- de-median and de-MAD before blurring and\n"
   "   before estimating blur, to be somewhat more self-consistent.\n"
   "* Added SAVE_PNG to the roster of AFNI driver commands.\n"
   },

  { 14,DEC,2006 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added SAVE_FILTERED to the roster of AFNI driver commands.\n"
   },

  { 15,DEC,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made SAVE_FILTERED work with graph windows -- by modifying mri_write_pnm()\n"
   "   to write through a filter if the output filename starts with '|'.\n"
   },

  { 19,DEC,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New constrained optimizer in powell_int.c.\n"
   },

  { 20,DEC,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* mri_purge.c for purging/unpurging MRI_IMAGEs to disk files.\n"
   "* modify thd_cliplevel.c to do float->int conversion inline, rather\n"
   "   than through a temp image (saves on memory).\n"
   "* modify mrilib.h to change MRI_BYTE_PTR() (etc.) macro to use\n"
   "   mri_data_pointer() function, which will invoke mri_unpurge()\n"
   "   if necessary.  This also entailed changing a lot of functions\n"
   "   to avoid using the im.*_data pointers, which I eventually want\n"
   "   to eliminate entirely.\n"
   },

  { 21,DEC,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Made the change to 'void *' -- no more im.short_data (etc.).  Changes\n"
   "   in a bunch more places.\n"
   "* Modified mri_purger.c to delete extant TIM_* files when exit() happens.\n"
   "* When AFNI_IMAGE_GLOBALRANGE is yes, don't do redisplay on isqDR_setrange.\n"
   "   Causes an unpleasant flickering in the image viewer window.  Changes\n"
   "   to afni.c (AFNI_range_setter()), imseq.c, etc.\n"
   },

  { 28,DEC,2006 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify afni_splash.c to save current dataset names and xyz coords in\n"
   "   the startup script.\n"
   "* Modify afni_driver.c to allow multiple 'keypress=' options to OPEN_WINDOW.\n"
   },

  { 5,JAN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify mri_read.c to allow 'ragged' input from '1D:' strings\n"
   "  (e.g., for use with 3dDeconvolve).\n"
   },

  { 10,JAN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified svd_double() in cs_symeig.c to sort singular values and vectors.\n"
   "* Modifed 1dsvd.c to have a '-sort' option.\n"
   },

  { 15,JAN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Added mri_gamma_rgb_inplace() to mri_to_rgb.c.\n"
   "* Modified 1dsvd.c to change Left for Right.\n"
   },

  { 19,JAN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c and mri_fromstring.c to use '|' as a line\n"
   "   separator in 'SYM:' and '1D:' inputs (as well as '\\').\n"
   },

  { 26,JAN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified 3dDeconvolve.c to echo -gltsym files as well as the matrices\n"
   "   they generate.\n"
   },

  { 1,FEB,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified mri_purger.c to use a process-specific 3 code suffix after\n"
   "   TIM_ to make it easier to clean up after a crash when multiple\n"
   "   jobs are running.\n"
   "* Modified mri_genalign.c to patch a memory leak in some floatvec's\n"
   "   not being freed before being reused.\n"
   "* Modified 3dAllineate.c to use mri_purge() on the output dataset's\n"
   "   sub-bricks as they are being created.\n"
   "* Modified thd_writedblk.c to deal with datasets that have mri_purge()-ed\n"
   "   sub-bricks.  Need to do the same for NIfTI someday, I suppose.\n"
   "* New function mri_clear() in mri_free.c, to free an MRI_IMAGE's data array\n"
   "   and get rid of it's purged TIM file, if necessary.\n"
   },

  { 2,FEB,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added a 'Rescan' button next to 'Overlay' and 'Underlay' in the main\n"
   "   AFNI controller.  At the demand of Ziad Saad.\n"
   "* Later: and a 'NIML+PO' button below that.\n"
   },

  { 5,FEB,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add AFNI_OVERLAY_ONTOP environment variable, to move 'Overlay' button\n"
   "   above 'Underlay'.  Also, change bg of 'Underlay' to black, to\n"
   "   distinguish it better from 'Overlay'.\n"
   },

  { 18,FEB,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* When running 'afni' (no directory args) and no data is found is './',\n"
   "   then afni.c will now recurse 1 level on './' to see if any datasets\n"
   "   can be found 1 level deeper.  Inspired by Korea workshop and Hame Park.\n"
   },

  { 20,FEB,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify list choosers in bbox.c to add XmNtraversal = True, so that arrow\n"
   "   keys can be used to move around in lists.\n"
   "* Also add TEAROFFIZE() macro to xutil.h and use it to make most popup or\n"
   "   pulldown menus in AFNI have the 'tear off' feature.\n"
   "* When Datamode->Misc->Purge Memory is used, and mcw_malloc() is turned on,\n"
   "   prints out the before and after usage, just for fun.\n"
   },

  { 21,FEB,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modified Edit Environment pseudo-plugin (afni_pplug_env.c) to sort\n"
   "   controls by variable name.\n"
   "* Added 'instant switch on selection' mode to 'Overlay' and 'Underlay'\n"
   "   dataset choosers, controlled by AFNI_DATASET_BROWSE variable.\n"
   "* And to 'Talairach To' controls.\n"
   "* Fixed bug when '-R' would fail to find any datasets, and would then\n"
   "   try to reconcile parents, look for duplicates, etc., de-referencing\n"
   "   NULL pointers.  Not sure what happened, actually, but one technique\n"
   "   was to avoid trying to read directories as regular file datasets.\n"
   },

  { 22,FEB,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_loaddblk.c to do floatscan on ANALYZE (etc.) datasets.\n"
   "* Also modified thd_floatscan.c to add functions to scan MRI_IMAGEs\n"
   "   and other AFNI-ish assemblies of numbers.\n"
   "* Modified afni.c to parse multiple commands in a single '-com' option,\n"
   "   separated by ';' (or by the choice in '-comsep').\n"
   "* Modified afni_driver.c to allow use of 'axial_image' (etc.) as easy\n"
   "   typos for 'axialimage' (etc.).\n"
   "* Modified dist_help script to include README.* files.\n"
   },

  { 23,FEB,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Change XmNtraversalOn from False to True in about 1 zillion places, to\n"
   "   make keyboard focus be kept better in text widgets.\n"
   "* Added 'dset=NULL' initializer to THD_open_one_dataset() in thd_opendset.c,\n"
   "   per Bernd Feige of Freiburg.\n"
   "* Modified bbox.c to make AFNI_list_doubleclick default be 'Apply' rather\n"
   "   than 'Set'.\n"
   "* Modified afni_func.c to replace 'RescanTh' button with 'EditEnv' if\n"
   "   Ziad's AFNI_RESCAN_AT_SWITCH is turned on.\n"
   "* Modified afni_func.c to do AFNI_RESCAN_AT_SWITCH only for 'Overlay'\n"
   "   and 'Underlay' buttons.  (It's pointless for 'Switch Session'.)\n"
   },

  { 26,FEB,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni_func.c to make the Session selection dialog directory-\n"
   "   specific, and the 1D selection dialog regular-file-specific.  Also\n"
   "   changed the labels on top of the file list for each case.\n"
   "* Modified xutil.c so that MCW_expose_widget() doesn't do anything for\n"
   "   a non-widget (e.g., gadget) input.\n"
   "* Added Ziad's Xt translations for Button4 and Button5 scrolling.\n"
   "* Fixed mri_medianfilter.c: had logic for usedxyz exactly backwards! Oopsie.\n"
   "* Added Button4+5 image window scrolling to imseq.c, and to afni_graph.c\n"
   "* If only dummy dataset is present, then 'Switch Session' opens up the\n"
   "   'Read Session' dialog.\n"
   },

  { 27,FEB,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_ENV , "Older History stuff" ,
   "* AFNI_DISABLE_TEAROFF environment variable.\n"
   },

  { 1,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add dilation to -automask option in 3dAllineate.  And -ignout option.\n"
   "* Add -CENSOR to 3dDeconvolve.c.\n"
   },

  { 2,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified -CENSOR to -CENSORTR (for Rick's sake) and added '*' capability\n"
   "  to the run syntax.\n"
   "* Added 3dDFT.c, from Kevin Murphy.\n"
   },

  { 4,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed overrun bug in THD_extract_array() for raw data -- code was never\n"
   "   tested!  Affected 3dDFT.c.\n"
   },

  { 5,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added -nfft and -detrend options to 3dDFT.\n"
   "* Added 'u' and 'o' image viewer keypress handling to afni.c.\n"
   "* Added Mod+Button4+5 threshold slider changing to imseq.c and afni.c.\n"
   "* Added AFNI_THRESH_AUTO to afni_func.c.\n"
   },

  { 6,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed pairmin() bug in parser.f.\n"
   "* Store column_metadata in 3dDeconvolve.c, and write it to -x1D file\n"
   "   if AFNI_3dDeconvolve_NIML is YES.\n"
   },

  { 7,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* 3dDeconvolve.c: -GOFORIT, check_matrix_condition(), report -stim_times\n"
   "   values outside of run intervals, condition numbers with and without\n"
   "   baseline regressors.\n"
   },

  { 8,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add mri_read_ascii_ragged_complex() to mri_read.c.\n"
   "* And modify mri_write_ascii() in mri_write.c to write complex numbers\n"
   "   with ';' as a real/imag separator, instead of ' '.\n"
   },

  { 9,MAR,2007 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* First test of -stim_times_AM? looks good.\n"
   "* MCW_discard_events() for Button4/5 ScrollWheel actions.\n"
   },

  { 12,MAR,2007 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dSynthesize.c.\n"
   },

  { 13,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Minor changes to 3dSynthesize.\n"
   "* Change PRINT_VERSION() macro in mrilib.h to include compilation __DATE__\n"
   "   in output.\n"
   "* '-float' option to 3dDeconvolve.  Also a few little other fixes.\n"
   },

  { 14,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* New program 1dMarry.c.\n"
   },

  { 15,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added CTENT() to 3dDeconvolve.c, for experimental purposes.\n"
   "* Modified bucket labels in 3dDeconvolve.c to be clearer (to me, anyhoo).\n"
   },

  { 16,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* -polort A and polort degree warning message.\n"
   "* Change CTENT() to CSPLIN().\n"
   },

  { 20,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* MCW_discard_events_all() in xutil.c, and its application to stop\n"
   "   over-scrolling in imseq.c, et cetera.\n"
   "* -nox1D in 3dDeconvolve.c.\n"
   "* Make -bout be always on for 3dDeconvolve -input1D.\n"
   },

  { 21,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dEmpty.c.\n"
   "* Fix 3dinfo.c to allow empty datasets (duh).\n"
   },

  { 22,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Make THD_filesize() return a 'long long', and change mri_read_3D() to\n"
   "   match.  (This is Colm Connolly's fault.)\n"
   },

  { 23,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* New function THD_deconflict_prefix() in thd_opendset.c.\n"
   "* Modify 3dDeconvolve.c to deconflict output prefixes (instead of failing),\n"
   "   and to compute Full F by default, unless -nofullf_atall is given.\n"
   "* AFNI_IMAGRA_CLOSER, for the FC5 abusers out there.\n"
   },

  { 26,MAR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Moved on-the-fly cluster editing to Define Overlay panel, from popup menu.\n"
   },

  { 27,MAR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Help for cluster editing, and some rationalization (e.g., reset it when\n"
   "   changing datasets, turn widgets off if it's not avaiable, et cetera).\n"
   },

  { 3,APR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify edt_blur.c to compute FIR Gaussian weights as a local sum across\n"
   "   each cell, rather than just the weight at the center of the cell.\n"
   },

  { 4,APR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dDespike.c to add -localedit option.\n"
   "* Modify 3dAllineate.c to save matrices into header of output.\n"
   },

  { 26,APR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify imseq.[ch], afni.c, and afni_graph.c to use '[' and ']' keys for\n"
   "   time index incrementing, per John Butman.\n"
   },

  { 27,APR,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified thd_niftiread.c to always do floatscan on datasets.\n"
   "* Modified 3dDeconvolve.c to make 0.0 ABI paired values not require -GOFORIT.\n"
   },

  { 30,APR,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Included 3dinfo.c patch from Colm Connolly with -label2index option.\n"
   },

  { 3,MAY,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'crop=' to afni_driver.c image window opening, and also to afni_splash.c\n"
   "   when saving .afni.startup_script file.  [Per John Butman]\n"
   "* Modified 3dDeconvolve.c to print clearer messages when -gltsym errors\n"
   "   occur, and also to allow the Decon -bucket dataset to be overwritten,\n"
   "   and also to add the -nobucket option.  [Per Mike Beauchamp]\n"
   },

  { 4,MAY,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* 'amongst' function in parser.f and so in 3dcalc.c.\n"
   "* Added more warnings to 3dDeconvolve.c, including a parametrized -GOFORIT.\n"
   },

  { 9,MAY,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* New advanced histogram options '-clbin' and '-eqbin' for 3dAllineate.\n"
   },

  { 10,MAY,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* -izz option to 3dAllineate.\n"
   "* L1 detrending in thd_detrend.c (and retrending).\n"
   },

  { 29,MAY,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Enforce RadioAlwaysOne behaviour on marks toggles.\n"
   "* Modify 3dDeconvolve to do proper kill-off of children when fork() fails.\n"
   "* Fix SPMG model power from 4 to 5.\n"
   "* Modify symeigval_double() to report nonzero error code from rs_().\n"
   },

  { 30,MAY,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dDeconvolve mmap() usage to include MAP_NORESERVE flag.\n"
   "* Add -allzero_OK option to 3dDeconvolve.c.\n"
   "* Make 3dttest check for duplicate dataset filenames.\n"
   },

  { 1,JUN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modifiy mri_dicom_hdr.c to use a larger buffer for RWC_printf() function,\n"
   "   which may help with really big DICOM header files.\n"
   },

  { 4,JUN,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dDeconvolve.c to use .xmat.1D instead of .x1D as output filename\n"
   "   for matrices.\n"
   },

  { 5,JUN,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dBlurToFWHM.c and 3dFWHMx.c to use detrending.\n"
   "* Add THD_patch_dxyz_* functions to thd_auxdata.c, to make sure MRI_IMAGE's\n"
   "   in a dataset have the correct dx,dy,dz fields.\n"
   },

  { 6,JUN,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify thd_writedset.c to always do THD_deconflict_prefix(), unless told\n"
   "   not to by AFNI_DONT_DECONFLICT.  Modify a bunch of 3d programs to NOT\n"
   "   check for filename conflict on output.\n"
   "* Modify thd_correlate.c build_2Dhist() to avoid histogram overflow (oops).\n"
   },

  { 25,JUN,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to 3dDeconvolve and 3dSynthesize so that censored time points\n"
   "   can be reconstructed in some fashion in the latter program.\n"
   "* -x1D_uncensored in 3dDeconvolve.\n"
   },

  { 26,JUN,2007 , RWC , "Miscellaneous" , MAJOR , TYPE_GENERAL , "Older History stuff" ,
   "* Boxed plots in afni_graph.[ch].  Probably a can of worms.\n"
   },

  { 28,JUN,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Threshold on/off in reactivated thr_label popup menu in Define Overlay.\n"
   "* -x1D_stop in 3dDeconvolve.\n"
   },

  { 29,JUN,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Boxed plots work with Dataset#N now, in glorious colors and thinned.\n"
   "   But not with Double Plot.\n"
   },

  { 11,JUL,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor tweaks to algorithm for setting voxel-dependent blur factors in\n"
   "   3dBlurToFWHM.c.\n"
   "* Various #define's for SOLARIS bad functions like fabsf() in 64 bit mode.\n"
   },

  { 16,JUL,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add -stim_times_IM to 3dDeconvolve.c, to get individual event amplitudes.\n"
   },

  { 19,JUL,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add THD_multiplex_dataset() to thd_mastery.c, and thence 3dttest.c.\n"
   "* Modify 3dAllineate to re-use last row of -1Dapply input if needed, and\n"
   "   also to print a warning that -master may be needed with -1Dapply.\n"
   },

  { 25,JUL,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify 3dAllineate, 3dvolreg, and 3dWarpDrive to output base-to-source matrices.\n"
   "* Modify cat_matvec to deal with files of multiple matrices.\n"
   "* Modify 3dAllineate to make '-clbin 0' the default.\n"
   "* Modify afni to add an AutoThreshold button to the threshold popup menu.\n"
   },

  { 27,JUL,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add 'SPMG3' to 3dDeconvolve.c.\n"
   "* Fix bug in -1Dmatrix_save in 3dAllineate, when dealing with zero-padded base.\n"
   },

  { 30,JUL,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify thd_read_vecmat.c to use mri_read_double_ascii() to read a file rather\n"
   "   than read it directly.  This way, comments will be skipped properly and\n"
   "   the '1D: ...' format can be used.\n"
   "* Fix afni_plugin.c to initialize loading of matrix_initialize(), to fix a\n"
   "   problem left in plug_deconvolve.c by RCR's matrix.c changes.\n"
   "* Matrix square root in mri_matrix.c, and thence to 1dmatcalc and cat_matvec.\n"
   },

  { 31,JUL,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Make -cmass the default in 3dAllineate.c.\n"
   "* Also add '+clip' feature to '-autoweight' in 3dAllineate.c.\n"
   },

  { 1,AUG,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify to3d.c to warn users of '-xFOV 120A-P' (e.g.) that this is a 240 mm\n"
   "  field of view (warn in the -help output, and in the program running).\n"
   },

  { 3,AUG,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_ENV , "Older History stuff" ,
   "* AFNI_SEE_OVERLAY environment variable.\n"
   "* Turn overlay on when user first-time switches Overlay, and turn TTatlas\n"
   "   colors on when user pops up atlas color chooser panel.\n"
   },

  { 8,AUG,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Add threshold signage feature to AFNI threshold menu ('Pos & Neg', etc).\n"
   },

  { 14,AUG,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Modify 3dDeconvolve.c to allow 'POLY' model to have order up to 20.\n"
   "* Modify 3dLocalBistat.c to allow '-weight' option for '-pearson' statistic.\n"
   },

  { 16,AUG,2007 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Modify 3dDeconvolve.c to check -stim_times files for 0-1 inputs and for\n"
   "   duplicate times.\n"
   },

  { 20,AUG,2007 , RWC , "Miscellaneous" , MAJOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* First version of '-lpc' Local Pearson Correlation cost function in\n"
   "   3dAllineate.c, with rhombic dodecahedra as the default building bloks.\n"
   },

  { 10,SEP,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Minor changes to 3dAllineate.c: '-autoweight**1.5' sub-option;\n"
   "   '-autoweight' now the default for '-ls' cost function.\n"
   "* Re-insert older fixes to afni.c that were lost in CVS.\n"
   },

  { 11,SEP,2007 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Modified 3dBlurToFWHM.c to remove scale factors from 'outset' if 'inset'\n"
   "   had them.  Oopsie.\n"
   },

  { 12,SEP,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'All AFNI Splashes' button to 'hidden' menu, just for fun.\n"
   },

  { 17,SEP,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'Clipped' option to image grayscaling, in imseq.c and afni.c.\n"
   },

  { 18,SEP,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Added 'RGB:r,g,b' format to DC_parse_color() in display.c.\n"
   "* Fixed some bugs in 'Clipped' display.\n"
   },

  { 20,SEP,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Add '-allcost' options to 3dAllineate.c and mri_genalign.c.\n"
   "* Neuter program 3dAcost.c.\n"
   "* Environment variable AFNI_INDEX_SCROLLREV.\n"
   },

  { 21,SEP,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified MCW_choose_vector() in bbox.c to make initvec a float array,\n"
   "   and then fixed the clusterize and graph pin stuff to match.\n"
   "* Modified thd_info.c to print out a more prominent warning message\n"
   "   from 3dinfo.c when sub-brick stats are hidden from the user.\n"
   "* Warning message popup when Define Markers is first opened on a\n"
   "   marker-less dataset.\n"
   },

  { 4,OCT,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Modify afni_graph.c to turn Double Plot on when Dataset#N is invoked,\n"
   "   and to not double plot transformed time series that didn't change.\n"
   "* Print 'NFirst > 0' informational message in 3dDeconvolve.c.\n"
   "* Change OPACITY_BOT from 0 to 1 in imseq.c.\n"
   },

  { 10,OCT,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to 3dDeconvolve.c to check -stim_label values: for un-assigned\n"
   "   values, and for duplicate values.\n"
   "* Change a few programs to check EQUIV_GRIDS() as well as voxel counts,\n"
   "   when combining multiple datasets (3dTcat, etc).\n"
   "* Modify plug_nth_dataset.c to use different set of default overlay colors.\n"
   "   Modify afni_plugin.[ch] to allow this.\n"
   "* Modify afni_graph.c to make 'Transform 1D' menu re-activate a button even\n"
   "   if it is already the activated one.  Goal: popup Dataset#N plugin\n"
   "   controls more easily.\n"
   "* Modify 3dttest.c to add the -sdn1 option (for Tom Johnstone).\n"
   },

  { 11,OCT,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Modify edt_filtervol.c to clip off voxels outside the fmask.\n"
   "* Modify 3dmerge.c to add -1fm_noclip and -1filter_blur options.\n"
   },

  { 12,OCT,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Changes to bbox.c to add an arrowval index selector to the single-selection\n"
   "   mode strlist chooser (per Shruti Japee's unreasonable demand).\n"
   },

  { 16,OCT,2007 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Small bug in decode_linebuf() in mri_read.c -- if a non-number character\n"
   "   is encountered in non-slowmo mode, infinite loop ensues and that's bad.\n"
   "   Now, such an encounter leads to an immediate break out of the loop.\n"
   "* Small changes to approximate_number_string() and it's usage in 3dDeconvolve.\n"
   "* Fix to afni_graph.c so that 'Boxes' plot mode turns off 'Double Plot'.\n"
   },

  { 24,OCT,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor fix to 3dAllineate.c to setup 'blok' stuff when allcost is on.\n"
   },

  { 25,OCT,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Minor change to RegAna.c to use matrix_psinv() as a backup to\n"
   "   matrix_inverse_dsc() when inverting GLT matrix.\n"
   },

  { 26,OCT,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified ranks.c and 3dMannWhitney.c to create sorted list all at once\n"
   "   (via qsort_float), hopefully speeding things up.  Also increased\n"
   "   default workmem and MAX_OBSERVATIONS.\n"
   "* Same deal for 3dWilcoxon.c.\n"
   },

  { 29,OCT,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Change 'workmem' default to 266 in several of Doug Ward's programs.\n"
   "* Added warning message to 3dDeconvolve if TENT() or CSPLIN() inter-knot\n"
   "   TR is smaller than the output TR for -iresp.\n"
   "* Added message to 3dSynthesize.c to indicate type of -cenfill being done.\n"
   },

  { 5,NOV,2007 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Insert some sleeping to try to avoid X11 errors that have been reported\n"
   "   (but not seen by me) about XGeometry.\n"
   "* If DONT_USE_XTDESTROY is #define-d, then XtDestroyWidget() is replaced by\n"
   "   XtUnrealizeWidget() -- this is used in the Makefile for linux_xorg7,\n"
   "   where the old bug in that Xt library function seems to have risen\n"
   "   from the dead.\n"
   },

  { 9,NOV,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fixed bug in build_2Dhist() in thd_correlate.c equal sized bin code,\n"
   "   where 'ytop-xbot' was used instead of 'ytop-ybot'.  This is obviously\n"
   "   the work of Dutch saboteurs.\n"
   },

  { 13,NOV,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* SAVE_RAW and SAVE_RAWMONT commands in afni_driver.c (and so imseq.[ch]).\n"
   "* Fix error in mri_genalign.c for allcost: the various 'cr?' functions\n"
   "   weren't being properly separated.\n"
   "* Added multiple -check ability to 3dAllineate.c, and -allcost now printed\n"
   "   for each alternative checked parameter set as well.\n"
   },

  { 15,NOV,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to interactive AFNI to save '1D:' ref and ort files in the\n"
   "   header of interactive fim files; example\n"
   "     1dplot '`3dAttribute -ssep ' ' AFNI_FIM_REF r1_time@1+orig`'\n"
   "* Changes to mri_read_1D() to allow arbitrarily long filenames.\n"
   "* New function mri_to1Dstring(), to create '1D:' strings from MRI_IMAGEs.\n"
   },

  { 16,NOV,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Added -global_times, -local_times, and -guess_times to 3dDeconvolve.c\n"
   "* For ragged 1D input files, arbitrary text starting with alphabetic\n"
   "   character is same as '*' for filler.\n"
   "* AFNI_RESCAN_AT_SWITCH is on by default now.\n"
   "* 1dplot takes a 1 row file and flips it to a column for plotting; however,\n"
   "   when 2 single row files were input, they'd be catenated and THEN\n"
   "   flipped, producing erroneous joined plot.  Now, they are flipped\n"
   "   THEN catenated, which does the right thing.\n"
   "* Speaking of 1dplot, '-nopush' now works for multiple graphs, rather\n"
   "   than just for '-one'.\n"
   "* Modify AlphaSim.c so that rmm defaults to -1 ==> nearest neighbors,\n"
   "   and so that '-seed 0' generates a custom seed.\n"
   "* Add some extra text to the 'Clusterize' label (afni_setup.c) to\n"
   "   explain that rmm=0 means NN clustering with vmul in voxel count.\n"
   },

  { 20,NOV,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_PROG, "Older History stuff" ,
   "* Copy auxdata from source to output in 3dAllineate.c (oops).\n"
   "* Changed 'report' from mri_clusterize.c and added info to the\n"
   "   clusterize vector chooser using about BHelp to see this report.\n"
   "* New program 3dTsort.c.  Will this grow to be a monster?\n"
   },

  { 23,NOV,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_PROG, "Older History stuff" ,
   "* Modify NI_decode_one_string() in niml_elemio.c to auto-expand buffer\n"
   "   size if string is too long.  For festering Lebanese programmers.\n"
   "* Similarly, modify scan_for_angles() if element header is bigger than\n"
   "   buffer size.\n"
   "* 'Histogram: Multi' (plug_histog_multi.c) plugin, for Kyle, Alex, & Pat.\n"
   },

  { 4,DEC,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add AFNI_driver_register() to afni_driver.c to allow a plugin (say) to\n"
   "   register a driver callback function.\n"
   },

  { 5,DEC,2007 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify plot_motif.c to allow saving plots (as in 1dplot.c) to .jpg and\n"
   "   .png files (if the proper suffix is given).\n"
   },

  { 6,DEC,2007 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Add -jpg and -png options to 1dplot.c.\n"
   "* Add thick line drawing to mri_coxplot.c (by repeated thin line drawing).\n"
   },

  { 20,DEC,2007 , RWC , "Miscellaneous" , MAJOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Clusterize reporting window.\n"
   },

  { 27,DEC,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Upgrades to clusterize reporting window: histograms, from/to indexes\n"
   "   on auxiliary dataset, 3dclust and save table buttons, &c.\n"
   "* Fixed bug in DG's obliquity reporting function in thd_coords.c -- it\n"
   "   didn't check if the input dataset was valid -- caused AFNI to crash.\n"
   },

  { 28,DEC,2007 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Popup menu in clusterize report to set histogram range.\n"
   },

  { 9,JAN,2008 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* SIGQUIT delay in afni.c, for Jerzy.\n"
   },

  { 11,JAN,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Small changes to AlphaSim.c, like using -fast option and better help.\n"
   "* Scrolling window changes to afni_cluster.c (Clusterize report panel).\n"
   "* mri_alphasim.c seems to work now.\n"
   },

  { 16,JAN,2008 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Fix clusterize so that too small a value of vmul means just set the\n"
   "   min cluster volume to 2 voxels.\n"
   "* Unfixed bug: display of clusterized func+tlrc.BRIK when func+tlrc dataset\n"
   "   is warped from func+orig -- must fix afni_warp.c in this case.\n"
   },

  { 17,JAN,2008 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* AFNI_FLOATIZE environment variable for 3dDeconvolve.c and 3dcalc.c.\n"
   "* mri_fdrize.c function for FDR-z conversion.\n"
   },

  { 18,JAN,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Incorporate mri_fdrize() into 3dFDR.\n"
   },

  { 22,JAN,2008 , RWC , "Miscellaneous" , MINOR , TYPE_GENERAL , "Older History stuff" ,
   "* Minor changes to 3dFDR: better help, -float, -qval.\n"
   "* Add -sort and -SORT options to 3dcalc, for no good reason.\n"
   "* Add 'help' command to ccalc, to give parser info.\n"
   },

  { 23,JAN,2008 , RWC , "Miscellaneous" , MAJOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* FDR thresh-vs-z(q) curve generation and storage in datasets.\n"
   "* Display FDR q in AFNI pval label.\n"
   "* Generate FDR curves automatically in 3dDeconvolve, and with\n"
   "   '-addFDR' option in 3drefit.\n"
   },

  { 24,JAN,2008 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Add FDR curve generation to 3dANOVA.lib, 3dNLfim, 3dRegana, 3dttest.\n"
   "* Fix little problems with AFNI threshold scale display as q-value alters.\n"
   "* Fix bug in thd_mastery.c in assiging FDR curves to mastered sub-bricks.\n"
   "* Add '-killSTAT' option to 3drefit.\n"
   },

  { 25,JAN,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* mri_read_1D_stdin() in mri_read.c.\n"
   },

  { 28,JAN,2008 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Added PRINT_COMPILE_DATE macro to mrilib.h and to a lot of programs'\n"
   "   -help output.\n"
   "* Fixed 'rule of 15' bug in afni_cluster.c, where the initial display only\n"
   "   showed a max of 15 widget rows, even if there were more clusters, but\n"
   "   the number of rows would be reset correctly on any re-clusterizing.\n"
   "   Problem: didn't initialize maxclu_default at the right location.\n"
   "* Fixed problem with display of clusterized dataset which is both the\n"
   "   Underlay and Overlay: the Underlay was being shown as the edited\n"
   "   volume, but that looks real bad.  Solution: disable vedit feature\n"
   "   in AFNI_dataset_slice() when calling from the 'get anat image' place\n"
   "   in afni.c, and then re-enable it right afterwards.\n"
   },

  { 29,JAN,2008 , RWC , "Miscellaneous" , MICRO , TYPE_BUG_FIX , "Older History stuff" ,
   "* Fixed bug in 3dFDR -new handling of mask -- ooooppssssie.\n"
   "* And bug in FDR curves generated from signed statistics (like t).\n"
   },

  { 31,JAN,2008 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_OPT , "Older History stuff" ,
   "* Modify plug_crender.c to obey AFNI_SLAVE_THRTIME.\n"
   },

  { 1,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* VOLUME_DATA_SPARSE in thd_nimlatr.c.\n"
   "* READ_NIML_FILE in afni_driver.c.\n"
   "* Force re-clustering when doing a redraw in the ROI plugin.\n"
   "* Modify NIML+PO button label to reflect status of NIML and plugouts\n"
   "   at time button is created.\n"
   },

  { 4,FEB,2008 , RWC , "Miscellaneous" , MINOR , TYPE_NEW_ENV , "Older History stuff" ,
   "* AFNI_NEWSESSION_SWITCH in afni_func.c == switch to new session?\n"
   },

  { 5,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Fix over-write checks in mri_write.c by moving all file open/close\n"
   "   operations to new fopen_maybe() and fclose_maybe() functions.\n"
   "* Move THD_deathcon() and THD_ok_overwrite() functions to afni_environ.c,\n"
   "   where they belong.\n"
   "* New function EDIT_geometry_constructor() to make an empty dataset from\n"
   "   a string specifying it's geometry.  Also, print this geometry string\n"
   "   in 3dinfo.\n"
   },

  { 7,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* DSET_overwrite() macro, and THD_force_ok_overwrite() function.\n"
   "* Modify plug_drawdset.c to use DSET_overwrite(), and a few other\n"
   "   plugins and 3d*.c programs as well.\n"
   "* Fix drawing of surface overlay when the functional overlay is being\n"
   "   shown as the underlay: must use the underlay dataset for coordinate\n"
   "   checking even if overlay dataset is actually being drawn as underlay,\n"
   "   since the coordinates for the images are still the underlay's in\n"
   "   this situation.\n"
   "* Fix 3dvolreg to work properly with sub-brick scale factors.\n"
   },

  { 11,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modified afni.c and thd_mastery.c to work better with '3dcalc' command\n"
   "   line datasets.\n"
   },

  { 13,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Modify mcw_glob.c and thd_mastery.c to pre-expand '~/' at the start of\n"
   "   a filename to '${HOME}/' to help some pitiful users (e.g., me).\n"
   "* Modify afni.c to turn off clusterizing when switching coordinate systems.\n"
   },

  { 14,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Similar changes to de-clusterize when a forced view switch occurs when\n"
   "   switching datasets, or when switching datasets to something that\n"
   "   doesn't have data available.\n"
   "* Also, AFNI_FLASH_VIEWSWITCH to disable Adam Thomas's view switching\n"
   "   flashiness.\n"
   },

  { 16,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* New program 3dTfitter.  And fitting function THD_fitter().\n"
   },

  { 19,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Bug fixes in 3dTfitter.c and thd_fitter.c.\n"
   },

  { 20,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Add cl2.c constrained least squares function to the library.\n"
   "* Use this in thd_fitter.c and thence in 3dTfitter.c.\n"
   "* Add '-1D:' option to 1deval.c.\n"
   },

  { 22,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Changes to 3dpc.c: -reduce and -eigonly options.\n"
   },

  { 25,FEB,2008 , RWC , "Miscellaneous" , MICRO , TYPE_GENERAL , "Older History stuff" ,
   "* Make 3dTfitter work when -RHS is a '1D:' input (-LHS already worked).\n"
   "* -vnorm and -vmean options for 1dsvd.c (as in 3dpc.c).\n"
   },

/*=====ABOVE THIS LINE=====*/
 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end */
} ;
