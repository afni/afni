
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
