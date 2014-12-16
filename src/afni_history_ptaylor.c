
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
