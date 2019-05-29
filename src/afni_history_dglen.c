
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

           - PLEASE, stick to what fits on an 80 column terminal
           - it may be nice to put the newest entires at the top
           - leave the last "99, NULL" entry as it is

 -- examples (newest at top) --
 
 { 30 , MAR , 2008 , DRG , "oblique_datasets" , MAJOR , TYPE_MODIFY ,
   "no longer create the TO_DICOM_REAL xform from current axes" ,
   NULL
 } ,

 { 19 , FEB , 2008 , DRG , "my_program" , SUPER , TYPE_NEW_PROG ,
   "short description of change" ,
   "(optional) description of where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/


afni_history_struct dglen_history[] = {
{ 29, MAY, 2019, DRG, "@auto_tlrc", MINOR, TYPE_MODIFY,
    "auto_tlrc changes",
    "NIFTI in and out fixes, prefix option, 3dAllineate option\n"
    "Default interpolation changes, one pass combination default too"
},
{ 13, MAY, 2019, DRG, "3dExchange", MINOR, TYPE_NEW_PROG,
    "Exchange specified values in a dataset",
    "Takes two columns of numbers to map input to output"
},
{ 18, APR, 2019, DRG, "whereami GUI with wrong atlas", MICRO, TYPE_BUG_FIX,
    "whereami would crash afni GUI if atlas was not found",
    "Also more descriptive message when no available atlases"
},		
{ 17, APR, 2019, DRG, "whereami minimum space", MICRO, TYPE_MODIFY,
    "Should have at least the space of the dataset coordinate"
},	
{ 12, APR, 2019, DRG, "unWarpEPI.py python3", MINOR, TYPE_MODIFY,
    "unWarpEPI.py python3 compatible"
},	
{ 12, APR, 2019, DRG, "unWarpEPI.py python3", MINOR, TYPE_MODIFY,
    "unWarpEPI.py python3 compatible"
},
{ 11, APR, 2019, DRG, "whereami", MAJOR, TYPE_NEW_ENV,
    "Brainnetome atlas",
    "This atlas is now included in the standard distribution\n"
    "This also is in the default list of atlases"
},
{ 7, APR, 2019, DRG, "whereami", MAJOR, TYPE_NEW_ENV,
    "Eickhoff-Zilles 2.2 MPM atlas",
    "This atlas is the newer 2.2 version of the MPM atlas\n"
    "This atlas replaced the 1.8 version in the list, but both\n"
    "are kept in the binary distribution"
},
{ 5, APR, 2019, DRG, "whereami", MAJOR, TYPE_NEW_ENV,
    "HCP Glasser atlas in AFNI atlases",
    "Mike Beauchamp and Meghan Robinson contributed atlas\n"
    "Now included and first in default list too\n"
    "This atlas is in MNI space (not in its original Contee\n"
    "grayordinate surface space.\n"
    "Additionally, all the pmaps (probability maps) have been\n"
    "removed from the standard distribution and default list"
},
{ 12, OCT, 2018, DRG, "auto_warp.py", MICRO, TYPE_NEW_OPT,
    "at_opts for @auto_tlrc options"
},
{ 12, OCT, 2018, DRG, "whereami", MINOR, TYPE_NEW_ENV,
    "Longname support in AFNI atlases",
    "Environment variable AFNI_ATLAS_NAME_TYPE to control"
},
{ 29, APR, 2018, DRG, "First space xform bug",
	MICRO, TYPE_BUG_FIX,
	"Fixed bug where first space in AFNI_atlas_spaces.niml had to have xform "
},
{ 26, APR, 2018, DRG, "ORIG space",
	MINOR, TYPE_GENERAL,
	"ORIG space formally defined in AFNI_atlas_spaces.niml",
    "Should allow for FreeSurfer segmentation to be used\n"
    "with the SessionAtlases.niml created by @SUMA_MakeSpecFS\n"
    "This change currently applies only to the whereami command\n"
    "line. The afni GUI still requires a +tlrc or equivalent view."
},
{ 26, APR, 2018, DRG, "@surf_to_vol_spackle",
	MINOR, TYPE_NEW_PROG,
	"propagate surface to volume and fill holes"
},
{ 23, APR, 2018, DRG, "align_epi_anat.py python3",
	MICRO, TYPE_MODIFY,
	"Makes align_epi_anat.py python3 compatible"
},
{ 28, JAN, 2018, DRG, "@measure_xxx surface object controllers",
	MICRO, TYPE_BUG_FIX,
	"keep surface object controllers open in the output suma scripts",
	"Mac OS bug makes closing and reopening controllers crash"
},
{ 18, JAN, 2018, DRG, "3dUndump cubes",
	MICRO, TYPE_NEW_OPT,
	"-cubes makes cubes instead of spheres"
},

{ 18, JAN, 2018, DRG, "thickness scripts",
	SUPER, TYPE_NEW_PROG,
	"Scripts to compute thickness of a mask 3 ways",
	"@measure_bb_thick, @measure_erosion_thick, @measure_in2out\n"
	"Suite of thickness scripts to compute thickness in volume\n"
    "and map to surface. Master script, @thickness_master, to\n"
    "call others with option for FreeSurfer input data"
},
{ 9, NOV, 2017, DRG, "whereami report changes",
	MICRO, TYPE_MODIFY,
	"linkrbrain, sumsdb no longer working, so turned off",
	"Also reduced line spacing in whereami html reports, default in GUI\n"
	"and coordinates showing up as -0.0 instead of 0.0 (for Justin)."
},
{ 31, OCT, 2017, DRG, "SUMA popups disabled",
	MICRO, TYPE_NEW_ENV,
	"SUMA popups are disabled by default, SUMA_SHOWPOPUPS",
	"Popups in SUMA that required clicking are now off\n"
	"by default. Bracket (hemisphere) hiding and small surface\n"
	"warnings only go to terminal. Other messages can be shown\n"
	"in popup messages with env. variable."
},
{ 11, JUL, 2017 , DRG , "DriveSuma quiet variable" , 
    MICRO , TYPE_NEW_OPT, 
    "SUMA_DriveSumaQuiet",
    "Make suma a little quieter with DriveSuma"
},
{ 19, APR, 2017 , DRG , "3dMean min, max options" , 
    MINOR , TYPE_NEW_OPT, 
    "3dMean computes min and max voxelwise across datasets",
    "Options -min, -max give min and max values. This can be\n"
    "combined with -non-zero to restrict to non-zero min and max."
},
{ 19, APR, 2017 , DRG , "3dLocalstat has_mask, has_mask2 options" , 
    MINOR , TYPE_NEW_OPT, 
    "3dLocalstat can report if neighborhood intersects specified values",
    "Options -has_mask and -has_mask2 allow flagging with unfillvalue"
},
{ 5, APR, 2017 , DRG , "3dVol2Surf nzoptions" , 
    MINOR , TYPE_NEW_OPT, 
    "3dVol2Surf nonzero min, nonzero max, nonzero ave",
    "Options allowing for statistics that ignore zero values"
},
{ 30, MAR, 2017 , DRG , "3dLocalstat" , 
    MINOR , TYPE_NEW_OPT, 
    "simple statistics of filled or unfilled",
    "These options provide binary tests for whether the neighborhood shape\n"
    "entirely fits within a mask or dataset around each voxel\n"
    "A value can be specified for a fill and unfill value"
},
{ 27, MAR, 2017 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "rm of temporary files could delete all files in current directory",
    "rm deletes all files that have tabs/spaces inserted in dataset names\n"
    "The tab  or space character can be somewhat invisible if after\n"
    "line continuation character in input script. Fix removes whitespace\n"
    "before deleting"
},
{ 5, JAN, 2017 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "NIFTI output and output directories not always handled correctly or completely"
},
{ 17, NOV, 2016 , DRG , "DriveSuma" , 
    MINOR , TYPE_NEW_OPT, 
    "Ask suma to send current surface name to SUMA_OUTPLUG file or stdout"
},
{ 19, AUG, 2016 , DRG , "3dLocalstat" , 
    MINOR , TYPE_BUG_FIX, 
    "Neighborhoods miscalculated"
},
{ 17, AUG, 2016 , DRG , "align_epi_anat.py" , 
    MICRO , TYPE_BUG_FIX, 
    "History not updated properly on output with NIFTI dset1/anat"
},
{ 9, AUG, 2016 , DRG , "@SUMA_MakeSpecFS" , 
    MINOR , TYPE_MODIFY, 
    "Atlasize original unranked datasets too. They will have more reliable indices than the ranked"
},
{ 11, JUL, 2016 , DRG , "3dWarp tta2mni grid dimensions bug" , 
    MINOR , TYPE_BUG_FIX, 
    "Ignored grid dimensions in transformation between Talairach to MNI"
},
{ 30, JUN, 2016 , DRG , "unWarpEPI.py anat to epi lpc+ZZ cost" , 
    MINOR , TYPE_BUG_FIX, 
    "Mistakenly used lpa for anat to epi alignment"
},
{ 29, JUN, 2016 , DRG , "align_epi_anat.py flipping identity" , 
    MINOR , TYPE_BUG_FIX, 
    "Case of mistaken IDENTITY for check_flip option"
},
{ 19, JUN, 2016 , DRG , "align_epi_anat.py dset1/2 deobliquing" , 
    MICRO , TYPE_BUG_FIX, 
    "Using dset1/2 terminology forced deobliquing off"
},
{ 13, JUN, 2016 , DRG , "3dTstat - single sub-bricks" , 
    MINOR , TYPE_NEW_OPT, 
    "For datasets with only a single sub-brick, 3dTstat would exit\n"
    "with an error for many statistics. The new behavior makes the\n"
    "program use the first value of the time series instead. Some other\n"
    "requested statistics like the argmax type stats are now calculated\n"
    "even for this trivial case. Optionally the user may use\n"
    "-firstvalue for a statistic"
},
{ 3, JUN, 2016 , DRG , "3dQwarp bug" , 
    MINOR , TYPE_BUG_FIX, 
    "bug fix in option processing\n"
    "Fixed wtprefix handling and gridlist option handling (with RCR)"
},
{ 16, MAY, 2016 , DRG , "align_epi_anat.py - left/right test" , 
    MINOR , TYPE_NEW_OPT, 
    "left/right checking using align_epi_anat.py\n"
    "flipped datasets go unnoticed even in major publicly available\n"
    "databases (initially noted by Brad Buchsbaum with FCON1000).\n"
    "Now automatically determine mismatch in L/R with simple test\n"
    "New -check_flip and -flip_giant options check alignment against\n"
    "flipped data."
},

{ 3, MAY, 2016 , DRG , "whereami - atlas queries" , 
    MICRO , TYPE_BUG_FIX, 
    "Removing structures that are all numbers was breaking atlas label queries"
},

{ 3, MAY, 2016 , DRG , "@Atlasize no voxel regions" , 
    MICRO , TYPE_NEW_OPT, 
    "Skip structures with no voxels",
    "New -skip_novoxels option in @Atlasize and @MakeLabelTable"
},
{ 25, APR, 2016 , DRG , "afni GUI perc threshold flag" , 
    MICRO , TYPE_BUG_FIX, 
    "percentile thresholding didn't work properly with warp-on-demand between views"
},

{ 19, APR, 2016 , DRG , "unWarpEPI.py" , 
    MINOR , TYPE_NEW_PROG, 
    "blip-up/down distortion correction script"
},
{ 7, MAR, 2016 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "fixed edge option change using too small neighborhood size"
},
{ 10, FEB, 2016 , DRG , "afni" , 
    MICRO , TYPE_BUG_FIX, 
    "fixed ignoring of -no1D option, and 1D files are not read on input"
},
{ 22, JAN, 2016 , DRG , "auto_warp.py" , 
    MICRO , TYPE_BUG_FIX, 
    "skip_affine option previously skipped"
},
{ 22, JAN, 2016 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_NEW_OPT, 
    "rigid_body alignment option and better handling of user allineate options"
},
{ 22, JAN, 2016 , DRG , "align_epi_anat.py" , 
    MICRO , TYPE_BUG_FIX, 
    "Incorrect check for file output existence for oblique data"
},
{ 18, SEP, 2015, DRG, "@DBSproc bug fix",
    MINOR, TYPE_BUG_FIX,
    "Left-right coordinate flip in @DBSproc script for Silvina Horovitz"
},
{ 1, SEP, 2015, DRG, "3dcalc extreme, absextreme",
    MINOR, TYPE_NEW_OPT,
    "New extreme and absextreme operators to find extreme values"
},
{ 9, JUL, 2015, DRG, "suma flip colormaps",
    MICRO, TYPE_MODIFY,
    "Colormaps imported from AFNI are now flipped \'properly\'"
},
{ 6, JUL, 2015, DRG, "RedBlueGreen Colormap",
    MICRO, TYPE_MODIFY,
    "New RedBlueGreen colormap for afni and suma"
},
{ 14, JUN, 2015, DRG, "align_epi_anat.py edge",
    MICRO, TYPE_BUG_FIX,
    "Fixed broken edge option"
},
{ 2, JUN, 2015, DRG, "plugout_drive to stdout or file",
    MICRO, TYPE_NEW_ENV,
    "Allow plugout_drive to get xyz,ijk,AFNI environment to file",
    "plugout_drive for GET_DICOM_XYZ, GET_ENV was to stdout of afni\n"
    "GUI. This was difficult to parse. Allow resetting output to file\n"
    "using AFNI_OUTPLUG environment variable or SET_OUTPLUG command"
},
{ 2, JUN, 2015, DRG, "graph allow single time point for writing graph",
    MICRO, TYPE_MODIFY,
    "Allow single TR/anat dataset to work with 'w' in graph mode"
},
{ 22, MAY, 2015 , DRG , "3dLocalstat mode" , 
    MINOR , TYPE_NEW_OPT, 
    "Find mode and non-zero mode in voxel neighborhood"
},
{ 30, APR, 2015 , DRG , "Selenium webdriver for afni help page too" , 
    MICRO , TYPE_GENERAL, 
    "Opens page for all afni help page using python selenium webdriver"
},
{ 28, APR, 2015 , DRG , "to3d - do not write BRIK" , 
    MICRO , TYPE_NEW_OPT, 
    "Do not write binary data with -nowritebrik",
    "May be useful for faster realtime acquisition with symlinks"
} ,
{ 27, MAR, 2015 , DRG , "Selenium webdriver to open webpages" , 
    MAJOR , TYPE_GENERAL, 
    "Opens webpages without multiple tabs using python selenium webdriver"
} ,
{ 3, MAR, 2015 , DRG , "align_epi_anat.py align_centers on/off/yes/no" , 
    MICRO , TYPE_MODIFY, 
    "align_centers can be on or yes, off or no"
} ,
{ 26, FEB, 2015 , DRG , "align_epi_anat.py align_centers bug" , 
    MINOR , TYPE_BUG_FIX, 
    "align_centers always called by mistake"
} ,
{ 19 , FEB, 2015 , DRG , "@Align_Centers child bug" , 
    MINOR , TYPE_BUG_FIX, 
    "child datasets not created properly",
    "If creating new shift datasets (the default), the\n"
    "datasets would not be properly updated to reflect\n"
    "the new origin shift."
} ,
{ 17 , FEB, 2015 , DRG , "align_epi_anat.py center alignment" , 
    MINOR , TYPE_NEW_OPT, 
    "-ginormous_move and -align_centers added",
    "Added call to @Align_Centers for initial center alignment\n"
    "The -align_centers option can be used by itself with the \n"
    "transformation matrix included in the combination transformation.\n"
    "Adding the center alignment on to giant_move, given the nom\n"
    "of ginormous_move. Note these options ignore any obliquity\n" 
    "in the original datasets."
} ,

{ 23 , JAN, 2015 , DRG , "whereami GUI" , 
    MINOR , TYPE_NEW_ENV, 
    "Web-based links in Whereami GUI on by default",
    "Links will be available by default now. Effectively\n"
    "equivalent to setting these environment variables to YES:\n"
    "AFNI_WEBBY_WAMI, AFNI_SUMSDB and AFNI_NEUROSYNTH."
} ,

{ 20 , JAN, 2015 , DRG , "whereami GUI" , 
    MINOR , TYPE_NEW_ENV, 
    "SumsDB link",
    "Link out from whereami GUI in afni to SumsDB database\n"
    "with new environment variable AFNI_SUMSDB (YES/NO).\n"
    "Must also set AFNI_WEBBY_WAMI to YES."
} ,

{ 8 , JAN, 2015 , DRG , "@Shift_Volume" , 
    MICRO , TYPE_BUG_FIX, 
    "No need to update space for simple shift option",
    "Previous version reported error for RAI shift but produced correct results\n"
} ,

{ 18 , DEC, 2014 , DRG , "VmPFC atlas" , 
    MINOR , TYPE_GENERAL, 
    "Ventro-medial prefrontal cortex atlas",
    "Worked with Scott Mackey to make maximum probability map atlas available"
} ,
{ 4 , DEC, 2014 , DRG , "@Align_Centers" , 
    MINOR , TYPE_NEW_OPT, 
    "1Dmat_only_nodset option",
    "Undid former fix because other scripts may rely on these shifted datasets\n"
    "and added new option. Datasets will not be output or modified with\n"
    "new option, and just the transformation matrix is output"
} ,

{ 2 , DEC, 2014 , DRG , "@Align_Centers" , 
    MICRO , TYPE_BUG_FIX, 
    "fix for 1Dmat_only output",
    "Datasets were output or modified even with this option,\n"
    "Now just the transformation matrix is output"
} ,

{ 2 , DEC, 2014 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "fixes for child_anat and mean epi base",
    "Thanks to Isaac Schwabacher for the child_anat fix!"
} ,

{ 9 , NOV, 2014 , DRG , "whereami connections" , 
    MINOR , TYPE_MODIFY, 
    "Web display includes connections links",
    "Developed for macaque connection webpages"
} ,
{ 20 , OCT, 2014 , DRG , "Haskins Pediatric templates and atlases" , 
    MAJOR , TYPE_GENERAL, 
    "Nonlinear and affine versions of Haskins Pediatric templates and atlases",
    "New templates with matching segmentation atlases are provided for\n"
    "pediatric subjects, ages 8-12"
} ,

{ 17 , SEP, 2014 , DRG , "MNI nonlinear templates" , 
    MINOR , TYPE_MODIFY, 
    "Nonlinear version of MNI-152 template in distribution",
    "The nonlinear 2009c version is more useful, particularly as a base for\n"
    "nonlinear alignment. AFNI_atlas_spaces.niml includes descriptions for\n"
    "native MNI152 2009c T1 template and a transformed version aligned to\n"
    "the TT_N27 dataset in Talairach space"
} ,

{ 17 , SEP, 2014 , DRG , "auto_warp.py" , 
    MICRO , TYPE_BUG_FIX, 
    "Did not pass qw_opts properly to 3dQwarp"
} ,

{ 4 , SEP, 2014 , DRG , "AFNI_ATLAS_PATH" , 
    MINOR , TYPE_NEW_ENV, 
    "Atlases and templates may be stored in location set by AFNI_ATLAS_PATH",
    "Atlases and templates need not be in the default afni binary directory.\n"
    "This variable may hold multiple directories that specify the possible\n"
    "locations of atlases when the atlas definition does not include the\n"
    "path in the filename. The atlases are typically defined in \n"
    "AFNI_atlas_spaces.niml file or in a CustomAtlases.niml file"
} ,

{ 9 , JUL, 2014 , DRG , "3dDWItoDT" , 
    MINOR , TYPE_NEW_OPT, 
    "Mean b=0 values computed for linear estimate",
    "New -mean_b0 option allows for averaging of b=0 values\n"
    "used in linear model and initial linear estimate for nonlinear\n"
    "method."
} ,

{ 3 , JUL, 2014 , DRG , "3dAllineate" , 
    MINOR , TYPE_BUG_FIX, 
    "oblique NIFTI dataset handling",
    "NIFTI datasets with oblique orientations were written\n"
    "with incorrect (original) orientations after alignment. Dataset is\n"
    "now cardinalized and input obliquity is ignored"
} ,

{ 13 , JUN, 2014 , DRG , "afni" , 
    MINOR , TYPE_GENERAL, 
    "Jump to MNI or any space coordinates",
    "Jump to MNI in afni GUI enhanced to recognize current dataset\n"
    "space and not assume MNI to TLRC transformation. Also can jump\n"
    "to other space coordinates by setting AFNI_JUMPTO_SPACE in\n"
    ".afnirc or Environment plugin"
} ,


{ 23 , MAY, 2014 , DRG , "whereami linkrbrain" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed linkrbrain coordinates and malloc/free error",
    "Coordinates were not transformed properly from TLRC to MNI\n"
    "space. Crashes from clusterize in afni GUI caused by mismatched\n"
    "AFNI-friendly malloc, free in strdup function"
} ,

{ 19 , MAY, 2014 , DRG , "align_epi_anat.py" , 
    MICRO , TYPE_BUG_FIX, 
    "NIFTI dataset input and save_skullstrip",
    "Fixed bug with both NIFTI input and save_skullstrip"
} ,

{ 2 , MAY, 2014 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_GENERAL, 
    "NIFTI dataset input",
    "align_epi_anat.py modified to allow NIFTI input for anat,epi,\n"
    "child_epi datasets. Output continues to be AFNI format."
} ,

{ 24 , APR, 2014 , DRG , "edge detect button in afni viewer" , 
    MICRO , TYPE_GENERAL, 
    "Toggle edge detection with 'e' key in viewer",
    "Can toggle with keypress from viewer allowing plugout to drive\n"
    "edge detection."
} ,

{ 9 , APR, 2014 , DRG , "whereami supplemental web-based info" , 
    MINOR , TYPE_GENERAL, 
    "Atlases may have supplemental information available from a website",
    "Further information can be opened through a web browser for individual\n"
    "structures. This feature is implemented initially for the support of the \n"
    "Saleem macaque atlas."
} ,

{ 04 , APR, 2014 , DRG , "whereami -linkrbrain" , 
    MINOR , TYPE_NEW_OPT, 
    "New option for getting task or gene correlation with coordinate list",
    "Linkrbrain.org support is available in alpha form from whereami and\n"
    "the Clusterize GUI. AFNI_LINKRBRAIN needs to be set to YES to use new\n"
    "features."
} ,

{ 27 , MAR, 2014 , DRG , "align_epi_anat.py" , 
    MICRO , TYPE_BUG_FIX, 
    "h_view help added"
} ,
{ 17 , Feb, 2014 , DRG , "@MakeLabelTable" , 
    MICRO , TYPE_BUG_FIX, 
    "-centers option incorrect for some atlases",
    "incorrect centers of mass for datasets with missing ROI values\n"
},

{ 31 , JAN, 2014 , DRG , "AFNI_IMAGE_GLOBALRANGE" , 
    MINOR , TYPE_NEW_ENV, 
    "New options for scaling display in afni GUI",
    "AFNI_IMAGE_GLOBALRANGE can be set to SLICE (default), VOLUME (SUBBRICK),\n"
    "or DSET. The GUI applies the lookup table to color the underlay with the\n"
    "range determined from the slice, sub-brick or the whole multi-sub-brick\n"
    "dataset. This environment variable may be set in a user's .afnirc file,\n"
    "Additionally, the GUI allows changes from the environment plugin menu or\n"
    "in two new places - the right-click menu on the image viewer colorbar.\n"
    "Typing Control-m in an image viewer cycles among the global range types.\n"
    "Previous YES/NO definitions for this variable correspond to VOLUME and\n"
    "SLICE respectively and will continue to work as before. The lower right\n"
    "corner of the image viewer shows the current range setting:\n"
    "(2%-98%/Min2Max, Vol, Dset)"
} ,

{ 9 , JAN, 2014 , DRG , "3dDWItoDT" , 
    MINOR , TYPE_BUG_FIX, 
    "bmatrix options",
    "bmatrix_Z and bmatrix_NZ options replace old -bmatrix option\n"
    "The new options allow for a B=0 and no B=0 row in an optional\n"
    "b-matrix input. The b-matrix may be used as input instead of the\n"
    "gradient vector direction file. The former -bmatrix option\n"
    "resulted in an error or incorrect results. (Paul Taylor is mostly\n"
    "responsible for recognizing and fixing this. Thanks, Paul!)"
} ,

{ 21 , OCT, 2013 , DRG , "whereami GUI" , 
    MINOR , TYPE_NEW_ENV, 
    "NeuroSynth.org link",
    "Link out from whereami GUI in afni to neurosynth.org\n"
    "with new environment variable AFNI_NEUROSYNTH (YES/NO).\n"
    "Must also set AFNI_WEBBY_WAMI to YES."
} ,

{ 14 , MAY, 2013 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "motion file renamed",
    "Renamed motion files for -save_vr without -epi2anat"
} ,
{ 7 , MAY, 2013 , DRG , "3dAnatNudge" , 
    MINOR , TYPE_MODIFY, 
    "always produce output with -prefix even if no shift"
} ,

{ 19 , APR, 2013 , DRG , "3dmaskdump" , 
    MINOR , TYPE_NEW_OPT, 
    "lpi xyz output option"
} ,

{ 17 , APR, 2013 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "save_vr result causes crash"
} ,

{ 1 , APR, 2013 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "motion file renamed",
    "Some output files like the motion files were accidentally renamed."
} ,
{ 29 , MAR, 2013 , DRG , "general compression" , 
    MINOR , TYPE_NEW_ENV, 
    "pigz for faster gzip-like compression",
    "pigz is a gzip utility that makes use of multiple CPU's.\n"
    "It produces and uses .gz files. To use, set AFNI_COMPRESSOR to PIGZ."
} ,
{ 27 , MAR, 2013 , DRG , "3dBrickStat" , 
    MINOR , TYPE_NEW_OPT, 
    "absolute value of voxels for statistics"
} ,
{ 19 , MAR, 2013 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "alignment AddEdge error",
    "Error in resampling step of AddEdge procedure causes crash.\n"
    "Problem requires both -giant_move (or -prep_off or -resample off) and\n"
    "-AddEdge."
} ,
{ 12 , MAR, 2013 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_NEW_OPT, 
    "Handling input and output directories",
    "Output now goes to current directory (./) unless otherwise specified\n"
    "with -output_dir. Thanks a lot, Rick."
} ,
{ 11 , MAR, 2013 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_NEW_OPT, 
    "Handling input and output directories",
    "Output follows input directories unless specified with -output_dir\n"
    "Previously, all input files had to exist in current directory.\n"
    "Changes allow for output to follow anat or epi dataset directories\n"
    "or specified output directory. Also minor change to call tcsh without\n"
    "sourcing startup .cshrc"
} ,
{ 11 , MAR, 2013 , DRG , "@Atlasize center of mass" , 
    MINOR , TYPE_NEW_OPT, 
    "Add -centers for center of mass locations in atlas regions",
    "-centers option added to @Atlasize and @MakeLabelTable\n"
    "This location is simple center of mass for now and may be\n"
    "outside region for non-blobbish regions"
} ,
{ 07 , FEB, 2013 , DRG , "3dDWItoDT" , 
    MINOR , TYPE_NEW_OPT, 
    "Added Greg Baxter's (UCSD) change for b-matrix input"
} ,
{ 04 , JAN, 2013 , DRG , "3drefit" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed bug that would make 3drefit exit if dataset contains a warpdrive attribute"
} ,
{ 16 , OCT, 2012 , DRG , "afni show atlas colors" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed bug that would crash afni if paned color and show atlas colors"
} ,
{ 15 , AUG, 2012 , DRG , "whereami web" , 
    MICRO , TYPE_BUG_FIX, 
    "Web-based atlases would not open webpage with ampersands, punctuation"
} ,
{ 14 , AUG, 2012 , DRG , "whereami minimum probability" , 
    MINOR , TYPE_NEW_OPT, 
    "Minimum probability option to consider for probabilistic atlases",
    "Can set option, -min_prob, or environment variable,\n"
    "AFNI_WHEREAMI_PROB_MIN, to control output of whereami reports or\n"
    "masks from probabilistic atlases"
} ,

{ 14 , AUG, 2012 , DRG , "3drefit -epan error" , 
    MINOR , TYPE_BUG_FIX, 
    "Removed error for refitting data type",
    "3drefit would exit with error for -epan and multi-subbrick data\n"
    "Removed incorrect checks against archaic timepoints per TR nvals"
} ,

{ 06 , AUG, 2012 , DRG , "NIFTI reading" , 
    MINOR , TYPE_NEW_ENV, 
    "Handling view equivalents for NIFTI",
    "AFNI_NIFTI_VIEW sets default view for AFNI datasets marked as\n"
    "'aligned' (sform_code or qform_code = 2). This variable should\n"
    "be set to 'TLRC', 'ORIG' or 'ACPC'."
} ,

{ 28 , JUN, 2012 , DRG , "Draw Dataset plugin" , 
    MINOR , TYPE_BUG_FIX, 
    "Resampling of atlas regions to lower resolution undercounted",
    "Draw Dataset plugin atlas resampling to a lower resolution dataset\n"
    "undercounts number of voxels. Incorrect usage of temporary volume\n"
    "in byte storage was fixed with float volume. Additionally, new\n"
    "environment variable, AFNI_DRAW_THRESH, allows for variable\n"
    "threshold level (done with RCR)"
} ,

{ 30 , MAY, 2012 , DRG , "3dinfo, 3drefit" , 
    MINOR , TYPE_MODIFY, 
    "av_space option handling and orig/other space refitting",
    "3dinfo reports view extension for -av_space instead of generic\n"
    "space view extension. 3drefit will warn if setting non-orig\n"
    "space on orig view data or orig space on tlrc view data."
} ,
{ 24 , MAY, 2012 , DRG , "3dDWItoDT" , 
    MINOR , TYPE_NEW_OPT, 
    "csf_val and csf_fa options apply to linear solutions too"
} ,
{ 23 , MAY, 2012 , DRG , "3dinfo, @auto_tlrc" , 
    MAJOR , TYPE_BUG_FIX, 
    "Generic space handling corrected for TT_N27 and other spaces",
    "@auto_tlrc would crash with improper generic spaces\n"
    "Generic space restricted to AFNI view names (orig, acpc, tlrc)"
} ,
{ 22 , MAY, 2012 , DRG , "3dDWItoDT" , 
    MINOR , TYPE_NEW_OPT, 
    "csf_val and csf_fa options for default values if B=0 less than gradient values"
} ,
{ 8 , MAY, 2012 , DRG , "3dinfo" , 
    MICRO , TYPE_NEW_OPT, 
    "gen_space option to see generic space type for a dataset"
} ,
{ 7 , MAY, 2012 , DRG , "NIFTI input/output" , 
    MINOR , TYPE_MODIFY, 
    "NIFTI qform/sform codes from generic space of template space",
    "qform and sform codes are set by string in generic space, not template\n"
    "space, of input dataset. For example, TT_N27 spaces will be marked for\n"
    "NIFTI output as TLRC. NIFTI data with any non-ORIG or ACPC space will\n"
    "be marked as an 'aligned' space in NIFTI with the sform and qform codes.\n"
    "Currently only TLRC/MNI/Aligned are supported spaces in NIFTI standard.\n"
    "'aligned' space datasets will be interpreted as equivalent to TLRC view\n"
    "datasets. See AFNI_atlas_spaces.niml for space definitions that include\n"
    "generic space names."
} ,
{ 7 , MAY, 2012 , DRG , "@auto_tlrc" , 
    MICRO , TYPE_NEW_OPT, 
    "out_space option to force output space for auto-talairached data"
} ,
{ 7 , MAY, 2012 , DRG , "to3d" , 
    MICRO , TYPE_MODIFY, 
    "Clearer warning for illegal transfer syntax"
} ,
{ 7 , MAY, 2012 , DRG , "whereami" , 
    MINOR , TYPE_BUG_FIX, 
    "Coordinate transformations in atlas access",
    "Coordinates were not transformed properly if an inverse affine\n"
    "transformation was required for the case of atlases in another\n"
    "space than the dataset."
} ,
{ 9 , APR, 2012 , DRG , "whereami" , 
    MICRO , TYPE_BUG_FIX, 
    "Typo in AFNI_WAMI_MAX_SEARCH_RAD",
    "Wrong name listed in AFNI GUI environment and README.environment"
} ,

{ 27 , FEB, 2012 , DRG , "whereami" , 
    MINOR , TYPE_BUG_FIX, 
    "Bug in combination of multiple affine transformations",
    "Computation error in combining affine transformations\n"
    "in whereami. Note all default transformations in whereami\n"
    "do not combine affine transformations. Instead, space\n"
    "transformations between TLRC and MNI_ANAT spaces are\n"
    "computed using a 12-piece or a Brett transformation combined\n"
    "with an affine in two stages."
} ,

{ 21 , FEB, 2012 , DRG , "whereami" , 
    SUPER , TYPE_NEW_OPT, 
    "Web-based access atlases available",
    "Able to query web atlases from whereami and open webpages\n"
    "Support starting with rat brain atlas from Elsevier's\n"
    "BrainNavigator"
} ,

{ 07 , NOV, 2011 , DRG , "deobliquing" , 
    MINOR , TYPE_MODIFY, 
    "More programs lose original data obliquity",
    "3drefit, 3dWarp, 3daxialize, 3dZcutup, 3dCM, 3dZregrid\n"
    "added to the list of programs"
} ,

{ 07 , NOV, 2011 , DRG , "lost last character in sub-brick labels" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed miscount of characters"
} ,

{ 03 , NOV, 2011 , DRG , "longer sub-brick labels" , 
    MINOR , TYPE_MODIFY, 
    "Allow sub-brick labels to be up to 63 characters"
} ,

{ 03 , NOV, 2011 , DRG , "3drefit" , 
    MINOR , TYPE_BUG_FIX, 
    "Allow refitting of some attributes",
    "Some attributes like IJK_TO_DICOM_REAL and DELTA were not\n"
    "being updated correctly with the original values remaining\n"
    "in the dataset"
} ,


{ 13 , SEP, 2011 , DRG , "3dMean" , 
    MINOR , TYPE_NEW_OPT, 
    "Added non_zero and count options"
} ,

{ 13 , SEP, 2011 , DRG , "whereami" , 
    MICRO , TYPE_BUG_FIX, 
    "Added trivial ability to invert identity transformation"
} ,

{ 6 , SEP, 2011 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "save_skullstrip option saved obliqued and skullstripped data",
    "Fixed to save only skullstripped"
} ,

{ 02 , SEP, 2011 , DRG , "3dhistog" , 
    MINOR , TYPE_BUG_FIX, 
    "Handle binning and output of integer and float data differently",
    "Distinguish between integer and float data. Added float and int\n"
    "options to enforce specified behavior.\n"
    "Done with Rick Reynolds."
} ,

{ 30 , AUG, 2011 , DRG , "Draw Dataset" , 
    MINOR , TYPE_BUG_FIX, 
    "Draw Dataset plugin incorrect label for current atlas",
    "Fixed current atlas to be updated properly if atlas is\n"
    "changed."
} ,

{ 11 , JUL, 2011 , DRG , "whereami" , 
    MINOR , TYPE_NEW_ENV, 
    "AFNI_ATLAS_COLORS and default atlas in AFNI GUI",
    "Set atlas to use for Atlas colors, Go to atlas location,\n"
    "Draw Dataset and Renderer. Name of atlas can be set in\n"
    "environment and modified in the AFNI GUI environmentalism menu.\n"
    "Several other environmental variables regarding atlas usage\n"
    "can also be modified there too."
} ,

{ 27 , JUN, 2011 , DRG , "whereami" , 
    MINOR , TYPE_NEW_ENV, 
    "AFNI_WHEREAMI_DEC_PLACES",
    "Set precision for whereami output\n"
    "Higher field data and animal atlases require higher precision.\n"
    "The default value used for focus point among template spaces is\n"
    "still 0 decimal places (closest mm), but animal data requires three\n"
    "decimal places. Value may range from 0 to 10.\n"
} ,

{ 23 , JUN, 2011 , DRG , "whereami" , 
    MINOR , TYPE_NEW_ENV, 
    "Desai MPM atlases",
    "New maximum probability map atlases derived from the Desai probability\n"
    "maps just introduced in distribution."
} ,

{ 10 , JUN, 2011 , DRG , "whereami" , 
    SUPER , TYPE_GENERAL, 
    "Generic Atlas Support and new atlases",
    "Atlases, spaces and transformations are now defined in a NIML file,\n"
    "AFNI_atlas_spaces.niml. Transformations among spaces for coordinates\n"
    "are defined in that file for use by whereami and the whereami display\n"
    "in the AFNI GUI. The flexible naming of the spaces and atlases allows\n"
    "easy addition of new atlases to AFNI processing.\n"
    "Two new environment variables control what atlases and spaces are shown\n"
    "when no atlas is specified, AFNI_ATLAS_LIST and AFNI_TEMPLATE_SPACE_LIST\n"
    "The list of atlases now defaults to the TT_Daemon and the Eickhoff-\n"
    "Zilles atlases in MNI_ANAT space.\n\n"
    "Additionally, the cytoarchitectonic atlases from the Zilles, Eickhoff\n"
    "group have been updated to the most recent version, 1.8.\n"
    "***The previous versions, 1.5, are not used in this release by default.\n"
    "Depending upon differences on how regions or codes are defined and used,\n"
    "it is possible that processing scripts and results may be affected.\n\n"
    "With this release, we also introduce three probabilistic atlases\n"
    "donated by the Desai group generated from a typical AFNI pipeline.\n"
    "These atlases contain a multitude of regions created using both\n"
    "@auto_tlrc and FreeSurfer."
} ,

{ 07 , APR, 2011 , DRG , "whereami" , 
    MAJOR , TYPE_NEW_ENV, 
    "Framework changes for generic atlases",
    "Atlases now can have segmentation in a NIML structure attribute\n"
    "in the atlas dataset header. The environment variable,\n"
    "AFNI_CUSTOM_ATLAS, can contain a custom atlas prefix. Details on\n"
    "NIML attributes will be forthcoming. In the meantime, existing\n"
    "atlases should work as before with the minor exception of mask\n"
    "datasets are now set to have voxel values of 1 rather than\n"
    "the values from the atlas dataset." 
} ,

{ 24 , MAR, 2011 , DRG , "whereami" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed bug for case of MNI_ANAT space as not processed"
} ,

{ 19 , MAR, 2011 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed bug for case master_dset2_dxyz was accidentally ignored"
} ,

{ 15 , MAR, 2011 , DRG , "lpc_align.py" , 
    MICRO , TYPE_GENERAL, 
    "lpc_align.py is superceded by align_epi_anat.py",
    "Program now exits with short message to use align_epi_anat.py"
} ,

{ 14 , MAR, 2011 , DRG , "Draw Dataset Plugin" , 
    MINOR , TYPE_MODIFY, 
    "Update Draw Dataset for clarity and ROI labels",
    "Draw Dataset is clearer with regards to overlay/underlay drawing\n"
    "instead of the older func/anat terminology. Also an ROI color scale\n"
    "is used for drawing and labels are updated in the AFNI Overlay GUI\n"
    "immediately. Need to set AFNI_INT_CMAP or AFNI_INT_PBAR to use."
} ,

{ 14 , MAR, 2011 , DRG , "afni GUI" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed bugs when switching between ROI and continuous overlay datasets",
    "Colorscale (scale, range) is restored with continuous overlay dataset.\n"
    "Need to set AFNI_INT_CMAP or AFNI_INT_PBAR to use."
} ,

{ 12 , OCT, 2010 , DRG , "3dDTtoDWI" , 
    MINOR , TYPE_NEW_PROG, 
    "Program to compute DWI images from diffusion tensor",
    "Release from old code, bug fix for scale factors and lower\n"
    "triangular order.\n"
} ,

{ 07 , OCT, 2010 , DRG , "Plugout commands" , 
    MINOR , TYPE_NEW_OPT, 
    "Commands to get current RAI xyz or IJK coordinates in a plugout script"
} ,

{ 01 , OCT, 2010 , DRG , "afni GUI" , 
    MINOR , TYPE_BUG_FIX, 
    "Overlay color autorange, range settings not initialized properly"
} ,

{ 27 , SEP, 2010 , DRG , "afni GUI" , 
    MINOR , TYPE_MODIFY, 
    "Datasets and atlas datasets show labels in overlay panel",
    "Atlases distributed with afni have been updated to include labels\n"
    "in the header that show a label for each value. If a dataset has\n"
    "been created with the Draw Dataset plugin or a label table has been\n"
    "applied with 3drefit, then the labels will be displayed next to the\n"
    "corresponding value in the Overlay panel. Integral ROI colormaps are\n"
    "used for any dataset with label tables assigned or with an INT_CMAP\n"
    "attribute." 
} ,

{ 27 , SEP, 2010 , DRG , "3dWarp" , 
    MICRO , TYPE_MODIFY, 
    "Output using mni2tta or tta2mni are marked with an appropriate space"
} ,

{ 10 , SEP, 2010 , DRG , "afni_all" , 
    MAJOR , TYPE_MODIFY, 
    "Introduce code allowing multiple space sessions but limited to existing",
    "spaces (ORIG, ACPC, TLRC) so should continue to be transparent.\n"
    "Datasets are now tagged with space attribute, TLRC/MNI/MNI_ANAT,\n"
    "and defines which kind of template a dataset has been aligned to.\n"
    "This attribute is handled by @auto_tlrc, adwarp, 3drefit, whereami,\n"
    "and the AFNI GUI using the whereami GUI. In the AFNI GUI, this\n"
    "has the effect of allowing a dataset to be identified by its template\n"
    "with the transformation to the TLRC or other atlas space handled\n"
    "automatically. Other AFNI programs should apply the template space of\n"
    "the master dataset or first dataset to the output.\n"
    "cvs tag marked on code before this change as mid-atlantic"
} ,
{ 14 , JUL, 2010 , DRG , "afni_all" , 
    MAJOR , TYPE_MODIFY, 
    "Beginning to introduce code for allowing multiple spaces and atlases",
    "These code changes should initially have no effect on functionality\n"
    "and provide only a framework for introducing changes\n"
    "cvs tag marked on code before this change as pre-atlantic"
} ,
{ 13 , JUL, 2010 , DRG , "3dresample" , 
    MINOR , TYPE_MODIFY, 
    "Allowed for wider range of voxel sizes"
} ,

{ 8 , JUN, 2010 , DRG , "RetroTS.m" , 
    MINOR , TYPE_NEW_OPT, 
    "Allowed for alt+z2 slice timing"
} ,

{ 18 , MAY , 2010 , DRG , "model_demri_3" , 
    MINOR , TYPE_NEW_ENV, 
    "Allowed for flip angle variation through volume with scaling dataset"
} ,

{ 11 , MAY , 2010 , DRG , "model_demri_3" , 
    MINOR , TYPE_MODIFY, 
    "Changed minor defaults and error handling in DEMRI model"
} ,

{ 24 , MAR , 2010 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, 
    "Some flexibility with animal alignment and giant move." ,
    "feature_size, rat_align options used for fineblur option"
} ,

{ 15 , MAR , 2010 , DRG , "3dTstat, 3dMean" , 
    MICRO , TYPE_MODIFY,
    "Minor text changes to refer to the other program in help"
} ,

{ 1 , MAR , 2010 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, 
    "Changed default options used with giant_move and 3dAllineate." ,
    "Changed with version 1.30 of align_epi_anat.py"
} ,

{ 9 , FEB , 2010 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, 
    "Fixed silly bug introduced in previous version" ,
    "isdigit() method not called properly (Thanks Rick)"
} ,

{ 28 , JAN , 2010 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_NEW_OPT, 
    "Options for multiple cost functionals,edge control" ,
    "multi_cost option for trying out different cost functionals.\n"
    "check_cost option for comparing for big differences among cost\n"
    "functionals\n"
    "edge_erodelevel option for adjusting the number of layers to erode\n"
    "for edge option"
} ,

{ 14 , DEC , 2009 , DRG , "McRetroTS" , 
    MINOR , TYPE_BUG_FIX,   "Variable Opt parameters were not parsed correctly" ,
    "Options structure fields should be added correctly into Opt structure"
} ,

{ 12 , DEC , 2009 , DRG , "1dSEM" , 
    MINOR , TYPE_BUG_FIX,   "modify parsimonious fit index to be based on null model" ,
    "Previous calculation used chi-square from minimum model without adjusting\n"
    "for number of parameters in model."
} ,

{ 20 , NOV , 2009 , DRG , "3dAutomask" , 
    MINOR , TYPE_NEW_OPT,   "apply_prefix option to save automasked input" ,
    "avoids 3dcalc step that is usually used to apply an automask."
} ,

{ 04 , NOV , 2009 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_NEW_OPT, "3dAllineate option for motion correction" ,
    "volreg_method allows 3dAllineate, which is useful for DTI data.\n"
    "Bug fix for post-transformation matrix and dataset view"
} ,

{ 19 , OCT , 2009 , DRG , "align_epi_anat.py" , 
    MAJOR , TYPE_NEW_OPT, "New master options, dset1/2 terminology",
    "New master_nnn_dxyz options to specify output resolution\n"
    "dset1 and dset2 for non-EPI/anat alignment with lpa cost function\n"
    "and turns off preprocessing steps\n"
    "giant_move option turns off resampling and changes master options\n"
    "Expanded help - including fuller description of edge method\n"
    "Removed volume registration (motion correction) for anat2epi option\n"
    "by default.\n"
    "AFNI dataset views (+orig/+acpc/+tlrc) are maintained in output\n"
    "depending on BASE, SOURCE or external dataset names for all output"
} ,

{ 29 , SEP , 2009 , DRG , "McRetroTS" , 
    MINOR , TYPE_NEW_OPT, "Opt options available to compiled version",
    "All options available through the Opt structure are now available\n"
    "even with the compiled version of McRetroTS, making the compiled version\n"
    "functionally equivalent to the RetroTS.m version"
} ,

{ 29 , SEP , 2009 , DRG , "RetroTS" , 
    MINOR , TYPE_NEW_OPT, "Flexible slice timing options",
    "SliceOrder option allows for standard slice timing order\n"
    "  alt+z, alt-z, seq+z, seq-z, Custom and 1D text file input"
} ,

{ 8 , SEP , 2009 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, "Properly allows post-transformation matrix"
} ,

{ 8 , SEP , 2009 , DRG , "whereami" , 
    MINOR , TYPE_MODIFY, "Allows MNI_ANAT space for whereami",
    "Subtracts 8mm I-S and uses MNI space transform to TTA"
} ,

{ 3 , SEP , 2009 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, "Bug fixes for certain combinations",
    "Allowed BASE and SOURCE specification for oblique data\n"
    "Allowed 3dWarpDrive volume registration with mean/max base\n"
    "Removed anatomical output with oblique data and only epi2anat"
} ,

{ 23 , JUL , 2009 , DRG , "MEMRI models" , 
    MINOR , TYPE_NEW_OPT, "Single and Dual exponential models",
    "New models for 3dNLfim that use single and dual exponential models\n"
    "that are appropriate for MEMRI (Manganese Enhanced MRI"

} ,

{ 14 , JUL , 2009 , DRG , "afni" , 
    MINOR , TYPE_NEW_ENV, 
    "Added AFNI_ONE_OBLIQUE_WARNING",
    "AFNI_ONE_OBLIQUE_WARNING = YES makes interactive spit out ONE warning\n"
    "about obliquity per session then go quiet."
} ,

{ 17 , JUN , 2009 , DRG , "McRetroTS.m" , 
    MINOR , TYPE_NEW_PROG, "Matlab Compilable version of RetroTS.m",
    "This version calls the RetroTS function, but can be compiled\n"
    "for users that either do not have Matlab, a required toolbox\n"
    "or want to use this function in a shell script more easily." 
} ,

{ 17 , JUN , 2009 , DRG , "RetroTS.m" , 
    MINOR , TYPE_BUG_FIX, "Number of TRs found incorrectly because of rounding"
} ,

{ 15 , JUN , 2009 , DRG , "BrikLoad.m" , 
    MINOR , TYPE_BUG_FIX, "Typo in BrikLoad",
    "Gremlin got to Pixy"
} ,

{ 1 , MAY , 2009 , DRG , "1dSEM" , 
    MINOR , TYPE_BUG_FIX, "Tree growth fix",
    "Fixed bug in tree growth and improved output text"
} ,

{ 22 , APR , 2009 , DRG , "@AddEdge, align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, "Oblique edge display",
    "Fixed bug with oblique data in edge display"
} ,

{ 20 , APR , 2009 , DRG , "3dAllineate, misc" , 
    MICRO , TYPE_MODIFY, "1D file handling",
    "Handle row and column selectors for 1D files better for most\n"
    "AFNI programs. Returns error if improper row or column selection"
} ,

{ 2 , APR , 2009 , DRG , "align_epi_anat.py" , 
    MICRO , TYPE_MODIFY, "help update",
    "Incorporated Bob's recommendations into help to make\n"
    "various options clearer"
} ,

{ 18 , MAR , 2009 , DRG , "3dZcat, 3daxialize" , 
    MINOR , TYPE_NEW_OPT, "NIFTI support",
    "Fixed support for NIFTI output in 3dZcat and 3daxialize\n"
    "Added -frugal option to 3daxialize to keep old behavior\n"
    "for AFNI format datasets. Existing -frugal option in\n"
    "3dZcat imposes 'oldish' behavior too."
} ,

{ 10 , MAR , 2009 , DRG , "3dLocalstat" , 
    MINOR , TYPE_NEW_OPT, "Sum option",
    "Sum option (for functional weighting of interiorosity measure)"
} ,


{ 19 , DEC , 2008 , DRG , "align_epi_anat.py" , 
    MICRO , TYPE_BUG_FIX, "Slice timing for children can be ignored",
    "If child epi data does not need slice timing correction, script will\n"
    "continue instead of exiting"
} ,

{ 05 , DEC , 2008 , DRG , "to3d" , 
    MINOR , TYPE_MODIFY, "Siemens DICOM handling",
    "added -oblique_origin and -reverse_list to help handle Siemens\n"
    "DICOM data. oblique_origin option added to to3d and 3drefit\n"
    "Rick added flipped slice handling to oblique mosaic handling"
} ,

{ 07 , NOV , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, "tlrc view handling",
    "fixed view name of tlrc output with tlrc_master set to SOURCE"
} ,

{ 06 , NOV , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, "Oblique children handling",
    "fixed combination of oblique with child epis for epi2anat output"
} ,

{ 31 , OCT , 2008 , DRG , "3drefit" , 
    MINOR , TYPE_MODIFY, "Time axis attributes",
    "Added support in 3drefit for applying time axis attributes"
} ,

{ 30 , OCT , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX, "AddEdge with epi2anat",
    "fixed AddEdge option for epi2anat output"
} ,

{ 15 , OCT , 2008 , DRG , "afni plugouts, @AddEdge" , 
    MINOR , TYPE_MODIFY, "quiet communications",
    "@AddEdge silences communications as plugout"
} ,
{ 14 , OCT , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, "minor updates",
    "AddEdge option enhanced and help updated"
} ,

{ 14 , OCT , 2008 , DRG , "@AddEdge" , 
    MINOR , TYPE_MODIFY, "Help and options",
    "More user options, improved help, removed temporary files "
} ,

{ 6 , OCT , 2008 , DRG , "3drefit" , 
    MINOR , TYPE_MODIFY, "-atrfloat and -atrint options",
    "Allows addition and modification of dataset attributes\n"
    "including modifying IJK_TO_DICOM_REAL. Not all attributes\n"
    "can be modified if fairly basic to dataset."
} ,

{ 26 , SEP , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, "-child_anat option",
    "Convenience option to align follower anat datasets to epi"
} ,
{ 19 , SEP , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, "-giant_move option",
    "For data that are very far apart\n"
    "Fixed bug using long path names"
} ,
{ 18 , SEP , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, "More options",
    "Intermediate file saved optionally now,\n"
    "partial_axial,sagittal,coronal options\n"
    "Edge-based method, nocmass default, resample step optional,\n"
    "Added options to support searching for smaller structures,\n"
    "3dWarpDrive can be used optionally as volume registration program\n"
    "prep_off option to turn off several preprocessing steps\n"
    "AddEdge option uses simplified names in output in new directory"
} ,
{
 29 , AUG , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, "Feature size, AddEdge, skullstrip,rat options" ,
    "Added options to support searching for smaller structures,\n"
    "an option for rat brain alignment, alternate options for 3dSkullstrip\n"
    "and an optional call for @AddEdge"
} ,

{ 27 , AUG , 2008 , DRG , "3danisosmooth" , 
    MINOR , TYPE_BUG_FIX , "Initialized variable for 3D case" ,
    NULL
} ,

{ 30 , JUL , 2008 , DRG , "3dinfo.c" , 
    MICRO , TYPE_MODIFY, "Exit codes set to 1 on error" ,
},

{ 28 , JUL , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY, "Pre and post transformation matrices" ,
    "Allow pre-transformation matrix instead of oblique transformation\n"
    "and post-transformation instead of tlrc transformation.\n"
    "Fixed concatenated matrices for oblique data. Master options\n"
    "allow specification of dimension size on output. Script arguments\n"
    "saved in output dataset."
} ,

{ 18 , JUL , 2008 , DRG , "to3d" , 
    MINOR , TYPE_MODIFY, "Reverse order of slices in Siemens Mosaic data" ,
    "Some Siemens Mosaic data can be in reverse order depending upon\n"
    "obscure Siemens private DICOM tags.\n"
    "Thanks to Doug Greve in Freesurfer group for edifying this situation"
} ,

{ 15 , JUL , 2008 , DRG , "general" , 
    MINOR , TYPE_MODIFY, "Can turn off all obliquity warnings" ,
    "AFNI_NO_OBLIQUE_WARNING variable is extended to turn off warnings\n"
    "about using oblique data in commandline programs and in GUI"
} ,

{ 15 , JUL , 2008 , DRG , "3dWarp" , 
    MINOR , TYPE_BUG_FIX , "Print correct obliquity transformation matrix" ,
    NULL
} ,

{ 18 , JUN , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX , "Bug fixes - duplicate master options, mislabeled variable" ,
    NULL
} ,

{ 11 , JUN , 2008 , DRG , "align_epi_anat.py" , 
    MAJOR , TYPE_MODIFY , "Obliquity handling in alignment, more grid options" ,
    NULL
} ,

{ 11 , JUN , 2008 , DRG , "3dWarp.c" , 
    MICRO , TYPE_MODIFY , "Prints oblique transformation matrix" ,
    NULL
} ,

{ 11 , JUN , 2008 , DRG , "afni_base.py" , 
    MICRO , TYPE_MODIFY , "Added isFloat method to python support" ,
    NULL
} ,

{ 14 , MAY , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX , "1D file names for child epi data,micro changes" ,
    NULL
} ,

{ 17 , APR , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY , "minor changes" ,
    "Child epi datasets can be volume registered even if parent epi\n"
    "is a single sub-brick"
} ,

{ 16 , APR , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX , "multiple changes" ,
    "Naming conventions for tlrc output, generic shell compatible,\n"
    "regridding options for epi and tlrc epi output"
} ,

{ 14 , APR , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_BUG_FIX , "minor change" ,
    "3dAllineate options are also applied to epi to anat alignment,\n"
    "so output EPI datasets get original resolution and type."
} ,

{ 10 , APR , 2008 , DRG , "align_epi_anat.py" , 
    MINOR , TYPE_MODIFY , "minor changes" ,
    "remove tabs in file, change output file names for epi children,\n"
    "changes to help, and renamed tlrc parent option"
} ,

{ 07 , APR , 2008 , DRG , "align_epi_anat.py" , 
    SUPER , TYPE_NEW_PROG , "Alignment of EPI and Anatomical datasets" ,
    "Aligns anat and EPI data. Alignment is in either direction of anat to\n"
    "epi or epi to anat. Transformations are combined where possible as\n"
    "from time series registration and talairach transformations. Multiple\n"
    "child epi datasets may be aligned at the same time."
 } ,

{ 28, MAR , 2008 , DRG , "3dDTeig" , MINOR ,
   TYPE_BUG_FIX ,"small negative eigenvalues are set to zero as in 3dDWItoDT" ,
   "This fix avoids FA being set to 0 for those affected voxels"
 } ,

{ 26, MAR , 2008 , DRG , "python" , MINOR ,
   TYPE_BUG_FIX ,   "repaired support for dry_run mode in python scripts" ,
   NULL
 } ,

{ 26, MAR , 2008 , DRG , "matrix.c" , MICRO , TYPE_BUG_FIX ,
   "freed matrix used in matrix_sqrt functions" ,
   NULL
 } ,


 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
