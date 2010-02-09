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
