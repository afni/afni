
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
