
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

 -- examples (newest at top) --

 { 30 , FEB , 2008 , BGP , "website" , MAJOR , TYPE_GENERAL ,
   "updated links in class handout" ,
   NULL
 } ,

 { 19 , FEB , 2008 , BGP , "my_program" , SUPER , TYPE_NEW_PROGRAM ,
   "short description of change" ,
   "(optional) description of where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

afni_history_struct discoraj_history[] = {

 /* can copy to and change one of the examples here */
 { 18 , MAY , 2018 , JKR , "abids_lib.py" , SUPER , TYPE_NEW_PROG ,
   "New library to handle data from json files. Useful for BIDS data."
 } ,
 { 18 , MAY , 2018 , JKR , "abids_tool.py" , SUPER , TYPE_NEW_PROG ,
   "New program to modify BIDS datasets."
 } ,
 { 18 , MAY , 2018 , JKR , "abids_json_info.py" , SUPER , TYPE_NEW_PROG ,
   "New program to extract data from json files. Useful for BIDS data."
 } ,
 { 04 , MAY , 2018 , JKR , "BayesianGroupAna.py" , SUPER , TYPE_NEW_PROG ,
   "New program to preform Bayesian group analysis on ROI level data."
 } ,
 { 23 , MAR , 2018 , JKR , "tedana_wrapper.py" , SUPER , TYPE_NEW_PROG ,
   "add wrapper for tedana.py that will be run from afni_proc.py"
 } ,
 { 5 , MAR , 2018 , JKR , "FATCAT_matplot" , MINOR , TYPE_GENERAL ,
   "changed name of @FATCAT_heatmap_shiny to FATCAT_matplot"
 } ,
 { 26 , JAN , 2018 , JKR , "dicom_hinfo" , MINOR , TYPE_GENERAL ,
   "add -full_entry" ,
   "prints out the full entry of a tag if there are more than one word"
 } ,
 { 25 , OCT , 2017 , JKR , "@afni_R_package_install" , SUPER , TYPE_NEW_PROG ,
   "add new program @afni_R_package_install" ,
   "Will install R libraries to run shiny apps"
 } ,
 { 11 , OCT , 2017 , JKR , "@FATCAT_heatmap_shiny" , SUPER , TYPE_NEW_PROG ,
   "add new program @FATCAT_heatmap_shiny" ,
   "Run a shiny app to view .netcc or .grid files"
 } ,
 { 11 , OCT , 2017 , JKR , "@ClustExp_CatLab" , SUPER , TYPE_NEW_PROG ,
   "add new program @ClustExp_CatLab" ,
   "Part of cluster explorer. Concatenates and labels input datasets"
 } ,
 { 11 , OCT , 2017 , JKR , "ClustExp_HistTable.py" , SUPER , TYPE_NEW_PROG ,
   "add new program ClustExp_HistTable.py" ,
   "Part of cluster explorer. Extract data tables from the history of datasets"
 } ,
 { 11 , OCT , 2017 , JKR , "ClustExp_StatParse.py" , SUPER , TYPE_NEW_PROG ,
   "add new program ClustExp_StatParse.py" ,
   "Part of cluster explorer. Extract subject level data from clusters and\n"
   "output tables and a shiny app"
 } ,
 { 11 , OCT , 2017 , JKR , "@ClustExp_run_shiny" , SUPER , TYPE_NEW_PROG ,
   "add new program @ClustExp_run_shiny" ,
   "Part of cluster explorer. Run the shiny app output from ClustExp_StatParse.py"
 } ,
 { 10 , APR , 2017 , JKR , "3dTstat" , MINOR , TYPE_GENERAL ,
   "add -tsnr" ,
   "same as -cvarinvNOD"
 } ,
 { 29 , MAR , 2017 , JKR , "prompt_popup" , MINOR , TYPE_NEW_PROG ,
   "add new program prompt_popup" ,
   "Similar to prompt_user, but adds label customization and up to 3 buttons"
 } ,
 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
