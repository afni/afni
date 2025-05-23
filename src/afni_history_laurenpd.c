
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

afni_history_struct laurenpd_history[] = {

 /* can copy to and change one of the examples here */
 { 21, May, 2025, PDL, "suma", MAJOR, TYPE_MODIFY,
   "Made alpha variable opacity apply to all surfaces",
   NULL
 } ,

 { 12, Mar, 2025, PDL, "suma", MAJOR, TYPE_MODIFY,
   "make |T|, 'sym I' and 'shw 0' apply to all surfaces",
   NULL
 } ,

 { 24, May, 2024, PDL, "suma", MAJOR, TYPE_NEW_OPT,
   "add alpha thresholding",
   "Fixed certain problems with variable transparency and\n"
   "made functions apply to all surfaces."
 } ,

 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
