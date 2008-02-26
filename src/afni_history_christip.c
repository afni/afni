
/** cf. afni_history.h **/

#include "afni_history.h"

/*  (for starting a new file, search for CHANGE)

    basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long descrption with intermediate newlines
  
    copy entire section: { ... } ,
  
    Notes: - months are JAN ... DEC (see afni_history.h)

           - levels are :
                    MICRO   - users don't see
                    MINOR   - small affect on users
                    MAJOR   - large affect on users
                    SUPER   - we expect users to know

           - these will probably replace AFNI.changes.*

           - PLEASE, stick to what fits on an 80 column terminal

           - it may be nice to put the newest entires at the top

           - leave the last "99, NULL" entry as it is

 -- example --

 { 26 , FEB , 2008 , rickr , "my_program" , MAJOR ,
   "short description of change" ,
   "(optional) description of where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

afni_history_struct christip_history[] = {

 /* CHANGE: copy and change the above 6 line example to here */

 { 99,99,99, NULL,NULL, 99, NULL,NULL}  /** the end (do not delete) **/
} ;
