
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

 { 30 , FEB , 2008 , GC , "3dLME.R" , SUPERDUPER , TYPE_NEW_OPT ,
   "added the ability for it to write the paper, too" ,
   NULL
 } ,

 { 19 , FEB , 2008 , GC , "my_program" , SUPER , TYPE_NEW_PROG ,
   "short description of change" ,
   "(optional) description of where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

afni_history_struct gangc_history[] = {

  /* can copy to and change one of the examples here */
  
  { 29 , FEB , 2008 , GC , "3dICA.R" , SUPERDUPER , TYPE_NEW_PROG ,
    "An R program that runs independent component analysis in AFNI.\n",	

    "This testing program for ICA only takes one dataset that presumably has already \n"
    "been properly pre-processed. See more details on: \n"
    "http://afni.nimh.nih.gov/sscc/gangc/ica.html" 
  } ,
  
  { 29 , FEB , 2008 , GC , "3dLME.R" , SUPERDUPER , TYPE_NEW_PROG ,
    "An R program that runs linear mixed-effects analysis at group level in AFNI.\n",
	 
    "See more details on: http://afni.nimh.nih.gov/sscc/gangc/lme.html" 
  } ,

 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
