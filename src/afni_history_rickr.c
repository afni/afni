
/** cf. afni_history.h **/

#include "afni_history.h"

/*  basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long descrption with intermediate newlines
  
    copy entire section: { ... } ,
  
    Notes: - months are JAN ... DEC (see .h file)

           - levels are :
                    MICRO   - users don't see
                    MINOR   - small affect on users
                    MAJOR   - large affect on users
                    SUPER   - we expect users to know

           - these will probably replace AFNI.changes.*

           - PLEASE, stick to what fits on an 80 column screen

           - it may be nice to put the newest entires at the top

           - leave the last entry as it is

 -- example --

 { 26 , FEB , 2008 , RCR , "my_program" , MAJOR ,
   "short description of change" ,
   "(optional) detailed description, or where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

afni_history_struct rickr_history[] = {

 { 26, FEB, 2008, RCR, "afni_history", MICRO,
   "checked in initial afni_history files", 
   NULL
 } ,

 { 25, FEB, 2008, RCR, "plug_vol2surf", MAJOR,
   "fixed application of cluster for sending data to suma",

   "Previously, clustering was only applied when the Olay and Thr sub-bricks\n"
   "  were the same."
 } ,

 { 24, FEB, 2008, RCR, "GIFTI", MINOR,
   "GIFTI library now considers MetaData without Value as valid",

   NULL
 } ,

 { 21, FEB, 2008, RCR, "GIFTI", MAJOR,
   "AFNI programs can now read and write GIFTI datasets",

   "GIFTI datasets are for data in the surface domain, with file suffix .gii.\n"
   "Support must be requested at compile time, and it requires libexpat.\n"
   "Please see http://www.nitrc.org/projects/gifti for many details."
 } ,

 {  6, FEB, 2008, RCR, "3dbucket", MINOR,
   "modified to copy FDR curves",
   NULL
 } ,

 { 99,99,99, NULL,NULL, 99, NULL,NULL}  /** the end (do not delete) **/
} ;
