
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
  
  { 16 , DEC , 2008 , GC , "3dGC.R" , MAJOR , TYPE_NEW_PROG ,
    "An R program that runs Granger causality analysis of a seed region versus the\n
	 "rest of the brain.\n",	

    "See more details at http://afni.nimh.nih.gov/sscc/gangc/3dGC" 
  } ,
  
  { 9 , OCT , 2008 , GC , "1dGC.R" , SUPER , TYPE_NEW_PROG ,
    "An R program that runs Granger causality analysis among a few pre-select regions.\n",	

    "See more details at http://afni.nimh.nih.gov/sscc/gangc/1dGC" 
  } ,
  
  { 29 , FEB , 2008 , GC , "3dICA.R" , SUPER , TYPE_NEW_PROG ,
    "An R program that runs independent component analysis in AFNI.\n",	

    "This testing program for ICA only takes one dataset that presumably has \n"
    "already been properly pre-processed. See more details at \n"
    "http://afni.nimh.nih.gov/sscc/gangc/ica.html" 
  } ,
  
  { 20 , SEP , 2007 , GC , "3dLME.R" , SUPER , TYPE_NEW_PROG ,
    "An R program that runs linear mixed-effects analysis at group level in AFNI.\n",
	 
    "See more details at http://afni.nimh.nih.gov/sscc/gangc/lme.html" 
  } ,
  
  { 17 , JAN , 2007 , GC , "1dSEM" , SUPER , TYPE_NEW_PROG ,
    "An AFNI program that runs path analysis (or structural equation modeling) at \n"
	 "group level.\n",
	 
    "See more details on: http://afni.nimh.nih.gov/sscc/gangc/PathAna.html" 
  } ,
  
  { 1 , DEC , 2005 , GC , "3dANOVA3" , MAJOR , TYPE_NEW_OPT ,
    "New options were made to run 2nd-order general linear contrasts in 3dANOVA3. \n",
	 
    "See more details on: http://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html" 
  } ,
  
  { 23 , SEP , 2005 , GC , "3dANOVA2 and 3dANOVA3" , SUPER , TYPE_BUG_FIX ,
    "Significant modifications were made to avoid statistics inflation for \n"
	 "general linear contrasts when coefficients don't add up to 0. \n",
	 
    "See more details on: http://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html" 
  } ,
  
  { 27 , JUL , 2004 , GC , "PathAna" , SUPER , TYPE_NEW_PROG ,
    "A Matlab package that runs group analysis of up to 5-way ANOVA \n",
	 
    "This packages adopts the conventional ANOVA approach to handling gorup \n"
	 "analysis. And it requires Statistics Toolbox other than the basic Matlab.\n"
	 "See more details on: http://afni.nimh.nih.gov/sscc/gangc" 
  } ,
  
  { 27 , JUL , 2004 , GC , "IndiAna" , SUPER , TYPE_NEW_PROG ,
    "A Matlab package that individual subject analysis in an interactive fashion.\n",
	 
	 "See more details on: http://afni.nimh.nih.gov/sscc/gangc" 
  } ,
  

 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
