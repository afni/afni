
/** cf. afni_history.h **/

#include "afni_history.h"

/*  basic format: 3-field date, user, program_name, impact_level,
                  short description without newline
                  (optional) long descrption with intermediate newlines
  
    copy entire section: { ... } ,
  
    Notes: - months are JAN ... DEC (see .h file)

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

 { 26 , FEB , 2008 , RCR , "my_program" , MAJOR , TYPE_GENERAL ,
   "short description of change" ,
   "(optional) detailed description, or where to get more information\n"
   "   - with newlines, if you babble for multiple lines\n"
   "   (but none at the end)"
 } ,

*/

afni_history_struct rickr_history[] = {

 { 10, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_NEW_ENV,
   "AFNI_GIFTI_VERB sets the verbose level in the gifti I/O library",
   "The default is 1, 0 is quiet, and values go up to 7."
 } ,

 { 10, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_GENERAL,
   "AFNI can read/write .gii.dset as with .gii",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "Dimon", MINOR, TYPE_NEW_OPT,
   "apply -gert_outdir in the case of dicom images",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "Dimon", MINOR, TYPE_MODIFY,
   "if only 1 run, GERT_Reco_dicom is named per run",
   NULL
 } ,

 { 10, MAR, 2008, RCR, "SUMA_SurfMeasures", MAJOR, TYPE_BUG_FIX,
   "averages did not include nodes lost to -cmask",
   "Noticed by M Beauchamp."
 } ,

 {  7, MAR, 2008, RCR, "make_stim_times.py", MINOR, TYPE_BUG_FIX,
   "properly ignore empty lines, and exit on short files",
   NULL
 } ,

 {  6, MAR, 2008, RCR, "GIFTI", MICRO, TYPE_MODIFY,
   "allow functional control over GIFTI encoding" ,
   NULL
 } ,

 {  5, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_BUG_FIX,
   "fixed passing of INDEX_LIST" ,
   NULL
 } ,

 {  5, MAR, 2008, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "do not duplicate data when reading and writing GIFTI from AFNI",
   NULL
 } ,

 {  4, MAR, 2008, RCR, "3dTstat", MINOR, TYPE_NEW_OPT,
   "added -accumulate option, to output each partial sum" ,
   "for k = 0..N-1 : output[k] = sum(input[i]) over i = 0..k"
 } ,

 {  3, MAR, 2008, RCR, "website", MINOR, TYPE_MODIFY,
   "updated the AFNI History website pages, with a table of entries",
   NULL
 } ,

 { 29, FEB, 2008, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added a TYPE, -type, a new level, and a string to identify each level",
   NULL
 } ,

 { 28, FEB, 2008, RCR, "afni_history", MINOR, TYPE_NEW_OPT,
   "added -list_authors option and adjusted spacing",
   NULL
 } ,

 { 27, FEB, 2008, RCR, "afni_history", SUPER, TYPE_NEW_PROG,
   "program to display the history of AFNI updates" ,
   "This will be used to create a web page of AFNI updates.\n"
   "Please see 'afni_history -help' for more details."
 } ,

 { 27, FEB, 2008, RCR, "afni_proc.py", MINOR, TYPE_BUG_FIX,
   "fixed -regress_use_stim_files typo (was -regress_use_stim_times)", 
   NULL
 } ,

 { 26, FEB, 2008, RCR, "afni_history", MICRO, TYPE_GENERAL,
   "checked in initial afni_history files", 
   NULL
 } ,

 { 25, FEB, 2008, RCR, "plug_vol2surf", MAJOR, TYPE_BUG_FIX,
   "fixed application of cluster for sending data to suma",

   "Previously, clustering was only applied when the Olay and Thr sub-bricks\n"
   "were the same."
 } ,

 { 24, FEB, 2008, RCR, "GIFTI", MINOR, TYPE_MODIFY,
   "GIFTI library now considers MetaData without Value as valid",

   NULL
 } ,

 /* pre-afni_history updates, mostly new programs and data formats */

 { 21, FEB, 2008, RCR, "GIFTI", SUPER, TYPE_GENERAL,
   "AFNI programs can now read and write GIFTI datasets",

   "GIFTI datasets are for data in the surface domain, with file suffix .gii.\n"
   "Support must be requested at compile time, and it requires libexpat.\n"
   "Please see http://www.nitrc.org/projects/gifti for many details."
 } ,

 {  6, FEB, 2008, RCR, "3dbucket", MINOR, TYPE_GENERAL,
   "modified to copy FDR curves",
   NULL
 } ,

 {  22, JAN, 2008, RCR, "afni_proc.py", MINOR, TYPE_NEW_OPT,
   "added options to estimate smoothness in data for use in AlphaSim",
   "See help options -regress_est_blur_epits and -regress_est_blur_errts."
 } ,

 {  28, DEC, 2007, RCR, "gifti_tool", MAJOR, TYPE_NEW_PROG,
   "program to read and write GIFTI datasets",
   NULL
 } ,

 {   3, DEC, 2007, RCR, "GIFTI", MAJOR, TYPE_GENERAL,
   "initial release of gifti I/O C API",
   NULL
 } ,

 {  31, AUG, 2007, RCR, "DECONFLICT", MAJOR, TYPE_MODIFY,
   "modified default behavior of programs from deconflict to no overwrite",
   "See AFNI_DECONFLICT in README.environment."
 } ,

 {  31, AUG, 2007, RCR, "model_conv_diffgamma", MINOR, TYPE_NEW_PROG,
   "NLfim model to compute the convolution of the difference of gammas",
   NULL
 } ,

 {  30, JUL, 2007, RCR, "regression_tests", MAJOR, TYPE_GENERAL,
   "added setup for regression testing to NIFTI package",
   "This can be used as a template for testing any command-line programs."
 } ,

 {  20, DEC, 2006, RCR, "afni_proc.py", SUPER, TYPE_NEW_PROG,
   "program to write complete single subject FMRI processing script", 
   NULL
 } ,

 {  11, DEC, 2006, RCR, "make_stim_times.py", MINOR, TYPE_NEW_PROG,
   "program to convert stim_files to stim_times files",
   NULL
 } ,

 {  22, OCT, 2006, RCR, "model_demri_3", MAJOR, TYPE_NEW_PROG,
   "NLfim model for Dynamic Enhanced MRI", 
   NULL
 } ,

 {  12, OCT, 2006, RCR, "serial_writer", MINOR, TYPE_NEW_PROG,
   "program to send data from a file, pipe or made up to a given serial port",
   NULL
 } ,

 {   8, AUG, 2006, RCR, "C++", MINOR, TYPE_MODIFY,
   "afni program compiles in C++ (effort with Rich and Greg Balls)",
   NULL
 } ,

 {   3, AUG, 2006, RCR, "NI_SURF_DSET", SUPER, TYPE_GENERAL,
   "added a new surface dataset format, with read/write ability in AFNI",
   NULL
 } ,

 {  25, JAN, 2006, RCR, "model_michaelis_menton", MAJOR, TYPE_NEW_PROG,
   "NLfim model function for ethanol studies",
   NULL
 } ,

 {   2, DEC, 2005, RCR, "ANOVA", SUPERDUPER, TYPE_MODIFY,
   "changed variance computations in 3dANOVA programs to not assume sphericity",
   "For details, see http://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html ."
 } ,

 {  11, OCT, 2005, RCR, "3dmaxima", MAJOR, TYPE_NEW_PROG,
   "command-line version of maxima plugin",
   NULL
 } ,

 {   5, JUL, 2005, RCR, "Dimon", SUPER, TYPE_NEW_PROG,
   "program to monitor real-time acquisition of DICOM images",
   NULL
 } ,

 {  25, APR, 2005, RCR, "NIFTI", SUPER, TYPE_GENERAL,
   "AFNI can read and write NIFTI datasets (effort with Bob and Rich)",
   NULL
 } ,

 {  7, JAN, 2005, RCR, "nifti_tool", SUPER, TYPE_NEW_PROG,
   "program to directly manipulate or compare NIFTI dataset headers",
   NULL
 } ,

 {  7, JAN, 2005, RCR, "NIFTI", SUPER, TYPE_GENERAL,
   "initial release of NIFTI library",
   NULL
 } ,

 {  4, OCT, 2004, RCR, "vol2surf", SUPER, TYPE_GENERAL,
   "added vol2surf interface for real-time mapping from afni to suma",
   NULL
 } ,

 { 31, MAR, 2004, RCR, "serial_helper", MAJOR, TYPE_NEW_PROG,
   "program to pass realtime registration params from TCP to serial port",
   NULL
 } ,

 {  1, DEC, 2003, RCR, "SurfMeasures", MAJOR, TYPE_NEW_PROG,
   "program to compute various measures over surfaces",
   NULL
 } ,

 {  5, AUG, 2003, RCR, "3dVol2Surf", SUPER, TYPE_NEW_PROG,
   "program to map data from the volume to domain to the surface domain",
   NULL
 } ,

 { 29, MAY, 2003, RCR, "3dSurf2Vol", SUPER, TYPE_NEW_PROG,
   "program to map data from the surface domain to the volume domain",
   NULL
 } ,

 { 15, JAN, 2003, RCR, "Imon", MAJOR, TYPE_NEW_OPT,
   "added connection to the realtime plugin in afni",
   NULL
 } ,

 { 27, NOV, 2002, RCR, "Imon", SUPER, TYPE_NEW_PROG,
   "program to monitor GE I-files as they are written to the scanner",
   NULL
 } ,

 {  1, OCT, 2002, RCR, "file_tool", SUPER, TYPE_NEW_PROG,
   "program to perform generic manipulations of binary files",
   NULL
 } ,

 { 20, JUN, 2002, RCR, "@make_stim_file", MINOR, TYPE_NEW_PROG,
   "script to create binary stim files",
   NULL
 } ,

 {  6, JUN, 2002, RCR, "@SUMA_Make_Spec_FS", MAJOR, TYPE_NEW_PROG,
   "script to import FreeSurfer surfaces into SUMA",
   NULL
 } ,

 { 21, MAY, 2002, RCR, "3dresample", SUPER, TYPE_NEW_PROG,
   "program to change a dataset orientation and/or grid spacing",
   NULL
 } ,

 {  8, MAR, 2002, RCR, "plug_crender", SUPER, TYPE_NEW_PROG,
   "added rendering plugin to afni",
   NULL
 } ,

 { 99,99,99, NULL,NULL, 99,99, NULL,NULL}  /** the end (do not delete) **/
} ;
