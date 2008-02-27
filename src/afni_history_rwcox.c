
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

afni_history_struct rwcox_history[] = {

 { 16 , FEB , 2008 , RWC , "3dTfitter" , MAJOR ,
   "new program = linear fits to voxel time series" ,
   "Uses L1 or L2 regression, with optional constraints to fit each voxel\n"
   "time series as a sum of basis time series, which can be 1D files or\n"
   "3D+time datasets.  Basis time series that are 1D time series are\n"
   "the same for all input voxels.  Basis time series that are 3D+time\n"
   "datasets are different for each voxel.\n"
   "Differences from 3dDeconvolve:\n"
   "* Basis time series can vary across voxels.\n"
   "* Fit coefficients can be found with L1 or L2 error functions, and\n"
   "  can be constrained to be positive or negative.\n"
   "* 3dTfitter does not compute goodness-of-fit statistics.\n" } ,

 { 20 , FEB , 2008 , RWC , "1deval" , MINOR ,
   "add '-1D:' option, to write output that is usable on the command line" ,
   "Sample usage:\n"
   " 1dplot `1deval -1D: -num 71 -expr 'cos(t/2)*exp(-t/19)'`\n"
   "The backquotes `...` capture command's output and put this string on\n"
   "the command line.  The '-1D:' option formats the 1deval output so that\n"
   "it is ready to be used in this way.\n" } ,

 { 22 , FEB , 2008 , RWC , "3dpc" , MINOR ,
   "add '-eigonly' and '-reduce' options; output eigenvalues to a 1D file"  ,
   "'-eigonly' causes 3dpc to print eigenvalues to stdout and stop there.\n"
   "'-reduce n pp' outputs a reduced dataset, using only the largest 'n'\n"
   "eigenvalues.\n" } ,

 { 25 , FEB , 2008 , RWC , "1dsvd" , MINOR ,
   "add '-vmean' and '-vnorm' options, to mirror capabilities in 3dpc" ,
   NULL } ,

 { 27 , FEB , 2008 , RWC , "3dTfitter" , MAJOR ,
   "add deconvolution via the '-FALTUNG' option" ,
   "Unlike 3dDeconvolve, this deconvolution is to find the input time\n"
   "series, given the impulse response function.\n" } ,

 { 99,99,99, NULL,NULL, 99, NULL,NULL}  /** the end */
} ;
