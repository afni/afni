/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifdef USE_SUNPERF       /** for Solaris **/
# include <sunperf.h>
#endif

/*
   This program calculates a nonlinear regression for each voxel of the input
   AFNI 3d+time data set.  The nonlinear regression is calculated by means of
   a least squares fit to the signal plus noise models which are specified 
   by the user.
 
   File:     3dNLfim.c
   Author:   B. Douglas Ward
   Date:     19 June 1997

   Mod:      Added external time reference capability (Rongyan Zhang)
   Date:     10 August 1997

   Mod:      Added option for absolute noise parameter constraints.
   Date:     22 August 1997

   Mod:      Added options for percent signal change above baseline, and
             calculation of area under the signal above baseline.
   Date:     26 November 1997

   Mod:      Print error message if user fails to specify the signal model or 
             the noise model.
   Date:     26 December 1997

   Mod:      Extensive changes required to implement the 'bucket' dataset.
   Date:     14 January 1998

   Mod:      Added the -inTR option.
             22 July 1998 -- RWCox

   Mod:      Incorporated THD_extract_series routine.
   Date:     19 April 1999

   Mod:      Added -sfit and -snfit options to write out the signal and
             the signal+noise model time series fit for each voxel 
             to a 3d+time dataset.
   Date:     08 July 1999

   Mod:      Added novar flag to eliminate unnecessary calculations.
   Date:     13 July 1999

   Mod:      Added changes for incorporating History notes.
   Date:     09 September 1999

   Mod:      Adjust F-statistics if parameter constraints force a parameter
             to be a constant.
   Date:     08 February 2000

   Mod:      Changes for output of R^2 (coefficient of multiple determination),
             and standard deviation of residuals from full model fit.
             Added global variable calc_tstats.
             Also, added screen display of p-values.
   Date:     10 May 2000

   Mod:      Corrected error in initializing of output data type (MRI_short)
             in routine write_3dtime.
   Date:     17 May 2000

   Mod:      Added -mask option.  (Adapted from: 3dpc.c)
   Date:     18 May 2000

   Mod:      Changed "return" at end of program to exit(0).  Also, increased
             maximum number of model parameters.
   Date:     08 August 2001

   Mod:      Added call to AFNI_logger.
   Date:     15 August 2001

   Mod:      Changes to allow -jobs option.
   Date:     07 May 2003 - RWCox.

   Mod:      Added options -aux_name, -aux_fval and -voxel_count.
   Date:     25 Jan 2006 [rickr]

   Mod:      Removed options -aux_name and -aux_fval, and the globals
             require linking to afni, too.
   Date:     30 Jan 2006 [rickr]

   Mod:      Added NEWUOA stuff (mostly to simplex.c, actually).
   Date:     20 Jul 2006 [RWCox]

   Mod:      Copied memmap code from 3dDeconvolve.c
   Date:     24 Oct 2006 [DRG]

   Mod:      Limit reports to nth voxels via progress option
   Date:     25 Oct 2006 [DRG]

   Mod:      Limit g_voxel_count reports to every 10th voxel.
   Date:     26 Oct 2006 [rickr]
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dNLfim"                       /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "19 June 1997"    /* date of initial program release */
#define PROGRAM_LATEST  "24 Oct 2006 - DRG"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define DEFAULT_NRAND 19999
#define DEFAULT_NBEST     9
#define DEFAULT_FDISP   999.0
#define DEFAULT_PROGRESS 10000

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mrilib.h"

static int do_FDR = 1 ;  /* 07 Oct 2008 */

/*---------------------------------------------------------------------------*/
/*--------- Global variables for multiple process execution - RWCox. --------*/
/*--------- All names start with "proc_", so search for that string. --------*/

#if !defined(DONT_USE_SHM) && !defined(DONT_USE_FORK) && !defined(CYGWIN)

# include "thd_iochan.h"                /* prototypes for shm stuff */

# define PROC_MAX   32                  /* max num processes */

  static int proc_numjob        = 1   ; /* num processes */
  static pid_t proc_pid[PROC_MAX]     ; /* IDs of processes */
  static int proc_shmid         = 0   ; /* shared memory ID */
  static char *proc_shmptr      = NULL; /* pointer to shared memory */
  static long long proc_shmsize       = 0   ; /* total size of shared memory */

  static int proc_shm_arnum     = 0   ; /* num arrays in shared memory */
  static float ***proc_shm_ar   = NULL; /* *proc_shm_ar[i] = ptr to array #i */
  static int *proc_shm_arsiz    = NULL; /* proc_shm_arsiz[i] = floats in #i */

  static int proc_vox_bot[PROC_MAX]   ; /* 1st voxel to use in each process */
  static int proc_vox_top[PROC_MAX]   ; /* last voxel (+1) in each process */

  static int proc_ind = 0             ; /* index of THIS job */

#else   /* can't use multiple processes */

# define proc_numjob 1   /* flag that only 1 process is in use */
# define proc_ind    0   /* index of THIS job */

#endif

/*---------------------------------------------------------------------------*/

#include "matrix.h"
#include "simplex.h"
#include "NLfit.h"

#include "simplex.c"   /* 20 Jul 2006 - now includes NEWUOA variables [N_*] */
#include "NLfit.c"

typedef struct NL_options
{ 
  char * bucket_filename;      /* file name for bucket dataset */
  int numbricks;               /* number of sub-bricks in bucket dataset */
  int * brick_type;            /* indicates type of sub-brick */
  int * brick_coef;            /* regression coefficient number for sub-brick*/
  char ** brick_label;         /* character string label for sub-brick */

} NL_options;


/*---------------------------------------------------------------------------*/
/*
  Global data 
*/

/***** 22 July 1998 -- RWCox:
       Modified to allow DELT to be set from the TR of the input file *****/

static float DELT = 1.0;        /* default */
static int   inTR = 0 ;         /* set to 1 if -inTR option is used */
static float dsTR = 0.0 ;       /* TR of the input file */
static float uuTR = 0.0 ;
static int   g_voxel_count = 0; /* display current voxel counter */
                                         /* 25 Jan 2006 [rickr]  */

static char * commandline = NULL ;       /* command line for history notes */

static byte * mask_vol  = NULL;          /* mask volume */
static int    mask_nvox = 0;             /* number of voxels in mask volume */
static int    output_datum = ILLEGAL_TYPE ;

/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dNLfim help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program calculates a nonlinear regression for each voxel of the  \n"
     "input AFNI 3d+time data set.  The nonlinear regression is calculated  \n"
     "by means of a least squares fit to the signal plus noise models which \n"
     "are specified by the user.                                            \n"
     "                                                                      \n"
     "Usage:                                                                \n"
     "3dNLfim                                                               \n"
     "-input fname       fname = filename of 3d + time data file for input  \n"
     "[-mask mset]       Use the 0 sub-brick of dataset 'mset' as a mask    \n"
     "                     to indicate which voxels to analyze (a sub-brick \n"
     "                     selector is allowed)  [default = use all voxels] \n"
     "[-ignore num]      num   = skip this number of initial images in the  \n"
     "                     time series for regresion analysis; default = 0  \n"
     "               ****N.B.: default ignore value changed from 3 to 0,    \n"
     "                         on 04 Nov 2008 (BHO day).                    \n"
     "[-inTR]            set delt = TR of the input 3d+time dataset         \n"
     "                     [The default is to compute with delt = 1.0 ]     \n"
     "                     [The model functions are calculated using a      \n"
     "                      time grid of: 0, delt, 2*delt, 3*delt, ... ]    \n"
     "[-TR delt]         directly set the TR of the time series model;      \n"
     "                     can be useful if the input file is a .1D file    \n"
     "                     (transposed with the \\' operator)               \n"
     "[-time fname]      fname = ASCII file containing each time point      \n"
     "                     in the time series. Defaults to even spacing     \n"
     "                     given by TR (this option overrides -inTR).       \n"
     "-signal slabel     slabel = name of (non-linear) signal model         \n"
     "-noise  nlabel     nlabel = name of (linear) noise model              \n"
     "-sconstr k c d     constraints for kth signal parameter:              \n"
     "                      c <= gs[k] <= d                                 \n"
     "                 **N.B.: It is important to set the parameter         \n"
     "                         constraints with care!                       \n"
     "                 **N.B.: -sconstr and -nconstr options must appear    \n"
     "                         AFTER -signal and -noise on the command line \n"
     "-nconstr k c d     constraints for kth noise parameter:               \n"
     "                      c+b[k] <= gn[k] <= d+b[k]                       \n"
     "[-nabs]            use absolute constraints for noise parameters:     \n"
     "                     c <= gn[k] <= d  [default=relative, as above]    \n"
     "[-nrand n]         n = number of random test points [default=%d]      \n"
     "[-nbest b]         b = use b best test points to start [default=%d]   \n"
     "[-rmsmin r]        r = minimum rms error to reject reduced model      \n"
     "[-fdisp fval]      display (to screen) results for those voxels       \n"
     "                     whose f-statistic is > fval [default=%.1f]       \n"
     "[-progress ival]   display (to screen) results for those voxels       \n"
     "                     every ival number of voxels                      \n"
     "[-voxel_count]     display (to screen) the current voxel index        \n"
     "                                                                      \n"
     "--- These options choose the least-square minimization algorithm ---  \n"
     "                                                                      \n"
     "[-SIMPLEX]         use Nelder-Mead simplex method [default]           \n"
     "[-POWELL]          use Powell's NEWUOA method instead of the          \n"
     "                     Nelder-Mead simplex method to find the           \n"
     "                     nonlinear least-squares solution                 \n"
     "                     [slower; usually more accurate, but not always!] \n"
     "[-BOTH]            use both Powell's and Nelder-Mead method           \n"
     "                     [slowest, but should be most accurate]           \n"
     "                                                                      \n"
     "--- These options generate individual AFNI 2 sub-brick datasets ---   \n"
     "--- [All these options must be AFTER options -signal and -noise]---   \n"
     "                                                                      \n"
     "[-freg fname]      perform f-test for significance of the regression; \n"
     "                     output 'fift' is written to prefix filename fname\n"
     "[-frsqr fname]     calculate R^2 (coef. of multiple determination);   \n"
     "                     store along with f-test for regression;          \n"
     "                     output 'fift' is written to prefix filename fname\n"
     "[-fsmax fname]     estimate signed maximum of signal; store along     \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
     "[-ftmax fname]     estimate time of signed maximum; store along       \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
     "[-fpsmax fname]    calculate (signed) maximum percentage change of    \n"
     "                     signal from baseline; output 'fift' is           \n"
     "                     written to prefix filename fname                 \n"
     "[-farea fname]     calculate area between signal and baseline; store  \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
     "[-fparea fname]    percentage area of signal relative to baseline;    \n"
     "                     store with f-test for regression; output 'fift'  \n"
     "                     is written to prefix filename fname              \n"
     "[-fscoef k fname]  estimate kth signal parameter gs[k]; store along   \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
     "[-fncoef k fname]  estimate kth noise parameter gn[k]; store along    \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
     "[-tscoef k fname]  perform t-test for significance of the kth signal  \n"
     "                     parameter gs[k]; output 'fitt' is written        \n"
     "                     to prefix filename fname                         \n"
     "[-tncoef k fname]  perform t-test for significance of the kth noise   \n"
     "                     parameter gn[k]; output 'fitt' is written        \n"
     "                     to prefix filename fname                         \n"
     "                                                                      \n"
     "--- These options generate one AFNI 'bucket' type dataset ---         \n"
     "                                                                      \n"
     "[-bucket n prefixname]   create one AFNI 'bucket' dataset containing  \n"
     "                           n sub-bricks; n=0 creates default output;  \n"
     "                           output 'bucket' is written to prefixname   \n"
     "The mth sub-brick will contain:                                       \n"
     "[-brick m scoef k label]   kth signal parameter regression coefficient\n"
     "[-brick m ncoef k label]   kth noise parameter regression coefficient \n"
     "[-brick m tmax label]      time at max. abs. value of signal          \n"
     "[-brick m smax label]      signed max. value of signal                \n"
     "[-brick m psmax label]     signed max. value of signal as percent     \n"
     "                             above baseline level                     \n"
     "[-brick m area label]      area between signal and baseline           \n"
     "[-brick m parea label]     signed area between signal and baseline    \n"
     "                             as percent of baseline area              \n"
     "[-brick m tscoef k label]  t-stat for kth signal parameter coefficient\n"
     "[-brick m tncoef k label]  t-stat for kth noise parameter coefficient \n"
     "[-brick m resid label]     std. dev. of the full model fit residuals  \n"
     "[-brick m rsqr  label]     R^2 (coefficient of multiple determination)\n"
     "[-brick m fstat label]     F-stat for significance of the regression  \n"
     "\n"
     "[-noFDR]                   Don't write the FDR (q vs. threshold)\n"
     "                           curves into the output dataset.\n"
     "                           (Same as 'setenv AFNI_AUTOMATIC_FDR NO')\n"
     "                                                                      \n"
     "     --- These options write time series fit for ---                  \n"
     "     --- each voxel to an AFNI 3d+time dataset   ---                  \n"
     "                                                                      \n"
     "[-sfit fname]      fname = prefix for output 3d+time signal model fit \n"
     "[-snfit fname]     fname = prefix for output 3d+time signal+noise fit \n"
     "                                                                      \n"
       , DEFAULT_NRAND , DEFAULT_NBEST , DEFAULT_FDISP
    );


#ifdef PROC_MAX
    printf( "\n"
            " -jobs J   Run the program with 'J' jobs (sub-processes).\n"
            "             On a multi-CPU machine, this can speed the\n"
            "             program up considerably.  On a single CPU\n"
            "             machine, using this option is silly.\n"
            "             J should be a number from 1 up to the\n"
            "             number of CPU sharing memory on the system.\n"
            "             J=1 is normal (single process) operation.\n"
            "             The maximum allowed value of J is %d.\n"
            "         * For more information on parallelizing, see\n"
            "             http://afni.nimh.nih.gov/afni/doc/misc/parallize.html\n"
            "         * Use -mask to get more speed; cf. 3dAutomask.\n"
          , PROC_MAX ) ;
#endif
  
    printf(
    "\n"
    "----------------------------------------------------------------------\n"
    "Signal Models (see the appropriate model_*.c file for exact details) :\n"
    "\n"
    "  Null                     : No Signal\n"
    "                             (no parameters)\n"
    "                             see model_null.c\n"
    "\n"
    "  SineWave_AP              : Sinusoidal Response\n"
    "                             (amplitude, phase)\n"
    "                             see model_sinewave_ap.c\n"
    "\n"
    "  SquareWave_AP            : Square Wave Response\n"
    "                             (amplitude, phase)\n"
    "                             see model_squarewave_ap.c\n"
    "\n"
    "  TrnglWave_AP             : Triangular Wave Response\n"
    "                             (amplitude, phase)\n"
    "                             see model_trnglwave_ap.c\n"
    "\n"
    "  SineWave_APF             : Sinusoidal Wave Response\n"
    "                             (amplitude, phase, frequency)\n"
    "                             see model_sinewave_apf.c\n"
    "\n"
    "  SquareWave_APF           : Sinusoidal Wave Response\n"
    "                             (amplitude, phase, frequency)\n"
    "                             see model_squarewave_apf.c\n"
    "\n"
    "  TrnglWave_APF            : Sinusoidal Wave Response\n"
    "                             (amplitude, phase, frequency)\n"
    "                             see model_trnglwave_apf.c\n"
    "\n"
    "  Exp                      : Exponential Function\n"
    "                             (a,b): a * exp(b * t)\n"
    "                             see model_exp.c\n"
    "\n"
    "  DiffExp                  : Differential-Exponential Drug Response\n"
    "                             (t0, k, alpha1, alpha2)\n"
    "                             see model_diffexp.c\n"
    "\n"
    "  GammaVar                 : Gamma-Variate Function Drug Response\n"
    "                             (t0, k, r, b)\n"
    "                             see model_gammavar.c\n"
    "\n"
    "  Beta                     : Beta Distribution Model\n"
    "                             (t0, tf, k, alpha, beta)\n"
    "                             see model_beta.c\n"
    "\n"
    "  ConvGamma2a              : Gamma Convolution with 2 Input Time Series\n"
    "                             (t0, r, b)\n"
    "                             see model_convgamma2a.c\n"
    "\n"
    "  ConvGamma                : Gamma Vairate Response Model\n"
    "                             (t0, amp, r, b)\n"
    "                             see model_convgamma.c\n"
    "\n"
    "  demri_3                  : Dynamic (contrast) Enhanced MRI\n"
    "                             (K_trans, Ve, k_ep)\n"
    "                             see model_demri_3.c\n"
    "                  for help : setenv AFNI_MODEL_HELP_DEMRI_3 YES\n"
    "\n"
    "  ADC                      : Diffusion Signal Model\n"
    "                             (So, D)\n"
    "                             see model_diffusion.c\n"
    "\n"
    "  michaelis_menton         : Michaelis/Menten Concentration Model\n"
    "                             (v, vmax, k12, k21, mag)\n"
    "                             see model_michaelis_menton.c\n"
    "\n"
    "  Expr2                    : generic (3dcalc-like) expression with\n"
    "                             exactly 2 'free' parameters and using\n"
    "                             symbol 't' as the time variable;\n"
    "                             see model_expr2.c for details.\n"
    "\n"
    "----------------------------------------\n"
    "Noise Models (see the appropriate model_*.c file for exact details) :\n"
    "\n"
    "  Zero                     : Zero Noise Model\n"
    "                             (no parameters)\n"
    "                             see model_zero.c\n"
    "\n"
    "  Constant                 : Constant Noise Model\n"
    "                             (constant)\n"
    "                             see model_constant.c\n"
    "\n"
    "  Linear                   : Linear Noise Model\n"
    "                             (constant, linear)\n"
    "                             see model_linear.c\n"
    "\n"
    "  Linear+Ort               : Linear+Ort Noise Model\n"
    "                             (constant, linear, Ort)\n"
    "                             see model_linplusort.c\n"
    "\n"
    "  Quadratic                : Quadratic Noise Model\n"
    "                             (constant, linear, quadratic)\n"
    "                             see model_quadratic.c\n"
    ) ;
  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/
     
/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( NLfit_error ("Cannot allocate memory") )
    

/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  int * ignore,            /* delete this number of points from time series */
  vfp * nmodel,            /* pointer to noise model */
  vfp * smodel,            /* pointer to signal model */  
  int * r,                 /* number of parameters in the noise model */
  int * p,                 /* number of parameters in the signal model */
  char *** npname,         /* noise parameter names */
  char *** spname,         /* signal parameter names */
  float ** min_nconstr,    /* minimum parameter constraints for noise model */
  float ** max_nconstr,    /* maximum parameter constraints for noise model */
  float ** min_sconstr,    /* minimum parameter constraints for signal model */
  float ** max_sconstr,    /* maximum parameter constraints for signal model */
  int * nabs,              /* use absolute constraints for noise parameters */
  int * nrand,             /* number of random vectors to generate */
  int * nbest,             /* number of random vectors to keep */
  float * rms_min,         /* minimum rms error to reject reduced model */
  float * fdisp,           /* minimum f-statistic for display */ 
  int *progress,           /* nth voxel to show report */
  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** tfilename,          /* file name for time point series */  
  char ** freg_filename,      /* file name for regression f-statistics */
  char ** frsqr_filename,     /* file name for R^2 statistics */
  char *** fncoef_filename,   /* file name for noise model parameters */
  char *** fscoef_filename,   /* file name for signal model parameters */
  char *** tncoef_filename,   /* file name for noise model t-statistics */
  char *** tscoef_filename,   /* file name for signal model t-statistics */
  char ** sfit_filename,      /* file name for 3d+time fitted signal model */
  char ** snfit_filename,     /* file name for 3d+time fitted signal+noise */
  NL_options * option_data    /* bucket dataset options */
)
 
{
  int ip;                     /* parameter index */


  /*----- initialize default values -----*/
  *ignore = 0;
  *nabs = 0;
  *nrand = DEFAULT_NRAND;
  *nbest = DEFAULT_NBEST; 
  *rms_min = 0.0;
  *fdisp = DEFAULT_FDISP;
  *progress = DEFAULT_PROGRESS;
  *smodel = NULL;
  *nmodel = NULL;
  *r = -1;
  *p = -1;


  /*----- allocate memory for noise parameter names -----*/
  *npname = (char **) malloc (sizeof(char *) * MAX_PARAMETERS); 
  MTEST (*npname);  
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*npname)[ip] = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
      MTEST ((*npname)[ip]);  
    }


  /*----- allocate memory for signal parameter names -----*/
  *spname = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  MTEST (*spname);  
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*spname)[ip] = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
      MTEST ((*spname)[ip]);  
    }
  

  /*----- allocate memory for parameter constraints -----*/
  *min_nconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  MTEST (*min_nconstr);  
  *max_nconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  MTEST (*max_nconstr);
  *min_sconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  MTEST (*min_sconstr);  
  *max_sconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  MTEST (*max_sconstr);


  /*----- allocate memory space and initialize pointers for filenames -----*/
  *input_filename = NULL;
  *tfilename = NULL;
  *freg_filename = NULL;  
  *frsqr_filename = NULL;
  *fncoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  MTEST (*fncoef_filename);
  *fscoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  MTEST (*fscoef_filename);
  *tncoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  MTEST (*tncoef_filename);
  *tscoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  MTEST (*tscoef_filename);
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*fncoef_filename)[ip] = NULL;
      (*fscoef_filename)[ip] = NULL;
      (*tncoef_filename)[ip] = NULL;
      (*tscoef_filename)[ip] = NULL;
    }
  *sfit_filename = NULL;
  *snfit_filename = NULL;


  /*----- initialize bucket dataset options -----*/
  option_data->bucket_filename = NULL;
  option_data->numbricks = -1;
  option_data->brick_type = NULL;
  option_data->brick_coef = NULL;
  option_data->brick_label = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                /* number of input arguments */
  char ** argv,            /* array of input arguments */ 
  int * ignore,            /* delete this number of points from time series */
  char ** nname,           /* name of noise model */
  char ** sname,           /* name of signal model */
  vfp * nmodel,            /* pointer to noise model */
  vfp * smodel,            /* pointer to signal model */  
  int * r,                 /* number of parameters in the noise model */
  int * p,                 /* number of parameters in the signal model */
  char *** npname,         /* noise parameter names */
  char *** spname,         /* signal parameter names */
  float ** min_nconstr,    /* minimum parameter constraints for noise model */
  float ** max_nconstr,    /* maximum parameter constraints for noise model */
  float ** min_sconstr,    /* minimum parameter constraints for signal model */
  float ** max_sconstr,    /* maximum parameter constraints for signal model */
  int * nabs,              /* use absolute constraints for noise parameters */
  int * nrand,             /* number of random vectors to generate */
  int * nbest,             /* number of random vectors to keep */
  float * rms_min,         /* minimum rms error to reject reduced model */
  float * fdisp,           /* minimum f-statistic for display */ 
  int * progress,          /* progress report every nth voxel */
  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** tfilename,          /* file name for time point series */  
  char ** freg_filename,      /* file name for regression f-statistics */
  char ** frsqr_filename,     /* file name for R^2 statistics */
  char ** fsmax_filename,     /* file name for signal signed maximum */
  char ** ftmax_filename,     /* file name for time of signed maximum */
  char ** fpmax_filename,     /* file name for max. percentage change */
  char ** farea_filename,     /* file name for area under the signal */
  char ** fparea_filename,    /* file name for percent area under the signal */
  char *** fncoef_filename,   /* file name for noise model parameters */
  char *** fscoef_filename,   /* file name for signal model parameters */
  char *** tncoef_filename,   /* file name for noise model t-statistics */
  char *** tscoef_filename,   /* file name for signal model t-statistics */
  char ** sfit_filename,      /* file name for 3d+time fitted signal model */
  char ** snfit_filename,     /* file name for 3d+time fitted signal+noise */

  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  int * nxyz,                       /* number of voxels in image */
  int * ts_length,                  /* length of time series data */  
  NL_options * option_data          /* bucket dataset options */
)

{
  const MAX_BRICKS = 100;           /* max. number of bricks in the bucket */
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[MAX_NAME_LENGTH];    /* error message */
  int ok;                           /* boolean for specified model exists */

  NLFIT_MODEL_array * model_array = NULL;   /* array of SO models */
  int im;                                   /* model index */
  int ibrick;                       /* sub-brick index */
  int nbricks;                      /* number of bricks in the bucket */

  
  /*----- does user request help menu? -----*/
  if (argc < 2 || strcmp(argv[1], "-help") == 0)  display_help_menu();  
  
  
  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 

  
  /*----- initialize the model array -----*/
  model_array = NLFIT_get_many_MODELs ();
  if ((model_array == NULL) || (model_array->num == 0))
    NLfit_error ("Unable to locate any models");

  /*----- initialize the input options -----*/
  initialize_options (ignore, nmodel, smodel, r, p, npname, spname, 
                min_nconstr, max_nconstr, min_sconstr, max_sconstr, nabs,
                nrand, nbest, rms_min, fdisp, progress, 
                input_filename, tfilename, freg_filename, 
                frsqr_filename, fncoef_filename, fscoef_filename,
                tncoef_filename, tscoef_filename,
                sfit_filename, snfit_filename, option_data); 

  if( *ignore == 0 )
    INFO_message("NOTICE: Default value of -ignore is now 0;\n"
                 "           Before 04 Nov 2008, default value was 3" ) ;
  
  /*----- main loop over input options -----*/
  while (nopt < argc )
    {
      /*-----   -noFDR   -----*/

      if( strcasecmp(argv[nopt],"-noFDR") == 0 ){
        do_FDR = 0 ; nopt++ ; continue ;
      }

      /*-----   -input filename   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -input ");
       *input_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*input_filename);
       strcpy (*input_filename, argv[nopt]);
       /* open input dataset - was THD_open_one_dataset, allow sub-bricks */
       *dset_time = THD_open_dataset (*input_filename);  
       if ((*dset_time) == NULL)  
         { 
           sprintf (message, 
                 "Unable to open data file: %s", *input_filename);
           NLfit_error (message);
         }
     DSET_UNMSEC( *dset_time ) ;  /* RWCox */

       DSET_load(*dset_time) ; CHECK_LOAD_ERROR(*dset_time) ;

       *nxyz =  (*dset_time)->dblk->diskptr->dimsizes[0]
         * (*dset_time)->dblk->diskptr->dimsizes[1]
         * (*dset_time)->dblk->diskptr->dimsizes[2] ;
#if 0
       *ts_length = DSET_NUM_TIMES(*dset_time);

       /* verify that we seem to have a time series */
       if( DSET_NVALS(*dset_time) != *ts_length )
          WARNING_message("dataset num_times (%d) != nvals (%d)\n"
                          "   --> no time axis could be a problem!\n",
                          *ts_length, DSET_NVALS(*dset_time));
#else
       *ts_length = DSET_NVALS(*dset_time);
       if( *ts_length > 1 )
         INFO_message("time series length = %d points",*ts_length) ;
       else
         ERROR_exit  ("time series length = %d points",*ts_length) ;
#endif

     dsTR = DSET_TIMESTEP(*dset_time) ;

     if(output_datum==ILLEGAL_TYPE) {   /* if output_datum type is not specified by user*/
        output_datum = DSET_BRICK_TYPE(*dset_time,0);  /* get datum type from dataset */
     }	
       nopt++;
       continue;
     }
      /**** -datum type ****/

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc )
           NLfit_error("need an argument after -datum!\n") ;
         if( strcmp(argv[nopt],"short") == 0 ){
            output_datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            output_datum = MRI_float ;
         } else {
            ERROR_exit("-datum of type '%s' not supported in 3dNLfim!\n",argv[nopt]) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }


      /**** -mask mset [18 May 2000] ****/

      if( strcmp(argv[nopt],"-mask") == 0 ){
         THD_3dim_dataset * mset ; int ii,mc ;
         nopt++ ;
         if (nopt >= argc)  NLfit_error ("need argument after -mask!") ;
         mset = THD_open_dataset( argv[nopt] ) ;
         if (mset == NULL)  NLfit_error ("can't open -mask dataset!") ;

         mask_vol = THD_makemask( mset , 0 , 1.0,0.0 ) ;
         mask_nvox = DSET_NVOX(mset) ;
         DSET_delete(mset) ;

         if (mask_vol == NULL )  NLfit_error ("can't use -mask dataset!") ;
         for( ii=mc=0 ; ii < mask_nvox ; ii++ )  if (mask_vol[ii])  mc++ ;
         if (mc == 0)  NLfit_error ("mask is all zeros!") ;
         printf("++ %d voxels in mask %s\n",mc,argv[nopt]) ;
         nopt++ ; continue ;
      }


      /*----- 22 July 1998: the -inTR option -----*/

      if( strcmp(argv[nopt],"-inTR") == 0 ){
         inTR = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-TR") == 0 ){
        uuTR = (float)strtod(argv[++nopt],NULL) ;
        if( uuTR <= 0.0f )
          ERROR_message("value after -TR is not positive!") ;
        nopt++ ; continue ;
      }

      /*-----   -ignore num  -----*/
      if (strcmp(argv[nopt], "-ignore") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -ignore ");
       sscanf (argv[nopt], "%d", &ival);
       if (ival < 0)
         NLfit_error ("illegal argument after -ignore ");
       *ignore = ival;
       nopt++;
       continue;
     }
      
      /*-----   -time filename   -----*/
      if (strcmp(argv[nopt], "-time") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -time ");
       *tfilename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*tfilename);
       strcpy (*tfilename, argv[nopt]);
       nopt++;
       continue;
     }
      
      /*-----   -nabs  -----*/
      if (strcmp(argv[nopt], "-nabs") == 0)
     {
       *nabs = 1;
       nopt++;
       continue;
     }

      /*-----  -POWELL  -----*/

      if( strcmp(argv[nopt],"-POWELL") == 0 ){   /* 20 Jul 2006 */
        N_newuoa = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-SIMPLEX") == 0 ){
        N_newuoa = 0 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-BOTH") == 0 ){
        N_newuoa = 2 ; nopt++ ; continue ;
      }
      
      
      /*-----   -signal slabel  -----*/
      if (strcmp(argv[nopt], "-signal") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -signal ");
       *sname = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*sname);
       strcpy (*sname, argv[nopt]);
       initialize_signal_model (model_array, *sname, 
                       smodel, p, *spname,
                       *min_sconstr, *max_sconstr);
       nopt++;
       continue;
     }
      
      
      /*-----   -noise nlabel  -----*/
      if (strcmp(argv[nopt], "-noise") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -noise ");
       *nname = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*nname);
       strcpy (*nname, argv[nopt]);
       initialize_noise_model (model_array, *nname, 
                      nmodel, r, *npname,
                      *min_nconstr, *max_nconstr);
       nopt++;
       continue;
     }
      
      
      /*-----   -nrand n  -----*/
      if (strcmp(argv[nopt], "-nrand") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -nrand ");
       sscanf (argv[nopt], "%d", &ival);
       if (ival <= 0)
         NLfit_error ("illegal argument after -nrand ");
       *nrand = ival;
       nopt++;
       continue;
     }
      
      
      /*-----   -nbest n  -----*/
      if (strcmp(argv[nopt], "-nbest") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -nbest ");
       sscanf (argv[nopt], "%d", &ival);
       if (ival <= 0)
         NLfit_error ("illegal argument after -nbest ");
       *nbest = ival;
       nopt++;
       continue;
     }
      
      
      /*-----   -rmsmin r  -----*/
      if (strcmp(argv[nopt], "-rmsmin") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -rmsmin ");
       sscanf (argv[nopt], "%f", &fval); 
       if (fval < 0.0)
         NLfit_error ("illegal argument after -rmsmin ");
       *rms_min = fval;
       nopt++;
       continue;
     }
      

       /*-----   -fdisp fval   -----*/
      if (strcmp(argv[nopt], "-fdisp") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -fdisp ");
       sscanf (argv[nopt], "%f", &fval); 
       *fdisp = fval;
       nopt++;
       continue;
     }
      
       /*-----   -progress ival   -----*/
      if (strcmp(argv[nopt], "-progress") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -progress ");
       sscanf (argv[nopt], "%d", &ival); 
       if (ival < 1)
          NLfit_error("illegal argument after -progress ");
       *progress = ival;
       nopt++;
       continue;
     }

       /*-----   -voxel_count   -----*/
      if (strcmp(argv[nopt], "-voxel_count") == 0)
     {
       nopt++;
       g_voxel_count = 1;
       continue;
     }

      /*-----   -jobs J   -----*/
      if( strcmp(argv[nopt],"-jobs") == 0 ){   /* RWCox */
        nopt++ ;
        if (nopt >= argc)  NLfit_error ("need J parameter after -jobs ");
#ifdef PROC_MAX
        proc_numjob = strtol(argv[nopt],NULL,10) ;
        if( proc_numjob < 1 ){
          fprintf(stderr,"** setting number of processes to 1!\n") ;
          proc_numjob = 1 ;
        } else if( proc_numjob > PROC_MAX ){
          fprintf(stderr,"** setting number of processes to %d!\n",PROC_MAX);
          proc_numjob = PROC_MAX ;
        }
#else
        fprintf(stderr,"** -jobs not supported in this version\n") ;
#endif
        nopt++; continue;
      }


      /*----- check that user has specified the signal and noise models -----*/
      if ((*smodel) == NULL)  ERROR_exit("Must specify signal model before '%s'",argv[nopt]);
      if ((*nmodel) == NULL)  ERROR_exit("Must specify noise model before '%s'" ,argv[nopt]);
      
      /*-----   -sconstr k min max  -----*/
      if (strcmp(argv[nopt], "-sconstr") == 0)
     {
       nopt++;
       if (nopt+2 >= argc)  NLfit_error("need 3 arguments after -sconstr ");

       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival >= *p)) {
        fprintf(stderr,"*p = %d, ival = %d\n", *p, ival);
        ERROR_exit("Illegal 'k' index after -sconstr: legal range is 0..%d",*p-1) ;
      }
       index = ival;
       nopt++;

       sscanf (argv[nopt], "%f", &fval); 
       (*min_sconstr)[index] = fval;
       nopt++;

       sscanf (argv[nopt], "%f", &fval); 
       (*max_sconstr)[index] = fval;
       nopt++;
       continue;
     }
      
      
      /*-----   -nconstr k min max  -----*/
      if (strcmp(argv[nopt], "-nconstr") == 0)
     {
       nopt++;
       if (nopt+2 >= argc)  NLfit_error("need 3 arguments after -nconstr ");

       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival >= *r))
         NLfit_error ("illegal argument after -nconstr ");
       index = ival;
       nopt++;

       sscanf (argv[nopt], "%f", &fval); 
       (*min_nconstr)[index] = fval;
       nopt++;

       sscanf (argv[nopt], "%f", &fval); 
       (*max_nconstr)[index] = fval;
       nopt++;
       continue;
     }
      

       /*-----   -freg filename   -----*/
      if (strcmp(argv[nopt], "-freg") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -freg ");
       *freg_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*freg_filename);
       strcpy (*freg_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

       /*-----   -frsqr filename   -----*/
      if (strcmp(argv[nopt], "-frsqr") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -frsqr ");
       *frsqr_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*frsqr_filename);
       strcpy (*frsqr_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

       /*-----   -fsmax filename   -----*/
      if (strcmp(argv[nopt], "-fsmax") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -fsmax ");
       *fsmax_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*fsmax_filename);
       strcpy (*fsmax_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

       /*-----   -ftmax filename   -----*/
      if (strcmp(argv[nopt], "-ftmax") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -ftmax ");
       *ftmax_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*ftmax_filename);
       strcpy (*ftmax_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

      /*-----   -fpmax filename   -----*/
      if (strcmp(argv[nopt], "-fpmax") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -fpmax ");
       *fpmax_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*fpmax_filename);
       strcpy (*fpmax_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

      /*-----   -farea filename   -----*/
      if (strcmp(argv[nopt], "-farea") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -farea ");
       *farea_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*farea_filename);
       strcpy (*farea_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

      /*-----   -fparea filename   -----*/
      if (strcmp(argv[nopt], "-fparea") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -fparea ");
       *fparea_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*fparea_filename);
       strcpy (*fparea_filename, argv[nopt]);
       nopt++;
       continue;
     }
      

      /*-----   -fscoef k filename   -----*/
      if (strcmp(argv[nopt], "-fscoef") == 0)
     {
       nopt++;
       if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -fscoef ");
       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival >= *p))
         NLfit_error ("illegal argument after -fscoef ");
       index = ival;
       nopt++;

       (*fscoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST ((*fscoef_filename)[index]);
       strcpy ((*fscoef_filename)[index], argv[nopt]);

       nopt++;
       continue;
     }
      

      /*-----   -fncoef k filename   -----*/
      if (strcmp(argv[nopt], "-fncoef") == 0)
     {
       nopt++;
       if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -fncoef ");
       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival >= *r))
         NLfit_error ("illegal argument after -fncoef ");
       index = ival;
       nopt++;

       (*fncoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST ((*fncoef_filename)[index]);
       strcpy ((*fncoef_filename)[index], argv[nopt]);

       nopt++;
       continue;
     }
      

      /*-----   -tscoef k filename   -----*/
      if (strcmp(argv[nopt], "-tscoef") == 0)
     {
       nopt++;
       if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -tscoef ");
       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival >= *p))
         NLfit_error ("illegal argument after -tscoef ");
       index = ival;
       nopt++;

       (*tscoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST ((*tscoef_filename)[index]);
       strcpy ((*tscoef_filename)[index], argv[nopt]);

       calc_tstats = 1;

       nopt++;
       continue;
     }
      

      /*-----   -tncoef k filename   -----*/
      if (strcmp(argv[nopt], "-tncoef") == 0)
     {
       nopt++;
       if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -tncoef ");
       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival >= *r))
         NLfit_error ("illegal argument after -tncoef ");
       index = ival;
       nopt++;

       (*tncoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST ((*tncoef_filename)[index]);
       strcpy ((*tncoef_filename)[index], argv[nopt]);

       calc_tstats = 1;

       nopt++;
       continue;
     }
      
      
      /*----- -bucket n prefixname -----*/
      if (strcmp(argv[nopt], "-bucket") == 0)
     {
       nopt++;
       if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -bucket ");
       sscanf (argv[nopt], "%d", &ival);
       if ((ival < 0) || (ival > MAX_BRICKS))
         NLfit_error ("illegal argument after -bucket ");
       nopt++;

       option_data->bucket_filename = 
         malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (option_data->bucket_filename);
       strcpy (option_data->bucket_filename, argv[nopt]);
       
       /*----- set number of sub-bricks in the bucket -----*/
       if (ival == 0)
         nbricks = (*p) + (*r) + 8;
       else
         nbricks = ival;
       option_data->numbricks = nbricks;
       
       /*----- allocate memory and initialize bucket dataset options -----*/
       option_data->brick_type = malloc (sizeof(int) * nbricks);
       MTEST (option_data->brick_type);
       option_data->brick_coef = malloc (sizeof(int) * nbricks);
       MTEST (option_data->brick_coef);
       option_data->brick_label = malloc (sizeof(char *) * nbricks);
       MTEST (option_data->brick_label);
       for (ibrick = 0;  ibrick < nbricks;  ibrick++)
         {
           option_data->brick_type[ibrick] = -1;
           option_data->brick_coef[ibrick] = -1;
           option_data->brick_label[ibrick] = 
          malloc (sizeof(char) * MAX_NAME_LENGTH);
           MTEST (option_data->brick_label[ibrick]);
         }
       

       if (ival == 0)   
         /*----- throw (almost) everything into the bucket -----*/
         {
           for (ibrick = 0;  ibrick < (*r);  ibrick++)
          {
            option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
            option_data->brick_coef[ibrick] = ibrick;
            strcpy (option_data->brick_label[ibrick], (*npname)[ibrick]);
          }
           
           for (ibrick = (*r);  ibrick < (*p) + (*r);  ibrick++)
          {
            option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
            option_data->brick_coef[ibrick] = ibrick;
            strcpy (option_data->brick_label[ibrick],
                 (*spname)[ibrick-(*r)]);
          }
           
           ibrick = (*p) + (*r);
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "Signal TMax");
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "Signal SMax");
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "Signal % SMax");
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "Signal Area");
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "Signal % Area"); 
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "Sigma Resid"); 
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "R^2"); 
           
           ibrick++;
           option_data->brick_type[ibrick] = FUNC_FT_TYPE;
           option_data->brick_coef[ibrick] = ibrick;
           strcpy (option_data->brick_label[ibrick], "F-stat Regression");
           
         }

       nopt++;
       continue;
     }


      /*----- -brick m type k label -----*/
      if (strcmp(argv[nopt], "-brick") == 0)
     {
       nopt++;
       if (nopt+2 >= argc)  
         NLfit_error ("need more arguments after -brick ");
       sscanf (argv[nopt], "%d", &ibrick);
       if ((ibrick < 0) || (ibrick >= option_data->numbricks))
         NLfit_error ("illegal argument after -brick ");
       nopt++;

       if (strcmp(argv[nopt], "scoef") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;

           nopt++;
           sscanf (argv[nopt], "%d", &ival);
           if ((ival < 0) || (ival > (*p)))
          NLfit_error ("illegal argument after scoef ");
           option_data->brick_coef[ibrick] = ival + (*r);
           
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]); 
         }

       else if (strcmp(argv[nopt], "ncoef") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;

           nopt++;
           sscanf (argv[nopt], "%d", &ival);
           if ((ival < 0) || (ival > (*r)))
          NLfit_error ("illegal argument after ncoef ");
           option_data->brick_coef[ibrick] = ival;
           
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]); 
         }

       else if (strcmp(argv[nopt], "tmax") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p);
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "smax") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 1;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "psmax") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 2;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "area") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 3;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "parea") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 4;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "resid") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 5;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "rsqr") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 6;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "fstat") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_FT_TYPE;
           option_data->brick_coef[ibrick] = (*r) + (*p) + 7;
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
         }

       else if (strcmp(argv[nopt], "tscoef") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_TT_TYPE;

           nopt++;
           sscanf (argv[nopt], "%d", &ival);
           if ((ival < 0) || (ival > (*p)))
          NLfit_error ("illegal argument after tscoef ");
           option_data->brick_coef[ibrick] = ival + (*r);
           
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]);
 
           calc_tstats = 1;
         }

       else if (strcmp(argv[nopt], "tncoef") == 0)
         {
           option_data->brick_type[ibrick] = FUNC_TT_TYPE;

           nopt++;
           sscanf (argv[nopt], "%d", &ival);
           if ((ival < 0) || (ival > (*r)))
          NLfit_error ("illegal argument after tncoef ");
           option_data->brick_coef[ibrick] = ival;
           
           nopt++;
           if (nopt >= argc)  
          NLfit_error ("need more arguments after -brick ");
           strcpy (option_data->brick_label[ibrick], argv[nopt]); 
 
           calc_tstats = 1;
         }

       else  NLfit_error ("unable to interpret options after -brick ");
       
       nopt++;
       continue;
     }
     

       /*-----   -sfit filename   -----*/
      if (strcmp(argv[nopt], "-sfit") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -sfit ");
       *sfit_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*sfit_filename);
       strcpy (*sfit_filename, argv[nopt]);
       nopt++;
       continue;
     }      

      
       /*-----   -snfit filename   -----*/
      if (strcmp(argv[nopt], "-snfit") == 0)
     {
       nopt++;
       if (nopt >= argc)  NLfit_error ("need argument after -snfit ");
       *snfit_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
       MTEST (*snfit_filename);
       strcpy (*snfit_filename, argv[nopt]);
       nopt++;
       continue;
     }      


      
      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      NLfit_error (message);
      
    }
   if (*ignore < 0) {
      fprintf(stderr,"\n** NOTICE: "
                     "ignore option now defaults to 0 instead of 3.\n\n");
      *ignore = 0;
   } 

  if( uuTR > 0.0f ) dsTR = uuTR ;
  
  /*----- discard the model array -----*/
  DESTROY_MODEL_ARRAY (model_array) ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one output file already exists.
*/

void check_one_output_file 
(
  THD_3dim_dataset * dset_time,     /* input 3d+time data set */
  char * filename                   /* name of output file */
)

{
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  char message[MAX_NAME_LENGTH];      /* error message */
  
  
  /*----- make an empty copy of input dataset -----*/
  new_dset = EDIT_empty_copy( dset_time ) ;
  
  
  ierror = EDIT_dset_items( new_dset ,
                   ADN_prefix , filename ,
                   ADN_label1 , filename ,
                   ADN_self_name , filename ,
                   ADN_type , ISHEAD(dset_time) ? HEAD_FUNC_TYPE : 
                                                         GEN_FUNC_TYPE ,
                   ADN_none ) ;
  
  if( ierror > 0 )
    {
      fprintf(stderr,
           "*** %d errors in attempting to create output dataset!\n", 
           ierror ) ;
      exit(1) ;
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message, "Output dataset file %s already exists",
           new_dset->dblk->diskptr->header_name ) ;
      NLfit_error (message);
    }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether output files already exist.
*/

void check_output_files 
(
  char * freg_filename,         /* file name for regression f-statistics */
  char * frsqr_filename,        /* file name for R^2 statistics */
  char * fsmax_filename,        /* file name for signal signed maximum */
  char * ftmax_filename,        /* file name for epoch of signed maximum */
  char * fpmax_filename,        /* file name for max. percentage change */
  char * farea_filename,        /* file name for area under the signal */
  char * fparea_filename,       /* file name for % area under the signal */
  char ** fncoef_filename,      /* file name for noise model parameters */
  char ** fscoef_filename,      /* file name for signal model parameters */
  char ** tncoef_filename,      /* file name for noise model t-statistics */
  char ** tscoef_filename,      /* file name for signal model t-statistics */
  char * bucket_filename,       /* file name for bucket dataset */
  char * sfit_filename,         /* file name for 3d+time fitted signal model */
  char * snfit_filename,        /* file name for 3d+time fitted signal+noise */
  THD_3dim_dataset * dset_time  /* input 3d+time data set */
)

{
  int ip;       /* parameter index */
  
  if (freg_filename != NULL)   
    check_one_output_file (dset_time, freg_filename);
  
  if (frsqr_filename != NULL)   
    check_one_output_file (dset_time, frsqr_filename);
  
  if (fsmax_filename != NULL)   
    check_one_output_file (dset_time, fsmax_filename);
  
  if (ftmax_filename != NULL)   
    check_one_output_file (dset_time, ftmax_filename);
  
  if (fpmax_filename != NULL)   
    check_one_output_file (dset_time, fpmax_filename);
  
  if (farea_filename != NULL)   
    check_one_output_file (dset_time, farea_filename);
  
  if (fparea_filename != NULL)   
    check_one_output_file (dset_time, fparea_filename);
  
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      if (fncoef_filename[ip] != NULL)
     check_one_output_file (dset_time, fncoef_filename[ip]);
      if (fscoef_filename[ip] != NULL)
     check_one_output_file (dset_time, fscoef_filename[ip]);
      if (tncoef_filename[ip] != NULL)
     check_one_output_file (dset_time, tncoef_filename[ip]);
      if (tscoef_filename[ip] != NULL)
     check_one_output_file (dset_time, tscoef_filename[ip]);      
    }

  if (bucket_filename != NULL)   
    check_one_output_file (dset_time, bucket_filename);


  if (sfit_filename != NULL)
    check_one_output_file (dset_time, sfit_filename);
  if (snfit_filename != NULL)
    check_one_output_file (dset_time, snfit_filename);


}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs 
(
  int nxyz,               /* number of voxels in image */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int  nrand,             /* number of random vectors to generate */
  int  nbest,             /* number of random vectors to keep */

  char * freg_filename,         /* file name for regression f-statistics */
  char * frsqr_filename,        /* file name for R^2 statistics */
  char * fsmax_filename,        /* file name for signal signed maximum */
  char * ftmax_filename,        /* file name for epoch of signed maximum */
  char * fpmax_filename,        /* file name for max. percentage change */
  char * farea_filename,        /* file name for area under the signal */
  char * fparea_filename,       /* file name for % area under the signal */
  char ** fncoef_filename,      /* file name for noise model parameters */
  char ** fscoef_filename,      /* file name for signal model parameters */
  char ** tncoef_filename,      /* file name for noise model t-statistics */
  char ** tscoef_filename,      /* file name for signal model t-statistics */
  char * bucket_filename,       /* file name for bucket dataset */
  char * sfit_filename,         /* file name for 3d+time fitted signal model */
  char * snfit_filename,        /* file name for 3d+time fitted signal+noise */
  THD_3dim_dataset * dset_time  /* input 3d+time data set */
)

{
  int ip;                       /* parameter index */


  /*----- check if mask is right size -----*/
  if (mask_vol != NULL) 
    if (mask_nvox != nxyz)  
      NLfit_error ("mask and input dataset bricks don't match in size");

    
  /*----- check for valid constraints -----*/
  for (ip = 0;  ip < r;  ip++)
    if (min_nconstr[ip] > max_nconstr[ip])
      NLfit_error ("Must have minimum constraints <= maximum constraints");
  for (ip = 0;  ip < p;  ip++)
    if (min_sconstr[ip] > max_sconstr[ip])
      NLfit_error("Must have mininum constraints <= maximum constraints");


  /*----- must have nbest <= nrand -----*/
  if (nrand < nbest)
    NLfit_error ("Must have nbest <= nrand");


  /*----- check whether any of the output files already exist -----*/
  if( THD_deathcon() )
   check_output_files (freg_filename, frsqr_filename, 
                fsmax_filename, ftmax_filename,
                fpmax_filename, farea_filename, fparea_filename,
                fncoef_filename, fscoef_filename,
                tncoef_filename, tscoef_filename, bucket_filename, 
                sfit_filename, snfit_filename, dset_time);

}

/*---------------------------------------------------------------------------*/
/*
  Allocate volume memory and fill with zeros.
*/

void zero_fill_volume (float ** fvol, int nxyz)
{
  int ixyz;

  if( proc_numjob == 1 ){ /* 1 process ==> allocate locally */

    *fvol  = (float *) malloc (sizeof(float) * nxyz);   MTEST(*fvol);
    for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      (*fvol)[ixyz]  = 0.0;

  }
#ifdef PROC_MAX
   else {             /* multiple processes ==> prepare for shared memory */
                      /*                        by remembering what to do */

    proc_shm_arnum++ ;
    proc_shm_arsiz = (int *)  realloc( proc_shm_arsiz ,
                                       sizeof(int)     *proc_shm_arnum ) ;
    proc_shm_ar = (float ***) realloc( proc_shm_ar ,
                                       sizeof(float **)*proc_shm_arnum ) ;
    proc_shm_arsiz[proc_shm_arnum-1] = nxyz ;
    proc_shm_ar[proc_shm_arnum-1]    = fvol ;

    /* actual allocation and pointer assignment (to *fvol)
       will take place in function proc_finalize_shm_volumes() */
  }
#endif
}


/*---------------------------------------------------------------------------*/

#ifdef PROC_MAX

/*** signal handler for fatal errors -- make sure shared memory is deleted ***/

#include <signal.h>

void proc_sigfunc(int sig)
{
   char * sname ; int ii ;
   static volatile int fff=0 ;
   if( fff ) _exit(1); else fff=1 ;
   switch(sig){
     default:      sname = "unknown" ; break ;
     case SIGHUP:  sname = "SIGHUP"  ; break ;
     case SIGTERM: sname = "SIGTERM" ; break ;
     case SIGILL:  sname = "SIGILL"  ; break ;
     case SIGKILL: sname = "SIGKILL" ; break ;
     case SIGPIPE: sname = "SIGPIPE" ; break ;
     case SIGSEGV: sname = "SIGSEGV" ; break ;
     case SIGBUS:  sname = "SIGBUS"  ; break ;
     case SIGINT:  sname = "SIGINT"  ; break ;
     case SIGFPE:  sname = "SIGFPE"  ; break ;
   }
   if( proc_shmid > 0 ){
     shmctl( proc_shmid , IPC_RMID , NULL ) ; proc_shmid = 0 ;
   }
   fprintf(stderr,"Fatal Signal %d (%s) received in job #%d\n",
           sig,sname,proc_ind) ;
   exit(1) ;
}

void proc_atexit( void )  /*** similarly - atexit handler ***/
{
  if( proc_shmid > 0 )
    shmctl( proc_shmid , IPC_RMID , NULL ) ;
}

/*---------------------------------------------------------------*/
/*** This function is called to allocate all output
     volumes at once in shared memory, and set their pointers ***/
/** 24 Oct 2006: use mmap() instead of shared memory, if possible **/

#include <sys/mman.h>
#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
# define MAP_ANON MAP_ANONYMOUS
#endif


void proc_finalize_shm_volumes(void)
{
   char kstr[32] ; int ii ;
   long long psum , twogig = 2ll * 1024ll * 1024ll * 1024ll ;

   if( proc_shm_arnum == 0 ) return ;   /* should never happen */

   /** 21 Oct 2005: check in big arithmetic how much mem we need **/

   psum = 0 ;
   for( ii=0 ; ii < proc_shm_arnum ; ii++ )
     psum += (long long)proc_shm_arsiz[ii] ;
   psum *= sizeof(float) ;
#ifdef MAP_ANON
   if( psum >= twogig &&
       ( sizeof(void *) < 8 || sizeof(size_t) < 8 ) ) /* too much for 32-bit */
#else
   if( psum >= twogig )                               /* too much for shmem */
#endif
     ERROR_exit(
     "Total shared memory needed = %lld >= %lld (2 GB)\n"
     "** SUGGESTION 1:  Use 3dAutobox to automatically eliminate non-brain\n"
     "   areas from the 3d+time input dataset and reduce memory \n"
     "   requirements,  e.g.\n"
     "     3dAutobox -prefix Subj1AllRuns_Smaller -input Subj1AllRuns\n"
     "   Then run 3dNLfim again with the smaller 3d+time input dataset\n"
     "\n"
     "** SUGGESTION 2:  Use 3dZcutup to slice dataset into smaller pieces\n"
     "**                and then 3dZcat to glue results back together.\n"
     "\n"
     "** SUGGESTION 3:  Run on a 64-bit computer system, instead of 32-bit.\n"
     , psum,twogig) ;
   else
     INFO_message("total shared memory needed = %lld bytes (about %s)" ,
                  psum , approximate_number_string((double)psum) ) ;

   proc_shmsize = psum ;  /* global variable */
#if 0
/* old code */
   if( proc_shm_arnum == 0 ) return ;   /* should never happen */

   proc_shmsize = 0 ;                       /* add up sizes of */
   for( ii=0 ; ii < proc_shm_arnum ; ii++ ) /* all arrays for */
     proc_shmsize += proc_shm_arsiz[ii] ;   /* shared memory */

   proc_shmsize *= sizeof(float) ;          /* convert to byte count */
#endif

   /* create shared memory segment */
#ifdef MAP_ANON  /** 24 Oct 2005: use mmap() instead of shmem **/

   proc_shmptr = mmap( (void *)0 , (size_t)psum ,
                       PROT_READ | PROT_WRITE , MAP_ANON | MAP_SHARED ,
                       -1 , 0 ) ;
   if( proc_shmptr == NULL || proc_shmptr == (void *)(-1) ){
     perror("** FATAL ERROR: Can't create shared mmap() segment\n"
            "** Unix message") ;
     INFO_message("\n\n"
       "** SUGGESTION:  Use 3dZcutup to slice dataset into smaller pieces\n"
       "**                and then 3dZcat to glue results back together.\n"
       "** SUGGESTION:  Run on a 64-bit computer system, instead of 32-bit.\n ");
     exit(1) ;
   }

#else    /** old code: shared memory segment **/

   UNIQ_idcode_fill( kstr ) ;               /* unique string "key" */
   proc_shmid = shm_create( kstr , proc_shmsize ) ; /* thd_iochan.c */
   if( proc_shmid < 0 ){
     fprintf(stderr,"\n** Can't create shared memory of size %d!\n"
                      "** Try re-running without -jobs option!\n" ,
             proc_shmsize ) ;

     /** if failed, print out some advice on how to tune SHMMAX **/

     { char *cmd=NULL ;
#if defined(LINUX)
       cmd = "cat /proc/sys/kernel/shmmax" ;
#elif defined(SOLARIS)
       cmd = "/usr/sbin/sysdef | grep SHMMAX" ;
#elif defined(DARWIN)
       cmd = "sysctl -n kern.sysv.shmmax" ;
#endif
       if( cmd != NULL ){
        FILE *fp = popen( cmd , "r" ) ;
        if( fp != NULL ){
         long long smax=0 ;
         fscanf(fp,"%lld",&smax) ; pclose(fp) ;
         if( smax > 0 )
           fprintf(stderr ,
                   "\n"
                   "** POSSIBLY USEFUL ADVICE:\n"
                   "** Current max shared memory size = %u bytes.\n"
                   "** For information on how to change this, see\n"
                   "**   http://afni.nimh.nih.gov/afni/parallize.htm\n"
                   "** and also contact your system administrator.\n"
                   , smax ) ;
        }
       }
     }
     exit(1) ;
   }
#endif  /* MAP_ANON */

   /* set a signal handler to catch most fatal errors and
      delete the shared memory segment if program crashes */

   signal(SIGPIPE,proc_sigfunc) ; signal(SIGSEGV,proc_sigfunc) ;
   signal(SIGINT ,proc_sigfunc) ; signal(SIGFPE ,proc_sigfunc) ;
   signal(SIGBUS ,proc_sigfunc) ; signal(SIGHUP ,proc_sigfunc) ;
   signal(SIGTERM,proc_sigfunc) ; signal(SIGILL ,proc_sigfunc) ;
   signal(SIGKILL,proc_sigfunc) ; signal(SIGPIPE,proc_sigfunc) ;
   atexit(proc_atexit) ;   /* or when the program exits */

#ifdef MAP_ANON
   fprintf(stderr , "++ mmap() memory allocated: %lld bytes\n" ,
           proc_shmsize ) ;
#else
   fprintf(stderr , "++ Shared memory allocated: %lld bytes at id=%d\n" ,
           proc_shmsize , proc_shmid ) ;
#endif

   /* get pointer to shared memory segment we just created */
#ifndef MAP_ANON
  if(proc_shmid > 0) {
     proc_shmptr = shm_attach( proc_shmid ) ; /* thd_iochan.c */
     if( proc_shmptr == NULL ){
       fprintf(stderr,"\n** Can't attach to shared memory!?\n"
                	"** This is bizarre.\n" ) ;
       shmctl( proc_shmid , IPC_RMID , NULL ) ;
       exit(1) ;
     }
   }
#endif   

   /* clear all shared memory to zero*/

   memset( proc_shmptr , 0 , proc_shmsize ) ;

   /* fix the local pointers to arrays in shared memory */

   *proc_shm_ar[0] = (float *) proc_shmptr ;
   for( ii=1 ; ii < proc_shm_arnum ; ii++ )
     *proc_shm_ar[ii] = *proc_shm_ar[ii-1] + proc_shm_arsiz[ii] ;
     
   return;
}

/*-------------------------------------------------------------*/
/*** This function replaces free();
     it won't try to free things stored in the shared memory ***/

void proc_free( void *ptr )
{
   int ii ;

   if( ptr == NULL ) return ;
   /* from if ( proc_shmid == 0 )             25 Oct 2006 DRG */
   if( proc_shmptr == NULL ){ free(ptr); return; }  /* no shm */
   for( ii=0 ; ii < proc_shm_arnum ; ii++ )
     if( ((float *)ptr) == *proc_shm_ar[ii] ) return;
   free(ptr); return;
}

#undef  free            /* replace use of library free() */
#define free proc_free  /* with proc_free() function     */

#endif /* PROC_MAX */

/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program 
(
  int argc,                /* number of input arguments */
  char ** argv,            /* array of input arguments */ 
  int * ignore,            /* delete this number of points from time series */
  char ** nname,           /* name of noise model */
  char ** sname,           /* name of signal model */
  vfp * nmodel,            /* pointer to noise model */
  vfp * smodel,            /* pointer to signal model */  
  int *r,                  /* number of parameters in the noise model */
  int *p,                  /* number of parameters in the signal model */
  char *** npname,         /* noise parameter names */
  char *** spname,         /* signal parameter names */
  float ** min_nconstr,    /* minimum parameter constraints for noise model */
  float ** max_nconstr,    /* maximum parameter constraints for noise model */
  float ** min_sconstr,    /* minimum parameter constraints for signal model */
  float ** max_sconstr,    /* maximum parameter constraints for signal model */
  int * nabs,              /* use absolute constraints for noise parameters */
  int * nrand,             /* number of random vectors to generate */
  int * nbest,             /* number of random vectors to keep */
  float * rms_min,         /* minimum rms error to reject reduced model */
  float * fdisp,           /* minimum f-statistic for display */ 
  int *progress,           /* progress report every nth voxel */
  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** tfilename,          /* file name for time point series */  
  char ** freg_filename,      /* file name for regression f-statistics */
  char ** frsqr_filename,     /* file name for R^2 statistics */
  char ** fsmax_filename,     /* file name for signal signed maximum */
  char ** ftmax_filename,     /* file name for time of signed maximum */
  char ** fpmax_filename,     /* file name for max. percentage change */
  char ** farea_filename,     /* file name for area under the signal */
  char ** fparea_filename,    /* file name for % area under the signal */
  char *** fncoef_filename,   /* file name for noise model parameters */
  char *** fscoef_filename,   /* file name for signal model parameters */
  char *** tncoef_filename,   /* file name for noise model t-statistics */
  char *** tscoef_filename,   /* file name for signal model t-statistics */
  char ** sfit_filename,      /* file name for 3d+time fitted signal model */
  char ** snfit_filename,     /* file name for 3d+time fitted signal+noise */

  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  int * nxyz,                       /* number of voxels in image */
  int * ts_length,                  /* length of time series data */  

  float *** x_array,       /* independent variable matrix */
  float ** ts_array,       /* input time series array */

  float ** par_rdcd,       /* estimated parameters for the reduced model */
  float ** par_full,       /* estimated parameters for the full model */
  float ** tpar_full,      /* t-statistic of the parameters in full model */

  float ** rmsreg_vol,     /* root-mean-square error for the full model */
  float ** freg_vol,       /* f-statistic for the full regression model */
  float ** rsqr_vol,       /* R^2 volume data */
  float ** smax_vol,       /* volume of signed maximum of signal */
  float ** tmax_vol,       /* volume of epoch of signed maximum of signal */
  float ** pmax_vol,       /* max. percentage change due to signal */
  float ** area_vol,       /* area between signal and baseline */
  float ** parea_vol,      /* percentage area between signal and baseline */
  float *** ncoef_vol,     /* volume of noise model parameters */
  float *** scoef_vol,     /* volume of signal model parameters */
  float *** tncoef_vol,    /* volume of noise model t-statistics */
  float *** tscoef_vol,    /* volume of signal model t-statistics */
  float *** sfit_vol,      /* voxelwise 3d+time fitted signal model */ 
  float *** snfit_vol,     /* voxelwise 3d+time fitted signal+noise model */ 

  NL_options * option_data          /* bucket dataset options */

)
     
{
  int dimension;           /* dimension of full model */
  int ip;                  /* parameter index */
  int it;                  /* time index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures 
                              -- used to read 1D ASCII */
  int nt;                  /* number of points in 1D x data file */
  float * tar;
  int ixyz;                /* voxel index */
  

  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- get command line inputs -----*/
  get_options(argc, argv, ignore, nname, sname, nmodel, smodel, 
           r, p, npname, spname, 
           min_nconstr, max_nconstr, min_sconstr, max_sconstr, nabs, 
           nrand, nbest, rms_min, fdisp, progress, input_filename, tfilename, 
           freg_filename, frsqr_filename, fsmax_filename, 
           ftmax_filename, fpmax_filename, farea_filename, fparea_filename, 
           fncoef_filename, fscoef_filename,
           tncoef_filename, tscoef_filename, sfit_filename, snfit_filename,
           dset_time, nxyz, ts_length, option_data);

 
  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*nxyz, *r, *p, *min_nconstr, *max_nconstr, 
                 *min_sconstr, *max_sconstr, *nrand, *nbest, 
                 *freg_filename, *frsqr_filename, 
                 *fsmax_filename, *ftmax_filename,
                 *fpmax_filename, *farea_filename, *fparea_filename,
                 *fncoef_filename, *fscoef_filename, 
                 *tncoef_filename, *tscoef_filename, 
                 *sfit_filename, *snfit_filename, 
                 option_data->bucket_filename, *dset_time);


  /*----- allocate space for input time series -----*/
  *ts_length = *ts_length - *ignore;
  *ts_array = (float *) malloc (sizeof(float) * (*ts_length));
  MTEST (*ts_array);

  
  /*----- allocate space for independent variable matrix -----*/
  *x_array = (float **) malloc (sizeof(float *) * (*ts_length));
  MTEST (*x_array);
  for (it = 0;  it < (*ts_length);  it++)
    {
      (*x_array)[it] = (float *) malloc (sizeof(float) * 3);
      MTEST ((*x_array)[it]);
    }

    
  /*----- initialize independent variable matrix -----*/
  if (*tfilename == NULL)
    {
     if( (inTR || uuTR > 0.0f) && dsTR > 0.0 ){   /* 22 July 1998 */
        DELT = dsTR ;
        fprintf(stderr,"--- computing with TR = %g\n",DELT) ;
     }
     for (it = 0;  it < (*ts_length);  it++)  
      {
        (*x_array)[it][0] = 1.0;
        (*x_array)[it][1] = it * DELT;
        (*x_array)[it][2] = (it * DELT) * (it * DELT);
      }
    }
  else 
    {
        flim = mri_read_1D(*tfilename) ;
     if (flim == NULL)
       NLfit_error ("Unable to read time reference file \n");
        nt = flim -> nx;
     if (nt < (*ts_length))
       NLfit_error ("Time reference array is too short");  
        tar = MRI_FLOAT_PTR(flim) ;
        for (it = 0;  it < (*ts_length);  it++)  
         {
        (*x_array)[it][0] = 1.0;
           (*x_array)[it][1] = tar[it] ;
           (*x_array)[it][2] = tar[it] * tar[it];
         }
        mri_free (flim);
     }

  /*--- 24 Jul 2006: special change to x_array[][2] for Linear+Ort [RWCox] ---*/

   if( strcmp(*nname,"Linear+Ort") == 0 ){
      char *fname ; MRI_IMAGE *fim ; int nx ; float *far ;
      fname = my_getenv("AFNI_ORTMODEL_REF") ;
      if( fname == NULL )
        ERROR_exit("Linear+Ort model: 'AFNI_ORTMODEL_REF' not set") ;

      fim = mri_read_1D(fname) ;
      if( fim == NULL || fim->nx < 2 )
        ERROR_exit(
          "Linear+Ort model: can't read file AFNI_ORTMODEL_REF='%s'",fname) ;

      nx = fim->nx ; far = MRI_FLOAT_PTR(fim) ;
      if( nx != (*ts_length) || fim->ny > 1 )
        WARNING_message(
         "Linear+Ort model: AFNI_ORTMODEL_REF='%s' has nx=%d ny=%d; data length=%d",
         fname , nx , fim->ny , *ts_length ) ;
      else
        INFO_message(
          "Linear+Ort model: AFNI_ORTMODEL_REF='%s' loaded; length=%d" ,
          fname , nx ) ;

      for( it=0 ; it < (*ts_length);  it++)
        (*x_array)[it][2] = (it < nx) ? far[it] : 0.0f ;
   }

  /*----- allocate memory space for parameters -----*/
  dimension = (*r) + (*p);
  *par_rdcd = (float *) malloc (sizeof(float) * dimension);  
  MTEST (*par_rdcd);
  *par_full = (float *) malloc (sizeof(float) * dimension);  
  MTEST (*par_full);
  *tpar_full = (float *) malloc (sizeof(float) * dimension); 
  MTEST (*tpar_full);


  /*----- allocate memory space for volume data -----*/
  zero_fill_volume( freg_vol , *nxyz ) ;

  if ((*freg_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( rmsreg_vol , *nxyz ) ;

  if ((*frsqr_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( rsqr_vol , *nxyz ) ;

  if ((*fsmax_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( smax_vol , *nxyz ) ;

  if ((*ftmax_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( tmax_vol , *nxyz ) ;

  
  if ((*fpmax_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( pmax_vol , *nxyz ) ;

  if ((*farea_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( area_vol , *nxyz ) ;

  if ((*fparea_filename != NULL) || (option_data->bucket_filename != NULL))
      zero_fill_volume( parea_vol , *nxyz ) ;

  
  *ncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  MTEST (*ncoef_vol);
  *tncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  MTEST (*tncoef_vol);

  for (ip = 0;  ip < (*r);  ip++)
    {
      if (((*fncoef_filename)[ip] != NULL) || ((*tncoef_filename)[ip] != NULL)
       || (option_data->bucket_filename != NULL))
          zero_fill_volume( &((*ncoef_vol)[ip]) , *nxyz ) ;
      else
     (*ncoef_vol)[ip] = NULL;

      if (((*tncoef_filename)[ip] != NULL) 
       || (option_data->bucket_filename != NULL))
          zero_fill_volume( &((*tncoef_vol)[ip]) , *nxyz ) ;
      else
     (*tncoef_vol)[ip] = NULL;
    }
  

  *scoef_vol = (float **) malloc (sizeof(float *) * (*p));
  MTEST (*scoef_vol);
  *tscoef_vol = (float **) malloc (sizeof(float *) * (*p));
  MTEST (*tscoef_vol);

  for (ip = 0;  ip < (*p);  ip++)
    {
      if (((*fscoef_filename)[ip] != NULL) || ((*tscoef_filename)[ip] != NULL)
       || (option_data->bucket_filename != NULL))
          zero_fill_volume( &((*scoef_vol)[ip]) , *nxyz ) ;
      else
     (*scoef_vol)[ip] = NULL;

      if (((*tscoef_filename)[ip] != NULL)
       || (option_data->bucket_filename != NULL))
          zero_fill_volume( &((*tscoef_vol)[ip]) , *nxyz ) ;
      else
     (*tscoef_vol)[ip] = NULL;
    }


  /*----- Allocate memory space for 3d+time fitted signal model -----*/
  if (*sfit_filename != NULL)
    {
      *sfit_vol = (float **) malloc (sizeof(float *) * (*ts_length));
      MTEST(*sfit_vol);
 
      for (it = 0;  it < *ts_length;  it++)
          zero_fill_volume( &((*sfit_vol)[it]) , *nxyz ) ;
    }

  /*----- Allocate memory space for 3d+time fitted signal+noise model -----*/
  if (*snfit_filename != NULL)
    {
      *snfit_vol = (float **) malloc (sizeof(float *) * (*ts_length));
      MTEST(*snfit_vol);
 
      for (it = 0;  it < *ts_length;  it++)
          zero_fill_volume( &((*snfit_vol)[it]) , *nxyz ) ;
    }

#ifdef PROC_MAX
  if( proc_numjob > 1 ) proc_finalize_shm_volumes() ;  /* RWCox */
#endif

}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void read_ts_array 
(
  THD_3dim_dataset * dset_time,      /* input 3d+time data set */
  int iv,                            /* get time series for this voxel */
  int ts_length,                     /* length of time series array */
  int ignore,              /* delete this number of points from time series */
  float * ts_array         /* input time series data for voxel #iv */
)

{
  MRI_IMAGE * im;          /* intermediate float data */
  float * ar;              /* pointer to float data */
  int it;                  /* time index */

ENTRY("read_ts_array") ;


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
STATUS("extracting time series") ;
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  NLfit_error ("Unable to extract data from 3d+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
STATUS("loading into ts_array") ;
  ar = MRI_FLOAT_PTR (im);
  for (it = 0;  it < ts_length;  it++)
    {
      ts_array[it] = ar[it+ignore];
    }


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;
  EXRETURN ;
}


/*---------------------------------------------------------------------------*/
/*
  Print out the given time series.
*/

void write_ts_array 
(
  int ts_length,               /* length of time series array */
  float * ts_array             /* time series data to be printed */
)

{
  int it;                      /* time index */

  for (it = 0;  it < ts_length;  it++)
    printf ("%d  %f \n", it, ts_array[it]);
}


/*---------------------------------------------------------------------------*/
/*
  Save results for output later.
*/

void save_results 
(
  int iv,                  /* current voxel number */
  vfp nmodel,              /* pointer to noise model */
  vfp smodel,              /* pointer to signal model */  
  int r,                   /* number of parameters in the noise model */
  int p,                   /* number of parameters in the signal model */
  int novar,               /* flag for insufficient variation in the data */
  int ts_length,           /* length of time series data */
  float ** x_array,        /* independent variable matrix */
  float * par_full,        /* estimated parameters for the full model */
  float * tpar_full,       /* t-statistic of the parameters in full model */
  float rmsreg,            /* root-mean-square error for the full model */
  float freg,              /* f-statistic for the full regression model */
  float rsqr,              /* R^2 (coef. of multiple determination) */
  float smax,              /* signed maximum of signal */
  float tmax,              /* epoch of signed maximum of signal */
  float pmax,              /* percentage change due to signal */
  float area,              /* area between signal and baseline */
  float parea,             /* percentage area between signal and baseline */
  
  float * rmsreg_vol,      /* root-mean-square for the full regression model */
  float * freg_vol,        /* f-statistic for the full regression model */
  float * rsqr_vol,        /* R^2 volume data */
  float * smax_vol,        /* signed maximum of signal volume data */
  float * tmax_vol,        /* epoch of signed maximum volume data */
  float * pmax_vol,        /* percentage change in signal volume data */
  float * area_vol,        /* area underneath signal volume data */
  float * parea_vol,       /* percentage area underneath signal volume data */
  float ** ncoef_vol,      /* volume of noise model parameters */
  float ** scoef_vol,      /* volume of signal model parameters */
  float ** tncoef_vol,     /* volume of noise model t-statistics */
  float ** tscoef_vol,     /* volume of signal model t-statistics */
  float ** sfit_vol,       /* voxelwise 3d+time fitted signal model */ 
  float ** snfit_vol       /* voxelwise 3d+time fitted signal+noise model */ 
)

{
  int ip;                  /* parameter index */
  int it;                  /* time index */
  float * s_array;         /* fitted signal model time series */
  float * n_array;         /* fitted noise model time series */


  /*----- save regression results into volume data -----*/ 
  if (freg_vol != NULL)    freg_vol[iv] = freg;
  if (rmsreg_vol != NULL)  rmsreg_vol[iv] = rmsreg;
  if (rsqr_vol != NULL)    rsqr_vol[iv] = rsqr;

  /*----- save signed maximum and epoch of signed maximum of signal -----*/
  if (smax_vol != NULL)  smax_vol[iv] = smax;
  if (tmax_vol != NULL)  tmax_vol[iv] = tmax;

  /*----- save percentage change and area beneath the signal -----*/
  if (pmax_vol != NULL)  pmax_vol[iv] = pmax;
  if (area_vol != NULL)  area_vol[iv] = area;
  if (parea_vol != NULL) parea_vol[iv] = parea;

  /*----- save noise parameter estimates -----*/
  for (ip = 0;  ip < r;  ip++)
    {
      if (ncoef_vol[ip] != NULL)  ncoef_vol[ip][iv] = par_full[ip];
      if (tncoef_vol[ip] != NULL)  tncoef_vol[ip][iv] = tpar_full[ip];
    }
  
  /*----- save signal parameter estimates -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      if (scoef_vol[ip] != NULL)  scoef_vol[ip][iv] = par_full[ip+r];
      if (tscoef_vol[ip] != NULL)  tscoef_vol[ip][iv] = tpar_full[ip+r];
    }

  
  /*----- save fitted signal model time series -----*/
  if (sfit_vol != NULL)
    {
      if (novar)
     {
       for (it = 0;  it < ts_length;  it++)
         sfit_vol[it][iv] = 0.0;
     }
      else
     {
       s_array = (float *) malloc (sizeof(float) * (ts_length));
       MTEST (s_array);

       smodel (par_full + r, ts_length, x_array, s_array);
       
       for (it = 0;  it < ts_length;  it++)
         sfit_vol[it][iv] = s_array[it];

       free (s_array);   s_array = NULL;
     }
    }


  /*----- save fitted signal+noise model time series -----*/
  if (snfit_vol != NULL)
    {
      n_array = (float *) malloc (sizeof(float) * (ts_length));
      MTEST (n_array);  
      nmodel (par_full, ts_length, x_array, n_array);

      for (it = 0;  it < ts_length;  it++)
     {
       snfit_vol[it][iv] = n_array[it];
     }

      free (n_array);   n_array = NULL;
      

      if (! novar)
     {
       s_array = (float *) malloc (sizeof(float) * (ts_length));
       MTEST (s_array);
       smodel (par_full + r, ts_length, x_array, s_array);

       for (it = 0;  it < ts_length;  it++)
         {
           snfit_vol[it][iv] += s_array[it];
         }
       
       free (s_array);   s_array = NULL;
     }
    }
  
}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI data set.
  
  This data set may be either 'fitt' type (intensity + t-statistic)
                           or 'fift' type (intensity + F-statistic).
  
  The intensity data is in ffim, and the corresponding statistic is in ftr.
  
  numdof = numerator degrees of freedom
  dendof = denominator degrees of freedom
  
  Note:  if dendof = 0, then write 'fitt' type data set;
         if dendof > 0, then write 'fift' type data set.
*/

void write_afni_data (char * input_filename, int nxyz, char * filename,  
                      float * ffim,  float * ftr,  int numdof,  int dendof)
{
  int ii;                             /* voxel index */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int iv;                             /* sub-brick index */ 
  int ierror;                         /* number of errors in editing data */
  int ibuf[32];                       /* integer buffer */
  float fbuf[MAX_STAT_AUX];           /* float buffer */
  float fimfac;                       /* scale factor for short data */
/*  int output_datum;  */                 /* data type for output data */
  short * tsp;                        /* 2nd sub-brick data pointer */
  void  * vdif;                       /* 1st sub-brick data pointer */
  int func_type;                      /* afni data set type */
  float top, func_scale_short;        /* parameters for scaling data */
  char label[80];                     /* label for output file history */ 
  int nbad;                           /* number of bad voxels in volume */
    
  /*----- read input dataset -----*/
  dset = THD_open_dataset (input_filename) ; /* was THD_open_one...*/
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
         input_filename);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  

  /*----- Record history of dataset -----*/
  tross_Copy_History( dset , new_dset ) ;
  sprintf (label, "Output prefix: %s", filename);
  if( commandline != NULL )
     tross_multi_Append_History( new_dset , commandline,label,NULL ) ;
  else
     tross_Append_History ( new_dset, label);

  
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  
  fprintf(stderr,"--output datum is %d\n", output_datum);
/*  output_datum = DSET_BRICK_TYPE(dset,iv) ;
  if( output_datum == MRI_byte ) output_datum = MRI_short ;
*/  
  
  ibuf[0] = output_datum ; ibuf[1] = MRI_short ;
  
  if (dendof == 0) func_type = FUNC_TT_TYPE;
  else func_type = FUNC_FT_TYPE;
  
  ierror = EDIT_dset_items( new_dset ,
                   ADN_prefix , filename ,
                   ADN_label1 , filename ,
                   ADN_self_name , filename ,
                   ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
                                             GEN_FUNC_TYPE ,
                   ADN_func_type , func_type ,
                   ADN_nvals , FUNC_nvals[func_type] ,
                   ADN_datum_array , ibuf ,
                   ADN_ntt , 0 ,
                   ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
                   ADN_none ) ;
  
  if( ierror > 0 ){
    fprintf(stderr,
          "*** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
         "*** Output dataset file %s already exists--cannot continue!\a\n",
         new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  /*----- deleting exemplar dataset -----*/ 
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
  
  nbad = thd_floatscan( nxyz , ffim ) ;
  if(nbad)
    fprintf(stderr,"++ %d bad floating point values in combined dataset\n",nbad);
  
  /*----- allocate memory for output data -----*/
  vdif = (void *)  malloc( mri_datum_size(output_datum) * nxyz );
  if (vdif == NULL)   NLfit_error ("Unable to allocate memory for vdif");
  tsp  = (short *) malloc( sizeof(short) * nxyz );
  if (tsp == NULL)   NLfit_error ("Unable to allocate memory for tsp");
  
  /*----- attach bricks to new data set -----*/
  mri_fix_data_pointer (vdif, DSET_BRICK(new_dset,0)); 
  mri_fix_data_pointer (tsp, DSET_BRICK(new_dset,1));  
  
  
  /*----- convert data type to output specification -----*/
  fimfac = EDIT_coerce_autoscale_new (nxyz, 
                          MRI_float, ffim, 
                          output_datum, vdif);
  if (fimfac != 0.0)  fimfac = 1.0 / fimfac;
  
#define TOP_SS  32700
  
  if (dendof == 0)   /* t-statistic */
    { 
      top = TOP_SS/FUNC_TT_SCALE_SHORT;
      func_scale_short = FUNC_TT_SCALE_SHORT;
    }
  else               /* F-statistic */
    {
      top = TOP_SS/FUNC_FT_SCALE_SHORT;
      func_scale_short = FUNC_FT_SCALE_SHORT;
    }
  
  for (ii = 0;  ii < nxyz;  ii++)
    {
      if (ftr[ii] > top)
     tsp[ii] = TOP_SS;
      else  if (ftr[ii] < -top)
     tsp[ii] = -TOP_SS;
      else  if (ftr[ii] >= 0.0)
     tsp[ii] = (short) (func_scale_short * ftr[ii] + 0.5);
      else
     tsp[ii] = (short) (func_scale_short * ftr[ii] - 0.5);

    }
  

  /*----- write afni data set -----*/
  INFO_message("Writing combined dataset into %s\n",DSET_BRIKNAME(new_dset)) ;
  
  fbuf[0] = numdof;   
  fbuf[1] = dendof;
  for( ii=2 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  if( do_FDR && !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
  { int ii = THD_create_all_fdrcurves( new_dset ) ;
    if( ii > 0 ) ININFO_message("created %d FDR curves in header",ii) ;
  }

  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one bucket data set.
*/

void write_bucket_data 
(
  int q,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int numdof,             /* numerator dof for F-statistic */
  int dendof,             /* denominator dof for F-statistic */
  int  nxyz,              /* number of voxels in image */
  int  n,                 /* length of time series data */  

  float * rmsreg_vol,     /* root-mean-square error for the full model */
  float * freg_vol,       /* f-statistic for the full regression model */
  float * rsqr_vol,       /* R^2 volume data */
  float * smax_vol,       /* signed maximum of signal volume data */
  float * tmax_vol,       /* epoch of signed maximum volume data */
  float * pmax_vol,       /* percentage change in signal volume data */
  float * area_vol,       /* area underneath signal volume data */
  float * parea_vol,      /* percentage area underneath signal volume data */
  float ** ncoef_vol,     /* volume of noise model parameters */
  float ** scoef_vol,     /* volume of signal model parameters */
  float ** tncoef_vol,    /* volume of noise model t-statistics */
  float ** tscoef_vol,    /* volume of signal model t-statistics */

  char * input_filename,

  NL_options * option_data     /* user input options */
)

{
  const float EPSILON = 1.0e-10;

  THD_3dim_dataset * old_dset = NULL;    /* prototype dataset */
  THD_3dim_dataset * new_dset = NULL;    /* output bucket dataset */
  char * output_prefix;     /* prefix name for bucket dataset */
  char * output_session;    /* directory for bucket dataset */
  int nbricks, ib;          /* number of sub-bricks in bucket dataset */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */
  float ** far = NULL;      /* far[ib] points to data for sub-brick #ib */
  float factor;             /* factor is new scale factor for sub-brick #ib */
  int brick_type;           /* indicates statistical type of sub-brick */
  int brick_coef;           /* regression coefficient index for sub-brick */
  char * brick_label;       /* character string label for sub-brick */
  int ierror;               /* number of errors in editing data */
  float *volume=NULL;       /* volume of floating point data */
  int dimension;            /* dimension of full model = p + q */
  char label[80];           /* label for output file history */ 
  void * imptr;             /* pointer to volume in correct datum type to actually write out*/
  int nbad;                 /* number of bad floating point values in volume */    

  /*----- initialize local variables -----*/
  nbricks = option_data->numbricks;
  output_prefix = option_data->bucket_filename;
  output_session = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
  strcpy (output_session, "./");
  dimension = p + q;
  

  /*----- allocate memory -----*/
  if(output_datum==MRI_short) {
     bar  = (short **) malloc (sizeof(short *) * nbricks);
     MTEST (bar);
  }
  else {
     far  = (float **) malloc (sizeof(float *) * nbricks);
     MTEST (far);
  }
  
  /*----- read first dataset -----*/
  old_dset = THD_open_dataset (input_filename); /* was THD_open_one...*/
  

  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (old_dset);
  

  /*----- Record history of dataset -----*/
  tross_Copy_History( old_dset , new_dset ) ;
  if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;
  sprintf (label, "Output prefix: %s", output_prefix);
  tross_Append_History ( new_dset, label);


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          output_prefix,
                   ADN_directory_name,  output_session,
                   ADN_type,            HEAD_FUNC_TYPE,
                   ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_ntt,             0,               /* no time */
                   ADN_nvals,           nbricks,
                   ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,  
                   ADN_none ) ;
  
  if( ierror > 0 )
    {
      fprintf(stderr, 
           "*** %d errors in attempting to create output dataset!\n", 
           ierror);
      exit(1);
    }
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      fprintf(stderr,
           "*** Output dataset file %s already exists--cannot continue!\n",
           DSET_HEADNAME(new_dset));
      exit(1);
    }
  

  /*----- deleting exemplar dataset -----*/ 
  THD_delete_3dim_dataset( old_dset , False );  old_dset = NULL ;
  
  nbad = 0;
  
  /*----- loop over new sub-brick index, attach data array with 
          EDIT_substitute_brick then put some strings into the labels and 
          keywords, and modify the sub-brick scaling factor -----*/
  for (ib = 0;  ib < nbricks;  ib++)
    {
      /*----- get information about this sub-brick -----*/
      brick_type  = option_data->brick_type[ib];
      brick_coef  = option_data->brick_coef[ib];
      brick_label = option_data->brick_label[ib];

      if (brick_type == FUNC_FIM_TYPE)
     {     
       if (brick_coef < q)
         volume = ncoef_vol[brick_coef];
       else if (brick_coef < p+q)
         volume = scoef_vol[brick_coef-q];
       else if (brick_coef == p+q)
         volume = tmax_vol;
       else if (brick_coef == p+q+1)
         volume = smax_vol;
       else if (brick_coef == p+q+2)
         volume = pmax_vol;
       else if (brick_coef == p+q+3)
         volume = area_vol;
       else if (brick_coef == p+q+4)
         volume = parea_vol;
       else if (brick_coef == p+q+5)
         volume = rmsreg_vol;
       else if (brick_coef == p+q+6)
         volume = rsqr_vol;
     }
      else  if (brick_type == FUNC_TT_TYPE)
     {     
       if (brick_coef < q)
         volume = tncoef_vol[brick_coef];
       else if (brick_coef < p+q)
         volume = tscoef_vol[brick_coef-q];
       EDIT_BRICK_TO_FITT (new_dset, ib, dendof);
     }
      else  if (brick_type == FUNC_FT_TYPE)
     {
       volume = freg_vol;
       EDIT_BRICK_TO_FIFT (new_dset, ib, numdof, dendof);
     }

      nbad += thd_floatscan( nxyz , volume ) ;

      /*----- allocate memory for output sub-brick -----*/
      if(output_datum==MRI_short) {
	 bar[ib]  = (short *) malloc (sizeof(short) * nxyz);
	 MTEST (bar[ib]);
	 factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
                              MRI_short, bar[ib]);
	 imptr = bar[ib];
      }
      else {
	 far[ib]  = (float *) malloc (sizeof(float) * nxyz);
	 MTEST (far[ib]);
	 factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
                              output_datum, far[ib]);
	 imptr = far[ib];
      }
     if (factor < EPSILON)  factor = 0.0;
     else factor = 1.0 / factor;

     if( output_datum == MRI_short )
       EDIT_misfit_report( DSET_FILECODE(new_dset) , ib ,
                           nxyz , factor , imptr , volume ) ;

      
      /*----- edit the sub-brick -----*/
      EDIT_BRICK_LABEL (new_dset, ib, brick_label);
      EDIT_BRICK_FACTOR (new_dset, ib, factor);
      
      /*----- attach image pointer to be sub-brick #ib -----*/
      EDIT_substitute_brick (new_dset, ib, output_datum, imptr);

    }

  if(nbad)
    fprintf(stderr,"++ %d bad floating point values in dataset\n",nbad);

  /*----- write bucket data set -----*/

  INFO_message("Writing bucket dataset: %s",DSET_BRIKNAME(new_dset)) ;

  if( do_FDR && !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
  { int ii = THD_create_all_fdrcurves( new_dset ) ;
    if( ii > 0 ) ININFO_message("created %d FDR curves in header",ii) ;
  }

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  
  /*----- deallocate memory -----*/   
 /* if(output_datum==MRI_short) {*/
    THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  /*}  */

}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI 3d+time data set. 
*/


void write_3dtime 
(
  char * input_filename,           /* file name of input 3d+time dataset */
  int ts_length,                   /* length of time series data */  
  float ** vol_array,              /* output time series volume data */
  char * output_filename           /* output afni data set file name */
)

{
  const float EPSILON = 1.0e-10;

  THD_3dim_dataset * dset = NULL;        /* input afni data set pointer */
  THD_3dim_dataset * new_dset = NULL;    /* output afni data set pointer */
  int ib;                                /* sub-brick index */ 
  int ierror;                            /* number of errors in editing data */
  int nxyz;                              /* total number of voxels */ 
  float factor;             /* factor is new scale factor for sub-brick #ib */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */
  float ** far = NULL;      /* far[ib] points to data for sub-brick #ib */
  float * fbuf;             /* float buffer */
  float * volume;           /* pointer to volume of data */
  char label[80];           /* label for output file history */ 
  int nbad;                 /* number of voxels in volume with bad floating point values */  

  /*----- Initialize local variables -----*/
  dset = THD_open_dataset (input_filename); /* was THD_open_one...*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;

 
  /*----- allocate memory -----*/
  if(output_datum==MRI_short) {
     bar  = (short **) malloc (sizeof(short *) * ts_length);   MTEST (bar);
     }
  else {
     far  = (float **) malloc (sizeof(float *) * ts_length);   MTEST (far);
     }
  
  fbuf = (float *)  malloc (sizeof(float)   * ts_length);   MTEST (fbuf);
  for (ib = 0;  ib < ts_length;  ib++)    fbuf[ib] = 0.0;

  
  /*-- make an empty copy of the prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History( dset , new_dset ) ;
  if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;
  sprintf (label, "Output prefix: %s", output_filename);
  tross_Append_History ( new_dset, label);


  /*----- delete prototype dataset -----*/
  THD_delete_3dim_dataset (dset, False);  dset = NULL ;
  

  ierror = EDIT_dset_items (new_dset,
                   ADN_prefix,      output_filename,
                   ADN_label1,      output_filename,
                   ADN_self_name,   output_filename,
                   ADN_malloc_type, DATABLOCK_MEM_MALLOC,  
                   ADN_datum_all,   output_datum,   
                   ADN_nvals,       ts_length,
                   ADN_ntt,         ts_length,
                   ADN_none);
 
  if( ierror > 0 ){
    fprintf(stderr,
          "*** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
         "*** Output dataset file %s already exists--cannot continue!\a\n",
         new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  nbad = 0;
  
  /*----- attach bricks to new data set -----*/
  for (ib = 0;  ib < ts_length;  ib++)
    {

      /*----- Set pointer to appropriate volume -----*/
      volume = vol_array[ib];
      nbad += thd_floatscan( nxyz , volume ) ;
      if(output_datum==MRI_short) {
	 /*----- Allocate memory for output sub-brick -----*/
	 bar[ib]  = (short *) malloc (sizeof(short) * nxyz);
	 MTEST (bar[ib]);

	 /*----- Convert data type to short for this sub-brick -----*/
	 factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
                              output_datum, bar[ib]);
	 if (factor < EPSILON)  factor = 0.0;
	 else factor = 1.0 / factor;
	 fbuf[ib] = factor;
         /*----- attach bar[ib] to be sub-brick #ib -----*/
         mri_fix_data_pointer (bar[ib], DSET_BRICK(new_dset,ib)); 

       EDIT_misfit_report( DSET_FILECODE(new_dset) , ib ,
                           nxyz , factor , bar[ib] , volume ) ;
      }
      else {
	 /*----- Allocate memory for output sub-brick -----*/
	 far[ib]  = (float *) malloc (sizeof(float) * nxyz);
	 MTEST (far[ib]);

	 /*----- Convert data type to float for this sub-brick -----*/
	 factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
                              output_datum, far[ib]);
	 if (factor < EPSILON)  factor = 0.0;
	 else factor = 1.0 / factor;
	 fbuf[ib] = factor;
         /*----- attach far[ib] to be sub-brick #ib -----*/
         mri_fix_data_pointer (far[ib], DSET_BRICK(new_dset,ib)); 
      }
    }
  if(nbad)
    WARNING_message("%d bad floating point values in dataset\n",nbad);

  /*----- write afni data set -----*/

  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  INFO_message("Writing 3D+time dataset %s\n",DSET_BRIKNAME(new_dset)) ;

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset (NULL, NULL, new_dset, True);


  /*----- deallocate memory -----*/   
/*  if(output_datum==MRI_short) {*/
     THD_delete_3dim_dataset (new_dset, False);   new_dset = NULL ;
     free (fbuf);   fbuf = NULL;
/*  }*/
}


/*---------------------------------------------------------------------------*/
/*
  Write out the user requested output files.
*/

void output_results 
(
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int  nxyz,              /* number of voxels in image */
  int  ts_length,         /* length of time series data */  
  float * rmsreg_vol,     /* root-mean-square error for the full model */
  float * freg_vol,       /* f-statistic for the full regression model */
  float * rsqr_vol,       /* R^2 volume data */
  float * smax_vol,       /* signed maximum of signal volume data */
  float * tmax_vol,       /* epoch of signed maximum volume data */
  float * pmax_vol,       /* percentage change in signal volume data */
  float * area_vol,       /* area underneath signal volume data */
  float * parea_vol,      /* percentage area underneath signal volume data */
  float ** ncoef_vol,     /* volume of noise model parameters */
  float ** scoef_vol,     /* volume of signal model parameters */
  float ** tncoef_vol,    /* volume of noise model t-statistics */
  float ** tscoef_vol,    /* volume of signal model t-statistics */
  float ** sfit_vol,      /* voxelwise 3d+time fitted signal model */ 
  float ** snfit_vol,     /* voxelwise 3d+time fitted signal+noise model */ 

  char * input_filename,     /* file name of input 3d+time dataset */
  char * freg_filename,      /* file name for f-statistics */
  char * frsqr_filename,     /* file name for R^2 statistics */
  char * fsmax_filename,     /* file name for signal signed maximum */
  char * ftmax_filename,     /* file name for time of signed maximum */
  char * fpmax_filename,     /* file name for percentage signal change */
  char * farea_filename,     /* file name for area underneath signal */
  char * fparea_filename,    /* file name for % area underneath signal */
  char ** fncoef_filename,   /* file name for noise model parameters */
  char ** fscoef_filename,   /* file name for signal model parameters */
  char ** tncoef_filename,   /* file name for noise model t-statistics */
  char ** tscoef_filename,   /* file name for signal model t-statistics */
  char * sfit_filename,      /* file name for 3d+time fitted signal model */
  char * snfit_filename,     /* file name for 3d+time fitted signal+noise */

  NL_options * option_data   /* user input options */

)

{
  int ip;                 /* parameter index */
  int dimension;          /* dimension of full model = r + p  */
  int numdof, dendof;     /* numerator and denominator degrees of freedom */


  /*----- Initialization -----*/
  dimension = r + p;
  numdof = p;
  dendof = ts_length - dimension;


  /*----- Adjust dof if constraints force a parameter to be a constant -----*/
  for (ip = 0;  ip < r;  ip++)
    if (min_nconstr[ip] == max_nconstr[ip])
      dendof++;

  for (ip = 0;  ip < p;  ip++)
    if (min_sconstr[ip] == max_sconstr[ip])
      {
     numdof--;
     dendof++;
      }


  /*----- write the bucket dataset -----*/
  if (option_data->numbricks > 0)
    write_bucket_data (r, p, numdof, dendof, nxyz, ts_length, rmsreg_vol, 
                 freg_vol, rsqr_vol, smax_vol, tmax_vol, pmax_vol, 
                 area_vol, parea_vol, ncoef_vol, scoef_vol, 
                 tncoef_vol, tscoef_vol, input_filename, option_data);


  /*----- write out the f-statistics for the regression -----*/
  if (freg_filename != NULL)
    write_afni_data (input_filename, nxyz, freg_filename, 
               rmsreg_vol, freg_vol, numdof, dendof); 


  /*----- write out the R^2 (coefficient of multiple determination) -----*/
  if (frsqr_filename != NULL)
    write_afni_data (input_filename, nxyz, frsqr_filename, 
               rsqr_vol, freg_vol, numdof, dendof); 


  /*----- write out the signed maximum of signal estimates -----*/
  if (fsmax_filename != NULL)
    write_afni_data (input_filename, nxyz, fsmax_filename, 
               smax_vol, freg_vol, numdof, dendof); 
  

  /*----- write out the epoch of signed maximum of signal estimates -----*/
  if (ftmax_filename != NULL)
    write_afni_data (input_filename, nxyz, ftmax_filename, 
               tmax_vol, freg_vol, numdof, dendof); 


  /*----- write out the max. percentage change due to signal -----*/
  if (fpmax_filename != NULL)
    write_afni_data (input_filename, nxyz, fpmax_filename, 
               pmax_vol, freg_vol, numdof, dendof); 


  /*----- write out the area between the signal and baseline -----*/
  if (farea_filename != NULL)
    write_afni_data (input_filename, nxyz, farea_filename, 
               area_vol, freg_vol, numdof, dendof); 


  /*----- write out the percentage area between the signal and baseline -----*/
  if (fparea_filename != NULL)
    write_afni_data (input_filename, nxyz, fparea_filename, 
               parea_vol, freg_vol, numdof, dendof); 


  /*----- write noise model parameters -----*/
  for (ip = 0;  ip < r;  ip++)
    {
      if (tncoef_filename[ip] != NULL)
     write_afni_data (input_filename, nxyz, tncoef_filename[ip], 
                ncoef_vol[ip], tncoef_vol[ip], dendof, 0); 

      if (fncoef_filename[ip] != NULL)
     write_afni_data (input_filename, nxyz, fncoef_filename[ip], 
                ncoef_vol[ip], freg_vol, numdof, dendof); 
    }


  /*----- write signal model parameters -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      if (tscoef_filename[ip] != NULL)
     write_afni_data (input_filename, nxyz, tscoef_filename[ip], 
                scoef_vol[ip], tscoef_vol[ip], dendof, 0); 

      if (fscoef_filename[ip] != NULL)
     write_afni_data (input_filename, nxyz, fscoef_filename[ip], 
                scoef_vol[ip], freg_vol, numdof, dendof); 
    }


  /*----- write the fitted 3d+time signal model -----*/
  if (sfit_filename != NULL)
    {
      write_3dtime (input_filename, ts_length, sfit_vol, sfit_filename);
    }


  /*----- write the fitted 3d+time signal+noise model -----*/
  if (snfit_filename != NULL)
    {
      write_3dtime (input_filename, ts_length, snfit_vol, snfit_filename);
    }

}


/*---------------------------------------------------------------------------*/
/*
  Release all allocated memory space.
*/

void terminate_program 
(
  int r,                       /* number of parameters in the noise model */
  int p,                       /* number of parameters in the signal model */
  int ts_length,               /* length of time series data */
  float *** x_array,           /* independent variable matrix */
  float ** ts_array,           /* input time series array */
  char  ** nname,         /* noise model name */
  char  *** npname,       /* noise parameter labels */
  float ** par_rdcd,      /* estimated parameters for the reduced model */
  float ** min_nconstr,   /* min parameter constraints for noise model */
  float ** max_nconstr,   /* max parameter constraints for noise model */
  char ** sname,          /* signal model name */
  char *** spname,        /* signal parameter labels */
  float ** par_full,      /* estimated parameters for the full model */
  float ** tpar_full,     /* t-statistic of parameters in full model */
  float ** min_sconstr,   /* min parameter constraints for signal model */
  float ** max_sconstr,   /* max parameter constraints for signal model */

  float ** rmsreg_vol,        /* rms error for the full regression model */
  float ** freg_vol,          /* f-statistic for the full regression model */
  float ** rsqr_vol,          /* R^2 volume data */
  float ** smax_vol,          /* signed max. of signal volume data */
  float ** tmax_vol,          /* epoch of signed max. volume data */
  float ** pmax_vol,          /* percentage change in signal volume data */
  float ** area_vol,          /* area underneath signal volume data */
  float ** parea_vol,         /* percent area underneath signal volume data */
  float *** ncoef_vol,        /* noise model parameters volume data */
  float *** scoef_vol,        /* signal model parameters volume data */
  float *** tncoef_vol,       /* noise model t-statistics volume data */
  float *** tscoef_vol,       /* signal model t-statistics volume data */
  float *** sfit_vol,         /* voxelwise 3d+time fitted signal model */ 
  float *** snfit_vol,        /* voxelwise 3d+time fitted signal+noise */ 

  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** freg_filename,      /* file name for regression f-statistics */
  char ** frsqr_filename,     /* file name for R^2 statistics */
  char ** fsmax_filename,     /* file name for signal signed maximum */
  char ** ftmax_filename,     /* file name for epoch of signed maximum */
  char ** fpmax_filename,     /* file name for percentage signal change */
  char ** farea_filename,     /* file name for area underneath signal */
  char ** fparea_filename,    /* file name for % area underneath signal */
  char *** fncoef_filename,   /* file name for noise model parameters */
  char *** fscoef_filename,   /* file name for signal model parameters */
  char *** tncoef_filename,   /* file name for noise model t-statistics */
  char *** tscoef_filename,   /* file name for signal model t-statistics */
  char ** sfit_filename,      /* file name for 3d+time fitted signal model */
  char ** snfit_filename      /* file name for 3d+time fitted signal+noise */
)
 
{
  int ip;                        /* parameter index */
  int it;                        /* time index */


  /*----- deallocate space for model names and parameters labels -----*/
  if (*nname != NULL)
    { free (*nname);  *nname = NULL; }
  if (*sname != NULL)
    { free (*sname);  *sname = NULL; }
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      if ((*npname)[ip] != NULL)
     { free ((*npname)[ip]);  (*npname)[ip] = NULL; }
      if ((*spname)[ip] != NULL)
     { free ((*spname)[ip]);  (*spname)[ip] = NULL; }
    }

  if (*npname != NULL)
    { free (*npname);  *npname = NULL; }
  if (*spname != NULL)
    { free (*spname);  *spname = NULL; }


  /*----- deallocate memory for parameter constraints -----*/
  if (*min_nconstr != NULL)  { free (*min_nconstr);  *min_nconstr = NULL; }
  if (*max_nconstr != NULL)  { free (*max_nconstr);  *max_nconstr = NULL; }
  if (*min_sconstr != NULL)  { free (*min_sconstr);  *min_sconstr = NULL; }
  if (*max_sconstr != NULL)  { free (*max_sconstr);  *max_sconstr = NULL; }


  /*----- deallocate memory space for filenames -----*/
  if (*input_filename != NULL)  
    { free (*input_filename);   *input_filename = NULL; }
  if (*freg_filename != NULL)
    { free (*freg_filename);    *freg_filename = NULL; }
  if (*frsqr_filename != NULL)
    { free (*frsqr_filename);   *frsqr_filename = NULL; }
  if (*fsmax_filename != NULL)
    { free (*fsmax_filename);   *fsmax_filename = NULL; }
  if (*ftmax_filename != NULL)
    { free (*ftmax_filename);   *ftmax_filename = NULL; }
  if (*fpmax_filename != NULL)
    { free (*fpmax_filename);   *fpmax_filename = NULL; }
  if (*farea_filename != NULL)
    { free (*farea_filename);   *farea_filename = NULL; }
  if (*fparea_filename != NULL)
    { free (*fparea_filename);   *fparea_filename = NULL; }

  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      if ((*fncoef_filename)[ip] != NULL)
     { free ((*fncoef_filename)[ip]);  (*fncoef_filename)[ip] = NULL; } 
      if ((*fscoef_filename)[ip] != NULL)
     { free ((*fscoef_filename)[ip]);  (*fscoef_filename)[ip] = NULL; } 
      if ((*tncoef_filename)[ip] != NULL)
     { free ((*tncoef_filename)[ip]);  (*tncoef_filename)[ip] = NULL; } 
      if ((*tscoef_filename)[ip] != NULL)
     { free ((*tscoef_filename)[ip]);  (*tscoef_filename)[ip] = NULL; } 
    }

  if (*fncoef_filename != NULL)
    { free (*fncoef_filename);  *fncoef_filename = NULL; } 
  if (*fscoef_filename != NULL)
    { free (*fscoef_filename);  *fscoef_filename = NULL; } 
  if (*tncoef_filename != NULL)
    { free (*tncoef_filename);  *tncoef_filename = NULL; } 
  if (*tscoef_filename != NULL)
    { free (*tscoef_filename);  *tscoef_filename = NULL; } 

  if (*sfit_filename != NULL)
    { free (*sfit_filename);    *sfit_filename = NULL; } 
  if (*snfit_filename != NULL)
    { free (*snfit_filename);   *snfit_filename = NULL; } 


  /*----- deallocate space for input time series -----*/
  if (*ts_array != NULL)    { free (*ts_array);    *ts_array = NULL; }


  /*----- deallocate space for independent variable matrix -----*/
  for (it = 0;  it < ts_length;  it++)
    if ((*x_array)[it] != NULL)
      { free ((*x_array)[it]);  (*x_array)[it] = NULL; }
  if (*x_array != NULL)     { free (*x_array);  *x_array = NULL; }


  /*----- deallocate space for parameters -----*/
  if (*par_rdcd != NULL)    { free (*par_rdcd);    *par_rdcd = NULL; }
  if (*par_full != NULL)    { free (*par_full);    *par_full = NULL; }
  if (*tpar_full != NULL)   { free (*tpar_full);   *tpar_full = NULL; }


  /*----- deallocate space for volume data -----*/
  if (*rmsreg_vol != NULL)   { free (*rmsreg_vol);  *rmsreg_vol = NULL; }
  if (*freg_vol   != NULL)   { free (*freg_vol);    *freg_vol = NULL; } 
  if (*rsqr_vol   != NULL)   { free (*rsqr_vol);    *rsqr_vol = NULL; } 
  if (*smax_vol   != NULL)   { free (*smax_vol);    *smax_vol = NULL; } 
  if (*tmax_vol   != NULL)   { free (*tmax_vol);    *tmax_vol = NULL; } 
  if (*pmax_vol   != NULL)   { free (*pmax_vol);    *pmax_vol = NULL; } 
  if (*area_vol   != NULL)   { free (*area_vol);    *area_vol = NULL; } 
  if (*parea_vol  != NULL)   { free (*parea_vol);   *parea_vol = NULL; } 

  if (*ncoef_vol != NULL)
    {
      for (ip = 0;  ip < r;  ip++)
     if ((*ncoef_vol)[ip] != NULL)
       { free ((*ncoef_vol)[ip]);  (*ncoef_vol)[ip] = NULL; }
      free (*ncoef_vol);  *ncoef_vol = NULL;
    }

  if (*tncoef_vol != NULL)  
    {    
      for (ip = 0;  ip < r;  ip++)
     if ((*tncoef_vol)[ip] != NULL)      
       { free ((*tncoef_vol)[ip]);  (*tncoef_vol)[ip] = NULL; }
      free (*tncoef_vol);  *tncoef_vol = NULL;
    }
  
  if (*scoef_vol != NULL)
    {
      for (ip = 0;  ip < p;  ip++)
     if ((*scoef_vol)[ip] != NULL)
       { free ((*scoef_vol)[ip]);  (*scoef_vol)[ip] = NULL; }
      free (*scoef_vol);  *scoef_vol = NULL; 
    }

  if (*tscoef_vol != NULL)      
    {
      for (ip = 0;  ip < p;  ip++)
     if ((*tscoef_vol)[ip] != NULL)      
       { free ((*tscoef_vol)[ip]);  (*tscoef_vol)[ip] = NULL; }
      free (*tscoef_vol);  *tscoef_vol = NULL; 
    }
  
  if (*sfit_vol != NULL)
    {
      for (it = 0;  it < ts_length;  it++)
     if ((*sfit_vol)[it] != NULL)
       { free ((*sfit_vol)[it]);  (*sfit_vol)[it] = NULL; }
      free (*sfit_vol);  *sfit_vol = NULL; 
    }

  if (*snfit_vol != NULL)
    {
      for (it = 0;  it < ts_length;  it++)
     if ((*snfit_vol)[it] != NULL)
       { free ((*snfit_vol)[it]);  (*snfit_vol)[it] = NULL; }
      free (*snfit_vol);  *snfit_vol = NULL; 
    }

}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  /*----- declare input option variables -----*/
  NL_options option_data;  /* bucket dataset options */
  int nabs;                /* use absolute constraints for noise parameters */
  int nrand;               /* number of random vectors to generate */
  int nbest;               /* number of random vectors to keep */
  float rms_min;           /* minimum rms error to reject reduced model */
  float fdisp;             /* minimum f-statistic for display */ 
  int progress;            /* show progress report every n voxels */

  /*----- declare time series variables -----*/
  THD_3dim_dataset * dset_time = NULL;      /* input 3d+time data set */
  int ts_length;                            /* length of time series data */
  int ignore;              /* delete this number of points from time series */
  float ** x_array = NULL;                  /* independent variable matrix */
  float * ts_array = NULL;                  /* input time series array */
  int nxyz;                                 /* number of voxels in image */
  int iv;                                   /* voxel counter */

  /*----- declare reduced (noise) model variables -----*/
  char * nname = NULL;         /* noise model name */
  vfp nmodel;                  /* pointer to noise model */
  int r;                       /* number of parameters in the noise model */
  char ** npname = NULL;       /* noise parameter labels */
  float * par_rdcd = NULL;     /* estimated parameters for the reduced model */
  float sse_rdcd;              /* error sum of squares for the reduced model */
  float * min_nconstr = NULL;  /* min parameter constraints for noise model */
  float * max_nconstr = NULL;  /* max parameter constraints for noise model */

  /*----- declare full (signal+noise) model variables -----*/
  char * sname = NULL;         /* signal model name */
  vfp smodel;                  /* pointer to signal model */
  int p;                       /* number of parameters in the signal model */
  char ** spname = NULL;       /* signal parameter labels */
  float * par_full = NULL;     /* estimated parameters for the full model */
  float sse_full;              /* error sum of squares for the full model */
  float * tpar_full = NULL;    /* t-statistic of parameters in full model */
  float freg;                  /* f-statistic for the full regression model */
  float rmsreg;                /* rms error for the full regression model */
  float rsqr;                  /* R^2 (coef. of multiple determination) */
  float smax;                  /* signed maximum of signal */
  float tmax;                  /* epoch of signed maximum of signal */
  float pmax;                  /* percentage change due to signal */
  float area;                  /* area between signal and baseline */
  float parea;                 /* percent area between signal and baseline */
  float * min_sconstr = NULL;  /* min parameter constraints for signal model */
  float * max_sconstr = NULL;  /* max parameter constraints for signal model */

  /*----- declare output volume data -----*/
  float * rmsreg_vol = NULL;    /* rms error for the full regression model */
  float * freg_vol = NULL;      /* f-statistic for the full regression model */
  float * rsqr_vol = NULL;      /* R^2 volume data */
  float * smax_vol = NULL;      /* signed max. of signal volume data */
  float * tmax_vol = NULL;      /* epoch of signed max. volume data */
  float * pmax_vol = NULL;      /* max. percentage change due to signal */
  float * area_vol = NULL;      /* area between signal and baseline */
  float * parea_vol = NULL;     /* percent area between signal and baseline */
  float ** ncoef_vol = NULL;    /* noise model parameters volume data */
  float ** scoef_vol = NULL;    /* signal model parameters volume data */
  float ** tncoef_vol = NULL;   /* noise model t-statistics volume data */
  float ** tscoef_vol = NULL;   /* signal model t-statistics volume data */
  float ** sfit_vol = NULL;     /* voxelwise 3d+time fitted signal model */ 
  float ** snfit_vol = NULL;    /* voxelwise 3d+time fitted signal+noise */ 

  /*----- declare file name variables -----*/
  char * input_filename = NULL;   /* file name of input 3d+time dataset */
  char * tfilename = NULL;        /* file name of time points */
  char * freg_filename = NULL;    /* file name for regression f-statistics */
  char * frsqr_filename= NULL;    /* file name for R^2 statistics */
  char * fsmax_filename = NULL;   /* file name for signal signed maximum */
  char * ftmax_filename = NULL;   /* file name for time of signed maximum */
  char * fpmax_filename = NULL;   /* file name for max. percentage change */
  char * farea_filename = NULL;   /* file name for area under the signal */
  char * fparea_filename = NULL;  /* file name for % area under the signal */
  char ** fncoef_filename = NULL; /* file name for noise model parameters */
  char ** fscoef_filename = NULL; /* file name for signal model parameters */
  char ** tncoef_filename = NULL; /* file name for noise model t-statistics */
  char ** tscoef_filename = NULL; /* file name for signal model t-statistics */
  char * sfit_filename = NULL;    /* file name for fitted signal model */
  char * snfit_filename = NULL;   /* file name for fitted signal+noise model */
  
  char * label;            /* report results for one voxel */
  int novar;               /* flag for insufficient variation in the data */

  int ixyz_bot , ixyz_top ;
  int voxel_count_index = 0 ;     /* for g_voxel_count output  26 Oct 2006 */

  /*----- start the elapsed time counter -----*/
  (void) COX_clock_time() ;
  
  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/
   PRINT_VERSION("3dNLfim") ; AUTHOR(PROGRAM_AUTHOR);
   mainENTRY("3dNLfim main") ; machdep() ;

  { 
    int new_argc ; char ** new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }

   
  /*----- program initialization -----*/
  initialize_program (argc, argv, &ignore, 
                &nname, &sname, &nmodel, &smodel, 
                &r, &p, &npname, &spname,
                &min_nconstr, &max_nconstr, &min_sconstr, &max_sconstr,
                &nabs, &nrand, &nbest, &rms_min, &fdisp,&progress, 
                &input_filename, &tfilename, 
                &freg_filename, &frsqr_filename,
                &fsmax_filename, &ftmax_filename, &fpmax_filename,
                &farea_filename, &fparea_filename, &fncoef_filename,
                &fscoef_filename, &tncoef_filename, &tscoef_filename,
                &sfit_filename, &snfit_filename, 
                &dset_time, &nxyz, &ts_length, &x_array, &ts_array, 
                &par_rdcd, &par_full, &tpar_full, 
                &rmsreg_vol, &freg_vol, &rsqr_vol,
                &smax_vol, &tmax_vol, &pmax_vol, &area_vol, &parea_vol, 
                &ncoef_vol, &scoef_vol, &tncoef_vol, &tscoef_vol,
                &sfit_vol, &snfit_vol, &option_data);

#if 0
#ifdef SAVE_RAN
   RAN_setup( nmodel , smodel , r , p , nabs ,
              min_nconstr, max_nconstr,
              min_sconstr, max_sconstr,
              ts_length, x_array, nrand ) ;
#endif
#endif

   INFO_message(
     "At each voxel, will use %d best of %d random parameter sets",
     nbest , nrand ) ;

   ixyz_bot = 0 ; ixyz_top = nxyz ;  /* RWCox */

#ifdef PROC_MAX
   if( proc_numjob > 1 ){    /*---- set up multiple processes ----*/
     int vv , nvox=nxyz , nper , pp , nv ;
     pid_t newpid ;

     /* count number of voxels to compute with into nvox */

     if( mask_vol != NULL ){
       for( vv=nvox=0 ; vv < nxyz ; vv++ )
         if( mask_vol[vv] != 0 ) nvox++ ;
     }

     if( nvox < proc_numjob ){  /* too few voxels for multiple jobs? */

       proc_numjob = 1 ;

     } else {                   /* prepare jobs */

       /* split voxels between jobs evenly */

#if 0
       if( g_voxel_count ){
         g_voxel_count = 0 ;
         WARNING_message("-voxel_count disabled by -jobs") ;
       }
#endif

       nper = nvox / proc_numjob ;  /* # voxels per job */
       if( mask_vol == NULL ){
         proc_vox_bot[0] = 0 ;
         for( pp=0 ; pp < proc_numjob ; pp++ ){
           proc_vox_top[pp] = proc_vox_bot[pp] + nper ;
           if( pp < proc_numjob-1 ) proc_vox_bot[pp+1] = proc_vox_top[pp] ;
         }
         proc_vox_top[proc_numjob-1] = nxyz ;
       } else {
         proc_vox_bot[0] = 0 ;
         for( pp=0 ; pp < proc_numjob ; pp++ ){
           for( nv=0,vv=proc_vox_bot[pp] ;         /* count ahead until */
                nv < nper && vv < nxyz  ; vv++ ){  /* find nper voxels */
             if( mask_vol[vv] != 0 ) nv++ ;        /* inside the mask */
           }
           proc_vox_top[pp] = vv ;
           if( pp < proc_numjob-1 ) proc_vox_bot[pp+1] = proc_vox_top[pp] ;
         }
         proc_vox_top[proc_numjob-1] = nxyz ;
       }

       /* make sure dataset is in memory before forks */

       DSET_load(dset_time) ;  /* so dataset will be common */

       /* start processes */

       fprintf(stderr,"++ Voxels in dataset: %d\n",nxyz) ;
       if( nvox < nxyz )
       fprintf(stderr,"++ Voxels in mask:    %d\n",nvox) ;
       fprintf(stderr,"++ Voxels per job:    %d\n",nper) ;

       for( pp=1 ; pp < proc_numjob ; pp++ ){
         ixyz_bot = proc_vox_bot[pp] ;   /* these 3 variables   */
         ixyz_top = proc_vox_top[pp] ;   /* are for the process */
         proc_ind = pp ;                 /* we're about to fork */
         newpid   = fork() ;
         if( newpid == -1 ){
           fprintf(stderr,"** Can't fork job #%d! Error exit!\n",pp);
           exit(1) ;
         }
         if( newpid == 0 ) break ;   /* I'm the child */
         proc_pid[pp] = newpid ;     /* I'm the parent */
         iochan_sleep(10) ;
       }
       if( pp == proc_numjob ){       /* only in the parent */
         ixyz_bot = proc_vox_bot[0] ; /* set the 3 control */
         ixyz_top = proc_vox_top[0] ; /* variables needed */
         proc_ind = 0 ;               /* below           */
       }
       fprintf(stderr,"++ Job #%d: processing voxels %d to %d; elapsed time=%.3f\n",
               proc_ind,ixyz_bot,ixyz_top-1,COX_clock_time()) ;
     }
   }
#endif /* PROC_MAX */

   if( proc_numjob == 1 )
     fprintf(stderr,"++ Calculations starting; elapsed time=%.3f\n",
             COX_clock_time()) ;


  /*----- loop over voxels in the data set -----*/
  for (iv = ixyz_bot;  iv < ixyz_top;  iv++)
    {
      /*----- check for mask -----*/
      if (mask_vol != NULL)
     if (mask_vol[iv] == 0)  continue;

      /*----- display progress for user (1-based) -----*/
      if (g_voxel_count && proc_ind == 0 )
      {
        /* only print every 100th         26 Oct 2006 [rickr] */
        if( voxel_count_index % 100 == 0 )
          fprintf(stderr,"\r++ voxel count: %8d (of %d)", iv+1, ixyz_top);
        voxel_count_index++ ;
      }
 
      AFNI_store_dset_index( iv, -1); /* set voxel index 03 Nov 2006 */
      /*----- read the time series for voxel iv -----*/
      read_ts_array (dset_time, iv, ts_length, ignore, ts_array);
 

      /*----- calculate the reduced (noise) model -----*/
STATUS("call calc_reduced_model") ;
      calc_reduced_model (ts_length, r, x_array, ts_array, 
                 par_rdcd, &sse_rdcd);


      /*----- calculate the full (signal+noise) model -----*/
STATUS("call calc_full_model") ;
      calc_full_model (nmodel, smodel, r, p,  
                 min_nconstr, max_nconstr, min_sconstr, max_sconstr,
                 ts_length, x_array, ts_array, par_rdcd, sse_rdcd, nabs,
                 nrand, nbest, rms_min, par_full, &sse_full, &novar);


      /*----- calculate statistics for the full model -----*/
STATUS("call analyze_results") ;
      analyze_results (nmodel, smodel, r, p, novar,
                 min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
                 ts_length, x_array,
                 par_rdcd, sse_rdcd, par_full, sse_full,
                 &rmsreg, &freg, &rsqr, &smax, &tmax, &pmax, 
                 &area, &parea, tpar_full);


      /*----- report results for this voxel -----*/
      if ((freg >= fdisp && proc_ind == 0 )
        && (iv % progress == 0))
       {
	 report_results (nname, sname, r, p, npname, spname, ts_length,
                   par_rdcd, sse_rdcd, par_full, sse_full, tpar_full,
                   rmsreg, freg, rsqr, smax, tmax, pmax, 
                   area, parea, &label);
	 printf ("\n\nVoxel #%d\n", iv);
	 printf ("%s \n", label);
       }

      /*----- save results for this voxel into volume data -----*/
      save_results (iv, nmodel, smodel, r, p, novar, ts_length, x_array, 
              par_full, tpar_full, rmsreg, freg, rsqr, smax, 
              tmax, pmax, area, parea, rmsreg_vol, 
              freg_vol, rsqr_vol, smax_vol, 
              tmax_vol, pmax_vol, area_vol, parea_vol,ncoef_vol, 
              scoef_vol, tncoef_vol, tscoef_vol, sfit_vol, snfit_vol);
    }
    if(g_voxel_count) fputc('\n',stderr);


    /*-- if this is a child process, we're done.
         if this is the parent process, wait for the children --*/

#ifdef PROC_MAX
    if( proc_numjob > 1 ){
     if( proc_ind > 0 ){                          /* death of child */
       fprintf(stderr,"++ Job #%d finished; elapsed time=%.3f\n",
               proc_ind,COX_clock_time()) ;
       _exit(0) ;

     } else {                      /* parent waits for children */
       int pp ;
       fprintf(stderr,"++ Job #0 waiting for children to finish; elapsed time=%.3f\n",COX_clock_time()) ;
       for( pp=1 ; pp < proc_numjob ; pp++ )
         waitpid( proc_pid[pp] , NULL , 0 ) ;
       fprintf(stderr,"++ Job #0 now finishing up; elapsed time=%.3f\n",
               COX_clock_time()) ;
     }

     /* when get to here, only parent process is left alive,
        and all the results are in the shared memory segment arrays */
   }
#endif
   if( proc_numjob == 1 )
     fprintf(stderr,"++ Calculations finished; elapsed time=%.3f\n",
             COX_clock_time()) ;


  /*----- delete input dataset -----*/ 
  THD_delete_3dim_dataset( dset_time , False ) ;  dset_time = NULL ;


  /*----- write requested output files -----*/
  output_results (r, p, min_nconstr, max_nconstr, min_sconstr, max_sconstr,
            nxyz, ts_length, rmsreg_vol, freg_vol, rsqr_vol, 
            smax_vol, tmax_vol, pmax_vol, area_vol, parea_vol, 
            ncoef_vol, scoef_vol, tncoef_vol, tscoef_vol, 
            sfit_vol, snfit_vol,
            input_filename, freg_filename, frsqr_filename,
             fsmax_filename, ftmax_filename, 
            fpmax_filename, farea_filename, fparea_filename, 
            fncoef_filename, fscoef_filename, 
            tncoef_filename, tscoef_filename, 
            sfit_filename, snfit_filename, &option_data);
           

  /*----- end of program -----*/
  terminate_program (r, p, ts_length, &x_array, &ts_array, 
               &nname, &npname, &par_rdcd, &min_nconstr, &max_nconstr, 
               &sname, &spname, &par_full, &tpar_full, 
               &min_sconstr, &max_sconstr, 
               &rmsreg_vol, &freg_vol, &rsqr_vol, 
               &smax_vol, &tmax_vol, &pmax_vol, &area_vol, &parea_vol,
               &ncoef_vol, &scoef_vol, &tncoef_vol, &tscoef_vol,
               &sfit_vol, &snfit_vol, &input_filename, 
               &freg_filename, &frsqr_filename, 
               &fsmax_filename, &ftmax_filename, 
               &fpmax_filename, &farea_filename, &fparea_filename,
               &fncoef_filename, &fscoef_filename, 
               &tncoef_filename, &tscoef_filename, 
               &sfit_filename, &snfit_filename);

  if( nwin_pow > 0 && (nwin_sim > 0 || nwin_stp > 0) )
    INFO_message(
     "# best via POWELL=%d; via SIMPLEX=%d; SIMPLEX then POWELL=%d",
     nwin_pow , nwin_sim , nwin_stp ) ;

  INFO_message("Program finished; elapsed time=%.3f\n",COX_clock_time()) ;
  exit (0);
}
