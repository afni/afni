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

*/


/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dNLfim"                       /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "08 February 2000"          /* date of last program mod */

/*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mrilib.h"

#include "matrix.h"
#include "simplex.h"
#include "NLfit.h"

#include "matrix.c"
#include "simplex.c"
#include "NLfit.c"


typedef struct NL_options
{ 
  char * bucket_filename;      /* file name for bucket dataset */
  int numbricks;               /* number of sub-bricks in bucket dataset */
  int * brick_type;            /* indicates type of sub-brick */
  int * brick_coef;            /* regression coefficient number for sub-brick*/
  char ** brick_label;         /* character string label for sub-brick */

} NL_options;

/***** 22 July 1998 -- RWCox:
       Modified to allow DELT to be set from the TR of the input file *****/

static float DELT = 1.0;   /* default */
static int   inTR = 0 ;    /* set to 1 if -inTR option is used */
static float dsTR = 0.0 ;  /* TR of the input file */


static char * commandline = NULL ;         /* command line for history notes */


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
     "[-inTR]            set the TR of the created timeseries to be the TR  \n"
     "                     of the prototype dataset                         \n"
     "                     [The default is to compute with TR = 1.]         \n"
     "                     [The model functions are called for a  ]         \n"
     "                     [time grid of 0, TR, 2*TR, 3*TR, ....  ]         \n"
     "-ignore num        num   = skip this number of initial images in the  \n"
     "                     time series for regresion analysis; default = 3  \n"
     "[-time fname]      fname = ASCII file containing each time point      \n"
     "                     in the time series. Defaults to even spacing     \n"
     "                     given by TR (this option overrides -inTR).       \n"
     "-signal slabel     slabel = name of (non-linear) signal model         \n"
     "-noise  nlabel     nlabel = name of (linear) noise model              \n"
     "-sconstr k c d     constraints for kth signal parameter:              \n"
     "                      c <= gs[k] <= d                                 \n"
     "-nconstr k c d     constraints for kth noise parameter:               \n"
     "                      c+b[k] <= gn[k] <= d+b[k]                       \n"
     "[-nabs]            use absolute constraints for noise parameters:     \n"
     "                      c <= gn[k] <= d                                 \n"
     "[-nrand n]         n = number of random test points                   \n"
     "[-nbest b]         b = find opt. soln. for b best test points         \n"
     "[-rmsmin r]        r = minimum rms error to reject reduced model      \n"
     "[-fdisp fval]      display (to screen) results for those voxels       \n"
     "                     whose f-statistic is > fval                      \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following commands generate individual AFNI 2 sub-brick datasets: \n"
     "                                                                      \n"
     "[-freg fname]      perform f-test for significance of the regression; \n"
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
     "                                                                      \n"
     "The following commands generate one AFNI 'bucket' type dataset:       \n"
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
     "[-brick m fstat label]     F-stat for significance of the regression  \n"
     "[-brick m tscoef k label]  t-stat for kth signal parameter coefficient\n"
     "[-brick m tncoef k label]  t-stat for kth noise parameter coefficient \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following commands write the time series fit for each voxel       \n"
     "to an AFNI 3d+time dataset:                                           \n"
     "[-sfit fname]      fname = prefix for output 3d+time signal model fit \n"
     "[-snfit fname]     fname = prefix for output 3d+time signal+noise fit \n"
     "                                                                      \n"
    );
  
  exit(0);
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
  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** tfilename,          /* file name for time point series */  
  char ** freg_filename,      /* file name for regression f-statistics */
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
  *ignore = 3;
  *nabs = 0;
  *nrand = 100;
  *nbest = 5; 
  *rms_min = 0.0;
  *fdisp = 10.0;
  *smodel = NULL;
  *nmodel = NULL;
  *r = -1;
  *p = -1;


  /*----- allocate memory for noise parameter names -----*/
  *npname = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*npname == NULL)  
    NLfit_error ("Unable to allocate memory for noise parameter names");
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*npname)[ip] = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
      if ((*npname)[ip] == NULL)  
	NLfit_error ("Unable to allocate memory for noise parameter names");
    }


  /*----- allocate memory for signal parameter names -----*/
  *spname = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*spname == NULL)  
    NLfit_error ("Unable to allocate memory for signal parameter names");
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*spname)[ip] = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
      if ((*spname)[ip] == NULL)  
	NLfit_error ("Unable to allocate memory for signal parameter names");
    }
  

  /*----- allocate memory for parameter constraints -----*/
  *min_nconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  if (*min_nconstr == NULL)  
    NLfit_error ("Unable to allocate memory for min_nconstr");
  *max_nconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  if (*max_nconstr == NULL)
    NLfit_error ("Unable to allocate memory for max_nconstr");
  *min_sconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  if (*min_sconstr == NULL)  
    NLfit_error ("Unable to allocate memory for min_sconstr");
  *max_sconstr = (float *) malloc (sizeof(float) * MAX_PARAMETERS);
  if (*max_sconstr == NULL)
    NLfit_error ("Unable to allocate memory for max_sconstr");


  /*----- allocate memory space and initialize pointers for filenames -----*/
  *input_filename = NULL;
  *tfilename = NULL;
  *freg_filename = NULL;  
  *fncoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*fncoef_filename == NULL)
    NLfit_error ("Unable to allocate memory for fncoef_filename");
  *fscoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*fscoef_filename == NULL)
    NLfit_error ("Unable to allocate memory for fscoef_filename");
  *tncoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*tncoef_filename == NULL)
    NLfit_error ("Unable to allocate memory for tncoef_filename");
  *tscoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*tscoef_filename == NULL)
    NLfit_error ("Unable to allocate memory for tscoef_filename");
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
  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** tfilename,          /* file name for time point series */  
  char ** freg_filename,      /* file name for regression f-statistics */
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
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  
  
  /*----- initialize the model array -----*/
  model_array = NLFIT_get_many_MODELs ();
  if ((model_array == NULL) || (model_array->num == 0))
    NLfit_error ("Unable to locate any models");

  /*----- initialize the input options -----*/
  initialize_options (ignore, nmodel, smodel, r, p, npname, spname, 
		      min_nconstr, max_nconstr, min_sconstr, max_sconstr, nabs,
		      nrand, nbest, rms_min, fdisp, 
		      input_filename, tfilename, freg_filename, 
		      fncoef_filename, fscoef_filename,
		      tncoef_filename, tscoef_filename,
		      sfit_filename, snfit_filename, option_data); 

  
  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -input filename   -----*/
      if (strncmp(argv[nopt], "-input", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -input ");
	  *input_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*input_filename == NULL)
	    NLfit_error ("Unable to allocate memory for input_filename");
	  strcpy (*input_filename, argv[nopt]);
	  *dset_time = THD_open_one_dataset (*input_filename);
	  if ((*dset_time) == NULL)  
	    { 
	      sprintf (message, 
		       "Unable to open data file: %s", *input_filename);
	      NLfit_error (message);
	    }

	  THD_load_datablock ((*dset_time)->dblk, NULL);

	  *nxyz =  (*dset_time)->dblk->diskptr->dimsizes[0]
	    * (*dset_time)->dblk->diskptr->dimsizes[1]
	    * (*dset_time)->dblk->diskptr->dimsizes[2] ;
	  *ts_length = DSET_NUM_TIMES(*dset_time);

          dsTR = DSET_TIMESTEP(*dset_time) ;
          if( DSET_TIMEUNITS(*dset_time) == UNITS_MSEC_TYPE ) dsTR *= 0.001 ;

	  nopt++;
	  continue;
	}

      /*----- 22 July 1998: the -inTR option -----*/

      if( strncmp(argv[nopt],"-inTR",5) == 0 ){
         inTR = 1 ;
         nopt++ ; continue ;
      }

      /*-----   -ignore num  -----*/
      if (strncmp(argv[nopt], "-ignore", 7) == 0)
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
      if (strncmp(argv[nopt], "-time", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -time ");
	  *tfilename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*tfilename == NULL)
	    NLfit_error ("-time:  Unable to allocate memory for tfilename");
	  strcpy (*tfilename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -signal slabel  -----*/
      if (strncmp(argv[nopt], "-signal", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -signal ");
	  *sname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*sname == NULL)
	    NLfit_error ("Unable to allocate memory for signal model name");
	  strcpy (*sname, argv[nopt]);
	  initialize_signal_model (model_array, *sname, 
				   smodel, p, *spname,
				   *min_sconstr, *max_sconstr);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -noise nlabel  -----*/
      if (strncmp(argv[nopt], "-noise", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -noise ");
	  *nname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*nname == NULL)
	    NLfit_error ("Unable to allocate memory for noise model name");
	  strcpy (*nname, argv[nopt]);
	  initialize_noise_model (model_array, *nname, 
				  nmodel, r, *npname,
				  *min_nconstr, *max_nconstr);
	  nopt++;
	  continue;
	}


      /*----- check that user has specified the signal and noise models -----*/
      if ((*smodel) == NULL)  NLfit_error ("Must specify signal model");
      if ((*nmodel) == NULL)  NLfit_error ("Must specify noise model");
      
      
      /*-----   -sconstr k min max  -----*/
      if (strncmp(argv[nopt], "-sconstr", 8) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  NLfit_error("need 3 arguments after -sconstr ");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *p))
	    NLfit_error ("illegal argument after -sconstr ");
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
      if (strncmp(argv[nopt], "-nconstr", 8) == 0)
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
      
      
      /*-----   -nabs  -----*/
      if (strncmp(argv[nopt], "-nabs", 6) == 0)
	{
	  *nabs = 1;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -nrand n  -----*/
      if (strncmp(argv[nopt], "-nrand", 6) == 0)
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
      if (strncmp(argv[nopt], "-nbest", 6) == 0)
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
      if (strncmp(argv[nopt], "-rmsmin", 7) == 0)
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
      if (strncmp(argv[nopt], "-fdisp", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -fdisp ");
	  sscanf (argv[nopt], "%f", &fval); 
	  *fdisp = fval;
	  nopt++;
	  continue;
	}
      

       /*-----   -freg filename   -----*/
      if (strncmp(argv[nopt], "-freg", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -freg ");
	  *freg_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*freg_filename == NULL)
	    NLfit_error ("Unable to allocate memory for freg_filename");
	  strcpy (*freg_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

       /*-----   -fsmax filename   -----*/
      if (strncmp(argv[nopt], "-fsmax", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -fsmax ");
	  *fsmax_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*fsmax_filename == NULL)
	    NLfit_error ("Unable to allocate memory for fsmax_filename");
	  strcpy (*fsmax_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

       /*-----   -ftmax filename   -----*/
      if (strncmp(argv[nopt], "-ftmax", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -ftmax ");
	  *ftmax_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*ftmax_filename == NULL)
	    NLfit_error ("Unable to allocate memory for ftmax_filename");
	  strcpy (*ftmax_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -fpmax filename   -----*/
      if (strncmp(argv[nopt], "-fpmax", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -fpmax ");
	  *fpmax_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*fpmax_filename == NULL)
	    NLfit_error ("Unable to allocate memory for fpmax_filename");
	  strcpy (*fpmax_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -farea filename   -----*/
      if (strncmp(argv[nopt], "-farea", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -farea ");
	  *farea_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*farea_filename == NULL)
	    NLfit_error ("Unable to allocate memory for farea_filename");
	  strcpy (*farea_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -fparea filename   -----*/
      if (strncmp(argv[nopt], "-fparea", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -fparea ");
	  *fparea_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*fparea_filename == NULL)
	    NLfit_error ("Unable to allocate memory for fparea_filename");
	  strcpy (*fparea_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -fscoef k filename   -----*/
      if (strncmp(argv[nopt], "-fscoef", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -fscoef ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *p))
	    NLfit_error ("illegal argument after -fscoef ");
	  index = ival;
	  nopt++;

	  (*fscoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if ((*fscoef_filename)[index] == NULL)
	    NLfit_error ("Unable to allocate memory for fscoef_filename");
	  strcpy ((*fscoef_filename)[index], argv[nopt]);

	  nopt++;
	  continue;
	}
      

      /*-----   -fncoef k filename   -----*/
      if (strncmp(argv[nopt], "-fncoef", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -fncoef ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *r))
	    NLfit_error ("illegal argument after -fncoef ");
	  index = ival;
	  nopt++;

	  (*fncoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if ((*fncoef_filename)[index] == NULL)
	    NLfit_error ("Unable to allocate memory for fncoef_filename");
	  strcpy ((*fncoef_filename)[index], argv[nopt]);

	  nopt++;
	  continue;
	}
      

      /*-----   -tscoef k filename   -----*/
      if (strncmp(argv[nopt], "-tscoef", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -tscoef ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *p))
	    NLfit_error ("illegal argument after -tscoef ");
	  index = ival;
	  nopt++;

	  (*tscoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if ((*tscoef_filename)[index] == NULL)
	    NLfit_error ("Unable to allocate memory for tscoef_filename");
	  strcpy ((*tscoef_filename)[index], argv[nopt]);

	  nopt++;
	  continue;
	}
      

      /*-----   -tncoef k filename   -----*/
      if (strncmp(argv[nopt], "-tncoef", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -tncoef ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *r))
	    NLfit_error ("illegal argument after -tncoef ");
	  index = ival;
	  nopt++;

	  (*tncoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if ((*tncoef_filename)[index] == NULL)
	    NLfit_error ("Unable to allocate memory for tncoef_filename");
	  strcpy ((*tncoef_filename)[index], argv[nopt]);

	  nopt++;
	  continue;
	}
      
      
      /*----- -bucket n prefixname -----*/
      if (strncmp(argv[nopt], "-bucket", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -bucket ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival > MAX_BRICKS))
	    NLfit_error ("illegal argument after -bucket ");
	  nopt++;

	  option_data->bucket_filename = 
	    malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (option_data->bucket_filename == NULL)
	    NLfit_error ("Unable to allocate memory for bucket_filename");
	  strcpy (option_data->bucket_filename, argv[nopt]);
	  
	  /*----- set number of sub-bricks in the bucket -----*/
	  if (ival == 0)
	    nbricks = (*p) + (*r) + 6;
	  else
	    nbricks = ival;
	  option_data->numbricks = nbricks;
	  
	  /*----- allocate memory and initialize bucket dataset options -----*/
	  option_data->brick_type = malloc (sizeof(int) * nbricks);
	  option_data->brick_coef = malloc (sizeof(int) * nbricks);
	  option_data->brick_label = malloc (sizeof(char *) * nbricks);
	  for (ibrick = 0;  ibrick < nbricks;  ibrick++)
	    {
	      option_data->brick_type[ibrick] = -1;
	      option_data->brick_coef[ibrick] = -1;
	      option_data->brick_label[ibrick] = 
		malloc (sizeof(char) * MAX_NAME_LENGTH);
	    }
	  

	  if (ival == 0)   
	    /*----- throw  (almost) everything into the bucket -----*/
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
	      option_data->brick_type[ibrick] = FUNC_FT_TYPE;
	      option_data->brick_coef[ibrick] = ibrick;
	      strcpy (option_data->brick_label[ibrick], "F-stat Regression");
	      
	    }

	  nopt++;
	  continue;
	}


      /*----- -brick m type k label -----*/
      if (strncmp(argv[nopt], "-brick", 6) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  
	    NLfit_error ("need more arguments after -brick ");
	  sscanf (argv[nopt], "%d", &ibrick);
	  if ((ibrick < 0) || (ibrick >= option_data->numbricks))
	    NLfit_error ("illegal argument after -brick ");
	  nopt++;

	  if (strncmp(argv[nopt], "scoef", 4) == 0)
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

	  else if (strncmp(argv[nopt], "ncoef", 4) == 0)
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

	  else if (strncmp(argv[nopt], "tmax", 4) == 0)
	    {
	      option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
	      option_data->brick_coef[ibrick] = (*r) + (*p);
	      nopt++;
	      if (nopt >= argc)  
		NLfit_error ("need more arguments after -brick ");
	      strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    }

	  else if (strncmp(argv[nopt], "smax", 4) == 0)
	    {
	      option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
	      option_data->brick_coef[ibrick] = (*r) + (*p) + 1;
	      nopt++;
	      if (nopt >= argc)  
		NLfit_error ("need more arguments after -brick ");
	      strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    }

	  else if (strncmp(argv[nopt], "psmax", 4) == 0)
	    {
	      option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
	      option_data->brick_coef[ibrick] = (*r) + (*p) + 2;
	      nopt++;
	      if (nopt >= argc)  
		NLfit_error ("need more arguments after -brick ");
	      strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    }

	  else if (strncmp(argv[nopt], "area", 4) == 0)
	    {
	      option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
	      option_data->brick_coef[ibrick] = (*r) + (*p) + 3;
	      nopt++;
	      if (nopt >= argc)  
		NLfit_error ("need more arguments after -brick ");
	      strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    }

	  else if (strncmp(argv[nopt], "parea", 4) == 0)
	    {
	      option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
	      option_data->brick_coef[ibrick] = (*r) + (*p) + 4;
	      nopt++;
	      if (nopt >= argc)  
		NLfit_error ("need more arguments after -brick ");
	      strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    }

	  else if (strncmp(argv[nopt], "fstat", 4) == 0)
	    {
	      option_data->brick_type[ibrick] = FUNC_FT_TYPE;
	      option_data->brick_coef[ibrick] = (*r) + (*p) + 5;
	      nopt++;
	      if (nopt >= argc)  
		NLfit_error ("need more arguments after -brick ");
	      strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    }

	  else if (strncmp(argv[nopt], "tscoef", 4) == 0)
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
	    }

	  else if (strncmp(argv[nopt], "tncoef", 4) == 0)
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
	    }

	  else  NLfit_error ("unable to interpret options after -brick ");
	  
	  nopt++;
	  continue;
	}
     

       /*-----   -sfit filename   -----*/
      if (strncmp(argv[nopt], "-sfit", 5) == 0)
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
      if (strncmp(argv[nopt], "-snfit", 6) == 0)
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
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int  nrand,             /* number of random vectors to generate */
  int  nbest,             /* number of random vectors to keep */

  char * freg_filename,         /* file name for regression f-statistics */
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
  check_output_files (freg_filename, fsmax_filename, ftmax_filename,
		      fpmax_filename, farea_filename, fparea_filename,
		      fncoef_filename, fscoef_filename,
		      tncoef_filename, tscoef_filename, bucket_filename, 
		      sfit_filename, snfit_filename, dset_time);

}


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

  char ** input_filename,     /* file name of input 3d+time dataset */
  char ** tfilename,          /* file name for time point series */  
  char ** freg_filename,      /* file name for regression f-statistics */
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

  float ** rmsreg_vol,     /* root-mean-square for the full regression model */
  float ** freg_vol,       /* f-statistic for the full regression model */
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
  

  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- get command line inputs -----*/
  get_options(argc, argv, ignore, nname, sname, nmodel, smodel, 
	      r, p, npname, spname, 
	      min_nconstr, max_nconstr, min_sconstr, max_sconstr, nabs, 
	      nrand, nbest, rms_min, fdisp, input_filename, tfilename, 
	      freg_filename, fsmax_filename, ftmax_filename, 
	      fpmax_filename, farea_filename, fparea_filename, 
	      fncoef_filename, fscoef_filename,
	      tncoef_filename, tscoef_filename, sfit_filename, snfit_filename,
	      dset_time, nxyz, ts_length, option_data);

 
  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*r, *p, *min_nconstr, *max_nconstr, 
			  *min_sconstr, *max_sconstr, *nrand, *nbest, 
			  *freg_filename, *fsmax_filename, *ftmax_filename,
			  *fpmax_filename, *farea_filename, *fparea_filename,
			  *fncoef_filename, *fscoef_filename, 
			  *tncoef_filename, *tscoef_filename, 
			  *sfit_filename, *snfit_filename, 
			  option_data->bucket_filename, *dset_time);


  /*----- allocate space for input time series -----*/
  *ts_length = *ts_length - *ignore;
  *ts_array = (float *) malloc (sizeof(float) * (*ts_length));
  if (*ts_array == NULL)
    NLfit_error ("Unable to allocate memory for ts_array");

  
  /*----- allocate space for independent variable matrix -----*/
  *x_array = (float **) malloc (sizeof(float *) * (*ts_length));
  if (*x_array == NULL)
    NLfit_error ("Unable to allocate memory for x_array");
  for (it = 0;  it < (*ts_length);  it++)
    {
      (*x_array)[it] = (float *) malloc (sizeof(float) * 3);
      if ((*x_array)[it] == NULL)
	NLfit_error ("Unable to allocate memory for x_array[it]");
    }

    
  /*----- initialize independent variable matrix -----*/
  if (*tfilename == NULL)
    {
     if( inTR && dsTR > 0.0 ){   /* 22 July 1998 */
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
        im = mri_read_ascii (*tfilename); 
	if (im == NULL)
	  NLfit_error ("Unable to read time reference file \n");
        flim = mri_transpose (im);  mri_free(im);
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
  
  /*----- allocate memory space for parameters -----*/
  dimension = (*r) + (*p);
  *par_rdcd = (float *) malloc (sizeof(float) * dimension);
  if (*par_rdcd == NULL)
    NLfit_error ("Unable to allocate memory for par_rdcd");
  *par_full = (float *) malloc (sizeof(float) * dimension);
  if (*par_full == NULL)
    NLfit_error ("Unable to allocate memory for par_full");
  *tpar_full = (float *) malloc (sizeof(float) * dimension);
  if (*tpar_full == NULL)
    NLfit_error ("Unable to allocate memory for tpar_full");


  /*----- allocate memory space for volume data -----*/
  *freg_vol = (float *) malloc (sizeof(float) * (*nxyz));
  if (*freg_vol == NULL)
    NLfit_error ("Unable to allocate memory for freg_vol");

  if (*freg_filename != NULL)
    {
      *rmsreg_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*rmsreg_vol == NULL)
	NLfit_error ("Unable to allocate memory for rmsreg_vol");
    }

  if ((*fsmax_filename != NULL) || (option_data->bucket_filename != NULL))
    {
      *smax_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*smax_vol == NULL)
	NLfit_error ("Unable to allocate memory for smax_vol");
    }

  if ((*ftmax_filename != NULL) || (option_data->bucket_filename != NULL))
    {
      *tmax_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*tmax_vol == NULL)
	NLfit_error ("Unable to allocate memory for tmax_vol");
    }

  
  if ((*fpmax_filename != NULL) || (option_data->bucket_filename != NULL))
    {
      *pmax_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*pmax_vol == NULL)
	NLfit_error ("Unable to allocate memory for pmax_vol");
    }

  if ((*farea_filename != NULL) || (option_data->bucket_filename != NULL))
    {
      *area_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*area_vol == NULL)
	NLfit_error ("Unable to allocate memory for area_vol");
    }

  if ((*fparea_filename != NULL) || (option_data->bucket_filename != NULL))
    {
      *parea_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*parea_vol == NULL)
	NLfit_error ("Unable to allocate memory for parea_vol");
    }

  
  *ncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  if (*ncoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for ncoef_vol");
  *tncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  if (*tncoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for tncoef_vol");

  for (ip = 0;  ip < (*r);  ip++)
    {
      if (((*fncoef_filename)[ip] != NULL) || ((*tncoef_filename)[ip] != NULL)
	  || (option_data->bucket_filename != NULL))
	{
	  (*ncoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));
	  if ((*ncoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for ncoef_vol[ip]");
	}
      else
	(*ncoef_vol)[ip] = NULL;

      if (((*tncoef_filename)[ip] != NULL) 
	  || (option_data->bucket_filename != NULL))
	{
	  (*tncoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));      
	  if ((*tncoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for tncoef_vol[ip]");
	}
      else
	(*tncoef_vol)[ip] = NULL;
    }
  

  *scoef_vol = (float **) malloc (sizeof(float *) * (*p));
  if (*scoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for scoef_vol");
  *tscoef_vol = (float **) malloc (sizeof(float *) * (*p));
  if (*tscoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for tscoef_vol");

  for (ip = 0;  ip < (*p);  ip++)
    {
      if (((*fscoef_filename)[ip] != NULL) || ((*tscoef_filename)[ip] != NULL)
	  || (option_data->bucket_filename != NULL))
	{
	  (*scoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));
	  if ((*scoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for scoef_vol[ip]");
	}
      else
	(*scoef_vol)[ip] = NULL;

      if (((*tscoef_filename)[ip] != NULL)
	  || (option_data->bucket_filename != NULL))
	{
	  (*tscoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));      
	  if ((*tscoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for tscoef_vol[ip]");
	}
      else
	(*tscoef_vol)[ip] = NULL;
    }


  /*----- Allocate memory space for 3d+time fitted signal model -----*/
  if (*sfit_filename != NULL)
    {
      *sfit_vol = (float **) malloc (sizeof(float *) * (*ts_length));
      MTEST(*sfit_vol);
 
      for (it = 0;  it < *ts_length;  it++)
	{
	  (*sfit_vol)[it] = (float *) malloc (sizeof(float) * (*nxyz));
	  MTEST ((*sfit_vol)[it]);
	}
    }

  /*----- Allocate memory space for 3d+time fitted signal+noise model -----*/
  if (*snfit_filename != NULL)
    {
      *snfit_vol = (float **) malloc (sizeof(float *) * (*ts_length));
      MTEST(*snfit_vol);
 
      for (it = 0;  it < *ts_length;  it++)
	{
	  (*snfit_vol)[it] = (float *) malloc (sizeof(float) * (*nxyz));
	  MTEST ((*snfit_vol)[it]);
	}
    }

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


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  NLfit_error ("Unable to extract data from 3d+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
  ar = MRI_FLOAT_PTR (im);
  for (it = 0;  it < ts_length;  it++)
    {
      ts_array[it] = ar[it+ignore];
    }


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;
   
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
  float rmsreg,            /* root-mean-square for the full regression model */
  float freg,              /* f-statistic for the full regression model */
  float smax,              /* signed maximum of signal */
  float tmax,              /* epoch of signed maximum of signal */
  float pmax,              /* percentage change due to signal */
  float area,              /* area between signal and baseline */
  float parea,             /* percentage area between signal and baseline */
  
  float * rmsreg_vol,      /* root-mean-square for the full regression model */
  float * freg_vol,        /* f-statistic for the full regression model */
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
  if (freg_vol != NULL)  freg_vol[iv] = freg;
  if (rmsreg_vol != NULL)  rmsreg_vol[iv] = rmsreg;

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
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
*/

float EDIT_coerce_autoscale_new( int nxyz ,
				 int itype,void *ivol , int otype,void *ovol )
{
  float fac=0.0 , top ;
  
  if( MRI_IS_INT_TYPE(otype) ){
    top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
    if (top == 0.0)  fac = 0.0;
    else  fac = MRI_TYPE_maxval[otype]/top;
  }
  
  EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
  return ( fac );
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
  int output_datum;                   /* data type for output data */
  short * tsp;                        /* 2nd sub-brick data pointer */
  void  * vdif;                       /* 1st sub-brick data pointer */
  int func_type;                      /* afni data set type */
  float top, func_scale_short;        /* parameters for scaling data */
  char label[80];                     /* label for output file history */ 
  
    
  /*----- read input dataset -----*/
  dset = THD_open_one_dataset (input_filename) ;
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
  output_datum = DSET_BRICK_TYPE(dset,iv) ;
  if( output_datum == MRI_byte ) output_datum = MRI_short ;
  
  
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
  printf("--- Writing combined dataset into %s\n",
	 new_dset->dblk->diskptr->header_name) ;
  
  fbuf[0] = numdof;   
  fbuf[1] = dendof;
  for( ii=2 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
  
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

  float * rmsreg_vol,     /* root-mean-square for the full regression model */
  float * freg_vol,       /* f-statistic for the full regression model */
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
  float factor;             /* factor is new scale factor for sub-brick #ib */
  int brick_type;           /* indicates statistical type of sub-brick */
  int brick_coef;           /* regression coefficient index for sub-brick */
  char * brick_label;       /* character string label for sub-brick */
  int ierror;               /* number of errors in editing data */
  float * volume;           /* volume of floating point data */
  int dimension;            /* dimension of full model = p + q */
  char label[80];           /* label for output file history */ 

    
  /*----- initialize local variables -----*/
  nbricks = option_data->numbricks;
  output_prefix = option_data->bucket_filename;
  output_session = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
  strcpy (output_session, "./");
  dimension = p + q;
  

  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * nbricks);
  MTEST (bar);

 
  /*----- read first dataset -----*/
  old_dset = THD_open_one_dataset (input_filename);
  

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

      /*----- allocate memory for output sub-brick -----*/
      bar[ib]  = (short *) malloc (sizeof(short) * nxyz);
      MTEST (bar[ib]);
      factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
					  MRI_short, bar[ib]);

      if (factor < EPSILON)  factor = 0.0;
      else factor = 1.0 / factor;

      /*----- edit the sub-brick -----*/
      EDIT_BRICK_LABEL (new_dset, ib, brick_label);
      EDIT_BRICK_FACTOR (new_dset, ib, factor);

      
      /*----- attach bar[ib] to be sub-brick #ib -----*/
      EDIT_substitute_brick (new_dset, ib, MRI_short, bar[ib]);

    }


  /*----- write bucket data set -----*/
  printf("Writing `bucket' dataset ");
  printf("into %s\n", DSET_HEADNAME(new_dset));
  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

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
  float * fbuf;             /* float buffer */
  float * volume;           /* pointer to volume of data */
  char label[80];           /* label for output file history */ 
  

  /*----- Initialize local variables -----*/
  dset = THD_open_one_dataset (input_filename);
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;

 
  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * ts_length);   MTEST (bar);
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
			    ADN_nvals,       ts_length,
			    ADN_ntt,         ts_length,
			    ADN_stat_aux,    fbuf,
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

  
  /*----- attach bricks to new data set -----*/
  for (ib = 0;  ib < ts_length;  ib++)
    {

      /*----- Set pointer to appropriate volume -----*/
      volume = vol_array[ib];
      
      /*----- Allocate memory for output sub-brick -----*/
      bar[ib]  = (short *) malloc (sizeof(short) * nxyz);
      MTEST (bar[ib]);

      /*----- Convert data type to short for this sub-brick -----*/
      factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
					  MRI_short, bar[ib]);
      if (factor < EPSILON)  factor = 0.0;
      else factor = 1.0 / factor;
      fbuf[ib] = factor;

      /*----- attach bar[ib] to be sub-brick #ib -----*/
      mri_fix_data_pointer (bar[ib], DSET_BRICK(new_dset,ib)); 
    }


  /*----- write afni data set -----*/
  printf ("--- Writing 3d+time dataset into %s\n",
	  new_dset->dblk->diskptr->header_name);

  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset (NULL, NULL, new_dset, True);
  

  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (new_dset, False);   new_dset = NULL ;
  free (fbuf);   fbuf = NULL;

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

  float * rmsreg_vol,     /* root-mean-square for the full regression model */
  float * freg_vol,       /* f-statistic for the full regression model */
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
    write_bucket_data (r, p, numdof, dendof, nxyz, ts_length, 
		  rmsreg_vol, freg_vol, smax_vol, tmax_vol, pmax_vol, area_vol,
		  parea_vol, ncoef_vol, scoef_vol, tncoef_vol, tscoef_vol,
		  input_filename, option_data);


  /*----- write out the f-statistics for the regression -----*/
  if (freg_filename != NULL)
    write_afni_data (input_filename, nxyz, freg_filename, 
		     rmsreg_vol, freg_vol, numdof, dendof); 


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

  float ** rmsreg_vol,        /* rms for the full regression model */
  float ** freg_vol,          /* f-statistic for the full regression model */
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
    { free (*input_filename);  *input_filename = NULL; }
  if (*freg_filename != NULL)
    { free (*freg_filename);   *freg_filename = NULL; }
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
  if (*rmsreg_vol != NULL)  { free (*rmsreg_vol);  *rmsreg_vol = NULL; }
  if (*freg_vol != NULL)    { free (*freg_vol);    *freg_vol = NULL; } 
  if (*smax_vol != NULL)    { free (*smax_vol);    *smax_vol = NULL; } 
  if (*tmax_vol != NULL)    { free (*tmax_vol);    *tmax_vol = NULL; } 
  if (*pmax_vol != NULL)    { free (*pmax_vol);    *pmax_vol = NULL; } 
  if (*area_vol != NULL)    { free (*area_vol);    *area_vol = NULL; } 
  if (*parea_vol != NULL)   { free (*parea_vol);   *parea_vol = NULL; } 

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
  float rmsreg;                /* rms for the full regression model */
  float smax;                  /* signed maximum of signal */
  float tmax;                  /* epoch of signed maximum of signal */
  float pmax;                  /* percentage change due to signal */
  float area;                  /* area between signal and baseline */
  float parea;                 /* percent area between signal and baseline */
  float * min_sconstr = NULL;  /* min parameter constraints for signal model */
  float * max_sconstr = NULL;  /* max parameter constraints for signal model */

  /*----- declare output volume data -----*/
  float * rmsreg_vol = NULL;    /* rms for the full regression model */
  float * freg_vol = NULL;      /* f-statistic for the full regression model */
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

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

   
  /*----- program initialization -----*/
  initialize_program (argc, argv, &ignore, 
		      &nname, &sname, &nmodel, &smodel, 
		      &r, &p, &npname, &spname,
		      &min_nconstr, &max_nconstr, &min_sconstr, &max_sconstr,
		      &nabs, &nrand, &nbest, &rms_min, &fdisp, 
		      &input_filename, &tfilename, &freg_filename, 
		      &fsmax_filename, &ftmax_filename, &fpmax_filename,
		      &farea_filename, &fparea_filename, &fncoef_filename,
		      &fscoef_filename, &tncoef_filename, &tscoef_filename,
		      &sfit_filename, &snfit_filename, 
		      &dset_time, &nxyz, &ts_length, &x_array, &ts_array, 
		      &par_rdcd, &par_full, &tpar_full, &rmsreg_vol, &freg_vol,
		      &smax_vol, &tmax_vol, &pmax_vol, &area_vol, &parea_vol, 
		      &ncoef_vol, &scoef_vol, &tncoef_vol, &tscoef_vol,
		      &sfit_vol, &snfit_vol, &option_data);


  /*----- loop over voxels in the data set -----*/
  for (iv = 0;  iv < nxyz;  iv++)
    {

      /*----- read the time series for voxel iv -----*/
      read_ts_array (dset_time, iv, ts_length, ignore, ts_array);
 

      /*----- calculate the reduced (noise) model -----*/
      calc_reduced_model (ts_length, r, x_array, ts_array, 
			  par_rdcd, &sse_rdcd);


      /*----- calculate the full (signal+noise) model -----*/
      calc_full_model (nmodel, smodel, r, p,  
		       min_nconstr, max_nconstr, min_sconstr, max_sconstr,
		       ts_length, x_array, ts_array, par_rdcd, sse_rdcd, nabs,
		       nrand, nbest, rms_min, par_full, &sse_full, &novar);


      /*----- calculate statistics for the full model -----*/
      analyze_results (nmodel, smodel, r, p, novar,
		       min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		       ts_length, x_array,
		       par_rdcd, sse_rdcd, par_full, sse_full,
		       &rmsreg, &freg, &smax, &tmax, &pmax, &area, &parea,
		       tpar_full);


      /*----- report results for this voxel -----*/
      if (freg >= fdisp)
	{
	  report_results (nname, sname, r, p, npname, spname, ts_length,
		      par_rdcd, sse_rdcd, par_full, sse_full, tpar_full,
		      rmsreg, freg, smax, tmax, pmax, area, parea, &label);
	  printf ("\n\nVoxel #%d\n", iv);
	  printf ("%s \n", label);
	}


      /*----- save results for this voxel into volume data -----*/
      save_results (iv, nmodel, smodel, r, p, novar, ts_length, x_array, 
		    par_full, tpar_full, rmsreg, freg, smax, tmax, pmax, area,
		    parea, rmsreg_vol, freg_vol, smax_vol, tmax_vol, pmax_vol, 
		    area_vol, parea_vol,ncoef_vol, scoef_vol, 
		    tncoef_vol, tscoef_vol, sfit_vol, snfit_vol);
    }


  /*----- delete input dataset -----*/ 
  THD_delete_3dim_dataset( dset_time , False ) ;  dset_time = NULL ;


  /*----- write requested output files -----*/
  output_results (r, p, min_nconstr, max_nconstr, min_sconstr, max_sconstr,
		  nxyz, ts_length, rmsreg_vol, freg_vol, 
		  smax_vol, tmax_vol, pmax_vol, area_vol, parea_vol, 
		  ncoef_vol, scoef_vol, tncoef_vol, tscoef_vol, 
		  sfit_vol, snfit_vol,
		  input_filename, freg_filename, 
		  fsmax_filename, ftmax_filename, 
		  fpmax_filename, farea_filename, fparea_filename, 
		  fncoef_filename, fscoef_filename, 
		  tncoef_filename, tscoef_filename, 
		  sfit_filename, snfit_filename, &option_data);
		 

  /*----- end of program -----*/
  terminate_program (r, p, ts_length, &x_array, &ts_array, 
		     &nname, &npname, &par_rdcd, &min_nconstr, &max_nconstr, 
		     &sname, &spname, &par_full, &tpar_full, 
		     &min_sconstr, &max_sconstr, &rmsreg_vol, &freg_vol, 
		     &smax_vol, &tmax_vol, &pmax_vol, &area_vol, &parea_vol,
		     &ncoef_vol, &scoef_vol, &tncoef_vol, &tscoef_vol,
		     &sfit_vol, &snfit_vol, &input_filename, &freg_filename, 
		     &fsmax_filename, &ftmax_filename, 
		     &fpmax_filename, &farea_filename, &fparea_filename,
		     &fncoef_filename, &fscoef_filename, 
		     &tncoef_filename, &tscoef_filename, 
		     &sfit_filename, &snfit_filename);

  return;
}


















