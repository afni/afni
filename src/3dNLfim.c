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

*/


/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
*/


/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dNLfim"                /* name of this program */
#define LAST_MOD_DATE "29 August 1997"        /* date of last program mod */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "3ddata.h"
#include "editvol.h"

#include "matrix.h"
#include "simplex.h"
#include "NLfit.h"

#include "matrix.c"
#include "simplex.c"
#include "NLfit.c"

#include "mrilib.h"

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
     "-ignore num        num   = skip this number of initial images in the  \n"
     "                     time series for regresion analysis; default = 3  \n"
     "[-time fname]      fname = ASCII file containing each time point      \n"
     "                     in the time series. Defaults to even spacing.    \n"
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
     "[-freg fname]      perform f-test for significance of the regression; \n"
     "                     output 'fift' is written to prefix filename fname\n"
     "[-fsmax fname]     estimate signed maximum of signal; store along     \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
     "[-ftmax fname]     estimate time of signed maximum; store along       \n"
     "                     with f-test for regression; output 'fift' is     \n"
     "                     written to prefix filename fname                 \n"
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
    );
  
  exit(0);
}


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
  char *** tscoef_filename    /* file name for signal model t-statistics */
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
  *r = 0;
  *p = 0;


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
  char *** fncoef_filename,   /* file name for noise model parameters */
  char *** fscoef_filename,   /* file name for signal model parameters */
  char *** tncoef_filename,   /* file name for noise model t-statistics */
  char *** tscoef_filename,   /* file name for signal model t-statistics */

  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  int * nxyz,                       /* number of voxels in image */
  int * ts_length                   /* length of time series data */  
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[MAX_NAME_LENGTH];    /* error message */
  int ok;                           /* boolean for specified model exists */

  NLFIT_MODEL_array * model_array = NULL;   /* array of SO models */
  int im;                                   /* model index */


  
  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  
  
  /*----- initialize the model array -----*/
  model_array = NLFIT_get_many_MODELs ();
  if ((model_array == NULL) || (model_array->num == 0))
    NLfit_error ("Unable to locate any models");

  /*----- initialize the input options -----*/
  initialize_options (ignore, nmodel, smodel, r, p, npname, spname, 
		      min_nconstr, max_nconstr, min_sconstr, max_sconstr, nabs,
		      nrand, nbest, rms_min, fdisp, input_filename, tfilename, 
		      freg_filename, fncoef_filename, fscoef_filename,
		      tncoef_filename, tscoef_filename); 

  
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

	  nopt++;
	  continue;
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
      fprintf(stderr,
	      "*** Output dataset file %s already exists"
	      "--cannot continue!\a\n",
	      new_dset->dblk->diskptr->header_name ) ;
      exit(1) ;
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
  char * freg_filename,           /* file name for regression f-statistics */
  char * fsmax_filename,          /* file name for signal signed maximum */
  char * ftmax_filename,          /* file name for epoch of signed maximum */
  char ** fncoef_filename,        /* file name for noise model parameters */
  char ** fscoef_filename,        /* file name for signal model parameters */
  char ** tncoef_filename,        /* file name for noise model t-statistics */
  char ** tscoef_filename,        /* file name for signal model t-statistics */
  THD_3dim_dataset * dset_time    /* input 3d+time data set */
)

{
  int ip;       /* parameter index */
  
  if (freg_filename != NULL)   
    check_one_output_file (dset_time, freg_filename);
  
  if (fsmax_filename != NULL)   
    check_one_output_file (dset_time, fsmax_filename);
  
  if (ftmax_filename != NULL)   
    check_one_output_file (dset_time, ftmax_filename);
  
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

  char * freg_filename,           /* file name for regression f-statistics */
  char * fsmax_filename,          /* file name for signal signed maximum */
  char * ftmax_filename,          /* file name for epoch of signed maximum */
  char ** fncoef_filename,        /* file name for noise model parameters */
  char ** fscoef_filename,        /* file name for signal model parameters */
  char ** tncoef_filename,        /* file name for noise model t-statistics */
  char ** tscoef_filename,        /* file name for signal model t-statistics */
  THD_3dim_dataset * dset_time    /* input 3d+time data set */
)

{
  int ip;                         /* parameter index */


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
		      fncoef_filename, fscoef_filename,
		      tncoef_filename, tscoef_filename, dset_time);

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
  char *** fncoef_filename,   /* file name for noise model parameters */
  char *** fscoef_filename,   /* file name for signal model parameters */
  char *** tncoef_filename,   /* file name for noise model t-statistics */
  char *** tscoef_filename,   /* file name for signal model t-statistics */

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
  float *** ncoef_vol,     /* volume of noise model parameters */
  float *** scoef_vol,     /* volume of signal model parameters */
  float *** tncoef_vol,    /* volume of noise model t-statistics */
  float *** tscoef_vol     /* volume of signal model t-statistics */
)
     
{
  const float DELT = 1.0;
  int dimension;           /* dimension of full model */
  int ip;                  /* parameter index */
  int it;                  /* time index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures 
                              -- used to read 1D ASCII */
  int nt;                  /* number of points in 1D x data file */
  float * tar;
  

  /*----- get command line inputs -----*/
  get_options(argc, argv, ignore, nname, sname, nmodel, smodel, 
	      r, p, npname, spname, 
	      min_nconstr, max_nconstr, min_sconstr, max_sconstr, nabs, 
	      nrand, nbest, rms_min, fdisp, input_filename, tfilename, 
	      freg_filename, fsmax_filename, ftmax_filename, 
	      fncoef_filename, fscoef_filename,
	      tncoef_filename, tscoef_filename, dset_time, nxyz, ts_length);

 
  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*r, *p, *min_nconstr, *max_nconstr, 
			  *min_sconstr, *max_sconstr, *nrand, *nbest, 
			  *freg_filename, *fsmax_filename, *ftmax_filename,
			  *fncoef_filename, *fscoef_filename, 
			  *tncoef_filename, *tscoef_filename, *dset_time);


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

  if (*fsmax_filename != NULL)
    {
      *smax_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*smax_vol == NULL)
	NLfit_error ("Unable to allocate memory for smax_vol");
    }

  if (*ftmax_filename != NULL)
    {
      *tmax_vol = (float *) malloc (sizeof(float) * (*nxyz));
      if (*tmax_vol == NULL)
	NLfit_error ("Unable to allocate memory for tmax_vol");
    }

  
  *ncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  if (*ncoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for ncoef_vol");
  *tncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  if (*tncoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for tncoef_vol");

  for (ip = 0;  ip < (*r);  ip++)
    {
      if (((*fncoef_filename)[ip] != NULL) || ((*tncoef_filename)[ip] != NULL))
	{
	  (*ncoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));
	  if ((*ncoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for ncoef_vol[ip]");
	}
      else
	(*ncoef_vol)[ip] = NULL;

      if ((*tncoef_filename)[ip] != NULL)
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
      if (((*fscoef_filename)[ip] != NULL) || ((*tscoef_filename)[ip] != NULL))
	{
	  (*scoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));
	  if ((*scoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for scoef_vol[ip]");
	}
      else
	(*scoef_vol)[ip] = NULL;

      if ((*tscoef_filename)[ip] != NULL)
	{
	  (*tscoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));      
	  if ((*tscoef_vol)[ip] == NULL)
	    NLfit_error ("Unable to allocate memory for tscoef_vol[ip]");
	}
      else
	(*tscoef_vol)[ip] = NULL;
    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Read one AFNI 3d+time data set.
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
  int it, dtyp;

  dtyp = DSET_BRICK_TYPE(dset_time,0) ;
  
   switch( dtyp )
     {
     case MRI_short:
       {
	 for (it = 0;  it < ts_length;  it++)
	   {
	     short * dar = (short *) DSET_ARRAY(dset_time,it+ignore) ;
	     ts_array[it] = (float) dar[iv] ;
	   }
       }
       break ;
       
     case MRI_float:
       {
	 for (it = 0;  it < ts_length;  it++)
	   {
	     float * dar = (float *) DSET_ARRAY(dset_time,it+ignore) ;
	     ts_array[it] = (float) dar[iv] ;
	   }
       }
       break ;
       
     case MRI_byte:
       {
	 for (it = 0;  it < ts_length;  it++)
	   {
	     byte * dar = (byte *) DSET_ARRAY(dset_time,it+ignore) ;
	     ts_array[it] = (float) dar[iv] ;
	   }
       }
       break ;
     }
   
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
  int r,                   /* number of parameters in the noise model */
  int p,                   /* number of parameters in the signal model */
  float * par_full,        /* estimated parameters for the full model */
  float * tpar_full,       /* t-statistic of the parameters in full model */
  float rmsreg,            /* root-mean-square for the full regression model */
  float freg,              /* f-statistic for the full regression model */
  float smax,              /* signed maximum of signal */
  float tmax,              /* epoch of signed maximum of signal */

  float * rmsreg_vol,      /* root-mean-square for the full regression model */
  float * freg_vol,        /* f-statistic for the full regression model */
  float * smax_vol,        /* signed maximum of signal volume data */
  float * tmax_vol,        /* epoch of signed maximum volume data */
  float ** ncoef_vol,      /* volume of noise model parameters */
  float ** scoef_vol,      /* volume of signal model parameters */
  float ** tncoef_vol,     /* volume of noise model t-statistics */
  float ** tscoef_vol      /* volume of signal model t-statistics */
)

{
  int ip;                  /* parameter index */


  /*----- save regression results into volume data -----*/ 
  if (freg_vol != NULL)  freg_vol[iv] = freg;
  if (rmsreg_vol != NULL)  rmsreg_vol[iv] = rmsreg;

  /*----- save signed maximum and epoch of signed maximum of signal -----*/
  if (smax_vol != NULL)  smax_vol[iv] = smax;
  if (tmax_vol != NULL)  tmax_vol[iv] = tmax;

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
  
    
  /*----- read input dataset -----*/
  dset = THD_open_one_dataset (input_filename) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    input_filename);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
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
  Write out the user requested output files.
*/

void output_results 
(
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int  nxyz,              /* number of voxels in image */
  int  ts_length,         /* length of time series data */  

  float * rmsreg_vol,     /* root-mean-square for the full regression model */
  float * freg_vol,       /* f-statistic for the full regression model */
  float * smax_vol,       /* signed maximum of signal volume data */
  float * tmax_vol,       /* epoch of signed maximum volume data */
  float ** ncoef_vol,     /* volume of noise model parameters */
  float ** scoef_vol,     /* volume of signal model parameters */
  float ** tncoef_vol,    /* volume of noise model t-statistics */
  float ** tscoef_vol,    /* volume of signal model t-statistics */

  char * input_filename,     /* file name of input 3d+time dataset */
  char * freg_filename,      /* file name for f-statistics */
  char * fsmax_filename,     /* file name for signal signed maximum */
  char * ftmax_filename,     /* file name for time of signed maximum */
  char ** fncoef_filename,   /* file name for noise model parameters */
  char ** fscoef_filename,   /* file name for signal model parameters */
  char ** tncoef_filename,   /* file name for noise model t-statistics */
  char ** tscoef_filename    /* file name for signal model t-statistics */
)

{
  int ip;                 /* parameter index */
  int dimension;          /* dimension of full model = r + p  */
  int numdof, dendof;     /* numerator and denominator degrees of freedom */


  dimension = r + p;


  /*----- write out the f-statistics for the regression -----*/
  if (freg_filename != NULL)
    {
      numdof = p;
      dendof = ts_length - dimension;
      write_afni_data (input_filename, nxyz, freg_filename, 
		       rmsreg_vol, freg_vol, numdof, dendof); 
    }


  /*----- write out the signed maximum of signal estimates -----*/
  if (fsmax_filename != NULL)
    {
      numdof = p;
      dendof = ts_length - dimension;
      write_afni_data (input_filename, nxyz, fsmax_filename, 
		       smax_vol, freg_vol, numdof, dendof); 
    }


  /*----- write out the epoch of signed maximum of signal estimates -----*/
  if (ftmax_filename != NULL)
    {
      numdof = p;
      dendof = ts_length - dimension;
      write_afni_data (input_filename, nxyz, ftmax_filename, 
		       tmax_vol, freg_vol, numdof, dendof); 
    }


  /*----- write noise model parameters -----*/
  for (ip = 0;  ip < r;  ip++)
    {
      if (tncoef_filename[ip] != NULL)
	{
	  numdof = ts_length - dimension;
	  dendof = 0;
	  write_afni_data (input_filename, nxyz, tncoef_filename[ip], 
			   ncoef_vol[ip], tncoef_vol[ip], numdof, dendof); 
	}
      if (fncoef_filename[ip] != NULL)
	{
	  numdof = p;
	  dendof = ts_length - dimension;
	  write_afni_data (input_filename, nxyz, fncoef_filename[ip], 
			   ncoef_vol[ip], freg_vol, numdof, dendof); 
	}
    }


  /*----- write signal model parameters -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      if (tscoef_filename[ip] != NULL)
	{
	  numdof = ts_length - dimension;
	  dendof = 0;
	  write_afni_data (input_filename, nxyz, tscoef_filename[ip], 
			   scoef_vol[ip], tscoef_vol[ip], numdof, dendof); 
	}
      if (fscoef_filename[ip] != NULL)
	{
	  numdof = p;
	  dendof = ts_length - dimension;
	  write_afni_data (input_filename, nxyz, fscoef_filename[ip], 
			   scoef_vol[ip], freg_vol, numdof, dendof); 
	}
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
  float *** ncoef_vol,        /* noise model parameters volume data */
  float *** scoef_vol,        /* signal model parameters volume data */
  float *** tncoef_vol,       /* noise model t-statistics volume data */
  float *** tscoef_vol,       /* signal model t-statistics volume data */

  char ** input_filename,        /* file name of input 3d+time dataset */
  char ** freg_filename,         /* file name for regression f-statistics */
  char ** fsmax_filename,        /* file name for signal signed maximum */
  char ** ftmax_filename,        /* file name for epoch of signed maximum */
  char *** fncoef_filename,      /* file name for noise model parameters */
  char *** fscoef_filename,      /* file name for signal model parameters */
  char *** tncoef_filename,      /* file name for noise model t-statistics */
  char *** tscoef_filename       /* file name for signal model t-statistics */
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

  for (ip = 0;  ip < r;  ip++)
    {
      if ((*ncoef_vol)[ip] != NULL)
	{ free ((*ncoef_vol)[ip]);  (*ncoef_vol)[ip] = NULL; }
      if ((*tncoef_vol)[ip] != NULL)      
	{ free ((*tncoef_vol)[ip]);  (*tncoef_vol)[ip] = NULL; }
    }
  if (*ncoef_vol != NULL)
	{ free (*ncoef_vol);   *ncoef_vol = NULL; }
  if (*tncoef_vol != NULL)      
	{ free (*tncoef_vol);  *tncoef_vol = NULL; }
  
  for (ip = 0;  ip < p;  ip++)
    {
      if ((*scoef_vol)[ip] != NULL)
	{ free ((*scoef_vol)[ip]);  (*scoef_vol)[ip] = NULL; }
      if ((*tscoef_vol)[ip] != NULL)      
	{ free ((*tscoef_vol)[ip]);  (*tscoef_vol)[ip] = NULL; }
    }
  if (*scoef_vol != NULL)
	{ free (*scoef_vol);   *scoef_vol = NULL; }
  if (*tscoef_vol != NULL)      
	{ free (*tscoef_vol);  *tscoef_vol = NULL; }
  
}


/*---------------------------------------------------------------------------*/

void main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  /*----- declare input option variables -----*/
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
  float * min_sconstr = NULL;  /* min parameter constraints for signal model */
  float * max_sconstr = NULL;  /* max parameter constraints for signal model */

  /*----- declare output volume data -----*/
  float * rmsreg_vol = NULL;    /* rms for the full regression model */
  float * freg_vol = NULL;      /* f-statistic for the full regression model */
  float * smax_vol = NULL;      /* signed max. of signal volume data */
  float * tmax_vol = NULL;      /* epoch of signed max. volume data */
  float ** ncoef_vol = NULL;    /* noise model parameters volume data */
  float ** scoef_vol = NULL;    /* signal model parameters volume data */
  float ** tncoef_vol = NULL;   /* noise model t-statistics volume data */
  float ** tscoef_vol = NULL;   /* signal model t-statistics volume data */

  /*----- declare file name variables -----*/
  char * input_filename = NULL;   /* file name of input 3d+time dataset */
  char * tfilename = NULL;        /* file name of time points */
  char * freg_filename = NULL;    /* file name for regression f-statistics */
  char * fsmax_filename = NULL;   /* file name for signal signed maximum */
  char * ftmax_filename = NULL;   /* file name for time of signed maximum */
  char ** fncoef_filename = NULL; /* file name for noise model parameters */
  char ** fscoef_filename = NULL; /* file name for signal model parameters */
  char ** tncoef_filename = NULL; /* file name for noise model t-statistics */
  char ** tscoef_filename = NULL; /* file name for signal model t-statistics */
  
  char * label;            /* report results for one voxel */

   
  /*----- program initialization -----*/
  printf ("\n\nProgram %s \n\n", PROGRAM_NAME);
  printf ("Last revision: %s\n", LAST_MOD_DATE);
  initialize_program (argc, argv, &ignore, 
		      &nname, &sname, &nmodel, &smodel, 
		      &r, &p, &npname, &spname,
		      &min_nconstr, &max_nconstr, &min_sconstr, &max_sconstr,
		      &nabs, &nrand, &nbest, &rms_min, &fdisp, 
		      &input_filename, &tfilename, &freg_filename, 
		      &fsmax_filename, &ftmax_filename, &fncoef_filename,
		      &fscoef_filename, &tncoef_filename, &tscoef_filename,
		      &dset_time, &nxyz, &ts_length, &x_array, &ts_array, 
		      &par_rdcd, &par_full, &tpar_full,
		      &rmsreg_vol, &freg_vol, &smax_vol, &tmax_vol, 
		      &ncoef_vol, &scoef_vol, &tncoef_vol, &tscoef_vol);


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
		       ts_length, x_array, ts_array, par_rdcd, sse_rdcd,
		       nabs, nrand, nbest, rms_min, par_full, &sse_full);


      /*----- calculate statistics for the full model -----*/
      analyze_results (nmodel, smodel, r, p,
		       min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		       ts_length, x_array,
		       par_rdcd, sse_rdcd, par_full, sse_full,
		       &rmsreg, &freg, &smax, &tmax, tpar_full);


      /*----- report results for this voxel -----*/
      if (freg >= fdisp)
	{
	  report_results (nname, sname, r, p, npname, spname, ts_length,
		      par_rdcd, sse_rdcd, par_full, sse_full, tpar_full,
		      rmsreg, freg, smax, tmax, &label);
	  printf ("\n\nVoxel #%d\n", iv);
	  printf ("%s \n", label);
	}


      /*----- save results for this voxel into volume data -----*/
      save_results (iv, r, p, par_full, tpar_full,
		    rmsreg, freg, smax, tmax, 
		    rmsreg_vol, freg_vol, smax_vol, tmax_vol,
		    ncoef_vol, scoef_vol, tncoef_vol, tscoef_vol);
    }


  /*----- delete input dataset -----*/ 
  THD_delete_3dim_dataset( dset_time , False ) ;  dset_time = NULL ;


  /*----- write requested output files -----*/
  output_results (r, p, nxyz, ts_length, 
		  rmsreg_vol, freg_vol, smax_vol, tmax_vol,
		  ncoef_vol, scoef_vol, tncoef_vol, tscoef_vol,
		  input_filename, freg_filename, fsmax_filename, 
		  ftmax_filename, fncoef_filename, fscoef_filename, 
		  tncoef_filename, tscoef_filename);
		 

  /*----- end of program -----*/
  terminate_program (r, p, ts_length, &x_array, &ts_array, 
		     &nname, &npname, &par_rdcd, &min_nconstr, &max_nconstr, 
		     &sname, &spname, &par_full, &tpar_full, 
		     &min_sconstr, &max_sconstr,
		     &rmsreg_vol, &freg_vol, &smax_vol, &tmax_vol, 
		     &ncoef_vol, &scoef_vol, &tncoef_vol, &tscoef_vol,
		     &input_filename, &freg_filename, &fsmax_filename,
		     &ftmax_filename, &fncoef_filename, &fscoef_filename, 
		     &tncoef_filename, &tscoef_filename);
}


















