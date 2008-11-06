/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This program generates an AFNI 3d+time data set.  The time series for
   each voxel is generated according to a user specified signal+noise model.

   File:     3dTSgen.c
   Author:   B. Douglas Ward
   Date:     17 June 1997
   
   Mod:      Function srand48 used for random number initialization.
   Date:     29 August 1997

   Mod:      Extensive changes required to implement the 'bucket' dataset.
   Date:     09 January 1998

   Mod:      Added the -inTR option.
             Removed duplicate readin of dset_time, and removed the
               input of dset_time's bricks (never used).
   Date:     22 July 1998 -- RWCox
	     
   Mod:      Correction to initialization of dataset parameters.
   Date:     12 November 1998

   Mod:      Added changes for incorporating History notes.
   Date:     09 September 1999

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dTSgen"                       /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "09 September 1999"         /* date of last program mod */


#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mrilib.h"

#include "matrix.h"
#include "simplex.h"
#include "NLfit.h"

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
  Routine to display 3dTSgen help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program generates an AFNI 3d+time data set.  The time series for \n"
     "each voxel is generated according to a user specified signal + noise  \n"
     "model.                                                              \n\n"
     "Usage:                                                                \n"
     "3dTSgen                                                               \n"
     "-input fname       fname = filename of prototype 3d + time data file  \n"
     "[-inTR]            set the TR of the created timeseries to be the TR  \n"
     "                     of the prototype dataset                         \n"
     "                     [The default is to compute with TR = 1.]         \n"
     "                     [The model functions are called for a  ]         \n"
     "                     [time grid of 0, TR, 2*TR, 3*TR, ....  ]         \n"
     "-signal slabel     slabel = name of (non-linear) signal model         \n"
     "-noise  nlabel     nlabel = name of (linear) noise model              \n"
     "-sconstr k c d     constraints for kth signal parameter:              \n"
     "                      c <= gs[k] <= d                                 \n"
     "-nconstr k c d     constraints for kth noise parameter:               \n"
     "                      c+b[k] <= gn[k] <= d+b[k]                       \n"
     "-sigma  s          s = std. dev. of additive Gaussian noise           \n"
     "[-voxel num]       screen output for voxel #num                       \n"
     "-output fname      fname = filename of output 3d + time data file     \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following commands generate individual AFNI 1 sub-brick datasets: \n"
     "                                                                      \n"
     "[-scoef k fname]   write kth signal parameter gs[k];                  \n"
     "                     output 'fim' is written to prefix filename fname \n"
     "[-ncoef k fname]   write kth noise parameter gn[k];                   \n"
     "                     output 'fim' is written to prefix filename fname \n"
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
    );
  
  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/
     
/** macro to test a malloc-ed pointer for validity **/
     
#define MTEST(ptr) \
     if((ptr)==NULL) \
     ( fprintf(stderr,"*** Cannot allocate memory for statistics!\n"         \
	       "*** Try using the -workmem option to reduce memory needs,\n" \
	       "*** or create more swap space in the operating system.\n"    \
	       ), exit(0) )
     

/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  vfp * nmodel,            /* pointer to noise model */
  vfp * smodel,            /* pointer to signal model */  
  char *** npname,         /* noise parameter names */
  char *** spname,         /* signal parameter names */
  float ** min_nconstr,    /* minimum parameter constraints for noise model */
  float ** max_nconstr,    /* maximum parameter constraints for noise model */
  float ** min_sconstr,    /* minimum parameter constraints for signal model */
  float ** max_sconstr,    /* maximum parameter constraints for signal model */
  float * sigma,           /* std. dev. of additive Gaussian noise */
  int * nvoxel,            /* screen output for voxel #nvoxel */
  char ** input_filename,     /* file name of prototype 3d+time dataset */
  char ** output_filename,    /* file name for output 3d+time dataset */
  char *** ncoef_filename,    /* file name for noise model parameters */
  char *** scoef_filename,    /* file name for signal model parameters */
  NL_options * option_data    /* bucket dataset options */
)
 
{
  int ip;                     /* parameter index */


  /*----- initialize default values -----*/
  *sigma = 0.0;
  *nvoxel = -1;
  *smodel = NULL;
  *nmodel = NULL;


  /*----- allocate memory for parameter names -----*/
  *npname = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*npname == NULL)  
    NLfit_error ("Unable to allocate memory for noise parameter names");
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*npname)[ip] = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
      if ((*npname)[ip] == NULL)  
	NLfit_error ("Unable to allocate memory for noise parameter names");
    }

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
  *output_filename = NULL;  
  *ncoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*ncoef_filename == NULL)
    NLfit_error ("Unable to allocate memory for ncoef_filename");
  *scoef_filename = (char **) malloc (sizeof(char *) * MAX_PARAMETERS);
  if (*scoef_filename == NULL)
    NLfit_error ("Unable to allocate memory for scoef_filename");
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      (*ncoef_filename)[ip] = NULL;
      (*scoef_filename)[ip] = NULL;
    }


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
  float * sigma,           /* std. dev. of additive Gaussian noise */
  int * nvoxel,            /* screen output for voxel #nvoxel */
  char ** input_filename,     /* file name of prototype 3d+time dataset */
  char ** output_filename,    /* file name for output 3d+time dataset */
  char *** ncoef_filename,    /* file name for noise model parameters */
  char *** scoef_filename,    /* file name for signal model parameters */

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
  THD_3dim_dataset * dset_time;     /* prototype 3d+time data set */

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
  initialize_options (nmodel, smodel, npname, spname,
		      min_nconstr, max_nconstr, min_sconstr, max_sconstr,
		      sigma, nvoxel, input_filename, 
		      output_filename, ncoef_filename, scoef_filename,
		      option_data); 
  
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

	  /*----- initialize data set parameters -----*/
	  dset_time = THD_open_one_dataset (*input_filename);
	  if (dset_time == NULL)  
	    { 
	      sprintf (message, 
		       "Unable to open data file: %s", *input_filename);
	      NLfit_error (message);
	    }
	  *nxyz =  dset_time->dblk->diskptr->dimsizes[0]
	    * dset_time->dblk->diskptr->dimsizes[1]
	    * dset_time->dblk->diskptr->dimsizes[2] ;
	  *ts_length = DSET_NUM_TIMES(dset_time);

     DSET_UNMSEC(dset_time) ;  /* RWCox */

     dsTR = DSET_TIMESTEP(dset_time) ;

	  THD_delete_3dim_dataset(dset_time, False);  dset_time = NULL ;

	  nopt++;
	  continue;
	}

      /*----- 22 July 1998: the -inTR option -----*/

      if( strncmp(argv[nopt],"-inTR",5) == 0 ){
         inTR = 1 ;
         nopt++ ; continue ;
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
      if (strncmp(argv[nopt], "-sconstr", 8) == 0 || strncmp(argv[nopt],"-scnstr",8) == 0 )
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
      if (strncmp(argv[nopt], "-nconstr", 8) == 0 || strncmp(argv[nopt],"-ncnstr",8) == 0 )
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
      
      
       /*-----   -sigma s  -----*/
      if (strncmp(argv[nopt], "-sigma", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -sigma ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if (fval < 0.0)
	    NLfit_error ("illegal argument after -sigma ");
	  *sigma = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -voxel num  -----*/
      if (strncmp(argv[nopt], "-voxel", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -voxel ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    NLfit_error ("illegal argument after -voxel ");
	  *nvoxel = ival;
	  nopt++;
	  continue;
	}
      
      
       /*-----   -output filename   -----*/
      if (strncmp(argv[nopt], "-output", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NLfit_error ("need argument after -output ");
	  *output_filename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (*output_filename == NULL)
	    NLfit_error ("Unable to allocate memory for output_filename");
	  strcpy (*output_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

     /*-----   -scoef k filename   -----*/
      if (strncmp(argv[nopt], "-scoef", 6) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -scoef ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *p))
	    NLfit_error ("illegal argument after -scoef ");
	  index = ival;
	  nopt++;

	  (*scoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if ((*scoef_filename)[index] == NULL)
	    NLfit_error ("Unable to allocate memory for scoef_filename");
	  strcpy ((*scoef_filename)[index], argv[nopt]);

	  nopt++;
	  continue;
	}
      

     /*-----   -ncoef k filename   -----*/
      if (strncmp(argv[nopt], "-ncoef", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NLfit_error ("need 2 arguments after -ncoef ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival >= *r))
	    NLfit_error ("illegal argument after -ncoef ");
	  index = ival;
	  nopt++;

	  (*ncoef_filename)[index] = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if ((*ncoef_filename)[index] == NULL)
	    NLfit_error ("Unable to allocate memory for ncoef_filename");
	  strcpy ((*ncoef_filename)[index], argv[nopt]);

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
	    nbricks = (*p) + (*r);
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

	  else  NLfit_error ("unable to interpret options after -brick ");
	  	  
	  printf ("ibrick = %d \n", ibrick);
	  printf ("brick_type  = %d \n", option_data->brick_type[ibrick]);
	  printf ("brick_coef  = %d \n", option_data->brick_coef[ibrick]);
	  printf ("brick_label = %s \n", option_data->brick_label[ibrick]);
	  
	  nopt++;
	  continue;
	}
     
      
      /*----- unknown command -----*/

      { char buf[256] ;
        sprintf(buf,"unrecognized command line option: %s",argv[nopt]) ;
        NLfit_error (buf);
      }
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
  THD_3dim_dataset * dset,          /* prototype 3d+time data set */
  char * filename                   /* name of output file */
)

{
  THD_3dim_dataset * new_dset = NULL;   /* output afni data set pointer */
  int ierror;                           /* number of errors in editing data */
  
  
  /*----- make an empty copy of input dataset -----*/
  new_dset = EDIT_empty_copy (dset);  
  
  ierror = EDIT_dset_items( new_dset,
			    ADN_prefix, filename ,
			    ADN_label1, filename ,
			    ADN_self_name, filename ,
			    ADN_type, ISHEAD(dset) ? HEAD_FUNC_TYPE : 
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
  char * input_filename,         /* file name for input 3d+time data set */
  char * output_filename,        /* file name for output 3d+time data set */
  char ** ncoef_filename,        /* file name for noise model parameters */
  char ** scoef_filename,        /* file name for signal model parameters */
  char * bucket_filename         /* file name for bucket dataset */
)

{
  THD_3dim_dataset * dset_time;  /* prototype 3d+time data set */
  int ip;                        /* parameter index */
  

  dset_time = THD_open_one_dataset (input_filename);


  if (output_filename != NULL)   
    check_one_output_file (dset_time, output_filename);
  
  for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
    {
      if (ncoef_filename[ip] != NULL)
	check_one_output_file (dset_time, ncoef_filename[ip]);
      if (scoef_filename[ip] != NULL)
	check_one_output_file (dset_time, scoef_filename[ip]);
    }


  if (bucket_filename != NULL)   
    check_one_output_file (dset_time, bucket_filename);

  THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL ;
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

  char * input_filename,          /* file name of prototype 3d+time dataset */
  char * output_filename,         /* file name for output 3d+time data set */
  char ** ncoef_filename,         /* file name for noise model parameters */
  char ** scoef_filename,         /* file name for signal model parameters */
  char * bucket_filename          /* file name for bucket dataset */
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
      

  /*----- check whether any of the output files already exist -----*/
  if( THD_deathcon() )
    check_output_files (input_filename, output_filename, 
		      ncoef_filename, scoef_filename, bucket_filename);

}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program 
(
  int argc,                /* number of input arguments */
  char ** argv,            /* array of input arguments */ 
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
  float * sigma,           /* std. dev. of additive Gaussian noise */
  int * nvoxel,            /* screen output for voxel #nvoxel */

  char ** input_filename,     /* file name of prototype 3d+time dataset */
  char ** output_filename,    /* file name for output 3d+time dataset */
  char *** ncoef_filename,    /* file name for noise model parameters */
  char *** scoef_filename,    /* file name for signal model parameters */

  int * nxyz,                       /* number of voxels in image */
  int * ts_length,                  /* length of time series data */  

  float *** x_array,       /* independent variable matrix */
  float ** ts_array,       /* input time series array */
  short *** d_array,       /* output times series volume */

  float ** par_full,       /* estimated parameters for the full model */

  float *** ncoef_vol,     /* volume of noise model parameters */
  float *** scoef_vol,     /* volume of signal model parameters */

  NL_options * option_data          /* bucket dataset options */

)
     
{
  int dimension;           /* dimension of full model */
  int ip;                  /* parameter index */
  int it;                  /* time index */


  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;

  
  /*----- get command line inputs -----*/
  get_options (argc, argv, nname, sname, nmodel, smodel, 
	       r, p, npname, spname,
	       min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
	       sigma, nvoxel,
	       input_filename, output_filename, ncoef_filename, scoef_filename,
	       nxyz, ts_length, option_data);

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*r, *p, *min_nconstr, *max_nconstr, 
			  *min_sconstr, *max_sconstr, *input_filename, 
			  *output_filename, *ncoef_filename, *scoef_filename,
			  option_data->bucket_filename);

  /*----- allocate space for input time series -----*/
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
  
  /*----- allocate memory space for parameters -----*/
  dimension = (*r) + (*p);
  *par_full = (float *) malloc (sizeof(float) * dimension);
  if (*par_full == NULL)
    NLfit_error ("Unable to allocate memory for par_full");

  *ncoef_vol = (float **) malloc (sizeof(float *) * (*r));
  if (*ncoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for ncoef_vol");
  for (ip = 0;  ip < (*r);  ip++)
    {
      (*ncoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));
      if ((*ncoef_vol)[ip] == NULL)
	NLfit_error ("Unable to allocate memory for ncoef_vol[ip]");
    }
  
  *scoef_vol = (float **) malloc (sizeof(float *) * (*p));
  if (*scoef_vol == NULL)
    NLfit_error ("Unable to allocate memory for scoef_vol");
  for (ip = 0;  ip < (*p);  ip++)
    {
      (*scoef_vol)[ip] = (float *) malloc (sizeof(float) * (*nxyz));
      if ((*scoef_vol)[ip] == NULL)
	NLfit_error ("Unable to allocate memory for scoef_vol[ip]");
    }

  
  /*----- allocate space for output time series volume -----*/
  *d_array = (short **) malloc (sizeof(short *) * (*ts_length));
  if (*d_array == NULL)
    NLfit_error ("Unable to allocate memory for d_array");
  for (it = 0;  it < *ts_length;  it++)
    {
      (*d_array)[it] = (short *) malloc (sizeof(short) * (*nxyz));
      if ((*d_array)[it] == NULL)
	NLfit_error ("Unable to allocate memory for d_array[it]");
    }


  /*----- initialize random number generator -----*/
  srand48 (1234567);
 
}


/*---------------------------------------------------------------------------*/
/*
  Generate artificial time series array.
*/

void generate_ts_array 
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
 
  float sigma,            /* std. dev. of additive Gaussian noise */

  float * par_true,       /* true parameters for this time series */
  float * ts_array        /* artificially generated time series data */
)

{
  int ip;                 /* parameter index */
  int it;                 /* time index */
  float n1, n2;           /* normal random variates */


  /*----- select 'true' noise parameters -----*/
  for (ip = 0;  ip < r;  ip++)
    par_true[ip] = get_random_value (min_nconstr[ip], max_nconstr[ip]);

  /*----- select 'true' signal parameters -----*/
  for (ip = 0;  ip < p;  ip++)
    par_true[ip+r] = get_random_value (min_sconstr[ip], max_sconstr[ip]);
  
  /*----- calculate the 'true' time series using the 'true' parameters -----*/
  full_model (nmodel, smodel, par_true, par_true + r, 
	      ts_length, x_array, ts_array);

  /*----- add Gaussian noise -----*/
  for (it = 0;  it < ts_length;  it++)
    {
      normal (&n1, &n2);
      ts_array[it] += n1*sigma;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Save time series into AFNI 3d+time data set.
*/

void save_ts_array 
(
  int iv,                  /* save time series for this voxel */
  int ts_length,           /* length of time series array */
  float * ts_array,        /* time series data for voxel #iv */
  short ** d_array         /* output time series volume */
)

{
  int it;


  for (it = 0;  it < ts_length;  it++)
    {
      d_array[it][iv] = (short) ts_array[it];
    }
        
}


/*---------------------------------------------------------------------------*/
/*
  Print out the given time series.
*/

void write_ts_array 
(
  int iv,                  /* write time series for this voxel */
  int ts_length,           /* length of time series array */
  float * ts_array,        /* time series data for voxel #iv */
  short ** d_array         /* output time series volume */
)

{
  int it;                      /* time index */

  for (it = 0;  it < ts_length;  it++)
    printf ("ts_array[%d] = %9.3f   d_array[%d][%d] = %d    \n", 
	    it, ts_array[it], it, iv+1, d_array[it][iv]);
      
}


/*---------------------------------------------------------------------------*/
/*
  Write parameters for this voxel.
*/

void write_parameters 
(
  int iv,                  /* current voxel number */
  char * nname,            /* name of noise model */
  char * sname,            /* name of signal model */
  int r,                   /* number of parameters in the noise model */
  int p,                   /* number of parameters in the signal model */
  char ** npname,          /* noise parameter names */
  char ** spname,          /* signal parameter names */
  float * par_full         /* estimated parameters for the full model */
)

{
  int ip;                  /* parameter index */


  if (iv < 0)
    printf ("\n\nVoxel Results: \n");
  else
    printf ("\n\nVoxel #%d\n", iv+1);
      

  /*----- write full model parameters -----*/  
  printf ("\nFull (%s + %s) Model: \n", nname, sname);
  
  for (ip = 0;  ip < r;  ip++)
    printf ("gn[%d]=%12.6f  %s \n", ip, par_full[ip], npname[ip]);
    
  for (ip = 0;  ip < p;  ip++)
    printf ("gs[%d]=%12.6f  %s \n", ip, par_full[ip+r], spname[ip]);	            
}


/*---------------------------------------------------------------------------*/
/*
  Save parameters for output later.
*/

void save_parameters 
(
  int iv,                  /* current voxel number */
  int r,                   /* number of parameters in the noise model */
  int p,                   /* number of parameters in the signal model */
  float * par_full,        /* estimated parameters for the full model */

  float ** ncoef_vol,      /* volume of noise model parameters */
  float ** scoef_vol       /* volume of signal model parameters */
)

{
  int ip;                  /* parameter index */


  /*----- save noise parameter estimates -----*/
  for (ip = 0;  ip < r;  ip++)
    {
      ncoef_vol[ip][iv] = par_full[ip];
    }
      
  /*----- save signal parameter estimates -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      scoef_vol[ip][iv] = par_full[ip+r];
    }

}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI 3d+time data set. 
*/


void output_ts_array 
(
  short ** d_array,                   /* output time series volume data */
  int  ts_length,                     /* length of time series data */  
  char * input_filename,              /* input afni data set file name */
  char * filename                     /* output afni data set file name */
)

{
  THD_3dim_dataset * dset = NULL;     /* input afni data set pointer */
  THD_3dim_dataset * new_dset = NULL; /* output afni data set pointer */
  int ib;                             /* sub-brick index */ 
  int ierror;                         /* number of errors in editing data */
  float * fbuf;                       /* float buffer */
  float fimfac;                       /* scale factor for short data */
  char label[80];                     /* label for output file history */ 
  
     
  /*----- allocate memory -----*/
  fbuf = (float *)  malloc (sizeof(float) * ts_length);   MTEST (fbuf);
  for (ib = 0;  ib < ts_length;  ib++)    fbuf[ib] = 0.0;

  
  /*-- make an empty copy of the prototype dataset, for eventual output --*/
  dset = THD_open_one_dataset (input_filename);
  new_dset = EDIT_empty_copy (dset);


  /*----- Record history of dataset -----*/

  sprintf (label, "Output prefix: %s", filename);
  if( commandline != NULL )
     tross_multi_Append_History( new_dset , commandline,label,NULL ) ;
  else
     tross_Append_History ( new_dset, label);


  /*----- delete prototype dataset -----*/
  THD_delete_3dim_dataset (dset, False);  dset = NULL ;
  

  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_self_name , filename ,
				    ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
			    ADN_nvals , ts_length ,
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

  
  /*----- attach bricks to new data set -----*/
  for (ib = 0;  ib < ts_length;  ib++)
    mri_fix_data_pointer (d_array[ib], DSET_BRICK(new_dset,ib)); 
  
  
  /*----- write afni data set -----*/

  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
  
  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  fprintf(stderr,"--- Wrote combined dataset into %s\n",DSET_BRIKNAME(new_dset)) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  free (fbuf);   fbuf = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI data set.  This data set must be of type 'fim'.
  The intensity data is in ffim.
*/


void write_afni_data (char * input_filename, int nxyz, char * filename,  
                      float * ffim)
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
  if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;
  sprintf (label, "Output prefix: %s", filename);
  tross_Append_History ( new_dset, label);

  
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  output_datum = DSET_BRICK_TYPE(dset,iv) ;
  if( output_datum == MRI_byte ) output_datum = MRI_short ;
  
  
  ibuf[0] = output_datum ; 
  
  func_type = FUNC_FIM_TYPE;
  
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

  
  /*----- attach bricks to new data set -----*/
  mri_fix_data_pointer (vdif, DSET_BRICK(new_dset,0)); 
  
  
  /*----- convert data type to output specification -----*/
  fimfac = EDIT_coerce_autoscale_new (nxyz, 
				      MRI_float, ffim, 
				      output_datum, vdif);
  if (fimfac != 0.0)  fimfac = 1.0 / fimfac;
    

  /*----- write afni data set -----*/
  printf("--- Writing combined dataset into %s\n",DSET_BRIKNAME(new_dset)) ;
  
  for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
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
  int  nxyz,              /* number of voxels in image */
  int  n,                 /* length of time series data */  

  float ** ncoef_vol,     /* volume of noise model parameters */
  float ** scoef_vol,     /* volume of signal model parameters */

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
  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  fprintf(stderr,"Wrote bucket dataset ");
  fprintf(stderr,"into %s\n", DSET_BRIKNAME(new_dset));

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Write out the parameter files.
*/

void output_parameters
(
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int  nxyz,              /* number of voxels in image */
  int  ts_length,         /* length of time series data */  

  float ** ncoef_vol,     /* volume of noise model parameters */
  float ** scoef_vol,     /* volume of signal model parameters */

  char * input_filename,     /* file name of prototype 3d+time dataset */
  char ** ncoef_filename,    /* file name for noise model parameters */
  char ** scoef_filename,    /* file name for signal model parameters */

  NL_options * option_data   /* user input options */
)

{
  int ip;                 /* parameter index */
  int dimension;          /* dimension of full model = r + p  */
  int numdof, dendof;     /* numerator and denominator degrees of freedom */


  dimension = r + p;


  /*----- write the bucket dataset -----*/
  if (option_data->numbricks > 0)
    write_bucket_data (r, p, nxyz, ts_length, ncoef_vol, scoef_vol,	  
		       input_filename, option_data);


  /*----- write noise model parameters -----*/
  for (ip = 0;  ip < r;  ip++)
    {
      if (ncoef_filename[ip] != NULL)
	{
	  write_afni_data (input_filename, nxyz, ncoef_filename[ip], 
			   ncoef_vol[ip]); 
	}
    }

  /*----- write signal model parameters -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      if (scoef_filename[ip] != NULL)
	{
	  write_afni_data (input_filename, nxyz, scoef_filename[ip], 
			   scoef_vol[ip]); 
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
  short *** d_array,           /* output time series volume data */
  char  ** nname,         /* noise model name */
  char  *** npname,       /* noise parameter labels */
  float ** min_nconstr,   /* min parameter constraints for noise model */
  float ** max_nconstr,   /* max parameter constraints for noise model */
  char ** sname,          /* signal model name */
  char *** spname,        /* signal parameter labels */
  float ** par_full,      /* estimated parameters for the full model */
  float ** min_sconstr,   /* min parameter constraints for signal model */
  float ** max_sconstr,   /* max parameter constraints for signal model */
  float *** ncoef_vol,        /* noise model parameters volume data */
  float *** scoef_vol,        /* signal model parameters volume data */
  char ** input_filename,        /* file name of prototype 3d+time dataset */
  char ** output_filename,       /* file name for output 3d+time dataset */
  char *** ncoef_filename,       /* file name for noise model parameters */
  char *** scoef_filename        /* file name for signal model parameters */
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
  if (*output_filename != NULL)
    { free (*output_filename);   *output_filename = NULL; }

  if (*ncoef_filename != NULL)
    {
      for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
	{
	  if ((*ncoef_filename)[ip] != NULL)
	    { free ((*ncoef_filename)[ip]);  (*ncoef_filename)[ip] = NULL; } 
	}
      free (*ncoef_filename);  *ncoef_filename = NULL;
    }

  if (*scoef_filename != NULL)
    {
      for (ip = 0;  ip < MAX_PARAMETERS;  ip++)
	{
	  if ((*scoef_filename)[ip] != NULL)
	    { free ((*scoef_filename)[ip]);  (*scoef_filename)[ip] = NULL; } 
	}
      free (*scoef_filename);  *scoef_filename = NULL;
    }


  /*----- deallocate space for input time series -----*/
  if (*ts_array != NULL)    { free (*ts_array);    *ts_array = NULL; }


  /*----- deallocate space for independent variable matrix -----*/
  if (*x_array != NULL) 
    {
      for (it = 0;  it < ts_length;  it++)
	if ((*x_array)[it] != NULL)
	  { free ((*x_array)[it]);  (*x_array)[it] = NULL; }
      free (*x_array);  *x_array = NULL; 
    }
 

  /*----- deallocate space for parameters -----*/
  if (*par_full != NULL)    { free (*par_full);    *par_full = NULL; }


  /*----- deallocate space for volume data -----*/
  if (*ncoef_vol != NULL)
    {
      for (ip = 0;  ip < r;  ip++)
	{
	  if ((*ncoef_vol)[ip] != NULL)
	    { free ((*ncoef_vol)[ip]);  (*ncoef_vol)[ip] = NULL; }
	}
      free (*ncoef_vol);   *ncoef_vol = NULL; 
    }
  
  if (*scoef_vol != NULL)
    {
      for (ip = 0;  ip < p;  ip++)
	{
	  if ((*scoef_vol)[ip] != NULL)
	    { free ((*scoef_vol)[ip]);  (*scoef_vol)[ip] = NULL; }
	}
      free (*scoef_vol);   *scoef_vol = NULL; 
    }
  
}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  NL_options option_data;  /* bucket dataset options */

  /*----- declare time series variables -----*/
  int ts_length;                       /* length of time series data */
  float ** x_array = NULL;             /* independent variable matrix */
  float * ts_array = NULL;             /* generated time series array */
  int nxyz;                            /* number of voxels in image */
  int iv;                              /* voxel counter */
  int nvoxel;                          /* screen output for voxel #nvoxel */
  short ** d_array = NULL;             /* output time series volume */

  /*----- declare reduced (noise) model variables -----*/
  char * nname = NULL;         /* noise model name */
  vfp nmodel;                  /* pointer to noise model */
  int r;                       /* number of parameters in the noise model */
  char ** npname = NULL;       /* noise parameter labels */
  float * min_nconstr = NULL;  /* min parameter constraints for noise model */
  float * max_nconstr = NULL;  /* max parameter constraints for noise model */

  /*----- declare full (signal+noise) model variables -----*/
  char * sname = NULL;         /* signal model name */
  vfp smodel;                  /* pointer to signal model */
  int p;                       /* number of parameters in the signal model */
  float * par_full = NULL;     /* estimated parameters for the full model */
  char ** spname = NULL;       /* signal parameter labels */
  float * min_sconstr = NULL;  /* min parameter constraints for signal model */
  float * max_sconstr = NULL;  /* max parameter constraints for signal model */

  /*----- declare statistical test variables -----*/
  float sigma;             /* std. dev. of additive Gaussian noise */

  /*----- declare output volume data -----*/
  float ** ncoef_vol = NULL;    /* noise model parameters volume data */
  float ** scoef_vol = NULL;    /* signal model parameters volume data */

  /*----- declare file name variables -----*/
  char * input_filename = NULL;    /* file name of prototype 3d+time dataset */
  char * output_filename = NULL;   /* file name for output 3d+time dataset */
  char ** ncoef_filename = NULL;   /* file name for noise model parameters */
  char ** scoef_filename = NULL;   /* file name for signal model parameters */  
  
  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");
#endif

   PRINT_VERSION("3dTSgen") ; AUTHOR(PROGRAM_AUTHOR);
   mainENTRY("3dTSgen main") ; machdep() ;

   
  /*----- program initialization -----*/
  initialize_program (argc, argv, 
		      &nname, &sname, &nmodel, &smodel, 
		      &r, &p, &npname, &spname,
		      &min_nconstr, &max_nconstr, &min_sconstr, &max_sconstr,
		      &sigma, &nvoxel,
		      &input_filename, &output_filename, &ncoef_filename,
		      &scoef_filename,
		      &nxyz, &ts_length, &x_array, &ts_array, &d_array, 
		      &par_full, 
		      &ncoef_vol, &scoef_vol, &option_data);


  /*----- loop over voxels in the data set -----*/
  for (iv = 0;  iv < nxyz;  iv++)
    {

      /*----- generate artificial time series for voxel iv -----*/
      generate_ts_array (nmodel, smodel, r, p,
			 min_nconstr, max_nconstr, min_sconstr, max_sconstr,
			 ts_length, x_array, sigma,
			 par_full, ts_array);


      /*----- save time series into data set -----*/
      save_ts_array (iv, ts_length, ts_array, d_array);


      /*----- write time series for this voxel -----*/
      if (iv == nvoxel-1)
	write_ts_array (iv, ts_length, ts_array, d_array);
      

      /*----- save parameters for this voxel into volume data -----*/
      save_parameters (iv, r, p, par_full, ncoef_vol, scoef_vol);
      

      /*----- write parameters for this voxel -----*/
      if (iv == nvoxel-1)
	write_parameters (iv, nname, sname, r, p, npname, spname, par_full);

    }


  /*----- save time series into data set -----*/
  output_ts_array (d_array, ts_length, input_filename, output_filename);


  /*----- output the parameter files -----*/
  output_parameters (r, p, nxyz, ts_length, ncoef_vol, scoef_vol,
		  input_filename,
		  ncoef_filename, scoef_filename, &option_data);

		 
  /*----- end of program -----*/
  terminate_program (r, p, ts_length, &x_array, &ts_array, &d_array, 
		     &nname, &npname, &min_nconstr, &max_nconstr, 
		     &sname, &spname, &par_full, &min_sconstr, &max_sconstr, 
		     &ncoef_vol, &scoef_vol,
		     &input_filename, &output_filename, 
		     &ncoef_filename, &scoef_filename);
}




