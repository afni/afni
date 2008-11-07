/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1998-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
/*---------------------------------------------------------------------------*/
/*
  Program to calculate the voxelwise convolution of given impulse response   
  function (IRF) time series contained in a 3d+time dataset with a specified 
  input stimulus function time series.  This program will also calculate     
  convolutions involving multiple IRF's and multiple stimulus functions.      
  Input options include addition of system noise to the estimated output.    
  Output consists of an AFNI 3d+time dataset which contains the estimated    
  system response.  Alternatively, if all inputs are .1D time series files,  
  then the output will be a single .1D time series file.                     

  File:    3dConvolve.c
  Author:  B. Douglas Ward
  Date:    28 June 2001

  Mod:     Correction to baseline parameter input error checking for case 
           of concatenated runs.
  Date:    11 July 2001

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Allow user to specify no baseline parameters in the model with
           command "-polort -1".
  Date:    28 February 2002

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "3dConvolve"                 /* name of this program */
#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "28 June 2001"    /* date of initial program release */
#define PROGRAM_LATEST  "28 Feb  2002"    /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define RA_error DC_error

/*---------------------------------------------------------------------------*/

#include "mrilib.h"
#include "matrix.h"

#include "Deconvolve.c"
#include "randgen.c"

/*---------------------------------------------------------------------------*/

typedef struct DC_options
{ 
  int nxyz;                /* number of voxels in the input dataset */
  int nt;                  /* number of input 3d+time dataset time points */
  int NFirst;              /* first time point to calculate by convolution */
  int NLast;               /* last time point to calculate by convolution */
  int N;                   /* total number of time points to calculate */
  int polort;              /* degree of polynomial for baseline model */
  int p;                   /* number of parameters in the full model */
  int q;                   /* number of parameters in the baseline model */
  int input1D;             /* flag indicating all inputs are .1D time series */
  char * input_filename;   /* input 3d+time dataset as template */
  char * mask_filename;    /* input mask dataset */
  char * base_filename;    /* file containing baseline parameters */
  char * censor_filename;  /* input censor time series filename */
  char * concat_filename;  /* filename for list of concatenated runs */

  int num_stimts;          /* number of stimulus time series */
  char ** stim_filename;   /* input stimulus time series file name */
  int * stim_minlag;       /* min. time lag for impulse response */
  int * stim_maxlag;       /* max. time lag for impulse response */
  int * stim_nptr;         /* number of stim fn. points per TR */

  char ** iresp_filename;  /* impulse response 3d+time input */
  char * errts_filename;   /* error time series 3d+time input */

  float sigma;             /* std.dev. for additive Gaussian noise */
  long seed;               /* seed for random number generator */

  int xout;                   /* flag to write X matrix to screen */
  char * output_filename;     /* convolved time series output filename */

} DC_options;


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void DC_error (char * message)
{
  fprintf (stderr, "%s Error: %s \a\n\n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dConvolve help menu.
*/

void display_help_menu()
{
  printf (
"Program to calculate the voxelwise convolution of given impulse response   \n"
"function (IRF) time series contained in a 3d+time dataset with a specified \n"
"input stimulus function time series.  This program will also calculate     \n"
"convolutions involving multiple IRF's and multiple stimulus functions.     \n"
"Input options include addition of system noise to the estimated output.    \n"
"Output consists of an AFNI 3d+time dataset which contains the estimated    \n"
"system response.  Alternatively, if all inputs are .1D time series files,  \n"
"then the output will be a single .1D time series file.                     \n"
    "                                                                       \n"
    "Usage:                                                                 \n"
    "3dConvolve                                                             \n"
    "-input fname         fname = filename of 3d+time template dataset      \n"
    "[-input1D]           flag to indicate all inputs are .1D time series   \n"
    "[-mask mname]        mname = filename of 3d mask dataset               \n"
    "[-censor cname]      cname = filename of censor .1D time series        \n"
    "[-concat rname]      rname = filename for list of concatenated runs    \n"
    "[-nfirst fnum]       fnum = number of first time point to calculate by \n"
    "                       convolution procedure.  (default = max maxlag)  \n"
    "[-nlast  lnum]       lnum = number of last time point to calculate by  \n"
    "                       convolution procedure.  (default = last point)  \n"
    "[-polort pnum]       pnum = degree of polynomial corresponding to the  \n"
    "                       baseline model  (default: pnum = 1)             \n"
    "[-base_file bname]   bname = file containing baseline parameters       \n"
    "                                                                       \n"
    "-num_stimts num      num = number of input stimulus time series        \n"
    "                       (default: num = 0)                              \n"
    "-stim_file k sname   sname = filename of kth time series input stimulus\n"
    "[-stim_minlag k m]   m = minimum time lag for kth input stimulus       \n"
    "                       (default: m = 0)                                \n"
    "[-stim_maxlag k n]   n = maximum time lag for kth input stimulus       \n"
    "                       (default: n = 0)                                \n"
    "[-stim_nptr k p]     p = number of stimulus function points per TR     \n"
    "                       Note: This option requires 0 slice offset times \n"
    "                       (default: p = 1)                                \n"
    "                                                                       \n"
    "[-iresp k iprefix]   iprefix = prefix of 3d+time input dataset which   \n"
    "                       contains the kth impulse response function      \n"
    "                                                                       \n"
    "[-errts eprefix]     eprefix = prefix of 3d+time input dataset which   \n"
    "                       contains the residual error time series         \n"
    "                       (i.e., noise which will be added to the output) \n"
    "                                                                       \n"
    "[-sigma s]           s = std. dev. of additive Gaussian noise          \n"
    "                       (default: s = 0)                                \n"
    "[-seed d]            d = seed for random number generator              \n"
    "                       (default: d = 1234567)                          \n"
    "                                                                       \n"
    "[-xout]              flag to write X matrix to screen                  \n"
    "[-output tprefix]    tprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the convolved time series data     \n"
    "                       (or tprefix = prefix of .1D output time series  \n"
    "                       if the -input1D option is used)                 \n"
    "                                                                       \n"
    );
  
  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  DC_options * option_data    /* deconvolution program options */
)
 
{
  int is;                     /* input stimulus time series index */



  /*----- Initialize default values -----*/
  option_data->nxyz    = -1;
  option_data->nt      = -1;
  option_data->NFirst  = -1;
  option_data->NLast   = -1;
  option_data->N       = 0;
  option_data->polort  = 1;
  option_data->p       = 0;
  option_data->q       = 0;
  option_data->input1D = 0;
  option_data->sigma   = 0.0;
  option_data->seed    = 1234567;
  option_data->xout    = 0;
  

  /*----- Initialize stimulus options -----*/
  option_data->num_stimts     = 0;
  option_data->stim_filename  = NULL;
  option_data->stim_minlag    = NULL;
  option_data->stim_maxlag    = NULL;
  option_data->stim_nptr      = NULL;
  option_data->iresp_filename = NULL;


  /*----- Initialize character strings -----*/
  option_data->input_filename  = NULL;
  option_data->mask_filename   = NULL;  
  option_data->base_filename   = NULL;  
  option_data->censor_filename = NULL;
  option_data->concat_filename = NULL;
  option_data->errts_filename  = NULL;
  option_data->output_filename = NULL;


}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the stimulus options.
*/
 
void initialize_stim_options 
(
  DC_options * option_data,   /* deconvolution program options */
  int num_stimts              /* number of input stimulus time series */
)
 
{
  int is;                     /* input stimulus time series index */


  /*----- Set number of input stimulus time series -----*/
  if (num_stimts <= 0)  return;
  else  option_data->num_stimts = num_stimts;


  /*----- Allocate memory for stimulus options -----*/
  option_data->stim_filename = (char **) malloc (sizeof(char *) * num_stimts);
  MTEST (option_data->stim_filename);
  option_data->stim_minlag = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_minlag);
  option_data->stim_maxlag = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_maxlag);
  option_data->stim_nptr   = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_nptr);
  option_data->iresp_filename = (char **) malloc (sizeof(char *) * num_stimts);
  MTEST (option_data->iresp_filename);


  /*----- Initialize stimulus options -----*/
  for (is = 0;  is < num_stimts;  is++)
    {  
      option_data->stim_filename[is] = NULL;

      option_data->stim_minlag[is] = 0;
      option_data->stim_maxlag[is] = 0;
      option_data->stim_nptr[is]   = 1;

      option_data->iresp_filename[is] = NULL;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */ 
  DC_options * option_data         /* deconvolution program options */
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  long lval;                        /* long integer input */
  char message[THD_MAX_NAME];       /* error message */
  int k;                            /* stimulus time series index */


  /*-- addto the arglist, if user wants to --*/
  { int new_argc ; char ** new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  

  /*----- does user request help menu? -----*/
  if (argc < 2 || strcmp(argv[1], "-help") == 0)  display_help_menu();  

  
  /*----- add to program log -----*/
  mainENTRY("3dConvolve"); machdep(); PRINT_VERSION("3dConvolve");
  AFNI_logger (PROGRAM_NAME,argc,argv); 

  
  /*----- initialize the input options -----*/
  initialize_options (option_data); 

  
  /*----- main loop over input options -----*/
  while (nopt < argc )
    {
      /*-----   -input filename   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -input ");
	  option_data->input_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->input_filename);
	  strcpy (option_data->input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
     
      /*-----   -input1D   -----*/
      if (strcmp(argv[nopt], "-input1D") == 0)
	{
	  option_data->input1D = 1;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -mask filename   -----*/
      if (strcmp(argv[nopt], "-mask") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -mask ");
	  option_data->mask_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->mask_filename);
	  strcpy (option_data->mask_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -censor filename   -----*/
      if (strcmp(argv[nopt], "-censor") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -censor ");
	  option_data->censor_filename = 
	    malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->censor_filename);
	  strcpy (option_data->censor_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -concat filename   -----*/
      if (strcmp(argv[nopt], "-concat") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -concat ");
	  option_data->concat_filename = 
	    malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->concat_filename);
	  strcpy (option_data->concat_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -nfirst num  -----*/
      if (strcmp(argv[nopt], "-nfirst") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -nfirst ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    DC_error ("illegal argument after -nfirst ");
	  option_data->NFirst = ival;
	  nopt++;
	  continue;
	}


      /*-----   -nlast num  -----*/
      if (strcmp(argv[nopt], "-nlast") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -nlast ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    DC_error ("illegal argument after -nlast ");
	  option_data->NLast = ival;
	  nopt++;
	  continue;
	}


      /*-----   -polort num  -----*/
      if (strcmp(argv[nopt], "-polort") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -polort ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < -1)
	    DC_error ("illegal argument after -polort ");
	  option_data->polort = ival;
	  nopt++;
	  continue;
	}

      
      /*-----   -base_file filename   -----*/
      if (strcmp(argv[nopt], "-base_file") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -base_file ");
	  option_data->base_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->base_filename);
	  strcpy (option_data->base_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -num_stimts num  -----*/
      if (strcmp(argv[nopt], "-num_stimts") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -num_stimts ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    {
	      DC_error ("-num_stimts num   Require: num >= 0 ");
	    }

	  initialize_stim_options (option_data, ival);
	 
	  nopt++;
	  continue;
	}

      
      /*-----   -stim_file k sname   -----*/
      if (strcmp(argv[nopt], "-stim_file") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -stim_file");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_file k sname   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  option_data->stim_filename[k] 
	    = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->stim_filename[k]);
	  strcpy (option_data->stim_filename[k], argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -stim_minlag k lag   -----*/
      if (strcmp(argv[nopt], "-stim_minlag") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  
	    DC_error ("need 2 arguments after -stim_minlag");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_minlag k lag   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    DC_error ("-stim_minlag k lag   Require: 0 <= lag");
	  option_data->stim_minlag[k] = ival;
	  nopt++;
	  continue;
	}
      

      /*-----   -stim_maxlag k lag   -----*/
      if (strcmp(argv[nopt], "-stim_maxlag") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  
	    DC_error ("need 2 arguments after -stim_maxlag");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_maxlag k lag   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    DC_error ("-stim_maxlag k lag   Require: 0 <= lag");
	  option_data->stim_maxlag[k] = ival;
	  nopt++;
	  continue;
	}
      

      /*-----   -stim_nptr k p   -----*/
      if (strcmp(argv[nopt], "-stim_nptr") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  
	    DC_error ("need 2 arguments after -stim_nptr");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_nptr k p   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 1)
	    DC_error ("-stim_nptr k p   Require: 1 <= p");
	  option_data->stim_nptr[k] = ival;
	  nopt++;
	  continue;
	}
      

      /*-----   -iresp k iprefix   -----*/
      if (strcmp(argv[nopt], "-iresp") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -iresp");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-iresp k iprefix   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  option_data->iresp_filename[k] 
	    = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->iresp_filename[k]);
	  strcpy (option_data->iresp_filename[k], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -errts filename   -----*/
      if (strcmp(argv[nopt], "-errts") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need file prefixname after -errts ");
	  option_data->errts_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->errts_filename);
	  strcpy (option_data->errts_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -sigma s  -----*/
      if (strcmp(argv[nopt], "-sigma") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -sigma ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if (fval < 0.0)
	    DC_error ("illegal argument after -sigma ");
	  option_data->sigma = fval;
	  nopt++;
	  continue;
	}
      

      /*-----  -seed s  -----*/
      if (strcmp(argv[nopt], "-seed") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -seed ");
	  sscanf (argv[nopt], "%ld", &lval);
	  if (lval <= 0)
	    DC_error ("illegal argument after -seed ");
	  option_data->seed = lval;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -xout   -----*/
      if (strcmp(argv[nopt], "-xout") == 0)
	{
	  option_data->xout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -output filename   -----*/
      if (strcmp(argv[nopt], "-output") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -output ");
	  option_data->output_filename = 
	    malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->output_filename);
	  strcpy (option_data->output_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      DC_error (message);
      
    }


}


/*---------------------------------------------------------------------------*/
/*
  Read time series from specified file name.  This file name may have
  a column selector attached.
*/

float * read_time_series 
(
  char * ts_filename,          /* time series file name (plus column index) */
  int * ts_length              /* output value for time series length */
)

{
  char message[THD_MAX_NAME];    /* error message */
  char * cpt;                    /* pointer to column suffix */
  char filename[THD_MAX_NAME];   /* time series file name w/o column index */
  char subv[THD_MAX_NAME];       /* string containing column index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures 
			      -- used to read 1D ASCII */
  float * far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */
  int iy;                  /* time series file column index */
  int ipt;                 /* time point index */
  float * ts_data = NULL;  /* input time series data */


  /*----- First, check for empty filename -----*/
  if (ts_filename == NULL)
    DC_error ("Missing input time series file name");


  /*----- Read the time series file -----*/

  flim = mri_read_1D(ts_filename) ;
  if( flim == NULL )
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      DC_error (message);
    }

  
  /*----- Set pointer to data, and set dimensions -----*/
  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny; iy = 0 ;
  if( ny > 1 ){
    fprintf(stderr,"WARNING: time series %s has %d columns\n",ts_filename,ny);
  }

  /*----- Save the time series data -----*/
  *ts_length = nx;
  ts_data = (float *) malloc (sizeof(float) * nx);
  MTEST (ts_data);
  for (ipt = 0;  ipt < nx;  ipt++)
    ts_data[ipt] = far[ipt + iy*nx];   
  
  
  mri_free (flim);  flim = NULL;

  return (ts_data);
}


/*---------------------------------------------------------------------------*/
/*
  Read the input data files.
*/

void read_input_data
(
  DC_options * option_data,         /* deconvolution program options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  THD_3dim_dataset ** base_dset,    /* input baseline parameter data set */
  THD_3dim_dataset ** err_dset,     /* input error 3d+time data set */
  THD_3dim_dataset *** irf_dset,    /* input IRF 3d+time dataset array */
  float ** base_data,               /* input baseline parameters */
  int * base_length,                /* number of input baseline parameters */
  float *** irf_data,               /* input IRF time series data */
  int ** irf_length,                /* length of IRF time series */
  float ** censor_array,            /* input censor time series array */
  int * censor_length,              /* length of censor time series */
  int ** block_list,                /* list of block (run) starting points */
  int * num_blocks,                 /* number of blocks (runs) */
  float *** stimulus,               /* stimulus time series arrays */
  int ** stim_length,               /* length of stimulus time series */
  float ** errts_data,              /* error time series data */
  int * errts_length                /* length of error time series data */
)

{
  char message[THD_MAX_NAME];   /* error message */
  int it;                  /* time point index */
  int nt;                  /* number of input data time points */
  int nxyz;                /* number of voxels */
  int num_stimts;          /* number of stimulus time series arrays */
  int * min_lag;           /* minimum time delay for impulse response */
  int * max_lag;           /* maximum time delay for impulse response */
  int q;                   /* number of baseline parameters */
  int p;                   /* total number of parameters */
  int is;                  /* stimulus time series index */


  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;


  /*----- Read the block list -----*/
  if (option_data->concat_filename == NULL)
    {
      *num_blocks = 1;
      *block_list = (int *) malloc (sizeof(int) * 1);
      (*block_list)[0] = 0;
    }
  else
    {
      float * f = NULL;

      f = read_time_series (option_data->concat_filename, num_blocks);
      if (*num_blocks < 1)
	{
	  sprintf (message, "Problem reading concat file: %s ",
		   option_data->concat_filename);
	  DC_error (message);
	}
      else
	{
	  *block_list = (int *) malloc (sizeof(int) * (*num_blocks));
	  for (it = 0;  it < *num_blocks;  it++)
	    (*block_list)[it] = floor (f[it]+0.5);
	}
    }


  /*----- Determine total number of parameters in the model -----*/
  q = (option_data->polort + 1) * (*num_blocks);
  p = q;
  for (is = 0;  is < num_stimts;  is++)
    {
      if (max_lag[is] < min_lag[is])
	DC_error ("Require min lag <= max lag for all stimuli");
      p += max_lag[is] - min_lag[is] + 1;
    }
  option_data->p = p;
  option_data->q = q;


  /*----- Read the input baseline, IRF, and error time series data -----*/


  /*----- Read .1D files -----*/  
  if (option_data->input1D)   
    {
      *dset_time = NULL;
      nt = option_data->NLast + 1;
      nxyz = 1;

      /*----- Read the baseline parameter .1D time series -----*/
      if (option_data->base_filename != NULL)
	{
	  *base_data = read_time_series (option_data->base_filename, 
					 base_length);
	  if (*base_data == NULL)
	    { 
	      sprintf (message,  "Unable to read baseline .1D file: %s", 
		       option_data->base_filename);
	      DC_error (message);
	    }
	}
      else
	{
	  *base_data = NULL;
	  *base_length = 0;
	}

      /*----- Read the input IRF .1D time series -----*/
      if (num_stimts > 0)
	{
	  *irf_data = (float **) malloc (sizeof(float *) * num_stimts);
	  MTEST (*irf_data);
	  *irf_length = (int *) malloc (sizeof(int) * num_stimts);
	  MTEST (*irf_length);
	    for (is = 0;  is < num_stimts;  is++)
	      {
		(*irf_data)[is] 
		  = read_time_series (option_data->iresp_filename[is], 
				      &(*irf_length)[is]);
		if ((*irf_data)[is] == NULL)  
		  { 
		    sprintf (message,  "Unable to read IRF .1D file: %s", 
			     option_data->iresp_filename[is]);
		    DC_error (message);
		  }
	      }  
	}

      /*----- Read the error .1D time series -----*/
      if (option_data->errts_filename != NULL)
	{
	  *errts_data = read_time_series (option_data->errts_filename, 
					  errts_length);
	  if (*errts_data == NULL)
	    { 
	      sprintf (message,  "Unable to read residual error .1D file: %s", 
		       option_data->errts_filename);
	      DC_error (message);
	    }
	}
      else
	{
	  *errts_data = NULL;
	  *errts_length = 0;
	}
    }


  /*----- Read 3d+time datasets -----*/
  else if (option_data->input_filename != NULL)   
    {
      /*----- Read the input 3d+time (template) dataset -----*/
      *dset_time = THD_open_one_dataset (option_data->input_filename);
      if (!ISVALID_3DIM_DATASET(*dset_time))  
	{ 
	  sprintf (message,  "Unable to open template dataset file: %s", 
		   option_data->input_filename);
	  DC_error (message);
	}  
      DSET_load(*dset_time) ; CHECK_LOAD_ERROR(*dset_time) ;
      nt = DSET_NUM_TIMES (*dset_time);
      nxyz = DSET_NVOX (*dset_time);

      /*----- Read the input mask dataset -----*/
      if (option_data->mask_filename != NULL)
	{
	  *mask_dset = THD_open_dataset (option_data->mask_filename);
	  if (!ISVALID_3DIM_DATASET(*mask_dset))  
	    { 
	      sprintf (message,  "Unable to open mask file: %s", 
		       option_data->mask_filename);
	      DC_error (message);
	    }  
     DSET_load(*mask_dset) ; CHECK_LOAD_ERROR(*mask_dset) ;
	}

      /*----- Read the input baseline parameter dataset -----*/
      if (option_data->base_filename != NULL)
	{
	  *base_dset = THD_open_dataset (option_data->base_filename);
	  if (!ISVALID_3DIM_DATASET(*base_dset))  
	    { 
	      sprintf (message,  "Unable to open baseline parameter file: %s", 
		       option_data->base_filename);
	      DC_error (message);
	    }  
	  DSET_load(*base_dset); CHECK_LOAD_ERROR(*base_dset);
	  *base_length = DSET_NVALS (*base_dset);
	}
      else
	{
	  *base_dset = NULL;
	  *base_length = 0;
	}

      /*----- Read the IRF 3d+time datasets -----*/
      if (num_stimts > 0)
	{
	  *irf_length = (int *) malloc (sizeof(int) * num_stimts);
	  MTEST (*irf_length);

	  *irf_dset = (THD_3dim_dataset **) 
	    malloc (sizeof(THD_3dim_dataset *) * num_stimts);
	  MTEST (*irf_dset);

	  for (is = 0;  is < num_stimts;  is++)
	    {
	      (*irf_dset)[is]
		= THD_open_dataset (option_data->iresp_filename[is]);
	      if (!ISVALID_3DIM_DATASET((*irf_dset)[is]))  
		{ 
		  sprintf (message,  "Unable to open IRF file: %s", 
			   option_data->iresp_filename[is]);
		  DC_error (message);
		}  
         DSET_load((*irf_dset)[is]) ; CHECK_LOAD_ERROR((*irf_dset)[is]) ;
	      (*irf_length)[is] = DSET_NVALS ((*irf_dset)[is]);
	    }	       
	} 

      /*----- Read the input error time series dataset -----*/
      if (option_data->errts_filename != NULL)
	{
	  *err_dset = THD_open_dataset (option_data->errts_filename);
	  if (!ISVALID_3DIM_DATASET(*err_dset))  
	    { 
	      sprintf (message,  "Unable to open error time series file: %s", 
		       option_data->errts_filename);
	      DC_error (message);
	    }  
     DSET_load(*err_dset); CHECK_LOAD_ERROR(*err_dset);
	  *errts_length = DSET_NVALS (*err_dset);
	}
      else
	{
	  *err_dset = NULL;
	  *errts_length = 0;
	}
    }

  else
    DC_error ("Must specify input 3d+time template dataset");


  /*----- Check number of data points -----*/
  if (nt <= 0)      DC_error ("No time points?  Please use -nlast option.");
  option_data->nt = nt;
  if (nxyz < 0)     DC_error ("Program initialization error: nxyz < 0");
  option_data->nxyz = nxyz;


  /*----- Read the censorship file -----*/
  if (option_data->censor_filename != NULL)
    {
      /*----- Read the input censor time series array -----*/
      *censor_array = read_time_series (option_data->censor_filename, 
					censor_length);
      if (*censor_array == NULL)  
	{ 
	  sprintf (message,  "Unable to read censor time series file: %s", 
		   option_data->censor_filename);
	  DC_error (message);
	}  
    }
  else
    {
      /*----- Create censor time series array -----*/
      *censor_array = (float *) malloc (sizeof(float) * nt);
      MTEST (*censor_array);
      *censor_length = nt;
      for (it = 0;  it < nt;  it++)
	(*censor_array)[it] = 1.0;
    }
      

  /*----- Read the input stimulus time series -----*/
  if (num_stimts > 0)
    {
      *stimulus = (float **) malloc (sizeof(float *) * num_stimts);
      MTEST (*stimulus);
      *stim_length = (int *) malloc (sizeof(int) * num_stimts);
      MTEST (*stim_length);

      for (is = 0;  is < num_stimts;  is++)
	{
	  (*stimulus)[is] = read_time_series (option_data->stim_filename[is], 
					      &((*stim_length)[is]));
	  
	  if ((*stimulus)[is] == NULL)
	    {
	      sprintf (message,  "Unable to read stimulus time series: %s", 
		       option_data->stim_filename[is]);
	      DC_error (message);
	    }
	}
    }

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
  char message[THD_MAX_NAME];      /* error message */
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
      sprintf (message,
	       "*** %d errors in attempting to create output dataset!\n", 
	       ierror);
      DC_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists "
	       " -- cannot continue! ",
	       new_dset->dblk->diskptr->header_name);
      DC_error (message);
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
  DC_options * option_data,       /* deconvolution program options */
  THD_3dim_dataset * dset_time    /* input 3d+time data set */
)

{

  if ((option_data->output_filename != NULL) && (option_data->input1D != 1))  
    check_one_output_file (dset_time, option_data->output_filename);
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs 
(
  DC_options * option_data,       /* deconvolution program options */
  THD_3dim_dataset * dset_time,   /* input 3d+time data set */
  THD_3dim_dataset * mask_dset,   /* input mask data set */
  int base_length,                /* number of input baseline parameters */
  int * irf_length,               /* length of input IRF time series */
  float * censor_array,           /* input censor time series array */
  int censor_length,              /* length of censor array */
  int * block_list,               /* list of block (run) starting points */
  int num_blocks,                 /* number of blocks (runs) */
  int * stim_length,              /* length of stimulus time series arrays */
  int errts_length,               /* length of error time series data */
  int ** good_list                /* list of usable time points */
)

{
  char message[THD_MAX_NAME];  /* error message */
  int is;                  /* stimulus index */
  int num_stimts;          /* number of stimulus time series */
  int * min_lag;           /* minimum time delay for impulse response */
  int * max_lag;           /* maximum time delay for impulse response */
  int * nptr;              /* number of stim fn. time points per TR */
  int m;                   /* number of time delays for impulse response */
  int q;                   /* number of baseline parameters */
  int p;                   /* total number of parameters */
  int it;                  /* time point index */
  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */
  int ib;                  /* block (run) index */
  int irb;                 /* time index relative to start of block (run) */
  int exp_length;          /* expected length of IRF time series */


  /*----- Initialize local variables -----*/
  nt = option_data->nt;
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;
  nptr = option_data->stim_nptr;
  p = option_data->p;
  q = option_data->q;


  /*----- Check length of censor array -----*/
  if (censor_length < nt)
    {
      sprintf (message, "Input censor time series file %s is too short",
	       option_data->censor_filename);
      DC_error (message);
    }
  
  
  /*----- Create list of good (usable) time points -----*/
  *good_list = (int *) malloc (sizeof(int) * nt);  MTEST (*good_list);
  NFirst = option_data->NFirst;
  if (NFirst < 0)
    {
      for (is = 0;  is < num_stimts;  is++)
	if (NFirst < (max_lag[is]+nptr[is]-1)/nptr[is])  
	  NFirst = (max_lag[is]+nptr[is]-1)/nptr[is];
    }
  NLast = option_data->NLast;   
  if (NLast < 0)  NLast = nt;

  N = 0;
  ib = 0;
  for (it = block_list[0];  it < nt;  it++)
    {
      if (ib+1 < num_blocks)
	if (it >= block_list[ib+1])  ib++;
      
      irb = it - block_list[ib];
	  
      if ((irb >= NFirst) && (irb <= NLast) && (censor_array[it]))
	{
	  (*good_list)[N] = it;
	  N++;
	}
    }


  /*----- Check for sufficient data -----*/
  if (N == 0)  DC_error ("No usable time points?");
  if (N <= p) 
    {
       sprintf (message,  "Insufficient data for estimating %d parameters", p);
       DC_error (message);
   }
  option_data->N = N;


  /*----- If mask is used, check for compatible dimensions -----*/
  if (mask_dset != NULL)
    {
      if ( (DSET_NX(dset_time) != DSET_NX(mask_dset))
	   || (DSET_NY(dset_time) != DSET_NY(mask_dset))
	   || (DSET_NZ(dset_time) != DSET_NZ(mask_dset)) )
	{
	  sprintf (message, "%s and %s have incompatible dimensions",
		   option_data->input_filename, option_data->mask_filename);
	  DC_error (message);
	}

      if (DSET_NVALS(mask_dset) != 1 )
	DC_error ("Must specify 1 sub-brick from mask dataset");
    }


  /*----- Check number of stimulus time series -----*/
  if (num_stimts < 0)
    {
      DC_error ("Require: 0 <= num_stimts ");
    }


  /*----- Check lengths of stimulus time series -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      if (stim_length[is] < nt*nptr[is])
	{
	  sprintf (message, "Input stimulus time series file %s is too short",
		   option_data->stim_filename[is]);
	  DC_error (message);
	}
    }


  /*----- Check length of error time series -----*/
  if ((errts_length > 0) && (errts_length < nt))
    {
      sprintf (message, "Input error time series file %s is too short",
	       option_data->errts_filename);
      DC_error (message);
    }
  

  /*----- Check number of baseline parameters -----*/
  if (base_length != (option_data->polort+1)*num_blocks)
    {
      sprintf (message, "Baseline parameters:   Expected: %d   Actual: %d",
	       option_data->polort+1, base_length);
      DC_error (message);
    }


  /*----- Check lengths of IRF time series -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      exp_length = max_lag[is] - min_lag[is] + 1;
      if (irf_length[is] != exp_length)
	{
	  sprintf (message, "Length of IRF #%d:   Expected: %d   Actual: %d",
		   is+1, exp_length, irf_length[is]);
	  DC_error (message);
	}
    }


  /*----- Check for zero slice offsets with nptr option -----*/
  if (dset_time != NULL)
    if (dset_time->taxis->nsl > 0)
      for (is = 0;  is < num_stimts;  is++)
	if (nptr[is] > 1)
	  {
	    sprintf (message, "Must align all slices to 0 offset time, \n ");
	    strcat  (message, "before using -stim_nptr option.  ");
	    strcat  (message, "See program 3dTshift. ");
	    DC_error (message);
	  }


  /*----- Check whether any of the output files already exist -----*/
  if( THD_deathcon() ) check_output_files (option_data, dset_time);

}


/*---------------------------------------------------------------------------*/
/*
  Allocate volume memory and fill with zeros.
*/

void zero_fill_volume (float ** fvol, int nxyz)
{
  int ixyz;

  *fvol  = (float *) malloc (sizeof(float) * nxyz);   MTEST(*fvol); 

  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    (*fvol)[ixyz]  = 0.0;
      
}


/*---------------------------------------------------------------------------*/
/*
  Allocate memory for output volumes.
*/

void allocate_memory 
(
  DC_options * option_data,  /* deconvolution algorithm options */
  float *** outts_vol        /* volumes for ouput time series */
)

{
  int nxyz;                  /* total number of voxels */
  int it;                    /* time point index */
  int nt;                    /* number of images in input 3d+time dataset */


  /*----- Initialize local variables -----*/
  nxyz = option_data->nxyz;
  nt = option_data->nt;


  /*----- Allocate memory for fitted time series -----*/
  if (option_data->output_filename != NULL)
    {
      *outts_vol = (float **) malloc (sizeof(float **) * nt);
      MTEST (*outts_vol);
      for (it = 0;  it < nt;  it++)
	{
	  zero_fill_volume (&((*outts_vol)[it]),  nxyz);
	}
    }

}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program 
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */ 
  DC_options ** option_data,       /* deconvolution algorithm options */
  THD_3dim_dataset ** dset_time,   /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,   /* input mask data set */
  THD_3dim_dataset ** base_dset,   /* input baseline parameter data set */
  THD_3dim_dataset ** err_dset,    /* input error 3d+time data set */
  THD_3dim_dataset *** irf_dset,   /* input IRF 3d+time dataset array */
  float ** base_data,              /* input baseline parameters */
  int * base_length,               /* number of input baseline parameters */
  float *** irf_data,              /* input IRF time series data */
  int ** irf_length,               /* length of IRF time series */
  float ** censor_array,           /* input censor time series array */
  int * censor_length,             /* length of censor time series */
  int ** good_list,                /* list of usable time points */
  int ** block_list,               /* list of block (run) starting points */
  int * num_blocks,                /* number of blocks (runs) */
  float *** stimulus,              /* stimulus time series arrays */
  int ** stim_length,              /* length of stimulus time series */
  float ** errts_data,             /* error time series data */
  int * errts_length,              /* length of error time series data */
  float *** predts_vol             /* volumes for predicted time series data */
)
     
{


  /*----- Allocate memory -----*/
  *option_data = (DC_options *) malloc (sizeof(DC_options));

   
  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Read input data -----*/
  read_input_data (*option_data, dset_time, mask_dset, base_dset, err_dset, 
		   irf_dset, base_data, base_length, irf_data, irf_length,
		   censor_array, censor_length, block_list, num_blocks,
		   stimulus, stim_length, errts_data, errts_length);

  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (*option_data, *dset_time, *mask_dset, *base_length,
			  *irf_length, *censor_array, *censor_length, 
			  *block_list, *num_blocks, *stim_length, 
			  *errts_length, good_list);
  

  /*----- Allocate memory for output volumes -----*/
  allocate_memory (*option_data, predts_vol);


  /*----- Initialize random number generator -----*/
  srand48 ((*option_data)->seed);

}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array 
(
  THD_3dim_dataset * dset,           /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
)

{
  MRI_IMAGE * im;          /* intermediate float data */
  float * ar;              /* pointer to float data */
  int it;                  /* time index */
  int nv;                  /* number of volumes */


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  DC_error ("Unable to extract data from 3d+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
  nv  = dset->dblk->nvals ;
  ar = MRI_FLOAT_PTR (im);
  for (it = 0;  it < nv;  it++)
    {
      ts_array[it] = ar[it];
    }


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the system response.
*/

void calc_response
(
  int nt,                    /* number of images in input 3d+time dataset */
  float * ts_array,          /* array of measured data for one voxel */
  int * good_list,           /* list of usable time points */
  int N,                     /* number of usable data points */
  matrix x,                  /* independent variable matrix  */
  vector b,                  /* vector of estimated regression parameters */
  float * predts,            /* output predicted time series */
  float * errts,             /* input error time series */
  float sigma                /* std. dev. of additive Gaussian noise */
)

{
  vector yhat;               /* product Xb */
  int it;                    /* time point index */


  /*----- initialize vectors -----*/
  vector_initialize (&yhat);


  /*----- calculate the system reponse -----*/
  vector_multiply (x, b, &yhat);


  /*----- copy to array -----*/
  for (it = 0;  it < nt;  it++)
    predts[it] = ts_array[it];
  for (it = 0;  it < N;  it++)
    predts[good_list[it]] = yhat.elts[it];


  /*----- add measurement noise -----*/
  if (errts != NULL)
    for (it = 0;  it < nt;  it++)
      {
	predts[it] += errts[it];
      }


  /*----- add Gaussian noise -----*/
  if (sigma > 0.0)
    {
      for (it = 0;  it < N;  it++)
	{
	  predts[good_list[it]] += rand_normal (0.0, sigma*sigma);
	}
    }
    

  /*----- dispose of vectors -----*/
  vector_destroy (&yhat);

 
}


/*---------------------------------------------------------------------------*/
/*
  Save results for this voxel.
*/

void save_voxel 
(
  DC_options * option_data,  /* deconvolution algorithm options */
  int iv,                    /* current voxel index */      
  float * predts,            /* output predicted time series */

  float ** predts_vol        /* volumes for predicted time series data */

)

{
  int it;                    /* time point index */
  int nt;                    /* total number of time points */


  if (predts_vol == NULL)  return;


  /*----- Initialize local variables -----*/
  nt = option_data->nt;


  /*----- Save the predicted time series -----*/
  for (it = 0;  it < nt;  it++)
    predts_vol[it][iv] = predts[it];

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the impulse response function and associated statistics.
*/

void calculate_results 
(
  DC_options * option_data,      /* deconvolution algorithm options */
  THD_3dim_dataset * dset,       /* input 3d+time data set */
  THD_3dim_dataset * mask,       /* input mask data set */
  THD_3dim_dataset * base_dset,  /* input baseline parameters data set */
  THD_3dim_dataset * err_dset,   /* input error 3d+time data set */
  THD_3dim_dataset ** irf_dset,  /* input IRF 3d+time dataset array */
  float * base_data,             /* input baseline parameters */
  int base_length,               /* number of input baseline parameters */
  float ** irf_data,             /* input IRF time series data */
  int * irf_length,              /* length of IRF time series */
  int * good_list,               /* list of usable time points */
  int * block_list,              /* list of block (run) starting points */
  int num_blocks,                /* number of blocks (runs) */
  float ** stimulus,             /* stimulus time series arrays */
  int * stim_length,             /* length of stimulus time series */
  float * errts_data,            /* error time series data */
  int  errts_length,             /* length of error time series data */
  float ** predts_vol            /* volumes for predicted time series data */
)
  
{
  float * ts_array = NULL;    /* array of measured data for one voxel */
  float mask_val[1];          /* value of mask at current voxel */

  int p;                      /* number of parameters in the full model */
  int q;                      /* number of parameters in the baseline model */
  int polort;                 /* degree of polynomial for baseline model */
  int m;                      /* parameter index */
  int n;                      /* data point index */

  vector coef;                /* regression parameters */

  matrix xdata;               /* independent variable matrix */

  int ixyz;                   /* voxel index */
  int nxyz;                   /* number of voxels per dataset */

  int nt;                  /* number of images in input 3d+time dataset */
  int N;                   /* number of usable data points */

  int num_stimts;          /* number of stimulus time series */
  int * min_lag;           /* minimum time delay for impulse response */ 
  int * max_lag;           /* maximum time delay for impulse response */ 
  int * nptr;              /* number of stim fn. time points per TR */

  int it;                  /* data point index */
  int ip;                  /* paramter index */
  int is;                  /* stimulus index */
  int ilag;                /* time lag index */
  float stddev;            /* normalized parameter standard deviation */
  char * label;            /* string containing stat. summary of results */
  int novar;               /* flag for insufficient variation in data */

  float * coefts = NULL;       /* regression coefficients */
  float * predts = NULL;       /* predicted time series data */
  float * errts = NULL;        /* full model residual error time series */
  float sigma;                 /* std. dev. of additive Gaussian noise */


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&xdata);
  vector_initialize (&coef);


  /*----- Initialize local variables -----*/
  nxyz = option_data->nxyz;
  nt = option_data->nt;
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;
  nptr    = option_data->stim_nptr;

  polort = option_data->polort;
  q = option_data->q;
  p = option_data->p;
  N = option_data->N;

  sigma = option_data->sigma;

  coefts   = (float *) malloc (sizeof(float) * p);    MTEST (coefts);
  if ((err_dset != NULL) || (errts_data != NULL))
    {
      errts    = (float *) malloc (sizeof(float) * nt);   MTEST (errts);
    }
  predts   = (float *) malloc (sizeof(float) * nt);   MTEST (predts);
  ts_array = (float *) malloc (sizeof(float) * nt);   MTEST (ts_array);
  if (option_data->input1D)
    for (it = 0;  it < nt;  it++)  ts_array[it] = 0.0;
  

  /*----- Initialize the independent variable matrix -----*/
  init_indep_var_matrix (p, q, polort, nt, N, good_list, block_list, 
			 num_blocks, num_stimts, stimulus, stim_length, 
			 min_lag, max_lag, nptr, NULL, &xdata);
  if (option_data->xout)  matrix_sprint ("X matrix:", xdata);


  vector_create (p, &coef);

  
  /*----- Loop over all voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      /*----- Apply mask? -----*/
      if (mask != NULL)
	{
	  extract_ts_array (mask, ixyz, mask_val);
	  if (mask_val[0] == 0.0)  continue; 
	}
      
	  
      /*----- Extract model parameters for this voxel -----*/
      if (option_data->input1D)
	{
	  if (q > 0)
	    {
	      for (ip = 0;  ip < q;  ip++)
		coef.elts[ip] = base_data[ip];
	    }
	  ip = q;
	  for (is = 0;  is < num_stimts;  is++)
	    for (ilag = min_lag[is];  ilag <= max_lag[is];  ilag++)
	      {
		coef.elts[ip] = irf_data[is][ilag-min_lag[is]];
		ip++;
	      }
	}
      else
	{
	  if (q > 0)
	    {
	      extract_ts_array (base_dset, ixyz, coefts);
	      for (ip = 0;  ip < q;  ip++)
		coef.elts[ip] = coefts[ip];
	    }
	  ip = q;
	  for (is = 0;  is < num_stimts;  is++)
	    {
	      extract_ts_array (irf_dset[is], ixyz, coefts);
	      for (ilag = min_lag[is];  ilag <= max_lag[is];  ilag++)
		{
		  coef.elts[ip] = coefts[ilag-min_lag[is]];
		  ip++;
		}	  
	    }
	}
        
   
      /*----- Extract error time series for this voxel -----*/
      if ((option_data->input1D) && (errts_data != NULL))
	for (it = 0;  it < nt;  it++)
	  errts[it] = errts_data[it];
      else  if (err_dset != NULL)
	extract_ts_array (err_dset, ixyz, errts);


      /*----- Extract measured data for this voxel -----*/
      if (option_data->input1D == 0)
	extract_ts_array (dset, ixyz, ts_array);


      /*----- Calculate the system response for this voxel -----*/
      calc_response (nt, ts_array, good_list, N, xdata, coef, 
		     predts, errts, sigma);
      
     
      /*----- Save results for this voxel -----*/
      save_voxel (option_data, ixyz, predts, predts_vol);
       
	  
      /*----- Report results for this voxel -----*/
      if (option_data->input1D) 
	{
	  printf ("\n\nResults for Voxel #%d: \n", ixyz);
	  
	  for (it = 0;  it < nt;  it++)
	    {
	      printf  (" %f \n", predts[it]);
	    }
	  
	}
      
    }  /*----- Loop over voxels -----*/
  

  /*----- Dispose of matrices and vectors -----*/
  vector_destroy (&coef);
  matrix_destroy (&xdata);

  if (coefts != NULL)     { free (coefts);    coefts   = NULL; }
  if (ts_array != NULL)   { free (ts_array);  ts_array = NULL; }
  if (predts != NULL)     { free (predts);    predts   = NULL; }
  if (errts != NULL)      { free (errts);     errts    = NULL; }
}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI 3d+time data set. 
*/


void write_ts_array 
(
  int argc,                              /* number of input arguments */
  char ** argv,                          /* array of input arguments */ 
  DC_options * option_data,              /* deconvolution algorithm options */
  int ts_length,                         /* length of time series data */  
  float ** vol_array,                    /* output time series volume data */
  char * output_filename                 /* output afni data set file name */
)

{
  const float EPSILON = 1.0e-10;

  THD_3dim_dataset * dset = NULL;        /* input afni data set pointer */
  THD_3dim_dataset * new_dset = NULL;    /* output afni data set pointer */
  int ib;                                /* sub-brick index */ 
  int ierror;                            /* number of errors in editing data */
  int nxyz;                              /* total number of voxels */ 
  float factor;             /* factor is new scale factor for sub-brick #ib */
  char * input_filename;    /* input afni data set file name */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */
  float * fbuf;             /* float buffer */
  float * volume;           /* pointer to volume of data */
  char label[80];           /* label for output file */ 
  

  /*----- Initialize local variables -----*/
  input_filename = option_data->input_filename;
  dset = THD_open_one_dataset (input_filename);
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;

 
  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * ts_length);   MTEST (bar);
  fbuf = (float *)  malloc (sizeof(float)   * ts_length);   MTEST (fbuf);
  
  
  /*-- make an empty copy of the prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History( dset , new_dset ) ;

  { char * commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
    sprintf (label, "Output prefix: %s", output_filename);
    if( commandline != NULL )
       tross_multi_Append_History( new_dset , commandline,label,NULL ) ;
    else
       tross_Append_History ( new_dset, label);
    free(commandline) ;
  }

  /*----- Delete prototype dataset -----*/
  THD_delete_3dim_dataset (dset, False);  dset = NULL ;
  

  ierror = EDIT_dset_items (new_dset,
			    ADN_prefix,      output_filename,
			    ADN_label1,      output_filename,
			    ADN_self_name,   output_filename,
			    ADN_malloc_type, DATABLOCK_MEM_MALLOC,  
			    ADN_datum_all,   MRI_short,   
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

      EDIT_misfit_report( DSET_FILECODE(new_dset) , ib ,
                          nxyz , factor , bar[ib] , volume ) ;
    }


  /*----- write afni data set -----*/

  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset (NULL, NULL, new_dset, True);
  fprintf (stderr,"-- Output 3D+time dataset into %s\n",DSET_BRIKNAME(new_dset)) ;

  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (new_dset, False);   new_dset = NULL ;
  free (fbuf);   fbuf = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Write one time series array to specified file.
*/
 
void write_one_ts 
(
  char * prefix,                  /* output time series prefix name */
  int ts_length,                  /* length of time series data */  
  float ** vol_array              /* output time series volume data */
)

{
  char filename[80];              /* output time series file name */
  int it;                         /* time index */
  FILE * outfile = NULL;          /* file pointer */


  /*----- Create output filename by appending ".1D" -----*/
  sprintf (filename, "%s.1D", prefix);
  outfile = fopen (filename, "w");


  /*----- 'Volume' data consists of just one voxel -----*/
  for (it = 0;  it < ts_length;  it++)
    {
      fprintf (outfile, "%f", vol_array[it][0]);
      fprintf (outfile, " \n");
    }


  fclose (outfile);
}


/*---------------------------------------------------------------------------*/
/*
  Write out the user requested output files.
*/

void output_results
(
  int argc,                 /* number of input arguments */
  char ** argv,             /* array of input arguments */ 
  DC_options * option_data, /* deconvolution algorithm options */

  float ** predts_vol       /* volumes for predicted time series data */
)

{
  int * nptr;               /* number of stim fn. time points per TR */
  int nt;                   /* number of images in input 3d+time dataset */


  /*----- Initialize local variables -----*/
  nptr    = option_data->stim_nptr;
  nt = option_data->nt;


  /*----- Write the predicted time series data -----*/
  if (option_data->output_filename != NULL)

    if (option_data->input1D)
      write_one_ts (option_data->output_filename, nt, predts_vol);
    else
      write_ts_array (argc, argv, option_data, nt, predts_vol, 
		      option_data->output_filename);

  
}


/*---------------------------------------------------------------------------*/

void terminate_program
(
  DC_options ** option_data,    /* deconvolution algorithm options */
  float ** base_data,           /* input baseline parameters */
  float *** irf_data,           /* input IRF time series data */
  int ** irf_length,            /* length of IRF time series */
  float ** censor_array,        /* input censor time series array */
  int ** good_list,             /* list of usable time points */
  int ** block_list,            /* list of block starting points */
  float *** stimulus,           /* stimulus time series arrays */
  int ** stim_length,           /* length of stimulus time series */
  float ** errts_data,          /* input error time series data */
  float *** predts_vol          /* volumes for predicted time series data */
)

{
  int num_stimts;           /* number of stimulus time series */
  int is;                   /* stimulus index */
  int it;                   /* time index */
  int nt;                   /* number of images in input 3d+time dataset */


  /*----- Initialize local variables -----*/
  num_stimts = (*option_data)->num_stimts;
  nt = (*option_data)->nt;


  /*----- Deallocate memory for option data -----*/   
  if (*option_data != NULL)
    { free (*option_data);  *option_data = NULL; }

  /*----- Deallocate base parameter memory -----*/
  if (*base_data != NULL)
    { free (*base_data);  *base_data = NULL; }
  
  /*----- Deallocate memory for IRF time series -----*/
  if (*irf_data != NULL)
    {
      for (is = 0;  is < num_stimts;  is++)
	if ((*irf_data)[is] != NULL)
	  { free ((*irf_data)[is]);  (*irf_data)[is] = NULL; } 
      free (*irf_data);  *irf_data = NULL; 
    } 

  /*----- Deallocate memory for IRF length -----*/
  if (*irf_length != NULL)
    { free (*irf_length);  *irf_length = NULL; }

  /*----- Deallocate memory for censor array -----*/
  if (*censor_array != NULL)
    { free (*censor_array);  *censor_array = NULL; }
  
  /*----- Deallocate memory for list of usable time points -----*/
  if (*good_list != NULL)
    { free (*good_list);  *good_list = NULL; }
  
  /*----- Deallocate memory for list of block starting points -----*/
  if (*block_list != NULL)
    { free (*block_list);  *block_list = NULL; }

 /*----- Deallocate memory for stimulus time series -----*/
  if (*stimulus != NULL)
    {
      for (is = 0;  is < num_stimts;  is++)
	if ((*stimulus)[is] != NULL)
	  { free ((*stimulus)[is]);  (*stimulus)[is] = NULL; } 
      free (*stimulus);  *stimulus = NULL; 
    } 

  /*----- Deallocate memory for length of stimulus time series -----*/
  if (*stim_length != NULL)
    { free (*stim_length);  *stim_length = NULL; }
  
  /*----- Deallocate memory for residual error time series -----*/
  if (*errts_data != NULL)
    { free (*errts_data);  *errts_data = NULL; }
  
  /*----- Deallocate space for predicted time series -----*/
  if (*predts_vol != NULL)
    {
      for (it = 0;  it < nt;  it++)
	if ((*predts_vol)[it] != NULL)
	  {  free ((*predts_vol)[it]);   (*predts_vol)[it] = NULL; }
      free (*predts_vol);   *predts_vol = NULL;
    }

}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  DC_options * option_data;             /* deconvolution algorithm options */
  THD_3dim_dataset * dset_time = NULL;  /* input 3d+time template data set */
  THD_3dim_dataset * mask_dset = NULL;  /* input mask data set */
  THD_3dim_dataset * base_dset = NULL;  /* input baseline parameter data set */
  THD_3dim_dataset * err_dset  = NULL;  /* input error 3d+time data set */
  THD_3dim_dataset ** irf_dset = NULL;  /* IRF 3d+time dataset array */
  float * base_data = NULL;           /* input baseline parameters */
  int base_length;                    /* number of input baseline parameters */
  float ** irf_data = NULL;           /* input IRF time series data */
  int * irf_length = NULL;            /* length of IRF time series */
  float * censor_array = NULL;        /* input censor time series array */
  int censor_length;                  /* length of censor time series */
  int * good_list = NULL;             /* list of usable time points */
  int * block_list = NULL;            /* list of block starting points */
  int num_blocks;                     /* number of blocks (runs) */
  float ** stimulus = NULL;           /* stimulus time series arrays */
  int * stim_length = NULL;           /* length of stimulus time series */
  float * errts_data = NULL;          /* input error time series data */
  int errts_length;                   /* length of error time series */

  float ** predts_vol = NULL;  /* volumes for estimated time series data */
  int is;                      /* stimulus index */
  

  
  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  mainENTRY("3dConvolve main") ; machdep() ; PRINT_VERSION("3dConvolve") ; AUTHOR(PROGRAM_AUTHOR);


  /*----- Program initialization -----*/
  initialize_program (argc, argv, 
              &option_data, &dset_time, &mask_dset, &base_dset, &err_dset,
	      &irf_dset, &base_data, &base_length, &irf_data, &irf_length,
	      &censor_array, &censor_length,
	      &good_list, &block_list, &num_blocks, &stimulus, &stim_length, 
	      &errts_data, &errts_length, &predts_vol);


  /*----- Perform convolution -----*/
  calculate_results (option_data, dset_time, mask_dset, base_dset, err_dset, 
		     irf_dset, base_data, base_length, irf_data, irf_length,
		     good_list, block_list, num_blocks, stimulus, stim_length,
		     errts_data, errts_length, predts_vol);
  

  /*----- Deallocate memory for input datasets -----*/   
  if (dset_time != NULL)  
    { THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL; }
  if (mask_dset != NULL)  
    { THD_delete_3dim_dataset (mask_dset, False);  mask_dset = NULL; }
  if (base_dset != NULL)  
    { THD_delete_3dim_dataset (base_dset, False);  base_dset = NULL; }
  if (err_dset != NULL)  
    { THD_delete_3dim_dataset (err_dset, False);   err_dset = NULL; }
  if (irf_dset != NULL)  
    { 
      for (is = 0;  is < option_data->num_stimts;  is++)
	if (irf_dset[is] != NULL)  
	  { 
	    THD_delete_3dim_dataset (irf_dset[is], False);  
	    irf_dset[is] = NULL; 
	  }
      free(irf_dset);   irf_dset = NULL; 
    }


  /*----- Write requested output files -----*/
  output_results (argc, argv, option_data, predts_vol);


  /*----- Terminate program -----*/
  terminate_program (&option_data, &base_data, &irf_data, &irf_length,
		     &censor_array, &good_list, &block_list,
		     &stimulus, &stim_length, &errts_data, &predts_vol);

  exit(0);
}


/*---------------------------------------------------------------------------*/









