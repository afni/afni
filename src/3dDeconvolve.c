/*---------------------------------------------------------------------------*/
/*
  Program to calculate the deconvolution of a measured 3d+time dataset
  with a specified input stimulus time series.  This program will also
  perform multiple linear regression using multiple input stimulus time
  series. Output consists of an AFNI 'bucket' type dataset containing the
  least squares estimates of the linear regression coefficients, t-statistics
  for significance of the coefficients, partial F-statistics for significance 
  of the individual input stimuli, and the F-statistic for significance of 
  the overall regression.  Additional output consists of a 3d+time dataset
  containing the estimated system impulse response function.

  File:    3dDeconvolve.c
  Author:  B. Douglas Ward
  Date:    02 September 1998

  Mod:     Minor corrections involving -fdisp option and get_options.
  Date:    29 October 1998

  Mod:     Restructured matrix calculations to improve execution speed.
  Date:    16 December 1998

  Mod:     Minor correction to -stim_label option.
  Date:    17 December 1998

*/


/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1998 by

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

#define PROGRAM_NAME "3dDeconvolve"                  /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "17 December 1998"          /* date of last program mod */

#define MAX_NAME_LENGTH 80              /* max. streng length for file names */
#define MAX_ARRAY_SIZE 1000        /* max. number of time series data points */
#define MAX_XVARS 200                           /* max. number of parameters */
#define MAX_STIMTS 10                 /* max. number of stimulus time series */

#define RA_error DC_error


#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mrilib.h"
#include "matrix.h"


/*---------------------------------------------------------------------------*/
/*
  Include deconvolution analysis software.
*/

#include "Deconvolve.c"


/*---------------------------------------------------------------------------*/

typedef struct DC_options
{ 
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int polort;              /* degree of polynomial for baseline model */
  float rms_min;           /* minimum rms error to reject reduced model */
  float fdisp;             /* minimum f-statistic for display */ 
  char * input_filename;   /* input 3d+time dataset */

  int num_stimts;                     /* number of stimulus time series */
  char * stim_filename[MAX_STIMTS];   /* input stimulus time series */
  char * stim_label[MAX_STIMTS];      /* label for stimulus time series */
  int stim_minlag[MAX_STIMTS];        /* min. time lag for impulse response */
  int stim_maxlag[MAX_STIMTS];        /* max. time lag for impulse response */

  char * bucket_filename;             /* bucket dataset file name */
  char * iresp_filename[MAX_STIMTS];  /* impulse response 3d+time output */
  char * sresp_filename[MAX_STIMTS];  /* std. dev. 3d+time output */
} DC_options;


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void DC_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dDeconvolve help menu.
*/

void display_help_menu()
{
  printf (
"Program to calculate the deconvolution of a measured 3d+time dataset       \n"
"with a specified input stimulus time series.  This program will also       \n"
"perform multiple linear regression using multiple input stimulus time      \n"
"series. Output consists of an AFNI 'bucket' type dataset containing the    \n"
"least squares estimates of the linear regression coefficients, t-statistics\n"
"for significance of the coefficients, partial F-statistics for significance\n"
"of the individual input stimuli, and the F-statistic for significance of   \n"
"the overall regression.  Additional output consists of a 3d+time dataset   \n"
"containing the estimated system impulse response function.                 \n"
    "                                                                       \n"
    "Usage:                                                                 \n"
    "3dDeconvolve                                                           \n"
    "-input fname         fname = filename of 3d+time input dataset         \n"
    "[-nfirst fnum]       fnum = number of first dataset image to use in    \n"
    "                       the deconvolution procedure. (default = 0)      \n"
    "[-nlast  lnum]       lnum = number of last dataset image to use in     \n"
    "                       the deconvolution procedure. (default = last)   \n"
    "[-polort pnum]       pnum = degree of polynomial corresponding to the  \n"
    "                       null hypothesis (pnum = 0, 1, or 2)             \n"
    "                       (default pnum = 1)                              \n"
    "[-rmsmin r]          r = minimum rms error to reject reduced model     \n"
    "[-fdisp fval]        Write (to screen) results for those voxels        \n"
    "                       whose F-statistic is > fval                     \n"
    "                                                                       \n"
    "-num_stimts num      num = number of input stimulus time series        \n"
    "                       (1 <= num <= %d)                                \n"
    "-stim_file k sname   sname = filename of kth time series input stimulus\n"
    "-stim_label k slabel slabel = label for kth time series input stimulus \n"
    "[-stim_minlag k m]   m = minimum time lag for kth input stimulus       \n"
    "                       (default m = 0)                                 \n"
    "[-stim_maxlag k n]   n = maximum time lag for kth input stimulus       \n"
    "                       (default n = 0)                                 \n"
    "                                                                       \n"
    "[-iresp k iprefix]   iprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the kth estimated impulse response \n"
    "                                                                       \n"
    "[-sresp k sprefix]   sprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the standard deviations of the     \n"
    "                       kth impulse response function parameters        \n"
    "                                                                       \n"
    "[-bucket bprefix]  Create one AFNI 'bucket' dataset containing various \n"
    "                   parameters of interest, such as the F-statistic for \n"
    "                   significance of the estimated impulse response.     \n"
    "                   Output 'bucket' dataset is written to prefixname.   \n"
    , MAX_STIMTS);
  
  exit(0);
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


  /*----- initialize default values -----*/
  option_data->NFirst = 0;
  option_data->NLast  = 1000;
  option_data->polort = 1;
  option_data->rms_min = 0.0;
  option_data->fdisp = -1.0;
  option_data->num_stimts = 0;
  for (is = 0;  is < MAX_STIMTS;  is++)
    {
      option_data->stim_minlag[is] = 0;
      option_data->stim_maxlag[is] = 0;
    }


  /*----- initialize character strings -----*/
  option_data->input_filename = NULL;
  option_data->bucket_filename = NULL;
  for (is = 0;  is < MAX_STIMTS;  is++)
    {  
      option_data->stim_label[is] = malloc (sizeof(char)*MAX_NAME_LENGTH);
      MTEST (option_data->stim_label[is]);
      strcpy (option_data->stim_label[is], " ");
      option_data->stim_filename[is] = NULL;
      option_data->iresp_filename[is] = NULL;
      option_data->sresp_filename[is] = NULL;
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
  char message[MAX_NAME_LENGTH];    /* error message */
  int k;                            /* stimulus index */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  
  /*----- initialize the input options -----*/
  initialize_options (option_data); 
  
  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -input filename   -----*/
      if (strncmp(argv[nopt], "-input", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -input ");
	  option_data->input_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->input_filename);
	  strcpy (option_data->input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -nfirst num  -----*/
      if (strncmp(argv[nopt], "-nfirst", 7) == 0)
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
      if (strncmp(argv[nopt], "-nlast", 6) == 0)
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
      if (strncmp(argv[nopt], "-polort", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -polort ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 0) || (ival > 2))
	    DC_error ("illegal argument after -polort ");
	  option_data->polort = ival;
	  nopt++;
	  continue;
	}

      
      /*-----   -rmsmin r  -----*/
      if (strncmp(argv[nopt], "-rmsmin", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -rmsmin ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if (fval < 0.0)
	    DC_error ("illegal argument after -rmsmin ");
	  option_data->rms_min = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -fdisp fval   -----*/
      if (strncmp(argv[nopt], "-fdisp", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -fdisp ");
	  sscanf (argv[nopt], "%f", &fval); 
	  option_data->fdisp = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -num_stimts num  -----*/
      if (strncmp(argv[nopt], "-num_stimts", 11) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -num_stimts ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > MAX_STIMTS))
	    {
	      sprintf (message,  "-num_stimts num   Require: 1 <= num <= %d", 
		       MAX_STIMTS);
	      DC_error (message);
	    }
	  option_data->num_stimts = ival;
	  nopt++;
	  continue;
	}

      
      /*-----   -stim_file k sname   -----*/
      if (strncmp(argv[nopt], "-stim_file", 10) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -stim_file");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_file k sname   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  option_data->stim_filename[k] 
	    = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->stim_filename[k]);
	  strcpy (option_data->stim_filename[k], argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -stim_label k slabel   -----*/
      if (strncmp(argv[nopt], "-stim_label", 10) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -stim_label");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_label k slabel   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  strcpy (option_data->stim_label[k], argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -stim_minlag k lag   -----*/
      if (strncmp(argv[nopt], "-stim_minlag", 12) == 0)
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
      if (strncmp(argv[nopt], "-stim_maxlag", 12) == 0)
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
      

      /*-----   -iresp k iprefix   -----*/
      if (strncmp(argv[nopt], "-iresp", 6) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -iresp");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-iresp k iprefix   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  option_data->iresp_filename[k] 
	    = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->iresp_filename[k]);
	  strcpy (option_data->iresp_filename[k], argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -sresp k sprefix   -----*/
      if (strncmp(argv[nopt], "-sresp", 6) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -sresp");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-sresp k iprefix   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  option_data->sresp_filename[k] 
	    = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->sresp_filename[k]);
	  strcpy (option_data->sresp_filename[k], argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -bucket filename   -----*/
      if (strncmp(argv[nopt], "-bucket", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -bucket ");
	  option_data->bucket_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->bucket_filename);
	  strcpy (option_data->bucket_filename, argv[nopt]);
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
  Read the input data files.
*/

void read_input_data
(
  DC_options * option_data,         /* deconvolution program options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  MRI_IMAGE ** stimulus             /* stimulus time series arrays */
)

{
  char message[MAX_NAME_LENGTH];    /* error message */
  int ts_length;                    /* length of 3d+time dataset */  

  MRI_IMAGE * im, * flim;  /* pointers to image structures 
                              -- used to read 1D ASCII */
  int nt;                  /* number of points in stimulus data file */
  int it;                  /* time point index */
  int num_stimts;          /* number of stimulus time series arrays */
  int is;                  /* stimulus time series index */


  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;


  /*----- Read the input 3d+time dataset -----*/
  *dset_time = THD_open_one_dataset (option_data->input_filename);
  if ((*dset_time) == NULL)  
    { 
      sprintf (message,  "Unable to open data file: %s", 
	       option_data->input_filename);
      DC_error (message);
    }  
  THD_load_datablock ((*dset_time)->dblk, NULL);

  ts_length = DSET_NUM_TIMES (*dset_time);


  /*----- Read the input stimulus time series -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      im = mri_read_ascii (option_data->stim_filename[is]); 

      if (im == NULL)
	{
	  sprintf (message,  "Unable to read stimulus time series file: %s", 
		   option_data->stim_filename[is]);
	  DC_error (message);
	}

      flim = mri_transpose (im);  mri_free(im);
      MTEST (flim);
      nt = flim -> nx;

      if (nt < ts_length)
	DC_error ("Input stimulus time series is too short");  

      stimulus[is] = flim;   flim = NULL;
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
  char message[MAX_NAME_LENGTH];      /* error message */
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
	       "*** Output dataset file %s already exists"
	       "--cannot continue!\a\n",
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
  int is;                         /* stimulus time series index */


  if (option_data->bucket_filename != NULL)   
    check_one_output_file (dset_time, option_data->bucket_filename);
  
  for (is = 0;  is < option_data->num_stimts;  is++)
    {
      if (option_data->iresp_filename[is] != NULL)   
	check_one_output_file (dset_time, option_data->iresp_filename[is]);
  
      if (option_data->sresp_filename[is] != NULL)   
	check_one_output_file (dset_time, option_data->sresp_filename[is]);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs 
(
  DC_options * option_data,       /* deconvolution program options */
  THD_3dim_dataset * dset_time    /* input 3d+time data set */
)

{
  char message[MAX_NAME_LENGTH];  /* error message */
  int is;                         /* stimulus index */
  int num_stimts;                 /* number of stimulus time series */
  int * min_lag;                  /* minimum time delay for impulse response */
  int * max_lag;                  /* maximum time delay for impulse response */
  int q;                   /* number of baseline parameters */
  int p;                   /* total number of parameters */
  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */


  /*----- Initialize local variables -----*/
  nt = DSET_NUM_TIMES (dset_time);
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;

  q = option_data->polort + 1;
  p = q;
  for (is = 0;  is < num_stimts;  is++)
    p += max_lag[is] - min_lag[is] + 1;

  NFirst = option_data->NFirst;
  for (is = 0;  is < num_stimts;  is++)
    if (NFirst < max_lag[is])  
      NFirst = max_lag[is];

  NLast = option_data->NLast;   
  if (NLast > nt-1)  NLast = nt-1;
  N = NLast - NFirst + 1;

  
  /*----- Check number of stimulus time series -----*/
  if ((num_stimts < 1) || (num_stimts > MAX_STIMTS))
    {
      sprintf (message,  "Require: 1 <= num_stimts <= %d",  MAX_STIMTS);
      DC_error (message);
    }


  /*----- Check for sufficient data -----*/
  if (N <= p) 
    {
       sprintf (message,  "Insufficient data for estimating %d parameters", p);
       DC_error (message);
   }

 
  /*----- Check whether time lags are reasonable -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      if (min_lag[is] > max_lag[is])
	{
	  sprintf (message, "min_lag > max_lag  for stimulus %d ", is+1);
          DC_error (message);
	}
    }


  /*----- Check whether any of the output files already exist -----*/
  check_output_files (option_data, dset_time);

}


/*---------------------------------------------------------------------------*/
/*
  Allocate memory for output volumes.
*/

void allocate_memory 
(
  DC_options * option_data,         /* deconvolution algorithm options */
  THD_3dim_dataset * dset,          /* input 3d+time data set */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float *** scoef_vol,      /* array of volumes of parameter std. devs. */
  float *** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float *** fpart_vol,      /* array of volumes of partial F-statistics */
  float ** freg_vol,        /* volume of regression F-statistic */
  float ** rsqr_vol         /* volume of R^2 for the regression model */
)

{
  int ip;                   /* parameter index */
  int p;                    /* total number of parameters */
  int nxyz;                 /* total number of voxels */
  int is;                   /* stimulus index */
  int num_stimts;           /* number of stimulus time series */


  /*----- Initialize local variables -----*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
  num_stimts = option_data->num_stimts;

  p = option_data->polort + 1;
  for (is = 0;  is < num_stimts;  is++)
    p += option_data->stim_maxlag[is] - option_data->stim_minlag[is] + 1;


  /*----- Allocate memory space for volume data -----*/
  *coef_vol  = (float **) malloc (sizeof(float *) * p);   MTEST(*coef_vol);
  *scoef_vol = (float **) malloc (sizeof(float *) * p);   MTEST(*scoef_vol);
  *tcoef_vol = (float **) malloc (sizeof(float *) * p);   MTEST(*tcoef_vol);

  for (ip = 0;  ip < p;  ip++)
    {
      (*coef_vol)[ip]  = (float *) malloc (sizeof(float) * nxyz);
      (*scoef_vol)[ip] = (float *) malloc (sizeof(float) * nxyz);
      (*tcoef_vol)[ip] = (float *) malloc (sizeof(float) * nxyz);
      MTEST((*coef_vol)[ip]);    
      MTEST((*scoef_vol)[ip]);    
      MTEST((*tcoef_vol)[ip]);  
    }

  *fpart_vol = (float **) malloc (sizeof(float *) * num_stimts);
  MTEST(*fpart_vol);
  for (is = 0;  is < num_stimts;  is++)
    {
      (*fpart_vol)[is] = (float *) malloc (sizeof(float) * nxyz);   
      MTEST((*fpart_vol)[is]);
    }

  *freg_vol = (float *) malloc (sizeof(float) * nxyz);   MTEST(*freg_vol);
  *rsqr_vol = (float *) malloc (sizeof(float) * nxyz);   MTEST(*rsqr_vol);

  
}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  DC_options * option_data,         /* deconvolution algorithm options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  MRI_IMAGE ** stimulus,            /* stimulus time series arrays */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float *** scoef_vol,      /* array of volumes of parameter std. devs. */
  float *** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float *** fpart_vol,      /* array of volumes of partial F-statistics */
  float ** freg_vol,        /* volume of regression F-statistic */
  float ** rsqr_vol         /* volume of R^2 for the regression model */
)
     
{

  /*----- Get command line inputs -----*/
  get_options (argc, argv, option_data);


  /*----- Read input data -----*/
  read_input_data (option_data, dset_time, stimulus);

 
  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (option_data, *dset_time);
  

  /*----- Allocate memory for output volumes -----*/
  allocate_memory (option_data, *dset_time, 
		   coef_vol, scoef_vol, tcoef_vol, 
		   fpart_vol, freg_vol, rsqr_vol);

}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array 
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array         /* input time series data for voxel #iv */
)

{
  int ts_length;           /* length of input 3d+time data set */
  int it;                  /* time index */
  int dtyp;                /* data type of input dataset */

  
  dtyp = DSET_BRICK_TYPE (dset_time,0);
  ts_length = DSET_NUM_TIMES (dset_time);


  switch( dtyp )
    {
    case MRI_short:
      {
	for (it = 0;  it < ts_length;  it++)
	  {
	    short * dar = (short *) DSET_ARRAY(dset_time,it);
	    ts_array[it] = (float) dar[iv] ;
	  }
      }
      break ;
      
    case MRI_float:
      {
	for (it = 0;  it < ts_length;  it++)
	  {
	    float * dar = (float *) DSET_ARRAY (dset_time,it);
	    ts_array[it] = (float) dar[iv] ;
	  }
      }
      break ;
      
    case MRI_byte:
      {
	for (it = 0;  it < ts_length;  it++)
	  {
	    byte * dar = (byte *) DSET_ARRAY (dset_time,it);
	    ts_array[it] = (float) dar[iv] ;
	  }
      }
      break ;
    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Save results for this voxel.
*/

void save_voxel 
(
  DC_options * option_data,    /* deconvolution algorithm options */
  int iv,                      /* current voxel index */      
  vector coef,                 /* regression parameters */
  vector scoef,                /* regression parameter standard deviations */
  vector tcoef,                /* t-statistics for regression parameters */
  float * fpart,               /* array of partial F-statistics */ 
  float freg,                  /* regression F-statistic */
  float rsqr,                  /* coeff. of multiple determination R^2  */

  float ** coef_vol,       /* array of volumes of signal model parameters */
  float ** scoef_vol,      /* array of volumes of parameter std. devs. */
  float ** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float ** fpart_vol,      /* array of volumes of partial F-statistics */
  float * freg_vol,        /* volume of regression F-statistic */
  float * rsqr_vol         /* volume of R^2 for the regression model */
)

{
  int ip;                  /* parameter index */ 
  int p;                   /* total number of parameters */
  int is;                  /* stimulus time series index */
  int num_stimts;          /* number of stimulus time series */


  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;
  p = option_data->polort + 1;
  for (is = 0;  is < num_stimts;  is++)
    p += option_data->stim_maxlag[is] - option_data->stim_minlag[is] + 1;


  /*----- Saved regression coefficients and t-statistics -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      coef_vol[ip][iv]  = coef.elts[ip];
      scoef_vol[ip][iv] = scoef.elts[ip];
      tcoef_vol[ip][iv] = tcoef.elts[ip];
    }


  /*----- Save partial F-statistics -----*/
  for (is = 0;  is < num_stimts;  is++)
    fpart_vol[is][iv] = fpart[is];


  /*----- Save regression F-statistic -----*/
  freg_vol[iv] = freg;


  /*----- Save R^2 values -----*/
  rsqr_vol[iv] = rsqr;

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the impulse response function and associated statistics.
*/

void calculate_results 
(
  DC_options * option_data,         /* deconvolution algorithm options */
  THD_3dim_dataset * dset,          /* input 3d+time data set */
  MRI_IMAGE ** stimulus,            /* stimulus time series arrays */

  float ** coef_vol,       /* array of volumes of signal model parameters */
  float ** scoef_vol,      /* array of volumes of parameter std. devs. */
  float ** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float ** fpart_vol,      /* array of volumes of partial F-statistics */
  float * freg_vol,        /* volume of regression F-statistic */
  float * rsqr_vol         /* volume of R^2 for the regression model */
)
  
{
  float ts_array[MAX_ARRAY_SIZE];   /* array of measured data for one voxel */

  int p;                      /* number of parameters in the full model */
  int q;                      /* number of parameters in the baseline model */
  int m;                      /* parameter index */
  int n;                      /* data point index */

  vector coef;                /* regression parameters */
  vector scoef;               /* std. devs. for regression parameters */
  vector tcoef;               /* t-statistics for regression parameters */
  float fpart[MAX_STIMTS];    /* partial F-statistics for the stimuli */
  float freg;                 /* regression F-statistic */
  float rsqr;                 /* coeff. of multiple determination R^2  */

  matrix xdata;               /* independent variable matrix */
  matrix x_full;              /* extracted X matrix    for full model */
  matrix xtxinv_full;         /* matrix:  1/(X'X)      for full model */
  matrix xtxinvxt_full;       /* matrix:  (1/(X'X))X'  for full model */
  matrix x_base;              /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base;       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix x_rdcd[MAX_STIMTS];  /* extracted X matrices  for reduced models */
  matrix xtxinvxt_rdcd[MAX_STIMTS];     
                              /* matrix:  (1/(X'X))X'  for reduced models */
  vector y;                   /* vector of measured data */       

  int ixyz;                   /* voxel index */
  int nxyz;                   /* number of voxels per dataset */

  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */

  int num_stimts;          /* number of stimulus time series */
  int * min_lag;           /* minimum time delay for impulse response */ 
  int * max_lag;           /* maximum time delay for impulse response */ 
  char ** stim_label;      /* label for stimulus time series */

  int i;                   /* data point index */
  int is;                  /* stimulus index */
  float rms_min;           /* minimum variation in data to fit full model */
  char * label;            /* string containing stat. summary of results */


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&xdata);
  matrix_initialize (&x_full);
  matrix_initialize (&xtxinv_full);
  matrix_initialize (&xtxinvxt_full);
  matrix_initialize (&x_base);
  matrix_initialize (&xtxinvxt_base);
  for (is =0;  is < MAX_STIMTS;  is++)
    {
      matrix_initialize (&x_rdcd[is]);
      matrix_initialize (&xtxinvxt_rdcd[is]);
    }
  vector_initialize (&coef);
  vector_initialize (&scoef);
  vector_initialize (&tcoef);
  vector_initialize (&y);


  /*----- Initialize local variables -----*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;       
  nt = DSET_NUM_TIMES (dset);
  rms_min = option_data->rms_min;
  num_stimts = option_data->num_stimts;
  stim_label = option_data->stim_label;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;

  q = option_data->polort + 1;
  p = q;
  for (is = 0;  is < num_stimts;  is++)
    p += max_lag[is] - min_lag[is] + 1;

  NFirst = option_data->NFirst;
  for (is = 0;  is < num_stimts;  is++)
    if (NFirst < max_lag[is])  
      NFirst = max_lag[is];

  NLast = option_data->NLast;   
  if (NLast > nt-1)  NLast = nt-1;
  N = NLast - NFirst + 1;


  /*----- Initialize the independent variable matrix -----*/
  init_indep_var_matrix (p, q, NFirst, N, num_stimts,
			 stimulus, min_lag, max_lag, &xdata);


  /*----- Print the X matrix if screen output is requested -----*/
  if (option_data->fdisp >= 0)
    {
      printf ("\n");
      printf ("\nX matrix: \n");
      matrix_print (xdata);
    }
  

  /*----- Initialization for the regression analysis -----*/
  init_regression_analysis (p, q, num_stimts, min_lag, max_lag, xdata, 
			    &x_full, &xtxinv_full, &xtxinvxt_full, 
			    &x_base, &xtxinvxt_base, x_rdcd, xtxinvxt_rdcd);


  vector_create (N, &y);

  
  /*----- Loop over all voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {

      /*----- Extract Y-data for this voxel -----*/
      extract_ts_array (dset, ixyz, ts_array);

      for (i = 0;  i < N;  i++)
	y.elts[i] = ts_array[i+NFirst];


      /*----- Perform the regression analysis for this voxel-----*/
      regression_analysis (N, p, q, num_stimts, min_lag, max_lag,
			   x_full, xtxinv_full, xtxinvxt_full, x_base,
			   xtxinvxt_base, x_rdcd, xtxinvxt_rdcd, y, rms_min, 
			   &coef, &scoef, &tcoef, fpart, &freg, &rsqr);


      /*----- Save results for this voxel -----*/
      save_voxel (option_data, ixyz, coef, scoef, tcoef, fpart, freg, rsqr,
		  coef_vol, scoef_vol, tcoef_vol, 
		  fpart_vol, freg_vol, rsqr_vol);


      /*----- Report results for this voxel -----*/
      if ((freg > option_data->fdisp) && (option_data->fdisp >= 0.0))
	{
	  printf ("\nResults for Voxel #%d: \n", ixyz);
	  report_results (q, num_stimts, stim_label, min_lag, max_lag,
			  coef, tcoef, fpart, freg, rsqr, &label);
	  printf ("%s \n", label);
	}
      
    }  /*----- Loop over voxels -----*/
  

  /*----- Dispose of matrices -----*/
  vector_destroy (&y);
  vector_destroy (&tcoef);
  vector_destroy (&scoef);
  vector_destroy (&coef);
  for (is = 0;  is < MAX_STIMTS;  is++)
    {
      matrix_destroy (&xtxinvxt_rdcd[is]);
      matrix_destroy (&x_rdcd[is]);
    } 
  matrix_destroy (&xtxinvxt_base);
  matrix_destroy (&x_base);
  matrix_destroy (&xtxinvxt_full);
  matrix_destroy (&xtxinv_full); 
  matrix_destroy (&x_full); 
  matrix_destroy (&xdata);
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
  Routine to write one AFNI 3d+time data set. 
*/


void write_ts_array 
(
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
  float fbuf[1000];         /* float buffer */
  float * volume;           /* pointer to volume of data */
  

  /*----- Initialize local variables -----*/
  input_filename = option_data->input_filename;
  dset = THD_open_one_dataset (input_filename);
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
  for (ib = 0;  ib < 1000;  ib++)
    fbuf[ib] = 0.0;

 
  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * ts_length);
  MTEST (bar);
  
  
  /*-- make an empty copy of the prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (dset);
  THD_delete_3dim_dataset (dset, False);  dset = NULL ;
  

  ierror = EDIT_dset_items (new_dset,
			    ADN_prefix,      output_filename,
			    ADN_label1,      output_filename,
			    ADN_self_name,   output_filename,
			    ADN_malloc_type, DATABLOCK_MEM_MALLOC,  
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
    }


  /*----- write afni data set -----*/
  printf ("--- Writing 3d+time dataset into %s\n",
	  new_dset->dblk->diskptr->header_name);

  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset (NULL, NULL, new_dset, True);
  

  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (new_dset, False);   new_dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one bucket data set.
*/

void write_bucket_data 
(
  DC_options * option_data,         /* deconvolution algorithm options */

  float ** coef_vol,       /* array of volumes of signal model parameters */
  float ** tcoef_vol,      /* array of volumes of signal model t-statistics */
  float ** fpart_vol,      /* array of volumes of partial F-statistics */
  float * freg_vol,        /* volume of regression F-statistic */
  float * rsqr_vol         /* volume of R^2 for the regression model */
)

{
  const float EPSILON = 1.0e-10;

  THD_3dim_dataset * old_dset = NULL;      /* prototype dataset */
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  char output_prefix[MAX_NAME_LENGTH];     /* prefix name for bucket dataset */
  char output_session[MAX_NAME_LENGTH];    /* directory for bucket dataset */
  int nbricks;              /* number of sub-bricks in bucket dataset */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */
  float factor;             /* factor is new scale factor for sub-brick #ib */

  int brick_type;           /* indicates statistical type of sub-brick */
  int brick_coef;           /* regression coefficient index for sub-brick */
  char brick_label[MAX_NAME_LENGTH]; /* character string label for sub-brick */

  int ierror;               /* number of errors in editing data */
  float * volume;           /* volume of floating point data */

  int N;                    /* number of usable data points */
  int p;                    /* number of parameters in the full model */
  int q;                    /* number of parameters in the rdcd model */
  int num_stimts;           /* number of stimulus time series */
  int istim;                /* stimulus index */
  int nxyz;                 /* total number of voxels */
  int nt;                   /* number of images in input 3d+time dataset */
  int NFirst;               /* first image from input 3d+time dataset to use */
  int NLast;                /* last image from input 3d+time dataset to use */
  int ilag;                 /* lag index */
  int icoef;                /* coefficient index */
  int ibrick, ibrickstim;   /* sub-brick indices */
  char label[MAX_NAME_LENGTH];   /* sub-brick lable */


  /*----- read prototype dataset -----*/
  old_dset = THD_open_one_dataset (option_data->input_filename);

    
  /*----- Initialize local variables -----*/
  nxyz = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;  
  num_stimts = option_data->num_stimts;
  nt = DSET_NUM_TIMES (old_dset);

  q = option_data->polort + 1;
  p = q;
  for (istim = 0;  istim < num_stimts;  istim++)
    p += option_data->stim_maxlag[istim] - option_data->stim_minlag[istim] + 1;

  NFirst = option_data->NFirst;
  for (istim = 0;  istim < num_stimts;  istim++)
    if (NFirst < option_data->stim_maxlag[istim])  
      NFirst = option_data->stim_maxlag[istim];

  NLast = option_data->NLast;   
  if (NLast > nt-1)  NLast = nt-1;
  N = NLast - NFirst + 1;


  nbricks = 2*p + num_stimts + 2;
  strcpy (output_prefix, option_data->bucket_filename);
  strcpy (output_session, "./");
  
 
  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * nbricks);
  MTEST (bar);
  

  /*-- make an empty copy of prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (old_dset);

  
  /*----- delete prototype dataset -----*/ 
  THD_delete_3dim_dataset( old_dset , False );  old_dset = NULL ;


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
	      "*** %d errors in attempting to create bucket dataset!\n", 
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
  

  /*----- loop over new sub-brick index, attach data array with 
          EDIT_substitute_brick then put some strings into the labels and 
          keywords, and modify the sub-brick scaling factor -----*/
  istim = -1;  
  ibrickstim = 0;
  icoef = 0;
  for (ibrick = 0;  ibrick < nbricks;  ibrick++)
    {
                                          /*----- Baseline statistics -----*/
      if (istim < 0)                     
	{                                 
	  strcpy (label, "Base");
                                          /*----- Baseline coefficient -----*/
	  if (ibrick % 2 == 0)            
	    {                             
	      brick_type = FUNC_FIM_TYPE;
	      sprintf (brick_label, "%s t^%d Coef", label, icoef);
	      volume = coef_vol[icoef];
	    }
                                          /*----- Baseline t-stat -----*/
	  else                            
	    {                             
	      brick_type = FUNC_TT_TYPE;
	      sprintf (brick_label, "%s t^%d t-st", label, icoef);
	      volume = tcoef_vol[icoef];
	      icoef++;
	      if (icoef == q)
		{
		  istim++;
		  ibrickstim = ibrick+1;
		}
	    }	  
       	}
                                          /*----- Stimulus statistics -----*/
      else if (istim < num_stimts)        
	{                                 
	  strcpy (label, option_data->stim_label[istim]);
	  ilag = option_data->stim_minlag[istim]  + (ibrick-ibrickstim)/2;

	  if (ilag <= option_data->stim_maxlag[istim])
	    {                             
                                          /*----- Stimulus coefficient -----*/
	      if ((ibrick-ibrickstim) % 2 == 0)      
		{
		  brick_type = FUNC_FIM_TYPE;
		  sprintf (brick_label, "%s[%d] Coef", label, ilag);
		  volume = coef_vol[icoef];		  
		}
                                          /*----- Stimulus t-stat -----*/
	      else                        
		{
		  brick_type = FUNC_TT_TYPE;
		  sprintf (brick_label, "%s[%d] t-st", label, ilag);
		  volume = tcoef_vol[icoef];
		  icoef++;
		}
	    }
                                          /*----- Stimulus F-stat -----*/
	  else                            
	    {
	      brick_type = FUNC_FT_TYPE;
	      sprintf (brick_label, "%s F-stat", label);
	      volume = fpart_vol[istim];
	      istim++;
	      ibrickstim = ibrick+1;
	    }
	}
                                          /*----- Regression statistics -----*/
      else                                
	{
	  istim++;
	  if (ibrick < nbricks-1)
	    {
	      brick_type = FUNC_THR_TYPE;
	      sprintf (brick_label, "Full R^2");
	      volume = rsqr_vol;
	    }
	  else
	    {
	      brick_type = FUNC_FT_TYPE;
	      sprintf (brick_label, "Full F-stat");
	      volume = freg_vol;
	    }
	}

      
      /*----- allocate memory for output sub-brick -----*/
      bar[ibrick]  = (short *) malloc (sizeof(short) * nxyz);
      MTEST (bar[ibrick]);
      factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
					  MRI_short, bar[ibrick]);

      if (factor < EPSILON)  factor = 0.0;
      else factor = 1.0 / factor;


      /*----- edit the sub-brick -----*/
      EDIT_BRICK_LABEL (new_dset, ibrick, brick_label);
      EDIT_BRICK_FACTOR (new_dset, ibrick, factor);

      if (brick_type == FUNC_TT_TYPE)
	EDIT_BRICK_TO_FITT (new_dset, ibrick, N-p);
      else if (brick_type == FUNC_FT_TYPE)
	if (istim <= num_stimts)
	  EDIT_BRICK_TO_FIFT (new_dset, ibrick,
			      option_data->stim_maxlag[istim-1]
			      - option_data->stim_minlag[istim-1] + 1, N-p);
	else
	  EDIT_BRICK_TO_FIFT (new_dset, ibrick, p-q, N-p);
	  

      
      /*----- attach bar[ib] to be sub-brick #ibrick -----*/
      EDIT_substitute_brick (new_dset, ibrick, MRI_short, bar[ibrick]);

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
  Write out the user requested output files.
*/

void output_results
(
  DC_options * option_data,         /* deconvolution algorithm options */
  THD_3dim_dataset * dset,          /* input 3d+time data set */

  float ** coef_vol,       /* array of volumes of signal model parameters */
  float ** scoef_vol,      /* array of volumes of parameter std. devs. */
  float ** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float ** fpart_vol,      /* array of volumes of partial F-statistics */
  float * freg_vol,        /* volume of regression F-statistic */
  float * rsqr_vol         /* volume of R^2 for the regression model */
)

{
  int q;                   /* number of parameters in baseline model */
  int num_stimts;          /* number of stimulus time series */
  int * min_lag;           /* minimum time delay for impulse response */ 
  int * max_lag;           /* maximum time delay for impulse response */ 
  int ib;                  /* sub-brick index */
  int is;                  /* stimulus index */
  int ts_length;           /* length of impulse reponse function */


  /*----- Initialize local variables -----*/
  q = option_data->polort + 1;
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;


  /*----- Write the bucket dataset -----*/
  if (option_data->bucket_filename != NULL)
    write_bucket_data (option_data, 
		       coef_vol, tcoef_vol, fpart_vol, freg_vol, rsqr_vol);


  /*----- Write the impulse response function 3d+time dataset -----*/
  ib = q;
  for (is = 0;  is < num_stimts;  is++)
    {
      ts_length = max_lag[is] - min_lag[is] + 1;
      if (option_data->iresp_filename[is] != NULL)
	write_ts_array (option_data, ts_length, coef_vol+ib, 
			option_data->iresp_filename[is]);
      ib += ts_length;
    }
 

  /*----- Write the standard deviation 3d+time dataset -----*/
  ib = q;
  for (is = 0;  is < num_stimts;  is++)
    {
      ts_length = max_lag[is] - min_lag[is] + 1;
      if (option_data->sresp_filename[is] != NULL)
	write_ts_array (option_data, ts_length, scoef_vol+ib, 
			option_data->sresp_filename[is]);
      ib += ts_length;
    }
}


/*---------------------------------------------------------------------------*/

void terminate_program
(
  DC_options ** option_data,         /* deconvolution algorithm options */
  THD_3dim_dataset ** dset,          /* input 3d+time data set */
  MRI_IMAGE ** stimulus,             /* stimulus time series arrays */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float *** scoef_vol,      /* array of volumes of parameter std. devs. */
  float *** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float *** fpart_vol,      /* array of volumes of partial F-statistics */
  float ** freg_vol,        /* volume of regression F-statistic */
  float ** rsqr_vol         /* volume of R^2 for the regression model */
)

{
  int p;                    /* number of parameters in full model */
  int q;                    /* number of parameters in baseline model */
  int num_stimts;           /* number of stimulus time series */
  int ip;                   /* parameter index */
  int is;                   /* stimulus index */


  /*----- Initialize local variables -----*/
  num_stimts = (*option_data)->num_stimts;
  q = (*option_data)->polort + 1;
  p = q;
  for (is = 0;  is < num_stimts;  is++)
    p += (*option_data)->stim_maxlag[is] 
      - (*option_data)->stim_minlag[is] + 1;


  /*----- Deallocate memory for option data -----*/   
  free (*option_data);  *option_data = NULL;


  /*----- Deallocate memory for dataset -----*/   
  THD_delete_3dim_dataset (*dset, False);  *dset = NULL;


  /*----- Deallocate memory for stimulus time series -----*/
  for (is = 0;  is < num_stimts;  is++)
    mri_free (stimulus[is]); 


  /*----- Deallocate space for volume data -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      if ((*coef_vol)[ip] != NULL)
	{ free ((*coef_vol)[ip]);   (*coef_vol)[ip] = NULL; }
      if ((*scoef_vol)[ip] != NULL)      
	{ free ((*scoef_vol)[ip]);  (*scoef_vol)[ip] = NULL; }
      if ((*tcoef_vol)[ip] != NULL)      
	{ free ((*tcoef_vol)[ip]);  (*tcoef_vol)[ip] = NULL; }
    }

  for (is = 0;  is < num_stimts;  is++)
    if ((*fpart_vol)[is] != NULL)    
      { free ((*fpart_vol)[is]);    (*fpart_vol)[is] = NULL; }

  if (*coef_vol  != NULL)    { free (*coef_vol);   *coef_vol = NULL; }
  if (*scoef_vol != NULL)    { free (*scoef_vol);  *scoef_vol = NULL; }
  if (*tcoef_vol != NULL)    { free (*tcoef_vol);  *tcoef_vol = NULL; }
  if (*fpart_vol != NULL)    { free (*fpart_vol);  *fpart_vol = NULL; }
  if (*freg_vol  != NULL)    { free (*freg_vol);   *freg_vol = NULL; }
  if (*rsqr_vol  != NULL)    { free (*rsqr_vol);   *rsqr_vol = NULL; } 


}


/*---------------------------------------------------------------------------*/

void main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  DC_options * option_data;               /* deconvolution algorithm options */
  THD_3dim_dataset * dset_time = NULL;    /* input 3d+time data set */
  MRI_IMAGE * stimulus[MAX_STIMTS];       /* stimulus time series arrays */

  float ** coef_vol = NULL;   /* array of volumes for model parameters */
  float ** scoef_vol = NULL;  /* array of volumes for parameter std. devs. */
  float ** tcoef_vol = NULL;  /* array of volumes for parameter t-statistics */
  float ** fpart_vol = NULL;  /* array of volumes of partial F-statistics */
  float * freg_vol = NULL;    /* volume of regression F-statistic */
  float * rsqr_vol = NULL;    /* volume of R^2 for the regression model */

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

  
  /*----- Allocate memory -----*/
  option_data = (DC_options *) malloc (sizeof(DC_options));

   
  /*----- Program initialization -----*/
  initialize_program (argc, argv, option_data, &dset_time, stimulus,
		      &coef_vol, &scoef_vol, &tcoef_vol, 
		      &fpart_vol, &freg_vol, &rsqr_vol);


  /*----- Perform deconvolution -----*/
  calculate_results (option_data, dset_time, stimulus,
		     coef_vol, scoef_vol, tcoef_vol,
		     fpart_vol, freg_vol, rsqr_vol);


  /*----- Write requested output files -----*/
  output_results (option_data, dset_time,
		  coef_vol, scoef_vol, tcoef_vol, 
		  fpart_vol, freg_vol, rsqr_vol);

  
  /*----- Terminate program -----*/
  terminate_program (&option_data, &dset_time, stimulus,
		     &coef_vol, &scoef_vol, &tcoef_vol,
		     &fpart_vol, &freg_vol, &rsqr_vol);

}


















