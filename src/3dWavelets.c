/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 2000-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Program to perform wavelet analysis of an FMRI 3d+time dataset.

  File:    3dWavelets.c
  Author:  B. Douglas Ward
  Date:    28 March 2000

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
  Date:    02 December 2002

  Mod:     Added -datum option.
  Date:    07 February 2006 [rickr]
*/

/*---------------------------------------------------------------------------*/
/*
  Software identification.
*/

#define PROGRAM_NAME "3dWavelets"                    /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "28 March 2000"   /* date of initial program release */
#define PROGRAM_LATEST "02 December 2002"   /* date of last program revision */

/*---------------------------------------------------------------------------*/
/*
  Include header files and source code.
*/

#include "mrilib.h"
#include "Wavelets.h"
#include "Wavelets.c"


/*---------------------------------------------------------------------------*/
/*
  Global constants. 
*/

#define MAX_NAME_LENGTH THD_MAX_NAME    /* max. string length for file names */
#define MAX_FILTERS 20               /* max. number of filter specifications */


/*---------------------------------------------------------------------------*/
/*
  Data structure for operator inputs.
*/

typedef struct WA_options
{ 
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable data points from input data */
  int f;                   /* number of parameters removed by the filter */
  int p;                   /* number of parameters in the full model */
  int q;                   /* number of parameters in the baseline model */

  float fdisp;             /* minimum F-statistic for display */ 
  char * input_filename;   /* input 3d+time dataset */
  char * mask_filename;    /* input mask dataset */
  char * input1D_filename; /* input fMRI measurement time series */

  int wavelet_type;        /* indicates type of wavelet */        

  int num_stop_filters;             /* number of wavelet stop filters */
  int stop_band[MAX_FILTERS];       /* wavelet filter stop band */ 
  int stop_mintr[MAX_FILTERS];      /* min. time for stop band */
  int stop_maxtr[MAX_FILTERS];      /* max. time for stop band */
  float * stop_filter;              /* select wavelet coefs. to stop */

  int num_base_filters;             /* number of wavelet base filters */
  int base_band[MAX_FILTERS];       /* wavelet filter base band */ 
  int base_mintr[MAX_FILTERS];      /* min. time for base band */
  int base_maxtr[MAX_FILTERS];      /* max. time for base band */
  float * base_filter;              /* select wavelet coefs. for baseline */

  int num_sgnl_filters;             /* number of wavelet signal filters */
  int sgnl_band[MAX_FILTERS];       /* wavelet filter signal band */ 
  int sgnl_mintr[MAX_FILTERS];      /* min. time for signal band */
  int sgnl_maxtr[MAX_FILTERS];      /* max. time for signal band */
  float * sgnl_filter;              /* select wavelet coefs. for signal */

  char * bucket_filename;   /* bucket dataset file name */
  char * coefts_filename;   /* forward wavelet transform coefficients output */
  char * fitts_filename;    /* full model time series 3d+time output */
  char * sgnlts_filename;   /* signal model time series 3d+time output */
  char * errts_filename;    /* residual error time series 3d+time output */

  int fout;                 /* flag to output F-statistics */
  int rout;                 /* flag to output R^2 statistics */
  int cout;                 /* flag to output wavelet coefficients */
  int vout;                 /* flag to output variance map */
  int stat_first;           /* flag to output full model stats first */

  int datum;                /* data type for output bucket and time_series */
} WA_options;


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dWavelets help menu.
*/

void display_help_menu()
{
  printf (
    "Program to perform wavelet analysis of an FMRI 3d+time dataset.        \n"
    "                                                                       \n"
    "Usage:                                                                 \n"
    "3dWavelets                                                             \n"
    "-type wname          wname = name of wavelet to use for the analysis   \n"
    "                     At present, there are only two choices for wname: \n"
    "                        Haar  -->  Haar wavelets                       \n"
    "                        Daub  -->  Daubechies wavelets                 \n"
    "-input fname         fname = filename of 3d+time input dataset         \n"
    "[-input1D dname]     dname = filename of single (fMRI) .1D time series \n"
    "[-mask mname]        mname = filename of 3d mask dataset               \n"
    "[-nfirst fnum]       fnum = number of first dataset image to use in    \n"
    "                       the wavelet analysis. (default = 0)             \n"
    "[-nlast  lnum]       lnum = number of last dataset image to use in     \n"
    "                       the wavelet analysis. (default = last)          \n"
    "[-fdisp fval]        Write (to screen) results for those voxels        \n"
    "                       whose F-statistic is >= fval                    \n"
    "                                                                       \n"
    "Filter options:                                                        \n"
    "                                                                       \n"
    "[-filt_stop band mintr maxtr] Specify wavelet coefs. to set to zero    \n"
    "[-filt_base band mintr maxtr] Specify wavelet coefs. for baseline model\n"
    "[-filt_sgnl band mintr maxtr] Specify wavelet coefs. for signal model  \n"
    "     where  band  = frequency band                                     \n"
    "            mintr = min. value for time window (in TR)                 \n"
    "            maxtr = max. value for time window (in TR)                 \n"
    "                                                                       \n"
    "Output options:                                                        \n"
    "                                                                       \n"
    "[-datum DTYPE]      Coerce the output data to be stored as type DTYPE, \n"
    "                       which may be short or float. (default = short)  \n"
    "                                                                       \n"
    "[-coefts cprefix]   cprefix = prefix of 3d+time output dataset which   \n"
    "                       will contain the forward wavelet transform      \n"
    "                       coefficients                                    \n"
    "                                                                       \n"
    "[-fitts  fprefix]   fprefix = prefix of 3d+time output dataset which   \n"
    "                       will contain the full model time series fit     \n"
    "                       to the input data                               \n"
    "                                                                       \n"
    "[-sgnlts sprefix]   sprefix = prefix of 3d+time output dataset which   \n"
    "                       will contain the signal model time series fit   \n"
    "                       to the input data                               \n"
    "                                                                       \n"
    "[-errts  eprefix]   eprefix = prefix of 3d+time output dataset which   \n"
    "                       will contain the residual error time series     \n"
    "                       from the full model fit to the input data       \n"
    "                                                                       \n"
    "The following options control the contents of the bucket dataset:      \n"
    "                                                                       \n"
    "[-fout]            Flag to output the F-statistics                     \n"
    "[-rout]            Flag to output the R^2 statistics                   \n"
    "[-cout]            Flag to output the full model wavelet coefficients  \n"
    "[-vout]            Flag to output the sample variance (MSE) map        \n"
    "                                                                       \n"
    "[-stat_first]      Flag to specify that the full model statistics will \n"
    "                     appear prior to the wavelet coefficients in the   \n"
    "                     bucket dataset output                             \n"
    "                                                                       \n"
    "[-bucket bprefix]  bprefix = prefix of AFNI 'bucket' dataset containing\n"
    "                     parameters of interest, such as the F-statistic   \n"
    "                     for significance of the wavelet signal model.     \n"
    );
  
  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  WA_options * option_data    /* wavelet analysis program options */
)
 
{
  int is;                     /* filter index */


  /*----- Initialize default values -----*/
  option_data->NFirst = 0;
  option_data->NLast  = 32767;
  option_data->N      = 0;
  option_data->f      = 0;
  option_data->p      = 0;
  option_data->q      = 0;
  option_data->fdisp = -1.0;
  option_data->wavelet_type = 0;


  /*----- Initialize filter specifications -----*/
  option_data->num_stop_filters = 0;
  option_data->num_base_filters = 0;
  option_data->num_sgnl_filters = 0;
  for (is = 0;  is < MAX_FILTERS;  is++)
    {
      option_data->stop_band[is] = 0;
      option_data->stop_mintr[is] = 0;
      option_data->stop_maxtr[is] = 0;
      option_data->base_band[is] = 0;
      option_data->base_mintr[is] = 0;
      option_data->base_maxtr[is] = 0;
      option_data->sgnl_band[is] = 0;
      option_data->sgnl_mintr[is] = 0;
      option_data->sgnl_maxtr[is] = 0;
    }
  option_data->stop_filter = NULL;
  option_data->base_filter = NULL;
  option_data->sgnl_filter = NULL;


  /*----- Initialize output flags -----*/
  option_data->fout = 0;
  option_data->rout = 0;
  option_data->cout = 0;
  option_data->vout = 0;
  option_data->stat_first = 0;
  option_data->datum = MRI_short;


  /*----- Initialize file name character strings -----*/
  option_data->input_filename = NULL;
  option_data->mask_filename = NULL;  
  option_data->input1D_filename = NULL;
  option_data->bucket_filename = NULL;
  option_data->coefts_filename = NULL;
  option_data->fitts_filename = NULL;
  option_data->sgnlts_filename = NULL;
  option_data->errts_filename = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */ 
  WA_options * option_data         /* wavelet program options */
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival;                         /* integer input */
  float fval;                       /* float input */
  char message[MAX_NAME_LENGTH];    /* error message */
  int ifilter;                      /* filter index */


  /*-- addto the arglist, if user wants to --*/
   machdep() ; 
  { int new_argc ; char ** new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  

  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  


  /*----- Add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 

  
  /*----- initialize the input options -----*/
  initialize_options (option_data); 

  
  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -type wname   -----*/
      if (strncmp(argv[nopt], "-type", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -type ");
	  for (ival = 0;  ival < MAX_WAVELET_TYPE;  ival++)
	    if (strncmp(argv[nopt], WAVELET_TYPE_name[ival], 4) == 0)  break;
	  if (ival >= MAX_WAVELET_TYPE)
	    {
	      sprintf(message,"Unrecognized wavelet type: %s\n", argv[nopt]);
	      WA_error (message);
	    }
	  option_data->wavelet_type = ival;
	  nopt++;
	  continue;
	}
      

      /*-----   -input filename   -----*/
      if (strncmp(argv[nopt], "-input", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -input ");
	  option_data->input_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->input_filename);
	  strcpy (option_data->input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -input1D filename   -----*/
      if (strncmp(argv[nopt], "-input1D", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -input1D ");
	  option_data->input1D_filename = 
	    malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->input1D_filename);
	  strcpy (option_data->input1D_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -mask filename   -----*/
      if (strncmp(argv[nopt], "-mask", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -mask ");
	  option_data->mask_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->mask_filename);
	  strcpy (option_data->mask_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -nfirst num  -----*/
      if (strncmp(argv[nopt], "-nfirst", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -nfirst ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("illegal argument after -nfirst ");
	  option_data->NFirst = ival;
	  nopt++;
	  continue;
	}


      /*-----   -nlast num  -----*/
      if (strncmp(argv[nopt], "-nlast", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -nlast ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("illegal argument after -nlast ");
	  option_data->NLast = ival;
	  nopt++;
	  continue;
	}


      /*-----   -fdisp fval   -----*/
      if (strncmp(argv[nopt], "-fdisp", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -fdisp ");
	  sscanf (argv[nopt], "%f", &fval); 
	  option_data->fdisp = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -filt_stop band mintr maxtr   -----*/
      if (strncmp(argv[nopt], "-filt_stop", 10) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  
	    WA_error ("need 3 arguments after -filt_stop");

	  ifilter = option_data->num_stop_filters;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < -1)
	    WA_error ("-filt_stop band mintr maxtr   Require: -1 <= band");
	  option_data->stop_band[ifilter] = ival;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("-filt_stop band mintr maxtr   Require: 0 <= mintr");
	  option_data->stop_mintr[ifilter] = ival;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("-filt_stop band mintr maxtr   Require: 0 <= maxtr");
	  option_data->stop_maxtr[ifilter] = ival;
	  nopt++;

	  option_data->num_stop_filters++;
	  continue;
	}
      

      /*-----   -filt_base band mintr maxtr   -----*/
      if (strncmp(argv[nopt], "-filt_base", 10) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  
	    WA_error ("need 3 arguments after -filt_base");

	  ifilter = option_data->num_base_filters;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < -1)
	    WA_error ("-filt_base band mintr maxtr   Require: -1 <= band");
	  option_data->base_band[ifilter] = ival;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("-filt_base band mintr maxtr   Require: 0 <= mintr");
	  option_data->base_mintr[ifilter] = ival;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("-filt_base band mintr maxtr   Require: 0 <= maxtr");
	  option_data->base_maxtr[ifilter] = ival;
	  nopt++;

	  option_data->num_base_filters++;
	  continue;
	}
      

      /*-----   -filt_sgnl band mintr maxtr   -----*/
      if (strncmp(argv[nopt], "-filt_sgnl", 10) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  
	    WA_error ("need 3 arguments after -filt_sgnl");

	  ifilter = option_data->num_sgnl_filters;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < -1)
	    WA_error ("-filt_sgnl band mintr maxtr   Require: -1 <= band");
	  option_data->sgnl_band[ifilter] = ival;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("-filt_sgnl band mintr maxtr   Require: 0 <= mintr");
	  option_data->sgnl_mintr[ifilter] = ival;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    WA_error ("-filt_sgnl band mintr maxtr   Require: 0 <= maxtr");
	  option_data->sgnl_maxtr[ifilter] = ival;
	  nopt++;

	  option_data->num_sgnl_filters++;
	  continue;
	}
      

      /*-----   -fout   -----*/
      if (strncmp(argv[nopt], "-fout", 5) == 0)
	{
	  option_data->fout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -rout   -----*/
      if (strncmp(argv[nopt], "-rout", 5) == 0)
	{
	  option_data->rout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -cout   -----*/
      if (strncmp(argv[nopt], "-cout", 5) == 0)
	{
	  option_data->cout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -vout   -----*/
      if (strncmp(argv[nopt], "-vout", 5) == 0)
	{
	  option_data->vout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -stat_first   -----*/
      if (strncmp(argv[nopt], "-stat_first", 11) == 0)
	{
	  option_data->stat_first = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -datum DTYPE   -----*/
      if (strncmp(argv[nopt], "-datum", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need argument after -datum ");

	  if      (!strcmp(argv[nopt],"short")) option_data->datum = MRI_short;
	  else if (!strcmp(argv[nopt],"float")) option_data->datum = MRI_float;
          else WA_error ("illegal argument after -datum ");

	  nopt++;
	  continue;
	}
      

      /*-----   -bucket filename   -----*/
      if (strncmp(argv[nopt], "-bucket", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need file prefixname after -bucket ");
	  option_data->bucket_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->bucket_filename);
	  strcpy (option_data->bucket_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -coefts filename   -----*/
      if (strncmp(argv[nopt], "-coefts", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need file prefixname after -coefts ");
	  option_data->coefts_filename = 
	    malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->coefts_filename);
	  strcpy (option_data->coefts_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -fitts filename   -----*/
      if (strncmp(argv[nopt], "-fitts", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need file prefixname after -fitts ");
	  option_data->fitts_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->fitts_filename);
	  strcpy (option_data->fitts_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -sgnlts filename   -----*/
      if (strncmp(argv[nopt], "-sgnlts", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need file prefixname after -sgnlts ");
	  option_data->sgnlts_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->sgnlts_filename);
	  strcpy (option_data->sgnlts_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -errts filename   -----*/
      if (strncmp(argv[nopt], "-errts", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  WA_error ("need file prefixname after -errts ");
	  option_data->errts_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  MTEST (option_data->errts_filename);
	  strcpy (option_data->errts_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      WA_error (message);
      
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
  char message[MAX_NAME_LENGTH];   /* error message */
  MRI_IMAGE * flim;                /* pointer to image structures
			      -- used to read 1D ASCII */
  float * far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */
  int iy;                  /* time series file column index */
  int ipt;                 /* time point index */
  float * ts_data = NULL;  /* input time series data */


  /*----- Read the time series file -----*/
  flim = mri_read_1D(ts_filename) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      WA_error (message);
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
  WA_options * option_data,         /* wavelet program options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length                 /* length of fMRI time series */
)

{
  char message[MAX_NAME_LENGTH];    /* error message */


  /*----- Read the input fMRI measurement data -----*/
  if (option_data->input1D_filename != NULL)
    {
      /*----- Read the input fMRI 1D time series -----*/
      *fmri_data = read_time_series (option_data->input1D_filename, 
				     fmri_length);
      if (*fmri_data == NULL)  
	{ 
	  sprintf (message,  "Unable to read time series file: %s", 
		   option_data->input1D_filename);
	  WA_error (message);
	}  
      *dset_time = NULL;
    }

  else if (option_data->input_filename != NULL)
    {
      /*----- Read the input 3d+time dataset -----*/
      *dset_time = THD_open_one_dataset (option_data->input_filename);
      if (!ISVALID_3DIM_DATASET(*dset_time))  
	{ 
	  sprintf (message,  "Unable to open data file: %s", 
		   option_data->input_filename);
	  WA_error (message);
	}  
      DSET_load(*dset_time) ; CHECK_LOAD_ERROR(*dset_time) ;

      if (option_data->mask_filename != NULL)
	{
	  /*----- Read the input mask dataset -----*/
	  *mask_dset = THD_open_dataset (option_data->mask_filename);
	  if (!ISVALID_3DIM_DATASET(*mask_dset))  
	    { 
	      sprintf (message,  "Unable to open mask file: %s", 
		       option_data->mask_filename);
	      WA_error (message);
	    }  
	  DSET_load(*mask_dset) ;
	}
    }
  else
    WA_error ("Must specify input data");

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
      WA_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists "
	       " -- cannot continue!\a\n",
	       new_dset->dblk->diskptr->header_name);
      WA_error (message);
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
  WA_options * option_data,       /* wavelet analysis program options */
  THD_3dim_dataset * dset_time    /* input 3d+time data set */
)

{

  if (option_data->bucket_filename != NULL)   
    check_one_output_file (dset_time, option_data->bucket_filename);

  if (option_data->coefts_filename != NULL)   
    check_one_output_file (dset_time, option_data->coefts_filename);

  if (option_data->fitts_filename != NULL)   
    check_one_output_file (dset_time, option_data->fitts_filename);

  if (option_data->sgnlts_filename != NULL)   
    check_one_output_file (dset_time, option_data->sgnlts_filename);

  if (option_data->errts_filename != NULL)   
    check_one_output_file (dset_time, option_data->errts_filename);
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs 
(
  WA_options * option_data,       /* wavelet analysis program options */
  THD_3dim_dataset * dset_time,   /* input 3d+time data set */
  THD_3dim_dataset * mask_dset    /* input mask data set */
)

{
  char message[MAX_NAME_LENGTH];    /* error message */


  /*----- If mask is used, check for compatible dimensions -----*/
  if (mask_dset != NULL)
    {
      if ( (DSET_NX(dset_time) != DSET_NX(mask_dset))
	   || (DSET_NY(dset_time) != DSET_NY(mask_dset))
	   || (DSET_NZ(dset_time) != DSET_NZ(mask_dset)) )
	{
	  sprintf (message, "%s and %s have incompatible dimensions",
		   option_data->input_filename, option_data->mask_filename);
	  WA_error (message);
	}

      if (DSET_NVALS(mask_dset) != 1 )
	WA_error ("Must specify 1 sub-brick from mask dataset");
    }


  /*----- Check whether any of the output files already exist -----*/
  if( THD_deathcon() ) check_output_files (option_data, dset_time);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the filters and control variables.
*/
  
void initialize_filters 
(
  WA_options * option_data,       /* wavelet analysis program options */
  THD_3dim_dataset * dset_time,   /* input 3d+time data set */
  THD_3dim_dataset * mask_dset,   /* input mask data set */
  int fmri_length                 /* length of input fMRI time series */
)

{
  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */
  int i;                   /* filter coefficient index */
  int f, q, p;             /* numbers of model parameters */


  /*----- Initialize local variables -----*/
  if (option_data->input1D_filename != NULL)
    nt = fmri_length;
  else
    nt = DSET_NUM_TIMES (dset_time);

 
  /*----- Determine the number of usable time points -----*/
  NFirst = option_data->NFirst;

  NLast = option_data->NLast;   
  if (NLast > nt-1)  NLast = nt-1;

  N = NLast - NFirst + 1;
  N = powerof2(my_log2(N));
  NLast = N + NFirst + 1;

  option_data->N = N;
  option_data->NLast = NLast;


  /*----- Initialize the filters -----*/
  option_data->stop_filter = 
    FWT_1d_stop_filter (option_data->num_stop_filters, option_data->stop_band,
			option_data->stop_mintr,  option_data->stop_maxtr,
			option_data->NFirst, option_data->N);

  option_data->base_filter = 
    FWT_1d_pass_filter (option_data->num_base_filters, option_data->base_band,
			option_data->base_mintr,  option_data->base_maxtr,
			option_data->NFirst, option_data->N);
					  
  option_data->sgnl_filter = 
    FWT_1d_pass_filter (option_data->num_sgnl_filters, option_data->sgnl_band,
			option_data->sgnl_mintr,  option_data->sgnl_maxtr,
			option_data->NFirst, option_data->N);

					  
  /*----- Count numbers of model parameters -----*/
  f = 0;
  for (i = 0;  i < N;  i++)
    if (option_data->stop_filter[i] == 0.0)
      {
	f++;
	option_data->base_filter[i] = 0.0;
	option_data->sgnl_filter[i] = 0.0;
      }
  option_data->f = f;

  q = 0;
  for (i = 0;  i < N;  i++)
    if (option_data->base_filter[i] == 1.0)
      {
	q++;
	option_data->sgnl_filter[i] = 1.0;
      }
  option_data->q = q;

  p = 0;
  for (i = 0;  i < N;  i++)
    if (option_data->sgnl_filter[i] == 1.0)
      {
	p++;
      }
  option_data->p = p;    


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Allocate memory for output volumes.
*/

void allocate_memory 
(
  WA_options * option_data,         /* wavelet analysis options */
  THD_3dim_dataset * dset,          /* input 3d+time data set */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float ** mse_vol,         /* volume of full model mean square error */
  float ** ffull_vol,       /* volume of full model F-statistics */
  float ** rfull_vol,       /* volume of full model R^2 stats. */

  float *** coefts_vol,     /* volumes for forward wavelet transform coefs. */
  float *** fitts_vol,      /* volumes for wavelet filtered time series data */
  float *** sgnlts_vol,     /* volumes for signal model fit to input data */
  float *** errts_vol       /* volumes for residual errors */
)

{
  int nxyz;                  /* total number of voxels */
  int ixyz;                  /* voxel index */
  int it;                    /* time point index */
  int N;                     /* number of usable data points */
  int ip;                    /* parameter index */
  int p;                     /* number of parameters in the full model */


  /*----- Initialize local variables -----*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
  N = option_data->N;
  p = option_data->p;


  /*----- Allocate memory space for volume data -----*/
  *coef_vol  = (float **) malloc (sizeof(float *) * p);   MTEST(*coef_vol);

  for (ip = 0;  ip < p;  ip++)
    {
      (*coef_vol)[ip]  = (float *) malloc (sizeof(float) * nxyz);
      MTEST((*coef_vol)[ip]);    
       for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  (*coef_vol)[ip][ixyz]  = 0.0;
	}
    }


  *mse_vol   = (float *) malloc (sizeof(float) * nxyz);   MTEST(*mse_vol);
  *ffull_vol = (float *) malloc (sizeof(float) * nxyz);   MTEST(*ffull_vol);
  *rfull_vol = (float *) malloc (sizeof(float) * nxyz);   MTEST(*rfull_vol);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      (*mse_vol)[ixyz]   = 0.0;
      (*ffull_vol)[ixyz] = 0.0;
      (*rfull_vol)[ixyz] = 0.0;
    }


  /*----- Allocate memory for forward wavelet transform coefficients -----*/
  if (option_data->coefts_filename != NULL)
    {
      *coefts_vol = (float **) malloc (sizeof(float **) * N);
      MTEST (*coefts_vol);
      for (it = 0;  it < N;  it++)
	{
	  (*coefts_vol)[it] = (float *) malloc (sizeof(float *) * nxyz);
	  MTEST ((*coefts_vol)[it]);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    (*coefts_vol)[it][ixyz] = 0.0;
	}
    }

  /*----- Allocate memory for filtered time series -----*/
  if (option_data->fitts_filename != NULL)
    {
      *fitts_vol = (float **) malloc (sizeof(float **) * N);
      MTEST (*fitts_vol);
      for (it = 0;  it < N;  it++)
	{
	  (*fitts_vol)[it] = (float *) malloc (sizeof(float *) * nxyz);
	  MTEST ((*fitts_vol)[it]);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    (*fitts_vol)[it][ixyz] = 0.0;
	}
    }

  /*----- Allocate memory for signal model time series -----*/
  if (option_data->sgnlts_filename != NULL)
    {
      *sgnlts_vol = (float **) malloc (sizeof(float **) * N);
      MTEST (*sgnlts_vol);
      for (it = 0;  it < N;  it++)
	{
	  (*sgnlts_vol)[it] = (float *) malloc (sizeof(float *) * nxyz);
	  MTEST ((*sgnlts_vol)[it]);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    (*sgnlts_vol)[it][ixyz] = 0.0;
	}
    }

  /*----- Allocate memory for residual time series -----*/
  if (option_data->errts_filename != NULL)
    {
      *errts_vol = (float **) malloc (sizeof(float **) * N);
      MTEST (*errts_vol);
      for (it = 0;  it < N;  it++)
	{
	  (*errts_vol)[it] = (float *) malloc (sizeof(float *) * nxyz);
	  MTEST ((*errts_vol)[it]);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    (*errts_vol)[it][ixyz] = 0.0;
	}
    }
}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  WA_options ** option_data,        /* wavelet analysis options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */

  float *** coef_vol,        /* array of volumes of signal model parameters */
  float ** mse_vol,          /* volume of full model mean square error */
  float ** ffull_vol,        /* volume of full model F-statistics */
  float ** rfull_vol,        /* volume of full model R^2 stats. */

  float *** coefts_vol,      /* volumes for forward wavelet transform coefs. */
  float *** fitts_vol,       /* volumes for wavelet filtered time series  */
  float *** sgnlts_vol,      /* volumes for signal model fit to input data */
  float *** errts_vol        /* volumes for residual errors */
)
     
{

  /*----- Allocate memory -----*/
  *option_data = (WA_options *) malloc (sizeof(WA_options));

   
  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Read input data -----*/
  read_input_data (*option_data, dset_time, mask_dset, fmri_data, fmri_length);


  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (*option_data, *dset_time, *mask_dset);
  

  /*----- Initialize the filters and control variables  -----*/
  initialize_filters (*option_data, *dset_time, *mask_dset, *fmri_length);
  

  /*----- Allocate memory for output volumes -----*/
  if ((*option_data)->input1D_filename == NULL)
    allocate_memory (*option_data, *dset_time, 
		     coef_vol, mse_vol, ffull_vol, rfull_vol,
		     coefts_vol, fitts_vol, sgnlts_vol, errts_vol);

}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array 
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
)

{
  MRI_IMAGE * im = NULL;   /* intermediate float data */
  float * ar = NULL;       /* pointer to float data */
  int ts_length;           /* length of input 3d+time data set */
  int it;                  /* time index */


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  WA_error ("Unable to extract data from 3d+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
  ts_length = DSET_NUM_TIMES (dset_time);
  ar = MRI_FLOAT_PTR (im);
  for (it = 0;  it < ts_length;  it++)
    {
      ts_array[it] = ar[it];
    }


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Save results for this voxel.
*/

void save_voxel 
(
  WA_options * option_data,    /* wavelet analysis options */
  int iv,                      /* current voxel index */      
  float * coef,                /* regression parameters */
  float mse,                   /* full model mean square error */
  float ffull,                 /* full model F-statistic */
  float rfull,                 /* full model R^2 stat. */

  float * coefts,             /* forward wavelet transform coefficients */
  float * fitts,              /* wavelet filtered time series */
  float * sgnlts,              /* signal model fitted time series */
  float * errts,               /* signal model residual error time series */

  float ** coef_vol,        /* array of volumes of signal model parameters */
  float * mse_vol,          /* volume of full model mean square error */
  float * ffull_vol,        /* volume of full model F-statistics */
  float * rfull_vol,        /* volume of full model R^2 stats. */

  float ** coefts_vol,     /* volumes for forward wavelet transform coefs. */
  float ** fitts_vol,      /* volumes for wavelet filtered time series data */
  float ** sgnlts_vol,      /* volumes for signal model fit to input data */
  float ** errts_vol        /* volumes for residual errors */

)

{
  int ip;                   /* parameter index */ 
  int p;                    /* total number of parameters */
  int it;                   /* time point index */
  int N;                    /* number of usable data points */


  /*----- Initialize local variables -----*/
  p = option_data->p;
  N = option_data->N;


  /*----- Saved wavelet coefficients -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      coef_vol[ip][iv]  = coef[ip];
    }


  /*----- Save full model mean square error -----*/
  mse_vol[iv] = mse;


  /*----- Save regression F-statistic -----*/
  ffull_vol[iv] = ffull;


  /*----- Save R^2 values -----*/
  rfull_vol[iv] = rfull;


  /*----- Save the forward wavelet transform coefficients -----*/
  if (coefts_vol != NULL)
    for (it = 0;  it < N;  it++)
      coefts_vol[it][iv] = coefts[it];


  /*----- Save the wavelet filtered time series -----*/
  if (fitts_vol != NULL)
    for (it = 0;  it < N;  it++)
      fitts_vol[it][iv] = fitts[it];


  /*----- Save the fitted signal model time series -----*/
  if (sgnlts_vol != NULL)
    for (it = 0;  it < N;  it++)
      sgnlts_vol[it][iv] = sgnlts[it];


  /*----- Save the residual errors time series -----*/
  if (errts_vol != NULL)
    for (it = 0;  it < N;  it++)
      errts_vol[it][iv] = errts[it];


}


/*---------------------------------------------------------------------------*/
/*
  Calculate the wavelet signal model and associated statistics.
*/

void calculate_results 
(
  WA_options * option_data,         /* wavelet analysis options */
  THD_3dim_dataset * dset,          /* input 3d+time data set */
  THD_3dim_dataset * mask,          /* input mask data set */
  float * fmri_data,                /* input fMRI time series data */
  int fmri_length,                  /* length of fMRI time series */

  float ** coef_vol,        /* array of volumes of signal model parameters */
  float * mse_vol,          /* volume of full model mean square error */
  float * ffull_vol,        /* volume of F-statistic for the full model */
  float * rfull_vol,        /* volume of R^2 for the full model */

  float ** coefts_vol,     /* volumes for forward wavelet transform coefs. */
  float ** fitts_vol,      /* volumes for wavelet filtered time series */
  float ** sgnlts_vol,      /* volumes for full model fit to input data */
  float ** errts_vol        /* volumes for residual errors */
)
  
{
  float * ts_array = NULL;    /* array of measured data for one voxel */
  float mask_val[1];          /* value of mask at current voxel */

  int f;
  int p;                      /* number of parameters in the full model */
  int q;                      /* number of parameters in the baseline model */

  float * coef = NULL;        /* regression parameters */
  float sse_base;             /* baseline model error sum of squares */ 
  float sse_full;             /* full model error sum of squares */
  float ffull;                /* full model F-statistic */
  float rfull;                /* full model R^2 stat. */

  int ixyz;                   /* voxel index */
  int nxyz;                   /* number of voxels per dataset */

  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable data points */

  int i;                   /* data point index */

  float * coefts = NULL;   /* filtered FWT coefficients */
  float * fitts  = NULL;   /* filterd time series */
  float * sgnlts = NULL;   /* signal model fitted time series */
  float * errts  = NULL;   /* residual error time series */

  char * label = NULL;     /* string containing stat. summary of results */


  /*----- Initialize local variables -----*/
  if (option_data->input1D_filename != NULL)
    {
      nxyz = 1;
      nt = fmri_length;
    }
  else
    {
      nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;       
      nt = DSET_NUM_TIMES (dset);
    }

  NFirst = option_data->NFirst;
  NLast = option_data->NLast;   
  N = option_data->N;
  f = option_data->f;
  q = option_data->q;
  p = option_data->p;


  /*----- Allocate memory for arrays -----*/
  ts_array = (float *) malloc (sizeof(float) * nt);   MTEST (ts_array);
  coef     = (float *) malloc (sizeof(float) * p);    MTEST (coef);
  coefts  = (float *) malloc (sizeof(float) * N);    MTEST (coefts);
  fitts   = (float *) malloc (sizeof(float) * N);    MTEST (fitts);
  sgnlts   = (float *) malloc (sizeof(float) * N);    MTEST (sgnlts);
  errts    = (float *) malloc (sizeof(float) * N);    MTEST (errts);

  
  /*----- Loop over all voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      /*----- Apply mask? -----*/
      if (mask != NULL)
	{
	  extract_ts_array (mask, ixyz, mask_val);
	  if (mask_val[0] == 0.0)  continue; 
	}
      
      
      /*----- Extract Y-data for this voxel -----*/
      if (option_data->input1D_filename != NULL)
	{
	  for (i = 0;  i < N;  i++)
	    ts_array[i] = fmri_data[i+NFirst];
	}
      else
	extract_ts_array (dset, ixyz, ts_array);

      	  
      /*----- Perform the wavelet analysis for this voxel-----*/
      wavelet_analysis (option_data->wavelet_type, 
			f, option_data->stop_filter,
			q, option_data->base_filter,
			p, option_data->sgnl_filter,
			N, ts_array+NFirst, coef,
			&sse_base, &sse_full, &ffull, &rfull,
			coefts, fitts, sgnlts, errts);

      
      /*----- Save results for this voxel -----*/
      if (option_data->input1D_filename == NULL)
	save_voxel (option_data, ixyz, coef, sse_full/(N-f-p), ffull, rfull, 
		    coefts, fitts, sgnlts, errts, 
		    coef_vol, mse_vol, ffull_vol, rfull_vol, 
		    coefts_vol, fitts_vol, sgnlts_vol, errts_vol);
      else
	{  
	  /*----- If only one voxel, save results as .1D files -----*/
	  ts_fprint ("WA.coefts.1D", N, coefts);
	  ts_fprint ("WA.fitts.1D",  N, fitts);
	  ts_fprint ("WA.sgnlts.1D", N, sgnlts);
	  ts_fprint ("WA.errts.1D",  N, errts);
	}
      
	  
      /*----- Report results for this voxel -----*/
      if ( ((ffull > option_data->fdisp) && (option_data->fdisp >= 0.0))
	   || (option_data->input1D_filename != NULL) )
	{
	  printf ("\n\nResults for Voxel #%d: \n", ixyz);
	  report_results (N, NFirst, f, p, q, 
			  option_data->base_filter, option_data->sgnl_filter,
			  coef, sse_base, sse_full, ffull, rfull, 
			  &label);
	  printf ("%s \n", label);
	}

	  
    }  /*----- Loop over voxels -----*/



  /*----- Release memory -----*/
  free (ts_array);  ts_array = NULL;
  free (coef);      coef     = NULL;
  free (coefts);    coefts   = NULL;
  free (fitts);     fitts    = NULL;
  free (sgnlts);    sgnlts   = NULL;
  free (errts);     errts    = NULL;

}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI 3d+time data set. 
*/


void write_ts_array 
(
  int argc,                              /* number of input arguments */
  char ** argv,                          /* array of input arguments */ 
  WA_options * option_data,              /* wavelet analysis options */
  int ts_length,                         /* length of time series data */  
  float *** vol_array,                   /* output time series volume data */
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
  if( option_data->datum != MRI_float )
    { bar  = (short **) malloc (sizeof(short *) * ts_length);   MTEST (bar); }
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
			    ADN_datum_all,   option_data->datum,
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
      if( option_data->datum == MRI_float )
      {
          fbuf[ib] = 1.0;  /* factor array is all 1's for float output */

          /*----- attach vol_array[ib] to be sub-brick #ib -----*/
          mri_fix_data_pointer ((*vol_array)[ib], DSET_BRICK(new_dset,ib)); 
          (*vol_array)[ib] = NULL;  /* data is now owned by dataset */
      }
      else
      {
          /*----- Set pointer to appropriate volume -----*/
          volume = (*vol_array)[ib];
      
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
  }
  if( option_data->datum == MRI_float ) *vol_array = NULL;


  /*----- write afni data set -----*/

  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset (NULL, NULL, new_dset, True);
  fprintf(stderr,"--- Wrote 3d+time dataset into %s\n",DSET_BRIKNAME(new_dset)) ;
  

  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (new_dset, False);   new_dset = NULL ;
  free (fbuf);   fbuf = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Attach one sub-brick to output bucket data set.
*/

void attach_sub_brick
(
  THD_3dim_dataset * new_dset,      /* output bucket dataset */
  int ibrick,               /* sub-brick indices */
  float * volume,           /* volume of floating point data */
  int nxyz,                 /* total number of voxels */
  int brick_type,           /* indicates statistical type of sub-brick */
  char * brick_label,       /* character string label for sub-brick */
  int dof, 
  int ndof, 
  int ddof,                 /* degrees of freedom */
  short ** bar              /* bar[ib] points to data for sub-brick #ib */  
)

{
  const float EPSILON = 1.0e-10;
  float factor;             /* factor is new scale factor for sub-brick #ib */


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
    EDIT_BRICK_TO_FITT (new_dset, ibrick, dof);
  else if (brick_type == FUNC_FT_TYPE)
    EDIT_BRICK_TO_FIFT (new_dset, ibrick, ndof, ddof);
  
  
  /*----- attach bar[ib] to be sub-brick #ibrick -----*/
  EDIT_substitute_brick (new_dset, ibrick, MRI_short, bar[ibrick]);

}

/*---------------------------------------------------------------------------*/
/*
  Attach one sub-brick to output bucket data set (of type MRI_float).
*/

void attach_float_brick
(
  THD_3dim_dataset * new_dset,      /* output bucket dataset */
  int ibrick,               /* sub-brick indices */
  float * volume,           /* volume of floating point data */
  int brick_type,           /* indicates statistical type of sub-brick */
  char * brick_label,       /* character string label for sub-brick */
  int dof, 
  int ndof, 
  int ddof                  /* degrees of freedom */
)

{
  /*----- edit the sub-brick -----*/
  EDIT_BRICK_LABEL (new_dset, ibrick, brick_label);
  EDIT_BRICK_FACTOR (new_dset, ibrick, 1.0);   /* all factors are 1.0 here */

  if (brick_type == FUNC_TT_TYPE)
    EDIT_BRICK_TO_FITT (new_dset, ibrick, dof);
  else if (brick_type == FUNC_FT_TYPE)
    EDIT_BRICK_TO_FIFT (new_dset, ibrick, ndof, ddof);
  
  
  /*----- attach bar[ib] to be sub-brick #ibrick -----*/
  EDIT_substitute_brick (new_dset, ibrick, MRI_float, volume);

}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one bucket data set.

  All _vol variables have ptrs passed now, to free memory if it is used
  for the MRI_float dataset sub-bricks.             07 Feb 2006 [rickr]
*/

void write_bucket_data 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  WA_options * option_data,         /* wavelet analysis options */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float ** mse_vol,         /* volume of full model mean square error */
  float ** ffull_vol,       /* volume of full model F-statistics */
  float ** rfull_vol        /* volume of full model R^2 statistics */
)

{
  THD_3dim_dataset * old_dset = NULL;      /* prototype dataset */
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  char output_prefix[MAX_NAME_LENGTH];     /* prefix name for bucket dataset */
  char output_session[MAX_NAME_LENGTH];    /* directory for bucket dataset */
  int nbricks;              /* number of sub-bricks in bucket dataset */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */

  int brick_type;           /* indicates statistical type of sub-brick */
  char brick_label[MAX_NAME_LENGTH]; /* character string label for sub-brick */

  int ierror;               /* number of errors in editing data */
  float * volume;           /* volume of floating point data */

  int N;                    /* number of usable data points */
  int NFirst;
  int f;                    /* number of parameters removed by stop filter */
  int p;                    /* number of parameters in the signal model */
  int q;                    /* number of parameters in the rdcd model */
  int nxyz;                 /* total number of voxels */
  int nt;                   /* number of images in input 3d+time dataset */
  int it;
  int icoef;                /* coefficient index */
  int ibrick;               /* sub-brick index */
  int ndof, ddof;           /* degrees of freedom */
  char label[MAX_NAME_LENGTH];   /* general label for sub-bricks */


  /*----- read prototype dataset -----*/
  old_dset = THD_open_one_dataset (option_data->input_filename);

    
  /*----- Initialize local variables -----*/
  nxyz = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;  
  nt = DSET_NUM_TIMES (old_dset);


  f = option_data->f;
  q = option_data->q;
  p = option_data->p;
  N = option_data->N;
  NFirst = option_data->NFirst;
  printf ("f = %d   q = %d   p = %d   N = %d \n", f, q, p, N);
  

  /*----- Calculate number of sub-bricks in the bucket -----*/
  nbricks = p * option_data->cout
    + option_data->rout + option_data->fout + option_data->vout;
  printf ("nbricks = %d \n", nbricks);


  strcpy (output_prefix, option_data->bucket_filename);
  strcpy (output_session, "./");
  
 
  /*----- allocate memory -----*/
  if( option_data->datum != MRI_float )
  {
      bar  = (short **) malloc (sizeof(short *) * nbricks);
      MTEST (bar);
  }


  /*-- make an empty copy of prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (old_dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History( old_dset , new_dset ) ;
  tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;
  sprintf (label, "Output prefix: %s", output_prefix);
  tross_Append_History ( new_dset, label);

  
  /*----- delete prototype dataset -----*/ 
  THD_delete_3dim_dataset( old_dset , False );  old_dset = NULL ;


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          output_prefix,
			    ADN_directory_name,  output_session,
			    ADN_type,            HEAD_FUNC_TYPE,
			    ADN_func_type,       FUNC_BUCK_TYPE,
			    ADN_datum_all,       option_data->datum,
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
  

  /*----- Attach individual sub-bricks to the bucket dataset -----*/
  ibrick = 0;


  /*----- Full model wavelet coefficients -----*/
  if (option_data->cout)
    {
      /*----- User can choose to place full model stats first -----*/
      if (option_data->stat_first)  
	ibrick += option_data->vout + option_data->rout + option_data->fout;

      icoef = 0;
      for (it = 0;  it < N;  it++)        
	{                      
	  if (option_data->sgnl_filter[it])
	    {                             
	      /*----- Wavelet coefficient -----*/
	      brick_type = FUNC_FIM_TYPE;
	      
	      {
		int band, mintr, maxtr;
		
		if (it == 0)
		  {
		    band = -1;
		    mintr = 0;
		    maxtr = N-1;
		  }
		else
		  {
		    band = my_log2(it);
		    mintr = (it - powerof2(band)) * powerof2(my_log2(N)-band);
		    maxtr = mintr + powerof2(my_log2(N)-band) - 1;
		  }
	    
		mintr += NFirst;
		maxtr += NFirst;
	    
		if (option_data->base_filter[it])
		  sprintf (brick_label, "B(%2d)[%3d,%3d]", band, mintr, maxtr);
		else
		  sprintf (brick_label, "S(%2d)[%3d,%3d]", band, mintr, maxtr);
	      }

	      volume = (*coef_vol)[icoef];		  
              if( option_data->datum == MRI_float )
              {
                  attach_float_brick (new_dset, ibrick, volume,
                                      brick_type, brick_label, 0, 0, 0);
                  (*coef_vol)[icoef] = NULL; /* dataset owns this now */
              }
              else
                  attach_sub_brick (new_dset, ibrick, volume, nxyz, 
                                    brick_type, brick_label, 0, 0, 0, bar);
	      
	      ibrick++;
	      icoef++;
	    }
	  
	}  /* End loop over Wavelet coefficients */

        if( option_data->datum == MRI_float ) /* then free array, also */
        {
            free( *coef_vol );
            *coef_vol = NULL;
        }
    }  /* if (option_data->cout) */


      
  /*----- Statistics for full model -----*/
  if (option_data->stat_first)  ibrick = 0;

  /*----- Full model MSE -----*/
  if (option_data->vout)
    {
      brick_type = FUNC_FIM_TYPE;
      sprintf (brick_label, "Full MSE");
      volume = *mse_vol;
      if( option_data->datum == MRI_float )
      {
          attach_float_brick (new_dset, ibrick, volume,
                              brick_type, brick_label, 0, 0, 0);
          *mse_vol = NULL;  /* since the dataset owns this now */
      }
      else
          attach_sub_brick (new_dset, ibrick, volume, nxyz, 
                            brick_type, brick_label, 0, 0, 0, bar);
      ibrick++;
    }

  /*----- Full model R^2 -----*/
  if (option_data->rout)
    {
      brick_type = FUNC_THR_TYPE;
      sprintf (brick_label, "Full R^2");
      volume = *rfull_vol;
      if( option_data->datum == MRI_float )
      {
          attach_float_brick (new_dset, ibrick, volume,
                              brick_type, brick_label, 0, 0, 0);
          *rfull_vol = NULL;  /* since the dataset owns this now */
      }
      else
          attach_sub_brick (new_dset, ibrick, volume, nxyz, 
                            brick_type, brick_label, 0, 0, 0, bar);
      ibrick++;
    }

  /*----- Full model F-stat -----*/
  if (option_data->fout)
    {
      brick_type = FUNC_FT_TYPE;
      ndof = p - q;
      ddof = N - f - p;
      sprintf (brick_label, "Full F-stat");
      volume = *ffull_vol;
      if( option_data->datum == MRI_float )
      {
          attach_float_brick (new_dset, ibrick, volume,
                              brick_type, brick_label, 0, ndof, ddof);
          *ffull_vol = NULL;  /* since the dataset owns this now */
      }
      else
          attach_sub_brick (new_dset, ibrick, volume, nxyz, 
                            brick_type, brick_label, 0, ndof, ddof, bar);
      ibrick++;
    }


  /*----- write bucket data set -----*/
  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  fprintf(stderr,"Writing `bucket' dataset ");
  fprintf(stderr,"into %s\n", DSET_BRIKNAME(new_dset));

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Write out the user requested output files.

  Each item has address passed, so that data used for MRI_float datasets
  can have its pointer(s) cleared.                    7 Feb 2006 [rickr]
*/

void output_results
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  WA_options * option_data,         /* wavelet analysis options */

  float *** coef_vol,        /* array of volumes of signal model parameters */
  float  ** mse_vol,         /* volume of full model mean square error */
  float  ** ffull_vol,       /* volume of full model F-statistics */
  float  ** rfull_vol,       /* volume of full model R^2 statistics */
  float *** coefts_vol,      /* volumes for forward wavelet transform coefs. */
  float *** fitts_vol,       /* volumes for wavelet filtered time series */
  float *** sgnlts_vol,      /* volumes for signal model time series */
  float *** errts_vol        /* volumes for residual errors */
)

{
  int N;                    /* number of usable data points */


  /*----- Initialize local variables -----*/
  N = option_data->N;


  /*----- Write the bucket dataset -----*/
  if (option_data->bucket_filename != NULL)
    write_bucket_data (argc, argv, option_data,  coef_vol, 
		       mse_vol, ffull_vol, rfull_vol);



  /*----- Write the forward wavelet transform coefficients -----*/
  if (option_data->coefts_filename != NULL)
    write_ts_array (argc, argv, option_data, N, coefts_vol, 
		    option_data->coefts_filename);


  /*----- Write the wavelet filtered 3d+time dataset -----*/
  if (option_data->fitts_filename != NULL)
    write_ts_array (argc, argv, option_data, N, fitts_vol, 
		    option_data->fitts_filename);


  /*----- Write the signal model 3d+time dataset -----*/
  if (option_data->sgnlts_filename != NULL)
    write_ts_array (argc, argv, option_data, N, sgnlts_vol, 
		    option_data->sgnlts_filename);


  /*----- Write the residual errors 3d+time dataset -----*/
  if (option_data->errts_filename != NULL)
    write_ts_array (argc, argv, option_data, N, errts_vol, 
		    option_data->errts_filename);



}


/*---------------------------------------------------------------------------*/

void terminate_program
(
  WA_options ** option_data,         /* wavelet analysis options */
  float ** fmri_data,                /* input fMRI time series data */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float ** mse_vol,         /* volume of full model mean square error */
  float ** ffull_vol,       /* volume of full model F-statistics */
  float ** rfull_vol,       /* volume of full model R^2 statistics */

  float *** coefts_vol,     /* volumes for forward wavelet transform coefs. */
  float *** fitts_vol,      /* volumes for wavelet filtered time series data */
  float *** sgnlts_vol,     /* volumes for signal model fit to input data */
  float *** errts_vol       /* volumes for residual errors */
)

{
  int p;                    /* number of parameters in full model */
  int ip;                   /* parameter index */
  int it;                   /* time index */
  int N;                    /* number of usable data points */


  /*----- Initialize local variables -----*/
  p = (*option_data)->p;
  N = (*option_data)->N;


  /*----- Deallocate memory for option data -----*/   
  if ((*option_data)->stop_filter != NULL)  free ((*option_data)->stop_filter);
  if ((*option_data)->base_filter != NULL)  free ((*option_data)->base_filter);
  if ((*option_data)->sgnl_filter != NULL)  free ((*option_data)->sgnl_filter);
  if ((*option_data)->input_filename != NULL) 
    free ((*option_data)->input_filename);
  if ((*option_data)->mask_filename != NULL)  
    free ((*option_data)->mask_filename);  
  if ((*option_data)->input1D_filename != NULL) 
    free ((*option_data)->input1D_filename);
  if ((*option_data)->bucket_filename != NULL) 
    free ((*option_data)->bucket_filename);
  if ((*option_data)->coefts_filename != NULL) 
    free ((*option_data)->coefts_filename);
  if ((*option_data)->fitts_filename != NULL) 
    free ((*option_data)->fitts_filename);
  if ((*option_data)->sgnlts_filename != NULL) 
    free ((*option_data)->sgnlts_filename);
  if ((*option_data)->errts_filename != NULL) 
    free ((*option_data)->errts_filename);
  free (*option_data);  *option_data = NULL;


  /*----- Deallocate memory for fMRI time series data -----*/
  if (*fmri_data != NULL)    { free (*fmri_data);  *fmri_data = NULL; }


  /*----- Deallocate space for volume data -----*/
  if (*coef_vol  != NULL) 
    {
      for (ip = 0;  ip < p;  ip++)
	{
	  if ((*coef_vol)[ip] != NULL)
	    { free ((*coef_vol)[ip]);   (*coef_vol)[ip] = NULL; }
	}
      free (*coef_vol);   *coef_vol  = NULL;
    }
  
  if (*mse_vol   != NULL)    { free (*mse_vol);    *mse_vol   = NULL; }
  if (*ffull_vol != NULL)    { free (*ffull_vol);  *ffull_vol = NULL; }
  if (*rfull_vol != NULL)    { free (*rfull_vol);  *rfull_vol = NULL; } 


  /*----- Deallocate space for forward wavelet transform coefficients -----*/
  if (*coefts_vol != NULL)
    {
      for (it = 0;  it < N;  it++)
	{
          if ((*coefts_vol)[it] != NULL)
            { free ((*coefts_vol)[it]);   (*coefts_vol)[it] = NULL; }
        }
      free (*coefts_vol);   *coefts_vol = NULL;
    }


  /*----- Deallocate space for wavelet filtered time series -----*/
  if (*fitts_vol != NULL)
    {
      for (it = 0;  it < N;  it++)
        {
          if ((*fitts_vol)[it] != NULL)
	    { free ((*fitts_vol)[it]);   (*fitts_vol)[it] = NULL; }
        }
      free (*fitts_vol);   *fitts_vol = NULL;
    }


  /*----- Deallocate space for signal model time series -----*/
  if (*sgnlts_vol != NULL)
    {
      for (it = 0;  it < N;  it++)
        {
          if ((*sgnlts_vol)[it] != NULL)
            { free ((*sgnlts_vol)[it]);   (*sgnlts_vol)[it] = NULL; }
        }
      free (*sgnlts_vol);   *sgnlts_vol = NULL;
    }


  /*----- Deallocate space for residual errors time series -----*/
  if (*errts_vol != NULL)
    {
      for (it = 0;  it < N;  it++)
        {
          if ((*errts_vol)[it] != NULL)
            { free ((*errts_vol)[it]);   (*errts_vol)[it] = NULL; }
        }
      free (*errts_vol);   *errts_vol = NULL;
    }
}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  WA_options * option_data;               /* wavelet analysis options */
  THD_3dim_dataset * dset_time = NULL;    /* input 3d+time data set */
  THD_3dim_dataset * mask_dset = NULL;    /* input mask data set */
  float * fmri_data = NULL;               /* input fMRI time series data */
  int fmri_length;                        /* length of fMRI time series */

  float ** coef_vol = NULL;   /* array of volumes for model parameters */
  float * mse_vol   = NULL;   /* volume of full model mean square error */
  float * ffull_vol = NULL;   /* volume of full model F-statistics */
  float * rfull_vol = NULL;   /* volume of full model R^2 stats. */

  float ** coefts_vol = NULL; /* volumes for wavelet transform coefs.*/
  float ** fitts_vol = NULL;  /* volumes for wavelet filtered time series */
  float ** sgnlts_vol = NULL;  /* volumes for signal model fit to input data */
  float ** errts_vol = NULL;   /* volumes for residual errors */

  
  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

   PRINT_VERSION("3dWavelets") ; AUTHOR(PROGRAM_AUTHOR);
   mainENTRY("3dWavelets main") ; machdep() ;

  
  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, 
		      &dset_time, &mask_dset, &fmri_data, &fmri_length,
		      &coef_vol, &mse_vol, &ffull_vol, &rfull_vol, 
		      &coefts_vol, &fitts_vol, &sgnlts_vol, &errts_vol);


  /*----- Perform wavelet analysis -----*/
  calculate_results (option_data, dset_time, mask_dset, fmri_data, fmri_length,
		     coef_vol, mse_vol, ffull_vol, rfull_vol, 
		     coefts_vol, fitts_vol, sgnlts_vol, errts_vol);
  

  /*----- Deallocate memory for input datasets -----*/   
  if (dset_time != NULL)  
    { THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL; }
  if (mask_dset != NULL)  
    { THD_delete_3dim_dataset (mask_dset, False);  mask_dset = NULL; }


  /*----- Write requested output files -----*/
  /* (pass addresses, in case data is stolen for datasets) 7 Feb 2006 [rickr] */
  if (option_data->input1D_filename == NULL)
    output_results (argc, argv, option_data, &coef_vol, &mse_vol, 
		    &ffull_vol, &rfull_vol, 
		    &coefts_vol, &fitts_vol, &sgnlts_vol, &errts_vol);


  /*----- Terminate program -----*/
  terminate_program (&option_data, &fmri_data, 
		     &coef_vol, &mse_vol, &ffull_vol, &rfull_vol,
		     &coefts_vol, &fitts_vol, &sgnlts_vol, &errts_vol);

  exit(0);
}
