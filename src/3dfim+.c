/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Program to calculate the cross-correlation of an ideal reference waveform  
  with the measured FMRI time series for each voxel.                          
  This is the stand-alone (batch command) version of the afni fim+ function.

  File:    3dfim+.c
  Author:  B. Douglas Ward
  Date:    28 April 2000


  Mod:     Use output_type array in regression_analysis routine to avoid
           some unnecessary calculations.
  Date:    18 May 2000

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Add FICO-ness of sub-bricks for Spearman and Quadrant correlation.
  Date     29 Oct 2004 - RWCox

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dfim+"                        /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "28 April 2000"   /* date of initial program release */
#define PROGRAM_LATEST  "29 October 2004"  /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define MAX_FILES 200                        /* maximum number of ideal files */
                                             /* = maximum number of ort files */
                                             /* It looks like this is also the
                                                limit on the number of regressors
                                                (columns) in an ideal file 
                                                   ZSS May 15 08 */ 
#define RA_error FIM_error

/*---------------------------------------------------------------------------*/

#include "mrilib.h"
#include "matrix.h"

#include "fim+.c"

/*---------------------------------------------------------------------------*/

typedef struct FIM_options
{ 
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable data points from input data */
  int polort;              /* degree of polynomial for baseline model */
  int num_ortts;           /* number of ort time series */
  int num_idealts;         /* number of ideal time series */
  int q;                   /* number of parameters in the baseline model */
  int p;                   /* number of parameters in the baseline model 
		              plus number of ideals */

  float fim_thr;           /* threshold for internal fim mask */
  float cdisp;             /* minimum correlation coefficient for display */ 

  char * input_filename;   /* input 3d+time dataset filename */
  char * mask_filename;    /* input mask dataset filename */
  char * input1D_filename; /* input fMRI measurement time series */

  int num_ort_files;                  /* number of ort files */
  char * ort_filename[MAX_FILES];     /* input ort time series file names */
  int num_ideal_files;                /* number of ideal files */
  char * ideal_filename[MAX_FILES];   /* input ideal time series file names */
  char * bucket_filename;             /* output bucket dataset file name */

  int output_type[MAX_OUTPUT_TYPE];   /* output type options */

} FIM_options;


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array 
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
);

/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void FIM_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dfim+ help menu.
*/

void display_help_menu()
{
  printf (
"Program to calculate the cross-correlation of an ideal reference waveform  \n"
"with the measured FMRI time series for each voxel.                         \n"
    "                                                                       \n"
    "Usage:                                                                 \n"
    "3dfim+                                                                 \n"
    "-input fname       fname = filename of input 3d+time dataset           \n"
    "[-input1D dname]   dname = filename of single (fMRI) .1D time series   \n"
    "[-mask mname]      mname = filename of 3d mask dataset                 \n"
    "[-nfirst fnum]     fnum = number of first dataset image to use in      \n"
    "                     the cross-correlation procedure. (default = 0)    \n"
    "[-nlast  lnum]     lnum = number of last dataset image to use in       \n"
    "                     the cross-correlation procedure. (default = last) \n"
    "[-polort pnum]     pnum = degree of polynomial corresponding to the    \n"
    "                     baseline model  (pnum = 0, 1, etc.)               \n"
    "                     (default: pnum = 1). Use -1 for no baseline model.\n"
    "[-fim_thr p]       p = fim internal mask threshold value (0 <= p <= 1) \n"
    "                     (default: p = 0.0999)                             \n"
    "[-cdisp cval]      Write (to screen) results for those voxels          \n"
    "                     whose correlation stat. > cval  (0 <= cval <= 1)  \n"
    "                     (default: disabled)                               \n"
    "[-ort_file sname]  sname = input ort time series file name             \n"
    "-ideal_file rname  rname = input ideal time series file name           \n"
    "                                                                       \n"
    "            Note:  The -ort_file and -ideal_file commands may be used  \n"
    "                   more than once.                                     \n"
    "            Note:  If files sname or rname contain multiple columns,   \n"
    "                   then ALL columns will be used as ort or ideal       \n"
    "                   time series.  However, individual columns or        \n"
    "                   a subset of columns may be selected using a file    \n"
    "                   name specification like 'fred.1D[0,3,5]', which     \n"
    "                   indicates that only columns #0, #3, and #5 will     \n"
    "                   be used for input.                                  \n"
    );

  printf("\n"
    "[-out param]       Flag to output the specified parameter, where       \n"
    "                   the string 'param' may be any one of the following: \n"
    "                                                                       \n"
    "%12s       L.S. fit coefficient for Best Ideal                \n"
    "%12s       Index number for Best Ideal                        \n"
    "%12s       P-P amplitude of signal response / Baseline        \n"
    "%12s       Average of baseline model response                 \n"
    "%12s       Best Ideal product-moment correlation coefficient  \n"
    "%12s       P-P amplitude of signal response / Average         \n"
    "%12s       Baseline + average of signal response              \n"
    "%12s       P-P amplitude of signal response / Topline         \n"
    "%12s       Baseline + P-P amplitude of signal response        \n"
    "%12s       Std. Dev. of residuals from best fit               \n"
    "%9sAll       This specifies all of the above parameters       \n"
    "%12s       Spearman correlation coefficient                   \n"
    "%12s       Quadrant correlation coefficient                   \n"
    "                                                                       \n"
    "            Note:  Multiple '-out' commands may be used.               \n"
    "            Note:  If a parameter name contains imbedded spaces, the   \n"
    "                   entire parameter name must be enclosed by quotes,   \n"
    "                   e.g.,  -out '%8s'                                   \n"
    "                                                                       \n"
    "[-bucket bprefix]  Create one AFNI 'bucket' dataset containing the     \n"
    "                   parameters of interest, as specified by the above   \n"
    "                   '-out' commands.                                    \n"
    "                   The output 'bucket' dataset is written to a file    \n"
    "                   with the prefix name bprefix.                       \n"
    ,
    OUTPUT_TYPE_name[FIM_FitCoef],
    OUTPUT_TYPE_name[FIM_BestIndex],
    OUTPUT_TYPE_name[FIM_PrcntChange],
    OUTPUT_TYPE_name[FIM_Baseline],
    OUTPUT_TYPE_name[FIM_Correlation],
    OUTPUT_TYPE_name[FIM_PrcntFromAve],
    OUTPUT_TYPE_name[FIM_Average],
    OUTPUT_TYPE_name[FIM_PrcntFromTop],
    OUTPUT_TYPE_name[FIM_Topline],
    OUTPUT_TYPE_name[FIM_SigmaResid],
    "",
    OUTPUT_TYPE_name[FIM_SpearmanCC],
    OUTPUT_TYPE_name[FIM_QuadrantCC],
    OUTPUT_TYPE_name[FIM_FitCoef]
);
  
  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  FIM_options * option_data    /* fim program options */
)
 
{
  int is;                     /* index */


  /*----- Initialize default values -----*/
  option_data->NFirst = 0;
  option_data->NLast  = 32767;
  option_data->N      = 0;
  option_data->polort = 1;
  option_data->num_ortts = 0;
  option_data->num_idealts = 0;
  option_data->q = 0;
  option_data->p = 0;

  option_data->fim_thr = 0.0999;
  option_data->cdisp = -1.0;

  option_data->num_ort_files = 0;
  option_data->num_ideal_files = 0;


  /*----- Initialize output flags -----*/
  for (is = 0;  is < MAX_OUTPUT_TYPE;  is++)
    option_data->output_type[is] = 0;


  /*----- Initialize file names -----*/
  option_data->input_filename = NULL;
  option_data->mask_filename = NULL;  
  option_data->input1D_filename = NULL;
  option_data->bucket_filename = NULL;

  for (is = 0;  is < MAX_FILES;  is++)
    {  
      option_data->ort_filename[is] = NULL;
      option_data->ideal_filename[is] = NULL;
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
  FIM_options * option_data        /* fim program options */
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[THD_MAX_NAME];       /* error message */
  int k;                            /* ideal time series index */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strcmp(argv[1], "-help") == 0)  display_help_menu();  

  
  /*----- add to program log -----*/
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
	  if (nopt >= argc)  FIM_error ("need argument after -input ");
	  option_data->input_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->input_filename);
	  strcpy (option_data->input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -mask filename   -----*/
      if (strcmp(argv[nopt], "-mask") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -mask ");
	  option_data->mask_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->mask_filename);
	  strcpy (option_data->mask_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -input1D filename   -----*/
      if (strcmp(argv[nopt], "-input1D") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -input1D ");
	  option_data->input1D_filename = 
	    malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->input1D_filename);
	  strcpy (option_data->input1D_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -nfirst num  -----*/
      if (strcmp(argv[nopt], "-nfirst") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -nfirst ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    FIM_error ("illegal argument after -nfirst ");
	  option_data->NFirst = ival;
	  nopt++;
	  continue;
	}


      /*-----   -nlast num  -----*/
      if (strcmp(argv[nopt], "-nlast") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -nlast ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    FIM_error ("illegal argument after -nlast ");
	  option_data->NLast = ival;
	  nopt++;
	  continue;
	}


      /*-----   -polort num  -----*/
      if (strcmp(argv[nopt], "-polort") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -polort ");
	  sscanf (argv[nopt], "%d", &ival);

#undef PMAX
#ifdef USE_LEGENDRE
# define PMAX 19
#else
# define PMAX  2
#endif

     if ((ival < -1) || (ival > PMAX)) /* ZSS May 08, allowed -1 */
	    FIM_error ("illegal argument after -polort ");

#ifdef USE_LEGENDRE
     if( ival > 2 )
       fprintf(stderr,
            "** WARNING: -polort > 2 is a new untested option: 29 Mar 2005\n") ;
#endif

	  option_data->polort = ival;
	  nopt++;
	  continue;
	}

      
      /*-----   -fim_thr r  -----*/
      if (strcmp(argv[nopt], "-fim_thr") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -fim_thr ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if ((fval < 0.0) || (fval > 1.0))
	    FIM_error ("illegal argument after -fim_thr ");
	  option_data->fim_thr = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -cdisp cval   -----*/
      if (strcmp(argv[nopt], "-cdisp") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -cdisp ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if ((fval < 0.0) || (fval > 1.0))
	    FIM_error ("illegal argument after -cdisp ");
	  option_data->cdisp = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -ort_file sname   -----*/
      if (strcmp(argv[nopt], "-ort_file") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -ort_file");

	  k = option_data->num_ort_files;
	  if (k+1 > MAX_FILES)
	    {
	      sprintf (message, "Too many ( > %d ) ort time series files. ", 
		       MAX_FILES);
	      FIM_error (message);
	    }

	  option_data->ort_filename[k] 
	    = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->ort_filename[k]);
	  strcpy (option_data->ort_filename[k], argv[nopt]);
	  option_data->num_ort_files++;
	  nopt++;
	  continue;
	}
      

      /*-----   -ideal_file rname   -----*/
      if (strcmp(argv[nopt], "-ideal_file") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -ideal_file");

	  k = option_data->num_ideal_files;
	  if (k+1 > MAX_FILES)
	    {
	      sprintf (message, "Too many ( > %d ) ideal time series files. ", 
		       MAX_FILES);
	      FIM_error (message);
	    }

	  option_data->ideal_filename[k] 
	    = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->ideal_filename[k]);
	  strcpy (option_data->ideal_filename[k], argv[nopt]);
	  option_data->num_ideal_files++;
	  nopt++;
	  continue;
	}
      

      /*-----   -out option   -----*/
      if (strcmp(argv[nopt], "-out") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need argument after -out ");
	  for (ival = 0;  ival < MAX_OUTPUT_TYPE;  ival++)
	    if (strcmp(argv[nopt], OUTPUT_TYPE_name[ival]) == 0)  break;
	  if (ival < MAX_OUTPUT_TYPE)
	    {
	      option_data->output_type[ival] = 1;
	      nopt++;
	      continue;
	    }
	  else if (strcmp(argv[nopt], "All") == 0)
	    {
	      for (ival = 0;  ival < MAX_OUTPUT_TYPE-2;  ival++)
		option_data->output_type[ival] = 1;
	      nopt++;
	      continue;
	    }
	  else
	    {
	      sprintf(message,"Unrecognized output type: %s\n", argv[nopt]);
	      FIM_error (message);
	    }
	}
      

      /*-----   -bucket filename   -----*/
      if (strcmp(argv[nopt], "-bucket") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FIM_error ("need file prefixname after -bucket ");
	  option_data->bucket_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->bucket_filename);
	  strcpy (option_data->bucket_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      FIM_error (message);
      
    }

}


/*---------------------------------------------------------------------------*/
/*
  Read one time series from specified file name.  This file name may have
  a column selector attached.
*/

float * read_one_time_series 
(
  char * ts_filename,          /* time series file name (plus column index) */
  int * ts_length              /* output value for time series length */
)

{
  char message[THD_MAX_NAME];    /* error message */
  char * cpt;                    /* pointer to column suffix */
  char subv[THD_MAX_NAME];       /* string containing column index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures 
			      -- used to read 1D ASCII */
  float * far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */
  int iy;                  /* time series file column index */
  int ipt;                 /* time point index */
  float * ts_data;         /* input time series data */


  /*----- Read the time series file -----*/
  flim = mri_read_1D(ts_filename) ;
  if (flim == NULL)
    {                      /* filename -> ts_filename   11 Sep 2003 [rickr] */
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      FIM_error (message);
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
  Read multiple time series from specified file name.  This file name may have
  a column selector attached.
*/

MRI_IMAGE * read_time_series 
(
  char * ts_filename,      /* time series file name (plus column selectors) */
  int ** column_list       /* list of selected columns */
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


  /*----- Read the time series file -----*/
  flim = mri_read_1D(ts_filename) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      FIM_error (message);
    }

  
  /*----- Set pointer to data, and set dimensions -----*/
  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny;
  *column_list = NULL;   /* mri_read_1D does column selection */

  return (flim);
}


/*---------------------------------------------------------------------------*/
/*
  Read the input data files.
*/

void read_input_data
(
  FIM_options * option_data,        /* fim program options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */
  MRI_IMAGE ** ort_array,           /* ort time series arrays */
  int ** ort_list,                  /* list of ort time series */
  MRI_IMAGE ** ideal_array,         /* ideal time series arrays */
  int ** ideal_list                 /* list of ideal time series */
)

{
  char message[THD_MAX_NAME];    /* error message */

  int num_ort_files;       /* number of ort time series files */
  int num_ideal_files;     /* number of ideal time series files */
  int is;                  /* time series file index */
  int polort;              /* degree of polynomial for baseline model */
  int num_ortts;           /* number of ort time series */
  int num_idealts;         /* number of ideal time series */
  int q;                   /* number of parameters in the baseline model */
  int p;                   /* number of parameters in the baseline model 
			      plus number of ideals */


  /*----- Initialize local variables -----*/
  polort          = option_data->polort;
  num_ort_files   = option_data->num_ort_files;
  num_ideal_files = option_data->num_ideal_files;


  /*----- Read the input fMRI measurement data -----*/
  if (option_data->input1D_filename != NULL)
    {
      /*----- Read the input fMRI 1D time series -----*/
      *fmri_data = read_one_time_series (option_data->input1D_filename, 
					 fmri_length);
      if (*fmri_data == NULL)  
	{ 
	  sprintf (message,  "Unable to read time series file: %s", 
		   option_data->input1D_filename);
	  FIM_error (message);
	}  
      *dset_time = NULL;
    }

  else if (option_data->input_filename != NULL)
    {
      /*----- Read the input 3d+time dataset -----*/
      *dset_time = THD_open_dataset (option_data->input_filename);   
      if (!ISVALID_3DIM_DATASET(*dset_time))  
	{ 
	  sprintf (message,  "Unable to open data file: %s", 
		   option_data->input_filename);
	  FIM_error (message);
	}  
      DSET_load(*dset_time); CHECK_LOAD_ERROR(*dset_time);

      if (option_data->mask_filename != NULL)
	{
	  /*----- Read the input mask dataset -----*/
	  *mask_dset = THD_open_dataset (option_data->mask_filename);
	  if (!ISVALID_3DIM_DATASET(*mask_dset))  
	    { 
	      sprintf (message,  "Unable to open mask file: %s", 
		       option_data->mask_filename);
	      FIM_error (message);
	    }  
	  DSET_load(*mask_dset); CHECK_LOAD_ERROR(*mask_dset);
	}
    }
  else
    FIM_error ("Must specify input measurement data");


  /*----- Read the input ort time series files -----*/
  for (is = 0;  is < num_ort_files;  is++)
    {
      ort_array[is] = read_time_series (option_data->ort_filename[is], 
					&(ort_list[is]));

      if (ort_array[is] == NULL)
	{
	  sprintf (message,  "Unable to read ort time series file: %s", 
		   option_data->ort_filename[is]);
	  FIM_error (message);
	}
    }

  
  /*----- Read the input ideal time series files -----*/
  for (is = 0;  is < num_ideal_files;  is++)
    {
      ideal_array[is] = read_time_series (option_data->ideal_filename[is], 
					  &(ideal_list[is]));

      if (ideal_array[is] == NULL)
	{
	  sprintf (message,  "Unable to read ideal time series file: %s", 
		   option_data->ideal_filename[is]);
	  FIM_error (message);
	}
    }

  
  /*----- Count number of ort and number of ideal time series -----*/
  num_ortts = 0;
  for (is = 0;  is < num_ort_files;  is++)
    {
      if (ort_list[is] == NULL)
	num_ortts += ort_array[is]->ny;
      else
	num_ortts += ort_list[is][0];
    }
  q = polort + 1 + num_ortts;

  num_idealts = 0;
  for (is = 0;  is < num_ideal_files;  is++)
    {
      if (ideal_list[is] == NULL)
	num_idealts += ideal_array[is]->ny;
      else
	num_idealts += ideal_list[is][0];
    }
  p = q + num_idealts;

  option_data->num_ortts = num_ortts;
  option_data->num_idealts = num_idealts;
  option_data->q = q;
  option_data->p = p;

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
      FIM_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists "
	       " -- cannot continue!\a\n",
	       new_dset->dblk->diskptr->header_name);
      FIM_error (message);
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
  FIM_options * option_data,       /* fim program options */
  THD_3dim_dataset * dset_time     /* input 3d+time data set */
)

{

  if ((option_data->bucket_filename != NULL)
      && (option_data->input1D_filename == NULL))
    check_one_output_file (dset_time, option_data->bucket_filename);
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs 
(
  FIM_options * option_data,      /* fim program options */
  THD_3dim_dataset * dset_time,   /* input 3d+time data set */
  THD_3dim_dataset * mask_dset,   /* input mask data set */
  int fmri_length,                /* length of input fMRI time series */
  MRI_IMAGE ** ort_array,         /* ort time series arrays */
  MRI_IMAGE ** ideal_array        /* ideal time series arrays */
)

{
  char message[THD_MAX_NAME];  /* error message */
  int is;                  /* ideal index */
  int num_ort_files;       /* number of ort time series files */
  int num_ideal_files;     /* number of ideal time series files */
  int num_idealts;         /* number of ideal time series */
  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */


  /*----- Initialize local variables -----*/
  if (option_data->input1D_filename != NULL)
    nt = fmri_length;
  else
    nt = DSET_NUM_TIMES (dset_time);

  num_ort_files   = option_data->num_ort_files;
  num_ideal_files = option_data->num_ideal_files;
  num_idealts     = option_data->num_idealts;


  NFirst = option_data->NFirst;

  NLast = option_data->NLast;   
  if (NLast > nt-1)  NLast = nt-1;
  option_data->NLast = NLast;

  N = NLast - NFirst + 1;
  option_data->N = N;


  /*----- Check number of ideal time series -----*/
  if (num_idealts < 1)  FIM_error ("No ideal time series?");
  if (num_idealts < 2)  option_data->output_type[FIM_BestIndex] = 0;


  /*----- If mask is used, check for compatible dimensions -----*/
  if (mask_dset != NULL)
    {
      if ( (DSET_NX(dset_time) != DSET_NX(mask_dset))
	   || (DSET_NY(dset_time) != DSET_NY(mask_dset))
	   || (DSET_NZ(dset_time) != DSET_NZ(mask_dset)) )
	{
	  sprintf (message, "%s and %s have incompatible dimensions",
		   option_data->input_filename, option_data->mask_filename);
	  FIM_error (message);
	}

      if (DSET_NVALS(mask_dset) != 1 )
	FIM_error ("Must specify 1 sub-brick from mask dataset");
    }


  /*----- Check lengths of ort time series -----*/
  for (is = 0;  is < num_ort_files;  is++)
    {
      if (ort_array[is]->nx < NLast+1)
	{
	  sprintf (message, "Input ort time series file %s is too short",
		   option_data->ort_filename[is]);
	  FIM_error (message);
	}
    }

 
  /*----- Check lengths of ideal time series -----*/
  for (is = 0;  is < num_ideal_files;  is++)
    {
      if (ideal_array[is]->nx < NLast+1)
	{
	  sprintf (message, "Input ideal time series file %s is too short",
		   option_data->ideal_filename[is]);
	  FIM_error (message);
	}
    }


  /*----- Check whether any of the output files already exist -----*/
  if( THD_deathcon() ) check_output_files (option_data, dset_time);

}


/*---------------------------------------------------------------------------*/
/*
  Allocate memory for output volumes.
*/

void allocate_memory 
(
  FIM_options * option_data,  /* fim program options */
  THD_3dim_dataset * dset,    /* input 3d+time data set */
  float *** fim_params_vol    /* array of volumes containing fim parameters */
)

{
  int ip;                    /* parameter index */
  int nxyz;                  /* total number of voxels */
  int ixyz;                  /* voxel index */


  /*----- Initialize local variables -----*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;


  /*----- Allocate memory space for fim parameters -----*/
  *fim_params_vol  = (float **) malloc (sizeof(float *) * MAX_OUTPUT_TYPE);   
  MTEST(*fim_params_vol);

  for (ip = 0;  ip < MAX_OUTPUT_TYPE;  ip++)
    {
      if (option_data->output_type[ip])
	{
	  (*fim_params_vol)[ip]  = (float *) malloc (sizeof(float) * nxyz);
	  MTEST((*fim_params_vol)[ip]);    
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    {
	      (*fim_params_vol)[ip][ixyz]  = 0.0;
	    }
	}
      else
	(*fim_params_vol)[ip] = NULL;
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
  FIM_options ** option_data,       /* fim algorithm options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */
  MRI_IMAGE ** ort_array,           /* ort time series arrays */
  int ** ort_list,                  /* list of ort time series */
  MRI_IMAGE ** ideal_array,         /* ideal time series arrays */
  int ** ideal_list,                /* list of ideal time series */
  float *** fim_params_vol    /* array of volumes containing fim parameters */
)
     
{
  /*----- Allocate memory -----*/
  *option_data = (FIM_options *) malloc (sizeof(FIM_options));

   
  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Read input data -----*/
  read_input_data (*option_data, dset_time, mask_dset, fmri_data, fmri_length,
		   ort_array, ort_list, ideal_array, ideal_list);


  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (*option_data, *dset_time, *mask_dset, 
			  *fmri_length, ort_array, ideal_array);
  

  /*----- Allocate memory for output volumes -----*/
  if ((*option_data)->input1D_filename == NULL)
    allocate_memory (*option_data, *dset_time, fim_params_vol);

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
  MRI_IMAGE * im;          /* intermediate float data */
  float * ar;              /* pointer to float data */
  int ts_length;           /* length of input 3d+time data set */
  int it;                  /* time index */


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  FIM_error ("Unable to extract data from 3d+time dataset");


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
  int iv,                      /* current voxel index */      
  float * fim_params,          /* array of fim parameters */
  float ** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  int ip;                   /* parameter index */ 


  /*----- Saved user requested fim parameters -----*/
  for (ip = 0;  ip < MAX_OUTPUT_TYPE;  ip++)
    {
      if (fim_params_vol[ip] != NULL)
	fim_params_vol[ip][iv]  = fim_params[ip];
    }

}


/*---------------------------------------------------------------------------*/
/*
  Set fim threshold level.
*/

float set_fim_thr_level 
(
  int NFirst,                /* first usable data point */
  float fim_thr,             /* fim threshold (as proportion of mean) */
  THD_3dim_dataset * dset    /* input 3d+time data set */
)

{
  int nt;                    /* number of time points */
  int nxyz;                  /* total number of voxels */
  int ixyz;                  /* voxel index */
  double sum;                /* sum of voxel intensities */
  float fthr;                /* fim threshold (as intensity level) */
  float * ts_array = NULL;   /* time series data for individual voxel */


  /*----- Initialize local variables -----*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
  nt = DSET_NUM_TIMES (dset);

  ts_array = (float *) malloc (sizeof(float) * nt);
  MTEST (ts_array);

  sum = 0.0;  /* Ides March 2004 [rickr] */
  /*----- Sum values of all voxels at initial time point -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      extract_ts_array (dset, ixyz, ts_array);
      sum += fabs(ts_array[NFirst]);
    }


  /*----- Set fim intensity level threshold -----*/
  fthr = fim_thr * sum / nxyz;


  free (ts_array);   ts_array = NULL;

  return (fthr);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the best cross correlation for each voxel.
*/

void calculate_results 
(
  FIM_options * option_data,  /* fim program options */
  THD_3dim_dataset * dset,    /* input 3d+time data set */
  THD_3dim_dataset * mask,    /* input mask data set */
  float * fmri_data,          /* input fMRI time series data */
  int fmri_length,            /* length of fMRI time series */
  MRI_IMAGE ** ort_array,     /* ort time series arrays */
  int ** ort_list,            /* list of ort time series */
  MRI_IMAGE ** ideal_array,   /* ideal time series arrays */
  int ** ideal_list,          /* list of ideal time series */
  float ** fim_params_vol     /* array of volumes of fim output parameters */
)
  
{
  float * ts_array = NULL;    /* array of measured data for one voxel */
  float mask_val[1];          /* value of mask at current voxel */
  float fthr;                 /* internal mask threshold level */

  int q;                      /* number of parameters in the baseline model */
  int p;                      /* number of parameters in the baseline model 
			         plus number of ideals */
  int m;                      /* parameter index */
  int n;                      /* data point index */


  matrix xdata;               /* independent variable matrix */
  matrix x_base;              /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base;       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix x_ideal[MAX_FILES];  /* extracted X matrices  for ideal models */
  matrix xtxinvxt_ideal[MAX_FILES];     
                              /* matrix:  (1/(X'X))X'  for ideal models */
  vector y;                   /* vector of measured data */       

  int ixyz;                   /* voxel index */
  int nxyz;                   /* number of voxels per dataset */

  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable data points */

  int num_ort_files;       /* number of ort time series files */
  int num_ideal_files;     /* number of ideal time series files */
  int polort;              /* degree of polynomial ort */
  int num_ortts;           /* number of ort time series */
  int num_idealts;         /* number of ideal time series */
  
  int i;                   /* data point index */
  int is;                  /* ideal index */
  int ilag;                /* time lag index */
  float stddev;            /* normalized parameter standard deviation */
  char * label;            /* string containing stat. summary of results */

  float * x_bot = NULL;    /* minimum of stimulus time series */
  float * x_ave = NULL;    /* average of stimulus time series */
  float * x_top = NULL;    /* maximum of stimulus time series */
  int * good_list = NULL;  /* list of good (usable) time points */ 
  float ** rarray = NULL;  /* ranked arrays of ideal time series */
  float FimParams[MAX_OUTPUT_TYPE];  /* output fim parameters */


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&xdata);
  matrix_initialize (&x_base);
  matrix_initialize (&xtxinvxt_base);
  for (is =0;  is < MAX_FILES;  is++)
    {
      matrix_initialize (&x_ideal[is]);
      matrix_initialize (&xtxinvxt_ideal[is]);
    }
  vector_initialize (&y);


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
  p = option_data->p;
  q = option_data->q;

  polort          = option_data->polort;
  num_idealts     = option_data->num_idealts;
  num_ort_files   = option_data->num_ort_files;
  num_ideal_files = option_data->num_ideal_files;
  if (num_idealts > MAX_FILES ||
      num_ort_files > MAX_FILES ||
      num_ideal_files > MAX_FILES ) {
   /* ZSS: I used one ideal file with 30 regressors when MAX_FILE was 20
   and I spent most of the day chasing memory corruption errors. 
   Tested with 20 regressors and all was well so now limit is 200 ZSS May 08 */
   fprintf(stderr,"Error: the number of ideal or ort regressors\n"
                  "exceeds the hard coded limit of %d\n"
                  "Contact authors if you need the limit extended.\n",
                  MAX_FILES);
   exit(0);     
  }

  /*----- Allocate memory -----*/
  ts_array = (float *) malloc (sizeof(float) * nt);      MTEST (ts_array);
  x_bot = (float *)    malloc (sizeof(float) * p);       MTEST (x_bot);
  x_ave = (float *)    malloc (sizeof(float) * p);       MTEST (x_ave);
  x_top = (float *)    malloc (sizeof(float) * p);       MTEST (x_top);
  good_list = (int *)  malloc (sizeof(int) * N);         MTEST (good_list);
  rarray = (float **)  malloc (sizeof(float *) * num_idealts);  MTEST (rarray);


  /*----- Initialize the independent variable matrix -----*/
  N = init_indep_var_matrix (q, p, NFirst, N, polort, 
			     num_ort_files, num_ideal_files, 
			     ort_array, ort_list, ideal_array, ideal_list, 
			     x_bot, x_ave, x_top, good_list, &xdata);
  option_data->N = N;


  /*----- Initialize fim threshold level -----*/
  if (option_data->input1D_filename == NULL)
    fthr = set_fim_thr_level (good_list[0]+NFirst, option_data->fim_thr, dset);


  /*----- Initialization for the regression analysis -----*/
  init_regression_analysis (q, p, N, num_idealts, xdata, &x_base, 
			    &xtxinvxt_base, x_ideal, xtxinvxt_ideal, rarray);


  vector_create (N, &y);

  
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
	    y.elts[i] = fmri_data[good_list[i]+NFirst];
	}
      else
	{
	  extract_ts_array (dset, ixyz, ts_array);
	  if (fabs(ts_array[good_list[0]+NFirst]) < fthr)  
	    continue;
	  for (i = 0;  i < N;  i++)
	    y.elts[i] = ts_array[good_list[i]+NFirst];
	}
      

      /*----- Perform the regression analysis for this voxel-----*/
      regression_analysis (N, q, num_idealts,
			   x_base, xtxinvxt_base, x_ideal, xtxinvxt_ideal, 
			   y, x_bot, x_ave, x_top, rarray, 
			   option_data->output_type, FimParams);


      /*----- Save results for this voxel -----*/
      if (option_data->input1D_filename == NULL)
	save_voxel (ixyz, FimParams, fim_params_vol);
      
      
      /*----- Report results for this voxel -----*/
      if ( ((fabs(FimParams[FIM_Correlation]) > option_data->cdisp) 
	    && (option_data->cdisp >= 0.0))
	   || (option_data->input1D_filename != NULL) )
	{
	  printf ("\n\nResults for Voxel #%d: \n", ixyz);
	  report_results (option_data->output_type, FimParams, &label);
	  printf ("%s \n", label);
	}
      
    }  /*----- Loop over voxels -----*/
  

  /*----- Dispose of matrices and vectors -----*/
  vector_destroy (&y);
  for (is = 0;  is < MAX_FILES;  is++)
    {
      matrix_destroy (&xtxinvxt_ideal[is]);
      matrix_destroy (&x_ideal[is]);
    } 
  matrix_destroy (&xtxinvxt_base);
  matrix_destroy (&x_base);
  matrix_destroy (&xdata);


  /*----- Deallocate memory -----*/
  free (ts_array);     ts_array = NULL;
  free (x_bot);        x_bot = NULL;
  free (x_ave);        x_ave = NULL;
  free (x_top);        x_top = NULL;
  free (good_list);    good_list = NULL;
  for (is = 0;  is < num_idealts;  is++)
    {
      free (rarray[is]);   rarray[is] = NULL;
    }
  free (rarray);       rarray = NULL;
  
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
  int nsam, 
  int nfit, 
  int nort,                 /* degrees of freedom */
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

  if (brick_type == FUNC_COR_TYPE)
    EDIT_BRICK_TO_FICO (new_dset, ibrick, nsam, nfit, nort);
  
  
  /*----- attach bar[ib] to be sub-brick #ibrick -----*/
  EDIT_substitute_brick (new_dset, ibrick, MRI_short, bar[ibrick]);

}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one bucket data set.
*/

void write_bucket_data 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  FIM_options * option_data,        /* fim program options */
  float ** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  THD_3dim_dataset * old_dset = NULL;      /* prototype dataset */
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  char output_prefix[THD_MAX_NAME];     /* prefix name for bucket dataset */
  char output_session[THD_MAX_NAME];    /* directory for bucket dataset */
  int nbricks;              /* number of sub-bricks in bucket dataset */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */

  int brick_type;           /* indicates statistical type of sub-brick */
  int brick_coef;           /* regression coefficient index for sub-brick */
  char brick_label[THD_MAX_NAME]; /* character string label for sub-brick */

  int ierror;               /* number of errors in editing data */
  float * volume;           /* volume of floating point data */

  int N;                    /* number of usable data points */
  int q;                    /* number of parameters in the ideal model */
  int num_idealts;           /* number of ideal time series */
  int ip;                   /* parameter index */
  int nxyz;                 /* total number of voxels */
  int ibrick;               /* sub-brick index */
  int nsam; 
  int nfit; 
  int nort;                 /* degrees of freedom */
  char label[THD_MAX_NAME];   /* general label for sub-bricks */


  /*----- read prototype dataset -----*/
  old_dset = THD_open_dataset (option_data->input_filename);
                     /* ZSS May 08, changed from THD_open_one_dataset  */

    
  /*----- Initialize local variables -----*/
  nxyz = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;  
  num_idealts = option_data->num_idealts;
  q = option_data->q;
  N = option_data->N;


  /*----- Calculate number of sub-bricks in the bucket -----*/
  nbricks = 0;
  for (ip = 0;  ip < MAX_OUTPUT_TYPE;  ip++)
    if (option_data->output_type[ip])  nbricks++;


  strcpy (output_prefix, option_data->bucket_filename);
  strcpy (output_session, "./");
  
 
  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * nbricks);
  MTEST (bar);
  

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
			    ADN_datum_all,       MRI_short ,   
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
  
  if (!THD_ok_overwrite() && THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      fprintf(stderr,
	      "*** Output dataset file %s already exists--cannot continue!\n",
	      DSET_HEADNAME(new_dset));
      exit(1);
    }
  

  /*----- Attach individual sub-bricks to the bucket dataset -----*/
  ibrick = 0;
  for (ip = 0;  ip < MAX_OUTPUT_TYPE;  ip++)        
    {                                 
      if (option_data->output_type[ip] == 0)  continue;

      strcpy (brick_label, OUTPUT_TYPE_name[ip]);

      if (ip == FIM_Correlation)
	{
	  brick_type = FUNC_COR_TYPE;
	  nsam = N;  nort = q;
	  if (num_idealts > 1)  nfit = 2;
	  else                  nfit = 1;
	}
      else if (ip == FIM_SpearmanCC)
	{
#if 0
	  brick_type = FUNC_THR_TYPE;
#else
	  brick_type = FUNC_COR_TYPE;
#endif
	  nsam = N;  nort = q;
	  if (num_idealts > 1)  nfit = 2;
	  else                  nfit = 1;
	} 
      else if (ip == FIM_QuadrantCC)
	{
#if 0
	  brick_type = FUNC_THR_TYPE;
#else
	  brick_type = FUNC_COR_TYPE;
#endif
	  nsam = N;  nort = q;
	  if (num_idealts > 1)  nfit = 2;
	  else                  nfit = 1;
	}
      else
	{
	  brick_type = FUNC_FIM_TYPE;
	  nsam = 0;  nfit = 0;  nort = 0;
	}

      volume = fim_params_vol[ip];		  
      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			brick_type, brick_label, nsam, nfit, nort, bar);  

      ibrick++;
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
  Write out the user requested output files.
*/

void output_results
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  FIM_options * option_data,        /* fim algorithm options */
  float ** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  int q;                    /* number of parameters in baseline model */
  int num_idealts;           /* number of ideal time series */
  int ib;                   /* sub-brick index */
  int is;                   /* ideal index */
  int ts_length;            /* length of impulse reponse function */
  int N;                    /* number of usable data points */


  /*----- Initialize local variables -----*/
  q = option_data->polort + 1;
  num_idealts = option_data->num_idealts;
  N = option_data->N;


  /*----- Write the bucket dataset -----*/
  if (option_data->bucket_filename != NULL)
    write_bucket_data (argc, argv, option_data, fim_params_vol);

}


/*---------------------------------------------------------------------------*/

void terminate_program
(
  FIM_options ** option_data,   /* fim program options */
  MRI_IMAGE ** ort_array,           /* ort time series arrays */
  int ** ort_list,                  /* list of ort time series */
  MRI_IMAGE ** ideal_array,         /* ideal time series arrays */
  int ** ideal_list,                /* list of ideal time series */
  float *** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  int num_idealts;           /* number of ideal time series */
  int ip;                   /* parameter index */
  int is;                   /* ideal index */


  /*----- Initialize local variables -----*/
  num_idealts = (*option_data)->num_idealts;


  /*----- Deallocate memory for option data -----*/   
  free (*option_data);  *option_data = NULL;


  /*----- Deallocate memory for ideal time series -----*/
  /*
  for (is = 0;  is < num_idealts;  is++)
    { free (ideal[is]);  ideal[is] = NULL; } 
  */


  /*----- Deallocate space for volume data -----*/
  if (*fim_params_vol != NULL)
    {
      for (ip = 0;  ip < MAX_OUTPUT_TYPE;  ip++)
	{
	  if ((*fim_params_vol)[ip] != NULL)
	    { free ((*fim_params_vol)[ip]);   (*fim_params_vol)[ip] = NULL; }
	}

      free (*fim_params_vol);   *fim_params_vol  = NULL; 
    }

}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  FIM_options * option_data;              /* fim algorithm options */
  THD_3dim_dataset * dset_time = NULL;    /* input 3d+time data set */
  THD_3dim_dataset * mask_dset = NULL;    /* input mask data set */
  float * fmri_data = NULL;               /* input fMRI time series data */
  int fmri_length;                        /* length of fMRI time series */
  MRI_IMAGE * ort_array[MAX_FILES];       /* ideal time series arrays */
  int * ort_list[MAX_FILES];              /* list of ideal time series */
  MRI_IMAGE * ideal_array[MAX_FILES];     /* ideal time series arrays */
  int * ideal_list[MAX_FILES];            /* list of ideal time series */

  float ** fim_params_vol = NULL;
                                /* array of volumes of fim output parameters */

  
  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

   PRINT_VERSION("3dfim+") ; AUTHOR(PROGRAM_AUTHOR) ;
   mainENTRY("3dfim+ main") ; machdep() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

  enable_mcw_malloc();
  
  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, &dset_time, &mask_dset, 
		      &fmri_data, &fmri_length, 
		      ort_array, ort_list, ideal_array, ideal_list, 
		      &fim_params_vol);


  /*----- Perform fim analysis -----*/
  calculate_results (option_data, dset_time, mask_dset, 
		     fmri_data, fmri_length,
		     ort_array, ort_list, ideal_array, ideal_list, 
		     fim_params_vol);
  
  /*----- Deallocate memory for input datasets -----*/   
  if (dset_time != NULL)  
    { THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL; }
  if (mask_dset != NULL)  
    { THD_delete_3dim_dataset (mask_dset, False);  mask_dset = NULL; }


  /*----- Write requested output files -----*/
  if (option_data->input1D_filename == NULL)
    output_results (argc, argv, option_data, fim_params_vol);


  /*----- Terminate program -----*/
  terminate_program (&option_data, 
		     ort_array, ort_list, ideal_array, ideal_list, 
		     &fim_params_vol); 

  exit(0);
}









