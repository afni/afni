/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1998-2003, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Program to calculate the deconvolution of a measurement 3d+time dataset
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

  Mod:     Allow fitting of baseline alone (i.e., no input stimulus functions
           are required).  Also, removed restriction on length of input time
           series.
  Date:    31 December 1998

  Mod:     Accept mean square error from full model.
  Date:    04 January 1999

  Mod:     Incorporated THD_extract_series routine.
  Date:    19 April 1999

  Mod:     Change NLast default value.
  Date:    27 May 1999

  Mod:     Added "no data" option.
  Date:    10 June 1999

  Mod:     Add use of the argument list extension routine addto_args
           to allow the last switch '-@' to get further command line
           arguments from stdin (RWC)
  Date:    28 June 1999

  Mod:     Added option for matrix calculation of general linear tests.
  Date:    02 July 1999

  Mod:     Increased max. allowed number of input stimulus functions.
  Date:    24 August 1999

  Mod:     Additional statistical output (partial R^2 statistics).
  Date:    07 September 1999

  Mod:     Added changes for incorporating History notes.
  Date:    09 September 1999

  Mod:     Use cubic spline interpolation to time shift the estimated 
           impulse  response function, in order to correct for differences 
           in slice acquisition times.
  Date:    27 October 1999

  Mod:     Allow reading of multiple input stimulus functions from a single
           file by selection of individual columns.
  Date:    09 November 1999

  Mod:     Automatic removal of input stimulus functions which consist of 
           all zeros.
  Date:    10 November 1999

  Mod:     Added options to allow operator more control over amount and 
           contents of output bucket dataset. (-fout, -rout, -tout) 
  Date:    11 November 1999

  Mod:     Added option to output the sample variance (MSE) for the 
           full model. (-vout)
  Date:    12 November 1999

  Mod:     Added options for writing the fitted full model time series (-fitts)
           and the residual error time series (-errts) to 3d+time datasets.
  Date:    22 November 1999

  Mod:     Added option to perform analysis on a single (fMRI) measurement
           time series instead of a 3d+time dataset (-input1D).
  Date:    23 November 1999

  Mod:     Added test for maximum number of full model parameters.
  Date:    24 November 1999

  Mod:     Added test for minimum IRF length for compatibility with -tshift
           option.  Also, added necessary duplicate initialization of total 
           number of parameters in the full model.
  Date:    29 November 1999

  Mod:     Increased maximum number of GLT matrices and linear constraints.
  Date:    03 January 2000

  Mod:     Added -mask option to speed up processing by performing calculations
           on brain voxels only.
  Date:    11 January 2000  

  Mod:     Modified matrix_file_read to use mri_read_ascii routine.
  Date:    12 January 2000

  Mod:     Added test for missing stim function time series file name.
  Date:    08 May 2000

  Mod:     Added -censor1D option to allow operator to eliminate individual
           time points from the analysis.
  Date:    21 June 2000

  Mod:     Added screen display of p-values.
  Date:    22 June 2000

  Mod:     Added -xout option for writing the X design matrix as well as
           the (X'X) inverse matrix to the screen.
  Date:    22 June 2000

  Mod:     Added -glt_label option for labeling the GLT matrix.
  Date:    23 June 2000

  Mod:     Added -concat option for analysis of a concatenated 3d+time dataset.
  Date:    26 June 2000

  Mod:     Increased max. allowed number of input stimulus functions, GLT's,
           and linear constraints per GLT.  Also, increased size of screen
	   output buffer.
  Date:    27 July 2000

  Mod:     Additional output with -nodata option (norm.std.dev.'s for
           GLT linear constraints).
  Date:    11 August 2000

  Mod:     Added -nocout option, to suppress the fit coefficient output.
  Date:    08 September 2000

  Mod:     Added -stim_nptr option, to allow input stim functions which are
           sampled at a multiple of the 1/TR rate.
  Date:    03 January 2001 

  Mod:     Added error message if user requests bucket dataset output with
           no sub-bricks.
  Date:    10 January 2001

  Mod:     Set slice offset times to 0 if -tshift option is used.
  Date:    11 January 2001

  Mod:     Increased max. number of input stimulus time series.
  Date:    19 March 2001 

  Mod:     Extended -iresp, -sresp, -fitts, and -errts output options 
           to work with -input1D input option.
  Date:    03 May 2001

  Mod:     Changes to eliminate constraints on number of stimulus time series,
           number of regressors, number of GLT's, and number of GLT linear 
           constraints.  Added -num_glt option.
  Date:    10 May 2001

  Mod:     Changes to reduce the volume memory allocation requirements; i.e.,
           a better match between what is saved and what is needed for output.
  Date:    14 May 2001

  Mod:     Removed automatic override of -nfirst option.
  Date:    08 June 2001

  Mod:     Added -progress option to print periodic progress reports.
  Date:    14 June 2001

  Mod:     Minor changes to input error checking routine.
  Date:    06 July 2001

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Corrected error in the baseline t-stat output.
  Date:    26 September 2001

  Mod:     Extended -bucket output option to work with -input1D input option.
  Date:    16 Oct 2001

  Mod:     Changed initialization of nt from -nlast input option.  This
           required major changes in read_input_data routine.  Also, added
	   protection against invalid -concat inputs in check_for_valid_inputs
	   routine.
  Date:    23 January 2002

  Mod:     Enhanced screen output:  Display of p-values for individual stim
           function regression coefficients.  Display of t-stats and p-values
           for individual linear constraints within a GLT.
  Date:    29 January 2002

  Mod:     Modified input routines to use THD_makemask.
  Date:    29 January 2002

  Mod:     Extended -tout option to write a t-statistic sub-brick for each
           of the GLT linear combinations (i.e., each row of each GLT matrix).
  Date:    08 February 2002

  Mod:     Allow user to specify no baseline parameters in the model with
           command "-polort -1".
  Date:    26 February 2002

  Mod:     Added -nobout option, to suppress the bucket dataset output
           of baseline parameters and statistics.
  Date:    27 February 2002

  Mod:     Allow user to specify which input stimulus functions are part of
           the baseline model.
  Date:    02 May 2002

  Mod:     Corrected problem in allocate_memory routine, which would result in
           memory error if both -stim_base and -nobout options are used 
           simultaneously.
  Date:    10 October 2002

  Mod:     Increased size of screen output buffer (again).
  Date:    02 December 2002

  Mod:     Added "multi-get" feature with USE_GET macro -- RWCox.
  Date:    27 Feb 2003

  Mod:     If FLOATIZE is defined, uses floats instead of doubles -- RWCox.
  Date:    03 Mar 2003 (triple trinity day)

  Mod:     Added -quiet option to suppress initial screen output.
  Date:    12 March 2003

  Mod:     Changes to allow -jobs option to run multiple processes.
  Date:    04 May 2003 -- RWCox

  Mod:     Suppress final timing output without -jobs option.
  Date:    15 August 2003 -- rickr

  Mod:     In check_for_valid_inputs(), instead of failing when a stim_length
           is too short, zeropad the time series.  To turn this off, look
           for the ALLOW_EXTEND macro in this code.
  Date     22 Oct 2003 -- RWCox

  Mod:     Various checks for bad matrix input:
            * duplicate -stim_file input filenames
            * collinear column pairs, and all zero columnns
            * matrix condition numbers
           Also - disable 3dDeconvolve_f (FLOATIZE)
  Date:    14 Jul 2004 - RWCox

*/

/*---------------------------------------------------------------------------*/

#ifndef FLOATIZE
# define PROGRAM_NAME   "3dDeconvolve"               /* name of this program */
#else
# define PROGRAM_NAME   "3dDeconvolve_f"             /* name of this program */
#endif

#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "02 September 1998"   /* initial program release date*/
#define PROGRAM_LATEST  "04 May 2003"         /* latest program revision date*/

/*---------------------------------------------------------------------------*/

#define RA_error DC_error

#if 1             /* set this to 0 to disable multiple gets */
#define USE_GET   /* RWCox: extract multiple timeseries at once for speed */
#endif

/*---------------------------------------------------------------------------*/

#include "mrilib.h"

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
  static int proc_shmsize       = 0   ; /* total size of shared memory */

  static int proc_shm_arnum     = 0   ; /* num arrays in shared memory */
  static float ***proc_shm_ar   = NULL; /* *proc_shm_ar[i] = ptr to array #i */
  static int *proc_shm_arsiz    = NULL; /* proc_shm_arsiz[i] = floats in #i */

  static int proc_vox_bot[PROC_MAX]   ; /* 1st voxel to use in each process */
  static int proc_vox_top[PROC_MAX]   ; /* last voxel (+1) in each process */

  static int proc_ind                 ; /* index of THIS job */

#else   /* can't use multiple processes */

# define proc_numjob 1   /* flag that only 1 process is in use */
# define proc_ind    0   /* index of THIS job */

#endif

  static int proc_use_jobs      = 0   ; /* jobs opt given - 2003.08.15[rickr] */

/*---------------------------------------------------------------------------*/

#ifndef FLOATIZE
# include "matrix.h"          /* double precision */
#else
# include "matrix_f.h"        /* single precision */
#endif

#include "Deconvolve.c"

/*---------------------------------------------------------------------------*/

typedef struct DC_options
{ 
  int nxyz;                /* number of voxels in the input dataset */
  int nt;                  /* number of input 3d+time dataset time points */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable data points from input data */
  int polort;              /* degree of polynomial for baseline model */
  float rms_min;           /* minimum rms error to reject reduced model */
  int quiet;               /* flag to suppress screen output */  
  int progress;            /* print periodic progress reports */
  float fdisp;             /* minimum f-statistic for display */ 
  char * input_filename;   /* input 3d+time dataset */
  char * mask_filename;    /* input mask dataset */
  char * input1D_filename; /* input fMRI measurement time series */
  char * censor_filename;  /* input censor time series filename */
  char * concat_filename;  /* filename for list of concatenated runs */
  int nodata;              /* flag for 'no data' option */
  int p;                   /* number of parameters in the full model */
  int q;                   /* number of parameters in the baseline model */
  int qp;                  /* number of polynomial trend parameters 
			      in the baseline model  Note: qp <= q <= p  */
  int nbricks;             /* number of sub-bricks in bucket dataset output */

  int num_stimts;          /* number of stimulus time series */
  char ** stim_filename;   /* input stimulus time series */
  char ** stim_label;      /* label for stimulus time series */
  int * stim_base;         /* flag for stim fn. is part of baseline model */
  int * stim_minlag;       /* min. time lag for impulse response */
  int * stim_maxlag;       /* max. time lag for impulse response */
  int * stim_nptr;         /* number of stim fn. points per TR */

  int num_glt;             /* number of general linear tests */
  int * glt_rows;          /* number of linear constraints in glt */
  char ** glt_filename;    /* file containing glt matrix */
  char ** glt_label;       /* label for general linear tests */

  char * bucket_filename;  /* bucket dataset file name */
  char ** iresp_filename;  /* impulse response 3d+time output */
  char ** sresp_filename;  /* std. dev. 3d+time output */
  char * fitts_filename;   /* fitted time series 3d+time output */
  char * errts_filename;   /* error time series 3d+time output */

  int tshift;           /* flag to time shift the impulse response */
  int fout;             /* flag to output F-statistics */
  int rout;             /* flag to output R^2 statistics */
  int tout;             /* flag to output t-statistics */
  int vout;             /* flag to output variance map */
  int nobout;           /* flag to suppress output of baseline coefficients */
  int nocout;           /* flag to suppress output of fit coefficients */
  int xout;             /* flag to write X and inv(X'X) matrices to screen */
  int full_first;       /* flag to output full model stats first */

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
  
void identify_software ()
{
    
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
}

/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dDeconvolve help menu.
*/

void display_help_menu()
{
  identify_software();

  printf (
"Program to calculate the deconvolution of a measurement 3d+time dataset    \n"
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
    PROGRAM_NAME "\n"
    "                                                                       \n"
    "     Input data and control options:                                   \n"
    "-input fname         fname = filename of 3d+time input dataset         \n"
    "[-input1D dname]     dname = filename of single (fMRI) .1D time series \n"
    "[-nodata]            Evaluate experimental design only (no input data) \n"
    "[-mask mname]        mname = filename of 3d mask dataset               \n"
    "[-censor cname]      cname = filename of censor .1D time series        \n"
    "[-concat rname]      rname = filename for list of concatenated runs    \n"
    "[-nfirst fnum]       fnum = number of first dataset image to use in the\n"
    "                       deconvolution procedure. (default = max maxlag) \n"
    "[-nlast  lnum]       lnum = number of last dataset image to use in the \n"
    "                       deconvolution procedure. (default = last point) \n"
    "[-polort pnum]       pnum = degree of polynomial corresponding to the  \n"
    "                       null hypothesis  (default: pnum = 1)            \n"
    "[-rmsmin r]          r = minimum rms error to reject reduced model     \n"
    "                                                                       \n"
    "     Input stimulus options:                                           \n"
    "-num_stimts num      num = number of input stimulus time series        \n"
    "                       (0 <= num)   (default: num = 0)                 \n"
    "-stim_file k sname   sname = filename of kth time series input stimulus\n"
    "[-stim_label k slabel] slabel = label for kth input stimulus           \n"
    "[-stim_base k]       kth input stimulus is part of the baseline model  \n"
    "[-stim_minlag k m]   m = minimum time lag for kth input stimulus       \n"
    "                       (default: m = 0)                                \n"
    "[-stim_maxlag k n]   n = maximum time lag for kth input stimulus       \n"
    "                       (default: n = 0)                                \n"
    "[-stim_nptr k p]     p = number of stimulus function points per TR     \n"
    "                       Note: This option requires 0 slice offset times \n"
    "                       (default: p = 1)                                \n"
    "                                                                       \n"
    "     General linear test (GLT) options:                                \n"
    "-num_glt num         num = number of general linear tests              \n"
    "                       (0 <= num)   (default: num = 0)                 \n"
    "[-glt s gltname]     Perform s simultaneous linear tests, as specified \n"
    "                       by the matrix contained in file gltname         \n"
    "[-glt_label k glabel]  glabel = label for kth general linear test      \n"
    "                                                                       \n"
    "     Options for output 3d+time datasets:                              \n"
    "[-iresp k iprefix]   iprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the kth estimated impulse response \n"
    "[-tshift]            Use cubic spline interpolation to time shift the  \n"
    "                       estimated impulse response function, in order to\n"
    "                       correct for differences in slice acquisition    \n"
    "                       times. Note that this effects only the 3d+time  \n"
    "                       output dataset generated by the -iresp option.  \n"
    "[-sresp k sprefix]   sprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the standard deviations of the     \n"
    "                       kth impulse response function parameters        \n"
    "[-fitts  fprefix]    fprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the (full model) time series fit   \n"
    "                       to the input data                               \n"
    "[-errts  eprefix]    eprefix = prefix of 3d+time output dataset which  \n"
    "                       will contain the residual error time series     \n"
    "                       from the full model fit to the input data       \n"
    "                                                                       \n"
    "     Options to control the contents of the output bucket dataset:     \n"
    "[-fout]            Flag to output the F-statistics                     \n"
    "[-rout]            Flag to output the R^2 statistics                   \n"
    "[-tout]            Flag to output the t-statistics                     \n"
    "[-vout]            Flag to output the sample variance (MSE) map        \n"
    "[-nobout]          Flag to suppress output of baseline coefficients    \n"
    "                     (and associated statistics)                       \n"
    "[-nocout]          Flag to suppress output of regression coefficients  \n"
    "                     (and associated statistics)                       \n"
    "[-full_first]      Flag to specify that the full model statistics will \n"
    "                     appear first in the bucket dataset output         \n"
    "[-bucket bprefix]  Create one AFNI 'bucket' dataset containing various \n"
    "                     parameters of interest, such as the estimated IRF \n"
    "                     coefficients, and full model fit statistics.      \n"
    "                     Output 'bucket' dataset is written to bprefix.    \n"
    "                                                                       \n"
    "     The following options control the screen output only:             \n"
    "[-quiet]             Flag to suppress initial screen output            \n"
    "[-xout]              Flag to write X and inv(X'X) matrices to screen   \n"
    "[-progress n]        Write statistical results for every nth voxel     \n"
    "[-fdisp fval]        Write statistical results for those voxels        \n"
    "                       whose full model F-statistic is > fval          \n"
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
            "             http://afni.nimh.nih.gov/afni/doc/misc/parallize.html \n"
            "         * Use -mask to get more speed; cf. 3dAutomask.\n"
          , PROC_MAX ) ;
#endif

    printf("\n"
           "** NOTE **\n"
           "This version of the program has been compiled to use\n"
#ifdef FLOATIZE
           "single precision arithmetic for most internal calculations.\n"
#else
           "double precision arithmetic for most internal calculations.\n"
#endif
          ) ;
  
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


  /*----- Initialize default values -----*/
  option_data->nxyz     = -1;
  option_data->nt       = -1;
  option_data->NFirst   = -1;
  option_data->NLast    = -1;
  option_data->N        = 0;
  option_data->polort   = 1;
  option_data->rms_min  = 0.0;
  option_data->quiet    = 0;
  option_data->progress = 0;
  option_data->fdisp    = -1.0;
  option_data->nodata   = 0;
  option_data->p        = 0;
  option_data->q        = 0;
  option_data->qp       = 0;
  option_data->nbricks  = 0;
  
  /*----- Initialize stimulus options -----*/
  option_data->num_stimts = 0;
  option_data->stim_filename = NULL;
  option_data->stim_label = NULL;
  option_data->stim_base = NULL;
  option_data->stim_minlag = NULL;
  option_data->stim_maxlag = NULL;
  option_data->stim_nptr = NULL;
  option_data->iresp_filename = NULL;
  option_data->sresp_filename = NULL;

  /*----- Initialize glt options -----*/
  option_data->num_glt = 0;
  option_data->glt_filename = NULL;
  option_data->glt_label = NULL;
  option_data->glt_rows = NULL;

  /*----- Initialize output flags -----*/
  option_data->tshift = 0;
  option_data->fout = 0;
  option_data->rout = 0;
  option_data->tout = 0;
  option_data->vout = 0;
  option_data->xout = 0;
  option_data->nobout = 0;
  option_data->nocout = 0;
  option_data->full_first = 0;

  /*----- Initialize character strings -----*/
  option_data->input_filename = NULL;
  option_data->mask_filename = NULL;  
  option_data->input1D_filename = NULL;
  option_data->censor_filename = NULL;
  option_data->concat_filename = NULL;
  option_data->bucket_filename = NULL;
  option_data->fitts_filename = NULL;
  option_data->errts_filename = NULL;

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
  option_data->stim_label = (char **) malloc (sizeof(char *) * num_stimts);
  MTEST (option_data->stim_label);
  option_data->stim_base = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_base);
  option_data->stim_minlag = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_minlag);
  option_data->stim_maxlag = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_maxlag);
  option_data->stim_nptr   = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->stim_nptr);
  option_data->iresp_filename = (char **) malloc (sizeof(char *) * num_stimts);
  MTEST (option_data->iresp_filename);
  option_data->sresp_filename = (char **) malloc (sizeof(char *) * num_stimts);
  MTEST (option_data->sresp_filename);


  /*----- Initialize stimulus options -----*/
  for (is = 0;  is < num_stimts;  is++)
    {  
      option_data->stim_filename[is] = NULL;
      option_data->stim_label[is] = malloc (sizeof(char)*THD_MAX_NAME);
      MTEST (option_data->stim_label[is]);
      sprintf (option_data->stim_label[is], "Stim #%d ", is+1);

      option_data->stim_base[is] = 0;
      option_data->stim_minlag[is]   = 0;
      option_data->stim_maxlag[is]   = 0;
      option_data->stim_nptr[is]     = 1;

      option_data->iresp_filename[is] = NULL;
      option_data->sresp_filename[is] = NULL;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the general linear test options.
*/
 
void initialize_glt_options 
(
  DC_options * option_data,   /* deconvolution program options */
  int num_glt                 /* number of general linear tests */
)
 
{
  int iglt;                   /* glt index */


  /*----- Set number of general linear tests -----*/
  if (num_glt <= 0)  return;
  else  option_data->num_glt = num_glt;


  /*----- Allocate memory for glt options -----*/
  option_data->glt_filename = (char **) malloc (sizeof(char *) * num_glt);
  MTEST (option_data->glt_filename);
  option_data->glt_label = (char **) malloc (sizeof(char *) * num_glt);
  MTEST (option_data->glt_label);
  option_data->glt_rows = (int *) malloc (sizeof(int) * num_glt);
  MTEST (option_data->glt_rows);


  /*----- Initialize glt options -----*/
  for (iglt = 0;  iglt < num_glt;  iglt++)
    {  
      option_data->glt_filename[iglt] = NULL;
      option_data->glt_label[iglt] = malloc (sizeof(char)*THD_MAX_NAME);
      MTEST (option_data->glt_label[iglt]);
      sprintf (option_data->glt_label[iglt], "GLT #%d ", iglt+1);
      option_data->glt_rows[iglt] = 0;
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
  char message[THD_MAX_NAME];       /* error message */
  int k;                            /* stimulus time series index */
  int s;                            /* number of linear constraints in GLT */
  int iglt = 0;                     /* general linear test index */


  /*-- addto the arglist, if user wants to --*/
  machdep() ; /* RWCox: 20 Apr 2001 */
  { int new_argc ; char ** new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  

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
	  if (nopt >= argc)  DC_error ("need argument after -input ");
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
	  if (nopt >= argc)  DC_error ("need argument after -mask ");
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
	  if (nopt >= argc)  DC_error ("need argument after -input1D ");
	  option_data->input1D_filename = 
	    malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->input1D_filename);
	  strcpy (option_data->input1D_filename, argv[nopt]);
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
      

      /*-----   -nodata   -----*/
      if (strcmp(argv[nopt], "-nodata") == 0)
	{
	  option_data->nodata = 1;
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


      /*-----   -quiet   -----*/
      if (strcmp(argv[nopt], "-quiet") == 0)
	{
	  option_data->quiet = 1;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -progress n  -----*/
      if (strcmp(argv[nopt], "-progress") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -progress ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    DC_error ("illegal argument after -progress ");
	  option_data->progress = ival;
	  nopt++;
	  continue;
	}

      
      /*-----   -rmsmin r  -----*/
      if (strcmp(argv[nopt], "-rmsmin") == 0)
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
      if (strcmp(argv[nopt], "-fdisp") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -fdisp ");
	  sscanf (argv[nopt], "%f", &fval); 
	  option_data->fdisp = fval;
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
      

      /*-----   -stim_label k slabel   -----*/
      if (strcmp(argv[nopt], "-stim_label") == 0)
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
      

      /*-----   -stim_base k   -----*/
      if (strcmp(argv[nopt], "-stim_base") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  
	    DC_error ("need 1 argument after -stim_base");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-stim_base k   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  option_data->stim_base[k] = 1;
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
      

      /*-----   -num_glt num  -----*/
      if (strcmp(argv[nopt], "-num_glt") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need argument after -num_glt ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0)
	    {
	      DC_error ("-num_glt num   Require: num >= 0 ");
	    }

	  if (option_data->num_glt > 0)
	    DC_error ("-num_glt option must precede any -glt options ");

	  initialize_glt_options (option_data, ival);
	 
	  nopt++;
	  continue;
	}

      
      /*-----   -glt s gltname   -----*/
      if (strcmp(argv[nopt], "-glt") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -glt");

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 1)
	    {
	      DC_error ("-glt s gltname  Require: s >= 1  (s = #rows in GLT)");
	    }
	  s = ival;

	  if (option_data->num_glt == 0) 
	    initialize_glt_options (option_data, 10);
	      
	  if (iglt+1 > option_data->num_glt)    
	    DC_error ("Use -num_glt option to specify number of GLT's ");

	  option_data->glt_rows[iglt] = s;
	  nopt++;

	  option_data->glt_filename[iglt] 
	    = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->glt_filename[iglt]);
	  strcpy (option_data->glt_filename[iglt], 
		  argv[nopt]);
	  iglt++;
	  
	  nopt++;
	  continue;
	}
      

      /*-----   -glt_label k glabel   -----*/
      if (strcmp(argv[nopt], "-glt_label") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -glt_label");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_glt))
	    DC_error ("-stim_label k slabel   Require: 1 <= k <= num_glt");
	  k = ival-1;
	  nopt++;

	  strcpy (option_data->glt_label[k], argv[nopt]);
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


      /*-----   -tshift   -----*/
      if (strcmp(argv[nopt], "-tshift") == 0)
	{
	  option_data->tshift = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -sresp k sprefix   -----*/
      if (strcmp(argv[nopt], "-sresp") == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  DC_error ("need 2 arguments after -sresp");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > option_data->num_stimts))
	    DC_error ("-sresp k iprefix   Require: 1 <= k <= num_stimts");
	  k = ival-1;
	  nopt++;

	  option_data->sresp_filename[k] 
	    = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->sresp_filename[k]);
	  strcpy (option_data->sresp_filename[k], argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -fout   -----*/
      if (strcmp(argv[nopt], "-fout") == 0)
	{
	  option_data->fout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -rout   -----*/
      if (strcmp(argv[nopt], "-rout") == 0)
	{
	  option_data->rout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -tout   -----*/
      if (strcmp(argv[nopt], "-tout") == 0)
	{
	  option_data->tout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -vout   -----*/
      if (strcmp(argv[nopt], "-vout") == 0)
	{
	  option_data->vout = 1;
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
      

      /*-----   -nobout   -----*/
      if (strcmp(argv[nopt], "-nobout") == 0)
	{
	  option_data->nobout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -nocout   -----*/
      if (strcmp(argv[nopt], "-nocout") == 0)
	{
	  option_data->nocout = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -full_first   -----*/
      if (strcmp(argv[nopt], "-full_first") == 0)
	{
	  option_data->full_first = 1;
	  nopt++;
	  continue;
	}
      

      /*-----   -bucket filename   -----*/
      if (strcmp(argv[nopt], "-bucket") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need file prefixname after -bucket ");
	  option_data->bucket_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->bucket_filename);
	  strcpy (option_data->bucket_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -fitts filename   -----*/
      if (strcmp(argv[nopt], "-fitts") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  DC_error ("need file prefixname after -fitts ");
	  option_data->fitts_filename = malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (option_data->fitts_filename);
	  strcpy (option_data->fitts_filename, argv[nopt]);
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

      /*-----   -jobs J   -----*/
      if( strcmp(argv[nopt],"-jobs") == 0 ){   /* RWCox */
        nopt++ ;
        if (nopt >= argc)  DC_error ("need J parameter after -jobs ");
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
        proc_use_jobs = 1 ;     /* -jobs opt given    2003.08.15 [rickr] */
        nopt++; continue;
      }
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      DC_error (message);
      
    }


  /*----- Set number of GLT's -----*/
  option_data->num_glt = iglt;

  /*---- if -jobs is given, make sure are processing 3D data ----*/

#ifdef PROC_MAX
  if( proc_numjob > 1 && option_data->input1D_filename != NULL )
    proc_numjob = 1 ;
#endif

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
  flim = mri_read_1D( ts_filename ) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      DC_error (message);
    }
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
  byte ** mask_vol,                 /* input mask volume */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */
  float ** censor_array,            /* input censor time series array */
  int * censor_length,              /* length of censor time series */
  int ** block_list,                /* list of block (run) starting points */
  int * num_blocks,                 /* number of blocks (runs) */
  float *** stimulus,               /* stimulus time series arrays */
  int ** stim_length,               /* length of stimulus time series */
  matrix ** glt_cmat                /* general linear test matrices */
)

{
  char message[THD_MAX_NAME];    /* error message */
  int it;                  /* time point index */
  int nt;                  /* number of input data time points */
  int nxyz;                /* number of voxels */
  int num_stimts;          /* number of stimulus time series arrays */
  int * baseline;          /* flag for baseline stimulus function */
  int * min_lag;           /* minimum time delay for impulse response */
  int * max_lag;           /* maximum time delay for impulse response */
  int qp;                  /* number of polynomial trend baseline parameters */
  int q;                   /* number of baseline model parameters */
  int p;                   /* number of full model parameters */
  int is;                  /* stimulus time series index */
  int num_glt;             /* number of general linear tests */
  int iglt;                /* general linear test index */


  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;
  num_glt    = option_data->num_glt;
  baseline = option_data->stim_base;
  min_lag  = option_data->stim_minlag;
  max_lag  = option_data->stim_maxlag;


  /*----- Read the input stimulus time series -----*/
  if (num_stimts > 0)
    {
      *stimulus = (float **) malloc (sizeof(float *) * num_stimts);
      MTEST (*stimulus);
      *stim_length = (int *) malloc (sizeof(int) * num_stimts);
      MTEST (*stim_length);

      /** 14 Jul 2004: check for duplicate filename - RWCox **/

      { int js ;
        for( is=0 ; is < num_stimts ; is++ ){
          for( js=is+1 ; js < num_stimts ; js++ ){
            if( strcmp( option_data->stim_filename[is] ,
                        option_data->stim_filename[js]  ) == 0 )
              fprintf(stderr,"** -stim_file ERROR: "
                             "#%d '%s' same as #%d '%s'\n" ,
                      is+1,option_data->stim_filename[is] ,
                      js+1,option_data->stim_filename[js]  ) ;
          }
        }
      }

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


  /*----- Read the input fMRI measurement data -----*/
  if (option_data->nodata)
    {
      /*----- No input data -----*/
      if (num_stimts <= 0)  
	DC_error ("Must have num_stimts > 0 for -nodata option");

      *dset_time = NULL;
      nt = (*stim_length)[0] / option_data->stim_nptr[0];
      nxyz = 0;
    }

  else if (option_data->input1D_filename != NULL)
    {
      /*----- Read the input fMRI 1D time series -----*/
      *fmri_data = read_time_series (option_data->input1D_filename, 
				     fmri_length);
      if (*fmri_data == NULL)  
	{ 
	  sprintf (message,  "Unable to read time series file: %s", 
		   option_data->input1D_filename);
	  DC_error (message);
	}  
      *dset_time = NULL;
      nt = *fmri_length;
      nxyz = 1;
   }

  else if (option_data->input_filename != NULL)
    {
      /*----- Read the input 3d+time dataset -----*/
      *dset_time = THD_open_one_dataset (option_data->input_filename);
      if (!ISVALID_3DIM_DATASET(*dset_time))  
	{ 
	  sprintf (message,  "Unable to open data file: %s", 
		   option_data->input_filename);
	  DC_error (message);
	}  
      THD_load_datablock ((*dset_time)->dblk);
      nt = DSET_NUM_TIMES (*dset_time);
      nxyz = DSET_NVOX (*dset_time);


      if (option_data->mask_filename != NULL)
	{
	  THD_3dim_dataset * mask_dset = NULL;

	  /*----- Read the input mask dataset -----*/
	  mask_dset = THD_open_dataset (option_data->mask_filename);
	  if (!ISVALID_3DIM_DATASET(mask_dset))  
	    { 
	      sprintf (message,  "Unable to open mask file: %s", 
		       option_data->mask_filename);
	      DC_error (message);
	    }  

	  /*----- If mask is used, check for compatible dimensions -----*/
	  if ( (DSET_NX(*dset_time) != DSET_NX(mask_dset))
	       || (DSET_NY(*dset_time) != DSET_NY(mask_dset))
	       || (DSET_NZ(*dset_time) != DSET_NZ(mask_dset)) )
	    {
	      sprintf (message, "%s and %s have incompatible dimensions",
		      option_data->input_filename, option_data->mask_filename);
	      DC_error (message);
	    }
	  
	  if (DSET_NVALS(mask_dset) != 1 )
	    DC_error ("Must specify 1 sub-brick from mask dataset");
     
	  *mask_vol = THD_makemask( mask_dset , 0 , 1.0,0.0 ) ;
	  if (*mask_vol == NULL)  DC_error ("Unable to read mask dataset");

	  DSET_delete(mask_dset) ; 
	}
    }
  else
    DC_error ("Must specify input data");


  /*----- Check number of data points -----*/
  if (nt <= 0)      DC_error ("No time points?");
  option_data->nt = nt;
  if (nxyz < 0)     DC_error ("Program initialization error: nxyz < 0");
  option_data->nxyz = nxyz;


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
  qp = (option_data->polort + 1) * (*num_blocks);
  q = qp;
  p = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      if (max_lag[is] < min_lag[is])
	DC_error ("Require min lag <= max lag for all stimuli");
      p += max_lag[is] - min_lag[is] + 1;
      if (baseline[is])  q += max_lag[is] - min_lag[is] + 1;
    }
  option_data->p  = p;
  option_data->q  = q;
  option_data->qp = qp;
  

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
      

  /*----- Read the general linear test matrices -----*/
  if (num_glt > 0)
    {
      *glt_cmat = (matrix *) malloc (sizeof(matrix) * num_glt);
      
      /*----- Initialize general linear test matrices -----*/
      for (iglt = 0;  iglt < num_glt;  iglt++)
	matrix_initialize (&((*glt_cmat)[iglt]));


      for (iglt = 0;  iglt < num_glt;  iglt++)
	{
	  matrix_file_read (option_data->glt_filename[iglt],
			    option_data->glt_rows[iglt],
			    p, &((*glt_cmat)[iglt]), 1);
	  if ((*glt_cmat)[iglt].elts == NULL)
	    { 
	      sprintf (message,  "Unable to read GLT matrix from file: %s", 
		       option_data->glt_filename[iglt]);
	      DC_error (message);
	    }  
	} 
    }

}


/*---------------------------------------------------------------------------*/
/*
  Check whether any of the input stimulus functions consists of all zeros.
  Remove any trace of all-zero stimulus functions.
*/

void remove_zero_stimfns
(
  DC_options * option_data,         /* deconvolution program options */
  float ** stimulus,                /* stimulus time series arrays */
  int * stim_length,                /* length of stimulus time series */
  matrix * glt_cmat                 /* general linear test matrices */
)

{
  int num_stimts;          /* number of stimulus time series arrays */
  int is, isp;             /* stimulus time series index */
  int it;                  /* time point index */
  int num_glt;             /* number of general linear tests */
  int iglt;                /* general linear test index */
  int all_zero;            /* boolean for stim function contains all zeros */


  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;
  num_glt    = option_data->num_glt;


  /*----- Loop over all stimulus funcitons -----*/
  is = 0;
  while (is < num_stimts)
    {
      /*----- Check whether stim function consists of all zeros -----*/
      all_zero = TRUE;
      for (it = 0;  it < stim_length[is];  it++)
	{
	  if (stimulus[is][it] != 0.0)
	    {
	      all_zero = FALSE;
	      break;
	    }
	}

      if (all_zero)  /*----- Remove this stimulus function -----*/
	{
	  printf ("Warning!  Stimulus function %s consists of all zeros! \n",
		 option_data->stim_filename[is]);
	  if (option_data->num_glt > 0)
	    DC_error 
	      ("Cannot process -glt option when stim function is all zero");

	  option_data->p -=  
	    option_data->stim_maxlag[is] - option_data->stim_minlag[is] + 1;
	  if (option_data->stim_base[is])
	    option_data->q -=  
	      option_data->stim_maxlag[is] - option_data->stim_minlag[is] + 1;
	  for (isp = is;  isp < num_stimts-1;  isp++)
	    {
	      stimulus[isp] = stimulus[isp+1];
	      stim_length[isp] = stim_length[isp+1];
	      option_data->stim_filename[isp] 
		= option_data->stim_filename[isp+1];
	      option_data->stim_label[isp] = option_data->stim_label[isp+1];
	      option_data->stim_base[isp] 
		= option_data->stim_base[isp+1];
	      option_data->stim_minlag[isp] = option_data->stim_minlag[isp+1];
	      option_data->stim_maxlag[isp] = option_data->stim_maxlag[isp+1];
	      option_data->iresp_filename[isp] 
		= option_data->iresp_filename[isp+1];
	      option_data->sresp_filename[isp] 
		= option_data->sresp_filename[isp+1];
	    }

	  num_stimts--;
	  option_data->num_stimts = num_stimts;
	}
      else
	is++;
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
  int is;                         /* stimulus time series index */


  if (option_data->bucket_filename != NULL)   
    check_one_output_file (dset_time, option_data->bucket_filename);

  if (option_data->fitts_filename != NULL)   
    check_one_output_file (dset_time, option_data->fitts_filename);

  if (option_data->errts_filename != NULL)   
    check_one_output_file (dset_time, option_data->errts_filename);
  
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
  THD_3dim_dataset * dset_time,   /* input 3d+time data set */
  int fmri_length,                /* length of input fMRI time series */
  float * censor_array,           /* input censor time series array */
  int censor_length,              /* length of censor array */
  int * block_list,               /* list of block (run) starting points */
  int num_blocks,                 /* number of blocks (runs) */
  int * stim_length,              /* length of stimulus time series arrays */
  float **stimulus ,              /* 22 Oct 2003: stimulus time series arrays */
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
  int qp;                  /* number of polynomial trend baseline parameters */
  int q;                   /* number of baseline model parameters */
  int p;                   /* number of full model parameters */
  int nbricks;             /* number of sub-bricks in bucket dataset output */
  int it;                  /* time point index */
  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */
  int ib;                  /* block (run) index */
  int irb;                 /* time index relative to start of block (run) */
  int num_glt;             /* number of general linear tests */
  int * glt_rows;          /* number of linear constraints in glt */
  int iglt;                /* general linear test index */
  int nerr=0 ;             /* 22 Oct 2003 */


  /*----- Initialize local variables -----*/
  nt = option_data->nt;
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;
  num_glt = option_data->num_glt;
  glt_rows = option_data->glt_rows;
  nptr = option_data->stim_nptr;
  p  = option_data->p;
  q  = option_data->q;
  qp = option_data->qp;


  /*----- Check length of censor array -----*/
  if (censor_length < nt)
    {
      sprintf (message, "Input censor time series file %s is too short",
	       option_data->censor_filename);
      DC_error (message);
    }


  /*----- Check validity of concatenated runs list -----*/
  for (ib = 0;  ib < num_blocks;  ib++)
    if ((block_list[ib] < 0) || (block_list[ib] >= nt))
      {
	sprintf (message, "Invalid -concat input: %d ", block_list[ib]);
	DC_error (message);
      }
  if (num_blocks > 1)
    for (ib = 1;  ib < num_blocks;  ib++)
      if (block_list[ib] <= block_list[ib-1])
	DC_error ("Invalid concatenated runs list");
    
  
  /*----- Create list of good (usable) time points -----*/
  *good_list = (int *) malloc (sizeof(int) * nt);  MTEST (*good_list);
  NFirst = option_data->NFirst;
  if (NFirst < 0)
    for (is = 0;  is < num_stimts;  is++)
      if (NFirst < (max_lag[is]+nptr[is]-1)/nptr[is])  
	NFirst = (max_lag[is]+nptr[is]-1)/nptr[is];
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


  /*----- Check number of stimulus time series -----*/
  if (num_stimts < 0)
    {
      DC_error ("Require: 0 <= num_stimts ");
    }


  /*----- Check lengths of stimulus time series -----*/

#define ALLOW_EXTEND
  for (is = 0;  is < num_stimts;  is++)
    {
      if (stim_length[is] < nt*nptr[is])
	{
#ifndef ALLOW_EXTEND
	  sprintf (message, "Input stimulus time series file %s is too short",
		   option_data->stim_filename[is]);
	  DC_error (message);
#else
          int nlen=nt*nptr[is], qq ;
          fprintf(stderr,
                  "++ WARNING: input stimulus time series file %s is too short:\n"
                  "            length = %d, but should be at least %d.\n" ,
            option_data->stim_filename[is] , stim_length[is] , nlen ) ;
          stimulus[is] = (float *) realloc( stimulus[is] , sizeof(float)*nlen ) ;
          for( qq=stim_length[is] ; qq < nlen ; qq++ ) stimulus[is][qq] = 0.0 ;
          stim_length[is] = nlen ; nerr++ ;
#endif
	}
    }
#ifdef ALLOW_EXTEND
    if( nerr > 0 ){
      char *eee = getenv("AFNI_3dDeconvolve_extend") ;
      if( eee != NULL && (*eee=='n' || *eee=='N') ){
        fprintf(stderr,"** ERROR: Can't continue with too short files!\n"
                       "          AFNI_3dDeconvolve_extend = %s\n",eee ) ;
        exit(1) ;
      }
      fprintf(stderr,"++ EXTENDING short files with zero values\n"
                     "   (to stop this behavior, setenv AFNI_3dDeconvolve_extend NO)\n") ;
    }
#endif


  /*----- Check whether time lags are reasonable -----*/
  for (is = 0;  is < num_stimts;  is++)
    {

      m = max_lag[is] - min_lag[is] + 1;
      if (m < 2)
	{
	  if (option_data->iresp_filename[is] != NULL)
	    {
	      sprintf (message, "Only %d time point for output dataset %s ", 
		       m, option_data->iresp_filename[is]);
	      DC_error (message);
	    }

	  if (option_data->sresp_filename[is] != NULL)
	    {
	      sprintf (message, "Only %d time point for output dataset %s", 
		       m, option_data->sresp_filename[is]);
	      DC_error (message);
	    }
	}	    
      if ((m < 4) && (option_data->tshift))
	{
	  if (option_data->iresp_filename[is] != NULL)
	    {
	      sprintf (message, "Only %d time points for 3d+time dataset %s\n",
 		       m, option_data->iresp_filename[is]);
	      strcat (message, "Require >= 4 data points for -tshift option");
	      DC_error (message);
	    }
	}	    
    }


  /*----- Calculate number of sub-bricks in the bucket dataset,
          and check for illegal number of sub-bricks -----*/
  nbricks = 0;
  if (option_data->bucket_filename != NULL)
    {
      if (! option_data->nocout)
	{
	  if (! option_data->nobout)
	    nbricks += qp * (1 + option_data->tout);

	  for (is = 0;  is < num_stimts;  is++)
	    {
	      if ((!option_data->stim_base[is]) || (!option_data->nobout))
		{
		  m = max_lag[is] - min_lag[is] + 1;
		  nbricks += m * (1 + option_data->tout);
		  nbricks += option_data->rout + option_data->fout;
		}
	    }
	}
	 
      nbricks += option_data->rout + option_data->fout + option_data->vout;

      if (num_glt > 0)
	for (iglt = 0;  iglt < num_glt;  iglt++)
	  {
	    nbricks += glt_rows[iglt] * (1 + option_data->tout);
	    nbricks += option_data->rout + option_data->fout;
	  }
      
      if (nbricks <= 0)
	{
	  sprintf (message, 
		   "User requested bucket dataset with only %d sub-bricks",
		   nbricks);
	  DC_error (message);
	}
      
    }
  option_data->nbricks = nbricks;
      

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


  /*----- Check for -tshift and -input1D option -----*/
  if ((option_data->tshift) && (option_data->input1D_filename != NULL))
    DC_error ("-tshift option is not compatible with -input1D option");


  /*----- Check whether any of the output files already exist -----*/
  if (option_data->input_filename != NULL)
    check_output_files (option_data, dset_time);

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

void proc_finalize_shm_volumes(void)
{
   char kstr[32] ; int ii ;

   if( proc_shm_arnum == 0 ) return ;   /* should never happen */

   proc_shmsize = 0 ;                       /* add up sizes of */
   for( ii=0 ; ii < proc_shm_arnum ; ii++ ) /* all arrays for */
     proc_shmsize += proc_shm_arsiz[ii] ;   /* shared memory */

   proc_shmsize *= sizeof(float) ;          /* convert to byte count */

   /* create shared memory segment */

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
         unsigned int smax=0 ;
         fscanf(fp,"%u",&smax) ; pclose(fp) ;
         if( smax > 0 )
           fprintf(stderr ,
                   "\n"
                   "** POSSIBLY USEFUL ADVICE:\n"
                   "** Current max shared memory size = %u bytes.\n"
                   "** For information on how to change this, see\n"
                   "**   http://afni.nimh.nih.gov/afni/doc/misc/parallize.html \n"
                   "** and also contact your system administrator.\n"
                   , smax ) ;
        }
       }
     }
     exit(1) ;
   }

   /* set a signal handler to catch most fatal errors and
      delete the shared memory segment if program crashes */

   signal(SIGPIPE,proc_sigfunc) ; signal(SIGSEGV,proc_sigfunc) ;
   signal(SIGINT ,proc_sigfunc) ; signal(SIGFPE ,proc_sigfunc) ;
   signal(SIGBUS ,proc_sigfunc) ; signal(SIGHUP ,proc_sigfunc) ;
   signal(SIGTERM,proc_sigfunc) ; signal(SIGILL ,proc_sigfunc) ;
   signal(SIGKILL,proc_sigfunc) ; signal(SIGPIPE,proc_sigfunc) ;
   atexit(proc_atexit) ;   /* or when the program exits */

   fprintf(stderr , "++ Shared memory: %d bytes at id=%d\n" ,
           proc_shmsize , proc_shmid ) ;

   /* get pointer to shared memory segment we just created */

   proc_shmptr = shm_attach( proc_shmid ) ; /* thd_iochan.c */
   if( proc_shmptr == NULL ){
     fprintf(stderr,"\n** Can't attach to shared memory!?\n"
                      "** This is bizarre.\n" ) ;
     shmctl( proc_shmid , IPC_RMID , NULL ) ;
     exit(1) ;
   }

   /* clear all shared memory */

   memset( proc_shmptr , 0 , proc_shmsize ) ;

   /* fix the local pointers to arrays in shared memory */

   *proc_shm_ar[0] = (float *) proc_shmptr ;
   for( ii=1 ; ii < proc_shm_arnum ; ii++ )
     *proc_shm_ar[ii] = *proc_shm_ar[ii-1] + proc_shm_arsiz[ii] ;
}

/*-------------------------------------------------------------*/
/*** This function replaces free();
     it won't try to free things stored in the shared memory ***/

void proc_free( void *ptr )
{
   int ii ;

   if( ptr == NULL ) return ;
   if( proc_shmid == 0 ){ free(ptr); return; }  /* no shm */
   for( ii=0 ; ii < proc_shm_arnum ; ii++ )
     if( ((float *)ptr) == *proc_shm_ar[ii] ) return;
   free(ptr); return;
}

#undef  free            /* replace use of library free() */
#define free proc_free  /* with proc_free() function     */

#endif /* PROC_MAX */

/*---------------------------------------------------------------------------*/
/*
  Allocate memory for output volumes.
*/

void allocate_memory 
(
  DC_options * option_data,  /* deconvolution algorithm options */

  float *** coef_vol,        /* array of volumes of signal model parameters */
  float *** scoef_vol,       /* array of volumes of parameter std. devs. */
  float *** tcoef_vol,       /* array of volumes of parameter t-statistics */
  float *** fpart_vol,       /* array of volumes of partial F-statistics */
  float *** rpart_vol,       /* array of volumes of partial R^2 stats. */
  float ** mse_vol,          /* volume of full model mean square error */
  float ** ffull_vol,        /* volume of full model F-statistics */
  float ** rfull_vol,        /* volume of full model R^2 stats. */

  float **** glt_coef_vol,   /* volumes for GLT linear combinations */
  float **** glt_tcoef_vol,  /* volumes for GLT t-statistics */
  float ***  glt_fstat_vol,  /* volumes for GLT F-statistics */
  float ***  glt_rstat_vol,  /* volumes for GLT R^2 stats. */

  float *** fitts_vol,       /* volumes for full model fit to input data */
  float *** errts_vol        /* volumes for residual errors */
)

{
  int ip;                  /* parameter index */
  int qp;                  /* number of polynomial trend baseline parameters */
  int q;                   /* number of baseline model parameters */
  int p;                   /* number of full model parameters */
  int nxyz;                /* total number of voxels */
  int ixyz;                /* voxel index */
  int is;                  /* stimulus index */
  int num_stimts;          /* number of stimulus time series */
  int num_glt;             /* number of general linear tests */
  int iglt;                /* general linear test index */
  int nlc;                 /* number of linear combinations in a GLT */
  int ilc;                 /* linear combination index */
  int it;                  /* time point index */
  int nt;                  /* number of images in input 3d+time dataset */
  int * min_lag;           /* minimum time delay for impulse response */
  int * max_lag;           /* maximum time delay for impulse response */

  int fout;             /* flag to output F-statistics */
  int rout;             /* flag to output R^2 statistics */
  int tout;             /* flag to output t-statistics */
  int vout;             /* flag to output variance map */
  int bout;             /* flag to output baseline coefficients */
  int cout;             /* flag to output fit coefficients */


  /*----- Initialize local variables -----*/
  nxyz = option_data->nxyz;
  num_stimts = option_data->num_stimts;
  num_glt = option_data->num_glt;
  qp = option_data->qp;
  q  = option_data->q;
  p  = option_data->p;
  nt = option_data->nt;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;

  fout  = option_data->fout;
  rout  = option_data->rout;
  tout  = option_data->tout;
  vout  = option_data->vout;
  bout  = 1 - (option_data->nobout || option_data->nocout);
  cout  = 1 - option_data->nocout;


  /*----- Allocate memory space for volume data -----*/
  *coef_vol  = (float **) malloc (sizeof(float *) * p);   MTEST(*coef_vol);
  *scoef_vol = (float **) malloc (sizeof(float *) * p);   MTEST(*scoef_vol);
  *tcoef_vol = (float **) malloc (sizeof(float *) * p);   MTEST(*tcoef_vol);
  for (ip = 0;  ip < p;  ip++)
    {
      (*coef_vol)[ip] = NULL;
      (*scoef_vol)[ip] = NULL;
      (*tcoef_vol)[ip] = NULL;
    }

  if (bout)
    for (ip = 0;  ip < qp;  ip++)
      {
	zero_fill_volume (&((*coef_vol)[ip]),  nxyz);
	if (tout) zero_fill_volume (&((*tcoef_vol)[ip]),  nxyz);
      }

  ip = qp - 1;
  for (is = 0;  is < num_stimts;  is++)
    {
      for (it = min_lag[is];  it <= max_lag[is];  it++)
	{
	  ip++;
	  if (option_data->stim_base[is])
	    {
	      if (bout || (option_data->iresp_filename[is] != NULL))
		zero_fill_volume (&((*coef_vol)[ip]),  nxyz);	  
	      if (bout && tout)
		zero_fill_volume (&((*tcoef_vol)[ip]), nxyz);
	    }
	  else
	    {
	      if (cout || (option_data->iresp_filename[is] != NULL))
		zero_fill_volume (&((*coef_vol)[ip]),  nxyz);	  
	      if (cout && tout)
		zero_fill_volume (&((*tcoef_vol)[ip]), nxyz);
	    }
	  if (option_data->sresp_filename[is] != NULL)
	    zero_fill_volume (&((*scoef_vol)[ip]), nxyz);
	}
    }


  if (fout) 
    {
      *fpart_vol = (float **) malloc (sizeof(float *) * num_stimts);
      MTEST(*fpart_vol);
      for (is = 0;  is < num_stimts;  is++)
	if ((! option_data->stim_base[is]) || (! option_data->nobout))
	  zero_fill_volume (&((*fpart_vol)[is]), nxyz);
	else
	  (*fpart_vol)[is] = NULL;
    }


  if (rout)
    {
      *rpart_vol = (float **) malloc (sizeof(float *) * num_stimts);
      MTEST(*rpart_vol);
      for (is = 0;  is < num_stimts;  is++)
	if ((! option_data->stim_base[is]) || (! option_data->nobout))
	  zero_fill_volume (&((*rpart_vol)[is]), nxyz);
	else
	  (*rpart_vol)[is] = NULL;
    }


  if (vout)  zero_fill_volume (&(*mse_vol),   nxyz);
  if (fout)  zero_fill_volume (&(*ffull_vol), nxyz);
  if (rout)  zero_fill_volume (&(*rfull_vol), nxyz);


  /*----- Allocate memory space for GLT volume data -----*/
  if (num_glt > 0)
    {
      *glt_coef_vol  = (float ***) malloc (sizeof(float **) * num_glt);
       MTEST(*glt_coef_vol); 
       
       if (tout)
	 {
	   *glt_tcoef_vol  = (float ***) malloc (sizeof(float **) * num_glt);
	   MTEST(*glt_tcoef_vol);
	 } 

       if (fout)
	 {
	   *glt_fstat_vol = (float **)  malloc (sizeof(float *)  * num_glt);
	   MTEST(*glt_fstat_vol); 
	 }

       if (rout)
	 {
	   *glt_rstat_vol = (float **)  malloc (sizeof(float *)  * num_glt);
	   MTEST(*glt_rstat_vol);
	 }
      
      for (iglt = 0;  iglt < num_glt;  iglt++)
	{
	  nlc = option_data->glt_rows[iglt];

	  (*glt_coef_vol)[iglt] = (float **) malloc (sizeof(float *) * nlc);   
	  MTEST((*glt_coef_vol)[iglt]);

	  if (tout)
	    {
	      (*glt_tcoef_vol)[iglt]
		= (float **) malloc (sizeof(float *) * nlc);   
	      MTEST((*glt_tcoef_vol)[iglt]);
	    }

	  for (ilc = 0;  ilc < nlc;  ilc++)
	    {
	      zero_fill_volume (&((*glt_coef_vol)[iglt][ilc]),  nxyz);
	      if (tout)
		zero_fill_volume (&((*glt_tcoef_vol)[iglt][ilc]),  nxyz);
	    }
	  
	  if (fout)  zero_fill_volume (&((*glt_fstat_vol)[iglt]),  nxyz);
	  if (rout)  zero_fill_volume (&((*glt_rstat_vol)[iglt]),  nxyz);
	}
    }


  /*----- Allocate memory for fitted time series -----*/
  if (option_data->fitts_filename != NULL)
    {
      *fitts_vol = (float **) malloc (sizeof(float **) * nt);
      MTEST (*fitts_vol);
      for (it = 0;  it < nt;  it++)
	{
	  zero_fill_volume (&((*fitts_vol)[it]),  nxyz);
	}
    }

  /*----- Allocate memory for residual errors -----*/
  if (option_data->errts_filename != NULL)
    {
      *errts_vol = (float **) malloc (sizeof(float **) * nt);
      MTEST (*errts_vol);
      for (it = 0;  it < nt;  it++)
	{
	  zero_fill_volume (&((*errts_vol)[it]),  nxyz);
	}
    }

#ifdef PROC_MAX
  if( proc_numjob > 1 ) proc_finalize_shm_volumes() ;  /* RWCox */
#endif
}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  DC_options ** option_data,        /* deconvolution algorithm options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  byte ** mask_vol,                 /* input mask volume */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */
  float ** censor_array,            /* input censor time series array */
  int * censor_length,              /* length of censor time series */
  int ** good_list,                 /* list of usable time points */
  int ** block_list,                /* list of block (run) starting points */
  int * num_blocks,                 /* number of blocks (runs) */
  float *** stimulus,               /* stimulus time series arrays */
  int ** stim_length,               /* length of stimulus time series */
  matrix ** glt_cmat,               /* general linear test matrices */

  float *** coef_vol,        /* array of volumes of signal model parameters */
  float *** scoef_vol,       /* array of volumes of parameter std. devs. */
  float *** tcoef_vol,       /* array of volumes of parameter t-statistics */
  float *** fpart_vol,       /* array of volumes of partial F-statistics */
  float *** rpart_vol,       /* array of volumes of partial R^2 stats. */
  float ** mse_vol,          /* volume of full model mean square error */
  float ** ffull_vol,        /* volume of full model F-statistics */
  float ** rfull_vol,        /* volume of full model R^2 stats. */

  float **** glt_coef_vol,   /* volumes for GLT linear combinations */
  float **** glt_tcoef_vol,  /* volumes for GLT t-statistics */
  float ***  glt_fstat_vol,  /* volumes for GLT F-statistics */
  float ***  glt_rstat_vol,  /* volumes for GLT R^2 stats. */

  float *** fitts_vol,       /* volumes for full model fit to input data */
  float *** errts_vol        /* volumes for residual errors */
)
     
{
  int iglt;                  /* general linear test index */


  /*----- Allocate memory -----*/
  *option_data = (DC_options *) malloc (sizeof(DC_options));

   
  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Identify software -----*/
  if (!(*option_data)->quiet)  identify_software();


  /*----- Read input data -----*/
  read_input_data (*option_data, dset_time, mask_vol, fmri_data, fmri_length,
		   censor_array, censor_length, block_list, num_blocks, 
		   stimulus, stim_length, glt_cmat);


  /*----- Remove all-zero stimulus functions -----*/
  remove_zero_stimfns (*option_data, *stimulus, *stim_length, *glt_cmat);
 

  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (*option_data, *dset_time, 
			  *fmri_length, *censor_array, *censor_length, 
			  *block_list, *num_blocks, *stim_length, *stimulus, good_list);
  

  /*----- Allocate memory for output volumes -----*/
  if (!(*option_data)->nodata)
    allocate_memory (*option_data, coef_vol, scoef_vol, tcoef_vol, 
		     fpart_vol, rpart_vol, mse_vol, ffull_vol, rfull_vol,
		     glt_coef_vol, glt_tcoef_vol, glt_fstat_vol, glt_rstat_vol,
		     fitts_vol, errts_vol);

}


#ifndef USE_GET
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
  if (im == NULL)  DC_error ("Unable to extract data from 3d+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
  ts_length = DSET_NUM_TIMES (dset_time);
  ar = MRI_FLOAT_PTR (im);
#if 0
  for (it = 0;  it < ts_length;  it++)
    {
      ts_array[it] = ar[it];
    }
#else
  memcpy( ts_array , ar , sizeof(float)*ts_length ) ;  /* RWCox */
#endif


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;

}
#endif /* USE_GET */


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
  float * rpart,               /* array of partial R^2 stats. */ 
  float mse,                   /* full model mean square error */
  float ffull,                 /* full model F-statistic */
  float rfull,                 /* full model R^2 stat. */
  vector * glt_coef,           /* linear combinations from GLT matrices */
  vector * glt_tcoef,          /* t-statistics for the general linear tests */
  float * fglt,                /* F-statistics for the general linear tests */
  float * rglt,                /* R^2 stats. for the general linear tests */
  int nt,                      /* number of images in input 3d+time dataset */
  float * ts_array,            /* array of measured data for one voxel */
  int * good_list,             /* list of usable time points */
  float * fitts,               /* full model fitted time series */
  float * errts,               /* full model residual error time series */

  float ** coef_vol,        /* array of volumes of signal model parameters */
  float ** scoef_vol,       /* array of volumes of parameter std. devs. */
  float ** tcoef_vol,       /* array of volumes of parameter t-statistics */
  float ** fpart_vol,       /* array of volumes of partial F-statistics */
  float ** rpart_vol,       /* array of volumes of partial R^2 stats. */
  float * mse_vol,          /* volume of full model mean square error */
  float * ffull_vol,        /* volume of full model F-statistics */
  float * rfull_vol,        /* volume of full model R^2 stats. */
  float *** glt_coef_vol,   /* volumes for GLT linear combinations */
  float *** glt_tcoef_vol,  /* volumes for GLT t-statistics */
  float **  glt_fstat_vol,  /* volumes for GLT F-statistics */
  float **  glt_rstat_vol,  /* volumes for GLT R^2 stats. */
  float ** fitts_vol,       /* volumes for full model fit to input data */
  float ** errts_vol        /* volumes for residual errors */

)

{
  int ip;                   /* parameter index */ 
  int p;                    /* total number of parameters */
  int is;                   /* stimulus time series index */
  int num_stimts;           /* number of stimulus time series */
  int num_glt;              /* number of general linear tests */
  int * glt_rows;           /* number of linear constraints in glt */
  int iglt;                 /* general linear test index */
  int ilc;                  /* linear combination index */
  int it;                    /* time point index */
  int N;                     /* number of usable data points */


  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;
  p = option_data->p;
  num_glt = option_data->num_glt;
  glt_rows = option_data->glt_rows;
  N = option_data->N;


  /*----- Saved regression coefficients, std.dev.'s, and t-statistics -----*/
  if (coef_vol != NULL)
    for (ip = 0;  ip < p;  ip++)
      if (coef_vol[ip] != NULL)
	coef_vol[ip][iv]   = coef.elts[ip];
  if (scoef_vol != NULL)
    for (ip = 0;  ip < p;  ip++)
      if (scoef_vol[ip] != NULL)
	scoef_vol[ip][iv]  = scoef.elts[ip];
  if (tcoef_vol != NULL)
    for (ip = 0;  ip < p;  ip++)
      if (tcoef_vol[ip] != NULL)
	tcoef_vol[ip][iv]  = tcoef.elts[ip];


  /*----- Save partial F-statistics and R^2 statistics -----*/
  if (fpart_vol != NULL)
    for (is = 0;  is < num_stimts;  is++)
      if (fpart_vol[is] != NULL)
	fpart_vol[is][iv] = fpart[is];
  if (rpart_vol != NULL)
    for (is = 0;  is < num_stimts;  is++)
      if (rpart_vol[is] != NULL)
	rpart_vol[is][iv] = rpart[is];
  

  /*----- Save full model mean square error -----*/
  if (mse_vol != NULL)  mse_vol[iv] = mse;


  /*----- Save regression F-statistic -----*/
  if (ffull_vol != NULL)  ffull_vol[iv] = ffull;


  /*----- Save R^2 values -----*/
  if (rfull_vol != NULL)  rfull_vol[iv] = rfull;


  /*----- If general linear test -----*/
  if (num_glt > 0)
    {
      /*----- Save linear combinations -----*/
      if (glt_coef_vol != NULL)
	for (iglt = 0;  iglt < num_glt;  iglt++)
	  if (glt_coef_vol[iglt] != NULL)
	    for (ilc = 0;  ilc < glt_rows[iglt];  ilc++) 
	      glt_coef_vol[iglt][ilc][iv] = glt_coef[iglt].elts[ilc];

      /*----- Save GLT t-statistics -----*/
      if (glt_tcoef_vol != NULL)
	for (iglt = 0;  iglt < num_glt;  iglt++)
	  if (glt_tcoef_vol[iglt] != NULL)
	    for (ilc = 0;  ilc < glt_rows[iglt];  ilc++) 
	      glt_tcoef_vol[iglt][ilc][iv] = glt_tcoef[iglt].elts[ilc];

      /*----- Save GLT F-statistics -----*/
      if (glt_fstat_vol != NULL)
	for (iglt = 0;  iglt < num_glt;  iglt++)
	  if (glt_fstat_vol[iglt] != NULL)
	    glt_fstat_vol[iglt][iv] = fglt[iglt];

      /*----- Save GLT R^2 statistics -----*/
      if (glt_rstat_vol != NULL)
	for (iglt = 0;  iglt < num_glt;  iglt++)
	  if (glt_rstat_vol[iglt] != NULL)
	    glt_rstat_vol[iglt][iv] = rglt[iglt];
    }


  /*----- Save the fitted time series and residual errors -----*/
  if (fitts_vol != NULL)
    {
      for (it = 0;  it < nt;  it++)
	fitts_vol[it][iv] = ts_array[it];
      
      for (it = 0;  it < N;  it++)
	fitts_vol[good_list[it]][iv] = fitts[it];
    }

  if (errts_vol != NULL)
    {
      for (it = 0;  it < nt;  it++)
	errts_vol[it][iv] = 0.0;

      for (it = 0;  it < N;  it++)
	errts_vol[good_list[it]][iv] = errts[it];
    }

}


/*---------------------------------------------------------------------------*/
/*
  Report the results from evaluation of the experimental design.
*/

void report_evaluation 
(
  int qp,                  /* number of polynomial trend baseline parameters */
  int num_stimts,          /* number of stimulus time series */
  char ** stim_label,      /* label for each stimulus */
  int * min_lag,           /* minimum time delay for impulse response */ 
  int * max_lag,           /* maximum time delay for impulse response */ 
  matrix xtxinv_full,      /* matrix:  1/(X'X)      for full model */
  int num_glt,             /* number of general linear tests */
  char ** glt_label,       /* label for general linear test */
  int * glt_rows,          /* number of linear constraints in glt */
  matrix * cxtxinvct       /* array of matrices:  C(1/(X'X))C' for glt */
)

{
  int m;                   /* parameter index */
  int is;                  /* stimulus index */
  int ilag;                /* time lag index */
  int iglt;                /* general linear test index */
  int ilc;                 /* linear combination index */
  float stddev;            /* normalized parameter standard deviation */

  
  /*----- Print the normalized parameter standard deviations -----*/
  m = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      printf ("\nStimulus: %s \n", stim_label[is]);
      for (ilag = min_lag[is];  ilag <= max_lag[is];  ilag++)
	{
	  stddev = sqrt ( 1.0 * xtxinv_full.elts[m][m] );
	  printf ("  h[%2d] norm. std. dev. = %8.4f  \n", ilag, stddev);
	  m++;
	}
    }
     
  /*----- Print normalized standard deviations for GLT's -----*/
  if (num_glt > 0)
    {
      for (iglt = 0;  iglt < num_glt;  iglt++)
	{
	  printf ("\nGeneral Linear Test: %s \n", glt_label[iglt]);

	  for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
	    {
	      stddev = sqrt ( 1.0 * cxtxinvct[iglt].elts[ilc][ilc] );
	      printf ("  LC[%d] norm. std. dev. = %8.4f \n", 
		       ilc, stddev);
	    }
	}
    }

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the impulse response function and associated statistics.
*/

void calculate_results 
(
  DC_options * option_data,         /* deconvolution algorithm options */
  THD_3dim_dataset * dset,          /* input 3d+time data set */
  byte * mask_vol,                  /* input mask volume */
  float * fmri_data,                /* input fMRI time series data */
  int fmri_length,                  /* length of fMRI time series */
  int * good_list,                  /* list of usable time points */
  int * block_list,                 /* list of block (run) starting points */
  int num_blocks,                   /* number of blocks (runs) */
  float ** stimulus,                /* stimulus time series arrays */
  int * stim_length,                /* length of stimulus time series */
  matrix * glt_cmat,                /* general linear test matrices */

  float ** coef_vol,        /* array of volumes of signal model parameters */
  float ** scoef_vol,       /* array of volumes of parameter std. devs. */
  float ** tcoef_vol,       /* array of volumes of parameter t-statistics */
  float ** fpart_vol,       /* array of volumes of partial F-statistics */
  float ** rpart_vol,       /* array of volumes of partial R^2 stats. */
  float * mse_vol,          /* volume of full model mean square error */
  float * ffull_vol,        /* volume of F-statistic for the full model */
  float * rfull_vol,        /* volume of R^2 for the full model */
  float *** glt_coef_vol,   /* volumes for GLT linear combinations */
  float *** glt_tcoef_vol,  /* volumes for GLT t-statistics */
  float **  glt_fstat_vol,  /* volumes for GLT F-statistics */
  float **  glt_rstat_vol,  /* volumes for GLT R^2 stats. */
  float ** fitts_vol,       /* volumes for full model fit to input data */
  float ** errts_vol        /* volumes for residual errors */
)
  
{
  float * ts_array = NULL;    /* array of measured data for one voxel */

#ifdef USE_GET
  int do_get = 0 ;            /* flag to use multi-gets */
#endif

  int qp;                     /* number of poly. trend baseline parameters */
  int q;                      /* number of baseline model parameters */
  int p;                      /* number of full model parameters */
  int polort;                 /* degree of polynomial for baseline model */
  int m;                      /* parameter index */
  int n;                      /* data point index */

  vector coef;                /* regression parameters */
  vector scoef;               /* std. devs. for regression parameters */
  vector tcoef;               /* t-statistics for regression parameters */
  float * fpart = NULL;       /* partial F-statistics for the stimuli */
  float * rpart = NULL;       /* partial R^2 stats. for the stimuli */
  float ffull;                /* full model F-statistic */
  float rfull;                /* full model R^2 stat. */
  float mse;                  /* mean square error from full model */

  matrix xdata;               /* independent variable matrix */
  matrix x_full;              /* extracted X matrix    for full model */
  matrix xtxinv_full;         /* matrix:  1/(X'X)      for full model */
  matrix xtxinvxt_full;       /* matrix:  (1/(X'X))X'  for full model */
  matrix x_base;              /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base;       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_rdcd = NULL;     /* extracted X matrices  for reduced models */
  matrix * xtxinvxt_rdcd = NULL;     
                              /* matrix:  (1/(X'X))X'  for reduced models */
  vector y;                   /* vector of measured data */       

  int ixyz;                   /* voxel index */
  int nxyz;                   /* number of voxels per dataset */

  int nt;                  /* number of images in input 3d+time dataset */
  int N;                   /* number of usable data points */

  int num_stimts;          /* number of stimulus time series */
  int * baseline;          /* flag for stim function in baseline model */ 
  int * min_lag;           /* minimum time delay for impulse response */ 
  int * max_lag;           /* maximum time delay for impulse response */ 
  int * nptr;              /* number of stim fn. time points per TR */
  char ** stim_label;      /* label for stimulus time series */

  int i;                   /* data point index */
  int is;                  /* stimulus index */
  int ilag;                /* time lag index */
  float stddev;            /* normalized parameter standard deviation */
  float rms_min;           /* minimum variation in data to fit full model */
  char * label;            /* string containing stat. summary of results */
  int nodata;              /* flag for 'no data' option */
  int novar;               /* flag for insufficient variation in data */

  int iglt;                    /* general linear test index */
  int ilc;                     /* linear combination index */
  int num_glt;                 /* number of general linear tests */
  char ** glt_label;           /* label for general linear test */
  int * glt_rows;              /* number of linear constraints in glt */
  matrix * cxtxinvct = NULL;   /* matrices: C(1/(X'X))C' for GLT */
  matrix * glt_amat = NULL;    /* constant GLT matrices for later use */
  vector * glt_coef = NULL;    /* linear combinations from GLT matrices */
  vector * glt_tcoef = NULL;   /* t-statistics for GLT linear combinations */
  float * fglt = NULL;         /* F-statistics for the general linear tests */
  float * rglt = NULL;         /* R^2 stats. for the general linear tests */

  float * fitts = NULL;        /* full model fitted time series */
  float * errts = NULL;        /* full model residual error time series */


  /*----- Initialize local variables -----*/
  nodata = option_data->nodata;
  nxyz = option_data->nxyz;       
  nt = option_data->nt;
  rms_min = option_data->rms_min;
  num_stimts = option_data->num_stimts;
  stim_label = option_data->stim_label;
  baseline = option_data->stim_base;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;
  nptr    = option_data->stim_nptr;
  num_glt = option_data->num_glt;
  glt_label = option_data->glt_label;
  glt_rows = option_data->glt_rows;

  polort = option_data->polort;
  qp = option_data->qp;
  q  = option_data->q;
  p  = option_data->p;

  N = option_data->N;


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&xdata);
  matrix_initialize (&x_full);
  matrix_initialize (&xtxinv_full);
  matrix_initialize (&xtxinvxt_full);
  matrix_initialize (&x_base);
  matrix_initialize (&xtxinvxt_base);
  vector_initialize (&coef);
  vector_initialize (&scoef);
  vector_initialize (&tcoef);
  vector_initialize (&y);

  if (num_stimts > 0)
    {
      x_rdcd = (matrix *) malloc (sizeof(matrix) * num_stimts);   
      MTEST (x_rdcd);
      xtxinvxt_rdcd = (matrix *) malloc (sizeof(matrix) * num_stimts);   
      MTEST (xtxinvxt_rdcd);
      
      for (is =0;  is < num_stimts;  is++)
	{
	  matrix_initialize (&x_rdcd[is]);
	  matrix_initialize (&xtxinvxt_rdcd[is]);
	}
    }

  if (num_glt > 0)
    {
      cxtxinvct = (matrix *) malloc (sizeof(matrix) * num_glt);   
      glt_amat  = (matrix *) malloc (sizeof(matrix) * num_glt);   
      glt_coef  = (vector *) malloc (sizeof(vector) * num_glt);   
      glt_tcoef = (vector *) malloc (sizeof(vector) * num_glt);   

      for (iglt =0;  iglt < num_glt;  iglt++)
	{
	  matrix_initialize (&cxtxinvct[iglt]);
	  matrix_initialize (&glt_amat[iglt]);
	  vector_initialize (&glt_coef[iglt]);
	  vector_initialize (&glt_tcoef[iglt]);
	}
    }


  /*----- Allocate memory -----*/
  if (num_stimts > 0)
    {
      fpart  = (float *) malloc (sizeof(float) * num_stimts);   MTEST (fpart);
      rpart  = (float *) malloc (sizeof(float) * num_stimts);   MTEST (rpart);
    }
  if (num_glt > 0)
    {
      fglt  = (float *) malloc (sizeof(float) * num_glt);   MTEST (fglt);
      rglt  = (float *) malloc (sizeof(float) * num_glt);   MTEST (rglt);
    }

  if (option_data->input1D_filename == NULL)
    {
#ifdef USE_GET
      do_get = 1 ;    /* don't need a pre-malloc-ed array for multi-gets */
#else
      ts_array = (float *) malloc (sizeof(float) * nt);   MTEST (ts_array);
#endif
    }
  fitts  = (float *) malloc (sizeof(float) * nt);   MTEST (fitts);
  errts  = (float *) malloc (sizeof(float) * nt);   MTEST (errts);


  /*----- Initialize the independent variable matrix -----*/
  init_indep_var_matrix (p, qp, polort, nt, N, good_list, block_list, 
			 num_blocks, num_stimts, stimulus, stim_length, 
			 min_lag, max_lag, nptr, &xdata);
  if (option_data->xout)  matrix_sprint ("X matrix:", xdata);

  /*-- 14 Jul 2004: check matrix for bad columns - RWCox --*/

  { int *iar , k ;
    iar = matrix_check_columns( xdata , 1.e-3 ) ;
    if( iar != NULL ){
      fprintf(stderr,"** Problems with the X matrix columns:\n") ;
      for( k=0 ; iar[2*k] >= 0 ; k++ ){
        if( iar[2*k+1] >= 0 )
          fprintf(stderr," * Columns %d and %d are nearly collinear!\n",
                  iar[2*k],iar[2*k+1] ) ;
        else
          fprintf(stderr," * Column %d is all zeros!\n",iar[2*k] ) ;
      }
      free(iar) ;
    }
  }

  /*-- 14 Jul 2004: calculate matrix condition number - RWCox --*/

  { double *ev , emin,emax ; int i ;
    ev = matrix_singvals( xdata ) ;
    emin = emax = ev[0] ;
    for( i=1 ; i < xdata.cols ; i++ ){
       if( ev[i] < emin ) emin = ev[i] ;
       if( ev[i] > emax ) emax = ev[i] ;
    }
    free((void *)ev) ;
    if( emin <= 0.0 || emax <= 0.0 ){
      fprintf(stderr,"** Matrix condition number undefined: "
                     "min ev=%g  max ev=%g\n",emin,emax ) ;
    } else {
      double cond = sqrt(emax/emin) ;
      fprintf(stderr,"++ Matrix condition:  %g\n",cond) ;
    }
  }

  /*----- Initialization for the regression analysis -----*/
  init_regression_analysis (p, qp, num_stimts, baseline, min_lag, max_lag, 
			    xdata, &x_full, &xtxinv_full, &xtxinvxt_full, 
			    &x_base, &xtxinvxt_base, x_rdcd, xtxinvxt_rdcd);
  if ((option_data->xout) || nodata) 
      matrix_sprint ("(X'X) inverse matrix:", xtxinv_full);


  /*----- Initialization for the general linear test analysis -----*/
  if (num_glt > 0)
    init_glt_analysis (xtxinv_full, num_glt, glt_cmat, glt_amat, cxtxinvct);


  vector_create (N, &y);

  
  if (nodata)
    {
      report_evaluation 
	(qp, num_stimts, stim_label, min_lag, max_lag, xtxinv_full, 
	 num_glt, glt_label, glt_rows, cxtxinvct); 
    }

  else
    {     /* actually process data */

      int ixyz_bot=0 , ixyz_top=nxyz ;  /* voxel indexes to process */

#ifdef USE_GET
#define NGET 32               /* number to get at one time */
      int nget=0 ,            /* number of time series current gotten */
          cget=0 ,            /* index of next timeseries in iget & imget */
          jget   ,            /* loop index for iget */
          iget[NGET] ;        /* voxel index of timeseries */
      MRI_IMARR *imget=NULL ; /* array of timeseries */
#endif

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

          DSET_load(dset) ;  /* so dataset will be common */

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
        fprintf(stderr,"++ Calculations starting; elapsed time=%.3f\n",COX_clock_time()) ;

      /*----- Loop over all voxels -----*/
      for (ixyz = ixyz_bot;  ixyz < ixyz_top;  ixyz++)
	{
	  /*----- Apply mask? -----*/
	  if (mask_vol != NULL)
	    if (mask_vol[ixyz] == 0)  continue; 

#ifdef USE_GET
          /*** race ahead and extract a bunch of voxel time series at once ***/

          if( do_get && cget == nget ){
            if( imget != NULL ) DESTROY_IMARR(imget) ;
            iget[0] = ixyz ; nget = 1 ;
            for( jget=ixyz+1 ; jget < nxyz && nget < NGET ; jget++ ){
              if( mask_vol == NULL || mask_vol[jget] != 0 )
                iget[nget++] = jget ;
            }
            imget = THD_extract_many_series( nget, iget, dset ) ;
            cget  = 0 ;  /* the next one to take out of imget */
          }
#endif

	  /*----- Extract Y-data for this voxel -----*/
	  if (option_data->input1D_filename != NULL)
	    ts_array = fmri_data;
	  else {
#ifdef USE_GET
            ts_array = MRI_FLOAT_PTR(IMARR_SUBIM(imget,cget));  /* the GET way */
            cget++ ;  /* take this one next time */
#else
            extract_ts_array (dset, ixyz, ts_array);            /* the OLD way */
#endif
          }

	  for (i = 0;  i < N;  i++)
	    y.elts[i] = ts_array[good_list[i]];
	   
	  
	  /*----- Perform the regression analysis for this voxel-----*/
	  regression_analysis (N, p, q, num_stimts, min_lag, max_lag,
			       x_full, xtxinv_full, xtxinvxt_full, x_base,
			       xtxinvxt_base, x_rdcd, xtxinvxt_rdcd, 
			       y, rms_min, &mse, &coef, &scoef, &tcoef, 
			       fpart, rpart, &ffull, &rfull, &novar,
			       fitts, errts);
	  

	  /*----- Perform the general linear tests for this voxel -----*/
	  if (num_glt > 0)
	    glt_analysis (N, p, x_full, y, mse*(N-p), coef, novar, cxtxinvct,
			  num_glt, glt_rows, glt_cmat, glt_amat, 
			  glt_coef, glt_tcoef, fglt, rglt);
	  
	  
	  /*----- Save results for this voxel into arrays -----*/
	  save_voxel (option_data, ixyz, coef, scoef, tcoef, fpart, rpart, mse,
		      ffull, rfull, glt_coef, glt_tcoef, fglt, rglt, 
		      nt, ts_array, good_list, fitts, errts, 
		      coef_vol, scoef_vol, tcoef_vol, fpart_vol, rpart_vol, 
		      mse_vol, ffull_vol, rfull_vol, glt_coef_vol, 
		      glt_tcoef_vol, glt_fstat_vol, glt_rstat_vol,
		      fitts_vol, errts_vol);
	  
	  
	  /*----- Report results for this voxel -----*/
	  if ( ((ffull > option_data->fdisp) && (option_data->fdisp >= 0.0))
	       || ((option_data->progress > 0) 
		   && (ixyz % option_data->progress == 0))
	       || (option_data->input1D_filename != NULL) )
	    {

              if( proc_ind == 0 ){
	        printf ("\n\nResults for Voxel #%d: \n", ixyz);
	        report_results (N, qp, q, p, polort, block_list, num_blocks, 
			     num_stimts, stim_label, baseline, min_lag, max_lag,
			     coef, tcoef, fpart, rpart, ffull, rfull, mse, 
		             num_glt, glt_label, glt_rows, glt_coef, 
		             glt_tcoef, fglt, rglt, &label);
	        printf ("%s \n", label);
              }
	    }
	  
	}  /*----- Loop over voxels -----*/

#ifdef USE_GET
        if( do_get ){
          if( imget != NULL ) DESTROY_IMARR(imget) ;
          ts_array = NULL ;
        }
#endif

        /*-- if this is a child process, we're done.
             if this is the parent process, wait for the children --*/

#ifdef PROC_MAX
        if( proc_numjob > 1 ){
          if( proc_ind > 0 ){                          /* death of child */
            fprintf(stderr,"++ Job #%d finished; elapsed time=%.3f\n",proc_ind,COX_clock_time()) ;
            _exit(0) ;

          } else {                      /* parent waits for children */
            int pp ;
            fprintf(stderr,"++ Job #0 waiting for children to finish; elapsed time=%.3f\n",COX_clock_time()) ;
            for( pp=1 ; pp < proc_numjob ; pp++ )
              waitpid( proc_pid[pp] , NULL , 0 ) ;
            fprintf(stderr,"++ Job #0 now finishing up; elapsed time=%.3f\n",COX_clock_time()) ;
          }

          /* when get to here, only parent process is left alive,
             and all the results are in the shared memory segment arrays */
        }
#endif
        if( proc_numjob == 1 )
          fprintf(stderr,"++ Calculations finished; elapsed time=%.3f\n",COX_clock_time()) ;

        if( option_data->input1D_filename == NULL)  /* don't need data anymore */
          DSET_unload(dset) ;
      
    }  /*----- NOT nodata -----*/


  /*----- Dispose of matrices and vectors -----*/
  vector_destroy (&y);
  vector_destroy (&tcoef);
  vector_destroy (&scoef);
  vector_destroy (&coef);

  if (num_stimts > 0)
    {
      for (is = 0;  is < num_stimts;  is++)
	{
	  matrix_destroy (&x_rdcd[is]);
	  matrix_destroy (&xtxinvxt_rdcd[is]);
	}
      free (x_rdcd);         x_rdcd = NULL;
      free (xtxinvxt_rdcd);  xtxinvxt_rdcd = NULL; 
    }

  matrix_destroy (&xtxinvxt_base);
  matrix_destroy (&x_base);
  matrix_destroy (&xtxinvxt_full);
  matrix_destroy (&xtxinv_full); 
  matrix_destroy (&x_full); 
  matrix_destroy (&xdata);

  if (num_glt > 0)
    {
      for (iglt = 0;  iglt < num_glt;  iglt++)
	{
	  matrix_destroy (&cxtxinvct[iglt]);      
	  matrix_destroy (&glt_amat[iglt]);
	  vector_destroy (&glt_coef[iglt]);
	  vector_destroy (&glt_tcoef[iglt]);
	} 
      free (cxtxinvct);   cxtxinvct = NULL; 
      free (glt_amat);    glt_amat  = NULL; 
      free (glt_coef);    glt_coef  = NULL; 
      free (glt_tcoef);   glt_tcoef = NULL; 
    }


  if (fpart != NULL)     { free (fpart);     fpart    = NULL; }
  if (rpart != NULL)     { free (rpart);     rpart    = NULL; }
  if (fglt  != NULL)     { free (fglt);      fglt     = NULL; }
  if (rglt  != NULL)     { free (rglt);      rglt     = NULL; }
  if (ts_array != NULL)  { free (ts_array);  ts_array = NULL; }
  if (fitts != NULL)     { free (fitts);     fitts    = NULL; }
  if (errts != NULL)     { free (errts);     errts    = NULL; }
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
  Use cubic spline interpolation to time shift the estimated impulse response 
  function, in order to correct for differences in slice acquisition times.

*/

void cubic_spline 
(
  DC_options * option_data,              /* deconvolution algorithm options */
  int ts_length,                         /* length of time series data */  
  float ** vol_array                     /* output time series volume data */
)

{
  THD_3dim_dataset * dset = NULL;        /* input afni data set pointer */
  int nx, ny, nz, nxyz;  /* dataset dimensions in voxels */
  int ixyz;              /* voxel index */
  int isl;               /* slice index */
  int i;                 /* data point index */
  float * yarray = NULL; /* impulse response function for a single voxel */
  float * sarray = NULL; /* second derivative of the cubic in each interval */
  matrix m, minv;        /* matrices for cubic spline interpolation */
  vector v, sv;          /* vectors for cubic spline interpolation */
  int n;                 /* number of intervals = ts_length-1 */
  float * a = NULL, 
        * b = NULL, 
        * c = NULL, 
        * d = NULL;      /* cubic spline interpolation polynomial coefs. */
  float tslice;          /* slice acquisition time offset */
  float tdelta;          /* time between same slice acquisitons */
  float frac;            /* fraction of interval for slice acq. time offset */
  int k;                 /* interval to use for interpolation */
  float t;               /* time in fractions of TR */
  float delt;            /* time offset relative to interpolation interval */
  float y;               /* interpolated value */


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&m);
  matrix_initialize (&minv);
  vector_initialize (&v);
  vector_initialize (&sv);


  /*----- Initialize local variables -----*/
  dset = THD_open_one_dataset (option_data->input_filename);
  n = ts_length - 1;
  tdelta = dset->taxis->ttdel;
  nx = dset->daxes->nxx;   ny = dset->daxes->nyy;   nz = dset->daxes->nzz;
  nxyz = nx * ny * nz;


  /*----- Allocate space for data and interpolation polynomials -----*/
  yarray = (float *) malloc (sizeof(float) * ts_length);
  sarray = (float *) malloc (sizeof(float) * (n+1));
  a = (float *) malloc (sizeof(float) * n);
  b = (float *) malloc (sizeof(float) * n);
  c = (float *) malloc (sizeof(float) * n);
  d = (float *) malloc (sizeof(float) * n);


  /*----- Calculate matrix for cubic spline interpolation -----*/
  matrix_create (n-1, n-1, &m);
  m.elts[0][0] = 4.0;
  m.elts[0][1] = 1.0;
  m.elts[n-2][n-3] = 1.0;
  m.elts[n-2][n-2] = 4.0;
  for (i = 1;  i < n-2;  i++)
    {
      m.elts[i][i] = 4.0;
      m.elts[i][i-1] = 1.0;
      m.elts[i][i+1] = 1.0;
    }
  matrix_inverse (m, &minv);


  vector_create (n-1, &v);


  /*----- Loop over all voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {

      /*----- Get time offset for this slice -----*/
      isl = ixyz / (nx*ny);
      tslice = THD_timeof_slice (0, isl, dset);
      frac = -((tslice/tdelta) - 0.5);

      /*----- Get impulse response function for this voxel -----*/
      for (i = 0;  i < ts_length;  i++)
	yarray[i] = vol_array[i][ixyz];


      /*----- Calculate vector for cubic spline interpolation -----*/
      for (i = 1;  i < n;  i++)
	v.elts[i-1] = 6.0 * (yarray[i+1] - 2.0 * yarray[i] + yarray[i-1]);
      vector_multiply (minv, v, &sv);


      /*----- Set array of second derivatives -----*/
      for (i = 1;  i < n;  i++)
	{
	  sarray[i] = sv.elts[i-1];
	}
      sarray[0] = 0.0;
      sarray[n] = 0.0;
      
      
      /*----- Calculate cubic spline polynomial coefficients -----*/
      for (i = 0;  i < n;  i++)
	{
	  a[i] = (sarray[i+1] - sarray[i]) / 6.0;
	  b[i] = sarray[i] / 2;
	  c[i] = (yarray[i+1]-yarray[i]) - (2.0*sarray[i]+sarray[i+1]) / 6.0;
	  d[i] = yarray[i];
	}
      
      
      /*----- Apply time shift to impulse response function -----*/
      for (i = 0;  i < ts_length;  i++)
	{
	  t = i + frac;
	  
	  if (frac < 0.0)  k = i-1;
	  else             k = i;
	  
	  if (k < 0)    k = 0;
	  if (k > n-1)  k = n-1;
	  
	  delt = t - k;
	  
	  yarray[i] = a[k]*delt*delt*delt + b[k]*delt*delt + c[k]*delt + d[k];
	}


      /*----- Save interpolated impulse response function -----*/
      for (i = 0;  i < ts_length;  i++)
	vol_array[i][ixyz] = yarray[i];



    }  /* Loop over voxels */


  /*----- Deallocate memory -----*/
  THD_delete_3dim_dataset (dset, False);  dset = NULL ;

  matrix_destroy (&m);
  matrix_destroy (&minv);
  vector_destroy (&v);
  vector_destroy (&sv);
  
  free (sarray);   sarray = NULL;
  free (yarray);   yarray = NULL;
  free (a);        a = NULL;
  free (b);        b = NULL;
  free (c);        c = NULL;
  free (d);        d = NULL;
}

 
/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI 3d+time data set. 
*/


void write_ts_array 
(
  int argc,                       /* number of input arguments */
  char ** argv,                   /* array of input arguments */ 
  DC_options * option_data,       /* deconvolution algorithm options */
  int ts_length,                  /* length of time series data */  
  int nptr,                       /* number of time points per TR */
  int tshift,                     /* flag to set slice offset times to 0 */
  float ** vol_array,             /* output time series volume data */
  char * output_filename          /* output afni data set file name */
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
  float * fbuf = NULL;      /* float buffer */
  float * volume;           /* pointer to volume of data */
  char label[THD_MAX_NAME]; /* label for output file */ 
  float newtr;              /* new time step = TR/nptr */
  

  /*----- Initialize local variables -----*/
  input_filename = option_data->input_filename;
  dset = THD_open_one_dataset (input_filename);
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;    
  newtr = DSET_TIMESTEP(dset) / nptr;
 
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
			    ADN_ttdel,       newtr,
			    ADN_none);
  
  if( ierror > 0 ){
    fprintf(stderr,
          "** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
	    "** Output dataset file %s already exists--cannot continue!\a\n",
	    new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }


  /*----- Reset slice offset times to zero -----*/
  if (tshift)
    EDIT_dset_items (new_dset, 
		     ADN_nsl,     0,    /* will have no offsets when done */
		     ADN_ttorg, 0.0,    /* in case not already set */
		     ADN_ttdur, 0.0,    /* in case not already set */
		     ADN_none);

  
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
  if (!  option_data->quiet)
    printf ("++ Writing 3d+time dataset into %s\n",
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
  Routine to write one bucket data set.
*/

void write_bucket_data 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  DC_options * option_data,         /* deconvolution algorithm options */

  float ** coef_vol,       /* array of volumes of signal model parameters */
  float ** tcoef_vol,      /* array of volumes of signal model t-statistics */
  float ** fpart_vol,      /* array of volumes of partial F-statistics */
  float ** rpart_vol,      /* array of volumes of partial R^2 statistics */
  float * mse_vol,         /* volume of full model mean square error */
  float * ffull_vol,       /* volume of full model F-statistics */
  float * rfull_vol,       /* volume of full model R^2 statistics */
  float *** glt_coef_vol,  /* volumes for GLT linear combinations */
  float *** glt_tcoef_vol, /* volumes for GLT t-statistics */
  float **  glt_fstat_vol, /* volumes for GLT F-statistics */
  float **  glt_rstat_vol  /* volumes for GLT R^2 statistics */
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
  int qp;                   /* number of poly. trend baseline parameters */
  int q;                    /* number of baseline model parameters */
  int p;                    /* number of full model parameters */
  int polort;               /* degree of polynomial for baseline model */
  int num_stimts;           /* number of stimulus time series */
  int istim;                /* stimulus index */
  int nxyz;                 /* total number of voxels */
  int nt;                   /* number of images in input 3d+time dataset */
  int ilag;                 /* lag index */
  int icoef;                /* coefficient index */
  int ibrick;               /* sub-brick index */
  int dof, ndof, ddof;      /* degrees of freedom */
  char label[THD_MAX_NAME];   /* general label for sub-bricks */
  int num_glt;                   /* number of general linear tests */
  int * glt_rows;                /* number of linear constraints in glt */
  int iglt;                      /* general linear test index */
  int ilc;                       /* linear combination index */


  /*----- read prototype dataset -----*/
  old_dset = THD_open_one_dataset (option_data->input_filename);

    
  /*----- Initialize local variables -----*/
  nxyz = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;  
  num_stimts = option_data->num_stimts;
  nt = DSET_NUM_TIMES (old_dset);
  num_glt = option_data->num_glt;
  glt_rows = option_data->glt_rows;

  polort = option_data->polort;
  qp = option_data->qp;
  q  = option_data->q;
  p  = option_data->p;
  N  = option_data->N;
  nbricks = option_data->nbricks;


  /*----- Prepare output file name -----*/
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
	      "** %d errors in attempting to create bucket dataset!\n", 
	      ierror);
      exit(1);
    }
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      fprintf(stderr,
	      "** Output dataset file %s already exists--cannot continue!\n",
	      DSET_HEADNAME(new_dset));
      exit(1);
    }
  

  /*----- Attach individual sub-bricks to the bucket dataset -----*/


  /*----- User can choose to place full model stats first -----*/
  ibrick = -1;
  if (option_data->full_first)  
    ibrick += option_data->vout + option_data->rout + option_data->fout;


  /*----- Include fit coefficients and associated statistics? -----*/
  if (! option_data->nocout)
    {

      /*----- Baseline statistics -----*/
      if (! option_data->nobout)
	{
	  strcpy (label, "Base");
	  for (icoef = 0;  icoef < qp;  icoef++)
	    {
	      if (qp == polort+1)
		strcpy (label, "Base");
	      else
		sprintf (label, "Run #%d", icoef/(polort+1) + 1);
	      
	      /*----- Baseline coefficient -----*/
	      ibrick++;
	      brick_type = FUNC_FIM_TYPE;
	      sprintf (brick_label, "%s t^%d Coef", label, icoef % (polort+1));
	      volume = coef_vol[icoef];
	      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				brick_type, brick_label, 0, 0, 0, bar);
	      
	      /*----- Baseline t-stat -----*/
	      if (option_data->tout)
		{
		  ibrick++;
		  brick_type = FUNC_TT_TYPE;
		  dof = N - p;
		  sprintf (brick_label, "%s t^%d t-st", 
			   label, icoef % (polort+1));
		  volume = tcoef_vol[icoef];
		  attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				    brick_type, brick_label, dof, 0, 0, bar);
		}
	    }
	}
      

      /*----- Stimulus statistics -----*/
      icoef = qp;
      for (istim = 0;  istim < num_stimts;  istim++)        
	{                                 
	  strcpy (label, option_data->stim_label[istim]);
	  
	  /*----- Loop over stimulus time lags -----*/
	  for (ilag = option_data->stim_minlag[istim];
	       ilag <= option_data->stim_maxlag[istim];  ilag++)
	    {                             
	      if ((! option_data->stim_base[istim]) 
	       || (! option_data->nobout))
		{
		  /*----- Stimulus coefficient -----*/
		  ibrick++;
		  brick_type = FUNC_FIM_TYPE;
		  sprintf (brick_label, "%s[%d] Coef", label, ilag);
		  volume = coef_vol[icoef];		  
		  attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				    brick_type, brick_label, 0, 0, 0, bar);
	      
		  /*----- Stimulus t-stat -----*/
		  if (option_data->tout)
		    {
		      ibrick++;
		      brick_type = FUNC_TT_TYPE;
		      dof = N - p;
		      sprintf (brick_label, "%s[%d] t-st", label, ilag);
		      volume = tcoef_vol[icoef];
		      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				    brick_type, brick_label, dof, 0, 0, bar);
		    }
		}
	      
	      icoef++;
	    }
	    
	  /*----- Stimulus R^2 stat -----*/
	  if ((option_data->rout)
	    && ((! option_data->stim_base[istim]) 
		|| (! option_data->nobout)))
	    {
	      ibrick++;
	      brick_type = FUNC_THR_TYPE;
	      sprintf (brick_label, "%s R^2", label);
	      volume = rpart_vol[istim];
	      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				brick_type, brick_label, 0, 0, 0, bar);
	    }
	  
	  /*----- Stimulus F-stat -----*/
	  if ((option_data->fout)
	    && ((! option_data->stim_base[istim]) 
		|| (! option_data->nobout)))
	    {
	      ibrick++;
	      brick_type = FUNC_FT_TYPE;
	      ndof = option_data->stim_maxlag[istim]
		- option_data->stim_minlag[istim] + 1;
	      ddof = N - p;
	      sprintf (brick_label, "%s F-stat", label);
	      volume = fpart_vol[istim];
	      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				brick_type, brick_label, 0, ndof, ddof, bar);
	    }
	  
	}  /* End loop over stim functions */
      
    }  /*  if (! option_data->nocout)  */

      
  /*----- General linear test statistics -----*/
  for (iglt = 0;  iglt < num_glt;  iglt++)
    {
      strcpy (label, option_data->glt_label[iglt]);

      /*----- Loop over rows of GLT matrix -----*/
      for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
	{
	  /*----- GLT coefficient -----*/
	  ibrick++;
	  brick_type = FUNC_FIM_TYPE;
	  sprintf (brick_label, "%s LC[%d] coef", label, ilc);
	  volume = glt_coef_vol[iglt][ilc];		  
	  attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			    brick_type, brick_label, 0, 0, 0, bar);
	      
	  /*----- GLT t-stat -----*/
	  if (option_data->tout)
	    {
	      ibrick++;
	      brick_type = FUNC_TT_TYPE;
	      dof = N - p;
	      sprintf (brick_label, "%s LC[%d] t-st", label, ilc);
	      volume = glt_tcoef_vol[iglt][ilc];		  
	      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
				brick_type, brick_label, dof, 0, 0, bar);
	    }	      
	}
      
      /*----- GLT R^2 stat -----*/
      if (option_data->rout)
	{
	  ibrick++;
	  brick_type = FUNC_THR_TYPE;
	  sprintf (brick_label, "%s R^2", label);
	  volume = glt_rstat_vol[iglt];   
	  attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			    brick_type, brick_label, 0, 0, 0, bar);
	}
      
      /*----- GLT F-stat -----*/
      if (option_data->fout)
	{
	  ibrick++;
	  brick_type = FUNC_FT_TYPE;
	  ndof = glt_rows[iglt];
	  ddof = N - p;
	  sprintf (brick_label, "%s F-stat", label);
	  volume = glt_fstat_vol[iglt];
	  attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			    brick_type, brick_label, 0, ndof, ddof, bar);
	}

    }  /* End loop over general linear tests */


  /*----- Statistics for full model -----*/
  if (option_data->full_first)  ibrick = -1;

  /*----- Full model MSE -----*/
  if (option_data->vout)
    {
      ibrick++;
      brick_type = FUNC_FIM_TYPE;
      sprintf (brick_label, "Full MSE");
      volume = mse_vol;
      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			brick_type, brick_label, 0, 0, 0, bar);
    }

  /*----- Full model R^2 -----*/
  if (option_data->rout)
    {
      ibrick++;
      brick_type = FUNC_THR_TYPE;
      sprintf (brick_label, "Full R^2");
      volume = rfull_vol;
      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			brick_type, brick_label, 0, 0, 0, bar);
    }

  /*----- Full model F-stat -----*/
  if (option_data->fout)
    {
      ibrick++;
      brick_type = FUNC_FT_TYPE;
      ndof = p - q;
      ddof = N - p;
      sprintf (brick_label, "Full F-stat");
      volume = ffull_vol;
      attach_sub_brick (new_dset, ibrick, volume, nxyz, 
			brick_type, brick_label, 0, ndof, ddof, bar);
    }


  /*----- write bucket data set -----*/
  if (! option_data->quiet)
    printf("++ Writing `bucket' dataset into %s\n",  DSET_HEADNAME(new_dset));
  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

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
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  DC_options * option_data,         /* deconvolution algorithm options */

  float ** coef_vol,        /* array of volumes of signal model parameters */
  float ** scoef_vol,       /* array of volumes of parameter std. devs. */
  float ** tcoef_vol,       /* array of volumes of parameter t-statistics */
  float ** fpart_vol,       /* array of volumes of partial F-statistics */
  float ** rpart_vol,       /* array of volumes of partial R^2 statistics */
  float * mse_vol,          /* volume of full model mean square error */
  float * ffull_vol,        /* volume of full model F-statistics */
  float * rfull_vol,        /* volume of full model R^2 statistics */
  float *** glt_coef_vol,   /* volumes for GLT linear combinations */
  float *** glt_tcoef_vol,  /* volumes for GLT t-statistics */
  float **  glt_fstat_vol,  /* volumes for GLT F-statistics */
  float **  glt_rstat_vol,  /* volumes for GLT R^2 statistics */
  float ** fitts_vol,       /* volumes for full model fit to input data */
  float ** errts_vol        /* volumes for residual errors */
)

{
  int qp;                   /* number of poly. trend baseline parameters */
  int q;                    /* number of baseline model parameters */
  int p;                    /* number of full model parameters */
  int polort;               /* degree of polynomial for baseline model */
  int num_stimts;           /* number of stimulus time series */
  int * min_lag;            /* minimum time delay for impulse response */ 
  int * max_lag;            /* maximum time delay for impulse response */ 
  int * nptr;               /* number of stim fn. time points per TR */
  int ib;                   /* sub-brick index */
  int is;                   /* stimulus index */
  int ts_length;            /* length of impulse reponse function */
  int nt;                   /* number of images in input 3d+time dataset */
  int nxyz;                 /* number of voxels */


  /*----- Initialize local variables -----*/
  polort = option_data->polort;
  qp = option_data->qp;
  q  = option_data->q;
  p  = option_data->p;
  num_stimts = option_data->num_stimts;
  min_lag = option_data->stim_minlag;
  max_lag = option_data->stim_maxlag;
  nptr    = option_data->stim_nptr;
  nt = option_data->nt;
  nxyz = option_data->nxyz;


  /*----- Write the bucket dataset -----*/
  if (option_data->bucket_filename != NULL) 
    if (nxyz > 1)
      write_bucket_data (argc, argv, option_data,  coef_vol, tcoef_vol, 
		   fpart_vol, rpart_vol, mse_vol, ffull_vol, rfull_vol, 
		   glt_coef_vol, glt_tcoef_vol, glt_fstat_vol, glt_rstat_vol);
    else
      write_one_ts (option_data->bucket_filename, p, coef_vol);


  /*----- Write the impulse response function 3d+time dataset -----*/
  ib = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      ts_length = max_lag[is] - min_lag[is] + 1;
      if (option_data->iresp_filename[is] != NULL)
	{
	  /*----- If requested, time shift the impulse response -----*/
	  if ((option_data->tshift) && (nptr[is] == 1) && (nxyz > 1))
	    cubic_spline (option_data, ts_length, coef_vol+ib);

	  if (nxyz > 1)
	    write_ts_array (argc, argv, option_data, ts_length, 
			    nptr[is], option_data->tshift, coef_vol+ib, 
			    option_data->iresp_filename[is]);
	  else
	    write_one_ts (option_data->iresp_filename[is], 
			  ts_length, coef_vol+ib);
	}
      ib += ts_length;
    }
 

  /*----- Write the standard deviation 3d+time dataset -----*/
  ib = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      ts_length = max_lag[is] - min_lag[is] + 1;
      if (option_data->sresp_filename[is] != NULL)
	if (nxyz > 1)
	  write_ts_array (argc, argv, option_data, ts_length, 
			  nptr[is], 0, scoef_vol+ib, 
			  option_data->sresp_filename[is]);
	else
	  write_one_ts (option_data->sresp_filename[is], 
			ts_length, scoef_vol+ib);

      ib += ts_length;
    }


  /*----- Write the fitted (full model) 3d+time dataset -----*/
  if (option_data->fitts_filename != NULL)
    if (nxyz > 1)
      write_ts_array (argc, argv, option_data, nt, 1, 0, fitts_vol, 
		      option_data->fitts_filename);
    else
      write_one_ts (option_data->fitts_filename, nt, fitts_vol);



  /*----- Write the residual errors 3d+time dataset -----*/
  if (option_data->errts_filename != NULL)
    if (nxyz > 1)
      write_ts_array (argc, argv, option_data, nt, 1, 0, errts_vol, 
		      option_data->errts_filename);
    else
      write_one_ts (option_data->errts_filename, nt, errts_vol);

}


/*---------------------------------------------------------------------------*/

void terminate_program
(
  DC_options ** option_data,         /* deconvolution algorithm options */
  float *** stimulus,                /* stimulus time series arrays */
  matrix ** glt_cmat,                /* general linear test matrices */

  float *** coef_vol,       /* array of volumes of signal model parameters */
  float *** scoef_vol,      /* array of volumes of parameter std. devs. */
  float *** tcoef_vol,      /* array of volumes of parameter t-statistics */
  float *** fpart_vol,      /* array of volumes of partial F-statistics */
  float *** rpart_vol,      /* array of volumes of partial R^2 statistics */
  float ** mse_vol,         /* volume of full model mean square error */
  float ** ffull_vol,       /* volume of full model F-statistics */
  float ** rfull_vol,       /* volume of full model R^2 statistics */

  float **** glt_coef_vol,  /* volumes for GLT linear combinations */
  float **** glt_tcoef_vol, /* volumes for GLT t-statistics */
  float ***  glt_fstat_vol, /* volumes for GLT F-statistics */
  float ***  glt_rstat_vol, /* volumes for GLT R^2 statistics */

  float *** fitts_vol,      /* volumes for full model fit to input data */
  float *** errts_vol       /* volumes for residual errors */
)

{
  int p;                    /* number of parameters in full model */
  int num_stimts;           /* number of stimulus time series */
  int ip;                   /* parameter index */
  int is;                   /* stimulus index */
  int num_glt;              /* number of general linear tests */
  int iglt;                 /* general linear test index */
  int * glt_rows;           /* number of linear constraints in glt */
  int ilc;                  /* linear combination index */
  int it;                   /* time index */
  int nt;                   /* number of images in input 3d+time dataset */


  /*----- Initialize local variables -----*/
  if (*option_data == NULL)  return;
  p = (*option_data)->p;
  num_stimts = (*option_data)->num_stimts;
  num_glt = (*option_data)->num_glt;
  glt_rows = (*option_data)->glt_rows;
  nt = (*option_data)->nt;


  /*----- Deallocate memory for option data -----*/
  free (*option_data);  *option_data = NULL; 


  /*----- Deallocate memory for stimulus time series -----*/
  if (*stimulus != NULL)
    {
      for (is = 0;  is < num_stimts;  is++)
	if ((*stimulus)[is] != NULL)
	  { free ((*stimulus)[is]);  (*stimulus)[is] = NULL; } 
      free (*stimulus);  *stimulus = NULL;
    }


  /*----- Deallocate memory for general linear test matrices -----*/
  if (*glt_cmat != NULL)
    {
      for (iglt = 0;  iglt < num_glt;  iglt++)
	matrix_destroy (&((*glt_cmat)[iglt]));
      free (*glt_cmat);  *glt_cmat = NULL;
    }


  /*----- Deallocate space for volume data -----*/
  if (*coef_vol  != NULL)    
    { 
      for (ip = 0;  ip < p;  ip++)
	if ((*coef_vol)[ip] != NULL)
	  { free ((*coef_vol)[ip]);   (*coef_vol)[ip] = NULL; }
      free (*coef_vol);   *coef_vol  = NULL; 
    }

  if (*scoef_vol  != NULL)    
    { 
      for (ip = 0;  ip < p;  ip++)
	if ((*scoef_vol)[ip] != NULL)
	  { free ((*scoef_vol)[ip]);   (*scoef_vol)[ip] = NULL; }
      free (*scoef_vol);   *scoef_vol  = NULL; 
    }

  if (*tcoef_vol  != NULL)    
    { 
      for (ip = 0;  ip < p;  ip++)
	if ((*tcoef_vol)[ip] != NULL)
	  { free ((*tcoef_vol)[ip]);   (*tcoef_vol)[ip] = NULL; }
      free (*tcoef_vol);   *tcoef_vol  = NULL; 
    }

  if (*fpart_vol != NULL)    
    { 
      for (is = 0;  is < num_stimts;  is++)
	if ((*fpart_vol)[is] != NULL)    
	  { free ((*fpart_vol)[is]);    (*fpart_vol)[is] = NULL; }
      free (*fpart_vol);  *fpart_vol = NULL; 
    }

  if (*rpart_vol != NULL)    
    { 
      for (is = 0;  is < num_stimts;  is++)
	if ((*rpart_vol)[is] != NULL)    
	  { free ((*rpart_vol)[is]);    (*rpart_vol)[is] = NULL; }
      free (*rpart_vol);  *rpart_vol = NULL; 
    }

  if (*mse_vol   != NULL)    { free (*mse_vol);    *mse_vol   = NULL; }
  if (*ffull_vol != NULL)    { free (*ffull_vol);  *ffull_vol = NULL; }
  if (*rfull_vol != NULL)    { free (*rfull_vol);  *rfull_vol = NULL; } 


  /*----- Deallocate space for general linear test results -----*/
  if (*glt_coef_vol  != NULL)  
    {
      for (iglt = 0;  iglt < num_glt;  iglt++)
	if ((*glt_coef_vol)[iglt] != NULL) 
	  {   
	    for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
	      if ((*glt_coef_vol)[iglt][ilc] != NULL)    
		{ 
		  free ((*glt_coef_vol)[iglt][ilc]);    
		  (*glt_coef_vol)[iglt][ilc] = NULL; 
		}   
	    free ((*glt_coef_vol)[iglt]);   (*glt_coef_vol)[iglt] = NULL; 
	  }
      free (*glt_coef_vol);  *glt_coef_vol = NULL; 
    }

  if (*glt_tcoef_vol  != NULL)  
    {
      for (iglt = 0;  iglt < num_glt;  iglt++)
	if ((*glt_tcoef_vol)[iglt] != NULL) 
	  {   
	    for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
	      if ((*glt_tcoef_vol)[iglt][ilc] != NULL)    
		{ 
		  free ((*glt_tcoef_vol)[iglt][ilc]);    
		  (*glt_tcoef_vol)[iglt][ilc] = NULL; 
		}   
	    free ((*glt_tcoef_vol)[iglt]);   (*glt_tcoef_vol)[iglt] = NULL; 
	  }
      free (*glt_tcoef_vol);  *glt_tcoef_vol = NULL; 
    }

  if (*glt_fstat_vol != NULL)  
    { 
      for (iglt = 0;  iglt < num_glt;  iglt++)
	if ((*glt_fstat_vol)[iglt] != NULL)    
	  { free ((*glt_fstat_vol)[iglt]);   (*glt_fstat_vol)[iglt] = NULL; } 
      free (*glt_fstat_vol); *glt_fstat_vol = NULL;
    }

  if (*glt_rstat_vol != NULL)  
    { 
      for (iglt = 0;  iglt < num_glt;  iglt++)
	if ((*glt_rstat_vol)[iglt] != NULL)    
	  { free ((*glt_rstat_vol)[iglt]);   (*glt_rstat_vol)[iglt] = NULL; } 
      free (*glt_rstat_vol); *glt_rstat_vol = NULL;
    }
 

  /*----- Deallocate space for fitted time series and residual errors -----*/
  if (*fitts_vol != NULL)
    {
      for (it = 0;  it < nt;  it++)
	{ free ((*fitts_vol)[it]);   (*fitts_vol)[it] = NULL; }
      free (*fitts_vol);   *fitts_vol = NULL;
    }

  if (*errts_vol != NULL)
    {
      for (it = 0;  it < nt;  it++)
	{ free ((*errts_vol)[it]);   (*errts_vol)[it] = NULL; }
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
  DC_options * option_data;               /* deconvolution algorithm options */
  THD_3dim_dataset * dset_time = NULL;    /* input 3d+time data set */
  byte * mask_vol  = NULL;                /* mask volume */
  float * fmri_data = NULL;               /* input fMRI time series data */
  int fmri_length;                        /* length of fMRI time series */
  float * censor_array = NULL;            /* input censor time series array */
  int censor_length;                      /* length of censor time series */
  int * good_list = NULL;                 /* list of usable time points */
  int * block_list = NULL;                /* list of block starting points */
  int num_blocks;                         /* number of blocks (runs) */
  float ** stimulus = NULL;               /* stimulus time series arrays */
  int  * stim_length = NULL;              /* length of stimulus time series */
  matrix * glt_cmat = NULL;               /* general linear test matrices */

  float ** coef_vol = NULL;   /* array of volumes for model parameters */
  float ** scoef_vol = NULL;  /* array of volumes for parameter std. devs. */
  float ** tcoef_vol = NULL;  /* array of volumes for parameter t-statistics */
  float ** fpart_vol = NULL;  /* array of volumes of partial F-statistics */
  float ** rpart_vol = NULL;  /* array of volumes of partial R^2 stats. */

  float * mse_vol   = NULL;   /* volume of full model mean square error */
  float * ffull_vol = NULL;   /* volume of full model F-statistics */
  float * rfull_vol = NULL;   /* volume of full model R^2 stats. */

  float *** glt_coef_vol = NULL;    /* volumes for GLT linear combinations */
  float *** glt_tcoef_vol = NULL;   /* volumes for GLT t-statistics */
  float **  glt_fstat_vol = NULL;   /* volumes for GLT F-statistics */
  float **  glt_rstat_vol = NULL;   /* volumes for GLT R^2 stats. */

  float ** fitts_vol = NULL;   /* volumes for full model fit to input data */
  float ** errts_vol = NULL;   /* volumes for residual errors */

#ifdef FLOATIZE
  fprintf(stderr,"**\n"
                 "** 3dDeconvolve_f is now disabled.\n"
                 "** It is too dangerous, due to roundoff problems.\n"
                 "** Please use 3dDeconvolve from now on!\n"
                 "** RWCox - 14 Jul 2004\n"
                 "**\n"
         ) ;
  exit(0) ;
#endif

  /*----- start the elapsed time counter -----*/
  (void) COX_clock_time() ;

  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, &dset_time, &mask_vol, 
     &fmri_data, &fmri_length, &censor_array, &censor_length, &good_list,
     &block_list, &num_blocks, &stimulus, &stim_length, &glt_cmat, 
     &coef_vol, &scoef_vol, &tcoef_vol, &fpart_vol, &rpart_vol, 
     &mse_vol, &ffull_vol, &rfull_vol, &glt_coef_vol, &glt_tcoef_vol, 
     &glt_fstat_vol, &glt_rstat_vol, &fitts_vol, &errts_vol);


  /*----- Perform deconvolution -----*/
  calculate_results (option_data, dset_time, mask_vol, fmri_data, fmri_length,
     good_list, block_list, num_blocks, stimulus, stim_length, 
     glt_cmat, coef_vol, scoef_vol, tcoef_vol, fpart_vol, 
     rpart_vol, mse_vol, ffull_vol, rfull_vol, glt_coef_vol, glt_tcoef_vol, 
     glt_fstat_vol, glt_rstat_vol, fitts_vol, errts_vol);
  

  /*----- Deallocate memory for input datasets -----*/   
  if (dset_time != NULL)  
    { THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL; }
  if (mask_vol != NULL)  
    { free (mask_vol);  mask_vol = NULL; }


  /*----- Write requested output files -----*/
  if (!option_data->nodata)
    output_results (argc, argv, option_data, coef_vol, scoef_vol, tcoef_vol, 
		    fpart_vol, rpart_vol, mse_vol, ffull_vol, rfull_vol,
		    glt_coef_vol, glt_tcoef_vol, glt_fstat_vol, glt_rstat_vol,
		    fitts_vol, errts_vol);


  /*----- Terminate program -----*/
  terminate_program (&option_data, &stimulus, &glt_cmat, &coef_vol, &scoef_vol,
 		     &tcoef_vol, &fpart_vol, &rpart_vol, & mse_vol, &ffull_vol,
		     &rfull_vol, &glt_coef_vol, &glt_tcoef_vol, &glt_fstat_vol,
 		     &glt_rstat_vol, &fitts_vol, &errts_vol);

  if (proc_use_jobs == 1){       /* output requested - 2003.08.15 [rickr] */
    fprintf(stderr,"++ Program finished; elapsed time=%.3f\n",COX_clock_time());
  }
#ifndef FLOATIZE
  if( proc_numjob == 1 ){              /* 16 Jan 2004: print operation count */
    double fv = get_matrix_flops() ;
    if( proc_use_jobs == 1 )
      fprintf(stderr,"++ Flops=%g\n",fv) ;
    else if( fv > 1000.0 )
      fprintf(stderr,"++ About %s arithmetic operations carried out\n",
              approximate_number_string(fv) );
  }
#endif

  exit(0);
}
