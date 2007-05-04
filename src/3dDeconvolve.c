/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1998-2003, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifdef USE_SUNPERF       /** for Solaris **/
# include <sunperf.h>
#endif

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

  Mod:     Increased max. allowed number of input stimulus functions, GLTs,
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
           number of regressors, number of GLTs, and number of GLT linear
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

  Mod:     Added -legendre, -nolegendre, -nocond options
  Date     15 Jul 2004 - RWCox

  Mod:     Replace matrix inversion by Gaussian elimination by SVD, and
           add -svd and -nosvd options.
  Date     20 Jul 2004 - RWCox

  Mod:     If SVD is on, don't eliminate all-zero stim files.
           Also, add -xjpeg option.
  Date     21 Jul 2004 - RWCox

  Mod:     -xsave and -xrestore options, to be able to run extra GLTs
  Date     28 Jul 2004 - RWCox

  Mod:     -gltsym option
  Date     29 Jul 2004 - RWCox

  Mod:     Multiple files (auto-tcat) for -input
  Date     05 Aug 2004
*/

/*---------------------------------------------------------------------------*/

#ifndef FLOATIZE
# define PROGRAM_NAME   "3dDeconvolve"            /* name of this program */
#else
# define PROGRAM_NAME   "3dDeconvolve_f"          /* name of this program */
#endif

#define PROGRAM_AUTHOR  "B. Douglas Ward, et al." /* program author */
#define PROGRAM_INITIAL "02 September 1998"       /* initial program release date*/
#define PROGRAM_LATEST  "04 March 2005 - RWCox"   /* latest program revision date*/

/*---------------------------------------------------------------------------*/

#define RA_error    DC_error

#define USE_GET   /* RWCox: extract multiple timeseries at once for speed */

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
  static long long proc_shmsize = 0   ; /* total size of shared memory */

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
# define MTYPE    double
# define NI_MTYPE NI_DOUBLE
# define QEPS 1.e-6
#else
# include "matrix_f.h"        /* single precision */
# define MTYPE    float
# define NI_MTYPE NI_FLOAT
# define QEPS 1.e-4
#endif

/*------------ prototypes for routines far below (RWCox) ------------------*/

void JPEG_matrix_gray( matrix X, char *fname );        /* save X matrix to JPEG */
void ONED_matrix_save( matrix X, char *fname, void * ); /* save X matrix to .1D */

void XSAVE_output( char * ) ;                      /* save X matrix into file */

static int check_matrix_condition( matrix xdata, char *xname ); /* 07 Mar 2007 */

static int xsave=0 ;                                   /* globals for -xsave */
static int nGoodList=0 , *GoodList=NULL ;
static int nParam=0 , *ParamIndex=NULL , *ParamStim=NULL ;
static char **ParamLabel=NULL ;
static char *InputFilename=NULL, *BucketFilename=NULL, *CoefFilename=NULL ;

static matrix X , XtXinv , XtXinvXt ;               /* global copies */

static int   *Xcol_inbase ;
static float *Xcol_mean ;
static int    voxel_num ;
static float *voxel_base = NULL ;

float baseline_mean( vector coef ) ;    /* compute mean of baseline stuff */

static int xrestore = 0 ;                           /* globals for -xrestore */
static char *xrestore_filename = NULL ;
static int NumTimePoints=0 , NumRegressors=0 ;

static int verb = 1 ;

static int goforit = 0 ;  /* 07 Mar 2007 */
static int badlev  = 0 ;
static int floatout= 0 ;  /* 13 Mar 2007 */

struct DC_options ;  /* incomplete struct definition */

void do_xrestore_stuff( int, char **, struct DC_options * ) ;

#define XSAVE_version "0.5"

static int nSymStim = 0 ;             /* 29 Jul 2004: symbols for stimuli */
static SYM_irange *SymStim = NULL ;

void read_glt_matrix( char *fname, int *nrows, int ncol, matrix *cmat ) ;
static void vstep_print(void) ;

static int show_singvals = 0 ;

static int         num_CENSOR = 0 ;     /* 01 Mar 2007 */
static int_triple *abc_CENSOR = NULL ;

/*---------- Typedefs for basis function expansions of the IRF ----------*/

#include "parser.h"   /* for EXPR, et cetera */

/***** Search for 'basis_' to find all stuff related to this *****/

#define USE_BASIS   /*** for Deconvolve.c ***/

/*! One basis function f(t,a,b,c). */

typedef struct {
  float a , b , c ;                            /* up to 3 parameters         */
  float ffac ;                                 /* scale factor [28 Apr 2005] */
  void *q ;                                    /* other parameters?         */
  float (*f)(float,float,float,float,void *) ; /* function: f(t,a,b,c,q)   */
} basis_func ;

/*! Macro to evaluate a basis_func at a particular point. */

#define basis_funceval(bf,x) ((bf).f( (x), (bf).a,(bf).b,(bf).c,(bf).q )*(bf).ffac)

/** Macros to define the meaning of the IRF times [08 Mar 2007] ('type') **/

#define BASIS_SINGLE         1   /* same IRF for all times */
#define BASIS_MODULATED_MONO 2   /* amplitude modulated IRF */
#define BASIS_MODULATED_PAIR 3   /* mean and delta amplitude modulated IRF */

/*! A whole set of basis functions (generated by -stim_times 3rd argument). */

typedef struct {
  int type , nfunc , nparm , pbot ;
  float tbot,ttop ;
  basis_func *bfunc ;
  char *name , *symfun , *option ;
} basis_expansion ;

/** Prototypes for some basis expansion functions appearing (much) later. **/

basis_expansion * basis_parser( char *sym ) ;
float basis_evaluation( basis_expansion *be , float *wt , float x ) ;
void basis_write_iresp( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float **wtar , char *output_filename ) ;
void basis_write_sresp( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float *mse ,
                        int pbot, matrix cvar, char *output_filename ) ;

/** global variables for stimulus basis expansions **/

static basis_expansion **basis_stim  = NULL ; /* equations for response model */
static MRI_IMAGE       **basis_times = NULL ; /* times for each response      */
static MRI_IMAGE       **basis_vect  = NULL ; /* vectors generated from above */
static float             basis_TR    = 1.0f ; /* data time step in seconds */
static int               basis_count = 0    ; /* any -stim_times inputs? */
static float             basis_dtout = 0.0f ; /* IRF time step in seconds */
static float             irc_dt      = 0.0f ;

static int               basis_need_mse = 0 ; /* need MSE volume */

static float             basis_normall  = 0.0f ; /* 28 Apr 2005 */

#define basis_filler 3.e+33 /* filler in basis_times for missing entries */

/*...........................................................................*/

typedef struct {           /** structure to hold one -IRC_times stuff **/
  int npar , pbot ;
  float *ww ;              /* [npar] */
  float scale_fac ;        /* fixed multiplier */
  int denom_flag ;         /* what to put in denominator? */
  char *name ;
} basis_irc ;

#if 0
typedef struct { float a,b ; } float_pair ;  /* moved to mrilib.h */
#endif

#define denom_BASELINE (1)

static int num_irc     = 0    ;  /* number of IRCs */
static basis_irc **irc = NULL ;  /* array of IRCs */

float_pair evaluate_irc( basis_irc *birc , vector coef ,
                         float base , float mse , matrix cvar ) ;

/*---------------------------------------------------------------------------*/

#define CM_BASELINE_MASK (1<<0)
#define CM_POLORT_MASK   (1<<1)

typedef struct {
  unsigned int  mask ;      /* binary properties of column */
           int  group ;     /* -1=polort 0=other baseline, larger=stimulus */
           char name[64] ;  /* string describing this column */
} column_metadata ;

static int             ncoldat = 0 ;
static column_metadata *coldat = NULL ;  /* global info about matrix columns */

#define COLUMN_LABEL(n) coldat[n].name

#undef  USE_OLD_LABELS

#define FIX_CONFLICTS   /* 23 Mar 2007 */

/*---------------------------------------------------------------------------*/

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
  float  input1D_TR;       /* TR for input 1D time series */
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
  int * slice_base ;       /* number of columns in a -slice_base file */
  int num_slice_base ;     /* number of -slice_base inputs */

  int num_glt;             /* number of general linear tests */
  int * glt_rows;          /* number of linear constraints in glt */
  char ** glt_filename;    /* file containing glt matrix */
  char ** glt_label;       /* label for general linear tests */

  char * bucket_filename;  /* bucket dataset file name */
  char ** iresp_filename;  /* impulse response 3d+time output */
  char ** sresp_filename;  /* std. dev. 3d+time output */
  char * fitts_filename;   /* fitted time series 3d+time output */
  char * errts_filename;   /* error time series 3d+time output */
  int nobucket ;           /* don't output a -bucket file! */

  int tshift;           /* flag to time shift the impulse response */
  int fout;             /* flag to output F-statistics */
  int do_fullf ;        /* flag to do full F, even if fout==0 */
  int rout;             /* flag to output R^2 statistics */
  int tout;             /* flag to output t-statistics */
  int vout;             /* flag to output variance map */
  int nobout;           /* flag to suppress output of baseline coefficients */
  int nocout;           /* flag to suppress output of fit coefficients */
  int xout;             /* flag to write X and inv(X'X) matrices to screen */
  int full_first;       /* flag to output full model stats first */

  int nocond ;          /* flag to disable condition numbering [15 Jul 2004] */

  char *xjpeg_filename; /* plot file for -xjpeg option [21 Jul 2004] */
  char *x1D_filename;   /* save filename for -x1D option [28 Mar 2006] */
  int nox1D ;           /* 20 Mar 2007 */

  int automask ;        /* flag to do automasking [15 Apr 2005] */

  int   nodata_NT ;     /* optional values after -nodata [27 Apr 2005] */
  float nodata_TR ;

  column_metadata *coldat ; /* info about each column [06 Mar 2007] */
} DC_options;


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

#if 1
void DC_error (char * message)
{
  ERROR_exit( PROGRAM_NAME " dies: %s",message) ;
}
#else
# define DC_error(m) ERROR_exit("3dDeconvolve dies: %s",(m))
#endif


/*---------------------------------------------------------------------------*/

void identify_software ()
{

  /*----- Identify software -----*/
#if 1
  PRINT_VERSION(PROGRAM_NAME) ; AUTHOR(PROGRAM_AUTHOR) ;
  THD_check_AFNI_version(PROGRAM_NAME) ;
#else
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR);
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
#endif

#ifdef USE_ACCELERATE
  INFO_message ("Compiled with vector acceleration for Mac OS X\n") ;
#elif defined(USE_SUNPERF) && !defined(FLOATIZE)
  INFO_message ("Compiled with BLAS-1 acceleration for Solaris\n") ;
#elif defined(USE_SCSLBLAS)
  INFO_message ("Compiled with BLAS-1 acceleration for SGI Altix\n") ;
#endif
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
    "**** Input data and control options:                                   \n"
    "-input fname         fname = filename of 3d+time input dataset         \n"
    "                       (more than  one filename  can be  given)        \n"
    "                       (here,   and  these  datasets  will  be)        \n"
    "                       (catenated  in time;   if you do this, )        \n"
    "                       ('-concat' is not needed and is ignored)        \n"
    "[-input1D dname]     dname = filename of single (fMRI) .1D time series \n"
    "[-TR_1D tr1d]        tr1d = TR for .1D time series (default 1.0 sec).  \n"
    "                     This option has no effect without -input1D        \n"
    "[-nodata [NT [TR]]   Evaluate experimental design only (no input data) \n"
    "[-mask mname]        mname = filename of 3d mask dataset               \n"
    "[-automask]          build a mask automatically from input data        \n"
    "                      (will be slow for long time series datasets)     \n"
    "[-censor cname]      cname = filename of censor .1D time series        \n"
    "[-CENSORTR clist]    clist = list of strings that specify time indexes \n"
    "                       to be removed from the analysis.  Each string is\n"
    "                       of one of the following forms:                  \n"
    "                           37 => remove global time index #37          \n"
    "                         2:37 => remove time index #37 in run #2       \n"
    "                       37..47 => remove global time indexes #37-47     \n"
    "                       37-47  => same as above                         \n"
    "                     2:37..47 => remove time indexes #37-47 in run #2  \n"
    "                     *:0-2    => remove time indexes #0-2 in all runs  \n"
    "                      +Time indexes within each run start at 0.        \n"
    "                      +Run indexes start at 1 (just be to confusing).  \n"
    "                      +Multiple -CENSORTR options may be used, or      \n"
    "                        multiple -CENSORTR strings can be given at     \n"
    "                        once, separated by spaces or commas.           \n"
    "                      +N.B.: 2:37,47 means index #37 in run #2 and     \n"
    "                        global time index 47; it does NOT mean         \n"
    "                        index #37 in run #2 AND index #47 in run #2.   \n"
    "[-concat rname]      rname = filename for list of concatenated runs    \n"
    "[-nfirst fnum]       fnum = number of first dataset image to use in the\n"
    "                       deconvolution procedure. (default = max maxlag) \n"
    "[-nlast  lnum]       lnum = number of last dataset image to use in the \n"
    "                       deconvolution procedure. (default = last point) \n"
    "[-polort pnum]       pnum = degree of polynomial corresponding to the  \n"
    "                       null hypothesis  (default: pnum = 1)            \n"
    "                       If you use 'A' for pnum, the program will       \n"
    "                       automatically choose a value.                   \n"
    "[-legendre]          use Legendre polynomials for null hypothesis      \n"
    "[-nolegendre]        use power polynomials for null hypotheses         \n"
    "                       (default is -legendre)                          \n"
    "[-nodmbase]          don't de-mean baseline time series                \n"
    "                       (i.e., polort>1 and -stim_base inputs)          \n"
    "[-dmbase]            de-mean baseline time series (default if polort>0)\n"
    "[-svd]               Use SVD instead of Gaussian elimination (default) \n"
    "[-nosvd]             Use Gaussian elimination instead of SVD           \n"
    "[-rmsmin r]          r = minimum rms error to reject reduced model     \n"
    "[-nocond]            DON'T calculate matrix condition number           \n"
    "                      ** This is NOT the same as Matlab!               \n"
    "[-singvals]          Print out the matrix singular values              \n"
    "[-GOFORIT [g]]       Use this to proceed even if the matrix has        \n"
    "                     bad problems (e.g., duplicate columns, large      \n"
    "                     condition number, etc.).                          \n"
    "               *N.B.: Warnings that you should particularly heed have  \n"
    "                      the string '!!' somewhere in their text.         \n"
    "               *N.B.: Error and Warning messages go to stderr and      \n"
    "                      also to file " PROGRAM_NAME ".err.               \n"
    "               *N.B.: The optional number 'g' that appears is the      \n"
    "                      number of warnings that can be ignored.          \n"
    "                      That is, if you use -GOFORIT 7 and 9 '!!'        \n"
    "                      matrix warnings appear, then the program will    \n"
    "                      not run.  If 'g' is not present, 1 is used.      \n"
    "                                                                       \n"
    "**** Input stimulus options:                                           \n"
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
    "[-stim_times k tname Rmodel]                                           \n"
    "   Generate the k-th response model from a set of stimulus times       \n"
    "   given in file 'tname'.  The response model is specified by the      \n"
    "   'Rmodel' argument, which can be one of                              \n"
    "     'GAM(p,q)'    = 1 parameter gamma variate                         \n"
    "     'SPMG'        = 2 parameter SPM gamma variate + derivative        \n"
    "     'POLY(b,c,n)' = n parameter polynomial expansion                  \n"
    "     'SIN(b,c,n)'  = n parameter sine series expansion                 \n"
    "     'TENT(b,c,n)' = n parameter tent function expansion               \n"
    "     'BLOCK(d,p)'  = 1 parameter block stimulus of duration 'd'        \n"
    "                     (can also be called 'IGFUN' which stands)         \n"
    "                     (for 'incomplete gamma function'        )         \n"
    "     'EXPR(b,c) exp1 ... expn' = n parameter; arbitrary expressions    \n"
#define USE_CSPLIN
#ifdef  USE_CSPLIN
    "     'CSPLIN(b,c,n)'= n parameter cubic spline function expansion      \n"
#endif
    "                                                                       \n"
    "[-stim_times_AM1 k tname Rmodel]                                       \n"
    "   Similar, but generates an amplitude modulated response model.       \n"
    "   The 'tname' file should consist of 'time*amplitude' pairs.          \n"
    "[-stim_times_AM2 k tname Rmodel]                                       \n"
    "   Similar, but generates 2 response models: one with the mean         \n"
    "   amplitude and one with the differences from the mean.               \n"
    "                                                                       \n"
    "[-basis_normall a]                                                     \n"
    "   Normalize all basis functions for '-stim_times' to have             \n"
    "   amplitude 'a' (must have a > 0).  The peak absolute value           \n"
    "   of each basis function will be scaled to be 'a'.                    \n"
    "   NOTE: -basis_normall only affect -stim_times options that           \n"
    "         appear LATER on the command line                              \n"
#if 0
    "                                                                       \n"
    "[-slice_base k sname]                                                  \n"
    "       Inputs the k'th stimulus time series from file sname,           \n"
    "   AND specifies that this regressor belongs to the baseline,          \n"
    "   AND specifies that the regressor is different for each slice in     \n"
    "       the input 3D+time dataset.  The sname file should have exactly  \n"
    "       nz columns of input, where nz=number of slices, OR it should    \n"
    "       have exactly 1 column, in which case this input is the same     \n"
    "       as using '-stim_file k sname' and '-stim_base k'.               \n"
    " N.B.: * You can't use -stim_minlag or -stim_maxlag or -stim_nptr      \n"
    "         with this value of k.                                         \n"
    "       * You can't use this option with -input1D or -nodata.           \n"
    "       * The intended use of this option is to provide slice-          \n"
    "         dependent physiological noise regressors, e.g., from program  \n"
    "         1dCRphase.                                                    \n"
    "     *** NOT YET IMPLEMENTED ***                                       \n"
#endif
    "                                                                       \n"
    "**** General linear test (GLT) options:                                \n"
    "-num_glt num         num = number of general linear tests (GLTs)       \n"
    "                       (0 <= num)   (default: num = 0)                 \n"
    "[-glt s gltname]     Perform s simultaneous linear tests, as specified \n"
    "                       by the matrix contained in file gltname         \n"
    "[-glt_label k glabel]  glabel = label for kth general linear test      \n"
    "[-gltsym gltname]    Read the GLT with symbolic names from the file    \n"
    "                                                                       \n"
    "[-TR_irc dt]                                                           \n"
    "   Use 'dt' as the stepsize for computation of integrals in -IRC_times \n"
    "   options.  Default is to use value given in '-TR_times'.             \n"
    "                                                                       \n"
    "**** Options for output 3d+time datasets:                              \n"
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
    "[-TR_times dt]                                                         \n"
    "   Use 'dt' as the stepsize for output of -iresp and -sresp file       \n"
    "   for response models generated by '-stim_times' options.             \n"
    "   Default is same as time spacing in the '-input' 3D+time dataset.    \n"
    "   The units here are in seconds!                                      \n"
    "                                                                       \n"
    "**** Options to control the contents of the output bucket dataset:     \n"
    "[-fout]            Flag to output the F-statistics                     \n"
    "[-rout]            Flag to output the R^2 statistics                   \n"
    "[-tout]            Flag to output the t-statistics                     \n"
    "[-vout]            Flag to output the sample variance (MSE) map        \n"
    "[-nobout]          Flag to suppress output of baseline coefficients    \n"
    "                     (and associated statistics) [** DEFAULT **]       \n"
    "[-bout]            Flag to turn on output of baseline coefs and stats. \n"
    "[-nocout]          Flag to suppress output of regression coefficients  \n"
    "                     (and associated statistics)                       \n"
    "[-full_first]      Flag to specify that the full model statistics will \n"
    "                     be first in the bucket dataset [** DEFAULT **]    \n"
    "[-nofull_first]    Flag to specify that full model statistics go last  \n"
    "[-nofullf_atall]   Flag to turn off the full model F statistic         \n"
    "                     ** DEFAULT: the full F is always computed, even if\n"
    "                     sub-model partial F's are not ordered with -fout. \n"
    "[-bucket bprefix]  Create one AFNI 'bucket' dataset containing various \n"
    "                     parameters of interest, such as the estimated IRF \n"
    "                     coefficients, and full model fit statistics.      \n"
    "                     Output 'bucket' dataset is written to bprefix.    \n"
    "[-nobucket]        Don't output a bucket dataset.  By default, the     \n"
    "                     program uses '-bucket Decon' if you don't give    \n"
    "                     either -bucket or -nobucket on the command line.  \n"
    "                                                                       \n"
    "[-xsave]           Flag to save X matrix into file bprefix.xsave       \n"
    "                     (only works if -bucket option is also given)      \n"
    "[-noxsave]         Don't save X matrix (this is the default)           \n"
    "[-cbucket cprefix] Save the regression coefficients (no statistics)    \n"
    "                     into a dataset named 'cprefix'.  This dataset     \n"
    "                     will be used in a -xrestore run instead of the    \n"
    "                     bucket dataset, if possible.                      \n"
    "                   Also, the -cbucket and -x1D output can be combined  \n"
    "                     in 3dSynthesize to produce 3D+time datasets that  \n"
    "                     are derived from subsets of the regression model  \n"
    "                     [generalizing the -fitts option, which produces]  \n"
    "                     [a 3D+time dataset derived from the full model].  \n"
    "                                                                       \n"
    "[-xrestore f.xsave] Restore the X matrix, etc. from a previous run     \n"
    "                     that was saved into file 'f.xsave'.  You can      \n"
    "                     then carry out new -glt tests.  When -xrestore    \n"
    "                     is used, most other command line options are      \n"
    "                     ignored.                                          \n"
    "                                                                       \n"
    "[-float]            Write output datasets in float format, instead of  \n"
    "                    as scaled shorts.                                  \n"
    "[-short]            Write output as scaled shorts [the default for now]\n"
    "                                                                       \n"
    "**** The following options control the screen output only:             \n"
    "[-quiet]             Flag to suppress most screen output               \n"
    "[-xout]              Flag to write X and inv(X'X) matrices to screen   \n"
    "[-xjpeg filename]    Write a JPEG file graphing the X matrix           \n"
    "[-x1D filename]      Save X matrix to a .x1D (ASCII) file [default]    \n"
    "[-nox1D]             Don't save X matrix                               \n"
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
            "             number of CPUs sharing memory on the system.\n"
            "             J=1 is normal (single process) operation.\n"
            "             The maximum allowed value of J is %d.\n"
            "         * For more information on parallelizing, see\n"
            "           http://afni.nimh.nih.gov/afni/doc/misc/afni_parallelize\n"
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
  option_data->nocond   = 0;   /* 15 Jul 2004 */
  option_data->nodata_NT= 0;   /* 27 Apr 2005 */
  option_data->nodata_TR= 0.0;
  option_data->coldat   = NULL; /* 06 Mar 2007 */

  option_data->xjpeg_filename = NULL ;  /* 21 Jul 2004 */
  option_data->x1D_filename   = NULL ;
  option_data->nox1D          = 0 ;

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
  option_data->slice_base = NULL ;
  option_data->num_slice_base = 0 ;

  /*----- Initialize glt options -----*/
  option_data->num_glt = 0;
  option_data->glt_filename = NULL;
  option_data->glt_label = NULL;
  option_data->glt_rows = NULL;

  /*----- Initialize output flags -----*/
  option_data->tshift = 0;
  option_data->fout = 0;
  option_data->do_fullf = 1 ;   /* 23 Mar 2007 */
  option_data->rout = 0;
  option_data->tout = 0;
  option_data->vout = 0;
  option_data->xout = 0;
  option_data->nobout = 1;      /* 13 Mar 2007: on by default now */
  option_data->nocout = 0;
  option_data->full_first = 1;  /* 13 Mar 2007; on by default now */

  /*----- Initialize character strings -----*/
  option_data->input_filename = NULL;
  option_data->mask_filename = NULL;
  option_data->input1D_filename = NULL;
  option_data->input1D_TR = 0.0;
  option_data->censor_filename = NULL;
  option_data->concat_filename = NULL;
  option_data->bucket_filename = NULL;
  option_data->fitts_filename = NULL;
  option_data->errts_filename = NULL;

  option_data->automask = 0 ;  /* 15 Apr 2005 */
  option_data->nobucket = 0 ;  /* 03 May 2007 */
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

ENTRY("initialize_stim_options") ;

  /*----- Set number of input stimulus time series -----*/
  if (num_stimts <= 0)  EXRETURN ;
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
  option_data->slice_base = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (option_data->slice_base);

  /* 10 Aug 2004: add space for basis function expansions */

  basis_stim  = (basis_expansion **)malloc(sizeof(basis_expansion *)*num_stimts);
  basis_times = (MRI_IMAGE **)      malloc(sizeof(MRI_IMAGE *)      *num_stimts);
  basis_vect  = (MRI_IMAGE **)      malloc(sizeof(MRI_IMAGE *)      *num_stimts);

  /*----- Initialize stimulus options -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      option_data->stim_filename[is] = NULL;
      option_data->stim_label[is] = malloc (sizeof(char)*THD_MAX_NAME);
      MTEST (option_data->stim_label[is]);
      sprintf (option_data->stim_label[is], "Stim#%d", is+1);

      option_data->stim_base[is]    = 0;
      option_data->stim_minlag[is]  = 0;
      option_data->stim_maxlag[is]  = 0;
      option_data->stim_nptr[is]    = 1;
      option_data->slice_base[is]   = 0;

      option_data->iresp_filename[is] = NULL;
      option_data->sresp_filename[is] = NULL;

      basis_stim [is] = NULL ;   /* 10 Aug 2004 */
      basis_times[is] = NULL ;
      basis_vect [is] = NULL ;
    }

   EXRETURN ;
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

ENTRY("initialize_glt_options") ;


  /*----- Set number of general linear tests -----*/
  if (num_glt <= 0)  EXRETURN ;
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
      sprintf (option_data->glt_label[iglt], "GLT#%d ", iglt+1);
      option_data->glt_rows[iglt] = 0;
    }


   EXRETURN ;
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
  int nerr ;

  /*-- addto the arglist, if user wants to --*/
  { int new_argc ; char **new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }

  /*----- does user request help menu? -----*/
  if (argc < 2 || strcmp(argv[1], "-help") == 0)  display_help_menu();


  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv);


  /*----- initialize the input options -----*/
  initialize_options (option_data);


  if( AFNI_yesenv("AFNI_3dDeconvolve_GOFORIT") ) goforit++ ; /* 07 Mar 2007 */

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      if( strcmp(argv[nopt],"-OK") == 0 ){ nopt++; continue; } /* 14 Jul 2004 */

      /*-----   -nocond           ------*/

      if( strcmp(argv[nopt],"-nocond") == 0 ){  /* 15 Jul 2004 */
#ifndef FLOATIZE
        option_data->nocond = 1 ;   /* only allow this for double precision */
#else
        INFO_message("-nocond is ignored in 3dDeconvolve_f!") ;
#endif
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-goforit") == 0 ){  /* 07 Mar 2007 */
        nopt++ ;
        if( nopt < argc && isdigit(argv[nopt][0]) )
          goforit += (int)strtod(argv[nopt++],NULL) ;  /* 04 May 2007 */
        else
          goforit++ ;
        continue ;
      }

      if( strcmp(argv[nopt],"-singvals") == 0 ){  /* 13 Aug 2004 */
        show_singvals = 1 ; option_data->nocond = 0 ;
        nopt++ ; continue ;
      }

      /*-----   -xjpeg filename  ------*/
      if (strcmp(argv[nopt], "-xjpeg") == 0)   /* 21 Jul 2004 */
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -xjpeg ");
        option_data->xjpeg_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->xjpeg_filename);
        strcpy (option_data->xjpeg_filename, argv[nopt]);
          if( strstr(option_data->xjpeg_filename,".jpg") == NULL &&
              strstr(option_data->xjpeg_filename,".JPG") == NULL   )
            strcat( option_data->xjpeg_filename , ".jpg" ) ;
        nopt++; continue;
      }

      /*-----   -x1D filename  ------*/
      if (strcmp(argv[nopt], "-x1D") == 0)   /* 28 Mar 2006 */
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -x1D ");
        option_data->x1D_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->x1D_filename);
        strcpy (option_data->x1D_filename, argv[nopt]);
        if( strstr(option_data->x1D_filename,"1D") == NULL )
          strcat( option_data->x1D_filename , ".x1D" ) ;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-nox1D") == 0 ){  /* 20 Mar 2007 */
        option_data->nox1D = 1 ; nopt++ ; continue ;
      }

      /*-----   -input filename   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
      {
          int iopt , slen ;
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -input ");
#if 0
        option_data->input_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->input_filename);
        strcpy (option_data->input_filename, argv[nopt]);
        nopt++;
#else                   /* 05 Aug 2004: multiple input datasets */
          slen = 0 ;
          for( iopt=nopt ; iopt < argc && argv[iopt][0] != '-' ; iopt++ )
            slen += strlen(argv[iopt])+8 ;
          option_data->input_filename = calloc(sizeof(char),slen) ;
        MTEST (option_data->input_filename);
          for( iopt=nopt ; iopt < argc && argv[iopt][0] != '-' ; iopt++ ){
            strcat( option_data->input_filename , argv[iopt] ) ;
            strcat( option_data->input_filename , " "        ) ;
          }
          slen = strlen(option_data->input_filename) ;
          option_data->input_filename[slen-1] = '\0' ; /* trim last blank */
          nopt = iopt ;
#endif
        continue;
      }


      /*-----   -mask filename   -----*/
      if (strcmp(argv[nopt], "-mask") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -mask ");
     if( option_data->automask ) DC_error("can't use -mask AND -automask!") ;
        option_data->mask_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->mask_filename);
        strcpy (option_data->mask_filename, argv[nopt]);
        nopt++;
        continue;
      }

      /*----    -automask   -----*/
      if( strcmp(argv[nopt],"-automask") == 0 ){   /* 15 Apr 2005 */
        if( option_data->mask_filename != NULL )
          DC_error("can't use -automask AND -mask!") ;
        option_data->automask = 1 ;
        nopt++ ; continue ;
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

      /*-----   -input1D TR  --------*/
      if (strcmp(argv[nopt], "-TR_1D") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -TR_1D ");
        option_data->input1D_TR = (float)strtod(argv[nopt++],NULL) ;
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

      /*-----  -CENSOR clist -----*/

      if( strncmp(argv[nopt],"-CENSOR",7) == 0 ){   /* RWCox - 01 Mar 2007 */
        NI_str_array *nsar ;
        char *src=malloc(1), *cpt, *dpt ;
        int ns, r,a,b, nerr=0 ; int_triple rab ;

        *src = '\0' ;   /* cat all following options until starts with '-' */
        for( nopt++ ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){
          ns = strlen(argv[nopt]) ; if( ns == 0 ) continue ;
          src = realloc(src,strlen(src)+ns+2) ;
          strcat(src," ") ; strcat(src,argv[nopt]) ;
        }
        if( *src == '\0' ) DC_error("Bad argument after -CENSORTR") ;
        nsar = NI_decode_string_list( src , "," ) ; /* break into substrings */
        for( ns=0 ; ns < nsar->num ; ns++ ){ /* loop over substrings */
          cpt = nsar->str[ns] ; dpt = strchr(cpt,':') ; r = 0 ;
          if( *cpt == '\0' ) continue ;   /* skip an empty string */
          if( dpt != NULL ){              /* found 'run:' */
            if( *cpt == '*' ){ /* wildcard = all runs */
              r = -666 ;
            } else {
              r = (int)strtol(cpt,NULL,10) ;
              if( r <= 0 ){  /* skip out */
                ERROR_message("-CENSORTR %s -- run index '%d' is bad!",nsar->str[ns],r);
                nerr++ ; continue ;
              }
            }
            cpt = dpt+1 ;  /* skip to character after ':' */
            if( *cpt == '\0' ){  /* skip out */
              ERROR_message("-CENSORTR %s -- no data after run index!",nsar->str[ns]);
              nerr++ ; continue ;
            }
          }
          a = (int)strtol(cpt,&dpt,10) ;    /* get first index number */
          if( a < 0 ){  /* skip out */
            ERROR_message("-CENSORTR %s -- time index '%d' is bad!",nsar->str[ns],a);
            nerr++ ; continue ;
          }
          if( *dpt == '\0' ){  /* no second number */
            b = a ;
          } else {             /* get second number */
            for( dpt++ ; *dpt != '\0' && !isdigit(*dpt) ; dpt++ ) ; /*nada*/
            b = (int)strtol(dpt,NULL,10) ;
            if( b < a || b < 0 ){  /* skip out */
              ERROR_message("-CENSORTR %s -- time indexes '%d' to '%d' is bad!",
                            nsar->str[ns],a,b);
              nerr++ ; continue ;
            }
          }
          abc_CENSOR = (int_triple *)realloc( abc_CENSOR ,
                                              sizeof(int_triple)*(num_CENSOR+1) );
          rab.i = r; rab.j = a; rab.k = b; abc_CENSOR[num_CENSOR++] = rab ;
        } /* end of loop over -CENSORTR strings */
        if( nerr > 0 ) ERROR_exit("Can't proceed after -CENSORTR errors!") ;
        NI_delete_str_array(nsar) ; free(src) ;
        continue ;  /* next option */
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


      /*-----   -nodata [NT [TR]]  -----*/
      if (strcmp(argv[nopt], "-nodata") == 0){
        option_data->nodata = 1; nopt++;

     /* 27 Apr 2005: check for additional numeric values */

        if( isdigit(argv[nopt][0]) ){  /* get NT */
          option_data->nodata_NT = (int)strtol(argv[nopt++],NULL,10) ;
          if( isdigit(argv[nopt][0]) ){  /* also get TR */
            option_data->nodata_TR = (float)strtod(argv[nopt++],NULL) ;
          }
        }
        continue;
      }


      /*-----   -nfirst num  -----*/
      if (strcmp(argv[nopt], "-nfirst") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -nfirst ");
        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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
        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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
        if( toupper(argv[nopt][0]) == 'A' ){  /* 16 Mar 2006: Automatic */
          ival = -666 ;
        } else {
          ival = -2 ; sscanf (argv[nopt], "%d", &ival);
          if (ival < -1)
            DC_error ("illegal argument after -polort ");
        }
        option_data->polort = ival;
        nopt++;
        continue;
      }

      /*----- -legendre AND -nolegendre [15 Jul 2004] -----*/

      if( strstr(argv[nopt],"legendre") != NULL ){
        legendre_polort = (strncmp(argv[nopt],"-leg",4) == 0) ;
        nopt++ ; continue ;
      }

      /*----- -nosvd [20 Jul 2004] -----*/

      if( strstr(argv[nopt],"svd") != NULL ){
        use_psinv = (strncmp(argv[nopt],"-svd",4) == 0) ;
        nopt++ ; continue ;
      }

      /*----- -nodmbase [12 Aug 2004] -----*/

      if( strstr(argv[nopt],"dmbase") != NULL ){
        demean_base = (strncmp(argv[nopt],"-dmb",4) == 0) ;
        nopt++ ; continue ;
      }

      /*----- -noxsave [25 Jul 2004] -----*/

      if( strstr(argv[nopt],"xsave") != NULL ){
        xsave = (strncmp(argv[nopt],"-xsave",5) == 0) ;
        nopt++ ; continue ;
      }

      /*----- -xrestore [26 Jul 2004] -----*/

      if( strcmp(argv[nopt],"-xrestore") == 0 ){
        nopt++;
        if( nopt >= argc) DC_error ("need argument after -xrestore ");
        xrestore_filename = strdup( argv[nopt] ) ;
        if( !THD_is_file(xrestore_filename) )
          DC_error("file named after -xrestore doesn't exist") ;
        xrestore = 1 ; nopt++ ; continue ;
      }

      /*-----   -quiet   -----*/
      if (strcmp(argv[nopt], "-quiet") == 0)
      {
        option_data->quiet = 1;  verb = 0 ;
        nopt++;
        continue;
      }

      if (strncmp(argv[nopt],"-verb",5) == 0){  /* 01 Mar 2007 */
        verb++ ; nopt++ ; continue ;
      }

      /*-----   -float, etc.   -----*/
      if( strcmp(argv[nopt],"-float") == 0 ){    /* 13 Mar 2007 */
        floatout = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-short") == 0 ){
        floatout = 0 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-datum") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need argument after -datum");
             if( strcmp(argv[nopt],"float") == 0 ) floatout = 1 ;
        else if( strcmp(argv[nopt],"short") == 0 ) floatout = 0 ;
        else              DC_error("illegal name after -datum") ;
        nopt++ ; continue ;
      }

      /*-----   -progress n  -----*/
      if (strcmp(argv[nopt], "-progress") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -progress ");
        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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
        fval = -666.0 ; sscanf (argv[nopt], "%f", &fval);
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
        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if (ival < 0)
          {
            DC_error ("-num_stimts num   Require: num >= 0 ");
          }

        initialize_stim_options (option_data, ival);

        nopt++;
        continue;
      }

      /*-----  -TR_times basis_dtout [16 Aug 2004]  -----*/
      if( strcmp(argv[nopt],"-TR_times") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need argument after -TR_times") ;
        basis_dtout = -1.0 ; sscanf( argv[nopt] , "%f" , &basis_dtout ) ;
        if( basis_dtout <= 0.0f )
          ERROR_exit("-TR_times '%s' is illegal\n",argv[nopt]) ;
        nopt++ ; continue ;
      }

      /*-----  -TR_irc irc_dt [08 Sep 2004]  -----*/
      if( strcmp(argv[nopt],"-TR_irc") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need argument after -TR_irc") ;
        irc_dt = -1.0 ; sscanf( argv[nopt] , "%f" , &irc_dt ) ;
        if( irc_dt <= 0.0f )
          ERROR_exit("-TR_irc '%s' is illegal\n",argv[nopt]) ;
        nopt++ ; continue ;
      }

      /*----- -basis_normall a  [28 Apr 2005] -----*/

      if( strcmp(argv[nopt],"-basis_normall") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need argument after -basis_normall") ;
        basis_normall = strtod( argv[nopt] , NULL ) ;
        if( basis_normall <= 0.0f )
          DC_error("value after -basis_normall is illegal!") ;
        nopt++ ; continue ;
      }

      /*-----  -stim_times k sname rtype [10 Aug 2004]  -----*/
      if( strncmp(argv[nopt],"-stim_times",11) == 0 ){
        char *suf = argv[nopt]+11 , *sopt=argv[nopt] ;
        nopt++ ;
        if( nopt+2 >= argc )
          ERROR_exit("need 3 arguments after %s",sopt) ;
        ival = -1 ; sscanf( argv[nopt] , "%d" , &ival ) ;
        if( ival < 1 || ival > option_data->num_stimts )
          ERROR_exit("'-stim_times %d' value out of range 1..%d\n",
                     ival , option_data->num_stimts ) ;
        k = ival-1 ; nopt++ ;
        if( option_data->stim_filename[k] != NULL )
          ERROR_exit("'%s %d' trying to overwrite previous stimulus",
                     sopt , ival ) ;
        option_data->stim_filename[k] = strdup( argv[nopt] ) ;
        if( *suf == '\0' )    /* 1 number per time */
          basis_times[k] = mri_read_ascii_ragged( argv[nopt] , basis_filler ) ;
        else                  /* 2 numbers per time */
          basis_times[k] = mri_read_ascii_ragged_complex(argv[nopt],basis_filler);
        if( basis_times[k] == NULL )
          ERROR_exit("'%s %d' can't read file '%s'\n",
                     sopt , ival , argv[nopt] ) ;
        nopt++ ;
        basis_stim[k] = basis_parser( argv[nopt] ) ;  /* regression model */
        if( basis_stim[k] == NULL )
          ERROR_exit("'%s %d' don't understand '%s'\n",
                     sopt , ival , argv[nopt] ) ;
        basis_stim[k]->nparm = basis_stim[k]->nfunc ; /* # of params */
        if( *suf == '\0' )                            /* 08 Mar 2007 */
          basis_stim[k]->type = BASIS_SINGLE ;
        else if( strcmp(suf,"_AM1") == 0 )            /* amplitude */
          basis_stim[k]->type = BASIS_MODULATED_MONO; /* modulation */
        else if( strcmp(suf,"_AM2") == 0 ){
          basis_stim[k]->type = BASIS_MODULATED_PAIR;
          basis_stim[k]->nparm *= 2 ;                 /* twice as many */
        } else
          ERROR_exit("'%s %d' -- unrecognized sub-type of '-stim_times' option!",
                     sopt , ival ) ;
        basis_stim[k]->option = strdup(sopt) ; /* 08 Mar 2007 */
        basis_count++ ;
        nopt++ ; continue ;
      }

      /*-----   -slice_base k sname  [12 Aug 2005] ------*/

      if( strcmp(argv[nopt],"-slice_base") == 0 ){
        DC_error(" -slice_base ***NOT IMPLEMENTED YET*** !!!") ;

        nopt++;
        if (nopt+1 >= argc)  DC_error ("need 2 arguments after -slice_base");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-slice_base k sname   Require: 1 <= k <= num_stimts");
        k = ival-1;
        nopt++;
        if( option_data->stim_filename[k] != NULL )
          ERROR_exit("'-slice_base %d' trying to overwrite previous stimulus",
                     ival ) ;

        option_data->stim_filename[k] = malloc(sizeof(char)*THD_MAX_NAME);
        MTEST(option_data->stim_filename[k]);
        strcpy(option_data->stim_filename[k], argv[nopt]);
        option_data->slice_base[k] = 1;
        option_data->stim_base[k]  = 1;   /* mark as being in the baseline */
        nopt++;
        continue;
      }

      /*-----   -stim_file k sname   -----*/
      if (strcmp(argv[nopt], "-stim_file") == 0)
      {
        nopt++;
        if (nopt+1 >= argc)  DC_error ("need 2 arguments after -stim_file");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-stim_file k sname   Require: 1 <= k <= num_stimts");
        k = ival-1;
        nopt++;
          if( option_data->stim_filename[k] != NULL )
            ERROR_exit("'-stim_file %d' trying to overwrite previous stimulus\n",
                    ival ) ;

        option_data->stim_filename[k] = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->stim_filename[k]);
        strcpy (option_data->stim_filename[k], argv[nopt]);
        nopt++;
        continue;
      }


      /*-----   -stim_label k slabel   -----*/
      if (strcmp(argv[nopt], "-stim_label") == 0)
      {
        char *qq ;
        nopt++;
        if (nopt+1 >= argc)  DC_error ("need 2 arguments after -stim_label");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-stim_label k slabel   Require: 1 <= k <= num_stimts");
        k = ival-1;
        nopt++;

        if( strncmp(option_data->stim_label[k],"Stim#",5) != 0 )
          WARNING_message("-stim_label %d '%s' replacing old label '%s'",
                          ival , argv[nopt] , option_data->stim_label[k] ) ;

        strcpy (option_data->stim_label[k], argv[nopt]);
        nopt++; continue;
      }


      /*-----   -stim_base k   -----*/
      if (strcmp(argv[nopt], "-stim_base") == 0)
      {
        nopt++;
        if (nopt >= argc)
          DC_error ("need 1 argument after -stim_base");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-stim_base k   Require: 1 <= k <= num_stimts");
        k = ival-1;
        option_data->stim_base[k] = 1;    /* mark as being in the baseline */
        nopt++;
        continue;
      }


      /*-----   -stim_minlag k lag   -----*/
      if (strcmp(argv[nopt], "-stim_minlag") == 0)
      {
        nopt++;
        if (nopt+1 >= argc)
          DC_error ("need 2 arguments after -stim_minlag");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-stim_minlag k lag   Require: 1 <= k <= num_stimts");
        k = ival-1;
        nopt++;

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-stim_maxlag k lag   Require: 1 <= k <= num_stimts");
        k = ival-1;
        nopt++;

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_stimts))
          DC_error ("-stim_nptr k p   Require: 1 <= k <= num_stimts");
        k = ival-1;
        nopt++;

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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
        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if (ival < 1)
          {
            DC_error ("-glt s gltname  Require: s >= 1  (s = #rows in GLT)");
          }
        s = ival;

        if (option_data->num_glt == 0)
          initialize_glt_options (option_data, 10);   /* default limit on GLTs */

        if (iglt+1 > option_data->num_glt)
          DC_error ("Use -num_glt option to specify number of GLTs");

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

      /*---- -gltsym gltname [29 Jul 2004] -----*/

      if( strcmp(argv[nopt],"-gltsym") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need 1 argument after -gltsym") ;

        if( option_data->num_glt == 0 )
          initialize_glt_options(option_data,10) ;   /* default limit on GLTs */

        if( iglt+1 > option_data->num_glt )
          DC_error("Use -num_glt option to specify number of GLTs when more than 10") ;

        option_data->glt_rows[iglt] = -1 ;  /* flag for symbolic read */

        option_data->glt_filename[iglt] = strdup( argv[nopt] ) ;
        iglt++ ; nopt++ ; continue ;
      }


      /*-----   -glt_label k glabel   -----*/
      if (strcmp(argv[nopt], "-glt_label") == 0)
      {
        nopt++;
        if (nopt+1 >= argc)  DC_error ("need 2 arguments after -glt_label");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
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
        option_data->nobout = 1; nopt++; continue;
      }
      if( strcmp(argv[nopt],"-bout") == 0 ){          /* 13 Mar 2007 */
        option_data->nobout = 0 ; nopt++ ; continue ;
      }


      /*-----   -nocout   -----*/
      if (strcmp(argv[nopt], "-nocout") == 0)
      {
        option_data->nocout = 1; nopt++; continue;
      }


      /*-----   -full_first   -----*/
      if (strcmp(argv[nopt], "-full_first") == 0)
      {
        option_data->full_first = 1; nopt++; continue;
      }
      if (strcmp(argv[nopt], "-nofull_first") == 0){    /* 13 Mar 2007 */
        option_data->full_first = 0; nopt++; continue;
      }
      if (strcmp(argv[nopt], "-nofullf_atall") == 0){   /* 23 Mar 2007 */
        option_data->do_fullf = 0; nopt++; continue;
      }


      /*-----   -bucket filename   -----*/
      if (strcmp(argv[nopt], "-bucket") == 0 || strcmp(argv[nopt],"-prefix") == 0 )
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need file prefixname after -bucket ");
        option_data->bucket_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->bucket_filename);
        strcpy (option_data->bucket_filename, argv[nopt]);
        nopt++;
        continue;
      }

      /*-----   -nobucket    [03 May 2007]  -----*/
      if( strcmp(argv[nopt],"-nobucket") == 0 ){
        option_data->nobucket = 1 ; nopt++ ; continue ;
      }

      /*-----   -cbucket filename   -----*/
      if (strcmp(argv[nopt], "-cbucket") == 0 || strcmp(argv[nopt],"-cprefix") == 0 )
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need file prefixname after -cbucket ");
          CoefFilename = strdup( argv[nopt] ) ;
        nopt++; continue;
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
          INFO_message("setting number of processes to 1!") ;
          proc_numjob = 1 ;
        } else if( proc_numjob > PROC_MAX ){
          INFO_message("setting number of processes to %d!",PROC_MAX);
          proc_numjob = PROC_MAX ;
        }
#else
        WARNING_message("-jobs not supported in this version\n") ;
#endif
        proc_use_jobs = 1 ;     /* -jobs opt given    2003.08.15 [rickr] */
        nopt++; continue;
      }


      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      DC_error (message);

    } /***** end of loop over input arguments ****/

  /*----- 23 Mar 2007: full first stuff? -----*/

  if( option_data->fout ) option_data->do_fullf = 1 ;

  /*----- Set number of GLTs actually present -----*/
  option_data->num_glt = iglt;

  /*---- if -jobs is given, make sure are processing 3D data ----*/

#ifdef PROC_MAX
  if( (xrestore || option_data->input1D_filename != NULL) && proc_numjob > 1 ){
    proc_numjob = 1 ;
    if( verb ) INFO_message("-jobs reset to 1") ;
  }
#endif

  /*---- 09 Mar 2007: manufacture -bucket name if needed ----*/

  if( option_data->bucket_filename == NULL &&
      option_data->input_filename  != NULL && !option_data->nobucket ){
    option_data->bucket_filename = strdup("Decon") ;
    INFO_message("No '-bucket' option given ==> using '-bucket %s'",
                 option_data->bucket_filename) ;
  }

  /*---- 09 Mar 2007: output -x1D file always ----*/

  if( option_data->x1D_filename == NULL && !option_data->nox1D ){
    char *pref=NULL , *cpt ;
    if( option_data->bucket_filename != NULL ){         /* use bucket name? */
      pref = strdup(option_data->bucket_filename) ;
      cpt = strstr(pref,".") ; if( cpt != NULL ) *cpt = '\0' ;
    } else if( option_data->input1D_filename != NULL ){ /* use 1D filename? */
      pref = strdup(option_data->input1D_filename) ;
      cpt = strstr(pref,".1D") ; if( cpt != NULL ) *cpt = '\0' ;
    } else if( option_data->nodata ){                   /* no data? */
      pref = strdup("nodata") ;
    } else {                                            /* default */
      pref = strdup("Decon") ;
    }
    option_data->x1D_filename = malloc(strlen(pref)+32) ;
    strcpy(option_data->x1D_filename,pref) ;
    strcat(option_data->x1D_filename,".x1D") ;
    free(pref) ;
  }

  if( !legendre_polort && option_data->polort == -666 ){
    WARNING_message("-nolegendre and -polort A are incompatible!") ;
    legendre_polort = 1 ;
  }

  /**--- Test various combinations for legality [11 Aug 2004] ---**/

#if 0
  if (option_data->input1D_TR > 0.0 && !option_data->input1D_filename) {
    option_data->input1D_TR = 0.0;
    if( verb ) WARNING_message("-TR_1D is meaningless without -input1D");
  }
#endif

  if( option_data->polort == -1 ) demean_base = 0 ;  /* 12 Aug 2004 */

  nerr = 0 ;
  for( k=0 ; k < option_data->num_stimts ; k++ ){

    if( basis_stim[k] != NULL ){    /* checking -stim_times input */

      basis_stim[k]->name = strdup( option_data->stim_label[k] ) ;

      if( option_data->sresp_filename[k] != NULL ) basis_need_mse = 1 ;

      if( option_data->stim_nptr[k] != 1 ){
        ERROR_message(
                "'-stim_nptr %d %d' illegal with '%s'\n",
                k+1 , option_data->stim_nptr[k] , basis_stim[k]->option ) ;
        nerr++ ;
      }
      if( option_data->stim_base[k] != 0 ){
        ERROR_message(
                "'-stim_base %d' illegal with '%s'\n",
                k+1 , basis_stim[k]->option ) ;
        nerr++ ;
      }
      if( option_data->stim_minlag[k] != 0 ){
        ERROR_message(
                "'-stim_minlag %d %d' illegal with '%s'\n",
                k+1 , option_data->stim_minlag[k] , basis_stim[k]->option ) ;
        nerr++ ;
      }
      if( option_data->stim_maxlag[k] != 0 ){
        ERROR_message(
                "'-stim_maxlag %d %d' illegal with '%s'\n",
                k+1 , option_data->stim_maxlag[k] , basis_stim[k]->option ) ;
        nerr++ ;
      }

    } else {    /* checking -stim_file type of input */

      if( option_data->stim_filename[k] == NULL ){
        ERROR_message("no stimulus #%d given\n",k+1) ;
        nerr++ ;
      }

      if( option_data->slice_base[k] ){          /* not yet implemented! */
        if( option_data->stim_minlag[k] != 0 ){
          ERROR_message(
                  "'-stim_minlag %d %d' illegal with '-slice_base'\n",
                  k+1 , option_data->stim_minlag[k] ) ;
          nerr++ ;
        }
        if( option_data->stim_maxlag[k] != 0 ){
          ERROR_message(
                  "'-stim_maxlag %d %d' illegal with '-slice_base'\n",
                  k+1 , option_data->stim_maxlag[k] ) ;
          nerr++ ;
        }
        if( option_data->stim_nptr[k] != 1 ){
          ERROR_message(
                  "'-stim_nptr %d %d' illegal with '-slice_base'\n",
                  k+1 , option_data->stim_nptr[k] ) ;
          nerr++ ;
        }
      }
    }

  } /* end of loop over stimuli */

  /* check CENSOR command for run indexes: should all have them, or none */

  if( abc_CENSOR != NULL ){
    int ic , rr , nzr=0 ;
    for( ic=0 ; ic < num_CENSOR ; ic++ ) /* count number with run=0 */
      if( abc_CENSOR[ic].i ) nzr++ ;
    if( nzr > 0 && nzr < num_CENSOR )
      WARNING_message("%d -CENSORTR commands have run: numbers and %d do not!" ,
                      num_CENSOR-nzr , nzr ) ;
  }

  /*-- check if we can continue! --*/

  if( nerr > 0 ) ERROR_exit("Can't continue after above problems!") ;
  return ;
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


ENTRY("read_time_series") ;

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
    WARNING_message("time series file %s has %d columns",ts_filename,ny);
  }


  /*----- Save the time series data -----*/
  *ts_length = nx;
  ts_data = (float *) malloc (sizeof(float) * nx);
  MTEST (ts_data);
  for (ipt = 0;  ipt < nx;  ipt++)
    ts_data[ipt] = far[ipt + iy*nx];   /* N.B.: iy=0 */


  mri_free (flim);  flim = NULL;

  RETURN (ts_data);
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
  column_metadata cd ;     /* 05 Mar 2007 */
  int nblk,npol , p1,nc ;
  unsigned int mk,gp ;
  float dtloc=0.0f ;


ENTRY("read_input_data") ;

  /*----- Initialize local variables -----*/

  num_stimts = option_data->num_stimts;
  num_glt    = option_data->num_glt;
  baseline   = option_data->stim_base;
  min_lag    = option_data->stim_minlag;
  max_lag    = option_data->stim_maxlag;


  /*----- Read the input stimulus time series -----*/

  if (num_stimts > 0)
    {
      int nerr=0 ;

      *stimulus = (float **) calloc (sizeof(float *) , num_stimts);
      MTEST (*stimulus);
      *stim_length = (int *) calloc (sizeof(int)     , num_stimts);
      MTEST (*stim_length);

      /** 14 Jul 2004: check for duplicate filename - RWCox **/

      { int js ;
        for( is=0 ; is < num_stimts ; is++ ){
          for( js=is+1 ; js < num_stimts ; js++ ){
            if( strcmp( option_data->stim_filename[is] ,
                        option_data->stim_filename[js]  ) == 0 ){
              WARNING_message(
                "!! stimulus filename #%d:'%s' same as #%d:'%s'" ,
                is+1,option_data->stim_filename[is] ,
                js+1,option_data->stim_filename[js]  ) ; nerr++ ;
              badlev++ ;
            }
          }
        }
      }

      if( nerr > 0 && AFNI_yesenv("AFNI_3dDeconvolve_nodup") )
        ERROR_exit("Can't continue after above warnings!\n");

      for (is = 0;  is < num_stimts;  is++)
      {
        if( basis_stim[is] != NULL ) continue ;  /* 11 Aug 2004: skip thisn */

        if( !option_data->slice_base[is] ){  /**** ordinary input file ****/

          (*stimulus)[is] = read_time_series (option_data->stim_filename[is],
                                              &((*stim_length)[is]));

          if ((*stimulus)[is] == NULL)
            {
              sprintf (message,  "Unable to read '-stim_file %d %s'",
                       is+1 , option_data->stim_filename[is]);
              DC_error (message);
            }

        } else {  /**** read a -slice_base file ****/

          MRI_IMAGE *sim ;
          sim = mri_read_1D( option_data->stim_filename[is] ) ;
          if( sim == NULL ){
            sprintf (message,  "Unable to read '-slice_base %d %s'",
                   is+1 , option_data->stim_filename[is]);
            DC_error (message);
          }
          (*stim_length)[is] = sim->nx ;             /* save length */
          if( sim->ny > 1 ){                         /* multicolumn */
            option_data->slice_base[is] = sim->ny ;
            option_data->num_slice_base++ ;
          } else {                                   /* uni-column! */
            option_data->slice_base[is] = 0 ;
            WARNING_message("'slice_base %d %s' has only 1 column",
                            is+1 , option_data->stim_filename[is] ) ;
          }
          (*stimulus)[is] = MRI_FLOAT_PTR(sim) ;
          if( sim->nx < 2 )
            WARNING_message("-slice_base %d %s: %d rows, %d cols ???",
                    is+1 , option_data->stim_filename[is] , sim->nx,sim->ny ) ;
        }

      }
    }  /* end of input of -stim_file and -slice_base files */


  *num_blocks = 0 ;  /* 04 Aug 2004 */

  /*----- Read the input fMRI measurement data -----*/

  if (option_data->nodata)  /*----- No input data -----*/
    {
      if (num_stimts <= 0)
        DC_error ("Must have num_stimts > 0 for -nodata option");

      if( option_data->num_slice_base > 0 )
        ERROR_exit("'-nodata' and '-slice_base' are incompatible!") ;

      if( option_data->nodata_TR > 0.0 ) dtloc = option_data->nodata_TR ;

      if( basis_count > 0 ){
             if( option_data->nodata_TR > 0.0 ) basis_TR = option_data->nodata_TR ;
        else if( basis_dtout            > 0.0 ) basis_TR = basis_dtout ;
        INFO_message("using TR=%g seconds for -stim_times and -nodata\n",basis_TR);
      }

      *dset_time = NULL;

      /* with no actual data, must determine matrix size somehow */

      if( option_data->nodata_NT > 0 )  /* user gave it to us directly? */
        nt = option_data->nodata_NT ;
      else if( option_data->NLast > 0 ) /* a little indirectly? */
        nt = option_data->NLast+1 ;
      else {                            /* use the longest -stim_file input? */
        int qt ;
        for( qt=nt=is=0 ; is < num_stimts ; is++ ){
          if( basis_stim[is] == NULL ){
            qt = (*stim_length)[is] / option_data->stim_nptr[is] ;
            nt = MAX(nt,qt) ;
          }
        }
        if( nt == 0 )
          ERROR_exit("-nodata has no way to determine time series length to model");
      }
      INFO_message("using NT=%d time points for -nodata",nt) ;

      nxyz = 0;  /* signal that there is no data */
    }

  else if (option_data->input1D_filename != NULL) /*----- 1D input file -----*/
    {

      if( option_data->num_slice_base > 0 )
        ERROR_exit("'-input1D' and '-slice_base' are incompatible!") ;

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
      option_data->nobout = 0 ;

      if (option_data->input1D_TR > 0.0)
        dtloc = basis_TR = option_data->input1D_TR;
      if (verb) INFO_message("1D TR is %.3f seconds", basis_TR);
   }

  else if (option_data->input_filename != NULL) /*----- 3d+time dataset -----*/
    {
      if( verb ) INFO_message("reading dataset %s",option_data->input_filename);
      *dset_time = THD_open_dataset (option_data->input_filename);
      CHECK_OPEN_ERROR(*dset_time,option_data->input_filename);
      DSET_load(*dset_time) ; CHECK_LOAD_ERROR(*dset_time) ;
      if( (*dset_time)->taxis == NULL )
        WARNING_message("dataset '%s' has no time axis",
                       option_data->input_filename) ;

      nt   = DSET_NUM_TIMES (*dset_time);
      nxyz = DSET_NVOX (*dset_time);

      DSET_UNMSEC( *dset_time ) ; /* 12 Aug 2005: surgery on the time units? */

      if( option_data->num_slice_base > 0 ){  /* check slice counts */
        int nz=DSET_NZ(*dset_time) , nerr=0 ;
        for( is=0 ; is < num_stimts ; is++ ){
          if( option_data->slice_base[is] >  0  &&
              option_data->slice_base[is] != nz   ){
            nerr++ ;
            ERROR_message("Dataset nz=%d but '-slice_base %d' has nz=%d",
                          nz, is+1, option_data->slice_base[is] ) ;
          }
        }
        if( nerr > 0 ) ERROR_exit("Can't continue from nz mismatch!") ;

        if( nz == 1 ){                           /* only 1 slice? */
          for( is=0 ; is < num_stimts ; is++ )   /* get rid of   */
            option_data->slice_base[is] = 0 ;    /* slice_base  */
          option_data->num_slice_base = 0 ;      /* markings!  */
        }
      }

      dtloc = basis_TR = DSET_TR(*dset_time) ;          /* 11 Aug 2004 */
      if( basis_TR <= 0.0f ){
        if( option_data->input1D_TR > 0.0f ){
          dtloc = basis_TR = option_data->input1D_TR ;
          WARNING_message("no TR in dataset: using -TR_1D=%.3f s",dtloc) ;
        } else {
          dtloc = basis_TR = 1.0f;
          WARNING_message("no TR in dataset; setting TR=1 s");
        }
      } else if( basis_TR <= 0.10f ){
        WARNING_message("TR in dataset = %g s (less than 0.100 s) -- FYI",
                        basis_TR ) ;  /* the PSFB syndrome */
      }

      if( DSET_IS_TCAT(*dset_time) ){  /** 04 Aug 2004: manufacture block list **/
        if( option_data->concat_filename != NULL ){
          WARNING_message(
             "'-concat %s' ignored: input dataset is auto-catenated\n" ,
             option_data->concat_filename ) ;
          option_data->concat_filename = NULL ;
        }
        *num_blocks = (*dset_time)->tcat_num ;
        *block_list = (int *) malloc (sizeof(int) * (*num_blocks));
        (*block_list)[0] = 0;
        for( it=0 ; it < (*num_blocks)-1 ; it++ )
          (*block_list)[it+1] = (*block_list)[it] + (*dset_time)->tcat_len[it] ;
        if( verb ){
          char *buf=calloc((*num_blocks),8) ;
          for( it=0 ; it < (*num_blocks) ; it++ )
            sprintf(buf+strlen(buf)," %d",(*block_list)[it]) ;
          INFO_message("Auto-catenated datasets start at: %s", buf) ;
          free(buf) ;
        }
      }

      if( option_data->automask ){            /* 15 Apr 2005: automasking */
        MRI_IMAGE *qim ; int mc ;
        qim = THD_rms_brick( *dset_time ) ;
        *mask_vol = mri_automask_image( qim ) ;
        mri_free( qim ) ;
        if( *mask_vol == NULL ){
          WARNING_message("unable to generate automask?!") ;
        } else {
          mc = THD_countmask( DSET_NVOX(*dset_time) , *mask_vol ) ;
          if( mc <= 1 ){
            WARNING_message("automask is empty!?") ;
            free(*mask_vol) ; *mask_vol = NULL ;
          } else {
            INFO_message("%d voxels in automask (out of %d)",
                         mc,DSET_NVOX(*dset_time)) ;
          }
        }
      }

      if (option_data->mask_filename != NULL)   /* read mask from file */
         {
           THD_3dim_dataset * mask_dset = NULL;

           /*----- Read the input mask dataset -----*/
           mask_dset = THD_open_dataset (option_data->mask_filename);
           CHECK_OPEN_ERROR(mask_dset,option_data->mask_filename);

           /*----- If mask is used, check for compatible dimensions -----*/
           if (    (DSET_NX(*dset_time) != DSET_NX(mask_dset))
                || (DSET_NY(*dset_time) != DSET_NY(mask_dset))
                || (DSET_NZ(*dset_time) != DSET_NZ(mask_dset)) )
             {
               sprintf (message, "datasets '%s' and '%s' have incompatible dimensions",
                     option_data->input_filename, option_data->mask_filename);
               DC_error (message);
             }

           if (DSET_NVALS(mask_dset) != 1 )
             DC_error ("Must specify exactly 1 sub-brick from mask dataset");

           *mask_vol = THD_makemask( mask_dset , 0 , 1.0,0.0 ) ;
           if (*mask_vol == NULL)  DC_error ("Unable to read mask dataset");

           DSET_delete(mask_dset) ;
         }

    } else {                                   /* no input data? */
      DC_error ("Must specify some sort of input data, or '-nodata'");
    }


  /*----- Check number of data (time) points -----*/

  if (nt <= 0)      DC_error ("No time points?");
  option_data->nt = nt;
  if (nxyz < 0)     DC_error ("Program initialization error: nxyz < 0");
  option_data->nxyz = nxyz;

  voxel_num = nxyz ;  /* 31 Aug 2004 */

  /*----- Read the block list (-concat option) -----*/

  if (option_data->concat_filename == NULL)
    {
      if( *num_blocks <= 0 ){   /* make one big block, if not already set */
        *num_blocks = 1;
        *block_list = (int *) malloc (sizeof(int) * 1);
        (*block_list)[0] = 0;
      }
    }
  else
    {
      float *f = NULL;

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

  /*** 16 Mar 2007:
       estimate desirable polort level from max block duration,
       and print warning if actual polort is smaller than this. ***/

   { int nbl=*num_blocks , *bl=*block_list ;
     int ibot , itop , ilen , lmax=0 ; float dmax ;
     for( it=0 ; it < nbl ; it++ ){   /* find longest block */
       ibot = bl[it] ;
       itop = (it < nbl-1) ? bl[it+1] : nt ;
       ilen = itop-ibot ;
       if( ilen > lmax ) lmax = ilen ;
     }
     if( dtloc <= 0.0f ) dtloc = 1.0f ;
     dmax = dtloc * lmax ;                /* duration of longest block */
          if( dmax <= 150.0 ) ilen = 1 ;
     else if( dmax <= 300.0 ) ilen = 2 ;
     else                     ilen = 1+(int)floor(dmax/150.0) ;
     switch( option_data->polort ){
       default:                           /* user supplied non-negative polort */
         if( option_data->polort < ilen )
           WARNING_message(
            "Input polort=%d; Imaging duration=%.1f s; Recommended minimum polort=%d",
            option_data->polort , dmax , ilen ) ;
         else
           INFO_message(
            "Input polort=%d; Imaging duration=%.1f s; Recommended minimum polort=%d ++ OK ++",
            option_data->polort , dmax , ilen ) ;
       break ;

       case -1: break ;                   /* user orders no baseline at all */

       case -666:                         /* user orders automatic polort */
         INFO_message("Imaging duration=%.1f s; Automatic polort=%d",dmax,ilen) ;
         option_data->polort = ilen ;
       break ;
     }
   }

  /*-- Create timing for each -stim_times input [11 Aug 2004] --*/
  /*-- Modified to deal with amplitude modulation [08 Mar 2007] --*/

  if( basis_count > 0 ){             /* if there are any, that is */
    MRI_IMAGE *tim , *qim , *aim , *bim ;
    float     *tar , *qar , *aar , *zar ;
    float toff , tmax,tt,ss , zbar ;
    int   ii,jj , kk , ngood , nx,ny , nf,dd , btyp , nbas ;
    int   nbl=*num_blocks , *bst=*block_list ;
    basis_expansion *be ;

    if( basis_TR    <= 0.0f ) basis_TR    = 1.0f ;
    if( basis_dtout <= 0.0f ) basis_dtout = basis_TR ;

    INFO_message("-stim_times using TR=%g seconds",basis_TR) ;

    for( is=0 ; is < num_stimts ; is++ ){
      be = basis_stim[is] ;
      if( be == NULL ) continue ;   /* old style -stim_file: BAH! */

      aim = bim = NULL ; aar = zar = NULL ;

      btyp = be->type ;  /* 08 Mar 2007: sub-type of -stim_times */

      /** convert entries to global time indexes **/

      tim = basis_times[is] ;        /* image that contains times */
      nx = tim->nx ; ny = tim->ny ;  /* read in from user's file */

      if( tim->kind == MRI_complex ){     /* 08 Mar 2007: pairs */
        MRI_IMARR *qimar=mri_complex_to_pair(tim); /* split them up */
        bim = IMARR_SUBIM(qimar,0); aim = IMARR_SUBIM(qimar,1);
        aar = MRI_FLOAT_PTR(aim);   tar = MRI_FLOAT_PTR(bim);
        FREE_IMARR(qimar);
      } else {
        tar = MRI_FLOAT_PTR(tim);  /* just times, no paired value */
      }

      ngood = 0 ;                  /* number of good time values found */
      qar   = (float *)calloc(sizeof(float),nx*ny) ; /* collects times */
      if( aar != NULL )
        zar = (float *)calloc(sizeof(float),nx*ny) ; /* collects paired val */

      if( nx == 1 ){                     /** 1 column = global times **/
        int nbad=0 , nout=0 ;
        INFO_message("'%s %d' using GLOBAL times",be->option,is+1) ;
        tmax = (nt-1)*basis_TR ;         /* max allowed time offset */
        for( ii=0 ; ii < ny ; ii++ ){    /* loop over all input times */
          tt = tar[ii] ;
               if( tt >= 0.0f && tt <= tmax ){
                              if( zar != NULL ) zar[ngood  ] = aar[ii] ;
                                                qar[ngood++] = tt/basis_TR ;
          } else if( tt >= basis_filler       ) nbad++ ; /* '*' entries */
            else                                nout++ ; /* PSFB entries */
        }
        if( nbad )          /* warn about '*' times in GLOBAL input */
          WARNING_message(
           "'%s %d' (GLOBAL) has %d '*' fillers; do you want LOCAL times?",
           be->option , is+1 , nbad ) ;
        if( nout )          /* warn about times outside the legal range */
          WARNING_message(
           "'%s %d' (GLOBAL) has %d times outside range 0 .. %g [PSFB syndrome]",
           be->option , is+1 , nout , tmax ) ;

      } else {                           /** multicol => 1 row per block **/

        int nout ;
        INFO_message("'%s %d' using LOCAL times",be->option,is+1) ;
        if( ny != nbl ){                 /* times are relative to block */
          WARNING_message(
                  "'%s %d' file '%s' has %d rows,"
                  " but dataset has %d time blocks" ,
                  be->option, is+1, option_data->stim_filename[is], ny,nbl ) ;
          if( ny > nbl ) ny = nbl ;
        }
        for( jj=0 ; jj < ny ; jj++ ){   /* jj=row index=block index */
          if( jj < nbl-1 ) tmax = (bst[jj+1]-1-bst[jj])*basis_TR ;
          else             tmax = (nt       -1-bst[jj])*basis_TR ;
          for( nout=ii=0 ; ii < nx ; ii++ ){
            tt = tar[ii+jj*nx] ;
            if( tt >= 0.0f && tt <= tmax ){
               if( zar != NULL ) zar[ngood  ] = aar[ii+jj*nx] ;
                                 qar[ngood++] = tt/basis_TR+bst[jj] ;
            } else if( tt < basis_filler ) nout++ ; /* PSFB entries */
          }
          if( nout )
            WARNING_message(
             "'%s %d' (LOCAL) run#%d has %d times outside range 0 .. %g [PSFB syndrome]",
             be->option , is+1 , jj+1 , nout , tmax ) ;
        }
      } /* end of converting times into indexes */

      /* create qim image to hold time indexes (and paired vals, if present) */

      if( ngood == 0 ){    /* WTF? no good values found */

        WARNING_message(
                "!! '%s %d' file '%s' has no good time values\n",
                be->option , is+1 , option_data->stim_filename[is] ) ;
        free((void *)qar) ; qim = NULL ; qar = NULL ;
        if( zar != NULL ){ free((void *)zar); zar = NULL; }
        badlev++ ;

      } else if( zar == NULL ){  /* no paired values */

        qim = mri_new_vol_empty(ngood,1,1,MRI_float) ;
        qar = (float *)realloc((void *)qar,sizeof(float)*ngood) ;
        mri_fix_data_pointer( qar , qim ) ;

      } else {                   /* 08 Mar 2007: have paired values */

        MRI_IMAGE *yim, *zim ; int nzer , nzb , nbad ;
        yim = mri_new_vol_empty(ngood,1,1,MRI_float) ;
        qar = (float *)realloc((void *)qar,sizeof(float)*ngood) ;
        mri_fix_data_pointer( qar , yim ) ;  /* image of times */
        zim = mri_new_vol_empty(ngood,1,1,MRI_float) ;
        zar = (float *)realloc((void *)zar,sizeof(float)*ngood) ;
        mri_fix_data_pointer( zar , zim ) ;  /* image of paired vals */
        qim = mri_pair_to_complex(yim,zim) ; /* join them! */
        mri_free(aim); mri_free(bim);        /* created earlier */
        mri_clear_data_pointer(yim); mri_free(yim); /* qar and zar */
        mri_clear_data_pointer(zim); mri_free(zim); /* are needed below */

        zbar = 0.0f ;
        for( nzb=nbad=nzer=kk=0 ; kk < ngood ; kk++ ){  /* check for bad vals */
               if( zar[kk] == 0.0f         ){ nzer++ ; nzb++ ;          }
          else if( zar[kk] >= basis_filler ){ nbad++ ; zar[kk] = 0.0f ; }
          else                              { nzb++  ; zbar += zar[kk]; }
        }
        if( nzer > 0 || nbad > 0 ){  /* report bad paired vals */
          WARNING_message(
           "%s '%s %d' file '%s' amplitudes: #times=%d #zero=%d #undefined=%d",
           (nbad > 0) ? "!! " : "\0" ,
           be->option, is+1, option_data->stim_filename[is] , ngood,nzer,nbad ) ;
          if( nbad > 0 ) badlev++ ;
        }
        if( nzb > 0 ) zbar /= nzb ;  /* average paired value */
        INFO_message("'%s %d' average value = %g",be->option, is+1,zbar) ;
      }

      /* replace input times image with time indexes image created above */

      mri_free(basis_times[is]) ; basis_times[is] = qim ;

      /*-- create basis vectors for this model now --*/
      /*-- qar[] = array of time indexes
           zar[] = array of amplitudes (if not NULL) --*/

      nf = basis_stim[is]->nfunc ; nbas = basis_stim[is]->nparm ;
      basis_vect[is] = mri_new( nt , nbas , MRI_float ) ;  /* all zeros */

      if( qar != NULL ){
        int imin,imax , ibot,itop ;
        float *bv = MRI_FLOAT_PTR(basis_vect[is]) ;
        float dbot,dtop , fnt=(float)nt , z1,z2 ;

#if 0
fprintf(stderr,"%s %d: adjusted time indexes follow:\n",be->option,is+1) ;
for( kk=0 ; kk < ngood ; kk++ ) fprintf(stderr," %g",qar[kk]) ;
fprintf(stderr,"\n") ;
#endif

        dbot = be->tbot / basis_TR ; /* range of indexes about each stim time */
        dtop = be->ttop / basis_TR ;
        imin = 0 ; imax = nt-1 ;     /* for the case of nbl=1 */
        z1   = 1.0f ;                /* default amplitude */

        for( kk=0 ; kk < ngood ; kk++ ){   /* for the kk-th stim time */
          tt = qar[kk] ; if( tt < 0.0f || tt >= fnt ) continue ;
          if( zar != NULL ){
            switch( btyp ){
              default:                   z1 = 1.0f ;                   break;
              case BASIS_MODULATED_MONO: z1 = zar[kk] ;                break;
              case BASIS_MODULATED_PAIR: z1 = 1.0f; z2 = zar[kk]-zbar; break;
            }
          }

          if( nbl > 1 ){
            for( jj=1 ; jj < nbl ; jj++ ) if( tt < bst[jj] ) break ;
            jj-- ;                          /* time index tt is in block #jj */
                             imin = bst[jj] ;       /* first index in block */
            if( jj < nbl-1 ) imax = bst[jj+1] - 1 ; /* last index in block */
            else             imax = nt - 1 ;
          }

          /* range of indexes to load with the response model */

          ibot = (int)ceil ( tt + dbot ) ; if( ibot < imin ) ibot = imin ;
          itop = (int)floor( tt + dtop ) ; if( itop > imax ) itop = imax ;

          for( ii=ibot ; ii <= itop ; ii++ ){   /* loop over active interval */
            ss = basis_TR*(ii-tt) ;
            if( ss < be->tbot || ss > be->ttop ) continue ;
            if( z1 != 0.0f ){
              for( jj=0 ; jj < nf ; jj++ )
                bv[ii+jj*nt] +=
                  z1 * basis_funceval( be->bfunc[jj], basis_TR*(ii-tt) );
            }

            if( btyp == BASIS_MODULATED_PAIR && z2 != 0.0f ){
              for( jj=0 ; jj < nf ; jj++ )
                bv[ii+(jj+nf)*nt] +=
                  z2 * basis_funceval( be->bfunc[jj], basis_TR*(ii-tt) );
            }
          }
        } /* end of loop over stimulus times */
#if 0
fprintf(stderr,"-stim_times %d: basis vectors follow:\n",is+1) ;
for( ii=0 ; ii < nt ; ii++ ){
  fprintf(stderr,"%d:",ii) ;
  for( jj=0 ; jj < nbas ; jj++ ) fprintf(stderr," %g",bv[ii+jj*nt]) ;
  fprintf(stderr,"\n") ;
}
#endif

        if( zar != NULL ){ free(qar); free(zar); }  /* for _AM? */

      } /* end of creating basis vectors */

    } /* end of loop over stim functions */
  } /* end of if we have any -stim_times inputs */


  /*----- Determine total number of parameters in the model -----*/

  qp = (option_data->polort + 1) * (*num_blocks);
  q = qp;   /* number of baseline parameters */
  p = qp;   /* number of total parameters */
  for (is = 0;  is < num_stimts;  is++){
    if( basis_stim[is] != NULL ){           /* 11 Aug 2004 */
      basis_stim[is]->pbot = p ;            /* 1st parameter for this model */
      p += basis_stim[is]->nparm ;          /* number of parameters in model */
    } else {
      if (max_lag[is] < min_lag[is])
      DC_error ("Require min lag <= max lag for all stimuli");
      p += max_lag[is] - min_lag[is] + 1;
      if (baseline[is])  q += max_lag[is] - min_lag[is] + 1;
    }
  }
  option_data->p  = p;   /* total number of parameters */
  option_data->q  = q;   /* number of baseline parameters (polort+stim_base) */
  option_data->qp = qp;  /* number of polort baseline parameters */

  /*----- Save info about each column, for later storage [06 Mar 2007] -----*/

  option_data->coldat = (column_metadata *)calloc(sizeof(column_metadata),p) ;
  nblk = *num_blocks ;
  npol = option_data->polort ;

  p1 = 0 ;
  for( is=0 ; is < nblk ; is++ ){
    for( it=0 ; it <= npol ; it++ ){
      cd.mask  = CM_BASELINE_MASK | CM_POLORT_MASK ;
      cd.group = -1 ;                            /* polort group = -1 */
      sprintf(cd.name,"Run#%dPol#%d",is+1,it) ;
      option_data->coldat[p1++] = cd ;
    }
  }
  for( is=0 ; is < num_stimts ; is++ ){
    if( basis_stim[is] != NULL ){
      nc = basis_stim[is]->nparm ;
      mk = 0 ; gp = is+1 ;
    } else {
      nc = max_lag[is] - min_lag[is] + 1 ;
      if( baseline[is] ){ mk = CM_BASELINE_MASK; gp = 0   ; }
      else              { mk = 0               ; gp = is+1; }
    }
    for( it=0 ; it < nc ; it++ ){
      cd.mask  = mk ; cd.group = gp ;
      sprintf(cd.name,"%-1.60s#%d",option_data->stim_label[is],it) ;
      DEBLANK(cd.name) ;
      option_data->coldat[p1++] = cd ;
    }
  }
  coldat = option_data->coldat ;  /* global variable */

  /*----- Read the censorship file (John Ashcroft, we miss you) -----*/

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
      /*----- Create non-censoring censor time series array -----*/
      *censor_array = (float *) malloc (sizeof(float) * nt);
      MTEST (*censor_array);
      *censor_length = nt;
      for (it = 0;  it < nt;  it++)
      (*censor_array)[it] = 1.0;
    }

  /*----- 01 Mar 2007: apply the -CENSORTR commands to censor_array -----*/

  if( abc_CENSOR != NULL ){
    int ic , rr , aa,bb , nerr=0 , bbot,btop , nblk=*num_blocks ;
    for( ic=0 ; ic < num_CENSOR ; ic++ ){  /* loop over CENSOR commands */
      rr = abc_CENSOR[ic].i ;
      aa = abc_CENSOR[ic].j ; if( aa < 0  ) continue ;  /* shouldn't happen */
      bb = abc_CENSOR[ic].k ; if( bb < aa ) continue ;  /* shouldn't happen */
      if( rr == -666 ){  /* run = wildcard ==> expand to nblk new triples */
        int_triple rab ;
        abc_CENSOR = (int_triple *)realloc( abc_CENSOR ,
                                            sizeof(int_triple)*(num_CENSOR+nblk) );
        for( rr=1 ; rr <= nblk ; rr++ ){
          rab.i = rr; rab.j = aa; rab.k = bb; abc_CENSOR[num_CENSOR++] = rab;
        }
        continue ;  /* skip to next one */
      }
      if( rr > 0 ){       /* convert local indexes to global */
        if( rr > nblk ){  /* stupid user */
          ERROR_message("-CENSORTR %d:%d-%d has run index out of range 1..%d",
                        rr,aa,bb , nblk ) ;
          nerr++ ; aa = -66666666 ;
        } else {
          bbot = (*block_list)[rr-1] ;        /* start index of block #rr */
          btop = (rr < nblk) ? (*block_list)[rr]-1 : nt-1 ; /* last index */
          if( aa+bbot > btop ){  /* WTF? */
            WARNING_message(
             "-CENSORTR %d:%d-%d has start index past end of run (%d) - IGNORING",
             rr,aa,bb,btop-bbot ) ; aa = -66666666 ;
          } else if( bb+bbot > btop ){  /* oopsie */
            WARNING_message(
             "-CENSORTR %d:%d-%d has stop index past end of run (%d) - STOPPING THERE",
             rr,aa,bb,btop-bbot ) ;
          }
          aa += bbot ; bb += bbot ; if( bb > btop ) bb = btop ;
        }
      } else {           /* global indexes: check for stupidities */
        if( aa >= nt ){
          WARNING_message(
           "-CENSORTR %d..%d has start index past end of data (%d) - IGNORING",
           rr,aa,bb,nt-1 ) ; aa = -66666666 ;
        } else if( bb > nt ){
          WARNING_message(
           "-CENSORTR %d..%d has stop index past end of data (%d) - STOPPING THERE",
           rr,aa,bb,nt-1 ) ; bb = nt-1 ;
        }
      }
      if( aa < 0  || aa >= nt ) continue ;  /* nothing to do */
      if( bb < aa || bb >= nt ) continue ;
      if( verb > 1 )
        ININFO_message("applying -CENSORTR global time indexes %d..%d",aa,bb) ;
      for( it=aa ; it <= bb ; it++ ) (*censor_array)[it] = 0.0f ;
    } /* end of loop over CENSOR commands */
    if( nerr > 0 ) ERROR_exit("Can't continue! Fix the -CENSORTR error%s",
                              (nerr==1) ? "." : "s." ) ;
    free((void *)abc_CENSOR) ; abc_CENSOR = NULL ; num_CENSOR = 0 ;
  }

  /*----- Build symbolic list of stim names and index ranges [29 Jul 2004] -----*/

  nSymStim = num_stimts+1 ;
  SymStim  = (SYM_irange *)calloc(sizeof(SYM_irange),nSymStim) ;

  strcpy( SymStim[num_stimts].name , "Ort" ) ;
  SymStim[num_stimts].nbot = 0 ;
  SymStim[num_stimts].ntop = qp-1 ;
  SymStim[num_stimts].gbot = 0 ;
  it = qp ;
  for( is=0 ; is < num_stimts ; is++ ){
    MCW_strncpy( SymStim[is].name , option_data->stim_label[is] , 64 ) ;
    if( basis_stim[is] != NULL ){
      SymStim[is].nbot = 0 ;
      SymStim[is].ntop = basis_stim[is]->nparm-1 ;
    } else {
      SymStim[is].nbot = min_lag[is] ;
      SymStim[is].ntop = max_lag[is] ;
    }
    SymStim[is].gbot = it ;
    it += SymStim[is].ntop - SymStim[is].nbot + 1 ;
    if( strchr(SymStim[is].name,' ') != NULL ||
        strchr(SymStim[is].name,'*') != NULL ||
        strchr(SymStim[is].name,';') != NULL   ){
      WARNING_message(
           "-stim_label #%d '%s' has characters bad for -gltsym",
           is+1 , SymStim[is].name ) ;
    }
  }

  /*----- Read the general linear test matrices -----*/

  if (num_glt > 0)
    {
      int ngerr ;
      *glt_cmat = (matrix *) malloc (sizeof(matrix) * num_glt);

      /*----- Initialize general linear test matrices -----*/
      for (iglt = 0;  iglt < num_glt;  iglt++)
      matrix_initialize (&((*glt_cmat)[iglt]));


      for (iglt = 0;  iglt < num_glt;  iglt++)
      {
#if 1
          ngerr = SYM_expand_errcount() ;
          read_glt_matrix( option_data->glt_filename[iglt] ,
                           option_data->glt_rows + iglt ,
                           p , *glt_cmat + iglt              ) ;
          ngerr = SYM_expand_errcount() - ngerr ;
          if( ngerr > 0 )
            ERROR_message("-gltsym errors immediately above from file '%s'",
                          option_data->glt_filename[iglt] ) ;
#else
        matrix_file_read (option_data->glt_filename[iglt],  /* uses MRI_read_1D() */
                      option_data->glt_rows[iglt],
                      p, &((*glt_cmat)[iglt]), 1);
        if ((*glt_cmat)[iglt].elts == NULL)
          {
            sprintf (message,  "Unable to read GLT matrix from file: %s",
                   option_data->glt_filename[iglt]);
            DC_error (message);
          }
#endif
      }

      if( SYM_expand_errcount() > 0 )
        ERROR_exit("Can't continue after the above -gltsym problems!") ;
    }

   /*----- done done done -----*/

   EXRETURN ;
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


ENTRY("remove_zero_stimfns") ;

  /*----- Initialize local variables -----*/
  num_stimts = option_data->num_stimts;
  num_glt    = option_data->num_glt;


  /*----- Loop over all stimulus funcitons -----*/
  is = 0;
  while (is < num_stimts)
    {
      if( basis_stim[is] != NULL ){ is++ ; continue ; }  /* 12 Aug 2004 */

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
        WARNING_message("!! -stim_file function %s comprises all zeros!",
             option_data->stim_filename[is]); fflush(stdout); badlev++ ;

        if (option_data->num_glt > 0)
          DC_error
            ("Cannot process -glt option(s) when -stim_file function is all zero");
        if( basis_count > 0 )
          DC_error
            ("Cannot process -stim_times option(s) when -stim_file function is all zero");

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

  EXRETURN ;
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

#ifdef FIX_CONFLICTS
  ierror = THD_deconflict_prefix( new_dset ) ;
  if( ierror > 0 ){
    char *pfx = DSET_PREFIX(new_dset) ;
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    filename , pfx ) ;
    strcpy(filename,pfx) ;
  }
#else
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
             "Output dataset file %s already exists "
             " -- cannot continue! ",
             new_dset->dblk->diskptr->header_name);
      DC_error (message);
    }
#endif

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


  /* 03 May 2007: allow default 'Decon' bucket file to be overwritten */

  if( option_data->bucket_filename != NULL &&
      strcmp(option_data->bucket_filename,"Decon") != 0 )
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

  /*----- Check if -xsave was given without -bucket ------*/
  if( xsave && option_data->bucket_filename == NULL ){
    WARNING_message("-xsave given without -bucket; -xsave is disabled!") ;
    xsave = 0 ;
  }

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
  if (NFirst < 0){
    for (is = 0;  is < num_stimts;  is++)
      if( basis_stim[is] == NULL && NFirst < (max_lag[is]+nptr[is]-1)/nptr[is] )
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

      if ((irb >= NFirst) && (irb <= NLast) && (censor_array[it] != 0.0f))
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

   GoodList = *good_list ;
  nGoodList = N ;
  nParam    = p ;

  /*----- Check number of stimulus time series -----*/
  if (num_stimts < 0)
    {
      DC_error ("Require: 0 <= num_stimts (DUH! --or-- D'oh!)");
    }


  /*----- Check lengths of stimulus time series -----*/

#define ALLOW_EXTEND
  for (is = 0;  is < num_stimts;  is++)
    {
      if( basis_stim[is] != NULL ) continue ;  /* 12 Aug 2004 */

      if (stim_length[is] < nt*nptr[is])
      {
#ifndef ALLOW_EXTEND
        sprintf (message, "Input stimulus time series file %s is too short",
               option_data->stim_filename[is]);
        DC_error (message);
#else
          int nlen=nt*nptr[is], qq ;
          WARNING_message(
                  "input stimulus time series file %s is too short:\n"
                  "            length = %d, but should be at least %d." ,
            option_data->stim_filename[is] , stim_length[is] , nlen ) ;

          if( option_data->slice_base[is] == 0 ){
            stimulus[is] = (float *) realloc( stimulus[is] , sizeof(float)*nlen ) ;
            for( qq=stim_length[is] ; qq < nlen ; qq++ ) stimulus[is][qq] = 0.0 ;
          } else {
            int ny=option_data->slice_base[is] , nx=stim_length[is] ;
            MRI_IMAGE *tim, *sim=mri_new_vol_empty(nx,ny,1,MRI_float) ;
            mri_fix_data_pointer( stimulus[is] , sim ) ;
            tim = mri_zeropad_2D( 0,nlen-nx , 0,0 , sim ) ;
            stimulus[is] = MRI_FLOAT_PTR(tim) ; mri_free(sim) ;
          }
          stim_length[is] = nlen ; nerr++ ;
#endif
      }
    }
#ifdef ALLOW_EXTEND
    if( nerr > 0 ){
      char *eee = getenv("AFNI_3dDeconvolve_extend") ;
      if( eee != NULL && (*eee=='n' || *eee=='N') )
        ERROR_exit("Can't continue with too short files!\n"
                   "        AFNI_3dDeconvolve_extend = %s",eee ) ;

      INFO_message("EXTENDING short files with zero values\n"
                   "   (to stop this behavior, setenv AFNI_3dDeconvolve_extend NO)") ;
    }
#endif


#undef  TMESS
#define TMESS WARNING_message   /* WARNING_message or ERROR_exit */

  /*----- Check whether time lags are reasonable -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      if( basis_stim[is] != NULL ) continue ; /* 12 Aug 2004 */

      m = max_lag[is] - min_lag[is] + 1;
      if (m < 2)
      {
        if (option_data->iresp_filename[is] != NULL)
          {
            TMESS("Only %d time point for output dataset %s",
                   m, option_data->iresp_filename[is]);
          }

        if (option_data->sresp_filename[is] != NULL)
          {
            TMESS("Only %d time point for output dataset %s",
                   m, option_data->sresp_filename[is]);
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
        if (! option_data->nobout)    /* baseline coefs + t stats */
          nbricks += qp * (1 + option_data->tout);

        for (is = 0;  is < num_stimts;  is++)  /* individual -stim coefs + stats */
          {
            if ((!option_data->stim_base[is]) || (!option_data->nobout))
            {
                  if( basis_stim[is] != NULL ) m = basis_stim[is]->nparm ;
                  else                         m = max_lag[is] - min_lag[is] + 1;
              nbricks += m * (1 + option_data->tout);
              nbricks += option_data->rout + option_data->fout;
            }
          }
      }

      /* full model stats */

      nbricks += option_data->rout + option_data->do_fullf + option_data->vout;

      if (num_glt > 0)
      for (iglt = 0;  iglt < num_glt;  iglt++)  /* GLT coefs + stats */
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
#ifndef FIX_CONFLICTS
  if (option_data->input_filename != NULL)
    check_output_files (option_data, dset_time);
#endif

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

/*---------------------------------------------------------------*/

void proc_atexit( void )  /*** similarly - atexit handler ***/
{
  if( proc_shmid > 0 )
    shmctl( proc_shmid , IPC_RMID , NULL ) ;
}

/*---------------------------------------------------------------*/
/*** This function is called to allocate all output
     volumes at once in shared memory, and set their pointers ***/

/** 24 Oct 2005: use mmap() instead of shared memory, if possible **/

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
       "** SUGGESTION:  Use 3dZcutup to slice dataset into smaller pieces\n"
       "**                and then 3dZcat to glue results back together.\n"
       "** SUGGESTION:  Run on a 64-bit computer system, instead of 32-bit.\n"
      , psum,twogig) ;
   else
     INFO_message("total shared memory needed = %lld bytes",psum) ;

   proc_shmsize = psum ;  /* global variable */

   /*------- create shared memory segment -------*/

#ifdef MAP_ANON  /** 24 Oct 2005: use mmap() instead of shmem **/

   proc_shmptr = mmap( (void *)0 , (size_t)psum ,
                       PROT_READ | PROT_WRITE , MAP_ANON | MAP_SHARED ,
                       -1 , 0 ) ;
   if( proc_shmptr == NULL || proc_shmptr == (void *)(-1) ){
     perror("** FATAL ERROR: Can't create shared mmap() segment\n"
            "** Unix message") ;
     exit(1) ;
   }

#else    /** old code: shared memory segment **/

   UNIQ_idcode_fill( kstr ) ;               /* unique string "key" */
   proc_shmid = shm_create( kstr , proc_shmsize ) ; /* thd_iochan.c */
   if( proc_shmid < 0 ){
     fprintf(stderr,"\n** Can't create shared memory of size %lld!\n"
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
               "** Current max shared memory size = %lld bytes.\n"
               "** For information on how to change this, see\n"
               "** http://afni.nimh.nih.gov/afni/doc/misc/afni_parallelize\n"
               "** and also contact your Unix system administrator.\n"
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
   INFO_message("mmap() memory allocated: %lld bytes [%s]\n" ,
                proc_shmsize, approximate_number_string((double)proc_shmsize) );
#else
   INFO_message("Shared memory allocated: %lld bytes at id=%d\n" ,
                proc_shmsize , proc_shmid ) ;
#endif

   /* get pointer to shared memory segment we just created */

#ifndef MAP_ANON
   if( proc_shmid > 0 ){
     proc_shmptr = shm_attach( proc_shmid ) ; /* thd_iochan.c */
     if( proc_shmptr == NULL ){
       fprintf(stderr,"\n** Can't attach to shared memory!?\n"
                        "** This is bizarre.\n" ) ;
       shmctl( proc_shmid , IPC_RMID , NULL ) ;
       exit(1) ;
     }
   }
#endif

   /* clear all shared memory to zero */

   memset( proc_shmptr , 0 , proc_shmsize ) ;

   /* fix the local pointers to arrays in shared memory */

   *proc_shm_ar[0] = (float *) proc_shmptr ;
   for( ii=1 ; ii < proc_shm_arnum ; ii++ )
     *proc_shm_ar[ii] = *proc_shm_ar[ii-1] + proc_shm_arsiz[ii] ;

   return ;
}

/*-------------------------------------------------------------*/
/*** This function replaces free();
     it won't try to free things stored in the shared memory ***/

void proc_free( void *ptr )
{
   int ii ;

   if( ptr == NULL ) return ;
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
  int ibot,itop ;


ENTRY("allocate_memory") ;

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

  if( CoefFilename != NULL ){ bout = cout = 1 ; }  /* 26 Jul 2004 */

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
      if( basis_stim[is] != NULL ){ ibot=0; itop=basis_stim[is]->nparm-1; }
      else                        { ibot=min_lag[is]; itop=max_lag[is];  }
      for (it = ibot;  it <= itop;  it++)
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


  if (vout || basis_need_mse)  zero_fill_volume (&(*mse_vol),   nxyz);
  if (option_data->do_fullf)  zero_fill_volume (&(*ffull_vol), nxyz);
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

  EXRETURN ;
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

ENTRY("initialize_program") ;


  /*----- Allocate memory -----*/
  *option_data = (DC_options *) malloc (sizeof(DC_options));


  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Identify software -----*/
  if (!(*option_data)->quiet)  identify_software();

  if( xrestore ) EXRETURN ;  /* 26 Jul 2004 - special operations to do! */

  /*----- Tell the user if he is being foolish -----*/
  if( !(*option_data)->quiet &&
      !legendre_polort       &&
      (*option_data)->polort > 1 ){  /* 20 Jul 2004 */
    WARNING_message("you have polynomials of order %d for the baseline\n"
                   "          but disabled use of the Legendre polynomials!\n"
                   "          Check the matrix condition and accuracy of results!" ,
            (*option_data)->polort ) ;
    (*option_data)->nocond = 0 ;
  }

  /*----- Read input data -----*/
  read_input_data (*option_data, dset_time, mask_vol, fmri_data, fmri_length,
               censor_array, censor_length, block_list, num_blocks,
               stimulus, stim_length, glt_cmat);


  /*----- Remove all-zero stimulus functions -----*/
  if( !use_psinv )
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

  EXRETURN ;
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
      for (it = 0;  it < nt;  it++)        /* for bad points */
      fitts_vol[it][iv] = ts_array[it];

      for (it = 0;  it < N;  it++)         /* for good points */
      fitts_vol[good_list[it]][iv] = fitts[it];
    }

  if (errts_vol != NULL)
    {
      for (it = 0;  it < nt;  it++)        /* for bad points */
      errts_vol[it][iv] = 0.0;

      for (it = 0;  it < N;  it++)         /* for good points */
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
  matrix x_full ,          /* matrix:  X            for full model */
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
  int ibot,itop ;
  int j , do_extras ; float jnorm ;


  /*----- Print the normalized parameter standard deviations -----*/
  do_extras = AFNI_yesenv("AFNI_3dDeconvolve_nodata_extras") ;
  if( do_extras ){
    printf("\nBaseline -polort parameters:\n") ;
    for( m=0 ; m < qp ; m++ ){
      jnorm = 0.0 ;
      for( j=0 ; j < x_full.rows ; j++ ) jnorm += SQR(x_full.elts[j][m]) ;
      jnorm = sqrt(jnorm) ;
      stddev = sqrt ( xtxinv_full.elts[m][m] );
      printf ("  h[%2d] norm. std. dev. = %8.4f   X col. norm = %8.4f\n",
              ilag, stddev, jnorm );
    }
  }

  /*----- Print the normalized parameter standard deviations -----*/
  m = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      printf ("\nStimulus: %s \n", stim_label[is]);
      if( basis_stim[is] != NULL ){ ibot = 0; itop = basis_stim[is]->nparm-1; }
      else                        { ibot = min_lag[is]; itop = max_lag[is];   }
      for (ilag = ibot;  ilag <= itop;  ilag++)
        {
          jnorm = 0.0 ;
          for( j=0 ; j < x_full.rows ; j++ ) jnorm += SQR(x_full.elts[j][m]) ;
          jnorm = sqrt(jnorm) ;
          stddev = sqrt ( xtxinv_full.elts[m][m] );
          if( do_extras )
            printf ("  h[%2d] norm. std. dev. = %8.4f   X col. norm = %8.4f\n",
                   ilag, stddev, jnorm );
          else
            printf ("  h[%2d] norm. std. dev. = %8.4f\n",
                   ilag, stddev );
          m++;
        }
    }

  /*----- Print normalized standard deviations for GLTs -----*/
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

  fflush(stdout);
}

/*---------------------------------------------------------------------------*/

typedef struct {
  int qp    ,           /* number of polynomial baseline parameters */
      q     ,           /* total number of baseline parameters */
      p     ,           /* total number of parameters */
      nstim ,           /* number of stim functions = number of rdcd matrices */
      nglt  ,           /* number of glt matrices */
      status ;          /* are all matrices computed? */

  matrix xdata ;        /* regression model, without censoring */
  matrix x_full ;       /* with censoring = [X] */
  matrix xtxinv_full ;  /* inv{ [X][X]' } */
  matrix xtxinvxt_full; /* inv{ [X][X]' } [X]' = solution matrix */

  matrix x_base ;       /* baseline model */
  matrix xtxinvxt_base; /* solution matrix for baseline model */

  matrix *x_rdcd ;      /* reduced models (with 1 stim function omitted) */
  matrix *xtxinvxt_rdcd;/* solution matrices for reduced models */

  matrix *glt_cxtxinvct;/* matrices for GLT calculations */
  matrix *glt_amat ;

} regression_matrices ;

/*...........................................................................*/

#define INIT_RMAT(rm,ns,ng)                                         \
 do{ int ii ;                                                       \
     matrix_initialize( &(rm).xdata ) ;                             \
     matrix_initialize( &(rm).x_full ) ;                            \
     matrix_initialize( &(rm).xtxinv_full ) ;                       \
     matrix_initialize( &(rm).xtxinvxt_full ) ;                     \
     matrix_initialize( &(rm).x_base ) ;                            \
     matrix_initialize( &(rm).xtxinvxt_base ) ;                     \
     (rm).x_rdcd        = (matrix *)malloc(sizeof(matrix)*(ns)) ;   \
     (rm).xtxinvxt_rdcd = (matrix *)malloc(sizeof(matrix)*(ns)) ;   \
     for( ii=0 ; ii < (ns) ; ii++ ){                                \
       matrix_initialize( &(rm).x_rdcd[ii] ) ;                      \
       matrix_initialize( &(rm).xtxinvxt_rdcd[ii] ) ;               \
     }                                                              \
     (rm).glt_cxtxinvct = (matrix *)malloc(sizeof(matrix)*(ng)) ;   \
     (rm).glt_amat      = (matrix *)malloc(sizeof(matrix)*(ng)) ;   \
     for( ii=0 ; ii < (ng) ; ii++ ){                                \
       matrix_initialize( &(rm).glt_cxtxinvct[ii] ) ;               \
       matrix_initialize( &(rm).glt_amat[ii] ) ;                    \
     }                                                              \
     (rm).nstim = (ns) ; (rm).nglt = (ng) ; (rm).status = 0 ;       \
  } while(0)

/*...........................................................................*/

#define FREE_RMAT(rm)                                 \
 do{ int ii ;                                         \
     matrix_destroy( &(rm).xdata ) ;                  \
     matrix_destroy( &(rm).x_full ) ;                 \
     matrix_destroy( &(rm).xtxinv_full ) ;            \
     matrix_destroy( &(rm).xtxinvxt_full ) ;          \
     matrix_destroy( &(rm).x_base ) ;                 \
     matrix_destroy( &(rm).xtxinvxt_base ) ;          \
     for( ii=0 ; ii < (rm).nstim ; ii++ ){            \
       matrix_destroy( &(rm).x_rdcd[ii] ) ;           \
       matrix_destroy( &(rm).xtxinvxt_rdcd[ii] ) ;    \
     }                                                \
     for( ii=0 ; ii < (rm).nglt ; ii++ ){             \
       matrix_destroy( &(rm).glt_cxtxinvct[ii] ) ;    \
       matrix_destroy( &(rm).glt_amat[ii] ) ;         \
     }                                                \
     (rm).status = -1 ;                               \
  } while(0)

/*...........................................................................*/

static int              num_Rmat = 0 ;
static regression_matrices *Rmat = NULL ;

/*---------------------------------------------------------------------------*/

void setup_regression_matrices( DC_options *option_data ,
                                int *good_list ,
                                int *block_list , int num_blocks ,
                                float **stimulus , int *stim_length )
{
  int is , mm , nz=1 ;
  float **sstim ;

  num_Rmat = 1 ;
  for( is=0 ; is < option_data->num_stimts && nz < 2 ; is++ )
    if( option_data->slice_base[is] > 0 )
      nz = num_Rmat = option_data->slice_base[is] ;

  Rmat = (regression_matrices *)malloc(sizeof(regression_matrices)*num_Rmat) ;

  for( mm=0 ; mm < num_Rmat ; mm++ )
    INIT_RMAT( Rmat[mm] , option_data->num_stimts , option_data->num_glt ) ;

  sstim = (float **) malloc( sizeof(float *)*option_data->num_stimts ) ;
  memcpy( sstim , stimulus , sizeof(float *)*option_data->num_stimts ) ;

  /*- loop over slices:
       substituting slice-dependent regressors,
       making matrices from those collections of vectors -*/

  for( mm=0 ; mm < num_Rmat ; mm++ ){

    for( is=0 ; is < option_data->num_stimts ; is++ )
      if( mm > 0 && option_data->slice_base[is] > 0 )
        sstim[is] = stimulus[is] + (mm*stim_length[is]) ;

    init_indep_var_matrix( option_data->p,
                           option_data->qp,
                           option_data->polort,
                           option_data->nt,
                           option_data->N,
                           good_list, block_list, num_blocks,
                           option_data->num_stimts,
                           sstim, stim_length,
                           option_data->stim_minlag,
                           option_data->stim_maxlag,
                           option_data->stim_nptr, option_data->stim_base ,
                           &Rmat[mm].xdata);
    Rmat[mm].qp = option_data->qp ;
    Rmat[mm].q  = option_data->q ;
    Rmat[mm].p  = option_data->p ;
  }

  free((void *)sstim) ; return ;
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

  int vstep ;                  /* interval progress meter dots */
  double ct ;                  /* clock time */

ENTRY("calculate_results") ;

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
  vector_initialize (&coef) ;
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

  ct = COX_clock_time() ;

  init_indep_var_matrix (p, qp, polort, nt, N, good_list, block_list,
                   num_blocks, num_stimts, stimulus, stim_length,
                   min_lag, max_lag, nptr, option_data->stim_base , &xdata);
  if (option_data->xout)  matrix_sprint ("X matrix:", xdata);

  if( option_data->xjpeg_filename != NULL )    /* 21 Jul 2004 */
    JPEG_matrix_gray( xdata , option_data->xjpeg_filename ) ;

  if( option_data->x1D_filename   != NULL ){   /* 28 Mar 2006 */
    void *cd=(void *)coldat ;
    if( AFNI_noenv("AFNI_3dDeconvolve_NIML") &&
        strstr(option_data->x1D_filename,"niml") == NULL ) cd = NULL ;
    ONED_matrix_save( xdata , option_data->x1D_filename , cd ) ;
  }


  /*----- 14 Jul 2004: check matrix for bad columns - RWCox -----*/

  { int *iar , k , nerr=0 ;
    iar = matrix_check_columns( xdata , QEPS ) ;
    if( iar != NULL ){
      WARNING_message("Problems with the X matrix columns, listed below:") ;
      for( k=0 ; iar[2*k] >= 0 ; k++ ){
        if( iar[2*k+1] >= 0 ){
          WARNING_message("!! * Columns %d and %d are nearly collinear!",
                  iar[2*k],iar[2*k+1] ) ; nerr++ ; badlev++ ;
        } else {
          WARNING_message("!! * Column %d is all zeros: %s",
                  iar[2*k] ,
                  use_psinv ? "SVD on => will be kept"
                            : "SVD off => will be excised" ) ; badlev++ ;
        }
      }
      if( nerr > 0 && AFNI_yesenv("AFNI_3dDeconvolve_nodup") )
        ERROR_exit("Can't continue after above problems/warnings!\n");
      free(iar) ;
    }
  }

  /*-- 14 Jul 2004: calculate matrix condition number - RWCox --*/

  if( !option_data->nocond ){
    int qbad , *clist , nlist , jj ; matrix xext ;

    /* examine full matrix */

    qbad = check_matrix_condition( xdata, "Signal+Baseline" ); badlev += qbad;

    /* extract columns for signal model only */

    clist = (int *)malloc(sizeof(int)*xdata.cols) ;
    for( nlist=jj=0 ; jj < xdata.cols ; jj++ )
      if( coldat[jj].group > 0 ) clist[nlist++] = jj ;
    if( nlist > 0 ){
      matrix_initialize (&xext);
      matrix_extract( xdata , nlist , clist , &xext ) ;
      qbad = check_matrix_condition( xext, "Signal-only" ); badlev += qbad;
      matrix_destroy( &xext ) ;
    }

    /* extract columns for baseline model only */

    for( nlist=jj=0 ; jj < xdata.cols ; jj++ )
      if( coldat[jj].group <= 0 ) clist[nlist++] = jj ;
    if( nlist > 0 ){
      matrix_initialize (&xext);
      matrix_extract( xdata , nlist , clist , &xext ) ;
      qbad = check_matrix_condition( xext, "Baseline-only" ); badlev += qbad;
      matrix_destroy( &xext ) ;
    }

    /* extract columns for -stim_base model only */

    for( nlist=jj=0 ; jj < xdata.cols ; jj++ )
      if( coldat[jj].group == 0 ) clist[nlist++] = jj ;
    if( nlist > 0 ){
      matrix_initialize (&xext);
      matrix_extract( xdata , nlist , clist , &xext ) ;
      qbad = check_matrix_condition( xext, "-stim_base-only" ); badlev += qbad;
      matrix_destroy( &xext ) ;
    }

    /* extract columns for -polort model only */

    for( nlist=jj=0 ; jj < xdata.cols ; jj++ )
      if( coldat[jj].group < 0 ) clist[nlist++] = jj ;
    if( nlist > 0 ){
      matrix_initialize (&xext);
      matrix_extract( xdata , nlist , clist , &xext ) ;
      qbad = check_matrix_condition( xext, "-polort-only" ); badlev += qbad;
      matrix_destroy( &xext ) ;
    }
  }

  /*----- Initialization for the regression analysis -----*/
  init_regression_analysis (p, qp, num_stimts, baseline, min_lag, max_lag,
                      xdata, &x_full, &xtxinv_full, &xtxinvxt_full,
                      &x_base, &xtxinvxt_base, x_rdcd, xtxinvxt_rdcd);
  if( option_data->xout )
    matrix_sprint ("(X'X) inverse matrix:", xtxinv_full);
  if( nodata && option_data->x1D_filename != NULL ){
    char fn[THD_MAX_NAME] , *jpt ;
    strcpy(fn,option_data->x1D_filename) ;
                       jpt = strstr(fn,".x1D") ;
    if( jpt == NULL )  jpt = fn + strlen(fn) ;
    strcpy(jpt,"_XtXinv.x1D") ;
    ONED_matrix_save( xtxinv_full , fn , NULL ) ;  /* no column metadata */
  }

  /*----- Save some of this stuff for later, dude -----*/
  X        = x_full        ;  /* 25 Jul 2004 (RWCox) */
  XtXinv   = xtxinv_full   ;
  XtXinvXt = xtxinvxt_full ;

  { int m , npar , j ;        /* 31 Aug 2004 (RWCox) */
    register double sum ;
    float mmax ;

    Xcol_inbase = (int *)  calloc(sizeof(int)  ,p) ;
    Xcol_mean   = (float *)calloc(sizeof(float),p) ;

    for( is=0 ; is < qp ; is++ ) Xcol_inbase[is] = 1 ; /* mark baseline columns */
    m = qp ;
    for( is=0 ; is < num_stimts ; is++ ){
      npar = (basis_stim[is] != NULL)
            ? basis_stim[is]->nparm
            : option_data->stim_maxlag[is] - option_data->stim_minlag[is] + 1 ;

      if( baseline[is] ) for( j=0 ; j < npar ; j++ ) Xcol_inbase[m+j] = 1 ;
      m += npar ;
    }

    mmax = 0.0f ;
    for( j=0 ; j < p ; j++ ){   /* compute mean of each column */
      sum = 0.0 ;
      for( i=0 ; i < X.rows ; i++ ) sum += X.elts[i][j] ;
      Xcol_mean[j] = (float)(sum/X.rows) ;
      if( Xcol_inbase[j] && fabs(Xcol_mean[j]) > mmax )  /* find largest */
        mmax = fabs(Xcol_mean[j]) ;             /* mean of baseline cols */
    }

    if( mmax > 0.0f ){    /* mark baseline cols that have nontrivial means */
      mmax *= 9.99e-6 ;
      for( j=0 ; j < p ; j++ )
        if( Xcol_inbase[j] && fabs(Xcol_mean[j]) > mmax ) Xcol_inbase[j] = 2 ;
    }
  }

  /*--- Compute abs sum of matrix [xtxinvxt][xdata]-I [19 Aug 2004]---*/
  if( !option_data->nocond ){
    double esum , sum ;
    int nn=xdata.rows , mm=xdata.cols , ii,jj,kk ;
    char *www ;
    esum = 0.0 ;
    for( ii=0 ; ii < mm ; ii++ ){
      for( jj=0 ; jj < mm ; jj++ ){
        sum = (ii==jj) ? -1.0 : 0.0 ;
        for( kk=0 ; kk < nn ; kk++ )
          sum += xtxinvxt_full.elts[ii][kk]*xdata.elts[kk][jj] ;
        esum += fabs(sum) ;
      }
    }
    esum /= (mm*mm) ;
         if( esum > 1.e-3 ){ www = " ** BEWARE **"   ; badlev++; }
    else if( esum > 1.e-4 ){ www = " ++ OK ++"       ; }
    else if( esum > 1.e-6 ){ www = " ++ GOOD ++"     ; }
    else                   { www = " ++ VERY GOOD ++"; }
    if( strstr(www,"**") != NULL )
      WARNING_message("!! Matrix inverse average error = %g %s",esum,www) ;
    else
      INFO_message("Matrix inverse average error = %g %s",esum,www) ;
  }

#if 0
  /*-- 19 Aug 2004: plot matrix pseudoinverse as well --*/
  if( option_data->xjpeg_filename != NULL || option_data->x1D_filename != NULL ){
    char *jpt , *jsuf ;
    char *fn = calloc( sizeof(char) , THD_MAX_NAME+16 ) ;
    matrix xpsinv ;

    matrix_initialize( &xpsinv ) ;
    matrix_transpose( xtxinvxt_full , &xpsinv ) ;

    if( option_data->xjpeg_filename != NULL ){
      strcpy(fn,option_data->xjpeg_filename) ;
                         jpt = strstr(fn,".jpg") ; jsuf = ".jpg" ;
      if( jpt == NULL ){ jpt = strstr(fn,".JPG") ; jsuf = ".JPG" ; }
      if( jpt == NULL )  jpt = fn + strlen(fn) ;
      strcpy(jpt,"_psinv") ; strcat(fn,jsuf) ;
      JPEG_matrix_gray( xpsinv , fn ) ;
    }

#if 0
    if( option_data->x1D_filename != NULL ){
      strcpy(fn,option_data->x1D_filename) ;
                         jpt = strstr(fn,".1D") ; jsuf = ".1D" ;
      if( jpt == NULL )  jpt = fn + strlen(fn) ;
      strcpy(jpt,"_psinv") ; strcat(fn,jsuf) ;
      ONED_matrix_save( xpsinv , fn , NULL ) ;  /* no column metadata */
    }
#endif

    free((void *)fn) ; matrix_destroy( &xpsinv ) ;
  }
#endif

  /*----- Initialization for the general linear test analysis -----*/
  if (num_glt > 0)
    init_glt_analysis (xtxinv_full, num_glt, glt_cmat, glt_amat, cxtxinvct);

  ct = COX_clock_time() - ct ;
  INFO_message("Matrix setup time = %.2f s\n",ct) ;  /* 25 Apr 2005 */

  vector_create (N, &y);

  if (nodata)
    {
      report_evaluation(qp, num_stimts, stim_label, min_lag, max_lag,
                        x_full, xtxinv_full,
                        num_glt, glt_label, glt_rows, cxtxinvct);
    }

  else
    {     /*============== actually process data ====================*/

      int ixyz_bot=0 , ixyz_top=nxyz ;  /* voxel indexes to process */

#ifdef USE_GET
#define NGET 128              /* number to get at one time */
      int nget=0 ,            /* number of time series current gotten */
          cget=0 ,            /* index of next timeseries in iget & imget */
          jget   ,            /* loop index for iget */
          iget[NGET] ;        /* voxel index of timeseries */
      MRI_IMARR *imget=NULL ; /* array of timeseries */
#endif

      if( badlev > 0 ){       /*--- 07 Mar 2007 ---*/
        if( goforit >= badlev ){
          WARNING_message(
            "!! " PROGRAM_NAME " -GOFORIT is set to %d: running despite %d matrix warnings",
            goforit , badlev ) ;
          WARNING_message(
            "!! See file " PROGRAM_NAME ".err for all WARNING and ERROR messages !!") ;
          WARNING_message(
            "!! Please be sure you understand what you are doing !!") ;
          WARNING_message(
            "!! If in doubt, consult with someone or with the AFNI message board !!") ;
        } else {
          ERROR_message(
            "!! " PROGRAM_NAME ": Can't run past %d matrix warnings without -GOFORIT %d",
            badlev , goforit ) ;
          ERROR_message(
            "!!                Currently at -GOFORIT %d",goforit) ;
          ERROR_message(
            "!! See file " PROGRAM_NAME ".err for all WARNING and ERROR messages !!") ;
          ERROR_message(
            "!! Be sure you understand what you are doing before using -GOFORIT !!") ;
          ERROR_message(
            "!! If in doubt, consult with someone or with the AFNI message board !!") ;
          ERROR_exit(
            "!! " PROGRAM_NAME " (regretfully) shuts itself down !!") ;
        }
      }

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

          if( !option_data->quiet ) INFO_message("Voxels in dataset: %d",nxyz);
          if( nvox < nxyz )
          if( !option_data->quiet ) INFO_message("Voxels in mask:    %d",nvox);
          if( !option_data->quiet ) INFO_message("Voxels per job:    %d",nper);

          for( pp=1 ; pp < proc_numjob ; pp++ ){
            ixyz_bot = proc_vox_bot[pp] ;   /* these 3 variables   */
            ixyz_top = proc_vox_top[pp] ;   /* are for the process */
            proc_ind = pp ;                 /* we're about to fork */
            newpid   = fork() ;
            if( newpid == -1 )
              ERROR_exit("Can't fork job #%d! Danger, Wil Robinson!",pp);

            if( newpid == 0 ) break ;   /* I'm the child */
            proc_pid[pp] = newpid ;     /* I'm the parent */
            iochan_sleep(10) ;          /* 10 ms, to let the child get going */
          }
          if( pp == proc_numjob ){       /* only in the parent */
            ixyz_bot = proc_vox_bot[0] ; /* set the 3 control */
            ixyz_top = proc_vox_top[0] ; /* variables needed */
            proc_ind = 0 ;               /* below           */
          }
          if( !option_data->quiet )
            INFO_message("Job #%d: processing voxels %d to %d; elapsed time=%.3f",
                    proc_ind,ixyz_bot,ixyz_top-1,COX_clock_time()) ;
        }
      }
#endif /* PROC_MAX */

      if( proc_numjob == 1 && !option_data->quiet )
        INFO_message("Calculations starting; elapsed time=%.3f",COX_clock_time()) ;

      vstep = nxyz / 50 ;
      if( option_data->quiet        ||
          option_data->fdisp >= 0.0 ||
          option_data->progress > 0 ||
          proc_numjob > 1             ) vstep = 0 ;

      if( vstep > 0 ) fprintf(stderr,"++ voxel loop:") ;

      /*----- Loop over all voxels -----*/
      for (ixyz = ixyz_bot;  ixyz < ixyz_top;  ixyz++)
      {

          if( vstep > 0 && ixyz%vstep==vstep-1 ) vstep_print() ;

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

          if( voxel_base != NULL )
            voxel_base[ixyz] = baseline_mean( coef ) ;   /* 31 Aug 2004 */


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
              printf ("%s \n", label); fflush(stdout);
              }
          }

      }  /*----- Loop over voxels -----*/

        if( vstep > 0 ) fprintf(stderr,"\n") ;

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
            if( !option_data->quiet )
              INFO_message("Job #%d finished; elapsed time=%.3f",
                           proc_ind,COX_clock_time()) ;
            _exit(0) ;

          } else {                      /* parent waits for children */
            int pp ;
            if( !option_data->quiet )
              INFO_message("Job #0 waiting for children to finish; elapsed time=%.3f",
                           COX_clock_time()) ;
            for( pp=1 ; pp < proc_numjob ; pp++ )
              waitpid( proc_pid[pp] , NULL , 0 ) ;
            if( !option_data->quiet )
              INFO_message("Job #0 now finishing up; elapsed time=%.3f",
                           COX_clock_time()) ;
          }

          /* when get to here, only parent process is left alive,
             and all the results are in the shared memory segment arrays */
        }
#endif
        if( proc_numjob == 1 && !option_data->quiet )
          INFO_message("Calculations finished; elapsed time=%.3f",COX_clock_time()) ;

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

  EXRETURN ;
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

ENTRY("EDIT_coerce_autoscale_new") ;

  if( MRI_IS_INT_TYPE(otype) ){
    top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
    if (top == 0.0)  fac = 0.0;
    else  fac = MRI_TYPE_maxval[otype]/top;
  }

  EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
  RETURN ( fac );
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
  dset = THD_open_dataset (option_data->input_filename);
  CHECK_OPEN_ERROR(dset,option_data->input_filename) ;
  n = ts_length - 1;
  tdelta = dset->taxis->ttdel;
  nx = dset->daxes->nxx;   ny = dset->daxes->nyy;   nz = dset->daxes->nzz;
  nxyz = nx * ny * nz;
  DSET_UNMSEC( dset ) ; /* 12 Aug 2005: surgery on the time units? */


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
  float * volume;           /* pointer to volume of data */
  char label[THD_MAX_NAME]; /* label for output file */
  float newtr;              /* new time step = TR/nptr */
  int dtype ;


  /*----- Initialize local variables -----*/
  input_filename = option_data->input_filename;
  dset = THD_open_dataset (input_filename);
  CHECK_OPEN_ERROR(dset,input_filename) ;
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
  DSET_UNMSEC(dset) ;  /* 12 Aug 2005 */
  newtr = DSET_TIMESTEP(dset) / nptr;

  /*----- allocate memory -----*/

  dtype = (floatout) ? MRI_float : MRI_short ;

  /*-- make an empty copy of the prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (dset);

  /*----- Record history of dataset -----*/
  tross_Copy_History( dset , new_dset ) ;

  { char *commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
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
                      ADN_datum_all,   dtype ,
                      ADN_nvals,       ts_length,
                      ADN_ntt,         ts_length,
                      ADN_ttdel,       newtr,
                      ADN_none);

  if( ierror > 0 )
    ERROR_exit(
          "%d errors in attempting to create output dataset!", ierror ) ;

#ifndef FIX_CONFLICTS
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    ERROR_exit(
          "Output dataset file %s already exists -- can't continue!",
          new_dset->dblk->diskptr->header_name ) ;
#else
  if( THD_deconflict_prefix(new_dset) > 0 )
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_filename,DSET_PREFIX(new_dset) ) ;
#endif


  /*----- Reset slice offset times to zero -----*/
  if (tshift)
    EDIT_dset_items (new_dset,
                 ADN_nsl,     0,    /* will have no offsets when done */
                 ADN_ttorg, 0.0,    /* in case not already set */
                 ADN_ttdur, 0.0,    /* in case not already set */
                 ADN_none);

  EDIT_dset_items( new_dset , ADN_brick_fac,NULL , ADN_none ) ;

  /*----- attach bricks to new data set -----*/
  for (ib = 0;  ib < ts_length;  ib++)
    {

      /*----- Set pointer to appropriate volume -----*/
      volume = vol_array[ib];

      if( floatout ){  /* the new (float) way */
        float *fff ;
        EDIT_substitute_brick( new_dset , ib , MRI_float , NULL ) ;
        fff = DSET_ARRAY(new_dset,ib) ;
        memcpy( fff , volume , sizeof(float)*nxyz ) ;

      } else {   /* the old (short) way */
        short *bar ;
        EDIT_substitute_brick( new_dset , ib , MRI_short , NULL ) ;
        bar = DSET_ARRAY(new_dset,ib) ;

        if( volume != NULL ){
          factor = EDIT_coerce_autoscale_new( nxyz, MRI_float, volume,
                                              MRI_short, bar);
          if (factor < EPSILON)  factor = 0.0;
          else                   factor = 1.0 / factor;
          EDIT_BRICK_FACTOR( new_dset , ib , factor ) ;
        }
      }
   }

  /*----- write afni data set -----*/

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset (NULL, NULL, new_dset, True);
  if (!  option_data->quiet)
    INFO_message("Wrote 3D+time dataset into %s",DSET_BRIKNAME(new_dset)) ;


  /*----- deallocate memory -----*/
  THD_delete_3dim_dataset (new_dset, False);   new_dset = NULL ;

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
  float dof,
  float ndof,
  float ddof,                 /* degrees of freedom */
  void ** bar              /* bar[ib] points to data for sub-brick #ib */
)

{
  float factor=0.0f ; const float EPSILON = 1.0e-10;

ENTRY("attach_sub_brick") ;

  /*----- allocate memory for output sub-brick -----*/
  if( floatout ){  /* the new (float) way */
    float *fff ;
    EDIT_substitute_brick( new_dset , ibrick , MRI_float , NULL ) ;
    fff = DSET_ARRAY(new_dset,ibrick) ;
    if( bar != NULL ) bar[ibrick] = (void *)fff ;
    memcpy( fff , volume , sizeof(float)*nxyz ) ;

  } else {         /* the old (short) way */
    short *sbr ;
    EDIT_substitute_brick( new_dset , ibrick , MRI_short , NULL ) ;
    sbr = DSET_ARRAY(new_dset,ibrick) ;
    if( bar != NULL ) bar[ibrick] = (void *)sbr ;
    factor = EDIT_coerce_autoscale_new(nxyz, MRI_float,volume, MRI_short,sbr);
    if (factor < EPSILON)  factor = 0.0;
    else                   factor = 1.0 / factor;
   }

   /*----- edit the sub-brick -----*/
   if( brick_label != NULL && *brick_label != '\0' )
     EDIT_BRICK_LABEL (new_dset, ibrick, brick_label);

   EDIT_BRICK_FACTOR(new_dset, ibrick, factor);

   switch( brick_type ){
     case FUNC_TT_TYPE: if( dof > 0 )
                          EDIT_BRICK_TO_FITT(new_dset, ibrick, dof);
     break ;
     case FUNC_FT_TYPE: if( ndof > 0 && ddof > 0 )
                          EDIT_BRICK_TO_FIFT(new_dset, ibrick, ndof, ddof);
     break ;
     case FUNC_BT_TYPE: if( ndof > 0 && ddof > 0 )
                          EDIT_BRICK_TO_FIBT(new_dset, ibrick, ndof, ddof);
     break ;
   }

   EXRETURN ;
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
  void ** bar = NULL;       /* bar[ib] points to data for sub-brick #ib */

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
  float dof, ndof, ddof;      /* degrees of freedom */
  char label[THD_MAX_NAME];   /* general label for sub-bricks */
  char blab[THD_MAX_NAME] ;   /* label for baseline funcs */
  int num_glt;                   /* number of general linear tests */
  int * glt_rows;                /* number of linear constraints in glt */
  int iglt;                      /* general linear test index */
  int ilc;                       /* linear combination index */

  THD_3dim_dataset *coef_dset = NULL ;   /* coefficient bucket? */
  int cbuck , bout,cout , ibot,itop ;

  /*----- read prototype dataset -----*/
  old_dset = THD_open_dataset (option_data->input_filename);
  CHECK_OPEN_ERROR(old_dset,option_data->input_filename);
  DSET_UNMSEC(old_dset) ;  /* 12 Aug 2005 */

  bout = !option_data->nobout ;
  cout = !option_data->nocout ;

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
  bar  = (void **) malloc (sizeof(void *) * nbricks);
  MTEST (bar);


  /*-- make an empty copy of prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (old_dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History( old_dset , new_dset ) ;
  tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;
  sprintf (label, "Output prefix: %s", output_prefix);
  tross_Append_History ( new_dset, label);

  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                      ADN_prefix,          output_prefix,
                      ADN_type,            HEAD_FUNC_TYPE,
                      ADN_func_type,       FUNC_BUCK_TYPE,
                      ADN_datum_all,       (floatout) ? MRI_float : MRI_short ,
                      ADN_ntt,             0,   /* no time axis */
                      ADN_nvals,           nbricks,
                      ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
                      ADN_none ) ;

  if( ierror > 0 )
    ERROR_exit("%d errors in attempting to create bucket dataset!",
               ierror);

  if( strstr(output_prefix,"/") == NULL )
   (void) EDIT_dset_items (new_dset,
                             ADN_directory_name,  output_session,
                           ADN_none ) ;

#ifndef FIX_CONFLICTS
  if( THD_is_file(DSET_HEADNAME(new_dset)) && strcmp(output_prefix,"Decon") != 0 )
    ERROR_exit(
          "Bucket dataset file %s already exists--cannot continue!",
          DSET_HEADNAME(new_dset));
#else
  if( strcmp(output_prefix,"Decon") != 0 && THD_deconflict_prefix(new_dset) > 0 )
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_prefix,DSET_PREFIX(new_dset) ) ;
#endif

  if( CoefFilename != NULL ){
    coef_dset = EDIT_empty_copy( new_dset ) ;
    tross_Copy_History( old_dset , coef_dset ) ;
    tross_Make_History( PROGRAM_NAME , argc , argv , coef_dset ) ;
    (void) EDIT_dset_items( coef_dset,
                      ADN_prefix,          CoefFilename ,
                      ADN_type,            HEAD_FUNC_TYPE,
                      ADN_func_type,       FUNC_BUCK_TYPE,
                      ADN_datum_all,       (floatout) ? MRI_float : MRI_short ,
                      ADN_ntt,             0,  /* no time axis */
                      ADN_nvals,           p ,
                      ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
                      ADN_none ) ;
    if( strstr(CoefFilename,"/") == NULL )
      (void) EDIT_dset_items( coef_dset ,
                                ADN_directory_name,  output_session,
                              ADN_none ) ;
#ifndef FIX_CONFLICTS
    if( THD_is_file(DSET_HEADNAME(coef_dset)) )
      ERROR_exit(
        "Coefficient dataset file %s already exists--cannot continue!",
        DSET_HEADNAME(coef_dset));
#else
  if( THD_deconflict_prefix(coef_dset) > 0 )
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    CoefFilename,DSET_PREFIX(coef_dset) ) ;
#endif
  }

  /*----- save names for future xsave-ing -----*/
  if( DSET_IS_TCAT(old_dset) )
    InputFilename = strdup(old_dset->tcat_list) ;
  else
    InputFilename = strdup(THD_trailname(DSET_HEADNAME(old_dset),0)) ;

  BucketFilename = strdup(THD_trailname(DSET_HEADNAME(new_dset),0)) ;

  if( coef_dset != NULL )
    CoefFilename = strdup(THD_trailname(DSET_HEADNAME(coef_dset),0)) ;

  /*----- delete prototype dataset -----*/
  THD_delete_3dim_dataset( old_dset , False );  old_dset = NULL ;


  /*----- Attach individual sub-bricks to the bucket dataset -----*/


  /*----- User can choose to place full model stats first -----*/
  ibrick = -1;
  if (option_data->full_first)
    ibrick += option_data->vout + option_data->rout + option_data->do_fullf;

  cbuck = -1 ;


  /*----- Include fit coefficients and associated statistics? -----*/
  if( cout || coef_dset != NULL )
    {

      { int ii ;
        ParamStim  = (int *)  calloc(sizeof(int)   ,nParam) ;
        ParamLabel = (char **)calloc(sizeof(char *),nParam) ;
        for( ii=0 ; ii < qp     ; ii++ ) ParamLabel[ii] = strdup("ort") ;
        for(      ; ii < nParam ; ii++ ) ParamLabel[ii] = strdup("coef") ;
        if( cout && bout )
          ParamIndex = (int *)calloc(sizeof(int)   ,nParam) ;
        else
          ParamIndex = NULL ;
      }

      /*----- Baseline statistics -----*/
      if( bout || coef_dset != NULL )
      {
        strcpy (label, "Base");

          if( legendre_polort ) strcpy(blab,"P_") ;  /* 25 Jul 2004: */
          else                  strcpy(blab,"t^") ;  /* label for polynomials */

        for (icoef = 0;  icoef < qp;  icoef++)
          {
            if (qp == polort+1)
            strcpy (label, "Base");                            /* only 1 run */
            else
            sprintf (label, "Run#%d", icoef/(polort+1) + 1);   /* multiple runs */

            /*----- Baseline coefficient -----*/
            brick_type = FUNC_FIM_TYPE;
#ifdef USE_OLD_LABELS
            sprintf (brick_label, "%s %s%d Coef", label,blab, icoef % (polort+1));
#else
            sprintf( brick_label, "%s_Coef" , COLUMN_LABEL(icoef) ) ;
#endif
            volume = coef_vol[icoef];

              if( cout && bout )
               attach_sub_brick (new_dset, ++ibrick, volume, nxyz,
                           brick_type, brick_label, 0, 0, 0, bar);

#ifdef USE_OLD_LABELS
              sprintf(brick_label,"%s:%s%d" , label,blab,icoef%(polort+1)) ;
#else
              strcpy(brick_label,COLUMN_LABEL(icoef)) ;  /* 12 Mar 2007 */
#endif

              if( ParamIndex != NULL ) ParamIndex[icoef] = ibrick ;
              ParamStim [icoef] = 0 ;
              ParamLabel[icoef] = strdup(brick_label) ;

              if( coef_dset != NULL ){
                cbuck++ ;
                attach_sub_brick( coef_dset , cbuck , volume , nxyz ,
                          brick_type, brick_label, 0, 0, 0, NULL);
              }

            /*----- Baseline t-stat -----*/
            if ( cout && bout && option_data->tout)
            {
              ibrick++;
              brick_type = FUNC_TT_TYPE;
              dof = N - p;
#ifdef USE_OLD_LABELS
              sprintf (brick_label, "%s %s%d t-st",
                     label,blab, icoef % (polort+1));
#else
              sprintf( brick_label, "%s_Tstat" , COLUMN_LABEL(icoef) ) ;
#endif
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

          if( basis_stim[istim] != NULL ){
            ibot = 0 ; itop = basis_stim[istim]->nparm-1 ;
          } else {
            ibot = option_data->stim_minlag[istim] ;
            itop = option_data->stim_maxlag[istim] ;
          }

        /*----- Loop over stimulus time lags -----*/
          for( ilag=ibot ; ilag <= itop ; ilag++ )
          {
            if( !option_data->stim_base[istim] ||
                bout                           ||
                  coef_dset != NULL                )
            {
              /*----- Stimulus coefficient -----*/
              brick_type = FUNC_FIM_TYPE;
#ifdef USE_OLD_LABELS
              sprintf (brick_label, "%s[%d] Coef", label, ilag);
#else
              sprintf( brick_label, "%s_Coef" , COLUMN_LABEL(icoef) ) ;
#endif
              volume = coef_vol[icoef];
              if( cout && (!option_data->stim_base[istim] || bout) )
                attach_sub_brick (new_dset, ++ibrick, volume, nxyz,
                              brick_type, brick_label, 0, 0, 0, bar);

#ifdef USE_OLD_LABELS
              sprintf(brick_label,"%s:%d",label,ilag) ;
#else
              strcpy(brick_label,COLUMN_LABEL(icoef)) ;  /* 12 Mar 2007 */
#endif

              if( ParamIndex != NULL ) ParamIndex[icoef] = ibrick ;
              ParamStim [icoef] = option_data->stim_base[istim] ? 0
                                                                : istim+1 ;
              ParamLabel[icoef] = strdup(brick_label) ;

              if( coef_dset != NULL ){
                cbuck++ ;
                attach_sub_brick( coef_dset , cbuck , volume , nxyz ,
                          brick_type, brick_label, 0, 0, 0, NULL);
              }

              /*----- Stimulus t-stat -----*/
              if (cout && option_data->tout && (!option_data->stim_base[istim] || bout) )
                {
                  ibrick++;
                  brick_type = FUNC_TT_TYPE;
                  dof = N - p;
#ifdef USE_OLD_LABELS
                  sprintf (brick_label, "%s[%d] t-st", label, ilag);
#else
                  sprintf( brick_label, "%s_Tstat" , COLUMN_LABEL(icoef) ) ;
#endif
                  volume = tcoef_vol[icoef];
                  attach_sub_brick (new_dset, ibrick, volume, nxyz,
                            brick_type, brick_label, dof, 0, 0, bar);
                }
            }

            icoef++;
          }

        /*----- Stimulus R^2 stat -----*/
        if( cout && option_data->rout
                 && (!option_data->stim_base[istim] || bout) )
          {
            ibrick++;
            brick_type = FUNC_BT_TYPE;
            ndof = 0.5*(itop - ibot + 1) ;
            ddof = 0.5*(N - p);
#ifdef USE_OLD_LABELS
            sprintf (brick_label, "%s R^2", label);
#else
            sprintf( brick_label, "%s_R^2" , label ) ;
#endif
            volume = rpart_vol[istim];
            attach_sub_brick (new_dset, ibrick, volume, nxyz,
                        brick_type, brick_label, 0, ndof,ddof, bar);
          }

        /*----- Stimulus F-stat -----*/
        if( cout && option_data->fout
                   && (!option_data->stim_base[istim] || bout) )
          {
            ibrick++;
            brick_type = FUNC_FT_TYPE;
            ndof = itop - ibot + 1 ;
            ddof = N - p;
#ifdef USE_OLD_LABELS
            sprintf (brick_label, "%s F-stat", label);
#else
            sprintf( brick_label, "%s_Fstat" , label ) ;
#endif
            volume = fpart_vol[istim];
            attach_sub_brick (new_dset, ibrick, volume, nxyz,
                        brick_type, brick_label, 0, ndof, ddof, bar);
          }

      }  /* End loop over stim functions */

    }  /*  if (! option_data->nocout)  */

    if( coef_dset != NULL ){
      DSET_write(coef_dset) ;
      if( !option_data->quiet )
        INFO_message("Wrote cbucket to %s",DSET_BRIKNAME(coef_dset)) ;
      DSET_delete(coef_dset) ; coef_dset = NULL ;
    }


  /*----- 25 Jul 2004: save matrix info (etc.) to dataset header -----*/

  if( xsave ) XSAVE_output( DSET_PREFIX(new_dset) ) ;

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
#ifdef USE_OLD_LABELS
        sprintf (brick_label, "%s LC[%d] coef", label, ilc);
#else
        sprintf (brick_label, "%s_GLT#%d_Coef", label, ilc);
#endif
        volume = glt_coef_vol[iglt][ilc];
        attach_sub_brick (new_dset, ibrick, volume, nxyz,
                      brick_type, brick_label, 0, 0, 0, bar);

        /*----- GLT t-stat -----*/
        if (option_data->tout)
          {
            ibrick++;
            brick_type = FUNC_TT_TYPE;
            dof = N - p;
#ifdef USE_OLD_LABELS
            sprintf (brick_label, "%s LC[%d] t-st", label, ilc);
#else
            sprintf (brick_label, "%s_GLT#%d_Tstat", label, ilc);
#endif
            volume = glt_tcoef_vol[iglt][ilc];
            attach_sub_brick (new_dset, ibrick, volume, nxyz,
                        brick_type, brick_label, dof, 0, 0, bar);
          }
      }

      /*----- GLT R^2 stat -----*/
      if (option_data->rout)
      {
        ibrick++;
        brick_type = FUNC_BT_TYPE;
        ndof = 0.5 * glt_rows[iglt];
        ddof = 0.5 * (N - p);
#ifdef USE_OLD_LABELS
        sprintf (brick_label, "%s R^2", label);
#else
        sprintf (brick_label, "%s_GLT_R^2", label);
#endif
        volume = glt_rstat_vol[iglt];
        attach_sub_brick (new_dset, ibrick, volume, nxyz,
                      brick_type, brick_label, 0, ndof,ddof, bar);
      }

      /*----- GLT F-stat -----*/
      if (option_data->fout)
      {
        ibrick++;
        brick_type = FUNC_FT_TYPE;
        ndof = glt_rows[iglt];
        ddof = N - p;
#ifdef USE_OLD_LABELS
        sprintf (brick_label, "%s F-stat", label);
#else
        sprintf (brick_label, "%s_GLT_Fstat", label);
#endif
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
#ifdef USE_OLD_LABELS
      sprintf (brick_label, "Full MSE");
#else
      sprintf (brick_label, "Full_MSE");
#endif
      volume = mse_vol;
      attach_sub_brick (new_dset, ibrick, volume, nxyz,
                  brick_type, brick_label, 0, 0, 0, bar);
    }

  /*----- Full model R^2 -----*/
  if (option_data->rout)
    {
      ibrick++;
      brick_type = FUNC_BT_TYPE;
      ndof = 0.5*(p - q);
      ddof = 0.5*(N - p);
#ifdef USE_OLD_LABELS
      sprintf (brick_label, "Full R^2");
#else
      sprintf (brick_label, "Full_R^2");
#endif
      volume = rfull_vol;
      attach_sub_brick (new_dset, ibrick, volume, nxyz,
                  brick_type, brick_label, 0, ndof,ddof, bar);
    }

  /*----- Full model F-stat -----*/
  if (option_data->do_fullf)
    {
      ibrick++;
      brick_type = FUNC_FT_TYPE;
      ndof = p - q;
      ddof = N - p;
#ifdef USE_OLD_LABELS
      sprintf (brick_label, "Full F-stat");
#else
      sprintf (brick_label, "Full_Fstat");
#endif
      volume = ffull_vol;
      attach_sub_brick (new_dset, ibrick, volume, nxyz,
                  brick_type, brick_label, 0, ndof, ddof, bar);
    }


  /*----- write bucket data set -----*/

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  if (! option_data->quiet)
    printf("++ Wrote bucket dataset into %s\n",  DSET_BRIKNAME(new_dset));


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
  char filename[THD_MAX_NAME];    /* output time series file name */
  int it;                         /* time index */
  FILE * outfile = NULL;          /* file pointer */

  if( vol_array == NULL )
    ERROR_message("Can't output 1D file '%s' -- data array is NULL!",prefix);

  /*----- Create output filename by appending ".1D" -----*/

  if( strstr(prefix,"1D") == NULL )
    sprintf (filename, "%s.1D", prefix);
  else
    sprintf (filename, "%s"   , prefix);

  outfile = fopen (filename, "w");


  /*----- 'Volume' data consists of just one voxel -----*/
  for (it = 0;  it < ts_length;  it++)
    {
      fprintf (outfile, "%f \n",
              (vol_array[it] != NULL) ? vol_array[it][0] : 0.0 ) ;
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
      if( basis_stim[is] != NULL ){                    /* until later */
        if( option_data->iresp_filename[is] != NULL )
          basis_write_iresp( argc , argv , option_data ,
                             basis_stim[is] , basis_dtout ,
                             coef_vol+ib    ,
                             option_data->iresp_filename[is] ) ;
          ib += basis_stim[is]->nparm ;
        continue ;
      }

      /* old style iresp: each coefficent is a response */

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
      if( basis_stim[is] != NULL ){                     /* until later */
        if( option_data->sresp_filename[is] != NULL )
          basis_write_sresp( argc , argv , option_data ,
                             basis_stim[is] , basis_dtout ,
                             mse_vol , ib , XtXinv ,
                             option_data->sresp_filename[is] ) ;
          ib += basis_stim[is]->nparm ;
        continue ;
      }

      /* old style iresp: each coef is a response, so coef var is what we want */

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
#if 0
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
#endif


/*---------------------------------------------------------------------------*/
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
  /*----- force the user to acknowledge the riskiness of his behavior -----*/
  if( argc < 2 || strcmp(argv[1],"-OK") != 0 ){
    fprintf(stderr,"**\n"
                   "** 3dDeconvolve_f is now disabled by default.\n"
                   "** It is dangerous, due to roundoff problems.\n"
                   "** Please use 3dDeconvolve from now on!\n"
                   "**\n"
                   "** HOWEVER, if you insist on using 3dDeconvolve_f, then:\n"
                   "**        + Use '-OK' as the first command line option.\n"
                   "**        + Check the matrix condition number;\n"
                   "**            if it is greater than 100, BEWARE!\n"
                   "**\n"
                   "** RWCox - July 2004\n"
                   "**\n"
         ) ;
    exit(0) ;
  }
#endif

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif
   mainENTRY(PROGRAM_NAME " main") ; machdep() ;
  SET_message_file( PROGRAM_NAME ".err" ) ;

  /*----- start the elapsed time counter -----*/
  (void) COX_clock_time() ;

  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, &dset_time, &mask_vol,
     &fmri_data, &fmri_length, &censor_array, &censor_length, &good_list,
     &block_list, &num_blocks, &stimulus, &stim_length, &glt_cmat,
     &coef_vol, &scoef_vol, &tcoef_vol, &fpart_vol, &rpart_vol,
     &mse_vol, &ffull_vol, &rfull_vol, &glt_coef_vol, &glt_tcoef_vol,
     &glt_fstat_vol, &glt_rstat_vol, &fitts_vol, &errts_vol);

  if( xrestore ){   /* 26 Jul 2004 - very special operations */
    do_xrestore_stuff( argc,argv , option_data ) ;
    exit(0) ;
  }


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


#if 0
  /*----- Terminate program -----*/
  terminate_program (&option_data, &stimulus, &glt_cmat, &coef_vol, &scoef_vol,
                  &tcoef_vol, &fpart_vol, &rpart_vol, & mse_vol, &ffull_vol,
                 &rfull_vol, &glt_coef_vol, &glt_tcoef_vol, &glt_fstat_vol,
                  &glt_rstat_vol, &fitts_vol, &errts_vol);
#endif

  if( proc_use_jobs == 1 && verb ){ /* output requested - 2003.08.15 [rickr] */
    INFO_message("Program finished; elapsed time=%.3f",COX_clock_time());
  }
#ifndef FLOATIZE
  if( proc_numjob == 1 && verb ){ /* 16 Jan 2004: print operation count */
    double fv=get_matrix_flops() , fd=get_matrix_dotlen() ;
    if( fv > 0.0 && fd > 0.0 )
      INFO_message("#Flops=%g  Average Dot Product=%g",fv,fd) ;
  }
#endif

  exit(0);
}

/*============================================================================*/
/*--------- Grayplot X matrix columns to an image file [21 Jul 2004] ---------*/
/*----------------------------------------------------------------------------*/
#include "coxplot.h"

#define TSGRAY_SEPARATE_YSCALE (1<<0)
#define TSGRAY_FLIP_XY         (1<<1)

/*-----------------------------------------------------------------
   Plot some timeseries in grayscale
     npt     = number of points in each series
     nts     = number of series
     ymask   = operation modifier:
                 TSGRAY_SEPARATE_YSCALE
                 TSGRAY_FLIP_XY
     y[j][i] = i-th point in j-th timeseries,
               for i=0..npt-1, j=0..nts-1
-------------------------------------------------------------------*/

MEM_plotdata * PLOT_tsgray( int npt , int nts , int ymask , float **y )
{
   MEM_plotdata *mp ;
   float ybot,ytop , yfac , dx,dy , val ;
   int ii,jj , flipxy ;
   char str[32] ;
   int sepscl ;
   float boxr=1.0,boxg=0.9,boxb=0.0 ;
   char *eee ;
   int dobox=1 ;

   if( npt < 2 || nts < 1 || y == NULL ) return NULL ;

   eee = my_getenv( "AFNI_XJPEG_COLOR" ) ;
   if( eee != NULL ){
     float rf=-1.0, gf=-1.0 , bf=-1.0 ;
     sscanf( eee , "rgbi:%f/%f/%f" , &rf,&gf,&bf ) ;
     if( rf >= 0.0 && rf <= 1.0 && gf >= 0.0 && gf <= 1.0 && bf >= 0.0 && bf <= 1.0 ){
       boxr = rf ; boxg = gf ; boxb = bf ;
     }
     if( NOISH(eee) ) dobox = 0 ;  /* don't do boxes */
   }

   /* find range of all the data */

   ybot = ytop = y[0][0] ;
   for( jj=0 ; jj < nts ; jj++ ){
     for( ii=0 ; ii < npt ; ii++ ){
       val = y[jj][ii] ;
            if( ybot > val ) ybot = val ;
       else if( ytop < val ) ytop = val ;
     }
   }
   if( ybot >= ytop ) return NULL ;  /* data is all the same? */
   yfac = 1.0/(ytop-ybot) ;
   dx   = 0.9999/npt ;
   dy   = 0.9999/nts ;

   create_memplot_surely( "Gplot" , 1.0 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.0 ) ;
   set_opacity_memplot( 1.0 ) ;

   flipxy = (ymask & TSGRAY_FLIP_XY) != 0 ;
   sepscl = (ymask & TSGRAY_SEPARATE_YSCALE) != 0 ;

   for( jj=0 ; jj < nts ; jj++ ){

     if( sepscl ){
       ybot = ytop = y[jj][0] ; /* find range of data */
       for( ii=1 ; ii < npt ; ii++ ){
         val = y[jj][ii] ;
              if( ybot > val ) ybot = val ;
         else if( ytop < val ) ytop = val ;
       }
       if( ybot >= ytop ) yfac = 1.0 ;
       else               yfac = 1.0/(ytop-ybot) ;
     }

     for( ii=0 ; ii < npt ; ii++ ){
       val = yfac*(ytop-y[jj][ii]) ;
       set_color_memplot( val,val,val ) ;
       if( flipxy )
         plotrect_memplot( ii*dx,jj*dy , (ii+1)*dx,(jj+1)*dy ) ;
       else
         plotrect_memplot( jj*dy,1.0-ii*dx , (jj+1)*dy,1.0-(ii+1)*dy ) ;
     }
   }

   if( dobox ){
     set_color_memplot( boxr, boxg, boxb ) ; /* lines between each column */
     set_opacity_memplot( 0.789 ) ;
     for( jj=0 ; jj <= nts ; jj++ ){
       if( flipxy ){
         plotline_memplot( 1.0,jj*dy , 0.0,jj*dy ) ;
       } else {
         plotline_memplot( jj*dy,1.0 , jj*dy,0.0 ) ;
       }
     }
   }

   set_opacity_memplot( 1.0 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   mp = get_active_memplot() ;
   return mp ;
}

/*-----------------------------------------------------------------*/

MRI_IMAGE * PLOT_matrix_gray( matrix X )
{
   int nts=X.cols , npt=X.rows , ii,jj , nxim=768 , nyim=1024 ;
   MEM_plotdata *mp ;
   float **xar ;
   MRI_IMAGE *im ;
   char *eee ;

   if( nts < 1 || npt < 2 ) return NULL ;

   xar = (float **)malloc( sizeof(float *)*nts ) ;
   for( jj=0 ; jj < nts ; jj++ ){
     xar[jj] = (float *)malloc( sizeof(float *)*npt ) ;
     for( ii=0 ; ii < npt ; ii++ ) xar[jj][ii] = X.elts[ii][jj] ;
   }

   mp = PLOT_tsgray( npt , nts , TSGRAY_SEPARATE_YSCALE , xar ) ;

   for( jj=0 ; jj < nts ; jj++ ) free((void *)xar[jj]) ;
   free((void *)xar) ;

   if( mp == NULL ) return NULL ;

   eee = my_getenv( "AFNI_XJPEG_IMXY") ;
   if( eee != NULL ){
     int a=-1, b=-1 ;
     sscanf( eee , "%dx%d" , &a,&b ) ;
     if( a > 99 && b > 99 ){ nxim = a ; nyim = b ; }
     if( a < -1 ) nxim = -a*nts ;
     if( b <  0 ) nyim = -b*npt ;
   }

   im = mri_new( nxim , nyim , MRI_rgb ) ;
   memset( MRI_RGB_PTR(im) , 255 , 3*im->nvox ) ;  /* whiten it */
   memplot_to_RGB_sef( im , mp , 0,0,1 ) ;
   delete_memplot( mp ) ;
   return im ;
}

/*-----------------------------------------------------------------*/

void JPEG_matrix_gray( matrix X , char *fname )
{
   char *pg , *jpfilt, *eee ;
   MRI_IMAGE *im ;
   FILE *fp ;
   int jpeg_compress;

   if( fname == NULL || *fname == '\0' ) return ;

   pg = THD_find_executable( "cjpeg" ) ;
   if( pg == NULL ){
     WARNING_message(
             "can't save %s because program 'cjpeg' not in path!",
             fname) ;
     return ;
   }

   im = PLOT_matrix_gray( X ) ;
   if( im == NULL ){
     WARNING_message(
             "can't save %s because of internal error!",fname) ;
     return ;
   }

   eee = my_getenv("AFNI_JPEG_COMPRESS");
   if(eee!=NULL) {
      jpeg_compress = strtod(eee, NULL);
      if((jpeg_compress<=0) || (jpeg_compress>100))
         jpeg_compress = 95;
    }
   else jpeg_compress = 95;

   jpfilt = (char *)malloc( sizeof(char)*(strlen(pg)+strlen(fname)+32) ) ;

   sprintf( jpfilt , "%s -quality %d > %s" , pg , jpeg_compress, fname ) ;

#ifndef CYGWIN
   signal( SIGPIPE , SIG_IGN ) ; errno = 0 ;
#endif
   fp = popen( jpfilt , "w" ) ;
   if( fp == NULL ){
     mri_free(im) ; free((void *)jpfilt) ;
     WARNING_message("can't save %s because cjpeg filter fails!",fname) ;
     return ;
   }
   fprintf(fp,"P6\n%d %d\n255\n" , im->nx,im->ny ) ;
   fwrite( MRI_RGB_PTR(im), sizeof(byte), 3*im->nvox, fp ) ;
   (void) pclose(fp) ;
   if( verb ) INFO_message("Wrote matrix image to file %s",fname) ;

   mri_free(im) ; free((void *)jpfilt) ; return ;
}

/*----------------------------------------------------------------------------*/
/*! Save matrix to a .1D text file */

void ONED_matrix_save( matrix X , char *fname , void *xd )
{
   int nx=X.rows , ny=X.cols , ii,jj ;
   column_metadata *cd = (column_metadata *)xd ;

   if( fname == NULL || *fname == '\0' ) return ;

   if( cd == NULL ){  /*---- standard save, plain text ----*/

     MRI_IMAGE *xim = mri_new( nx , ny , MRI_float ) ;
     float     *xar = MRI_FLOAT_PTR(xim) ;
     for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ ) xar[ii+jj*nx] = X.elts[ii][jj] ;
     if( THD_is_file(fname) ) remove(fname) ;  /* 09 Mar 2007 */
     mri_write_1D(fname,xim) ; mri_free(xim) ;

   } else {  /*---- save with NIML information ----*/

     NI_element *nel ; NI_stream ns ; MTYPE *col ;
     char *lab=NULL,lll[128] ;
     nel = NI_new_data_element( "matrix" , nx ) ;
     col = (MTYPE *)malloc(sizeof(MTYPE)*nx) ;
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ) col[ii] = X.elts[ii][jj] ;
       NI_add_column( nel , NI_MTYPE , col ) ;
       strcpy(lll,cd[jj].name) ;
       for( ii=0 ; lll[ii] != '\0' ; ii++ ) if( isspace(lll[ii]) ) lll[ii]='_';
       if( jj < ny-1 ) strcat(lll," ; ") ;
       lab = THD_zzprintf( lab , "%s" , lll ) ;
     }
     NI_set_attribute( nel , "ColumnLabels" , lab ) ;
     free((void *)col) ; free((void *)lab) ; lab = NULL ;
#if 0
     for( jj=0 ; jj < ny ; jj++ ){
       sprintf(lll,"%u",cd[jj].mask) ; if( jj < ny-1 ) strcat(lll,";") ;
       lab = THD_zzprintf( lab , "%s" , lll ) ;
     }
     NI_set_attribute( nel, "ColumnMasks", lab ); free((void *)lab); lab = NULL;
#endif
#if 1
     for( jj=0 ; jj < ny ; jj++ ){
       sprintf(lll,"%d",cd[jj].group) ; if( jj < ny-1 ) strcat(lll,";") ;
       lab = THD_zzprintf( lab , "%s" , lll ) ;
     }
     NI_set_attribute( nel, "ColumnGroups", lab ); free((void *)lab); lab = NULL;
#endif
#if 1
     lab = THD_zzprintf( lab , "%g" , basis_TR ) ;
     NI_set_attribute( nel, "RowTR", lab ); free((void *)lab); lab = NULL;
#endif
     NI_write_element_tofile( fname, nel, NI_HEADERSHARP_FLAG | NI_TEXT_MODE );
     NI_free_element( nel ) ;
   }
   if( verb ) INFO_message("Wrote matrix values to file %s",fname) ;
   return ;
}

/*----------------------------------------------------------------------------*/
/*---------  End of Grayplot addition  ---------------------------------------*/
/*============================================================================*/

/*============================================================================*/
/*-- Save X, XtXinv, and XtXinvXt matrices in bucket file for later re-use. --*/
/*----------------------------------------------------------------------------*/

#include "niml.h"

#if 0
/*-----------------------------------------------------------------*/
/*! Turn a NIML element into a string.
-------------------------------------------------------------------*/

char * niml_element_to_string( NI_element *nel )
{
   NI_stream ns ;
   char *stout ;
   int ii,jj ;

   if( nel == NULL ) return NULL ;

   ns = NI_stream_open( "str:" , "w" ) ;
   (void) NI_write_element( ns , nel , NI_TEXT_MODE ) ;
   stout = strdup( NI_stream_getbuf(ns) ) ;
   NI_stream_close( ns ) ;
   jj = strlen(stout) ;
   for( ii=jj-1 ; ii > 0 && isspace(stout[ii]) ; ii-- ) ; /* trailing blanks */
   stout[ii+1] = '\0' ;
   return stout ;
}
#endif

/*-----------------------------------------------------------------*/
/*! Encode a matrix into a NIML element, which is returned;
    NI_free_element() this when done with it.
    ename is the name of the NIML element to create.
-------------------------------------------------------------------*/

NI_element * matrix_to_niml( matrix a , char *ename )
{
   int m=a.rows , n=a.cols , ii,jj ;
   MTYPE **aar = a.elts ;
   NI_element *nel ;
   double *ecol ;

   if( m < 1 || n < 1 || aar == NULL ) return NULL ;  /* bad user, bad */

   if( ename == NULL || *ename == '\0' ) ename = "matrix" ;

   nel  = NI_new_data_element( ename , m ) ;
   ecol = (double *) malloc(sizeof(double)*m) ;

   for( jj=0 ; jj < n ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ) ecol[ii] = aar[ii][jj] ;
     NI_add_column( nel , NI_DOUBLE , ecol ) ;
   }

   free((void *)ecol) ;
   return nel ;
}

/*-------------------------------------------------------------------*/
/*! Decode an NIML element into a matrix.
---------------------------------------------------------------------*/

void niml_to_matrix( NI_element *nel , matrix *a )
{
   int m , n , ii , jj ;
   double *ecol ;
   MTYPE **aar ;
   char *ename ;

   if( nel == NULL || a == NULL ) return ;

   m = nel->vec_len ;   /* number of rows */
   n = nel->vec_num ;   /* number of cols */

   if( m < 1 || n < 1 ) return ;
   for( jj=0 ; jj < n ; jj++ )
     if( nel->vec_typ[jj] != NI_DOUBLE ) return ;

   matrix_create( m , n , a ) ;
   aar = a->elts ;

   for( jj=0 ; jj < n ; jj++ ){
     ecol = (double *)nel->vec[jj] ;
     for( ii=0 ; ii < m ; ii++ ) aar[ii][jj] = (MTYPE)ecol[ii] ;;
   }

   return ;
}

/*-----------------------------------------------------------------*/
/*! Encode an integer list into a NIML element.
    NI_free_element() this when done with it.
    ename is the name of the NIML element to create.
-------------------------------------------------------------------*/

NI_element * intvec_to_niml( int nvec , int *vec , char *ename )
{
   NI_element *nel ;

   if( nvec < 1 || vec == NULL ) return NULL ;  /* bad user, bad */

   if( ename == NULL || *ename == '\0' ) ename = "intvec" ;

   nel = NI_new_data_element( ename , nvec ) ;
   NI_add_column( nel , NI_INT , vec ) ;
   return nel ;
}

/*-------------------------------------------------------------------*/

void niml_to_intvec( NI_element *nel , int *nvec , int **vec )
{
   if( nel == NULL || nvec == NULL || vec == NULL ) return ;

   if( nel->vec_len < 1 || nel->vec_num < 1 ) return ;
   if( nel->vec_typ[0] != NI_INT ) return ;

   *nvec = nel->vec_len ;
   *vec  = (int *)malloc(*nvec*sizeof(int)) ;
   memcpy(*vec,nel->vec[0],*nvec*sizeof(int)) ;
   return ;
}

/*-------------------------------------------------------------------*/

NI_element * stringvec_to_niml( int nstr , char **str , char *ename )
{
   NI_element *nel ;

   if( nstr < 1 || str == NULL ) return NULL ;  /* bad user, bad */

   if( ename == NULL || *ename == '\0' ) ename = "stringvec" ;

   nel = NI_new_data_element( ename , nstr ) ;
   NI_add_column( nel , NI_STRING , str ) ;
   return nel ;
}

/*-------------------------------------------------------------------*/

void niml_to_stringvec( NI_element *nel , int *nstr , char ***str )
{
   int ii ;
   char **sv ;

   if( nel == NULL || nstr == NULL || str == NULL ) return ;

   if( nel->vec_len < 1 || nel->vec_num < 1 ) return ;
   if( nel->vec_typ[0] != NI_STRING ) return ;
   sv = (char **) nel->vec[0] ;

   *nstr = nel->vec_len ;
   *str  = (char **)calloc(sizeof(char *),*nstr) ;
   for( ii=0 ; ii < *nstr ; ii++ ) (*str)[ii] = strdup(sv[ii]) ;
   return ;
}

/*-------------------------------------------------------------------*/

NI_element * symvec_to_niml( int ns , SYM_irange *sv , char *ename )
{
   NI_element *nel ;
   int ii , *bc,*tc,*gc ; char **sc ;

   if( ns < 1 || sv == NULL ) return NULL ;  /* bad user, bad */

   if( ename == NULL || *ename == '\0' ) ename = "symvec" ;

   nel = NI_new_data_element( ename , ns ) ;

   bc  = (int *)  malloc(sizeof(int)   *ns) ;
   tc  = (int *)  malloc(sizeof(int)   *ns) ;
   gc  = (int *)  malloc(sizeof(int)   *ns) ;
   sc  = (char **)malloc(sizeof(char *)*ns) ;

   for( ii=0 ; ii < ns ; ii++ ){
     bc[ii] = sv[ii].nbot ; tc[ii] = sv[ii].ntop ;
     gc[ii] = sv[ii].gbot ; sc[ii] = sv[ii].name ;
   }

   NI_add_column( nel , NI_INT ,    bc ); free((void *)bc) ;
   NI_add_column( nel , NI_INT    , tc ); free((void *)tc) ;
   NI_add_column( nel , NI_INT    , gc ); free((void *)gc) ;
   NI_add_column( nel , NI_STRING , sc ); free((void *)sc) ;

   return nel ;
}

/*-------------------------------------------------------------------*/

void niml_to_symvec( NI_element *nel , int *ns , SYM_irange **sv )
{
   int ii , *bc,*tc,*gc ; char **sc ;

   if( nel == NULL || ns == NULL || sv == NULL ) return ;

   if( nel->vec_len < 1 || nel->vec_num < 4 ) return ;
   if( nel->vec_typ[0] != NI_INT    ) return ;  bc = (int *)  nel->vec[0] ;
   if( nel->vec_typ[1] != NI_INT    ) return ;  tc = (int *)  nel->vec[1] ;
   if( nel->vec_typ[2] != NI_INT    ) return ;  gc = (int *)  nel->vec[2] ;
   if( nel->vec_typ[3] != NI_STRING ) return ;  sc = (char **)nel->vec[3] ;

   *ns = nel->vec_len ;
   *sv = (SYM_irange *)calloc(sizeof(SYM_irange),*ns) ;
   for( ii=0 ; ii < *ns ; ii++ ){
     (*sv)[ii].nbot = bc[ii] ;
     (*sv)[ii].ntop = tc[ii] ;
     (*sv)[ii].gbot = gc[ii] ;
     NI_strncpy( (*sv)[ii].name , sc[ii] , 64 ) ;
   }
   return ;
}

/*-------------------------------------------------------------------*/

void XSAVE_output( char *prefix )
{
   char *fname , *cpt ;
   NI_stream ns ;
   NI_element *nel ;
   int nimode = NI_BINARY_MODE ;

   if( !xsave ) return ;

   if( AFNI_yesenv("AFNI_XSAVE_TEXT") ) nimode = NI_TEXT_MODE ;

   /*-- open output stream --*/

   if( prefix == NULL || *prefix == '\0' ) prefix = "X" ;

   fname  = malloc( sizeof(char) * (strlen(prefix)+32) ) ;
   strcpy(fname,"file:") ; strcat(fname,prefix) ; strcat(fname,".xsave") ;
   if( THD_is_ondisk(fname+5) && verb )
     WARNING_message(
             "-xsave output file %s will be overwritten!",fname+5) ;
   ns = NI_stream_open( fname , "w" ) ;
   if( ns == (NI_stream)NULL ){
     ERROR_message(
             "-xsave output file %s can't be opened!",fname+5) ;
     free((void *)fname) ;
     return ;
   }

   /*-- write a header element --*/

   nel = NI_new_data_element( "XSAVE", 0 ) ;
   if( InputFilename != NULL )
     NI_set_attribute( nel , "InputFilename"  , InputFilename  ) ;
   if( BucketFilename != NULL )
     NI_set_attribute( nel , "BucketFilename" , BucketFilename ) ;
   if( CoefFilename != NULL )
     NI_set_attribute( nel , "CoefFilename"   , CoefFilename   ) ;

   sprintf(fname,"%d",X.rows) ;
   NI_set_attribute( nel , "NumTimePoints" , fname ) ;
   sprintf(fname,"%d",X.cols) ;
   NI_set_attribute( nel , "NumRegressors" , fname ) ;

   NI_set_attribute( nel , "Deconvolveries" , XSAVE_version ) ;

   cpt = tross_datetime() ;
   NI_set_attribute( nel , "DateTime" , cpt ) ;
   free((void *)cpt) ;
   cpt = tross_hostname() ;
   NI_set_attribute( nel , "Hostname" , cpt ) ;
   free((void *)cpt) ;
   cpt = tross_username() ;
   NI_set_attribute( nel , "Username" , cpt ) ;
   free((void *)cpt) ;

   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   /*-- write the matrices --*/

   nel = matrix_to_niml( X , "matrix" ) ;
   NI_set_attribute( nel , "Xname" , "X" ) ;
   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   nel = matrix_to_niml( XtXinv , "matrix" ) ;
   NI_set_attribute( nel , "Xname" , "XtXinv" ) ;
   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   nel = matrix_to_niml( XtXinvXt , "matrix" ) ;
   NI_set_attribute( nel , "Xname" , "XtXinvXt" ) ;
   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   /*-- list of good time points --*/

   nel = intvec_to_niml( nGoodList , GoodList , "intvec" ) ;
   NI_set_attribute( nel , "Xname" , "GoodList" ) ;
   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   /*-- list of bucket indices with estimated parameters --*/

   if( ParamIndex != NULL ){
     nel = intvec_to_niml( nParam, ParamIndex , "intvec" ) ;
     NI_set_attribute( nel , "Xname" , "ParamIndex" ) ;
     (void) NI_write_element( ns , nel , nimode ) ;
     NI_free_element( nel ) ;
   }

   /*-- which stimlus input file, for each parameter --*/

   nel = intvec_to_niml( nParam, ParamStim , "intvec" ) ;
   NI_set_attribute( nel , "Xname" , "ParamStim" ) ;
   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   /*-- stimulus label, for each parameter --*/

   nel = stringvec_to_niml( nParam , ParamLabel , "stringvec" ) ;
   NI_set_attribute( nel , "Xname" , "ParamLabel" ) ;
   (void) NI_write_element( ns , nel , nimode ) ;
   NI_free_element( nel ) ;

   /*-- stimulus symbols [29 Jul 2004] --*/

   if( nSymStim > 0 ){
     nel = symvec_to_niml( nSymStim , SymStim , "symvec" ) ;
     NI_set_attribute( nel , "Xname" , "SymStim" ) ;
     (void) NI_write_element( ns , nel , nimode ) ;
     NI_free_element( nel ) ;
   }

   /*-- done, finito, ciao babee --*/

   NI_stream_close(ns) ; free((void *)fname) ; return ;
}

/*-------------------------------------------------------------------*/

#define DPR(s) fprintf(stderr,"%s\n",(s))

void XSAVE_input( char *xname )
{
   char *fname , *cpt ;
   NI_stream ns ;
   NI_element *nel ;

   if( xname == NULL || *xname == '\0' ) return ;

   /*-- open input file --*/

   fname  = malloc( sizeof(char) * (strlen(xname)+32) ) ;
   strcpy(fname,"file:") ; strcat(fname,xname) ;
   ns = NI_stream_open( fname , "r" ) ;
   free((void *)fname) ;
   if( ns == (NI_stream)NULL ){
     ERROR_message("can't open file %s for -xrestore!",xname) ;
     return ;
   }

   /*-- read and decode header element --*/

   nel = NI_read_element( ns , 1 ) ;
   if( nel == NULL ){
     ERROR_message("can't read header in file %s for -xrestore!",xname) ;
     NI_stream_close( ns ) ; return ;
   }

   /* extract filenames */

   cpt = NI_get_attribute( nel , "InputFilename"  ) ;
                if( cpt != NULL ) InputFilename = strdup(cpt) ;
   cpt = NI_get_attribute( nel , "BucketFilename" ) ;
                if( cpt != NULL ) BucketFilename = strdup(cpt) ;
   cpt = NI_get_attribute( nel , "CoefFilename"   ) ;
                if( cpt != NULL ) CoefFilename = strdup(cpt) ;

   /* extract other stuff (not used, yet) */

   cpt = NI_get_attribute( nel , "NumTimePoints" ) ;
                if( cpt != NULL ) NumTimePoints = strtol(cpt,NULL,10) ;
   cpt = NI_get_attribute( nel , "NumRegressors" ) ;
                if( cpt != NULL ) NumRegressors = strtol(cpt,NULL,10) ;

   NI_free_element( nel ) ;

   /*-- read succeeding elements, decode and store them --*/

   while(1){

     nel = NI_read_element( ns , 1 ) ;
     if( nel == NULL ) break ;          /* end of input */

     cpt = NI_get_attribute( nel , "Xname" ) ;  /*- name of variable to save -*/
     if( cpt == NULL ){
       NI_free_element( nel ) ; continue ;        /*- unnamed ==> skip this! -*/
     }

     if( strcmp(nel->name,"matrix") == 0 ){              /*- matrix elements -*/

            if( strcmp(cpt,"X"       )==0 ) niml_to_matrix( nel , &X        );
       else if( strcmp(cpt,"XtXinv"  )==0 ) niml_to_matrix( nel , &XtXinv   );
       else if( strcmp(cpt,"XtXinvXt")==0 ) niml_to_matrix( nel , &XtXinvXt );

     } else if( strcmp(nel->name,"intvec") == 0 ){       /*- intvec elements -*/

            if( strcmp(cpt,"GoodList")   == 0 )
                              niml_to_intvec( nel , &nGoodList, &GoodList   );
       else if( strcmp(cpt,"ParamIndex") == 0 )
                              niml_to_intvec( nel , &nParam   , &ParamIndex );
       else if( strcmp(cpt,"ParamStim" ) == 0 )
                              niml_to_intvec( nel , &nParam   , &ParamStim  );

     } else if( strcmp(nel->name,"stringvec") == 0 ){ /*- stringvec elements -*/

       if( strcmp(cpt,"ParamLabel") == 0 )
                             niml_to_stringvec( nel , &nParam , &ParamLabel );

     } else if( strcmp(nel->name,"symvec") == 0 ){       /*- symvec elements -*/

       if( strcmp(cpt,"SymStim") == 0 )
                                 niml_to_symvec( nel , &nSymStim , &SymStim );

     } else {                                            /*- other elements? -*/
                                                      /*- silently skip them -*/
     }

     NI_free_element( nel ) ;  /* toss the trash (or recycle it) */

   } /*-- end of loop over elements in file --*/

   NI_stream_close( ns ) ; return ;
}

/*============================================================================*/
/*------ xrestore operations: 26 Jul 2004 ------------------------------------*/
/*----------------------------------------------------------------------------*/

void check_xrestore_data(void)
{
   int nerr = 0 ;

   if( X.rows < 1 || X.cols < 1 ){
     ERROR_message(
             "-xrestore %s has bad X matrix",xrestore_filename) ;
     nerr++ ;
   }
   if( XtXinv.rows < 1 || XtXinv.cols < 1 ){
     ERROR_message(
             "-xrestore %s has bad XtXinv matrix",xrestore_filename) ;
     nerr++ ;
   }
   if( XtXinvXt.rows < 1 || XtXinvXt.cols < 1 ){
     ERROR_message(
             "-xrestore %s has bad XtXinvXt matrix",xrestore_filename) ;
     nerr++ ;
   }
   if( nParam > 0 && X.cols != nParam ){
     ERROR_message(
             "-xrestore %s X matrix cols mismatch: %d != %d",
             xrestore_filename, X.cols , nParam ) ;
     nerr++ ;
   }
   if( GoodList == NULL ){
     ERROR_message(
             "-xrestore %s missing GoodList field",xrestore_filename) ;
     nerr++ ;
   } else if( nGoodList != X.rows ){
     ERROR_message(
             "-xrestore %s X matrix rows mismatch: %d != %d",
             xrestore_filename, X.cols , nGoodList ) ;
     nerr++ ;
   }
#if 0
   if( ParamStim == NULL ){
     ERROR_message(
             "-xrestore %s missing ParamStim field",xrestore_filename) ;
     nerr++ ;
   }
   if( ParamLabel == NULL ){
     ERROR_message(
             "-xrestore %s missing ParamLabel field",xrestore_filename) ;
     nerr++ ;
   }
#endif
   if( InputFilename == NULL ){
     ERROR_message(
             "-xrestore %s missing InputFilename field",xrestore_filename) ;
     nerr++ ;
   }

   if( nerr > 0 ) exit(1) ;   /** bad bad bad **/
   return ;
}

/*-------------------------------------------------------------------*/

void do_xrestore_stuff( int argc , char **argv , DC_options *option_data )
{
   THD_3dim_dataset *dset_time , *dset_coef, *dset_buck ;
   int nt , np , ii, nxyz, tout,rout,fout, ixyz,novar,view ;
   char *buck_prefix , *buck_name ;
   char brick_label[THD_MAX_NAME] ;

   float *ts_array = NULL; /* array of measured data for one voxel */
#undef  NGET
#define NGET 32            /* number to get at one time */
   int nget=0 ,            /* number of time series current gotten */
       cget=0 ,            /* index of next timeseries in iget & imget */
       jget   ,            /* loop index for iget */
       iget[NGET] ;        /* voxel index of timeseries */
   MRI_IMARR *imget=NULL ; /* array of timeseries */

   int num_glt , iglt , ilc,nlc ;
   matrix *glt_cmat , *glt_amat , *cxtxinvct ;
   vector *glt_coef , *glt_tcoef, y , coef ;
   float *fglt , *rglt ;
   char **glt_label ;
   int *glt_rows ;
   float ***glt_coef_vol       ,
         ***glt_tcoef_vol=NULL ,
         ** glt_fstat_vol=NULL ,
         ** glt_rstat_vol=NULL  ;
   float *cdar=NULL , ssef , *volume ;
   int ivol , nvol , nbuck , vstep ;

   /*----- Check for GLT options -----*/

   num_glt   = option_data->num_glt   ;
   glt_label = option_data->glt_label ;
   glt_rows  = option_data->glt_rows  ;

   tout = option_data->tout ;
   rout = option_data->rout ;
   fout = option_data->fout ;

   if( num_glt < 1 )
     ERROR_exit("-xrestore with no new GLTs???") ;

   /*----- initialize values to be read from xsave file -----*/

   matrix_initialize( &X )        ;
   matrix_initialize( &XtXinv )   ;
   matrix_initialize( &XtXinvXt ) ;
   nGoodList = 0 ; GoodList   = NULL ;
   nParam    = 0 ; ParamIndex = NULL ; ParamStim = NULL ; ParamLabel = NULL ;

   /*----- read xsave file -----*/

   if( verb ) INFO_message("Starting -xrestore %s",xrestore_filename) ;

   XSAVE_input( xrestore_filename ) ;
   check_xrestore_data() ;

   nt = X.rows ; np = X.cols ;  /* number of time points and parameters */

   /*----- read input time series dataset -----*/

   if( verb ) INFO_message("loading time series dataset '%s'\n",InputFilename);
   dset_time = THD_open_dataset( InputFilename ) ;
   if( dset_time == NULL )
     ERROR_exit(
             "-xrestore can't open time series dataset '%s'" ,
             InputFilename ) ;

   DSET_load( dset_time ) ;
   if( !DSET_LOADED(dset_time) )
     ERROR_exit(
             "-xrestore can't load time series dataset '%s'" ,
             InputFilename ) ;

   nxyz = DSET_NVOX(dset_time) ;  /* number of voxels */
   view = dset_time->view_type ;  /* +orig, +acpc, +tlrc ? */

   /*----- read coefficient dataset (if possible) -----*/

   dset_coef = NULL ;
   if( CoefFilename != NULL ){
     if( verb) INFO_message("loading coefficient dataset %s\n",CoefFilename);
     dset_coef = THD_open_dataset( CoefFilename ) ;
     if( dset_coef == NULL ){
       WARNING_message(
               "-xrestore can't open coefficient dataset %s",
               CoefFilename);
     } else {
       DSET_load(dset_coef) ;
       if( !DSET_LOADED(dset_coef) ){
         WARNING_message(
                 "-xrestore can't load coefficient dataset %s",
                 CoefFilename);
         DSET_delete(dset_coef) ; dset_coef = NULL ;
       }
     }
     if( dset_coef != NULL && DSET_NVALS(dset_coef) < np ){
       WARNING_message(
               "-xrestore coefficient dataset %s too short",
               CoefFilename);
       DSET_delete(dset_coef) ; dset_coef = NULL ;
     }
     if( dset_coef != NULL ){
       if( ParamIndex != NULL ) free((void *)ParamIndex) ;
       ParamIndex = (int *)malloc(sizeof(int)*np) ;
       for( ii=0 ; ii < np ; ii++ ) ParamIndex[ii] = ii ;
     }
   }

   /*----- if above failed, try the old bucket dataset -----*/

   if( dset_coef == NULL && BucketFilename != NULL && ParamIndex != NULL ){
     if( verb ) INFO_message("loading original bucket dataset %s\n",BucketFilename) ;
     dset_coef = THD_open_dataset( BucketFilename ) ;
     if( dset_coef == NULL ){
       WARNING_message(
               "-xrestore can't open old bucket dataset %s",
               BucketFilename);
     } else {
       DSET_load(dset_coef) ;
       if( !DSET_LOADED(dset_coef) ){
         WARNING_message(
                 "-xrestore can't load old bucket dataset %s",
                 BucketFilename);
         DSET_delete(dset_coef) ; dset_coef = NULL ;
       }
     }
     if( dset_coef != NULL && DSET_NVALS(dset_coef) < ParamIndex[np-1] ){
       WARNING_message(
               "-xrestore old bucket dataset %s too short",
               BucketFilename);
       DSET_delete(dset_coef) ; dset_coef = NULL ;
     }
   }

   if( ISVALID_DSET(dset_coef) && DSET_NVOX(dset_coef) != nxyz )
     ERROR_exit("dataset mismatch between time series and coef!") ;

   /*----- neither worked ==> must recompute from input data time series -----*/

   if( dset_coef == NULL && verb )
    INFO_message("-xrestore recomputing coefficients from time series");

   /*----- read new GLT matrices -----*/

   glt_cmat = (matrix *) malloc( sizeof(matrix) * num_glt ) ;

   for( iglt=0; iglt < num_glt ; iglt++){
     matrix_initialize( glt_cmat + iglt ) ;
#if 1
     read_glt_matrix( option_data->glt_filename[iglt] ,
                      option_data->glt_rows + iglt ,
                      np , glt_cmat + iglt             ) ;
#else
     matrix_file_read ( option_data->glt_filename[iglt] ,
                            option_data->glt_rows[iglt] ,
                        np , glt_cmat+iglt , 1           ) ;
     if( glt_cmat[iglt].elts == NULL )
       ERROR_exit( "Can't read GLT matrix from file %s",
                   option_data->glt_filename[iglt] ) ;
#endif
   }

   /*----- initialize new GLT calculations:
            - setup space for matrices and vectors
            - calculate matrices
            - malloc space for output bricks -----*/

   cxtxinvct = (matrix *) malloc( sizeof(matrix) * num_glt ) ;
   glt_amat  = (matrix *) malloc( sizeof(matrix) * num_glt ) ;
   glt_coef  = (vector *) malloc( sizeof(vector) * num_glt ) ;
   glt_tcoef = (vector *) malloc( sizeof(vector) * num_glt ) ;

   for( iglt=0 ; iglt < num_glt ; iglt++ ){
     matrix_initialize( cxtxinvct+iglt ) ;  /* will be loaded in   */
     matrix_initialize( glt_amat +iglt ) ;  /* init_glt_analysis() */
     vector_initialize( glt_coef +iglt ) ;
     vector_initialize( glt_tcoef+iglt ) ;
   }

   fglt = (float *) malloc( sizeof(float) * num_glt ) ;
   rglt = (float *) malloc( sizeof(float) * num_glt ) ;

   vector_initialize( &y )   ; vector_create( nt, &y    );  /* time series */
   vector_initialize( &coef ); vector_create( np, &coef );  /* parameters */

   if( dset_coef != NULL )
     cdar = (float *)malloc( sizeof(float) * DSET_NVALS(dset_coef) ) ;

   init_glt_analysis( XtXinv , num_glt , glt_cmat , glt_amat , cxtxinvct ) ;

   /*-- malloc output bricks --*/

   glt_coef_vol = (float ***) malloc( sizeof(float **) * num_glt ) ;
   if( tout )
     glt_tcoef_vol  = (float ***) malloc( sizeof(float **) * num_glt ) ;
   if( fout )
     glt_fstat_vol = (float **) malloc( sizeof(float *)  * num_glt ) ;
   if( rout )
     glt_rstat_vol = (float **) malloc( sizeof(float *)  * num_glt ) ;

   nvol = 0 ;
   for( iglt=0 ; iglt < num_glt ; iglt++ ){
     nlc = glt_rows[iglt];
     glt_coef_vol[iglt] = (float **) malloc( sizeof(float *) * nlc ) ;
     if( tout )
       glt_tcoef_vol[iglt] = (float **) malloc( sizeof(float *) * nlc ) ;
     for( ilc=0 ; ilc < nlc ; ilc++ ){
       glt_coef_vol[iglt][ilc] = (float *)calloc(sizeof(float),nxyz); nvol++;
       if( tout ){
         glt_tcoef_vol[iglt][ilc] = (float *)calloc(sizeof(float),nxyz); nvol++;
       }
     }
     if( fout ){
       glt_fstat_vol[iglt] = (float *)calloc( sizeof(float), nxyz ); nvol++;
     }
     if( rout ){
       glt_rstat_vol[iglt] = (float *)calloc( sizeof(float), nxyz ); nvol++;
     }
   }

   /*----- loop over voxels:
            - fetch coefficients (check for all zero), or recompute them
            - fetch time series
            - compute SSE of full model
            - compute and store new GLT results in arrays -----*/

   vstep = nxyz / 50 ; if( !verb ) vstep = 0 ;
   if( vstep > 0 ) fprintf(stderr,"++ voxel loop:") ;
   for( ixyz=0 ; ixyz < nxyz ; ixyz++ ){

     if( vstep > 0 && ixyz%vstep == vstep-1 ) vstep_print() ;

     /*** race ahead and extract a bunch of voxel time series at once ***/

     if( cget == nget ){
       if( imget != NULL ) DESTROY_IMARR(imget) ;
       iget[0] = ixyz ; nget = 1 ;
       for( jget=ixyz+1 ; jget < nxyz && nget < NGET ; jget++ )
         iget[nget++] = jget ;
       imget = THD_extract_many_series( nget, iget, dset_time ) ;
       cget  = 0 ;  /* the next one to take out of imget */
     }

     /*** Extract Y-data for this voxel ***/

     ts_array = MRI_FLOAT_PTR(IMARR_SUBIM(imget,cget)) ; cget++ ;
     for( ii=0 ; ii < nt ; ii++ ) y.elts[ii] = ts_array[GoodList[ii]];

     /*** Extract or recompute coefficients for this voxel ***/

     if( dset_coef != NULL ){
       (void) THD_extract_array( ixyz , dset_coef , 0 , cdar ) ;
       for( ii=0 ; ii < np ; ii++ ) coef.elts[ii] = cdar[ParamIndex[ii]] ;
     } else {
       vector_multiply( XtXinvXt , y , &coef ) ;
     }

     /*** if coef is all zero, skip this voxel ***/

     for( ii=0 ; ii < np ; ii++ ) if( coef.elts[ii] != 0.0 ) break ;
     novar = (ii==np) ;

     if( !novar ) ssef = calc_sse( X , coef , y ) ;

     /************************************/
     /*** Do the work we came here for ***/
     /************************************/

     glt_analysis( nt , np ,
                   X , y , ssef , coef , novar ,
                   cxtxinvct , num_glt , glt_rows , glt_cmat , glt_amat ,
                   glt_coef , glt_tcoef , fglt , rglt ) ;

     /*** save results into output bricks ***/

     if (glt_coef_vol != NULL)
      for (iglt = 0;  iglt < num_glt;  iglt++)
       if (glt_coef_vol[iglt] != NULL)
        for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
         glt_coef_vol[iglt][ilc][ixyz] = glt_coef[iglt].elts[ilc];

     if (glt_tcoef_vol != NULL)
      for (iglt = 0;  iglt < num_glt;  iglt++)
       if (glt_tcoef_vol[iglt] != NULL)
        for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
         glt_tcoef_vol[iglt][ilc][ixyz] = glt_tcoef[iglt].elts[ilc];

     if (glt_fstat_vol != NULL)
      for (iglt = 0;  iglt < num_glt;  iglt++)
       if (glt_fstat_vol[iglt] != NULL)
        glt_fstat_vol[iglt][ixyz] = fglt[iglt];

     if (glt_rstat_vol != NULL)
      for (iglt = 0;  iglt < num_glt;  iglt++)
       if (glt_rstat_vol[iglt] != NULL)
        glt_rstat_vol[iglt][ixyz] = rglt[iglt];

   } /*** end of loop over voxels */

   if( vstep > 0 ) fprintf(stderr,"\n") ;  /* end of progress meter */

   /*** unload input datasets to save memory ***/

                           DSET_unload( dset_time ) ;
   if( dset_coef != NULL ) DSET_delete( dset_coef ) ;

   /*----- open old dataset for output if
             (a) -bucket was given for an existing dataset, or
             (b) no -bucket option was given;
            otherwise, open a new dataset for output of the GLT results -----*/

   if( option_data->bucket_filename != NULL ){
     buck_prefix = strdup(option_data->bucket_filename) ;
   } else if( BucketFilename != NULL ){
     buck_prefix = (char *)malloc(strlen(BucketFilename)+4) ;
     FILENAME_TO_PREFIX( BucketFilename , buck_prefix ) ;
   } else {
     buck_prefix = strdup("X") ;   /* bad user, bad bad bad */
   }

   /*** try to open dataset as an old one **/

   buck_name = (char *)malloc(strlen(buck_prefix)+32) ;
   strcpy(buck_name,buck_prefix) ;
   strcat(buck_name,"+") ; strcat(buck_name,VIEW_codestr[view]) ;

   dset_buck = THD_open_dataset( buck_name ) ;

   if( dset_buck != NULL ){

     if( verb) INFO_message("-xrestore appending to dataset %s",buck_name) ;
     DSET_mallocize( dset_buck ) ;
     if( DSET_NVOX(dset_buck) != nxyz )
       ERROR_exit(
               "dataset %s mismatch with time series '%s'" ,
               buck_name , DSET_HEADNAME(dset_time)       );

     DSET_load( dset_buck ) ;
     if( !DSET_LOADED(dset_buck) )
       ERROR_exit( "can't load dataset %s from disk" , buck_name ) ;

     /* add nvol empty bricks at the end */

     ivol = DSET_NVALS(dset_buck) ;   /* where to save first new brick */

     EDIT_add_bricklist( dset_buck , nvol, NULL, NULL, NULL ) ;

   } else {  /*** create a new dataset ***/

     if( verb ) INFO_message("xrestore creating new dataset %s",buck_name) ;

     dset_buck = EDIT_empty_copy( dset_time ) ;
     (void) EDIT_dset_items( dset_buck ,
                             ADN_prefix,          buck_prefix ,
                             ADN_type,            HEAD_FUNC_TYPE,
                             ADN_func_type,       FUNC_BUCK_TYPE,
                             ADN_datum_all,       (floatout)?MRI_float:MRI_short,
                             ADN_ntt,             0,  /* no time axis */
                             ADN_nvals,           nvol ,
                             ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
                             ADN_none ) ;
     ivol = 0 ;
   }

   tross_Make_History( PROGRAM_NAME , argc , argv , dset_buck ) ;

   nbuck = DSET_NVALS(dset_buck) ;

   /*** attach sub-bricks to output ***/

   for( iglt=0 ; iglt < num_glt ; iglt++ ){

     for( ilc=0 ; ilc < glt_rows[iglt] ; ilc++ ){
#ifdef USE_OLD_LABELS
       sprintf( brick_label , "%s LC[%d] coef" , glt_label[iglt], ilc ) ;
#else
       sprintf (brick_label, "%s_GLT#%d_Coef", glt_label[iglt], ilc);
#endif
       volume = glt_coef_vol[iglt][ilc];
       attach_sub_brick( dset_buck, ivol, volume, nxyz,
                         FUNC_FIM_TYPE, brick_label, 0, 0, 0, NULL);
       free((void *)volume) ; ivol++ ;

       if( tout ){
#ifdef USE_OLD_LABELS
         sprintf( brick_label , "%s LC[%d] t-st" , glt_label[iglt], ilc ) ;
#else
         sprintf (brick_label, "%s_GLT#%d_Tstat", glt_label[iglt], ilc);
#endif
         volume = glt_tcoef_vol[iglt][ilc];
         attach_sub_brick( dset_buck, ivol, volume, nxyz,
                           FUNC_TT_TYPE, brick_label, nt-np, 0, 0, NULL);
         free((void *)volume) ; ivol++ ;
       }
     }

     if( rout ){
       float a,b ;
#ifdef USE_OLD_LABELS
       sprintf( brick_label , "%s R^2" , glt_label[iglt] ) ;
#else
       sprintf( brick_label , "%s_GLT_R^2" , glt_label[iglt] ) ;
#endif
       volume = glt_rstat_vol[iglt];
       a = 0.5*glt_rows[iglt] ; b = 0.5*(nt-np) ;
       attach_sub_brick( dset_buck, ivol, volume, nxyz,
                         FUNC_BT_TYPE, brick_label, 0, a, b, NULL);
       free((void *)volume) ; ivol++ ;
     }

     if( fout ){
#ifdef USE_OLD_LABELS
       sprintf( brick_label , "%s F-stat" , glt_label[iglt] ) ;
#else
       sprintf( brick_label , "%s_GLT_Fstat" , glt_label[iglt] ) ;
#endif
       volume = glt_fstat_vol[iglt];
       attach_sub_brick( dset_buck, ivol, volume, nxyz,
                         FUNC_FT_TYPE, brick_label, 0, glt_rows[iglt], nt-np, NULL);
       free((void *)volume) ; ivol++ ;
     }
   }  /** End loop over general linear tests **/

   /*----- Write dataset out! -----*/

   DSET_write( dset_buck ) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* New code to read one GLT matrix: the old way and the new way [29 Jul 2004]
-----------------------------------------------------------------------------*/

#define GLT_ERR  ERROR_exit("Can't read GLT matrix from file %s",fname)

void read_glt_matrix( char *fname, int *nrows, int ncol, matrix *cmat )
{
   int ii,jj ;

ENTRY("read_glt_matrix") ;

   if( *nrows > 0 ){    /* standard read of numbers from a file*/

     matrix_file_read( fname , *nrows , ncol , cmat , 1 ) ;  /* mri_read_1D */
     if( cmat->elts == NULL ) GLT_ERR ;

   } else {             /* symbolic read of stim_labels */
     floatvecvec *fvv ;
     float **far=NULL ;
     int nr=0 , iv ;
     char *str_echo=NULL ;  /* 26 Jan 2007 */

     if( nSymStim < 1 )
       ERROR_exit("use of -gltsym without SymStim being defined");

     if( strncmp(fname,"SYM:",4) == 0 ){  /* read directly from fname string */
       char *fdup=strdup(fname+4) , *fpt , *buf ;
       int ss , ns ;

       buf = fdup ;
       while(1){
                           fpt = strchr(buf,'\\'); /* find end of 'line' */
         if( fpt == NULL ) fpt = strchr(buf,'|') ;
         if( fpt != NULL ) *fpt = '\0' ;
         fvv = SYM_expand_ranges( ncol-1 , nSymStim,SymStim , buf ) ;
         if( fvv == NULL || fvv->nvec < 1 ) continue ;
         far = (float **)realloc((void *)far , sizeof(float *)*(nr+fvv->nvec)) ;
         for( iv=0 ; iv < fvv->nvec ; iv++ ) far[nr++] = fvv->fvar[iv].ar ;
         free((void *)fvv->fvar) ; free((void *)fvv) ;
         if( fpt == NULL ) break ;   /* reached end of string? */
         buf = fpt+1 ;               /* no, so loop back for next 'line' */
       }
       free((void *)fdup) ;

     } else {                             /* read from file */
       char buf[8192] , *cpt ;
       FILE *fp = fopen( fname , "r" ) ;
       if( fp == NULL ) GLT_ERR ;
       while(1){
         cpt = fgets( buf , 8192 , fp ) ;   /* read next line */
         if( cpt == NULL ) break ;          /* end of input? */
         str_echo = THD_zzprintf(str_echo," : %s",cpt) ;
         fvv = SYM_expand_ranges( ncol-1 , nSymStim,SymStim , buf ) ;
         if( fvv == NULL || fvv->nvec < 1 ) continue ;
         far = (float **)realloc((void *)far , sizeof(float *)*(nr+fvv->nvec)) ;
         for( iv=0 ; iv < fvv->nvec ; iv++ ) far[nr++] = fvv->fvar[iv].ar ;
         free((void *)fvv->fvar) ; free((void *)fvv) ;
       }
       fclose(fp) ;
     }
     if( nr == 0 ) GLT_ERR ;
     *nrows = nr ;
     array_to_matrix( nr , ncol , far , cmat ) ;

     for( ii=0 ; ii < nr ; ii++ ) free((void *)far[ii]) ;
     free((void *)far) ;

     if( !AFNI_noenv("AFNI_GLTSYM_PRINT") ){
       printf("GLT matrix from '%s':\n",fname) ;
       if( str_echo != NULL ){ printf("%s",str_echo); free(str_echo); }
       matrix_print( *cmat ) ;
     }
   }

   /** check for all zero rows, which will cause trouble later **/

   for( ii=0 ; ii < *nrows ; ii++ ){
     for( jj=0 ; jj < ncol && cmat->elts[ii][jj] == 0.0 ; jj++ ) ; /* nada */
     if( jj == ncol )
       ERROR_message("row #%d of matrix '%s' is all zero!", ii+1 , fname ) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,".") ;
   nn++ ;
}

/***************************************************************************/
/** Functions  for basis function expansion of impulse response function. **/
/***************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Evaluate a basis expansion, given the weights for each function
    in wt[] and the point of evaluation.
----------------------------------------------------------------------------*/

float basis_evaluation( basis_expansion *be , float *wt , float x )
{
   float sum=0.0 ;
   int ii ;

   if( x >= be->tbot && x <= be->ttop ){
     for( ii=0 ; ii < be->nfunc ; ii++ )
       sum += wt[ii] * basis_funceval( be->bfunc[ii] , x ) ;
   }

   return sum ;
}

/*--------------------------------------------------------------------------*/
/*! Tent basis function:
     - 0 for x outside bot..top range
     - piecewise linear and equal to 1 at x=mid
----------------------------------------------------------------------------*/

static float basis_tent( float x, float bot, float mid, float top, void *q )
{
   if( x <= bot || x >= top ) return 0.0f ;
   if( x <= mid )             return (x-bot)/(mid-bot) ;
                              return (top-x)/(top-mid) ;
}

#ifdef USE_CSPLIN
/*--------------------------------------------------------------------------*/
#undef  CA
#define CA  0.5f   /* negative of slope at x=0 */
static float hh_csplin( float y )   /* for CSPLIN */
{
   float yy = fabsf(y) ;
   if( yy >= 2.0f ) return 0.0f ;
   if( yy >= 1.0f ) return -CA*(-4.0f+yy*(8.0f+yy*(-5.0f+yy))) ;
   return 1.0f+yy*yy*((2.0f-CA)*yy-(3.0f-CA)) ;
}
#undef CA
/*--------------------------------------------------------------------------*/
/*! CSPLIN: Cardinal spline basis function: [15 Mar 2007]
----------------------------------------------------------------------------*/

static float basis_csplin( float x, float a, float dx, float flag, void *q )
{
   float y=(x-a)/dx , bot=-2.0f , top=2.0f ; int gg=(int)flag ;
   switch(gg){
     case -2: bot =  0.0f ; break ;  /* at left edge */
     case -1: bot = -1.0f ; break ;  /* 1 in from left edge */
     case  1: top =  1.0f ; break ;  /* 1 in from right edge */
     case  2: top =  0.0f ; break ;  /* at right edge */
   }
   if( y < bot || y > top ) return 0.0f ;
   return hh_csplin(y) ;
}
#endif /* USE_CSPLIN */

/*--------------------------------------------------------------------------*/
/* Basis function that is 1 inside the bot..top interval, 0 outside of it.
----------------------------------------------------------------------------*/

static float basis_one( float x, float bot, float top, float junk, void *q )
{
   if( x < bot || x > top ) return 0.0f ;
   return 1.0f ;
}

/*--------------------------------------------------------------------------*/
/* Cosine basis function:
    - 0 for x outside bot..top range
    - cos(freq*(x-bot)) otherwise.
----------------------------------------------------------------------------*/

static float basis_cos( float x, float bot, float top, float freq, void *q )
{
   if( x < bot || x > top ) return 0.0f ;
   return (float)cos(freq*(x-bot)) ;
}

/*--------------------------------------------------------------------------*/
/* Sine basis function:
    - 0 for x outside bot..top range
    - sin(freq*(x-bot)) otherwise.
----------------------------------------------------------------------------*/

static float basis_sin( float x, float bot, float top, float freq, void *q )
{
   if( x <= bot || x >= top ) return 0.0f ;
   return (float)sin(freq*(x-bot)) ;
}

/*--------------------------------------------------------------------------*/
/* Gamma variate basis function
    - 0 for x outside range 0..top
    - x^b * exp(-x/c), scaled to peak value=1, otherwise
----------------------------------------------------------------------------*/

static float basis_gam( float x, float b, float c, float top, void *q )
{
   if( x <= 0.0f || x > top ) return 0.0f ;
   return (float)(pow(x/(b*c),b)*exp(b-x/c)) ;
}

/*--------------------------------------------------------------------------*/

#undef  SPM_A1
#undef  SPM_A2
#undef  SPM_P1
#undef  SPM_P2

#define SPM_A1 0.0083333333    /* A * exp(-x) * x^P */
#define SPM_P1 4.0
#define SPM_A2 1.274527e-13
#define SPM_P2 15.0

static float basis_spmg1( float x, float a, float b, float c, void *q )
{
   if( x <= 0.0f || x >= 25.0f ) return 0.0f ;
   return (float)(exp(-x)*( SPM_A1*pow(x,SPM_P1)
                           -SPM_A2*pow(x,SPM_P2) )) ;
}

static float basis_spmg2( float x, float a, float b, float c, void *q )
{
   if( x <= 0.0f || x >= 25.0f ) return 0.0f ;
   return (float)(exp(-x)*( SPM_A1*pow(x,SPM_P1-1.0)*(SPM_P1-x)
                           -SPM_A2*pow(x,SPM_P2-1.0)*(SPM_P2-x) )) ;
}

/*--------------------------------------------------------------------------*/
/*  f(t,T) = int( h(t-s) , s=0..min(t,T) )
    where h(t) = t^4 * exp(-t) /(4^4*exp(-4))
    Code generated by Maple.
----------------------------------------------------------------------------*/

#undef  TPEAK4
#define TPEAK4(TT) ((TT)/(1.0-exp(-0.25*(TT))))

static float basis_block_hrf4( float tt , float TT )
{
  register double t26, t2, t4, t1, t42, t12, t34, t35, t16, t46, t,L ;
  double w ;

  if( tt <= 0.0f || tt >= (TT+15.0f) ) return 0.0f ;

  t = tt ; L = TT ; t4 = exp(0.4e1 - t);
  if( t < L ){ L = t ; t16 = 54.5982 ; }
  else       { t16 = exp(4.0-t+L) ;    }

  t1 = t * t;
  t2 = t1 * t1;
  t4 = exp(4.0 - t);
  t12 = t1 * t;
  t26 = t16 * L;
  t34 = L * L;
  t35 = t16 * t34;
  t42 = t16 * t34 * L;
  t46 = t34 * t34;

  w = -t2 * t4 / 0.256e3 - 0.3e1 / 0.32e2 * t4 - 0.3e1 / 0.32e2 * t4 * t
      - 0.3e1 / 0.64e2 * t4 * t1 - t4 * t12 / 0.64e2 + t16 * t2 / 0.256e3
      + 0.3e1 / 0.32e2 * t16 + 0.3e1 / 0.32e2 * t16 * t
      + 0.3e1 / 0.64e2 * t1 * t16 + t16 * t12 / 0.64e2 - 0.3e1 / 0.32e2 * t26
      - 0.3e1 / 0.32e2 * t26 * t - 0.3e1 / 0.64e2 * t1 * t26
      - t26 * t12 / 0.64e2 + 0.3e1 / 0.64e2 * t35 + 0.3e1 / 0.64e2 * t35 * t
      + 0.3e1 / 0.128e3 * t1 * t35 - t42 / 0.64e2 - t42 * t / 0.64e2
      + t16 * t46 / 0.256e3 ;
  return (float)w ;
}

static float basis_block4( float t, float T, float peak, float junk, void *q )
{
   float w , tp , pp ;

   w = basis_block_hrf4(t,T) ;
   if( w > 0.0f && peak > 0.0f ){
     tp = TPEAK4(T) ;  pp = basis_block_hrf4(tp,T) ;
     if( pp > 0.0f ) w *= peak / pp ;
   }
   return w ;
}

/*--------------------------------------------------------------------------*/
/*  f(t,T) = int( h(t-s) , s=0..min(t,T) )
    where h(t) = t^5 * exp(-t) /(5^5*exp(-5))
    Code generated by Maple.
----------------------------------------------------------------------------*/

#undef  TPEAK5
#define TPEAK5(TT) ((TT)/(1.0-exp(-0.2*(TT))))

static float basis_block_hrf5( float tt, float TT )
{
   register double t , T ;
   register double t2,t3,t4,t5,t6,t7,t9,t10,t11,t14,t20,t25,t28,t37,t57 ;
   double w ;

   if( tt <= 0.0f || tt >= (TT+15.0f) ) return 0.0f ;

   t = tt ; T = TT ;

#if 1
   t2 = exp(-t) ;
   if( t <= T ){ t3 = t ; t4 = 1.0/t2 ; }
   else        { t3 = T ; t4 = exp(T)  ; }
   t2 *= 148.413 ;    /* 148.413 = exp(5) */
#else
   t2 = exp(0.5e1 - t);
   t3 = (t <= T ? t : T);
   t4 = exp(t3);
#endif
   t5 = t * t;
   t6 = t5 * t5;
   t7 = t6 * t;
   t9 = t3 * t3;
   t10 = t9 * t9;
   t11 = t4 * t10;
   t14 = t4 * t9 * t3;
   t20 = t4 * t3;
   t25 = t4 * t9;
   t28 = t5 * t;
   t37 = -0.120e3 + t4 * t7 + 0.5e1 * t11 - 0.20e2 * t14 - t4 * t10 * t3
         - 0.10e2 * t14 * t5 - 0.120e3 * t20 * t - 0.20e2 * t14 * t
         + 0.30e2 * t25 * t5 + 0.10e2 * t25 * t28 + 0.5e1 * t11 * t
         + 0.20e2 * t4 * t28 + 0.60e2 * t25 * t;
   t57 = -0.5e1 * t20 * t6 - 0.20e2 * t20 * t28 - 0.60e2 * t20 * t5
         - 0.5e1 * t6 - 0.20e2 * t28 + 0.120e3 * t4 - 0.120e3 * t
         - 0.120e3 * t20 + 0.60e2 * t25 - t7 - 0.60e2 * t5 + 0.120e3 * t4 * t
         + 0.60e2 * t4 * t5 + 0.5e1 * t4 * t6;
   w = t2 * (t37 + t57) / 0.3125e4;

   return (float)w ;
}

static float basis_block5( float t, float T, float peak, float junk, void *q )
{
   float w , tp , pp ;

   w = basis_block_hrf5(t,T) ;
   if( w > 0.0f && peak > 0.0f ){
     tp = TPEAK5(T) ;  pp = basis_block_hrf5(tp,T) ;
     if( pp > 0.0f ) w *= peak / pp ;
   }
   return w ;
}

/*--------------------------------------------------------------------------*/
/* Legendre polynomial basis function
    - 0 for x outside range bot..top
    - P_n(x), x scaled to be -1..1 over range bot..top
----------------------------------------------------------------------------*/

static float basis_legendre( float x, float bot, float top, float n, void *q )
{
   float xq ;

   x = 2.0f*(x-bot)/(top-bot) - 1.0f ;  /* now in range -1..1 */

   if( x < -1.0f || x > 1.0f ) return 0.0f ;

   xq = x*x ;

   switch( (int)n ){
    case 0: return 1.0f ;
    case 1: return x ;
    case 2: return (3.0f*xq-1.0f)/2.0f ;
    case 3: return (5.0f*xq-3.0f)*x/2.0f ;
    case 4: return ((35.0f*xq-30.0f)*xq+3.0f)/8.0f ;
    case 5: return ((63.0f*xq-70.0f)*xq+15.0f)*x/8.0f ;
    case 6: return (((231.0f*xq-315.0f)*xq+105.0f)*xq-5.0f)/16.0f ;
    case 7: return (((429.0f*xq-693.0f)*xq+315.0f)*xq-35.0f)*x/16.0f ;

    case 8: return ((((6435.0f*xq-12012.0f)*xq+6930.0f)*xq-1260.0f)*xq+35.0f)
                  /128.0f;

    case 9: return ((((12155.0f*xq-25740.0f)*xq+18018.0f)*xq-4620.0f)*xq+315.0f)
                  *x/128.0f ;
   }

   return 0.0f ;   /* should never be reached */
}

#undef  POLY_MAX
#define POLY_MAX 9  /* max order allowed in function above */

/*--------------------------------------------------------------------------*/
#define ITT 19
#define IXX 23
#define IZZ 25

/*------------------------------------------------------*/
/*! Basis function given by a user-supplied expression. */

static float basis_expr( float x, float bot, float top, float dtinv, void *q )
{
   PARSER_code *pc = (PARSER_code *)q ;
   double atoz[26] , val ;

   if( x < bot || x > top ) return 0.0f ;
   atoz[ITT] = x ;                            /* t = true time from stim */
   atoz[IXX] = (x-bot)*dtinv ;                /* x = scaled to [0,1] */
   atoz[IZZ] = 2.0*atoz[IXX] - 1.0 ;          /* z = scaled to [-1,1] */
   val = PARSER_evaluate_one( pc , atoz ) ;
   return (float)val ;
}

/*--------------------------------------------------------------------------*/
/* Take a string and generate a basis expansion structure from it.
----------------------------------------------------------------------------*/

basis_expansion * basis_parser( char *sym )
{
   basis_expansion *be ;
   char *cpt , *scp ;
   float bot=0.0f, top=0.0f ;
   int nn , nord=0 ;

   if( sym == NULL ) return NULL ;

   scp = strdup(sym) ;                        /* duplicate, for editing */
   cpt = strchr(scp,'(') ;                    /* find opening '(' */
   if( cpt != NULL ){ *cpt = '\0' ; cpt++ ; } /* cut string there */

   be = (basis_expansion *)malloc(sizeof(basis_expansion)) ;
   be->name = NULL ;   /* will be fixed later */
   be->symfun = strdup(sym) ;  /* 06 Mar 2007 */

   /*--- GAM(b,c) ---*/

   if( strcmp(scp,"GAM") == 0 ){

     be->nfunc = 1 ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = basis_gam ;
     if( cpt == NULL ){
       be->bfunc[0].a = 8.6f ;     /* Mark Cohen's parameters */
       be->bfunc[0].b = 0.547f ;   /* t_peak=4.7 FWHM=3.7 */
       be->bfunc[0].c = 11.1f ;    /* return to zero-ish */
     } else {
       sscanf(cpt,"%f,%f",&bot,&top) ;
       if( bot <= 0.0f || top <= 0.0f ){
         ERROR_message("'GAM(%s' is illegal",cpt) ;
         ERROR_message(
           " Correct format: 'GAM(b,c)' with b > 0 and c > 0.");
         free((void *)be->bfunc); free((void *)be); free(scp); return NULL;
       }
       be->bfunc[0].a = bot ;    /* t_peak = bot*top */
       be->bfunc[0].b = top ;    /* FWHM   = 2.3*sqrt(bot)*top */
       be->bfunc[0].c = bot*top + 4.0f*sqrt(bot)*top ;  /* long enough */
     }
     be->tbot = 0.0f ; be->ttop = be->bfunc[0].c ;

   /*--- TENT(bot,top,order) ---*/

   } else if( strcmp(scp,"TENT") == 0 ){
     float dx ;

     if( cpt == NULL ){
       ERROR_message("'TENT' by itself is illegal") ;
       ERROR_message(
        " Correct format: 'TENT(bot,top,n)' with bot < top and n > 0.") ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 2 ){
       ERROR_message("'TENT(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'TENT(bot,top,n)' with bot < top and n > 1.") ;
       free((void *)be); free(scp); return NULL;
     }
     be->nfunc = nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     dx        = (top-bot) / (nord-1) ;

     be->bfunc[0].f = basis_tent ;
     be->bfunc[0].a = bot-0.001*dx ;
     be->bfunc[0].b = bot ;
     be->bfunc[0].c = bot+dx ;
     for( nn=1 ; nn < nord-1 ; nn++ ){
       be->bfunc[nn].f = basis_tent ;
       be->bfunc[nn].a = bot + (nn-1)*dx ;
       be->bfunc[nn].b = bot +  nn   *dx ;
       be->bfunc[nn].c = bot + (nn+1)*dx ;
     }
     be->bfunc[nord-1].f = basis_tent ;
     be->bfunc[nord-1].a = bot + (nord-2)*dx ;
     be->bfunc[nord-1].b = top ;
     be->bfunc[nord-1].c = top + 0.001*dx ;

#ifdef USE_CSPLIN
   /*--- CSPLIN(bot,top,order) ---*/

   } else if( strcmp(scp,"CSPLIN") == 0 ){   /* 15 Mar 2007 */
     float dx ;

     if( cpt == NULL ){
       ERROR_message("'CSPLIN' by itself is illegal") ;
       ERROR_message(
        " Correct format: 'CSPLIN(bot,top,n)' with bot < top and n > 3.") ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 4 ){
       ERROR_message("'CSPLIN(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'CSPLIN(bot,top,n)' with bot < top and n > 3.") ;
       free((void *)be); free(scp); return NULL;
     }
     be->nfunc = nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     dx        = (top-bot) / (nord-1) ;

     for( nn=0 ; nn < nord ; nn++ ){
       be->bfunc[nn].f = basis_csplin ;
       be->bfunc[nn].a = bot +  nn*dx ;
       be->bfunc[nn].b = dx ;
       be->bfunc[nn].c = 0.0f ;
     }
     be->bfunc[0].c      = -2.0f ;  /* edge markers */
     be->bfunc[1].c      = -1.0f ;
     be->bfunc[nord-2].c =  1.0f ;
     be->bfunc[nord-1].c =  2.0f ;
#endif /* USE_CSPLIN */

   /*--- TRIG(bot,top,order) ---*/

   } else if( strcmp(scp,"TRIG") == 0 ){

     if( cpt == NULL ){
       ERROR_message("'TRIG' by itself is illegal") ;
       ERROR_message(
        " Correct format: 'TRIG(bot,top,n)' with bot < top and n > 2.") ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 3 ){
       ERROR_message("'TRIG(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'TRIG(bot,top,n)' with bot < top and n > 2.") ;
       free((void *)be); free(scp); return NULL;
     }
     be->nfunc = nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;

     be->bfunc[0].f = basis_one ;
     be->bfunc[0].a = bot ;
     be->bfunc[0].b = top ;
     be->bfunc[0].c = 0.0f ;
     for( nn=1 ; nn < nord ; nn++ ){
       be->bfunc[nn].f = basis_cos ;
       be->bfunc[nn].a = bot ;
       be->bfunc[nn].b = top ;
       be->bfunc[nn].c = (2.0*PI)*((nn+1)/2)/(top-bot) ;
       nn++ ; if( nn >= nord ) break ;
       be->bfunc[nn].f = basis_sin ;
       be->bfunc[nn].a = bot ;
       be->bfunc[nn].b = top ;
       be->bfunc[nn].c = (2.0*PI)*((nn+1)/2)/(top-bot) ;
     }

   /*--- SIN(bot,top,order) ---*/

   } else if( strcmp(scp,"SIN") == 0 ){

     if( cpt == NULL ){
       ERROR_message("'SIN' by itself is illegal") ;
       ERROR_message(
        " Correct format: 'SIN(bot,top,n)' with bot < top and n > 0.") ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 1 ){
       ERROR_message("'SIN(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'SIN(bot,top,n)' with bot < top and n > 0.") ;
       free((void *)be); free(scp); return NULL;
     }
     be->nfunc = nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;

     for( nn=0 ; nn < nord ; nn++ ){
       be->bfunc[nn].f = basis_sin ;
       be->bfunc[nn].a = bot ;
       be->bfunc[nn].b = top ;
       be->bfunc[nn].c = PI*(nn+1)/(top-bot) ;
     }

   /*--- POLY(bot,top,order) ---*/

   } else if( strcmp(scp,"POLY") == 0 ){

     if( cpt == NULL ){
       ERROR_message("'POLY' by itself is illegal") ;
       ERROR_message(
        " Correct format: 'POLY(bot,top,n)' with bot<top and n>0 and n<%d",
        POLY_MAX+1) ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 1 || nord > POLY_MAX ){
       ERROR_message("'POLY(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'POLY(bot,top,n)' with bot<top and n>0 and n<%d",
        POLY_MAX+1) ;
       free((void *)be); free(scp); return NULL;
     }
     be->nfunc = nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = basis_one ;
     be->bfunc[0].a = bot ;
     be->bfunc[0].b = top ;
     be->bfunc[0].c = 0.0f ;
     for( nn=1 ; nn < nord ; nn++ ){
       be->bfunc[nn].f = basis_legendre ;
       be->bfunc[nn].a = bot ;
       be->bfunc[nn].b = top ;
       be->bfunc[nn].c = (float)nn ;
     }

   /*--- SPMG ---*/

   } else if( strncmp(scp,"SPMG",4) == 0 ){

     be->nfunc = 2 ;
     be->tbot  = 0.0f ; be->ttop = 25.0f ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = basis_spmg1 ;
     be->bfunc[0].a = 0.0f ;
     be->bfunc[0].b = 0.0f ;
     be->bfunc[0].c = 0.0f ;
     be->bfunc[1].f = basis_spmg2 ;
     be->bfunc[1].a = 0.0f ;
     be->bfunc[1].b = 0.0f ;
     be->bfunc[1].c = 0.0f ;

   /*--- BLOCKn(duration,peak) for n=4 or 5 ---*/

   } else if( strncmp(scp,"BLOCK",5) == 0 || strncmp(scp,"IGFUN",5) == 0 ){
     int nb=4 ;

     if( scp[5] != '\0' ){
       nb = strtol( scp+5 , NULL , 10 ) ;
       if( nb != 4 && nb != 5 ){
         ERROR_message("'%s' has illegal power: only 4 or 5 allowed",scp) ;
         free((void *)be); free(scp); return NULL;
       }
     }

     if( cpt == NULL ){
       ERROR_message("'%s' by itself is illegal",scp) ;
       ERROR_message(
        " Correct format: 'BLOCKn(dur)' with dur > 0, n=4 or 5\n"
        "                     *OR* 'BLOCKn(dur,peak)' with peak > 0 too.") ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f",&top,&bot) ;
     if( top <= 0.0f ){
       ERROR_message("'%s(%s' is illegal",scp,cpt) ;
       ERROR_message(
        " Correct format: 'BLOCKn(dur)' with dur > 0, n=4 or 4\n"
        "                      or: 'BLOCKn(dur,peak)' with peak > 0 too.") ;
       free((void *)be); free(scp); return NULL;
     }
     if( bot < 0.0f ) bot = 0.0f ;

     be->nfunc = 1 ;
     be->tbot  = 0.0f ; be->ttop = top+15.0f ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = (nb==5) ? basis_block5 : basis_block4 ;
     be->bfunc[0].a = top ;
     be->bfunc[0].b = bot ;
     be->bfunc[0].c = 0.0f ;

     /* 04 May 2007: check for consistency in BLOCK-izing */

     { static float first_block_peakval = 0.0f ;
       static char *first_block_peaksym = NULL ;
       static int   num_block           = 0 ;
       static float first_len_pkzero    = 0.0f ;
       static char *first_sym_pkzero    = NULL ;

       if( num_block == 0 ){
         first_block_peakval = bot ;
         first_block_peaksym = strdup(sym) ;
       } else if( FLDIF(bot,first_block_peakval) ){
         WARNING_message(
          "%s has different peak value than first %s\n"
          "            We hope you know what you are doing!" ,
          sym , first_block_peaksym ) ;
       }

       if( bot == 0.0f ){
         if( first_len_pkzero == 0.0f ){
           first_len_pkzero = top ;
           first_sym_pkzero = strdup(sym) ;
         } else if( FLDIF(top,first_len_pkzero) ){
           WARNING_message(
            "%s has different duration than first %s\n"
            "            ==> Amplitudes will differ.  We hope you know what you are doing!" ,
            sym , first_sym_pkzero ) ;
         }
       }

       num_block++ ;
     }

   /*--- EXPR(bot,top) exp1 exp2 ... ---*/

   } else if( strcmp(scp,"EXPR") == 0 ){   /* 28 Aug 2004 */
     char *ept ;
     NI_str_array *sar ;
     int nexpr , ie ;
     PARSER_code *pc ;

     if( cpt == NULL ){
       ERROR_message("'EXPR' by itself is illegal") ;
       free((void *)be); free(scp); return NULL;
     }
     sscanf(cpt,"%f,%f",&bot,&top) ;
     if( top <= bot ){
       ERROR_message("'EXPR(%f,%f)' has illegal time range",bot,top) ;
       free((void *)be); free(scp); return NULL;
     }
     ept = strchr( cpt , ')' ) ;
     if( ept == NULL ){
       ERROR_message("'EXPR(%f,%f)' has no expressions!?",bot,top);
       free((void *)be); free(scp); return NULL;
     }
     sar = NI_decode_string_list( ept+1 , "~" ) ;
     if( sar == NULL || sar->num == 0 ){
       ERROR_message("'EXPR(%f,%f)' has no expressions!?",bot,top);
       free((void *)be); free(scp); return NULL;
     }
     be->nfunc = nexpr = sar->num ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     PARSER_set_printout(1) ;
     for( ie=0 ; ie < nexpr ; ie++ ){
       pc = PARSER_generate_code( sar->str[ie] ) ;
       if( pc == NULL ){
         ERROR_message("unparsable EXPRession '%s'",sar->str[ie]) ;
         free((void *)be); free(scp); return NULL;
       }
       if( strcmp(sar->str[ie],"1") != 0 &&    /* must either be "1" */
           !PARSER_has_symbol("t",pc)    &&    /* or contain symbol  */
           !PARSER_has_symbol("x",pc)    &&    /* 't', 'x', or 'z'   */
           !PARSER_has_symbol("z",pc)      ){
         ERROR_message(
           "illegal EXPRession '%s' lacks symbol t, x, or z",
           sar->str[ie]) ;
         free((void *)be); free(scp); return NULL;
       }
       be->bfunc[ie].f = basis_expr ;
       be->bfunc[ie].a = bot ;
       be->bfunc[ie].b = top ;
       be->bfunc[ie].c = 1.0f/(top-bot) ;  /* for ease of scaling */
       be->bfunc[ie].q = (void *)pc ;
     }
     PARSER_set_printout(0) ;

     NI_delete_str_array(sar) ;

   /*--- NO MORE BASIS FUNCTION CHOICES ---*/

   } else {
     ERROR_message("'%s' is unknown response function type",scp) ;
     free((void *)be); free(scp); return NULL;
   }

   /* 28 Apr 2005: set scaling factors */

   for( nn=0 ; nn < be->nfunc ; nn++ )  /* initialize factors to 1.0 */
     be->bfunc[nn].ffac = 1.0 ;

   /* for each basis function, find its peak value, then
      set ffac so that the peak value becomes basis_normall */

#undef  BNSUB
#define BNSUB 999
   if( basis_normall > 0.0f ){
     int jj ; float dt , ftop , val ;
     bot = be->tbot ; top = be->ttop ; dt = (top-bot)/BNSUB ;
     for( nn=0 ; nn < be->nfunc ; nn++ ){
       ftop = 0.0f ;
       for( jj=0 ; jj <= BNSUB ; jj++ ){
         val = basis_funceval( be->bfunc[nn] , bot+jj*dt ) ;
         val = fabs(val) ; if( val > ftop ) ftop = val ;
       }
       if( ftop > 0.0f ) be->bfunc[nn].ffac = basis_normall / ftop ;
     }
   }

   free(scp); return be;
}

/*----------------------------------------------------------------------*/
/*! For IRFs defined by -stim_times basis function expansion, write out
    a 3D+time dataset with time spacing dt.
------------------------------------------------------------------------*/

void basis_write_iresp( int argc , char *argv[] ,
                        DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float **wtar , char *output_filename )
{
   int nvox, ii, nf, allz, ts_length ;
   register int pp, ib ;
   float *wt , *tt , **hout , factor , **bb ;
   short *bar ;
   THD_3dim_dataset *in_dset = NULL;
   THD_3dim_dataset *out_dset = NULL;
   char *commandline , label[512] ;
   const float EPSILON = 1.0e-10 ;

   /* open input 3D+time dataset to get some parameters */

   in_dset = THD_open_dataset(option_data->input_filename);
   CHECK_OPEN_ERROR(in_dset,option_data->input_filename);
   nvox    = in_dset->daxes->nxx * in_dset->daxes->nyy * in_dset->daxes->nzz;
   DSET_UNMSEC(in_dset) ;  /* 12 Aug 2005 */

   if( dt <= 0.0f ) dt = DSET_TR(in_dset) ;
   if( dt <= 0.0f ) dt = 1.0f ;

   /* create output dataset on the input as a model, then close the input */

   out_dset = EDIT_empty_copy( in_dset ) ;
   tross_Copy_History( in_dset , out_dset ) ;
   DSET_delete( in_dset ) ;

   /* historicize the output */

   commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
   sprintf( label , "Impulse response: %s" , output_filename ) ;
   if( commandline != NULL )
     tross_multi_Append_History( out_dset , commandline,label,NULL ) ;
   else
     tross_Append_History ( out_dset, label);
   free((void *)commandline) ;

   ts_length = 1 + (int)ceil( (be->ttop - be->tbot)/dt ) ; /* 13 Apr 2005: +1 */

   /* modify the output dataset appropriately */

   (void ) EDIT_dset_items( out_dset,
                             ADN_prefix,      output_filename,
                             ADN_label1,      output_filename,
                             ADN_self_name,   output_filename,
                             ADN_malloc_type, DATABLOCK_MEM_MALLOC,
                             ADN_datum_all,   (floatout)?MRI_float:MRI_short,
                             ADN_nvals,       ts_length,
                             ADN_ntt,         ts_length,
                             ADN_ttdel,       dt ,
                             ADN_ttorg,       be->tbot,
                            ADN_none ) ;

#ifndef FIX_CONFLICTS
   if( THD_is_file(out_dset->dblk->diskptr->header_name) ){
     ERROR_message(
             "Output dataset file %s already exists - won't overwrite",
             out_dset->dblk->diskptr->header_name ) ;
     DSET_delete(out_dset) ; return ;
   }
#else
   if( THD_deconflict_prefix(out_dset) > 0 )
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_filename,DSET_PREFIX(out_dset) ) ;
#endif

   /* create output bricks (float for now, will scale to shorts later) */

   hout = (float **) malloc( sizeof(float *) * ts_length ) ;
   for( ib=0 ; ib < ts_length ; ib++ )
     hout[ib] = (float *)calloc(sizeof(float),nvox) ;

   /* create basis vectors for the expansion on the dt time grid */

   nf = be->nfunc ;
   wt = (float *) malloc( sizeof(float) * nf ) ;
   tt = (float *) malloc( sizeof(float) * ts_length ) ;

   for( ib=0 ; ib < ts_length ; ib++ )     /* output time grid */
     tt[ib] = be->tbot + ib*dt ;

   bb = (float **) malloc( sizeof(float *) * nf ) ;
   for( pp=0 ; pp < nf ; pp++ ){
     bb[pp] = (float *) malloc( sizeof(float) * ts_length ) ;
     for( ib=0 ; ib < ts_length ; ib++ )
       bb[pp][ib] = basis_funceval( be->bfunc[pp] , tt[ib] ) ;
   }

   /* loop over voxels:
        extract coefficient (weights) for each basis function into wt
        sum up basis vectors times wt to get result, save into output arrays */

   for( ii=0 ; ii < nvox ; ii++ ){
     allz = 1 ;
     for( pp=0 ; pp < nf ; pp++ ){
       wt[pp] = wtar[pp][ii] ; allz = ( allz && (wt[pp] == 0.0f) );
     }

     if( allz ){
       for( ib=0 ; ib < ts_length ; ib++ ) hout[ib][ii] = 0.0f ;
     } else {
       register float sum ;
       for( ib=0 ; ib < ts_length ; ib++ ){
         sum = 0.0f ;
         for( pp=0 ; pp < nf ; pp++ ) sum += wt[pp] * bb[pp][ib] ;
         hout[ib][ii] = sum ;
       }
     }
   }

   /* toss some trash */

   for( pp=0 ; pp < nf ; pp++ ) free((void *)bb[pp]) ;
   free((void *)bb) ; free((void *)tt) ; free((void *)wt) ;

   /* scale floating point bricks to shorts and insert into output dataset */

   for( ib=0 ; ib < ts_length ; ib++ ){
     if( floatout ){
       EDIT_substitute_brick( out_dset , ib , MRI_float , hout[ib] ) ;
       factor = 0.0f ;
     } else {
       bar = (short *) malloc( sizeof(short) * nvox ) ;
       factor = EDIT_coerce_autoscale_new(nvox, MRI_float,hout[ib], MRI_short,bar) ;
       if( factor < EPSILON ) factor = 0.0f ;          /* if brick is all zero */
       else                   factor = 1.0f / factor ;
       EDIT_substitute_brick( out_dset , ib , MRI_short , bar ) ;
       free((void *)hout[ib]) ;
     }
     EDIT_BRICK_FACTOR( out_dset , ib , factor ) ;
   }
   free((void *)hout) ;

   /* and save the results to disk! */

   DSET_write( out_dset ) ;
   if( verb )
    INFO_message("Wrote iresp 3D+time dataset into %s\n",DSET_BRIKNAME(out_dset)) ;

   DSET_delete( out_dset ) ;
   return ;
}

/*----------------------------------------------------------------------*/

void basis_write_sresp( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float *mse ,
                        int pbot, matrix cvar, char *output_filename )
{
   int nvox, ii, nf, allz, ts_length ;
   register int pp,qq, ib ;
   register float sum ;
   float *vv , *tt , **hout , factor , **bb ;
   short *bar ;
   THD_3dim_dataset *in_dset = NULL;
   THD_3dim_dataset *out_dset = NULL;
   char *commandline , label[512] ;
   const float EPSILON = 1.0e-10 ;

   /* open input 3D+time dataset to get some parameters */

   in_dset = THD_open_dataset(option_data->input_filename);
   CHECK_OPEN_ERROR(in_dset,option_data->input_filename);
   nvox    = in_dset->daxes->nxx * in_dset->daxes->nyy * in_dset->daxes->nzz;
   DSET_UNMSEC(in_dset) ;  /* 12 Aug 2005 */

   if( dt <= 0.0f ) dt = DSET_TR(in_dset) ;
   if( dt <= 0.0f ) dt = 1.0f ;

   /* create output dataset on the input as a model, then close the input */

   out_dset = EDIT_empty_copy( in_dset ) ;
   tross_Copy_History( in_dset , out_dset ) ;
   DSET_delete( in_dset ) ;

   /* historicize the output */

   commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
   sprintf( label , "Sigma response: %s" , output_filename ) ;
   if( commandline != NULL )
     tross_multi_Append_History( out_dset , commandline,label,NULL ) ;
   else
     tross_Append_History ( out_dset, label);
   free((void *)commandline) ;

   ts_length = 1 + (int)ceil( (be->ttop - be->tbot)/dt ) ;

   /* modify the output dataset appropriately */

   (void ) EDIT_dset_items( out_dset,
                             ADN_prefix,      output_filename,
                             ADN_label1,      output_filename,
                             ADN_self_name,   output_filename,
                             ADN_malloc_type, DATABLOCK_MEM_MALLOC,
                             ADN_datum_all,   (floatout)?MRI_float:MRI_short,
                             ADN_nvals,       ts_length,
                             ADN_ntt,         ts_length,
                             ADN_ttdel,       dt ,
                             ADN_ttorg,       be->tbot,
                            ADN_none ) ;

#ifndef FIX_CONFLICTS
   if( THD_is_file(out_dset->dblk->diskptr->header_name) ){
     ERROR_message(
             "Output dataset file %s already exists - won't overwrite",
             out_dset->dblk->diskptr->header_name ) ;
     DSET_delete(out_dset) ; return ;
   }
#else
   if( THD_deconflict_prefix(out_dset) > 0 )
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_filename,DSET_PREFIX(out_dset) ) ;
#endif

   /* create output bricks (float for now, will scale to shorts later) */

   hout = (float **) malloc( sizeof(float *) * ts_length ) ;
   for( ib=0 ; ib < ts_length ; ib++ )
     hout[ib] = (float *)calloc(sizeof(float),nvox) ;

   nf = be->nfunc ;
   tt = (float *) malloc( sizeof(float) * ts_length ) ;

   for( ib=0 ; ib < ts_length ; ib++ )     /* output time grid */
     tt[ib] = be->tbot + ib*dt ;

   /* evaluate basis vectors for output on dt time grid */

   bb = (float ** ) malloc( sizeof(float *) * nf ) ;
   for( pp=0 ; pp < nf ; pp++ ){
     bb[pp] = (float * ) malloc( sizeof(float  ) * ts_length ) ;
     for( ib=0 ; ib < ts_length ; ib++ )
       bb[pp][ib] = basis_funceval( be->bfunc[pp] , tt[ib] ) ;
   }
   free((void *)tt) ;

   /* evaluate unscaled variance on dt time grid */

   vv = (float *) malloc( sizeof(float) * ts_length ) ;
   for( ib=0 ; ib < ts_length ; ib++ ){
     sum = 0.0f ;
     for( pp=0 ; pp < nf ; pp++ ){
       for( qq=0 ; qq < nf ; qq++ )
         sum += cvar.elts[pbot+pp][pbot+qq] * bb[pp][ib] * bb[qq][ib] ;
     }
     vv[ib] = (sum >= 0.0f) ? sum : 0.0f ;
   }
   for( pp=0 ; pp < nf ; pp++ ) free((void *)bb[pp]) ;
   free((void *)bb) ;

   /* loop over voxels, scale by mse to get variance */

   for( ii=0 ; ii < nvox ; ii++ ){
     for( ib=0 ; ib < ts_length ; ib++ )
       hout[ib][ii] = sqrt( vv[ib] * mse[ii] ) ;
   }
   free((void *)vv) ;

   /* scale floating point bricks to shorts and insert into output dataset */

   for( ib=0 ; ib < ts_length ; ib++ ){
     if( floatout ){
       EDIT_substitute_brick( out_dset , ib , MRI_float , hout[ib] ) ;
       factor = 0.0f ;
     } else {
       bar = (short *) malloc( sizeof(short) * nvox ) ;
       factor = EDIT_coerce_autoscale_new(nvox, MRI_float,hout[ib], MRI_short,bar) ;
       if( factor < EPSILON ) factor = 0.0f ;          /* if brick is all zero */
       else                   factor = 1.0f / factor ;
       EDIT_substitute_brick( out_dset , ib , MRI_short , bar ) ;
       free((void *)hout[ib]) ;
     }
     EDIT_BRICK_FACTOR( out_dset , ib , factor ) ;
   }
   free((void *)hout) ;

   /* and save the results to disk! */

   DSET_write( out_dset ) ;
   if( verb )
    INFO_message("Wrote sresp 3D+time dataset into %s\n",DSET_BRIKNAME(out_dset)) ;

   DSET_delete( out_dset ) ;
   return ;
}

/*----------------------------------------------------------------------*/

float baseline_mean( vector coef )  /* 31 Aug 2004 */
{
   register int jj ;
   register double sum ;

   sum = 0.0 ;
   for( jj=0 ; jj < nParam ; jj++ )
     if( Xcol_inbase[jj] == 2 ) sum += coef.elts[jj] * Xcol_mean[jj] ;

   return (float)sum ;
}

/*----------------------------------------------------------------------*/

#if 0
typedef struct {           /** structure to hold one -IRC_times stuff **/
  int npar , pbot ;
  float *ww ;              /* [npar] */
  float scale_fac ;        /* fixed multiplier */
  int denom_flag ;         /* what to put in denominator? */
  char *name ;
} basis_irc ;
#endif

float_pair evaluate_irc( basis_irc *birc , vector coef ,
                         float base , float mse , matrix cvar )
{
   float_pair vt={0.0f,0.0f} ;
   int ii,jj, np, pb ;
   double asum , bsum ;
   float *ww ;

   np = birc->npar ;
   pb = birc->pbot ;
   ww = birc->ww ;

   asum = 0.0 ;
   for( ii=0 ; ii < np ; ii++ )
     asum += coef.elts[pb+ii] * ww[ii] ;

   bsum = 0.0 ;
   for( ii=0 ; ii < np ; ii++ )
     for( jj=0 ; jj < np ; jj++ )
       bsum += cvar.elts[pb+ii][pb+jj] * ww[ii] * ww[jj] ;

   bsum *= mse ;  /* variance estimate */

   if( bsum > 0.0 ) vt.b = asum / sqrt(bsum) ;  /* t statistic */

   vt.a = (float)(asum * birc->scale_fac) ;
   if( birc->denom_flag && denom_BASELINE ){
     if( base == 0.0f ) vt.a  = 0.0f ;
     else               vt.a /= base ;
   }
   return vt ;
}

/*---------------------------------------------------------------------------*/
/*! Check matrix condition number.  Return value is the
    number of bad things that were detected.  [07 Mar 2007]
-----------------------------------------------------------------------------*/

static int check_matrix_condition( matrix xdata , char *xname )
{
    double *ev, ebot,emin,emax ;
    int i,nsmall=0, ssing=show_singvals, bad=0 ;

#undef PSINV_EPS
#ifdef FLOATIZE
# define PSINV_EPS      1.e-8
# define CN_VERYGOOD   10.0
# define CN_GOOD       40.0
# define CN_OK        160.0
# define CN_BEWARE    666.0
#else
# define PSINV_EPS      1.e-14
# define CN_VERYGOOD   20.0
# define CN_GOOD      400.0
# define CN_OK       8000.0
# define CN_BEWARE 160000.0
#endif

    ev   = matrix_singvals( xdata ) ;
    emax = ev[0] ;
    for( i=1 ; i < xdata.cols ; i++ ) if( ev[i] > emax ) emax = ev[i] ;
    ebot = sqrt(PSINV_EPS)*emax ; emin = 1.e+38 ;
    for( i=0 ; i < xdata.cols ; i++ ){
      if( ev[i] >= ebot && ev[i] < emin ) emin = ev[i] ;
      if( ev[i] <  ebot ) nsmall++ ;
    }

    if( ssing ) fprintf(stderr,"\n") ;

    if( emin <= 0.0 || emax <= 0.0 ){
      ERROR_message("!! %s matrix condition:  UNDEFINED ** VERY BAD **\n" ,
                     xname ) ;
      ssing = 1 ; bad++ ;
    } else {
      double cond=sqrt(emax/emin) ; char *rating ;
      if( !use_psinv ) cond = cond*cond ;  /* Gaussian elim is twice as bad */
           if( cond < CN_VERYGOOD )  rating = "++ VERY GOOD ++"    ;
      else if( cond < CN_GOOD     )  rating = "++ GOOD ++"         ;
      else if( cond < CN_OK       )  rating = "++ OK ++"           ;
      else if( cond < CN_BEWARE   )  rating = "++ A LITTLE BIG ++" ;
      else                         { rating = "** BEWARE **" ; bad++; ssing = 1; }
      if( strstr(rating,"*") == NULL )
        INFO_message("%s matrix condition [%s] (%dx%d):  %g  %s\n",
                xname, (use_psinv) ? "X" : "XtX",
                xdata.rows,xdata.cols , cond , rating ) ;
      else
        WARNING_message("!! %s matrix condition [%s] (%dx%d):  %g  %s\n",
                xname, (use_psinv) ? "X" : "XtX",
                xdata.rows,xdata.cols , cond , rating ) ;
    }

    if( nsmall > 0 ){
      WARNING_message(
        "!! in %s matrix:\n"
        " * Largest singular value=%g\n"
        " * %d singular value%s less than cutoff=%g\n"
        " * Implies strong collinearity in the matrix columns! \n",
        xname , emax , nsmall , (nsmall==1)?" is":"s are" , ebot ) ;
      ssing = 1 ; bad++ ;
    }

    if( ssing ){
      INFO_message("%s matrix singular values:\n",xname) ;
      for( i=0; i < xdata.cols ; i++ ){
        fprintf(stderr," %13g",ev[i]) ;
        if( i < xdata.cols-1 && i%5 == 4 ) fprintf(stderr,"\n") ;
      }
      fprintf(stderr,"\n") ;
    }

    free((void *)ev) ; return bad ;
}
